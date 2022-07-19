#include "../objects/coff.h"

// constant sized "hash map" which is used to
// deduplicate types in the codeview
#define MAX_TYPE_ENTRY_LOOKUP_SIZE 1024

// leftrotate function definition
#define LEFTROTATE(x, c) (((x) << (c)) | ((x) >> (32 - (c))))

static void md5sum_file(uint8_t out_bytes[16], const char* filepath) {
    FILE* file = fopen(filepath, "rb");
    if (!file) {
        printf("Could not read file: %s\n", filepath);
        abort();
    }

    int descriptor = fileno(file);

    struct stat file_stats;
    if (fstat(descriptor, &file_stats) == -1) {
        fclose(file);
        abort();
    }

    size_t len  = file_stats.st_size;
    unsigned char* data = tb_platform_heap_alloc(len + 17);

    fseek(file, 0, SEEK_SET);
    fread(data, 1, len, file);

    tb__md5sum(out_bytes, data, len);

    fclose(file);
    tb_platform_heap_free(data);
}

static uint32_t hash_buffer(uint32_t hash, size_t n, const void* s) {
    const uint8_t* p = s;
    while (n--) {
        hash = (hash ^ *p++) * 16777619;
    }
    return hash;
}

static uint16_t get_codeview_type(TB_DataType dt) {
    assert(dt.width == 0 && "TODO: implement vector types in CodeView output");
    switch (dt.type) {
        case TB_INT: {
            if (dt.data <= 0)  return 0x0003; // T_VOID
            if (dt.data <= 1)  return 0x0030; // T_BOOL08
            if (dt.data <= 8)  return 0x0020; // T_UCHAR
            if (dt.data <= 16) return 0x0073; // T_UINT2
            if (dt.data <= 32) return 0x0075; // T_UINT4
            if (dt.data <= 64) return 0x0023; // T_UQUAD
            return 0x0023; // T_64PUCHAR
        }
        case TB_FLOAT: {
            if (dt.data == TB_FLT_32) return 0x0040; // T_REAL32
            if (dt.data == TB_FLT_64) return 0x0041; // T_REAL64

            assert(0 && "Unknown float type");
        }
        case TB_PTR: {
            return 0x0023; // T_64PUCHAR
        }
        default: assert(0 && "TODO: missing type in CodeView output");
    }

    return 0x0003; // T_VOID
}

static uint16_t find_or_make_cv_type(TB_Emitter* sect, uint32_t* type_entry_count, CV_TypeEntry* lookup_table, size_t length, uint8_t* key) {
    assert(length % 4 == 0);

    // Hash it
    uint32_t hash = hash_buffer(0, length, key);

    // Search (if there's a collision replace the old one)
    size_t index = hash % MAX_TYPE_ENTRY_LOOKUP_SIZE;
    CV_TypeEntry lookup = lookup_table[index];

    // printf("Lookup %zu (%x hash, has match? %s)\n", index, hash, lookup.key ?
    // "yea" : "naw");
    if (lookup.key) {
        // verify it even matches
        size_t lookup_size = tb_get2b(sect, lookup.key) + 2;

        if (length == lookup_size && memcmp(key, &sect->data[lookup.key], length) == 0) {
            //printf("Saved %zu bytes (%d)\n", length, lookup.value);
            return lookup.value;
        }
    }

    uint16_t type_index = *type_entry_count;
    *type_entry_count += 1;

    // printf("Used %zu bytes (%d)\n", length, type_index);
    lookup_table[index].key   = sect->count;
    lookup_table[index].value = type_index;

    tb_out_reserve(sect, length);
    tb_outs_UNSAFE(sect, length, (const uint8_t*)key);
    return type_index;
}

static TB_Slice gimme_cstr_as_slice(const char* str) {
    return (TB_Slice){ strlen(str), (uint8_t*) strdup(str) };
}

static void add_reloc(TB_ObjectSection* section, const TB_ObjectReloc* reloc) {
    section->relocations[section->relocation_count++] = *reloc;
}

static bool codeview_supported_target(TB_Module* m) {
    return true;
}

static int codeview_number_of_debug_sections(TB_Module* m) {
    return 2;
}

// Based on this, it's the only nice CodeView source out there:
// https://github.com/netwide-assembler/nasm/blob/master/output/codeview.c
static TB_SectionGroup codeview_generate_debug_info(TB_Module* m, TB_TemporaryStorage* tls, const ICodeGen* code_gen, const char* path, size_t function_sym_start, uint32_t* func_layout) {
    TB_ObjectSection* sections = tb_platform_heap_alloc(2 * sizeof(TB_ObjectSection));
    sections[0] = (TB_ObjectSection){ gimme_cstr_as_slice(".debug$S") };
    sections[1] = (TB_ObjectSection){ gimme_cstr_as_slice(".debug$T") };

    // debug$S does quite a few relocations :P, namely saying that
    // certain things point to specific areas of .text section
    sections[0].relocations = tb_platform_heap_alloc(4 * m->functions.compiled_count * sizeof(TB_ObjectReloc));

    // Write type table
    uint32_t* function_type_table = tb_tls_push(tls, m->functions.count * sizeof(uint32_t));
    uint32_t* file_table_offset = tb_tls_push(tls, m->functions.count * sizeof(uint32_t));

    TB_Emitter debugs_out = { 0 };
    TB_Emitter debugt_out = { 0 };
    {
        tb_out4b(&debugt_out, 0x00000004);

        CV_TypeEntry* lookup_table = tb_tls_push(tls, MAX_TYPE_ENTRY_LOOKUP_SIZE * sizeof(CV_TypeEntry));
        memset(lookup_table, 0, MAX_TYPE_ENTRY_LOOKUP_SIZE * sizeof(CV_TypeEntry));

        uint32_t type_entry_count = 0x1000;
        loop(i, m->functions.count) {
            if (m->functions.data[i].output == NULL) continue;
            const TB_FunctionPrototype* proto = m->functions.data[i].prototype;

            // Create argument list
            uint16_t arg_list;
            {
                size_t param_count = proto->param_count + proto->has_varargs;

                size_t length = 2 + 2 + 4 + (4 * param_count);
                uint8_t* data = tb_tls_push(tls, length);

                *((uint16_t*)&data[0]) = length - 2;
                *((uint16_t*)&data[2]) = 0x1201; // ARGLIST type
                *((uint32_t*)&data[4]) = param_count;

                uint32_t* param_data = (uint32_t*)&data[8];
                loop(j, proto->param_count) {
                    param_data[j] = get_codeview_type(proto->params[j]);
                }

                // varargs add a dummy type at the end
                if (proto->has_varargs) {
                    param_data[proto->param_count] = 0;
                }

                arg_list = find_or_make_cv_type(&debugt_out, &type_entry_count, lookup_table, length, data);
                tb_tls_restore(tls, data);
            }

            // Create the procedure type
            uint16_t proc_type;
            {
                CV_LFProc* data = tb_tls_push(tls, sizeof(CV_LFProc));

                uint16_t return_type = get_codeview_type(proto->return_dt);

                *data = (CV_LFProc) {
                    .len = sizeof(CV_LFProc) - 2,
                    .leaf = 0x1008, // LF_PROCEDURE
                    .rvtype = return_type,
                    .parmcount = proto->param_count,
                    .arglist = arg_list
                };

                proc_type = find_or_make_cv_type(&debugt_out, &type_entry_count, lookup_table, sizeof(CV_LFProc), (uint8_t*)data);
                tb_tls_restore(tls, data);
            }

            // Create the function type... which is somehow different from the procedure...
            // it's basically referring to the procedure but it has a name
            {
                size_t name_len   = strlen(m->functions.data[i].name);
                size_t entry_size = sizeof(CV_LFFuncID) + name_len;
                entry_size        = align_up(entry_size + 1, 4);

                CV_LFFuncID* data = tb_tls_push(tls, entry_size);

                *data = (CV_LFFuncID) {
                    .len = entry_size - 2,
                    .leaf = 0x1601, // LF_FUNC_ID
                    .type = proc_type
                };

                memcpy(data->name, m->functions.data[i].name, name_len);
                data->name[name_len] = 0;

                function_type_table[i] = find_or_make_cv_type(&debugt_out, &type_entry_count, lookup_table, entry_size, (uint8_t*) data);
                tb_tls_restore(tls, data);
            }
        }

        tb_tls_restore(tls, lookup_table);

        size_t pad = 4 - (debugt_out.count % 4);
        if (pad == 4) pad = 0;
        while (pad--) tb_out1b(&debugt_out, 0x00);
    }

    // Write symbol info table
    {
        static const char creator_str[] = "Bitch";
        uint32_t creator_length = 2 + 4 + 2 + (3 * 2) + (3 * 2) + sizeof(creator_str) + 2;

        /*sym_length = (cv8_state.num_syms[SYMTYPE_CODE] * 7)
                + (cv8_state.num_syms[SYMTYPE_PROC]  * 7)
                + (cv8_state.num_syms[SYMTYPE_LDATA] * 10)
                + (cv8_state.num_syms[SYMTYPE_GDATA] * 10)
                + (cv8_state.symbol_lengths);*/

        size_t path_len = strlen(path) + 1;

        tb_out4b(&debugs_out, 0x00000004);

        // File nametable
        if (false) {
            tb_out4b(&debugs_out, 0x000000F3);

            size_t field_length_patch = debugs_out.count;
            tb_out4b(&debugs_out, 0);
            tb_out1b(&debugs_out, 0);

            // skip the NULL file entry
            size_t pos = 1;
            loop_range(i, 1, m->files.count) {
                size_t len = strlen(m->files.data[i].path);

                tb_out_reserve(&debugs_out, len + 1);
                tb_outs_UNSAFE(&debugs_out, len + 1, (const uint8_t*)m->files.data[i].path);

                file_table_offset[i] = pos;
                pos += len + 1;
            }

            tb_patch4b(&debugs_out, field_length_patch, (debugs_out.count - field_length_patch) - 4);
        }

        // Source file table
        // we practically transmute the file_table_offset from meaning file string
        // table entries into source file entries.
        if (false) {
            tb_out4b(&debugs_out, 0x000000F4);

            size_t field_length_patch = debugs_out.count;
            tb_out4b(&debugs_out, 0);

            size_t pos = 0;
            loop_range(i, 1, m->files.count) {
                uint8_t sum[16];
                md5sum_file(sum, m->files.data[i].path);

                tb_out4b(&debugs_out, file_table_offset[i]);
                tb_out2b(&debugs_out, 0x0110);
                tb_out_reserve(&debugs_out, MD5_HASHBYTES);
                tb_outs_UNSAFE(&debugs_out, MD5_HASHBYTES, sum);
                tb_out2b(&debugs_out, 0);

                file_table_offset[i] = pos;
                pos += 4 + 2 + MD5_HASHBYTES + 2;
            }

            tb_patch4b(&debugs_out, field_length_patch, (debugs_out.count - field_length_patch) - 4);
        }

        // Line info table
        if (true) {
            loop(func_id, m->functions.count) {
                TB_FunctionOutput* out_f = m->functions.data[func_id].output;
                if (!out_f) continue;

                uint64_t meta = out_f->prologue_epilogue_metadata;
                uint64_t stack_usage = out_f->stack_usage;

                // Layout crap
                uint32_t function_start = func_layout[func_id];
                uint32_t function_end = func_layout[func_id + 1];

                uint32_t body_start = code_gen->get_prologue_length(meta, stack_usage);

                size_t line_count = m->functions.data[func_id].line_count;
                TB_Line* restrict lines = m->functions.data[func_id].lines;

                tb_out4b(&debugs_out, 0x000000F2);
                size_t field_length_patch = debugs_out.count;
                tb_out4b(&debugs_out, 0);

                // Source mapping header
                {
                    size_t patch_pos = debugs_out.count;
                    add_reloc(&sections[0], &(TB_ObjectReloc){
                            TB_OBJECT_RELOC_SECREL,
                            function_sym_start + func_id,
                            patch_pos
                        });

                    add_reloc(&sections[0], &(TB_ObjectReloc){
                            TB_OBJECT_RELOC_SECTION,
                            function_sym_start + func_id,
                            patch_pos + 4
                        });
                }

                tb_out4b(&debugs_out, 0); // SECREL  | .text
                tb_out4b(&debugs_out, 0); // SECTION | .text
                tb_out4b(&debugs_out, function_end - function_start);

                // when we make new file line regions
                // we backpatch the line count for the
                // region we just finished
                uint32_t backpatch = 0;
                int last_line = 0;
                TB_FileID last_file = 0;
                uint32_t current_line_count = 0;

                loop(line_id, line_count) {
                    TB_Line line = lines[line_id];

                    if (last_file != line.file) {
                        if (backpatch) {
                            tb_patch4b(&debugs_out, backpatch, current_line_count);
                            tb_patch4b(&debugs_out, backpatch + 4, 12 + (current_line_count * 8));
                        }
                        last_file = line.file;

                        // File entry
                        tb_out4b(&debugs_out, file_table_offset[line.file]);
                        backpatch = debugs_out.count;
                        tb_out4b(&debugs_out, 0);
                        tb_out4b(&debugs_out, 0);

                        // printf("  FILE %d\n", line.file);
                        current_line_count = 0;
                        last_line = 0;
                    }

                    if (last_line != line.line) {
                        last_line = line.line;
                        // printf("  * LINE %d : %x\n", line.line, body_start + line.pos);

                        tb_out4b(&debugs_out, line.pos ? body_start + line.pos : line.pos);
                        tb_out4b(&debugs_out, line.line);
                        current_line_count++;
                    }
                }

                // finalize the patch work
                if (backpatch) {
                    tb_patch4b(&debugs_out, backpatch, current_line_count);
                    tb_patch4b(&debugs_out, backpatch + 4, 12 + (current_line_count * 8));
                }

                tb_patch4b(&debugs_out, field_length_patch, (debugs_out.count - field_length_patch) - 4);
                // printf("\n");
            }
        }

        // Symbol table
        tb_out4b(&debugs_out, 0x000000F1);

        size_t field_length_patch = debugs_out.count;
        tb_out4b(&debugs_out, 0);

        // Symbol info object
        {
            uint32_t obj_length = 2 + 4 + path_len;
            tb_out2b(&debugs_out, obj_length);
            tb_out2b(&debugs_out, 0x1101);
            tb_out4b(&debugs_out, 0);

            tb_out_reserve(&debugs_out, sizeof(creator_str));
            tb_outs_UNSAFE(&debugs_out, path_len, (const uint8_t*)path);
        }

        // Symbol info properties
        {
            tb_out2b(&debugs_out, creator_length);
            tb_out2b(&debugs_out, 0x1116);
            tb_out4b(&debugs_out, 0);

            tb_out2b(&debugs_out, 0x00D0); // machine
            tb_out2b(&debugs_out, 0);      // verFEMajor
            tb_out2b(&debugs_out, 0);      // verFEMinor
            tb_out2b(&debugs_out, 0);      // verFEBuild

            tb_out2b(&debugs_out, TB_VERSION_MAJOR); // verMajor
            tb_out2b(&debugs_out, TB_VERSION_MINOR); // verMinor
            tb_out2b(&debugs_out, TB_VERSION_PATCH); // verBuild

            tb_out_reserve(&debugs_out, sizeof(creator_str));
            tb_outs_UNSAFE(&debugs_out, sizeof(creator_str), (const uint8_t*)creator_str);

            tb_out2b(&debugs_out, 0);
        }

        // Symbols
        loop(i, m->functions.count) {
            TB_Function* f = &m->functions.data[i];
            TB_FunctionOutput* out_f = f->output;
            if (!out_f) continue;

            const char* name = f->name;
            size_t name_len = strlen(f->name) + 1;

            size_t baseline = debugs_out.count;
            tb_out2b(&debugs_out, 0);
            tb_out2b(&debugs_out, S_GPROC32_ID);

            tb_out4b(&debugs_out, 0); // pointer to the parent
            tb_out4b(&debugs_out, 0); // pointer to this blocks end (left as zero?)
            tb_out4b(&debugs_out, 0); // pointer to the next symbol (left as zero?)

            // TODO(NeGate): correctly fill this
            tb_out4b(&debugs_out, 1);                      // procedure length
            tb_out4b(&debugs_out, 0);                      // debug start offset (?)
            tb_out4b(&debugs_out, 0);                      // debug end offset (?)
            tb_out4b(&debugs_out, function_type_table[i]); // type index

            // we save this location because there's two relocations
            // we'll put there:
            //   type      target     size
            //   SECREL    .text     4 bytes
            //   SECTION   .text     2 bytes
            {
                size_t patch_pos = debugs_out.count;

                add_reloc(&sections[0], &(TB_ObjectReloc){
                        TB_OBJECT_RELOC_SECREL,
                        function_sym_start + i,
                        patch_pos
                    });

                add_reloc(&sections[0], &(TB_ObjectReloc){
                        TB_OBJECT_RELOC_SECTION,
                        function_sym_start + i,
                        patch_pos + 4
                    });
            }
            tb_out4b(&debugs_out, 0); // offset
            tb_out2b(&debugs_out, 0); // segment

            // the 1 means we have a frame pointer present
            tb_out1b(&debugs_out, 1); // flags

            tb_out_reserve(&debugs_out, name_len);
            tb_outs_UNSAFE(&debugs_out, name_len, (const uint8_t*)name);

            // patch field length
            tb_patch2b(&debugs_out, baseline, (debugs_out.count - baseline) - 2);

            if (1) {
                // frameproc
                size_t frameproc_baseline = debugs_out.count;

                tb_out2b(&debugs_out, 0);
                tb_out2b(&debugs_out, S_FRAMEPROC);

                size_t stack_usage = out_f->stack_usage == 8 ? 0 : out_f->stack_usage;

                tb_out4b(&debugs_out, stack_usage); // count of bytes of total frame of procedure
                tb_out4b(&debugs_out, 0); // count of bytes of padding in the frame
                tb_out4b(&debugs_out, 0); // offset (relative to frame poniter) to where padding starts
                tb_out4b(&debugs_out, 0); // count of bytes of callee save registers
                tb_out4b(&debugs_out, 0); // offset of exception handler
                tb_out4b(&debugs_out, 0); // section id of exception handler
                tb_out4b(&debugs_out, 0); // flags

                tb_patch2b(&debugs_out, frameproc_baseline, (debugs_out.count - frameproc_baseline) - 2);
            }

            // end the block
            tb_out2b(&debugs_out, 2);
            tb_out2b(&debugs_out, S_PROC_ID_END);
        }
        tb_patch4b(&debugs_out, field_length_patch, (debugs_out.count - field_length_patch) - 4);

        size_t pad = 4 - (debugs_out.count % 4);
        if (pad == 4) pad = 0;
        while (pad--) tb_out1b(&debugs_out, 0x00);
    }
    tb_tls_restore(tls, file_table_offset);

    sections[0].raw_data = (TB_Slice){ debugs_out.count, debugs_out.data };
    sections[1].raw_data = (TB_Slice){ debugt_out.count, debugt_out.data };

    return (TB_SectionGroup) {
        2, sections
    };
}

IDebugFormat codeview_debug_format = {
    "CodeView",
    codeview_supported_target,
    codeview_number_of_debug_sections,
    codeview_generate_debug_info
};
