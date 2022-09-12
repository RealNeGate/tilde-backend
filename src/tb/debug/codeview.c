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

static void align_up_type_record(TB_Emitter* e) {
    // type records need to be 4 byte aligned
    size_t pad = align_up(e->count, 4) - e->count;

    while (pad--) {
        tb_out1b(e, LF_PAD0 + pad);
    }
}

static uint16_t find_or_make_cv_type(TB_Emitter* sect, uint32_t* type_entry_count, CV_TypeEntry* lookup_table, size_t length, const void* k) {
    // Hash it
    const uint8_t* key = k;
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

    tb_outs(sect, length, key);
    // align_up_type_record(sect);
    return type_index;
}

static void align_up_emitter(TB_Emitter* e, size_t u) {
    size_t pad = align_up(e->count, u) - e->count;
    while (pad--) tb_out1b(e, 0x00);
}

static uint16_t convert_to_codeview_type(TB_Emitter* sect, uint32_t* type_entry_count, CV_TypeEntry* lookup_table, const TB_DebugType* type) {
    // find_or_make_cv_type(sect, type_entry_count, lookup_table, );
    switch (type->tag) {
        case TB_DEBUG_TYPE_VOID: return T_VOID;
        case TB_DEBUG_TYPE_BOOL: return T_BOOL08; // T_BOOL08

        case TB_DEBUG_TYPE_INT:
        case TB_DEBUG_TYPE_UINT: {
            bool is_signed = (type->tag == TB_DEBUG_TYPE_INT);

            if (type->int_bits <= 8)  return is_signed ? T_INT1 : T_UINT1;
            if (type->int_bits <= 16) return is_signed ? T_INT2 : T_UINT2;
            if (type->int_bits <= 32) return is_signed ? T_INT4 : T_UINT4;
            if (type->int_bits <= 64) return is_signed ? T_INT8 : T_UINT8;
            assert(0 && "Unsupported int type");
        }

        case TB_DEBUG_TYPE_FLOAT: {
            switch (type->float_fmt) {
                case TB_FLT_32: return T_REAL32;
                case TB_FLT_64: return T_REAL64;
                default: assert(0 && "Unknown float type");
            }
        }

        case TB_DEBUG_TYPE_POINTER: {
            CV_LFPointer ptr = {
                .len = sizeof(CV_LFPointer) - 2,
                .leaf = LF_POINTER,
                .utype = convert_to_codeview_type(sect, type_entry_count, lookup_table, type->ptr_to),
                .attr = {
                    .ptrtype = 0x0c, // CV_PTR_64
                    .ptrmode = 0,    // CV_PTR_MODE_PTR
                    .size    = 8,
                }
            };

            return find_or_make_cv_type(sect, type_entry_count, lookup_table, sizeof(CV_LFPointer), &ptr);
        }

        case TB_DEBUG_TYPE_STRUCT: {
            TB_TemporaryStorage* tls = tb_tls_steal();
            uint16_t* list = tb_tls_push(tls, type->struct_.count * sizeof(uint16_t));
            FOREACH_N(i, 0, type->struct_.count) {
                const TB_DebugType* f = type->struct_.members[i];
                assert(f->tag == TB_DEBUG_TYPE_FIELD);

                list[i] = convert_to_codeview_type(sect, type_entry_count, lookup_table, f->field.type);
            }

            // write field list
            uint16_t fields_type_index = *type_entry_count;
            *type_entry_count += 1;

            size_t patch_pos = sect->count;
            tb_out2b(sect, 0); // length (we'll patch it later)
            tb_out2b(sect, LF_FIELDLIST); // type

            FOREACH_N(i, 0, type->struct_.count) {
                const TB_DebugType* f = type->struct_.members[i];
                size_t name_len = strlen(f->field.name);
                assert(f->tag == TB_DEBUG_TYPE_FIELD);

                // First the member, then offset (using any of the variable length records)
                // then it's an LF_STRING for the name
                CV_LFMember m = {
                    .leaf = LF_MEMBER,
                    .index = list[i],
                };
                tb_outs(sect, sizeof(CV_LFMember), &m);

                // write offset
                tb_out2b(sect, LF_LONG);
                tb_out4b(sect, f->field.offset);

                // write out C string
                tb_outs(sect, name_len + 1, f->field.name);
            }
            tb_patch2b(sect, patch_pos, (sect->count - patch_pos) - 2);

            // write struct type
            uint16_t struct_type_index = *type_entry_count;
            *type_entry_count += 1;

            // hash pointer for a simple name
            char tmp[10];
            int tmp_len = snprintf(tmp, 10, "S%x", hash_buffer(0, sizeof(type), &type));

            CV_LFStruct s = {
                .len = sizeof(CV_LFStruct) + sizeof(CV_LFLong) + (tmp_len + 1) - 2,
                .leaf = LF_STRUCTURE,
                .count = type->struct_.count,
                .field = fields_type_index,
            };
            tb_outs(sect, sizeof(s), &s);

            // write size (simple LF_LONG)
            tb_out2b(sect, LF_LONG);
            tb_out4b(sect, type->struct_.size);

            // TODO(NeGate): write a real name
            tb_outs(sect, tmp_len + 1, tmp);

            tb_tls_restore(tls, list);
            return struct_type_index;
        }

        default:
        assert(0 && "TODO: missing type in CodeView output");
        return 0x0003;
    }
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

    CV_TypeEntry* lookup_table = tb_tls_push(tls, MAX_TYPE_ENTRY_LOOKUP_SIZE * sizeof(CV_TypeEntry));
    memset(lookup_table, 0, MAX_TYPE_ENTRY_LOOKUP_SIZE * sizeof(CV_TypeEntry));
    uint32_t type_entry_count = 0x1000;
    {
        tb_out4b(&debugt_out, 0x00000004);

        FOREACH_N(i, 0, m->functions.count) {
            if (m->functions.data[i].output == NULL) continue;
            const TB_FunctionPrototype* proto = m->functions.data[i].prototype;

            // Create argument list
            uint16_t arg_list;
            {
                size_t param_count = proto->param_count + proto->has_varargs;

                size_t length = 2 + 2 + 4 + (4 * param_count);
                uint8_t* data = tb_tls_push(tls, length);

                *((uint16_t*)&data[0]) = length - 2;
                *((uint16_t*)&data[2]) = LF_ARGLIST;
                *((uint32_t*)&data[4]) = param_count;

                uint32_t* param_data = (uint32_t*)&data[8];
                FOREACH_N(j, 0, proto->param_count) {
                    uint32_t type_index;
                    if (proto->params[j].debug_type != NULL) {
                        type_index = convert_to_codeview_type(&debugt_out, &type_entry_count, lookup_table, proto->params[j].debug_type);
                    } else {
                        type_index = get_codeview_type(proto->params[j].dt);
                    }

                    param_data[j] = type_index;
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
                    .leaf = LF_PROCEDURE,
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
        if (true) {
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
            align_up_emitter(&debugs_out, 4);
        }

        // Source file table
        // we practically transmute the file_table_offset from meaning file string
        // table entries into source file entries.
        if (true) {
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
            align_up_emitter(&debugs_out, 4);
        }

        // Line info table
        if (true) {
            FOREACH_N(func_id, 0, m->functions.count) {
                TB_FunctionOutput* out_f = m->functions.data[func_id].output;
                if (!out_f) continue;

                // Layout crap
                uint32_t function_start = func_layout[func_id];
                uint32_t function_end = func_layout[func_id + 1];

                uint32_t body_start = out_f->prologue_length;

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

                FOREACH_N(line_id, 0, line_count) {
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

            align_up_emitter(&debugs_out, 4);
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
        FOREACH_N(i, 0, m->functions.count) {
            TB_Function* f = &m->functions.data[i];
            TB_FunctionOutput* out_f = f->output;
            if (!out_f) continue;

            // Layout crap
            uint32_t function_start = func_layout[i];
            uint32_t function_end = func_layout[i + 1];
            uint32_t function_length = function_end - function_start;

            const char* name = f->name;
            size_t name_len = strlen(f->name) + 1;

            size_t baseline = debugs_out.count;
            tb_out2b(&debugs_out, 0);
            tb_out2b(&debugs_out, S_GPROC32_ID);

            tb_out4b(&debugs_out, 0); // pointer to the parent
            tb_out4b(&debugs_out, 0); // pointer to this blocks end (left as zero?)
            tb_out4b(&debugs_out, 0); // pointer to the next symbol (left as zero?)

            // TODO(NeGate): correctly fill this
            tb_out4b(&debugs_out, function_length);        // procedure length
            tb_out4b(&debugs_out, 0);                      // debug start offset (?)
            tb_out4b(&debugs_out, function_length);        // debug end offset (?)
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
                tb_out2b(&debugs_out, 0); // section id of exception handler
                tb_out4b(&debugs_out, 0x00114200); // flags

                tb_patch2b(&debugs_out, frameproc_baseline, (debugs_out.count - frameproc_baseline) - 2);

                dyn_array_for(j, out_f->stack_slots) {
                    int stack_pos = out_f->stack_slots[j].position;
                    const TB_DebugType* type = out_f->stack_slots[j].storage_type;

                    const char* var_name = out_f->stack_slots[j].name;
                    size_t var_name_len = strlen(var_name);
                    assert(var_name != NULL);

                    uint32_t type_index = convert_to_codeview_type(&debugt_out, &type_entry_count, lookup_table, type);

                    // define S_REGREL32
                    CV_RegRel32 l = {
                        .reclen = sizeof(CV_RegRel32) + (var_name_len + 1) - 2,
                        .rectyp = S_REGREL32,
                        .off = stack_pos,
                        .typind = type_index,
                        // AMD64_RBP is 334, AMD64_RSP is 335
                        .reg = 334,
                    };
                    tb_outs(&debugs_out, sizeof(CV_RegRel32), &l);
                    tb_outs(&debugs_out, var_name_len + 1, (const uint8_t*) var_name);
                }
            }

            // end the block
            tb_out2b(&debugs_out, 2);
            tb_out2b(&debugs_out, S_PROC_ID_END);
        }
        tb_patch4b(&debugs_out, field_length_patch, (debugs_out.count - field_length_patch) - 4);
        align_up_emitter(&debugs_out, 4);
    }

    // finalize debugt_out
    align_up_emitter(&debugt_out, 4);
    sections[0].raw_data = (TB_Slice){ debugs_out.count, debugs_out.data };
    sections[1].raw_data = (TB_Slice){ debugt_out.count, debugt_out.data };

    return (TB_SectionGroup) { 2, sections };
}

IDebugFormat tb__codeview_debug_format = {
    "CodeView",
    codeview_supported_target,
    codeview_number_of_debug_sections,
    codeview_generate_debug_info
};
