#include "coff.h"

#define NL_STRING_MAP_INLINE
#define NL_STRING_MAP_IMPL
#include "../string_map.h"

typedef struct {
    NL_Slice name;
    // this is the location the thunk will call
    uint32_t ds_address;
    // this is the ID of the thunk
    uint32_t thunk_id;
} ImportThunk;

typedef struct {
    NL_Slice libpath;
    DynArray(ImportThunk) thunks;
} ImportTable;

typedef struct {
    TB_LinkerInput inputs;

    // symbol name -> imports
    NL_Strmap(int) import_nametable;
    DynArray(ImportTable) imports;
} LinkerCtx;

const static uint8_t dos_stub[] = {
    // header
    0x4d,0x5a,0x78,0x00,0x01,0x00,0x00,0x00,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x40,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x78,0x00,0x00,0x00,

    // machine code
    0x0e,0x1f,0xba,0x0e,0x00,0xb4,0x09,0xcd,0x21,0xb8,0x01,0x4c,0xcd,0x21,0x54,0x68,
    0x69,0x73,0x20,0x70,0x72,0x6f,0x67,0x72,0x61,0x6d,0x20,0x63,0x61,0x6e,0x6e,0x6f,
    0x74,0x20,0x62,0x65,0x20,0x72,0x75,0x6e,0x20,0x69,0x6e,0x20,0x44,0x4f,0x53,0x20,
    0x6d,0x6f,0x64,0x65,0x2e,0x24,0x00,0x00
};

static TB_Slice read_file_into_slice(FILE* file) {
    if (!file) {
        printf("Internal error: could not read file\n");
        abort();
    }

    int descriptor = fileno(file);
    struct stat file_stats;
    if (fstat(descriptor, &file_stats) == -1) {
        fclose(file);
        abort();
    }

    fseek(file, 0, SEEK_SET);
    uint8_t* buffer = malloc(file_stats.st_size);
    size_t length_read = fread(buffer, 1, file_stats.st_size, file);

    fclose(file);
    return (TB_Slice){length_read, buffer};
}

static FILE* locate_file(LinkerCtx* restrict ctx, const char* path) {
    FILE* file = fopen(path, "rb");
    if (file) return file;

    char temp_str[FILENAME_MAX];
    loop(i, ctx->inputs.search_dir_count) {
        snprintf(temp_str, FILENAME_MAX, "%s/%s", ctx->inputs.search_dirs[i], path);

        file = fopen(temp_str, "rb");
        if (file) return file;
    }

    return NULL;
}

static void pad_file(TB_TemporaryStorage* tls, FILE* file, char pad, size_t align) {
    size_t align_mask = align - 1;

    long i = ftell(file);
    long end = (i + align_mask) & ~align_mask;
    if (i == end) return;

    char* tmp = tb_tls_push(tls, end - i);
    memset(tmp, pad, end - i);
    fwrite(tmp, end - i, 1, file);
    tb_tls_restore(tls, tmp);
}

static void import_archive(LinkerCtx* restrict ctx, TB_ArchiveFile* restrict ar) {
    loop(i, ar->import_count) {
        TB_ArchiveImport* restrict import = &ar->imports[i];
        NL_Slice libname = { strlen(import->libname), (uint8_t*) import->libname };

        ptrdiff_t import_index = -1;
        dyn_array_for(j, ctx->imports) {
            ImportTable* table = &ctx->imports[j];

            if (table->libpath.length == libname.length &&
                memcmp(table->libpath.data, libname.data, libname.length) == 0) {
                import_index = j;
                break;
            }
        }

        if (import_index < 0) {
            // we haven't used this DLL yet, make an import table for it
            import_index = dyn_array_length(ctx->imports);
            dyn_array_put_uninit(ctx->imports, 1);

            ImportTable* t = &ctx->imports[import_index];
            t->libpath = libname;
            t->thunks = dyn_array_create(ImportThunk);
        }

        nl_strmap_put_cstr(ctx->import_nametable, import->name, import_index);
    }
}

static void align_up_emitter(TB_Emitter* e, size_t u) {
    size_t pad = align_up(e->count, u) - e->count;
    while (pad--) tb_out1b(e, 0x00);
}

void tb_export_pe(TB_Module* m, const ICodeGen* restrict code_gen, const TB_LinkerInput* restrict input, const char* path, const IDebugFormat* debug_fmt) {
    TB_TemporaryStorage* tls = tb_tls_allocate();
    LinkerCtx ctx = { .inputs = *input };

    // The prologue and epilogue generators need some storage
    uint8_t* proepi_buffer = tb_tls_push(tls, PROEPI_BUFFER);

    // Buffer stores all the positions of each
    // function relative to the .text section start.
    uint32_t* func_layout = tb_platform_heap_alloc((1+m->functions.count) * sizeof(uint32_t));

    // Generate import lookup table
    ctx.imports = dyn_array_create(ImportTable);

    loop(i, ctx.inputs.input_count) {
        FILE* file = locate_file(&ctx, ctx.inputs.inputs[i]);
        if (file == NULL) {
            tb_panic("Could not locate file: %s\n", ctx.inputs.inputs[i]);
        }

        printf("Symbols: %s\n", ctx.inputs.inputs[i]);
        TB_Slice buffer = read_file_into_slice(file);

        const char* dot = strrchr(ctx.inputs.inputs[i], '.');
        if (dot && strcmp(dot, ".lib") == 0) {
            TB_ArchiveFile* archive = tb_archive_parse_lib(buffer);
            import_archive(&ctx, archive);

            assert(archive->object_file_count == 0);
        } else {
            tb_todo();
        }
    }

    // Find all the imports & place them into the right buckets
    uint32_t thunk_id_counter = 0;
    loop(i, m->max_threads) {
        pool_for(TB_External, e, m->thread_info[i].externals) {
            ptrdiff_t search = nl_strmap_get_cstr(ctx.import_nametable, e->name);
            if (search < 0) {
                tb_panic("Could not link external: %s\n", e->name);
            }

            ImportTable* table = &ctx.imports[ctx.import_nametable[search]];

            ImportThunk t = { 0 };
            t.name = nl_slice__cstr(e->name);
            t.thunk_id = thunk_id_counter++;

            dyn_array_put(table->thunks, t);
            e->address = &table->thunks[dyn_array_length(table->thunks) - 1];
        }
    }

    // Generate bytes for the import table
    TB_Emitter import_table = { 0 };

    // add DLL import directories
    //
    // each entry is 20bytes and they're ordered by the imports so we
    // can patch back with that kind of knowledge
    dyn_array_for(i, ctx.imports) {
        tb_out4b(&import_table, 0); // import lookup table RVA
        tb_out4b(&import_table, 0); // timestamp
        tb_out4b(&import_table, 0); // forwarder chain
        tb_out4b(&import_table, 0); // name RVA
        tb_out4b(&import_table, 0); // import address table RVA
    }

    // null directory
    loop(i, 5) tb_out4b(&import_table, 0);

    dyn_array_for(i, ctx.imports) {
        ImportTable* imp = &ctx.imports[i];
        printf("\nIMPORT FOR '%.*s'\n", (int) imp->libpath.length, imp->libpath.data);

        // slap that lib name (these are RVAs)
        tb_patch4b(&import_table, (i * 20) + 12, 0x1000 + import_table.count);

        tb_out_reserve(&import_table, imp->libpath.length + 1);
        tb_outs_UNSAFE(&import_table, imp->libpath.length, imp->libpath.data);
        tb_out1b_UNSAFE(&import_table, 0x00);

        // setup a patch for the thunk array and then fill it out
        tb_patch4b(&import_table, (i * 20) + 16, 0x1000 + import_table.count);

        dyn_array_for(j, imp->thunks) {
            ImportThunk* t = &imp->thunks[j];
            printf("  %.*s\n", (int) t->name.length, t->name.data);

            imp->thunks[j].ds_address = import_table.count;
            tb_out8b(&import_table, 0);
        }

        // these structures are null terminated :P
        tb_out8b(&import_table, 0);
    }

    size_t import_entry_count = 0;
    dyn_array_for(i, ctx.imports) {
        ImportTable* imp = &ctx.imports[i];

        // fill all the C strings and then backpatch
        dyn_array_for(j, imp->thunks) {
            ImportThunk* t = &imp->thunks[j];

            uint32_t patch_pos = imp->thunks[j].ds_address;
            tb_patch4b(&import_table, patch_pos, 0x1000 + import_table.count);

            tb_out2b(&import_table, 0);
            tb_out_reserve(&import_table, t->name.length + 1);
            tb_outs_UNSAFE(&import_table, t->name.length, t->name.data);
            tb_out1b_UNSAFE(&import_table, 0x00);
        }

        import_entry_count += dyn_array_length(imp->thunks) + 1;
    }
    align_up_emitter(&import_table, 16);

    size_t iat_start = import_table.count;
    {
        // leave the IAT undefined for now, it's filled in in a sec
        tb_out_reserve(&import_table, import_entry_count * 8);
        tb_out_commit(&import_table, import_entry_count * 8);
        align_up_emitter(&import_table, 16);
    }

    // fill in IAT correctly
    import_entry_count = 0;
    dyn_array_for(i, ctx.imports) {
        ImportTable* imp = &ctx.imports[i];

        size_t thunk_count = dyn_array_length(imp->thunks);
        if (thunk_count != 0) {
            // duplicate ILT into IAT
            uint32_t src_pos = imp->thunks[0].ds_address;
            uint32_t dst_pos = iat_start + (import_entry_count * 8);

            // patch in IAT into the import directory table header
            tb_patch4b(&import_table, (i * 20), 0x1000 + dst_pos);

            memcpy(&import_table.data[dst_pos], &import_table.data[src_pos], thunk_count * 8);
            // null termination
            memset(&import_table.data[dst_pos + (thunk_count * 8)], 0, 8);
        }

        import_entry_count += thunk_count + 1;
    }

    size_t data_size = align_up(m->data_region_size, 512);
    size_t rdata_size = align_up(import_table.count + m->rdata_region_size, 512);

    size_t text_base = align_up(0x1000 + rdata_size, 4096);

    // generate trampolines
    size_t trampolines_size = 6 * thunk_id_counter;
    uint8_t* trampolines = malloc(trampolines_size);

    loop(i, thunk_id_counter) {
        // add trampoline JMP
        // NOTE(NeGate): this is x64 specific, we can technically generate
        // PE files for other platforms so that's something to think about
        trampolines[i*6 + 0] = 0xFF;
        trampolines[i*6 + 1] = 0x25;

        uint32_t actual_pos = text_base + (i*6);
        uint32_t target = 0x1000 + iat_start + (i*8);

        uint32_t p = (target - actual_pos) + 4;
        memcpy(&trampolines[i*6 + 2], &p, sizeof(uint32_t));
    }

    // Layout functions
    size_t text_section_size = align_up(trampolines_size, 16);
    size_t entrypoint = SIZE_MAX;
    loop(i, m->functions.count) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;
        func_layout[i] = text_section_size;
        if (out_f == NULL) continue;

        if (m->functions.data[i].name[0] == 'm' && strcmp(m->functions.data[i].name, "main") == 0) {
            entrypoint = text_section_size;
        }

        uint64_t meta = out_f->prologue_epilogue_metadata;
        uint64_t stack_usage = out_f->stack_usage;

        size_t code_size = out_f->code_size;
        size_t prologue = code_gen->get_prologue_length(meta, stack_usage);
        size_t epilogue = code_gen->get_epilogue_length(meta, stack_usage);
        assert(prologue + epilogue < PROEPI_BUFFER);

        text_section_size += prologue;
        text_section_size += epilogue;
        text_section_size += code_size;
    }
    func_layout[m->functions.count] = text_section_size;
    text_section_size = align_up(text_section_size, 512);

    size_t data_base = align_up(text_base + text_section_size, 4096);
    if (entrypoint == SIZE_MAX) {
        fprintf(stderr, "error: no entrypoint defined!\n");
        return;
    }

    // Target specific: resolve internal call patches
    code_gen->emit_call_patches(m, func_layout);

    // Apply external relocations
    ptrdiff_t const_data_rva = 0x1000 + import_table.count;
    ptrdiff_t global_data_rva = 0x1000 + import_table.count;
    loop(i, m->max_threads) {
        loop(j, dyn_array_length(m->thread_info[i].ecall_patches)) {
            TB_ExternFunctionPatch* patch = &m->thread_info[i].ecall_patches[j];
            TB_FunctionOutput* out_f = patch->source->output;

            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;
            uint8_t* code = out_f->code;

            size_t actual_pos = func_layout[patch->source - m->functions.data] +
                code_gen->get_prologue_length(meta, stack_usage) + patch->pos + 4;

            ImportThunk* thunk = patch->target->address;
            assert(thunk != NULL);

            uint32_t p = (thunk->thunk_id * 6) - actual_pos;
            memcpy(&code[patch->pos], &p, sizeof(uint32_t));
        }

        loop(j, dyn_array_length(m->thread_info[i].global_patches)) {
            TB_GlobalPatch* patch = &m->thread_info[i].global_patches[j];
            TB_FunctionOutput* out_f = patch->source->output;

            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;
            uint8_t* code = out_f->code;

            size_t actual_pos = func_layout[patch->source - m->functions.data] +
                code_gen->get_prologue_length(meta, stack_usage) + patch->pos + 4;

            const TB_Global* global = patch->target;
            (void)global;
            assert(global->storage == TB_STORAGE_DATA);

            int32_t p = global_data_rva - actual_pos;
            *((int32_t*)&code[patch->pos]) += p;
        }

        loop(j, dyn_array_length(m->thread_info[i].const_patches)) {
            TB_ConstPoolPatch* patch = &m->thread_info[i].const_patches[j];
            TB_FunctionOutput* out_f = patch->source->output;

            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;
            uint8_t* code = out_f->code;

            size_t actual_pos = text_base + func_layout[patch->source - m->functions.data] +
                code_gen->get_prologue_length(meta, stack_usage) + patch->pos + 4;

            int32_t p = const_data_rva - actual_pos;
            *((int32_t*)&code[patch->pos]) += p;
        }
    }

    FILE* f = fopen(path, "wb");

    // write DOS header
    fwrite(dos_stub, sizeof(dos_stub), 1, f);

    // write PE header
    PE_Header header = {
        .magic = 0x00004550,
        .machine = 0x8664,
        .section_count = 3,
        .timestamp = time(NULL),
        .symbol_table = 0,
        .symbol_count = 0,
        .size_of_optional_header = sizeof(PE_OptionalHeader64),
        .characteristics = 2 /* executable */
    };
    fwrite(&header, sizeof(header), 1, f);

    long size_of_headers = ftell(f)
        + sizeof(PE_OptionalHeader64)
        + (header.section_count * sizeof(PE_SectionHeader));

    PE_OptionalHeader64 opt_header = {
        .magic = 0x20b,

        .size_of_code = align_up(text_section_size, 0x1000),
        .size_of_initialized_data = rdata_size + data_size,
        .size_of_uninitialized_data = 0,

        .base_of_code = text_base,

        .section_alignment = 0x1000,
        .file_alignment = 0x200,

        .image_base = 0x00400000,
        .entrypoint = text_base + entrypoint,

        // 6.0
        .major_os_ver = 6,
        .minor_os_ver = 0,

        // 6.0
        .major_subsystem_ver = 6,
        .minor_subsystem_ver = 0,

        .size_of_image = data_base + data_size,
        .size_of_headers = (size_of_headers + 0x1FF) & ~0x1FF,
        .subsystem = 2 /* WINDOWS_GUI */,

        .size_of_stack_reserve = 2 << 20,
        .size_of_stack_commit  = 4096,

        .rva_size_count = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
        .data_directories = {
            [IMAGE_DIRECTORY_ENTRY_IMPORT] = { 0x1000, import_table.count },
            [IMAGE_DIRECTORY_ENTRY_IAT]  = { 0x1000 + iat_start, import_entry_count * 8 },
        }
    };
    fwrite(&opt_header, sizeof(opt_header), 1, f);

    // generate text section
    {
        long section_raw_data_pos = ftell(f) + (header.section_count * sizeof(PE_SectionHeader));
        section_raw_data_pos = align_up(section_raw_data_pos, 512);

        PE_SectionHeader sections[] = {
            {
                .name = { ".rdata" },
                .virtual_size = rdata_size,
                .virtual_address = 0x1000, // at the start of our section memory
                .size_of_raw_data = rdata_size,
                .pointer_to_raw_data = section_raw_data_pos,
                .characteristics = IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA,
            },
            {
                .name = { ".text" },
                .virtual_size = text_section_size,
                .virtual_address = text_base,
                .size_of_raw_data = text_section_size,
                .pointer_to_raw_data = section_raw_data_pos + rdata_size,
                .characteristics = IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE,
            },
            {
                .name = { ".data" },
                .virtual_size = data_size,
                .virtual_address = data_base,
                .size_of_raw_data = data_size,
                .pointer_to_raw_data = section_raw_data_pos + rdata_size + text_section_size,
                .characteristics = IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE,
            },
        };
        fwrite(sections, sizeof(sections), 1, f);
        pad_file(tls, f, 0x00, 0x200);

        {
            assert(ftell(f) == sections[0].pointer_to_raw_data);
            fwrite(import_table.data, import_table.count, 1, f);

            char* rdata = tb_platform_heap_alloc(m->rdata_region_size);
            loop(i, m->max_threads) {
                loop(j, dyn_array_length(m->thread_info[i].const_patches)) {
                    TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                    memcpy(&rdata[p->rdata_pos], p->data, p->length);
                }
            }

            fwrite(rdata, m->rdata_region_size, 1, f);
            pad_file(tls, f, 0x00, 0x200);
            tb_platform_heap_free(rdata);
        }

        assert(ftell(f) == sections[1].pointer_to_raw_data);
        fwrite(trampolines, trampolines_size, 1, f);
        pad_file(tls, f, 0xCC, 16);

        loop(i, m->functions.count) {
            TB_FunctionOutput* out_f = m->functions.data[i].output;
            if (!out_f) continue;

            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;
            const uint8_t* code = out_f->code;
            size_t code_size = out_f->code_size;

            uint8_t* prologue = proepi_buffer;
            size_t prologue_len = code_gen->emit_prologue(prologue, meta, stack_usage);

            uint8_t* epilogue = proepi_buffer + prologue_len;
            size_t epilogue_len = code_gen->emit_epilogue(epilogue, meta, stack_usage);

            fwrite(prologue, prologue_len, 1, f);
            fwrite(code, code_size, 1, f);
            fwrite(epilogue, epilogue_len, 1, f);
        }
        pad_file(tls, f, 0xCC, 0x200);

        assert(ftell(f) == sections[2].pointer_to_raw_data);
        {
            char* data = tb_platform_heap_alloc(m->data_region_size);

            loop(i, m->max_threads) {
                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    if (g->storage != TB_STORAGE_DATA) continue;
                    TB_Initializer* init = g->init;

                    // clear out space
                    memset(&data[g->pos], 0, init->size);

                    loop(k, init->obj_count) {
                        if (init->objects[k].type == TB_INIT_OBJ_REGION) {
                            memcpy(
                                &data[g->pos + init->objects[k].offset],
                                init->objects[k].region.ptr,
                                init->objects[k].region.size
                            );
                        } else {
                            tb_todo();
                        }
                    }
                }
            }

            fwrite(data, m->data_region_size, 1, f);
            tb_platform_heap_free(data);

            pad_file(tls, f, 0xCC, 0x200);
        }
    }

    fclose(f);
}
