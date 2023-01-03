#include "coff.h"

#define NL_STRING_MAP_IMPL
#define NL_STRING_MAP_INLINE
#include "../string_map.h"

enum {
    RVA_BASE = 0x1000,
    SECTION_COUNT = 3,
};

typedef struct {
    TB_Slice name;
    // this is the location the thunk will call
    uint32_t ds_address;
    // this is the ID of the thunk
    uint32_t thunk_id;
} ImportThunk;

typedef struct {
    TB_Slice libpath;
    DynArray(ImportThunk) thunks;
} ImportTable;

struct TB_ModuleExporter {
    const ICodeGen* code_gen;
    size_t write_pos;

    TB_LinkerInput* inputs;

    // headers
    PE_Header header;
    PE_OptionalHeader64 opt_header;
    PE_SectionHeader sections[SECTION_COUNT];

    // layout
    size_t text_base;

    // for allocating space in the image directories
    ptrdiff_t entrypoint; // -1 if not available

    // appended to the start of the text section, it holds all the trampolines for
    // DLL imports
    TB_Emitter trampolines;
    TB_Emitter import_table;

    // symbol name -> imports
    NL_Strmap(int) import_nametable;
    DynArray(ImportTable) imports;
};

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

#define WRITE(data, length_) write_data(e, output, length_, data)
static void write_data(TB_ModuleExporter* e, uint8_t* output, size_t length, const void* data) {
    memcpy(output + e->write_pos, data, length);
    e->write_pos += length;
}

static void set_data_directory(TB_ModuleExporter* e, size_t dir_i, size_t pos, size_t size) {
    e->opt_header.data_directories[dir_i].virtual_address = pos;
    e->opt_header.data_directories[dir_i].size = size;
}

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

static FILE* locate_file(TB_ModuleExporter* restrict e, const char* path) {
    FILE* file = fopen(path, "rb");
    if (file) return file;

    if (e->inputs != NULL) {
        char temp_str[FILENAME_MAX];
        FOREACH_N(i, 0, e->inputs->search_dir_count) {
            snprintf(temp_str, FILENAME_MAX, "%s/%s", e->inputs->search_dirs[i], path);

            file = fopen(temp_str, "rb");
            if (file) return file;
        }
    }

    return NULL;
}

static void pad_file(TB_ModuleExporter* restrict e, uint8_t* output, char pad, size_t align) {
    size_t align_mask = align - 1;

    size_t i = e->write_pos;
    size_t end = (i + align_mask) & ~align_mask;
    if (i != end) {
        memset(output + e->write_pos, 0, end - i);
        e->write_pos = end;
    }
}

static void import_archive(TB_ModuleExporter* restrict e, TB_ArchiveFile* restrict ar) {
    FOREACH_N(i, 0, ar->import_count) {
        TB_ArchiveImport* restrict import = &ar->imports[i];
        TB_Slice libname = { strlen(import->libname), (uint8_t*) import->libname };

        printf("DLL %s:\n", import->libname);
        ptrdiff_t import_index = -1;
        dyn_array_for(j, e->imports) {
            ImportTable* table = &e->imports[j];

            if (table->libpath.length == libname.length &&
                memcmp(table->libpath.data, libname.data, libname.length) == 0) {
                import_index = j;
                break;
            }
        }

        if (import_index < 0) {
            // we haven't used this DLL yet, make an import table for it
            import_index = dyn_array_length(e->imports);
            dyn_array_put_uninit(e->imports, 1);

            ImportTable* t = &e->imports[import_index];
            t->libpath = libname;
            t->thunks = dyn_array_create(ImportThunk);
        }

        printf("  %s\n", import->name);
        nl_strmap_put_cstr(e->import_nametable, import->name, import_index);
    }
}

static void align_up_emitter(TB_Emitter* e, size_t u) {
    size_t pad = align_up(e->count, u) - e->count;
    while (pad--) tb_out1b(e, 0x00);
}

////////////////////////////////
// PE imports
////////////////////////////////
static void gen_imports(TB_Module* restrict m, TB_ModuleExporter* restrict e) {
    int errors = 0;

    // Generate import lookup table
    e->imports = dyn_array_create(ImportTable);

    // Enumerate all extra archive and object files for DLL imports
    FOREACH_N(i, 0, e->inputs->input_count) {
        FILE* file = locate_file(e, e->inputs->inputs[i]);
        if (file == NULL) {
            tb_panic("Could not locate file: %s\n", e->inputs->inputs[i]);
        }

        printf("Symbols: %s\n", e->inputs->inputs[i]);
        TB_Slice buffer = read_file_into_slice(file);

        if (buffer.length >= 7 && memcmp(buffer.data, "!<arch>", 7) == 0) {
            TB_ArchiveFile* archive = tb_archive_parse_lib(buffer);
            import_archive(e, archive);

            assert(archive->object_file_count == 0);
        } else {
            tb_panic("Unknown linker input: %s\n", e->inputs->inputs[i]);
        }
    }

    // Find all the imports & place them into the right buckets
    uint32_t thunk_id_counter = 0;
    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_External, ext, m->thread_info[i].externals) {
            ptrdiff_t search = nl_strmap_get_cstr(e->import_nametable, ext->super.name);
            if (search < 0) {
                printf("unresolved external: %s\n", ext->super.name);
                errors++;
                continue;
            }

            ImportTable* table = &e->imports[e->import_nametable[search]];
            ImportThunk t = {
                .name = (TB_Slice){ strlen(ext->super.name), (uint8_t*) ext->super.name },
                .thunk_id = thunk_id_counter++,
            };

            dyn_array_put(table->thunks, t);
            ext->super.address = &table->thunks[dyn_array_length(table->thunks) - 1];
        }
    }

    if (errors > 0) {
        tb_panic("Failed to link with errors!\n");
    }

    // cull any import directory
    size_t j = 0;
    size_t import_entry_count = 0;
    dyn_array_for(i, e->imports) {
        if (dyn_array_length(e->imports[i].thunks) != 0) {
            if (i != j) {
                e->imports[j] = e->imports[i];
            }
            j += 1;

            import_entry_count += dyn_array_length(e->imports[i].thunks) + 1;
        }
    }
    dyn_array_set_length(e->imports, j); // trimmed

    // Generate import thunks
    dyn_array_for(i, e->imports) {
        ImportTable* imp = &e->imports[i];

        dyn_array_for(j, imp->thunks) {
            imp->thunks[j].ds_address = e->trampolines.count;

            tb_out1b(&e->trampolines, 0xFF);
            tb_out1b(&e->trampolines, 0x25);
            // gonna overlap a PC32 relocation
            tb_out4b(&e->trampolines, -4);
            // padding to 8bytes
            tb_out2b(&e->trampolines, 0x00);
        }
    }

    ////////////////////////////////
    // Generate import table
    ////////////////////////////////
    align_up_emitter(&e->import_table, 16);

    size_t import_dir_size = (1 + dyn_array_length(e->imports)) + sizeof(COFF_ImportDirectory);
    COFF_ImportDirectory* import_dirs = tb_out_grab(&e->import_table, import_dir_size);

    size_t iat_size = (dyn_array_length(e->imports) + import_entry_count) * sizeof(uint64_t);
    size_t iat_pos = tb_out_grab_i(&e->import_table, iat_size);
    size_t ilt_pos = tb_out_grab_i(&e->import_table, iat_size);

    set_data_directory(e, IMAGE_DIRECTORY_ENTRY_IMPORT, RVA_BASE, import_dir_size);
    set_data_directory(e, IMAGE_DIRECTORY_ENTRY_IAT,    RVA_BASE, + iat_size);

    dyn_array_for(i, e->imports) {
        ImportTable* imp = &e->imports[i];
        COFF_ImportDirectory* header = &import_dirs[i];

        header->import_lookup_table  = iat_pos + RVA_BASE;
        header->import_address_table = ilt_pos + RVA_BASE;

        header->name = e->import_table.count;
        tb_outs(&e->import_table, imp->libpath.length, imp->libpath.data);
        tb_out1b(&e->import_table, 0x00);

        dyn_array_for(j, imp->thunks) {
            ImportThunk* t = &imp->thunks[j];
            printf("  %.*s\n", (int) t->name.length, t->name.data);

            // import-by-name
            uint64_t value = e->import_table.count;
            tb_out2b(&e->import_table, 0x00);
            tb_outs(&e->import_table, t->name.length, t->name.data);
            tb_out1b(&e->import_table, 0x00);

            // both the ILT and IAT are practically identical at this point
            memcpy(&e->import_table.data[iat_pos], &value, sizeof(uint64_t));
            memcpy(&e->import_table.data[ilt_pos], &value, sizeof(uint64_t));

            iat_pos += sizeof(uint64_t);
            ilt_pos += sizeof(uint64_t);
        }
    }
    // add NULL import directory at the end
    import_dirs[dyn_array_length(e->imports)] = (COFF_ImportDirectory){ 0 };
}

// also finds the entrypoints (kill two birds amirite)
static size_t layout_text_section(TB_Module* m, TB_ModuleExporter* restrict e) {
    const char* entrypoint_name = "mainCRTStartup";
    char entrypoint_name_first_char = entrypoint_name[0];

    size_t size = align_up(e->trampolines.count, 16);
    e->entrypoint = -1;

    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;
        if (out_f != NULL) {
            if (f->super.name[0] == entrypoint_name_first_char) {
                if (strcmp(f->super.name, entrypoint_name) == 0) {
                    e->entrypoint = size;
                }
            }

            out_f->code_pos = size;
            size += out_f->code_size;
        }
    }
    size = align_up(size, 512);
    return size;
}

static void init_headers(TB_Module* restrict m, TB_ModuleExporter* restrict e) {
    size_t size_of_headers = sizeof(dos_stub) + sizeof(PE_Header) + sizeof(PE_OptionalHeader64) + (SECTION_COUNT * sizeof(PE_SectionHeader));
    e->header = (PE_Header){
        .magic = 0x00004550,
        .machine = 0x8664,
        .section_count = SECTION_COUNT,
        .timestamp = time(NULL),
        .symbol_table = 0,
        .symbol_count = 0,
        .size_of_optional_header = sizeof(PE_OptionalHeader64),
        .characteristics = 2 /* executable */
    };

    e->opt_header = (PE_OptionalHeader64){
        .magic = 0x20b,
        .size_of_uninitialized_data = 0,
        .section_alignment = 0x1000,
        .file_alignment = 0x200,

        .image_base = 0x00400000,

        // 6.0
        .major_os_ver = 6,
        .minor_os_ver = 0,

        // 6.0
        .major_subsystem_ver = 6,
        .minor_subsystem_ver = 0,

        .size_of_headers = (size_of_headers + 0x1FF) & ~0x1FF,
        .subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI,

        .size_of_stack_reserve = 2 << 20,
        .size_of_stack_commit = 4096,

        .rva_size_count = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
    };

    gen_imports(m, e);

    size_t data_size = align_up(m->data_region_size + 16, 512);
    size_t rdata_size = align_up(e->import_table.count + m->rdata_region_size, 512);
    e->text_base = align_up(0x1000 + rdata_size, 4096);

    // Layout functions (and find entrypoint)
    size_t text_section_size = layout_text_section(m, e);
    size_t data_base = align_up(e->text_base + text_section_size, 4096);
    if (e->entrypoint < 0) {
        fprintf(stderr, "error: no entrypoint defined!\n");
        abort();
    }

    e->opt_header.base_of_code = e->text_base;
    e->opt_header.size_of_code = align_up(text_section_size, 0x1000);
    e->opt_header.size_of_initialized_data = rdata_size + data_size;
    e->opt_header.entrypoint = e->text_base + e->entrypoint;
    e->opt_header.size_of_image = data_base + data_size;

    e->sections[0] = (PE_SectionHeader){
        .name = { ".rdata" },
        .virtual_size = rdata_size,
        .virtual_address = RVA_BASE, // at the start of our section memory
        .size_of_raw_data = rdata_size,
        .characteristics = IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA,
    };
    e->sections[1] = (PE_SectionHeader){
        .name = { ".text" },
        .virtual_size = text_section_size,
        .virtual_address = e->text_base,
        .size_of_raw_data = text_section_size,
        .characteristics = IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE,
    };
    e->sections[2] = (PE_SectionHeader){
        .name = { ".data" },
        .virtual_size = data_size,
        .virtual_address = data_base,
        .size_of_raw_data = data_size,
        .characteristics = IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_CNT_INITIALIZED_DATA,
    };
}

TB_API TB_Exports tb_pe_write_output(TB_Module* m, const IDebugFormat* dbg) {
    TB_ModuleExporter* e = tb_platform_heap_alloc(sizeof(TB_ModuleExporter));
    memset(e, 0, sizeof(TB_ModuleExporter));

    e->code_gen = tb__find_code_generator(m);

    init_headers(m, e);

    // Target specific: resolve internal call patches
    const ICodeGen* restrict code_gen = tb__find_code_generator(m);
    code_gen->emit_call_patches(m);

    // Apply external relocations
    ptrdiff_t const_data_rva = 0x1000 + e->import_table.count;
    // ptrdiff_t global_data_rva = 0x1000 + import_table.count;
    FOREACH_N(i, 0, m->max_threads) {
        FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].symbol_patches)) {
            TB_SymbolPatch* patch = &m->thread_info[i].symbol_patches[j];

            if (patch->target->tag == TB_SYMBOL_EXTERNAL) {
                TB_FunctionOutput* out_f = patch->source->output;

                size_t actual_pos = out_f->code_pos + out_f->prologue_length + patch->pos + 4;

                ImportThunk* thunk = patch->target->address;
                assert(thunk != NULL);

                uint32_t p = (thunk->thunk_id * 6) - actual_pos;
                memcpy(&out_f->code[patch->pos], &p, sizeof(uint32_t));
            } else {
                tb_todo();
            }
        }

        /*FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].global_patches)) {
            TB_SymbolPatch* patch = &m->thread_info[i].global_patches[j];
            TB_FunctionOutput* out_f = patch->source->output;

            size_t actual_pos = out_f->code_pos + out_f->prologue_length + patch->pos + 4;
            TB_Global* global = (TB_Global*) patch->target;
            assert(global->super.tag == TB_SYMBOL_GLOBAL);
            assert(global->storage == TB_STORAGE_DATA);

            int32_t p = global_data_rva - actual_pos;
            *((int32_t*)&out_f->code[patch->pos]) += p;
        }*/

        FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].const_patches)) {
            TB_ConstPoolPatch* patch = &m->thread_info[i].const_patches[j];
            TB_FunctionOutput* out_f = patch->source->output;

            size_t actual_pos = e->text_base + out_f->code_pos + out_f->prologue_length + patch->pos + 4;

            int32_t p = const_data_rva - actual_pos;
            *((int32_t*)&out_f->code[patch->pos]) += p;
        }
    }

    // Layout PE file
    size_t size_of_headers = sizeof(dos_stub) + sizeof(PE_Header) + sizeof(PE_OptionalHeader64) + (SECTION_COUNT * sizeof(PE_SectionHeader));
    size_t output_size = align_up(size_of_headers, 512);
    FOREACH_N(i, 0, SECTION_COUNT) {
        e->sections[i].pointer_to_raw_data = output_size;
        output_size += e->sections[i].size_of_raw_data;
    }

    uint8_t* restrict output = tb_platform_heap_alloc(output_size);
    WRITE(dos_stub, sizeof(dos_stub));
    WRITE(&e->header, sizeof(PE_Header));
    WRITE(&e->opt_header, sizeof(PE_OptionalHeader64));

    // generate text section
    {
        WRITE(e->sections, sizeof(e->sections));
        pad_file(e, output, 0x00, 0x200);

        {
            assert(e->write_pos == e->sections[0].pointer_to_raw_data);
            WRITE(e->import_table.data, e->import_table.count);

            char* rdata = tb_platform_heap_alloc(m->rdata_region_size);
            FOREACH_N(i, 0, m->max_threads) {
                dyn_array_for(j, m->thread_info[i].const_patches) {
                    TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                    memcpy(&rdata[p->rdata_pos], p->data, p->length);
                }
            }

            WRITE(rdata, m->rdata_region_size);
            pad_file(e, output, 0x00, 0x200);
            tb_platform_heap_free(rdata);
        }

        assert(e->write_pos == e->sections[1].pointer_to_raw_data);
        WRITE(e->trampolines.data, e->trampolines.count);
        pad_file(e, output, 0xCC, 16);

        TB_FOR_FUNCTIONS(func, m) {
            TB_FunctionOutput* out_f = func->output;
            if (out_f) WRITE(out_f->code, out_f->code_size);
        }
        pad_file(e, output, 0xCC, 0x200);

        assert(e->write_pos == e->sections[2].pointer_to_raw_data);
        {
            char* data = tb_platform_heap_alloc(m->data_region_size);

            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    if (g->storage != TB_STORAGE_DATA) continue;
                    TB_Initializer* init = g->init;

                    // clear out space
                    memset(&data[g->pos], 0, init->size);

                    FOREACH_N(k, 0, init->obj_count) {
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

            WRITE((const char[16]) { "TildeBackend" }, 16);
            WRITE(data, m->data_region_size);
            tb_platform_heap_free(data);

            pad_file(e, output, 0x00, 0x200);
        }
    }

    return (TB_Exports){ 0 };
}
