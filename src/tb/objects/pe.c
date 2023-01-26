#include "coff.h"

#define NL_STRING_MAP_IMPL
#define NL_STRING_MAP_INLINE
#include "../string_map.h"

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

    uint64_t *iat, *ilt;
} ImportTable;

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

// we use a linked list to store these because i couldn't be bothered to allocate
// one giant sequential region for the entire linker.
struct TB_LinkerSectionPiece {
    TB_LinkerSectionPiece* next;

    enum {
        // write the data buffer in this struct
        PIECE_NORMAL,
        // Write the TB module's text section
        PIECE_TEXT,
        // Write the TB module's data section
        PIECE_DATA,
        // Write the TB module's rdata section
        PIECE_RDATA
    } kind;

    TB_Module* module;
    size_t offset, size;
    const uint8_t* data;
};

struct TB_LinkerSection {
    const char* name;

    // probably memory characteristics like EXECUTE (output format specific)
    uint32_t flags;

    size_t address; // usually a relative virtual address.
    size_t offset;  // in the file.

    size_t total_size;
    TB_LinkerSectionPiece *first, *last;
};

typedef struct TB_Linker {
    NL_Strmap(TB_LinkerSection*) sections;

    // relocations
    DynArray(TB_Module*) ir_modules;

    size_t trampoline_pos;  // relative to the .text section
    TB_Emitter trampolines; // these are for calling imported functions

    // symbol name -> imports
    NL_Strmap(int) import_nametable;
    DynArray(ImportTable) imports;

    // extra data
    ptrdiff_t entrypoint; // -1 if not available
} TB_Linker;

// also finds the entrypoints (kill two birds amirite)
static size_t layout_text_section(TB_Module* m, ptrdiff_t* entrypoint) {
    const char* entrypoint_name = "mainCRTStartup";
    char entrypoint_name_first_char = entrypoint_name[0];

    size_t size = 0;
    *entrypoint = -1;

    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;
        if (out_f != NULL) {
            if (f->super.name[0] == entrypoint_name_first_char) {
                if (strcmp(f->super.name, entrypoint_name) == 0) {
                    *entrypoint = size;
                }
            }

            out_f->code_pos = size;
            size += out_f->code_size;
        }
    }

    return size;
}

static size_t pad_file(uint8_t* output, size_t write_pos, char pad, size_t align) {
    size_t align_mask = align - 1;
    size_t end = (write_pos + align_mask) & ~align_mask;
    if (write_pos != end) {
        memset(output + write_pos, 0, end - write_pos);
        write_pos = end;
    }
    return write_pos;
}

static TB_LinkerSection* find_or_create_section(TB_Linker* linker, const char* name, uint32_t flags) {
    // allocate new section if one doesn't exist already
    ptrdiff_t search = nl_strmap_get_cstr(linker->sections, name);
    if (search < 0) {
        TB_LinkerSection* s = tb_platform_heap_alloc(sizeof(TB_LinkerSection));
        *s = (TB_LinkerSection){ .name = name, .flags = flags };

        nl_strmap_put_cstr(linker->sections, name, s);
        return s;
    } else {
        assert(linker->sections[search]->flags == flags);
        return linker->sections[search];
    }
}

static TB_LinkerSectionPiece* append_piece(TB_LinkerSection* section, int kind, size_t size, const void* data, TB_Module* mod) {
    // allocate some space for it, we might wanna make the total_size increment atomic
    TB_LinkerSectionPiece* piece = tb_platform_heap_alloc(sizeof(TB_LinkerSectionPiece));
    *piece = (TB_LinkerSectionPiece){
        .kind   = kind,
        .offset = section->total_size,
        .size   = size,
        .data   = data,
        .module = mod
    };
    section->total_size += size;

    if (section->last == NULL) {
        section->first = section->last = piece;
    } else {
        section->last->next = piece;
        section->last = piece;
    }
    return piece;
}

TB_API TB_Linker* tb_linker_create(void) {
    TB_Linker* l = tb_platform_heap_alloc(sizeof(TB_Linker));
    memset(l, 0, sizeof(TB_Linker));
    l->entrypoint = -1;
    return l;
}

TB_API void tb_linker_destroy(TB_Linker* l) {
    tb_platform_heap_free(l);
}

TB_API void tb_linker_append_library(TB_Linker* l, TB_ArchiveFile* ar) {
    FOREACH_N(i, 0, ar->import_count) {
        TB_ArchiveImport* restrict import = &ar->imports[i];
        TB_Slice libname = { strlen(import->libname), (uint8_t*) import->libname };

        ptrdiff_t import_index = -1;
        dyn_array_for(j, l->imports) {
            ImportTable* table = &l->imports[j];

            if (table->libpath.length == libname.length &&
                memcmp(table->libpath.data, libname.data, libname.length) == 0) {
                import_index = j;
                break;
            }
        }

        if (import_index < 0) {
            // we haven't used this DLL yet, make an import table for it
            import_index = dyn_array_length(l->imports);

            ImportTable t = {
                .libpath = libname,
                .thunks = dyn_array_create(ImportThunk, 4096)
            };
            dyn_array_put(l->imports, t);
            // printf("DLL %s:\n", import->libname);
        }

        // printf("  %s\n", import->name);
        nl_strmap_put_cstr(l->import_nametable, import->name, import_index);
    }

    if (ar->object_file_count > 0) {
        printf("LIBRARY\n");
        FOREACH_N(i, 0, ar->object_file_count) {
            printf("  %s\n", ar->object_file_names[i]);
        }

        tb_todo();
    }
}

TB_API void tb_linker_append_module(TB_Linker* l, TB_Module* m) {
    // Convert module into sections which we can then append to the output
    ptrdiff_t entry;
    TB_LinkerSection* text = find_or_create_section(l, ".text", IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
    m->linker.text = append_piece(text, PIECE_TEXT, layout_text_section(m, &entry), NULL, m);
    if (entry >= 0) {
        l->entrypoint = m->linker.text->offset + entry;
        printf("Found entry at %zu\n", l->entrypoint);
    }

    if (m->data_region_size > 0) {
        TB_LinkerSection* data = find_or_create_section(l, ".data", IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
        m->linker.data = append_piece(data, PIECE_DATA, m->data_region_size, NULL, m);
    }

    if (m->rdata_region_size > 0) {
        TB_LinkerSection* rdata = find_or_create_section(l, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
        m->linker.rdata = append_piece(rdata, PIECE_RDATA, m->rdata_region_size, NULL, m);
    }

    /*if (m->compiled_function_count > 0) {
        TB_LinkerSection* pdata = find_or_create_section(l, ".pdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
        append_piece(pdata, PIECE_PDATA, m->compiled_function_count * 12, NULL, m);
    }*/

    dyn_array_put(l->ir_modules, m);
}

static void apply_external_relocs(TB_Linker* l, TB_Module* m, uint8_t* output) {
    // ptrdiff_t global_data_rva = 0x1000 + import_table.count;
    TB_LinkerSection* text = find_or_create_section(l, ".text", IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
    TB_LinkerSection* data = find_or_create_section(l, ".data", IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    TB_LinkerSection* rdata = find_or_create_section(l, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);

    FOREACH_N(i, 0, m->max_threads) {
        uint64_t text_piece_rva = text->address + m->linker.text->offset;
        uint64_t text_piece_file = text->offset + m->linker.text->offset;

        uint64_t trampoline_rva = text->address + l->trampoline_pos;

        uint64_t data_piece_rva = 0;
        if (m->linker.data) {
            data_piece_rva = data->address + m->linker.data->offset;
        }

        FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].symbol_patches)) {
            TB_SymbolPatch* patch = &m->thread_info[i].symbol_patches[j];

            if (patch->target->tag == TB_SYMBOL_EXTERNAL) {
                TB_FunctionOutput* out_f = patch->source->output;
                size_t actual_pos = text_piece_rva + out_f->code_pos + out_f->prologue_length + patch->pos + 4;

                ImportThunk* thunk = patch->target->address;
                assert(thunk != NULL);

                int32_t p = (trampoline_rva + (thunk->thunk_id * 6)) - actual_pos;
                int32_t* dst = (int32_t*) &output[text_piece_file + out_f->code_pos + out_f->prologue_length + patch->pos];

                (*dst) += p;
            } else if (patch->target->tag == TB_SYMBOL_FUNCTION) {
                // internal patching has already handled this
            } else if (patch->target->tag == TB_SYMBOL_GLOBAL) {
                TB_FunctionOutput* out_f = patch->source->output;
                size_t actual_pos = text_piece_rva + out_f->code_pos + out_f->prologue_length + patch->pos + 4;

                TB_Global* global = (TB_Global*) patch->target;
                (void)global;
                assert(global->super.tag == TB_SYMBOL_GLOBAL);
                assert(global->storage == TB_STORAGE_DATA);

                int32_t p = data_piece_rva - actual_pos;
                int32_t* dst = (int32_t*) &output[text_piece_file + out_f->code_pos + out_f->prologue_length + patch->pos];

                (*dst) += p;
            } else {
                tb_todo();
            }
        }

        if (m->linker.rdata) {
            uint64_t rdata_piece_rva = rdata->address + m->linker.rdata->offset;

            FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].const_patches)) {
                TB_ConstPoolPatch* patch = &m->thread_info[i].const_patches[j];
                TB_FunctionOutput* out_f = patch->source->output;

                size_t actual_pos = text_piece_rva + out_f->code_pos + out_f->prologue_length + patch->pos + 4;
                int32_t* dst = (int32_t*) &output[text_piece_file + out_f->code_pos + out_f->prologue_length + patch->pos];

                // relocations add not replace
                (*dst) += (rdata_piece_rva - actual_pos);
            }
        }
    }
}

static void align_up_emitter(TB_Emitter* e, size_t u) {
    size_t pad = align_up(e->count, u) - e->count;
    while (pad--) tb_out1b(e, 0x00);
}

// returns the two new section pieces for the IAT and ILT
static COFF_ImportDirectory* gen_imports(TB_Linker* l, PE_ImageDataDirectory* imp_dir, PE_ImageDataDirectory* iat_dir) {
    int errors = 0;

    dyn_array_for(j, l->ir_modules) {
        TB_Module* m = l->ir_modules[j];

        // Find all the imports & place them into the right buckets
        uint32_t thunk_id_counter = 0;
        FOREACH_N(i, 0, m->max_threads) {
            pool_for(TB_External, ext, m->thread_info[i].externals) {
                ptrdiff_t search = nl_strmap_get_cstr(l->import_nametable, ext->super.name);
                if (search < 0) {
                    printf("tblink: unresolved external: %s\n", ext->super.name);
                    errors++;
                    continue;
                }

                ImportTable* table = &l->imports[l->import_nametable[search]];
                ImportThunk t = {
                    .name = (TB_Slice){ strlen(ext->super.name), (uint8_t*) ext->super.name },
                    .thunk_id = thunk_id_counter++,
                };

                dyn_array_put(table->thunks, t);
                ext->super.address = &table->thunks[dyn_array_length(table->thunks) - 1];
            }
        }
    }

    if (errors > 0) {
        tb_panic("Failed to link with errors!\n");
    }

    // cull any import directory
    size_t j = 0;
    size_t import_entry_count = 0;
    dyn_array_for(i, l->imports) {
        if (dyn_array_length(l->imports[i].thunks) != 0) {
            if (i != j) {
                l->imports[j] = l->imports[i];
            }
            j += 1;

            // there's an extra NULL terminator for the import entry lists
            import_entry_count += dyn_array_length(l->imports[i].thunks) + 1;
        }
    }
    dyn_array_set_length(l->imports, j); // trimmed

    // Generate import thunks
    l->trampolines = (TB_Emitter){ 0 };
    dyn_array_for(i, l->imports) {
        ImportTable* imp = &l->imports[i];

        dyn_array_for(j, imp->thunks) {
            imp->thunks[j].ds_address = l->trampolines.count;

            tb_out1b(&l->trampolines, 0xFF);
            tb_out1b(&l->trampolines, 0x25);
            // we're gonna relocate this to map onto an import thunk later
            tb_out4b(&l->trampolines, 0);
        }
    }

    ////////////////////////////////
    // Generate import table
    ////////////////////////////////
    size_t import_dir_size = (1 + dyn_array_length(l->imports)) * sizeof(COFF_ImportDirectory);
    size_t iat_size = import_entry_count * sizeof(uint64_t);
    size_t total_size = import_dir_size + 2*iat_size;
    dyn_array_for(i, l->imports) {
        ImportTable* imp = &l->imports[i];
        total_size += imp->libpath.length + 1;

        dyn_array_for(j, imp->thunks) {
            ImportThunk* t = &imp->thunks[j];
            total_size += t->name.length + 3;
        }
    }

    uint8_t* output = tb_platform_heap_alloc(total_size);

    COFF_ImportDirectory* import_dirs = (COFF_ImportDirectory*) &output[0];
    uint64_t* iat = (uint64_t*) &output[import_dir_size];
    uint64_t* ilt = (uint64_t*) &output[import_dir_size + iat_size];
    size_t strtbl_pos = import_dir_size + iat_size*2;

    // We put both the IAT and ILT into the rdata, the PE loader doesn't care but it
    // means the user can't edit these... at least not easily
    TB_LinkerSection* rdata = find_or_create_section(l, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    TB_LinkerSectionPiece* import_piece = append_piece(rdata, PIECE_NORMAL, total_size, output, NULL);

    *imp_dir = (PE_ImageDataDirectory){ import_piece->offset, import_dir_size };
    *iat_dir = (PE_ImageDataDirectory){ import_piece->offset + import_dir_size, iat_size };

    size_t p = 0;
    dyn_array_for(i, l->imports) {
        ImportTable* imp = &l->imports[i];
        COFF_ImportDirectory* header = &import_dirs[i];
        TB_Slice lib = imp->libpath;

        // after we resolve RVAs we need to backpatch stuff
        imp->iat = &iat[p], imp->ilt = &ilt[p];

        *header = (COFF_ImportDirectory){
            .import_lookup_table  = import_piece->offset + import_dir_size + iat_size + p*sizeof(uint64_t),
            .import_address_table = import_piece->offset + import_dir_size + p*sizeof(uint64_t),
            .name = import_piece->offset + strtbl_pos,
        };

        memcpy(&output[strtbl_pos], lib.data, lib.length), strtbl_pos += lib.length;
        output[strtbl_pos++] = 0;

        dyn_array_for(j, imp->thunks) {
            ImportThunk* t = &imp->thunks[j];
            // printf("  %.*s\n", (int) t->name.length, t->name.data);

            // import-by-name
            uint64_t value = import_piece->offset + strtbl_pos;
            memset(&output[strtbl_pos], 0, 2), strtbl_pos += 2;
            memcpy(&output[strtbl_pos], t->name.data, t->name.length), strtbl_pos += t->name.length;
            output[strtbl_pos++] = 0;

            // both the ILT and IAT are practically identical at this point
            iat[p] = ilt[p] = value, p++;
        }

        // NULL terminated
        iat[p] = ilt[p] = 0, p++;
    }
    assert(p == import_entry_count);

    // add NULL import directory at the end
    import_dirs[dyn_array_length(l->imports)] = (COFF_ImportDirectory){ 0 };

    {
        TB_LinkerSection* text = find_or_create_section(l, ".text", IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
        TB_LinkerSectionPiece* piece = append_piece(text, PIECE_NORMAL, l->trampolines.count, l->trampolines.data, NULL);
        l->trampoline_pos = piece->offset;
    }
    return import_dirs;
}

#define WRITE(data, size) (memcpy(&output[write_pos], data, size), write_pos += (size))
TB_API TB_Exports tb_linker_export(TB_Linker* l) {
    PE_ImageDataDirectory imp_dir, iat_dir;
    COFF_ImportDirectory* import_dirs = gen_imports(l, &imp_dir, &iat_dir);

    size_t size_of_headers =
        sizeof(dos_stub)
        + sizeof(PE_Header)
        + sizeof(PE_OptionalHeader64)
        + (nl_strmap_get_load(l->sections) * sizeof(PE_SectionHeader));

    size_of_headers = align_up(size_of_headers, 512);

    size_t pe_code_size   = 0; // bytes in total marked as IMAGE_SCN_CNT_CODE
    size_t pe_init_size   = 0; // bytes in total marked as IMAGE_SCN_CNT_INITIALIZED_DATA
    size_t pe_uninit_size = 0; // bytes in total marked as IMAGE_SCN_CNT_UNINITIALIZED_DATA

    size_t section_content_size = 0;
    uint64_t virt_addr = 0x1000; // this area is reserved for the PE header stuff
    nl_strmap_for(i, l->sections) {
        TB_LinkerSection* s = l->sections[i];
        if (s->flags & IMAGE_SCN_CNT_CODE) pe_code_size += s->total_size;
        if (s->flags & IMAGE_SCN_CNT_INITIALIZED_DATA) pe_init_size += s->total_size;
        if (s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) pe_uninit_size += s->total_size;

        s->offset = size_of_headers + section_content_size;
        section_content_size += align_up(s->total_size, 512);

        s->address = virt_addr;
        virt_addr += align_up(s->total_size, 4096);
    }

    TB_LinkerSection* text = find_or_create_section(l, ".text", IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
    TB_LinkerSection* rdata = find_or_create_section(l, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    iat_dir.virtual_address += rdata->address;
    imp_dir.virtual_address += rdata->address;
    dyn_array_for(i, l->imports) {
        ImportTable* imp = &l->imports[i];
        COFF_ImportDirectory* header = &import_dirs[i];
        header->import_lookup_table  += rdata->address;
        header->import_address_table += rdata->address;
        header->name += rdata->address;

        uint64_t *ilt = imp->ilt, *iat = imp->iat;

        uint64_t iat_rva = header->import_address_table;
        uint64_t trampoline_rva = text->address + l->trampoline_pos;

        dyn_array_for(j, imp->thunks) {
            assert(iat[j] == ilt[j]);

            if (iat[j] != 0) {
                iat[j] += rdata->address;
                ilt[j] += rdata->address;
            }

            // Relocate trampoline entries to point into the IAT, the PE loader will
            // fill these slots in with absolute addresses of the symbols.
            int32_t* trampoline_dst = (int32_t*) &l->trampolines.data[imp->thunks[j].ds_address + 2];
            assert(*trampoline_dst == 0 && "We set this earlier... why isn't it here?");

            (*trampoline_dst) += (iat_rva + j*8) - (trampoline_rva + imp->thunks[j].ds_address + 6);
        }
    }

    size_t output_size = size_of_headers + section_content_size;
    PE_Header header = {
        .magic = 0x00004550,
        .machine = 0x8664,
        .section_count = nl_strmap_get_load(l->sections),
        .timestamp = time(NULL),
        .symbol_table = 0,
        .symbol_count = 0,
        .size_of_optional_header = sizeof(PE_OptionalHeader64),
        .characteristics = 2 /* executable */
    };

    PE_OptionalHeader64 opt_header = {
        .magic = 0x20b,
        .section_alignment = 0x1000,
        .file_alignment = 0x200,

        .image_base = 0x140000000,

        .size_of_code = pe_code_size,
        .size_of_initialized_data = pe_init_size,
        .size_of_uninitialized_data = pe_uninit_size,

        // 6.0
        .major_os_ver = 6,
        .minor_os_ver = 0,

        // 6.0
        .major_subsystem_ver = 6,
        .minor_subsystem_ver = 0,

        .size_of_image = virt_addr,
        .size_of_headers = (size_of_headers + 0x1FF) & ~0x1FF,
        .subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI,

        .size_of_stack_reserve = 2 << 20,
        .size_of_stack_commit = 4096,

        .rva_size_count = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
        .data_directories = {
            [IMAGE_DIRECTORY_ENTRY_IMPORT] = imp_dir,
            [IMAGE_DIRECTORY_ENTRY_IAT] = iat_dir,
        }
    };

    // text section crap
    if (text) {
        opt_header.base_of_code = text->address;
        opt_header.size_of_code = align_up(text->total_size, 4096);

        if (l->entrypoint >= 0) {
            opt_header.entrypoint = text->address + l->entrypoint;
        } else {
            printf("tblink: could not find entrypoint!\n");
        }
    }

    size_t write_pos = 0;
    uint8_t* restrict output = tb_platform_heap_alloc(output_size);
    WRITE(dos_stub, sizeof(dos_stub));
    WRITE(&header, sizeof(PE_Header));
    WRITE(&opt_header, sizeof(PE_OptionalHeader64));

    nl_strmap_for(i, l->sections) {
        TB_LinkerSection* s = l->sections[i];
        PE_SectionHeader sec_header = {
            .virtual_size = align_up(s->total_size, 4096),
            .virtual_address = s->address,
            .pointer_to_raw_data = s->offset,
            .size_of_raw_data = s->total_size,
            .characteristics = s->flags,
        };
        strncpy(sec_header.name, s->name, 8);
        WRITE(&sec_header, sizeof(sec_header));
    }
    write_pos = pad_file(output, write_pos, 0x00, 0x200);

    // write section contents
    // TODO(NeGate): we can actually parallelize this part of linking
    nl_strmap_for(i, l->sections) {
        TB_LinkerSection* s = l->sections[i];
        assert(s->offset == write_pos);

        for (TB_LinkerSectionPiece* p = s->first; p != NULL; p = p->next) {
            uint8_t* p_out = &output[write_pos];
            TB_Module* m = p->module;

            switch (p->kind) {
                case PIECE_NORMAL: {
                    memcpy(p_out, p->data, p->size);
                    break;
                }
                case PIECE_TEXT: {
                    TB_FOR_FUNCTIONS(func, m) {
                        TB_FunctionOutput* out_f = func->output;
                        if (out_f) {
                            memcpy(p_out, out_f->code, out_f->code_size);
                            p_out += out_f->code_size;
                        }
                    }

                    // Target specific: resolve internal call patches
                    tb__find_code_generator(m)->emit_call_patches(m);
                    break;
                }
                case PIECE_DATA: {
                    memset(p_out, 0, p->size);
                    FOREACH_N(i, 0, m->max_threads) {
                        pool_for(TB_Global, g, m->thread_info[i].globals) {
                            if (g->storage != TB_STORAGE_DATA) continue;
                            TB_Initializer* init = g->init;

                            // clear out space
                            memset(&p_out[g->pos], 0, init->size);

                            FOREACH_N(k, 0, init->obj_count) {
                                if (init->objects[k].type == TB_INIT_OBJ_REGION) {
                                    memcpy(
                                        &p_out[g->pos + init->objects[k].offset],
                                        init->objects[k].region.ptr,
                                        init->objects[k].region.size
                                    );
                                } else {
                                    tb_todo();
                                }
                            }
                        }
                    }
                    break;
                }
                case PIECE_RDATA: {
                    memset(p_out, 0, p->size);
                    FOREACH_N(i, 0, m->max_threads) {
                        dyn_array_for(j, m->thread_info[i].const_patches) {
                            TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                            memcpy(&p_out[p->rdata_pos], p->data, p->length);
                        }
                    }
                    break;
                }
                default: __debugbreak();
            }

            write_pos += p->size;
        }

        write_pos = pad_file(output, write_pos, 0x00, 512);
    }

    // TODO(NeGate): multithread this too
    dyn_array_for(i, l->ir_modules) {
        apply_external_relocs(l, l->ir_modules[i], output);
    }

    return (TB_Exports){ .count = 1, .files = { { output_size, output } } };
}
