#include "coff.h"

void tb_export_coff(TB_Module* m, const ICodeGen* restrict code_gen, const char* path, const IDebugFormat* debug_fmt) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    // The prologue and epilogue generators need some storage
    uint8_t* proepi_buffer = tb_tls_push(tls, PROEPI_BUFFER);

    // Buffer stores all the positions of each
    // function relative to the .text section start.
    uint32_t* func_layout = tb_platform_heap_alloc((1+m->functions.count) * sizeof(uint32_t));

    // String table array, stores the strings which will be put
    // into the string table
    uint32_t string_table_length = 0;
    uint32_t string_table_mark = 4;
    uint32_t string_table_cap = 0;

    uint32_t data_relocation_count = 0;

    loop(t, m->max_threads) {
        pool_for(TB_Global, g, m->thread_info[t].globals) {
            TB_Initializer* init = g->init;
            loop(k, init->obj_count) {
                data_relocation_count += (init->objects[k].type != TB_INIT_OBJ_REGION);
            }
        }
    }

    int number_of_sections = 3
        + (m->tls_region_size ? 1 : 0)
        + (debug_fmt != NULL ? debug_fmt->number_of_debug_sections(m) : 0);

    // mark each with a unique id
    size_t function_sym_start = (number_of_sections * 2);
    size_t external_sym_start = function_sym_start + m->functions.compiled_count;

    size_t unique_id_counter = 0;
    loop(i, m->max_threads) {
        pool_for(TB_External, e, m->thread_info[i].externals) {
            int id = external_sym_start + unique_id_counter;
            e->address = (void*) (uintptr_t) id;
            unique_id_counter += 1;
        }

        pool_for(TB_Global, g, m->thread_info[i].globals) {
            g->id = external_sym_start + unique_id_counter;
            unique_id_counter += 1;
        }
    }

    string_table_cap += unique_id_counter;
    string_table_cap += m->functions.compiled_count;

    char** string_table = tb_platform_heap_alloc(string_table_cap * sizeof(const char*));

    COFF_FileHeader header = {
        .num_sections = number_of_sections,
        .timestamp = time(NULL),
        .symbol_count = 0,
        .symbol_table = 0,
        .characteristics = IMAGE_FILE_LINE_NUMS_STRIPPED
    };

    COFF_SectionHeader text_section = {
        .name = { ".text" }, // .text
        .characteristics = COFF_CHARACTERISTICS_TEXT
    };

    COFF_SectionHeader rdata_section = {
        .name = { ".rdata" }, // .rdata
        .characteristics = COFF_CHARACTERISTICS_RODATA,
        .raw_data_size = m->rdata_region_size
    };

    COFF_SectionHeader data_section = {
        .name = { ".data" }, // .data
        .characteristics = COFF_CHARACTERISTICS_DATA,
        .raw_data_size = m->data_region_size
    };

    COFF_SectionHeader tls_section = {
        .name = { ".tls$" },
        .characteristics = COFF_CHARACTERISTICS_DATA,
        .raw_data_size = m->tls_region_size
    };

    switch (m->target_arch) {
        case TB_ARCH_X86_64:  header.machine = COFF_MACHINE_AMD64; break;
        case TB_ARCH_AARCH64: header.machine = COFF_MACHINE_ARM64; break;
        default: tb_todo();
    }

    loop(i, m->functions.count) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;
        func_layout[i] = text_section.raw_data_size;
        if (out_f == NULL) continue;

        uint64_t meta = out_f->prologue_epilogue_metadata;
        uint64_t stack_usage = out_f->stack_usage;

        size_t code_size = out_f->code_size;
        size_t prologue = code_gen->get_prologue_length(meta, stack_usage);
        size_t epilogue = code_gen->get_epilogue_length(meta, stack_usage);
        assert(prologue + epilogue < PROEPI_BUFFER);

        text_section.raw_data_size += prologue;
        text_section.raw_data_size += epilogue;
        text_section.raw_data_size += code_size;
    }
    func_layout[m->functions.count] = text_section.raw_data_size;

    TB_SectionGroup debug_sections = { 0 };
    COFF_SectionHeader* debug_section_headers = NULL;
    if (debug_fmt != NULL) {
        debug_sections = debug_fmt->generate_debug_info(m, tls, code_gen, path, function_sym_start, func_layout);
        debug_section_headers = tb_platform_heap_alloc(debug_sections.length * sizeof(COFF_SectionHeader));

        loop(i, debug_sections.length) {
            debug_section_headers[i] = (COFF_SectionHeader) { 0 };
            debug_section_headers[i].characteristics = COFF_CHARACTERISTICS_CV;
            debug_section_headers[i].num_reloc = debug_sections.data[i].relocation_count;

            TB_Slice name = debug_sections.data[i].name;
            if (name.length > 8) {
                // we'd need to put it into the string table... im lazy
                // so i wont do the logic for it yet
                tb_todo();
            } else {
                memcpy(debug_section_headers[i].name, name.data, name.length);
                if (name.length < 8) debug_section_headers[i].name[name.length] = 0;
            }
        }
    }

    // Target specific: resolve internal call patches
    code_gen->emit_call_patches(m, func_layout);

    // symbols
    {
        header.symbol_count = (number_of_sections * 2) + m->functions.compiled_count;

        // total externals + total globals
        header.symbol_count += unique_id_counter;
    }

    // relocation
    {
        text_section.num_reloc = 0;
        loop(i, m->max_threads) {
            text_section.num_reloc += dyn_array_length(m->thread_info[i].const_patches);
            text_section.num_reloc += dyn_array_length(m->thread_info[i].ecall_patches);
            text_section.num_reloc += dyn_array_length(m->thread_info[i].global_patches);
        }

        data_section.num_reloc = data_relocation_count;
    }

    // layout sections & relocations
    size_t string_table_pos = 0;
    {
        size_t counter = sizeof(COFF_FileHeader) + (number_of_sections * sizeof(COFF_SectionHeader));

        text_section.raw_data_pos = tb_post_inc(&counter, text_section.raw_data_size);
        rdata_section.raw_data_pos = tb_post_inc(&counter, rdata_section.raw_data_size);
        data_section.raw_data_pos = tb_post_inc(&counter, data_section.raw_data_size);
        tls_section.raw_data_pos = tb_post_inc(&counter, tls_section.raw_data_size);

        loop(i, debug_sections.length) {
            debug_section_headers[i].raw_data_size = debug_sections.data[i].raw_data.length;
            debug_section_headers[i].raw_data_pos = tb_post_inc(&counter, debug_section_headers[i].raw_data_size);
        }

        // Do the relocation lists next
        text_section.pointer_to_reloc = tb_post_inc(&counter, text_section.num_reloc * sizeof(COFF_ImageReloc));
        data_section.pointer_to_reloc = tb_post_inc(&counter, data_section.num_reloc * sizeof(COFF_ImageReloc));

        loop(i, debug_sections.length) {
            debug_section_headers[i].pointer_to_reloc = tb_post_inc(&counter, debug_section_headers[i].num_reloc * sizeof(COFF_ImageReloc));
        }

        header.symbol_table = tb_post_inc(&counter, header.symbol_count * sizeof(COFF_Symbol));
        string_table_pos = counter;

        // it's only here for an assertion, so i'm making
        // sure it doesn't get mark as unused in release.
        ((void)string_table_pos);
    }

    FILE* f = fopen(path, "wb");
    fwrite(&header, sizeof(header), 1, f);
    fwrite(&text_section, sizeof(text_section), 1, f);
    fwrite(&rdata_section, sizeof(rdata_section), 1, f);
    fwrite(&data_section, sizeof(data_section), 1, f);

    if (tls_section.raw_data_size) {
        fwrite(&tls_section, sizeof(tls_section), 1, f);
    }

    fwrite(debug_section_headers, sizeof(COFF_SectionHeader), debug_sections.length, f);

    assert(ftell(f) == text_section.raw_data_pos);
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

        assert(ftell(f) == (text_section.raw_data_pos + func_layout[i]));

        fwrite(prologue, prologue_len, 1, f);
        fwrite(code, code_size, 1, f);
        fwrite(epilogue, epilogue_len, 1, f);
    }

    assert(ftell(f) == rdata_section.raw_data_pos);
    {
        char* rdata = tb_platform_heap_alloc(m->rdata_region_size);

        loop(i, m->max_threads) {
            loop(j, dyn_array_length(m->thread_info[i].const_patches)) {
                TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                memcpy(&rdata[p->rdata_pos], p->data, p->length);
            }
        }

        fwrite(rdata, m->rdata_region_size, 1, f);
        tb_platform_heap_free(rdata);
    }

    assert(ftell(f) == data_section.raw_data_pos);
    {
        // TODO(NeGate): Optimize this for size and speed, sometimes
        // there's huge sections will be filled with just empty regions
        // so it's probably best not to represent everything in a big
        // buffer.
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
                    }
                }
            }
        }

        fwrite(data, m->data_region_size, 1, f);
        tb_platform_heap_free(data);
    }

    assert(ftell(f) == tls_section.raw_data_pos);
    {
        // TODO(NeGate): Optimize this for size and speed, sometimes
        // there's huge sections will be filled with just empty regions
        // so it's probably best not to represent everything in a big
        // buffer.
        char* data = tb_platform_heap_alloc(m->tls_region_size);

        loop(i, m->max_threads) {
            pool_for(TB_Global, g, m->thread_info[i].globals) {
                if (g->storage != TB_STORAGE_TLS) continue;

                TB_Initializer* init = g->init;

                // clear out space
                memset(&data[g->pos], 0, init->size);

                loop(k, init->obj_count) {
                    const TB_InitObj* o = &init->objects[k];
                    if (o->type == TB_INIT_OBJ_REGION) {
                        memcpy(&data[g->pos + o->offset], o->region.ptr, o->region.size);
                    }
                }
            }
        }

        fwrite(data, m->tls_region_size, 1, f);
        tb_platform_heap_free(data);
    }

    // Emit debug info
    loop(i, debug_sections.length) {
        assert(ftell(f) == debug_section_headers[i].raw_data_pos);
        fwrite(debug_sections.data[i].raw_data.data, debug_sections.data[i].raw_data.length, 1, f);
    }

    assert(ftell(f) == text_section.pointer_to_reloc);
    loop(i, m->max_threads) {
        loop(j, dyn_array_length(m->thread_info[i].const_patches)) {
            TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
            TB_FunctionOutput* out_f = p->source->output;

            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;

            size_t actual_pos = func_layout[p->source - m->functions.data] + code_gen->get_prologue_length(meta, stack_usage) + p->pos;

            fwrite(&(COFF_ImageReloc) {
                    .Type = IMAGE_REL_AMD64_REL32,
                    .SymbolTableIndex = 2, // rdata section
                    .VirtualAddress = actual_pos
                },
                sizeof(COFF_ImageReloc), 1, f
            );
        }
    }

    loop(i, m->max_threads) {
        loop(j, dyn_array_length(m->thread_info[i].ecall_patches)) {
            TB_ExternFunctionPatch* p = &m->thread_info[i].ecall_patches[j];
            TB_FunctionOutput* out_f = p->source->output;

            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;

            size_t actual_pos = func_layout[p->source - m->functions.data] + code_gen->get_prologue_length(meta, stack_usage) + p->pos;
            int symbol_id = (uintptr_t) p->target->address;
            assert(symbol_id != 0);

            fwrite(&(COFF_ImageReloc) {
                    .Type = IMAGE_REL_AMD64_REL32,
                    .SymbolTableIndex = symbol_id,
                    .VirtualAddress = actual_pos
                }, sizeof(COFF_ImageReloc), 1, f
            );
        }
    }

    loop(i, m->max_threads) {
        loop(j, dyn_array_length(m->thread_info[i].global_patches)) {
            TB_GlobalPatch* p = &m->thread_info[i].global_patches[j];
            TB_FunctionOutput* out_f = p->source->output;

            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;

            size_t actual_pos = func_layout[p->source - m->functions.data] + code_gen->get_prologue_length(meta, stack_usage) + p->pos;
            const TB_Global* global = p->target;

            int symbol_id = global->id;
            assert(symbol_id != 0);

            fwrite(&(COFF_ImageReloc) {
                    .Type = global->storage == TB_STORAGE_TLS ? IMAGE_REL_AMD64_SECREL : IMAGE_REL_AMD64_REL32,
                    .SymbolTableIndex = symbol_id,
                    .VirtualAddress = actual_pos
                }, sizeof(COFF_ImageReloc), 1, f
            );
        }
    }

    assert(ftell(f) == data_section.pointer_to_reloc);
    loop(i, m->max_threads) {
        pool_for(TB_Global, g, m->thread_info[i].globals) {
            TB_Initializer* init = g->init;

            loop(k, init->obj_count) {
                size_t actual_pos = g->pos + init->objects[k].offset;

                switch (init->objects[k].type) {
                    case TB_INIT_OBJ_RELOC_GLOBAL: {
                        const TB_Global* g = init->objects[k].reloc_global;

                        fwrite(&(COFF_ImageReloc) {
                                .Type = IMAGE_REL_AMD64_ADDR64,
                                .SymbolTableIndex = g->id,
                                .VirtualAddress = actual_pos
                            }, sizeof(COFF_ImageReloc), 1, f);
                        break;
                    }

                    case TB_INIT_OBJ_RELOC_EXTERN: {
                        const TB_External* e = init->objects[k].reloc_extern;
                        int id = (uintptr_t) e->address;

                        fwrite(&(COFF_ImageReloc) {
                                .Type = IMAGE_REL_AMD64_ADDR64,
                                .SymbolTableIndex = id,
                                .VirtualAddress = actual_pos
                            }, sizeof(COFF_ImageReloc), 1, f);
                        break;
                    }

                    case TB_INIT_OBJ_RELOC_FUNCTION: {
                        int symbol_id = init->objects[k].reloc_function - m->functions.data;

                        fwrite(&(COFF_ImageReloc) {
                                .Type = IMAGE_REL_AMD64_ADDR64,
                                .SymbolTableIndex = function_sym_start + symbol_id,
                                .VirtualAddress = actual_pos
                            }, sizeof(COFF_ImageReloc), 1, f);
                        break;
                    }

                    default: break;
                }
            }
        }
    }

    // slap those nice debug section relocations
    loop(i, debug_sections.length) {
        assert(ftell(f) == debug_section_headers[i].pointer_to_reloc);

        loop(j, debug_sections.data[i].relocation_count) {
            TB_ObjectReloc* reloc = &debug_sections.data[i].relocations[j];

            COFF_ImageReloc out_reloc = {
                .SymbolTableIndex = reloc->symbol_index,
                .VirtualAddress = reloc->virtual_address
            };

            switch (reloc->type) {
                case TB_OBJECT_RELOC_SECREL: out_reloc.Type = IMAGE_REL_AMD64_SECREL; break;
                case TB_OBJECT_RELOC_SECTION: out_reloc.Type = IMAGE_REL_AMD64_SECTION; break;
                default: tb_todo();
            }

            fwrite(&out_reloc, sizeof(out_reloc), 1, f);
        }
    }

    assert(ftell(f) == header.symbol_table);
    int section_num = 1;

    fwrite(&(COFF_Symbol) {
            .short_name = { ".text" },
            .section_number = section_num,
            .storage_class = IMAGE_SYM_CLASS_STATIC,
            .aux_symbols_count = 1
        }, sizeof(COFF_Symbol), 1, f);

    fwrite(&(COFF_AuxSectionSymbol) {
            .length = text_section.raw_data_size,
            .reloc_count = text_section.num_reloc,
            .number = section_num
        }, sizeof(COFF_AuxSectionSymbol), 1, f);

    section_num += 1;

    assert(section_num == 2); // things expect it to be 2
    fwrite(&(COFF_Symbol) {
            .short_name = { ".rdata" },
            .section_number = section_num,
            .storage_class = IMAGE_SYM_CLASS_STATIC,
            .aux_symbols_count = 1
        }, sizeof(COFF_Symbol), 1, f);

    fwrite(&(COFF_AuxSectionSymbol) {
            .length = data_section.raw_data_size,
            .number = section_num
        }, sizeof(COFF_AuxSectionSymbol), 1, f);

    section_num += 1;

    assert(section_num == 3); // things expect it to be 3
    fwrite(&(COFF_Symbol) {
            .short_name = { ".data" },
            .section_number = section_num,
            .storage_class = IMAGE_SYM_CLASS_STATIC,
            .aux_symbols_count = 1
        }, sizeof(COFF_Symbol), 1, f);

    fwrite(&(COFF_AuxSectionSymbol) {
            .length = data_section.raw_data_size,
            .reloc_count = data_section.num_reloc,
            .number = section_num
        }, sizeof(COFF_AuxSectionSymbol), 1, f);
    section_num += 1;

    int tls_section_num = section_num;
    if (m->tls_region_size) {
        fwrite(&(COFF_Symbol) {
                .short_name = { ".tls$" },
                .section_number = section_num,
                .storage_class = IMAGE_SYM_CLASS_STATIC,
                .aux_symbols_count = 1
            }, sizeof(COFF_Symbol), 1, f);

        fwrite(&(COFF_AuxSectionSymbol) {
                .length = data_section.raw_data_size,
                .reloc_count = data_section.num_reloc,
                .number = section_num
            }, sizeof(COFF_AuxSectionSymbol), 1, f);
        section_num += 1;
    }

    loop(i, debug_sections.length) {
        COFF_Symbol sym = {
            .section_number = section_num,
            .storage_class = IMAGE_SYM_CLASS_STATIC,
            .aux_symbols_count = 1
        };

        TB_Slice name = debug_sections.data[i].name;
        if (name.length > 8) {
            // string table crap i dont wanna do rn
            tb_todo();
        } else {
            memcpy(sym.short_name, name.data, name.length);
            if (name.length < 8) sym.short_name[name.length] = 0;
        }
        fwrite(&sym, sizeof(sym), 1, f);

        COFF_AuxSectionSymbol aux = {
            .length = debug_section_headers[i].raw_data_size,
            .reloc_count = debug_section_headers[i].num_reloc,
            .number = section_num
        };
        fwrite(&aux, sizeof(aux), 1, f);
        section_num += 1;
    }

    assert(ftell(f) == header.symbol_table + (sizeof(COFF_Symbol) * number_of_sections * 2));

    loop(i, m->functions.count) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;
        if (!out_f) continue;

        bool is_extern = out_f->linkage == TB_LINKAGE_PUBLIC;
        COFF_Symbol sym = {
            .value = func_layout[i],
            .section_number = 1,
            .storage_class = is_extern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC
        };

        const char* name = m->functions.data[i].name;
        size_t name_len = strlen(name);
        assert(name_len < UINT16_MAX);

        if (name_len >= 8) {
            sym.long_name[0] = 0; // this value is 0 for long names
            sym.long_name[1] = string_table_mark;

            string_table[string_table_length++] = (char*)name;
            string_table_mark += name_len + 1;
        } else {
            memcpy(sym.short_name, name, name_len + 1);
        }

        fwrite(&sym, sizeof(sym), 1, f);
    }

    loop(i, m->max_threads) {
        pool_for(TB_External, e, m->thread_info[i].externals) {
            COFF_Symbol sym = {
                .value = 0,
                .section_number = 0,
                .storage_class = IMAGE_SYM_CLASS_EXTERNAL
            };

            size_t name_len = strlen(e->name);
            assert(name_len < UINT16_MAX);

            if (name_len >= 8) {
                sym.long_name[0] = 0; // this value is 0 for long names
                sym.long_name[1] = string_table_mark;

                string_table[string_table_length++] = e->name;
                string_table_mark += name_len + 1;
            } else {
                memcpy(sym.short_name, e->name, name_len + 1);
            }

            fwrite(&sym, sizeof(sym), 1, f);
        }

        pool_for(TB_Global, g, m->thread_info[i].globals) {
            bool is_extern = g->linkage == TB_LINKAGE_PUBLIC;
            COFF_Symbol sym = {
                .value = g->pos,
                .section_number = g->storage == TB_STORAGE_TLS ? tls_section_num : 3, // data or tls section
                .storage_class = is_extern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC
            };

            size_t name_len = strlen(g->name);
            assert(name_len < UINT16_MAX);

            if (name_len >= 8) {
                sym.long_name[0] = 0; // this value is 0 for long names
                sym.long_name[1] = string_table_mark;

                string_table[string_table_length++] = g->name;
                string_table_mark += name_len + 1;
            } else {
                memcpy(sym.short_name, g->name, name_len + 1);
            }

            fwrite(&sym, sizeof(sym), 1, f);
        }
    }

    // String table
    // First 4 bytes are the size of the string table
    assert(ftell(f) == string_table_pos);
    fwrite(&string_table_mark, sizeof(string_table_mark), 1, f);

    for (size_t i = 0; i < string_table_length; i++) {
        const char* s = string_table[i];
        fwrite(s, 1, strlen(s) + 1, f);
    }
    fclose(f);

    tb_platform_heap_free(string_table);
    tb_platform_heap_free(func_layout);
}
