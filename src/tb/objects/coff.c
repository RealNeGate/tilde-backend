#include "coff.h"
#include "../coroutine.h"

// my section numbers in TB_ModuleExporterCOFF.sections
enum {
    S_TEXT,
    S_RDATA,
    S_DATA,
    S_PDATA,
    S_XDATA,
    S_TLS,
    S_MAX
};

typedef struct TB_ModuleExporterCOFF {
    int state;

    // used by iterators that need to yield
    ptrdiff_t i, i_limit;
    ptrdiff_t j, j_limit;

    const ICodeGen* code_gen;
    size_t write_pos;

    size_t temporary_memory_capacity;
    void* temporary_memory;

    // [m->functions.count + 1] last slot is the size of the text section
    const IDebugFormat* dbg;
    uint32_t* func_layout;

    // String table array, stores the strings which will be put
    // into the string table
    uint32_t string_table_length;
    uint32_t string_table_mark;
    uint32_t string_table_cap;
    char** string_table;

    size_t function_sym_start;
    size_t external_sym_start;

    size_t string_table_pos;
    size_t tls_section_num;

    // unwind_info[function id] is the position (in the xdata section) where
    // the unwind info for a specific function exists
    uint32_t* unwind_info;
    TB_Emitter xdata;

    // COFF file header & section headers
    COFF_FileHeader header;
    COFF_SectionHeader sections[S_MAX];

    TB_SectionGroup debug_sections;
    COFF_SectionHeader* debug_section_headers;

    uint8_t proepi_buffer[PROEPI_BUFFER];
} TB_ModuleExporterCOFF;

static void send_write_message(TB_ModuleExporterCOFF* e, TB_ModuleExportPacket* packet, const void* data, size_t length) {
    packet->type = TB_EXPORT_PACKET_WRITE;
    packet->write.length = length;
    packet->write.data = data;

    e->write_pos += length;
}

static void* get_temporary_storage(TB_ModuleExporterCOFF* e, size_t request_size) {
    if (e->temporary_memory_capacity < request_size) {
        e->temporary_memory_capacity = tb_next_pow2(request_size);
        if (e->temporary_memory_capacity < (4*1024*1024)) {
            e->temporary_memory_capacity = (4*1024*1024);
        }

        e->temporary_memory = tb_platform_heap_realloc(e->temporary_memory, e->temporary_memory_capacity);
    }

    return e->temporary_memory;
}

// yields a buffer that it needs to write to the output
#define YIELD_WRITE(data, length_) do {                           \
    *packet = (TB_ModuleExportPacket){                            \
        .type = TB_EXPORT_PACKET_WRITE, .write = { length_, data }\
    };                                                            \
    e->write_pos += packet->write.length;                         \
    CO_YIELD(e, true);                                            \
} while (0)

static COFF_Symbol section_sym(const char* name, int num, int sc) {
    COFF_Symbol s = { .section_number = num, .storage_class = sc, .aux_symbols_count = 1 };
    strncpy((char*) s.short_name, name, 8);
    return s;
}

static COFF_AuxSectionSymbol section_aux_sym(COFF_SectionHeader* s, int num) {
    return (COFF_AuxSectionSymbol){
        .length = s->raw_data_size,
        .reloc_count = s->num_reloc,
        .number = num,
    };
}

static size_t append_section_sym(COFF_SymbolUnion* symbols, size_t count, COFF_SectionHeader* section, const char* name, int sc) {
    symbols[count + 0].s = section_sym(name, (count / 2) + 1, sc);
    symbols[count + 1].a = section_aux_sym(section, (count / 2) + 1);
    return count + 2;
}

static TB_Emitter write_xdata_section(TB_Module* m, uint32_t* unwind_info, const ICodeGen* restrict code_gen) {
    if (code_gen->emit_win64eh_unwind_info == NULL) {
        tb_panic("write_xdata_section: emit_win64eh_unwind_info is required.");
    }

    TB_Emitter xdata = { 0 };
    FOREACH_N(i, 0, m->functions.count) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;

        unwind_info[i] = xdata.count;
        if (out_f) {
            code_gen->emit_win64eh_unwind_info(&xdata, out_f, out_f->prologue_epilogue_metadata, out_f->stack_usage);
        }
    }

    return xdata;
}

void* tb_coff__make(TB_Module* m, const IDebugFormat* dbg) {
    TB_ModuleExporterCOFF* e = memset(tb_platform_heap_alloc(sizeof(TB_ModuleExporterCOFF)), 0, sizeof(TB_ModuleExporterCOFF));
    e->code_gen = tb__find_code_generator(m);
    e->dbg = dbg;
    return e;
}

bool tb_coff__next(TB_Module* m, void* exporter, TB_ModuleExportPacket* packet) {
    TB_ModuleExporterCOFF* restrict e = exporter;

    CO_SCOPE(e) {
        CO_START();

        ////////////////////////////////
        // Layout work
        ////////////////////////////////
        {
            const char* path = "fallback.obj";
            e->string_table_mark = 4;

            // tally up .data relocations
            uint32_t data_relocation_count = 0;

            // Generate .xdata section (unwind info)
            e->unwind_info = tb_platform_heap_alloc(m->functions.count * sizeof(uint32_t));
            e->xdata = write_xdata_section(m, e->unwind_info, e->code_gen);

            FOREACH_N(t, 0, m->max_threads) {
                pool_for(TB_Global, g, m->thread_info[t].globals) {
                    TB_Initializer* init = g->init;
                    FOREACH_N(k, 0, init->obj_count) {
                        data_relocation_count += (init->objects[k].type != TB_INIT_OBJ_REGION);
                    }
                }
            }

            const IDebugFormat* debug_fmt = e->dbg;
            int number_of_sections = 5
                + (m->tls_region_size ? 1 : 0)
                + (debug_fmt != NULL ? debug_fmt->number_of_debug_sections(m) : 0);

            // mark each with a unique id
            e->function_sym_start = (number_of_sections * 2);
            e->external_sym_start = e->function_sym_start + m->functions.compiled_count;

            size_t unique_id_counter = 0;
            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_External, ext, m->thread_info[i].externals) {
                    int id = e->external_sym_start + unique_id_counter;
                    ext->address = (void*) (uintptr_t) id;
                    unique_id_counter += 1;
                }

                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    g->id = e->external_sym_start + unique_id_counter;
                    unique_id_counter += 1;
                }
            }

            e->string_table_cap += unique_id_counter;
            e->string_table_cap += m->functions.compiled_count;
            e->string_table = tb_platform_heap_alloc(e->string_table_cap * sizeof(const char*));

            // layout functions in file
            e->func_layout = tb_platform_heap_alloc((m->functions.count + 1) * sizeof(uint32_t));

            // TODO(NeGate): We might do alphabetical sorting for consistent binary output
            const ICodeGen* restrict code_gen = e->code_gen;
            size_t text_section_size = 0;
            FOREACH_N(i, 0, m->functions.count) {
                TB_FunctionOutput* out_f = m->functions.data[i].output;

                e->func_layout[i] = text_section_size;
                if (out_f != NULL) {
                    text_section_size += out_f->code_size;
                }
            }
            e->func_layout[m->functions.count] = text_section_size;

            ////////////////////////////////
            // create headers
            ////////////////////////////////
            e->header = (COFF_FileHeader){
                .num_sections = number_of_sections,
                .timestamp = time(NULL),
                .symbol_count = 0,
                .symbol_table = 0,
                .characteristics = IMAGE_FILE_LINE_NUMS_STRIPPED
            };

            e->sections[S_TEXT] = (COFF_SectionHeader){
                .name = { ".text" }, // .text
                .characteristics = COFF_CHARACTERISTICS_TEXT,
                .raw_data_size = text_section_size,
            };

            e->sections[S_RDATA] = (COFF_SectionHeader){
                .name = { ".rdata" }, // .rdata
                .characteristics = COFF_CHARACTERISTICS_RODATA,
                .raw_data_size = m->rdata_region_size
            };

            e->sections[S_DATA] = (COFF_SectionHeader){
                .name = { ".data" }, // .data
                .characteristics = COFF_CHARACTERISTICS_DATA,
                .raw_data_size = m->data_region_size
            };

            e->sections[S_PDATA] = (COFF_SectionHeader){
                .name = { ".pdata" }, // .pdata
                .characteristics = COFF_CHARACTERISTICS_RODATA,
                .raw_data_size = m->functions.compiled_count * 12,
                .num_reloc = m->functions.compiled_count * 3,
            };

            e->sections[S_XDATA] = (COFF_SectionHeader){
                .name = { ".xdata" }, // .xdata
                .characteristics = COFF_CHARACTERISTICS_RODATA,
                .raw_data_size = e->xdata.count,
                .num_reloc = 0,
            };

            e->sections[S_TLS] = (COFF_SectionHeader){
                .name = { ".tls$" },
                .characteristics = COFF_CHARACTERISTICS_DATA,
                .raw_data_size = m->tls_region_size,
            };

            switch (m->target_arch) {
                case TB_ARCH_X86_64:  e->header.machine = COFF_MACHINE_AMD64; break;
                case TB_ARCH_AARCH64: e->header.machine = COFF_MACHINE_ARM64; break;
                default: tb_todo();
            }

            if (debug_fmt != NULL) {
                TB_TemporaryStorage* tls = tb_tls_allocate();

                e->debug_sections = debug_fmt->generate_debug_info(m, tls, code_gen, path, e->function_sym_start, e->func_layout);
                e->debug_section_headers = tb_platform_heap_alloc(e->debug_sections.length * sizeof(COFF_SectionHeader));

                FOREACH_N(i, 0, e->debug_sections.length) {
                    e->debug_section_headers[i] = (COFF_SectionHeader) { 0 };
                    e->debug_section_headers[i].characteristics = COFF_CHARACTERISTICS_CV;
                    e->debug_section_headers[i].num_reloc = e->debug_sections.data[i].relocation_count;

                    TB_Slice name = e->debug_sections.data[i].name;
                    if (name.length > 8) {
                        // we'd need to put it into the string table... im lazy
                        // so i wont do the logic for it yet
                        tb_todo();
                    } else {
                        memcpy(e->debug_section_headers[i].name, name.data, name.length);
                        if (name.length < 8) e->debug_section_headers[i].name[name.length] = 0;
                    }
                }
            }

            // Target specific: resolve internal call patches
            code_gen->emit_call_patches(m, e->func_layout);

            // symbols
            {
                e->header.symbol_count = (number_of_sections * 2) + m->functions.compiled_count;

                // total externals + total globals
                e->header.symbol_count += unique_id_counter;
            }

            // relocation
            {
                e->sections[S_TEXT].num_reloc = 0;
                FOREACH_N(i, 0, m->max_threads) {
                    e->sections[S_TEXT].num_reloc += dyn_array_length(m->thread_info[i].const_patches);
                    e->sections[S_TEXT].num_reloc += dyn_array_length(m->thread_info[i].ecall_patches);
                    e->sections[S_TEXT].num_reloc += dyn_array_length(m->thread_info[i].global_patches);
                }

                e->sections[S_DATA].num_reloc = data_relocation_count;
            }

            // layout sections & relocations
            {
                size_t counter = sizeof(COFF_FileHeader) + (number_of_sections * sizeof(COFF_SectionHeader));

                FOREACH_N(i, 0, S_MAX) {
                    e->sections[i].raw_data_pos = counter;
                    counter += e->sections[i].raw_data_size;
                }

                FOREACH_N(i, 0, e->debug_sections.length) {
                    e->debug_section_headers[i].raw_data_size = e->debug_sections.data[i].raw_data.length;
                    e->debug_section_headers[i].raw_data_pos = tb_post_inc(&counter, e->debug_section_headers[i].raw_data_size);
                }

                // Do the relocation lists next
                FOREACH_N(i, 0, S_MAX) {
                    e->sections[i].pointer_to_reloc = counter;
                    counter += e->sections[i].num_reloc * sizeof(COFF_ImageReloc);
                }

                FOREACH_N(i, 0, e->debug_sections.length) {
                    e->debug_section_headers[i].pointer_to_reloc = counter;
                    counter += e->debug_section_headers[i].num_reloc * sizeof(COFF_ImageReloc);
                }

                e->header.symbol_table = tb_post_inc(&counter, e->header.symbol_count * sizeof(COFF_Symbol));
                e->string_table_pos = counter;
            }
            YIELD_WRITE(&e->header, sizeof(COFF_FileHeader));

            // figure out how many section headers to write out
            int count = (e->header.num_sections - e->debug_sections.length);
            YIELD_WRITE(e->sections, count * sizeof(COFF_SectionHeader));
            YIELD_WRITE(e->debug_section_headers, e->debug_sections.length * sizeof(COFF_SectionHeader));
        }

        // write TEXT section
        {
            CO_FOREACH_N(e, i, 0, m->functions.count) {
                TB_FunctionOutput* out_f = m->functions.data[i].output;
                if (out_f != NULL) {
                    YIELD_WRITE(out_f->code, out_f->code_size);
                }
            }
        }

        // RDATA section
        {
            assert(e->write_pos == e->sections[S_RDATA].raw_data_pos);
            char* rdata = get_temporary_storage(e, m->rdata_region_size);

            CO_FOREACH_N(e, i, 0, m->max_threads) {
                CO_FOREACH_N(e, j, 0, dyn_array_length(m->thread_info[i].const_patches)) {
                    TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                    memcpy(&rdata[p->rdata_pos], p->data, p->length);
                }
            }

            YIELD_WRITE(rdata, m->rdata_region_size);
        }

        // DATA section
        {
            char* data = get_temporary_storage(e, m->data_region_size);

            assert(e->write_pos == e->sections[S_DATA].raw_data_pos);
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
                        }
                    }
                }
            }

            YIELD_WRITE(data, m->data_region_size);
        }

        // PDATA section
        {
            assert(e->write_pos == e->sections[S_PDATA].raw_data_pos);
            uint32_t* pdata = get_temporary_storage(e, m->functions.compiled_count * 12);

            size_t j = 0;
            FOREACH_N(i, 0, m->functions.count) {
                TB_FunctionOutput* out_f = m->functions.data[i].output;
                if (!out_f) continue;

                pdata[j+0] = e->func_layout[i];
                pdata[j+1] = e->func_layout[i + 1];
                pdata[j+2] = e->unwind_info[i];
                j += 3;
            }

            YIELD_WRITE(pdata, m->functions.compiled_count * 12);
        }

        // XDATA section
        {
            assert(e->write_pos == e->sections[S_XDATA].raw_data_pos);
            YIELD_WRITE(e->xdata.data, e->xdata.count);
        }

        // TLS section
        {
            char* tls = get_temporary_storage(e, m->tls_region_size);

            assert(e->write_pos == e->sections[S_TLS].raw_data_pos);
            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    if (g->storage != TB_STORAGE_TLS) continue;

                    TB_Initializer* init = g->init;

                    // clear out space
                    memset(&tls[g->pos], 0, init->size);

                    FOREACH_N(k, 0, init->obj_count) {
                        const TB_InitObj* o = &init->objects[k];
                        if (o->type == TB_INIT_OBJ_REGION) {
                            memcpy(&tls[g->pos + o->offset], o->region.ptr, o->region.size);
                        }
                    }
                }
            }

            YIELD_WRITE(tls, m->tls_region_size);
        }

        // write DEBUG sections
        CO_FOREACH_N(e, i, 0, e->debug_sections.length) {
            assert(e->write_pos == e->debug_section_headers[i].raw_data_pos);
            YIELD_WRITE(e->debug_sections.data[i].raw_data.data, e->debug_sections.data[i].raw_data.length);
        }

        // write TEXT patches
        {
            size_t capacity = e->sections[S_TEXT].num_reloc;
            COFF_ImageReloc* relocs = get_temporary_storage(e, capacity * sizeof(COFF_ImageReloc));

            size_t count = 0;
            assert(e->write_pos == e->sections[S_TEXT].pointer_to_reloc);
            FOREACH_N(i, 0, m->max_threads) {
                FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].const_patches)) {
                    TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                    TB_FunctionOutput* out_f = p->source->output;

                    size_t actual_pos = e->func_layout[p->source - m->functions.data]
                        + out_f->prologue_length + p->pos;

                    assert(count < capacity);
                    relocs[count++] = (COFF_ImageReloc) {
                        .Type = IMAGE_REL_AMD64_REL32,
                        .SymbolTableIndex = 2, // rdata section
                        .VirtualAddress = actual_pos
                    };
                }

                FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].ecall_patches)) {
                    TB_ExternFunctionPatch* p = &m->thread_info[i].ecall_patches[j];
                    TB_FunctionOutput* out_f = p->source->output;

                    size_t actual_pos = e->func_layout[p->source - m->functions.data]
                        + out_f->prologue_length + p->pos;

                    int symbol_id = (uintptr_t) p->target->address;
                    assert(symbol_id != 0);

                    assert(count < capacity);
                    relocs[count++] = (COFF_ImageReloc) {
                        .Type = IMAGE_REL_AMD64_REL32,
                        .SymbolTableIndex = symbol_id,
                        .VirtualAddress = actual_pos
                    };
                }

                FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].global_patches)) {
                    TB_GlobalPatch* p = &m->thread_info[i].global_patches[j];
                    TB_FunctionOutput* out_f = p->source->output;

                    size_t actual_pos = e->func_layout[p->source - m->functions.data]
                        + out_f->prologue_length + p->pos;

                    const TB_Global* global = p->target;

                    int symbol_id = global->id;
                    assert(symbol_id != 0);

                    assert(count < capacity);
                    relocs[count++] = (COFF_ImageReloc) {
                        .Type = global->storage == TB_STORAGE_TLS ? IMAGE_REL_AMD64_SECREL : IMAGE_REL_AMD64_REL32,
                        .SymbolTableIndex = symbol_id,
                        .VirtualAddress = actual_pos
                    };
                }
            }

            assert(count == capacity);
            YIELD_WRITE(relocs, count * sizeof(COFF_ImageReloc));
        }

        // write DATA patches
        {
            size_t count = 0, capacity = e->sections[S_DATA].num_reloc;
            COFF_ImageReloc* relocs = get_temporary_storage(e, capacity * sizeof(COFF_ImageReloc));

            assert(e->write_pos == e->sections[S_DATA].pointer_to_reloc);
            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    TB_Initializer* init = g->init;

                    FOREACH_N(k, 0, init->obj_count) {
                        size_t actual_pos = g->pos + init->objects[k].offset;

                        switch (init->objects[k].type) {
                            case TB_INIT_OBJ_RELOC_GLOBAL: {
                                const TB_Global* g = init->objects[k].reloc_global;

                                assert(count < capacity);
                                relocs[count++] = (COFF_ImageReloc) {
                                    .Type = IMAGE_REL_AMD64_ADDR64,
                                    .SymbolTableIndex = g->id,
                                    .VirtualAddress = actual_pos
                                };
                                break;
                            }

                            case TB_INIT_OBJ_RELOC_EXTERN: {
                                const TB_External* e = init->objects[k].reloc_extern;
                                int id = (uintptr_t) e->address;

                                assert(count < capacity);
                                relocs[count++] = (COFF_ImageReloc) {
                                    .Type = IMAGE_REL_AMD64_ADDR64,
                                    .SymbolTableIndex = id,
                                    .VirtualAddress = actual_pos
                                };
                                break;
                            }

                            case TB_INIT_OBJ_RELOC_FUNCTION: {
                                int symbol_id = init->objects[k].reloc_function - m->functions.data;

                                assert(count < capacity);
                                relocs[count++] = (COFF_ImageReloc) {
                                    .Type = IMAGE_REL_AMD64_ADDR64,
                                    .SymbolTableIndex = e->function_sym_start + symbol_id,
                                    .VirtualAddress = actual_pos
                                };
                                break;
                            }

                            default: break;
                        }
                    }
                }
            }

            assert(count == capacity);
            YIELD_WRITE(relocs, count * sizeof(COFF_ImageReloc));
        }

        // write PDATA patches
        {
            size_t count = 0, capacity = m->functions.compiled_count * 3;
            COFF_ImageReloc* relocs = get_temporary_storage(e, capacity * sizeof(COFF_ImageReloc));

            size_t j = 0;
            assert(e->write_pos == e->sections[S_PDATA].pointer_to_reloc);
            FOREACH_N(i, 0, m->functions.count) {
                TB_FunctionOutput* out_f = m->functions.data[i].output;
                if (!out_f) continue;

                relocs[count++] = (COFF_ImageReloc){
                    .Type = IMAGE_REL_AMD64_ADDR32NB,
                    .SymbolTableIndex = 0, // text section
                    .VirtualAddress = (j * 12)
                };

                relocs[count++] = (COFF_ImageReloc){
                    .Type = IMAGE_REL_AMD64_ADDR32NB,
                    .SymbolTableIndex = 0, // text section
                    .VirtualAddress = (j * 12) + 4
                };

                relocs[count++] = (COFF_ImageReloc){
                    .Type = IMAGE_REL_AMD64_ADDR32NB,
                    .SymbolTableIndex = 8, // xdata section
                    .VirtualAddress = (j * 12) + 8
                };
                j += 1;
            }

            assert(count == capacity);
            YIELD_WRITE(relocs, count * sizeof(COFF_ImageReloc));
        }

        if (e->debug_sections.length > 0) {
            size_t count = 0, capacity = 0;
            FOREACH_N(i, 0, e->debug_sections.length) {
                capacity += e->debug_sections.data[i].relocation_count;
            }

            COFF_ImageReloc* relocs = get_temporary_storage(e, capacity * sizeof(COFF_ImageReloc));
            assert(e->write_pos == e->debug_section_headers[0].pointer_to_reloc);
            FOREACH_N(i, 0, e->debug_sections.length) {

                FOREACH_N(j, 0, e->debug_sections.data[i].relocation_count) {
                    TB_ObjectReloc* in_reloc = &e->debug_sections.data[i].relocations[j];

                    relocs[count++] = (COFF_ImageReloc){
                        .SymbolTableIndex = in_reloc->symbol_index,
                        .VirtualAddress = in_reloc->virtual_address
                    };

                    switch (in_reloc->type) {
                        case TB_OBJECT_RELOC_SECREL: relocs[j].Type = IMAGE_REL_AMD64_SECREL; break;
                        case TB_OBJECT_RELOC_SECTION: relocs[j].Type = IMAGE_REL_AMD64_SECTION; break;
                        default: tb_todo();
                    }
                }
            }

            assert(count == capacity);
            YIELD_WRITE(relocs, count * sizeof(COFF_ImageReloc));
        }

        // Emit section symbols
        {
            // COFF_AuxSectionSymbol is the same size as COFF_Symbol
            assert(e->write_pos == e->header.symbol_table);

            size_t count = 0, capacity = e->header.symbol_count;
            COFF_SymbolUnion* symbols = get_temporary_storage(e, capacity * sizeof(COFF_SymbolUnion));

            count = append_section_sym(symbols, count, &e->sections[S_TEXT],  ".text",  IMAGE_SYM_CLASS_STATIC);
            count = append_section_sym(symbols, count, &e->sections[S_RDATA], ".rdata", IMAGE_SYM_CLASS_STATIC);
            count = append_section_sym(symbols, count, &e->sections[S_DATA],  ".data",  IMAGE_SYM_CLASS_STATIC);
            count = append_section_sym(symbols, count, &e->sections[S_PDATA], ".pdata", IMAGE_SYM_CLASS_STATIC);
            count = append_section_sym(symbols, count, &e->sections[S_XDATA], ".xdata", IMAGE_SYM_CLASS_STATIC);

            e->tls_section_num = (count / 2) + 1;
            if (m->tls_region_size) {
                count = append_section_sym(symbols, count, &e->sections[S_TLS], ".tls$", IMAGE_SYM_CLASS_STATIC);
            }

            FOREACH_N(i, 0, e->debug_sections.length) {
                COFF_Symbol sym = {
                    .section_number = (count / 2) + 1,
                    .storage_class = IMAGE_SYM_CLASS_STATIC,
                    .aux_symbols_count = 1
                };

                TB_Slice name = e->debug_sections.data[i].name;
                if (name.length < 8) {
                    strncpy((char*) sym.short_name, (const char*) name.data, name.length);
                } else {
                    memcpy((char*) sym.short_name, (const char*) name.data, 8);
                }

                symbols[count++].s = sym;
                symbols[count++].a = (COFF_AuxSectionSymbol){
                    .length      = e->debug_section_headers[i].raw_data_size,
                    .reloc_count = e->debug_section_headers[i].num_reloc,
                    .number      = sym.section_number
                };
            }

            FOREACH_N(i, 0, m->functions.count) {
                TB_FunctionOutput* out_f = m->functions.data[i].output;
                if (out_f == NULL) continue;

                bool is_extern = out_f->linkage == TB_LINKAGE_PUBLIC;
                COFF_Symbol sym = {
                    .value = e->func_layout[i],
                    .section_number = 1,
                    .storage_class = is_extern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC
                };

                const char* name = m->functions.data[i].name;
                size_t name_len = strlen(name);
                assert(name_len < UINT16_MAX);
                if (name_len >= 8) {
                    sym.long_name[0] = 0; // this value is 0 for long names
                    sym.long_name[1] = e->string_table_mark;

                    e->string_table[e->string_table_length++] = (char*)name;
                    e->string_table_mark += name_len + 1;
                } else {
                    memcpy(sym.short_name, name, name_len + 1);
                }

                assert(count < capacity);
                symbols[count++].s = sym;
            }

            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_External, ext, m->thread_info[i].externals) {
                    COFF_Symbol sym = {
                        .value = 0,
                        .section_number = 0,
                        .storage_class = IMAGE_SYM_CLASS_EXTERNAL
                    };

                    size_t name_len = strlen(ext->name);
                    assert(name_len < UINT16_MAX);

                    if (name_len >= 8) {
                        sym.long_name[0] = 0; // this value is 0 for long names
                        sym.long_name[1] = e->string_table_mark;

                        e->string_table[e->string_table_length++] = ext->name;
                        e->string_table_mark += name_len + 1;
                    } else {
                        memcpy(sym.short_name, ext->name, name_len + 1);
                    }

                    assert(count < capacity);
                    symbols[count++].s = sym;
                }

                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    bool is_extern = g->linkage == TB_LINKAGE_PUBLIC;
                    COFF_Symbol sym = {
                        .value = g->pos,
                        .section_number = g->storage == TB_STORAGE_TLS ? e->tls_section_num : 3, // data or tls section
                        .storage_class = is_extern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC
                    };

                    size_t name_len = strlen(g->name);
                    assert(name_len < UINT16_MAX);

                    if (name_len >= 8) {
                        sym.long_name[0] = 0; // this value is 0 for long names
                        sym.long_name[1] = e->string_table_mark;

                        e->string_table[e->string_table_length++] = g->name;
                        e->string_table_mark += name_len + 1;
                    } else {
                        memcpy(sym.short_name, g->name, name_len + 1);
                    }

                    assert(count < capacity);
                    symbols[count++].s = sym;
                }
            }

            YIELD_WRITE(symbols, count * sizeof(COFF_Symbol));
        }

        // Emit string table
        {
            assert(e->write_pos == e->string_table_pos);

            char *start = get_temporary_storage(e, e->string_table_mark), *buffer = start;
            memcpy(buffer, &e->string_table_mark, sizeof(uint32_t));
            buffer += 4;

            FOREACH_N(i, 0, e->string_table_length) {
                const char* s = e->string_table[i];
                size_t l = strlen(s) + 1;

                memcpy(buffer, s, l);
                buffer += l;
            }

            assert((buffer - start) == e->string_table_mark);
            YIELD_WRITE(start, e->string_table_mark);
        }

        // TODO(NeGate): we have a lot of shit being freed... maybe we wanna think of smarter
        // allocation schemes
        tb_platform_heap_free(e->temporary_memory);
        tb_platform_heap_free(e->debug_section_headers);
        tb_platform_heap_free(e->string_table);
        tb_platform_heap_free(e->func_layout);
        tb_platform_heap_free(e->unwind_info);
        tb_platform_heap_free(e->xdata.data);
        tb_platform_heap_free(e);
    }

    CO_DONE();
}
