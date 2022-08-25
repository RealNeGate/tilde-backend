#include "coff.h"

// my section numbers in TB_ModuleExporterCOFF.sections
enum {
    S_TEXT,
    S_RDATA,
    S_DATA,
    S_TLS,
    S_MAX
};

typedef struct TB_ModuleExporterCOFF {
    enum {
        STAGE__WRITE_FILE_HEADER,
        STAGE__WRITE_SECTION_HEADERS,
        STAGE__WRITE_DEBUG_HEADERS,

        STAGE__WRITE_CODE_SECTION,
        STAGE__WRITE_RDATA_SECTION,
        STAGE__WRITE_DATA_SECTION,
        STAGE__WRITE_TLS_SECTION,
        STAGE__WRITE_DEBUG_SECTION,

        STAGE__WRITE_TEXT_PATCHES,
        STAGE__WRITE_DATA_PATCHES,
        STAGE__WRITE_DEBUG_PATCHES,

        STAGE__WRITE_SECTION_SYMBOLS,
        STAGE__WRITE_SYMBOLS,
        STAGE__WRITE_STRING_TABLE,
        STAGE__DONE,
    } stage;
    // depends on the stage, go check the code for them
    size_t tick[2];
    size_t write_pos;

    // temporary memory allocation, usually just used to store the contents
    // of file writing
    bool alloc_request_ongoing;
    void* temporary_memory;

    // [m->functions.count + 1] last slot is the size of the text section
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

static bool send_alloc_message(TB_ModuleExporterCOFF* e, TB_ModuleExportPacket* packet, size_t request_size) {
    if (e->temporary_memory == NULL) {
        if (!e->alloc_request_ongoing) {
            e->alloc_request_ongoing = true;

            packet->type = TB_EXPORT_PACKET_ALLOC;
            packet->alloc.request_size = request_size;
            packet->alloc.memory = NULL;
            return true;
        } else {
            assert(packet->alloc.memory != NULL && "expected valid memory region");

            e->alloc_request_ongoing = false;
            e->temporary_memory = packet->alloc.memory;
        }
    }

    return false;
}

#define COPY_ADVANCE(buffer, pos, T, ...) (memcpy(&buffer[pos], &(T) __VA_ARGS__, sizeof(T)), pos += sizeof(T))

TB_ModuleExporter* tb_coff__make(TB_Module* m) {
    return memset(tb_platform_heap_alloc(sizeof(TB_ModuleExporterCOFF)), 0, sizeof(TB_ModuleExporterCOFF));
}

bool tb_coff__next(TB_Module* m, TB_ModuleExporter* exporter, TB_ModuleExportPacket* packet) {
    TB_ModuleExporterCOFF* restrict e = (TB_ModuleExporterCOFF*) exporter;

    switch (e->stage) {
        case STAGE__WRITE_FILE_HEADER: {
            ////////////////////////////////
            // Layout work
            ////////////////////////////////
            const char* path = "fallback.obj";
            e->string_table_mark = 4;

            // tally up .data relocations
            uint32_t data_relocation_count = 0;

            FOREACH_N(t, 0, m->max_threads) {
                pool_for(TB_Global, g, m->thread_info[t].globals) {
                    TB_Initializer* init = g->init;
                    FOREACH_N(k, 0, init->obj_count) {
                        data_relocation_count += (init->objects[k].type != TB_INIT_OBJ_REGION);
                    }
                }
            }

            const IDebugFormat* restrict debug_fmt = tb__find_debug_format(m);

            int number_of_sections = 3
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
            const ICodeGen* restrict code_gen = tb__find_code_generator(m);
            size_t text_section_size = 0;
            FOREACH_N(i, 0, m->functions.count) {
                TB_FunctionOutput* out_f = m->functions.data[i].output;
                e->func_layout[i] = text_section_size;
                if (out_f == NULL) continue;

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

            e->sections[S_TLS] = (COFF_SectionHeader){
                .name = { ".tls$" },
                .characteristics = COFF_CHARACTERISTICS_DATA,
                .raw_data_size = m->tls_region_size
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

                e->sections[S_TEXT].raw_data_pos = tb_post_inc(&counter, e->sections[S_TEXT].raw_data_size);
                e->sections[S_RDATA].raw_data_pos = tb_post_inc(&counter, e->sections[S_RDATA].raw_data_size);
                e->sections[S_DATA].raw_data_pos = tb_post_inc(&counter, e->sections[S_DATA].raw_data_size);
                e->sections[S_TLS].raw_data_pos = tb_post_inc(&counter, e->sections[S_TLS].raw_data_size);

                FOREACH_N(i, 0, e->debug_sections.length) {
                    e->debug_section_headers[i].raw_data_size = e->debug_sections.data[i].raw_data.length;
                    e->debug_section_headers[i].raw_data_pos = tb_post_inc(&counter, e->debug_section_headers[i].raw_data_size);
                }

                // Do the relocation lists next
                e->sections[S_TEXT].pointer_to_reloc = tb_post_inc(&counter, e->sections[S_TEXT].num_reloc * sizeof(COFF_ImageReloc));
                e->sections[S_DATA].pointer_to_reloc = tb_post_inc(&counter, e->sections[S_DATA].num_reloc * sizeof(COFF_ImageReloc));

                FOREACH_N(i, 0, e->debug_sections.length) {
                    e->debug_section_headers[i].pointer_to_reloc = tb_post_inc(&counter, e->debug_section_headers[i].num_reloc * sizeof(COFF_ImageReloc));
                }

                e->header.symbol_table = tb_post_inc(&counter, e->header.symbol_count * sizeof(COFF_Symbol));
                e->string_table_pos = counter;
            }

            ////////////////////////////////
            // advance state & write out file header
            ////////////////////////////////
            send_write_message(e, packet, &e->header, sizeof(COFF_FileHeader));
            e->stage += 1;
            break;
        }

        case STAGE__WRITE_SECTION_HEADERS: {
            // figure out how many section headers to write out
            int count = (e->header.num_sections - e->debug_sections.length);

            send_write_message(e, packet, e->sections, count * sizeof(COFF_SectionHeader));
            e->stage += 1;
            break;
        }

        case STAGE__WRITE_DEBUG_HEADERS: {
            send_write_message(e, packet, e->debug_section_headers, e->debug_sections.length * sizeof(COFF_SectionHeader));

            // find first non-empty function
            size_t i = 0;
            while (i < m->functions.count && m->functions.data[i].output == NULL) {
                i += 1;
            }
            assert(i != m->functions.count && "TODO: handle completely empty code section");

            e->tick[0] = i, e->tick[1] = 0;
            e->stage += 1;
            break;
        }
        case STAGE__WRITE_CODE_SECTION: {
            const ICodeGen* restrict code_gen = tb__find_code_generator(m);

            TB_FunctionOutput* out_f = m->functions.data[e->tick[0]].output;
            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;

            // tick[0]
            //   is the function index
            //
            // tick[1]
            //   0 is prologue
            //   1 is body
            //   2 is epilogue
            switch (e->tick[1]++) {
                case 0: {
                    size_t len = code_gen->emit_prologue(e->proepi_buffer, meta, stack_usage);
                    send_write_message(e, packet, e->proepi_buffer, len);
                    break;
                }

                case 1: {
                    send_write_message(e, packet, out_f->code, out_f->code_size);
                    break;
                }

                case 2: {
                    size_t len = code_gen->emit_epilogue(e->proepi_buffer, meta, stack_usage);
                    send_write_message(e, packet, e->proepi_buffer, len);

                    // next compiled function
                    size_t i = e->tick[0] + 1;
                    while (i < m->functions.count && m->functions.data[i].output == NULL) {
                        i += 1;
                    }
                    e->tick[0] = i;
                    e->tick[1] = 0;

                    if (i >= m->functions.count) {
                        // reset tickers and advance
                        e->tick[0] = 0;
                        e->stage += 1;
                    }
                    break;
                }
            }

            break;
        }
        case STAGE__WRITE_RDATA_SECTION: {
            // we shouldn't use the heap here, *beg* the user for free memory
            if (send_alloc_message(e, packet, m->rdata_region_size)) return true;
            char* rdata = e->temporary_memory;

            assert(e->write_pos == e->sections[S_RDATA].raw_data_pos);
            loop(i, m->max_threads) {
                loop(j, dyn_array_length(m->thread_info[i].const_patches)) {
                    TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                    memcpy(&rdata[p->rdata_pos], p->data, p->length);
                }
            }

            send_write_message(e, packet, rdata, m->rdata_region_size);
            e->stage += 1;
            break;
        }
        case STAGE__WRITE_DATA_SECTION: {
            // TODO(NeGate): Optimize this for size and speed, sometimes
            // there's huge sections will be filled with just empty regions
            // so it's probably best not to represent everything in a big
            // buffer.
            if (send_alloc_message(e, packet, m->data_region_size)) return true;
            char* data = e->temporary_memory;

            assert(e->write_pos == e->sections[S_DATA].raw_data_pos);
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

            send_write_message(e, packet, data, m->data_region_size);
            e->stage += 1;
            break;
        }
        case STAGE__WRITE_TLS_SECTION: {
            if (send_alloc_message(e, packet, m->tls_region_size)) return true;
            char* data = e->temporary_memory;

            assert(e->write_pos == e->sections[S_TLS].raw_data_pos);
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

            send_write_message(e, packet, data, m->tls_region_size);

            // next section *unironically* uses the ticker
            e->tick[0] = 0;
            if (e->debug_sections.length == 0) {
                // skip the WRITE_DEBUG_SECTION stage
                e->stage += 2;
            } else {
                e->stage += 1;
            }
            break;
        }
        case STAGE__WRITE_DEBUG_SECTION: {
            size_t i = e->tick[0]++;

            assert(e->write_pos == e->debug_section_headers[i].raw_data_pos);
            send_write_message(e, packet, e->debug_sections.data[i].raw_data.data, e->debug_sections.data[i].raw_data.length);

            if (i + 1 >= e->debug_sections.length) {
                e->tick[0] = 0;
                e->stage += 1;
            }
            break;
        }
        case STAGE__WRITE_TEXT_PATCHES: {
            size_t capacity = e->sections[S_TEXT].num_reloc;
            if (send_alloc_message(e, packet, capacity * sizeof(COFF_ImageReloc))) return true;

            const ICodeGen* restrict code_gen = tb__find_code_generator(m);
            COFF_ImageReloc* relocs = e->temporary_memory;

            size_t count = 0;
            assert(e->write_pos == e->sections[S_TEXT].pointer_to_reloc);
            FOREACH_N(i, 0, m->max_threads) {
                FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].const_patches)) {
                    TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                    TB_FunctionOutput* out_f = p->source->output;

                    uint64_t meta = out_f->prologue_epilogue_metadata;
                    uint64_t stack_usage = out_f->stack_usage;

                    size_t actual_pos = e->func_layout[p->source - m->functions.data] + code_gen->get_prologue_length(meta, stack_usage) + p->pos;

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

                    uint64_t meta = out_f->prologue_epilogue_metadata;
                    uint64_t stack_usage = out_f->stack_usage;

                    size_t actual_pos = e->func_layout[p->source - m->functions.data] + code_gen->get_prologue_length(meta, stack_usage) + p->pos;
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

                    uint64_t meta = out_f->prologue_epilogue_metadata;
                    uint64_t stack_usage = out_f->stack_usage;

                    size_t actual_pos = e->func_layout[p->source - m->functions.data] + code_gen->get_prologue_length(meta, stack_usage) + p->pos;
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
            send_write_message(e, packet, relocs, count * sizeof(COFF_ImageReloc));

            e->stage += 1;
            break;
        }
        case STAGE__WRITE_DATA_PATCHES: {
            size_t capacity = e->sections[S_DATA].num_reloc;
            if (send_alloc_message(e, packet, capacity * sizeof(COFF_ImageReloc))) return true;

            size_t count = 0;
            COFF_ImageReloc* relocs = e->temporary_memory;

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
            send_write_message(e, packet, relocs, count * sizeof(COFF_ImageReloc));
            e->stage += 1;
            break;
        }
        case STAGE__WRITE_DEBUG_PATCHES: {
            size_t count = e->debug_sections.data[e->tick[0]].relocation_count;
            if (send_alloc_message(e, packet, count * sizeof(COFF_ImageReloc))) return true;

            size_t i = e->tick[0]++;
            assert(e->write_pos == e->debug_section_headers[i].pointer_to_reloc);

            COFF_ImageReloc* relocs = e->temporary_memory;
            FOREACH_N(j, 0, count) {
                TB_ObjectReloc* in_reloc = &e->debug_sections.data[i].relocations[j];

                relocs[j] = (COFF_ImageReloc){
                    .SymbolTableIndex = in_reloc->symbol_index,
                    .VirtualAddress = in_reloc->virtual_address
                };

                switch (in_reloc->type) {
                    case TB_OBJECT_RELOC_SECREL: relocs[j].Type = IMAGE_REL_AMD64_SECREL; break;
                    case TB_OBJECT_RELOC_SECTION: relocs[j].Type = IMAGE_REL_AMD64_SECTION; break;
                    default: tb_todo();
                }
            }
            send_write_message(e, packet, relocs, count * sizeof(COFF_ImageReloc));

            if (i + 1 >= e->debug_sections.length) {
                e->tick[0] = 0;
                e->stage += 1;
            }
            break;
        }
        case STAGE__WRITE_SECTION_SYMBOLS: {
            // COFF_AuxSectionSymbol is the same size as COFF_Symbol
            if (send_alloc_message(e, packet, e->header.num_sections * 2 * sizeof(COFF_Symbol))) return true;
            assert(e->write_pos == e->header.symbol_table);

            size_t pos = 0;
            char* buffer = e->temporary_memory;

            int section_num = 1;
            COPY_ADVANCE(buffer, pos, COFF_Symbol, {
                    .short_name = { ".text" },
                    .section_number = section_num,
                    .storage_class = IMAGE_SYM_CLASS_STATIC,
                    .aux_symbols_count = 1
                });

            COPY_ADVANCE(buffer, pos, COFF_AuxSectionSymbol, {
                    .length = e->sections[S_TEXT].raw_data_size,
                    .reloc_count = e->sections[S_TEXT].num_reloc,
                    .number = section_num
                });

            section_num += 1;

            assert(section_num == 2); // things expect it to be 2
            COPY_ADVANCE(buffer, pos, COFF_Symbol, {
                    .short_name = { ".rdata" },
                    .section_number = section_num,
                    .storage_class = IMAGE_SYM_CLASS_STATIC,
                    .aux_symbols_count = 1
                });

            COPY_ADVANCE(buffer, pos, COFF_AuxSectionSymbol, {
                    .length = e->sections[S_RDATA].raw_data_size,
                    .number = section_num
                });

            section_num += 1;

            assert(section_num == 3); // things expect it to be 3
            COPY_ADVANCE(buffer, pos, COFF_Symbol, {
                    .short_name = { ".data" },
                    .section_number = section_num,
                    .storage_class = IMAGE_SYM_CLASS_STATIC,
                    .aux_symbols_count = 1
                });

            COPY_ADVANCE(buffer, pos, COFF_AuxSectionSymbol, {
                    .length = e->sections[S_DATA].raw_data_size,
                    .reloc_count = e->sections[S_DATA].num_reloc,
                    .number = section_num
                });
            section_num += 1;

            e->tls_section_num = section_num;
            if (m->tls_region_size) {
                COPY_ADVANCE(buffer, pos, COFF_Symbol, {
                        .short_name = { ".tls$" },
                        .section_number = section_num,
                        .storage_class = IMAGE_SYM_CLASS_STATIC,
                        .aux_symbols_count = 1
                    });

                COPY_ADVANCE(buffer, pos, COFF_AuxSectionSymbol, {
                        .length = e->sections[S_TLS].raw_data_size,
                        .reloc_count = e->sections[S_TLS].num_reloc,
                        .number = section_num
                    });
                section_num += 1;
            }

            FOREACH_N(i, 0, e->debug_sections.length) {
                COFF_Symbol sym = {
                    .section_number = section_num,
                    .storage_class = IMAGE_SYM_CLASS_STATIC,
                    .aux_symbols_count = 1
                };

                TB_Slice name = e->debug_sections.data[i].name;
                if (name.length > 8) {
                    // string table crap i dont wanna do rn
                    tb_todo();
                } else {
                    memcpy(sym.short_name, name.data, name.length);
                    if (name.length < 8) sym.short_name[name.length] = 0;
                }

                memcpy(&buffer[pos], &sym, sizeof(COFF_Symbol));
                pos += sizeof(COFF_Symbol);

                COFF_AuxSectionSymbol aux = {
                    .length = e->debug_section_headers[i].raw_data_size,
                    .reloc_count = e->debug_section_headers[i].num_reloc,
                    .number = section_num
                };
                memcpy(&buffer[pos], &aux, sizeof(COFF_AuxSectionSymbol));
                pos += sizeof(COFF_AuxSectionSymbol);
                section_num += 1;
            }

            send_write_message(e, packet, buffer, e->header.num_sections * 2 * sizeof(COFF_Symbol));
            e->stage += 1;
            break;
        }
        case STAGE__WRITE_SYMBOLS: {
            size_t capacity = e->header.symbol_count - (e->header.num_sections * 2);
            if (send_alloc_message(e, packet, capacity * sizeof(COFF_Symbol))) return true;

            assert(e->write_pos == e->header.symbol_table + (sizeof(COFF_Symbol) * e->header.num_sections * 2));

            size_t count = 0;
            COFF_Symbol* symbols = (COFF_Symbol*) e->temporary_memory;
            FOREACH_N(i, 0, m->functions.count) {
                TB_FunctionOutput* out_f = m->functions.data[i].output;
                if (!out_f) continue;

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
                symbols[count++] = sym;
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
                    symbols[count++] = sym;
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
                    symbols[count++] = sym;
                }
            }

            assert(count == capacity);
            send_write_message(e, packet, symbols, capacity * sizeof(COFF_Symbol));
            e->stage += 1;
            break;
        }
        case STAGE__WRITE_STRING_TABLE: {
            if (send_alloc_message(e, packet, e->string_table_mark)) return true;
            assert(e->write_pos == e->string_table_pos);

            char *start = e->temporary_memory, *buffer = start;
            memcpy(buffer, &e->string_table_mark, sizeof(uint32_t));
            buffer += 4;

            FOREACH_N(i, 0, e->string_table_length) {
                const char* s = e->string_table[i];
                size_t l = strlen(s) + 1;

                memcpy(buffer, s, l);
                buffer += l;
            }

            assert((buffer - start) == e->string_table_mark);
            send_write_message(e, packet, start, e->string_table_mark);
            e->stage += 1;
            return true;
        }
        case STAGE__DONE: {
            tb_platform_heap_free(e->debug_section_headers);
            tb_platform_heap_free(e->string_table);
            tb_platform_heap_free(e->func_layout);
            tb_platform_heap_free(e);
            return false;
        }

        default: tb_todo();
    }

    // reset temporary allocation
    e->temporary_memory = NULL;
    return true;
}

#if 0
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
            .length = rdata_section.raw_data_size,
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
#endif

