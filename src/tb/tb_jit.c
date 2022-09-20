#include "tb_internal.h"
#include "host.h"

size_t tb_helper_write_text_section(size_t write_pos, TB_Module* m, uint8_t* output, uint32_t pos);
size_t tb_helper_write_data_section(size_t write_pos, TB_Module* m, uint8_t* output, uint32_t pos);
size_t tb_helper_write_rodata_section(size_t write_pos, TB_Module* m, uint8_t* output, uint32_t pos);

// NOTE(NeGate): This only currently supports the text and rdata sections,
// it puts the rdata on the next 4KB page after the text section all within
// the same memory mapping, this is actually very bad because it means that
// read-only data is executable.
void tb_module_export_jit(TB_Module* m) {
    ICodeGen* restrict codegen = tb__find_code_generator(m);

    // Buffer stores all the positions of each
    // function relative to the .text section start.
    uint32_t* func_layout = tb_platform_heap_alloc((m->functions.count + 1) * sizeof(uint32_t));
    size_t page_size = 4096;

    size_t text_section_size = 0;
    {
        for (size_t i = 0; i < m->functions.count; i++) {
            func_layout[i] = text_section_size;

            TB_FunctionOutput* out_f = m->functions.data[i].output;
            if (out_f) text_section_size += out_f->code_size;
        }

        // Target specific: resolve internal call patches
        codegen->emit_call_patches(m, func_layout);
    }
    size_t rdata_section_size = align_up(m->rdata_region_size, page_size);

    size_t external_count = 0;
    FOREACH_N(i, 0, m->max_threads) {
        external_count += pool_popcount(m->thread_info[i].externals);
    }
    rdata_section_size += align_up(external_count * sizeof(void*), page_size);

    typedef struct {
        size_t offset;
        size_t size;
        TB_MemProtect protect;
    } Section;

    enum {
        S_RDATA, S_TEXT, S_DATA
    };

    int section_count = 3;
    Section sections[] = {
        [S_RDATA] = { .size = rdata_section_size,  .protect = TB_PAGE_READONLY    }, // .rdata
        [S_TEXT]  = { .size = text_section_size,   .protect = TB_PAGE_READEXECUTE }, // .text
        [S_DATA]  = { .size = m->data_region_size, .protect = TB_PAGE_READWRITE   }, // .data
    };

    // Layout sections
    size_t jit_region_size = 0;
    FOREACH_N(i, 0, section_count) {
        sections[i].offset = jit_region_size;
        jit_region_size = align_up(jit_region_size + sections[i].size, page_size);
    }
    uint8_t* jit_region = tb_platform_valloc(jit_region_size);

    // .RDATA
    size_t write_pos = 0;
    {
        write_pos = tb_helper_write_rodata_section(write_pos, m, jit_region, sections[S_RDATA].offset);

        // last region is a jump table
        uint8_t* import_table = jit_region + align_up(m->rdata_region_size, page_size);
        size_t count = 0;
        FOREACH_N(i, 0, m->max_threads) {
            pool_for(TB_External, ext, m->thread_info[i].externals) {
                // replace the ext->address with the jump table
                void* old = ext->address;
                void* new = import_table + (count * sizeof(void*));

                memcpy(new, old, sizeof(void*));
                ext->address = new;
                count += 1;
            }
        }
        write_pos += count*sizeof(void*);
    }

    // .TEXT
    {
        uint8_t* text_section = jit_region + sections[S_TEXT].offset;
        FOREACH_N(i, 0, m->functions.count) {
            TB_FunctionOutput* out_f = m->functions.data[i].output;
            if (out_f != NULL) {
                m->functions.data[i].compiled_pos = &text_section[func_layout[i]];

                memcpy(&text_section[func_layout[i]], out_f->code, out_f->code_size);
            }
        }

        // Emit external patches
        // These have dealt with the jump table so none of our relocations should
        // cross the 2GB limit.
        FOREACH_N(i, 0, m->max_threads) {
            dyn_array_for(j, m->thread_info[i].ecall_patches) {
                TB_ExternFunctionPatch* p = &m->thread_info[i].ecall_patches[j];
                TB_FunctionOutput* out_f = p->source->output;

                size_t actual_pos = func_layout[p->source - m->functions.data] +
                    out_f->prologue_length + p->pos;

                ptrdiff_t displacement = (uint8_t*)p->target->address - &text_section[actual_pos];
                int32_t disp32 = displacement;

                assert(displacement == disp32);
                memcpy(&text_section[actual_pos], &disp32, sizeof(disp32));
            }
        }
    }

    // .DATA
    write_pos = tb_helper_write_data_section(sections[S_DATA].offset, m, jit_region, sections[S_DATA].offset);

    FOREACH_N(i, 0, section_count) {
        tb_platform_vprotect(jit_region + sections[i].offset, sections[i].size, sections[i].protect);
    }
    tb_platform_heap_free(func_layout);

    m->jit_region_size = jit_region_size;
    m->jit_region = jit_region;
}
