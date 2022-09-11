#include "tb_internal.h"
#include "host.h"

// NOTE(NeGate): This only currently supports the text and rdata sections,
// it puts the rdata on the next 4KB page after the text section all within
// the same memory mapping, this is actually very bad because it means that
// read-only data is executable.
void tb_module_export_jit(TB_Module* m) {
    ICodeGen* restrict codegen = tb__find_code_generator(m);

    // Buffer stores all the positions of each
    // function relative to the .text section start.
    uint32_t* func_layout = tb_platform_heap_alloc((m->functions.count + 1) * sizeof(uint32_t));
    size_t text_section_size = 0;

    for (size_t i = 0; i < m->functions.count; i++) {
        func_layout[i] = text_section_size;

        TB_FunctionOutput* out_f = m->functions.data[i].output;
        if (out_f) text_section_size += out_f->code_size;
    }

    // Target specific: resolve internal call patches
    codegen->emit_call_patches(m, func_layout);

    // TODO(NeGate): Implement rdata
    // Output function
    m->jit_region_size = text_section_size;
    m->jit_region = tb_platform_valloc(text_section_size);

    // TODO(NeGate): We might wanna implement a fancy free list in the JIT code region
    uint8_t* text_section = m->jit_region;
    FOREACH_N(i, 0, m->functions.count) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;
        m->functions.data[i].compiled_pos = (void*)text_section;

        // Copy into JIT region
        memcpy(text_section, out_f->code, out_f->code_size);
        text_section += out_f->code_size;
    }

    // Emit external patches
    FOREACH_N(i, 0, m->max_threads) {
        dyn_array_for(j, m->thread_info[i].ecall_patches) {
            TB_ExternFunctionPatch* p = &m->thread_info[i].ecall_patches[j];
            TB_FunctionOutput* out_f = p->source->output;

            size_t actual_pos = func_layout[p->source - m->functions.data] +
                out_f->prologue_length + p->pos;

            // TODO(NeGate): Implement something smarter... this will break
            // if you somehow compile an external that's like 2GB away
            ptrdiff_t displacement = ((uintptr_t) p->target->address) - actual_pos;
            assert(displacement == (int32_t)displacement);
            *((int32_t*)&text_section[actual_pos]) = displacement;
        }
    }

    tb_platform_heap_free(func_layout);

    // convert to executable
    if (!tb_platform_vprotect(m->jit_region, m->jit_region_size, true)) {
        printf("FUCK!!!\n");
        abort();
    }
}
