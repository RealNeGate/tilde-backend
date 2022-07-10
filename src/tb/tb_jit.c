#include "tb_internal.h"

// NOTE(NeGate): This only currently supports the text and rdata sections,
// it puts the rdata on the next 4KB page after the text section all within
// the same memory mapping, this is actually very bad because it means that
// read-only data is executable.
void tb_module_export_jit(TB_Module* m, TB_ISelMode isel_mode) {
    #if TB_HOST_ARCH == TB_HOST_X86_64
    const ICodeGen* restrict codegen = &x64_codegen;
    #else
    #error "Cannot compile JIT for this target architecture!"
    #endif

    TB_TemporaryStorage* tls = tb_tls_allocate();
    m->compiled_function_pos = tb_platform_heap_alloc(m->functions.count * sizeof(void*));

    // The prologue and epilogue generators need some storage
    uint8_t* proepi_buffer = tb_tls_push(tls, PROEPI_BUFFER);

    // Buffer stores all the positions of each
    // function relative to the .text section start.
    uint32_t* func_layout = tb_platform_heap_alloc(m->functions.count * sizeof(uint32_t));
    size_t text_section_size = 0;

    for (size_t i = 0; i < m->functions.count; i++) {
        func_layout[i] = text_section_size;

        // TODO(NeGate): This data could be arranged better for streaming
        size_t prologue = codegen->get_prologue_length(
            m->functions.data[i].output->prologue_epilogue_metadata,
            m->functions.data[i].output->stack_usage
        );

        size_t epilogue = codegen->get_epilogue_length(
            m->functions.data[i].output->prologue_epilogue_metadata,
            m->functions.data[i].output->stack_usage
        );

        text_section_size += prologue;
        text_section_size += epilogue;
        text_section_size += m->functions.data[i].output->code_size;
    }

    // Target specific: resolve internal call patches
    codegen->emit_call_patches(m, func_layout);

    // TODO(NeGate): Implement rdata
    // Output function
    m->jit_region_size = text_section_size;
    m->jit_region      = tb_platform_valloc(text_section_size);

    uint8_t* text_section = m->jit_region;
    loop(i, m->functions.count) {
        TB_FunctionOutput* out_f = m->functions.data[i].output;
        m->compiled_function_pos[i] = (void*)text_section;

        uint64_t meta = out_f->prologue_epilogue_metadata;
        uint64_t stack_usage = out_f->stack_usage;
        const uint8_t* code = out_f->code;
        size_t code_size = out_f->code_size;

        uint8_t* prologue = proepi_buffer;
        size_t prologue_len = codegen->emit_prologue(prologue, meta, stack_usage);

        uint8_t* epilogue = proepi_buffer + prologue_len;
        size_t epilogue_len = codegen->emit_epilogue(epilogue, meta, stack_usage);

        // Copy into JIT region
        memcpy(text_section, prologue, prologue_len);
        text_section += prologue_len;

        memcpy(text_section, code, code_size);
        text_section += code_size;

        memcpy(text_section, epilogue, epilogue_len);
        text_section += epilogue_len;
    }

    // Emit external patches
    loop(i, m->max_threads) {
        dyn_array_for(j, m->thread_info[i].ecall_patches) {
            TB_ExternFunctionPatch* p = &m->thread_info[i].ecall_patches[j];
            TB_FunctionOutput* out_f = p->source->output;

            uint64_t meta = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;

            uintptr_t actual_pos = func_layout[p->source - m->functions.data] +
                codegen->get_prologue_length(meta, stack_usage) + p->pos;

            // TODO(NeGate): Implement something smarter... this will break
            // if you somehow compile an external that's like 2GB away
            intptr_t displacement = ((uintptr_t) p->target->address) - actual_pos;
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
