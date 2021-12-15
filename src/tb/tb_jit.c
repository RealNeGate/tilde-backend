#ifdef _WIN32
#include "tb_internal.h"

#include <windows.h>

// NOTE(NeGate): This only currently supports the text and rdata sections,
// it puts the rdata on the next 4KB page after the text section all within
// the same memory mapping, this is actually very bad because it means that
// read-only data is executable.
void tb_module_export_jit(TB_Module* m) {
#if TB_HOST_ARCH == TB_HOST_X86_64
	const ICodeGen* restrict code_gen = &x64_fast_code_gen;
#else
#error "Cannot compile JIT for this target architecture!"
#endif
	
	TB_TemporaryStorage* tls = tb_tls_allocate();
	m->compiled_function_pos = malloc(m->compiled_functions.count * sizeof(void*));
	
	// The prologue and epilogue generators need some storage
	uint8_t* proepi_buffer = tb_tls_push(tls, PROEPI_BUFFER);
	
	// Buffer stores all the positions of each 
	// function relative to the .text section start.
	uint32_t* func_layout = malloc(m->compiled_functions.count * sizeof(uint32_t));
	size_t text_section_size = 0;
	
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		func_layout[i] = text_section_size;
		
		// TODO(NeGate): This data could be arranged better for streaming
		size_t prologue = code_gen->get_prologue_length(m->compiled_functions.data[i].prologue_epilogue_metadata,
														m->compiled_functions.data[i].stack_usage);
		
		size_t epilogue = code_gen->get_epilogue_length(m->compiled_functions.data[i].prologue_epilogue_metadata,
														m->compiled_functions.data[i].stack_usage);
		
		text_section_size += prologue;
		text_section_size += epilogue;
		text_section_size += m->compiled_functions.data[i].code_size;
	}
	
	// Target specific: resolve internal call patches
	code_gen->emit_call_patches(m, func_layout);
	
	// TODO(NeGate): Implement rdata
	// Output function
	m->jit_region_size = text_section_size;
	m->jit_region = tb_platform_valloc(text_section_size);
	
	uint8_t* text_section = m->jit_region;
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		TB_FunctionOutput* out_f = &m->compiled_functions.data[i];
		m->compiled_function_pos[i] = (void*)text_section;
		
		uint64_t meta = out_f->prologue_epilogue_metadata;
		uint64_t stack_usage = out_f->stack_usage;
		const uint8_t* code = out_f->code;
		size_t code_size = out_f->code_size;
		
		uint8_t* prologue = proepi_buffer;
		size_t prologue_len = code_gen->emit_prologue(prologue, meta, stack_usage);
		
		uint8_t* epilogue = proepi_buffer + prologue_len;
		size_t epilogue_len = code_gen->emit_epilogue(epilogue, meta, stack_usage);
		
		// Copy into JIT region
		memcpy(text_section, prologue, prologue_len);
		text_section += prologue_len;
		
		memcpy(text_section, code, code_size);
		text_section += code_size;
		
		memcpy(text_section, epilogue, epilogue_len);
		text_section += epilogue_len;
	}
	
	free(func_layout);
	
	// convert to executable
	if (!tb_platform_vprotect(m->jit_region, m->jit_region_size, true)) {
		printf("FUCK!!!\n");
		abort();
	}
}

#endif
