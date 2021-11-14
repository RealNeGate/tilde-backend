#ifdef _WIN32
#define TB_INTERNAL
#include "tb.h"

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
	char* mini_out_buffer = tb_tls_push(tls, 64);
	
	// Buffer stores all the positions of each 
	// function relative to the .text section start.
	uint32_t* func_layout = (uint32_t*)tb_tls_push(tls, m->compiled_functions.count * sizeof(uint32_t));
	size_t text_section_size = 0;
	
#if TB_HOST_ARCH == TB_HOST_X86_64
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
#else
#error "Cannot compile JIT for this target architecture!"
#endif
	
	// Patch the function calls
	for (size_t i = 0; i < m->call_patches.count; i++) {
		TB_FunctionPatch* p = &m->call_patches.data[i];
		TB_FunctionOutput* out_f = &m->compiled_functions.data[p->func_id];
		uint8_t* code = m->compiled_functions.data[p->func_id].code;
		
		// TODO(NeGate): Consider caching this value if it gets expensive to calculate.
		uint32_t actual_pos = func_layout[p->func_id] + p->pos + 4;
		
		actual_pos += code_gen->get_prologue_length(out_f->prologue_epilogue_metadata,
													out_f->stack_usage);
		
		*((uint32_t*)&code[p->pos]) = func_layout[p->target_id] - actual_pos;
	}
	
	// TODO(NeGate): Implement rdata
	assert(m->const32_patches.count == 0);
	
	// Output function
	m->jit_region_size = text_section_size;
	m->jit_region = VirtualAlloc(NULL, text_section_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
	
	uint8_t* text_section = m->jit_region;
#if TB_HOST_ARCH == TB_HOST_X86_64
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		TB_FunctionOutput* out_f = &m->compiled_functions.data[i];
		m->compiled_function_pos[i] = (void*)text_section;
		
		// prologue
		size_t prologue_len = code_gen->emit_prologue(mini_out_buffer,
													  out_f->prologue_epilogue_metadata,
													  out_f->stack_usage);
		memcpy(text_section, mini_out_buffer, prologue_len);
		text_section += prologue_len;
		
		// body
		memcpy(text_section, m->compiled_functions.data[i].code, m->compiled_functions.data[i].code_size);
		text_section += m->compiled_functions.data[i].code_size;
		
		// epilogue
		size_t epilogue_len = code_gen->emit_epilogue(mini_out_buffer,
													  out_f->prologue_epilogue_metadata,
													  out_f->stack_usage);
		memcpy(text_section, mini_out_buffer, epilogue_len);
		text_section += epilogue_len;
	}
#else
#error "Cannot compile JIT for this target architecture!"
#endif
	
	// convert to executable
	DWORD old_protect;
	if (!VirtualProtect(m->jit_region, m->jit_region_size, PAGE_EXECUTE_READ, &old_protect)) {
		abort();
	}
}

#endif
