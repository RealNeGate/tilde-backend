#ifdef _WIN32
#define TB_INTERNAL
#include "tb.h"

#include <windows.h>

// NOTE(NeGate): This only currently supports the text and rdata sections,
// it puts the rdata on the next 4KB page after the text section all within
// the same memory mapping, this is actually very bad because it means that
// read-only data is executable.
TB_API void tb_module_export_jit(TB_Module* m) {
	TB_TemporaryStorage* tls = tb_tls_allocate();
	m->compiled_function_pos = malloc(m->compiled_functions.count * sizeof(void*));
	
	// Buffer stores all the positions of each 
	// function relative to the .text section start.
	uint32_t* func_layout = (uint32_t*)tb_tls_push(tls, m->compiled_functions.count * sizeof(uint32_t));
	size_t text_section_size = 0;
	
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		func_layout[i] = text_section_size;
		
		if (m->compiled_functions.data[i].has_no_prologue) {
			text_section_size += m->compiled_functions.data[i].emitter.count - 7;
		}
		else {
			text_section_size += m->compiled_functions.data[i].emitter.count;
		}
	}
	
	// Patch the function calls
	for (size_t i = 0; i < m->call_patches.count; i++) {
		TB_FunctionPatch* p = &m->call_patches.data[i];
		uint8_t* code = m->compiled_functions.data[p->func_id].emitter.data;
		
		*((uint32_t*)&code[p->pos]) = func_layout[p->target_id] - (func_layout[p->func_id] + p->pos + 4);
	}
	
	// TODO(NeGate): Implement rdata
	assert(m->const32_patches.count == 0);
	
	// Output function
	m->jit_region_size = text_section_size;
	m->jit_region = VirtualAlloc(NULL, text_section_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
	
	uint8_t* text_section = m->jit_region;
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		m->compiled_function_pos[i] = (void*)text_section;
		
		// On x64, the smallest prologue is 7 bytes so if there's no prologue
		// then just skip it :)
		if (m->compiled_functions.data[i].has_no_prologue) {
			memcpy(text_section, m->compiled_functions.data[i].emitter.data + 7, m->compiled_functions.data[i].emitter.count - 7);
			text_section += (m->compiled_functions.data[i].emitter.count - 7);
		}
		else {
			memcpy(text_section, m->compiled_functions.data[i].emitter.data, m->compiled_functions.data[i].emitter.count);
			text_section += m->compiled_functions.data[i].emitter.count;
		}
	}
	
	// convert to executable
	DWORD old_protect;
	if (!VirtualProtect(m->jit_region, m->jit_region_size, PAGE_EXECUTE_READ, &old_protect)) {
		abort();
	}
}

#endif
