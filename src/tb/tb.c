#include "tb_internal.h"

#ifdef _WIN32
#define OS_API __stdcall
#else
#define OS_API
#endif

// Used for some optimizations
#if TB_HOST_ARCH == TB_HOST_X86_64
#include <x86intrin.h>
#endif

#define CODE_OUTPUT_BUFFER (32 * 1024 * 1024)

// TODO(NeGate): Doesn't free the name, it's needed for later
static void tb_function_free(TB_Function* f) {
	if (f->is_ir_free) return;
	
#if _WIN32
	// Windows is different :P
	_aligned_free(f->nodes.type);
	_aligned_free(f->nodes.dt);
	_aligned_free(f->nodes.payload);
#else
	free(f->nodes.type);
	free(f->nodes.dt);
	free(f->nodes.payload);
#endif
	
	f->nodes.type = NULL;
	f->nodes.dt = NULL;
	f->nodes.payload = NULL;
	
	f->is_ir_free = true;
}

#define OPT(x) if (tb_opt_ ## x (f)) goto repeat_opt
static void tb_optimize_func(TB_Function* f) {
	repeat_opt: {
		//tb_function_print(f);
		//printf("\n\n\n");
		
		OPT(remove_pass_node);
		OPT(canonicalize);
		OPT(strength_reduction);
		OPT(mem2reg);
		OPT(dce);
		OPT(compact_dead_regs);
	}
	
	//tb_function_print(f);
	//printf("\n\n\n");
}
#undef OPT

static OS_API unsigned long job_system_thread_func(void* lpParam) {
	TB_Module* m = (TB_Module*) lpParam;
	TB_JobSystem* s = m->jobs;
	
	// TODO(NeGate): Properly detect this
	ICodeGen* gen = &x64_fast_code_gen;
	
	size_t i = m->code_region_count++;
	assert(i < TB_MAX_THREADS);
	
	size_t code_size = 0;
	uint8_t* code = VirtualAlloc(NULL,
								 CODE_OUTPUT_BUFFER,
								 MEM_RESERVE | MEM_COMMIT,
								 PAGE_READWRITE);
	
	while (s->running) {
		uint32_t read_ptr = s->read_pointer;
		uint32_t new_read_ptr = (read_ptr + 1) % MAX_JOBS_PER_JOB_SYSTEM;
		
		if (read_ptr == s->write_pointer) {
			// Nothing to do, just wait
			WaitForSingleObjectEx(s->semaphore, -1, false);
			continue;
		}
		
		if (atomic_compare_exchange_strong(&s->read_pointer, &read_ptr, new_read_ptr)) {
			// Compile function
			TB_Function* f = s->functions[read_ptr];
			size_t index = f - m->functions.data;
			assert(f->validated);
			
			if (m->optimization_level != TB_OPT_O0) {
				tb_optimize_func(f);
			}
			
			TB_FunctionOutput* out = &m->compiled_functions.data[index];
			*out = gen->compile_function(f, &m->features, &code[code_size]);
			code_size += out->code_size;
			
			// Free the IR, no longer needed
			tb_function_free(f);
		}
	}
	
	m->code_regions[i].data = code;
	m->code_regions[i].size = code_size;
	return 0;
}

TB_API TB_Module* tb_module_create(TB_Arch target_arch, TB_System target_system, const TB_FeatureSet* features, int optimization_level, int max_threads) {
	assert(max_threads >= 1 && max_threads <= TB_MAX_THREADS);
	TB_Module* m = calloc(1, sizeof(TB_Module));
    
	m->optimization_level = optimization_level;
	m->target_arch = target_arch;
	m->target_system = target_system;
	m->features = *features;
	
#if !TB_STRIP_LABELS
	m->label_symbols.count = 0;
	m->label_symbols.capacity = 64;
	m->label_symbols.data = malloc(64 * sizeof(TB_LabelSymbol));
#endif
	
	m->const32_patches.count = 0;
	m->const32_patches.capacity = 64;
	m->const32_patches.data = malloc(64 * sizeof(TB_ConstPool32Patch));
    
	m->call_patches.count = 0;
	m->call_patches.capacity = 64;
	m->call_patches.data = malloc(64 * sizeof(TB_FunctionPatch));
	
	m->ecall_patches.count = 0;
	m->ecall_patches.capacity = 64;
	m->ecall_patches.data = malloc(64 * sizeof(TB_ExternFunctionPatch));
	
	m->files.count = 1;
	m->files.capacity = 64;
	m->files.data = malloc(64 * sizeof(TB_File));
    m->files.data[0] = (TB_File){ 0 };
	
	m->functions.count = 0;
	m->functions.data = malloc(TB_MAX_FUNCTIONS * sizeof(TB_Function));
    
	m->compiled_functions.count = 0;
	m->compiled_functions.data = malloc(TB_MAX_FUNCTIONS * sizeof(TB_FunctionOutput));
    
	m->line_info_count = 0;
	
#ifdef _WIN32
	TB_JobSystem* j = calloc(1, sizeof(TB_JobSystem));
	m->jobs = j;
	
	j->running = true;
	j->write_pointer = 0;
	j->read_pointer = 0;
	j->thread_count = max_threads;
	
	j->semaphore = CreateSemaphoreExA(0,
									  max_threads, max_threads,
									  0, 0,
									  SEMAPHORE_ALL_ACCESS);
	
	loop(i, max_threads) {
		j->threads[i] = CreateThread(0, 4 * 1024 * 1024,
									 job_system_thread_func,
									 m, 0, 0);
	}
	
	InitializeCriticalSection(&j->mutex);
#endif
	
	return m;
}

TB_API bool tb_module_compile_func(TB_Module* m, TB_Function* f) {
	if (!tb_validate(f)) abort();
	TB_JobSystem* s = m->jobs;
	
#if _WIN32
	EnterCriticalSection(&s->mutex);
#endif
	
	uint32_t write_ptr = s->write_pointer;
	uint32_t new_write_ptr = (write_ptr + 1) % MAX_JOBS_PER_JOB_SYSTEM;
	while (new_write_ptr == s->read_pointer) { 
#if _WIN32
		SwitchToThread();
#endif
	}
	
	s->functions[write_ptr] = f;
	s->write_pointer = new_write_ptr;
	ReleaseSemaphore(s->semaphore, 1, 0);
	
#if _WIN32
	LeaveCriticalSection(&s->mutex);
#endif
	
	return true;
}

TB_API bool tb_module_compile(TB_Module* m) {
	TB_JobSystem* s = m->jobs;
	
	// wait for the threads to finish
	while (s->write_pointer != s->read_pointer) {
		__builtin_ia32_pause();
	}
	
	s->running = false;
	m->compiled_functions.count = m->functions.count;
    
#if _WIN32
	ReleaseSemaphore(m->jobs->semaphore, m->jobs->thread_count, 0);
	WaitForMultipleObjects(s->thread_count, s->threads, TRUE, -1);
	loop(i, s->thread_count) CloseHandle(s->threads[i]);
	
	DeleteCriticalSection(&s->mutex);
#endif
	
	return true;
}

TB_API size_t tb_DEBUG_module_get_full_node_count(TB_Module* m) {
	size_t node_count = 0;
	loop(i, m->functions.count) {
		node_count += m->functions.data[i].nodes.count;
	}
	return node_count;
}

TB_API void tb_module_destroy(TB_Module* m) {
	loop(i, m->functions.count) {
		tb_function_free(&m->functions.data[i]);
		free(m->functions.data[i].name);
	}
    
#if _WIN32
	loop(i, m->code_region_count) {
		VirtualFree(m->code_regions[i].data, 0, MEM_RELEASE);
	}
#else
#error "TODO: setup code region delete"
#endif
    
	free(m->functions.data);
	free(m->compiled_functions.data);
	free(m);
}

TB_API void tb_get_constraints(TB_Arch target_arch, const TB_FeatureSet* features, TB_FeatureConstraints* constraints) {
    *constraints = (TB_FeatureConstraints){};
    
    if (target_arch == TB_ARCH_X86_64) {
        // void and pointers dont get vector types
        constraints->max_vector_width[TB_VOID] = 1;
        constraints->max_vector_width[TB_PTR] = 1;
        
        // Basic stuff that x64 and SSE guarentee
        constraints->max_vector_width[TB_I8] = 16;
        constraints->max_vector_width[TB_I16] = 8;
        constraints->max_vector_width[TB_I32] = 4;
        constraints->max_vector_width[TB_I64] = 2;
        
        constraints->max_vector_width[TB_F32] = 4;
        constraints->max_vector_width[TB_F64] = 2;
        
        // NOTE(NeGate): Booleans aren't a fixed idea
        // in x64 vectors it's generally represented 
        // as the same bit size as the operation that
        // creates it so 16 is picked because of vector
        // byte comparisons being the most you can get
        // from vector bools.
        constraints->max_vector_width[TB_BOOL] = 16;
    } else tb_todo();
}

TB_API TB_FileID tb_register_file(TB_Module* m, const char* path) {
	if (m->files.count + 1 >= m->files.capacity) {
		m->files.capacity *= 2;
		m->files.data = realloc(m->files.data, m->files.capacity * sizeof(TB_File));
	}
    
	char* str = malloc(strlen(path) + 1);
	strcpy(str, path);
	
	size_t r = m->files.count++;
	m->files.data[r] = (TB_File){
		.path = str
	};
	return r;
}

// https://create.stephan-brumme.com/fnv-hash/
// hash a block of memory
static uint32_t fnv1a(const void* data, size_t num_bytes) {
	const unsigned char* ptr = (const unsigned char*)data;
	uint32_t hash = 0x811C9DC5;
	
	while (num_bytes--) {
		hash = ((*ptr++) ^ hash) * 0x01000193;
	}
	
	return hash;
}

TB_API bool tb_module_export(TB_Module* m, FILE* f) {
	const ICodeGen* restrict code_gen = NULL;
	switch (m->target_arch) {
		case TB_ARCH_X86_64: code_gen = &x64_fast_code_gen; break;
		default: tb_todo();
	}
	
	switch (m->target_system) {
        case TB_SYSTEM_WINDOWS:
		tb_export_coff(m, code_gen, f);
		break;
        case TB_SYSTEM_LINUX:
		tb_export_elf64(m, code_gen, f);
		break;
        default:
		printf("TinyBackend error: Unknown system!\n");
		tb_todo();
	}
	
	return true;
}

void tb_resize_node_stream(TB_Function* f, size_t cap) {
	f->nodes.capacity = cap;
	
#if _WIN32
	// Windows is different... but nicer imo here
	f->nodes.type = _aligned_recalloc(f->nodes.type, cap, sizeof(TB_RegType), 64);
	f->nodes.dt = _aligned_recalloc(f->nodes.dt, cap, sizeof(TB_DataType), 64);
	f->nodes.payload = _aligned_recalloc(f->nodes.payload, cap, sizeof(TB_RegPayload), 64);
#else
	if (f->nodes.type) {
		f->nodes.type = realloc(f->nodes.type, cap * sizeof(TB_RegType));
		f->nodes.dt = realloc(f->nodes.dt, cap * sizeof(TB_DataType));
		f->nodes.payload = realloc(f->nodes.payload, cap * sizeof(TB_RegPayload));
	} else {
		f->nodes.type = aligned_alloc(64, cap * sizeof(TB_RegType));
		f->nodes.dt = aligned_alloc(64, cap * sizeof(TB_DataType));
		f->nodes.payload = aligned_alloc(64, cap * sizeof(TB_RegPayload));
	}
	
	// zero out the extra space
	// TODO(NeGate): optimize this... somehow?
	memset(&f->nodes.type[f->nodes.count], 0, (cap - f->nodes.count) * sizeof(TB_RegType));
	memset(&f->nodes.dt[f->nodes.count], 0, (cap - f->nodes.count) * sizeof(TB_DataType));
	memset(&f->nodes.payload[f->nodes.count], 0, (cap - f->nodes.count) * sizeof(TB_RegPayload));
#endif
}

TB_API TB_Function* tb_function_create(TB_Module* m, const char* name, TB_DataType return_dt) {
	
	size_t i = m->functions.count++;
	assert(i < TB_MAX_FUNCTIONS);
	
	TB_Function* f = &m->functions.data[i];
	memset(f, 0, sizeof(TB_Function));
	
	// TODO(NeGate): We might wanna do something better with these strings
	// especially since they'll be packed in a string table eventually
	f->name = malloc(strlen(name) + 1);
	strcpy(f->name, name);
	
	f->return_dt = return_dt;
	f->module = m;
    
	f->nodes.count = 0;
	tb_resize_node_stream(f, 64);
	
	f->parameter_count = 0;
	
	// Null slot
	f->nodes.type[0] = TB_NULL;
	f->nodes.dt[0] = (TB_DataType){ 0 };
	f->nodes.payload[0] = (TB_RegPayload){ 0 };
    
	// Entry label
	f->nodes.type[1] = TB_LABEL;
	f->nodes.dt[1] = TB_TYPE_PTR();
	f->nodes.payload[1].label.id = 0;
	f->nodes.payload[1].label.terminator = TB_NULL_REG;
	f->nodes.payload[1].label.is_loop = false;
	f->nodes.count = 2;
    
	f->current_label = 1;
	return f;
}

TB_API TB_ExternalID tb_module_extern(TB_Module* m, const char* name) {
	if (m->externals.count + 1 >= m->externals.capacity) {
		// TODO(NeGate): This might be excessive for this array, idk :P
		m->externals.capacity = tb_next_pow2(m->externals.count + 1);
		m->externals.data = realloc(m->externals.data, m->externals.capacity * sizeof(TB_External));
	}
	
	// TODO(NeGate): We might wanna do something better with these strings
	// especially since they'll be packed in a string table eventually
	char* new_name = malloc(strlen(name) + 1);
	strcpy(new_name, name);
	
	TB_ExternalID i = m->externals.count++;
	m->externals.data[i].name = new_name;
	return i;
}

TB_API void* tb_module_get_jit_func_by_name(TB_Module* m, const char* name) {
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		if (strcmp(m->compiled_functions.data[i].name, name)) return m->compiled_function_pos[i];
	}
	
	return NULL;
}

TB_API void* tb_module_get_jit_func_by_id(TB_Module* m, size_t i) {
	assert(m->compiled_function_pos);
	return m->compiled_function_pos[i];
}

TB_API void* tb_module_get_jit_func(TB_Module* m, TB_Function* f) {
	assert(m->compiled_function_pos);
	return m->compiled_function_pos[f - m->functions.data];
}

TB_API TB_Label tb_get_current_label(TB_Function* f) {
	if (!f->current_label) return 0;
	
	return f->nodes.payload[f->current_label].label.id;
}

//
// TLS - Thread local storage
// 
// Certain backend elements require memory but we would prefer to avoid 
// making any heap allocations when possible to there's a preallocated 
// block per thread that can run TB.
//
TB_TemporaryStorage* tb_tls_allocate() {
	static _Thread_local uint8_t* tb_thread_storage;
	
	if (tb_thread_storage == NULL) {
		tb_thread_storage = malloc(TB_TEMPORARY_STORAGE_SIZE);
	}
    
	TB_TemporaryStorage* store = (TB_TemporaryStorage*)tb_thread_storage;
	store->used = 0;
	return store;
}

void* tb_tls_push(TB_TemporaryStorage* store, size_t size) {
	assert(sizeof(TB_TemporaryStorage) + store->used + size < TB_TEMPORARY_STORAGE_SIZE);
    
	void* ptr = &store->data[store->used];
	store->used += size;
	return ptr;
}

void* tb_tls_pop(TB_TemporaryStorage* store, size_t size) {
	assert(sizeof(TB_TemporaryStorage) + store->used > size);
    
	store->used -= size;
	return &store->data[store->used];
}

void* tb_tls_peek(TB_TemporaryStorage* store, size_t distance) {
	assert(sizeof(TB_TemporaryStorage) + store->used > distance);
    
	return &store->data[store->used - distance];
}

void tb_emit_call_patch(TB_Module* m, uint32_t func_id, uint32_t target_id, size_t pos) {
	assert(pos < UINT32_MAX);
	if (m->call_patches.count + 1 >= m->call_patches.capacity) {
		m->call_patches.capacity *= 2;
		m->call_patches.data = realloc(m->call_patches.data,
									   m->call_patches.capacity * sizeof(TB_FunctionPatch));
	}
    
	size_t r = m->call_patches.count++;
	m->call_patches.data[r] = (TB_FunctionPatch){
		.func_id = func_id,
		.target_id = target_id,
		.pos = pos
	};
}

void tb_emit_ecall_patch(TB_Module* m, uint32_t func_id, TB_ExternalID target_id, size_t pos) {
	assert(pos < UINT32_MAX);
	if (m->ecall_patches.count + 1 >= m->ecall_patches.capacity) {
		m->ecall_patches.capacity *= 2;
		m->ecall_patches.data = realloc(m->ecall_patches.data,
										m->ecall_patches.capacity * sizeof(TB_FunctionPatch));
	}
    
	size_t r = m->ecall_patches.count++;
	m->ecall_patches.data[r] = (TB_ExternFunctionPatch){
		.func_id = func_id,
		.target_id = target_id,
		.pos = pos
	};
}

//
// IR PRINTER
//
static void tb_print_type(TB_DataType dt) {
	switch (dt.type) {
        case TB_VOID:   printf("[void]   \t"); break;
        case TB_BOOL:   printf("[bool x %d]\t", dt.count); break;
        case TB_I8:     printf("[i8 x %d]\t", dt.count); break;
        case TB_I16:    printf("[i16 x %d]\t", dt.count); break;
        case TB_I32:    printf("[i32 x %d]\t", dt.count); break;
        case TB_I64:    printf("[i64 x %d]\t", dt.count); break;
        case TB_I128:   printf("[i128 x %d]\t", dt.count); break;
        case TB_PTR:    printf("[ptr]    \t"); break;
        case TB_F32:    printf("[f32 x %d]\t", dt.count); break;
        case TB_F64:    printf("[f64 x %d]\t", dt.count); break;
        default:        tb_todo();
	}
}

TB_API void tb_function_print(TB_Function* f) {
	printf("%s():\n", f->name);
    
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		TB_RegType type = f->nodes.type[i];
		TB_DataType dt = f->nodes.dt[i];
		TB_RegPayload p = f->nodes.payload[i];
        
		switch (type) {
            case TB_NULL:
			printf("  r%u\t=\t", i);
			printf(" NOP\n");
			break;
            case TB_INT_CONST:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
            
			if (p.i_const.hi) {
				printf(" %" PRIx64 "%" PRIx64 "\n", p.i_const.hi, p.i_const.lo);
			}
			else {
				printf(" %" PRIu64 "\n", p.i_const.lo);
			}
			break;
			case TB_LINE_INFO:
			//printf("  # LOC %s:%d\n", f->module->files.data[p.line_info.file].path, p.line_info.line);
			break;
            case TB_FLOAT_CONST:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
            printf(" %f\n", p.f_const);
			break;
            case TB_ZERO_EXT:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" ZXT r%u\n", p.ext);
			break;
            case TB_SIGN_EXT:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" SXT r%u\n", p.ext);
			break;
			case TB_MEMSET:
			printf("  MEMSET\t(r%d, r%d, r%d)\n", p.mem_op.dst, p.mem_op.src, p.mem_op.size);
			break;
			case TB_MEMCPY:
			printf("  MEMCPY\t(r%d, r%d, r%d)\n", p.mem_op.dst, p.mem_op.src, p.mem_op.size);
			break;
			case TB_MEMBER_ACCESS:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" &r%u[r%d]\n", p.member_access.base, p.member_access.offset);
			break;
			case TB_ARRAY_ACCESS:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" &r%u[r%u * %u]\n", p.array_access.base, p.array_access.index, p.array_access.stride);
			break;
			case TB_AND:
            case TB_OR:
			case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_UDIV:
            case TB_SDIV:
            case TB_SHL:
            case TB_SHR:
            case TB_SAR:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" r%u ", p.i_arith.a);
            
			switch (type) {
                case TB_AND: printf("&"); break;
                case TB_OR: printf("|"); break;
                case TB_ADD: printf("+"); break;
                case TB_SUB: printf("-"); break;
                case TB_MUL: printf("*"); break;
                case TB_UDIV: printf("/u"); break;
                case TB_SDIV: printf("/s"); break;
				case TB_SHL: printf("<<"); break;
				case TB_SHR: printf(">>"); break;
				case TB_SAR: printf(">>s"); break;
                default: tb_todo();
			}
            
			printf(" r%u\n", p.i_arith.b);
			break;
            case TB_FADD:
            case TB_FSUB:
            case TB_FMUL:
            case TB_FDIV:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" r%u ", p.f_arith.a);
            
			switch (type) {
                case TB_FADD: printf("+"); break;
                case TB_FSUB: printf("-"); break;
                case TB_FMUL: printf("*"); break;
                case TB_FDIV: printf("/"); break;
                default: tb_todo();
			}
            
			printf(" r%u\n", p.f_arith.b);
			break;
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" r%u ", p.cmp.a);
            
			switch (type) {
                case TB_CMP_NE: printf("!="); break;
                case TB_CMP_EQ: printf("=="); break;
                case TB_CMP_ULT: printf("<"); break;
                case TB_CMP_ULE: printf("<="); break;
                case TB_CMP_SLT: printf("<"); break;
                case TB_CMP_SLE: printf("<="); break;
                default: tb_todo();
			}
            
			printf(" r%u", p.cmp.b);
			
			if (type == TB_CMP_SLT || type == TB_CMP_SLE) printf(" # signed\n");
			else printf("\n");
			break;
            case TB_NEG:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" NEG r%u\n", p.unary);
			break;
            case TB_NOT:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" NOT r%u\n", p.unary);
			break;
            case TB_LOCAL:
			printf("  r%u\t=\tLOCAL %d (%d align)\n", i, p.local.size, p.local.alignment);
			break;
            case TB_ICALL:
			printf("  r%u\t=\tINLINE CALL %s(", i, p.call.target->name);
			for (size_t j = p.call.param_start; j < p.call.param_end; j++) {
				if (j != p.call.param_start) printf(", ");
				
				printf("r%u", f->vla.data[j]);
			}
			printf(")\n");
			break;
            case TB_CALL:
			printf("  r%u\t=\tCALL %s(", i, p.call.target->name);
			for (size_t j = p.call.param_start; j < p.call.param_end; j++) {
				if (j != p.call.param_start) printf(", ");
				
				printf("r%u", f->vla.data[j]);
			}
			printf(")\n");
			break;
            case TB_SWITCH: {
				printf(" SWITCH\t");
				tb_print_type(dt);
				printf("\tr%u (\n", p.switch_.key);
				
				size_t entry_start = p.switch_.entries_start;
				size_t entry_count = (p.switch_.entries_end - p.switch_.entries_start) / 2;
				
				for (size_t j = 0; j < entry_count; j++) {
					TB_SwitchEntry* e = (TB_SwitchEntry*)&f->vla.data[entry_start + (j * 2)];
					
					printf("\t\t\t%u -> L%d,\n", e->key, e->value);
				}
				printf("\t\t\tdefault -> L%d)\n", p.switch_.default_label);
				break;
			}
            case TB_PARAM:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf("  PARAM %u\n", p.param.id);
			break;
            case TB_PARAM_ADDR:
			printf("  r%u\t=\t&PARAM %u\n", i, f->nodes.payload[p.param_addr.param].param.id);
			break;
            case TB_LOAD:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" *r%u (%d align)\n", p.load.address, p.load.alignment);
			break;
            case TB_STORE:
			printf(" *r%u \t=\t", p.store.address);
			tb_print_type(dt);
			printf(" r%u (%d align)\n", p.store.value, p.store.alignment);
			break;
            case TB_LABEL:
			printf("L%d: # r%u terminates at r%u\n", p.label.id, i, p.label.terminator);
			break;
            case TB_GOTO:
			printf("  goto L%d\n", p.goto_.label);
			break;
            case TB_IF:
			printf("  if (r%u)\tL%d else L%d\n", p.if_.cond, p.if_.if_true, p.if_.if_false);
			break;
            case TB_PASS:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" PASS r%u\n", p.pass);
			break;
            case TB_PHI1:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" PHI L%d:r%u\n", f->nodes.payload[p.phi1.a_label].label.id, p.phi1.a);
			break;
            case TB_PHI2:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" PHI L%d:r%u, L%d:r%u\n", f->nodes.payload[p.phi2.a_label].label.id, p.phi2.a, f->nodes.payload[p.phi2.b_label].label.id, p.phi2.b);
			break;
            case TB_RET:
			printf("  ret\t\t");
			tb_print_type(dt);
			printf(" r%u\n", p.i_arith.a);
			break;
            default: tb_todo();
		}
	}
}

//
// EMITTER CODE
// 
// Simple linear allocation for the backend's to output code with
//
uint8_t* tb_out_reserve(TB_Emitter* o, size_t count) {
	if (o->count + count >= o->capacity) {
		if (o->capacity == 0) {
			o->capacity = 64;
		}
		else {
			o->capacity += count;
			o->capacity *= 2;
		}
        
		o->data = realloc(o->data, o->capacity);
		if (o->data == NULL) tb_todo();
	}
    
    return &o->data[o->count];
}

void tb_out_commit(TB_Emitter* o, size_t count) {
	assert(o->count + count < o->capacity);
    o->count += count;
}

void tb_out1b_UNSAFE(TB_Emitter* o, uint8_t i) {
	assert(o->count + 1 < o->capacity);
    
	o->data[o->count] = i;
	o->count += 1;
}

void tb_out4b_UNSAFE(TB_Emitter* o, uint32_t i) {
	tb_out_reserve(o, 4);
    
	*((uint32_t*)&o->data[o->count]) = i;
	o->count += 4;
}

void tb_out1b(TB_Emitter* o, uint8_t i) {
	tb_out_reserve(o, 1);
    
	o->data[o->count] = i;
	o->count += 1;
}

void tb_out2b(TB_Emitter* o, uint16_t i) {
	tb_out_reserve(o, 2);
    
	*((uint16_t*)&o->data[o->count]) = i;
	o->count += 2;
}

void tb_out4b(TB_Emitter* o, uint32_t i) {
	tb_out_reserve(o, 4);
    
	*((uint32_t*)&o->data[o->count]) = i;
	o->count += 4;
}

void tb_out8b(TB_Emitter* o, uint64_t i) {
	tb_out_reserve(o, 8);
    
	*((uint64_t*)&o->data[o->count]) = i;
	o->count += 8;
}

void tb_outstr_UNSAFE(TB_Emitter* o, const char* str) {
	while (*str) o->data[o->count++] = *str++;
}

void tb_outs_UNSAFE(TB_Emitter* o, size_t len, const uint8_t* str) {
	for (size_t i = 0; i < len; i++) o->data[o->count + i] = str[i];
	o->count += len;
}

