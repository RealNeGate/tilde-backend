#include "tb_internal.h"

// Used for some optimizations
#if TB_HOST_ARCH == TB_HOST_X86_64
#include <x86intrin.h>
#endif

#define CODE_OUTPUT_BUFFER (256 * 1024 * 1024)

static _Thread_local uint8_t* tb_thread_storage;

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
	
	free(f->vla.data);
	
	f->nodes.type = NULL;
	f->nodes.dt = NULL;
	f->nodes.payload = NULL;
	f->vla.data = NULL;
	
	f->is_ir_free = true;
}

#define OPT(x) if (tb_opt_ ## x (f)) { \
printf("%s   ", #x); \
tb_function_print(f, stdout); \
printf("\n\n\n"); \
goto repeat_opt; \
}

static void tb_optimize_func(TB_Function* f) {
	repeat_opt: {
		OPT(remove_pass_node);
		OPT(canonicalize);
		OPT(fold);
		OPT(load_elim);
		OPT(strength_reduction);
		OPT(dce);
		OPT(hoist_locals);
		OPT(mem2reg);
		OPT(compact_dead_regs);
	}
	
	printf("FINAL   ");
	tb_function_print(f, stdout);
	printf("\n\n\n");
}
#undef OPT

#if _WIN32
static __stdcall unsigned long job_system_thread_func(void* lpParam)
#else
static void* job_system_thread_func(void* lpParam)
#endif
{
	TB_Module* m = (TB_Module*) lpParam;
	TB_JobSystem* s = m->jobs;
	
	// TODO(NeGate): Properly detect this
	ICodeGen* gen = &x64_fast_code_gen;
	
	size_t i = m->code_region_count++;
	assert(i < TB_MAX_THREADS);
	
	size_t code_size = 0;
	uint8_t* code = tb_platform_valloc(CODE_OUTPUT_BUFFER);
	
	while (s->running) {
		uint32_t read_ptr = s->read_pointer;
		uint32_t new_read_ptr = (read_ptr + 1) % MAX_JOBS_PER_JOB_SYSTEM;
		
		if (read_ptr == s->write_pointer) {
			// Nothing to do, just wait
#if _WIN32
			WaitForSingleObjectEx(s->semaphore, -1, false);
#else
			sem_wait(s->semaphore);
#endif
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
			
			// Free the IR, no longer needed,
            // unless we say otherwise
            if (!m->preserve_ir_after_submit) tb_function_free(f);
		}
	}
	
	m->code_regions[i].data = code;
	m->code_regions[i].size = code_size;
	
	free(tb_thread_storage);
	return 0;
}

TB_API TB_Module* tb_module_create(TB_Arch target_arch,
                                   TB_System target_system,
                                   const TB_FeatureSet* features,
                                   int optimization_level,
                                   int max_threads,
                                   bool preserve_ir_after_submit) {
	assert(max_threads >= 1 && max_threads <= TB_MAX_THREADS);
	TB_Module* m = calloc(1, sizeof(TB_Module));
    
    m->preserve_ir_after_submit = preserve_ir_after_submit;
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
	
	TB_JobSystem* j = calloc(1, sizeof(TB_JobSystem));
	m->jobs = j;
	
	j->running = true;
	j->write_pointer = 0;
	j->read_pointer = 0;
	j->thread_count = max_threads;
	
#ifdef _WIN32
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
#else
	j->semaphore = sem_open(strdup("BITCH"), 0, 0, max_threads);
	
	loop(i, max_threads) {
		pthread_create(&j->threads[i], 0, job_system_thread_func, m);
	}
	
	pthread_mutex_init(&j->mutex, NULL);
#endif
	
	return m;
}

TB_API bool tb_module_compile_func(TB_Module* m, TB_Function* f) {
	if (!tb_validate(f)) abort();
	TB_JobSystem* s = m->jobs;
	
#if _WIN32
	EnterCriticalSection(&s->mutex);
#else
	pthread_mutex_lock(&s->mutex);
#endif
	
	uint32_t write_ptr = s->write_pointer;
	uint32_t new_write_ptr = (write_ptr + 1) % MAX_JOBS_PER_JOB_SYSTEM;
	while (new_write_ptr == s->read_pointer) { 
#if _WIN32
		SwitchToThread();
#else
		sleep(0);
#endif
	}
	
	s->functions[write_ptr] = f;
	s->write_pointer = new_write_ptr;
	
#if _WIN32
	ReleaseSemaphore(s->semaphore, 1, 0);
	LeaveCriticalSection(&s->mutex);
#else
	sem_post(s->semaphore);
	pthread_mutex_unlock(&s->mutex);
#endif
	
	return true;
}

TB_API bool tb_module_compile(TB_Module* m) {
	TB_JobSystem* s = m->jobs;
	
	while (s->write_pointer != s->read_pointer) {
		__builtin_ia32_pause();
	}
	s->running = false;
	m->compiled_functions.count = m->functions.count;
	
#if _WIN32
	ReleaseSemaphore(s->semaphore, s->thread_count, 0);
	WaitForMultipleObjects(s->thread_count, s->threads, TRUE, -1);
	
	loop(i, s->thread_count) {
		CloseHandle(s->threads[i]);
		s->threads[i] = NULL;
	}
	
	DeleteCriticalSection(&s->mutex);
	CloseHandle(s->semaphore);
	
	s->semaphore = NULL;
#else
	// TODO(NeGate): Delete the stuff
	sem_close(s->semaphore); 
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
		m->functions.data[i].name = NULL;
	}
    
	loop(i, m->code_region_count) {
		tb_platform_vfree(m->code_regions[i].data, CODE_OUTPUT_BUFFER);
	}
	
	if (m->jit_region) {
		tb_platform_vfree(m->jit_region, m->jit_region_size);
		m->jit_region = NULL;
	}
	
	loop_range(i, 1, m->files.count) {
		free(m->files.data[i].path);
	}
    
	free(m->jobs);
#if !TB_STRIP_LABELS
	free(m->label_symbols.data);
#endif
	free(m->const32_patches.data);
	free(m->call_patches.data);
	free(m->ecall_patches.data);
	free(m->files.data);
	free(m->functions.data);
	free(m->externals.data);
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
	f->nodes.dt[1] = TB_TYPE_PTR;
	f->nodes.payload[1].label.id = 0;
	f->nodes.payload[1].label.terminator = TB_NULL_REG;
	f->nodes.payload[1].label.is_loop = false;
	f->nodes.count = 2;
    
	f->label_count = 1;
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
static void tb_print_type(FILE* out, TB_DataType dt) {
	switch (dt.type) {
        case TB_VOID:   printf("[void]   \t"); break;
        case TB_BOOL:   printf("[bool x %d]\t", dt.count); break;
        case TB_I8:     printf("[i8  x %d]\t", dt.count); break;
        case TB_I16:    printf("[i16 x %d]\t", dt.count); break;
        case TB_I32:    printf("[i32 x %d]\t", dt.count); break;
        case TB_I64:    printf("[i64 x %d]\t", dt.count); break;
        case TB_PTR:    printf("[ptr]    \t"); break;
        case TB_F32:    printf("[f32 x %d]\t", dt.count); break;
        case TB_F64:    printf("[f64 x %d]\t", dt.count); break;
        default:        tb_todo();
	}
}

TB_API void tb_function_print(TB_Function* f, FILE* out) {
    TB_Module* m = f->module;
	fprintf(out, "%s():\n", f->name);
    
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		TB_RegType type = f->nodes.type[i];
		TB_DataType dt = f->nodes.dt[i];
		TB_RegPayload p = f->nodes.payload[i];
        
		switch (type) {
            case TB_NULL:
			fprintf(out, "  r%u\t=\t", i);
			fprintf(out, " NOP\n");
			break;
            case TB_INT_CONST:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " %" PRIu64 "\n", p.i_const);
			break;
			case TB_LINE_INFO:
			//fprintf(out, "  # LOC %s:%d\n", f->module->files.data[p.line_info.file].path, p.line_info.line);
			break;
            case TB_FLOAT_CONST:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
            fprintf(out, " %f\n", p.f_const);
			break;
            case TB_ZERO_EXT:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " ZXT r%u\n", p.ext);
			break;
            case TB_SIGN_EXT:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " SXT r%u\n", p.ext);
			break;
            case TB_TRUNCATE:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " TRUNC r%u\n", p.trunc);
			break;
			case TB_MEMSET:
			fprintf(out, "  MEMSET\t(r%d, r%d, r%d)\n", p.mem_op.dst, p.mem_op.src, p.mem_op.size);
			break;
			case TB_MEMCPY:
			fprintf(out, "  MEMCPY\t(r%d, r%d, r%d)\n", p.mem_op.dst, p.mem_op.src, p.mem_op.size);
			break;
			case TB_MEMBER_ACCESS:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " r%u.data[%d]\n", p.member_access.base, p.member_access.offset);
			break;
			case TB_ARRAY_ACCESS:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " &r%u[r%u * %d]\n", p.array_access.base, p.array_access.index, p.array_access.stride);
			break;
			case TB_AND:
            case TB_OR:
            case TB_XOR:
			case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_UDIV:
            case TB_SDIV:
            case TB_SHL:
            case TB_SHR:
            case TB_SAR:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " r%u ", p.i_arith.a);
            
			switch (type) {
                case TB_AND: fprintf(out, "&"); break;
                case TB_OR: fprintf(out, "|"); break;
                case TB_XOR: fprintf(out, "^"); break;
                case TB_ADD: fprintf(out, "+"); break;
                case TB_SUB: fprintf(out, "-"); break;
                case TB_MUL: fprintf(out, "*"); break;
                case TB_UDIV: fprintf(out, "/u"); break;
                case TB_SDIV: fprintf(out, "/s"); break;
				case TB_SHL: fprintf(out, "<<"); break;
				case TB_SHR: fprintf(out, ">>"); break;
				case TB_SAR: fprintf(out, ">>s"); break;
                default: tb_todo();
			}
            
			fprintf(out, " r%u\n", p.i_arith.b);
			break;
            case TB_FADD:
            case TB_FSUB:
            case TB_FMUL:
            case TB_FDIV:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " r%u ", p.f_arith.a);
            
			switch (type) {
                case TB_FADD: fprintf(out, "+"); break;
                case TB_FSUB: fprintf(out, "-"); break;
                case TB_FMUL: fprintf(out, "*"); break;
                case TB_FDIV: fprintf(out, "/"); break;
                default: tb_todo();
			}
            
			fprintf(out, " r%u\n", p.f_arith.b);
			break;
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " r%u ", p.cmp.a);
            
			switch (type) {
                case TB_CMP_NE: fprintf(out, "!="); break;
                case TB_CMP_EQ: fprintf(out, "=="); break;
                case TB_CMP_ULT: fprintf(out, "<"); break;
                case TB_CMP_ULE: fprintf(out, "<="); break;
                case TB_CMP_SLT: fprintf(out, "<"); break;
                case TB_CMP_SLE: fprintf(out, "<="); break;
                default: tb_todo();
			}
            
			fprintf(out, " r%u", p.cmp.b);
			
			if (type == TB_CMP_SLT || type == TB_CMP_SLE) fprintf(out, " # signed\n");
			else fprintf(out, "\n");
			break;
            case TB_NEG:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " NEG r%u\n", p.unary);
			break;
            case TB_NOT:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " NOT r%u\n", p.unary);
			break;
            case TB_LOCAL:
			fprintf(out, "  r%u\t=\tLOCAL %d (%d align)\n", i, p.local.size, p.local.alignment);
			break;
            case TB_ICALL:
			fprintf(out, "  r%u\t=\tINLINE CALL %s(", i, p.call.target->name);
			for (size_t j = p.call.param_start; j < p.call.param_end; j++) {
				if (j != p.call.param_start) fprintf(out, ", ");
				
				fprintf(out, "r%u", f->vla.data[j]);
			}
			fprintf(out, ")\n");
			break;
            case TB_ECALL:
			fprintf(out, "  r%u\t=\tECALL %s(", i, m->externals.data[p.ecall.target].name);
			for (size_t j = p.ecall.param_start; j < p.ecall.param_end; j++) {
				if (j != p.ecall.param_start) fprintf(out, ", ");
				
				fprintf(out, "r%u", f->vla.data[j]);
			}
			fprintf(out, ")\n");
			break;
            case TB_CALL:
			fprintf(out, "  r%u\t=\tCALL %s(", i, p.call.target->name);
			for (size_t j = p.call.param_start; j < p.call.param_end; j++) {
				if (j != p.call.param_start) fprintf(out, ", ");
				
				fprintf(out, "r%u", f->vla.data[j]);
			}
			fprintf(out, ")\n");
			break;
            case TB_VCALL:
			fprintf(out, "  r%u\t=\tVCALL r%u(", i, p.vcall.target);
			for (size_t j = p.vcall.param_start; j < p.vcall.param_end; j++) {
				if (j != p.vcall.param_start) fprintf(out, ", ");
				
				fprintf(out, "r%u", f->vla.data[j]);
			}
			fprintf(out, ")\n");
			break;
            case TB_SWITCH: {
				fprintf(out, " SWITCH\t");
				tb_print_type(out, dt);
				fprintf(out, "\tr%u (\n", p.switch_.key);
				
				size_t entry_start = p.switch_.entries_start;
				size_t entry_count = (p.switch_.entries_end - p.switch_.entries_start) / 2;
				
				for (size_t j = 0; j < entry_count; j++) {
					TB_SwitchEntry* e = (TB_SwitchEntry*)&f->vla.data[entry_start + (j * 2)];
					
					fprintf(out, "\t\t\t%u -> L%d,\n", e->key, e->value);
				}
				fprintf(out, "\t\t\tdefault -> L%d)\n", p.switch_.default_label);
				break;
			}
            case TB_PARAM:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, "  PARAM %u\n", p.param.id);
			break;
            case TB_PARAM_ADDR:
			fprintf(out, "  r%u\t=\t&PARAM %u\n", i, f->nodes.payload[p.param_addr.param].param.id);
			break;
            case TB_LOAD:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " *r%u (%d align)\n", p.load.address, p.load.alignment);
			break;
            case TB_STORE:
			fprintf(out, " *r%u \t=\t", p.store.address);
			tb_print_type(out, dt);
			fprintf(out, " r%u (%d align)\n", p.store.value, p.store.alignment);
			break;
            case TB_LABEL:
			fprintf(out, "L%d: # r%u terminates at r%u\n", p.label.id, i, p.label.terminator);
			break;
            case TB_GOTO:
			fprintf(out, "  goto L%d\n", p.goto_.label);
			break;
            case TB_IF:
			fprintf(out, "  if (r%u)\tL%d else L%d\n", p.if_.cond, p.if_.if_true, p.if_.if_false);
			break;
            case TB_PASS:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " PASS r%u\n", p.pass);
			break;
            case TB_PHI1:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " PHI L%d:r%u\n", f->nodes.payload[p.phi1.a_label].label.id, p.phi1.a);
			break;
            case TB_PHI2:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " PHI L%d:r%u, L%d:r%u\n", f->nodes.payload[p.phi2.a_label].label.id, p.phi2.a, f->nodes.payload[p.phi2.b_label].label.id, p.phi2.b);
			break;
            case TB_RET:
			fprintf(out, "  ret\t\t");
			tb_print_type(out, dt);
			fprintf(out, " r%u\n", p.i_arith.a);
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

