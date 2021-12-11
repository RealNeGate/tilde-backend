#include "tb_internal.h"

// Used for some optimizations
#if TB_HOST_ARCH == TB_HOST_X86_64
#include <x86intrin.h>
#endif

#define CODE_OUTPUT_BUFFER (256 * 1024 * 1024)

static _Thread_local uint8_t* tb_thread_storage;

static _Atomic int total_tid;
static _Thread_local int tid;

static int get_local_thread_id() {
	// the value it spits out is zero-based, but
	// the TIDs consider zero as a NULL space.
	if (tid == 0) {
		int new_id = total_tid++;
		tid = new_id + 1;
	}
	
	return tid - 1;
}

// TODO(NeGate): Doesn't free the name, it's needed for later
static void tb_function_free(TB_Function* f) {
	if (f->is_ir_free) return;
	
	tb_platform_heap_free(f->nodes.type);
	tb_platform_heap_free(f->nodes.dt);
	tb_platform_heap_free(f->nodes.payload);
	tb_platform_heap_free(f->vla.data);
	
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
	
	TB_FifoQueue* restrict q = &s->queue[i];
	while (true) {
		uint32_t read_ptr = q->read_pointer;
		uint32_t new_read_ptr = (read_ptr + 1) % MAX_JOBS_PER_JOB_SYSTEM;
		
		if (read_ptr == q->write_pointer) {
			// Nothing to do, just wait
			//printf("We waitin'\n");
#if _WIN32
			WaitForSingleObjectEx(q->semaphore, -1, false);
#else
			sem_wait(q->semaphore);
#endif
			
			// only actually shut off if we run out of work
			// and are told to stop.
			if (m->compiled_functions.completed == m->compiled_functions.count 
				&& s->running == false) {
				break;
			}
			
			continue;
		}
		
		if (atomic_compare_exchange_strong(&q->read_pointer, &read_ptr, new_read_ptr)) {
			// Compile function
			TB_Function* f = q->functions[read_ptr];
			size_t index = f - m->functions.data;
			
			if (m->optimization_level != TB_OPT_O0) {
				tb_optimize_func(f);
			}
			
			TB_FunctionOutput* out = &m->compiled_functions.data[index];
			*out = gen->compile_function(f, &m->features, &code[code_size], i);
			code_size += out->code_size;
			
			// Free the IR, no longer needed,
            // unless we say otherwise
            if (!m->preserve_ir_after_submit) tb_function_free(f);
			
			m->compiled_functions.completed++;
		}
	}
	
	m->code_regions[i].data = code;
	m->code_regions[i].size = code_size;
	
	tb_platform_vfree(tb_thread_storage, TB_TEMPORARY_STORAGE_SIZE);
	return 0;
}

TB_API TB_Module* tb_module_create(TB_Arch target_arch,
                                   TB_System target_system,
                                   const TB_FeatureSet* features,
                                   int optimization_level,
                                   int max_threads,
                                   bool preserve_ir_after_submit) {
	assert(max_threads >= 1 && max_threads <= TB_MAX_THREADS);
	TB_Module* m = tb_platform_heap_alloc(sizeof(TB_Module));
    memset(m, 0, sizeof(TB_Module));
	
	m->max_threads = max_threads;
    m->preserve_ir_after_submit = preserve_ir_after_submit;
	m->optimization_level = optimization_level;
	m->target_arch = target_arch;
	m->target_system = target_system;
	m->features = *features;
	
#if !TB_STRIP_LABELS
	m->label_symbols.count = 0;
	m->label_symbols.capacity = 64;
	m->label_symbols.data = tb_platform_heap_alloc(64 * sizeof(TB_LabelSymbol));
#endif
	
	m->const32_patches.count = 0;
	m->const32_patches.capacity = 64;
	m->const32_patches.data = tb_platform_heap_alloc(64 * sizeof(TB_ConstPool32Patch));
    
	m->externals.count = 0;
	m->externals.capacity = 64;
	m->externals.data = tb_platform_heap_alloc(64 * sizeof(TB_External));
	
	m->files.count = 1;
	m->files.capacity = 64;
	m->files.data = tb_platform_heap_alloc(64 * sizeof(TB_File));
    m->files.data[0] = (TB_File){ 0 };
	
	m->functions.count = 0;
	m->functions.data = tb_platform_heap_alloc(TB_MAX_FUNCTIONS * sizeof(TB_Function));
    
	m->compiled_functions.count = 0;
	m->compiled_functions.data = tb_platform_heap_alloc(TB_MAX_FUNCTIONS * sizeof(TB_FunctionOutput));
    
	m->line_info_count = 0;
	
	TB_JobSystem* j = tb_platform_heap_alloc(sizeof(TB_JobSystem));
	// TODO(NeGate): Kinda expensive but pretend it isn't lol
	memset(j, 0, sizeof(TB_JobSystem));
	
	m->jobs = j;
	
	j->running = true;
	j->thread_count = max_threads;
	
#ifdef _WIN32
	loop(i, max_threads) {
		j->queue[i].semaphore = CreateSemaphoreExA(0, 1, 1, 0, 0, SEMAPHORE_ALL_ACCESS);
		j->threads[i] = CreateThread(0, 4 * 1024 * 1024,
									 job_system_thread_func,
									 m, 0, 0);
	}
	
	InitializeCriticalSection(&m->mutex);
#else
	loop(i, max_threads) {
		pthread_create(&j->threads[i], 0, job_system_thread_func, m);
		
		char temp[10];
		sprintf("BITCH%zu", i);
		j->queue[i].semaphore = sem_open(strdup(temp), 0, 0, max_threads);
	}
	
	pthread_mutex_init(&m->mutex, NULL);
#endif
	
	return m;
}

TB_API bool tb_module_compile_func(TB_Module* m, TB_Function* f) {
	if (!tb_validate(f)) abort();
	
	int id = get_local_thread_id();
	TB_FifoQueue* restrict q = &m->jobs->queue[id];
	
	uint32_t write_ptr = q->write_pointer;
	uint32_t new_write_ptr = (write_ptr + 1) % MAX_JOBS_PER_JOB_SYSTEM;
	while (new_write_ptr == q->read_pointer) {
#if _WIN32
		SwitchToThread();
#else
		sleep(0);
#endif
	}
	
	q->functions[write_ptr] = f;
	q->write_pointer = new_write_ptr;
	
	m->compiled_functions.count++;
	
#if _WIN32
	ReleaseSemaphore(q->semaphore, 1, 0);
#else
	sem_post(q->semaphore);
#endif
	
	return true;
}

TB_API bool tb_module_compile(TB_Module* m) {
	TB_JobSystem* s = m->jobs;
	s->running = false;
	
#if _WIN32
	loop(i, s->thread_count) {
		ReleaseSemaphore(s->queue[i].semaphore, 1, 0);
	}
	
	WaitForMultipleObjects(s->thread_count, s->threads, TRUE, -1);
	
	loop(i, s->thread_count) {
		CloseHandle(s->threads[i]);
		CloseHandle(s->queue[i].semaphore);
		
		s->threads[i] = NULL;
	}
	
	DeleteCriticalSection(&m->mutex);
	assert(m->compiled_functions.count == m->functions.count);
#else
	// TODO(NeGate): Delete the mutexes and fix this
	// sem_close(s->semaphore); 
#error "Fix this!"
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
	}
    
	loop(i, m->code_region_count) {
		tb_platform_vfree(m->code_regions[i].data, CODE_OUTPUT_BUFFER);
	}
	
	if (m->jit_region) {
		tb_platform_vfree(m->jit_region, m->jit_region_size);
		m->jit_region = NULL;
	}
	
	loop_range(i, 1, m->files.count) {
		tb_platform_heap_free(m->files.data[i].path);
	}
	
	loop(i, m->max_threads) arrfree(m->call_patches[i]);
	loop(i, m->max_threads) arrfree(m->ecall_patches[i]);
	
	tb_platform_string_free();
	tb_platform_heap_free(m->jobs);
#if !TB_STRIP_LABELS
	tb_platform_heap_free(m->label_symbols.data);
#endif
	tb_platform_heap_free(m->const32_patches.data);
	tb_platform_heap_free(m->files.data);
	tb_platform_heap_free(m->functions.data);
	tb_platform_heap_free(m->externals.data);
	tb_platform_heap_free(m->compiled_functions.data);
	tb_platform_heap_free(m);
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
    
	char* str = tb_platform_string_alloc(path);
	
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
	
	f->nodes.type = tb_platform_heap_realloc(f->nodes.type, cap * sizeof(TB_RegType));
	f->nodes.dt = tb_platform_heap_realloc(f->nodes.dt, cap * sizeof(TB_DataType));
	f->nodes.payload = tb_platform_heap_realloc(f->nodes.payload, cap * sizeof(TB_RegPayload));
	
	// zero out the extra space
	memset(&f->nodes.type[f->nodes.count], 0, (cap - f->nodes.count) * sizeof(TB_RegType));
}

TB_API TB_FunctionPrototype* tb_prototype_create(TB_Module* m, TB_CallingConv conv, TB_DataType return_dt, int num_params, bool has_varargs) {
	size_t len = arrlen(m->prototypes);
	size_t space_needed = (sizeof(TB_FunctionPrototype) + (sizeof(uint64_t)-1)) / sizeof(uint64_t);
	space_needed += ((num_params * sizeof(TB_DataType)) + (sizeof(uint64_t)-1)) / sizeof(uint64_t);
	
	arrsetlen(m->prototypes, len + space_needed);
	
	TB_FunctionPrototype* p = (TB_FunctionPrototype*)&m->prototypes[len];
	p->call_conv = conv;
	p->param_capacity = num_params;
	p->param_count = 0;
	p->return_dt = return_dt;
	p->has_varargs = has_varargs;
	return p;
}

TB_API void tb_prototype_add_param(TB_FunctionPrototype* p, TB_DataType dt) {
	assert(p->param_count+1 <= p->param_capacity);
	p->params[p->param_count++] = dt;
}

TB_API void tb_prototype_add_params(TB_FunctionPrototype* p, size_t count, const TB_DataType* dt) {
	assert(p->param_count+count <= p->param_capacity);
	memcpy(&p->params[p->param_count], dt, count * sizeof(TB_DataType));
	p->param_count += count;
}

TB_API TB_Function* tb_prototype_build(TB_Module* m, TB_FunctionPrototype* p, const char* name) {
	size_t i = m->functions.count++;
	assert(i < TB_MAX_FUNCTIONS);
	assert(p->param_count == p->param_capacity);
	
	TB_Function* f = &m->functions.data[i];
	*f = (TB_Function){
		.module = m,
		.prototype = p,
		.name = tb_platform_string_alloc(name)
	};
	
	// TODO(NeGate): The node stream can never be under 16 entries
	// for the SIMD optimizations to work so we bias the initial size.
	tb_resize_node_stream(f, tb_next_pow2(16 + 2 + p->param_count));
	
	f->parameter_count = 0;
	
	// Null slot, Entry label & Parameters
	f->nodes.count = 2 + p->param_count;
	f->nodes.type[0] = TB_NULL;
	f->nodes.type[1] = TB_LABEL;
	loop(i, p->param_count) f->nodes.type[2+i] = TB_PARAM;
	
	f->nodes.dt[0] = (TB_DataType){ 0 };
	f->nodes.dt[1] = TB_TYPE_PTR;
	loop(i, p->param_count) f->nodes.dt[2+i] = p->params[i];
	
	f->nodes.payload[0] = (TB_RegPayload){ 0 };
	f->nodes.payload[1] = (TB_RegPayload){ 
		.label.id = 0,
		.label.terminator = TB_NULL_REG,
		.label.is_loop = false
	};
	loop(i, p->param_count) {
		TB_DataType dt = p->params[i];
		
		// TODO(NeGate): It's currently assuming that all pointers are 8bytes big,
		// which is untrue for some platforms.
		int size = 0;
		switch (dt.type) {
			case TB_BOOL:size = 1; break;
			case TB_I8:  size = 1; break;
			case TB_I16: size = 2; break;
			case TB_I32: size = 4; break;
			case TB_I64: size = 8; break;
			case TB_F32: size = 4; break;
			case TB_F64: size = 8; break;
			case TB_PTR: size = 8; break;
			default: break;
		}
		
		assert(size);
		assert(dt.count > 0);
		f->nodes.payload[2+i] = (TB_RegPayload){
			.param.id = i,
			.param.size = size * dt.count
		};
	}
	
	f->label_count = 1;
	f->current_label = 1;
	return f;
}

TB_API TB_ExternalID tb_module_extern(TB_Module* m, const char* name) {
#if _WIN32
	EnterCriticalSection(&m->mutex);
#else
	pthread_mutex_lock(&m->mutex);
#endif

	if (m->externals.count + 1 >= m->externals.capacity) {
		// TODO(NeGate): This might be excessive for this array, idk :P
		m->externals.capacity = tb_next_pow2(m->externals.count + 1);
		m->externals.data = tb_platform_heap_realloc(m->externals.data, m->externals.capacity * sizeof(TB_External));
	}
	
	
	TB_ExternalID i = m->externals.count++;

#if _WIN32
	LeaveCriticalSection(&m->mutex);
#else
	pthread_mutex_unlock(&m->mutex);
#endif

	m->externals.data[i].name = tb_platform_string_alloc(name);
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
		tb_thread_storage = tb_platform_valloc(TB_TEMPORARY_STORAGE_SIZE);
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

void tb_emit_call_patch(TB_Module* m, uint32_t func_id, uint32_t target_id, size_t pos, size_t local_thread_id) {
	assert(pos < UINT32_MAX);
	dyn_array(TB_FunctionPatch) patches = m->call_patches[local_thread_id];
	
	TB_FunctionPatch p = {
		.func_id = func_id,
		.target_id = target_id,
		.pos = pos
	};
	
	arrput(patches, p);
}

void tb_emit_ecall_patch(TB_Module* m, uint32_t func_id, TB_ExternalID target_id, size_t pos, size_t local_thread_id) {
	dyn_array(TB_ExternFunctionPatch) patches = m->ecall_patches[local_thread_id];
	TB_ExternFunctionPatch p = {
		.func_id = func_id,
		.target_id = target_id,
		.pos = pos
	};
	
	arrput(patches, p);
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
			case TB_FUNC_ADDRESS:
			fprintf(out, "  r%u\t=\t &%s\n", i, p.func_addr->name);
			break;
			case TB_EFUNC_ADDRESS:
			fprintf(out, "  r%u\t=\t &%s\n", i, m->externals.data[p.efunc_addr].name);
			break;
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
        
		o->data = tb_platform_heap_realloc(o->data, o->capacity);
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

