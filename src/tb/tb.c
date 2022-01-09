#include "tb_internal.h"

// Used for some optimizations
#if TB_HOST_ARCH == TB_HOST_X86_64
#include <x86intrin.h>
#endif

static _Thread_local uint8_t tb_thread_storage[TB_TEMPORARY_STORAGE_SIZE];

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

static TB_CodeRegion* get_or_allocate_code_region(TB_Module* m, int tid) {
	if (!m->code_regions[tid]) {
		m->code_regions[tid] = tb_platform_valloc(CODE_REGION_BUFFER_SIZE);
	}
	
	return m->code_regions[tid];
}

TB_API TB_Function* tb_function_clone(TB_Module* m, TB_Function* source_func, const char* name) {
	size_t i = m->functions.count++;
	assert(i < TB_MAX_FUNCTIONS);
	
	TB_Function* f = &m->functions.data[i];
	*f = (TB_Function){
		.module = m,
		.prototype = f->prototype,
		.name = tb_platform_string_alloc(name),
		.nodes.count = source_func->nodes.count,
		.label_count = source_func->label_count,
		.current_label = source_func->current_label
	};
	
	// TODO(NeGate): The node stream can never be under 16 entries
	// for the SIMD optimizations to work so we bias the initial size.
	tb_resize_node_stream(f, f->nodes.count < 16 ? 16 : tb_next_pow2(f->nodes.count));
	
	memcpy(f->nodes.type, source_func->nodes.type, f->nodes.count * sizeof(uint8_t));
	memcpy(f->nodes.dt, source_func->nodes.dt, f->nodes.count * sizeof(TB_DataType));
	memcpy(f->nodes.payload, source_func->nodes.payload, f->nodes.count * sizeof(TB_RegPayload));
	return f;
}

// TODO(NeGate): Doesn't free the name, it's kept forever
TB_API void tb_function_free(TB_Function* f) {
	if (f->nodes.type == NULL) return;
	
	tb_platform_heap_free(f->nodes.type);
	tb_platform_heap_free(f->nodes.dt);
	tb_platform_heap_free(f->nodes.payload);
	tb_platform_heap_free(f->vla.data);
	
	f->nodes.type = NULL;
	f->nodes.dt = NULL;
	f->nodes.payload = NULL;
	f->vla.data = NULL;
}

TB_API TB_DataType tb_vector_type(TB_DataTypeEnum type, int width) {
	assert(!tb_is_power_of_two(width));
	
	return (TB_DataType) { .type = type, .width = __builtin_ffs(width) - 1 };
}

TB_API TB_Module* tb_module_create(TB_Arch target_arch,
                                   TB_System target_system,
                                   const TB_FeatureSet* features) {
	TB_Module* m = tb_platform_heap_alloc(sizeof(TB_Module));
    memset(m, 0, sizeof(TB_Module));
	
	m->max_threads = TB_MAX_THREADS;
	m->target_arch = target_arch;
	m->target_system = target_system;
	m->features = *features;
	
	m->prototypes_arena = tb_platform_valloc(PROTOTYPES_ARENA_SIZE * sizeof(uint64_t));
	
#if !TB_STRIP_LABELS
	m->label_symbols.count = 0;
	m->label_symbols.capacity = 64;
	m->label_symbols.data = tb_platform_heap_alloc(64 * sizeof(TB_LabelSymbol));
#endif
	
	m->files.count = 1;
	m->files.capacity = 64;
	m->files.data = tb_platform_heap_alloc(64 * sizeof(TB_File));
    m->files.data[0] = (TB_File){ 0 };
	
	m->functions.count = 0;
	m->functions.data = tb_platform_heap_alloc(TB_MAX_FUNCTIONS * sizeof(TB_Function));
    
	m->compiled_functions.count = 0;
	m->compiled_functions.data = tb_platform_heap_alloc(TB_MAX_FUNCTIONS * sizeof(TB_FunctionOutput));
    
	m->line_info_count = 0;
	
#if defined(_WIN32)
	InitializeCriticalSection(&m->mutex);
#elif defined(__unix__)
	pthread_mutex_init(&m->mutex, NULL);
#else
#error "TODO"
#endif
	
	tb_platform_arena_init();
	return m;
}

#define OPT(x) if (tb_opt_ ## x (f)) { \
/* printf("%s   ", #x); */ \
/* tb_function_print(f, stdout); */ \
goto repeat_opt; \
}

TB_API void tb_function_optimize(TB_Function* f, TB_OptLevel opt) {
	printf("INITIAL   ");
	tb_function_print(f, stdout);
	printf("\n\n\n");
	
	repeat_opt: {
		OPT(dead_expr_elim);
		OPT(remove_pass_node);
		OPT(canonicalize);
		OPT(fold);
		OPT(load_elim);
		OPT(strength_reduction);
		OPT(hoist_locals);
		OPT(mem2reg);
		OPT(dead_block_elim);
		OPT(deshort_circuit);
		OPT(copy_elision);
		OPT(compact_dead_regs);
	}
	
	printf("FINAL   ");
	tb_function_print(f, stdout);
	printf("\n\n\n");
}
#undef OPT

TB_API bool tb_module_compile_func(TB_Module* m, TB_Function* f) {
	// TODO(NeGate): Properly detect this
	ICodeGen* gen = &x64_fast_code_gen;
	
	// Machine code gen
	int id = get_local_thread_id();
	assert(id < TB_MAX_THREADS);
	
	int index = m->compiled_functions.count++;
	
	TB_CodeRegion* restrict region = get_or_allocate_code_region(m, id);
	TB_FunctionOutput* func_out = m->compiled_functions.data;
	
	func_out[index] = gen->compile_function(f, &m->features, &region->data[region->size], id);
	region->size += func_out[index].code_size;
	
	if (region->size > CODE_REGION_BUFFER_SIZE) {
		printf("Code region buffer: out of memory!\n");
		abort();
	}
	
	return true;
}

TB_API bool tb_module_compile(TB_Module* m) {
	m->max_threads = total_tid;
	
#if _WIN32
	DeleteCriticalSection(&m->mutex);
#else
	pthread_mutex_destroy(&m->mutex);
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
	tb_platform_arena_free();
	tb_platform_string_free();
	
	loop(i, m->functions.count) {
		tb_function_free(&m->functions.data[i]);
	}
    
	loop(i, m->max_threads) if (m->code_regions[i]) {
		tb_platform_vfree(m->code_regions[i], CODE_REGION_BUFFER_SIZE);
		m->code_regions[i] = NULL;
	}
	
	if (m->jit_region) {
		tb_platform_vfree(m->jit_region, m->jit_region_size);
		m->jit_region = NULL;
	}
	
	loop_range(i, 1, m->files.count) {
		tb_platform_heap_free(m->files.data[i].path);
	}
	
	loop(i, m->max_threads) arrfree(m->initializers[i]);
	loop(i, m->max_threads) arrfree(m->call_patches[i]);
	loop(i, m->max_threads) arrfree(m->ecall_patches[i]);
	loop(i, m->max_threads) arrfree(m->const_patches[i]);
	loop(i, m->max_threads) arrfree(m->externals[i]);
	
	tb_platform_vfree(m->prototypes_arena, PROTOTYPES_ARENA_SIZE * sizeof(uint64_t));
	
#if !TB_STRIP_LABELS
	tb_platform_heap_free(m->label_symbols.data);
#endif
	tb_platform_heap_free(m->files.data);
	tb_platform_heap_free(m->functions.data);
	tb_platform_heap_free(m->compiled_functions.data);
	tb_platform_heap_free(m);
}

TB_API TB_FileID tb_file_create(TB_Module* m, const char* path) {
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

TB_API bool tb_module_export(TB_Module* m, const char* path) {
	const ICodeGen* restrict code_gen = NULL;
	switch (m->target_arch) {
		case TB_ARCH_X86_64: code_gen = &x64_fast_code_gen; break;
		default: tb_todo();
	}
	
	switch (m->target_system) {
        case TB_SYSTEM_WINDOWS:
		tb_export_coff(m, code_gen, path);
		break;
        case TB_SYSTEM_LINUX:
		tb_export_elf64(m, code_gen, path);
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
	size_t space_needed = (sizeof(TB_FunctionPrototype) + (sizeof(uint64_t)-1)) / sizeof(uint64_t);
	space_needed += ((num_params * sizeof(TB_DataType)) + (sizeof(uint64_t)-1)) / sizeof(uint64_t);
	
	size_t len = atomic_fetch_add(&m->prototypes_arena_size, space_needed);
	if (len+space_needed >= PROTOTYPES_ARENA_SIZE) {
		printf("Prototype arena: out of memory!\n");
		abort();
	}
	
	TB_FunctionPrototype* p = (TB_FunctionPrototype*)&m->prototypes_arena[len];
	p->call_conv = conv;
	p->param_capacity = safe_cast(short, num_params);
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

TB_API TB_Function* tb_prototype_build(TB_Module* m, TB_FunctionPrototype* p, const char* name, TB_Linkage linkage) {
	size_t i = m->functions.count++;
	assert(i < TB_MAX_FUNCTIONS);
	assert(p->param_count == p->param_capacity);
	
	TB_Function* f = &m->functions.data[i];
	*f = (TB_Function){
		.module = m,
		.linkage = linkage,
		.prototype = p,
		.name = tb_platform_string_alloc(name)
	};
	
	// TODO(NeGate): The node stream can never be under 16 entries
	// for the SIMD optimizations to work so we bias the initial size.
	tb_resize_node_stream(f, tb_next_pow2(16 + 2 + p->param_count));
	
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
		assert(dt.width < 8 && "Vector width too big!");
		f->nodes.payload[2+i] = (TB_RegPayload){
			.param.id = i,
			.param.size = size << dt.width
		};
	}
	
	f->label_count = 1;
	f->current_label = 1;
	return f;
}

TB_API TB_InitializerID tb_initializer_create(TB_Module* m, size_t size, size_t align, size_t max_objects) {
	int tid = get_local_thread_id();
	
	size_t space_needed = (sizeof(TB_Initializer) + (sizeof(uint64_t)-1)) / sizeof(uint64_t);
	space_needed += ((max_objects * sizeof(TB_InitObj)) + (sizeof(uint64_t)-1)) / sizeof(uint64_t);
	
	// each thread gets their own grouping of ids
	size_t len = arrlen(m->initializers[tid]);
	
	TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
	TB_InitializerID i = (tid * per_thread_stride) + len;
	arrsetlen(m->initializers[tid], len + space_needed);
	
	TB_Initializer* initializer = (TB_Initializer*)&m->initializers[tid][len];
	initializer->size = safe_cast(uint32_t, size);
	initializer->align = safe_cast(uint32_t, align);
	initializer->obj_capacity = safe_cast(uint32_t, max_objects);
	initializer->obj_count = 0;
	return i;
}

TB_API void* tb_initializer_add_region(TB_Module* m, TB_InitializerID id, size_t offset, size_t size) {
	TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
	TB_Initializer* i = (TB_Initializer*)&m->initializers[id / per_thread_stride][id % per_thread_stride];
	
	assert(i->obj_count+1 <= i->obj_capacity);
	
	// TODO(NeGate): Remove the malloc here
	void* ptr = malloc(size);
	i->objects[i->obj_count++] = (TB_InitObj) {
		.type = TB_INIT_OBJ_REGION,
		.offset = safe_cast(uint32_t, offset),
		.region.size = safe_cast(uint32_t, size),
		.region.ptr = ptr
	};
	
	return ptr;
}

TB_API TB_GlobalID tb_global_create(TB_Module* m, TB_InitializerID initializer, const char* name, TB_Linkage linkage) {
	int tid = get_local_thread_id();
	
	// each thread gets their own grouping of global ids
	TB_GlobalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
	
	// layout
	size_t pos;
	{
		TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
		TB_Initializer* i = (TB_Initializer*)&m->initializers[initializer / per_thread_stride][initializer % per_thread_stride];
		
		pos = atomic_fetch_add(&m->data_region_size, i->size + i->align);
		
		// TODO(NeGate): Assert on non power of two alignment
		size_t align_mask = i->align - 1;
		pos = (pos + align_mask) & ~align_mask;
		
		assert(pos < UINT32_MAX && "Cannot fit global into space");
		assert((pos + i->size) < UINT32_MAX && "Cannot fit global into space");
	}
	
	TB_GlobalID i = (tid * per_thread_stride) + arrlen(m->globals[tid]);
	TB_Global g = {
		.name = tb_platform_string_alloc(name),
		.linkage = linkage,
		.init = initializer,
		.pos = pos
	};
	arrput(m->globals[tid], g);
	
	return i;
}

TB_API bool tb_jit_import(TB_Module* m, const char* name, void* address) {
	// TODO(NeGate): Maybe speed this up but also maybe don't... idk
	loop(i, m->max_threads) {
		loop(j, arrlen(m->externals[i])) if (strcmp(m->externals[i][j].name, name) == 0) {
			m->externals[i][j].address = address;
			return true;
		}
	}
	
	return false;
}

TB_API TB_ExternalID tb_extern_create(TB_Module* m, const char* name) {
	int tid = get_local_thread_id();
	
	// each thread gets their own grouping of external ids
	TB_ExternalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
	
	TB_ExternalID i = (tid * per_thread_stride) + arrlen(m->externals[tid]);
	TB_External e = {
		.name = tb_platform_string_alloc(name)
	};
	arrput(m->externals[tid], e);
	
	return i;
}

TB_API void* tb_module_get_jit_func_by_name(TB_Module* m, const char* name) {
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		if (strcmp(m->compiled_functions.data[i].name, name) == 0) return m->compiled_function_pos[i];
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

TB_API TB_Label tb_inst_get_current_label(TB_Function* f) {
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
	TB_TemporaryStorage* store = (TB_TemporaryStorage*)tb_thread_storage;
	store->used = 0;
	return store;
}

void* tb_tls_try_push(TB_TemporaryStorage* store, size_t size) {
	if (sizeof(TB_TemporaryStorage) + store->used + size >= TB_TEMPORARY_STORAGE_SIZE) {
		return NULL;
	}
    
	void* ptr = &store->data[store->used];
	store->used += size;
	return ptr;
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
	TB_FunctionPatch p = {
		.func_id = func_id,
		.target_id = target_id,
		.pos = safe_cast(uint32_t, pos)
	};
	
	arrput(m->call_patches[local_thread_id], p);
}

void tb_emit_ecall_patch(TB_Module* m, uint32_t func_id, TB_ExternalID target_id, size_t pos, size_t local_thread_id) {
	TB_ExternFunctionPatch p = {
		.func_id = func_id,
		.target_id = target_id,
		.pos = safe_cast(uint32_t, pos)
	};
	
	arrput(m->ecall_patches[local_thread_id], p);
}

uint32_t tb_emit_const_patch(TB_Module* m, uint32_t func_id, size_t pos, const void* ptr, size_t len, size_t local_thread_id) {
	size_t rdata_pos = atomic_fetch_add(&m->rdata_region_size, len);
	TB_ConstPoolPatch p = {
		.func_id = func_id,
		.pos = safe_cast(uint32_t, pos),
		.rdata_pos = rdata_pos,
		.data = ptr,
		.length = len
	};
	arrput(m->const_patches[local_thread_id], p);
	return safe_cast(uint32_t, rdata_pos);
}

void tb_emit_global_patch(TB_Module* m, uint32_t func_id, size_t pos, TB_GlobalID global, size_t local_thread_id) {
	TB_GlobalPatch p = {
		.func_id = func_id,
		.pos = safe_cast(uint32_t, pos),
		.global = global
	};
	arrput(m->global_patches[local_thread_id], p);
}

//
// IR PRINTER
//
static void tb_print_type(FILE* out, TB_DataType dt) {
	assert(dt.width < 8 && "Vector width too big!");
	
	switch (dt.type) {
        case TB_VOID:   printf("[void]   \t"); break;
        case TB_BOOL:   printf("[bool]    \t"); break;
        case TB_I8:     printf("[i8  x %02d]\t", 1 << dt.width); break;
        case TB_I16:    printf("[i16 x %02d]\t", 1 << dt.width); break;
        case TB_I32:    printf("[i32 x %02d]\t", 1 << dt.width); break;
        case TB_I64:    printf("[i64 x %02d]\t", 1 << dt.width); break;
        case TB_PTR:    printf("[ptr]    \t"); break;
        case TB_F32:    printf("[f32 x %02d]\t", 1 << dt.width); break;
        case TB_F64:    printf("[f64 x %02d]\t", 1 << dt.width); break;
        default:        tb_todo();
	}
}

TB_API void tb_function_print(TB_Function* f, FILE* out) {
    TB_Module* m = f->module;
	fprintf(out, "%s():\n", f->name);
    
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		TB_RegTypeEnum type = f->nodes.type[i];
		TB_DataType dt = f->nodes.dt[i];
		TB_RegPayload p = f->nodes.payload[i];
        
		switch (type) {
            case TB_NULL:
			fprintf(out, "  r%u\t=\t", i);
			fprintf(out, " NOP\n");
			break;
            case TB_SIGNED_CONST:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " %" PRIi64 "\n", p.s_const);
			break;
            case TB_UNSIGNED_CONST:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " %" PRIu64 "\n", p.u_const);
			break;
			case TB_STRING_CONST:
			fprintf(out, "  r%u\t=\t\"%.*s\"\n", i, (int)f->nodes.payload[i].str_const.len, f->nodes.payload[i].str_const.data);
			break;
			case TB_LINE_INFO:
			//fprintf(out, "  # LOC %s:%d\n", f->module->files.data[p.line_info.file].path, p.line_info.line);
			break;
            case TB_FLOAT_CONST:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
            fprintf(out, " %f\n", p.f_const);
			break;
            case TB_FLOAT_EXT:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " FPXT r%u\n", p.ext);
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
            case TB_INT2PTR:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " INT2PTR r%u\n", p.ptrcast);
			break;
            case TB_PTR2INT:
			fprintf(out, "  r%u\t=\t", i);
			tb_print_type(out, dt);
			fprintf(out, " PTR2INT r%u\n", p.ptrcast);
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
			case TB_INITIALIZE:
			// TODO(NeGate): Make this more presentable
			fprintf(out, "  r%u = INITIALIZER ...\n", i);
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
			case TB_ATOMIC_XCHG:
			fprintf(out, "  r%u\t=\tATOMIC XCHG *r%u, r%u\n", i, p.atomic.addr, p.atomic.src);
			break;
			case TB_ATOMIC_ADD:
			fprintf(out, "  r%u\t=\tATOMIC ADD *r%u, r%u\n", i, p.atomic.addr, p.atomic.src);
			break;
			case TB_ATOMIC_SUB:
			fprintf(out, "  r%u\t=\tATOMIC SUB *r%u, r%u\n", i, p.atomic.addr, p.atomic.src);
			break;
			case TB_ATOMIC_AND:
			fprintf(out, "  r%u\t=\tATOMIC AND *r%u, r%u\n", i, p.atomic.addr, p.atomic.src);
			break;
			case TB_ATOMIC_XOR:
			fprintf(out, "  r%u\t=\tATOMIC XOR *r%u, r%u\n", i, p.atomic.addr, p.atomic.src);
			break;
			case TB_ATOMIC_OR: 
			fprintf(out, "  r%u\t=\tATOMIC OR *r%u, r%u\n", i, p.atomic.addr, p.atomic.src);
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
			case TB_ECALL: {
				TB_ExternalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_External* e = &m->externals[p.ecall.target / per_thread_stride][p.ecall.target % per_thread_stride];
				
				fprintf(out, "  r%u\t=\tECALL %s(", i, e->name);
				for (size_t j = p.ecall.param_start; j < p.ecall.param_end; j++) {
					if (j != p.ecall.param_start) fprintf(out, ", ");
					
					fprintf(out, "r%u", f->vla.data[j]);
				}
				fprintf(out, ")\n");
				break;
			}
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
			case TB_EFUNC_ADDRESS: {
				TB_ExternalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_External* e = &m->externals[p.efunc_addr / per_thread_stride][p.efunc_addr % per_thread_stride];
				
				fprintf(out, "  r%u\t=\t &%s\n", i, e->name);
				break;
			}
			case TB_GLOBAL_ADDRESS: {
				TB_GlobalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_Global* g = &m->globals[p.global_addr / per_thread_stride][p.global_addr % per_thread_stride];
				
				fprintf(out, "  r%u\t=\t &%s\n", i, g->name);
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
			if (p.label.id > 0) fprintf(out, "\n"); 
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
	
	printf("\n\n\n");
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

