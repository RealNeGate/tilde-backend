#include "tb_internal.h"
#include <stdarg.h>

// Used for some optimizations
#if TB_HOST_ARCH == TB_HOST_X86_64
#include <emmintrin.h>
#endif

static thread_local uint8_t tb_thread_storage[TB_TEMPORARY_STORAGE_SIZE];
static thread_local int tid;

static tb_atomic_int total_tid;

static int get_local_thread_id() {
	// the value it spits out is zero-based, but
	// the TIDs consider zero as a NULL space.
	if (tid == 0) {
		int new_id = tb_atomic_int_add(&total_tid, 1);
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
	size_t i = tb_atomic_size_add(&m->functions.count, 1);
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
	
	f->nodes.capacity = f->nodes.count < 16 ? 16 : tb_next_pow2(f->nodes.count);
	f->nodes.data = malloc(f->nodes.capacity * sizeof(TB_Node));
	
	memcpy(f->nodes.data, source_func->nodes.data, f->nodes.count * sizeof(uint8_t));
	return f;
}

// TODO(NeGate): Doesn't free the name, it's kept forever
TB_API void tb_function_free(TB_Function* f) {
	if (f->nodes.data == NULL) return;
	
	tb_platform_heap_free(f->nodes.data);
	tb_platform_heap_free(f->vla.data);
	
	f->nodes.data = NULL;
	f->vla.data = NULL;
}

TB_API TB_DataType tb_vector_type(TB_DataTypeEnum type, int width) {
	assert(tb_is_power_of_two(width));
	
	return (TB_DataType) { .type = type, .width = tb_ffs(width) - 1 };
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
	
	m->files.count = 1;
	m->files.capacity = 64;
	m->files.data = tb_platform_heap_alloc(64 * sizeof(TB_File));
    m->files.data[0] = (TB_File){ 0 };
	
	m->functions.count = 0;
	m->functions.data = tb_platform_heap_alloc(TB_MAX_FUNCTIONS * sizeof(TB_Function));
    
	m->compiled_functions.count = 0;
	m->compiled_functions.data = tb_platform_heap_alloc(TB_MAX_FUNCTIONS * sizeof(TB_FunctionOutput));
    
	// we start a little off the start just because
	m->rdata_region_size = 16;
	
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
/* tb_function_print(f, tb_default_print_callback, stdout); */ \
/* printf("\n\n\n"); */ \
goto repeat_opt; \
}

TB_API void tb_function_optimize(TB_Function* f, TB_OptLevel opt) {
	//printf("INITIAL   ");
	//tb_function_print(f, tb_default_print_callback, stdout);
	//printf("\n\n\n");
	
	// only needs to run once
	tb_opt_hoist_locals(f);
	
	repeat_opt: {
		// Passive optimizations
		OPT(dead_expr_elim);
		OPT(remove_pass_node);
		OPT(fold);
		OPT(load_elim);
		OPT(strength_reduction);
		OPT(copy_elision);
		OPT(canonicalize);
		OPT(mem2reg);
		OPT(dead_block_elim);
		OPT(deshort_circuit);
		OPT(compact_dead_regs);
	}
	
	//printf("FINAL   ");
	//tb_function_print(f, tb_default_print_callback, stdout);
	//printf("\n\n\n");
}
#undef OPT

TB_API bool tb_module_compile_func(TB_Module* m, TB_Function* f) {
	// TODO(NeGate): Properly detect this
	ICodeGen* gen = &x64_fast_code_gen;
	
	// Machine code gen
	int id = get_local_thread_id();
	assert(id < TB_MAX_THREADS);
	
	// NOTE(NeGate): imma not use this counter as an index and just directly map
	// each function to their compiled variants
	tb_atomic_size_add(&m->compiled_functions.count, 1);
	
	TB_CodeRegion* restrict region = get_or_allocate_code_region(m, id);
	TB_FunctionOutput* func_out = m->compiled_functions.data;
	
	TB_FunctionID index = tb_function_get_id(m, f);
	func_out[index] = gen->compile_function(index, f, &m->features, &region->data[region->size], id);
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
	
	loop(i, m->max_threads) arrfree(m->initializers[i]);
	loop(i, m->max_threads) arrfree(m->call_patches[i]);
	loop(i, m->max_threads) arrfree(m->ecall_patches[i]);
	loop(i, m->max_threads) arrfree(m->const_patches[i]);
	loop(i, m->max_threads) arrfree(m->externals[i]);
	
	tb_platform_vfree(m->prototypes_arena, PROTOTYPES_ARENA_SIZE * sizeof(uint64_t));
	
#if !TB_STRIP_LABELS
	arrfree(m->label_symbols);
#endif
	tb_platform_heap_free(m->files.data);
	tb_platform_heap_free(m->functions.data);
	tb_platform_heap_free(m->compiled_functions.data);
	tb_platform_heap_free(m);
}

TB_API TB_FileID tb_file_create(TB_Module* m, const char* path) {
	// skip the NULL file entry
	loop_range(i, 1, m->files.count) {
		if (strcmp(m->files.data[i].path, path) == 0) {
			return i;
		}
	}
	
	if (m->files.count + 1 >= m->files.capacity) {
		m->files.capacity *= 2;
		m->files.data = realloc(m->files.data, m->files.capacity * sizeof(TB_File));
	}
    
	char* str = tb_platform_string_alloc(path);
	
	size_t r = m->files.count++;
	m->files.data[r] = (TB_File){ .path = str };
	return r;
}

TB_API bool tb_module_export(TB_Module* m, const char* path, bool emit_debug_info) {
	const ICodeGen* restrict code_gen = NULL;
	switch (m->target_arch) {
		case TB_ARCH_X86_64: code_gen = &x64_fast_code_gen; break;
		default: tb_todo();
	}
	
	switch (m->target_system) {
        case TB_SYSTEM_WINDOWS:
		tb_export_coff(m, code_gen, path, emit_debug_info);
		break;
        case TB_SYSTEM_LINUX:
		tb_export_elf64(m, code_gen, path, emit_debug_info);
		break;
        default:
		tb_panic("TinyBackend error: Unknown system!\n");
	}
	
	return true;
}

void tb_function_reserve_nodes(TB_Function* f, size_t extra) {
    if (f->nodes.count + extra >= f->nodes.capacity) {
        f->nodes.capacity = (f->nodes.count + extra) * 2;
		
        TB_Node* new_ptr = realloc(f->nodes.data, sizeof(TB_Node) * f->nodes.capacity);
        if (!new_ptr) tb_panic("Out of memory");
		f->nodes.data = new_ptr;
    }
}

TB_API TB_FunctionPrototype* tb_prototype_create(TB_Module* m, TB_CallingConv conv, TB_DataType return_dt, int num_params, bool has_varargs) {
	assert(num_params == (uint32_t) num_params);
	
	size_t space_needed = (sizeof(TB_FunctionPrototype) + (sizeof(uint64_t)-1)) / sizeof(uint64_t);
	space_needed += ((num_params * sizeof(TB_DataType)) + (sizeof(uint64_t)-1)) / sizeof(uint64_t);
	
	size_t len = tb_atomic_size_add(&m->prototypes_arena_size, space_needed);
	if (len+space_needed >= PROTOTYPES_ARENA_SIZE) {
		tb_panic("Prototype arena: out of memory!\n");
	}
	
	TB_FunctionPrototype* p = (TB_FunctionPrototype*)&m->prototypes_arena[len];
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

TB_API TB_Function* tb_prototype_build(TB_Module* m, TB_FunctionPrototype* p, const char* name, TB_Linkage linkage) {
	size_t i = tb_atomic_size_add(&m->functions.count, 1);
	assert(i < TB_MAX_FUNCTIONS);
	assert(p->param_count == p->param_capacity);
	
	TB_Function* f = &m->functions.data[i];
	*f = (TB_Function){
		.module = m,
		.linkage = linkage,
		.prototype = p,
		.name = tb_platform_string_alloc(name)
	};
	
	f->nodes.capacity = 64;
	f->nodes.count = 2 + p->param_count;
	f->nodes.data = malloc(f->nodes.capacity * sizeof(TB_Node));
	
	// Null slot, Entry label & Parameters
	f->nodes.data[0] = (TB_Node){ .next = 1 };
	f->nodes.data[1] = (TB_Node){
		.type = TB_LABEL,
		.dt = TB_TYPE_PTR,
		.label = { 0, 0 }
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
		
		f->nodes.data[1 + i].next = 2 + i;
		f->nodes.data[2 + i] = (TB_Node){
			.type = TB_PARAM,
			.dt = dt,
			.next = 2,
			.param = { .id = i, .size = size << dt.width }
		};
	}
	
	f->nodes.end = 2 + (p->param_count - 1);
	f->label_count = 1;
	f->current_label = 1;
	return f;
}

TB_API TB_InitializerID tb_initializer_create(TB_Module* m, size_t size, size_t align, size_t max_objects) {
	tb_assume(size == (uint32_t)size);
	tb_assume(align == (uint32_t)align);
	tb_assume(max_objects == (uint32_t)max_objects);
	int tid = get_local_thread_id();
	
	size_t space_needed = (sizeof(TB_Initializer) + (sizeof(uint64_t)-1)) / sizeof(uint64_t);
	space_needed += ((max_objects * sizeof(TB_InitObj)) + (sizeof(uint64_t)-1)) / sizeof(uint64_t);
	
	// each thread gets their own grouping of ids
	size_t len = arrlen(m->initializers[tid]);
	
	TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
	TB_InitializerID i = (tid * per_thread_stride) + len;
	arrsetlen(m->initializers[tid], len + space_needed);
	
	TB_Initializer* initializer = (TB_Initializer*)&m->initializers[tid][len];
	initializer->size = size;
	initializer->align = align;
	initializer->obj_capacity = max_objects;
	initializer->obj_count = 0;
	return i;
}

TB_API void* tb_initializer_add_region(TB_Module* m, TB_InitializerID id, size_t offset, size_t size) {
	assert(offset == (uint32_t)offset);
	assert(size == (uint32_t)size);
	
	TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
	TB_Initializer* i = (TB_Initializer*)&m->initializers[id / per_thread_stride][id % per_thread_stride];
	
	assert(i->obj_count+1 <= i->obj_capacity);
	
	// TODO(NeGate): Remove the malloc here
	void* ptr = malloc(size);
	i->objects[i->obj_count++] = (TB_InitObj) {
		.type = TB_INIT_OBJ_REGION,
		.offset = offset,
		.region = {
			.size = size,
			.ptr = ptr
		}
	};
	
	return ptr;
}

TB_API void tb_initializer_add_global(TB_Module* m, TB_InitializerID id, size_t offset, TB_GlobalID global) {
	assert(offset == (uint32_t)offset);
	
	TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
	TB_Initializer* i = (TB_Initializer*)&m->initializers[id / per_thread_stride][id % per_thread_stride];
	
	assert(i->obj_count+1 <= i->obj_capacity);
	i->objects[i->obj_count++] = (TB_InitObj) {
		.type = TB_INIT_OBJ_RELOC_GLOBAL,
		.offset = offset,
		.reloc_global = global
	};
}

TB_API void tb_initializer_add_function(TB_Module* m, TB_InitializerID id, size_t offset, TB_FunctionID func) {
	assert(offset == (uint32_t)offset);
	
	TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
	TB_Initializer* i = (TB_Initializer*)&m->initializers[id / per_thread_stride][id % per_thread_stride];
	
	assert(i->obj_count+1 <= i->obj_capacity);
	i->objects[i->obj_count++] = (TB_InitObj) {
		.type = TB_INIT_OBJ_RELOC_FUNCTION,
		.offset = offset,
		.reloc_function = func
	};
}

TB_API void tb_initializer_add_extern(TB_Module* m, TB_InitializerID id, size_t offset, TB_ExternalID external) {
	assert(offset == (uint32_t)offset);
	
	TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
	TB_Initializer* i = (TB_Initializer*)&m->initializers[id / per_thread_stride][id % per_thread_stride];
	
	assert(i->obj_count+1 <= i->obj_capacity);
	i->objects[i->obj_count++] = (TB_InitObj) {
		.type = TB_INIT_OBJ_RELOC_EXTERN,
		.offset = offset,
		.reloc_extern = external
	};
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
		
		pos = tb_atomic_size_add(&m->data_region_size, i->size + i->align);
		
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
	
	return f->nodes.data[f->current_label].label.id;
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

bool tb_tls_can_fit(TB_TemporaryStorage* store, size_t size) {
	return (sizeof(TB_TemporaryStorage) + store->used + size < TB_TEMPORARY_STORAGE_SIZE);
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

void tb_tls_restore(TB_TemporaryStorage* store, void* ptr) {
	size_t i = ((uint8_t*)ptr) - store->data;
	assert(i <= store->used);
	
	store->used = i;
}

void tb_emit_call_patch(TB_Module* m, TB_CompiledFunctionID source, uint32_t target_id, size_t pos, size_t local_thread_id) {
	assert(pos == (uint32_t) pos);
	
	TB_FunctionPatch p = {
		.source = source,
		.target_id = target_id,
		.pos = pos
	};
	
	arrput(m->call_patches[local_thread_id], p);
}

void tb_emit_ecall_patch(TB_Module* m, TB_CompiledFunctionID source, TB_ExternalID target_id, size_t pos, size_t local_thread_id) {
	assert(pos == (uint32_t) pos);
	
	TB_ExternFunctionPatch p = {
		.source = source,
		.target_id = target_id,
		.pos = pos
	};
	
	arrput(m->ecall_patches[local_thread_id], p);
}

uint32_t tb_emit_const_patch(TB_Module* m, TB_CompiledFunctionID source, size_t pos, const void* ptr, size_t len, size_t local_thread_id) {
	assert(pos == (uint32_t) pos);
	assert(len == (uint32_t) len);
	
	size_t align = len > 8 ? 16 : 0;
	size_t alloc_pos = tb_atomic_size_add(&m->rdata_region_size, len + align);
	
	size_t rdata_pos = len > 8 ? align_up(alloc_pos, 16) : alloc_pos;
	TB_ConstPoolPatch p = {
		.source = source,
		.pos = pos,
		.rdata_pos = rdata_pos,
		.data = ptr,
		.length = len
	};
	arrput(m->const_patches[local_thread_id], p);
	
	assert(rdata_pos == (uint32_t) rdata_pos);
	return rdata_pos;
}

void tb_emit_global_patch(TB_Module* m, TB_CompiledFunctionID source, size_t pos, TB_GlobalID global, size_t local_thread_id) {
	assert(pos == (uint32_t) pos);
	
	TB_GlobalPatch p = {
		.source = source,
		.pos = pos,
		.global = global
	};
	arrput(m->global_patches[local_thread_id], p);
}

//
// IR PRINTER
//
TB_API void tb_default_print_callback(void* user_data, const char* fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	vfprintf((FILE*)user_data, fmt, ap);
	va_end(ap);
}

static void tb_print_type(TB_DataType dt, TB_PrintCallback callback, void* user_data) {
	assert(dt.width < 8 && "Vector width too big!");
	
	switch (dt.type) {
        case TB_VOID:   callback(user_data, "[void]     \t"); break;
        case TB_BOOL:   callback(user_data, "[bool]     \t"); break;
        case TB_I8:     callback(user_data, "[i8  x %3d]\t", 1 << dt.width); break;
        case TB_I16:    callback(user_data, "[i16 x %3d]\t", 1 << dt.width); break;
        case TB_I32:    callback(user_data, "[i32 x %3d]\t", 1 << dt.width); break;
        case TB_I64:    callback(user_data, "[i64 x %3d]\t", 1 << dt.width); break;
        case TB_PTR:    callback(user_data, "[ptr]      \t"); break;
        case TB_F32:    callback(user_data, "[f32 x %3d]\t", 1 << dt.width); break;
        case TB_F64:    callback(user_data, "[f64 x %3d]\t", 1 << dt.width); break;
        default:        tb_todo();
	}
}

TB_API void tb_function_print(TB_Function* f, TB_PrintCallback callback, void* user_data) {
    TB_Module* m = f->module;
	callback(user_data, "%s():\n", f->name);
    
	TB_FOR_EACH_NODE(n, f) {
		TB_Reg i = n - f->nodes.data;
        TB_NodeTypeEnum type = n->type;
		TB_DataType dt = n->dt;
		
		switch (type) {
            case TB_NULL:
			callback(user_data, "  r%u\t=\t", i);
			callback(user_data, " NOP\n");
			break;
            case TB_DEBUGBREAK:
			callback(user_data, " DEBUGBREAK\n");
			break;
            case TB_SIGNED_CONST:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " %" PRIi64 "\n", n->sint.value);
			break;
            case TB_UNSIGNED_CONST:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " %" PRIu64 "\n", n->uint.value);
			break;
			case TB_STRING_CONST:
			callback(user_data, "  r%u\t=\t\"%.*s\"\n", i, (int)n->string.length, n->string.data);
			break;
			case TB_LINE_INFO:
			callback(user_data, "  # LOC %s:%d\n", f->module->files.data[n->line_info.file].path, n->line_info.line);
			break;
            case TB_FLOAT_CONST:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
            callback(user_data, " %f\n", n->flt.value);
			break;
            case TB_FLOAT_EXT:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " FPXT r%u\n", n->unary.src);
			break;
            case TB_ZERO_EXT:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " ZXT r%u\n", n->unary.src);
			break;
            case TB_SIGN_EXT:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " SXT r%u\n", n->unary.src);
			break;
            case TB_INT2PTR:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " INT2PTR r%u\n", n->unary.src);
			break;
            case TB_PTR2INT:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " PTR2INT r%u\n", n->unary.src);
			break;
            case TB_FLOAT2INT:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " FLT2INT r%u\n", n->unary.src);
			break;
            case TB_INT2FLOAT:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " INT2FLT r%u\n", n->unary.src);
			break;
            case TB_TRUNCATE:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " TRUNC r%u\n", n->unary.src);
			break;
            case TB_RESTRICT:
			callback(user_data, "  r%u\t=\tRESTRICT r%u\n", i, n->unary.src);
			break;
			case TB_MEMSET:
			callback(user_data, "  MEMSET\t(r%d, r%d, r%d)\n", n->mem_op.dst, n->mem_op.src, n->mem_op.size);
			break;
			case TB_MEMCPY:
			callback(user_data, "  MEMCPY\t(r%d, r%d, r%d)\n", n->mem_op.dst, n->mem_op.src, n->mem_op.size);
			break;
			case TB_INITIALIZE:
			callback(user_data, " *r%u = INITIALIZER ...\n", n->init.addr);
			break;
			case TB_MEMBER_ACCESS:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " r%u.data[%d]\n", n->member_access.base, n->member_access.offset);
			break;
			case TB_ARRAY_ACCESS:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " &r%u[r%u * %d]\n", n->array_access.base, n->array_access.index, n->array_access.stride);
			break;
			case TB_ATOMIC_XCHG:
			callback(user_data, "  r%u\t=\tATOMIC XCHG *r%u, r%u\n", i, n->atomic.addr, n->atomic.src);
			break;
			case TB_ATOMIC_ADD:
			callback(user_data, "  r%u\t=\tATOMIC ADD *r%u, r%u\n", i, n->atomic.addr, n->atomic.src);
			break;
			case TB_ATOMIC_SUB:
			callback(user_data, "  r%u\t=\tATOMIC SUB *r%u, r%u\n", i, n->atomic.addr, n->atomic.src);
			break;
			case TB_ATOMIC_AND:
			callback(user_data, "  r%u\t=\tATOMIC AND *r%u, r%u\n", i, n->atomic.addr, n->atomic.src);
			break;
			case TB_ATOMIC_XOR:
			callback(user_data, "  r%u\t=\tATOMIC XOR *r%u, r%u\n", i, n->atomic.addr, n->atomic.src);
			break;
			case TB_ATOMIC_OR: 
			callback(user_data, "  r%u\t=\tATOMIC OR *r%u, r%u\n", i, n->atomic.addr, n->atomic.src);
			break;
			case TB_AND:
            case TB_OR:
            case TB_XOR:
			case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_UDIV:
            case TB_SDIV:
            case TB_UMOD:
            case TB_SMOD:
            case TB_SHL:
            case TB_SHR:
            case TB_SAR:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " r%u ", n->i_arith.a);
            
			switch (type) {
                case TB_AND: callback(user_data, "&"); break;
                case TB_OR: callback(user_data, "|"); break;
                case TB_XOR: callback(user_data, "^"); break;
                case TB_ADD: callback(user_data, "+"); break;
                case TB_SUB: callback(user_data, "-"); break;
                case TB_MUL: callback(user_data, "*"); break;
                case TB_UDIV: callback(user_data, "/u"); break;
                case TB_SDIV: callback(user_data, "/s"); break;
                case TB_UMOD: callback(user_data, "%%u"); break;
                case TB_SMOD: callback(user_data, "%%s"); break;
				case TB_SHL: callback(user_data, "<<"); break;
				case TB_SHR: callback(user_data, ">>"); break;
				case TB_SAR: callback(user_data, ">>s"); break;
                default: tb_todo();
			}
            
			callback(user_data, " r%u\n", n->i_arith.b);
			break;
            case TB_FADD:
            case TB_FSUB:
            case TB_FMUL:
            case TB_FDIV:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " r%u ", n->f_arith.a);
            
			switch (type) {
                case TB_FADD: callback(user_data, "+"); break;
                case TB_FSUB: callback(user_data, "-"); break;
                case TB_FMUL: callback(user_data, "*"); break;
                case TB_FDIV: callback(user_data, "/"); break;
                default: tb_todo();
			}
            
			callback(user_data, " r%u\n", n->f_arith.b);
			break;
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
            case TB_CMP_FLT:
            case TB_CMP_FLE:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " r%u ", n->cmp.a);
            
			switch (type) {
                case TB_CMP_NE: callback(user_data, "!="); break;
                case TB_CMP_EQ: callback(user_data, "=="); break;
                case TB_CMP_ULT: callback(user_data, "<"); break;
                case TB_CMP_ULE: callback(user_data, "<="); break;
                case TB_CMP_SLT: callback(user_data, "<"); break;
                case TB_CMP_SLE: callback(user_data, "<="); break;
                case TB_CMP_FLT: callback(user_data, "<"); break;
                case TB_CMP_FLE: callback(user_data, "<="); break;
                default: tb_todo();
			}
            
			callback(user_data, " r%u", n->cmp.b);
			
			if (type == TB_CMP_SLT || type == TB_CMP_SLE) callback(user_data, " # signed\n");
			else if (type == TB_CMP_FLT || type == TB_CMP_FLE) callback(user_data, " # float\n");
			else callback(user_data, "\n");
			break;
            case TB_NEG:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " NEG r%u\n", n->unary);
			break;
            case TB_NOT:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " NOT r%u\n", n->unary);
			break;
            case TB_VA_START:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " VA_START r%u\n", n->unary);
			break;
            case TB_X86INTRIN_SQRT:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " X86.SQRT r%u\n", n->unary);
			break;
            case TB_X86INTRIN_RSQRT:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " X86.RSQRT r%u\n", n->unary);
			break;
            case TB_LOCAL:
			callback(user_data, "  r%u\t=\tLOCAL %d (%d align)\n", i, n->local.size, n->local.alignment);
			break;
            case TB_ICALL: {
				callback(user_data, "  r%u\t=\t", i);
				tb_print_type(dt, callback, user_data);
				callback(user_data, "INLINE CALL %s(", i, n->call.target->name);
				for (size_t j = n->call.param_start; j < n->call.param_end; j++) {
					if (j != n->call.param_start) callback(user_data, ", ");
					
					callback(user_data, "r%u", f->vla.data[j]);
				}
				callback(user_data, ")\n");
				break;
			}
			case TB_ECALL: {
				TB_ExternalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_External* e = &m->externals[n->ecall.target / per_thread_stride][n->ecall.target % per_thread_stride];
				
				callback(user_data, "  r%u\t=\t", i);
				tb_print_type(dt, callback, user_data);
				callback(user_data, "ECALL %s(", e->name);
				for (size_t j = n->ecall.param_start; j < n->ecall.param_end; j++) {
					if (j != n->ecall.param_start) callback(user_data, ", ");
					
					callback(user_data, "r%u", f->vla.data[j]);
				}
				callback(user_data, ")\n");
				break;
			}
            case TB_CALL: {
				callback(user_data, "  r%u\t=\t", i);
				tb_print_type(dt, callback, user_data);
				callback(user_data, "CALL %s(", n->call.target->name);
				for (size_t j = n->call.param_start; j < n->call.param_end; j++) {
					if (j != n->call.param_start) callback(user_data, ", ");
					
					callback(user_data, "r%u", f->vla.data[j]);
				}
				callback(user_data, ")\n");
				break;
			}
            case TB_VCALL: {
				callback(user_data, "  r%u\t=\t", i);
				tb_print_type(dt, callback, user_data);
				callback(user_data, "VCALL r%u(", n->vcall.target);
				for (size_t j = n->vcall.param_start; j < n->vcall.param_end; j++) {
					if (j != n->vcall.param_start) callback(user_data, ", ");
					
					callback(user_data, "r%u", f->vla.data[j]);
				}
				callback(user_data, ")\n");
				break;
			}
            case TB_SWITCH: {
				callback(user_data, " SWITCH\t");
				tb_print_type(dt, callback, user_data);
				callback(user_data, "\tr%u (\n", n->switch_.key);
				
				size_t entry_start = n->switch_.entries_start;
				size_t entry_count = (n->switch_.entries_end - n->switch_.entries_start) / 2;
				
				for (size_t j = 0; j < entry_count; j++) {
					TB_SwitchEntry* e = (TB_SwitchEntry*)&f->vla.data[entry_start + (j * 2)];
					
					callback(user_data, "\t\t\t%u -> L%d,\n", e->key, e->value);
				}
				callback(user_data, "\t\t\tdefault -> L%d)\n", n->switch_.default_label);
				break;
			}
			case TB_FUNC_ADDRESS:
			callback(user_data, "  r%u\t=\t &%s\n", i, n->func.value->name);
			break;
			case TB_EXTERN_ADDRESS: {
				TB_ExternalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_External* e = &m->externals[n->external.value / per_thread_stride][n->external.value % per_thread_stride];
				
				callback(user_data, "  r%u\t=\t &%s\n", i, e->name);
				break;
			}
			case TB_GLOBAL_ADDRESS: {
				TB_GlobalID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_Global* g = &m->globals[n->global.value / per_thread_stride][n->global.value % per_thread_stride];
				
				callback(user_data, "  r%u\t=\t &%s\n", i, g->name);
				break;
			}
            case TB_PARAM:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, "  PARAM %u\n", n->param.id);
			break;
            case TB_PARAM_ADDR:
			callback(user_data, "  r%u\t=\t&PARAM %u\n", i, f->nodes.data[n->param_addr.param].param.id);
			break;
            case TB_LOAD:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " *r%u (%d align)\n", n->load.address, n->load.alignment);
			break;
            case TB_STORE:
			callback(user_data, " *r%u \t=\t", n->store.address);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " r%u (%d align)\n", n->store.value, n->store.alignment);
			break;
            case TB_LABEL:
			if (n->label.id > 0) callback(user_data, "\n"); 
			callback(user_data, "L%d: # r%u terminates at r%u\n", n->label.id, i, n->label.terminator);
			break;
            case TB_GOTO:
			callback(user_data, "  goto L%d\n", n->goto_.label);
			break;
            case TB_IF:
			callback(user_data, "  if (r%u)\tL%d else L%d\n", n->if_.cond, n->if_.if_true, n->if_.if_false);
			break;
            case TB_PASS:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " PASS r%u\n", n->pass);
			break;
            case TB_PHI1:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " PHI L%d:r%u\n", f->nodes.data[n->phi1.a_label].label.id, n->phi1.a);
			break;
            case TB_PHI2:
			callback(user_data, "  r%u\t=\t", i);
			tb_print_type(dt, callback, user_data);
			callback(user_data, " PHI L%d:r%u, L%d:r%u\n", f->nodes.data[n->phi2.a_label].label.id, n->phi2.a, f->nodes.data[n->phi2.b_label].label.id, n->phi2.b);
			break;
            case TB_RET:
			callback(user_data, "  ret\t\t");
			tb_print_type(dt, callback, user_data);
			callback(user_data, " r%u", n->i_arith.a);
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
		} else {
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

void tb_patch2b(TB_Emitter* o, uint32_t pos, uint16_t i) {
	*((uint16_t*)&o->data[pos]) = i;
}

void tb_patch4b(TB_Emitter* o, uint32_t pos, uint32_t i) {
	*((uint32_t*)&o->data[pos]) = i;
}

uint8_t tb_get1b(TB_Emitter* o, uint32_t pos) {
	return *((uint8_t*)&o->data[pos]);
}

uint16_t tb_get2b(TB_Emitter* o, uint32_t pos) {
	return *((uint16_t*)&o->data[pos]);
}

uint32_t tb_get4b(TB_Emitter* o, uint32_t pos) {
	return *((uint32_t*)&o->data[pos]);
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

