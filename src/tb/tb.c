#define TB_INTERNAL
#include "tb.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#else
#warning "Threading implementation not supported! Cannot use multiple threads to compile"
#endif

static size_t tb_get_ptr_size(TB_Arch target_arch) {
    if (target_arch == TB_ARCH_X86_64) return 8;
    if (target_arch == TB_ARCH_AARCH64) return 8;
    
    tb_todo();
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

TB_API TB_Module* tb_module_create(TB_Arch target_arch, TB_System target_system, const TB_FeatureSet* features) {
	TB_Module* m = malloc(sizeof(TB_Module));
    
	m->target_arch = target_arch;
	m->target_system = target_system;
	m->features = *features;
    
	m->const32_patches.count = 0;
	m->const32_patches.capacity = 64;
	m->const32_patches.data = malloc(64 * sizeof(TB_ConstPool32Patch));
    
	m->call_patches.count = 0;
	m->call_patches.capacity = 64;
	m->call_patches.data = malloc(64 * sizeof(TB_FunctionPatch));
	
	m->functions.count = 0;
	m->functions.data = malloc(TB_MAX_FUNCTIONS * sizeof(TB_Function));
    
	m->compiled_functions.count = 0;
	m->compiled_functions.data = malloc(TB_MAX_FUNCTIONS * sizeof(TB_FunctionOutput));
    
	return m;
}

TB_API void tb_module_destroy(TB_Module* m) {
	loop(i, m->functions.count) {
		_aligned_free(m->functions.data[i].nodes.type);
		_aligned_free(m->functions.data[i].nodes.dt);
		_aligned_free(m->functions.data[i].nodes.payload);
		
		free(m->functions.data[i].name);
	}
    
	loop(i, m->compiled_functions.count) {
		free(m->compiled_functions.data[i].emitter.data);
	}
    
	free(m->functions.data);
	free(m->compiled_functions.data);
	free(m);
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

static int tb_x64_compile_thread(TB_Module* m) {
	const size_t count = m->compiled_functions.count;
	
	while (true) {
		size_t i = m->compiled_functions.num_reserved++;
		if (i >= count) return 0;
		
		m->compiled_functions.data[i] = x64_fast_code_gen.compile_function(&m->functions.data[i], &m->features);
		m->compiled_functions.num_compiled++;
	}
}

TB_API bool tb_module_compile(TB_Module* m, int optimization_level, int max_threads) {
	// Validate the functions
	uint32_t errors = 0;
	uint32_t ir_node_count = 0;
	loop(i, m->functions.count) {
		errors += tb_validate(&m->functions.data[i]);
		ir_node_count += m->functions.data[i].nodes.count;
	}
	if (errors > 0) return false;
	
	printf("Node count: %d\n", ir_node_count);
	m->compiled_functions.count = m->functions.count;
    
	if (optimization_level == TB_OPT_O0) {
		// Don't optimize
		/*loop(i, m->functions.count) {
			TB_Function* f = &m->functions.data[i];
			tb_function_print(f);
		}*/
	} else {
		loop(i, m->functions.count) {
			TB_Function* f = &m->functions.data[i];
			
			restart_opt: {
				tb_function_print(f);
				printf("\n\n\n");
				
				if (tb_opt_canonicalize(f)) goto restart_opt;
				if (tb_opt_strength_reduction(f)) goto restart_opt;
				if (tb_opt_mem2reg(f)) goto restart_opt;
				if (tb_opt_dce(f)) goto restart_opt;
				if (tb_opt_inline(f)) goto restart_opt;
				if (tb_opt_compact_dead_regs(f)) goto restart_opt;
			}
			
			tb_function_print(f);
			printf("\n\n\n");
		}
	}
	
	if (max_threads > 1) {
		// TODO(NeGate): Needs some rework, but it should
		// be a simple way of doing multithreading
		m->compiled_functions.num_compiled = 0;
		m->compiled_functions.num_reserved = 0;
		
#ifdef _WIN32
		assert(max_threads <= TB_MAX_THREADS);
		HANDLE threads[TB_MAX_THREADS];
		
		switch (m->target_arch) {
			case TB_ARCH_X86_64:
			loop(i, max_threads) {
				threads[i] = CreateThread(NULL, 2 * 1024 * 1024, (LPTHREAD_START_ROUTINE)tb_x64_compile_thread, m, 0, 0);
			}
			break;
			default:
			printf("TinyBackend error: Unknown target!\n");
			tb_todo();
		}
		
		WaitForMultipleObjects(max_threads, threads, TRUE, -1);
		loop(i, max_threads) {
			CloseHandle(threads[i]);
		}
		return true;
#else
#error "Implement this!!!"
#endif
	} else {
		switch (m->target_arch) {
			case TB_ARCH_X86_64:
			loop(i, m->functions.count) {
				m->compiled_functions.data[i] = x64_fast_code_gen.compile_function(&m->functions.data[i], &m->features);
			}
			return true;
			default:
			printf("TinyBackend error: Unknown target!\n");
			tb_todo();
		}
	}
}

TB_API bool tb_module_export(TB_Module* m, FILE* f) {
	const ICodeGen* restrict code_gen;
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
	
	f->nodes.type = _aligned_recalloc(f->nodes.type, cap, sizeof(TB_RegType), 64);
	f->nodes.dt = _aligned_recalloc(f->nodes.dt, cap, sizeof(TB_DataType), 64);
	f->nodes.payload = _aligned_recalloc(f->nodes.payload, cap, sizeof(TB_RegPayload), 64);
	
	// zero out the extra space
	// TODO(NeGate): optimize this... somehow?
	//memset(&f->nodes.type[f->nodes.count], 0, (cap - f->nodes.count) * sizeof(TB_RegType));
	//memset(&f->nodes.dt[f->nodes.count], 0, (cap - f->nodes.count) * sizeof(TB_DataType));
	//memset(&f->nodes.payload[f->nodes.count], 0, (cap - f->nodes.count) * sizeof(TB_RegPayload));
}

TB_API TB_Function* tb_function_create(TB_Module* m, const char* name, TB_DataType return_dt) {
	assert(m->functions.count < TB_MAX_FUNCTIONS);
    
	TB_Function* f = &m->functions.data[m->functions.count++];
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

TB_API void* tb_module_get_jit_func_by_name(TB_Module* m, const char* name) {
	for (size_t i = 0; i < m->compiled_functions.count; i++) {
		if (strcmp(m->compiled_functions.data[i].name, name)) return m->compiled_function_pos[i];
	}
	
	return NULL;
}

TB_API void* tb_module_get_jit_func(TB_Module* m, TB_Function* f) {
	assert(m->compiled_function_pos);
	return m->compiled_function_pos[f - m->functions.data];
}

//
// TLS - Thread local storage
// 
// Certain backend elements require memory but we would prefer to avoid 
// making any heap allocations when possible to there's a preallocated 
// block per thread that can run TB.
//
static TB_TemporaryStorage* tb_tls_allocate() {
	static _Thread_local uint8_t* tb_thread_storage;
	static uint8_t tb_temporary_storage[TB_TEMPORARY_STORAGE_SIZE * TB_MAX_THREADS];
	static atomic_uint tb_used_tls_slots;
	
	if (tb_thread_storage == NULL) {
		unsigned int slot = atomic_fetch_add(&tb_used_tls_slots, 1);
		if (slot >= TB_MAX_THREADS) tb_todo();
        
		tb_thread_storage = &tb_temporary_storage[slot * TB_TEMPORARY_STORAGE_SIZE];
	}
    
	TB_TemporaryStorage* store = (TB_TemporaryStorage*)tb_thread_storage;
	store->used = 0;
	return store;
}

static void* tb_tls_push(TB_TemporaryStorage* store, size_t size) {
	assert(sizeof(TB_TemporaryStorage) + store->used + size < TB_TEMPORARY_STORAGE_SIZE);
    
	void* ptr = &store->data[store->used];
	store->used += size;
	return ptr;
}

static void* tb_tls_pop(TB_TemporaryStorage* store, size_t size) {
	assert(sizeof(TB_TemporaryStorage) + store->used > size);
    
	store->used -= size;
	return &store->data[store->used];
}

static void* tb_tls_peek(TB_TemporaryStorage* store, size_t distance) {
	assert(sizeof(TB_TemporaryStorage) + store->used > distance);
    
	return &store->data[store->used - distance];
}

// IR BUILDER
// 
// Handles generating the TB_Function IR via C functions. 
// Note that these functions can perform certain simple
// optimizations while the generation happens to improve
// the machine code output or later analysis stages.
static TB_Register tb_make_reg(TB_Function* f, int type, TB_DataType dt) {
	// Cannot add registers to terminated basic blocks, except labels
	// which start new basic blocks
	assert(type == TB_LABEL || f->current_label);
    
	if (f->nodes.count + 1 >= f->nodes.capacity) tb_resize_node_stream(f, tb_next_pow2(f->nodes.count + 1));
    
	TB_Register r = f->nodes.count++;
	f->nodes.type[r] = type;
	f->nodes.dt[r] = dt;
	return r;
}

static TB_Int128 tb_fold_add(TB_ArithmaticBehavior ab, TB_DataType dt, TB_Int128 a, TB_Int128 b) {
	assert(a.hi == 0 && b.hi == 0);
    if (dt.type == TB_I128) {
		tb_todo();
	} else {
		uint64_t shift = 64 - (8 << (dt.type - TB_I8));
		uint64_t mask = (~0ull) >> shift;
		
		uint64_t sum;
		if (__builtin_add_overflow(a.lo << shift, b.lo << shift, &sum)) {
			sum >>= shift;
			
			if (ab == TB_CAN_WRAP) sum &= mask;
			else if (ab == TB_SATURATED_UNSIGNED) sum = mask;
			//else if (ab == TB_WRAP_CHECK) { printf("warp check!!!\n"); }
		}
		
		sum = (sum >> shift) & mask;
		return (TB_Int128) { sum }; 
	}
}

static TB_Int128 tb_fold_sub(TB_ArithmaticBehavior ab, TB_DataType dt, TB_Int128 a, TB_Int128 b) {
	assert(a.hi == 0 && b.hi == 0);
    if (dt.type == TB_I128) {
		tb_todo();
	} else {
		uint64_t shift = 64 - (8 << (dt.type - TB_I8));
		uint64_t mask = (~0ull) >> shift;
		
		uint64_t sum;
		if (__builtin_sub_overflow(a.lo << shift, b.lo << shift, &sum)) {
			sum = (sum >> shift) & mask;
			
			if (ab == TB_CAN_WRAP) sum &= mask;
			else if (ab == TB_SATURATED_UNSIGNED) sum = 0;
			//else if (ab == TB_WRAP_CHECK) { printf("warp check!!!\n"); }
		} else {
			sum = (sum >> shift) & mask;
		}
		
		return (TB_Int128) { sum }; 
	}
}

static TB_Int128 tb_fold_mul(TB_ArithmaticBehavior ab, TB_DataType dt, TB_Int128 a, TB_Int128 b) {
	assert(a.hi == 0 && b.hi == 0);
    if (dt.type == TB_I128) {
		tb_todo();
	} else {
		uint64_t shift = 64 - (8 << (dt.type - TB_I8));
		uint64_t mask = (~0ull) >> shift;
		
		uint64_t sum;
		if (__builtin_mul_overflow(a.lo << shift, b.lo << shift, &sum)) {
			sum = (sum >> shift) & mask;
			
			if (ab == TB_CAN_WRAP) sum &= mask;
			else if (ab == TB_SATURATED_UNSIGNED) sum = 0;
			//else if (ab == TB_WRAP_CHECK) { printf("warp check!!!\n"); }
		} else {
			sum = (sum >> shift) & mask;
		}
		
		return (TB_Int128) { sum }; 
	}
}

static TB_Register tb_cse_arith(TB_Function* f, int type, TB_DataType dt, TB_ArithmaticBehavior arith_behavior, TB_Register a, TB_Register b) {
	assert(f->current_label);
	for (size_t i = f->current_label + 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == type
			&& TB_DATA_TYPE_EQUALS(f->nodes.dt[i], dt)
			&& f->nodes.payload[i].i_arith.arith_behavior == arith_behavior
			&& f->nodes.payload[i].i_arith.a == a
			&& f->nodes.payload[i].i_arith.b == b) {
			return i;
		}
	}
    
	TB_Register r = tb_make_reg(f, type, dt);
	f->nodes.payload[r].i_arith.arith_behavior = arith_behavior;
	f->nodes.payload[r].i_arith.a = a;
	f->nodes.payload[r].i_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_sxt(TB_Function* f, TB_Register src, TB_DataType dt) {
	assert(f->current_label);
	for (size_t i = f->current_label + 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_SIGN_EXT
			&& TB_DATA_TYPE_EQUALS(f->nodes.dt[i], dt)
			&& f->nodes.payload[i].ext == src) {
			return i;
		}
	}
    
    TB_Register r = tb_make_reg(f, TB_SIGN_EXT, dt);
	f->nodes.payload[r].ext = src;
	return r;
}

TB_API TB_Register tb_inst_zxt(TB_Function* f, TB_Register src, TB_DataType dt) {
    assert(f->current_label);
	for (size_t i = f->current_label + 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_ZERO_EXT
			&& TB_DATA_TYPE_EQUALS(f->nodes.dt[i], dt)
			&& f->nodes.payload[i].ext == src) {
			return i;
		}
	}
    
	TB_Register r = tb_make_reg(f, TB_ZERO_EXT, dt);
	f->nodes.payload[r].ext = src;
	return r;
}

TB_API TB_Register tb_inst_param(TB_Function* f, TB_DataType dt) {
	TB_Register r = tb_make_reg(f, TB_PARAM, dt);
	f->nodes.payload[r].param.id = f->parameter_count++;
    
    // TODO(NeGate): It's currently assuming that all pointers are 8bytes big,
    // which is untrue for some platforms.
	int param_size;
	switch (dt.type) {
        case TB_I8:  param_size = 1; break;
        case TB_I16: param_size = 2; break;
        case TB_I32: param_size = 4; break;
        case TB_I64: param_size = 8; break;
        case TB_F32: param_size = 4; break;
        case TB_F64: param_size = 8; break;
        case TB_PTR: param_size = 8; break;
        default: tb_todo();
	}
    
	assert(dt.count > 0);
	f->nodes.payload[r].param.size = param_size * dt.count;
	return r;
}

TB_API TB_Register tb_inst_param_addr(TB_Function* f, TB_Register param) {
	assert(f->nodes.type[param] == TB_PARAM);
    
	int param_size = f->nodes.payload[param].param.size;
	
	TB_Register r = tb_make_reg(f, TB_PARAM_ADDR, TB_TYPE_PTR());
	f->nodes.payload[r].param_addr.param = param;
	f->nodes.payload[r].param_addr.size = param_size;
	f->nodes.payload[r].param_addr.alignment = param_size;
	return r;
}

TB_API TB_Register tb_inst_local(TB_Function* f, uint32_t size, uint32_t alignment) {
	TB_Register r = tb_make_reg(f, TB_LOCAL, TB_TYPE_PTR());
	f->nodes.payload[r].local.alignment = alignment;
	f->nodes.payload[r].local.size = size;
	return r;
}

TB_API TB_Register tb_inst_load(TB_Function* f, TB_DataType dt, TB_Register addr, uint32_t alignment) {
	assert(f->current_label);
    for (size_t i = f->current_label + 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LOAD
			&& TB_DATA_TYPE_EQUALS(f->nodes.dt[i], dt)
			&& f->nodes.payload[i].load.address == addr
			&& f->nodes.payload[i].load.alignment == alignment) {
			return i;
		}
	}
	
	TB_Register r = tb_make_reg(f, TB_LOAD, dt);
	f->nodes.payload[r].load.address = addr;
	f->nodes.payload[r].load.alignment = alignment;
	return r;
}

TB_API void tb_inst_store(TB_Function* f, TB_DataType dt, TB_Register addr, TB_Register val, uint32_t alignment) {
	TB_Register r = tb_make_reg(f, TB_STORE, dt);
	f->nodes.payload[r].store.address = addr;
	f->nodes.payload[r].store.value = val;
	f->nodes.payload[r].store.alignment = alignment;
	return;
}

TB_API TB_Register tb_inst_iconst(TB_Function* f, TB_DataType dt, uint64_t imm) {
	assert(f->current_label);
    for (size_t i = f->current_label + 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LOAD
			&& TB_DATA_TYPE_EQUALS(f->nodes.dt[i], dt)
			&& f->nodes.payload[i].i_const.lo == imm
			&& f->nodes.payload[i].i_const.hi == 0) {
			return i;
		}
	}
	
	assert(dt.type >= TB_I8 && dt.type <= TB_I128);
	uint64_t mask = (~0ull) >> (64 - (8 << (dt.type - TB_I8)));
	
	TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
	f->nodes.payload[r].i_const.hi = 0;
	f->nodes.payload[r].i_const.lo = imm & mask;
	return r;
}

TB_API TB_Register tb_inst_iconst128(TB_Function* f, TB_DataType dt, TB_Int128 imm) {
	if (dt.type == TB_I128) {
		TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
		f->nodes.payload[r].i_const.lo = imm.lo;
		f->nodes.payload[r].i_const.hi = imm.hi;
		return r;
	}
	else if (dt.type == TB_I64) {
		// Truncate
		TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
		f->nodes.payload[r].i_const.lo = imm.lo;
		f->nodes.payload[r].i_const.hi = 0;
		return r;
	}
	else if (dt.type == TB_I32) {
		// Truncate
		TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
		f->nodes.payload[r].i_const.lo = imm.lo & 0xFFFFFFFFull;
		f->nodes.payload[r].i_const.hi = 0;
		return r;
	}
	else if (dt.type == TB_I16) {
		// Truncate
		TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
		f->nodes.payload[r].i_const.lo = imm.lo & 0xFFFFull;
		f->nodes.payload[r].i_const.hi = 0;
		return r;
	}
	else if (dt.type == TB_I8) {
		// Truncate
		TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
		f->nodes.payload[r].i_const.lo = imm.lo & 0xFFull;
		f->nodes.payload[r].i_const.hi = 0;
		return r;
	}
	else tb_todo();
}

TB_API TB_Register tb_inst_fconst(TB_Function* f, TB_DataType dt, double imm) {
	TB_Register r = tb_make_reg(f, TB_FLOAT_CONST, dt);
	f->nodes.payload[r].f_const = imm;
	return r;
}

TB_API TB_Register tb_inst_array_access(TB_Function* f, TB_Register base, TB_Register index, uint32_t stride) {
	TB_Register r = tb_make_reg(f, TB_ARRAY_ACCESS, TB_TYPE_PTR());
	f->nodes.payload[r].array_access.base = base;
	f->nodes.payload[r].array_access.index = index;
	f->nodes.payload[r].array_access.stride = stride;
	return r;
}

TB_API TB_Register tb_inst_member_access(TB_Function* f, TB_Register base, int32_t offset) {
	TB_Register r = tb_make_reg(f, TB_MEMBER_ACCESS, TB_TYPE_PTR());
	f->nodes.payload[r].member_access.base = base;
	f->nodes.payload[r].member_access.offset = offset;
	return r;
}

TB_API TB_Register tb_inst_call(TB_Function* f, TB_DataType dt, const TB_Function* target, size_t param_count, const TB_Register* params) {
	// Reserve space for the arguments
	if (f->vla.count + param_count >= f->vla.capacity) {
		// TODO(NeGate): This might be excessive for this array, idk :P
		f->vla.capacity = tb_next_pow2(f->vla.count + param_count);
		f->vla.data = realloc(f->vla.data, f->vla.capacity * sizeof(TB_Register));
	}
	
	int param_start = f->vla.count;
	memcpy(f->vla.data + f->vla.count, params, param_count * sizeof(TB_Register));
	f->vla.count += param_count;
	int param_end = f->vla.count;
	
	TB_Register r = tb_make_reg(f, TB_CALL, dt);
	f->nodes.payload[r].call.target = target;
	f->nodes.payload[r].call.param_start = param_start;
	f->nodes.payload[r].call.param_end = param_end;
	return r;
}

TB_API TB_Register tb_inst_and(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	// bitwise operators can't wrap
	return tb_cse_arith(f, TB_AND, dt, TB_NO_WRAP, a, b);
}

TB_API TB_Register tb_inst_or(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	// bitwise operators can't wrap
	return tb_cse_arith(f, TB_OR, dt, TB_NO_WRAP, a, b);
}

TB_API TB_Register tb_inst_add(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
	if (f->nodes.type[a] == TB_INT_CONST) tb_swap(a, b);
	
    TB_RegType a_type = f->nodes.type[a];
	TB_RegType b_type = f->nodes.type[b];
	
	if (a_type == TB_INT_CONST && b_type == TB_INT_CONST) {
		TB_Int128 sum = tb_fold_add(arith_behavior, dt, f->nodes.payload[a].i_const, f->nodes.payload[b].i_const);
		
		return tb_inst_iconst128(f, dt, sum);
	} else if (b_type == TB_INT_CONST && f->nodes.payload[b].i_const.lo == 0) {
		return a;
	} else if (a_type == TB_ADD) {
		// reassoc
		TB_Register aa = f->nodes.payload[a].i_arith.a;
		TB_Register ab = f->nodes.payload[a].i_arith.b;
		
		return tb_inst_add(f, dt, aa, tb_inst_add(f, dt, ab, b, arith_behavior), arith_behavior);
    }
	
	return tb_cse_arith(f, TB_ADD, dt, arith_behavior, a, b);
}

TB_API TB_Register tb_inst_sub(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
	if (a == b) return tb_inst_iconst(f, dt, 0);
	
	if (f->nodes.type[a] == TB_INT_CONST && f->nodes.type[b] == TB_INT_CONST) {
		TB_Int128 sum = tb_fold_sub(arith_behavior, dt, f->nodes.payload[a].i_const, f->nodes.payload[b].i_const);
		
		return tb_inst_iconst128(f, dt, sum);
	}
	
	return tb_cse_arith(f, TB_SUB, dt, arith_behavior, a, b);
}

TB_API TB_Register tb_inst_mul(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
    TB_RegType a_type = f->nodes.type[a];
	TB_RegType b_type = f->nodes.type[b];
	
	if (a_type == TB_INT_CONST && b_type == TB_INT_CONST) {
		TB_Int128 sum = tb_fold_mul(arith_behavior, dt, f->nodes.payload[a].i_const, f->nodes.payload[b].i_const);
		
		return tb_inst_iconst128(f, dt, sum);
	}
	
	return tb_cse_arith(f, TB_MUL, dt, arith_behavior, a, b);
}

TB_API TB_Register tb_inst_div(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness) {
	// division can't wrap or overflow
    return tb_cse_arith(f, signedness ? TB_SDIV : TB_UDIV, dt, TB_NO_WRAP, a, b);
}

TB_API TB_Register tb_inst_shl(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
    return tb_cse_arith(f, TB_SHL, dt, arith_behavior, a, b);
}

TB_API TB_Register tb_inst_sar(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
    // shift right can't wrap or overflow
    return tb_cse_arith(f, TB_SAR, dt, TB_NO_WRAP, a, b);
}

TB_API TB_Register tb_inst_shr(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
    // shift right can't wrap or overflow
    return tb_cse_arith(f, TB_SHR, dt, TB_NO_WRAP, a, b);
}

TB_API TB_Register tb_inst_fadd(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_FADD, dt);
	f->nodes.payload[r].f_arith.a = a;
	f->nodes.payload[r].f_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_fsub(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_FSUB, dt);
	f->nodes.payload[r].f_arith.a = a;
	f->nodes.payload[r].f_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_fmul(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_FMUL, dt);
	f->nodes.payload[r].f_arith.a = a;
	f->nodes.payload[r].f_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_fdiv(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_FDIV, dt);
	f->nodes.payload[r].f_arith.a = a;
	f->nodes.payload[r].f_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_cmp_eq(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_EQ, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ne(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_NE, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_slt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_SLT, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_sle(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_SLE, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_sgt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_SLT, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = b;
	f->nodes.payload[r].cmp.b = a;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_sge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_SLE, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = b;
	f->nodes.payload[r].cmp.b = a;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ult(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_ULT, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ule(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_ULE, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ugt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_ULT, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = b;
	f->nodes.payload[r].cmp.b = a;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_uge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_ULE, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = b;
	f->nodes.payload[r].cmp.b = a;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_flt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLT, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_fle(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLE, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_fgt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLT, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = b;
	f->nodes.payload[r].cmp.b = a;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_fge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLE, TB_TYPE_BOOL(1));
	f->nodes.payload[r].cmp.a = b;
	f->nodes.payload[r].cmp.b = a;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_phi2(TB_Function* f, TB_DataType dt, TB_Label a_label, TB_Register a, TB_Label b_label, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_PHI2, dt);
	f->nodes.payload[r].phi2.a_label = tb_find_reg_from_label(f, a_label);
	f->nodes.payload[r].phi2.a = a;
	f->nodes.payload[r].phi2.b_label = tb_find_reg_from_label(f, b_label);
	f->nodes.payload[r].phi2.b = b;
	return r;
}

TB_API TB_Register tb_inst_label(TB_Function* f, TB_Label id) {
	TB_Register r = tb_make_reg(f, TB_LABEL, TB_TYPE_PTR());
	f->nodes.payload[r].label.id = id;
	f->nodes.payload[r].label.terminator = TB_NULL_REG;
	f->nodes.payload[r].label.is_loop = false;
    
	if (f->current_label) {
		f->nodes.payload[f->current_label].label.terminator = r;
	}
    
	f->current_label = r;
	return r;
}

TB_API void tb_inst_goto(TB_Function* f, TB_Label id) {
    if (f->current_label == TB_NULL_REG) {
        // Was placed after a terminator instruction,
        // just omit this to avoid any issues it's not
        // a big deal for example:
        // RET x
        // ~~GOTO .L5~~
        // .L4:
        return;
    }
    
	TB_Register r = tb_make_reg(f, TB_GOTO, TB_TYPE_VOID());
	f->nodes.payload[r].goto_.label = id;
    
	assert(f->current_label);
	f->nodes.payload[f->current_label].label.terminator = r;
	f->current_label = TB_NULL_REG;
}

TB_API TB_Register tb_inst_if(TB_Function* f, TB_Register cond, TB_Label if_true, TB_Label if_false) {
	TB_Register r = tb_make_reg(f, TB_IF, TB_TYPE_VOID());
	f->nodes.payload[r].if_.cond = cond;
	f->nodes.payload[r].if_.if_true = if_true;
	f->nodes.payload[r].if_.if_false = if_false;
    
	assert(f->current_label);
	f->nodes.payload[f->current_label].label.terminator = r;
	f->current_label = TB_NULL_REG;
	return r;
}

TB_API void tb_inst_switch(TB_Function* f, TB_DataType dt, TB_Register key, TB_Label default_label, size_t entry_count, const TB_SwitchEntry* entries) {
	// the switch entries are 2 slots each
	size_t param_count = entry_count * 2;
	
	// Reserve space for the arguments
	if (f->vla.count + param_count >= f->vla.capacity) {
		// TODO(NeGate): This might be excessive for this array, idk :P
		f->vla.capacity = tb_next_pow2(f->vla.count + param_count);
		f->vla.data = realloc(f->vla.data, f->vla.capacity * sizeof(TB_Register));
	}
	
	int param_start = f->vla.count;
	memcpy(f->vla.data + f->vla.count, entries, param_count * sizeof(TB_Register));
	f->vla.count += param_count;
	int param_end = f->vla.count;
	
	TB_Register r = tb_make_reg(f, TB_SWITCH, dt);
	f->nodes.payload[r].switch_.key = key;
	f->nodes.payload[r].switch_.default_label = default_label;
	f->nodes.payload[r].switch_.entries_start = param_start;
	f->nodes.payload[r].switch_.entries_end = param_end;
	
	assert(f->current_label);
	f->nodes.payload[f->current_label].label.terminator = r;
	f->current_label = TB_NULL_REG;
}

TB_API void tb_inst_ret(TB_Function* f, TB_DataType dt, TB_Register value) {
	TB_Register r = tb_make_reg(f, TB_RET, dt);
	f->nodes.payload[r].ret.value = value;
    
	assert(f->current_label);
	f->nodes.payload[f->current_label].label.terminator = r;
	f->current_label = TB_NULL_REG;
}

uint32_t tb_emit_const32_patch(TB_Module* m, uint32_t func_id, size_t pos, uint32_t data) {
	assert(pos < UINT32_MAX);
	if (m->const32_patches.count + 1 >= m->const32_patches.capacity) {
		m->const32_patches.capacity *= 2;
		m->const32_patches.data = realloc(m->const32_patches.data, m->const32_patches.capacity * sizeof(TB_ConstPool32Patch));
	}
    
	size_t r = m->const32_patches.count++;
	m->const32_patches.data[r] = (TB_ConstPool32Patch){
		.func_id = func_id,
		.pos = pos,
		.raw_data = data
	};
	
	return r * 4;
}

void tb_emit_call_patch(TB_Module* m, uint32_t func_id, uint32_t target_id, size_t pos) {
	assert(pos < UINT32_MAX);
	if (m->call_patches.count + 1 >= m->call_patches.capacity) {
		m->call_patches.capacity *= 2;
		m->call_patches.data = realloc(m->call_patches.data, m->call_patches.capacity * sizeof(TB_FunctionPatch));
	}
    
	size_t r = m->call_patches.count++;
	m->call_patches.data[r] = (TB_FunctionPatch){
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
    
	for (TB_Register i = 0; i < f->nodes.count; i++) {
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
				printf(" %llx%llx\n", p.i_const.hi, p.i_const.lo);
			}
			else {
				printf(" %lld\n", p.i_const.lo);
			}
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
			printf("  ret\t \t");
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

