#define TB_INTERNAL
#include "tb.h"

static uint8_t tb_temporary_storage[TB_TEMPORARY_STORAGE_SIZE * TB_MAX_THREADS];
static uint8_t* tb_thread_storage;
static atomic_uint tb_used_tls_slots;

static size_t tb_get_ptr_size(TB_Arch target_arch) {
    if (target_arch == TB_ARCH_X86_64) return 8;
    if (target_arch == TB_ARCH_AARCH64) return 8;
    
    tb_unreachable();
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
    } else tb_unreachable();
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
		free(m->functions.data[i].nodes);
		free(m->functions.data[i].name);
	}
    
	loop(i, m->compiled_functions.count) {
		free(m->compiled_functions.data[i].emitter.data);
	}
    
	free(m->functions.data);
	free(m->compiled_functions.data);
	free(m);
}

TB_API void tb_module_compile(TB_Module* m, int optimization_level, int max_threads) {
	m->compiled_functions.count = m->functions.count;
    
	if (optimization_level == TB_OPT_O0) {
		// Don't optimize
		loop(i, m->functions.count) {
			TB_Function* f = &m->functions.data[i];
			tb_function_print(f);
		}
	} else if (optimization_level == TB_OPT_O1) {
		// Perform basic optimizations, mem2reg, dce, cse
		// No complex loop transforms, minor inlining allowed
		
		loop(i, m->functions.count) {
			TB_Function* f = &m->functions.data[i];
			
			bool changes;
			do {
				changes = false;
				
				tb_function_print(f);
				printf("\n\n\n");
				
				changes |= tb_opt_mem2reg(f);
				changes |= tb_opt_dce(f);
			} while (changes);
		}
	} else {
		// TODO(NeGate): Implement this!
		tb_unreachable();
	}
	
	// TODO(NeGate): Implement the optimization passes
	
	switch (m->target_arch) {
		case TB_ARCH_X86_64:
		loop(i, m->functions.count) {
			m->compiled_functions.data[i] = x64_fast_code_gen.compile_function(&m->functions.data[i], &m->features);
		}
		break;
		case TB_ARCH_AARCH64:
		loop(i, m->functions.count) {
			m->compiled_functions.data[i] = aarch64_fast_code_gen.compile_function(&m->functions.data[i], &m->features);
		}
		break;
		default:
		printf("TinyBackend error: Unknown target!\n");
		tb_unreachable();
	}
}

TB_API void tb_module_export(TB_Module* m, FILE* f) {
	const ICodeGen* restrict code_gen;
	switch (m->target_arch) {
		case TB_ARCH_X86_64: code_gen = &x64_fast_code_gen; break;
		case TB_ARCH_AARCH64: code_gen = &aarch64_fast_code_gen; break;
		default: tb_unreachable();
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
		tb_unreachable();
	}
}

TB_API TB_Function* tb_function_create(TB_Module* m, const char* name, TB_DataType return_dt) {
	assert(m->functions.count < TB_MAX_FUNCTIONS);
    
	TB_Function* f = &m->functions.data[m->functions.count++];
	// TODO(NeGate): We might wanna do something better with these strings
	// especially since they'll be packed in a string table eventually
	f->name = malloc(strlen(name) + 1);
	strcpy(f->name, name);
	
	f->return_dt = return_dt;
	f->module = m;
    
	f->capacity = 64;
	f->count = 0;
	f->nodes = malloc(f->capacity * sizeof(TB_Node));
    
	f->parameter_count = 0;
	f->locals_stack_usage = 0;
    
	f->nodes[0] = (TB_Node){ 0 };
    
	f->nodes[1].type = TB_LABEL;
	f->nodes[1].dt = TB_TYPE_PTR();
	f->nodes[1].label.id = 0;
	f->nodes[1].label.terminator = TB_NULL_REG;
	f->nodes[1].label.is_loop = false;
	f->count = 2;
    
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
	if (tb_thread_storage == NULL) {
		unsigned int slot = atomic_fetch_add(&tb_used_tls_slots, 1);
		if (slot >= TB_MAX_THREADS) tb_unreachable();
        
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
	// Cannot add registers to terminated basic blocks
	assert(f->current_label);
    
	if (f->count + 1 >= f->capacity) {
		f->capacity *= 2;
		f->nodes = realloc(f->nodes, f->capacity * sizeof(TB_Node));
	}
    
	TB_Register r = f->count++;
	f->nodes[r].type = type;
	f->nodes[r].dt = dt;
	return r;
}

// This function performs addition portable using the arithmatic behavior enum
TB_API TB_Int128 tb_emulate_add(TB_Function* f, TB_ArithmaticBehavior arith_behavior, TB_DataType dt, TB_Int128 a, TB_Int128 b) {
	assert(a.hi == 0 && b.hi == 0);
    
	uint64_t mask;
	switch (dt.type) {
        case TB_I8:   mask  = 0xFFull; break;
        case TB_I16:  mask = 0xFFFFull; break;
        case TB_I32:  mask = 0xFFFFFFFFull; break;
        case TB_I64:  mask = 0xFFFFFFFFFFFFFFFFull; break;
        case TB_I128: mask = 0xFFFFFFFFFFFFFFFFull; break;
        default: tb_unreachable();
	}
    
	switch (arith_behavior) {
        case TB_NO_WRAP:
		return (TB_Int128) { a.lo + b.lo };
        case TB_WRAP_CHECK: {
            // TODO(NeGate): Throw runtime error in this scenario 
            // if the value is out of bounds
            tb_unreachable();
        }
        case TB_CAN_WRAP: {
            return (TB_Int128) { (a.lo + b.lo) & mask };
        }
        case TB_SATURATED_UNSIGNED: {
            a.lo &= mask;
            b.lo &= mask;
            
            uint64_t sum = a.lo + b.lo;
            
            // Overflow
            if (sum < a.lo) return (TB_Int128) { mask };
            return (TB_Int128) { sum };
        }
        // TODO(NeGate): Implement this
        default: tb_unreachable();
	}
}

static TB_Register tb_cse_arith(TB_Function* f, int type, TB_DataType dt, TB_ArithmaticBehavior arith_behavior, TB_Register a, TB_Register b) {
	assert(f->current_label);
	loop_range(i, f->current_label, f->count) {
		if (TB_DATA_TYPE_EQUALS(f->nodes[i].dt, dt)
			&& f->nodes[i].type == type
			&& f->nodes[i].i_arith.arith_behavior == arith_behavior
			&& f->nodes[i].i_arith.a == a
			&& f->nodes[i].i_arith.b == b) {
			return i;
		}
	}
    
	TB_Register r = tb_make_reg(f, type, dt);
	f->nodes[r].i_arith.arith_behavior = arith_behavior;
	f->nodes[r].i_arith.a = a;
	f->nodes[r].i_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_sxt(TB_Function* f, TB_Register src, TB_DataType dt) {
	loop_range(i, f->current_label, f->count) {
		if (f->nodes[i].type == TB_SIGN_EXT 
			&& f->nodes[i].ext == src
			&& TB_DATA_TYPE_EQUALS(f->nodes[i].dt, dt)) {
			return i;
		}
	}
    
    TB_Register r = tb_make_reg(f, TB_SIGN_EXT, dt);
	f->nodes[r].ext = src;
	return r;
}

TB_API TB_Register tb_inst_zxt(TB_Function* f, TB_Register src, TB_DataType dt) {
    assert(f->current_label);
	loop_range(i, f->current_label, f->count) {
		if (f->nodes[i].type == TB_ZERO_EXT 
			&& f->nodes[i].ext == src
			&& TB_DATA_TYPE_EQUALS(f->nodes[i].dt, dt)) {
			return i;
		}
	}
    
	TB_Register r = tb_make_reg(f, TB_ZERO_EXT, dt);
	f->nodes[r].ext = src;
	return r;
}

TB_API TB_Register tb_inst_param(TB_Function* f, TB_DataType dt) {
	TB_Register r = tb_make_reg(f, TB_PARAM, dt);
	f->nodes[r].param.id = f->parameter_count++;
    
    // TODO(NeGate): It's currently assuming that all pointers are 8bytes big,
    // which is untrue for some platforms.
	switch (dt.type) {
        case TB_I8:  f->nodes[r].param.size = 1; break;
        case TB_I16: f->nodes[r].param.size = 2; break;
        case TB_I32: f->nodes[r].param.size = 4; break;
        case TB_I64: f->nodes[r].param.size = 8; break;
        case TB_F32: f->nodes[r].param.size = 4; break;
        case TB_F64: f->nodes[r].param.size = 8; break;
        case TB_PTR: f->nodes[r].param.size = 8; break;
        default: tb_unreachable();
	}
    
	assert(dt.count > 0);
	f->nodes[r].param.size *= dt.count;
	return r;
}

TB_API TB_Register tb_inst_param_addr(TB_Function* f, TB_Register param) {
	assert(f->nodes[param].type == TB_PARAM);
    
	TB_Register r = tb_make_reg(f, TB_PARAM_ADDR, TB_TYPE_PTR());
	f->nodes[r].param_addr.param = param;
	f->nodes[r].param_addr.size = f->nodes[param].param.size;
	f->nodes[r].param_addr.alignment = f->nodes[param].param.size;
	return r;
}

TB_API TB_Register tb_inst_local(TB_Function* f, uint32_t size, uint32_t alignment) {
	TB_Register r = tb_make_reg(f, TB_LOCAL, TB_TYPE_PTR());
	f->nodes[r].local.alignment = alignment;
	f->nodes[r].local.size = size;
	return r;
}

TB_API TB_Register tb_inst_load(TB_Function* f, TB_DataType dt, TB_Register addr, uint32_t alignment) {
	assert(f->current_label);
	loop_range(i, f->current_label, f->count) {
		if (f->nodes[i].type == TB_LOAD &&
			memcmp(&f->nodes[i].dt, &dt, sizeof(TB_DataType)) == 0 &&
			f->nodes[i].load.address == addr &&
			f->nodes[i].load.alignment == alignment) {
			return i;
		}
	}
    
	TB_Register r = tb_make_reg(f, TB_LOAD, dt);
	f->nodes[r].load.address = addr;
	f->nodes[r].load.alignment = alignment;
	return r;
}

TB_API TB_Register tb_inst_store(TB_Function* f, TB_DataType dt, TB_Register addr, TB_Register val, uint32_t alignment) {
    assert(f->current_label);
	loop_range(i, f->current_label, f->count) {
		if (f->nodes[i].type == TB_STORE &&
			memcmp(&f->nodes[i].dt, &dt, sizeof(TB_DataType)) == 0 &&
			f->nodes[i].store.address == addr &&
			f->nodes[i].store.value == val &&
			f->nodes[i].store.alignment == alignment) {
			return i;
		}
	}
    
	TB_Register r = tb_make_reg(f, TB_STORE, dt);
	f->nodes[r].store.address = addr;
	f->nodes[r].store.value = val;
	f->nodes[r].store.alignment = alignment;
	return r;
}

TB_API TB_Register tb_inst_iconst(TB_Function* f, TB_DataType dt, uint64_t imm) {
	assert(f->current_label);
	loop_range(i, f->current_label, f->count) {
		if (f->nodes[i].type == TB_INT_CONST &&
			memcmp(&f->nodes[i].dt, &dt, sizeof(TB_DataType)) == 0 &&
			f->nodes[i].i_const.lo == imm &&
			f->nodes[i].i_const.hi == 0) {
			return i;
		}
	}
    
	TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
	f->nodes[r].i_const.hi = 0;
	f->nodes[r].i_const.lo = imm;
	return r;
}

TB_API TB_Register tb_inst_iconst128(TB_Function* f, TB_DataType dt, TB_Int128 imm) {
	if (dt.type == TB_I128) {
		TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
		f->nodes[r].i_const.lo = imm.lo;
		f->nodes[r].i_const.hi = imm.hi;
		return r;
	}
	else if (dt.type == TB_I64) {
		// Truncate
		TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
		f->nodes[r].i_const.lo = imm.lo;
		f->nodes[r].i_const.hi = 0;
		return r;
	}
	else if (dt.type == TB_I32) {
		// Truncate
		TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
		f->nodes[r].i_const.lo = imm.lo & 0xFFFFFFFFull;
		f->nodes[r].i_const.hi = 0;
		return r;
	}
	else if (dt.type == TB_I16) {
		// Truncate
		TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
		f->nodes[r].i_const.lo = imm.lo & 0xFFFFull;
		f->nodes[r].i_const.hi = 0;
		return r;
	}
	else if (dt.type == TB_I8) {
		// Truncate
		TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
		f->nodes[r].i_const.lo = imm.lo & 0xFFull;
		f->nodes[r].i_const.hi = 0;
		return r;
	}
	else tb_unreachable();
}

TB_API TB_Register tb_inst_fconst(TB_Function* f, TB_DataType dt, double imm) {
	TB_Register r = tb_make_reg(f, TB_FLOAT_CONST, dt);
	f->nodes[r].f_const = imm;
	return r;
}

TB_API TB_Register tb_inst_array_access(TB_Function* f, TB_Register base, TB_Register index, uint32_t stride) {
	TB_Register r = tb_make_reg(f, TB_ARRAY_ACCESS, TB_TYPE_PTR());
	f->nodes[r].array_access.base = base;
	f->nodes[r].array_access.index = index;
	f->nodes[r].array_access.stride = stride;
	return r;
}

TB_API TB_Register tb_inst_member_access(TB_Function* f, TB_Register base, int32_t offset) {
	TB_Register r = tb_make_reg(f, TB_MEMBER_ACCESS, TB_TYPE_PTR());
	f->nodes[r].member_access.base = base;
	f->nodes[r].member_access.offset = offset;
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
	f->nodes[r].call.target = target;
	f->nodes[r].call.param_start = param_start;
	f->nodes[r].call.param_end = param_end;
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
	if (f->nodes[a].type == TB_INT_CONST) tb_swap(a, b);
    
	// TODO(NeGate): Fix the constant folding to handle i128
	if (f->nodes[a].type == TB_INT_CONST && f->nodes[b].type == TB_INT_CONST) return tb_inst_iconst128(f, dt, tb_emulate_add(f, arith_behavior, dt, f->nodes[a].i_const, f->nodes[b].i_const));
	else if (f->nodes[b].type == TB_INT_CONST && f->nodes[b].i_const.lo == 0) return a;
	else if (f->nodes[a].type == TB_ADD) return tb_inst_add(f, dt, f->nodes[a].i_arith.a, tb_inst_add(f, dt, f->nodes[a].i_arith.b, b, arith_behavior), arith_behavior);
    
	return tb_cse_arith(f, TB_ADD, dt, arith_behavior, a, b);
}

TB_API TB_Register tb_inst_sub(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
	return tb_cse_arith(f, TB_SUB, dt, arith_behavior, a, b);
}

TB_API TB_Register tb_inst_mul(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
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
	f->nodes[r].f_arith.a = a;
	f->nodes[r].f_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_fsub(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_FSUB, dt);
	f->nodes[r].f_arith.a = a;
	f->nodes[r].f_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_fmul(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_FMUL, dt);
	f->nodes[r].f_arith.a = a;
	f->nodes[r].f_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_fdiv(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_FDIV, dt);
	f->nodes[r].f_arith.a = a;
	f->nodes[r].f_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_cmp_eq(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_EQ, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = a;
	f->nodes[r].cmp.b = b;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ne(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_NE, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = a;
	f->nodes[r].cmp.b = b;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_slt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_SLT, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = a;
	f->nodes[r].cmp.b = b;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_sle(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_SLE, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = a;
	f->nodes[r].cmp.b = b;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_sgt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_SLT, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = b;
	f->nodes[r].cmp.b = a;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_sge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_SLE, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = b;
	f->nodes[r].cmp.b = a;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ult(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_ULT, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = a;
	f->nodes[r].cmp.b = b;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ule(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_ULE, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = a;
	f->nodes[r].cmp.b = b;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ugt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_ULT, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = b;
	f->nodes[r].cmp.b = a;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_uge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_ULE, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = b;
	f->nodes[r].cmp.b = a;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_flt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLT, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = a;
	f->nodes[r].cmp.b = b;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_fle(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLE, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = a;
	f->nodes[r].cmp.b = b;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_fgt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLT, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = b;
	f->nodes[r].cmp.b = a;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_fge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLE, TB_TYPE_BOOL(1));
	f->nodes[r].cmp.a = b;
	f->nodes[r].cmp.b = a;
	f->nodes[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_phi2(TB_Function* f, TB_DataType dt, TB_Label a_label, TB_Register a, TB_Label b_label, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_PHI2, dt);
	f->nodes[r].phi2.a_label = tb_find_reg_from_label(f, a_label);
	f->nodes[r].phi2.a = a;
	f->nodes[r].phi2.b_label = tb_find_reg_from_label(f, b_label);
	f->nodes[r].phi2.b = b;
	return r;
}

TB_API TB_Register tb_inst_label(TB_Function* f, TB_Label id) {
	if (f->count + 1 < f->capacity) {
		f->capacity *= 2;
		f->nodes = realloc(f->nodes, f->capacity * sizeof(TB_Node));
	}
    
	TB_Register r = f->count++;
	f->nodes[r].type = TB_LABEL;
	f->nodes[r].dt = TB_TYPE_PTR();
	f->nodes[r].label.id = id;
	f->nodes[r].label.terminator = TB_NULL_REG;
	f->nodes[r].label.is_loop = false;
    
	if (f->current_label) {
		f->nodes[f->current_label].label.terminator = r;
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
	f->nodes[r].goto_.label = id;
    
	assert(f->current_label);
	f->nodes[f->current_label].label.terminator = r;
	f->current_label = TB_NULL_REG;
}

TB_API TB_Register tb_inst_if(TB_Function* f, TB_Register cond, TB_Label if_true, TB_Label if_false) {
	TB_Register r = tb_make_reg(f, TB_IF, TB_TYPE_VOID());
	f->nodes[r].if_.cond = cond;
	f->nodes[r].if_.if_true = if_true;
	f->nodes[r].if_.if_false = if_false;
    
	assert(f->current_label);
	f->nodes[f->current_label].label.terminator = r;
	f->current_label = TB_NULL_REG;
	return r;
}

TB_API void tb_inst_ret(TB_Function* f, TB_DataType dt, TB_Register value) {
	TB_Register r = tb_make_reg(f, TB_RET, dt);
	f->nodes[r].ret.value = value;
    
	assert(f->current_label);
	f->nodes[f->current_label].label.terminator = r;
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
        default:        tb_unreachable();
	}
}

TB_API void tb_function_print(TB_Function* f) {
	printf("%s():\n", f->name);
    
	loop(i, f->count) {
		enum TB_RegisterType type = f->nodes[i].type;
		TB_DataType dt = f->nodes[i].dt;
        
		switch (type) {
            case TB_NULL:
			//printf("  r%u\t=\t", i);
			//printf(" ???\n");
			break;
            case TB_INT_CONST:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
            
			if (f->nodes[i].i_const.hi) {
				printf(" %llx%llx\n", f->nodes[i].i_const.hi, f->nodes[i].i_const.lo);
			}
			else {
				printf(" %lld\n", f->nodes[i].i_const.lo);
			}
			break;
            case TB_FLOAT_CONST:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
            printf(" %f\n", f->nodes[i].f_const);
			break;
            case TB_ZERO_EXT:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" ZXT r%u\n", f->nodes[i].ext);
			break;
            case TB_SIGN_EXT:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" SXT r%u\n", f->nodes[i].ext);
			break;
			case TB_MEMBER_ACCESS:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" &r%u[r%d]\n", f->nodes[i].member_access.base, f->nodes[i].member_access.offset);
			break;
			case TB_ARRAY_ACCESS:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" &r%u[r%u * %u]\n", f->nodes[i].array_access.base, f->nodes[i].array_access.index, f->nodes[i].array_access.stride);
			break;
			case TB_AND:
            case TB_OR:
			case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_UDIV:
            case TB_SDIV:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" r%u ", f->nodes[i].i_arith.a);
            
			switch (type) {
                case TB_AND: printf("&"); break;
                case TB_OR: printf("|"); break;
                case TB_ADD: printf("+"); break;
                case TB_SUB: printf("-"); break;
                case TB_MUL: printf("*"); break;
                case TB_UDIV: printf("/u"); break;
                case TB_SDIV: printf("/s"); break;
                default: tb_unreachable();
			}
            
			printf(" r%u\n", f->nodes[i].i_arith.b);
			break;
            case TB_FADD:
            case TB_FSUB:
            case TB_FMUL:
            case TB_FDIV:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" r%u ", f->nodes[i].f_arith.a);
            
			switch (type) {
                case TB_FADD: printf("+"); break;
                case TB_FSUB: printf("-"); break;
                case TB_FMUL: printf("*"); break;
                case TB_FDIV: printf("/"); break;
                default: tb_unreachable();
			}
            
			printf(" r%u\n", f->nodes[i].f_arith.b);
			break;
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" r%u ", f->nodes[i].cmp.a);
            
			switch (type) {
                case TB_CMP_NE: printf("!="); break;
                case TB_CMP_EQ: printf("=="); break;
                case TB_CMP_ULT: printf("<"); break;
                case TB_CMP_ULE: printf("<="); break;
                case TB_CMP_SLT: printf("<"); break;
                case TB_CMP_SLE: printf("<="); break;
                default: tb_unreachable();
			}
            
			printf(" r%u", f->nodes[i].cmp.b);
			
			if (type == TB_CMP_SLT || type == TB_CMP_SLE) printf(" # signed\n");
			else printf("\n");
			break;
            case TB_LOCAL:
			printf("  r%u\t=\tLOCAL %d (%d align)\n", i, f->nodes[i].local.size, f->nodes[i].local.alignment);
			break;
            case TB_CALL:
			printf("  r%u\t=\tCALL %s(", i, f->nodes[i].call.target->name);
			for (size_t j = f->nodes[i].call.param_start; j < f->nodes[i].call.param_end; j++) {
				if (j != f->nodes[i].call.param_start) printf(", ");
				
				printf("r%u", f->vla.data[j]);
			}
			printf(")\n");
			break;
            case TB_PARAM:
			printf("  r%u\t=\tPARAM %u\n", i, f->nodes[i].param.id);
			break;
            case TB_PARAM_ADDR:
			printf("  r%u\t=\t&PARAM %u\n", i, f->nodes[f->nodes[i].param_addr.param].param.id);
			break;
            case TB_LOAD:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" *r%u (%d align)\n", f->nodes[i].load.address, f->nodes[i].load.alignment);
			break;
            case TB_STORE:
			printf(" *r%u \t=\t", f->nodes[i].store.address);
			tb_print_type(dt);
			printf(" r%u (%d align)\n", f->nodes[i].store.value, f->nodes[i].store.alignment);
			break;
            case TB_LABEL:
			printf("L%d: # r%u terminates at r%u\n", f->nodes[i].label.id, i, f->nodes[i].label.terminator);
			break;
            case TB_GOTO:
			printf("  goto L%d\n", f->nodes[i].goto_.label);
			break;
            case TB_IF:
			printf("  if (r%u)\tL%d else L%d\n", f->nodes[i].if_.cond, f->nodes[i].if_.if_true, f->nodes[i].if_.if_false);
			break;
            case TB_PASS:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" PASS r%u\n", f->nodes[i].pass);
			break;
            case TB_PHI1:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" PHI L%d:r%u\n", f->nodes[f->nodes[i].phi1.a_label].label.id, f->nodes[i].phi1.a);
			break;
            case TB_PHI2:
			printf("  r%u\t=\t", i);
			tb_print_type(dt);
			printf(" PHI L%d:r%u, L%d:r%u\n", f->nodes[f->nodes[i].phi2.a_label].label.id, f->nodes[i].phi2.a, f->nodes[f->nodes[i].phi2.b_label].label.id, f->nodes[i].phi2.b);
			break;
            case TB_RET:
			printf("  ret\t \t");
			tb_print_type(dt);
			printf(" r%u\n", f->nodes[i].i_arith.a);
			break;
            default: tb_unreachable();
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
		if (o->data == NULL) tb_unreachable();
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

