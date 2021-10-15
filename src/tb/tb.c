#define TB_INTERNAL
#include "tb.h"

static uint8_t tb_temporary_storage[TB_TEMPORARY_STORAGE_SIZE * TB_MAX_THREADS];
static uint8_t* tb_thread_storage;
static atomic_uint tb_used_tls_slots;

TB_API TB_Module* tb_module_create(TB_Arch target_arch, TB_System target_system, const TB_FeatureSet* features) {
	TB_Module* m = malloc(sizeof(TB_Module));

	m->target_arch = target_arch;
	m->target_system = target_system;
	m->features = *features;

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

	clock_t t1 = clock();
	if (optimization_level == TB_OPT_O0) {}
	else if (optimization_level == TB_OPT_O1) {
		loop(i, m->functions.count) {
			TB_Function* f = &m->functions.data[i];

			while (tb_opt_mem2reg(f) ||
				tb_opt_phi_cleanup(f) ||
				tb_opt_dce(f) ||
				tb_opt_cse(f)) {
			}
		}
	}
	else if (optimization_level == TB_OPT_SIZE) {
		loop(i, m->functions.count) {
			TB_Function* f = &m->functions.data[i];

			while (tb_opt_mem2reg(f) ||
				tb_opt_phi_cleanup(f) ||
				tb_opt_dce(f) ||
				tb_opt_cse(f)) {
			}
		}
	}
	else if (optimization_level == TB_OPT_SPEED) {
		loop(i, m->functions.count) {
			TB_Function* f = &m->functions.data[i];

			while (tb_opt_mem2reg(f) ||
				tb_opt_phi_cleanup(f) ||
				tb_opt_loop_unroll(f) ||
				tb_opt_dce(f) ||
				tb_opt_cse(f)) {
			}
		}
	}
	clock_t t2 = clock();
	double delta_ms = 1000.0 * ((t2 - t1) / CLOCKS_PER_SEC);
	printf("optimizations took %f ms\n", delta_ms);

	switch (m->target_arch) {
	case TB_ARCH_X86_64:
		loop(i, m->functions.count) {
			m->compiled_functions.data[i] = x64_compile_function(&m->functions.data[i], &m->features);
		}
		break;
	case TB_ARCH_AARCH64:
		loop(i, m->functions.count) {
			m->compiled_functions.data[i] = aarch64_compile_function(&m->functions.data[i], &m->features);
		}
		break;
	default:
		printf("TinyBackend error: Unknown target!\n");
		abort();
	}
}

TB_API void tb_module_export(TB_Module* m, FILE* f) {
	switch (m->target_system) {
	case TB_SYSTEM_WINDOWS:
		tb_export_coff(m, m->target_arch, f);
		break;
	case TB_SYSTEM_LINUX:
		tb_export_elf64(m, m->target_arch, f);
		break;
	default:
		printf("TinyBackend error: Unknown system!\n");
		abort();
	}
}

TB_API TB_Function* tb_function_create(TB_Module* m, const char* name) {
	assert(m->functions.count < TB_MAX_FUNCTIONS);

	TB_Function* f = &m->functions.data[m->functions.count++];
	f->name = malloc(strlen(name) + 1);
	strcpy(f->name, name);

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
		if (slot >= TB_MAX_THREADS) abort();

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

//
// IR BUILDER
// 
// Handles generating the TB_Function IR via C functions. 
// Note that these functions can perform certain simple
// optimizations while the generation happens to improve
// the machine code output or later analysis stages.
//
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
	case TB_I8:		mask  = 0xFFull; break;
	case TB_I16:	mask = 0xFFFFull; break;
	case TB_I32:	mask = 0xFFFFFFFFull; break;
	case TB_I64:	mask = 0xFFFFFFFFFFFFFFFFull; break;
	case TB_I128:	mask = 0xFFFFFFFFFFFFFFFFull; break;
	default: abort();
	}

	switch (arith_behavior) {
	case TB_NO_WRAP:
		return (TB_Int128) { a.lo + b.lo };
	case TB_WRAP_CHECK: {
		// TODO(NeGate): Throw runtime error in this scenario 
		// if the value is out of bounds
		abort();
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
	default: abort();
	}
}

static TB_Register tb_cse_arith(TB_Function* f, int type, TB_DataType dt, TB_ArithmaticBehavior arith_behavior, TB_Register a, TB_Register b) {
	assert(f->current_label);
	size_t label_start = f->current_label;

	for (size_t i = label_start; i < f->count; i++) {
		if (memcmp(&f->nodes[i].dt, &dt, sizeof(TB_DataType)) == 0
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

TB_API TB_Register tb_inst_param(TB_Function* f, TB_DataType dt) {
	TB_Register r = tb_make_reg(f, TB_PARAM, dt);
	f->nodes[r].param.id = f->parameter_count++;

	switch (dt.type) {
	case TB_I8:  f->nodes[r].param.size = 1; break;
	case TB_I16: f->nodes[r].param.size = 2; break;
	case TB_I32: f->nodes[r].param.size = 4; break;
	case TB_I64: f->nodes[r].param.size = 8; break;
	default: abort();
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

	// NOTE(NeGate): The position value provided here are only hints, they can be ignored.
	f->nodes[r].local.position = f->locals_stack_usage;
	f->nodes[r].local.size = size;

	f->locals_stack_usage += size;

	uint32_t padding = (alignment - (f->locals_stack_usage % alignment)) % alignment;
	f->locals_stack_usage += padding;

	return r;
}

TB_API TB_Register tb_inst_load(TB_Function* f, TB_DataType dt, TB_Register addr, uint32_t alignment) {
	assert(f->current_label);
	for (size_t i = f->current_label; i < f->count; i++) {
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
	for (size_t i = f->current_label; i < f->count; i++) {
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
	for (size_t i = f->current_label; i < f->count; i++) {
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
	else abort();
}

TB_API TB_Register tb_inst_fconst(TB_Function* f, TB_DataType dt, double imm) {
	TB_Register r = tb_make_reg(f, TB_FLOAT_CONST, dt);
	f->nodes[r].f_const = imm;
	return r;
}

TB_API TB_Register tb_inst_add(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
	if (f->nodes[a].type == TB_INT_CONST) tb_swap(a, b);

	// TODO: Fix the constant folding to handle i128
	if (f->nodes[a].type == TB_INT_CONST && f->nodes[b].type == TB_INT_CONST) return tb_inst_iconst128(f, dt, tb_emulate_add(f, arith_behavior, dt, f->nodes[a].i_const, f->nodes[b].i_const));
	else if (f->nodes[b].type == TB_INT_CONST && f->nodes[b].i_const.lo == 0) return a;
	else if (f->nodes[a].type == TB_ADD) return tb_inst_add(f, dt, f->nodes[a].i_arith.a, tb_inst_add(f, dt, f->nodes[a].i_arith.b, b, arith_behavior), arith_behavior);

	return tb_cse_arith(f, TB_ADD, dt, arith_behavior, a, b);
}

TB_API TB_Register tb_inst_sub(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
	TB_Register r = tb_make_reg(f, TB_SUB, dt);
	f->nodes[r].i_arith.arith_behavior = arith_behavior;
	f->nodes[r].i_arith.a = a;
	f->nodes[r].i_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_mul(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
	return tb_cse_arith(f, TB_MUL, dt, arith_behavior, a, b);
}

TB_API TB_Register tb_inst_udiv(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_UDIV, dt);
	f->nodes[r].i_arith.arith_behavior = TB_NO_WRAP;
	f->nodes[r].i_arith.a = a;
	f->nodes[r].i_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_sdiv(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_SDIV, dt);
	f->nodes[r].i_arith.arith_behavior = TB_NO_WRAP;
	f->nodes[r].i_arith.a = a;
	f->nodes[r].i_arith.b = b;
	return r;
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

TB_API TB_Register tb_inst_goto(TB_Function* f, TB_Label id) {
	TB_Register r = tb_make_reg(f, TB_GOTO, TB_TYPE_VOID(0));
	f->nodes[r].goto_.label = id;

	assert(f->current_label);
	f->nodes[f->current_label].label.terminator = r;
	f->current_label = TB_NULL_REG;
	return r;
}

TB_API TB_Register tb_inst_if(TB_Function* f, TB_Register cond, TB_Label if_true, TB_Label if_false) {
	TB_Register r = tb_make_reg(f, TB_IF, TB_TYPE_VOID(0));
	f->nodes[r].if_.cond = cond;
	f->nodes[r].if_.if_true = if_true;
	f->nodes[r].if_.if_false = if_false;

	assert(f->current_label);
	f->nodes[f->current_label].label.terminator = r;
	f->current_label = TB_NULL_REG;
	return r;
}

TB_API TB_Register tb_inst_ret(TB_Function* f, TB_DataType dt, TB_Register value) {
	TB_Register r = tb_make_reg(f, TB_RET, dt);
	f->nodes[r].ret.value = value;

	assert(f->current_label);
	f->nodes[f->current_label].label.terminator = r;
	f->current_label = TB_NULL_REG;

	return r;
}

//
// IR PRINTER
//
static void tb_print_type(TB_DataType dt) {
	switch (dt.type) {
	case TB_BOOL:   printf("[bool x %d]\t", dt.count); break;
	case TB_I8:     printf("[i8 x %d]\t", dt.count); break;
	case TB_I16:    printf("[i16 x %d]\t", dt.count); break;
	case TB_I32:    printf("[i32 x %d]\t", dt.count); break;
	case TB_I64:    printf("[i64 x %d]\t", dt.count); break;
	case TB_I128:   printf("[i128 x %d]\t", dt.count); break;
	case TB_PTR:    printf("[ptr x %d]\t", dt.count); break;
	case TB_F32:    printf("[f32 x %d]\t", dt.count); break;
	case TB_F64:    printf("[f64 x %d]\t", dt.count); break;
	default:        abort();
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
			printf("  r%llu\t=\t", i);
			tb_print_type(dt);

			if (f->nodes[i].i_const.hi) {
				printf(" %llx%llx\n", f->nodes[i].i_const.hi, f->nodes[i].i_const.lo);
			}
			else {
				printf(" %lld\n", f->nodes[i].i_const.lo);
			}
			break;
		case TB_ADD:
		case TB_SUB:
		case TB_MUL:
		case TB_UDIV:
		case TB_SDIV:
			printf("  r%llu\t=\t", i);
			tb_print_type(dt);
			printf(" r%llu ", f->nodes[i].i_arith.a);

			switch (type) {
			case TB_ADD: printf("+"); break;
			case TB_SUB: printf("-"); break;
			case TB_MUL: printf("*"); break;
			case TB_UDIV: printf("/u"); break;
			case TB_SDIV: printf("/s"); break;
			default: abort();
			}

			printf(" r%llu\n", f->nodes[i].i_arith.b);
			break;
		case TB_CMP_EQ:
		case TB_CMP_NE:
		case TB_CMP_ULT:
		case TB_CMP_ULE:
			printf("  r%llu\t=\t", i);
			tb_print_type(dt);
			printf(" r%llu ", f->nodes[i].cmp.a);

			switch (type) {
			case TB_CMP_NE: printf("!="); break;
			case TB_CMP_EQ: printf("=="); break;
			case TB_CMP_ULT: printf("<"); break;
			case TB_CMP_ULE: printf("<="); break;
			default: abort();
			}

			printf(" r%llu\n", f->nodes[i].cmp.b);
			break;
		case TB_LOCAL:
			printf("  r%llu\t=\tLOCAL %d (%d align)\n", i, f->nodes[i].local.size, f->nodes[i].local.alignment);
			break;
		case TB_PARAM:
			printf("  r%llu\t=\tPARAM %u\n", i, f->nodes[i].param.id);
			break;
		case TB_PARAM_ADDR:
			printf("  r%llu\t=\t&PARAM %u\n", i, f->nodes[f->nodes[i].param_addr.param].param.id);
			break;
		case TB_LOAD:
			printf("  r%llu\t=\t", i);
			tb_print_type(dt);
			printf(" *r%llu (%d align)\n", f->nodes[i].load.address, f->nodes[i].load.alignment);
			break;
		case TB_STORE:
			printf(" *r%llu \t=\t", f->nodes[i].store.address);
			tb_print_type(dt);
			printf(" r%llu (%d align)\n", f->nodes[i].store.value, f->nodes[i].store.alignment);
			break;
		case TB_LABEL:
			printf("L%d: # r%llu terminates at r%llu\n", f->nodes[i].label.id, i, f->nodes[i].label.terminator);
			break;
		case TB_GOTO:
			printf("  goto L%d\n", f->nodes[i].goto_.label);
			break;
		case TB_IF:
			printf("  if (r%llu)\tL%d else L%d\n", f->nodes[i].if_.cond, f->nodes[i].if_.if_true, f->nodes[i].if_.if_false);
			break;
		case TB_PASS:
			printf("  r%llu\t=\t", i);
			tb_print_type(dt);
			printf(" PASS r%llu\n", f->nodes[i].pass);
			break;
		case TB_PHI1:
			printf("  r%llu\t=\t", i);
			tb_print_type(dt);
			printf(" PHI L%d:r%llu\n", f->nodes[f->nodes[i].phi1.a_label].label.id, f->nodes[i].phi1.a);
			break;
		case TB_PHI2:
			printf("  r%llu\t=\t", i);
			tb_print_type(dt);
			printf(" PHI L%d:r%llu, L%d:r%llu\n", f->nodes[f->nodes[i].phi2.a_label].label.id, f->nodes[i].phi2.a, f->nodes[f->nodes[i].phi2.b_label].label.id, f->nodes[i].phi2.b);
			break;
		case TB_RET:
			printf("  ret\t \t");
			tb_print_type(dt);
			printf(" r%llu\n", f->nodes[i].i_arith.a);
			break;
		default: abort();
		}
	}
}

//
// EMITTER CODE
// 
// Simple linear allocation for the backend's to output code with
//
static void tb_out_reserve(TB_Emitter* o, size_t count) {
	if (o->count + count >= o->capacity) {
		if (o->capacity == 0) {
			o->capacity = 64;
		}
		else {
			o->capacity += count;
			o->capacity *= 2;
		}

		o->data = realloc(o->data, o->capacity);
		if (o->data == NULL) abort();
	}
}

static void tb_out1b_UNSAFE(TB_Emitter* o, uint8_t i) {
	assert(o->count + 1 < o->capacity);

	o->data[o->count] = i;
	o->count += 1;
}

static void tb_out1b(TB_Emitter* o, uint8_t i) {
	tb_out_reserve(o, 1);

	o->data[o->count] = i;
	o->count += 1;
}

static void tb_out2b(TB_Emitter* o, uint16_t i) {
	tb_out_reserve(o, 2);

	*((uint16_t*)&o->data[o->count]) = i;
	o->count += 2;
}

static void tb_out4b(TB_Emitter* o, uint32_t i) {
	tb_out_reserve(o, 4);

	*((uint32_t*)&o->data[o->count]) = i;
	o->count += 4;
}

static void tb_out8b(TB_Emitter* o, uint64_t i) {
	tb_out_reserve(o, 8);

	*((uint64_t*)&o->data[o->count]) = i;
	o->count += 8;
}

static void tb_outstr_UNSAFE(TB_Emitter* o, const char* str) {
	while (*str) o->data[o->count++] = *str++;
}

static void tb_outs_UNSAFE(TB_Emitter* o, size_t len, const uint8_t* str) {
	for (size_t i = 0; i < len; i++) o->data[o->count + i] = str[i];
	o->count += len;
}

