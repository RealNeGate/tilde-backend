// IR BUILDER
// 
// Handles generating the TB_Function IR via C functions. 
// Note that these functions can perform certain simple
// optimizations while the generation happens to improve
// the machine code output or later analysis stages.
#include "tb_internal.h"

// Used for some optimizations
#if TB_HOST_ARCH == TB_HOST_X86_64
#include <x86intrin.h>
#endif

TB_API TB_DataType tb_node_get_data_type(TB_Function* f, TB_Register r) {
	assert(r < f->nodes.count);
	return f->nodes.dt[r];
}

TB_API void tb_get_function_get_local_info(TB_Function* f, TB_Register r, int* size, int* align) {
	assert(f->nodes.type[r] == TB_LOCAL);
	
	*size = f->nodes.payload[r].local.size;
	*align = f->nodes.payload[r].local.alignment;
}

TB_API bool tb_node_is_conditional(TB_Function* f, TB_Register r) {
	return f->nodes.type[r] == TB_IF;
}

TB_API bool tb_node_is_terminator(TB_Function* f, TB_Register r) {
	return f->nodes.type[r] == TB_IF ||
		f->nodes.type[r] == TB_GOTO ||
		f->nodes.type[r] == TB_RET ||
		f->nodes.type[r] == TB_LABEL;
}

TB_API bool tb_node_is_label(TB_Function* f, TB_Register r) {
	return f->nodes.type[r] == TB_LABEL;
}

TB_API TB_Register tb_node_get_last_register(TB_Function* f) {
	return f->nodes.count - 1;
}

TB_API TB_Register tb_node_load_get_address(TB_Function* f, TB_Register r) {
	assert(f->nodes.type[r] == TB_LOAD);
	
	return f->nodes.payload[r].load.address;
}

TB_API TB_Register tb_node_arith_get_left(TB_Function* f, TB_Register r) {
	assert(f->nodes.type[r] >= TB_AND && f->nodes.type[r] <= TB_CMP_FLE);
	
	// TODO(NeGate): They share position in the union
	return f->nodes.payload[r].i_arith.a;
}

TB_API TB_Register tb_node_arith_get_right(TB_Function* f, TB_Register r) {
	assert(f->nodes.type[r] >= TB_AND && f->nodes.type[r] <= TB_CMP_FLE);
	
	// TODO(NeGate): They share position in the union
	return f->nodes.payload[r].i_arith.b;
}

static TB_Register tb_make_reg(TB_Function* f, int type, TB_DataType dt) {
	// Cannot add registers to terminated basic blocks, except labels
	// which start new basic blocks
	assert(type == TB_LABEL || f->current_label);
    
	if (f->nodes.count + 1 >= f->nodes.capacity) {
		tb_resize_node_stream(f, tb_next_pow2(f->nodes.count + 1));
	}
	
	TB_Register r = f->nodes.count++;
	f->nodes.type[r] = type;
	f->nodes.dt[r] = dt;
	return r;
}

uint64_t tb_fold_add(TB_ArithmaticBehavior ab, TB_DataType dt, uint64_t a, uint64_t b) {
	uint64_t shift = 64 - (8 << (dt.type - TB_I8));
	uint64_t mask = (~0ull) >> shift;
	
	uint64_t sum;
	if (__builtin_add_overflow(a << shift, b << shift, &sum)) {
		sum >>= shift;
		
		if (ab == TB_CAN_WRAP) sum &= mask;
		else if (ab == TB_SATURATED_UNSIGNED) sum = mask;
		//else if (ab == TB_WRAP_CHECK) { printf("warp check!!!\n"); }
	}
	
	sum = (sum >> shift) & mask;
	return sum;
}

uint64_t tb_fold_sub(TB_ArithmaticBehavior ab, TB_DataType dt, uint64_t a, uint64_t b) {
	uint64_t shift = 64 - (8 << (dt.type - TB_I8));
	uint64_t mask = (~0ull) >> shift;
	
	uint64_t sum;
	if (__builtin_sub_overflow(a << shift, b << shift, &sum)) {
		sum = (sum >> shift) & mask;
		
		if (ab == TB_CAN_WRAP) sum &= mask;
		else if (ab == TB_SATURATED_UNSIGNED) sum = 0;
		//else if (ab == TB_WRAP_CHECK) { printf("warp check!!!\n"); }
	} else {
		sum = (sum >> shift) & mask;
	}
	
	return sum;
}

uint64_t tb_fold_mul(TB_ArithmaticBehavior ab, TB_DataType dt, uint64_t a, uint64_t b) {
	uint64_t shift = 64 - (8 << (dt.type - TB_I8));
	uint64_t mask = (~0ull) >> shift;
	
	uint64_t sum;
	if (__builtin_mul_overflow(a << shift, b << shift, &sum)) {
		sum = (sum >> shift) & mask;
		
		if (ab == TB_CAN_WRAP) sum &= mask;
		else if (ab == TB_SATURATED_UNSIGNED) sum = 0;
		//else if (ab == TB_WRAP_CHECK) { printf("warp check!!!\n"); }
	} else {
		sum = (sum >> shift) & mask;
	}
	
	return sum;
}

uint64_t tb_fold_div(TB_DataType dt, uint64_t a, uint64_t b) {
	uint64_t shift = 64 - (8 << (dt.type - TB_I8));
	uint64_t mask = (~0ull) >> shift;
	
	if (b == 0) return (uint64_t) { 0 };
	uint64_t q = (a << shift) / (b << shift);
	
	return (q >> shift) & mask; 
}

// literally only used in `tb_cse_arith` i just don't wanna
// inline it, readability wise at least.
inline static bool tb_match_iarith_payload(const TB_RegPayload* a, const TB_RegPayload* b) {
	__m128i a128 = _mm_load_si128((__m128i*)a);
	__m128i b128 = _mm_load_si128((__m128i*)b);
	
	return _mm_movemask_epi8(_mm_cmpeq_epi8(a128, b128)) == 0xFFF;
}

static TB_Register tb_cse_arith(TB_Function* f, int type, TB_DataType dt, TB_ArithmaticBehavior arith_behavior, TB_Register a, TB_Register b) {
	assert(f->current_label);
	
#if TB_FRONTEND_OPT
#if TB_HOST_ARCH == TB_HOST_X86_64
	size_t aligned_start = ((f->current_label + 15) / 16) * 16;
	__m128i pattern = _mm_set1_epi8(type);
	
	for (size_t i = aligned_start; i < f->nodes.count; i += 16) {
		__m128i bytes = _mm_load_si128((__m128i*)&f->nodes.type[i]);
		unsigned int mask = _mm_movemask_epi8(_mm_cmpeq_epi8(bytes, pattern));
		if (mask == 0) continue;
		
		// this one is guarentee to not be zero so it's fine
		// to not check that FFS.
		size_t offset = __builtin_ffs(mask) - 1;
		
		size_t j = i + offset;
		// skip over the mask bit for the next iteration
		mask >>= (offset + 1);
		
		// We know it loops at least once by this point
		do {
			assert(f->nodes.type[j] == type);
			if (TB_DATA_TYPE_EQUALS(f->nodes.dt[j], dt)
				&& f->nodes.payload[j].i_arith.arith_behavior == arith_behavior
				&& f->nodes.payload[j].i_arith.a == a
				&& f->nodes.payload[j].i_arith.b == b) {
				return j;
			}
			
			size_t ffs = __builtin_ffs(mask);
			if (ffs == 0) break;
			
			// skip over the mask bit for the next iteration
			mask >>= ffs;
			j += ffs;
		} while (true);
	}
#else
	for (size_t i = f->current_label + 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == type
			&& TB_DATA_TYPE_EQUALS(f->nodes.dt[i], dt)
			&& f->nodes.payload[i].i_arith.arith_behavior == arith_behavior
			&& f->nodes.payload[i].i_arith.a == a
			&& f->nodes.payload[i].i_arith.b == b) {
			return i;
		}
	}
#endif
#endif
	
	TB_Register r = tb_make_reg(f, type, dt);
	f->nodes.payload[r].i_arith.arith_behavior = arith_behavior;
	f->nodes.payload[r].i_arith.a = a;
	f->nodes.payload[r].i_arith.b = b;
	return r;
}

TB_API TB_Register tb_inst_trunc(TB_Function* f, TB_Register src, TB_DataType dt) {
	assert(f->current_label);
	
#if TB_FRONTEND_OPT
	for (size_t i = f->current_label + 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_TRUNCATE
			&& TB_DATA_TYPE_EQUALS(f->nodes.dt[i], dt)
			&& f->nodes.payload[i].trunc == src) {
			return i;
		}
	}
#endif
	
	TB_Register r = tb_make_reg(f, TB_TRUNCATE, dt);
	f->nodes.payload[r].trunc = src;
	return r;
}

TB_API TB_Register tb_inst_sxt(TB_Function* f, TB_Register src, TB_DataType dt) {
	assert(f->current_label);
	
#if TB_FRONTEND_OPT
	for (size_t i = f->current_label + 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_SIGN_EXT
			&& TB_DATA_TYPE_EQUALS(f->nodes.dt[i], dt)
			&& f->nodes.payload[i].ext == src) {
			return i;
		}
	}
#endif
	
	TB_Register r = tb_make_reg(f, TB_SIGN_EXT, dt);
	f->nodes.payload[r].ext = src;
	return r;
}

TB_API TB_Register tb_inst_zxt(TB_Function* f, TB_Register src, TB_DataType dt) {
	assert(f->current_label);
	
#if TB_FRONTEND_OPT
	for (size_t i = f->current_label + 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_ZERO_EXT
			&& TB_DATA_TYPE_EQUALS(f->nodes.dt[i], dt)
			&& f->nodes.payload[i].ext == src) {
			return i;
		}
	}
#endif
	
	TB_Register r = tb_make_reg(f, TB_ZERO_EXT, dt);
	f->nodes.payload[r].ext = src;
	return r;
}

TB_API TB_Register tb_inst_param(TB_Function* f, TB_DataType dt) {
	TB_Register r = tb_make_reg(f, TB_PARAM, dt);
	f->nodes.payload[r].param.id = f->parameter_count++;
	
	// TODO(NeGate): It's currently assuming that all pointers are 8bytes big,
	// which is untrue for some platforms.
	int param_size = 0;
	switch (dt.type) {
		case TB_I8:  param_size = 1; break;
		case TB_I16: param_size = 2; break;
		case TB_I32: param_size = 4; break;
		case TB_I64: param_size = 8; break;
		case TB_F32: param_size = 4; break;
		case TB_F64: param_size = 8; break;
		case TB_PTR: param_size = 8; break;
		default: break;
	}
	
	assert(param_size);
	assert(dt.count > 0);
	f->nodes.payload[r].param.size = param_size * dt.count;
	return r;
}

TB_API void tb_inst_loc(TB_Function* f, TB_FileID file, int line) {
	TB_Register r = tb_make_reg(f, TB_LINE_INFO, TB_TYPE_VOID);
	f->nodes.payload[r].line_info.file = file;
	f->nodes.payload[r].line_info.line = line;
	f->nodes.payload[r].line_info.pos = 0;
	
	f->module->line_info_count++;
}

TB_API TB_Register tb_inst_param_addr(TB_Function* f, TB_Register param) {
	assert(f->nodes.type[param] == TB_PARAM);
	
	int param_size = f->nodes.payload[param].param.size;
	
	TB_Register r = tb_make_reg(f, TB_PARAM_ADDR, TB_TYPE_PTR);
	f->nodes.payload[r].param_addr.param = param;
	f->nodes.payload[r].param_addr.size = param_size;
	f->nodes.payload[r].param_addr.alignment = param_size;
	return r;
}

TB_API TB_Register tb_inst_local(TB_Function* f, uint32_t size, uint32_t alignment) {
	TB_Register r = tb_make_reg(f, TB_LOCAL, TB_TYPE_PTR);
	f->nodes.payload[r].local.alignment = alignment;
	f->nodes.payload[r].local.size = size;
	return r;
}

TB_API TB_Register tb_inst_load(TB_Function* f, TB_DataType dt, TB_Register addr, uint32_t alignment) {
	assert(f->current_label);
	
#if TB_FRONTEND_OPT
	for (size_t i = f->current_label + 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LOAD
			&& TB_DATA_TYPE_EQUALS(f->nodes.dt[i], dt)
			&& f->nodes.payload[i].load.address == addr
			&& f->nodes.payload[i].load.alignment == alignment) {
			return i;
		}
	}
#endif
	
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
	
#if TB_FRONTEND_OPT
#if TB_HOST_ARCH == TB_HOST_X86_64
	size_t aligned_start = ((f->current_label + 15) / 16) * 16;
	
	for (size_t i = aligned_start; i < f->nodes.count; i += 16) {
		__m128i bytes = _mm_load_si128((__m128i*)&f->nodes.type[i]);
		unsigned int mask = _mm_movemask_epi8(_mm_cmpeq_epi8(bytes, _mm_set1_epi8(TB_INT_CONST)));
		if (mask == 0) continue;
		
		// this one is guarentee to not be zero so it's fine
		// to not check that FFS.
		size_t offset = __builtin_ffs(mask) - 1;
		
		size_t j = i + offset;
		// skip over the mask bit for the next iteration
		mask >>= (offset + 1);
		
		// We know it loops at least once by this point
		do {
			assert(f->nodes.type[j] == type);
			if (TB_DATA_TYPE_EQUALS(f->nodes.dt[j], dt)
				&& f->nodes.payload[j].i_const.lo == imm
				&& f->nodes.payload[j].i_const.hi == 0) {
				return j;
			}
			
			size_t ffs = __builtin_ffs(mask);
			if (ffs == 0) break;
			
			// skip over the mask bit for the next iteration
			mask >>= ffs;
			j += ffs;
		} while (true);
	}
#else
	for (size_t i = f->current_label + 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_INT_CONST
			&& TB_DATA_TYPE_EQUALS(f->nodes.dt[i], dt)
			&& f->nodes.payload[i].i_const == imm) {
			return i;
		}
	}
#endif
#endif
	
	assert(dt.type == TB_BOOL || dt.type == TB_PTR || (dt.type >= TB_I8 && dt.type <= TB_I64));
	
	uint64_t mask = (~0ull) >> (64 - (8 << (dt.type - TB_I8)));
	((void)mask);
	assert((imm & mask) == imm);
	
	TB_Register r = tb_make_reg(f, TB_INT_CONST, dt);
	f->nodes.payload[r].i_const = imm;
	return r;
}

TB_API TB_Register tb_inst_fconst(TB_Function* f, TB_DataType dt, double imm) {
	TB_Register r = tb_make_reg(f, TB_FLOAT_CONST, dt);
	f->nodes.payload[r].f_const = imm;
	return r;
}

TB_API TB_Register tb_inst_array_access(TB_Function* f, TB_Register base, TB_Register index, uint32_t stride) {
	TB_Register r = tb_make_reg(f, TB_ARRAY_ACCESS, TB_TYPE_PTR);
	f->nodes.payload[r].array_access.base = base;
	f->nodes.payload[r].array_access.index = index;
	f->nodes.payload[r].array_access.stride = stride;
	return r;
}

TB_API TB_Register tb_inst_member_access(TB_Function* f, TB_Register base, int32_t offset) {
	TB_Register r = tb_make_reg(f, TB_MEMBER_ACCESS, TB_TYPE_PTR);
	f->nodes.payload[r].member_access.base = base;
	f->nodes.payload[r].member_access.offset = offset;
	return r;
}

TB_API TB_Register tb_inst_get_func_address(TB_Function* f, const TB_Function* target) {
	TB_Register r = tb_make_reg(f, TB_FUNC_ADDRESS, TB_TYPE_PTR);
	f->nodes.payload[r].func_addr = target;
	return r;
}

TB_API TB_Register tb_inst_get_extern_address(TB_Function* f, TB_ExternalID target) {
	TB_Register r = tb_make_reg(f, TB_EFUNC_ADDRESS, TB_TYPE_PTR);
	f->nodes.payload[r].efunc_addr = target;
	return r;
}

TB_API TB_Register tb_inst_call(TB_Function* f, TB_DataType dt, const TB_Function* target, size_t param_count, const TB_Register* params) {
	// Reserve space for the arguments
	if (f->vla.count + param_count >= f->vla.capacity) {
		// TODO(NeGate): This might be excessive for this array, idk :P
		f->vla.capacity = tb_next_pow2(f->vla.count + param_count);
		f->vla.data = tb_platform_heap_realloc(f->vla.data, f->vla.capacity * sizeof(TB_Register));
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

TB_API TB_Register tb_inst_vcall(TB_Function* f, TB_DataType dt, const TB_Register target, size_t param_count, const TB_Register* params) {
	// Reserve space for the arguments
	if (f->vla.count + param_count >= f->vla.capacity) {
		// TODO(NeGate): This might be excessive for this array, idk :P
		f->vla.capacity = tb_next_pow2(f->vla.count + param_count);
		f->vla.data = tb_platform_heap_realloc(f->vla.data, f->vla.capacity * sizeof(TB_Register));
	}
	
	int param_start = f->vla.count;
	memcpy(f->vla.data + f->vla.count, params, param_count * sizeof(TB_Register));
	f->vla.count += param_count;
	int param_end = f->vla.count;
	
	TB_Register r = tb_make_reg(f, TB_VCALL, dt);
	f->nodes.payload[r].vcall.target = target;
	f->nodes.payload[r].vcall.param_start = param_start;
	f->nodes.payload[r].vcall.param_end = param_end;
	return r;
}

TB_API TB_Register tb_inst_ecall(TB_Function* f, TB_DataType dt, const TB_ExternalID target, size_t param_count, const TB_Register* params) {
	// Reserve space for the arguments
	if (f->vla.count + param_count >= f->vla.capacity) {
		// TODO(NeGate): This might be excessive for this array, idk :P
		f->vla.capacity = tb_next_pow2(f->vla.count + param_count);
		f->vla.data = tb_platform_heap_realloc(f->vla.data, f->vla.capacity * sizeof(TB_Register));
	}
	
	int param_start = f->vla.count;
	memcpy(f->vla.data + f->vla.count, params, param_count * sizeof(TB_Register));
	f->vla.count += param_count;
	int param_end = f->vla.count;
	
	TB_Register r = tb_make_reg(f, TB_ECALL, dt);
	f->nodes.payload[r].ecall.target = target;
	f->nodes.payload[r].ecall.param_start = param_start;
	f->nodes.payload[r].ecall.param_end = param_end;
	return r;
}

TB_API void tb_inst_memset(TB_Function* f, TB_Register dst, TB_Register val, TB_Register size, int align) {
	TB_Register r = tb_make_reg(f, TB_MEMSET, TB_TYPE_PTR);
	f->nodes.payload[r].mem_op.dst = dst;
	f->nodes.payload[r].mem_op.src = val;
	f->nodes.payload[r].mem_op.size = size;
	f->nodes.payload[r].mem_op.align = align;
}

TB_API void tb_inst_memcpy(TB_Function* f, TB_Register dst, TB_Register src, TB_Register size, int align) {
	TB_Register r = tb_make_reg(f, TB_MEMCPY, TB_TYPE_PTR);
	f->nodes.payload[r].mem_op.dst = dst;
	f->nodes.payload[r].mem_op.src = src;
	f->nodes.payload[r].mem_op.size = size;
	f->nodes.payload[r].mem_op.align = align;
}

TB_API TB_Register tb_inst_not(TB_Function* f, TB_DataType dt, TB_Register n) {
	TB_Register r = tb_make_reg(f, TB_NOT, dt);
	f->nodes.payload[r].unary = n;
	return r;
}

TB_API TB_Register tb_inst_neg(TB_Function* f, TB_DataType dt, TB_Register n) {
	TB_Register r = tb_make_reg(f, TB_NEG, dt);
	f->nodes.payload[r].unary = n;
	return r;
}

TB_API TB_Register tb_inst_and(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	// bitwise operators can't wrap
	return tb_cse_arith(f, TB_AND, dt, TB_ASSUME_NUW, a, b);
}

TB_API TB_Register tb_inst_or(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	// bitwise operators can't wrap
	return tb_cse_arith(f, TB_OR, dt, TB_ASSUME_NUW, a, b);
}

TB_API TB_Register tb_inst_xor(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	// bitwise operators can't wrap
	return tb_cse_arith(f, TB_XOR, dt, TB_ASSUME_NUW, a, b);
}

TB_API TB_Register tb_inst_add(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
	if (f->nodes.type[a] == TB_INT_CONST) tb_swap(a, b);
	
	TB_RegType a_type = f->nodes.type[a];
	TB_RegType b_type = f->nodes.type[b];
	
	if (a_type == TB_INT_CONST && b_type == TB_INT_CONST) {
		uint64_t sum = tb_fold_add(arith_behavior, dt, f->nodes.payload[a].i_const, f->nodes.payload[b].i_const);
		
		return tb_inst_iconst(f, dt, sum);
	} else if (b_type == TB_INT_CONST && f->nodes.payload[b].i_const == 0) {
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
		uint64_t sum = tb_fold_sub(arith_behavior, dt, f->nodes.payload[a].i_const, f->nodes.payload[b].i_const);
		
		return tb_inst_iconst(f, dt, sum);
	}
	
	return tb_cse_arith(f, TB_SUB, dt, arith_behavior, a, b);
}

TB_API TB_Register tb_inst_mul(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
	TB_RegType a_type = f->nodes.type[a];
	TB_RegType b_type = f->nodes.type[b];
	
	if (a_type == TB_INT_CONST && b_type == TB_INT_CONST) {
		uint64_t sum = tb_fold_mul(arith_behavior, dt, f->nodes.payload[a].i_const, f->nodes.payload[b].i_const);
		
		return tb_inst_iconst(f, dt, sum);
	}
	
	return tb_cse_arith(f, TB_MUL, dt, arith_behavior, a, b);
}

TB_API TB_Register tb_inst_div(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness) {
	TB_RegType a_type = f->nodes.type[a];
	TB_RegType b_type = f->nodes.type[b];
	
	if (a_type == TB_INT_CONST && b_type == TB_INT_CONST) {
		uint64_t sum = tb_fold_div(dt, f->nodes.payload[a].i_const, f->nodes.payload[b].i_const);
		
		return tb_inst_iconst(f, dt, sum);
	}
	
	// division can't wrap or overflow
	return tb_cse_arith(f, signedness ? TB_SDIV : TB_UDIV, dt, TB_ASSUME_NUW, a, b);
}

TB_API TB_Register tb_inst_shl(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, TB_ArithmaticBehavior arith_behavior) {
	return tb_cse_arith(f, TB_SHL, dt, arith_behavior, a, b);
}

// TODO(NeGate): Maybe i should split the bitshift operations into a separate kind of
// operator that has different arithmatic behaviors, maybe like trap on a large shift amount
TB_API TB_Register tb_inst_sar(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	// shift right can't wrap or overflow
	return tb_cse_arith(f, TB_SAR, dt, TB_ASSUME_NUW, a, b);
}

TB_API TB_Register tb_inst_shr(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	// shift right can't wrap or overflow
	return tb_cse_arith(f, TB_SHR, dt, TB_ASSUME_NUW, a, b);
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
	TB_Register r = tb_make_reg(f, TB_CMP_EQ, TB_TYPE_BOOL);
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ne(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_NE, TB_TYPE_BOOL);
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ilt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness) {
	TB_Register r = tb_make_reg(f, signedness ? TB_CMP_SLT : TB_CMP_ULT, TB_TYPE_BOOL);
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ile(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness) {
	TB_Register r = tb_make_reg(f, signedness ? TB_CMP_SLE : TB_CMP_ULE, TB_TYPE_BOOL);
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_igt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness) {
	TB_Register r = tb_make_reg(f, signedness ? TB_CMP_SLT : TB_CMP_ULT, TB_TYPE_BOOL);
	f->nodes.payload[r].cmp.a = b;
	f->nodes.payload[r].cmp.b = a;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_ige(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b, bool signedness) {
	TB_Register r = tb_make_reg(f, signedness ? TB_CMP_SLE : TB_CMP_ULE, TB_TYPE_BOOL);
	f->nodes.payload[r].cmp.a = b;
	f->nodes.payload[r].cmp.b = a;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_flt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLT, TB_TYPE_BOOL);
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_fle(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLE, TB_TYPE_BOOL);
	f->nodes.payload[r].cmp.a = a;
	f->nodes.payload[r].cmp.b = b;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_fgt(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLT, TB_TYPE_BOOL);
	f->nodes.payload[r].cmp.a = b;
	f->nodes.payload[r].cmp.b = a;
	f->nodes.payload[r].cmp.dt = dt;
	return r;
}

TB_API TB_Register tb_inst_cmp_fge(TB_Function* f, TB_DataType dt, TB_Register a, TB_Register b) {
	TB_Register r = tb_make_reg(f, TB_CMP_FLE, TB_TYPE_BOOL);
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

TB_API TB_Label tb_inst_new_label_id(TB_Function* f) {
	return f->label_count++;
}

TB_API TB_Register tb_inst_label(TB_Function* f, TB_Label id) {
	assert(id < f->label_count);
	
	TB_Register r = tb_make_reg(f, TB_LABEL, TB_TYPE_PTR);
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
	
	TB_Register r = tb_make_reg(f, TB_GOTO, TB_TYPE_VOID);
	f->nodes.payload[r].goto_.label = id;
	
	assert(f->current_label);
	f->nodes.payload[f->current_label].label.terminator = r;
	f->current_label = TB_NULL_REG;
}

TB_API TB_Register tb_inst_if(TB_Function* f, TB_Register cond, TB_Label if_true, TB_Label if_false) {
	TB_Register r = tb_make_reg(f, TB_IF, TB_TYPE_VOID);
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
		f->vla.data = tb_platform_heap_realloc(f->vla.data, f->vla.capacity * sizeof(TB_Register));
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

#if !TB_STRIP_LABELS
void tb_emit_label_symbol(TB_Module* m, uint32_t func_id, uint32_t label_id, size_t pos) {
	assert(pos < UINT32_MAX);
	if (m->label_symbols.count + 1 >= m->label_symbols.capacity) {
		m->label_symbols.capacity *= 2;
		m->label_symbols.data = tb_platform_heap_realloc(m->label_symbols.data, m->label_symbols.capacity * sizeof(TB_ConstPool32Patch));
	}
	
	size_t r = m->label_symbols.count++;
	m->label_symbols.data[r] = (TB_LabelSymbol){
		.func_id = func_id,
		.label_id = label_id,
		.pos = pos
	};
}
#endif

uint32_t tb_emit_const32_patch(TB_Module* m, uint32_t func_id, size_t pos, uint32_t data) {
	assert(pos < UINT32_MAX);
	if (m->const32_patches.count + 1 >= m->const32_patches.capacity) {
		m->const32_patches.capacity *= 2;
		m->const32_patches.data = tb_platform_heap_realloc(m->const32_patches.data, m->const32_patches.capacity * sizeof(TB_ConstPool32Patch));
	}
	
	size_t r = m->const32_patches.count++;
	m->const32_patches.data[r] = (TB_ConstPool32Patch){
		.func_id = func_id,
		.pos = pos,
		.raw_data = data
	};
	
	return r * 4;
}
