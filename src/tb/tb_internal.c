#include "tb_internal.h"
//#include "tb_builder.h"

#define TB_VALIDATOR_PRINT 0

#if TB_VALIDATOR_PRINT
#define printf(...)
#endif

TB_API bool tb_function_validate(TB_Function* restrict f) {
	int error_count = 0;
	
	size_t node_count = f->nodes.count;
	TB_Register i = 0;
	do {
		TB_RegType type = f->nodes.type[i];
		TB_DataType dt = f->nodes.dt[i];
		TB_RegPayload* restrict p = &f->nodes.payload[i];
		
		if (type == TB_SIGN_EXT ||
			type == TB_ZERO_EXT) {
			// Must all take in integral types
			if (TB_UNLIKELY(!TB_IS_INTEGER_TYPE(dt.type))) {
				printf("error on %s:r%u: Integer extensions has to use an integral type!\n", f->name, i);
				error_count++;
			}
			
			TB_Register src = p->ext;
			if (TB_UNLIKELY(dt.type < f->nodes.dt[src].type)) {
				printf("error on %s:r%u: destination type must be bigger than the source type.\n", f->name, i);
				error_count++;
			}
		} else if (type >= TB_AND &&
				   type <= TB_SAR) {
			// Must all take in integral types
			if (TB_UNLIKELY(!TB_IS_INTEGER_TYPE(dt.type) && dt.type != TB_PTR)) {
				printf("error on %s:r%u: Integer arithmatic has to use an integral type!\n", f->name, i);
				error_count++;
			}
			
			TB_DataType a = f->nodes.dt[p->i_arith.a];
			TB_DataType b = f->nodes.dt[p->i_arith.b];
			
			if (TB_UNLIKELY(!TB_DATA_TYPE_EQUALS(dt, a) || !TB_DATA_TYPE_EQUALS(dt, b))) {
				printf("error on %s:r%u: Arithmatic operands must match output type!\n", f->name, i);
				error_count++;
			}
		} else if (type >= TB_FADD &&
				   type <= TB_FDIV) {
			// Must all take in float types
			if (TB_UNLIKELY(!TB_IS_FLOAT_TYPE(dt.type))) {
				printf("error on %s:r%u: Float arithmatic has to use a floating-point type!\n", f->name, i);
				error_count++;
			}
			
			TB_DataType a = f->nodes.dt[p->i_arith.a];
			TB_DataType b = f->nodes.dt[p->i_arith.b];
			
			if (TB_UNLIKELY(!TB_DATA_TYPE_EQUALS(dt, a) || !TB_DATA_TYPE_EQUALS(dt, b))) {
				printf("error on %s:r%u: Arithmatic operands must match output type!\n", f->name, i);
				error_count++;
			}
		}
		
		i++;
	} while (i < node_count);
	
	if (error_count) {
		fprintf(stdout, "Validation Error!!");
		tb_function_print(f, tb_default_print_callback, stderr);
		fprintf(stderr, "\n\n\n");
		
		return false;
	}
	
	return true;
}

#if TB_VALIDATOR_PRINT
#undef printf
#endif

#define FOR_EACH_REGISTER_IN_FUNC \
for (size_t i = 1; i < f->nodes.count; i++) { \
TB_RegType type = f->nodes.type[i]; \
TB_RegPayload* p = &f->nodes.payload[i]; \
switch (type) { \
case TB_NULL: \
case TB_SIGNED_CONST: \
case TB_UNSIGNED_CONST: \
case TB_FLOAT_CONST: \
case TB_STRING_CONST: \
case TB_LOCAL: \
case TB_PARAM: \
case TB_GOTO: \
case TB_LINE_INFO: \
case TB_FUNC_ADDRESS: \
case TB_EFUNC_ADDRESS: \
case TB_GLOBAL_ADDRESS: \
break; \
case TB_LABEL: \
X(p->label.terminator); \
break; \
case TB_INITIALIZE: \
X(p->init.addr); \
break; \
case TB_KEEPALIVE: \
X(p->keepalive); \
break; \
case TB_RESTRICT: \
X(p->restrict_); \
break; \
case TB_ATOMIC_XCHG: \
case TB_ATOMIC_ADD: \
case TB_ATOMIC_SUB: \
case TB_ATOMIC_AND: \
case TB_ATOMIC_XOR: \
case TB_ATOMIC_OR: \
X(p->atomic.addr); \
X(p->atomic.src); \
break; \
case TB_MEMCPY: \
case TB_MEMSET: \
X(p->mem_op.dst); \
X(p->mem_op.src); \
X(p->mem_op.size); \
break; \
case TB_MEMBER_ACCESS: \
X(p->member_access.base); \
break; \
case TB_ARRAY_ACCESS: \
X(p->array_access.base); \
X(p->array_access.index); \
break; \
case TB_PARAM_ADDR: \
X(p->param_addr.param); \
break; \
case TB_PASS: \
X(p->pass); \
break; \
case TB_PHI1: \
X(p->phi1.a); \
X(p->phi1.a_label); \
break; \
case TB_PHI2: \
X(p->phi2.a); \
X(p->phi2.b); \
X(p->phi2.a_label); \
X(p->phi2.b_label); \
break; \
case TB_LOAD: \
X(p->load.address); \
break; \
case TB_STORE: \
X(p->store.address); \
X(p->store.value); \
break; \
case TB_ZERO_EXT: \
case TB_SIGN_EXT: \
case TB_FLOAT_EXT: \
X(p->ext); \
break; \
case TB_INT2PTR: \
case TB_PTR2INT: \
X(p->ptrcast); \
case TB_INT2FLOAT: \
case TB_FLOAT2INT: \
X(p->cvt.src); \
break; \
case TB_TRUNCATE: \
X(p->trunc); \
break; \
case TB_AND: \
case TB_OR: \
case TB_XOR: \
case TB_ADD: \
case TB_SUB: \
case TB_MUL: \
case TB_UDIV: \
case TB_SDIV: \
case TB_UMOD: \
case TB_SMOD: \
case TB_SAR: \
case TB_SHL: \
case TB_SHR: \
X(p->i_arith.a); \
X(p->i_arith.b); \
break; \
case TB_NOT: \
case TB_NEG: \
case TB_X86INTRIN_SQRT: \
case TB_X86INTRIN_RSQRT: \
X(p->unary.src); \
break; \
case TB_FADD: \
case TB_FSUB: \
case TB_FMUL: \
case TB_FDIV: \
X(p->f_arith.a); \
X(p->f_arith.b); \
break; \
case TB_CMP_EQ: \
case TB_CMP_NE: \
case TB_CMP_SLT: \
case TB_CMP_SLE: \
case TB_CMP_ULT: \
case TB_CMP_ULE: \
case TB_CMP_FLT: \
case TB_CMP_FLE: \
X(p->cmp.a); \
X(p->cmp.b); \
break; \
case TB_VCALL: \
X(p->vcall.target); \
case TB_CALL: \
case TB_ICALL: \
case TB_ECALL: \
for (size_t j = p->call.param_start; j < p->call.param_end; j++) { \
X(f->vla.data[j]); \
} \
break; \
case TB_SWITCH: \
X(p->switch_.key); \
break; \
case TB_IF: \
X(p->if_.cond); \
break; \
case TB_RET: \
X(p->ret.value); \
break; \
default: tb_panic(false, "Unknown node type: %d", type); \
} \
}

//
// IR ANALYSIS
//
void tb_find_live_intervals(const TB_Function* f, TB_Register intervals[]) {
	for (size_t i = 0; i < f->nodes.count; i++) intervals[i] = TB_NULL_REG;
    
#define X(reg) intervals[reg] = i
	FOR_EACH_REGISTER_IN_FUNC
#undef X
}

void tb_find_use_count(const TB_Function* f, int use_count[]) {
	for (size_t i = 0; i < f->nodes.count; i++) use_count[i] = 0;
	
#define X(reg) use_count[reg] += 1
	FOR_EACH_REGISTER_IN_FUNC
#undef X
}

size_t tb_count_uses(const TB_Function* f, TB_Register find, size_t start, size_t end) {
	size_t count = 0;
	
#define X(reg) count += (reg == find)
	FOR_EACH_REGISTER_IN_FUNC
#undef X
	
	return count;
}

TB_Register tb_find_first_use(const TB_Function* f, TB_Register find, size_t start, size_t end) {
#define X(reg) if (reg == find) return i
	FOR_EACH_REGISTER_IN_FUNC
#undef X
	
	return 0;
}

void tb_function_find_replace_reg(TB_Function* f, TB_Register find, TB_Register replace) {
#define X(reg) if (reg == find) { reg = replace; }
	FOR_EACH_REGISTER_IN_FUNC
#undef X
}

TB_Register tb_find_reg_from_label(TB_Function* f, TB_Label id) {
	for (size_t i = 0; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LABEL && f->nodes.payload[i].label.id == id) return i;
	}
    
	return TB_NULL_REG;
}

// NOTE(NeGate): Any previous TB_Register you have saved locally,
// update them or at least shift over all the indices based on `at`
void tb_insert_op(TB_Function* f, TB_Register at) {
	// Reserve the space
	if (f->nodes.count + 1 >= f->nodes.capacity) tb_resize_node_stream(f, tb_next_pow2(f->nodes.count + 1));
	
	// Shift over registers
	int registers_beyond_end_point = f->nodes.count - at;
	memmove(&f->nodes.type[at + 1], &f->nodes.type[at], registers_beyond_end_point * sizeof(TB_RegType));
	memmove(&f->nodes.dt[at + 1], &f->nodes.dt[at], registers_beyond_end_point * sizeof(TB_DataType));
	memmove(&f->nodes.payload[at + 1], &f->nodes.payload[at], registers_beyond_end_point * sizeof(TB_RegPayload));
	f->nodes.count += 1;
	
	// Clear out register
	// necessary for the find & replace not to screw up
	tb_kill_op(f, at);
	
	// Shift all references over by 1
	while (registers_beyond_end_point--) {
		tb_function_find_replace_reg(f, at + registers_beyond_end_point, at + registers_beyond_end_point + 1);
	}
}

// NOTE(NeGate): Any previous TB_Register you have saved locally,
// update them or at least shift over all the indices based on `at`
//
// TODO(NeGate): Move this out of this file once it's relevant
// TODO(NeGate): Implement multiple return statements, VLA insertion, and proper labels
TB_Register tb_insert_copy_ops(TB_Function* f, const TB_Register* params, TB_Register at, const TB_Function* src_func, TB_Register src_base, int count) {
	// Reserve the space
	if (f->nodes.count + count >= f->nodes.capacity) tb_resize_node_stream(f, tb_next_pow2(f->nodes.count + count));
	
	// Shift over registers
	int registers_beyond_end_point = f->nodes.count - at;
	memmove(&f->nodes.type[at + count], &f->nodes.type[at], registers_beyond_end_point * sizeof(TB_RegType));
	memmove(&f->nodes.dt[at + count], &f->nodes.dt[at], registers_beyond_end_point * sizeof(TB_DataType));
	memmove(&f->nodes.payload[at + count], &f->nodes.payload[at], registers_beyond_end_point * sizeof(TB_RegPayload));
	f->nodes.count += count;
	
	// Clear out registers
	// necessary for the find & replace not to screw up
	for (size_t i = 0; i < count; i++) {
		tb_kill_op(f, at + i);
	}
	
	// Shift all references over by the amount inserted
	size_t i = registers_beyond_end_point;
	while (i--) {
		tb_function_find_replace_reg(f, at + i, at + i + count);
	}
	
	// Copy in nodes
	memcpy(&f->nodes.type[at], &src_func->nodes.type[src_base], count * sizeof(TB_RegType));
	memcpy(&f->nodes.dt[at], &src_func->nodes.dt[src_base], count * sizeof(TB_DataType));
	memcpy(&f->nodes.payload[at], &src_func->nodes.payload[src_base], count * sizeof(TB_RegPayload));
	
	// Fix all references
	TB_Register ret = 0;
	
#define ffu(r) if (r < count) r += (at - src_base)
	for (int i = at; i < (at+count); i++) {
		TB_RegType type = f->nodes.type[i];
		TB_RegPayload* p = &f->nodes.payload[i];
		
		switch (type) {
			case TB_NULL:
			case TB_SIGNED_CONST:
			case TB_UNSIGNED_CONST:
			case TB_LOCAL:
			break;
			case TB_PARAM: {
				TB_Register r = params[p->param.id];
				
				f->nodes.type[i] = TB_PASS;
				*p = (TB_RegPayload){
					.pass = r
				};
				break;
			}
			case TB_LABEL:
			if (p->label.id != 0) {
				// TODO(NeGate): Fix this!
				tb_todo();
			} else {
				tb_kill_op(f, i);
			}
			break;
			case TB_PHI1:
			ffu(p->phi1.a);
			ffu(p->phi1.a_label);
			break;
			case TB_PHI2:
			ffu(p->phi2.a);
			ffu(p->phi2.b);
			ffu(p->phi2.a_label);
			ffu(p->phi2.b_label);
			break;
			case TB_ARRAY_ACCESS:
			ffu(p->array_access.base);
			ffu(p->array_access.index);
			break;
			case TB_MEMBER_ACCESS:
			ffu(p->member_access.base);
			break;
			case TB_SIGN_EXT:
			case TB_ZERO_EXT:
			ffu(p->ext);
			break;
			case TB_PARAM_ADDR:
			ffu(p->param_addr.param);
			break;
			case TB_LOAD:
			ffu(p->load.address);
			break;
			case TB_STORE:
			ffu(p->store.address);
			ffu(p->store.value);
			break;
			case TB_AND:
			case TB_OR:
			case TB_ADD:
			case TB_SUB:
			case TB_MUL:
			case TB_UDIV:
			case TB_SDIV:
			ffu(p->i_arith.a);
			ffu(p->i_arith.b);
			break;
			case TB_FADD:
			case TB_FSUB:
			case TB_FMUL:
			case TB_FDIV:
			ffu(p->f_arith.a);
			ffu(p->f_arith.b);
			break;
			case TB_CMP_EQ:
			case TB_CMP_NE:
			case TB_CMP_SLT:
			case TB_CMP_SLE:
			case TB_CMP_ULT:
			case TB_CMP_ULE:
			case TB_CMP_FLT:
			case TB_CMP_FLE:
			ffu(p->cmp.a);
			ffu(p->cmp.b);
			break;
			case TB_CALL:
			case TB_ICALL:
			case TB_ECALL:
			case TB_VCALL:
			for (size_t j = p->call.param_start; j < p->call.param_end; j++) {
				ffu(f->vla.data[j]);
			}
			break;
			case TB_IF:
			ffu(p->if_.cond);
			break;
			case TB_RET:
			// TODO(NeGate): Implement multiple return values
			if (ret) tb_todo();
			
			ffu(p->ret.value);
			
			ret = p->ret.value;
			tb_kill_op(f, i);
			break;
			default: tb_todo();
		}
#undef ffu
	}
	
	return ret;
}

TB_Label* tb_calculate_immediate_predeccessors(TB_Function* f, TB_TemporaryStorage* tls, TB_Label l, int* dst_count) {
	size_t count = 0;
	TB_Label* preds = tb_tls_push(tls, 0);
	
	TB_Register label = 1;
	do {
		TB_Register terminator = f->nodes.payload[label].label.terminator;
		TB_Label id = f->nodes.payload[label].label.id;
		
		if (f->nodes.type[terminator] == TB_LABEL) {
			if (l == f->nodes.payload[terminator].label.id) {
				*((TB_Register*)tb_tls_push(tls, sizeof(TB_Register))) = id;
				count++;
			}
			label = terminator;
		} else if (f->nodes.type[terminator] == TB_IF) {
			if (l == f->nodes.payload[terminator].if_.if_true) {
				*((TB_Register*)tb_tls_push(tls, sizeof(TB_Register))) = id;
				count++;
			}
			
			if (l == f->nodes.payload[terminator].if_.if_false) {
				*((TB_Register*)tb_tls_push(tls, sizeof(TB_Register))) = id;
				count++;
			}
			label = terminator + 1;
		} else if (f->nodes.type[terminator] == TB_GOTO) {
			if (l == f->nodes.payload[terminator].goto_.label) {
				*((TB_Register*)tb_tls_push(tls, sizeof(TB_Register))) = id;
				count++;
			}
			label = terminator + 1;
		} else if (f->nodes.type[terminator] == TB_RET) {
			label = terminator + 1;
		} else tb_todo();
	} while (label < f->nodes.count);
	
	*dst_count = count;
	return preds;
}
