// TODO(NeGate): Consider some smart list macros for this.
#define TB_INTERNAL
#include "tb.h"

//
// IR ANALYSIS
//
void tb_find_live_intervals(size_t intervals[], const TB_Function* f) {
	for (size_t i = 0; i < f->count; i++) intervals[i] = TB_NULL_REG;
    
	for (size_t i = 0; i < f->count; i++) {
		switch (f->nodes[i].type) {
            case TB_NULL:
            case TB_INT_CONST:
            case TB_FLOAT_CONST:
            case TB_LOCAL:
            case TB_PARAM:
            case TB_LABEL:
            case TB_GOTO:
			break;
            case TB_MEMBER_ACCESS:
			intervals[f->nodes[i].member_access.base] = i;
			break;
            case TB_ARRAY_ACCESS:
			intervals[f->nodes[i].array_access.base] = i;
			intervals[f->nodes[i].array_access.index] = i;
			break;
            case TB_PARAM_ADDR:
			intervals[f->nodes[i].param_addr.param] = i;
			break;
            case TB_PHI1:
			intervals[f->nodes[i].phi1.a] = i;
			intervals[f->nodes[i].phi1.a_label] = i;
			break;
            case TB_PHI2:
			intervals[f->nodes[i].phi2.a] = i;
			intervals[f->nodes[i].phi2.b] = i;
			intervals[f->nodes[i].phi2.a_label] = i;
			intervals[f->nodes[i].phi2.b_label] = i;
			break;
            case TB_LOAD:
			intervals[f->nodes[i].load.address] = i;
			break;
            case TB_STORE:
			intervals[f->nodes[i].store.address] = i;
			intervals[f->nodes[i].store.value] = i;
			break;
            case TB_ZERO_EXT:
            case TB_SIGN_EXT:
			intervals[f->nodes[i].ext] = i;
			break;
            case TB_AND:
            case TB_OR:
            case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_UDIV:
            case TB_SDIV:
            case TB_SAR:
            case TB_SHL:
            case TB_SHR:
			intervals[f->nodes[i].i_arith.a] = i;
			intervals[f->nodes[i].i_arith.b] = i;
			break;
            case TB_FADD:
            case TB_FSUB:
            case TB_FMUL:
            case TB_FDIV:
			intervals[f->nodes[i].f_arith.a] = i;
			intervals[f->nodes[i].f_arith.b] = i;
			break;
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_FLT:
            case TB_CMP_FLE:
			intervals[f->nodes[i].cmp.a] = i;
			intervals[f->nodes[i].cmp.b] = i;
			break;
            case TB_CALL:
            case TB_ICALL:
			for (size_t j = f->nodes[i].call.param_start; j < f->nodes[i].call.param_end; j++) {
				intervals[f->vla.data[j]] = i;
			}
			break;
			case TB_SWITCH: 
			intervals[f->nodes[i].switch_.key] = i;
			break;
            case TB_IF:
			intervals[f->nodes[i].if_.cond] = i;
			break;
            case TB_RET:
			intervals[f->nodes[i].ret.value] = i;
			break;
            default: tb_todo();
		}
	}
}

size_t tb_count_uses(const TB_Function* f, TB_Register find, size_t start, size_t end) {
#define ffu(r) count += (r == find);
    size_t count = 0;
	
	for (size_t i = start; i < end; i++) {
		switch (f->nodes[i].type) {
            case TB_NULL:
            case TB_INT_CONST:
            case TB_LOCAL:
            case TB_PARAM:
            case TB_LABEL:
			break;
            case TB_PHI1:
			ffu(f->nodes[i].phi1.a);
			ffu(f->nodes[i].phi1.a_label);
			break;
            case TB_PHI2:
			ffu(f->nodes[i].phi2.a);
			ffu(f->nodes[i].phi2.b);
			ffu(f->nodes[i].phi2.a_label);
			ffu(f->nodes[i].phi2.b_label);
			break;
            case TB_MEMBER_ACCESS:
			ffu(f->nodes[i].member_access.base);
			break;
            case TB_ARRAY_ACCESS:
			ffu(f->nodes[i].array_access.base);
			ffu(f->nodes[i].array_access.index);
			break;
            case TB_SIGN_EXT:
            case TB_ZERO_EXT:
			ffu(f->nodes[i].ext);
			break;
            case TB_PARAM_ADDR:
			ffu(f->nodes[i].param_addr.param);
			break;
            case TB_LOAD:
			ffu(f->nodes[i].load.address);
			break;
            case TB_STORE:
			ffu(f->nodes[i].store.address);
			ffu(f->nodes[i].store.value);
			break;
            case TB_AND:
            case TB_OR:
            case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_UDIV:
            case TB_SDIV:
            case TB_SAR:
            case TB_SHL:
            case TB_SHR:
			ffu(f->nodes[i].i_arith.a);
			ffu(f->nodes[i].i_arith.b);
			break;
            case TB_FADD:
            case TB_FSUB:
            case TB_FMUL:
            case TB_FDIV:
			ffu(f->nodes[i].f_arith.a);
			ffu(f->nodes[i].f_arith.b);
			break;
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_FLT:
            case TB_CMP_FLE:
			ffu(f->nodes[i].cmp.a);
			ffu(f->nodes[i].cmp.b);
			break;
            case TB_CALL:
            case TB_ICALL:
			for (size_t j = f->nodes[i].call.param_start; j < f->nodes[i].call.param_end; j++) {
				ffu(f->vla.data[j]);
			}
			break;
            case TB_IF:
			ffu(f->nodes[i].if_.cond);
			break;
			case TB_SWITCH: 
			ffu(f->nodes[i].switch_.key);
			break;
            case TB_RET:
			ffu(f->nodes[i].ret.value);
			break;
            default: tb_todo();
		}
	}
    
	return count;
#undef ffu
}

TB_Register tb_find_first_use(const TB_Function* f, TB_Register find, size_t start, size_t end) {
#define ffu(r) if (r == find) return i
    
	for (size_t i = start; i < end; i++) {
		switch (f->nodes[i].type) {
            case TB_NULL:
            case TB_INT_CONST:
            case TB_LOCAL:
            case TB_PARAM:
            case TB_LABEL:
			break;
            case TB_PHI1:
			ffu(f->nodes[i].phi1.a);
			ffu(f->nodes[i].phi1.a_label);
			break;
            case TB_PHI2:
			ffu(f->nodes[i].phi2.a);
			ffu(f->nodes[i].phi2.b);
			ffu(f->nodes[i].phi2.a_label);
			ffu(f->nodes[i].phi2.b_label);
			break;
            case TB_ARRAY_ACCESS:
			ffu(f->nodes[i].array_access.base);
			ffu(f->nodes[i].array_access.index);
			break;
            case TB_MEMBER_ACCESS:
			ffu(f->nodes[i].member_access.base);
			break;
            case TB_SIGN_EXT:
            case TB_ZERO_EXT:
			ffu(f->nodes[i].ext);
			break;
            case TB_PARAM_ADDR:
			ffu(f->nodes[i].param_addr.param);
			break;
            case TB_LOAD:
			ffu(f->nodes[i].load.address);
			break;
            case TB_STORE:
			ffu(f->nodes[i].store.address);
			ffu(f->nodes[i].store.value);
			break;
            case TB_AND:
            case TB_OR:
            case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_UDIV:
            case TB_SDIV:
            case TB_SAR:
            case TB_SHL:
            case TB_SHR:
			ffu(f->nodes[i].i_arith.a);
			ffu(f->nodes[i].i_arith.b);
			break;
            case TB_FADD:
            case TB_FSUB:
            case TB_FMUL:
            case TB_FDIV:
			ffu(f->nodes[i].f_arith.a);
			ffu(f->nodes[i].f_arith.b);
			break;
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_FLT:
            case TB_CMP_FLE:
			ffu(f->nodes[i].cmp.a);
			ffu(f->nodes[i].cmp.b);
			break;
			case TB_CALL:
            case TB_ICALL:
			for (size_t j = f->nodes[i].call.param_start; j < f->nodes[i].call.param_end; j++) {
				ffu(f->vla.data[j]);
			}
			break;
			case TB_SWITCH: 
			ffu(f->nodes[i].switch_.key);
			break;
            case TB_IF:
			ffu(f->nodes[i].if_.cond);
			break;
            case TB_RET:
			ffu(f->nodes[i].ret.value);
			break;
            default: tb_todo();
		}
	}
    
	return 0;
#undef ffu
}

void tb_function_find_replace_reg(TB_Function* f, TB_Register find, TB_Register replace) {
#define f_n_r(r) if (r == find) r = replace
    
	for (size_t i = 0; i < f->count; i++) {
		switch (f->nodes[i].type) {
            case TB_NULL:
            case TB_GOTO:
            case TB_INT_CONST:
            case TB_LOCAL:
            case TB_PARAM:
            break;
            case TB_LABEL:
			f_n_r(f->nodes[i].label.terminator);
			break;
            case TB_PASS:
			f_n_r(f->nodes[i].pass);
			break;
            case TB_PHI1:
			f_n_r(f->nodes[i].phi1.a);
			f_n_r(f->nodes[i].phi1.a_label);
			break;
            case TB_PHI2:
			f_n_r(f->nodes[i].phi2.a);
			f_n_r(f->nodes[i].phi2.b);
			f_n_r(f->nodes[i].phi2.a_label);
			f_n_r(f->nodes[i].phi2.b_label);
			break;
            case TB_ARRAY_ACCESS:
			f_n_r(f->nodes[i].array_access.base);
			f_n_r(f->nodes[i].array_access.index);
			break;
            case TB_MEMBER_ACCESS:
			f_n_r(f->nodes[i].member_access.base);
			break;
            case TB_SIGN_EXT:
            case TB_ZERO_EXT:
			f_n_r(f->nodes[i].ext);
			break;
            case TB_PARAM_ADDR:
			f_n_r(f->nodes[i].param_addr.param);
			break;
            case TB_LOAD:
			f_n_r(f->nodes[i].load.address);
			break;
            case TB_STORE:
			f_n_r(f->nodes[i].store.address);
			f_n_r(f->nodes[i].store.value);
			break;
            case TB_AND:
            case TB_OR:
            case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_UDIV:
            case TB_SDIV:
            case TB_SAR:
            case TB_SHL:
            case TB_SHR:
			f_n_r(f->nodes[i].i_arith.a);
			f_n_r(f->nodes[i].i_arith.b);
			break;
            case TB_FADD:
            case TB_FSUB:
            case TB_FMUL:
            case TB_FDIV:
			f_n_r(f->nodes[i].f_arith.a);
			f_n_r(f->nodes[i].f_arith.b);
			break;
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_FLT:
            case TB_CMP_FLE:
			f_n_r(f->nodes[i].cmp.a);
			f_n_r(f->nodes[i].cmp.b);
			break;
			case TB_CALL:
            case TB_ICALL:
			for (size_t j = f->nodes[i].call.param_start; j < f->nodes[i].call.param_end; j++) {
				f_n_r(f->vla.data[j]);
			}
			break;
			case TB_SWITCH: 
			f_n_r(f->nodes[i].switch_.key);
			break;
            case TB_IF:
			f_n_r(f->nodes[i].if_.cond);
			break;
            case TB_RET:
			f_n_r(f->nodes[i].ret.value);
			break;
            default: tb_todo();
		}
	}
    
#undef f_n_r
}

TB_Register tb_find_reg_from_label(TB_Function* f, TB_Label id) {
	for (size_t i = 0; i < f->count; i++) {
		if (f->nodes[i].type == TB_LABEL && f->nodes[i].label.id == id) return i;
	}
    
	return TB_NULL_REG;
}

// NOTE(NeGate): Any previous TB_Register you have saved locally,
// update them or at least shift over all the indices based on `at`
static TB_Node* tb_insert_op(TB_Function* f, TB_Register at) {
	// Reserve the space
	if (f->count + 1 >= f->capacity) {
		f->capacity = f->count + 1;
		f->capacity = tb_next_pow2(f->capacity);
        
		f->nodes = realloc(f->nodes, f->capacity * sizeof(TB_Node));
	}
	
	// Shift over registers
	int registers_beyond_end_point = f->count - at;
	memmove(&f->nodes[at + 1], &f->nodes[at], registers_beyond_end_point * sizeof(TB_Node));
	f->count += 1;
	
	// Clear out register
	// necessary for the find & replace not to screw up
	f->nodes[at] = (TB_Node){ 0 };
	
	// Shift all references over by 1
	while (registers_beyond_end_point--) {
		tb_function_find_replace_reg(f, at + registers_beyond_end_point, at + registers_beyond_end_point + 1);
	}
	
	return &f->nodes[at];
}
