#define TB_INTERNAL
#include "tb.h"

// NOTE(NeGate): Any previous TB_Register you have saved locally,
// update them or at least shift over all the indices based on `at`
//
// TODO(NeGate): Move this out of this file once it's relevant
// TODO(NeGate): Implement multiple return statements, VLA insertion, and proper labels
static TB_Register tb_insert_copy_ops(TB_Function* f, const TB_Register* params, TB_Register at, TB_Node* src, TB_Register src_base, int count) {
	// Reserve the space
	if (f->count + count >= f->capacity) {
		f->capacity = tb_next_pow2(f->count + count);
		f->nodes = realloc(f->nodes, f->capacity * sizeof(TB_Node));
	}
	
	// Shift over registers
	int registers_beyond_end_point = f->count - at;
	memmove(&f->nodes[at + count], &f->nodes[at], registers_beyond_end_point * sizeof(TB_Node));
	f->count += count;
	
	// Clear out registers
	// necessary for the find & replace not to screw up
	for (size_t i = 0; i < count; i++) {
		f->nodes[at + i] = (TB_Node){ 0 };
	}
	
	// Shift all references over by the amount inserted
	size_t i = registers_beyond_end_point;
	while (i--) {
		tb_function_find_replace_reg(f, at + i, at + i + count);
	}
	
	// Copy in nodes
	memcpy(&f->nodes[at], src, count * sizeof(TB_Node));
	
	// Fix all references
	TB_Register ret = 0;
	
#define ffu(r) if (r < count) r += (at - src_base)
	for (int i = at; i < (at+count); i++) {
		switch (f->nodes[i].type) {
			case TB_NULL:
			case TB_INT_CONST:
			case TB_LOCAL:
			break;
			case TB_PARAM: {
				TB_Register r = params[f->nodes[i].param.id];
				
				f->nodes[i].type = TB_PASS;
				f->nodes[i].pass = r;
				break;
			}
			case TB_LABEL:
			if (f->nodes[i].label.id != 0) {
				// TODO(NeGate): Fix this!
				tb_todo();
			} else {
				f->nodes[i] = (TB_Node){ 0 };
			}
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
			case TB_RET:
			// TODO(NeGate): Implement multiple return values
			if (ret) tb_todo();
			
			ffu(f->nodes[i].ret.value);
			
			ret = f->nodes[i].ret.value;
			f->nodes[i] = (TB_Node){ 0 };
			break;
			default: tb_todo();
		}
#undef ffu
	}
	
	return ret;
}

static int tb_estimate_expr_pressure(const TB_Function* f, TB_Register i) {
	switch (f->nodes[i].type) {
		// If the node is a leaf (has no children), its Strahler number is one.
		case TB_PARAM:
		case TB_INT_CONST: 
		return 1;
		// If the node has one child with Strahler number i, and all other children
		// have Strahler numbers less than i, then the Strahler number of the node
		// is i again.
		//
		// If the node has two or more children with Strahler number i, and no 
		// children with greater number, then the Strahler number of the node is i + 1.
		case TB_AND:
		case TB_OR:
		case TB_ADD:
		case TB_SUB:
		case TB_MUL:
		case TB_UDIV:
		case TB_SDIV: {
			int a = tb_estimate_expr_pressure(f, f->nodes[i].i_arith.a);
			int b = tb_estimate_expr_pressure(f, f->nodes[i].i_arith.b);
			
			if (a == b) return a + 1;
			else if (a > b) return a;
			else return b;
		}
		case TB_CMP_EQ:
		case TB_CMP_NE:
		case TB_CMP_SLT:
		case TB_CMP_SLE:
		case TB_CMP_ULT:
		case TB_CMP_ULE:
		case TB_CMP_FLT:
		case TB_CMP_FLE: {
			int a = tb_estimate_expr_pressure(f, f->nodes[i].cmp.a);
			int b = tb_estimate_expr_pressure(f, f->nodes[i].cmp.b);
			
			if (a == b) return a + 1;
			else if (a > b) return a;
			else return b;
		}
		default: tb_todo();
	}
}

static int tb_estimate_func_pressure(const TB_Function* f) {
	int max_pressure = 0;
	TB_Register label = 1;
	
	// get an estimate of register pressure in a basic block.
	// we'll be using it in the opportunity calculation.
	for (TB_Register i = 1; i < f->count; i++) {
		if (f->nodes[i].type == TB_LABEL) {
			label = i;
			max_pressure = 0;
		} else if (f->nodes[i].type == TB_CALL) {
			// tally up pressure from parameters, it'll probably be relevant too
			int p = 0;
			for (size_t j = f->nodes[i].call.param_start; j < f->nodes[i].call.param_end; j++) {
				p += tb_estimate_expr_pressure(f, f->vla.data[j]);
			}
			
			if (p > max_pressure) p = max_pressure;
		} else if (f->nodes[i].type == TB_STORE) {
			int p = tb_estimate_expr_pressure(f, f->nodes[i].store.address);
			p += tb_estimate_expr_pressure(f, f->nodes[i].store.value);
			
			if (p > max_pressure) p = max_pressure;
		} else if (f->nodes[i].type == TB_LOAD) {
			int p = tb_estimate_expr_pressure(f, f->nodes[i].load.address);
			p += 2;
			
			if (p > max_pressure) p = max_pressure;
		} else if (f->nodes[i].type == TB_IF) {
			int p = tb_estimate_expr_pressure(f, f->nodes[i].if_.cond);
			p += 1;
			
			if (p > max_pressure) p = max_pressure;
		}
	}
	
	return max_pressure;
}

bool tb_opt_inline(TB_Function* f) {
	int max_pressure = 0;
	TB_Register label = 1;
	
	// get an estimate of register pressure in a basic block.
	// we'll be using it in the opportunity calculation.
	int changes = 0;
	for (TB_Register i = 1; i < f->count; i++) {
		if (f->nodes[i].type == TB_LABEL) {
			label = i;
			max_pressure = 0;
		} else if (f->nodes[i].type == TB_CALL && f->nodes[i].call.target != f) {
			// tally up pressure from parameters, it'll probably be relevant too
			int p = 0;
			for (size_t j = f->nodes[i].call.param_start; j < f->nodes[i].call.param_end; j++) {
				p += tb_estimate_expr_pressure(f, f->vla.data[j]);
			}
			if (p > max_pressure) max_pressure = p;
			
			int opportunity = 32;
			int cost = (max_pressure * 2) + tb_estimate_func_pressure(f->nodes[i].call.target);
			
			// convert to inline'd call
			if (opportunity > cost) {
				//printf("Inlined call to '%s' in '%s'\n", f->nodes[i].call.target->name, f->name);
				
				f->nodes[i].type = TB_ICALL;
				changes++;
			}
		} else if (f->nodes[i].type == TB_STORE) {
			int p = tb_estimate_expr_pressure(f, f->nodes[i].store.address);
			p += tb_estimate_expr_pressure(f, f->nodes[i].store.value);
			
			if (p > max_pressure) max_pressure = p;
		} else if (f->nodes[i].type == TB_LOAD) {
			int p = tb_estimate_expr_pressure(f, f->nodes[i].load.address);
			p += 1;
			
			if (p > max_pressure) max_pressure = p;
		}
	}
	
	if (changes) {
		// spot inline calls and unfold them
		for (TB_Register i = 1; i < f->count; i++) if (f->nodes[i].type == TB_ICALL) {
			const TB_Function* target = f->nodes[i].call.target;
			const TB_Register* params = &f->vla.data[f->nodes[i].call.param_start];
			
			// kill call
			f->nodes[i] = (TB_Node){ 0 };
			
			// insert target's body
			TB_Register ret = tb_insert_copy_ops(f, params, i, &target->nodes[1], 1, target->count - 1);
			
			// the return value will exist in `i + (target->count - 1)` since
			// got shifted over
			if (ret) tb_function_find_replace_reg(f, i + (target->count - 1), ret);
		}
		
		//tb_function_print(f);
		//printf("\n\n\n");
		
		// Remove the PASS nodes
		for (TB_Register i = 1; i < f->count; i++) if (f->nodes[i].type == TB_PASS) {
			tb_function_find_replace_reg(f, i, f->nodes[i].pass);
			
			// Kill PASS
			f->nodes[i] = (TB_Node){ 0 };
		}
		
		//tb_function_print(f);
		//printf("\n\n\n");
	}
	
	return (changes > 0);
}
