#define TB_INTERNAL
#include "tb.h"

bool tb_opt_compact_dead_regs(TB_Function* f) {
	// Shift the dead regs out
	for (TB_Register i = 1; i < f->nodes.count; i++) if (f->nodes.type[i] == TB_NULL) {
		// Check if there's repeated dead regs to remove them all at once
		TB_Register j = i + 1;
		while (j < f->nodes.count && f->nodes.type[j] == TB_NULL) j++; 
		size_t regs_to_remove = j - i;
		
		// Shift everything back
		size_t regs_beyond = f->nodes.count - j;
		memmove(&f->nodes.type[i], &f->nodes.type[j], regs_beyond * sizeof(TB_RegType));
		memmove(&f->nodes.dt[i], &f->nodes.dt[j], regs_beyond * sizeof(TB_DataType));
		memmove(&f->nodes.payload[i], &f->nodes.payload[j], regs_beyond * sizeof(TB_RegPayload));
		
		// Move references
		// NOTE(NeGate): Usually these go backwards since they are inserting ops
		// and thus we don't want the find and replace to mess up anything coming
		// up but since this version needs to shift backwards we find & replace
		// forwards.
		size_t k = 0;
		while (k < regs_beyond) {
			tb_function_find_replace_reg(f, j + k, i + k);
			k++;
		}
		
		// Move the count back
		f->nodes.count -= regs_to_remove;
		return true;
	}
	
	return false;
}

bool tb_opt_remove_pass_node(TB_Function* f) {
	int changes = 0;
	
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_PASS) {
			tb_function_find_replace_reg(f, i, f->nodes.payload[i].pass);
			
			// Kill PASS
			tb_kill_op(f, i);
			changes++;
		}
	}
	
	return changes;
}

bool tb_opt_canonicalize(TB_Function* f) {
	int changes = 0;
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		TB_RegType type = f->nodes.type[i];
		
		if (type == TB_ADD || type == TB_MUL) {
			TB_Register a = f->nodes.payload[i].i_arith.a;
			TB_Register b = f->nodes.payload[i].i_arith.b;
			
			// Move all integer constants to the right side
			if (f->nodes.type[a] == TB_INT_CONST && f->nodes.type[b] != TB_INT_CONST) {
				tb_swap(a, b);
				
				f->nodes.payload[i].i_arith.a = a;
				f->nodes.payload[i].i_arith.b = b;
				changes++;
			} else if (f->nodes.type[a] == type && f->nodes.type[b] != type) {
				// Reshuffle the adds from 
				// (x + y) + z => x + (y + z)
				/*TB_Register xy = a;
				TB_Register x = f->nodes.payload[a].i_arith.a;
				TB_Register y = f->nodes.payload[a].i_arith.b;
				TB_Register z = b;
				
				f->nodes.payload[i].i_arith.a = x;
				f->nodes.payload[i].i_arith.b = xy;
				
				f->nodes.payload[a].i_arith.a = y;
				f->nodes.payload[a].i_arith.b = z;
				changes++;*/
			}
		} else if (type == TB_ARRAY_ACCESS) {
			TB_Register base = f->nodes.payload[i].array_access.base;
			TB_Register index = f->nodes.payload[i].array_access.index;
			
			if (f->nodes.type[index] == TB_INT_CONST) {
				TB_Int128 index_imm = f->nodes.payload[index].i_const;
				assert(index_imm.hi == 0);
				
				if (index_imm.lo == 0) {
					f->nodes.type[i] = TB_PASS;
					f->nodes.payload[i].pass = base;
					changes++;
				}
			}
		}
	}
	
	return (changes > 0);
}
