#define TB_INTERNAL
#include "tb.h"

bool tb_opt_compact_dead_regs(TB_Function* f) {
	// Shift the dead regs out
	for (TB_Register i = 1; i < f->count; i++) if (f->nodes[i].type == TB_NULL) {
		// Check if there's repeated dead regs to remove them all at once
		TB_Register j = i + 1;
		while (j < f->count && f->nodes[j].type == TB_NULL) j++; 
		size_t regs_to_remove = j - i;
		
		// Shift everything back
		size_t regs_beyond = f->count - j;
		memmove(&f->nodes[i], &f->nodes[j], regs_beyond * sizeof(TB_Node));
		
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
		f->count -= regs_to_remove;
		return true;
	}
	
	return false;
}

bool tb_opt_canonicalize(TB_Function* f) {
	int changes = 0;
	for (TB_Register i = 1; i < f->count; i++) {
		int type = f->nodes[i].type;
		
		if (type == TB_ADD || type == TB_MUL) {
			// Move all integer constants to the right side
			if (f->nodes[f->nodes[i].i_arith.a].type == TB_INT_CONST) {
				tb_swap(f->nodes[i].i_arith.a, f->nodes[i].i_arith.b);
				changes++;
			}
			
			TB_Register a = f->nodes[i].i_arith.a;
			TB_Register b = f->nodes[i].i_arith.b;
			if (f->nodes[a].type == type) {
				// Reshuffle the adds from 
				// (x + y) + z => x + (y + z)
				TB_Register xy = a;
				TB_Register x = f->nodes[a].i_arith.a;
				TB_Register y = f->nodes[a].i_arith.b;
				TB_Register z = b;
				
				f->nodes[i].i_arith.a = x;
				f->nodes[i].i_arith.b = xy;
				
				f->nodes[a].i_arith.a = y;
				f->nodes[a].i_arith.b = z;
				changes++;
			}
		}
	}
	
	return (changes > 0);
}
