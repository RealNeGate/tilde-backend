#define TB_INTERNAL
#include "tb.h"

bool tb_opt_strength_reduction(TB_Function* f) {
	int changes = 0;
	for (TB_Register i = 1; i < f->count; i++) {
		if (f->nodes[i].type == TB_MUL) {
			TB_Register a = f->nodes[i].i_arith.a;
			TB_Register b = f->nodes[i].i_arith.b;
			TB_DataType dt = f->nodes[i].dt;
			
			if (f->nodes[b].type == TB_INT_CONST) {
				assert(f->nodes[b].i_const.hi == 0);
				
				int log2 = __builtin_ffs(f->nodes[b].i_const.lo) - 1;
				if (f->nodes[b].i_const.lo == (1 << log2)) {
					// It's a power of two, swap in a left-shift
					TB_Node* new_op = tb_insert_op(f, i - 1);
					*new_op = (TB_Node){
						.type = TB_INT_CONST,
						.dt = dt,
						.i_const.lo = log2
					};
					
					// shift over `i` because it's infront of the insertion
					i++;
					
					f->nodes[i].type = TB_SHL;
					f->nodes[i].i_arith.a = a;
					f->nodes[i].i_arith.b = new_op - f->nodes;
					changes++;
				}
			}
		}
	}
	
	return (changes > 0);
}
