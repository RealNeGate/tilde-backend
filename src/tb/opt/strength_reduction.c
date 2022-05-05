#include "../tb_internal.h"

bool tb_opt_strength_reduction(TB_Function* f) {
    int changes = 0;
	
    TB_FOR_EACH_NODE(n, f) {
        TB_Reg i = n - f->nodes.data;
		
        if (n->type == TB_MUL) {
            TB_Node* a = &f->nodes.data[n->i_arith.a];
            TB_Node* b = &f->nodes.data[n->i_arith.b];
            TB_DataType dt = n->dt;
			
            if (a->type == TB_SIGNED_CONST || b->type == TB_UNSIGNED_CONST) {
                uint64_t b_const = b->uint.value;
				
                int log2 = tb_ffs(b_const) - 1;
                if (b_const == (1 << log2)) {
                    // It's a power of two, swap in a left-shift
                    // just slap it right after the label
                    TB_Reg new_op = tb_function_insert_after(f, i);
					
                    f->nodes.data[new_op].type = TB_UNSIGNED_CONST;
                    f->nodes.data[new_op].dt   = dt;
                    f->nodes.data[new_op].uint = (struct TB_NodeUint) { log2 };
					
                    n->type = TB_SHL;
					n->dt = dt;
                    n->i_arith = (struct TB_NodeIArith) { .a = a - f->nodes.data, .b = new_op };
                    changes++;
                }
            }
        }
    }
	
    return (changes > 0);
}
