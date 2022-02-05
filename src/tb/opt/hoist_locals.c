#include "../tb_internal.h"

// We just move them up because it's slightly easier to think about them
bool tb_opt_hoist_locals(TB_Function* f) {
	size_t locals_to_move = 0;
	
	TB_Node* entry_terminator = &f->nodes.data[f->nodes.data[1].label.terminator];
	for (TB_Node* n = entry_terminator; n != &f->nodes.data[0]; n = &f->nodes.data[n->next]) {
		locals_to_move += (n->type == TB_LOCAL);
	}
	
	if (locals_to_move == 0) {
		return false;
	}
	
	// place to start putting all the locals 
	// must go after the parameters
	TB_Reg local_basepoint = 1;
	
	// keep moving locals until we run out
	for (TB_Node* n = &f->nodes.data[1]; locals_to_move > 0; n = &f->nodes.data[n->next]) {
		if (n->type == TB_LOCAL) {
			// move to the entry block
			TB_Reg new_reg = tb_function_insert_after(f, local_basepoint);
			TB_Node* new_node = &f->nodes.data[new_reg];
			
			*new_node = *n;
			n->type = TB_NULL;
			locals_to_move--;
			
			tb_function_find_replace_reg(f, n - f->nodes.data, new_reg);
		} else if (n->type != TB_PARAM && n->type != TB_PARAM_ADDR) {
			local_basepoint = (n - f->nodes.data);
		}
	}
	
	return true;
}
