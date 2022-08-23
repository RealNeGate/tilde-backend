#include "../tb_internal.h"

// We just move them up because it's slightly easier to think about them
static bool hoist_locals(TB_Function* f) {
    size_t locals_to_move = 0;

    TB_Node* entry_terminator = &f->nodes[f->nodes[1].label.terminator];
    for (TB_Node* n = entry_terminator; n != &f->nodes[0]; n = &f->nodes[n->next]) {
        locals_to_move += (n->type == TB_LOCAL);
    }

    if (locals_to_move == 0) {
		return false;
	}

    // place to start putting all the locals
    // must go after the parameters
    TB_Reg local_basepoint = 1;
    bool is_past_entry_bb = false;

    // keep moving locals until we run out
    for (TB_Node* n = &f->nodes[1]; locals_to_move > 0; n = &f->nodes[n->next]) {
        if (is_past_entry_bb) {
            if (n->type == TB_LOCAL) {
                // move to the entry block
                TB_Reg new_reg = tb_function_insert_after(f, local_basepoint);
                TB_Node* new_node = &f->nodes[new_reg];

                TB_Reg new_reg_next = new_node->next;
                memcpy(new_node, n, sizeof(TB_Node));
                new_node->next = new_reg_next;

                n->type = TB_NULL;
                locals_to_move--;

                OPTIMIZER_LOG(n - f->nodes, "hoisted local");
                tb_function_find_replace_reg(f, n - f->nodes, new_reg);
            }
        } else {
            if (n->type == TB_LABEL) {
                is_past_entry_bb = (n->label.id != 0);
            } else if (n->type != TB_PARAM && n->type != TB_PARAM_ADDR) {
                local_basepoint = (n - f->nodes);
            }
        }
    }

    return true;
}

TB_API TB_Pass tb_opt_hoist_locals(void) {
    return (TB_Pass){
        .mode = TB_FUNCTION_PASS,
        .name = "HoistLocals",
        .func_run = hoist_locals,
    };
}
