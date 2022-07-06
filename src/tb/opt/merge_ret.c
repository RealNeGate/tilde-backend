#include "../tb_internal.h"

bool tb_opt_merge_rets(TB_Function* f) {
	TB_Label label_reg = 0;

	int count = 0;
	TB_PhiInput* inputs = NULL;

	TB_Label endpoint = f->label_count;
	TB_DataType dt = TB_TYPE_VOID;
	TB_Node* the_goto_we_might_convert_back_if_we_fail = f->nodes;
	TB_FOR_EACH_NODE(n, f) {
		TB_Reg i = (n - f->nodes);

		if (n->type == TB_LABEL) {
			label_reg = i;
		} else if (n->type == TB_RET) {
			int index = count++;
			inputs = tb_platform_heap_realloc(inputs, count * sizeof(TB_PhiInput));
			inputs[index] = (TB_PhiInput){ label_reg, n->ret.value };

			dt = n->dt;

			n->type = TB_GOTO;
			n->dt = TB_TYPE_VOID;
			n->goto_.label = endpoint;
			the_goto_we_might_convert_back_if_we_fail = n;
		}
	}

	if (count > 1) {
	    f->label_count += 1;

		TB_Reg new_label_reg = tb_function_insert_after(f, f->node_end);
		TB_Reg new_phi_reg = tb_function_insert_after(f, new_label_reg);
		TB_Reg new_ret_reg = tb_function_insert_after(f, new_phi_reg);
		OPTIMIZER_LOG(new_label_reg, "Insert new PHI node");

		f->nodes[new_label_reg].type = TB_LABEL;
		f->nodes[new_label_reg].dt = TB_TYPE_PTR;
		f->nodes[new_label_reg].label = (struct TB_NodeLabel){
			.id = endpoint,
			.terminator = new_ret_reg
		};

		f->nodes[new_phi_reg].type = TB_PHIN;
		f->nodes[new_phi_reg].dt = dt;
		f->nodes[new_phi_reg].phi = (struct TB_NodePhi){
			.count = count,
			.inputs = inputs
		};

		f->nodes[new_ret_reg].type = TB_RET;
		f->nodes[new_ret_reg].dt = dt;
		f->nodes[new_ret_reg].ret = (struct TB_NodeReturn){
			.value = new_phi_reg
		};

		return true;
	} else {
		if (inputs != NULL) {
			TB_Node* n = the_goto_we_might_convert_back_if_we_fail;
			n->type = TB_RET;
			n->dt = dt;
			n->ret.value = inputs[0].val;

			tb_platform_heap_free(inputs);
		}

		return false;
	}
}
