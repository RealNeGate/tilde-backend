#include "../tb_internal.h"

bool tb_opt_subexpr_elim(TB_Function* f) {
	bool changes = false;
	TB_Reg bb_start = 1;

	TB_FOR_EACH_NODE(n, f) {
		TB_Reg i = (n - f->nodes.data);
		TB_NodeTypeEnum type = n->type;

		if (type == TB_LABEL) {
			bb_start = n - f->nodes.data;
		} else if (type == TB_SIGN_EXT || type == TB_ZERO_EXT) {
			TB_Reg src = n->unary.src;
			TB_DataType dt = n->dt;

			TB_FOR_EACH_NODE_BB(other, f, i) {
				if (other->type == type && other->unary.src == src &&
					TB_DATA_TYPE_EQUALS(other->dt, dt)) {
					OPTIMIZER_LOG(i, "merged extension operations");

					other->type = TB_PASS;
					other->pass.value = i;
					changes = true;
				}
			}
		} else if (type == TB_UNSIGNED_CONST || type == TB_SIGNED_CONST) {
			uint64_t data = n->uint.value;
			TB_DataType dt = n->dt;

			TB_FOR_EACH_NODE_BB(other, f, i) {
				if (other->type == type && other->uint.value == data &&
					TB_DATA_TYPE_EQUALS(other->dt, dt)) {
					OPTIMIZER_LOG(i, "merged integer constants");

					other->type = TB_PASS;
					other->pass.value = i;
					changes = true;
				}
			}
		}
	}

	return changes;
}

bool tb_opt_hoist_invariants(TB_Function* f) {
	bool changes = false;
	TB_Label current_label = 0;

	TB_FOR_EACH_NODE(n, f) {
		TB_NodeTypeEnum type = n->type;
		TB_Reg i = (n - f->nodes.data);

		if (type == TB_LABEL) {
			current_label = n->label.id;
		} else if (type == TB_SIGN_EXT || type == TB_ZERO_EXT) {
			TB_Reg src = n->unary.src;
			TB_DataType dt = n->dt;

			// check if we already exist within the src's basic block
			bool already_in_same_bb = false;
			TB_FOR_EACH_NODE_BB(other, f, src) {
				TB_Reg j = (other - f->nodes.data);

				if (other->type == type && other->unary.src == src &&
					TB_DATA_TYPE_EQUALS(other->dt, dt)) {
					if (i != j) {
						OPTIMIZER_LOG(i, "hoist extension operation");

						n->type = TB_PASS;
						n->pass.value = j;
						changes = true;
					}

					already_in_same_bb = true;
					break;
				}
			}

			if (!already_in_same_bb) {
				OPTIMIZER_LOG(i, "hoist extension operation");

				TB_Reg hoisted_reg = tb_function_insert_after(f, src);
				TB_Node* hoisted = &f->nodes.data[hoisted_reg];
				hoisted->type = type;
				hoisted->dt = dt;
				hoisted->unary.src = src;

				// change this location to point to our hoisted extension
				n->type = TB_PASS;
				n->pass.value = hoisted_reg;
				changes = true;
			}
		}
	}

	return changes;
}
