// Based on "Simple and Efficient SSA Construction":
// https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf
#include "../tb_internal.h"

typedef enum {
    COHERENCY_GOOD,

    // failure states
    COHERENCY_USES_ADDRESS,
    COHERENCY_BAD_DATA_TYPE,
    COHERENCY_VOLATILE
} Coherency;

typedef struct Mem2Reg_Ctx {
    TB_TemporaryStorage* tls;
    TB_Function*         f;
    size_t               label_count;

    // Stack slots we're going to convert into
    // SSA form
    size_t  to_promote_count;
    TB_Reg* to_promote;

    // [to_promote_count][label_count]
    TB_Reg* current_def;

    // [to_promote_count][label_count]
    TB_Reg* incomplete_phis;

    // [label_count]
    bool* sealed_blocks;

    // [label_count]
    int* pred_count;
    // [label_count][pred_count[i]]
    TB_Label** preds;
} Mem2Reg_Ctx;

static Coherency tb_get_stack_slot_coherency(TB_Function* f, TB_Reg address, TB_DataType* dt);

static int get_variable_id(Mem2Reg_Ctx* restrict c, TB_Reg r) {
    // TODO(NeGate): Maybe we speed this up... maybe it doesn't matter :P
    loop(i, c->to_promote_count) {
        if (c->to_promote[i] == r) return (int)i;
    }

    return -1;
}

// NOTE(NeGate): Be careful when using this function because it will shift over registers
// it does correct the ones in the context but not any you loaded because... i cant?
// I just recommend not caching indices across this function.
//
// This doesn't really generate a PHI node, it just produces a NULL node which will
// be mutated into a PHI node by the rest of the code.
static TB_Reg new_phi(Mem2Reg_Ctx* restrict c, TB_Function* f, int var, TB_Label block, TB_DataType dt) {
    TB_Reg label_reg = tb_find_reg_from_label(f, block);

    TB_Reg new_phi_reg = tb_function_insert_after(f, label_reg);
    OPTIMIZER_LOG(new_phi_reg, "Insert new PHI node");
	f->nodes.data[new_phi_reg].dt = dt;

    return new_phi_reg;
}

static TB_Reg read_variable_recursive(Mem2Reg_Ctx* restrict c, int var, TB_Label block);
static TB_Reg try_remove_trivial_phi(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Reg phi_reg);
static TB_Reg add_phi_operands(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Reg phi_reg, TB_Label label, int var);
static void add_phi_operand(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Reg phi_reg, TB_Label label, TB_Reg reg);

////////////////////////////////
// Algorithm 1: Implementation of local value numbering
////////////////////////////////
static void write_variable(Mem2Reg_Ctx* c, int var, TB_Label block, TB_Reg value) {
    c->current_def[(var * c->label_count) + block] = value;
}

static TB_Reg read_variable(Mem2Reg_Ctx* restrict c, int var, TB_Label block) {
    if (c->current_def[(var * c->label_count) + block] != 0) {
        return c->current_def[(var * c->label_count) + block];
    }

    return read_variable_recursive(c, var, block);
}

////////////////////////////////
// Algorithm 2: Implementation of global value numbering
////////////////////////////////
static TB_Reg read_variable_recursive(Mem2Reg_Ctx* restrict c, int var, TB_Label block) {
    TB_Reg val = 0;

    if (!c->sealed_blocks[block]) {
        // incomplete CFG
        val = new_phi(c, c->f, var, block, c->f->nodes.data[c->to_promote[var]].dt);
        c->incomplete_phis[(block * c->to_promote_count) + var] = val;
    } else if (c->pred_count[block] == 0) {
        // TODO(NeGate): Idk how to handle this ngl, i
        // don't think it's possible tho
        //abort();
	} else if (c->pred_count[block] == 1) {
        // Optimize the common case of one predecessor: No phi needed
        val = read_variable(c, var, c->preds[block][0]);
    } else {
        // Break potential cycles with operandless phi
        val = new_phi(c, c->f, var, block, c->f->nodes.data[c->to_promote[var]].dt);
        write_variable(c, var, block, val);
        val = add_phi_operands(c, c->f, val, block, var);
    }

    write_variable(c, var, block, val);
    return val;
}

static TB_Reg add_phi_operands(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Reg phi_reg, TB_Label block, int var) {
    // Determine operands from predecessors
    loop(i, c->pred_count[block]) {
        TB_Reg val = read_variable(c, var, c->preds[block][i]);
		if (val) add_phi_operand(c, c->f, phi_reg, c->preds[block][i], val);
    }

    return try_remove_trivial_phi(c, c->f, phi_reg);
}

// Algorithm 3: Detect and recursively remove a trivial phi function
static TB_Reg try_remove_trivial_phi(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Reg phi_reg) {
    // Walk past any pass nodes
    while (f->nodes.data[phi_reg].type == TB_PASS) {
        phi_reg = f->nodes.data[phi_reg].pass.value;
    }

    // Get operands
    TB_Node* phi_node = &f->nodes.data[phi_reg];
	if (!tb_node_is_phi_node(f, phi_reg)) {
		return phi_reg;
	}

	int count = tb_node_get_phi_width(f, phi_reg);
	TB_PhiInput* inputs = tb_node_get_phi_inputs(f, phi_reg);

	TB_Reg same = TB_NULL_REG;
    loop(i, count) {
        // Unique value or self−reference
        if (inputs[i].val == phi_reg || inputs[i].val == same) continue;

        // The phi merges at least two values: not trivial
        if (same != TB_NULL_REG) return phi_reg;

        same = inputs[i].val;
    }

    if (same == TB_NULL_REG) {
        // The phi is unreachable or in the start block
        return 0;
    }

    TB_Reg* uses = tb_tls_push(c->tls, f->nodes.count * sizeof(TB_Reg));
    int use_count = tb_function_find_uses_of_node(f, phi_reg, uses);

    // trim the memory to avoid wasting too much
    tb_tls_restore(c->tls, &uses[use_count]);

    // replace all references
    OPTIMIZER_LOG(phi_reg, "  renamed trivial PHI with PASS r%d", same);
    assert(same != TB_NULL_REG);
    phi_node->type = TB_PASS;
    phi_node->pass.value = same;

    // Try to recursively remove all phi users, which might have become trivial
    loop(i, use_count) if (uses[i] == phi_reg) {
		try_remove_trivial_phi(c, f, uses[i]);
	}

    return same;
}

static void add_phi_operand(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Reg phi_reg, TB_Label label, TB_Reg reg) {
    assert(reg >= 1 && reg < f->nodes.count);
    // assert(f->nodes.type[reg] != TB_NULL);
    // assert(f->nodes.dt[reg].type != TB_VOID);
    // assert(phi_reg != reg);

    // we're using NULL nodes as the baseline PHI0
    OPTIMIZER_LOG(phi_reg, "  adding r%d to PHI", reg);
    TB_DataType dt = f->nodes.data[reg].dt;

	TB_Node* phi_node = &f->nodes.data[phi_reg];
	phi_node->dt = dt;

	if (phi_node->type == TB_NULL) {
        phi_node->type = TB_PHI1;
        phi_node->phi2.inputs[0] = (TB_PhiInput){ tb_find_reg_from_label(f, label), reg };
		return;
	}

	int count = tb_node_get_phi_width(f, phi_reg);
	TB_PhiInput* inputs = tb_node_get_phi_inputs(f, phi_reg);

	if (count == 1) {
		phi_node->type = TB_PHI2;
		phi_node->phi2.inputs[0] = inputs[0];
        phi_node->phi2.inputs[1] = (TB_PhiInput){ tb_find_reg_from_label(f, label), reg };
	} else if (count == 2) {
        phi_node->type = TB_PHIN;

        TB_PhiInput* new_inputs = tb_platform_heap_alloc(3 * sizeof(TB_PhiInput));
		new_inputs[0] = inputs[0];
		new_inputs[1] = inputs[1];
		new_inputs[2] = (TB_PhiInput){ tb_find_reg_from_label(f, label), reg };

        phi_node->phi.count = 3;
		phi_node->phi.inputs = new_inputs;
    } else {
		size_t index = phi_node->phi.count++;
		phi_node->phi.inputs = tb_platform_heap_realloc(phi_node->phi.inputs, phi_node->phi.count * sizeof(TB_PhiInput));
		if (phi_node->phi.inputs == NULL) {
			tb_panic("Out of memory!\n");
		}

		phi_node->phi.inputs[index] = (TB_PhiInput) { tb_find_reg_from_label(f, label), reg };
    }
}

// Algorithm 4: Handling incomplete CFGs
static void seal_block(Mem2Reg_Ctx* restrict c, TB_Label block) {
	//printf("SEAL_BLOCK L%d\n", block);

	loop(i, c->to_promote_count) {
        TB_Reg phi_reg = c->incomplete_phis[(block * c->to_promote_count) + i];
        if (phi_reg) add_phi_operands(c, c->f, phi_reg, block, i);
    }

    c->sealed_blocks[block] = true;
}

static void try_seal_block(Mem2Reg_Ctx* restrict c, TB_Label block, int* seal_counter) {
	int remaining = --seal_counter[block];
	if (remaining == 0) {
		assert(c->sealed_blocks[block] == false);
		seal_block(c, block);
	}
}

// NOTE(NeGate): All locals were moved into the first basic block by
// opt_hoist_locals earlier
bool tb_opt_mem2reg(TB_Function* f) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    ////////////////////////////////
    // Decide which stack slots to promote
    ////////////////////////////////
    size_t to_promote_count = 0;
    TB_Reg* to_promote = tb_tls_push(tls, 0);

    TB_Node* entry_terminator = &f->nodes.data[f->nodes.data[1].label.terminator];
    for (TB_Node* n = &f->nodes.data[1]; n != entry_terminator; n = &f->nodes.data[n->next]) {
        TB_Reg i = n - f->nodes.data;

        if (n->type == TB_LOCAL || n->type == TB_PARAM_ADDR) {
            TB_DataType dt;
            Coherency coherence = tb_get_stack_slot_coherency(f, i, &dt);

            switch (coherence) {
				case COHERENCY_GOOD: {
					*((TB_Reg*)tb_tls_push(tls, sizeof(TB_Reg))) = i;
					to_promote_count++;

					n->dt = dt;

					OPTIMIZER_LOG(i, "promoting to IR register");
					break;
				}
				case COHERENCY_VOLATILE: {
					OPTIMIZER_LOG(i, "could not mem2reg a stack slot (volatile load/store)");
					break;
				}
				case COHERENCY_USES_ADDRESS: {
					OPTIMIZER_LOG(i, "could not mem2reg a stack slot (uses pointer arithmatic)");
					break;
				}
				case COHERENCY_BAD_DATA_TYPE: {
					OPTIMIZER_LOG(i, "could not mem2reg a stack slot (data type is too inconsistent)");
					break;
				}
				default: tb_todo();
            }
        }
    }

    if (to_promote_count == 0) {
        // doesn't need to mem2reg
        return false;
    }

    Mem2Reg_Ctx c = { 0 };
    c.tls = tls;
    c.f = f;

    c.to_promote_count = to_promote_count;
    c.to_promote = to_promote;

    c.label_count = f->label_count;
    c.current_def = tb_tls_push(tls, to_promote_count * c.label_count * sizeof(TB_Reg));
    memset(c.current_def, 0, to_promote_count * c.label_count * sizeof(TB_Reg));

    c.incomplete_phis = tb_tls_push(tls, to_promote_count * c.label_count * sizeof(TB_Reg));
    memset(c.incomplete_phis, 0, to_promote_count * c.label_count * sizeof(TB_Reg));

    // TODO(NeGate): Maybe we should bitpack this?
    c.sealed_blocks = tb_tls_push(tls, c.label_count * sizeof(bool));
    memset(c.sealed_blocks, 0, c.label_count * sizeof(bool));

	// counts how many blocks must be completed until we can seal a block
	//int* seal_counter = tb_tls_push(tls, c.label_count * sizeof(int));

    // Calculate all the immediate predecessors
    // First BB has no predecessors
    {
        c.pred_count = tb_tls_push(tls, c.label_count * sizeof(int));
        c.preds = tb_tls_push(tls, c.label_count * sizeof(TB_Label*));

        // entry label has no predecessors
        c.pred_count[0] = 0;
        c.preds[0] = NULL;

		//seal_counter[0] = 1;

        loop_range(j, 1, c.label_count) {
            c.preds[j] = (TB_Label*)tb_tls_push(tls, 0);
            tb_calculate_immediate_predeccessors(f, tls, j, &c.pred_count[j]);

			//seal_counter[j] = c.pred_count[j];
		}
    }

    TB_Label block = 0;
    TB_FOR_EACH_NODE(n, f) {
        TB_Reg i = n - f->nodes.data;

        switch (n->type) {
			case TB_LABEL: {
				block = n->label.id;
				break;
			}
			case TB_LOCAL: {
				int var = get_variable_id(&c, i);
				if (var >= 0) tb_kill_op(f, i);
				break;
			}
			case TB_PARAM_ADDR: {
				// Parameter stack slots map to parameter registers
				// so we need to tell mem2reg about that.
				int var = get_variable_id(&c, i);
				if (var >= 0) {
					write_variable(&c, var, block, n->param_addr.param);
					tb_kill_op(f, i);
				}
				break;
			}
			case TB_LOAD: {
				int var = get_variable_id(&c, n->load.address);
				if (var >= 0) {
					TB_Reg value = read_variable(&c, var, block);
					assert(value);

					n->type = TB_PASS;
					n->pass.value = value;
				}
				break;
			}
			case TB_STORE: {
				int var = get_variable_id(&c, n->store.address);
				if (var >= 0) {
					tb_kill_op(f, i);
					write_variable(&c, var, block, n->store.value);
				}
				break;
			}
        }
    }

    loop_range(j, 1, c.label_count) {
		seal_block(&c, j);
		//assert(c.sealed_blocks[j]);
	}

    return true;
}

// NOTE(NeGate): a stack slot is coherent when all loads and stores share
// the same type and alignment along with not needing any address usage.
static Coherency tb_get_stack_slot_coherency(TB_Function* f, TB_Reg address, TB_DataType* out_dt) {
    // if there's a difference between the times we want the value and the
    // times we want the address, then some address calculations are being done
    // and thus we can't mem2reg
    int use_count = 0;

#define X(reg) \
if (reg == address) use_count += 1;
    TB_FOR_EACH_NODE(n, f) {
        switch (n->type) {
            TB_FOR_EACH_REG_IN_NODE(X);
			default: tb_todo();
        }
    }
#undef X

    int value_based_use_count = 0;

    // pick the first load/store and use that as the baseline
    TB_DataType dt = TB_TYPE_VOID;
    bool initialized = false;
    for (TB_Node* n = &f->nodes.data[address]; n != &f->nodes.data[0]; n = &f->nodes.data[n->next]) {
        static_assert(offsetof(TB_Node, load.address) == offsetof(TB_Node, store.address),
					  "TB_Node::load.address == TB_Node::store.address");

        if ((n->type == TB_LOAD || n->type == TB_STORE) && n->load.address == address) {
            value_based_use_count += 1;

            if (n->load.is_volatile) {
                return COHERENCY_VOLATILE;
            } else {
                if (!initialized) dt = n->dt;
                else if (!TB_DATA_TYPE_EQUALS(dt, n->dt)) return COHERENCY_BAD_DATA_TYPE;
            }
        }
    }

    if (value_based_use_count != use_count) {
        return COHERENCY_USES_ADDRESS;
    }

    *out_dt = dt;
    return COHERENCY_GOOD;
}
