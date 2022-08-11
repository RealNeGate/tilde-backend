#include "../tb_internal.h"

// TODO(NeGate): clean up this code
static bool try_jump_thread(TB_Function* f, TB_TemporaryStorage* tls, TB_Label* dst_label) {
    TB_Label label = *dst_label;
    TB_Reg target_reg = tb_find_reg_from_label(f, label);
    assert(target_reg != 0);

    TB_Node* target = &f->nodes[target_reg];

    // skip any NULL slots
    while (f->nodes[target->next].type == TB_NULL) {
        target = &f->nodes[target->next];
    }

    if (f->nodes[target->next].type == TB_GOTO) {
        if (label == 13) __debugbreak();

        TB_Label new_target_label = f->nodes[target->next].goto_.label;
        TB_Reg new_target_reg = tb_find_reg_from_label(f, new_target_label);
        assert(new_target_reg != 0);

        *dst_label = new_target_label;

        // figure out if we've removed all paths from the old label
        bool replace_old = true;
        {
            // TODO(NeGate): Create a proper caching system for this info
            int pred_count = 0;
            TB_Label* preds = tb_tls_push(tls, 0);
            tb_calculate_immediate_predeccessors(f, tls, new_target_label, &pred_count);

            FOREACH_N(i, 0, pred_count) {
                if (preds[i] == label) {
                    replace_old = false;
                    break;
                }
            }

            tb_tls_restore(tls, preds);
        }

        // reorganize PHI nodes
        TB_Reg terminator = f->nodes[new_target_reg].label.terminator;
        TB_FOR_EACH_NODE_RANGE(n, f, new_target_reg, terminator) {
            TB_Reg r = n - f->nodes;

            if (tb_node_is_phi_node(f, r)) {
                int count = tb_node_get_phi_width(f, r);
                TB_PhiInput* inputs = tb_node_get_phi_inputs(f, r);

                if (replace_old) {
                    OPTIMIZER_LOG(r, "Replaced extra path to the phi node via jump threading");
                    FOREACH_N(j, 0, count) {
                        if (inputs[j].label == target_reg) inputs[j].label = new_target_label;
                    }
                } else {
                    TB_Reg found = 0;
                    FOREACH_N(j, 0, count) {
                        if (inputs[j].label == target_reg) {
                            found = inputs[j].val;
                            break;
                        }
                    }

                    if (found) {
                        OPTIMIZER_LOG(r, "Added extra path to the phi node via jump threading");

                        TB_PhiInput* new_inputs = tb_platform_heap_alloc((count + 1) * sizeof(TB_PhiInput));
                        memcpy(new_inputs, inputs, count * sizeof(TB_PhiInput));
                        new_inputs[count] = (TB_PhiInput){ .label = new_target_reg, .val = found };

                        n->type = TB_PHIN;
                        n->phi.count = count + 1;
                        n->phi.inputs = new_inputs;
                    } else {
                        OPTIMIZER_LOG(r, "Jump threading didn't add extra path since it leads to NULL node");
                    }
                }
            }
        }

        return true;
    }

    return false;
}

static void replace_label(TB_Function* f, TB_Node* seq, TB_Label label, TB_Reg label_reg) {
    // replace all by-label references
    TB_Label old_label = seq->label.id;
    tb_murder_node(f, seq);

    TB_FOR_EACH_NODE(m, f) {
        if (m->type == TB_GOTO) {
            if (m->goto_.label == old_label) m->goto_.label = label;
        } else if (m->type == TB_IF) {
            if (m->if_.if_true == old_label) m->if_.if_true = label;
            if (m->if_.if_false == old_label) m->if_.if_false = label;
        } else if (m->type == TB_SWITCH) {
            size_t entry_start = m->switch_.entries_start;
            size_t entry_count = (m->switch_.entries_end - m->switch_.entries_start) / 2;

            for (size_t j = 0; j < entry_count; j++) {
                TB_SwitchEntry* e = (TB_SwitchEntry*)&f->vla.data[entry_start + (j * 2)];
                if (e->value == old_label) e->value = label;
            }

            if (m->switch_.default_label == old_label) m->switch_.default_label = label;
        }
    }

    // replace any by-register references
    TB_Reg j = seq - f->nodes;
    tb_function_find_replace_reg(f, j, label_reg);
}

bool tb_opt_compact_dead_regs(TB_Function* f) {
    int changes = 0;

    TB_Node* nodes = f->nodes;
    TB_Node* n = &f->nodes[1];
    TB_Node* prev = &f->nodes[1];

    // Find a NULL, skip over any NULLs until a valid node is found and cut out the middle men
    while (n != &nodes[0]) {
        if (n->type == TB_NULL) {
            tb_assume(prev != n);
            do {
                n = &nodes[n->next];
            } while (n->type == TB_NULL && n != &nodes[0] /* node0 is exit */);

            prev->next = n - nodes;
            changes++;
        }

        prev = n;
        n = &nodes[n->next];
    }

    return changes;
}

bool tb_opt_remove_pass_node(TB_Function* f) {
    int changes = 0;
    TB_FOR_EACH_NODE(n, f) {
        TB_Reg i = (n - f->nodes);

        if (n->type == TB_PASS) {
            OPTIMIZER_LOG(i, "Replacing PASS with r%d", n->unary.src);
            tb_function_find_replace_reg(f, i, n->unary.src);

            n->type = TB_NULL;
            changes++;
        }
    }

    return changes;
}

bool tb_opt_jump_threading(TB_Function* f) {
    TB_TemporaryStorage* tls = tb_tls_allocate();
    int changes = 0;

    TB_FOR_EACH_NODE(n, f) {
        TB_Reg i = (n - f->nodes);

        if (n->type == TB_IF) {
            if (try_jump_thread(f, tls, &n->if_.if_true)) {
                OPTIMIZER_LOG(i, "jump threading");
                changes++;
            }

            if (try_jump_thread(f, tls, &n->if_.if_false)) {
                OPTIMIZER_LOG(i, "jump threading");
                changes++;
            }
        } else if (n->type == TB_GOTO) {
            if (try_jump_thread(f, tls, &n->goto_.label)) {
                OPTIMIZER_LOG(i, "jump threading");
                changes++;
            }
        }
    }

    return changes;
}

static bool tb_opt_canonicalize_phase1(TB_Function* f) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    int changes = 0;
    TB_FOR_EACH_NODE(n, f) {
        TB_Reg i = (n - f->nodes);
        TB_NodeTypeEnum type = n->type;

        // TODO(NeGate): Maybe we should have a proper function/macro
        // for detecting integer compares like this
        if (type >= TB_CMP_EQ && type <= TB_CMP_ULE) {
            TB_Node* a = &f->nodes[n->cmp.a];
            TB_Node* b = &f->nodes[n->cmp.b];

            // (cmpeq (cmpeq a 0) 0) => a
            if (type == TB_CMP_EQ && tb_node_is_constant_zero(f, n->cmp.b) &&
                a->type == TB_CMP_EQ && tb_node_is_constant_zero(f, a->cmp.b)) {
                OPTIMIZER_LOG(i, "removed redundant comparisons");

                n->type = TB_PASS;
                n->pass.value = a->cmp.a;
            } else {
                // Sometimes we promote some types up when we don't need to
                // (cmp (sxt/zxt A) (int B))
                // VVV
                // (cmp A (int B))
                if (a->type == TB_SIGN_EXT && b->type == TB_INTEGER_CONST && TB_DATA_TYPE_EQUALS(f->nodes[a->unary.src].dt, b->dt)) {
                    OPTIMIZER_LOG(i, "removed unnecessary sign extension for compare against constants");

                    n->cmp.a = a->unary.src;
                    changes++;
                    continue;
                } else if (a->type == TB_ZERO_EXT && b->type == TB_INTEGER_CONST && TB_DATA_TYPE_EQUALS(f->nodes[a->unary.src].dt, b->dt)) {
                    OPTIMIZER_LOG(i, "removed unnecessary zero extension for compare against constants");

                    n->cmp.a = a->unary.src;
                    changes++;
                    continue;
                }
            }
        }

        if (type == TB_ADD || type == TB_MUL ||
            type == TB_AND || type == TB_XOR ||
            type == TB_CMP_NE || type == TB_CMP_EQ) {
            // NOTE(NeGate): compares alias the operands with i_arith so it's
            // alright to group them here.
            TB_Node* a = &f->nodes[n->i_arith.a];
            TB_Node* b = &f->nodes[n->i_arith.b];
            TB_ArithmaticBehavior ab = n->i_arith.arith_behavior;

            // Move all integer constants to the right side
            bool is_aconst = (a->type == TB_INTEGER_CONST);
            bool is_bconst = (b->type == TB_INTEGER_CONST);

            if (is_aconst && !is_bconst) {
                OPTIMIZER_LOG(i, "moved constants to right hand side.");

                tb_swap(TB_Reg, n->i_arith.a, n->i_arith.b);
                changes++;
            } else if (a->type == type && is_bconst) {
                // Reshuffle the adds from
                // (x + y) + z => x + (y + z)
                OPTIMIZER_LOG(i, "Reassociated expressions");

                TB_Reg x = a->i_arith.a;
                TB_Reg y = a->i_arith.b;
                TB_Reg z = b - f->nodes;

                TB_Reg extra_reg = tb_function_insert_after(f, n->i_arith.b);
                TB_Node* extra = &f->nodes[extra_reg];
                extra->type = type;
                extra->dt = n->dt;
                extra->i_arith.a = y;
                extra->i_arith.b = z;
                extra->i_arith.arith_behavior = ab;

                n = &f->nodes[i];
                n->i_arith.a = x;
                n->i_arith.b = extra_reg;
                changes++;
            }
        } else if (type == TB_UMOD || type == TB_SMOD) {
            TB_Node* b = &f->nodes[n->i_arith.b];

            // (mod a N) => (and a N-1) where N is a power of two
            if (b->type == TB_INTEGER_CONST && b->integer.num_words == 1) {
                uint64_t mask = b->integer.single_word;

                if (tb_is_power_of_two(mask)) {
                    OPTIMIZER_LOG(i, "converted modulo into AND with constant mask");

                    // generate mask
                    TB_Reg extra_reg = tb_function_insert_after(f, n->i_arith.b);
                    TB_Node* extra = &f->nodes[extra_reg];
                    extra->type = TB_INTEGER_CONST;
                    extra->dt = n->dt;
                    extra->integer.num_words = 1;
                    extra->integer.single_word = mask - 1;

                    // new AND operation to replace old MOD
                    n = &f->nodes[i];
                    n->type = TB_AND;
                    n->i_arith.b = extra_reg;
                    changes++;
                }
            }
        } else if (type == TB_INITIALIZE) {
            TB_Reg addr = n->init.addr;
            TB_Initializer* init = n->init.src;

            if (init->obj_count == 0) {
                OPTIMIZER_LOG(i, "Replaced complex initializer with memset");

                TB_Reg imm_reg = tb_function_insert_after(f, addr);
                f->nodes[imm_reg].type = TB_INTEGER_CONST;
                f->nodes[imm_reg].dt = TB_TYPE_I8;
                f->nodes[imm_reg].integer = (struct TB_NodeInt) {
                    .num_words = 1,
                    .single_word = 0
                };

                TB_Reg size_reg = tb_function_insert_after(f, imm_reg);
                f->nodes[size_reg].type = TB_INTEGER_CONST;
                f->nodes[size_reg].dt = TB_TYPE_PTR;
                f->nodes[size_reg].integer = (struct TB_NodeInt) {
                    .num_words = 1,
                    .single_word = init->size
                };

                n = &f->nodes[i];
                n->type = TB_MEMSET;
                n->mem_op.dst = addr;
                n->mem_op.src = imm_reg;
                n->mem_op.size = size_reg;
                n->mem_op.align = init->align;
                changes++;
            }
        } else if (type == TB_MEMBER_ACCESS) {
            TB_Node* base = &f->nodes[n->member_access.base];

            if (base->type == TB_MEMBER_ACCESS) {
                uint32_t offset = n->member_access.offset;
                offset += base->member_access.offset;

                if (!TB_FITS_INTO(int32_t, offset)) {
                    OPTIMIZER_LOG(i, "FAILURE cannot fold into member access without overflow");
                } else {
                    TB_Reg base_base = base->member_access.base;

                    n->member_access.base = base_base;
                    n->member_access.offset = offset;
                    changes++;
                }
            } else {
                int32_t offset = n->member_access.offset;

                if (offset == 0) {
                    OPTIMIZER_LOG(i, "elided member access to first element");

                    n->type = TB_PASS;
                    n->pass.value = n->member_access.base;
                    changes++;
                }
            }
        } else if (type == TB_ARRAY_ACCESS) {
            TB_Node* index = &f->nodes[n->array_access.index];

            if (index->type == TB_INTEGER_CONST && index->integer.num_words == 1) {
                uint64_t index_imm = index->integer.single_word;

                uint64_t res = n->array_access.stride * index_imm;
                if (!TB_FITS_INTO(int32_t, res)) {
                    OPTIMIZER_LOG(i, "FAILURE cannot fold into array access without overflow");
                } else {
                    // success!
                    OPTIMIZER_LOG(i, "folded constant array access");
                    TB_Reg base_reg = n->array_access.base;

                    n->type = TB_MEMBER_ACCESS;
                    n->member_access.base = base_reg;
                    n->member_access.offset = res;
                    changes++;
                }
            } else if (tb_node_is_constant_zero(f, n->array_access.index)) {
                OPTIMIZER_LOG(i, "elided array access to first element");

                n->type = TB_PASS;
                n->pass.value = n->array_access.base;
                changes++;
            } else if (index->type == TB_MUL) {
                TB_Node* potential_constant = &f->nodes[index->i_arith.b];

                if (potential_constant->type == TB_INTEGER_CONST && potential_constant->integer.num_words == 1) {
                    // don't worry it doesn't loop i just needed to have 'break' support
                    do {
                        uint64_t factor = potential_constant->integer.single_word;
                        if (!TB_FITS_INTO(int32_t, factor)) {
                            OPTIMIZER_LOG(i, "FAILURE multiply cannot fold into array access because too big");
                            break;
                        }

                        uint64_t res = n->array_access.stride * factor;
                        if (!TB_FITS_INTO(int32_t, res)) {
                            OPTIMIZER_LOG(i, "FAILURE multiply cannot fold into array access without overflow");
                            break;
                        }

                        // success!
                        OPTIMIZER_LOG(i, "folded multiply into array access");
                        n->array_access.index = index->i_arith.a;
                        n->array_access.stride = res;
                        changes++;
                    } while (0);
                }
            } else if (index->type == TB_ADD) {
                // (array A (add B O) C) => (member (array A B C) O*C)
                TB_Node* potential_constant = &f->nodes[index->i_arith.b];

                if (potential_constant->type == TB_INTEGER_CONST && potential_constant->integer.num_words == 1) {
                    TB_CharUnits c = n->array_access.stride;
                    uint64_t res = potential_constant->integer.single_word * c;

                    if (res < UINT32_MAX) {
                        OPTIMIZER_LOG(i, "converted add into member access");
                        TB_Reg new_array_reg = tb_function_insert_after(f, n->array_access.index);

                        TB_Reg a = n->array_access.base;
                        TB_Reg b = index->i_arith.a;

                        n = &f->nodes[i];
                        n->type = TB_MEMBER_ACCESS;
                        n->dt = TB_TYPE_PTR;
                        n->member_access.base = new_array_reg;
                        n->member_access.offset = potential_constant->integer.single_word * c;

                        TB_Node* new_array = &f->nodes[new_array_reg];
                        new_array->type = TB_ARRAY_ACCESS;
                        new_array->dt = TB_TYPE_PTR;
                        new_array->array_access.base = a;
                        new_array->array_access.index = b;
                        new_array->array_access.stride = c;
                        changes++;
                    }
                }
            }
        } else if (type == TB_INT2PTR) {
            TB_Node* src = &f->nodes[n->unary.src];

            if (src->type == TB_INTEGER_CONST && src->integer.num_words == 1) {
                OPTIMIZER_LOG(i, "constant int2ptr removed.");

                uint64_t imm = src->integer.single_word;

                n->type = TB_INTEGER_CONST;
                // preserve the int2ptr's pointer type
                n->integer.num_words = 1;
                n->integer.single_word = imm;
                changes++;
            }
        } else if (type == TB_IF) {
            TB_Node* cond = &f->nodes[n->if_.cond];

            if (cond->type == TB_STRING_CONST) {
                // (if str B C) => (goto B)
                TB_Label new_target = n->if_.if_true;

                n->type = TB_GOTO;
                n->dt = TB_TYPE_VOID;
                n->goto_.label = new_target;
                changes++;
            } else if (cond->type == TB_INTEGER_CONST) {
                // (if A B C) => (goto X) where X = A ? B : C
                TB_Label new_target = !tb_node_is_constant_zero(f, n->if_.cond) ?
                    n->if_.if_true : n->if_.if_false;

                n->type = TB_GOTO;
                n->dt = TB_TYPE_VOID;
                n->goto_.label = new_target;
                changes++;
            } else if (cond->type == TB_CMP_NE && tb_node_is_constant_zero(f, cond->cmp.b)) {
                // (if (cmpne A 0) B C) => (if A B C)
                OPTIMIZER_LOG(i, "removed redundant compare-to-zero on if node");

                TB_DataType dt = f->nodes[cond->cmp.a].dt;

                n->dt = dt;
                n->if_.cond = cond->cmp.a;
                changes++;
            } else if (cond->type == TB_CMP_EQ && tb_node_is_constant_zero(f, cond->cmp.b)) {
                // (if (cmpeq A 0) B C) => (if A C B)
                OPTIMIZER_LOG(i, "removed redundant compare-to-zero on if node");

                TB_DataType dt = f->nodes[cond->cmp.a].dt;

                n->dt = dt;
                n->if_.cond = cond->cmp.a;
                tb_swap(TB_Label, n->if_.if_true, n->if_.if_false);
                changes++;
            }
        } else if (tb_node_is_phi_node(f, i)) {
            int count = tb_node_get_phi_width(f, i);
            TB_PhiInput* inputs = tb_node_get_phi_inputs(f, i);

            if (count == 0) {
                tb_murder_node(f, n);
            } else if (count == 1) {
                OPTIMIZER_LOG(i, "removed trivial phi");

                TB_Reg r = inputs[0].val;
                assert(r > 0 && r < f->node_count);

                if (n->type == TB_PHIN) tb_platform_heap_free(inputs);

                // remove useless phi
                n->type = TB_PASS;
                n->pass.value = r;
                changes++;
            } else {
                // Check for any duplicate inputs
                size_t new_length = 0;
                TB_PhiInput* new_inputs = tb_tls_push(tls, 0);

                loop(j, count) {
                    TB_Reg a = inputs[j].val;
                    TB_Reg b = inputs[j].label;

                    bool duplicate = false;
                    if (a == i) {
                        duplicate = true;
                    } else if (f->nodes[a].type != TB_NULL) {
	                    loop_range(k, 0, j) {
	                        if (inputs[k].val == a && inputs[k].label == b) {
	                            duplicate = true;
	                            break;
	                        }
	                    }
                    } else {
                        duplicate = true;
                    }

                    if (!duplicate) {
                        if (a != TB_NULL_REG && f->nodes[a].type == TB_NULL) {
                            a = TB_NULL_REG;
                        }

                        tb_tls_push(tls, sizeof(TB_PhiInput));
                        new_inputs[new_length++] = (TB_PhiInput){ .label = b, .val = a };
                    }
                }

                if (new_length != count) {
                    // Pass it off to more permanent storage
                    if (n->type == TB_PHIN) {
                        tb_platform_heap_free(inputs);
                    }

                    if (new_length == 0) {
                        OPTIMIZER_LOG(i, "Deduplicated away the PHI node");
                        n->type = TB_NULL;
                    } else {
                        OPTIMIZER_LOG(i, "Deduplicated PHI node entries");
                        TB_PhiInput* more_permanent_store = tb_platform_heap_alloc(new_length * sizeof(TB_PhiInput));
                        memcpy(more_permanent_store, new_inputs, new_length * sizeof(TB_PhiInput));

                        n->type = TB_PHIN;
                        n->phi.count = new_length;
                        n->phi.inputs = more_permanent_store;
                        changes++;
                    }
                }
                tb_tls_restore(tls, new_inputs);
            }
        } else if (n->type == TB_LABEL) {
            if (f->nodes[n->label.terminator].type == TB_GOTO) {
                TB_Reg target_reg = tb_find_reg_from_label(f, f->nodes[n->label.terminator].goto_.label);
                TB_Node* target = &f->nodes[target_reg];

                if (tb_node_is_phi_node(f, target->next)) {
                    //  goto Lx <-- we can jump thread this
                    //  ...
                    // Lx:
                    //  a = i1 1
                    //  goto Ly
                    // Ly:
                    //  ...
                    // Lz:
                    //  merge = phi(Lx:a, Lz:b)
                    //  if (merge) Li else Lj
                    TB_Node* if_node = &f->nodes[f->nodes[target->next].next];
                    if (if_node->type == TB_IF && if_node->if_.cond == target->next) {
                        int count = tb_node_get_phi_width(f, target->next);
                        TB_PhiInput* inputs = tb_node_get_phi_inputs(f, target->next);

                        int input_slot = -1;
                        loop(j, count) {
                            if (inputs[j].label == i) {
                                input_slot = j;
                                break;
                            }
                        }

                        TB_Reg match = input_slot >= 0 ? inputs[input_slot].val : TB_NULL_REG;
                        if (f->nodes[match].type == TB_INTEGER_CONST) {
                            OPTIMIZER_LOG(i, "jump threading through PHI node");

                            // remove this from the inputs list
                            switch (f->nodes[match].type) {
                                case TB_PHI1: tb_murder_node(f, &f->nodes[match]); break;
                                case TB_PHI2: f->nodes[match].type = TB_PHI1; break;
                                case TB_PHIN: f->nodes[match].phi.count -= 1; break;
                                default: break;
                            }

                            // remove swap amirite
                            if (count == 1) {
                                OPTIMIZER_LOG(match, "due to jump threading, PHI node became PASS due to only having a single pred");
                                TB_Reg src = inputs[input_slot].val;

                                f->nodes[match].type = TB_PASS;
                                f->nodes[match].pass.value = src;
                            } else if (count > 1) {
                                inputs[input_slot] = inputs[count-1];
                            }

                            // fold original GOTO
                            TB_Label new_target = !tb_node_is_constant_zero(f, match) ?
                                if_node->if_.if_true : if_node->if_.if_false;

                            f->nodes[n->label.terminator].goto_.label = new_target;
                            changes++;

                            // just don't run the stuff below, move on...
                            continue;
                        }
                    }
                }
            }

            TB_Node* end = f->nodes;
            TB_Reg bb_start = 0;
            TB_Reg bb_end = 0;

            // Find sequence of labels
            int count = 0;
            {
                TB_Node* seq = n;
                while (seq = &f->nodes[seq->next],
                    seq->type == TB_LABEL && seq != end) {
                    bb_end = seq->label.terminator;
                    count += 1;
                }

                bb_start = (seq - f->nodes);
            }

            if (count > 0) {
                TB_Label label = n->label.id;

                TB_Node* seq = &f->nodes[n->next];
                do {
                    OPTIMIZER_LOG(i, "merge labels r%d", (TB_Reg)(seq - f->nodes));
                    replace_label(f, seq, label, i);
                    seq = &f->nodes[seq->next];
                } while (seq->type == TB_LABEL && seq != end);

                assert(bb_end >= 0);
                n->next = bb_start;
                n->label.terminator = bb_end;
                changes++;
            }
        }
    }

    return (changes > 0);
}

TB_API bool tb_opt_canonicalize(TB_Function* f) {
    static TB_FunctionPass opts[] = {
        { "canonicalize_phase1", tb_opt_canonicalize_phase1 },
        // { "jump_threading",      tb_opt_jump_threading },
        { "subexpr_elim",        tb_opt_subexpr_elim },
        { "remove_pass_node",    tb_opt_remove_pass_node },
        { "fold",                tb_opt_fold },
        { "strength",            tb_opt_strength_reduction },
        { "dead_expr_elim",      tb_opt_dead_expr_elim }
    };

    bool changes = false;
    retry: {
        if (tb_function_optimize(f, COUNTOF(opts), opts)) {
            changes = true;
            goto retry;
        }
    }

    return changes;
}
