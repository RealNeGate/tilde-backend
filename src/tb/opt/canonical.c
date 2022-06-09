#include "../tb_internal.h"

static bool try_jump_thread(TB_Function* f, TB_Label label, TB_Label* new_label) {
    TB_Reg target_reg = tb_find_reg_from_label(f, label);
    TB_Node* target = &f->nodes.data[target_reg];

    if (f->nodes.data[target->next].type == TB_GOTO) {
        *new_label = f->nodes.data[target->next].goto_.label;
        return true;
    }

    return false;
}

static void try_garbage_collect(TB_Function* f, TB_TemporaryStorage* tls, TB_Label label) {
    int pred_count;
    TB_Label* preds = tb_tls_push(tls, 0);
    tb_calculate_immediate_predeccessors(f, tls, label, &pred_count);

    if (pred_count == 0) {
        TB_Reg label_reg = tb_find_reg_from_label(f, label);
        OPTIMIZER_LOG(label_reg, "Killed unused BB");

        // just murder every node in the BB and we should be good
        TB_FOR_EACH_NODE_BB(n, f, label_reg) {
            tb_murder_node(f, n);
        }
    }

    tb_tls_restore(tls, preds);
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
    TB_Reg j = seq - f->nodes.data;
    tb_function_find_replace_reg(f, j, label_reg);
}

bool tb_opt_compact_dead_regs(TB_Function* f) {
    int changes = 0;

    TB_Node* n = &f->nodes.data[1];
    TB_Node* prev = &f->nodes.data[1];
    TB_Node* end  = &f->nodes.data[0];

    // Find a NULL, skip over any NULLs until a valid node is found and cut out the middle men
    while (n != end) {
        if (n->type == TB_NULL) {
            tb_assume(prev != n);
            do {
                n = &f->nodes.data[n->next];
            } while (n->type == TB_NULL && n != end);

            prev->next = n - f->nodes.data;
            changes++;
        }

        prev = n;
        n = &f->nodes.data[n->next];
    }

    return changes;
}

bool tb_opt_remove_pass_node(TB_Function* f) {
    int changes = 0;
    TB_FOR_EACH_NODE(n, f) {
        TB_Reg i = (n - f->nodes.data);

        if (n->type == TB_PASS) {
            OPTIMIZER_LOG(i, "Replacing PASS with r%d", n->unary.src);
            tb_function_find_replace_reg(f, i, n->unary.src);

            n->type = TB_NULL;
            changes++;
        }
    }

    return changes;
}

bool tb_opt_canonicalize(TB_Function* f) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    int changes = 0;
    TB_FOR_EACH_NODE(n, f) {
        TB_Reg i = (n - f->nodes.data);
        TB_NodeTypeEnum type = n->type;

        // TODO(NeGate): Maybe we should have a proper function/macro
        // for detecting integer compares like this
        if (type >= TB_CMP_EQ && type <= TB_CMP_ULE) {
            // Sometimes we promote some types up when we don't need to
            TB_Node* a = &f->nodes.data[n->cmp.a];
            TB_Node* b = &f->nodes.data[n->cmp.b];

            // (cmp (sxt/zxt A) (int B))
            // VVV
            // (cmp A (int B))
            if (a->type == TB_SIGN_EXT && b->type == TB_INTEGER_CONST) {
                OPTIMIZER_LOG(i, "removed unnecessary zero extension for compare against constants");

                n->cmp.a = a->unary.src;
                changes++;
            } else if (a->type == TB_ZERO_EXT && b->type == TB_INTEGER_CONST) {
                OPTIMIZER_LOG(i, "removed unnecessary zero extension for compare against constants");

                n->cmp.a = a->unary.src;
                changes++;
            }
        } else if (type == TB_ADD || type == TB_MUL ||
                   type == TB_AND || type == TB_XOR ||
                   type == TB_CMP_NE || type == TB_CMP_EQ) {
            // NOTE(NeGate): compares alias the operands with i_arith so it's
            // alright to group them here.
            TB_Node* a = &f->nodes.data[n->i_arith.a];
            TB_Node* b = &f->nodes.data[n->i_arith.b];

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
                TB_Reg z = b - f->nodes.data;

                TB_Reg extra_reg = tb_function_insert_after(f, n->i_arith.b);
                TB_Node* extra = &f->nodes.data[extra_reg];
                extra->type = type;
                extra->dt = n->dt;
                extra->i_arith.a = y;
                extra->i_arith.b = z;

                n->i_arith.a = x;
                n->i_arith.b = extra_reg;
                changes++;
            }
        } else if (type == TB_UMOD || type == TB_SMOD) {
            TB_Node* b = &f->nodes.data[n->i_arith.b];

            // (mod a N) => (and a N-1) where N is a power of two
            if (b->type == TB_INTEGER_CONST && b->integer.num_words == 1) {
                uint64_t mask = b->integer.single_word;

                if (tb_is_power_of_two(mask)) {
                    OPTIMIZER_LOG(i, "converted modulo into AND with constant mask");

                    // generate mask
                    TB_Reg extra_reg = tb_function_insert_after(f, n->i_arith.b);
                    TB_Node* extra = &f->nodes.data[extra_reg];
                    extra->type = TB_INTEGER_CONST;
                    extra->dt = n->dt;
                    extra->integer.num_words = 1;
                    extra->integer.single_word = mask - 1;

                    // new AND operation to replace old MOD
                    n->type = TB_AND;
                    n->i_arith.b = extra_reg;
                    changes++;
                }
            }
        } else if (type == TB_MEMBER_ACCESS) {
            TB_Node* base = &f->nodes.data[n->member_access.base];

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
            TB_Node* index = &f->nodes.data[n->array_access.index];

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
                TB_Node* potential_constant = &f->nodes.data[index->i_arith.b];

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
                TB_Node* potential_constant = &f->nodes.data[index->i_arith.b];

                if (potential_constant->type == TB_INTEGER_CONST && potential_constant->integer.num_words == 1) {
                    TB_CharUnits c = n->array_access.stride;
                    uint64_t res = potential_constant->integer.single_word * c;

                    if (res < UINT32_MAX) {
                        OPTIMIZER_LOG(i, "converted add into member access");
                        TB_Reg new_array_reg = tb_function_insert_after(f, n->array_access.index);

                        TB_Reg a = n->array_access.base;
                        TB_Reg b = index->i_arith.a;

                        n->type = TB_MEMBER_ACCESS;
                        n->dt = TB_TYPE_PTR;
                        n->member_access.base = new_array_reg;
                        n->member_access.offset = potential_constant->integer.single_word * c;

                        TB_Node* new_array = &f->nodes.data[new_array_reg];
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
            TB_Node* src = &f->nodes.data[n->unary.src];

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
            TB_Node* cond = &f->nodes.data[n->if_.cond];

            if (cond->type == TB_INTEGER_CONST) {
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

                TB_DataType dt = f->nodes.data[cond->cmp.a].dt;

                n->dt = dt;
                n->if_.cond = cond->cmp.a;
                changes++;
            } else if (cond->type == TB_CMP_EQ && tb_node_is_constant_zero(f, cond->cmp.b)) {
                // (if (cmpeq A 0) B C) => (if A C B)
                OPTIMIZER_LOG(i, "removed redundant compare-to-zero on if node");

                TB_DataType dt = f->nodes.data[cond->cmp.a].dt;

                n->dt = dt;
                n->if_.cond = cond->cmp.a;
                tb_swap(TB_Label, n->if_.if_true, n->if_.if_false);
                changes++;
            } else {
                TB_Label new_label;
                if (try_jump_thread(f, n->if_.if_true, &new_label)) {
                    OPTIMIZER_LOG(i, "jump threading");
                    n->if_.if_true = new_label;

                    try_garbage_collect(f, tls, n->if_.if_true);
                    changes++;
                }

                if (try_jump_thread(f, n->if_.if_false, &new_label)) {
                    OPTIMIZER_LOG(i, "jump threading");
                    n->if_.if_false = new_label;

                    try_garbage_collect(f, tls, n->if_.if_false);
                    changes++;
                }
            }
        } else if (tb_node_is_phi_node(f, i)) {
            int count = tb_node_get_phi_width(f, i);
            TB_PhiInput* inputs = tb_node_get_phi_inputs(f, i);

            if (count == 0) {
                tb_murder_node(f, n);
            } else if (count == 1) {
                OPTIMIZER_LOG(i, "removed trivial phi");

                TB_Reg r = inputs[0].val;
                assert(r > 0 && r < f->nodes.count);

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
                    loop_range(k, 0, j) {
                        if (inputs[k].val == i) {
                            duplicate = true;
                            break;
                        } else if (inputs[k].val == a) {
                            duplicate = true;
                            break;
                        }
                    }

                    if (!duplicate) {
                        if (a != TB_NULL_REG && f->nodes.data[a].type == TB_NULL) {
                            a = TB_NULL_REG;
                        }

                        tb_tls_push(tls, sizeof(TB_PhiInput));
                        new_inputs[new_length++] = (TB_PhiInput){ .label = b, .val = a };
                    }
                }

                if (new_length != n->phi.count) {
                    // Pass it off to more permanent storage
                    OPTIMIZER_LOG(i, "Deduplicated PHI node entries");

                    if (n->type == TB_PHIN) {
                        tb_platform_heap_free(inputs);
                    }

                    TB_PhiInput* more_permanent_store = tb_platform_heap_alloc(new_length * sizeof(TB_PhiInput));
                    memcpy(more_permanent_store, new_inputs, new_length * sizeof(TB_PhiInput));

                    n->type = TB_PHIN;
                    n->phi.count = new_length;
                    n->phi.inputs = more_permanent_store;
                    changes++;
                }
                tb_tls_restore(tls, new_inputs);
            }
        } else if (n->type == TB_GOTO) {
            TB_Label new_label;
            if (try_jump_thread(f, n->goto_.label, &new_label)) {
                OPTIMIZER_LOG(i, "jump threading");
                n->goto_.label = new_label;

                try_garbage_collect(f, tls, n->goto_.label);
                changes++;
            }
        } else if (n->type == TB_LABEL) {
            if (f->nodes.data[n->label.terminator].type == TB_GOTO) {
                TB_Reg target_reg = tb_find_reg_from_label(f, f->nodes.data[n->label.terminator].goto_.label);
                TB_Node* target = &f->nodes.data[target_reg];

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
                    TB_Node* if_node = &f->nodes.data[f->nodes.data[target->next].next];
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
                        if (f->nodes.data[match].type == TB_INTEGER_CONST) {
                            OPTIMIZER_LOG(i, "jump threading through PHI node");

                            // remove this from the inputs list
                            switch (f->nodes.data[match].type) {
                                case TB_PHI1: tb_murder_node(f, &f->nodes.data[match]); break;
                                case TB_PHI2: f->nodes.data[match].type = TB_PHI1; break;
                                case TB_PHIN: f->nodes.data[match].phi.count -= 1; break;
                                default: break;
                            }

                            // remove swap amirite
                            if (count > 0) {
                                inputs[input_slot] = inputs[count-1];
                            }

                            // fold original GOTO
                            TB_Label new_target = !tb_node_is_constant_zero(f, match) ?
                                if_node->if_.if_true : if_node->if_.if_false;

                            f->nodes.data[n->label.terminator].goto_.label = new_target;
                            changes++;

                            // just don't run the stuff below, move on...
                            continue;
                        }
                    }
                }
            }

            TB_Node* end = &f->nodes.data[0];
            TB_Reg bb_start = 0;
            TB_Reg bb_end = 0;

            // Find sequence of labels
            int count = 0;
            {
                TB_Node* seq = n;
                while (seq = &f->nodes.data[seq->next],
                       seq->type == TB_LABEL && seq != end) {
                    bb_end = seq->label.terminator;
                    count += 1;
                }

                bb_start = (seq - f->nodes.data);
            }

            if (count > 0) {
                TB_Label label = n->label.id;

                TB_Node* seq = &f->nodes.data[n->next];
                do {
                    OPTIMIZER_LOG(i, "merge labels r%d", (TB_Reg)(seq - f->nodes.data));
                    replace_label(f, seq, label, i);
                    seq = &f->nodes.data[seq->next];
                } while (seq->type == TB_LABEL && seq != end);

                n->next = bb_start;
                n->label.terminator = bb_end;
                changes++;
            }
        }
    }

    return (changes > 0);
}
