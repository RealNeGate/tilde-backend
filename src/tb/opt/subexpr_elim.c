#include "../tb_internal.h"

// this is a conservative algorithm, if we don't handle a node in here
// it'll just fail to compare
static bool is_node_the_same(TB_Node* a, TB_Node* b) {
    if (a->type != b->type) return false;
    if (!TB_DATA_TYPE_EQUALS(a->dt, b->dt)) return false;

    size_t bytes = 0;
    switch (a->type) {
        case TB_INTEGER_CONST:
        if (a->integer.num_words == 1 &&
            b->integer.num_words == 1 &&
            a->integer.single_word == b->integer.single_word) {
            return true;
        }

        // TODO: handle large integer comparisons
        return false;

        case TB_KEEPALIVE:
        case TB_VA_START:
        case TB_NOT:
        case TB_NEG:
        case TB_X86INTRIN_SQRT:
        case TB_X86INTRIN_RSQRT:
        case TB_INT2PTR:
        case TB_PTR2INT:
        case TB_UINT2FLOAT:
        case TB_FLOAT2UINT:
        case TB_INT2FLOAT:
        case TB_FLOAT2INT:
        case TB_TRUNCATE:
        case TB_X86INTRIN_LDMXCSR:
        case TB_BITCAST:
        bytes = sizeof(struct TB_NodeUnary);
        break;

        case TB_ATOMIC_LOAD:
        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_SUB:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        bytes = sizeof(struct TB_NodeAtomicRMW);
        break;

        case TB_MEMBER_ACCESS:
        bytes = sizeof(struct TB_NodeMemberAccess);
        break;

        case TB_ARRAY_ACCESS:
        bytes = sizeof(struct TB_NodeArrayAccess);
        break;

        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL:
        case TB_UDIV:
        case TB_SDIV:
        case TB_UMOD:
        case TB_SMOD:
        case TB_SAR:
        case TB_SHL:
        case TB_SHR:
        bytes = sizeof(struct TB_NodeIArith);
        break;

        case TB_FADD:
        case TB_FSUB:
        case TB_FMUL:
        case TB_FDIV:
        bytes = sizeof(struct TB_NodeFArith);
        break;

        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        case TB_CMP_FLT:
        case TB_CMP_FLE:
        bytes = sizeof(struct TB_NodeCompare);
        break;

        default: return false;
    }
    assert(bytes != 0 && "this shouldn't be possible");

    void* a_start = &a->integer;
    void* b_start = &b->integer;
    return memcmp(a_start, b_start, bytes) == 0;
}

typedef struct {
    TB_Reg reg;
    size_t count;
    TB_Reg* regs;
} BasicBlockDefs;

static BasicBlockDefs* generate_def_table(TB_Function* f, TB_TemporaryStorage* tls) {
    BasicBlockDefs* table = tb_tls_push(tls, f->label_count * sizeof(BasicBlockDefs));

    TB_Label bb = 0;
    TB_FOR_EACH_NODE(n, f) {
        TB_Reg r = (n - f->nodes);

        if (n->type == TB_LABEL) {
            bb = n->label.id;

            table[bb].reg = r;
            table[bb].count = 0;
            table[bb].regs = tb_tls_push(tls, 0);
        } else if (!TB_IS_NODE_SIDE_EFFECT(n->type) && n->type != TB_LOAD) {
            tb_tls_push(tls, sizeof(TB_Reg));

            size_t i = table[bb].count++;
            table[bb].regs[i] = r;
        }
    }

    return table;
}

TB_Reg find_similar_def_in_bb(TB_Function* f, BasicBlockDefs* bb_defs, TB_Reg r) {
    TB_Node* restrict n = &f->nodes[r];

    FOREACH_N(i, 0, bb_defs->count) {
        TB_Reg other = bb_defs->regs[i];
        if (is_node_the_same(n, &f->nodes[other])) {
            return other;
        }
    }

    return 0;
}

bool tb_opt_subexpr_elim(TB_Function* f) {
    bool has_ever_changed = false;
    bool changes;
    do {
        changes = false;

        // we use the dominators to figure out the search space of computed values at
        // any BB
        TB_TemporaryStorage* tls = tb_tls_allocate();
        TB_Predeccesors preds = tb_get_temp_predeccesors(f, tls);
        TB_Label* doms = tb_tls_push(tls, f->label_count * sizeof(TB_Label));
        tb_get_dominators(f, preds, doms);

        // list of defined nodes in for every basic block relevant to global CSE
        BasicBlockDefs* defs = generate_def_table(f, tls);

        // list of resolved nodes in this basic block, used for local CSE
        size_t resolved_reg_count = 0;
        TB_Reg* resolved_regs = tb_tls_push(tls, 0);

        TB_Label bb = 0;
        TB_FOR_EACH_NODE(n, f) {
            TB_Reg r = (n - f->nodes);

            if (n->type == TB_LABEL) {
                bb = n->label.id;

                // reset list
                tb_tls_restore(tls, resolved_regs);
                resolved_reg_count = 0;
            } else if (!TB_IS_NODE_SIDE_EFFECT(n->type) && n->type != TB_LOAD) {
                // try the Global CSE:
                // check dominators for value, we dont need the same checks of resolution
                // as local CSE since we can guarentee the entire BB is resolved at this point
                TB_Label curr = doms[bb];
                TB_Reg found = 0;
                for (; curr != 0; curr = doms[curr]) {
                    found = find_similar_def_in_bb(f, &defs[curr], r);
                    if (found != TB_NULL_REG) goto done_with_cse;
                }

                // try local CSE:
                FOREACH_N(i, 0, resolved_reg_count) {
                    TB_Reg other = resolved_regs[i];
                    if (is_node_the_same(n, &f->nodes[other])) {
                        found = other;
                        goto done_with_cse;
                    }
                }

                tb_tls_push(tls, resolved_reg_count);
                resolved_regs[resolved_reg_count++] = r;
                continue;

                // replace with PASS node
                done_with_cse:
                OPTIMIZER_LOG(r, "Removed duplicate expression");
                f->nodes[r].type = TB_PASS;
                f->nodes[r].pass.value = found;
                changes++;
            }
        }

        has_ever_changed = (changes > 0);
    } while (changes);

    return has_ever_changed;
}
