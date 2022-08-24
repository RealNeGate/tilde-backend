#include "../tb_internal.h"

static bool dead_expr_elim(TB_Function* f) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    // TODO(NeGate): rewrite this such that we don't do this loop and instead just mark
    // when objects are murdered such that we change the time complexity of this crap
    bool changes = false;
    int* use_count = tb_tls_push(tls, f->node_count * sizeof(int));
    tb_function_calculate_use_count(f, use_count);

    bool local_changes;
    do {
        local_changes = false;

        TB_FOR_EACH_NODE(n, f) {
            TB_Reg i = TB_GET_REG(n, f);

            if (use_count[i] == 0) {
                switch (n->type) {
                    // keep
                    case TB_NULL:
                    case TB_LINE_INFO:
                    case TB_LABEL:
                    case TB_INITIALIZE:
                    case TB_PHI1:
                    case TB_PHI2:
                    case TB_PHIN:
                    case TB_GOTO:
                    case TB_IF:
                    case TB_RET:
                    case TB_STORE:
                    case TB_SWITCH:
                    case TB_PARAM:
                    case TB_MEMSET:
                    case TB_MEMCPY:
                    case TB_DEBUGBREAK:
                    case TB_KEEPALIVE:
                    case TB_TRAP:
                    case TB_UNREACHABLE:
                    case TB_ATOMIC_XCHG:
                    case TB_ATOMIC_CMPXCHG:
                    case TB_ATOMIC_CMPXCHG2:
                    case TB_ATOMIC_ADD:
                    case TB_ATOMIC_SUB:
                    case TB_ATOMIC_AND:
                    case TB_ATOMIC_XOR:
                    case TB_ATOMIC_OR:
                    case TB_X86INTRIN_SQRT:
                    case TB_X86INTRIN_RSQRT:
                    case TB_X86INTRIN_LDMXCSR:
                    case TB_X86INTRIN_STMXCSR:
                    break;

                    case TB_CALL:
                    case TB_VCALL:
                    case TB_ECALL: {
                        // convert it to a void CALL just because
                        n->dt = TB_TYPE_VOID;
                        break;
                    }

                    // don't delete volatile loads
                    case TB_LOAD: {
                        if (n->load.is_volatile) {
                            OPTIMIZER_LOG(i, "FAILURE could not remove volatile load");
                        } else {
                            OPTIMIZER_LOG(i, "removed unused expression node");

                            use_count[n->load.address] -= 1;
                            tb_murder_node(f, n);
                            local_changes = true;
                        }
                        break;
                    }
                    // delete:
                    case TB_GLOBAL_ADDRESS:
                    case TB_FUNC_ADDRESS:
                    case TB_EXTERN_ADDRESS:
                    case TB_INTEGER_CONST:
                    case TB_ARRAY_ACCESS:
                    case TB_MEMBER_ACCESS:
                    case TB_BITCAST:
                    case TB_STRING_CONST:
                    case TB_FLOAT32_CONST:
                    case TB_FLOAT64_CONST:
                    case TB_INT2PTR:
                    case TB_PTR2INT:
                    case TB_INT2FLOAT:
                    case TB_FLOAT2INT:
                    case TB_SIGN_EXT:
                    case TB_ZERO_EXT:
                    case TB_TRUNCATE:
                    case TB_LOCAL:
                    case TB_PASS:
                    case TB_NOT:
                    case TB_NEG:
                    case TB_AND:
                    case TB_OR:
                    case TB_XOR:
                    case TB_ADD:
                    case TB_SUB:
                    case TB_MUL:
                    case TB_SDIV:
                    case TB_UDIV:
                    case TB_SHL:
                    case TB_SHR:
                    case TB_SAR:
                    case TB_FADD:
                    case TB_FSUB:
                    case TB_FMUL:
                    case TB_FDIV:
                    case TB_PARAM_ADDR:
                    case TB_CMP_EQ:
                    case TB_CMP_NE:
                    case TB_CMP_SLT:
                    case TB_CMP_SLE:
                    case TB_CMP_ULT:
                    case TB_CMP_ULE:
                    case TB_CMP_FLT:
                    case TB_CMP_FLE: {
                        OPTIMIZER_LOG(i, "removed unused expression node");

                        TB_FOR_INPUT_IN_NODE(it, f, n) {
                            use_count[it.r] -= 1;
                        }

                        tb_murder_node(f, n);
                        local_changes = true;
                        break;
                    }
                    default: tb_todo();
                }
            }
        }

        changes |= local_changes;
    } while (local_changes);

    return changes;
}

TB_API TB_Pass tb_opt_dead_expr_elim(void) {
    return (TB_Pass){
        .mode = TB_FUNCTION_PASS,
        .name = "DeadExprElimination",
        .func_run = dead_expr_elim,
    };
}
