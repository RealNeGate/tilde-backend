#include "../tb_internal.h"

bool tb_opt_dead_expr_elim(TB_Function* f) {
    int changes = 0;
    TB_TemporaryStorage* tls = tb_tls_allocate();

    int* use_count = tb_tls_push(tls, f->nodes.count * sizeof(int));
    tb_function_calculate_use_count(f, use_count);

    TB_FOR_EACH_NODE(n, f) {
        TB_Reg i = n - f->nodes.data;

        if (use_count[i] == 0) {
            switch (n->type) {
				// keep
				case TB_NULL:
				case TB_LABEL:
				case TB_INITIALIZE:
				case TB_PHI1:
				case TB_PHI2:
				case TB_PHIN:
				case TB_GOTO:
				case TB_IF:
				case TB_RET:
				case TB_STORE:
				case TB_CALL:
				case TB_VCALL:
				case TB_ECALL:
				case TB_SWITCH:
				case TB_PARAM:
				case TB_MEMSET:
				case TB_MEMCPY:
				case TB_DEBUGBREAK:
				case TB_KEEPALIVE:
				case TB_TRAP:
				case TB_UNREACHABLE:
				case TB_ATOMIC_XCHG:
				case TB_ATOMIC_ADD:
				case TB_ATOMIC_SUB:
				case TB_ATOMIC_AND:
				case TB_ATOMIC_XOR:
				case TB_ATOMIC_OR: break;
				// don't delete volatile loads
				case TB_LOAD: {
					if (n->load.is_volatile) {
						OPTIMIZER_LOG(i, "FAILURE could not remove volatile load");
					} else {
						OPTIMIZER_LOG(i, "removed unused expression node");

						tb_murder_node(f, n);
						changes++;
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
				case TB_STRING_CONST:
				case TB_FLOAT_CONST:
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

					tb_murder_node(f, n);
					changes++;
					break;
				}
				default: tb_todo();
            }
        }
    }

    return (changes > 0);
}

bool tb_opt_dead_block_elim(TB_Function* f) {
	return false;
}
