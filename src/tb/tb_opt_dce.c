#define TB_INTERNAL
#include "tb.h"

bool tb_opt_dce(TB_Function* f) {
	int changes = 0;
	TB_TemporaryStorage* tls = tb_tls_allocate();
    
	size_t* intervals = tb_tls_push(tls, f->nodes.count * sizeof(size_t));
	tb_find_live_intervals(intervals, f);
    
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		if (intervals[i] == 0) {
			switch (f->nodes.type[i]) {
				// keep
                case TB_NULL:
                case TB_LABEL:
                case TB_PHI1:
                case TB_PHI2:
                case TB_GOTO:
                case TB_IF:
                case TB_RET:
                case TB_STORE:
                case TB_LOAD:
                case TB_CALL:
                case TB_SWITCH:
                case TB_PARAM:
				break;
				// delete:
                case TB_INT_CONST:
                case TB_LOCAL:
                case TB_AND:
                case TB_OR:
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
                case TB_CMP_FLE:
				tb_kill_op(f, i);
				changes++;
				break;
				default:
				tb_todo();
			}
		}
	}
    
	return (changes > 0);
}
