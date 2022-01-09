
bool tb_opt_dead_expr_elim(TB_Function* f) {
	int changes = 0;
	TB_TemporaryStorage* tls = tb_tls_allocate();
    
	TB_Register* intervals = tb_tls_push(tls, f->nodes.count * sizeof(TB_Register));
	tb_find_live_intervals(f, intervals);
    
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		if (intervals[i] == 0) {
			switch (f->nodes.type[i]) {
				// keep
				case TB_NULL:
                case TB_LABEL:
				case TB_INITIALIZE:
                case TB_PHI1:
                case TB_PHI2:
                case TB_GOTO:
                case TB_IF:
                case TB_RET:
                case TB_STORE:
                case TB_LOAD:
                case TB_CALL:
				case TB_VCALL:
				case TB_ECALL:
                case TB_SWITCH:
                case TB_PARAM:
				case TB_MEMSET:
				case TB_MEMCPY:
                case TB_KEEPALIVE:
                case TB_TRAP:
				case TB_UNREACHABLE:
				case TB_ATOMIC_XCHG:
				case TB_ATOMIC_ADD:
				case TB_ATOMIC_SUB:
				case TB_ATOMIC_AND:
				case TB_ATOMIC_XOR:
				case TB_ATOMIC_OR:
				break;
				// delete:
                case TB_UNSIGNED_CONST:
                case TB_SIGNED_CONST:
                case TB_LOCAL:
                case TB_PASS:
                case TB_NOT:
                case TB_NEG:
                case TB_SIGN_EXT:
                case TB_ZERO_EXT:
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

bool tb_opt_dead_block_elim(TB_Function* f) {
#if 0
	int changes = 0;
	TB_TemporaryStorage* tls = tb_tls_allocate();
    
	// Calculate all the immediate predecessors
	// First BB has no predecessors
	int* pred_count; // [label_count]
	TB_Label** preds; // [label_count][pred_count[i]]
	{
		pred_count = tb_tls_push(tls, f->label_count * sizeof(int));
		preds = tb_tls_push(tls, f->label_count * sizeof(TB_Label*));
		
		// entry label has no predecessors
		pred_count[0] = 0;
		preds[0] = NULL;
		
		loop_range(j, 1, f->label_count) {
			preds[j] = (TB_Label*)tb_tls_push(tls, 0);
			tb_calculate_immediate_predeccessors(f, tls, j, &pred_count[j]);
		}
	}
	
	// NOTE(NeGate): Try to see if the conditions to a basic block
	// can ever be true.
	loop_range(i, 1, f->label_count) if (pred_count[i] == 1) {
		TB_Register parent = tb_find_reg_from_label(f, preds[i][0]);
		TB_Register parent_terminator = f->nodes.payload[parent].label.terminator;
		
		// we want simple IFs
		if (f->nodes.type[parent_terminator] != TB_IF) continue;
		
		bool is_hit_on_true = (i == f->nodes.payload[parent_terminator].if_.if_true);
		TB_Register condition = f->nodes.payload[parent_terminator].if_.cond;
		
		((void)is_hit_on_true);
		((void)condition);
		__debugbreak();
	}
	
	return (changes > 0);
#endif
	
	return false;
}

