
	TB_API bool tb_opt_copy_elision(TB_Function* f) {
	int changes = 0;
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	TB_Register* intervals = tb_tls_push(tls, f->nodes.count * sizeof(TB_Register));
	
	TB_Label bb = 0, bb_end = 0;
	loop_range(i, 1, f->nodes.count) {
		TB_RegPayload* restrict p = &f->nodes.payload[i];
		
		if (f->nodes.type[i] == TB_LABEL) {
			bb = i;
			bb_end = p->label.terminator;
		} else if (f->nodes.type[i] == TB_MEMCPY) {
			// TODO(NeGate): maybe we can move this out of the loop but
			// verify that later
			tb_find_live_intervals(f, intervals);
			
			TB_Register dst_reg = p->mem_op.dst;
			TB_Register src_reg = p->mem_op.src;
			TB_Register size_reg = p->mem_op.size;
			
			// find memcpy with stack slot as it's source
			// where the size is a simple constant
			// that's within the same basic block
			// and the memcpy matches the size of the stack slot
			if (f->nodes.type[src_reg] != TB_LOCAL) continue;
			if (f->nodes.type[size_reg] != TB_INT_CONST) continue;
			if (src_reg < bb && src_reg > bb_end) continue;
			if (f->nodes.payload[size_reg].i_const != f->nodes.payload[src_reg].local.size) continue;
			
			// We should probably invest in smarter escape analysis but 
			// for now we'll assume that any CALLs are somehow escaping
			// this stack slot.
			bool pointer_escapes = false;
			loop_range(j, bb, bb_end) {
				if (f->nodes.type[j] == TB_CALL ||
					f->nodes.type[j] == TB_VCALL ||
					f->nodes.type[j] == TB_ECALL) {
					pointer_escapes = true;
					break;
				}
			}
			
			if (pointer_escapes) continue;
			
			tb_kill_op(f, i);
			tb_function_find_replace_reg(f, src_reg, dst_reg);
			changes++;
		}
	}
	
	return (changes > 0);
}
