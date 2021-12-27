
// We just move them up because it's slightly easier to think about them
bool tb_opt_hoist_locals(TB_Function* f) {
	int changes = 0;
	
	TB_Register entry_terminator = f->nodes.payload[1].label.terminator;
	
	for (TB_Register i = entry_terminator; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LOCAL) {
			// move to the entry block
			// try replacing a NOP
			TB_Register new_reg = TB_NULL_REG;
			
			for (TB_Register j = 1; i < entry_terminator; j++) {
				if (f->nodes.type[j] == TB_NULL) {
					new_reg = j;
					break;
				}
			}
			
			if (!new_reg) {
				// Insert new node
				new_reg = entry_terminator;
				tb_insert_op(f, entry_terminator);
				
				// account for the insertion
				entry_terminator++;
				i++;
			}
			
			f->nodes.dt[new_reg] = f->nodes.dt[i];
			f->nodes.type[new_reg] = f->nodes.type[i];
			f->nodes.payload[new_reg] = f->nodes.payload[i];
			
			tb_kill_op(f, i);
			tb_function_find_replace_reg(f, i, new_reg);
			changes++;
		}
	}
	
	return changes;
}
