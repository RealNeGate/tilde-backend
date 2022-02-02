#include "../tb_internal.h"

// We just move them up because it's slightly easier to think about them
bool tb_opt_hoist_locals(TB_Function* f) {
	int changes = 0;
	TB_Register entry_terminator = f->nodes.payload[1].label.terminator;
	
	size_t locals_spotted = 0;
	loop_range(i, entry_terminator, f->nodes.count) {
		locals_spotted += f->nodes.type[i] == TB_LOCAL;
	}
	
	if (locals_spotted == 0) return false;
	
	TB_Register baseline = entry_terminator;
	tb_insert_ops(f, baseline, locals_spotted);
	entry_terminator += baseline;
	
	for (TB_Register i = entry_terminator; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LOCAL) {
			// move to the entry block
			// try replacing a NOP
			assert(f->nodes.type[baseline] == TB_NULL);
			
			TB_Register new_reg = baseline++;
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
