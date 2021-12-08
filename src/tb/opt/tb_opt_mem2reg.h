
static bool tb_is_stack_slot_coherent(TB_Function* f, TB_Register address);
static TB_Label* tb_calculate_immediate_predeccessors(TB_Function* f, TB_TemporaryStorage* tls, TB_Label l, int* dst_count);
static TB_Register tb_walk_for_intermediate_phi(TB_Function* f,
												TB_Label label_count,
												TB_Label l,
												TB_Register first_revision[restrict label_count],
												TB_Register last_revision[restrict label_count],
												TB_Label* preds[restrict label_count],
												int pred_count[restrict label_count]);

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

// NOTE(NeGate): All locals were moved into the first basic block by
// opt_hoist_locals earlier
bool tb_opt_mem2reg(TB_Function* f) {
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	size_t to_promote_count = 0;
	TB_Register* to_promote = tb_tls_push(tls, 0);
	
	TB_Register entry_terminator = f->nodes.payload[1].label.terminator;
	loop_range(i, 1, entry_terminator) {
		if (f->nodes.type[i] == TB_LOCAL || f->nodes.type[i] == TB_PARAM_ADDR) {
			if (tb_is_stack_slot_coherent(f, i)) {
				*((TB_Register*)tb_tls_push(tls, sizeof(TB_Register))) = i;
				to_promote_count++;
			}
		}
	}
	
	if (to_promote_count == 0) {
		// doesn't need to mem2reg
		return false;
	}
	
	int label_count = f->label_count;
	TB_Register* first_revision = tb_tls_push(tls, label_count * sizeof(TB_Register));
	TB_Register* last_revision = tb_tls_push(tls, label_count * sizeof(TB_Register));
	
	int* pred_count = tb_tls_push(tls, label_count * sizeof(int));
	TB_Label** preds = tb_tls_push(tls, label_count * sizeof(TB_Label*));
	
	loop(i, to_promote_count) {
		size_t saved = tls->used;
		TB_Register address = to_promote[i];
		
		memset(first_revision, 0, label_count * sizeof(TB_Register));
		memset(last_revision, 0, label_count * sizeof(TB_Register));
		
		TB_Register initial_value = TB_NULL_REG;
		if (f->nodes.type[address] == TB_PARAM_ADDR) {
			initial_value = f->nodes.payload[address].param_addr.param;
		}
		
		//
		// Perform local value numbering on all basic blocks
		//
		TB_Register latest = initial_value;
		TB_Label current_label = 0;
		loop(j, f->nodes.count) {
			TB_RegType reg_type = f->nodes.type[j];
			TB_RegPayload p = f->nodes.payload[j];
			
			if (reg_type == TB_LABEL) {
				current_label = p.label.id;
				
				first_revision[current_label] = initial_value;
				last_revision[current_label] = initial_value;
				initial_value = 0;
			} else if (reg_type == TB_LOAD && p.load.address == address) {
				if (first_revision[current_label] == 0) {
					first_revision[current_label] = j;
				}
				
				// convert to internal pass
				f->nodes.type[j] = TB_PASS;
				f->nodes.payload[j] = (TB_RegPayload) { .pass = latest };
			} else if (reg_type == TB_STORE && p.store.address == address) {
				last_revision[current_label] = latest = p.store.value;
				
				// kill store
				tb_kill_op(f, j);
			}
		}
		
		// Early out: if the local is not changed then we don't need PHI nodes
		bool immutable = true;
		loop(j, label_count) if (first_revision[j] != last_revision[j]) {
			immutable = false;
			break;
		}
		
		if (immutable) continue;
		
		//
		// Calculate all the immediate predecessors
		// First BB has no predecessors
		//
		pred_count[0] = 0;
		preds[0] = NULL;
		
		loop_range(j, 1, label_count) {
			preds[j] = (TB_Label*)tb_tls_push(tls, 0);
			tb_calculate_immediate_predeccessors(f, tls, j, &pred_count[j]);
		}
		
		//
		// Insert PHI nodes
		//
		// this is done by finding all BBs which disagree
		// on the last_revision
		//
		loop_range(j, 1, label_count) if (first_revision[j]) {
			tb_walk_for_intermediate_phi(f, label_count, j, first_revision, last_revision, preds, pred_count);
		}
		
		tls->used = saved;
	}
	
	return true;
}

static TB_Register tb_walk_for_intermediate_phi(TB_Function* f,
												TB_Label label_count,
												TB_Label l,
												TB_Register first_revision[restrict label_count],
												TB_Register last_revision[restrict label_count],
												TB_Label* preds[restrict label_count],
												int pred_count[restrict label_count]) {
	if (pred_count[l] == 0) return last_revision[l];
	
	int first = preds[l][0];
	bool agree = true;
	loop_range(k, 1, pred_count[l]) if (first != preds[l][k]) {
		agree = false;
		break;
	}
	
	if (agree && last_revision[l]) return last_revision[l];
	
	if (pred_count[l] == 1) {
		TB_Register a = tb_walk_for_intermediate_phi(f, label_count, preds[l][0], first_revision, last_revision, preds, pred_count);
		
		last_revision[l] = a;
		return a;
	}
	
	// TODO(NeGate): Implement phi with n-parameters
	assert(pred_count[l] == 2);
	
	// Insert intermediate node
	TB_Register label_reg = tb_find_reg_from_label(f, l);
	
	TB_Register new_phi_reg = label_reg + 1;
	tb_insert_op(f, new_phi_reg);
	
	// Update the first and last revisions
	loop(k, label_count) {
		if (first_revision[k] + 1 >= new_phi_reg) first_revision[k]++;
		if ( last_revision[k] + 1 >= new_phi_reg)  last_revision[k]++;
	}
	
	first_revision[l] = new_phi_reg;
	
	// TODO(NeGate): This is to force any loops to early out
	int saved_pred_count = pred_count[l];
	pred_count[l] = 0;
	
	// add phi node
	TB_Register a = tb_walk_for_intermediate_phi(f, label_count, preds[l][0], first_revision, last_revision, preds, pred_count);
	TB_Register b = tb_walk_for_intermediate_phi(f, label_count, preds[l][1], first_revision, last_revision, preds, pred_count);
	
	if (a == 0 && b == 0) {
		abort();
	}
	else if (a == 0 || b == 0) {
		TB_Register src = a ? a : b;

		f->nodes.type[new_phi_reg] = TB_PASS;
		f->nodes.dt[new_phi_reg] = f->nodes.dt[src];
		f->nodes.payload[new_phi_reg] = (TB_RegPayload){
			.pass = src
		};
	}
	else {
		f->nodes.type[new_phi_reg] = TB_PHI2;
		f->nodes.dt[new_phi_reg] = f->nodes.dt[a];
		f->nodes.payload[new_phi_reg] = (TB_RegPayload){
			.phi2 = {
				.a_label = tb_find_reg_from_label(f, preds[l][0]),
				.a = a,
				.b_label = tb_find_reg_from_label(f, preds[l][1]),
				.b = b
			}
		};
	}
	
	last_revision[l] = new_phi_reg;
	pred_count[l] = saved_pred_count;
	
	// Any PASSes which used the PHI node's inputs should be converted
	// to PASSes to said PHI node
	//TB_Register terminator = f->nodes.payload[label_reg].label.terminator;
	loop_range(i, 1, f->nodes.count) {
		if (f->nodes.type[i] == TB_PASS && (f->nodes.payload[i].pass == a || f->nodes.payload[i].pass == b)) {
			if (i != new_phi_reg) f->nodes.payload[i].pass = new_phi_reg;
		}
	}
	
	return new_phi_reg;
}

static TB_Label* tb_calculate_immediate_predeccessors(TB_Function* f, TB_TemporaryStorage* tls, TB_Label l, int* dst_count) {
	size_t count = 0;
	TB_Label* preds = tb_tls_push(tls, 0);
	
	TB_Register label = 1;
	do {
		TB_Register terminator = f->nodes.payload[label].label.terminator;
		TB_Label id = f->nodes.payload[label].label.id;
		
		if (f->nodes.type[terminator] == TB_LABEL) {
			if (l == f->nodes.payload[terminator].label.id) {
				*((TB_Register*)tb_tls_push(tls, sizeof(TB_Register))) = id;
				count++;
			}
			label = terminator;
		} else if (f->nodes.type[terminator] == TB_IF) {
			if (l == f->nodes.payload[terminator].if_.if_true) {
				*((TB_Register*)tb_tls_push(tls, sizeof(TB_Register))) = id;
				count++;
			}
			
			if (l == f->nodes.payload[terminator].if_.if_false) {
				*((TB_Register*)tb_tls_push(tls, sizeof(TB_Register))) = id;
				count++;
			}
			label = terminator + 1;
		} else if (f->nodes.type[terminator] == TB_GOTO) {
			if (l == f->nodes.payload[terminator].goto_.label) {
				*((TB_Register*)tb_tls_push(tls, sizeof(TB_Register))) = id;
				count++;
			}
			label = terminator + 1;
		} else if (f->nodes.type[terminator] == TB_RET) {
			label = terminator + 1;
		} else tb_todo();
	} while (label < f->nodes.count);
	
	*dst_count = count;
	return preds;
}

// NOTE(NeGate): a stack slot is coherent when all loads and stores share
// the same type and properties.
static bool tb_is_stack_slot_coherent(TB_Function* f, TB_Register address) {
	bool initialized = false;
	TB_DataType dt;
	
	// pick the first load/store and use that as the baseline
	for (TB_Register i = address; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LOAD && f->nodes.payload[i].load.address == address) {
			if (!initialized) dt = f->nodes.dt[i];
			else if (TB_DATA_TYPE_EQUALS(dt, f->nodes.dt[i])) return false;
		} else if (f->nodes.type[i] == TB_STORE && f->nodes.payload[i].store.address == address) {
			if (!initialized) dt = f->nodes.dt[i];
			else if (TB_DATA_TYPE_EQUALS(dt, f->nodes.dt[i])) return false;
		} else if (f->nodes.type[i] == TB_MEMSET && f->nodes.payload[i].mem_op.dst == address) {
			return false;
		} else if (f->nodes.type[i] == TB_ARRAY_ACCESS && f->nodes.payload[i].array_access.base == address) {
			return false;
		} else if (f->nodes.type[i] == TB_MEMBER_ACCESS && f->nodes.payload[i].member_access.base == address) {
			return false;
		}
	}
	
	return true;
}
