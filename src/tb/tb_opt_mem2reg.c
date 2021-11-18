#include "tb_internal.h"

static bool tb_is_stack_slot_coherent(TB_Function* f, TB_Register address, TB_DataType* dst_dt);
static TB_Label* tb_calculate_immediate_predeccessors(TB_Function* f, TB_TemporaryStorage* tls, TB_Label l, int* dst_count);
static bool tb_mem2reg_single_reg(TB_Function* f, TB_TemporaryStorage* tls, int label_count, TB_Register address, TB_Register initial_value);
static TB_Register tb_walk_for_intermediate_phi(TB_Function* f, TB_Label label_count, TB_Label l, TB_Register* first_revision, TB_Register* last_revision, TB_Label** preds, int* pred_count);

bool tb_opt_mem2reg(TB_Function* f) {
	int changes = 0;
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	int label_count = 0;
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LABEL) label_count++;
	}
	
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LOCAL || f->nodes.type[i] == TB_PARAM_ADDR) {
			// Make sure that the stack slots are coherent
			TB_DataType initial_dt;
			if (!tb_is_stack_slot_coherent(f, i, &initial_dt)) continue;
			
			TB_Register initial_val = TB_NULL_REG;
			if (f->nodes.type[i] == TB_PARAM_ADDR) {
				initial_val = f->nodes.payload[i].param_addr.param;
			}
			
			tls->used = 0;
			changes += tb_mem2reg_single_reg(f, tls, label_count, i, initial_val);
			
			//tb_function_print(f);
		}
	}
	
	return changes;
}

// NOTE(NeGate): a stack slot is coherent when all loads and stores share
// the same type and properties.
static bool tb_is_stack_slot_coherent(TB_Function* f, TB_Register address, TB_DataType* dst_dt) {
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
		}
	}
	
	assert(dst_dt);
	*dst_dt = dt;
	return true;
}

// TODO(NeGate): Optimize this for speed...
static bool tb_mem2reg_single_reg(TB_Function* f, TB_TemporaryStorage* tls, int label_count, TB_Register address, TB_Register initial_value) {
	// Perform local value numbering on all basic blocks
	TB_Register* first_revision = tb_tls_push(tls, label_count * sizeof(TB_Register));
	TB_Register* last_revision = tb_tls_push(tls, label_count * sizeof(TB_Register));
	memset(first_revision, 0, label_count * sizeof(TB_Register));
	memset(last_revision, 0, label_count * sizeof(TB_Register));
	
	int changes = 0;
	TB_Label current_label = 0;
	TB_Register latest = initial_value;
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LABEL) {
			current_label = f->nodes.payload[i].label.id;
			
			first_revision[current_label] = initial_value;
			last_revision[current_label] = initial_value;
			initial_value = 0;
		} else if (f->nodes.type[i] == TB_LOAD && f->nodes.payload[i].load.address == address) {
			if (first_revision[current_label] == 0) first_revision[current_label] = i;
			
			// convert to internal pass
			f->nodes.type[i] = TB_PASS;
			f->nodes.payload[i].pass = latest;
			changes++;
		} else if (f->nodes.type[i] == TB_STORE && f->nodes.payload[i].store.address == address) {
			last_revision[current_label] = latest = f->nodes.payload[i].store.value;
			
			// kill store
			tb_kill_op(f, i);
			changes++;
		}
	}
	
	// Early out: if the local is not changed then we don't need PHI nodes
	bool immutable = true;
	for (size_t i = 0; i < label_count; i++) {
		if (last_revision[i]) {
			immutable = false;
			break;
		}
	}
	
	if (immutable) return changes;
	
	// Calculate all the immediate predecessors, we'll be using them
	// to fill in the PHI nodes
	int* pred_count = tb_tls_push(tls, label_count * sizeof(int));
	TB_Label** preds = tb_tls_push(tls, label_count * sizeof(TB_Label*));
	
	// First basic block has no predecessors
	pred_count[0] = 0;
	preds[0] = NULL;
	
	for (TB_Label i = 1; i < label_count; i++) {
		preds[i] = (TB_Label*)&tls->data[tls->used];
		tb_calculate_immediate_predeccessors(f, tls, i, &pred_count[i]);
	}
	
	// Insert intermediate PHI nodes
	//
	// success means it inserts a PHI node, if don't make any
	// more changes it's complete
	for (TB_Label i = 1; i < label_count; i++) {
		if (first_revision[i] == 0) continue;
		
		// TODO(NeGate): There's probably smarter ways than crap loads of recursion... :P
		for (int j = 0; j < pred_count[i]; j++) {
			tb_walk_for_intermediate_phi(f, label_count, preds[i][j], first_revision, last_revision, preds, pred_count);
		}
	}
	
	// Stitch the basic block revisions together
	for (TB_Label i = 1; i < label_count; i++) {
		if (first_revision[i] == 0) continue;
		
		size_t tls_saved = tls->used;
		
		// PHI2+ should have been handled by the intermediate PHI node insertion
		if (pred_count[i] == 1) {
			TB_Label pred_label = preds[i][0];
			TB_Register pred_label_reg = tb_find_reg_from_label(f, pred_label);
			TB_Register last_rev_in_pred = last_revision[pred_label];
			
			TB_Register first_rev_in_bb = first_revision[i];
			
			// TODO(NeGate): If this is 0, then we need to insert a
			// PHI node in the predecessor going up the chain
			assert(last_rev_in_pred != 0);
			
			// promote pass into PHI1
			assert(f->nodes.type[first_rev_in_bb] == TB_PASS);
			f->nodes.type[first_rev_in_bb] = TB_PHI1;
			f->nodes.payload[first_rev_in_bb].phi1.a = last_rev_in_pred;
			f->nodes.payload[first_rev_in_bb].phi1.a_label = pred_label_reg;
			changes++;
		}
		
		tls->used = tls_saved;
	}
	
	return changes;
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

static TB_Register tb_walk_for_intermediate_phi(TB_Function* f, TB_Label label_count, TB_Label l, TB_Register* first_revision, TB_Register* last_revision, TB_Label** preds, int* pred_count) {
	TB_Register last_rev_in_pred = last_revision[l];
	if (last_rev_in_pred) return last_rev_in_pred;
	
	// Insert intermediate node
	TB_Register label_reg = tb_find_reg_from_label(f, l);
	
	TB_Register new_phi_reg = label_reg + 1;
	tb_insert_op(f, new_phi_reg);
	
	// Update the first and last revisions
	for (int i = 0; i < label_count; i++) {
		if (first_revision[i] + 1 >= new_phi_reg) first_revision[i]++;
		if (last_revision[i] + 1 >= new_phi_reg) last_revision[i]++;
	}
	
	last_revision[l] = new_phi_reg;
	
	// Insert intermediate phi nodes
	TB_Register a = TB_NULL_REG;
	TB_Register b = TB_NULL_REG;
	if (pred_count[l] == 1) {
		a = tb_walk_for_intermediate_phi(f, label_count, preds[l][0], first_revision, last_revision, preds, pred_count);
		
		f->nodes.type[new_phi_reg] = TB_PHI1;
		// They should both match so it doesn't matter
		f->nodes.dt[new_phi_reg] = f->nodes.dt[a];
		f->nodes.payload[new_phi_reg] = (TB_RegPayload){
			.phi1 = {
				.a_label = tb_find_reg_from_label(f, preds[l][0]),
				.a = a
			}
		};
	} else if (pred_count[l] == 2) {
		a = tb_walk_for_intermediate_phi(f, label_count, preds[l][0], first_revision, last_revision, preds, pred_count);
		b = tb_walk_for_intermediate_phi(f, label_count, preds[l][1], first_revision, last_revision, preds, pred_count);
		
		f->nodes.type[new_phi_reg] = TB_PHI2;
		// They should both match so it doesn't matter
		f->nodes.dt[new_phi_reg] = f->nodes.dt[a];
		f->nodes.payload[new_phi_reg] = (TB_RegPayload){
			.phi2 = {
				.a_label = tb_find_reg_from_label(f, preds[l][0]),
				.a = a,
				.b_label = tb_find_reg_from_label(f, preds[l][1]),
				.b = b
			}
		};
	} else tb_todo();
	
	// Any PASSes which used the PHI node's inputs should be converted
	// to PASSes to said PHI node
	TB_Register terminator = f->nodes.payload[label_reg].label.terminator;
	for (TB_Register i = label_reg + 1; i < terminator; i++) {
		if (f->nodes.type[i] == TB_PASS && (f->nodes.payload[i].pass == a || f->nodes.payload[i].pass == b)) {
			f->nodes.payload[i].pass = new_phi_reg;
		}
	}
	
	last_revision[l] = new_phi_reg;
	return new_phi_reg;
}
