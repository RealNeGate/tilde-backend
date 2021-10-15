#define TB_INTERNAL
#include "tb.h"

//
// IR ANALYSIS
//
TB_API void tb_find_live_intervals(size_t intervals[], const TB_Function* f) {
	for (size_t i = 0; i < f->count; i++) intervals[i] = TB_NULL_REG;
    
	for (size_t i = 0; i < f->count; i++) {
		switch (f->nodes[i].type) {
            case TB_NULL:
            case TB_INT_CONST:
            case TB_LOCAL:
            case TB_PARAM:
            case TB_LABEL:
            case TB_GOTO:
			break;
            case TB_PARAM_ADDR:
			intervals[f->nodes[i].param_addr.param] = i;
			break;
            case TB_PHI1:
			intervals[f->nodes[i].phi1.a] = i;
			break;
            case TB_PHI2:
			intervals[f->nodes[i].phi2.a] = i;
			intervals[f->nodes[i].phi2.b] = i;
			break;
            case TB_LOAD:
			intervals[f->nodes[i].load.address] = i;
			break;
            case TB_STORE:
			intervals[f->nodes[i].store.address] = i;
			intervals[f->nodes[i].store.value] = i;
			break;
            case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_UDIV:
            case TB_SDIV:
			intervals[f->nodes[i].i_arith.a] = i;
			intervals[f->nodes[i].i_arith.b] = i;
			break;
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_FLT:
            case TB_CMP_FLE:
			intervals[f->nodes[i].cmp.a] = i;
			intervals[f->nodes[i].cmp.b] = i;
			break;
            case TB_IF:
			intervals[f->nodes[i].if_.cond] = i;
			break;
            case TB_RET:
			intervals[f->nodes[i].ret.value] = i;
			break;
            default: abort();
		}
	}
}

TB_API TB_Register tb_find_first_use(const TB_Function* f, TB_Register find, size_t start, size_t end) {
#define ffu(r) if (r == find) return i
    
	for (size_t i = start; i < end; i++) {
		switch (f->nodes[i].type) {
            case TB_NULL:
            case TB_INT_CONST:
            case TB_LOCAL:
            case TB_PARAM:
            case TB_LABEL:
			break;
            case TB_PHI1:
			ffu(f->nodes[i].phi1.a);
			break;
            case TB_PHI2:
			ffu(f->nodes[i].phi2.a);
			ffu(f->nodes[i].phi2.b);
			break;
            case TB_PARAM_ADDR:
			ffu(f->nodes[i].param_addr.param);
			break;
            case TB_LOAD:
			ffu(f->nodes[i].load.address);
			break;
            case TB_STORE:
			ffu(f->nodes[i].store.address);
			ffu(f->nodes[i].store.value);
			break;
            case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_UDIV:
            case TB_SDIV:
			ffu(f->nodes[i].i_arith.a);
			ffu(f->nodes[i].i_arith.b);
			break;
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_FLT:
            case TB_CMP_FLE:
			ffu(f->nodes[i].cmp.a);
			ffu(f->nodes[i].cmp.b);
			break;
            case TB_IF:
			ffu(f->nodes[i].if_.cond);
			break;
            case TB_RET:
			ffu(f->nodes[i].ret.value);
			break;
            default: abort();
		}
	}
    
	return 0;
#undef ffu
}

static void tb_function_find_replace_reg(TB_Function* f, TB_Register find, TB_Register replace) {
#define f_n_r(r) if (r == find) r = replace
    
	for (size_t i = 0; i < f->count; i++) {
		switch (f->nodes[i].type) {
            case TB_NULL:
            case TB_INT_CONST:
            case TB_LOCAL:
            case TB_PARAM:
            case TB_LABEL:
			f_n_r(f->nodes[i].label.terminator);
			break;
            case TB_PASS:
			f_n_r(f->nodes[i].pass);
			break;
            case TB_PHI1:
			f_n_r(f->nodes[i].phi1.a);
			break;
            case TB_PHI2:
			f_n_r(f->nodes[i].phi2.a);
			f_n_r(f->nodes[i].phi2.b);
			break;
            case TB_PARAM_ADDR:
			f_n_r(f->nodes[i].param_addr.param);
			break;
            case TB_LOAD:
			f_n_r(f->nodes[i].load.address);
			break;
            case TB_STORE:
			f_n_r(f->nodes[i].store.address);
			f_n_r(f->nodes[i].store.value);
			break;
            case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_UDIV:
            case TB_SDIV:
			f_n_r(f->nodes[i].i_arith.a);
			f_n_r(f->nodes[i].i_arith.b);
			break;
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_FLT:
            case TB_CMP_FLE:
			f_n_r(f->nodes[i].cmp.a);
			f_n_r(f->nodes[i].cmp.b);
			break;
            case TB_IF:
			f_n_r(f->nodes[i].if_.cond);
			break;
            case TB_RET:
			f_n_r(f->nodes[i].ret.value);
			break;
            default: abort();
		}
	}
    
#undef f_n_r
}

TB_Register tb_find_reg_from_label(TB_Function* f, TB_Label id) {
	for (size_t i = 0; i < f->count; i++) {
		if (f->nodes[i].type == TB_LABEL && f->nodes[i].label.id == id) return i;
	}
    
	return TB_NULL_REG;
}

static bool tb_can_reach(TB_Function* f, TB_Register label, TB_Register end) {
	if (label == end) return true;
    
	TB_Register terminator = f->nodes[label].label.terminator;
    
	if (f->nodes[terminator].type == TB_LABEL) {
		return tb_can_reach(f, terminator, end);
	}
	else if (f->nodes[terminator].type == TB_IF) {
		if (tb_can_reach(f, tb_find_reg_from_label(f, f->nodes[terminator].if_.if_true), end)) return true;
        
		return tb_can_reach(f, tb_find_reg_from_label(f, f->nodes[terminator].if_.if_false), end);
	}
	else if (f->nodes[terminator].type == TB_RET) {
		return false;
	}
	else abort();
}

typedef enum TB_DataflowPattern {
	TB_DataflowPattern_Unknown,
	TB_DataflowPattern_IntConstant,
	TB_DataflowPattern_IntStep // y = mx + b
} TB_DataflowPattern;

typedef struct TB_RegisterDataflow {
	TB_DataflowPattern pattern;
	union {
		struct {
			uint64_t v;
		} iconst;
		struct {
			TB_Register loop_label;
			bool pre_iterator; // or post if false
			uint64_t m, b;
		} istep;
	};
} TB_RegisterDataflow;

static TB_RegisterDataflow tb_analyze_register_dataflow(TB_Function* f, TB_Register label_reg, TB_Register reg) {
	if (f->nodes[reg].type == TB_INT_CONST) {
		assert(f->nodes[reg].i_const.hi == 0);
		return (TB_RegisterDataflow) {
			.pattern = TB_DataflowPattern_IntConstant,
            .iconst = f->nodes[reg].i_const.lo
		};
	}
	else if (f->nodes[reg].type == TB_PHI2) {
		// Pre-step pattern (happens once before): 
		// 
		// rA = PHI before_loop:rInitial, loop_label:rB
		// rB = rA + rStep
		TB_Register initial = f->nodes[reg].phi2.a;
		TB_Register stepper = f->nodes[reg].phi2.b;
		TB_Register initial_lbl = f->nodes[reg].phi2.a_label;
		TB_Register stepper_lbl = f->nodes[reg].phi2.b_label;
        
		if (f->nodes[reg].phi2.a_label == label_reg) {
			tb_swap(stepper, initial);
			tb_swap(stepper_lbl, initial_lbl);
		}
        
		if (stepper_lbl == label_reg &&
			f->nodes[initial].type == TB_INT_CONST &&
			f->nodes[stepper].type == TB_ADD &&
			f->nodes[stepper].i_arith.a == reg &&
			f->nodes[f->nodes[stepper].i_arith.b].type == TB_INT_CONST) {
			assert(f->nodes[f->nodes[stepper].i_arith.b].i_const.hi == 0);
			assert(f->nodes[initial].i_const.hi == 0);
            
			return (TB_RegisterDataflow) {
				.pattern = TB_DataflowPattern_IntStep,
                .istep = {
                    .loop_label = label_reg,
                    .pre_iterator = true,
                    .b = f->nodes[initial].i_const.lo,
                    .m = f->nodes[f->nodes[stepper].i_arith.b].i_const.lo
				}
			};
		}
	}
    
	return (TB_RegisterDataflow) {
		.pattern = TB_DataflowPattern_Unknown
	};
}

// Results are in the temp storage
static size_t tb_loop_analysis(TB_TemporaryStorage* tls, TB_Function* f) {
	size_t loop_count = 0;
    
	size_t i = 0;
	while (i < f->count) {
		if (f->nodes[i].type == TB_LABEL) {
			TB_Register terminator = f->nodes[i].label.terminator;
            
			if (f->nodes[terminator].type == TB_IF) {
				f->nodes[i].label.is_loop =
					tb_can_reach(f, tb_find_reg_from_label(f, f->nodes[terminator].if_.if_true), i) ||
					tb_can_reach(f, tb_find_reg_from_label(f, f->nodes[terminator].if_.if_false), i);
			}
			else if (f->nodes[terminator].type == TB_GOTO) {
				f->nodes[i].label.is_loop =
					tb_can_reach(f, tb_find_reg_from_label(f, f->nodes[terminator].goto_.label), i);
			}
			else if (f->nodes[terminator].type == TB_LABEL) {}
			else if (f->nodes[terminator].type == TB_RET) {}
			else abort();
            
			if (f->nodes[i].label.is_loop) {
				uint32_t* loop_info = tb_tls_push(tls, 2 * sizeof(uint32_t));
                
				loop_info[0] = i;
				loop_info[1] = terminator;
				loop_count++;
			}
            
			i = terminator;
			continue;
		}
        
		i++;
	}
    
	return loop_count;
}

// Returns the size of the replicated space
static TB_Register tb_replicate_loop_statements(TB_Function* f, TB_TemporaryStorage* tls, TB_Register label_reg, TB_Register start, TB_Register end, TB_Register at, int times) {
	// Count how many registers to actually replicate
	int registers_to_replicate = end - start;
    
	// Reserve the space
	int extra_register_space = registers_to_replicate * times;
    
	if (f->count + extra_register_space < f->capacity) {
		f->capacity = f->count + extra_register_space;
		f->capacity = tb_next_pow2(f->capacity);
        
		f->nodes = realloc(f->nodes, f->capacity * sizeof(TB_Node));
	}
    
	// Shift over registers
	int registers_beyond_end_point = f->count - at;
	memmove(&f->nodes[at + extra_register_space], &f->nodes[end], registers_beyond_end_point * sizeof(TB_Node));
	f->count += extra_register_space;
    
	// Clear out that newly reserved space
	for (TB_Register i = 0; i < extra_register_space; i++) {
		f->nodes[at + i] = (TB_Node){ 0 };
	}
    
	for (TB_Register i = 0; i < extra_register_space; i++) {
		tb_function_find_replace_reg(f, end + i, end + extra_register_space + i);
	}
    
	// Mark any PHI nodes, all replications exist in the same basic block so these can be shared.
	TB_Register* latest = tb_tls_push(tls, sizeof(TB_Register) * registers_to_replicate);
	for (TB_Register i = 0; i < registers_to_replicate; i++) {
		if (f->nodes[start + i].type == TB_PHI2) {
			assert(f->nodes[start + i].phi2.a_label != f->nodes[start + i].phi2.b_label);
            
			if (f->nodes[start + i].phi2.a_label == label_reg) latest[i] = f->nodes[start + i].phi2.a;
			else if (f->nodes[start + i].phi2.b_label == label_reg) latest[i] = f->nodes[start + i].phi2.b;
		}
		else latest[i] = 0;
	}
    
	// Place the replicas
	for (int t = 0; t < times; t++) {
		TB_Register base = at + (t * registers_to_replicate);
        
		for (TB_Register i = 0; i < registers_to_replicate; i++) {
			if (f->nodes[start + i].type == TB_PHI2) {
				if (f->nodes[start + i].phi2.a_label == label_reg) {
					f->nodes[base + i] = (TB_Node){
						.type = TB_PASS,
						.dt = f->nodes[start + i].dt,
						.pass = latest[i]
					};
                    
					latest[i] = (f->nodes[start + i].phi2.a - start) + base;
				}
				else if (f->nodes[start + i].phi2.b_label == label_reg) {
					f->nodes[base + i] = (TB_Node){
						.type = TB_PASS,
						.dt = f->nodes[start + i].dt,
						.pass = latest[i]
					};
                    
					latest[i] = (f->nodes[start + i].phi2.b - start) + base;
				}
			}
			else if (f->nodes[start + i].type == TB_CMP_NE) {
				f->nodes[base + i] = f->nodes[start + i];
                
				// remap
				if (f->nodes[base + i].cmp.a >= start &&
					f->nodes[base + i].cmp.a < end &&
					latest[f->nodes[base + i].cmp.a - start]) {
					f->nodes[base + i].cmp.a = latest[f->nodes[base + i].cmp.a - start];
				}
                
				// remap
				if (f->nodes[base + i].cmp.b >= start &&
					f->nodes[base + i].cmp.b < end &&
					latest[f->nodes[base + i].cmp.b - start]) {
					f->nodes[base + i].cmp.b = latest[f->nodes[base + i].cmp.b - start];
				}
			}
			else if (f->nodes[start + i].type == TB_ADD) {
				f->nodes[base + i] = f->nodes[start + i];
                
				// remap
				if (f->nodes[base + i].i_arith.a >= start &&
					f->nodes[base + i].i_arith.a < end &&
					latest[f->nodes[base + i].i_arith.a - start]) {
					f->nodes[base + i].i_arith.a = (f->nodes[base + i].i_arith.a - start) + base;
				}
                
				// remap
				if (f->nodes[base + i].i_arith.b >= start &&
					f->nodes[base + i].i_arith.b < end &&
					latest[f->nodes[base + i].i_arith.b - start]) {
					f->nodes[base + i].i_arith.b = (f->nodes[base + i].i_arith.b - start) + base;
				}
			}
			else f->nodes[base + i] = f->nodes[start + i];
		}
	}
    
	// Remove all PASS statements
	for (int t = 0; t < times; t++) {
		TB_Register base = at + (t * registers_to_replicate);
        
		for (TB_Register i = 0; i < registers_to_replicate; i++) {
			if (f->nodes[base + i].type == TB_PASS) {
				tb_function_find_replace_reg(f, base + i, f->nodes[base + i].pass);
				f->nodes[base + i] = (TB_Node){ 0 };
			}
		}
	}
    
	// Change PHI node to incorporate the replications
	for (TB_Register i = 0; i < registers_to_replicate; i++) {
		if (f->nodes[start + i].type == TB_PHI2) {
			assert(f->nodes[start + i].phi2.a_label != f->nodes[start + i].phi2.b_label);
            
			TB_Register original = TB_NULL_REG;
			TB_Register new_value = latest[i];
			if (f->nodes[start + i].phi2.a_label == label_reg) {
				original = f->nodes[start + i].phi2.a;
				f->nodes[start + i].phi2.a = new_value;
			}
			else if (f->nodes[start + i].phi2.b_label == label_reg) {
				original = f->nodes[start + i].phi2.b;
				f->nodes[start + i].phi2.b = new_value;
			}
            
			// Replace any PHI nodes which used the old latest value
			if (original) {
				for (TB_Register j = 1; j < f->count; j++) {
					if (f->nodes[j].type == TB_PHI1) {
						// Replace
						if (f->nodes[j].phi1.a == original) f->nodes[j].phi1.a = new_value;
					}
					else if (f->nodes[j].type == TB_PHI2) {
						// Replace
						if (f->nodes[j].phi2.a == original) f->nodes[j].phi2.a = new_value;
						if (f->nodes[j].phi2.b == original) f->nodes[j].phi2.b = new_value;
					}
				}
			}
		}
		else latest[i] = 0;
	}
    
	return extra_register_space;
}

bool tb_opt_loop_unroll(TB_Function* f) {
	TB_TemporaryStorage* tls = tb_tls_allocate();
	int changes = 0;
    
	TB_Register last_label_reg = 0;
	size_t i = 1;
	while (i < f->count) {
		if (f->nodes[i].type == TB_LABEL) {
			TB_Register terminator = f->nodes[i].label.terminator;
            
			if (f->nodes[terminator].type == TB_IF) {
				TB_Register if_true_pos = tb_find_reg_from_label(f, f->nodes[terminator].if_.if_true);
				TB_Register if_false_pos = tb_find_reg_from_label(f, f->nodes[terminator].if_.if_false);
                
				bool loops_on_false = tb_can_reach(f, if_true_pos, i);
				bool loops_on_true = tb_can_reach(f, if_false_pos, i);
                
				if (loops_on_true && loops_on_false) printf("Warning: infinite loop :(\n");
				else if (!loops_on_true && !loops_on_false) { /* Not a loop */ }
				else {
					TB_Register exit = loops_on_true ? if_true_pos : if_false_pos;
					enum TB_RegisterType cond_type = f->nodes[f->nodes[terminator].if_.cond].type;
                    
					bool loop_count_known = false;
					uint64_t loop_count = 0;
                    
					if (cond_type == TB_CMP_EQ || cond_type == TB_CMP_NE) {
						bool exit_on_equal = cond_type == TB_CMP_NE ? !loops_on_true : loops_on_true;
                        
						TB_Register a = f->nodes[f->nodes[terminator].if_.cond].cmp.a;
						TB_Register b = f->nodes[f->nodes[terminator].if_.cond].cmp.b;
                        
						TB_RegisterDataflow df_a = tb_analyze_register_dataflow(f, i, a);
						TB_RegisterDataflow df_b = tb_analyze_register_dataflow(f, i, b);
                        
						if (df_a.pattern == TB_DataflowPattern_IntStep &&
							df_b.pattern == TB_DataflowPattern_IntConstant) {
							// by the time it reaches the if statement it's already done an iteration
							if (df_a.istep.pre_iterator) df_a.istep.b++;
                            
							if (exit_on_equal && df_b.iconst.v >= df_a.istep.b) {
								loop_count_known = true;
								loop_count = ((df_b.iconst.v - df_a.istep.b) / df_a.istep.m);
                                
								// Verify that it lines up perfectly
								int df_endpoint = (loop_count * df_a.istep.m) + df_a.istep.b;
								if (df_endpoint != df_b.iconst.v) loop_count = UINT64_MAX;
                                
								// TODO(Negate): verify that this is congruent with pre_iterator.
								// It goes through the loop once even before checking the if statement
								if (df_a.istep.pre_iterator) loop_count++;
							}
						}
					}
                    
					// Decide if it's worth unrolling
					if (loop_count_known) {
						if (loop_count == UINT64_MAX) printf("Warning: infinite loop :(\n");
						else if (loop_count == 1) {
							// Remove loop stuff since it only goes once
							f->nodes[terminator] = (TB_Node){ 0 };
                            
							for (TB_Register j = i + 1; j < terminator; j++) {
								if (f->nodes[j].type == TB_PHI2) {
									// FIXME(NeGate): Verify this is correct, im not actually sure
									if (f->nodes[j].phi2.a_label == last_label_reg) {
										tb_function_find_replace_reg(f, j, f->nodes[j].phi2.a);
										f->nodes[j] = (TB_Node){ 0 };
									}
									else if (f->nodes[j].phi2.b_label == last_label_reg) {
										tb_function_find_replace_reg(f, j, f->nodes[j].phi2.b);
										f->nodes[j] = (TB_Node){ 0 };
									}
								}
							}
                            
							// Replace any reference to this label from PHI nodes and make them point to the predecessor
							for (TB_Register j = 1; j < f->count; j++) {
								if (f->nodes[j].type == TB_PHI1 && f->nodes[j].phi1.a_label == i) {
									f->nodes[j].phi1.a_label = last_label_reg;
								}
							}
                            
							// Change the previous label's terminator
							assert(last_label_reg);
							f->nodes[last_label_reg].label.terminator = exit;
							// Kill label since it's no longer even a loop
							f->nodes[i] = (TB_Node){ 0 };
                            
							changes++;
						}
						// TODO: Implement a way better heuristic lol
						else if (loop_count > 1 && loop_count <= 4) {
							// Kill exit instruction since we know when it happens
							f->nodes[terminator] = (TB_Node){ 0 };
							TB_Register extra_body_size = tb_replicate_loop_statements(f, tls, i, i + 1, terminator, terminator, loop_count - 1);
                            
							for (TB_Register j = i + 1; j < terminator + extra_body_size; j++) {
								if (f->nodes[j].type == TB_PHI2) {
									// FIXME(NeGate): Verify this is correct, im not actually sure
									if (f->nodes[j].phi2.a_label == last_label_reg) {
										tb_function_find_replace_reg(f, j, f->nodes[j].phi2.a);
										f->nodes[j] = (TB_Node){ 0 };
									}
									else if (f->nodes[j].phi2.b_label == last_label_reg) {
										tb_function_find_replace_reg(f, j, f->nodes[j].phi2.b);
										f->nodes[j] = (TB_Node){ 0 };
									}
								}
							}
                            
							// Replace any reference to this label from PHI nodes and make them point to the predecessor
							for (TB_Register j = 1; j < f->count; j++) {
								if (f->nodes[j].type == TB_PHI1 && f->nodes[j].phi1.a_label == i) {
									f->nodes[j].phi1.a_label = last_label_reg;
								}
							}
                            
							// FIXME(NeGate): check if this is correct too!
							if (exit >= terminator) exit += extra_body_size;
                            
							// Change the previous label's terminator
							assert(last_label_reg);
							f->nodes[last_label_reg].label.terminator = exit;
							// Kill label since it's no longer even a loop
							f->nodes[i] = (TB_Node){ 0 };
                            
							changes++;
						}
					}
				}
			}
			else if (f->nodes[terminator].type == TB_GOTO) {
				bool is_loop = tb_can_reach(f, tb_find_reg_from_label(f, f->nodes[terminator].goto_.label), i);
                
				if (is_loop) printf("Warning: infinite loop :(\n");
			}
			else if (f->nodes[terminator].type == TB_LABEL) {}
			else if (f->nodes[terminator].type == TB_RET) {}
			else abort();
            
			last_label_reg = i;
			i = terminator;
			continue;
		}
        
		i++;
	}
    
	return (changes > 0);
}

bool tb_opt_mem2reg(TB_Function* f) {
	typedef struct VarRevision {
		TB_Register label;
		TB_Register reg;
	} VarRevision;
    
	int changes = 0;
	TB_TemporaryStorage* tls = tb_tls_allocate();
    
	for (TB_Register i = 1; i < f->count; i++) {
		if (f->nodes[i].type == TB_LOCAL || f->nodes[i].type == TB_PARAM_ADDR) {
			// Verify that it holds a consistent type, no unions
			// or weird type casts.
			bool bad_sroa = false;
			bool initialized = false;
            
			TB_Register initial_value;
			TB_Register initial_store;
			TB_Register initial_label;
			TB_DataType dt;
            
			// Param addrs are automatically initialized with
			// the parameter value
			if (f->nodes[i].type == TB_PARAM_ADDR) {
				// TODO(NeGate): Verify that parameter storage is always in the first basic block
				initialized = true;
				initial_label = 1;
				initial_store = i;
				initial_value = f->nodes[i].param_addr.param;
				dt = f->nodes[f->nodes[i].param_addr.param].dt;
			}
            
			for (size_t j = 1; j < f->count; j++) {
				if (!initialized) {
					if (f->nodes[j].type == TB_LABEL) {
						initial_label = j;
					}
					else if (f->nodes[j].type == TB_STORE &&
                             f->nodes[j].store.address == i) {
						initialized = true;
						initial_store = j;
						initial_value = f->nodes[j].store.value;
						dt = f->nodes[j].dt;
					}
					else if (f->nodes[j].type == TB_LOAD &&
                             f->nodes[j].load.address == i) {
						// Uninitialized load
						abort();
					}
				}
				else {
					if (f->nodes[j].type == TB_STORE &&
						f->nodes[j].store.address == i) {
						if (memcmp(&f->nodes[j].dt, &dt, sizeof(TB_DataType)) != 0) bad_sroa = true;
					}
					else if (f->nodes[j].type == TB_LOAD &&
                             f->nodes[j].load.address == i) {
						if (memcmp(&f->nodes[j].dt, &dt, sizeof(TB_DataType)) != 0) bad_sroa = true;
					}
				}
			}
            
			if (!initialized) continue;
			if (bad_sroa) continue;
            
			// Kill allocation
			f->nodes[i] = (TB_Node){ 0 };
            
			// Make a copy of the initial value in case multiple registers 
			// are using it, it makes it easier to track in this pass.
			f->nodes[initial_store] = f->nodes[initial_value];
            
			// Stack of all variable's revision history
			*((VarRevision*)tb_tls_push(tls, sizeof(VarRevision))) = (VarRevision){
				.label = initial_label,
				.reg = initial_store
			};
            
			// Used in case two loads exist within one basic block
			TB_Register last_phi_node_per_bb = 0;
            
			size_t j = initial_store + 1;
			TB_Register current_label = initial_label;
			while (j < f->count) {
				if (f->nodes[j].type == TB_LABEL) {
					current_label = j;
					last_phi_node_per_bb = 0;
				}
				else if (f->nodes[j].type == TB_LOAD &&
                         f->nodes[j].load.address == i) {
					VarRevision* last_rev = (VarRevision*)tb_tls_peek(tls, sizeof(VarRevision));
                    
					if (last_rev->label != current_label) {
						// Insert phi node
						f->nodes[j] = (TB_Node){
							.type = TB_PHI1,
							.dt = dt,
							.phi1 = {
								.a_label = last_rev->label,
								.a = last_rev->reg
							}
						};
                        
						//printf("Load replaced with PHI L%u:r%u\n", f->nodes[last_rev->label].label.id, last_rev->reg);
						last_phi_node_per_bb = j;
					}
					else {
						if (last_phi_node_per_bb) {
							//printf("Load replaced with previous PHI L%u:r%u\n", f->nodes[last_rev->label].label.id, last_rev->reg);
                            
							tb_function_find_replace_reg(f, j, last_phi_node_per_bb);
                            
							// Kill load
							f->nodes[j] = (TB_Node){ 0 };
						}
						else {
							// Replace load
							f->nodes[j] = (TB_Node){ .type = TB_PASS, .pass = last_rev->reg };
						}
					}
				}
				else if (f->nodes[j].type == TB_STORE &&
                         f->nodes[j].store.address == i) {
					// Check if this store can escape this basic
					// block, if so then append to the original PHI node
					TB_Register terminator = f->nodes[current_label].label.terminator;
					if (f->nodes[terminator].type == TB_IF) {
						VarRevision* last_rev = (VarRevision*)tb_tls_peek(tls, sizeof(VarRevision));
                        
						// Check both paths for any loads of the register `i`
						TB_Register path_true = tb_find_reg_from_label(f, f->nodes[terminator].if_.if_true);
						TB_Register path_true_end = f->nodes[path_true].label.terminator;
                        
						TB_Register first_use = tb_find_first_use(f, last_rev->reg, path_true, path_true_end);
						if (first_use) {
							// Append to phi node
							//printf("Append PHI to L%u:r%u\n", f->nodes[current_label].label.id, f->nodes[j].store.value);
							assert(f->nodes[first_use].type == TB_PHI1);
                            
							f->nodes[first_use].type = TB_PHI2;
							f->nodes[first_use].phi2.b_label = current_label;
							f->nodes[first_use].phi2.b = f->nodes[j].store.value;
						}
                        
						TB_Register path_false = tb_find_reg_from_label(f, f->nodes[terminator].if_.if_false);
						TB_Register path_false_end = f->nodes[path_false].label.terminator;
                        
						first_use = tb_find_first_use(f, last_rev->reg, path_false, path_false_end);
						if (first_use) {
							// Append to phi node
							//printf("Append PHI to L%u:r%u\n", f->nodes[current_label].label.id, f->nodes[j].store.value);
							assert(f->nodes[first_use].type == TB_PHI1);
                            
							f->nodes[first_use].type = TB_PHI2;
							f->nodes[first_use].phi2.b_label = current_label;
							f->nodes[first_use].phi2.b = f->nodes[j].store.value;
						}
					}
					else abort();
                    
					// Mark this as the latest
					*((VarRevision*)tb_tls_push(tls, sizeof(VarRevision))) = (VarRevision){
						.label = current_label,
						.reg = f->nodes[j].store.value
					};
                    
					// Kill store
					f->nodes[j] = (TB_Node){ 0 };
				}
                
				j++;
			}
            
			// Remove all PASS statements
			for (TB_Register k = 1; k < f->count; k++) {
				if (f->nodes[k].type == TB_PASS) {
					// Replace all references
					tb_function_find_replace_reg(f, k, f->nodes[k].pass);
                    
					// Kill PASS
					f->nodes[k] = (TB_Node){ 0 };
				}
			}
            
			changes++;
		}
	}
    
	return (changes > 0);
}

// Some phi nodes are wasteful, get rid of them
bool tb_opt_phi_cleanup(TB_Function* f) {
	int changes = 0;
    
	for (TB_Register i = 1; i < f->count; i++) {
		if (f->nodes[i].type == TB_PHI1) {
			TB_Register a = f->nodes[i].phi1.a;
            
			if (f->nodes[a].type != TB_INT_CONST) continue;
            
			// Replace this phi node with that constant
			f->nodes[i] = f->nodes[a];
			changes++;
		}
	}
    
	return (changes > 0);
}

bool tb_opt_cse(TB_Function* f) {
	int changes = 0;
    
	for (TB_Register i = 1; i < f->count; i++) {
		if (f->nodes[i].type == TB_ADD) {
			if (f->nodes[f->nodes[i].i_arith.a].type == TB_INT_CONST &&
				f->nodes[f->nodes[i].i_arith.b].type == TB_INT_CONST) {
				TB_Int128 result = tb_emulate_add(
                                                  f, f->nodes[i].i_arith.arith_behavior, f->nodes[i].dt,
                                                  f->nodes[f->nodes[i].i_arith.a].i_const, f->nodes[f->nodes[i].i_arith.b].i_const
                                                  );
                
				f->nodes[i].type = TB_INT_CONST;
				f->nodes[i].i_const = result;
				changes++;
			}
		}
	}
    
	return (changes > 0);
}

bool tb_opt_dce(TB_Function* f) {
	int changes = 0;
	TB_TemporaryStorage* tls = tb_tls_allocate();
    
	size_t* intervals = tb_tls_push(tls, f->count * sizeof(size_t));
	tb_find_live_intervals(intervals, f);
    
	for (TB_Register i = 1; i < f->count; i++) {
		if (intervals[i] == 0) {
			switch (f->nodes[i].type) {
				// keep
                case TB_NULL:
                case TB_LABEL:
                case TB_PHI1:
                case TB_PHI2:
                case TB_GOTO:
                case TB_IF:
                case TB_RET:
				break;
				// delete:
                case TB_INT_CONST:
                case TB_ADD:
                case TB_SUB:
                case TB_MUL:
                case TB_SDIV:
                case TB_UDIV:
                case TB_STORE:
                case TB_LOAD:
                case TB_PARAM:
                case TB_CMP_EQ:
                case TB_CMP_NE:
                case TB_CMP_SLT:
                case TB_CMP_SLE:
                case TB_CMP_ULT:
                case TB_CMP_ULE:
                case TB_CMP_FLT:
                case TB_CMP_FLE:
				f->nodes[i] = (TB_Node){ 0 };
				break;
                default:
				abort();
			}
		}
	}
    
	return (changes > 0);
}
