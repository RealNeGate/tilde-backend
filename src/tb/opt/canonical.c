#include "../tb_internal.h"

bool tb_opt_compact_dead_regs(TB_Function* f) {
	int changes = 0;
	
	// Shift the dead regs out
	for (TB_Register i = 1; i < f->nodes.count; i++) if (f->nodes.type[i] == TB_NULL) {
		// Check if there's repeated dead regs to remove them all at once
		TB_Register j = i + 1;
		while (j < f->nodes.count && f->nodes.type[j] == TB_NULL) j++;
		size_t regs_to_remove = j - i;
		
		OPTIMIZER_LOG(i, "deleted NOPs at r%d-r%d", i, j);
		
		// Shift everything back
		size_t regs_beyond = f->nodes.count - j;
		memmove(&f->nodes.type[i], &f->nodes.type[j], regs_beyond * sizeof(TB_RegType));
		memmove(&f->nodes.dt[i], &f->nodes.dt[j], regs_beyond * sizeof(TB_DataType));
		memmove(&f->nodes.payload[i], &f->nodes.payload[j], regs_beyond * sizeof(TB_RegPayload));
		
		// Move references
		// NOTE(NeGate): Usually these go backwards since they are inserting ops
		// and thus we don't want the find and replace to mess up anything coming
		// up but since this version needs to shift backwards we find & replace
		// forwards.
		size_t k = 0;
		while (k < regs_beyond) {
			tb_function_find_replace_reg(f, j + k, i + k);
			k++;
		}
		
		// Move the count back
		f->nodes.count -= regs_to_remove;
		changes++;
	}
	
	return changes;
}

bool tb_opt_remove_pass_node(TB_Function* f) {
	int changes = 0;
	TB_TemporaryStorage* tls = tb_tls_allocate();
    
	TB_Register* intervals = tb_tls_push(tls, f->nodes.count * sizeof(TB_Register));
	tb_find_live_intervals(f, intervals);
    
	loop(i, f->nodes.count) if (intervals[i] > 0 && f->nodes.type[i] == TB_PASS) {
		OPTIMIZER_LOG(i, "removing PASS node");
		TB_Register replacement = f->nodes.payload[i].pass;
		TB_Register endpoint = intervals[i]+1;
		
		if (endpoint > i) {
#define X(reg) reg = (reg == i) ? replacement : reg;
			loop_range(j, i, endpoint) {
				TB_RegType type = f->nodes.type[j]; \
				TB_RegPayload* p = &f->nodes.payload[j]; \
				
				switch (type) {
					FOR_EACH_REGISTER_IN_NODE(X);
					default: tb_panic(false, "Unknown node type: %d", type);
				}
			}
#undef X
		}
		
		tb_kill_op(f, i);
		changes++;
	}
	
	return changes;
}

bool tb_opt_canonicalize(TB_Function* f) {
	int changes = 0;
	loop_range(i, 1, f->nodes.count) {
		TB_RegType type = f->nodes.type[i];
		
		// TODO(NeGate): Maybe we should have a proper function/macro
		// for detecting integer compares like this
		if (type >= TB_CMP_EQ && type <= TB_CMP_ULE) {
			// Sometimes we promote some types up when we don't need to
			TB_Register a = f->nodes.payload[i].cmp.a;
			TB_Register b = f->nodes.payload[i].cmp.b;
			
			// (cmp (sxt/zxt A) (int B))
			// VVV
			// (cmp A (int B))
			if (f->nodes.type[a] == TB_SIGN_EXT && f->nodes.type[b] == TB_SIGNED_CONST) {
				OPTIMIZER_LOG(i, "removed unnecessary zero extension for compare against constants");
				
				TB_Register src = f->nodes.payload[a].ext;
				f->nodes.payload[i].cmp.a = src;
				changes++;
			} else if (f->nodes.type[a] == TB_ZERO_EXT && f->nodes.type[b] == TB_UNSIGNED_CONST) {
				OPTIMIZER_LOG(i, "removed unnecessary zero extension for compare against constants");
				
				TB_Register src = f->nodes.payload[a].ext;
				f->nodes.payload[i].cmp.a = src;
				changes++;
			}
		} else if (type == TB_ADD || type == TB_MUL) {
			TB_Register a = f->nodes.payload[i].i_arith.a;
			TB_Register b = f->nodes.payload[i].i_arith.b;
			
			// Move all integer constants to the right side
			bool is_aconst = (f->nodes.type[a] == TB_SIGNED_CONST ||
							  f->nodes.type[a] == TB_UNSIGNED_CONST);
			
			bool is_bconst = (f->nodes.type[b] == TB_SIGNED_CONST ||
							  f->nodes.type[b] == TB_UNSIGNED_CONST);
			
			if (is_aconst && !is_bconst) {
				OPTIMIZER_LOG(i, "moved constants to right hand side.");
				
				f->nodes.payload[i].i_arith.a = b;
				f->nodes.payload[i].i_arith.b = a;
				changes++;
			} else if (f->nodes.type[a] == type && f->nodes.type[b] != type) {
				// Reshuffle the adds from 
				// (x + y) + z => x + (y + z)
				/*TB_Register xy = a;
				TB_Register x = f->nodes.payload[a].i_arith.a;
				TB_Register y = f->nodes.payload[a].i_arith.b;
				TB_Register z = b;
				
				f->nodes.payload[i].i_arith.a = x;
				f->nodes.payload[i].i_arith.b = xy;
				
				f->nodes.payload[a].i_arith.a = y;
				f->nodes.payload[a].i_arith.b = z;
				changes++;*/
			}
		} else if (type == TB_MEMBER_ACCESS) {
			TB_Register base = f->nodes.payload[i].member_access.base;
			int32_t offset = f->nodes.payload[i].member_access.offset;
			
			if (offset == 0) {
				OPTIMIZER_LOG(i, "elided member access to first element");
				
				f->nodes.type[i] = TB_PASS;
				f->nodes.payload[i].pass = base;
				changes++;
			}
		} else if (type == TB_ARRAY_ACCESS) {
			TB_Register base = f->nodes.payload[i].array_access.base;
			TB_Register index = f->nodes.payload[i].array_access.index;
			
			if (f->nodes.type[index] == TB_SIGNED_CONST || f->nodes.type[index] == TB_UNSIGNED_CONST) {
				uint64_t index_imm = f->nodes.payload[index].u_const;
				
				if (index_imm == 0) {
					OPTIMIZER_LOG(i, "elided array access to first element");
					
					f->nodes.type[i] = TB_PASS;
					f->nodes.payload[i].pass = base;
					changes++;
				}
			}
		} else if (f->nodes.type[i] == TB_UNSIGNED_CONST) {
			uint64_t data = f->nodes.payload[i].u_const;
			
			loop_range(j, i+1, f->nodes.count) {
				if (f->nodes.type[j] == TB_UNSIGNED_CONST && f->nodes.payload[j].u_const == data) {
					OPTIMIZER_LOG(i, "merged integer constants");
					
					f->nodes.type[j] = TB_PASS;
					f->nodes.payload[j].pass = i;
					changes++;
				}
			}
		} else if (f->nodes.type[i] == TB_SIGNED_CONST) {
			int64_t data = f->nodes.payload[i].s_const;
			
			loop_range(j, i+1, f->nodes.count) {
				if (f->nodes.type[j] == TB_SIGNED_CONST && f->nodes.payload[j].s_const == data) {
					OPTIMIZER_LOG(i, "merged integer constants");
					
					f->nodes.type[j] = TB_PASS;
					f->nodes.payload[j].pass = i;
					changes++;
				}
			}
		} else if (type == TB_INT2PTR) {
			TB_Register src = f->nodes.payload[i].cvt.src;
			
			if (f->nodes.type[src] == TB_SIGNED_CONST || f->nodes.type[src] == TB_UNSIGNED_CONST) {
				OPTIMIZER_LOG(i, "constant int2ptr removed.");
				
				uint64_t imm = f->nodes.payload[src].u_const;
				
				f->nodes.type[i] = TB_UNSIGNED_CONST;
				f->nodes.dt[i] = TB_TYPE_PTR;
				f->nodes.payload[i].u_const = imm;
				changes++;
			}
		} else if (type == TB_TRUNCATE) {
			TB_Register src = f->nodes.payload[i].cvt.src;
			
			if (f->nodes.type[src] == TB_SIGNED_CONST || f->nodes.type[src] == TB_UNSIGNED_CONST) {
				OPTIMIZER_LOG(i, "constant truncate removed.");
				
				uint64_t imm = f->nodes.payload[src].u_const;
				
				uint64_t shift = 64 - (8 << (f->nodes.dt[src].type - TB_I8));
				uint64_t mask = (~0ull) >> shift;
				
				f->nodes.type[i] = TB_UNSIGNED_CONST;
				f->nodes.dt[i] = f->nodes.dt[src];
				f->nodes.payload[i].u_const = imm & mask;
				changes++;
			}
		} else if (type == TB_PHI2) {
			// remove useless phi
			TB_Register a = f->nodes.payload[i].phi2.a;
			TB_Register b = f->nodes.payload[i].phi2.b;
			
			// trivial phi
			if (a == b) {
				OPTIMIZER_LOG(i, "removed trivial phi");
				
				f->nodes.type[i] = TB_PASS;
				f->nodes.payload[i].pass = a;
				changes++;
			} else if (i == b) {
				OPTIMIZER_LOG(i, "removed trivial phi");
				
				f->nodes.type[i] = TB_PASS;
				f->nodes.payload[i].pass = a;
				changes++;
			} else if (i == a) {
				OPTIMIZER_LOG(i, "removed trivial phi");
				
				f->nodes.type[i] = TB_PASS;
				f->nodes.payload[i].pass = b;
				changes++;
			}
		} else if (f->nodes.type[i] == TB_PHI1) {
			OPTIMIZER_LOG(i, "removed trivial phi");
			
			// remove useless phi
			TB_Register reg = f->nodes.payload[i].phi1.a;
			
			f->nodes.type[i] = TB_PASS;
			f->nodes.payload[i].pass = reg;
			changes++;
		} else if (f->nodes.type[i] == TB_LABEL) {
			// By eliding labels that share the same body
			// we can improve the analysis.
			
			// NOTE(NeGate): We can read ahead because of the zeroed
			// out padding space in the TB_NodeStream::type stream
			if (false /* TODO(NeGate): f->nodes.type[i + 1] == TB_LABEL */) {
				OPTIMIZER_LOG(i, "combined subsequent labels");
				
				TB_Label after = f->nodes.payload[i].label.id;
				TB_Label before = f->nodes.payload[i+1].label.id;
				
				// inherit the terminator
				f->nodes.payload[i].label.terminator = f->nodes.payload[i+1].label.terminator;
				
				tb_kill_op(f, i + 1);
				
				// TODO(NeGate): We need a find & replace that's global instead
				// of assuming that the definition is always in front of the uses.
				tb_function_find_replace_reg(f, i + 1, i);
				
				// replace all references to the label
				loop_range(j, 1, f->nodes.count) switch (f->nodes.type[j]) {
					case TB_GOTO: {
						if (f->nodes.payload[j].goto_.label == before) {
							f->nodes.payload[j].goto_.label = after;
						}
						break;
					}
					case TB_IF: {
						if (f->nodes.payload[j].if_.if_true == before) {
							f->nodes.payload[j].if_.if_true = after;
						}
						
						if (f->nodes.payload[j].if_.if_false == before) {
							f->nodes.payload[j].if_.if_false = after;
						}
						break;
					}
					
					case TB_SWITCH: {
						size_t entry_count = (f->nodes.payload[j].switch_.entries_end - f->nodes.payload[j].switch_.entries_start) / 2;
						loop(i, entry_count) {
							TB_SwitchEntry* entry = (TB_SwitchEntry*) &f->vla.data[f->nodes.payload[j].switch_.entries_start + (i * 2)];
							
							if (entry->key == before) {
								entry->key = after;
							}
						}
						
						if (f->nodes.payload[j].switch_.default_label == before) {
							f->nodes.payload[j].switch_.default_label = after;
						}
						break;
					}
					
					default: break;
				}
				
				changes++;
			}
		}
	}
	
	return (changes > 0);
}
