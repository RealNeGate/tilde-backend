
bool tb_opt_compact_dead_regs(TB_Function* f) {
	// Shift the dead regs out
	for (TB_Register i = 1; i < f->nodes.count; i++) if (f->nodes.type[i] == TB_NULL) {
		// Check if there's repeated dead regs to remove them all at once
		TB_Register j = i + 1;
		while (j < f->nodes.count && f->nodes.type[j] == TB_NULL) j++; 
		size_t regs_to_remove = j - i;
		
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
		return true;
	}
	
	return false;
}

bool tb_opt_remove_pass_node(TB_Function* f) {
	int changes = 0;
	
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_PASS) {
			tb_function_find_replace_reg(f, i, f->nodes.payload[i].pass);
			changes++;
		}
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
			if ((f->nodes.type[a] == TB_SIGN_EXT || f->nodes.type[a] == TB_ZERO_EXT) &&
				f->nodes.type[b] == TB_INT_CONST) {
				TB_Register src = f->nodes.payload[a].ext;
				
				f->nodes.payload[i].cmp.a = src;
			}
		} else if (type == TB_ADD || type == TB_MUL) {
			TB_Register a = f->nodes.payload[i].i_arith.a;
			TB_Register b = f->nodes.payload[i].i_arith.b;
			
			// Move all integer constants to the right side
			if (f->nodes.type[a] == TB_INT_CONST && f->nodes.type[b] != TB_INT_CONST) {
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
		} else if (type == TB_ARRAY_ACCESS) {
			TB_Register base = f->nodes.payload[i].array_access.base;
			TB_Register index = f->nodes.payload[i].array_access.index;
			
			if (f->nodes.type[index] == TB_INT_CONST) {
				uint64_t index_imm = f->nodes.payload[index].i_const;
				
				if (index_imm == 0) {
					f->nodes.type[i] = TB_PASS;
					f->nodes.payload[i].pass = base;
					changes++;
				}
			}
		} else if (type == TB_PHI2) {
			// remove useless phi
			TB_Register a = f->nodes.payload[i].phi2.a;
			TB_Register b = f->nodes.payload[i].phi2.b;
			
			// trivial phi
			if (a == b) {
				f->nodes.type[i] = TB_PASS;
				f->nodes.payload[i].pass = a;
				changes++;
			} else if (i == b) {
				f->nodes.type[i] = TB_PASS;
				f->nodes.payload[i].pass = a;
				changes++;
			} else if (i == a) {
				f->nodes.type[i] = TB_PASS;
				f->nodes.payload[i].pass = b;
				changes++;
			}
		} else if (f->nodes.type[i] == TB_PHI1) {
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
			if (f->nodes.type[i + 1] == TB_LABEL) {
				TB_Label after = f->nodes.payload[i].label.id;
				TB_Label before = f->nodes.payload[i+1].label.id;
				
				// inherit the terminator
				f->nodes.payload[i].label.terminator = f->nodes.payload[i+1].label.terminator;
				
				tb_kill_op(f, i + 1);
				
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
					
					// TODO(NeGate): implement switch statement
					case TB_SWITCH: tb_todo();
					
					default: break;
				}
				
				changes++;
			}
		}
	}
	
	return (changes > 0);
}
