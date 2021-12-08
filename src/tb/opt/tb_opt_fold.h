
bool tb_opt_load_elim(TB_Function* f) {
	int changes = 0;
	
	loop(i, f->nodes.count) {
		if (f->nodes.type[i] != TB_LOAD) continue;
		
		TB_DataType dt = f->nodes.dt[i];
		TB_Register addr = f->nodes.payload[i].load.address;
		uint32_t alignment = f->nodes.payload[i].load.alignment;
		
		// Find memory latest revision
		TB_Register j = i - 1;
		do {
			assert(i != j);
			TB_RegType t = f->nodes.type[j];
			
			if (t == TB_STORE) {
				if (TB_DATA_TYPE_EQUALS(dt, f->nodes.dt[j]) &&
					f->nodes.payload[j].store.alignment == alignment &&
					f->nodes.payload[j].store.address == addr) {
					// if the load and store pair up, then elide the load
					// don't remove the store since it's unknown if it's
					// used elsewhere.
					f->nodes.type[i] = TB_PASS;
					f->nodes.payload[i].pass = f->nodes.payload[j].store.value;
					changes++;
				}
				
				// aliasing problems...
				// TODO(NeGate): Implement a noalias
				break;
			} else if (TB_IS_NODE_TERMINATOR(t) || TB_IS_NODE_SIDE_EFFECT(t)) {
				// Can't read past side effects or terminators, don't
				// know what might happen
				break;
			}
		} while (j--);
	}
	
	if (changes) {
		tb_function_print(f, stdout);
		printf("\n\n\n");
	}
	
	return changes;
}

bool tb_opt_fold(TB_Function* f) {
	int changes = 0;
	
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		TB_DataType dt = f->nodes.dt[i];
		
		// It's technically legal to read this space even tho SIGN_EXTEND and ZERO_EXTEND
		// don't use it so long as we don't actually depend on it's results.
		TB_Register a = f->nodes.payload[i].i_arith.a;
		TB_Register b = f->nodes.payload[i].i_arith.b;
		TB_ArithmaticBehavior ab = f->nodes.payload[i].i_arith.arith_behavior;
		
		if (f->nodes.type[i] >= TB_AND &&
			f->nodes.type[i] <= TB_SDIV &&
			f->nodes.type[a] == TB_INT_CONST &&
			f->nodes.type[b] == TB_INT_CONST) {
			uint64_t ai = f->nodes.payload[a].i_const;
			uint64_t bi = f->nodes.payload[b].i_const;
			
			uint64_t result;
			switch (f->nodes.type[i]) {
				case TB_AND:
				result = ai & bi;
				break;
				case TB_XOR:
				result = ai ^ bi;
				break;
				case TB_OR:
				result = ai | bi;
				break;
				case TB_ADD:
				result = tb_fold_add(ab, dt, ai, bi);
				break;
				case TB_SUB:
				result = tb_fold_sub(ab, dt, ai, bi);
				break;
				case TB_MUL:
				result = tb_fold_mul(ab, dt, ai, bi);
				break;
				case TB_UDIV:
				result = tb_fold_div(dt, ai, bi);
				break;
				case TB_SDIV:
				result = tb_fold_div(dt, ai, bi);
				break;
				default: 
				tb_todo();
			}
			
			f->nodes.type[i] = TB_INT_CONST;
			f->nodes.payload[i].i_const = result;
			changes++;
		}
		else if (f->nodes.type[i] == TB_SIGN_EXT || f->nodes.type[i] == TB_ZERO_EXT) {
			f->nodes.type[i] = TB_INT_CONST;
			f->nodes.payload[i].i_const = f->nodes.payload[a].i_const;
			changes++;
		}
	}
	
	return changes;
}

