
static int tb_estimate_expr_pressure(const TB_Function* f, TB_Register i) {
	switch (f->nodes.type[i]) {
		// If the node is a leaf (has no children), its Strahler number is one.
		case TB_PARAM:
		case TB_LOCAL:
		case TB_LOAD:
		case TB_INT_CONST: 
		case TB_MEMBER_ACCESS:
		case TB_ARRAY_ACCESS:
		return 1;
		// If the node has one child with Strahler number i, and all other children
		// have Strahler numbers less than i, then the Strahler number of the node
		// is i again.
		//
		// If the node has two or more children with Strahler number i, and no 
		// children with greater number, then the Strahler number of the node is i + 1.
		case TB_AND:
		case TB_OR:
		case TB_ADD:
		case TB_SUB:
		case TB_MUL:
		case TB_UDIV:
		case TB_SDIV: {
			int a = tb_estimate_expr_pressure(f, f->nodes.payload[i].i_arith.a);
			int b = tb_estimate_expr_pressure(f, f->nodes.payload[i].i_arith.b);
			
			if (a == b) return a + 1;
			else if (a > b) return a;
			else return b;
		}
		case TB_CMP_EQ:
		case TB_CMP_NE:
		case TB_CMP_SLT:
		case TB_CMP_SLE:
		case TB_CMP_ULT:
		case TB_CMP_ULE:
		case TB_CMP_FLT:
		case TB_CMP_FLE: {
			int a = tb_estimate_expr_pressure(f, f->nodes.payload[i].cmp.a);
			int b = tb_estimate_expr_pressure(f, f->nodes.payload[i].cmp.b);
			
			if (a == b) return a + 1;
			else if (a > b) return a;
			else return b;
		}
		default: tb_todo();
	}
}

static int tb_estimate_func_pressure(const TB_Function* f) {
	int max_pressure = 0;
	
	// get an estimate of register pressure in a basic block.
	// we'll be using it in the opportunity calculation.
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LABEL) {
			max_pressure = 0;
		} else if (f->nodes.type[i] == TB_CALL) {
			// tally up pressure from parameters, it'll probably be relevant too
			int p = 0;
			for (size_t j = f->nodes.payload[i].call.param_start; j < f->nodes.payload[i].call.param_end; j++) {
				p += tb_estimate_expr_pressure(f, f->vla.data[j]);
			}
			
			if (p > max_pressure) p = max_pressure;
		} else if (f->nodes.type[i] == TB_STORE) {
			int p = tb_estimate_expr_pressure(f, f->nodes.payload[i].store.address);
			p += tb_estimate_expr_pressure(f, f->nodes.payload[i].store.value);
			
			if (p > max_pressure) p = max_pressure;
		} else if (f->nodes.type[i] == TB_LOAD) {
			int p = tb_estimate_expr_pressure(f, f->nodes.payload[i].load.address);
			p += 2;
			
			if (p > max_pressure) p = max_pressure;
		} else if (f->nodes.type[i] == TB_IF) {
			int p = tb_estimate_expr_pressure(f, f->nodes.payload[i].if_.cond);
			p += 1;
			
			if (p > max_pressure) p = max_pressure;
		}
	}
	
	return max_pressure;
}

bool tb_opt_inline(TB_Function* f) {
	int max_pressure = 0;
	
	// get an estimate of register pressure in a basic block.
	// we'll be using it in the opportunity calculation.
	int changes = 0;
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_LABEL) {
			max_pressure = 0;
		} else if (f->nodes.type[i] == TB_CALL && f->nodes.payload[i].call.target != f) {
			// tally up pressure from parameters, it'll probably be relevant too
			int p = 0;
			for (size_t j = f->nodes.payload[i].call.param_start; j < f->nodes.payload[i].call.param_end; j++) {
				p += tb_estimate_expr_pressure(f, f->vla.data[j]);
			}
			if (p > max_pressure) max_pressure = p;
			
			int opportunity = 32;
			int cost = (max_pressure * 2) + tb_estimate_func_pressure(f->nodes.payload[i].call.target);
			
			// convert to inline'd call
			if (opportunity > cost) {
				//printf("Inlined call to '%s' in '%s'\n", f->nodes[i].call.target->name, f->name);
				
				f->nodes.type[i] = TB_ICALL;
				changes++;
			}
		} else if (f->nodes.type[i] == TB_STORE) {
			int p = tb_estimate_expr_pressure(f, f->nodes.payload[i].store.address);
			p += tb_estimate_expr_pressure(f, f->nodes.payload[i].store.value);
			
			if (p > max_pressure) max_pressure = p;
		} else if (f->nodes.type[i] == TB_LOAD) {
			int p = tb_estimate_expr_pressure(f, f->nodes.payload[i].load.address);
			p += 1;
			
			if (p > max_pressure) max_pressure = p;
		}
	}
	
	if (changes) {
		// spot inline calls and unfold them
		for (TB_Register i = 1; i < f->nodes.count; i++) if (f->nodes.type[i] == TB_ICALL) {
			const TB_Function* target = f->nodes.payload[i].call.target;
			const TB_Register* params = &f->vla.data[f->nodes.payload[i].call.param_start];
			
			// kill call
			tb_kill_op(f, i);
			
			// insert target's body
			TB_Register ret = tb_insert_copy_ops(f, params, i, target, 1, target->nodes.count - 1);
			
			// the return value will exist in `i + (target->count - 1)` since
			// got shifted over
			if (ret) tb_function_find_replace_reg(f, i + (target->nodes.count - 1), ret);
		}
		
		//tb_function_print(f);
		//printf("\n\n\n");
		
		// Remove the PASS nodes
		for (TB_Register i = 1; i < f->nodes.count; i++) if (f->nodes.type[i] == TB_PASS) {
			tb_function_find_replace_reg(f, i, f->nodes.payload[i].pass);
			
			// Kill PASS
			tb_kill_op(f, i);
		}
		
		//tb_function_print(f);
		//printf("\n\n\n");
	}
	
	return (changes > 0);
}
