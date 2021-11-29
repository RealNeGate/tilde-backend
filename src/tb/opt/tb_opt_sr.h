
bool tb_opt_strength_reduction(TB_Function* f) {
	int changes = 0;
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		if (f->nodes.type[i] == TB_MUL) {
			TB_Register a = f->nodes.payload[i].i_arith.a;
			TB_Register b = f->nodes.payload[i].i_arith.b;
			TB_DataType dt = f->nodes.dt[i];
			
			if (f->nodes.type[b] == TB_INT_CONST) {
				uint64_t b_const = f->nodes.payload[b].i_const;
				
				int log2 = __builtin_ffs(b_const) - 1;
				if (b_const == (1 << log2)) {
					// It's a power of two, swap in a left-shift
					TB_Register new_op = i - 1;
					tb_insert_op(f, new_op);
					
					f->nodes.type[new_op] = TB_INT_CONST;
					f->nodes.dt[new_op] = dt;
					f->nodes.payload[new_op] = (TB_RegPayload){
						.i_const = log2
					};
					
					// shift over `i` because it's infront of the insertion
					i++;
					
					f->nodes.type[i] = TB_SHL;
					f->nodes.payload[i].i_arith.a = a;
					f->nodes.payload[i].i_arith.b = new_op;
					changes++;
				}
			}
		}
	}
	
	return (changes > 0);
}
