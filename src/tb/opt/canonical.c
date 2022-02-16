#include "../tb_internal.h"

bool tb_opt_compact_dead_regs(TB_Function* f) {
	int changes = 0;
	
	TB_Node* n = &f->nodes.data[1];
	TB_Node* prev = &f->nodes.data[1];
	TB_Node* end = &f->nodes.data[0];
	
	// Find a NULL, skip over any NULLs until a valid node is found and cut out the middle men
	while (n != end) {
		if (n->type == TB_NULL) {
			tb_assume(prev != n);
			do {
				n = &f->nodes.data[n->next];
			} while (n != end);
			
			prev->next = n - f->nodes.data;
			changes++;
		}
		
		prev = n;
		n = &f->nodes.data[n->next];
	}
	
	return changes;
}

bool tb_opt_remove_pass_node(TB_Function* f) {
	int changes = 0;
	TB_FOR_EACH_NODE(n, f) {
		TB_Reg i = (n - f->nodes.data);
		
		if (n->type == TB_PASS) {
			tb_function_find_replace_reg(f, i, n->unary.src);
			
			n->type = TB_NULL;
			changes++;
		}
	}
	
	return changes;
}

bool tb_opt_canonicalize(TB_Function* f) {
	int changes = 0;
	TB_FOR_EACH_NODE(n, f) {
		TB_Reg i = (n - f->nodes.data);
		TB_NodeTypeEnum type = n->type;
		
		// TODO(NeGate): Maybe we should have a proper function/macro
		// for detecting integer compares like this
		if (type >= TB_CMP_EQ && type <= TB_CMP_ULE) {
			// Sometimes we promote some types up when we don't need to
			TB_Node* a = &f->nodes.data[n->cmp.a];
			TB_Node* b = &f->nodes.data[n->cmp.b];
			
			// (cmp (sxt/zxt A) (int B))
			// VVV
			// (cmp A (int B))
			if (a->type == TB_SIGN_EXT && b->type == TB_SIGNED_CONST) {
				OPTIMIZER_LOG(i, "removed unnecessary zero extension for compare against constants");
				
				n->cmp.a = a->unary.src;
				changes++;
			} else if (a->type == TB_ZERO_EXT && b->type == TB_UNSIGNED_CONST) {
				OPTIMIZER_LOG(i, "removed unnecessary zero extension for compare against constants");
				
				n->cmp.a = a->unary.src;
				changes++;
			}
		} else if (type == TB_ADD || type == TB_MUL) {
			TB_Node* a = &f->nodes.data[n->i_arith.a];
			TB_Node* b = &f->nodes.data[n->i_arith.b];
			
			// Move all integer constants to the right side
			bool is_aconst = (a->type == TB_SIGNED_CONST ||
							  a->type == TB_UNSIGNED_CONST);
			
			bool is_bconst = (b->type == TB_SIGNED_CONST ||
							  b->type == TB_UNSIGNED_CONST);
			
			if (is_aconst && !is_bconst) {
				OPTIMIZER_LOG(i, "moved constants to right hand side.");
				
				tb_swap(TB_Reg, n->i_arith.a, n->i_arith.b);
				changes++;
			} else if (a->type == type && b->type != type) {
				// Reshuffle the adds from 
				// (x + y) + z => x + (y + z)
				/*TB_Reg xy = a;
				TB_Reg x = f->nodes.payload[a].i_arith.a;
				TB_Reg y = f->nodes.payload[a].i_arith.b;
				TB_Reg z = b;
				
				f->nodes.payload[i].i_arith.a = x;
				f->nodes.payload[i].i_arith.b = xy;
				
				f->nodes.payload[a].i_arith.a = y;
				f->nodes.payload[a].i_arith.b = z;
				changes++;*/
			}
		} else if (type == TB_MEMBER_ACCESS) {
			int32_t offset = n->member_access.offset;
			
			if (offset == 0) {
				OPTIMIZER_LOG(i, "elided member access to first element");
				
				n->type = TB_PASS;
				n->pass.value = n->member_access.base;
				changes++;
			}
		} else if (type == TB_ARRAY_ACCESS) {
			TB_Node* index = &f->nodes.data[n->array_access.index];
			
			if (index->type == TB_SIGNED_CONST || index->type == TB_UNSIGNED_CONST) {
				uint64_t index_imm = index->uint.value;
				
				if (index_imm == 0) {
					OPTIMIZER_LOG(i, "elided array access to first element");
					
					n->type = TB_PASS;
					n->pass.value = n->array_access.base;
					changes++;
				}
			}
		} else if (type == TB_UNSIGNED_CONST || type == TB_SIGNED_CONST) {
			uint64_t data = n->uint.value;
			TB_DataType dt = n->dt;
			
			for (TB_Node* other = &f->nodes.data[n->next]; other != &f->nodes.data[0]; other = &f->nodes.data[other->next]) {
				if (other->type == type && other->uint.value == data && TB_DATA_TYPE_EQUALS(other->dt, dt)) {
					OPTIMIZER_LOG(i, "merged integer constants");
					
					other->type = TB_PASS;
					other->pass.value = i;
					changes++;
				}
			}
		} else if (type == TB_INT2PTR) {
			TB_Node* src = &f->nodes.data[n->unary.src];
			
			if (src->type == TB_SIGNED_CONST || src->type == TB_UNSIGNED_CONST) {
				OPTIMIZER_LOG(i, "constant int2ptr removed.");
				
				uint64_t imm = src->uint.value;
				
				n->type = TB_UNSIGNED_CONST;
				n->dt = TB_TYPE_PTR;
				n->uint.value = imm;
				changes++;
			}
		} else if (type == TB_TRUNCATE) {
			TB_Node* src = &f->nodes.data[n->unary.src];
			
			if (src->type == TB_SIGNED_CONST || src->type == TB_UNSIGNED_CONST) {
				OPTIMIZER_LOG(i, "constant truncate removed.");
				
				uint64_t imm = src->uint.value;
				
				uint64_t shift = 64 - (8 << (src->dt.type - TB_I8));
				uint64_t mask = (~0ull) >> shift;
				
				TB_DataType dt = src->dt;
				
				n->type = TB_UNSIGNED_CONST;
				n->dt = dt;
				n->uint.value = imm & mask;
				changes++;
			}
		} else if (type == TB_PHI2) {
			// remove useless phi
			TB_Reg a = n->phi2.a;
			TB_Reg b = n->phi2.b;
			
			// trivial phi
			if (a == b) {
				OPTIMIZER_LOG(i, "removed trivial phi");
				
				n->type = TB_PASS;
				n->pass.value = a;
				changes++;
			} else if (i == b) {
				OPTIMIZER_LOG(i, "removed trivial phi");
				
				n->type = TB_PASS;
				n->pass.value = a;
				changes++;
			} else if (i == a) {
				OPTIMIZER_LOG(i, "removed trivial phi");
				
				n->type = TB_PASS;
				n->pass.value = b;
				changes++;
			}
		} else if (n->type == TB_PHI1) {
			OPTIMIZER_LOG(i, "removed trivial phi");
			
			// remove useless phi
			TB_Reg reg = n->phi1.a;
			
			n->type = TB_PASS;
			n->pass.value = reg;
			changes++;
		}
	}
	
	return (changes > 0);
}
