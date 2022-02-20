#include "../tb_internal.h"

bool tb_opt_fold(TB_Function* f) {
	int changes = 0;
	
	TB_FOR_EACH_NODE(n, f) {
		TB_DataType dt = n->dt;
		
		if (n->type >= TB_AND && n->type <= TB_SDIV) {
			TB_Node* a = &f->nodes.data[n->i_arith.a];
			TB_Node* b = &f->nodes.data[n->i_arith.b];
			TB_ArithmaticBehavior ab = n->i_arith.arith_behavior;
			
			if (a->type != b->type) continue;
			if (a->type != TB_SIGNED_CONST && a->type != TB_UNSIGNED_CONST) continue;
			
			bool is_signed = (a->type == TB_SIGNED_CONST);
			uint64_t ai = a->uint.value;
			uint64_t bi = b->uint.value;
			
			uint64_t shift = 64 - (8 << (dt.type - TB_I8));
			uint64_t mask = (~0ull) >> shift;
			
			uint64_t result;
			switch (n->type) {
				case TB_AND: {
					result = ai & bi; 
					break;
				}
				case TB_XOR: {
					result = ai ^ bi; 
					break;
				}
				case TB_OR: {
					result = ai | bi; 
					break;
				}
				case TB_ADD: {
					if (tb_add_overflow(ai << shift, bi << shift, &result)) {
						result >>= shift;
						
						if (ab == TB_CAN_WRAP) result &= mask;
						else if (ab == TB_SATURATED_UNSIGNED) result = mask;
					} else {
						result = (result >> shift) & mask;
					}
					break;
				}
				case TB_SUB: {
					if (tb_sub_overflow(ai << shift, bi << shift, &result)) {
						result >>= shift;
						
						if (ab == TB_CAN_WRAP) result &= mask;
						else if (ab == TB_SATURATED_UNSIGNED) result = mask;
					} else {
						result = (result >> shift) & mask;
					}
					break;
				}
				case TB_MUL: {
					TB_MultiplyResult res = tb_mul64x128(ai, bi);
					
					if ((res.lo & ~mask) || res.hi) {
						result = res.lo & mask;
						
						if (ab == TB_SATURATED_SIGNED) {
							tb_todo();
							result = ((int64_t)res.lo) >= 0 ? INT64_MAX : INT64_MIN;
						} else if (ab == TB_SATURATED_UNSIGNED) {
							result = UINT64_MAX;
						} else if (ab == TB_SIGNED_TRAP_ON_WRAP) {
							tb_todo();
						} else if (ab == TB_UNSIGNED_TRAP_ON_WRAP) {
							tb_todo();
						}
					} else {
						result = res.lo & mask;
					}
					break;
				}
				case TB_UDIV:
				case TB_SDIV: {
					if (bi == 0) {
						n->type = TB_POISON;
						goto skip_normal_const;
					}
					
					if (is_signed) {
						result = ((int64_t)ai) / ((int64_t)bi);
						result &= mask;
					} else {
						result = (ai / bi) & mask;
					}
					break;
				}
				default: tb_todo();
			}
			
			OPTIMIZER_LOG(n - f->nodes.data, "constant folded operation");
			
			n->type = is_signed ? TB_SIGNED_CONST : TB_UNSIGNED_CONST;
			n->uint.value = result;
			
			skip_normal_const:
			changes++;
		} else if (n->type == TB_SIGN_EXT) {
			TB_Node* src = &f->nodes.data[n->unary.src];
			
			if (src->type == TB_SIGNED_CONST) {
				// NOTE(NeGate): We're using unsigned numbers because we're operating
				// on the raw bits but it's reinterpreted to signed integers.
				uint64_t shift = 64 - (8 << (dt.type - TB_I8));
				uint64_t mask = (~0ull) >> shift;
				uint16_t sign_bit = (src->uint.value >> (shift - 1)) & 1;
				
				uint64_t num = (src->uint.value & mask) | (sign_bit ? ~mask : 0);
				
				n->type = TB_SIGNED_CONST;
				n->uint.value = num;
				changes++;
			}
		} else if (n->type == TB_ZERO_EXT) {
			TB_Node* src = &f->nodes.data[n->unary.src];
			
			if (src->type == TB_UNSIGNED_CONST) {
				uint64_t shift = 64 - (8 << (dt.type - TB_I8));
				uint64_t mask = (~0ull) >> shift;
				uint64_t num = (src->uint.value & mask);
				
				n->type = TB_UNSIGNED_CONST;
				n->uint.value = num;
				changes++;
			}
		}
	}
	
	return changes;
}

