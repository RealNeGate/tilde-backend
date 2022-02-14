#include "../tb_internal.h"

static bool tb_address_may_alias(TB_Function* f, TB_Reg r, TB_Reg target) {
	switch (f->nodes.data[r].type) {
		case TB_ARRAY_ACCESS:
		return tb_address_may_alias(f, f->nodes.data[r].array_access.base, target);
		
		case TB_MEMBER_ACCESS:
		return tb_address_may_alias(f, f->nodes.data[r].member_access.base, target);
		
		case TB_RESTRICT:
		return (target != r);
		
		default:
		return false;
	}
}

bool tb_opt_load_elim(TB_Function* f) {
	int changes = 0;
	
	TB_FOR_EACH_NODE(n, f) {
		TB_Reg i = (n - f->nodes.data);
		
		if (n->type == TB_RESTRICT) {
			TB_Reg addr = n->unary.src;
			
			// Find any duplicates
			for (TB_Node* other = &f->nodes.data[n->next]; other != &f->nodes.data[0]; other = &f->nodes.data[other->next]) {
				TB_NodeTypeEnum t = other->type;
				
				if (t == TB_RESTRICT && other->unary.src == addr) {
					other->type = TB_PASS;
					other->pass.value = i;
					changes++;
				} else if (TB_IS_NODE_TERMINATOR(t) || TB_IS_NODE_SIDE_EFFECT(t)) {
					// Can't read past side effects or terminators, don't
					// know what might happen
					if (t != TB_STORE || (t == TB_STORE && !tb_address_may_alias(f, other->store.address, addr))) {
						break;
					}
				}
			}
		} else if (n->type == TB_LOAD) {
			TB_DataType dt = n->dt;
			TB_Reg addr = n->load.address;
			uint32_t alignment = n->load.alignment;
			
			// Find any duplicates
			for (TB_Node* other = &f->nodes.data[n->next]; other != &f->nodes.data[0]; other = &f->nodes.data[other->next]) {
				TB_NodeTypeEnum t = other->type;
				
				if (t == TB_LOAD &&
					other->load.alignment == alignment &&
					other->load.address == addr &&
					TB_DATA_TYPE_EQUALS(dt, other->dt)) {
					other->type = TB_PASS;
					other->pass.value = i;
					changes++;
				} else if (TB_IS_NODE_TERMINATOR(t) || TB_IS_NODE_SIDE_EFFECT(t)) {
					// Can't read past side effects or terminators, don't
					// know what might happen
					if (t != TB_STORE || (t == TB_STORE && !tb_address_may_alias(f, n->store.address, addr))) {
						break;
					}
				}
			}
		} else if (n->type == TB_MEMBER_ACCESS) {
			TB_Reg base = n->member_access.base;
			int32_t offset = n->member_access.offset;
			
			// Find any duplicates
			for (TB_Node* other = &f->nodes.data[n->next]; other != &f->nodes.data[0]; other = &f->nodes.data[other->next]) {
				TB_NodeTypeEnum t = other->type;
				
				if (t == TB_MEMBER_ACCESS &&
					other->member_access.base == base &&
					other->member_access.offset == offset) {
					// if the load and store pair up, then elide the load
					// don't remove the store since it's unknown if it's
					// used elsewhere.
					other->type = TB_PASS;
					other->pass.value = i;
					changes++;
				} else if (TB_IS_NODE_TERMINATOR(t) || TB_IS_NODE_SIDE_EFFECT(t)) {
					// Can't read past side effects or terminators, don't
					// know what might happen
					break;
				}
			}
		}
	}
	
	// STORE *p, _1 #
	// ...          # anything but a possible store to addr, terminator,
	// _2 = LOAD *p # or side effect then _2 = _1
	TB_FOR_EACH_NODE(n, f) {
		TB_Reg i = (n - f->nodes.data);
		
		if (n->type == TB_STORE) {
			TB_DataType dt = n->dt;
			//TB_Reg value = n->store.value;
			TB_Reg addr = n->store.address;
			uint32_t alignment = n->store.alignment;
			
			// Find any duplicates
			for (TB_Node* other = &f->nodes.data[n->next]; other != &f->nodes.data[0]; other = &f->nodes.data[other->next]) {
				TB_NodeTypeEnum t = other->type;
				
				if (t == TB_LOAD &&
					other->load.alignment == alignment &&
					other->load.address == addr &&
					TB_DATA_TYPE_EQUALS(dt, other->dt)) {
					other->type = TB_PASS;
					other->pass.value = i;
					changes++;
				} else if (TB_IS_NODE_TERMINATOR(t) || TB_IS_NODE_SIDE_EFFECT(t)) {
					// Can't read past side effects or terminators, don't
					// know what might happen
					if (t != TB_STORE || (t == TB_STORE && !tb_address_may_alias(f, other->store.address, addr))) {
						break;
					}
				}
			}
		}
	}
	
	return changes;
}

bool tb_opt_fold(TB_Function* f) {
	int changes = 0;
	
	TB_FOR_EACH_NODE(n, f) {
		TB_DataType dt = n->dt;
		
		if (n->type >= TB_AND && n->type <= TB_SDIV) {
			// It's technically legal to read this space even tho SIGN_EXTEND and ZERO_EXTEND
			// don't use it so long as we don't actually depend on it's results.
			TB_Node* a = &f->nodes.data[n->i_arith.a];
			TB_Node* b = &f->nodes.data[n->i_arith.b];
			TB_ArithmaticBehavior ab = n->i_arith.arith_behavior;
			
			if (a->type != b->type) continue;
			if (a->type == TB_SIGNED_CONST || b->type == TB_UNSIGNED_CONST) continue;
			
			bool is_signed = (a->type == TB_SIGNED_CONST);
			uint64_t ai = a->uint.value;
			uint64_t bi = b->uint.value;
			
			uint64_t result;
			switch (n->type) {
				case TB_AND: result = ai & bi; break;
				case TB_XOR: result = ai ^ bi; break;
				case TB_OR: result = ai | bi; break;
				case TB_ADD: result = tb_fold_add(ab, dt, ai, bi); break;
				case TB_SUB: result = tb_fold_sub(ab, dt, ai, bi); break;
				case TB_MUL: result = tb_fold_mul(ab, dt, ai, bi); break;
				case TB_UDIV: result = tb_fold_div(dt, ai, bi); break;
				case TB_SDIV: result = tb_fold_div(dt, ai, bi); break;
				default: tb_todo();
			}
			
			n->type = is_signed ? TB_SIGNED_CONST : TB_UNSIGNED_CONST;
			n->uint.value = result;
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

