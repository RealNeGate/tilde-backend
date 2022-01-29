#include "../tb_internal.h"

static bool tb_address_may_alias(TB_Function* f, TB_Register r, TB_Register target) {
	switch (f->nodes.type[r]) {
		case TB_ARRAY_ACCESS:
		return tb_address_may_alias(f, f->nodes.payload[r].array_access.base, target);
		
		case TB_MEMBER_ACCESS:
		return tb_address_may_alias(f, f->nodes.payload[r].member_access.base, target);
		
		case TB_RESTRICT:
		return (target != r);
		
		default:
		return false;
	}
}

bool tb_opt_load_elim(TB_Function* f) {
	int changes = 0;
	
	loop(i, f->nodes.count) {
		if (f->nodes.type[i] == TB_RESTRICT) {
			TB_Register addr = f->nodes.payload[i].unary.src;
			
			// Find any duplicates
			loop_range(j, i + 1, f->nodes.count) {
				TB_RegType t = f->nodes.type[j];
				
				if (t == TB_RESTRICT &&
					f->nodes.payload[j].unary.src == addr) {
					f->nodes.type[j] = TB_PASS;
					f->nodes.payload[j].pass = i;
					changes++;
				} else if (TB_IS_NODE_TERMINATOR(t) || TB_IS_NODE_SIDE_EFFECT(t)) {
					// Can't read past side effects or terminators, don't
					// know what might happen
					if (t != TB_STORE || (t == TB_STORE && !tb_address_may_alias(f, f->nodes.payload[j].store.address, addr))) {
						break;
					}
				}
			}
		} else if (f->nodes.type[i] == TB_LOAD) {
			TB_DataType dt = f->nodes.dt[i];
			TB_Register addr = f->nodes.payload[i].load.address;
			uint32_t alignment = f->nodes.payload[i].load.alignment;
			
			// Find any duplicates
			loop_range(j, i + 1, f->nodes.count) {
				TB_RegType t = f->nodes.type[j];
				
				if (t == TB_LOAD &&
					f->nodes.payload[j].load.alignment == alignment &&
					f->nodes.payload[j].load.address == addr &&
					TB_DATA_TYPE_EQUALS(dt, f->nodes.dt[j])) {
					f->nodes.type[j] = TB_PASS;
					f->nodes.payload[j].pass = i;
					changes++;
				} else if (TB_IS_NODE_TERMINATOR(t) || TB_IS_NODE_SIDE_EFFECT(t)) {
					// Can't read past side effects or terminators, don't
					// know what might happen
					if (t != TB_STORE || (t == TB_STORE && !tb_address_may_alias(f, f->nodes.payload[j].store.address, addr))) {
						break;
					}
				}
			}
		} else if (f->nodes.type[i] == TB_MEMBER_ACCESS) {
			TB_Register base = f->nodes.payload[i].member_access.base;
			int32_t offset = f->nodes.payload[i].member_access.offset;
			
			// Find any duplicates
			loop_range(j, i + 1, f->nodes.count) {
				TB_RegType t = f->nodes.type[j];
				
				if (t == TB_MEMBER_ACCESS &&
					f->nodes.payload[j].member_access.base == base &&
					f->nodes.payload[j].member_access.offset == offset) {
					// if the load and store pair up, then elide the load
					// don't remove the store since it's unknown if it's
					// used elsewhere.
					f->nodes.type[j] = TB_PASS;
					f->nodes.payload[j].pass = i;
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
	loop(i, f->nodes.count) {
		if (f->nodes.type[i] == TB_STORE) {
			TB_DataType dt = f->nodes.dt[i];
			TB_Register value = f->nodes.payload[i].store.value;
			TB_Register addr = f->nodes.payload[i].store.address;
			uint32_t alignment = f->nodes.payload[i].store.alignment;
			
			// Find any duplicates
			loop_range(j, i + 1, f->nodes.count) {
				TB_RegType t = f->nodes.type[j];
				
				if (t == TB_LOAD &&
					f->nodes.payload[j].load.alignment == alignment &&
					f->nodes.payload[j].load.address == addr &&
					TB_DATA_TYPE_EQUALS(dt, f->nodes.dt[j])) {
					f->nodes.type[j] = TB_PASS;
					f->nodes.payload[j].pass = value;
					changes++;
				} else if (TB_IS_NODE_TERMINATOR(t) || TB_IS_NODE_SIDE_EFFECT(t)) {
					// Can't read past side effects or terminators, don't
					// know what might happen
					if (t != TB_STORE || (t == TB_STORE && !tb_address_may_alias(f, f->nodes.payload[j].store.address, addr))) {
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
	
	for (TB_Register i = 1; i < f->nodes.count; i++) {
		TB_DataType dt = f->nodes.dt[i];
		
		// It's technically legal to read this space even tho SIGN_EXTEND and ZERO_EXTEND
		// don't use it so long as we don't actually depend on it's results.
		TB_Register a = f->nodes.payload[i].i_arith.a;
		TB_Register b = f->nodes.payload[i].i_arith.b;
		TB_ArithmaticBehavior ab = f->nodes.payload[i].i_arith.arith_behavior;
		
		if (f->nodes.type[i] >= TB_AND &&
			f->nodes.type[i] <= TB_SDIV &&
			f->nodes.type[a] == f->nodes.type[b] &&
			(f->nodes.type[a] == TB_SIGNED_CONST || f->nodes.type[b] == TB_UNSIGNED_CONST)) {
			bool is_signed = f->nodes.type[a] == TB_SIGNED_CONST;
			uint64_t ai = f->nodes.payload[a].u_const;
			uint64_t bi = f->nodes.payload[b].u_const;
			
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
				tb_unreachable();
			}
			
			f->nodes.type[i] = is_signed ? TB_SIGNED_CONST : TB_UNSIGNED_CONST;
			f->nodes.payload[i].u_const = result;
			changes++;
		} else if (f->nodes.type[i] == TB_SIGN_EXT) {
			TB_Register src = f->nodes.payload[i].ext;
			
			if (f->nodes.type[src] == TB_SIGNED_CONST) {
				// NOTE(NeGate): We're using unsigned numbers because we're operating
				// on the raw bits but it's reinterpreted to signed integers.
				uint64_t shift = 64 - (8 << (dt.type - TB_I8));
				uint64_t mask = (~0ull) >> shift;
				uint16_t sign_bit = (f->nodes.payload[src].u_const >> (shift - 1)) & 1;
				
				uint64_t num = (f->nodes.payload[src].u_const & mask) | (sign_bit ? ~mask : 0);
				
				printf("%lld -> %llu\n", f->nodes.payload[src].s_const, num);
				
				f->nodes.type[i] = TB_SIGNED_CONST;
				f->nodes.payload[i].u_const = num;
				changes++;
			}
		} else if (f->nodes.type[i] == TB_ZERO_EXT) {
			TB_Register src = f->nodes.payload[i].ext;
			
			if (f->nodes.type[src] == TB_UNSIGNED_CONST) {
				uint64_t shift = 64 - (8 << (dt.type - TB_I8));
				uint64_t mask = (~0ull) >> shift;
				uint64_t num = (f->nodes.payload[src].u_const & mask);
				
				f->nodes.type[i] = TB_UNSIGNED_CONST;
				f->nodes.payload[i].u_const = num;
				changes++;
			}
		}
	}
	
	return changes;
}

