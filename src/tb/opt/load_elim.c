#include "../tb_internal.h"

#if 0
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
#endif

bool tb_opt_load_elim(TB_Function* f) {
    int changes = 0;

#if 0
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
		} else if (n->type == TB_ARRAY_ACCESS) {
			TB_Reg base = n->array_access.base;
			TB_Reg index = n->array_access.index;
			int32_t stride = n->array_access.stride;
			
			// Find any duplicates
			for (TB_Node* other = &f->nodes.data[n->next]; other != &f->nodes.data[0]; other = &f->nodes.data[other->next]) {
				TB_NodeTypeEnum t = other->type;
				
				if (t == TB_ARRAY_ACCESS &&
					other->array_access.base == base &&
					other->array_access.index == index &&
					other->array_access.stride == stride) {
					other->type = TB_PASS;
					other->pass.value = i;
					changes++;
				} else if (TB_IS_NODE_TERMINATOR(t)) {
					// Can't read past terminators, don't
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
		//TB_Reg i = (n - f->nodes.data);
		
		if (n->type == TB_STORE) {
			TB_DataType dt = n->dt;
			TB_Reg value = n->store.value;
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
					other->pass.value = value;
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
#endif

    return changes;
}
