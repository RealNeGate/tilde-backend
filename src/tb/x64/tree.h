// This submodule just evaluates expression trees
static TreeNodeIndex push_leaf(Ctx* restrict ctx, TB_Register r) {
	assert(ctx->tree_len + 1 < ctx->tree_cap && "Cannot evaluate tree that big");
	TreeNodeIndex i = ctx->tree_len++;
	ctx->tree[i] = (TreeNode){ .reg = r };
	return i;
}

static TreeNodeIndex push_unary(Ctx* restrict ctx, TB_Register r, TreeNodeIndex a) {
	TreeNodeIndex i = ctx->tree_len++;
	ctx->tree[i] = (TreeNode){ .reg = r, .a = a };
	return i;
}

static TreeNodeIndex push_binary(Ctx* restrict ctx, TB_Register r, TreeNodeIndex a, TreeNodeIndex b) {
	TreeNodeIndex i = ctx->tree_len++;
	ctx->tree[i] = (TreeNode){ .reg = r, .a = a, .b = b };
	return i;
}

static TreeNodeIndex gen_tree(Ctx* restrict ctx, TB_Function* f, TB_Register r) {
	TB_RegTypeEnum reg_type = f->nodes.type[r];
	TB_RegPayload* restrict p = &f->nodes.payload[r];
	
	switch (reg_type) {
		case TB_PARAM:
		case TB_LOCAL:
		case TB_PARAM_ADDR:
		case TB_SIGNED_CONST:
		case TB_UNSIGNED_CONST:
		case TB_FLOAT_CONST:
		case TB_STRING_CONST:
		case TB_GLOBAL_ADDRESS:
		case TB_FUNC_ADDRESS:
		case TB_EFUNC_ADDRESS:
		case TB_PHI2:
		case TB_CALL:
		case TB_ECALL:
		case TB_VCALL:
		return push_leaf(ctx, r);
		
		case TB_LOAD:
		return push_unary(ctx, r, gen_tree(ctx, f, p->load.address));
		
		case TB_ARRAY_ACCESS:
		return push_binary(ctx, r, 
						   gen_tree(ctx, f, p->array_access.base),
						   gen_tree(ctx, f, p->array_access.index));
		
		case TB_MEMBER_ACCESS:
		return push_unary(ctx, r, gen_tree(ctx, f, p->member_access.base));
		
		case TB_TRUNCATE:
		case TB_ZERO_EXT:
		case TB_SIGN_EXT:
		case TB_FLOAT_EXT:
		return push_unary(ctx, r, gen_tree(ctx, f, p->ext));
		
		case TB_INT2PTR:
		case TB_PTR2INT:
		case TB_FLOAT2INT:
		case TB_INT2FLOAT:
		return push_unary(ctx, r, gen_tree(ctx, f, p->cvt.src));
		
		case TB_NEG:
		case TB_NOT:
		return push_unary(ctx, r, gen_tree(ctx, f, p->unary));
		
		case TB_FADD:
		case TB_FSUB:
		case TB_FMUL:
		case TB_FDIV:
		return push_binary(ctx, r, 
						   gen_tree(ctx, f, p->f_arith.a),
						   gen_tree(ctx, f, p->f_arith.b));
		
		case TB_AND:
		case TB_OR:
		case TB_XOR:
		case TB_ADD:
		case TB_SUB:
		case TB_MUL:
		case TB_SHL:
		case TB_SHR:
		case TB_SAR:
		case TB_UDIV:
		case TB_SDIV:
		case TB_UMOD:
		case TB_SMOD:
		return push_binary(ctx, r, 
						   gen_tree(ctx, f, p->i_arith.a),
						   gen_tree(ctx, f, p->i_arith.b));
		
		case TB_CMP_EQ:
		case TB_CMP_NE:
		case TB_CMP_SLT:
		case TB_CMP_SLE:
		case TB_CMP_ULT:
		case TB_CMP_ULE:
		return push_binary(ctx, r, 
						   gen_tree(ctx, f, p->cmp.a),
						   gen_tree(ctx, f, p->cmp.b));
		
		default: 
		tb_unreachable();
	}
}

static void print_tree(Ctx* restrict ctx, TB_Function* f, TreeNodeIndex n, int depth) {
	for (int i = 0; i < depth; i++) printf("  ");
	
	printf("node %d, r%u:\n", n, ctx->tree[n].reg);
	if (ctx->tree[n].a) print_tree(ctx, f, ctx->tree[n].a, depth+1);
	if (ctx->tree[n].b) print_tree(ctx, f, ctx->tree[n].b, depth+1);
}

static Val spill_stack_slot(Ctx* restrict ctx, TB_Function* f, StackSlot* restrict slot) {
	TB_DataType dt = f->nodes.dt[slot->reg];
	
	Val dst = val_stack(dt, slot->pos);
	if (slot->gpr != GPR_NONE) {
		Val src = val_gpr(TB_I64, slot->gpr);
		
		// don't keep reference to GPR, we'll be using the
		// memory version only
		ctx->gpr_allocator &= ~(1u << slot->gpr);
		
		// save the shadow space into the stack
		inst2(ctx, MOV, &dst, &src, dt.type);
		
		slot->gpr = GPR_NONE;
	} else if (slot->xmm != XMM_NONE) {
		// the parameters map to XMM0-XMM3
		Val src = val_xmm(dt, slot->xmm);
		
		// don't keep reference to XMM, we'll be using the memory
		// version only
		ctx->xmm_allocator &= ~(1u << slot->xmm);
		
		// save the shadow space into the stack
		uint8_t flags = 0;
		flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
		flags |= (dt.width) ? INST2FP_PACKED : 0;
		
		inst2sse(ctx, FP_MOV, &dst, &src, flags);
		slot->xmm = XMM_NONE;
	}
	
	return dst;
}

static Val get_stack_slot(Ctx* restrict ctx, TB_Function* f, TB_Register r, TB_DataType dt) {
	loop(i, arrlen(ctx->locals)) if (ctx->locals[i].reg == r) {
		if (ctx->locals[i].xmm != XMM_NONE) return val_xmm(dt, ctx->locals[i].xmm);
		else if (ctx->locals[i].gpr != GPR_NONE) return val_gpr(dt.type, ctx->locals[i].gpr);
		
		return val_stack(dt, ctx->locals[i].pos);
	}
	
	abort();
}

static StackSlot* try_get_stack_slot(Ctx* restrict ctx, TB_Function* f, TB_Register r, TB_DataType dt) {
	loop(i, arrlen(ctx->locals)) if (ctx->locals[i].reg == r) {
		return &ctx->locals[i];
	}
	
	return NULL;
}

static Val alloc_gpr(Ctx* restrict ctx, TB_Function* f, int dt_type) {
	int search = __builtin_ffs(~ctx->gpr_allocator);
	assert(search && "TODO: Implement spilling");
	
	GPR gpr = (GPR)(search-1);
	ctx->gpr_allocator |= (1u << gpr);
	
	return (Val) {
		.type = VAL_GPR,
		.is_owned = true,
		.dt.width = 0,
		.dt.type = dt_type,
		.gpr = gpr
	};
}

static void free_val(Ctx* restrict ctx, TB_Function* f, Val v) {
	if (v.is_owned) return;
	
	if (v.type == VAL_GPR) {
		ctx->gpr_allocator &= ~(1u << v.gpr);
	} else if (v.type == VAL_XMM) {
		ctx->xmm_allocator &= ~(1u << v.xmm);
	} else if (v.type == VAL_MEM) {
		if (v.mem.base != RSP && v.mem.base != RBP) {
			ctx->gpr_allocator &= ~(1u << v.mem.base);
		}
		
		if (v.mem.index != GPR_NONE && v.mem.index != RSP && v.mem.index != RBP) {
			ctx->gpr_allocator &= ~(1u << v.mem.index);
		}
	}
}

// Each of these eval_node functions will set a value in ctx->tree[n].val
// we don't directly return it because it can change as spills happen.
static void eval_node(Ctx* restrict ctx, TB_Function* f, TreeNodeIndex n, TreeNodeIndex next) {
	assert(n != 0 && "Invalid node");
	TreeNode* restrict node = &ctx->tree[n];
	TB_Register r = node->reg;
	
	TB_RegTypeEnum reg_type = f->nodes.type[r];
	TB_DataType dt = f->nodes.dt[r];
	TB_RegPayload* restrict p = &f->nodes.payload[r];
	
	if (TB_UNLIKELY(r < ctx->last_fence || r >= ctx->root_reg)) {
		// these are always thought of as out-of-scope in a weird way and have their
		// own special behavior described in the normal path.
		if (reg_type != TB_UNSIGNED_CONST && 
			reg_type != TB_SIGNED_CONST && 
			reg_type != TB_FLOAT_CONST && 
			reg_type != TB_PARAM && 
			reg_type != TB_PARAM_ADDR && 
			reg_type != TB_PHI2) {
			StackSlot* slot = try_get_stack_slot(ctx, f, r, dt);
			
			if (slot) {
				if (slot->gpr != GPR_NONE) {
					node->val = val_gpr(dt.type, slot->gpr);
				} else if (slot->xmm != XMM_NONE) {
					node->val = val_xmm(dt, slot->xmm);
				} else {
					node->val = val_stack(dt, slot->pos);
				}
				return;
			}
		}
	}
	
	switch (reg_type) {
		case TB_SIGNED_CONST:
		case TB_UNSIGNED_CONST: {
			int32_t imm32 = (int32_t)p->s_const;
			if (p->s_const == imm32) {
				node->val = val_imm(dt, imm32);
				break;
			}
			
			// explicit mov
			Val dst = alloc_gpr(ctx, f, dt.type);
			
			// mov reg64, imm64
			emit(rex(true, 0x0, dst.gpr, 0));
			emit(0xB8 + (dst.gpr & 0b111));
			emit8(p->u_const);
			
			node->val = dst;
			break;
		}
		
		case TB_PARAM: {
			int id = f->nodes.payload[r].param.id;
			assert(TB_FIRST_PARAMETER_REG + id == r);
			
			StackSlot* slot = &ctx->locals[id];
			if (slot->gpr != GPR_NONE) node->val = val_gpr(dt.type, slot->gpr);
			else if (slot->xmm != XMM_NONE) node->val = val_xmm(dt, slot->xmm);
			else node->val = val_stack(dt, slot->pos);
			break;
		}
		case TB_PARAM_ADDR: {
			TB_Register param = f->nodes.payload[r].param_addr.param;
			int id = f->nodes.payload[param].param.id;
			assert(TB_FIRST_PARAMETER_REG + id == param);
			
			node->val = spill_stack_slot(ctx, f, &ctx->locals[id]);
			break;
		}
		
		case TB_CALL:
		case TB_ECALL:
		case TB_VCALL:
		case TB_LOCAL: {
			node->val = get_stack_slot(ctx, f, r, dt);
			break;
		}
		
		case TB_LOAD: {
			eval_node(ctx, f, node->a, n);
			Val v = ctx->tree[node->a].val;
			
			if (is_value_mem(&v)) {
				if (v.mem.is_rvalue) {
					// deref
					assert(TB_IS_INTEGER_TYPE(v.dt.type) && "TODO: support more load types");
					
					Val new_v = alloc_gpr(ctx, f, TB_PTR);
					inst2(ctx, MOV, &new_v, &v, TB_PTR);
					
					v = val_base_disp(v.dt, new_v.gpr, 0);
					v.is_owned = true;
				}
				
				v.mem.is_rvalue = true;
			}
			
			node->val = v;
			break;
		}
		case TB_MEMBER_ACCESS: {
			eval_node(ctx, f, node->a, n);
			Val v = val_addressof(ctx, f, ctx->tree[node->a].val);
			
			v.mem.disp += p->member_access.offset;
			node->val = v;
			break;
		}
		
		case TB_NEG:
		case TB_NOT: {
			int op = (reg_type == TB_NEG ? NEG : NOT);
			
			eval_node(ctx, f, node->a, n);
			Val src = val_rvalue(ctx, f, ctx->tree[node->a].val, ctx->tree[node->a].reg);
			
			bool recycled = src.is_owned && src.type != VAL_MEM;
			if (recycled) {
				inst1(ctx, op, &src);
				
				node->val = src;
				break;
			} else {
				Val dst = alloc_gpr(ctx, f, dt.type);
				
				inst2(ctx, MOV, &dst, &src, dt.type);
				inst1(ctx, op, &dst);
				
				free_val(ctx, f, src);
				node->val = dst;
				break;
			}
		}
		
		// Integer binary operations
		case TB_AND:
		case TB_OR:
		case TB_XOR:
		case TB_ADD:
		case TB_SUB:
		case TB_MUL: {
			assert(dt.width == 0 && "TODO: Implement vector integer arithmatic");
			
			// NOTE(NeGate): It is kinda weird to put my struct here but it's only
			// relevant here so :p
			struct IselInfo {
				int inst;
				
				bool communitive;
				bool has_immediates;
				bool has_memory_dst;
				bool has_memory_src;
			};
			
			const static struct IselInfo tbl[] = {
				// type               inst  commut  has_imm  mem_dst  mem_src   
				[TB_AND - TB_AND] = { AND,  true,   true,    true,    true },
				[TB_OR  - TB_AND] = { OR,   true,   true,    true,    true },
				[TB_XOR - TB_AND] = { XOR,  true,   true,    true,    true },
				[TB_ADD - TB_AND] = { ADD,  true,   true,    true,    true },
				[TB_SUB - TB_AND] = { SUB,  false,  true,    true,    true },
				[TB_MUL - TB_AND] = { IMUL, true,   false,   false,   true }
			};
			const struct IselInfo* info = &tbl[reg_type - TB_AND];
			
			eval_node(ctx, f, node->a, n);
			eval_node(ctx, f, node->b, n);
			
			Val a = val_rvalue(ctx, f, ctx->tree[node->a].val, ctx->tree[node->a].reg);
			Val b = val_rvalue(ctx, f, ctx->tree[node->b].val, ctx->tree[node->b].reg);
			
			Val dst;
			bool recycled = a.is_owned && !(a.type == VAL_MEM && !info->has_memory_dst);
			if (recycled) {
				dst = a;
			} else {
				dst = alloc_gpr(ctx, f, dt.type);
				inst2(ctx, MOV, &dst, &a, dt.type);
			}
			
			// we can't do a OP mem, mem
			// and imul doesn't support a simple OP r/m, imm mode
			// we'll need a temporary in those cases
			bool is_mem_dst = is_value_mem(&dst);
			if ((is_mem_dst && is_value_mem(&b)) ||
				(b.type == VAL_IMM && !info->has_immediates) ||
				(is_mem_dst && !info->has_memory_dst)) {
				Val tmp = alloc_gpr(ctx, f, dt.type);
				
				inst2(ctx, MOV, &tmp, &b, dt.type);
				inst2(ctx, info->inst, &dst, &tmp, dt.type);
				
				free_val(ctx, f, tmp);
			} else {
				inst2(ctx, info->inst, &dst, &b, dt.type);
			}
			
			free_val(ctx, f, a);
			free_val(ctx, f, b);
			
			node->val = dst;
			break;
		}
		
		case TB_UDIV:
		case TB_SDIV:
		case TB_UMOD:
		case TB_SMOD: {
			assert(dt.width == 0 && "TODO: Implement vector integer division and modulo");
			
			// NOTE(NeGate): This code resolves the two inputs in two separate
			// places because it generally improves codegen.
			bool is_signed = (reg_type == TB_SDIV || reg_type == TB_SMOD);
			bool is_div = (reg_type == TB_UDIV || reg_type == TB_SDIV);
			
			int rax_savepoint = 0;
			if (!evict_gpr(ctx, f, RAX)) {
				// weird but it works, just give it a basic saveslot
				ctx->stack_usage = align_up(ctx->stack_usage + 8, 8);
				rax_savepoint = -ctx->stack_usage;
				
				Val sav = val_stack(TB_TYPE_I64, rax_savepoint);
				Val reg = val_gpr(TB_I64, RAX);
				inst2(ctx, MOV, &sav, &reg, dt.type);
			}
			
			// we'll just be using CDQ if it's signed
			// and zeroing RDX it if it's unsigned. 
			int rdx_savepoint = 0;
			if (!evict_gpr(ctx, f, RDX)) {
				// weird but it works, just give it a basic saveslot
				ctx->stack_usage = align_up(ctx->stack_usage + 8, 8);
				rdx_savepoint = -ctx->stack_usage;
				
				Val sav = val_stack(TB_TYPE_I64, rdx_savepoint);
				Val reg = val_gpr(TB_I64, RDX);
				inst2(ctx, MOV, &sav, &reg, dt.type);
			}
			
			eval_node(ctx, f, node->a, n);
			eval_node(ctx, f, node->b, n);
			
			Val a = val_rvalue(ctx, f, ctx->tree[node->a].val, ctx->tree[node->a].reg);
			Val b = val_rvalue(ctx, f, ctx->tree[node->b].val, ctx->tree[node->b].reg);
			
			// needs to mov the a value into rdx:rax
			Val rax = val_gpr(dt.type, RAX);
			if (!is_value_gpr(&a, RAX)) {
				inst2(ctx, MOV, &rax, &a, dt.type);
			}
			
			if (is_signed) {
				emit(0x99);
			} else {
				emit(0x31);
				emit(mod_rx_rm(MOD_DIRECT, RDX, RDX));
			}
			
			if (b.type == VAL_IMM) {
				Val tmp = alloc_gpr(ctx, f, dt.type);
				inst2(ctx, MOV, &tmp, &b, dt.type);
				inst1(ctx, IDIV, &tmp);
				free_val(ctx, f, tmp);
			} else {
				inst1(ctx, IDIV, &b);
			}
			
			free_val(ctx, f, b);
			free_val(ctx, f, a);
			
			if (rax_savepoint) {
				Val sav = val_stack(TB_TYPE_I64, rax_savepoint);
				Val reg = val_gpr(TB_I64, RAX);
				inst2(ctx, MOV, &reg, &sav, dt.type);
				
				ctx->gpr_allocator |= (1u << RAX);
			}
			
			if (rdx_savepoint) {
				Val sav = val_stack(TB_TYPE_I64, rdx_savepoint);
				Val reg = val_gpr(TB_I64, RAX);
				inst2(ctx, MOV, &reg, &sav, dt.type);
				
				ctx->gpr_allocator |= (1u << RDX);
			}
			
			// the return value is in RAX for division
			// and RDX for modulo
			node->val = val_gpr(dt.type, is_div ? RAX : RDX);
			node->val.is_owned = true;
			break;
		}
		
		case TB_CMP_EQ:
		case TB_CMP_NE:
		case TB_CMP_SLT:
		case TB_CMP_SLE:
		case TB_CMP_ULT:
		case TB_CMP_ULE: {
			TB_DataType cmp_dt = p->cmp.dt;
			
			eval_node(ctx, f, node->a, n);
			eval_node(ctx, f, node->b, n);
			
			Val a = val_rvalue(ctx, f, ctx->tree[node->a].val, ctx->tree[node->a].reg);
			Val b = val_rvalue(ctx, f, ctx->tree[node->b].val, ctx->tree[node->b].reg);
			
			Val dst;
			bool convert_to_reg = (f->nodes.type[ctx->next_reg] != TB_IF);
			if (convert_to_reg) {
				dst = alloc_gpr(ctx, f, dt.type);
				
				// xor temp, temp
				if (dst.gpr >= 8) emit(rex(true, dst.gpr, dst.gpr, 0));
				emit(0x31);
				emit(mod_rx_rm(MOD_DIRECT, dst.gpr, dst.gpr));
			}
			
			bool invert = false;
			if (is_value_mem(&a) && is_value_mem(&b)) {
				Val tmp = alloc_gpr(ctx, f, dt.type);
				
				inst2(ctx, MOV, &tmp, &a, dt.type);
				inst2(ctx, CMP, &tmp, &b, dt.type);
				
				free_val(ctx, f, tmp);
			} else {
				invert = (a.type == VAL_IMM);
				
				if (invert) inst2(ctx, CMP, &b, &a, cmp_dt.type);
				else inst2(ctx, CMP, &a, &b, cmp_dt.type);
			}
			
			Cond cc;
			switch (reg_type) {
				case TB_CMP_EQ: cc = E; break;
				case TB_CMP_NE: cc = NE; break;
				case TB_CMP_SLT: cc = L; break;
				case TB_CMP_SLE: cc = LE; break;
				case TB_CMP_ULT: cc = B; break;
				case TB_CMP_ULE: cc = BE; break;
				default: tb_todo();
			}
			
			cc ^= invert;
			
			free_val(ctx, f, b);
			free_val(ctx, f, a);
			
			if (convert_to_reg) {
				// setcc v
				if (dst.gpr >= 8) emit(rex(true, dst.gpr, dst.gpr, 0));
				emit(0x0F);
				emit(0x90 + cc);
				emit(mod_rx_rm(MOD_DIRECT, dst.gpr, dst.gpr));
				
				node->val = dst;
			} else {
				node->val = val_flags(cc);
			}
			break;
		}
		
		case TB_ZERO_EXT: {
			eval_node(ctx, f, node->a, n);
			Val src = val_rvalue(ctx, f, ctx->tree[node->a].val, ctx->tree[node->a].reg);
			if (src.type == VAL_IMM) {
				src.dt = dt;
				node->val = src;
				break;
			}
			
			Val dst;
			bool recycled = src.is_owned && src.type != VAL_MEM;
			if (recycled) {
				dst = src;
			} else {
				dst = alloc_gpr(ctx, f, dt.type);
				inst2(ctx, MOV, &dst, &src, src.dt.type);
			}
			
			if (src.dt.type == TB_I16) {
				inst2(ctx, MOVZXW, &dst, &src, dt.type);
			} else if (src.dt.type == TB_I8 || src.dt.type == TB_BOOL) {
				inst2(ctx, MOVZXB, &dst, &src, dt.type);
			}
			
			free_val(ctx, f, src);
			node->val = dst;
			break;
		}
		
		case TB_TRUNCATE: {
			if (TB_IS_FLOAT_TYPE(dt.type)) {
				assert(0 && "TODO: implement float truncate");
				/*Val src = use_as_rvalue(ctx, f, p->ext);
				Val v = def_new_xmm(ctx, f, r, dt);
				
				uint8_t flags = 0;
				flags |= (src.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (src.dt.width) ? INST2FP_PACKED : 0;
				inst2sse(ctx, FP_CVT, &v, &src, flags);
				
				free_val(ctx, f, src);
				node->val = v;
				break;*/
			} else {
				eval_node(ctx, f, node->a, n);
				Val src = val_rvalue(ctx, f, ctx->tree[node->a].val, ctx->tree[node->a].reg);
				if (src.type == VAL_IMM) {
					src.dt = dt;
					
					node->val = src;
					break;
				}
				
				Val dst;
				bool recycled = src.is_owned && src.type != VAL_MEM;
				if (recycled) {
					dst = src;
				} else {
					dst = alloc_gpr(ctx, f, dt.type);
					inst2(ctx, MOV, &dst, &src, src.dt.type);
				}
				
				if (dt.type == TB_I16) {
					inst2(ctx, MOVZXW, &dst, &src, dt.type);
				} else if (dt.type == TB_I8 || dt.type == TB_BOOL) {
					inst2(ctx, MOVZXB, &dst, &src, dt.type);
				}
				
				free_val(ctx, f, src);
				node->val = dst;
				break;
			}
		}
		
		case TB_PHI2: {
			PhiValue* phi = find_phi(ctx, r);
			assert(phi && "PHI node not initialized but used");
			
			node->val = phi->value;
			break;
		}
		
		default: tb_unreachable();
	}
}

static Val eval(Ctx* restrict ctx, TB_Function* f, TB_Register root_reg, TB_Register next) {
	// we reserve slot 0
	ctx->tree_len = 1;
	ctx->root_reg = root_reg;
	ctx->next_reg = next;
	
	// Generate tree from DAG
	gen_tree(ctx, f, root_reg);
	
	// Walk tree to generate machine code
	assert(ctx->tree_len > 1 && "We should have pushed some nodes");
	
	TreeNodeIndex root_node = ctx->tree_len - 1;
	assert(ctx->tree[root_node].reg == root_reg);
	ctx->tree[root_node].val.type = 0;
	
	eval_node(ctx, f, root_node, 0);
	
	Val v = ctx->tree[root_node].val;
	assert(v.type && "eval failed to initialize the resulting node.");
	
	if (v.type == VAL_GPR) {
		StackSlot slot = { 
			.reg = root_reg, .pos = 0, .gpr = v.gpr, .xmm = XMM_NONE
		};
		arrput(ctx->locals, slot);
	} else if (v.type == VAL_XMM) {
		StackSlot slot = { 
			.reg = root_reg, .pos = 0, .gpr = GPR_NONE, .xmm = v.xmm
		};
		arrput(ctx->locals, slot);
	}
	
	return v;
}

static Val val_addressof(Ctx* ctx, TB_Function* f, Val v) {
	if (v.dt.type == TB_BOOL) v.dt.type = TB_I8;
	
	if (is_value_mem(&v)) {
		if (v.mem.is_rvalue) {
			Val tmp = alloc_gpr(ctx, f, TB_PTR);
			inst2(ctx, MOV, &tmp, &v, TB_PTR);
			
			v = val_base_disp(TB_TYPE_PTR, tmp.gpr, 0);
			v.is_owned = true;
			return v;
		}
	} else if (v.type == VAL_GPR) {
		return val_base_disp(TB_TYPE_PTR, v.gpr, 0);
	} else if (v.type == VAL_IMM) {
		Val tmp = alloc_gpr(ctx, f, TB_PTR);
		
		// mov reg64, imm64
		emit(rex(true, 0x0, tmp.gpr, 0));
		emit(0xB8 + (tmp.gpr & 0b111));
		emit8(v.imm);
		
		v = val_base_disp(TB_TYPE_PTR, tmp.gpr, 0);
		v.is_owned = true;
		return v;
	} else tb_todo();
	
	return v;
}

static Val val_rvalue(Ctx* ctx, TB_Function* f, Val v, TB_Register r) {
	if (v.dt.type == TB_BOOL) v.dt.type = TB_I8;
	
	if (is_value_mem(&v)) {
		if (is_address_node(f->nodes.type[r])) {
			assert(!v.mem.is_rvalue);
			
			Val new_v = alloc_gpr(ctx, f, v.dt.type);
			inst2(ctx, LEA, &new_v, &v, v.dt.type);
			return new_v;
		} else if (!v.mem.is_rvalue) {
			if (v.dt.width || TB_IS_FLOAT_TYPE(v.dt.type)) {
				assert(0 && "TODO: Implement lvalue->rvalue conversion for XMM-based values");
				/*Val new_v = def_new_xmm(ctx, f, r, v.dt);
				
				uint8_t flags = 0;
				flags |= (v.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (v.dt.width) ? INST2FP_PACKED : 0;
				
				inst2sse(ctx, FP_MOV, &new_v, &v, flags);
				return new_v;*/
			} else {
				Val tmp = alloc_gpr(ctx, f, v.dt.type);
				inst2(ctx, MOV, &tmp, &v, v.dt.type);
				return tmp;
			}
		}
	} else if (v.type == VAL_FLAGS) {
		assert(0 && "No fucking way bro");
	}
	
	return v;
}
