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
		case TB_X86INTRIN_SQRT:
		case TB_X86INTRIN_RSQRT:
		case TB_RESTRICT:
		return push_unary(ctx, r, gen_tree(ctx, f, p->unary.src));
		
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
		case TB_CMP_FLT:
		case TB_CMP_FLE:
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
	
	if (slot->pos == 0) {
		int size = get_data_type_size(dt);
		ctx->stack_usage = align_up(ctx->stack_usage + size, size);
		
		slot->pos = -ctx->stack_usage;
	}
	
	Val dst = val_stack(dt, slot->pos);
	if (slot->gpr != GPR_NONE) {
		// desugar booleans
		if (dt.type == TB_BOOL) dt.type = TB_I8;
		
		Val src = val_gpr(TB_I64, slot->gpr);
		
		// don't keep reference to GPR, we'll be using the
		// memory version only
		ctx->gpr_allocator &= ~(1u << slot->gpr);
		
		inst2(ctx, MOV, &dst, &src, dt.type);
		
		slot->gpr = GPR_NONE;
	} else if (slot->xmm != XMM_NONE) {
		Val src = val_xmm(dt, slot->xmm);
		
		ctx->xmm_allocator &= ~(1u << slot->xmm);
		
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
	
	tb_function_print(f, tb_default_print_callback, stdout);
	abort();
}

static StackSlot* try_get_stack_slot(Ctx* restrict ctx, TB_Function* f, TB_Register r, TB_DataType dt) {
	loop(i, arrlen(ctx->locals)) if (ctx->locals[i].reg == r) {
		return &ctx->locals[i];
	}
	
	return NULL;
}

static Val alloc_gpr(Ctx* restrict ctx, TB_Function* f, int dt_type, TreeNodeIndex node) {
	if (ctx->gpr_allocator == 0xFFFF) {
		// Start by trying to spill any locals until we've freed enough
		loop(i, ctx->phi_count) {
			PhiValue* restrict phi = &ctx->phis[i];
			if (phi->value.type == VAL_GPR) {
				TB_DataType dt = f->nodes.dt[phi->reg];
				if (dt.type == TB_BOOL) dt.type = TB_I8;
				
				if (phi->spill == 0) {
					int size = get_data_type_size(dt);
					ctx->stack_usage = align_up(ctx->stack_usage + size, size);
					
					phi->spill = -ctx->stack_usage;
				}
				
				Val dst = val_stack(dt, phi->spill);
				Val src = val_gpr(dt.type, phi->value.gpr);
				inst2(ctx, MOV, &dst, &src, dt.type);
				
				return (Val) {
					.type = VAL_GPR,
					.is_owned = true,
					.dt.width = 0,
					.dt.type = dt_type,
					.gpr = src.gpr
				};
			}
		}
		
		loop(i, arrlen(ctx->locals)) if (ctx->locals[i].gpr != GPR_NONE) {
			GPR g = ctx->locals[i].gpr;
			spill_stack_slot(ctx, f, &ctx->locals[i]);
			
			return (Val) {
				.type = VAL_GPR,
				.is_owned = true,
				.dt.width = 0,
				.dt.type = dt_type,
				.gpr = g
			};
		}
		
		tb_panic(0, "Failed to allocate a GPR");
	}
	
	int search = __builtin_ffs(~ctx->gpr_allocator);
	assert(search != 0);
	
	GPR gpr = (GPR)(search-1);
	ctx->gpr_allocator |= (1u << gpr);
	
	// mark register as to be saved
	ctx->regs_to_save |= (1u << gpr) & (ctx->is_sysv ? SYSV_ABI_CALLEE_SAVED : WIN64_ABI_CALLEE_SAVED);
	
	return (Val) {
		.type = VAL_GPR,
		.is_owned = true,
		.dt.width = 0,
		.dt.type = dt_type,
		.gpr = gpr
	};
}

static Val alloc_xmm(Ctx* restrict ctx, TB_Function* f, TB_DataType dt) {
	if (ctx->xmm_allocator == 0xFFFF) {
		loop(i, arrlen(ctx->locals)) if (ctx->locals[i].xmm != XMM_NONE) {
			spill_stack_slot(ctx, f, &ctx->locals[i]);
			ctx->xmm_allocator |= (1u << ctx->locals[i].xmm);
			
			return (Val) {
				.type = VAL_XMM,
				.is_owned = true,
				.dt = dt,
				.xmm = ctx->locals[i].xmm
			};
		}
		
		tb_panic(0, "Failed to allocate a XMM");
	}
	
	int search = __builtin_ffs(~ctx->xmm_allocator);
	
	XMM xmm = (XMM)(search-1);
	ctx->xmm_allocator |= (1u << xmm);
	//printf("Alloc XMM%d\n", xmm);
	
	return (Val) {
		.type = VAL_XMM,
		.is_owned = true,
		.dt = dt,
		.xmm = xmm
	};
}

static void free_val(Ctx* restrict ctx, TB_Function* f, Val v) {
	if (!v.is_owned) return;
	
	if (v.type == VAL_GPR) {
		ctx->gpr_allocator &= ~(1u << v.gpr);
	} else if (v.type == VAL_XMM) {
		ctx->xmm_allocator &= ~(1u << v.xmm);
		//printf("Free XMM%d\n", v.xmm);
	} else if (v.type == VAL_MEM) {
		if (v.mem.base != RSP && v.mem.base != RBP) {
			ctx->gpr_allocator &= ~(1u << v.mem.base);
		}
		
		if (v.mem.index != GPR_NONE && v.mem.index != RSP && v.mem.index != RBP) {
			ctx->gpr_allocator &= ~(1u << v.mem.index);
		}
	}
}

static void free_gpr(Ctx* restrict ctx, TB_Function* f, GPR gpr) {
	ctx->gpr_allocator &= ~(1u << gpr);
}

static Val gen_float_const(Ctx* ctx, TB_Function* f, double float_value, TB_DataType dt) {
	// Unlike integers, there's no float immediates
	Val v = alloc_xmm(ctx, f, dt);
	XMM dst_xmm = v.xmm;
	
	assert(TB_IS_FLOAT_TYPE(dt.type) && dt.width == 0);
	uint64_t imm = (Cvt_F64U64){ .f = float_value }.i;
	
	if (imm == 0) {
		if (dst_xmm >= 8) emit(rex(true, dst_xmm, dst_xmm, 0));
		emit(0x0F);
		emit(0x57);
		emit(mod_rx_rm(MOD_DIRECT, dst_xmm, dst_xmm));
	} else {
		// Convert it to raw bits
		if (dst_xmm >= 8) emit(rex(true, dst_xmm, dst_xmm, 0));
		emit(dt.type == TB_F64 ? 0xF2 : 0xF3);
		emit(0x0F);
		emit(0x10);
		emit(((dst_xmm & 7) << 3) | RBP);
		
		uint32_t disp;
		if (dt.type == TB_F64) {
			uint64_t* rdata_payload = tb_platform_arena_alloc(sizeof(uint64_t));
			*rdata_payload = imm;
			
			disp = tb_emit_const_patch(f->module, ctx->function_id, code_pos(), rdata_payload, sizeof(uint64_t), s_local_thread_id);
		} else {
			uint32_t imm32 = (Cvt_F32U32){ .f = float_value }.i;
			
			uint32_t* rdata_payload = tb_platform_arena_alloc(sizeof(uint32_t));
			*rdata_payload = imm32;
			
			disp = tb_emit_const_patch(f->module, ctx->function_id, code_pos(), rdata_payload, sizeof(uint32_t), s_local_thread_id);
		}
		
		emit4(disp);
	}
	
	return v;
}

// Each of these eval_node functions will set a value in ctx->tree[n].val
// we don't directly return it because it can change as spills happen.
static Val eval_node(Ctx* restrict ctx, TB_Function* f, TreeNodeIndex n, TreeNodeIndex next) {
	assert(n != 0 && "Invalid node");
	TreeNode* restrict node = &ctx->tree[n];
	TB_Register r = node->reg;
	
	TB_RegTypeEnum reg_type = f->nodes.type[r];
	TB_DataType dt = f->nodes.dt[r];
	TB_RegPayload* restrict p = &f->nodes.payload[r];
	
	if (TB_UNLIKELY(ctx->use_count[r] > 1 || r < ctx->last_fence || r >= ctx->root_reg)) {
		// these are always thought of as out-of-scope in a weird way and have their
		// own special behavior described in the normal path.
		if (reg_type != TB_UNSIGNED_CONST && 
			reg_type != TB_SIGNED_CONST && 
			reg_type != TB_FLOAT_CONST && 
			reg_type != TB_PARAM && 
			reg_type != TB_PARAM_ADDR && 
			reg_type != TB_LOCAL && 
			reg_type != TB_PHI2) {
			StackSlot* slot = try_get_stack_slot(ctx, f, r, dt);
			
			if (slot) {
				if (slot->gpr != GPR_NONE) {
					return val_gpr(dt.type, slot->gpr);
				} else if (slot->xmm != XMM_NONE) {
					return val_xmm(dt, slot->xmm);
				} else {
					return val_stack(dt, slot->pos);
				}
			}
		}
	}
	
	Val val = {};
	switch (reg_type) {
		case TB_SIGNED_CONST:
		case TB_UNSIGNED_CONST: {
			int32_t imm32 = (int32_t)p->s_const;
			if (p->s_const == imm32) {
				val = val_imm(dt, imm32);
				break;
			}
			
			// explicit mov
			Val dst = alloc_gpr(ctx, f, dt.type, n);
			
			// mov reg64, imm64
			emit(rex(true, 0x0, dst.gpr, 0));
			emit(0xB8 + (dst.gpr & 0b111));
			emit8(p->u_const);
			
			val = dst;
			break;
		}
		case TB_FLOAT_CONST: {
			val = gen_float_const(ctx, f, p->f_const, dt);
			break;
		}
		case TB_STRING_CONST: {
			const char* str = p->str_const.data;
			size_t len = p->str_const.len;
			
			Val dst = alloc_gpr(ctx, f, dt.type, n);
			
			emit(rex(true, dst.gpr, RBP, 0));
			emit(0x8D);
			emit(mod_rx_rm(MOD_INDIRECT, dst.gpr, RBP));
			
			uint32_t disp = tb_emit_const_patch(f->module, ctx->function_id, code_pos(), str, len, s_local_thread_id);
			emit4(disp);
			
			val = dst;
			break;
		}
		case TB_GLOBAL_ADDRESS: {
			val = val_global(p->global_addr);
			break;
		}
		case TB_FUNC_ADDRESS: {
			int source_func = f - f->module->functions.data;
			
			Val dst = alloc_gpr(ctx, f, dt.type, n);
			
			emit(rex(true, dst.gpr, RBP, 0));
			emit(0x8D);
			emit(mod_rx_rm(MOD_INDIRECT, dst.gpr, RBP));
			emit4(0x0);
			
			int target_func = p->func_addr - f->module->functions.data;
			tb_emit_call_patch(f->module,
							   source_func,
							   target_func,
							   code_pos() - 4,
							   s_local_thread_id);
			
			val = dst;
			break;
		}
		case TB_EFUNC_ADDRESS: {
			int source_func = f - f->module->functions.data;
			
			Val dst = alloc_gpr(ctx, f, dt.type, n);
			
			emit(rex(true, dst.gpr, RBP, 0));
			emit(0x8D);
			emit(mod_rx_rm(MOD_INDIRECT, dst.gpr, RBP));
			emit4(0x0);
			
			tb_emit_ecall_patch(f->module,
								source_func,
								p->efunc_addr,
								code_pos() - 4,
								s_local_thread_id);
			
			val = dst;
			break;
		}
		
		case TB_RESTRICT: {
			val = val_addressof(ctx, f, eval_node(ctx, f, node->a, n));
			break;
		}
		
		
		
		case TB_ARRAY_ACCESS: {
			Val base = val_addressof(ctx, f, eval_node(ctx, f, node->a, n));
			Val index = val_rvalue(ctx, f, eval_node(ctx, f, node->b, n), ctx->tree[node->b].reg);
			
			uint32_t stride = p->array_access.stride;
			
			// move into a GPR to make life easier
			if (is_value_mem(&index)) {
				Val new_index = alloc_gpr(ctx, f, dt.type, n);
				
				inst2(ctx, MOV, &new_index, &index, TB_I64);
				
				free_val(ctx, f, index);
				index = new_index;
			}
			
			// TODO(NeGate): Redo this code, it's scary levels of branchy
			if (index.type == VAL_IMM) {
				base.mem.disp += index.imm * stride;
				val = base;
			} else if (index.type == VAL_GPR) {
				GPR base_reg = base.mem.base;
				int32_t disp = base.mem.disp;
				
				if (tb_is_power_of_two(stride)) {
					uint8_t stride_as_shift = __builtin_ffs(stride) - 1;
					
					if (stride_as_shift <= 3) {
						// it can use the shift in the memory operand
						if (base.mem.index != GPR_NONE) {
							// nested indices a[x][y]
							// lea dst, [base + index0 * scale0 + offset]
							// lea dst, [dst + index1 * scale1] or add dst, index1
							Val v = alloc_gpr(ctx, f, dt.type, 0);
							inst2(ctx, LEA, &v, &base, TB_PTR);
							
							if (stride_as_shift) {
								Val addr = val_base_index(dt, v.gpr, index.gpr, stride_as_shift);
								inst2(ctx, LEA, &v, &addr, TB_PTR);
							} else {
								Val idx = val_gpr(TB_PTR, index.gpr);
								inst2(ctx, ADD, &v, &idx, TB_PTR);
							}
							
							val = val_base_disp(dt, v.gpr, 0);
						} else {
							// single index a[x] with a small power of two
							val = val_base_index_disp(dt, base_reg, index.gpr, stride_as_shift, disp);
						}
					} else {
						if (index.is_owned) {
							Val new_index = alloc_gpr(ctx, f, dt.type, n);
							
							inst2(ctx, MOV, &new_index, &index, TB_I64);
							
							free_val(ctx, f, index);
							index = new_index;
						}
						assert(stride_as_shift < 64);
						
						// shl index, stride_as_shift
						emit(rex(true, 0x04, index.gpr, 0));
						emit(0xC1);
						emit(mod_rx_rm(MOD_DIRECT, 0x04, index.gpr));
						emit(stride_as_shift);
						
						// add dst, index
						emit(rex(true, base_reg, index.gpr, 0));
						emit(0x01);
						emit(mod_rx_rm(MOD_DIRECT, base_reg, index.gpr));
						
						val = index;
					}
				} else {
					Val dst = alloc_gpr(ctx, f, dt.type, n);
					
					// imul dst, index, stride
					emit(rex(true, dst.gpr, index.gpr, 0));
					emit(0x69);
					emit(mod_rx_rm(MOD_DIRECT, dst.gpr, index.gpr));
					emit4(stride);
					
					// add dst, base
					emit(rex(true, base_reg, dst.gpr, 0));
					emit(0x01);
					emit(mod_rx_rm(MOD_DIRECT, base_reg, dst.gpr));
					
					val = dst;
				}
			}
			
			free_val(ctx, f, base);
			free_val(ctx, f, index);
			break;
		}
		
		case TB_MEMBER_ACCESS: {
			Val v = val_addressof(ctx, f, eval_node(ctx, f, node->a, n));
			
			v.mem.disp += p->member_access.offset;
			val = v;
			break;
		}
		
		
		// Float binary operators
		case TB_FADD:
		case TB_FSUB:
		case TB_FMUL:
		case TB_FDIV: {
			// supported modes (for now)
			assert(dt.width <= 2);
			
			const static Inst2FPType tbl[] = { FP_ADD, FP_SUB, FP_MUL, FP_DIV };
			
			Val a = val_rvalue(ctx, f, eval_node(ctx, f, node->a, n), ctx->tree[node->a].reg);
			Val b = val_rvalue(ctx, f, eval_node(ctx, f, node->b, n), ctx->tree[node->b].reg);
			
			uint8_t flags = 0;
			flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
			flags |= (dt.width) ? INST2FP_PACKED : 0;
			
			Val dst;
			bool recycled = a.is_owned && a.type == VAL_XMM;
			if (recycled) {
				dst = a;
				//printf("recycled XMM%d for r%d (originally from r%d)\n", a.xmm, r, ctx->tree[node->a].reg);
			} else {
				dst = alloc_xmm(ctx, f, dt);
				inst2sse(ctx, FP_MOV, &dst, &a, flags);
			}
			
			Inst2FPType op = tbl[reg_type - TB_FADD];
			inst2sse(ctx, op, &dst, &b, flags);
			
			if (!recycled) free_val(ctx, f, a);
			free_val(ctx, f, b);
			
			val = dst;
			break;
		}
		
		// Integer binary operations
		case TB_AND:
		case TB_OR:
		case TB_XOR:
		case TB_ADD:
		case TB_SUB:
		case TB_MUL: {
			if (dt.width == 0) {
				// simple scalar ops
				const static Inst2Type ops[] = { AND, OR, XOR, ADD, SUB, IMUL };
				bool can_immediates = (reg_type != TB_MUL);
				bool can_mem_dst = (reg_type != TB_MUL);
				
				Val a = val_rvalue(ctx, f, eval_node(ctx, f, node->a, n), ctx->tree[node->a].reg);
				Val b = val_rvalue(ctx, f, eval_node(ctx, f, node->b, n), ctx->tree[node->b].reg);
				
				Val dst;
				bool recycle = a.is_owned && !(is_value_mem(&a) && !can_mem_dst);
				if (recycle) {
					dst = a;
				} else {
					dst = alloc_gpr(ctx, f, dt.type, n);
					inst2(ctx, MOV, &dst, &a, dt.type);
				}
				
				// we can't do a OP mem, mem
				// and imul doesn't support a simple OP r/m, imm mode
				// we'll need a temporary in those cases
				bool is_mem_dst = is_value_mem(&dst);
				if ((is_mem_dst && is_value_mem(&b)) ||
					(b.type == VAL_IMM && !can_immediates) ||
					(is_mem_dst && !can_mem_dst)) {
					Val tmp = alloc_gpr(ctx, f, dt.type, n);
					
					inst2(ctx, MOV, &tmp, &b, dt.type);
					inst2(ctx, ops[reg_type - TB_AND], &dst, &tmp, dt.type);
					
					free_val(ctx, f, tmp);
				} else {
					inst2(ctx, ops[reg_type - TB_AND], &dst, &b, dt.type);
				}
				
				if (!recycle) free_val(ctx, f, a);
				free_val(ctx, f, b);
				
				val = dst;
				break;
			} else {
				// supported modes (for now)
				assert(dt.width <= 2);
				
				Val a = val_rvalue(ctx, f, eval_node(ctx, f, node->a, n), ctx->tree[node->a].reg);
				Val b = val_rvalue(ctx, f, eval_node(ctx, f, node->b, n), ctx->tree[node->b].reg);
				
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				
				Val dst;
				bool recycle = a.is_owned && a.type == VAL_XMM;
				if (recycle) {
					dst = a;
				} else {
					dst = alloc_xmm(ctx, f, dt);
					inst2sse(ctx, FP_MOV, &dst, &a, flags);
				}
				
				// integer multiplication is a complex case
				switch (reg_type) {
					case TB_AND: inst2sse(ctx, FP_AND, &dst, &b, flags); break;
					case TB_OR: inst2sse(ctx, FP_OR, &dst, &b, flags); break;
					case TB_XOR: inst2sse(ctx, FP_XOR, &dst, &b, flags); break;
					case TB_ADD: {
						// padd(b|w|d|q)
						const static uint8_t opcode[] = { 0xFC, 0xFD, 0xFE, 0xD4 };
						assert((dt.type - TB_I8) < tb_arrlen(opcode));
						
						emit(0x66);
						emit(0x0F);
						emit(opcode[dt.type - TB_I8]);
						emit_memory_operand(ctx, dst.xmm, &b);
						break;
					}
					case TB_SUB: {
						// psub(b|w|d|q)
						const static uint8_t opcode[] = { 0xF8, 0xF9, 0xFA, 0xFB };
						assert((dt.type - TB_I8) < tb_arrlen(opcode));
						
						emit(0x66);
						emit(0x0F);
						emit(opcode[dt.type - TB_I8]);
						emit_memory_operand(ctx, dst.xmm, &b);
						break;
					}
					case TB_MUL: {
						tb_panic(0, "Implement vector integer multiply");
					}
					default: tb_unreachable();
				}
				
				if (!recycle) free_val(ctx, f, a);
				free_val(ctx, f, b);
				
				val = dst;
				break;
			}
		}
		
		default:
		assert(0 && "TODO: Implement node eval");
		break;
	}
	
	return val;
}

// some macros local to eval
#define kill(n) (ctx->tree[n].val = (Val){})
#define use(n) (ctx->tree[n].val)

static Val eval(Ctx* restrict ctx, TB_Function* f, TB_Register root_reg, TB_Register next) {
	// we reserve slot 0
	ctx->tree_len = 1;
	ctx->root_reg = root_reg;
	ctx->next_reg = next;
	
	// Generate tree from DAG
	gen_tree(ctx, f, root_reg);
	
	assert(ctx->tree_len > 1 && "We should have pushed some nodes");
	TreeNodeIndex root_node = ctx->tree_len - 1;
	assert(ctx->tree[root_node].reg == root_reg);
	
#if 0
	// Walk tree to generate machine code
	//print_tree(ctx, f, root_node, 0);
	
	Val v = eval_node(ctx, f, root_node, 0);
#else
	// Iterate over the nodes linearly since we've effectively scheduled everything
	loop_range(n, 1, ctx->tree_len) {
		// Spill if necessary
		int steps = 0;
		while (steps < 2 && __builtin_popcount(ctx->gpr_allocator) >= 14) {
			loop_range(j, 1, n) if (ctx->tree[j].val.type == VAL_GPR) {
				TB_DataType dt = ctx->tree[j].val.dt;
				
				int size = get_data_type_size(dt);
				ctx->stack_usage = align_up(ctx->stack_usage + size, size);
				
				Val dst = val_stack(dt, -ctx->stack_usage);
				Val src = ctx->tree[j].val;
				inst2(ctx, MOV, &dst, &src, dt.type);
				
				ctx->tree[j].val = dst;
				ctx->gpr_allocator &= ~(1u << src.gpr);
				
				//printf("Spill internal node %zu to [RBP-%d]\n", j, ctx->stack_usage);
				break;
			}
			
			steps++;
			//printf("Allocated: %d / %d\n", __builtin_popcount(ctx->gpr_allocator), 16);
		}
		
		TreeNode* restrict node = &ctx->tree[n];
		TB_Register r = node->reg;
		
		TB_RegTypeEnum reg_type = f->nodes.type[r];
		TB_DataType dt = f->nodes.dt[r];
		TB_RegPayload* restrict p = &f->nodes.payload[r];
		
		// TODO(NeGate): Simplify this code
		if (TB_UNLIKELY(ctx->use_count[r] > 1 || r < ctx->last_fence || r >= ctx->root_reg)) {
			// these are always thought of as out-of-scope in a weird way and have their
			// own special behavior described in the normal path.
			if (reg_type != TB_UNSIGNED_CONST && 
				reg_type != TB_SIGNED_CONST && 
				reg_type != TB_FLOAT_CONST && 
				reg_type != TB_PARAM && 
				reg_type != TB_PARAM_ADDR && 
				reg_type != TB_LOCAL && 
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
					continue;
				}
			}
		}
		
		Val val = {};
		switch (reg_type) {
			case TB_SIGNED_CONST:
			case TB_UNSIGNED_CONST: {
				int32_t imm32 = (int32_t)p->s_const;
				if (p->s_const == imm32) {
					val = val_imm(dt, imm32);
					break;
				}
				
				// explicit mov
				val = alloc_gpr(ctx, f, dt.type, 0);
				
				// mov reg64, imm64
				emit(rex(true, 0x0, val.gpr, 0));
				emit(0xB8 + (val.gpr & 0b111));
				emit8(p->u_const);
				break;
			}
			case TB_FLOAT_CONST: {
				val = gen_float_const(ctx, f, p->f_const, dt);
				break;
			}
			case TB_STRING_CONST: {
				const char* str = p->str_const.data;
				size_t len = p->str_const.len;
				
				val = alloc_gpr(ctx, f, dt.type, n);
				
				emit(rex(true, val.gpr, RBP, 0));
				emit(0x8D);
				emit(mod_rx_rm(MOD_INDIRECT, val.gpr, RBP));
				
				uint32_t disp = tb_emit_const_patch(f->module, ctx->function_id, code_pos(), str, len, s_local_thread_id);
				emit4(disp);
				break;
			}
			case TB_GLOBAL_ADDRESS: {
				val = val_global(p->global_addr);
				break;
			}
			case TB_EFUNC_ADDRESS:
			case TB_FUNC_ADDRESS: {
				int source_func = f - f->module->functions.data;
				
				val = alloc_gpr(ctx, f, dt.type, n);
				
				emit(rex(true, val.gpr, RBP, 0));
				emit(0x8D);
				emit(mod_rx_rm(MOD_INDIRECT, val.gpr, RBP));
				emit4(0x0);
				
				if (reg_type == TB_EFUNC_ADDRESS) {
					tb_emit_ecall_patch(f->module, source_func, p->efunc_addr, code_pos() - 4, s_local_thread_id);
				} else {
					int target_func = p->func_addr - f->module->functions.data;
					tb_emit_call_patch(f->module, source_func, target_func, code_pos() - 4, s_local_thread_id);
				}
				break;
			}
			
			case TB_PARAM: {
				int id = f->nodes.payload[r].param.id;
				assert(TB_FIRST_PARAMETER_REG + id == r);
				
				StackSlot* slot = &ctx->locals[id];
				if (slot->gpr != GPR_NONE) val = val_gpr(dt.type, slot->gpr);
				else if (slot->xmm != XMM_NONE) val = val_xmm(dt, slot->xmm);
				else {
					val = val_stack(dt, slot->pos);
					val.mem.is_rvalue = true;
				}
				break;
			}
			case TB_PARAM_ADDR: {
				TB_Register param = f->nodes.payload[r].param_addr.param;
				int id = f->nodes.payload[param].param.id;
				assert(TB_FIRST_PARAMETER_REG + id == param);
				
				val = spill_stack_slot(ctx, f, &ctx->locals[id]);
				break;
			}
			case TB_CALL:
			case TB_ECALL:
			case TB_VCALL:
			case TB_LOCAL: {
				val = get_stack_slot(ctx, f, r, dt);
				break;
			}
			
			case TB_LOAD: {
				val= val_addressof(ctx, f, use(node->a));
				
				val.dt = dt;
				val.mem.is_rvalue = true;
				
				kill(node->a);
				break;
			}
			
			case TB_RESTRICT: {
				val = val_addressof(ctx, f, eval_node(ctx, f, node->a, n));
				kill(node->a);
				break;
			}
			case TB_ARRAY_ACCESS: {
				Val base = val_addressof(ctx, f, eval_node(ctx, f, node->a, n));
				Val index = val_rvalue(ctx, f, eval_node(ctx, f, node->b, n), ctx->tree[node->b].reg);
				
				uint32_t stride = p->array_access.stride;
				
				// move into a GPR to make life easier
				if (is_value_mem(&index)) {
					Val new_index = alloc_gpr(ctx, f, dt.type, n);
					
					inst2(ctx, MOV, &new_index, &index, TB_I64);
					
					free_val(ctx, f, index);
					index = new_index;
				}
				
				// TODO(NeGate): Redo this code, it's scary levels of branchy
				if (index.type == VAL_IMM) {
					base.mem.disp += index.imm * stride;
					val = base;
				} else if (index.type == VAL_GPR) {
					GPR base_reg = base.mem.base;
					int32_t disp = base.mem.disp;
					
					if (tb_is_power_of_two(stride)) {
						uint8_t stride_as_shift = __builtin_ffs(stride) - 1;
						
						if (stride_as_shift <= 3) {
							// it can use the shift in the memory operand
							if (base.mem.index != GPR_NONE) {
								// nested indices a[x][y]
								// lea dst, [base + index0 * scale0 + offset]
								// lea dst, [dst + index1 * scale1] or add dst, index1
								Val v = alloc_gpr(ctx, f, dt.type, 0);
								inst2(ctx, LEA, &v, &base, TB_PTR);
								
								if (stride_as_shift) {
									Val addr = val_base_index(dt, v.gpr, index.gpr, stride_as_shift);
									inst2(ctx, LEA, &v, &addr, TB_PTR);
								} else {
									Val idx = val_gpr(TB_PTR, index.gpr);
									inst2(ctx, ADD, &v, &idx, TB_PTR);
								}
								
								val = val_base_disp(dt, v.gpr, 0);
							} else {
								// single index a[x] with a small power of two
								val = val_base_index_disp(dt, base_reg, index.gpr, stride_as_shift, disp);
							}
						} else {
							if (index.is_owned) {
								Val new_index = alloc_gpr(ctx, f, dt.type, n);
								
								inst2(ctx, MOV, &new_index, &index, TB_I64);
								
								free_val(ctx, f, index);
								index = new_index;
							}
							assert(stride_as_shift < 64);
							
							// shl index, stride_as_shift
							emit(rex(true, 0x04, index.gpr, 0));
							emit(0xC1);
							emit(mod_rx_rm(MOD_DIRECT, 0x04, index.gpr));
							emit(stride_as_shift);
							
							// add dst, index
							emit(rex(true, base_reg, index.gpr, 0));
							emit(0x01);
							emit(mod_rx_rm(MOD_DIRECT, base_reg, index.gpr));
							
							val = index;
						}
					} else {
						val = alloc_gpr(ctx, f, dt.type, n);
						
						// imul dst, index, stride
						emit(rex(true, val.gpr, index.gpr, 0));
						emit(0x69);
						emit(mod_rx_rm(MOD_DIRECT, val.gpr, index.gpr));
						emit4(stride);
						
						// add dst, base
						emit(rex(true, base_reg, val.gpr, 0));
						emit(0x01);
						emit(mod_rx_rm(MOD_DIRECT, base_reg, val.gpr));
					}
				}
				
				kill(node->a);
				kill(node->b);
				break;
			}
			case TB_MEMBER_ACCESS: {
				val = val_addressof(ctx, f, use(node->a));
				
				val.mem.disp += p->member_access.offset;
				kill(node->a);
				break;
			}
			
			case TB_NEG: {
				Val src = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				
				if (dt.type == TB_F64) {
					// .LCPI0_0:
					//   .quad   0x8000000000000000
					//   .quad   0x8000000000000000
					// ...
					// xorps   xmm0, xmmword ptr [rip + .LCPI0_0]
					val = alloc_xmm(ctx, f, dt);
					XMM dst_xmm = val.xmm;
					
					if (dst_xmm >= 8) emit(rex(true, dst_xmm, dst_xmm, 0));
					emit(dt.type == TB_F64 ? 0xF2 : 0xF3);
					emit(0x0F);
					emit(0x57);
					emit(((dst_xmm & 7) << 3) | RBP);
					
					uint64_t* rdata_payload = tb_platform_arena_alloc(2 * sizeof(uint64_t));
					rdata_payload[0] = (1ull << 63ull);
					rdata_payload[1] = (1ull << 63ull);
					
					uint32_t disp = tb_emit_const_patch(f->module, ctx->function_id, code_pos(), rdata_payload, 2 * sizeof(uint64_t), s_local_thread_id);
					emit4(disp);
					
					free_val(ctx, f, src);
				} else if (dt.type == TB_F32) {
					// .LCPI0_0:
					//   .long   0x80000000
					//   .long   0x80000000
					//   .long   0x80000000
					//   .long   0x80000000
					// ...
					// xorps   xmm0, xmmword ptr [rip + .LCPI0_0]
					val = alloc_xmm(ctx, f, dt);
					XMM dst_xmm = val.xmm;
					
					if (dst_xmm >= 8) emit(rex(true, dst_xmm, dst_xmm, 0));
					emit(0x0F);
					emit(0x57);
					emit(((dst_xmm & 7) << 3) | RBP);
					
					uint64_t* rdata_payload = tb_platform_arena_alloc(4 * sizeof(uint32_t));
					rdata_payload[0] = (1ull << 31ull);
					rdata_payload[1] = (1ull << 31ull);
					rdata_payload[2] = (1ull << 31ull);
					rdata_payload[3] = (1ull << 31ull);
					
					uint32_t disp = tb_emit_const_patch(f->module, ctx->function_id, code_pos(), rdata_payload, 4 * sizeof(uint32_t), s_local_thread_id);
					emit4(disp);
					
					free_val(ctx, f, src);
				} else {
					bool recycle = src.is_owned && src.type != VAL_MEM;
					if (recycle) {
						inst1(ctx, NEG, &src);
						val = src;
					} else {
						val = alloc_gpr(ctx, f, dt.type, n);
						
						inst2(ctx, MOV, &val, &src, dt.type);
						inst1(ctx, NEG, &val);
						
						free_val(ctx, f, src);
					}
				}
				
				break;
			}
			case TB_NOT: {
				Val src = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				
				bool recycled = src.is_owned && src.type != VAL_MEM;
				if (recycled) {
					val = src;
					
					inst1(ctx, NOT, &src);
					break;
				} else {
					val = alloc_gpr(ctx, f, dt.type, n);
					
					inst2(ctx, MOV, &val, &src, dt.type);
					inst1(ctx, NOT, &val);
					
					free_val(ctx, f, src);
				}
				break;
			}
			
			// Integer binary operations
			case TB_AND:
			case TB_OR:
			case TB_XOR:
			case TB_ADD:
			case TB_SUB:
			case TB_MUL: {
				Val a = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				Val b = val_rvalue(ctx, f, use(node->b), ctx->tree[node->b].reg);
				
				if (dt.width == 0) {
					// simple scalar ops
					const static Inst2Type ops[] = { AND, OR, XOR, ADD, SUB, IMUL };
					
					bool recycled = a.is_owned && !(is_value_mem(&a) && reg_type == TB_MUL);
					if (recycled) {
						val = a;
					} else {
						val = alloc_gpr(ctx, f, dt.type, n);
						inst2(ctx, MOV, &val, &a, dt.type);
					}
					
					// we can't do a OP mem, mem
					// and imul doesn't support a simple OP r/m, imm mode
					// we'll need a temporary in those cases
					bool is_mem_dst = is_value_mem(&val);
					
					bool needs_temporary = is_mem_dst && is_value_mem(&b);
					needs_temporary |= (reg_type == TB_MUL && (is_mem_dst || b.type == VAL_IMM));
					
					if (needs_temporary) {
						Val tmp = alloc_gpr(ctx, f, dt.type, n);
						
						inst2(ctx, MOV, &tmp, &b, dt.type);
						inst2(ctx, ops[reg_type - TB_AND], &val, &tmp, dt.type);
						
						free_gpr(ctx, f, tmp.gpr);
					} else {
						inst2(ctx, ops[reg_type - TB_AND], &val, &b, dt.type);
					}
					
					free_val(ctx, f, a);
					free_val(ctx, f, b);
				} else {
					// supported modes (for now)
					assert(dt.width <= 2);
					
					uint8_t flags = 0;
					flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
					flags |= (dt.width) ? INST2FP_PACKED : 0;
					
					bool recycled = a.is_owned && a.type == VAL_XMM;
					if (recycled) {
						val = a;
					} else {
						val = alloc_xmm(ctx, f, dt);
						inst2sse(ctx, FP_MOV, &val, &a, flags);
					}
					
					// integer multiplication is a complex case
					switch (reg_type) {
						case TB_AND: inst2sse(ctx, FP_AND, &val, &b, flags); break;
						case TB_OR: inst2sse(ctx, FP_OR, &val, &b, flags); break;
						case TB_XOR: inst2sse(ctx, FP_XOR, &val, &b, flags); break;
						case TB_ADD: {
							// padd(b|w|d|q)
							const static uint8_t opcode[] = { 0xFC, 0xFD, 0xFE, 0xD4 };
							assert((dt.type - TB_I8) < tb_arrlen(opcode));
							
							emit(0x66);
							emit(0x0F);
							emit(opcode[dt.type - TB_I8]);
							emit_memory_operand(ctx, val.xmm, &b);
							break;
						}
						case TB_SUB: {
							// psub(b|w|d|q)
							const static uint8_t opcode[] = { 0xF8, 0xF9, 0xFA, 0xFB };
							assert((dt.type - TB_I8) < tb_arrlen(opcode));
							
							emit(0x66);
							emit(0x0F);
							emit(opcode[dt.type - TB_I8]);
							emit_memory_operand(ctx, val.xmm, &b);
							break;
						}
						case TB_MUL: {
							tb_panic(0, "Implement vector integer multiply");
						}
						default: tb_unreachable();
					}
					
					free_val(ctx, f, a);
					free_val(ctx, f, b);
				}
				
				kill(node->a);
				kill(node->b);
				break;
			}
			
			case TB_SHR:
			case TB_SHL:
			case TB_SAR: {
				Val a = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				Val b = val_rvalue(ctx, f, use(node->b), ctx->tree[node->b].reg);
				
				if (a.type == VAL_IMM && b.type == VAL_IMM) {
					val = val_imm(dt, a.imm << b.imm);
					break;
				}
				
				bool recycled = a.is_owned && a.type == VAL_GPR;
				if (recycled) {
					val = a;
				} else {
					val = alloc_gpr(ctx, f, dt.type, n);
					inst2(ctx, MOV, &val, &a, dt.type);
				}
				free_val(ctx, f, a);
				free_val(ctx, f, b);
				
				bool is_64bit = dt.type == TB_I64;
				if (b.type == VAL_IMM) {
					assert(b.imm < 64);
					
					// shl r/m, imm
					if (dt.type == TB_I16) emit(0x66);
					emit(rex(is_64bit, 0x00, val.gpr, 0x00));
					emit(dt.type == TB_I8 ? 0xC0 : 0xC1);
					emit(mod_rx_rm(MOD_DIRECT, 0x04, val.gpr));
					emit(b.imm);
					break;
				}
				
				int rcx_savepoint = 0;
				if (!evict_gpr(ctx, f, RCX)) {
					// weird but it works, just give it a basic saveslot
					ctx->stack_usage = align_up(ctx->stack_usage + 8, 8);
					rcx_savepoint = -ctx->stack_usage;
					
					Val sav = val_stack(TB_TYPE_I64, rcx_savepoint);
					Val reg = val_gpr(TB_I64, RCX);
					inst2(ctx, MOV, &sav, &reg, dt.type);
				}
				
				Val rcx = val_gpr(dt.type, RCX);
				if (!is_value_gpr(&a, RCX)) {
					inst2(ctx, MOV, &rcx, &a, dt.type);
				}
				
				// D2 /4       shl r/m, cl
				// D2 /5       shr r/m, cl
				// D2 /7       sar r/m, cl
				if (dt.type == TB_I16) emit(0x66);
				emit(rex(is_64bit, 0x00, val.gpr, 0x00));
				emit(dt.type == TB_I8 ? 0xD2 : 0xD3);
				
				free_val(ctx, f, a);
				free_val(ctx, f, b);
				
				switch (reg_type) {
					case TB_SHL:
					emit(mod_rx_rm(MOD_DIRECT, 0x04, val.gpr));
					break;
					case TB_SHR:
					emit(mod_rx_rm(MOD_DIRECT, 0x05, val.gpr));
					break;
					case TB_SAR:
					emit(mod_rx_rm(MOD_DIRECT, 0x07, val.gpr));
					break;
					default:
					tb_unreachable();
				}
				
				if (rcx_savepoint) {
					// reload
					Val sav = val_stack(TB_TYPE_I64, rcx_savepoint);
					Val reg = val_gpr(TB_I64, RCX);
					
					inst2(ctx, MOV, &reg, &sav, dt.type);
				}
				
				kill(node->a);
				kill(node->b);
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
				
				// reserve
				ctx->gpr_allocator |= (1u << RAX);
				ctx->gpr_allocator |= (1u << RDX);
				
				Val a = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				Val b = val_rvalue(ctx, f, use(node->b), ctx->tree[node->b].reg);
				
				// needs to mov the a value into rdx:rax
				Val rax = val_gpr(dt.type, RAX);
				if (!is_value_gpr(&a, RAX)) {
					inst2(ctx, MOV, &rax, &a, dt.type);
				}
				
				if (is_signed) {
					// cdq
					emit(0x99);
				} else {
					// xor rdx, rdx
					emit(0x31);
					emit(mod_rx_rm(MOD_DIRECT, RDX, RDX));
				}
				
				if (b.type == VAL_IMM) {
					Val tmp = alloc_gpr(ctx, f, dt.type, n);
					
					inst2(ctx, MOV, &tmp, &b, dt.type);
					inst1(ctx, IDIV, &tmp);
					
					free_gpr(ctx, f, tmp.gpr);
				} else {
					inst1(ctx, IDIV, &b);
				}
				
				// the return value is in RAX for division
				// and RDX for modulo
				val = val_gpr(dt.type, is_div ? RAX : RDX);
				val.is_owned = true;
				
				free_val(ctx, f, a);
				free_val(ctx, f, b);
				
				// NOTE(NeGate): This is bit janky but to reload the old stuff to their
				// registers while keeping our results we just XCHG (not optimal)
				if (rax_savepoint) {
					// reload
					Val sav = val_stack(TB_TYPE_I64, rax_savepoint);
					Val reg = val_gpr(TB_I64, RAX);
					
					inst2(ctx, is_div ? XCHG : MOV, &reg, &sav, dt.type);
					if (is_div) val = sav;
				} else {
					free_gpr(ctx, f, RAX);
				}
				
				if (rdx_savepoint) {
					// reload
					Val sav = val_stack(TB_TYPE_I64, rdx_savepoint);
					Val reg = val_gpr(TB_I64, RCX);
					
					inst2(ctx, is_div ? MOV : XCHG, &reg, &sav, dt.type);
					if (!is_div) val = sav;
				} else {
					free_gpr(ctx, f, RDX);
				}
				
				kill(node->a);
				kill(node->b);
				break;
			}
			
			case TB_CMP_EQ:
			case TB_CMP_NE:
			case TB_CMP_SLT:
			case TB_CMP_SLE:
			case TB_CMP_ULT:
			case TB_CMP_ULE:
			case TB_CMP_FLT:
			case TB_CMP_FLE: {
				TB_DataType cmp_dt = p->cmp.dt;
				assert(cmp_dt.width == 0 && "TODO: Implement vector compares");
				
				if (cmp_dt.type == TB_BOOL) cmp_dt.type = TB_I8;
				
				Val a = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				Val b = val_rvalue(ctx, f, use(node->b), ctx->tree[node->b].reg);
				
				bool convert_to_reg = !(n == (ctx->tree_len-1) && f->nodes.type[ctx->next_reg] == TB_IF);
				if (convert_to_reg) {
					val = alloc_gpr(ctx, f, TB_I8, n);
					
					// xor temp, temp
					if (val.gpr >= 8) emit(rex(false, val.gpr, val.gpr, 0));
					emit(0x31);
					emit(mod_rx_rm(MOD_DIRECT, val.gpr, val.gpr));
				}
				
				Cond cc;
				if (TB_IS_FLOAT_TYPE(cmp_dt.type)) {
					uint8_t flags = 0;
					flags |= (cmp_dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
					flags |= (cmp_dt.width) ? INST2FP_PACKED : 0;
					
					if (is_value_mem(&a) && is_value_mem(&b)) {
						Val tmp = alloc_xmm(ctx, f, cmp_dt);
						
						inst2sse(ctx, FP_MOV, &tmp, &a, flags);
						inst2sse(ctx, FP_UCOMI, &tmp, &b, flags);
						
						free_val(ctx, f, tmp);
					} else {
						inst2sse(ctx, FP_UCOMI, &a, &b, flags);
					}
					
					switch (reg_type) {
						case TB_CMP_EQ: cc = E; break;
						case TB_CMP_NE: cc = NE; break;
						case TB_CMP_FLT: cc = L; break;
						case TB_CMP_FLE: cc = LE; break;
						default: tb_unreachable();
					}
				} else {
					bool invert = false;
					if (is_value_mem(&a) && is_value_mem(&b)) {
						Val tmp = alloc_gpr(ctx, f, cmp_dt.type, n);
						
						inst2(ctx, MOV, &tmp, &a, cmp_dt.type);
						inst2(ctx, CMP, &tmp, &b, cmp_dt.type);
						
						free_val(ctx, f, tmp);
					} else {
						invert = (a.type == VAL_IMM);
						
						if (invert) inst2(ctx, CMP, &b, &a, cmp_dt.type);
						else inst2(ctx, CMP, &a, &b, cmp_dt.type);
					}
					
					switch (reg_type) {
						case TB_CMP_EQ: cc = E; break;
						case TB_CMP_NE: cc = NE; break;
						case TB_CMP_SLT: cc = L; break;
						case TB_CMP_SLE: cc = LE; break;
						case TB_CMP_ULT: cc = B; break;
						case TB_CMP_ULE: cc = BE; break;
						default: tb_unreachable();
					}
					cc ^= invert;
				}
				
				free_val(ctx, f, b);
				free_val(ctx, f, a);
				
				if (convert_to_reg) {
					// setcc v
					if (val.gpr >= 8) emit(rex(true, val.gpr, val.gpr, 0));
					emit(0x0F);
					emit(0x90 + cc);
					emit(mod_rx_rm(MOD_DIRECT, val.gpr, val.gpr));
				} else {
					val = val_flags(cc);
				}
				break;
			}
			
			case TB_X86INTRIN_SQRT:
			case TB_X86INTRIN_RSQRT: {
				Val src = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				kill(node->a);
				
				bool recycled = src.is_owned && !is_value_mem(&src);
				if (recycled) {
					uint8_t flags = 0;
					flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
					flags |= (dt.width) ? INST2FP_PACKED : 0;
					inst2sse(ctx, reg_type == TB_X86INTRIN_SQRT ? FP_SQRT : FP_RSQRT, &src, &src, flags);
					
					val = src;
				} else {
					val = alloc_xmm(ctx, f, dt);
					
					uint8_t flags = 0;
					flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
					flags |= (dt.width) ? INST2FP_PACKED : 0;
					inst2sse(ctx, reg_type == TB_X86INTRIN_SQRT ? FP_SQRT : FP_RSQRT, &val, &src, flags);
				}
				break;
			}
			
			case TB_FLOAT_EXT: {
				Val src = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				kill(node->a);
				
				val = alloc_xmm(ctx, f, dt);
				
				uint8_t flags = 0;
				flags |= (src.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (src.dt.width) ? INST2FP_PACKED : 0;
				inst2sse(ctx, FP_CVT, &val, &src, flags);
				break;
			}
			
			case TB_TRUNCATE: {
				Val src = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				kill(node->a);
				
				if (TB_IS_FLOAT_TYPE(dt.type)) {
					val = alloc_xmm(ctx, f, dt);
					
					uint8_t flags = 0;
					flags |= (src.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
					flags |= (src.dt.width) ? INST2FP_PACKED : 0;
					inst2sse(ctx, FP_CVT, &val, &src, flags);
					break;
				} else {
					if (src.type == VAL_IMM) {
						src.dt = dt;
						val = src;
						break;
					}
					
					bool recycle = src.is_owned && src.type == VAL_GPR;
					if (recycle) {
						val = src;
					} else {
						val = alloc_gpr(ctx, f, dt.type, n);
						inst2(ctx, MOV, &val, &src, src.dt.type);
					}
					
					if (dt.type == TB_I16) {
						inst2(ctx, MOVZXW, &val, &src, TB_I16);
					} else if (dt.type == TB_I8 || dt.type == TB_BOOL) {
						inst2(ctx, MOVZXB, &val, &src, TB_I8);
					}
					break;
				}
			}
			
			case TB_PHI2: {
				PhiValue* phi = find_phi(ctx, r);
				assert(phi && "PHI node not initialized but used");
				
				val = phi->value;
				break;
			}
			
			case TB_FLOAT2INT: {
				assert(dt.width == 0 && "TODO: Implement vector float2int");
				
				Val src = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				assert(src.type == VAL_MEM || src.type == VAL_GLOBAL || src.type == VAL_XMM);
				
				Val v = alloc_gpr(ctx, f, dt.type, n);
				
				// it's either 32bit or 64bit conversion
				// F3 0F 2D /r            CVTSS2SI xmm1, r/m32
				// F3 REX.W 0F 2D /r      CVTSS2SI xmm1, r/m64
				// F2 0F 2D /r            CVTSD2SI xmm1, r/m32
				// F2 REX.W 0F 2D /r      CVTSD2SI xmm1, r/m64
				if (src.dt.width == 0) {
					emit((src.dt.type == TB_F64) ? 0xF2 : 0xF3);
				} else if (src.dt.type == TB_F64) {
					// packed double
					emit(0x66);
				}
				
				uint8_t rx = v.gpr;
				uint8_t base, index;
				if (src.type == VAL_MEM) {
					base = src.mem.base;
					index = src.mem.index != GPR_NONE ? src.mem.index : 0;
				} else if (src.type == VAL_XMM) {
					base = src.xmm;
					index = 0;
				} else tb_todo();
				
				bool is_64bit = (dt.type == TB_I64);
				if (is_64bit || rx >= 8 || base >= 8 || index >= 8) {
					emit(rex(is_64bit, rx, base, index));
				}
				
				emit(0x0F);
				emit(0x2D);
				emit_memory_operand(ctx, rx, &src);
				
				free_val(ctx, f, src);
				val = v;
				break;
			}
			case TB_INT2FLOAT: {
				assert(dt.width == 0 && "TODO: Implement vector int2float");
				
				Val src = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				if (src.type == VAL_IMM) {
					val = gen_float_const(ctx, f, (double)src.imm, dt);
					break;
				}
				
				assert(src.type == VAL_MEM || src.type == VAL_GLOBAL || src.type == VAL_GPR);
				val = alloc_xmm(ctx, f, dt);
				
				// it's either 32bit or 64bit conversion
				// F3 0F 2A /r            CVTSI2SS xmm1, r/m32
				// F3 REX.W 0F 2A /r      CVTSI2SS xmm1, r/m64
				// F2 0F 2A /r            CVTSI2SD xmm1, r/m32
				// F2 REX.W 0F 2A /r      CVTSI2SD xmm1, r/m64
				if (dt.width == 0) {
					emit((dt.type == TB_F64) ? 0xF2 : 0xF3);
				} else if (dt.type == TB_F64) {
					// packed double
					emit(0x66);
				}
				
				uint8_t rx = val.xmm;
				uint8_t base, index;
				if (src.type == VAL_MEM) {
					base = src.mem.base;
					index = src.mem.index != GPR_NONE ? src.mem.index : 0;
				} else if (src.type == VAL_GPR) {
					base = src.gpr;
					index = 0;
				} else tb_unreachable();
				
				bool is_64bit = (src.dt.type == TB_I64);
				if (is_64bit || rx >= 8 || base >= 8 || index >= 8) {
					emit(rex(is_64bit, rx, base, index));
				}
				
				emit(0x0F);
				emit(0x2A);
				emit_memory_operand(ctx, rx, &src);
				break;
			}
			
			case TB_PTR2INT: {
				assert(dt.type == TB_I64 && "TODO: implement the other ptr2int variations");
				
				Val src = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				
				src.dt.type = TB_I64;
				free_val(ctx, f, src);
				
				val = src;
				break;
			}
			case TB_INT2PTR:
			case TB_SIGN_EXT:
			case TB_ZERO_EXT: {
				assert(dt.width == 0 && "TODO: Implement vector zero extend");
				bool sign_ext = (reg_type == TB_SIGN_EXT);
				
				Val src = val_rvalue(ctx, f, use(node->a), ctx->tree[node->a].reg);
				if (src.type == VAL_IMM) {
					src.dt = dt;
					val = src;
					break;
				}
				
				bool recycled = src.is_owned && src.type != VAL_MEM;
				if (recycled) {
					val = src;
				} else {
					val = alloc_gpr(ctx, f, dt.type, n);
					inst2(ctx, MOV, &val, &src, src.dt.type);
				}
				
				if (src.dt.type == TB_I32 && sign_ext) {
					inst2(ctx, MOVSXD, &val, &src, TB_I32);
				} else if (src.dt.type == TB_I16) {
					inst2(ctx, sign_ext ? MOVSXW : MOVZXW, &val, &src, TB_I16);
				} else if (src.dt.type == TB_I8 || src.dt.type == TB_BOOL) {
					inst2(ctx, sign_ext ? MOVSXB : MOVZXB, &val, &src, TB_I8);
				}
				
				free_val(ctx, f, src);
				break;
			}
			
			default:
			assert(0 && "TODO: Implement node eval");
			break;
		}
		
		node->val = val;
	}
#endif
	
	Val val = use(root_node);
	assert(val.type && "eval failed to initialize the resulting node.");
	return val;
}

#undef kill
#undef use

static Val val_addressof(Ctx* ctx, TB_Function* f, Val v) {
	if (v.dt.type == TB_BOOL) v.dt.type = TB_I8;
	
	if (is_value_mem(&v)) {
		if (v.mem.is_rvalue) {
			if (v.is_owned && v.mem.index == GPR_NONE) {
				// recycle
				Val tmp = val_gpr(TB_PTR, v.mem.base);
				inst2(ctx, MOV, &tmp, &v, TB_PTR);
				
				v = val_base_disp(TB_TYPE_PTR, tmp.gpr, 0);
				v.is_owned = true;
				return v;
			} else {
				Val tmp = alloc_gpr(ctx, f, TB_PTR, 0);
				inst2(ctx, MOV, &tmp, &v, TB_PTR);
				
				v = val_base_disp(TB_TYPE_PTR, tmp.gpr, 0);
				v.is_owned = true;
				return v;
			}
		}
	} else if (v.type == VAL_GPR) {
		return val_base_disp(TB_TYPE_PTR, v.gpr, 0);
	} else if (v.type == VAL_IMM) {
		Val tmp = alloc_gpr(ctx, f, TB_PTR, 0);
		
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
			
			Val new_v = alloc_gpr(ctx, f, v.dt.type, 0);
			inst2(ctx, LEA, &new_v, &v, TB_PTR);
			free_val(ctx, f, v);
			return new_v;
		} else if (!v.mem.is_rvalue) {
			if (v.dt.width || TB_IS_FLOAT_TYPE(v.dt.type)) {
				Val new_v = alloc_xmm(ctx, f, v.dt);
				
				uint8_t flags = 0;
				flags |= (v.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (v.dt.width) ? INST2FP_PACKED : 0;
				
				inst2sse(ctx, FP_MOV, &new_v, &v, flags);
				return new_v;
			} else {
				Val tmp = alloc_gpr(ctx, f, v.dt.type, 0);
				inst2(ctx, MOV, &tmp, &v, v.dt.type);
				return tmp;
			}
		}
	} else if (v.type == VAL_FLAGS) {
		assert(0 && "No fucking way bro");
	}
	
	return v;
}
