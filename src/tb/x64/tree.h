// we track use counts to know when things should die
//
// i = load     i2
// j = i + 1    i1 j1
// store j      i1 j0    kill j
// store i      i0       kill i

static Val gen_float_const(Ctx* ctx, TB_Function* f, TB_Reg r, double float_value, TB_DataType dt) {
	// Unlike integers, there's no float immediates
	Val v = alloc_xmm(ctx, f, r, dt);
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
			
			disp = tb_emit_const_patch(f->module, s_compiled_func_id, code_pos(), rdata_payload, sizeof(uint64_t), s_local_thread_id);
		} else {
			uint32_t imm32 = (Cvt_F32U32){ .f = float_value }.i;
			
			uint32_t* rdata_payload = tb_platform_arena_alloc(sizeof(uint32_t));
			*rdata_payload = imm32;
			
			disp = tb_emit_const_patch(f->module, s_compiled_func_id, code_pos(), rdata_payload, sizeof(uint32_t), s_local_thread_id);
		}
		
		emit4(disp);
	}
	
	return v;
}

static Val eval(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
	assert(ctx->use_count[r] > 0);
	ctx->use_count[r] -= 1;
	
	assert(ctx->values[r].type);
	return ctx->values[r];
}

#if 0
static Val eval(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
	if (ctx->values[r].type != VAL_NONE) {
		if (ctx->is_tallying) {
			if (ctx->use_count[r] == 0) {
				if (!should_rematerialize(f->nodes.data[r].type)) {
					tb_function_print(f, tb_default_print_callback, stdout);
					printf("\n\n\n");
					assert(0);
				}
			}
		}
		
	}
	
	TB_Node* restrict n = &f->nodes.data[r];
	TB_NodeTypeEnum reg_type = n->type;
	TB_DataType dt = n->dt;
	
	Val val = { 0 };
	switch (reg_type) {
		case TB_ARRAY_ACCESS: {
			Val base = eval_addressof(ctx, f, n->array_access.base);
			Val index = eval_rvalue(ctx, f, n->array_access.index);
			
			uint32_t stride = n->array_access.stride;
			
			if (base.type == VAL_GLOBAL) {
				Val new_base = alloc_gpr(ctx, f, r, TB_PTR);
				
				inst2(ctx, LEA, &new_base, &base, TB_PTR);
				
				base = val_base_disp(dt, new_base.gpr, 0);
				base.is_temp = true;
			}
			
			// move into a GPR to make life easier
			bool can_recycle_index = ctx->use_count[n->array_access.index] == 0;
			if (is_value_mem(&index)) {
				Val new_index = alloc_gpr(ctx, f, r, TB_PTR);
				inst2(ctx, MOV, &new_index, &index, TB_PTR);
				
				kill(ctx, f, n->array_access.index, index);
				
				index = new_index;
				index.is_temp = true;
				
				can_recycle_index = true;
			}
			
			// TODO(NeGate): Redo this code, it's scary levels of branchy
			if (index.type == VAL_IMM) {
				base.mem.disp += index.imm * stride;
				
				val = base;
			} else if (index.type == VAL_GPR) {
				GPR base_reg = base.mem.base;
				int32_t disp = base.mem.disp;
				
				// use index as dst
				if (tb_is_power_of_two(stride)) {
					uint8_t stride_as_shift = tb_ffs(stride) - 1;
					
					if (stride_as_shift <= 3) {
						// it can use the shift in the memory operand
						if (base.mem.index != GPR_NONE) {
							// nested indices a[x][y]
							// lea dst, [base + index0 * scale0 + offset]
							// lea dst, [dst + index1 * scale1] or add dst, index1
							val = alloc_gpr(ctx, f, r, dt.type);
							inst2(ctx, LEA, &val, &base, TB_PTR);
							
							if (stride_as_shift) {
								Val addr = val_base_index(dt, val.gpr, index.gpr, stride_as_shift);
								inst2(ctx, LEA, &val, &addr, TB_PTR);
							} else {
								Val idx = val_gpr(TB_PTR, index.gpr);
								inst2(ctx, ADD, &val, &idx, TB_PTR);
							}
							
							val = val_base_disp(dt, val.gpr, 0);
						} else {
							// single index a[x] with a small power of two
							val = val_base_index_disp(dt, base_reg, index.gpr, stride_as_shift, disp);
						}
					} else {
						if (!can_recycle_index) {
							Val new_index = alloc_gpr(ctx, f, r, dt.type);
							inst2(ctx, MOV, &new_index, &index, index.dt.type);
							
							index = new_index;
							index.is_temp = true;
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
					val = alloc_gpr(ctx, f, r, dt.type);
					
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
			
			Val a = eval_rvalue(ctx, f, n->f_arith.a);
			Val b = eval_rvalue(ctx, f, n->f_arith.b);
			
			uint8_t flags = 0;
			flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
			flags |= (dt.width) ? INST2FP_PACKED : 0;
			
			bool recycled = ctx->use_count[n->i_arith.a] == 0 && a.type == VAL_XMM;
			if (recycled) {
				val = a;
			} else {
				val = alloc_xmm(ctx, f, r, dt);
				inst2sse(ctx, FP_MOV, &val, &a, flags);
			}
			
			Inst2FPType op = tbl[reg_type - TB_FADD];
			inst2sse(ctx, op, &val, &b, flags);
			
			kill(ctx, f, n->f_arith.a, a);
			kill(ctx, f, n->f_arith.a, b);
			break;
		}
		
		case TB_SHR:
		case TB_SHL:
		case TB_SAR: {
			Val a = eval_rvalue(ctx, f, n->i_arith.a);
			Val b = eval_rvalue(ctx, f, n->i_arith.b);
			
			if (a.type == VAL_IMM && b.type == VAL_IMM) {
				val = val_imm(dt, (uint64_t)a.imm << (uint64_t)b.imm);
				break;
			}
			
			bool recycled = ctx->use_count[n->i_arith.a] == 0 &&
				a.type == VAL_GPR;
			
			if (recycled) {
				val = a;
			} else {
				val = alloc_gpr(ctx, f, r, dt.type);
				inst2(ctx, MOV, &val, &a, dt.type);
			}
			
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
			
			Val rcx = val_gpr(dt.type, RCX);
			if (!is_value_gpr(&b, RCX)) {
				evict_gpr(ctx, f, RCX);
				inst2(ctx, MOV, &rcx, &b, dt.type);
			}
			
			// D2 /4       shl r/m, cl
			// D2 /5       shr r/m, cl
			// D2 /7       sar r/m, cl
			if (dt.type == TB_I16) emit(0x66);
			emit(rex(is_64bit, 0x00, val.gpr, 0x00));
			emit(dt.type == TB_I8 ? 0xD2 : 0xD3);
			
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
			
			kill(ctx, f, n->i_arith.a, a);
			kill(ctx, f, n->i_arith.a, b);
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
			
			Val a = eval_rvalue(ctx, f, n->i_arith.a);
			Val b = eval_rvalue(ctx, f, n->i_arith.b);
			
			// needs to mov the a value into rdx:rax
			Val rax = val_gpr(dt.type, RAX);
			if (!is_value_gpr(&a, RAX)) {
				evict_gpr(ctx, f, RAX);
				inst2(ctx, MOV, &rax, &a, dt.type);
			}
			ctx->gpr_allocator[RAX] = TB_TEMP_REG;
			ctx->gpr_allocator[RDX] = TB_TEMP_REG;
			
			if (is_signed) {
				// cqo/cdq
				if (dt.type == TB_PTR || dt.type == TB_I64) emit(0x48);
				
				emit(0x99);
			} else {
				// xor rdx, rdx
				emit(0x31);
				emit(mod_rx_rm(MOD_DIRECT, RDX, RDX));
			}
			
			if (b.type == VAL_IMM) {
				Val tmp = alloc_gpr(ctx, f, TB_TEMP_REG, dt.type);
				
				inst2(ctx, MOV, &tmp, &b, dt.type);
				inst1(ctx, IDIV, &tmp);
				
				free_gpr(ctx, f, tmp.gpr);
			} else {
				inst1(ctx, IDIV, &b);
			}
			
			kill(ctx, f, n->i_arith.a, a);
			kill(ctx, f, n->i_arith.a, b);
			
			// the return value is in RAX for division
			// and RDX for modulo
			val = val_gpr(dt.type, is_div ? RAX : RDX);
			
			// free the other piece of the divmod result
			ctx->gpr_allocator[is_div ? RDX : RAX] = TB_NULL_REG;
			break;
		}
		
		case TB_X86INTRIN_SQRT:
		case TB_X86INTRIN_RSQRT: {
			Val src = eval_rvalue(ctx, f, n->unary.src);
			
			bool recycled = ctx->use_count[n->unary.src] == 0 &&
				!is_value_mem(&src);
			
			if (recycled) {
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				inst2sse(ctx, reg_type == TB_X86INTRIN_SQRT ? FP_SQRT : FP_RSQRT, &src, &src, flags);
				
				val = src;
			} else {
				val = alloc_xmm(ctx, f, r, dt);
				
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				inst2sse(ctx, reg_type == TB_X86INTRIN_SQRT ? FP_SQRT : FP_RSQRT, &val, &src, flags);
			}
			
			kill(ctx, f, n->unary.src, src);
			break;
		}
		
		case TB_FLOAT_EXT: {
			Val src = eval_rvalue(ctx, f, n->unary.src);
			val = alloc_xmm(ctx, f, r, dt);
			
			uint8_t flags = 0;
			flags |= (src.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
			flags |= (src.dt.width) ? INST2FP_PACKED : 0;
			inst2sse(ctx, FP_CVT, &val, &src, flags);
			
			kill(ctx, f, n->unary.src, src);
			break;
		}
		
		default:
		assert(0 && "TODO: Implement node eval");
		break;
	}
	
	if (ctx->is_tallying) {
		if (ctx->use_count[r] == 0) {
			if (!should_rematerialize(reg_type)) {
				tb_function_print(f, tb_default_print_callback, stdout);
				printf("\n\n\n");
				assert(0);
			}
			
			ctx->use_count[r] -= 1;
		}
	}
	
	// it's no longer a temporary if it's here
	if (val.type == VAL_GPR) {
		ctx->gpr_allocator[val.gpr] = r;
	} else if (val.type == VAL_XMM) {
		ctx->xmm_allocator[val.xmm] = r;
	} else if (val.type == VAL_MEM) {
		if (val.mem.base != RSP && val.mem.base != RBP) {
			assert(val.mem.base != GPR_NONE);
			ctx->gpr_allocator[val.mem.base] = r;
		}
		
		if (val.mem.index != GPR_NONE) {
			ctx->gpr_allocator[val.mem.index] = r;
		}
	}
	
	val.is_temp = false;
	ctx->values[r] = val;
	
	return val;
}

static Val eval_addressof(Ctx* ctx, TB_Function* f, TB_Reg r) {
	Val v = eval(ctx, f, r, false);
	if (v.dt.type == TB_BOOL) v.dt.type = TB_I8;
	
	if (is_value_mem(&v)) {
		if (v.mem.is_rvalue) {
			if (ctx->use_count[r] == 0 &&
				v.mem.index == GPR_NONE &&
				v.mem.base != RSP &&
				v.mem.base != RBP) {
				// recycle
				Val tmp = val_gpr(TB_PTR, v.mem.base);
				inst2(ctx, MOV, &tmp, &v, TB_PTR);
				
				v = val_base_disp(TB_TYPE_PTR, tmp.gpr, 0);
			} else {
				Val tmp = alloc_gpr(ctx, f, TB_TEMP_REG, TB_PTR);
				inst2(ctx, MOV, &tmp, &v, TB_PTR);
				
				v = val_base_disp(TB_TYPE_PTR, tmp.gpr, 0);
			}
		}
	} else if (v.type == VAL_GPR) {
		v = val_base_disp(TB_TYPE_PTR, v.gpr, 0);
	} else if (v.type == VAL_IMM) {
		Val tmp = alloc_gpr(ctx, f, TB_TEMP_REG, TB_PTR);
		
		// mov reg64, imm64
		emit(rex(true, 0x0, tmp.gpr, 0));
		emit(0xB8 + (tmp.gpr & 0b111));
		emit8(v.imm);
		
		v = val_base_disp(TB_TYPE_PTR, tmp.gpr, 0);
	} else tb_todo();
	
	v.is_temp = true;
	return v;
}

static Val eval_rvalue(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
	Val v = eval(ctx, f, r, false);
	if (v.dt.type == TB_BOOL) v.dt.type = TB_I8;
	
	if (is_value_mem(&v)) {
		if (is_address_node(f->nodes.data[r].type)) {
			assert(!v.mem.is_rvalue);
			
			Val new_v = alloc_gpr(ctx, f, TB_TEMP_REG, v.dt.type);
			new_v.is_temp = true;
			
			inst2(ctx, LEA, &new_v, &v, TB_PTR);
			kill(ctx, f, r);
			
			v = new_v;
		} else if (!v.mem.is_rvalue) {
			if (v.dt.width || TB_IS_FLOAT_TYPE(v.dt.type)) {
				Val new_v = alloc_xmm(ctx, f, TB_TEMP_REG, v.dt);
				
				uint8_t flags = 0;
				flags |= (v.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (v.dt.width) ? INST2FP_PACKED : 0;
				
				inst2sse(ctx, FP_MOV, &new_v, &v, flags);
				kill(ctx, f, r, v);
				
				v = new_v;
			} else {
				Val new_v = alloc_gpr(ctx, f, TB_TEMP_REG, v.dt.type);
				
				inst2(ctx, MOV, &new_v, &v, v.dt.type);
				kill(ctx, f, r, v);
				
				v = new_v;
			}
		}
	} else if (v.type == VAL_FLAGS) {
		//assert(0 && "No fucking way bro");
	}
	
	v.is_temp = true;
	return v;
}
#endif
