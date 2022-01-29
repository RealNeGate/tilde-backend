// we track use counts to know when things should die
//
// i = load     i2
// j = i + 1    i1 j1
// store j      i1 j0    kill j
// store i      i0       kill i

#define done_with(r) if (ctx->use_count[r] == 0) { \
Val v = ctx->values[r]; \
if (v.type == VAL_GPR) { \
ctx->gpr_allocator &= ~(1u << v.gpr); \
ctx->values[r] = (Val){ 0 }; \
} else if (v.type == VAL_XMM) { \
ctx->xmm_allocator &= ~(1u << v.xmm); \
ctx->values[r] = (Val){ 0 }; \
} \
}

static void spill_reg(Ctx* restrict ctx, TB_Function* f, TB_Register r) {
	TB_DataType dt = f->nodes.dt[r];
	Val src = ctx->values[r];
	
	if (ctx->values[r].type == VAL_GPR) {
		Val dst;
		if (r < f->prototype->param_count) {
			dst = val_stack(dt, 16 + ((r - TB_FIRST_PARAMETER_REG) * 8));
		} else {
			int size = get_data_type_size(dt);
			ctx->stack_usage = align_up(ctx->stack_usage + size, size);
			dst = val_stack(dt, -ctx->stack_usage);
		}
		
		if (dt.type == TB_BOOL) dt.type = TB_I8;
		
		inst2(ctx, MOV, &dst, &src, dt.type);
		ctx->values[r] = dst;
		ctx->gpr_allocator &= ~(1u << src.gpr);
	} else if (ctx->values[r].type == VAL_XMM) {
		Val dst;
		if (r < f->prototype->param_count) {
			dst = val_stack(dt, 16 + ((r - TB_FIRST_PARAMETER_REG) * 8));
		} else {
			int size = get_data_type_size(dt);
			ctx->stack_usage = align_up(ctx->stack_usage + size, size);
			dst = val_stack(dt, -ctx->stack_usage);
		}
		
		uint8_t flags = 0;
		flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
		flags |= (dt.width) ? INST2FP_PACKED : 0;
		
		inst2sse(ctx, FP_MOV, &dst, &src, flags);
		ctx->values[r] = dst;
		ctx->xmm_allocator &= ~(1u << src.xmm);
	}
}

static Val alloc_gpr(Ctx* restrict ctx, TB_Function* f, int dt_type) {
	if (ctx->gpr_allocator == 0xFFFF) {
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
					.is_temp = true,
					.dt.width = 0,
					.dt.type = dt_type,
					.gpr = src.gpr
				};
			}
		}
		
		loop_reverse(i, f->nodes.count) if (ctx->values[i].type == VAL_GPR) {
			GPR gpr = ctx->values[i].gpr;
			
			spill_reg(ctx, f, i);
			
			ctx->gpr_allocator |= (1u << gpr);
			return (Val) {
				.type = VAL_GPR,
				.is_temp = true,
				.dt.width = 0,
				.dt.type = dt_type,
				.gpr = gpr
			};
		}
		
		tb_function_print(f, tb_default_print_callback, stdout);
		tb_panic(0, "Failed to allocate a GPR");
	}
	
	int search = tb_ffs(~ctx->gpr_allocator);
	assert(search != 0);
	
	GPR gpr = (GPR)(search-1);
	ctx->gpr_allocator |= (1u << gpr);
	
	// mark register as to be saved
	ctx->regs_to_save |= (1u << gpr) & (ctx->is_sysv ? SYSV_ABI_CALLEE_SAVED : WIN64_ABI_CALLEE_SAVED);
	
	return (Val) {
		.type = VAL_GPR,
		.is_temp = true,
		.dt.width = 0,
		.dt.type = dt_type,
		.gpr = gpr
	};
}

static Val alloc_xmm(Ctx* restrict ctx, TB_Function* f, TB_DataType dt) {
	if (ctx->xmm_allocator == 0xFFFF) {
		loop(i, ctx->phi_count) {
			PhiValue* restrict phi = &ctx->phis[i];
			if (phi->value.type == VAL_XMM) {
				TB_DataType dt = f->nodes.dt[phi->reg];
				if (dt.type == TB_BOOL) dt.type = TB_I8;
				
				if (phi->spill == 0) {
					int size = get_data_type_size(dt);
					ctx->stack_usage = align_up(ctx->stack_usage + size, size);
					
					phi->spill = -ctx->stack_usage;
				}
				
				Val dst = val_stack(dt, phi->spill);
				Val src = val_xmm(dt, phi->value.xmm);
				
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				
				inst2sse(ctx, FP_MOV, &dst, &src, flags);
				src.is_temp = true;
				return src;
			}
		}
		
		loop_reverse(i, f->nodes.count) if (ctx->values[i].type == VAL_XMM) {
			XMM xmm = ctx->values[i].xmm;
			
			spill_reg(ctx, f, i);
			
			ctx->xmm_allocator |= (1u << xmm);
			return (Val) {
				.type = VAL_XMM,
				.is_temp = true,
				.dt = dt,
				.xmm = xmm
			};
		}
		
		tb_panic(0, "Failed to allocate a XMM");
	}
	
	int search = tb_ffs(~ctx->xmm_allocator);
	
	XMM xmm = (XMM)(search-1);
	ctx->xmm_allocator |= (1u << xmm);
	
	// callee saves
	if (ctx->is_sysv) {
		// TODO(NeGate): What does SysV say?
	} else if (xmm > 5) {
		ctx->regs_to_save |= (1u << (16 + xmm));
	}
	
	return (Val) {
		.type = VAL_XMM,
		.is_temp = true,
		.dt = dt,
		.xmm = xmm
	};
}

static void free_gpr(Ctx* restrict ctx, TB_Function* f, GPR gpr) {
	ctx->gpr_allocator &= ~(1u << gpr);
}

static void free_xmm(Ctx* restrict ctx, TB_Function* f, XMM xmm) {
	ctx->xmm_allocator &= ~(1u << xmm);
}

static void free_val(Ctx* restrict ctx, TB_Function* f, Val val) {
	if (val.is_temp) {
		if (val.type == VAL_GPR) {
			ctx->gpr_allocator &= ~(1u << val.gpr);
		} else if (val.type == VAL_XMM) {
			ctx->xmm_allocator &= ~(1u << val.xmm);
		} else if (val.type == VAL_MEM) {
			assert(val.mem.base != RBP && val.mem.base != RSP);
			assert(val.mem.index == GPR_NONE);
			ctx->gpr_allocator &= ~(1u << val.mem.base);
		}
	}
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

static Val eval(Ctx* restrict ctx, TB_Function* f, TB_Register r) {
	if (ctx->values[r].type != VAL_NONE) {
		assert(ctx->use_count[r] > 0);
		ctx->use_count[r] -= 1;
		return ctx->values[r];
	}
	
	TB_RegTypeEnum reg_type = f->nodes.type[r];
	TB_DataType dt = f->nodes.dt[r];
	TB_RegPayload* restrict p = &f->nodes.payload[r];
	
	Val val = { 0 };
	switch (reg_type) {
		case TB_SIGNED_CONST:
		case TB_UNSIGNED_CONST: {
			int32_t imm32 = (int32_t)p->s_const;
			if (p->s_const == imm32) {
				val = val_imm(dt, imm32);
				break;
			}
			
			// explicit mov
			val = alloc_gpr(ctx, f, dt.type);
			
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
			
			val = alloc_gpr(ctx, f, dt.type);
			
			emit(rex(true, val.gpr, RBP, 0));
			emit(0x8D);
			emit(mod_rx_rm(MOD_INDIRECT, val.gpr, RBP));
			
			uint32_t disp = tb_emit_const_patch(f->module, s_compiled_func_id, code_pos(), str, len, s_local_thread_id);
			emit4(disp);
			break;
		}
		case TB_GLOBAL_ADDRESS: {
			val = val_global(p->global_addr);
			break;
		}
		case TB_EFUNC_ADDRESS:
		case TB_FUNC_ADDRESS: {
			val = alloc_gpr(ctx, f, dt.type);
			
			emit(rex(true, val.gpr, RBP, 0));
			emit(0x8D);
			emit(mod_rx_rm(MOD_INDIRECT, val.gpr, RBP));
			emit4(0x0);
			
			if (reg_type == TB_EFUNC_ADDRESS) {
				tb_emit_ecall_patch(f->module, s_compiled_func_id, p->efunc_addr, code_pos() - 4, s_local_thread_id);
			} else {
				int target_func = p->func_addr - f->module->functions.data;
				tb_emit_call_patch(f->module, s_compiled_func_id, target_func, code_pos() - 4, s_local_thread_id);
			}
			break;
		}
		case TB_LOAD: {
			val = eval_addressof(ctx, f, p->load.address);
			
			val.dt = dt;
			val.mem.is_rvalue = true;
			
			done_with(p->load.address);
			break;
		}
		case TB_PARAM_ADDR: {
			TB_Register param = p->param_addr.param;
			val = eval_rvalue(ctx, f, param);
			
			done_with(param);
			break;
		}
		case TB_RESTRICT: {
			val = eval_addressof(ctx, f, p->restrict_);
			break;
		}
		case TB_ARRAY_ACCESS: {
			Val base = eval_addressof(ctx, f, p->array_access.base);
			Val index = eval_rvalue(ctx, f, p->array_access.index);
			
			uint32_t stride = p->array_access.stride;
			
			// move into a GPR to make life easier
			bool can_recycle_index = ctx->use_count[p->array_access.index] == 0;
			if (is_value_mem(&index)) {
				Val new_index = alloc_gpr(ctx, f, dt.type);
				inst2(ctx, MOV, &new_index, &index, index.dt.type);
				free_val(ctx, f, index);
				
				index = new_index;
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
				bool recycle = false;
				
				if (tb_is_power_of_two(stride)) {
					uint8_t stride_as_shift = tb_ffs(stride) - 1;
					
					if (stride_as_shift <= 3) {
						// it can use the shift in the memory operand
						if (base.mem.index != GPR_NONE) {
							// nested indices a[x][y]
							// lea dst, [base + index0 * scale0 + offset]
							// lea dst, [dst + index1 * scale1] or add dst, index1
							val = alloc_gpr(ctx, f, dt.type);
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
						if (can_recycle_index) {
							Val new_index = alloc_gpr(ctx, f, dt.type);
							inst2(ctx, MOV, &new_index, &index, index.dt.type);
							index = new_index;
							
							recycle = true;
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
					val = alloc_gpr(ctx, f, dt.type);
					
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
			
			done_with(p->array_access.base);
			done_with(p->array_access.index);
			break;
		}
		case TB_MEMBER_ACCESS: {
			val = eval_addressof(ctx, f, p->member_access.base);
			val.mem.disp += p->member_access.offset;
			
			done_with(p->member_access.base);
			break;
		}
		
		case TB_NEG: {
			Val src = eval_rvalue(ctx, f, p->unary.src);
			
			if (dt.type == TB_F64) {
				// .LCPI0_0:
				//   .quad   0x8000000000000000
				//   .quad   0x8000000000000000
				// ...
				// xorps   xmm0, xmmword ptr [rip + .LCPI0_0]
				val = alloc_xmm(ctx, f, dt);
				XMM dst_xmm = val.xmm;
				
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				inst2sse(ctx, FP_MOV, &val, &src, flags);
				
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
				
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				inst2sse(ctx, FP_MOV, &val, &src, flags);
				
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
				bool recycle = ctx->use_count[p->unary.src] == 0 && src.type == VAL_GPR;
				
				if (recycle) {
					inst1(ctx, NEG, &src);
					
					val = src;
					val.is_temp = true;
				} else {
					val = alloc_gpr(ctx, f, dt.type);
					
					inst2(ctx, MOV, &val, &src, dt.type);
					inst1(ctx, NEG, &val);
					
					free_val(ctx, f, src);
				}
			}
			
			done_with(p->unary.src);
			break;
		}
		case TB_NOT: {
			Val src = eval_rvalue(ctx, f, p->unary.src);
			
			bool recycled = ctx->use_count[p->unary.src] == 0 &&
				src.type != VAL_MEM;
			
			if (recycled) {
				val = src;
				val.is_temp = true;
				
				inst1(ctx, NOT, &src);
				break;
			} else {
				val = alloc_gpr(ctx, f, dt.type);
				
				inst2(ctx, MOV, &val, &src, dt.type);
				inst1(ctx, NOT, &val);
				
				free_val(ctx, f, src);
			}
			
			done_with(p->unary.src);
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
			
			Val a = eval_rvalue(ctx, f, p->f_arith.a);
			Val b = eval_rvalue(ctx, f, p->f_arith.b);
			
			uint8_t flags = 0;
			flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
			flags |= (dt.width) ? INST2FP_PACKED : 0;
			
			bool recycle = ctx->use_count[p->i_arith.a] == 0 && a.type == VAL_XMM;
			if (recycle) {
				val = a;
				val.is_temp = true;
			} else {
				val = alloc_xmm(ctx, f, dt);
				inst2sse(ctx, FP_MOV, &val, &a, flags);
				free_val(ctx, f, a);
			}
			
			Inst2FPType op = tbl[reg_type - TB_FADD];
			inst2sse(ctx, op, &val, &b, flags);
			
			free_val(ctx, f, b);
			done_with(p->f_arith.a);
			done_with(p->f_arith.b);
			break;
		}
		
		// Integer binary operations
		case TB_AND:
		case TB_OR:
		case TB_XOR:
		case TB_ADD:
		case TB_SUB:
		case TB_MUL: {
			Val a = eval_rvalue(ctx, f, p->i_arith.a);
			Val b = eval_rvalue(ctx, f, p->i_arith.b);
			
			if (dt.width == 0) {
				// simple scalar ops
				const static Inst2Type ops[] = { AND, OR, XOR, ADD, SUB, IMUL };
				
				if (dt.type == TB_BOOL) dt.type = TB_I8;
				
				bool recycle = ctx->use_count[p->i_arith.a] == 0 &&
					!(is_value_mem(&a) && reg_type == TB_MUL);
				
				if (recycle) {
					val = a;
					val.is_temp = true;
				} else {
					val = alloc_gpr(ctx, f, dt.type);
					inst2(ctx, MOV, &val, &a, dt.type);
					free_val(ctx, f, a);
				}
				
				// we can't do a OP mem, mem
				// and imul doesn't support a simple OP r/m, imm mode
				// we'll need a temporary in those cases
				bool is_mem_dst = is_value_mem(&val);
				
				bool needs_temporary = is_mem_dst && is_value_mem(&b);
				needs_temporary |= (reg_type == TB_MUL && (is_mem_dst || b.type == VAL_IMM));
				
				if (needs_temporary) {
					Val tmp = alloc_gpr(ctx, f, dt.type);
					
					inst2(ctx, MOV, &tmp, &b, dt.type);
					inst2(ctx, ops[reg_type - TB_AND], &val, &tmp, dt.type);
					
					free_gpr(ctx, f, tmp.gpr);
				} else {
					inst2(ctx, ops[reg_type - TB_AND], &val, &b, dt.type);
				}
				free_val(ctx, f, b);
			} else {
				// supported modes (for now)
				assert(dt.width <= 2);
				
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				
				bool recycle = ctx->use_count[p->i_arith.a] == 0 &&
					a.type == VAL_XMM;
				
				if (recycle) {
					val = a;
				} else {
					val = alloc_xmm(ctx, f, dt);
					inst2sse(ctx, FP_MOV, &val, &a, flags);
					free_val(ctx, f, a);
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
				
				free_val(ctx, f, b);
			}
			
			done_with(p->i_arith.a);
			done_with(p->i_arith.b);
			break;
		}
		
		case TB_SHR:
		case TB_SHL:
		case TB_SAR: {
			Val a = eval_rvalue(ctx, f, p->i_arith.a);
			Val b = eval_rvalue(ctx, f, p->i_arith.b);
			
			if (a.type == VAL_IMM && b.type == VAL_IMM) {
				val = val_imm(dt, a.imm << b.imm);
				break;
			}
			
			bool recycle = ctx->use_count[p->i_arith.a] == 0 &&
				a.type == VAL_GPR;
			
			if (recycle) {
				val = a;
			} else {
				val = alloc_gpr(ctx, f, dt.type);
				inst2(ctx, MOV, &val, &a, dt.type);
				free_val(ctx, f, a);
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
			free_val(ctx, f, b);
			
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
			
			done_with(p->i_arith.a);
			done_with(p->i_arith.b);
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
			
			evict_gpr(ctx, f, RAX);
			evict_gpr(ctx, f, RDX);
			
			// reserve
			ctx->gpr_allocator |= (1u << RAX);
			ctx->gpr_allocator |= (1u << RDX);
			
			Val a = eval_rvalue(ctx, f, p->i_arith.a);
			Val b = eval_rvalue(ctx, f, p->i_arith.b);
			
			// needs to mov the a value into rdx:rax
			Val rax = val_gpr(dt.type, RAX);
			if (!is_value_gpr(&a, RAX)) {
				inst2(ctx, MOV, &rax, &a, dt.type);
			}
			free_val(ctx, f, a);
			
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
				Val tmp = alloc_gpr(ctx, f, dt.type);
				
				inst2(ctx, MOV, &tmp, &b, dt.type);
				inst1(ctx, IDIV, &tmp);
				
				free_gpr(ctx, f, tmp.gpr);
			} else {
				inst1(ctx, IDIV, &b);
			}
			free_val(ctx, f, b);
			
			// the return value is in RAX for division
			// and RDX for modulo
			val = val_gpr(dt.type, is_div ? RAX : RDX);
			val.is_temp = true;
			
			// free the other piece of the divmod result
			ctx->gpr_allocator &= ~(1u << (is_div ? RDX : RAX));
			
			done_with(p->i_arith.a);
			done_with(p->i_arith.b);
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
			
			Val a = eval_rvalue(ctx, f, p->cmp.a);
			Val b = eval_rvalue(ctx, f, p->cmp.b);
			
			bool convert_to_reg = true;//!(n == (ctx->tree_len-1) && f->nodes.type[ctx->next_reg] == TB_IF);
			if (convert_to_reg) {
				val = alloc_gpr(ctx, f, TB_I8);
				
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
					
					free_xmm(ctx, f, tmp.xmm);
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
					Val tmp = alloc_gpr(ctx, f, cmp_dt.type);
					
					inst2(ctx, MOV, &tmp, &a, cmp_dt.type);
					inst2(ctx, CMP, &tmp, &b, cmp_dt.type);
					
					free_gpr(ctx, f, tmp.gpr);
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
				
				if (reg_type != TB_CMP_EQ && reg_type != TB_CMP_NE) {
					cc ^= invert;
				}
			}
			
			if (convert_to_reg) {
				// setcc v
				if (val.gpr >= 8) emit(rex(true, val.gpr, val.gpr, 0));
				emit(0x0F);
				emit(0x90 + cc);
				emit(mod_rx_rm(MOD_DIRECT, val.gpr, val.gpr));
			} else {
				val = val_flags(cc);
			}
			
			done_with(p->cmp.a);
			done_with(p->cmp.b);
			break;
		}
		
		case TB_X86INTRIN_SQRT:
		case TB_X86INTRIN_RSQRT: {
			Val src = eval_rvalue(ctx, f, p->unary.src);
			
			bool recycled = ctx->use_count[p->unary.src] == 0 &&
				!is_value_mem(&src);
			
			if (recycled) {
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				inst2sse(ctx, reg_type == TB_X86INTRIN_SQRT ? FP_SQRT : FP_RSQRT, &src, &src, flags);
				
				val = src;
				val.is_temp = true;
			} else {
				val = alloc_xmm(ctx, f, dt);
				
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				inst2sse(ctx, reg_type == TB_X86INTRIN_SQRT ? FP_SQRT : FP_RSQRT, &val, &src, flags);
				
				free_val(ctx, f, src);
			}
			
			done_with(p->unary.src);
			break;
		}
		
		case TB_FLOAT_EXT: {
			Val src = eval_rvalue(ctx, f, p->unary.src);
			val = alloc_xmm(ctx, f, dt);
			
			uint8_t flags = 0;
			flags |= (src.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
			flags |= (src.dt.width) ? INST2FP_PACKED : 0;
			inst2sse(ctx, FP_CVT, &val, &src, flags);
			
			free_val(ctx, f, src);
			done_with(p->unary.src);
			break;
		}
		
		case TB_PHI2: {
			PhiValue* phi = find_phi(ctx, r);
			assert(phi && "PHI node not initialized but used");
			
			val = phi->value;
			break;
		}
		
		case TB_FLOAT2INT: {
			assert(dt.width == 0 && "TODO: Implement vector float2int");
			
			Val src = eval_rvalue(ctx, f, p->unary.src);
			assert(src.type == VAL_MEM || src.type == VAL_GLOBAL || src.type == VAL_XMM);
			
			Val v = alloc_gpr(ctx, f, dt.type);
			
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
			
			Val src = eval_rvalue(ctx, f, p->unary.src);
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
			
			done_with(p->unary.src);
			break;
		}
		
		case TB_TRUNCATE: {
			Val src = eval_rvalue(ctx, f, p->unary.src);
			
			if (TB_IS_FLOAT_TYPE(dt.type)) {
				val = alloc_xmm(ctx, f, dt);
				
				uint8_t flags = 0;
				flags |= (src.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (src.dt.width) ? INST2FP_PACKED : 0;
				inst2sse(ctx, FP_CVT, &val, &src, flags);
			} else {
				if (src.type == VAL_IMM) {
					src.dt = dt;
					val = src;
					
					done_with(p->unary.src);
					break;
				}
				
				bool recycle = ctx->use_count[p->unary.src] == 0 &&
					src.type == VAL_GPR;
				
				if (recycle) {
					val = src;
					val.is_temp = true;
				} else {
					val = alloc_gpr(ctx, f, dt.type);
					free_val(ctx, f, src);
				}
				
				if (dt.type == TB_I16) {
					inst2(ctx, MOVZXW, &val, &src, TB_I16);
				} else if (dt.type == TB_I8 || dt.type == TB_BOOL) {
					inst2(ctx, MOVZXB, &val, &src, TB_I8);
				} else if (dt.type == TB_I32) {
					// this forces the high 32bits to be cleared
					inst2(ctx, MOV, &val, &src, TB_I32);
				} else {
					if (!recycle) inst2(ctx, MOV, &val, &src, src.dt.type);
				}
			}
			
			done_with(p->unary.src);
			break;
		}
		
		case TB_PTR2INT: {
			Val src = eval_rvalue(ctx, f, p->unary.src);
			
			bool recycle = ctx->use_count[p->unary.src] == 0 &&
				src.type == VAL_GPR;
			
			if (recycle) {
				val = src;
				val.is_temp = true;
			} else {
				val = alloc_gpr(ctx, f, dt.type);
				free_val(ctx, f, src);
			}
			
			if (dt.type == TB_I16) {
				inst2(ctx, MOVZXW, &val, &src, TB_I16);
			} else if (dt.type == TB_I8 || dt.type == TB_BOOL) {
				inst2(ctx, MOVZXB, &val, &src, TB_I8);
			} else {
				inst2(ctx, MOV, &val, &src, src.dt.type);
			}
			
			done_with(p->unary.src);
			break;
		}
		case TB_INT2PTR:
		case TB_SIGN_EXT:
		case TB_ZERO_EXT: {
			assert(dt.width == 0 && "TODO: Implement vector zero extend");
			bool sign_ext = (reg_type == TB_SIGN_EXT);
			
			Val src = eval_rvalue(ctx, f, p->unary.src);
			if (src.type == VAL_IMM) {
				src.dt = dt;
				val = src;
				
				done_with(p->unary.src);
				break;
			}
			
			bool recycle = ctx->use_count[p->unary.src] == 0 &&
				src.type != VAL_MEM;
			
			if (recycle) {
				val = src;
				val.is_temp = true;
			} else {
				val = alloc_gpr(ctx, f, dt.type);
				free_val(ctx, f, src);
			}
			
			if (src.dt.type == TB_I32 && sign_ext) {
				inst2(ctx, MOVSXD, &val, &src, dt.type);
			} else if (src.dt.type == TB_I16) {
				inst2(ctx, sign_ext ? MOVSXW : MOVZXW, &val, &src, dt.type);
			} else if (src.dt.type == TB_I8 || src.dt.type == TB_BOOL) {
				inst2(ctx, sign_ext ? MOVSXB : MOVZXB, &val, &src, dt.type);
			} else {
				inst2(ctx, MOV, &val, &src, src.dt.type);
			}
			
			done_with(p->unary.src);
			break;
		}
		
		default:
		assert(0 && "TODO: Implement node eval");
		break;
	}
	
	assert(ctx->use_count[r] > 0);
	ctx->use_count[r] -= 1;
	
	// it's no longer a temporary if it's here
	if (val.is_temp) {
		ctx->values[r] = val;
		ctx->values[r].is_temp = false;
	} else {
		ctx->values[r] = val;
	}
	
	return val;
}

static Val eval_addressof(Ctx* ctx, TB_Function* f, TB_Register r) {
	Val v = eval(ctx, f, r);
	if (v.dt.type == TB_BOOL) v.dt.type = TB_I8;
	
	if (is_value_mem(&v)) {
		if (v.mem.is_rvalue) {
			if (ctx->use_count[r] == 0 && v.mem.index == GPR_NONE) {
				// recycle
				Val tmp = val_gpr(TB_PTR, v.mem.base);
				inst2(ctx, MOV, &tmp, &v, TB_PTR);
				
				v = val_base_disp(TB_TYPE_PTR, tmp.gpr, 0);
				v.is_temp = true;
				return v;
			} else {
				Val tmp = alloc_gpr(ctx, f, TB_PTR);
				inst2(ctx, MOV, &tmp, &v, TB_PTR);
				
				v = val_base_disp(TB_TYPE_PTR, tmp.gpr, 0);
				v.is_temp = true;
				return v;
			}
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
		v.is_temp = true;
		return v;
	} else tb_todo();
	
	return v;
}

static Val eval_rvalue(Ctx* restrict ctx, TB_Function* f, TB_Register r) {
	Val v = eval(ctx, f, r);
	if (v.dt.type == TB_BOOL) v.dt.type = TB_I8;
	
	if (is_value_mem(&v)) {
		if (is_address_node(f->nodes.type[r])) {
			assert(!v.mem.is_rvalue);
			
			Val new_v = alloc_gpr(ctx, f, v.dt.type);
			new_v.is_temp = true;
			
			inst2(ctx, LEA, &new_v, &v, TB_PTR);
			free_val(ctx, f, v);
			return new_v;
		} else if (!v.mem.is_rvalue) {
			if (v.dt.width || TB_IS_FLOAT_TYPE(v.dt.type)) {
				Val new_v = alloc_xmm(ctx, f, v.dt);
				new_v.is_temp = true;
				
				uint8_t flags = 0;
				flags |= (v.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (v.dt.width) ? INST2FP_PACKED : 0;
				
				inst2sse(ctx, FP_MOV, &new_v, &v, flags);
				return new_v;
			} else {
				Val tmp = alloc_gpr(ctx, f, v.dt.type);
				tmp.is_temp = true;
				
				inst2(ctx, MOV, &tmp, &v, v.dt.type);
				
				return tmp;
			}
		}
	} else if (v.type == VAL_FLAGS) {
		assert(0 && "No fucking way bro");
	}
	
	return v;
}
