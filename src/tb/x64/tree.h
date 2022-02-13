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

static Val eval(Ctx* restrict ctx, TB_Function* f, TB_Reg r, bool root) {
	if (ctx->values[r].type != VAL_NONE) {
		if (ctx->is_tallying) {
			if (ctx->use_count[r] == 0) {
				if (!should_rematerialize(f->nodes.data[r].type)) {
					tb_function_print(f, tb_default_print_callback, stdout);
					printf("\n\n\n");
					assert(0);
				}
			}
			ctx->use_count[r] -= 1;
		}
		
		return ctx->values[r];
	}
	
	TB_Node* restrict n = &f->nodes.data[r];
	TB_NodeTypeEnum reg_type = n->type;
	TB_DataType dt = n->dt;
	
	Val val = { 0 };
	switch (reg_type) {
		case TB_SIGNED_CONST:
		case TB_UNSIGNED_CONST: {
			int32_t imm32 = (int32_t)n->sint.value;
			if (n->sint.value == imm32) {
				val = val_imm(dt, imm32);
				break;
			}
			
			// explicit mov
			val = alloc_gpr(ctx, f, r, dt.type);
			
			// mov reg64, imm64
			emit(rex(true, 0x0, val.gpr, 0));
			emit(0xB8 + (val.gpr & 0b111));
			emit8(n->uint.value);
			break;
		}
		case TB_FLOAT_CONST: {
			val = gen_float_const(ctx, f, r, n->flt.value, dt);
			break;
		}
		
		case TB_STRING_CONST: {
			const char* str = n->string.data;
			size_t len = n->string.length;
			
			val = alloc_gpr(ctx, f, r, dt.type);
			
			emit(rex(true, val.gpr, RBP, 0));
			emit(0x8D);
			emit(mod_rx_rm(MOD_INDIRECT, val.gpr, RBP));
			
			uint32_t disp = tb_emit_const_patch(f->module, s_compiled_func_id, code_pos(), str, len, s_local_thread_id);
			emit4(disp);
			break;
		}
		case TB_GLOBAL_ADDRESS: {
			val = val_global(n->global.value);
			break;
		}
		case TB_EXTERN_ADDRESS:
		case TB_FUNC_ADDRESS: {
			val = alloc_gpr(ctx, f, r, dt.type);
			
			emit(rex(true, val.gpr, RBP, 0));
			emit(0x8D);
			emit(mod_rx_rm(MOD_INDIRECT, val.gpr, RBP));
			emit4(0x0);
			
			if (reg_type == TB_EXTERN_ADDRESS) {
				tb_emit_ecall_patch(f->module, s_compiled_func_id, n->external.value, code_pos() - 4, s_local_thread_id);
			} else {
				int target_func = n->func.value - f->module->functions.data;
				tb_emit_call_patch(f->module, s_compiled_func_id, target_func, code_pos() - 4, s_local_thread_id);
			}
			break;
		}
		case TB_LOAD: {
			val = eval_addressof(ctx, f, n->load.address);
			
			val.dt = dt;
			val.mem.is_rvalue = true;
			
			kill(ctx, f, n->load.address, val);
			break;
		}
		case TB_PARAM_ADDR: {
			TB_Reg param = n->param_addr.param;
			val = eval(ctx, f, param, false);
			
			kill(ctx, f, n->param_addr.param, val);
			break;
		}
		case TB_RESTRICT: {
			val = eval(ctx, f, n->unary.src, false);
			break;
		}
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
			
			kill(ctx, f, n->array_access.base, base);
			kill(ctx, f, n->array_access.index, index);
			break;
		}
		case TB_MEMBER_ACCESS: {
			val = eval_addressof(ctx, f, n->member_access.base);
			val.mem.disp += n->member_access.offset;
			
			kill(ctx, f, n->member_access.base, val);
			break;
		}
		
		case TB_NEG: {
			Val src = eval_rvalue(ctx, f, n->unary.src);
			
			if (dt.type == TB_F64) {
				// .LCPI0_0:
				//   .quad   0x8000000000000000
				//   .quad   0x8000000000000000
				// ...
				// xorps   xmm0, xmmword ptr [rip + .LCPI0_0]
				val = alloc_xmm(ctx, f, r, dt);
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
			} else if (dt.type == TB_F32) {
				// .LCPI0_0:
				//   .long   0x80000000
				//   .long   0x80000000
				//   .long   0x80000000
				//   .long   0x80000000
				// ...
				// xorps   xmm0, xmmword ptr [rip + .LCPI0_0]
				val = alloc_xmm(ctx, f, r, dt);
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
			} else {
				bool recycled = ctx->use_count[n->unary.src] == 0 && src.type == VAL_GPR;
				
				if (recycled) {
					inst1(ctx, NEG, &src);
					
					val = src;
				} else {
					val = alloc_gpr(ctx, f, r, dt.type);
					
					inst2(ctx, MOV, &val, &src, dt.type);
					inst1(ctx, NEG, &val);
				}
			}
			
			kill(ctx, f, n->unary.src, src);
			break;
		}
		case TB_NOT: {
			Val src = eval_rvalue(ctx, f, n->unary.src);
			
			bool recycled = ctx->use_count[n->unary.src] == 0 &&
				src.type != VAL_MEM;
			
			if (recycled) {
				val = src;
				
				inst1(ctx, NOT, &src);
			} else {
				val = alloc_gpr(ctx, f, r, dt.type);
				
				inst2(ctx, MOV, &val, &src, dt.type);
				inst1(ctx, NOT, &val);
			}
			
			kill(ctx, f, n->unary.src, src);
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
		
		// Integer binary operations
		case TB_AND:
		case TB_OR:
		case TB_XOR:
		case TB_ADD:
		case TB_SUB:
		case TB_MUL: {
			Val a = eval_rvalue(ctx, f, n->i_arith.a);
			Val b = eval_rvalue(ctx, f, n->i_arith.b);
			
			if (dt.width == 0) {
				// simple scalar ops
				const static Inst2Type ops[] = { AND, OR, XOR, ADD, SUB, IMUL };
				
				if (dt.type == TB_BOOL) dt.type = TB_I8;
				
				bool recycled = ctx->use_count[n->i_arith.a] == 0 &&
					!(is_value_mem(&a) && reg_type == TB_MUL);
				
				if (recycled) {
					val = a;
				} else {
					val = alloc_gpr(ctx, f, r, dt.type);
					
					//assert(!is_value_match(&val, &a));
					if (is_value_match(&val, &a)) {
						a = ctx->values[n->i_arith.a];
					}
					
					if (is_value_match(&val, &b)) {
						b = ctx->values[n->i_arith.b];
					}
					
					inst2(ctx, MOV, &val, &a, dt.type);
				}
				
				// we can't do a OP mem, mem
				// and imul doesn't support a simple OP r/m, imm mode
				// we'll need a temporary in those cases
				bool is_mem_dst = is_value_mem(&val);
				
				bool needs_temporary = is_mem_dst && is_value_mem(&b);
				needs_temporary |= (reg_type == TB_MUL && (is_mem_dst || b.type == VAL_IMM));
				
				if (needs_temporary) {
					Val tmp = alloc_gpr(ctx, f, TB_TEMP_REG, dt.type);
					
					inst2(ctx, MOV, &tmp, &b, dt.type);
					inst2(ctx, ops[reg_type - TB_AND], &val, &tmp, dt.type);
					
					free_gpr(ctx, f, tmp.gpr);
				} else {
					inst2(ctx, ops[reg_type - TB_AND], &val, &b, dt.type);
				}
			} else {
				// supported modes (for now)
				assert(dt.width <= 2);
				
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				
				bool recycled = ctx->use_count[n->i_arith.a] == 0 &&
					a.type == VAL_XMM;
				
				if (recycled) {
					val = a;
				} else {
					val = alloc_xmm(ctx, f, r, dt);
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
			}
			
			kill(ctx, f, n->i_arith.a, a);
			kill(ctx, f, n->i_arith.a, b);
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
		
		case TB_CMP_EQ:
		case TB_CMP_NE:
		case TB_CMP_SLT:
		case TB_CMP_SLE:
		case TB_CMP_ULT:
		case TB_CMP_ULE:
		case TB_CMP_FLT:
		case TB_CMP_FLE: {
			TB_DataType cmp_dt = n->cmp.dt;
			assert(cmp_dt.width == 0 && "TODO: Implement vector compares");
			
			if (cmp_dt.type == TB_BOOL) cmp_dt.type = TB_I8;
			
			Val a = eval_rvalue(ctx, f, n->cmp.a);
			Val b = eval_rvalue(ctx, f, n->cmp.b);
			if (a.type == VAL_IMM && b.type == VAL_IMM) {
				switch (reg_type) {
					case TB_CMP_EQ: val = val_imm(dt, a.imm == b.imm); break;
					case TB_CMP_NE: val = val_imm(dt, a.imm != b.imm); break;
					case TB_CMP_SLT: val = val_imm(dt, a.imm < b.imm); break;
					case TB_CMP_SLE: val = val_imm(dt, a.imm <= b.imm); break;
					case TB_CMP_ULT: val = val_imm(dt, (uint32_t)a.imm < (uint32_t)b.imm); break;
					case TB_CMP_ULE: val = val_imm(dt, (uint32_t)a.imm <= (uint32_t)b.imm); break;
					default: tb_unreachable();
				}
				
				break;
			}
			
			bool convert_to_reg = !(root && ctx->is_if_statement_next);
			if (convert_to_reg) {
				val = alloc_gpr(ctx, f, r, TB_I8);
				
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
					Val tmp = alloc_xmm(ctx, f, r, cmp_dt);
					
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
					Val tmp = alloc_gpr(ctx, f, r, cmp_dt.type);
					
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
			
			kill(ctx, f, n->cmp.a, a);
			kill(ctx, f, n->cmp.a, b);
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
		
		case TB_PHI2: {
			PhiValue* phi = find_phi(ctx, r);
			assert(phi && "PHI node not initialized but used");
			
			val = phi->value;
			break;
		}
		
		case TB_FLOAT2INT: {
			assert(dt.width == 0 && "TODO: Implement vector float2int");
			
			Val src = eval_rvalue(ctx, f, n->unary.src);
			assert(src.type == VAL_MEM || src.type == VAL_GLOBAL || src.type == VAL_XMM);
			
			Val v = alloc_gpr(ctx, f, r, dt.type);
			
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
			
			val = v;
			
			kill(ctx, f, n->unary.src, src);
			break;
		}
		case TB_INT2FLOAT: {
			assert(dt.width == 0 && "TODO: Implement vector int2float");
			
			Val src = eval_rvalue(ctx, f, n->unary.src);
			if (src.type == VAL_IMM) {
				val = gen_float_const(ctx, f, r, (double)src.imm, dt);
				break;
			}
			
			assert(src.type == VAL_MEM || src.type == VAL_GLOBAL || src.type == VAL_GPR);
			val = alloc_xmm(ctx, f, r, dt);
			
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
			
			kill(ctx, f, n->unary.src, src);
			break;
		}
		
		case TB_TRUNCATE: {
			Val src = eval_rvalue(ctx, f, n->unary.src);
			
			if (TB_IS_FLOAT_TYPE(dt.type)) {
				val = alloc_xmm(ctx, f, r, dt);
				
				uint8_t flags = 0;
				flags |= (src.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (src.dt.width) ? INST2FP_PACKED : 0;
				inst2sse(ctx, FP_CVT, &val, &src, flags);
			} else {
				if (src.type == VAL_IMM) {
					src.dt = dt;
					val = src;
					break;
				}
				
				bool recycled = ctx->use_count[n->unary.src] == 0 && src.type == VAL_GPR;
				
				if (recycled) {
					val = src;
				} else {
					val = alloc_gpr(ctx, f, r, dt.type);
				}
				
				if (dt.type == TB_I16) {
					inst2(ctx, MOVZXW, &val, &src, TB_I16);
				} else if (dt.type == TB_I8 || dt.type == TB_BOOL) {
					inst2(ctx, MOVZXB, &val, &src, TB_I8);
				} else if (dt.type == TB_I32) {
					// this forces the high 32bits to be cleared
					inst2(ctx, MOV, &val, &src, TB_I32);
				} else {
					if (!recycled) inst2(ctx, MOV, &val, &src, src.dt.type);
				}
			}
			
			kill(ctx, f, n->unary.src, src);
			break;
		}
		
		case TB_PTR2INT: {
			Val src = eval_rvalue(ctx, f, n->unary.src);
			
			bool recycled = ctx->use_count[n->unary.src] == 0 &&
				src.type == VAL_GPR;
			
			if (recycled) {
				val = src;
			} else {
				val = alloc_gpr(ctx, f, r, dt.type);
			}
			
			if (dt.type == TB_I16) {
				inst2(ctx, MOVZXW, &val, &src, TB_I16);
			} else if (dt.type == TB_I8 || dt.type == TB_BOOL) {
				inst2(ctx, MOVZXB, &val, &src, TB_I8);
			} else {
				inst2(ctx, MOV, &val, &src, src.dt.type);
			}
			
			kill(ctx, f, n->unary.src, src);
			break;
		}
		case TB_INT2PTR:
		case TB_SIGN_EXT:
		case TB_ZERO_EXT: {
			assert(dt.width == 0 && "TODO: Implement vector zero extend");
			bool sign_ext = (reg_type == TB_SIGN_EXT);
			
			Val src = eval_rvalue(ctx, f, n->unary.src);
			if (src.type == VAL_IMM) {
				src.dt = dt;
				val = src;
				break;
			}
			
			bool recycled = ctx->use_count[n->unary.src] == 0 &&
				src.type != VAL_MEM;
			
			if (recycled) {
				val = src;
			} else {
				val = alloc_gpr(ctx, f, r, dt.type);
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
			kill(ctx, f, r, v);
			
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
