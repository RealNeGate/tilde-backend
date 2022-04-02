// we track use counts to know when things should die
//
// i = load     i2
// j = i + 1    i1 j1
// store j      i1 j0    kill j
// store i      i0       kill i
static Val gen_float_const(Ctx* ctx, TB_Function* f, TB_Reg r, double float_value, TB_DataType dt) {
    // Unlike integers, there's no float immediates
    Val v       = alloc_xmm(ctx, f, r, dt);
    XMM dst_xmm = v.xmm;

    assert(TB_IS_FLOAT_TYPE(dt.type) && dt.width == 0);
    uint64_t imm = (Cvt_F64U64) { .f = float_value }.i;

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
            *rdata_payload          = imm;

            disp = tb_emit_const_patch(f->module, s_compiled_func_id, code_pos(), rdata_payload,
                sizeof(uint64_t), s_local_thread_id);
        } else {
            uint32_t imm32 = (Cvt_F32U32) { .f = float_value }.i;

            uint32_t* rdata_payload = tb_platform_arena_alloc(sizeof(uint32_t));
            *rdata_payload          = imm32;

            disp = tb_emit_const_patch(f->module, s_compiled_func_id, code_pos(), rdata_payload,
                sizeof(uint32_t), s_local_thread_id);
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

static Val eval_addressof(Ctx* ctx, TB_Function* f, TB_Reg r) {
    Val v = eval(ctx, f, r, false);
    if (v.dt.type == TB_BOOL) v.dt.type = TB_I8;

    if (is_value_mem(&v)) {
        if (v.mem.is_rvalue) {
            if (ctx->use_count[r] == 0 && v.mem.index == GPR_NONE && v.mem.base != RSP &&
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
    } else
        tb_todo();

    v.is_temp = true;
    return v;
}

static Val eval_rvalue(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    Val v = eval(ctx, f, r, false);
    if (v.dt.type == TB_BOOL) v.dt.type = TB_I8;

    if (is_value_mem(&v)) {
        if (is_address_node(f->nodes.data[r].type)) {
            assert(!v.mem.is_rvalue);

            Val new_v     = alloc_gpr(ctx, f, TB_TEMP_REG, v.dt.type);
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
        // assert(0 && "No fucking way bro");
    }

    v.is_temp = true;
    return v;
}
#endif

#if 0

static void eval_basic_block(Ctx* restrict ctx, TB_Function* f, TB_Reg bb, TB_Reg bb_end) {
	// first node in the basic block
	bb = f->nodes.data[bb].next;
	if (bb == bb_end) return;
	
	TB_FOR_EACH_NODE_RANGE(n, f, bb, bb_end) {
		TB_Reg r = n - f->nodes.data;
		
		TB_Node* restrict n = &f->nodes.data[r];
		TB_NodeTypeEnum reg_type = n->type;
		TB_DataType dt = n->dt;
		
		switch (reg_type) {
			case TB_NULL:
			case TB_PARAM:
			case TB_LOCAL:
			case TB_PHI1:
			break;
			
			case TB_PHI2: {
				PhiValue* phi = find_phi(ctx, r);
				assert(phi && "PHI node not initialized but used");
				
				def(ctx, f, r, phi->simple.value);
				break;
			}
			
			case TB_LINE_INFO: {
				f->lines[f->line_count++] = (TB_Line){
					.file = n->line_info.file,
					.line = n->line_info.line,
					.pos = code_pos()
				};
				break;
			}
			
			case TB_DEBUGBREAK: {
				emit(0xCC);
				break;
			}
			
			case TB_SIGNED_CONST:
			case TB_UNSIGNED_CONST: {
				int32_t imm32 = (int32_t)n->sint.value;
				if (n->sint.value == imm32) {
					ctx->values[r] = val_imm(dt, imm32);
					break;
				}
				
				// explicit mov
				Val val = alloc_gpr(ctx, f, r, dt.type);
				
				// mov reg64, imm64
				emit(rex(true, 0x0, val.gpr, 0));
				emit(0xB8 + (val.gpr & 0b111));
				emit8(n->uint.value);
				
				ctx->values[r] = val;
				break;
			}
			case TB_FLOAT_CONST: {
				ctx->values[r] = gen_float_const(ctx, f, r, n->flt.value, dt);
				break;
			}
			case TB_STRING_CONST: {
				const char* str = n->string.data;
				size_t len = n->string.length;
				
				Val val = alloc_gpr(ctx, f, r, dt.type);
				
				emit(rex(true, val.gpr, RBP, 0));
				emit(0x8D);
				emit(mod_rx_rm(MOD_INDIRECT, val.gpr, RBP));
				
				uint32_t disp = tb_emit_const_patch(f->module, s_compiled_func_id, code_pos(), str, len, s_local_thread_id);
				emit4(disp);
				break;
			}
			case TB_GLOBAL_ADDRESS: {
				ctx->values[r] = val_global(n->global.value);
				break;
			}
			case TB_EXTERN_ADDRESS:
			case TB_FUNC_ADDRESS: {
				Val val = alloc_gpr(ctx, f, r, dt.type);
				
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
			case TB_VA_START: {
				Val src = eval(ctx, f, n->unary.src);
				assert(is_value_mem(&src));
				assert(!ctx->is_sysv && "How does va_start even work on SysV?");
				
				src.mem.disp += 8;
				
				Val dst = alloc_gpr(ctx, f, r, TB_PTR);
				inst2(ctx, LEA, &dst, &src, TB_PTR);
				
				kill(ctx, f, n->unary.src);
				break;
			}
			case TB_ARRAY_ACCESS: {
				Val base = eval(ctx, f, n->array_access.base);
				Val index = eval(ctx, f, n->array_access.index);
				uint32_t stride = n->array_access.stride;
				
				if (base.type == VAL_MEM &&
					base.mem.index == GPR_NONE &&
					index.type == VAL_IMM) {
					kill(ctx, f, n->array_access.base);
					kill(ctx, f, n->array_access.index);
					
					int64_t disp = base.mem.disp;
					disp += ((int64_t)index.imm) * stride;
					assert(disp == (int32_t)disp);
					
					def(ctx, f, r, val_base_disp(TB_TYPE_PTR, base.mem.base, disp));
					break;
				}
				
				// Try to recycle the index
				bool recycled = ctx->use_count[n->array_access.index] == 0 &&
					index.type == VAL_GPR;
				
				// We try to delay it for better codegen
				bool has_filled_dst = false;
				
				Val val;
				if (recycled) {
					has_filled_dst = true;
					
					val = index;
					
					kill(ctx, f, n->array_access.base);
					kill(ctx, f, n->array_access.index);
					def(ctx, f, r, val);
				} else {
					// we'll delay setting the dst_gpr here because
					// it's possible to fold it into another operation
					// like LEA
					//   lea dst, [base + index * stride]
					//  vs
					//   mov dst, index
					//   lea dst, [base + dst * stride]
					val = alloc_gpr(ctx, f, r, TB_PTR);
					if (index.type != VAL_GPR) {
						inst2(ctx, MOV, &val, &index, TB_PTR);
						
						has_filled_dst = true;
					}
					
					kill(ctx, f, n->array_access.base);
					kill(ctx, f, n->array_access.index);
				}
				
				// if it's an LEA index*stride
				// then stride > 0, if not it's free
				// do think of it however
				GPR index_reg = GPR_NONE;
				uint8_t stride_as_shift = 0;
				
				if (tb_is_power_of_two(stride)) {
					stride_as_shift = tb_ffs(stride) - 1;
					
					if (stride_as_shift <= 3) {
						// it can use the shift in the memory operand
						index_reg = has_filled_dst ? val.gpr : index.gpr;
					} else {
						assert(stride_as_shift < 64 && "Stride to big!!!");
						
						if (!has_filled_dst) {
							inst2(ctx, MOV, &val, &index, TB_PTR);
						}
						
						// shl index, stride_as_shift
						emit(rex(true, 0x04, val.gpr, 0));
						emit(0xC1);
						emit(mod_rx_rm(MOD_DIRECT, 0x04, val.gpr));
						emit(stride_as_shift);
						
						index_reg = val.gpr;
						stride_as_shift = 0; // pre-multiplied, don't propagate
					}
				} else {
					if (has_filled_dst) {
						// imul dst, index, stride
						emit(rex(true, val.gpr, val.gpr, 0));
						emit(0x69);
						emit(mod_rx_rm(MOD_DIRECT, val.gpr, val.gpr));
						emit4(stride);
					} else {
						// imul dst, index, stride
						emit(rex(true, val.gpr, index.gpr, 0));
						emit(0x69);
						emit(mod_rx_rm(MOD_DIRECT, val.gpr, index.gpr));
						emit4(stride);
					}
					
					index_reg = val.gpr;
					stride_as_shift = 0; // pre-multiplied, don't propagate
				}
				
				// post conditions :)
				assert(index_reg != GPR_NONE);
				assert(stride_as_shift >= 0 && stride_as_shift <= 3 && "stride_as_shift can't fit into an LEA");
				
				// Resolve base (if it's not already in a register)
				if (base.type != VAL_GPR) {
					bool is_base_an_address = is_address_node(f->nodes.data[n->array_access.base].type);
					
					Val temp = alloc_gpr(ctx, f, TB_TEMP_REG, TB_PTR);
					inst2(ctx, is_base_an_address ? LEA : MOV, &temp, &base, TB_PTR);
					
					if (stride_as_shift) {
						Val arith = val_base_index(TB_TYPE_PTR, temp.gpr, index_reg, stride_as_shift);
						inst2(ctx, LEA, &val, &arith, TB_PTR);
					} else {
						inst2(ctx, ADD, &val, &temp, TB_PTR);
					}
					
					free_gpr(ctx, f, temp.gpr);
				} else {
					if (stride_as_shift) {
						Val arith = val_base_index(TB_TYPE_PTR, base.gpr, index_reg, stride_as_shift);
						inst2(ctx, LEA, &val, &arith, TB_PTR);
					} else {
						inst2(ctx, ADD, &val, &base, TB_PTR);
					}
				}
				break;
			}
			case TB_MEMBER_ACCESS: {
				Val base = eval(ctx, f, n->member_access.base);
				
				if (base.type == VAL_GLOBAL) {
					base.global.disp += n->member_access.offset;
				} else if (base.type == VAL_MEM) {
					base.mem.disp += n->member_access.offset;
				} else if (base.type == VAL_GPR) {
					base = val_base_disp(TB_TYPE_PTR, base.gpr, n->member_access.offset);
				} else if (base.type == VAL_IMM) {
					int64_t a = base.imm;
					int64_t b = n->member_access.offset;
					int64_t result = a + b;
					
					assert(result == (int32_t)result);
					base = val_imm(TB_TYPE_PTR, result);
				}
				
				kill(ctx, f, n->member_access.base);
				def(ctx, f, r, base);
				break;
			}
			case TB_LOAD: {
				Val src = eval(ctx, f, n->load.address);
				if (src.type == VAL_GPR) {
					src = val_base_disp(TB_TYPE_PTR, src.gpr, 0);
				} else if (src.type == VAL_IMM) {
					assert(0 && "Support load from constant address");
				}
				
				kill(ctx, f, n->load.address);
				if (TB_IS_FLOAT_TYPE(dt.type) || dt.width) {
					Val val = alloc_xmm(ctx, f, r, dt);
					
					uint8_t flags = 0;
					flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
					flags |= (dt.width) ? INST2FP_PACKED : 0;
					inst2sse(ctx, FP_MOV, &val, &src, flags);
				} 
				// sometimes the load is chained to a sign/zero extension
				// sxt(load(p)) => movsx tmp, [addr]
				// zxt(load(p)) => movzx tmp, [addr]
				// load(p)      => mov   tmp, [addr]
				else if ((f->nodes.data[n->next].type == TB_SIGN_EXT ||
						  f->nodes.data[n->next].type == TB_ZERO_EXT) &&
						 ctx->use_count[r] == 1 &&
						 f->nodes.data[n->next].unary.src == r) {
					src.dt = dt;
					def(ctx, f, r, src);
				} else {
					Val val = alloc_gpr(ctx, f, r, dt.type);
					inst2(ctx, MOV, &val, &src, dt.type);
				}
				break;
			}
			case TB_STORE: {
				TB_Reg addr_reg = n->store.address;
				TB_Reg val_reg = n->store.value;
				
				// Eval address and cast to the correct type for the store
				Val address = eval(ctx, f, addr_reg);
				if (address.type == VAL_GPR) {
					address = val_base_disp(address.dt, address.gpr, 0);
				} else if (address.is_spill) {
					Val val = alloc_gpr(ctx, f, r, dt.type);
					inst2(ctx, MOV, &val, &address, TB_PTR);
					address = val_base_disp(TB_TYPE_PTR, val.gpr, 0);
				}
				
				store_into(ctx, f, dt, &address, r, addr_reg, val_reg, true);
				
				if (address.is_spill) {
					free_gpr(ctx, f, address.mem.base);
				}
				kill(ctx, f, addr_reg);
				break;
			}
			case TB_PARAM_ADDR: {
				TB_Reg param = n->param_addr.param;
				
				Val val = eval(ctx, f, param);
				val.dt = dt;
				val.is_spill = false;
				assert(is_value_mem(&val));
				
				kill(ctx, f, n->param_addr.param);
				def(ctx, f, r, val);
				break;
			}
			case TB_RESTRICT: {
				Val val = eval(ctx, f, n->unary.src);
				
				kill(ctx, f, n->unary.src);
				def(ctx, f, r, val);
				break;
			}
			case TB_NEG: {
				Val src = eval(ctx, f, n->unary.src);
				
				if (dt.type == TB_F64) {
					// .LCPI0_0:
					//   .quad   0x8000000000000000
					//   .quad   0x8000000000000000
					// ...
					// xorps   xmm0, xmmword ptr [rip + .LCPI0_0]
					Val val = alloc_xmm(ctx, f, r, dt);
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
					
					kill(ctx, f, n->unary.src);
				} else if (dt.type == TB_F32) {
					// .LCPI0_0:
					//   .long   0x80000000
					//   .long   0x80000000
					//   .long   0x80000000
					//   .long   0x80000000
					// ...
					// xorps   xmm0, xmmword ptr [rip + .LCPI0_0]
					Val val = alloc_xmm(ctx, f, r, dt);
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
					
					kill(ctx, f, n->unary.src);
				} else {
					bool recycled = ctx->use_count[n->unary.src] == 0 && src.type == VAL_GPR;
					
					if (recycled) {
						inst1(ctx, NEG, &src);
						
						// Kill old value, slap in new one
						kill(ctx, f, n->unary.src);
						
						assert(src.type == VAL_GPR);
						ctx->gpr_allocator[src.gpr] = r;
						ctx->values[r] = src;
					} else {
						Val val = alloc_gpr(ctx, f, r, dt.type);
						
						inst2(ctx, MOV, &val, &src, dt.type);
						inst1(ctx, NEG, &val);
						
						kill(ctx, f, n->unary.src);
					}
				}
				break;
			}
			case TB_NOT: {
				Val src = eval(ctx, f, n->unary.src);
				
				bool recycled = ctx->use_count[n->unary.src] == 0 && src.type == VAL_GPR;
				if (recycled) {
					inst1(ctx, NOT, &src);
					
					// Kill old value, slap in new one
					kill(ctx, f, n->unary.src);
					
					assert(src.type == VAL_GPR);
					ctx->gpr_allocator[src.gpr] = r;
					ctx->values[r] = src;
				} else {
					Val val = alloc_gpr(ctx, f, r, dt.type);
					
					inst2(ctx, MOV, &val, &src, dt.type);
					inst1(ctx, NOT, &val);
					
					kill(ctx, f, n->unary.src);
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
				Val a = eval(ctx, f, n->i_arith.a);
				Val b = eval(ctx, f, n->i_arith.b);
				
				Val val;
				if (dt.width == 0) {
					// simple scalar ops
					const static Inst2Type ops[] = { AND, OR, XOR, ADD, SUB, IMUL };
					
					if (dt.type == TB_BOOL) dt.type = TB_I8;
					
					bool recycled = ctx->use_count[n->i_arith.a] == 0 &&
						a.type != VAL_IMM &&
						!(is_value_mem(&a) && reg_type == TB_MUL);
					
					if (recycled) {
						val = a;
						
						kill(ctx, f, n->i_arith.a);
						kill(ctx, f, n->i_arith.b);
						
						def(ctx, f, r, val);
					} else {
						val = alloc_gpr(ctx, f, r, dt.type);
						inst2(ctx, MOV, &val, &a, dt.type);
						
						kill(ctx, f, n->i_arith.a);
						kill(ctx, f, n->i_arith.b);
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
						
						kill(ctx, f, n->i_arith.a);
						kill(ctx, f, n->i_arith.b);
						
						def(ctx, f, r, val);
					} else {
						val = alloc_xmm(ctx, f, r, dt);
						inst2sse(ctx, FP_MOV, &val, &a, flags);
						
						kill(ctx, f, n->i_arith.a);
						kill(ctx, f, n->i_arith.b);
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
							tb_panic("Implement vector integer multiply");
						}
						default: tb_unreachable();
					}
				}
				break;
			}
			case TB_UDIV:
			case TB_SDIV:
			case TB_UMOD:
			case TB_SMOD: {
				assert(dt.width == 0 && "TODO: Implement vector integer division and modulo");
				
				bool is_signed = (reg_type == TB_SDIV || reg_type == TB_SMOD);
				bool is_div = (reg_type == TB_UDIV || reg_type == TB_SDIV);
				
				Val a = eval(ctx, f, n->i_arith.a);
				Val b = eval(ctx, f, n->i_arith.b);
				
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
				
				kill(ctx, f, n->i_arith.a);
				kill(ctx, f, n->i_arith.b);
				
				// the return value is in RAX for division
				// and RDX for modulo
				def(ctx, f, r, val_gpr(dt.type, is_div ? RAX : RDX));
				
				// free the other piece of the divmod result
				ctx->gpr_allocator[is_div ? RDX : RAX] = TB_NULL_REG;
				break;
			}
			case TB_SHR:
			case TB_SHL:
			case TB_SAR: {
				Val a = eval(ctx, f, n->i_arith.a);
				Val b = eval(ctx, f, n->i_arith.b);
				
				if (a.type == VAL_IMM && b.type == VAL_IMM) {
					uint64_t result;
					switch (reg_type) {
						case TB_SHL: result = (uint64_t)a.imm << (uint64_t)b.imm; break;
						case TB_SHR: result = (uint64_t)a.imm >> (uint64_t)b.imm; break;
						case TB_SAR: result = (int64_t)a.imm >> (int64_t)b.imm; break;
					}
					
					if (result == (int32_t)result) {
						def(ctx, f, r, val_imm(dt, result));
						break;
					}
				}
				
				bool recycled = ctx->use_count[n->i_arith.a] == 0 &&
					a.type == VAL_GPR;
				
				Val val;
				if (recycled) {
					val = a;
					
					kill(ctx, f, n->i_arith.a);
					kill(ctx, f, n->i_arith.b);
					
					def(ctx, f, r, val);
				} else {
					val = alloc_gpr(ctx, f, r, dt.type);
					inst2(ctx, MOV, &val, &a, dt.type);
					
					kill(ctx, f, n->i_arith.a);
					kill(ctx, f, n->i_arith.b);
				}
				
				bool is_64bit = dt.type == TB_I64;
				if (b.type == VAL_IMM) {
					assert(b.imm < 64);
					
					// C1 /4       shl r/m, imm
					// C1 /5       shr r/m, imm
					// C1 /7       sar r/m, imm
					if (dt.type == TB_I16) emit(0x66);
					emit(rex(is_64bit, 0x00, val.gpr, 0x00));
					emit(dt.type == TB_I8 ? 0xC0 : 0xC1);
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
				break;
			}
			// Float binary operators
			case TB_FADD:
			case TB_FSUB:
			case TB_FMUL:
			case TB_FDIV: {
				// supported modes (for now)
				assert(dt.width <= 2 && "TODO: Implement vector");
				const static Inst2FPType tbl[] = { FP_ADD, FP_SUB, FP_MUL, FP_DIV };
				
				Val a = eval(ctx, f, n->f_arith.a);
				Val b = eval(ctx, f, n->f_arith.b);
				
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				
				bool recycled = ctx->use_count[n->i_arith.a] == 0 && a.type == VAL_XMM;
				
				Val val;
				if (recycled) {
					val = a;
					
					kill(ctx, f, n->f_arith.a);
					kill(ctx, f, n->f_arith.b);
					
					def(ctx, f, r, val);
				} else {
					val = alloc_xmm(ctx, f, r, dt);
					inst2sse(ctx, FP_MOV, &val, &a, flags);
					
					kill(ctx, f, n->f_arith.a);
					kill(ctx, f, n->f_arith.b);
				}
				
				Inst2FPType op = tbl[reg_type - TB_FADD];
				inst2sse(ctx, op, &val, &b, flags);
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
				
				Val a = eval(ctx, f, n->cmp.a);
				Val b = eval(ctx, f, n->cmp.b);
				if (a.type == VAL_IMM && b.type == VAL_IMM) {
					bool result = false;
					switch (reg_type) {
						case TB_CMP_EQ:  result = (a.imm == b.imm); break;
						case TB_CMP_NE:  result = (a.imm != b.imm); break;
						case TB_CMP_SLT: result = (a.imm < b.imm); break;
						case TB_CMP_SLE: result = (a.imm <= b.imm); break;
						case TB_CMP_ULT: result = ((uint32_t)a.imm < (uint32_t)b.imm); break;
						case TB_CMP_ULE: result = ((uint32_t)a.imm <= (uint32_t)b.imm); break;
						default: tb_unreachable();
					}
					
					def(ctx, f, r, val_imm(dt, result));
					break;
				}
				
				// if (cmp XX (a, b)) should return a FLAGS because the IF
				// will handle it properly
				bool returns_flags = ctx->use_count[r] == 1 && 
					f->nodes.data[n->next].type == TB_IF &&
					f->nodes.data[n->next].if_.cond == r;
				
				Val val = { 0 };
				if (!returns_flags) {
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
						case TB_CMP_FLT: cc = B; break;
						case TB_CMP_FLE: cc = BE; break;
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
				
				if (!returns_flags) {
					// setcc v
					if (val.gpr >= 8) emit(rex(true, val.gpr, val.gpr, 0));
					emit(0x0F);
					emit(0x90 + cc);
					emit(mod_rx_rm(MOD_DIRECT, val.gpr, val.gpr));
				} else {
					val = val_flags(cc);
				}
				
				kill(ctx, f, n->cmp.a);
				kill(ctx, f, n->cmp.b);
				
				def(ctx, f, r, val);
				break;
			}
			case TB_FLOAT2INT: {
				assert(dt.width == 0 && "TODO: Implement vector float2int");
				
				Val src = eval(ctx, f, n->unary.src);
				assert(src.type == VAL_MEM || src.type == VAL_GLOBAL || src.type == VAL_XMM);
				
				Val val = alloc_gpr(ctx, f, r, dt.type);
				
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
				
				uint8_t rx = val.gpr;
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
				
				kill(ctx, f, n->unary.src);
				break;
			}
			case TB_INT2FLOAT: {
				assert(dt.width == 0 && "TODO: Implement vector int2float");
				
				Val src = eval(ctx, f, n->unary.src);
				if (src.type == VAL_IMM) {
					def(ctx, f, r, gen_float_const(ctx, f, r, (double)src.imm, dt));
					break;
				}
				
				assert(src.type == VAL_MEM || src.type == VAL_GLOBAL || src.type == VAL_GPR);
				Val val = alloc_xmm(ctx, f, r, dt);
				
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
				
				kill(ctx, f, n->unary.src);
				break;
			}
			case TB_TRUNCATE: {
				Val src = eval(ctx, f, n->unary.src);
				
				if (TB_IS_FLOAT_TYPE(dt.type)) {
					Val val = alloc_xmm(ctx, f, r, dt);
					
					uint8_t flags = 0;
					flags |= (src.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
					flags |= (src.dt.width) ? INST2FP_PACKED : 0;
					inst2sse(ctx, FP_CVT, &val, &src, flags);
					
					kill(ctx, f, n->unary.src);
				} else {
					assert(dt.width == 0 && "TODO: vector truncate support");
					
					if (src.type == VAL_IMM) {
						uint64_t shift = 64 - (8 << (dt.type - TB_I8));
						uint64_t mask = (~0ull) >> shift;
						uint64_t num = (((uint64_t)src.imm) & mask);
						
						src.dt = dt;
						src.imm = num;
						
						def(ctx, f, r, src);
						break;
					}
					
					bool recycled = ctx->use_count[n->unary.src] == 0 && src.type == VAL_GPR;
					
					Val val;
					if (recycled) {
						val = src;
						
						kill(ctx, f, n->unary.src);
						def(ctx, f, r, val);
					} else {
						val = alloc_gpr(ctx, f, r, dt.type);
						
						kill(ctx, f, n->unary.src);
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
				break;
			}
			case TB_PTR2INT: {
				Val src = eval(ctx, f, n->unary.src);
				
				bool recycled = ctx->use_count[n->unary.src] == 0 &&
					src.type == VAL_GPR;
				
				Val val;
				if (recycled) {
					val = src;
					
					kill(ctx, f, n->unary.src);
					def(ctx, f, r, val);
				} else {
					val = alloc_gpr(ctx, f, r, dt.type);
					
					kill(ctx, f, n->unary.src);
				}
				
				if (dt.type == TB_I16) {
					inst2(ctx, MOVZXW, &val, &src, TB_I16);
				} else if (dt.type == TB_I8 || dt.type == TB_BOOL) {
					inst2(ctx, MOVZXB, &val, &src, TB_I8);
				} else {
					if (!recycled) inst2(ctx, MOV, &val, &src, src.dt.type);
				}
				break;
			}
			case TB_INT2PTR:
			case TB_SIGN_EXT:
			case TB_ZERO_EXT: {
				assert(dt.width == 0 && "TODO: Implement vector zero extend");
				bool sign_ext = (reg_type == TB_SIGN_EXT);
				
				Val src = eval(ctx, f, n->unary.src);
				if (src.type == VAL_IMM) {
					src.dt = dt;
					
					def(ctx, f, r, src);
					break;
				}
				
				bool recycled = ctx->use_count[n->unary.src] == 0 && src.type == VAL_GPR;
				
				Val val;
				if (recycled) {
					val = src;
					
					kill(ctx, f, n->unary.src);
					def(ctx, f, r, val);
				} else {
					val = alloc_gpr(ctx, f, r, dt.type);
					
					kill(ctx, f, n->unary.src);
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
				break;
			}
			case TB_FLOAT_EXT: {
				Val src = eval(ctx, f, n->unary.src);
				Val val = alloc_xmm(ctx, f, r, dt);
				
				uint8_t flags = 0;
				flags |= (src.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (src.dt.width) ? INST2FP_PACKED : 0;
				
				if (src.dt.type != dt.type || src.dt.width != dt.width) {
					inst2sse(ctx, FP_CVT, &val, &src, flags);
				} else {
					inst2sse(ctx, FP_MOV, &val, &src, flags);
				}
				kill(ctx, f, n->unary.src); 
				break;
			}
			case TB_BITCAST: {
				assert(dt.width == 0 && "TODO: Implement vector bitcast");
				
				Val src = eval(ctx, f, n->unary.src);
				assert(get_data_type_size(dt) == get_data_type_size(src.dt));
				
				// movd/q
				emit(0x66);
				
				Val val;
				if (src.dt.type == TB_I64 && dt.type == TB_F64) {
					// movd dst:xmm, src:gpr
					assert(src.type == VAL_GPR);
					val = alloc_xmm(ctx, f, r, dt);
					
					emit(rex(true, src.gpr, val.xmm, 0));
					emit(0x0F);
					emit(0x6E);
				} else if (src.dt.type == TB_I32 && dt.type == TB_F32) {
					// movq dst:xmm, src:gpr
					assert(src.type == VAL_GPR);
					val = alloc_xmm(ctx, f, r, dt);
					
					if (val.xmm >= 8 || src.gpr >= 8) {
						emit(rex(false, src.gpr, val.xmm, 0));
					}
					
					emit(0x0F);
					emit(0x6E);
				} else if (src.dt.type == TB_F64 && dt.type == TB_I64) {
					// movd dst:gpr, src:xmm
					assert(src.type == VAL_XMM);
					val = alloc_gpr(ctx, f, r, dt.type);
					
					emit(rex(true, src.xmm, val.gpr, 0));
					emit(0x0F);
					emit(0x7E);
				} else if (src.dt.type == TB_F32 && dt.type == TB_I32) {
					// movq dst:gpr, src:xmm
					assert(src.type == VAL_XMM);
					val = alloc_gpr(ctx, f, r, dt.type);
					
					if (val.gpr >= 8 || src.xmm >= 8) {
						emit(rex(true, src.xmm, val.gpr, 0));
					}
					
					emit(0x0F);
					emit(0x7E);
				} else tb_todo();
				
				// val.gpr and val.xmm alias so it's irrelevant which one we pick
				emit_memory_operand(ctx, src.gpr, &val);
				
				kill(ctx, f, n->unary.src);
				break;
			}
			
			case TB_CALL:
			case TB_ECALL:
			case TB_VCALL: {
				int param_start = n->call.param_start;
				int param_count = n->call.param_end - n->call.param_start;
				
				// Evict the GPRs that are caller saved
				uint16_t caller_saved = (ctx->is_sysv ? SYSV_ABI_CALLER_SAVED : WIN64_ABI_CALLER_SAVED);
				const GPR* parameter_gprs = ctx->is_sysv ? SYSV_GPR_PARAMETERS : WIN64_GPR_PARAMETERS;
				
				loop(j, param_count) {
					TB_Reg param_reg = f->vla.data[param_start + j];
					TB_DataType param_dt = f->nodes.data[param_reg].dt;
					
					if (!(TB_IS_FLOAT_TYPE(param_dt.type) || param_dt.width)) {
						caller_saved &= ~(1u << parameter_gprs[j]);
					}
				}
				
				loop(j, 16) if (caller_saved & (1u << j)) {
					evict_gpr(ctx, f, j);
				}
				
				// Evict the XMMs that are caller saved
				loop_range(j, ctx->is_sysv ? 0 : 5, 16) {
					evict_xmm(ctx, f, j);
				}
				
				TB_Reg before_gpr_reserves[tb_arrlen(ctx->gpr_allocator)];
				TB_Reg before_xmm_reserves[tb_arrlen(ctx->xmm_allocator)];
				memcpy(before_gpr_reserves, ctx->gpr_allocator, sizeof(before_gpr_reserves));
				memcpy(before_xmm_reserves, ctx->xmm_allocator, sizeof(before_xmm_reserves));
				
				// evict & reserve return value
				if (TB_IS_FLOAT_TYPE(dt.type) || dt.width) {
					ctx->xmm_allocator[XMM0] = TB_TEMP_REG;
				} else if (dt.type != TB_VOID) {
					ctx->gpr_allocator[RAX] = TB_TEMP_REG;
				}
				
				// evaluate parameters
				loop(j, param_count) {
					TB_Reg param_reg = f->vla.data[param_start + j];
					Val param = eval(ctx, f, param_reg);
					
					bool use_lea = is_address_node(f->nodes.data[param_reg].type);
					bool is_xmm = TB_IS_FLOAT_TYPE(param.dt.type) || param.dt.width;
					
					int register_params = 4;
					if (!is_xmm && ctx->is_sysv) {
						register_params = 6;
					}
					
					if (j < register_params) {
						if (is_xmm) {
							Val dst = val_xmm(param.dt, j);
							
							// move into param slot
							uint8_t flags = 0;
							flags |= (param.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
							flags |= (param.dt.width) ? INST2FP_PACKED : 0;
							
							inst2sse(ctx, FP_MOV, &dst, &param, flags);
							
							ctx->gpr_allocator[j] = param_reg;
						} else if (param.dt.type != TB_VOID) {
							Val dst = val_gpr(param.dt.type, parameter_gprs[j]);
							if (!is_value_gpr(&param, parameter_gprs[j])) {
								evict_gpr(ctx, f, parameter_gprs[j]);
								
								inst2(ctx, use_lea ? LEA : MOV, &dst, &param, param.dt.type);
							}
							
							ctx->gpr_allocator[parameter_gprs[j]] = param_reg;
						}
					} else {
						Val dst = val_base_disp(param.dt, RSP, 8 * j);
						
						// parameter is in memory
						if (is_xmm) {
							uint8_t flags = 0;
							flags |= (param.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
							flags |= (param.dt.width) ? INST2FP_PACKED : 0;
							
							if (param.type == VAL_MEM) {
								Val tmp = alloc_xmm(ctx, f, TB_TEMP_REG, param.dt);
								
								inst2sse(ctx, FP_MOV, &tmp, &param, flags);
								inst2sse(ctx, FP_MOV, &dst, &tmp, flags);
								
								free_xmm(ctx, f, tmp.xmm);
							} else {
								inst2sse(ctx, FP_MOV, &dst, &param, flags);
							}
						} else {
							if (param.type == VAL_MEM) {
								Val tmp = alloc_gpr(ctx, f, TB_TEMP_REG, param.dt.type);
								
								inst2(ctx, use_lea ? LEA : MOV, &tmp, &param, param.dt.type);
								inst2(ctx, MOV, &dst, &tmp, param.dt.type);
								
								free_gpr(ctx, f, tmp.gpr);
							} else {
								inst2(ctx, use_lea ? LEA : MOV, &dst, &param, param.dt.type);
							}
						}
					}
				}
				
				// CALL instruction and patch
				if (reg_type == TB_CALL) {
					TB_FunctionID target = n->call.target - f->module->functions.data;
					
					tb_emit_call_patch(f->module,
									   s_compiled_func_id,
									   target,
									   code_pos() + 1,
									   s_local_thread_id);
					
					// CALL rel32
					emit(0xE8);
					emit4(0x0);
				} else if (reg_type == TB_ECALL) {
					TB_ExternalID target = n->ecall.target;
					
					tb_emit_ecall_patch(f->module,
										s_compiled_func_id,
										target,
										code_pos() + 1,
										s_local_thread_id);
					
					// CALL rel32
					emit(0xE8);
					emit4(0x0);
				} else if (reg_type == TB_VCALL) {
					Val target = eval(ctx, f, n->vcall.target);
					
					// call r/m64
					inst1(ctx, CALL_RM, &target);
					
					kill(ctx, f, n->vcall.target);
				}
				
				memcpy(ctx->gpr_allocator, before_gpr_reserves, sizeof(before_gpr_reserves));
				memcpy(ctx->xmm_allocator, before_xmm_reserves, sizeof(before_xmm_reserves));
				
				// the return value
				if (dt.type == TB_VOID) {
					/* none */
				} else if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
					ctx->xmm_allocator[XMM0] = r;
					ctx->values[r] = val_xmm(dt, XMM0);
				} else {
					ctx->gpr_allocator[RAX] = r;
					ctx->values[r] = val_gpr(dt.type, RAX);
				}
				break;
			}
			
			case TB_INITIALIZE: {
				TB_Reg addr = n->mem_op.dst;
				
				TB_Module* m = f->module;
				TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_Initializer* i = (TB_Initializer*)&m->initializers[n->init.id / per_thread_stride][n->init.id % per_thread_stride];
				
				if (i->size >= 16) {
					// rep stosb, ol' reliable
					evict_gpr(ctx, f, RAX);
					evict_gpr(ctx, f, RCX);
					evict_gpr(ctx, f, RDI);
					
					{
						Val param = eval(ctx, f, addr);
						Val dst = val_gpr(TB_PTR, RDI);
						
						if (is_value_mem(&param) && is_address_node(f->nodes.data[addr].type)) {
							inst2(ctx, LEA, &dst, &param, TB_PTR);
						} else {
							if (!is_value_gpr(&param, RDI)) {
								inst2(ctx, MOV, &dst, &param, TB_PTR);
							}
						}
					}
					
					{
						Val dst = val_gpr(TB_PTR, RAX);
						inst2(ctx, XOR, &dst, &dst, TB_I32);
					}
					
					{
						Val dst = val_gpr(TB_PTR, RCX);
						Val src = val_imm(TB_TYPE_PTR, i->size);
						inst2(ctx, MOV, &dst, &src, TB_PTR);
					}
					
					// rep stosb
					emit(0xF3);
					emit(0xAA);
				} else {
					Val src = val_imm(TB_TYPE_I32, 0);
					
					Val dst = eval(ctx, f, addr);
					bool is_dst_temp = false;
					
					bool is_address = is_address_node(f->nodes.data[addr].type);
					if (dst.type == VAL_GPR) {
						dst = val_base_disp(TB_TYPE_PTR, dst.gpr, 0);
					} else if (dst.type == VAL_GLOBAL || !is_address) {
						Val new_dst = alloc_gpr(ctx, f, TB_TEMP_REG, TB_PTR);
						
						inst2(ctx, is_address ? LEA : MOV, &new_dst, &dst, TB_PTR);
						
						free_gpr(ctx, f, new_dst.gpr);
						dst = val_base_disp(TB_TYPE_PTR, new_dst.gpr, 0);
					}
					
					assert(is_value_mem(&dst));
					
					size_t sz = i->size;
					
					for (; sz >= 8; sz -= 8, dst.mem.disp += 8) {
						inst2(ctx, MOV, &dst, &src, TB_I64);
					}
					
					for (; sz >= 4; sz -= 4, dst.mem.disp += 4) {
						inst2(ctx, MOV, &dst, &src, TB_I32);
					}
					
					for (; sz >= 2; sz -= 2, dst.mem.disp += 2) {
						inst2(ctx, MOV, &dst, &src, TB_I16);
					}
					
					for (; sz >= 1; sz -= 1, dst.mem.disp += 1) {
						inst2(ctx, MOV, &dst, &src, TB_I8);
					}
				}
				
				kill(ctx, f, addr);
				break;
			}
			case TB_MEMSET: {
				TB_Reg dst_reg = n->mem_op.dst;
				TB_Reg val_reg = n->mem_op.src;
				TB_Reg size_reg = n->mem_op.size;
				
				// memset on constant size
				if (f->nodes.data[size_reg].type == TB_UNSIGNED_CONST &&
					f->nodes.data[size_reg].type == TB_SIGNED_CONST) {
					int64_t sz = f->nodes.data[size_reg].sint.value;
					assert(sz <= 0 && "Cannot memset on negative numbers");
					
					ctx->use_count[size_reg]--;
					assert(ctx->use_count[size_reg] >= 0);
					
					if (sz >= 512) {
						/* too big, just rep stos */
					} else if (sz >= 16) {
						// SSE memset
						/*do {
							// TODO(NeGate): we should try to use movaps when possible
							// movups 
							
							sz -= 16;
						} while (sz >= 16);
						break;*/
					} else if (sz >= 1) {
						if (f->nodes.data[val_reg].type == TB_UNSIGNED_CONST &&
							f->nodes.data[val_reg].type == TB_SIGNED_CONST) {
							assert(f->nodes.data[val_reg].uint.value == 0 && "TODO: Implement the fancy stuff soon");
							
							// tally down the value so it still counts like we used it
							ctx->use_count[val_reg]--;
							assert(ctx->use_count[val_reg] >= 0);
							
							Val src = val_imm(TB_TYPE_VOID, f->nodes.data[val_reg].uint.value);
							Val dst = eval(ctx, f, dst_reg);
							if (dst.type == VAL_GPR) {
								dst = val_base_disp(TB_TYPE_PTR, dst.gpr, 0);
							}
							
							for (; sz >= 8; sz -= 8, dst.mem.disp += 8) {
								inst2(ctx, MOV, &dst, &src, TB_I64);
							}
							
							for (; sz >= 4; sz -= 4, dst.mem.disp += 4) {
								inst2(ctx, MOV, &dst, &src, TB_I32);
							}
							
							for (; sz >= 2; sz -= 2, dst.mem.disp += 2) {
								inst2(ctx, MOV, &dst, &src, TB_I16);
							}
							
							for (; sz >= 1; sz -= 1, dst.mem.disp += 1) {
								inst2(ctx, MOV, &dst, &src, TB_I8);
							}
							
							kill(ctx, f, val_reg);
							kill(ctx, f, size_reg);
							kill(ctx, f, dst_reg);
							break;
						}
					}
				}
				
				// rep stosb, ol' reliable
				evict_gpr(ctx, f, RAX);
				evict_gpr(ctx, f, RCX);
				evict_gpr(ctx, f, RDI);
				
				{
					Val param = eval(ctx, f, dst_reg);
					if (!is_value_gpr(&param, RDI)) {
						Val dst = val_gpr(TB_PTR, RDI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					kill(ctx, f, dst_reg);
					ctx->gpr_allocator[RDI] = TB_TEMP_REG;
				}
				
				{
					Val param = eval(ctx, f, val_reg);
					if (!is_value_gpr(&param, RAX)) {
						Val dst = val_gpr(TB_PTR, RAX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					kill(ctx, f, val_reg);
					ctx->gpr_allocator[RAX] = TB_TEMP_REG;
				}
				
				{
					Val param = eval(ctx, f, size_reg);
					if (!is_value_gpr(&param, RCX)) {
						Val dst = val_gpr(TB_PTR, RCX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					kill(ctx, f, size_reg);
					ctx->gpr_allocator[RCX] = TB_TEMP_REG;
				}
				
				// rep stosb
				emit(0xF3);
				emit(0xAA);
				
				// free up stuff
				ctx->gpr_allocator[RAX] = TB_NULL_REG;
				ctx->gpr_allocator[RCX] = TB_NULL_REG;
				ctx->gpr_allocator[RDI] = TB_NULL_REG;
				break;
			}
			
			case TB_MEMCPY: {
				TB_Reg dst_reg = n->mem_op.dst;
				TB_Reg src_reg = n->mem_op.src;
				TB_Reg size_reg = n->mem_op.size;
				
				// TODO(NeGate): Implement vector memset
				// rep movsb, ol' reliable
				evict_gpr(ctx, f, RCX);
				evict_gpr(ctx, f, RSI);
				evict_gpr(ctx, f, RDI);
				
				{
					Val param = eval(ctx, f, dst_reg);
					Val dst = val_gpr(TB_PTR, RDI);
					if (is_value_mem(&param) && is_address_node(f->nodes.data[dst_reg].type)) {
						inst2(ctx, LEA, &dst, &param, TB_PTR);
					} else {
						if (!is_value_gpr(&param, RDI)) {
							inst2(ctx, MOV, &dst, &param, TB_PTR);
						}
					}
					kill(ctx, f, dst_reg);
					ctx->gpr_allocator[RDI] = TB_TEMP_REG;
				}
				
				{
					Val param = eval(ctx, f, src_reg);
					Val dst = val_gpr(TB_PTR, RSI);
					if (is_value_mem(&param) && is_address_node(f->nodes.data[src_reg].type)) {
						inst2(ctx, LEA, &dst, &param, TB_PTR);
					} else {
						if (!is_value_gpr(&param, RSI)) {
							inst2(ctx, MOV, &dst, &param, TB_PTR);
						}
					}
					kill(ctx, f, src_reg);
					ctx->gpr_allocator[RSI] = TB_TEMP_REG;
				}
				
				{
					Val param = eval(ctx, f, size_reg);
					if (!is_value_gpr(&param, RCX)) {
						Val dst = val_gpr(TB_PTR, RCX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					kill(ctx, f, size_reg);
					ctx->gpr_allocator[RCX] = TB_TEMP_REG;
				}
				
				// rep movsb
				emit(0xF3);
				emit(0xA4);
				
				// free up stuff
				ctx->gpr_allocator[RSI] = TB_NULL_REG;
				ctx->gpr_allocator[RDI] = TB_NULL_REG;
				ctx->gpr_allocator[RCX] = TB_NULL_REG;
				break;
			}
			
			case TB_ATOMIC_TEST_AND_SET: {
				assert(0 && "Atomic flag test & set not supported yet.");
				break;
			}
			case TB_ATOMIC_CLEAR: {
				assert(0 && "Atomic flag clear not supported yet.");
				break;
			}
			case TB_ATOMIC_XCHG:
			case TB_ATOMIC_ADD:
			case TB_ATOMIC_SUB:
			case TB_ATOMIC_AND:
			case TB_ATOMIC_XOR:
			case TB_ATOMIC_OR: {
				const static int tbl[] = { MOV, ADD, SUB, AND, XOR, OR };
				
				Val src = eval(ctx, f, n->atomic.src);
				Val addr = eval(ctx, f, n->atomic.addr);
				
				// sometimes we only need to do the operation atomic without
				// a fetch, then things get... fancy
				if (ctx->use_count[r]) {
					assert(0 && "TODO: Atomic operations with fetch.");
				} else {
					int op = tbl[reg_type - TB_ATOMIC_XCHG];
					
					// LOCK prefix is not needed on XCHG because
					// it's actually a MOV which is naturally atomic
					// when aligned.
					if (is_value_mem(&src)) {
						Val tmp = alloc_gpr(ctx, f, TB_TEMP_REG, dt.type);
						inst2(ctx, MOV, &tmp, &src, dt.type);
						
						if (reg_type != TB_ATOMIC_XCHG) emit(0xF0);
						
						inst2(ctx, op, &addr, &tmp, dt.type);
						free_gpr(ctx, f, tmp.gpr);
					} else {
						if (reg_type != TB_ATOMIC_XCHG) emit(0xF0);
						
						inst2(ctx, op, &addr, &src, dt.type);
					}
				}
				break;
			}
			case TB_ATOMIC_CMPXCHG: {
				assert(0 && "Atomic cmpxchg not supported yet.");
				break;
			}
			case TB_ATOMIC_CMPXCHG2: break;
			
			default:
			assert(0);
			break;
		}
	}
}
#endif
