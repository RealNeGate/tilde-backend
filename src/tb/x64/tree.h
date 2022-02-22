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
