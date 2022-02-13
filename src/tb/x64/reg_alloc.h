
#define TB_TEMP_REG INT_MAX

static void spill_reg(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
	TB_DataType dt = f->nodes.data[r].dt;
	Val src = ctx->values[r];
	
	if (src.type == VAL_GPR) {
		Val dst;
		if (r < TB_FIRST_PARAMETER_REG+f->prototype->param_count) {
			dst = val_stack(dt, 16 + ((r - TB_FIRST_PARAMETER_REG) * 8));
		} else {
			int size = get_data_type_size(dt);
			ctx->stack_usage = align_up(ctx->stack_usage + size, size);
			dst = val_stack(dt, -ctx->stack_usage);
		}
		
		if (dt.type == TB_BOOL) dt.type = TB_I8;
		
		inst2(ctx, MOV, &dst, &src, dt.type);
		
		ctx->values[r] = dst;
		ctx->gpr_allocator[src.gpr] = TB_NULL_REG;
	} else if (src.type == VAL_XMM) {
		Val dst;
		if (r < TB_FIRST_PARAMETER_REG+f->prototype->param_count) {
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
		ctx->xmm_allocator[src.xmm] = TB_NULL_REG;
	}
}

static Val alloc_gpr(Ctx* restrict ctx, TB_Function* f, TB_Reg r, int dt_type) {
	static const GPR PRIORITIES[] = {
		RAX, RCX, RDX, R8,
		R9,  R10, R11, RDI,
		RSI, RBX, R12, R13,
		R14, R15
	};
	
	loop(i, tb_arrlen(PRIORITIES)) {
		GPR gpr = PRIORITIES[i];
		
		if (ctx->gpr_allocator[gpr] == TB_NULL_REG) {
			ctx->gpr_allocator[gpr] = r;
			
			// mark register as to be saved
			ctx->regs_to_save |= (1u << gpr) & (ctx->is_sysv ? SYSV_ABI_CALLEE_SAVED : WIN64_ABI_CALLEE_SAVED);
			
			Val v = (Val) {
				.type = VAL_GPR,
				.dt.width = 0,
				.dt.type = dt_type,
				.gpr = gpr
			};
			
			if (r != TB_TEMP_REG) ctx->values[r] = v;
			return v;
		}
	}
	
	intptr_t most_distant = INT_MAX;
	GPR most_distant_gpr = GPR_NONE;
	loop_reverse(i, tb_arrlen(PRIORITIES)) {
		GPR gpr = PRIORITIES[i];
		TB_Reg reg = ctx->gpr_allocator[gpr];
		
		if (reg != TB_NULL_REG && reg != TB_TEMP_REG) {
			intptr_t dist = reg - r;
			
			// "God gives his silliest battles to his silliest clowns"
			if (dist < most_distant) {
				most_distant = dist;
				most_distant_gpr = gpr;
			}
		}
	}
	
	assert(most_distant_gpr != GPR_NONE);
	spill_reg(ctx, f, ctx->gpr_allocator[most_distant_gpr]);
	
	Val src = (Val) {
		.type = VAL_GPR,
		.dt.width = 0,
		.dt.type = dt_type,
		.gpr = most_distant_gpr
	};
	ctx->gpr_allocator[most_distant_gpr] = r;
	
	if (r != TB_TEMP_REG) ctx->values[r] = src;
	return src;
}

static Val alloc_xmm(Ctx* restrict ctx, TB_Function* f, TB_Reg r, TB_DataType dt) {
	loop(xmm, 16) {
		if (ctx->xmm_allocator[xmm] == TB_NULL_REG) {
			ctx->xmm_allocator[xmm] = r;
			
			// callee saves
			if (!ctx->is_sysv && xmm > 5) {
				ctx->regs_to_save |= (1u << (16 + xmm));
			}
			
			Val v = (Val) {
				.type = VAL_XMM,
				.dt = dt,
				.xmm = xmm
			};
			
			if (r != TB_TEMP_REG) ctx->values[r] = v;
			return v;
		}
	}
	
	assert(0 && "Spill");
	return (Val){ 0 };
}

static void free_gpr(Ctx* restrict ctx, TB_Function* f, GPR g) {
	assert(ctx->gpr_allocator[g] == TB_TEMP_REG);
	ctx->gpr_allocator[g] = TB_NULL_REG;
}

static void free_xmm(Ctx* restrict ctx, TB_Function* f, XMM x) {
	assert(ctx->xmm_allocator[x] == TB_TEMP_REG);
	ctx->xmm_allocator[x] = TB_NULL_REG;
}

static void free_val(Ctx* restrict ctx, TB_Function* f, TB_Reg r, Val val) {
	if (val.type == VAL_GPR) {
		ctx->gpr_allocator[val.gpr] = TB_NULL_REG;
	} else if (val.type == VAL_XMM) {
		ctx->xmm_allocator[val.xmm] = TB_NULL_REG;
	} else if (val.type == VAL_MEM) {
		assert(val.mem.index == GPR_NONE);
		ctx->gpr_allocator[val.mem.base] = TB_NULL_REG;
	}
}

static void kill(Ctx* restrict ctx, TB_Function* f, TB_Reg r, Val val) {
	if (val.is_temp) {
		free_val(ctx, f, r, val);
	}
	
	if (ctx->use_count[r] == 0) {
		free_val(ctx, f, r, ctx->values[r]);
		ctx->values[r] = (Val){ 0 };
	}
}

static bool evict_gpr(Ctx* restrict ctx, TB_Function* f, GPR g) {
	TB_Reg r = ctx->gpr_allocator[g];
	if (r == TB_NULL_REG) return true;
	if (r == TB_TEMP_REG) return false;
	
	assert(ctx->use_count[r] >= 0);
	if (ctx->use_count[r]) {
		TB_DataType dt = f->nodes.data[r].dt;
		if (dt.type == TB_BOOL) dt.type = TB_I8;
		
		if (f->nodes.data[r].type == TB_PHI2) {
			// spill into designated space
			PhiValue* phi = find_phi(ctx, r);
			if (phi->spill == 0) {
				int size = get_data_type_size(dt);
				ctx->stack_usage = align_up(ctx->stack_usage + size, size);
				
				phi->spill = -ctx->stack_usage;
			}
			
			assert(phi->value.type == VAL_GPR);
			Val dst = val_stack(dt, phi->spill);
			Val src = val_gpr(dt.type, phi->value.gpr);
			inst2(ctx, MOV, &dst, &src, dt.type);
		} else {
			assert(ctx->values[r].type == VAL_GPR && ctx->values[r].gpr == g);
			
			int size = get_data_type_size(dt);
			ctx->stack_usage = align_up(ctx->stack_usage + size, size);
			
			Val dst = val_stack(dt, -ctx->stack_usage);
			Val src = val_gpr(dt.type, g);
			inst2(ctx, MOV, &dst, &src, dt.type);
			
			ctx->values[r] = dst;
		}
	}
	
	ctx->gpr_allocator[g] = TB_NULL_REG;
	return true;
}

static bool evict_xmm(Ctx* restrict ctx, TB_Function* f, XMM x) {
	TB_Reg r = ctx->xmm_allocator[x];
	if (r == TB_NULL_REG) return true;
	if (r == TB_TEMP_REG) return false;
	
	assert(ctx->use_count[r] >= 0);
	if (ctx->use_count[r]) {
		TB_DataType dt = f->nodes.data[r].dt;
		
		uint8_t flags = 0;
		flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
		flags |= (dt.width) ? INST2FP_PACKED : 0;
		
		if (f->nodes.data[r].type == TB_PHI2) {
			// spill into designated space
			PhiValue* phi = find_phi(ctx, r);
			if (phi->spill == 0) {
				int size = get_data_type_size(dt);
				ctx->stack_usage = align_up(ctx->stack_usage + size, size);
				
				phi->spill = -ctx->stack_usage;
			}
			
			if (dt.type == TB_BOOL) dt.type = TB_I8;
			
			assert(phi->value.type == VAL_XMM);
			Val dst = val_stack(dt, phi->spill);
			Val src = val_xmm(dt, phi->value.xmm);
			inst2sse(ctx, FP_MOV, &dst, &src, flags);
		} else {
			assert(ctx->values[r].type == VAL_XMM && ctx->values[r].xmm == x);
			
			int size = get_data_type_size(dt);
			ctx->stack_usage = align_up(ctx->stack_usage + size, size);
			
			Val dst = val_stack(dt, -ctx->stack_usage);
			Val src = val_xmm(dt, x);
			inst2sse(ctx, FP_MOV, &dst, &src, flags);
			
			ctx->values[r] = dst;
		}
	}
	
	ctx->xmm_allocator[x] = TB_NULL_REG;
	return true;
}
