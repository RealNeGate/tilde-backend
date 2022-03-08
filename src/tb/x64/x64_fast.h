
typedef enum {
	ADDRESS_DESC_NONE,
	ADDRESS_DESC_GPR,
	ADDRESS_DESC_XMM,
	ADDRESS_DESC_FLAGS,
	ADDRESS_DESC_SPILL
} AddressDescType;

typedef struct {
	uint8_t type;
	TB_DataType dt;
	union {
		GPR gpr;
		XMM xmm;
		Cond flags;
		int spill;
	};
} AddressDesc;

typedef struct {
	size_t memory_usage;
	
	size_t locals_count;
	size_t return_count;
	size_t line_info_count;
	size_t label_patch_count;
} FunctionTallySimple;

typedef struct {
	X64_CtxHeader header;
	
	bool is_sysv;
	
	TB_Reg* use_count;
	
	TB_Reg* phis;
	uint32_t phi_count;
	
	uint32_t caller_usage;
	
	// Register allocation:
	TB_Reg gpr_allocator[16];
	TB_Reg xmm_allocator[16];
	
	AddressDesc addresses[];
} X64_FastCtx;

#define EITHER2(a, b, c) ((a) == (b) || (a) == (c))
#define EITHER3(a, b, c, d) ((a) == (b) || (a) == (c) || (a) == (d))
#define FITS_INTO(a, type) ((a) == ((type)(a)))

static bool is_address_node(TB_Function* f, TB_Reg r) {
	switch (f->nodes.data[r].type) {
		case TB_LOCAL:
		case TB_RESTRICT:
		case TB_PARAM_ADDR:
		case TB_EXTERN_ADDRESS:
		case TB_GLOBAL_ADDRESS:
		case TB_ARRAY_ACCESS:
		case TB_MEMBER_ACCESS:
		return true;
		
		default: 
		return false;
	}
}

static GPR fast_alloc_gpr(X64_FastCtx* restrict ctx, TB_Function* f, TB_Reg r) {
	static const GPR PRIORITIES[] = {
		RAX, RCX, RDX, R8,
		R9,  R10, R11, RDI,
		RSI, RBX, R12, R13,
		R14, R15
	};
	
	loop(i, COUNTOF(PRIORITIES)) {
		GPR gpr = PRIORITIES[i];
		
		if (ctx->gpr_allocator[gpr] == TB_NULL_REG) {
			ctx->gpr_allocator[gpr] = r;
			
			// mark register as to be saved
			ctx->header.regs_to_save |= (1u << gpr) & (ctx->is_sysv ? SYSV_ABI_CALLEE_SAVED : WIN64_ABI_CALLEE_SAVED);
			
			return gpr;
		}
	}
	
	// spilling
	tb_todo();
	return GPR_NONE;
}

static XMM fast_alloc_xmm(X64_FastCtx* restrict ctx, TB_Function* f, TB_Reg r) {
	loop(xmm, 16) {
		if (ctx->xmm_allocator[xmm] == TB_NULL_REG) {
			ctx->xmm_allocator[xmm] = r;
			
			// callee saves
			if (!ctx->is_sysv && xmm > 5) {
				ctx->header.regs_to_save |= (1u << (16 + xmm));
			}
			
			return xmm;
		}
	}
	
	// spilling
	tb_todo();
	return XMM_NONE;
}

static void fast_evict_gpr(X64_FastCtx* restrict ctx, TB_Function* f, GPR gpr) {
	if (ctx->gpr_allocator[gpr] == TB_NULL_REG ||
		ctx->gpr_allocator[gpr] == TB_TEMP_REG) {
		ctx->gpr_allocator[gpr] = TB_NULL_REG;
		return;
	}
	
	// Allocate stack slot and remap value into it
	TB_Reg r = ctx->gpr_allocator[gpr];
	TB_DataType dt = f->nodes.data[r].dt;
	if (dt.type == TB_BOOL) dt.type = TB_I8;
	
	ctx->gpr_allocator[gpr] = TB_NULL_REG;
	
	int size = get_data_type_size(dt);
	int pos = STACK_ALLOC(size, size);
	
	ctx->addresses[r] = (AddressDesc){
		.type = ADDRESS_DESC_SPILL, .dt = dt, .spill = pos
	};
	
	// Save out GPR into stack slot
	Val src = val_gpr(dt.type, gpr);
	Val dst = val_stack(dt, pos);
	INST2(MOV, &dst, &src, dt.type);
}

static void fast_kill_temp_gpr(X64_FastCtx* restrict ctx, TB_Function* f, GPR gpr) {
	if (ctx->gpr_allocator[gpr] == TB_TEMP_REG) {
		ctx->gpr_allocator[gpr] = TB_NULL_REG;
	}
}

static void fast_kill_reg(X64_FastCtx* restrict ctx, TB_Function* f, TB_Reg r) {
	if (ctx->use_count[r] == 0 && ctx->addresses[r].type == ADDRESS_DESC_GPR) {
		GPR gpr = ctx->addresses[r].gpr;
		
		assert(ctx->gpr_allocator[gpr] == r || ctx->gpr_allocator[gpr] == TB_TEMP_REG);
		ctx->gpr_allocator[gpr] = TB_NULL_REG;
	}
}

static Val fast_eval(X64_FastCtx* ctx, TB_Function* f, TB_Reg r) {
	TB_Node* restrict n = &f->nodes.data[r];
	TB_DataType dt = n->dt;
	
	ctx->use_count[r] -= 1;
	if (ctx->addresses[r].type) {
		switch (ctx->addresses[r].type) {
			case ADDRESS_DESC_GPR:
			assert(ctx->addresses[r].dt.width == 0);
			return val_gpr(dt.type, ctx->addresses[r].gpr);
			
			case ADDRESS_DESC_XMM: 
			return val_xmm(dt, ctx->addresses[r].xmm);
			
			case ADDRESS_DESC_SPILL: 
			return val_stack(dt, ctx->addresses[r].spill);
			
			case ADDRESS_DESC_FLAGS:
			return val_flags(ctx->addresses[r].flags);
			
			default: break;
		}
	} else {
		if (EITHER2(n->type, TB_UNSIGNED_CONST, TB_SIGNED_CONST) &&
			FITS_INTO(n->uint.value, int32_t)) {
			return val_imm(n->dt, n->uint.value);
		} else if (n->type == TB_GLOBAL_ADDRESS) {
			return val_global(n->global.value);
		}
	}
	
	tb_unreachable();
	return (Val){ 0 };
}

// OP lhs, eval(rhs)
static void fast_folded_op(X64_FastCtx* ctx, TB_Function* f, Inst2Type op, const Val* lhs, TB_Reg rhs_reg) {
	Val rhs = fast_eval(ctx, f, rhs_reg);
	
	TB_Node* restrict n = &f->nodes.data[rhs_reg];
	TB_DataType dt = n->dt;
	if (dt.type == TB_BOOL) dt.type = TB_I8;
	
	if (is_value_mem(&rhs) && n->type != TB_LOAD) {
		// TODO(NeGate): peephole to remove extra MOV in the op=MOV && lhs=GPR case
		Val tmp = val_gpr(TB_PTR, fast_alloc_gpr(ctx, f, TB_TEMP_REG));
		
		INST2(LEA, &tmp, &rhs, dt.type);
		INST2(op, lhs, &tmp, dt.type);
		
		fast_kill_temp_gpr(ctx, f, tmp.gpr);
	} else if (is_value_mem(lhs) && is_value_mem(&rhs)) {
		Val tmp = val_gpr(TB_PTR, fast_alloc_gpr(ctx, f, TB_TEMP_REG));
		
		INST2(MOV, &tmp, &rhs, dt.type);
		INST2(op, lhs, &tmp, dt.type);
		
		fast_kill_temp_gpr(ctx, f, tmp.gpr);
	} else {
		INST2(op, lhs, &rhs, dt.type);
	}
}

// (eval(src) != 0) ? 1 : 0
static Cond fast_eval_cond(X64_FastCtx* ctx, TB_Function* f, TB_Reg src_reg) {
	Val src = fast_eval(ctx, f, src_reg);
	
	TB_Node* restrict n = &f->nodes.data[src_reg];
	TB_DataType dt = n->dt;
	if (dt.type == TB_BOOL) dt.type = TB_I8;
	
	if (is_value_mem(&src) && n->type != TB_LOAD) {
		Val tmp = val_gpr(TB_PTR, fast_alloc_gpr(ctx, f, TB_TEMP_REG));
		INST2(LEA, &tmp, &src, dt.type);
		
		// early-kill: this is fine here because no allocations are made
		// between here and the end of the function (the time it actually
		// should be killed)
		fast_kill_temp_gpr(ctx, f, tmp.gpr);
	}
	
	// TODO(NeGate): regalloc
	if (is_value_mem(&src)) {
		Val imm = val_imm(TB_TYPE_I32, 0);
		INST2(CMP, &src, &imm, dt.type);
		return NE;
	} else if (src.type == VAL_GPR) {
		INST2(TEST, &src, &src, dt.type);
		return NE;
	} else if (src.type == VAL_IMM) {
		Val tmp = val_gpr(TB_I32, fast_alloc_gpr(ctx, f, TB_TEMP_REG));
		
		// 'xor a, a' will set ZF to 1
		INST2(XOR, &src, &src, dt.type);
		fast_kill_temp_gpr(ctx, f, tmp.gpr);
		
		tb_todo(); // verify this
		return (src.imm ? NE : E);
	} else if (src.type == VAL_FLAGS) {
		return src.cond;
	} else {
		tb_todo();
		return NE;
	}
}

static Val fast_eval_address(X64_FastCtx* ctx, TB_Function* f, TB_Reg r) {
	Val address = fast_eval(ctx, f, r);
	
	TB_Node* restrict n = &f->nodes.data[r];
	TB_DataType dt = n->dt;
	
	if (address.type == VAL_GPR) {
		return val_base_disp(TB_TYPE_PTR, address.gpr, 0);
	} else if (is_value_mem(&address) && n->type == TB_LOAD) {
		// TODO(NeGate): regalloc
		Val tmp = val_gpr(TB_PTR, R15);
		INST2(MOV, &tmp, &address, dt.type);
		return tmp;
	} else {
		return address;
	}
}

static void fast_def_gpr(X64_FastCtx* restrict ctx, TB_Function* f, TB_Reg r, GPR gpr, TB_DataType dt) {
	ctx->addresses[r] = (AddressDesc){
		.type = ADDRESS_DESC_GPR, .dt = dt, .gpr = gpr
	};
}

static void fast_def_xmm(X64_FastCtx* restrict ctx, TB_Function* f, TB_Reg r, XMM xmm, TB_DataType dt) {
	ctx->addresses[r] = (AddressDesc){
		.type = ADDRESS_DESC_XMM, .dt = dt, .xmm = xmm
	};
}

static void fast_def_spill(X64_FastCtx* restrict ctx, TB_Function* f, TB_Reg r, int spill, TB_DataType dt) {
	ctx->addresses[r] = (AddressDesc){
		.type = ADDRESS_DESC_SPILL, .dt = dt, .spill = spill
	};
}

static void fast_def_flags(X64_FastCtx* restrict ctx, TB_Function* f, TB_Reg r, Cond cc, TB_DataType dt) {
	ctx->addresses[r] = (AddressDesc){
		.type = ADDRESS_DESC_FLAGS, .dt = dt, .flags = cc
	};
}

static void fast_memset_const_size(X64_FastCtx* restrict ctx, TB_Function* f, TB_Reg addr, const Val* src, size_t sz) {
	Val dst = fast_eval_address(ctx, f, addr);
	assert(is_value_mem(&dst));
	
	for (; sz >= 8; sz -= 8, dst.mem.disp += 8) {
		INST2(MOV, &dst, src, TB_I64);
	}
	
	for (; sz >= 4; sz -= 4, dst.mem.disp += 4) {
		INST2(MOV, &dst, src, TB_I32);
	}
	
	for (; sz >= 2; sz -= 2, dst.mem.disp += 2) {
		INST2(MOV, &dst, src, TB_I16);
	}
	
	for (; sz >= 1; sz -= 1, dst.mem.disp += 1) {
		INST2(MOV, &dst, src, TB_I8);
	}
}

static void fast_eval_basic_block(X64_FastCtx* restrict ctx, TB_Function* f, TB_Reg bb, TB_Reg bb_end) {
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
			case TB_PHI2:
			case TB_PARAM_ADDR:
			case TB_GLOBAL_ADDRESS:
			break;
			case TB_EXTERN_ADDRESS:
			case TB_FUNC_ADDRESS: {
				GPR dst_gpr = fast_alloc_gpr(ctx, f, r);
				fast_def_gpr(ctx, f, r, dst_gpr, TB_TYPE_PTR);
				
				*ctx->header.out++ = rex(true, dst_gpr, RBP, 0);
				*ctx->header.out++ = 0x8D;
				*ctx->header.out++ = mod_rx_rm(MOD_INDIRECT, dst_gpr, RBP);
				*((uint64_t*) ctx->header.out) = 0;
				ctx->header.out += 4;
				
				if (reg_type == TB_EXTERN_ADDRESS) {
					tb_emit_ecall_patch(f->module, s_compiled_func_id, n->external.value, GET_CODE_POS() - 4, s_local_thread_id);
				} else {
					int target_func = n->func.value - f->module->functions.data;
					tb_emit_call_patch(f->module, s_compiled_func_id, target_func, GET_CODE_POS() - 4, s_local_thread_id);
				}
				break;
			}
			case TB_SIGNED_CONST:
			case TB_UNSIGNED_CONST:
			if (!FITS_INTO(n->uint.value, int32_t)) {
				GPR dst_gpr = fast_alloc_gpr(ctx, f, r);
				fast_def_gpr(ctx, f, r, dst_gpr, TB_TYPE_PTR);
				
				*ctx->header.out++ = rex(true, dst_gpr, RBP, 0);
				*ctx->header.out++ = 0xB8 + (dst_gpr & 7);
				*((uint64_t*) ctx->header.out) = n->uint.value;
				ctx->header.out += 8;
			}
			break;
			case TB_FLOAT_CONST: {
				assert(TB_IS_FLOAT_TYPE(dt.type) && dt.width == 0);
				uint64_t imm = (Cvt_F64U64){ .f = n->flt.value }.i;
				
				XMM dst_xmm = fast_alloc_xmm(ctx, f, r);
				fast_def_xmm(ctx, f, r, dst_xmm, TB_TYPE_PTR);
				
				if (imm == 0) {
					if (dst_xmm >= 8) {
						*ctx->header.out++ = rex(true, dst_xmm, dst_xmm, 0);
					}
					*ctx->header.out++ = 0x0F;
					*ctx->header.out++ = 0x57;
					*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, dst_xmm, dst_xmm);
				} else {
					// Convert it to raw bits
					if (dst_xmm >= 8) {
						*ctx->header.out++ = rex(true, dst_xmm, dst_xmm, 0);
					}
					*ctx->header.out++ = dt.type == TB_F64 ? 0xF2 : 0xF3;
					*ctx->header.out++ = 0x0F;
					*ctx->header.out++ = 0x10;
					*ctx->header.out++ = ((dst_xmm & 7) << 3) | RBP;
					
					uint32_t disp;
					if (dt.type == TB_F64) {
						uint64_t* rdata_payload = tb_platform_arena_alloc(sizeof(uint64_t));
						*rdata_payload = imm;
						
						disp = tb_emit_const_patch(f->module, s_compiled_func_id, GET_CODE_POS(), rdata_payload, sizeof(uint64_t), s_local_thread_id);
					} else {
						uint32_t imm32 = (Cvt_F32U32){ .f = n->flt.value }.i;
						
						uint32_t* rdata_payload = tb_platform_arena_alloc(sizeof(uint32_t));
						*rdata_payload = imm32;
						
						disp = tb_emit_const_patch(f->module, s_compiled_func_id, GET_CODE_POS(), rdata_payload, sizeof(uint32_t), s_local_thread_id);
					}
					
					*((uint32_t*) ctx->header.out) = disp;
					ctx->header.out += 4;
				}
				break;
			}
			case TB_STRING_CONST: {
				const char* str = n->string.data;
				size_t len = n->string.length;
				
				GPR dst_gpr = fast_alloc_gpr(ctx, f, r);
				fast_def_gpr(ctx, f, r, dst_gpr, TB_TYPE_PTR);
				
				*ctx->header.out++ = rex(true, dst_gpr, RBP, 0);
				*ctx->header.out++ = 0x8D;
				*ctx->header.out++ = mod_rx_rm(MOD_INDIRECT, dst_gpr, RBP);
				
				uint32_t disp = tb_emit_const_patch(f->module, s_compiled_func_id, GET_CODE_POS(), str, len, s_local_thread_id);
				
				*((uint32_t*) ctx->header.out) = disp;
				ctx->header.out += 4;
				break;
			}
			
			case TB_LINE_INFO: {
				f->lines[f->line_count++] = (TB_Line){
					.file = n->line_info.file,
					.line = n->line_info.line,
					.pos = GET_CODE_POS()
				};
				break;
			}
			
			case TB_DEBUGBREAK: {
				*ctx->header.out++ = 0xCC;
				break;
			}
			
			case TB_VA_START: {
				assert(!ctx->is_sysv && "How does va_start even work on SysV?");
				
				// on Win64 va_start just means whatever is one parameter away from
				// the parameter you give it (plus in Win64 the parameters in the stack
				// are 8bytes, no fanciness like in SysV):
				// void printf(const char* fmt, ...) {
				//     va_list args;
				//     va_start(args, fmt); // args = (char*) (((uintptr_t) &fmt) + 8);
				//     ...
				// }
				Val addr = fast_eval_address(ctx, f, n->unary.src);
				assert(addr.type == VAL_MEM);
				addr.mem.disp += 8;
				
				GPR dst_gpr = fast_alloc_gpr(ctx, f, r);
				fast_def_gpr(ctx, f, r, dst_gpr, TB_TYPE_PTR);
				
				Val dst = val_gpr(dt.type, dst_gpr);
				INST2(LEA, &dst, &addr, dt.type);
				break;
			}
			
			case TB_MEMBER_ACCESS: {
				Val addr = fast_eval_address(ctx, f, n->member_access.base);
				assert(addr.type == VAL_MEM || addr.type == VAL_GLOBAL);
				addr.mem.disp += n->member_access.offset;
				
				GPR dst_gpr = fast_alloc_gpr(ctx, f, r);
				fast_def_gpr(ctx, f, r, dst_gpr, TB_TYPE_PTR);
				
				Val dst = val_gpr(dt.type, dst_gpr);
				INST2(LEA, &dst, &addr, dt.type);
				break;
			}
			case TB_ARRAY_ACCESS: {
				// it's called fast isel for a reason and it's definetely not
				// because of the codegen quality...
				uint32_t stride = n->array_access.stride;
				
				Val val = val_gpr(TB_PTR, fast_alloc_gpr(ctx, f, r));
				fast_folded_op(ctx, f, MOV, &val, n->array_access.index);
				fast_kill_reg(ctx, f, n->array_access.index);
				
				// if it's an LEA index*stride
				// then stride > 0, if not it's free
				// do think of it however
				GPR index_reg = val.gpr;
				uint8_t stride_as_shift = 0;
				
				if (tb_is_power_of_two(stride)) {
					stride_as_shift = tb_ffs(stride) - 1;
					
					if (stride_as_shift > 3) {
						assert(stride_as_shift < 64 && "Stride to big!!!");
						
						// shl index, stride_as_shift
						*ctx->header.out++ = rex(true, 0x04, val.gpr, 0);
						*ctx->header.out++ = 0xC1;
						*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, 0x04, val.gpr);
						*ctx->header.out++ = stride_as_shift;
						
						stride_as_shift = 0; // pre-multiplied, don't propagate
					}
				} else {
					// imul dst, index, stride
					*ctx->header.out++ = rex(true, val.gpr, val.gpr, 0);
					*ctx->header.out++ = 0x69;
					*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, val.gpr, val.gpr);
					
					*((uint32_t*) ctx->header.out) = stride;
					ctx->header.out += 4;
					
					stride_as_shift = 0; // pre-multiplied, don't propagate
				}
				
				// post conditions :)
				assert(index_reg != GPR_NONE);
				assert(stride_as_shift >= 0 && stride_as_shift <= 3 && "stride_as_shift can't fit into an LEA");
				
				// Resolve base (if it's not already in a register)
				if (stride_as_shift) {
					// TODO(NeGate): Maybe i should couple these bad boys
					Val temp = val_gpr(TB_PTR, fast_alloc_gpr(ctx, f, r));
					
					fast_folded_op(ctx, f, MOV, &val, n->array_access.base);
					
					Val arith = val_base_index(TB_TYPE_PTR, temp.gpr, index_reg, stride_as_shift);
					INST2(LEA, &val, &arith, TB_PTR);
					
					fast_kill_temp_gpr(ctx, f, temp.gpr);
				} else {
					fast_folded_op(ctx, f, ADD, &val, n->array_access.base);
				}
				
				assert(val.type == VAL_GPR);
				fast_def_gpr(ctx, f, r, val.gpr, TB_TYPE_PTR);
				fast_kill_reg(ctx, f, n->array_access.base);
				break;
			}
			case TB_LOAD: {
				Val addr = fast_eval_address(ctx, f, n->store.address);
				
				if (TB_IS_FLOAT_TYPE(dt.type) || dt.width) {
					tb_todo();
				} else {
					GPR dst_gpr = fast_alloc_gpr(ctx, f, r);
					fast_def_gpr(ctx, f, r, dst_gpr, dt);
					
					Val dst = val_gpr(dt.type, dst_gpr);
					INST2(MOV, &dst, &addr, dt.type);
				}
				break;
			}
			case TB_STORE: {
				Val dst = fast_eval_address(ctx, f, n->store.address);
				
				if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
					// TODO(NeGate): Handle vector and float types
					tb_todo();
				} else {
					fast_folded_op(ctx, f, MOV, &dst, n->store.value);
				}
				break;
			}
			case TB_INITIALIZE: {
				TB_Reg addr = n->mem_op.dst;
				
				TB_Module* m = f->module;
				TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_Initializer* i = (TB_Initializer*)&m->initializers[n->init.id / per_thread_stride][n->init.id % per_thread_stride];
				
				Val src = val_imm(TB_TYPE_I32, 0);
				fast_memset_const_size(ctx, f, addr, &src, i->size);
				
				fast_kill_reg(ctx, f, addr);
				break;
			}
			
			// Integer binary operations
			case TB_AND:
			case TB_OR:
			case TB_XOR:
			case TB_ADD:
			case TB_SUB:
			case TB_MUL: {
				GPR dst_gpr = fast_alloc_gpr(ctx, f, r);
				fast_def_gpr(ctx, f, r, dst_gpr, dt);
				
				Val dst = val_gpr(dt.type, dst_gpr);
				{
					// simple scalar ops
					const static Inst2Type ops[] = { AND, OR, XOR, ADD, SUB, IMUL };
					
					fast_folded_op(ctx, f, MOV, &dst, n->i_arith.a);
					fast_folded_op(ctx, f, ops[reg_type - TB_AND], &dst, n->i_arith.b);
				}
				
				fast_kill_reg(ctx, f, n->i_arith.a);
				fast_kill_reg(ctx, f, n->i_arith.b);
				break;
			}
			case TB_UDIV:
			case TB_SDIV:
			case TB_UMOD:
			case TB_SMOD: {
				assert(dt.width == 0 && "TODO: Implement vector integer division and modulo");
				
				bool is_signed = (reg_type == TB_SDIV || reg_type == TB_SMOD);
				bool is_div = (reg_type == TB_UDIV || reg_type == TB_SDIV);
				
				fast_evict_gpr(ctx, f, RAX);
				
				// MOV rax, a
				Val rcx = val_gpr(dt.type, RAX);
				fast_folded_op(ctx, f, MOV, &rcx, n->i_arith.a);
				ctx->gpr_allocator[RAX] = TB_TEMP_REG;
				
				if (is_signed) {
					// cqo/cdq
					if (dt.type == TB_PTR || dt.type == TB_I64) {
						*ctx->header.out++ = 0x48;
					}
					*ctx->header.out++ = 0x99;
				} else {
					// xor rdx, rdx
					*ctx->header.out++ = 0x31;
					*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, RDX, RDX);
				}
				
				{
					Val tmp = val_gpr(dt.type, fast_alloc_gpr(ctx, f, TB_TEMP_REG));
					
					fast_folded_op(ctx, f, MOV, &tmp, n->i_arith.b);
					INST1(IDIV, &tmp);
					
					fast_kill_temp_gpr(ctx, f, tmp.gpr);
				}
				
				// the return value is in RAX for division
				// and RDX for modulo
				fast_def_gpr(ctx, f, r, is_div ? RAX : RDX, dt);
				
				// free the other piece of the divmod result
				ctx->gpr_allocator[is_div ? RDX : RAX] = TB_NULL_REG;
				break;
			}
			case TB_SHR:
			case TB_SHL:
			case TB_SAR: {
				bool is_64bit = dt.type == TB_I64;
				
				Val dst = val_gpr(dt.type, fast_alloc_gpr(ctx, f, r));
				fast_def_gpr(ctx, f, r, dst.gpr, dt);
				
				fast_folded_op(ctx, f, MOV, &dst, n->i_arith.a);
				
				if (EITHER2(f->nodes.data[n->i_arith.b].type, TB_UNSIGNED_CONST, TB_SIGNED_CONST)) {
					uint64_t imm = f->nodes.data[n->i_arith.b].uint.value;
					assert(imm < 64);
					
					// C1 /4       shl r/m, imm
					// C1 /5       shr r/m, imm
					// C1 /7       sar r/m, imm
					if (dt.type == TB_I16) *ctx->header.out++ = 0x66;
					*ctx->header.out++ = rex(is_64bit, 0x00, dst.gpr, 0x00);
					*ctx->header.out++ = (dt.type == TB_I8 ? 0xC0 : 0xC1);
					switch (reg_type) {
						case TB_SHL:
						*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, 0x04, dst.gpr);
						break;
						case TB_SHR:
						*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, 0x05, dst.gpr);
						break;
						case TB_SAR:
						*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, 0x07, dst.gpr);
						break;
						default: tb_unreachable();
					}
					*ctx->header.out++ = imm;
					
					fast_kill_reg(ctx, f, n->i_arith.a);
					fast_kill_reg(ctx, f, n->i_arith.b);
					break;
				}
				
				// MOV rcx, b
				Val rcx = val_gpr(dt.type, RCX);
				fast_folded_op(ctx, f, MOV, &rcx, n->i_arith.b);
				
				// D2 /4       shl r/m, cl
				// D2 /5       shr r/m, cl
				// D2 /7       sar r/m, cl
				if (dt.type == TB_I16) *ctx->header.out++ = 0x66;
				*ctx->header.out++ = rex(is_64bit, 0x00, dst.gpr, 0x00);
				*ctx->header.out++ = (dt.type == TB_I8 ? 0xD2 : 0xD3);
				switch (reg_type) {
					case TB_SHL:
					*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, 0x04, dst.gpr);
					break;
					case TB_SHR:
					*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, 0x05, dst.gpr);
					break;
					case TB_SAR:
					*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, 0x07, dst.gpr);
					break;
					default:
					tb_unreachable();
				}
				
				fast_kill_reg(ctx, f, n->i_arith.a);
				fast_kill_reg(ctx, f, n->i_arith.b);
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
				
				// TODO(NeGate): add some simple const folding here... maybe?
				// if (cmp XX (a, b)) should return a FLAGS because the IF
				// will handle it properly
				bool returns_flags = ctx->use_count[r] == 1 && 
					f->nodes.data[n->next].type == TB_IF &&
					f->nodes.data[n->next].if_.cond == r;
				
				GPR dst_gpr = fast_alloc_gpr(ctx, f, r);
				fast_def_gpr(ctx, f, r, dst_gpr, dt);
				Val val = val_gpr(cmp_dt.type, dst_gpr);
				
				if (!returns_flags) {
					// xor temp, temp
					if (val.gpr >= 8) {
						*ctx->header.out++ = rex(false, val.gpr, val.gpr, 0);
					}
					*ctx->header.out++ = 0x31;
					*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, val.gpr, val.gpr);
				}
				
				Cond cc;
				if (TB_IS_FLOAT_TYPE(cmp_dt.type)) {
					tb_todo();
				} else {
					bool invert = (f->nodes.data[n->i_arith.a].type == TB_UNSIGNED_CONST ||
								   f->nodes.data[n->i_arith.a].type == TB_SIGNED_CONST);
					
					if (invert) {
						fast_folded_op(ctx, f, MOV, &val, n->i_arith.b);
						fast_folded_op(ctx, f, CMP, &val, n->i_arith.a);
					} else {
						fast_folded_op(ctx, f, MOV, &val, n->i_arith.a);
						fast_folded_op(ctx, f, CMP, &val, n->i_arith.b);
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
					assert(val.type == VAL_GPR);
					if (val.gpr >= 8) {
						*ctx->header.out++ = rex(true, val.gpr, val.gpr, 0);
					}
					*ctx->header.out++ = 0x0F;
					*ctx->header.out++ = 0x90 + cc;
					*ctx->header.out++ = mod_rx_rm(MOD_DIRECT, val.gpr, val.gpr);
				}
				
				fast_kill_reg(ctx, f, n->cmp.a);
				fast_kill_reg(ctx, f, n->cmp.b);
				break;
			}
			
			case TB_BITCAST: {
				assert(dt.width == 0 && "TODO: Implement vector bitcast");
				
				Val src = fast_eval(ctx, f, n->unary.src);
				assert(get_data_type_size(dt) == get_data_type_size(src.dt));
				
				// movd/q
				*ctx->header.out++ = 0x66;
				
				Val val;
				if ((src.dt.type == TB_I64 || src.dt.type == TB_PTR) &&
					dt.type == TB_F64) {
					// movd dst:xmm, src:gpr
					assert(src.type == VAL_GPR);
					val = val_xmm(dt, fast_alloc_gpr(ctx, f, r));
					fast_def_xmm(ctx, f, r, val.xmm, dt);
					
					*ctx->header.out++ = rex(true, src.gpr, val.xmm, 0);
					*ctx->header.out++ = 0x0F;
					*ctx->header.out++ = 0x6E;
				} else if (src.dt.type == TB_I32 && dt.type == TB_F32) {
					// movq dst:xmm, src:gpr
					assert(src.type == VAL_GPR);
					val = val_xmm(dt, fast_alloc_gpr(ctx, f, r));
					fast_def_xmm(ctx, f, r, val.xmm, dt);
					
					if (val.xmm >= 8 || src.gpr >= 8) {
						*ctx->header.out++ = rex(false, src.gpr, val.xmm, 0);
					}
					
					*ctx->header.out++ = 0x0F;
					*ctx->header.out++ = 0x6E;
				} else if (src.dt.type == TB_F64 &&
						   (dt.type == TB_I64 || dt.type == TB_PTR)) {
					// movd dst:gpr, src:xmm
					assert(src.type == VAL_XMM);
					val = val_gpr(dt.type, fast_alloc_gpr(ctx, f, r));
					fast_def_gpr(ctx, f, r, val.gpr, dt);
					
					*ctx->header.out++ = rex(true, src.xmm, val.gpr, 0);
					*ctx->header.out++ = 0x0F;
					*ctx->header.out++ = 0x7E;
				} else if (src.dt.type == TB_F32 && dt.type == TB_I32) {
					// movq dst:gpr, src:xmm
					assert(src.type == VAL_XMM);
					val = val_gpr(dt.type, fast_alloc_gpr(ctx, f, r));
					fast_def_gpr(ctx, f, r, val.gpr, dt);
					
					if (val.gpr >= 8 || src.xmm >= 8) {
						*ctx->header.out++ = rex(true, src.xmm, val.gpr, 0);
					}
					
					*ctx->header.out++ = 0x0F;
					*ctx->header.out++ = 0x7E;
				} else tb_todo();
				
				// val.gpr and val.xmm alias so it's irrelevant which one we pick
				emit_memory_operand(&ctx->header, src.gpr, &val);
				
				fast_kill_reg(ctx, f, n->unary.src);
				break;
			}
			// realistically TRUNCATE doesn't need to do shit on integers :p
			case TB_TRUNCATE: {
				assert(dt.width == 0 && "TODO: Implement vector truncate");
				
				if (TB_IS_FLOAT_TYPE(dt.type)) {
					tb_todo();
				} else {
					// we probably want some recycling eventually...
					GPR dst_gpr = fast_alloc_gpr(ctx, f, r);
					fast_def_gpr(ctx, f, r, dst_gpr, dt);
					Val val = val_gpr(dt.type, dst_gpr);
					
					fast_folded_op(ctx, f, MOV, &val, n->unary.src);
					
					fast_kill_reg(ctx, f, n->unary.src);
				}
				break;
			}
			case TB_NOT:
			case TB_NEG: {
				assert(dt.width == 0 && "TODO: Implement vector negate");
				bool is_not = reg_type == TB_NOT;
				
				if (TB_IS_FLOAT_TYPE(dt.type)) {
					tb_todo();
				} else {
					// we probably want some recycling eventually...
					GPR dst_gpr = fast_alloc_gpr(ctx, f, r);
					fast_def_gpr(ctx, f, r, dst_gpr, dt);
					Val val = val_gpr(dt.type, dst_gpr);
					
					fast_folded_op(ctx, f, MOV, &val, n->unary.src);
					INST1(is_not ? NOT : NEG, &val);
					
					fast_kill_reg(ctx, f, n->unary.src);
				}
				break;
			}
			
			case TB_INT2PTR:
			case TB_SIGN_EXT:
			case TB_ZERO_EXT: {
				assert(dt.width == 0 && "TODO: Implement vector zero extend");
				TB_DataType src_dt = f->nodes.data[n->unary.src].dt;
				bool sign_ext = (reg_type == TB_SIGN_EXT);
				
				GPR dst_gpr = fast_alloc_gpr(ctx, f, r);
				fast_def_gpr(ctx, f, r, dst_gpr, dt);
				Val val = val_gpr(dt.type, dst_gpr);
				
				if (src_dt.type == TB_I32 && sign_ext) {
					fast_folded_op(ctx, f, MOVSXD, &val, n->unary.src);
				} else if (src_dt.type == TB_I16) {
					fast_folded_op(ctx, f, sign_ext ? MOVSXW : MOVZXW, &val, n->unary.src);
				} else if (src_dt.type == TB_I8 || src_dt.type == TB_BOOL) {
					fast_folded_op(ctx, f, sign_ext ? MOVSXB : MOVZXB, &val, n->unary.src);
				} else {
					fast_folded_op(ctx, f, MOV, &val, n->unary.src);
				}
				
				fast_kill_reg(ctx, f, n->unary.src);
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
					fast_evict_gpr(ctx, f, j);
				}
				
				// TODO(NeGate): Evict the XMMs that are caller saved
				/*loop_range(j, ctx->is_sysv ? 0 : 5, 16) {
					evict_xmm(ctx, f, j);
				}*/
				
				// reserve return value
				if (TB_IS_FLOAT_TYPE(dt.type) || dt.width) {
					ctx->xmm_allocator[XMM0] = TB_TEMP_REG;
				} else if (dt.type != TB_VOID) {
					ctx->gpr_allocator[RAX] = TB_TEMP_REG;
				}
				
				// evaluate parameters
				loop(j, param_count) {
					TB_Reg param_reg = f->vla.data[param_start + j];
					TB_DataType param_dt = f->nodes.data[param_reg].dt;
					
					if (TB_IS_FLOAT_TYPE(param_dt.type) || param_dt.width) {
						tb_todo();
					} else if (param_dt.type != TB_VOID) {
						// Win64 has 4 GPR parameters (RCX, RDX, R8, R9)
						// SysV has 6 of them (RDI, RSI, RDX, RCX, R8, R9)
						if ((ctx->is_sysv && j < 6) || j < 4) {
							Val dst = val_gpr(param_dt.type, parameter_gprs[j]);
							
							// move to parameter GPR and reserve it
							fast_folded_op(ctx, f, MOV, &dst, param_reg);
							ctx->gpr_allocator[parameter_gprs[j]] = TB_TEMP_REG;
						} else {
							Val dst = val_base_disp(param_dt, RSP, 8 * j);
							fast_folded_op(ctx, f, MOV, &dst, param_reg);
						}
					}
				}
				
				// CALL instruction and patch
				if (reg_type == TB_CALL) {
					TB_FunctionID target = n->call.target - f->module->functions.data;
					
					tb_emit_call_patch(f->module,
									   s_compiled_func_id,
									   target,
									   GET_CODE_POS() + 1,
									   s_local_thread_id);
					
					// CALL rel32
					ctx->header.out[0] = 0xE8;
					*((uint32_t*) &ctx->header.out[1]) = 0x0;
					ctx->header.out += 5;
				} else if (reg_type == TB_ECALL) {
					TB_ExternalID target = n->ecall.target;
					
					tb_emit_ecall_patch(f->module,
										s_compiled_func_id,
										target,
										GET_CODE_POS() + 1,
										s_local_thread_id);
					
					// CALL rel32
					ctx->header.out[0] = 0xE8;
					*((uint32_t*) &ctx->header.out[1]) = 0x0;
					ctx->header.out += 5;
				} else if (reg_type == TB_VCALL) {
					Val target = fast_eval_address(ctx, f, n->vcall.target);
					
					// call r/m64
					INST1(CALL_RM, &target);
					
					fast_kill_reg(ctx, f, n->vcall.target);
				}
				
				// get rid of all those reserved TEMP_REGs
				loop(i, 16) if (ctx->gpr_allocator[i] == TB_TEMP_REG) {
					ctx->gpr_allocator[i] = TB_NULL_REG;
				}
				
				// the return value
				if (dt.type == TB_VOID) {
					/* none */
				} else if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
					ctx->xmm_allocator[XMM0] = r;
					fast_def_xmm(ctx, f, r, XMM0, dt);
				} else {
					ctx->gpr_allocator[RAX] = r;
					fast_def_gpr(ctx, f, r, RAX, dt);
				}
				break;
			}
			
			default: tb_todo();
		}
	}
}

static void fast_eval_terminator_phis(X64_FastCtx* restrict ctx, TB_Function* f, TB_Reg from, TB_Reg from_terminator, TB_Reg to, TB_Reg to_terminator) {
	TB_FOR_EACH_NODE_RANGE(n, f, to, to_terminator) if (n->type == TB_PHI2) {
		TB_Reg r = n - f->nodes.data;
		TB_DataType dt = n->dt;
		
		if (dt.type == TB_BOOL) dt.type = TB_I8;
		
		TB_Reg src = (n->phi2.a_label == from ? n->phi2.a :
					  n->phi2.b_label == from ? n->phi2.b :
					  0);
		
		Val dst;
		if (ctx->addresses[r].type == ADDRESS_DESC_NONE) {
			int size = get_data_type_size(dt);
			int pos = STACK_ALLOC(size, size);
			
			dst = val_stack(dt, pos);
			fast_def_spill(ctx, f, r, pos, dt);
		} else {
			assert(ctx->addresses[r].type == ADDRESS_DESC_SPILL);
			dst = val_stack(dt, ctx->addresses[r].spill);
		}
		
		if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
			// TODO(NeGate): Handle vector and float types
			tb_todo();
		} else {
			fast_folded_op(ctx, f, MOV, &dst, src);
		}
	}
}

static FunctionTallySimple tally_memory_usage_simple(TB_Function* restrict f) {
	size_t locals_count = 0;
	size_t return_count = 0;
	size_t label_patch_count = 0;
	size_t line_info_count = 0;
	
	TB_FOR_EACH_NODE(n, f) {
		TB_NodeTypeEnum t = n->type;
		
		if (t == TB_RET) return_count++;
		else if (t == TB_LOCAL) locals_count++;
		else if (t == TB_IF) label_patch_count += 2;
		else if (t == TB_GOTO) label_patch_count++;
		else if (t == TB_LINE_INFO) line_info_count++;
		else if (t == TB_SWITCH) {
			label_patch_count += 1 + ((n->switch_.entries_end - n->switch_.entries_start) / 2);
		}
	}
	
	// parameters are locals too... ish
	locals_count += f->prototype->param_count;
	
	size_t align_mask = _Alignof(long double)-1;
	size_t tally = 0;
	
	// context
	tally += sizeof(X64_FastCtx) + (f->nodes.count * sizeof(AddressDesc));
	tally = (tally + align_mask) & ~align_mask;
	
	// use_count
	tally += f->nodes.count * sizeof(TB_Reg);
	tally = (tally + align_mask) & ~align_mask;
	
	// intervals
	tally += f->nodes.count * sizeof(TB_Reg);
	tally = (tally + align_mask) & ~align_mask;
	
	// labels
	tally += f->label_count * sizeof(uint32_t);
	tally = (tally + align_mask) & ~align_mask;
	
	// label_patches
	tally += label_patch_count * sizeof(LabelPatch);
	tally = (tally + align_mask) & ~align_mask;
	
	// ret_patches
	tally += return_count * sizeof(ReturnPatch);
	tally = (tally + align_mask) & ~align_mask;
	
	return (FunctionTallySimple){
		.memory_usage = tally,
		
		.line_info_count = line_info_count,
		.locals_count = locals_count,
		.return_count = return_count,
		.label_patch_count = label_patch_count
	};
}

// entry point to the x64 fast isel, it's got some nice features like when the
// temporary storage can't fit the necessary memory, it'll fallback to the heap
// to avoid just crashing.
TB_FunctionOutput x64_fast_compile_function(TB_CompiledFunctionID id, TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id) {
	s_local_thread_id = local_thread_id;
	s_compiled_func_id = id;
	
	//tb_function_print(f, tb_default_print_callback, stdout);
	//printf("\n\n\n");
	
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	// Allocate all the memory we'll need
	bool is_ctx_heap_allocated = false;
	X64_FastCtx* restrict ctx = NULL;
	{
		// if we can't fit our memory usage into memory, we fallback
		FunctionTallySimple tally = tally_memory_usage_simple(f);
		is_ctx_heap_allocated = !tb_tls_can_fit(tls, tally.memory_usage);
		
		size_t ctx_size = sizeof(X64_FastCtx) + (f->nodes.count * sizeof(AddressDesc));
		if (is_ctx_heap_allocated) {
			ctx = malloc(ctx_size);
			*ctx = (X64_FastCtx) {
				.header = {
					.out = out,
					.start_out = out,
					
					.labels        = malloc(f->label_count * sizeof(uint32_t)),
					.label_patches = malloc(tally.label_patch_count * sizeof(LabelPatch)),
					.ret_patches   = malloc(tally.return_count * sizeof(ReturnPatch))
				},
				.use_count     = malloc(f->nodes.count * sizeof(TB_Reg))
			};
		} else {
			ctx = tb_tls_push(tls, ctx_size);
			*ctx = (X64_FastCtx) {
				.header = {
					.out = out,
					.start_out = out,
					
					.labels        = tb_tls_push(tls, f->label_count * sizeof(uint32_t)),
					.label_patches = tb_tls_push(tls, tally.label_patch_count * sizeof(LabelPatch)),
					.ret_patches   = tb_tls_push(tls, tally.return_count * sizeof(ReturnPatch))
				},
				.use_count = tb_tls_push(tls, f->nodes.count * sizeof(TB_Reg))
			};
		}
		
		ctx->header.f = f;
		ctx->header.function_id = f - f->module->functions.data;
		
		ctx->header.lines = tb_platform_arena_alloc(tally.line_info_count * sizeof(TB_Line));
		
		ctx->is_sysv = EITHER2(f->module->target_system, TB_SYSTEM_LINUX, TB_SYSTEM_MACOS);
		memset(ctx->addresses, 0, f->nodes.count * sizeof(AddressDesc));
	}
	
	// Analyze function for stack, use counts and phi nodes
	tb_function_calculate_use_count(f, ctx->use_count);
	
	// Create phi lookup table for later evaluation stages
	// and calculate the maximum parameter usage for a call
	size_t caller_usage = 0;
	TB_FOR_EACH_NODE(n, f) {
		if (EITHER3(n->type, TB_CALL, TB_ECALL, TB_VCALL)) {
			int param_usage = CALL_NODE_PARAM_COUNT(n);
			if (caller_usage < param_usage) {
				caller_usage = param_usage;
			}
		}
	}
	
	// On Win64 if we have at least one parameter in any of it's calls, the
	// caller must reserve 32bytes called the shadow space.
	if (!ctx->is_sysv && caller_usage > 0 && caller_usage < 4) caller_usage = 4;
	
	const TB_FunctionPrototype* restrict proto = f->prototype;
	loop(i, (size_t)proto->param_count) {
		TB_DataType dt = proto->params[i];
		TB_Reg r = TB_FIRST_PARAMETER_REG + i;
		
		// Allocate space in stack
		int size = get_data_type_size(dt);
		assert(size <= 8 && "Parameter too big");
		
		if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
			// xmm parameters
			if (i < 4) fast_def_xmm(ctx, f, r, (XMM)i, dt);
			else fast_def_spill(ctx, f, r, 16 + (i * 8), dt);
		} else {
			// gpr parameters
			if (ctx->is_sysv && i < 6) {
				fast_def_gpr(ctx, f, r, (GPR)SYSV_GPR_PARAMETERS[i], dt);
			} else if (i < 4) {
				fast_def_gpr(ctx, f, r, (GPR)WIN64_GPR_PARAMETERS[i], dt);
			} else {
				fast_def_spill(ctx, f, r, 16 + (i * 8), dt);
			}
		}
	}
	ctx->header.stack_usage += 16 + (proto->param_count * 8);
	
	if (proto->has_varargs) {
		const GPR* parameter_gprs = ctx->is_sysv ? SYSV_GPR_PARAMETERS : WIN64_GPR_PARAMETERS;
		
		// spill the rest of the parameters (assumes they're all in the GPRs)
		size_t gpr_count = ctx->is_sysv ? 6 : 4;
		size_t extra_param_count = proto->param_count > gpr_count ? 0 : gpr_count - proto->param_count;
		
		loop(i, extra_param_count) {
			size_t param_num = proto->param_count + i;
			
			Val dst = val_stack(TB_TYPE_I64, 16 + (param_num * 8));
			Val src = val_gpr(TB_I64, parameter_gprs[param_num]);
			INST2(MOV, &dst, &src, TB_I64);
		}
	}
	
	// Just the splitting point between parameters
	// and locals in the stack.
	TB_FOR_EACH_NODE(n, f) {
		TB_Reg r = n - f->nodes.data;
		
		if (n->type == TB_PARAM_ADDR) {
			int id = n->param_addr.param - TB_FIRST_PARAMETER_REG;
			
			fast_def_spill(ctx, f, r, 16 + (id * 8), n->dt);
		} else if (n->type == TB_LOCAL) {
			uint32_t size = n->local.size;
			uint32_t align = n->local.alignment;
			int pos = STACK_ALLOC(size, align);
			
			fast_def_spill(ctx, f, r, pos, n->dt);
		}
	}
	
	// Evaluate basic blocks
	TB_Reg bb = 1;
	do {
		assert(f->nodes.data[bb].type == TB_LABEL);
		TB_Node* start = &f->nodes.data[bb];
		
		TB_Reg bb_end = start->label.terminator;
		TB_Node* end = &f->nodes.data[bb_end];
		
		// Define label position
		TB_Label label_id = start->label.id;
		ctx->header.labels[label_id] = GET_CODE_POS();
		
#if !TB_STRIP_LABELS
		if (label_id) {
			tb_emit_label_symbol(f->module, ctx->function_id, label_id, GET_CODE_POS());
		}
#endif
		
		// Generate instruction
		fast_eval_basic_block(ctx, f, bb, bb_end);
		
		// Resolve any leftover expressions which are used later
		TB_Node* next_bb = end;
		if (end->type != TB_LABEL) next_bb = &f->nodes.data[next_bb->next];
		
		TB_Reg next_bb_reg = next_bb - f->nodes.data;
		
		// Evaluate the terminator
		if (end->type == TB_RET) {
			TB_DataType dt = end->dt;
			if (dt.type == TB_BOOL) dt.type = TB_I8;
			
			// Evaluate return value
			if (end->ret.value) {
				if (TB_IS_FLOAT_TYPE(dt.type) || dt.width) {
					tb_todo();
				} else if (dt.type == TB_I8 ||
						   dt.type == TB_I16 ||
						   dt.type == TB_I32 ||
						   dt.type == TB_I64 ||
						   dt.type == TB_PTR) {
					Val dst = val_gpr(dt.type, RAX);
					fast_folded_op(ctx, f, MOV, &dst, end->ret.value);
				} else tb_todo();
			}
			
			// Only jump if we aren't literally about to end the function
			if (next_bb != &f->nodes.data[0]) {
				RET_JMP();
			}
		} else if (end->type == TB_IF) {
			TB_Label if_true = end->if_.if_true;
			TB_Label if_false = end->if_.if_false;
			
			// Save out PHI nodes
			{
				TB_Reg if_true_reg = tb_find_reg_from_label(f, if_true);
				TB_Reg if_false_reg = tb_find_reg_from_label(f, if_false);
				
				TB_Reg if_true_reg_end = f->nodes.data[if_true_reg].label.terminator;
				TB_Reg if_false_reg_end = f->nodes.data[if_false_reg].label.terminator;
				
				fast_eval_terminator_phis(ctx, f, bb, bb_end, if_true_reg, if_true_reg_end);
				fast_eval_terminator_phis(ctx, f, bb, bb_end, if_false_reg, if_false_reg_end);
			}
			
			Cond cc = fast_eval_cond(ctx, f, end->if_.cond);
			
			// Reorder the targets to avoid an extra JMP
			TB_Label fallthrough_label = 0;
			if (next_bb != &f->nodes.data[0]) {
				fallthrough_label = next_bb->label.id;
			}
			bool has_fallthrough = fallthrough_label == if_false;
			
			// flip the condition and the labels if
			// it allows for fallthrough
			if (fallthrough_label == if_true) {
				tb_swap(TB_Label, if_true, if_false);
				cc ^= 1;
				
				has_fallthrough = true;
			}
			
			// JCC .true
			// JMP .false # elidable if it points to the next instruction
			JCC(cc, if_true);
			if (!has_fallthrough) JMP(if_false);
		} else if (end->type == TB_LABEL) {
			// save out PHI nodes
			TB_Reg next_terminator = end->label.terminator;
			fast_eval_terminator_phis(ctx, f, bb, bb_end, bb_end, next_terminator);
		} else if (end->type == TB_GOTO) {
			// save out PHI nodes
			TB_Label target_label = end->goto_.label;
			TB_Reg target = tb_find_reg_from_label(f, target_label);
			TB_Reg target_end = f->nodes.data[target].label.terminator;
			
			fast_eval_terminator_phis(ctx, f, bb, bb_end, target, target_end);
			
			// TODO(NeGate): don't emit jump if we can fallthrough
			JMP(end->goto_.label);
		} else tb_todo();
		
		// Next Basic block
		bb = next_bb_reg;
	} while (bb != TB_NULL_REG);
	
	// Fix up stack usage
	// Tally up any saved XMM registers
	ctx->header.stack_usage += tb_popcount((ctx->header.regs_to_save >> 16) & 0xFFFF) * 16;
	
	// allocate callee parameter space
	ctx->header.stack_usage += caller_usage * 8;
	
	// Align stack usage to 16bytes and add 8 bytes for the return address
	ctx->header.stack_usage = align_up(ctx->header.stack_usage + 8, 16) + 8;
	
	// Resolve internal relocations
	loop(i, ctx->header.ret_patch_count) {
		uint32_t pos = ctx->header.ret_patches[i];
		PATCH4(pos, GET_CODE_POS() - (pos + 4));
	}
	
	loop(i, ctx->header.label_patch_count) {
		uint32_t pos = ctx->header.label_patches[i].pos;
		uint32_t target_lbl = ctx->header.label_patches[i].target_lbl;
		
		PATCH4(pos, ctx->header.labels[target_lbl] - (pos + 4));
	}
	
	TB_FunctionOutput func_out = {
		.name = f->name,
		.function = tb_function_get_id(f->module, f),
		.linkage = f->linkage,
		.code = ctx->header.start_out,
		.code_size = ctx->header.out - ctx->header.start_out,
		.stack_usage = ctx->header.stack_usage,
		.prologue_epilogue_metadata = ctx->header.regs_to_save
	};
	
	if (is_ctx_heap_allocated) {
		free(ctx->use_count);
		free(ctx->phis);
		
		free(ctx->header.labels);
		free(ctx->header.label_patches);
		free(ctx->header.ret_patches);
		free(ctx);
	}
	return func_out;
}
