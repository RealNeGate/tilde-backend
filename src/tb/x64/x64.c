// This entire module is one translation unit so that it doesn't have to worry
// about C's crappy support for public and private interfaces.
#include "x64.h"
#include "inst.c"
#include "proepi.c"

#define printf(...) ((void)0)

static int get_data_type_size(const TB_DataType dt);
static PhiValue* find_phi(Ctx* ctx, TB_Register r);
static bool is_address_node(TB_RegType t);

static void eval_basic_block(Ctx* ctx, TB_Function* f, TB_Register bb, TB_Register bb_end);

// Just handles the PHI nodes that we'll encounter when leaving `from`
// into `to`
static void eval_terminator_phis(Ctx* ctx, TB_Function* f, TB_Register from, TB_Register from_terminator, TB_Register to, TB_Register to_terminator);

static void convert_l2r(Ctx* ctx, TB_Function* f, TB_Register r, Val* val);

#define expect_lval(val) assert((val)->type == VAL_MEM && !(val)->mem.is_rvalue);

TB_FunctionOutput x64_compile_function(TB_Function* f, const TB_FeatureSet* features, uint8_t* out) {
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	const size_t ctx_size = sizeof(Ctx) + (f->nodes.count * sizeof(Val));
	Ctx* ctx = tb_tls_push(tls, ctx_size);
	memset(ctx, 0, ctx_size);
	
	ctx->start_out = ctx->out = out;
	
	//tb_function_print(f);
	printf("\n\n\n%s:\n", f->name);
	
	////////////////////////////////
	// Allocate all the local storage we'll need
	////////////////////////////////
	size_t caller_usage = 0;
	{
		size_t phi_count = 0;
		size_t locals_count = 0;
		size_t return_count = 0;
		size_t label_patch_count = 0;
		
		// TODO(NeGate): Consider splitting this into two separate versions, one with 
		// SSE, one without.
#if TB_HOST_ARCH == TB_HOST_X86_64
#define COUNT_OF_TYPE_IN_M128(t) \
__builtin_popcount(_mm_movemask_epi8(_mm_cmpeq_epi8(bytes, _mm_set1_epi8(t))))
		
		// the node types are aligned to a cache line so we could in theory
		// grab up to 64bytes aligned without seg faulting
		for (size_t i = 0; i < f->nodes.count; i += 16) {
			__m128i bytes = _mm_load_si128((__m128i*)&f->nodes.type[i]);
			
			phi_count += COUNT_OF_TYPE_IN_M128(TB_PHI1);
			phi_count += COUNT_OF_TYPE_IN_M128(TB_PHI2);
			
			locals_count += COUNT_OF_TYPE_IN_M128(TB_LOCAL);
			locals_count += COUNT_OF_TYPE_IN_M128(TB_PARAM);
			
			return_count += COUNT_OF_TYPE_IN_M128(TB_RET);
			
			label_patch_count += COUNT_OF_TYPE_IN_M128(TB_GOTO);
			label_patch_count += COUNT_OF_TYPE_IN_M128(TB_IF) * 2;
		}
#undef COUNT_OF_TYPE_IN_M128
#else
		for (size_t i = 1; i < f->nodes.count; i++) {
			TB_RegType t = f->nodes[i].type;
			
			if (t == TB_PHI1) phi_count++;
			else if (t == TB_PHI2) phi_count++;
			else if (t == TB_LOCAL) locals_count++;
			else if (t == TB_PARAM) locals_count++;
			else if (t == TB_RET) return_count++;
			else if (t == TB_IF) label_patch_count += 2;
			else if (t == TB_GOTO) label_patch_count++;
		}
#endif
		
		// Skim over the types and look for CALLs
		// calculate the maximum parameter usage for a call
		FOR_EACH_NODE(i, f, TB_CALL, {
						  int param_usage = CALL_NODE_PARAM_COUNT(f, i);
						  if (caller_usage < param_usage) {
							  caller_usage = param_usage;
						  }
					  });
		
		FOR_EACH_NODE(i, f, TB_VCALL, {
						  int param_usage = CALL_NODE_PARAM_COUNT(f, i);
						  if (caller_usage < param_usage) {
							  caller_usage = param_usage;
						  }
					  });
		
		FOR_EACH_NODE(i, f, TB_ECALL, {
						  int param_usage = CALL_NODE_PARAM_COUNT(f, i);
						  if (caller_usage < param_usage) {
							  caller_usage = param_usage;
						  }
					  });
		
		ctx->intervals = tb_tls_push(tls, f->nodes.count * sizeof(TB_Register));
		ctx->use_count = tb_tls_push(tls, f->nodes.count * sizeof(int));
		ctx->phis = tb_tls_push(tls, phi_count * sizeof(PhiValue));
		
		ctx->params = tb_tls_push(tls, f->parameter_count * sizeof(int32_t));
		ctx->labels = tb_tls_push(tls, f->label_count * sizeof(uint32_t));
		ctx->label_patches = tb_tls_push(tls, label_patch_count * sizeof(LabelPatch));
		ctx->ret_patches = tb_tls_push(tls, return_count * sizeof(ReturnPatch));
	}
	
	////////////////////////////////
	// Analyze function for stack, live intervals and phi nodes
	////////////////////////////////
	ctx->regs_to_save = 0;
	
	// On Win64 if we have at least one parameter, the caller must reserve 32bytes
	// called the shadow space.
	if (ctx->caller_usage > 0 && ctx->caller_usage < 4) ctx->caller_usage = 4;
	
	tb_find_live_intervals(f, ctx->intervals);
	tb_find_use_count(f, ctx->use_count);
	
	// Create phi lookup table for later evaluation stages
	FOR_EACH_NODE(i, f, TB_PHI2, {
					  ctx->phis[ctx->phi_count++] = (PhiValue){
						  .reg = i, .storage_a = f->nodes.payload[i].phi1.a
					  };
				  });
	
	FOR_EACH_NODE(i, f, TB_PHI1, {
					  ctx->phis[ctx->phi_count++] = (PhiValue){
						  .reg = i, .storage_a = f->nodes.payload[i].phi1.a
					  };
				  });
	
    // Reserve stack and base pointer
	ctx->gpr_desc[RSP] = TB_REG_MAX;
	ctx->gpr_desc[RBP] = TB_REG_MAX;
	
	// Allocate local and parameter stack slots
	int stack_usage = 0;
	int params_consumed = 0;
	FOR_EACH_NODE(i, f, TB_PARAM, {
					  TB_DataType dt = f->nodes.dt[i];
					  int id = f->nodes.payload[i].param.id;
					  
					  int size = get_data_type_size(dt);
					  stack_usage = align_up(stack_usage + size, size);
					  
					  // The first few parameters are backed by registers
					  // not memory addresses but they have space reserved
					  // for spilling so it's fine that we mark them in the
					  // locals.
					  printf("PARAM %zu <- [rbp - %d]\n", i, stack_usage);
					  
					  if (id < 4) {
						  if (TB_IS_FLOAT_TYPE(dt.type) || dt.count > 1) {
							  ctx->xmm_desc[id] = i;
							  
							  printf("   PARAM XMM%d\n", GPR_NAMES[id]);
							  def(ctx, f, val_xmm(dt, id), i);
						  } else if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
							  ctx->gpr_desc[GPR_PARAMETERS[id]] = i;
							  printf("   PARAM GPR %s\n", GPR_NAMES[GPR_PARAMETERS[id]]);
							  
							  def(ctx, f, val_gpr(dt.type, GPR_PARAMETERS[id]), i);
						  }
					  } else {
						  def(ctx, f, val_stack(dt, -stack_usage), i);
					  }
					  
					  ctx->params[id] = -stack_usage;
					  params_consumed++;
					  
					  // short circuit, once we've seen all the parameters (which we
					  // know) then we can quit.
					  if (params_consumed == f->parameter_count)
						  goto done_with_param_scan;
				  });
	
	done_with_param_scan:;
	// Just the splitting point between parameters
	// and locals in the stack.
	int param_space = stack_usage;
	bool saves_parameters = false;
	
	FOR_EACH_NODE(i, f, TB_PARAM_ADDR, {
					  TB_Register param = f->nodes.payload[i].param_addr.param;
					  
					  TB_DataType dt = f->nodes.dt[param];
					  int id = f->nodes.payload[param].param.id;
					  
					  if (id < 4) {
						  saves_parameters = true;
						  Val dst = val_stack(dt, ctx->params[id]);
						  
						  if (TB_IS_FLOAT_TYPE(dt.type) || dt.count > 1) {
							  // the parameters map to XMM0-XMM3
							  Val src = val_xmm(dt, id);
							  
							  // don't keep reference to XMM, we'll be using the memory
							  // version only
							  ctx->xmm_desc[id] = 0;
							  
							  // save the shadow space into the stack
							  if (dt.type == TB_F32) {
								  inst2(ctx, MOVSS, &dst, &src, dt.type);
							  }
							  else tb_todo(); // TODO(NeGate): Implement movsd
						  } else if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
							  Val src = val_gpr(TB_I64, GPR_PARAMETERS[id]); 
							  
							  // don't keep reference to GPR, we'll be using the
							  // memory version only
							  ctx->gpr_desc[GPR_PARAMETERS[id]] = 0;
							  printf("   move to stack slot %s\n", GPR_NAMES[GPR_PARAMETERS[id]]);
							  
							  // save the shadow space into the stack
							  inst2(ctx, MOV, &dst, &src, dt.type);
						  } else tb_todo();
					  }
				  });
	
	FOR_EACH_NODE(i, f, TB_LOCAL, {
					  uint32_t size = f->nodes.payload[i].local.size;
					  uint32_t align = f->nodes.payload[i].local.alignment;
					  
					  stack_usage = align_up(stack_usage + size, align);
					  
					  def(ctx, f, val_stack(f->nodes.dt[i], -stack_usage), i);
				  });
	
	// Didn't originally write into this value because C
	// aliasing rules :P, we'll need it later for the spilling
	// mechanics.
	ctx->stack_usage = stack_usage;
	
	////////////////////////////////
	// Evaluate each basic block
	////////////////////////////////
	TB_Register bb = 1;
    do {
        assert(f->nodes.type[bb] == TB_LABEL);
        TB_Label label_id = f->nodes.payload[bb].label.id;
        TB_Register bb_end = f->nodes.payload[bb].label.terminator;
		
		ctx->labels[label_id] = code_pos();
		
#if !TB_STRIP_LABELS
		if (label_id) tb_emit_label_symbol(f->module,
										   f - f->module->functions.data,
										   label_id,
										   code_pos());
#endif
		
		// Generate instructions from the side-effect nodes using
		// all the other nodes and then terminate the basic block
        if (bb < bb_end) eval_basic_block(ctx, f, bb, bb_end);
		
		// Evaluate the terminator
		const TB_RegType reg_type = f->nodes.type[bb_end];
		const TB_DataType dt = f->nodes.dt[bb_end];
		const TB_RegPayload p = f->nodes.payload[bb_end];
		
		if (reg_type == TB_RET) {
			// Evaluate return value
			if (dt.type != TB_VOID) {
				Val value;
				use(ctx, f, &value, p.ret.value, bb_end);
				convert_l2r(ctx, f, p.ret.value, &value);
				
				if (value.dt.type == TB_F32 || value.dt.count > 1) {
					// Float results use XMM0
					Val dst = val_xmm(value.dt, XMM0);
					
					if (is_value_xmm(&value, XMM0)) {
						inst2(ctx, value.dt.count > 1 ? MOVAPS : MOVSS, &dst, &value, value.dt.type);
					}
				} else if (value.dt.type == TB_I8 ||
						   value.dt.type == TB_I16 ||
						   value.dt.type == TB_I32 ||
						   value.dt.type == TB_I64 ||
						   value.dt.type == TB_PTR) {
					// Integer results use RAX and if result is extended RDX
					Val dst = val_gpr(value.dt.type, RAX);
					
					if (value.type == VAL_IMM) {
						assert(value.imm < INT32_MAX);
						if (value.imm == 0) {
							emit(0x31);
							emit(mod_rx_rm(MOD_DIRECT, RAX, RAX));
						} else {
							// mov eax, imm32
							emit(0xB8);
							emit4(value.imm);
						}
					} else if (value.type != VAL_GPR || (value.type == VAL_GPR && value.gpr != RAX)) {
						inst2(ctx, MOV, &dst, &value, value.dt.type);
					}
				} else tb_todo();
			}
			
			// Only jump if we aren't literally about to end the function
			if (bb_end + 1 != f->nodes.count) {
				ctx->ret_patches[ctx->ret_patch_count++] = code_pos() + 1;
				
				emit(0xE9);
				emit4(0x0);
			}
		} else if (reg_type == TB_IF) {
			TB_Label if_true = p.if_.if_true;
			TB_Label if_false = p.if_.if_false;
			
			// Save out PHI nodes
			{
				TB_Register if_true_reg = tb_find_reg_from_label(f, if_true);
				TB_Register if_false_reg = tb_find_reg_from_label(f, if_false);
				
				TB_Register if_true_reg_end = f->nodes.payload[if_true_reg].label.terminator;
				TB_Register if_false_reg_end = f->nodes.payload[if_false_reg].label.terminator;
				
				eval_terminator_phis(ctx, f, bb, bb_end, if_true_reg, if_true_reg_end);
				eval_terminator_phis(ctx, f, bb, bb_end, if_false_reg, if_false_reg_end);
			}
			
			Val cond;
			use(ctx, f, &cond, p.if_.cond, bb_end);
			convert_l2r(ctx, f, p.if_.cond, &cond);
			
			if (cond.type == VAL_IMM) {
				TB_Label dst = (cond.imm ? if_true : if_false);
				
				if (dst != f->nodes.payload[bb_end + 1].label.id) jmp(ctx, dst);
			} else {
				// Implicit convert into FLAGS
				if (cond.type == VAL_GPR) {
					inst2(ctx, TEST, &cond, &cond, cond.dt.type);
					cond = val_flags(NE);
				} else if (cond.type == VAL_MEM) {
					Val imm = val_imm(cond.dt, 0);
					inst2(ctx, CMP, &cond, &imm, cond.dt.type);
					cond = val_flags(NE);
				} else if (cond.type == VAL_FLAGS) {
					// this is what we what
				} else tb_todo();
				
				// Reorder the targets to avoid an extra JMP
				TB_Label fallthrough_label = 0;
				if (bb_end + 1 < f->nodes.count) {
					fallthrough_label = f->nodes.payload[bb_end + 1].label.id;
				}
				bool has_fallthrough = fallthrough_label == if_false;
				
				Cond cc = cond.cond;
				
				// flip the condition and the labels if
				// it allows for fallthrough
				if (fallthrough_label == if_true) {
					tb_swap(if_true, if_false);
					cc ^= 1;
					
					has_fallthrough = true;
				}
				
				// JCC .true
				// JMP .false # elidable if it points to the next instruction
				jcc(ctx, cc, if_true);
				if (!has_fallthrough) jmp(ctx, if_false);
			}
		} else if (reg_type == TB_GOTO) {
			// TODO(NeGate): save out any phi nodes
			assert(f->nodes.type[bb_end + 1] == TB_LABEL);
			
			TB_Label target_label = f->nodes.payload[bb_end].goto_.label;
			TB_Register target = tb_find_reg_from_label(f, target_label);
			TB_Register target_end = f->nodes.payload[target].label.terminator;
			
			eval_terminator_phis(ctx, f, bb, bb_end, target, target_end);
			
			TB_Label fallthrough_label = f->nodes.payload[bb_end + 1].label.id;
			if (fallthrough_label != p.goto_.label) jmp(ctx, p.goto_.label);
		} else if (reg_type == TB_LABEL) {
			// simple fallthrough
			// TODO(NeGate): save out any phi nodes
			TB_Register next_terminator = f->nodes.payload[bb_end].label.terminator;
			
			eval_terminator_phis(ctx, f, bb, bb_end, bb_end, next_terminator);
		} else {
			tb_todo();
		}
		
		// Next Basic block
		bb = bb_end + ((reg_type == TB_LABEL) ? 0 : 1);
	} while (bb < f->nodes.count);
	
	// Align stack usage to 16bytes and add 8 bytes for the return address
	{
		stack_usage = ctx->stack_usage + (caller_usage * 8);
		if (!saves_parameters && stack_usage <= param_space && caller_usage == 0) {
			stack_usage = 8;
		} else {
			stack_usage = align_up(stack_usage + 8, 16);
		}
	}
	
	////////////////////////////////
	// Evaluate internal relocations (return and labels)
	////////////////////////////////
	for (size_t i = 0, cc = ctx->ret_patch_count; i < cc; i++) {
		uint32_t pos = ctx->ret_patches[i];
		patch4(pos, code_pos() - (pos + 4));
	}
	
	for (size_t i = 0, cc = ctx->label_patch_count; i < cc; i++) {
		uint32_t pos = ctx->label_patches[i].pos;
		uint32_t target_lbl = ctx->label_patches[i].target_lbl;
		
		patch4(pos, ctx->labels[target_lbl] - (pos + 4));
	}
	
	return (TB_FunctionOutput) {
		.name = f->name,
		.code = ctx->start_out,
		.code_size = ctx->out - ctx->start_out,
		.stack_usage = stack_usage,
		
		.prologue_epilogue_metadata = ctx->regs_to_save
	};
}

static void eval_terminator_phis(Ctx* ctx, TB_Function* f, TB_Register from, TB_Register from_terminator, TB_Register to, TB_Register to_terminator) {
	for (size_t i = to; i < to_terminator; i++) if (f->nodes.type[i] == TB_PHI2) {
		TB_RegPayload p = f->nodes.payload[i];
		TB_DataType dt = f->nodes.dt[i];
		assert(p.phi2.a_label != p.phi2.b_label);
		
		TB_Register src;
		if (p.phi2.a_label == from) src = p.phi2.a;
		else if (p.phi2.b_label == from) src = p.phi2.b;
		else tb_unreachable();
		
		PhiValue* phi = find_phi(ctx, i);
		if (phi->value.type == VAL_NONE) {
			// Initialize it
			Val src_value;
			use(ctx, f, &src_value, src, 0);
			convert_l2r(ctx, f, src, &src_value);
			
			if (src_value.type == VAL_GPR && 
				is_temporary_of_bb(ctx, f, src_value.gpr, from, from_terminator)) {
				// Recycle old value
				phi->value = src_value;
				def(ctx, f, src_value, i);
			} else {
				// Create a new GPR and map it
				def_new_gpr(ctx, f, &phi->value, i, dt.type);
				
				// TODO(NeGate): Handle vector and float types
				if (!is_value_gpr(&src_value, phi->value.gpr)) {
					inst2(ctx, MOV, &phi->value, &src_value, dt.type);
				}
			}
		} else {
			// Load value into existing phi node
			store_into(ctx, f, dt, &phi->value, i, i, src);
		}
	}
}

static void eval_basic_block(Ctx* ctx, TB_Function* f, TB_Register bb, TB_Register bb_end) {
	// Evaluate all side effect instructions
	ctx->current_bb = bb;
	ctx->current_bb_end = bb_end;
	
	for (size_t i = bb + 1; i < bb_end; i++) {
		TB_RegTypeEnum reg_type = f->nodes.type[i];
		
		if (reg_type >= TB_SIDE_EFFECT_MIN &&
			reg_type <= TB_SIDE_EFFECT_MAX) {
			TB_DataType dt = f->nodes.dt[i];
			TB_RegPayload* restrict p = &f->nodes.payload[i];
			
			printf("r%zu:\n", i);
			switch (reg_type) {
				case TB_LINE_INFO: {
					// Ignore the first one since it's got special rules
					if (i > 2) p->line_info.pos = code_pos();
					break;
				}
				case TB_LOAD: {
					TB_Register addr_reg = p->load.address;
					bool explicit_load = ctx->use_count[i] > 1;
					
					if (!explicit_load &&
						(f->nodes.type[addr_reg] == TB_LOCAL ||
						 f->nodes.type[addr_reg] == TB_PARAM_ADDR)) {
						explicit_load = false;
						
						// If this load out-lives the next side-effect
						// that might change it
						// TODO(NeGate): Implement noalias
						for (size_t j = i + 1; j < bb_end; j++) {
							if (f->nodes.type[j] == TB_STORE ||
								f->nodes.type[j] == TB_CALL ||
								f->nodes.type[j] == TB_VCALL ||
								f->nodes.type[j] == TB_ECALL) {
								explicit_load = (ctx->intervals[i] > j);
								break;
							}
						}
					}
					
					if (explicit_load) {
						Val v;
						TB_DataType dt = f->nodes.dt[i];
						load_into(ctx, f, dt, &v, i, addr_reg, TB_NULL_REG);
						
						def(ctx, f, v, i);
					}
					break;
				}
				case TB_STORE: {
					TB_Register addr_reg = f->nodes.payload[i].store.address;
					TB_Register val_reg = f->nodes.payload[i].store.value;
					
					// Eval address and cast to the correct type for the store
					Val address;
					use(ctx, f, &address, addr_reg, i);
					
					// Desugar booleans into bytes
					if (dt.type == TB_BOOL) dt.type = TB_I8;
					
					store_into(ctx, f, dt, &address, i, addr_reg, val_reg);
					break;
				}
				case TB_CALL:
				case TB_ECALL:
				case TB_VCALL: {
					int param_start = p->call.param_start;
					int param_count = p->call.param_end - p->call.param_start;
					
					// Evict the GPRs that are caller saved
					for (size_t j = 0; j < 16; j++) {
						if (ABI_CALLER_SAVED & (1u << j)) evict_gpr(ctx, f, j, i);
					}
					
					// Evict the XMMs that are caller saved
					for (size_t j = 0; j < param_count; j++) {
						TB_DataType param_dt = f->nodes.dt[param_start + j];
						
						if (j < 4 && (param_dt.count > 1 ||
									  TB_IS_FLOAT_TYPE(param_dt.type))) {
							evict_xmm(ctx, f, j, i);
						}
					}
					
					// evict return value
					if (dt.count > 1 || TB_IS_FLOAT_TYPE(dt.type)) {
						evict_xmm(ctx, f, XMM0, i);
					} else if (dt.type != TB_VOID) {
						evict_gpr(ctx, f, RAX, i);
						ctx->gpr_desc[RAX] = TB_REG_TEMP;
					}
					
					// evaluate parameters
					for (size_t j = 0; j < param_count; j++) {
						TB_Register param_reg = f->vla.data[param_start + j];
						
						Val param;
						use(ctx, f, &param, param_reg, i);
						convert_l2r(ctx, f, param_reg, &param);
						
						if (j < 4) {
							if (param.dt.count > 1 ||
								TB_IS_FLOAT_TYPE(param.dt.type)) {
								Val dst = val_xmm(param.dt, j);
								
								// move into param slot
								inst2(ctx,
									  param.dt.count == 1 ? MOVSS : MOVAPS,
									  &dst, &param,
									  TB_F32);
								
								// reserve for a bit
								ctx->xmm_desc[j] = TB_REG_TEMP;
							} else if (param.dt.type != TB_VOID) {
								Val dst = val_gpr(param.dt.type, GPR_PARAMETERS[j]);
								if (!is_value_gpr(&param, GPR_PARAMETERS[j])) {
									inst2(ctx, MOV, &dst, &param, param.dt.type);
								}
								
								// reserve for a bit
								ctx->gpr_desc[GPR_PARAMETERS[j]] = TB_REG_TEMP;
							}
						} else {
							// parameter is in memory
							tb_todo();
						}
					}
					
					// CALL instruction and patch
					int source_func = f - f->module->functions.data;
					
					if (reg_type == TB_CALL) {
						int target_func = p->call.target - f->module->functions.data;
						
						tb_emit_call_patch(f->module,
										   source_func,
										   target_func,
                                           code_pos() + 1);
                        
                        // CALL rel32
                        emit(0xE8);
                        emit4(0x0);
					} else if (reg_type == TB_CALL) {
						tb_emit_ecall_patch(f->module,
											source_func,
											p->ecall.target,
                                            code_pos() + 1);
                        
                        // CALL rel32
                        emit(0xE8);
                        emit4(0x0);
					} else if (reg_type == TB_VCALL) {
                        Val target_ptr;
                        use(ctx, f, &target_ptr, p->vcall.target, i); 
						convert_l2r(ctx, f, p->vcall.target, &target_ptr);
                        
                        // call r/m64
                        inst1(ctx, CALL_RM, &target_ptr);
                    }
					
					// the return value
					if (dt.type == TB_VOID) {
						/* none */
					} else if (dt.count > 1 || TB_IS_FLOAT_TYPE(dt.type)) {
						Val xmm0 = val_xmm(dt, XMM0);
						def(ctx, f, xmm0, i);
					} else {
						Val rax = val_gpr(dt.type, RAX);
						def(ctx, f, rax, i);
					}
					break;
				}
				case TB_MEMSET: {
					TB_Register dst_reg = p->mem_op.dst;
					TB_Register val_reg = p->mem_op.src;
					TB_Register size_reg = p->mem_op.size;
					//int align = f->nodes.payload[i].mem_op.align;
					
					// TODO(NeGate): Implement vector memset
					
					// rep stosb, ol' reliable
					evict_gpr(ctx, f, RAX, i);
					evict_gpr(ctx, f, RCX, i);
					evict_gpr(ctx, f, RDI, i);
					
					{
						Val param;
						use(ctx, f, &param, dst_reg, i);
						convert_l2r(ctx, f, dst_reg, &param);
						
						if (!is_value_gpr(&param, RDI)) {
							Val dst = val_gpr(TB_PTR, RDI);
							inst2(ctx, MOV, &dst, &param, TB_PTR);
						}
						ctx->gpr_desc[RDI] = TB_REG_TEMP;
					}
					
					{
						Val param;
						use(ctx, f, &param, val_reg, i);
						convert_l2r(ctx, f, val_reg, &param);
						
						if (!is_value_gpr(&param, RAX)) {
							Val dst = val_gpr(TB_PTR, RAX);
							inst2(ctx, MOV, &dst, &param, TB_PTR);
						}
						ctx->gpr_desc[RAX] = TB_REG_TEMP;
					}
					
					{
						Val param;
						use(ctx, f, &param, size_reg, i);
						convert_l2r(ctx, f, size_reg, &param);
						
						if (!is_value_gpr(&param, RCX)) {
							Val dst = val_gpr(TB_PTR, RCX);
							inst2(ctx, MOV, &dst, &param, TB_PTR);
						}
						ctx->gpr_desc[RCX] = TB_REG_TEMP;
					}
					
					// rep stosb
					emit(0xF3);
					emit(0xAA);
					break;
				}
				default:
				tb_todo();
			}
			
			// free temporaries
			loop(i, 16) {
				if (ctx->gpr_desc[i] == TB_REG_TEMP) ctx->gpr_desc[i] = 0;
			}
			
			loop(i, 16) {
				if (ctx->xmm_desc[i] == TB_REG_TEMP) ctx->xmm_desc[i] = 0;
			}
		}
	}
}

static void use(Ctx* ctx, TB_Function* f, Val* v, TB_Register r, TB_Register next) {
	TB_RegTypeEnum reg_type = f->nodes.type[r];
	TB_DataType dt = f->nodes.dt[r];
	
	if (reg_type == TB_PARAM && f->nodes.payload[r].param.id < 4) {
		int* arr = NULL;
		bool use_xmm = false;
		
		if (TB_IS_FLOAT_TYPE(dt.type) || dt.count > 1) {
			use_xmm = true;
			arr = &ctx->xmm_desc[0];
		} else arr = &ctx->gpr_desc[0];
		
		// This is a simple linear search over a fixed set so it's very easy
		// to optimize, if we use SSE we can do it 4-wide and if we use AVX
		// we can do it 8-wide
#if TB_HOST_ARCH == TB_HOST_X86_64
#ifdef __AVX__
		for (size_t i = 0; i < 2; i++) {
			__m256i s = _mm256_loadu_si256((__m256i*)&arr[i * 8]);
			unsigned int mask = _mm256_movemask_ps(_mm256_castsi256_ps(_mm256_cmpeq_epi32(s, _mm256_set1_epi32(r))));
			
			if (mask) {
				*v = (Val) {
					.type = use_xmm ? VAL_XMM : VAL_GPR,
					.dt = dt,
					// gpr/xmm both take the same space in the tagged union
					// so it doesn't matter
					.gpr = (i * 8) + (__builtin_ffs(mask) - 1)
				};
				return;
			}
		}
#else
		for (size_t i = 0; i < 4; i++) {
			__m128i s = _mm_loadu_si128((__m128i*)&arr[i * 4]);
			unsigned int mask = _mm_movemask_ps(_mm_castsi128_ps(_mm_cmpeq_epi32(s, _mm_set1_epi32(r))));
			
			if (mask) {
				*v = (Val) {
					.type = use_xmm ? VAL_XMM : VAL_GPR,
					.dt = dt,
					// gpr/xmm both take the same space in the tagged union
					// so it doesn't matter
					.gpr = (i * 4) + (__builtin_ffs(mask) - 1)
				};
				return;
			}
		}
#endif
#else
		for (int i = 0; i < 16; i++) {
			TB_Register bound = ctx->xmm_desc[i];
			
			if (r == bound) { 
				*v = (Val) {
					.type = use_xmm ? VAL_XMM : VAL_GPR,
					.dt = dt,
					// gpr/xmm both take the same space in the tagged union
					// so it doesn't matter
					.gpr = i
				};
				return;
			}
		}
#endif
	} 
	
	if (ctx->addr_desc[r].type != VAL_NONE) {
		// Already resolved, just reuse
		*v = ctx->addr_desc[r];
	} else {
		// Evaluate for the first time
		TB_RegPayload p = f->nodes.payload[r];
		
		switch (reg_type) {
			// loaded beforehand so
			// it should never reach here
			case TB_PARAM:
			case TB_PHI2:
			tb_unreachable();
			case TB_PHI1: {
				TB_Register phi = f->nodes.payload[r].phi1.a;
				
				*v = find_phi(ctx, phi)->value;
				break;
			}
			case TB_INT_CONST: {
				assert(p.i_const.hi == 0);
				
				if (p.i_const.lo > UINT32_MAX) {
					// explicit mov
					assert(dt.type == TB_I64 || dt.type == TB_PTR || dt.type == TB_I128);
					
					def_new_gpr(ctx, f, v, r, dt.type);
					
					// mov reg64, imm64
					emit(rex(true, 0x0, v->gpr, 0));
					emit(0xB8 + (v->gpr & 0b111));
					emit8(p.i_const.lo);
					break;
				}
				
				// 32bit immediate case
				*v = val_imm(dt, p.i_const.lo);
				break;
			}
			case TB_FLOAT_CONST: {
				// Unlike integers, there's no float immediates
				def_new_xmm(ctx, f, v, r, dt);
				XMM dst_xmm = v->xmm;
				
				assert(dt.type == TB_F32 && dt.count == 1);
				uint32_t imm = (Cvt_F32U32){ .f = p.f_const }.i;
				
				if (imm == 0) {
					if (dst_xmm >= 8) emit(rex(true, dst_xmm, dst_xmm, 0));
					emit(0x0F);
					emit(0x57);
					emit(mod_rx_rm(MOD_DIRECT, dst_xmm, dst_xmm));
				} else {
					// Convert it to raw bits
					if (dst_xmm >= 8) emit(rex(true, dst_xmm, dst_xmm, 0));
					emit(0xF3);
					emit(0x0F);
					emit(0x10);
					emit(((dst_xmm & 7) << 3) | RBP);
					
					size_t func_id = f - f->module->functions.data;
					uint32_t offset = tb_emit_const32_patch(f->module, 
															func_id,
															code_pos(),
															imm);
					emit4(offset);
				}
				break;
			}
			case TB_ARRAY_ACCESS: {
				Val base, index;
				use(ctx, f, &base, p.array_access.base, r);
				if (base.type == VAL_MEM) {
					assert(!base.mem.is_rvalue);
				} else if (base.type == VAL_GPR) {
					base = val_base_disp(TB_TYPE_PTR, base.gpr, 0);
				} else tb_todo();
				
				use(ctx, f, &index, p.array_access.index, r);
				convert_l2r(ctx, f, p.array_access.index, &index);
				
				uint32_t stride = p.array_access.stride;
				int32_t disp = base.mem.disp;
				
				if (index.type == VAL_MEM) {
					Val new_v;
					def_new_gpr(ctx, f, &new_v, p.array_access.index, TB_I64);
					inst2(ctx, MOV, &new_v, &index, TB_I64);
					
					index = new_v;
				}
				
				if (index.type == VAL_IMM) {
					*v = base;
					v->mem.disp += index.imm * stride;
				} else if (index.type == VAL_GPR) {
					GPR base_reg = base.mem.base;
					
					if (stride == 1 ||
						stride == 2 ||
						stride == 4 ||
						stride == 8) {
						Scale scale = __builtin_ffs(stride) - 1;
						
						if (base.mem.index != GPR_NONE) {
							// nested indices a[x][y]
							// lea dst, [base + index0 * scale0 + offset]
							// lea dst, [dst + index1 * scale1] or add dst, index1
							def_new_gpr(ctx, f, v, r, dt.type);
							inst2(ctx, LEA, v, &base, TB_PTR);
							
							if (scale) {
								Val addr = val_base_index(dt, v->gpr, index.gpr, scale);
								inst2(ctx, LEA, v, &addr, TB_PTR);
							}
							else {
								Val idx = val_gpr(TB_PTR, index.gpr);
								inst2(ctx, ADD, v, &idx, TB_PTR);
							}
							
							*v = val_base_disp(dt, v->gpr, 0);
						}
						else {
							*v = val_base_index_disp(dt, base_reg, index.gpr, scale, disp);
						}
					} else {
						// Resolve the index then add the base
						// TODO(NeGate): Improve this scaling method by
						// adding ways to break down multiplies into bitshifts.
						// int shifts_needed = __builtin_popcount(stride);
						def_new_gpr(ctx, f, v, r, dt.type);
						
						emit(rex(true, v->gpr, index.gpr, 0));
						emit(0x69);
						emit(mod_rx_rm(MOD_DIRECT, v->gpr, index.gpr));
						emit4(stride);
						
						emit(rex(true, base.gpr, v->gpr, 0));
						emit(0x01);
						emit(mod_rx_rm(MOD_DIRECT, base_reg, v->gpr));
						
						if (base.mem.index != GPR_NONE) {
							// accumulate the index to the result
							base.mem.base = v->gpr;
							
							inst2(ctx, LEA, v, &base, TB_PTR);
						}
					}
				} else tb_todo();
				break;
			}
			case TB_MEMBER_ACCESS: {
				use(ctx, f, v, p.member_access.base, r);
				expect_lval(v);
				
				int32_t offset = p.member_access.offset;
				if (v->type == VAL_MEM) {
					v->mem.disp += offset;
				} else tb_todo();
				break;
			}
			case TB_LOAD: {
				load_into(ctx, f, dt, v, r, p.load.address, next);
				break;
			}
			case TB_PARAM_ADDR: {
				TB_Register param = p.param_addr.param;
				int id = f->nodes.payload[param].param.id;
				
				*v = val_stack(dt, ctx->params[id]);
				break;
			}
			case TB_TRUNCATE: {
				Val src;
				use(ctx, f, &src, p.ext, r);
				convert_l2r(ctx, f, p.ext, &src);
				def_new_gpr(ctx, f, v, r, dt.type);
				
				// TODO(NeGate): Implement recycle
				if (dt.type == TB_PTR || dt.type == TB_I64 || dt.type == TB_I32) {
					// 32bit operations automatically truncate
					inst2(ctx, MOV, v, &src, dt.type);
				} else {
					uint64_t shift = 64 - (8 << (dt.type - TB_I8));
					uint64_t mask = (~0ull) >> shift;
					Val imm = val_imm(dt, mask);
					
					inst2(ctx, MOV, v, &src, dt.type);
					inst2(ctx, AND, v, &imm, dt.type);
				}
				break;
			}
			case TB_ZERO_EXT: {
				Val src;
				use(ctx, f, &src, p.ext, r);
				convert_l2r(ctx, f, p.ext, &src);
				
				if (src.dt.type == TB_I32 &&
					dt.type == TB_I64 &&
					ctx->intervals[p.ext] == r) {
					assert(src.dt.count == 1);
					assert(dt.count == 1);
					
					if (src.type != VAL_MEM) {
						kill(ctx, f, p.ext);
						def(ctx, f, src, r);
						
						*v = src;
						v->dt.type = TB_I64;
					}
					else {
						// for 32->64 memory, it needs to actually
						// just do a 32bit load since it can't promote
						// the load itself up without "over-reading"
						def_new_gpr(ctx, f, v, r, TB_I64);
						inst2(ctx, MOV, v, &src, TB_I32);
					}
				}
				else {
					def_new_gpr(ctx, f, v, r, dt.type);
					inst2(ctx, MOVZX, v, &src, dt.type);
				}
				break;
			}
			case TB_SIGN_EXT: {
				Val src;
				use(ctx, f, &src, p.ext, r);
				convert_l2r(ctx, f, p.ext, &src);
				def_new_gpr(ctx, f, v, r, dt.type);
				
				// TODO(NeGate): Implement recycle
				inst2(ctx, MOVSX, v, &src, dt.type);
				break;
			}
			case TB_NEG: {
				Val src;
				use(ctx, f, &src, p.unary, r);
				convert_l2r(ctx, f, p.unary, &src);
				def_new_gpr(ctx, f, v, r, dt.type);
				
				// TODO(NeGate): Implement recycle
				inst2(ctx, MOV, v, &src, dt.type);
				inst1(ctx, NEG, v);
				break;
			}
			case TB_NOT: {
				Val src;
				use(ctx, f, &src, p.unary, r);
				convert_l2r(ctx, f, p.unary, &src);
				def_new_gpr(ctx, f, v, r, dt.type);
				
				// TODO(NeGate): Implement recycle
				inst2(ctx, MOV, v, &src, dt.type);
				inst1(ctx, NOT, v);
				break;
			}
			case TB_FADD:
			case TB_FSUB:
			case TB_FMUL:
			case TB_FDIV: {
				const static IselInfo tbl[] = {
					// inst   commut has_imm mem_dst mem_src   
					{ ADDSS,  true,  true,   false,  true },
					{ SUBSS,  true,  true,   false,  true },
					{ MULSS,  true,  true,   false,  true },
					{ DIVSS,  true,  true,   false,  true },
					
					// 4-wide versions
					{ ADDPS,  true,  true,   false,  true },
					{ SUBPS,  true,  true,   false,  true },
					{ MULPS,  true,  true,   false,  true },
					{ DIVPS,  true,  true,   false,  true }
				};
				assert((dt.count == 1 || dt.count == 4) && dt.type == TB_F32);
				const IselInfo* info = &tbl[(dt.count == 4 ? 4 : 0) + (reg_type - TB_FADD)];
				
				TB_Register a_reg = p.f_arith.a;
				TB_Register b_reg = p.f_arith.b;
				
				if (a_reg == b_reg) {
					Val src;
					use(ctx, f, &src, a_reg, r);
					convert_l2r(ctx, f, a_reg, &src);
					
					isel_aliased(ctx, f, v, info, r, a_reg, &src);
				} else {
					Val a, b;
					use(ctx, f, &a, a_reg, r);
					use(ctx, f, &b, b_reg, r);
					convert_l2r(ctx, f, a_reg, &a);
					convert_l2r(ctx, f, b_reg, &b);
					
					isel(ctx, f, v, info, r, a_reg, b_reg, &a, &b);
				}
				break;
			}
			case TB_AND:
			case TB_OR:
			case TB_XOR:
			case TB_ADD:
			case TB_SUB:
			case TB_MUL: {
				const static IselInfo tbl[] = {
					// type               inst  commut  has_imm  mem_dst  mem_src   
					[TB_AND - TB_AND] = { AND,  true,   true,    false,   true },
					[TB_OR  - TB_AND] = { OR,   true,   true,    false,   true },
					[TB_XOR - TB_AND] = { XOR,  true,   true,    false,   true },
					[TB_ADD - TB_AND] = { ADD,  true,   true,    false,   true },
					[TB_SUB - TB_AND] = { SUB,  false,  true,    false,   true },
					[TB_MUL - TB_AND] = { IMUL, true,   false,   false,   true }
				};
				const IselInfo* info = &tbl[reg_type - TB_AND];
				
				TB_Register a_reg = p.i_arith.a;
				TB_Register b_reg = p.i_arith.b;
				
				if (a_reg == b_reg) {
					Val src;
					use(ctx, f, &src, a_reg, r);
					convert_l2r(ctx, f, a_reg, &src);
					
					isel_aliased(ctx, f, v, info, r, a_reg, &src);
				} else {
					Val a, b;
					use(ctx, f, &a, a_reg, r);
					use(ctx, f, &b, b_reg, r);
					convert_l2r(ctx, f, a_reg, &a);
					convert_l2r(ctx, f, b_reg, &b);
					
					if (dt.count == 1 &&
						reg_type == TB_ADD &&
						a.type == VAL_GPR &&
						b.type == VAL_GPR) {
						def_new_gpr(ctx, f, v, r, dt.type);
						
						// lea _0, [_1 + _2]
						// TODO(NeGate): Maybe allow for the 32bit version sometimes
						GPR dst_gpr = v->gpr;
						
						emit(rex(true, dst_gpr, b.gpr, a.gpr));
						emit(0x8D);
						emit(mod_rx_rm(MOD_INDIRECT, dst_gpr, RSP));
						emit(mod_rx_rm(SCALE_X1, (b.gpr & 7) == RSP ? RSP : b.gpr, a.gpr));
					} else if (dt.count == 1 &&
							   reg_type == TB_ADD &&
							   a.type == VAL_GPR &&
							   b.type == VAL_IMM) {
						// lea _0, [_1 + _2]
						def_new_gpr(ctx, f, v, r, dt.type);
						Val arith = val_base_disp(dt, a.gpr, b.imm);
						
						inst2(ctx, LEA, v, &arith, dt.type);
					} else if (dt.count == 1 &&
							   reg_type == TB_SUB &&
							   a.type == VAL_GPR &&
							   b.type == VAL_IMM) {
						// lea _0, [_1 + _2]
						def_new_gpr(ctx, f, v, r, dt.type);
						Val arith = val_base_disp(dt, a.gpr, -b.imm);
						
						inst2(ctx, LEA, v, &arith, dt.type);
					} else {
						isel(ctx, f, v, info, r, a_reg, b_reg, &a, &b);
					}
				}
				break;
			}
			case TB_SHL: {
				TB_Register a_reg = p.i_arith.a;
				TB_Register b_reg = p.i_arith.b;
				
				Val a;
				use(ctx, f, &a, a_reg, r);
				convert_l2r(ctx, f, a_reg, &a);
				
				if (f->nodes.type[b_reg] == TB_INT_CONST) {
					assert(f->nodes.payload[b_reg].i_const.hi == 0);
					assert(f->nodes.payload[b_reg].i_const.lo < 64);
					
					if (f->nodes.type[next] == TB_RET) {
						// The destination is now RAX since that'll save us a move
						// and we can also use RAX as a temporary without allocating
						// it.
						*v = val_gpr(TB_I64, RAX);
						GPR dst_gpr = RAX;
						
						if (a.type != VAL_GPR) {
							// move into a register
							inst2(ctx, MOV, v, &a, dt.type);
						}
						
						bool is_64bit = dt.type == TB_I64 || dt.type == TB_PTR;
						if (f->nodes.payload[b_reg].i_const.lo == 1) {
							// lea _0, [_1 + _1]
							emit(rex(is_64bit, dst_gpr, a.gpr, a.gpr));
							emit(0x8D);
							emit(mod_rx_rm(MOD_INDIRECT, dst_gpr, RSP));
							emit(mod_rx_rm(SCALE_X1, (a.gpr & 7) == RSP ? RSP : a.gpr, a.gpr));
						} else {
							emit(rex(is_64bit, 0x00, dst_gpr, 0x00));
							emit(dt.type == TB_I8 ? 0xC0 : 0xC1);
							emit(mod_rx_rm(MOD_DIRECT, 0x04, dst_gpr));
							emit(f->nodes.payload[b_reg].i_const.lo);
						}
						break;
					} else {
						def_new_gpr(ctx, f, v, r, dt.type);
						inst2(ctx, MOV, v, &a, dt.type);
						
						GPR dst_gpr = v->gpr;
						bool is_64bit = dt.type == TB_I64 || dt.type == TB_PTR;
						
						if (dt.type == TB_I16) emit(0x66);
						emit(rex(is_64bit, 0x00, dst_gpr, 0x00));
						emit(dt.type == TB_I8 ? 0xC0 : 0xC1);
						emit(mod_rx_rm(MOD_DIRECT, 0x04, dst_gpr));
						emit(f->nodes.payload[b_reg].i_const.lo);
						break;
					}
				}
				
				tb_todo();
			}
			case TB_UDIV:
			case TB_SDIV: {
				evict_gpr(ctx, f, RAX, r);
				evict_gpr(ctx, f, RDX, r);
				
				TB_Register a_reg = f->nodes.payload[r].i_arith.a;
				TB_Register b_reg = f->nodes.payload[r].i_arith.b;
				
				Val a, b;
				use(ctx, f, &a, a_reg, r);
				use(ctx, f, &b, b_reg, r);
				convert_l2r(ctx, f, a_reg, &a);
				convert_l2r(ctx, f, b_reg, &b);
				
				// needs to mov the a value into rdx:rax
				Val rax = val_gpr(dt.type, RAX);
				if (!is_value_gpr(&a, RAX)) {
					inst2(ctx, MOV, &rax, &a, dt.type);
				}
				
				// we'll just be using CDQ if it's signed
				// and zeroing RDX it if it's unsigned. 
				if (reg_type == TB_SDIV) {
					emit(0x99);
				} else {
					emit(0x31);
					emit(mod_rx_rm(MOD_DIRECT, RDX, RDX));
				}
				
				if (b.type == VAL_IMM) {
					Val new_b;
					def_new_gpr(ctx, f, &new_b, b_reg, dt.type);
					inst2(ctx, MOV, &new_b, &b, dt.type);
					b = new_b;
				}
				
				inst1(ctx, IDIV, &b);
				
				// the return value is in RAX
				def(ctx, f, rax, r);
				*v = rax;
				break;
			}
			case TB_CMP_EQ:
			case TB_CMP_NE:
			case TB_CMP_SLT:
			case TB_CMP_SLE:
			case TB_CMP_ULT:
			case TB_CMP_ULE: {
				TB_DataType cmp_dt = p.cmp.dt;
				
				Val a, b;
				use(ctx, f, &a, p.cmp.a, r);
				use(ctx, f, &b, p.cmp.b, r);
				convert_l2r(ctx, f, p.cmp.a, &a);
				convert_l2r(ctx, f, p.cmp.b, &b);
				
				bool invert = false;
				if (a.type == VAL_MEM && b.type == VAL_MEM) {
					Val dst;
					def_new_gpr(ctx, f, &dst, r, cmp_dt.type);
					inst2(ctx, MOV, &dst, &a, cmp_dt.type);
					inst2(ctx, CMP, &dst, &b, cmp_dt.type);
					kill(ctx, f, r);
				}
				else {
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
				
				// TODO(NeGate): Implement the case where the value is converted 
				// into a byte, IF nodes don't require it but it may come up in 
				// code.
				if (f->nodes.type[next] != TB_IF) {
					def_new_gpr(ctx, f, v, r, dt.type);
					
					if (v->gpr >= 8) emit(rex(true, v->gpr, v->gpr, 0));
					emit(0x31);
					emit(mod_rx_rm(MOD_DIRECT, v->gpr, v->gpr));
					
					emit(0x0F);
					emit(0x90 + cc);
					emit(mod_rx_rm(MOD_DIRECT, v->gpr, v->gpr));
					break;
				}
				
				*v = (Val) { .type = VAL_FLAGS, .cond = cc };
				break;
			}
			default: tb_todo();
		}
	}
}

static void load_into(Ctx* ctx, TB_Function* f, TB_DataType dt, Val* v, TB_Register r, TB_Register addr, TB_Register next) {
	use(ctx, f, v, addr, next);
	
	// Desugar booleans into bytes
	if (v->dt.type == TB_BOOL) v->dt.type = TB_I8;
	
	if (v->mem.is_rvalue) {
		// deref
		Val new_v;
		def_new_gpr(ctx, f, &new_v, r, v->dt.type);
		inst2(ctx, MOV, &new_v, v, v->dt.type);
		
		*v = val_base_disp(v->dt, new_v.gpr, 0);
	}
	
	v->dt = dt;
	v->mem.is_rvalue = true;
}

static void store_into(Ctx* ctx, TB_Function* f, TB_DataType dt, const Val* dst, TB_Register r, TB_Register dst_reg, TB_Register val_reg) {
	int op = MOV;
	
	// Peephole folded operation:
	// OP dst, src
	if (f->nodes.type[val_reg] >= TB_AND && f->nodes.type[val_reg] <= TB_SUB) {
		TB_Register a = f->nodes.payload[val_reg].i_arith.a;
		TB_Register b = f->nodes.payload[val_reg].i_arith.b;
		
		bool is_phi = f->nodes.type[r] == TB_PHI2;
		bool folded = false;
		if (is_phi) {
			folded = is_phi_that_contains(f, r, a);
		} else {
			folded = (f->nodes.type[a] == TB_LOAD && f->nodes.payload[a].load.address == dst_reg);
		}
		
		if (folded) {
			switch (f->nodes.type[val_reg]) {
				case TB_AND: op = AND; break;
				case TB_OR: op = OR; break;
				case TB_XOR: op = XOR; break;
				case TB_ADD: op = ADD; break;
				case TB_SUB: op = SUB; break;
				default: tb_unreachable();
			}
			
			val_reg = b;
		}
	}
	
	// Default case:
	// mov dst, src
	// or
	// mov temp, src
	// mov dst, temp
	Val value;
	use(ctx, f, &value, val_reg, r);
	convert_l2r(ctx, f, val_reg, &value);
	
	// if they match and it's just a MOV, don't do it
	if (op == MOV && is_value_match(dst, &value)) return;
	
	if (value.type != VAL_MEM) {
		inst2(ctx, op, dst, &value, dt.type);
	} else {
		Val temp;
		def_new_gpr(ctx, f, &temp, r, dt.type);
		
		inst2(ctx, MOV, &temp, &value, dt.type);
		inst2(ctx, op, dst, &temp, dt.type);
		
		kill(ctx, f, r);
	}
}

static void kill(Ctx* ctx, TB_Function* f, TB_Register r) {
	printf("   kill r%u\n", r);
	Val* v = &ctx->addr_desc[r];
	
	if (v->type == VAL_GPR) ctx->gpr_desc[v->gpr] = 0;
	else if (v->type == VAL_XMM) ctx->xmm_desc[v->xmm] = 0;
	
	// params die different lmao
	if (f->nodes.type[r] != TB_PARAM) {
		// Maybe just clear the type,
		// TODO(NeGate): Perf?
		ctx->addr_desc[r] = (Val){ 0 };
	}
}

static void def(Ctx* ctx, TB_Function* f, const Val v, TB_Register r) {
	if (v.type == VAL_GPR) ctx->gpr_desc[v.gpr] = r;
	else if (v.type == VAL_XMM) ctx->xmm_desc[v.xmm] = r;
	
	ctx->addr_desc[r] = v;
	printf("   def r%u\n", r);
}

static void def_new_gpr(Ctx* ctx, TB_Function* f, Val* v, TB_Register r, int dt_type) {
	do {
		for (size_t i = 0; i < tb_arrlen(GPR_PRIORITY_LIST); i++) {
			GPR gpr = GPR_PRIORITY_LIST[i];
			
			if (ctx->gpr_desc[gpr] == 0) {
				// mark register as to be saved
				ctx->regs_to_save |= (1u << gpr) & ABI_CALLEE_SAVED;
				
				Val val = val_gpr(dt_type, gpr);
				ctx->gpr_desc[gpr] = r;
				if (r != TB_REG_MAX && r != TB_REG_TEMP) ctx->addr_desc[r] = val;
				
				*v = val;
				return;
			}
		}
		
		// If it fails just try GCing it and hopefully space was made.
	} while (garbage_collect_gpr(ctx, f, r));
	
	// TODO(NeGate): Try figuring out which register is best to evict.
	for (size_t i = 14; i--;) {
		GPR gpr = GPR_PRIORITY_LIST[i];
		
		if (evict_gpr(ctx, f, gpr, r)) {
			ctx->gpr_desc[gpr] = r;
			Val val = val_gpr(dt_type, gpr);
			if (r != TB_REG_MAX && r != TB_REG_TEMP) ctx->addr_desc[r] = val;
			
			*v = val;
			return;
		}
	}
	
	abort();
}

static void def_new_xmm(Ctx* ctx, TB_Function* f, Val* v, TB_Register r, TB_DataType dt) {
	do {
		for (size_t i = 0; i < 16; i++) {
			if (ctx->xmm_desc[i] == 0) {
				// mark register as to be saved
				// ctx->regs_to_save |= (1u << (i + 32)) & ABI_CALLEE_SAVED;
				
				ctx->xmm_desc[i] = r;
				
				Val val = val_xmm(dt, i);
				if (r != TB_REG_MAX && r != TB_REG_TEMP) ctx->addr_desc[r] = val;
				
				*v = val;
				printf("   allocated r%u <- XMM%d\n", r, i);
				return;
			}
		}
		
		// If it fails just try GCing it and hopefully space was made.
	} while (garbage_collect_xmm(ctx, f, r));
	
	// TODO(NeGate): Try figuring out which register is best to evict.
	for (size_t i = 16; i--;) {
		if (evict_xmm(ctx, f, i, r)) {
			ctx->xmm_desc[i] = r;
			Val val = val_xmm(dt, i);
			if (r != TB_REG_MAX && r != TB_REG_TEMP) ctx->addr_desc[r] = val;
			
			*v = val;
			return;
		}
	}
	
	abort();
}

static bool evict_gpr(Ctx* ctx, TB_Function* f, GPR g, TB_Register r) {
	TB_Register bound = ctx->gpr_desc[g];
	
	// if it dies... right now!! we don't need to spill it
	if (bound != TB_REG_MAX && ctx->intervals[bound] == (r - 1)) return true;
	
	if (bound && bound != TB_REG_MAX) {
		printf("   evicted %s\n", GPR_NAMES[g]);
		
		// unbind it
		ctx->gpr_desc[g] = 0;
		
		// spill it
		ctx->stack_usage = align_up(ctx->stack_usage + 8, 8);
		Val dst = val_stack(f->nodes.dt[bound], -ctx->stack_usage);
		dst.mem.is_rvalue = true;
		
		Val src = val_gpr(TB_I64, g);
		
		inst2(ctx, MOV, &dst, &src, TB_I64);
		ctx->addr_desc[bound] = dst;
		return true;
	}
	
	return false;
}

static bool evict_xmm(Ctx* ctx, TB_Function* f, XMM x, TB_Register r) {
	TB_Register bound = ctx->xmm_desc[x];
	
	// if it dies... right now!! we don't need to spill it
	if (bound != TB_REG_MAX && ctx->intervals[bound] == (r - 1)) return true;
	
	if (bound && bound != TB_REG_MAX) {
		printf("   evicted XMM%d\n", x);
		
		// unbind it
		ctx->xmm_desc[x] = 0;
		
		// spill it
		TB_DataType dt = f->nodes.dt[bound];
		
		Val dst;
		if (dt.count > 1) {
			ctx->stack_usage = align_up(ctx->stack_usage + 16, 16);
			dst = val_stack(f->nodes.dt[bound], -ctx->stack_usage);
			
			Val src = val_xmm(dt, x);
			inst2(ctx, MOVAPS, &dst, &src, TB_F32);
		} else if (dt.type == TB_F32) {
			ctx->stack_usage = align_up(ctx->stack_usage + 8, 8);
			dst = val_stack(f->nodes.dt[bound], -ctx->stack_usage);
			
			Val src = val_xmm(dt, x);
			inst2(ctx, MOVSS, &dst, &src, TB_F32);
		} else tb_todo();
		
		ctx->addr_desc[bound] = dst;
		return true;
	}
	
	return false;
}

static void materialize(Ctx* ctx, TB_Function* f, Val* dst, const Val* src, TB_Register src_reg, TB_DataType dt) {
	if (dt.type == TB_F32 || dt.count > 1) {
		def_new_xmm(ctx, f, dst, src_reg, dt);
		inst2(ctx, dt.count == 1 ? MOVSS : MOVAPS, dst, src, dt.type);
	} else if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
		def_new_gpr(ctx, f, dst, src_reg, dt.type);
		inst2(ctx, MOV, dst, src, dt.type);
	} else {
		tb_todo();
	}
}

static void isel(Ctx* ctx, TB_Function* f, Val* v, const IselInfo* info,
				 TB_Register dst_reg, TB_Register a_reg, TB_Register b_reg, 
				 const Val* a, const Val* b) {
	// use isel_aliased instead
	assert(a != b);
	
	TB_DataType dst_dt = f->nodes.dt[dst_reg];
	bool is_xmm = dst_dt.count > 1 || TB_IS_FLOAT_TYPE(dst_dt.type);
	
	// (i8 | i16) => i32
	TB_DataType prev_dt;
	bool is_promoted = dst_dt.count == 1 && (dst_dt.type == TB_I8 || dst_dt.type == TB_I16);
	if (is_promoted) {
		prev_dt = dst_dt;
		dst_dt = TB_TYPE_I32;
	}
	
	// NOTE(NeGate): The operands are ordered for this "magic"
	if (info->communitive && a->type < b->type) tb_swap(a, b);
	
	// If both source operands are memory addresses, promote one into a register
	Val av;
	if (a->type == VAL_MEM && b->type == VAL_MEM) {
		printf("materialize r%u\n", a_reg);
		
		materialize(ctx, f, &av, a, a_reg, dst_dt);
	} else {
		av = *a;
	}
	
	// Some instructions don't have an immediate form
	Val bv;
	if (!info->has_immediates && b->type == VAL_IMM) {
		assert(TB_IS_INTEGER_TYPE(dst_dt.type) || dst_dt.type == TB_PTR);
		
		// special case of materialize where it can only ever 
		// be an integral type
		printf("materialize r%u\n", b_reg);
		
		def_new_gpr(ctx, f, &bv, b_reg, dst_dt.type);
		inst2(ctx, MOV, &bv, b, dst_dt.type);
	} else {
		bv = *b;
	}
	
	bool recycle = a->type != VAL_IMM &&
		ctx->intervals[a_reg] == dst_reg &&
		ctx->use_count[a_reg] == 1;
	
	// Can't recycle memory if the operation can't store directly into it
	if (!info->has_memory_dst && a->type == VAL_MEM) recycle = false;
	
	if (recycle) {
		assert(av.type != VAL_MEM);
		
		// (a => dst) += b
		kill(ctx, f, a_reg);
		inst2(ctx, info->inst, &av, &bv, dst_dt.type);
		def(ctx, f, av, dst_reg);
		
		*v = av;
	} else {
		// dst = a, dst += b;
		if (is_xmm) def_new_xmm(ctx, f, v, dst_reg, dst_dt);
		else def_new_gpr(ctx, f, v, dst_reg, dst_dt.type);
		
		int mov_type = MOV;
		if (dst_dt.count > 1) mov_type = MOVAPS;
		else if (dst_dt.type == TB_F32) mov_type = MOVSS;
		
		inst2(ctx, mov_type, v, &av, dst_dt.type);
		inst2(ctx, info->inst, v, &bv, dst_dt.type);
	}
	
	if (is_promoted) {
		inst2(ctx, MOVZX, v, v, prev_dt.type);
	}
}

static void isel_aliased(Ctx* ctx, TB_Function* f, Val* v, const IselInfo* info,
						 TB_Register dst_reg, TB_Register src_reg, 
						 const Val* src) {
	TB_DataType dst_dt = f->nodes.dt[dst_reg];
	
	// (i8 | i16) => i32
	TB_DataType prev_dt;
	bool is_promoted = dst_dt.count == 1 && (dst_dt.type == TB_I8 || dst_dt.type == TB_I16);
	if (is_promoted) {
		prev_dt = dst_dt;
		dst_dt = TB_TYPE_I32;
	}
	
	bool recycle = src->type != VAL_IMM && ctx->intervals[src_reg] == dst_reg;
	
	// Can't recycle memory if the operation can't store directly into it
	if (!info->has_memory_dst && src->type == VAL_MEM) recycle = false;
	
	if (recycle) {
		// src += src
		kill(ctx, f, src_reg);
		inst2(ctx, info->inst, src, src, dst_dt.type);
		def(ctx, f, *src, dst_reg);
		
		*v = *src;
	} else {
		// dst = src, dst += src;
		def_new_gpr(ctx, f, v, dst_reg, dst_dt.type);
		
		int mov_type = MOV;
		if (dst_dt.count > 1) mov_type = MOVAPS;
		else if (dst_dt.type == TB_F32) mov_type = MOVSS;
		inst2(ctx, mov_type, v, src, dst_dt.type);
		inst2(ctx, info->inst, v, src, dst_dt.type);
	}
	
	if (is_promoted) {
		inst2(ctx, MOVZX, v, v, prev_dt.type);
	}
}

static bool garbage_collect_gpr(Ctx* ctx, TB_Function* f, TB_Register end) {
	TB_Register bb = ctx->current_bb;
	TB_Register bb_end = ctx->current_bb_end;
	
	int changes = 0;
	for (size_t i = 0; i < 16; i++) {
		TB_Register r = ctx->gpr_desc[i];
		if (r <= bb || r > bb_end) continue;
		if (f->nodes.type[r] == TB_PHI1) continue;
		if (f->nodes.type[r] == TB_PHI2) continue;
		
		if (ctx->intervals[r] < end) {
			if (f->nodes.type[r] == TB_PARAM) {
				printf("   param spill r%u : %s\n", r, GPR_NAMES[i]);
				
				// Unbind the GPR and update the stack slot
				int id = f->nodes.payload[r].param.id;
				
				Val dst = val_stack(f->nodes.dt[r], ctx->params[id]);
				Val src = val_gpr(TB_I64, i);
				
				inst2(ctx, MOV, &dst, &src, TB_I64);
				ctx->gpr_desc[i] = 0;
			} else {
				printf("   gc kill r%u\n", r);
				ctx->gpr_desc[i] = 0;
				ctx->addr_desc[r] = (Val) { 0 };
			}
			changes++;
		}
	}
	
	return changes;
}

static bool garbage_collect_xmm(Ctx* ctx, TB_Function* f, TB_Register end) {
	TB_Register bb = ctx->current_bb;
	TB_Register bb_end = ctx->current_bb_end;
	
	int changes = 0;
	for (size_t i = 0; i < 16; i++) {
		TB_Register r = ctx->xmm_desc[i];
		if (r <= bb || r > bb_end) continue;
		if (f->nodes.type[r] == TB_PHI1) continue;
		if (f->nodes.type[r] == TB_PHI2) continue;
		
		if (ctx->intervals[r] < end) {
			if (f->nodes.type[r] == TB_PARAM) {
				printf("   param spill r%u : %s\n", r, GPR_NAMES[i]);
				
				// Unbind the GPR and update the stack slot
				int id = f->nodes.payload[r].param.id;
				
				Val dst = val_stack(f->nodes.dt[r], ctx->params[id]);
				Val src = val_gpr(TB_I64, i);
				
				inst2(ctx, MOV, &dst, &src, TB_F32);
				ctx->xmm_desc[i] = 0;
			} else {
				printf("   gc kill r%u\n", r);
				ctx->xmm_desc[i] = 0;
				ctx->addr_desc[r] = (Val) { 0 };
			}
			changes++;
		}
	}
	
	return changes;
}

static bool is_temporary_of_bb(Ctx* ctx, TB_Function* f, GPR gpr, TB_Register bb, TB_Register bb_end) {
	TB_Register bound = ctx->gpr_desc[gpr];
	
	if (bound >= bb &&
		bound <= bb_end &&
		f->nodes.type[bound] != TB_PHI1 &&
		f->nodes.type[bound] != TB_PHI2) {
		return true;
	}
	
	return false;
}

static void convert_l2r(Ctx* ctx, TB_Function* f, TB_Register r, Val* val) {
	if (val->dt.type == TB_BOOL) val->dt.type = TB_I8;
	
	if (val->type == VAL_MEM) {
		if (is_address_node(f->nodes.type[r])) {
			assert(!val->mem.is_rvalue);
			
			Val new_v;
			def_new_gpr(ctx, f, &new_v, TB_REG_TEMP, val->dt.type);
			inst2(ctx, LEA, &new_v, val, val->dt.type);
			
			*val = new_v;
		} else if (!val->mem.is_rvalue) {
			// TODO(NeGate): Implement vector/float rvalue-conversion
			Val new_v;
			def_new_gpr(ctx, f, &new_v, r, val->dt.type);
			inst2(ctx, MOV, &new_v, val, val->dt.type);
			
			*val = new_v;
		}
	}
}

// Is this a phi node? does it can the register `reg`?
static bool is_phi_that_contains(TB_Function* f, TB_Register phi, TB_Register reg) {
	if (f->nodes.type[phi] == TB_PHI1) {
		return f->nodes.payload[phi].phi1.a == reg;
	} else if (f->nodes.type[phi] == TB_PHI2) {
		return f->nodes.payload[phi].phi2.a == reg || f->nodes.payload[phi].phi2.b == reg;
	} else {
		return false;
	}
}

static PhiValue* find_phi(Ctx* ctx, TB_Register r) {
	for (size_t i = 0; i < ctx->phi_count; i++) {
		if (ctx->phis[i].reg == r) return &ctx->phis[i];
	}
	
	return NULL;
}

static bool is_address_node(TB_RegType t) {
	switch (t) {
		case TB_LOCAL:
		case TB_PARAM_ADDR:
		case TB_ARRAY_ACCESS:
		case TB_MEMBER_ACCESS:
		return true;
		default: 
		return false;
	}
}

static int get_data_type_size(const TB_DataType dt) {
	switch (dt.type) {
		// TODO(NeGate): Cannot pass void or boolean via parameter
		case TB_VOID:
		case TB_BOOL:
		return 0;
		case TB_I8: 
		return 1 * dt.count;
		case TB_I16: 
		return 2 * dt.count;
		case TB_I32: case TB_F32:
		return 4 * dt.count;
		case TB_I64: case TB_F64:
		return 8 * dt.count;
		case TB_PTR:
		return 8;
		default: tb_todo();
	}
}

void x64_emit_call_patches(TB_Module* m, uint32_t func_layout[]) {
	for (size_t i = 0; i < m->call_patches.count; i++) {
		TB_FunctionPatch* p = &m->call_patches.data[i];
		TB_FunctionOutput* out_f = &m->compiled_functions.data[p->func_id];
		
		uint64_t meta = out_f->prologue_epilogue_metadata;
		uint64_t stack_usage = out_f->stack_usage;
		uint8_t* code = out_f->code;
		
		size_t actual_pos = func_layout[p->func_id] 
			+ x64_get_prologue_length(meta, stack_usage)
			+ p->pos
			+ 4;
		
		*((uint32_t*)&code[p->pos]) = func_layout[p->target_id] - actual_pos;
	}
}

// I put it down here because i can :P
ICodeGen x64_fast_code_gen = {
	.emit_call_patches = x64_emit_call_patches,
	.get_prologue_length = x64_get_prologue_length,
	.get_epilogue_length = x64_get_epilogue_length,
	.emit_prologue = x64_emit_prologue,
	.emit_epilogue = x64_emit_epilogue,
	.compile_function = x64_compile_function
};
