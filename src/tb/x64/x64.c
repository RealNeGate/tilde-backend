// This entire module is one translation unit so that it doesn't have to worry
// about C's crappy support for public and private interfaces.
#include "x64.h"
#include "inst.h"
#include "proepi.h"

#if 0
#define DEBUG_LOG(...) printf(__VA_ARGS__)

static const char* GPR_NAMES[] = {
	"RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI",
	"R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"
};
#else
#define DEBUG_LOG(...) ((void)0)
#endif

static int get_data_type_size(const TB_DataType dt);
static PhiValue* find_phi(Ctx* ctx, TB_Register r);
static bool is_address_node(TB_RegType t);

static void eval_basic_block(Ctx* ctx, TB_Function* f, TB_Register bb, TB_Register bb_end);

// Just handles the PHI nodes that we'll encounter when leaving `from`
// into `to`
static void eval_terminator_phis(Ctx* ctx, TB_Function* f, TB_Register from, TB_Register from_terminator, TB_Register to, TB_Register to_terminator);
static bool is_local_of_bb(Ctx* ctx, TB_Function* f, TB_Register bound, TB_Register bb, TB_Register bb_end);
static bool can_recycle_into(Ctx* ctx, TB_Function* f, TB_Register to, TB_Register from);

TB_FunctionOutput x64_compile_function(TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id) {
	s_local_thread_id = local_thread_id;
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	////////////////////////////////
	// Allocate all the memory we'll need
	////////////////////////////////
	bool is_ctx_heap_allocated = false;
	Ctx* ctx;
	
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
			
			if (t == TB_PHI2) phi_count++;
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
		
		const size_t ctx_size = sizeof(Ctx) + (f->nodes.count * sizeof(Val));
		
		ctx = tb_tls_try_push(tls, ctx_size);
		if (!ctx) {
			// try the heap (not ideal)
			// also we do an all-or-nothing style where if you can't do the context
			// in the temporary storage, then none of the other uses of it are available.
			is_ctx_heap_allocated = true;
			ctx = malloc(ctx_size);
			
			printf("Could not allocate x64 code gen context: using heap fallback.\n");
		}
		
		memset(ctx, 0, ctx_size);
		ctx->start_out = ctx->out = out;
		ctx->f = f;
		ctx->function_id = f - f->module->functions.data;
		
		if (is_ctx_heap_allocated) {
			ctx->intervals = malloc(f->nodes.count * sizeof(TB_Register));
			ctx->use_count = malloc(f->nodes.count * sizeof(int));
			ctx->phis = malloc(phi_count * sizeof(PhiValue));
			
			ctx->phi_queue = malloc(phi_count * sizeof(TB_Register));
			
			ctx->params = malloc(f->prototype->param_count * sizeof(int32_t));
			ctx->labels = malloc(f->label_count * sizeof(uint32_t));
			ctx->label_patches = malloc(label_patch_count * sizeof(LabelPatch));
			ctx->ret_patches = malloc(return_count * sizeof(ReturnPatch));
		} else {
			ctx->intervals = tb_tls_push(tls, f->nodes.count * sizeof(TB_Register));
			ctx->use_count = tb_tls_push(tls, f->nodes.count * sizeof(int));
			ctx->phis = tb_tls_push(tls, phi_count * sizeof(PhiValue));
			
			ctx->phi_queue = tb_tls_push(tls, phi_count * sizeof(TB_Register));
			
			ctx->params = tb_tls_push(tls, f->prototype->param_count * sizeof(int32_t));
			ctx->labels = tb_tls_push(tls, f->label_count * sizeof(uint32_t));
			ctx->label_patches = tb_tls_push(tls, label_patch_count * sizeof(LabelPatch));
			ctx->ret_patches = tb_tls_push(tls, return_count * sizeof(ReturnPatch));
		}
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
						  .reg = i, 
						  .storage_a = f->nodes.payload[i].phi2.a,
						  .storage_b = f->nodes.payload[i].phi2.b
					  };
				  });
	
    // Reserve stack and base pointer
	ctx->gpr_desc[RSP] = TB_REG_MAX;
	ctx->gpr_desc[RBP] = TB_REG_MAX;
	
	// Allocate local and parameter stack slots
	int stack_usage = 0;
	
	DEBUG_LOG("\n\n\nFUNCTION %s:\n", f->name);
	
	const TB_FunctionPrototype* restrict proto = f->prototype;
	loop(i, (size_t) proto->param_count) {
		TB_DataType dt = proto->params[i];
		
		int size = get_data_type_size(dt);
		stack_usage = align_up(stack_usage + size, size);
		
		TB_Register reg = TB_FIRST_PARAMETER_REG+i;
		
		// The first few parameters are backed by registers
		// not memory addresses but they have space reserved
		// for spilling so it's fine that we mark them in the
		// locals.
		if (i < 4) {
			if (TB_IS_FLOAT_TYPE(dt.type) || dt.count > 1) {
				DEBUG_LOG("   PARAM XMM%zu\n", i);
				
				def(ctx, f, val_xmm(dt, i), reg);
			} else if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
				DEBUG_LOG("   PARAM GPR %s\n", GPR_NAMES[GPR_PARAMETERS[i]]);
				
				def(ctx, f, val_gpr(dt.type, GPR_PARAMETERS[i]), reg);
			}
		} else {
			def(ctx, f, val_stack(dt, -stack_usage), reg);
		}
		
		ctx->params[i] = -stack_usage;
	}
	
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
							  if (dt.count > 1) {
								  inst2(ctx, MOVAPS, &dst, &src, dt.type);
							  } else {
								  if (dt.type == TB_F32) {
									  inst2(ctx, MOVSS, &dst, &src, dt.type);
								  } else if (dt.type == TB_F64) {
									  inst2(ctx, MOVSD, &dst, &src, dt.type);
								  } else tb_unreachable();
							  }
						  } else if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
							  Val src = val_gpr(TB_I64, GPR_PARAMETERS[id]); 
							  
							  // don't keep reference to GPR, we'll be using the
							  // memory version only
							  ctx->gpr_desc[GPR_PARAMETERS[id]] = 0;
							  DEBUG_LOG("   move to stack slot %s\n", GPR_NAMES[GPR_PARAMETERS[id]]);
							  
							  // save the shadow space into the stack
							  inst2(ctx, MOV, &dst, &src, dt.type);
						  } else tb_todo();
						  
						  def(ctx, f, dst, i);
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
		//const TB_DataType dt = f->nodes.dt[bb_end];
		const TB_RegPayload p = f->nodes.payload[bb_end];
		
		if (reg_type == TB_RET) {
			// Evaluate return value
			if (p.ret.value) {
				Val value = use_as_rvalue(ctx, f, p.ret.value);
				
				if (TB_IS_FLOAT_TYPE(value.dt.type) || value.dt.count > 1) {
					// Float results use XMM0
					Val dst = val_xmm(value.dt, XMM0);
					
					if (!is_value_xmm(&value, XMM0)) {
						int mov_type = MOVSS;
						if (value.dt.type == TB_F64) mov_type = MOVSD;
						if (value.dt.count > 1) mov_type = MOVAPS;
						
						inst2(ctx, mov_type, &dst, &value, value.dt.type);
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
					} else if (!is_value_gpr(&value, RAX)) {
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
			
			Val cond = use(ctx, f, p.if_.cond);
			if (cond.type == VAL_IMM) {
				TB_Label dst = (cond.imm ? if_true : if_false);
				
				if (dst != f->nodes.payload[bb_end + 1].label.id) jmp(ctx, dst);
			} else {
				// Implicit convert into FLAGS
				if (cond.type == VAL_GPR) {
					inst2(ctx, TEST, &cond, &cond, cond.dt.type);
					cond = val_flags(NE);
				} else if (cond.type == VAL_MEM || cond.type == VAL_GLOBAL) {
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
		} else if (reg_type == TB_TRAP) {
			// ud2
			emit(0x0F); 
			emit(0x0B);
		} else if (reg_type == TB_UNREACHABLE) {
			// doesn't need to do anything because it's all UB from here
		} else {
			tb_todo();
		}
		
		// Next Basic block
		bb = bb_end + ((reg_type == TB_LABEL) ? 0 : 1);
		
		// Garbage collect for the basic block
		garbage_collect_gpr(ctx, f, bb);
		garbage_collect_xmm(ctx, f, bb);
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
	
	TB_FunctionOutput func_out = {
		.name = f->name,
		.code = ctx->start_out,
		.code_size = ctx->out - ctx->start_out,
		.stack_usage = stack_usage,
		
		.prologue_epilogue_metadata = ctx->regs_to_save
	};
	
	if (is_ctx_heap_allocated) {
		free(ctx->intervals);
		free(ctx->use_count);
		free(ctx->phis);
		
		free(ctx->phi_queue);
		
		free(ctx->params);
		free(ctx->labels);
		free(ctx->label_patches);
		free(ctx->ret_patches);
		free(ctx);
	}
	return func_out;
}

static void eval_terminator_phis(Ctx* ctx, TB_Function* f, TB_Register from, TB_Register from_terminator, TB_Register to, TB_Register to_terminator) {
	ctx->phi_queue_count = 0;
	
	loop_range(i, to, to_terminator) if (f->nodes.type[i] == TB_PHI2) {
		TB_RegPayload p = f->nodes.payload[i];
		assert(p.phi2.a_label != p.phi2.b_label);
		
		TB_Register src;
		if (p.phi2.a_label == from) src = p.phi2.a;
		else if (p.phi2.b_label == from) src = p.phi2.b;
		else tb_unreachable();
		
		if (src >= to && src <= to_terminator) {
			// reschedule it's dependency to happen first
			//
			// if it's already been placed then it's fine, but if not
			// placed it first then place this node.
			bool found = false;
			loop(j, ctx->phi_queue_count) if (ctx->phi_queue[j] == src) {
				ctx->phi_queue[ctx->phi_queue_count++] = i;
				found = true;
				break;
			}
			
			if (!found) {
				assert(f->nodes.type[src] == TB_PHI2);
				ctx->phi_queue[ctx->phi_queue_count++] = src;
				ctx->phi_queue[ctx->phi_queue_count++] = i;
			}
		} else {
			// If it's already been scheduled don't worry about it
			bool found = false;
			loop(j, ctx->phi_queue_count) if (ctx->phi_queue[j] == i) {
				found = true;
				break;
			}
			
			if (!found) {
				ctx->phi_queue[ctx->phi_queue_count++] = i;
			}
		}
	}
	
	loop(i, ctx->phi_queue_count) {
		TB_Register r = ctx->phi_queue[i];
		TB_DataType dt = f->nodes.dt[r];
		
		if (dt.type == TB_BOOL) dt.type = TB_I8;
		
		PhiValue* phi = find_phi(ctx, r);
		if (phi->value.type != VAL_NONE && !is_value_match(&phi->value, &ctx->addr_desc[r])) {
			// resync if they somehow changed (usually caused by spilling)
			inst2(ctx, MOV, &phi->value, &ctx->addr_desc[r], dt.type);
			
			def(ctx, f, phi->value, r);
		}
	}
	
	//printf("PHI QUEUE: ");
	loop(i, ctx->phi_queue_count) {
		TB_Register r = ctx->phi_queue[i];
		//printf("r%d ", r);
		
		TB_RegPayload* restrict p = &f->nodes.payload[r];
		TB_DataType dt = f->nodes.dt[r];
		
		if (dt.type == TB_BOOL) dt.type = TB_I8;
		
		TB_Register src;
		if (p->phi2.a_label == from) src = p->phi2.a;
		else if (p->phi2.b_label == from) src = p->phi2.b;
		else tb_unreachable();
		
		PhiValue* phi = find_phi(ctx, r);
		if (phi->value.type == VAL_NONE) {
			// Initialize it
			Val src_value = use_as_rvalue(ctx, f, src);
			
			if (src_value.type == VAL_GPR && 
				is_temporary_of_bb(ctx, f, src_value.gpr, from, from_terminator)) {
				// Recycle old value
				phi->value = src_value;
				def(ctx, f, src_value, r);
			} else {
				// Create a new GPR and map it
				phi->value = def_new_gpr(ctx, f, r, dt.type);
				
				// TODO(NeGate): Handle vector and float types
				if (!is_value_gpr(&src_value, phi->value.gpr)) {
					if (src_value.type == VAL_IMM && src_value.imm == 0) {
						GPR gpr = phi->value.gpr;
						
						if (gpr >= 8) emit(rex(true, gpr, gpr, 0));
						emit(0x31);
						emit(mod_rx_rm(MOD_DIRECT, gpr, gpr));
					}
					else {
						inst2(ctx, MOV, &phi->value, &src_value, dt.type);
					}
				}
			}
		} else if (src >= to && src <= to_terminator) {
			// this means we gotta do a swap
			assert(f->nodes.type[src] == TB_PHI2);
			PhiValue* other_phi = find_phi(ctx, src);
			
			inst2(ctx, XCHG, &phi->value, &other_phi->value, dt.type);
		} else {
			// Load value into existing phi node
			store_into(ctx, f, dt, &phi->value, r, r, src);
		}
	}
	
	//printf("\n");
	ctx->phi_queue_count = 0;
}

static void eval_basic_block(Ctx* ctx, TB_Function* f, TB_Register bb, TB_Register bb_end) {
	ctx->current_bb = bb;
	ctx->current_bb_end = bb_end;
	
	// Evaluate all side effect instructions
	size_t last_fence = bb + 1;
	for (size_t r = bb + 1; r < bb_end; r++) {
		TB_RegTypeEnum reg_type = f->nodes.type[r];
		if (!TB_IS_NODE_SIDE_EFFECT(reg_type)) continue;
		
		// If there's any non-side effect instructions which happen before a 
		// fence/side-effect instruction but are used afterwards, they're evaluated
		// before the fence. Also post-pone their death... eheh
		for (size_t j = last_fence; j < r; j++) {
			if (ctx->intervals[j] > r && f->nodes.type[j] != TB_LOAD) {
				def(ctx, f, use(ctx, f, j), j);
			}
		}
		last_fence = r;
		
		TB_DataType dt = f->nodes.dt[r];
		TB_RegPayload* restrict p = &f->nodes.payload[r];
		switch (reg_type) {
			case TB_LINE_INFO: {
				// Ignore the first one since it's got special rules
				if (r > 2) p->line_info.pos = code_pos();
				break;
			}
			case TB_LOAD: {
				TB_Register addr_reg = p->load.address;
				bool explicit_load = ctx->use_count[r] > 1;
				
				if (!explicit_load &&
					(f->nodes.type[addr_reg] == TB_LOCAL ||
					 f->nodes.type[addr_reg] == TB_PARAM_ADDR)) {
					// If this load out-lives the next side-effect
					// that might change it
					// TODO(NeGate): Implement noalias
					// TODO(NeGate): This is a fastpath code gen so
					// we don't necessarily need to be perfectly accurate.
					// we can limit look ahead for perf.
					size_t peek_start = r+1;
					size_t peek_end = bb_end;
					
					if (peek_end - peek_start > 50) {
						peek_end = peek_start + 50;
					}
					
					bool exhausted = true;
					loop_range(j, peek_start, peek_end) {
						if (f->nodes.type[j] == TB_STORE ||
							f->nodes.type[j] == TB_CALL ||
							f->nodes.type[j] == TB_VCALL ||
							f->nodes.type[j] == TB_ECALL) {
							explicit_load = (ctx->intervals[r] > j);
							exhausted = false;
							break;
						}
					}
					
					if (exhausted) {
						explicit_load = true;
					}
				}
				
				TB_DataType dt = f->nodes.dt[r];
				Val v = load_into(ctx, f, dt, r, addr_reg);
				
				if (explicit_load) {
					// Load into GPR
					Val storage = def_new_gpr(ctx, f, r, dt.type);
					inst2(ctx, MOV, &storage, &v, dt.type);
					
					def(ctx, f, storage, r);
				} else {
					// this means it's ok to use folded loads like
					// add rcx, [rbp-8]
					// instead of explicit loads like:
					// mov rax, [rbp-8]
					// add rcx, rax
					def(ctx, f, v, r);
				}
				break;
			}
			case TB_STORE: {
				TB_Register addr_reg = f->nodes.payload[r].store.address;
				TB_Register val_reg = f->nodes.payload[r].store.value;
				
				// Eval address and cast to the correct type for the store
				Val address = use_as_address(ctx, f, addr_reg);
				
				store_into(ctx, f, dt, &address, r, addr_reg, val_reg);
				break;
			}
			case TB_CALL:
			case TB_ECALL:
			case TB_VCALL: {
				int param_start = p->call.param_start;
				int param_count = p->call.param_end - p->call.param_start;
				
				// Evict the GPRs that are caller saved
				for (size_t j = 0; j < 16; j++) {
					if (ABI_CALLER_SAVED & (1u << j)) evict_gpr(ctx, f, j, r);
				}
				
				// Evict the XMMs that are caller saved
				for (size_t j = 0; j < param_count; j++) {
					TB_DataType param_dt = f->nodes.dt[param_start + j];
					
					if (j < 4 && (param_dt.count > 1 || TB_IS_FLOAT_TYPE(param_dt.type))) {
						evict_xmm(ctx, f, j, r);
					}
				}
				
				// evict return value
				if (dt.count > 1 || TB_IS_FLOAT_TYPE(dt.type)) {
					evict_xmm(ctx, f, XMM0, r);
					temporary_reserve_xmm(ctx, f, XMM0);
				} else if (dt.type != TB_VOID) {
					evict_gpr(ctx, f, RAX, r);
					temporary_reserve_gpr(ctx, f, RAX);
				}
				
				// evaluate parameters
				for (size_t j = 0; j < param_count; j++) {
					TB_Register param_reg = f->vla.data[param_start + j];
					
					Val param = use_as_rvalue(ctx, f, param_reg);
					
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
							temporary_reserve_xmm(ctx, f, j);
						} else if (param.dt.type != TB_VOID) {
							Val dst = val_gpr(param.dt.type, GPR_PARAMETERS[j]);
							if (!is_value_gpr(&param, GPR_PARAMETERS[j])) {
								inst2(ctx, MOV, &dst, &param, param.dt.type);
							}
							
							// reserve for a bit
							temporary_reserve_gpr(ctx, f, GPR_PARAMETERS[j]);
						}
					} else {
						Val dst = val_base_disp(param.dt, RSP, 8 * j);
						
						// parameter is in memory
						if (param.dt.count > 1 ||
							TB_IS_FLOAT_TYPE(param.dt.type)) {
							tb_todo();
						}
						else if (param.dt.type != TB_VOID) {
							Val tmp = val_gpr(param.dt.type, RAX);
							if (param.type == VAL_MEM) {
								inst2(ctx, MOV, &tmp, &param, param.dt.type);
								inst2(ctx, MOV, &dst, &tmp, param.dt.type);
							}
							else {
								inst2(ctx, MOV, &dst, &param, param.dt.type);
							}
						}
					}
				}
				
				// CALL instruction and patch
				int source_func = f - f->module->functions.data;
				
				if (reg_type == TB_CALL) {
					int target_func = p->call.target - f->module->functions.data;
					
					tb_emit_call_patch(f->module,
									   source_func,
									   target_func,
									   code_pos() + 1,
									   s_local_thread_id);
					
					// CALL rel32
					emit(0xE8);
					emit4(0x0);
				} else if (reg_type == TB_ECALL) {
					tb_emit_ecall_patch(f->module,
										source_func,
										p->ecall.target,
										code_pos() + 1,
										s_local_thread_id);
					
					// CALL rel32
					emit(0xE8);
					emit4(0x0);
				} else if (reg_type == TB_VCALL) {
					Val target_ptr = use_as_rvalue(ctx, f, p->vcall.target); 
					
					// call r/m64
					inst1(ctx, CALL_RM, &target_ptr);
				}
				
				// the return value
				if (dt.type == TB_VOID) {
					/* none */
				} else if (dt.count > 1 || TB_IS_FLOAT_TYPE(dt.type)) {
					Val xmm0 = val_xmm(dt, XMM0);
					def(ctx, f, xmm0, r);
				} else {
					Val rax = val_gpr(dt.type, RAX);
					def(ctx, f, rax, r);
				}
				break;
			}
			case TB_INITIALIZE: {
				TB_Register addr = p->mem_op.dst;
				
				TB_Module* m = f->module;
				TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_Initializer* i = (TB_Initializer*)&m->initializers[p->init.id / per_thread_stride][p->init.id % per_thread_stride];
				
				// rep stosb, ol' reliable
				evict_gpr(ctx, f, RAX, r);
				evict_gpr(ctx, f, RCX, r);
				evict_gpr(ctx, f, RDI, r);
				
				{
					Val param = use_as_rvalue(ctx, f, addr);
					
					if (!is_value_gpr(&param, RDI)) {
						Val dst = val_gpr(TB_PTR, RDI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					
					temporary_reserve_gpr(ctx, f, RDI);
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
				break;
			}
			case TB_MEMSET: {
				TB_Register dst_reg = p->mem_op.dst;
				TB_Register val_reg = p->mem_op.src;
				TB_Register size_reg = p->mem_op.size;
				//int align = f->nodes.payload[i].mem_op.align;
				
				// TODO(NeGate): Implement vector memset
				
				// rep stosb, ol' reliable
				evict_gpr(ctx, f, RAX, r);
				evict_gpr(ctx, f, RCX, r);
				evict_gpr(ctx, f, RDI, r);
				
				{
					Val param = use_as_rvalue(ctx, f, dst_reg);
					
					if (!is_value_gpr(&param, RDI)) {
						Val dst = val_gpr(TB_PTR, RDI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					
					temporary_reserve_gpr(ctx, f, RDI);
				}
				
				{
					Val param = use_as_rvalue(ctx, f, val_reg);
					
					if (!is_value_gpr(&param, RAX)) {
						Val dst = val_gpr(TB_PTR, RAX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					temporary_reserve_gpr(ctx, f, RAX);
				}
				
				{
					Val param = use_as_rvalue(ctx, f, size_reg);
					
					if (!is_value_gpr(&param, RCX)) {
						Val dst = val_gpr(TB_PTR, RCX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					temporary_reserve_gpr(ctx, f, RCX);
				}
				
				// rep stosb
				emit(0xF3);
				emit(0xAA);
				break;
			}
			case TB_MEMCPY: {
				TB_Register dst_reg = p->mem_op.dst;
				TB_Register src_reg = p->mem_op.src;
				TB_Register size_reg = p->mem_op.size;
				
				// TODO(NeGate): Implement vector memcpy
				// rep movsb, ol' reliable
				evict_gpr(ctx, f, RCX, r);
				evict_gpr(ctx, f, RSI, r);
				evict_gpr(ctx, f, RDI, r);
				
				{
					Val param = use_as_rvalue(ctx, f, dst_reg);
					
					if (!is_value_gpr(&param, RDI)) {
						Val dst = val_gpr(TB_PTR, RDI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					
					temporary_reserve_gpr(ctx, f, RDI);
				}
				
				{
					Val param = use_as_rvalue(ctx, f, src_reg);
					
					if (!is_value_gpr(&param, RSI)) {
						Val dst = val_gpr(TB_PTR, RSI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					temporary_reserve_gpr(ctx, f, RSI);
				}
				
				{
					Val param = use_as_rvalue(ctx, f, size_reg);
					
					if (!is_value_gpr(&param, RCX)) {
						Val dst = val_gpr(TB_PTR, RCX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					temporary_reserve_gpr(ctx, f, RCX);
				}
				
				// rep movsb
				emit(0xF3);
				emit(0xA4);
				break;
			}
			default:
			break;
		}
		
		// clear all temporaries
		ctx->gpr_temp_bits = 0;
		ctx->xmm_temp_bits = 0;
	}
}

static Val use(Ctx* ctx, TB_Function* f, TB_Register r) {
	if (ctx->addr_desc[r].type) {
		return ctx->addr_desc[r];
	}
	
	TB_RegTypeEnum reg_type = f->nodes.type[r];
	TB_DataType dt = f->nodes.dt[r];
	TB_RegPayload* restrict p = &f->nodes.payload[r];
	
	switch (reg_type) {
		case TB_INT_CONST: {
			int32_t imm32 = (int32_t)p->i_const;
			if (p->i_const == imm32) {
				Val v = val_imm(dt, imm32); 
				def(ctx, f, v, r);
				return v;
			}
			
			// explicit mov
			Val v = def_new_gpr(ctx, f, r, dt.type);
			
			// mov reg64, imm64
			emit(rex(true, 0x0, v.gpr, 0));
			emit(0xB8 + (v.gpr & 0b111));
			emit8(p->i_const);
			return v;
		}
		case TB_FLOAT_CONST: {
			// Unlike integers, there's no float immediates
			Val v = def_new_xmm(ctx, f, r, dt);
			
			XMM dst_xmm = v.xmm;
			
			assert((dt.type == TB_F32 || dt.type == TB_F64) && dt.count == 1);
			uint64_t imm = (Cvt_F64U64){ .f = p->f_const }.i;
			
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
					uint32_t imm32 = (Cvt_F32U32){ .f = p->f_const }.i;
					
					uint32_t* rdata_payload = tb_platform_arena_alloc(sizeof(uint32_t));
					*rdata_payload = imm32;
					
					disp = tb_emit_const_patch(f->module, ctx->function_id, code_pos(), rdata_payload, sizeof(uint32_t), s_local_thread_id);
				}
				emit4(disp);
			}
			
			return v;
		}
		case TB_STRING_CONST: {
			const char* str = f->nodes.payload[r].str_const.data;
			size_t len = f->nodes.payload[r].str_const.len;
			
			Val v = def_new_gpr(ctx, f, r, TB_PTR);
			
			emit(rex(true, v.gpr, RBP, 0));
			emit(0x8D);
			emit(mod_rx_rm(MOD_INDIRECT, v.gpr, RBP));
			
			uint32_t disp = tb_emit_const_patch(f->module, ctx->function_id, code_pos(), str, len, s_local_thread_id);
			emit4(disp);
			return v;
		}
		case TB_ARRAY_ACCESS: {
			Val base = use_as_address(ctx, f, p->array_access.base);
			Val index = use_as_rvalue(ctx, f, p->array_access.index);
			
			uint32_t stride = p->array_access.stride;
			int32_t disp = base.mem.disp;
			
			if (is_value_mem(&index)) {
				Val new_v = def_new_gpr(ctx, f, p->array_access.index, TB_I64);
				inst2(ctx, MOV, &new_v, &index, TB_I64);
				
				index = new_v;
			}
			
			if (index.type == VAL_IMM) {
				base.mem.disp += index.imm * stride;
				
				def(ctx, f, base, r);
				return base;
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
						Val v = def_new_gpr(ctx, f, r, dt.type);
						inst2(ctx, LEA, &v, &base, TB_PTR);
						
						if (scale) {
							Val addr = val_base_index(dt, v.gpr, index.gpr, scale);
							inst2(ctx, LEA, &v, &addr, TB_PTR);
						}
						else {
							Val idx = val_gpr(TB_PTR, index.gpr);
							inst2(ctx, ADD, &v, &idx, TB_PTR);
						}
						
						v = val_base_disp(dt, v.gpr, 0);
						def(ctx, f, v, r);
						return v;
					}
					else {
						Val v = val_base_index_disp(dt, base_reg, index.gpr, scale, disp);
						def(ctx, f, v, r);
						return v;
					}
				} else {
					// Resolve the index then add the base
					// TODO(NeGate): Improve this scaling method by
					// adding ways to break down multiplies into bitshifts.
					// int shifts_needed = __builtin_popcount(stride);
					Val v = def_new_gpr(ctx, f, r, dt.type);
					
					emit(rex(true, v.gpr, index.gpr, 0));
					emit(0x69);
					emit(mod_rx_rm(MOD_DIRECT, v.gpr, index.gpr));
					emit4(stride);
					
					emit(rex(true, base.gpr, v.gpr, 0));
					emit(0x01);
					emit(mod_rx_rm(MOD_DIRECT, base_reg, v.gpr));
					
					if (base.mem.index != GPR_NONE) {
						// accumulate the index to the result
						base.mem.base = v.gpr;
						
						inst2(ctx, LEA, &v, &base, TB_PTR);
					}
					
					def(ctx, f, v, r);
					return v;
				}
			} else tb_todo();
		}
		case TB_MEMBER_ACCESS: {
			Val v = use_as_address(ctx, f, p->member_access.base);
			
			v.mem.disp += p->member_access.offset;
			def(ctx, f, v, r);
			return v;
		}
		case TB_GLOBAL_ADDRESS: {
			return val_global(p->global_addr);
		}
		case TB_FUNC_ADDRESS: {
			int source_func = f - f->module->functions.data;
			
			Val v = def_new_gpr(ctx, f, r, TB_PTR);
			
			emit(rex(true, v.gpr, RBP, 0));
			emit(0x8D);
			emit(mod_rx_rm(MOD_INDIRECT, v.gpr, RBP));
			emit4(0x0);
			
			int target_func = p->func_addr - f->module->functions.data;
			tb_emit_call_patch(f->module,
							   source_func,
							   target_func,
							   code_pos() - 4,
							   s_local_thread_id);
			
			return v;
		}
		case TB_EFUNC_ADDRESS: {
			int source_func = f - f->module->functions.data;
			
			Val v = def_new_gpr(ctx, f, r, TB_PTR);
			
			emit(rex(true, v.gpr, RBP, 0));
			emit(0x8D);
			emit(mod_rx_rm(MOD_INDIRECT, v.gpr, RBP));
			emit4(0x0);
			
			tb_emit_ecall_patch(f->module,
								source_func,
								p->efunc_addr,
								code_pos() - 4,
								s_local_thread_id);
			
			return v;
		}
		case TB_TRUNCATE: {
			if (dt.type == TB_F32) {
				Val src = use_as_rvalue(ctx, f, p->ext);
				assert(src.dt.type == TB_F64);
				
				Val v = def_new_xmm(ctx, f, r, dt);
				inst2(ctx, CVTSD2SS, &v, &src, dt.type);
				return v;
			} else {
				Val src = use_as_rvalue(ctx, f, p->ext);
				if (src.type == VAL_IMM) {
					src.dt = dt;
					def(ctx, f, src, r);
					return src;
				}
				
				// TODO(NeGate): Implement recycle
				if (dt.type == TB_BOOL) dt.type = TB_I8;
				Val v = def_new_gpr(ctx, f, r, dt.type);
				
				if (dt.type == TB_PTR || dt.type == TB_I64 || dt.type == TB_I32) {
					// 32bit operations automatically truncate
					inst2(ctx, MOV, &v, &src, dt.type);
				} else if (dt.type == TB_I16) {
					inst2(ctx, MOVZXW, &v, &src, dt.type);
				} else if (dt.type == TB_I8 || dt.type == TB_BOOL) {
					inst2(ctx, MOVZXB, &v, &src, dt.type);
				} else {
					tb_unreachable();
				}
				
				return v;
			}
		}
		case TB_ZERO_EXT: {
			Val src = use_as_rvalue(ctx, f, p->ext);
			
			if (src.dt.type == TB_I32 &&
				dt.type == TB_I64 &&
				can_recycle_into(ctx, f, p->ext, r)) {
				assert(src.dt.count == 1);
				assert(dt.count == 1);
				
				if (!is_value_mem(&src)) {
					src.dt.type = TB_I64;
					
					kill(ctx, f, p->ext);
					def(ctx, f, src, r);
					return src;
				}
				else {
					// for 32->64 memory, it needs to actually
					// just do a 32bit load since it can't promote
					// the load itself up without "over-reading"
					Val v = def_new_gpr(ctx, f, r, TB_I64);
					inst2(ctx, MOV, &v, &src, TB_I32);
					return v;
				}
			} else if (src.dt.type == TB_I16) {
				Val v = def_new_gpr(ctx, f, r, dt.type);
				inst2(ctx, MOVZXW, &v, &src, dt.type);
				return v;
			} else if (src.dt.type == TB_I8 || src.dt.type == TB_BOOL) {
				Val v = def_new_gpr(ctx, f, r, dt.type);
				inst2(ctx, MOVZXB, &v, &src, dt.type);
				return v;
			} else {
				// TODO(NeGate): Implement recycle
				Val v = def_new_gpr(ctx, f, r, dt.type);
				inst2(ctx, MOV, &v, &src, dt.type);
				return src;
			}
		}
		case TB_FLOAT_EXT: {
			Val src = use_as_rvalue(ctx, f, p->ext);
			Val v = def_new_xmm(ctx, f, r, dt);
			
			inst2(ctx, CVTSS2SD, &v, &src, dt.type);
			return v;
		}
		case TB_SIGN_EXT: {
			Val src = use_as_rvalue(ctx, f, p->ext);
			if (src.type == VAL_IMM) {
				src.dt = dt;
				def(ctx, f, src, r);
				return src;
			}
			
			Val v;
			bool recycle = false;
			if (can_recycle_into(ctx, f, p->ext, r) && !is_value_mem(&src)) {
				v = src;
				recycle = true;
			} else {
				v = def_new_gpr(ctx, f, r, dt.type);
			}
			
			if (src.dt.type == TB_I64) {
				if (!recycle) inst2(ctx, MOV, &v, &src, dt.type);
			} else if (src.dt.type == TB_I32 && (dt.type == TB_I64 || dt.type == TB_PTR)) {
				inst2(ctx, MOVSXD, &v, &src, dt.type);
			} else if (src.dt.type == TB_I16) {
				inst2(ctx, MOVSXW, &v, &src, dt.type);
			} else if (src.dt.type == TB_I8 || src.dt.type == TB_BOOL) {
				inst2(ctx, MOVSXB, &v, &src, dt.type);
			} else {
				tb_unreachable();
			}
			return v;
		}
		case TB_INT2PTR: {
			Val src = use_as_rvalue(ctx, f, p->ext);
			if (src.type == VAL_IMM) {
				src.dt = TB_TYPE_PTR;
				def(ctx, f, src, r);
				return src;
			}
			
			if ((src.dt.type == TB_I32 ||
				 src.dt.type == TB_I64 ||
				 src.dt.type == TB_PTR) &&
				can_recycle_into(ctx, f, p->ext, r)) {
				assert(src.dt.count == 1);
				assert(dt.count == 1);
				
				if (!is_value_mem(&src)) {
					src.dt.type = TB_PTR;
					
					kill(ctx, f, p->ext);
					def(ctx, f, src, r);
					return src;
				}
				else {
					// for 32->64 memory, it needs to actually
					// just do a 32bit load since it can't promote
					// the load itself up without "over-reading"
					Val v = def_new_gpr(ctx, f, r, TB_I64);
					inst2(ctx, MOV, &v, &src, TB_I32);
					return v;
				}
			}
			else {
				bool recycle = false;
				
				Val v;
				if (can_recycle_into(ctx, f, p->ext, r)) {
					v = src;
					recycle = true;
				} else {
					v = def_new_gpr(ctx, f, r, dt.type);
				}
				
				if (src.dt.type == TB_I16) {
					inst2(ctx, MOVZXW, &v, &src, dt.type);
				} else if (src.dt.type == TB_I8 || src.dt.type == TB_BOOL) {
					inst2(ctx, MOVZXB, &v, &src, dt.type);
				} else {
					if (!recycle) inst2(ctx, MOV, &v, &src, dt.type);
				}
				return v;
			}
		}
		case TB_NEG: {
			Val src = use_as_rvalue(ctx, f, p->unary);
			
			if (dt.type == TB_F64) {
				// .LCPI0_0:
				//   .quad   0x8000000000000000
				//   .quad   0x8000000000000000
				// ...
				// xorps   xmm0, xmmword ptr [rip + .LCPI0_0]
				Val v = def_new_xmm(ctx, f, r, dt);
				XMM dst_xmm = v.xmm;
				
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
				
				return v;
			} else if (dt.type == TB_F32) {
				// .LCPI0_0:
				//   .long   0x80000000
				//   .long   0x80000000
				//   .long   0x80000000
				//   .long   0x80000000
				// ...
				// xorps   xmm0, xmmword ptr [rip + .LCPI0_0]
				Val v = def_new_xmm(ctx, f, r, dt);
				XMM dst_xmm = v.xmm;
				
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
				
				return v;
			} else {
				// TODO(NeGate): Implement recycle
				Val v = def_new_gpr(ctx, f, r, dt.type);
				inst2(ctx, MOV, &v, &src, TB_I64);
				inst1(ctx, NEG, &v);
				return v;
			}
		}
		case TB_NOT: {
			Val src = use_as_rvalue(ctx, f, p->unary);
			
			// TODO(NeGate): Implement recycle
			Val v = def_new_gpr(ctx, f, r, dt.type);
			inst2(ctx, MOV, &v, &src, TB_I64);
			inst1(ctx, NOT, &v);
			return v;
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
				
				{ ADDSD,  true,  true,   false,  true },
				{ SUBSD,  true,  true,   false,  true },
				{ MULSD,  true,  true,   false,  true },
				{ DIVSD,  true,  true,   false,  true },
				
				// 4-wide versions
				{ ADDPS,  true,  true,   false,  true },
				{ SUBPS,  true,  true,   false,  true },
				{ MULPS,  true,  true,   false,  true },
				{ DIVPS,  true,  true,   false,  true },
				
				// TODO(NeGate): This is incorrect but i
				// don't wanna implement
				{ ADDPS,  true,  true,   false,  true },
				{ SUBPS,  true,  true,   false,  true },
				{ MULPS,  true,  true,   false,  true },
				{ DIVPS,  true,  true,   false,  true },
			};
			
			// supported modes
			assert((dt.type == TB_F32 && dt.count == 1) ||
				   (dt.type == TB_F64 && dt.count == 1) ||
				   (dt.type == TB_F32 && dt.count == 4) ||
				   (dt.type == TB_F64 && dt.count == 2));
			
			// TODO(NeGate): Implement [f64 x 2]
			assert(dt.count != 2);
			const IselInfo* info = &tbl[(dt.count == 4 ? 8 : 0) + (reg_type - TB_FADD) + (dt.type == TB_F64 ? 4 : 0)];
			
			TB_Register a_reg = p->f_arith.a;
			TB_Register b_reg = p->f_arith.b;
			
			if (a_reg == b_reg) {
				Val src = use_as_rvalue(ctx, f, a_reg);
				
				return isel_aliased(ctx, f, info, r, a_reg, &src);
			} else {
				Val a = use_as_rvalue(ctx, f, a_reg);
				Val b = use_as_rvalue(ctx, f, b_reg);
				
				return isel(ctx, f, info, r, a_reg, b_reg, &a, &b);
			}
		}
		case TB_AND:
		case TB_OR:
		case TB_XOR:
		case TB_ADD:
		case TB_SUB:
		case TB_MUL: {
			const static IselInfo tbl[] = {
				// type               inst  commut  has_imm  mem_dst  mem_src   
				[TB_AND - TB_AND] = { AND,  true,   true,    true,    true },
				[TB_OR  - TB_AND] = { OR,   true,   true,    true,    true },
				[TB_XOR - TB_AND] = { XOR,  true,   true,    true,    true },
				[TB_ADD - TB_AND] = { ADD,  true,   true,    true,    true },
				[TB_SUB - TB_AND] = { SUB,  false,  true,    true,    true },
				[TB_MUL - TB_AND] = { IMUL, true,   false,   false,   true }
			};
			const IselInfo* info = &tbl[reg_type - TB_AND];
			
			TB_Register a_reg = p->i_arith.a;
			TB_Register b_reg = p->i_arith.b;
			
			if (a_reg == b_reg) {
				Val src = use_as_rvalue(ctx, f, a_reg);
				
				return isel_aliased(ctx, f, info, r, a_reg, &src);
			} else {
				Val a = use_as_rvalue(ctx, f, a_reg);
				Val b = use_as_rvalue(ctx, f, b_reg);
				
				if (dt.count == 1 &&
					reg_type == TB_ADD &&
					a.type == VAL_GPR &&
					b.type == VAL_GPR) {
					Val v = def_new_gpr(ctx, f, r, dt.type);
					
					// lea _0, [_1 + _2]
					// TODO(NeGate): Maybe allow for the 32bit version sometimes
					emit(rex(true, v.gpr, b.gpr, a.gpr));
					emit(0x8D);
					emit(mod_rx_rm(MOD_INDIRECT, v.gpr, RSP));
					emit(mod_rx_rm(SCALE_X1, (b.gpr & 7) == RSP ? RSP : b.gpr, a.gpr));
					return v;
				} else if (dt.count == 1 &&
						   reg_type == TB_ADD &&
						   a.type == VAL_GPR &&
						   b.type == VAL_IMM) {
					// lea _0, [_1 + _2]
					Val v = def_new_gpr(ctx, f, r, dt.type);
					Val arith = val_base_disp(dt, a.gpr, b.imm);
					
					inst2(ctx, LEA, &v, &arith, dt.type);
					return v;
				} else if (dt.count == 1 &&
						   reg_type == TB_SUB &&
						   a.type == VAL_GPR &&
						   b.type == VAL_IMM) {
					// lea _0, [_1 + _2]
					Val v = def_new_gpr(ctx, f, r, dt.type);
					Val arith = val_base_disp(dt, a.gpr, -b.imm);
					
					inst2(ctx, LEA, &v, &arith, dt.type);
					return v;
				} else {
					return isel(ctx, f, info, r, a_reg, b_reg, &a, &b);
				}
			}
			break;
		}
		case TB_SHL: {
			TB_Register a_reg = p->i_arith.a;
			TB_Register b_reg = p->i_arith.b;
			
			Val a = use_as_rvalue(ctx, f, a_reg);
			if (f->nodes.type[b_reg] == TB_INT_CONST) {
				assert(f->nodes.payload[b_reg].i_const < 64);
				
				Val v = def_new_gpr(ctx, f, r, dt.type);
				inst2(ctx, MOV, &v, &a, dt.type);
				
				GPR dst_gpr = v.gpr;
				bool is_64bit = dt.type == TB_I64 || dt.type == TB_PTR;
				
				if (dt.type == TB_I16) emit(0x66);
				emit(rex(is_64bit, 0x00, dst_gpr, 0x00));
				emit(dt.type == TB_I8 ? 0xC0 : 0xC1);
				emit(mod_rx_rm(MOD_DIRECT, 0x04, dst_gpr));
				emit(f->nodes.payload[b_reg].i_const);
				return v;
			}
			
			tb_todo();
		}
		case TB_UDIV:
		case TB_SDIV: {
			evict_gpr(ctx, f, RAX, r);
			evict_gpr(ctx, f, RDX, r);
			
			TB_Register a_reg = f->nodes.payload[r].i_arith.a;
			TB_Register b_reg = f->nodes.payload[r].i_arith.b;
			
			Val a = use_as_rvalue(ctx, f, a_reg);
			Val b = use_as_rvalue(ctx, f, b_reg);
			
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
				Val new_b = def_new_gpr(ctx, f, b_reg, dt.type);
				inst2(ctx, MOV, &new_b, &b, dt.type);
				b = new_b;
			}
			
			inst1(ctx, IDIV, &b);
			
			// the return value is in RAX
			def(ctx, f, rax, r);
			return rax;
		}
		case TB_CMP_EQ:
		case TB_CMP_NE:
		case TB_CMP_SLT:
		case TB_CMP_SLE:
		case TB_CMP_ULT:
		case TB_CMP_ULE: {
			TB_DataType cmp_dt = p->cmp.dt;
			
			Val a = use_as_rvalue(ctx, f, p->cmp.a);
			Val b = use_as_rvalue(ctx, f, p->cmp.b);
			
			bool invert = false;
			if (is_value_mem(&a) && is_value_mem(&b)) {
				Val dst = def_new_gpr(ctx, f, r, cmp_dt.type);
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
			Val v = val_flags(cc);
			
			def(ctx, f, v, r);
			return v;
		}
		default: break;
	}
	
	tb_function_print(f, stdout);
	printf("\n\n\n");
	abort();
}

static Val load_into(Ctx* ctx, TB_Function* f, TB_DataType dt, TB_Register r, TB_Register addr) {
	Val v = use(ctx, f, addr);
	
	// Desugar booleans into bytes
	if (v.dt.type == TB_BOOL) v.dt.type = TB_I8;
	
	if (v.mem.is_rvalue) {
		// deref
		Val new_v = def_new_gpr(ctx, f, r, TB_PTR);
		inst2(ctx, MOV, &new_v, &v, TB_PTR);
		
		v = val_base_disp(v.dt, new_v.gpr, 0);
	}
	
	v.dt = dt;
	v.mem.is_rvalue = true;
	return v;
}

static void store_into(Ctx* ctx, TB_Function* f, TB_DataType dt, const Val* dst, TB_Register r, TB_Register dst_reg, TB_Register val_reg) {
	// Desugar booleans into bytes
	if (dt.type == TB_BOOL) dt.type = TB_I8;
	
	int op = MOV;
	
	// Peephole folded operation:
	// OP dst, src
	if (f->nodes.type[val_reg] >= TB_AND && f->nodes.type[val_reg] <= TB_MUL) {
		TB_Register a = f->nodes.payload[val_reg].i_arith.a;
		TB_Register b = f->nodes.payload[val_reg].i_arith.b;
		
		if (ctx->addr_desc[a].type == VAL_NONE) {
			def(ctx, f, use(ctx, f, a), a);
		}
		
		if (ctx->addr_desc[b].type == VAL_NONE) {
			def(ctx, f, use(ctx, f, b), b);
		}
		
		{
			bool is_phi = f->nodes.type[r] == TB_PHI2;
			bool folded = false;
			if (is_phi) {
				folded = is_phi_that_contains(f, r, val_reg);
				folded |= (f->nodes.type[val_reg] == TB_MUL && dst->type == VAL_GPR);
			} else {
				folded = (f->nodes.type[a] == TB_LOAD && f->nodes.payload[a].load.address == dst_reg) &&
					(f->nodes.type[val_reg] != TB_MUL);
			}
			
			if (folded) {
				switch (f->nodes.type[val_reg]) {
					case TB_AND: op = AND; break;
					case TB_OR: op = OR; break;
					case TB_XOR: op = XOR; break;
					case TB_ADD: op = ADD; break;
					case TB_SUB: op = SUB; break;
					case TB_MUL: op = IMUL; break;
					default: tb_unreachable();
				}
				
				val_reg = b;
			}
		}
	}
	
	int real_mov = MOV;
	if (dt.count > 1) real_mov = MOVAPS;
	else if (dt.type == TB_F32) real_mov = MOVSS;
	else if (dt.type == TB_F64) real_mov = MOVSD;
	
	// Default case:
	// mov dst, src
	// or
	// mov temp, src
	// mov dst, temp
	Val value = use_as_rvalue(ctx, f, val_reg);
	
	// if they match and it's just a MOV, don't do it
	if (op == MOV) {
		op = real_mov;
		
		if (is_value_match(dst, &value)) return;
	}
	
	if (!is_value_mem(&value)) {
		inst2(ctx, op, dst, &value, dt.type);
	} else {
		Val temp = def_new_gpr(ctx, f, r, dt.type);
		
		inst2(ctx, real_mov, &temp, &value, dt.type);
		inst2(ctx, op, dst, &temp, dt.type);
		
		kill(ctx, f, r);
	}
}

static void kill(Ctx* ctx, TB_Function* f, TB_Register r) {
	DEBUG_LOG("   kill r%u\n", r);
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
	DEBUG_LOG("   def r%u (should die at r%u)\n", r, ctx->intervals[r]);
}

static Val def_new_gpr(Ctx* ctx, TB_Function* f, TB_Register r, int dt_type) {
	uint16_t gpr_temp_bits = ctx->gpr_temp_bits;
	
	do {
		for (size_t i = 0; i < tb_arrlen(GPR_PRIORITY_LIST); i++) {
			GPR gpr = GPR_PRIORITY_LIST[i];
			
			if (ctx->gpr_desc[gpr] == 0 && (gpr_temp_bits & (1u << gpr)) == 0) {
				// mark register as to be saved
				ctx->regs_to_save |= (1u << gpr) & ABI_CALLEE_SAVED;
				
				Val val = val_gpr(dt_type, gpr);
				ctx->gpr_desc[gpr] = r;
				
				if (r && r != TB_REG_MAX) {
					ctx->addr_desc[r] = val;
				}
				
				DEBUG_LOG("   def gpr r%u\n", r);
				return val;
			}
		}
		
		// If it fails just try GCing it and hopefully space was made.
	} while (garbage_collect_gpr(ctx, f, r));
	
	// TODO(NeGate): Try figuring out which register is best to evict.
	for (size_t i = tb_arrlen(GPR_PRIORITY_LIST); i--;) {
		GPR gpr = GPR_PRIORITY_LIST[i];
		
		if (evict_gpr(ctx, f, gpr, r)) {
			ctx->gpr_desc[gpr] = r;
			Val val = val_gpr(dt_type, gpr);
			
			if (r && r != TB_REG_MAX) {
				ctx->addr_desc[r] = val;
			}
			
			return val;
		}
	}
	
	abort();
}

static Val def_new_xmm(Ctx* ctx, TB_Function* f, TB_Register r, TB_DataType dt) {
	uint16_t xmm_temp_bits = ctx->xmm_temp_bits;
	
	do {
		for (size_t i = 0; i < 16; i++) {
			if (ctx->xmm_desc[i] == 0 && (xmm_temp_bits & (1u << i)) == 0) {
				// mark register as to be saved
				// ctx->regs_to_save |= (1u << (i + 32)) & ABI_CALLEE_SAVED;
				ctx->xmm_desc[i] = r;
				
				Val val = val_xmm(dt, i);
				if (r && r != TB_REG_MAX) ctx->addr_desc[r] = val;
				
				DEBUG_LOG("   def xmm r%u\n", r);
				return val;
			}
		}
		
		// If it fails just try GCing it and hopefully space was made.
	} while (garbage_collect_xmm(ctx, f, r));
	
	// TODO(NeGate): Try figuring out which register is best to evict.
	for (size_t i = 16; i--;) {
		if (evict_xmm(ctx, f, i, r)) {
			ctx->gpr_desc[i] = r;
			Val val = val_xmm(dt, i);
			
			if (r && r != TB_REG_MAX) {
				ctx->addr_desc[r] = val;
			}
			
			return val;
		}
	}
	
	abort();
}

static bool evict_gpr(Ctx* ctx, TB_Function* f, GPR g, TB_Register r) {
	TB_Register bound = ctx->gpr_desc[g];
	
	// if it dies we don't need to spill it
	TB_Register bb = ctx->current_bb;
	if (bound != TB_REG_MAX &&
		bound > bb &&
		ctx->intervals[bound] < r &&
		f->nodes.type[bound] != TB_PARAM &&
		f->nodes.type[bound] != TB_LOAD) {
		DEBUG_LOG("   collected r%u\n", bound);
		ctx->gpr_desc[g] = 0;
		ctx->addr_desc[bound] = (Val){ 0 };
		return true;
	}
	
	if (bound && bound != TB_REG_MAX) {
		DEBUG_LOG("   evicted %s\n", GPR_NAMES[g]);
		
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
	
	// if it dies we don't need to spill it
	TB_Register bb = ctx->current_bb;
	if (bound != TB_REG_MAX &&
		bound > bb &&
		ctx->intervals[bound] < r &&
		f->nodes.type[bound] != TB_PARAM &&
		f->nodes.type[bound] != TB_LOAD) {
		DEBUG_LOG("   collected r%u\n", bound);
		ctx->xmm_desc[x] = 0;
		ctx->addr_desc[bound] = (Val){ 0 };
		return true;
	}
	
	if (bound && bound != TB_REG_MAX) {
		DEBUG_LOG("   evicted XMM%d\n", x);
		
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
			ctx->stack_usage = align_up(ctx->stack_usage + 4, 4);
			dst = val_stack(f->nodes.dt[bound], -ctx->stack_usage);
			
			Val src = val_xmm(dt, x);
			inst2(ctx, MOVSS, &dst, &src, TB_F32);
		} else if (dt.type == TB_F64) {
			ctx->stack_usage = align_up(ctx->stack_usage + 8, 8);
			dst = val_stack(f->nodes.dt[bound], -ctx->stack_usage);
			
			Val src = val_xmm(dt, x);
			inst2(ctx, MOVSD, &dst, &src, TB_F64);
		} else tb_todo();
		
		ctx->addr_desc[bound] = dst;
		return true;
	}
	
	return false;
}

static void temporary_reserve_gpr(Ctx* ctx, TB_Function* f, GPR g) {
	ctx->gpr_temp_bits |= (1u << g);
}

static void temporary_reserve_xmm(Ctx* ctx, TB_Function* f, XMM x) {
	ctx->xmm_temp_bits |= (1u << x);
}

static Val materialize(Ctx* ctx, TB_Function* f, const Val* src, TB_Register src_reg, TB_DataType dt) {
	if (dt.count > 1) {
		Val v = def_new_xmm(ctx, f, src_reg, dt);
		inst2(ctx, MOVAPS, &v, src, dt.type);
		return v;
	} else if (dt.type == TB_F32) {
		Val v = def_new_xmm(ctx, f, src_reg, dt);
		inst2(ctx, MOVSS, &v, src, dt.type);
		return v;
	} else if (dt.type == TB_F64) {
		Val v = def_new_xmm(ctx, f, src_reg, dt);
		inst2(ctx, MOVSD, &v, src, dt.type);
		return v;
	} else if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
		Val v = def_new_gpr(ctx, f, src_reg, dt.type);
		inst2(ctx, MOV, &v, src, dt.type);
		return v;
	} else {
		tb_todo();
	}
}

static Val isel(Ctx* ctx, TB_Function* f, const IselInfo* info,
				TB_Register dst_reg, TB_Register a_reg, TB_Register b_reg, 
				const Val* a, const Val* b) {
	// use isel_aliased instead
	assert(a != b);
	
	TB_DataType dst_dt = f->nodes.dt[dst_reg];
	bool is_xmm = dst_dt.count > 1 || TB_IS_FLOAT_TYPE(dst_dt.type);
	
	// (i8 | i16) => i32
	TB_DataType prev_dt;
	int mov_extend = MOVZXW;
	bool is_promoted = dst_dt.count == 1 && (dst_dt.type == TB_I8 || dst_dt.type == TB_I16);
	if (is_promoted) {
		prev_dt = dst_dt;
		dst_dt = TB_TYPE_I32;
		
		if (dst_dt.type == TB_BOOL || dst_dt.type == TB_I8) mov_extend = MOVZXB;
	}
	
	// NOTE(NeGate): The operands are ordered for this "magic"
	if (info->communitive && a->type < b->type) tb_swap(a, b);
	
	// If both source operands are memory addresses, promote one into a register
	Val av;
	if (is_value_mem(a) && is_value_mem(b)) {
		av = materialize(ctx, f, a, a_reg, dst_dt);
	} else {
		av = *a;
	}
	
	// Some instructions don't have an immediate form
	Val bv;
	if (!info->has_immediates && b->type == VAL_IMM) {
		assert(TB_IS_INTEGER_TYPE(dst_dt.type) || dst_dt.type == TB_PTR);
		
		// special case of materialize where it can only ever 
		// be an integral type
		bv = def_new_gpr(ctx, f, b_reg, dst_dt.type);
		inst2(ctx, MOV, &bv, b, dst_dt.type);
	} else {
		bv = *b;
	}
	
	bool recycle = a->type != VAL_IMM &&
		can_recycle_into(ctx, f, a_reg, dst_reg);
	
	// Can't recycle memory if the operation can't store directly into it
	if (!info->has_memory_dst && is_value_mem(a)) recycle = false;
	
	if (recycle) {
		// (a => dst) += b
		kill(ctx, f, a_reg);
		inst2(ctx, info->inst, &av, &bv, dst_dt.type);
		def(ctx, f, av, dst_reg);
		
		if (is_promoted) {
			assert(!is_value_mem(&av));
			inst2(ctx, mov_extend, &av, &av, prev_dt.type);
		}
		return av;
	} else {
		// dst = a, dst += b;
		Val v;
		if (is_xmm) v = def_new_xmm(ctx, f, dst_reg, dst_dt);
		else v = def_new_gpr(ctx, f, dst_reg, dst_dt.type);
		
		int mov_type = MOV;
		if (dst_dt.count > 1) mov_type = MOVAPS;
		else if (dst_dt.type == TB_F32) mov_type = MOVSS;
		else if (dst_dt.type == TB_F64) mov_type = MOVSD;
		
		inst2(ctx, mov_type, &v, &av, dst_dt.type);
		inst2(ctx, info->inst, &v, &bv, dst_dt.type);
		
		if (is_promoted) inst2(ctx, mov_extend, &v, &v, prev_dt.type);
		return v;
	}
}

static Val isel_aliased(Ctx* ctx, TB_Function* f, const IselInfo* info,
						TB_Register dst_reg, TB_Register src_reg, 
						const Val* src) {
	TB_DataType dst_dt = f->nodes.dt[dst_reg];
	
	// (i8 | i16) => i32
	TB_DataType prev_dt;
	int mov_extend = MOVZXW;
	bool is_promoted = dst_dt.count == 1 && (dst_dt.type == TB_BOOL || dst_dt.type == TB_I8 || dst_dt.type == TB_I16);
	if (is_promoted) {
		prev_dt = dst_dt;
		dst_dt = TB_TYPE_I32;
		
		if (dst_dt.type == TB_BOOL || dst_dt.type == TB_I8) mov_extend = MOVZXB;
	}
	
	bool recycle = src->type != VAL_IMM && can_recycle_into(ctx, f, src_reg, dst_reg);
	
	// Can't recycle memory if the operation can't store directly into it
	if (!info->has_memory_dst && is_value_mem(src)) recycle = false;
	
	if (recycle) {
		// src += src
		kill(ctx, f, src_reg);
		inst2(ctx, info->inst, src, src, dst_dt.type);
		def(ctx, f, *src, dst_reg);
		
		if (is_promoted) inst2(ctx, mov_extend, src, src, prev_dt.type);
		return *src;
	} else {
		// dst = src, dst += src;
		Val v = def_new_gpr(ctx, f, dst_reg, dst_dt.type);
		
		int mov_type = MOV;
		if (dst_dt.count > 1) mov_type = MOVAPS;
		else if (dst_dt.type == TB_F32) mov_type = MOVSS;
		else if (dst_dt.type == TB_F64) mov_type = MOVSD;
		
		inst2(ctx, mov_type, &v, src, dst_dt.type);
		
		if (info->inst == IMUL && src->type == VAL_IMM) {
			// imul reg/mem64, imm32 isn't a thing, it's a bit fancier than
			// that really, so we'll just materialize a temporary
			Val new_src = def_new_gpr(ctx, f, 0, TB_I32);
			inst2(ctx, MOV, &new_src, src, TB_I32);
			inst2(ctx, info->inst, &v, &new_src, dst_dt.type);
		} else {
			inst2(ctx, info->inst, &v, src, dst_dt.type);
		}
		
		if (is_promoted) inst2(ctx, mov_extend, &v, &v, prev_dt.type);
		return v;
	}
}

static bool garbage_collect_gpr(Ctx* ctx, TB_Function* f, TB_Register end) {
	TB_Register bb = ctx->current_bb;
	TB_Register bb_end = ctx->current_bb_end;
	
	int changes = 0;
	for (size_t i = 0; i < 16; i++) {
		TB_Register r = ctx->gpr_desc[i];
		if (r <= bb || r > bb_end) continue;
		if (f->nodes.type[r] == TB_PHI2) continue;
		if (f->nodes.type[r] == TB_LOAD) continue;
		if (f->nodes.type[r] == TB_PARAM) continue;
		if (f->nodes.type[r] == TB_PARAM_ADDR) continue;
		
		if (ctx->intervals[r] < end) {
			DEBUG_LOG("   gc kill r%u\n", r);
			ctx->gpr_desc[i] = 0;
			ctx->addr_desc[r] = (Val) { 0 };
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
		if (f->nodes.type[r] == TB_PHI2) continue;
		if (f->nodes.type[r] == TB_LOAD) continue;
		if (f->nodes.type[r] == TB_PARAM) continue;
		if (f->nodes.type[r] == TB_PARAM_ADDR) continue;
		
		if (ctx->intervals[r] < end) {
			DEBUG_LOG("   gc kill r%u\n", r);
			ctx->xmm_desc[i] = 0;
			ctx->addr_desc[r] = (Val) { 0 };
			changes++;
		}
	}
	
	return changes;
}

static bool is_temporary_of_bb(Ctx* ctx, TB_Function* f, GPR gpr, TB_Register bb, TB_Register bb_end) {
	TB_Register bound = ctx->gpr_desc[gpr];
	
	return (bound >= bb &&
			bound <= bb_end &&
			f->nodes.type[bound] != TB_PHI2);
}

static bool can_recycle_into(Ctx* ctx, TB_Function* f, TB_Register from, TB_Register to) {
	if (f->nodes.type[from] == TB_LOAD || f->nodes.type[from] == TB_PARAM) {
		return false;
	}
	
	return ctx->use_count[from] == 1 && 
		ctx->intervals[from] == to;
}

static bool is_local_of_bb(Ctx* ctx, TB_Function* f, TB_Register bound, TB_Register bb, TB_Register bb_end) {
	return bound >= bb && bound <= bb_end;
}

static Val use_as_rvalue(Ctx* ctx, TB_Function* f, TB_Register r) {
	Val temp;
	if (f->nodes.type[r] >= TB_CMP_EQ && f->nodes.type[r] <= TB_CMP_FLE) {
		// when lowering boolean FLAGS into rvalues we need to keep a little GPR
		// temporary that's going to be used to store this new integer variant of
		// the boolean.
		temp = def_new_gpr(ctx, f, 0, TB_I64);
		
		// xor temp, temp
		if (temp.gpr >= 8) emit(rex(true, temp.gpr, temp.gpr, 0));
		emit(0x31);
		emit(mod_rx_rm(MOD_DIRECT, temp.gpr, temp.gpr));
	}
	
	Val v = use(ctx, f, r);
	if (v.dt.type == TB_BOOL) v.dt.type = TB_I8;
	
	if (is_value_mem(&v)) {
		if (is_address_node(f->nodes.type[r])) {
			assert(!v.mem.is_rvalue);
			
			Val new_v = def_new_gpr(ctx, f, X64_TEMP_REG, v.dt.type);
			inst2(ctx, LEA, &new_v, &v, v.dt.type);
			
			return new_v;
		} else if (!v.mem.is_rvalue) {
			if (v.dt.count > 1) {
				Val new_v = def_new_xmm(ctx, f, r, v.dt);
				inst2(ctx, MOVAPS, &new_v, &v, TB_F32);
				return new_v;
			} else if (v.dt.type == TB_F32) {
				Val new_v = def_new_xmm(ctx, f, r, v.dt);
				inst2(ctx, MOVSS, &new_v, &v, TB_F32);
				return new_v;
			} else if (v.dt.type == TB_F64) {
				Val new_v = def_new_xmm(ctx, f, r, v.dt);
				inst2(ctx, MOVSD, &new_v, &v, TB_F64);
				return new_v;
			} else {
				Val new_v = def_new_gpr(ctx, f, r, v.dt.type);
				inst2(ctx, MOV, &new_v, &v, v.dt.type);
				return new_v;
			}
		}
	} else if (v.type == VAL_FLAGS) {
		assert(f->nodes.type[r] >= TB_CMP_EQ && f->nodes.type[r] <= TB_CMP_FLE);
		Cond cc = v.cond;
		
		// setcc v
		if (temp.gpr >= 8) emit(rex(true, temp.gpr, temp.gpr, 0));
		emit(0x0F);
		emit(0x90 + cc);
		emit(mod_rx_rm(MOD_DIRECT, temp.gpr, temp.gpr));
		
		def(ctx, f, temp, r);
		return temp;
	}
	
	return v;
}

static Val use_as_address(Ctx* ctx, TB_Function* f, TB_Register r) {
	Val v = use(ctx, f, r);
	
	if (is_value_mem(&v)) {
		if (v.mem.is_rvalue) {
			Val new_val = def_new_gpr(ctx, f, r, TB_PTR);
			inst2(ctx, MOV, &new_val, &v, TB_PTR);
			
			return val_base_disp(TB_TYPE_PTR, new_val.gpr, 0);
		}
	}
	else if (v.type == VAL_GPR) {
		return val_base_disp(TB_TYPE_PTR, v.gpr, 0);
	}
	else if (v.type == VAL_IMM) {
		Val new_val = def_new_gpr(ctx, f, r, TB_PTR);
		
		// TODO(NeGate): implement a shared "load immediate into register" function
		// mov reg64, imm64
		emit(rex(true, 0x0, new_val.gpr, 0));
		emit(0xB8 + (new_val.gpr & 0b111));
		emit8(v.imm);
		
		return val_base_disp(TB_TYPE_PTR, new_val.gpr, 0);
	}
	else tb_todo();
	
	return v;
}

// Is this a phi node? does it can the register `reg`?
static bool is_phi_that_contains(TB_Function* f, TB_Register phi, TB_Register reg) {
	if (f->nodes.type[phi] == TB_PHI2) {
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
	loop(i, m->max_threads) {
		dyn_array(TB_FunctionPatch) patches = m->call_patches[i];
		
		loop(j, arrlen(patches)) {
			TB_FunctionPatch* p = &patches[j];
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
