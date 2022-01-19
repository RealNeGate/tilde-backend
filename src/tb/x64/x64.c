// This entire module is one translation unit so that it doesn't have to worry
// about C's crappy support for public and private interfaces.
//
// my model for x64 code generation is just building small expression trees from
// the DAG that is my IR, essentially when a function requires a hard value it
// will generate and evaluate a tree because then i can perform more optimizations
// over it.
#include "x64.h"
#include "inst.h"
#include "proepi.h"
#include "tree.h"

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
static void eval_basic_block(Ctx* restrict ctx, TB_Function* f, TB_Register bb, TB_Register bb_end);
static void store_into(Ctx* restrict ctx, TB_Function* f, TB_DataType dt, const Val* dst, TB_Register r, TB_Register dst_reg, TB_Register val_reg);

// Just handles the PHI nodes that we'll encounter when leaving `from` into `to`
static void eval_terminator_phis(Ctx* ctx, TB_Function* f, TB_Register from, TB_Register from_terminator, TB_Register to, TB_Register to_terminator);

typedef struct FunctionTally {
	size_t memory_usage;
	
	size_t phi_count;
	size_t tree_count;
	size_t locals_count;
	size_t return_count;
	size_t label_patch_count;
} FunctionTally;

static FunctionTally tally_memory_usage(TB_Function* restrict f) {
	size_t phi_count = 0;
	size_t locals_count = 0;
	size_t return_count = 0;
	size_t label_patch_count = 0;
	
#if TB_HOST_ARCH == TB_HOST_X86_64
#define COUNT_OF_TYPE_IN_M128(t) __builtin_popcount(_mm_movemask_epi8(_mm_cmpeq_epi8(bytes, _mm_set1_epi8(t))))
	
	// the node types are aligned to a cache line so we could in theory
	// grab up to 64bytes aligned without UB
	for (size_t i = 0; i < f->nodes.count; i += 16) {
		__m128i bytes = _mm_load_si128((__m128i*)&f->nodes.type[i]);
		
		phi_count += COUNT_OF_TYPE_IN_M128(TB_PHI2);
		locals_count += COUNT_OF_TYPE_IN_M128(TB_LOCAL);
		
		return_count += COUNT_OF_TYPE_IN_M128(TB_RET);
		
		label_patch_count += COUNT_OF_TYPE_IN_M128(TB_GOTO);
		label_patch_count += COUNT_OF_TYPE_IN_M128(TB_IF) * 2;
	}
#undef COUNT_OF_TYPE_IN_M128
#else
	for (size_t i = 1; i < f->nodes.count; i++) {
		TB_RegType t = f->nodes[i].type;
		
		if (t == TB_PHI2) phi_count++;
		else if (t == TB_RET) return_count++;
		else if (t == TB_LOCAL) locals_count++;
		else if (t == TB_IF) label_patch_count += 2;
		else if (t == TB_GOTO) label_patch_count++;
	}
#endif
	
	size_t tree_count = f->nodes.count;
	if (tree_count < 2048) tree_count = 2048;
	//printf("%s: max tree nodes = %zu\n", f->name, tree_count);
	
	// parameters are locals too... ish
	locals_count += f->prototype->param_count;
	
	size_t align_mask = _Alignof(max_align_t)-1;
	size_t tally = 0;
	
	// context
	tally += sizeof(Ctx) + (tree_count * sizeof(TreeNode));
	tally = (tally + align_mask) & ~align_mask;
	
	// use_count
	tally += f->nodes.count * sizeof(TB_Register);
	tally = (tally + align_mask) & ~align_mask;
	
	// intervals
	tally += f->nodes.count * sizeof(TB_Register);
	tally = (tally + align_mask) & ~align_mask;
	
	// phis
	tally += phi_count * sizeof(PhiValue);
	tally = (tally + align_mask) & ~align_mask;
	
	// phi_queue
	tally += phi_count * sizeof(TB_Register);
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
	
	return (FunctionTally){
		.memory_usage = tally,
		
		.phi_count = phi_count,
		.tree_count = tree_count,
		.locals_count = locals_count,
		.return_count = return_count,
		.label_patch_count = label_patch_count
	};
}

TB_FunctionOutput x64_compile_function(TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id) {
	s_local_thread_id = local_thread_id;
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	////////////////////////////////
	// Allocate all the memory we'll need
	////////////////////////////////
	bool is_ctx_heap_allocated = false;
	Ctx* restrict ctx = NULL;
	
	{
		// if we can't fit our memory usage into memory, we fallback
		FunctionTally tally = tally_memory_usage(f);
		is_ctx_heap_allocated = !tb_tls_can_fit(tls, tally.memory_usage);
		
		size_t ctx_size = sizeof(Ctx) + (tally.tree_count * sizeof(TreeNode));
		if (is_ctx_heap_allocated) {
			//printf("Could not allocate x64 code gen context: using heap fallback. (%zu bytes)\n", tally.memory_usage);
			
			ctx = calloc(1, ctx_size);
			
			ctx->use_count = malloc(f->nodes.count * sizeof(TB_Register));
			ctx->intervals = malloc(f->nodes.count * sizeof(TB_Register));
			ctx->phis = malloc(tally.phi_count * sizeof(PhiValue));
			
			ctx->phi_queue = malloc(tally.phi_count * sizeof(TB_Register));
			
			ctx->labels = malloc(f->label_count * sizeof(uint32_t));
			ctx->label_patches = malloc(tally.label_patch_count * sizeof(LabelPatch));
			ctx->ret_patches = malloc(tally.return_count * sizeof(ReturnPatch));
		} else {
			ctx = tb_tls_push(tls, ctx_size);
			
			// we only care about clearing out the actual context
			// data, not the tree nodes
			memset(ctx, 0, sizeof(Ctx));
			
			ctx->use_count = tb_tls_push(tls, f->nodes.count * sizeof(TB_Register));
			ctx->intervals = tb_tls_push(tls, f->nodes.count * sizeof(TB_Register));
			ctx->phis = tb_tls_push(tls, tally.phi_count * sizeof(PhiValue));
			
			ctx->phi_queue = tb_tls_push(tls, tally.phi_count * sizeof(TB_Register));
			
			ctx->labels = tb_tls_push(tls, f->label_count * sizeof(uint32_t));
			ctx->label_patches = tb_tls_push(tls, tally.label_patch_count * sizeof(LabelPatch));
			ctx->ret_patches = tb_tls_push(tls, tally.return_count * sizeof(ReturnPatch));
		}
		
		ctx->tree_cap = tally.tree_count;
		ctx->start_out = ctx->out = out;
		ctx->f = f;
		ctx->function_id = f - f->module->functions.data;
		ctx->last_fence = 1;
		
		ctx->is_sysv = (f->module->target_system == TB_SYSTEM_LINUX ||
						f->module->target_system == TB_SYSTEM_MACOS);
		
		arrsetcap(ctx->locals, tally.locals_count);
	}
	
	////////////////////////////////
	// Analyze function for stack, live intervals and phi nodes
	////////////////////////////////
	// calculate the maximum parameter usage for a call
	size_t caller_usage = 0;
	loop(i, f->nodes.count) {
		if (f->nodes.type[i] == TB_CALL ||
			f->nodes.type[i] == TB_ECALL ||
			f->nodes.type[i] == TB_VCALL) {
			int param_usage = CALL_NODE_PARAM_COUNT(f, i);
			if (caller_usage < param_usage) {
				caller_usage = param_usage;
			}
		}
	}
	ctx->regs_to_save = 0;
	
	// On Win64 if we have at least one parameter, the caller must reserve 32bytes
	// called the shadow space.
	if (!ctx->is_sysv && caller_usage > 0 && caller_usage < 4) caller_usage = 4;
	
	tb_find_use_count(f, ctx->use_count);
	tb_find_live_intervals(f, ctx->intervals);
	
	// Create phi lookup table for later evaluation stages
	FOR_EACH_NODE(i, f, TB_PHI2, {
					  ctx->phis[ctx->phi_count++] = (PhiValue){
						  .reg = i, 
						  .storage_a = f->nodes.payload[i].phi2.a,
						  .storage_b = f->nodes.payload[i].phi2.b
					  };
				  });
	
    // Reserve stack and base pointer
	ctx->gpr_allocator |= 1u << RSP;
	ctx->gpr_allocator |= 1u << RBP;
	
	// Allocate local and parameter stack slots
	//tb_function_print(f, tb_default_print_callback, stdout);
	//printf("\n\n\n");
	
	const TB_FunctionPrototype* restrict proto = f->prototype;
	const GPR* parameter_gprs = ctx->is_sysv ? SYSV_GPR_PARAMETERS : WIN64_GPR_PARAMETERS;
	
	loop(i, (size_t)proto->param_count) {
		TB_DataType dt = proto->params[i];
		
		// Allocate space in stack
		int size = get_data_type_size(dt);
		ctx->stack_usage = align_up(ctx->stack_usage + size, size);
		
		// Setup cache
		StackSlot slot = { 
			.reg = TB_FIRST_PARAMETER_REG + i, .pos = -ctx->stack_usage,
			.gpr = GPR_NONE, .xmm = XMM_NONE
		};
		
		if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
			// xmm parameters
			if (i < 4) {
				slot.xmm = i;
				ctx->xmm_allocator |= (1u << i);
			}
		} else {
			// gpr parameters
			if (ctx->is_sysv && i < 6) { 
				slot.gpr = parameter_gprs[i];
				ctx->gpr_allocator |= (1u << parameter_gprs[i]);
			} else if (i < 4) { 
				slot.gpr = parameter_gprs[i];
				ctx->gpr_allocator |= (1u << parameter_gprs[i]);
			}
		}
		
		arrput(ctx->locals, slot);
	}
	
	// Just the splitting point between parameters
	// and locals in the stack.
	int param_space = ctx->stack_usage;
	bool saves_parameters = false;
	
	loop(i, f->nodes.count) {
		if (f->nodes.type[i] == TB_PARAM_ADDR) {
			// having a PARAM_ADDR forces the parameter to spill onto the stack
			TB_Register param = f->nodes.payload[i].param_addr.param;
			int id = f->nodes.payload[param].param.id;
			
			assert(TB_FIRST_PARAMETER_REG + id == param);
			spill_stack_slot(ctx, f, &ctx->locals[id]);
		} else if (f->nodes.type[i] == TB_LOCAL) {
			uint32_t size = f->nodes.payload[i].local.size;
			uint32_t align = f->nodes.payload[i].local.alignment;
			
			ctx->stack_usage = align_up(ctx->stack_usage + size, align);
			
			StackSlot slot = {
				.reg = i, .pos = -ctx->stack_usage,
				.gpr = GPR_NONE, .xmm = XMM_NONE
			};
			arrput(ctx->locals, slot);
		}
	}
	
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
		
		// Resolve any leftover expressions which are used later
		TB_Register next_label = bb_end + (reg_type == TB_LABEL ? 0 : 1);
		eval_compiler_fence(ctx, f, ctx->last_fence, next_label);
		ctx->last_fence = next_label;
		
		if (reg_type == TB_RET) {
			// Evaluate return value
			if (p.ret.value) {
				Val value = val_rvalue(ctx, f, eval(ctx, f, p.ret.value, bb_end), p.ret.value);
				
				if (TB_IS_FLOAT_TYPE(value.dt.type) || value.dt.width) {
					// Float results use XMM0
					if (!is_value_xmm(&value, XMM0)) {
						uint8_t flags = 0;
						flags |= (value.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
						flags |= (value.dt.width) ? INST2FP_PACKED : 0;
						
						Val dst = val_xmm(value.dt, XMM0);
						inst2sse(ctx, FP_MOV, &dst, &value, flags);
					}
				} else if (value.dt.type == TB_I8 ||
						   value.dt.type == TB_I16 ||
						   value.dt.type == TB_I32 ||
						   value.dt.type == TB_I64 ||
						   value.dt.type == TB_PTR) {
					// Integer results use RAX and if result is extended RDX
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
						Val dst = val_gpr(value.dt.type, RAX);
						
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
			
			Val cond = eval(ctx, f, p.if_.cond, bb_end);
			if (cond.type == VAL_IMM) {
				TB_Label dst = (cond.imm ? if_true : if_false);
				
				if (dst != f->nodes.payload[bb_end + 1].label.id) jmp(ctx, dst);
			} else {
				// Implicit convert into FLAGS
				if (cond.dt.type == TB_BOOL) cond.dt.type = TB_I8;
				
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
		
		// Kill any stack slots and registers which aren't used 
		// beyond this basic block
		if (next_label < f->nodes.count) {
			loop_range(i, bb, bb_end) if (ctx->intervals[i] <= bb_end) {
				loop(j, arrlen(ctx->locals)) if (ctx->locals[j].reg == i) {
					//printf("kill r%d\n", i);
					arrdelswap(ctx->locals, j);
					break;
				}
			}
		}
		
		// Next Basic block
		bb = next_label;
	} while (bb < f->nodes.count);
	
	// Align stack usage to 16bytes and add 8 bytes for the return address
	ctx->stack_usage = ctx->stack_usage + (caller_usage * 8);
	if (!saves_parameters && ctx->stack_usage <= param_space && caller_usage == 0) {
		ctx->stack_usage = 8;
	} else {
		ctx->stack_usage = align_up(ctx->stack_usage + 8, 16);
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
		.linkage = f->linkage,
		.code = ctx->start_out,
		.code_size = ctx->out - ctx->start_out,
		.stack_usage = ctx->stack_usage,
		
		.prologue_epilogue_metadata = ctx->regs_to_save
	};
	
	arrfree(ctx->locals);
	
	if (is_ctx_heap_allocated) {
		free(ctx->use_count);
		free(ctx->intervals);
		free(ctx->phis);
		
		free(ctx->phi_queue);
		
		free(ctx->labels);
		free(ctx->label_patches);
		free(ctx->ret_patches);
		free(ctx);
	}
	return func_out;
}

// If there's any non-side effect instructions which happen before a 
// fence/side-effect instruction but are used afterwards, they're evaluated
// before the fence.
static void eval_compiler_fence(Ctx* restrict ctx, TB_Function* f, TB_Register start, TB_Register end) {
	for (size_t j = start; j < end; j++) {
		if (ctx->use_count[j] > 1 &&
			f->nodes.type[j] != TB_PARAM &&
			f->nodes.type[j] != TB_SIGNED_CONST &&
			f->nodes.type[j] != TB_UNSIGNED_CONST) {
			/* nothing for now */
			TB_DataType dt = f->nodes.dt[j];
			
			Val src = val_rvalue(ctx, f, eval(ctx, f, j, 0), j);
			if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
				// Spill into new slot
				Val dst;
				if (src.is_owned && src.type == VAL_XMM) {
					dst = src;
				} else {
					dst = alloc_xmm(ctx, f, dt);
					
					uint8_t flags = 0;
					flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
					flags |= (dt.width) ? INST2FP_PACKED : 0;
					inst2sse(ctx, FP_MOV, &dst, &src, flags);
					
					free_val(ctx, f, src);
				}
				
				StackSlot slot = {
					.reg = j, .pos = 0,
					.gpr = GPR_NONE, .xmm = dst.xmm
				};
				arrput(ctx->locals, slot);
				//printf("%s: r%zu is shared in XMM%d.\n", f->name, j, dst.xmm);
			} else {
				Val dst;
				if (src.is_owned && src.type == VAL_GPR) {
					dst = src;
				} else {
					dst = alloc_gpr(ctx, f, dt.type, 0);
					inst2(ctx, MOV, &dst, &src, dt.type);
					free_val(ctx, f, src);
				}
				
				StackSlot slot = {
					.reg = j, .pos = 0,
					.gpr = dst.gpr, .xmm = XMM_NONE
				};
				arrput(ctx->locals, slot);
				//printf("%s: r%zu is shared in GPR%d.\n", f->name, j, dst.gpr);
			}
		} else if (ctx->intervals[j] > end) {
			if (f->nodes.type[j] == TB_PARAM) {
				if (TB_IS_NODE_TERMINATOR(f->nodes.type[end]) && f->nodes.type[end] != TB_RET) {
					spill_stack_slot(ctx, f, &ctx->locals[j - TB_FIRST_PARAMETER_REG]);
				}
			} else if (f->nodes.type[j] == TB_CALL ||
					   f->nodes.type[j] == TB_ECALL ||
					   f->nodes.type[j] == TB_VCALL) {
				TB_DataType dt = f->nodes.dt[j];
				
				// NOTE(NeGate): These generally generate their own stack slot reservations
				// so just append to it.
				StackSlot* slot = NULL;
				loop(l, arrlen(ctx->locals)) if (ctx->locals[l].reg == j) {
					slot = &ctx->locals[l];
					break;
				}
				assert(slot);
				
				if (slot->pos == 0) {
					int size = get_data_type_size(dt);
					ctx->stack_usage = align_up(ctx->stack_usage + size, size);
					
					slot->pos = -ctx->stack_usage;
				}
				
				spill_stack_slot(ctx, f, slot);
			} else if (f->nodes.type[j] != TB_LOCAL &&
					   f->nodes.type[j] != TB_LABEL &&
					   f->nodes.type[j] != TB_PARAM_ADDR &&
					   f->nodes.type[j] != TB_SIGNED_CONST &&
					   f->nodes.type[j] != TB_UNSIGNED_CONST &&
					   f->nodes.type[j] != TB_FLOAT_CONST &&
					   f->nodes.type[j] != TB_STRING_CONST) {
				TB_DataType dt = f->nodes.dt[j];
				
				// TODO(NeGate): Implement saving to registers when enough are
				// available
				//
				// Allocate space in stack
				int size = get_data_type_size(dt);
				ctx->stack_usage = align_up(ctx->stack_usage + size, size);
				
				// Setup cache
				StackSlot slot = { 
					.reg = j, .pos = -ctx->stack_usage,
					.gpr = GPR_NONE, .xmm = XMM_NONE
				};
				
				// Spill into new slot
				Val dst = val_stack(dt, slot.pos);
				Val src = val_rvalue(ctx, f, eval(ctx, f, j, 0), j);
				
				if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
					uint8_t flags = 0;
					flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
					flags |= (dt.width) ? INST2FP_PACKED : 0;
					
					if (!is_value_mem(&src)) {
						inst2sse(ctx, FP_MOV, &dst, &src, flags);
					} else {
						Val tmp = alloc_xmm(ctx, f, dt);
						
						inst2sse(ctx, FP_MOV, &tmp, &src, flags);
						inst2sse(ctx, FP_MOV, &dst, &tmp, flags);
						
						free_val(ctx, f, tmp);
					}
				} else {
					// TODO(NeGate): Implement XMM-based movs
					if (is_value_mem(&src)) {
						Val tmp = alloc_gpr(ctx, f, dt.type, 0);
						
						inst2(ctx, MOV, &tmp, &src, dt.type);
						inst2(ctx, MOV, &dst, &tmp, dt.type);
						
						free_val(ctx, f, tmp);
					} else {
						inst2(ctx, MOV, &dst, &src, dt.type);
					}
				}
				
				free_val(ctx, f, src);
				arrput(ctx->locals, slot);
				//printf("%s: spilled value r%zu because of barrier at r%d\n", f->name, j, end);
			}
		}
	}
}

static void eval_basic_block(Ctx* restrict ctx, TB_Function* f, TB_Register bb, TB_Register bb_end) {
	ctx->current_bb = bb;
	ctx->current_bb_end = bb_end;
	
	// Evaluate all side effect instructions
	for (size_t r = bb + 1; r < bb_end; r++) {
		TB_RegTypeEnum reg_type = f->nodes.type[r];
		if (!TB_IS_NODE_SIDE_EFFECT(reg_type)) continue;
		
		eval_compiler_fence(ctx, f, ctx->last_fence, r);
		
		TB_DataType dt = f->nodes.dt[r];
		TB_RegPayload* restrict p = &f->nodes.payload[r];
		switch (reg_type) {
			case TB_LINE_INFO: {
				p->line_info.pos = code_pos();
				break;
			}
			
			case TB_STORE: {
				TB_Register addr_reg = p->store.address;
				TB_Register val_reg = p->store.value;
				
				// Eval address and cast to the correct type for the store
				Val address = val_addressof(ctx, f, eval(ctx, f, addr_reg, r));
				
				store_into(ctx, f, dt, &address, r, addr_reg, val_reg);
				
				free_val(ctx, f, address);
				break;
			}
			
			case TB_CALL:
			case TB_ECALL:
			case TB_VCALL: {
				int param_start = p->call.param_start;
				int param_count = p->call.param_end - p->call.param_start;
				
				// Evict the GPRs that are caller saved
				uint16_t caller_saved = (ctx->is_sysv ? SYSV_ABI_CALLER_SAVED : WIN64_ABI_CALLER_SAVED);
				for (size_t j = 0; j < 16; j++) {
					if (caller_saved & (1u << j)) {
						assert(evict_gpr(ctx, f, j));
					}
				}
				
				// Evict the XMMs that are caller saved
				for (size_t j = 0; j < param_count; j++) {
					TB_DataType param_dt = f->nodes.dt[param_start + j];
					
					if (j < 4 && (param_dt.width || TB_IS_FLOAT_TYPE(param_dt.type))) {
						assert(evict_xmm(ctx, f, j));
					}
				}
				
				uint16_t before_gpr_reserves = ctx->gpr_allocator;
				uint16_t before_xmm_reserves = ctx->xmm_allocator;
				
				// evict & reserve return value
				if (TB_IS_FLOAT_TYPE(dt.type) || dt.width) {
					ctx->xmm_allocator |= (1u << XMM0);
				} else if (dt.type != TB_VOID) {
					ctx->gpr_allocator |= (1u << RAX);
				}
				
				// evaluate parameters
				const GPR* parameter_gprs = ctx->is_sysv ? SYSV_GPR_PARAMETERS : WIN64_GPR_PARAMETERS;
				for (size_t j = 0; j < param_count; j++) {
					TB_Register param_reg = f->vla.data[param_start + j];
					Val param = val_rvalue(ctx, f, eval(ctx, f, param_reg, r), param_reg);
					
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
							
							// reserve for a bit
							ctx->xmm_allocator |= (1u << j);
						} else if (param.dt.type != TB_VOID) {
							Val dst = val_gpr(param.dt.type, parameter_gprs[j]);
							if (!is_value_gpr(&param, parameter_gprs[j])) {
								inst2(ctx, MOV, &dst, &param, param.dt.type);
							}
							
							// reserve for a bit
							ctx->gpr_allocator |= (1u << parameter_gprs[j]);
						}
					} else {
						Val dst = val_base_disp(param.dt, RSP, 8 * j);
						
						// parameter is in memory
						if (is_xmm) {
							uint8_t flags = 0;
							flags |= (param.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
							flags |= (param.dt.width) ? INST2FP_PACKED : 0;
							
							if (param.type == VAL_MEM) {
								/*temporary_reserve_xmm(ctx, f, XMM0);
								Val tmp = val_xmm(param.dt, XMM0);
								
								inst2sse(ctx, FP_MOV, &tmp, &param, flags);
								inst2sse(ctx, FP_MOV, &dst, &tmp, flags);*/
								assert(0 && "TODO: setup XMM parameter passing with temporary");
							} else {
								inst2sse(ctx, FP_MOV, &dst, &param, flags);
							}
						} else {
							if (param.type == VAL_MEM) {
								Val tmp = alloc_gpr(ctx, f, dt.type, 0);
								
								inst2(ctx, MOV, &tmp, &param, param.dt.type);
								inst2(ctx, MOV, &dst, &tmp, param.dt.type);
								
								free_val(ctx, f, tmp);
							} else {
								inst2(ctx, MOV, &dst, &param, param.dt.type);
							}
						}
					}
					
					free_val(ctx, f, param);
				}
				
				// CALL instruction and patch
				TB_FunctionID source_func = f - f->module->functions.data;
				
				if (reg_type == TB_CALL) {
					TB_FunctionID target = p->call.target - f->module->functions.data;
					
					tb_emit_call_patch(f->module,
									   source_func,
									   target,
									   code_pos() + 1,
									   s_local_thread_id);
					
					// CALL rel32
					emit(0xE8);
					emit4(0x0);
				} else if (reg_type == TB_ECALL) {
					TB_ExternalID target = p->ecall.target;
					
					tb_emit_ecall_patch(f->module,
										source_func,
										target,
										code_pos() + 1,
										s_local_thread_id);
					
					// CALL rel32
					emit(0xE8);
					emit4(0x0);
				} else if (reg_type == TB_VCALL) {
					Val target = val_rvalue(ctx, f, eval(ctx, f, p->vcall.target, r), p->vcall.target);
					
					// call r/m64
					inst1(ctx, CALL_RM, &target);
					
					free_val(ctx, f, target);
				}
				
				ctx->gpr_allocator = before_gpr_reserves;
				ctx->xmm_allocator = before_xmm_reserves;
				
				// the return value
				if (dt.type == TB_VOID) {
					/* none */
				} else {
					StackSlot slot = { 
						.reg = r, .pos = 0, .gpr = GPR_NONE, .xmm = XMM_NONE
					};
					
					if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
						slot.xmm = XMM0;
					} else {
						slot.gpr = RAX;
					}
					arrput(ctx->locals, slot);
				}
				break;
			}
			
			case TB_INITIALIZE: {
				TB_Register addr = p->mem_op.dst;
				
				TB_Module* m = f->module;
				TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_Initializer* i = (TB_Initializer*)&m->initializers[p->init.id / per_thread_stride][p->init.id % per_thread_stride];
				
				// rep stosb, ol' reliable
				evict_gpr(ctx, f, RAX);
				evict_gpr(ctx, f, RCX);
				evict_gpr(ctx, f, RDI);
				
				{
					Val param = val_rvalue(ctx, f, eval(ctx, f, addr, r), addr);
					if (!is_value_gpr(&param, RDI)) {
						Val dst = val_gpr(TB_PTR, RDI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					free_val(ctx, f, param);
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
				
				// TODO(NeGate): Implement vector memset
				// rep stosb, ol' reliable
				assert(evict_gpr(ctx, f, RAX));
				assert(evict_gpr(ctx, f, RCX));
				assert(evict_gpr(ctx, f, RDI));
				
				{
					Val param = val_rvalue(ctx, f, eval(ctx, f, dst_reg, r), dst_reg);
					if (!is_value_gpr(&param, RDI)) {
						Val dst = val_gpr(TB_PTR, RDI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					free_val(ctx, f, param);
					ctx->gpr_allocator |= (1u << RDI);
				}
				
				{
					Val param = val_rvalue(ctx, f, eval(ctx, f, val_reg, r), val_reg);
					if (!is_value_gpr(&param, RAX)) {
						Val dst = val_gpr(TB_PTR, RAX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					free_val(ctx, f, param);
					ctx->gpr_allocator |= (1u << RAX);
				}
				
				{
					Val param = val_rvalue(ctx, f, eval(ctx, f, size_reg, r), size_reg);
					if (!is_value_gpr(&param, RCX)) {
						Val dst = val_gpr(TB_PTR, RCX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					free_val(ctx, f, param);
					ctx->gpr_allocator |= (1u << RCX);
				}
				
				// rep stosb
				emit(0xF3);
				emit(0xAA);
				
				// free up stuff
				ctx->gpr_allocator &= ~(1u << RAX);
				ctx->gpr_allocator &= ~(1u << RCX);
				ctx->gpr_allocator &= ~(1u << RDI);
				break;
			}
			
			case TB_MEMCPY: {
				TB_Register dst_reg = p->mem_op.dst;
				TB_Register src_reg = p->mem_op.src;
				TB_Register size_reg = p->mem_op.size;
				
				// TODO(NeGate): Implement vector memset
				// rep movsb, ol' reliable
				assert(evict_gpr(ctx, f, RCX));
				assert(evict_gpr(ctx, f, RSI));
				assert(evict_gpr(ctx, f, RDI));
				
				{
					Val param = val_rvalue(ctx, f, eval(ctx, f, dst_reg, r), dst_reg);
					if (!is_value_gpr(&param, RDI)) {
						Val dst = val_gpr(TB_PTR, RDI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					free_val(ctx, f, param);
					ctx->gpr_allocator |= (1u << RDI);
				}
				
				{
					Val param = val_rvalue(ctx, f, eval(ctx, f, src_reg, r), src_reg);
					if (!is_value_gpr(&param, RSI)) {
						Val dst = val_gpr(TB_PTR, RSI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					free_val(ctx, f, param);
					ctx->gpr_allocator |= (1u << RSI);
				}
				
				{
					Val param = val_rvalue(ctx, f, eval(ctx, f, size_reg, r), size_reg);
					if (!is_value_gpr(&param, RCX)) {
						Val dst = val_gpr(TB_PTR, RCX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					free_val(ctx, f, param);
					ctx->gpr_allocator |= (1u << RCX);
				}
				
				// rep movsb
				emit(0xF3);
				emit(0xA4);
				
				// free up stuff
				ctx->gpr_allocator &= ~(1u << RSI);
				ctx->gpr_allocator &= ~(1u << RDI);
				ctx->gpr_allocator &= ~(1u << RCX);
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
				
				Val src = val_rvalue(ctx, f, eval(ctx, f, p->atomic.src, r), p->atomic.src);
				Val addr = val_addressof(ctx, f, eval(ctx, f, p->atomic.addr, r));
				
				// sometimes we only need to do the operation atomic without
				// a fetch, then things get... fancy
				if (ctx->intervals[r]) {
					assert(0 && "TODO: Atomic operations with fetch.");
				} else {
					int op = tbl[reg_type - TB_ATOMIC_XCHG];
					
					// LOCK prefix is not needed on XCHG because
					// it's actually a MOV which is naturally atomic
					// when aligned.
					if (is_value_mem(&src)) {
						Val tmp = alloc_gpr(ctx, f, dt.type, 0);
						inst2(ctx, MOV, &tmp, &src, dt.type);
						
						if (reg_type != TB_ATOMIC_XCHG) emit(0xF0);
						
						inst2(ctx, op, &addr, &tmp, dt.type);
						free_val(ctx, f, tmp);
					} else {
						if (reg_type != TB_ATOMIC_XCHG) emit(0xF0);
						
						inst2(ctx, op, &addr, &src, dt.type);
					}
				}
				
				free_val(ctx, f, src);
				free_val(ctx, f, addr);
				break;
			}
			case TB_ATOMIC_CMPXCHG: {
				//TB_RegPayload* restrict p2 = &f->nodes.payload[r + 1];
				assert(0 && "Atomic cmpxchg not supported yet.");
				break;
			}
			case TB_ATOMIC_CMPXCHG2: break;
			
			default:
			assert(0);
			break;
		}
		
		ctx->last_fence = r;
	}
}

static void store_into(Ctx* restrict ctx, TB_Function* f, TB_DataType dt, const Val* dst, TB_Register r, TB_Register dst_reg, TB_Register val_reg) {
	if (dt.type == TB_BOOL) dt.type = TB_I8;
	
	if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
		Val value = val_rvalue(ctx, f, eval(ctx, f, val_reg, r), val_reg);
		
		// if they match and it's just a MOV, don't do it
		if (is_value_match(dst, &value)) return;
		
		uint8_t flags = 0;
		flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
		flags |= (dt.width) ? INST2FP_PACKED : 0;
		
		if (!is_value_mem(&value)) {
			inst2sse(ctx, FP_MOV, dst, &value, flags);
		} else {
			Val tmp = alloc_xmm(ctx, f, dt);
			
			inst2sse(ctx, FP_MOV, &tmp, &value, flags);
			inst2sse(ctx, FP_MOV, dst, &tmp, flags);
			
			free_val(ctx, f, tmp);
		}
		
		free_val(ctx, f, value);
	} else {
		int op = MOV;
		
		// Peephole folded operation:
		// OP dst, src
		if (f->nodes.type[val_reg] >= TB_AND && f->nodes.type[val_reg] <= TB_MUL) {
			TB_Register a = f->nodes.payload[val_reg].i_arith.a;
			TB_Register b = f->nodes.payload[val_reg].i_arith.b;
			
			bool is_phi = f->nodes.type[r] == TB_PHI2;
			bool folded = false;
			if (is_phi) {
				folded = is_phi_that_contains(f, r, val_reg);
				folded |= (f->nodes.type[val_reg] == TB_MUL && dst->type == VAL_GPR);
			} else {
				folded = f->nodes.type[val_reg] != TB_MUL &&
					f->nodes.type[a] == TB_LOAD && 
					f->nodes.payload[a].load.address == dst_reg;
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
		
		Val value = val_rvalue(ctx, f, eval(ctx, f, val_reg, r), val_reg);
		
		// if they match and it's just a MOV, don't do it
		if (op == MOV && is_value_match(dst, &value)) return;
		
		if (!is_value_mem(&value)) {
			inst2(ctx, op, dst, &value, dt.type);
		} else {
			Val tmp = alloc_gpr(ctx, f, dt.type, 0);
			
			inst2(ctx, MOV, &tmp, &value, dt.type);
			inst2(ctx, op, dst, &tmp, dt.type);
			
			free_val(ctx, f, tmp);
		}
		
		free_val(ctx, f, value);
	}
}

static bool evict_gpr(Ctx* restrict ctx, TB_Function* f, GPR g) {
	if ((ctx->gpr_allocator & (1u << g)) == 0) return true;
	
	// find the stack slot that maps to this GPR
	loop(i, arrlen(ctx->locals)) if (ctx->locals[i].gpr == g) {
		StackSlot* slot = &ctx->locals[i];
		ctx->gpr_allocator &= ~(1u << g);
		
		// if it has no spill location we automatically give it one
		TB_DataType dt = f->nodes.dt[slot->reg];
		if (slot->pos == 0) {
			int size = get_data_type_size(dt);
			ctx->stack_usage = align_up(ctx->stack_usage + size, size);
			
			slot->pos = -ctx->stack_usage;
		}
		
		spill_stack_slot(ctx, f, slot);
		return true;
	}
	
	loop(i, ctx->phi_count) {
		PhiValue* restrict phi = &ctx->phis[i];
		if (phi->value.type == VAL_GPR && phi->value.gpr == g) {
			TB_DataType dt = f->nodes.dt[phi->reg];
			
			ctx->gpr_allocator &= ~(1u << phi->value.gpr);
			
			if (phi->spill == 0) {
				int size = get_data_type_size(dt);
				ctx->stack_usage = align_up(ctx->stack_usage + size, size);
				
				phi->spill = -ctx->stack_usage;
			}
			
			Val dst = val_stack(dt, phi->spill);
			Val src = val_gpr(dt.type, phi->value.gpr);
			inst2(ctx, MOV, &dst, &src, dt.type);
			return true;
		}
	}
	
	return false;
}

static bool evict_xmm(Ctx* restrict ctx, TB_Function* f, XMM x) {
	if ((ctx->xmm_allocator & (1u << x)) == 0) return true;
	
	// find the stack slot that maps to this XMM
	loop(i, arrlen(ctx->locals)) if (ctx->locals[i].xmm == x) {
		StackSlot* slot = &ctx->locals[i];
		
		ctx->xmm_allocator &= ~(1u << x);
		
		// if it has no spill location we automatically give it one
		TB_DataType dt = f->nodes.dt[slot->reg];
		if (slot->pos == 0) {
			int size = get_data_type_size(dt);
			ctx->stack_usage = align_up(ctx->stack_usage + size, size);
			
			slot->pos = -ctx->stack_usage;
		}
		
		spill_stack_slot(ctx, f, slot);
		return true;
	}
	
	loop(i, ctx->phi_count) {
		PhiValue* restrict phi = &ctx->phis[i];
		
		if (phi->value.type == VAL_XMM && phi->value.xmm == x) {
			TB_DataType dt = f->nodes.dt[phi->reg];
			
			ctx->xmm_allocator &= ~(1u << phi->value.xmm);
			
			if (phi->spill == 0) {
				int size = get_data_type_size(dt);
				ctx->stack_usage = align_up(ctx->stack_usage + size, size);
				
				phi->spill = -ctx->stack_usage;
			}
			
			uint8_t flags = 0;
			flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
			flags |= (dt.width) ? INST2FP_PACKED : 0;
			
			Val dst = val_stack(dt, phi->spill);
			Val src = val_xmm(dt, phi->value.xmm);
			inst2sse(ctx, FP_MOV, &dst, &src, flags);
			return true;
		}
	}
	
	return false;
}

static bool is_address_node(TB_RegType t) {
	switch (t) {
		case TB_LOCAL:
		case TB_RESTRICT:
		case TB_PARAM_ADDR:
		case TB_ARRAY_ACCESS:
		case TB_MEMBER_ACCESS:
		return true;
		default: 
		return false;
	}
}

static int get_data_type_size(const TB_DataType dt) {
	assert(dt.width <= 2 && "Vector width too big!");
	
	switch (dt.type) {
		case TB_VOID:
		case TB_BOOL:
		return 1;
		case TB_I8: 
		return 1 << dt.width;
		case TB_I16: 
		return 2 << dt.width;
		case TB_I32: case TB_F32:
		return 4 << dt.width;
		case TB_I64: case TB_F64:
		return 8 << dt.width;
		case TB_PTR:
		return 8;
		default: tb_unreachable();
	}
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
		
		TB_RegPayload* restrict p = &f->nodes.payload[r];
		TB_DataType dt = f->nodes.dt[r];
		
		if (dt.type == TB_BOOL) dt.type = TB_I8;
		
		TB_Register src;
		if (p->phi2.a_label == from) src = p->phi2.a;
		else if (p->phi2.b_label == from) src = p->phi2.b;
		else tb_unreachable();
		
		PhiValue* phi = find_phi(ctx, r);
		if (phi->spill) {
			Val src = val_stack(dt, phi->spill);
			
			if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
				uint8_t flags = 0;
				flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (dt.width) ? INST2FP_PACKED : 0;
				
				inst2sse(ctx, FP_MOV, &phi->value, &src, flags);
			} else {
				inst2(ctx, MOV, &phi->value, &src, dt.type);
			}
			phi->spill = 0;
		}
		
		if (phi->value.type == VAL_NONE) {
			// Initialize it
			Val src_value = val_rvalue(ctx, f, eval(ctx, f, src, r), src);
			
			if (src_value.type == VAL_GPR && is_temporary_of_bb(ctx, f, src, from, from_terminator)) {
				// Recycle old value
				phi->value = src_value;
			} else {
				// Create a new GPR and map it
				phi->value = alloc_gpr(ctx, f, dt.type, 0);
				
				// TODO(NeGate): Handle vector and float types
				if (!is_value_gpr(&src_value, phi->value.gpr)) {
					if (src_value.type == VAL_IMM && src_value.imm == 0) {
						GPR gpr = phi->value.gpr;
						
						if (gpr >= 8) emit(rex(true, gpr, gpr, 0));
						emit(0x31);
						emit(mod_rx_rm(MOD_DIRECT, gpr, gpr));
					} else {
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
	
	ctx->phi_queue_count = 0;
}

static bool is_temporary_of_bb(Ctx* ctx, TB_Function* f, TB_Register bound, TB_Register bb, TB_Register bb_end) {
	return (bound >= bb && bound <= bb_end && f->nodes.type[bound] != TB_PHI2);
}

static PhiValue* find_phi(Ctx* ctx, TB_Register r) {
	for (size_t i = 0; i < ctx->phi_count; i++) {
		if (ctx->phis[i].reg == r) return &ctx->phis[i];
	}
	
	return NULL;
}

// Is this a phi node? does it can the register `reg`?
static bool is_phi_that_contains(TB_Function* f, TB_Register phi, TB_Register reg) {
	if (f->nodes.type[phi] == TB_PHI2) {
		return f->nodes.payload[phi].phi2.a == reg || f->nodes.payload[phi].phi2.b == reg;
	} else {
		return false;
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
			
			// x64 thinks of relative addresses as being relative
			// to the end of the instruction or in this case just
			// 4 bytes ahead hence the +4.
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
