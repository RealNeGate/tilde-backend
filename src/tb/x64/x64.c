// This entire module is one translation unit so that it doesn't have to worry
// about C's crappy support for public and private interfaces.
static const char* GPR_NAMES[] = {
	"RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI",
	"R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"
};

#include "x64.h"
#include "inst.h"
#include "proepi.h"
#include "reg_alloc.h"
#include "tree.h"

#if 1
#define DEBUG_LOG(...) printf(__VA_ARGS__)
#else
#define DEBUG_LOG(...) ((void)0)
#endif

static int get_data_type_size(const TB_DataType dt);
static void eval_basic_block(Ctx* restrict ctx, TB_Function* f, TB_Reg bb, TB_Reg bb_end);
static void store_into(Ctx* restrict ctx, TB_Function* f, TB_DataType dt, const Val* dst, TB_Reg r, TB_Reg dst_reg, TB_Reg val_reg, bool place_fence);

// Just handles the PHI nodes that we'll encounter when leaving `from` into `to`
static void eval_terminator_phis(Ctx* ctx, TB_Function* f, TB_Reg from, TB_Reg from_terminator, TB_Reg to, TB_Reg to_terminator);
static bool does_compiler_fence_do_stuff(Ctx* restrict ctx, TB_Function* f, TB_Reg start, TB_Reg end, bool dont_handle_last_node);

typedef struct FunctionTally {
	size_t memory_usage;
	
	size_t phi_count;
	size_t locals_count;
	size_t return_count;
	size_t line_info_count;
	size_t label_patch_count;
} FunctionTally;

static FunctionTally tally_memory_usage(TB_Function* restrict f) {
	size_t phi_count = 0;
	size_t locals_count = 0;
	size_t return_count = 0;
	size_t label_patch_count = 0;
	size_t line_info_count = 0;
	
	TB_FOR_EACH_NODE(n, f) {
		TB_NodeTypeEnum t = n->type;
		
		if (t == TB_PHI2) phi_count++;
		else if (t == TB_RET) return_count++;
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
	tally += sizeof(Ctx) + (f->nodes.count * sizeof(Val));
	tally = (tally + align_mask) & ~align_mask;
	
	// use_count
	tally += f->nodes.count * sizeof(TB_Reg);
	tally = (tally + align_mask) & ~align_mask;
	
	// intervals
	tally += f->nodes.count * sizeof(TB_Reg);
	tally = (tally + align_mask) & ~align_mask;
	
	// phis
	tally += phi_count * sizeof(PhiValue);
	tally = (tally + align_mask) & ~align_mask;
	
	// phi_queue
	tally += phi_count * sizeof(TB_Reg);
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
		.line_info_count = line_info_count,
		.locals_count = locals_count,
		.return_count = return_count,
		.label_patch_count = label_patch_count
	};
}

TB_FunctionOutput x64_compile_function(TB_CompiledFunctionID id, TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id) {
	s_local_thread_id = local_thread_id;
	s_compiled_func_id = id;
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
		
		size_t ctx_size = sizeof(Ctx) + (f->nodes.count * sizeof(Val));
		if (is_ctx_heap_allocated) {
			//printf("Could not allocate x64 code gen context: using heap fallback. (%zu bytes)\n", tally.memory_usage);
			
			ctx = calloc(1, ctx_size);
			
			ctx->use_count = malloc(f->nodes.count * sizeof(TB_Reg));
			ctx->phis = malloc(tally.phi_count * sizeof(PhiValue));
			
			ctx->phi_queue = malloc(tally.phi_count * sizeof(TB_Node*));
			
			ctx->labels = malloc(f->label_count * sizeof(uint32_t));
			ctx->label_patches = malloc(tally.label_patch_count * sizeof(LabelPatch));
			ctx->ret_patches = malloc(tally.return_count * sizeof(ReturnPatch));
		} else {
			ctx = tb_tls_push(tls, ctx_size);
			
			memset(ctx, 0, ctx_size);
			
			ctx->use_count = tb_tls_push(tls, f->nodes.count * sizeof(TB_Reg));
			ctx->phis = tb_tls_push(tls, tally.phi_count * sizeof(PhiValue));
			
			ctx->phi_queue = tb_tls_push(tls, tally.phi_count * sizeof(TB_Node*));
			
			ctx->labels = tb_tls_push(tls, f->label_count * sizeof(uint32_t));
			ctx->label_patches = tb_tls_push(tls, tally.label_patch_count * sizeof(LabelPatch));
			ctx->ret_patches = tb_tls_push(tls, tally.return_count * sizeof(ReturnPatch));
		}
		
		ctx->last_fence = 1;
		ctx->start_out = ctx->out = out;
		ctx->f = f;
		ctx->function_id = f - f->module->functions.data;
		
		ctx->is_sysv = (f->module->target_system == TB_SYSTEM_LINUX ||
						f->module->target_system == TB_SYSTEM_MACOS);
		
		f->line_count = 0;
		f->lines = tb_platform_arena_alloc(tally.line_info_count * sizeof(TB_Line));
	}
	
	////////////////////////////////
	// Analyze function for stack, live intervals and phi nodes
	////////////////////////////////
	ctx->regs_to_save = 0;
	
	tb_function_calculate_use_count(f, ctx->use_count);
	
	// Create phi lookup table for later evaluation stages
	// and calculate the maximum parameter usage for a call
	size_t caller_usage = 0;
	TB_FOR_EACH_NODE(n, f) {
		if (n->type == TB_PHI2) {
			ctx->phis[ctx->phi_count++] = (PhiValue){
				.reg = n - f->nodes.data,
				.storage_a = n->phi2.a,
				.storage_b = n->phi2.b
			};
		} else if (n->type == TB_CALL ||
				   n->type == TB_ECALL ||
				   n->type == TB_VCALL) {
			int param_usage = CALL_NODE_PARAM_COUNT(n);
			if (caller_usage < param_usage) {
				caller_usage = param_usage;
			}
		}
	}
	
	// On Win64 if we have at least one parameter in any of it's calls, the
	// caller must reserve 32bytes called the shadow space.
	if (!ctx->is_sysv && caller_usage > 0 && caller_usage < 4) caller_usage = 4;
	
	ctx->is_tallying = true;
	
	// Allocate local and parameter stack slots
	//tb_function_print(f, tb_default_print_callback, stdout);
	//printf("\n\n\n");
	
	const TB_FunctionPrototype* restrict proto = f->prototype;
	
	loop(i, (size_t)proto->param_count) {
		TB_DataType dt = proto->params[i];
		
		// Allocate space in stack
		int size = get_data_type_size(dt);
		ctx->stack_usage = align_up(ctx->stack_usage + size, size);
		assert(size <= 8 && "Parameter too big");
		
		if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
			// xmm parameters
			if (i < 4) {
				ctx->xmm_allocator[i] = TB_FIRST_PARAMETER_REG + i;
				ctx->values[TB_FIRST_PARAMETER_REG + i] = val_xmm(dt, i);
			} else {
				ctx->values[TB_FIRST_PARAMETER_REG + i] = val_stack(dt, 16 + (i * 8));
			}
		} else {
			// gpr parameters
			if (ctx->is_sysv && i < 6) { 
				ctx->values[TB_FIRST_PARAMETER_REG + i] = val_gpr(dt.type, SYSV_GPR_PARAMETERS[i]);
				ctx->gpr_allocator[SYSV_GPR_PARAMETERS[i]] = TB_FIRST_PARAMETER_REG + i;
			} else if (i < 4) { 
				ctx->values[TB_FIRST_PARAMETER_REG + i] = val_gpr(dt.type, WIN64_GPR_PARAMETERS[i]);
				ctx->gpr_allocator[WIN64_GPR_PARAMETERS[i]] = TB_FIRST_PARAMETER_REG + i;
			} else {
				ctx->values[TB_FIRST_PARAMETER_REG + i] = val_stack(dt, 16 + (i * 8));
			}
		}
	}
	
	if (proto->has_varargs) {
		const GPR* parameter_gprs = ctx->is_sysv ? SYSV_GPR_PARAMETERS : WIN64_GPR_PARAMETERS;
		
		// spill the rest of the parameters (assumes they're all in the GPRs)
		size_t gpr_count = ctx->is_sysv ? 6 : 4;
		size_t extra_param_count = proto->param_count > gpr_count ? 0 : gpr_count - proto->param_count;
		
		loop(i, extra_param_count) {
			size_t param_num = proto->param_count + i;
			
			Val dst = val_stack(TB_TYPE_I64, 16 + (param_num * 8));
			Val src = val_gpr(TB_I64, parameter_gprs[param_num]);
			inst2(ctx, MOV, &dst, &src, TB_I64);
		}
	}
	
	// Just the splitting point between parameters
	// and locals in the stack.
	int param_space = ctx->stack_usage;
	bool saves_parameters = false;
	bool spills_params = false;
	
	TB_FOR_EACH_NODE(n, f) {
		TB_Reg i = n - f->nodes.data;
		
		if (n->type == TB_PARAM_ADDR) {
			// having a PARAM_ADDR forces the parameter to spill onto the stack
			TB_Reg param = n->param_addr.param;
			
			spill_reg(ctx, f, param);
			spills_params = true;
		} else if (n->type == TB_LOCAL) {
			uint32_t size = n->local.size;
			uint32_t align = n->local.alignment;
			
			ctx->stack_usage = align_up(ctx->stack_usage + size, align);
			ctx->values[i] = val_stack(TB_TYPE_PTR, -ctx->stack_usage);
		}
	}
	
	////////////////////////////////
	// Evaluate each basic block
	////////////////////////////////
	TB_Reg bb = 1;
	do {
		assert(f->nodes.data[bb].type == TB_LABEL);
		TB_Node* start = &f->nodes.data[bb];
		
		TB_Reg bb_end = start->label.terminator;
		TB_Node* end = &f->nodes.data[bb_end];
		
		// Define label position
		TB_Label label_id = start->label.id;
		ctx->labels[label_id] = code_pos();
		
#if !TB_STRIP_LABELS
		if (label_id) tb_emit_label_symbol(f->module,
										   f - f->module->functions.data,
										   label_id,
										   code_pos());
#endif
		
		// Generate instructions from the side-effect nodes using
		// all the other nodes and then terminate the basic block
		eval_basic_block(ctx, f, bb, bb_end);
		
		// Resolve any leftover expressions which are used later
		TB_Node* next_bb = end;
		if (end->type != TB_LABEL) next_bb = &f->nodes.data[next_bb->next];
		
		TB_Reg next_bb_reg = next_bb - f->nodes.data;
		bool needs_to_execute_fence = (next_bb_reg && ctx->last_fence != next_bb_reg);
		
		// Evaluate the terminator
		if (end->type == TB_RET) {
			TB_DataType dt = end->dt;
			
			// Evaluate return value
			if (end->ret.value) {
				Val value = eval_rvalue(ctx, f, end->ret.value);
				
				if (needs_to_execute_fence) {
					eval_compiler_fence(ctx, f, ctx->last_fence, next_bb_reg, true);
				}
				
				if (TB_IS_FLOAT_TYPE(dt.type) || dt.width) {
					// Float results use XMM0
					if (!is_value_xmm(&value, XMM0)) {
						uint8_t flags = 0;
						flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
						flags |= (dt.width) ? INST2FP_PACKED : 0;
						
						Val dst = val_xmm(dt, XMM0);
						inst2sse(ctx, FP_MOV, &dst, &value, flags);
					}
				} else if (dt.type == TB_I8 ||
						   dt.type == TB_I16 ||
						   dt.type == TB_I32 ||
						   dt.type == TB_I64 ||
						   dt.type == TB_PTR) {
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
						Val dst = val_gpr(dt.type, RAX);
						
						inst2(ctx, MOV, &dst, &value, dt.type);
					}
				} else tb_todo();
			}
			
			// Only jump if we aren't literally about to end the function
			if (next_bb != &f->nodes.data[0]) {
				ctx->ret_patches[ctx->ret_patch_count++] = code_pos() + 1;
				
				emit(0xE9);
				emit4(0x0);
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
				
				eval_terminator_phis(ctx, f, bb, bb_end, if_true_reg, if_true_reg_end);
				eval_terminator_phis(ctx, f, bb, bb_end, if_false_reg, if_false_reg_end);
			}
			
			ctx->is_if_statement_next = true;
			Val cond = eval(ctx, f, end->if_.cond, true);
			ctx->is_if_statement_next = false;
			
			bool cond_is_a_gpr_we_wanna_kill = false;
			if (needs_to_execute_fence) {
				if (cond.type == VAL_FLAGS && does_compiler_fence_do_stuff(ctx, f, ctx->last_fence, next_bb_reg, true)) {
					// save out the flags we wanted
					Val val = alloc_gpr(ctx, f, end->if_.cond, TB_I8);
					val.is_temp = true;
					
					// mov v, 0
					Val zero = val_imm(TB_TYPE_I8, 0);
					inst2(ctx, MOV, &val, &zero, TB_I8);
					
					// setcc v
					if (val.gpr >= 8) emit(rex(true, val.gpr, val.gpr, 0));
					emit(0x0F);
					emit(0x90 + cond.cond);
					emit(mod_rx_rm(MOD_DIRECT, val.gpr, val.gpr));
					
					kill(ctx, f, end->if_.cond, cond);
					cond = val;
				}
				
				eval_compiler_fence(ctx, f, ctx->last_fence, next_bb_reg, true);
			}
			
			if (cond.type == VAL_IMM) {
				TB_Label dst = (cond.imm ? if_true : if_false);
				
				if (dst != next_bb->label.id) jmp(ctx, dst);
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
					/* this is what we what */
				} else tb_todo();
				
				// Reorder the targets to avoid an extra JMP
				TB_Label fallthrough_label = 0;
				if (next_bb != &f->nodes.data[0]) {
					fallthrough_label = next_bb->label.id;
				}
				bool has_fallthrough = fallthrough_label == if_false;
				
				Cond cc = cond.cond;
				
				// flip the condition and the labels if
				// it allows for fallthrough
				if (fallthrough_label == if_true) {
					tb_swap(TB_Label, if_true, if_false);
					cc ^= 1;
					
					has_fallthrough = true;
				}
				
				// JCC .true
				// JMP .false # elidable if it points to the next instruction
				jcc(ctx, cc, if_true);
				if (!has_fallthrough) jmp(ctx, if_false);
			}
			
			kill(ctx, f, end->if_.cond, cond);
		} else if (end->type == TB_GOTO) {
			// TODO(NeGate): save out any phi nodes
			assert(next_bb->type == TB_LABEL);
			
			if (needs_to_execute_fence) {
				eval_compiler_fence(ctx, f, ctx->last_fence, next_bb_reg, true);
			}
			
			TB_Label target_label = end->goto_.label;
			TB_Reg target = tb_find_reg_from_label(f, target_label);
			TB_Reg target_end = f->nodes.data[target].label.terminator;
			
			eval_terminator_phis(ctx, f, bb, bb_end, target, target_end);
			
			TB_Label fallthrough_label = next_bb->label.id;
			if (fallthrough_label != end->goto_.label) jmp(ctx, end->goto_.label);
		} else if (end->type == TB_LABEL) {
			// simple fallthrough
			if (needs_to_execute_fence) {
				eval_compiler_fence(ctx, f, ctx->last_fence, next_bb_reg, true);
			}
			
			// save out any phi nodes
			TB_Reg next_terminator = end->label.terminator;
			eval_terminator_phis(ctx, f, bb, bb_end, bb_end, next_terminator);
		} else if (end->type == TB_TRAP) {
			// ud2
			emit(0x0F); 
			emit(0x0B);
		} else if (end->type == TB_SWITCH) {
			static_assert(_Alignof(TB_SwitchEntry) == _Alignof(TB_Reg), "We don't want any unaligned accesses");
			
			Val key = eval(ctx, f, end->switch_.key, true);
			
			if (needs_to_execute_fence) {
				eval_compiler_fence(ctx, f, ctx->last_fence, next_bb_reg, true);
			}
			
			if (key.type == VAL_IMM) {
				size_t entry_count = (end->switch_.entries_end - end->switch_.entries_start) / 2;
				
				TB_Label target_label = end->switch_.default_label;
				loop(i, entry_count) {
					TB_SwitchEntry* entry = (TB_SwitchEntry*) &f->vla.data[end->switch_.entries_start + (i * 2)];
					
					if (entry->key == key.imm) {
						target_label = entry->value;
						break;
					}
				}
				
				TB_Reg target = tb_find_reg_from_label(f, target_label);
				TB_Reg target_end = f->nodes.data[target].label.terminator;
				eval_terminator_phis(ctx, f, bb, bb_end, target, target_end);
				
				TB_Label fallthrough_label = next_bb->label.id;
				if (fallthrough_label != target) {
					jmp(ctx, target_label);
				}
			} else {
				TB_DataType dt = key.dt;
				if (dt.type == TB_BOOL) dt.type = TB_I8;
				
				if (key.type == VAL_GPR) {
					kill(ctx, f, end->switch_.key, key);
				} else {
					Val new_val = alloc_gpr(ctx, f, TB_TEMP_REG, TB_PTR);
					inst2(ctx, MOV, &new_val, &key, dt.type);
					key = new_val;
					
					free_gpr(ctx, f, new_val.gpr);
				}
				
				// Shitty if-chain
				// CMP key, 0
				// JE .case0
				// CMP key, 10
				// JE .case10
				// JMP .default
				size_t entry_count = (end->switch_.entries_end - end->switch_.entries_start) / 2;
				loop(i, entry_count) {
					TB_SwitchEntry* entry = (TB_SwitchEntry*) &f->vla.data[end->switch_.entries_start + (i * 2)];
					
					Val operand = val_imm(dt, entry->key);
					inst2(ctx, CMP, &key, &operand, dt.type);
					
					jcc(ctx, E, entry->value);
				}
				
				jmp(ctx, end->switch_.default_label);
			}
		} else if (end->type == TB_UNREACHABLE) {
			// doesn't need to do anything because it's all UB from here
		} else {
			tb_todo();
		}
		
		// kill any values, unless it's the last basic block then it doesn't matter :p
		if (next_bb_reg) loop_range(i, bb, bb_end) {
			if (ctx->use_count[i] == 0 && ctx->values[i].type != VAL_NONE) {
				free_val(ctx, f, i, ctx->values[i]);
				ctx->values[i] = (Val){ 0 };
			}
		}
		
		// Next Basic block
		bb = next_bb_reg;
	} while (bb != TB_NULL_REG);
	
	// Tally up any saved XMM registers
	ctx->stack_usage += tb_popcount((ctx->regs_to_save >> 16) & 0xFFFF) * 16;
	
	if (!spills_params) {
		TB_FOR_EACH_NODE(n, f) {
			TB_Reg i = n - f->nodes.data;
			if (n->type == TB_PARAM_ADDR && ctx->values[i].type == VAL_MEM) {
				spills_params = true;
				break;
			}
		}
	}
	
	// if we spilled any parameters just bias it so
	// that the prologue and epilogue are added
	if (spills_params) {
		ctx->stack_usage += 8;
	}
	
	// Align stack usage to 16bytes and add 8 bytes for the return address
	ctx->stack_usage = ctx->stack_usage + (caller_usage * 8);
	if (!saves_parameters && ctx->stack_usage <= param_space && caller_usage == 0) {
		ctx->stack_usage = 8;
	} else {
		ctx->stack_usage = align_up(ctx->stack_usage + 8, 16) + 8;
	}
	
	////////////////////////////////
	// Evaluate internal relocations (return and labels)
	////////////////////////////////
	loop(i, ctx->ret_patch_count) {
		uint32_t pos = ctx->ret_patches[i];
		patch4(pos, code_pos() - (pos + 4));
	}
	
	loop(i, ctx->label_patch_count) {
		uint32_t pos = ctx->label_patches[i].pos;
		uint32_t target_lbl = ctx->label_patches[i].target_lbl;
		
		patch4(pos, ctx->labels[target_lbl] - (pos + 4));
	}
	
	TB_FunctionOutput func_out = {
		.name = f->name,
		.function = tb_function_get_id(f->module, f),
		.linkage = f->linkage,
		.code = ctx->start_out,
		.code_size = ctx->out - ctx->start_out,
		.stack_usage = ctx->stack_usage,
		
		.prologue_epilogue_metadata = ctx->regs_to_save
	};
	
	if (is_ctx_heap_allocated) {
		free(ctx->use_count);
		free(ctx->phis);
		
		free(ctx->phi_queue);
		
		free(ctx->labels);
		free(ctx->label_patches);
		free(ctx->ret_patches);
		free(ctx);
	}
	return func_out;
}

static bool does_compiler_fence_do_stuff(Ctx* restrict ctx, TB_Function* f, TB_Reg start, TB_Reg end, bool dont_handle_last_node) {
	TB_FOR_EACH_NODE_RANGE(n, f, start, end) {
		if (dont_handle_last_node && n->next == end) break;
		
		TB_Reg r = n - f->nodes.data;
		TB_NodeTypeEnum reg_type = n->type;
		
		if (ctx->use_count[r] && !should_rematerialize(reg_type)) {
			return true;
		}
	}
	
	return false;
}

// If there's any non-side effect instructions which happen before a 
// fence/side-effect instruction but are used afterwards, they're evaluated
// before the fence.
static void eval_compiler_fence(Ctx* restrict ctx, TB_Function* f, TB_Reg start, TB_Reg end, bool dont_handle_last_node) {
	// big stack allocs amirite...
	size_t to_save_count = 0;
	TB_Reg to_save[512];
	
	TB_FOR_EACH_NODE_RANGE(n, f, start, end) {
		if (dont_handle_last_node && n->next == end) break;
		
		assert(to_save_count < tb_arrlen(to_save));
		to_save[to_save_count++] = (n - f->nodes.data);
	}
	
	loop_reverse(i, to_save_count) {
		TB_Reg r = to_save[i];
		TB_Node* n = &f->nodes.data[r];
		
		TB_NodeTypeEnum reg_type = n->type;
		if (ctx->use_count[r] && !should_rematerialize(reg_type)) {
			// dummy eval to cache the results before the fence
			// it gets it's use count tick undone to avoid issues
			Val val;
			
			ctx->is_tallying = false;
			bool is_rval = is_address_node(reg_type) || (reg_type == TB_LOAD && n->dt.type == TB_PTR);
			if (is_rval) {
				val = eval(ctx, f, r, true);
			} else {
				val = eval_rvalue(ctx, f, r);
			}
			ctx->is_tallying = true;
			
			if (val.is_temp) {
				val.is_temp = false;
			} else if (val.dt.width || TB_IS_FLOAT_TYPE(val.dt.type)) {
				Val dst = alloc_xmm(ctx, f, r, val.dt);
				
				uint8_t flags = 0;
				flags |= (val.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
				flags |= (val.dt.width) ? INST2FP_PACKED : 0;
				
				if (!is_value_xmm(&val, dst.xmm)) {
					inst2sse(ctx, FP_MOV, &dst, &val, flags);
				}
				ctx->values[r] = dst;
			} else if (is_value_mem(&val) && is_address_node(reg_type)) {
				Val dst = alloc_gpr(ctx, f, r, val.dt.type);
				inst2(ctx, LEA, &dst, &val, val.dt.type);
				ctx->values[r] = dst;
			} else {
				Val dst = alloc_gpr(ctx, f, r, val.dt.type);
				if (!is_value_gpr(&val, dst.gpr)) {
					inst2(ctx, MOV, &dst, &val, val.dt.type);
				}
				ctx->values[r] = dst;
			}
		}
	}
	
	ctx->last_fence = end;
}

static void eval_basic_block(Ctx* restrict ctx, TB_Function* f, TB_Reg bb, TB_Reg bb_end) {
	ctx->current_bb = bb;
	ctx->current_bb_end = bb_end;
	
	// first node in the basic block
	bb = f->nodes.data[bb].next;
	if (bb == bb_end) {
		ctx->last_fence = bb;
		return;
	}
	
	// Evaluate all side effect instructions
	TB_FOR_EACH_NODE_RANGE(n, f, bb, bb_end) {
		TB_Reg r = n - f->nodes.data;
		TB_NodeTypeEnum reg_type = n->type;
		if (!TB_IS_NODE_SIDE_EFFECT(reg_type)) continue;
		
		TB_DataType dt = n->dt;
		switch (reg_type) {
			case TB_LINE_INFO: {
				f->lines[f->line_count++] = (TB_Line){
					.file = n->line_info.file,
					.line = n->line_info.line,
					.pos = code_pos()
				};
				break;
			}
			
			case TB_DEBUGBREAK: {
				eval_compiler_fence(ctx, f, ctx->last_fence, r, true);
				emit(0xCC);
				break;
			}
			
			case TB_STORE: {
				TB_Reg addr_reg = n->store.address;
				TB_Reg val_reg = n->store.value;
				
				// Eval address and cast to the correct type for the store
				Val address = eval_addressof(ctx, f, addr_reg);
				
				store_into(ctx, f, dt, &address, r, addr_reg, val_reg, true);
				
				free_val(ctx, f, addr_reg, address);
				break;
			}
			
			case TB_CALL:
			case TB_ECALL:
			case TB_VCALL: {
				int param_start = n->call.param_start;
				int param_count = n->call.param_end - n->call.param_start;
				
				// TODO(NeGate): We can probably get more eager than
				// this later this fence could be moved up...
				eval_compiler_fence(ctx, f, ctx->last_fence, r, true);
				
				// Evict the GPRs that are caller saved
				uint16_t caller_saved = (ctx->is_sysv ? SYSV_ABI_CALLER_SAVED : WIN64_ABI_CALLER_SAVED);
				
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
				const GPR* parameter_gprs = ctx->is_sysv ? SYSV_GPR_PARAMETERS : WIN64_GPR_PARAMETERS;
				for (size_t j = 0; j < param_count; j++) {
					TB_Reg param_reg = f->vla.data[param_start + j];
					Val param = eval_rvalue(ctx, f, param_reg);
					
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
						} else if (param.dt.type != TB_VOID) {
							evict_gpr(ctx, f, parameter_gprs[j]);
							
							Val dst = val_gpr(param.dt.type, parameter_gprs[j]);
							if (!is_value_gpr(&param, parameter_gprs[j])) {
								inst2(ctx, MOV, &dst, &param, param.dt.type);
							}
						}
					} else {
						Val dst = val_base_disp(param.dt, RSP, 8 * j);
						
						// parameter is in memory
						if (is_xmm) {
							uint8_t flags = 0;
							flags |= (param.dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
							flags |= (param.dt.width) ? INST2FP_PACKED : 0;
							
							if (param.type == VAL_MEM) {
								Val tmp = alloc_xmm(ctx, f, TB_TEMP_REG, dt);
								
								inst2sse(ctx, FP_MOV, &tmp, &param, flags);
								inst2sse(ctx, FP_MOV, &dst, &tmp, flags);
								
								free_xmm(ctx, f, tmp.xmm);
							} else {
								inst2sse(ctx, FP_MOV, &dst, &param, flags);
							}
						} else {
							if (param.type == VAL_MEM) {
								Val tmp = alloc_gpr(ctx, f, TB_TEMP_REG, dt.type);
								
								inst2(ctx, MOV, &tmp, &param, param.dt.type);
								inst2(ctx, MOV, &dst, &tmp, param.dt.type);
								
								free_gpr(ctx, f, tmp.gpr);
							} else {
								inst2(ctx, MOV, &dst, &param, param.dt.type);
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
					Val target = eval_rvalue(ctx, f, n->vcall.target);
					
					// call r/m64
					inst1(ctx, CALL_RM, &target);
					
					kill(ctx, f, n->vcall.target, target);
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
				eval_compiler_fence(ctx, f, ctx->last_fence, r, true);
				
				TB_Reg addr = n->mem_op.dst;
				
				TB_Module* m = f->module;
				TB_InitializerID per_thread_stride = UINT_MAX / TB_MAX_THREADS;
				TB_Initializer* i = (TB_Initializer*)&m->initializers[n->init.id / per_thread_stride][n->init.id % per_thread_stride];
				
				// rep stosb, ol' reliable
				evict_gpr(ctx, f, RAX);
				evict_gpr(ctx, f, RCX);
				evict_gpr(ctx, f, RDI);
				
				{
					Val param = eval_rvalue(ctx, f, addr);
					if (!is_value_gpr(&param, RDI)) {
						Val dst = val_gpr(TB_PTR, RDI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
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
				break;
			}
			case TB_MEMSET: {
				TB_Reg dst_reg = n->mem_op.dst;
				TB_Reg val_reg = n->mem_op.src;
				TB_Reg size_reg = n->mem_op.size;
				
				// TODO(NeGate): Implement vector memset
				// rep stosb, ol' reliable
				evict_gpr(ctx, f, RAX);
				evict_gpr(ctx, f, RCX);
				evict_gpr(ctx, f, RDI);
				
				{
					Val param = eval_rvalue(ctx, f, dst_reg);
					if (!is_value_gpr(&param, RDI)) {
						Val dst = val_gpr(TB_PTR, RDI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					kill(ctx, f, dst_reg, param);
					ctx->gpr_allocator[RDI] = TB_TEMP_REG;
				}
				
				{
					Val param = eval_rvalue(ctx, f, val_reg);
					if (!is_value_gpr(&param, RAX)) {
						Val dst = val_gpr(TB_PTR, RAX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					kill(ctx, f, dst_reg, param);
					ctx->gpr_allocator[RAX] = TB_TEMP_REG;
				}
				
				{
					Val param = eval_rvalue(ctx, f, size_reg);
					if (!is_value_gpr(&param, RCX)) {
						Val dst = val_gpr(TB_PTR, RCX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					kill(ctx, f, dst_reg, param);
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
				eval_compiler_fence(ctx, f, ctx->last_fence, r, true);
				
				TB_Reg dst_reg = n->mem_op.dst;
				TB_Reg src_reg = n->mem_op.src;
				TB_Reg size_reg = n->mem_op.size;
				
				// TODO(NeGate): Implement vector memset
				// rep movsb, ol' reliable
				evict_gpr(ctx, f, RCX);
				evict_gpr(ctx, f, RSI);
				evict_gpr(ctx, f, RDI);
				
				{
					Val param = eval_rvalue(ctx, f, dst_reg);
					if (!is_value_gpr(&param, RDI)) {
						Val dst = val_gpr(TB_PTR, RDI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					kill(ctx, f, dst_reg, param);
					ctx->gpr_allocator[RDI] = TB_TEMP_REG;
				}
				
				{
					Val param = eval_rvalue(ctx, f, src_reg);
					if (!is_value_gpr(&param, RSI)) {
						Val dst = val_gpr(TB_PTR, RSI);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					kill(ctx, f, src_reg, param);
					ctx->gpr_allocator[RSI] = TB_TEMP_REG;
				}
				
				{
					Val param = eval_rvalue(ctx, f, size_reg);
					if (!is_value_gpr(&param, RCX)) {
						Val dst = val_gpr(TB_PTR, RCX);
						inst2(ctx, MOV, &dst, &param, TB_PTR);
					}
					kill(ctx, f, size_reg, param);
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
				
				Val src = eval_rvalue(ctx, f, n->atomic.src);
				Val addr = eval_addressof(ctx, f, n->atomic.addr);
				
				eval_compiler_fence(ctx, f, ctx->last_fence, r, true);
				
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

static void store_into(Ctx* restrict ctx, TB_Function* f, TB_DataType dt, const Val* dst, TB_Reg r, TB_Reg dst_reg, TB_Reg val_reg, bool place_fence) {
	if (dt.type == TB_BOOL) dt.type = TB_I8;
	
	if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
		Val value = eval_rvalue(ctx, f, val_reg);
		
		if (place_fence) {
			eval_compiler_fence(ctx, f, ctx->last_fence, r, true);
		}
		
		// if they match and it's just a MOV, don't do it
		if (is_value_match(dst, &value)) {
			return;
		}
		
		uint8_t flags = 0;
		flags |= (dt.type == TB_F64) ? INST2FP_DOUBLE : 0;
		flags |= (dt.width) ? INST2FP_PACKED : 0;
		
		if (!is_value_mem(&value)) {
			inst2sse(ctx, FP_MOV, dst, &value, flags);
		} else {
			Val tmp = alloc_xmm(ctx, f, TB_TEMP_REG, dt);
			
			inst2sse(ctx, FP_MOV, &tmp, &value, flags);
			inst2sse(ctx, FP_MOV, dst, &tmp, flags);
			
			free_xmm(ctx, f, tmp.xmm);
		}
		
		kill(ctx, f, val_reg, value);
	} else {
		int op = MOV;
		
		// Peephole folded operation:
		// OP dst, src
		TB_Node* val = &f->nodes.data[val_reg];
		
		// TB_AND TB_OR TB_XOR TB_ADD TB_SUB TB_MUL
		if (val->type >= TB_AND && val->type <= TB_MUL) {
			TB_Node* a = &f->nodes.data[val->i_arith.a];
			
			bool is_phi = f->nodes.data[r].type == TB_PHI2;
			bool folded = false;
			if (is_phi) {
				folded = is_phi_that_contains(f, r, val_reg);
				folded |= (val->type == TB_MUL &&
						   dst->type == VAL_GPR);
			} else {
				folded = (val->type != TB_MUL &&
						  a->type == TB_LOAD && 
						  a->load.address == dst_reg);
			}
			
			if (folded && ctx->use_count[val->i_arith.b]) {
				switch (val->type) {
					case TB_AND: op = AND; break;
					case TB_OR: op = OR; break;
					case TB_XOR: op = XOR; break;
					case TB_ADD: op = ADD; break;
					case TB_SUB: op = SUB; break;
					case TB_MUL: op = IMUL; break;
					default: tb_unreachable();
				}
				
				assert(ctx->use_count[val_reg] > 0);
				ctx->use_count[val_reg]--;
				
				//assert(ctx->use_count[val->i_arith.a] > 0);
				//ctx->use_count[val->i_arith.a]--;
				
				val_reg = val->i_arith.b;
			}
		}
		
		Val value = eval_rvalue(ctx, f, val_reg);
		
		if (place_fence) {
			eval_compiler_fence(ctx, f, ctx->last_fence, r, true);
		}
		
		// if they match and it's just a MOV, don't do it
		if (op == MOV && is_value_match(dst, &value)) {
			return;
		}
		
		if (!is_value_mem(&value)) {
			inst2(ctx, op, dst, &value, dt.type);
		} else {
			Val tmp = alloc_gpr(ctx, f, TB_TEMP_REG, dt.type);
			
			inst2(ctx, MOV, &tmp, &value, dt.type);
			inst2(ctx, op, dst, &tmp, dt.type);
			
			free_gpr(ctx, f, tmp.gpr);
		}
		
		kill(ctx, f, val_reg, value);
	}
}

static bool is_address_node(TB_NodeTypeEnum t) {
	switch (t) {
		case TB_LOCAL:
		case TB_RESTRICT:
		case TB_PARAM_ADDR:
		case TB_EXTERN_ADDRESS:
		case TB_GLOBAL_ADDRESS:
		case TB_ARRAY_ACCESS:
		case TB_MEMBER_ACCESS:
		//case TB_INT2PTR:
		return true;
		default: 
		return false;
	}
}

static bool should_rematerialize(TB_NodeTypeEnum t) {
	switch (t) {
		case TB_PARAM:
		case TB_PARAM_ADDR:
		case TB_LOCAL:
		case TB_LABEL:
		case TB_GLOBAL_ADDRESS:
		case TB_SIGNED_CONST:
		case TB_UNSIGNED_CONST:
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

static void eval_terminator_phis(Ctx* ctx, TB_Function* f, TB_Reg from, TB_Reg from_terminator, TB_Reg to, TB_Reg to_terminator) {
	ctx->phi_queue_count = 0;
	
	TB_FOR_EACH_NODE_RANGE(n, f, to, to_terminator) {
		if (n->type == TB_PHI2) ctx->phi_queue[ctx->phi_queue_count++] = n;
	}
	
	loop(i, ctx->phi_queue_count) {
		TB_Node* n = ctx->phi_queue[i];
		TB_Reg r = n - f->nodes.data;
		TB_DataType dt = n->dt;
		
		if (dt.type == TB_BOOL) dt.type = TB_I8;
		
		TB_Reg src;
		if (n->phi2.a_label == from) src = n->phi2.a;
		else if (n->phi2.b_label == from) src = n->phi2.b;
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
			Val src_value = eval_rvalue(ctx, f, src);
			
			if (src_value.type == VAL_GPR && is_temporary_of_bb(ctx, f, src, from, from_terminator)) {
				// Recycle old value
				phi->value = src_value;
			} else {
				// Create a new GPR and map it
				phi->value = alloc_gpr(ctx, f, r, dt.type);
				
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
			assert(f->nodes.data[src].type == TB_PHI2);
			PhiValue* other_phi = find_phi(ctx, src);
			
			inst2(ctx, XCHG, &phi->value, &other_phi->value, dt.type);
		} else {
			// Load value into existing phi node
			store_into(ctx, f, dt, &phi->value, r, r, src, false);
		}
	}
	
	ctx->phi_queue_count = 0;
}

static bool is_temporary_of_bb(Ctx* ctx, TB_Function* f, TB_Reg bound, TB_Reg bb, TB_Reg bb_end) {
	return (bound >= bb && bound <= bb_end && f->nodes.data[bound].type != TB_PHI2);
}

static PhiValue* find_phi(Ctx* ctx, TB_Reg r) {
	for (size_t i = 0; i < ctx->phi_count; i++) {
		if (ctx->phis[i].reg == r) return &ctx->phis[i];
	}
	
	return NULL;
}

// Is this a phi node? does it can the register `reg`?
static bool is_phi_that_contains(TB_Function* f, TB_Reg phi, TB_Reg reg) {
	if (f->nodes.data[phi].type == TB_PHI2) {
		return f->nodes.data[phi].phi2.a == reg || f->nodes.data[phi].phi2.b == reg;
	} else {
		return false;
	}
}

void x64_emit_call_patches(TB_Module* m, uint32_t* func_layout) {
	loop(i, m->max_threads) {
		TB_FunctionPatch* patches = m->call_patches[i];
		
		loop(j, arrlen(patches)) {
			TB_FunctionPatch* p = &patches[j];
			TB_FunctionOutput* out_f = &m->compiled_functions.data[p->source];
			
			uint64_t meta = out_f->prologue_epilogue_metadata;
			uint64_t stack_usage = out_f->stack_usage;
			uint8_t* code = out_f->code;
			
			// x64 thinks of relative addresses as being relative
			// to the end of the instruction or in this case just
			// 4 bytes ahead hence the +4.
			size_t actual_pos = func_layout[p->source]
				+ x64_get_prologue_length(meta, stack_usage)
				+ p->pos 
				+ 4;
			
			*((uint32_t*)&code[p->pos]) = func_layout[p->target_id] - actual_pos;
		}
	}
}

_Pragma("warning (push)")
_Pragma("warning (disable: 4028)")
// I put it down here because i can :P
ICodeGen x64_fast_code_gen = {
	.emit_call_patches = x64_emit_call_patches,
	.get_prologue_length = x64_get_prologue_length,
	.get_epilogue_length = x64_get_epilogue_length,
	.emit_prologue = x64_emit_prologue,
	.emit_epilogue = x64_emit_epilogue,
	.compile_function = x64_compile_function
};
_Pragma("warning (pop)")
