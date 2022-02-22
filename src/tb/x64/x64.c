// This entire module is one translation unit so that it doesn't have to worry
// about C's crappy support for public and private interfaces.
#include "x64.h"
#include "inst.h"
#include "proepi.h"
#include "reg_alloc.h"
#include "tree.h"

#if 0
static const char* GPR_NAMES[] = {
	"RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI",
	"R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"
};

#define DEBUG_LOG(...) printf(__VA_ARGS__)
#else
#define DEBUG_LOG(...) ((void)0)
#endif

static int get_data_type_size(const TB_DataType dt);
static void eval_basic_block(Ctx* restrict ctx, TB_Function* f, TB_Reg bb, TB_Reg bb_end);
static void store_into(Ctx* restrict ctx, TB_Function* f, TB_DataType dt, const Val* dst, TB_Reg r, TB_Reg dst_reg, TB_Reg val_reg, bool place_fence);

// Just handles the PHI nodes that we'll encounter when leaving `from` into `to`
static void eval_terminator_phis(Ctx* ctx, TB_Function* f, TB_Reg from, TB_Reg from_terminator, TB_Reg to, TB_Reg to_terminator);

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
		
		// Evaluate the terminator
		if (end->type == TB_RET) {
			TB_DataType dt = end->dt;
			if (dt.type == TB_BOOL) dt.type = TB_I8;
			
			// Evaluate return value
			if (end->ret.value) {
				Val value = eval(ctx, f, end->ret.value);
				
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
					} else {
						bool is_address = is_address_node(f->nodes.data[end->ret.value].type);
						
						Val dst = val_gpr(dt.type, RAX);
						if (is_address && is_value_mem(&value)) {
							inst2(ctx, LEA, &dst, &value, dt.type);
						} else if (!is_value_gpr(&value, RAX)) {
							inst2(ctx, MOV, &dst, &value, dt.type);
						}
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
			
			Val cond = eval(ctx, f, end->if_.cond);
			
			// NOTE(NeGate): the FLAGS should be fine as long as spill_reg doesn't clobber
			// registers... which it doesn't...
			loop_range(i, bb, bb_end) {
				if (ctx->values[i].type == VAL_GPR || ctx->values[i].type == VAL_XMM) {
					spill_reg(ctx, f, i);
				}
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
			
			kill(ctx, f, end->if_.cond);
		} else if (end->type == TB_GOTO) {
			// TODO(NeGate): save out any phi nodes
			assert(next_bb->type == TB_LABEL);
			
			TB_Label target_label = end->goto_.label;
			TB_Reg target = tb_find_reg_from_label(f, target_label);
			TB_Reg target_end = f->nodes.data[target].label.terminator;
			
			eval_terminator_phis(ctx, f, bb, bb_end, target, target_end);
			
			loop_range(i, bb, bb_end) {
				if (ctx->values[i].type == VAL_GPR || ctx->values[i].type == VAL_XMM) {
					spill_reg(ctx, f, i);
				}
			}
			
			TB_Label fallthrough_label = next_bb->label.id;
			if (fallthrough_label != end->goto_.label) jmp(ctx, end->goto_.label);
		} else if (end->type == TB_LABEL) {
			// simple fallthrough
			// save out any phi nodes
			TB_Reg next_terminator = end->label.terminator;
			eval_terminator_phis(ctx, f, bb, bb_end, bb_end, next_terminator);
			
			loop_range(i, bb, bb_end) {
				if (ctx->values[i].type == VAL_GPR || ctx->values[i].type == VAL_XMM) {
					spill_reg(ctx, f, i);
				}
			}
		} else if (end->type == TB_TRAP) {
			loop_range(i, bb, bb_end) {
				if (ctx->values[i].type == VAL_GPR || ctx->values[i].type == VAL_XMM) {
					spill_reg(ctx, f, i);
				}
			}
			
			// ud2
			emit(0x0F); 
			emit(0x0B);
		} else if (end->type == TB_SWITCH) {
			static_assert(_Alignof(TB_SwitchEntry) == _Alignof(TB_Reg), "We don't want any unaligned accesses");
			
			Val key = eval(ctx, f, end->switch_.key);
			
			// spill any leftover values
			loop_range(i, bb, bb_end) {
				if (ctx->values[i].type == VAL_GPR || ctx->values[i].type == VAL_XMM) {
					spill_reg(ctx, f, i);
				}
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
				
				kill(ctx, f, end->switch_.key);
				
				if (key.type != VAL_GPR) {
					bool is_address = is_address_node(f->nodes.data[end->switch_.key].type);
					
					Val new_val = alloc_gpr(ctx, f, TB_TEMP_REG, TB_PTR);
					inst2(ctx, is_address ? LEA : MOV, &new_val, &key, dt.type);
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

static void eval_basic_block(Ctx* restrict ctx, TB_Function* f, TB_Reg bb, TB_Reg bb_end) {
	// first node in the basic block
	bb = f->nodes.data[bb].next;
	if (bb == bb_end) return;
	
	// Evaluate all side effect instructions
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
				
				def(ctx, f, r, phi->value);
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
				}
				
				store_into(ctx, f, dt, &address, r, addr_reg, val_reg, true);
				
				kill(ctx, f, addr_reg);
				break;
			}
			case TB_PARAM_ADDR: {
				TB_Reg param = n->param_addr.param;
				
				Val val = eval(ctx, f, param);
				val.dt = dt;
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
				inst2sse(ctx, FP_CVT, &val, &src, flags);
				
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

static void store_into(Ctx* restrict ctx, TB_Function* f, TB_DataType dt, const Val* dst, TB_Reg r, TB_Reg dst_reg, TB_Reg val_reg, bool place_fence) {
	Val value = eval(ctx, f, val_reg);
	if (dt.type == TB_BOOL) dt.type = TB_I8;
	
	// if they match, don't do it
	if (is_value_match(dst, &value)) return;
	
	if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
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
		
		kill(ctx, f, val_reg);
	} else {
		if (!is_value_mem(&value)) {
			inst2(ctx, MOV, dst, &value, dt.type);
		} else {
			Val tmp = alloc_gpr(ctx, f, TB_TEMP_REG, dt.type);
			bool is_address = is_address_node(f->nodes.data[val_reg].type);
			
			inst2(ctx, is_address ? LEA : MOV, &tmp, &value, dt.type);
			inst2(ctx, MOV, dst, &tmp, dt.type);
			
			free_gpr(ctx, f, tmp.gpr);
		}
	}
	
	kill(ctx, f, val_reg);
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
		
		default:
		tb_unreachable();
		return 0;
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
			Val src_value = eval(ctx, f, src);
			
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

#if _MSC_VER
_Pragma("warning (push)")
_Pragma("warning (disable: 4028)")
#endif

// I put it down here because i can :P
ICodeGen x64_fast_code_gen = {
	.emit_call_patches = x64_emit_call_patches,
	.get_prologue_length = x64_get_prologue_length,
	.get_epilogue_length = x64_get_epilogue_length,
	.emit_prologue = x64_emit_prologue,
	.emit_epilogue = x64_emit_epilogue,
	.compile_function = x64_compile_function
};

#if _MSC_VER
_Pragma("warning (pop)")
#endif
