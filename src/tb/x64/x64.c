// This entire module is one translation unit so that it doesn't have to worry
// about C's crappy support for public and private interfaces. Because it's a
// separate TU completely from the rest of the project, it means that we can use
// tiny function names so long as they don't internally collide since they're static.
#define USING_FAST_PATH (0)

#include "x64.h"
#include "x64_emitter.h"
#include "x64_proepi.h"
#include "x64_fast.h"
#include "x64_complex.h"

#if 0
#define DEBUG_LOG(...) printf(__VA_ARGS__)
#else
#define DEBUG_LOG(...) ((void)0)
#endif

#if 0
TB_FunctionOutput x64_fast_compile_function(TB_CompiledFunctionID id, TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id) {
	s_local_thread_id = local_thread_id;
	s_compiled_func_id = id;
	
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	////////////////////////////////
	// Allocate all the memory we'll need
	bool is_ctx_heap_allocated = false;
	Ctx* restrict ctx = NULL;
	{
		// if we can't fit our memory usage into memory, we fallback
		FunctionTallySimple tally = tally_memory_usage_simple(f);
		is_ctx_heap_allocated = !tb_tls_can_fit(tls, tally.memory_usage);
		
		size_t ctx_size = sizeof(Ctx);
		if (is_ctx_heap_allocated) {
			ctx = tb_platform_heap_alloc(1, ctx_size);
			memset(ctx, 0, sizeof(Ctx));
			
			ctx->use_count = tb_platform_heap_alloc(f->nodes.count * sizeof(TB_Reg));
			ctx->phis = tb_platform_heap_alloc(tally.phi_count * sizeof(PhiValue));
			
			ctx->labels = tb_platform_heap_alloc(f->label_count * sizeof(uint32_t));
			ctx->label_patches = tb_platform_heap_alloc(tally.label_patch_count * sizeof(LabelPatch));
			ctx->ret_patches = tb_platform_heap_alloc(tally.return_count * sizeof(ReturnPatch));
			
			ctx->values = tb_platform_heap_alloc(f->nodes.count, sizeof(Val));
		} else {
			ctx = tb_tls_push(tls, ctx_size);
			memset(ctx, 0, ctx_size);
			
			ctx->use_count = tb_tls_push(tls, f->nodes.count * sizeof(TB_Reg));
			ctx->phis = tb_tls_push(tls, tally.phi_count * sizeof(PhiValue));
			
			ctx->labels = tb_tls_push(tls, f->label_count * sizeof(uint32_t));
			ctx->label_patches = tb_tls_push(tls, tally.label_patch_count * sizeof(LabelPatch));
			ctx->ret_patches = tb_tls_push(tls, tally.return_count * sizeof(ReturnPatch));
			
			ctx->values = tb_tls_push(tls, f->nodes.count * sizeof(Val));
		}
		memset(ctx->values, 0, f->nodes.count * sizeof(Val));
		
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
			ctx->phis[ctx->phi_count++] = (PhiValue){ .simple = { n - f->nodes.data } };
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
		if (label_id) {
			tb_emit_label_symbol(f->module, ctx->function_id, label_id, code_pos());
		}
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
		tb_platform_heap_free(ctx->use_count);
		tb_platform_heap_free(ctx->phis);
		
		tb_platform_heap_free(ctx->labels);
		tb_platform_heap_free(ctx->label_patches);
		tb_platform_heap_free(ctx->ret_patches);
		tb_platform_heap_free(ctx);
	}
	return func_out;
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
#endif

static int get_data_type_size(const TB_DataType dt) {
    assert(dt.width <= 2 && "Vector width too big!");
	
    switch (dt.type) {
		case TB_VOID:
		case TB_BOOL: return 1;
		
		case TB_I8: return 1 << dt.width;
		
		case TB_I16: return 2 << dt.width;
		
		case TB_I32:
		case TB_F32: return 4 << dt.width;
		
		case TB_I64:
		case TB_F64: return 8 << dt.width;
		
		case TB_PTR: return 8;
		
		default: tb_unreachable(); return 0;
    }
}

void x64_emit_call_patches(TB_Module* m, uint32_t* func_layout) {
    loop(i, m->max_threads) {
        TB_FunctionPatch* patches = m->call_patches[i];
		
        loop(j, arrlen(patches)) {
            TB_FunctionPatch*  p     = &patches[j];
            TB_FunctionOutput* out_f = p->source->output;
			assert(out_f && "Patch cannot be applied to function with no compiled output");
			
            uint64_t meta        = out_f->prologue_epilogue_metadata;
            uint64_t stack_usage = out_f->stack_usage;
            uint8_t* code        = out_f->code;
			
            // x64 thinks of relative addresses as being relative
            // to the end of the instruction or in this case just
            // 4 bytes ahead hence the +4.
            size_t actual_pos = func_layout[p->source - m->functions.data] +
				x64_get_prologue_length(meta, stack_usage) + p->pos + 4;
			
            *((uint32_t*)&code[p->pos]) = func_layout[p->target_id] - actual_pos;
        }
    }
}

#if _MSC_VER
_Pragma("warning (push)") _Pragma("warning (disable: 4028)")
#endif

ICodeGen x64_codegen = {
	.emit_call_patches   = x64_emit_call_patches,
	.get_prologue_length = x64_get_prologue_length,
	.get_epilogue_length = x64_get_epilogue_length,
	.emit_prologue       = x64_emit_prologue,
	.emit_epilogue       = x64_emit_epilogue,
	
	.fast_path    = x64_fast_compile_function,
	.complex_path = x64_complex_compile_function
};

#if _MSC_VER
_Pragma("warning (pop)")
#endif
