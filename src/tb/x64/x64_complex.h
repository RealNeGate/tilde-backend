
// there's an infinite number of virtual registers
// which map to some finite number of actual values
typedef enum {
	VREG_FAMILY_GPR,
	VREG_FAMILY_XMM,
	VREG_FAMILY_FLAGS
} VRegFamily;

#define VREG_STACK_POINTER (TreeVReg){ 1, VREG_FAMILY_GPR }
#define VREG_BASE_POINTER (TreeVReg){ 2, VREG_FAMILY_GPR }

// slight abstraction over x64 instructions
typedef enum {
	INST_NULL,
	
	INST_LABEL,
	INST_JUMP_IF,
	INST_JUMP,
	INST_RET,
	
	// rA = GPR rax 
	INST_EXPLICIT_GPR,
	INST_COPY_TO_GPR,
	
	// rA = rB
	INST_COPY,
	INST_IMMEDIATE,
	
	INST_COMPARE,
	
	// rA = rB OP rC
	INST_BINARY_OP,
	INST_BINARY_OP_IMM,
	
	// lea rA, [rB + (rC * scale) + disp]
	INST_LEA,
	
	// rA = rB OP [rC + (rD * scale) + disp]
	// OP [rA + (rB * scale) + disp], rC
	// OP [rA + (rB * scale) + disp], imm
	INST_FOLDED_LOAD,
	INST_FOLDED_STORE,
	INST_FOLDED_STORE_IMM,
} MachineInstType;

typedef struct {
	Scale scale;
	TreeVReg base;
	TreeVReg index; // can be 0
	int32_t disp;
} MachineInstMem;

typedef struct {
	uint16_t type;
	TB_DataType dt;
	
	union {
		struct {
			TB_Label id;
			int next_label;
		} label;
		struct {
			TB_Label target;
		} jump;
		struct {
			TB_Label if_true, if_false;
			Cond cond;
		} jump_if;
		struct {
			TreeVReg src_vreg;
			GPR gpr;
		} copy_gpr;
		struct {
			TreeVReg dst_vreg;
			GPR gpr;
		} gpr;
		struct {
			TreeVReg dst_vreg;
			TreeVReg src_vreg;
		} copy;
		struct {
			TreeVReg dst_vreg;
			uint64_t src;
		} imm;
		struct {
			uint8_t op;
			TreeVReg dst_vreg;
			TreeVReg a_vreg, b_vreg;
		} binary;
		struct {
			uint8_t op;
			TreeVReg dst_vreg;
			TreeVReg a_vreg;
			uint64_t b_imm;
		} binary_imm;
		struct {
			TreeVReg a_vreg, b_vreg;
		} compare;
		struct {
			TreeVReg dst_vreg;
			MachineInstMem src;
		} lea;
		struct {
			uint8_t op;
			
			// src (destructured MachineInstMem for better layout)
			Scale scale;
			TreeVReg base;
			TreeVReg index; // can be 0
			int32_t disp;
			
			TreeVReg dst_vreg;
		} folded_load;
		struct {
			uint8_t op;
			
			// dst (destructured MachineInstMem for better layout)
			Scale scale;
			TreeVReg base;
			TreeVReg index; // can be 0
			int32_t disp;
			
			// src
			union {
				TreeVReg src_vreg; 
				int32_t src_i32;
			};
		} folded_store;
	};
} MachineInst;

typedef struct {
	TB_Reg reg;
	TreeVReg mapping;
} PhiValue;

typedef struct {
	X64_CtxHeader header;
	int* use_count;
	
	bool is_sysv;
	uint32_t caller_usage;
	
	uint32_t phi_count;
	PhiValue* phis;
	
	// virtual registers
	uint32_t vgpr_count, vxmm_count;
	uint32_t inst_count, inst_cap;
	uint32_t last_machine_inst_label;
	
	MachineInst* insts;
	
	TreeVReg* parameters;
} X64_ComplexCtx;

typedef struct {
	size_t memory_usage;
	
	size_t phi_count;
	size_t locals_count;
	size_t return_count;
	size_t line_info_count;
	size_t label_patch_count;
} FunctionTallyComplex;

typedef struct {
	int start, end;
} LiveInterval;

static MachineInstMem compute_complex_address(X64_ComplexCtx* restrict ctx, TB_Function* f, TreeNode* tree_node);
static void add_machine_inst(X64_ComplexCtx* restrict ctx, const MachineInst* inst);
static TreeVReg isel(X64_ComplexCtx* restrict ctx, TB_Function* f, TreeNode* tree_node);

static FunctionTallyComplex tally_memory_usage_complex(TB_Function* restrict f) {
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
	tally += sizeof(X64_ComplexCtx);
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
	
	// labels
	tally += f->label_count * sizeof(uint32_t);
	tally = (tally + align_mask) & ~align_mask;
	
	// label_patches
	tally += label_patch_count * sizeof(LabelPatch);
	tally = (tally + align_mask) & ~align_mask;
	
	// ret_patches
	tally += return_count * sizeof(ReturnPatch);
	tally = (tally + align_mask) & ~align_mask;
	
	// insts
	tally += f->nodes.count * sizeof(MachineInst);
	tally = (tally + align_mask) & ~align_mask;
	
	// parameters
	tally += f->prototype->param_count * sizeof(TreeVReg);
	tally = (tally + align_mask) & ~align_mask;
	
	return (FunctionTallyComplex){
		.memory_usage = tally,
		
		.phi_count = phi_count,
		.line_info_count = line_info_count,
		.locals_count = locals_count,
		.return_count = return_count,
		.label_patch_count = label_patch_count
	};
}

static void print_machine_insts(X64_ComplexCtx* restrict ctx) {
	static const char* inst2_names[] = {
		"ADD", "AND", "OR", "SUB", "XOR", "CMP", "MOV",
		"TEST", "LEA", "IMUL", "XCHG", "MOVSXB", "MOVSXW",
		"MOVSXD", "MOVZXB", "MOVZXW"
	};
	
	static const char* cc_names[] = {
		"O", "NO", "B", "NB", "E", "NE", "BE", "A",
		"S", "NS", "P", "NP", "L", "GE", "LE", "G"
	};
	
	loop(i, ctx->inst_count) {
		MachineInst* inst = &ctx->insts[i];
		
		switch (inst->type) {
			case INST_LABEL: {
				printf("L%d:\n", inst->label.id);
				break;
			}
			case INST_JUMP: {
				printf("    JMP L%d\n", inst->jump.target);
				break;
			}
			case INST_JUMP_IF: {
				printf("    J%s L%d ELSE L%d\n", cc_names[inst->jump_if.cond], inst->jump_if.if_true, inst->jump_if.if_false);
				break;
			}
			case INST_RET: {
				printf("    RET\n");
				break;
			}
			case INST_EXPLICIT_GPR: {
				printf("    gpr:%d = $%s\n", inst->gpr.dst_vreg.value, GPR_NAMES[inst->gpr.gpr]);
				break;
			}
			case INST_COPY: {
				printf("    gpr:%d = gpr:%d\n", inst->copy.dst_vreg.value, inst->copy.src_vreg.value);
				break;
			}
			case INST_COPY_TO_GPR: {
				printf("    $%s = gpr:%d\n", GPR_NAMES[inst->copy_gpr.gpr], inst->copy_gpr.src_vreg.value);
				break;
			}
			case INST_IMMEDIATE: {
				printf("    gpr:%d = %lld\n", inst->imm.dst_vreg.value, inst->imm.src);
				break;
			}
			case INST_BINARY_OP: {
				printf("    gpr:%d = %s ", inst->binary.dst_vreg.value, inst2_names[inst->binary.op]);
				printf("gpr:%d, gpr:%d\n", inst->binary.a_vreg.value, inst->binary.b_vreg.value);
				break;
			}
			case INST_BINARY_OP_IMM: {
				printf("    gpr:%d = %s ", inst->binary_imm.dst_vreg.value, inst2_names[inst->binary_imm.op]);
				printf("gpr:%d, %lld\n", inst->binary_imm.a_vreg.value, inst->binary_imm.b_imm);
				break;
			}
			case INST_COMPARE: {
				printf("    COMPARE gpr:%d, gpr:%d\n", inst->compare.a_vreg.value, inst->compare.b_vreg.value);
				break;
			}
			case INST_FOLDED_LOAD: {
				printf("    %s ", inst2_names[inst->folded_load.op]);
				
				printf("gpr:%d, ", inst->folded_load.dst_vreg.value);
				if (inst->folded_load.index.value) {
					printf("[gpr:%d + gpr:%d * %d",
						   inst->folded_load.base.value,
						   inst->folded_load.index.value,
						   1 << inst->folded_load.scale);
				} else {
					printf("[gpr:%d", inst->folded_load.base.value);
				}
				
				if (inst->folded_load.disp) {
					printf("+ %d]\n", inst->folded_load.disp);
				} else {
					printf("]\n");
				}
				break;
			}
			case INST_FOLDED_STORE:
			case INST_FOLDED_STORE_IMM: {
				printf("    %s ", inst2_names[inst->folded_store.op]);
				if (inst->folded_store.index.value) {
					printf("[gpr:%d + gpr:%d * %d",
						   inst->folded_store.base.value,
						   inst->folded_store.index.value,
						   1 << inst->folded_store.scale);
				} else {
					printf("[gpr:%d", inst->folded_store.base.value);
				}
				
				if (inst->folded_store.disp) {
					printf("+ %d], ", inst->folded_store.disp);
				} else {
					printf("], ");
				}
				
				if (inst->type == INST_FOLDED_STORE_IMM) { 
					printf(" %d\n", inst->folded_store.src_i32);
				} else {
					printf(" gpr:%d\n", inst->folded_store.src_vreg.value);
				}
				break;
			}
			default: tb_todo();
		}
	}
}

static PhiValue* find_phi(X64_ComplexCtx* restrict ctx, TB_Reg r) {
	for (size_t i = 0; i < ctx->phi_count; i++) {
		if (ctx->phis[i].reg == r) return &ctx->phis[i];
	}
	
	return NULL;
}

static void add_machine_inst(X64_ComplexCtx* restrict ctx, const MachineInst* inst) {
	assert(ctx->inst_count+1 < ctx->inst_cap);
	ctx->insts[ctx->inst_count++] = *inst;
}

static bool is_a_32bit_immediate(X64_ComplexCtx* restrict ctx, TB_Function* f, TreeNode* tree_node, int32_t* out) {
	if (EITHER2(f->nodes.data[tree_node->reg].type, TB_UNSIGNED_CONST, TB_SIGNED_CONST)) {
		uint64_t imm = f->nodes.data[tree_node->reg].uint.value;
		
		if (imm == (int32_t)imm) {
			*out = (int32_t)imm;
			return true;
		}
	}
	
	return false;
}

static TreeVReg isel(X64_ComplexCtx* restrict ctx, TB_Function* f, TreeNode* tree_node) {
	if (tree_node->use_count) {
		// shared node, try reuse
		if (tree_node->vreg.value != 0) {
			return tree_node->vreg;
		}
	}
	
	TB_Node* restrict n = &f->nodes.data[tree_node->reg];
	TB_NodeTypeEnum reg_type = n->type;
	TB_DataType dt = n->dt;
	
	TreeVReg dst;
	switch (reg_type) {
		case TB_PHI2: {
			PhiValue* phi = find_phi(ctx, tree_node->reg);
			assert(phi);
			dst = phi->mapping;
			break;
		}
		case TB_PARAM: {
			uint32_t id = n->param.id;
			
			// 16 + (i * 8)
			if (ctx->parameters[id].value) {
				dst = ctx->parameters[id];
			} else {
				// parameter not cached in GPR or XMM, we need to load it
				// from memory
				dst = (TreeVReg){ ctx->vgpr_count++, VREG_FAMILY_GPR };
				
				MachineInst inst = {
					.type = INST_FOLDED_LOAD,
					.dt = dt,
					.folded_load = {
						.op = MOV,
						.base = VREG_STACK_POINTER,
						.disp = 16 + (id * 8),
						.dst_vreg = dst
					}
				};
				add_machine_inst(ctx, &inst);
			}
			break;
		}
		case TB_LOAD: {
			MachineInstMem addr = compute_complex_address(ctx, f, tree_node->operands[0]);
			dst = (TreeVReg){ ctx->vgpr_count++, VREG_FAMILY_GPR };
			
			MachineInst inst = {
				.type = INST_FOLDED_LOAD,
				.dt = dt,
				.folded_load = {
					.op = MOV,
					.scale = addr.scale,
					.base = addr.base,
					.index = addr.index,
					.disp = addr.disp,
					.dst_vreg = dst
				}
			};
			add_machine_inst(ctx, &inst);
			break;
		}
		case TB_UNSIGNED_CONST:
		case TB_SIGNED_CONST: {
			dst = (TreeVReg){ ctx->vgpr_count++, VREG_FAMILY_GPR };
			
			MachineInst inst = {
				.type = INST_IMMEDIATE,
				.dt = dt,
				.imm = {
					.dst_vreg = dst,
					.src = n->uint.value
				}
			};
			add_machine_inst(ctx, &inst);
			break;
		}
		case TB_AND:
		case TB_OR:
		case TB_XOR:
		case TB_ADD:
		case TB_SUB:
		case TB_MUL: {
			TreeVReg a = isel(ctx, f, tree_node->operands[0]);
			TreeVReg b = isel(ctx, f, tree_node->operands[1]);
			
			Inst2Type op;
			switch (reg_type) {
				case TB_AND: op = AND; break;
				case TB_OR:  op = OR; break;
				case TB_XOR: op = XOR; break;
				case TB_ADD: op = ADD; break;
				case TB_SUB: op = SUB; break;
				case TB_MUL: op = IMUL; break;
				default: tb_todo();
			}
			
			dst = (TreeVReg){ ctx->vgpr_count++, VREG_FAMILY_GPR };
			add_machine_inst(ctx, &(MachineInst){
								 .type = INST_BINARY_OP,
								 .dt = dt,
								 .binary = { op, dst, a, b }
							 });
			break;
		}
		default: tb_todo();
	}
	
	if (tree_node->use_count) {
		// shared node, define
		tree_node->vreg = dst;
	}
	
	return dst;
}

static MachineInstMem compute_complex_address(X64_ComplexCtx* restrict ctx, TB_Function* f, TreeNode* tree_node) {
	TB_Node* restrict n = &f->nodes.data[tree_node->reg];
	TB_NodeTypeEnum reg_type = n->type;
	//TB_DataType dt = n->dt;
	
	if (reg_type == TB_ARRAY_ACCESS) {
		TreeVReg base = isel(ctx, f, tree_node->operands[0]);
		TreeVReg index = isel(ctx, f, tree_node->operands[1]);
		uint32_t stride = n->array_access.stride;
		
		uint8_t stride_as_shift = 0;
		if (tb_is_power_of_two(stride)) {
			stride_as_shift = tb_ffs(stride) - 1;
			
			if (stride_as_shift <= 3) {
				// this section is intentionally left blank
			} else {
				tb_todo();
			}
		}
		
		return (MachineInstMem) {
			.scale = stride_as_shift,
			.base = base,
			.index = index,
			.disp = 0
		};
	} else if (reg_type == TB_MEMBER_ACCESS) {
		return (MachineInstMem) {
			.scale = SCALE_X1,
			.base = isel(ctx, f, tree_node->operands[0]),
			.index = { 0 },
			.disp = n->member_access.offset
		};
	} else {
		return (MachineInstMem){ 
			.scale = SCALE_X1,
			.base = isel(ctx, f, tree_node),
			.index = { 0 },
			.disp = 0
		};
	}
}

static void isel_top_level(X64_ComplexCtx* restrict ctx, TB_Function* f, TreeNode* tree_node) {
	TB_Node* restrict n = &f->nodes.data[tree_node->reg];
	TB_NodeTypeEnum reg_type = n->type;
	TB_DataType dt = n->dt;
	
	switch (reg_type) {
		case TB_PHI2: {
			// map phi node to shared location
			PhiValue* phi = find_phi(ctx, tree_node->reg);
			assert(phi);
			
			// generate new mapping if one doesn't exist
			TB_NodeTypeEnum op_type = f->nodes.data[tree_node->operands[0]->reg].type;
			TreeVReg mapping = phi->mapping;
			
			if (mapping.value == 0) {
				if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
					mapping = (TreeVReg){ ctx->vxmm_count++, VREG_FAMILY_XMM };
				} else {
					mapping = (TreeVReg){ ctx->vgpr_count++, VREG_FAMILY_GPR };
				}
				phi->mapping = mapping;
			}
			
			// copy correct value into the mapping
			if (tree_node->operands[0]->use_count == 0 && op_type >= TB_AND && op_type <= TB_MUL) {
				TreeNode* possible_phi = tree_node->operands[0]->operands[0];
				
				if (possible_phi->reg == tree_node->reg) {
					// choose folded instruction type
					Inst2Type inst_type = MOV;
					
					switch (op_type) {
						case TB_AND: inst_type = AND; break;
						case TB_OR:  inst_type = OR; break;
						case TB_XOR: inst_type = XOR; break;
						case TB_ADD: inst_type = ADD; break;
						case TB_SUB: inst_type = SUB; break;
						case TB_MUL: inst_type = IMUL; break;
						default: tb_todo();
					}
					
					TreeNode* rhs = tree_node->operands[0]->operands[1];
					TB_NodeTypeEnum rhs_type = f->nodes.data[rhs->reg].type;
					if (rhs->use_count == 0 && rhs_type == TB_LOAD) {
						// peephole optimization to convert binary op and folded load pair
						// into just a folded load
						MachineInstMem addr = compute_complex_address(ctx, f, rhs->operands[0]);
						
						MachineInst inst = {
							.type = INST_FOLDED_LOAD,
							.dt = dt,
							.folded_load = {
								.op = inst_type,
								.scale = addr.scale,
								.base = addr.base,
								.index = addr.index,
								.disp = addr.disp,
								.dst_vreg = mapping
							}
						};
						add_machine_inst(ctx, &inst);
					} else if (rhs->use_count == 0 && (rhs_type == TB_UNSIGNED_CONST || rhs_type == TB_SIGNED_CONST)) {
						uint64_t imm = f->nodes.data[rhs->reg].uint.value;
						
						MachineInst inst = {
							.type = INST_BINARY_OP_IMM,
							.dt = dt,
							.binary_imm = {
								.op = inst_type,
								.dst_vreg = mapping,
								.a_vreg = mapping,
								.b_imm = imm
							}
						};
						add_machine_inst(ctx, &inst);
					} else {
						TreeVReg src_vreg = isel(ctx, f, tree_node->operands[0]->operands[1]);
						
						MachineInst inst = {
							.type = INST_BINARY_OP,
							.dt = dt,
							.binary = { inst_type, mapping, mapping, src_vreg }
						};
						add_machine_inst(ctx, &inst);
					}
					break;
				}
			}
			
			TreeVReg src_vreg = isel(ctx, f, tree_node->operands[0]);
			add_machine_inst(ctx, &(MachineInst){
								 .type = INST_COPY,
								 .dt = dt,
								 .copy = { mapping, src_vreg }
							 });
			break;
		}
		case TB_STORE: {
			MachineInstMem addr = compute_complex_address(ctx, f, tree_node->operands[0]);
			
			// (store t0 (binop (load t0) t1)) => (folded-store binop t0 t1)
			Inst2Type inst_type = MOV;
			TreeNode* store_rhs = tree_node->operands[1];
			
			TB_NodeTypeEnum rhs_type = f->nodes.data[tree_node->operands[1]->reg].type;
			if (tree_node->operands[1]->use_count == 0 &&
				rhs_type >= TB_AND && rhs_type <= TB_MUL) {
				TreeNode* possible_load = tree_node->operands[1]->operands[0];
				
				if (f->nodes.data[possible_load->reg].type == TB_LOAD &&
					possible_load->operands[0] == tree_node->operands[0]) {
					// choose folded instruction type
					switch (rhs_type) {
						case TB_AND: inst_type = AND; break;
						case TB_OR:  inst_type = OR; break;
						case TB_XOR: inst_type = XOR; break;
						case TB_ADD: inst_type = ADD; break;
						case TB_SUB: inst_type = SUB; break;
						case TB_MUL: inst_type = IMUL; break;
						default: tb_todo();
					}
					
					store_rhs = tree_node->operands[1]->operands[1];
				}
			}
			
			MachineInst inst = { 0 };
			inst.dt = dt;
			inst.folded_store.scale = addr.scale;
			inst.folded_store.base = addr.base;
			inst.folded_store.index = addr.index;
			inst.folded_store.disp = addr.disp;
			
			int32_t imm;
			if (is_a_32bit_immediate(ctx, f, store_rhs, &imm)) {
				inst.type = INST_FOLDED_STORE_IMM;
				inst.folded_store.src_i32 = imm;
			} else {
				inst.type = INST_FOLDED_STORE;
				inst.folded_store.src_vreg = isel(ctx, f, store_rhs);
			}
			add_machine_inst(ctx, &inst);
			break;
		} 
		case TB_RET: {
			if (tree_node->operands[0]) {
				TreeVReg src_vreg = isel(ctx, f, tree_node->operands[0]);
				
				if (TB_IS_FLOAT_TYPE(dt.type) || dt.width) {
					tb_todo();
				} else {
					add_machine_inst(ctx, &(MachineInst){
										 .type = INST_COPY_TO_GPR,
										 .dt = dt,
										 .copy_gpr = { src_vreg, RAX }
									 });
				}
			}
			
			add_machine_inst(ctx, &(MachineInst) { .type = INST_RET });
			break;
		}
		case TB_LABEL: {
			break;
		}
		case TB_GOTO: {
			add_machine_inst(ctx, &(MachineInst) {
								 .type = INST_JUMP,
								 .jump = { n->goto_.label }
							 });
			break;
		}
		case TB_IF: {
			TB_Label if_true = n->if_.if_true;
			TB_Label if_false = n->if_.if_false;
			
			// tile to directly use flags instead of vreg
			TB_NodeTypeEnum cond_type = f->nodes.data[tree_node->operands[0]->reg].type;
			if (cond_type >= TB_CMP_EQ && cond_type <= TB_CMP_FLE) {
				TreeVReg a = isel(ctx, f, tree_node->operands[0]->operands[0]);
				TreeVReg b = isel(ctx, f, tree_node->operands[0]->operands[1]);
				add_machine_inst(ctx, &(MachineInst) {
									 .type = INST_COMPARE,
									 .compare = { a, b }
								 });
				
				Cond cc;
				switch (cond_type) {
					case TB_CMP_EQ: cc = E; break;
					case TB_CMP_NE: cc = NE; break;
					case TB_CMP_SLT: cc = L; break;
					case TB_CMP_SLE: cc = LE; break;
					case TB_CMP_ULT: cc = B; break;
					case TB_CMP_ULE: cc = BE; break;
					default: tb_todo();
				}
				
				add_machine_inst(ctx, &(MachineInst) {
									 .type = INST_JUMP_IF,
									 .jump_if = { if_true, if_false, cc }
								 });
			} else {
				tb_todo();
			}
			break;
		}
		default: tb_todo();
	}
}

#define idef(r) if (intervals[r].start == 0) intervals[r].start = intervals[r].end = i
#define iuse(r) intervals[r].end = i
static void compute_live_intervals(X64_ComplexCtx* restrict ctx, TB_Function* restrict f, LiveInterval* intervals) {
	loop(i, ctx->vgpr_count) {
		intervals[i].start = 0;
	}
	
	loop(i, ctx->inst_count) {
		MachineInst* inst = &ctx->insts[i];
		
		switch (inst->type) {
			case INST_LABEL: break;
			case INST_JUMP: break;
			case INST_JUMP_IF: break;
			case INST_RET: break;
			case INST_EXPLICIT_GPR: {
				idef(inst->gpr.dst_vreg.value);
				break;
			}
			case INST_COPY: {
				idef(inst->copy.dst_vreg.value);
				iuse(inst->copy.src_vreg.value);
				break;
			}
			case INST_COPY_TO_GPR: {
				iuse(inst->copy_gpr.src_vreg.value);
				break;
			}
			case INST_IMMEDIATE: {
				idef(inst->imm.dst_vreg.value);
				break;
			}
			case INST_BINARY_OP: {
				idef(inst->binary.dst_vreg.value);
				iuse(inst->binary.a_vreg.value);
				iuse(inst->binary.b_vreg.value);
				break;
			}
			case INST_BINARY_OP_IMM: {
				idef(inst->binary_imm.dst_vreg.value);
				iuse(inst->binary_imm.dst_vreg.value);
				iuse(inst->binary_imm.a_vreg.value);
				break;
			}
			case INST_COMPARE: {
				iuse(inst->compare.a_vreg.value);
				iuse(inst->compare.b_vreg.value);
				break;
			}
			case INST_FOLDED_LOAD: {
				iuse(inst->folded_load.dst_vreg.value);
				iuse(inst->folded_load.base.value);
				iuse(inst->folded_load.index.value);
				break;
			}
			case INST_FOLDED_STORE: {
				iuse(inst->folded_store.base.value);
				iuse(inst->folded_store.index.value);
				iuse(inst->folded_store.src_vreg.value);
				break;
			}
			case INST_FOLDED_STORE_IMM: {
				iuse(inst->folded_store.index.value);
				iuse(inst->folded_store.base.value);
				break;
			}
			default: tb_todo();
		}
	}
}
#undef idef
#undef iuse

static GPR complex_alloc_gpr(X64_ComplexCtx* restrict ctx, TB_Function* restrict f, uint16_t* gpr_allocator, LiveInterval* interval, int i, TreeVReg dst_vreg) {
	assert(dst_vreg.family == VREG_FAMILY_GPR);
	assert(*gpr_allocator != 0xFFFF && "TODO: Implement spilling");
	loop_range(j, i, interval->end+1) {
		MachineInst* inst = &ctx->insts[j];
		
		if (inst->type == INST_COPY_TO_GPR && 
			(*gpr_allocator & (1u << inst->copy_gpr.gpr)) &&
			inst->copy_gpr.src_vreg.value == dst_vreg.value) {
			// we can avoid a move if we just use this register directly
			// TODO(NeGate): implement a weight system in case the same value
			// is used across a bunch of values
			*gpr_allocator |= (1u << inst->copy_gpr.gpr);
			return inst->copy_gpr.gpr;
		}
	}
	
	int search = tb_ffs(~*gpr_allocator);
	
	GPR gpr = (GPR)(search-1);
	*gpr_allocator |= (1u << gpr);
	
	return gpr;
}

TB_FunctionOutput x64_complex_compile_function(TB_CompiledFunctionID id, TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id) {
	s_local_thread_id = local_thread_id;
	s_compiled_func_id = id;
	
	TB_TemporaryStorage* tls = tb_tls_allocate();
	
	////////////////////////////////
	// Allocate all the memory we'll need
	////////////////////////////////
	bool is_ctx_heap_allocated = false;
	X64_ComplexCtx* restrict ctx = NULL;
	{
		// if we can't fit our memory usage into memory, we fallback
		FunctionTallyComplex tally = tally_memory_usage_complex(f);
		is_ctx_heap_allocated = !tb_tls_can_fit(tls, tally.memory_usage);
		
		size_t ctx_size = sizeof(X64_ComplexCtx);
		if (is_ctx_heap_allocated) {
			//printf("Could not allocate x64 code gen context: using heap fallback. (%zu bytes)\n", tally.memory_usage);
			ctx = calloc(1, ctx_size);
			
			ctx->header.labels = malloc(f->label_count * sizeof(uint32_t));
			ctx->header.label_patches = malloc(tally.label_patch_count * sizeof(LabelPatch));
			ctx->header.ret_patches = malloc(tally.return_count * sizeof(ReturnPatch));
			
			ctx->use_count = malloc(f->nodes.count * sizeof(TB_Reg));
			ctx->phis = malloc(tally.phi_count * sizeof(PhiValue));
			ctx->insts = malloc(f->nodes.count * 2 * sizeof(MachineInst));
			ctx->parameters = malloc(f->prototype->param_count * sizeof(TreeVReg));
		} else {
			ctx = tb_tls_push(tls, ctx_size);
			memset(ctx, 0, ctx_size);
			
			ctx->header.labels = tb_tls_push(tls, f->label_count * sizeof(uint32_t));
			ctx->header.label_patches = tb_tls_push(tls, tally.label_patch_count * sizeof(LabelPatch));
			ctx->header.ret_patches = tb_tls_push(tls, tally.return_count * sizeof(ReturnPatch));
			
			ctx->use_count = tb_tls_push(tls, f->nodes.count * sizeof(TB_Reg));
			ctx->phis = tb_tls_push(tls, tally.phi_count * sizeof(PhiValue));
			ctx->insts = tb_tls_push(tls, f->nodes.count * 2 * sizeof(MachineInst));
			ctx->parameters = tb_tls_push(tls, f->prototype->param_count * sizeof(TreeVReg));
		}
		
		ctx->header.start_out = ctx->header.out = out;
		ctx->header.f = f;
		ctx->header.function_id = f - f->module->functions.data;
		
		ctx->inst_cap = f->nodes.count*2;
		
		// virtual registers keep 0 as a NULL slot
		ctx->vgpr_count = 1;
		ctx->vxmm_count = 1;
		
		ctx->is_sysv = EITHER2(f->module->target_system, TB_SYSTEM_LINUX, TB_SYSTEM_MACOS);
		
		f->line_count = 0;
		f->lines = tb_platform_arena_alloc(tally.line_info_count * sizeof(TB_Line));
	}
	
	////////////////////////////////
	// Analyze function for stack, live intervals and phi nodes
	////////////////////////////////
	tb_function_calculate_use_count(f, ctx->use_count);
	
	// Create phi lookup table for later evaluation stages
	// and calculate the maximum parameter usage for a call
	size_t caller_usage = 0;
	TB_FOR_EACH_NODE(n, f) {
		if (n->type == TB_PHI2) {
			ctx->phis[ctx->phi_count++] = (PhiValue){
				n - f->nodes.data
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
	
	// Allocate local and parameter stack slots
	tb_function_print(f, tb_default_print_callback, stdout);
	printf("\n\n\n");
	
	const TB_FunctionPrototype* restrict proto = f->prototype;
	
	// entry label
	add_machine_inst(ctx, &(MachineInst) { .type = INST_LABEL, .label = { 0 } });
	
	ctx->vgpr_count = 3 + proto->param_count;
	add_machine_inst(ctx, &(MachineInst){
						 .type = INST_EXPLICIT_GPR,
						 .dt = TB_TYPE_PTR, .gpr = { VREG_STACK_POINTER, RSP }
					 });
	
	add_machine_inst(ctx, &(MachineInst){
						 .type = INST_EXPLICIT_GPR,
						 .dt = TB_TYPE_PTR, .gpr = { VREG_BASE_POINTER, RBP }
					 });
	
	loop(i, proto->param_count) {
		TB_DataType dt = proto->params[i];
		
		// Allocate space in stack
		int size = get_data_type_size(dt);
		STACK_ALLOC(size, size);
		assert(size <= 8 && "Parameter too big");
		
		if (dt.width || TB_IS_FLOAT_TYPE(dt.type)) {
			tb_todo();
		} else {
			// gpr parameters
			TreeVReg dst = { 3+i, VREG_FAMILY_GPR };
			
			if (ctx->is_sysv && i < 6) {
				add_machine_inst(ctx, &(MachineInst){
									 .type = INST_EXPLICIT_GPR,
									 .dt = dt, .gpr = { dst, SYSV_GPR_PARAMETERS[i] }
								 });
				
				ctx->parameters[i] = dst;
			} else if (i < 4) {
				add_machine_inst(ctx, &(MachineInst){
									 .type = INST_EXPLICIT_GPR,
									 .dt = dt, .gpr = { dst, WIN64_GPR_PARAMETERS[i] }
								 });
				
				ctx->parameters[i] = dst;
			} else {
				ctx->parameters[i] = (TreeVReg){ 0 };
			}
		}
	}
	
	////////////////////////////////
	// Evaluate each basic block
	////////////////////////////////
	TB_Reg bb = 1;
	TreeNodeArena tree = { 0 };
	do {
		assert(f->nodes.data[bb].type == TB_LABEL);
		TB_Node* start = &f->nodes.data[bb];
		
		TB_Reg bb_end = start->label.terminator;
		TB_Node* end = &f->nodes.data[bb_end];
		
		// Generate expression tree
		TreeNode* node = tb_tree_generate(&tree, f, ctx->use_count, bb, bb_end);
		
		// Identify next BB
		TB_Node* next_bb = end;
		if (end->type != TB_LABEL) next_bb = &f->nodes.data[next_bb->next];
		
		TB_Reg next_bb_reg = next_bb - f->nodes.data;
		
		// Walk expression tree
		if (start->label.id) {
			add_machine_inst(ctx, &(MachineInst) { .type = INST_LABEL, .label = { start->label.id } });
			
			ctx->insts[ctx->last_machine_inst_label].label.next_label = ctx->inst_count-1;
			ctx->last_machine_inst_label = ctx->inst_count-1;
		}
		
		// Instruction selection
		while (node) {
			assert(node->reg == TB_NULL_REG);
			
			isel_top_level(ctx, f, node->operands[0]);
			node = node->operands[1];
		}
		
		// Next Basic block
		tb_tree_clear(&tree);
		bb = next_bb_reg;
	} while (bb != TB_NULL_REG);
	
	tb_tree_free(&tree);
	print_machine_insts(ctx);
	
	// TODO(NeGate): Register allocation
	// we just gotta map each VGPR into some Val
	Val* mappings = calloc(1, ctx->vgpr_count * sizeof(Val));
	LiveInterval* intervals = malloc(ctx->vgpr_count * sizeof(LiveInterval));
	
	compute_live_intervals(ctx, f, intervals);
	
	int next_label = 0;
	uint16_t gpr_allocator = 0;
	loop(i, ctx->inst_count) {
		MachineInst* inst = &ctx->insts[i];
		switch (inst->type) {
			case INST_IMMEDIATE: {
				TreeVReg dst = inst->imm.dst_vreg;
				
				if (intervals[dst.value].end > next_label) {
					GPR gpr = complex_alloc_gpr(ctx, f, &gpr_allocator, &intervals[dst.value], i, dst);
					mappings[dst.value] = val_gpr(TB_I64, gpr);
				} else {
					mappings[dst.value] = val_imm(TB_TYPE_I64, 0);
				}
				break;
			}
			
			case INST_COPY: {
				TreeVReg dst = inst->copy.dst_vreg;
				
				GPR gpr = complex_alloc_gpr(ctx, f, &gpr_allocator, &intervals[dst.value], i, dst);
				mappings[inst->gpr.dst_vreg.value] = val_gpr(TB_I64, gpr);
				break;
			}
			
			case INST_EXPLICIT_GPR: {
				mappings[inst->gpr.dst_vreg.value] = val_gpr(TB_I64, inst->gpr.gpr);
				gpr_allocator |= inst->gpr.gpr;
				break;
			}
			
			case INST_LABEL:
			next_label = ctx->insts[i].label.next_label;
			break;
			
			default: break;
		}
	}
	
	// verify
	bool has_unknowns = false;
	loop_range(i, 1, ctx->vgpr_count) {
		if (mappings[i].type == VAL_NONE) {
			printf("x64 complex: unknown gpr:%zu\n", i);
			has_unknowns = true;
		}
	}
	
	if (has_unknowns) {
		printf("x64 complex: regalloc left unknowns\n");
		abort();
	}
	
	// Generate machine code
	loop(i, ctx->inst_count) {
		MachineInst* inst = &ctx->insts[i];
		switch (inst->type) {
			case INST_LABEL: {
				// Define label position
				ctx->header.labels[inst->label.id] = GET_CODE_POS();
				
#if !TB_STRIP_LABELS
				if (inst->label.id) {
					tb_emit_label_symbol(f->module,
										 f - f->module->functions.data,
										 inst->label.id,
										 code_pos());
				}
#endif
				break;
			}
			case INST_COPY: {
				TreeVReg dst = inst->copy.dst_vreg;
				TreeVReg src = inst->copy.src_vreg;
				
				// TODO(NeGate): implement vector or float MOVs
				if (mappings[dst.value].type == VAL_GPR &&
					mappings[src.value].type == VAL_IMM && 
					mappings[src.value].imm == 0) {
					INST2(XOR, &mappings[dst.value], &mappings[dst.value], TB_I32);
				} else {
					INST2(XOR, &mappings[dst.value], &mappings[src.value], TB_I64);
				}
				break;
			}
			case INST_COMPARE: {
				TreeVReg dst = inst->copy.dst_vreg;
				TreeVReg src = inst->copy.src_vreg;
				
				// TODO(NeGate): implement vector or float CMPs
				INST2(CMP, &mappings[dst.value], &mappings[src.value], TB_I64);
				break;
			}
			case INST_JUMP_IF: {
				JCC(inst->jump_if.cond, inst->jump_if.if_true);
				JMP(inst->jump_if.if_false);
				break;
			}
			case INST_JUMP: {
				JMP(inst->jump.target);
				break;
			}
			default: break;
		}
	}
	
	// Tally up any saved XMM registers
	ctx->header.stack_usage += tb_popcount((ctx->header.regs_to_save >> 16) & 0xFFFF) * 16;
	ctx->header.stack_usage = align_up(ctx->header.stack_usage + 8, 16) + 8;
	
	////////////////////////////////
	// Evaluate internal relocations (return and labels)
	////////////////////////////////
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
		free(ctx->header.labels);
		free(ctx->header.label_patches);
		free(ctx->header.ret_patches);
		
		free(ctx->use_count);
		free(ctx->phis);
		
		free(ctx->insts);
		free(ctx->parameters);
		free(ctx);
	}
	return func_out;
}
