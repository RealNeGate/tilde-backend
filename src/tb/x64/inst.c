// This file is responsible for generating normal x64 instructions

inline static uint8_t mod_rx_rm(uint8_t mod, uint8_t rx, uint8_t rm) {
	return ((mod & 3) << 6) | ((rx & 7) << 3) | (rm & 7);
}

inline static uint8_t rex(bool is_64bit, uint8_t rx, uint8_t base, uint8_t index) {
	return 0x40 | (is_64bit ? 8 : 0) | (base >> 3) | ((index >> 3) << 1) | ((rx >> 3) << 2);
}

inline static void inst1(Ctx* ctx, Inst1 op, Val* r) {
	if (r->type == VAL_GPR) {
		emit(rex(true, 0x00, r->gpr, 0x00));
		emit(0xF7);
		emit(mod_rx_rm(MOD_DIRECT, op, r->gpr));
	} else if (r->type == VAL_MEM) {
		GPR base = r->mem.base;
		GPR index = r->mem.index;
		uint8_t scale = r->mem.scale;
		int32_t disp = r->mem.disp;
		
		bool needs_index = (index != GPR_NONE) || (base & 7) == RSP;
		
		emit(rex(true, 0x00, base, index != GPR_NONE ? index : 0));
		emit(0xF7);
		
		// If it needs an index, it'll put RSP into the base slot
		// and write the real base into the SIB
		uint8_t mod = MOD_INDIRECT_DISP32;
		if (disp == 0) mod = MOD_INDIRECT_DISP8;
		else if (disp == (int8_t)disp) mod = MOD_INDIRECT_DISP8;
		
		emit(mod_rx_rm(mod, op, needs_index ? RSP : base));
		if (needs_index) {
			emit(mod_rx_rm(scale, (base & 7) == RSP ? RSP : index, base));
		}
		
		if (mod == MOD_INDIRECT_DISP8) emit((int8_t)disp);
		else if (mod == MOD_INDIRECT_DISP32) emit4((int32_t)disp);
	} else tb_unreachable();
}

inline static void inst2(Ctx* ctx, int op, const Val* a, const Val* b, int dt_type) {
	assert(op < (sizeof(inst2_tbl) / sizeof(inst2_tbl[0])));
	const Inst2* inst = &inst2_tbl[op];
	
	bool dir = b->type == VAL_MEM;
	if (dir) tb_swap(a, b);
	//  || inst->op == 0xAF
	
	// operand size
	uint8_t sz = (dt_type != TB_I8);
	
	// All instructions that go through here are
	// based on the ModRxRm encoding so we do need
	// an RX and an RM (base, index, shift, disp)
	uint8_t base = 0;
	uint8_t rx = 0xFF;
	if (inst->ext == EXT_NONE || inst->ext == EXT_DEF) {
		assert(dt_type == TB_I8 || dt_type == TB_I16 || dt_type == TB_I32 || dt_type == TB_I64 || dt_type == TB_PTR);
		
		// the destination can only be a GPR, no direction flag
		bool is_gpr_only_dst = (inst->op & 1);
		bool dir_flag = dir != is_gpr_only_dst;
		
		// Address size prefix
		if (dt_type == TB_I16) {
			emit(0x66);
		}
		
		// RX
		if (b->type == VAL_GPR) rx = b->gpr;
		else if (b->type == VAL_IMM) rx = inst->rx_i;
		else __builtin_unreachable();
		
		// RM & REX
		bool is_64bit = (dt_type == TB_I64 || dt_type == TB_PTR);
		
		if (a->type == VAL_GPR) {
			base = a->gpr;
			
			if (base >= 8 || rx >= 8 || is_64bit) {
				emit(rex(is_64bit, rx, base, 0));
			}
		}
		else if (a->type == VAL_MEM) {
			base = a->mem.base;
			
			uint8_t rex_index = (a->mem.index != GPR_NONE ? a->mem.index : 0);
			if (base >= 8 || rx >= 8 || rex_index >= 8 || is_64bit) {
				emit(rex(is_64bit, rx, base, rex_index));
			}
		}
		else __builtin_unreachable();
		
		// Opcode
		if (inst->ext == EXT_DEF) {
			// DEF instructions can only be 32bit and 64bit
			assert(dt_type == TB_I32 || dt_type == TB_I64 || dt_type == TB_PTR);
			emit(0x0F);
		}
		
		if (b->type == VAL_IMM && inst->op_i == 0 && inst->rx_i == 0) {
			// No immediate version
			__builtin_unreachable();
		}
		
		// Immediates have a custom opcode
		uint8_t op = b->type == VAL_IMM ? inst->op_i : inst->op;
		emit(op | sz | (dir_flag ? 2 : 0));
	}
	else if (inst->ext == EXT_SSE_SS || inst->ext == EXT_SSE_PS) {
		assert(b->type != VAL_IMM);
		
		// SSE also extends to integer vectors so this is wrong
		//assert(dt_type == TB_F32 || dt_type == TB_F64);
		
		bool is_vec_mov = inst->op == 0x10 || inst->op == 0x28;
		
		// TODO(NeGate): normal SSE instructions don't support store mode, except MOV__
		if (!is_vec_mov && a->type == VAL_MEM) assert(dir); 
		
		if (a->type == VAL_MEM) base = a->mem.base;
		else base = a->xmm;
		rx = b->xmm;
		
		// This is pretty nasty but essentially the normal SSE instructions are always
		// in the flipped form (except for MOV__)
		if (!is_vec_mov && a->type != VAL_MEM) tb_swap(base, rx);
		
		if (rx >= 8 || base >= 8) {
			emit(rex(true, rx, base, 0));
		}
		
		if (inst->ext == EXT_SSE_SS) {
			emit(0xF3);
		}
		
		emit(0x0F);
		emit(is_vec_mov ? inst->op + !dir : inst->op);
	}
	else tb_unreachable();
	
	// We forgot a case!
	assert(rx != 0xFF);
	
	// Operand encoding
	if (a->type == VAL_GPR || a->type == VAL_XMM) {
		emit(mod_rx_rm(MOD_DIRECT, rx, base));
	} else if (a->type == VAL_MEM) {
		GPR index = a->mem.index;
		uint8_t scale = a->mem.scale;
		int32_t disp = a->mem.disp;
		
		bool needs_index = (index != GPR_NONE) || (base & 7) == RSP;
		
		// If it needs an index, it'll put RSP into the base slot
		// and write the real base into the SIB
		uint8_t mod = MOD_INDIRECT_DISP32;
		if (disp == 0) mod = MOD_INDIRECT;
		else if (disp == (int8_t)disp) mod = MOD_INDIRECT_DISP8;
		
		emit(mod_rx_rm(mod, rx, needs_index ? RSP : base));
		if (needs_index) {
			emit(mod_rx_rm(scale, (base & 7) == RSP ? RSP : index, base));
		}
		
		if (mod == MOD_INDIRECT_DISP8) emit((int8_t)disp);
		else if (mod == MOD_INDIRECT_DISP32) emit4(disp);
	} else tb_unreachable();
	
	if (b->type == VAL_IMM) {
		if (dt_type == TB_I8) {
			assert(b->imm == (int8_t)b->imm);
			emit((int8_t)b->imm);
		} else if (dt_type == TB_I16) {
			assert(b->imm == (int16_t)b->imm);
			emit2((int16_t)b->imm);
		} else {
			assert(dt_type == TB_I32 || dt_type == TB_I64 || dt_type == TB_PTR);
			emit4((int32_t)b->imm);
		}
	}
}

inline static void jcc(Ctx* ctx, Cond cc, int label) {
	size_t code_pos = ctx->out - ctx->start_out;
	ctx->label_patches[ctx->label_patch_count++] = (LabelPatch){
		.pos = code_pos + 2, .target_lbl = label
	};
	
	emit(0x0F);
	emit(0x80 + (uint8_t)cc);
	emit4(0x0);
}

inline static void jmp(Ctx* ctx, int label) {
	size_t code_pos = ctx->out - ctx->start_out;
	ctx->label_patches[ctx->label_patch_count++] = (LabelPatch){
		.pos = code_pos + 1, .target_lbl = label 
	};
	
	emit(0xE9);
	emit4(0x0);
}
