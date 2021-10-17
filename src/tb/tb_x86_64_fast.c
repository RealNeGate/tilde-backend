#define TB_INTERNAL
#include "tb.h"
#include "tb_x86_64.h"

typedef enum X64_Scale {
	X64_Scale_X1,
	X64_Scale_X2,
	X64_Scale_X4,
	X64_Scale_X8
} X64_Scale;

typedef enum X64_ValueType {
	X64_None,
	X64_Integer,
	X64_Integer128,
    X64_Local,
	X64_Array,
	X64_Flags,
    
    X64_CACHED_XMM,
    
	X64_CachedGPR,
	X64_CachedGPRPair
} X64_ValueType;

typedef struct X64_Value {
	X64_ValueType type : 8;
	// Has the cache deviated from the actual storage value
	bool is_mutated;
	TB_DataType dt;
	union {
		X64_GPR gpr;
        X64_XMM xmm;
		X64_Cond cond;
        
		uint64_t num;
		TB_Int128 big_num;
		int32_t local;
		struct {
			X64_GPR gpr_lo;
			X64_GPR gpr_hi;
		} gpr_pair;
		struct {
			X64_GPR base;
			X64_GPR index;
			X64_Scale scale;
			int32_t disp;
		} array;
	};
} X64_Value;

typedef struct X64_RegisterDesc {
	TB_Register bound_value;
} X64_RegisterDesc;

typedef struct X64_PhiValue {
	TB_Register reg;
	TB_Register storage_a;
	TB_Register storage_b;
	X64_Value value;
} X64_PhiValue;

typedef struct X64_Context {
	size_t phi_count;
	size_t* intervals;
	X64_Value* cached_values;
    
	X64_RegisterDesc gpr_desc[16];
	X64_RegisterDesc xmm_desc[16];
	X64_PhiValue phis[];
} X64_Context;

typedef struct X64_LabelPatch {
	int base;
	int pos;
	int target_lbl;
} X64_LabelPatch;

enum {
	X64_Indirect = 0,			// [rax]
	X64_IndirectDisp8 = 1,		// [rax + disp8]
	X64_IndirectDisp32 = 2,		// [rax + disp32]
	X64_Direct = 3,				// rax
};

typedef struct X64_NormalInst {
	uint8_t op;
    
	// IMMEDIATES
	uint8_t op_i;
	uint8_t rx_i;
    
    uint8_t ext;
} X64_NormalInst;

enum {
	X64_ADD, X64_AND, X64_SUB, X64_XOR, X64_CMP, X64_MOV,
    X64_LEA, X64_IMUL, X64_MOVSX, X64_MOVZX,
    
    X64_MOVSS, X64_ADDSS, X64_MULSS, X64_SUBSS, X64_DIVSS,
    X64_CMPSS
};

enum {
    // Normal
	X64_EXT_NONE,
    
    // DEF instructions have a 0F prefix
	X64_EXT_DEF,
    
    // SSE instructions have a F3 0F prefix
    X64_EXT_SSE,
};

static const X64_NormalInst insts[] = {
	[X64_ADD] = { 0x00, 0x80, 0x00 },
	[X64_AND] = { 0x20, 0x80, 0x04 },
	[X64_SUB] = { 0x2A, 0x80, 0x05 },
	[X64_XOR] = { 0x30, 0x80, 0x06 },
	[X64_CMP] = { 0x38, 0x80, 0x07 },
	[X64_MOV] = { 0x88, 0xC6, 0x00 },
    
	[X64_LEA] = { 0x8C },
    
	[X64_IMUL] = { 0xAE, .ext = X64_EXT_DEF },
	[X64_MOVSX] = { 0xBE, .ext = X64_EXT_DEF },
	[X64_MOVZX] = { 0xB6, .ext = X64_EXT_DEF },
    
	[X64_MOVSS] = { 0x10, .ext = X64_EXT_SSE },
	[X64_ADDSS] = { 0x58, .ext = X64_EXT_SSE },
	[X64_MULSS] = { 0x59, .ext = X64_EXT_SSE },
	[X64_SUBSS] = { 0x5C, .ext = X64_EXT_SSE },
	[X64_DIVSS] = { 0x5E, .ext = X64_EXT_SSE },
	[X64_CMPSS] = { 0xC2, .ext = X64_EXT_SSE }
};

static const X64_GPR GPR_PARAMETERS[] = {
	X64_RCX, X64_RDX, X64_R8, X64_R9
};

static const X64_GPR GPR_PRIORITY_LIST[] = {
	X64_RAX, X64_RCX, X64_RDX, X64_R8,
	X64_R9, X64_R10, X64_R11, X64_RDI,
	X64_RSI, X64_RBX, X64_R12, X64_R13,
	X64_R14, X64_R15
};

static void x64_reserve_parameters(TB_Function* f, X64_Context* ctx, TB_Emitter* out);

#if 0
static bool x64_is_valid_dst(const X64_Value* v) {
	return v->type == X64_CachedGPR || v->type == X64_Local || v->type == X64_Array;
}

static bool x64_is_immediate(const X64_Value* v) {
	return v->type == X64_Integer || v->type == X64_Integer128;
}
#endif

static bool x64_is_mem_op(const X64_Value* v) {
	return v->type == X64_Local || v->type == X64_Array;
}

static X64_PhiValue* x64_find_phi(X64_Context* ctx, TB_Register r) {
	for (size_t i = 0; i < ctx->phi_count; i++) {
		if (ctx->phis[i].reg == r) return &ctx->phis[i];
	}
    
	return NULL;
}

// Searches by the values the PHI node could have
static X64_PhiValue* x64_find_phi_values(X64_Context* ctx, TB_Register r) {
	for (size_t i = 0; i < ctx->phi_count; i++) {
		if (ctx->phis[i].storage_a == r || ctx->phis[i].storage_b == r) return &ctx->phis[i];
	}
    
	return NULL;
}

static X64_Value x64_allocate_temp_gpr(X64_Context* ctx, TB_DataType dt) {
	for (unsigned int i = 0; i < 14; i++) {
		X64_GPR gpr = GPR_PRIORITY_LIST[i];
        
		if (ctx->gpr_desc[gpr].bound_value == 0) {
			ctx->gpr_desc[gpr].bound_value = SIZE_MAX;
            
			return (X64_Value) {
				.type = X64_CachedGPR,
                .dt = dt,
                .gpr = gpr
			};
		}
	}
    
	// Spill GPRs
	abort();
}

#if 0
static X64_Value x64_allocate_xmm(X64_Context* ctx, TB_Register reg, TB_DataType dt) {
	for (unsigned int i = 0; i < 16; i++) {
		if (ctx->xmm_desc[i].bound_value == 0) {
			ctx->xmm_desc[i].bound_value = reg;
            
			return (X64_Value) {
				.type = X64_CACHED_XMM,
                .dt = dt,
                .xmm = i
			};
		}
	}
    
	// Spill XMMs
	abort();
}
#endif

static X64_Value x64_allocate_gpr(X64_Context* ctx, TB_Register reg, TB_DataType dt) {
	for (unsigned int i = 0; i < 14; i++) {
		X64_GPR gpr = GPR_PRIORITY_LIST[i];
        
		if (ctx->gpr_desc[gpr].bound_value == 0) {
			ctx->gpr_desc[gpr].bound_value = reg;
            
			return (X64_Value) {
				.type = X64_CachedGPR,
                .dt = dt,
                .gpr = gpr
			};
		}
	}
    
	// Spill GPRs
	abort();
}

#if 0
static X64_Value x64_allocate_gpr_pair(X64_Context* ctx, TB_Register reg, TB_DataType dt) {
	X64_Value lo = x64_allocate_gpr(ctx, reg, TB_TYPE_I64(1));
	X64_Value hi = x64_allocate_gpr(ctx, reg, TB_TYPE_I64(1));
    
	return (X64_Value) {
		.type = X64_CachedGPRPair,
        .dt = dt,
        .gpr_pair = { lo.gpr, hi.gpr }
	};
}
#endif

static void x64_free_gpr(X64_Context* ctx, uint8_t gpr) {
	ctx->gpr_desc[gpr].bound_value = 0;
}

static uint8_t x64_inst_mod_rx_rm(uint8_t mod, uint8_t rx, uint8_t rm) {
	return ((mod & 3) << 6) | ((rx & 7) << 3) | (rm & 7);
}

static uint8_t x64_inst_rex(bool is_64bit, uint8_t rx, uint8_t base, uint8_t index) {
	return 0x40 | (is_64bit ? 8 : 0) | (base >> 3) | ((index >> 3) << 1) | ((rx >> 3) << 2);
}

static void x64_inst_op64_ri32(TB_Emitter* out, uint8_t opcode, uint8_t rx, uint8_t dst, uint32_t imm) {
	tb_out_reserve(out, 7);
    
	tb_out1b_UNSAFE(out, x64_inst_rex(true, rx, dst, 0));
	tb_out1b_UNSAFE(out, opcode | 1);
	tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Direct, rx, dst));
    
	tb_out1b_UNSAFE(out, imm & 0xFF);
	tb_out1b_UNSAFE(out, (imm >> 8) & 0xFF);
	tb_out1b_UNSAFE(out, (imm >> 16) & 0xFF);
	tb_out1b_UNSAFE(out, (imm >> 24) & 0xFF);
}

static void x64_inst_op(TB_Emitter* out, int dt_type, const X64_NormalInst* inst, X64_Value* a, X64_Value* b) {
	bool dir = x64_is_mem_op(b);
	// Both arguments cannot be memory operands
	assert(!x64_is_mem_op(a) || !x64_is_mem_op(b));
    
	if (dir || inst->op == 0xAE) tb_swap(a, b);
	tb_out_reserve(out, 16);
    
	// operand size prefix
	uint8_t sz = (dt_type != TB_I8);
	bool is_64bit = (dt_type == TB_I64 || dt_type == TB_PTR);
    
    if (inst->ext == X64_EXT_NONE) {
        assert((dt_type == TB_I8 || dt_type == TB_I16 || dt_type == TB_I32 || dt_type == TB_I64 || dt_type == TB_PTR));
        
        if (dt_type == TB_I8 || dt_type == TB_I16) tb_out1b_UNSAFE(out, 0x66);
    } 
    else if (inst->ext == X64_EXT_DEF) {
        assert(dt_type == TB_I32 || dt_type == TB_I64 || dt_type == TB_PTR);
        
        tb_out1b_UNSAFE(out, 0x0F);
    } 
    else if (inst->ext == X64_EXT_SSE) {
        assert(dt_type == TB_F32 || dt_type == TB_F64);
        
        tb_out1b_UNSAFE(out, 0xF3);
        tb_out1b_UNSAFE(out, 0x0F);
    }
    else abort();
    
	if (a->type == X64_CachedGPR && b->type == X64_CachedGPR) {
		// SPECIAL CASE IMUL
		if (b->gpr >= 8 || is_64bit) tb_out1b_UNSAFE(out, x64_inst_rex(is_64bit, b->gpr, a->gpr, 0));
        
        // SPECIAL CASE IMUL
        if (inst->op == 0xAE) {
            assert(!dir);
            tb_out1b_UNSAFE(out, inst->op | sz);
        }
        else {
            tb_out1b_UNSAFE(out, inst->op | sz | (dir ? 2 : 0));
        }
        
        tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Direct, b->gpr, a->gpr));
	}
	else if (a->type == X64_CachedGPR && b->type == X64_Integer) {
		assert(inst->op_i != 0 || inst->rx_i != 0); // Doesn't support immediates
        
		if (b->gpr >= 8 || is_64bit) tb_out1b_UNSAFE(out, x64_inst_rex(is_64bit, inst->rx_i, a->gpr, 0));
		tb_out1b_UNSAFE(out, inst->op_i | sz);
		tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Direct, inst->rx_i, a->gpr));
        
		if (dt_type == TB_I8) {
			assert(b->num < UINT8_MAX);
			tb_out1b_UNSAFE(out, b->num & 0xFF);
		}
		else if (dt_type == TB_I16) {
			assert(b->num < UINT16_MAX);
			tb_out1b_UNSAFE(out, b->num & 0xFF);
			tb_out1b_UNSAFE(out, (b->num >> 8) & 0xFF);
		}
		else if (dt_type == TB_I32 || dt_type == TB_I64 || dt_type == TB_PTR) {
			assert(b->num < UINT32_MAX);
			tb_out1b_UNSAFE(out, b->num & 0xFF);
			tb_out1b_UNSAFE(out, (b->num >> 8) & 0xFF);
			tb_out1b_UNSAFE(out, (b->num >> 16) & 0xFF);
			tb_out1b_UNSAFE(out, (b->num >> 24) & 0xFF);
		}
		else abort();
	}
	else if (a->type == X64_Local && b->type == X64_CachedGPR) {
		if (b->gpr >= 8 || is_64bit) tb_out1b_UNSAFE(out, x64_inst_rex(is_64bit, b->gpr, X64_RSP, 0));
        
		if (inst->ext) tb_out1b_UNSAFE(out, 0x0F);
		tb_out1b_UNSAFE(out, inst->op | sz | (dir ? 2 : 0));
        
		// For odd reasons RSP relative addressing requires scaling
		int8_t disp8 = a->local;
		if (a->local == 0) {
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Indirect, b->gpr, X64_RSP));
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Scale_X1, X64_RSP, X64_RSP));
		}
		else if (a->local == disp8) {
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_IndirectDisp8, b->gpr, X64_RSP));
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Scale_X1, X64_RSP, X64_RSP));
			tb_out1b_UNSAFE(out, disp8);
		}
		else {
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_IndirectDisp32, b->gpr, X64_RSP));
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Scale_X1, X64_RSP, X64_RSP));
            
			tb_out1b_UNSAFE(out, a->local & 0xFF);
			tb_out1b_UNSAFE(out, (a->local >> 8) & 0xFF);
			tb_out1b_UNSAFE(out, (a->local >> 16) & 0xFF);
			tb_out1b_UNSAFE(out, (a->local >> 24) & 0xFF);
		}
	}
	else if (a->type == X64_Array && b->type == X64_CachedGPR) {
		if (b->gpr >= 8 || is_64bit) tb_out1b_UNSAFE(out, x64_inst_rex(is_64bit, b->gpr, a->array.base, (a->array.index != GPR_NONE) ? a->array.index : 0));
        
		// SPECIAL CASE LEA
		if (inst->op == 0x8C) {
			assert(dir);
			tb_out1b_UNSAFE(out, inst->op | sz);
		}
		else {
			if (inst->ext) tb_out1b_UNSAFE(out, 0x0F);
			tb_out1b_UNSAFE(out, inst->op | sz | (dir ? 2 : 0));
		}
        
		// For odd reasons RSP relative addressing requires scaling
		int8_t disp8 = a->local;
		bool needs_index = (a->array.index != GPR_NONE) || (a->array.base & 7) == X64_RSP;
		if (a->array.disp == 0) {
			if (needs_index) {
				tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Indirect, b->gpr, X64_RSP));
				tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(a->array.scale, (a->array.base & 7) == X64_RSP ? X64_RSP : a->array.index, a->array.base));
			}
			else {
				tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Indirect, b->gpr, a->array.base));
			}
		}
		else if (needs_index) {
			if (disp8 == a->array.disp) {
				tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_IndirectDisp8, b->gpr, X64_RSP));
				tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(a->array.scale, (a->array.base & 7) == X64_RSP ? X64_RSP : a->array.index, a->array.base));
				tb_out1b_UNSAFE(out, disp8);
			}
			else {
				tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_IndirectDisp32, b->gpr, X64_RSP));
				tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(a->array.scale, (a->array.base & 7) == X64_RSP ? X64_RSP : a->array.index, a->array.base));
                
				tb_out1b_UNSAFE(out, a->array.disp & 0xFF);
				tb_out1b_UNSAFE(out, (a->array.disp >> 8) & 0xFF);
				tb_out1b_UNSAFE(out, (a->array.disp >> 16) & 0xFF);
				tb_out1b_UNSAFE(out, (a->array.disp >> 24) & 0xFF);
			}
		}
		else if (disp8 == a->array.disp) {
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_IndirectDisp8, b->gpr, a->array.base));
			tb_out1b_UNSAFE(out, disp8);
		}
		else {
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_IndirectDisp32, b->gpr, a->array.base));
            
			tb_out1b_UNSAFE(out, a->array.disp & 0xFF);
			tb_out1b_UNSAFE(out, (a->array.disp >> 8) & 0xFF);
			tb_out1b_UNSAFE(out, (a->array.disp >> 16) & 0xFF);
			tb_out1b_UNSAFE(out, (a->array.disp >> 24) & 0xFF);
		}
	}
	else if (a->type == X64_Local && b->type == X64_Integer) {
		assert(inst->op_i != 0 || inst->rx_i != 0); // Doesn't support immediates
        
		if (is_64bit) tb_out1b_UNSAFE(out, x64_inst_rex(is_64bit, inst->rx_i, X64_RSP, 0));
		tb_out1b_UNSAFE(out, inst->op_i | sz | (dir ? 2 : 0));
        
		// For odd reasons RSP relative addressing requires scaling
		int8_t disp8 = a->local;
		if (a->local == 0) {
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Indirect, inst->rx_i, X64_RSP));
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Scale_X1, X64_RSP, X64_RSP));
		}
		else if (a->local == disp8) {
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_IndirectDisp8, inst->rx_i, X64_RSP));
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Scale_X1, X64_RSP, X64_RSP));
			tb_out1b_UNSAFE(out, disp8);
		}
		else {
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_IndirectDisp32, inst->rx_i, X64_RSP));
			tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Scale_X1, X64_RSP, X64_RSP));
            
			tb_out1b_UNSAFE(out, a->local & 0xFF);
			tb_out1b_UNSAFE(out, (a->local >> 8) & 0xFF);
			tb_out1b_UNSAFE(out, (a->local >> 16) & 0xFF);
			tb_out1b_UNSAFE(out, (a->local >> 24) & 0xFF);
		}
        
		tb_out1b_UNSAFE(out, b->num & 0xFF);
		tb_out1b_UNSAFE(out, (b->num >> 8) & 0xFF);
		tb_out1b_UNSAFE(out, (b->num >> 16) & 0xFF);
		tb_out1b_UNSAFE(out, (b->num >> 24) & 0xFF);
	}
    else if (a->type == X64_CACHED_XMM && b->type == X64_CACHED_XMM) {
        tb_out1b_UNSAFE(out, inst->op);
        tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Direct, a->xmm, b->xmm));
    }
    else abort();
}

#if 0
static void x64_inst_xlat(TB_Emitter* out) {
	tb_out_reserve(out, 1);
	tb_out1b_UNSAFE(out, 0xD7);
}
#endif

#define x64_emit_normal(out, dt, op, a, b) x64_inst_op(out, dt, &insts[X64_ ## op], a, b)
#define x64_emit_normal8(out, op, a, b) x64_inst_op(out, TB_I8, &insts[X64_ ## op], a, b)
#define x64_emit_normal16(out, op, a, b) x64_inst_op(out, TB_I16, &insts[X64_ ## op], a, b)
#define x64_emit_normal32(out, op, a, b) x64_inst_op(out, TB_I32, &insts[X64_ ## op], a, b)
#define x64_emit_normal64(out, op, a, b) x64_inst_op(out, TB_I64, &insts[X64_ ## op], a, b)

static void x64_inst_ret(TB_Emitter* out) {
	tb_out1b(out, 0xC3);
}

static void x64_inst_nop(TB_Emitter* out, int count) {
	if (count == 0) return;
    
	tb_out_reserve(out, count);
	do {
		if (count >= 10) {
			tb_outs_UNSAFE(out, 10, (uint8_t[10]) { 0x66, 0x2E, 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00 });
			count -= 10;
		}
        
		if (count >= 9) {
			tb_outs_UNSAFE(out, 9, (uint8_t[9]) { 0x66, 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00 });
			count -= 9;
		}
        
		if (count >= 8) {
			tb_outs_UNSAFE(out, 8, (uint8_t[8]) { 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00 });
			count -= 8;
		}
        
		if (count >= 7) {
			tb_outs_UNSAFE(out, 7, (uint8_t[7]) { 0x0F, 0x1F, 0x80, 0x00, 0x00, 0x00, 0x00 });
			count -= 7;
		}
        
		if (count >= 6) {
			tb_outs_UNSAFE(out, 6, (uint8_t[6]) { 0x66, 0x0F, 0x1F, 0x44, 0x00, 0x00 });
			count -= 6;
		}
        
		if (count >= 5) {
			tb_outs_UNSAFE(out, 5, (uint8_t[5]) { 0x0F, 0x1F, 0x44, 0x00, 0x00 });
			count -= 5;
		}
        
		if (count >= 4) {
			tb_outs_UNSAFE(out, 4, (uint8_t[4]) { 0x0F, 0x1F, 0x40, 0x00 });
			count -= 4;
		}
        
		if (count >= 3) {
			tb_outs_UNSAFE(out, 3, (uint8_t[3]) { 0x0F, 0x1F, 0x00 });
			count -= 3;
		}
        
		if (count >= 2) {
			tb_outs_UNSAFE(out, 2, (uint8_t[2]) { 0x66, 0x90 });
			count -= 2;
		}
        
		if (count >= 1) {
			tb_out1b_UNSAFE(out, 0x90);
			count -= 1;
		}
	} while (count);
}

static void x64_load_number_into_gpr(TB_Emitter* out, int dt, uint8_t dst, uint32_t num) {
	tb_out_reserve(out, 16);
    
	if (num == 0) {
		if (dst >= 8) tb_out1b_UNSAFE(out, x64_inst_rex(true, dst, dst, 0));
		tb_out1b_UNSAFE(out, insts[X64_XOR].op | 1);
		tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Direct, dst, dst));
	}
	else if (TB_IS_INTEGER_TYPE(dt)) {
		uint8_t sz = (dt != TB_I8);
		bool is_64bit = (dt == TB_I64 || dt == TB_PTR);
        
		if (dt == TB_I8 || dt == TB_I16) tb_out1b_UNSAFE(out, 0x66);
        
		// MOV gpr, imm
		if (dst >= 8 || is_64bit) tb_out1b_UNSAFE(out, x64_inst_rex(is_64bit, insts[X64_MOV].rx_i, dst, 0));
		tb_out1b_UNSAFE(out, insts[X64_MOV].op_i | sz);
		tb_out1b_UNSAFE(out, x64_inst_mod_rx_rm(X64_Direct, insts[X64_MOV].rx_i, dst));
        
		if (dt == TB_I8) {
			assert(num < UINT8_MAX);
			tb_out1b_UNSAFE(out, num & 0xFF);
		}
		else if (dt == TB_I16) {
			assert(num < UINT16_MAX);
			tb_out1b_UNSAFE(out, num & 0xFF);
			tb_out1b_UNSAFE(out, (num >> 8) & 0xFF);
		}
		else if (dt == TB_I32 || dt == TB_I64 || dt == TB_PTR) {
			assert(num < UINT32_MAX);
			tb_out1b_UNSAFE(out, num & 0xFF);
			tb_out1b_UNSAFE(out, (num >> 8) & 0xFF);
			tb_out1b_UNSAFE(out, (num >> 16) & 0xFF);
			tb_out1b_UNSAFE(out, (num >> 24) & 0xFF);
		}
		else abort();
	}
	else abort();
}

typedef struct X64_ISel_Pattern {
	int cost;
    
	const char* pattern;
	const char* fmt;
    
	// Recycle patterns only use 2 ops, a is ignored because
	// it's aliased with dst.
	bool recycle;
	bool forced_64bit;
} X64_ISel_Pattern;

static char x64_value_type_to_pattern_char(X64_ValueType type) {
	switch (type) {
        case X64_Integer: return 'i';
        case X64_CachedGPR: return 'r';
        case X64_Local: return 'm';
        default: abort();
	}
}

static const char* x64_micro_assemble_operand(const char* format, X64_Value* dst, size_t count, const X64_Value* operands) {
	if (*format == '[') {
		format++;
        
		int32_t disp = 0;
		X64_GPR base = GPR_NONE;
		X64_GPR index = GPR_NONE;
        
		if (*format == '%') {
			format++;
			int num = *format - '0';
            
			assert(num < count);
			assert(operands[num].type == X64_CachedGPR);
			base = operands[num].gpr;
			format++;
		}
		else abort();
        
		if (*format == '+') {
			format++;
			if (*format++ != '%') abort();
            
			int num = *format - '0';
            
			assert(num < count);
			if (operands[num].type == X64_CachedGPR) {
				index = operands[num].gpr;
			}
			else if (operands[num].type == X64_Integer) {
				disp = operands[num].num;
			}
			else abort();
			format++;
		}
        
		*dst = (X64_Value){
			.type = X64_Array,
			.dt = TB_TYPE_I64(1),
			.array = {
				.base = base,
				.index = index,
				.scale = X64_Scale_X1,
				.disp = disp
			}
		};
        
		if (*format++ != ']') abort();
	}
	else {
		if (*format == '%') {
			format++;
			int num = *format - '0';
            
			assert(num < count);
			*dst = operands[num];
			format++;
		}
        
		if (*format == '+' || *format == '-' || *format == '*' || *format == '/') {
			format++;
			assert(dst->type == X64_Integer);
            
			X64_Value other;
			format = x64_micro_assemble_operand(format, &other, count, operands);
            
			assert(other.type == X64_Integer);
			if (*format == '+') dst->num += other.num;
			else if (*format == '-') dst->num -= other.num;
			else if (*format == '*') dst->num *= other.num;
			else if (*format == '/') dst->num /= other.num;
		}
	}
    
	return format;
}

// This micro assemblers can take assembly strings and convert them to actual 
static void x64_micro_assemble(TB_Emitter* out, int dt_type, const char* format, size_t count, const X64_Value* operands) {
	while (*format) {
		// Parse mnemonic
		const char* opcode_end = format;
		while (*opcode_end && *opcode_end != ' ') opcode_end++;
        
		size_t length = opcode_end - format;
		int inst = -1;
		switch (length) {
            case 3:
			if (memcmp(format, "add", length) == 0) inst = X64_ADD;
			else if (memcmp(format, "and", length) == 0) inst = X64_AND;
			else if (memcmp(format, "sub", length) == 0) inst = X64_SUB;
			else if (memcmp(format, "xor", length) == 0) inst = X64_XOR;
			else if (memcmp(format, "cmp", length) == 0) inst = X64_CMP;
			else if (memcmp(format, "mov", length) == 0) inst = X64_MOV;
			else if (memcmp(format, "lea", length) == 0) inst = X64_LEA;
			break;
            case 4:
			if (memcmp(format, "imul", length) == 0) inst = X64_IMUL;
			break;
            case 5:
			if (memcmp(format, "movsx", length) == 0) inst = X64_MOVSX;
			else if (memcmp(format, "movzx", length) == 0) inst = X64_MOVZX;
			break;
            default:
			printf("Unknown opcode!\n");
			abort();
		}
        
		if (inst == -1) {
			printf("Unknown opcode! %.*s\n", (int)length, format);
			abort();
		}
        
		// Skip mnemonic
		format += length;
		format++;
        
		X64_Value left;
		format = x64_micro_assemble_operand(format, &left, count, operands);
        
		if (*format++ != ',') abort();
        
		X64_Value right;
		format = x64_micro_assemble_operand(format, &right, count, operands);
        
		x64_inst_op(out, dt_type, &insts[inst], &left, &right);
		if (*format++ != ';') break;
	}
}

static X64_Value x64_compile_value(TB_Function* f, TB_Emitter* out, X64_Context* ctx, TB_Register r, TB_Register next);

// Going to be used in the instruction selector, promote 8bit and 16bit to 32bit or 64bit.
// Immediates can go either way but with GPRs and memory prefer 32bit if possible.
static X64_Value x64_legalize(TB_Function* f, TB_Emitter* out, X64_Context* ctx, TB_DataType dt, TB_Register reg, TB_Register next) {
	X64_Value v = x64_compile_value(f, out, ctx, reg, next);
    
	// This is kinda weird but essentially a load might 
	// return the address because this is x64 and we don't
	// need to load some in a separate instruction.
	if (dt.type == TB_PTR && f->nodes[reg].type == TB_LOAD) {
		v.dt = dt;
	}
    
	if (v.type == X64_Integer) {
		if (v.num >= INT32_MAX) {
			X64_Value dst = x64_allocate_gpr(ctx, reg, TB_TYPE_I64(1));
			x64_load_number_into_gpr(out, TB_I64, dst.gpr, v.num);
            
			return dst;
		}
		else {
			return v;
		}
	}
	else if (v.type == X64_CachedGPR) {
		TB_DataType dt = v.dt;
        
		// No vectors yet :(
		assert(dt.count == 1);
        
		if (dt.type == TB_I8) {
			X64_Value dst = x64_allocate_gpr(ctx, reg, TB_TYPE_I32(1));
            
			// TODO: Implement sign extend case
			x64_emit_normal(out, dst.type, MOVZX, &dst, &v);
			return dst;
		}
		else if (dt.type == TB_I16) {
			X64_Value dst = x64_allocate_gpr(ctx, reg, TB_TYPE_I32(1));
            
			// TODO: Implement sign extend case
			x64_emit_normal(out, dst.type, MOVZX, &dst, &v);
			return dst;
		}
		else if (dt.type == TB_I32 || dt.type == TB_I64) {
			return v;
		}
		else abort();
	}
	else if (v.type == X64_Local) {
		TB_DataType dt = v.dt;
        
		// No vectors yet :(
		assert(dt.count == 1);
        
		return v;
	}
	else abort();
}

// Not built for vector selection
// Maybe it will be one day?
// if `next_reg` is not 0, then it's the register which we expect the `dst_reg` to go into
static X64_Value x64_std_isel(TB_Function* f, TB_Emitter* out, X64_Context* ctx, TB_Register dst_reg, TB_Register a_reg, TB_Register b_reg, TB_Register next_reg, const X64_ISel_Pattern* patterns, size_t pattern_count) {
	TB_DataType dst_dt = f->nodes[dst_reg].dt;
    
	X64_Value a = x64_legalize(f, out, ctx, dst_dt, a_reg, dst_reg);
	X64_Value b = x64_legalize(f, out, ctx, dst_dt, b_reg, dst_reg);
    
	bool can_recycle = (ctx->intervals[a_reg] == dst_reg);
	if (f->nodes[next_reg].type == TB_RET &&
		a.type == X64_CachedGPR &&
		b.type == X64_CachedGPR &&
		(a.gpr != X64_RAX ||
         b.gpr != X64_RAX)) {
		// If it's about to be returned and none of the inputs are RAX, they not recycling
		can_recycle = false;
	}
    
	if (a.type == X64_Integer && b.type != X64_Integer) tb_swap(a, b);
    
	TB_DataType a_dt = a.dt;
	//TB_DataType b_dt = b.dt;
    
	assert(dst_dt.count == 1);
	assert(a.dt.count == 1);
	assert(b.dt.count == 1);
    
	// If both source operands are memory addresses, change one into a register
	if (x64_is_mem_op(&a) && x64_is_mem_op(&b)) {
		X64_Value temp = x64_allocate_gpr(ctx, a_reg, a_dt);
		x64_emit_normal(out, dst_dt.type, MOV, &temp, &a);
		a = temp;
	}
    
	bool will_try_recycle = can_recycle && a.type != X64_Integer;
    
	// Identify the pattern of the input
	char pattern_str[4] = {};
	pattern_str[0] = 'r';
	pattern_str[1] = x64_value_type_to_pattern_char(a.type);
	pattern_str[2] = x64_value_type_to_pattern_char(b.type);
	pattern_str[3] = '\0';
    
	// Find best pattern
	int lowest_cost = 255;
	const X64_ISel_Pattern* best_match = 0;
    
	for (size_t i = 0; i < pattern_count; i++) {
		int actual_cost = patterns[i].cost + (!patterns[i].recycle && will_try_recycle ? 3 : 0);
        
		if (actual_cost < lowest_cost &&
			memcmp(pattern_str, patterns[i].pattern, 4) == 0) {
			best_match = &patterns[i];
			lowest_cost = actual_cost;
		}
	}
    
	// Pattern matcher failed
	if (best_match == NULL) abort();
    
	X64_Value dst;
	if (best_match->recycle) dst = a;
	else if (f->nodes[next_reg].type == TB_RET) dst = (X64_Value){ .type = X64_CachedGPR, .gpr = X64_RAX };
	else dst = x64_allocate_gpr(ctx, dst_reg, dst_dt);
    
	X64_Value operands[3] = { dst, a, b };
	if (best_match->forced_64bit) {
		dst_dt.type = TB_I64;
	}
    
	x64_micro_assemble(out, dst_dt.type, best_match->fmt, 3, operands);
    
	return dst;
}

static X64_Value x64_compile_value(TB_Function* f, TB_Emitter* out, X64_Context* ctx, TB_Register r, TB_Register next) {
	TB_Node* reg = &f->nodes[r];
    
	enum TB_RegisterType type = reg->type;
	TB_DataType dt = reg->dt;
    
	// TODO(NeGate): Implement vectors
	assert(dt.count == 1);
    
	switch (type) {
        case TB_INT_CONST: {
            if (dt.type == TB_I128) {
                return (X64_Value) {
                    .type = X64_Integer128,
					.dt = dt,
					.big_num = reg->i_const,
					.is_mutated = false
                };
            }
            else {
                return (X64_Value) {
                    .type = X64_Integer,
					.dt = dt,
					.num = reg->i_const.lo,
					.is_mutated = false
                };
            }
        }
        case TB_ADD: {
            if (ctx->cached_values[r].type != X64_None) {
                return ctx->cached_values[r];
            }
            
            return ctx->cached_values[r] = x64_std_isel(f, out, ctx, r, reg->i_arith.a, reg->i_arith.b, next, (X64_ISel_Pattern[]) {
                                                            { 0, "rii", "mov %0,%1+%2", false },
                                                            { 0, "mii", "mov %0,%1+%2", false },
                                                            { 1, "rrr", "lea %0,[%1+%2]", false, true },
                                                            { 1, "rri", "lea %0,[%1+%2]", false, true },
                                                            { 2, "rrr", "add %0,%2", true },
                                                            { 2, "rri", "add %0,%2", true },
                                                            { 3, "rrm", "add %0,%2", true },
                                                            { 4, "rrm", "mov %0,%1;add %0,%2", false },
                                                            { 4, "rmr", "mov %0,%1;add %0,%2", false },
                                                            { 4, "rmi", "mov %0,%1;add %0,%2", false }
                                                        }, 10);
        }
        case TB_SUB: {
            if (ctx->cached_values[r].type != X64_None) {
                return ctx->cached_values[r];
            }
            
            return ctx->cached_values[r] = x64_std_isel(f, out, ctx, r, reg->i_arith.a, reg->i_arith.b, next, (X64_ISel_Pattern[]) {
                                                            { 0, "rii", "mov %0,%1-%2", false },
                                                            { 0, "mii", "mov %0,%1-%2", false },
                                                            { 2, "rrr", "sub %0,%2", true },
                                                            { 2, "rri", "sub %0,%2", true },
                                                            { 3, "rrm", "sub %0,%2", true },
                                                            { 4, "rrm", "mov %0,%1;sub %0,%2", false },
                                                            { 4, "rmr", "mov %0,%1;sub %0,%2", false },
                                                            { 4, "rmi", "mov %0,%1;sub %0,%2", false }
                                                        }, 8);
        }
        case TB_MUL: {
            if (ctx->cached_values[r].type != X64_None) {
                return ctx->cached_values[r];
            }
            
            return ctx->cached_values[r] = x64_std_isel(f, out, ctx, r, reg->i_arith.a, reg->i_arith.b, next, (X64_ISel_Pattern[]) {
                                                            { 0, "rii", "mov %0,%1*%2", false },
                                                            { 0, "mii", "mov %0,%1*%2", false },
                                                            { 2, "rrr", "imul %0,%2", true },
                                                            { 2, "rri", "imul %0,%2", true },
                                                            { 3, "rrm", "imul %0,%2", true },
                                                            { 4, "rrm", "mov %0,%1;imul %0,%2", false },
                                                            { 4, "rmr", "mov %0,%1;imul %0,%2", false },
                                                            { 4, "rmi", "mov %0,%1;imul %0,%2", false }
                                                        }, 8);
        }
        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE: {
            TB_DataType cmp_dt = reg->cmp.dt;
            
            X64_Value a = x64_compile_value(f, out, ctx, reg->i_arith.a, r);
            X64_Value b = x64_compile_value(f, out, ctx, reg->i_arith.b, r);
            
            bool invert = false;
            if (x64_is_mem_op(&a) && x64_is_mem_op(&b)) {
                X64_Value dst = x64_allocate_gpr(ctx, reg->i_arith.a, cmp_dt);
                x64_emit_normal(out, cmp_dt.type, MOV, &dst, &a);
                x64_emit_normal(out, cmp_dt.type, CMP, &dst, &b);
            }
            else {
                invert = (a.type == X64_Integer);
                
                if (invert) x64_emit_normal(out, cmp_dt.type, CMP, &b, &a);
                else x64_emit_normal(out, cmp_dt.type, CMP, &a, &b);
            }
            
            X64_Cond cc;
            switch (type) {
                case TB_CMP_EQ: cc = X64_E; break;
                case TB_CMP_NE: cc = X64_NE; break;
                case TB_CMP_SLT: cc = X64_L; break;
                case TB_CMP_SLE: cc = X64_LE; break;
                case TB_CMP_ULT: cc = X64_B; break;
                case TB_CMP_ULE: cc = X64_BE; break;
                default: abort();
            }
            
            if (invert) {
                if (cc & 1) cc &= ~1;
                else cc |= 1;
            }
            
            return (X64_Value) { .type = X64_Flags, .cond = cc };
        }
        case TB_LOAD: {
            X64_Value addr = x64_compile_value(f, out, ctx, reg->load.address, r);
            
            assert(reg->dt.count == 1);
            if (f->nodes[reg->load.address].type == TB_LOCAL ||
                f->nodes[reg->load.address].type == TB_PARAM_ADDR) {
                return addr;
            }
            else {
                X64_Value dst = x64_allocate_gpr(ctx, r, dt);
                x64_emit_normal(out, reg->dt.type, MOV, &dst, &addr);
                return dst;
            }
        }
        case TB_LOCAL: {
            return (X64_Value) {
                .type = X64_Local,
				.dt = dt,
				.local = reg->local.position,
				.is_mutated = false
            };
        }
        case TB_PARAM_ADDR: {
            TB_DataType param_dt = f->nodes[reg->param_addr.param].dt;
            
            return (X64_Value) {
                .type = X64_Local,
				.dt = param_dt,
				.local = reg->param_addr.position,
				.is_mutated = false
            };
        }
        case TB_PARAM:
		if (reg->param.id < 4) {
			// TODO(NeGate): Implement floats
			assert(dt.type != TB_F32 && dt.type != TB_F64);
            
			return (X64_Value) {
				.type = X64_CachedGPR,
                .dt = dt,
                .gpr = GPR_PARAMETERS[reg->param.id],
                .is_mutated = false
			};
		}
		else {
			// Past that it's all stack variables
			// which i haven't implemented yet
			abort();
		}
        case TB_PHI2: {
            return x64_find_phi(ctx, r)->value;
        }
        case TB_PHI1: {
            return x64_find_phi_values(ctx, f->nodes[r].phi1.a)->value;
        }
        default: abort();
	}
}

// Is this a phi node? does it can the register `reg`?
static bool x64_is_phi_that_contains(TB_Function* f, TB_Register phi, TB_Register reg) {
	if (f->nodes[phi].type == TB_PHI1) return f->nodes[phi].phi1.a == reg;
	else if (f->nodes[phi].type == TB_PHI2) return f->nodes[phi].phi2.a == reg || f->nodes[phi].phi2.b == reg;
	else return false;
}

static void x64_store_into(TB_Function* f, TB_Emitter* out, X64_Context* ctx, TB_DataType dt, X64_Value dst, TB_Register src_reg) {
	if (f->nodes[src_reg].type == TB_ADD && x64_is_phi_that_contains(f, f->nodes[src_reg].i_arith.a, src_reg)) {
		X64_Value b = x64_compile_value(f, out, ctx, f->nodes[src_reg].i_arith.b, 0);
		x64_emit_normal(out, dt.type, ADD, &dst, &b);
	}
	else {
		X64_Value value = x64_compile_value(f, out, ctx, src_reg, 0);
		x64_emit_normal(out, dt.type, MOV, &dst, &value);
	}
}

// Any basic-block local values are killed when the basic block exits
static void x64_kill_cached(TB_Function* f, TB_Emitter* out, X64_Context* ctx, TB_Register label, TB_Register terminator) {
	for (size_t i = label; i <= terminator; i++) {
		ctx->cached_values[i] = (X64_Value){ 0 };
	}
}

// Prepares to enter the basic block [label:terminator] from [from_label:from_label.terminator]
static void x64_terminate_bb(TB_Function* f, TB_Emitter* out, X64_Context* ctx, TB_Register from_label, TB_Register label, TB_Register terminator) {
	for (size_t i = label; i <= terminator; i++) {
		if (f->nodes[i].type == TB_PHI1 && f->nodes[i].phi1.a_label == from_label) {
			X64_PhiValue* phi = x64_find_phi_values(ctx, f->nodes[i].phi1.a);
            
			if (phi->value.type == X64_None) {
				phi->value = x64_allocate_gpr(ctx, i, f->nodes[i].dt);
				//printf("USE PHI node r%llu -> GPR %d\n", i, phi->value.gpr);
                
				x64_store_into(f, out, ctx, f->nodes[i].dt, phi->value, f->nodes[i].phi1.a);
			}
		}
		else if (f->nodes[i].type == TB_PHI2) {
			assert(f->nodes[i].phi2.a_label != f->nodes[i].phi2.b_label);
			X64_PhiValue* phi = x64_find_phi(ctx, i);
            
			if (phi->value.type == X64_None) {
				phi->value = x64_allocate_gpr(ctx, i, f->nodes[i].dt);
				//printf("DEF PHI node r%llu -> GPR %d\n", i, phi->value.gpr);
			}
			else {
				//printf("USE PHI node r%llu -> GPR %d\n", i, phi->value.gpr);
			}
            
			if (f->nodes[i].phi2.a_label == from_label) {
				x64_store_into(f, out, ctx, f->nodes[i].dt, phi->value, f->nodes[i].phi2.a);
			}
			else if (f->nodes[i].phi2.b_label == from_label) {
				x64_store_into(f, out, ctx, f->nodes[i].dt, phi->value, f->nodes[i].phi2.b);
			}
		}
	}
}

static TB_FunctionOutput x64_fast_compile_function(TB_Function* f, const TB_FeatureSet* features) {
	TB_TemporaryStorage* tls = tb_tls_allocate();
    
    //
	// Do a bit of analysis for the allocations, we
    // wanna minimize dynamic allocation
    //
	uint32_t ret_patch_count = 0;
	uint32_t label_patch_count = 0;
    
	uint32_t* ret_patches;
	uint32_t* labels;
	X64_LabelPatch* label_patches;
	X64_Context* ctx;
	{
		uint32_t ir_return_count = 0;
		uint32_t ir_label_count = 0;
		uint32_t ir_label_patch_count = 0;
		uint32_t phi_count = 0;
        
		for (size_t i = 1; i < f->count; i++) {
			if (f->nodes[i].type == TB_RET) ir_return_count++;
			else if (f->nodes[i].type == TB_LABEL) ir_label_count++;
			else if (f->nodes[i].type == TB_IF) ir_label_patch_count += 2;
			else if (f->nodes[i].type == TB_GOTO) ir_label_patch_count++;
			else if (f->nodes[i].type == TB_PHI1) phi_count++;
			else if (f->nodes[i].type == TB_PHI2) phi_count++;
		}
        
		// Allocate stuff from the temporary storage
		ret_patches = (uint32_t*)tb_tls_push(tls, ir_return_count * sizeof(uint32_t));
		labels = (uint32_t*)tb_tls_push(tls, ir_label_count * sizeof(uint32_t));
		label_patches = (X64_LabelPatch*)tb_tls_push(tls, ir_label_patch_count * sizeof(X64_LabelPatch));
		ctx = (X64_Context*)tb_tls_push(tls, sizeof(X64_Context) + (phi_count * sizeof(X64_PhiValue)));
        
		memset(ctx, 0, sizeof(X64_Context));
		ctx->intervals = tb_tls_push(tls, f->count * sizeof(size_t));
		ctx->cached_values = tb_tls_push(tls, f->count * sizeof(X64_Value));
        
		for (size_t i = 1; i < f->count; i++) {
			if (f->nodes[i].type == TB_PHI1) {
				ctx->phis[ctx->phi_count++] = (X64_PhiValue){
					.reg = i, .storage_a = f->nodes[i].phi1.a
				};
			}
			else if (f->nodes[i].type == TB_PHI2) {
				ctx->phis[ctx->phi_count++] = (X64_PhiValue){
					.reg = i, .storage_a = f->nodes[i].phi2.a, .storage_b = f->nodes[i].phi2.b
				};
			}
		}
        
		for (size_t i = 0; i < f->count; i++) {
			ctx->cached_values[i] = (X64_Value){ 0 };
		}
	}
    
	tb_find_live_intervals(ctx->intervals, f);
    
	ctx->gpr_desc[X64_RSP].bound_value = SIZE_MAX; // reserved
	if (!features->x64.omit_frame_pointer) ctx->gpr_desc[X64_RBP].bound_value = SIZE_MAX; // reserved
    
    TB_Emitter out = { 0 };
    
    //
    // Emit prologue
	//
    x64_inst_op64_ri32(&out, insts[X64_SUB].op_i, insts[X64_SUB].rx_i, X64_RSP, 0xCDCDCDCD);
	size_t stack_sub_patch = out.count - 4;
    
    //
    // Marks parameter registers as allocated and 
    // saves any parameters into the stack slots
	//
	x64_reserve_parameters(f, ctx, &out);
    
    X64_Value test_l = {
        .type = X64_CACHED_XMM,
        .dt = TB_TYPE_F32(1),
        .xmm = X64_XMM0
    };
    X64_Value test_r = {
        .type = X64_CACHED_XMM,
        .dt = TB_TYPE_F32(1),
        .xmm = X64_XMM1
    };
    x64_inst_op(&out, TB_F32, &insts[X64_MOVSS], &test_l, &test_r);
    x64_inst_op(&out, TB_F32, &insts[X64_ADDSS], &test_l, &test_r);
    
    TB_Register current_label = 1;
    for (size_t i = 2; i < f->count; i++) {
        enum TB_RegisterType type = f->nodes[i].type;
        TB_DataType dt = f->nodes[i].dt;
        
        switch (type) {
            case TB_NULL:
            case TB_INT_CONST:
            case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_LOAD:
            case TB_PARAM:
            case TB_PARAM_ADDR:
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_PHI1:
            case TB_PHI2:
            break;
            case TB_LOCAL: {
                uint32_t size = f->nodes[i].local.size;
                uint32_t alignment = f->nodes[i].local.alignment;
                
                f->locals_stack_usage += size;
                f->locals_stack_usage += (alignment - (f->locals_stack_usage % alignment)) % alignment;
                
                f->nodes[i].local.position = -f->locals_stack_usage;
                break;
            }
            case TB_STORE: {
                TB_Register dst_addr = f->nodes[i].store.address;
                X64_Value addr = x64_compile_value(f, &out, ctx, f->nodes[i].store.address, i);
                
                // TODO(NeGate): Handle the case where a pointer type is returned but not
                // as a pointer value but address operand?
                addr.dt = dt;
                
                TB_Register value_reg = f->nodes[i].store.value;
                if (addr.type == X64_Local) {
                    if (f->nodes[value_reg].type == TB_ADD &&
                        f->nodes[f->nodes[value_reg].i_arith.a].type == TB_LOAD &&
                        f->nodes[f->nodes[value_reg].i_arith.a].load.address == dst_addr) {
                        X64_Value rhs = x64_compile_value(f, &out, ctx, f->nodes[value_reg].i_arith.b, i);
                        if (x64_is_mem_op(&rhs)) {
                            X64_Value temp = x64_allocate_temp_gpr(ctx, dt);
                            
                            x64_emit_normal(&out, dt.type, MOV, &temp, &rhs);
                            x64_emit_normal(&out, dt.type, ADD, &addr, &temp);
                            
                            x64_free_gpr(ctx, temp.gpr);
                        }
                        else {
                            x64_emit_normal(&out, dt.type, ADD, &addr, &rhs);
                        }
                    }
                    else {
                        X64_Value value = x64_compile_value(f, &out, ctx, f->nodes[i].store.value, i);
                        
                        if (x64_is_mem_op(&value)) {
                            X64_Value temp = x64_allocate_temp_gpr(ctx, dt);
                            
                            x64_emit_normal(&out, dt.type, MOV, &temp, &value);
                            x64_emit_normal(&out, dt.type, MOV, &addr, &temp);
                            
                            x64_free_gpr(ctx, temp.gpr);
                        }
                        else {
                            x64_emit_normal(&out, dt.type, MOV, &addr, &value);
                        }
                    }
                }
                else abort();
                break;
            }
            case TB_LABEL: {
                TB_Register terminator = f->nodes[i].label.terminator;
                x64_kill_cached(f, &out, ctx, current_label, f->nodes[current_label].label.terminator);
                x64_terminate_bb(f, &out, ctx, current_label, i, terminator);
                
                /*if (f->nodes[i].label.id > 0) {
                    // TODO(NeGate): tests this for performance, it's been said that
                    // 16byte aligned jump targets are better but idk
                    if ((out.count % 16) != 0) x64_inst_nop(&out, 16 - (out.count % 16));
                }*/
                labels[f->nodes[i].label.id] = out.count;
                
                current_label = i;
                break;
            }
            case TB_IF: {
                TB_Register true_label_reg = tb_find_reg_from_label(f, f->nodes[i].if_.if_true);
                TB_Register false_label_reg = tb_find_reg_from_label(f, f->nodes[i].if_.if_false);
                TB_Register true_label_end = f->nodes[true_label_reg].label.terminator;
                TB_Register false_label_end = f->nodes[false_label_reg].label.terminator;
                
                x64_kill_cached(f, &out, ctx, current_label, f->nodes[current_label].label.terminator);
                x64_terminate_bb(f, &out, ctx, current_label, true_label_reg, true_label_end);
                x64_terminate_bb(f, &out, ctx, current_label, false_label_reg, false_label_end);
                
                X64_Value cond = x64_compile_value(f, &out, ctx, f->nodes[i].if_.cond, i);
                
                if (cond.type == X64_Flags) {
                    tb_out_reserve(&out, 6);
                    
                    tb_out1b_UNSAFE(&out, 0x0F);
                    tb_out1b_UNSAFE(&out, 0x80 + (uint8_t)cond.cond);
                    tb_out1b_UNSAFE(&out, 0x0);
                    tb_out1b_UNSAFE(&out, 0x0);
                    tb_out1b_UNSAFE(&out, 0x0);
                    tb_out1b_UNSAFE(&out, 0x0);
                    
                    label_patches[label_patch_count++] = (X64_LabelPatch){
                        .base = out.count - 6, .pos = out.count - 4, .target_lbl = f->nodes[i].if_.if_true
                    };
                    
                    size_t if_false = f->nodes[i].if_.if_false;
                    if (!(f->nodes[i + 1].type == TB_LABEL && f->nodes[i + 1].label.id == if_false)) {
                        tb_out_reserve(&out, 5);
                        tb_out1b_UNSAFE(&out, 0xE9);
                        tb_out1b_UNSAFE(&out, 0x0);
                        tb_out1b_UNSAFE(&out, 0x0);
                        tb_out1b_UNSAFE(&out, 0x0);
                        tb_out1b_UNSAFE(&out, 0x0);
                        
                        label_patches[label_patch_count++] = (X64_LabelPatch){
                            .base = out.count - 5, .pos = out.count - 4, .target_lbl = f->nodes[i].goto_.label
                        };
                    }
                }
                else abort();
                break;
            }
            case TB_GOTO: {
                x64_kill_cached(f, &out, ctx, current_label, f->nodes[current_label].label.terminator);
                
                tb_out_reserve(&out, 5);
                tb_out1b_UNSAFE(&out, 0xE9);
                tb_out1b_UNSAFE(&out, 0x0);
                tb_out1b_UNSAFE(&out, 0x0);
                tb_out1b_UNSAFE(&out, 0x0);
                tb_out1b_UNSAFE(&out, 0x0);
                
                label_patches[label_patch_count++] = (X64_LabelPatch){
                    .base = out.count - 5, .pos = out.count - 4, .target_lbl = f->nodes[i].goto_.label
                };
                break;
            }
            case TB_RET: {
                X64_Value value = x64_compile_value(f, &out, ctx, f->nodes[i].ret.value, i);
                x64_kill_cached(f, &out, ctx, current_label, f->nodes[current_label].label.terminator);
                
                if (dt.type == TB_I8 || dt.type == TB_I16 || dt.type == TB_I32 || dt.type == TB_I64) {
                    // Integer results use RAX and if result is extended RDX
                    X64_Value a = (X64_Value){
                        .type = X64_CachedGPR,
                        .dt = f->nodes[i].dt,
                        .gpr = X64_RAX
                    };
                    
                    if (value.type == X64_CachedGPR) {
                        if (value.gpr != X64_RAX) x64_emit_normal(&out, f->nodes[i].dt.type, MOV, &a, &value);
                    }
                    else if (value.type == X64_Integer) {
                        x64_load_number_into_gpr(&out, TB_I64, X64_RAX, value.num);
                    }
                    else if (value.type == X64_Local) {
                        x64_emit_normal(&out, f->nodes[i].dt.type, MOV, &a, &value);
                    }
                    else abort();
                }
                else if (dt.type == TB_I128) {
                    if (value.type == X64_Integer) {
                        x64_load_number_into_gpr(&out, TB_I64, X64_RAX, value.num);
                        x64_load_number_into_gpr(&out, TB_I64, X64_RDX, 0);
                    }
                    else if (value.type == X64_Integer128) {
                        x64_load_number_into_gpr(&out, TB_I64, X64_RAX, value.big_num.lo);
                        x64_load_number_into_gpr(&out, TB_I64, X64_RDX, value.big_num.hi);
                    }
                    else abort();
                }
                else abort();
                
                if (i < (f->count - 1)) {
                    // Only jump if we aren't literally about to end the function
                    tb_out_reserve(&out, 5);
                    tb_out1b_UNSAFE(&out, 0xE9);
                    tb_out1b_UNSAFE(&out, 0x0);
                    tb_out1b_UNSAFE(&out, 0x0);
                    tb_out1b_UNSAFE(&out, 0x0);
                    tb_out1b_UNSAFE(&out, 0x0);
                    
                    ret_patches[ret_patch_count++] = out.count - 4;
                }
                break;
            }
            default: abort();
        }
    }
    
    f->locals_stack_usage += (16 - (f->locals_stack_usage % 16)) % 16;
    assert((f->locals_stack_usage & 15) == 0);
    f->locals_stack_usage += 8;
    
    // patch labels
    for (size_t i = 0; i < label_patch_count; i++) {
        uint32_t pos = label_patches[i].pos;
        uint32_t target_lbl = label_patches[i].target_lbl;
        
        int32_t rel32 = labels[target_lbl] - (pos + 4);
        
        *((uint32_t*)&out.data[pos]) = rel32;
    }
    
    // patch return
    for (int i = 0; i < ret_patch_count; i++) {
        uint32_t pos = ret_patches[i];
        
        *((uint32_t*)&out.data[pos]) = out.count - (pos + 4);
    }
    
    // only patch the prologue and epilogue if we even need one
    if (f->locals_stack_usage > 8) {
        // patch prologue
        *((uint32_t*)&out.data[stack_sub_patch]) = f->locals_stack_usage;
        
        x64_inst_op64_ri32(&out, insts[X64_ADD].op_i, insts[X64_ADD].rx_i, X64_RSP, f->locals_stack_usage);
    }
    
    x64_inst_ret(&out);
    
    // the written size of the function taking into account the prologue skipping
    size_t actual_size = out.count;
    if (f->locals_stack_usage <= 8) actual_size -= 7; // prologue is 7 bytes long
    
    // align function to 16bytes
    // this is actually helpful to the branch predictor
    // since it has hard limits on how many branches it
    // can keep track of in the same small regions, we'd 
    // wanna avoid getting two functions mixed together
    // in it's perspective.
    x64_inst_nop(&out, 16 - (actual_size % 16));
    
    // trim memory
    out.capacity = out.count;
    out.data = realloc(out.data, out.capacity);
    if (!out.data) abort(); // I don't know if this can even fail...
    
    return (TB_FunctionOutput) {
        .name = f->name,
        .has_no_prologue = (f->locals_stack_usage <= 8),
        .emitter = out
    };
}

static void x64_reserve_parameters(TB_Function* f, X64_Context* ctx, TB_Emitter* out) {
    // This loop is supposed to reserve space for the parameters.
	// The ABI uses RCX,RDX,R8,R9 as the integer parameters
	// and XMM0-XMM5 as the float parameters.
	// It also uses RAX, RDX as integer return 
	// and XMM0 as float/vector return
	for (size_t i = 1; i < f->count; i++) {
		if (f->nodes[i].type == TB_PARAM) {
			TB_DataType dt = f->nodes[i].dt;
			int param_id = f->nodes[i].param.id;
            
			// TODO(NeGate): Implement the vector parameters
			assert(dt.count == 1);
            
			if (dt.type == TB_I8 || dt.type == TB_I16 || dt.type == TB_I32 || dt.type == TB_I64) {
				if (param_id < 4) ctx->gpr_desc[GPR_PARAMETERS[param_id]].bound_value = SIZE_MAX;
				else abort();
			}
			else abort();
		}
	}
    
	f->parameter_stack_usage = 0;
	f->locals_stack_usage = 0;
    
	for (size_t i = 1; i < f->count; i++) {
		if (f->nodes[i].type == TB_PARAM_ADDR) {
			size_t j = f->nodes[f->nodes[i].param_addr.param].param.id;
			TB_DataType dt = f->nodes[f->nodes[i].param_addr.param].dt;
            
			// TODO(NeGate): Implement the vector parameters
			assert(dt.count == 1);
            
			f->nodes[i].param_addr.position = (j + 1) * 8;
			f->parameter_stack_usage += 8;
            
			if (j < 4) {
				// Stores the parameter which were in registers:
				// mov [rsp + 8], rcx
				// mov [rsp + 16], rdx
				// mov [rsp + 24], r8
				// mov [rsp + 32], r9
				X64_Value mem_dst = (X64_Value){
					.type = X64_Local,
					.dt = TB_TYPE_I64(1),
					.local = f->nodes[i].param_addr.position
				};
                
				X64_Value param_reg = (X64_Value){
					.type = X64_CachedGPR,
					.dt = TB_TYPE_I64(1),
					.gpr = GPR_PARAMETERS[j]
				};
                
				x64_emit_normal64(out, MOV, &mem_dst, &param_reg);
			}
		}
	}
}
