#pragma once

#define TB_INTERNAL
#include "tb.h"

#define GPR_NONE 0xFF

typedef enum X64_Cond {
	X64_O, X64_NO, X64_B, X64, X64_E, X64_NE, X64_BE, X64_A,
	X64_S, X64_NS, X64_P, X64_NP, X64_L, X64_GE, X64_LE, X64_G,
} X64_Cond;

typedef enum X64_GPR {
	X64_RAX, X64_RCX, X64_RDX, X64_RBX, X64_RSP, X64_RBP, X64_RSI, X64_RDI,
	X64_R8, X64_R9, X64_R10, X64_R11, X64_R12, X64_R13, X64_R14, X64_R15,
    
    X64_GPR_NONE = -1
} X64_GPR;

typedef enum X64_XMM {
	X64_XMM0, X64_XMM1, X64_XMM2, X64_XMM3, X64_XMM4, X64_XMM5, X64_XMM6, X64_XMM7,  
    X64_XMM8, X64_XMM9, X64_XMM10, X64_XMM11, X64_XMM12, X64_XMM13, X64_XMM14, X64_XMM15  
} X64_XMM;

typedef enum X64_Mod {
	X64_MOD_INDIRECT = 0,        // [rax]
	X64_MOD_INDIRECT_DISP8 = 1,  // [rax + disp8]
	X64_MOD_INDIRECT_DISP32 = 2, // [rax + disp32]
	X64_MOD_DIRECT = 3,          // rax
} X64_Mod;

typedef enum X64_ValueType {
	X64_NONE,
    
    // Real encodable types
	// Their order is based on which should go
	// on the right hand side of isel(...)
    X64_VALUE_IMM32,
    X64_VALUE_MEM,
    X64_VALUE_GPR,
    X64_VALUE_XMM,
    
    X64_VALUE_FLAGS,
    X64_VALUE_GPR_PAIR
} X64_ValueType;

typedef enum X64_Scale {
	X64_SCALE_X1,
	X64_SCALE_X2,
	X64_SCALE_X4,
	X64_SCALE_X8
} X64_Scale;

typedef struct X64_Value {
	X64_ValueType type : 8;
	TB_DataType dt;
    
	union {
		X64_GPR gpr : 8;
        X64_XMM xmm : 8;
        X64_Cond cond : 8;
        struct {
            X64_GPR hi, lo;
        } gpr_pair;
		struct {
			X64_GPR base : 8;
			X64_GPR index : 8;
			X64_Scale scale : 8;
			int32_t disp;
		} mem;
        int32_t imm32;
	};
} X64_Value;

typedef struct X64_LabelPatch {
	int base;
	int pos;
    TB_Label target_lbl;
} X64_LabelPatch;

typedef struct X64_F32Patch {
    TB_Register src;
	int base;
    float value;
} X64_F32Patch;

typedef struct X64_PhiValue {
	TB_Register reg;
	TB_Register storage_a;
	TB_Register storage_b;
	X64_Value value;
} X64_PhiValue;

typedef struct X64_LocalDesc {
	TB_Register address;
    int32_t disp;
} X64_LocalDesc;

typedef struct X64_MemCacheDesc {
	TB_Register address;
    X64_Value value;
} X64_MemCacheDesc;

typedef struct X64_Context {
	size_t phi_count, locals_count, f32_patch_count;
    size_t mem_cache_count, local_stack_usage;
	
	uint64_t regs_to_save;
    
	size_t* intervals;
	X64_PhiValue* phis;
	X64_LocalDesc* locals;
    X64_MemCacheDesc* mem_caches;
    
	TB_Register gpr_desc[16];
	TB_Register xmm_desc[16];
} X64_Context;

// Used to quickly reorder the basic blocks
typedef struct X64_BBStack {
	TB_Register top;
	TB_Register capacity;
	
    bool* completed;
	TB_Register data[];
} X64_BBStack;

typedef enum X64_InstType {
    // Integer data processing
	X64_ADD, X64_AND, X64_OR, X64_SUB, X64_XOR, X64_CMP, X64_MOV,
    X64_TEST, X64_LEA, X64_IMUL, X64_MOVSX, X64_MOVZX,
    
    // Scalar Single
    X64_MOVSS, X64_ADDSS, X64_MULSS, X64_SUBSS, X64_DIVSS,
    X64_CMPSS,
	
	// Packed Single
	X64_MOVAPS, X64_ADDPS, X64_SUBPS, X64_MULPS, X64_DIVPS 
} X64_InstType;

typedef enum X64_ExtMode {
    // Normal
	X64_EXT_NONE,
    
    // DEF instructions have a 0F prefix
	X64_EXT_DEF,
    
    // SSE instructions have a F3 0F prefix
    X64_EXT_SSE_SS,
    X64_EXT_SSE_PS,
} X64_ExtMode;

typedef struct X64_IselInfo {
	int inst;
	
	bool communitive;
	bool has_immediates;
	bool has_memory_dst;
	bool has_memory_src;
} X64_IselInfo;

// Keeps track of any register spills within an operation
typedef struct X64_SpillInfo {
	TB_Register regs_to_save[16];
	int savepoints[16];
	
	// stack position before the spills
	uint32_t stack_pos;
} X64_SpillInfo;

typedef struct X64_NormalInst {
	uint8_t op;
    
	// IMMEDIATES
	uint8_t op_i;
	uint8_t rx_i;
    
    X64_ExtMode ext : 8;
} X64_NormalInst;

static const X64_NormalInst insts[] = {
	[X64_ADD] = { 0x00, 0x80, 0x00 },
	[X64_AND] = { 0x20, 0x80, 0x04 },
	[X64_OR]  = { 0x08, 0x80, 0x00 },
	[X64_SUB] = { 0x28, 0x80, 0x05 },
	[X64_XOR] = { 0x30, 0x80, 0x06 },
	[X64_CMP] = { 0x38, 0x80, 0x07 },
	[X64_MOV] = { 0x88, 0xC6, 0x00 },
	[X64_TEST] = { 0x84, 0xF6, 0x00 },
    
	[X64_LEA] = { 0x8D },
    
	[X64_IMUL] = { 0xAF, .ext = X64_EXT_DEF },
	[X64_MOVSX] = { 0xBE, .ext = X64_EXT_DEF },
	[X64_MOVZX] = { 0xB6, .ext = X64_EXT_DEF },
    
	[X64_MOVSS] = { 0x10, .ext = X64_EXT_SSE_SS },
	[X64_ADDSS] = { 0x58, .ext = X64_EXT_SSE_SS },
	[X64_MULSS] = { 0x59, .ext = X64_EXT_SSE_SS },
	[X64_SUBSS] = { 0x5C, .ext = X64_EXT_SSE_SS },
	[X64_DIVSS] = { 0x5E, .ext = X64_EXT_SSE_SS },
	[X64_CMPSS] = { 0xC2, .ext = X64_EXT_SSE_SS },
	
	[X64_MOVAPS] = { 0x28, .ext = X64_EXT_SSE_PS },
	[X64_ADDPS] = { 0x58, .ext = X64_EXT_SSE_PS },
	[X64_SUBPS] = { 0x5C, .ext = X64_EXT_SSE_PS },
	[X64_MULPS] = { 0x59, .ext = X64_EXT_SSE_PS },
	[X64_DIVPS] = { 0x5E, .ext = X64_EXT_SSE_PS }
};

static const X64_GPR GPR_PARAMETERS[] = {
	X64_RCX, X64_RDX, X64_R8, X64_R9
};

static const X64_GPR GPR_PRIORITY_LIST[] = {
	X64_RAX, X64_RCX, X64_RDX, X64_R8,
	X64_R9,  X64_R10, X64_R11, X64_RDI,
	X64_RSI, X64_RBX, X64_R12, X64_R13,
	X64_R14, X64_R15
};

// NOTE(NeGate): This is for Win64, we can handle SysV later
static const uint16_t ABI_CALLER_SAVED = (1u << X64_RAX) | (1u << X64_RCX) | (1u << X64_RDX) | (1u << X64_R8) | (1u << X64_R9) | (1u << X64_R10) | (1u << X64_R11);
static const uint16_t ABI_CALLEE_SAVED = ~ABI_CALLER_SAVED;

inline static uint8_t x64_inst_mod_rx_rm(uint8_t mod, uint8_t rx, uint8_t rm) {
	return ((mod & 3) << 6) | ((rx & 7) << 3) | (rm & 7);
}

inline static uint8_t x64_inst_rex(bool is_64bit, uint8_t rx, uint8_t base, uint8_t index) {
	return 0x40 | (is_64bit ? 8 : 0) | (base >> 3) | ((index >> 3) << 1) | ((rx >> 3) << 2);
}

static bool x64_is_value_gpr(const X64_Value* v, X64_GPR g) {
	if (v->type != X64_VALUE_GPR) return false;
	
	return (v->gpr == g);
}

static bool x64_is_value_equals(const X64_Value* a, const X64_Value* b) {
	if (a->type != b->type) return false;
	
	// TODO(NeGate): Implement this!
	assert(a->type == X64_VALUE_GPR);
	
	return (a->gpr == b->gpr);
}

static void x64_enqueue_bb(TB_Function* f, X64_BBStack* queue, TB_Register bb);

static void x64_spill_regs(TB_Function* f, X64_Context* ctx, TB_Emitter* out, X64_SpillInfo* info, uint32_t spill_mask, bool reserve_rax);
static void x64_reload_regs(TB_Function* f, X64_Context* ctx, TB_Emitter* out, X64_SpillInfo* info, uint32_t spill_mask, bool reserve_rax);

// Preprocessing stuff
static void x64_create_phi_lookup(TB_Function* f, X64_Context* ctx);
static int32_t x64_allocate_locals(TB_Function* f, X64_Context* ctx, TB_Emitter* out, int32_t* param_space, bool* saves_parameters);

// IR -> Machine IR Lookups
static X64_PhiValue* x64_find_phi(X64_Context* ctx, TB_Register r);
static int32_t x64_find_local(X64_Context* ctx, TB_Register r);

// Machine code generation
static void x64_eval_bb(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register bb, TB_Register bb_end);
static void x64_terminate_path(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register from_label, TB_Register label, TB_Register terminator);
static X64_Value x64_eval(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, TB_Register next);
static X64_Value x64_eval_immediate(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, const TB_Int128* imm);
static X64_Value x64_eval_float_immediate(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, float imm);
static X64_Value x64_as_memory_operand(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_DataType dt, TB_Register r);
static X64_Value x64_std_isel(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register dst_reg, TB_Register next_reg, TB_Register a_reg, TB_Register b_reg, X64_Value a, X64_Value b, const X64_IselInfo* info, bool can_recycle);
static X64_Value x64_as_bool(TB_Function* f, X64_Context* ctx, TB_Emitter* out, X64_Value src, TB_DataType src_dt);
static X64_Value x64_explicit_load(TB_Function* f, X64_Context* ctx, TB_Emitter* out, X64_Value addr, TB_Register r, TB_Register addr_reg);
static void x64_inst_bin_op(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_DataType dt, const X64_NormalInst* inst, const X64_Value* a, const X64_Value* b, TB_Register b_reg);
static X64_Value x64_legalize(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, TB_Register next);

// x64 instruction emitter
static void x64_inst_mov_ri64(TB_Emitter* out, X64_GPR dst, uint64_t imm);
static void x64_inst_op(TB_Emitter* out, int dt_type, const X64_NormalInst* inst, const X64_Value* a, const X64_Value* b);
static void x64_inst_idiv(TB_Emitter* out, int dt_type, const X64_Value* r);
static void x64_inst_nop(TB_Emitter* out, int count);

#define x64_emit_normal(out, dt, op, a, b) x64_inst_op(out, dt, &insts[X64_ ## op], a, b)
#define x64_emit_normal8(out, op, a, b) x64_inst_op(out, TB_I8, &insts[X64_ ## op], a, b)
#define x64_emit_normal16(out, op, a, b) x64_inst_op(out, TB_I16, &insts[X64_ ## op], a, b)
#define x64_emit_normal32(out, op, a, b) x64_inst_op(out, TB_I32, &insts[X64_ ## op], a, b)
#define x64_emit_normal64(out, op, a, b) x64_inst_op(out, TB_I64, &insts[X64_ ## op], a, b)

// Register allocation
static X64_Value x64_allocate_gpr_pair(TB_Function* f, X64_Context* ctx, TB_Register reg, TB_DataType dt);
static X64_Value x64_allocate_gpr(TB_Function* f, X64_Context* ctx, TB_Register reg, TB_DataType dt);
static X64_Value x64_allocate_xmm(X64_Context* ctx, TB_Register reg, TB_DataType dt);
static void x64_free_xmm(X64_Context* ctx, X64_XMM gpr);
static void x64_free_gpr(X64_Context* ctx, X64_GPR gpr);
static bool x64_is_temporary_of_bb(TB_Function* f, X64_Context* ctx, X64_GPR gpr, TB_Register bb, TB_Register bb_end);
static bool x64_register_garbage_collect(TB_Function* f, X64_Context* ctx, TB_Register bb, TB_Register bb_end);
