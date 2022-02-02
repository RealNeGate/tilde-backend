#pragma once
#include "../tb_internal.h"

#if TB_HOST_ARCH == TB_HOST_X86_64
// Needed for some of the fancier 
#include <emmintrin.h>
#endif

static_assert(sizeof(float) == sizeof(uint32_t), "Float needs to be a 32-bit float!");
static_assert(sizeof(double) == sizeof(uint64_t), "Double needs to be a 64-bit float!");

typedef union Cvt_F32U32 {
	float f;
	uint32_t i;
} Cvt_F32U32;

typedef union Cvt_F64U64 {
	double f;
	uint64_t i;
} Cvt_F64U64;

typedef enum Cond {
	O, NO, B, X64, E, NE, BE, A,
	S, NS, P, NP, L, GE, LE, G,
} Cond;

typedef enum GPR {
	RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
	R8, R9, R10, R11, R12, R13, R14, R15,
    
    GPR_NONE = -1
} GPR;

typedef enum XMM {
	XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,  
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
	
	XMM_NONE = -1
} XMM;

enum {
	MOD_INDIRECT = 0,        // [rax]
	MOD_INDIRECT_DISP8 = 1,  // [rax + disp8]
	MOD_INDIRECT_DISP32 = 2, // [rax + disp32]
	MOD_DIRECT = 3,          // rax
};

typedef enum ValType {
	VAL_NONE,
    
    VAL_IMM,
    VAL_MEM,
    VAL_GPR,
    VAL_XMM,
	
	VAL_GLOBAL,
    VAL_FLAGS
} ValType;

typedef enum Scale {
	SCALE_X1,
	SCALE_X2,
	SCALE_X4,
	SCALE_X8
} Scale;

typedef enum Inst2FPFlags {
	INST2FP_DOUBLE = (1u << 0),
	INST2FP_PACKED = (1u << 1)
} Inst2FPFlags;

typedef struct Val {
	uint8_t type;
	bool is_temp;
	TB_DataType dt;
	
	union {
		GPR gpr;
        XMM xmm;
        Cond cond;
		struct {
			bool is_rvalue;
			GPR base : 8;
			GPR index : 8;
			Scale scale : 8;
			int32_t disp;
		} mem;
		struct {
			// this should alias with mem.is_rvalue
			bool is_rvalue;
			TB_GlobalID id;
		} global;
        int32_t imm;
	};
} Val;

static_assert(offsetof(Val, gpr) == offsetof(Val, xmm),
			  "Val::gpr and Val::xmm must alias!");

static_assert(offsetof(Val, global.is_rvalue) == offsetof(Val, mem.is_rvalue),
			  "Val::mem.is_rvalue and Val::global.is_rvalue must alias!");

// We really only need the position where to patch
// it since it's all internal and the target is implicit.
typedef uint32_t ReturnPatch;

typedef struct LabelPatch {
	int pos;
    TB_Label target_lbl;
} LabelPatch;

typedef struct PhiValue {
	TB_Register reg;
	TB_Register storage_a, storage_b;
	
	// if it's not 0, then at termination, we need 
	// to reload into the 'value'
	int spill;
	Val value;
} PhiValue;

typedef struct StackSlot {
	TB_Register reg;
	int32_t pos;
	GPR gpr : 8;
	XMM xmm : 8;
} StackSlot;

typedef struct Ctx {
	uint8_t* out;
	uint8_t* start_out;
	
	bool is_sysv;
	
	size_t function_id;
	TB_Function* f;
	
	TB_Register current_bb;
	TB_Register current_bb_end;
	
	// used to schedule phi nodes in cases where some phi nodes
	// depend on each other
	size_t phi_queue_count;
	TB_Register* phi_queue;
	
	// Patch info
	uint32_t label_patch_count;
	
	uint32_t* labels;
	LabelPatch* label_patches;
	
	TB_Register* use_count;
	uint8_t* should_share;
	PhiValue* phis;
	ReturnPatch* ret_patches;
	
	uint32_t phi_count;
	uint32_t ret_patch_count;
	uint32_t caller_usage;
	
	// Used to allocate spills
	uint32_t stack_usage;
	
	// Register allocation:
	uint16_t gpr_allocator;
	uint16_t xmm_allocator;
	
	// GPRs are the bottom 32bit
	// XMM is the top 32bit
	uint64_t regs_to_save;
	
	TB_Register last_fence;
	
	// allows for eval with compares to return FLAGS
	bool is_if_statement_next;
	// disables counting on the use_count
	bool is_tallying;
	
	Val values[];
} Ctx;

typedef enum Inst2Type {
    // Integer data processing
	ADD, AND, OR, SUB, XOR, CMP, MOV,
    TEST, LEA, IMUL, XCHG,
	
	MOVSXB, MOVSXW, MOVSXD, 
	MOVZXB, MOVZXW
} Inst2Type;

typedef enum Inst2FPType {
	FP_MOV, FP_ADD, FP_SUB, FP_MUL, FP_DIV, FP_CMP, FP_UCOMI,
	FP_CVT, // cvtss2sd or cvtsd2ss
	FP_SQRT, FP_RSQRT,
	FP_AND, FP_OR, FP_XOR,
} Inst2FPType;

typedef enum ExtMode {
    // Normal
	EXT_NONE,
    
    // DEF instructions have a 0F prefix
	EXT_DEF,
	
	// same as DEF but for MOVZX and MOVSX
	// these are forced as always load.
	EXT_DEF2
} ExtMode;

// Describes what general 2 operand instructions are like
typedef struct Inst2 {
	uint8_t op;
    
	// IMMEDIATES
	uint8_t op_i;
	uint8_t rx_i;
    
    ExtMode ext : 8;
} Inst2;

static const GPR WIN64_GPR_PARAMETERS[4] = {
	RCX, RDX, R8, R9
};

static const GPR SYSV_GPR_PARAMETERS[6] = {
	RDI, RSI, RDX, RCX, R8, R9
};

static const GPR GPR_PRIORITY_LIST[] = {
	RAX, RCX, RDX, R8,
	R9,  R10, R11, RDI,
	RSI, RBX, R12, R13,
	R14, R15
};

typedef enum Inst1 {
	// 0xF7
    NOT  = 0xF702,
	NEG  = 0xF703,
	IDIV = 0xF707,
    
    // 0xFF
    CALL_RM = 0xFF02
} Inst1;

static const Inst2 inst2_tbl[] = {
	[ADD] = { 0x00, 0x80, 0x00 },
	[AND] = { 0x20, 0x80, 0x04 },
	[OR]  = { 0x08, 0x80, 0x01 },
	[SUB] = { 0x28, 0x80, 0x05 },
	[XOR] = { 0x30, 0x80, 0x06 },
	[CMP] = { 0x38, 0x80, 0x07 },
	[MOV] = { 0x88, 0xC6, 0x00 },
	[TEST] = { 0x84, 0xF6, 0x00 },
    
	[XCHG] = { 0x86 },
	[LEA] = { 0x8D },
    
	[IMUL] = { 0xAF, .ext = EXT_DEF },
	
	[MOVSXB] = { 0xBE, .ext = EXT_DEF2 },
	[MOVSXW] = { 0xBF, .ext = EXT_DEF2 },
	[MOVSXD] = { 0x63, .ext = EXT_NONE },
	
	[MOVZXB] = { 0xB6, .ext = EXT_DEF2 },
	[MOVZXW] = { 0xB7, .ext = EXT_DEF2 }
};

// NOTE(NeGate): This is for Win64, we can handle SysV later
static const uint16_t WIN64_ABI_CALLER_SAVED = (1u << RAX) | (1u << RCX) | (1u << RDX) | (1u << R8) | (1u << R9) | (1u << R10) | (1u << R11);
#define WIN64_ABI_CALLEE_SAVED ~WIN64_ABI_CALLER_SAVED

static const uint16_t SYSV_ABI_CALLER_SAVED = (1u << RAX) | (1u << RDI) | (1u << RSI) | (1u << RCX) | (1u << RDX) | (1u << R8) | (1u << R9) | (1u << R10) | (1u << R11);
#define SYSV_ABI_CALLEE_SAVED ~SYSV_ABI_CALLER_SAVED

// GPRs can only ever be scalar
inline static Val val_gpr(int dt_type, GPR g) {
	return (Val) {
		.type = VAL_GPR,
		.dt.width = 0,
		.dt.type = dt_type,
		.gpr = g
	};
}

inline static Val val_xmm(TB_DataType dt, XMM x) {
	return (Val) {
		.type = VAL_XMM,
		.dt = dt,
		.xmm = x
	};
}

inline static Val val_flags(Cond c) {
	return (Val) {
		.type = VAL_FLAGS,
		.dt = TB_TYPE_BOOL,
		.cond = c
	};
}

inline static Val val_global(TB_GlobalID g) {
	return (Val) {
		.type = VAL_GLOBAL,
		.dt = TB_TYPE_PTR,
		.global.is_rvalue = false,
		.global.id = g
	};
}

inline static Val val_imm(TB_DataType dt, int32_t imm) {
	return (Val) {
		.type = VAL_IMM,
		.dt = dt,
		.imm = imm
	};
}

inline static Val val_stack(TB_DataType dt, int s) {
	return (Val) {
		.type = VAL_MEM,
		.dt = dt,
		.mem = {
			.base = RBP,
			.index = GPR_NONE,
			.scale = SCALE_X1,
			.disp = s
		}
	};
}

inline static Val val_base_disp(TB_DataType dt, GPR b, int d) {
	return (Val) {
		.type = VAL_MEM,
		.dt = dt,
		.mem = {
			.base = b,
			.index = GPR_NONE,
			.scale = SCALE_X1,
			.disp = d
		}
	};
}

inline static Val val_base_index(TB_DataType dt, GPR b, GPR i, Scale s) {
	return (Val) {
		.type = VAL_MEM,
		.dt = dt,
		.mem = {
			.base = b,
			.index = i,
			.scale = s,
			.disp = 0
		}
	};
}

inline static Val val_base_index_disp(TB_DataType dt, GPR b, GPR i, Scale s, int d) {
	return (Val) {
		.type = VAL_MEM,
		.dt = dt,
		.mem = {
			.base = b,
			.index = i,
			.scale = s,
			.disp = d
		}
	};
}

inline static bool is_value_mem(const Val* v) {
	return v->type == VAL_MEM || v->type == VAL_GLOBAL;
}

inline static bool is_value_gpr(const Val* v, GPR g) {
	if (v->type != VAL_GPR) return false;
	
	return (v->gpr == g);
}

inline static bool is_value_xmm(const Val* v, XMM x) {
	if (v->type != VAL_XMM) return false;
	
	return (v->xmm == x);
}

inline static bool is_value_match(const Val* a, const Val* b) {
	if (a->type != b->type) return false;
	
	if (a->type == VAL_GPR) return a->gpr == b->gpr;
	else return false;
}

// Short and sweet C:
#define code_pos() (ctx->out - ctx->start_out)

#define emit(b) (*ctx->out++ = (b))
#define emit2(b) (*((uint16_t*) ctx->out) = (b), ctx->out += 2)
#define emit4(b) (*((uint32_t*) ctx->out) = (b), ctx->out += 4)
#define emit8(b) (*((uint64_t*) ctx->out) = (b), ctx->out += 8)

#define patch4(p, b) (*((uint32_t*) &ctx->start_out[p]) = (b))

static bool is_address_node(TB_RegType t);

static Val eval_addressof(Ctx* ctx, TB_Function* f, TB_Register r);
static Val eval_rvalue(Ctx* ctx, TB_Function* f, TB_Register r);

// returns true if we have the register is free now
static bool evict_gpr(Ctx* restrict ctx, TB_Function* f, GPR g);
static bool evict_xmm(Ctx* restrict ctx, TB_Function* f, XMM x);

static PhiValue* find_phi(Ctx* ctx, TB_Register r);
static bool is_phi_that_contains(TB_Function* f, TB_Register phi, TB_Register reg);

static bool is_temporary_of_bb(Ctx* ctx, TB_Function* f, TB_Register bound, TB_Register bb, TB_Register bb_end);
static void eval_compiler_fence(Ctx* restrict ctx, TB_Function* f, TB_Register start, TB_Register end);

static int get_data_type_size(const TB_DataType dt);
static bool should_rematerialize(TB_RegType t);

// used to add patches since there's separate arrays per thread
static thread_local size_t s_local_thread_id;
static thread_local TB_CompiledFunctionID s_compiled_func_id;
