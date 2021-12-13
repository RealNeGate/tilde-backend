#pragma once
#include "../tb_internal.h"

#if TB_HOST_ARCH == TB_HOST_X86_64
// Needed for some of the fancier 
#include <x86intrin.h>
#endif

_Static_assert(sizeof(float) == sizeof(uint32_t), "Float needs to be a 32-bit single float!");

// can be cleared after the side effect that originally created it
//#define TB_REG_TEMP (((TB_Register)INT32_MAX) - 1)
#define X64_TEMP_REG ((TB_Register)0)

typedef union Cvt_F32U32 {
	float f;
	uint32_t i;
} Cvt_F32U32;

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
    
    // Real encodable types
	// Their order is based on which should go
	// on the right hand side of isel(...)
    VAL_IMM,
    VAL_MEM,
    VAL_GPR,
    VAL_XMM,
	
    VAL_FLAGS
} ValType;

typedef enum Scale {
	SCALE_X1,
	SCALE_X2,
	SCALE_X4,
	SCALE_X8
} Scale;

typedef struct Val {
	ValType type : 8;
	TB_DataType dt;
    
	union {
		GPR gpr : 8;
        XMM xmm : 8;
        Cond cond : 8;
		struct {
			GPR base : 8;
			GPR index : 8;
			Scale scale : 8;
			bool is_rvalue;
			int32_t disp;
		} mem;
        int32_t imm;
	};
} Val;

// We really only need the position where to patch
// it since it's all internal and the target is implicit.
typedef uint32_t ReturnPatch;

typedef struct LabelPatch {
	int pos;
    TB_Label target_lbl;
} LabelPatch;

typedef struct F32Patch {
    TB_Register src;
	int base;
    float value;
} F32Patch;

typedef struct PhiValue {
	TB_Register reg;
	TB_Register storage_a;
	TB_Register storage_b;
	Val value;
} PhiValue;

typedef struct LocalDesc {
	TB_Register address;
    int32_t disp;
} LocalDesc;

typedef struct MemCacheDesc {
	TB_Register address;
    Val value;
} MemCacheDesc;

typedef struct Ctx {
	uint8_t* out;
	uint8_t* start_out;
	
	TB_Register current_bb;
	TB_Register current_bb_end;
	
	//
	// Patch info
	//
	uint32_t label_patch_count;
	
	uint32_t* labels;
	LabelPatch* label_patches;
	
	// Some analysis crap
	int32_t* params;
	TB_Register* intervals;
	int* use_count;
	PhiValue* phis;
	ReturnPatch* ret_patches;
	
	uint32_t phi_count;
	uint32_t ret_patch_count;
	uint32_t caller_usage;
	
	// Used to allocate spills
	uint32_t stack_usage;
	
	// Register allocation:
	// these just keep track of which register is
	// bound to a value, there's no such thing as
	// aliasing one physical register with two
	// virtual registers if some optimizations need
	// that they need to handle it in the IR earlier.
	TB_Register gpr_desc[16];
	TB_Register xmm_desc[16];
	
	// reserved for a side-effect node
	uint16_t gpr_temp_bits;
	uint16_t xmm_temp_bits;
	
	// GPRs are the bottom 32bit
	// XMM is the top 32bit
	uint64_t regs_to_save;
	
	// The value of all virtual registers
	// VAL_NONE is for unmapped registers
	Val addr_desc[];
} Ctx;

typedef enum Inst2Type {
    // Integer data processing
	ADD, AND, OR, SUB, XOR, CMP, MOV,
    TEST, LEA, IMUL, MOVSX, MOVZX,
    
    // Scalar Single
    MOVSS, ADDSS, MULSS, SUBSS, DIVSS,
    CMPSS,
	
	// Packed Single
	MOVAPS, MOVUPS, ADDPS, SUBPS, MULPS, DIVPS 
} Inst2Type;

typedef enum ExtMode {
    // Normal
	EXT_NONE,
    
    // DEF instructions have a 0F prefix
	EXT_DEF,
	
	// same as DEF but for MOVZX and MOVSX
	EXT_DEF2,
    
    // SSE instructions have a F3 0F prefix
    EXT_SSE_SS,
    EXT_SSE_PS
} ExtMode;

typedef struct IselInfo {
	int inst;
	
	bool communitive;
	bool has_immediates;
	bool has_memory_dst;
	bool has_memory_src;
} IselInfo;

// Describes what general 2 operand instructions are like
typedef struct Inst2 {
	uint8_t op;
    
	// IMMEDIATES
	uint8_t op_i;
	uint8_t rx_i;
    
    ExtMode ext : 8;
} Inst2;

static const GPR GPR_PARAMETERS[4] = {
	RCX, RDX, R8, R9
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
    
	[LEA] = { 0x8D },
    
	[IMUL] = { 0xAF, .ext = EXT_DEF },
	[MOVSX] = { 0xBE, .ext = EXT_DEF },
	[MOVZX] = { 0xB6, .ext = EXT_DEF },
    
	[MOVSS] = { 0x10, .ext = EXT_SSE_SS },
	[ADDSS] = { 0x58, .ext = EXT_SSE_SS },
	[MULSS] = { 0x59, .ext = EXT_SSE_SS },
	[SUBSS] = { 0x5C, .ext = EXT_SSE_SS },
	[DIVSS] = { 0x5E, .ext = EXT_SSE_SS },
	[CMPSS] = { 0xC2, .ext = EXT_SSE_SS },
	
	[MOVAPS] = { 0x28, .ext = EXT_SSE_PS },
	[MOVUPS] = { 0x10, .ext = EXT_SSE_PS },
	[ADDPS] = { 0x58, .ext = EXT_SSE_PS },
	[SUBPS] = { 0x5C, .ext = EXT_SSE_PS },
	[MULPS] = { 0x59, .ext = EXT_SSE_PS },
	[DIVPS] = { 0x5E, .ext = EXT_SSE_PS }
};

// NOTE(NeGate): This is for Win64, we can handle SysV later
static const uint16_t ABI_CALLER_SAVED = (1u << RAX) | (1u << RCX) | (1u << RDX) | (1u << R8) | (1u << R9) | (1u << R10) | (1u << R11);
static const uint16_t ABI_CALLEE_SAVED = ~ABI_CALLER_SAVED;

// TODO(NeGate): Maybe move this out, other things might like it
inline static int align_up(int a, int b) { return a + (b - (a % b)) % b; }

// GPRs can only ever be scalar
inline static Val val_gpr(int dt_type, GPR g) {
	return (Val) {
		.type = VAL_GPR,
		.dt.count = 1,
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

static bool is_temporary_of_bb(Ctx* ctx, TB_Function* f, GPR gpr, TB_Register bb, TB_Register bb_end);

static Val use(Ctx* ctx, TB_Function* f, TB_Register r);

static void kill(Ctx* ctx, TB_Function* f, TB_Register r);

static void def(Ctx* ctx, TB_Function* f, const Val v, TB_Register r);

// allocates a new gpr as the `r` virtual reg
static Val def_new_gpr(Ctx* ctx, TB_Function* f, TB_Register r, int dt_type);

// allocates a new xmm as the `r` virtual reg
static Val def_new_xmm(Ctx* ctx, TB_Function* f, TB_Register r, TB_DataType dt);

// if something is bound to this GPR, it's spilled into a stack slot
// returns true if it spilled
static bool evict_gpr(Ctx* ctx, TB_Function* f, GPR g, TB_Register r);

// same as evict_gpr(...) but for XMM
static bool evict_xmm(Ctx* ctx, TB_Function* f, XMM x, TB_Register r);

static void temporary_reserve_gpr(Ctx* ctx, TB_Function* f, GPR g);
static void temporary_reserve_xmm(Ctx* ctx, TB_Function* f, XMM x);

static Val materialize(Ctx* ctx, TB_Function* f, const Val* src, TB_Register src_reg, TB_DataType dt);

static void isel(Ctx* ctx, TB_Function* f, const IselInfo* info,
				 TB_Register dst_reg, TB_Register a_reg, TB_Register b_reg, 
				 const Val* a, const Val* b);

// Same as isel(...) except both parameters are the same so things are slightly simpler
static void isel_aliased(Ctx* ctx, TB_Function* f, const IselInfo* info,
						 TB_Register dst_reg, TB_Register src_reg, 
						 const Val* src);

// Garbage collects up to the `r` register (doesn't handle registers 
// between basic blocks only locals).
// Returns true if any virtual registers were unused and now gone.
static bool garbage_collect_gpr(Ctx* ctx, TB_Function* f, TB_Register r);
static bool garbage_collect_xmm(Ctx* ctx, TB_Function* f, TB_Register r);

static bool is_phi_that_contains(TB_Function* f, TB_Register phi, TB_Register reg);
static void store_into(Ctx* ctx, TB_Function* f, TB_DataType dt, const Val* dst, TB_Register r, TB_Register dst_reg, TB_Register val_reg);
static Val load_into(Ctx* ctx, TB_Function* f, TB_DataType dt, TB_Register r, TB_Register addr);

