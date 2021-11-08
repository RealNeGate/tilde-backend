#include "tb_x86_64.h"

#if TB_HOST_ARCH == TB_HOST_X86_64
#include <x86intrin.h>
#endif

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

typedef struct X64_RegisterDesc {
	TB_Register bound_value;
} X64_RegisterDesc;

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
    
	X64_RegisterDesc gpr_desc[16];
	X64_RegisterDesc xmm_desc[16];
} X64_Context;

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

typedef struct X64_IselInfo {
	int inst;
	
	bool communitive;
	bool has_immediates;
	bool has_memory_dst;
	bool has_memory_src;
} X64_IselInfo;

static const X64_GPR GPR_PARAMETERS[] = {
	X64_RCX, X64_RDX, X64_R8, X64_R9
};

static const X64_GPR GPR_PRIORITY_LIST[] = {
	X64_RAX, X64_RCX, X64_RDX, X64_R8,
	X64_R9, X64_R10, X64_R11, X64_RDI,
	X64_RSI, X64_RBX, X64_R12, X64_R13,
	X64_R14, X64_R15
};

// NOTE(NeGate): This is for Win64, we can handle SysV later
static const uint16_t ABI_CALLER_SAVED = (1u << X64_RAX) | (1u << X64_RCX) | (1u << X64_RDX) | (1u << X64_R8) | (1u << X64_R9) | (1u << X64_R10) | (1u << X64_R11);

static const uint16_t ABI_CALLEE_SAVED = ~ABI_CALLER_SAVED;

// Used to quickly reorder the basic blocks
typedef struct X64_BBStack {
	TB_Register top;
	TB_Register capacity;
	
    bool* completed;
	TB_Register data[];
} X64_BBStack;

static void x64_enqueue_bb(TB_Function* f, X64_BBStack* queue, TB_Register bb);

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

size_t x64_get_prologue_length(uint64_t saved, uint64_t stack_usage) {
	// If the stack usage is zero we don't need a prologue
	if (stack_usage == 8) return 0;
	
	bool is_stack_usage_imm8 = (stack_usage == (int8_t)stack_usage);
	return (is_stack_usage_imm8 ? 4 : 7) 
		+ 4 + (__builtin_popcount(saved & 0xFF00) * 2) 
		+ __builtin_popcount(saved & 0x00FF);
}

size_t x64_get_epilogue_length(uint64_t saved, uint64_t stack_usage) {
	if (stack_usage == 8) return 1;
	
	bool is_stack_usage_imm8 = (stack_usage == (int8_t)stack_usage);
	return (is_stack_usage_imm8 ? 5 : 8) + 1 
		+ (__builtin_popcount(saved & 0xFF00) * 2) 
		+ __builtin_popcount(saved & 0x00FF);
}

size_t x64_emit_prologue(char out[64], uint64_t saved, uint64_t stack_usage) {
	// If the stack usage is zero we don't need a prologue
	if (stack_usage == 8) return 0;
	
	size_t used = 0;
	out[used++] = 0x50 + X64_RBP;
	
	for (size_t i = 0; i < 16; i++) if (saved & (1ull << i)) {
		if (i < 8) {
			out[used++] = 0x50 + i;
		} else {
			out[used++] = 0x41;
			out[used++] = 0x50 + (i & 0b111);
		}
	}
	
	out[used++] = x64_inst_rex(true, X64_RSP, X64_RBP, 0);
	out[used++] = 0x89;
	out[used++] = x64_inst_mod_rx_rm(X64_MOD_DIRECT, X64_RSP, X64_RBP);
	
	if (stack_usage == (int8_t)stack_usage) {
		out[used++] = x64_inst_rex(true, 0x00, X64_RSP, 0);
		out[used++] = 0x83;
		out[used++] = x64_inst_mod_rx_rm(X64_MOD_DIRECT, 0x05, X64_RSP);
		out[used++] = (int8_t)stack_usage;
	} else {
		out[used++] = x64_inst_rex(true, 0x00, X64_RSP, 0);
		out[used++] = 0x81;
		out[used++] = x64_inst_mod_rx_rm(X64_MOD_DIRECT, 0x05, X64_RSP);
		*((uint32_t*)&out[used]) = stack_usage;
		used += 4;
	}
	
	return used;
}

size_t x64_emit_epilogue(char out[64], uint64_t saved, uint64_t stack_usage) {
	// if the stack isn't used then just return
	if (stack_usage == 8) {
		out[0] = 0xC3;
		return 1;
	}
	
	size_t used = 0;
	
	if (stack_usage == (int8_t)stack_usage) {
		out[used++] = x64_inst_rex(true, 0x00, X64_RSP, 0);
		out[used++] = 0x83;
		out[used++] = x64_inst_mod_rx_rm(X64_MOD_DIRECT, 0x00, X64_RSP);
		out[used++] = (int8_t)stack_usage;
	} else {
		out[used++] = x64_inst_rex(true, 0x00, X64_RSP, 0);
		out[used++] = 0x81;
		out[used++] = x64_inst_mod_rx_rm(X64_MOD_DIRECT, 0x00, X64_RSP);
		*((uint32_t*)&out[used]) = stack_usage;
		used += 4;
	}
	
	for (size_t i = 16; i--;) if (saved & (1ull << i)) {
		if (i < 8) {
			out[used++] = 0x58 + i;
		} else {
			out[used++] = 0x41;
			out[used++] = 0x58 + (i & 0b111);
		}
	}
	
	out[used++] = 0x58 + X64_RBP;
	
	out[used++] = 0xC3;
	return used;
}

TB_FunctionOutput x64_compile_function(TB_Function* f, const TB_FeatureSet* features) {
    TB_TemporaryStorage* tls = tb_tls_allocate();
    
    // Allocate all the TLS memory for the function
	X64_Context* ctx;
	uint32_t* ret_patches;
	uint32_t* labels;
    X64_BBStack* bb_stack;
	X64_LabelPatch* label_patches;
    
	uint32_t ret_patch_count = 0;
	uint32_t label_patch_count = 0;
	uint32_t caller_usage_in_bytes = 0;
	uint32_t ir_label_count = 0;
	uint32_t ir_label_patch_count = 0;
    {
		uint32_t phi_count = 0;
		uint32_t locals_count = 0;
		uint32_t mem_cache_count = 0;
		uint32_t ir_return_count = 0;
        
#if TB_HOST_ARCH == TB_HOST_X86_64
		// the node types are aligned to a cache line so we could in theory
		// grab up to 64bytes aligned without seg faulting
#define COUNT_OF_TYPE_IN_M128(t) _mm_popcnt_u32(_mm_movemask_epi8(_mm_cmpeq_epi8(bytes, _mm_set1_epi8(t))))
		
		// NOTE(NeGate): Stuff like this makes me wonder if I should SOA the
		// function nodes... not anymore haha!!
		for (size_t i = 0; i < f->nodes.count; i += 16) {
			__m128i bytes = _mm_load_si128((__m128i*)&f->nodes.type[i]);
			
			phi_count += COUNT_OF_TYPE_IN_M128(TB_PHI1);
			phi_count += COUNT_OF_TYPE_IN_M128(TB_PHI2);
			
			locals_count += COUNT_OF_TYPE_IN_M128(TB_LOCAL);
			locals_count += COUNT_OF_TYPE_IN_M128(TB_PARAM);
			
			ir_return_count += COUNT_OF_TYPE_IN_M128(TB_RET);
			ir_label_count += COUNT_OF_TYPE_IN_M128(TB_LABEL);
			
			ir_label_patch_count += COUNT_OF_TYPE_IN_M128(TB_GOTO);
			ir_label_patch_count += COUNT_OF_TYPE_IN_M128(TB_IF) * 2;
			
			mem_cache_count += COUNT_OF_TYPE_IN_M128(TB_LOAD);
		}
		
		// Skim over the types and look for CALLs
		// calculate the maximum parameter usage for a call
		for (size_t i = 0; i < f->nodes.count; i += 16) {
			__m128i bytes = _mm_load_si128((__m128i*)&f->nodes.type[i]);
			int mask = _mm_movemask_epi8(_mm_cmpeq_epi8(bytes, _mm_set1_epi8(TB_CALL)));
			
			if (mask) {
				size_t end = i + 16;
				size_t j = i + __builtin_ffsll(mask);
				
				// We know it loops at least once by this point
				do {
					int param_usage = (f->nodes.payload[i].call.param_end - f->nodes.payload[i].call.param_start);
					
					caller_usage_in_bytes = (caller_usage_in_bytes < param_usage) 
						? param_usage : caller_usage_in_bytes;
					
					j++;
				} while (j < end);
			}
		}
#undef COUNT_OF_TYPE_IN_M128
#else
		for (size_t i = 1; i < f->nodes.count; i++) {
			if (f->nodes[i].type == TB_PHI1) phi_count++;
			else if (f->nodes[i].type == TB_PHI2) phi_count++;
			else if (f->nodes[i].type == TB_LOCAL) locals_count++;
			else if (f->nodes[i].type == TB_PARAM) locals_count++;
			else if (f->nodes[i].type == TB_RET) ir_return_count++;
			else if (f->nodes[i].type == TB_LABEL) ir_label_count++;
			else if (f->nodes[i].type == TB_IF) ir_label_patch_count += 2;
			else if (f->nodes[i].type == TB_GOTO) ir_label_patch_count++;
			else if (f->nodes[i].type == TB_LOAD) mem_cache_count++;
			else if (f->nodes[i].type == TB_CALL) {
				int param_usage = (f->nodes[i].call.param_end - f->nodes[i].call.param_start);
				if (caller_usage_in_bytes < param_usage) {
					caller_usage_in_bytes = param_usage;
				}
			}
		}
#endif
		
        ir_label_patch_count += ir_label_count; // just in case
		
		ctx = (X64_Context*)tb_tls_push(tls, sizeof(X64_Context));
        memset(ctx, 0, sizeof(X64_Context));
		
        ctx->intervals = tb_tls_push(tls, f->nodes.count * sizeof(size_t));
        ctx->phis = tb_tls_push(tls, phi_count * sizeof(X64_PhiValue));
        ctx->locals = tb_tls_push(tls, locals_count * sizeof(X64_LocalDesc));
        ctx->mem_caches = tb_tls_push(tls, mem_cache_count * sizeof(X64_MemCacheDesc));
        
        ret_patches = (uint32_t*)tb_tls_push(tls, ir_return_count * sizeof(uint32_t));
		labels = (uint32_t*)tb_tls_push(tls, ir_label_count * sizeof(uint32_t));
		label_patches = (X64_LabelPatch*)tb_tls_push(tls, ir_label_patch_count * sizeof(X64_LabelPatch));
		
		bb_stack = (X64_BBStack*)tb_tls_push(tls, sizeof(X64_BBStack) + (ir_label_count * sizeof(TB_Register)));
        bb_stack->capacity = ir_label_count;
        bb_stack->completed = tb_tls_push(tls, ir_label_count * sizeof(bool));
        memset(bb_stack->completed, 0, ir_label_count * sizeof(bool));
    }
    
	// Microsoft wants at least 32bytes of parameter storage called the shadow space.
	// This is used to store the first 4 parameters which are usually in registers but
	// can possibly spill.
	if (caller_usage_in_bytes > 0 && caller_usage_in_bytes < 32) {
		caller_usage_in_bytes = 32;
	}
    
	tb_find_live_intervals(ctx->intervals, f);
    x64_create_phi_lookup(f, ctx);
    
    // Reserve stack
	ctx->gpr_desc[X64_RSP].bound_value = TB_REG_MAX; // reserved
	ctx->gpr_desc[X64_RBP].bound_value = TB_REG_MAX; // reserved
    
    TB_Emitter out = { 0 };
    
	int32_t param_space;
	bool saves_parameters;
    ctx->local_stack_usage = x64_allocate_locals(f, ctx, &out, &param_space, &saves_parameters);
	
	// the queue starts with just the entry BB
	// then just follows it's terminators
    bb_stack->top = 1;
    bb_stack->data[0] = 1;
	
    do {
		TB_Register bb = bb_stack->data[--bb_stack->top];
		
        assert(f->nodes.type[bb] == TB_LABEL);
        TB_Label label_id = f->nodes.payload[bb].label.id;
        TB_Register bb_end = f->nodes.payload[bb].label.terminator;
        
        // Clear and initialize new cache
        //printf("Process BB: r%u-r%u\n", bb, bb_end);
        bb_stack->completed[label_id] = true;
        labels[label_id] = out.count;
        
		// Generate instructions from the side-effect nodes using
		// all the other nodes and then terminate the basic block
        if (bb < bb_end) x64_eval_bb(f, ctx, &out, bb, bb_end);
		
        // Handle the terminator node
		TB_RegType reg_type = f->nodes.type[bb_end];
		TB_DataType dt = f->nodes.dt[bb_end];
		TB_RegPayload p = f->nodes.payload[bb_end];
		
		if (reg_type == TB_IF) {
			TB_Label if_true = p.if_.if_true;
			TB_Label if_false = p.if_.if_false;
			
			TB_Register if_true_reg = tb_find_reg_from_label(f, if_true);
			TB_Register if_false_reg = tb_find_reg_from_label(f, if_false);
			
			TB_Register if_true_reg_end = f->nodes.payload[if_true_reg].label.terminator;
			TB_Register if_false_reg_end = f->nodes.payload[if_false_reg].label.terminator;
			
			x64_terminate_path(f, ctx, &out, bb, if_true_reg, if_true_reg_end);
			x64_terminate_path(f, ctx, &out, bb, if_false_reg, if_false_reg_end);
			
			x64_enqueue_bb(f, bb_stack, bb_end + 1);
			
			TB_Register cond_reg = p.if_.cond;
			X64_Value cond = x64_eval(f, ctx, &out, cond_reg, bb_end);
			
			if (cond.type == X64_VALUE_IMM32) {
				TB_Label dst = (cond.imm32 ? if_true : if_false);
				label_patches[label_patch_count++] = (X64_LabelPatch){
					.base = out.count, .pos = out.count + 1, .target_lbl = dst
				};
				
				uint8_t* out_buffer = tb_out_reserve(&out, 5);
				out_buffer[0] = 0xE9;
				out_buffer[1] = 0x00;
				out_buffer[2] = 0x00;
				out_buffer[3] = 0x00;
				out_buffer[4] = 0x00;
				tb_out_commit(&out, 5);
			} else {
				// Implicit convert into FLAGS
				cond = x64_as_bool(f, ctx, &out, cond, f->nodes.dt[cond_reg]);
				
				// Reorder the targets to avoid an extra JMP
				TB_Label fallthrough_label = 0;
				if (bb_stack->top > 0) {
					fallthrough_label = f->nodes.payload[bb_stack->data[bb_stack->top - 1]].label.id;
				}
				bool has_fallthrough = fallthrough_label == if_false;
				
				X64_Cond cc = cond.cond;
				if (fallthrough_label == if_true) {
					// flip the condition and the labels
					tb_swap(if_true, if_false);
					cc ^= 1;
					
					has_fallthrough = true;
				}
				
				// JCC .true
				// JMP .false # elidable if it points to the next instruction
				uint8_t* out_buffer = tb_out_reserve(&out, 11);
				out_buffer[0] = 0x0F;
				out_buffer[1] = 0x80 + (uint8_t)cc;
				out_buffer[2] = 0x00;
				out_buffer[3] = 0x00;
				out_buffer[4] = 0x00;
				out_buffer[5] = 0x00;
				
				label_patches[label_patch_count++] = (X64_LabelPatch){
					.base = out.count, .pos = out.count + 2, .target_lbl = if_true
				};
				
				if (!has_fallthrough) {
					label_patches[label_patch_count++] = (X64_LabelPatch){
						.base = out.count + 6, .pos = out.count + 7, .target_lbl = if_false
					};
					
					out_buffer[6] = 0xE9;
					out_buffer[7] = 0x00;
					out_buffer[8] = 0x00;
					out_buffer[9] = 0x00;
					out_buffer[10] = 0x00;
					
					tb_out_commit(&out, 11);
				} else {
					tb_out_commit(&out, 6);
				}
			}
		} else if (reg_type == TB_RET) {
			if (dt.type != TB_VOID) {
				X64_Value value = x64_eval(f, ctx, &out, p.ret.value, bb_end);
				
				if (value.dt.type == TB_F32 || value.dt.count > 1) {
					// Float results use XMM0
					X64_Value dst = (X64_Value){
						.type = X64_VALUE_XMM,
						.dt = value.dt,
						.xmm = X64_XMM0
					};
					
					if (value.type != X64_VALUE_XMM || (value.type == X64_VALUE_XMM && value.gpr != X64_XMM0)) {
						if (value.dt.count > 1) x64_emit_normal(&out, value.dt.type, MOVAPS, &dst, &value);
						else x64_emit_normal(&out, value.dt.type, MOVSS, &dst, &value);
					}
				} else if (value.dt.type == TB_I8 ||
						   value.dt.type == TB_I16 ||
						   value.dt.type == TB_I32 ||
						   value.dt.type == TB_I64 ||
						   value.dt.type == TB_PTR) {
					// Integer results use RAX and if result is extended RDX
					X64_Value dst = (X64_Value){
						.type = X64_VALUE_GPR,
						.dt = value.dt,
						.gpr = X64_RAX
					};
					
					if (value.type == X64_VALUE_IMM32) {
						uint8_t* out_buffer = tb_out_reserve(&out, 5);
						
						assert(value.imm32 < INT32_MAX);
						if (value.imm32 == 0) {
							*out_buffer++ = 0x31;
							*out_buffer++ = x64_inst_mod_rx_rm(X64_MOD_DIRECT, X64_RAX, X64_RAX);
							
							tb_out_commit(&out, 2);
						} else {
							// mov eax, imm32
							*out_buffer++ = 0xB8;
							*((uint32_t*)out_buffer) = value.imm32;
							out_buffer += 4;
							
							tb_out_commit(&out, 5);
						}
					} else if (value.type != X64_VALUE_GPR || (value.type == X64_VALUE_GPR && value.gpr != X64_RAX)) {
						x64_emit_normal(&out, value.dt.type, MOV, &dst, &value);
					}
				} else tb_todo();
			}
			
			x64_enqueue_bb(f, bb_stack, bb_end + 1);
			
			TB_Label fallthrough_label_reg = 0;
			if (bb_stack->top > 0) {
				fallthrough_label_reg = bb_stack->data[bb_stack->top - 1];
			}
			
			// Only jump if we aren't literally about to end the function
			if (fallthrough_label_reg != TB_NULL_REG) {
				ret_patches[ret_patch_count++] = out.count + 1;
				
				uint8_t* out_buffer = tb_out_reserve(&out, 5);
				out_buffer[0] = 0xE9;
				out_buffer[1] = 0x00;
				out_buffer[2] = 0x00;
				out_buffer[3] = 0x00;
				out_buffer[4] = 0x00;
				tb_out_commit(&out, 5);
			}
		} else if (reg_type == TB_LABEL) {
			x64_terminate_path(f, ctx, &out, bb, bb_end, p.label.terminator);
			x64_enqueue_bb(f, bb_stack, bb_end);
			
			TB_Label fallthrough_label = 0;
			if (bb_stack->top > 0) {
				fallthrough_label = f->nodes.payload[bb_stack->data[bb_stack->top - 1]].label.id;
			}
			
			TB_Label target_lbl = p.label.id;
			if (fallthrough_label != target_lbl) {
				label_patches[label_patch_count++] = (X64_LabelPatch){
					.base = out.count, .pos = out.count + 1, .target_lbl = target_lbl
				};
				
				uint8_t* out_buffer = tb_out_reserve(&out, 5);
				out_buffer[0] = 0xE9;
				out_buffer[1] = 0x00;
				out_buffer[2] = 0x00;
				out_buffer[3] = 0x00;
				out_buffer[4] = 0x00;
				tb_out_commit(&out, 5);
			}
		} else if (reg_type == TB_GOTO) {
			TB_Register target_reg = tb_find_reg_from_label(f, p.goto_.label);
			x64_terminate_path(f, ctx, &out, bb, target_reg, f->nodes.payload[target_reg].label.terminator);
			
			// Try to compile it's target next
			//x64_enqueue_bb(f, bb_stack, target_reg);
			x64_enqueue_bb(f, bb_stack, bb_end + 1);
			
			TB_Label fallthrough_label = 0;
			if (bb_stack->top > 0) {
				fallthrough_label = f->nodes.payload[bb_stack->data[bb_stack->top - 1]].label.id;
			}
			
			TB_Label target_lbl = p.goto_.label;
			if (fallthrough_label != target_lbl) {
				label_patches[label_patch_count++] = (X64_LabelPatch){
					.base = out.count, .pos = out.count + 1, .target_lbl = target_lbl
				};
				
				uint8_t* out_buffer = tb_out_reserve(&out, 5);
				out_buffer[0] = 0xE9;
				out_buffer[1] = 0x00;
				out_buffer[2] = 0x00;
				out_buffer[3] = 0x00;
				out_buffer[4] = 0x00;
				tb_out_commit(&out, 5);
			}
		} else if (reg_type == TB_SWITCH) {
			x64_enqueue_bb(f, bb_stack, bb_end + 1);
			
			// the switch statement is possibly the most complicated single
			// node in the IR, it matches the switch statement in C almost
			// perfectly and can be thought of as a search function.
			// 
			//  goto jump_table[search(n)]
			// 
			// There's a variety of ways that the switch node can be evaluated.
			// 
			// First attempt: if the entries are mostly linear and mostly sequencial
			// then it may be best to build a small indirect jump table and use that,
			// pros are that it's simple and sequencial, cons are that it's takes up
			// a lot of space when the entries are sparse.
			size_t entry_start = p.switch_.entries_start;
			size_t entry_count = (p.switch_.entries_end - p.switch_.entries_start) / 2;
			
			size_t switch_range_min = UINT64_MAX;
			size_t switch_range_max = 0;
			size_t switch_max_case_dist = 0; // Maximum distance between any two case keys
			size_t switch_avg_case_dist = 0;
			
			for (size_t j = 0; j < entry_count; j++) {
				TB_SwitchEntry* e = (TB_SwitchEntry*)&f->vla.data[entry_start + (j * 2)];
				
				switch_range_min = (e->key < switch_range_min) ? e->key : switch_range_min;
				switch_range_max = (e->key < switch_range_max) ? switch_range_max : e->key;
				
				if (j) {
					TB_SwitchEntry* prev = (TB_SwitchEntry*)&f->vla.data[entry_start + (j * 2) - 2];
					
					size_t dist = e->key - prev->key;
					switch_max_case_dist = (dist < switch_max_case_dist) ? switch_max_case_dist : dist;
					switch_avg_case_dist = (dist + switch_avg_case_dist + 1) / 2;
				}
			}
			
			if (switch_avg_case_dist > 4 || switch_avg_case_dist > switch_max_case_dist) {
				// Don't use a normal jump table, it's probably too expensive
				uint8_t* out_buffer = tb_out_reserve(&out, 1);
				*out_buffer++ = 0xCC;
				tb_out_commit(&out, 1);
			} else {
				uint8_t* out_buffer = tb_out_reserve(&out, 1);
				*out_buffer++ = 0xCC;
				tb_out_commit(&out, 1);
			}
		} else {
			tb_todo(); // TODO
		}
		
		x64_register_garbage_collect(f, ctx, bb, bb_end);
		
		// Terminate any cached values
		ctx->mem_cache_count = 0;
	} while (bb_stack->top > 0);
	
	// Align stack usage to 16bytes and add 8 bytes for the return address
	int local_stack_usage = ctx->local_stack_usage + caller_usage_in_bytes;
	if (!saves_parameters && local_stack_usage <= param_space) {
		local_stack_usage = 0;
	}
	
	local_stack_usage += (16 - (local_stack_usage % 16)) % 16;
	assert((local_stack_usage & 15) == 0);
	local_stack_usage += 8;
	
	// patch return
	for (size_t i = 0; i < ret_patch_count; i++) {
		uint32_t pos = ret_patches[i];
		
		*((uint32_t*)&out.data[pos]) = out.count - (pos + 4);
	}
	
	// patch labels
	for (size_t i = 0; i < label_patch_count; i++) {
		uint32_t pos = label_patches[i].pos;
		uint32_t target_lbl = label_patches[i].target_lbl;
		
		int32_t rel32 = labels[target_lbl] - (pos + 4);
		
		*((uint32_t*)&out.data[pos]) = rel32;
	}
	
	// Trim code output memory
	out.capacity = out.count;
	out.data = realloc(out.data, out.capacity);
	if (!out.data) tb_todo(); // I don't know if this can even fail...
	
	return (TB_FunctionOutput) {
		.name = f->name,
		.emitter = out,
		.stack_usage = local_stack_usage,
		.prologue_epilogue_metadata = ctx->regs_to_save
	};
}

static void x64_enqueue_bb(TB_Function* f, X64_BBStack* stack, TB_Register bb) {
    if (bb >= f->nodes.count) return;
	
	assert(bb);
    TB_Label id = f->nodes.payload[bb].label.id;
	
	// Don't enqueue if it already got compiled
    if (stack->completed[id]) return;
	
	assert(stack->top < stack->capacity);
    stack->data[stack->top++] = bb;
}

static void x64_eval_bb(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register bb, TB_Register bb_end) {
	// Evaluate all side effect instructions
	loop_range(i, bb + 1, bb_end) {
		TB_DataType dt = f->nodes.dt[i];
		
		switch (f->nodes.type[i]) {
			case TB_NULL:
			case TB_LOCAL: // the allocation is handled beforehand
			case TB_PARAM:
			case TB_PARAM_ADDR:
			case TB_INT_CONST:
			case TB_FLOAT_CONST:
			case TB_ARRAY_ACCESS:
			case TB_MEMBER_ACCESS:
			case TB_SIGN_EXT:
			case TB_ZERO_EXT:
			case TB_AND:
			case TB_OR:
			case TB_ADD:
			case TB_SUB:
			case TB_MUL:
			case TB_SHL:
			case TB_SHR:
			case TB_SAR:
			case TB_SDIV:
			case TB_UDIV:
			case TB_FADD:
			case TB_FSUB:
			case TB_FMUL:
			case TB_FDIV:
			case TB_CMP_EQ:
			case TB_CMP_NE:
			case TB_CMP_ULT:
			case TB_CMP_ULE:
			case TB_CMP_SLT:
			case TB_CMP_SLE:
			case TB_PHI1:
			case TB_PHI2:
			break;
			case TB_CALL: {
				// garbage collect everything before the call since we'd rather
				// not save more than necessary
				x64_register_garbage_collect(f, ctx, bb, i + 1);
				
				int param_count = f->nodes.payload[i].call.param_end - f->nodes.payload[i].call.param_start;
				int saved_local_stack_usage = ctx->local_stack_usage; 
				
				// identify which registers to save
				// TODO(NeGate): do a small garbage collection stage just 
				// to get rid of any registers we don't need
				// any more
				// TODO(NeGate): Implement XMM registers
				TB_Register gprs_to_save[16] = { 0 };
				for (int i = 0; i < 16; i++) {
					if (ctx->gpr_desc[i].bound_value != 0 && ABI_CALLER_SAVED & (1u << i)) {
						gprs_to_save[i] = ctx->gpr_desc[i].bound_value;
					}
				}
				
				// Any parameters after 4 require memory stores so it's likely that they'll
				// need to use a register to perform any mem<->mem transfers.
				if (gprs_to_save[X64_RAX] == 0 && param_count > 4) {
					gprs_to_save[X64_RAX] = ctx->gpr_desc[i].bound_value;
				}
				
				// save the registers
				// TODO(NeGate): Implement XMM registers
				int savepoints[16];
				for (int i = 0; i < 16; i++) if (gprs_to_save[i]) {
					// TODO(NeGate): Implement a smarter alignment setup
					ctx->local_stack_usage += 8;
					ctx->local_stack_usage += (8 - (ctx->local_stack_usage % 8)) % 8;
					
					int pos = savepoints[i] = ctx->local_stack_usage;
					TB_DataType dt = f->nodes.dt[gprs_to_save[i]];
					
					X64_Value src = (X64_Value){
						.type = X64_VALUE_GPR, .dt = dt, .gpr = i
					};
					X64_Value dst = (X64_Value){
						.type = X64_VALUE_MEM, .dt = dt, .mem = {
							.base = X64_RBP,
							.index = X64_GPR_NONE,
							.scale = X64_SCALE_X1,
							.disp = -pos
						}
					};
					
					x64_emit_normal(out, dt.type, MOV, &dst, &src);
				}
				
				// don't let anything allocate this while generating the parameters
				if (param_count > 4) ctx->gpr_desc[X64_RAX].bound_value = 0;
				
				// evaluate parameters
				for (size_t j = f->nodes.payload[i].call.param_start; j < f->nodes.payload[i].call.param_end; j++) {
					TB_Register param_reg = f->vla.data[j];
					X64_Value param = x64_eval(f, ctx, out, param_reg, i);
					
					size_t id = j - f->nodes.payload[i].call.param_start;
					if (id < 4) {
						// parameter is a register
						X64_Value dst = (X64_Value) {
							.type = X64_VALUE_GPR,
							.dt = param.dt,
							.gpr = GPR_PARAMETERS[id]
						};
						
						if (param.type != X64_VALUE_GPR ||
							(param.type == X64_VALUE_GPR && param.gpr != GPR_PARAMETERS[id])) {
							x64_emit_normal(out, dt.type, MOV, &dst, &param);
						}
						
						// don't let anything allocate this while generating the parameters
						ctx->gpr_desc[GPR_PARAMETERS[id]].bound_value = TB_REG_MAX;
					} else {
						// parameter is in memory
						tb_todo();
						//x64_emit_normal(out, dt.type, MOV, &dst, &src);
					}
				}
				
				// because of the weird way we allocate the stack, before and after
				// a function call we must "allocate" the local storage space
				size_t stack_alloc = ctx->local_stack_usage + (param_count * 8);
				//if (is_extern_c) stack_alloc += out.locals + caller_usage;
				
				// Align to 16 bytes in compliance with the SysV and Microsoft ABIs.
				stack_alloc = (stack_alloc & ~0xF) + 16;
				
				// Emit stack movements, CALL instruction and CALL patch
				int target_func_id = f->nodes.payload[i].call.target - f->module->functions.data;
				int source_func_id = f - f->module->functions.data;
				
				{
					tb_emit_call_patch(f->module, source_func_id, target_func_id, out->count + 1);
					uint8_t* out_buffer = tb_out_reserve(out, 5);
					
					// CALL rel32
					out_buffer[0] = 0xE8;
					*((uint32_t*)&out_buffer[1]) = 0;
					
					tb_out_commit(out, 5);
				}
				
				// restore any saved registers
				// handle the return value separately, we'll be swapping it's place
				for (int i = 16; (--i) >= 1;) if (gprs_to_save[i]) {
					int pos = savepoints[i];
					TB_DataType spill_dt = f->nodes.dt[gprs_to_save[i]];
					
					X64_Value dst = (X64_Value){
						.type = X64_VALUE_GPR, .dt = spill_dt, .gpr = i
					};
					X64_Value src = (X64_Value){
						.type = X64_VALUE_MEM, .dt = spill_dt, .mem = {
							.base = X64_RBP,
							.index = X64_GPR_NONE,
							.scale = X64_SCALE_X1,
							.disp = -pos
						}
					};
					
					x64_emit_normal(out, spill_dt.type, MOV, &dst, &src);
					
					// restore it's binding
					ctx->gpr_desc[i].bound_value = gprs_to_save[i];
				}
				
				// the return value is in RAX
				ctx->gpr_desc[X64_RAX].bound_value = i;
				
				// if there was a value in RAX, we need to restore it to a different place
				if (gprs_to_save[X64_RAX] != 0) {
					TB_DataType spill_dt = f->nodes.dt[gprs_to_save[X64_RAX]];
					
					X64_Value dst = x64_allocate_gpr(f, ctx, gprs_to_save[X64_RAX], spill_dt);
					X64_Value src = (X64_Value){
						.type = X64_VALUE_MEM, .dt = spill_dt, .mem = {
							.base = X64_RBP,
							.index = X64_GPR_NONE,
							.scale = X64_SCALE_X1,
							.disp = -savepoints[X64_RAX]
						}
					};
					
					x64_emit_normal(out, spill_dt.type, MOV, &dst, &src);
					ctx->gpr_desc[dst.gpr].bound_value = gprs_to_save[X64_RAX];
				}
				
				ctx->local_stack_usage = saved_local_stack_usage; 
				break;
			}
			case TB_LOAD: {
				TB_Register addr_reg = f->nodes.payload[i].load.address;
				
				bool explicit_load = true;
				if (f->nodes.type[addr_reg] == TB_LOCAL ||
					f->nodes.type[addr_reg] == TB_PARAM_ADDR) {
					explicit_load = false;
					
					// If this load out-lives the next store it must be explicitly 
					// loaded now since it might deviate from the memory.
					// TODO(NeGate): Implement noalias, where loads are only invalidate
					// if the load out-lives a store from the same pointer not just
					// any pointer.
					loop_range(j, i + 1, bb_end) {
						if (f->nodes.type[j] == TB_STORE) {
							explicit_load = (ctx->intervals[i] > j);
							break;
						}
					}
				}
				
				if (explicit_load) {
					X64_Value addr = x64_eval(f, ctx, out, addr_reg, i);
					
					ctx->mem_caches[ctx->mem_cache_count++] = (X64_MemCacheDesc){
						.address = addr_reg,
						.value = x64_explicit_load(f, ctx, out, addr, i, addr_reg)
					};
				}
				break;
			}
			case TB_STORE: {
				// TODO(NeGate): Allow for patterns such as:
				TB_Register address_reg = f->nodes.payload[i].store.address;
				TB_Register value_reg = f->nodes.payload[i].store.value;
				
				// Eval address and cast to the correct type for the store
				X64_Value address = x64_eval(f, ctx, out, address_reg, i);
				//if (address.dt.type != TB_PTR) tb_todo();
				
				// TODO(NeGate): Cast to store type
				
				// TODO(NeGate): Clean this code up
				if (f->nodes.type[value_reg] == TB_ADD 
					&& f->nodes.type[f->nodes.payload[value_reg].i_arith.a] == TB_LOAD
					&& f->nodes.payload[f->nodes.payload[value_reg].i_arith.a].load.address == address_reg) {
					// *p = *p + a  => add Xword [p], a
					X64_Value value = x64_eval(f, ctx, out, f->nodes.payload[value_reg].i_arith.b, i);
					
					x64_inst_bin_op(f, ctx, out, dt, &insts[X64_ADD], &address, &value, f->nodes.payload[value_reg].i_arith.b);
				} else if (f->nodes.type[value_reg] == TB_SUB &&
						   f->nodes.type[f->nodes.payload[value_reg].i_arith.a] == TB_LOAD &&
						   f->nodes.payload[f->nodes.payload[value_reg].i_arith.a].load.address == address_reg) {
					// *p = *p - a  => sub Xword [p], a
					X64_Value value = x64_eval(f, ctx, out, f->nodes.payload[value_reg].i_arith.b, i);
					
					x64_inst_bin_op(f, ctx, out, dt, &insts[X64_SUB], &address, &value, f->nodes.payload[value_reg].i_arith.b);
				} else {
					X64_Value value = x64_eval(f, ctx, out, value_reg, i);
					if (value.type == X64_VALUE_MEM && f->nodes.type[value_reg] != TB_LOAD) {
						// passing address, not value
						X64_Value value_addr = x64_allocate_gpr(f, ctx, value_reg, TB_TYPE_PTR());
						
						x64_emit_normal(out, TB_I64, LEA, &value_addr, &value);
						x64_emit_normal(out, dt.type, MOV, &address, &value_addr);
					} else {
						x64_inst_bin_op(f, ctx, out, dt, &insts[X64_MOV], &address, &value, value_reg);
					}
				}
				break;
			}
			default: 
			tb_todo();
		}
	}
}

#include "tb_x86_64_patterns.h"

static X64_Value x64_eval(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, TB_Register next) {
	TB_RegType reg_type = f->nodes.type[r];
	TB_DataType dt = f->nodes.dt[r];
	TB_RegPayload p = f->nodes.payload[r];
	
	// Check if it's already cached
	if (TB_IS_FLOAT_TYPE(dt.type) || dt.count > 1) {
		// Check if it's in registers
		for (int i = 0; i < 16; i++) {
			TB_Register bound = ctx->xmm_desc[i].bound_value;
			
			if (r == bound) {
				return (X64_Value) {
					.type = X64_VALUE_XMM,
					.dt = dt,
					.gpr = i
				};
			}
		}
	} else if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
		for (int i = 0; i < 16; i++) {
			TB_Register bound = ctx->gpr_desc[i].bound_value;
			
			if (r == bound) {
				return (X64_Value) {
					.type = X64_VALUE_GPR,
					.dt = dt,
					.gpr = i
				};
			}
		}
	}
	
	switch (reg_type) {
        case TB_INT_CONST: {
            return x64_eval_immediate(f, ctx, out, r, &p.i_const);
        }
        case TB_FLOAT_CONST: {
            return x64_eval_float_immediate(f, ctx, out, r, p.f_const);
        }
        case TB_CALL: {
			// How? Where did you lose it?
			tb_todo();
        }
        case TB_PHI2: {
            return x64_find_phi(ctx, r)->value;
        }
        case TB_PHI1: {
			// PHI1 just points to an owner PHI2
            return x64_find_phi(ctx, f->nodes.payload[r].phi1.a)->value;
        }
		case TB_MEMBER_ACCESS: {
            X64_Value base = x64_eval(f, ctx, out, p.member_access.base, r);
			int32_t offset = p.member_access.offset;
			
			// Load value
			if (base.type == X64_VALUE_MEM) {
				base.mem.disp += offset;
				
				return base;
			} else if (base.type == X64_VALUE_GPR) {
				return (X64_Value) {
					.type = X64_VALUE_MEM,
					.dt = dt,
					.mem = {
						.base = base.gpr,
						.index = X64_GPR_NONE,
						.scale = X64_SCALE_X1,
						.disp = offset
					}
				};
			} else tb_todo();
		}
		case TB_ARRAY_ACCESS: {
            X64_Value base = x64_eval(f, ctx, out, p.array_access.base, r);
            X64_Value index = x64_eval(f, ctx, out, p.array_access.index, r);
			uint32_t stride = p.array_access.stride;
			
			// Load value
			X64_GPR base_reg;
			if (base.type == X64_VALUE_MEM) {
				X64_Value ptr = x64_allocate_gpr(f, ctx, r, dt);
				
				x64_emit_normal64(out, MOV, &ptr, &base);
				base_reg = ptr.gpr;
			} else if (base.type == X64_VALUE_GPR) {
				base_reg = base.gpr;
			} else tb_todo();
			
			if (index.type == X64_VALUE_IMM32) {
				return (X64_Value) {
					.type = X64_VALUE_MEM,
					.dt = dt,
					.mem = {
						.base = base_reg,
						.index = X64_GPR_NONE,
						.scale = X64_SCALE_X1,
						.disp = index.imm32 * stride
					}
				};
			} else if (index.type == X64_VALUE_MEM) {
				X64_Value idx_as_gpr = x64_allocate_gpr(f, ctx, TB_NULL_REG, dt);
				x64_emit_normal64(out, MOV, &idx_as_gpr, &index);
				
				assert(stride == 1 || stride == 2 || stride == 4 || stride == 8);
				X64_Scale scale = __builtin_ffs(stride) - 1;
				
				return (X64_Value) {
					.type = X64_VALUE_MEM,
					.dt = dt,
					.mem = {
						.base = base_reg,
						.index = idx_as_gpr.gpr,
						.scale = scale,
						.disp = 0
					}
				};
			} else if (index.type == X64_VALUE_GPR) {
				assert(stride == 1 || stride == 2 || stride == 4 || stride == 8);
				X64_Scale scale = __builtin_ffs(stride) - 1;
				
				return (X64_Value) {
					.type = X64_VALUE_MEM,
					.dt = dt,
					.mem = {
						.base = base_reg,
						.index = index.gpr,
						.scale = scale,
						.disp = 0
					}
				};
			} else tb_todo();
		}
        case TB_SIGN_EXT: {
            X64_Value v = x64_eval(f, ctx, out, p.ext, r);
            X64_Value dst = x64_allocate_gpr(f, ctx, r, dt);
            
            // TODO(NeGate): Implement a sign extend recycle case e.g.
            // movsx eax, ax
            x64_emit_normal(out, dt.type, MOVSX, &dst, &v);
            return dst;
        }
        case TB_ZERO_EXT: {
            X64_Value v = x64_eval(f, ctx, out, p.ext, r);
            X64_Value dst = x64_allocate_gpr(f, ctx, r, dt);
            
            // TODO(NeGate): Implement a zero extend recycle case e.g.
            // movzx eax, ax
            x64_emit_normal(out, dt.type, MOVZX, &dst, &v);
            return dst;
        }
        case TB_ADD:
		case TB_AND:
		case TB_OR:
		case TB_SUB:
		case TB_MUL: {
            //bool can_lea_add = reg->i_arith.arith_behavior == TB_NO_WRAP 
			// || reg->i_arith.arith_behavior == TB_CAN_WRAP;
            
			const X64_IselInfo* info = NULL;
			switch (reg_type) {
				case TB_ADD: info = SELECTION_IADD; break;
				case TB_SUB: info = SELECTION_ISUB; break;
				case TB_MUL: info = SELECTION_IMUL; break;
				case TB_AND: info = SELECTION_AND; break;
				case TB_OR: info = SELECTION_OR; break;
				default: tb_todo();
			}
			
			TB_Register a_reg = p.i_arith.a;
			TB_Register b_reg = p.i_arith.b;
			
			X64_Value a = x64_legalize(f, ctx, out, a_reg, r);
			X64_Value b = (a_reg == b_reg) ? a : x64_legalize(f, ctx, out, b_reg, r);
			
			if (reg_type == TB_ADD && a.type == X64_VALUE_GPR && b.type == X64_VALUE_GPR) {
				// try LEA addition
				X64_Value dst;
				if (f->nodes.type[next] == TB_RET) {
					dst = (X64_Value){ .type = X64_VALUE_GPR, .dt = dt, .gpr = X64_RAX };
				} else {
					dst = x64_allocate_gpr(f, ctx, r, dt);
				}
				
				// lea _0, [_1 + _2]
				uint8_t* out_buffer = tb_out_reserve(out, 4);
				
				// TODO(NeGate): Maybe allow for the 32bit version sometimes
				*out_buffer++ = x64_inst_rex(true, dst.gpr, b.gpr, a.gpr);
				*out_buffer++ = 0x8D;
				*out_buffer++ = x64_inst_mod_rx_rm(X64_MOD_INDIRECT, dst.gpr, X64_RSP);
				*out_buffer++ = x64_inst_mod_rx_rm(X64_SCALE_X1, (b.gpr & 7) == X64_RSP ? X64_RSP : b.gpr, a.gpr);
				
				tb_out_commit(out, 4);
				return dst;
			} else {
				X64_Value result = x64_std_isel(f, ctx, out, r, next, a_reg, b_reg, a, b, info, ctx->intervals[a_reg] == r);
				
				if (reg_type == TB_ADD || reg_type == TB_MUL) {
					switch (p.i_arith.arith_behavior) {
						case TB_NO_WRAP: break;
						case TB_CAN_WRAP: break;
						case TB_WRAP_CHECK: {
							// INTO, trap if overflow
							// NOTE(NeGate): 
							uint8_t* out_buffer = tb_out_reserve(out, 3);
							*out_buffer++ = 0x71;
							*out_buffer++ = 0x01;
							*out_buffer++ = 0xCC;
							tb_out_commit(out, 3);
							break;
						}
						case TB_SATURATED_UNSIGNED: {
							uint8_t* out_buffer = tb_out_reserve(out, 16);
							uint8_t* out_buffer_start = out_buffer;
							
							X64_Value temp = x64_allocate_gpr(f, ctx, TB_NULL_REG, TB_TYPE_I64(1));
							bool is_64bit = (dt.type == TB_I64 || dt.type == TB_PTR);
							
							// mov temp, -1 
							if (is_64bit || temp.gpr >= 8) *out_buffer++ = x64_inst_rex(false, 0x00, temp.gpr, 0x00);
							*out_buffer++ = 0xB8 + (temp.gpr & 0x7);
							*((uint32_t*)out_buffer) = 0xFFFFFFFF;
							out_buffer += 4;
							
							// cmovae result, temp
							if (is_64bit || result.gpr >= 8 || temp.gpr >= 8) *out_buffer++ = x64_inst_rex(false, result.gpr, temp.gpr, 0x00);
							*out_buffer++ = 0x0F;
							*out_buffer++ = 0x43;
							*out_buffer++ = x64_inst_mod_rx_rm(X64_MOD_DIRECT, result.gpr, temp.gpr);
							
							tb_out_commit(out, out_buffer - out_buffer_start);
							x64_free_gpr(ctx, temp.gpr);
							break;
						}
						case TB_SATURATED_SIGNED: tb_todo();
					}
				}
				
				return result;
			}
		}
		case TB_FADD:
		case TB_FSUB:
		case TB_FMUL:
		case TB_FDIV: {
			assert(dt.count == 1 || dt.count == 4);
			const X64_IselInfo* info = NULL;
			switch (reg_type) {
				case TB_FADD: info = (dt.count == 4) ? SELECTION_F32X4_ADD : SELECTION_F32_ADD; break;
				case TB_FSUB: info = (dt.count == 4) ? SELECTION_F32X4_SUB : SELECTION_F32_SUB; break;
				case TB_FMUL: info = (dt.count == 4) ? SELECTION_F32X4_MUL : SELECTION_F32_MUL; break;
				case TB_FDIV: info = (dt.count == 4) ? SELECTION_F32X4_DIV : SELECTION_F32_DIV; break;
				default: tb_todo();
			}
			
			TB_Register a_reg = p.i_arith.a;
			TB_Register b_reg = p.i_arith.b;
			
			X64_Value a = x64_legalize(f, ctx, out, a_reg, r);
			X64_Value b = (a_reg == b_reg) ? a : x64_legalize(f, ctx, out, b_reg, r);
			
			return x64_std_isel(f, ctx, out, r, next, a_reg, b_reg, a, b, info, ctx->intervals[a_reg] == r);
		}
		case TB_SHL: {
			TB_Register a = p.i_arith.a;
			TB_Register b = p.i_arith.b;
			
			X64_Value a_value = x64_eval(f, ctx, out, a, r);
			
			if (f->nodes.type[b] == TB_INT_CONST) {
				assert(f->nodes.payload[b].i_const.hi == 0);
				assert(f->nodes.payload[b].i_const.lo < 64);
				
				if (f->nodes.type[next] == TB_RET) {
					// The destination is now RAX since that'll save us a move
					// and we can also use RAX as a temporary without allocating
					// it.
					X64_Value dst = (X64_Value){
						.type = X64_VALUE_GPR,
						.dt = dt,
						.gpr = X64_RAX
					};
					
					if (a_value.type != X64_VALUE_GPR) {
						// move into a register
						x64_emit_normal(out, dt.type, MOV, &dst, &a_value);
					}
					
					bool is_64bit = dt.type == TB_I64 || dt.type == TB_PTR;
					if (f->nodes.payload[b].i_const.lo == 1) {
						// lea _0, [_1 + _1]
						uint8_t* out_buffer = tb_out_reserve(out, 4);
						
						*out_buffer++ = x64_inst_rex(is_64bit, dst.gpr, a_value.gpr, a_value.gpr);
						*out_buffer++ = 0x8D;
						*out_buffer++ = x64_inst_mod_rx_rm(X64_MOD_INDIRECT, dst.gpr, X64_RSP);
						*out_buffer++ = x64_inst_mod_rx_rm(X64_SCALE_X1, (a_value.gpr & 7) == X64_RSP ? X64_RSP : a_value.gpr, a_value.gpr);
						
						tb_out_commit(out, 4);
					} else {
						uint8_t* out_buffer = tb_out_reserve(out, 5);
						
						*out_buffer++ = x64_inst_rex(is_64bit, 0x00, dst.gpr, 0x00);
						*out_buffer++ = dt.type == TB_I8 ? 0xC0 : 0xC1;
						*out_buffer++ = x64_inst_mod_rx_rm(X64_MOD_DIRECT, 0x04, dst.gpr);
						*out_buffer++ = f->nodes.payload[b].i_const.lo;
						
						tb_out_commit(out, (dt.type == TB_I16) ? 5 : 4);
					}
					return dst;
				} else {
					X64_Value dst = x64_allocate_gpr(f, ctx, r, dt);
					x64_emit_normal(out, dt.type, MOV, &dst, &a_value);
					
					bool is_64bit = dt.type == TB_I64 || dt.type == TB_PTR;
					uint8_t* out_buffer = tb_out_reserve(out, 5);
					
					if (dt.type == TB_I16) *out_buffer++ = 0x66;
					*out_buffer++ = x64_inst_rex(is_64bit, 0x00, dst.gpr, 0x00);
					*out_buffer++ = dt.type == TB_I8 ? 0xC0 : 0xC1;
					*out_buffer++ = x64_inst_mod_rx_rm(X64_MOD_DIRECT, 0x04, dst.gpr);
					*out_buffer++ = f->nodes.payload[b].i_const.lo;
					
					tb_out_commit(out, (dt.type == TB_I16) ? 5 : 4);
					return dst;
				}
			}
			
			tb_todo();
		}
		case TB_LOCAL: {
			return (X64_Value) {
				.type = X64_VALUE_MEM,
				.dt = TB_TYPE_PTR(),
				.mem = {
					.base = X64_RBP,
					.index = X64_GPR_NONE,
					.scale = X64_SCALE_X1,
					.disp = x64_find_local(ctx, r)
				}
			};
		}
		case TB_PARAM_ADDR: {
			TB_Register param_reg = p.param_addr.param;
			TB_DataType param_dt = f->nodes.dt[p.param_addr.param];
			
			return (X64_Value) {
				.type = X64_VALUE_MEM,
				.dt = param_dt,
				.mem = {
					.base = X64_RBP,
					.index = X64_GPR_NONE,
					.scale = X64_SCALE_X1,
					.disp = x64_find_local(ctx, param_reg)
				}
			};
		}
		case TB_PARAM: {
			if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
				X64_Value mem = (X64_Value) {
					.type = X64_VALUE_MEM,
					.dt = dt,
					.mem = {
						.base = X64_RBP,
						.index = X64_GPR_NONE,
						.scale = X64_SCALE_X1,
						.disp = x64_find_local(ctx, r)
					}
				};
				X64_Value result = x64_allocate_gpr(f, ctx, r, dt);
				
				x64_emit_normal(out, dt.type, MOV, &result, &mem);
				return result;
			} else if (TB_IS_FLOAT_TYPE(dt.type)) {
				X64_Value mem = (X64_Value) {
					.type = X64_VALUE_MEM,
					.dt = dt,
					.mem = {
						.base = X64_RBP,
						.index = X64_GPR_NONE,
						.scale = X64_SCALE_X1,
						.disp = x64_find_local(ctx, r)
					}
				};
				X64_Value result = x64_allocate_gpr(f, ctx, r, dt);
				
				x64_emit_normal(out, dt.type, MOV, &result, &mem);
				return result;
			} else tb_todo();
		}
		case TB_CMP_EQ:
		case TB_CMP_NE:
		case TB_CMP_SLT:
		case TB_CMP_SLE:
		case TB_CMP_ULT:
		case TB_CMP_ULE: {
			TB_DataType cmp_dt = p.cmp.dt;
			
			X64_Value a = x64_eval(f, ctx, out, p.cmp.a, r);
			X64_Value b = x64_eval(f, ctx, out, p.cmp.b, r);
			
			bool invert = false;
			if (a.type == X64_VALUE_MEM && b.type == X64_VALUE_MEM) {
				X64_Value dst = x64_allocate_gpr(f, ctx, p.cmp.a, cmp_dt);
				x64_emit_normal(out, cmp_dt.type, MOV, &dst, &a);
				x64_emit_normal(out, cmp_dt.type, CMP, &dst, &b);
				x64_free_gpr(ctx, dst.gpr);
			}
			else {
				invert = (a.type == X64_VALUE_IMM32);
				
				if (invert) x64_emit_normal(out, cmp_dt.type, CMP, &b, &a);
				else x64_emit_normal(out, cmp_dt.type, CMP, &a, &b);
			}
			
			X64_Cond cc;
			switch (reg_type) {
				case TB_CMP_EQ: cc = X64_E; break;
				case TB_CMP_NE: cc = X64_NE; break;
				case TB_CMP_SLT: cc = X64_L; break;
				case TB_CMP_SLE: cc = X64_LE; break;
				case TB_CMP_ULT: cc = X64_B; break;
				case TB_CMP_ULE: cc = X64_BE; break;
				default: tb_todo();
			}
			
			cc ^= invert;
			
			// TODO(NeGate): Implement the case where the value is converted 
			// into a byte, IF nodes don't require it but it may come up in 
			// code.
			assert(f->nodes.type[next] == TB_IF);
			return (X64_Value) { .type = X64_VALUE_FLAGS, .cond = cc };
		}
		case TB_LOAD: {
			TB_Register addr_reg = p.load.address;
			
			loop(i, ctx->mem_cache_count) {
				if (ctx->mem_caches[i].address == addr_reg) {
					return ctx->mem_caches[i].value;
				}
			}
			
			// TODO(NeGate): Test this!
			return x64_eval(f, ctx, out, addr_reg, r);
		}
		default: tb_todo();
	}
}

// Is this a phi node? does it can the register `reg`?
static bool x64_is_phi_that_contains(TB_Function* f, TB_Register phi, TB_Register reg) {
	if (f->nodes.type[phi] == TB_PHI1) {
		return f->nodes.payload[phi].phi1.a == reg;
	} else if (f->nodes.type[phi] == TB_PHI2) {
		return f->nodes.payload[phi].phi2.a == reg || f->nodes.payload[phi].phi2.b == reg;
	} else {
		return false;
	}
}

static void x64_phi_store_into(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_DataType dt, X64_Value dst, TB_Register src_reg) {
	if (f->nodes.type[src_reg] == TB_ADD && x64_is_phi_that_contains(f, f->nodes.payload[src_reg].i_arith.a, src_reg)) {
		X64_Value b = x64_eval(f, ctx, out, f->nodes.payload[src_reg].i_arith.b, 0);
		x64_inst_bin_op(f, ctx, out, dt, &insts[X64_ADD], &dst, &b, f->nodes.payload[src_reg].i_arith.b);
	}
	else {
		X64_Value value = x64_eval(f, ctx, out, src_reg, 0);
		if (!x64_is_value_equals(&dst, &value)) {
			x64_inst_bin_op(f, ctx, out, dt, &insts[X64_MOV], &dst, &value, src_reg);
		}
	}
}

// Prepares to enter the basic block [label:terminator] from [from_label:from_label.terminator],
// This means saving any value that are accessed via PHI nodes.
static void x64_terminate_path(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register from_label, TB_Register label, TB_Register terminator) {
	for (size_t i = label; i <= terminator; i++) {
		if (f->nodes.type[i] == TB_PHI1 && f->nodes.payload[i].phi1.a_label == from_label) {
			X64_PhiValue* phi = x64_find_phi(ctx, f->nodes.payload[i].phi1.a);
			assert(f->nodes.type[f->nodes.payload[i].phi1.a] == TB_PHI2);
			assert(phi->value.type != X64_NONE);
			
			x64_phi_store_into(f, ctx, out, f->nodes.dt[i], phi->value, f->nodes.payload[i].phi1.a);
		}
		else if (f->nodes.type[i] == TB_PHI2) {
			assert(f->nodes.payload[i].phi2.a_label != f->nodes.payload[i].phi2.b_label);
			X64_PhiValue* phi = x64_find_phi(ctx, i);
			
			TB_Register src = 0;
			if (f->nodes.payload[i].phi2.a_label == from_label) src = f->nodes.payload[i].phi2.a;
			else if (f->nodes.payload[i].phi2.b_label == from_label) src = f->nodes.payload[i].phi2.b;
			else tb_todo();
			
			if (phi->value.type == X64_NONE) {
				// Attempt to recycle
				X64_Value src_value = x64_eval(f, ctx, out, src, 0);
				
				// if the value is a temporary from the initial BB then just convert it into
				// the PHI node storage
				if (src_value.type == X64_VALUE_GPR &&
					x64_is_temporary_of_bb(f, ctx, src_value.gpr, from_label, f->nodes.payload[from_label].label.terminator)) {
					// Recycle old value as PHI node
					phi->value = src_value;
				} else {
					// Initialize the new PHI node
					phi->value = x64_allocate_gpr(f, ctx, i, f->nodes.dt[i]);
					
					x64_inst_bin_op(f, ctx, out, f->nodes.dt[i], &insts[X64_MOV], &phi->value, &src_value, src);
				}
			} else {
				x64_phi_store_into(f, ctx, out, f->nodes.dt[i], phi->value, src);
			}
		}
	}
}

static X64_Value x64_explicit_load(TB_Function* f, X64_Context* ctx, TB_Emitter* out, X64_Value addr, TB_Register r, TB_Register addr_reg) {
	if (f->nodes.type[addr_reg] == TB_LOAD) {
		assert(addr.type == X64_VALUE_MEM);
		
		X64_Value dst;
		if (addr.mem.index != X64_GPR_NONE || addr.mem.base == X64_RSP || addr.mem.base == X64_RBP) {
			dst = x64_allocate_gpr(f, ctx, r, addr.dt);
		} else {
			// We can reuse the source base register
			// as the destination instead of allocating
			// a new register:
			// mov rax, qword [rax]
			X64_GPR base_gpr = addr.mem.base;
			TB_Register base_ir_reg = ctx->gpr_desc[base_gpr].bound_value;
			
			if (base_ir_reg && ctx->intervals[base_ir_reg] == r) {
				dst = (X64_Value) {
					.type = X64_VALUE_GPR,
					.dt = addr.dt,
					.gpr = base_gpr
				};
				
				// the register was recycled and thus `r` owns it now
				ctx->gpr_desc[base_gpr].bound_value = r;
			} else {
				dst = x64_allocate_gpr(f, ctx, r, addr.dt);
			}
		}
		
		x64_emit_normal(out, addr.dt.type, MOV, &dst, &addr);
		return (X64_Value) {
			.type = X64_VALUE_MEM,
			.dt = f->nodes.dt[r],
			.mem = {
				.base = dst.gpr,
				.index = X64_GPR_NONE,
				.scale = X64_SCALE_X1,
				.disp = 0
			}
		};
	} else {
		// Load `addr` into register
		X64_Value dst = x64_allocate_gpr(f, ctx, r, f->nodes.dt[r]);
		x64_emit_normal(out, f->nodes.dt[r].type, MOV, &dst, &addr);
		
		return dst;
	}
}

static X64_Value x64_as_bool(TB_Function* f, X64_Context* ctx, TB_Emitter* out, X64_Value src, TB_DataType src_dt) {
	if (src.type == X64_VALUE_FLAGS || src.type == X64_VALUE_IMM32) {
		return src;
	} else if (src.type == X64_VALUE_GPR) {
		x64_emit_normal(out, src_dt.type, TEST, &src, &src);
		
		return (X64_Value) { .type = X64_VALUE_FLAGS, .cond = X64_NE };
	} else if (src.type == X64_VALUE_MEM) {
		X64_Value imm = (X64_Value) {
			.type = X64_VALUE_IMM32,
			.dt = src_dt,
			.imm32 = 0
		};
		
		x64_emit_normal(out, src_dt.type, CMP, &src, &imm);
		return (X64_Value) { .type = X64_VALUE_FLAGS, .cond = X64_NE };
	} else tb_todo();
}

static X64_Value x64_eval_immediate(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, const TB_Int128* imm) {
	TB_DataType dt = f->nodes.dt[r];
	
	// x64 can only handle 32bit immediates within
	// normal instructions, if you want bigger an
	// explicit MOV is required and 128bit immediates
	// require 2 registers.
	if (imm->hi) {
		// register pair
		assert(dt.type == TB_I128);
		
		X64_Value pair = x64_allocate_gpr_pair(f, ctx, r, dt);
		x64_inst_mov_ri64(out, pair.gpr_pair.lo, imm->lo);
		x64_inst_mov_ri64(out, pair.gpr_pair.hi, imm->hi);
		return pair;
	} else if (imm->lo > UINT32_MAX) {
		// explicit mov
		assert(dt.type == TB_I64 || dt.type == TB_PTR || dt.type == TB_I128);
		
		X64_Value dst = x64_allocate_gpr(f, ctx, r, dt);
		x64_inst_mov_ri64(out, dst.gpr, imm->lo);
		return dst;
	}
	
	// 32bit immediate case
	return (X64_Value) {
		.type = X64_VALUE_IMM32,
		.dt = dt,
		.imm32 = imm->lo
	};
}

// TODO(NeGate): Implement a better float immediate system
// zeroes can be dealt with XORPS
static X64_Value x64_eval_float_immediate(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, float imm) {
	TB_DataType dt = f->nodes.dt[r];
	X64_Value dst = x64_allocate_xmm(ctx, r, dt);
	
	if (imm == 0.0f) {
		uint8_t* out_buffer = tb_out_reserve(out, 4);
		if (dst.xmm >= 8) {
			*out_buffer++ = x64_inst_rex(true, dst.xmm, dst.xmm, 0);
			*out_buffer++ = 0x0F;
			*out_buffer++ = 0x57;
			*out_buffer++ = x64_inst_mod_rx_rm(X64_MOD_DIRECT, dst.xmm, dst.xmm);
			tb_out_commit(out, 4);
		} else {
			*out_buffer++ = 0x0F;
			*out_buffer++ = 0x57;
			*out_buffer++ = x64_inst_mod_rx_rm(X64_MOD_DIRECT, dst.xmm, dst.xmm);
			tb_out_commit(out, 3);
		}
	} else {
		// Convert it to raw bits
		_Static_assert(sizeof(float) == sizeof(uint32_t), "Float needs to be a 32-bit single float!");
		union { float f; uint32_t i; } imm_cast = { .f = imm };
		uint32_t offset = tb_emit_const32_patch(f->module, f - f->module->functions.data, out->count + 4, imm_cast.i);
		
		// Load from RIP, patch and have it resolved later
		uint8_t* out_buffer = tb_out_reserve(out, 8);
		*out_buffer++ = 0xF3;
		*out_buffer++ = 0x0F;
		*out_buffer++ = 0x10;
		*out_buffer++ = ((dst.gpr & 7) << 3) | X64_RBP;
		
		*((uint32_t*)out_buffer) = offset;
		tb_out_commit(out, 8);
	}
	
	return dst;
}

static X64_Value x64_allocate_gpr(TB_Function* f, X64_Context* ctx, TB_Register reg, TB_DataType dt) {
	for (size_t i = 0; i < 14; i++) {
		X64_GPR gpr = GPR_PRIORITY_LIST[i];
		
		if (ctx->gpr_desc[gpr].bound_value == 0) {
			ctx->gpr_desc[gpr].bound_value = reg;
			
			// mark register as to be saved
			ctx->regs_to_save |= (1u << gpr) & ABI_CALLEE_SAVED;
			
			return (X64_Value) {
				.type = X64_VALUE_GPR,
				.dt = dt,
				.gpr = gpr
			};
		}
	}
	
	// TODO(NeGate): Make sure all places provide a function
	assert(f);
	
	if (reg) {
		// Try to find the basic block, needed for garbage collection
		TB_Register bb;
		for (TB_Register i = 0; i < reg; i++) {
			if (f->nodes.type[i] == TB_LABEL) bb = i;
		}
		
		// Garbage collect
		assert(bb != 0);
		TB_Register bb_end = f->nodes.payload[bb].label.terminator;
		if (x64_register_garbage_collect(f, ctx, bb, bb_end)) {
			// Tail call, try again
			return x64_allocate_gpr(f, ctx, reg, dt);
		}
	}
	
	// Spill GPRs
	// TODO(NeGate): Implement some smart LRU crap or something
	for (size_t i = 14; i--; i++) {
		X64_GPR gpr = GPR_PRIORITY_LIST[i];
		
		if (ctx->gpr_desc[gpr].bound_value != TB_REG_MAX) {
			ctx->gpr_desc[gpr].bound_value = reg;
			
			// mark register as to be saved
			ctx->regs_to_save |= (1u << gpr) & ABI_CALLEE_SAVED;
			
			return (X64_Value) {
				.type = X64_VALUE_GPR,
				.dt = dt,
				.gpr = gpr
			};
		}
	}
	
	abort();
}

static X64_Value x64_allocate_gpr_pair(TB_Function* f, X64_Context* ctx, TB_Register reg, TB_DataType dt) {
	X64_Value lo = x64_allocate_gpr(f, ctx, reg, TB_TYPE_I64(1));
	X64_Value hi = x64_allocate_gpr(f, ctx, reg, TB_TYPE_I64(1));
	
	return (X64_Value) {
		.type = X64_VALUE_GPR_PAIR,
		.dt = dt,
		.gpr_pair = { lo.gpr, hi.gpr }
	};
}

static X64_Value x64_allocate_xmm(X64_Context* ctx, TB_Register reg, TB_DataType dt) {
	for (unsigned int i = 0; i < 16; i++) {
		if (ctx->xmm_desc[i].bound_value == 0) {
			ctx->xmm_desc[i].bound_value = reg;
			
			return (X64_Value) {
				.type = X64_VALUE_XMM,
				.dt = dt,
				.xmm = i
			};
		}
	}
	
	// Spill XMMs
	tb_todo();
}

static void x64_free_xmm(X64_Context* ctx, X64_XMM xmm) {
	ctx->xmm_desc[xmm].bound_value = 0;
}

static void x64_free_gpr(X64_Context* ctx, X64_GPR gpr) {
	ctx->gpr_desc[gpr].bound_value = 0;
}

static void x64_inst_mov_ri64(TB_Emitter* out, X64_GPR dst, uint64_t imm) {
	uint8_t* out_buffer = tb_out_reserve(out, 10);
	
	*out_buffer++ = x64_inst_rex(true, 0x0, dst, 0);
	*out_buffer++ = 0xB8 + (dst & 0b111);
	
	*((uint64_t*)out_buffer) = imm;
	out_buffer += 8;
	
	tb_out_commit(out, 10);
}

// performs an OP a, b
// if both operands are memory, `a` is promoted to a GPR
static void x64_inst_bin_op(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_DataType dt, const X64_NormalInst* inst, const X64_Value* a, const X64_Value* b, TB_Register b_reg) {
	if (a->type == X64_VALUE_MEM && b->type == X64_VALUE_MEM) {
		X64_Value b_gpr = x64_allocate_gpr(f, ctx, b_reg, dt);
		
		x64_emit_normal64(out, MOV, &b_gpr, b);
		x64_inst_op(out, dt.type, inst, a, &b_gpr);
	} else {
		x64_inst_op(out, dt.type, inst, a, b);
	}
}

// NOTE(NeGate): Both arguments cannot be memory operands
void x64_inst_op(TB_Emitter* out, int dt_type, const X64_NormalInst* inst, const X64_Value* a, const X64_Value* b) {
	// x64 can only have up to 16bytes in one instruction
	uint8_t* out_buffer_start = tb_out_reserve(out, 16);
	uint8_t* out_buffer = out_buffer_start;
	
	bool dir = b->type == X64_VALUE_MEM;
	if (dir || inst->op == 0xAF) tb_swap(a, b);
	
	// operand size
	uint8_t sz = (dt_type != TB_I8);
	
	// All instructions that go through here are
	// based on the ModRxRm encoding so we do need
	// an RX and an RM (base, index, shift, disp)
	uint8_t base = 0;
	uint8_t rx = GPR_NONE;
	if (inst->ext == X64_EXT_NONE || inst->ext == X64_EXT_DEF) {
		assert(dt_type == TB_I8 || dt_type == TB_I16 || dt_type == TB_I32 || dt_type == TB_I64 || dt_type == TB_PTR);
		
		// the destination can only be a GPR, no direction flag
		bool is_gpr_only_dst = (inst->op & 1);
		bool dir_flag = dir != is_gpr_only_dst;
		
		// Address size prefix
		if (dt_type == TB_I8 || dt_type == TB_I16) {
			*out_buffer++ = 0x66;
		}
		
		// RX
		if (b->type == X64_VALUE_GPR) rx = b->gpr;
		else if (b->type == X64_VALUE_IMM32) rx = inst->rx_i;
		else __builtin_unreachable();
		
		// RM & REX
		bool is_64bit = (dt_type == TB_I64 || dt_type == TB_PTR);
		
		if (a->type == X64_VALUE_GPR) {
			base = a->gpr;
			
			if (base >= 8 || rx >= 8 || is_64bit) {
				*out_buffer++ = x64_inst_rex(is_64bit, rx, base, 0);
			}
		}
		else if (a->type == X64_VALUE_MEM) {
			base = a->mem.base;
			
			uint8_t rex_index = (a->mem.index != X64_GPR_NONE ? a->mem.index : 0);
			if (base >= 8 || rx >= 8 || rex_index >= 8 || is_64bit) {
				*out_buffer++ = x64_inst_rex(is_64bit, rx, base, rex_index);
			}
		}
		else __builtin_unreachable();
		
		// Opcode
		if (inst->ext == X64_EXT_DEF) {
			// DEF instructions can only be 32bit and 64bit
			assert(dt_type == TB_I32 || dt_type == TB_I64 || dt_type == TB_PTR);
			*out_buffer++ = 0x0F;
		}
		
		if (b->type == X64_VALUE_IMM32 && inst->op_i == 0 && inst->rx_i == 0) {
			// No immediate version
			__builtin_unreachable();
		}
		
		// Immediates have a custom opcode
		uint8_t op = b->type == X64_VALUE_IMM32 ? inst->op_i : inst->op;
		*out_buffer++ = op | sz | (dir_flag ? 2 : 0);
	}
	else if (inst->ext == X64_EXT_SSE_SS || inst->ext == X64_EXT_SSE_PS) {
		assert(b->type != X64_VALUE_IMM32);
		assert(dt_type == TB_F32 || dt_type == TB_F64);
		
		// TODO(NeGate): normal SSE instructions don't support store mode, except MOV__
		if (inst->op != 0x10 && a->type == X64_VALUE_MEM) assert(dir); 
		
		if (a->type == X64_VALUE_MEM) base = a->mem.base;
		else base = a->xmm;
		rx = b->xmm;
		
		// This is pretty nasty but essentially the normal SSE instructions are always
		// in the flipped form (except for MOV__)
		if (inst->op != 0x10 && a->type != X64_VALUE_MEM) tb_swap(base, rx);
		
		if (rx >= 8 || base >= 8) {
			*out_buffer++ = x64_inst_rex(true, rx, base, 0);
		}
		
		if (inst->ext == X64_EXT_SSE_SS) {
			*out_buffer++ = 0xF3;
		}
		*out_buffer++ = 0x0F;
		*out_buffer++ = inst->op == 0x10 ? inst->op + !dir : inst->op;
	}
	else __builtin_unreachable();
	
	// We forgot a case!
	assert(rx != GPR_NONE);
	
	// Operand encoding
	if (a->type == X64_VALUE_GPR || a->type == X64_VALUE_XMM) {
		*out_buffer++ = x64_inst_mod_rx_rm(X64_MOD_DIRECT, rx, base);
	} else if (a->type == X64_VALUE_MEM) {
		uint8_t index = a->mem.index;
		uint8_t scale = a->mem.scale;
		int32_t disp = a->mem.disp;
		
		bool needs_index = (index != GPR_NONE) || (base & 7) == X64_RSP;
		
		// If it needs an index, it'll put RSP into the base slot
		// and write the real base into the SIB
		uint8_t mod = X64_MOD_INDIRECT_DISP32;
		if (disp == 0) mod = X64_MOD_INDIRECT_DISP8;
		else if (disp == (int8_t)disp) mod = X64_MOD_INDIRECT_DISP8;
		
		*out_buffer++ = x64_inst_mod_rx_rm(mod, rx, needs_index ? X64_RSP : base);
		if (needs_index) {
			*out_buffer++ = x64_inst_mod_rx_rm(scale, (base & 7) == X64_RSP ? X64_RSP : index, base);
		}
		
		if (mod == X64_MOD_INDIRECT_DISP8) {
			*out_buffer++ = (int8_t)disp;
		}
		else if (mod == X64_MOD_INDIRECT_DISP32) {
			*((uint32_t*)out_buffer) = disp;
			out_buffer += 4;
		}
	} else __builtin_unreachable();
	
	if (b->type == X64_VALUE_IMM32) {
		if (dt_type == TB_I8) {
			assert(b->imm32 == (int8_t)b->imm32);
			*out_buffer++ = (int8_t)b->imm32;
		} else if (dt_type == TB_I16) {
			assert(b->imm32 == (int16_t)b->imm32);
			*((uint16_t*)out_buffer) = (int16_t)b->imm32;
			out_buffer += 2;
		} else {
			assert(dt_type == TB_I32 || dt_type == TB_I64 || dt_type == TB_PTR);
			*((uint32_t*)out_buffer) = b->imm32;
			out_buffer += 4;
		}
	}
	
	tb_out_commit(out, out_buffer - out_buffer_start);
}

static void x64_inst_nop(TB_Emitter* out, int count) {
	if (count == 0) return;
	
	/*
	NOPS lol
	90H                             nop
	66 90H                          data16 nop
	0F 1F 00H                       nop dword [rax]
	0F 1F 40 00H                    nop dword [rax + 0x00]
	0F 1F 44 00 00H                 nop dword [rax + rax + 0x00]
	66 0F 1F 44 00 00H              nop  word [rax + rax + 0x00]
	0F 1F 80 00 00 00 00H           nop dword [rax + 0x00000000]
	0F 1F 84 00 00 00 00 00H        nop dword [rax + rax + 0x00000000]
	66 0F 1F 84 00 00 00 00 00H     nop  word [rax + rax + 0x00000000]
	66 2E 0F 1F 84 00 00 00 00 00H  nop  word cs:[rax + rax + 0x00000000]
	*/
	
	int initial_count = count;
	uint8_t* out_buffer = tb_out_reserve(out, count);
	do {
		if (count >= 10) {
			memcpy(out_buffer, (uint8_t[10]) { 0x66, 0x2E, 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00 }, 10);
			out_buffer += 10;
			count -= 10;
		}
		
		if (count >= 9) {
			memcpy(out_buffer, (uint8_t[9]) { 0x66, 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00 }, 9);
			out_buffer += 9;
			count -= 9;
		}
		
		if (count >= 8) {
			memcpy(out_buffer, (uint8_t[8]) { 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00 }, 8);
			out_buffer += 8;
			count -= 8;
		}
		
		if (count >= 7) {
			memcpy(out_buffer, (uint8_t[7]) { 0x0F, 0x1F, 0x80, 0x00, 0x00, 0x00, 0x00 }, 7);
			out_buffer += 7;
			count -= 7;
		}
		
		if (count >= 6) {
			memcpy(out_buffer, (uint8_t[6]) { 0x66, 0x0F, 0x1F, 0x44, 0x00, 0x00 }, 6);
			out_buffer += 6;
			count -= 6;
		}
		
		if (count >= 5) {
			memcpy(out_buffer, (uint8_t[5]) { 0x0F, 0x1F, 0x44, 0x00, 0x00 }, 5);
			out_buffer += 5;
			count -= 5;
		}
		
		if (count >= 4) {
			memcpy(out_buffer, (uint8_t[4]) { 0x0F, 0x1F, 0x40, 0x00 }, 4);
			out_buffer += 4;
			count -= 4;
		}
		
		if (count >= 3) {
			memcpy(out_buffer, (uint8_t[3]) { 0x0F, 0x1F, 0x00 }, 3);
			out_buffer += 3;
			count -= 3;
		}
		
		if (count >= 2) {
			memcpy(out_buffer, (uint8_t[2]) { 0x66, 0x90 }, 2);
			out_buffer += 2;
			count -= 2;
		}
		
		if (count >= 1) {
			*out_buffer++ = 0x90;
			count -= 1;
		}
	} while (count);
	
	tb_out_commit(out, initial_count);
}

static char x64_value_type_to_pattern_char(X64_ValueType type) {
	switch (type) {
		case X64_VALUE_IMM32: return 'i';
		case X64_VALUE_GPR: return 'r';
		case X64_VALUE_XMM: return 'x';
		case X64_VALUE_MEM: return 'm';
		default: tb_todo();
	}
}

static X64_Value x64_legalize(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, TB_Register next) {
	X64_Value v = x64_eval(f, ctx, out, r, next);
	if (v.dt.type == TB_PTR && f->nodes.type[r] == TB_LOAD) {
		v.dt = f->nodes.dt[next];
	}
	assert(v.dt.type != TB_I8 && v.dt.type != TB_I16);
	return v;
}

static X64_Value x64_std_isel(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register dst_reg, TB_Register next_reg, TB_Register a_reg, TB_Register b_reg, X64_Value a, X64_Value b, const X64_IselInfo* info, bool can_recycle) {
	TB_DataType dst_dt = f->nodes.dt[dst_reg];
	
	// NOTE(NeGate): The operands are ordered for this magic?
	if (info->communitive && a.type < b.type) tb_swap(a, b);
	
	// If both source operands are memory addresses, promote one into a register
	if (a.type == X64_VALUE_MEM && b.type == X64_VALUE_MEM) {
		// TODO(NeGate): Design a special case for these
		// kinds of "generic loads"
		if (dst_dt.count > 1) {
			X64_Value temp = x64_allocate_xmm(ctx, a_reg, a.dt);
			x64_emit_normal(out, dst_dt.type, MOVAPS, &temp, &a);
			a = temp;
		} else if (dst_dt.type == TB_F32) {
			X64_Value temp = x64_allocate_xmm(ctx, a_reg, a.dt);
			x64_emit_normal(out, dst_dt.type, MOVSS, &temp, &a);
			a = temp;
		} else if (TB_IS_INTEGER_TYPE(dst_dt.type)) {
			X64_Value temp = x64_allocate_gpr(f, ctx, a_reg, a.dt);
			x64_emit_normal(out, dst_dt.type, MOV, &temp, &a);
			a = temp;
		} else tb_todo();
	}
	
	// Some instructions don't have an immediate form
	if (!info->has_immediates && b.type == X64_VALUE_IMM32) {
		assert(TB_IS_INTEGER_TYPE(b.dt.type) || b.dt.type == TB_PTR);
		
		X64_Value temp = x64_allocate_gpr(f, ctx, a_reg, a.dt);
		x64_emit_normal(out, dst_dt.type, MOV, &temp, &a);
		b = temp;
	}
	
	bool recycle = a.type != X64_VALUE_IMM32 && can_recycle;
	if (!info->has_memory_dst && a.type == X64_VALUE_MEM) recycle = false;
	
	X64_Value dst;
	if (recycle) dst = a;
	else {
		// NOTE(NeGate): This is a special case before returning where you can forcefully
		// use a register that's in use so long as the two direct inputs are not using it.
		bool can_use_rax = f->nodes.type[next_reg] == TB_RET && !x64_is_value_gpr(&a, X64_RAX) && !x64_is_value_gpr(&b, X64_RAX);
		
		if ((TB_IS_INTEGER_TYPE(dst_dt.type) || dst_dt.type == TB_PTR) && can_use_rax) {
			dst = (X64_Value){ .type = X64_VALUE_GPR, .dt = dst_dt, .gpr = X64_RAX };
		} else {
			dst = x64_allocate_gpr(f, ctx, dst_reg, dst_dt);
		}
		
		// TODO(NeGate): Design a special case for these
		// kinds of "generic loads"
		if (dst_dt.count > 1) {
			X64_Value temp = x64_allocate_xmm(ctx, a_reg, a.dt);
			x64_emit_normal(out, dst_dt.type, MOVAPS, &dst, &a);
			a = temp;
		} else if (dst_dt.type == TB_F32) {
			X64_Value temp = x64_allocate_xmm(ctx, a_reg, a.dt);
			x64_emit_normal(out, dst_dt.type, MOVSS, &dst, &a);
			a = temp;
		} else if (TB_IS_INTEGER_TYPE(dst_dt.type)) {
			X64_Value temp = x64_allocate_gpr(f, ctx, a_reg, a.dt);
			x64_emit_normal(out, dst_dt.type, MOV, &dst, &a);
			a = temp;
		} else tb_todo();
	}
	
	x64_inst_op(out, dst_dt.type, &insts[info->inst], &dst, &b);
	return dst;
}

static uint32_t x64_get_data_type_size(TB_DataType dt) {
	switch (dt.type) {
		// TODO(NeGate): Cannot pass void or boolean via parameter
		case TB_VOID:
		case TB_BOOL:
		return 0;
		case TB_I8: 
		return 1 * dt.count;
		case TB_I16: 
		return 2 * dt.count;
		case TB_I32: case TB_F32:
		return 4 * dt.count;
		case TB_I64: case TB_F64:
		return 8 * dt.count;
		default: tb_todo();
	}
}

static int32_t x64_allocate_locals(TB_Function* f, X64_Context* ctx, TB_Emitter* out, int32_t* param_space, bool* out_saves_parameters) {
	int32_t stack_usage = 0;
	bool saves_parameters = false;
	
	// Allocate parameters
	loop(i, f->nodes.count) {
		if (f->nodes.type[i] == TB_PARAM) {
			TB_DataType dt = f->nodes.dt[i];
			int id = f->nodes.payload[i].param.id;
			
			uint32_t size = x64_get_data_type_size(dt);
			stack_usage += size;
			stack_usage += (size - (stack_usage % size)) % size;
			
			if (id < 4) {
				if (TB_IS_FLOAT_TYPE(dt.type) || dt.count > 1) {
					ctx->xmm_desc[id].bound_value = i;
				} else if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
					ctx->gpr_desc[GPR_PARAMETERS[id]].bound_value = i;
				}
			}
			
			// should be the same
			assert(ctx->locals_count == id);
			ctx->locals[ctx->locals_count++] = (X64_LocalDesc) {
				.address = i,
				.disp = -stack_usage
			};
		}
	}
	
	*param_space = stack_usage;
	
	// Allocate parameter storage
	loop(i, f->nodes.count) if (f->nodes.type[i] == TB_PARAM_ADDR) {
		TB_Register param = f->nodes.payload[i].param_addr.param;
		
		TB_DataType dt = f->nodes.dt[param];
		int id = f->nodes.payload[param].param.id;
		
		if (id < 4) {
			saves_parameters = true;
			X64_Value dst = (X64_Value) {
				.type = X64_VALUE_MEM,
				.dt = dt,
				.mem = {
					.base = X64_RBP,
					.index = X64_GPR_NONE,
					.scale = X64_SCALE_X1,
					.disp = ctx->locals[id].disp
				}
			};
			
			if (TB_IS_FLOAT_TYPE(dt.type) || dt.count > 1) {
				X64_Value src = (X64_Value) {
					.type = X64_VALUE_XMM,
					.dt = dt,
					.xmm = id // the parameters map to XMM0-XMM3
				};
				
				// don't keep reference to XMM, we'll be using the memory
				// version only
				ctx->xmm_desc[id].bound_value = 0;
				
				// save the shadow space into the stack
				if (dt.type == TB_F32) x64_emit_normal(out, dt.type, MOVSS, &dst, &src);
				else tb_todo(); // TODO(NeGate): Implement movsd
			} else if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
				X64_Value src = (X64_Value) {
					.type = X64_VALUE_GPR,
					.dt = TB_TYPE_I64(1),
					.gpr = GPR_PARAMETERS[id]
				};
				
				// don't keep reference to GPR, we'll be using the memory
				// version only
				ctx->gpr_desc[GPR_PARAMETERS[id]].bound_value = 0;
				
				// save the shadow space into the stack
				x64_emit_normal(out, dt.type, MOV, &dst, &src);
			} else tb_todo();
		}
	}
	
	// Allocate locals
	loop(i, f->nodes.count) {
		if (f->nodes.type[i] == TB_LOCAL) {
			uint32_t size = f->nodes.payload[i].local.size;
			uint32_t align = f->nodes.payload[i].local.alignment;
			
			// Increment and align
			stack_usage += size;
			stack_usage += (align - (stack_usage % align)) % align;
			
			//printf("Stack alloc: %u bytes (%u align)\n", size, align);
			
			ctx->locals[ctx->locals_count++] = (X64_LocalDesc) {
				.address = i,
				.disp = -stack_usage
			};
		}
	}
	
	*out_saves_parameters = saves_parameters;
	return stack_usage;
}

static void x64_create_phi_lookup(TB_Function* f, X64_Context* ctx) {
	// Generate PHI lookup table
	loop(i, f->nodes.count) {
		if (f->nodes.type[i] == TB_PHI1) {
			ctx->phis[ctx->phi_count++] = (X64_PhiValue){
				.reg = i, .storage_a = f->nodes.payload[i].phi1.a
			};
		}
		else if (f->nodes.type[i] == TB_PHI2) {
			ctx->phis[ctx->phi_count++] = (X64_PhiValue){
				.reg = i, .storage_a = f->nodes.payload[i].phi2.a, .storage_b = f->nodes.payload[i].phi2.b
			};
		}
	}
}

static int32_t x64_find_local(X64_Context* ctx, TB_Register r) {
	loop(i, ctx->locals_count) {
		if (ctx->locals[i].address == r) return ctx->locals[i].disp;
	}
	
	tb_todo();
}

static X64_PhiValue* x64_find_phi(X64_Context* ctx, TB_Register r) {
	loop(i, ctx->phi_count) {
		if (ctx->phis[i].reg == r) return &ctx->phis[i];
	}
	
	return NULL;
}

static bool x64_is_temporary_of_bb(TB_Function* f, X64_Context* ctx, X64_GPR gpr, TB_Register bb, TB_Register bb_end) {
	TB_Register r = ctx->gpr_desc[gpr].bound_value;
	
	if (r >= bb &&
		r <= bb_end &&
		f->nodes.type[r] != TB_PHI1 &&
		f->nodes.type[r] != TB_PHI2) {
		return true;
	}
	
	return false;
}

static bool x64_register_garbage_collect(TB_Function* f, X64_Context* ctx, TB_Register bb, TB_Register bb_end) {
	int changes = 0;
	
	for (size_t i = 0; i < 16; i++) {
		TB_Register r = ctx->gpr_desc[i].bound_value;
		if (r <= bb || r > bb_end) continue;
		if (f->nodes.type[r] == TB_PHI1) continue;
		if (f->nodes.type[r] == TB_PHI2) continue;
		
		if (ctx->intervals[r] < bb_end) {
			// kill it
			//printf("Killed GPR: %zu (bound: r%u)\n", i, r);
			ctx->gpr_desc[i].bound_value = 0;
			changes++;
		}
	}
	
	return changes;
}

// I put it down here because i can :P
ICodeGen x64_fast_code_gen = {
	.get_prologue_length = x64_get_prologue_length,
	.get_epilogue_length = x64_get_epilogue_length,
	.emit_prologue = x64_emit_prologue,
	.emit_epilogue = x64_emit_epilogue,
	.compile_function = x64_compile_function
};
