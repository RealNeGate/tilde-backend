#define TB_INTERNAL
#include "tb.h"

#define GPR_ZR 0x1F
#define GPR_SP 0x1F
#define GPR_NONE 0xFF

typedef struct Aarch64_RegisterDesc {
	TB_Register bound_value;
} Aarch64_RegisterDesc;

typedef struct Aarch64_LocalDesc {
	TB_Register address;
    int32_t disp;
} Aarch64_LocalDesc;

typedef struct Aarch64_LabelPatch {
	int base;
	int pos;
    TB_Label target_lbl;
} Aarch64_LabelPatch;

typedef struct Aarch64_MemCacheDesc {
	TB_Register address;
    int storage;
} Aarch64_MemCacheDesc;

typedef struct Aarch64_PhiValue {
	TB_Register reg;
	TB_Register storage_a;
	TB_Register storage_b;
	
	// TODO(NeGate): Implement the phi storage method in such
	// a way where it supports memory and register storage.
} Aarch64_PhiValue;

typedef struct Aarch64_Context {
	size_t locals_count;
    size_t mem_cache_count, local_stack_usage;
	
	uint64_t regs_to_save;
	size_t* intervals;
	Aarch64_PhiValue* phis;
    Aarch64_MemCacheDesc* mem_caches;
	
	Aarch64_LocalDesc* locals;
	Aarch64_RegisterDesc gpr_desc[32];
} Aarch64_Context;

enum {
	AARCH64_AND,
	AARCH64_ADD,
	AARCH64_ADDS,
	AARCH64_SUB,
	AARCH64_SUBS,
};

enum {
	AARCH64_SHIFT_LSL,
	AARCH64_SHIFT_LSR,
	AARCH64_SHIFT_ASR,
	AARCH64_SHIFT_RESERVED
};

// Instructions
static uint32_t aarch64_inst_ret();
static uint32_t aarch64_inst_mov(uint8_t dst, uint8_t src, bool _64bit);
static uint32_t aarch64_inst_movz(uint8_t dst, uint16_t imm, uint8_t shift, bool _64bit);

// IR evaluation
static int aarch64_allocate_gpr(Aarch64_Context* ctx, TB_Register reg);
static void aarch64_eval_bb(TB_Function* f, Aarch64_Context* ctx, TB_Emitter* out, TB_Register bb, TB_Register bb_end);
static int aarch64_eval(TB_Function* f, Aarch64_Context* ctx, TB_Emitter* out, TB_Register r, TB_Register next);

size_t aarch64_get_prologue_length(uint64_t saved, uint64_t stack_usage) {
	return 0;
}

size_t aarch64_get_epilogue_length(uint64_t saved, uint64_t stack_usage) {
	return 4;
}

size_t aarch64_emit_prologue(char out[64], uint64_t saved, uint64_t stack_usage) {
	return 0;
}

size_t aarch64_emit_epilogue(char out[64], uint64_t saved, uint64_t stack_usage) {
	size_t used = 0;
	
	// ret instruction
	*((uint32_t*)&out[used]) = 0xD65F03C0;
	used += 4;
	
	return used;
}

TB_FunctionOutput aarch64_compile_function(TB_Function* f, const TB_FeatureSet* features) {
    TB_TemporaryStorage* tls = tb_tls_allocate();
    
    // Allocate all the TLS memory for the function
	Aarch64_Context* ctx;
	uint32_t* ret_patches;
	uint32_t* labels;
	Aarch64_LabelPatch* label_patches;
	
	uint32_t ret_patch_count = 0;
	uint32_t label_patch_count = 0;
    {
		uint32_t phi_count = 0;
		uint32_t locals_count = 0;
		uint32_t mem_cache_count = 0;
		uint32_t ir_return_count = 0;
		uint32_t ir_label_count = 0;
		uint32_t ir_label_patch_count = 0;
        
		for (size_t i = 1; i < f->count; i++) {
			if (f->nodes[i].type == TB_PHI1) phi_count++;
			else if (f->nodes[i].type == TB_PHI2) phi_count++;
			else if (f->nodes[i].type == TB_LOCAL) locals_count++;
			else if (f->nodes[i].type == TB_PARAM_ADDR) locals_count++;
			else if (f->nodes[i].type == TB_RET) ir_return_count++;
			else if (f->nodes[i].type == TB_LABEL) ir_label_count++;
			else if (f->nodes[i].type == TB_IF) ir_label_patch_count += 2;
			else if (f->nodes[i].type == TB_GOTO) ir_label_patch_count++;
			else if (f->nodes[i].type == TB_LOAD) mem_cache_count++;
		}
		
		ctx = (Aarch64_Context*)tb_tls_push(tls, sizeof(Aarch64_Context));
        memset(ctx, 0, sizeof(Aarch64_Context));
		
		ctx->intervals = tb_tls_push(tls, f->count * sizeof(size_t));
        ctx->phis = tb_tls_push(tls, phi_count * sizeof(Aarch64_PhiValue));
        ctx->locals = tb_tls_push(tls, locals_count * sizeof(Aarch64_LocalDesc));
        ctx->mem_caches = tb_tls_push(tls, mem_cache_count * sizeof(Aarch64_MemCacheDesc));
		
        ret_patches = (uint32_t*)tb_tls_push(tls, ir_return_count * sizeof(uint32_t));
		labels = (uint32_t*)tb_tls_push(tls, ir_label_count * sizeof(uint32_t));
		label_patches = (Aarch64_LabelPatch*)tb_tls_push(tls, ir_label_patch_count * sizeof(Aarch64_LabelPatch));
	}
	
	tb_find_live_intervals(ctx->intervals, f);
	
    // Reserve zero register/stack pointer
	ctx->gpr_desc[15].bound_value = TB_REG_MAX; // reserved
	
	TB_Emitter out = { 0 };
	
	TB_Register bb = 1;
    do {
        assert(f->nodes[bb].type == TB_LABEL);
        TB_Label label_id = f->nodes[bb].label.id;
        TB_Register bb_end = f->nodes[bb].label.terminator;
		
        labels[label_id] = out.count;
		
		// Generate instructions from the side-effect nodes using
		// all the other nodes and then terminate the basic block
        if (bb < bb_end) aarch64_eval_bb(f, ctx, &out, bb, bb_end);
		
		if (f->nodes[bb_end].type == TB_IF) {
			tb_unreachable();
		}
		else if (f->nodes[bb_end].type == TB_RET) {
			TB_DataType dt = f->nodes[bb_end].dt;
			
			if (dt.type != TB_VOID) {
				int val = aarch64_eval(f, ctx, &out, f->nodes[bb_end].ret.value, bb_end);
				
				if (dt.type == TB_I8 ||
					dt.type == TB_I16 ||
					dt.type == TB_I32 ||
					dt.type == TB_I64 ||
					dt.type == TB_PTR) {
					// Integer return values are in X0
					if (val) tb_out4b(&out, aarch64_inst_mov(0, val, true));
				} else tb_unreachable();
			}
			
			bb = bb_end + 1;
		}
		else tb_unreachable();
	} while (bb < f->count);
	
	int local_stack_usage = ctx->local_stack_usage;// + caller_usage_in_bytes;
	
    // Align stack usage to 16bytes and add 8 bytes for the return address
    local_stack_usage += (16 - (local_stack_usage % 16)) % 16;
    assert((local_stack_usage & 15) == 0);
    local_stack_usage += 8;
	
	assert(ret_patch_count == 0);
	assert(label_patch_count == 0);
	
    // Trim code output memory
    out.capacity = out.count;
    out.data = realloc(out.data, out.capacity);
    if (!out.data) tb_unreachable(); // I don't know if this can even fail...
    
    return (TB_FunctionOutput) {
        .name = f->name,
        .emitter = out,
		.stack_usage = local_stack_usage,
		.prologue_epilogue_metadata = ctx->regs_to_save
    };
}

static void aarch64_eval_bb(TB_Function* f, Aarch64_Context* ctx, TB_Emitter* out, TB_Register bb, TB_Register bb_end) {
	// Evaluate all side effect instructions
	loop_range(i, bb + 1, bb_end) {
		// TB_DataType dt = f->nodes[i].dt;
		
		switch (f->nodes[i].type) {
			case TB_NULL:
			case TB_INT_CONST:
			break;
			default: tb_unreachable();
		}
	}
}

static int aarch64_eval(TB_Function* f, Aarch64_Context* ctx, TB_Emitter* out, TB_Register r, TB_Register next) {
    TB_Node* reg = &f->nodes[r];
    assert(reg->dt.count == 1);
    
    switch (reg->type) {
		case TB_INT_CONST: {
			// No 128bit support yet :(
			assert(reg->i_const.hi == 0);
			
			// TODO(NeGate): Investigate was to shrink immediate loads.
			uint64_t num = reg->i_const.lo;
			int dst = aarch64_allocate_gpr(ctx, r);
			
			if (num == 0) tb_out4b(out, aarch64_inst_mov(dst, GPR_ZR, true));
			else {
				tb_out4b(out, aarch64_inst_movz(dst, num & 0xFFFF, 0, true));
				
				if ((num >> 16) & 0xFFFF) { // Has any bits set in this range
					tb_out4b(out, aarch64_inst_movz(dst, (num >> 16) & 0xFFFF, 1, true));
				}
				
				if ((num >> 32) & 0xFFFF) { // Has any bits set in this range
					tb_out4b(out, aarch64_inst_movz(dst, (num >> 32) & 0xFFFF, 2, true));
				}
				
				if ((num >> 48) & 0xFFFF) { // Has any bits set in this range
					tb_out4b(out, aarch64_inst_movz(dst, (num >> 48) & 0xFFFF, 3, true));
				}
			}
			
			return dst;
		}
		default: tb_unreachable();
	}
}

static int aarch64_allocate_gpr(Aarch64_Context* ctx, TB_Register reg) {
	for (unsigned int i = 0; i < 14; i++) {
		if (ctx->gpr_desc[i].bound_value == 0) {
			ctx->gpr_desc[i].bound_value = reg;
			
			// TODO(NeGate): mark register as to be saved
			// ctx->regs_to_save |= (1u << i) & ABI_CALLEE_SAVED;
			
			return i;
		}
	}
	
	// Spill GPRs
	tb_unreachable();
}

static void aarch64_free_gpr(Aarch64_Context* ctx, int gpr) {
	ctx->gpr_desc[gpr].bound_value = 0;
}

static uint32_t aarch64_inst_mov(uint8_t dst, uint8_t src, bool _64bit) {
	uint32_t inst = (_64bit ? (1 << 31u) : 0);
    
	inst |= (0b00101010 << 24u);
    
	inst |= (src & 0b11111) << 16u;
	inst |= (0b11111) << 5u;
	inst |= (dst & 0b11111) << 0u;
    
	return inst;
}

static uint32_t aarch64_inst_movz(uint8_t dst, uint16_t imm, uint8_t shift, bool _64bit) {
	uint32_t inst = (_64bit ? (1 << 31u) : 0);
    
	inst |= (0b010100101 << 23u);
    
	inst |= shift << 21u;
	inst |= imm << 5u;
	inst |= (dst & 0b11111) << 0u;
    
	return inst;
}

static uint32_t aarch64_inst_ret() {
	return 0xD65F03C0;
}

// I put it down here because i can :P
ICodeGen aarch64_fast_code_gen = {
	.get_prologue_length = aarch64_get_prologue_length,
	.get_epilogue_length = aarch64_get_epilogue_length,
	.emit_prologue = aarch64_emit_prologue,
	.emit_epilogue = aarch64_emit_epilogue,
	.compile_function = aarch64_compile_function
};
