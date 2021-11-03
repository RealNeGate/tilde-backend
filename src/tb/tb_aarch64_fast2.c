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
static uint32_t aarch64_inst_mov(uint8_t dst, uint8_t src, bool _64bit);
static uint32_t aarch64_inst_movz(uint8_t dst, uint16_t imm, uint8_t shift, bool _64bit);
static uint32_t aarch64_inst_ldur(uint8_t size, uint8_t rt, uint8_t rn, int32_t imm);
static uint32_t aarch64_inst_stur(uint8_t size, uint8_t rt, uint8_t rn, int32_t imm);
static uint32_t aarch64_inst_dp_i(int op, uint8_t rd, uint8_t rn, uint16_t imm, uint8_t shift, bool _64bit);
static uint32_t aarch64_inst_dp_r(int op, uint8_t rd, uint8_t rn, uint8_t rm, uint16_t imm, uint8_t shift, bool _64bit);

// IR evaluation
static int aarch64_allocate_gpr(Aarch64_Context* ctx, TB_Register reg);
static void aarch64_eval_bb(TB_Function* f, Aarch64_Context* ctx, TB_Emitter* out, TB_Register bb, TB_Register bb_end);
static int aarch64_eval(TB_Function* f, Aarch64_Context* ctx, TB_Emitter* out, TB_Register r, TB_Register next);
static int aarch64_eval_bin_op(TB_Function* f, Aarch64_Context* ctx, TB_Emitter* out, int inst, TB_Register r, TB_Register a_reg, TB_Register b_reg, TB_Register next);
static int32_t aarch64_allocate_locals(TB_Function* f, Aarch64_Context* ctx, TB_Emitter* out);
static int32_t aarch64_find_local(Aarch64_Context* ctx, TB_Register r);

size_t aarch64_get_prologue_length(uint64_t saved, uint64_t stack_usage) {
	return stack_usage == 0 ? 0 : 4;
}

size_t aarch64_get_epilogue_length(uint64_t saved, uint64_t stack_usage) {
	return stack_usage == 0 ? 4 : 8;
}

size_t aarch64_emit_prologue(char out[64], uint64_t saved, uint64_t stack_usage) {
	// If the stack usage is zero we don't stack allocation
	if (stack_usage == 0) return 0;
	
	size_t used = 0;
	
	// stack alloc
	*((uint32_t*)&out[used]) = aarch64_inst_dp_i(AARCH64_SUB, GPR_SP, GPR_SP, stack_usage, 0, true);
	used += 4;
	
	return used;
}

size_t aarch64_emit_epilogue(char out[64], uint64_t saved, uint64_t stack_usage) {
	size_t used = 0;
	
	// If the stack usage is zero we don't stack free
	if (stack_usage) {
		// stack free
		*((uint32_t*)&out[used]) = aarch64_inst_dp_i(AARCH64_ADD, GPR_SP, GPR_SP, stack_usage, 0, true);
		used += 4;
	}
	
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
	ctx->local_stack_usage = aarch64_allocate_locals(f, ctx, &out);
	
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
			tb_todo();
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
				} else tb_todo();
			}
			
			bb = bb_end + 1;
		}
		else tb_todo();
		
        // Terminate any cached values
        ctx->mem_cache_count = 0;
	} while (bb < f->count);
	
	int local_stack_usage = ctx->local_stack_usage;// + caller_usage_in_bytes;
	
    // Align stack usage to 16bytes and add 8 bytes for the return address
    local_stack_usage += (16 - (local_stack_usage % 16)) % 16;
    assert((local_stack_usage & 15) == 0);
	
	assert(ret_patch_count == 0);
	assert(label_patch_count == 0);
	
	// Relocate stack vars
	for (size_t i = 0; i < out.count; i += 4) {
		uint32_t inst = *((uint32_t*)&out.data[i]);
		uint32_t op = (inst >> 21u) & 0b111111111;
		uint32_t rn = (inst >> 5u) & 0b11111;
		
		if ((op == 0b111000000 || op == 0b111000010) && rn == GPR_SP) {
			// load or store instruction
			uint32_t disp = (inst >> 12u) & 0x1FF;
			disp = local_stack_usage - disp;
			
			inst &= ~(0x1FF << 12u);
			inst |= (disp & 0x1FF) << 12u;
			
			*((uint32_t*)&out.data[i]) = inst;
		}
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

static void aarch64_eval_bb(TB_Function* f, Aarch64_Context* ctx, TB_Emitter* out, TB_Register bb, TB_Register bb_end) {
	// Evaluate all side effect instructions
	loop_range(i, bb + 1, bb_end) {
		// TB_DataType dt = f->nodes[i].dt;
		
		switch (f->nodes[i].type) {
			case TB_NULL:
			case TB_INT_CONST:
			case TB_LOCAL:
			case TB_PARAM:
			case TB_PARAM_ADDR:
			case TB_ADD:
			case TB_SUB:
			break;
			case TB_LOAD: {
				// Unlike x64 you can't fold memory operations into instructions
				// so we just load them immediately and explictly.
				TB_Register addr_reg = f->nodes[i].load.address;
				int dst = aarch64_allocate_gpr(ctx, i);
				
				int base = 0;
				int disp = 0;
				
				if (f->nodes[addr_reg].type == TB_LOCAL || 
					f->nodes[addr_reg].type == TB_PARAM_ADDR) {
					base = GPR_SP;
					disp = aarch64_find_local(ctx, addr_reg);
				} else tb_todo();
				
				TB_DataType dt = f->nodes[i].dt;
				if (dt.type == TB_I8) tb_out4b(out, aarch64_inst_ldur(0, dst, base, disp));
				else if (dt.type == TB_I16) tb_out4b(out, aarch64_inst_ldur(1, dst, base, disp));
				else if (dt.type == TB_I32) tb_out4b(out, aarch64_inst_ldur(2, dst, base, disp));
				else if (dt.type == TB_I64) tb_out4b(out, aarch64_inst_ldur(3, dst, base, disp));
				else tb_todo();
				
				ctx->mem_caches[ctx->mem_cache_count++] = (Aarch64_MemCacheDesc){
					.address = addr_reg,
					.storage = dst
				};
				break;
			}
			case TB_STORE: {
				TB_Register addr_reg = f->nodes[i].store.address;
				TB_Register value_reg = f->nodes[i].store.value;
				
				int base = 0;
				int disp = 0;
				if (f->nodes[addr_reg].type == TB_LOCAL || 
					f->nodes[addr_reg].type == TB_PARAM_ADDR) {
					base = GPR_SP;
					disp = aarch64_find_local(ctx, addr_reg);
				} else tb_todo();
				
				int src = aarch64_eval(f, ctx, out, value_reg, i);
				
				TB_DataType dt = f->nodes[i].dt;
				if (dt.type == TB_I8) tb_out4b(out, aarch64_inst_stur(0, src, base, disp));
				else if (dt.type == TB_I16) tb_out4b(out, aarch64_inst_stur(1, src, base, disp));
				else if (dt.type == TB_I32) tb_out4b(out, aarch64_inst_stur(2, src, base, disp));
				else if (dt.type == TB_I64) tb_out4b(out, aarch64_inst_stur(3, src, base, disp));
				else tb_todo();
				break;
			}
			default: tb_todo();
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
			int dst = (next && f->nodes[next].type == TB_RET) 
				? 0 : aarch64_allocate_gpr(ctx, r);
			
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
		case TB_PARAM: {
			// Check if it's in registers
			for (int i = 0; i < 16; i++) {
				TB_Register bound = ctx->gpr_desc[i].bound_value;
				if (r == bound) return i;
			}
			
			// check it's designated stack slot
			int stack_slot = aarch64_find_local(ctx, r);
			int dst = (next && f->nodes[next].type == TB_RET) 
				? 0 : aarch64_allocate_gpr(ctx, r);
			
			int dt_type = reg->dt.type;
			if (dt_type == TB_I8) tb_out4b(out, aarch64_inst_ldur(0, dst, GPR_SP, stack_slot));
			else if (dt_type == TB_I16) tb_out4b(out, aarch64_inst_ldur(1, dst, GPR_SP, stack_slot));
			else if (dt_type == TB_I32) tb_out4b(out, aarch64_inst_ldur(2, dst, GPR_SP, stack_slot));
			else if (dt_type == TB_I64) tb_out4b(out, aarch64_inst_ldur(3, dst, GPR_SP, stack_slot));
			else tb_todo();
			
			return dst;
		}
		case TB_ADD: 
		return aarch64_eval_bin_op(f, ctx, out, AARCH64_ADD, r, reg->i_arith.a, reg->i_arith.b, next);
		case TB_SUB: 
		return aarch64_eval_bin_op(f, ctx, out, AARCH64_SUBS, r, reg->i_arith.a, reg->i_arith.b, next);
        case TB_LOAD: {
            TB_Register addr_reg = reg->load.address;
            
            loop(i, ctx->mem_cache_count) {
                if (ctx->mem_caches[i].address == addr_reg) {
                    return ctx->mem_caches[i].storage;
                }
            }
            
			tb_todo();
        }
		default: tb_todo();
	}
}

static int aarch64_eval_bin_op(TB_Function* f, Aarch64_Context* ctx, TB_Emitter* out, int inst, TB_Register r, TB_Register a_reg, TB_Register b_reg, TB_Register next) {
	int dt_type = f->nodes[r].dt.type;
	
	int a = aarch64_eval(f, ctx, out, a_reg, r);
	int b = aarch64_eval(f, ctx, out, b_reg, r);
	
	// recycle a
	int dst = a;
	if (next && f->nodes[next].type == TB_RET) {
		// if we are about to return just put it into the return value.
		dst = 0;
	} else if (ctx->intervals[a_reg] != r) {
		dst = aarch64_allocate_gpr(ctx, r);
	}
	
	if (dt_type == TB_I32) tb_out4b(out, aarch64_inst_dp_r(inst, dst, a, b, 0, 0, false));
	else if (dt_type == TB_I64) tb_out4b(out, aarch64_inst_dp_r(inst, dst, a, b, 0, 0, true));
	else tb_todo();
	
	return dst;
}

static int aarch64_allocate_gpr(Aarch64_Context* ctx, TB_Register reg) {
	static const int GPR_PRIORITY_LIST[] = {
		// indirect return value address (this is where struct returns go)
		8,
		
		// these are caller saved so we can use them without
		// emitting spills
		9, 10, 11, 12, 13, 14, 15, 
		
		// these are the parameter values, ideally we don't override
		// them but eh
		// TODO(NeGate): experiment with reordering this for different
		// functions, maybe we wanna spill these backwards
		0, 1, 2, 3, 4, 5, 6, 7
	};
	
	for (unsigned int i = 0; i < tb_arrlen(GPR_PRIORITY_LIST); i++) {
		int gpr = GPR_PRIORITY_LIST[i];
		
		if (ctx->gpr_desc[gpr].bound_value == 0) {
			ctx->gpr_desc[gpr].bound_value = reg;
			
			// TODO(NeGate): mark register as to be saved
			// ctx->regs_to_save |= (1u << i) & ABI_CALLEE_SAVED;
			
			return gpr;
		}
	}
	
	// Spill GPRs
	tb_todo();
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

// rt is the destination
// rn is the base
static uint32_t aarch64_inst_ldur(uint8_t size, uint8_t rt, uint8_t rn, int32_t imm) {
	uint16_t imm9 = imm;
	uint32_t inst = 0;
    
	inst |= ((size & 3) << 30u);
	inst |= (0b111000010 << 21u);
    
	inst |= (imm9 & 0x1FF) << 12u;
	inst |= (rn & 0b11111) << 5u;
	inst |= (rt & 0b11111) << 0u;
	return inst;
}

// rt is the destination
// rn is the base
static uint32_t aarch64_inst_stur(uint8_t size, uint8_t rt, uint8_t rn, int32_t imm) {
	uint16_t imm9 = imm;
	uint32_t inst = 0;
    
	inst |= ((size & 3) << 30u);
	inst |= (0b111000000 << 21u);
    
	inst |= (imm9 & 0x1FF) << 12u;
	inst |= (rn & 0b11111) << 5u;
	inst |= (rt & 0b11111) << 0u;
	return inst;
}

static uint32_t aarch64_inst_dp_i(int op, uint8_t rd, uint8_t rn, uint16_t imm, uint8_t shift, bool _64bit) {
	uint32_t inst = (_64bit ? (1 << 31u) : 0);
    
	switch (op) {
        case AARCH64_AND:  inst |= (0b00010010) << 24u; break;
        case AARCH64_ADD:  inst |= (0b00010001) << 24u; break;
        case AARCH64_ADDS: inst |= (0b00110001) << 24u; break;
        case AARCH64_SUB:  inst |= (0b01010001) << 24u; break;
        case AARCH64_SUBS: inst |= (0b01110001) << 24u; break;
        default: abort();
	}
    
	if (op == AARCH64_ADD || op == AARCH64_SUB) {
		if (shift == 12) inst |= (shift << 22u);
		else if (shift == 0) {}
		else abort(); // Only options for reasons...
	}
    
	inst |= (imm & 0xFFF) << 10u;
	inst |= (rn & 0b11111) << 5u;
	inst |= (rd & 0b11111) << 0u;
    
	return inst;
}

static uint32_t aarch64_inst_dp_r(int op, uint8_t rd, uint8_t rn, uint8_t rm, uint16_t imm, uint8_t shift, bool _64bit) {
	uint32_t inst = (_64bit ? (1 << 31u) : 0);
    
	switch (op) {
        case AARCH64_AND:  inst |= (0b00001010) << 24u; break;
        case AARCH64_ADD:  inst |= (0b00001011) << 24u; break;
        case AARCH64_ADDS: inst |= (0b00101011) << 24u; break;
        case AARCH64_SUB:  inst |= (0b01010001) << 24u; break;
        case AARCH64_SUBS: inst |= (0b01101011) << 24u; break;
        default: abort();
	}
    
	if (op == AARCH64_ADD || op == AARCH64_SUB) {
		inst |= ((shift & 0b11) << 22u);
	}
    
	inst |= (rm & 0b111111) << 16u;
	inst |= (imm & 0b111111) << 10u;
	inst |= (rn & 0b11111) << 5u;
	inst |= (rd & 0b11111) << 0u;
    
	return inst;
}

static int32_t aarch64_allocate_locals(TB_Function* f, Aarch64_Context* ctx, TB_Emitter* out) {
	int32_t stack_usage = 0;
	
	loop(i, f->count) {
		if (f->nodes[i].type == TB_LOCAL) {
			uint32_t size = f->nodes[i].local.size;
			uint32_t align = f->nodes[i].local.alignment;
			
			// Increment and align
			int stack_slot = stack_usage;
			stack_slot += size;
			stack_slot += (align - (stack_usage % align)) % align;
			stack_usage = stack_slot;
			
			ctx->locals[ctx->locals_count++] = (Aarch64_LocalDesc) {
				.address = i,
				.disp = stack_slot
			};
		} else if (f->nodes[i].type == TB_PARAM_ADDR) {
			TB_Register param = f->nodes[i].param_addr.param;
			TB_DataType dt = f->nodes[param].dt;
			int id = f->nodes[param].param.id;
			
			uint32_t size = f->nodes[param].param_addr.size;
			uint32_t align = size;
			
			int stack_slot = stack_usage;
			stack_slot += size;
			stack_slot += (align - (stack_usage % align)) % align;
			stack_usage = stack_slot;
			
			ctx->locals[ctx->locals_count++] = (Aarch64_LocalDesc) {
				.address = i,
				.disp = stack_slot
			};
			
			if (id < 8) {
				if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
					// don't keep reference to GPR, we'll be using the memory
					// version only
					ctx->gpr_desc[id].bound_value = 0;
					
					// save the shadow space into the stack
					if (dt.type == TB_I8) tb_out4b(out, aarch64_inst_stur(0, id, GPR_SP, stack_slot));
					else if (dt.type == TB_I16) tb_out4b(out, aarch64_inst_stur(1, id, GPR_SP, stack_slot));
					else if (dt.type == TB_I32) tb_out4b(out, aarch64_inst_stur(2, id, GPR_SP, stack_slot));
					else if (dt.type == TB_I64) tb_out4b(out, aarch64_inst_stur(3, id, GPR_SP, stack_slot));
					else if (dt.type == TB_PTR) tb_out4b(out, aarch64_inst_stur(3, id, GPR_SP, stack_slot));
					else tb_todo();
				} else tb_todo();
			}
		} else if (f->nodes[i].type == TB_PARAM) {
			int id = f->nodes[i].param.id;
			TB_DataType dt = f->nodes[i].dt;
			
			if (id < 8) {
				if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
					ctx->gpr_desc[id].bound_value = i;
				} else tb_todo();
			}
		}
	}
	
	return stack_usage;
}

static int32_t aarch64_find_local(Aarch64_Context* ctx, TB_Register r) {
	loop(i, ctx->locals_count) {
		if (ctx->locals[i].address == r) return ctx->locals[i].disp;
	}
	
	tb_todo();
}

// I put it down here because i can :P
ICodeGen aarch64_fast_code_gen = {
	.get_prologue_length = aarch64_get_prologue_length,
	.get_epilogue_length = aarch64_get_epilogue_length,
	.emit_prologue = aarch64_emit_prologue,
	.emit_epilogue = aarch64_emit_epilogue,
	.compile_function = aarch64_compile_function
};
