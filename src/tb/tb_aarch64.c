#define TB_INTERNAL
#include "tb.h"

#define GPR_ZR 0x1F
#define GPR_SP 0x1F
#define GPR_NONE 0xFF

typedef struct Aarch64_Value {
	enum {
		Aarch64_None,
		Aarch64_Integer,
		Aarch64_Local,
        
		Aarch64_CachedGPR,
		Aarch64_CachedFPR,
	} type : 8;
	// Has the cache deviated from the actual storage value
	bool is_mutated;
	TB_DataType dt;
	union {
		uint64_t num;
		uint8_t gpr;
		uint8_t fpr;
		int32_t local;
	};
} Aarch64_Value;

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
	AARCH64_SHIFT_RESERVED,
};

// rt is the destination
// rn is the base
static uint32_t aarch64_inst_ldur(uint8_t size, uint8_t rt, uint8_t rn, int32_t imm) {
	uint16_t imm9 = imm;
	uint32_t inst = 0;
    
	inst |= ((size & 3) << 30u);
	inst |= (0b111000010 << 21u);
    
	inst |= (imm9 & 0x1F) << 12u;
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

// rt is the destination
// rn is the base
// rm is the index
#if 0
static uint32_t aarch64_inst_ldr_rrr(uint8_t size, uint8_t rt, uint8_t rn, uint8_t rm) {
	uint32_t inst = 0b00111000011000000000100000000000;
    
	inst |= ((size & 3) << 30u);
    
	inst |= (rm & 0b11111) << 16u;
	inst |= (rn & 0b11111) << 5u;
	inst |= (rt & 0b11111) << 0u;
	return inst;
}
#endif

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

#if 0
static uint32_t aarch64_inst_movk(uint8_t dst, uint16_t imm, uint8_t shift, bool _64bit) {
	uint32_t inst = (_64bit ? (1 << 31u) : 0);
    
	inst |= (0b011100101 << 23u);
    
	inst |= shift << 21u;
	inst |= imm << 5u;
	inst |= (dst & 0b11111) << 0u;
    
	return inst;
}

static uint32_t aarch64_inst_nop() {
	return 0xD503201F;
}
#endif

static uint32_t aarch64_inst_ret() {
	return 0xD65F03C0;
}

static uint8_t aarch64_allocate_gpr(uint32_t allocator[]) {
	for (unsigned int i = 0; i < 30; i++) {
		if ((allocator[0] & (1u << i)) == 0) {
			allocator[0] |= (1u << i);
			return i;
		}
	}
    
	return -1;
}

// This version checks if your hint is available and if so uses it instead of picking any random slot
static uint8_t aarch64_allocate_gpr_hint(uint32_t allocator[], uint8_t desired) {
	if (desired != GPR_NONE && (allocator[0] & (1u << desired)) == 0) {
		allocator[0] |= (1u << desired);
		return desired;
	}
    
	// first try the 8-30
	for (unsigned int i = 8; i < 30; i++) {
		if ((allocator[0] & (1u << i)) == 0) {
			allocator[0] |= (1u << i);
			return i;
		}
	}
    
	// then try the 0-7
	for (unsigned int i = 0; i < 7; i++) {
		if ((allocator[0] & (1u << i)) == 0) {
			allocator[0] |= (1u << i);
			return i;
		}
	}
    
	return -1;
}


static void aarch64_free_gpr(uint32_t allocator[], uint8_t gpr) {
	allocator[0] &= ~(1u << gpr);
}

static uint8_t aarch64_load_into_gpr(TB_Emitter* out, uint32_t allocator[], const Aarch64_Value v, bool read_only) {
	if (v.type == Aarch64_CachedGPR) {
		if (!read_only) {
			// Needs a custom copy
			uint8_t dst = aarch64_allocate_gpr(allocator);
			tb_out4b(out, aarch64_inst_mov(dst, v.gpr, v.dt.type == TB_I64));
			return dst;
		}
        
		return v.gpr;
	}
	else if (v.type == Aarch64_Integer) {
		uint8_t dst = aarch64_allocate_gpr(allocator);
        
		if (v.num == 0) tb_out4b(out, aarch64_inst_mov(dst, GPR_ZR, true));
		else if (v.dt.type == TB_I64) {
			tb_out4b(out, aarch64_inst_movz(dst, v.num & 0xFFFF, 0, true));
            
			if ((v.num >> 16) & 0xFFFF) { // Has any bits set in this range
				tb_out4b(out, aarch64_inst_movz(dst, (v.num >> 16) & 0xFFFF, 1, true));
			}
            
			if ((v.num >> 32) & 0xFFFF) { // Has any bits set in this range
				tb_out4b(out, aarch64_inst_movz(dst, (v.num >> 32) & 0xFFFF, 2, true));
			}
            
			if ((v.num >> 48) & 0xFFFF) { // Has any bits set in this range
				tb_out4b(out, aarch64_inst_movz(dst, (v.num >> 48) & 0xFFFF, 3, true));
			}
		}
		else if (v.dt.type == TB_I32) {
			tb_out4b(out, aarch64_inst_movz(dst, v.num & 0xFFFF, 0, false));
            
			if ((v.num >> 16) & 0xFFFF) { // Has any bits set in this range
				tb_out4b(out, aarch64_inst_movz(dst, (v.num >> 16) & 0xFFFF, 1, false));
			}
		}
		else if (v.dt.type == TB_I16) {
			tb_out4b(out, aarch64_inst_movz(dst, v.num & 0xFFFF, 0, false));
		}
		else if (v.dt.type == TB_I8) {
			tb_out4b(out, aarch64_inst_movz(dst, v.num & 0xFF, 0, false));
		}
		else abort();
        
		return dst;
	}
	else {
		abort();
	}
}

static Aarch64_Value aarch64_compile_value(TB_Function* f, TB_Emitter* out, uint32_t allocator[], TB_Register r, uint8_t desired) {
	TB_Node* reg = &f->nodes[r];
    
	enum TB_RegisterType type = reg->type;
	TB_DataType dt = reg->dt;
	assert(dt.count == 1);
	assert(dt.type != TB_I128);
    
	switch (type) {
        case TB_INT_CONST: {
            return (Aarch64_Value) {
                .type = Aarch64_Integer,
				.dt = dt,
				.num = reg->i_const.lo,
				.is_mutated = false
            };
        }
        case TB_ADD: {
            uint8_t dst = aarch64_allocate_gpr_hint(allocator, desired);
            
            uint8_t a = aarch64_load_into_gpr(
                                              out, allocator, aarch64_compile_value(f, out, allocator, reg->i_arith.a, GPR_NONE), true
                                              );
            uint8_t b = aarch64_load_into_gpr(
                                              out, allocator, aarch64_compile_value(f, out, allocator, reg->i_arith.b, GPR_NONE), true
                                              );
            
            // If the data type is 16bit or 8bit, we have to mask the values.
            // There's a trick where we only mask one value and use a special
            // sub-register addition
            // 
            // Here's an example with 16bit addition:
            // AND TMP,B, #0xffff
            // ADD DST,TMP,A,UXTH
            bool extend_rule_signed = reg->i_arith.arith_behavior == TB_NO_WRAP;
            
            if (dt.type == TB_I8) {
                uint8_t tmp = aarch64_allocate_gpr(allocator);
                
                // 00FF size=64 length=07 rotation=00 N=1 immr=000000 imms=000111
                uint64_t bitmask_imm = (1 << 12) | (0 << 6) | (0b000111 & 0x3f);
                tb_out4b(out, aarch64_inst_dp_i(AARCH64_AND, dst, b, bitmask_imm, 0, false));
                
                uint32_t inst = aarch64_inst_dp_r(AARCH64_ADD, dst, tmp, a, 0, 0, false);
                inst |= (1 << 21u);
                
                if (extend_rule_signed) inst |= (0b100 << 13u); // UXTB
                else inst |= (0b000 << 13u); // SXTB
                
                tb_out4b(out, inst);
                
                aarch64_free_gpr(allocator, tmp);
            }
            else if (dt.type == TB_I16) {
                uint8_t tmp = aarch64_allocate_gpr(allocator);
                
                // FFFF size=64 length=15 rotation=00 N=1 immr=000000 imms=001111
                uint64_t bitmask_imm = (1 << 12) | (0 << 6) | (0b001111 & 0x3f);
                tb_out4b(out, aarch64_inst_dp_i(AARCH64_AND, dst, b, bitmask_imm, 0, false));
                
                uint32_t inst = aarch64_inst_dp_r(AARCH64_ADD, dst, tmp, a, 0, 0, false);
                inst |= (1 << 21u);
                
                if (extend_rule_signed) inst |= (0b101 << 13u); // UXTB
                else inst |= (0b001 << 13u); // SXTB
                
                tb_out4b(out, inst);
                
                aarch64_free_gpr(allocator, tmp);
            }
            else if (dt.type == TB_I32) tb_out4b(out, aarch64_inst_dp_r(AARCH64_ADD, dst, a, b, 0, 0, false));
            else if (dt.type == TB_I64) tb_out4b(out, aarch64_inst_dp_r(AARCH64_ADD, dst, a, b, 0, 0, true));
            else abort();
            
            return (Aarch64_Value) {
                .type = Aarch64_CachedGPR,
				.dt = dt,
				.gpr = dst,
				.is_mutated = false
            };
        }
        case TB_LOAD: {
            uint8_t dst = aarch64_allocate_gpr_hint(allocator, desired);
            Aarch64_Value addr = aarch64_compile_value(f, out, allocator, reg->store.address, 0);
            
            if (addr.type == Aarch64_Local) {
                if (dt.type == TB_I8) tb_out4b(out, aarch64_inst_ldur(0, dst, GPR_SP, addr.local));
                else if (dt.type == TB_I16) tb_out4b(out, aarch64_inst_ldur(1, dst, GPR_SP, addr.local));
                else if (dt.type == TB_I32) tb_out4b(out, aarch64_inst_ldur(2, dst, GPR_SP, addr.local));
                else if (dt.type == TB_I64) tb_out4b(out, aarch64_inst_ldur(3, dst, GPR_SP, addr.local));
                else abort();
            }
            else abort();
            
            return (Aarch64_Value) {
                .type = Aarch64_CachedGPR,
				.dt = dt,
				.gpr = dst,
				.is_mutated = false
            };
        }
        case TB_LOCAL:
		return (Aarch64_Value) {
			.type = Aarch64_Local,
            .dt = dt,
            .local = f->stack_usage - (f->parameter_stack_usage + reg->local.position + reg->local.size),
            .is_mutated = false
		};
        case TB_PARAM:
		// Both floats and integers only have 8 register slots for parameters
		if (reg->param.id < 8) {
			// TODO(NeGate): Implement floats
			assert(dt.type != TB_F32 && dt.type != TB_F64);
            
			// TODO(NeGate): Implement vectors
			assert(dt.count == 1);
            
			return (Aarch64_Value) {
				.type = Aarch64_CachedGPR,
                .dt = dt,
                .gpr = reg->param.id,
                .is_mutated = false
			};
		}
		else {
			// Past that it's all stack variables
			// which i haven't implemented yet
			abort();
		}
        case TB_PARAM_ADDR: {
            //size_t param_i = f->nodes[reg->param_addr.param].param.id;
            TB_DataType param_dt = f->nodes[reg->param_addr.param].dt;
            
            uint32_t param_size = reg->param_addr.size;
            uint32_t param_pos = reg->param_addr.position;
            
            return (Aarch64_Value) {
                .type = Aarch64_Local,
				.dt = param_dt,
				.local = f->stack_usage - (param_pos + param_size),
				.is_mutated = false
            };
        }
        default: abort();
	}
}

static TB_FunctionOutput aarch64_compile_function(TB_Function* f, const TB_FeatureSet* features) {
	uint32_t allocator[2] = {
		// GPR
		0,
		// FPR
		0
	};
    
	// This loop is supposed to reserve space for the parameters.
	// Aarch64 uses X0-X7 as the integer parameter and result registers
	// and D0-D7 as the float parameter and result registers
	for (size_t i = 0; i < f->parameter_count; i++) {
		assert(f->nodes[i].type == TB_PARAM);
		TB_DataType dt = f->nodes[i].dt;
        
		// TODO(NeGate): Implement the vector parameters
		assert(dt.count == 1);
        
		if (dt.type == TB_I8 || dt.type == TB_I16 || dt.type == TB_I32 || dt.type == TB_I64) {
			if (i < 8) allocator[0] |= (1u << i);
			else {
				// TODO(NeGate): Use stack
				assert(0);
			}
		}
		else if (dt.type == TB_F32 || dt.type == TB_F64) {
			if (i < 8) allocator[1] |= (1u << i);
			else {
				// TODO(NeGate): Use stack
				assert(0);
			}
		}
		else abort();
	}
    
	f->parameter_stack_usage = 0;
	for (size_t i = 0; i < f->count; i++) {
		if (f->nodes[i].type == TB_PARAM_ADDR) {
			TB_DataType dt = f->nodes[f->nodes[i].param_addr.param].dt;
            
			// TODO(NeGate): Implement the vector parameters
			if (dt.count != 1) assert(0);
            
			uint32_t size = f->nodes[i].param_addr.size;
			uint32_t align = f->nodes[i].param_addr.alignment;
            
			f->nodes[i].param_addr.position = f->parameter_stack_usage;
            
			f->parameter_stack_usage += size;
			uint32_t padding = (align - (f->parameter_stack_usage % align)) % align;
			f->parameter_stack_usage += padding;
		}
	}
    
	uint32_t padding = (16 - (f->locals_stack_usage % 16)) % 16;
	f->locals_stack_usage += padding;
    
	// Align stack usage to 16bytes
	f->stack_usage = f->parameter_stack_usage + f->locals_stack_usage;
	padding = (16 - (f->stack_usage % 16)) % 16;
	f->stack_usage += padding;
    
	TB_Emitter out = { 0 };
    
	if (f->stack_usage > 0) {
		tb_out4b(&out, aarch64_inst_dp_i(AARCH64_SUB, GPR_SP, GPR_SP, f->stack_usage, 0, true));
	}
    
	for (size_t i = 0; i < f->count; i++) {
		enum TB_RegisterType type = f->nodes[i].type;
		TB_DataType dt = f->nodes[i].dt;
        
		switch (type) {
            case TB_INT_CONST:
            case TB_ADD:
            case TB_LOCAL:
            case TB_LOAD:
            case TB_PARAM:
			break;
            case TB_STORE: {
                Aarch64_Value addr = aarch64_compile_value(f, &out, allocator, f->nodes[i].store.address, 0);
                uint8_t dst = aarch64_load_into_gpr(&out, allocator, aarch64_compile_value(f, &out, allocator, f->nodes[i].store.value, 0), true);
                
                if (addr.type == Aarch64_Local) {
                    if (dt.type == TB_I8) tb_out4b(&out, aarch64_inst_ldur(0, dst, GPR_SP, addr.local));
                    else if (dt.type == TB_I16) tb_out4b(&out, aarch64_inst_ldur(1, dst, GPR_SP, addr.local));
                    else if (dt.type == TB_I32) tb_out4b(&out, aarch64_inst_ldur(2, dst, GPR_SP, addr.local));
                    else if (dt.type == TB_I64) tb_out4b(&out, aarch64_inst_ldur(3, dst, GPR_SP, addr.local));
                    else abort();
                }
                else abort();
                
                break;
            }
            case TB_PARAM_ADDR: {
                size_t param_i = f->nodes[i].param_addr.param;
                
                if (param_i < 8) {
                    TB_DataType param_dt = f->nodes[f->nodes[i].param_addr.param].dt;
                    
                    uint32_t param_size = f->nodes[i].param_addr.size;
                    uint32_t param_pos = f->nodes[i].param_addr.position;
                    int32_t disp = f->stack_usage - (param_pos + param_size);
                    
                    if (param_dt.type == TB_I8
                        || param_dt.type == TB_I16
                        || param_dt.type == TB_I32
                        || param_dt.type == TB_I64) {
                        uint8_t size;
                        
                        if (param_dt.type == TB_I8) size = 0;
                        else if (param_dt.type == TB_I16) size = 1;
                        else if (param_dt.type == TB_I32) size = 2;
                        else if (param_dt.type == TB_I64) size = 3;
                        else abort();
                        
                        tb_out4b(&out, aarch64_inst_stur(size, param_i, GPR_SP, disp));
                    }
                }
                else abort();
                break;
            }
            case TB_RET: {
                Aarch64_Value dst = aarch64_compile_value(f, &out, allocator, f->nodes[i].ret.value, 0);
                
                if (dst.type != Aarch64_CachedGPR || (dst.type == Aarch64_CachedGPR && dst.gpr != 0)) {
                    uint8_t b = aarch64_load_into_gpr(&out, allocator, dst, true);
                    
                    tb_out4b(&out, aarch64_inst_mov(0, b, dst.dt.type == TB_I64));
                }
                break;
            }
            default:
			abort();
			break;
		}
	}
    
	if (f->stack_usage > 0) {
		tb_out4b(&out, aarch64_inst_dp_i(AARCH64_ADD, GPR_SP, GPR_SP, f->stack_usage, 0, true));
	}
	tb_out4b(&out, aarch64_inst_ret());
    
	return (TB_FunctionOutput) {
		.name = f->name,
        .emitter = out
	};
}
