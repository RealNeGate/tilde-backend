#include "tb_x86_64.h"

typedef enum X64_Mod {
	X64_MOD_INDIRECT = 0,        // [rax]
	X64_MOD_INDIRECT_DISP8 = 1,  // [rax + disp8]
	X64_MOD_INDIRECT_DISP32 = 2, // [rax + disp32]
	X64_MOD_DIRECT = 3,          // rax
} X64_Mod;

typedef enum X64_ValueType {
	X64_NONE,
    
    // Real encodable types
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

typedef struct X64_Context {
	size_t phi_count, locals_count, f32_patch_count;
    size_t cached_value_start, cached_value_end;
    
	size_t* intervals;
	X64_PhiValue* phis;
	X64_LocalDesc* locals;
    X64_F32Patch* f32_patches;
    
	X64_RegisterDesc gpr_desc[16];
	X64_RegisterDesc xmm_desc[16];
} X64_Context;

typedef enum X64_InstType {
    // Integer data processing
	X64_ADD, X64_AND, X64_SUB, X64_XOR, X64_CMP, X64_MOV,
    X64_TEST, X64_LEA, X64_IMUL, X64_MOVSX, X64_MOVZX,
    
    // Single Scalar
    X64_MOVSS, X64_ADDSS, X64_MULSS, X64_SUBSS, X64_DIVSS,
    X64_CMPSS
} X64_InstType;

typedef enum X64_ExtMode {
    // Normal
	X64_EXT_NONE,
    
    // DEF instructions have a 0F prefix
	X64_EXT_DEF,
    
    // SSE instructions have a F3 0F prefix
    X64_EXT_SSE,
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
	[X64_SUB] = { 0x2A, 0x80, 0x05 },
	[X64_XOR] = { 0x30, 0x80, 0x06 },
	[X64_CMP] = { 0x38, 0x80, 0x07 },
	[X64_MOV] = { 0x88, 0xC6, 0x00 },
	[X64_TEST] = { 0x84, 0xF6, 0x00 },
    
	[X64_LEA] = { 0x8D },
    
	[X64_IMUL] = { 0xAF, .ext = X64_EXT_DEF },
	[X64_MOVSX] = { 0xBE, .ext = X64_EXT_DEF },
	[X64_MOVZX] = { 0xB6, .ext = X64_EXT_DEF },
    
	[X64_MOVSS] = { 0x10, .ext = X64_EXT_SSE },
	[X64_ADDSS] = { 0x58, .ext = X64_EXT_SSE },
	[X64_MULSS] = { 0x59, .ext = X64_EXT_SSE },
	[X64_SUBSS] = { 0x5C, .ext = X64_EXT_SSE },
	[X64_DIVSS] = { 0x5E, .ext = X64_EXT_SSE },
	[X64_CMPSS] = { 0xC2, .ext = X64_EXT_SSE }
};

typedef struct X64_ISel_Pattern {
	int cost;
    
	const char* pattern;
	const uint8_t* fmt;
    
	// Recycle patterns only use 2 ops, a is ignored because
	// it's aliased with dst.
	bool recycle;
	bool forced_64bit;
} X64_ISel_Pattern;

static const X64_GPR GPR_PARAMETERS[] = {
	X64_RCX, X64_RDX, X64_R8, X64_R9
};

static const X64_GPR GPR_PRIORITY_LIST[] = {
	X64_RAX, X64_RCX, X64_RDX, X64_R8,
	X64_R9, X64_R10, X64_R11, X64_RDI,
	X64_RSI, X64_RBX, X64_R12, X64_R13,
	X64_R14, X64_R15
};

// Preprocessing stuff
static void x64_create_phi_lookup(TB_Function* f, X64_Context* ctx);
static int32_t x64_allocate_locals(TB_Function* f, X64_Context* ctx, TB_Emitter* out);

// IR -> Machine IR Lookups
static X64_PhiValue* x64_find_phi(X64_Context* ctx, TB_Register r);
static X64_PhiValue* x64_find_phi_values(X64_Context* ctx, TB_Register r);
static int32_t x64_find_local(X64_Context* ctx, TB_Register r);

// Machine code generation
static X64_Value x64_eval(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, TB_Register next);
static X64_Value x64_eval_immediate(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, const TB_Int128* imm);
static X64_Value x64_eval_float_immediate(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, float imm);
static X64_Value x64_as_memory_operand(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_DataType dt, TB_Register r);
static X64_Value x64_std_isel(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register dst_reg, TB_Register a_reg, TB_Register b_reg, TB_Register next_reg, const X64_ISel_Pattern patterns[], size_t pattern_count);
static X64_Value x64_as_bool(TB_Function* f, X64_Context* ctx, TB_Emitter* out, X64_Value src);

// x64 instruction emitter
static void x64_inst_mov_ri64(TB_Emitter* out, X64_GPR dst, uint64_t imm);
static void x64_inst_op(TB_Emitter* out, int dt_type, const X64_NormalInst* inst, const X64_Value* a, const X64_Value* b);
static void x64_micro_assemble(TB_Emitter* out, int dt_type, const uint8_t* format, const X64_Value* operands);
static void x64_inst_nop(TB_Emitter* out, int count);

#define x64_emit_normal(out, dt, op, a, b) x64_inst_op(out, dt, &insts[X64_ ## op], a, b)
#define x64_emit_normal8(out, op, a, b) x64_inst_op(out, TB_I8, &insts[X64_ ## op], a, b)
#define x64_emit_normal16(out, op, a, b) x64_inst_op(out, TB_I16, &insts[X64_ ## op], a, b)
#define x64_emit_normal32(out, op, a, b) x64_inst_op(out, TB_I32, &insts[X64_ ## op], a, b)
#define x64_emit_normal64(out, op, a, b) x64_inst_op(out, TB_I64, &insts[X64_ ## op], a, b)

// Register allocation
static X64_Value x64_allocate_gpr_pair(X64_Context* ctx, TB_Register reg, TB_DataType dt);
static X64_Value x64_allocate_gpr(X64_Context* ctx, TB_Register reg, TB_DataType dt);
static X64_Value x64_allocate_xmm(X64_Context* ctx, TB_Register reg, TB_DataType dt);
static void x64_free_xmm(X64_Context* ctx, X64_XMM gpr);
static void x64_free_gpr(X64_Context* ctx, X64_GPR gpr);

TB_FunctionOutput x64_compile_function(TB_Function* f, const TB_FeatureSet* features) {
    TB_TemporaryStorage* tls = tb_tls_allocate();
    
    // Allocate all the TLS memory for the function
	X64_Context* ctx;
	uint32_t* ret_patches;
	uint32_t* labels;
	X64_LabelPatch* label_patches;
    
	uint32_t ret_patch_count = 0;
	uint32_t label_patch_count = 0;
    {
		uint32_t phi_count = 0;
		uint32_t locals_count = 0;
		uint32_t ir_return_count = 0;
		uint32_t ir_label_count = 0;
		uint32_t ir_label_patch_count = 0;
        uint32_t ir_float_consts = 0;
        
		for (size_t i = 1; i < f->count; i++) {
			// Not counting the PHI1 because they aren't real PHI nodes
			if (f->nodes[i].type == TB_PHI2) phi_count++;
			else if (f->nodes[i].type == TB_LOCAL) locals_count++;
			else if (f->nodes[i].type == TB_RET) ir_return_count++;
			else if (f->nodes[i].type == TB_LABEL) ir_label_count++;
			else if (f->nodes[i].type == TB_IF) ir_label_patch_count += 2;
			else if (f->nodes[i].type == TB_GOTO) ir_label_patch_count++;
            else if (f->nodes[i].type == TB_FLOAT_CONST) ir_float_consts++;
		}
        
		ctx = (X64_Context*)tb_tls_push(tls, sizeof(X64_Context) + (phi_count * sizeof(X64_PhiValue)));
        memset(ctx, 0, sizeof(X64_Context));
		
        ctx->intervals = tb_tls_push(tls, f->count * sizeof(size_t));
        ctx->phis = tb_tls_push(tls, phi_count * sizeof(size_t));
        ctx->locals = tb_tls_push(tls, locals_count * sizeof(X64_LocalDesc));
        ctx->f32_patches = tb_tls_push(tls, ir_float_consts * sizeof(X64_LocalDesc));
        
        ret_patches = (uint32_t*)tb_tls_push(tls, ir_return_count * sizeof(uint32_t));
		labels = (uint32_t*)tb_tls_push(tls, ir_label_count * sizeof(uint32_t));
		label_patches = (X64_LabelPatch*)tb_tls_push(tls, ir_label_patch_count * sizeof(X64_LabelPatch));
    }
    
#if 0
    printf("Compiling %s...\n", f->name);
    printf("  IR space: %d nodes (%zu kB)\n", f->capacity, (f->capacity * sizeof(TB_Node)) / 1024);
    printf("  TLS usage: %d\n", tls->used);
#endif
    
	tb_find_live_intervals(ctx->intervals, f);
    x64_create_phi_lookup(f, ctx);
    
    // Reserve stack
	ctx->gpr_desc[X64_RSP].bound_value = TB_REG_MAX; // reserved
	if (!features->x64.omit_frame_pointer) ctx->gpr_desc[X64_RBP].bound_value = TB_REG_MAX; // reserved
    
    TB_Emitter out = { 0 };
    
    // Emit prologue
    // NOTE(NeGate): it might not be used but we assume
    // it will and if not we can just skip it in the file
    // output
    size_t prologue_patch = out.count + 3;
    {
        // sub rsp, 0 # patched later
        uint8_t* out_buffer = tb_out_reserve(&out, 7);
        out_buffer[0] = x64_inst_rex(true, 0x00, X64_RSP, 0);
        out_buffer[1] = 0x81;
        out_buffer[2] = x64_inst_mod_rx_rm(X64_MOD_DIRECT, 0x05, X64_RSP);
        *((uint32_t*)&out_buffer[3]) = 0;
        tb_out_commit(&out, 7);
    }
    
    int32_t local_stack_usage = x64_allocate_locals(f, ctx, &out);
    
    // Go through each basic block:
    // Generate instructions from the side-effect nodes using
    // all the other nodes and then terminate the basic block
    TB_Register bb = 1; // The initial label is always at r1
    do {
        assert(f->nodes[bb].type == TB_LABEL);
        TB_Label label_id = f->nodes[bb].label.id;
        TB_Register bb_end = f->nodes[bb].label.terminator;
        
        // Clear and initialize new cache
        //printf("Process BB: r%llu-r%llu\n", bb, bb_end);
        labels[label_id] = out.count;
        
        // Evaluate all side effect instructions
        // Don't eval if the basic block is empty
        if (bb != bb_end) loop_range(i, bb + 1, bb_end) {
            TB_DataType dt = f->nodes[i].dt;
            
            switch (f->nodes[i].type) {
                case TB_NULL:
                case TB_LOCAL: // the allocation is handled beforehand
                case TB_LOAD:
                case TB_PARAM:
                case TB_PARAM_ADDR:
                case TB_INT_CONST:
                case TB_FLOAT_CONST:
                case TB_SIGN_EXT:
                case TB_ZERO_EXT:
                case TB_ADD:
                case TB_SUB:
                case TB_MUL:
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
                break;
                case TB_STORE: {
                    // TODO(NeGate): Allow for patterns such as:
                    TB_Register address_reg = f->nodes[i].store.address;
                    TB_Register value_reg = f->nodes[i].store.value;
                    
                    // Eval address and cast to the correct type for the store
                    X64_Value address = x64_eval(f, ctx, &out, address_reg, i);
                    if (address.dt.type != TB_PTR) abort();
                    
                    // TODO(NeGate): Cast to store type
                    if (f->nodes[value_reg].type == TB_ADD 
                        && f->nodes[f->nodes[value_reg].i_arith.a].type == TB_LOAD
                        && f->nodes[f->nodes[value_reg].i_arith.a].load.address == address_reg) {
                        // *p = *p + a  => add Xword [p], a
                        X64_Value value = x64_eval(f, ctx, &out, f->nodes[value_reg].i_arith.b, i);
                        x64_emit_normal(&out, dt.type, ADD, &address, &value);
                    } else if (f->nodes[value_reg].type == TB_SUB 
                               && f->nodes[f->nodes[value_reg].i_arith.a].type == TB_LOAD
                               && f->nodes[f->nodes[value_reg].i_arith.a].load.address == address_reg) {
                        // *p = *p - a  => sub Xword [p], a
                        X64_Value value = x64_eval(f, ctx, &out, f->nodes[value_reg].i_arith.b, i);
                        x64_emit_normal(&out, dt.type, SUB, &address, &value);
                    } else {
                        X64_Value value = x64_eval(f, ctx, &out, value_reg, i);
                        
                        x64_emit_normal(&out, dt.type, MOV, &address, &value);
                    }
                    break;
                }
                default: 
                abort();
            }
        }
        
        // TODO(NeGate): Terminate any cached values
        // Handle the terminator node
        if (f->nodes[bb_end].type == TB_IF) {
            //TB_Register true_label_reg = tb_find_reg_from_label(f, f->nodes[i].if_.if_true);
            //TB_Register false_label_reg = tb_find_reg_from_label(f, f->nodes[i].if_.if_false);
            //TB_Register true_label_end = f->nodes[true_label_reg].label.terminator;
            //TB_Register false_label_end = f->nodes[false_label_reg].label.terminator;
            
            X64_Value cond = x64_eval(f, ctx, &out, f->nodes[bb_end].if_.cond, bb_end);
            
            TB_Register if_true = f->nodes[bb_end].if_.if_true;
            TB_Register if_false = f->nodes[bb_end].if_.if_false;
            
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
                cond = x64_as_bool(f, ctx, &out, cond);
                
                bool fallthrough = f->nodes[bb_end + 1].type == TB_LABEL 
                    && f->nodes[bb_end + 1].label.id == if_false;
                
                X64_Cond cc = cond.cond;
                if (f->nodes[bb_end + 1].type == TB_LABEL &&
                    f->nodes[bb_end + 1].label.id == if_true) {
                    tb_swap(if_true, if_false);
                    
                    cc ^= 1;
                    
                    fallthrough = true;
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
                
                if (!fallthrough) {
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
            
            bb = bb_end + 1;
        } else if (f->nodes[bb_end].type == TB_RET) {
            assert(f->nodes[bb_end].dt.type != TB_VOID);
            X64_Value value = x64_eval(f, ctx, &out, f->nodes[bb_end].ret.value, bb_end);
            
            if (value.dt.type == TB_I8 ||
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
                
                if (value.type != X64_VALUE_GPR || (value.type == X64_VALUE_GPR && value.gpr != X64_RAX)) {
                    x64_emit_normal(&out, value.dt.type, MOV, &dst, &value);
                }
            } else if (value.dt.type == TB_F32) {
                // Float results use XMM0
                X64_Value dst = (X64_Value){
                    .type = X64_VALUE_XMM,
                    .dt = value.dt,
                    .xmm = X64_XMM0
                };
                
                if (value.type != X64_VALUE_XMM || (value.type == X64_VALUE_XMM && value.gpr != X64_XMM0)) {
                    x64_emit_normal(&out, value.dt.type, MOV, &dst, &value);
                }
            } else abort();
            
            // Only jump if we aren't literally about to end the function
            if (bb_end + 1 != f->count) {
                ret_patches[ret_patch_count++] = out.count + 1;
                
                uint8_t* out_buffer = tb_out_reserve(&out, 5);
                out_buffer[0] = 0xE9;
                out_buffer[1] = 0x00;
                out_buffer[2] = 0x00;
                out_buffer[3] = 0x00;
                out_buffer[4] = 0x00;
                tb_out_commit(&out, 5);
            }
            
            bb = bb_end + 1;
        } else if (f->nodes[bb_end].type == TB_LABEL) {
            bb = bb_end;
        } else if (f->nodes[bb_end].type == TB_GOTO) {
            label_patches[label_patch_count++] = (X64_LabelPatch){
                .base = out.count, .pos = out.count + 1, .target_lbl = f->nodes[bb_end].goto_.label
            };
            
            uint8_t* out_buffer = tb_out_reserve(&out, 5);
            out_buffer[0] = 0xE9;
            out_buffer[1] = 0x00;
            out_buffer[2] = 0x00;
            out_buffer[3] = 0x00;
            out_buffer[4] = 0x00;
            tb_out_commit(&out, 5);
            
            bb = bb_end + 1;
        } else {
            abort(); // TODO
        }
    } while (bb != f->count);
    
    // Align stack usage to 16bytes and add 8 bytes for the return address
    local_stack_usage += (16 - (local_stack_usage % 16)) % 16;
    assert((local_stack_usage & 15) == 0);
    local_stack_usage += 8;
    
    bool has_stack_setup = (local_stack_usage > 8);
    
    // patch return
    for (int i = 0; i < ret_patch_count; i++) {
        uint32_t pos = ret_patches[i];
        
        *((uint32_t*)&out.data[pos]) = out.count - (pos + 4);
    }
    
    // Patch prologue (or just omit it)
    // and emit epilogue (or dont)
    if (has_stack_setup) {
        // patch prologue
        *((uint32_t*)&out.data[prologue_patch]) = local_stack_usage;
        
        // add rsp, stack_usage
        uint8_t* out_buffer = tb_out_reserve(&out, 7);
        out_buffer[0] = x64_inst_rex(true, 0x00, X64_RSP, 0);
        out_buffer[1] = 0x81;
        out_buffer[2] = x64_inst_mod_rx_rm(X64_MOD_DIRECT, 0x00, X64_RSP);
        *((uint32_t*)&out_buffer[3]) = local_stack_usage;
        tb_out_commit(&out, 7);
    }
    
    // patch labels
    for (size_t i = 0; i < label_patch_count; i++) {
        uint32_t pos = label_patches[i].pos;
        uint32_t target_lbl = label_patches[i].target_lbl;
        
        int32_t rel32 = labels[target_lbl] - (pos + 4);
        
        *((uint32_t*)&out.data[pos]) = rel32;
    }
    
    tb_out1b(&out, 0xC3); // ret
    
    // align to 16 bytes
    size_t actual_size = out.count;
    if (!has_stack_setup) actual_size -= 7; // prologue is 7 bytes long
    x64_inst_nop(&out, 16 - (actual_size % 16));
    
    // patch floats
    tb_out_reserve(&out, ctx->f32_patch_count * 4);
    
    for (size_t i = 0; i < ctx->f32_patch_count; i++) {
        int pos = ctx->f32_patches[i].base;
        
        *((uint32_t*)&out.data[pos]) = out.count - (pos + 4);
        
        struct {
            uint32_t u;
            float f;
        } flt_to_bits = { .f = ctx->f32_patches[i].value };
        
        tb_out4b_UNSAFE(&out, flt_to_bits.u);
    }
    
    // align to 16 bytes
    actual_size = out.count;
    if (!has_stack_setup) actual_size -= 7; // prologue is 7 bytes long
    x64_inst_nop(&out, 16 - (actual_size % 16));
    
    // Trim code output memory
    out.capacity = out.count;
    out.data = realloc(out.data, out.capacity);
    if (!out.data) abort(); // I don't know if this can even fail...
    
    return (TB_FunctionOutput) {
        .name = f->name,
        .has_no_prologue = !has_stack_setup,
        .emitter = out
    };
}

// LEA addition only works in NO_WRAP and CAN_WRAP
static const X64_ISel_Pattern IADD_PATTERNS[] = {
    { 1, "rrr", (uint8_t[]){ X64_LEA, 0, '[', 1, 2, 0x7F }, false, true },
    { 1, "rri", (uint8_t[]){ X64_LEA, 0, '[', 1, 2, 0x7F }, false, true },
    { 2, "rrr", (uint8_t[]){ X64_ADD, 0, 2, 0x7F }, true },
    { 2, "rri", (uint8_t[]){ X64_ADD, 0, 2, 0x7F }, true },
    { 3, "rrm", (uint8_t[]){ X64_ADD, 0, 2, 0x7F }, true },
    { 4, "rrm", (uint8_t[]){ X64_MOV, 0, 1, X64_ADD, 0, 2, 0x7F }, false },
    { 4, "rmr", (uint8_t[]){ X64_MOV, 0, 1, X64_ADD, 0, 2, 0x7F }, false },
    { 4, "rmi", (uint8_t[]){ X64_MOV, 0, 1, X64_ADD, 0, 2, 0x7F }, false }
};

static X64_Value x64_eval(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, TB_Register next) {
    TB_Node* reg = &f->nodes[r];
    //TB_DataType dt = reg->dt;
    assert(reg->dt.count == 1);
    
    switch (reg->type) {
        case TB_INT_CONST: {
            return x64_eval_immediate(f, ctx, out, r, &reg->i_const);
        }
        case TB_FLOAT_CONST: {
            return x64_eval_float_immediate(f, ctx, out, r, reg->f_const);
        }
        case TB_SIGN_EXT: {
            X64_Value v = x64_eval(f, ctx, out, reg->ext, r);
            X64_Value dst = x64_allocate_gpr(ctx, r, reg->dt);
            
            // TODO(NeGate): Implement a sign extend recycle case e.g.
            // movsx eax, ax
            x64_emit_normal(out, reg->dt.type, MOVSX, &dst, &v);
            return dst;
        }
        case TB_ZERO_EXT: {
            X64_Value v = x64_eval(f, ctx, out, reg->ext, r);
            X64_Value dst = x64_allocate_gpr(ctx, r, reg->dt);
            
            // TODO(NeGate): Implement a zero extend recycle case e.g.
            // movzx eax, ax
            x64_emit_normal(out, reg->dt.type, MOVZX, &dst, &v);
            return dst;
        }
        case TB_ADD: {
            bool can_lea_add = reg->i_arith.arith_behavior == TB_NO_WRAP 
                || reg->i_arith.arith_behavior == TB_CAN_WRAP;
            
            X64_Value result = x64_std_isel(
                                            f, ctx, out, r,
                                            reg->i_arith.a, reg->i_arith.b, next,
                                            IADD_PATTERNS + (can_lea_add ? 0 : 2),
                                            8 - (can_lea_add ? 0 : 2)
                                            );
            
            switch (reg->i_arith.arith_behavior) {
                case TB_NO_WRAP: break;
                case TB_CAN_WRAP: break;
                case TB_WRAP_CHECK: {
                    // INTO, trap if overflow
                    tb_out1b(out, 0xCE);
                    break;
                }
                case TB_SATURATED_UNSIGNED: {
                    X64_Value temp = x64_allocate_gpr(ctx, TB_NULL_REG, TB_TYPE_I64(1));
                    
                    uint8_t* out_buffer = tb_out_reserve(out, 11);
                    
                    // mov temp, -1 
                    *out_buffer++ = x64_inst_rex(true, 0x00, temp.gpr, 0x00);
                    *out_buffer++ = 0xC7;
                    *out_buffer++ = x64_inst_mod_rx_rm(X64_MOD_DIRECT, 0x00, temp.gpr);
                    *((uint32_t*)out_buffer) = 0xFFFFFFFF;
                    out_buffer += 4;
                    
                    // cmovae result, temp
                    *out_buffer++ = x64_inst_rex(true, result.gpr, temp.gpr, 0x00);
                    *out_buffer++ = 0x0F;
                    *out_buffer++ = 0x43;
                    *out_buffer++ = x64_inst_mod_rx_rm(X64_MOD_DIRECT, result.gpr, temp.gpr);
                    
                    tb_out_commit(out, 11);
                    
                    x64_free_gpr(ctx, temp.gpr);
                    break;
                }
                case TB_SATURATED_SIGNED: abort();
            }
            
            return result;
        }
        case TB_SUB: {
            return x64_std_isel(f, ctx, out, r, reg->i_arith.a, reg->i_arith.b, next, (X64_ISel_Pattern[]) {
                                    { 1, "rrr", (uint8_t[]){ X64_SUB, 0, 2, 0x7F }, true },
                                    { 2, "rri", (uint8_t[]){ X64_SUB, 0, 2, 0x7F }, true },
                                    { 3, "rrm", (uint8_t[]){ X64_SUB, 0, 2, 0x7F }, true },
                                    { 4, "rrm", (uint8_t[]){ X64_MOV, 0, 1, X64_SUB, 0, 2, 0x7F }, false },
                                    { 4, "rmr", (uint8_t[]){ X64_MOV, 0, 1, X64_SUB, 0, 2, 0x7F }, false },
                                    { 4, "rmi", (uint8_t[]){ X64_MOV, 0, 1, X64_SUB, 0, 2, 0x7F }, false }
                                }, 6);
        }
        case TB_MUL: {
            // Must be promoted up before it's multiplied
            assert(reg->dt.type == TB_I32 || reg->dt.type == TB_I64 || reg->dt.type == TB_PTR);
            
            return x64_std_isel(f, ctx, out, r, reg->i_arith.a, reg->i_arith.b, next, (X64_ISel_Pattern[]) {
                                    { 1, "rrr", (uint8_t[]){ X64_IMUL, 0, 2, 0x7F }, true },
                                    { 2, "rri", (uint8_t[]){ X64_IMUL, 0, 2, 0x7F }, true },
                                    { 3, "rrm", (uint8_t[]){ X64_IMUL, 0, 2, 0x7F }, true },
                                    { 4, "rrm", (uint8_t[]){ X64_MOV, 0, 1, X64_IMUL, 0, 2, 0x7F }, false },
                                    { 4, "rmr", (uint8_t[]){ X64_MOV, 0, 1, X64_IMUL, 0, 2, 0x7F }, false },
                                    { 4, "rmi", (uint8_t[]){ X64_MOV, 0, 1, X64_IMUL, 0, 2, 0x7F }, false }
                                }, 6);
        }
        case TB_FADD: {
            return x64_std_isel(f, ctx, out, r, reg->i_arith.a, reg->i_arith.b, next, 
                                (X64_ISel_Pattern[]) {
                                    { 1, "xxx", (uint8_t[]){ X64_ADDSS, 0, 2, 0x7F }, true },
                                    { 2, "xxm", (uint8_t[]){ X64_ADDSS, 0, 2, 0x7F }, true },
                                    { 3, "xmm", (uint8_t[]){ X64_MOVSS, 0, 1, X64_ADDSS, 0, 2, 0x7F }, false }
                                }, 3);
        }
        case TB_FSUB: {
            return x64_std_isel(f, ctx, out, r, reg->i_arith.a, reg->i_arith.b, next, 
                                (X64_ISel_Pattern[]) {
                                    { 1, "xxx", (uint8_t[]){ X64_SUBSS, 0, 2, 0x7F }, true },
                                    { 2, "xxm", (uint8_t[]){ X64_SUBSS, 0, 2, 0x7F }, true },
                                    { 3, "xmm", (uint8_t[]){ X64_MOVSS, 0, 1, X64_SUBSS, 0, 2, 0x7F }, false }
                                }, 3);
        }
        case TB_FMUL: {
            return x64_std_isel(f, ctx, out, r, reg->i_arith.a, reg->i_arith.b, next, 
                                (X64_ISel_Pattern[]) {
                                    { 1, "xxx", (uint8_t[]){ X64_MULSS, 0, 2, 0x7F }, true },
                                    { 2, "xxm", (uint8_t[]){ X64_MULSS, 0, 2, 0x7F }, true },
                                    { 3, "xmm", (uint8_t[]){ X64_MOVSS, 0, 1, X64_MULSS, 0, 2, 0x7F }, false }
                                }, 3);
        }
        case TB_FDIV: {
            return x64_std_isel(f, ctx, out, r, reg->i_arith.a, reg->i_arith.b, next, 
                                (X64_ISel_Pattern[]) {
                                    { 1, "xxx", (uint8_t[]){ X64_DIVSS, 0, 2, 0x7F }, true },
                                    { 2, "xxm", (uint8_t[]){ X64_DIVSS, 0, 2, 0x7F }, true },
                                    { 3, "xmm", (uint8_t[]){ X64_MOVSS, 0, 1, X64_DIVSS, 0, 2, 0x7F }, false }
                                }, 3);
        }
        case TB_LOCAL: {
            return (X64_Value) {
                .type = X64_VALUE_MEM,
                .dt = TB_TYPE_PTR(),
                .mem = {
                    .base = X64_RSP,
                    .index = X64_GPR_NONE,
                    .scale = X64_SCALE_X1,
                    .disp = x64_find_local(ctx, r)
                }
            };
        }
        case TB_PARAM_ADDR: {
            TB_DataType param_dt = f->nodes[reg->param_addr.param].dt;
            int id = f->nodes[reg->param_addr.param].param.id;
            
            return (X64_Value) {
                .type = X64_VALUE_MEM,
				.dt = param_dt,
                .mem = {
                    .base = X64_RSP,
                    .index = X64_GPR_NONE,
                    .scale = X64_SCALE_X1,
                    .disp = (1 + id) * 8
                }
            };
        }
        case TB_PARAM: {
            TB_DataType param_dt = reg->dt;
            
            if (reg->param.id < 4) {
                // TODO(NeGate): Implement floats
                assert(param_dt.type != TB_F32 && param_dt.type != TB_F64);
                
                return (X64_Value) {
                    .type = X64_VALUE_GPR,
                    .dt = param_dt,
                    .gpr = GPR_PARAMETERS[reg->param.id]
                };
            }
            else {
                // TODO(NeGate): Implement floats
                assert(param_dt.type != TB_F32 && param_dt.type != TB_F64);
                
                int id = reg->param.id;
                X64_Value mem = (X64_Value) {
                    .type = X64_VALUE_MEM,
                    .dt = param_dt,
                    .mem = {
                        .base = X64_RSP,
                        .index = X64_GPR_NONE,
                        .scale = X64_SCALE_X1,
                        .disp = (1 + id) * 8
                    }
                };
                X64_Value result = x64_allocate_gpr(ctx, r, param_dt);
                
                x64_emit_normal(out, param_dt.type, MOV, &result, &mem);
                return result;
            }
        }
        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE: {
            TB_DataType cmp_dt = reg->cmp.dt;
            
            X64_Value a = x64_eval(f, ctx, out, reg->cmp.a, r);
            X64_Value b = x64_eval(f, ctx, out, reg->cmp.b, r);
            
            bool invert = false;
            if (a.type == X64_VALUE_MEM && b.type == X64_VALUE_MEM) {
                X64_Value dst = x64_allocate_gpr(ctx, reg->cmp.a, cmp_dt);
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
            switch (reg->type) {
                case TB_CMP_EQ: cc = X64_E; break;
                case TB_CMP_NE: cc = X64_NE; break;
                case TB_CMP_SLT: cc = X64_L; break;
                case TB_CMP_SLE: cc = X64_LE; break;
                case TB_CMP_ULT: cc = X64_B; break;
                case TB_CMP_ULE: cc = X64_BE; break;
                default: abort();
            }
            
            cc ^= invert;
            
            // TODO(NeGate): Implement the case where the value is converted 
            // into a byte, IF nodes don't require it but it may come up in 
            // code.
            assert(f->nodes[next].type == TB_IF);
            return (X64_Value) { .type = X64_VALUE_FLAGS, .cond = cc };
        }
        case TB_LOAD: {
            X64_Value addr = x64_eval(f, ctx, out, reg->load.address, r);
            
            if (f->nodes[reg->load.address].type == TB_LOCAL ||
                f->nodes[reg->load.address].type == TB_PARAM_ADDR) {
                return addr;
            }
            else if (f->nodes[reg->load.address].type == TB_LOAD) {
                assert(addr.type == X64_VALUE_MEM);
                
                X64_Value dst;
                if (addr.mem.index != X64_GPR_NONE || addr.mem.base == X64_RSP || addr.mem.base == X64_RBP) {
                    dst = x64_allocate_gpr(ctx, r, addr.dt);
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
                        dst = x64_allocate_gpr(ctx, r, addr.dt);
                    }
                }
                
                x64_emit_normal(out, addr.dt.type, MOV, &dst, &addr);
                return (X64_Value) {
                    .type = X64_VALUE_MEM,
                    .dt = reg->dt,
                    .mem = {
                        .base = dst.gpr,
                        .index = X64_GPR_NONE,
                        .scale = X64_SCALE_X1,
                        .disp = 0
                    }
                };
            }
            else {
                // Load `addr` into register
                X64_Value dst = x64_allocate_gpr(ctx, r, reg->dt);
                x64_emit_normal(out, reg->dt.type, MOV, &dst, &addr);
                
                return dst;
            }
        }
        default: abort();
    }
}

static X64_Value x64_as_bool(TB_Function* f, X64_Context* ctx, TB_Emitter* out, X64_Value src) {
    if (src.type == X64_VALUE_FLAGS || src.type == X64_VALUE_IMM32) {
        return src;
    } else if (src.type == X64_VALUE_GPR) {
        x64_emit_normal(out, src.dt.type, TEST, &src, &src);
        
        return (X64_Value) { .type = X64_VALUE_FLAGS, .cond = X64_NE };
    } else if (src.type == X64_VALUE_MEM) {
        X64_Value imm = (X64_Value) {
            .type = X64_VALUE_IMM32,
            .dt = src.dt,
            .imm32 = 0
        };
        
        x64_emit_normal(out, src.dt.type, CMP, &src, &imm);
        return (X64_Value) { .type = X64_VALUE_FLAGS, .cond = X64_NE };
    }else abort();
}

static X64_Value x64_eval_immediate(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, const TB_Int128* imm) {
    TB_DataType dt = f->nodes[r].dt;
    
    // x64 can only handle 32bit immediates within
    // normal instructions, if you want bigger an
    // explicit MOV is required and 128bit immediates
    // require 2 registers.
    if (imm->hi) {
        // register pair
        assert(dt.type == TB_I128);
        
        X64_Value pair = x64_allocate_gpr_pair(ctx, r, dt);
        x64_inst_mov_ri64(out, pair.gpr_pair.lo, imm->lo);
        x64_inst_mov_ri64(out, pair.gpr_pair.hi, imm->hi);
        return pair;
    } else if (imm->lo > UINT32_MAX) {
        // explicit mov
        assert(dt.type == TB_I64 || dt.type == TB_PTR || dt.type == TB_I128);
        
        X64_Value dst = x64_allocate_gpr(ctx, r, dt);
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

static X64_Value x64_eval_float_immediate(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register r, float imm) {
    TB_DataType dt = f->nodes[r].dt;
    X64_Value dst = x64_allocate_xmm(ctx, r, dt);
    
    // Load from RIP
    // TODO(NeGate): Implement a better float immediate system
    // zeroes can be dealt with XORPS
    uint8_t* out_buffer = tb_out_reserve(out, 8);
    *out_buffer++ = 0xF3;
    *out_buffer++ = 0x0F;
    *out_buffer++ = 0x10;
    *out_buffer++ = ((dst.gpr & 7) << 3) | X64_RBP;
    
    *((uint32_t*)out_buffer) = 0;
    tb_out_commit(out, 8);
    
    X64_F32Patch* patch = &ctx->f32_patches[ctx->f32_patch_count++];
    patch->src = r;
    patch->base = out->count - 4;
    patch->value = imm;
    
    return dst;
}

static X64_Value x64_allocate_gpr(X64_Context* ctx, TB_Register reg, TB_DataType dt) {
	for (unsigned int i = 0; i < 14; i++) {
		X64_GPR gpr = GPR_PRIORITY_LIST[i];
        
		if (ctx->gpr_desc[gpr].bound_value == 0) {
			ctx->gpr_desc[gpr].bound_value = reg;
            
			return (X64_Value) {
				.type = X64_VALUE_GPR,
                .dt = dt,
                .gpr = gpr
			};
		}
	}
    
	// Spill GPRs
	abort();
}

static X64_Value x64_allocate_gpr_pair(X64_Context* ctx, TB_Register reg, TB_DataType dt) {
	X64_Value lo = x64_allocate_gpr(ctx, reg, TB_TYPE_I64(1));
	X64_Value hi = x64_allocate_gpr(ctx, reg, TB_TYPE_I64(1));
    
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
	abort();
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

// NOTE(NeGate): Both arguments cannot be memory operands
void x64_inst_op(TB_Emitter* out, int dt_type, const X64_NormalInst* inst, const X64_Value* a, const X64_Value* b) {
	// x64 can only have up to 16bytes in one instruction
    uint8_t* out_buffer_start = tb_out_reserve(out, 16);
    uint8_t* out_buffer = out_buffer_start;
    
    // the destination can only be a GPR, no direction flag
    bool is_gpr_only_dst = (inst->op & 1);
    
    bool dir = b->type == X64_VALUE_MEM;
	if (dir || inst->op == 0xAF) tb_swap(a, b);
    
    bool dir_flag = (dir != is_gpr_only_dst);
    
    // operand size
    uint8_t sz = (dt_type != TB_I8);
    
    // All instructions that go through here are
    // based on the ModRxRm encoding so we do need
    // an RX and an RM (base, index, shift, disp)
    uint8_t base = 0;
    uint8_t rx = GPR_NONE;
    if (inst->ext == X64_EXT_NONE || inst->ext == X64_EXT_DEF) {
        assert(dt_type == TB_I8 || dt_type == TB_I16 || dt_type == TB_I32 || dt_type == TB_I64 || dt_type == TB_PTR);
        
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
    else if (inst->ext == X64_EXT_SSE) {
        assert(b->type != X64_VALUE_IMM32);
        assert(dt_type == TB_F32 || dt_type == TB_F64);
        
        rx = b->xmm;
        base = a->xmm;
        
        if (rx >= 8 || base >= 8) {
            *out_buffer++ = x64_inst_rex(true, rx, base, 0);
        }
        
        *out_buffer++ = 0xF3;
        *out_buffer++ = 0x0F;
        *out_buffer++ = inst->op | (dir_flag ? 2 : 0);
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
			tb_out1b_UNSAFE(out, 0x90);
			out_buffer += 1;
			count -= 1;
		}
	} while (count);
    
    tb_out_commit(out, count);
}

// Going to be used in the instruction selector, promote 8bit and 16bit to 32bit or 64bit.
// Immediates can go either way but with GPRs and memory prefer 32bit if possible.
static X64_Value x64_legalize(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_DataType dt, TB_Register reg, TB_Register next) {
    // TODO(NeGate): Vectors
    assert(dt.count == 1);
    X64_Value v = x64_eval(f, ctx, out, reg, next);
    
	// This is kinda weird but essentially a load might 
	// return the address because this is x64 and we don't
	// need to load some in a separate instruction.
	if (dt.type == TB_PTR && f->nodes[reg].type == TB_LOAD) {
		v.dt = dt;
	}
    
    // This only needs to worry about 8 and 16bit GPRs
    if (v.dt.type == TB_I8 || v.dt.type == TB_I16) {
        // Types should have been promoted out
        abort();
    }
    
    return v;
}

static char x64_value_type_to_pattern_char(X64_ValueType type) {
	switch (type) {
        case X64_VALUE_IMM32: return 'i';
        case X64_VALUE_GPR: return 'r';
        case X64_VALUE_XMM: return 'x';
        case X64_VALUE_MEM: return 'm';
        default: abort();
	}
}

// Not built for vector selection
// Maybe it will be one day?
// if `next_reg` is not 0, then it's the register which we expect the `dst_reg` to go into
static X64_Value x64_std_isel(TB_Function* f, X64_Context* ctx, TB_Emitter* out, TB_Register dst_reg, TB_Register a_reg, TB_Register b_reg, TB_Register next_reg, const X64_ISel_Pattern patterns[], size_t pattern_count) {
    TB_DataType dst_dt = f->nodes[dst_reg].dt;
	assert(dst_dt.count == 1);
    
    X64_Value b;
	X64_Value a = x64_legalize(f, ctx, out, dst_dt, a_reg, dst_reg);
    
    if (a_reg == b_reg) b = a;
    else b = x64_legalize(f, ctx, out, dst_dt, b_reg, dst_reg);
    
	bool can_recycle = (ctx->intervals[a_reg] == dst_reg);
	if (f->nodes[next_reg].type == TB_RET &&
		a.type == X64_VALUE_GPR &&
		b.type == X64_VALUE_GPR &&
		(a.gpr != X64_RAX ||
         b.gpr != X64_RAX)) {
		// If it's about to be returned and none of 
        // the inputs are RAX, don't recycle
		can_recycle = false;
	}
    
	if (a.type == X64_VALUE_IMM32 && b.type != X64_VALUE_IMM32) tb_swap(a, b);
    
	// If both source operands are memory addresses, change one into a register
	if (a.type == X64_VALUE_MEM && b.type == X64_VALUE_MEM) {
		X64_Value temp = x64_allocate_gpr(ctx, a_reg, a.dt);
		x64_emit_normal(out, dst_dt.type, MOV, &temp, &a);
		a = temp;
	}
    
	bool will_try_recycle = can_recycle && a.type != X64_VALUE_IMM32;
    
	// Identify the pattern of the input
	char pattern_str[4] = {};
	pattern_str[0] = (dst_dt.type == TB_F32 || dst_dt.type == TB_F64) ? 'x' : 'r';
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
	else if (f->nodes[next_reg].type == TB_RET) {
        // It doesn't matter if something was using RAX before
        // since we're about to exit
        dst = (X64_Value){ .type = X64_VALUE_GPR, .dt = dst_dt, .gpr = X64_RAX };
    } else dst = x64_allocate_gpr(ctx, dst_reg, dst_dt);
    
	const X64_Value operands[3] = { dst, a, b };
	x64_micro_assemble(
                       out,
                       best_match->forced_64bit ? TB_I64 : dst_dt.type,
                       best_match->fmt, operands
                       );
	return dst;
}

static const uint8_t* x64_micro_assemble_operand(const uint8_t* format, X64_Value* dst, const X64_Value* operands) {
    if (*format == '[') {
        format++;
        
        // Memory operands
        assert(operands[format[0]].type == X64_VALUE_GPR);
        assert(operands[format[1]].type == X64_VALUE_GPR);
        X64_GPR base = operands[format[0]].gpr;
        X64_GPR index = operands[format[1]].gpr;
        format += 2;
        
        *dst = (X64_Value){
            .type = X64_VALUE_MEM,
            .dt = TB_TYPE_I64(1),
            .mem = {
                .base = base,
                .index = index,
                .scale = X64_SCALE_X1,
                .disp = 0
            }
        };
    } else {
        *dst = operands[format[0]];
        format++;
    }
    
    return format;
}

static void x64_micro_assemble(TB_Emitter* out, int dt_type, const uint8_t* format, const X64_Value* operands) {
    while (*format != 0x7F) {
		X64_InstType inst = *format++;
        
        X64_Value left, right;
        format = x64_micro_assemble_operand(format, &left, operands);
        format = x64_micro_assemble_operand(format, &right, operands);
        
		x64_inst_op(out, dt_type, &insts[inst], &left, &right);
    }
}

static int32_t x64_allocate_locals(TB_Function* f, X64_Context* ctx, TB_Emitter* out) {
    int32_t stack_usage = 0;
    
    loop(i, f->count) {
        if (f->nodes[i].type == TB_LOCAL) {
            uint32_t size = f->nodes[i].local.size;
            uint32_t align = f->nodes[i].local.alignment;
            
            // Increment and align
            stack_usage += size;
            stack_usage += (align - (stack_usage % align)) % align;
            
            //printf("Stack alloc: %u bytes (%u align)\n", size, align);
            
            ctx->locals[ctx->locals_count++] = (X64_LocalDesc) {
                .address = i,
                .disp = -stack_usage
            };
        } else if (f->nodes[i].type == TB_PARAM_ADDR) {
            TB_Register param = f->nodes[i].param_addr.param;
            TB_DataType dt = f->nodes[param].dt;
            int id = f->nodes[param].param.id;
            
            if (id < 4) {
                if (TB_IS_INTEGER_TYPE(dt.type) || dt.type == TB_PTR) {
                    X64_Value dst = (X64_Value) {
                        .type = X64_VALUE_MEM,
                        .dt = TB_TYPE_I64(1),
                        .mem = {
                            .base = X64_RSP,
                            .index = X64_GPR_NONE,
                            .scale = X64_SCALE_X1,
                            .disp = (1 + id) * 8
                        }
                    };
                    
                    X64_Value src = (X64_Value) {
                        .type = X64_VALUE_GPR,
                        .dt = TB_TYPE_I64(1),
                        .gpr = GPR_PARAMETERS[id]
                    };
                    
                    // save the shadow space into the stack
                    x64_emit_normal(out, dt.type, MOV, &dst, &src);
                } else abort();
            }
        }
    }
    
    return stack_usage;
}

static void x64_create_phi_lookup(TB_Function* f, X64_Context* ctx) {
    // Generate PHI lookup table
    loop(i, f->count) {
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
}

static int32_t x64_find_local(X64_Context* ctx, TB_Register r) {
    loop(i, ctx->locals_count) {
		if (ctx->locals[i].address == r) return ctx->locals[i].disp;
	}
    
	abort();
}

static X64_PhiValue* x64_find_phi(X64_Context* ctx, TB_Register r) {
    loop(i, ctx->phi_count) {
		if (ctx->phis[i].reg == r) return &ctx->phis[i];
	}
    
	return NULL;
}

// Searches by the values the PHI node could have
static X64_PhiValue* x64_find_phi_values(X64_Context* ctx, TB_Register r) {
    loop(i, ctx->phi_count) {
		if (ctx->phis[i].storage_a == r || ctx->phis[i].storage_b == r) return &ctx->phis[i];
	}
    
	return NULL;
}
