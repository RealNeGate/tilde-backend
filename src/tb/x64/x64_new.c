#include "../tb_internal.h"
#include "x64.h"

#if 1
#define LISTING(...) printf(__VA_ARGS__)
#else
#define LISTING(fmt, ...)
#endif

typedef struct Ctx Ctx;

static void ret_jmp(Ctx* restrict ctx);

static void x64v2_initial_reg_alloc(Ctx* restrict ctx);
static Val x64v2_resolve_value(Ctx* restrict ctx, TB_Function* f, TB_Reg r);
static void x64v2_resolve_params(Ctx* restrict ctx, TB_Function* f);
static void x64v2_resolve_stack_slot(Ctx* restrict ctx, TB_Function* f, TB_Node* restrict n);

#define GAD_REG_PRIORITIES { \
    { RAX, RCX, RDX, R8, R9, R10, R11, RDI, RSI, RBX, R12, R13, R14, R15 }, \
    { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15 } \
}

#if 1
#define GAD_EXTRA_CTX {}
#else
#define GAD_EXTRA_CTX {   \
    struct X64_Tile {     \
        TB_Reg  mapping;  \
        GPR     base  : 8;\
        GPR     index : 8;\
        Scale   scale : 8;\
        int32_t disp;     \
    } tile;               \
}
#endif

#define GAD_FN(name) x64v2_ ## name // all exported symbols have this prefix
#define GAD_NUM_REG_FAMILIES 2
#define GAD_REGS_IN_FAMILY 16
#define GAD_INITIAL_REG_ALLOC(ctx) x64v2_initial_reg_alloc(ctx)
#define GAD_RESOLVE_PARAMS(ctx, f) x64v2_resolve_params(ctx, f)
#define GAD_RESOLVE_VALUE(ctx, f, r) x64v2_resolve_value(ctx, f, r)
#define GAD_RESOLVE_STACK_SLOT(ctx, f, n) x64v2_resolve_stack_slot(ctx, f, n)
#define GAD_MAKE_STACK_SLOT(ctx, f, r, pos) (Val){ VAL_MEM, .r = r, .mem = { .base = RBP, .index = GPR_NONE, .disp = (pos) } }
#define GAD_RET_JMP(ctx) ret_jmp(ctx)
#define GAD_VAL Val
#include "../codegen/generic_addrdesc.h"
#include "x64_emitter.h"
#include "x64_proepi.h"

enum {
    X64_REG_CLASS_GPR,
    X64_REG_CLASS_XMM
};

// a valid type that the x64 backend can eat along with
typedef struct {
    TB_DataType dt;
    uint64_t mask;
} LegalInt;

// returns a mask to remove the "out of bounds" bits
static LegalInt legalize_int(TB_DataType dt) {
    if (dt.type != TB_INT) return (LegalInt){ dt, 0 };

    int bits;
    if (!TB_NEXT_BIGGEST(&bits, dt.data, 8, 16, 32, 64)) {
        // support bigger types
        tb_todo();
    }

    int original_bits = dt.data;
    uint64_t mask = ~UINT64_C(0) >> (64 - original_bits);

    // we don't need the mask if it lines up nicely with machine sizes
    if (original_bits == 8 || original_bits == 16 || original_bits == 32 || original_bits == 64) {
        mask = 0;
    }

    dt.data = bits;
    return (LegalInt){ dt, mask };
}

static uint8_t legalize_float(TB_DataType dt) {
    assert(dt.type == TB_FLOAT);

    uint8_t flags = 0;
    if (dt.data == TB_FLT_64) {
        assert(dt.width == 0 || dt.width == 1);
        flags |= INST2FP_DOUBLE;
    } else if (dt.data == TB_FLT_32) {
        assert(dt.width == 0 || dt.width == 2);
    } else {
        tb_unreachable();
    }

    flags |= (dt.width ? INST2FP_PACKED : 0);
    return flags;
}

static int get_data_type_size(const TB_DataType dt) {
    assert(dt.width <= 2 && "Vector width too big!");

    switch (dt.type) {
        case TB_INT: {
            // round up bits to a byte
            bool is_big_int = dt.data > 64;
            int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data);

            return ((bits+7) / 8) << dt.width;
        }
        case TB_FLOAT: {
            int s = 0;
            if (dt.data == TB_FLT_32) s = 4;
            else if (dt.data == TB_FLT_64) s = 8;
            else tb_unreachable();

            return s << dt.width;
        }
        case TB_PTR: {
            return 8;
        }
        default: {
            tb_unreachable();
            return 0;
        }
    }
}

static void x64v2_resolve_params(Ctx* restrict ctx, TB_Function* f) {
    bool is_sysv = (f->module->target_abi == TB_ABI_SYSTEMV);
    const TB_FunctionPrototype* restrict proto = f->prototype;

    FOREACH_N(i, 0, proto->param_count) {
        TB_DataType dt = proto->params[i];
        TB_Reg r = TB_FIRST_PARAMETER_REG + i;

        // Allocate space in stack
        assert(get_data_type_size(dt) <= 8 && "Parameter too big");

        if (dt.width || TB_IS_FLOAT_TYPE(dt)) {
            // xmm parameters
            if (i < 4) {
                GAD_FN(reserve_register)(ctx, f, r, X64_REG_CLASS_XMM, i);
            } else {
                GAD_FN(force_stack)(ctx, f, r, 16 + (i * 8));
            }
        } else {
            // gpr parameters
            if (is_sysv && i < 6) {
                GAD_FN(reserve_register)(ctx, f, r, X64_REG_CLASS_GPR, SYSV_GPR_PARAMETERS[i]);
            } else if (i < 4) {
                GAD_FN(reserve_register)(ctx, f, r, X64_REG_CLASS_GPR, WIN64_GPR_PARAMETERS[i]);
            } else {
                GAD_FN(force_stack)(ctx, f, r, 16 + (i * 8));
            }
        }
    }

    if (proto->param_count) {
        ctx->stack_usage += 16 + (proto->param_count * 8);
    }

    if (proto->has_varargs) {
        const GPR* parameter_gprs = is_sysv ? SYSV_GPR_PARAMETERS : WIN64_GPR_PARAMETERS;

        // spill the rest of the parameters (assumes they're all in the GPRs)
        size_t gpr_count = is_sysv ? 6 : 4;
        size_t extra_param_count = proto->param_count > gpr_count ? 0 : gpr_count - proto->param_count;

        FOREACH_N(i, 0, extra_param_count) {
            size_t param_num = proto->param_count + i;

            Val dst = val_stack(TB_TYPE_I64, 16 + (param_num * 8));
            Val src = val_gpr(TB_TYPE_I64, parameter_gprs[param_num]);
            INST2(MOV, &dst, &src, TB_TYPE_I64);
        }
    }
}

static void x64v2_resolve_stack_slot(Ctx* restrict ctx, TB_Function* f, TB_Node* restrict n) {
    bool is_sysv = (f->module->target_abi == TB_ABI_SYSTEMV);

    if (n->type == TB_PARAM_ADDR) {
        int id = n->param_addr.param - TB_FIRST_PARAMETER_REG;
        TB_DataType dt = n->dt;

        GAD_VAL dst = GAD_FN(force_stack)(ctx, f, n - f->nodes, 16 + (id * 8));

        if (dt.width || TB_IS_FLOAT_TYPE(dt)) {
            tb_todo();
        } else {
            // don't keep a reference of it in GPR if it's in memory
            if (is_sysv && id < 6) {
                GAD_VAL src = GAD_FN(steal_register)(
                    ctx, f, TB_TYPE_I64, X64_REG_CLASS_GPR, WIN64_GPR_PARAMETERS[id]
                );
                INST2(MOV, &dst, &src, TB_TYPE_I64);
            } else if (id < 4) {
                GAD_VAL src = GAD_FN(steal_register)(
                    ctx, f, TB_TYPE_I64, X64_REG_CLASS_GPR, WIN64_GPR_PARAMETERS[id]
                );
                INST2(MOV, &dst, &src, TB_TYPE_I64);
            }
        }
    } else if (n->type == TB_LOCAL) {
        GAD_FN(alloc_stack)(ctx, f, n - f->nodes, n->local.size, n->local.alignment);
    }
}

static void x64v2_initial_reg_alloc(Ctx* restrict ctx) {
    ctx->regs_available[0] = 14; // take out the stack pointer
    ctx->regs_available[1] = 16;
}

static Val x64v2_get_val_gpr(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    ptrdiff_t i = GAD_FN(await)(ctx, f, r, 0);
    assert(i >= 0);

    if (ctx->queue[i].type == VAL_MEM) {
        GAD_VAL addr = ctx->queue[i];
        GAD_VAL dst = GAD_FN(alloc_reg)(ctx, f, X64_REG_CLASS_GPR, TB_TEMP_REG);
        dst.dt = f->nodes[r].dt;

        INST2(addr.mem.is_rvalue ? MOV : LEA, &dst, &addr, f->nodes[r].dt);
        return dst;
    }

    assert(ctx->queue[i].type == VAL_GPR || ctx->queue[i].type == VAL_IMM);
    return ctx->queue[i];
}

static void x64v2_return(Ctx* restrict ctx, TB_Function* f, TB_Node* restrict n) {
    TB_DataType dt = n->dt;

    // Evaluate return value
    if (dt.type == TB_FLOAT) {
        tb_todo();
    } else if ((dt.type == TB_INT && dt.data > 0) || dt.type == TB_PTR) {
        GAD_VAL dst = val_gpr(dt, RAX);
        GAD_VAL src = GAD_FN(get_val_gpr)(ctx, f, n->ret.value);

        if (src.gpr != dst.gpr) {
            INST2(MOV, &dst, &src, dt);
        }
    } else tb_todo();
}

static void x64v2_cond_to_reg(Ctx* restrict ctx, TB_Function* f, TB_Reg r, int cc) {
    GAD_VAL dst = GAD_FN(alloc_reg)(ctx, f, X64_REG_CLASS_GPR, r);

    EMIT((dst.gpr >= 8) ? 0x41 : 0x40);
    EMIT(0x0F);
    EMIT(0x90 + cc);
    EMIT(mod_rx_rm(MOD_DIRECT, 0, dst.gpr));
}

static void x64v2_branch_if(Ctx* restrict ctx, TB_Function* f, TB_Reg cond, TB_Label fallthrough, TB_Label if_true, TB_Label if_false) {
    Cond cc = 0;
    if (ctx->flags_bound == cond) {
        cc = ctx->flags_code;
    } else {
        tb_todo();
    }

    // flip the condition and the labels if
    // it allows for fallthrough
    bool has_fallthrough = (fallthrough == if_false);
    if (fallthrough == if_true) {
        tb_swap(TB_Label, if_true, if_false);
        cc ^= 1;

        has_fallthrough = true;
    }

    // JCC .true
    // JMP .false # elidable if it points to the next instruction
    JCC(cc, if_true);
    if (!has_fallthrough) JMP(if_false);
}

static void x64v2_phi_move(Ctx* restrict ctx, TB_Function* f, GAD_VAL* dst, TB_Reg src) {
    GAD_VAL src_val = GAD_FN(get_val_gpr)(ctx, f, src);
    INST2(MOV, dst, &src_val, f->nodes[src].dt);
}

static void x64v2_store(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    TB_Node* restrict n = &f->nodes[r];

    GAD_VAL addr = GAD_FN(get_val)(ctx, f, n->store.address);
    if (addr.is_spill) {
        // restore from spill
        GAD_VAL tmp = GAD_FN(alloc_reg)(ctx, f, X64_REG_CLASS_GPR, TB_TEMP_REG);
        INST2(MOV, &tmp, &addr, TB_TYPE_PTR);

        addr = val_base_disp(TB_TYPE_PTR, tmp.gpr, 0);
    } else if (addr.type == VAL_GPR) {
        addr = val_base_disp(TB_TYPE_PTR, addr.gpr, 0);
    }

    LegalInt l = legalize_int(n->dt);

    GAD_VAL src = GAD_FN(get_val_gpr)(ctx, f, n->store.value);
    INST2(MOV, &addr, &src, l.dt);

    if (addr.is_spill) {
        tb_assert_once("Freeing temporary register in addr.mem.base");
    }
}

static bool fits_into_int32(TB_Node* n) {
    if (n->type == TB_INTEGER_CONST &&
        n->integer.num_words == 1) {
        uint64_t x = n->integer.single_word;
        int32_t y = x & 0xFFFFFFFF;

        return (int64_t)y == x;
    }

    return false;
}

static Val x64v2_resolve_value(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    TB_Node* restrict n = &f->nodes[r];
    TB_NodeTypeEnum type = n->type;

    switch (type) {
        case TB_INTEGER_CONST: {
            if (fits_into_int32(n)) {
                LegalInt l = legalize_int(n->dt);
                uint64_t imm = n->integer.single_word;
                if (l.mask) {
                    imm &= l.mask;
                }

                return val_imm(n->dt, imm);
            } else {
                tb_todo();
            }
        }

        case TB_LOAD: {
            // convert entry into memory slot
            GAD_VAL src = GAD_FN(get_val)(ctx, f, n->load.address);
            GAD_VAL dst = GAD_FN(alloc_reg)(ctx, f, X64_REG_CLASS_GPR, r);

            if (src.is_spill) {
                // restore from spill
                INST2(MOV, &dst, &src, TB_TYPE_PTR);

                src = val_base_disp(TB_TYPE_PTR, dst.gpr, 0);
            } else if (src.type == VAL_GPR) {
                src = val_base_disp(TB_TYPE_PTR, src.gpr, 0);
            }

            INST2(MOV, &dst, &src, f->nodes[r].dt);
            return dst;
        }

        case TB_MEMBER_ACCESS: {
            GAD_VAL base = GAD_FN(get_val_gpr)(ctx, f, n->member_access.base);
            GAD_VAL dst = GAD_FN(alloc_reg)(ctx, f, X64_REG_CLASS_GPR, r);

            GAD_VAL addr = val_base_disp(TB_TYPE_PTR, base.gpr, n->member_access.offset);
            INST2(LEA, &dst, &addr, TB_TYPE_PTR);
            return dst;
        }

        case TB_ARRAY_ACCESS: {
            uint32_t stride = n->array_access.stride;
            GAD_VAL index = GAD_FN(get_val_gpr)(ctx, f, n->array_access.index);

            // if it's an LEA index*stride
            // then stride > 0, if not it's free
            // do think of it however
            GPR index_reg = index.gpr;
            uint8_t stride_as_shift = 0;

            GAD_VAL dst = GAD_FN(alloc_reg)(ctx, f, X64_REG_CLASS_GPR, r);
            bool written_to_dst = false;

            if (tb_is_power_of_two(stride)) {
                stride_as_shift = tb_ffs(stride) - 1;

                if (stride_as_shift > 3) {
                    assert(stride_as_shift < 64 && "Stride to big!!!");

                    INST2(MOV, &dst, &index, TB_TYPE_PTR);
                    written_to_dst = true;

                    // shl index, stride_as_shift
                    EMIT(rex(true, 0, dst.gpr, 0));
                    EMIT(0xC1);
                    EMIT(mod_rx_rm(MOD_DIRECT, 0x04, dst.gpr));
                    EMIT(stride_as_shift);

                    stride_as_shift = 0; // pre-multiplied, don't propagate
                }
            } else {
                // imul dst, index, stride
                EMIT(rex(true, dst.gpr, index.gpr, 0));
                EMIT(0x69);
                EMIT(mod_rx_rm(MOD_DIRECT, dst.gpr, index.gpr));
                EMIT4(stride);

                written_to_dst = true;
                stride_as_shift = 0; // pre-multiplied, don't propagate
            }

            // sanity check some post conditions ;)
            assert(index_reg != GPR_NONE);
            assert(stride_as_shift >= 0 && stride_as_shift <= 3
                && "stride_as_shift can't fit into an LEA");

            GAD_VAL base = GAD_FN(get_val_gpr)(ctx, f, n->array_access.base);
            assert(base.type == VAL_GPR);

            if (stride_as_shift) {
                GAD_VAL addr = val_base_index(TB_TYPE_PTR, base.gpr, written_to_dst ? dst.gpr : index.gpr, stride_as_shift);

                // INST2(LEA, &dst, &addr, TB_TYPE_PTR);
                return addr;
            } else {
                if (written_to_dst) {
                    INST2(ADD, &dst, &base, TB_TYPE_PTR);
                    return dst;
                } else {
                    tb_assert_once("does this path get hit?");

                    return val_base_index(TB_TYPE_PTR, base.gpr, index.gpr, SCALE_X1);
                    // INST2(LEA, &dst, &addr, TB_TYPE_PTR);
                }
            }
        }

        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        case TB_CMP_FLT:
        case TB_CMP_FLE: {
            TB_DataType cmp_dt = n->cmp.dt;
            assert(cmp_dt.width == 0 && "TODO: Implement vector compares");

            Cond cc = -1;
            if (TB_IS_FLOAT_TYPE(cmp_dt)) {
                tb_todo();
            } else {
                cmp_dt = legalize_int(cmp_dt).dt;

                bool invert = false;
                TB_Reg lhs = n->cmp.a, rhs = n->cmp.b;
                if (f->nodes[n->cmp.a].type == TB_INTEGER_CONST) {
                    tb_swap(TB_Reg, lhs, rhs);
                    invert = true;
                }

                GAD_VAL lhs_val = GAD_FN(get_val_gpr)(ctx, f, lhs);
                GAD_VAL rhs_val = GAD_FN(get_val_gpr)(ctx, f, rhs);
                INST2(CMP, &lhs_val, &rhs_val, cmp_dt);

                switch (type) {
                    case TB_CMP_EQ: cc = E; break;
                    case TB_CMP_NE: cc = NE; break;
                    case TB_CMP_SLT: cc = invert ? G : L; break;
                    case TB_CMP_SLE: cc = invert ? GE : LE; break;
                    case TB_CMP_ULT: cc = invert ? A : B; break;
                    case TB_CMP_ULE: cc = invert ? NB : BE; break;
                    default: tb_unreachable();
                }
            }
            assert(cc != -1);

            GAD_FN(set_flags)(ctx, f, r, cc);
            return val_flags(cc);
        }

        default: tb_todo();
    }

    return (Val){ 0 };
}

#if _MSC_VER
_Pragma("warning (push)") _Pragma("warning (disable: 4028)")
#endif
ICodeGen tb__x64v2_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,

    .get_data_type_size  = x64v2_get_data_type_size,
    .emit_call_patches   = x64v2_emit_call_patches,
    .get_prologue_length = x64_get_prologue_length,
    .get_epilogue_length = x64_get_epilogue_length,
    .emit_prologue       = x64_emit_prologue,
    .emit_epilogue       = x64_emit_epilogue,

    .fast_path    = x64v2_compile_function,
    //.complex_path = x64_complex_compile_function
};
#if _MSC_VER
_Pragma("warning (pop)")
#endif
