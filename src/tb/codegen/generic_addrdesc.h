// this is a generic code generator you can use to get started with a new register machine target in TB
// all you have to do is hook in the correct details and functions and it'll do the heavy lifting...
// it's all managed through preprocessor defines and monomorphized using #include like such:
//
//
// #define GAD_EXPORT(name) aarch64_ ## name // all "exported" symbols have this prefix
// #define GAD_NUM_REG_FAMILIES 2
// #define GAD_REGS_IN_FAMILY 16
// #define GAD_VAL MyCpuVal
// #include "generic_addrdesc.h"
//
static_assert(sizeof(float) == sizeof(uint32_t), "lil bitch... float gotta be 32bit");
static_assert(sizeof(double) == sizeof(uint64_t), "big bitch... double gotta be 64bit");

static thread_local size_t s_local_thread_id;

#define EITHER2(a, b, c)    ((a) == (b) || (a) == (c))
#define EITHER3(a, b, c, d) ((a) == (b) || (a) == (c) || (a) == (d))
#define FITS_INTO(a, type)  ((a) == ((type)(a)))

#define GET_CODE_POS() (ctx->out - ctx->start_out)

// We really only need the position where to patch
// it since it's all internal and the target is implicit.
typedef uint32_t ReturnPatch;

typedef struct LabelPatch {
    int pos;
    TB_Label target_lbl;
} LabelPatch;

typedef struct {
    int unused;
} AddressDesc;

typedef struct {
    // Header that
    uint8_t* out;
    uint8_t* start_out;

    TB_Function* f;

    // some analysis
    TB_Reg* use_count;
    int* ordinal;

    // Used to allocate stack stuff
    uint32_t stack_usage;

    // just keeps track of the callee saved registers that
    // we actually used.
    uint64_t regs_to_save;

    // Patch info
    uint32_t label_patch_count;
    uint32_t ret_patch_count;

    uint32_t* labels;
    LabelPatch* label_patches;
    ReturnPatch* ret_patches;

    // Register allocation
    TB_Reg reg_allocator[GAD_NUM_REG_FAMILIES][GAD_REGS_IN_FAMILY];
    int regs_available[GAD_NUM_REG_FAMILIES];

    AddressDesc addresses[];
} Ctx;

typedef struct {
    size_t memory_usage;

    size_t locals_count;
    size_t return_count;
    size_t line_info_count;
    size_t label_patch_count;
} FunctionTallySimple;

static FunctionTallySimple tally_memory_usage_simple(TB_Function* restrict f) {
    size_t locals_count = 0;
    size_t return_count = 0;
    size_t label_patch_count = 0;
    size_t line_info_count = 0;

    TB_FOR_EACH_NODE(n, f) {
        TB_NodeTypeEnum t = n->type;

        if (t == TB_RET) return_count++;
        else if (t == TB_LOCAL) locals_count++;
        else if (t == TB_IF) label_patch_count += 2;
        else if (t == TB_GOTO) label_patch_count++;
        else if (t == TB_LINE_INFO) line_info_count++;
        else if (t == TB_SWITCH) {
            label_patch_count += 1 + ((n->switch_.entries_end - n->switch_.entries_start) / 2);
        }
    }

    // parameters are locals too... ish
    locals_count += f->prototype->param_count;

    size_t align_mask = _Alignof(long double) - 1;
    size_t tally      = 0;

    // context
    tally += sizeof(Ctx) + (f->node_count * sizeof(AddressDesc));
    tally = (tally + align_mask) & ~align_mask;

    // ordinal
    tally += f->node_count * sizeof(int);
    tally = (tally + align_mask) & ~align_mask;

    // use_count
    tally += f->node_count * sizeof(TB_Reg);
    tally = (tally + align_mask) & ~align_mask;

    // intervals
    tally += f->node_count * sizeof(TB_Reg);
    tally = (tally + align_mask) & ~align_mask;

    // labels
    tally += f->label_count * sizeof(uint32_t);
    tally = (tally + align_mask) & ~align_mask;

    // label_patches
    tally += label_patch_count * sizeof(LabelPatch);
    tally = (tally + align_mask) & ~align_mask;

    // ret_patches
    tally += return_count * sizeof(ReturnPatch);
    tally = (tally + align_mask) & ~align_mask;

    return (FunctionTallySimple) {
        .memory_usage = tally,
        .line_info_count = line_info_count,
        .locals_count = locals_count,
        .return_count = return_count,
        .label_patch_count = label_patch_count
    };
}

static TB_FunctionOutput GAD_EXPORT(compile_function)(TB_FunctionID id, TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t local_thread_id) {
    s_local_thread_id = local_thread_id;
    TB_TemporaryStorage* tls = tb_tls_allocate();

    //bool is_ctx_heap_allocated = false;
    Ctx* restrict ctx = NULL;
    {
        size_t ctx_size = sizeof(Ctx) + (f->node_count * sizeof(AddressDesc));
        FunctionTallySimple tally = tally_memory_usage_simple(f);

        ctx = tb_tls_push(tls, ctx_size);
        *ctx = (Ctx){
            .out           = out,
            .start_out     = out,
            .labels        = tb_tls_push(tls, f->label_count * sizeof(uint32_t)),
            .label_patches = tb_tls_push(tls, tally.label_patch_count * sizeof(LabelPatch)),
            .ret_patches   = tb_tls_push(tls, tally.return_count * sizeof(ReturnPatch)),
            .ordinal       = tb_tls_push(tls, f->node_count * sizeof(int)),
            .use_count     = tb_tls_push(tls, f->node_count * sizeof(TB_Reg)),
        };

        f->line_count = 0;
        f->lines = tb_platform_arena_alloc(tally.line_info_count * sizeof(TB_Line));

        memset(ctx->addresses, 0, f->node_count * sizeof(AddressDesc));
    }

    // Analyze function for stack, use counts and phi nodes
    tb_function_calculate_use_count(f, ctx->use_count);

    // calculate the order of the nodes, it helps since node indices
    // don't actually tell us this especially once the optimizer has
    // taken a jab at it.
    int counter = 0;
    TB_FOR_EACH_NODE(n, f) {
        ctx->ordinal[n - f->nodes] = counter++;
    }

    // calculate the maximum parameter usage for a call
    size_t caller_usage = 0;
    TB_FOR_EACH_NODE(n, f) {
        if (EITHER3(n->type, TB_CALL, TB_ECALL, TB_VCALL)) {
            int param_usage = CALL_NODE_PARAM_COUNT(n);
            if (caller_usage < param_usage) caller_usage = param_usage;
        }
    }

    // Evaluate basic blocks
    TB_Reg bb = 1;
    do {
        assert(f->nodes[bb].type == TB_LABEL);
        TB_Node* start = &f->nodes[bb];

        TB_Reg bb_end = start->label.terminator;
        TB_Node* end = &f->nodes[bb_end];

        // Define label position
        TB_Label label_id = start->label.id;
        ctx->labels[label_id] = GET_CODE_POS();

        // Generate instructions per BB

        // Evaluate the terminator
        TB_Node* next_bb = end;

        if (end->type != TB_LABEL) next_bb = &f->nodes[next_bb->next];
        TB_Reg next_bb_reg = next_bb - f->nodes;

        if (end->type == TB_RET) {
            tb_todo();
        } else tb_todo();

        // Next Basic block
        bb = next_bb_reg;
    } while (bb != TB_NULL_REG);

    // TODO(NeGate): resolve function-level patches (returns and labels)

    // hack to make the first line in a function think it's at
    // the top of the prologue not within the body
    if (f->line_count > 0) {
        f->lines[0].pos = 0;
    }

    // we're done, clean up
    TB_FunctionOutput func_out = {
        .linkage = f->linkage,
        .code = ctx->start_out,
        .code_size = ctx->out - ctx->start_out,
        .stack_usage = ctx->stack_usage,
        .prologue_epilogue_metadata = ctx->regs_to_save
    };

    return func_out;
}
