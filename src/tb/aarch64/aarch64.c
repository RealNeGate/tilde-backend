#include "../tb_internal.h"

#define GAD_EXPORT(name) aarch64_ ## name // all exported symbols have this prefix
#define GAD_NUM_REG_FAMILIES 2
#define GAD_REGS_IN_FAMILY 32
#define GAD_INITIAL_REG_ALLOC(ctx)
#define GAD_VAL Val
#include "../codegen/generic_addrdesc.h"

static void aarch64_initial_reg_alloc(Ctx* restrict ctx) {
    ctx->regs_available[0] = 30; // take out the stack pointer
    ctx->regs_available[1] = 32;
}

#if _MSC_VER
_Pragma("warning (push)") _Pragma("warning (disable: 4028)")
#endif
ICodeGen tb__aarch64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,

    .get_data_type_size  = aarch64_get_data_type_size,
    .emit_call_patches   = aarch64_emit_call_patches,
    //.get_prologue_length = aarch64_get_prologue_length,
    //.get_epilogue_length = aarch64_get_epilogue_length,
    //.emit_prologue       = aarch64_emit_prologue,
    //.emit_epilogue       = aarch64_emit_epilogue,

    .fast_path    = aarch64_compile_function,
    //.complex_path = x64_complex_compile_function
};
#if _MSC_VER
_Pragma("warning (pop)")
#endif
