#include "tb_internal.h"

#define NEW(...) memcpy(make_type(m), &(TB_DebugType){ __VA_ARGS__ }, sizeof(TB_DebugType))

static TB_DebugType* make_type(TB_Module* m) {
    int tid = tb__get_local_tid();
    return pool_put(m->thread_info[tid].debug_types);
}

TB_API const TB_DebugType* tb_debug_get_integer(TB_Module* m, bool is_signed, int bits) {
    static const TB_DebugType types[] = {
        { .tag = TB_DEBUG_TYPE_UINT, .int_bits = 1 },
        { .tag = TB_DEBUG_TYPE_UINT, .int_bits = 8 },
        { .tag = TB_DEBUG_TYPE_UINT, .int_bits = 16 },
        { .tag = TB_DEBUG_TYPE_UINT, .int_bits = 32 },
        { .tag = TB_DEBUG_TYPE_UINT, .int_bits = 64 },

        { .tag = TB_DEBUG_TYPE_INT, .int_bits = 1 },
        { .tag = TB_DEBUG_TYPE_INT, .int_bits = 8 },
        { .tag = TB_DEBUG_TYPE_INT, .int_bits = 16 },
        { .tag = TB_DEBUG_TYPE_INT, .int_bits = 32 },
        { .tag = TB_DEBUG_TYPE_INT, .int_bits = 64 },
    };

    int b = (is_signed ? 5 : 0);
    if (bits <= 1)  return &types[b + 0];
    if (bits <= 8)  return &types[b + 1];
    if (bits <= 16) return &types[b + 2];
    if (bits <= 32) return &types[b + 3];
    if (bits <= 64) return &types[b + 4];
    tb_todo();
}

TB_API const TB_DebugType* tb_debug_get_float(TB_Module* m, TB_FloatFormat fmt) {
    static const TB_DebugType types[] = {
        [TB_FLT_32] = { TB_DEBUG_TYPE_FLOAT, .float_fmt = TB_FLT_32 },
        [TB_FLT_64] = { TB_DEBUG_TYPE_FLOAT, .float_fmt = TB_FLT_64 },
    };

    return &types[fmt];
}

TB_API const TB_DebugType* tb_debug_create_ptr(TB_Module* m, const TB_DebugType* base) {
    return NEW(TB_DEBUG_TYPE_POINTER, .ptr_to = base);
}

TB_API const TB_DebugType* tb_debug_create_array(TB_Module* m, const TB_DebugType* base, size_t count) {
    return NEW(TB_DEBUG_TYPE_ARRAY, .array = { base, count });
}
