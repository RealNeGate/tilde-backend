#include "../tb_internal.h"

#ifdef _WIN32
#define LJ_EXPORT __attribute__((dllexport))
#else
#define LJ_EXPORT __attribute__((visibility("default")))
#endif

typedef struct {
    TB_Function* func;
    TB_Reg reg;
} TB_NodeRef;

LJ_EXPORT TB_NodeRef tb__first_node(TB_Function* f) {
    return (TB_NodeRef){ f, 1 };
}

LJ_EXPORT TB_NodeRef tb__next_node(TB_NodeRef r) {
    return (TB_NodeRef){ r.func, r.func->nodes[r.reg].next };
}

LJ_EXPORT TB_Node* tb__nodes(TB_Function* f) {
    return f->nodes;
}

LJ_EXPORT void tb__print_func(TB_Function* f) {
    tb_function_print(f, tb_default_print_callback, stdout);
}
