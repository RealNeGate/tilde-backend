#include "tb_internal.h"
#include "coroutine.h"

TB_API bool tb_validator_next(TB_Function* f, TB_Validator* v) {
    CO_SCOPE(v) {
        CO_START();

        for (; v->l < f->bb_count; v->l++) {
            v->end = f->bbs[v->l].end;

            v->r = f->bbs[v->l].start;
            while (v->r != 0 && v->r != v->end) {
                if (TB_IS_NODE_TERMINATOR(f->nodes[v->r].type)) {
                    v->type = TB_VALIDATION_OUT_OF_PLACE_TERMINATOR;
                    v->out_of_place_terminator = v->r;

                    CO_YIELD(v, true);
                }

                TB_Reg next = f->nodes[v->r].next;
                if (next == 0) {
                    v->type = TB_VALIDATION_NON_TERMINATOR_HAS_NO_NEXT;
                    v->non_terminator_no_next = v->r;

                    CO_YIELD(v, true);
                }

                v->r = f->nodes[v->r].next;
            }

            if (!TB_IS_NODE_TERMINATOR(f->nodes[v->end].type)) {
                // terminator shouldn't have a valid next
                if (f->nodes[v->end].next != 0) {
                    v->type = TB_VALIDATION_TERMINATOR_HAS_NEXT;
                    v->terminator_has_next = v->end;
                    CO_YIELD(v, true);
                }

                v->type = TB_VALIDATION_NO_TERMINATOR;
                v->no_terminator_label = v->l;
                CO_YIELD(v, true);
            }
        }
    }

    CO_DONE();
}

TB_API int tb_function_validate(TB_Function* f) {
    int error_count = 0;

    TB_Validator v = { 0 };
    while (tb_validator_next(f, &v)) {
        if (v.type != TB_VALIDATION_SUCCESS) {
            if (error_count == 0) {
                // only print the first time
                tb_function_print(f, tb_default_print_callback, stderr, true);
            }
            error_count += 1;
        }

        switch (v.type) {
            case TB_VALIDATION_SUCCESS: break;
            case TB_VALIDATION_NO_TERMINATOR: {
                fprintf(stderr, "%s:L%d: no terminator at the end of basic block.\n", f->name, v.no_terminator_label);
                break;
            }
            case TB_VALIDATION_TERMINATOR_HAS_NEXT: {
                fprintf(stderr, "%s:r%d: terminator should not have a valid next.\n", f->name, v.terminator_has_next);
                break;
            }
            case TB_VALIDATION_NON_TERMINATOR_HAS_NO_NEXT: {
                fprintf(stderr, "%s:r%d: non-terminator should have a valid next.\n", f->name, v.non_terminator_no_next);
                break;
            }
            case TB_VALIDATION_OUT_OF_PLACE_TERMINATOR: {
                fprintf(stderr, "%s:r%d: cannot place a terminator in the middle of a basic block.\n", f->name, v.out_of_place_terminator);
                break;
            }
        }
    }

    return error_count;
}
