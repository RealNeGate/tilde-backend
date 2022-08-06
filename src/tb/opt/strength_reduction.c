#include "../tb_internal.h"

bool tb_opt_strength_reduction(TB_Function* f) {
    int changes = 0;

    TB_FOR_EACH_NODE(n, f) {
        TB_Reg i = n - f->nodes;

        if (n->type == TB_MUL) {
            TB_Node* a = &f->nodes[n->i_arith.a];
            TB_Node* b = &f->nodes[n->i_arith.b];
            TB_DataType dt = n->dt;

            tb_assume(dt.type == TB_INT && dt.data > 0);
            if (dt.data <= 64) {
                OPTIMIZER_LOG(i, "TODO multiply by power-of-two folding doesn't work on 64bit+ integers");
                continue;
            }

            if (b->type == TB_INTEGER_CONST && b->integer.num_words == 1) {
                uint64_t b_const = b->integer.single_word;

                uint64_t log2 = tb_ffs(b_const) - 1;
                if (b_const == (UINT64_C(1) << log2)) {
                    OPTIMIZER_LOG(i, "converted power-of-two multiply into left shift");

                    // It's a power of two, swap in a left-shift
                    // just slap it right after the label
                    TB_Reg new_op = tb_function_insert_after(f, i);

                    f->nodes[new_op].type = TB_INTEGER_CONST;
                    f->nodes[new_op].dt = dt;
                    f->nodes[new_op].integer.num_words = 1;
                    f->nodes[new_op].integer.single_word = log2;

                    n->type = TB_SHL;
                    n->dt = dt;
                    n->i_arith = (struct TB_NodeIArith) { .a = a - f->nodes, .b = new_op };
                    changes++;
                }
            }
        } else if (n->type == TB_SDIV || n->type == TB_UDIV) {
            TB_Node* a = &f->nodes[n->i_arith.a];
            TB_Node* b = &f->nodes[n->i_arith.b];
            TB_DataType dt = n->dt;

            tb_assume(dt.type == TB_INT && dt.data > 0);
            if (dt.data <= 64) {
                OPTIMIZER_LOG(i, "TODO division by one folding doesn't work on 64bit+ integers");
                continue;
            }

            if (b->type == TB_INTEGER_CONST &&
                b->integer.num_words == 1 &&
                b->integer.single_word == 1) {
                OPTIMIZER_LOG(i, "Removed division-by-one");

                n->type = TB_PASS;
                n->dt = dt;
                n->pass.value = a - f->nodes;
                changes++;
            }
        }
    }

    return (changes > 0);
}
