#if 0
// pattern operand options
enum {
    ZERO, INT, ANY,
};

// fold result types
typedef struct {
    enum {
        FOLDED_FAIL,    // do nothing
        FOLDED_POISON,  // value is erronous
        FOLDED_CONST,   // result is the value
        FOLDED_PASS,    // value is equivalent to a previous one
    } tag;
    union {
        TB_Reg pass;
        uint64_t result;
    };
} Folded;

#define F_FAIL(x) ((Folded){ FOLDED_FAIL })
#define F_POISON(x) ((Folded){ FOLDED_POISON })
#define F_CONST(x) ((Folded){ FOLDED_CONST, .result = (x) })
#define F_PASS(r) ((Folded){ FOLDED_PASS, .pass = (r) })

// #define FOLDS X(TB_ADD, INT, INT)
#endif

#define MASK_UPTO(pos) (~UINT64_C(0) >> (64 - pos))
#define BEXTR(src,pos) (((src) >> (pos)) & 1)
static uint64_t sxt(uint64_t src, uint64_t src_bits, uint64_t dst_bits) {
    uint64_t sign_bit = BEXTR(src, src_bits-1);
    uint64_t mask = MASK_UPTO(dst_bits) & ~MASK_UPTO(src_bits);

    uint64_t dst = src & ~mask;
    return dst | (sign_bit ? mask : 0);
}

static bool single_word_compare_fold(TB_NodeTypeEnum node_type, TB_DataType dt, uint64_t ai, uint64_t bi) {
    uint64_t diff;
    bool overflow = tb_sub_overflow(ai, bi, &diff);
    bool sign = diff & (1u << (dt.data - 1));

    switch (node_type) {
        case TB_CMP_EQ:  return (diff == 0);
        case TB_CMP_NE:  return (diff != 0);
        case TB_CMP_SLT: return (sign != overflow);
        case TB_CMP_SLE: return (diff == 0) || (sign != overflow);
        case TB_CMP_ULT: return (overflow);
        case TB_CMP_ULE: return (diff == 0) || overflow;
        default: tb_unreachable(); return false;
    }
}

typedef struct {
    uint64_t result;
    bool poison;
} ArithResult;

static ArithResult single_word_arith_fold(TB_NodeTypeEnum node_type, TB_DataType dt, uint64_t ai, uint64_t bi, TB_ArithmaticBehavior ab) {
    uint64_t shift = 64-dt.data;
    uint64_t mask = ~UINT64_C(0) >> shift;

    switch (node_type) {
        case TB_AND: return (ArithResult){ ai & bi };
        case TB_XOR: return (ArithResult){ ai ^ bi };
        case TB_OR:  return (ArithResult){ ai | bi };
        case TB_ADD: {
            uint64_t result;
            bool ovr = tb_add_overflow(ai << shift, bi << shift, &result);

            if ((ab & TB_ARITHMATIC_NUW) && ovr) {
                return (ArithResult){ 0, true };
            } else {
                return (ArithResult){ (result >> shift) & mask };
            }
            break;
        }
        case TB_SUB: {
            uint64_t result;
            bool ovr = tb_sub_overflow(ai << shift, bi << shift, &result);

            if ((ab & TB_ARITHMATIC_NUW) && ovr) {
                return (ArithResult){ 0, true };
            } else {
                return (ArithResult){ (result >> shift) & mask };
            }
        }
        case TB_MUL: {
            TB_MultiplyResult res = tb_mul64x128(ai, bi);

            if ((ab & TB_ARITHMATIC_NUW) && (res.hi || res.lo & ~mask)) {
                return (ArithResult){ 0, true };
            } else if ((ab & TB_ARITHMATIC_NSW) && res.hi != res.lo >> 63) {
                return (ArithResult){ 0, true };
            } else {
                return (ArithResult){ res.lo & mask };
            }
        }
        case TB_UDIV:
        case TB_SDIV: {
            if (bi == 0) {
                return (ArithResult){ 0, true };
            }

            if (node_type == TB_SDIV) {
                return (ArithResult){ ((int64_t)ai / (int64_t)bi) & mask };
            } else {
                return (ArithResult){ (ai / bi) & mask };
            }
        }
        case TB_SHL: {
            return (ArithResult){ (ai << bi) };
        }
        case TB_SHR: {
            return (ArithResult){ (ai >> bi) };
        }
        case TB_SAR: {
            tb_assert_once("Idk if this works");

            bool sign_bit = BEXTR(ai, dt.data - 1);
            uint64_t mask = (~UINT64_C(0) >> (64 - dt.data)) << dt.data;

            return (ArithResult){ (ai >> bi) | (sign_bit ? mask : 0) };
        }
        default: tb_todo();
    }
}

static bool is_associative(TB_NodeTypeEnum type) {
    switch (type) {
        case TB_ADD: case TB_MUL:
        case TB_AND: case TB_XOR: case TB_OR:
        case TB_CMP_NE: case TB_CMP_EQ:
        return true;

        default:
        return false;
    }
}

typedef struct {
    TB_Reg r;
    bool changes;
} BinOpReg;

static bool const_fold(TB_Function* f, TB_Node* n);

static BinOpReg create_binop(TB_Function* f, TB_Reg r, TB_Reg x, TB_Reg y) {
    TB_NodeTypeEnum type = f->nodes[r].type;
    TB_DataType dt = f->nodes[r].dt;

    bool changes = false;
    if (f->nodes[x].type == TB_INTEGER_CONST && f->nodes[y].type != TB_INTEGER_CONST) {
        OPTIMIZER_LOG(r, "moved constants to right hand side.");
        tb_swap(TB_Reg, x, y);

        f->nodes[r].i_arith.a = x;
        f->nodes[r].i_arith.b = y;
        changes = true;
    }

    if (f->nodes[x].type == type && f->nodes[y].type != type) {
        // Reshuffle the adds from
        // (x + y) + z => x + (y + z)
        OPTIMIZER_LOG(r, "Reassociated expressions");

        TB_Reg xx = f->nodes[x].i_arith.a;
        TB_Reg yy = f->nodes[x].i_arith.b;
        TB_Reg zz = y;

        TB_Reg xy_reg = tb_function_insert_before(f, r);

        TB_Node* xy = &f->nodes[xy_reg];
        xy->type = type;
        xy->dt = dt;
        xy->i_arith.a = yy;
        xy->i_arith.b = zz;
        if (type >= TB_ADD && type <= TB_SMOD) {
            xy->i_arith.arith_behavior = f->nodes[r].i_arith.arith_behavior;
        }

        BinOpReg new_xy = create_binop(f, xy_reg, xx, yy);
        const_fold(f, &f->nodes[new_xy.r]);

        TB_Node* n = &f->nodes[r];
        n->i_arith.a = xx;
        n->i_arith.b = new_xy.r;
        changes = true;
    }

    return (BinOpReg){ r, changes };
}

static bool phi_motion(TB_Function* f, TB_Node* n) {
    TB_Reg r = (n - f->nodes);

    if (tb_node_is_phi_node(f, r)) {
        int count = tb_node_get_phi_width(f, r);
        TB_PhiInput* inputs = tb_node_get_phi_inputs(f, r);

        // (phi (add X A)), (add Y A)) => (add (phi(X, Y) A)
        TB_NodeTypeEnum shared_type = 0;
        TB_Reg shared_a = 0;
        TB_ArithmaticBehavior shared_ab = 0;

        bool success = (count > 0);
        FOREACH_N(j, 0, count) {
            TB_Reg a = inputs[j].val;

            if (f->nodes[a].type >= TB_AND && f->nodes[a].type <= TB_SMOD) {
                if (shared_type == 0) {
                    // decide on a shared value
                    shared_a = f->nodes[a].i_arith.a;
                    shared_type = f->nodes[a].type;
                    shared_ab = f->nodes[a].i_arith.arith_behavior;
                } else if (shared_type != f->nodes[a].type || f->nodes[a].i_arith.arith_behavior == shared_ab) {
                    // does it not share the left side?
                    if (f->nodes[a].i_arith.a != shared_a) {
                        success = false;
                        break;
                    }
                }
            } else {
                success = false;
                break;
            }
        }

        if (success) {
            FOREACH_N(j, 0, count) {
                TB_Reg a = inputs[j].val;

                if (f->nodes[a].type >= TB_AND && f->nodes[a].type <= TB_SMOD) {
                    inputs[j].val = f->nodes[a].i_arith.b;
                }
            }

            TB_Label bb = tb_find_label_from_reg(f, r);
            TB_Reg shared_op = tb_function_insert_after(f, bb, r);
            f->nodes[shared_op].type = shared_type;
            f->nodes[shared_op].dt = f->nodes[r].dt;
            f->nodes[shared_op].i_arith.a = shared_a;
            f->nodes[shared_op].i_arith.b = 0;
            f->nodes[shared_op].i_arith.arith_behavior = shared_ab;

            tb_function_find_replace_reg(f, r, shared_op);

            // set after the replace op
            f->nodes[shared_op].i_arith.b = r;
            return true;
        }
    }

    return false;
}

static bool reassoc(TB_Function* f, TB_Node* n) {
    if (!is_associative(n->type)) {
        return false;
    }

    TB_Reg old = n - f->nodes;
    BinOpReg result = create_binop(f, old, n->i_arith.a, n->i_arith.b);
    if (old != result.r) {
        f->nodes[old].type = TB_PASS;
        f->nodes[old].pass.value = result.r;
        return true;
    }

    return result.changes;
}

static bool const_fold(TB_Function* f, TB_Node* n) {
    TB_DataType dt = n->dt;

    if ((n->type >= TB_CMP_EQ && n->type <= TB_CMP_ULE) || (n->type >= TB_ADD && n->type <= TB_SMOD)) {
        // we don't fold anything but integers
        if (dt.type != TB_INT) return false;

        TB_Node* a = &f->nodes[n->i_arith.a];
        TB_Node* b = &f->nodes[n->i_arith.b];
        // printf("TRY FOLD? r%d OP r%d\n", n->i_arith.a, n->i_arith.b);

        if (a->type == TB_INTEGER_CONST && b->type == TB_INTEGER_CONST) {
            if (a->integer.num_words == 1) {
                OPTIMIZER_LOG(n - f->nodes, "constant fold");
                assert(b->integer.num_words == 1);

                uint64_t ai = a->integer.single_word;
                uint64_t bi = b->integer.single_word;

                uint64_t result;
                if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_ULE) {
                    result = single_word_compare_fold(n->type, dt, ai, bi) ? 1 : 0;
                } else {
                    ArithResult r = single_word_arith_fold(n->type, dt, ai, bi, n->i_arith.arith_behavior);
                    if (r.poison) {
                        n->type = TB_POISON;
                        return true;
                    }

                    result = r.result;
                }

                n->type = TB_INTEGER_CONST;
                n->dt = dt;
                n->integer.num_words = 1;
                n->integer.single_word = result;
                return true;
            } else {
                OPTIMIZER_LOG(n - f->nodes, "TODO implement large int fold");
            }
        } else if (b->type == TB_INTEGER_CONST && b->integer.num_words == 1) {
            OPTIMIZER_LOG(n - f->nodes, "partial folding");

            TB_Reg ar = n->i_arith.a;
            if (b->integer.single_word == 0) {
                switch (n->type) {
                    case TB_ADD: case TB_SUB:
                    case TB_XOR: case TB_OR:
                    case TB_SHL: case TB_SHR:
                    case TB_SAR:
                    n->type = TB_PASS;
                    n->pass.value = ar;
                    return true;

                    case TB_MUL: case TB_AND:
                    n->type = TB_INTEGER_CONST;
                    n->integer.num_words = 1;
                    n->integer.single_word = 0;
                    return true;

                    case TB_SDIV: case TB_UDIV:
                    n->type = TB_POISON;
                    return true;

                    default: break;
                }
            } else if (b->integer.single_word == 1) {
                switch (n->type) {
                    case TB_MUL: case TB_SDIV: case TB_UDIV:
                    n->type = TB_PASS;
                    n->pass.value = ar;
                    return true;

                    default: break;
                }
            }

            TB_Reg r = n - f->nodes;
            TB_Label bb = tb_find_label_from_reg(f, ar);
            if (n->type == TB_MUL) {
                // (a * b) => (a << log2(b)) where b is a power of two
                uint64_t log2 = tb_ffs(b->integer.single_word) - 1;
                if (b->integer.single_word == (UINT64_C(1) << log2)) {
                    OPTIMIZER_LOG(r, "converted power-of-two multiply into left shift");

                    // It's a power of two, swap in a left-shift
                    // just slap it right after the label
                    TB_Reg new_op = tb_function_insert_after(f, bb, ar);

                    f->nodes[new_op].type = TB_INTEGER_CONST;
                    f->nodes[new_op].dt = dt;
                    f->nodes[new_op].integer.num_words = 1;
                    f->nodes[new_op].integer.single_word = log2;

                    n->type = TB_SHL;
                    n->dt = dt;
                    n->i_arith = (struct TB_NodeIArith) { .a = a - f->nodes, .b = new_op };
                    return true;
                }
            } else if (n->type == TB_UMOD || n->type == TB_SMOD) {
                // (mod a N) => (and a N-1) where N is a power of two
                uint64_t mask = b->integer.single_word;
                if (tb_is_power_of_two(mask)) {
                    OPTIMIZER_LOG(r, "converted modulo into AND with constant mask");

                    // generate mask
                    TB_Reg extra_reg = tb_function_insert_after(f, bb, ar);
                    TB_Node* extra = &f->nodes[extra_reg];
                    extra->type = TB_INTEGER_CONST;
                    extra->dt = n->dt;
                    extra->integer.num_words = 1;
                    extra->integer.single_word = mask - 1;

                    // new AND operation to replace old MOD
                    n = &f->nodes[r];
                    n->type = TB_AND;
                    n->i_arith.b = extra_reg;
                    return true;
                }
            }
        }
    } else if (n->type == TB_SIGN_EXT) {
        TB_Node* src = &f->nodes[n->unary.src];

        if (src->type == TB_INTEGER_CONST) {
            assert(src->dt.type == TB_INT && src->dt.data > 0);
            assert(n->dt.type == TB_INT && n->dt.data > 0);

            if (src->integer.num_words == 1 && n->dt.data <= 64) {
                // fast path: single word
                n->type = TB_INTEGER_CONST;
                n->integer.num_words = 1;
                n->integer.single_word = sxt(src->integer.single_word, src->dt.data, dt.data);
            } else {
                int src_num_words = src->integer.num_words;
                int dst_num_words = (n->dt.data + (BigIntWordSize*8) - 1) / (BigIntWordSize*8);
                BigInt_t* words = tb_platform_heap_alloc(BigIntWordSize * dst_num_words);

                bool is_signed = false;
                if (src_num_words == 1) {
                    is_signed = BEXTR(src->integer.single_word, src->dt.data-1);
                    src->integer.words[0] = src->integer.single_word;
                } else {
                    is_signed = BigInt_bextr(src->integer.num_words, src->integer.words, src->dt.data-1);
                    memcpy(words, src->integer.words, BigIntWordSize * src_num_words);
                }

                // fixup the bits here
                uint64_t mask = ~UINT64_C(0) >> (64 - (src->dt.data % 64));
                if (is_signed) words[src_num_words-1] |= mask;
                else words[src->integer.num_words-1] &= ~mask;

                FOREACH_N(i, src_num_words, dst_num_words) {
                    words[i] = is_signed ? ~UINT64_C(0) : 0;
                }

                n->type = TB_INTEGER_CONST;
                n->integer.num_words = dst_num_words;
                n->integer.words = words;
            }

            return true;
        }
    } else if (n->type == TB_ZERO_EXT) {
        TB_Node* src = &f->nodes[n->unary.src];

        if (src->type == TB_INTEGER_CONST) {
            assert(src->dt.type == TB_INT && src->dt.data > 0);

            if (src->integer.num_words == 1 && dt.data <= 64) {
                // fast path: single word
                uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);
                uint64_t num = (src->integer.single_word & mask);

                n->type = TB_INTEGER_CONST;
                n->integer.num_words = 1;
                n->integer.single_word = num;
            } else {
                int src_num_words = src->integer.num_words;
                int dst_num_words = (n->dt.data + (BigIntWordSize*8) - 1) / (BigIntWordSize*8);
                BigInt_t* words = tb_platform_heap_alloc(BigIntWordSize * dst_num_words);

                if (src_num_words == 1) {
                    src->integer.words[0] = src->integer.single_word;
                } else {
                    memcpy(words, src->integer.words, BigIntWordSize * src_num_words);
                }

                // fixup the bits here
                uint64_t mask = ~UINT64_C(0) >> (64 - (src->dt.data % 64));
                words[src->integer.num_words-1] &= ~mask;

                FOREACH_N(i, src->integer.num_words, dst_num_words) {
                    words[i] = 0;
                }

                n->type = TB_INTEGER_CONST;
                n->integer.num_words = dst_num_words;
                n->integer.words = words;
            }

            return true;
        }
    } else if (n->type == TB_TRUNCATE) {
        TB_Node* src = &f->nodes[n->unary.src];

        if (src->type == TB_INTEGER_CONST) {
            assert(src->dt.type == TB_INT && src->dt.data > 0);

            if (src->integer.num_words == 1) {
                // fast path: single word
                uint64_t mask = ~UINT64_C(0) >> (64 - (src->dt.data % 64));

                n->type = TB_INTEGER_CONST;
                n->integer.num_words = 1;
                n->integer.single_word = src->integer.single_word & mask;
            } else {
                int dst_num_words = (n->dt.data + (BigIntWordSize*8) - 1) / (BigIntWordSize*8);
                BigInt_t* words = tb_platform_heap_alloc(BigIntWordSize * dst_num_words);

                // copy old stuff
                memcpy(words, src->integer.words, BigIntWordSize * dst_num_words);

                // fixup the bits here
                uint64_t mask = ~UINT64_C(0) >> (64 - (src->dt.data % 64));
                words[dst_num_words-1] &= ~mask;

                n->type = TB_INTEGER_CONST;
                n->integer.num_words = 1;
                n->integer.words = words;
            }

            return true;
        }
    }

    return false;
}
