#include "../tb_internal.h"

#define MASK_UPTO(pos) (~UINT64_C(0) >> (64 - pos))
#define BEXTR(src,pos) (((src) >> (pos)) & 1)
static uint64_t sxt(uint64_t src, uint64_t src_bits, uint64_t dst_bits) {
    uint64_t sign_bit = BEXTR(src, src_bits-1);
    uint64_t mask = MASK_UPTO(dst_bits) & ~MASK_UPTO(src_bits);

    uint64_t dst = src & ~mask;
    return dst | (sign_bit ? mask : 0);
}

bool tb_opt_fold(TB_Function* f) {
    int changes = 0;

    TB_FOR_EACH_NODE(n, f) {
        TB_DataType dt = n->dt;

        if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_ULE) {
            TB_Node* a = &f->nodes[n->cmp.a];
            TB_Node* b = &f->nodes[n->cmp.b];

            if (dt.type != TB_INT) continue;
            if (a->type != b->type) continue;
            if (a->type != TB_INTEGER_CONST) continue;

            if (a->integer.num_words == 1) {
                OPTIMIZER_LOG(n - f->nodes, "constant fold comparisons");
                assert(b->integer.num_words == 1);

                // fast path: single word
                uint64_t ai = a->integer.single_word;
                uint64_t bi = b->integer.single_word;

                uint64_t diff;
                bool overflow = tb_sub_overflow(ai, bi, &diff);
                bool sign = diff & (1u << (dt.data - 1));

                bool result = false;
                switch (n->type) {
                    case TB_CMP_EQ:  result = (diff == 0); break;
                    case TB_CMP_NE:  result = (diff != 0); break;
                    case TB_CMP_SLT: result = (sign != overflow); break;
                    case TB_CMP_SLE: result = (diff == 0) || (sign != overflow); break;
                    case TB_CMP_ULT: result = (overflow); break;
                    case TB_CMP_ULE: result = (diff == 0) || overflow; break;
                    default: tb_unreachable();
                }

                n->type = TB_INTEGER_CONST;
                n->dt = TB_TYPE_BOOL;
                n->integer.num_words = 1;
                n->integer.single_word = result;
                changes++;
            } else {
                OPTIMIZER_LOG(n - f->nodes, "TODO implement large int compares");
            }
        } else if (n->type >= TB_AND && n->type <= TB_SDIV) {
            TB_Node* a = &f->nodes[n->i_arith.a];
            TB_Node* b = &f->nodes[n->i_arith.b];
            TB_ArithmaticBehavior ab = n->i_arith.arith_behavior;

            if (dt.type != TB_INT) continue;
            if (a->type != b->type) continue;
            if (a->type != TB_INTEGER_CONST) continue;

            if (a->integer.num_words == 1) {
                OPTIMIZER_LOG(n - f->nodes, "constant folded operation");
                assert(b->integer.num_words == 1);

                // fast path: single word
                uint64_t ai = a->integer.single_word;
                uint64_t bi = b->integer.single_word;

                uint64_t shift = 64-dt.data;
                uint64_t mask = ~UINT64_C(0) >> shift;

                uint64_t result;
                switch (n->type) {
                    case TB_AND: result = ai & bi; break;
                    case TB_XOR: {
                        result = ai ^ bi;
                        break;
                    }
                    case TB_OR: {
                        result = ai | bi;
                        break;
                    }
                    case TB_ADD: {
                        bool ovr = tb_add_overflow(ai << shift, bi << shift, &result);

                        if ((ab & TB_ARITHMATIC_NUW) && ovr) {
                            OPTIMIZER_LOG(n - f->nodes, "add overflow poison");
                            n->type = TB_POISON;
                            goto skip_normal_const;
                        } else {
                            result = (result >> shift) & mask;
                        }
                        break;
                    }
                    case TB_SUB: {
                        bool ovr = tb_sub_overflow(ai << shift, bi << shift, &result);
                        if ((ab & TB_ARITHMATIC_NUW) && ovr) {
                            OPTIMIZER_LOG(n - f->nodes, "add overflow poison");
                            n->type = TB_POISON;
                            goto skip_normal_const;
                        } else {
                            result = (result >> shift) & mask;
                        }
                        break;
                    }
                    case TB_MUL: {
                        TB_MultiplyResult res = tb_mul64x128(ai, bi);

                        if ((ab & TB_ARITHMATIC_NUW) && (res.hi || res.lo & ~mask)) {
                            OPTIMIZER_LOG(n - f->nodes, "multiply overflow poison");
                            n->type = TB_POISON;
                            goto skip_normal_const;
                        } else if ((ab & TB_ARITHMATIC_NSW) && res.hi != res.lo >> 63) {
                            OPTIMIZER_LOG(n - f->nodes, "multiply overflow poison");
                            n->type = TB_POISON;
                            goto skip_normal_const;
                        } else {
                            result = res.lo & mask;
                        }
                        break;
                    }
                    case TB_UDIV:
                    case TB_SDIV: {
                        if (bi == 0) {
                            OPTIMIZER_LOG(n - f->nodes, "division by zero converted to poison");
                            n->type = TB_POISON;
                            goto skip_normal_const;
                        }

                        if (n->type == TB_SDIV) {
                            result = ((int64_t)ai) / ((int64_t)bi);
                            result &= mask;
                        } else {
                            result = (ai / bi) & mask;
                        }
                        break;
                    }
                    case TB_SHL: {
                        result = (ai << bi);
                        break;
                    }
                    case TB_SHR: {
                        result = (ai >> bi);
                        break;
                    }
                    case TB_SAR: {
                        tb_todo();

                        bool sign_bit = BEXTR(ai, dt.data - 1);
                        uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);

                        result >>= bi;
                        if (sign_bit) result |= mask;
                        break;
                    }
                    default: tb_todo();
                }

                n->type = TB_INTEGER_CONST;
                n->integer.num_words = 1;
                n->integer.single_word = result;

                skip_normal_const:
                changes++;
            } else {
                OPTIMIZER_LOG(n - f->nodes, "TODO constant folding on big integers");
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

                    loop_range(i, src_num_words, dst_num_words) {
                        words[i] = is_signed ? ~UINT64_C(0) : 0;
                    }

                    n->type = TB_INTEGER_CONST;
                    n->integer.num_words = dst_num_words;
                    n->integer.words = words;
                }

                changes++;
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

                    loop_range(i, src->integer.num_words, dst_num_words) {
                        words[i] = 0;
                    }

                    n->type = TB_INTEGER_CONST;
                    n->integer.num_words = dst_num_words;
                    n->integer.words = words;
                }

                changes++;
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

                changes++;
            }
        }
    }

    return changes;
}
