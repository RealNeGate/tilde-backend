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

        if (n->type >= TB_AND && n->type <= TB_SDIV) {
            TB_Node* a = &f->nodes.data[n->i_arith.a];
            TB_Node* b = &f->nodes.data[n->i_arith.b];
            TB_ArithmaticBehavior ab = n->i_arith.arith_behavior;

			if (dt.type != TB_INT) continue;
			if (a->type != b->type) continue;
            if (a->type != TB_INTEGER_CONST) continue;

			if (a->integer.num_words == 1) {
				OPTIMIZER_LOG(n - f->nodes.data, "constant folded operation");
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
						if (tb_add_overflow(ai << shift, bi << shift, &result)) {
							result >>= shift;

							if (ab == TB_CAN_WRAP) result &= mask;
							else if (ab == TB_SATURATED_UNSIGNED) result = mask;
							else if (ab == TB_SATURATED_SIGNED) tb_todo();
						} else {
							result = (result >> shift) & mask;
						}
						break;
					}
					case TB_SUB: {
						if (tb_sub_overflow(ai << shift, bi << shift, &result)) {
							result >>= shift;

							if (ab == TB_CAN_WRAP) result &= mask;
							else if (ab == TB_SATURATED_UNSIGNED) result = mask;
							else if (ab == TB_SATURATED_SIGNED) tb_todo();
						} else {
							result = (result >> shift) & mask;
						}
						break;
					}
					case TB_MUL: {
						TB_MultiplyResult res = tb_mul64x128(ai, bi);

						if ((res.lo & ~mask) || res.hi) {
							result = res.lo & mask;

							if (ab == TB_SATURATED_SIGNED) {
								tb_todo();
								result = ((int64_t)res.lo) >= 0 ? INT64_MAX : INT64_MIN;
							} else if (ab == TB_SATURATED_UNSIGNED) {
								result = UINT64_MAX;
							} else if (ab == TB_SIGNED_TRAP_ON_WRAP) {
								tb_panic("compile time trap");
							} else if (ab == TB_UNSIGNED_TRAP_ON_WRAP) {
								tb_panic("compile time trap");
							}
						} else {
							result = res.lo & mask;
						}
						break;
					}
					case TB_UDIV:
					case TB_SDIV: {
						if (bi == 0) {
							OPTIMIZER_LOG(n - f->nodes.data, "division by zero converted to poison");
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
				OPTIMIZER_LOG(n - f->nodes.data, "TODO constant folding on big integers");
			}
        } else if (n->type == TB_SIGN_EXT) {
            TB_Node* src = &f->nodes.data[n->unary.src];

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
            TB_Node* src = &f->nodes.data[n->unary.src];

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
            TB_Node* src = &f->nodes.data[n->unary.src];

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
