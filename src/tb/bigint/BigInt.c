#include <string.h>
#include <assert.h>

#include "BigInt.h"

#define MaxBigIntWords 2

/* Printing format strings */
#ifndef BigIntWordSize
#error Must define BigIntWordSize to be 1, 2, 4
#elif (BigIntWordSize == 1)
/* Max value of integer type */
#define MAX_VAL ((BigInt_tmp_t)0xFF)
#elif (BigIntWordSize == 2)
#define MAX_VAL ((BigInt_tmp_t)0xFFFF)
#elif (BigIntWordSize == 4)
#define MAX_VAL ((BigInt_tmp_t)0xFFFFFFFF)
#elif (BigIntWordSize == 8)
#define MAX_VAL ((BigInt_tmp_t)0xFFFFFFFFFFFFFFFF)
#endif

/* Bad macros */
#define MIN(A,B) (((A)<(B))?(A):(B))
#define MAX(A,B) (((A)>(B))?(A):(B))

/* Functions for shifting number in-place. */
static void _lshift_one_bit(size_t NumWords, BigInt_t * A);
static void _rshift_one_bit(size_t NumWords, BigInt_t * A);
static void _lshift_word(size_t NumWords, BigInt_t * A, int nwords);
static void _rshift_word(size_t NumWords, BigInt_t * A, int nwords);

/* Endianness issue if machine is not little-endian? */
#ifdef BigIntWordSize
#if (BigIntWordSize == 1)
#define BigInt_FROM_INT(BigInt, Integer) { \
((BigInt_t *)(void *)BigInt)[0] = (((BigInt_tmp_t)Integer) & 0x000000ff); \
((BigInt_t *)(void *)BigInt)[1] = (((BigInt_tmp_t)Integer) & 0x0000ff00) >> 8; \
((BigInt_t *)(void *)BigInt)[2] = (((BigInt_tmp_t)Integer) & 0x00ff0000) >> 16; \
((BigInt_t *)(void *)BigInt)[3] = (((BigInt_tmp_t)Integer) & 0xff000000) >> 24; \
}
#elif (BigIntWordSize == 2)
#define BigInt_FROM_INT(BigInt, Integer) { \
((BigInt_t *)(void *)BigInt)[0] = (((BigInt_tmp_t)Integer) & 0x0000ffff); \
((BigInt_t *)(void *)BigInt)[1] = (((BigInt_tmp_t)Integer) & 0xffff0000) >> 16; \
}
#elif (BigIntWordSize == 4)
#define BigInt_FROM_INT(BigInt, Integer) { \
((BigInt_t *)(void *)BigInt)[0] = ((BigInt_tmp_t)Integer); \
((BigInt_t *)(void *)BigInt)[1] = ((BigInt_tmp_t)Integer) >> ((BigInt_tmp_t)32); \
}
#elif (BigIntWordSize == 8)
#define BigInt_FROM_INT(BigInt, Integer) { \
((BigInt_t *)(void *)BigInt)[0] = ((BigInt_tmp_t)Integer); \
((BigInt_t *)(void *)BigInt)[1] = ((BigInt_tmp_t)Integer) >> ((BigInt_tmp_t)64); \
}
#endif
#endif

/* Public / Exported functions. */
void BigInt_zero(size_t NumWords, BigInt_t * BigInt)
{
    for (size_t i = 0; i < NumWords; ++i) {
        BigInt[i] = 0;
    }
}

void BigInt_from_int(size_t NumWords, BigInt_t * BigInt, BigInt_tmp_t Integer)
{
    BigInt_zero(NumWords, BigInt);
    BigInt_FROM_INT(BigInt, Integer);
}

int BigInt_to_int(size_t NumWords, BigInt_t * BigInt)
{
    int ret = 0;

	/* Endianness issue if machine is not little-endian? */
#if (BigIntWordSize == 1)
    ret += BigInt[0];
    ret += BigInt[1] << 8;
    ret += BigInt[2] << 16;
    ret += BigInt[3] << 24;
#elif (BigIntWordSize == 2)
    ret += BigInt[0];
    ret += BigInt[1] << 16;
#elif (BigIntWordSize == 4)
    ret += BigInt[0];
#elif (BigIntWordSize == 8)
    ret += BigInt[0];
#endif

    return ret;
}

size_t BigInt_truncate(size_t NumWords, BigInt_t * BigInt)
{
    --NumWords;
    while (BigInt[NumWords] == 0 && NumWords > 0) --NumWords;
    return ++NumWords;
}

void BigInt_from_string(size_t NumWords, BigInt_t * BigInt, char * str)
{
	assert(NumWords <= MaxBigIntWords);
    BigInt_zero(NumWords, BigInt);

    BigInt_t temp[MaxBigIntWords];
    BigInt_t digit;
    BigInt_t ten = 10;

    while (*str != 0)
    {
        BigInt_mul(NumWords, BigInt, 1, &ten, NumWords, temp);

        digit = (*(str++)-'0');

        if (digit != 0)
            BigInt_add(NumWords, temp, 1, &digit, NumWords, BigInt);
        else
            BigInt_copy(NumWords, BigInt, temp);
    }
}

static BigInt_t hex_to_word(char * Text, int Length)
{
    BigInt_t word = 0;
    for (int i = 0; i < Length; ++i)
    {
        char character = Text[i];
        word <<= 4;
        if (character >= '0' && character <= '9')
            word += character - '0';
        else if (character <= 'F' && character >= 'A')
            word += character - 'A' + 10;
        else if (character <= 'f' && character >= 'a')
            word += character - 'a' + 10;
    }
    return word;
}

void BigInt_from_hex_string(size_t NumWords, BigInt_t * BigInt, char * Str)
{
    BigInt_zero(NumWords, BigInt);
    size_t length = strlen(Str);

    /* whole Words in this string */
    size_t num_words = length / (BigIntWordSize*2);
    if (num_words * (BigIntWordSize*2) < length) ++num_words; /* round up */

    char * string_word = Str + length;

    for (size_t i = 0; i < num_words; ++i)
    {
        /* How many characters should be read from the string */
        size_t hex_length = MIN(BigIntWordSize*2, string_word-Str);
        string_word -= (BigIntWordSize*2);
        BigInt[i] = hex_to_word(string_word, hex_length);
    }
}

void BigInt_to_hex_string(size_t NumWords, BigInt_t * BigInt, char * Str)
{
    NumWords = BigInt_truncate(NumWords, BigInt);

    size_t str_index = 0;

    for (int_fast32_t d = NumWords-1; d >= 0; --d)
    {
        BigInt_t word = BigInt[d];
        for (int BigInt = 0; BigInt < BigIntWordSize*2; ++BigInt) {
            uint8_t nibble = (word >> (BigInt_t)(BigInt*4)) & 0x0F;
            char hexchar = (nibble <= 9) ? '0' + nibble : 'a' + nibble-10;
            Str[str_index+BigIntWordSize*2-1-BigInt] = hexchar;
        }
        str_index += BigIntWordSize*2;
    }

    Str[str_index] = 0;
}

void BigInt_dec(size_t NumWords, BigInt_t * BigInt)
{
    BigInt_t tmp; /* copy of BigInt */
    BigInt_t res;

    for (size_t i = 0; i < NumWords; ++i) {
        tmp = BigInt[i];
        res = tmp - 1;
        BigInt[i] = res;

        if (!(res > tmp)) {
            break;
        }
    }
}

void BigInt_inc(size_t NumWords, BigInt_t * BigInt)
{
    BigInt_t res;
    BigInt_tmp_t tmp; /* copy of BigInt */

    for (size_t i = 0; i < NumWords; ++i) {
        tmp = BigInt[i];
        res = tmp + 1;
        BigInt[i] = res;

        if (res > tmp) {
            break;
        }
    }
}

int BigInt_bextr(size_t NumWords, BigInt_t * BigInt, int Bit)
{
	size_t ElemIndex, ElemBit;

	ElemIndex = Bit / BigIntWordSize;
	ElemBit = Bit % BigIntWordSize;

	return BigInt[ElemIndex] & (1u << ElemBit);
}

void BigInt_add(size_t AWords, BigInt_t * A, size_t BWords, BigInt_t * B, size_t Out_NumWords, BigInt_t * Out)
{
    /* Make it so that A will be smaller than B */
    if (AWords > BWords)
    {
        size_t temp1 = BWords;
        BWords = AWords;
        AWords = temp1;
        BigInt_t * temp2 = B;
        B = A;
        A = temp2;
    }

    int loop_to = 0;
    size_t loop1 = 0;
    size_t loop2 = 0;
    size_t loop3 = 0;

    if (Out_NumWords <= AWords) {
        loop_to = 1;
        loop1 = Out_NumWords;
    }
    else if (Out_NumWords <= BWords) {
        loop_to = 2;
        loop1 = AWords;
        loop2 = Out_NumWords;
    }
    else {
        loop_to = 3;
        loop1 = AWords;
        loop2 = BWords;
        loop3 = Out_NumWords;
    }

    int carry = 0;
    BigInt_tmp_t tmp;
    size_t i;

    for (i = 0; i < loop1; ++i)
    {
        tmp = (BigInt_tmp_t)A[i] + B[i] + carry;
        carry = (tmp > MAX_VAL);
        Out[i] = (tmp & MAX_VAL);
    }

    if (loop_to == 1) return;

    for (; i < loop2; ++i)
    {
        tmp = (BigInt_tmp_t)B[i] + 0 + carry;
        carry = (tmp > MAX_VAL);
        Out[i] = (tmp & MAX_VAL);
    }

    if (loop_to == 2) return;

    /* Do the carry, then fill the rest with zeros */
    Out[i++] = carry;
    for (; i < loop3; ++i) Out[i] = 0;
}

void BigInt_sub(size_t AWords, BigInt_t * A, size_t BWords, BigInt_t * B, size_t Out_NumWords, BigInt_t * Out)
{
    int loop_to = 0;
    size_t loop1 = 0;
    size_t loop2 = 0;
    size_t loop3 = 0;

    if (Out_NumWords <= MIN(AWords, BWords))
    {
        loop_to = 1;
        loop1 = MIN(AWords, BWords);
    }
    else if (Out_NumWords <= MAX(AWords, BWords))
    {
        loop_to = 2;
        loop1 = MIN(AWords, BWords);
        loop2 = Out_NumWords;
    }
    else {
        loop_to = 3;
        loop1 = AWords;
        loop2 = BWords;
        loop3 = Out_NumWords;
    }

    BigInt_tmp_t res;
    BigInt_tmp_t tmp1;
    BigInt_tmp_t tmp2;
    int borrow = 0;
    size_t i;

    for (i = 0; i < loop1; ++i) {
        tmp1 = (BigInt_tmp_t)A[i] + (MAX_VAL + 1); /* + number_base */
        tmp2 = (BigInt_tmp_t)B[i] + borrow;
        ;
        res = (tmp1 - tmp2);
        Out[i] = (BigInt_t)(res & MAX_VAL); /* "modulo number_base" == "%
            (number_base - 1)" if number_base is 2^N */
        borrow = (res <= MAX_VAL);
    }

    if (loop_to == 1) return;

    if (AWords > BWords)
    {
        for (; i < loop2; ++i) {
            tmp1 = (BigInt_tmp_t)A[i] + (MAX_VAL + 1);
            tmp2 =  borrow;
            res = (tmp1 - tmp2);
            Out[i] = (BigInt_t)(res & MAX_VAL);
            borrow = (res <= MAX_VAL);
        }
    }
    else
    {
        for (; i < loop2; ++i) {
            tmp1 = (BigInt_tmp_t)MAX_VAL + 1;
            tmp2 = (BigInt_tmp_t)B[i] + borrow;
            res = (tmp1 - tmp2);
            Out[i] = (BigInt_t)(res & MAX_VAL);
            borrow = (res <= MAX_VAL);
        }
    }

    if (loop_to == 2) return;

    for (; i < loop3; ++i) {
        tmp1 = (BigInt_tmp_t)0 + (MAX_VAL + 1);
        tmp2 = (BigInt_tmp_t)0 + borrow;
        res = (tmp1 - tmp2);
        Out[i] = (BigInt_t)(res & MAX_VAL);
        borrow = (res <= MAX_VAL);
    }
}

void BigInt_mul_basic(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
	assert(NumWords <= MaxBigIntWords);
	
    BigInt_t row[MaxBigIntWords];
    BigInt_t tmp[MaxBigIntWords];
    size_t i, j;

    BigInt_zero(NumWords, Out);

    for (i = 0; i < NumWords; ++i) {
        BigInt_zero(NumWords, row);

        for (j = 0; j < NumWords; ++j) {
            if (i + j < NumWords) {
                BigInt_zero(NumWords, tmp);
                BigInt_tmp_t intermediate = ((BigInt_tmp_t)A[i] * (BigInt_tmp_t)B[j]);
                BigInt_from_int(NumWords, tmp, intermediate);
                _lshift_word(NumWords, tmp, i + j);
                BigInt_add(NumWords, tmp, NumWords, row, NumWords, row);
            }
        }
        BigInt_add(NumWords, Out, NumWords, row, NumWords, Out);
    }
}

/* Cool USSR algorithm for fast multiplication (THERE IS NOT A SINGLE 100% CORRECT PSEUDO CODE ONLINE) */
static void BigInt_Karatsuba_internal(size_t num1_NumWords, BigInt_t * num1, size_t num2_NumWords, BigInt_t * num2, size_t Out_NumWords, BigInt_t * Out, int rlevel) /* Out should be XWords + YWords in size to always avoid overflow */
{
    /* Optimise the size, to avoid any waste any resources */
    num1_NumWords = BigInt_truncate(num1_NumWords, num1);
    num2_NumWords = BigInt_truncate(num2_NumWords, num2);

    if (num1_NumWords == 0 || num2_NumWords == 0)
    {
        BigInt_zero(Out_NumWords, Out);
        return;
    }
    if (num1_NumWords == 1 && num2_NumWords == 1)
    {
        BigInt_tmp_t result = ((BigInt_tmp_t)(*num1)) * ((BigInt_tmp_t)(*num2));
        if (Out_NumWords == 2) { BigInt_FROM_INT(Out, result); }
        else BigInt_from_int(Out_NumWords, Out, result);
        return;
    }

    size_t m = MIN(num2_NumWords, num1_NumWords);
    size_t m2 = m / 2;
    /* do A round up, this is what stops infinite recursion when the inputs are size 1 and 2 */
    if ((m % 2) == 1) ++m2;

    /* low 1 */
    size_t low1_NumWords = m2;
    BigInt_t * low1 = num1;
    /* high 1 */
    size_t high1_NumWords = num1_NumWords - m2;
    BigInt_t * high1 = num1 + m2;
    /* low 2 */
    size_t low2_NumWords = m2;
    BigInt_t * low2 = num2;
    /* high 2 */
    size_t high2_NumWords = num2_NumWords - m2;
    BigInt_t * high2 = num2 + m2;

    // z0 = karatsuba(low1, low2)
    // z1 = karatsuba((low1 + high1), (low2 + high2))
    // z2 = karatsuba(high1, high2)
    size_t z0_NumWords = low1_NumWords + low2_NumWords;
	assert(z0_NumWords <= MaxBigIntWords);
    BigInt_t z0[MaxBigIntWords];
	
    size_t z1_NumWords = (MAX(low1_NumWords, high1_NumWords)+1) + (MAX(low2_NumWords, high2_NumWords)+1);
	assert(z1_NumWords <= MaxBigIntWords);
    BigInt_t z1[MaxBigIntWords];
	
    size_t z2_NumWords =  high1_NumWords + high2_NumWords;
    int use_out_as_z2 = (Out_NumWords >= z2_NumWords); /* Sometimes we can use Out to store z2, then we don't have to copy from z2 to out later (2X SPEEDUP!) */
    if (use_out_as_z2) {BigInt_zero(Out_NumWords-(z2_NumWords),Out+z2_NumWords);}/* The remaining part of Out must be ZERO'D */
	
	assert(z2_NumWords <= MaxBigIntWords);
	BigInt_t tmp[MaxBigIntWords];
    BigInt_t * z2 = (use_out_as_z2) ? Out : tmp;

    /* Make z0 and z2 */
    BigInt_Karatsuba_internal(low1_NumWords, low1, low2_NumWords, low2, z0_NumWords, z0, rlevel+1);
    BigInt_Karatsuba_internal(high1_NumWords, high1, high2_NumWords, high2, z2_NumWords, z2, rlevel+1);

    /* make z1 */
    {
        size_t low1high1_NumWords = MAX(low1_NumWords, high1_NumWords)+1;
        size_t low2high2_NumWords = MAX(low2_NumWords, high2_NumWords)+1;
		assert(low1high1_NumWords <= MaxBigIntWords && low2high2_NumWords <= MaxBigIntWords);
        BigInt_t low1high1[MaxBigIntWords];
        BigInt_t low2high2[MaxBigIntWords];
        BigInt_add(low1_NumWords, low1, high1_NumWords, high1, low1high1_NumWords, low1high1);
        BigInt_add(low2_NumWords, low2, high2_NumWords, high2, low2high2_NumWords, low2high2);
        BigInt_Karatsuba_internal(low1high1_NumWords, low1high1, low2high2_NumWords, low2high2, z1_NumWords, z1, rlevel+1);
    }

    // return (z2 * 10 ^ (m2 * 2)) + ((z1 - z2 - z0) * 10 ^ m2) + z0
    BigInt_sub(z1_NumWords, z1, z2_NumWords, z2, z1_NumWords, z1);
    BigInt_sub(z1_NumWords, z1, z0_NumWords, z0, z1_NumWords, z1);
    if (!use_out_as_z2) BigInt_copy_dif(Out_NumWords, Out, z2_NumWords, z2);
    _lshift_word(Out_NumWords, Out, m2);
    BigInt_add(z1_NumWords, z1, Out_NumWords, Out, Out_NumWords, Out);
    _lshift_word(Out_NumWords, Out, m2);
    BigInt_add(Out_NumWords, Out, z0_NumWords, z0, Out_NumWords, Out);
}

void BigInt_mul(size_t ANumWords, BigInt_t * A, size_t BNumWords, BigInt_t * B, size_t OutNumWords, BigInt_t * Out)
{
    BigInt_Karatsuba_internal(ANumWords, A, BNumWords, B, OutNumWords, Out, 0);
}

void BigInt_div(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
	assert(NumWords <= MaxBigIntWords);
	BigInt_t current[NumWords];
	BigInt_t denom[NumWords];
	BigInt_t tmp[NumWords];

    BigInt_from_int(NumWords, current, 1); // int current = 1;
    BigInt_copy(NumWords, denom, B); // denom = B
    BigInt_copy(NumWords, tmp, A); // tmp   = A

    const BigInt_tmp_t half_max = 1 + (BigInt_tmp_t)(MAX_VAL / 2);
    int overflow = 0;
    while (BigInt_cmp(NumWords, denom, A) != LARGER) // while (denom <= A) {
    {
        if (denom[NumWords - 1] >= half_max) {
            overflow = 1;
            break;
        }
        _lshift_one_bit(NumWords, current); //   current <<= 1;
        _lshift_one_bit(NumWords, denom); //   denom <<= 1;
    }
    if (!overflow) {
        _rshift_one_bit(NumWords, denom); // denom >>= 1;
        _rshift_one_bit(NumWords, current); // current >>= 1;
    }
    BigInt_zero(NumWords, Out); // int answer = 0;

    while (!BigInt_is_zero(NumWords, current)) // while (current != 0)
    {
        if (BigInt_cmp(NumWords, tmp, denom) != SMALLER) //   if (dividend >= denom)
        {
            BigInt_sub(NumWords, tmp, NumWords, denom, NumWords, tmp); //     dividend -= denom;
            BigInt_or(NumWords, Out, current, Out); //     answer |= current;
        }
        _rshift_one_bit(NumWords, current); //   current >>= 1;
        _rshift_one_bit(NumWords, denom); //   denom >>= 1;
    }
}

void BigInt_lshift(size_t NumWords, BigInt_t * B, int nbits)
{
    /* Handle shift in multiples of word-size */
    const int nbits_pr_word = (BigIntWordSize * 8);
    int nwords = nbits / nbits_pr_word;
    if (nwords != 0) {
        _lshift_word(NumWords, B, nwords);
        nbits -= (nwords * nbits_pr_word);
    }

    if (nbits != 0) {
        size_t i;
        for (i = (NumWords - 1); i > 0; --i) {
            B[i] = (B[i] << nbits) | (B[i - 1] >> ((8 * BigIntWordSize) - nbits));
        }
        B[i] <<= nbits;
    }
}

void BigInt_rshift(size_t NumWords, BigInt_t * B, int nbits)
{
    /* Handle shift in multiples of word-size */
    const int nbits_pr_word = (BigIntWordSize * 8);
    int nwords = nbits / nbits_pr_word;
    if (nwords != 0) {
        _rshift_word(NumWords, B, nwords);
        nbits -= (nwords * nbits_pr_word);
    }

    if (nbits != 0) {
        size_t i;
        for (i = 0; i < (NumWords - 1); ++i) {
            B[i] = (B[i] >> nbits) | (B[i + 1] << ((8 * BigIntWordSize) - nbits));
        }
        B[i] >>= nbits;
    }
}

void BigInt_mod(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
    /* Take divmod and throw away div part */
	assert(NumWords <= MaxBigIntWords);
    BigInt_t tmp[MaxBigIntWords];
    BigInt_divmod(NumWords, A, B, tmp, Out);
}

void BigInt_divmod(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * C, BigInt_t * D)
{
	assert(NumWords <= MaxBigIntWords);
    BigInt_t tmp[MaxBigIntWords];

    /* Out = (A / B) */
    BigInt_div(NumWords, A, B, C);

    /* tmp = (Out * B) */
    BigInt_mul(NumWords, C, NumWords, B, NumWords, tmp);

    /* Out = A - tmp */
    BigInt_sub(NumWords, A, NumWords, tmp, NumWords, D);
}

void BigInt_and(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
    for (size_t i = 0; i < NumWords; ++i) {
        Out[i] = (A[i] & B[i]);
    }
}

void BigInt_or(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
    for (size_t i = 0; i < NumWords; ++i) {
        Out[i] = (A[i] | B[i]);
    }
}

void BigInt_xor(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
    for (size_t i = 0; i < NumWords; ++i) {
        Out[i] = (A[i] ^ B[i]);
    }
}

int BigInt_cmp(size_t NumWords, BigInt_t * A, BigInt_t * B)
{
    size_t i = NumWords;
    do {
        i -= 1; /* Decrement first, to start with last array element */
        if (A[i] > B[i]) {
            return LARGER;
        } else if (A[i] < B[i]) {
            return SMALLER;
        }
    } while (i != 0);

    return EQUAL;
}

int BigInt_is_zero(size_t NumWords, BigInt_t * BigInt)
{
    for (size_t i = 0; i < NumWords; ++i) {
        if (BigInt[i]) {
            return 0;
        }
    }

    return 1;
}

void BigInt_pow(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
    BigInt_zero(NumWords, Out);

    if (BigInt_cmp(NumWords, B, Out) == EQUAL) {
        /* Return 1 when exponent is 0 -- BigInt^0 = 1 */
        BigInt_inc(NumWords, Out);
    } else {
		assert(NumWords <= MaxBigIntWords);
        BigInt_t bcopy[MaxBigIntWords];
		BigInt_t tmp[MaxBigIntWords];
        BigInt_copy(NumWords, bcopy, B);

        /* Copy A -> tmp */
        BigInt_copy(NumWords, tmp, A);

        BigInt_dec(NumWords, bcopy);

        /* Begin summing products: */
        while (!BigInt_is_zero(NumWords, bcopy)) {
            /* Out = tmp * tmp */
            BigInt_mul(NumWords, tmp, NumWords, A, NumWords, Out);
            /* Decrement B by one */
            BigInt_dec(NumWords, bcopy);

            BigInt_copy(NumWords, tmp, Out);
        }

        /* Out = tmp */
        BigInt_copy(NumWords, Out, tmp);
    }
}

void BigInt_isqrt(size_t NumWords, BigInt_t * A, BigInt_t * B)
{
	assert(NumWords <= MaxBigIntWords);
    BigInt_t low[MaxBigIntWords];
    BigInt_t high[MaxBigIntWords];
    BigInt_t mid[MaxBigIntWords];
    BigInt_t tmp[MaxBigIntWords];

    BigInt_zero(NumWords, low);
    BigInt_copy(NumWords, high, A);
    BigInt_copy(NumWords, mid, high);
    BigInt_rshift(NumWords, mid, 1);
    BigInt_inc(NumWords, mid);

    while (BigInt_cmp(NumWords, high, low) > 0) {
        BigInt_mul(NumWords, mid, NumWords, mid, NumWords, tmp);
        if (BigInt_cmp(NumWords, tmp, A) > 0) {
            BigInt_copy(NumWords, high, mid);
            BigInt_dec(NumWords, high);
        } else {
            BigInt_copy(NumWords, low, mid);
        }
        BigInt_sub(NumWords, high, NumWords, low, NumWords, mid);
        _rshift_one_bit(NumWords, mid);
        BigInt_add(NumWords, low, NumWords, mid, NumWords, mid);
        BigInt_inc(NumWords, mid);
    }
    BigInt_copy(NumWords, B, low);
}

void BigInt_copy(size_t NumWords, BigInt_t * Dst, BigInt_t * Src)
{
    for (size_t i = 0; i < NumWords; ++i) {
        Dst[i] = Src[i];
    }
}

void BigInt_copy_dif(size_t DstNumWords, BigInt_t * Dst, size_t SrcNumWords, BigInt_t * Src)
{
    size_t smallest = (DstNumWords < SrcNumWords) ? DstNumWords : SrcNumWords;
    size_t i;
    for (i = 0; i < smallest; ++i) Dst[i] = Src[i];
    for (; i < DstNumWords; ++i) Dst[i] = 0;
}

/* Private / Static functions. */
static void _rshift_word(size_t NumWords, BigInt_t * A, int nwords)
{
    size_t i;
    if (nwords >= NumWords) {
        for (i = 0; i < NumWords; ++i) {
            A[i] = 0;
        }
        return;
    }

    for (i = 0; i < NumWords - nwords; ++i) {
        A[i] = A[i + nwords];
    }
    for (; i < NumWords; ++i) {
        A[i] = 0;
    }
}

static void _lshift_word(size_t NumWords, BigInt_t * A, int nwords)
{
    int_fast32_t i;
    /* Shift whole words */
    for (i = (NumWords - 1); i >= nwords; --i) {
        A[i] = A[i - nwords];
    }
    /* Zero pad shifted words. */
    for (; i >= 0; --i) {
        A[i] = 0;
    }
}

static void _lshift_one_bit(size_t NumWords, BigInt_t * A)
{
    for (size_t i = (NumWords - 1); i > 0; --i) {
        A[i] = (A[i] << 1) | (A[i - 1] >> ((8 * BigIntWordSize) - 1));
    }
    A[0] <<= 1;
}

static void _rshift_one_bit(size_t NumWords, BigInt_t * A)
{
    for (size_t i = 0; i < (NumWords - 1); ++i) {
        A[i] = (A[i] >> 1) | (A[i + 1] << ((8 * BigIntWordSize) - 1));
    }
    A[NumWords - 1] >>= 1;
}
