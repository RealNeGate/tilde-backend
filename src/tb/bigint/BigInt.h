#ifndef __BigInt_H__
#define __BigInt_H__

#include <stdint.h>

/* Size of 'digits' or 'words' used to represent big ints, you can set this to 1, 2, 4 or 8 */
#define BigIntWordSize 8

#ifndef BigIntWordSize
#error Must define BigIntWordSize to be 1, 2, 4
#elif (BigIntWordSize == 1)
/* Data type of array in structure */
typedef uint8_t BigInt_t;
/* Data-type larger than BigInt_t, for holding intermediate results of calculations */
typedef uint32_t BigInt_tmp_t;
#elif (BigIntWordSize == 2)
typedef uint16_t BigInt_t;
typedef uint32_t BigInt_tmp_t;
#elif (BigIntWordSize == 4)
typedef uint32_t BigInt_t;
typedef uint64_t BigInt_tmp_t;
#elif (BigIntWordSize == 8)
typedef uint64_t BigInt_t;
typedef __uint128_t BigInt_tmp_t;
#endif

/* Tokens returned by BigInt_cmp() for value comparison */
enum { SMALLER = -1, EQUAL = 0, LARGER = 1 };

/* In/out functions */
void BigInt_zero(size_t NumWords, BigInt_t * BigInt);
void BigInt_from_int(size_t NumWords, BigInt_t * BigInt, BigInt_tmp_t Integer);
int  BigInt_to_int(size_t NumWords, BigInt_t * BigInt);
void BigInt_from_string(size_t NumWords, BigInt_t * BigInt, char * Str); /* From decimal string */
void BigInt_from_hex_string(size_t NumWords, BigInt_t * BigInt, char * Str); /* From hex string */
void BigInt_to_hex_string(size_t NumWords, BigInt_t * BigInt, char * Str); /* To hex string */
void BigInt_copy(size_t NumWords, BigInt_t * dst, BigInt_t * src);
void BigInt_copy_dif(size_t DstNumWords, BigInt_t * Dst, size_t SrcNumWords, BigInt_t * Src); /* Copy different sized ones */

/* Basic arithmetic operations: */
void BigInt_add(size_t ANumWords, BigInt_t * A, size_t BNumWords, BigInt_t * B, size_t OutNumWords, BigInt_t * Out); /* Out = A + B */
void BigInt_sub(size_t ANumWords, BigInt_t * A, size_t BNumWords, BigInt_t * B, size_t OutNumWords, BigInt_t * Out); /* Out = A - B */
void BigInt_mul(size_t XWords, BigInt_t * X, size_t YWords, BigInt_t * Y, size_t OutWords, BigInt_t * Out); /* Karatsuba multiplication */
void BigInt_mul_basic(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out); /* Out = A * B, old method, faster for small numbers */
void BigInt_div(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out); /* Out = A / B */
void BigInt_mod(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out); /* Out = A % B */
void BigInt_divmod(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * C, BigInt_t * D); /* C = A/B, D = A%B */

/* Bitwise operations: */
void BigInt_and(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out); /* Out = A & B */
void BigInt_or(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out);  /* Out = A | B */
void BigInt_xor(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out); /* Out = A ^ B */
void BigInt_lshift(size_t NumWords, BigInt_t * B, int nbits); /* B = A << nbits */
void BigInt_rshift(size_t NumWords, BigInt_t * B, int nbits); /* B = A >> nbits */

/* Special operators and comparison */
int  BigInt_cmp(size_t NumWords, BigInt_t * A, BigInt_t * B); /* Compare: returns LARGER, EQUAL or SMALLER */
int  BigInt_is_zero(size_t NumWords, BigInt_t * BigInt); /* For comparison with zero */
void BigInt_inc(size_t NumWords, BigInt_t * BigInt); /* Increment: add one to BigInt */
void BigInt_dec(size_t NumWords, BigInt_t * BigInt); /* Decrement: subtract one from BigInt */
int  BigInt_bextr(size_t NumWords, BigInt_t * BigInt, int Bit); /* Read bit:  grabs a bit from any point in the BigInt */
void BigInt_pow(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out); /* Calculate A^B -- e.g. 2^10 => 1024 */
void BigInt_isqrt(size_t NumWords, BigInt_t * A, BigInt_t * B); /* Integer square root -- e.g. isqrt(5) => 2*/
size_t BigInt_truncate(size_t BigIntWords, BigInt_t * BigInt); /* How many digits are actually needed */

#endif
