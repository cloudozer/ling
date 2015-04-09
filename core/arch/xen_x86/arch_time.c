//
//
//

/* copied from mini-os */

#include <stdint.h>

/*
 * Scale a 64-bit delta by scaling and multiplying by a 32-bit fraction,
 * yielding a 64-bit result.
 */
uint64_t scale_delta(uint64_t delta, uint32_t mul_frac, int shift)
{
	uint64_t product;
#ifdef __i386__
	uint32_t tmp1, tmp2;
#endif

	if ( shift < 0 )
		delta >>= -shift;
	else
		delta <<= shift;

#ifdef __i386__
	__asm__ (
		"mul  %5       ; "
		"mov  %4,%%eax ; "
		"mov  %%edx,%4 ; "
		"mul  %5       ; "
		"add  %4,%%eax ; "
		"xor  %5,%5    ; "
		"adc  %5,%%edx ; "
		: "=A" (product), "=r" (tmp1), "=r" (tmp2)
		: "a" ((uint32_t)delta), "1" ((uint32_t)(delta >> 32)), "2" (mul_frac) );
#else
	__asm__ (
		"mul %%rdx ; shrd $32,%%rdx,%%rax"
		: "=a" (product) : "0" (delta), "d" ((uint64_t)mul_frac) );
#endif

	return product;
}

//EOF
