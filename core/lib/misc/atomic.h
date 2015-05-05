#ifndef _INTERNAL_ATOMIC_H
#define _INTERNAL_ATOMIC_H

#include <stdint.h>

#ifdef __arm__

//copied from musl sources
static inline int a_ctz_l(unsigned long x)
{
	static const char debruijn32[32] = {
		0, 1, 23, 2, 29, 24, 19, 3, 30, 27, 25, 11, 20, 8, 4, 13,
		31, 22, 28, 18, 26, 10, 7, 12, 21, 17, 9, 6, 16, 5, 15, 14
	};
	return debruijn32[(x&-x)*0x076be629 >> 27];
}

#else /* __arm__ */

static inline int a_ctz_64(uint64_t x)
{
	__asm__( "bsf %1,%0" : "=r"(x) : "r"(x) );
	return x;
}

static inline int a_ctz_l(unsigned long x)
{
	__asm__( "bsf %1,%0" : "=r"(x) : "r"(x) );
	return x;
}

static inline void a_and_64(volatile uint64_t *p, uint64_t v)
{
	__asm__( "lock ; and %1, %0"
			 : "=m"(*p) : "r"(v) : "memory" );
}

static inline void a_or_64(volatile uint64_t *p, uint64_t v)
{
	__asm__( "lock ; or %1, %0"
			 : "=m"(*p) : "r"(v) : "memory" );
}

static inline void a_or_l(volatile void *p, long v)
{
	__asm__( "lock ; or %1, %0"
		: "=m"(*(long *)p) : "r"(v) : "memory" );
}

static inline void *a_cas_p(volatile void *p, void *t, void *s)
{
	__asm__( "lock ; cmpxchg %3, %1"
		: "=a"(t), "=m"(*(long *)p) : "a"(t), "r"(s) : "memory" );
	return t;
}

static inline int a_cas(volatile int *p, int t, int s)
{
	__asm__( "lock ; cmpxchg %3, %1"
		: "=a"(t), "=m"(*p) : "a"(t), "r"(s) : "memory" );
	return t;
}

static inline void a_or(volatile void *p, int v)
{
	__asm__( "lock ; or %1, %0"
		: "=m"(*(int *)p) : "r"(v) : "memory" );
}

static inline void a_and(volatile void *p, int v)
{
	__asm__( "lock ; and %1, %0"
		: "=m"(*(int *)p) : "r"(v) : "memory" );
}

static inline int a_swap(volatile int *x, int v)
{
	__asm__( "xchg %0, %1" : "=r"(v), "=m"(*x) : "0"(v) : "memory" );
	return v;
}

static inline int a_fetch_add(volatile int *x, int v)
{
	__asm__( "lock ; xadd %0, %1" : "=r"(v), "=m"(*x) : "0"(v) : "memory" );
	return v;
}

static inline void a_inc(volatile int *x)
{
	__asm__( "lock ; incl %0" : "=m"(*x) : "m"(*x) : "memory" );
}

static inline void a_dec(volatile int *x)
{
	__asm__( "lock ; decl %0" : "=m"(*x) : "m"(*x) : "memory" );
}

static inline void a_store(volatile int *p, int x)
{
	__asm__( "mov %1, %0" : "=m"(*p) : "r"(x) : "memory" );
}

static inline void a_spin()
{
	__asm__ __volatile__( "pause" : : : "memory" );
}

static inline void a_barrier()
{
	__asm__ __volatile__( "" : : : "memory" );
}

static inline void a_crash()
{
	__asm__ __volatile__( "hlt" : : : "memory" );
}

#endif

#endif
