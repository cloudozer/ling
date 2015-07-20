#ifndef __LING_CORE_LASSERT_H__
#define __LING_CORE_LASSERT_H__

void __assert_fail(const char *assertion,
	const char *file, unsigned int line, const char * function) __attribute__ ((noreturn));

#ifndef SUPPRESS_ASSERTS
#define assert(x) \
	do { \
		if (!(x)) { \
			__assert_fail(#x, __FILE__, __LINE__, __FUNCTION__); \
		} \
	} while (0)
#else
#define assert(x)
#endif

#endif //__LING_CORE_LASSERT_H__
