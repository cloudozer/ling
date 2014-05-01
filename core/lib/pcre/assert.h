#pragma once

//MK
//
// This file hijacks assert.h declarations to avoid importing the whole libc as
// the standard assert() implementation references fprintf().
//

void printk(const char *fmt, ...) __attribute__ ((format (printf, 1, 2)));

void fatal_error(const char *fmt, ...) __attribute__ ((format (printf, 1, 2))) __attribute__ ((noreturn));

#ifndef SUPPRESS_ASSERTS
#define assert(x) \
	do { \
		if (!(x)) { \
			fatal_error("assertion %s failed at %s:%d\n", #x, __FILE__, __LINE__); \
		} \
	} while (0)
#else
#define assert(x)
#endif

//EOF
