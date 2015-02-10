#ifndef LIBC_H
#define LIBC_H

#include <stdlib.h>
#include <limits.h>

#undef weak_alias
#define weak_alias(old, new) \
	extern __typeof(old) new __attribute__((weak, alias(#old)))

#endif
