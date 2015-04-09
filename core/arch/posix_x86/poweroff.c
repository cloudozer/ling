#include "ling_common.h"

extern void exit(int) __attribute__((noreturn));

void domain_poweroff(void)
{
	exit(0);
}
