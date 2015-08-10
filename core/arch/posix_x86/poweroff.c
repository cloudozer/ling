#include "ling_common.h"

extern void exit(int) __attribute__((noreturn));

void domain_poweroff(int status)
{
	exit(status);
}