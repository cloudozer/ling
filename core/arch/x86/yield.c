#include "ling_xen.h"

void yield(void)
{
	HYPERVISOR_sched_op(SCHEDOP_yield, 0);
}
