#include "ling_common.h"
#include "ling_xen.h"
#include "arch_console.h"

void domain_poweroff(void)
{
	printk("\nBye\n");
	console_done();	// flushes and restores terminal mode

	struct sched_shutdown op;
	op.reason = SHUTDOWN_poweroff;
	HYPERVISOR_sched_op(SCHEDOP_shutdown, &op);
}
