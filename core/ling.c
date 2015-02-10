// Copyright (c) 2013-2014 Cloudozer LLP. All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
// 
// * Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// 
// * Redistributions in any form must be accompanied by information on how to
// obtain complete source code for the LING software and any accompanying
// software that uses the LING software. The source code must either be included
// in the distribution or be available for no more than the cost of distribution
// plus a nominal fee, and must be freely redistributable under reasonable
// conditions.  For an executable file, complete source code means the source
// code for all modules it contains. It does not include source code for modules
// or files that typically accompany the major components of the operating
// system on which the executable file runs.
// 
// THIS SOFTWARE IS PROVIDED BY CLOUDOZER LLP ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE
// DISCLAIMED. IN NO EVENT SHALL CLOUDOZER LLP BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "ling_common.h"

#include <stdarg.h>
#include <stddef.h>

#include <stdlib.h>

#include "string.h"
#include "snprintf.h"

#include "mm.h"
#include "nalloc.h"

#include "console.h"
#include "netif.h"

#include "bignum.h"
#include "atoms.h"
#include "atom_defs.h"
#include "embed.h"
#include "code_base.h"
#include "proc.h"
#include "scheduler.h"
#include "ets.h"
#include "list_util.h"

// PRNG
#include "mtwist.h"

#define quote(x) #x
#define quote_and_expand(x) quote(x)

// capabilities of the ARM processor - how to set this?
size_t __hwcap = 0;

void proc_main(proc_t *proc);

void time_init(void);

UNUSED void lwip_init(void);
void pcre_init(void);
void counters_init(void);

static void spawn_init_start(char *cmd_line);

// both domain and host name
char my_domain_name[256];

void start_ling(void)
{
	//-------- init phase 1 --------
	//
	time_init();	// sets start_of_day_wall_clock

	// use the time value to seed PRNG
	mt_seed(start_of_day_wall_clock);
	
	nalloc_init();

	//lwip_init();

	//-------- init phase 2 --------
	//
	if (nalloc_no_memory())
		fatal_error("init phase 2: no memory"); 	

	sys_stats_init();

	atoms_init();
	embed_init();
	code_base_init();
	scheduler_init();
	ets_init();
	pcre_init();
	counters_init();

	//print_start_info();
	//print_xenmem_info();
	//run_alloc_tests();
	//run_mm_tests();
	//print_xenstore_values();
	//run_bignum_tests();
	
	//printk("\r\nLing %s is here\r\n", quote_and_expand(LING_VER));

	proc_main(0); // preliminary run

	static char *hardcoded_command_line = "-hello";
	spawn_init_start(hardcoded_command_line);

	//while (1)
	//	HYPERVISOR_sched_op(SCHEDOP_block, 0);

	/* UNREACHABLE */
}

void domain_poweroff(void)
{
	while (1);
}

void printk(const char *fmt, ...)
{
    char buffer[BUFSIZ];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buffer, sizeof(buffer), fmt, ap);
    va_end(ap);

	int len = strlen(buffer);

	if (console_is_initialized())
		console_write(buffer, len);
	if (ser_cons_present())
		ser_cons_write(buffer, len);
}

void fatal_error(const char *fmt, ...)
{
	char buffer[BUFSIZ];

	va_list ap;
	va_start(ap, fmt);
	vsnprintf(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);
		
	printk("*** CRASH: %s\r\n", buffer);

	while (1)
	{
#ifdef LING_DEBUG
		// Provide for attaching the debugger to examine the crash
		gdb_break();
#endif
	}
}

void __assert_fail(const char *assertion,
	const char *file, unsigned int line, const char * function)
{
	fatal_error("assertion %s failed at %s:%d (%s)", assertion, file, line, function);
}

static term_t parse_cmd_line(heap_t *hp, char *cmd_line)
{
	term_t args = nil;
	char *p = cmd_line;
	while (*p != 0)
	{
		while (*p == ' ')
			p++;
		char *token1, *token2;
		if (*p == '\'' || *p == '"')
		{
			int8_t quote = *p++;
			token1 = p;
			while (*p != 0 && *p != quote)
				p++;
			token2 = p;
			if (*p != 0)
				p++;
		}
		else
		{
			token1 = p;
			while (*p != 0 && *p != ' ')
				p++;
			token2 = p;
		}
		uint8_t *data;
		term_t bin = heap_make_bin(hp, token2 -token1, &data);
		memcpy(data, token1, token2 -token1);
		args = heap_cons(hp, bin, args);
	}

	return list_rev(args, hp);
}

static void spawn_init_start(char *cmd_line)
{
	proc_t *init_proc = proc_make(noval);	// group leader set later
	assert(init_proc != 0);

	init_proc->cap.regs[0] = parse_cmd_line(&init_proc->hp, cmd_line);
	assert(init_proc->cap.regs[0] != noval);
	init_proc->cap.live = 1;

	init_proc->init_call_mod = A_INIT;
	init_proc->init_call_func = A_BOOT;
	init_proc->init_call_arity = 1;

	export_t *exp = code_base_lookup(A_INIT, A_BOOT, 1);
	assert(exp != 0);

	init_proc->cap.ip = exp->entry;
	module_info_t *mi = code_base_module_by_name(A_INIT, 0);
	assert(mi != 0);
	init_proc->cap.cp = mi->code_starts + mi->code_size-1;

	scheduler_enlist0(init_proc);	// pid assigned
	init_proc->group_leader = init_proc->pid;	// init is its own leader

	proc_main(init_proc);

	/* UNREACHANBLE */
}

void abort(void)
{
	fatal_error("aborted");
}

/*EOF*/

