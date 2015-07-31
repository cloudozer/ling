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
#include "netfe.h"
#include "disk.h"

#include "bignum.h"
#include "atoms.h"
#include "atom_defs.h"
#include "embed.h"
#include "code_base.h"
#include "proc.h"
#include "scheduler.h"
#include "time.h"
#include "ets.h"
#include "list_util.h"

#ifdef LING_XEN
#include "ling_xen.h"
#include "xenstore.h"
#include "event.h"
#include "grant.h"
#endif

// PRNG
#include "mtwist.h"

#define quote(x) #x
#define quote_and_expand(x) quote(x)

#ifdef LING_XEN
char stack[2*STACK_SIZE];
start_info_t start_info;
unsigned long *phys_to_machine_mapping;
shared_info_t *HYPERVISOR_shared_info;

void proc_main(proc_t *proc);

void lwip_init(void);
void pcre_init(void);
void counters_init(void);
void gc_opt_init(void);

UNUSED static void print_start_info(void);
UNUSED static void print_xenmem_info(void);
UNUSED static void print_xenstore_values(void);
UNUSED static void print_xs_tree(char *start);
#endif

void proc_main(proc_t *proc);

void time_init(void);

UNUSED void lwip_init(void);
void pcre_init(void);
void counters_init(void);

static void spawn_init_start(char *cmd_line);

// both domain and host name
char my_domain_name[256];

#ifdef LING_XEN
/* defined by startup.[sSc] calling conventions */
void start_ling(start_info_t *si)
#else
void start_ling(void)
#endif
{
	//-------- init phase 1 --------
	//
#ifdef LING_XEN
	memcpy(&start_info, si, sizeof(*si));

	phys_to_machine_mapping = (unsigned long *)start_info.mfn_list;

	HYPERVISOR_update_va_mapping((unsigned long)&shared_info,
		__pte(start_info.shared_info | 7), UVMF_INVLPG);
	HYPERVISOR_shared_info = &shared_info;
#endif

#ifndef LING_XEN
	mm_init();
#endif
	time_init();	// sets start_of_day_wall_clock

	//XXX mt_seed(start_of_day_wall_clock);
	mt_seed(0);
	
#ifdef LING_XEN
#if defined(__x86_64__)
	HYPERVISOR_set_callbacks(0, 0, 0);
#else /* __x86_64__ */
	HYPERVISOR_set_callbacks(0, 0, 0, 0);
#endif
	mm_init(start_info.nr_pages, start_info.pt_base, start_info.nr_pt_frames);

	// LING does not support more than 4G of memory.
	// Postpone the check until the console is available.
	nalloc_init();

	events_init();
	grants_init();

	console_init(mfn_to_virt(start_info.console.domU.mfn),
			start_info.console.domU.evtchn);

	xenstore_init(mfn_to_virt(start_info.store_mfn),
		start_info.store_evtchn);

	xenstore_read("name", my_domain_name, sizeof(my_domain_name));
	//print_xenstore_values();

	if (start_info.nr_pages > 1024*1024)
		fatal_error("LING does not support domains larger than 4Gb");

	if (disk_vbd_is_present())
		disk_init();

	lwip_init();
	netfe_init();
#else
	console_init();
	nalloc_init();
#endif

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
#if LING_XEN
    //can it be used outside Xen?
	gc_opt_init();
#endif

	//print_start_info();
	//print_xenmem_info();
	//run_alloc_tests();
	//run_mm_tests();
	//print_xenstore_values();
	//run_bignum_tests();
	
	//printk("\r\nLing %s is here\r\n", quote_and_expand(LING_VER));

	proc_main(0); // preliminary run

#ifdef LING_XEN
	spawn_init_start((char *)start_info.cmd_line);
#else
    /*
	static char *hardcoded_command_line = "-eval \"lists:map(fun application:start/1, [crypto,asn1,public_key,ssh,compiler,syntax_tools,xmerl,mnesia,lager,linc])\" -config /lincx/priv/sys.config -home /lincx -pz /lincx/apps/linc/ebin /lincx/apps/linc_max/ebin /lincx/apps/linc_us3/ebin /lincx/apps/linc_us4/ebin /lincx/deps/eenum/ebin /lincx/deps/enetconf/ebin /lincx/deps/lager/ebin /lincx/deps/meck/ebin /lincx/deps/of_config/ebin /lincx/deps/of_protocol/ebin /lincx/deps/pkt/ebin /lincx/priv";
	*/
	static char hardcoded_command_line[] = "-home /";
	spawn_init_start(hardcoded_command_line);
#endif

	//while (1)
	//	HYPERVISOR_sched_op(SCHEDOP_block, 0);

	/* UNREACHABLE */
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

extern void exit(int) __attribute__((noreturn));

void fatal_error(const char *fmt, ...)
{
	char buffer[BUFSIZ];

	va_list ap;
	va_start(ap, fmt);
	vsnprintf(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);
		
	printk("*** CRASH: %s\r\n", buffer);

#ifdef LING_POSIX
	exit(42);
#endif
	while (1)
	{
#ifdef LING_DEBUG
		// Provide for attaching the debugger to examine the crash
		gdb_break();
#endif
		yield();
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

#ifdef LING_XEN
static void print_start_info(void) {
	printk("start_info [%s]\r\n", start_info.magic);
	printk(".nr_pages = %lu\r\n", start_info.nr_pages);
	printk(".shared_info = 0x%lu\r\n", start_info.shared_info);
	printk(".flags = %d\r\n", start_info.flags);
	printk(".store_mfn = %d\r\n", (int)start_info.store_mfn);
	printk(".store_evtchn = %d\r\n", start_info.store_evtchn);
	printk(".console.domU.mfn = 0x%x\r\n", (int)start_info.console.domU.mfn);
	printk(".console.domU.evtchn = %d\r\n", start_info.console.domU.evtchn);
	printk(".pt_base = 0x%lx\r\n", start_info.pt_base);
	printk(".nr_pt_frames = %lu\r\n", start_info.nr_pt_frames);
	printk(".mfn_list = 0x%lx\r\n", start_info.mfn_list);
	printk(".mod_start = %lu\r\n", start_info.mod_start);
	printk(".mod_len = %lu\r\n", start_info.mod_len);
	printk(".cmd_line = [%s]\r\n", start_info.cmd_line);
	printk(".first_p2m_pfn = %lu\r\n", start_info.first_p2m_pfn);
	printk(".nr_p2m_frames = %lu\r\n", start_info.nr_p2m_frames);
}

static void print_xenmem_info(void) {
	domid_t domid = DOMID_SELF;
	printk("xenmem info:\r\n");
	
	int max_ram_page = HYPERVISOR_memory_op(XENMEM_maximum_ram_page, &domid);
	printk(".max_ram_page=%d\r\n", max_ram_page);
	int cur_res = HYPERVISOR_memory_op(XENMEM_current_reservation, &domid);
	printk(".cur_res=%d\r\n", cur_res);
	int max_res = HYPERVISOR_memory_op(XENMEM_maximum_reservation, &domid);
	printk(".max_res=%d\r\n", max_res);
}

static void print_xenstore_values(void)
{
	const char *keys[] = { "device/vif/0/mac",
						   "device/vif/0/handle",
						   "device/vif/0/protocol",
						   "device/vif/0/backend-id",
						   "device/vif/0/state",
						   "device/vif/0/backend",
						   0 };

	//const char *keys[] = { "domid",
	//					   "name",
	//					   "memory/target",
	//					   "nonexistent",
	//					   "cpu/0/availability",
	//					   0 };
	char value[256];

	printk("xenstore info:\r\n");
	const char **pkeys = keys;
	while (*pkeys)
	{
		const char *key = *pkeys++;
		int r = xenstore_read(key, value, sizeof(value));
		if (r == 0)
			printk(" [%s]=%s\r\n", key, value);
		else
			printk(" [%s] *** xenstore_read() returns %d\r\n", key, r);
	}
}

static int print_xs(int indent, char *key)
{
	char buf[1024];
	assert(indent < sizeof(buf));
	memset(buf, ' ', indent);
	buf[indent] = 0;
	printk("%s%s\n", buf, key);

	int st = xenstore_read_dir(key, buf, sizeof(buf));
	if (st < 0)
		return st;
	char *dir = buf;
	while (*dir != 0)
	{
		char path[1024];
		strcpy(path, key);
		strcat(path, "/");
		strcat(path, dir);

		int ret_code = print_xs(indent +4, path);
		if (ret_code < 0)
			return ret_code;
		dir += strlen(dir) +1;
	}
	return 0;
}

static void print_xs_tree(char *start)
{
	int ret_code = print_xs(0, start);
	if (ret_code < 0)
		fatal_error("print_xs_tree failed: %d", ret_code);
}
#endif /* LING_XEN */

/*EOF*/

