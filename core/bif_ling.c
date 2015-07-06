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

#include "bif_impl.h"

void gdb_break(void);

#ifdef EXP_LINC_LATENCY
void linc_display(void);
#endif // EXP_LINC_LATENCY

#ifdef EXP_LINC_LLSTAT
void llstat_restart(int ifidx);
void llstat_stop(void);
void llstat_display(void);
#endif // EXP_LINC_LLSTAT

#ifdef EXP_COUNT_IOPS
void print_iop_counters(void);
#endif // EXP_COUNT_IOPS

#ifdef EXP_RUNTIME_METRICS
void print_variant_code_sizes(void);
void print_loaded_module_sizes(void);
#endif // EXP_RUNTIME_METRICS

// Cloudozer's 2nd anniversary
void cloudozer2(void);

// defined in ling_main.c
#ifdef TRACE_HARNESS
extern uint32_t trace_mask;
#endif

void dump_netmap_state(int what);

term_t cbif_domain_name0(proc_t *proc, term_t *regs)
{
	return heap_strz(&proc->hp, my_domain_name);
}

term_t cbif_b1_0(proc_t *proc, term_t *regs)
{
#ifdef LING_DEBUG
	gdb_break();
#endif
	return A_OK;
}

term_t cbif_b2_0(proc_t *proc, term_t *regs)
{
#ifdef LING_DEBUG
	gdb_break();
#endif
	return A_OK;
}

term_t cbif_b3_0(proc_t *proc, term_t *regs)
{
#ifdef LING_DEBUG
	gdb_break();
#endif
	return A_OK;
}

term_t cbif_trace1(proc_t *proc, term_t *regs)
{
	term_t Mask = regs[0];
	if (!is_int(Mask))
		badarg(Mask);

#ifdef TRACE_HARNESS
	trace_mask = int_value(Mask);
	trace_module = noval;
#endif
	return A_OK;
}

term_t cbif_trace2(proc_t *proc, term_t *regs)
{
	// Cloudozer's 2nd anniversary -- remove in 2016
	term_t Mask = regs[0];
	term_t Mod = regs[1];
	if (!is_int(Mask))
		badarg(Mask);
	if (!is_atom(Mod))
		badarg(Mod);

#ifdef TRACE_HARNESS
	trace_mask = int_value(Mask);
	trace_module = Mod;
#endif
	return A_OK;
}

term_t cbif_profile1(proc_t *proc, term_t *regs)
{
	term_t Flag = regs[0];
	if (!is_bool(Flag))
		badarg(Flag);

#ifdef PROFILE_HARNESS
	if (Flag == A_TRUE)
		prof_restart();
	else
	{
		uint64_t now = monotonic_clock();
		prof_stop(now);
	}
#endif
	return A_OK;
}

term_t cbif_profile_display0(proc_t *proc, term_t *regs)
{
#ifdef PROFILE_HARNESS
	prof_print_summary();
#endif
	return A_OK;
}

term_t cbif_experimental2(proc_t *proc, term_t *regs)
{
	term_t What = regs[0];
	UNUSED term_t Arg = regs[1];
	if (!is_atom(What))
		badarg(What);
	
	switch (What)
	{
	// Cloudozer's 2nd anniversary -- remove in 2016
	case A_CLOUDOZER:
		if (Arg == tag_int(2))
			cloudozer2();
		break;
	// Cloudozer's 2nd anniversary
	case A_MODULE_SIZE:
#ifdef EXP_RUNTIME_METRICS
		print_loaded_module_sizes();
#endif // EXP_RUNTIME_METRICS
		break;
	case A_VARIANT_SIZE:
#ifdef EXP_RUNTIME_METRICS
		print_variant_code_sizes();
#endif // EXP_RUNTIME_METRICS
		break;
	case A_COUNT_IOPS:
#ifdef EXP_COUNT_IOPS
		print_iop_counters();
#endif // EXP_COUNT_IOPS
		break;
	case A_PROCESSING_DELAY:
#ifdef EXP_LINC_LATENCY
		if (Arg == A_HELP)
			printk("ping -Q 42 -q -n -c 25000 -f <ip>\n");
		else
			linc_display();
#endif // EXP_LINC_LATENCY
		break;
	case A_LLSTAT:
#ifdef EXP_LINC_LLSTAT
		if (is_int(Arg))
			llstat_restart(int_value(Arg));
		else if (Arg == A_STOP)
			llstat_stop();
		else
			llstat_display();
#endif // EXP_LINC_LLSTAT
		break;
	case A_NETMAP:
		if (Arg == A_TX)
			dump_netmap_state(0);
		else if (Arg == A_RX)
			dump_netmap_state(1);
		break;
	case A_GC:
		if (Arg == tag_int(1))
			printk("Pages left: %d\n", mm_alloc_left());
		else if (is_short_pid(Arg))
		{
			proc_t *target = scheduler_lookup(Arg);
			assert(target != 0);	
		
			heap_t *hp = &target->hp;
			memnode_t *node = hp->nodes;
			int ch = 0;
			int size = 0;
			printk("ch");
			while (1)
			{
				while (ch < GC_COHORTS && node == hp->gc_cohorts[ch])
				{
					printk(":%d", size);
					ch++;
					size = 0;
				}
				if (node == 0)
					break;
				size++;
				node = node->next;
			}
			printk(":%d\n", size);
		}
		break;
	default:
		badarg(What);
	}

	return A_OK;
}

term_t cbif_stats0(proc_t *proc, term_t *regs)
{
	stats_display();
	return A_OK;
}

term_t cbif_xenstore_read1(proc_t *proc, term_t *regs)
{
	term_t Key = regs[0];
	if (!is_list(Key) && !is_boxed_binary(Key))
		badarg(Key);
	int key_size = iolist_size(Key);
	if (key_size < 0)
		badarg(Key);
	uint8_t buf[key_size +1];
	iolist_flatten(Key, buf);
	buf[key_size] = 0;

	char val[XENSTORE_RING_SIZE];
	int ok = xenstore_read((char *)buf, val, sizeof(val));
	if (ok != 0)
		return tag_int(ok);

	return heap_strz(&proc->hp, val);	
}

term_t cbif_xenstore_write2(proc_t *proc, term_t *regs)
{
	term_t Key = regs[0];
	term_t Value = regs[1];
	if (!is_list(Key) && !is_boxed_binary(Key))
		badarg(Key);
	if (!is_list(Value) && !is_boxed_binary(Value))
		badarg(Value);
	int key_size = iolist_size(Key);
	if (key_size < 0)
		badarg(Key);
	int val_size = iolist_size(Value);
	if (val_size < 0)
		badarg(Value);
	uint8_t key_buf[key_size+1];
	iolist_flatten(Key, key_buf);
	key_buf[key_size] = 0;
	uint8_t val_buf[val_size+1];
	iolist_flatten(Value, val_buf);
	val_buf[val_size] = 0;

	int ok = xenstore_write((char *)key_buf, (char *)val_buf);
	//printk("xenstore_write(\"%s\", \"%s\");\n", key_buf, val_buf);
	if (ok != 0)
		return int_value(ok);

	return A_OK;
}

term_t cbif_xenstore_list1(proc_t *proc, term_t *regs)
{
	term_t Key = regs[0];
	if (!is_list(Key) && !is_boxed_binary(Key))
		badarg(Key);
	int key_size = iolist_size(Key);
	if (key_size < 0)
		badarg(Key);
	uint8_t buf[key_size +1];
	iolist_flatten(Key, buf);
	buf[key_size] = 0;

	char val[XENSTORE_RING_SIZE];
	int ok = xenstore_read_dir((char *)buf, val, sizeof(val));
	if (ok != 0)
		return tag_int(ok);

	term_t dir = nil;
	char *p = val;
	while (*p != 0)
	{
		term_t name = heap_strz(&proc->hp, p);
		dir = heap_cons(&proc->hp, name, dir);
		p += strlen(p) +1;
	}

	return list_rev(dir, &proc->hp);
}

