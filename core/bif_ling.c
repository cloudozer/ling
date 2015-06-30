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

extern int gc_model_size_multiplier;
extern int gc_model_yield_up;
extern int gc_model_yield_down;
extern int gc_model_wait_up;
extern int gc_model_wait_down;

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

void collect_gc_times_and_efficiency(proc_t *proc)
{
	static int done = 0;
	if (done) return;
	done = 1;

	int nr_regs = proc_count_root_regs(proc);
	assert(nr_regs <= MAX_ROOT_REGS);
	region_t root_regs[nr_regs];
	proc_fill_root_regs(proc, root_regs, 0, 0);
	heap_t *hp = &proc->hp;

	int nr_nodes = 0;
	memnode_t *gc_nodes[1000];
	memnode_t *node = hp->nodes;
	while (nr_nodes < 1000)
	{
		assert(node != 0);
		gc_nodes[nr_nodes++] = node;
		assert(node->next != 0);
		node = node->next->next;	// skip one
	}

	for (int i = nr_nodes-1; i >= 0; i--)	// backwards
	{
		uint32_t saved_size = hp->total_size;
		uint64_t started_ns = monotonic_clock();

		int ok = heap_gc_generational_N(hp, gc_nodes[i], root_regs, nr_regs);
		assert(ok == 0);

		uint64_t elapsed_ns = monotonic_clock() - started_ns;
		uint32_t reclaimed = saved_size - hp->total_size;

		printk("%d:%d:%d:%lu\n", i, gc_nodes[i]->index, reclaimed, elapsed_ns);
	}
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
		else if (Arg == tag_int(2))
		{
			int nr_nodes = 0;
			int total_pages = 0;
			memnode_t *node = proc->hp.nodes;
			while (node != 0)
			{
				nr_nodes++;
				total_pages += node->index;
				node = node->next;
			}
			printk("Heap: %d nodes %d pages %d total_size\n", nr_nodes, total_pages, proc->hp.total_size);
		}
		else if (Arg == tag_int(10))
		{
			// dump cohort sizes
			heap_t *hp = &proc->hp;
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
		else if (Arg == tag_int(13))
		{
			int nr_nodes = 0;
			memnode_t *node = proc->hp.nodes;
			while (node != 0)
			{
				nr_nodes++;
				node = node->next;
			}

			if (nr_nodes >= 2000)
				collect_gc_times_and_efficiency(proc);
		}
		else if (is_tuple(Arg))
		{
			term_t *elts = peel_tuple(Arg);
			assert(elts[0] == 5);
			assert(is_int(elts[1]));
			assert(is_int(elts[2]));
			assert(is_int(elts[3]));
			assert(is_int(elts[4]));
			assert(is_int(elts[5]));
			gc_model_size_multiplier = int_value(elts[1]);
			gc_model_yield_up = int_value(elts[2]); 
			gc_model_yield_down = int_value(elts[3]);
			gc_model_wait_up = int_value(elts[4]);
			gc_model_wait_down = int_value(elts[5]);
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

