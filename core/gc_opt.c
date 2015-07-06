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

#include "heap.h"

#include "ling_common.h"

#include "string.h"
#include "assert.h"
#include "proc.h"

// When a cohort size reaches GC_COHORT_SIZE, the GC_LIMIT_PASSED flag is set.
// This enables GC for the cohort until its size drop to 0.
#define GC_LIMIT_PASSED			1
#define GC_COHORT_LIMIT			4

// A cohort size time GC_SIZE_MULTIPLIER must exceed the size of the next cohort,
// for the smaller cohort to be eligible for GC.
#define GC_SIZE_MULTIPLIER		2

// A number of reductions between two GC runs
#define GC_MIN_REDS				6000

// A full-sweep GC may happen when the scheduler is IDLE and all data is old.

static void collect(heap_t *hp, region_t *root_regs, int nr_regs);
static void collect_cohort(heap_t *hp, int ch, region_t *root_regs, int nr_regs);

void gc_opt_init(void)
{
}

int gc_skip_idle(heap_t *hp)
{
	return (hp->gc_yield_runs == 0);
}

void gc_hook(int gc_loc, proc_t *proc, region_t *root_regs, int nr_regs)
{
	heap_t *hp = &proc->hp;

	if (gc_loc == GC_LOC_IDLE)
	{
		if (hp->full_sweep_after != 0 &&
			hp->gc_cohorts[GC_COHORTS-1] == hp->nodes && 	// all nodes are old
			hp->sweep_after_count >= hp->full_sweep_after)
		{
			heap_gc_full_sweep_N(hp, root_regs, nr_regs);
			return;
		}

		assert(hp->gc_yield_runs > 0);
		hp->gc_yield_runs--;
		collect(hp, root_regs, nr_regs);
	}
	else
	{
		uint32_t new_reds = proc->total_reds - hp->gc_last_reds;
		if (new_reds < GC_MIN_REDS)
			return;
		hp->gc_last_reds = proc->total_reds;
		collect(hp, root_regs, nr_regs);
	}
}

static void collect(heap_t *hp, region_t *root_regs, int nr_regs)
{
	memnode_t *node = hp->nodes;
	int size0 = 0;
	while (node != hp->gc_cohorts[0])
	{
		size0++;
		node = node->next;
	}

	int nr_collectable = size0;

	int ch = 0;
	while (ch < GC_COHORTS)
	{
		int size1 = 0;
		memnode_t *limit = (ch < GC_COHORTS-1) ?hp->gc_cohorts[ch+1] :0;
		while (node != limit)
		{
			size1++;
			node = node->next;
		}

		if (size0 == 0)
			hp->gc_flags[ch] &= ~GC_LIMIT_PASSED;
		else if (size0 >= GC_COHORT_LIMIT)
			hp->gc_flags[ch] |= GC_LIMIT_PASSED;

		if ((hp->gc_flags[ch] & GC_LIMIT_PASSED) &&
			(size0 * GC_SIZE_MULTIPLIER > size1))
			break;
		else
		{
			nr_collectable += size1;

			size0 = size1;
			ch++;
		}
	}

	// limit the number of yield runs to the number of collectable nodes
	hp->gc_yield_runs = nr_collectable;

	if (ch >= GC_COHORTS)
		return;		// cohorts lined up perfectly

	collect_cohort(hp, ch, root_regs, nr_regs);
}

static void collect_cohort(heap_t *hp, int ch, region_t *root_regs, int nr_regs)
{
	memnode_t *top = hp->gc_cohorts[ch];
	memnode_t *prev = (ch == 0) ?hp->nodes :hp->gc_cohorts[ch-1];
	if (prev == top)
		return;	// the previous cohort is empty
	while (prev->next != top)
		prev = prev->next;

	memnode_t **ref = &hp->nodes;
	while (*ref != prev)
		ref = &(*ref)->next;

	hp->gc_cohorts[ch] = prev; // may become invalid; fix after GC

	int ok = heap_gc_generational_N(hp, prev, root_regs, nr_regs);
	if (ok < 0)
		printk("GC:collect_cohort: no memory while collecting cohort %d, ignored\n", ch);
		
	for (int i = 0; i <= ch; i++)
		if (hp->gc_cohorts[i] == prev)
			hp->gc_cohorts[i] = *ref;
}

