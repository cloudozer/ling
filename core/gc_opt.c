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

#define GC_LIMIT_PASSED		1
#define GC_COHORT_LIMIT		4

// Tunable parameters
int gc_model_size_multiplier = 2;
int gc_model_yield_up = 1;
int gc_model_yield_down = 4;
int gc_model_wait_up = 1;
int gc_model_wait_down = 4;

static void collect(heap_t *hp, region_t *root_regs, int nr_regs);
static void collect_cohort(heap_t *hp, int ch, region_t *root_regs, int nr_regs);

int cohort_colls[GC_COHORTS] = { 0 };

void gc_opt_init(void)
{
}

int gc_skip_idle(heap_t *hp)
{
	return (hp->gc_yield_runs == 0);
}

void gc_hook(int gc_loc, term_t pid, heap_t *hp, region_t *root_regs, int nr_regs)
{
	if (gc_loc == GC_LOC_TEST_HEAP)
		collect(hp, root_regs, nr_regs);	// may this tunable?
	else if (gc_loc == GC_LOC_PROC_YIELD)
	{
		hp->gc_yield_tally += gc_model_yield_up;
		while (hp->gc_yield_tally >= gc_model_yield_down)
		{
			collect(hp, root_regs, nr_regs);
			hp->gc_yield_tally -= gc_model_yield_down;
		}
	}
	else if (gc_loc == GC_LOC_PROC_WAIT)
	{
		hp->gc_wait_tally += gc_model_wait_up;
		while (hp->gc_wait_tally >= gc_model_wait_down)
		{
			collect(hp, root_regs, nr_regs);
			hp->gc_wait_tally -= gc_model_wait_down;
		}
	}
	else
	{
		assert(gc_loc == GC_LOC_IDLE);
		assert(hp->gc_yield_runs > 0);
		hp->gc_yield_runs--;
		collect(hp, root_regs, nr_regs);
	}
}

static void collect(heap_t *hp, region_t *root_regs, int nr_regs)
{
	if (hp->full_sweep_after != 0 && hp->sweep_after_count >= hp->full_sweep_after)
	{
		heap_gc_full_sweep_N(hp, root_regs, nr_regs);
		return;
	}

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
			(size0 * gc_model_size_multiplier > size1))
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

	cohort_colls[ch]++;

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
		printk("collect_cohort: no memory while collecting cohort %d, ignored\n", ch);
		
	for (int i = 0; i <= ch; i++)
		if (hp->gc_cohorts[i] == prev)
			hp->gc_cohorts[i] = *ref;
}

