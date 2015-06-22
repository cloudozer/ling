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

#define VGC_RETIRE_AFTER	50
#define VGC_GC_HYSTER		512

//static uint32_t fast_rand(void);

static int will_merge(memnode_t *cur)
{
	if (cur->index == 0)
		return 0;	// init_node is unmergeable

	int cur_size = cur->starts - (uint32_t *)NODE_THRESHOLD(cur);
	int next_slack = (cur->next != 0 && cur->next->index != 0)	// !init_node
			?cur->next->ends - cur->next->starts
			:0;
	return (cur_size <= next_slack);
}

static int collect_mergeable(heap_t *hp, region_t *root_regs, int nr_regs)
{
	memnode_t *cur = hp->nodes;
	while (cur != 0 && !will_merge(cur))
		cur = cur->next;

	if (cur == 0)
		return 0;

	if (hp->gc_scythe == cur)
		hp->gc_scythe = hp->gc_scythe->next;	// make sure gc_scythe is valid

	//printk("GC: merging: cur 0x%08x\n", cur);

	return heap_gc_generational_N(hp, cur, root_regs, nr_regs);
}

static inline int will_expand(memnode_t *node, int needed)
{
	return (node->ends - node->starts < needed);
}

// proc_burn_fat0()
void gc_hook0(heap_t *hp, region_t *root_regs, int nr_regs)
{
	// select the youngestest node
	memnode_t *youngest = hp->nodes;
	if (youngest != 0)
	{
		if (hp->gc_scythe == youngest)
			hp->gc_scythe = hp->gc_scythe->next;	// make sure gc_scythe is valid

		int ok = heap_gc_generational_N(hp, youngest, root_regs, nr_regs);
		if (ok < 0)
			printk("GC:generational: not enough memory for GC run, ignored\n");
	}

	if (++hp->retire_count >= VGC_RETIRE_AFTER)
	{
		hp->retire_count = 0;
		heap_retire(hp, root_regs, nr_regs);
	}
}

// heap_ensure()
int gc_hook1(heap_t *hp, int needed, region_t *root_regs, int nr_regs)
{
	assert(needed > 0);
	needed += VGC_GC_HYSTER;	// avoid degenerate GC of the youngest node

	// select the youngestest node
	memnode_t *youngest = hp->nodes;
	if (youngest != 0)
	{
		if (hp->gc_scythe == youngest)
			hp->gc_scythe = hp->gc_scythe->next;	// make sure gc_scythe is valid

		int ok = heap_gc_generational_N(hp, youngest, root_regs, nr_regs);
		if (ok < 0)
			printk("GC:generational: not enough memory for GC run, ignored\n");
	}

	if (will_expand(hp->nodes, needed))
	{
		// collect the second youngest too
		if (hp->nodes != 0 && hp->nodes->next != 0)
		{
			memnode_t *young = hp->nodes->next;
			if (hp->gc_scythe == young)
				hp->gc_scythe = hp->gc_scythe->next;	// make sure gc_scythe is valid

			int ok = heap_gc_generational_N(hp, young, root_regs, nr_regs);
			if (ok < 0)
				printk("GC:generational: not enough memory for GC run [2], ignored\n");
		}
	
		collect_mergeable(hp, root_regs, nr_regs);
	}

	return needed;
}

// heap_retire()
void gc_hook2(heap_t *hp, region_t *root_regs, int nr_regs)
{
	if (hp->full_sweep_after != 0 && hp->sweep_after_count >= hp->full_sweep_after)
	{
		heap_gc_full_sweep_N(hp, root_regs, nr_regs);
		return;
	}

	if (hp->gc_scythe == 0)
		hp->gc_scythe = hp->nodes;

	// gc may destroy gc_scythe; save its successor
	memnode_t *new_scythe = hp->gc_scythe->next;

	//printk("GC:retire: heap 0x%08x node 0x%08x pages %d\n", hp, hp->gc_scythe, hp->gc_scythe->index);

	int ok = heap_gc_generational_N(hp, hp->gc_scythe, root_regs, nr_regs);
	if (ok == 0)
		hp->gc_scythe = new_scythe;
}

//static uint32_t fast_rand(void)
//{
//	static uint32_t x = 123456789;
//	static uint32_t y = 362436069;
//	static uint32_t z = 521288629;
//
//	x ^= x << 16;
//	x ^= x >> 5;
//	x ^= x << 1;
//
//	uint32_t t = x;
//	x = y;
//	y = z;
//	z = t ^ x ^ y;
//
//	return z;
//}

