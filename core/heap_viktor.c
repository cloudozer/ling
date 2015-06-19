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

#define VGC_DEEP_FACTOR		(0xc0000000)
#define VGC_RETIRE_AFTER	256

static uint32_t fast_rand(void);

uint32_t *heap_ensure(heap_t *hp, int needed, region_t *root_regs, int nr_regs)
{
	if (hp->nodes == hp->gc_old)
		return heap_alloc(hp, needed);	// no young nodes
	
	// select a node between nodes and gc_old-1
	memnode_t *gc_node = hp->nodes;
	while (gc_node->next != hp->gc_old)
	{
		if (fast_rand() < VGC_DEEP_FACTOR)
			break;
		gc_node = gc_node->next;
	}

	int ok = heap_gc_generational_N(hp, gc_node, root_regs, nr_regs);
	if (ok < 0)
		printk("GC:generational: not enough memory for GC run, ignored\n");

	if (++hp->retire_count >= VGC_RETIRE_AFTER)
	{
		hp->retire_count = 0;
		if (heap_retire(hp, root_regs, nr_regs) < 0)
			printk("GC:generational: not enough memory to retire, ignored\n");
	}

	return heap_alloc(hp, needed);	// no young nodes
}

int heap_retire(heap_t *hp, region_t *root_regs, int nr_regs)
{
	if (hp->full_sweep_after != 0 && hp->sweep_after_count >= hp->full_sweep_after)
		return heap_gc_full_sweep_N(hp, root_regs, nr_regs);

	if (hp->nodes == hp->gc_old)		// no young nodes
		hp->gc_old = hp->gc_tail;		// (re)start from the bottom

	if (hp->nodes == hp->gc_old)		// no young nodes, still
		return 0;

	memnode_t *gc_node = hp->nodes;
	while (gc_node->next != hp->gc_old)
		gc_node = gc_node->next;

	// gc may destroy gc_node; save its predecessor
	memnode_t *pred = hp->nodes;
	if (pred != gc_node)
		while (pred->next != gc_node)
			pred = pred->next;
	else
		pred = 0;

	//printk("GC:retire: heap 0x%08x node 0x%08x pages %d\n", hp, gc_node, gc_node->index);

	int ok = heap_gc_generational_N(hp, gc_node, root_regs, nr_regs);
	if (ok == 0)
		hp->gc_old = (pred == 0) ?hp->nodes
								 :pred->next;

	return ok;
}

static uint32_t fast_rand(void)
{
	static uint32_t x = 123456789;
	static uint32_t y = 362436069;
	static uint32_t z = 521288629;

	x ^= x << 16;
	x ^= x >> 5;
	x ^= x << 1;

	uint32_t t = x;
	x = y;
	y = z;
	z = t ^ x ^ y;

	return z;
}

