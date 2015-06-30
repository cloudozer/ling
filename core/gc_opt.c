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

//static uint32_t fast_rand(void);

static void do_action(int action, heap_t *hp, region_t *root_regs, int nr_regs);

struct gc_point_t {
	int up;
	int down;
};

struct gc_point_t gc_model[GC_NR_LOCS][GC_NR_ACTS];

static int gc_loc_counters[GC_NR_LOCS] = { 0 };
static int gc_act_counters[GC_NR_ACTS] = { 0 };

void gc_opt_init(void)
{
	memset(gc_model, 0, sizeof(gc_model));
	return;

	struct gc_point_t *pts = gc_model[GC_LOC_PROC_YIELD];
	pts[GC_ACT_YOUNGEST].up = 1;  pts[GC_ACT_YOUNGEST].down = 4;
	//pts[GC_ACT_YOUNGER].up = 1;  pts[GC_ACT_YOUNGER].down = 4;
	pts[GC_ACT_MERGEABLE].up = 1;  pts[GC_ACT_MERGEABLE].down = 32;
	pts[GC_ACT_SCYTHE].up = 1;    pts[GC_ACT_SCYTHE].down = 64;

	pts = gc_model[GC_LOC_PROC_WAIT];
	pts[GC_ACT_YOUNGEST].up = 1;  pts[GC_ACT_YOUNGEST].down = 8;
	//pts[GC_ACT_YOUNGER].up = 1;  pts[GC_ACT_YOUNGER].down = 4;
	pts[GC_ACT_MERGEABLE].up = 1;  pts[GC_ACT_MERGEABLE].down = 64;
	pts[GC_ACT_SCYTHE].up = 1;    pts[GC_ACT_SCYTHE].down = 20;

	pts = gc_model[GC_LOC_TEST_HEAP];
	pts[GC_ACT_YOUNGEST].up = 1;  pts[GC_ACT_YOUNGEST].down = 1;
	pts[GC_ACT_YOUNGER].up = 1;   pts[GC_ACT_YOUNGER].down = 1;
	pts[GC_ACT_MERGEABLE].up = 1; pts[GC_ACT_MERGEABLE].down = 1;

	pts = gc_model[GC_LOC_IDLE];
	pts[GC_ACT_SCYTHE].up = 1;    pts[GC_ACT_SCYTHE].down = 1;
}

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

void gc_hook(int gc_loc, term_t pid, heap_t *hp, region_t *root_regs, int nr_regs)
{
	gc_loc_counters[gc_loc]++;
	for (int action = 0; action < GC_NR_ACTS; action++)
	{
		struct gc_point_t *pnt = &gc_model[gc_loc][action];
		if (pnt->up > 0)
		{
			int *counter = &hp->gc_counters[gc_loc][action];
			*counter += pnt->up;
			if (*counter >= pnt->down)
			{
				gc_act_counters[action]++;
				do_action(action, hp, root_regs, nr_regs);
				*counter -= pnt->down;
			}
		}
	}
}

static void do_action(int action, heap_t *hp, region_t *root_regs, int nr_regs)
{
	switch (action)
	{
	case GC_ACT_YOUNGEST:
	{
		memnode_t *youngest = hp->nodes;
		if (youngest != 0)
		{
			if (hp->gc_scythe == youngest)
				hp->gc_scythe = hp->gc_scythe->next;	// make sure gc_scythe is valid

			int ok = heap_gc_generational_N(hp, youngest, root_regs, nr_regs);
			if (ok < 0)
				printk("GC:generational: not enough memory for GC run [0], ignored\n");
		}
		break;
	}
	case GC_ACT_YOUNGER:
	{
		if (hp->nodes != 0 && hp->nodes->next != 0)
		{
			memnode_t *young = hp->nodes->next;
			if (hp->gc_scythe == young)
				hp->gc_scythe = hp->gc_scythe->next;	// make sure gc_scythe is valid

			int ok = heap_gc_generational_N(hp, young, root_regs, nr_regs);
			if (ok < 0)
				printk("GC:generational: not enough memory for GC run [1], ignored\n");
		}
		break;
	}
	case GC_ACT_MERGEABLE:
	{
		collect_mergeable(hp, root_regs, nr_regs);
		break;
	}
	default:
		assert(action == GC_ACT_SCYTHE);

		if (hp->gc_scythe == 0)
			hp->gc_scythe = hp->nodes;

		// gc may destroy gc_scythe; save its successor
		memnode_t *new_scythe = hp->gc_scythe->next;

		//printk("GC:scythe: heap 0x%08x node 0x%08x pages %d\n", hp, hp->gc_scythe, hp->gc_scythe->index);

		int ok = heap_gc_generational_N(hp, hp->gc_scythe, root_regs, nr_regs);
		if (ok < 0)
			printk("GC:generational: not enough memory for GC run [2], ignored\n");
		else
			hp->gc_scythe = new_scythe;

		break;
	}
}

void dump_gc_counters(void)
{
	printk("GC_LOC_IDLE: %d\n", gc_loc_counters[GC_LOC_IDLE]);
	printk("GC_LOC_PROC_YIELD: %d\n", gc_loc_counters[GC_LOC_PROC_YIELD]);
	printk("GC_LOC_PROC_WAIT: %d\n", gc_loc_counters[GC_LOC_PROC_WAIT]);
	printk("GC_LOC_TEST_HEAP: %d\n", gc_loc_counters[GC_LOC_TEST_HEAP]);
	printk("GC_ACT_YOUNGEST: %d\n", gc_act_counters[GC_ACT_YOUNGEST]);
	printk("GC_ACT_YOUNGER: %d\n", gc_act_counters[GC_ACT_YOUNGER]);
	printk("GC_ACT_MERGEABLE: %d\n", gc_act_counters[GC_ACT_MERGEABLE]);
	printk("GC_ACT_SCYTHE: %d\n", gc_act_counters[GC_ACT_SCYTHE]);
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

