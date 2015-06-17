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

#include "math.h"
#include "mtwist.h"
#include "mm.h"
#include "time.h"
#include "stack.h"
#include "assert.h"

#define QTAB_SIZE	(2 << 14)
#define ACTIONS		5

#define GC_ALPHA	0.2
#define GC_GAMMA	0.95

#define GC_YINDEX	16

static struct q_table_t {
	// 0: skip
	// 1: GC0
	// 2: GC1
	// 3: GC2
	// 4: GC3
	double a[ACTIONS];
	int visits;
} q_table[QTAB_SIZE] = { { { 0.0 }, 0 } };

static int state_index(uint64_t now, int free_pages, region_t *root_regs, int nr_regs, heap_t *hp);
static double calc_reward(int no_memory, uint32_t free_pages,
		uint32_t reclaimed, uint32_t recl_pages, uint64_t elapsed_ns);
static int s_free_pages(int free_pages);
static int s_since_last_gc(uint64_t now, heap_t *hp);
static int s_last_reclaimed(heap_t *hp);
static int s_last_recl_pages(heap_t *hp);
static int s_gc_count(int cnt);
static int s_root_count(region_t *root_regs, int nr_regs);

int adapt_gc_random = 0;

uint32_t *heap_ensure_adaptive(heap_t *hp, int needed, region_t *root_regs, int nr_regs)
{
	// full sweep?
	if (hp->sweep_after_count >= hp->full_sweep_after)
	{
		if (heap_gc_full_sweep_N(hp, root_regs, nr_regs) < 0)
		{
			printk("heap_ensure: no memory while doing full-sweep GC, ignored\n");
		}

		return heap_alloc(hp, needed);
	}

	uint64_t now = monotonic_clock();
	int free_pages = mm_alloc_left() + nalloc_freed_pages();

	int index0 = state_index(now, free_pages, root_regs, nr_regs, hp);
	assert(index0 < QTAB_SIZE);

	q_table[index0].visits++;

//	double e[ACTIONS];
//	double se = 0.0;
//	for (int i = 0; i < ACTIONS; i++)
//	{
//		e[i] = exp(q_table[index0].a[i]);
//		assert(e[i] == e[i]);	// !inf()
//		se += e[i];
//	}

	uint32_t roll = mt_lrand();
	int action;
	if (adapt_gc_random || roll < (0x100000000l / 10))	// 10%
	{
		// pick random
		action = roll % ACTIONS;
	}
	else
	{
		// choose best
		double *qq = q_table[index0].a;
		action = 0;
		for (int i = 1; i < ACTIONS; i++)
			if (qq[i] > qq[action])
				action = i;
	}

	hp->gc1_count++;
	hp->gc2_count++;
	hp->gc3_count++;

	double reward = 0.0;

	// apply action
	switch (action) {
	case 0:	// skip
		break;

	case 1: // GC0
	case 2:	// GC1
	case 3: // GC2
	case 4:	// GC3
	{
		int pos = action -1;
		hp->gc_spot = hp->nodes;
		while (pos-- > 0 && hp->gc_spot != 0)
			hp->gc_spot = hp->gc_spot->next;

		// action prohibited
		if (hp->gc_spot == 0 || hp->gc_spot->index > GC_YINDEX)
			break;	// skip
		
		uint32_t saved_size = hp->total_size;
		uint32_t saved_alloc_pages = hp->total_alloc_pages;
		int ok = heap_gc_non_recursive_N(hp, root_regs, nr_regs);

		hp->last_reclaimed = saved_size - hp->total_size;
		// possible due to 'init_node'
		if (saved_alloc_pages >= hp->total_alloc_pages)
			hp->last_recl_pages = saved_alloc_pages - hp->total_alloc_pages;
		else
			hp->last_recl_pages = 0;
		hp->ts_last_gc = monotonic_clock();
		uint64_t elapsed_ns = hp->ts_last_gc - now;

		int no_memory = (ok < 0);
		reward = calc_reward(no_memory,
			free_pages, hp->last_reclaimed, hp->last_recl_pages, elapsed_ns);

		if (action == 2) hp->gc1_count = 0;
		else if (action == 3) hp->gc2_count = 0;
		else if (action == 4) hp->gc3_count = 0;

		break;
	}
	}

	int free_pages1 = mm_alloc_left() + nalloc_freed_pages();
	int index1 = state_index(hp->ts_last_gc, free_pages1, root_regs, nr_regs, hp);
	assert(index1 < QTAB_SIZE);

	double maxq = q_table[index1].a[0];
	for (int i = 1; i < ACTIONS; i++)
		if (q_table[index1].a[i] > maxq)
			maxq = q_table[index1].a[i];

	// learning
	double change = GC_ALPHA * (reward + GC_GAMMA * maxq - q_table[index0].a[action]);
	q_table[index0].a[action] += change;

	return heap_alloc(hp, needed);
}

static int state_index(uint64_t now, int free_pages, region_t *root_regs, int nr_regs, heap_t *hp)
{
	int s0_1 = s_free_pages(free_pages);
	int s1_2 = s_since_last_gc(now, hp);
	int s2_2 = s_last_reclaimed(hp);
	int s3_1 = s_last_recl_pages(hp);
	int s4_2 = s_gc_count(hp->gc1_count);
	int s5_2 = s_gc_count(hp->gc2_count);
	int s6_2 = s_gc_count(hp->gc3_count);
	int s7_2 = s_root_count(root_regs, nr_regs);

	return (s7_2 << 12) | (s6_2 << 10) | (s5_2 << 8) | (s4_2 << 6) |
		   (s3_1 <<  5) | (s2_2 <<  3) | (s1_2 << 1) | s0_1;
}

//a1
//#define K1	(1e6)
//#define K2	(2.5e-9)
//#define K3	(6e-1)

//a2
#define K1	(250000)
#define K2	(2.5e-9)
#define K3	(6e-1)

static double calc_reward(int no_memory, uint32_t free_pages,
		uint32_t reclaimed, uint32_t recl_pages, uint64_t elapsed_ns)
{
	static int nr_calls = 0;
	static double min_free_pages, avg_free_pages, max_free_pages;
	static double min_reclaimed, avg_reclaimed, max_reclaimed;
	static double min_recl_pages, avg_recl_pages, max_recl_pages;
	static double min_elapsed_ns, avg_elapsed_ns, max_elapsed_ns;

	if (nr_calls == 0)
	{
		min_free_pages = max_free_pages = avg_free_pages = (double) free_pages;
		min_reclaimed = max_reclaimed = avg_reclaimed = (double) reclaimed;
		min_recl_pages = max_recl_pages = avg_recl_pages = (double) recl_pages;
		min_elapsed_ns = max_elapsed_ns = avg_elapsed_ns = (double) elapsed_ns;
	}
	else
	{
		if ((double) free_pages < min_free_pages) min_free_pages = (double) free_pages;
		if ((double) free_pages > max_free_pages) max_free_pages = (double) free_pages;
		if ((double) reclaimed < min_reclaimed) min_reclaimed = (double) reclaimed;
		if ((double) reclaimed > max_reclaimed) max_reclaimed = (double) reclaimed;
		if ((double) recl_pages < min_recl_pages) min_recl_pages = (double) recl_pages;
		if ((double) recl_pages > max_recl_pages) max_recl_pages = (double) recl_pages;
		if ((double) elapsed_ns < min_elapsed_ns) min_elapsed_ns = (double) elapsed_ns;
		if ((double) elapsed_ns > max_elapsed_ns) max_elapsed_ns = (double) elapsed_ns;
		avg_free_pages += (double) free_pages;
		avg_reclaimed += (double) reclaimed;
		avg_recl_pages += (double) recl_pages;
		avg_elapsed_ns += (double) elapsed_ns;
	}

	nr_calls++;

	if (nr_calls % 1000 == 0)
	{
		printk("free_pages: %f-%f-%f\n", min_free_pages, avg_free_pages /nr_calls, max_free_pages);
		printk("reclaimed: %f-%f-%f\n", min_reclaimed, avg_reclaimed /nr_calls, max_reclaimed);
		printk("recl_pages: %f-%f-%f\n", min_recl_pages, avg_recl_pages /nr_calls, max_recl_pages);
		printk("elapsed_ns: %f-%f-%f\n", min_elapsed_ns, avg_elapsed_ns /nr_calls, max_elapsed_ns);
	}

	if (no_memory)
		return -100.0;

	if (recl_pages > 0)
		return K1 * recl_pages * recl_pages / (elapsed_ns + 5000) - K2 * elapsed_ns * free_pages;
	
	return K3 * reclaimed * reclaimed / (elapsed_ns + 5000) - K2 * elapsed_ns * free_pages;
}

static int s_free_pages(int free_pages)
{
	if (free_pages >= 256*256) return 0;
	return 1;
}

static int s_since_last_gc(uint64_t now, heap_t *hp)
{
	int64_t elapsed_ns = now - hp->ts_last_gc;
	if (elapsed_ns <= 100*1000) return 0;
	if (elapsed_ns <= 500*1000) return 1;
	if (elapsed_ns <= 2000*1000) return 2;
	return 3;
}

static int s_last_reclaimed(heap_t *hp)
{
	if (hp->last_reclaimed >= 4096) return 3;
	if (hp->last_reclaimed >= 1024) return 2;
	if (hp->last_reclaimed >= 256) return 1;
	return 0;
}

static int s_gc_count(int cnt)
{
	if (cnt >= 64) return 3;
	if (cnt >= 32) return 2;
	if (cnt >= 16) return 1;
	return 0;
}

static int s_last_recl_pages(heap_t *hp)
{
	if (hp->last_recl_pages > 0) return 1;
	return 0;
}

static int s_root_count(region_t *root_regs, int nr_regs)
{
	int count = 0;
	for (int i = 0; i < nr_regs; i++)
		count += (root_regs[i].ends - root_regs[i].starts);

	if (count >= 256) return 3;
	if (count >= 64) return 2;
	if (count >= 16) return 1;
	return 0;
}

void dump_q_table(void)
{
	printk("free_pages|since_last|reclaimed|recl_pages|cnt1|cnt2|cnt3|roots|skip|gc0|gc1|gc2|gc3|visits\n");
	for (int i = 0; i < QTAB_SIZE; i++)
	{
		int i0 = i & 1;
		int i1 = (i >> 1) & 3;
		int i2 = (i >> 3) & 3;
		int i3 = (i >> 5) & 1;
		int i4 = (i >> 6) & 3;
		int i5 = (i >> 8) & 3;
		int i6 = (i >> 10) & 3;
		int i7 = (i >> 12) & 3;

		printk("%d|%d|%d|%d|%d|%d|%d|%d|%f|%f|%f|%f|%f|%d\n",
				i0, i1, i2, i3, i4, i5, i6, i7,
				q_table[i].a[0],
				q_table[i].a[1],
				q_table[i].a[2],
				q_table[i].a[3],
				q_table[i].a[4],
				q_table[i].visits);
	}
}

