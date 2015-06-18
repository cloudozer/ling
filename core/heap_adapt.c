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

// Space
#define AGC_P_LESS_ROOTS		0
#define AGC_N_LESS_ROOTS		2
#define AGC_P_RECLAIMED			2
#define AGC_N_RECLAIMED			2
#define AGC_P_RECL_PAGES		4
#define AGC_N_RECL_PAGES		1
#define AGC_P_RECL_BINS			5
#define AGC_N_RECL_BINS			1
#define AGC_P_GC1_COUNT			6
#define AGC_N_GC1_COUNT			2
#define	AGC_P_FS_COUNT			8
#define AGC_N_FS_COUNT			2
#define AGC_P_FREE_PAGES		10
#define AGC_N_FREE_PAGES		1
#define AGC_P_SINCE_LAST		11
#define AGC_N_SINCE_LAST		2
#define AGC_INDEX_SIZE			13

#define AGC_Q_TABLE_SIZE		(2 << AGC_INDEX_SIZE)

#define AGC_ACTIONS				4
#define AGC_SPACE_DIMS			8

static const char *q_dim_desc[AGC_SPACE_DIMS] = {
	"roots", "reclaimed", "recl_pages", "recl_bins",
	"gc1_count", "fs_count", "free_pages", "since_last"
};

static const char *q_action_desc[AGC_ACTIONS] = {
	"skip", "gc0", "gc1", "fs"
};

static struct q_table_t {
	// 0: skip
	// 1: GC0
	// 2: GC1
	// 3: FS
	double a[AGC_ACTIONS];
	int visits;
} q_table[AGC_Q_TABLE_SIZE] = { { { 0.0 }, 0 } };

// do not use GC0 or GC1 on nodes larger than this
#define AGC_YINDEX	16

#define AGC_ALPHA	0.2
#define AGC_GAMMA	0.95

static int s_less_roots(int new_roots, int old_roots);
static int s_reclaimed(int recl_words);
static int s_recl_pages(int recl_pages);
static int s_recl_bins(int recl_bins);
static int s_gc1_count(int count);
static int s_fs_count(int count);
static int s_free_pages(int free_pages);
static int s_since_last(uint64_t elapsed_ns);

static double calc_reward(int no_memory, uint32_t free_pages,
		uint32_t reclaimed, uint32_t recl_pages, uint64_t elapsed_ns);

static int state_index(int new_roots, int free_pages, uint64_t now, heap_t *hp)
{
	int i_less_roots = s_less_roots(new_roots, hp->last_roots);
	int i_reclaimed  = s_reclaimed(hp->last_reclaimed);
	int i_recl_pages = s_recl_pages(hp->last_recl_pages);
	int i_recl_bins  = s_recl_bins(hp->last_recl_bins);
	int i_gc1_count  = s_gc1_count(hp->gc1_count);
	int i_fs_count   = s_fs_count(hp->sweep_after_count);
	int i_free_pages = s_free_pages(free_pages);
	int i_since_last = s_since_last(now - hp->ts_last_gc);

	return (i_less_roots << AGC_P_LESS_ROOTS) |
		   (i_reclaimed  << AGC_P_RECLAIMED) |
		   (i_recl_pages << AGC_P_RECL_PAGES) |
		   (i_recl_bins  << AGC_P_RECL_BINS) |
		   (i_gc1_count  << AGC_P_GC1_COUNT) |
		   (i_fs_count   << AGC_P_FS_COUNT) |
		   (i_free_pages << AGC_P_FREE_PAGES) |
		   (i_since_last << AGC_P_SINCE_LAST);
}

static int s_less_roots(int new_roots, int old_roots)
{
	if (new_roots < old_roots - 64) return 3;
	if (new_roots < old_roots - 32) return 2;
	if (new_roots < old_roots - 16) return 1;
	return 0;
}

static int s_reclaimed(int recl_words)
{
	if (recl_words >= 4096) return 3;
	if (recl_words >= 1024) return 2;
	if (recl_words >= 256) return 1;
	return 0;
}

static int s_recl_pages(int recl_pages)
{
	if (recl_pages > 0) return 1;
	return 0;
}

static int s_recl_bins(int recl_bins)
{
	if (recl_bins > 0) return 1;
	return 0;
}

static int s_gc1_count(int count)
{
	if (count >= 64) return 3;
	if (count >= 32) return 2;
	if (count >= 16) return 1;
	return 0;
}

static int s_fs_count(int count)
{
	if (count >= 16384) return 3;
	if (count >= 4096) return 2;
	if (count >= 1024) return 1;
	return 0;
}

static int s_free_pages(int free_pages)
{
	if (free_pages >= 256*256) return 0;
	return 1;
}

static int s_since_last(uint64_t elapsed_ns)
{
	if (elapsed_ns <= 100*1000) return 0;
	if (elapsed_ns <= 500*1000) return 1;
	if (elapsed_ns <= 2000*1000) return 2;
	return 3;
}

uint32_t *heap_ensure_adaptive(heap_t *hp, int needed, region_t *root_regs, int nr_regs)
{
	//NB: ignores full_sweep_after option

	uint64_t now = monotonic_clock();
	int free_pages = mm_alloc_left() + nalloc_freed_pages();
	int new_roots = 0;
	for (int i = 0; i < nr_regs; i++)
		new_roots += (root_regs[i].ends - root_regs[i].starts);

	int index0 = state_index(new_roots, free_pages, now, hp);
	assert(index0 < AGC_Q_TABLE_SIZE);

	q_table[index0].visits++;

	int action = 0;
#if 0
	double intervals[AGC_ACTIONS];
	double total = 0;
	for (int i = 0; i < AGC_ACTIONS; i++)
	{
		intervals[i] = exp(q_table[index0].a[i]);
		//XXX: fp overflow
		total += intervals[i];
	}

	double roll = (double) mt_lrand() * total / 0x100000000l;
	while (1)
	{
		assert(action < AGC_ACTIONS);
		roll -= intervals[action];
		if (roll < 0)
			break;
		action++;
	}
#else
	int nr_action3 = 0;
	uint32_t roll;
toss:
	roll = mt_lrand();
	if (roll < (0x100000000l / 10))	// 10%
	{
		// pick random
		action = roll % AGC_ACTIONS;
		if (action == 3)
			if(++nr_action3 < 3)
				goto toss;
	}
	else
	{
		// choose best
		double *qq = q_table[index0].a;
		action = 0;
		for (int i = 1; i < AGC_ACTIONS; i++)
			if (qq[i] > qq[action])
				action = i;
	}
#endif

	hp->gc1_count++;

	double reward = 0.0;

	if (action == 0)
	{
		// do nothing
		reward = -0.1;	// avoid too many skips
	}
	else
	{
		if (action == 1)	// GC0
		{
			hp->gc_spot = hp->nodes;
		}
		else if (action == 2)	// GC1
		{
			hp->gc_spot = hp->nodes;
			if (hp->gc_spot->next == 0)
				goto prohibited;

			// choose a node other than the first
			do {
				hp->gc_spot = hp->gc_spot->next;
				uint32_t rand = mt_lrand();
				if (rand < (0x100000000l / 4))	// 25%
					break;
			} while (hp->gc_spot->next != 0);
		}

		// action prohibited, node too large
		if ((action == 1 || action == 2) && hp->gc_spot->index > AGC_YINDEX)
			goto prohibited;
		
		int saved_size = hp->total_size;
		int saved_alloc_pages = hp->total_alloc_pages;
		int saved_pb_size = hp->total_pb_size;

		int ok;
		if (action == 3)
			ok = heap_gc_full_sweep_N(hp, root_regs, nr_regs);
		else
			ok = heap_gc_non_recursive_N(hp, root_regs, nr_regs);

		hp->last_recl_bins = saved_pb_size - hp->total_pb_size;
		// possible, due to 'init_node'
		if (saved_alloc_pages >= hp->total_alloc_pages)
			hp->last_recl_pages = saved_alloc_pages - hp->total_alloc_pages;
		else
			hp->last_recl_pages = 0;
		hp->last_reclaimed = saved_size - hp->total_size;

		hp->ts_last_gc = monotonic_clock();
		uint64_t elapsed_ns = hp->ts_last_gc - now;

		int no_memory = (ok < 0);
		reward = calc_reward(no_memory,
			free_pages, hp->last_reclaimed, hp->last_recl_pages, elapsed_ns);

		if (action == 2) hp->gc1_count = 0;

		int free_pages1 = mm_alloc_left() + nalloc_freed_pages();
		int index1 = state_index(new_roots, free_pages1, hp->ts_last_gc, hp);
		assert(index1 < AGC_Q_TABLE_SIZE);

		hp->last_roots = new_roots;

		double maxq = q_table[index1].a[0];
		for (int i = 1; i < AGC_ACTIONS; i++)
			if (q_table[index1].a[i] > maxq)
				maxq = q_table[index1].a[i];

		// learning
		double change = AGC_ALPHA * (reward + AGC_GAMMA * maxq - q_table[index0].a[action]);
		q_table[index0].a[action] += change;
	}

prohibited:
	return heap_alloc(hp, needed);
}

//a1
//#define K1	(1e6)
//#define K2	(2.5e-9)
//#define K3	(6e-1)

//a2
//#define K1	(250000)
//#define K2	(2.5e-9)
//#define K3	(6e-1)

//a3
//#define K1	(1e6)
//#define K2	(1.25e-9)
//#define K3	(6e-1)

//a4
//#define K1	(1e6)
//#define K2	(2.5e-9)
//#define K3	(0.15)

//a5
//#define K1	(250000)
//#define K2	(1.25e-9)
//#define K3	(0.6)

//a6
//#define K1	(250000)
//#define K2	(2.5e-9)
//#define K3	(0.15)

//a7
//#define K1	(1e6)
//#define K2	(1.25e-9)
//#define K3	(0.15)

//a8
//#define K1	(250000)
//#define K2	(1.25e-9)
//#define K3	(0.15)

//b1
//#define K1	(50000)
//#define K2	(1.25e-9)
//#define K3	(0.6)

//b2
//#define K1	(10000)
//#define K2	(1.25e-9)
//#define K3	(0.6)

//b3
//#define K1	(2000)
//#define K2	(1.25e-9)
//#define K3	(0.6)

//c0 (b2 copy)
//#define K1	(10000)
//#define K2	(1.25e-9)
//#define K3	(0.6)

//c1
//#define K1	(10000)
//#define K2	(4.17e-10)
//#define K3	(0.6)

//c2
//#define K1	(10000)
//#define K2	(1.25e-9)
//#define K3	(0.2)

//c3
//#define K1	(10000)
//#define K2	(3.75e-9)
//#define K3	(0.6)

//c4
//#define K1	(10000)
//#define K2	(1.25e-9)
//#define K3	(1.8)

//d1 (c0 copy)
//#define K1	(10000)
//#define K2	(2e-9)
//#define K3	(0.6)

//d2
#define K1	(10000)
#define K2	(2.5e-9)
#define K3	(0.6)

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

void dump_q_table(void)
{
	for (int i = 0; i < AGC_SPACE_DIMS; i++)
		printk("%s|", q_dim_desc[i]);
	for (int i = 0; i < AGC_ACTIONS; i++)
		printk("%s|", q_action_desc[i]);
	printk("visits\n");
	for (int i = 0; i < AGC_Q_TABLE_SIZE; i++)
	{
		int s[AGC_SPACE_DIMS];
		s[0] = (i >> AGC_P_LESS_ROOTS) & ((1 << AGC_N_LESS_ROOTS) -1);
		s[1] = (i >> AGC_P_RECLAIMED)  & ((1 << AGC_N_RECLAIMED) -1);
		s[2] = (i >> AGC_P_RECL_PAGES) & ((1 << AGC_N_RECL_PAGES) -1);
		s[3] = (i >> AGC_P_RECL_BINS)  & ((1 << AGC_N_RECL_BINS) -1);
		s[4] = (i >> AGC_P_GC1_COUNT)  & ((1 << AGC_N_GC1_COUNT) -1);
		s[5] = (i >> AGC_P_FS_COUNT)   & ((1 << AGC_N_FS_COUNT) -1);
		s[6] = (i >> AGC_P_FREE_PAGES) & ((1 << AGC_N_FREE_PAGES) -1);
		s[7] = (i >> AGC_P_SINCE_LAST) & ((1 << AGC_N_SINCE_LAST) -1);

		for (int j = 0; j < AGC_SPACE_DIMS; j++)
			printk("%d|", s[j]);
		for (int j = 0; j < AGC_ACTIONS; j++)
			printk("%f|", q_table[i].a[j]);
		printk("%d\n", q_table[i].visits);
	}
}

