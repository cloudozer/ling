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

//
// Garbage collection - no recursion
//

#include "heap.h"

#include "ling_common.h"

#include "stdlib.h"
#include "bits.h"
#include "string.h"
#include "mm.h"

#define HEAP_GC_MAX_INDEX	16

typedef struct regions_t regions_t;
struct regions_t {
	region_t *top;
	region_t *bottom;
	region_t *end;
};

#define region_has(reg, addr) \
	((uint32_t *)(addr) >= (reg)->starts && (uint32_t *)(addr) < (reg)->ends)

static uint32_t *marshal_terms(regions_t *ss, region_t *gc_region,
		region_t *active_regions, int num_active,
	   	uint32_t *htop, t_proc_bin_t **pbs, int *pb_size);

static uint32_t *marshal_terms_fixed(regions_t *ss,
		region_t *gc_regions, int num_gc_regs,
	   	uint32_t *htop, t_proc_bin_t **pbs, int *pb_size);

static int region_compare(const void *v1, const void *v2);
static region_t *containing_region(region_t *regions, int n, void *addr);

#define RPOP(ss, reg) do { \
	assert((ss)->top < (ss)->end); \
	(reg)->starts = (ss)->top->starts; \
	(reg)->ends = (ss)->top->ends; \
	(ss)->top++; \
} while (0)

#define RPUSH(ss, starts__, ends__) do { \
	assert((ss)->top > (ss)->bottom); \
	(ss)->top--; \
	(ss)->top->starts = (starts__); \
	(ss)->top->ends = (ends__); \
} while (0)

int heap_gc_generational_N(heap_t *hp, memnode_t *gc_node, region_t *root_regs, int nr_regs)
{
	ssi(SYS_STATS_GC_RUNS);
	hp->minor_gcs++;

	assert(gc_node != 0);
	if (gc_node->index > HEAP_GC_MAX_INDEX)
		return 0;

	int is_init_node = (gc_node == &hp->init_node);

	region_t gc_region = {
		.starts = (is_init_node)
			? hp->init_node_threshold
			: NODE_THRESHOLD(gc_node),
		.ends = gc_node->starts
	};

	// determine the size needed
	int copy_size = gc_region.ends - gc_region.starts;

	// node immediately preceding gc_node
	memnode_t *prev_node = 0;
	memnode_t **ref = &hp->nodes;

	// count active nodes
	int num_active = 0;

	if (gc_node != hp->nodes)
	{
		memnode_t *node = hp->nodes;
		do {
			num_active++;
			if (node->next == gc_node)
				break;
			node = node->next;
		} while (1);
		prev_node = node;
		ref = &node->next;
	}

	// empty gc_node case
	if (copy_size == 0)
	{
		if (!is_init_node)
	   	{
			*ref = gc_node->next;
			nfree(gc_node);
		}

		return 0;
	}

	// collect and sort active regions
	region_t active_regions[num_active];

	// The total size of active nodes is needed for estimation of the maximum
	// size of allocations made by regions_t.
	int total_active_size = 0;

	memnode_t *node = hp->nodes;
	region_t *ar = active_regions;
	while (ar < active_regions + num_active)
	{
		//
		// NB: NODE_THRESHOLD() can be safely used here as the odd init_node
		// can not happen here; init_node is always the last node of the chain
		//
		ar->starts = NODE_THRESHOLD(node);
		ar->ends = node->starts;
		total_active_size += (ar->ends - ar->starts);

		ar++;
		node = node->next;
	}
	assert(node == gc_node);

	qsort(active_regions, num_active, sizeof(region_t), region_compare);

	// preceding or subsequent nodes may have enough space - merge
	memnode_t *new_node = 0;
	int nodes_merging = 1;
	if (prev_node != 0 && NODE_SPACE_LEFT(prev_node) >= copy_size*sizeof(uint32_t))
		new_node = prev_node;
	else if (gc_node->next != 0 && NODE_SPACE_LEFT(gc_node->next) >= copy_size*sizeof(uint32_t))
		new_node = gc_node->next;
	else
	{
		// always allocate standard size chunks
		int csize = heap_chunk_size(copy_size, hp->total_size);
		new_node = nalloc_N(csize *sizeof(uint32_t));
		if (new_node == 0)
			return -NO_MEMORY;
		nodes_merging = 0;
	}

	//printk("GC2: heap 0x%pp total_size %d copy_size %d: ",
	//						hp, hp->total_size, copy_size);
	//printk("GC2: gc_node 0x%pp new_node 0x%pp\n", gc_node, new_node);

	//
	// Unlinking unreferenced proc_bins:
	//
	// 1. Collect newly created proc_bins on a temporary list.
	// 2. Scan old proc_bins for those residing in the gc_region and unlink
	// them.
	// 3. Append the temporary list to the list of proc_bins of the heap.
	//
	t_proc_bin_t *new_proc_bins = 0;

	// Estimate the upper boundary of size of the regions stack
	// This is a bit of dark magic. The assumption is the most dense term type
	// with respect to the number of regions to be put on the stack relative to
	// the term size of a cons cell. This may not be the case as some programs
	// seems to require more. The upper bound esimation requires more thought.
	uint32_t upper_bound = nr_regs + (copy_size + total_active_size +1) /2 +256; // 2 is the size of a cons cell

	// The region stack now uses a unallocated page space. It is possible to use
	// it because no allocations are allowed during term marshalling.
	void *stack_starts = mm_alloc_tmp();
	uint32_t stack_size = mm_alloc_left() *PAGE_SIZE;
	if (upper_bound *sizeof(region_t) > stack_size)
	{
		//debug("heap_gc_non_recursive_N(): not enough memory for regions stack: upper %d\n", upper_bound);
		if (!nodes_merging)
			nfree(new_node);
		return -NO_MEMORY;
	}
	void *stack_ends = stack_starts +stack_size;
	//NB: sizeof(region_t) must be divide PAGE_SIZE

	// set up the regions stack
	regions_t scan_regs = {
		.bottom = (region_t *)stack_starts,
		.top = (region_t *)stack_ends,
		.end = (region_t *)stack_ends,
	};

	// add root regions to the regions stack
	for (int i = 0; i < nr_regs; i++)
		RPUSH(&scan_regs, root_regs[i].starts, root_regs[i].ends);

	uint32_t *htop = new_node->starts;
	htop = marshal_terms(&scan_regs, &gc_region,
		   	active_regions, num_active,
			htop, &new_proc_bins, &hp->total_pb_size);

	// Unlink unreferenced proc bins; see the comment above
	t_proc_bin_t *pb = hp->proc_bins;
	while (pb != 0)
	{
		t_proc_bin_t *npb = pb->next;
		if (region_has(&gc_region, pb))
			proc_bin_unlink(pb, &hp->total_pb_size);
		pb = npb;
	}

	// Append the temporary list of proc_bins to the proc_bin list of the heap
	if (new_proc_bins != 0)
	{
		t_proc_bin_t *last_new = new_proc_bins;
		while (last_new->next != 0)
			last_new = last_new->next;

		last_new->next = hp->proc_bins;
		if (last_new->next != 0)
			last_new->next->ref = &last_new->next;

		hp->proc_bins = new_proc_bins;
		hp->proc_bins->ref = &hp->proc_bins;
	}

	uint32_t new_size = htop - new_node->starts;
	new_node->starts = htop;

	hp->total_size -= copy_size;
	hp->total_size += new_size;
	assert(new_size <= copy_size);
	ssa(SYS_STATS_GC_WORDS_RECLAIM, copy_size - new_size);

	if (!nodes_merging && NODE_THRESHOLD(new_node) == new_node->starts)
	{
		nfree(new_node);
		new_node = 0;
	}

	if (nodes_merging || new_node == 0)
	{
		if (!is_init_node)
			*ref = gc_node->next;
	}
	else
	{
		*ref = new_node;
		new_node->next = (!is_init_node)
			? gc_node->next
			: gc_node;
	}

	if (is_init_node)
		gc_node->starts = hp->init_node_threshold;
	else
		nfree(gc_node);

	//printk("reclaimed %d done\n", copy_size - new_size);
	hp->sweep_after_count++;
	return 0;
}

int heap_gc_full_sweep_N(heap_t *hp, region_t *root_regs, int nr_regs)
{
	hp->sweep_after_count = 0;

	// copies all live terms to a single node
	ssi(SYS_STATS_GC_RUNS);
	//printk("GC:full-sweep: heap %pp total_size %d", hp, hp->total_size);

	// Potentially, we can use init_node here if total_size is very low;
	// this happens rarely and will complicate logic too much though.

	assert(hp->total_size > 0);
	memnode_t *sweep_node = nalloc_N(hp->total_size *sizeof(uint32_t));
	if (sweep_node == 0)
	{
		printk("GC: cannot allocate sweep node: total_size %u\n", hp->total_size);
		return -NO_MEMORY;
	}
	
	// The estimate of the upper bound of regions needed to the process to
	// complete - keep in sync with the formula in heap_gc_non_recursive()
	uint32_t upper_bound = nr_regs + (hp->total_size +1) /2 +256;

	// The region stack now uses a unallocated page space. It is possible to use
	// it because no allocations are allowed during term marshalling.
	void *stack_starts = mm_alloc_tmp();
	uint32_t stack_size = mm_alloc_left() *PAGE_SIZE;
	if (upper_bound *sizeof(region_t) > stack_size)
	{
		printk("GC: stack too small: stack_size %d needed %d\n",
				stack_size, upper_bound *sizeof(region_t));
		//printk("heap_gc_full_sweep(): not enough memory for regions stack: upper %d\n", upper_bound);
		nfree(sweep_node);
		return -NO_MEMORY;
	}
	void *stack_ends = stack_starts +stack_size;
	//NB: sizeof(region_t) must be divide PAGE_SIZE

	// Set up the regions stack
	regions_t scan_regs = {
		.bottom = (region_t *)stack_starts,
		.top = (region_t *)stack_ends,
		.end = (region_t *)stack_ends,
	};

	// Add root regions to the regions stack
	for (int i = 0; i < nr_regs; i++)
		RPUSH(&scan_regs, root_regs[i].starts, root_regs[i].ends);

	// see comment in heap_gc_non_recursive()
	t_proc_bin_t *new_proc_bins = 0;

	// collect and sort gc regions
	int num_gc_regs = 0;
	memnode_t *node = hp->nodes;
	while (node != 0)
	{
		num_gc_regs++;
		node = node->next;
	}

	region_t gc_regions[num_gc_regs];

	node = hp->nodes;
	region_t *gr = gc_regions;
	while (gr < gc_regions +num_gc_regs)
	{
		gr->starts = (node == &hp->init_node)
			?hp->init_node_threshold
			:NODE_THRESHOLD(node);
		gr->ends = node->ends;
		gr++;
		node = node->next;
	}
	assert(node == 0);

	qsort(gc_regions, num_gc_regs, sizeof(region_t), region_compare);

	uint32_t *htop = sweep_node->starts;
	htop = marshal_terms_fixed(&scan_regs,
				gc_regions, num_gc_regs,
				htop, &new_proc_bins, &hp->total_pb_size);
	assert(htop <= sweep_node->ends);

	// Unlink unreferenced proc bins
	t_proc_bin_t *pb = hp->proc_bins;
	while (pb != 0)
	{
		t_proc_bin_t *npb = pb->next;
		proc_bin_unlink(pb, &hp->total_pb_size);
		pb = npb;
	}

	// Append the temporary list of proc_bins to the proc_bin list of the heap
	if (new_proc_bins != 0)
	{
		t_proc_bin_t *last_new = new_proc_bins;
		while (last_new->next != 0)
			last_new = last_new->next;

		last_new->next = hp->proc_bins;
		if (last_new->next != 0)
			last_new->next->ref = &last_new->next;

		hp->proc_bins = new_proc_bins;
		hp->proc_bins->ref = &hp->proc_bins;
	}

	uint32_t sweep_size = htop - sweep_node->starts;
	sweep_node->starts = htop;

	ssa(SYS_STATS_GC_WORDS_RECLAIM, hp->total_size -sweep_size);

	// Replace all heap nodes with the sweep node - beware of init_node
	node = hp->nodes;
	while (node != &hp->init_node)
	{
		memnode_t *saved = node;
		node = node->next;
		nfree(saved);
	}

	hp->init_node.starts = hp->init_node_threshold;	// empty init_node
	sweep_node->next = &hp->init_node;
	hp->nodes = sweep_node;

	for (int ch = 0; ch < GC_COHORTS; ch++)
		hp->gc_cohorts[ch] = sweep_node;

	hp->total_size = sweep_size;

	//printk(" done, sweep_size %d\n", sweep_size);
	return 0;
}

static int region_compare(const void *v1, const void *v2)
{
	const region_t *r1 = (const region_t *)v1;
	const region_t *r2 = (const region_t *)v2;
	if (r1->starts > r2->starts)
		return 1;
	if (r1->starts < r2->starts)
		return -1;
	return 0;
}

static region_t *containing_region(region_t *regions, int n, void *addr)
{
	if (n == 0
		|| regions[0].starts > (uint32_t *)addr
	   	|| regions[n-1].ends <= (uint32_t *)addr)
		return 0;

	// find a region that contains the address; regions array is sorted
	int alpha = 0;
	int beta = n;
	
	while (beta - alpha > 1)
	{
		int mid = (alpha + beta) / 2;
		if (regions[mid].starts > (uint32_t *)addr)
			beta = mid;
		else
			alpha = mid;
	}

	if (regions[alpha].ends <= (uint32_t *)addr)
		return 0;

	assert(region_has(&regions[alpha], addr));
	return &regions[alpha];
}

static uint32_t *marshal_terms(regions_t *ss, region_t *gc_region,
		region_t *active_regions, int num_active,
	   	uint32_t *htop, t_proc_bin_t **pbs, int *pb_size)
{
while (ss->top < ss->end)
{
	region_t reg;
	RPOP(ss, &reg);

	term_t *ptr = reg.starts;
	while (ptr < reg.ends)
	{
		uint32_t p_tag = primary_tag(*ptr);
		if (p_tag == PRIMARY_TAG_IMMED)
		{
			ptr++;
			continue;
		}

		term_t old_term = *ptr;
		void *term_data = peel_any(old_term);

		// Skip CP stored on stack
		if (p_tag == PRIMARY_TAG_BOXED && is_cp(term_data))
		{
			ptr++;
			continue;
		}

		if (region_has(gc_region, term_data))
	   	{
			t_grave_t *grave = term_data;
			if (grave->epitaph == R_I_P)
			{
				// The term is already marshalled
				*ptr++ = grave->body;
				continue;
			}

			// The term belongs to the region being collected - marshal to
			// the new node and replace the term with a 'grave' in the
			// original location

			// Create the new term here as htop is going to move		
			term_t new_term = (shrink_ptr(htop) | p_tag);

			switch (p_tag)
			{
			case PRIMARY_TAG_CONS:
			{
				// Lists are partially unfolded to speed things up
				term_t *cons = term_data;
				term_t *new_cons = htop;
				new_cons[0] = cons[0];
				new_cons[1] = cons[1];
				htop += 2;

				do {
					if (!is_immed(new_cons[0]))
						RPUSH(ss, new_cons, new_cons+1);

					term_t tail = new_cons[1];
					if (is_immed(tail))
						break;

					cons = peel_any(tail);	// usually cons, not always
					if (!region_has(gc_region, cons))
					{
						assert(containing_region(active_regions, num_active, cons) == 0);
						break;	// tail can not be in any active region
					}

					// cons points to gc_region; the rest of the list may have
					// already been marshalled; look out for any graves
					//
					if (((t_grave_t *)cons)->epitaph == R_I_P)
					{
						new_cons[1] = ((t_grave_t *)cons)->body;
						break;
					}

					if (!is_cons(tail))	// odd list
					{
						RPUSH(ss, new_cons+1, new_cons+2);
						break;
					}
					
					new_cons[1] = tag_cons(htop);

					new_cons = htop;
					new_cons[0] = cons[0];
					new_cons[1] = cons[1];
					htop += 2;

					make_grave(cons, tag_cons(new_cons));

				} while (1);

				break;
			}
			case PRIMARY_TAG_TUPLE:
			{
				int arity = *(uint32_t *)term_data;
				term_t *elts = htop+1;
				memcpy(htop, term_data, (arity+1)*sizeof(term_t));
				// the copy of the tuple contents is now at htop:
				// htop[0] is arity, htop[1..arity] are elements
				htop += arity+1;

				RPUSH(ss, elts, elts+arity);

				break;
			}
			default:
			{
				assert(p_tag == PRIMARY_TAG_BOXED);

				uint32_t s_tag = boxed_tag(term_data);
				switch (s_tag)
				{
				case SUBTAG_POS_BIGNUM:
				case SUBTAG_NEG_BIGNUM:
				{
					uint32_t sign = bignum_sign(term_data);
					uint32_t arity = bignum_arity(term_data);
					uint16_t *digits = bignum_digits(term_data);
					box_bignum(htop, sign, arity, digits);

					break;
				}
				case SUBTAG_FLOAT:
				{
					double v = float_value(term_data);
					box_float(htop, v); 

					break;
				}
				case SUBTAG_FUN:
				{
					int arity = fun_arity(term_data);
					int num_free = fun_num_free(term_data);

					t_fun_t *fun = (t_fun_t *)term_data;
					t_fun_t *new_fun = (t_fun_t *)htop;
					box_fun(htop, num_free, arity, fun->pid, fun->module,
						fun->index, fun->uniq, fun->old_index, fun->old_uniq,
						fun->fe, fun->frozen);

					// Copied free vars require marshalling
					if (num_free > 0)
						RPUSH(ss, new_fun->frozen, new_fun->frozen+num_free);

					break;
				}	
				case SUBTAG_EXPORT:
				{
					export_t *e = ((t_export_t *)term_data)->e;
					box_export(htop, e);

					break;
				}
				case SUBTAG_MAP:
				{
					t_map_t *map = (t_map_t *)term_data;
					int size = map_size(map);
					uint32_t *saved = htop;
					box_map(htop, size, map->keys);
					memcpy(saved +2, map->values, size *sizeof(term_t));

					// hdr
					// keys
					// val1
					// val2
					// ...

					RPUSH(ss, saved +1, saved +1 +size);  // both keys and values
					break;
				}
				case SUBTAG_PID:
				{
					uint32_t id = opr_hdr_id(term_data);
					uint32_t creat = opr_hdr_creat(term_data);

					t_long_pid_t *pid = term_data;
					box_long_pid(htop, pid->node, id, pid->serial, creat);

					break;
				}
				case SUBTAG_OID:
				{
					uint32_t id = opr_hdr_id(term_data);
					uint32_t creat = opr_hdr_creat(term_data);

					t_long_oid_t *oid = term_data;
					box_long_oid(htop, oid->node, id, creat);

					break;
				}
				case SUBTAG_REF:
				{
					uint32_t id0 = opr_hdr_id(term_data);
					uint32_t creat = opr_hdr_creat(term_data);

					t_long_ref_t *ref = term_data;
					box_long_ref(htop, ref->node, creat, id0, ref->id1, ref->id2);

					break;
				}
				case SUBTAG_PROC_BIN:
				{
					t_proc_bin_t *pb = term_data;
					t_proc_bin_t *new_pb = (t_proc_bin_t *)htop;
					box_proc_bin(htop, pb->byte_size, pb->node);

					// Link the new pb before unlinking the old,
					// otherwise the bin node may get lost.
					proc_bin_link(pbs, new_pb, pb_size);
					proc_bin_unlink(pb, pb_size);

					break;
				}
				case SUBTAG_HEAP_BIN:
				{
					t_heap_bin_t *hb = term_data;
					box_heap_bin(htop, hb->byte_size, hb->data);

					break;
				}
				case SUBTAG_MATCH_CTX:
				{
					int nslots = match_ctx_num_slots(term_data);

					t_match_ctx_t *mc = term_data;
					t_match_ctx_t *new_mc = (t_match_ctx_t *)htop;
					box_match_ctx(htop, &mc->bs, mc->parent, nslots);
					memcpy(new_mc->saved_offsets, mc->saved_offsets, nslots*sizeof(int64_t));

					//
					// If the underlying is a heap binary then the data
					// reference in the new_mc is no longer valid and requires
					// an update - the heap bin should marshalled first
					//
					// the parent may happen to be a grave - it needs to be
					// resolved first - otherwise we can not tell if the parent
					// is a heap_bin
					//

					assert(is_boxed(new_mc->parent));
					t_grave_t *possible = (t_grave_t *)peel_boxed(new_mc->parent);
					if (possible->epitaph == R_I_P)
					{
						new_mc->parent = possible->body;
						if (boxed_tag(peel_boxed(new_mc->parent)) == SUBTAG_HEAP_BIN)
						{
							uint8_t *data = ((t_heap_bin_t *)peel_boxed(new_mc->parent))->data;
							new_mc->bs.data = data;
						}
					}
					else
					{
						if (boxed_tag(peel_boxed(new_mc->parent)) == SUBTAG_HEAP_BIN)
						{
							t_heap_bin_t *hb = (t_heap_bin_t *)peel_boxed(new_mc->parent);
							if (region_has(gc_region, hb))
							{
								term_t new_bin = tag_boxed(htop);
								box_heap_bin(htop, hb->byte_size, hb->data);
								make_grave((uint32_t *)hb, new_bin);

								new_mc->parent = new_bin;
								uint8_t *data = ((t_heap_bin_t *)peel_boxed(new_bin))->data;
								new_mc->bs.data = data;
							}
						}
						else
						{
							assert(boxed_tag(peel_boxed(new_mc->parent)) == SUBTAG_PROC_BIN);
							RPUSH(ss, &new_mc->parent, &new_mc->parent +1);
						}
					}

					break;
				}
				default:
				{
					assert(s_tag == SUBTAG_SUB_BIN);
					t_sub_bin_t *sb = term_data;
					int is_writable = sub_bin_is_writable(term_data);
					t_sub_bin_t *new_sb = (t_sub_bin_t *)htop;
					box_sub_bin(htop, sb->parent, sb->starts, sb->ends, is_writable);

					// marshal parent binary
					RPUSH(ss, &new_sb->parent, &new_sb->parent +1);

					break;
				}
				}
			}
			}

			make_grave(term_data, new_term);
			*ptr++ = new_term;

		} // (region_has(gc_region, term_data))
		else
		{
			region_t *my_region = containing_region(active_regions, num_active, term_data);
			if (my_region == 0)
			{
				ptr++;
				continue;
			}

			switch (p_tag)
			{
			case PRIMARY_TAG_CONS:
			{
				// Lists used to be deep thus the recursion is not the best option;
				// do a loop expecting a cons as a tail of the current cons cell

				term_t *pair = (term_t *)term_data;
				do {
					if (!is_immed(pair[0]))
						RPUSH(ss, pair, pair+1);
				
					term_t tail = pair[1];	
					if (is_nil(tail))
						break;

					if (!is_cons(tail) || !region_has(my_region, peel_cons(tail)))
					{
						// Convert the tail the hard way
						RPUSH(ss, pair+1, pair+2);
						break;
					}

					pair = (term_t *)peel_cons(tail);
				} while (1);

				break;
			}
			case PRIMARY_TAG_TUPLE:
			{
				int arity = *(uint32_t *)term_data;
				term_t *elts = (term_t *)term_data + 1;

				RPUSH(ss, elts, elts +arity);

				break;
			}
			default:
			{
				assert(p_tag == PRIMARY_TAG_BOXED);

				uint32_t s_tag = boxed_tag(term_data);
				switch(s_tag)
				{
				case SUBTAG_POS_BIGNUM:
				case SUBTAG_NEG_BIGNUM:
				case SUBTAG_FLOAT:
				case SUBTAG_EXPORT:
				case SUBTAG_PID:
				case SUBTAG_OID:
				case SUBTAG_REF:
				case SUBTAG_PROC_BIN:
				case SUBTAG_HEAP_BIN:

					break;

				case SUBTAG_FUN:
				{
					int num_free = fun_num_free(term_data);
					t_fun_t *fun = (t_fun_t *)term_data;

					if (num_free > 0)
						RPUSH(ss, fun->frozen, fun->frozen +num_free);

					break;
				}	
				case SUBTAG_MAP:
				{
					int size = map_size(term_data);
					term_t *kvs = (term_t *)term_data +1;
					RPUSH(ss, kvs +1, kvs +1 +size);	// both keys and values
					break;
				}
				case SUBTAG_MATCH_CTX:
				{
					t_match_ctx_t *mc = term_data;

					//
					// If the parent binary is a heap binary the data pointer
					// should be updated too -- see comment above
					//

					assert(is_boxed(mc->parent));
					t_grave_t *possible = (t_grave_t *)peel_boxed(mc->parent);
					if (possible->epitaph == R_I_P)
					{
						mc->parent = possible->body;
						if (boxed_tag(peel_boxed(mc->parent)) == SUBTAG_HEAP_BIN)
						{
							uint8_t *data = ((t_heap_bin_t *)peel_boxed(mc->parent))->data;
							mc->bs.data = data;
						}
					}
					else
					{
						if (boxed_tag(peel_boxed(mc->parent)) == SUBTAG_HEAP_BIN)
						{
							t_heap_bin_t *hb = (t_heap_bin_t *)peel_boxed(mc->parent);
							if (region_has(gc_region, hb))
							{
								term_t new_bin = tag_boxed(htop);
								box_heap_bin(htop, hb->byte_size, hb->data);
								make_grave((uint32_t *)hb, new_bin);
								mc->parent = new_bin;

								mc->parent = new_bin;
								uint8_t *data = ((t_heap_bin_t *)peel_boxed(new_bin))->data;
								mc->bs.data = data;
							}
						}
						else
						{
							assert(boxed_tag(peel_boxed(mc->parent)) == SUBTAG_PROC_BIN);
							RPUSH(ss, &mc->parent, &mc->parent +1);
						}
					}

					break;
				}
				default:
				{
					assert(s_tag == SUBTAG_SUB_BIN);

					t_sub_bin_t *sb = term_data;

					// marshal parent binary
					RPUSH(ss, &sb->parent, &sb->parent +1);

					break;
				}
				}
			}
			}

			ptr++;
		}
	}
}
return htop;
}

static uint32_t *marshal_terms_fixed(regions_t *ss,
		region_t *gc_regions, int num_gc_regs,
	   	uint32_t *htop, t_proc_bin_t **pbs, int *pb_size)
{
while (ss->top < ss->end)
{
	region_t reg;
	RPOP(ss, &reg);

	term_t *ptr = reg.starts;
	while (ptr < reg.ends)
	{
		uint32_t p_tag = primary_tag(*ptr);
		if (p_tag == PRIMARY_TAG_IMMED)
		{
			ptr++;
			continue;
		}

		term_t old_term = *ptr;
		void *term_data = peel_any(old_term);

		// Skip CP stored on stack
		if (p_tag == PRIMARY_TAG_BOXED && is_cp(term_data))
		{
			ptr++;
			continue;
		}

		// Skip a literal
		if (containing_region(gc_regions, num_gc_regs, term_data) == 0)
		{
			ptr++;
			continue;
		}
	   	
		t_grave_t *grave = term_data;
		if (grave->epitaph == R_I_P)
		{
			// The term is already marshalled
			*ptr++ = grave->body;
			continue;
		}

		// The term belongs to the process heap - marshal it to
		// the sweep node and replace the term with a 'grave' at the
		// original location

		// Create the new term here as htop is going to move		
		term_t new_term = (shrink_ptr(htop) | p_tag);

		switch (p_tag)
		{
		case PRIMARY_TAG_CONS:
		{
			// Lists are partially unfolded to speed things up
			term_t *cons = term_data;
			term_t *new_cons = htop;
			new_cons[0] = cons[0];
			new_cons[1] = cons[1];
			htop += 2;

			do {
				if (!is_immed(new_cons[0]))
					RPUSH(ss, new_cons, new_cons+1);

				term_t tail = new_cons[1];
				if (is_immed(tail))
					break;

				cons = peel_any(tail);	// usually cons, not always

				if (containing_region(gc_regions, num_gc_regs, cons) == 0)
					break;	// must be a literal

				// cons points to gc_region; the rest of the list may have
				// already been marshalled; look out for any graves
				//
				if (((t_grave_t *)cons)->epitaph == R_I_P)
				{
					new_cons[1] = ((t_grave_t *)cons)->body;
					break;
				}

				if (!is_cons(tail))	// odd list
				{
					RPUSH(ss, new_cons+1, new_cons+2);
					break;
				}
				
				new_cons[1] = tag_cons(htop);

				new_cons = htop;
				new_cons[0] = cons[0];
				new_cons[1] = cons[1];
				htop += 2;

				make_grave(cons, tag_cons(new_cons));

			} while (1);

			break;
		}
		case PRIMARY_TAG_TUPLE:
		{
			int arity = *(uint32_t *)term_data;
			term_t *elts = htop+1;
			memcpy(htop, term_data, (arity+1)*sizeof(term_t));
			// the copy of the tuple contents is now at htop:
			// htop[0] is arity, htop[1..arity] are elements
			htop += arity+1;

			RPUSH(ss, elts, elts+arity);

			break;
		}
		default:
		{
			assert(p_tag == PRIMARY_TAG_BOXED);

			uint32_t s_tag = boxed_tag(term_data);
			switch (s_tag)
			{
			case SUBTAG_POS_BIGNUM:
			case SUBTAG_NEG_BIGNUM:
			{
				uint32_t sign = bignum_sign(term_data);
				uint32_t arity = bignum_arity(term_data);
				uint16_t *digits = bignum_digits(term_data);
				box_bignum(htop, sign, arity, digits);

				break;
			}
			case SUBTAG_FLOAT:
			{
				double v = float_value(term_data);
				box_float(htop, v); 

				break;
			}
			case SUBTAG_FUN:
			{
				int arity = fun_arity(term_data);
				int num_free = fun_num_free(term_data);

				t_fun_t *fun = (t_fun_t *)term_data;
				t_fun_t *new_fun = (t_fun_t *)htop;
				box_fun(htop, num_free, arity, fun->pid, fun->module,
					fun->index, fun->uniq, fun->old_index, fun->old_uniq,
					fun->fe, fun->frozen);

				// Copied free vars require marshalling
				if (num_free > 0)
					RPUSH(ss, new_fun->frozen, new_fun->frozen+num_free);

				break;
			}	
			case SUBTAG_EXPORT:
			{
				export_t *e = ((t_export_t *)term_data)->e;
				box_export(htop, e);

				break;
			}
			case SUBTAG_PID:
			{
				uint32_t id = opr_hdr_id(term_data);
				uint32_t creat = opr_hdr_creat(term_data);

				t_long_pid_t *pid = term_data;
				box_long_pid(htop, pid->node, id, pid->serial, creat);

				break;
			}
			case SUBTAG_OID:
			{
				uint32_t id = opr_hdr_id(term_data);
				uint32_t creat = opr_hdr_creat(term_data);

				t_long_oid_t *oid = term_data;
				box_long_oid(htop, oid->node, id, creat);

				break;
			}
			case SUBTAG_REF:
			{
				uint32_t id0 = opr_hdr_id(term_data);
				uint32_t creat = opr_hdr_creat(term_data);

				t_long_ref_t *ref = term_data;
				box_long_ref(htop, ref->node, creat, id0, ref->id1, ref->id2);

				break;
			}
			case SUBTAG_PROC_BIN:
			{
				t_proc_bin_t *pb = term_data;
				t_proc_bin_t *new_pb = (t_proc_bin_t *)htop;
				box_proc_bin(htop, pb->byte_size, pb->node);

				// Link the new pb before unlinking the old,
				// otherwise the bin node may get lost.
				proc_bin_link(pbs, new_pb, pb_size);
				proc_bin_unlink(pb, pb_size);

				break;
			}
			case SUBTAG_HEAP_BIN:
			{
				t_heap_bin_t *hb = term_data;
				box_heap_bin(htop, hb->byte_size, hb->data);

				break;
			}
			case SUBTAG_MATCH_CTX:
			{
				int nslots = match_ctx_num_slots(term_data);

				t_match_ctx_t *mc = term_data;
				t_match_ctx_t *new_mc = (t_match_ctx_t *)htop;
				box_match_ctx(htop, &mc->bs, mc->parent, nslots);
				memcpy(new_mc->saved_offsets, mc->saved_offsets, nslots*sizeof(int64_t));

				//
				// If the underlying is a heap binary then the data
				// reference in the new_mc is no longer valid and requires
				// an update - the heap bin should marshalled first
				//
				// the parent may happen to be a grave - it needs to be
				// resolved first - otherwise we can not tell if the parent
				// is a heap_bin
				//

				assert(is_boxed(new_mc->parent));
				t_grave_t *possible = (t_grave_t *)peel_boxed(new_mc->parent);
				if (possible->epitaph == R_I_P)
				{
					new_mc->parent = possible->body;
					if (boxed_tag(peel_boxed(new_mc->parent)) == SUBTAG_HEAP_BIN)
					{
						uint8_t *data = ((t_heap_bin_t *)peel_boxed(new_mc->parent))->data;
						new_mc->bs.data = data;
					}
				}
				else
				{
					if (boxed_tag(peel_boxed(new_mc->parent)) == SUBTAG_HEAP_BIN)
					{
						t_heap_bin_t *hb = (t_heap_bin_t *)peel_boxed(new_mc->parent);

						if (containing_region(gc_regions, num_gc_regs, hb) != 0)
						{
							term_t new_bin = tag_boxed(htop);
							box_heap_bin(htop, hb->byte_size, hb->data);
							make_grave((uint32_t *)hb, new_bin);

							new_mc->parent = new_bin;
							uint8_t *data = ((t_heap_bin_t *)peel_boxed(new_bin))->data;
							new_mc->bs.data = data;
						}
					}
					else
					{
						assert(boxed_tag(peel_boxed(new_mc->parent)) == SUBTAG_PROC_BIN);
						RPUSH(ss, &new_mc->parent, &new_mc->parent +1);
					}
				}

				break;
			}
			default:
			{
				assert(s_tag == SUBTAG_SUB_BIN);
				t_sub_bin_t *sb = term_data;
				int is_writable = sub_bin_is_writable(term_data);
				t_sub_bin_t *new_sb = (t_sub_bin_t *)htop;
				box_sub_bin(htop, sb->parent, sb->starts, sb->ends, is_writable);

				// marshal parent binary
				RPUSH(ss, &new_sb->parent, &new_sb->parent +1);

				break;
			}
			}
		}
		}

		make_grave(term_data, new_term);
		*ptr++ = new_term;
	}
}
return htop;
}

//EOF

