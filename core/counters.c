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
//
//

#include <stdint.h>
#include <string.h>

#include "ling_common.h"

#include "nalloc.h"
#include "limits.h"

typedef struct counter_t counter_t;
struct counter_t {
	uint64_t ref_id;
	uint64_t value;
	uint64_t mask;
};

// All counters are in a single array
static counter_t *all_counters = 0;
static int nr_counters = 0;
static memnode_t *counters_node = 0;

static counter_t *counter_lookup(uint64_t ref_id);

void counters_init()
{
	counters_node = nalloc(QUICK_SIZE -sizeof(memnode_t));
	all_counters = (counter_t *)counters_node->starts;
}

void counter_add(uint64_t ref_id, uint64_t mask)
{
	if (all_counters +1 > (counter_t *)counters_node->ends)
	{
		// overflow, need a bigger node
		int cur_size = counters_node->index *PAGE_SIZE;
		memnode_t *new_node = nalloc(cur_size *2 -sizeof(memnode_t));
		memcpy((void *)new_node->starts,
				all_counters, nr_counters *sizeof(counter_t));
		nfree(counters_node);
		counters_node = new_node;
		all_counters = (counter_t *)new_node->starts;
	}

	counter_t *alpha = all_counters;
	counter_t *beta = all_counters +nr_counters;
	while (beta > alpha)
	{
		counter_t *mid = alpha + (beta-alpha)/2;
		if (mid->ref_id > ref_id)
			beta = mid;
		else
			alpha = mid +1;
	}

	// insert the new counter before *alpha
	memmove(alpha +1, alpha, (void *)(all_counters +nr_counters) -(void *)alpha);
	nr_counters++;

	counter_t *cntr = alpha;
	cntr->ref_id = ref_id;
	cntr->value = 0;
	cntr->mask = mask;
}

int counter_read(uint64_t ref_id, uint64_t *pval)
{
	counter_t *cntr = counter_lookup(ref_id);
	if (cntr == 0)
		return -NOT_FOUND;
	*pval = cntr->value;
	return 0;
}

int counter_increment(uint64_t ref_id, uint64_t incr)
{
	counter_t *cntr = counter_lookup(ref_id);
	if (cntr == 0)
		return -NOT_FOUND;
	cntr->value += incr;
	cntr->value &= cntr->mask;
	return 0;
}

int counter_remove(uint64_t ref_id)
{
	counter_t *cntr = counter_lookup(ref_id);
	if (cntr == 0)
		return -NOT_FOUND;
	memmove(cntr, cntr +1,
			(void *)(all_counters +nr_counters -1) -(void *)cntr);
	nr_counters--;
	return 0;
}

static counter_t *counter_lookup(uint64_t ref_id)
{
	counter_t *alpha = all_counters;
	counter_t *beta = all_counters +nr_counters;
	while (beta > alpha)
	{
		counter_t *mid = alpha + (beta-alpha)/2;
		if (mid->ref_id == ref_id)
			return mid;

		if (mid->ref_id > ref_id)
			beta = mid;
		else
			alpha = mid +1;
	}

	return 0;
}

//EOF
