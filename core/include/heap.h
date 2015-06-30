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

#pragma once

#include <stdint.h>

#include "nalloc.h"

//
// Heap allocations
//

// gc_hook() locations
#define GC_LOC_IDLE			0
#define GC_LOC_PROC_YIELD	1
#define GC_LOC_PROC_WAIT	2
#define GC_LOC_TEST_HEAP	3
#define GC_LOCS				4

#define GC_COHORTS			3

#define HEAP_ASK_FOR_MORE	512

typedef struct heap_t heap_t;
struct heap_t {
	memnode_t init_node;			// a fake memory node for the initial buffer
	uint32_t *init_node_threshold;	// 
	memnode_t *nodes;				// a chain of memory nodes; never 0
	int total_size;					// the size of the heap
	t_proc_bin_t *proc_bins;		// the list of refc binaries
	int total_pb_size;				// the sum of sizes of referenced proc_bins
	int suppress_gc;				// do not collect garbage
	int full_sweep_after;			// the fullsweep_after option value
	int sweep_after_count;			// the counter for fullsweep_after
	int minor_gcs;					// the number of gc runs while running
#ifdef LING_DEBUG
	// the expected value of the next heap_set_top()
	uint32_t *expected_top;
#endif
	// Viktor's GC
	memnode_t *gc_cohorts[GC_COHORTS];
	int gc_yield_tally;
	int gc_wait_tally;
};

typedef struct region_t region_t;
struct region_t {
	uint32_t *starts;
	uint32_t *ends;
};

#define HEAP_COPY_TERMS_MAX_DEPTH	1000

#define HEAP_NEW_SIZE_RATIO			16
#define HEAP_NEW_SIZE_CAP			(16384 - WSIZE(memnode_t))

inline static int heap_chunk_size(int needed, int total_size)
{
	int csize = total_size / HEAP_NEW_SIZE_RATIO;
	if (csize > HEAP_NEW_SIZE_CAP)
		csize = HEAP_NEW_SIZE_CAP;
	if (csize < needed)
		csize = needed;
	return csize;
}

void heap_init(heap_t *hp, uint32_t *init_starts, uint32_t *init_ends);

void heap_reset_init_node_end(heap_t *hp, uint32_t *ends);

uint32_t *heap_alloc(heap_t *hp, int needed);
uint32_t *heap_alloc_N(heap_t *hp, int needed);

int heap_gc_generational_N(heap_t *hp, memnode_t *gc_node, region_t *root_regs, int nr_regs);
int heap_gc_full_sweep_N(heap_t *hp, region_t *root_regs, int nr_regs);

void gc_hook(int gc_loc, term_t pid, heap_t *hp, region_t *root_regs, int nr_regs);

void *heap_top(heap_t *hp);
void heap_set_top0(heap_t *hp, uint32_t *new_top);
#ifdef LING_DEBUG
void heap_set_top(heap_t *hp, uint32_t *new_top);
#else
#define heap_set_top(hp, top)	heap_set_top0((hp), (top))
#endif
uint32_t *heap_end(heap_t *hp);

void heap_done(heap_t *hp);

term_t heap_cons(heap_t *hp, term_t hd, term_t tl);
term_t heap_tuple2(heap_t *hp, term_t e1, term_t e2);
term_t heap_tuple3(heap_t *hp, term_t e1, term_t e2, term_t e3);
term_t heap_tuple4(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4);
term_t heap_tuple5(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5);
term_t heap_tuple6(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6);
term_t heap_strz_N(heap_t *hp, const char *s);
term_t heap_strz(heap_t *hp, const char *s);
term_t heap_str_N(heap_t *hp, const char *s, int len);
term_t heap_str(heap_t *hp, const char *s, int len);
term_t heap_vector_to_list(heap_t *hp, term_t *vec, int num);
term_t heap_vector_to_list_N(heap_t *hp, term_t *vec, int num);
int heap_list_to_vector(term_t lst, term_t *vec);
int heap_copy_terms_N(heap_t *hp, term_t *terms, int num);
term_t heap_make_bin(heap_t *hp, int size, uint8_t **ptr);
term_t heap_make_bin_N(heap_t *hp, int size, uint8_t **ptr);
term_t heap_float(heap_t *hp, double val);
term_t heap_float_with_check(heap_t *hp, double val);
term_t heap_bignum(heap_t *hp, int sign, int ndigs, uint16_t **dp);
term_t int_to_term(int64_t z, heap_t *hp);
term_t uint_to_term(uint64_t u, heap_t *hp);

uint8_t *heap_tmp_buf(heap_t *hp, int size);
uint8_t *heap_tmp_buf_N(heap_t *hp, int size);
term_t heap_remake_local_ref_N(heap_t *hp, uint64_t ref_id);
term_t heap_make_ref(heap_t *heap);
int ref_is_local(term_t t);
uint64_t local_ref_id(term_t t);

