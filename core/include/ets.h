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

#include "term.h"
#include "heap.h"

typedef struct ets_table_t ets_table_t;

#define ETS_MATCH_MAX_NAMES		256

typedef struct ets_match_spec_t ets_match_spec_t;
typedef struct ets_match_ctx_t ets_match_ctx_t;

typedef void (*ets_insert_func_t)(ets_table_t *tab, term_t *elts, int arity);
typedef void (*ets_insert_many_func_t)(ets_table_t *tab, term_t objs);
typedef term_t (*ets_lookup_func_t)(ets_table_t *tab, term_t key, heap_t *hp);
typedef term_t (*ets_member_func_t)(ets_table_t *tab, term_t key);
typedef term_t (*ets_first_func_t)(ets_table_t *tab, heap_t *hp);
typedef term_t (*ets_last_func_t)(ets_table_t *tab, heap_t *hp);
typedef term_t (*ets_next_func_t)(ets_table_t *tab, term_t key, heap_t *hp);
typedef term_t (*ets_prev_func_t)(ets_table_t *tab, term_t key, heap_t *hp);
typedef term_t (*ets_slot_func_t)(ets_table_t *tab, int n, heap_t *hp);
typedef void (*ets_delete_func_t)(ets_table_t *tab, term_t key);
typedef void (*ets_delete_object_func_t)(ets_table_t *tab, term_t *elts, int arity);
typedef void (*ets_delete_all_objects_func_t)(ets_table_t *tab);
typedef void (*ets_data_destroy_func_t)(ets_table_t *tab);
typedef ets_match_ctx_t *(*ets_match_first_func_t)(ets_table_t *tab,
							ets_match_spec_t *ms, term_t pid, heap_t *hp);
typedef ets_match_ctx_t *(*ets_match_last_func_t)(ets_table_t *tab,
							ets_match_spec_t *ms, term_t pid, heap_t *hp);
typedef term_t (*ets_match_next_func_t)(ets_table_t *tab, ets_match_ctx_t *ctx, heap_t *hp);
typedef term_t (*ets_match_prev_func_t)(ets_table_t *tab, ets_match_ctx_t *ctx, heap_t *hp);
typedef void (*ets_match_delete_func_t)(ets_table_t *tab, ets_match_ctx_t *ctx);
typedef term_t (*ets_match_make_continuation_func_t)(ets_table_t *tab,
		term_t match_spec, int limit, ets_match_ctx_t *ctx, heap_t *hp);
typedef ets_match_ctx_t *(*ets_match_use_continuation_func_t)(ets_table_t *tab,
							ets_match_spec_t *ms, term_t continuation, term_t pid, heap_t *hp);

typedef struct ets_virt_t ets_virt_t;
struct ets_virt_t {
	ets_insert_func_t insert;
	ets_insert_many_func_t insert_many;
	ets_lookup_func_t lookup;
	ets_member_func_t member;
	ets_first_func_t first;
	ets_last_func_t last;
	ets_next_func_t next;
	ets_prev_func_t prev;
	ets_slot_func_t slot;
	ets_delete_func_t delete;
	ets_delete_object_func_t delete_object;
	ets_delete_all_objects_func_t delete_all_objects;
	ets_data_destroy_func_t data_destroy;
	ets_match_first_func_t match_first;
	ets_match_last_func_t match_last;
	ets_match_next_func_t match_next;
	ets_match_prev_func_t match_prev;
	ets_match_delete_func_t match_delete;
	ets_match_make_continuation_func_t match_make_continuation;
	ets_match_use_continuation_func_t match_use_continuation;
};

struct ets_table_t {
	term_t tid;
	term_t name;
	term_t type;
	term_t access;
	int key_pos;
	term_t owner;
	term_t heir;		// a short pid or 'none'
	term_t heir_data;

	int write_concurrency;	// ignored
	int read_concurrency;	// ignored
	int compressed;			// ignored

	// record count
	int count;

	// total memory allocated (words)
	uint32_t total_alloc;

	// safe_fixtable/2 calls
	int fixed;
	uint64_t first_fix_timestamp;

	// table home node
	memnode_t *home_node;

	// virtual methods
	ets_virt_t *virt;

	// type-dependent data
	void *data;
};

// ets_plane_t - big chunks of memory allocated using nalloc()
// 				 ref-counted, go away when count drops to zero
//
// ets_lot_t -   small memory ranges that can be freed
// 				 individually; hold references to ets_plane_t
//

typedef struct ets_plane_t ets_plane_t;
//NB: overlapping with memnode_t
struct ets_plane_t {
	unsigned long refc;	// was: memnode_t *next;
	int index;
	uint32_t *starts;
	uint32_t *ends;
};

typedef struct ets_lot_t ets_lot_t;
struct ets_lot_t {
	ets_plane_t *my_plane;
	t_proc_bin_t *proc_bins;
	uint32_t wsize;
};

typedef struct ets_obj_t ets_obj_t;
struct ets_obj_t {
	term_t *elts;
	int arity;
};

#define ALLOC_WSIZE(p)		(((ets_lot_t *)(p))[-1].wsize)
#define ALLOC_PROC_BINS(p)	(((ets_lot_t *)(p))[-1].proc_bins)

extern uint32_t total_ets_alloc_size;

void ets_init(void);
void ets_alloc_init(void);

term_t ets_all_tables(heap_t *hp);
ets_table_t *ets_table_make(term_t name, int is_named, term_t type,
			term_t access, int key_pos, term_t owner, term_t heir, term_t heir_data);
void ets_process_exits(term_t pid);
ets_table_t *ets_table_lookup(term_t tid);
void ets_table_rename(ets_table_t *tab, term_t new_name);
void ets_table_delete(ets_table_t *tab);
int ets_set_opt(ets_table_t *tab, term_t opt);
int ets_fix_table(ets_table_t *tab, term_t pid);
int ets_unfix_table(ets_table_t *tab, term_t pid);
term_t ets_fix_info(ets_table_t *tab, heap_t *hp);
void ets_insert(ets_table_t *tab, term_t *elts, int arity);
void ets_insert_many(ets_table_t *tab, term_t objs);
term_t ets_lookup(ets_table_t *tab, term_t key, heap_t *hp);
term_t ets_member(ets_table_t *tab, term_t key);
term_t ets_first(ets_table_t *tab, heap_t *hp);
term_t ets_last(ets_table_t *tab, heap_t *hp);
term_t ets_next(ets_table_t *tab, term_t key, heap_t *hp);
term_t ets_prev(ets_table_t *tab, term_t key, heap_t *hp);
term_t ets_slot(ets_table_t *tab, int n, heap_t *hp);
void ets_delete(ets_table_t *tab, term_t key);
void ets_delete_object(ets_table_t *tab, term_t *elts, int arity);
void ets_delete_all_objects(ets_table_t *tab);

void *ets_hash_data_make(memnode_t *home_node);
void *ets_ordset_data_make(memnode_t *home_node);

uint32_t *ets_alloc(int wsize);
uint32_t *ets_alloc_N(int wsize);
void ets_free(uint32_t *p);

int ets_terms_copy_size(term_t *terms, int num);
int ets_terms_copy_size_N(term_t *terms, int num);
uint32_t *ets_terms_copy_non_recursive_N(term_t *terms, int num,
								uint32_t *htop, t_proc_bin_t **pbs);

term_t *ets_marshal_object(term_t *terms, int num);
term_t *ets_marshal_object_N(term_t *terms, int num);
term_t ets_demarshal_object(term_t *elts, int arity, heap_t *hp);

ets_match_spec_t *ets_match_compile_spec(term_t MatchSpec, heap_t *hp);
ets_match_spec_t *ets_match_compile_pattern(term_t MatchPat, int return_object, heap_t *hp);
term_t ets_match_spec_run(ets_match_spec_t *mspec,
		term_t *elts, int arity, term_t pid, heap_t *hp);

ets_match_ctx_t *ets_match_first(ets_table_t *tab, ets_match_spec_t *ms, term_t pid, heap_t *hp);
ets_match_ctx_t *ets_match_last(ets_table_t *tab, ets_match_spec_t *ms, term_t pid, heap_t *hp);
term_t ets_match_next(ets_table_t *tab, ets_match_ctx_t *ctx, heap_t *hp);
term_t ets_match_prev(ets_table_t *tab, ets_match_ctx_t *ctx, heap_t *hp);
void ets_match_delete(ets_table_t *tab, ets_match_ctx_t *ctx);
term_t ets_match_make_continuation(ets_table_t *tab,
		term_t match_spec, int limit, ets_match_ctx_t *ctx, heap_t *hp);
ets_match_ctx_t *ets_match_use_continuation(ets_table_t *tab,
		ets_match_spec_t *ms, term_t continuation, term_t pid, heap_t *hp);

//EOF
