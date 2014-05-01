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

#include "ets.h"

#include "ling_common.h"

#include "atom_defs.h"
#include "hash.h"
#include "string.h"
#include "scheduler.h"
#include "time.h"

#define ETS_HASH_HOME_SIZE		4096
#define ETS_ORDSET_HOME_SIZE	4096

#define ETS_MAX_SAFE_FIXES		256

typedef struct ets_safe_fix_t ets_safe_fix_t;
struct ets_safe_fix_t {
	term_t pid;
	ets_table_t *tab;
	int count;
};

extern ets_virt_t ets_hash_virt;
extern ets_virt_t ets_ordset_virt;

static hash_t *ets_tables = 0;
static ets_safe_fix_t ets_safe_fixes[ETS_MAX_SAFE_FIXES];
static int nr_safe_fixes = 0;

static void ets_table_destroy(ets_table_t *tab);

void ets_init(void)
{
	ets_alloc_init();
	ets_tables = hash_make();
	//nr_safe_fixes = 0;
}

term_t ets_all_tables(heap_t *hp)
{
	hash_index_t hi;
	hash_start(ets_tables, &hi);
	ets_table_t *tab;
	term_t all = nil;
	while ((tab = hash_next(&hi)) != 0)
		all = heap_cons(hp, tab->tid, all);

	return all;
}

ets_table_t *ets_table_make(term_t name, int is_named, term_t type,
			term_t access, int key_pos, term_t owner, term_t heir, term_t heir_data)
{
	int size;
	if (type == A_SET || type == A_BAG || type == A_DUPLICATE_BAG)
		size = ETS_HASH_HOME_SIZE;
	else
	{
		assert(type == A_ORDERED_SET);
		size = ETS_ORDSET_HOME_SIZE;
	}

	if (is_named && hash_get(ets_tables, &name, sizeof(name)) != 0)
		return 0;

	int heir_data_wsize = 0;
	// EXCEPTION POSSIBLE
	if (heir != A_NONE)
		heir_data_wsize = ets_terms_copy_size(&heir_data, 1);

	// EXCEPTION POSSIBLE
	memnode_t *home_node = nalloc(size - sizeof(memnode_t));
	memset(home_node->starts, 0, (home_node->ends - home_node->starts) *sizeof(uint32_t));
	ets_table_t *tab = (ets_table_t *)home_node->starts;
	home_node->starts += WSIZE(ets_table_t);
	tab->home_node = home_node;
	
	tab->name = name;
	if (is_named)
		tab->tid = name;
	else
	{
		static int next_numeric_tid = 0;
		next_numeric_tid++;
		tab->tid = tag_int(next_numeric_tid);
	}

	tab->type = type;
	tab->access = access;
	tab->key_pos = key_pos;
	tab->owner = owner;

	tab->write_concurrency = 0;
	tab->read_concurrency = 0;
	tab->compressed = 0;

	//tab->count = 0;
	//tab->total_alloc = 0;

	//tab->fixed = 0;

	// The layout of a home node of a table:
	//
	// home_node -->		 memnode_t struct
	// 						 ets_table_t struct
	// 						 ets_hash_data_t or similar
	// 						 (type-dependent data)
	// home_node->starts --> init heap space
	// home_node->ends -->
	//

	if (type == A_SET || type == A_BAG || type == A_DUPLICATE_BAG)
	{
		tab->data = ets_hash_data_make(home_node);
		tab->virt = &ets_hash_virt;
	}
	else
	{
		assert(type == A_ORDERED_SET);
		tab->data = ets_ordset_data_make(home_node);
		tab->virt = &ets_ordset_virt;
	}

	uint32_t *heir_data_space = 0;
	if (heir != A_NONE && heir_data_wsize > 0)
	{
		heir_data_space = ets_alloc_N(heir_data_wsize);
		if (heir_data_space == 0)
			goto no_memory1;
		tab->total_alloc += heir_data_wsize;
		t_proc_bin_t **pbs = &ALLOC_PROC_BINS(heir_data_space);

		uint32_t *last = ets_terms_copy_non_recursive_N(&heir_data, 1,
													heir_data_space, pbs);
		if (last == 0)
			goto no_memory2;
		assert(last == heir_data_space +heir_data_wsize);
	}

	tab->heir = heir;
	tab->heir_data = heir_data;

	assert(hash_get(ets_tables, &tab->tid, sizeof(tab->tid)) == 0);
	if (hash_set_N(ets_tables, &tab->tid, sizeof(tab->tid), tab) < 0)
		goto no_memory2;

	return tab;

no_memory2:
	tab->total_alloc -= ALLOC_WSIZE(heir_data_space);
	if (heir_data_space != 0)
		ets_free(heir_data_space);

no_memory1:
	nfree(home_node);
	no_memory_signal();
}

void ets_process_exits(term_t pid)
{
	// release fixed tables
	ets_safe_fix_t *fix = ets_safe_fixes;
	while (fix < ets_safe_fixes + nr_safe_fixes)
	{
		if (fix->pid == pid)
		{
			assert(fix->tab->fixed >= fix->count);
			fix->tab->fixed -= fix->count;
			ets_safe_fix_t *last = ets_safe_fixes + nr_safe_fixes -1;
			if (fix < last)
				*fix = *last;
			nr_safe_fixes--;
		}
		else
			fix++;
	}

	// delete owned tables
	hash_index_t hi;
	hash_start(ets_tables, &hi);
	ets_table_t *tab;
	while ((tab = hash_next(&hi)) != 0)
	{
		if (tab->owner != pid)
			continue;

		proc_t *successor;
		if (tab->heir != A_NONE && tab->heir != pid &&
				(successor = scheduler_lookup(tab->heir)) != 0)
		{
			//{'ETS-TRANSFER',Tab,FromPid,HeirData}
			term_t new_heir_data = tab->heir_data;
			uint32_t *htop = 0;
			int x = heap_copy_terms_N(&successor->hp, &new_heir_data, 1);
			assert(x != -TOO_DEEP);		// heap_copy_terms is still recursive
			if (x == 0)
				htop = heap_alloc_N(&successor->hp, 1 +4);
			if (x == 0 && htop != 0)
			{
				htop[0] = 4;
				htop[1] = AETS_TRANSFER__;
				htop[2] = tab->tid;
				htop[3] = pid;
				htop[4] = new_heir_data;
				heap_set_top(&successor->hp, htop +1 +4);

				x = scheduler_new_local_mail_N(successor, tag_tuple(htop));
			}
			if (x < 0)
				printk("ETS-TRANSFER notification for table %pt "
						"not delivered to %pt~n", T(tab->tid), T(tab->heir));

			tab->owner = tab->heir;
		}
		else
			ets_table_delete(tab);
	}
}

ets_table_t *ets_table_lookup(term_t tid)
{
	return hash_get(ets_tables, &tid, sizeof(tid));
}

void ets_table_rename(ets_table_t *tab, term_t new_name)
{
	if (tab->tid == tab->name)
	{
		hash_set(ets_tables, &tab->tid, sizeof(tab->tid), 0);
		tab->tid = new_name;
		if (hash_set_N(ets_tables, &tab->tid, sizeof(tab->tid), tab) < 0)
		{
			// undo the renaming
			tab->tid = tab->name;
			no_memory_signal();
		}
	}
	tab->name = new_name;
}

void ets_table_delete(ets_table_t *tab)
{
	// remove table fixes
	ets_safe_fix_t *fix = ets_safe_fixes;
	while (fix < ets_safe_fixes + nr_safe_fixes)
	{
		if (fix->tab == tab)
		{
			ets_safe_fix_t *last = ets_safe_fixes + nr_safe_fixes -1;
			if (fix < last)
				*fix = *last;
			nr_safe_fixes--;
		}
		else
			fix++;
	}

	// remove from registry
	assert(hash_get(ets_tables, &tab->tid, sizeof(tab->tid)) == tab);
	hash_set_N(ets_tables, &tab->tid, sizeof(tab->tid), 0);		// never fails

	// release memory
	ets_table_destroy(tab);
}

int ets_set_opt(ets_table_t *tab, term_t opt)
{
	if (!is_tuple(opt))
		return -BAD_ARG;
	uint32_t *tp = peel_tuple(opt);
	
	if (tp[0] == 3 && tp[1] == A_HEIR && is_short_pid(tp[2]))
	{
		term_t heir = tp[2];
		term_t heir_data = tp[3];

		if (!is_immed(heir_data))
		{
			// EXCEPTION POSSIBLE
			int heir_data_wsize = ets_terms_copy_size(&heir_data, 1);
			// EXCEPTION POSSIBLE
			uint32_t *heir_data_space = ets_alloc(heir_data_wsize);
			tab->total_alloc += heir_data_wsize;
			t_proc_bin_t **pbs = &ALLOC_PROC_BINS(heir_data_space);
			uint32_t *last = ets_terms_copy_non_recursive_N(&heir_data, 1, heir_data_space, pbs);
			if (last == 0)
			{
				tab->total_alloc -= ALLOC_WSIZE(heir_data_space);
				ets_free(heir_data_space);
				no_memory_signal();
			}
			assert(last == heir_data_space +heir_data_wsize);
		}

		if (tab->heir != A_NONE && !is_immed(tab->heir_data))
		{
			uint32_t *old_heir_data_space = peel_any(tab->heir_data);
			tab->total_alloc -= ALLOC_WSIZE(old_heir_data_space);
			ets_free(old_heir_data_space);
		}

		tab->heir = heir;
		tab->heir_data = heir_data;
	}
	else if (tp[0] == 2 && tp[1] == A_HEIR && tp[2] == A_NONE)
	{
		if (tab->heir != A_NONE && !is_immed(tab->heir_data))
		{
			uint32_t *old_heir_data_space = peel_any(tab->heir_data);
			tab->total_alloc -= ALLOC_WSIZE(old_heir_data_space);
			ets_free(old_heir_data_space);
		}

		tab->heir = A_NONE;
		tab->heir_data = noval;
	}
	else if (tp[0] == 2 && tp[1] == A_PROTECTION &&
			(tp[2] == A_PRIVATE || tp[2] == A_PROTECTED || tp[2] == A_PUBLIC))
	{
		tab->access = tp[2];
	}
	else
		return -BAD_ARG;

	return 0;
}

int ets_fix_table(ets_table_t *tab, term_t pid)
{
	ets_safe_fix_t *fix = ets_safe_fixes;
	while (fix < ets_safe_fixes + nr_safe_fixes)
	{
		if (fix->tab == tab && fix->pid == pid)
		{
			fix->count++;
			assert(tab->fixed > 0);
			tab->fixed++;
			return 0;
		}
		fix++;
	}

	if (nr_safe_fixes >= ETS_MAX_SAFE_FIXES)
		return -TOO_LONG;

	ets_safe_fix_t *new_fix = ets_safe_fixes + nr_safe_fixes;
	nr_safe_fixes++;

	new_fix->tab = tab;
	new_fix->pid = pid;
	new_fix->count = 1;

	if (tab->fixed == 0)
		tab->first_fix_timestamp = wall_clock();
	tab->fixed++;

	return 0;
}

int ets_unfix_table(ets_table_t *tab, term_t pid)
{
	ets_safe_fix_t *fix = ets_safe_fixes;
	while (fix < ets_safe_fixes + nr_safe_fixes)
	{
		if (fix->tab == tab && fix->pid == pid)
		{
			assert(tab->fixed > 0);
			tab->fixed--;
			fix->count--;
			if (fix->count == 0)
			{
				ets_safe_fix_t *last = ets_safe_fixes + nr_safe_fixes -1;
				if (fix < last)
					*fix = *last;
				nr_safe_fixes--;
			}
			return 0;
		}
		fix++;
	}

	return 0;	// unjustified unfix is a no-op
	//return -BAD_ARG;
}

term_t ets_fix_info(ets_table_t *tab, heap_t *hp)
{
	if (tab->fixed == 0)
		return A_FALSE;

	uint64_t usecs = tab->first_fix_timestamp / 1000;
	uint64_t secs = usecs / 1000000;

	int t1 = usecs % 1000000;
	int t2 = secs % 1000000;
	int t3 = secs / 1000000;

	term_t timestamp = heap_tuple3(hp, tag_int(t3), tag_int(t2), tag_int(t1));

	term_t fs = nil;
	ets_safe_fix_t *fix = ets_safe_fixes;
	while (fix < ets_safe_fixes + nr_safe_fixes)
	{
		if (fix->tab == tab)
		{
			assert(fits_int(fix->count));
			term_t t = heap_tuple2(hp, fix->pid, tag_int(fix->count));
			fs = heap_cons(hp, t, fs);
		}
		fix++;
	}

	return heap_tuple2(hp, timestamp, fs);
}

void ets_insert(ets_table_t *tab, term_t *elts, int arity)
{
	tab->virt->insert(tab, elts, arity);
}

void ets_insert_many(ets_table_t *tab, term_t objs)
{
	tab->virt->insert_many(tab, objs);
}

term_t ets_lookup(ets_table_t *tab, term_t key, heap_t *hp)
{
	return tab->virt->lookup(tab, key, hp);
}

term_t ets_member(ets_table_t *tab, term_t key)
{
	return tab->virt->member(tab, key);
}

term_t ets_first(ets_table_t *tab, heap_t *hp)
{
	return tab->virt->first(tab, hp);
}

term_t ets_last(ets_table_t *tab, heap_t *hp)
{
	return tab->virt->last(tab, hp);
}

term_t ets_next(ets_table_t *tab, term_t key, heap_t *hp)
{
	return tab->virt->next(tab, key, hp);
}

term_t ets_prev(ets_table_t *tab, term_t key, heap_t *hp)
{
	return tab->virt->prev(tab, key, hp);
}

term_t ets_slot(ets_table_t *tab, int n, heap_t *hp)
{
	return tab->virt->slot(tab, n, hp);
}

void ets_delete(ets_table_t *tab, term_t key)
{
	tab->virt->delete(tab, key);
}

void ets_delete_object(ets_table_t *tab, term_t *elts, int arity)
{
	tab->virt->delete_object(tab, elts, arity);
}

void ets_delete_all_objects(ets_table_t *tab)
{
	tab->virt->delete_all_objects(tab);
}

ets_match_ctx_t *ets_match_first(ets_table_t *tab, ets_match_spec_t *ms, term_t pid, heap_t *hp)
{
	return tab->virt->match_first(tab, ms, pid, hp);
}

ets_match_ctx_t *ets_match_last(ets_table_t *tab, ets_match_spec_t *ms, term_t pid, heap_t *hp)
{
	return tab->virt->match_last(tab, ms, pid, hp);
}

term_t ets_match_next(ets_table_t *tab, ets_match_ctx_t *ctx, heap_t *hp)
{
	return tab->virt->match_next(tab, ctx, hp);
}

term_t ets_match_prev(ets_table_t *tab, ets_match_ctx_t *ctx, heap_t *hp)
{
	return tab->virt->match_prev(tab, ctx, hp);
}

void ets_match_delete(ets_table_t *tab, ets_match_ctx_t *ctx)
{
	tab->virt->match_delete(tab, ctx);
}

term_t ets_match_make_continuation(ets_table_t *tab,
		term_t match_spec, int limit, ets_match_ctx_t *ctx, heap_t *hp)
{
	return tab->virt->match_make_continuation(tab, match_spec, limit, ctx, hp);
}

ets_match_ctx_t *ets_match_use_continuation(ets_table_t *tab,
		ets_match_spec_t *ms, term_t continuation, term_t pid, heap_t *hp)
{
	return tab->virt->match_use_continuation(tab, ms, continuation, pid, hp);
}

static void ets_table_destroy(ets_table_t *tab)
{
	tab->virt->data_destroy(tab);

	if (tab->heir != A_NONE && !is_immed(tab->heir_data))
	{
		uint32_t *old_heir_data_space = peel_any(tab->heir_data);
		//NB: total_alloc not updated
		ets_free(old_heir_data_space);
	}

	nfree(tab->home_node);
}

term_t *ets_marshal_object(term_t *terms, int num)
{
	// EXCEPTION POSSIBLE
	int wsize = ets_terms_copy_size(terms, num);

	// EXCEPTION POSSIBLE
	uint32_t *p = ets_alloc(num + wsize);
	t_proc_bin_t **pbs = &ALLOC_PROC_BINS(p);
	memcpy(p, terms, num *sizeof(term_t));

	UNUSED uint32_t *last = ets_terms_copy_non_recursive_N(p, num, p +num, pbs);
	if (p == 0)
	{
		ets_free(p);
		no_memory_signal();
	}
	assert(last == p +num +wsize);

	return p;
}

term_t *ets_marshal_object_N(term_t *terms, int num)
{
	int wsize = ets_terms_copy_size(terms, num);
	if (wsize < 0)
	{
		assert(wsize == -NO_MEMORY);
		return 0;
	}

	uint32_t *p = ets_alloc_N(num + wsize);
	if (p == 0)
		return 0;
	t_proc_bin_t **pbs = &ALLOC_PROC_BINS(p);
	memcpy(p, terms, num *sizeof(term_t));

	uint32_t *last = ets_terms_copy_non_recursive_N(p, num, p +num, pbs);
	if (last == 0)
	{
		ets_free(p);
		return 0;
	}
	assert(last == p +num +wsize);

	return p;
}

term_t ets_demarshal_object(term_t *elts, int arity, heap_t *hp)
{
	if (arity == 0)
		return ZERO_TUPLE;

	// marshal elements first; then allocate a tuple
	term_t new_elts[arity];
	memcpy(new_elts, elts, arity *sizeof(term_t));
	int x = heap_copy_terms_N(hp, new_elts, arity);
	assert(x != -TOO_DEEP);		// heap_copy_terms still recursive
	if (x < 0)
	{
		assert(x == -NO_MEMORY);
		no_memory_signal();
	}

	// EXCEPTION POSSIBLE
	uint32_t *p = heap_alloc(hp, arity +1);
	term_t dt = tag_tuple(p);
	heap_set_top(hp, p +1 +arity);
	*p++ = arity;
	memcpy(p, new_elts, arity *sizeof(term_t));
	return dt;
}

//EOF
