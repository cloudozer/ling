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

#include "string.h"
#include "atom_defs.h"
#include "list_util.h"
#include "term_util.h"

#define ETS_HASH_BUCKETS	256
#define ETS_HASH_EXT_SIZE	4096

typedef struct ets_hash_entry_t ets_hash_entry_t;
struct ets_hash_entry_t {
	ets_hash_entry_t *next;
	uint32_t hash;
	int arity;
	term_t *elts;
	ets_hash_entry_t *alt;
};

typedef struct ets_hash_data_t ets_hash_data_t;
struct ets_hash_data_t {
	ets_hash_entry_t **buckets;
	int nr_buckets;
	ets_hash_entry_t *free;
	int nr_free;
	memnode_t *nodes;
};

typedef struct ets_hash_index_t ets_hash_index_t;
struct ets_hash_index_t {
	ets_hash_data_t *data;
	ets_hash_entry_t *entry;
	int buck;
};

typedef struct ets_hash_match_ctx_t ets_hash_match_ctx_t;
struct ets_hash_match_ctx_t {
	ets_match_spec_t *mspec;
	term_t self;
	term_t leftovers;
	int key_bound;
	union {
	struct {
		int buck;
		ets_hash_entry_t *ent;
		ets_hash_entry_t **ref;	// *ref == ent if ent != 0
		ets_hash_entry_t *more;
	};
	struct {
	};
	};
};

static void ets_hash_data_destroy(ets_table_t *tab);
static void ets_hash_insert(ets_table_t *tab, term_t *elts, int arity);
static void ets_hash_insert_many(ets_table_t *tab, term_t objs);
static term_t ets_hash_lookup(ets_table_t *tab, term_t key, heap_t *hp);
static term_t ets_hash_member(ets_table_t *tab, term_t key);
static term_t ets_hash_first(ets_table_t *tab, heap_t *hp);
static term_t ets_hash_next(ets_table_t *tab, term_t key, heap_t *hp);
static term_t ets_hash_slot(ets_table_t *tab, int n, heap_t *hp);
static void ets_hash_delete(ets_table_t *tab, term_t key);
static void ets_hash_delete_object(ets_table_t *tab, term_t *elts, int arity);
static void ets_hash_delete_all_objects(ets_table_t *tab);

static ets_match_ctx_t *ets_hash_match_first(ets_table_t *tab,
									ets_match_spec_t *ms, term_t pid, heap_t *hp);
static term_t ets_hash_match_next(ets_table_t *tab,
									ets_match_ctx_t *ctx, heap_t *hp);
static void ets_hash_match_delete(ets_table_t *tab, ets_match_ctx_t *ctx);
static term_t ets_hash_match_make_continuation(ets_table_t *tab,
		term_t match_spec, int limit, ets_match_ctx_t *ctx, heap_t *hp);
static ets_match_ctx_t *ets_hash_match_use_continuation(ets_table_t *tab,
					ets_match_spec_t *ms, term_t continuation, term_t pid, heap_t *hp);

static void ets_hash_index_start(ets_hash_data_t *data, ets_hash_index_t *index);
static ets_hash_entry_t *ets_hash_index_next(ets_hash_index_t *index);

static void split_node_into_entries(ets_hash_data_t *data, memnode_t *node);
static ets_hash_entry_t *lookup_entry(term_t key, ets_table_t *tab, int *buckp);

ets_virt_t ets_hash_virt = {
	.insert = ets_hash_insert,
	.insert_many = ets_hash_insert_many,
	.lookup = ets_hash_lookup,
	.member = ets_hash_member,
	.first = ets_hash_first,
	.last = ets_hash_first,
	.next = ets_hash_next,
	.prev = ets_hash_next,
	.slot = ets_hash_slot,
	.delete = ets_hash_delete,
	.delete_object = ets_hash_delete_object,
	.delete_all_objects = ets_hash_delete_all_objects,
	.data_destroy = ets_hash_data_destroy,
	.match_first = ets_hash_match_first,
	.match_last = ets_hash_match_first,
	.match_next = ets_hash_match_next,
	.match_prev = ets_hash_match_next,
	.match_delete = ets_hash_match_delete,
	.match_make_continuation = ets_hash_match_make_continuation,
	.match_use_continuation = ets_hash_match_use_continuation
};

void *ets_hash_data_make(memnode_t *node)
{
	ets_hash_data_t *data = (ets_hash_data_t *)node->starts;
	node->starts += WSIZE(ets_hash_data_t);
	ets_hash_entry_t **buckets = (ets_hash_entry_t **)node->starts;
	node->starts = (uint32_t *)((ets_hash_entry_t **)node->starts + ETS_HASH_BUCKETS);
	data->buckets = buckets;
	data->nr_buckets = ETS_HASH_BUCKETS;
	//data->free_entry = 0;
	//data->nr_free_entries = 0;
	//data->free_cons = 0;
	//data->nr_free_conses = 0;
	//data->nodes = 0;
	split_node_into_entries(data, node);
	return data;
}

static void split_node_into_entries(ets_hash_data_t *data, memnode_t *node)
{
	ets_hash_entry_t *ent = (ets_hash_entry_t *)node->starts;
	while (ent +1 <= (ets_hash_entry_t *)node->ends)
	{
		ent->next = data->free;
		data->free = ent;
		data->nr_free++;
		ent++;
	}
	node->starts = (uint32_t *)ent;
}

static void cleanup_deleted_entry(ets_table_t *tab, ets_hash_entry_t **ref)
{
	ets_hash_data_t *data = tab->data;

	ets_hash_entry_t *ent = *ref;
	assert(ent->alt == 0);
	assert(ent->arity < 0);
	tab->total_alloc -= ALLOC_WSIZE(ent->elts);
	ets_free(ent->elts);

	*ref = ent->next;

	ent->next = data->free;
	data->free = ent;
	data->nr_free++;
}

static ets_hash_entry_t *lookup_entry(term_t key, ets_table_t *tab, int *buckp)
{
	ets_hash_data_t *data = tab->data;

	uint32_t hash = portable_hash(key, 0);		//NB: erlang:phash/2
	uint32_t buck = hash & (data->nr_buckets -1);
	ets_hash_entry_t **ref = &data->buckets[buck];
	while (*ref != 0)
	{
		if ((*ref)->hash == hash)
		{
			term_t key1 = (*ref)->elts[tab->key_pos -1];
			if (key == key1 || are_terms_equal(key, key1, 1))	// =:=
				break;
		}
		ref = &(*ref)->next;
	}

	ets_hash_entry_t *ent = *ref;
	if (ent == 0)
		return 0;

	if (ent->arity < 0)
	{
		if (tab->fixed == 0)
			cleanup_deleted_entry(tab, ref);
		return 0;
	}

	if (buckp)
		*buckp = buck;
	return ent;
}

static ets_hash_entry_t *find_matching_entry(ets_hash_entry_t *ent, term_t *elts, int arity)
{
	while (ent != 0)
	{
		if (ent->arity == arity)
		{
			int match = 1;
			for (int i = 0; i < arity; i++)
			{
				if (ent->elts[i] != elts[i] &&
						!are_terms_equal(ent->elts[i], elts[i], 1))	// =:=
				{
					match = 0;
					break;
				}
			}
			if (match)
				return ent;
		}
		ent = ent->alt;
	}
	return 0;
}

static void ets_hash_data_destroy(ets_table_t *tab)
{
	ets_hash_data_t *data = tab->data;

	ets_hash_index_t index;
	ets_hash_index_start(data, &index);
	ets_hash_entry_t *ent;
	while ((ent = ets_hash_index_next(&index)) != 0)
	{
		ets_hash_entry_t *more = ent;
		while (more != 0)
		{
			ets_free(more->elts);	// total_alloc not updated
			more = more->alt;
		}
	}
	nfree_chain(data->nodes);
}

static void ets_hash_insert(ets_table_t *tab, term_t *elts, int arity)
{
	assert(arity >= tab->key_pos);
	ets_hash_data_t *data = tab->data;

	if (data->free == 0)
	{
		// EXCEPTION POSSIBLE
		memnode_t *node = nalloc(ETS_HASH_EXT_SIZE - sizeof(memnode_t));
		node->next = data->nodes;
		data->nodes = node;

		split_node_into_entries(data, node);
		assert(data->free != 0);
	}

	// EXCEPTION POSSIBLE
	uint32_t *new_elts = ets_marshal_object(elts, arity);
	tab->total_alloc += ALLOC_WSIZE(new_elts);
	
	term_t key = new_elts[tab->key_pos -1];
	uint32_t hash = portable_hash(key, 0);		//NB: erlang:phash/2
	uint32_t buck = hash & (data->nr_buckets -1);
	ets_hash_entry_t *ent = data->buckets[buck];
	while (ent != 0)
	{
		if (ent->hash == hash)
		{
			term_t key1 = ent->elts[tab->key_pos -1];
			if (key == key1 || are_terms_equal(key, key1, 1))	// =:=
				break;
		}
		ent = ent->next;
	}

	if (ent != 0)
	{
		if (tab->type == A_SET)
		{
			assert(ent->alt == 0);
			if (ent->arity < 0)
				tab->count++;

			tab->total_alloc -= ALLOC_WSIZE(ent->elts);
			ets_free(ent->elts);

			ent->arity = arity;
			ent->elts = new_elts;
			return;
		}
		else
		{
			assert(tab->type == A_BAG || tab->type == A_DUPLICATE_BAG);
			if (ent->arity < 0)
			{
				// deleted entry - replace
				assert(ent->alt == 0);

				tab->total_alloc -= ALLOC_WSIZE(ent->elts);
				ets_free(ent->elts);

				ent->arity = arity;
				ent->elts = new_elts;
				tab->count++;
				return;
			}

			if (tab->type == A_BAG &&
					find_matching_entry(ent, new_elts, arity) != 0)
			{
				tab->total_alloc -= ALLOC_WSIZE(new_elts);
				ets_free(new_elts);
				return;
			}

			assert(data->free != 0);
			ets_hash_entry_t *add = data->free;
			data->free = add->next;
			data->nr_free--;

			// A new entry is about to be added to the list of alternatives; the
			// documentation requires that the time order of insertions is
			// preserved; thus we need to move data from 'ent' to 'add' and load
			// the new data to 'ent'.

			add->hash = hash;
			add->arity = ent->arity;
			add->elts = ent->elts;
			
			add->alt = ent->alt;
			ent->alt = add;

			ent->arity = arity;
			ent->elts = new_elts;

			tab->count++;
			return;
		}
	}

	assert(data->free != 0);
	ets_hash_entry_t *new_ent = data->free;
	data->free = new_ent->next;
	data->nr_free--;

	// same for set, bag, and duplicate bag
	new_ent->hash = hash;
	new_ent->arity = arity;
	new_ent->elts = new_elts;
	new_ent->alt = 0;

	new_ent->next = data->buckets[buck];
	data->buckets[buck] = new_ent;

	tab->count++;
}

static void ets_hash_insert_many(ets_table_t *tab, term_t objs)
{
	ets_hash_data_t *data = tab->data;
	int count = list_len(objs);
	assert(count >= 0);

	// Avoid a partial insert in the face of possible 'no memory' exception:
	// 1. objs is a proper list of tuples of sufficient size - checked by the
	// caller;
	// 2. verify that there are enough free entries in the table;
	// 3. marshal the objs to ETS keeping rollback information;
	// 4. insert the tuples into appropriate buckets without a possibility of an
	// exception.
	//

	if (count == 0)
		return;
	
	while (data->nr_free < count)
	{
		// EXCEPTION POSSIBLE
		memnode_t *node = nalloc(ETS_HASH_EXT_SIZE - sizeof(memnode_t));
		node->next = data->nodes;
		data->nodes = node;

		split_node_into_entries(data, node);
	}

	// EXCEPTION POSSIBLE
	ets_obj_t *my_objects = (ets_obj_t *)ets_alloc(count *WSIZE(ets_obj_t));
	tab->total_alloc += ALLOC_WSIZE(my_objects);
	int num_marsh = 0;

	term_t t = objs;
	ets_obj_t *o = my_objects;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		assert(is_tuple(cons[0]));
		uint32_t *p = peel_tuple(cons[0]);
		int arity = *p++;

		term_t *new_elts = ets_marshal_object_N(p, arity);
		if (new_elts == 0)
		{
			// undo allocations before raising an exception
			for (int i = 0; i < num_marsh; i++)
			{
				tab->total_alloc -= ALLOC_WSIZE(my_objects[i].elts);
				ets_free(my_objects[i].elts);
			}
			tab->total_alloc -= ALLOC_WSIZE(my_objects);
			ets_free((uint32_t *)my_objects);
			no_memory_signal();
		}
		tab->total_alloc += ALLOC_WSIZE(new_elts);

		o->elts = new_elts;
		o->arity = arity;
		o++;
		num_marsh++;

		t = cons[1];
	}
	assert(is_nil(t));
	assert(num_marsh == count);

	assert(data->nr_free >= count);
	ets_obj_t *obj = my_objects;
	while (obj < my_objects +count)
	{
		assert(obj->arity >= tab->key_pos);

		term_t key = obj->elts[tab->key_pos -1];
		uint32_t hash = portable_hash(key, 0);		//NB: erlang:phash/2
		uint32_t buck = hash & (data->nr_buckets -1);
		ets_hash_entry_t *ent = data->buckets[buck];
		while (ent != 0)
		{
			if (ent->hash == hash)
			{
				term_t key1 = ent->elts[tab->key_pos -1];
				if (key == key1 || are_terms_equal(key, key1, 1))	// =:=
						break;
			}
			ent = ent->next;
		}

		if (ent != 0)
		{
			if (tab->type == A_SET)
			{
				assert(ent->alt == 0);
				if (ent->arity < 0)
					tab->count++;

				tab->total_alloc -= ALLOC_WSIZE(ent->elts);
				ets_free(ent->elts);

				ent->arity = obj->arity;
				ent->elts = obj->elts;
				goto next_obj;
			}
			else
			{
				assert(tab->type == A_BAG || tab->type == A_DUPLICATE_BAG);
				if (ent->arity < 0)
				{
					// deleted entry - replace
					assert(ent->alt == 0);

					tab->total_alloc -= ALLOC_WSIZE(ent->elts);
					ets_free(ent->elts);

					ent->arity = obj->arity;
					ent->elts = obj->elts;
					tab->count++;
					goto next_obj;
				}

				if (tab->type == A_BAG &&
						find_matching_entry(ent, obj->elts, obj->arity) != 0)
				{
					tab->total_alloc -= ALLOC_WSIZE(obj->elts);
					ets_free(obj->elts);
					goto next_obj;
				}

				assert(data->free != 0);
				ets_hash_entry_t *add = data->free;
				data->free = add->next;
				data->nr_free--;

				// see comment in insert()

				add->hash = hash;
				add->arity = ent->arity;
				add->elts = ent->elts;
				
				add->alt = ent->alt;
				ent->alt = add;

				ent->arity = obj->arity;
				ent->elts = obj->elts;

				tab->count++;
				goto next_obj;
			}
		}

		assert(data->free != 0);
		ets_hash_entry_t *new_ent = data->free;
		data->free = new_ent->next;
		data->nr_free--;

		// same for set, bag, and duplicate bag
		new_ent->hash = hash;
		new_ent->arity = obj->arity;
		new_ent->elts = obj->elts;
		new_ent->alt = 0;

		new_ent->next = data->buckets[buck];
		data->buckets[buck] = new_ent;

		tab->count++;
next_obj:
		obj++;
	}
	tab->total_alloc -= ALLOC_WSIZE(my_objects);
	ets_free((uint32_t *)my_objects);
}

static term_t ets_hash_lookup(ets_table_t *tab, term_t key, heap_t *hp)
{
	ets_hash_entry_t *ent = lookup_entry(key, tab, 0);
	if (ent == 0)
		return nil;

	term_t result = nil;
	do {
		// EXCEPTION POSSIBLE
		term_t dt = ets_demarshal_object(ent->elts, ent->arity, hp);
		// EXCEPTION POSSIBLE
		result = heap_cons(hp, dt, result);
		ent = ent->alt;
	} while (ent != 0);
	return result;
}

static term_t ets_hash_member(ets_table_t *tab, term_t key)
{
	if (lookup_entry(key, tab, 0) != 0)
		return A_TRUE;

	return A_FALSE;
}

static term_t ets_hash_first(ets_table_t *tab, heap_t *hp)
{
	ets_hash_data_t *data = tab->data;
	for (int i = 0; i < data->nr_buckets; i++)
	{
		ets_hash_entry_t *ent = data->buckets[i];
		while (ent != 0)
		{
			if (ent->arity > 0)
			{
				term_t new_key = ent->elts[tab->key_pos -1];
				int x = heap_copy_terms_N(hp, &new_key, 1);
				assert(x != -TOO_DEEP);		// heap_copy_terms is still recursive
				if (x < 0)
				{
					assert(x == -NO_MEMORY);
					no_memory_signal();
				}
				return new_key;
			}
			ent = ent->next;
		}
	}

	return AEOT__;
}

static term_t ets_hash_next(ets_table_t *tab, term_t key, heap_t *hp)
{
	ets_hash_data_t *data = tab->data;

	uint32_t hash = portable_hash(key, 0);		//NB: erlang:phash/2
	uint32_t buck = hash & (data->nr_buckets -1);
	ets_hash_entry_t **ref = &data->buckets[buck];
	while (*ref != 0)
	{
		if ((*ref)->hash == hash)
		{
			term_t key1 = (*ref)->elts[tab->key_pos -1];
			if (key == key1 || are_terms_equal(key, key1, 1))	// =:=
				break;
		}
		ref = &(*ref)->next;
	}

	ets_hash_entry_t *ent = *ref;
	if (ent == 0)
		return noval;

	ent = ent->next;
	if ((*ref)->arity < 0 && tab->fixed == 0)
		cleanup_deleted_entry(tab, ref);

next_try:	
	while (ent != 0)
	{
		if (ent != 0 && ent->arity > 0)
		{
			term_t new_key = ent->elts[tab->key_pos -1];
			int x = heap_copy_terms_N(hp, &new_key, 1);
			assert(x != -TOO_DEEP);		// heap_copy_terms is still recursive
			if (x < 0)
			{
				assert(x == -NO_MEMORY);
				no_memory_signal();
			}
			return new_key;
		}
		ent = ent->next;
	}
	buck++;
	if (buck >= data->nr_buckets)
		return AEOT__;

	ent = data->buckets[buck];
	goto next_try;
}

static term_t ets_hash_slot(ets_table_t *tab, int n, heap_t *hp)
{
	ets_hash_data_t *data = tab->data;

	if (n < 0 || n > data->nr_buckets)
		return noval;
	if (n == data->nr_buckets)
		return AEOT__;

	ets_hash_entry_t *ent = data->buckets[n];
	term_t objs = nil;
	while (ent != 0)
	{
		ets_hash_entry_t *more = ent;
		do {
			// EXCEPTION POSSIBLE
			term_t obj = ets_demarshal_object(more->elts, more->arity, hp);
			// EXCEPTION POSSIBLE
			objs = heap_cons(hp, obj, objs);
			more = more->alt;
		} while (more != 0);
		ent = ent->next;
	}

	return objs;
}

static void ets_hash_delete(ets_table_t *tab, term_t key)
{
	uint32_t hash = portable_hash(key, 0);		//NB: erlang:phash/2
	ets_hash_data_t *data = tab->data;
	uint32_t buck = hash & (data->nr_buckets -1);
	ets_hash_entry_t **ref = &data->buckets[buck];
	while (*ref != 0)
	{
		if ((*ref)->hash == hash)
		{
			term_t key1 = (*ref)->elts[tab->key_pos -1];
			if (key == key1 || are_terms_equal(key, key1, 1))	// =:=
				break;
		}
		ref = &(*ref)->next;
	}

	ets_hash_entry_t *ent = *ref;
	if (ent == 0)
		return;

	if (tab->fixed == 0)
	{
		if (ent->arity < 0)
		{
			cleanup_deleted_entry(tab, ref);
			return;
		}
		else
			*ref = ent->next;	// detached from buckets
	}
	else
	{
		if (ent->arity < 0)
			return;		// already deleted

		ets_hash_entry_t *last = ent;
		ent = ent->alt;

		last->arity = -last->arity;
		last->alt = 0;
	}

	while (ent != 0)
	{
		tab->total_alloc -= ALLOC_WSIZE(ent->elts);
		ets_free(ent->elts);

		ets_hash_entry_t *freeme = ent;
		ent = ent->alt;

		freeme->next = data->free;
		data->free = freeme;
		data->nr_free++;
	}
}

static void ets_hash_delete_object(ets_table_t *tab, term_t *elts, int arity)
{
	ets_hash_data_t *data = tab->data;

	int key_pos = tab->key_pos;
	term_t key = elts[key_pos -1];
	uint32_t hash = portable_hash(key, 0);		//NB: erlang:phash/2
	uint32_t buck = hash & (data->nr_buckets -1);
	ets_hash_entry_t **ref = &data->buckets[buck];
	while (*ref != 0)
	{
		if ((*ref)->hash == hash)
		{
			term_t key1 = (*ref)->elts[tab->key_pos -1];
			if (key == key1 || are_terms_equal(key, key1, 1))	// =:=
				break;
		}
		ref = &(*ref)->next;
	}

	ets_hash_entry_t *ent = *ref;
	if (ent == 0)
		return;
	
	if (ent->arity < 0)
	{
		assert(ent->alt == 0);
		if (tab->fixed == 0)
			cleanup_deleted_entry(tab, ref);
		return;
	}

	// 1. detach the found entry from the next-list;
	// 2. traverse alt-list and release entries that match;
	// 3. do not remove the last entry if the table is fixed - mark with
	// 	  negative arity instead;
	// 4. reattach the found entry to the next-list, if not empty.
	
	// 1.
	*ref = ent->next;

	// 2.
	ets_hash_entry_t **tee = &ent;
	while (*tee != 0)
	{
		if ((*tee)->arity == arity)
		{
			term_t *other_elts = (*tee)->elts;
			int match = 1;
			for (int i = 0; i < arity; i++)
			{
				if (i != key_pos -1 && elts[i] != other_elts[i]
						&& !are_terms_equal(elts[i], other_elts[i], 1))		// =:=
				{
					match = 0;
					break;
				}
			}
			if (match)
			{
				tab->count--;

				// 3.
				if (tab->fixed > 0 && tee == &ent && (*tee)->alt == 0)
				{
					(*tee)->arity = -(*tee)->arity;
					break;
				}

				tab->total_alloc -= ALLOC_WSIZE(other_elts);
				ets_free(other_elts);

				ets_hash_entry_t *freeme = *tee;
				*tee = freeme->alt;

				freeme->next = data->free;
				data->free = freeme;
				data->nr_free++;
				continue;
			}
		}
		tee = &(*tee)->alt;
	}
	assert(tab->fixed == 0 || ent != 0);

	// 4.
	if (ent != 0)
	{
		ent->next = *ref;
		*ref = ent;
	}
}

static void ets_hash_delete_all_objects(ets_table_t *tab)
{
	ets_hash_data_t *data = tab->data;

	ets_hash_index_t index;
	ets_hash_index_start(data, &index);
	ets_hash_entry_t *ent = ets_hash_index_next(&index);
	while (ent != 0)
	{
		ets_hash_entry_t *p = ent;
		ent = ets_hash_index_next(&index);

		if (tab->fixed > 0)
		{
			p->arity = -p->arity;
			p = p->alt;
		}

		while (p != 0)
		{
			tab->total_alloc -= ALLOC_WSIZE(p->elts);
			ets_free(p->elts);

			ets_hash_entry_t *freeme = p;
			p = p->alt;

			freeme->next = data->free;
			data->free = freeme;
			data->nr_free++;	
		}
	}

	if (tab->fixed == 0)
		memset(data->buckets, 0, data->nr_buckets *sizeof(ets_hash_entry_t *));
	tab->count = 0;
}

static ets_match_ctx_t *ets_hash_match_first(ets_table_t *tab,
								ets_match_spec_t *ms, term_t pid, heap_t *hp)
{
	ets_hash_data_t *data = tab->data;

	ets_hash_match_ctx_t *ctx = (ets_hash_match_ctx_t *)heap_tmp_buf(hp, sizeof(*ctx));
	ctx->mspec = ms;
	ctx->self = pid;
	ctx->leftovers = nil;

	//ctx->key_bound = ets_match_key_bound(ms, tab->key_pos);
	ctx->key_bound = 0;
	if (ctx->key_bound)
	{
		//TODO
		not_implemented("ets hash match first (key bound)");
	}
	else
	{
		ctx->buck = 0;
		do {
			ctx->ref = &data->buckets[ctx->buck];
			ctx->ent = data->buckets[ctx->buck];
			if (ctx->ent != 0 && ctx->ent->arity >= 0)
				break;
			ctx->buck++;
		} while (ctx->buck < ETS_HASH_BUCKETS);
		if (ctx->buck >= ETS_HASH_BUCKETS)
			ctx->ent = 0;	// $end_of_table
		ctx->more = ctx->ent;
	}

	return (ets_match_ctx_t *)ctx;
}

static term_t ets_hash_match_next(ets_table_t *tab,
						ets_match_ctx_t *mc, heap_t *hp)
{
	ets_hash_match_ctx_t *ctx = (ets_hash_match_ctx_t *)mc;

	if (ctx->ent == 0)
		return noval;	// $end_of_table

	if (is_cons(ctx->leftovers))
	{
		term_t *cons = peel_cons(ctx->leftovers);
		ctx->leftovers = cons[1];
		return cons[0];
	}

more_run:
	while (ctx->more != 0)
	{
		term_t r = ets_match_spec_run(ctx->mspec,
				ctx->more->elts, ctx->more->arity, ctx->self, hp);
		ctx->more = ctx->more->alt;
		if (r != noval)
			return r;
	}

	ets_hash_data_t *data = tab->data;

	// iterate next-list
	assert(ctx->ent != 0);
	ctx->ref = &ctx->ent->next;
	ctx->ent = ctx->ent->next;

	// iterate buckets
	while (ctx->ent == 0 || ctx->ent->arity < 0)
	{
		ctx->buck++;
		if (ctx->buck >= ETS_HASH_BUCKETS)
		{
			ctx->ent = 0;
			return noval;
		}
		ctx->ref = &data->buckets[ctx->buck];
		ctx->ent = data->buckets[ctx->buck];
	}
	
	ctx->more = ctx->ent;
	goto more_run;
}

static void ets_hash_match_delete(ets_table_t *tab, ets_match_ctx_t *mc)
{
	ets_hash_match_ctx_t *ctx = (ets_hash_match_ctx_t *)mc;
	assert(ctx->ent != 0);
	tab->count--;	// something gonna be deleted

	if (tab->fixed > 0 && ctx->ent->alt == 0)
	{
		assert(ctx->more == 0);
		assert(ctx->ent->arity >= 0);
		ctx->ent->arity = -ctx->ent->arity;
		return;
	}

	// detach ctx->ent from next-list
	assert(*ctx->ref == ctx->ent);
	*ctx->ref = ctx->ent->next;

	// match_delete/select_delete do not use continuations -
	// leftovers is not an issue
	
	ets_hash_data_t *data = tab->data;

	// find object previous to ctx->more
	ets_hash_entry_t **tee = &ctx->ent;
	while (*tee != 0 && (*tee)->alt != ctx->more)
		tee = &(*tee)->alt;
	assert(*tee != 0);

	ets_hash_entry_t *freeme = *tee;
	*tee = freeme->alt;

	tab->total_alloc -= ALLOC_WSIZE(freeme->elts);
	ets_free(freeme->elts);

	freeme->next = data->free;
	data->free = freeme;
	data->nr_free++;

	// reattach to next-list
	if (ctx->ent != 0)
	{
		ctx->ent->next = *ctx->ref;
		*ctx->ref = ctx->ent;
	}
	else
	{
		// alt-list is gone - find the next
		ctx->ent = *ctx->ref;

		// iterate buckets
		while (ctx->ent == 0 || ctx->ent->arity < 0)
		{
			ctx->buck++;
			if (ctx->buck >= ETS_HASH_BUCKETS)
			{
				ctx->ent = 0;
				return;
			}
			ctx->ref = &data->buckets[ctx->buck];
			ctx->ent = data->buckets[ctx->buck];
		}
		
		ctx->more = ctx->ent;
	}
}

static term_t ets_hash_match_make_continuation(ets_table_t *tab,
		term_t match_spec, int limit, ets_match_ctx_t *mc, heap_t *hp)
{
	ets_hash_match_ctx_t *ctx = (ets_hash_match_ctx_t *)mc;

	// {Tid,MatchSpec,Limit,Key,LeftOvers}
	// (the first three elements must be present)
	
	if (ctx->ent == 0)
		return AEOT__;

	term_t leftovers_r = list_rev(ctx->leftovers, hp);
	while (ctx->more != 0)
	{
		term_t r = ets_match_spec_run(ctx->mspec,
				ctx->more->elts, ctx->more->arity, ctx->self, hp);
		if (r != noval)
			leftovers_r = heap_cons(hp, r, leftovers_r);
		ctx->more = ctx->more->alt;
	}

	term_t leftovers = list_rev(leftovers_r, hp);
	term_t key = ctx->ent->elts[tab->key_pos -1];

	assert(fits_int(limit));
	return heap_tuple5(hp, tab->tid, match_spec, tag_int(limit), key, leftovers);
}

static ets_match_ctx_t *ets_hash_match_use_continuation(ets_table_t *tab,
					ets_match_spec_t *ms, term_t continuation, term_t pid, heap_t *hp)
{
	// {Tid,MatchSpec,Limit,Key,LeftOvers}
	
	assert(is_tuple(continuation));
	uint32_t *p = peel_tuple(continuation);
	if (*p++ != 5)
		return 0;
	// 0: tid
	// 1: match_spec
	// 2: limit
	// 3: key
	// 4: leftovers
	
	// tid, match_spec, limit are validated by the caller
	//
	if (!is_list(p[4]))
		return 0;

	ets_hash_match_ctx_t *ctx = (ets_hash_match_ctx_t *)heap_tmp_buf(hp, sizeof(*ctx));

	ctx->mspec = ms;
	ctx->self = pid;
	ctx->leftovers = p[4];
	ctx->key_bound = 0;	//TODO

	ctx->ent = lookup_entry(p[3], tab, &ctx->buck);
	if (ctx->ent == 0)
		return 0;	
	ctx->more = 0;	// all remaing objects on alt-list are in leftovers

	return (ets_match_ctx_t *)ctx;
}

static void ets_hash_index_start(ets_hash_data_t *data, ets_hash_index_t *index)
{
	index->data = data;
	index->entry = data->buckets[0];
	index->buck = 0;
}

static ets_hash_entry_t *ets_hash_index_next(ets_hash_index_t *index)
{
	ets_hash_data_t *data = index->data;
	while (index->entry == 0)
	{
		index->buck++;
		if (index->buck >= data->nr_buckets)
			return 0;
		index->entry = data->buckets[index->buck];
	}
	ets_hash_entry_t *ent = index->entry;
	index->entry = index->entry->next;
	return ent;
}

//EOF
