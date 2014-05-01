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
#include "list_util.h"

#define ETS_ORDSET_EXT_SIZE		4096
#define ETS_ORDSET_MAX_SAVED	32
#define ETS_ORDSET_MAX_PATH		32

#define KEY_MATCH		100
#define NO_MATCH		99

typedef struct ordset_twig_t ordset_twig_t;
struct ordset_twig_t {
	int balance;
	int arity;
	term_t *elts;
	ordset_twig_t *left;
	ordset_twig_t *right;
};

typedef struct ets_ordset_data_t ets_ordset_data_t;
struct ets_ordset_data_t {
	ordset_twig_t *root;
	ordset_twig_t *free;
	int nr_free;
	memnode_t *nodes;
};

typedef struct ets_ordset_index_t ets_ordset_index_t;
struct ets_ordset_index_t {
	ordset_twig_t *saved[ETS_ORDSET_MAX_SAVED];
	int nr_saved;
	ordset_twig_t *next;
};

typedef struct ets_ordset_match_ctx_t ets_ordset_match_ctx_t;
struct ets_ordset_match_ctx_t {
	ets_match_spec_t *mspec;
	term_t self;
	term_t leftovers;
	int key_bound;
	union {
	struct {
		term_t prev_key;	// keep for match_delete
		ordset_twig_t *path[ETS_ORDSET_MAX_PATH];
		int path_len;
		ordset_twig_t *next;
		int dir;	// +1 or -1
	};
	struct {
	};
	};
};

static void ets_ordset_data_destroy(ets_table_t *tab);
static void ets_ordset_insert(ets_table_t *tab, term_t *elts, int arity);
static void ets_ordset_insert_many(ets_table_t *tab, term_t objs);
static term_t ets_ordset_lookup(ets_table_t *tab, term_t key, heap_t *hp);
static term_t ets_ordset_member(ets_table_t *tab, term_t key);
static term_t ets_ordset_first(ets_table_t *tab, heap_t *hp);
static term_t ets_ordset_last(ets_table_t *tab, heap_t *hp);
static term_t ets_ordset_next(ets_table_t *tab, term_t key, heap_t *hp);
static term_t ets_ordset_prev(ets_table_t *tab, term_t key, heap_t *hp);
static term_t ets_ordset_slot(ets_table_t *tab, int n, heap_t *hp);
static void ets_ordset_delete(ets_table_t *tab, term_t key);
static void ets_ordset_delete_object(ets_table_t *tab, term_t *elts, int arity);
static void ets_ordset_delete_all_objects(ets_table_t *tab);

static ets_match_ctx_t *ets_ordset_match_first(ets_table_t *tab,
									ets_match_spec_t *ms, term_t pid, heap_t *hp);
static ets_match_ctx_t *ets_ordset_match_last(ets_table_t *tab,
									ets_match_spec_t *ms, term_t pid, heap_t *hp);
static term_t ets_ordset_match_next(ets_table_t *tab,
									ets_match_ctx_t *ctx, heap_t *hp);
static term_t ets_ordset_match_prev(ets_table_t *tab,
									ets_match_ctx_t *ctx, heap_t *hp);
static void ets_ordset_match_delete(ets_table_t *tab, ets_match_ctx_t *ctx);
static term_t ets_ordset_match_make_continuation(ets_table_t *tab,
		term_t match_spec, int limit, ets_match_ctx_t *ctx, heap_t *hp);
static ets_match_ctx_t *ets_ordset_match_use_continuation(ets_table_t *tab,
					ets_match_spec_t *ms, term_t continuation, term_t pid, heap_t *hp);

static void split_node_into_twigs(ets_ordset_data_t *data, memnode_t *node);

static void ets_ordset_index_start(ets_ordset_data_t *data, ets_ordset_index_t *index);
static ordset_twig_t *ets_ordset_index_next(ets_ordset_index_t *index);

static int insert_element(ordset_twig_t **ref,
				int key_pos, term_t *elts, int arity, ets_table_t *tab);
static int rebalance_left(ordset_twig_t **ref);
static int rebalance_right(ordset_twig_t **ref);

static int delete_by_key(ordset_twig_t **ref,
					int key_pos, term_t key, ets_table_t *tab);
static int delete_object(ordset_twig_t **ref,
					term_t *elts, int arity, ets_table_t *tab);
static int replace_successor(ordset_twig_t *match,
		ordset_twig_t **ref, int key_pos, term_t key, ets_table_t *tab);
static int replace_predecessor(ordset_twig_t *match,
		ordset_twig_t **ref, int key_pos, term_t key, ets_table_t *tab);

static int lookup_by_index(ordset_twig_t *twig,
		int index, int needle, ordset_twig_t **match);

ets_virt_t ets_ordset_virt = {
	.insert = ets_ordset_insert,
	.insert_many = ets_ordset_insert_many,
	.lookup = ets_ordset_lookup,
	.member = ets_ordset_member,
	.first = ets_ordset_first,
	.last = ets_ordset_last,
	.next = ets_ordset_next,
	.prev = ets_ordset_prev,
	.slot = ets_ordset_slot,
	.delete = ets_ordset_delete,
	.delete_object = ets_ordset_delete_object,
	.delete_all_objects = ets_ordset_delete_all_objects,
	.data_destroy = ets_ordset_data_destroy,
	.match_first = ets_ordset_match_first,
	.match_last = ets_ordset_match_last,
	.match_next = ets_ordset_match_next,
	.match_prev = ets_ordset_match_prev,
	.match_delete = ets_ordset_match_delete,
	.match_make_continuation = ets_ordset_match_make_continuation,
	.match_use_continuation = ets_ordset_match_use_continuation
};

void *ets_ordset_data_make(memnode_t *node)
{
	ets_ordset_data_t *data = (ets_ordset_data_t *)node->starts;
	node->starts += WSIZE(ets_ordset_data_t);
	//data->root = 0;
	//data->free = 0;
	//data->nr_free = 0;
	//data->nodes = 0;
	split_node_into_twigs(data, node);
	return data;
}

static void ets_ordset_data_destroy(ets_table_t *tab)
{
	ets_ordset_data_t *data = tab->data;

	ets_ordset_index_t index;
	ets_ordset_index_start(data, &index);
	ordset_twig_t *twig;
	while ((twig = ets_ordset_index_next(&index)) != 0)
		ets_free(twig->elts);	// total_alloc not updated

	nfree_chain(data->nodes);
}

static void ets_ordset_insert(ets_table_t *tab, term_t *elts, int arity)
{
	assert(arity >= tab->key_pos);
	ets_ordset_data_t *data = tab->data;

	if (data->free == 0)
	{
		// EXCEPTION POSSIBLE
		memnode_t *node = nalloc(ETS_ORDSET_EXT_SIZE - sizeof(memnode_t));
		node->next = data->nodes;
		data->nodes = node;

		split_node_into_twigs(data, node);
		assert(data->free != 0);
	}

	// EXCEPTION POSSIBLE
	uint32_t *new_elts = ets_marshal_object(elts, arity);
	tab->total_alloc += ALLOC_WSIZE(new_elts);

	if (insert_element(&data->root,
			tab->key_pos, new_elts, arity, tab) != KEY_MATCH)
		tab->count++;
}

static void ets_ordset_insert_many(ets_table_t *tab, term_t objs)
{
	ets_ordset_data_t *data = tab->data;
	assert(is_list(objs));
	int count = list_len(objs);
	assert(count >= 0);

	if (count == 0)
		return;

	while (data->nr_free < count)
	{
		// EXCEPTION POSSIBLE
		memnode_t *node = nalloc(ETS_ORDSET_EXT_SIZE - sizeof(memnode_t));
		node->next = data->nodes;
		data->nodes = node;

		split_node_into_twigs(data, node);
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
			// rollback allocations
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
		if (insert_element(&data->root,
				tab->key_pos, obj->elts, obj->arity, tab) != KEY_MATCH);
			tab->count++;
		obj++;
	}
	tab->total_alloc -= ALLOC_WSIZE(my_objects);
	ets_free((uint32_t *)my_objects);
}

static term_t ets_ordset_lookup(ets_table_t *tab, term_t key, heap_t *hp)
{
	ets_ordset_data_t *data = tab->data;

	ordset_twig_t *twig = data->root;
	while (twig != 0)
	{
		term_t key1 = twig->elts[tab->key_pos -1];
		if (is_term_smaller(key, key1))
			twig = twig->left;
		else if (key == key1 || are_terms_equal(key, key1, 0))	// ==
		{
			// EXCEPTION POSSIBLE
			term_t t = ets_demarshal_object(twig->elts, twig->arity, hp);
			// EXCEPTION POSSIBLE
			return heap_cons(hp, t, nil);
		}
		else
			twig = twig->right;
	}

	return nil;
}

static term_t ets_ordset_member(ets_table_t *tab, term_t key)
{
	ets_ordset_data_t *data = tab->data;

	ordset_twig_t *twig = data->root;
	while (twig != 0)
	{
		term_t key1 = twig->elts[tab->key_pos -1];
		if (is_term_smaller(key, key1))
			twig = twig->left;
		else if (key == key1 || are_terms_equal(key, key1, 0))	// ==
			return A_TRUE;
		else
			twig = twig->right;
	}

	return A_FALSE;
}

static term_t ets_ordset_first(ets_table_t *tab, heap_t *hp)
{
	ets_ordset_data_t *data = tab->data;
	if (data->root == 0)
		return AEOT__;

	ordset_twig_t *twig = data->root;
	while (twig->left != 0)
		twig = twig->left;

	term_t new_key = twig->elts[tab->key_pos -1];
	int x = heap_copy_terms_N(hp, &new_key, 1);
	assert(x != -TOO_DEEP);		// heap_copy_terms is still recursive
	if (x < 0)
	{
		assert(x == -NO_MEMORY);
		no_memory_signal();
	}
	return new_key;
}

static term_t ets_ordset_last(ets_table_t *tab, heap_t *hp)
{
	ets_ordset_data_t *data = tab->data;
	if (data->root == 0)
		return AEOT__;

	ordset_twig_t *twig = data->root;
	while (twig->right != 0)
		twig = twig->right;

	term_t new_key = twig->elts[tab->key_pos -1];
	int x = heap_copy_terms_N(hp, &new_key, 1);
	assert(x != -TOO_DEEP);		// heap_copy_terms is still recursive
	if (x < 0)
	{
		assert(x == -NO_MEMORY);
		no_memory_signal();
	}
	return new_key;
}

static term_t ets_ordset_next(ets_table_t *tab, term_t key, heap_t *hp)
{
	ets_ordset_data_t *data = tab->data;

	ordset_twig_t *twig = data->root;
	ordset_twig_t *next_right = 0;
	while (twig != 0)
	{
		term_t key1 = twig->elts[tab->key_pos -1];
		if (is_term_smaller(key, key1))
		{
			next_right = twig;
			twig = twig->left;
		}
		else if (is_term_smaller(key1, key))
			twig = twig->right;
		else
		{
			twig = twig->right;
			if (twig != 0)
				while (twig->left != 0)
					twig = twig->left;
			break;
		}
	}

	if (twig == 0)
	   twig = next_right;

	if (twig == 0)
		return AEOT__;

	term_t new_key = twig->elts[tab->key_pos -1];
	int x = heap_copy_terms_N(hp, &new_key, 1);
	assert(x != -TOO_DEEP);		// heap_copy_terms is still recursive
	if (x < 0)
	{
		assert(x == -NO_MEMORY);
		no_memory_signal();
	}
	return new_key;
}

static term_t ets_ordset_prev(ets_table_t *tab, term_t key, heap_t *hp)
{
	ets_ordset_data_t *data = tab->data;

	ordset_twig_t *twig = data->root;
	ordset_twig_t *next_left = 0;
	while (twig != 0)
	{
		term_t key1 = twig->elts[tab->key_pos -1];
		if (is_term_smaller(key, key1))
			twig = twig->left;
		else if (is_term_smaller(key1, key))
		{
			next_left = twig;
			twig = twig->right;
		}
		else
		{
			twig = twig->left;
			if (twig != 0)
				while (twig->right != 0)
					twig = twig->right;
			break;
		}
	}

	if (twig == 0)
	   twig = next_left;

	if (twig == 0)
		return AEOT__;

	term_t new_key = twig->elts[tab->key_pos -1];
	int x = heap_copy_terms_N(hp, &new_key, 1);
	assert(x != -TOO_DEEP);		// heap_copy_terms is still recursive
	if (x < 0)
	{
		assert(x == -NO_MEMORY);
		no_memory_signal();
	}
	return new_key;
}

static term_t ets_ordset_slot(ets_table_t *tab, int n, heap_t *hp)
{
	ets_ordset_data_t *data = tab->data;

	if (n < 0 || n > tab->count)
		return noval;
	if (n == tab->count)
		return AEOT__;

	//
	// NB: the algorithm is O(N)
	//
	// To have better complexity, the size of a subtree must be stored at nodes;
	// not justified given relative infrequent use of slot().
	//

	ordset_twig_t *match;
	UNUSED int r = lookup_by_index(data->root, 0, n, &match);
	assert(r == -1);	// match found

	// EXCEPTION POSSIBLE
	term_t t = ets_demarshal_object(match->elts, match->arity, hp);
	// EXCEPTION POSSIBLE
	return heap_cons(hp, t, nil);
}

#ifdef LING_DEBUG
int __avl_tree_validate(ordset_twig_t *tw);
#endif

static void ets_ordset_delete(ets_table_t *tab, term_t key)
{
	ets_ordset_data_t *data = tab->data;
	if (delete_by_key(&data->root, tab->key_pos, key, tab) != NO_MATCH)
		tab->count--;

#ifdef LING_DEBUG
	assert(__avl_tree_validate(data->root) >= 0);
#endif
}

static void ets_ordset_delete_object(ets_table_t *tab, term_t *elts, int arity)
{
	ets_ordset_data_t *data = tab->data;
	if (delete_object(&data->root, elts, arity, tab) != NO_MATCH)
		tab->count--;
}

static void ets_ordset_delete_all_objects(ets_table_t *tab)
{	
	ets_ordset_data_t *data = tab->data;

	ets_ordset_index_t index;
	ets_ordset_index_start(data, &index);
	ordset_twig_t *twig = ets_ordset_index_next(&index);
	while (twig != 0)
	{
		ordset_twig_t *freeme = twig;
		twig = ets_ordset_index_next(&index);

		tab->total_alloc -= ALLOC_WSIZE(freeme->elts);
		ets_free(freeme->elts);

		freeme->right = data->free;
		data->free = freeme;
		data->nr_free++;
	}

	data->root = 0;
	tab->count = 0;
}

static void ets_ordset_index_start(ets_ordset_data_t *data, ets_ordset_index_t *index)
{
	index->nr_saved = 0;
	index->next = data->root;
}

static ordset_twig_t *ets_ordset_index_next(ets_ordset_index_t *index)
{
	ordset_twig_t *twig = index->next;
	if (twig == 0)
		return 0;

	if (twig->left != 0 && twig->right != 0)
	{
		assert(index->nr_saved < ETS_ORDSET_MAX_SAVED);
		index->saved[index->nr_saved++] = twig->left;
		index->next = twig->right;
	}
	else if (twig->left == 0 && twig->right == 0)
	{
		if (index->nr_saved > 0)
			index->next = index->saved[--index->nr_saved];
		else
			index->next = 0;
	}
	else
	{
		if (twig->left != 0)
			index->next = twig->left;
		else
			index->next = twig->right;
	}

	return twig;
}

static void split_node_into_twigs(ets_ordset_data_t *data, memnode_t *node)
{
	ordset_twig_t *twig = (ordset_twig_t *)node->starts;
	while (twig +1 <= (ordset_twig_t *)node->ends)
	{
		twig->right = data->free;	// right ref is used for linking free twigs
		data->free = twig;
		data->nr_free++;
		twig++;
	}
	node->starts = (uint32_t *)twig;
}

static int insert_element(ordset_twig_t **ref,
				int key_pos, term_t *elts, int arity, ets_table_t *tab)
{
	ets_ordset_data_t *data = tab->data;

	ordset_twig_t *twig = *ref;
	if (twig == 0)
	{
		assert(data->free != 0);
		ordset_twig_t *new_twig = data->free;
		data->free = new_twig->right;
		data->nr_free--;

		new_twig->balance = 0;
		new_twig->arity = arity;
		new_twig->elts = elts;
		new_twig->left = 0;
		new_twig->right = 0;

		*ref = new_twig;
		return 1;
	}

	term_t key = elts[key_pos -1];
	term_t key1 = twig->elts[key_pos -1];
	if (is_term_smaller(key, key1))
	{
		int dh = insert_element(&twig->left, key_pos, elts, arity, tab);
		if (dh == KEY_MATCH || dh == 0)
			return dh;

		assert(dh == 1);
		twig->balance -= dh;
		if (twig->balance == -2)
		{
			rebalance_left(ref);
			return 0;	// AVL-tree requires only a single rebalance on insert
		}
		return (twig->balance == 0) ?0 :1;
	}
	if (key != key1 && !are_terms_equal(key, key1, 0))		// ==
	{
		int dh = insert_element(&twig->right, key_pos, elts, arity, tab);
		if (dh == KEY_MATCH || dh == 0)
			return dh;

		assert(dh == 1);
		twig->balance += dh;
		if (twig->balance == 2)
		{
			rebalance_right(ref);
			return 0;	// AVL-tree requires only a single rebalance on insert
		}

		return (twig->balance == 0) ?0 :1;
	}
	else
	{
		// keys match - replace
		tab->total_alloc -= ALLOC_WSIZE(twig->elts);
		ets_free(twig->elts);

		twig->arity = arity;
		twig->elts = elts;
		return KEY_MATCH;
	}
}

static int delete_by_key(ordset_twig_t **ref,
					int key_pos, term_t key, ets_table_t *tab)
{
	ets_ordset_data_t *data = tab->data;
	ordset_twig_t *twig = *ref;
	if (twig == 0)
		return NO_MATCH;

	term_t key1 = twig->elts[key_pos -1];
	int dh;
	if (is_term_smaller(key, key1))
	{
		dh = delete_by_key(&twig->left, key_pos, key, tab);
		if (dh == NO_MATCH)
			return dh;
		twig->balance -= dh;
		if (twig->balance > 0)
			dh = 0;
	}
	else if (key != key1 && !are_terms_equal(key, key1, 0))		// ==
	{
		dh = delete_by_key(&twig->right, key_pos, key, tab);
		if (dh == NO_MATCH)
			return dh;
		twig->balance += dh;
		if (twig->balance < 0)
			dh = 0;
	}
	else
	{
		tab->total_alloc -= ALLOC_WSIZE(twig->elts);
		ets_free(twig->elts);

		if (twig->right == 0 && twig->left == 0)
		{
			*ref = 0;

			twig->right = data->free;
			data->free = twig;
			data->nr_free++;

			return -1;
		}

		if (twig->balance >= 0)
		{
			dh = replace_successor(twig, &twig->right, key_pos, key, tab);
			twig->balance += dh;
			if (twig->balance < 0)
				dh = 0;
		}
		else
		{
			dh = replace_predecessor(twig, &twig->left, key_pos, key, tab);
			twig->balance -= dh;
			if (twig->balance > 0)
				dh = 0;
		}
	}

	if (twig->balance == 2)
		return dh +rebalance_right(ref);
	else if (twig->balance == -2)
		return dh +rebalance_left(ref);

	return dh;
}

static int delete_object(ordset_twig_t **ref,
				term_t *elts, int arity, ets_table_t *tab)
{
	ets_ordset_data_t *data = tab->data;
	ordset_twig_t *twig = *ref;
	if (twig == 0)
		return NO_MATCH;

	int key_pos = tab->key_pos;
	term_t key = elts[key_pos -1];
	term_t key1 = twig->elts[key_pos -1];
	int dh;
	if (is_term_smaller(key, key1))
	{
		dh = delete_object(&twig->left, elts, arity, tab);
		if (dh == NO_MATCH)
			return dh;
		twig->balance -= dh;
		if (twig->balance > 0)
			dh = 0;
	}
	else if (key != key1 && !are_terms_equal(key, key1, 0))		// ==
	{
		dh = delete_object(&twig->right, elts, arity, tab);
		if (dh == NO_MATCH)
			return dh;
		twig->balance += dh;
		if (twig->balance < 0)
			dh = 0;
	}
	else
	{
		if (twig->arity != arity)
			return NO_MATCH;

		for (int i = 0; i < arity; i++)
			if (i != key_pos -1 && twig->elts[i] != elts[i] &&
							!are_terms_equal(twig->elts[i], elts[i], 1))	// =:=
				return NO_MATCH;

		tab->total_alloc -= ALLOC_WSIZE(twig->elts);
		ets_free(twig->elts);

		if (twig->right == 0 && twig->left == 0)
		{
			*ref = 0;

			twig->right = data->free;
			data->free = twig;
			data->nr_free++;

			return -1;
		}

		if (twig->balance >= 0)
		{
			dh = replace_successor(twig, &twig->right, key_pos, key, tab);
			twig->balance += dh;
		}
		else
		{
			dh = replace_predecessor(twig, &twig->left, key_pos, key, tab);
			twig->balance -= dh;
		}
	}

	if (twig->balance == 2)
		return dh +rebalance_right(ref);
	else if (twig->balance == -2)
		return dh +rebalance_left(ref);

	return dh;
}

static int replace_successor(ordset_twig_t *match,
		ordset_twig_t **ref, int key_pos, term_t key, ets_table_t *tab)
{
	ets_ordset_data_t *data = tab->data;

	ordset_twig_t *twig = *ref;
	assert(twig != 0);
	if (twig->left == 0)
	{
		match->elts = twig->elts;
		match->arity = twig->arity;

		ordset_twig_t *freeme = twig;
		if (twig->right)
		{
			freeme = twig->right;
			twig->elts = freeme->elts;
			twig->arity = freeme->arity;
			twig->right = 0;
			twig->balance = 0;
		}
		else
			*ref = 0;

		freeme->right = data->free;
		data->free = freeme;
		data->nr_free++;

		return -1;
	}
	else
	{
		int dh = replace_successor(match, &twig->left, key_pos, key, tab);
		twig->balance -= dh;
		if (twig->balance > 0)
			dh = 0;
		if (twig->balance == 2)
			return dh +rebalance_right(ref);

		return dh;
	}
}

static int replace_predecessor(ordset_twig_t *match,
		ordset_twig_t **ref, int key_pos, term_t key, ets_table_t *tab)
{
	ets_ordset_data_t *data = tab->data;

	ordset_twig_t *twig = *ref;
	assert(twig != 0);
	if (twig->right == 0)
	{
		match->elts = twig->elts;
		match->arity = twig->arity;

		ordset_twig_t *freeme = twig;
		if (twig->left)
		{
			freeme = twig->left;
			twig->elts = freeme->elts;
			twig->arity = freeme->arity;
			twig->left = 0;
			twig->balance = 0;
		}
		else
			*ref = 0;

		freeme->right = data->free;
		data->free = freeme;
		data->nr_free++;

		return -1;
	}
	else
	{
		int dh = replace_predecessor(match, &twig->right, key_pos, key, tab);
		twig->balance += dh;
		if (twig->balance < 0)
			dh = 0;
		if (twig->balance == -2)
			return dh +rebalance_left(ref);

		return dh;
	}
}

static int rotate_right(ordset_twig_t **ref)
{
	ordset_twig_t *z = *ref;
	assert(z != 0);
	ordset_twig_t *y = z->left;
	assert(y != 0);

	*ref = y;
	z->left = y->right;
	y->right = z;

	// magic, magic
	int bz, by;
	if (y->balance <= 0)
	{
		bz = z->balance -y->balance +1;
		by = (bz <= 0)
			?y->balance +1
			:z->balance +2;
	}
	else
	{
		bz = z->balance +1;
		by = (bz <= 0)
			?y->balance +1
			:z->balance + y->balance +2;
	}

	int dh;
	if (z->balance >= 0 && by >= 0)
		dh = by -bz -y->balance;
	else if (z->balance >= 0)
		dh = -bz -y->balance;
	else if (by < 0)
		dh = z->balance -bz -y->balance;
	else
		dh = by + z->balance -bz -y->balance;

	y->balance = by;
	z->balance = bz;

	return dh;
}

static int rotate_left(ordset_twig_t **ref)
{
	ordset_twig_t *z = *ref;
	assert(z != 0);
	ordset_twig_t *y = z->right;
	assert(y != 0);

	*ref = y;
	z->right = y->left;
	y->left = z;

	// magic, magic
	int bz, by;
	if (y->balance >= 0)
	{
		bz = z->balance -y->balance -1;
		by = (bz >= 0)
			?y->balance -1
			:z->balance -2;
	}
	else
	{
		bz = z->balance -1;
		by = (bz >= 0)
			?y->balance -1
			:z->balance + y->balance -2;
	}

	int dh;
	if (z->balance <= 0 && by <= 0)
		dh = -by +bz +y->balance;
	else if (z->balance <= 0)
		dh = bz +y->balance;
	else if (by > 0)
		dh = -z->balance +bz +y->balance;
	else
		dh = -by -z->balance +bz +y->balance;

	y->balance = by;
	z->balance = bz;

	return dh;
}

static int rebalance_left(ordset_twig_t **ref)
{
	ordset_twig_t *z = *ref;
	assert(z != 0);
	assert(z->balance == -2);

	ordset_twig_t *y = z->left;
	assert(y != 0);
	//assert(y->balance != 0);

	if (y->balance == 1)
		z->balance -= rotate_left(&z->left);

	return rotate_right(ref);
}

static int rebalance_right(ordset_twig_t **ref)
{
	ordset_twig_t *z = *ref;
	assert(z != 0);
	assert(z->balance == 2);

	ordset_twig_t *y = z->right;
	assert(y != 0);
	//assert(y->balance != 0);

	if (y->balance == -1)
		z->balance += rotate_right(&z->right);

	return rotate_left(ref);
}

static int lookup_by_index(ordset_twig_t *twig,
		int index, int needle, ordset_twig_t **match)
{
	if (twig->left)
	{
		index = lookup_by_index(twig->left, index, needle, match);
		if (index == -1)
			return -1;
	}
	if (index == needle)
	{
		*match = twig;
		return -1;
	}
	index++;
	if (twig->right == 0)
		return index;
	return lookup_by_index(twig->right, index, needle, match);
}

static ets_match_ctx_t *ets_ordset_match_first(ets_table_t *tab,
									ets_match_spec_t *ms, term_t pid, heap_t *hp)
{
	ets_ordset_data_t *data = tab->data;

	ets_ordset_match_ctx_t *ctx = (ets_ordset_match_ctx_t *)heap_tmp_buf(hp, sizeof(*ctx));
	ctx->mspec = ms;
	ctx->self = pid;
	ctx->leftovers = nil;

	//ctx->key_bound = ets_match_key_bound(ms, tab->key_pos)
	ctx->key_bound = 0;
	if (ctx->key_bound)
	{
		//TODO
		not_implemented("ets ordset match first (key bound)");
	}
	else
	{
		ctx->prev_key = noval;
		ctx->path_len = 0;
		ordset_twig_t *twig = data->root;
		if (twig != 0)
		{
			while (twig->left != 0)
			{
				assert(ctx->path_len < ETS_ORDSET_MAX_PATH);
				ctx->path[ctx->path_len++] = twig;
				twig = twig->left;
			}
		}
		ctx->next = twig;
		ctx->dir = 1;
	}

	return (ets_match_ctx_t *)ctx;
}

static ets_match_ctx_t *ets_ordset_match_last(ets_table_t *tab,
									ets_match_spec_t *ms, term_t pid, heap_t *hp)
{
	ets_ordset_data_t *data = tab->data;

	ets_ordset_match_ctx_t *ctx = (ets_ordset_match_ctx_t *)heap_tmp_buf(hp, sizeof(*ctx));
	ctx->mspec = ms;
	ctx->self = pid;
	ctx->leftovers = nil;

	//ctx->key_bound = ets_match_key_bound(ms, tab->key_pos)
	ctx->key_bound = 0;
	if (ctx->key_bound)
	{
		//TODO
		not_implemented("ets ordset match first (key bound)");
	}
	else
	{
		ctx->prev_key = noval;
		ctx->path_len = 0;
		ordset_twig_t *twig = data->root;
		if (twig != 0)
		{
			while (twig->right != 0)
			{
				assert(ctx->path_len < ETS_ORDSET_MAX_PATH);
				ctx->path[ctx->path_len++] = twig;
				twig = twig->right;
			}
		}
		ctx->next = twig;
		ctx->dir = -1;
	}

	return (ets_match_ctx_t *)ctx;
}

static term_t ets_ordset_match_next(ets_table_t *tab,
									ets_match_ctx_t *mc, heap_t *hp)
{
	ets_ordset_match_ctx_t *ctx = (ets_ordset_match_ctx_t *)mc;

	if (ctx->next == 0)
		return noval;
	term_t result;
next_run:
	result = ets_match_spec_run(ctx->mspec,
			ctx->next->elts, ctx->next->arity, ctx->self, hp);
	ctx->prev_key = ctx->next->elts[tab->key_pos -1];

	ordset_twig_t *tw = ctx->next;
	if (tw->right != 0)
	{
		assert(ctx->path_len < ETS_ORDSET_MAX_PATH);
		ctx->path[ctx->path_len++] = tw;
		tw = tw->right;

		while (tw->left != 0)
		{
			assert(ctx->path_len < ETS_ORDSET_MAX_PATH);
			ctx->path[ctx->path_len++] = tw;
			tw = tw->left;
		}
		ctx->next = tw;
		if (result == noval)
			goto next_run;
	}
	else
	{
		while (ctx->path_len > 0)
		{
			ordset_twig_t *src = tw;
			tw = ctx->path[--ctx->path_len];
			if (tw->left == src)
			{
				ctx->next = tw;
				if (result == noval)
					goto next_run;
				return result;
			}
		}
		ctx->next = 0;
	}
	return result;
}

static term_t ets_ordset_match_prev(ets_table_t *tab,
									ets_match_ctx_t *mc, heap_t *hp)
{
	ets_ordset_match_ctx_t *ctx = (ets_ordset_match_ctx_t *)mc;

	if (ctx->next == 0)
		return noval;

	term_t result;
next_run:
	result = ets_match_spec_run(ctx->mspec,
			ctx->next->elts, ctx->next->arity, ctx->self, hp);
	ctx->prev_key = ctx->next->elts[tab->key_pos -1];
	
	ordset_twig_t *tw = ctx->next;
	if (tw->left != 0)
	{
		assert(ctx->path_len < ETS_ORDSET_MAX_PATH);
		ctx->path[ctx->path_len++] = tw;
		tw = tw->left;

		while (tw->right != 0)
		{
			assert(ctx->path_len < ETS_ORDSET_MAX_PATH);
			ctx->path[ctx->path_len++] = tw;
			tw = tw->right;
		}
		ctx->next = tw;
		if (result == noval)
			goto next_run;
	}
	else
	{
		while (ctx->path_len > 0)
		{
			ordset_twig_t *src = tw;
			tw = ctx->path[--ctx->path_len];
			if (tw->right == src)
			{
				ctx->next = tw;
				if (result == noval)
					goto next_run;
				return result;
			}
		}
		ctx->next = 0;
	}
	return result;
}

static void ets_ordset_match_delete(ets_table_t *tab, ets_match_ctx_t *mc)
{
	ets_ordset_match_ctx_t *ctx = (ets_ordset_match_ctx_t *)mc;
	assert(ctx->prev_key != noval);

	int key_pos = tab->key_pos;
	term_t key = noval;
	if (ctx->next != 0)
		key = ctx->next->elts[key_pos -1];

	ets_ordset_data_t *data = tab->data;
	if (delete_by_key(&data->root, key_pos, ctx->prev_key, tab) != NO_MATCH)
		tab->count--;

	// the saved path is most probably invalidated by delete - refresh
	if (ctx->next == 0)
		return;

	ctx->path_len = 0;
	ordset_twig_t *tw = data->root;
	while (1)
	{
		assert(tw != 0);
		term_t key1 = tw->elts[tab->key_pos -1];
		if (is_term_smaller(key, key1))
		{
			assert(ctx->path_len < ETS_ORDSET_MAX_PATH);
			ctx->path[ctx->path_len++] = tw;
			tw = tw->left;
		}
		else if (key == key1 || are_terms_equal(key, key1, 0))	// ==
		{
			ctx->next = tw;
			return;
		}
		else
		{
			assert(ctx->path_len < ETS_ORDSET_MAX_PATH);
			ctx->path[ctx->path_len++] = tw;
			tw = tw->right;
		}
	}
}

static term_t ets_ordset_match_make_continuation(ets_table_t *tab,
		term_t match_spec, int limit, ets_match_ctx_t *mc, heap_t *hp)
{
	ets_ordset_match_ctx_t *ctx = (ets_ordset_match_ctx_t *)mc;

	// {Tid,MatchSpec,Limit,Key,LeftOvers,Dir}
	// (the first three elements must be present)
	
	if (ctx->next == 0)
		return AEOT__;

	term_t key = ctx->next->elts[tab->key_pos -1];
	term_t leftovers = nil;		//TODO: needed for key bound case
	
	assert(fits_int(limit));
	return heap_tuple6(hp, tab->tid,
		   				   match_spec,
						   tag_int(limit),
						   key,
						   leftovers,
						   tag_int(ctx->dir)); 
}

static ets_match_ctx_t *ets_ordset_match_use_continuation(ets_table_t *tab,
					ets_match_spec_t *ms, term_t continuation, term_t pid, heap_t *hp)
{
	// {Tid,MatchSpec,Limit,Key,LeftOvers,Dir}
	
	assert(is_tuple(continuation));
	uint32_t *p = peel_tuple(continuation);
	if (*p++ != 6)
		return 0;
	// 0: tid
	// 1: match_spec
	// 2: limit
	// 3: key
	// 4: leftovers
	// 5: dir
	
	// tid, match_spec, limit are validated by the caller
	//
	if (!is_list(p[4]) || (p[5] != tag_int(1) && p[5] != tag_int(-1)))
		return 0l;

	ets_ordset_data_t *data = tab->data;
	ets_ordset_match_ctx_t *ctx = (ets_ordset_match_ctx_t *)heap_tmp_buf(hp, sizeof(*ctx));

	ctx->mspec = ms;
	ctx->self = pid;
	ctx->leftovers = p[4];
	ctx->key_bound = 0;	//TODO

	ctx->prev_key = noval;	// prev_key relevant for match_delete/select_delete and
							// these do not use continuations
	ctx->path_len = 0;
	ctx->dir = int_value(p[5]);

	term_t key = p[3];

	ordset_twig_t *tw = data->root;
	ordset_twig_t *next_left = 0;
	ordset_twig_t *next_right = 0;

	while (tw != 0)
	{
		term_t key1 = tw->elts[tab->key_pos -1];
		if (is_term_smaller(key, key1))
		{
			next_right = tw;
			assert(ctx->path_len < ETS_ORDSET_MAX_PATH);
			ctx->path[ctx->path_len++] = tw;
			tw = tw->left;
		}
		else if (key == key1 || are_terms_equal(key, key1, 0))	// ==
			break;
		else
		{
			next_left = tw;
			assert(ctx->path_len < ETS_ORDSET_MAX_PATH);
			ctx->path[ctx->path_len++] = tw;
			tw = tw->right;
		}
	}
	if (tw == 0)
		tw = (ctx->dir == 1) ?next_right :next_left;

	ctx->next = tw;
	return (ets_match_ctx_t *)ctx;
}

#ifdef LING_DEBUG
void __print_ind_twig(int ind, int key_pos, ordset_twig_t *twig)
{
	if (twig == 0)
		printk("%*snil\n", ind, "");
	else
	{
		__print_ind_twig(ind +4, key_pos, twig->right);
		printk("%*s%pt [%d] T/%d\n", ind, "", T(twig->elts[key_pos -1]), twig->balance, twig->arity);
		__print_ind_twig(ind +4, key_pos, twig->left);
	}
}

void __print_ordset(ets_table_t *tab)
{
	assert(tab->type == A_ORDERED_SET);
	ets_ordset_data_t *data = tab->data;
	__print_ind_twig(0, tab->key_pos, data->root);
}

int __avl_tree_validate(ordset_twig_t *tw)
{
	if (tw == 0)
		return 0;
	int lh = __avl_tree_validate(tw->left);
	if (lh < 0)
		return -1;
	int rh = __avl_tree_validate(tw->right);
	if (rh < 0)
		return -1;
	if (tw->balance != rh - lh)
		return -1;
	if (tw->balance > 1 || tw->balance < -1)
		return -1;
	return (lh > rh) ?lh+1 :rh+1;
}
	
void __avl_tree_is_valid(ets_table_t *tab)
{
	assert(tab->type == A_ORDERED_SET);
	ets_ordset_data_t *data = tab->data;

	if (__avl_tree_validate(data->root) >= 0)
		printk("AVL-tree ok\n");
	else
		printk("*** error: corrupted or not an AVL-tree\n");
}

#endif

//EOF
