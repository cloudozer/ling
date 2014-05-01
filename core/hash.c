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

/**
 *
 *
 *
 */

#include "hash.h"

#include <stdint.h>

#include "ling_common.h"

#include "nalloc.h"
#include "string.h"
#include "limits.h"

#define HASH_BUCKETS	32
#define HASH_NODE_SIZE	4096

struct hash_entry_t {
	hash_entry_t *next;
	uint32_t hash;
	const void *key;
	int klen;
	void *val;
};

struct hash_t {
	memnode_t *nodes;
	hash_entry_t **buckets;
	int nr_buckets;
	int count;
	hash_entry_t *free;
};

typedef memnode_t *(*alloc_func_t)(int size);
static int hash_set_internal(hash_t *ht, const void *key, int klen, void *val, alloc_func_t alloc_func);
static uint32_t digest(const unsigned char *key, int klen);

hash_t *hash_make(void)
{
	memnode_t *node = nalloc(HASH_NODE_SIZE - sizeof(memnode_t));
	if (node == 0)
		return 0;

	// Initial node layout:
	//
	// memnode_t
	// hash_t
	// hash_entry_t *[nr_buckets]
	// hash_entry_t [] (free)
	//

	hash_t *ht = (hash_t *)node->starts;
	ht->nodes = node;

	hash_entry_t **buckets = (void *)ht + sizeof(*ht);
	ht->buckets = buckets;
	ht->nr_buckets = HASH_BUCKETS;
	ht->count = 0;

	ht->free = 0;

	for (int i = 0; i < HASH_BUCKETS; i++)
		buckets[i] = 0;

	hash_entry_t *entry = (void *)(buckets + HASH_BUCKETS);
	while ((void *)node->ends - (void *)entry >= sizeof(hash_entry_t))
	{
		entry->next = ht->free;
		ht->free = entry;
		entry++;
	}

	return ht;
}

int hash_set(hash_t *ht, const void *key, int klen, void *val)
{
	return hash_set_internal(ht, key, klen, val, nalloc);
}

int hash_set_N(hash_t *ht, const void *key, int klen, void *val)
{
	return hash_set_internal(ht, key, klen, val, nalloc_N);
}

static int hash_set_internal(hash_t *ht, const void *key, int klen, void *val, alloc_func_t alloc_func)
{
	assert(ht != 0);
	assert(key != 0);
	assert(klen > 0);

	uint32_t hash = digest(key, klen);

	int index = hash & (HASH_BUCKETS-1);
	hash_entry_t *entry = ht->buckets[index];
	hash_entry_t **ref = &ht->buckets[index];
	while (entry != 0)
	{
		if (entry->hash == hash &&
			entry->klen == klen && memcmp(key, entry->key, klen) == 0)
		{
			//key exists - update (or delete)
			if (val == 0)
			{
				//delete
				*ref = entry->next;

				entry->next = ht->free;
				ht->free = entry;
			}
			else
			{
				//update
				entry->val = val;
			}
			return 0;
		}
		ref = &entry->next;
		entry = entry->next;
	}

	if (ht->free == 0)
	{
		//no more free entries - allocate a new node
		memnode_t *node = alloc_func(HASH_NODE_SIZE - sizeof(memnode_t));
		if (node == 0)
			return -NO_MEMORY;

		node->next = ht->nodes;
		ht->nodes = node;

		entry = (hash_entry_t *)node->starts;
		while ((void *)node->ends - (void *)entry >= sizeof(*entry))
		{
			entry->next = ht->free;
			ht->free = entry;
			entry++;
		}
	}

	entry = ht->free;
	ht->free = entry->next;

	entry->hash = hash;
	entry->key = key;
	entry->klen = klen;
	entry->val = val;

	entry->next = ht->buckets[index];
	ht->buckets[index] = entry;

	ht->count++;
	return 0;	//success
}

void *hash_get(hash_t *ht, const void *key, int klen)
{
	assert(ht != 0);
	assert(key != 0);
	assert(klen > 0);

	uint32_t hash = digest(key, klen);

	int index = hash & (HASH_BUCKETS-1);
	hash_entry_t *entry = ht->buckets[index];
	while (entry != 0)
	{
		if (entry->hash == hash &&
			entry->klen == klen && memcmp(key, entry->key, klen) == 0)
		{
			//found
			return entry->val;
		}
		entry = entry->next;
	}

	return 0;
}

int hash_count(hash_t *ht)
{
	return ht->count;
}

//
// Hash traversal
//

void hash_start(hash_t *ht, hash_index_t *hi)
{
	hi->hash = ht;
	hi->entry = ht->buckets[0];
	hi->buck = 0;
}

void *hash_next(hash_index_t *hi)
{
	hash_t *ht = hi->hash;
	while (hi->entry == 0)
	{
		hi->buck++;
		if (hi->buck >= ht->nr_buckets)
			return 0;
		hi->entry = ht->buckets[hi->buck];
	}
	void *val = hi->entry->val;
	hi->entry = hi->entry->next;
	return val;
}

void hash_destroy(hash_t *ht)
{
	nfree_chain(ht->nodes);
}

static uint32_t digest(const unsigned char *key, int klen)
{
	uint32_t hash = key[0];
	for (int i = 1; i < klen; i++)
		hash = 33 * hash + key[i];
	return hash;
}

uint32_t hash_mem_size(hash_t *ht)
{
	uint32_t mem_size = 0;
	memnode_t *node = ht->nodes;
	while (node != 0)
	{
		mem_size += node->index *PAGE_SIZE;
		node = node->next;
	}
	return mem_size;
}

uint32_t hash_used_mem_size(hash_t *ht)
{
	uint32_t mem_size = sizeof(ht);
	mem_size += ht->count *sizeof(hash_entry_t);
	return mem_size;
}

//EOF
