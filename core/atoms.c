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

#include <stdint.h>

#include "ling_common.h"

#include "hash.h"
#include "nalloc.h"
#include "string.h"
#include "limits.h"

#define ATOMS_CHUNK_NODE_SIZE 8192
#define ATOMS_NAME_NODE_SIZE 32768

typedef struct atom_info_t atom_info_t;
struct atom_info_t {
	int index;
	uint8_t *name;
};

typedef struct atom_chunk_t atom_chunk_t;
struct atom_chunk_t {
	int start_index;
	int end_index;
	int max_index;	// chunk may grow up to this limit
	atom_chunk_t *next;
	atom_info_t items[];
};

// defines atom_chunk_t standard_atoms;
#include "atoms.inc"

static atom_chunk_t *last_chunk;
static memnode_t *name_node = 0;
static hash_t *name_to_info;

int atoms_init(void)
{
	standard_atoms.next = 0;
	last_chunk = &standard_atoms;
	name_node = 0;
	
	name_to_info = hash_make();
	if (name_to_info < 0)
		return -1;

	atom_info_t *ai = (atom_info_t *)standard_atoms.items;
	assert(standard_atoms.start_index == 0);
	atom_info_t *end = ai + standard_atoms.end_index;
	while (ai < end)
	{
		if (hash_set(name_to_info, ai->name, ai->name[0]+1, ai) < 0)
			return -1;
		ai++;
	}

	return 0; // Success
}

uint8_t *atoms_get(int index)
{
	if (index < 0)
		return 0;

	atom_chunk_t *chunk = &standard_atoms;
	do {
		if (index < chunk->end_index)
			return chunk->items[index - chunk->start_index].name;
		chunk = chunk->next;
	} while (chunk != 0);

	return 0;
}

int atom_exists(uint8_t *name)
{
	return hash_get(name_to_info, name, name[0]+1) != 0;
}

int atoms_set(uint8_t *name)
{
	atom_info_t *existing = hash_get(name_to_info, name, name[0]+1);
	if (existing != 0)
		return existing->index;

	atom_chunk_t *chunk = last_chunk;
	if (chunk->end_index >= chunk->max_index)
	{
		int start_index = chunk->end_index;

		// Add a new atom chunk
		memnode_t *node = nalloc(ATOMS_CHUNK_NODE_SIZE - sizeof(memnode_t));
		chunk = (atom_chunk_t *)node->starts;
		int nr_items = ((void *)node->ends - (void *)chunk->items) / sizeof(atom_info_t);
		chunk->start_index = start_index;
		chunk->end_index = start_index;
		chunk->max_index = start_index + nr_items;
		chunk->next = 0;

		last_chunk->next = chunk;
		last_chunk = chunk;

		//debug("Atom chunk node added: start_index %d nr_items %d\n",
		//								start_index, nr_items);
	}

	atom_info_t *ai = chunk->items + chunk->end_index - chunk->start_index;
	ai->index = chunk->end_index++;

	if (name_node == 0 || NODE_SPACE_LEFT(name_node) < name[0]+1)
	{
		memnode_t *node = nalloc(ATOMS_NAME_NODE_SIZE - sizeof(memnode_t));
		node->next = name_node;
		name_node = node;

		//debug("Atom names node added: size %d addr 0x%08x\n",
		//				NODE_SPACE_LEFT(node), node);
	}

	ai->name = (uint8_t *)name_node->starts;
	memcpy(ai->name, name, name[0]+1);
	name_node->starts = (void *)name_node->starts + name[0]+1;

	hash_set(name_to_info, ai->name, ai->name[0]+1, ai);
	return ai->index;
}

uint32_t atoms_estimate_allocated_memory(void)
{
	uint32_t mem_size = 0;
	mem_size += sizeof(standard_atoms);
	mem_size += (standard_atoms.end_index - standard_atoms.start_index) *sizeof(uint32_t);
	atom_chunk_t *ch = standard_atoms.next;
	while (ch != 0)
	{
		memnode_t *node = (memnode_t *)ch -1;
		mem_size += node->index *PAGE_SIZE;
		ch = ch->next;
	}
	memnode_t *node = name_node;
	while (node != 0)
	{
		mem_size += node->index *PAGE_SIZE;
		node = node->next;
	}
	mem_size += hash_mem_size(name_to_info);

	return mem_size;
}

uint32_t atoms_estimate_used_memory(void)
{
	uint32_t mem_size = 0;
	atom_chunk_t *ch = &standard_atoms;
	while (ch != 0)
	{
		mem_size += sizeof(atom_chunk_t);
		mem_size += (ch->end_index - ch->start_index) *sizeof(atom_info_t);
		ch = ch->next;
	}
	memnode_t *node = name_node;
	while (node != 0)
	{
		mem_size += (node->ends - node->starts) *sizeof(uint32_t);
		node = node->next;
	}
	mem_size += hash_used_mem_size(name_to_info);

	return mem_size;
}

