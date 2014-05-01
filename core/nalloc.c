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
// Node allocator
//

#include "nalloc.h"

#include "ling_common.h"

#include "mm.h"

#define NBOUND	4096
#define NINDEX	12

#define MAX_INDEX	20

jmp_buf no_memory_jmp_buf;

struct {
	// lists of nodes of same size; free[0] contains odd-sized nodes
	memnode_t *free[MAX_INDEX];
	int nr_freed_pages;
} node_allocator;

void nalloc_init(void)
{
	int i;
	for (i = 0; i < MAX_INDEX; i++)
		node_allocator.free[i] = 0;

	node_allocator.nr_freed_pages = 0;
}

static memnode_t *nalloc_internal(int size);
static memnode_t *split_nodes(int index);
static memnode_t *split_odd_nodes(int index);

memnode_t *nalloc(int size)
{
	memnode_t *node = nalloc_internal(size);
	if (node == 0)
		no_memory_signal();
	return node;
}

// An allocation routine that returns 0 when there is not enough memory
// available. The routine is used almost exclusively by functions called
// (indirectly) by scheduler_next, when the context is not obvious.
memnode_t *nalloc_N(int size)
{
	memnode_t *node = nalloc_internal(size);
//#ifdef LING_DEBUG
//	if (node == 0)
//		gdb_break();
//#endif
	return node;
}

int nalloc_freed_pages()
{
	return node_allocator.nr_freed_pages;
}

static memnode_t *nalloc_internal(int size)
{
	assert(size > 0);

	// ext_size accomodates both the client memory and the memnode_t structure
	// itself aligned to NBOUND
	int ext_size = (sizeof(memnode_t) + size + (NBOUND-1)) & ~(NBOUND-1);

	int index = ext_size >> NINDEX;
	if (index >= MAX_INDEX)
	{
		// The large size is large -
		// the first fitting large node is returned

		memnode_t **ref = &node_allocator.free[0];
		memnode_t *node = *ref;
		while (node)
		{
			if (node->index >= index)
				break;
			ref = &node->next;
			node = *ref;
		}

		if (node)
		{
			*ref = node->next;
			node->next = 0;

			node_allocator.nr_freed_pages -= node->index;
			return node;
		}

		//FALLTHROUGH
	}
	else if (node_allocator.free[index] != 0)
	{
		// reuse
		memnode_t *node = node_allocator.free[index];
		node_allocator.free[index] = node->next;
		node->next = 0;

		node_allocator.nr_freed_pages -= node->index;
		return node;
	}

	// no same size node; dive into mm_alloc_pages()
	int nr_pages = ext_size / PAGE_SIZE;
	memnode_t *new_node = (memnode_t *)mm_alloc_pages(nr_pages);
	if (new_node != 0)
	{
		new_node->next = 0;
		new_node->index = index;
		new_node->starts = NODE_THRESHOLD(new_node);
		new_node->ends = (void *)new_node + ext_size;

		node_allocator.nr_freed_pages -= new_node->index;
		return new_node;
	}

	if (index >= MAX_INDEX)
		return 0;	// possibilities are exhausted for odd sizes

	// try to allocate a node by splitting odd-sized nodes
	memnode_t *chip = split_odd_nodes(index);
	if (chip == 0)
		// try to allocate a node by splitting larger nodes
		chip = split_nodes(index);

	if (chip != 0)
		node_allocator.nr_freed_pages -= chip->index;
	return chip;
}

static memnode_t *split_nodes(int index)
{
	memnode_t *node = 0;
	for (int i = index +1; i < MAX_INDEX; i++)
	{
		if (node_allocator.free[i] != 0)
		{
			node = node_allocator.free[i];
			node_allocator.free[i] = node->next;
			break;
		}
	}

	if (node == 0)
		return 0;
	
	// The node large enough node is found - split into two parts and put the
	// remainder back into appropriate chain.

	int ext_size = index << NINDEX;
	memnode_t *chip = (memnode_t *)((void *)node->ends - ext_size);
	chip->next = 0;
	chip->index = index;
	chip->starts = NODE_THRESHOLD(chip);
	chip->ends = node->ends;

	node->ends = (uint32_t *)chip;
	node->index -= index;

	node->next = node_allocator.free[node->index];
	node_allocator.free[node->index] = node;

	return chip;
}

static memnode_t *split_odd_nodes(int index)
{
	memnode_t **ref = &node_allocator.free[0];
	memnode_t *node = *ref;
	while (node)
	{
		if (node->index > index)
			break;
		ref = &node->next;
		node = *ref;
	}

	if (node == 0)
		return 0;
	
	// The node large enough node is found - remove it from the zero chain,
	// split into two parts and put the remainder back into appropriate chain.
	*ref = node->next;
	node->next = 0;

	int ext_size = index << NINDEX;
	memnode_t *chip = (memnode_t *)((void *)node->ends - ext_size);
	chip->next = 0;
	chip->index = index;
	chip->starts = NODE_THRESHOLD(chip);
	chip->ends = node->ends;

	node->ends = (uint32_t *)chip;
	node->index -= index;

	if (node->index >= MAX_INDEX)
	{
		node->next = node_allocator.free[0];
		node_allocator.free[0] = node;
	}
	else
	{
		node->next = node_allocator.free[node->index];
		node_allocator.free[node->index] = node;
	}

	return chip;
}

void nfree(memnode_t *node)
{
	if (node == 0)
		return;

	// reset starts; the caller cannot touch .ends
	node->starts = NODE_THRESHOLD(node);

#ifdef DEBUG_UNUSED_MEM
	uint32_t *p = node->starts;
	while (p +1 <= node->ends)
		*p++ = UNUSED_MEM_SIGN;
#endif

	node_allocator.nr_freed_pages += node->index;

	int index = node->index;
	if (node->index >= MAX_INDEX)
		index = 0;
	node->next = node_allocator.free[index];
	node_allocator.free[index] = node;
}

void nfree_chain(memnode_t *nodes)
{
	while (nodes != 0)
	{
		memnode_t *freeme = nodes;
		nodes = nodes->next;
		nfree(freeme);
	}
}

void nalloc_dump_stats(void)
{
	printk("\nNode allocator map:\n\n");
	printk("Index      Min      Max Count    Total\n");
	printk("--------------------------------------\n");

	int grand_count = 0;
	int grand_total = 0;
	for (int i = 0; i < MAX_INDEX; i++)
	{
		memnode_t *node = node_allocator.free[i];
		int min_index = MAX_INT_VALUE;
		int max_index = MIN_INT_VALUE;
		int count = 0;
		int total = 0;
		while (node != 0)
		{
			if (node->index < min_index)
				min_index = node->index;
			if (node->index > max_index)
				max_index = node->index;
			count++;
			total += node->index;
			node = node->next;
		}

		if (count > 0)
			printk("%5d %8d %8d %5d %8d\n", i, min_index, max_index, count, total);
		else
			printk("%5d        -        - %5d %8d\n", i, count, total);
	
		grand_count += count;
		grand_total += total;
	}

	printk("--------------------------------------\n");
	printk("TOTAL                   %5d %8d\n", grand_count, grand_total);
}

//EOF
