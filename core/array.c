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

#include "array.h"

#include "ling_common.h"

#include "string.h"

#define ARRAY_NODE_SIZE	4096

void array_init(array_t *arr, int elt_size, int alloc)
{
	int node_size = elt_size * alloc + sizeof(memnode_t);
	node_size +=  (ARRAY_NODE_SIZE-1);
	node_size &= ~(ARRAY_NODE_SIZE-1);
	memnode_t *node = nalloc(node_size - sizeof(memnode_t));
	arr->node = node;
	arr->elt_size = elt_size;
	arr->elts = node->starts;
	arr->nelts = 0;
	arr->alloc = ((void *)node->ends - arr->elts) / elt_size;
}

void *array_push_N(array_t *arr)
{
	if (arr->nelts >= arr->alloc)
	{
		//need a bigger node
		int alloc = 2*arr->alloc;
		if (alloc == 0)
			alloc = 1;
		int node_size = arr->elt_size * alloc + sizeof(memnode_t);
		node_size +=  (ARRAY_NODE_SIZE-1);
		node_size &= ~(ARRAY_NODE_SIZE-1);
		memnode_t *node = nalloc_N(node_size - sizeof(memnode_t));
		if (node == 0)
			return 0;

		memcpy(node->starts, arr->elts, arr->elt_size*arr->nelts);

		nfree(arr->node);
		arr->node = node;
		arr->elts = node->starts;
		arr->alloc = alloc;
	}

	void *ptr = arr->elts + arr->elt_size * arr->nelts;
	arr->nelts++;
	return ptr;
}

void *array_pop(array_t *arr)
{
	assert(arr->nelts > 0);
	arr->nelts--;
	void *ptr = arr->elts + arr->elt_size * arr->nelts;
	return ptr;
}

void array_done(array_t *arr)
{
	nfree(arr->node);
}

//EOF
