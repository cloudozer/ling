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

#include "stack.h"

#include "ling_common.h"

#include "string.h"

#define STACK_NODE_SIZE	4096

void stack_init(stack_t *st, int elt_wsize, uint32_t *init_space, int num)
{
	memset(st, 0, sizeof(*st));
	st->init_node.starts = init_space +num;
	st->init_node.ends = init_space +num;
	//st->init_node.next = 0;
	st->init_node_threshold = init_space;
	st->nodes = &st->init_node;
	st->elt_wsize = elt_wsize;
}

int stack_is_empty(stack_t *st)
{
	return (st->nodes == &st->init_node) &&
		   (st->nodes->starts == st->nodes->ends);
}

uint32_t *stack_push_N(stack_t *st)
{
	uint32_t *thr = (st->nodes == &st->init_node)
		?st->init_node_threshold
		:NODE_THRESHOLD(st->nodes);
	if (st->nodes->starts -st->elt_wsize < thr)
	{
		memnode_t *node = nalloc_N(STACK_NODE_SIZE - sizeof(memnode_t));
		if (node == 0)
			return 0;
		node->starts = node->ends;	// make empty

		node->next = st->nodes;
		st->nodes = node;

		assert(st->nodes->starts -st->elt_wsize
					>= (uint32_t *)NODE_THRESHOLD(st->nodes));
	}

	uint32_t *top = st->nodes->starts -st->elt_wsize;
	st->nodes->starts = top;

	return top;
}

uint32_t *stack_pop(stack_t *st)
{
	if (st->nodes->starts == st->nodes->ends &&
			st->nodes != &st->init_node)
	{
		memnode_t *free_me = st->nodes;
		st->nodes = st->nodes->next;
		nfree(free_me);
	}

	assert(!stack_is_empty(st));
	term_t *top = (term_t *)st->nodes->starts;
	st->nodes->starts = top +st->elt_wsize;
	assert(st->nodes->starts <= st->nodes->ends);

	return top;
}

void stack_done(stack_t *st)
{
	memnode_t *node = st->nodes;
	while (node != &st->init_node)
	{
		memnode_t *freeme = node;
		node = node->next;
		nfree(freeme);
	}
}

//EOF
