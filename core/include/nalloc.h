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
#include <setjmp.h>
#include "term.h"

#define WALIGN(p)	(((p) + 3) & ~3)

extern jmp_buf no_memory_jmp_buf;

#define nalloc_no_memory()	setjmp(no_memory_jmp_buf)
#define no_memory_signal()	longjmp(no_memory_jmp_buf, 1)

typedef struct memnode_t memnode_t;
struct memnode_t {
	// Free nodes of the same standard size are chained;
	// After a node is allocated the caller of nalloc() may use this field at
	// will.
	memnode_t *next;

	// Log mem node size (1 = NBOUND, 2 = 2*NBOUND, etc), 0 for
	// oddball nodes
	int index;		

	// Upon allocations these pointers delimit the available memory for the
	// node. The caller then may increase starts pointer to indicate that node
	// memory is consumed.
	uint32_t *starts;
	uint32_t *ends;
};

#define NODE_SPACE_LEFT(node) ((void *)(node)->ends - (void *)(node)->starts)
#define NODE_THRESHOLD(node) ((void *)(node) + sizeof(memnode_t))

void nalloc_init(void);
memnode_t *nalloc(int size);
memnode_t *nalloc_N(int size);
int nalloc_freed_pages();
void nfree(memnode_t *node);
void nfree_chain(memnode_t *nodes);

//EOF
