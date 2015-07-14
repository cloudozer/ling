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

#include "tube.h"

#include "ling_common.h"

typedef struct tube_slot_t tube_slot_t;
struct tube_slot_t {
	uint32_t len;
	uint32_t gref;
	uint32_t off;
};

typedef struct tube_ring_t tube_ring_t;
struct tube_ring_t {
	int head, tail;
	// head == tail 	ring is empty
	// head == tail+1	ring is full
	tube_slot_t slots[TUBE_SLOTS];
};

typedef struct tube_shared_t tube_shared_t;
struct tube_shared_t {
	tube_ring_t tx;
	tube_ring_t rx;
};

struct tube_t {
	int active;
	tube_shared_t *page;
	uint8_t *tx_buffers[TUBE_SLOTS];
	uint8_t *rx_buffers[TUBE_SLOTS];
	uint32_t page_ref;
	uint32_t evtchn;
};

tube_t *tube_make(void)
{
	tube_t *tb = nalloc_N(sizeof(tube_t));
	if (tb == 0)
		goto error0;
	memset(tb, 0, sizeof(tb));
	assert(sizeof(tube_shared_t) <= PAGE_SIZE);
	tb->page  = mm_alloc_page();	//XXX cannot use mm_alloc_page()
	if (tb->page == 0)
		goto error1;
	for (int i = 0; i < TUBE_SLOTS; i++)
	{
		tb->tx_buffers[i] = mm_alloc_page();
		if (tb->tx_buffers[i] == 0)
			goto error2;
		tb->rx_buffers[i] = mm_alloc_page();
		if (tb->rx_buffers[i] == 0)
			goto error2;
	}


	
	
	
	


	// allocate shared page
	// allocate tx buffers
	// allocate rx buffers
	// grant shared page
	// grant tx buffers
	// grant rx buffers
	// allocate event channel
	// bind event

	//TODO

error2:
	for (int i == 0; i < TUBE_SLOTS; i++)
		if (tb->tx_buffers[i] != 0)
			nfree XXX

error1:
	nfree(tb);
error0:
	return 0;
}

tube_t *tube_attach(void)
{
	// map shared page
	// map rx buffers
	// map tx buffers
	// bind event

	//TODO
	return 0;
}

void tube_destroy(void)
{
	// unbind event
	// if active
	// 		ungrant rx buffers
	//		ungrant tx buffers
	//		ungrant shared page
	//		unallocate rx buffers
	//		unallocate tx buffers
	//		unallocate shared page
	// if inactive
	//		unmap rx buffers
	//		unmap tx buffers
	//		unmap shared page

	//TODO
}

