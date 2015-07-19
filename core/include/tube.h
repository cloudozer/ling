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

#include "ling_common.h"

#include "nalloc.h"

#include "xen/grant_table.h"

#define TUBE_SLOTS	16

typedef struct tube_slot_t tube_slot_t;
struct tube_slot_t {
	uint32_t len;
	uint32_t gref;
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

typedef struct tube_t tube_t;
struct tube_t {
	memnode_t *node;
	int accepting;
	tube_shared_t *page;
	uint8_t *tx_buffers[TUBE_SLOTS];
	uint8_t *rx_buffers[TUBE_SLOTS];
	uint32_t evtchn_tx;
	uint32_t evtchn_rx;
	//accepting only
	uint32_t page_ref;
	//opening only
	struct gnttab_map_grant_ref page_map;
	struct gnttab_map_grant_ref bufs_map[2*TUBE_SLOTS];
};

static inline int tube_ring_next(int index)
{
	if (index == (TUBE_SLOTS-1))
		return 0;
	return index+1;
}

tube_t *tube_make(domid_t peer_domid, void *data);
void tube_info(tube_t *tb, uint32_t *page_ref, uint32_t *evtchn_tx, uint32_t *evtchn_rx);
tube_t *tube_attach(domid_t peer_domid,
		uint32_t page_ref, uint32_t evtchn_rx, uint32_t evtchn_tx, void *data);
void tube_destroy(tube_t *tb);

