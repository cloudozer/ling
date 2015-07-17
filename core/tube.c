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

#include <string.h>

#include "xen/grant_table.h"
#include "nalloc.h"
#include "grant.h"
#include "event.h"

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

typedef struct tube_t tube_t;
struct tube_t {
	memnode_t *node;
	int active;
	tube_shared_t *page;
	uint8_t *tx_buffers[TUBE_SLOTS];
	uint8_t *rx_buffers[TUBE_SLOTS];
	uint32_t evtchn_tx;
	uint32_t evtchn_rx;
	//active only
	uint32_t page_ref;
	//passive only
	struct gnttab_map_grant_ref page_map;
	struct gnttab_map_grant_ref bufs_map[2*TUBE_SLOTS];
};

static void tube_tx_int(uint32_t port, void *data);
static void tube_rx_int(uint32_t port, void *data);

static tube_t *alloc_tube(int active)
{
	// share page and tx/rx buffees must be page-aligned
	int num_pages = 1 +				// tube_t
					1 +				// shared page
					TUBE_SLOTS +	// tx buffers
					TUBE_SLOTS;		// rx buffers
	memnode_t *node = nalloc_N(num_pages*PAGE_SIZE -sizeof(memnode_t));
	if (node == 0)
		return 0;
	assert(sizeof(tube_t) <= PAGE_SIZE-sizeof(memnode_t));
	tube_t *tb = (tube_t *)node->starts;
	memset(tb, 0, sizeof(*tb));
	tube_shared_t *page = (tube_shared_t *)((uint8_t *)node +PAGE_SIZE);
	uint8_t *bufs1 = (uint8_t *)node +2*PAGE_SIZE;
	uint8_t *bufs2 = bufs1 +TUBE_SLOTS*PAGE_SIZE;
	assert(bufs2 +TUBE_SLOTS*PAGE_SIZE <= (uint8_t *)node->ends);

	tb->node = node;
	tb->active = active;
	tb->page = page;
	for (int i = 0; i < TUBE_SLOTS; i++)
		tb->tx_buffers[i] = bufs1 +i*PAGE_SIZE;
	for (int i = 0; i < TUBE_SLOTS; i++)
		tb->rx_buffers[i] = bufs2 +i*PAGE_SIZE;

	return tb;
}


tube_t *tube_make(domid_t peer_domid)
{
	tube_t *tb = alloc_tube(1);
	if (tb == 0)
		return 0;

	tube_shared_t *page = tb->page;
	memset(page, 0, PAGE_SIZE);
	grants_allow_access(&tb->page_ref, peer_domid, virt_to_mfn(page));
	tb->evtchn_tx = event_alloc_unbound(peer_domid);
	tb->evtchn_rx = event_alloc_unbound(peer_domid);

	for (int i = 0; i < TUBE_SLOTS; i++)
		grants_allow_access(&page->tx.slots[i].gref, peer_domid, virt_to_mfn(tb->tx_buffers[i]));
	for (int i = 0; i < TUBE_SLOTS; i++)
		grants_allow_access(&page->rx.slots[i].gref, peer_domid, virt_to_mfn(tb->rx_buffers[i]));

	event_bind(tb->evtchn_tx, tube_tx_int, tb);
	event_bind(tb->evtchn_rx, tube_rx_int, tb);

	return tb;
}

void tube_info(tube_t *tb, uint32_t *page_ref, uint32_t *evtchn_tx, uint32_t *evtchn_rx)
{
	assert(tb != 0);
	*page_ref = tb->page_ref;
	*evtchn_tx = tb->evtchn_tx;
	*evtchn_rx = tb->evtchn_rx;
}

tube_t *tube_attach(domid_t peer_domid, uint32_t page_ref, uint32_t evtchn_rx, uint32_t evtchn_tx)
{
	tube_t *tb = alloc_tube(0);
	if (tb == 0)
		return 0;
	tube_shared_t *page = tb->page;

	tb->page_map.ref = page_ref;
	tb->page_map.dom = peer_domid;
	tb->page_map.flags = GNTMAP_host_map;
	tb->page_map.host_addr = (uint64_t)page;
	int rs = HYPERVISOR_grant_table_op(GNTTABOP_map_grant_ref, &tb->page_map, 1);
	assert(rs == 0);
	assert(tb->page_map.status == GNTST_okay);

	for (int i = 0; i < TUBE_SLOTS; i++)
	{
		struct gnttab_map_grant_ref *m = &tb->bufs_map[i];
		m->ref = page->tx.slots[i].gref;
		m->dom = peer_domid;
		m->flags = GNTMAP_host_map;
		m->host_addr = (uint64_t)tb->tx_buffers[i];
	}
	for (int i = 0; i < TUBE_SLOTS; i++)
	{
		struct gnttab_map_grant_ref *m = &tb->bufs_map[i+TUBE_SLOTS];
		m->ref = page->tx.slots[i].gref;
		m->dom = peer_domid;
		m->flags = GNTMAP_host_map;
		m->host_addr = (uint64_t)tb->rx_buffers[i];
	}
	
	rs = HYPERVISOR_grant_table_op(GNTTABOP_map_grant_ref, tb->bufs_map, 2*TUBE_SLOTS);
	assert(rs == 0);
	for (int i = 0; i < 2*TUBE_SLOTS; i++)
	{
		assert(tb->bufs_map[i].status == GNTST_okay);
		rmb();	//dark
	}

	event_bind(evtchn_tx, tube_tx_int, tb);
	event_bind(evtchn_rx, tube_rx_int, tb);
	tb->evtchn_tx = evtchn_tx;
	tb->evtchn_rx = evtchn_rx;

	return tb;
}

void tube_destroy(tube_t *tb)
{
	event_unbind(tb->evtchn_tx);
	event_unbind(tb->evtchn_rx);

	if (tb->active)
	{
		for (int i = 0; i < TUBE_SLOTS; i++)
			grants_end_access(tb->page->tx.slots[i].gref);
		for (int i = 0; i < TUBE_SLOTS; i++)
			grants_end_access(tb->page->rx.slots[i].gref);
		grants_end_access(tb->page_ref);
	}
	else
	{
		int rs = HYPERVISOR_grant_table_op(GNTTABOP_unmap_grant_ref, tb->bufs_map, 2*TUBE_SLOTS);
		assert(rs == 0);
		rs = HYPERVISOR_grant_table_op(GNTTABOP_unmap_grant_ref, &tb->page_map, 1);
		assert(rs == 0);
	}

	nfree(tb->node);
}

static void tube_tx_int(uint32_t port, void *data)
{
	//TODO
}

static void tube_rx_int(uint32_t port, void *data)
{
	//TODO
}

