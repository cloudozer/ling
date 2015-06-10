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
// A Xen vif front-end driver
//

#include "netfe.h"

#include "ling_common.h"

#include <string.h>
#include <ctype.h>

#include "lwip/stats.h"
#include "lwip/pbuf.h"
#include "lwip/netif.h"

#include "xen/io/ring.h"
#include "xen/io/netif.h"

#include "snprintf.h"
#include "xenstore.h"
#include "mm.h"
#include "grant.h"
#include "event.h"
#include "getput.h"

#include "outlet.h"
#include "ol_inet.h"

#include "netmap.h"

#ifdef EXP_LINC_LATENCY
void linc_incoming(int index);
void linc_output(int index);
#endif // EXP_LINC_LATENCY

#ifdef EXP_LINC_LLSTAT
void linc_int_stat(int ifidx, int rx_consumed, int tx_freed, int kicked);
void linc_out_stat(int ifidx, int tx_len, int tx_freed, int kicked);
void linc_out_drop(int ifidx, int tx_len);
#endif // EXP_LINC_LLSTAT

#define NR_RX_BUFFERS	256
#define EXT_RX_BUFFERS  256

#define NR_TX_BUFFERS	256
#define NO_TX_BUFFER	-1

#define NM_MAX_RINGS	16
#define NM_MAX_BUFS		1024

// The total size of the response data chained using NETRXF_more_data flag
#define CHAINED_DATA_SIZE	8192

#define PSIZE(bytes)	(((bytes) +PAGE_SIZE -1) /PAGE_SIZE)

static netfe_t *net_front_ends = 0;
static int num_net_front_ends = 0;

typedef struct netfe_generic_t netfe_generic_t;
struct netfe_generic_t {
	netif_rx_sring_t *rxs;
	netif_tx_sring_t *txs;

	netif_rx_front_ring_t rx_ring;	
	netif_tx_front_ring_t tx_ring;

	grant_ref_t rx_ref;
	grant_ref_t tx_ref;

	void *rx_buffers[NR_RX_BUFFERS];
	grant_ref_t rx_buf_refs[NR_RX_BUFFERS];

//TODO: create two tx buffers per page as standard mtu (1500) allows this. This
// will not work for jumbo frames.

	void *tx_buffers[NR_TX_BUFFERS];
	grant_ref_t tx_buf_refs[NR_TX_BUFFERS];
	int free_tx_bufs[NR_TX_BUFFERS];
	int free_tx_head;

	uint32_t evtchn;
};

typedef struct netfe_netmap_t netfe_netmap_t;
struct netfe_netmap_t {
	int nr_tx_desc;
	int nr_rx_desc;

	int nr_tx_ring_refs;
	grant_ref_t tx_ring_refs[NM_MAX_RINGS];
	int nr_rx_ring_refs;
	grant_ref_t rx_ring_refs[NM_MAX_RINGS];

	struct netmap_ring *tx_rings;
	struct netmap_ring *rx_rings;

	int nr_tx_buf_refs;
	uint32_t tx_buf_refs[NM_MAX_BUFS];
	int nr_rx_buf_refs;
	uint32_t rx_buf_refs[NM_MAX_BUFS];

	uint8_t *tx_buf_base;
	uint8_t *rx_buf_base;

	uint32_t evtchn_tx;
	uint32_t evtchn_rx;

	uint16_t txhead;
	int		 txkick;	
};

struct netfe_t {
	int index;
	netfe_t *next;

	int netmap;
	void *priv;		// either netfe_generic_t* or netfe_netmap_t* (netmap=1)

	uint8_t mac[ETH_ALEN];
	int mac_len;

	struct netif *attached_lwip_netif;
	outlet_t *attached_outlet;
};

static netfe_t *init_generic_vif(domid_t backend_id, int index);
static netfe_t *init_netmap_vif(domid_t backend_id, int index, const char *backend);
static void netfe_netmap_connect(netfe_t *fe);
static void netfe_generic_output(netfe_t *fe, uint8_t *packet, int pack_len);
static void netfe_netmap_output(netfe_t *fe, uint8_t *packet, int pack_len);
static void netfe_generic_int(uint32_t port, void *data);
static void netfe_netmap_tx_int(uint32_t port, void *data);
static void netfe_netmap_rx_int(uint32_t port, void *data);
static void netfe_netmap_rx(netfe_t *fe);
static void netfe_incoming(netfe_t *fe, uint8_t *packet, int pack_len);
static int netfe_tx_buf_gc(netfe_generic_t *priv);
static struct pbuf *packet_to_pbuf(unsigned char *packet, int pack_len);
static int parse_mac(char *s, uint8_t mac[ETH_ALEN]);

netfe_t *netfe_get_eth_by_index(int index)
{
	netfe_t *fe = net_front_ends;
	while (fe != 0 && fe->index != index)
		fe = fe->next;
	return fe;
}

void netfe_init(void)
{
	int index = 0;
	netfe_t **link = &net_front_ends;

	while (1)
	{
		int n;
		char xs_key[256];
		snprintf(xs_key, sizeof(xs_key), "device/vif/%d/backend-id", index);
		int rs = xenstore_read_int(&n, xs_key);
		if (rs != 0)
			break;

		// FE/(index) is present
		domid_t backend_id = (domid_t)n;

		char backend[256];
		snprintf(xs_key, sizeof(xs_key), "device/vif/%d/backend", index);
		rs = xenstore_read(xs_key, backend, sizeof(backend));
		assert(rs == 0);

		netfe_t *fe = 0;

		// netmap or not?
		snprintf(xs_key, sizeof(xs_key), "%s/feature-netmap", backend);
		rs = xenstore_read_int(&n, xs_key);
		if (rs == 0 && n != 0)
			fe = init_netmap_vif(backend_id, index, backend);
		else
			fe = init_generic_vif(backend_id, index);

		snprintf(xs_key, sizeof(xs_key), "device/vif/%d/state", index);
		rs = xenstore_write(xs_key, "4");	// XenbusStateConnected
		assert(rs == 0);

		if (fe->netmap)
		{
			printk("\rnetmap: waiting for eth%d... ", index);
			do {
				snprintf(xs_key, sizeof(xs_key), "%s/state", backend);
				rs = xenstore_read_int(&n, xs_key);
				assert(rs == 0);
			} while (n != 4); // XenbusStateConnected
			printk("connected\r\n");

			netfe_netmap_connect(fe);
		}

		// read MAC address
		char buf[64];
		snprintf(xs_key, sizeof(xs_key), "device/vif/%d/mac", index);
		rs = xenstore_read(xs_key, buf, sizeof(buf));
		assert(rs == 0);
		rs = parse_mac(buf, fe->mac);
		assert(rs == 0);

		fe->mac_len = ETH_ALEN;
		printk("\reth%d: MAC %02x:%02x:%02x:%02x:%02x:%02x\r\n", index,
					fe->mac[0], fe->mac[1], fe->mac[2],
					fe->mac[3], fe->mac[4], fe->mac[5]);

		fe->index = index++;
		//fe->next = 0;

		//fe->attached_lwip_netif = 0;
		//fe->attached_outlet = 0;

		// add to net_front_ends list
		*link = fe;
		link = &fe->next;
	}

	num_net_front_ends = index;
}

static netfe_t *init_generic_vif(domid_t backend_id, int index)
{
	int np = PSIZE(sizeof(netfe_t) + sizeof(netfe_generic_t));
	netfe_t *fe = (netfe_t *)mm_alloc_pages(np);
	memset(fe, 0, sizeof(*fe));
	netfe_generic_t *priv = (netfe_generic_t *)(fe+1);
	fe->priv = priv;

	//fe->netmap = 0;
	
	// setup shared rings
	priv->rxs = (netif_rx_sring_t *)mm_alloc_page();
	assert(priv->rxs != 0);
	priv->txs = (netif_tx_sring_t *)mm_alloc_page();
	assert(priv->txs != 0);

	SHARED_RING_INIT(priv->rxs);
	SHARED_RING_INIT(priv->txs);

	FRONT_RING_INIT(&priv->rx_ring, priv->rxs, PAGE_SIZE);
	FRONT_RING_INIT(&priv->tx_ring, priv->txs, PAGE_SIZE);

	grants_allow_access(&priv->rx_ref, backend_id, virt_to_mfn(priv->rxs));
	grants_allow_access(&priv->tx_ref, backend_id, virt_to_mfn(priv->txs));

	// set up receive buffers
	for (int i = 0; i < NR_RX_BUFFERS; i++)
	{
		priv->rx_buffers[i] = mm_alloc_page();
		assert(priv->rx_buffers[i] != 0);
		unsigned long mfn = virt_to_mfn(priv->rx_buffers[i]);
		grants_allow_access(&priv->rx_buf_refs[i], backend_id, mfn);
	}

	// set up send buffers
	priv->free_tx_head = NO_TX_BUFFER;
	for (int i = 0; i < NR_TX_BUFFERS; i++)
	{
		priv->tx_buffers[i] = mm_alloc_page();
		assert(priv->tx_buffers[i] != 0);
		unsigned long mfn = virt_to_mfn(priv->tx_buffers[i]);
		grants_allow_access(&priv->tx_buf_refs[i], backend_id, mfn);

		priv->free_tx_bufs[i] = priv->free_tx_head;
		priv->free_tx_head = i;
	}

	// set up interrupt
	priv->evtchn = event_alloc_unbound(backend_id);
	event_bind(priv->evtchn, netfe_generic_int, (void *)fe);

	char xs_key[256];
	snprintf(xs_key, sizeof(xs_key), "device/vif/%d/rx-ring-ref", index);
	int rs = xenstore_write_uint(xs_key, priv->rx_ref);
	assert(rs == 0);
	snprintf(xs_key, sizeof(xs_key), "device/vif/%d/tx-ring-ref", index);
	rs = xenstore_write_uint(xs_key, priv->tx_ref);
	assert(rs == 0);
	snprintf(xs_key, sizeof(xs_key), "device/vif/%d/event-channel", index);
	rs = xenstore_write_uint(xs_key, priv->evtchn);
	assert(rs == 0);
	snprintf(xs_key, sizeof(xs_key), "device/vif/%d/request-rx-copy", index);
	rs = xenstore_write(xs_key, "1");
	assert(rs == 0);
	snprintf(xs_key, sizeof(xs_key), "device/vif/%d/feature-no-csum-offload", index);
	rs = xenstore_write(xs_key, "1");
	assert(rs == 0);
	snprintf(xs_key, sizeof(xs_key), "device/vif/%d/feature-rx-notify", index);
	rs = xenstore_write(xs_key, "1");
	assert(rs == 0);

	//
	// Publish EXT_RX_BUFFERS requests only and replenish then to this number
	// during each interrupt handler invocation.
	//
	for (int i = 0; i < EXT_RX_BUFFERS; i++)
	{
		netif_rx_request_t *req = RING_GET_REQUEST(&priv->rx_ring, priv->rx_ring.req_prod_pvt);
		req->id = i; //rx_id++;
		req->gref = priv->rx_buf_refs[i];
		priv->rx_ring.req_prod_pvt++;
	}

	RING_PUSH_REQUESTS(&priv->rx_ring);
	event_kick(priv->evtchn);	

	return fe;
}

static netfe_t *init_netmap_vif(domid_t backend_id, int index, const char *backend)
{
	int j;

	int np = PSIZE(sizeof(netfe_t) + sizeof(netfe_netmap_t));
	netfe_t *fe = (netfe_t *)mm_alloc_pages(np);
	memset(fe, 0, sizeof(*fe));
	netfe_netmap_t *priv = (netfe_netmap_t *)(fe+1);
	fe->priv = priv;

	fe->netmap = 1;

	char xs_key[256];
	int n;
	// read number of slots in each ring
	snprintf(xs_key, sizeof(xs_key), "%s/feature-netmap-tx-desc", backend);
	int rs = xenstore_read_int(&priv->nr_tx_desc, xs_key);
	assert(rs == 0);
	snprintf(xs_key, sizeof(xs_key), "%s/feature-netmap-rx-desc", backend);
	rs = xenstore_read_int(&priv->nr_rx_desc, xs_key);
	assert(rs == 0);

	priv->evtchn_tx = event_alloc_unbound(backend_id);
	event_bind(priv->evtchn_tx, netfe_netmap_tx_int, (void *)fe);
	priv->evtchn_rx = event_alloc_unbound(backend_id);
	event_bind(priv->evtchn_rx, netfe_netmap_rx_int, (void *)fe);

	// read tx/rx ring refs
	snprintf(xs_key, sizeof(xs_key), "device/vif/%d/tx-ring-refs", index);
	rs = xenstore_read_int(&priv->nr_tx_ring_refs, xs_key);
	assert(rs == 0);
	assert(priv->nr_tx_ring_refs <= NM_MAX_RINGS);
	for (int i = 0; i < priv->nr_tx_ring_refs; i++)
	{
		snprintf(xs_key, sizeof(xs_key), "device/vif/%d/tx-ring-ref%d", index, i);
		rs = xenstore_read_int(&n, xs_key);
		assert(rs == 0);
		priv->tx_ring_refs[i] = n;
	}
	snprintf(xs_key, sizeof(xs_key), "device/vif/%d/rx-ring-refs", index);
	rs = xenstore_read_int(&priv->nr_rx_ring_refs, xs_key);
	assert(rs == 0);
	assert(priv->nr_rx_ring_refs <= NM_MAX_RINGS);
	for (int i = 0; i < priv->nr_rx_ring_refs; i++)
	{
		snprintf(xs_key, sizeof(xs_key), "device/vif/%d/rx-ring-ref%d", index, i);
		rs = xenstore_read_int(&n, xs_key);
		assert(rs == 0);
		priv->rx_ring_refs[i] = n;
	}

	assert(NM_MAX_BUFS >= NM_MAX_RINGS);
	struct gnttab_map_grant_ref op[NM_MAX_BUFS];
	//struct gnttab_map_entry pte[NM_MAX_BUFS];

	//printk("\rnetmap: mapping TX rings\r\n");
	unsigned long tx_addr = (unsigned long)mm_alloc_pages(priv->nr_tx_ring_refs);
	assert(tx_addr != 0);

	for (int i = 0; i < priv->nr_tx_ring_refs -1; i++)	//NB: -1
	{
		op[i].ref = priv->tx_ring_refs[i];
		op[i].dom = (domid_t) 0;	// Dom0 only
		op[i].flags = GNTMAP_host_map;
		op[i].host_addr = tx_addr + PAGE_SIZE*i;
	}

	rs = HYPERVISOR_grant_table_op(GNTTABOP_map_grant_ref, &op, priv->nr_tx_ring_refs -1); //NB: -1
	assert(rs == 0);
		
	for (int i = 0; i < priv->nr_tx_ring_refs -1; i++)	//NB: -1
	{
		assert(op[i].status == GNTST_okay);
		//pte[i].host_addr = op[i].host_addr;
		//pte[i].handle = op[i].handle;
		rmb();
	}

	priv->tx_rings = (struct netmap_ring *)op[0].host_addr;
	while (priv->tx_rings->num_slots != priv->nr_tx_desc)
		rmb();

	//printk("\rnetmap: mapping RX rings\r\n");
	unsigned long rx_addr = (unsigned long)mm_alloc_pages(priv->nr_rx_ring_refs);
	assert(rx_addr != 0);

	for (int i = 0; i < priv->nr_rx_ring_refs -1; i++)	//NB: -1
	{
		op[i].ref = priv->rx_ring_refs[i];
		op[i].dom = (domid_t) 0;	// Dom0 only
		op[i].flags = GNTMAP_host_map;
		op[i].host_addr = rx_addr + PAGE_SIZE*i;
	}

	rs = HYPERVISOR_grant_table_op(GNTTABOP_map_grant_ref, &op, priv->nr_rx_ring_refs -1); //NB: -1
	assert(rs == 0);
		
	for (int i = 0; i < priv->nr_rx_ring_refs -1; i++)	//NB: -1
	{
		assert(op[i].status == GNTST_okay);
		//pte[i].host_addr = op[i].host_addr;
		//pte[i].handle = op[i].handle;
		rmb();
	}

	priv->rx_rings = (struct netmap_ring *)op[0].host_addr;
	while (priv->rx_rings->num_slots != priv->nr_rx_desc)
		rmb();

	priv->nr_tx_buf_refs = priv->tx_rings->num_slots/2;
	j = 0;
	for (int i = 0; i < priv->tx_rings->num_slots; i++)
	{
		if (priv->tx_rings->slot[i].ptr != 0)
			priv->tx_buf_refs[j++] = priv->tx_rings->slot[i].ptr;
	}
	priv->nr_tx_buf_refs = j;
	assert(priv->nr_tx_buf_refs <= NM_MAX_BUFS);

	//printk("\rnetmap: mapping TX buffers\r\n");
	unsigned long tx_buf_addr = (unsigned long)mm_alloc_pages(priv->nr_tx_buf_refs);

	for (int i = 0; i < priv->nr_tx_buf_refs; i++)
	{
		op[i].ref = (grant_ref_t) priv->tx_buf_refs[i];
		op[i].dom = (domid_t) 0;	// Dom0 only
		op[i].flags = GNTMAP_host_map;
		op[i].host_addr = tx_buf_addr + PAGE_SIZE*i;
	}

	rs = HYPERVISOR_grant_table_op(GNTTABOP_map_grant_ref, &op, priv->nr_tx_buf_refs);
	assert(rs == 0);
		
	for (int i = 0; i < priv->nr_tx_buf_refs; i++)
	{
		assert(op[i].status == GNTST_okay);
		//pte[i].host_addr = op[i].host_addr;
		//pte[i].handle = op[i].handle;
		rmb();
	}
	priv->tx_buf_base = (uint8_t *)op[0].host_addr;

	priv->nr_rx_buf_refs = priv->rx_rings->num_slots/2;
	j = 0;
	for (int i = 0; i < priv->rx_rings->num_slots; i++)
	{
		if (priv->rx_rings->slot[i].ptr != 0)
			priv->rx_buf_refs[j++] = priv->rx_rings->slot[i].ptr;
	}
	priv->nr_rx_buf_refs = j;
	assert(priv->nr_rx_buf_refs <= NM_MAX_BUFS);

	//printk("\rnetmap: mapping RX buffers\r\n");
	unsigned long rx_buf_addr = (unsigned long)mm_alloc_pages(priv->nr_rx_buf_refs);

	for (int i = 0; i < priv->nr_rx_buf_refs; i++)
	{
		op[i].ref = (grant_ref_t) priv->rx_buf_refs[i];
		op[i].dom = (domid_t) 0;	// Dom0 only
		op[i].flags = GNTMAP_host_map;
		op[i].host_addr = rx_buf_addr + PAGE_SIZE*i;
	}

	rs = HYPERVISOR_grant_table_op(GNTTABOP_map_grant_ref, &op, priv->nr_rx_buf_refs);
	assert(rs == 0);
		
	for (int i = 0; i < priv->nr_rx_buf_refs; i++)
	{
		assert(op[i].status == GNTST_okay);
		//pte[i].host_addr = op[i].host_addr;
		//pte[i].handle = op[i].handle;
		rmb();
	}
	priv->rx_buf_base = (uint8_t *)op[0].host_addr;

	snprintf(xs_key, sizeof(xs_key), "device/vif/%d/event-channel-tx", index);
	rs = xenstore_write_uint(xs_key, priv->evtchn_tx);
	assert(rs == 0);
	snprintf(xs_key, sizeof(xs_key), "device/vif/%d/event-channel-rx", index);
	rs = xenstore_write_uint(xs_key, priv->evtchn_rx);
	assert(rs == 0);

	priv->txhead = priv->tx_rings->head;
	priv->txkick = 1;

	return fe;
}

// complete the netmap initialisation
static void netfe_netmap_connect(netfe_t *fe)
{
	netfe_netmap_t *priv = (netfe_netmap_t *)fe->priv;
	event_kick(priv->evtchn_tx);
	event_kick(priv->evtchn_rx);
}

void netfe_get_mac(netfe_t *fe, uint8_t *mac, int mac_len)
{
	assert(mac_len == fe->mac_len);
	memcpy(mac, fe->mac, fe->mac_len);
}

void netfe_attach_lwip_netif(netfe_t *fe, struct netif *nf)
{
	assert(nf != 0);
	assert(fe->attached_lwip_netif == 0);
	fe->attached_lwip_netif = nf;
}

void netfe_attach_outlet(netfe_t *fe, outlet_t *ol)
{
	assert(ol != 0);
	if (fe->attached_outlet != 0)
		printk("netfe_attach_outlet: %pt is stealing control over eth%d from %pt\n",
						T(ol->oid), fe->index, T(fe->attached_outlet->oid));
	fe->attached_outlet = ol;
}

void netfe_detach_outlet(netfe_t *fe)
{
	assert(fe->attached_outlet != 0);
	fe->attached_outlet = 0;
}

static void netfe_generic_int(uint32_t port, void *data)
{
	netfe_t *fe = (netfe_t *)data;
	assert(fe != 0);
	assert(!fe->netmap);
	netfe_generic_t *priv = (netfe_generic_t *)fe->priv;

	UNUSED int nr_tx_gc = netfe_tx_buf_gc(priv);

	// A reponse may have NETRXF_more_data flag set. Such responses are buffered
	// instead of passing it to upper layer immediately.
	//
	static uint8_t chained_data_buffer[CHAINED_DATA_SIZE];
	static int chained_data_offset = 0;		// buffer is empty

	RING_IDX prod, cons;

try_harder:
	prod = priv->rx_ring.sring->rsp_prod;
	rmb();	// magic
	cons = priv->rx_ring.rsp_cons;

	while (cons != prod)
	{
		netif_rx_response_t *rsp = RING_GET_RESPONSE(&priv->rx_ring, cons);
		//assert(rsp->id == (cons & (NR_RX_BUFFERS -1)));
		assert(rsp->status > 0);
		//assert(rsp->offset == 0);
		assert((rsp->flags & NETRXF_extra_info) == 0);

		uint8_t *data = priv->rx_buffers[rsp->id];
		int data_len = rsp->status;

		if (chained_data_offset > 0 || (rsp->flags & NETRXF_more_data))
		{
			assert(chained_data_offset +data_len <= CHAINED_DATA_SIZE);
			memcpy(chained_data_buffer +chained_data_offset, data, data_len);
			chained_data_offset += data_len;
		}

		if ((rsp->flags & NETRXF_more_data) == 0)
		{
			if (chained_data_offset > 0)
			{
				netfe_incoming(fe, chained_data_buffer, chained_data_offset);
				chained_data_offset = 0;
			}
			else
				netfe_incoming(fe, data, data_len);
		}

		cons++;
	}
	priv->rx_ring.rsp_cons = cons;

	int more;
	RING_FINAL_CHECK_FOR_RESPONSES(&priv->rx_ring, more);
	if (more)
		goto try_harder;
	
	int add_reqs = EXT_RX_BUFFERS - (priv->rx_ring.req_prod_pvt -priv->rx_ring.rsp_cons);
	//assert(add_reqs >= 0);

	RING_IDX req_prod = priv->rx_ring.req_prod_pvt;
	for (int i = 0; i < add_reqs; i++)
	{
		netif_rx_request_t *req = RING_GET_REQUEST(&priv->rx_ring, req_prod +i);
		req->id = (req_prod +i) & (NR_RX_BUFFERS -1); 
		req->gref = priv->rx_buf_refs[req->id];
	}

	wmb();	// dark
	priv->rx_ring.req_prod_pvt = req_prod +add_reqs;

	int notify;
	RING_PUSH_REQUESTS_AND_CHECK_NOTIFY(&priv->rx_ring, notify);
	if (notify)
		event_kick(priv->evtchn);

#ifdef EXP_LINC_LLSTAT
	linc_int_stat(fe->index, add_reqs, nr_tx_gc, notify);
#endif // EXP_LINC_LLSTAT
}

static void netfe_incoming(netfe_t *fe, uint8_t *packet, int pack_len)
{
#ifdef EXP_LINC_LATENCY
	// DSCP:ECN must be 42
	// 6 +6 +2	ether
	// 20		ip
	// -		icmp
	if (pack_len >= 6 +6 +2 +20 && packet[6 +6 +2 +1] == 42)
		linc_incoming(fe->index);
#endif // EXP_LINC_LATENCY

	// NI
	if (fe->attached_lwip_netif != 0)
	{
		LINK_STATS_INC(link.recv);

		struct pbuf *p = packet_to_pbuf(packet, pack_len);
		if (p != 0)
		{
			struct netif *nf = fe->attached_lwip_netif;
			if (nf->input(p, nf) != ERR_OK)
			{
				printk("netfe_incoming: input error\n");
				pbuf_free(p);
			}
		}
		else
		{
			//printk("netfe_incoming: packet dropped\n");
			LINK_STATS_INC(link.memerr);
			LINK_STATS_INC(link.drop);
		}
	}

	// OL
	if (fe->attached_outlet != 0)
		outlet_new_data(fe->attached_outlet, packet, pack_len);
}

static void netfe_netmap_tx_int(uint32_t port, void *data)
{
	netfe_t *fe = (netfe_t *)data;
	assert(fe->netmap);
	netfe_netmap_t *priv = (netfe_netmap_t *)fe->priv;
	struct netmap_ring *ring = priv->tx_rings;

	priv->txkick = 1;

	if (priv->txhead != ring->head)
	{
		rmb();
		ring->head = ring->cur = priv->txhead;
		priv->txkick = 0;
		event_kick(priv->evtchn_tx);
	}
}

static void netfe_netmap_rx_int(uint32_t port, void *data)
{	
	netfe_t *fe = (netfe_t *)data;
	netfe_netmap_rx(fe);
}

static void netfe_netmap_rx(netfe_t *fe)
{
	assert(fe->netmap);
	netfe_netmap_t *priv = (netfe_netmap_t *)fe->priv;
	struct netmap_ring *ring = priv->rx_rings;

	if (nm_ring_empty(ring))
		return;

	uint32_t cur = ring->cur;
	uint32_t space = nm_ring_space(ring);
	uint32_t limit = ring->num_slots;
	if (space < limit)
		limit = space;

	for (uint32_t rx = 0; rx < limit; rx++)
	{
		struct netmap_slot *slot = &ring->slot[cur];
		if (slot->len == 0)
			continue;
		uint8_t *p = priv->rx_buf_base + (cur+1) * NETMAP_BUF_SIZE;	// why +1?
		//uint8_t *p = priv->rx_buf_base + cur * NETMAP_BUF_SIZE;
		netfe_incoming(fe, p, slot->len);
		cur = NETMAP_RING_NEXT(ring, cur);
	}

	ring->head = ring->cur = cur;
	event_kick(priv->evtchn_rx);
}

void netfe_output(netfe_t *fe, uint8_t *packet, int pack_len)
{
	assert(pack_len <= ETH_MTU +ETH_HDR_LEN +ETH_CSUM_LEN);
	assert(pack_len <= PAGE_SIZE);

#ifdef EXP_LINC_LATENCY
	// see comment above
	if (pack_len >= 6 +6 +2 +20 && packet[6 +6 +2 +1] == 42)
		linc_output(fe->index);
#endif // EXP_LINC_LATENCY

	if (!fe->netmap)
		netfe_generic_output(fe, packet, pack_len);
	else
		netfe_netmap_output(fe, packet, pack_len);
}

static void netfe_generic_output(netfe_t *fe, uint8_t *packet, int pack_len)
{
	assert(fe->netmap == 0);
	netfe_generic_t *priv = (netfe_generic_t *)fe->priv;

	if (priv->free_tx_head == NO_TX_BUFFER)
	{
		//printk("netfe_output: packet dropped [size %d]\n", pack_len);
#ifdef EXP_LINC_LLSTAT
		linc_out_drop(priv->index, pack_len);
#endif // EXP_LINC_LLSTAT
		LINK_STATS_INC(link.drop);
		return;
	}

	int tx_buf = priv->free_tx_head;
	priv->free_tx_head = priv->free_tx_bufs[tx_buf];

	uint8_t *p = priv->tx_buffers[tx_buf];
	memcpy(p, packet, pack_len);

	RING_IDX prod = priv->tx_ring.req_prod_pvt;
	netif_tx_request_t *req = RING_GET_REQUEST(&priv->tx_ring, prod);
	req->gref = priv->tx_buf_refs[tx_buf];
	req->id = tx_buf;
	req->offset = 0;
	req->flags = 0;
	req->size = pack_len;
	priv->tx_ring.req_prod_pvt = prod +1;

	wmb(); // dark

	int notify;
	RING_PUSH_REQUESTS_AND_CHECK_NOTIFY(&priv->tx_ring, notify);
	if (notify)
		event_kick(priv->evtchn);

	UNUSED int nr_tx_gc = netfe_tx_buf_gc(priv);

#ifdef EXP_LINC_LLSTAT
	linc_out_stat(priv->index, pack_len, nr_tx_gc, notify);
#endif // EXP_LINC_LLSTAT
}

static int netfe_tx_buf_gc(netfe_generic_t *priv)
{
	RING_IDX prod, cons;
	int nr_freed = 0;

	do {
		prod = priv->tx_ring.sring->rsp_prod;
		rmb(); // dark

		for (cons = priv->tx_ring.rsp_cons; cons != prod; cons++)
		{
			netif_tx_response_t *rsp = RING_GET_RESPONSE(&priv->tx_ring, cons);
			priv->free_tx_bufs[rsp->id] = priv->free_tx_head;
			priv->free_tx_head = rsp->id;
			nr_freed++;
		}

		priv->tx_ring.rsp_cons = prod;

		// mindlessly copied from netfront.c
		priv->tx_ring.sring->rsp_event =
			prod + ((priv->tx_ring.sring->req_prod - prod) >> 1) +1;
		mb();

	} while ((cons == prod) && (prod != priv->tx_ring.sring->rsp_prod));

	return nr_freed;
}

static void netfe_netmap_output(netfe_t *fe, uint8_t *packet, int pack_len)
{
	assert(fe->netmap);
	netfe_netmap_t *priv = (netfe_netmap_t *)fe->priv;
	struct netmap_ring *ring = priv->tx_rings;

	if (priv->txhead == ring->tail)
	{
		printk("netmap: packet dropped, len %d\n", pack_len);
		return;
	}

	uint16_t head = priv->txhead;
	struct netmap_slot *slot = &ring->slot[head];
	uint8_t *p = priv->tx_buf_base + (head+1) * NETMAP_BUF_SIZE;	// why +1
	//uint8_t *p = priv->tx_buf_base + head * NETMAP_BUF_SIZE;
	memcpy(p, packet, pack_len);
	slot->len = pack_len;
	priv->txhead = NETMAP_RING_NEXT(ring, head);

	if (priv->txkick)
	{
		ring->head = ring->cur = priv->txhead;
		priv->txkick = 0;
		wmb();
		event_kick(priv->evtchn_tx);
	}
}

static struct pbuf *packet_to_pbuf(unsigned char *packet, int pack_len)
{
	struct pbuf *p;
	u16_t len = pack_len;
#if ETH_PAD_SIZE
	len += ETH_PAD_SIZE;
#endif

	p = pbuf_alloc(PBUF_RAW, len, PBUF_POOL);
	if (p == 0)
		return 0;

#if ETH_PAD_SIZE
	pbuf_header(p, -ETH_PAD_SIZE);
#endif

	void *data = packet;
	for (struct pbuf *q = p; q != 0; q = q->next)
	{
		memcpy(q->payload, data, q->len);
		data += q->len;
	}

#if ETH_PAD_SIZE
	pbuf_header(p, ETH_PAD_SIZE);
#endif

	return p;
}

#define MACNUM(p, r) do { \
	uint8_t a__ = tolower((p)[0]); \
	uint8_t b__ = tolower((p)[1]); \
	if (a__ < '0' || (a__ > '9' && a__ < 'a') || a__ > 'f') \
		return -BAD_ARG; \
	if (b__ < '0' || (b__ > '9' && b__ < 'a') || b__ > 'f') \
		return -BAD_ARG; \
	(r) = (((a__ > '9') ?(a__ -'a' + 10) :(a__ -'0')) << 4) + \
		   ((b__ > '9') ?(b__ -'a' + 10) :(b__ -'0')); \
} while (0)

static int parse_mac(char *s, uint8_t mac[ETH_ALEN])
{
	//	0: 00:
	//  3: 16:
	//  6: 3e:
	//  9: xx:
	// 12: yy:
	// 15: zz
	
	MACNUM(s, mac[0]);
	if (s[2] != ':')
		return -BAD_ARG;
	MACNUM(s +3, mac[1]);
	if (s[2 +3] != ':')
		return -BAD_ARG;
	MACNUM(s +6, mac[2]);
	if (s[2 +6] != ':')
		return -BAD_ARG;
	MACNUM(s +9, mac[3]);
	if (s[2 +9] != ':')
		return -BAD_ARG;
	MACNUM(s +12, mac[4]);
	if (s[2 +12] != ':')
		return -BAD_ARG;
	MACNUM(s +15, mac[5]);
	if (s[2 +15] != '\0')
		return -BAD_ARG;

	return 0;
}

// {ok,[{"lo",
// 		[{flags,[up,loopback,running]},
// 		 {hwaddr,[0,0,0,0,0,0]},
// 		 {addr,{127,0,0,1}},
// 		 {netmask,{255,0,0,0}}]},
// 		{"eth0",
// 		[{flags,[up,broadcast,running,multicast]},
// 		 {hwaddr,[xx,xx,xx,xx,xx,xx]},
// 		 {addr,{aa,bb,cc,dd}},
// 		 {netmask,{255,255,254,0}},
// 		 {broadaddr,{aa,bb,cc,255}}]}]}

#define more(n) do { \
	if (p +(n) > buf +sz) \
		return -TOO_LONG; \
} while (0)

int build_getifaddrs_reply(char *buf, int sz)
{
	char *p = buf;
	more(1);
	*p++ = INET_REP_OK;

	// loopback interface
	more(3);
	*p++ = 'l'; *p++ = 'o';
	*p++ = 0;
	more(1 +4);
	*p++ = INET_IFOPT_FLAGS;
	uint32_t loflags = INET_IFF_UP | INET_IFF_LOOPBACK | INET_IFF_RUNNING;
	PUT_UINT_32(p, loflags);
	p += 4;
	more(1 +1 +4);
	*p++ = INET_IFOPT_ADDR;
	*p++ = INET_AF_INET;
	*p++ = 127; *p++ = 0; *p++ = 0; *p++ = 1;
	more(1 +1 +4);
	*p++ = INET_IFOPT_NETMASK;
	*p++ = INET_AF_INET;
	*p++ = 255; *p++ = 0; *p++ = 0; *p++ = 0;
	more(1);
	*p++ = 0; 	// end of 'lo' options

	// network front ends
	netfe_t *fe = net_front_ends;
	while (fe != 0)
	{
		// ethNN

		// convert NN first and append in reverse order - NN may be more than 9
		int n = fe->index;
		assert(n >= 0);
		char pad[16];
		char *conv = pad;
		do {
			*conv++ = (n % 10) +'0';
			n /= 10;
		} while (n > 0);
		
		more(3);
		*p++ = 'e'; *p++ = 't'; *p++ = 'h';
		do {
			more(1);
			conv--;
			*p++ = *conv;
		} while (conv > pad);
		more(1);
		*p++ = 0;

		more(1 +4);
		*p++ = INET_IFOPT_FLAGS;
		uint32_t ethflags = INET_IFF_UP | INET_IFF_BROADCAST | INET_IFF_RUNNING | INET_IFF_MULTICAST;
		PUT_UINT_32(p, ethflags);
		p += 4;

		more(1 +2 +6);
		*p++ = INET_IFOPT_HWADDR;
		PUT_UINT_16(p, fe->mac_len);
		p += 2;
		memcpy(p, fe->mac, fe->mac_len);
		p += fe->mac_len;

		if (fe->attached_lwip_netif)
		{
			// can happen for eth0 only
			struct netif *nf = fe->attached_lwip_netif;
			if (!ip_addr_isany(&nf->ip_addr))
			{
				more(1 +1 +4);
				*p++ = INET_IFOPT_ADDR;
				*p++ = INET_AF_INET;
				*p++ = ip4_addr1(&nf->ip_addr);
				*p++ = ip4_addr2(&nf->ip_addr);
				*p++ = ip4_addr3(&nf->ip_addr);
				*p++ = ip4_addr4(&nf->ip_addr);
				more(1 +1 +4);
				*p++ = INET_IFOPT_NETMASK;
				*p++ = INET_AF_INET;
				*p++ = ip4_addr1(&nf->netmask);
				*p++ = ip4_addr2(&nf->netmask);
				*p++ = ip4_addr3(&nf->netmask);
				*p++ = ip4_addr4(&nf->netmask);
			}
		}

		more(1);
		*p++ = 0; 	// end of ethXX options

		fe = fe->next;
	}

	return p -buf;
}

//EOF
