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
// A Xen front-end driver
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

#ifdef EXP_LINC_LATENCY
void linc_incoming(int index);
void linc_output(int index);
#endif // EXP_LINC_LATENCY

#define NR_RX_BUFFERS	256
#define EXT_RX_BUFFERS  256

#define NR_TX_BUFFERS	256
#define NO_TX_BUFFER	-1

// The total size of the response data chained using NETRXF_more_data flag
#define CHAINED_DATA_SIZE	8192

#define PSIZE(bytes)	(((bytes) +PAGE_SIZE -1) /PAGE_SIZE)

static netfe_t *net_front_ends = 0;
static int num_net_front_ends = 0;

struct netfe_t {
	int index;
	netfe_t *next;

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

	uint8_t mac[ETH_ALEN];
	int mac_len;

	uint32_t evtchn;

	struct netif *attached_lwip_netif;
	outlet_t *attached_outlet;
};

static void netfe_int(uint32_t port, void *data);
static void netfe_incoming(netfe_t *fe, uint8_t *packet, int pack_len);
static void netfe_tx_buf_gc(netfe_t *fe);
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

		netfe_t *fe = (netfe_t *)mm_alloc_pages(PSIZE(sizeof(netfe_t)));
		memset(fe, 0, sizeof(*fe));
		
		// setup shared rings
		fe->rxs = (netif_rx_sring_t *)mm_alloc_page();
		assert(fe->rxs != 0);
		fe->txs = (netif_tx_sring_t *)mm_alloc_page();
		assert(fe->txs != 0);

		SHARED_RING_INIT(fe->rxs);
		SHARED_RING_INIT(fe->txs);
	
		FRONT_RING_INIT(&fe->rx_ring, fe->rxs, PAGE_SIZE);
		FRONT_RING_INIT(&fe->tx_ring, fe->txs, PAGE_SIZE);
	
		grants_allow_access(&fe->rx_ref, backend_id, virt_to_mfn(fe->rxs));
		grants_allow_access(&fe->tx_ref, backend_id, virt_to_mfn(fe->txs));

		// set up receive buffers
		for (int i = 0; i < NR_RX_BUFFERS; i++)
		{
			fe->rx_buffers[i] = mm_alloc_page();
			assert(fe->rx_buffers[i] != 0);
			unsigned long mfn = virt_to_mfn(fe->rx_buffers[i]);
			grants_allow_access(&fe->rx_buf_refs[i], backend_id, mfn);
		}
	
		// set up send buffers
		fe->free_tx_head = NO_TX_BUFFER;
		for (int i = 0; i < NR_TX_BUFFERS; i++)
		{
			fe->tx_buffers[i] = mm_alloc_page();
			assert(fe->tx_buffers[i] != 0);
			unsigned long mfn = virt_to_mfn(fe->tx_buffers[i]);
			grants_allow_access(&fe->tx_buf_refs[i], backend_id, mfn);

			fe->free_tx_bufs[i] = fe->free_tx_head;
			fe->free_tx_head = i;
		}
	
		// set up interrupt
		fe->evtchn = event_alloc_unbound(backend_id);
		event_bind(fe->evtchn, netfe_int, (void *)fe);
	
		snprintf(xs_key, sizeof(xs_key), "device/vif/%d/rx-ring-ref", index);
		rs = xenstore_write_uint(xs_key, fe->rx_ref);
		assert(rs == 0);
		snprintf(xs_key, sizeof(xs_key), "device/vif/%d/tx-ring-ref", index);
		rs = xenstore_write_uint(xs_key, fe->tx_ref);
		assert(rs == 0);
		snprintf(xs_key, sizeof(xs_key), "device/vif/%d/event-channel", index);
		rs = xenstore_write_uint(xs_key, fe->evtchn);
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
		snprintf(xs_key, sizeof(xs_key), "device/vif/%d/state", index);
		rs = xenstore_write(xs_key, "4");	// XenbusStateConnected
		assert(rs == 0);

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

		//
		// Publish EXT_RX_BUFFERS requests only and replenish then to this number
		// during each interrupt handler invocation.
		//
		for (int i = 0; i < EXT_RX_BUFFERS; i++)
		{
			netif_rx_request_t *req = RING_GET_REQUEST(&fe->rx_ring, fe->rx_ring.req_prod_pvt);
			req->id = i; //rx_id++;
			req->gref = fe->rx_buf_refs[i];
			fe->rx_ring.req_prod_pvt++;
		}

		RING_PUSH_REQUESTS(&fe->rx_ring);
		event_kick(fe->evtchn);	

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

static void netfe_int(uint32_t port, void *data)
{
	netfe_t *fe = (netfe_t *)data;
	assert(fe != 0);

	netfe_tx_buf_gc(fe);

	// A reponse may have NETRXF_more_data flag set. Such responses are buffered
	// instead of passing it to upper layer immediately.
	//
	static uint8_t chained_data_buffer[CHAINED_DATA_SIZE];
	static int chained_data_offset = 0;		// buffer is empty

	RING_IDX prod, cons;

try_harder:
	prod = fe->rx_ring.sring->rsp_prod;
	rmb();	// magic
	cons = fe->rx_ring.rsp_cons;

	while (cons != prod)
	{
		netif_rx_response_t *rsp = RING_GET_RESPONSE(&fe->rx_ring, cons);
		//assert(rsp->id == (cons & (NR_RX_BUFFERS -1)));
		assert(rsp->status > 0);
		//assert(rsp->offset == 0);
		assert((rsp->flags & NETRXF_extra_info) == 0);

		uint8_t *data = fe->rx_buffers[rsp->id];
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
	fe->rx_ring.rsp_cons = cons;

	int more;
	RING_FINAL_CHECK_FOR_RESPONSES(&fe->rx_ring, more);
	if (more)
		goto try_harder;
	
	int add_reqs = EXT_RX_BUFFERS - (fe->rx_ring.req_prod_pvt -fe->rx_ring.rsp_cons);
	//assert(add_reqs >= 0);

	RING_IDX req_prod = fe->rx_ring.req_prod_pvt;
	for (int i = 0; i < add_reqs; i++)
	{
		netif_rx_request_t *req = RING_GET_REQUEST(&fe->rx_ring, req_prod +i);
		req->id = (req_prod +i) & (NR_RX_BUFFERS -1); 
		req->gref = fe->rx_buf_refs[req->id];
	}

	wmb();	// dark
	fe->rx_ring.req_prod_pvt = req_prod +add_reqs;

	int notify;
	RING_PUSH_REQUESTS_AND_CHECK_NOTIFY(&fe->rx_ring, notify);
	if (notify)
		event_kick(fe->evtchn);
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

void netfe_output(netfe_t *fe, uint8_t *packet, int pack_len)
{
	assert(pack_len <= ETH_MTU +ETH_HDR_LEN +ETH_CSUM_LEN);
	assert(pack_len <= PAGE_SIZE);

#ifdef EXP_LINC_LATENCY
	// see comment above
	if (pack_len >= 6 +6 +2 +20 && packet[6 +6 +2 +1] == 42)
		linc_output(fe->index);
#endif // EXP_LINC_LATENCY

	if (fe->free_tx_head == NO_TX_BUFFER)
	{
		//printk("netfe_output: packet dropped [size %d]\n", pack_len);
		LINK_STATS_INC(link.drop);
		return;
	}

	int tx_buf = fe->free_tx_head;
	fe->free_tx_head = fe->free_tx_bufs[tx_buf];

	uint8_t *p = fe->tx_buffers[tx_buf];
	memcpy(p, packet, pack_len);

	RING_IDX prod = fe->tx_ring.req_prod_pvt;
	netif_tx_request_t *req = RING_GET_REQUEST(&fe->tx_ring, prod);
	req->gref = fe->tx_buf_refs[tx_buf];
	req->id = tx_buf;
	req->offset = 0;
	req->flags = 0;
	req->size = pack_len;
	fe->tx_ring.req_prod_pvt = prod +1;

	wmb(); // dark

	int notify;
	RING_PUSH_REQUESTS_AND_CHECK_NOTIFY(&fe->tx_ring, notify);
	if (notify)
		event_kick(fe->evtchn);

	netfe_tx_buf_gc(fe);
}

static void netfe_tx_buf_gc(netfe_t *fe)
{
	RING_IDX prod, cons;

	do {
		prod = fe->tx_ring.sring->rsp_prod;
		rmb(); // dark

		for (cons = fe->tx_ring.rsp_cons; cons != prod; cons++)
		{
			netif_tx_response_t *rsp = RING_GET_RESPONSE(&fe->tx_ring, cons);
			fe->free_tx_bufs[rsp->id] = fe->free_tx_head;
			fe->free_tx_head = rsp->id;
		}

		fe->tx_ring.rsp_cons = prod;

		// mindlessly copied from netfront.c
		fe->tx_ring.sring->rsp_event =
			prod + ((fe->tx_ring.sring->req_prod - prod) >> 1) +1;
		mb();

	} while ((cons == prod) && (prod != fe->tx_ring.sring->rsp_prod));
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
		more(3);
		*p++ = 'e'; *p++ = 't'; *p++ = 'h';
		int n = fe->index;
		assert(n >= 0);
		do {
			more(1);
			*p++ = (n % 10) +'0';
			n /= 10;
		} while (n > 0);
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
