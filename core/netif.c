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
 *	lwIP bindings
 *
 */

#include <stdint.h>

#include "ling_common.h"

#include "lwip/netif.h"
#include "lwip/stats.h"
#include "lwip/dhcp.h"
#include "netif/etharp.h"

#include "time.h"
#include "netfe.h"

#define IFNAME0	'x'
#define IFNAME1	'e'

uint32_t sys_closest_timeout(void);
void sys_check_timeouts(void);

// sys_now() is the time in ms since this moment
static uint64_t sys_now_origin_ns;

// linked by lwIP
void sys_init()
{
	sys_now_origin_ns = monotonic_clock();
}

// linked by lwIP
uint32_t sys_now()
{
	return (monotonic_clock() - sys_now_origin_ns) /1000000;
}

// a better function name for the scheduler loop
void lwip_check_timeouts(void)
{
	sys_check_timeouts();	// defined in lwIP
}

uint64_t lwip_closest_timeout(void)
{
	uint32_t relative_ms = sys_closest_timeout();
	if (relative_ms == 0xffffffff)
		return LING_INFINITY;
	return (uint64_t)relative_ms *1000000 + sys_now_origin_ns;
}

static err_t
netif_ethif_output(struct netif *nf, struct pbuf *p)
{
#if ETH_PAD_SIZE
	pbuf_header(p, -ETH_PAD_SIZE);
#endif

	uint8_t packet[p->tot_len];
	pbuf_copy_partial(p, packet, p->tot_len, 0);

	netfe_t *fe = (netfe_t *)nf->state;
	netfe_output(fe, packet, p->tot_len);

#if ETH_PAD_SIZE
	pbuf_header(p, ETH_PAD_SIZE);
#endif

	LINK_STATS_INC(link.xmit);
	return ERR_OK;
}

static err_t netif_ethif_init(struct netif *nf)
{
	nf->name[0] = IFNAME0;
	nf->name[1] = IFNAME1;

	NETIF_INIT_SNMP(nf, snmp_ifType_ethernet_csmacd, LINK_SPEED_OF_YOUR_NETIF_IN_BPS);

	nf->output = etharp_output;
	nf->linkoutput = netif_ethif_output;

	nf->mtu = ETH_MTU;
	nf->flags = NETIF_FLAG_BROADCAST | NETIF_FLAG_ETHARP | NETIF_FLAG_LINK_UP;

	nf->hwaddr_len = ETH_ALEN;
	netfe_get_mac((netfe_t *)nf->state, nf->hwaddr, nf->hwaddr_len);

	//
	// The low-level driver gets initialized early and may start sending packets
	// before lwIP is ready
	//
	netfe_t *fe = (netfe_t *)nf->state;
	assert(fe != 0);
	netfe_attach_lwip_netif(fe, nf);

	return ERR_OK;
}

// called by lwip:setup/4
void netif_setup(ip_addr_t *ip_addr,
				 ip_addr_t *net_mask,
				 ip_addr_t *gateway, int dhcp, netif_status_callback_fn status_cb)
{
	static struct netif lone_wolf;

	netfe_t *eth0 = netfe_get_eth_by_index(0);
	assert(eth0 != 0);

	// eth0 ref saved as state of lwip netif
	struct netif *nf;
	nf = netif_add(&lone_wolf, ip_addr, net_mask, gateway,
						eth0, netif_ethif_init, ethernet_input);
	assert(nf != 0);

	netif_set_status_callback(nf, status_cb);
	netif_set_default(nf);

	if (dhcp)
	{
		dhcp_start(nf);
		// brings the interface up after successful DHCP negotiations
	}
	else
	{
		// interface config is static; fires the callback immediately
		netif_set_up(nf);
	}
}

//EOF
