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

#include "ling_common.h"

#include "scheduler.h"

#include "outlet.h"
#include "ol_inet.h"

#include "atom_defs.h"
#include "getput.h"

#include <string.h>
#include "term_util.h"

static uint8_t *ol_udp_get_send_buffer(outlet_t *ol, int len);
static int ol_udp_send(outlet_t *ol, int len, term_t reply_to);
static term_t ol_udp_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp);
static void ol_udp_destroy_private(outlet_t *ol);

static outlet_vtab_t ol_udp_vtab = {
	.get_send_buffer = ol_udp_get_send_buffer,
	.send = ol_udp_send,
	.control = ol_udp_control,
	.destroy_private = ol_udp_destroy_private,
};

static int ol_udp_set_opts(outlet_t *ol, uint8_t *data, int dlen);
static int ol_udp_get_opts(outlet_t *ol,
					uint8_t *data, int dlen, char *buf, int sz);

#ifdef LING_WITH_LIBUV

#ifdef AF_PACKET
# define PACKET_CAPTURE_ENABLED 1

# include <sys/ioctl.h>
# include <linux/if.h>
# include <linux/if_packet.h>
# include <linux/if_ether.h>
#endif

static void
uv_on_recv(uv_udp_t *, ssize_t,
           const uv_buf_t *, const struct sockaddr *, unsigned);

static void
uv_on_recv_timeout(uv_timer_t *timeout);

static void
uv_on_send(uv_udp_send_t *udp, int status)
{
	if (status)
		printk("%s(status: %s)\n", __FUNCTION__, uv_strerror(status));
		//debug("%s(status=%d)\n", __FUNCTION__, status);
	free(udp);
}

static term_t
send_udp_packet(outlet_t *ol, ip_addr_t *ipaddr, uint16_t port, void *data, uint16_t len)
{
	int ret;
	union {
		struct sockaddr     sa;
		struct sockaddr_in  ipv4;	/* AF_INET */
		struct sockaddr_in6 ipv6; /* AF_INET6 */
#if PACKET_CAPTURE_ENABLED
		struct sockaddr_ll  eth;	/* AF_PACKET */
#endif
	} saddr;
	uv_buf_t buf = { .base = data, .len = (size_t)len };

	switch (ol->family)
	{
	case INET_AF_INET:
		saddr.sa.sa_family = AF_INET;
		saddr.ipv4.sin_port = htons(port);
		saddr.ipv4.sin_addr.s_addr = ipaddr->addr;
		debug("%s(ipv4=0x%x, ipport=0x%x)\n", __FUNCTION__, ipaddr->addr, port);
		break;

#if PACKET_CAPTURE_ENABLED
	case INET_AF_PACKET:
	{
		assert(ol->raw_ifindex != 0);

		struct ethhdr *hdr = (struct ethhdr *)data;

		memset(&saddr, 0, sizeof(saddr));
		saddr.sa.sa_family = AF_PACKET;
		saddr.eth.sll_ifindex = ol->raw_ifindex;
		saddr.eth.sll_halen = ETH_ALEN;
		memcpy(saddr.eth.sll_addr, hdr->h_dest, ETH_ALEN);

		debug("%s(iface=%d, addr=%02x:%02x:%02x:%02x:%02x:%02x)\n", __FUNCTION__, ol->raw_ifindex,
		       saddr.eth.sll_addr[0], saddr.eth.sll_addr[1], saddr.eth.sll_addr[2],
		       saddr.eth.sll_addr[3], saddr.eth.sll_addr[4], saddr.eth.sll_addr[5]);
	} break;
#endif

	case INET_AF_INET6: /* TODO */
	default:
		return A_ENOTSUP;
	}

	uv_udp_send_t *req = malloc(sizeof(uv_udp_send_t));
	ret = uv_udp_send(req, ol->udp, &buf, 1, &saddr.sa, uv_on_send);
	if (ret < 0) {
		printk("%s: uv_udp_send failed(%s)\n", __FUNCTION__, uv_strerror(ret));
		return A_ERROR; /* TODO: better description */
	}
	return A_OK;
}

static inline int udp_recv_start(outlet_t *ol)
{
	assert(ol->udp);
	return uv_udp_recv_start(ol->udp, on_alloc, uv_on_recv);
}

static inline int udp_recv_stop(outlet_t *ol)
{
	assert(ol->udp);
	return uv_udp_recv_stop(ol->udp);
}

static inline int udp_recv_set_timeout(outlet_t *ol, unsigned int msecs)
{
	assert(ol->conn_timer.data == NULL);
	int ret;
	uv_timer_t *timeout = &ol->conn_timer;

	ret = uv_timer_init(uv_default_loop(), timeout);
	if (ret)
		return -2;

	ret = uv_timer_start(timeout, uv_on_recv_timeout, msecs, 0);
	if (ret)
		return -3;

	timeout->data = ol;
	return 0;
}

static inline void udp_recv_untimeout(outlet_t *ol)
{
	debug("%s\n", __FUNCTION__);
	assert(ol->conn_timer.data);
	assert(ol->conn_timer.data == ol);

	uv_timer_stop(&ol->conn_timer);

	ol->conn_timer.data = NULL;
}


static void uv_on_close(uv_handle_t *handle)
{
	debug("%s\n", __FUNCTION__);
	free(handle);
	/* don't use handle->data here, outlet may be freed already */
}

static void udp_destroy_private(outlet_t *ol)
{
	if (!ol->udp) return;

	/* uv_udp_t is a "subclass" of uv_handle_t */
	uv_handle_t *udp = (uv_handle_t *)ol->udp;

	if (ol->cr_in_progress)
		udp_recv_stop(ol);

	debug("%s: uv_is_active=%d, uv_is_closing=%d\n", __FUNCTION__,
		  uv_is_active(udp), uv_is_closing(udp));

#if PACKET_CAPTURE_ENABLED
	if (ol->family == INET_AF_PACKET)
	{
		int fd;
		int ret;

		/* unset promisc mode */
		ret = uv_fileno((uv_handle_t *)udp, &fd);
		if (ret) goto cont;
		struct ifreq ifr;
		ret = ioctl(fd, SIOCGIFFLAGS, (void *)&ifr);
		if (ret) goto cont;
		ifr.ifr_flags &= ~IFF_PROMISC;
		ret = ioctl(fd, SIOCSIFFLAGS, (void *)&ifr);
		if (ret) goto cont;
	}
cont:
#endif
	uv_close(udp, uv_on_close);
	debug("%s: uv_close()\n", __FUNCTION__);
}

static int udp_control_open(outlet_t *ol, int family)
{
	debug("%s\n", __FUNCTION__);
	int ret = 0;

	uv_udp_t *udp = malloc(sizeof(uv_udp_t));
	if (!udp)
		return -ENOMEM;

	udp->data = ol;

	ret = uv_udp_init(uv_default_loop(), udp);
	if (ret)
		goto cleanup;

#if PACKET_CAPTURE_ENABLED
	if (family == INET_AF_PACKET)
	{
		int sock = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
		if (sock < 0) {
			ret = errno;
			debug("%s: socket() error: %s\n", __FUNCTION__, strerror(errno));
			goto cleanup;
		}

		ret = uv_udp_open(udp, sock);
		if (ret) {
			debug("%s: uv_udp_open failed: %s\n", __FUNCTION__, uv_strerror(ret));
			goto cleanup;
		}
	}
#endif

	ol->udp = udp;
	ol->family = family;

	return 0;

cleanup:
	free(udp);
	return ret;
}

static int udp_control_bind(outlet_t *ol, ipX_addr_t *addr, uint16_t port)
{
	int ret;
	saddr_t saddr;

	assert(ol->family == INET_AF_INET6 || ol->family == INET_AF_INET);

	debug("%s(addr=0x%x, port=%d)\n", __FUNCTION__, addr->ip4.addr, (int)port);

	if (is_ipv6_outlet(ol))
	{
		saddr.saddr.sa_family = AF_INET6;
		saddr.in6.sin6_port = htons(port);
		memcpy(saddr.in6.sin6_addr.s6_addr, addr, 16);
	}
	else
	{
		saddr.saddr.sa_family = AF_INET;
		saddr.in.sin_port = htons(port);
		saddr.in.sin_addr.s_addr = htonl(addr->ip4.addr);
	}
	ret = uv_udp_bind(ol->udp, &saddr.saddr, 0);
	if (ret) {
		debug("%s: uv_udp_bind failed: %s\n", __FUNCTION__, uv_strerror(ret));
		return -1;
	}

	if (ol->active)
		udp_recv_start(ol);

	saddr_t ip;
	int saddr_len = sizeof(ip);
	ret = uv_udp_getsockname(ol->udp, &ip.saddr, &saddr_len);
	if (ret) return -3;

	switch (ip.saddr.sa_family)
	{
	case AF_INET:
		return ntohs(ip.in.sin_port);
	case AF_INET6:
		return ntohs(ip.in6.sin6_port);
	default:
		debug("%s: getsockname returned unknown family\n", __FUNCTION__);
		return -4;
	}
}

#if PACKET_CAPTURE_ENABLED
static int raw_udp_bind_iface(outlet_t *ol, char *ifname)
{
	int ret = 0;
	int fd;
	struct ifreq iface;
	assert(ol->udp);

	size_t iflen = strlen(ifname);
	assert(iflen + 1 < sizeof(iface.ifr_name)); /* common sense and 0 */

	ret = uv_fileno((uv_handle_t *)ol->udp, &fd);
	if (ret) goto exit;

	/* get interface index */
	memset(&iface, 0, sizeof(struct ifreq));
	strncpy(iface.ifr_name, ifname, IFNAMSIZ);
	ret = ioctl(fd, SIOCGIFINDEX, (void *)&iface);
	if (ret) goto exit;

	ol->raw_ifindex = iface.ifr_ifindex;
	debug("%s: bound to if#%d\n", __FUNCTION__, ol->raw_ifindex);

	struct sockaddr_ll sll;
	sll.sll_family = AF_PACKET;
	sll.sll_protocol = htons(ETH_P_ALL);
	sll.sll_ifindex = iface.ifr_ifindex;

	ret = bind(fd, (struct sockaddr *)&sll, (socklen_t)sizeof(sll));
	if (ret)
		goto exit;

	/* set interface promiscuous */
	ret = ioctl(fd, SIOCGIFFLAGS, (void *)&iface);
	if (ret) goto exit;
	iface.ifr_flags |= IFF_PROMISC;
	ret = ioctl(fd, SIOCSIFFLAGS, (void *)&iface);
	if (ret) goto exit;

	if (ol->active)
		udp_recv_start(ol);

exit:
	if (ret)
		debug("%s(ifname='%s'): failed(%d)\n", __FUNCTION__, ifname, ret);
	return ret;
}
#endif  //PACKET_CAPTURE_ENABLED

#endif //LING_WITH_LIBUV

#ifdef LING_WITH_LWIP

static void lwip_recv_cb(void *arg,
	struct udp_pcb *udp, struct pbuf *data, struct ip_addr *addr, uint16_t port);
static void lwip_recv_timeout_cb(void *arg);

static term_t
send_udp_packet(outlet_t *ol, ip_addr_t *addr, uint16_t port, void *data, uint16_t len)
{
	struct pbuf *packet = pbuf_alloc(PBUF_TRANSPORT, len, PBUF_POOL);
	struct pbuf *pb = packet;
	while (len > 0)
	{
		assert(pb != 0);
		int n = (len > pb->len)
					?pb->len
					:len;
		memcpy(pb->payload, data, n);
		data += n;
		len -= n;
		pb = pb->next;
	}

	//)debug("UDP: sending %d byte(s) to %d.%d.%d.%d:%d\n", len,
	//)	ip4_addr1(&addr), ip4_addr2(&addr), ip4_addr3(&addr), ip4_addr4(&addr), port);

	int rc = udp_sendto(ol->udp, packet, addr, port);
	pbuf_free(packet);
	return (rc ? lwip_err_to_term(rc) : A_OK);
}

static inline int udp_recv_start(outlet_t *ol)
{
	return 0;
}
static inline int udp_recv_stop(outlet_t *ol)
{
	return 0;
}

static inline int udp_recv_set_timeout(outlet_t *ol, unsigned int msecs)
{
	sys_timeout_adj(msecs, lwip_recv_timeout_cb, ol);
	return 0;
}

static inline void udp_recv_untimeout(outlet_t *ol)
{
	sys_untimeout(lwip_recv_timeout_cb, ol);
}


static int udp_control_open(outlet_t *ol, int family)
{
	assert(ol->udp == 0);

#if LWIP_IPV6
	ol->udp = (family == INET_AF_INET6)
		?udp_new_ip6()
		:udp_new();
#else
	if (family != INET_AF_INET)
		return -1;
	ol->udp = udp_new();
#endif
	assert(ol->udp != 0);

	// set the callback that receives messages
	udp_recv(ol->udp, lwip_recv_cb, ol);
	return 0;
}

/* returns assigned local_port */
static int udp_control_bind(outlet_t *ol, ipX_addr_t *addr, uint16_t port)
{
	assert(ol->udp != 0);
	int is_ipv6 = PCB_ISIPV6(ol->udp);
	if (!is_ipv6)
	{
		//debug("UDP: binding %pt to %d.%d.%d.%d:%d\n",
		//	T(ol->oid), data[2], data[3], data[4], data[5], port);
		udp_bind(ol->udp, (ip_addr_t *)addr, port); // always succeeds
	}
	else
	{
#if LWIP_IPV6
		udp_bind_ip6(ol->udp, addr, port); // always succeeds
#else
		return -1;
#endif
	}

	return ol->udp->local_port;
}

static void udp_destroy_private(outlet_t *ol)
{
	if (ol->udp != 0)
		udp_remove(ol->udp);
}

#endif //LING_WITH_LWIP

outlet_t *ol_udp_factory(proc_t *cont_proc, uint32_t bit_opts)
{
	outlet_t *new_ol = outlet_make_N(&ol_udp_vtab, cont_proc, bit_opts, 0);
	if (new_ol == 0)
		return 0;

	inet_set_default_opts(new_ol);

#if PACKET_CAPTURE_ENABLED
	new_ol->raw_ifindex = 0;
#endif

	return new_ol;
}

static uint8_t *ol_udp_get_send_buffer(outlet_t *ol, int len)
{
	if (len > ol->max_send_bufsize)
	{
		nfree(ol->send_buf_node);
		ol->send_buf_node = 0;
		memnode_t *node = nalloc_N(len);
		if (node == 0)
			return 0;
		ol->send_buf_node = node;
		ol->max_send_bufsize = (uint8_t *)node->ends -(uint8_t *)node->starts;
		ol->send_buffer = (uint8_t *)node->starts;
		assert(ol->max_send_bufsize >= len);
	}

	return ol->send_buffer;
}

static int ol_udp_send(outlet_t *ol, int len, term_t reply_to)
{
	// port[2] addr[4] data[n]
	// port[2] addr[16] data[n]
	debug("%s\n", __FUNCTION__);
	assert(ol->udp != 0);

	uint8_t *data = ol->send_buffer;

	ip_addr_t addr = { .addr = 0 };
	uint16_t port = 0;

#if PACKET_CAPTURE_ENABLED
	if (ol->family == INET_AF_PACKET)
	{
		debug("%s(ETH)\n", __FUNCTION__);
	}
	else
#endif
	if (is_ipv6_outlet(ol))
	{
		inet_reply_error(ol->oid, reply_to, A_NOT_SUPPORTED);
		return 0;
	}
	else
	{
		assert(len >= 2);
		port = GET_UINT_16(data);
		data += 2;
		len -= 2;

		assert(len >= 4);
		ip_addr_set((ip_addr_t *)&addr, (ip_addr_t *)data);
		data += 4;
		len -= 4;
		debug("%s(port=0x%04x, addr=0x%x\n", __FUNCTION__, port, addr.addr);
	}

	term_t ret = send_udp_packet(ol, &addr, port, data, len);

	(ret == A_OK ? inet_reply : inet_reply_error)(ol->oid, reply_to, ret);
	return 0;
}

#define REPLY_INET_ERROR(err)	do { \
	*reply++ = INET_REP_ERROR; \
	strcpy(reply, err); \
	reply += strlen(err); \
} while (0)

static term_t ol_udp_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp)
{
	assert(ol->vtab == &ol_udp_vtab);
	debug("%s(%d)\n", __FUNCTION__, op);
	char rbuf[4096];
	char *reply = rbuf;
	int sz;

	//debug("UDP: op %d dlen %d\n", op, dlen);
	switch (op)
	{
	case INET_REQ_OPEN:
	{
		if (dlen != 2)
			goto error;
		if (data[1] != INET_TYPE_DGRAM && data[1] != INET_TYPE_LINK)
			goto error;
		uint8_t family = data[0];
		debug("%s(OPEN, family=%d)\n", __FUNCTION__, family);
		switch (family)
		{
		case INET_AF_INET:
		case INET_AF_INET6:
#if PACKET_CAPTURE_ENABLED
		case INET_AF_PACKET:
#endif
			break;
		default:
			goto error;
		}

		if (udp_control_open(ol, family)) {
			debug("%s(OPEN) failed\n", __FUNCTION__);
			goto error;
		}

		*reply++ = INET_REP_OK;
	}
	break;

	case INET_REQ_PEER:
	{
		REPLY_INET_ERROR("enotconn");
	}
	break;

	case INET_REQ_NAME:
	{
		assert(ol->udp != 0);
#if LING_WITH_LWIP
		*reply++ = INET_REP_OK;
		uint16_t name_port = ol->udp->local_port;
		int is_ipv6 = PCB_ISIPV6(ol->udp);
		*reply++ = (is_ipv6)
			?INET_AF_INET6
			:INET_AF_INET;
		PUT_UINT_16(reply, name_port);
		reply += 2;
		if (!is_ipv6) {
			ip_addr_set_hton((ip_addr_t *)reply, (ip_addr_t *)&ol->udp->local_ip);
			reply += 4;
		} else {
#if LWIP_IPV6
			ip6_addr_set_hton((ip6_addr_t *)reply, (ip6_addr_t *)&ol->udp->local_ip);
			reply += 16;
#else
			goto error;
#endif
		}
#else // LING_WITH_LWIP
		REPLY_INET_ERROR("enotimpl");
#endif  // LING_WITH_LWIP
	}
	break;

	case INET_REQ_BIND:
	{
		uint16_t port = GET_UINT_16(data);
		ipX_addr_t addr;

#if PACKET_CAPTURE_ENABLED
		if (ol->family == INET_AF_PACKET)
		{
			if (raw_udp_bind_iface(ol, (char *)data + 2))
				goto error;

			*reply++ = INET_REP_OK;
			PUT_UINT_16(reply, (uint16_t)0);
			reply += 2;
			break;
		}
#endif
		if (dlen != 2 + (is_ipv6_outlet(ol) ? 16 : 4))
			goto error;

		if (is_ipv6_outlet(ol)) {
			addr.ip6.addr[0] = ntohl(GET_UINT_32(data +2));
			addr.ip6.addr[1] = ntohl(GET_UINT_32(data +2 +4));
			addr.ip6.addr[2] = ntohl(GET_UINT_32(data +2 +8));
			addr.ip6.addr[3] = ntohl(GET_UINT_32(data +2 +12));
		} else {
			addr.ip4.addr = ntohl(GET_UINT_32(data +2));
		}

		debug("%s(BIND): udp_control_bind(0x%x)\n", __FUNCTION__, addr.ip4.addr);
		int local_port = udp_control_bind(ol, &addr, port);
		if (local_port < 0) {
			debug("%s(BIND): udp_control_bind ERR=%d\n", __FUNCTION__, local_port);
			goto error;
		}
		assert(local_port <= 65535);  /* TODO MAX_UINT16 */
		debug("%s(BIND): local_port=%d\n", __FUNCTION__, local_port);

		*reply++ = INET_REP_OK;
		PUT_UINT_16(reply, (uint16_t)local_port);
		reply += 2;
	}
	break;

	case PACKET_REQ_RECV:
	{
		assert(ol->udp != 0);
		if (dlen != 4 +4)
			goto error;

		uint32_t msecs = GET_UINT_32(data);
		// length value ignored

		if (ol->active != INET_PASSIVE)
			goto error;

		assert(ol->cr_in_progress == 0);
		assert(ol->cr_timeout_set == 0);

		int ret = udp_recv_start(ol);
		if (ret) goto error;
		ol->cr_in_progress = 1;

		ol->cr_reply_to = reply_to;
		if (msecs != INET_INFINITY)
		{
			udp_recv_set_timeout(ol, msecs);
			ol->cr_timeout_set = 1;
		}

		*reply++ = INET_REP_OK;
		uint16_t my_ref = 0; //ASYNC_REF;
		PUT_UINT_16(reply, my_ref);
		reply += 2;
		break;
	}

	case INET_REQ_SETOPTS:
	{
		if (ol_udp_set_opts(ol, data, dlen) < 0) {
			debug("%s: ol_udp_set_opts error\n", __FUNCTION__);
			goto error;
		}

		*reply++ = INET_REP_OK;
	}
	break;

	case INET_REQ_GETOPTS:
	{
		sz = ol_udp_get_opts(ol, data, dlen, rbuf+1, sizeof(rbuf) -1);
		if (sz < 0)
			goto error;

		*reply++ = INET_REP_OK;
		reply += sz;
	}
	break;

	case INET_REQ_GETSTAT:
	{
		//
		// lwIP can provide some of the statistics but not all
		//
		REPLY_INET_ERROR("enotsup");
	}
	break;

	case INET_REQ_GETHOSTNAME:
	{
		// why use UDP outlet to get this?
		*reply++ = INET_REP_OK;
		strcpy(reply, my_domain_name);
		reply += strlen(my_domain_name);
	}
	break;

	case INET_REQ_SUBSCRIBE:
	{
		if (dlen != 1 && data[0] != INET_SUBS_EMPTY_OUT_Q) {
			debug("%s(SUBSCRIBE): dlen=%d, data[0]=%d\n", __FUNCTION__, dlen, (int)data[0]);
			goto error;
		}
		//
		// output queue is always empty
		//
		*reply++ = INET_REP_OK;
		*reply++ = INET_SUBS_EMPTY_OUT_Q;
		PUT_UINT_32(reply, 0);
		reply += 4;
	}
	break;

	case INET_REQ_GETIFADDRS:
	{
		if (dlen != 0)
			goto error;

		sz = build_getifaddrs_reply(rbuf, sizeof(rbuf));
		if (sz < 0)
			goto error;
		reply = rbuf +sz;
	}
	break;

	default:
error:
	REPLY_INET_ERROR("einval");
	}

	int rlen = reply -rbuf;
	assert(rlen >= 1 && rlen <= sizeof(rbuf));
	term_t result = heap_str_N(hp, rbuf, rlen);
	if (result == noval)
		return A_NO_MEMORY;

	return result;
}

static void ol_udp_destroy_private(outlet_t *ol)
{
	udp_destroy_private(ol);
	nfree(ol->send_buf_node);
}

static int ol_udp_set_opts(outlet_t *ol, uint8_t *data, int dlen)
{
	uint8_t *p = data;
	int left = dlen;

	while (left > 0)
	{
		int opt = *p++;
		left--;
		if (left < 4)
			return -BAD_ARG;
		uint32_t val = GET_UINT_32(p);
		debug("%s(opt=%d, val=%u)\n", __FUNCTION__, opt, val);
		p += 4;
		left -= 4;

		debug("%s(opt=%d)\n", __FUNCTION__, opt);
		switch (opt)
		{
		case INET_OPT_RCVBUF:
			//TODO
			break;

		case INET_OPT_TOS:
#ifdef LING_WITH_LWIP
			ol->udp->tos = (uint8_t)val;
#endif
			break;

		case INET_LOPT_ACTIVE:
			if (ol->active && val) break;
			if (!ol->active && !val) break;
			if (val)
				udp_recv_start(ol);
			else
				udp_recv_stop(ol);
			/* fallthrough */

		default:
			if (inet_set_opt(ol, opt, val) < 0) {
				debug("%s: inet_set_opt ERR\n",__FUNCTION__);
				return -BAD_ARG;
			}
		}
	}
	return 0;
}

static int ol_udp_get_opts(outlet_t *ol,
					uint8_t *data, int dlen, char *buf, int sz)
{
	uint8_t *p = data;
	char *q = buf;
	int left = sz;

	while (p < data +dlen)
	{
		int opt = *p++;
		uint32_t val;
		debug("%s(opt=%d)\n", __FUNCTION__, opt);

		switch (opt)
		{
		case INET_OPT_RCVBUF:
			//TODO
			val = 0;
			break;

#ifdef LING_WITH_LWIP
		case INET_OPT_TOS:
			val = ol->udp->tos;
			break;
#endif

		default:
			if (inet_get_opt(ol, opt, &val) < 0)
				return -BAD_ARG;
		}

		if (left < 1 +4)
			return -TOO_LONG;
		*q++ = opt;
		left--;
		PUT_UINT_32(q, val);
		q += 4;
		left -= 4;
	}

	return q -buf;
}

//------------------------------------------------------------------------------


static void udp_on_recv_timeout(outlet_t *ol)
{
	phase_expected2(PHASE_EVENTS, PHASE_NEXT);

	assert(ol != 0);
	assert(ol->cr_timeout_set);
	assert(ol->cr_in_progress);

	ol->cr_timeout_set = 0;

	udp_recv_stop(ol);
	ol->cr_in_progress = 0;
	inet_async_error(ol->oid, ol->cr_reply_to, 0 /*ASYNC_REF*/, A_TIMEOUT);
}

static void udp_on_recv(outlet_t *ol, const void *pbuf, const struct sockaddr *addr);

#if LING_WITH_LIBUV

static void uv_on_recv(uv_udp_t *handle, ssize_t nread, const uv_buf_t *buf,
                       const struct sockaddr *addr, unsigned flags)
{
	if (addr == NULL) {
		debug("%s(addr=NULL)\n", __FUNCTION__);
		return;
	}
	if (flags & UV_UDP_PARTIAL) {
		debug("%s: buffer trunkated\n");
	}

	if (addr->sa_family == AF_INET)
	{
		debug("%s(nread=%d, addr=0x%x\n", __FUNCTION__,
		  nread, ((const struct sockaddr_in *)addr)->sin_addr.s_addr);
	}

	uv_buf_t data = { .base = buf->base, .len = nread };

	outlet_t *ol = (outlet_t *)handle->data;
	udp_on_recv(ol, (void *)&data, addr);
}

static void uv_on_recv_timeout(uv_timer_t *timeout)
{
	outlet_t *ol = (outlet_t *)timeout->data;
	ol->conn_timer.data = NULL;

	uv_udp_recv_stop(ol->udp);
	udp_on_recv_timeout(ol);
}

#endif //LING_WITH_LIBUV

#if LING_WITH_LWIP

static void lwip_recv_cb(void *arg,
	struct udp_pcb *udp, struct pbuf *data, struct ip_addr *addr, uint16_t port)
{
	phase_expected(PHASE_EVENTS);
	outlet_t *ol = (outlet_t *)arg;
	if (ol == 0)
		return;		// outlet has gone already

	assert(data != 0);
	assert(ol->udp == udp);

	saddr_t saddr;
	if (PCB_ISIPV6(udp)) {
		ip6_addr_t *addr6 = (ip6_addr_t*)addr;
		saddr.saddr.sa_family = AF_INET6;
		saddr.in6.sin6_port = port;
		uint32_t *saptr = (uint32_t *)saddr.in6.sin6_addr.s6_addr;
		saptr[0] = addr6->addr[0];
		saptr[1] = addr6->addr[1];
		saptr[2] = addr6->addr[2];
		saptr[3] = addr6->addr[3];
	} else {
		saddr.saddr.sa_family = AF_INET;
		saddr.in.sin_port = port;
		saddr.in.sin_addr.s_addr = addr->addr;
	}

	udp_on_recv(ol, (void *)data, &saddr.saddr);
}

static void lwip_recv_timeout_cb(void *arg)
{
	udp_on_recv_timeout((outlet_t *)arg);
}

#endif  //LING_WITH_LWIP

static void udp_on_recv(outlet_t *ol, const void *pbuf, const struct sockaddr *addr)
{
	RECV_PKT_T *data = (RECV_PKT_T *)pbuf;
	size_t dlen = RECV_PKT_LEN(data);

	term_t pid = (ol->cr_in_progress ? ol->cr_reply_to : ol->owner);
	proc_t *cont_proc = scheduler_lookup(pid);
	if (cont_proc == 0)
	{
		debug("%s: cont_proc null\n", __FUNCTION__);
		// nowhere to send
		RECV_PKT_FREE(data);
		return;
	}

	uint8_t *ptr;
	term_t packet = heap_make_bin_N(&cont_proc->hp, dlen, &ptr);
	if (packet == noval)
	{
		RECV_PKT_FREE(data);
		goto no_memory;
	}
	RECV_PKT_COPY(ptr, data, dlen);
	RECV_PKT_FREE(data);

	int is_ipv6 = is_ipv6_outlet(ol);
	uint8_t *addrptr = (is_ipv6
	            ? (uint8_t *)&((struct sockaddr_in6 *)addr)->sin6_addr
	            : (uint8_t *)&((struct sockaddr_in *)addr)->sin_addr);

	if (ol->cr_in_progress)
	{
		udp_recv_stop(ol);
		ol->cr_in_progress = 0;
		if (ol->cr_timeout_set)
		{
			udp_recv_untimeout(ol);
			ol->cr_timeout_set = 0;
		}

		// [ F, P1, P0, A3, A2, A1, A0 | Data ]

		uint8_t header[1 +2 +16];
		int hdr_size;

		header[0] = (is_ipv6 ? INET_AF_INET6 : INET_AF_INET);
		PUT_UINT_16(header +1, sockaddr_port(addr));
		int addr_size = (is_ipv6) ?16 :4;
		memcpy(header +1 +2, addrptr, addr_size);
		hdr_size = 1 +2 +addr_size;

		uint32_t *p = heap_alloc_N(&cont_proc->hp, 2 *hdr_size);
		if (p == 0)
			goto no_memory;
		term_t tail = packet;	// odd list: tail is a binary
		uint8_t *hptr = header +hdr_size;
		do {
			hptr--;
			*p++ = tag_int(*hptr);
			*p++ = tail;
			tail = tag_cons(p -2);
		} while (hptr > header);
		heap_set_top(&cont_proc->hp, p);

		inet_async2(ol->oid, ol->cr_reply_to, 0 /*ASYNC_REF*/, A_OK, tail);
	}
	else if (ol->active != INET_PASSIVE)
	{
		term_t remote_addr;
		if (is_ipv6)
		{
			uint32_t *p = heap_alloc_N(&cont_proc->hp, 1 +8);
			if (p == 0)
				goto no_memory;
			p[0] = 8;
			p[1] = tag_int(get_ipv6_nth(addrptr, 0));
			p[2] = tag_int(get_ipv6_nth(addrptr, 1));
			p[3] = tag_int(get_ipv6_nth(addrptr, 2));
			p[4] = tag_int(get_ipv6_nth(addrptr, 3));
			p[5] = tag_int(get_ipv6_nth(addrptr, 4));
			p[6] = tag_int(get_ipv6_nth(addrptr, 5));
			p[7] = tag_int(get_ipv6_nth(addrptr, 6));
			p[8] = tag_int(get_ipv6_nth(addrptr, 7));
			heap_set_top(&cont_proc->hp, p +1 +8);

			remote_addr = tag_tuple(p);
		}
		else
		{
			uint32_t *p = heap_alloc_N(&cont_proc->hp, 1 +4);
			if (p == 0)
				goto no_memory;
			p[0] = 4;
			/* TODO: test endianness and such */
			p[1] = tag_int(addrptr[0]);
			p[2] = tag_int(addrptr[1]);
			p[3] = tag_int(addrptr[2]);
			p[4] = tag_int(addrptr[3]);
			heap_set_top(&cont_proc->hp, p +1 +4);

			remote_addr = tag_tuple(p);
		}

		// {udp,#Port<0.676>,{127,0,0,1},8790,<<"hey there!">>}
		uint32_t *p = heap_alloc_N(&cont_proc->hp, 1 +5);
		if (p == 0)
			goto no_memory;
		p[0] = 5;
		p[1] = A_UDP;
		p[2] = ol->oid;
		p[3] = remote_addr;
		p[4] = tag_int(sockaddr_port(addr));
		p[5] = packet;
		heap_set_top(&cont_proc->hp, p +1 +5);

		int x = scheduler_new_local_mail_N(cont_proc, tag_tuple(p));
		if (x < 0)
		{
			scheduler_signal_exit_N(cont_proc, ol->oid, err_to_term(x));
			return;
		}

		if (ol->active == INET_ONCE)
		{
			ol->active = INET_PASSIVE;
			udp_recv_stop(ol);
		}
	}

	return;

no_memory:
	scheduler_signal_exit_N(cont_proc, ol->oid, A_NO_MEMORY);
}

//EOF
