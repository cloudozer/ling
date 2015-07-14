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

#include "ling_common.h"

#include "scheduler.h"

#include "outlet.h"
#include "ol_inet.h"

#ifdef LING_WITH_LWIP

#include "lwip/udp.h"
#include "lwip/timers.h"
#undef LWIP_SOCKET
#define LWIP_SOCKET 1
#include "lwip/sockets.h"
#undef LWIP_SOCKET
#include "netif.h"

#elif LING_WITH_LIBUV
# include <uv.h>

/* TODO : don't use malloc/free */
void *malloc(size_t size);
void free(void *ptr);
#endif

#include "atom_defs.h"
#include "getput.h"

#include <string.h>
#include "term_util.h"

static inline int is_ipv6_outlet(outlet_t *ol);
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

#ifdef LING_WITH_LWIP
static void recv_cb(void *arg,
	struct udp_pcb *udp, struct pbuf *data, struct ip_addr *addr, uint16_t port);
static void recv_timeout_cb(void *arg);
#endif

static int ol_udp_set_opts(outlet_t *ol, uint8_t *data, int dlen);
static int ol_udp_get_opts(outlet_t *ol,
					uint8_t *data, int dlen, char *buf, int sz);

#ifdef LING_WITH_LIBUV

static void
uv_on_recv(uv_udp_t *, ssize_t,
           const uv_buf_t *, const struct sockaddr *, unsigned);

static void
uv_on_send(uv_udp_send_t *udp, int status)
{
	debug("%s(status=%d)\n", __FUNCTION__, status);
	free(udp);
}

static term_t
send_udp_packet(outlet_t *ol, ip_addr_t *ipaddr, uint16_t port, void *data, uint16_t len)
{
	int ret;
	uv_buf_t buf = { .base = data, .len = (size_t)len };

	struct sockaddr_in saddr;
	saddr.sin_family = AF_INET;
	saddr.sin_port = htons(port);
	saddr.sin_addr.s_addr = (ipaddr->addr);

	debug("%s(addr=0x%x, port=0x%x)\n", __FUNCTION__, saddr.sin_addr.s_addr, saddr.sin_port);

	uv_udp_send_t *req = malloc(sizeof(uv_udp_send_t));
	ret = uv_udp_send(req, &ol->udp_conn, &buf, 1, (struct sockaddr *)&saddr, uv_on_send);
	if (ret < 0) {
		debug("%s: uv_udp_send failed(%d)\n", __FUNCTION__, ret);
		return A_ERROR; /* TODO: better description */
	}
	return A_OK;
}
#endif

#ifdef LING_WITH_LWIP

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

	//debug("UDP: sending %d byte(s) to %d.%d.%d.%d:%d\n", len,
	//	ip4_addr1(&addr), ip4_addr2(&addr), ip4_addr3(&addr), ip4_addr4(&addr), port);

	int rc = udp_sendto(ol->udp, packet, addr, port);
	pbuf_free(packet);
	return (rc ? lwip_err_to_term(rc) : A_OK);
}
#endif

outlet_t *ol_udp_factory(proc_t *cont_proc, uint32_t bit_opts)
{
	outlet_t *new_ol = outlet_make_N(&ol_udp_vtab, cont_proc, bit_opts, 0);
	if (new_ol == 0)
		return 0;

	inet_set_default_opts(new_ol);

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

#ifdef LING_WITH_LWIP
	assert(ol->udp != 0);
#endif
	uint8_t *data = ol->send_buffer;
	assert(len >= 2);
	uint16_t port = GET_UINT_16(data);
	data += 2;
	len -= 2;

	ip_addr_t addr;

	if (is_ipv6_outlet(ol))
	{
#if 0
		assert(len >= 16);
		((ip6_addr_t *)&addr)->addr[0] = GET_UINT_32(data);
		((ip6_addr_t *)&addr)->addr[1] = GET_UINT_32(data +4);
		((ip6_addr_t *)&addr)->addr[2] = GET_UINT_32(data +8);
		((ip6_addr_t *)&addr)->addr[3] = GET_UINT_32(data +12);
		data += 16;
		len -= 16;
#else
	inet_reply_error(ol->oid, reply_to, A_NOT_SUPPORTED);
		return 0;
#endif
	}
	else
	{
		assert(len >= 4);
		ip_addr_set((ip_addr_t *)&addr, (ip_addr_t *)data);
		data += 4;
		len -= 4;
	}

	term_t ret = send_udp_packet(ol, &addr, port, data, len);
	if (ret == A_OK)
		inet_reply(ol->oid, reply_to, A_OK);
	else
		inet_reply_error(ol->oid, reply_to, ret);

	return 0;
}

#define REPLY_INET_ERROR(err)	do { \
	*reply++ = INET_REP_ERROR; \
	strcpy(reply, err); \
	reply += strlen(err); \
} while (0)

#if LING_WITH_LWIP

static inline int is_ipv6_outlet(outlet_t *ol)
{
	return PCB_ISIPV6(ol->udp);
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
	udp_recv(ol->udp, recv_cb, ol);
	return 0;
}

/* returns assigned local_port */
static int udp_control_bind(outlet_t *ol, ipX_addr_t addr, uint16_t port)
{
	assert(ol->udp != 0);
	int is_ipv6 = PCB_ISIPV6(ol->udp);
	if (!is_ipv6)
	{
		//debug("UDP: binding %pt to %d.%d.%d.%d:%d\n",
		//	T(ol->oid), data[2], data[3], data[4], data[5], port);
		udp_bind(ol->udp, &addr, port); // always succeeds
	}
	else
	{
#if LWIP_IPV6
		udp_bind_ip6(ol->udp, &addr, port); // always succeeds
#else
		return -1;
#endif
	}

	return ol->udp->local_port;
}

#endif

#if LING_WITH_LIBUV

static void on_alloc(uv_handle_t *handle, size_t size, uv_buf_t *buf)
{
	debug("%s(%d)\n", __FUNCTION__, size);
	buf->base = malloc(buf->len);
}

static inline int is_ipv6_outlet(outlet_t *ol)
{
	return ol->family == INET_AF_INET6;
}

static int udp_control_open(outlet_t *ol, int family)
{
	debug("%s\n", __FUNCTION__);
	int ret = uv_udp_init(uv_default_loop(), &ol->udp_conn);
	if (ret)
		return -1;

	ol->family = family;
	return 0;
}

static int udp_control_bind(outlet_t *ol, ipX_addr_t *addr, uint16_t port)
{
	int ret;
	debug("%s(addr=0x%x, port=%d)\n", __FUNCTION__, addr->ip4.addr, (int)port);
	struct sockaddr *saddr;
	if (is_ipv6_outlet(ol))
	{
		struct sockaddr_in6 sa;
		sa.sin6_family = AF_INET6;
		sa.sin6_port = port;
		memcpy(sa.sin6_addr.s6_addr, addr, 16);

		saddr = (struct sockaddr *)&sa;
	}
	else
	{
		struct sockaddr_in sa;
		sa.sin_family = AF_INET;
		sa.sin_port = htons(port);
		sa.sin_addr.s_addr = htonl(addr->ip4.addr);

		saddr = (struct sockaddr *)&sa;
	}
	ret = uv_udp_bind(&ol->udp_conn, saddr, 0);
	if (ret) return -1;

	ret = uv_udp_recv_start(&ol->udp_conn, on_alloc, uv_on_recv);
	if (ret) return -2;

	union {
		struct sockaddr_storage addr;
		struct sockaddr         saddr;
		struct sockaddr_in      v4;
		struct sockaddr_in6     v6;
	} ip;
	int saddr_len = sizeof(ip);
	ret = uv_udp_getsockname(&ol->udp_conn, &ip.saddr, &saddr_len);
	if (ret) return -3;

	switch (ip.addr.ss_family)
	{
	case AF_INET:
		return ntohs(ip.v4.sin_port);
	case AF_INET6:
		return ntohs(ip.v6.sin6_port);
	default:
		debug("%s: getsockname returned unknown family\n", __FUNCTION__);
		return -4;
	}
}

#endif

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
		if (dlen != 2 || data[1] != INET_TYPE_DGRAM)
			goto error;
		uint8_t family = data[0];
		switch (family)
		{
		case INET_AF_INET: case INET_AF_INET6:
			break;
		default:
			goto error;
		}

		if (udp_control_open(ol, family))
			goto error;

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
#if LING_WITH_LWIP
		assert(ol->udp != 0);
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
#ifdef LING_WITH_LWIP
		assert(ol->udp != 0);
		if (dlen != 4 +4)
			goto error;

		uint32_t msecs = GET_UINT_32(data);
		// length value ignored

		if (ol->active != INET_PASSIVE)
			goto error;

		assert(ol->cr_in_progress == 0);
		assert(ol->cr_timeout_set == 0);
		ol->cr_in_progress = 1;
		ol->cr_reply_to = reply_to;
		if (msecs != INET_INFINITY)
		{
			sys_timeout_adj(msecs, recv_timeout_cb, ol);
			ol->cr_timeout_set = 1;
		}

		*reply++ = INET_REP_OK;
		uint16_t my_ref = 0; //ASYNC_REF;
		PUT_UINT_16(reply, my_ref);
		reply += 2;
#else
		REPLY_INET_ERROR("enotimpl");
#endif
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
		assert(sz >= 0);
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

#if LING_WITH_LIBUV
static void uv_on_close(uv_handle_t *handle)
{
	debug("%s\n", __FUNCTION__);
}
#endif

static void ol_udp_destroy_private(outlet_t *ol)
{
	debug("%s\n", __FUNCTION__);
#if LING_WITH_LWIP
	if (ol->udp != 0)
		udp_remove(ol->udp);
#elif LING_WITH_LIBUV
	/* uv_udp_t is a "subclass" of uv_handle_t */
	uv_handle_t *udp = (uv_handle_t *)&ol->udp_conn;
	if (uv_is_active(udp) && !uv_is_closing(udp))
		uv_close(udp, uv_on_close);
	else {
		debug("%s: already closed\n", __FUNCTION__);
	}
#endif
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

#if LING_WITH_LIBUV
static void uv_on_recv(uv_udp_t *handle, ssize_t nread, const uv_buf_t *buf,
                       const struct sockaddr *addr, unsigned flags)
{
	debug("%s(nread=%d, addr=0x%x\n", __FUNCTION__,
	      nread, ((const struct sockaddr_in *)addr)->sin_addr.s_addr);
	/* handle to outlet_t - ? */
}

#endif //LING_WITH_LIBUV

#if LING_WITH_LWIP
static void recv_cb(void *arg,
	struct udp_pcb *udp, struct pbuf *data, struct ip_addr *addr, uint16_t port)
{
	phase_expected(PHASE_EVENTS);
	outlet_t *ol = (outlet_t *)arg;
	if (ol == 0)
		return;		// outlet has gone already

	assert(data != 0);
	//debug("UDP: recv_cb: tot_len %d\n", data->tot_len);
	assert(ol->udp == udp);
	int is_ipv6 = PCB_ISIPV6(ol->udp);

	term_t pid = (ol->cr_in_progress) ?ol->cr_reply_to :ol->owner;
	proc_t *cont_proc = scheduler_lookup(pid);
	if (cont_proc == 0)
	{
		// nowhere to send
		pbuf_free(data);
		return;
	}

	uint8_t *ptr;
	term_t packet = heap_make_bin_N(&cont_proc->hp, data->tot_len, &ptr);
	if (packet == noval)
	{
		pbuf_free(data);
		goto no_memory;
	}
	pbuf_copy_partial(data, ptr, data->tot_len, 0);
	pbuf_free(data);

	if (ol->cr_in_progress)
	{
		ol->cr_in_progress = 0;
		if (ol->cr_timeout_set)
		{
			sys_untimeout(recv_timeout_cb, ol);
			ol->cr_timeout_set = 0;
		}

		// [ F, P1, P0, A3, A2, A1, A0 | Data ]

		uint8_t header[1 +2 +16];
		int hdr_size;

		header[0] = (is_ipv6)
				?INET_AF_INET6
				:INET_AF_INET;
		PUT_UINT_16(header +1, port);
		int addr_size = (is_ipv6) ?16 :4;
		memcpy(header +1 +2, (uint8_t *)addr, addr_size);
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
			ip6_addr_t *six = ip_2_ip6(addr);
			uint32_t *p = heap_alloc_N(&cont_proc->hp, 1 +8);
			if (p == 0)
				goto no_memory;
			p[0] = 8;
			p[1] = tag_int(IP6_ADDR_BLOCK1(six));
			p[2] = tag_int(IP6_ADDR_BLOCK2(six));
			p[3] = tag_int(IP6_ADDR_BLOCK3(six));
			p[4] = tag_int(IP6_ADDR_BLOCK4(six));
			p[5] = tag_int(IP6_ADDR_BLOCK5(six));
			p[6] = tag_int(IP6_ADDR_BLOCK6(six));
			p[7] = tag_int(IP6_ADDR_BLOCK7(six));
			p[8] = tag_int(IP6_ADDR_BLOCK8(six));
			heap_set_top(&cont_proc->hp, p +1 +8);

			remote_addr = tag_tuple(p);
		}
		else
		{
			ip_addr_t *four = addr;
			uint32_t *p = heap_alloc_N(&cont_proc->hp, 1 +4);
			if (p == 0)
				goto no_memory;
			p[0] = 4;
			p[1] = ip4_addr1(four);
			p[2] = ip4_addr2(four);
			p[3] = ip4_addr3(four);
			p[4] = ip4_addr4(four);
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
		p[4] = tag_int(port);
		p[5] = packet;
		heap_set_top(&cont_proc->hp, p +1 +5);

		int x = scheduler_new_local_mail_N(cont_proc, tag_tuple(p));
		if (x < 0)
		{
			scheduler_signal_exit_N(cont_proc, ol->oid, err_to_term(x));
			return;
		}

		if (ol->active == INET_ONCE)
			ol->active = INET_PASSIVE;
	}

	return;

no_memory:
	scheduler_signal_exit_N(cont_proc, ol->oid, A_NO_MEMORY);
}

static void recv_timeout_cb(void *arg)
{
	phase_expected(PHASE_EVENTS);

	outlet_t *ol = (outlet_t *)arg;
	assert(ol != 0);
	assert(ol->cr_timeout_set);
	assert(ol->cr_in_progress);

	ol->cr_timeout_set = 0;
	ol->cr_in_progress = 0;
	inet_async_error(ol->oid, ol->cr_reply_to, 0 /*ASYNC_REF*/, A_TIMEOUT);
}

#endif // LING_WITH_LWIP

//EOF
