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

#include "outlet.h"
#include "ol_inet.h"

#include "ling_common.h"

#ifdef LING_WITH_LWIP

#include "lwip/def.h"
#include "lwip/tcp.h"
#include "lwip/timers.h"
#undef LWIP_SOCKET
#define LWIP_SOCKET 1
#include "lwip/sockets.h"
#undef LWIP_SOCKET

#endif //LING_WITH_LWIP

#if LING_WITH_LIBUV
# include <uv.h>
#endif //LING_WITH_LIBUV

#include "getput.h"
#include "bits.h"
#include "atom_defs.h"
#include "scheduler.h"
#include "term_util.h"

#include <stdbool.h>
#include <string.h>

#define ASYNC_REF	0

#if LING_WITH_LWIP
// dictated by lwIP
# define TCP_MIN_SEND_BUF			TCP_SND_BUF
# define TCP_MIN_RECV_BUF			TCP_WND
#else
# define TCP_MIN_SEND_BUF           1024
# define TCP_MIN_RECV_BUF           2048
#endif

static uint8_t *ol_tcp_get_send_buffer(outlet_t *ol, int len);
static int ol_tcp_send(outlet_t *ol, int len, term_t reply_to);
static term_t ol_tcp_control(outlet_t *ol, uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp);
static int ol_tcp_attach(outlet_t *ol);
static void ol_tcp_detach(outlet_t *ol);
static void ol_tcp_destroy_private(outlet_t *ol);

static outlet_vtab_t ol_tcp_vtab = {
	.get_send_buffer = ol_tcp_get_send_buffer,
	.send = ol_tcp_send,
	.control = ol_tcp_control,
	.attach = ol_tcp_attach,
	.detach = ol_tcp_detach,
	.destroy_private = ol_tcp_destroy_private,
};

outlet_t *ol_tcp_factory(proc_t *cont_proc, uint32_t bit_opts)
{
	int extra = TCP_MIN_SEND_BUF +TCP_MIN_RECV_BUF;
	outlet_t *new_ol = outlet_make_N(&ol_tcp_vtab, cont_proc, bit_opts, extra);
	if (new_ol == 0)
		return 0;

	//printk("TCP outlet occupies %d pages\n", new_ol->home_node->index);

	inet_set_default_opts(new_ol);

	/* Split the unoccupied space in home_node between send_buffer and recv_buffer */
	new_ol->max_send_bufsize = TCP_MIN_SEND_BUF;
	new_ol->send_buffer = (uint8_t *)(new_ol +1);
	new_ol->send_buf_left = 0;
	new_ol->send_reply_to = noval;
	new_ol->send_timeout = INET_INFINITY;

	new_ol->recv_buffer = new_ol->send_buffer +new_ol->max_send_bufsize;
	uint32_t init_size = (uint8_t *)new_ol->home_node->ends -new_ol->recv_buffer;
	assert(init_size >= TCP_MIN_RECV_BUF);
	new_ol->recv_bufsize = new_ol->max_recv_bufsize = init_size;
	new_ol->empty_queue_reply_to = noval;
	new_ol->cr_reply_to = noval;

	/*
	new_ol->tcp = 0;
	new_ol->cr_in_progress = 0;
	new_ol->cr_timeout_set = 0;
	new_ol->peer_close_detected = 0;
	new_ol->recv_buf_node = 0;
	new_ol->recv_buf_off = 0;
	new_ol->recv_expected_size = 0;
	new_ol->empty_queue_in_progress = 0;
	new_ol->send_timeout_set = 0;
	new_ol->send_buf_node = 0;
	new_ol->send_buf_ack = 0;
	new_ol->send_buf_off = 0;
	new_ol->send_in_progress = 0;
	*/

	return new_ol;
}

/*
 *   recv-send status control
 */
static void cr_defer_reply(outlet_t *ol, term_t reply_to, uint32_t millis);
static void cr_cancel_deferred(outlet_t *ol);
static void send_defer_reply(outlet_t *ol, term_t reply_to);
static void send_cancel_deferred(outlet_t *ol);

/*
 *   Generic callbacks
 */
static void cr_timeout_cb(void *arg);
static void send_timeout_cb(void *arg);

static int tcp_on_connected(outlet_t *ol);
static int tcp_on_send(outlet_t *ol);
static int tcp_on_recv(outlet_t *ol, const void *packet);
static void tcp_on_error(outlet_t *ol, term_t reason);

static int recv_bake_packets(outlet_t *ol, proc_t *cont_proc);
static int ol_tcp_set_opts(outlet_t *ol, uint8_t *data, int dlen);
static int ol_tcp_get_opts(outlet_t *ol,
					uint8_t *data, int dlen, char *buf, int sz);

void ol_tcp_acc_promote(outlet_t *ol, int backlog);


#if LING_WITH_LWIP

#define RECV_ACKNOWLEDGE(tcp, len) \
	do tcp_recved((tcp), (len)); while(0)
#define RECV_CHECK_BUF(ol, len)    \
	do assert((len) <= (ol)->recv_bufsize - (ol)->recv_buf_off); while (0)

static err_t lwip_recv_cb(void *arg, struct tcp_pcb *tcp, struct pbuf *data, err_t err);
static err_t lwip_sent_cb(void *arg, struct tcp_pcb *tcp, uint16_t len);
static void lwip_error_cb(void *arg, err_t err);

/*
 *    LWIP connection state
 */
static inline bool
is_outlet_listening(outlet_t *ol)
{
	return ol->tcp->state == LISTEN;
}

static inline bool
is_outlet_connected(outlet_t *ol)
{
	return ol->tcp->state == ESTABLISHED;
}

static inline bool
is_outlet_closed(outlet_t *ol)
{
	return ol->tcp->state == CLOSED;
}

/*
 *    LWIP connection properties
 */
int ol_tcp_set_nodelay(outlet_t *ol, bool nodelay)
{
	if (nodelay)
		tcp_nagle_disable(ol->tcp);
	else
		tcp_nagle_enable(ol->tcp);
	return 0;
}

/*
 *    LWIP timeouts
 */
static void
tcp_set_recv_timeout(outlet_t *ol, uint32_t millis)
{
	debug("%s(millis=%d)\n", __FUNCTION__, millis);
	sys_timeout_adj(millis, cr_timeout_cb, ol);
}

static void
tcp_set_send_timeout(outlet_t *ol, uint32_t millis)
{
	debug("%s(millis=%d)\n", __FUNCTION__, millis);
	sys_timeout_adj(millis, send_timeout_cb, ol);
}

static void
tcp_recv_untimeout(outlet_t *ol)
{
	debug("%s()\n", __FUNCTION__);
	sys_untimeout(cr_timeout_cb, ol);
}

static void
tcp_send_untimeout(outlet_t *ol)
{
	debug("%s()\n", __FUNCTION__);
	sys_untimeout(send_timeout_cb, ol);
}


/*
 *    LWIP callbacks
 */

static err_t connected_cb(void *arg, struct tcp_pcb *tcp, err_t err)
{
	assert(err == ERR_OK);	// argument kept for backward compatibility?
	outlet_t *ol = (outlet_t *)arg;
	tcp_on_connected(ol);
	return ERR_OK;
}

static err_t lwip_recv_cb(void *arg, struct tcp_pcb *tcp, struct pbuf *data, err_t err)
{
	phase_expected(PHASE_EVENTS);

	outlet_t *ol = (outlet_t *)arg;
	if (ol == 0)
		return ERR_OK;		// outlet has gone already
	//debug("---> recv_cb(arg 0x%pp, tcp 0x%pp, %d, err %d)\n",
	//				arg, tcp, (data == 0) ?0: data->tot_len, err);
	assert(ol->tcp == tcp);

	return tcp_on_recv(ol, data);
}

static err_t lwip_sent_cb(void *arg, struct tcp_pcb *tcp, uint16_t len)
{
	//debug("sent_cb: len %d\n", len);
	outlet_t *ol = (outlet_t *)arg;
	if (!ol)
		return ERR_OK; // outlet has gone already

	assert(ol->tcp == tcp);

	ol->send_buf_left -= len;
	ol->send_buf_ack += len;
	assert(ol->send_buf_ack <= ol->send_buf_off);
	assert(ol->send_buf_left >= len);

	return tcp_on_send(ol);
}

static void lwip_error_cb(void *arg, err_t err)
{
	phase_expected(PHASE_EVENTS);

	//debug("**** error_cb(arg 0x%pp, err %d)\n", arg, err);
	outlet_t *ol = (outlet_t *)arg;
	if (ol == 0)
		return;		// outlet already gone

	// from lwIP documentation:
	//
	// The error callback function does not get the pcb passed to it as a
	// parameter since the pcb may already have been deallocated.
	//
	ol->tcp = 0;

	// Do not throw an exception if there is an outstanding request - return an
	// error instead.
	tcp_on_error(ol, lwip_err_to_term(err));
}


/*
 *    LWIP-specific actions
 */

void ol_tcp_animate(outlet_t *new_ol, acc_pend_t *pend)
{
	// lwIP tries hard to allocate a new PCB. If there is not enough memory it
	// first kills TIME-WAIT connections and then active connections. The
	// error_cb callback is used along the way. The callback may sent an exit
	// signal to an outlet and the chain of exit signal may reach the current
	// outlet. To avoid this the priority of all PCBs is set to TCP_PRIO_MAX+1.

	struct tcp_pcb *pcb = pend->pcb;
	struct pbuf *ante = pend->ante;

	tcp_setprio(pcb, TCP_PRIO_MAX +1);

	tcp_arg(pcb, new_ol);	// callback arg
	tcp_recv(pcb, lwip_recv_cb);
	tcp_sent(pcb, lwip_sent_cb);
	tcp_err(pcb, lwip_error_cb);

	new_ol->tcp = pcb;
	if (ante != 0)	// data receive while enqueued
	{
		uint16_t len = ante->tot_len;
		if (len > new_ol->recv_bufsize)
		{
			debug("%s: tot_len=%d, recv_bufsize=%d, truncating\n",
				  __FUNCTION__, ante->tot_len, new_ol->recv_bufsize);
			len = new_ol->recv_bufsize;	
		}
		pbuf_copy_partial(ante, new_ol->recv_buffer, len, 0);
		new_ol->recv_buf_off = len;
		pbuf_free(ante);
	}
}

static int tcp_control_open(outlet_t *ol, int family)
{
#if LWIP_IPV6
	ol->tcp = (family == INET_AF_INET6)
		?tcp_new_ip6()
		:tcp_new();
#else
	if (family != INET_AF_INET)
		return -1;
	ol->tcp = tcp_new();
#endif
	assert(ol->tcp != 0);

	// see comment in ol_tcp_animate()
	tcp_setprio(ol->tcp, TCP_PRIO_MAX +1);

	tcp_arg(ol->tcp, ol);	// callback arg
	tcp_recv(ol->tcp, lwip_recv_cb);
	tcp_sent(ol->tcp, lwip_sent_cb);
	tcp_err(ol->tcp, lwip_error_cb);
	return 0;
}

/*
 *   returns the bound local port on success;
 *            negative error on failure;
 */
static int tcp_control_bind(outlet_t *ol, const inet_sockaddr *saddr)
{
	if (saddr->saddr.sa_family == AF_INET)
	{
		assert(!is_ipv6_outlet(ol));
		ip_addr_t addr;
		sockaddrin_to_ipaddr(&saddr->in, &addr);
		tcp_bind(ol->tcp, &addr, saddr->in.sin_port); // always succeeds
	}
	else
	{
#if LWIP_IPV6
		assert(is_ipv6_outlet(ol));
		ip6_addr_t addr;
		sockaddrin6_to_ip6addr(&saddr->in6, &addr);
		tcp_bind_ip6(ol->tcp, &addr, saddr->in6.sin6_port); // always succeeds
#else
		return -1;
#endif
	}

	return ol->tcp->local_port;
}

static int tcp_control_connect(outlet_t *ol, inet_sockaddr *saddr)
{
	err_t err;
	uint16_t remote_port = sockaddr_port(&saddr->saddr);
	int is_ipv6 = is_ipv6_outlet(ol);
	if (!is_ipv6)
	{
		assert(saddr->saddr.sa_family == AF_INET);
		ip_addr_t where_to;
		sockaddrin_to_ipaddr(&saddr->in, &where_to);
		err = tcp_connect(ol->tcp, &where_to, remote_port, connected_cb);
	}
	else
	{
		assert(saddr->saddr.sa_family == AF_INET6);
#if LWIP_IPV6
		ip6_addr_t where_to;
		sockaddrin6_to_ip6addr(&saddr->in6, &where_to);
		err = tcp_connect_ip6(ol->tcp, &where_to, remote_port, connected_cb);
#else
		return -1;
#endif
	}

	// Does it make connections faster?
	tcp_output(ol->tcp);
	return err;
}

static inline int tcp_control_peername(outlet_t *ol, inet_sockaddr *saddr)
{
	if (PCB_ISIPV6(ol->tcp))
	{
#if LWIP_IPV6
		saddr->saddr.sa_family = AF_INET6;
		saddr->in6.sin6_port = ol->tcp->remote_port;
		memcpy((void *)&saddr->in6.sin6_addr.s6_addr, &ol->tcp->remote_port, 16);
		return 0;
#else
		return -1;
#endif
	}
	saddr->saddr.sa_family = AF_INET;
	saddr->in.sin_port = ol->tcp->remote_port;
	saddr->in.sin_addr.s_addr = ntohl(ol->tcp->remote_ip.ip4.addr);
	return 0;
}

static int tcp_send_buffer(outlet_t *ol)
{
	uint16_t max_len = tcp_sndbuf(ol->tcp);
	uint16_t write_len = ol->send_buf_left;
	if (write_len > max_len)
		write_len = max_len;

	ol->send_buf_off += write_len;

	//debug("ol_tcp_send: tcp_write(%d)\n", write_len);
	int rc = tcp_write(ol->tcp, ol->send_buffer + ol->send_buf_ack,
	                   write_len, TCP_WRITE_FLAG_COPY);
	if (rc != ERR_OK)
		return rc;

	// Otherwise, the data are buffered until the next tcp_tmr timeout
	return tcp_output(ol->tcp);
}

void ol_tcp_close(outlet_t *ol)
{
	if (ol->tcp == NULL)
		return;

	// Sever all outstanding relationships with processes and subsystems
	// - a lwIP PCB
	// - a process waiting for an empty send queue
	// - a process waits until data is sent
	// - a process waiting of a connection or data (optionally, with timeout)
	tcp_arg(ol->tcp, 0);	// quench last lwIP callback calls

	// from lwIP documentation:
	//
	// Closes the connection. The function may return ERR_MEM if no memory was
	// available for closing the connection. If so, the application should wait
	// and try again either by using the acknowledgment callback or the polling
	// functionality. If the close succeeds, the function returns ERR_OK.
	//
	tcp_close(ol->tcp);
	ol->tcp = 0;
}

#endif //LING_WITH_LWIP

#if LING_WITH_LIBUV

#define RECV_CHECK_BUF(ol, len) \
	do {                                                      \
		typeof(len) new_buf_off = (ol)->recv_buf_off + (len); \
		if (new_buf_off > (ol)->recv_bufsize)                 \
			ol_realloc_recvbuf((ol), 2 * (ol)->recv_bufsize); \
	} while(0)

#define tcp_sndqueuelen(tcp)    (((outlet_t*)tcp->data)->send_buf_left)

/*
 *    libuv connection state
 */

static inline bool is_outlet_closed(outlet_t *ol)
{
	return !uv_is_active((uv_handle_t *)ol->tcp);
}

#if 0
static inline bool is_outlet_listening(outlet_t *ol)
{
	return uv_is_active((uv_handle_t *)ol->tcp)
	     && (ol->active == INET_PASSIVE);
}

static inline bool is_outlet_connected(outlet_t *ol)
{
	return uv_is_active((uv_handle_t *)ol->tcp)
	       && (ol->active == INET_ACTIVE);
}
#endif

/*
 *    libuv connection properties
 */
int ol_tcp_set_nodelay(outlet_t *ol, bool nodelay)
{
	ol->nodelay = nodelay;
	return uv_tcp_nodelay(ol->tcp, nodelay);
}


/*
 *    libuv callbacks
 */

static void uv_on_tcp_recv(uv_stream_t *tcp, ssize_t nread, const uv_buf_t *buf)
{
	debug("%s(*%p, nread=%d, dlen=%d)\n", __FUNCTION__, tcp, nread, buf->len);

	outlet_t *ol = (outlet_t *)tcp->data;

	uv_buf_t rcvbuf = { .base = buf->base, .len = nread };
	tcp_on_recv(ol, (nread >= 0 ? &rcvbuf : NULL));
}

static void uv_on_tcp_send(uv_write_t *req, int status)
{
	debug("%s(*%p, status: %s)\n", __FUNCTION__, req, status ? uv_strerror(status) : "ok");

	outlet_t *ol = req->data;
	if (status == 0)
	{
		ol->send_buf_left = 0;
		ol->send_buf_ack = ol->send_buf_off;
		tcp_on_send(ol);
	}
	else
	{
		tcp_on_error(ol, termerror(status));
	}

	free(req);
}

static void uv_on_tcp_recv_timeout(uv_timer_t *timer)
{
	debug("%s(*%p)\n", __FUNCTION__, timer);
	assert(timer->data);

	cr_timeout_cb(timer->data);
	timer->data = NULL;
}

static void uv_on_tcp_send_timeout(uv_timer_t *timer)
{
	debug("%s(*%p)\n", __FUNCTION__, timer);
	assert(timer->data);

	send_timeout_cb(timer->data);
	timer->data = NULL;
}

static void uv_on_recv_alloc(uv_handle_t *tcp, size_t suggested_size, uv_buf_t *buf)
{
	assert(tcp);
	outlet_t *ol = (outlet_t *)tcp->data;
	assert(ol);
	debug("%s(recv_bufsize=%d, max_recv_bufsize=%d, recv_buf_off=%d)\n",
		  __FUNCTION__, ol->recv_bufsize, ol->max_recv_bufsize, ol->recv_buf_off);

	buf->len = ol->max_recv_bufsize;
	buf->base = malloc(buf->len);
}

static void uv_on_tcp_connect(uv_connect_t *req, int status)
{
	outlet_t *ol = (outlet_t *)req->data;
	assert(ol);
	assert(ol->tcp);
	int ret;

	debug("%s(status: (%d) %s)\n", __FUNCTION__, status, status ? uv_strerror(status) : "ok");
	if (status == -125)
		return; /* outlet is likely to be freed already, TODO */
	if (status)
	{
		tcp_on_error(ol, A_LWIP_CONN);
		return;
	}
	tcp_on_connected(ol);

	/* start recv */
	ret = uv_read_start((uv_stream_t *)ol->tcp, uv_on_recv_alloc, uv_on_tcp_recv);
	if (ret) debug("%s: uv_read_start failed(%s)\n", __FUNCTION__, uv_strerror(ret));

	free(req);
}

static void uv_on_tcp_closed(uv_handle_t *tcp)
{
	debug("%s(*%p)\n", __FUNCTION__, tcp);
	free(tcp);
}

/*
 *    libuv timers and timeouts
 */

static inline void
set_uv_timeout(outlet_t *ol, uv_timer_t *timer, uv_timer_cb cb, uint32_t timeout)
{
	assert(timer->data == NULL);
	int ret;

	ret = uv_timer_init(uv_default_loop(), timer);
	if (ret) return;

	ret = uv_timer_start(timer, cb, (uint64_t)timeout, 0);
	if (ret) return;

	timer->data = ol;
}

static inline void
unset_uv_timeout(outlet_t *ol, uv_timer_t *timer)
{
	uv_timer_stop(timer);
	timer->data = NULL;
}

static void tcp_set_recv_timeout(outlet_t *ol, uint32_t millis)
{
	set_uv_timeout(ol, &ol->conn_timer, uv_on_tcp_recv_timeout, millis);
}

static void tcp_set_send_timeout(outlet_t *ol, uint32_t millis)
{
	set_uv_timeout(ol, &ol->send_timer, uv_on_tcp_send_timeout, millis);
}

static void tcp_recv_untimeout(outlet_t *ol)
{
	unset_uv_timeout(ol, &ol->conn_timer);
}

static void tcp_send_untimeout(outlet_t *ol)
{
	unset_uv_timeout(ol, &ol->send_timer);
}


/*
 *    libuv-specific actions
 */

void ol_tcp_animate(outlet_t *new_ol, acc_pend_t *pend)
{
	assert(new_ol);
	assert(new_ol->tcp == 0);

	pend->tcp->data = new_ol;

	new_ol->tcp = pend->tcp;

	void *data = pend->ante.base;
	if (data)	// data received while enqueued
	{
		size_t len = pend->ante.len;
		if (len > new_ol->recv_bufsize)
		{
			debug("%s: recv_bufsize=%d and len=%d, truncating\n", __FUNCTION__,
			      new_ol->recv_bufsize, len);
			len = new_ol->recv_bufsize;
		}
		memcpy(new_ol->recv_buffer, data, len);
		free(data);

		new_ol->recv_buf_off = len;
	}
}

static int tcp_control_open(outlet_t *ol, int family)
{
	int ret;
	uv_tcp_t *tcp = malloc(sizeof(uv_tcp_t));
	if (!tcp)
		return -ENOMEM;

	tcp->data = ol;

	ret = uv_tcp_init(uv_default_loop(), tcp);
	if (ret) {
		free(tcp);
		return ret;
	}

	ol->tcp = tcp;
	ol->family = family;
	return 0;
}

static int tcp_control_bind(outlet_t *ol, const inet_sockaddr *saddr)
{
	assert(ol->tcp);
	assert((saddr->saddr.sa_family == AF_INET6 && ol->family == INET_AF_INET6)
	    || (saddr->saddr.sa_family == AF_INET && ol->family == INET_AF_INET));
	int ret;

	ret = uv_tcp_bind(ol->tcp, &saddr->saddr, 0); // UV_TCP_IPV6ONLY
	if (ret) return -1;

#if LING_DEBUG
	char showbuf[64];
	if (saddr->saddr.sa_family == AF_INET6) {
		uv_ip6_name(&saddr->in6, showbuf, 64);
	} else if (saddr->saddr.sa_family == AF_INET) {
		uv_ip4_name(&saddr->in, showbuf, 64);
	};
	debug("%s(addr=%s : %d)\n", __FUNCTION__, showbuf, sockaddr_port(&saddr->saddr));
#endif

	/* get local port */
	inet_sockaddr srcaddr = { .saddr.sa_family = AF_INET };
	int slen = 0;
	ret = uv_tcp_getsockname(ol->tcp, &srcaddr.saddr, &slen);
	if (ret) return -2;

	return sockaddr_port(&srcaddr.saddr);
}

static int tcp_control_connect(outlet_t *ol, inet_sockaddr *saddr)
{
	int ret;
	uv_connect_t *conn_req = malloc(sizeof(uv_connect_t));
	if (!conn_req)
		return -ENOMEM;

	conn_req->data = ol;

#if LING_DEBUG
	char showbuf[64];
	if (saddr->saddr.sa_family == AF_INET6) {
		uv_ip6_name(&saddr->in6, showbuf, 64);
	} else if (saddr->saddr.sa_family == AF_INET) {
		uv_ip4_name(&saddr->in, showbuf, 64);
	};
	debug("%s(addr=%s : %d)\n", __FUNCTION__, showbuf, sockaddr_port(&saddr->saddr));
#endif

	ret = uv_tcp_connect(conn_req, ol->tcp, &saddr->saddr, uv_on_tcp_connect);
	if (ret) {
		free(conn_req);
		return ret;
	}
	return 0;
}

static inline int tcp_control_peername(outlet_t *ol, inet_sockaddr *saddr)
{
	assert(ol->tcp);
	int namelen = sizeof(inet_sockaddr);
	//memset(saddr, 0, sizeof(inet_sockaddr));
	int ret = uv_tcp_getpeername(ol->tcp, &saddr->saddr, &namelen);
	if (ret) debug("%s() failed: %s\n", __FUNCTION__, uv_strerror(ret));
	return ret;
}

static int tcp_send_buffer(outlet_t *ol)
{
	uv_write_t *req = malloc(sizeof(uv_write_t));
	if (!req) return -1;
	req->data = ol;

	uv_buf_t buf = { .base = (char *)ol->send_buffer, .len = ol->send_buf_left };

	return uv_write(req, (uv_stream_t *)ol->tcp, &buf, 1, uv_on_tcp_send);
}

void ol_tcp_close(outlet_t *ol)
{
	uv_close((uv_handle_t *)ol->tcp, uv_on_tcp_closed);
}

#endif //LING_WITH_LIBUV

static uint8_t *ol_tcp_get_send_buffer(outlet_t *ol, int len)
{
	assert(ol->vtab == &ol_tcp_vtab);
	int buf_len = len;

	switch (ol->packet)
	{
	case TCP_PB_1: buf_len = len +1; break;
	case TCP_PB_2: buf_len = len +2; break;
	case TCP_PB_4: buf_len = len +4; break;
	}

	if (buf_len > ol->max_send_bufsize)
	{
		debug("%s: reallocating send_buffer, new size = %d\n", __FUNCTION__, len);
		nfree(ol->send_buf_node);
		ol->send_buf_node = 0;
		memnode_t *node = nalloc_N(buf_len);
		if (node == 0)
			return 0;
		ol->send_buf_node = node;
		ol->max_send_bufsize = (uint8_t *)node->ends -(uint8_t *)node->starts;
		ol->send_buffer = (uint8_t *)node->starts;
		assert(ol->max_send_bufsize >= buf_len);
	}

	switch (ol->packet)
	{
	case TCP_PB_1: return ol->send_buffer +1;
	case TCP_PB_2: return ol->send_buffer +2;
	case TCP_PB_4: return ol->send_buffer +4;
	default: return ol->send_buffer;
	}
}

static int ol_tcp_send(outlet_t *ol, int len, term_t reply_to)
{
	debug("%s(len=%d)\n", __FUNCTION__, len);
	assert(ol->vtab == &ol_tcp_vtab);
	assert(ol->send_in_progress == 0);
	assert(ol->send_buf_left == 0);
	//debug("ol_tcp_send: len %d\n", len);
	// TCP_PB_1
	// TCP_PB_2
	// TCP_PB_4
	
	int buf_len = len;
	switch (ol->packet)
	{
	case TCP_PB_1:
		if (len < 0 || len > 255)
			return -BAD_ARG;
		ol->send_buffer[0] = len;
		buf_len = len +1;
		break;
	case TCP_PB_2:
		if (len < 0 || len > 65535)
			return -BAD_ARG;
		PUT_UINT_16(ol->send_buffer, len);
		buf_len = len +2;
		break;
	case TCP_PB_4:
		if (len < 0)
			return -BAD_ARG;
		PUT_UINT_32(ol->send_buffer, len);
		buf_len = len +4;
		break;
	}

	assert(buf_len <= ol->max_send_bufsize);

	ol->send_buf_left = buf_len;
#if LING_WITH_LWIP
	uint16_t max_len = tcp_sndbuf(ol->tcp);
	uint16_t write_len = (buf_len > max_len) ?max_len :buf_len;
	ol->send_buf_off = write_len;
#endif
	ol->send_buf_ack = 0;       // start transmission from the start of the buffer

	int ret = tcp_send_buffer(ol);
	if (ret)
	{
		debug("%s(): tcp_send_buffer() failed(%d)\n", __FUNCTION__, ret);
		inet_reply_error(ol->oid, reply_to, termerror(ret));
		return 0;
	}

	// may set a timeout
	send_defer_reply(ol, reply_to);
	return 0;
}

#define REPLY_INET_ERROR(err)	do { \
	*reply++ = INET_REP_ERROR; \
	strcpy(reply, err); \
	reply += strlen(err); \
} while (0)

static term_t ol_tcp_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp)
{
	char rbuf[256];
	char *reply = rbuf;
	int sz;
	int ret;

	debug("%s(op=%d)\n", __FUNCTION__, op);
	assert(ol != 0);
	assert(ol->vtab == &ol_tcp_vtab);
	assert(ol->tcp != 0 || op == INET_REQ_OPEN
	       || op == INET_REQ_SUBSCRIBE || op == INET_REQ_SETOPTS);

	switch (op)
	{
	case INET_REQ_OPEN:
	{
		assert(ol->tcp == 0);

		if (dlen != 2 || data[1] != INET_TYPE_STREAM)
			goto error;
		uint8_t family = data[0];
		if (family != INET_AF_INET && family != INET_AF_INET6)
			goto error;

	ret = tcp_control_open(ol, family);
	if (ret)
		goto error;

		*reply++ = INET_REP_OK;
	}
	break;

	case INET_REQ_CONNECT:
	{
		int is_ipv6 = is_ipv6_outlet(ol);
		if ((is_ipv6 && dlen != 4 +2 +16) || (!is_ipv6 && dlen != 4 +2 +4))
			goto error;

		uint32_t timeout = GET_UINT_32(data);
		uint16_t remote_port = htons(GET_UINT_16(data +4));

		inet_sockaddr saddr;
		if (is_ipv6) {
			saddr.saddr.sa_family = AF_INET6;
			saddr.in6.sin6_port = remote_port;
			uint32_t *in6addr = (uint32_t *)saddr.in6.sin6_addr.s6_addr;
			in6addr[0] = ntohl(GET_UINT_32(data +4 +2));
			in6addr[1] = ntohl(GET_UINT_32(data +4 +2 +4));
			in6addr[2] = ntohl(GET_UINT_32(data +4 +2 +8));
			in6addr[3] = ntohl(GET_UINT_32(data +4 +2 +12));
		} else {
			saddr.saddr.sa_family = AF_INET;
			saddr.in.sin_port = remote_port;
			saddr.in.sin_addr.s_addr = ntohl(GET_UINT_32(data +4 +2));
		}

		ret = tcp_control_connect(ol, &saddr);
		if (ret == 0)
		{
			cr_defer_reply(ol, reply_to, timeout);

			*reply++ = INET_REP_OK;
			uint16_t ref = ASYNC_REF;
			PUT_UINT_16(reply, ref);
			reply += 2;
		}
#if LING_WITH_LWIP
		else if (ret == ERR_RTE)
			REPLY_INET_ERROR("eunreach");
		else
		{
			assert(ret == ERR_MEM);
			REPLY_INET_ERROR("enomem");
		}
#endif
#if LING_WITH_LIBUV
		else
			goto error;
#endif
	}
	break;

	case INET_REQ_PEER:
	if (is_outlet_closed(ol))
		REPLY_INET_ERROR("enotconn");
	else
	{
		inet_sockaddr peeraddr;
		int ret = tcp_control_peername(ol, &peeraddr);
		if (ret) goto error;

		switch (peeraddr.saddr.sa_family)
		{
		case AF_INET:
			debug("%s(PEER): AF_INET\n", __FUNCTION__);
			*reply++ = INET_REP_OK;
			*reply++ = INET_AF_INET;

			PUT_UINT_16(reply, ntohs(peeraddr.in.sin_port));
			reply += 2;

			PUT_UINT_32(reply, ntohl(peeraddr.in.sin_addr.s_addr));
			reply += 4;
			break;
		case AF_INET6:
			debug("%s(PEER): AF_INET6\n", __FUNCTION__);
			*reply++ = INET_REP_OK;
			*reply++ = INET_AF_INET6;

			PUT_UINT_16(reply, ntohs(peeraddr.in6.sin6_port));
			reply += 2;

			memcpy(reply, peeraddr.in6.sin6_addr.s6_addr, 16);
			reply += 16;
			break;
		default: goto error;
		}
	}
	break;

	case INET_REQ_NAME:
#if LING_WITH_LWIP
	if (is_outlet_closed(ol))
		REPLY_INET_ERROR("enotconn");
	else
	{
		*reply++ = INET_REP_OK;
		int is_ipv6 = is_ipv6_outlet(ol);
		*reply++ = (is_ipv6) ?INET_AF_INET6 :INET_AF_INET;
		uint16_t name_port = ol->tcp->local_port;
		PUT_UINT_16(reply, name_port);
		reply += 2;
		if (is_ipv6)
		{
			ip_addr_set_hton((ip_addr_t *)reply, (ip_addr_t *)&ol->tcp->local_ip);
			reply += 4;
		}
		else
		{
#if LWIP_IPV6
			ip6_addr_set_hton((ip6_addr_t *)reply, (ip6_addr_t *)&ol->tcp->local_ip);
			reply += 16;
#else
			goto error;
#endif
		}
	}
#else  //!LING_WITH_LWIP
	REPLY_INET_ERROR("enotimpl");
#endif
	break;

	case INET_REQ_BIND:
	{
		inet_sockaddr saddr;
		int is_ipv6 = is_ipv6_outlet(ol);
		if ((is_ipv6 && dlen != 2 +16) || (!is_ipv6 && dlen != 2 +4))
			goto error;
		uint16_t port = GET_UINT_16(data);

		if (!is_ipv6)
		{
			saddr.saddr.sa_family = AF_INET;
			saddr.in.sin_port = htons(port);
			saddr.in.sin_addr.s_addr = ntohl(GET_UINT_32(data +2));
		}
		else
		{
			saddr.saddr.sa_family = AF_INET6;
			saddr.in6.sin6_port = htons(port);
			uint32_t *sin6addr = (uint32_t *)saddr.in6.sin6_addr.s6_addr;
			sin6addr[0] = ntohl(GET_UINT_32(data +2));
			sin6addr[1] = ntohl(GET_UINT_32(data +2 +4));
			sin6addr[2] = ntohl(GET_UINT_32(data +2 +8));
			sin6addr[3] = ntohl(GET_UINT_32(data +2 +12));

		}
		int ret = tcp_control_bind(ol, &saddr);
		if (ret < 0)
			goto error;

		uint16_t local_port = (uint16_t)port;
		*reply++ = INET_REP_OK;
		PUT_UINT_16(reply, local_port);
		reply += 2;
	}
	break;

	case INET_REQ_LISTEN:
	{
		//XXX: how rec_buf_size option gets inherited?

		//assert(ol->recv_buf_node == 0);	// or use destroy_private()
		int backlog = GET_UINT_16(data);
		ol_tcp_acc_promote(ol, backlog);
		*reply++ = INET_REP_OK;
	}
	break;

	case INET_REQ_SETOPTS:
	if (ol_tcp_set_opts(ol, data, dlen) < 0)
		goto error;

	*reply++ = INET_REP_OK;
	break;

	case INET_REQ_GETOPTS:
	sz = ol_tcp_get_opts(ol, data, dlen, rbuf+1, sizeof(rbuf) -1);
	if (sz < 0)
		goto error;

	*reply++ = INET_REP_OK;
	reply += sz;
	break;

	case INET_REQ_GETSTAT:
	//
	// lwIP can provide some of the statistics but not all
	//
	REPLY_INET_ERROR("enotsup");
	break;

	case INET_REQ_SUBSCRIBE:
	if (dlen != 1 && data[0] != INET_SUBS_EMPTY_OUT_Q) {
		debug("%s(SUBSCRIBE): error (1)\n", __FUNCTION__);
		goto error;
	}
	if (ol->empty_queue_in_progress) {
		debug("%s(SUBSCRIBE): error (2)\n", __FUNCTION__);
		goto error;		//TODO: allow multiple subscriptions
	}

	int qlen = tcp_sndqueuelen(ol->tcp);
	if (qlen > 0)
	{
		ol->empty_queue_in_progress = 1;
		ol->empty_queue_reply_to = reply_to;
	}

	*reply++ = INET_REP_OK;
	*reply++ = INET_SUBS_EMPTY_OUT_Q;
	PUT_UINT_32(reply, qlen);
	reply += 4;
	break;

	case TCP_REQ_RECV:
	if (dlen != 4 +4)
		goto error;

	uint32_t msecs = GET_UINT_32(data);
	uint32_t recv_num = GET_UINT_32(data +4);

	if (ol->active != INET_PASSIVE) {
		debug("%s(RECV): ol->active is not INET_PASSIVE\n", __FUNCTION__);
		goto error;
	}
	if (ol->packet == TCP_PB_RAW && recv_num > ol->recv_bufsize) {
		debug("%s(RECV): ol->packet, recv_num=%d, ol->recv_bufsize=%d\n", __FUNCTION__,
			  recv_num, ol->recv_bufsize);
		goto error;
	}
	
	if (ol->peer_close_detected && ol->recv_buf_off == 0)
		inet_async_error(ol->oid, reply_to, ASYNC_REF, A_CLOSED);
	else
	{
		cr_defer_reply(ol, reply_to, msecs);

		if (ol->packet == TCP_PB_RAW)
			ol->recv_expected_size = recv_num;

		// Enough data may have already been buffered
		proc_t *cont_proc = scheduler_lookup(reply_to);
		assert(cont_proc != 0);
		if (recv_bake_packets(ol, cont_proc) < 0) {
			debug("%s(RECV): recv_bake_packets() failed\n");
			goto error;
		}
	}

	*reply++ = INET_REP_OK;
	uint16_t my_ref = ASYNC_REF;
	PUT_UINT_16(reply, my_ref);
	reply += 2;
	break;

#if LING_WITH_LWIP
	case TCP_REQ_SHUTDOWN:
	if (dlen != 1)
		goto error;

	uint8_t what = data[0];
	// 0 - read
	// 1 - write
	// 2 - read_write
	
	int shut_rx = (what == 0) || (what == 2);
	int shut_tx = (what == 1) || (what == 2);

	if (is_outlet_listening(ol))
		REPLY_INET_ERROR("enotconn");
	else
	{
		tcp_shutdown(ol->tcp, shut_rx, shut_tx);
		// TODO: return code ignored

		*reply++ = INET_REP_OK;
	}
	break;
#endif

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

static int ol_tcp_attach(outlet_t *ol)
{
	assert(ol->vtab == &ol_tcp_vtab);
	return 0;
}

static void ol_tcp_detach(outlet_t *ol)
{
	debug("%s(*%p)\n", __FUNCTION__, ol);
	assert(ol->vtab == &ol_tcp_vtab);

	//
	// Take care as the oultet may be closed by
	// inet_reply_error/inet_async_error() if the memory is tight.
	//
	ol_tcp_close(ol);

	//
	// Outstanding empty queue subscriptions ignored - prim_inet:close() exits
	// after a timeout.
	//

	term_t send_reply_to = noval;
	term_t cr_reply_to = noval;

	if (ol->send_in_progress)
	{
		send_cancel_deferred(ol);
		send_reply_to = ol->send_reply_to;
	}

	if (ol->cr_in_progress)
	{
		cr_cancel_deferred(ol);
		cr_reply_to = ol->cr_reply_to;
	}

	term_t saved_oid = ol->oid;

	if (send_reply_to != noval)
		inet_reply_error(saved_oid, send_reply_to, A_CLOSED);

	if (cr_reply_to != noval)
		inet_async_error(saved_oid, cr_reply_to, ASYNC_REF, A_CLOSED);
}

static void ol_tcp_destroy_private(outlet_t *ol)
{
	debug("%s(*%p)\n", __FUNCTION__, ol);
	assert(ol->vtab == &ol_tcp_vtab);

	nfree(ol->send_buf_node);
	nfree(ol->recv_buf_node);
}

static void cr_timeout_cb(void *arg)
{
	phase_expected2(PHASE_EVENTS, PHASE_NEXT);

	outlet_t *ol = (outlet_t *)arg;
	assert(ol != 0);
	assert(ol->cr_timeout_set);
	assert(ol->cr_in_progress);
	ol->cr_timeout_set = 0;
	ol->cr_in_progress = 0;
	inet_async_error(ol->oid, ol->cr_reply_to, ASYNC_REF, A_TIMEOUT);
}

static void send_timeout_cb(void *arg)
{
	phase_expected2(PHASE_EVENTS, PHASE_NEXT);

	outlet_t *ol = (outlet_t *)arg;
	assert(ol != 0);
	assert(ol->send_timeout_set);
	assert(ol->send_in_progress);
	ol->send_timeout_set = 0;
	ol->send_in_progress = 0;
	inet_async_error(ol->oid, ol->send_reply_to, ASYNC_REF, A_TIMEOUT);
}

static void cr_defer_reply(outlet_t *ol, term_t reply_to, uint32_t millis)
{
	assert(ol->cr_in_progress == 0);
	assert(ol->cr_timeout_set == 0);
	ol->cr_in_progress = 1;
	ol->cr_reply_to = reply_to;
	if (millis != INET_INFINITY)
	{
		tcp_set_recv_timeout(ol, millis);
		ol->cr_timeout_set = 1;
	}
}

static void send_defer_reply(outlet_t *ol, term_t reply_to)
{
	assert(ol->send_in_progress == 0);
	assert(ol->send_timeout_set == 0);
	ol->send_in_progress = 1;
	ol->send_reply_to = reply_to;
	if (ol->send_timeout != INET_INFINITY)
	{
		tcp_set_send_timeout(ol, ol->send_timeout);
		ol->send_timeout_set = 1;
	}
}

static void cr_cancel_deferred(outlet_t *ol)
{
	assert(ol->cr_in_progress);
	ol->cr_in_progress = 0;
	if (ol->cr_timeout_set)
	{
		tcp_recv_untimeout(ol);
		ol->cr_timeout_set = 0;
	}
}

static void send_cancel_deferred(outlet_t *ol)
{
	assert(ol->send_in_progress);
	ol->send_in_progress = 0;
	if (ol->send_timeout_set)
	{
		tcp_send_untimeout(ol);
		ol->send_timeout_set = 0;
	}
}

static int tcp_on_connected(outlet_t *ol)
{
	phase_expected2(PHASE_EVENTS, PHASE_NEXT);

	assert(ol != 0);

	//debug("connected_cb: %pt\n", T(ol->oid));
	assert(ol->cr_in_progress);

	cr_cancel_deferred(ol);
	inet_async(ol->oid, ol->cr_reply_to, ASYNC_REF, A_OK);
	return 0;
}

static int tcp_on_send(outlet_t *ol)
{
	phase_expected2(PHASE_EVENTS, PHASE_NEXT);

	// inet_reply() may close the outlet
	term_t saved_oid = ol->oid;
	term_t send_reply_to = noval;
	term_t send_error = noval;
	term_t empty_queue_reply_to = noval;

	assert(ol->send_in_progress);

	if (ol->send_buf_ack == ol->send_buf_off)
	{
		if (ol->send_buf_left > 0)
		{
			// transmit the next slice of the buffer
			int rc = tcp_send_buffer(ol);
			if (rc)
			{
				send_cancel_deferred(ol);
				send_reply_to = ol->send_reply_to;
				send_error = termerror(rc);
			}
		}
		else
		{
			send_cancel_deferred(ol);
			send_reply_to = ol->send_reply_to;
		}
	}

	if (ol->empty_queue_in_progress && tcp_sndqueuelen(ol->tcp) == 0)
	{
		ol->empty_queue_in_progress = 0;
		empty_queue_reply_to = ol->empty_queue_reply_to;
	}

	if (send_reply_to != noval && send_error != noval)
		inet_reply_error(saved_oid, send_reply_to, send_error);
	else if (send_reply_to != noval)
		inet_reply(saved_oid, send_reply_to, A_OK);
	
	if (empty_queue_reply_to != noval)
	{
		// non-standard reply
		proc_t *caller = scheduler_lookup(empty_queue_reply_to);
		if (caller != 0)
		{
			// {empty_out_q,S}
			uint32_t *p = heap_alloc_N(&caller->hp, 1 +2);
			if (p == 0)
				scheduler_signal_exit_N(caller, saved_oid, A_NO_MEMORY);
			else
			{
				heap_set_top(&caller->hp, p +1 +2);
				p[0] = 2;
				p[1] = A_EMPTY_OUT_Q;
				p[2] = saved_oid;
				term_t msg = tag_tuple(p);

				int x = scheduler_new_local_mail_N(caller, msg);
				if (x < 0)
					scheduler_signal_exit_N(caller, saved_oid, err_to_term(x));
			}
		}
	}

	return 0;
}

static int tcp_on_recv(outlet_t *ol, const void *packet)
{
	RECV_PKT_T *data = (RECV_PKT_T *)packet;
	debug("%s(len=%d)\n", __FUNCTION__, (data ? RECV_PKT_LEN(data) : -1));

	term_t pid = (ol->cr_in_progress) ?ol->cr_reply_to :ol->owner;
	proc_t *cont_proc = scheduler_lookup(pid);
	if (cont_proc == 0)
	{
		debug("recv_cb: nowhere to send - discard\n");
		if (data != 0)
			RECV_PKT_FREE(data);
		return 0;
	}

	if (data == 0)
	{
		if (ol->active != INET_PASSIVE)
		{
			// deliver {tcp_closed,Sock}
			uint32_t *p = heap_alloc_N(&cont_proc->hp, 1 +2);
			if (p == 0)
				scheduler_signal_exit_N(cont_proc, ol->oid, A_NO_MEMORY);
			else
			{
				p[0] = 2;
				p[1] = A_TCP_CLOSED;
				p[2] = ol->oid;
				heap_set_top(&cont_proc->hp, p +1 +2);
				int x = scheduler_new_local_mail_N(cont_proc, tag_tuple(p));
				if (x < 0)
					scheduler_signal_exit_N(cont_proc, ol->oid, err_to_term(x));
			}

			if (ol->active == INET_ONCE)
				ol->active = INET_PASSIVE;
		}
		else if (ol->cr_in_progress)
		{
			cr_cancel_deferred(ol);
			inet_async2(ol->oid, ol->cr_reply_to, ASYNC_REF, A_ERROR, A_CLOSED);
		}

		// No more data will be received, otherwise it is a normal connection.
		// No need to do tcp_close() or anything.
		ol->peer_close_detected = 1;
		debug("%s: peer_close_detected <- 1\n", __FUNCTION__);
	}
	else
	{
		uint16_t len = RECV_PKT_LEN(data);
		debug("---> recv_cb: recv_bufsize=%d, recv_buf_off=%d, tot_len=%d, len=%d\n",
				ol->recv_bufsize, ol->recv_buf_off, RECV_PKT_LEN(data), len);

		RECV_CHECK_BUF(ol, len);
		RECV_PKT_COPY(ol->recv_buffer + ol->recv_buf_off, data, len);
		ol->recv_buf_off += len;

		RECV_PKT_FREE(data);
		int x = recv_bake_packets(ol, cont_proc);
		if (x < 0) {
			debug("%s: recv_bake_packets() faled(%d)\n", __FUNCTION__, x);
			scheduler_signal_exit_N(cont_proc, ol->oid, err_to_term(x));
		}
	}

	return 0;
}

static void tcp_on_error(outlet_t *ol, term_t reason)
{
	if (ol->cr_in_progress)
	{
		cr_cancel_deferred(ol);
		inet_async_error(ol->oid, ol->cr_reply_to, ASYNC_REF, reason);
	}
	else
		outlet_signal_exit_N(ol, ol->oid, reason);
}

//NB: called both from a callback and a BIF - do not send signals
static int recv_bake_packets(outlet_t *ol, proc_t *cont_proc)
{
	term_t reason = noval;
	term_t packet = noval;
	term_t active_tag = A_TCP;
	debug("%s(recv_buf_off=%d, cr_in_progress=%d)\n", __FUNCTION__,
	      ol->recv_buf_off, ol->cr_in_progress);

more_packets:
	if (ol->cr_in_progress || ol->active != INET_PASSIVE)
	{
		if (ol->packet == TCP_PB_RAW &&
			ol->recv_expected_size != 0 &&
			ol->recv_buf_off < ol->recv_expected_size)
		{
			debug("%s: packet = A_MORE\n", __FUNCTION__);
				packet = A_MORE;
		}
		else
		{
			uint32_t more_len;

			uint32_t adj_len = ol->recv_buf_off;
			// take into account expected_size for raw packets
			if (ol->packet == TCP_PB_RAW && ol->recv_expected_size != 0)
				adj_len = ol->recv_expected_size;
				
			bits_t bs;
			bits_init_buf(ol->recv_buffer, adj_len, &bs);
			packet = decode_packet_N(ol->packet, &bs, noval, ol->binary,
						&reason, &more_len, ol->packet_size, 0, &cont_proc->hp);
			debug("%s: ol->packet=%d, packet=0x%x, packet_size=%d, reason=0x%x\n", __FUNCTION__,
			      ol->packet, packet, ol->packet_size, reason);

			if (packet == A_MORE && more_len != 0 && more_len > ol->recv_bufsize)
				return -TOO_LONG;

			if (packet != A_MORE && packet != noval)
			{
				uint32_t left = (bs.ends -bs.starts) /8;
				uint32_t consumed = adj_len -left;
				memmove(ol->recv_buffer, ol->recv_buffer +consumed, ol->recv_buf_off -consumed);
				ol->recv_buf_off -= consumed;
				debug("---> recv_bake_packets: consumed=%d, left=%d, cr_in_progress=%d, active=%d\n",
						consumed, left, ol->cr_in_progress, ol->active);

				// Is it safe to acknowledge the data here, outside of the
				// receive callback?
				RECV_ACKNOWLEDGE(ol->tcp, consumed);

				if (ol->packet == TCP_PB_HTTP || ol->packet == TCP_PB_HTTP_BIN)
					active_tag = A_HTTP;

				if (ol->packet == TCP_PB_HTTP)
					ol->packet = TCP_PB_HTTPH;
				else if (ol->packet == TCP_PB_HTTP_BIN)
					ol->packet = TCP_PB_HTTPH_BIN;
				else if (ol->packet == TCP_PB_HTTPH && packet == A_HTTP_EOH)
					ol->packet = TCP_PB_HTTP;
				else if (ol->packet == TCP_PB_HTTPH_BIN && packet == A_HTTP_EOH)
					ol->packet = TCP_PB_HTTP_BIN;
			}
		}
	}

	if (packet != A_MORE && ol->cr_in_progress)
	{
		debug("%s: cr_cancel_deferred\n", __FUNCTION__);
		cr_cancel_deferred(ol);
		term_t a = (packet == noval) ?A_ERROR :A_OK;
		term_t b = (packet == noval) ?reason :packet;
		inet_async2(ol->oid, ol->cr_reply_to, ASYNC_REF, a, b);
	}
	else if (packet != A_MORE && ol->active != INET_PASSIVE)
	{
		debug("%s: {tcp_error, Oid, Reason}\n", __FUNCTION__);
		uint32_t *p = heap_alloc_N(&cont_proc->hp, 1 +3);
		if (p == 0)
			return -NO_MEMORY;
		p[0] = 3;
		p[1] = (packet == noval) ?A_TCP_ERROR :active_tag;
		p[2] = ol->oid;
		p[3] = (packet == noval) ?reason :packet;
		heap_set_top(&cont_proc->hp, p +1 +3);
		int x = scheduler_new_local_mail_N(cont_proc, tag_tuple(p));
		if (x < 0)
			return x;

		if (ol->active == INET_ONCE && !is_tuple(packet))	// http_eoh
			ol->active = INET_PASSIVE;
		else if (ol->recv_buf_off > 0 && packet != noval)
			goto more_packets;
	}

	debug("%s(): done\n", __FUNCTION__);
	return 0;
}

static int ol_tcp_set_opts(outlet_t *ol, uint8_t *data, int dlen)
{
	uint8_t *p = data;
	int left = dlen;
	int saved_active = ol->active;

	while (left > 0)
	{
		int opt = *p++;
		left--;
		if (left < 4)
			return -BAD_ARG;
		uint32_t val = GET_UINT_32(p);
		p += 4;
		left -= 4;
		debug("%s(ol=*%p, opt=%d, val=%d)\n", __FUNCTION__, ol, opt, val);

		switch (opt)
		{
		case INET_OPT_RCVBUF:
			if (val >= 0x80000000)
				return -BAD_ARG;
			if (val > ol->max_recv_bufsize)
			{
				if (ol_realloc_recvbuf(ol, val))
					continue;
			}
			else
			{
				if (val < TCP_MIN_RECV_BUF)
					val = TCP_MIN_RECV_BUF;
				ol->recv_bufsize = val;
			}
			break;

		case INET_OPT_PRIORITY:
			//
			// There is a priority setting in a PCB. It is always set to high
			// value to avoid closing of open connections when the stack runs
			// out of memory. Thus the option is ignored.
			//
			//printk("tcp_set_opts: unsupported option SO_PRIORITY ignored\n");
			break;

		case INET_OPT_TOS:
#if LING_WITH_LWIP
			if (ol->tcp)
				ol->tcp->tos = (uint8_t)val;
			else
#endif
				printk("tcp_set_opts: INET_OPT_TOS ignored\n");
			break;

		case TCP_OPT_NODELAY:

			//
			// Nagle's algo fights silly window syndrome. What is its
			// relationship to not delaying send?
			//
			if (ol->tcp)
				ol_tcp_set_nodelay(ol, val);
			else
				printk("tcp_set_opts: TCP_OPT_NODELAY ignored\n");
			break;

		default:
			if (inet_set_opt(ol, opt, val) < 0)
				return -BAD_ARG;
		}
	}

	//
	// If 'active' option was set among other options, then act immediately to
	// deliver a buffered packet, if any.
	//
	
	if (saved_active == INET_PASSIVE && ol->active != INET_PASSIVE)
	{
		proc_t *cont_proc = scheduler_lookup(ol->owner);
		assert(cont_proc != 0);
		recv_bake_packets(ol, cont_proc);
	}

	return 0;
}

static int ol_tcp_get_opts(outlet_t *ol,
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
			val = ol->recv_bufsize;
			break;

		case INET_OPT_PRIORITY:
			//
			// See comment in ol_tcp_animate()
			//
#if LING_WITH_LWIP
			val = ol->tcp->prio;
#elif LING_WITH_LIBUV
			val = 0;
#endif
			break;

		case INET_OPT_TOS:
#if LING_WITH_LWIP
			val = ol->tcp->tos;
#elif LING_WITH_LIBUV
			val = 0;
#endif
			break;

		case TCP_OPT_NODELAY:
			val = tcp_get_nodelay(ol);
			break;

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

//EOF
