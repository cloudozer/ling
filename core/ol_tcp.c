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

#include "lwip/def.h"
#include "lwip/tcp.h"
#include "lwip/timers.h"
#undef LWIP_SOCKET
#define LWIP_SOCKET 1
#include "lwip/sockets.h"
#undef LWIP_SOCKET

#include "getput.h"
#include "bits.h"
#include "atom_defs.h"
#include "scheduler.h"
#include "term_util.h"

#include <string.h>

#define ASYNC_REF	0
//
// To achieve a record number of concurrent connections, the TCP_MIN_RECV_BUF
// +TCP_MIN_SEND_BUF should be set to a small value (at most 3200?). This
// ensures that a single page is allocated per TCP/IP outlet.
//
//#define TCP_MIN_SEND_BUF			1024
//#define TCP_MIN_RECV_BUF			2048
#define TCP_MIN_SEND_BUF			1024
#define TCP_MIN_RECV_BUF			4096

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

static void cr_timeout_cb(void *arg);
static void cr_defer_reply(outlet_t *ol, term_t reply_to, uint32_t millis);
static void cr_cancel_deferred(outlet_t *ol);
static void send_timeout_cb(void *arg);
static void send_defer_reply(outlet_t *ol, term_t reply_to);
static void send_cancel_deferred(outlet_t *ol);

static err_t connected_cb(void *arg, struct tcp_pcb *tcp, err_t err);
static err_t recv_cb(void *arg, struct tcp_pcb *tcp, struct pbuf *data, err_t err);
static err_t sent_cb(void *arg, struct tcp_pcb *tcp, uint16_t len);
static void error_cb(void *arg, err_t err);

static int recv_bake_packets(outlet_t *ol, proc_t *cont_proc);
static int ol_tcp_set_opts(outlet_t *ol, uint8_t *data, int dlen);
static int ol_tcp_get_opts(outlet_t *ol,
					uint8_t *data, int dlen, char *buf, int sz);

void ol_tcp_acc_promote(outlet_t *ol, struct tcp_pcb *old_pcb, int backlog);

outlet_t *ol_tcp_factory(proc_t *cont_proc, uint32_t bit_opts)
{
	int extra = TCP_MIN_SEND_BUF +TCP_MIN_RECV_BUF;
	outlet_t *new_ol = outlet_make_N(&ol_tcp_vtab, cont_proc, bit_opts, extra);
	if (new_ol == 0)
		return 0;

	inet_set_default_opts(new_ol);
	//new_ol->tcp = 0;
	
	//new_ol->cr_in_progress = 0;
	new_ol->cr_reply_to = noval;
	//new_ol->cr_timeout_set = 0;

	// Split the unoccupied space in home_node between send_buffer and
	// recv_buffer.
	
	//new_ol->send_in_progress = 0;
	new_ol->max_send_bufsize = TCP_MIN_SEND_BUF;
	new_ol->send_buffer = (uint8_t *)(new_ol +1);
	//new_ol->send_buf_node = 0;
	//new_ol->send_buf_ack = 0;
	//new_ol->send_buf_off = 0;
	new_ol->send_buf_left = 0;
	new_ol->send_reply_to = noval;
	//new_ol->send_timeout_set = 0;
	new_ol->send_timeout = INET_INFINITY;

	//new_ol->recv_expected_size = 0;

	new_ol->recv_buffer = new_ol->send_buffer +new_ol->max_send_bufsize;
	uint32_t init_size = (uint8_t *)new_ol->home_node->ends -new_ol->recv_buffer;
	assert(init_size >= TCP_MIN_RECV_BUF);
	new_ol->recv_bufsize = new_ol->max_recv_bufsize = init_size;
	//new_ol->recv_buf_node = 0;
	//new_ol->recv_buf_off = 0;

	//new_ol->empty_queue_in_progress = 0;
	new_ol->empty_queue_reply_to = noval;
	
	//new_ol->peer_close_detected = 0;

	return new_ol;
}

void ol_tcp_animate(outlet_t *new_ol, struct tcp_pcb *pcb, struct pbuf *ante)
{
	//
	// lwIP tries hard to allocate a new PCB. If there is not enough memory it
	// first kills TIME-WAIT connections and then active connections. The
	// error_cb callback is used along the way. The callback may sent an exit
	// signal to an outlet and the chain of exit signal may reach the current
	// outlet. To avoid this the priority of all PCBs is set to TCP_PRIO_MAX+1.
	//

	tcp_setprio(pcb, TCP_PRIO_MAX +1);

	tcp_arg(pcb, new_ol);	// callback arg
	tcp_recv(pcb, recv_cb);
	tcp_sent(pcb, sent_cb);
	tcp_err(pcb, error_cb);

	new_ol->tcp = pcb;
	if (ante != 0)	// data receive while enqueued
	{
		uint16_t len = ante->tot_len;
		if (len > new_ol->recv_bufsize)
		{
			debug("ol_tcp_animate: tot_len=%d, recv_bufsize=%d, truncating\n",
				  ante->tot_len, new_ol->recv_bufsize);
			len = new_ol->recv_bufsize;	
		}
		pbuf_copy_partial(ante, new_ol->recv_buffer, len, 0);
		new_ol->recv_buf_off = len;
		pbuf_free(ante);
	}
}

static uint8_t *ol_tcp_get_send_buffer(outlet_t *ol, int len)
{
	int buf_len = len;
	switch (ol->packet)
	{
	case TCP_PB_1: buf_len = len +1; break;
	case TCP_PB_2: buf_len = len +2; break;
	case TCP_PB_4: buf_len = len +4; break;
	}

	if (buf_len > ol->max_send_bufsize)
	{
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
	uint16_t write_len = (buf_len > TCP_SND_BUF)
		?TCP_SND_BUF
		:buf_len;
	ol->send_buf_ack = 0;
	ol->send_buf_off = write_len;

	//debug("ol_tcp_send: tcp_write(%d)\n", write_len);
	int rc = tcp_write(ol->tcp, ol->send_buffer, write_len, TCP_WRITE_FLAG_COPY);
	if (rc != ERR_OK)
	{
		//debug("ol_tcp_send: tcp_write() returns error %d\n", rc);
		inet_reply_error(ol->oid, reply_to, lwip_err_to_term(rc));
		return 0;
	}

	// Otherwise, the data are buffered until the next tcp_tmr timeout
	tcp_output(ol->tcp);

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

	assert(ol != 0);
	assert(ol->tcp != 0 || op == INET_REQ_OPEN || op == INET_REQ_SUBSCRIBE || op == INET_REQ_SETOPTS);

	switch (op)
	{
	case INET_REQ_OPEN:
	{
		if (dlen != 2 || data[1] != INET_TYPE_STREAM)
			goto error;
		uint8_t family = data[0];
		if (family != INET_AF_INET && family != INET_AF_INET6)
			goto error;
		assert(ol->tcp == 0);

#if LWIP_IPV6
		ol->tcp = (family == INET_AF_INET6)
			?tcp_new_ip6()
			:tcp_new();
#else
		if (family != INET_AF_INET)
			goto error;
		ol->tcp = tcp_new();
#endif
		assert(ol->tcp != 0);

		// see comment in ol_tcp_animate()
		tcp_setprio(ol->tcp, TCP_PRIO_MAX +1);

		tcp_arg(ol->tcp, ol);	// callback arg
		tcp_recv(ol->tcp, recv_cb);
		tcp_sent(ol->tcp, sent_cb);
		tcp_err(ol->tcp, error_cb);

		*reply++ = INET_REP_OK;
	}
	break;

	case INET_REQ_CONNECT:
	{
		int is_ipv6 = PCB_ISIPV6(ol->tcp);
		if ((is_ipv6 && dlen != 4 +2 +16) || (!is_ipv6 && dlen != 4 +2 +4))
			goto error;

		uint32_t timeout = GET_UINT_32(data);
		uint16_t remote_port = GET_UINT_16(data +4);
	
		err_t err;
		if (!is_ipv6)
		{
			ip_addr_t where_to;
			where_to.addr = ntohl(GET_UINT_32(data +4 +2));
			err = tcp_connect(ol->tcp, &where_to, remote_port, connected_cb);
		}
		else
		{
#if LWIP_IPV6
			ip6_addr_t where_to;
			where_to.addr[0] = ntohl(GET_UINT_32(data +4 +2));
			where_to.addr[1] = ntohl(GET_UINT_32(data +4 +2 +4));
			where_to.addr[2] = ntohl(GET_UINT_32(data +4 +2 +8));
			where_to.addr[3] = ntohl(GET_UINT_32(data +4 +2 +12));
			err = tcp_connect_ip6(ol->tcp, &where_to, remote_port, connected_cb);
#else
			goto error;
#endif
		}

		// Does it make connections faster?
		tcp_output(ol->tcp);

		if (err == ERR_OK)
		{
			cr_defer_reply(ol, reply_to, timeout);

			*reply++ = INET_REP_OK;
			uint16_t ref = ASYNC_REF;	// Why this is needed? A constant will do.
			PUT_UINT_16(reply, ref);
			reply += 2;
		}
		else if (err == ERR_MEM)
			REPLY_INET_ERROR("enomem");
		else
		{
			// ERR_RTE?
			printk("ol_tcp: connect failed: %d\n", err);
			REPLY_INET_ERROR("enotcon");
		}
	}
	break;

	case INET_REQ_PEER:
	if (ol->tcp->state == CLOSED)
		REPLY_INET_ERROR("enotconn");
	else
	{
		*reply++ = INET_REP_OK;
		*reply++ = INET_AF_INET;
		uint16_t peer_port = ol->tcp->remote_port;
		PUT_UINT_16(reply, peer_port);
		reply += 2;
		if (PCB_ISIPV6(ol->tcp))
		{
			ip_addr_set_hton((ip_addr_t *)reply, (ip_addr_t *)&ol->tcp->remote_ip);
			reply += 4;
		}
		else
		{
#if LWIP_IPV6
			ip6_addr_set_hton((ip6_addr_t *)reply, (ip6_addr_t *)&ol->tcp->remote_ip);
			reply += 16;
#else
			goto error;
#endif
		}
	}
	break;

	case INET_REQ_NAME:
	if (ol->tcp->state == CLOSED)
		REPLY_INET_ERROR("enotconn");
	else
	{
		*reply++ = INET_REP_OK;
		int is_ipv6 = PCB_ISIPV6(ol->tcp);
		*reply++ = (is_ipv6) ?INET_AF_INET6 :INET_AF_INET;
		uint16_t name_port = ol->tcp->local_port;
		PUT_UINT_16(reply, name_port);
		reply += 2;
		if (PCB_ISIPV6(ol->tcp))
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
	break;

	case INET_REQ_BIND:
	{
		int is_ipv6 = PCB_ISIPV6(ol->tcp);
		if ((is_ipv6 && dlen != 2 +16) || (!is_ipv6 && dlen != 2 +4))
			goto error;
		uint16_t port = GET_UINT_16(data);
		if (!is_ipv6)
		{
			ip_addr_t addr;
			addr.addr = ntohl(GET_UINT_32(data +2));
			tcp_bind(ol->tcp, &addr, port); // always succeeds
		}
		else
		{
#if LWIP_IPV6
			ip6_addr_t addr;
			addr.addr[0] = ntohl(GET_UINT_32(data +2));
			addr.addr[1] = ntohl(GET_UINT_32(data +2 +4));
			addr.addr[2] = ntohl(GET_UINT_32(data +2 +8));
			addr.addr[3] = ntohl(GET_UINT_32(data +2 +12));
			tcp_bind_ip6(ol->tcp, &addr, port); // always succeeds
#else
			goto error;
#endif
		}

		uint16_t local_port = ol->tcp->local_port;
		*reply++ = INET_REP_OK;
		PUT_UINT_16(reply, local_port);
		reply += 2;
	}
	break;

	case INET_REQ_LISTEN:
	{
		assert(ol->recv_buf_node == 0);	// or use destroy_private()
		int backlog = GET_UINT_16(data);
		ol_tcp_acc_promote(ol, ol->tcp, backlog);
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
	if (dlen != 1 && data[0] != INET_SUBS_EMPTY_OUT_Q)
		goto error;
	if (ol->empty_queue_in_progress)
		goto error;		//TODO: allow multiple subscriptions

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

	if (ol->active != INET_PASSIVE)
		goto error;
	if (ol->packet == TCP_PB_RAW && recv_num > ol->recv_bufsize)
		goto error;
	
	if (ol->peer_close_detected)
		inet_async_error(ol->oid, reply_to, ASYNC_REF, A_CLOSED);
	else
	{
		cr_defer_reply(ol, reply_to, msecs);

		if (ol->packet == TCP_PB_RAW)
			ol->recv_expected_size = recv_num;

		// Enough data may have already been buffered
		proc_t *cont_proc = scheduler_lookup(reply_to);
		assert(cont_proc != 0);
		if (recv_bake_packets(ol, cont_proc) < 0)
			goto error;
	}

	*reply++ = INET_REP_OK;
	uint16_t my_ref = ASYNC_REF;
	PUT_UINT_16(reply, my_ref);
	reply += 2;
	break;

	case TCP_REQ_SHUTDOWN:
	if (dlen != 1)
		goto error;

	uint8_t what = data[0];
	// 0 - read
	// 1 - write
	// 2 - read_write
	
	int shut_rx = (what == 0) || (what == 2);
	int shut_tx = (what == 1) || (what == 2);

	if (ol->tcp->state == LISTEN)
		REPLY_INET_ERROR("enotconn");
	else
	{
		tcp_shutdown(ol->tcp, shut_rx, shut_tx);
		// TODO: return code ignored

		*reply++ = INET_REP_OK;
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

static int ol_tcp_attach(outlet_t *ol)
{
	// The outlet's lwIP PCB gets created upon INET_REQ_OPEN
	return 0;
}

static void ol_tcp_detach(outlet_t *ol)
{
	//
	// Sever all outstanding relationships with processes and subsystems
	//
	// - a lwIP PCB
	// - a process waiting for an empty send queue
	// - a process waits until data is sent
	// - a process waiting of a connection or data (optionally, with timeout)
	//
	
	if (ol->tcp != 0)
	{
		tcp_arg(ol->tcp, 0);	// quench last lwIP callback calls
		tcp_close(ol->tcp);
		ol->tcp = 0;
	}

	//
	// Take care as the oultet may be closed by
	// inet_reply_error/inet_async_error() if the memory is tight.
	//
	
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
	nfree(ol->send_buf_node);
	nfree(ol->recv_buf_node);
}

static void cr_timeout_cb(void *arg)
{
	phase_expected(PHASE_EVENTS);

	outlet_t *ol = (outlet_t *)arg;
	assert(ol != 0);
	assert(ol->cr_timeout_set);
	assert(ol->cr_in_progress);
	//debug("cr_timeout_cb: %pt\n", T(ol->oid));
	ol->cr_timeout_set = 0;
	ol->cr_in_progress = 0;
	inet_async_error(ol->oid, ol->cr_reply_to, ASYNC_REF, A_TIMEOUT);
}

static void send_timeout_cb(void *arg)
{
	phase_expected(PHASE_EVENTS);

	outlet_t *ol = (outlet_t *)arg;
	assert(ol != 0);
	assert(ol->send_timeout_set);
	assert(ol->send_in_progress);
	//debug("send_timeout_cb: %pt\n", T(ol->oid));
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
		sys_timeout_adj(millis, cr_timeout_cb, ol);
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
		sys_timeout_adj(ol->send_timeout, send_timeout_cb, ol);
		ol->send_timeout_set = 1;
	}
}

static void cr_cancel_deferred(outlet_t *ol)
{
	assert(ol->cr_in_progress);
	ol->cr_in_progress = 0;
	if (ol->cr_timeout_set)
	{
		sys_untimeout(cr_timeout_cb, ol);
		ol->cr_timeout_set = 0;
	}
}

static void send_cancel_deferred(outlet_t *ol)
{
	assert(ol->send_in_progress);
	ol->send_in_progress = 0;
	if (ol->send_timeout_set)
	{
		sys_untimeout(send_timeout_cb, ol);
		ol->send_timeout_set = 0;
	}
}

static err_t connected_cb(void *arg, struct tcp_pcb *tcp, err_t err)
{
	phase_expected(PHASE_EVENTS);

	outlet_t *ol = (outlet_t *)arg;
	assert(ol != 0);
	assert(err == ERR_OK);	// argument kept for backward compatibility?

	//debug("connected_cb: %pt\n", T(ol->oid));
	assert(ol->tcp == tcp);
	assert(ol->cr_in_progress);

	cr_cancel_deferred(ol);
	inet_async(ol->oid, ol->cr_reply_to, ASYNC_REF, A_OK);
	return ERR_OK;
}

static err_t recv_cb(void *arg, struct tcp_pcb *tcp, struct pbuf *data, err_t err)
{
	phase_expected(PHASE_EVENTS);

	outlet_t *ol = (outlet_t *)arg;
	if (ol == 0)
		return ERR_OK;		// outlet has gone already
	//debug("---> recv_cb(arg 0x%pp, tcp 0x%pp, %d, err %d)\n",
	//				arg, tcp, (data == 0) ?0: data->tot_len, err);
	assert(ol->tcp == tcp);

	term_t pid = (ol->cr_in_progress) ?ol->cr_reply_to :ol->owner;
	proc_t *cont_proc = scheduler_lookup(pid);
	if (cont_proc == 0)
	{
		//debug("recv_cb: nowhere to send - discard\n");
		if (data != 0)
			pbuf_free(data);
		return ERR_OK;
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
	}
	else
	{
		uint16_t len = data->tot_len;
		if (len > ol->recv_bufsize -ol->recv_buf_off)
		{
			debug("recv_cb: len %d recv_bufsize %d recv_buf_off %d\n",
									len, ol->recv_bufsize, ol->recv_buf_off);
			debug("recv_cb: received data do not fit recv_buffer - truncated\n");
			len = ol->recv_bufsize -ol->recv_buf_off;	// truncation
		}

		//debug("---> recv_cb: recv_bufsize %d recv_buf_off %d\n\t\ttot_len %d len %d\n", 
		//		ol->recv_bufsize, ol->recv_buf_off, data->tot_len, len);
		pbuf_copy_partial(data, ol->recv_buffer +ol->recv_buf_off, len, 0);
		ol->recv_buf_off += len;

		// A more natural place to acknowledge the data when complete packets
		// are baked.
		//tcp_recved(ol->tcp, len);

		pbuf_free(data);
		int x = recv_bake_packets(ol, cont_proc);
		if (x < 0)
			scheduler_signal_exit_N(cont_proc, ol->oid, err_to_term(x));
	}

	return ERR_OK;
}

static err_t sent_cb(void *arg, struct tcp_pcb *tcp, uint16_t len)
{
	phase_expected(PHASE_EVENTS);

	outlet_t *ol = (outlet_t *)arg;
	if (ol == 0)
		return ERR_OK;		// outlet has gone already
	assert(ol->tcp == tcp);

	//debug("sent_cb: len %d\n", len);
	
	// inet_reply() may close the outlet
	term_t saved_oid = ol->oid;
	term_t send_reply_to = noval;
	term_t send_error = noval;
	term_t empty_queue_reply_to = noval;

	assert(ol->send_in_progress);
	assert(ol->send_buf_left >= len);
	ol->send_buf_left -= len;
	ol->send_buf_ack += len;
	assert(ol->send_buf_ack <= ol->send_buf_off);

	if (ol->send_buf_ack == ol->send_buf_off)
	{
		if (ol->send_buf_left > 0)
		{
			// write more
			uint16_t write_len = (ol->send_buf_left > TCP_SND_BUF)
				?TCP_SND_BUF
				:ol->send_buf_left;

			ol->send_buf_off += write_len;

			//debug("ol_tcp_send: tcp_write(%d)\n", write_len);
			int rc = tcp_write(ol->tcp, ol->send_buffer +ol->send_buf_ack, write_len, TCP_WRITE_FLAG_COPY);
			if (rc != ERR_OK)
			{
				send_cancel_deferred(ol);
				send_reply_to = ol->send_reply_to;
				send_error = lwip_err_to_term(rc);
			}

			// Kick the TCP/IP stack
			tcp_output(ol->tcp);
		}
		else
		{
			send_cancel_deferred(ol);
			send_reply_to = ol->send_reply_to;
		}
	}

	if (ol->empty_queue_in_progress && tcp_sndqueuelen(tcp) == 0)
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

	return ERR_OK;
}

static void error_cb(void *arg, err_t err)
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
    //
    term_t reason = lwip_err_to_term(err);
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

more_packets:
	if (ol->cr_in_progress || ol->active != INET_PASSIVE)
	{
		if (ol->packet == TCP_PB_RAW &&
			ol->recv_expected_size != 0 &&
			ol->recv_buf_off < ol->recv_expected_size)
				packet = A_MORE;
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

			if (packet == A_MORE && more_len != 0 && more_len > ol->recv_bufsize)
				return -TOO_LONG;

			if (packet != A_MORE && packet != noval)
			{
				uint32_t left = (bs.ends -bs.starts) /8;
				uint32_t consumed = adj_len -left;
				memmove(ol->recv_buffer, ol->recv_buffer +consumed, ol->recv_buf_off -consumed);
				ol->recv_buf_off -= consumed;
				//debug("---> recv_bake_packets: consumed %d left %d cr_in_progress %d active %d\n",
				//		consumed, left, ol->cr_in_progress, ol->active);

				// Is it safe to acknowledge the data here, outside of the
				// receive callback?
				tcp_recved(ol->tcp, consumed);

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
		cr_cancel_deferred(ol);
		term_t a = (packet == noval) ?A_ERROR :A_OK;
		term_t b = (packet == noval) ?reason :packet;
		inet_async2(ol->oid, ol->cr_reply_to, ASYNC_REF, a, b);
	}
	else if (packet != A_MORE && ol->active != INET_PASSIVE)
	{
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

		switch (opt)
		{
		case INET_OPT_RCVBUF:
			if (val >= 0x80000000)
				return -BAD_ARG;
			if (val > ol->max_recv_bufsize)
			{
				memnode_t *node = nalloc_N(val);
				if (node == 0)
				{
					// We may return -NO_MEMORY here; a more conservative
					// approach is to ignore the option and continue
					printk("ol_tcp_set_opts: cannot expand recv_buffer to %d byte(s)\n", val);
					continue;
				}
				ol->recv_bufsize = val;
				assert(ol->recv_buf_off <= ol->recv_bufsize); // no truncation
				memcpy(node->starts, ol->recv_buffer, ol->recv_buf_off);
				ol->max_recv_bufsize = (void *)node->ends -(void *)node->starts;
				ol->recv_buffer = (uint8_t *)node->starts;
				nfree(ol->recv_buf_node);
				ol->recv_buf_node = node;
				// ol->recv_buf_off stays the same
			}
			else
				ol->recv_bufsize = val;
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
			if (ol->tcp)
				ol->tcp->tos = (uint8_t)val;
			else
				printk("tcp_set_opts: INET_OPT_TOS ignored\n");
			break;

		case TCP_OPT_NODELAY:

			//
			// Nagle's algo fights silly window syndrome. What is its
			// relationship to not delaying send?
			//
			if (ol->tcp)
				if (val)
					tcp_nagle_disable(ol->tcp);
				else
					tcp_nagle_enable(ol->tcp);
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

		switch (opt)
		{
		case INET_OPT_RCVBUF:
			val = ol->recv_bufsize;
			break;

		case INET_OPT_PRIORITY:
			//
			// See comment in ol_tcp_animate()
			//
			val = ol->tcp->prio;
			break;

		case INET_OPT_TOS:
			val = ol->tcp->tos;
			break;

		case TCP_OPT_NODELAY:
			val = tcp_nagle_disabled(ol->tcp);
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
