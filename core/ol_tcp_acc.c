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

#include "outlet.h"
#include "ol_inet.h"

#ifdef LING_WITH_LWIP
# include "lwip/tcp.h"
# include "lwip/timers.h"
#endif //LING_WITH_LWIP

#ifdef LING_WITH_LIBUV
# include <uv.h>
#endif //LING_WITH_LIBUV

#include "outlet.h"
#include "ol_inet.h"
#include "getput.h"
#include "scheduler.h"
#include "atom_defs.h"

#include <string.h>

#define ASYNC_REF	0

static term_t ol_tcp_acc_control(
    outlet_t *ol, uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp
);
static void ol_tcp_acc_destroy_private(outlet_t *ol);

static outlet_vtab_t ol_tcp_acc_vtab = {
	.control = ol_tcp_acc_control,
	.destroy_private = ol_tcp_acc_destroy_private,
};

outlet_t *ol_tcp_factory(proc_t *cont_proc, uint32_t bit_opts);

static int tcpacc_on_recv(acc_pend_t *pend, const void *packet);
static void tcp_on_accept_timout(acc_pend_t *);

static int try_to_bake(outlet_t *ol, acc_pend_t *pend);
static void bake_one_accepted(outlet_t *ol, proc_t *cont_proc);
static void bake_all_accepted(outlet_t *ol);

static int ol_tcp_acc_set_opts(outlet_t *ol, uint8_t *data, int dlen);
static int ol_tcp_acc_get_opts(outlet_t *ol,
					uint8_t *data, int dlen, char *buf, int sz);

static acc_pend_t *get_free_pending(outlet_t *ol);
static void append_to_ring(acc_pend_t **ring, acc_pend_t *elem);
static acc_pend_t *pop_from_ring(acc_pend_t **ring);
static void reuse_pending(acc_pend_t **free_pends, acc_pend_t *elem);

#if LING_WITH_LWIP
static err_t lwip_accept_cb(void *arg, struct tcp_pcb *newpcb, err_t err);
static void accept_timeout_cb(void *arg);
static err_t accept_recv_cb(void *arg, struct tcp_pcb *tcp, struct pbuf *data, err_t err);
static void accept_error_cb(void *arg, err_t err);

/*
 *    LWIP callbacks
 */

static err_t lwip_accept_cb(void *arg, struct tcp_pcb *newpcb, err_t err)
{
	phase_expected(PHASE_EVENTS);

	debug("<--- %s(arg 0x%pp, newpcb 0x%pp, err %d)\n",
	      __FUNCTION__, arg, newpcb, err);
	outlet_t *ol = (outlet_t *)arg;
	assert(ol != 0);
	assert(err == ERR_OK);

	acc_pend_t *pend = get_free_pending(ol);
	if (pend == 0)
	{
		tcp_abort(newpcb);
		return ERR_ABRT;
	}

	pend->pcb = newpcb;
	pend->ante = 0;

	// The newpcb is being enqueued into
	// Data may be received for the newpcb while
	// it is still on the queue. Such data is handled by a *temporary* recevive
	// callback accept_recv_cb.

	tcp_arg(newpcb, pend);
	tcp_recv(newpcb, accept_recv_cb);
	tcp_err(newpcb, accept_error_cb);

	return try_to_bake(ol, pend);
}

static void accept_timeout_cb(void *arg)
{
	phase_expected(PHASE_EVENTS);

	acc_pend_t *pend = (acc_pend_t *)arg;
	assert(pend != 0);

	tcp_on_accept_timout(pend);
}

// handles received data until a process "animates" the pending request:
static err_t accept_recv_cb(void *arg, struct tcp_pcb *tcp, struct pbuf *data, err_t err)
{
	acc_pend_t *pend = (acc_pend_t *)arg;
	assert(pend != 0);
	assert(pend->pcb == tcp);

	return tcpacc_on_recv(pend, data);
}

static void accept_error_cb(void *arg, err_t err)
{
	fatal_error("accept_error_cb called: err %d\n", err);
}


static void save_until_animated(acc_pend_t *pend, struct pbuf *data)
{
	tcp_recved(pend->pcb, data->tot_len);

	if (pend->ante == 0)
		pend->ante = data;
	else
		pbuf_cat(pend->ante, data);
}

/*
 *    LWIP-specific actions
 */

static void tcp_start_listen(outlet_t *ol)
{
	struct tcp_pcb *smaller_pcb = tcp_listen(ol->tcp);
	if (smaller_pcb)
		ol->tcp = smaller_pcb;
	tcp_arg(ol->tcp, ol);
	tcp_accept(ol->tcp, lwip_accept_cb);
}

static void tcp_close_pending(struct acc_pend_t *pend)
{
	tcp_arg(pend->pcb, 0);
	tcp_close(pend->pcb);
	if (pend->ante != 0)
		pbuf_free(pend->ante);
}

/*
 *    Timers and timeouts
 */
static void tcp_accept_timeout(void *ctx, uint32_t millis)
{
	sys_timeout_adj(millis, accept_timeout_cb, ctx);
}

static void tcp_accept_untimeout(void *ctx)
{
	sys_untimeout(accept_timeout_cb, ctx);
}

#endif //LING_WITH_LWIP

#if LING_WITH_LIBUV

/*
 *    libuv callbacks
 */

static void uv_on_tcpacc_recv(uv_stream_t *conn, ssize_t nread, const uv_buf_t *buf)
{
	debug("%s(nread=%d)\n", __FUNCTION__, nread);

	acc_pend_t *pend = conn->data;
	tcpacc_on_recv(pend, buf);
}

static void uv_on_tcp_accept_timeout(uv_timer_t *timer)
{
	debug("%s()\n", __FUNCTION__);

	tcp_on_accept_timout(timer->data);
}

static void uv_on_tcp_accept(uv_stream_t *tcpacc, int status)
{
	debug("%s(status=%d)\n", __FUNCTION__, status);
	int ret;

	outlet_t *ol = tcpacc->data;
	assert(ol);

	if (status < 0)
		return;

	acc_pend_t *pend = get_free_pending(ol);
	if (pend == 0)
		return;  // fatal error?

	uv_tcp_t *conn = malloc(sizeof(uv_tcp_t));
	if (!conn)
		goto pend_cleanup;

	ret = uv_tcp_init(uv_default_loop(), conn);
	if (ret)
		goto pend_cleanup;

	ret = uv_accept(tcpacc, (uv_stream_t *)conn);
	if (ret)
		goto pend_cleanup;

	ret = uv_read_start((uv_stream_t *)conn, on_alloc, uv_on_tcpacc_recv);
	if (ret)
		goto pend_cleanup;

	pend->tcp = conn;
	try_to_bake(ol, pend);
	return;

pend_cleanup:
	if (conn)
		uv_close((uv_handle_t *)conn, NULL);
	reuse_pending(&ol->free_pends, pend);
	return;
}

/*
 *    libuv-specific actions
 */
static void tcp_start_listen(outlet_t *ol)
{
	debug("%s(*%p, backlog=%d)\n", __FUNCTION__, ol, ol->backlog);
	assert(ol->tcp);
	int ret;

	ol->tcp->data = ol;
	ret = uv_listen((uv_stream_t *)ol->tcp, ol->backlog /* DEFAULT_BACKLOG? */, uv_on_tcp_accept);
	if (ret)
	{
		debug("%s: uv_listen() failed(%s)\n", __FUNCTION__, uv_strerror(ret));
		uv_close((uv_handle_t *)ol->tcp, NULL);
		free(ol->tcp);
	}
}

static void tcp_close_pending(struct acc_pend_t *pend)
{
	uv_close((uv_handle_t *)pend->tcp, NULL);
	free(pend->tcp);
	pend->tcp = NULL;

	if (pend->ante.base)
		free(pend->ante.base);
}

static void save_until_animated(acc_pend_t *pend, const uv_buf_t *data)
{
	if (pend->ante.base) {
		void *newbase = malloc(pend->ante.len + data->len);
		if (!newbase) {
			debug("%s(data.len=%d): realloc failed, data dropped\n", __FUNCTION__, data->len);
			return;
		}

		memcpy(newbase + pend->ante.len, data->base, data->len);
		memcpy(newbase, pend->ante.base, pend->ante.len);
		free(pend->ante.base);

		pend->ante.base = newbase;
		pend->ante.len += data->len;
	} else {
		pend->ante.base = data->base;
		pend->ante.len = data->len;
	}
}

/*
 *    libuv timers and timeouts
 */
static void tcp_accept_timeout(struct acc_pend_t *pend, uint32_t millis)
{
	int ret;

	ret = uv_timer_init(uv_default_loop(), &pend->accept_timer);
	if (ret)
		return; /* TODO*/

	pend->accept_timer.data = pend;

	uv_timer_start(&pend->accept_timer, uv_on_tcp_accept_timeout, (uint64_t)millis, 0);
}

static void tcp_accept_untimeout(struct acc_pend_t *pend)
{
	uv_timer_stop(&pend->accept_timer);
}
#endif


// turn an ordinary TCP outlet to a listening mode
void ol_tcp_acc_promote(outlet_t *ol, int backlog)
{
	//XXX: see similar assert() in ol_tcp.c
	//assert(ol->recv_buf_node == 0);
	ol->vtab = &ol_tcp_acc_vtab;	// switch outlet behaviour

	ol->backlog = backlog;
	ol->free_pends = 0;
	acc_pend_t *pend = (acc_pend_t *)(ol +1);
	void *thresh = (void *)ol->home_node->ends;
	while (pend +1 <= (acc_pend_t *)thresh)
	{
		pend->next = ol->free_pends;
		ol->free_pends = pend;
		pend++;
	}
	ol->pend_nodes = 0;
	ol->accepting = 0;
	ol->accepted = 0;

	tcp_start_listen(ol);
}

#define REPLY_INET_ERROR(err)	do { \
	*reply++ = INET_REP_ERROR; \
	strcpy(reply, err); \
	reply += strlen(err); \
} while (0)

static term_t ol_tcp_acc_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp)
{
	char rbuf[256];
	char *reply = rbuf;

	assert(ol != 0);
	assert(ol->tcp != 0);
	debug("%s(op=%d)\n", __FUNCTION__, op);

	switch (op)
	{
	case INET_REQ_ACCEPT:
	{
		if (dlen != 4)
			goto error;

		uint32_t millis = GET_UINT_32(data);
		if (millis == 0)
		{
			// immediate reply requested
			if (ol->accepting != 0 && ol->accepted != 0)
			{
				struct	proc_t *cont_proc = scheduler_lookup(reply_to);
				assert(cont_proc != 0);

				bake_one_accepted(ol, cont_proc);
			}
			else
				inet_async_error(ol->oid, reply_to, ASYNC_REF, A_TIMEOUT);

			break;
		}

		acc_pend_t *pend = get_free_pending(ol);
		if (pend == 0)
		{
			REPLY_INET_ERROR("emfile");
			break;
		}
		pend->outlet = ol;
		pend->reply_to = reply_to;
		pend->timeout_set = 0;
		if (millis != INET_INFINITY)
		{
			tcp_accept_timeout(pend, millis);
			pend->timeout_set = 1;
		}

		append_to_ring(&ol->accepting, pend);
		bake_all_accepted(ol);

		*reply++ = INET_REP_OK;
		uint16_t ref = ASYNC_REF;
		PUT_UINT_16(reply, ref);
		reply += 2;
	}
	break;

	case INET_REQ_NAME:
	{
#if LING_WITH_LWIP
		//TODO: exactly the same as in ol_tcp_control() - refactor?
		*reply++ = INET_REP_OK;
		int is_ipv6 = is_ipv6_outlet(ol);
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
#else
		REPLY_INET_ERROR("enotimpl");
#endif
	}
	break;

	case INET_REQ_SETOPTS:
	if (ol_tcp_acc_set_opts(ol, data, dlen) < 0)
		goto error;

	*reply++ = INET_REP_OK;
	break;

	case INET_REQ_GETOPTS:
	{
		int sz = ol_tcp_acc_get_opts(ol, data, dlen, rbuf+1, sizeof(rbuf) -1);
		if (sz < 0)
			goto error;

		*reply++ = INET_REP_OK;
		reply += sz;
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

static void ol_tcp_acc_destroy_private(outlet_t *ol)
{
	ol_tcp_close(ol);

	// Take ownership of bits of the outlet that require a cleanup. This is
	// needed as the outlet may be destroyed within inet_async_error() call.
	//
	acc_pend_t *accepting = ol->accepting;
	ol->accepting = 0;
	acc_pend_t *accepted = ol->accepted;
	ol->accepted = 0;
	memnode_t *pend_nodes = ol->pend_nodes;
	ol->pend_nodes = 0;
	term_t saved_oid = ol->oid;

	// break the ring
	if (accepting != 0)
		accepting->prev->next = 0;
	acc_pend_t *pend = accepting;
	while (pend != 0)
	{
		if (pend->timeout_set)
			tcp_accept_untimeout(pend);

		inet_async_error(saved_oid, pend->reply_to, ASYNC_REF, A_CLOSED);
		pend = pend->next;
	}

	// break the ring
	if (accepted != 0)
		accepted->prev->next = 0;
	pend = ol->accepted;
	while (pend != 0)
	{
		tcp_close_pending(pend);
		pend = pend->next;
	}

	nfree_chain(pend_nodes);
}

static int tcpacc_on_recv(acc_pend_t *pend, const void *packet)
{
	RECV_PKT_T *data = (RECV_PKT_T *)packet;
	phase_expected(PHASE_EVENTS);

	debug("%s(*0x%p, len=%d)\n", __FUNCTION__,
	      pend, (data == 0) ?0: RECV_PKT_LEN(data));
	if (data == 0)
	{
		outlet_t *ol = pend->outlet;

		// The connection is closed while still waiting on the accepted queue
		tcp_close_pending(pend);

		pend->next->prev = pend->prev;
		pend->prev->next = pend->next;

		if (ol->accepted == pend)
			ol->accepted = (pend->next != pend)
				?pend->next
				:0;

		reuse_pending(&ol->free_pends, pend);
	}
	else
		save_until_animated(pend, data);

	return 0;
}

static void tcp_on_accept_timout(acc_pend_t *pend)
{
	// no accept requests from network to meet `pend` of a process
	outlet_t *ol = pend->outlet;
	term_t reply_to = pend->reply_to;
	assert(pend->timeout_set);

	assert(ol->accepting != 0);

	pend->next->prev = pend->prev;
	pend->prev->next = pend->next;

	if (ol->accepting == pend)
		ol->accepting = (pend->next != pend)
			?pend->next
			:0;

	reuse_pending(&ol->free_pends, pend);
	inet_async_error(ol->oid, reply_to, ASYNC_REF, A_TIMEOUT);
}

/* This function animates an accept request from `ol->accepted` queue */
static void bake_one_accepted(outlet_t *ol, proc_t *cont_proc)
{
#if LING_WITH_LWIP
	tcp_accepted(ol->tcp);	// not needed, really
#endif

	assert(ol->accepted != 0);
	acc_pend_t *pend = pop_from_ring(&ol->accepted);

	outlet_t *new_ol = ol_tcp_factory(cont_proc, 0);	// default bit options?
	if (new_ol == 0)
		scheduler_signal_exit_N(cont_proc, ol->oid, A_NO_MEMORY);
	else
	{
		ol_tcp_animate(new_ol, pend);

		// BEAM copies some options in the driver code and some - in gen_tcp.
		// This creates an interplay that may result in the first packet getting
		// lost of interpreted as a wrong packet type. The following assignments
		// are determined by trial and error to let the right first packet
		// through.

		new_ol->active = INET_PASSIVE;
		new_ol->packet = ol->packet;
		new_ol->binary = ol->binary;

		inet_async2(ol->oid, cont_proc->pid, ASYNC_REF, A_OK, new_ol->oid);
	}

	reuse_pending(&ol->free_pends, pend);
}

static int try_to_bake(outlet_t *ol, acc_pend_t *pend)
{
	pend->outlet = ol;

	append_to_ring(&ol->accepted, pend);

	// try to find an accept request from a process
	bake_all_accepted(ol);
	return 0;
}

// This function "zips" all ready items from:
// `ol->accepting` queue (requests from the VM)
// and `ol->accepted` queue (requests from network)
static void bake_all_accepted(outlet_t *ol)
{
	// bake_one_accepted() may close the outlet, so save oid:
	term_t saved_oid = ol->oid;

	// exhaust either ol->accepting or ol->accepted :
	while (ol->accepting != 0 && ol->accepted != 0)
	{
		acc_pend_t *pend = pop_from_ring(&ol->accepting);

		if (pend->timeout_set)
			tcp_accept_untimeout(pend);

		term_t reply_to = pend->reply_to;

		reuse_pending(&ol->free_pends, pend);

		proc_t *cont_proc = scheduler_lookup(reply_to);
		if (cont_proc == 0)
			continue;

		// this will pop a `pend` from ol->accepted
		bake_one_accepted(ol, cont_proc);

		if (outlet_lookup(saved_oid) == 0)
			break;
	}
}

static int ol_tcp_acc_set_opts(outlet_t *ol, uint8_t *data, int dlen)
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

		switch (opt)
		{
		case INET_OPT_PRIORITY:
			//
			// There is a priority setting in a PCB. It is always set to high
			// value to avoid closing of open connections when the stack runs
			// out of memory. Thus the option is ignored.
			//
			//printk("tcp_set_opts: unsupported option SO_PRIORITY ignored\n");
			break;

#if LING_WITH_LWIP
		case INET_OPT_TOS:
			ol->tcp->tos = (uint8_t)val;
			break;
#endif

		case TCP_OPT_NODELAY:
			//
			// Nagle's algo fights silly window syndrome. What is its
			// relationship to not delaying send?
			//
			ol_tcp_set_nodelay(ol, val);
			break;

		default:
			if (inet_set_opt(ol, opt, val) < 0)
				return -BAD_ARG;
		}
	}

	return 0;
}

static int ol_tcp_acc_get_opts(outlet_t *ol,
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

// Both processes that called accept() on a socket and connections that were
// accepted by lwIP but not consumed yet but application code are kept on rings
// of acc_pend_t structures. The following helper functions implement operations
// on such rings.

static acc_pend_t *get_free_pending(outlet_t *ol)
{
	if (ol->free_pends == 0)
	{
		memnode_t *node = nalloc_N(QUICK_SIZE -sizeof(memnode_t));
		if (node == 0)
			return 0;
		acc_pend_t *pend = (acc_pend_t *)node->starts;
		while (pend +1 <= (acc_pend_t *)node->ends)
		{
			pend->next = ol->free_pends;
			ol->free_pends = pend;
			pend++;
		}
		node->next = ol->pend_nodes;
		ol->pend_nodes = node;
		assert(ol->free_pends != 0);
	}

	acc_pend_t *pend = ol->free_pends;
	ol->free_pends = pend->next;

	memset(pend, 0, sizeof(acc_pend_t));
	//pend->prev = pend->next = 0;
	return pend;
}

static void append_to_ring(acc_pend_t **ring, acc_pend_t *elem)
{
	assert(elem->prev == 0);
	assert(elem->next == 0);

	if (*ring == 0)
	{
		elem->next = elem;
		elem->prev = elem;
		*ring = elem;
	}
	else
	{
		acc_pend_t *tail = (*ring)->prev;
		elem->next = *ring;
		elem->prev = tail;
		tail->next = elem;
		(*ring)->prev = elem;
	}
}

static acc_pend_t *pop_from_ring(acc_pend_t **ring)
{
	acc_pend_t *pend = *ring;
	assert(pend != 0);

	pend->next->prev = pend->prev;
	pend->prev->next = pend->next;

	*ring = (pend->next != pend)
		?pend->next
		:0;

	pend->prev = pend->next = 0;
	return pend;
}

static void reuse_pending(acc_pend_t **free_pends, acc_pend_t *elem)
{
	elem->next = *free_pends;
	*free_pends = elem;
}

//EOF
