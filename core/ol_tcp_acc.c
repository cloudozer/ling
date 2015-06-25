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

#include "lwip/tcp.h"
#include "lwip/timers.h"

#include "outlet.h"
#include "ol_inet.h"
#include "getput.h"
#include "scheduler.h"
#include "atom_defs.h"

#include <string.h>

#define ASYNC_REF	0

static term_t ol_tcp_acc_control(outlet_t *ol, uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp);
static void ol_tcp_acc_destroy_private(outlet_t *ol);

static outlet_vtab_t ol_tcp_acc_vtab = {
	.control = ol_tcp_acc_control,
	.destroy_private = ol_tcp_acc_destroy_private,
};

outlet_t *ol_tcp_factory(proc_t *cont_proc, uint32_t bit_opts);
void ol_tcp_animate(outlet_t *ol, struct tcp_pcb *pcb, struct pbuf *ante);

static err_t accept_cb(void *arg, struct tcp_pcb *newpcb, err_t err);
static void accept_timeout_cb(void *arg);
static err_t accept_recv_cb(void *arg, struct tcp_pcb *tcp, struct pbuf *data, err_t err);
static void accept_error_cb(void *arg, err_t err);

static void do_bake_accepted(outlet_t *ol, proc_t *cont_proc);
static void bake_accepted(outlet_t *ol);

static int ol_tcp_acc_set_opts(outlet_t *ol, uint8_t *data, int dlen);
static int ol_tcp_acc_get_opts(outlet_t *ol,
					uint8_t *data, int dlen, char *buf, int sz);

static acc_pend_t *get_free_pending(outlet_t *ol);
static void append_to_ring(acc_pend_t **ring, acc_pend_t *elem);
static acc_pend_t *pop_from_ring(acc_pend_t **ring);
static void reuse_pending(acc_pend_t **free_pends, acc_pend_t *elem);

// turn an ordinary TCP outlet to a listening mode
void ol_tcp_acc_promote(outlet_t *ol, struct tcp_pcb *old_pcb, int backlog)
{
	//XXX: see similar assert() in ol_tcp.c
	//assert(ol->recv_buf_node == 0);
	ol->vtab = &ol_tcp_acc_vtab;	// switch outlet behaviour

	struct tcp_pcb *smaller_pcb = tcp_listen(old_pcb);
	ol->tcp = (smaller_pcb != 0)
		?smaller_pcb
		:old_pcb;
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

	tcp_arg(ol->tcp, ol);
	tcp_accept(ol->tcp, accept_cb);
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

				do_bake_accepted(ol, cont_proc);
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
			sys_timeout_adj(millis, accept_timeout_cb, pend);
			pend->timeout_set = 1;
		}

		append_to_ring(&ol->accepting, pend);
		bake_accepted(ol);

		*reply++ = INET_REP_OK;
		uint16_t ref = ASYNC_REF;
		PUT_UINT_16(reply, ref);
		reply += 2;
	}
	break;

	case INET_REQ_NAME:
	{
		//TODO: exactly the same as in ol_tcp_control() - refactor?
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
	// from lwIP documentation:
	//
	// Closes the connection. The function may return ERR_MEM if no memory was
	// available for closing the connection. If so, the application should wait
	// and try again either by using the acknowledgment callback or the polling
	// functionality. If the close succeeds, the function returns ERR_OK.
	//
	if (ol->tcp != 0)
	{
		tcp_arg(ol->tcp, 0);
		tcp_close(ol->tcp);
		ol->tcp = 0;
	}

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
			sys_untimeout(accept_timeout_cb, pend);

		inet_async_error(saved_oid, pend->reply_to, ASYNC_REF, A_CLOSED);
		pend = pend->next;
	}

	// break the ring
	if (accepted != 0)
		accepted->prev->next = 0;
	pend = ol->accepted;
	while (pend != 0)
	{
		tcp_arg(pend->pcb, 0);
		tcp_close(pend->pcb);
		if (pend->ante != 0)
			pbuf_free(pend->ante);
		pend = pend->next;
	}

	nfree_chain(pend_nodes);
}

static err_t accept_cb(void *arg, struct tcp_pcb *newpcb, err_t err)
{
	phase_expected(PHASE_EVENTS);

	//debug("<--- accept_cb(arg 0x%pp, newpcb 0x%pp, err %d)\n",
	//				arg, newpcb, err);
	outlet_t *ol = (outlet_t *)arg;
	assert(ol != 0);
	assert(err == ERR_OK);

	//TODO: enforce backlog length

	acc_pend_t *pend = get_free_pending(ol);
	if (pend == 0)
	{
		tcp_abort(newpcb);
		return ERR_ABRT;
	}

	pend->outlet = ol;
	pend->pcb = newpcb;
	pend->ante = 0;

	// The newpcb is being enqueued. Data may be received for the newpcb while
	// it is still on the queue. Such data is handled by a *temporary* recevive
	// callback accept_recv_cb.
	//
	tcp_arg(newpcb, pend);
	tcp_recv(newpcb, accept_recv_cb);
	tcp_err(newpcb, accept_error_cb);

	append_to_ring(&ol->accepted, pend);
	bake_accepted(ol);
	return ERR_OK;
}

static void accept_timeout_cb(void *arg)
{
	phase_expected(PHASE_EVENTS);

	acc_pend_t *pend = (acc_pend_t *)arg;
	assert(pend != 0);
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

static err_t accept_recv_cb(void *arg, struct tcp_pcb *tcp, struct pbuf *data, err_t err)
{
	phase_expected(PHASE_EVENTS);

	//debug("---> accept_recv_cb(arg 0x%pp, tcp 0x%pp, %d, err %d)\n",
	//				arg, tcp, (data == 0) ?0: data->tot_len, err);
	acc_pend_t *pend = (acc_pend_t *)arg;
	assert(pend != 0);
	assert(pend->pcb == tcp);

	if (data == 0)
	{
		outlet_t *ol = pend->outlet;

		// The connection is closed while still waiting on the accepted queue
		tcp_close(tcp);

		pend->next->prev = pend->prev;
		pend->prev->next = pend->next;

		if (ol->accepted == pend)
			ol->accepted = (pend->next != pend)
				?pend->next
				:0;

		reuse_pending(&ol->free_pends, pend);
	}
	else
	{
		tcp_recved(tcp, data->tot_len);

		if (pend->ante == 0)
			pend->ante = data;
		else
			pbuf_cat(pend->ante, data);
	}

	return ERR_OK;
}

static void accept_error_cb(void *arg, err_t err)
{
	fatal_error("accept_error_cb called: err %d\n", err);
}

static void do_bake_accepted(outlet_t *ol, proc_t *cont_proc)
{
	tcp_accepted(ol->tcp);	// not needed, really

	assert(ol->accepted != 0);
	acc_pend_t *pend = pop_from_ring(&ol->accepted);

	struct tcp_pcb *pcb = pend->pcb;
	struct pbuf *ante = pend->ante;

	reuse_pending(&ol->free_pends, pend);

	outlet_t *new_ol = ol_tcp_factory(cont_proc, 0);	// default bit options?
	if (new_ol == 0)
		scheduler_signal_exit_N(cont_proc, ol->oid, A_NO_MEMORY);
	else
	{
		ol_tcp_animate(new_ol, pcb, ante);

		// BEAM copies some options in the driver code and some - in gen_tcp.
		// This creates an interplay that may result in the first packet getting
		// lost of interpreted as a wrong packet type. The following assignments
		// are determined by trial and error to let the right first packet
		// through.
		//
		new_ol->active = INET_PASSIVE;
		new_ol->packet = ol->packet;
		new_ol->binary = ol->binary;

		inet_async2(ol->oid, cont_proc->pid, ASYNC_REF, A_OK, new_ol->oid);
	}
}

static void bake_accepted(outlet_t *ol)
{
	term_t saved_oid = ol->oid;
	while (ol->accepting != 0 && ol->accepted != 0)
	{
		acc_pend_t *pend = pop_from_ring(&ol->accepting);
		
		if (pend->timeout_set)
			sys_untimeout(accept_timeout_cb, pend);

		term_t reply_to = pend->reply_to;

		reuse_pending(&ol->free_pends, pend);

		proc_t *cont_proc = scheduler_lookup(reply_to);
		if (cont_proc == 0)
			continue;
		do_bake_accepted(ol, cont_proc);

		// do_bake_accepted() may close the outlet
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

		case INET_OPT_TOS:
			ol->tcp->tos = (uint8_t)val;
			break;

		case TCP_OPT_NODELAY:

			//
			// Nagle's algo fights silly window syndrome. What is its
			// relationship to not delaying send?
			//
			if (val)
				tcp_nagle_disable(ol->tcp);
			else
				tcp_nagle_enable(ol->tcp);
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

		switch (opt)
		{
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

//
// Both processes that called accept() on a socket and connections that were
// accepted by lwIP but not consumed yet but application code are kept on rings
// of acc_pend_t structures. The following helper functions implement operations
// on such rings.
//

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

	pend->prev = pend->next = 0;
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
