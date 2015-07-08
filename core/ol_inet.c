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

#include "ol_inet.h"
#include "scheduler.h"
#include "atom_defs.h"
#include "term_util.h"

#ifdef LING_WITH_LWIP
#include "lwip/ip.h"
#undef LWIP_SOCKET
#define LWIP_SOCKET 1
#include "lwip/sockets.h"
#undef LWIP_SOCKET
#endif

void inet_set_default_opts(outlet_t *ol)
{
	ol->active = INET_ACTIVE;

	ol->deliver = INET_DELIVER_TERM;

	ol->buffer = 0;
	ol->header = 0;
	ol->packet = TCP_PB_RAW;
	ol->packet_size = 0;

	ol->exit_on_close = 1;
}

#define SET_SO_OPT(opts, name, val)  do { \
	if (val) \
		*(opts) |= (name); \
	else \
		*(opts) &= ~(name); \
} while (0)

int inet_set_opt(outlet_t *ol, int opt, uint32_t val)
{
#ifdef LING_WITH_LWIP
	assert(ol->ip != 0);
	uint8_t *opts = &ol->ip->so_options;

	switch (opt)
	{
	case INET_OPT_REUSEADDR:
		SET_SO_OPT(opts, SO_REUSEADDR, val);
		break;

	case INET_OPT_KEEPALIVE:
		SET_SO_OPT(opts, SO_KEEPALIVE, val);
		break;

	case INET_OPT_DONTROUTE:
		SET_SO_OPT(opts, SO_DONTROUTE, val);
		break;

	case INET_OPT_LINGER:
		SET_SO_OPT(opts, SO_LINGER, val);
		break;

	case INET_OPT_BROADCAST:
		SET_SO_OPT(opts, SO_BROADCAST, val);
		break;

	case INET_OPT_OOBINLINE:
		SET_SO_OPT(opts, SO_OOBINLINE, val);
		break;

	case INET_OPT_SNDBUF:
	case INET_OPT_RCVBUF:
	case INET_OPT_PRIORITY:
	case INET_OPT_TOS:
	case TCP_OPT_NODELAY:
	case UDP_OPT_MULTICAST_IF:
	case UDP_OPT_MULTICAST_TTL:
	case UDP_OPT_MULTICAST_LOOP:
	case UDP_OPT_ADD_MEMBERSHIP:
	case UDP_OPT_DROP_MEMBERSHIP:
		goto unsupported;

	case INET_LOPT_BUFFER:
		ol->buffer = val;
		break;

	case INET_LOPT_HEADER:
		ol->header = val;
		break;

	case INET_LOPT_ACTIVE:
		ol->active = val;
		break;

	case INET_LOPT_PACKET:
		ol->packet = val;
		break;

	case INET_LOPT_MODE:
		ol->binary = (val == INET_MODE_BINARY);
		break;

	case INET_LOPT_DELIVER:
		ol->deliver = val;
		break;

	case INET_LOPT_EXITONCLOSE:
		ol->exit_on_close = val;
		break;

	case INET_LOPT_TCP_SEND_TIMEOUT:
		ol->send_timeout = val;
		break;

	case INET_LOPT_TCP_SEND_TIMEOUT_CLOSE:
		// ignore silently
		break;

	case INET_LOPT_TCP_HIWTRMRK:
	case INET_LOPT_TCP_LOWTRMRK:
	case INET_LOPT_BIT8:
		goto unsupported;

	case INET_LOPT_TCP_DELAY_SEND:
		// ignore silently
		break;

	case INET_LOPT_PACKET_SIZE:
		ol->packet_size = val;
		break;

	case INET_LOPT_READ_PACKETS:
	case INET_OPT_RAW:
		goto unsupported;

	default:
unsupported:
		printk("inet_set_opt: unsupported option [%d] set\n", opt);
		return -BAD_ARG;
	}

	return 0;
#else
	return -1;
#endif
}

int inet_get_opt(outlet_t *ol, int opt, uint32_t *val)
{
#ifdef LING_WITH_LWIP
	assert(ol->ip != 0);
	uint8_t opts = ol->ip->so_options;

	switch (opt)
	{
	case INET_OPT_REUSEADDR:
		*val = (opts & SO_REUSEADDR) != 0;
		break;

	case INET_OPT_KEEPALIVE:
		*val = (opts & SO_KEEPALIVE) != 0;
		break;

	case INET_OPT_DONTROUTE:
		*val = (opts & SO_DONTROUTE) != 0;
		break;

	case INET_OPT_LINGER:
		*val = (opts & SO_LINGER) != 0;
		break;

	case INET_OPT_BROADCAST:
		*val = (opts & SO_BROADCAST) != 0;
		break;

	case INET_OPT_OOBINLINE:
		*val = (opts & SO_OOBINLINE) != 0;
		break;

	case INET_OPT_SNDBUF:
	case INET_OPT_RCVBUF:
	case INET_OPT_PRIORITY:
	case INET_OPT_TOS:
	case TCP_OPT_NODELAY:
	case UDP_OPT_MULTICAST_IF:
	case UDP_OPT_MULTICAST_TTL:
	case UDP_OPT_MULTICAST_LOOP:
	case UDP_OPT_ADD_MEMBERSHIP:
	case UDP_OPT_DROP_MEMBERSHIP:
		goto unsupported;

	case INET_LOPT_BUFFER:
		*val = ol->buffer;
		break;

	case INET_LOPT_HEADER:
		*val = ol->header;
		break;

	case INET_LOPT_ACTIVE:
		*val = ol->active;
		break;

	case INET_LOPT_PACKET:
		*val = ol->packet;
		break;

	case INET_LOPT_MODE:
		*val = (ol->binary) ?INET_MODE_BINARY :INET_MODE_LIST;
		break;

	case INET_LOPT_DELIVER:
		*val = ol->deliver;
		break;

	case INET_LOPT_EXITONCLOSE:
		*val = ol->exit_on_close;
		break;

	case INET_LOPT_TCP_HIWTRMRK:
	case INET_LOPT_TCP_LOWTRMRK:
	case INET_LOPT_BIT8:
	case INET_LOPT_TCP_SEND_TIMEOUT:
		goto unsupported;

	case INET_LOPT_TCP_DELAY_SEND:
		*val = 0;
		break;

	case INET_LOPT_PACKET_SIZE:
		*val = ol->packet_size;
		break;

	case INET_LOPT_READ_PACKETS:
	case INET_OPT_RAW:
	case INET_LOPT_TCP_SEND_TIMEOUT_CLOSE:
		goto unsupported;

	default:
		assert(opt == INET_LOPT_TCP_SEND_TIMEOUT_CLOSE);
unsupported:
		printk("inet_get_opt: unsupported option [%d] requested\n", opt);
		return -BAD_ARG;
	}

	return 0;
#else
	return -1;
#endif
}

void inet_async(term_t oid, term_t reply_to, uint16_t ref, term_t reply)
{
	proc_t *caller = scheduler_lookup(reply_to);
	if (caller == 0)
		return;

	// {inet_async,S,Ref,Reply}
	uint32_t *p = heap_alloc_N(&caller->hp, 1 +4);
	if (p == 0)
		goto nomem;
	heap_set_top(&caller->hp, p +1 +4);
	p[0] = 4;
	p[1] = A_INET_ASYNC;
	p[2] = oid;
	p[3] = tag_int(ref);
	p[4] = reply;
	term_t msg = tag_tuple(p);

	int x = scheduler_new_local_mail_N(caller, msg);
	if (x < 0)
		scheduler_signal_exit_N(caller, oid, err_to_term(x));

	return;
nomem:
	scheduler_signal_exit_N(caller, oid, A_NO_MEMORY);
}

void inet_async2(term_t oid, term_t reply_to, uint16_t ref, term_t a, term_t b)
{
	proc_t *caller = scheduler_lookup(reply_to);
	if (caller == 0)
		return;

	// {inet_async,S,Ref,{A,B}}
	uint32_t *p = heap_alloc_N(&caller->hp, 1 +2 +1 +4);
	if (p == 0)
		goto nomem;
	heap_set_top(&caller->hp, p +1 +2 +1 +4);
	p[0] = 2;
	p[1] = a;
	p[2] = b;
	term_t reply = tag_tuple(p);
	p += 3;
	p[0] = 4;
	p[1] = A_INET_ASYNC;
	p[2] = oid;
	p[3] = tag_int(ref);
	p[4] = reply;
	term_t msg = tag_tuple(p);

	int x = scheduler_new_local_mail_N(caller, msg);
	if (x < 0)
		scheduler_signal_exit_N(caller, oid, err_to_term(x));

	return;
nomem:
	scheduler_signal_exit_N(caller, oid, A_NO_MEMORY);
}

void inet_async_error(term_t oid, term_t reply_to, uint16_t ref, term_t err)
{
	inet_async2(oid, reply_to, ref, A_ERROR, err);
}

void inet_reply(term_t oid, term_t reply_to, term_t reply)
{
	proc_t *caller = scheduler_lookup(reply_to);
	if (caller == 0)
		return;

    // {inet_reply,S,Reply}
    assert(is_immed(reply));
    uint32_t *p = heap_alloc_N(&caller->hp, 1 +3);
	if (p == 0)
		goto nomem;
	heap_set_top(&caller->hp, p +1 +3);
	p[0] = 3;
	p[1] = A_INET_REPLY;
	p[2] = oid;
	p[3] = reply;
	term_t msg = tag_tuple(p);
    int x = scheduler_new_local_mail_N(caller, msg);
	if (x < 0)
		scheduler_signal_exit_N(caller, oid, err_to_term(x));

	return;
nomem:
	scheduler_signal_exit_N(caller, oid, A_NO_MEMORY);
}

void inet_reply_error(term_t oid, term_t reply_to, term_t reason)
{
	proc_t *caller = scheduler_lookup(reply_to);
	if (caller == 0)
		return;

    // {inet_reply,S,{error,Reason}}
	assert(is_immed(reason));
	uint32_t *p = heap_alloc_N(&caller->hp, 1 +2 +1 +3);
	if (p == 0)
		goto nomem;
	heap_set_top(&caller->hp, p +1 +2 +1 +3);
	p[0] = 2;
	p[1] = A_ERROR;
	p[2] = reason;
	term_t res = tag_tuple(p);
	p += 3;
	p[0] = 3;
	p[1] = A_INET_REPLY;
	p[2] = oid;
	p[3] = res;
	term_t msg = tag_tuple(p);
    int x = scheduler_new_local_mail_N(caller, msg);
	if (x < 0)
		scheduler_signal_exit_N(caller, oid, err_to_term(x));

	return;
nomem:
	scheduler_signal_exit_N(caller, oid, A_NO_MEMORY);
}

#ifdef LING_WITH_LWIP
term_t lwip_err_to_term(err_t err)
{
	switch ((int)err)
	{
	case ERR_OK:			return A_OK;
	case ERR_MEM:			return A_LWIP_MEM;
	case ERR_BUF:			return A_LWIP_BUF;
	case ERR_TIMEOUT:		return A_LWIP_TIMEOUT;
	case ERR_RTE:			return A_LWIP_RTE;
	case ERR_INPROGRESS:	return A_LWIP_INPROGRESS;
	case ERR_VAL:			return A_LWIP_VAL;
	case ERR_WOULDBLOCK:	return A_LWIP_WOULDBLOCK;
	case ERR_USE:			return A_LWIP_USE;
	case ERR_ISCONN:		return A_LWIP_ISCONN;
	case ERR_ABRT:			return A_LWIP_ABRT;
	case ERR_RST:			return A_LWIP_RST;
	case ERR_CLSD:			return A_LWIP_CLSD;
	case ERR_CONN:			return A_LWIP_CONN;
	case ERR_ARG:			return A_LWIP_ARG;
	default:
		assert(err == ERR_IF);
		return A_LWIP_IF;
	}
}
#endif

//EOF
