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

#include "outlet.h"

#include <string.h>

#include "ling_common.h"

#include "atom_defs.h"
#include "scheduler.h"
#include "proc.h"
#include "console.h"
#include "netif.h"
#include "disk.h"
#include "bits.h"

static uint32_t next_outlet_id = 0;

static outlet_t *active_outlets = 0;

typedef struct drv_spec_t drv_spec_t;
struct drv_spec_t {
	term_t name;	// '9p', 'vif, etc
	outlet_factory_func_t factory;
};

outlet_t *ol_echo_factory(proc_t *cont_proc, uint32_t bit_opts);
outlet_t *ol_xstore_factory(proc_t *cont_proc, uint32_t bit_opts);
outlet_t *ol_vif_factory(proc_t *cont_proc, uint32_t bit_opts);
outlet_t *ol_tube_factory(proc_t *cont_proc, uint32_t bit_opts);
outlet_t *ol_dcons_factory(proc_t *cont_proc, uint32_t bit_opts);
outlet_t *ol_console_factory(proc_t *cont_proc, uint32_t bit_opts);
outlet_t *ol_dns_factory(proc_t *cont_proc, uint32_t bit_opts);
outlet_t *ol_udp_factory(proc_t *cont_proc, uint32_t bit_opts);
outlet_t *ol_tcp_factory(proc_t *cont_proc, uint32_t bit_opts);
outlet_t *ol_disk_factory(proc_t *cont_proc, uint32_t bit_opts);

#define NUM_DRIVERS	10

drv_spec_t outlet_drivers[NUM_DRIVERS] = {
	{ .name = A_ECHO,			.factory = ol_echo_factory },
	{ .name = A_XENSTORE,		.factory = ol_xstore_factory },
	{ .name = A_VIF,			.factory = ol_vif_factory },
	{ .name = A_TUBE,			.factory = ol_tube_factory },
	{ .name = A_DUMB_CONSOLE,	.factory = ol_dcons_factory },
	{ .name = A_CONSOLE,		.factory = ol_console_factory },
	{ .name = A_DNS,			.factory = ol_dns_factory },
	{ .name = A_UDP,			.factory = ol_udp_factory },
	{ .name = A_TCP,			.factory = ol_tcp_factory },
	{ .name = A_DISK,			.factory = ol_disk_factory },
};

outlet_factory_func_t outlet_resolve_driver(term_t name)
{
	drv_spec_t *ds = outlet_drivers;
	while (ds < outlet_drivers +NUM_DRIVERS && ds->name != name)
		ds++;
	if (ds < outlet_drivers +NUM_DRIVERS)
		return ds->factory;
	return 0;
}

outlet_t *outlet_make_N(outlet_vtab_t *vtab, proc_t *cont_proc, int32_t bit_opts, uint32_t extra)
{
	memnode_t *home_node = nalloc_N(sizeof(outlet_t) +extra);
	if (home_node == 0)
		return 0;
	outlet_t *new_ol = (outlet_t *)home_node->starts;
	
	memset(new_ol, 0, sizeof(*new_ol));
	new_ol->home_node = home_node;
	
	new_ol->oid = tag_short_oid(next_outlet_id++);
	new_ol->name = noval;
	new_ol->vtab = vtab;
	new_ol->owner = cont_proc->pid;

	inter_links_init(&new_ol->links);

	// Establish port<->owner links
	int x = inter_link_establish_N(&new_ol->links, cont_proc->pid);
	if (x == 0)
		x = inter_link_establish_N(&cont_proc->links, new_ol->oid);
	if (x < 0)
	{
		// no need to unlink, new_ol may have a link to cont_proc but it is
		// destroyed anyway
		inter_links_done(&new_ol->links);
		nfree(home_node);
		return 0;
	}

	new_ol->data = A_UNDEFINED;

	new_ol->inout = PB_VALUE(bit_opts, PB_INOUT_OFF, PB_INOUT_SIZE);
	new_ol->binary = PB_VALUE(bit_opts, PB_BINARY_OFF, PB_BINARY_SIZE);

	// 0 - raw
	// 1 - 1 byte size
	// 2 - 2 byte size
	// 3 - 4 byte size
	new_ol->packet = PB_VALUE(bit_opts, PB_PACKET_OFF, PB_PACKET_SIZE);

	new_ol->line = PB_VALUE(bit_opts, PB_LINE_OFF, PB_LINE_SIZE);
	new_ol->eof = PB_VALUE(bit_opts, PB_EOF_OFF, PB_EOF_SIZE);

	new_ol->notify_on_close = 0;

	// Use the remainder of the home_node as a send buffer
	new_ol->send_buffer = (uint8_t *)(new_ol +1);
	new_ol->max_send_bufsize = (uint8_t *)new_ol->home_node->ends -new_ol->send_buffer;
	//new_ol->send_buf_left = 0;

	if (active_outlets != 0)
		active_outlets->ref = &new_ol->next;
	new_ol->ref = &active_outlets;
	new_ol->next = active_outlets;
	active_outlets = new_ol;

	return new_ol;
}

uint8_t *outlet_get_send_buffer(outlet_t *ol, int len)
{
	assert(ol->vtab->get_send_buffer);
	return ol->vtab->get_send_buffer(ol, len);
}

int outlet_send(outlet_t *ol, int len, term_t reply_to)
{
	assert(ol->vtab->send != 0);
	return ol->vtab->send(ol, len, reply_to);
}

void outlet_new_data(outlet_t *ol, uint8_t *data, int dlen)
{
	assert(ol->vtab->new_data != 0);
	ol->vtab->new_data(ol, data, dlen);
}

term_t outlet_control(outlet_t *ol, uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp)
{
	assert(ol->vtab->control != 0);
	return ol->vtab->control(ol, op, data, dlen, reply_to, hp);
}

int outlet_attach(outlet_t *ol)
{
	if (ol->vtab->attach == 0)
		return 0;
	return ol->vtab->attach(ol);
}

void outlet_detach(outlet_t *ol)
{
	if (ol->vtab->detach)
		ol->vtab->detach(ol);
}

void outlet_destroy_private(outlet_t *ol)
{
	if (ol->vtab->destroy_private != 0)
		ol->vtab->destroy_private(ol);
}

outlet_t *outlet_lookup(term_t oid)
{
	assert(is_short_oid(oid));
	outlet_t *ol = active_outlets;
	while (ol != 0)
	{
		if (ol->oid == oid)
			return ol;
		ol = ol->next;
	}
	return 0;
}

outlet_t *outlet_lookup_by_name(term_t name)
{
	assert(is_atom(name));
	outlet_t *ol = active_outlets;
	while (ol != 0)
	{
		if (ol->name == name)
			return ol;
		ol = ol->next;
	}
	return 0;
}

term_t outlet_all(heap_t *hp)
{
	term_t all = nil;
	outlet_t *ol = active_outlets;
	while (ol != 0)
	{
		all = heap_cons(hp, ol->oid, all);
		ol = ol->next;
	}
	return all;
}

// similar to scheduler_exit_signal()
int outlet_signal_exit_N(outlet_t *ol, term_t src, term_t reason)
{
	//debug("outlet_signal_exit_N: oid %pt src %pt reason %pt\n", T(ol->oid), T(src), T(reason));
	outlet_close(ol, reason);
	return 0;
}

// similar to sceduler_exit_process()
void outlet_close(outlet_t *ol, term_t reason)
{
	//debug("outlet_close: oid %pt reason %pt\n", T(ol->oid), T(reason));
	term_t saved_oid = ol->oid;
	phase_expected2(PHASE_NEXT, PHASE_EVENTS);

	//if (reason != A_NORMAL)
	//{
	//	if (ol->name == noval)
	//		printk("*** %pt closes on signal: %pt\n",
	//				T(ol->oid), T(reason));
	//	else
	//		printk("*** %pt (%pt) closes on signal: %pt\n",
	//				T(ol->oid), T(ol->name), T(reason));
	//}

	outlet_detach(ol);

	// The outlet may go away while detaching. The possible sequence of events
	// starts with a 'no memory' condition when passing messages to processes.
	// The resulting cascade of exit signals is likely to reach the outlet being
	// closed. The detach() method will then run twice (or multiple times). It
	// must be idempotent. The code that follows is certainly not.
	//
	if (outlet_lookup(saved_oid) == 0)
	{
		debug("outlet_close: outlet %pt is closed while being detached\n", T(saved_oid));
		return;
	}

	inter_links_notify(&ol->links, ol->oid, reason);

	if (ol->name != noval)
		outlet_unregister(ol);

	*ol->ref = ol->next;
	if (ol->next)
		ol->next->ref = ol->ref;

	if (ol->notify_on_close)
		if (outlet_notify_owner(ol, A_CLOSED) < 0)
			printk("outlet_close: close message not delivered to %pt\n", T(ol->owner));

	outlet_destroy(ol);
}

void outlet_register(outlet_t *ol, term_t name)
{
	assert(ol->name == noval);
	ol->name = name;
}

void outlet_unregister(outlet_t *ol)
{
	assert(ol->name != noval);
	ol->name = noval;
}

void outlet_pass_new_data(outlet_t *ol, uint8_t *data, int dlen)
{
	proc_t *proc = scheduler_lookup(ol->owner);
	if (proc == 0)
		return;	// drop

	// The max_mq_len can be changed by vif outlets only. It stops adding
	// messages to the owner's mailbox if it is overflowing already.
	//
	if (ol->max_mq_len != 0 && msg_queue_len(&proc->mailbox) >= ol->max_mq_len)
		return;	// drop

	int needed = 3 +3;	// two 2-tuples: {Port,{data,Data}}
	uint32_t *htop;
	term_t td = nil;

	if (ol->binary)
	{
		if (dlen <= MAX_HEAP_BIN)
		{
			needed += WSIZE(t_heap_bin_t) + (dlen +3) /4;
			htop = heap_alloc_N(&proc->hp, needed);
			if (htop != 0)
			{
				td = tag_boxed(htop);
				box_heap_bin(htop, dlen, data);
			}
		}
		else
		{
			binnode_t *node = binnode_make_N(dlen);
			if (node == 0)
				goto no_memory;
			memcpy(node->starts, data, dlen);
			needed += WSIZE(t_proc_bin_t);
			htop = heap_alloc_N(&proc->hp, needed);
			if (htop != 0)
			{
				td = tag_boxed(htop);
				t_proc_bin_t *pb = (t_proc_bin_t *)htop;
				box_proc_bin(htop, dlen, node);
				proc_bin_link(&proc->hp.proc_bins, pb, &proc->hp.total_pb_size);
			}
			else
				binnode_destroy(node);
		}
	}
	else
	{
		needed += 2*dlen;
		htop = heap_alloc_N(&proc->hp, needed);
		if (htop != 0)
		{
			td = nil;
			for (int i = dlen -1; i >= 0; i--)
			{
				htop[0] = tag_int(data[i]);
				htop[1] = td;
				td = tag_cons(htop);
				htop += 2;
			}
		}
	}

	if (htop == 0)
		goto no_memory;

	// {Port,{data,Data}}
	term_t inner = tag_tuple(htop);
	*htop++ = 2;
	*htop++ = A_DATA;
	*htop++ = td;
	term_t msg = tag_tuple(htop);
	*htop++ = 2;
	*htop++ = ol->oid;
	*htop++ = inner;
	heap_set_top(&proc->hp, htop);

	if (scheduler_new_local_mail_N(proc, msg) < 0)
		goto no_memory;
	
	return;

no_memory:
	scheduler_signal_exit_N(proc, ol->oid, A_NO_MEMORY);
}

void outlet_destroy(outlet_t *ol)
{
	outlet_destroy_private(ol);
	inter_links_done(&ol->links);
	nfree(ol->home_node);
}

int outlet_notify_owner(outlet_t *ol, term_t what)
{
	proc_t *proc = scheduler_lookup(ol->owner);
	if (proc == 0)
		return 0;

	uint32_t *p = heap_alloc_N(&proc->hp, 3);
	if (p == 0)
		return -NO_MEMORY;
	p[0] = 2;
	p[1] = ol->oid;
	p[2] = what;
	heap_set_top(&proc->hp, p +3);

	return scheduler_new_local_mail_N(proc, tag_tuple(p));
}

//EOF
