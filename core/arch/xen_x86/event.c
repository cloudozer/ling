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

/*
**
**
**
*/

#include "event.h"

#include "ling_common.h"
#include "ling_xen.h"

#define NR_EVENTS	((sizeof(unsigned long) * 8) * 8)

struct event_handler {
	event_entry_t entry;
	void *data;
};

static struct event_handler event_handlers[NR_EVENTS];
static uint32_t bound_events[NR_EVENTS];
static int nr_bound;

void events_poll(uint64_t timeout)
{
	if (nr_bound == 0 && timeout == 0)
		return;

	struct sched_poll op;
	op.nr_ports = nr_bound;
	op.timeout = timeout;
	set_xen_guest_handle(op.ports, bound_events);
	int rs = HYPERVISOR_sched_op(SCHEDOP_poll, &op);
	if (rs)
		fatal_error("events_poll failed: %d\n", rs);
}

int events_do_pending(void)
{
	int nr_fired = 0;
	for (int i = 0; i < nr_bound; i++)
	{
		uint32_t port = bound_events[i];
		if (test_bit(port, &shared_info.evtchn_pending[0]))
		{
			event_clear(port);
			struct event_handler *eh = &event_handlers[port];
			assert(eh->entry != 0);
			eh->entry(port, eh->data);

			nr_fired++;
		}
	}
	return nr_fired;
}

static void dummy_handler(evtchn_port_t port, void *data)
{
	printk("*** Dummy event handler [%d]\n", port);
}

void events_init(void)
{
	int i;
	for (i = 0; i < NR_EVENTS; i++)
	{
		event_handlers[i].entry = dummy_handler;
		event_handlers[i].data = 0;
		event_mask(i);
	}
	nr_bound = 0;
}

void event_bind(uint32_t port, event_entry_t entry, void *data)
{
	if (event_handlers[port].entry != dummy_handler)
	{
		printk("[WARN] port %d already has a handler, was %pp replacing...\n",
			port, event_handlers[port].entry);
	}

	event_handlers[port].data = data;
	event_handlers[port].entry = entry;
	
	bound_events[nr_bound++] = port;
}

uint32_t event_alloc_unbound(domid_t remote_domid)
{
	evtchn_alloc_unbound_t op;
	op.dom = DOMID_SELF;
	op.remote_dom = remote_domid;
	int rs = HYPERVISOR_event_channel_op(EVTCHNOP_alloc_unbound, &op);
	if (rs)
		fatal_error("events_alloc_unbound failed: %d\n", rs);
	return op.port;
}

uint32_t event_bind_virq(uint32_t virq, event_entry_t entry, void *data)
{
	evtchn_bind_virq_t op;
	op.virq = virq;
	op.vcpu = 0;
	int rs = HYPERVISOR_event_channel_op(EVTCHNOP_bind_virq, &op);
	if (rs)
		fatal_error("events_bind_virq failed: %d\n", rs);
	event_bind(op.port, entry, data);
	return op.port;
}

void event_kick(uint32_t port)
{
	evtchn_send_t op;
	op.port = port;
	int rs = HYPERVISOR_event_channel_op(EVTCHNOP_send, &op);
	if (rs < 0)
		fatal_error("event_kick: %d", rs);
}

/*EOF*/

