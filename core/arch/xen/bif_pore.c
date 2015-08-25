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

#include "bif_impl.h"

#define MAX_PORE_DATA	2048

term_t cbif_pore_xs_open0(proc_t *proc, term_t *regs)
{
	pore_xs_t *xp = (pore_xs_t *)pore_make_N(A_XENSTORE,
			sizeof(pore_xs_t), proc->pid, 0, start_info.store_evtchn);
	if (xp == 0)
		fail(A_NO_MEMORY);
	xp->intf = mfn_to_virt(start_info.store_mfn);
	return xp->parent.eid;
}

term_t cbif_pore_xs_write2(proc_t *proc, term_t *regs)
{
	term_t Pore = regs[0];
	term_t Data = regs[1];
	if (!is_short_eid(Pore))
		badarg(Pore);
	if (!is_list(Data) && !is_boxed_binary(Data))
		badarg(Data);
	pore_t *pr = pore_lookup(Pore);
	if (pr == 0 || pr->tag != A_XENSTORE)
		badarg(Pore);

	int64_t size = iolist_size(Data);
	if (size < 0)
		badarg(Data);
	uint8_t buf[size];
	iolist_flatten(Data, buf);

	pore_xs_t *xp = (pore_xs_t *)pr;
	struct xenstore_domain_interface *intf = xp->intf;
	uint32_t cons = intf->req_cons;
	uint32_t prod = intf->req_prod;
	assert(prod +size -cons <= XENSTORE_RING_SIZE); 
	mb();
	uint8_t *pd = buf;
	for (uint32_t i = prod; i < prod +size; i++)
		intf->req[MASK_XENSTORE_IDX(i)] = *pd++;
	wmb();
	intf->req_prod += size;

	return A_OK;
}

term_t cbif_pore_xs_read1(proc_t *proc, term_t *regs)
{
	term_t Pore = regs[0];
	if (!is_short_eid(Pore))
		badarg(Pore);
	pore_t *pr = pore_lookup(Pore);
	if (pr == 0 || pr->tag != A_XENSTORE)
		badarg(Pore);

	pore_xs_t *xp = (pore_xs_t *)pr;
	struct xenstore_domain_interface *intf = xp->intf;
	uint32_t cons = intf->rsp_cons;
	uint32_t prod = intf->rsp_prod;
	uint32_t avail = prod - cons;
	assert(avail > 0);
	rmb();
	uint8_t *ptr;
	term_t bin = heap_make_bin(&proc->hp, avail, &ptr);
	for (uint32_t i = cons; i < prod; i++)
		*ptr++ = intf->rsp[MASK_XENSTORE_IDX(i)];
	mb();
	intf->rsp_cons += avail;

	return bin;
}

term_t cbif_pore_xs_avail1(proc_t *proc, term_t *regs)
{
	term_t Pore = regs[0];
	if (!is_short_eid(Pore))
		badarg(Pore);
	pore_t *pr = pore_lookup(Pore);
	if (pr == 0 || pr->tag != A_XENSTORE)
		badarg(Pore);

	pore_xs_t *xp = (pore_xs_t *)pr;
	struct xenstore_domain_interface *intf = xp->intf;
	int qa = XENSTORE_RING_SIZE -intf->req_prod +intf->req_cons;
	int ra = intf->rsp_prod -intf->rsp_cons;
	
	return heap_tuple2(&proc->hp, tag_int(qa), tag_int(ra));
}

static void straw_destroy(pore_t *pore)
{
	assert(pore->tag == A_STRAW);
	pore_straw_t *ps = (pore_straw_t *)pore;
	if (ps->active)
	{
		for (int i = 0; i < NUM_STRAW_REFS; i++)
			grants_end_access(ps->ring_refs[i]);

		printk("End access for %d refs\n", NUM_STRAW_REFS);
	}
	else
	{
		struct gnttab_unmap_grant_ref unmap[NUM_STRAW_REFS];
		for (int i = 0; i < NUM_STRAW_REFS; i++)
		{
			unmap[i].host_addr = ps->page_map[i].host_addr;
			unmap[i].handle = ps->page_map[i].handle;
			unmap[i].dev_bus_addr = 0;
		}

		int rs = HYPERVISOR_grant_table_op(GNTTABOP_unmap_grant_ref, unmap, NUM_STRAW_REFS);
		assert(rs == 0);

		for (int i = 0; i < NUM_STRAW_REFS; i++)
		{
			assert(unmap[i].status == GNTST_okay);
			rmb();
		}

		printk("%d refs unmapped\n", NUM_STRAW_REFS);
	}
}

term_t cbif_pore_straw_open1(proc_t *proc, term_t *regs)
{
	term_t Domid = regs[0];
	if (!is_int(Domid))
		badarg(Domid);
	int peer = int_value(Domid);

	uint32_t evtchn = event_alloc_unbound(peer);
	assert(sizeof(straw_ring_t) == NUM_STRAW_REFS*PAGE_SIZE);
	int size = (NUM_STRAW_REFS+1)*PAGE_SIZE -sizeof(memnode_t);
	pore_straw_t *ps = (pore_straw_t *)pore_make_N(A_STRAW, size, proc->pid, straw_destroy, evtchn);
	if (ps == 0)
		fail(A_NO_MEMORY);

	straw_ring_t *ring = (straw_ring_t *)((uint8_t *)ps -sizeof(memnode_t) +PAGE_SIZE);
	assert(((uintptr_t)ring & (PAGE_SIZE-1)) == 0); // page-aligned
	ps->shared = ring;
	ps->active = 1;
	// all other fields are zero

	for (int i = 0; i < NUM_STRAW_REFS; i++)
	{
		void *page = (void *)ps->shared + PAGE_SIZE*i;
		grants_allow_access(&ps->ring_refs[i], peer, virt_to_mfn(page));
	}

	return ps->parent.eid;	
}

term_t cbif_pore_straw_open3(proc_t *proc, term_t *regs)
{
	term_t Domid = regs[0];
	term_t Refs = regs[1];
	term_t Channel = regs[2];
	if (!is_int(Domid))
		badarg(Domid);
	int peer_domid = int_value(Domid);
	if (!is_int(Channel))
		badarg(Channel);
	int peer_port = int_value(Channel);
	uint32_t refs[NUM_STRAW_REFS];
	term_t l = Refs;
	for (int i = 0; i < NUM_STRAW_REFS; i++)
	{
		if (!is_cons(l))
			badarg(Refs);
		term_t *cons = peel_cons(l);
		if (!is_int(cons[0]))
			badarg(Refs);
		refs[i] = int_value(cons[0]);
		l = cons[1];
	}
	if (l != nil)
		badarg(Refs);

	uint32_t evtchn = event_bind_interdomain(peer_domid, peer_port);

	assert(sizeof(straw_ring_t) == NUM_STRAW_REFS*PAGE_SIZE);
	int size = (NUM_STRAW_REFS+1)*PAGE_SIZE -sizeof(memnode_t);
	pore_straw_t *ps = (pore_straw_t *)pore_make_N(A_STRAW, size, proc->pid, straw_destroy, evtchn);
	if (ps == 0)
		fail(A_NO_MEMORY);

	straw_ring_t *ring = (straw_ring_t *)((uint8_t *)ps -sizeof(memnode_t) +PAGE_SIZE);
	assert(((uintptr_t)ring & (PAGE_SIZE-1)) == 0); // page-aligned
	ps->shared = ring;
	// all other fields are zero

	for (int i = 0; i < NUM_STRAW_REFS; i++)
	{
		struct gnttab_map_grant_ref *m = &ps->page_map[i];
		m->ref = refs[i];
		m->dom = peer_domid;
		m->flags = GNTMAP_host_map;
		m->host_addr = (uintptr_t)ps->shared + PAGE_SIZE*i;
	}

	int rs = HYPERVISOR_grant_table_op(GNTTABOP_map_grant_ref, ps->page_map, NUM_STRAW_REFS);
	assert(rs == 0);

	for (int i = 0; i < NUM_STRAW_REFS; i++)
	{
		assert(ps->page_map[i].status == GNTST_okay);
		rmb();
	}

	return ps->parent.eid;
}

term_t cbif_pore_straw_write2(proc_t *proc, term_t *regs)
{
	term_t Pore = regs[0];
	term_t Data = regs[1];
	if (!is_short_eid(Pore))
		badarg(Pore);
	if (!is_list(Data) && !is_boxed_binary(Data))
		badarg(Data);
	pore_t *pr = pore_lookup(Pore);
	if (pr == 0 || pr->tag != A_STRAW)
		badarg(Pore);

	int64_t size = iolist_size(Data);
	if (size < 0)
		badarg(Data);
	uint8_t buf[size];
	iolist_flatten(Data, buf);

	pore_straw_t *ps = (pore_straw_t *)pr;
	straw_ring_t *ring = ps->shared;
	int prod = (ps->active) ?ring->out_prod :ring->in_prod;
	int cons = (ps->active) ?ring->out_cons :ring->in_cons;
	mb();
	uint8_t *ptr = buf;
	uint8_t *buffer = (ps->active) ?ring->output :ring->input;
	while (size-- > 0)
	{
		buffer[prod++] = *ptr++;
		if (prod == STRAW_RING_SIZE)
			prod = 0;
		assert(prod != cons);	// too long - avoid crash?
	}
	wmb();
	if (ps->active)
		ring->out_prod = prod;
	else
		ring->in_prod = prod;

	return A_OK;
}

term_t cbif_pore_straw_read1(proc_t *proc, term_t *regs)
{
	term_t Pore = regs[0];
	if (!is_short_eid(Pore))
		badarg(Pore);
	pore_t *pr = pore_lookup(Pore);
	if (pr == 0 || pr->tag != A_STRAW)
		badarg(Pore);

	pore_straw_t *ps = (pore_straw_t *)pr;
	straw_ring_t *ring = ps->shared;
	int prod = (ps->active) ?ring->in_prod :ring->out_prod;
	int cons = (ps->active) ?ring->in_cons :ring->out_cons;
	int avail = prod - cons;
	while (avail < 0)
		avail += STRAW_RING_SIZE;
	assert(avail > 0);
	rmb();
	uint8_t *ptr;
	uint8_t *buffer = (ps->active) ?ring->input :ring->output;
	term_t bin = heap_make_bin(&proc->hp, avail, &ptr);
	while (avail-- > 0)
	{
		*ptr++ = buffer[cons++];
		if (cons >= STRAW_RING_SIZE)
			cons = 0;
	}
	mb();
	if (ps->active)
		ring->in_cons = cons;
	else
		ring->out_cons = cons;

	return bin;
}

term_t cbif_pore_straw_info1(proc_t *proc, term_t *regs)
{
	term_t Pore = regs[0];
	if (!is_short_eid(Pore))
		badarg(Pore);
	pore_t *pr = pore_lookup(Pore);
	if (pr == 0 || pr->tag != A_STRAW)
		badarg(Pore);
	pore_straw_t *ps = (pore_straw_t *)pr;
	term_t refs = nil;
	for (int i = NUM_STRAW_REFS-1; i >= 0; i--)
	{
		int ref = (ps->active) ?ps->ring_refs[i]
							   :ps->page_map[i].ref;
		assert(fits_int(ref));
		refs = heap_cons(&proc->hp, tag_int(ref), refs);
	}

	assert(fits_int((int)pr->evtchn));
	return heap_tuple2(&proc->hp, refs, tag_int(pr->evtchn));
}

term_t cbif_pore_straw_avail1(proc_t *proc, term_t *regs)
{
	term_t Pore = regs[0];
	if (!is_short_eid(Pore))
		badarg(Pore);
	pore_t *pr = pore_lookup(Pore);
	if (pr == 0 || pr->tag != A_STRAW)
		badarg(Pore);

	pore_straw_t *ps = (pore_straw_t *)pr;
	straw_ring_t *ring = ps->shared;

	// how much we can read
	int avail1 = (ps->active) ?ring->in_prod - ring->in_cons
							  :ring->out_prod - ring->out_cons;
	while (avail1 < 0)
		avail1 += STRAW_RING_SIZE;

	// how much we can write
	int avail2 = (ps->active) ?ring->out_cons - ring->out_prod
							  :ring->in_cons - ring->in_prod;
	while (avail2 <= 0)
		avail2 += STRAW_RING_SIZE;
	avail2--;	// unused byte

	return heap_tuple2(&proc->hp, tag_int(avail1), tag_int(avail2));
}

term_t cbif_pore_poke1(proc_t *proc, term_t *regs)
{
	term_t Pore = regs[0];
	if (!is_short_eid(Pore))
		badarg(Pore);
	pore_t *pr = pore_lookup(Pore);
	if (pr == 0)
		badarg(Pore);

	event_kick(pr->evtchn);
	return A_TRUE;
}

term_t cbif_pore_close1(proc_t *proc, term_t *regs)
{
	term_t Pore = regs[0];
	if (!is_short_eid(Pore))
		badarg(Pore);

	pore_t *pr = pore_lookup(Pore);
	if (pr == 0)
		return A_FALSE;

	pore_destroy(pr);
	return A_TRUE;
}

