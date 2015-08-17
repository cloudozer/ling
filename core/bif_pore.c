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

