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

#include "pore.h"

#include "ling_common.h"
#include <string.h>
#include "event.h"
#include "scheduler.h"
#include "atom_defs.h"

static uint32_t next_pore_id = 0;
static pore_t *active_pores = 0;

static void pore_universal_handler(uint32_t evtchn, void *data);

pore_t *pore_make_N(term_t tag,
		uint32_t size, term_t owner, void (*destroy_private)(pore_t *), uint32_t evtchn)
{
	memnode_t *home = nalloc_N(size);
	if (home == 0)
		return 0;
	pore_t *np = (pore_t *)home->starts;
	memset(np, 0, size);

	np->eid = tag_short_eid(next_pore_id++);
	np->tag = tag;
	np->owner = owner;
	np->destroy_private = destroy_private;
	np->home = home;
	np->evtchn = evtchn;

	if (evtchn != NO_EVENT)
		event_bind(evtchn, pore_universal_handler, np);

	if (active_pores != 0)
		active_pores->ref = &np->next;
	np->ref = &active_pores;
	np->next = active_pores;
	active_pores = np;

	return np;
}

static void pore_universal_handler(uint32_t evtchn, void *data)
{
	assert(data != 0);
	pore_t *pore = (pore_t *)data;
	proc_t *proc = scheduler_lookup(pore->owner);
	if (proc == 0)
		return;	// drop

	// {irq,Pore}
	uint32_t *p = heap_alloc_N(&proc->hp, 3);
	if (p == 0)
		goto no_memory;
	term_t irq = tag_tuple(p);
	*p++ = 2;
	*p++ = A_IRQ;
	*p++ = pore->eid;
	heap_set_top(&proc->hp, p);

	if (scheduler_new_local_mail_N(proc, irq) < 0)
		goto no_memory;
	return;

no_memory:
	scheduler_signal_exit_N(proc, pore->eid, A_NO_MEMORY);
}

pore_t *pore_lookup(term_t eid)
{
	assert(is_short_eid(eid));
	pore_t *pr = active_pores;
	while (pr != 0)
	{
		if (pr->eid == eid)
			return pr;
		pr = pr->next;
	}
	return 0;
}

void pore_destroy(pore_t *pore)
{
	if (pore->evtchn != NO_EVENT)
		event_unbind(pore->evtchn);

	*pore->ref = pore->next;
	if (pore->next != 0)
		pore->next->ref = pore->ref;

	if (pore->destroy_private != 0)
		pore->destroy_private(pore);

	nfree(pore->home);
}

void pore_destroy_owned_by(term_t pid)
{
	pore_t *pr = active_pores;
	while (pr != 0)
	{
		if (pr->owner == pid)
		{
			pore_t *doomed = pr;
			pr = pr->next;
			pore_destroy(doomed);
		}
		else
			pr = pr->next;	
	}
}

