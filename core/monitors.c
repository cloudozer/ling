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

#include "monitors.h"

#include "ling_common.h"

#include "nalloc.h"
#include "scheduler.h"
#include "heap.h"
#include "atom_defs.h"

typedef struct monitor_t monitor_t;
struct monitor_t {
	uint64_t ref_id;
	term_t pid1;		// watcher
	term_t pid2;		// watchee
	term_t what;		// A_PROCESS
	monitor_t *next;
};

static memnode_t *monitor_nodes = 0;
static monitor_t *active_monitors = 0;
static monitor_t *free_monitors = 0;

int monitor(uint64_t ref_id, term_t pid1, term_t pid2)
{
	if (free_monitors == 0)
	{
		memnode_t *node = nalloc_N(QUICK_SIZE - sizeof(memnode_t));
		if (node == 0)
			return -NO_MEMORY;

		node->next = monitor_nodes;
		monitor_nodes = node;

		monitor_t *ptr = (monitor_t *)node->starts;
		while (ptr +1 <= (monitor_t *)node->ends)
		{
			ptr->next = free_monitors;
			free_monitors = ptr;
			ptr++;
		}
		assert(free_monitors != 0);
	}

	monitor_t *m = free_monitors;
	free_monitors = m->next;

	m->ref_id = ref_id;
	m->pid1 = pid1;
	m->pid2 = pid2;
	
	m->next = active_monitors;
	active_monitors = m;

	return 0;
}

int demonitor(uint64_t ref_id, term_t pid1)
{
	monitor_t **p = &active_monitors;
	while ((*p) != 0 && (*p)->ref_id != ref_id)
		p = &(*p)->next;

	if ((*p) == 0)
		return 0;

	if ((*p)->pid1 != pid1)
		return -1;	// someone else's monitoring

	monitor_t *m = *p;
	*p = (*p)->next;

	m->next = free_monitors;
	free_monitors = m;
	return 0;
}

int notify_monitors_N(term_t late, term_t reason)
{
	monitor_t **p = &active_monitors;
	while ((*p) != 0)
	{
		if ((*p)->pid1 == late || (*p)->pid2 == late)
		{
			monitor_t *m = *p;
			*p = (*p)->next;

			if (m->pid2 == late)
			{
				// notify the watcher
				proc_t *watcher = scheduler_lookup(m->pid1);
				assert(watcher != 0);

				term_t ref = heap_remake_local_ref_N(&watcher->hp, m->ref_id);
				if (ref == noval)
					return -NO_MEMORY;

				term_t marshalled_reason = reason;
				int x = heap_copy_terms_N(&watcher->hp, &marshalled_reason, 1);
				if (x < 0)
					return x;
				uint32_t *htop = heap_alloc_N(&watcher->hp, 1 +5);
				if (htop == 0)
					return -NO_MEMORY;
				heap_set_top(&watcher->hp, htop +1 +5);
				htop[0] = 5;
				htop[1] = ADOWN__;
				htop[2] = ref;
				htop[3] = A_PROCESS;
				htop[4] = late;
				htop[5] = marshalled_reason;

				x = scheduler_new_local_mail_N(watcher, tag_tuple(htop));
				if (x < 0)
					return x;
			}
		}
		else
			p = &(*p)->next;
	}

	return 0;
}

term_t list_monitored_by(term_t pid2, heap_t *hp)
{
	monitor_t *m = active_monitors;
	term_t list = nil;
	while (m != 0)
	{
		if (m->pid2 == pid2)
			list = heap_cons(hp, m->pid1, list);
		m = m->next;
	}
	return list;
}

term_t list_monitors(term_t pid1, heap_t *hp)
{
	monitor_t *m = active_monitors;
	term_t list = nil;
	while (m != 0)
	{
		if (m->pid1 == pid1)
			list = heap_cons(hp, heap_tuple2(hp, A_PROCESS, m->pid2), list);
		m = m->next;
	}
	return list;
}

//EOF
