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
//
//

#include "timers.h"

#include "ling_common.h"

#include "nalloc.h"
#include "proc.h"
#include "scheduler.h"
#include "atom_defs.h"
#include "time.h"

struct etimer_t {
	uint64_t ref_id;

	uint64_t timeout;
	term_t dst;
	term_t msg;

	int enveloped;	// true - send {timeout,TRef,Msg} message, false - send Msg

	// The reference to the process that created the timer if msg is a pointer
	// term or 0 if msg is not. The reference is needed to update pending_timers
	// count of the sending process and and destroy it if is a zombie already.
	proc_t *sender;

	// The function that is called when the timer expire. It is introduced to
	// use the same code for TCP/IP and potentially other timers.
	etimer_func_t fire;

	etimer_t *next;
};

static etimer_t *free_timers = 0;
static etimer_t *active_timers = 0;
static memnode_t *etimer_nodes = 0;

static int erlang_fire(etimer_t *tm)
{
	proc_t *to_proc = (is_atom(tm->dst))
		?scheduler_process_by_name(tm->dst)
		:scheduler_lookup(tm->dst);

	int rc = 0;
	if (to_proc != 0)
	{
		term_t marsh_msg = tm->msg;
		if (tm->sender != to_proc)	// tm->sender may be zero
		{
			rc = heap_copy_terms_N(&to_proc->hp, &marsh_msg, 1);
			if (rc < 0)
				goto error;
		}

		term_t env_msg = marsh_msg;	// {timeout,TRef,Msg} or Msg
		if (tm->enveloped)
		{
			term_t tref = heap_remake_local_ref_N(&to_proc->hp, tm->ref_id);
			if (tref == noval)
			{
				rc = -NO_MEMORY;
				goto error;
			}

			uint32_t *htop = heap_alloc_N(&to_proc->hp, 1 +3);
			if (htop == 0)
			{
				rc = -NO_MEMORY;
				goto error;
			}
			heap_set_top(&to_proc->hp, htop +1 +3);
			env_msg = tag_tuple(htop);
			htop[0] = 3;
			htop[1] = A_TIMEOUT;
			htop[2] = tref;
			htop[3] = marsh_msg;
		}

		rc = scheduler_new_local_mail_N(to_proc, env_msg);
	}

error:
	if (tm->sender != 0)
	{
		assert(tm->sender->pending_timers > 0);
		tm->sender->pending_timers--;
		if (tm->sender->pending_timers == 0 &&
				tm->sender->my_queue == MY_QUEUE_PENDING_TIMERS)
			proc_destroy(tm->sender);	// destroy a zombie process
	}

	return rc;
}

int etimer_add(uint64_t ref_id, uint64_t timeout,
			term_t dst, term_t msg, proc_t *sender, int enveloped)
{
	assert(is_atom(dst) || is_short_pid(dst));

	if (free_timers == 0)
	{
		memnode_t *node = nalloc_N(QUICK_SIZE -sizeof(memnode_t));
		if (node == 0)
			return -NO_MEMORY;

		node->next = etimer_nodes;
		etimer_nodes = node;

		etimer_t *ptr = (etimer_t *)node->starts;
		while (ptr +1 <= (etimer_t *)node->ends)
		{
			ptr->next = free_timers;
			free_timers = ptr;
			ptr++;
		}
		assert(free_timers != 0);
	}

	etimer_t *tm = free_timers;
	free_timers = tm->next;

	tm->ref_id = ref_id;
	tm->timeout = timeout;
	tm->dst = dst;
	tm->msg = msg;

	if (is_immed(msg))
		tm->sender = 0;
	else
	{
		sender->pending_timers++;
		tm->sender = sender;
	}

	tm->enveloped = enveloped;
	tm->fire = erlang_fire;

	etimer_t **ref = &active_timers;
	etimer_t *ptr = active_timers;

	while (ptr != 0 && ptr->timeout < timeout)
	{
		ref = &ptr->next;
		ptr = ptr->next;
	}

	tm->next = ptr;
	*ref = tm;

	return 0;
}

uint64_t etimer_closest_timeout(void)
{
	if (active_timers == 0)
		return LING_INFINITY;
	return active_timers->timeout;
}

void etimer_expired(uint64_t now)
{
	while (active_timers != 0 && active_timers->timeout <= now)
	{
		etimer_t *tm = active_timers;
		active_timers = active_timers->next;

		//printk("*** etimer_expired: ref_id %ld\n", tm->ref_id);

		if (tm->fire(tm) < 0)
			printk("etimer_expired: scheduled message"
							" not delivered to %pt\n", T(tm->dst));

		tm->next = free_timers;
		free_timers = tm;
	}
}

int etimer_cancel(uint64_t ref_id, int64_t *left_ns)
{
	etimer_t **ref = &active_timers;
	etimer_t *tm = active_timers;
	while (tm != 0 && tm->ref_id != ref_id)
	{
		ref = &tm->next;
		tm = tm->next;
	}

	if (tm == 0)
		return -NOT_FOUND;

	*ref = tm->next;

	if (tm->sender != 0)
	{
		assert(tm->sender->pending_timers > 0);
		tm->sender->pending_timers--;
		if (tm->sender->pending_timers == 0 &&
				tm->sender->my_queue == MY_QUEUE_PENDING_TIMERS)
			proc_destroy(tm->sender);	// destroy a zombie process
	}

	*left_ns = tm->timeout - monotonic_clock();

	//printk("*** etimer_cancel: ref_id %ld left_ms %ld\n", tm->ref_id, *left_ns /1000000);

	tm->next = free_timers;
	free_timers = tm;
	return 0;
}

void etimer_cancel_by_receiver(term_t dst)
{
	etimer_t **ref = &active_timers;
	etimer_t *tm = active_timers;
	while (tm != 0)
	{
		if (tm->dst == dst)
		{
			if (tm->sender != 0)
			{
				assert(tm->sender->pending_timers > 0);
				tm->sender->pending_timers--;
				if (tm->sender->pending_timers == 0 &&
						tm->sender->my_queue == MY_QUEUE_PENDING_TIMERS)
					proc_destroy(tm->sender);	// destroy a zombie process
			}

			*ref = tm->next;
			etimer_t *free_me = tm;
			tm = tm->next;

			free_me->next = free_timers;
			free_timers = free_me;
		}
		else
		{
			ref = &tm->next;
			tm = tm->next;
		}
	}
}

int etimer_read(uint64_t ref_id, int64_t *left_ns)
{
	etimer_t *tm = active_timers;
	while (tm != 0 && tm->ref_id != ref_id)
		tm = tm->next;

	if (tm == 0)
		return -NOT_FOUND;

	*left_ns = tm->timeout - monotonic_clock();
	return 0;
}

//
// Timers is yet another source of root regions for garbage collection
//

void etimer_fill_root_regs(proc_t *sender, region_t *regs, int nr_expected)
{
	if (nr_expected == 0)
		return;

	int nr_found = 0;

	etimer_t *tm = active_timers;
	while (tm != 0)
	{
		if (tm->sender == sender)
		{
			regs[nr_found].starts = &tm->msg;
			regs[nr_found].ends = &tm->msg +1;
			nr_found++;
		}
		tm = tm->next;
	}
	
	assert(nr_found == nr_expected);
}

//EOF
