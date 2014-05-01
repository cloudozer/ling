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

#include "wait_list.h"

#include "ling_common.h"

#include "string.h"

#define INIT_WAIT_LIST_SIZE	256
#define INIT_PROC_LIST_SIZE	256

typedef struct timed_proc_t timed_proc_t;
struct timed_proc_t {
	proc_t *proc;
	uint64_t timeout;
};

void wait_list_init(wait_list_t *wl)
{
	array_init(&wl->arr, sizeof(timed_proc_t), INIT_WAIT_LIST_SIZE);
}

int wait_list_put_N(wait_list_t *wl, proc_t *proc, uint64_t until)
{
	timed_proc_t *last = array_push_N(&wl->arr);
	if (last == 0)
		return -NO_MEMORY;

	timed_proc_t *ptr = (timed_proc_t *)wl->arr.elts;
	while (ptr < last)
	{
		if (ptr->timeout > until)
			break;
		ptr++;
	}

	if (ptr < last)
		memmove(ptr+1, ptr, (last-ptr)*sizeof(timed_proc_t));
		
	ptr->proc = proc;
	ptr->timeout = until;
	//debug("proc %pt put on wait list until %llu\r\n",
	//	T(ptr->proc->pid), ptr->timeout);

	return 0;
}

proc_t *wait_list_expired(wait_list_t *wl, uint64_t now)
{
	if (wl->arr.nelts == 0)
		return 0;
	
	timed_proc_t *ptr = (timed_proc_t *)wl->arr.elts;
	if (ptr->timeout <= now)
	{
		//debug("proc %pt timeout expired: timeout %llu now %llu\r\n",
		//	T(ptr->proc->pid), ptr->timeout, now);
		proc_t *expired = ptr->proc;
		timed_proc_t *last = (timed_proc_t *)array_pop(&wl->arr);
		//NB: array_pop() keeps elements in place
		memmove(ptr, ptr+1, (last-ptr)*sizeof(timed_proc_t));
		return expired;
	}

	return 0;
}

uint64_t wait_list_timeout(wait_list_t *wl)
{
	if (wl->arr.nelts == 0)
		return LING_INFINITY;

	timed_proc_t *ptr = (timed_proc_t *)wl->arr.elts;
	return ptr->timeout;
}

void wait_list_remove(wait_list_t *wl, proc_t *proc)
{
	timed_proc_t *ptr = (timed_proc_t *)wl->arr.elts;
	timed_proc_t *end = ptr + wl->arr.nelts;
	while (ptr < end)
	{
		if (ptr->proc == proc)
		{
			if (ptr < end-1)
				memmove(ptr, ptr+1, (end-ptr -1)*sizeof(timed_proc_t));
			wl->arr.nelts--;
			break;
		}
		ptr++;
	}	
}

int wait_list_len(wait_list_t *wl)
{
	return wl->arr.nelts;
}

proc_t *wait_list_at(wait_list_t *wl, int index)
{
	assert(index >= 0 && index < wl->arr.nelts);
	timed_proc_t *p = (timed_proc_t *)wl->arr.elts;
	return p[index].proc;
}

void proc_list_init(proc_list_t *pl)
{
	array_init(&pl->arr, sizeof(proc_t *), INIT_PROC_LIST_SIZE);
}

int proc_list_put_N(proc_list_t *pl, proc_t *proc)
{
	proc_t **last = array_push_N(&pl->arr);
	if (last == 0)
		return -1;

	*last = proc;
	return 0;
}

void proc_list_remove(proc_list_t *pl, proc_t *proc)
{
	proc_t **ptr = (proc_t **)pl->arr.elts;
	proc_t **end = ptr + pl->arr.nelts;
	while (ptr < end)
	{
		if (*ptr == proc)
		{
			if (ptr < end-1)
				memmove(ptr, ptr+1, (end-ptr -1)*sizeof(proc_t *));
			pl->arr.nelts--;
		}
		ptr++;
	}
}

int proc_list_len(proc_list_t *pl)
{
	return pl->arr.nelts;
}

proc_t *proc_list_at(proc_list_t *pl, int index)
{
	assert(index >= 0 && index < pl->arr.nelts);
	proc_t **p = (proc_t **)pl->arr.elts;
	return p[index];
}

//EOF
