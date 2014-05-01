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

#include "proc_queue.h"

#include "ling_common.h"
#include "string.h"

#define MIN_OPT_QUEUE_LEN	32

void proc_queue_init(proc_queue_t *pq)
{
	array_init(&pq->q, sizeof(proc_t *), 512);
	pq->hd = 0;
}

int proc_queue_put_N(proc_queue_t *pq, proc_t *proc)
{
	proc_t **ptr = array_push_N(&pq->q);
	if (ptr == 0)
		return -NO_MEMORY;

	*ptr = proc;
	return 0;
}

int proc_queue_is_empty(proc_queue_t *pq)
{
	return pq->q.nelts == pq->hd;
}

int proc_queue_count(proc_queue_t *pq)
{
	return pq->q.nelts - pq->hd;
}

proc_t *proc_queue_get(proc_queue_t *pq)
{
	if (pq->hd == pq->q.nelts)
		return 0;

	proc_t *proc = ((proc_t **)pq->q.elts)[pq->hd++];

	int n = pq->q.nelts - pq->hd;
	if (pq->hd > MIN_OPT_QUEUE_LEN && pq->hd > n/2)
	{
		proc_t **ps = (proc_t **)pq->q.elts;
		memcpy(ps, ps + pq->hd, n*sizeof(proc_t *));
		pq->q.nelts = n;
		pq->hd = 0;
	}

	assert(proc != 0);
	return proc;
}

void proc_queue_remove(proc_queue_t *pq, proc_t *proc)
{
	proc_t **ptr = (proc_t **)pq->q.elts + pq->hd;
	proc_t **end = (proc_t **)pq->q.elts + pq->q.nelts;
	while (ptr < end)
	{
		if (*ptr == proc)
		{
			proc_t **last = (proc_t **)array_pop(&pq->q);
			memmove(ptr, ptr+1, (last-ptr)*sizeof(proc_t *));
			return;
		}
		ptr++;
	}
}

void proc_queue_done(proc_queue_t *pq)
{
	array_done(&pq->q);
}

//EOF
