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

#include "msg_queue.h"

#include "ling_common.h"

#include "string.h"

#define MSG_QUEUE_NODE_SIZE	4096

void msg_queue_init(msg_queue_t *mq, uint32_t *buf_starts, uint32_t *buf_ends)
{
	memset(mq, 0, sizeof(*mq));

	//mq->head = 0;
	mq->tail = &mq->head;
	//mq->count = 0;
	
	//mq->saved_last = 0;
	//mq->mark = 0;

	//mq->free = 0;
	message_t *ptr = (message_t *)buf_starts;
	while (ptr+1 <= (message_t *)buf_ends)
	{
		ptr->next = mq->free;
		mq->free = ptr;
		ptr++;
	}

	//mq->nodes = 0;
}

void msg_queue_mark(msg_queue_t *mq, uint32_t *mark)
{
	mq->mark = mark;
	mq->saved_last = mq->current;
}

void msg_queue_restore(msg_queue_t *mq, uint32_t *mark)
{
	if (mq->mark == mark)
		mq->current = mq->saved_last;
}

int msg_queue_push_N(msg_queue_t *mq, term_t t)
{
	if (mq->free == 0)
	{
		memnode_t *node = nalloc_N(MSG_QUEUE_NODE_SIZE - sizeof(memnode_t));
		if (node == 0)
			return -NO_MEMORY;

		node->next = mq->nodes;
		mq->nodes = node;

		message_t *ptr = (message_t *)node->starts;
		while (ptr+1 <= (message_t *)node->ends)
		{
			ptr->next = mq->free;
			mq->free = ptr;
			ptr++;
		}
		assert(mq->free != 0);
	}

	message_t *msg = mq->free;
	mq->free = msg->next;

	msg->body = t;
	msg->next = 0;
	
	*mq->tail = msg;
	mq->tail = &msg->next;

	mq->count++;

	return 0; // Success
}

int msg_queue_len(msg_queue_t *mq)
{
	return mq->count;
}

term_t msg_queue_current(msg_queue_t *mq)
{
	if (mq->current == 0)
		mq->current = &mq->head;
	if (*mq->current != 0)
		return (*mq->current)->body;
	else
		return noval;
}

void msg_queue_next(msg_queue_t *mq)
{
	if (*mq->current != 0)
		mq->current = &(*mq->current)->next;
	else
		mq->current = 0;
}

void msg_queue_reset(msg_queue_t *mq)
{
	mq->current = 0;
}

void msg_queue_drop(msg_queue_t *mq)
{
	assert(*mq->current != 0);
	message_t *msg = *mq->current;

	if (mq->tail == &msg->next)
	{
		assert(msg->next == 0);
		mq->tail = mq->current;
	}
	*mq->current = msg->next;

	msg->body = noval;
	msg->next = mq->free;
	mq->free = msg;

	mq->count--;
}

void msg_queue_fill_root_regs(msg_queue_t *mq, region_t *regs)
{
	message_t *msg = mq->head;
	while (msg != 0)
	{
		regs->starts = &msg->body;
		regs->ends = &msg->body+1;
		regs++;
		msg = msg->next;
	}
}

void msg_queue_done(msg_queue_t *mq)
{
	nfree_chain(mq->nodes);
}

//EOF
