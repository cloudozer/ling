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

#pragma once

#include "term.h"
#include "nalloc.h"
#include "heap.h"

typedef struct message_t message_t;
struct message_t {
	term_t body;
	message_t *next;
};

typedef struct msg_queue_t msg_queue_t;
struct msg_queue_t {
	message_t *head;
	message_t **tail;
	int count;

	message_t **current;

	// the dark recv_mark/recv_set machinery
	message_t **saved_last;
	uint32_t *mark;

	message_t *free;

	memnode_t *nodes;
};

void msg_queue_init(msg_queue_t *mq, uint32_t *buf_starts, uint32_t *buf_ends);
void msg_queue_mark(msg_queue_t *mq, uint32_t *mark);
void msg_queue_restore(msg_queue_t *mq, uint32_t *mark);
int msg_queue_push_N(msg_queue_t *mq, term_t t);
int msg_queue_len(msg_queue_t *mq);
term_t msg_queue_current(msg_queue_t *mq);
void msg_queue_next(msg_queue_t *mq);
void msg_queue_reset(msg_queue_t *mq);
void msg_queue_drop(msg_queue_t *mq);
void msg_queue_fill_root_regs(msg_queue_t *mq, region_t *regs);
void msg_queue_done(msg_queue_t *mq);

//EOF
