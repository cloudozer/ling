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

#include "proc.h"

#define SLICE_REDUCTIONS	1000

void scheduler_init(void);
void scheduler_runtime_start(void);
void scheduler_runtime_update(void);
uint64_t scheduler_runtime_get(void);
void scheduler_enlist0(proc_t *first_born);
int scheduler_enlist_N(proc_t *spawning);
proc_t *scheduler_lookup(term_t pid);
int scheduler_park_runnable(proc_t *proc);
proc_t *scheduler_next(proc_t *current, int reds_left);
int scheduler_new_local_mail_N(proc_t *proc, term_t msg);
void scheduler_dequeue_process(proc_t *proc);
int scheduler_signal_exit_N(proc_t *proc, term_t source, term_t reason);
void scheduler_exit_process(proc_t *proc, term_t reason);
proc_t *scheduler_process_by_name(term_t name);
void scheduler_register(proc_t *proc, term_t name);
void scheduler_unregister(proc_t *proc);
int scheduler_run_queue(void);
term_t scheduler_list_processes(heap_t *hp);
term_t scheduler_list_registered(heap_t *hp);
void scheduler_add_purged(proc_t *proc);

//EOF
