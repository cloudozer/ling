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

#include <stdint.h>

#include "array.h"
#include "proc.h"

//TODO: try other collections

typedef struct wait_list_t wait_list_t;
struct wait_list_t {
	array_t arr;
};

typedef struct proc_list_t proc_list_t;
struct proc_list_t {
	array_t arr;
};

void wait_list_init(wait_list_t *wl);
int wait_list_put_N(wait_list_t *wl, proc_t *proc, uint64_t until);
proc_t *wait_list_expired(wait_list_t *wl, uint64_t ticks);
uint64_t wait_list_timeout(wait_list_t *wl);
void wait_list_remove(wait_list_t *wl, proc_t *proc);
int wait_list_len(wait_list_t *wl);
proc_t *wait_list_at(wait_list_t *wl, int index);

void proc_list_init(proc_list_t *pl);
int proc_list_put_N(proc_list_t *pl, proc_t *proc);
void proc_list_remove(proc_list_t *pl, proc_t *proc);
int proc_list_len(proc_list_t *pl);
proc_t *proc_list_at(proc_list_t *pl, int index);

//EOF
