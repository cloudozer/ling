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

#include "sys_stats.h"

#include "ling_common.h"

#include <stdint.h>

static uint64_t sys_stats_counters[SYS_STATS_MAX];
static const char *sys_stats_formats[SYS_STATS_MAX];

void sys_stats_init(void)
{
	for (int i = 0; i < SYS_STATS_MAX; i++)
		sys_stats_counters[i] = 0;

	sys_stats_formats[SYS_STATS_REDUCTIONS] = "proc_main: %llu reductions made\n";
	sys_stats_formats[SYS_STATS_IO_INPUT] = "io: %llu received\n";
	sys_stats_formats[SYS_STATS_IO_OUTPUT] = "io: %llu sent\n";
	sys_stats_formats[SYS_STATS_CTX_SWITCHES] = "sched: %llu contexts switched\n";
	sys_stats_formats[SYS_STATS_GC_RUNS] = "GC: %llu runs\n";
	sys_stats_formats[SYS_STATS_GC_WORDS_RECLAIM] = "GC: %llu words reclaimed\n";
}

void sys_stats_dump(void)
{
	printk("--- System statistics:\n");
	for (int i = 0; i < SYS_STATS_MAX; i++)
		if (sys_stats_formats[i] != 0)
			printk(sys_stats_formats[i], sys_stats_counters[i]);
}

void sys_stats_inc(int what)
{
	assert(what >= 0 && what < SYS_STATS_MAX);
	sys_stats_counters[what]++;
}

void sys_stats_add(int what, uint64_t n)
{
	assert(what >= 0 && what < SYS_STATS_MAX);
	sys_stats_counters[what] += n;
}

uint64_t sys_stats_reductions(void)
{
	return sys_stats_counters[SYS_STATS_REDUCTIONS];
}

uint64_t sys_stats_io_input(void)
{
	return sys_stats_counters[SYS_STATS_IO_INPUT];
}

uint64_t sys_stats_io_output(void)
{
	return sys_stats_counters[SYS_STATS_IO_OUTPUT];
}

uint64_t sys_stats_ctx_switches(void)
{
	return sys_stats_counters[SYS_STATS_CTX_SWITCHES];
}

uint64_t sys_stats_gc_runs(void)
{
	return sys_stats_counters[SYS_STATS_GC_RUNS];
}

uint64_t sys_stats_gc_words_reclaimed(void)
{
	return sys_stats_counters[SYS_STATS_GC_WORDS_RECLAIM];
}

//EOF
