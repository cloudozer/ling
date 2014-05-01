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
// A profiler
//

#include "ling_common.h"

#ifdef PROFILE_HARNESS

//NB: the profiling code may not work if modules are added/removed dynamically

#include "term.h"
#include "qsort.h"
#include "code_base.h"

#include <string.h>

#define MAX_PROF_PROCS	1024
#define MAX_PROF_LOCS	65536
#define NUM_PROF_TOP	16

typedef struct loc_time_t loc_time_t;
struct loc_time_t {
	uint32_t *ip;
	uint64_t total_wait_ns;
	uint64_t total_runtime_ns;
	int nr_hits;
};

typedef struct proc_time_t proc_time_t;
struct proc_time_t {
	term_t pid;
	uint32_t *ip;
	uint64_t wait_since_ns;
};

static proc_time_t proc_times[MAX_PROF_PROCS];
static int nr_proc_times = 0;

static loc_time_t loc_times[MAX_PROF_LOCS];
static int nr_loc_times = 0;

static int nr_events = 0;
static int nr_errors = 0;
static int nr_overflows = 0;

static int is_running = 0;

static proc_time_t *lookup_proc_time(term_t pid);
static proc_time_t *new_proc_time(term_t pid);
static loc_time_t *lookup_loc_time(uint32_t *ip);
static loc_time_t *new_loc_time(uint32_t *ip);

void prof_restart(void)
{
	nr_proc_times = 0;
	nr_loc_times = 0;
	nr_events = 0;
	nr_errors = 0;
	nr_overflows = 0;
	is_running = 1;
}

void prof_slice_complete(term_t pid,
		int status, uint32_t *ip, uint64_t started_ns, uint64_t finished_ns)
{
	//NB: status ignored

	if (!is_running)
		return;

	proc_time_t *pt = lookup_proc_time(pid);
	if (pt != 0)
	{
		if (started_ns < pt->wait_since_ns)
		{
			nr_errors++;
			started_ns = pt->wait_since_ns;
		}

		uint64_t wait_time_ns = started_ns - pt->wait_since_ns;
		assert(finished_ns >= started_ns);

		uint64_t runtime_ns = finished_ns - started_ns;

		loc_time_t *lt = lookup_loc_time(ip);
		if (lt != 0)
		{
			lt->total_wait_ns += wait_time_ns;
			lt->total_runtime_ns += runtime_ns;
			lt->nr_hits++;
		}
		else
		{
			// new location
			loc_time_t *new_lt = new_loc_time(ip);
			if (new_lt == 0)
				nr_overflows++;
			else
			{
				new_lt->total_wait_ns = wait_time_ns;
				new_lt->total_runtime_ns = runtime_ns;
				new_lt->nr_hits = 1;
			}
		}

		pt->ip = ip;
		pt->wait_since_ns = finished_ns;
	}
	else
	{
		// first time
		proc_time_t *new_pt = new_proc_time(pid);
		if (new_pt == 0)
			nr_overflows++;
		else
		{
			new_pt->ip = ip;
			new_pt->wait_since_ns = finished_ns;
		}
	}

	nr_events++;
}

void prof_stop(uint64_t stopped_ns)
{
	//NB: currently waiting processes not counted
	is_running = 0;
}

static int compare_wait_time(const void *p1, const void *p2)
{
	loc_time_t *lt1 = (loc_time_t *)p1;
	loc_time_t *lt2 = (loc_time_t *)p2;

	if (lt1->total_wait_ns < lt2->total_wait_ns)
		return 1;
	if (lt2->total_wait_ns < lt1->total_wait_ns)
		return -1;

	return 0;
}

static int compare_runtime(const void *p1, const void *p2)
{
	loc_time_t *lt1 = (loc_time_t *)p1;
	loc_time_t *lt2 = (loc_time_t *)p2;

	if (lt1->total_runtime_ns < lt2->total_runtime_ns)
		return 1;
	if (lt2->total_runtime_ns < lt1->total_runtime_ns)
		return -1;

	return 0;
}

static int compare_hits(const void *p1, const void *p2)
{
	loc_time_t *lt1 = (loc_time_t *)p1;
	loc_time_t *lt2 = (loc_time_t *)p2;

	if (lt1->nr_hits < lt2->nr_hits)
		return 1;
	if (lt2->nr_hits < lt1->nr_hits)
		return -1;

	return 0;
}

void prof_print_summary(void)
{
	printk("\n================ profiling report ================\n");
	printk("events recorded: %d\n", nr_events);
	if (nr_errors != 0)
		printk("*** errors detected: %d\n", nr_errors);
	if (nr_overflows != 0)
		printk("*** overflows detected: %d\n", nr_overflows);

	int i;
	printk("\ntop locations by waiting time:\n");
	qsort(loc_times, nr_loc_times, sizeof(loc_time_t), compare_wait_time);
	for (i = 0; i < NUM_PROF_TOP; i++)
	{
		if (i >= nr_loc_times)
			break;
		loc_time_t *lt = &loc_times[i];
		char fname[256];
		uint32_t line = code_base_source_line(lt->ip, fname, sizeof(fname));
		if (line != 0)
			printk("%s:%d ", fname, line);
		else
			printk("(0x%pp) ", lt->ip);
		printk("wait %lums run %lums hits %d\n",
			lt->total_wait_ns /1000000, lt->total_runtime_ns /1000000, lt->nr_hits);
	}

	printk("\ntop locations by runtime:\n");
	qsort(loc_times, nr_loc_times, sizeof(loc_time_t), compare_runtime);
	for (i = 0; i < NUM_PROF_TOP; i++)
	{
		if (i >= nr_loc_times)
			break;
		loc_time_t *lt = &loc_times[i];
		char fname[256];
		uint32_t line = code_base_source_line(lt->ip, fname, sizeof(fname));
		if (line != 0)
			printk("%s:%d ", fname, line);
		else
			printk("(0x%pp) ", lt->ip);
		printk("wait %lums run %lums hits %d\n",
			lt->total_wait_ns /1000000, lt->total_runtime_ns /1000000, lt->nr_hits);
	}

	printk("\ntop locations by hits:\n");
	qsort(loc_times, nr_loc_times, sizeof(loc_time_t), compare_hits);
	for (i = 0; i < NUM_PROF_TOP; i++)
	{
		if (i >= nr_loc_times)
			break;
		loc_time_t *lt = &loc_times[i];
		char fname[256];
		uint32_t line = code_base_source_line(lt->ip, fname, sizeof(fname));
		if (line != 0)
			printk("%s:%d ", fname, line);
		else
			printk("(0x%pp) ", lt->ip);
		printk("wait %lums run %lums hits %d\n",
			lt->total_wait_ns /1000000, lt->total_runtime_ns /1000000, lt->nr_hits);
	}

	if (is_running)
		printk("*** profiling is still running\n");
}

//------------------------------------------------------------------------------

static proc_time_t *lookup_proc_time(term_t pid)
{
	if (nr_proc_times == 0)
		return 0;

	proc_time_t *alpha = proc_times;
	proc_time_t *beta = proc_times +nr_proc_times;
	if (pid < alpha->pid)
		return 0;
	while (beta -alpha > 0)
	{
		proc_time_t *mid = alpha + (beta -alpha) /2;
		if (pid < mid->pid)
			beta = mid;
		else if (pid == mid->pid)
			return mid;
		else
			alpha = mid +1;
	}

	return 0;
}

static proc_time_t *new_proc_time(term_t pid)
{
	if (nr_proc_times >= MAX_PROF_PROCS)
		return 0;

	// keep the list sorted
	proc_time_t *ptr = proc_times;
	proc_time_t *end = ptr +nr_proc_times;
	while (ptr < end && ptr->pid <= pid)
		ptr++;

	// move tail by one cell
	if (ptr < end)
		memmove(ptr +1, ptr, (end -ptr)*sizeof(proc_time_t));
	nr_proc_times++;
	ptr->pid = pid;
	return ptr;
}

static loc_time_t *lookup_loc_time(uint32_t *ip)
{
	if (nr_loc_times == 0)
		return 0;

	loc_time_t *alpha = loc_times;
	loc_time_t *beta = loc_times +nr_loc_times;
	if (ip < alpha->ip)
		return 0;
	while (beta -alpha > 0)
	{
		loc_time_t *mid = alpha + (beta -alpha) /2;
		if (ip < mid->ip)
			beta = mid;
		else if (ip == mid->ip)
			return mid;
		else
			alpha = mid +1;
	}

	return 0;
}

static loc_time_t *new_loc_time(uint32_t *ip)
{
	if (nr_loc_times >= MAX_PROF_LOCS)
		return 0;

	// keep the list sorted
	loc_time_t *ptr = loc_times;
	loc_time_t *end = ptr +nr_loc_times;
	while (ptr < end && ptr->ip <= ip)
		ptr++;

	// move tail by one cell
	if (ptr < end)
		memmove(ptr +1, ptr, (end -ptr)*sizeof(loc_time_t));
	nr_loc_times++;
	ptr->ip = ip;
	return ptr;
}

#endif /* PROFILE_HARNESS */

//EOF
