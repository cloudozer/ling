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

#include "scheduler.h"

#include "proc_queue.h"
#include "wait_list.h"

#include "ling_common.h"

#include "event.h"
#include "time.h"
#include "hash.h"
#include "atom_defs.h"
#include "string.h"
#include "monitors.h"
#include "ets.h"
#include "outlet.h"
#include "timers.h"
#include "prof.h"

#include "heap.h"

#define NORMAL_ADVANTAGE	8

// The maximum number of processes purge_module() can terminate
#define PURGATORY_SIZE		16

// Smoothing factor for the interval between events; prefer 2^N -1
#define EVENT_GAP_SMOOTH	15

// Do not yield to hypervisor if the gap between events is lower than
#define MANUAL_POLLING_THRESHOLD	1000000

uint32_t next_proc_id;
hash_t *registry;
hash_t *named_processes;

static struct {
	proc_queue_t high_prio;
	proc_queue_t normal_prio;
	proc_queue_t low_prio;

	wait_list_t on_timed_receive;
	proc_list_t on_infinite_receive;
} queues;

static uint64_t runtime = 0;
static uint64_t rt_start;

static int normal_count = 0;

// A list of processes to terminate due to lingering on the old code of the
// module upon purge_module()
//
static proc_t *purgatory[PURGATORY_SIZE];
static int num_purged = 0;

// The time last 'hardware' event fired
static uint64_t last_event_fired_ns;

// The (moving) average of gaps between events
static uint64_t avg_event_gap_ns;

// Expect the next 'hardware' event in
static uint64_t expect_event_in_ns;

static int scheduler_park_runnable_N(proc_t *proc);

uint64_t lwip_closest_timeout(void);
void lwip_check_timeouts(void);

void netif_poll_all(void);

#ifdef LING_DEBUG
static enum sched_phase_t current_phase = PHASE_NONE;

#define set_phase(phase) current_phase = (phase)

void phase_expected(enum sched_phase_t phase)
{
	assert(current_phase == phase);
}

void phase_expected2(enum sched_phase_t phase1, enum sched_phase_t phase2)
{
	assert(current_phase == phase1 || current_phase == phase2);
}
#else
#define set_phase(phase)
#endif

void scheduler_init(void)
{
	next_proc_id = 0;

	registry = hash_make();
	named_processes = hash_make();
	proc_queue_init(&queues.high_prio);
	proc_queue_init(&queues.normal_prio);
	proc_queue_init(&queues.low_prio);

	wait_list_init(&queues.on_timed_receive);
	proc_list_init(&queues.on_infinite_receive);

	//runtime = 0;
	scheduler_runtime_start();

	last_event_fired_ns = monotonic_clock();
	avg_event_gap_ns = MANUAL_POLLING_THRESHOLD;
	expect_event_in_ns = MANUAL_POLLING_THRESHOLD;
}

static void update_event_times(int nr_fired, uint64_t ticks)
{
	uint64_t between_ns = ticks - last_event_fired_ns;
	if (nr_fired > 0)
	{
		// at least one event fired
		last_event_fired_ns = ticks;
		avg_event_gap_ns = (between_ns + avg_event_gap_ns *EVENT_GAP_SMOOTH)
								/(EVENT_GAP_SMOOTH +1);
		expect_event_in_ns = avg_event_gap_ns;
	}
	else
	{
		// no events fired
		expect_event_in_ns = (between_ns + avg_event_gap_ns *EVENT_GAP_SMOOTH)
									/(EVENT_GAP_SMOOTH +1);
	}
}

static void garbage_collect_waiting_processes(uint64_t alloted_ns)
{
	int nr_timed = wait_list_len(&queues.on_timed_receive);
	int nr_infinite = proc_list_len(&queues.on_infinite_receive);
	int nr_waiting = nr_timed + nr_infinite;
	if (nr_waiting == 0)
		return;

	int nr_scanned = 0;
	while (nr_scanned < nr_waiting)
	{
		int gc_runs = estimate_max_gc_runs(alloted_ns);
		if (gc_runs < 0)
			break;	// not enough time for any run

		static int64_t counter = 0;
		int index = counter % nr_waiting;
		proc_t *fatty = (index >= nr_timed)
				? proc_list_at(&queues.on_infinite_receive, index -nr_timed)
				: wait_list_at(&queues.on_timed_receive, index);
		heap_t *hp = &fatty->hp;
		if (hp->wait_gc_runs >= 0 && hp->wait_gc_runs <= gc_runs)
		{
			int nr_regs = proc_count_root_regs(fatty);
			if (nr_regs <= MAX_ROOT_REGS && !hp->suppress_gc)
			{
				region_t root_regs[nr_regs];
				proc_fill_root_regs(fatty, root_regs, fatty->cap.regs, fatty->cap.live);
				uint64_t gc_started_ns = monotonic_clock();
				if (heap_gc_non_recursive_N(hp, root_regs, nr_regs) < 0)
				{
					printk("garbage_collect_waiting_processes: no memory while collecting garbage, ignored\n");
					break;
				}
				uint64_t consumed_ns = (monotonic_clock() -gc_started_ns);
				alloted_ns -= consumed_ns;

				//printk("%d|%d|%d|%d|%llu|%llu\n",
				//	nr_timed,
				//	nr_infinite,
				//	gc_runs,
				//	hp->wait_gc_runs,
				//	consumed_ns,
				//	alloted_ns);

				hp->wait_gc_runs++;
				if (hp->gc_spot == 0)
					hp->wait_gc_runs = -1; // no more gc for this waiting process

				continue;	// continue with the same process
			}
		}

		counter++;
		nr_scanned++;
	}
}

void scheduler_runtime_start(void)
{
	rt_start = monotonic_clock();
}

void scheduler_runtime_update(void)
{
	runtime += (monotonic_clock() - rt_start);
}

uint64_t scheduler_runtime_get(void)
{
	return runtime;
}

// For the first born process only
void scheduler_enlist0(proc_t *first_born)
{
	first_born->pid = tag_short_pid(next_proc_id);
	next_proc_id++;
	hash_set(registry, &first_born->pid, sizeof(first_born->pid), first_born);
}

int scheduler_enlist_N(proc_t *spawning)
{
	assert(spawning != 0);
	spawning->pid = tag_short_pid(next_proc_id);
	next_proc_id++;
	int x = hash_set_N(registry,
			&spawning->pid, sizeof(spawning->pid), spawning);
	if (x == 0)
		x = scheduler_park_runnable_N(spawning);
	if (x < 0)
		return x;

	return 0; // Success
}

proc_t *scheduler_lookup(term_t pid)
{
	assert(is_short_pid(pid));
	return hash_get(registry, &pid, sizeof(pid));
}

static int scheduler_park_runnable_N(proc_t *proc)
{
	term_t prio = proc->priority;
	if (prio == A_NORMAL)
	{
		int x = proc_queue_put_N(&queues.normal_prio, proc);
		if (x < 0)
			return x;
		proc->my_queue = MY_QUEUE_NORMAL;
	}
	else if (prio == A_HIGH)
	{
		int x = proc_queue_put_N(&queues.high_prio, proc);
		if (x < 0)
			return x;
		proc->my_queue = MY_QUEUE_HIGH;
	}
	else // PRIO_LOW
	{
		int x = proc_queue_put_N(&queues.low_prio, proc);
		if (x < 0)
			return x;
		proc->my_queue = MY_QUEUE_LOW;
	}

	return 0; // Success
}

proc_t *scheduler_next(proc_t *current, int reds_left)
{
	set_phase(PHASE_NEXT);
	uint32_t reds_used = SLICE_REDUCTIONS -reds_left;
	ssi(SYS_STATS_CTX_SWITCHES);
	ssa(SYS_STATS_REDUCTIONS, reds_used);
	current->total_reds += reds_used;
	proc_t *next_proc = 0;
	uint64_t ticks = monotonic_clock(); // freeze time

	assert(current->my_queue == MY_QUEUE_NONE);

#ifdef PROFILE_HARNESS
static uint64_t proc_started_ns = 0;
	if (proc_started_ns != 0)
		prof_slice_complete(current->pid,
			current->result.what, current->cap.ip, proc_started_ns, ticks);
#endif

	proc_t *expired;
	while ((expired = wait_list_expired(&queues.on_timed_receive, ticks)) != 0)
	{
		expired->cap.ip = expired->result.jump_to;
		if (scheduler_park_runnable_N(expired) < 0)
			scheduler_exit_process(expired, A_NO_MEMORY);
	}

	int memory_exhausted = 0;
	switch (current->result.what)
	{
	case SLICE_RESULT_YIELD:
		if (scheduler_park_runnable_N(current) < 0)
			memory_exhausted = 1;
		break;
	case SLICE_RESULT_WAIT:
		if (current->result.until_when == LING_INFINITY)
		{
			if (proc_list_put_N(&queues.on_infinite_receive, current) < 0)
				memory_exhausted = 1;
			else
				current->my_queue = MY_QUEUE_INF_WAIT;
		}
		else
		{
			if (wait_list_put_N(&queues.on_timed_receive,
					current, current->result.until_when) < 0)
				memory_exhausted = 1;
			else
				current->my_queue = MY_QUEUE_TIMED_WAIT;
		}
		// Reset the number of gc runs for the waiting process
		current->hp.wait_gc_runs = 0;
		break;
	case SLICE_RESULT_DONE:
		scheduler_exit_process(current, A_NORMAL);
		break;

	case SLICE_RESULT_PURGE_PROCS:
		// purge_module() call may have detected processes lingering on the old
		// code - terminate them
		if (scheduler_park_runnable_N(current) < 0)
			memory_exhausted = 1;
		for (int i = 0; i < num_purged; i++)
			if (scheduler_signal_exit_N(purgatory[i], current->pid, A_KILL) < 0)
				memory_exhausted = 1;
		num_purged = 0;
		break;

	case SLICE_RESULT_EXIT:
		scheduler_exit_process(current, current->result.reason);
		// what about the returned value when main function just returns?
		break;
	case SLICE_RESULT_EXIT2:
		// only needed to implement erlang:exit/2
		if (scheduler_park_runnable_N(current) < 0 ||
				(scheduler_signal_exit_N(current->result.victim,
								         current->pid, 
								         current->result.reason2) < 0))
			memory_exhausted = 1;
		break;
	case SLICE_RESULT_ERROR:
		scheduler_exit_process(current, current->result.reason);
		// how is this different from SLICE_RESULT_EXIT?
		break;
	case SLICE_RESULT_THROW:
		scheduler_exit_process(current, current->result.reason);
		// how is this different from SLICE_RESULT_EXIT?
		break;
	default:
	{
		assert(current->result.what == SLICE_RESULT_OUTLET_CLOSE);
		if (scheduler_park_runnable_N(current) < 0)
			memory_exhausted = 1;
		outlet_t *closing = current->result.closing;
		assert(is_atom(current->result.why));
		outlet_close(closing, current->result.why);
		break;
	}
	}

	if (memory_exhausted)
		scheduler_exit_process(current, A_NO_MEMORY);

do_pending:

	ticks = monotonic_clock();
	while ((expired = wait_list_expired(&queues.on_timed_receive, ticks)) != 0)
	{
		expired->cap.ip = expired->result.jump_to;
		if (scheduler_park_runnable_N(expired) < 0)
			scheduler_exit_process(expired, A_NO_MEMORY);
	}

	set_phase(PHASE_EVENTS);
	// software events/timeouts
	lwip_check_timeouts();
	netif_poll_all();		// for loopback 
	etimer_expired(ticks);
	// 'hardware' events
	int nr_fired = events_do_pending();
	update_event_times(nr_fired, ticks);
	set_phase(PHASE_NEXT);

	// select_runnable
	if (!proc_queue_is_empty(&queues.high_prio))
		next_proc = proc_queue_get(&queues.high_prio);
	else if (normal_count < NORMAL_ADVANTAGE)
	{
		if (!proc_queue_is_empty(&queues.normal_prio))
			next_proc = proc_queue_get(&queues.normal_prio);
		else if (!proc_queue_is_empty(&queues.low_prio))
			next_proc = proc_queue_get(&queues.low_prio);
		normal_count++;
	}
	else
	{
		if (!proc_queue_is_empty(&queues.low_prio))
			next_proc = proc_queue_get(&queues.low_prio);
		else if (!proc_queue_is_empty(&queues.normal_prio))
			next_proc = proc_queue_get(&queues.normal_prio);
		normal_count = 0;
	}

	if (next_proc == 0)
	{
		// no runnable processes; poll for events from all three sources

		// Beware that events_poll() reports events 5us after they occur. If
		// a new event is expected very soon we are better off polling event
		// bits manually (using events_do_pending())

		// Devote a portion of time until the next event to gc waiting processes
		garbage_collect_waiting_processes(expect_event_in_ns /2);

		if (expect_event_in_ns < MANUAL_POLLING_THRESHOLD)
			goto do_pending;

		uint64_t next_ticks = wait_list_timeout(&queues.on_timed_receive);
		uint64_t closest_timeout = etimer_closest_timeout();
		if (closest_timeout < next_ticks)
			next_ticks = closest_timeout;

		closest_timeout = lwip_closest_timeout();
		if (closest_timeout < next_ticks)
			next_ticks = closest_timeout;

		scheduler_runtime_update();
		events_poll(next_ticks);		// LING_INFINITY is big enough
		scheduler_runtime_start();

		goto do_pending;
	}

	next_proc->my_queue = MY_QUEUE_NONE;
	
	//TODO: update stats

#ifdef PROFILE_HARNESS
	proc_started_ns = ticks;
#endif

	set_phase(PHASE_ERLANG);
	return next_proc;
}

int scheduler_new_local_mail_N(proc_t *proc, term_t msg)
{
	// marshalling done by the caller
	int x  = msg_queue_push_N(&proc->mailbox, msg);
	if (x < 0)
		return x;

	if (proc->my_queue == MY_QUEUE_NONE)
		return 0;
	if (proc->my_queue == MY_QUEUE_NORMAL ||
		proc->my_queue == MY_QUEUE_HIGH ||
		proc->my_queue == MY_QUEUE_LOW)
		return 0;

	if (proc->my_queue == MY_QUEUE_TIMED_WAIT)
		wait_list_remove(&queues.on_timed_receive, proc);
	else
	{
		assert(proc->my_queue == MY_QUEUE_INF_WAIT);
		proc_list_remove(&queues.on_infinite_receive, proc);
	}

	proc->my_queue = MY_QUEUE_NONE;
	return scheduler_park_runnable_N(proc);
}

void scheduler_dequeue_process(proc_t *proc)
{
	if (proc->my_queue == MY_QUEUE_NONE)
		return;

	if (proc->my_queue == MY_QUEUE_NORMAL)
		proc_queue_remove(&queues.normal_prio, proc);
	else if (proc->my_queue == MY_QUEUE_HIGH)
		proc_queue_remove(&queues.high_prio, proc);
	else if (proc->my_queue == MY_QUEUE_LOW)
		proc_queue_remove(&queues.low_prio, proc);
	else if (proc->my_queue == MY_QUEUE_TIMED_WAIT)
		wait_list_remove(&queues.on_timed_receive, proc);
	else
	{
		assert(proc->my_queue == MY_QUEUE_INF_WAIT);
		proc_list_remove(&queues.on_infinite_receive, proc);
	}

	proc->my_queue = MY_QUEUE_NONE;
}

int scheduler_signal_exit_N(proc_t *proc, term_t source, term_t reason)
{
	//printk("scheduler_signal_exit_N: pid %pt src %pt reason %pt\n", T(proc->pid), T(source), T(reason));
	if (reason == A_KILL)
	{
		scheduler_dequeue_process(proc);
		scheduler_exit_process(proc, A_KILLED);
		return 0;
	}

	if (proc->trap_exit == A_TRUE)
	{
		term_t marshalled_reason = reason;
		int x = heap_copy_terms_N(&proc->hp, &marshalled_reason, 1);
		if (x < 0)
		{
			printk("Cannot marshal the exit reason, replacing with 'undefined'\n");
			marshalled_reason = A_UNDEFINED;
		}

		// build {'EXIT',Pid,Reason}
		uint32_t *htop = heap_alloc_N(&proc->hp, 1 +3);
		if (htop == 0)
			return -NO_MEMORY;
		term_t msg = tag_tuple(htop);
		*htop++ = 3;
		*htop++ = AEXIT__;
		*htop++ = source;
		*htop++ = marshalled_reason;
		heap_set_top(&proc->hp, htop);

		x = scheduler_new_local_mail_N(proc, msg);
		if (x < 0)
			return x;
	}
	else
	{
		if (reason != A_NORMAL)
		{
			scheduler_dequeue_process(proc);
			scheduler_exit_process(proc, reason);
			return 0;
		}
	}

	return 0;
}

void scheduler_exit_process(proc_t *proc, term_t reason)
{
	assert(proc->my_queue == MY_QUEUE_NONE);

//	if (reason != A_NORMAL || proc->name == A_INIT)
//	{
//		if (proc->name == noval)
//			printk("*** Process %pt exits: %pt\n",
//				   			T(proc->pid), T(reason));
//		else
//			printk("*** Process %pt (%pt) exits: %pt\n",
//				   			T(proc->pid), T(proc->name), T(reason));
//	}

	// init calls halt() rather then exits normally
	assert(proc->name != A_INIT);

	//
	// Remove/unfix ETS table(s)
	//
	// NB: must precede monitor and link notifications
	//
	ets_process_exits(proc->pid);

	// Notify monitoring processes
	//
	int x = notify_monitors_N(proc->pid, reason);
	if (x < 0)
		printk("scheduler_exit_process: monitoring message(s)"
					" not delivered: pid %pt reason %pt\n", T(proc->pid), T(reason));

	// Cancel timers that are known to have proc->pid as their destination
	//
	etimer_cancel_by_receiver(proc->pid);

	//
	// Notify linked processes/outlets
	//
	inter_links_notify(&proc->links, proc->pid, reason);

	if (proc->name != noval)
		scheduler_unregister(proc);

	hash_set_N(registry, &proc->pid, sizeof(proc->pid), 0);	// never fails

	if (proc->pending_timers == 0)
		proc_destroy(proc);
	else
	{
		// The process is entering a 'zombie' state. There are pending timers
		// that have references to the process heap and thus the process heap
		// can not be released now. The process gets destroyed later, when the
		// last pending timer is fired or cancelled.
		proc->my_queue = MY_QUEUE_PENDING_TIMERS;
	}
}

proc_t *scheduler_process_by_name(term_t name)
{
	assert(is_atom(name));
	return hash_get(named_processes, &name, sizeof(name));
}

void scheduler_register(proc_t *proc, term_t name)
{
	assert(proc != 0);
	assert(proc->name == noval);
	proc->name = name;
	hash_set(named_processes, &proc->name, sizeof(proc->name), proc);
}

void scheduler_unregister(proc_t *proc)
{
	assert(proc != 0);
	assert(proc->name != noval);
	hash_set_N(named_processes, &proc->name, sizeof(proc->name), 0);	// never fails
	proc->name = noval;
}

int scheduler_run_queue(void)
{
	return proc_queue_count(&queues.high_prio) +
		   proc_queue_count(&queues.normal_prio) +
		   proc_queue_count(&queues.low_prio);
}

term_t scheduler_list_processes(heap_t *hp)
{
	hash_index_t hi;
	hash_start(registry, &hi);
	proc_t *proc;
	term_t ps = nil;
	while ((proc = hash_next(&hi)) != 0)
		ps = heap_cons(hp, proc->pid, ps);

	return ps;
}

term_t scheduler_list_registered(heap_t *hp)
{
	hash_index_t hi;
	hash_start(named_processes, &hi);
	proc_t *proc;
	term_t regs = nil;
	while ((proc = hash_next(&hi)) != 0)
		regs = heap_cons(hp, proc->name, regs);

	return regs;
}

// for use by purge_module()
void scheduler_add_purged(proc_t *proc)
{
	assert(num_purged < PURGATORY_SIZE);
	purgatory[num_purged++] = proc;
}

//EOF
