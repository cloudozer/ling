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

#include "bif_impl.h"

term_t cbif_process_flag2(proc_t *proc, term_t *regs)
{
	term_t Flag = regs[0];
	term_t Value = regs[1];
	if (!is_atom(Flag))
		badarg(Flag);

	term_t old_val = proc_set_flag(proc, Flag, Value);
	if (old_val == noval)
		badarg(Value);

	return old_val;
}

term_t cbif_process_flag3(proc_t *proc, term_t *regs)
{
	term_t Pid = regs[0];
	term_t Flag = regs[1];
	term_t Value = regs[2];
	if (!is_short_pid(Pid))
		badarg(Pid);
	proc_t *subj = scheduler_lookup(Pid);
	if (subj == 0)
		badarg(Pid);
	if (!is_atom(Flag))
		badarg(Flag);

	term_t old_val = proc_set_flag(subj, Flag, Value);
	if (old_val == noval)
		badarg(Value);

	return old_val;
}

term_t cbif_process_info2(proc_t *proc, term_t *regs)
{
	term_t Pid = regs[0];
	term_t What = regs[1];

	if (!is_short_pid(Pid))
		badarg(Pid);
	if (!is_atom(What))
		badarg(What);

	proc_t *probe = scheduler_lookup(Pid);
	if (probe == 0)
		return A_UNDEFINED;

	term_t val;
	if (What == A_BACKTRACE)
	{
		//TODO: current stack trace is not enough
		val = A_UNDEFINED;
	}
	else if (What == A_BINARY)
	{
		//NB: BinInfo is documented to be a list, yet its contents is unspesfied
		val = int_to_term(probe->hp.total_pb_size, &probe->hp);
	}
	else if (What == A_CATCHLEVEL)
	{
		assert(fits_int(probe->catch_level));
		val = tag_int(probe->catch_level);
	}
	else if (What == A_CURRENT_FUNCTION)
	{
		// NB: probe->cap.ip is valid even if proc == probe
		uint32_t *fi = backstep_to_func_info(probe->cap.ip);
		val = heap_tuple3(&proc->hp, fi[1], fi[2], tag_int(fi[3]));
	}
	else if (What == A_CURRENT_LOCATION)
	{
		// NB: probe->cap.ip is valid even if proc == probe
		uint32_t *fi = backstep_to_func_info(probe->cap.ip);
		term_t loc = nil;
		char fname[256];
		uint32_t line = code_base_source_line(probe->cap.ip, fname, sizeof(fname));
		if (line != 0)
		{
			term_t f = heap_strz(&proc->hp, fname);
			term_t t1 = heap_tuple2(&proc->hp, A_FILE, f);
			term_t t2 = heap_tuple2(&proc->hp, A_LINE, tag_int(line));
			loc = heap_cons(&proc->hp, t2, nil);
			loc = heap_cons(&proc->hp, t1, loc);
		}

		val = heap_tuple4(&proc->hp, fi[1], fi[2], tag_int(fi[3]), loc);
	}
	else if (What == A_CURRENT_STACKTRACE)
	{
		val = probe->stack_trace;
		if (probe != proc)
		{
			int x = heap_copy_terms_N(&proc->hp, &val, 1);
			if (x < 0)
				fail(err_to_term(x));
		}
	}
	else if (What == A_DICTIONARY)
	{
		val = probe->dictionary;
		if (probe != proc)
		{
			int x = heap_copy_terms_N(&proc->hp, &val, 1);
			if (x < 0)
				fail(err_to_term(x));
		}
	}
	else if (What == A_ERROR_HANDLER)
		val = A_ERROR_HANDLER;
	else if (What == A_GARBAGE_COLLECTION)
	{
		// BEAM returns a property list
		val = tag_int(probe->hp.minor_gcs);
	}
	else if (What == A_GROUP_LEADER)
		val = probe->group_leader;
	else if (What == A_HEAP_SIZE)
		val = int_to_term(probe->hp.total_size, &proc->hp);
	else if (What == A_INITIAL_CALL)
	{
		val = (probe->init_call_mod == noval)
				?A_UNDEFINED
				:heap_tuple3(&proc->hp, probe->init_call_mod,
										probe->init_call_func,
										tag_int(probe->init_call_arity));
	}
	else if (What == A_LINKS)
	{
		term_t ids = nil;
		plink_t *pl = probe->links.active;
		while (pl != 0)
		{
			ids = heap_cons(&proc->hp, pl->id, ids);
			pl = pl->next;
		}

		val = ids;
	}
	else if (What == A_LAST_CALLS)
	{
		//TODO
		val = A_FALSE;
	}
	else if (What == A_MEMORY)
	{
		int pages = 0;

		pages += probe->home_node->index;
		pages += probe->stack_node->index;
		memnode_t *node = probe->hp.nodes;
		while (node != 0)
		{
			pages += node->index;
			node = node->next;
		}
		node = probe->mailbox.nodes;
		while (node != 0)
		{
			pages += node->index;
			node = node->next;
		}
		node = probe->links.nodes;
		while (node != 0)
		{
			pages += node->index;
			node = node->next;
		}

		int bytes = pages * PAGE_SIZE;
		val = int_to_term(bytes, &proc->hp);
	}
	else if (What == A_MESSAGE_BINARY)
	{
		//TODO
		val = A_UNDEFINED;
	}
	else if (What == A_MESSAGE_QUEUE_LEN)
	{
		int len = msg_queue_len(&probe->mailbox);
		assert(fits_int(len));
		val = tag_int(len);
	}
	else if (What == A_MESSAGES)
	{
		int messages = nil;
		message_t *msg = probe->mailbox.head;
		while (msg != 0)
		{
			term_t marsh_body = msg->body;
			if (probe != proc)
			{
				int x = heap_copy_terms_N(&proc->hp, &marsh_body, 1);
				if (x < 0)
					fail(err_to_term(x));
			}
			messages = heap_cons(&proc->hp, marsh_body, messages);
			msg = msg->next;
		}

		val = list_rev(messages, &proc->hp);
	}
	else if (What == A_MIN_HEAP_SIZE)
		val = tag_int(INIT_HEAP_SIZE);
	else if (What == A_MIN_BIN_VHEAP_SIZE)
	{
		//TODO
		val = A_UNDEFINED;
	}
	else if (What == A_MONITORED_BY)
		val = list_monitored_by(probe->pid, &proc->hp);
	else if (What == A_MONITORS)
		val = list_monitors(probe->pid, &proc->hp);
	else if (What == A_PRIORITY)
		val = probe->priority;
	else if (What == A_REDUCTIONS)
		val = int_to_term(probe->total_reds, &proc->hp);
	else if (What == A_REGISTERED_NAME)
	{
		val = probe->name;
		if (val == noval)
			return nil;		// be backward compatible
	}
	else if (What == A_SEQUENTIAL_TRACE_TOKEN)
	{
		//TODO
		val = A_UNDEFINED;
	}
	else if (What == A_STACK_SIZE)
	{
		int ss = proc_stack_bottom(probe) - proc_stack_top(probe);
		assert(fits_int(ss));
		val = tag_int(ss);
	}
	else if (What == A_STATUS)
	{
		if (probe->my_queue == MY_QUEUE_NORMAL ||
			probe->my_queue == MY_QUEUE_HIGH ||
			probe->my_queue == MY_QUEUE_LOW)
				val = A_RUNNABLE;
		else if (probe->my_queue == MY_QUEUE_INF_WAIT ||
				 probe->my_queue == MY_QUEUE_TIMED_WAIT)
			val = A_WAITING;
		else
		{
			assert(probe->my_queue == MY_QUEUE_NONE);
			val = A_RUNNING;
		}
	}
	else if (What == A_SUSPENDING)
	{
		//TODO
		val = nil;
	}
	else if (What == A_TOTAL_HEAP_SIZE)
	{
		int ss = proc_stack_bottom(probe) - proc_stack_top(probe);
		int ths = probe->hp.total_size + ss;
		assert(fits_int(ths));
		val = tag_int(ths);
	}
	else if (What == A_TRACE)
	{
		//TODO
		val = A_UNDEFINED;
	}
	else if (What == A_TRAP_EXIT)
		val = probe->trap_exit;
	else
		badarg(What);

	return heap_tuple2(&proc->hp, What, val);
}

term_t cbif_group_leader0(proc_t *proc, term_t *rs)
{
	return proc->group_leader;
}

term_t cbif_group_leader2(proc_t *proc, term_t *rs)
{
	term_t Leader = rs[0];
	term_t Pid = rs[1];

	if (!is_short_pid(Leader) && !is_atom(Leader))
		badarg(Leader);
	if (!is_short_pid(Pid))
		badarg(Pid);

	proc_t *peer = scheduler_lookup(Pid);
	if (peer == 0)
		badarg(Pid);

	peer->group_leader = Leader;
	return A_TRUE;
}

term_t cbif_is_process_alive1(proc_t *proc, term_t *rs)
{
	term_t Pid = rs[0];
	if (!is_short_pid(Pid))
		badarg(Pid);

	if (scheduler_lookup(Pid) != 0)
		return A_TRUE;

	return A_FALSE;
}

term_t cbif_garbage_collect0(proc_t *proc, term_t *rs)
{
	//NB: no live registers
	//NB: supress_gc flag ignored

	int nr_messages = msg_queue_len(&proc->mailbox);
	int nr_regs = 1 +1 +1 +nr_messages +proc->pending_timers;
	if (nr_regs <= MAX_ROOT_REGS)
	{
		region_t root_regs[nr_regs];
		root_regs[0].starts = proc->stop;
		root_regs[0].ends = proc_stack_bottom(proc);
		root_regs[1].starts = &proc->dictionary;
		root_regs[1].ends = &proc->dictionary +1;
		root_regs[2].starts = &proc->stack_trace;
		root_regs[2].ends = &proc->stack_trace +1;

		// Creates a root region for each message in the mailbox
		msg_queue_fill_root_regs(&proc->mailbox, root_regs +3);		//NB: +3
		// Creates a root region for each pending timer
		etimer_fill_root_regs(proc, root_regs +3 +nr_messages,
											proc->pending_timers);	//NB: +3

		if (heap_gc_full_sweep_N(&proc->hp, root_regs, nr_regs) < 0)
		{
			printk("garbage_collect(): no memory during GC, ignored\n");
			return A_TRUE;
		}
	}
	else
		printk("garbage_collect(): too many roots, skipped\n");

	return A_TRUE;
}

term_t cbif_garbage_collect1(proc_t *proc, term_t *rs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_get_stacktrace0(proc_t *proc, term_t *regs)
{
	if (proc->stack_trace != noval)
		return proc->stack_trace;
	else
		return nil;
}

term_t cbif_spawn3(proc_t *proc, term_t *regs)
{
	term_t m = regs[0];
	term_t f = regs[1];
	term_t args = regs[2];

	if (!is_atom(m))
		badarg(m);
	if (!is_atom(f))
		badarg(f);
	if (!is_list(args))
		badarg(args);

	if (list_len(args) < 0)
		badarg(args); // too odd

	proc_t *new_proc = proc_make(proc->group_leader);
	int x = proc_spawn_N(new_proc, m, f, args);
	if (x < 0)
	{
		proc_destroy(new_proc);	// safe
		fail(err_to_term(x));
	}

	return new_proc->pid;
}

term_t cbif_spawn_link3(proc_t *proc, term_t *regs)
{
	term_t m = regs[0];
	term_t f = regs[1];
	term_t args = regs[2];

	if (!is_atom(m))
		badarg(m);
	if (!is_atom(f))
		badarg(f);
	if (!is_list(args))
		badarg(args);

	if (list_len(args) < 0)
		badarg(args); // too odd

	proc_t *new_proc = proc_make(proc->group_leader);
	int x = proc_spawn_N(new_proc, m, f, args);
	if (x == 0)
		x = inter_link_establish_N(&new_proc->links, proc->pid);
	if (x == 0)
		x = inter_link_establish_N(&proc->links, new_proc->pid);
	if (x < 0)
	{
		proc_destroy(new_proc);
		// no need to unlink, new_proc might have a link to proc but it is destroyed anyway
		fail(err_to_term(x));
	}

	return new_proc->pid;
}

term_t cbif_spawn_monitor3(proc_t *proc, term_t *regs)
{
	term_t m = regs[0];
	term_t f = regs[1];
	term_t args = regs[2];

	if (!is_atom(m))
		badarg(m);
	if (!is_atom(f))
		badarg(f);
	if (!is_list(args))
		badarg(args);

	if (list_len(args) < 0)
		badarg(args); // too odd

	term_t ref = heap_make_ref(&proc->hp);

	proc_t *new_proc = proc_make(proc->group_leader);
	int x = proc_spawn_N(new_proc, m, f, args);
	if (x == 0)
	{
		uint64_t ref_id = local_ref_id(ref);
		x = monitor(ref_id, proc->pid, new_proc->pid);
	}
	if (x < 0)
	{
		//NB: no need to demonitor
		proc_destroy(new_proc);

		if (x == -TOO_DEEP)
			fail(A_SYSTEM_LIMIT);
		else
			fail(A_NOT_SPAWNED);
	}

	return heap_tuple2(&proc->hp, new_proc->pid, ref);
}

term_t cbif_spawn1(proc_t *proc, term_t *regs)
{
	term_t Fun = regs[0];
	if (!is_boxed(Fun))
		badarg(Fun);
	uint32_t *fdata = peel_boxed(Fun);
	if (boxed_tag(fdata) != SUBTAG_FUN)
		badarg(Fun);
	t_fun_t *f = (t_fun_t *)fdata;
	if (f->fe == 0)
		not_implemented("unloaded funs");
	if (fun_arity(fdata) != fun_num_free(fdata))
		badarg();

	proc_t *new_proc = proc_make(proc->group_leader);
	int x = proc_spawn_fun0_N(new_proc, f);
	if (x < 0)
	{
		proc_destroy(new_proc);
		if (x == -TOO_DEEP)
			fail(A_SYSTEM_LIMIT);
		else
			fail(A_NOT_SPAWNED);
	}

	return new_proc->pid;
}

term_t cbif_spawn_link1(proc_t *proc, term_t *regs)
{
	term_t Fun = regs[0];
	if (!is_boxed(Fun))
		badarg(Fun);
	uint32_t *fdata = peel_boxed(Fun);
	if (boxed_tag(fdata) != SUBTAG_FUN)
		badarg(Fun);
	t_fun_t *f = (t_fun_t *)fdata;
	if (f->fe == 0)
		not_implemented("unloaded funs");

	proc_t *new_proc = proc_make(proc->group_leader);
	int x = proc_spawn_fun0_N(new_proc, f);
	if (x == 0)
		x = inter_link_establish_N(&new_proc->links, proc->pid);
	if (x == 0)
		x = inter_link_establish_N(&proc->links, new_proc->pid);
	if (x < 0)
	{
		proc_destroy(new_proc);
		// no need to unlink, new_proc might have a link to proc but it was destroyed anyway
		if (x == -TOO_DEEP)
			fail(A_SYSTEM_LIMIT);
		else
			fail(A_NOT_SPAWNED);
	}

	return new_proc->pid;
}

term_t cbif_spawn_monitor1(proc_t *proc, term_t *regs)
{
	term_t Fun = regs[0];
	if (!is_boxed(Fun))
		badarg(Fun);
	uint32_t *fdata = peel_boxed(Fun);
	if (boxed_tag(fdata) != SUBTAG_FUN)
		badarg(Fun);
	t_fun_t *f = (t_fun_t *)fdata;
	if (f->fe == 0)
		not_implemented("unloaded funs");

	term_t ref = heap_make_ref(&proc->hp);

	proc_t *new_proc = proc_make(proc->group_leader);
	int x = proc_spawn_fun0_N(new_proc, f);
	if (x == 0)
	{
		uint64_t ref_id = local_ref_id(ref);
		x = monitor(ref_id, proc->pid, new_proc->pid);
	}
	if (x < 0)
	{
		// no need to demonitor
		proc_destroy(new_proc);

		if (x == -TOO_DEEP)
			fail(A_SYSTEM_LIMIT);
		else
			fail(A_NOT_SPAWNED);
	}

	return heap_tuple2(&proc->hp, new_proc->pid, ref);
}

term_t cbif_monitor2(proc_t *proc, term_t *regs)
{
	term_t Type = regs[0];
	term_t Item = regs[1];

	if (Type != A_PROCESS)
		badarg(Type);
	if (!is_short_pid(Item) && !is_atom(Item))
		badarg(Item);

	term_t ref = heap_make_ref(&proc->hp);

	proc_t *target = (is_short_pid(Item))
		?scheduler_lookup(Item)
		:scheduler_process_by_name(Item);
	if (target == 0)
	{
		// the process is gone already - send notification immediately

		//	{'DOWN',#Ref<0.0.0.38>,process,<0.34.0>,noproc}
		term_t msg = heap_tuple5(&proc->hp, ADOWN__, ref, A_PROCESS, Item, A_NOPROC);
		int x = scheduler_new_local_mail_N(proc, msg);
		if (x < 0)
			fail(A_NO_MEMORY);
	}
	else
	{
		uint64_t ref_id = local_ref_id(ref);
		if (monitor(ref_id, proc->pid, target->pid) < 0)
			fail(A_NO_MEMORY);
	}

	return ref;
}

term_t cbif_demonitor1(proc_t *proc, term_t *regs)
{
	term_t MonRef = regs[0];
	if (!is_boxed(MonRef) || boxed_tag(peel_boxed(MonRef)) != SUBTAG_REF)
		badarg(MonRef);
	if (!ref_is_local(MonRef))
		badarg(MonRef);

	if (demonitor(local_ref_id(MonRef), proc->pid) < 0)
		badarg(MonRef);

	return A_TRUE;
}

term_t cbif_link1(proc_t *proc, term_t *regs)
{
	term_t PidOid = regs[0];

	if (!is_short_pid(PidOid) && !is_short_oid(PidOid))
		badarg(PidOid);

	if (PidOid == proc->pid)
		return A_TRUE;

	proc_t *peer_proc = 0;
	outlet_t *peer_outlet = 0;

	if (is_short_pid(PidOid))
		peer_proc = scheduler_lookup(PidOid);
	else
		peer_outlet = outlet_lookup(PidOid);

	inter_links_t *peer_links = 0;
	if (peer_proc != 0)
		peer_links = &peer_proc->links;
	else if (peer_outlet != 0)
		peer_links = &peer_outlet->links;

	if (peer_proc == 0 && peer_outlet == 0)
	{
		if (proc->trap_exit == A_TRUE)
		{
			int x = scheduler_signal_exit_N(proc, proc->pid, A_NOPROC);	// does not destroy the proc
			if (x < 0)
				fail(A_NOT_LINKED);

			return A_TRUE;
		}
		else
			fail(A_NOPROC);
	}
	else
	{
		if (!are_inter_linked(&proc->links, PidOid))
		{
			int x = inter_link_establish_N(&proc->links, PidOid);
			if (x < 0)
				fail(A_NOT_LINKED);
			x = inter_link_establish_N(peer_links, proc->pid);
			if (x < 0)
			{
				inter_link_break(&proc->links, PidOid);
				fail(A_NOT_LINKED);
			}
		}
		return A_TRUE;
	}
}

term_t cbif_unlink1(proc_t *proc, term_t *regs)
{
	term_t PidOid = regs[0];

	if (!is_short_pid(PidOid) && !is_short_oid(PidOid))
		badarg(PidOid);

	proc_t *peer_proc = 0;
	outlet_t *peer_outlet = 0;

	if (is_short_pid(PidOid))
		peer_proc = scheduler_lookup(PidOid);
	else
		peer_outlet = outlet_lookup(PidOid);

	inter_links_t *peer_links = 0;
	if (peer_proc != 0)
		peer_links = &peer_proc->links;
	else if (peer_outlet != 0)
		peer_links = &peer_outlet->links;

	if (peer_links != 0)
	{
		if (are_inter_linked(&proc->links, PidOid))
		{
			inter_link_break(&proc->links, PidOid);
			inter_link_break(peer_links, proc->pid);
		}
	}

	return A_TRUE;
}

term_t cbif_get_dictionary0(proc_t *proc, term_t *regs)
{
	return proc->dictionary;
}

term_t cbif_set_dictionary1(proc_t *proc, term_t *regs)
{
	term_t Dict = regs[0];
	if (!is_list(Dict))
		badarg(Dict);

	proc->dictionary = Dict;
	return A_OK;
}

//EOF
