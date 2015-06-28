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

#include "proc.h"

#include "ling_common.h"

#include "nalloc.h"
#include "atom_defs.h"
#include "string.h"
#include "scheduler.h"
#include "outlet.h"
#include "limits.h"
#include "timers.h"

proc_t *proc_make(term_t leader)
{
	memnode_t *home_node = nalloc(QUICK_SIZE - sizeof(memnode_t));
	proc_t *proc = (proc_t *)home_node->starts;
	home_node->starts += WSIZE(proc_t);

	memset(proc, 0, sizeof(*proc));
	proc->home_node = home_node;

	// A boundary between the initial heap and the initial stack
	uint32_t *boundary = home_node->starts + INIT_HEAP_SIZE;
	assert(boundary <= home_node->ends);

	proc->pid = noval; // set by scheduler when the process is enlisted
	proc->name = noval;

	// The initial stack
	proc->init_send = boundary;
	proc->stop = home_node->ends;
	// proc->stack_node = 0;
	
	proc->stack_trace = nil;

	//proc->total_reds = 0;

	//proc->catch_level = 0;
	
	//proc->pending_times = 0;
	
	proc->trap_exit = A_FALSE;

	heap_init(&proc->hp, home_node->starts, boundary);

	msg_queue_init(&proc->mailbox,
			proc->mb_space,
		   	proc->mb_space + sizeof(proc->mb_space)/sizeof(uint32_t));

	proc->init_call_mod = noval;
	proc->init_call_func = noval;
	//proc->init_call_arity = 0;

	proc->dictionary = nil;

	proc->group_leader = leader;

	proc->priority = A_NORMAL;
	proc->my_queue = MY_QUEUE_NONE;

	inter_links_init(&proc->links);

	proc->last_excep_class = noval;

	return proc;
}

void proc_stack_ensure(proc_t *proc, int needed)
{
	assert(proc->stop - proc_stack_end(proc) < needed);

	uint32_t *sbot = (proc->stack_node == 0)
		? proc->home_node->ends
		: proc->stack_node->ends;

	int stack_size = sbot - proc->stop;

	int new_size = (needed > stack_size)
		? stack_size + needed
		: 2*stack_size;

	memnode_t *new_node = nalloc(new_size*sizeof(uint32_t));

	uint32_t *new_top = new_node->ends - stack_size;
	memcpy(new_top, proc->stop, stack_size * sizeof(uint32_t));

	if (proc->stack_node != 0)
		nfree(proc->stack_node);
	else
	{
		// The stack stack just left the home node; give some
		// slack to the init_node of the heap
		heap_reset_init_node_end(&proc->hp, proc->home_node->ends);
	}

	proc->stack_node = new_node;
	proc->stop = new_top;

	//debug("Proc %pt: stack enlarged (used %d free %ld needed %d)\n",
	//	T(proc->pid), stack_size, proc->stop - proc_stack_end(proc), needed);
}

uint32_t *proc_stack_top(proc_t *proc)
{
	return proc->stop;
}

void proc_stack_set_top(proc_t *proc, uint32_t *new_top)
{
	assert(new_top != 0);
	assert(proc->stack_node != 0 || new_top >= proc->init_send);
	assert(proc->stack_node == 0 || new_top >= proc->stack_node->starts);
	proc->stop = new_top;
}

uint32_t *proc_stack_end(proc_t *proc)
{
	return (proc->stack_node == 0)
		? proc->init_send
		: proc->stack_node->starts;
}

uint32_t *proc_stack_bottom(proc_t *proc)
{
	return (proc->stack_node == 0)
		? proc->home_node->ends
		: proc->stack_node->ends;
}

void inter_links_init(inter_links_t *links)
{
	// memset(links, 0, size) assumed
	assert(links->active == 0);
	assert(links->free == 0);
	plink_t *pl = links->init_space;
	while (pl < links->init_space + LINK_SPACE_SIZE)
	{
		pl->id = noval;
		pl->next = links->free;
		links->free = pl;
		pl++;
	}
	assert(links->nodes == 0);
}

int are_inter_linked(inter_links_t *links, term_t id)
{
	plink_t *pl = links->active;
	while (pl != 0)
	{
		if (pl->id == id)
			return 1;
		pl = pl->next;
	}

	return 0;
}

int inter_link_establish_N(inter_links_t *links, term_t id)
{
	if (links->free == 0)
	{
		memnode_t *node = nalloc_N(QUICK_SIZE - sizeof(memnode_t));
		if (node == 0)
			return -NO_MEMORY;
		plink_t *pl = (plink_t *)node->starts;
		while (pl +1 <= (plink_t *)node->ends)
		{
			pl->id = noval;
			pl->next = links->free;
			links->free = pl;
			pl++;
		}

		node->next = links->nodes;
		links->nodes = node;

		assert(links->free != 0);
	}

	plink_t *link = links->free;
	links->free = link->next;

	link->id = id;
	link->next = links->active;
	links->active = link;

	return 0;
}

void inter_link_break(inter_links_t *links, term_t id)
{
	plink_t *found = 0;

	plink_t **ref = &links->active;
	while (*ref != 0)
	{
		if ((*ref)->id == id)
		{
			found = *ref;
			break;
		}
		ref = &(*ref)->next;
	}

	assert(found != 0);
	*ref = found->next;

	found->id = noval;
	found->next = links->free;
	links->free = found;
}

void inter_link_break_generic(term_t from, term_t to)
{
	inter_links_t *links;
	if (is_short_pid(from))
	{
		proc_t *proc = scheduler_lookup(from);
		assert(proc != 0);
		links = &proc->links;
	}
	else
	{
		assert(is_short_oid(from));
		outlet_t *ol = outlet_lookup(from);
		assert(ol != 0);
		links = &ol->links;
	}

	inter_link_break(links, to);
}

void inter_links_notify(inter_links_t *links, term_t src, term_t reason)
{
	//
	// Break all links first - when scheduler_signal_exit() is called
	// all links should be consistent and point to live processes
	//
	
	plink_t *pl = links->active;
	while (pl != 0)
	{
		inter_link_break_generic(pl->id, src);
		pl = pl->next;
	}

	pl = links->active;
	while (pl != 0)
	{
		if (is_short_pid(pl->id))
		{
			proc_t *peer = scheduler_lookup(pl->id);
			assert(peer != 0);
			int x = scheduler_signal_exit_N(peer, src, reason);
			if (x < 0)
				printk("scheduler_exit_process:"
					" exit signal not delivered: pid %pt reason %pt\n", T(src), T(reason));
		}
		else
		{
			assert(is_short_oid(pl->id));
			outlet_t *ol = outlet_lookup(pl->id);
			assert(ol != 0);
			int x = outlet_signal_exit_N(ol, src, reason);
			if (x < 0)
				printk("scheduler_exit_process:"
					" exit signal not propagated: pid %pt reason %pt\n", T(src), T(reason));
		}
		pl = pl->next;
	}
}

void inter_links_done(inter_links_t *links)
{
	nfree_chain(links->nodes);
}

int proc_count_root_regs(proc_t *proc)
{
	// root regions:
	// 0: registers
	// 1: stack
	// 2: dictionary
	// 3: msg1
	// 4: msg2
	// 5: ...
	// -: timer msg1
	// -: timer msg2
	// -: ...
	//

	int nr_messages = msg_queue_len(&proc->mailbox);
	return 1 +1 +1 +1 +nr_messages +proc->pending_timers;
}

void proc_fill_root_regs(proc_t *proc, region_t *root_regs, term_t *rs, int live)
{
	// see proc_count_root_regs()
	root_regs[0].starts = rs;
	root_regs[0].ends = rs + live;
	root_regs[1].starts = proc->stop;
	root_regs[1].ends = proc_stack_bottom(proc);
	root_regs[2].starts = &proc->dictionary;
	root_regs[2].ends = &proc->dictionary +1;
	root_regs[3].starts = &proc->stack_trace;
	root_regs[3].ends = &proc->stack_trace +1;
	// Creates a root region for each message in the mailbox
	msg_queue_fill_root_regs(&proc->mailbox, root_regs +4);
	// Creates a root region for each pending timer
	int nr_messages = msg_queue_len(&proc->mailbox);
	etimer_fill_root_regs(proc, root_regs +4 +nr_messages,
										proc->pending_timers);
}

void proc_burn_fat(int gc_loc, proc_t *proc, term_t *rs, int live)
{
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs > MAX_ROOT_REGS || proc->hp.suppress_gc)
		return;
	region_t root_regs[nr_regs];
	proc_fill_root_regs(proc, root_regs, rs, live);
	heap_t *hp = &proc->hp;
	if (hp->full_sweep_after != 0 && hp->sweep_after_count >= hp->full_sweep_after)
		heap_gc_full_sweep_N(hp, root_regs, nr_regs);
	else
		gc_hook(gc_loc, proc->pid, &proc->hp, root_regs, nr_regs);
}

void proc_destroy(proc_t *proc)
{
	nfree(proc->stack_node);

	msg_queue_done(&proc->mailbox);
	heap_done(&proc->hp);

	inter_links_done(&proc->links);

	nfree(proc->home_node);
}

uint32_t proc_estimate_allocated_memory(proc_t *proc)
{
	uint32_t mem_size = 0;
	// home node
	mem_size += proc->home_node->index *PAGE_SIZE;
	if (proc->stack_node)
		mem_size += proc->stack_node->index *PAGE_SIZE;
	memnode_t *node = proc->hp.nodes;
	while (node != &proc->hp.init_node)
	{
		mem_size += node->index *PAGE_SIZE;
		node = node->next;
	}
	node = proc->mailbox.nodes;
	while (node != 0)
	{
		mem_size += node->index *PAGE_SIZE;
		node = node->next;
	}
	node = proc->links.nodes;
	while (node != 0)
	{
		mem_size += node->index *PAGE_SIZE;
		node = node->next;
	}
	return mem_size;
}

uint32_t proc_estimate_used_memory(proc_t *proc)
{
	uint32_t mem_size = 0;
	mem_size += sizeof(proc_t);
	mem_size += (proc_stack_bottom(proc) - proc_stack_top(proc)) *sizeof(uint32_t);
	memnode_t *node = proc->hp.nodes;
	while (node != 0)
	{
		mem_size += (node->ends - node->starts) *sizeof(uint32_t);
		node = node->next;
	}
	node = proc->mailbox.nodes;
	while (node != 0)
	{
		mem_size += (node->ends - node->starts) *sizeof(uint32_t);
		node = node->next;
	}
	int nlinks = -LINK_SPACE_SIZE;
	plink_t *pl = proc->links.active;
	while (pl != 0)
	{
		nlinks++;
		pl = pl->next;
	}
	if (nlinks < 0)
		nlinks = 0;
	mem_size += nlinks *sizeof(plink_t);

	return mem_size;
}

//EOF
