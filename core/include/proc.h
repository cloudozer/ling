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

//
// The home of proc_t structure
//

#include "nalloc.h"
#include "term.h"
#include "heap.h"
#include "msg_queue.h"
#include "code_base.h"

typedef struct proc_t proc_t;

#define NUM_REGS	256
#define NUM_FREGS	16

#define MB_SPACE_SIZE	16
#define LINK_SPACE_SIZE	4

#define INIT_HEAP_SIZE	512

#define SLICE_RESULT_NONE			0
#define SLICE_RESULT_YIELD			100
#define SLICE_RESULT_WAIT			101
#define SLICE_RESULT_DONE			102
#define SLICE_RESULT_EXIT			103
#define SLICE_RESULT_EXIT2			104
#define SLICE_RESULT_ERROR			105
#define SLICE_RESULT_THROW			106
#define SLICE_RESULT_OUTLET_CLOSE	111
#define SLICE_RESULT_PURGE_PROCS	666

typedef struct slice_result_t slice_result_t;
struct slice_result_t {
	int what;
	union {
		term_t reason;
		struct {
			uint64_t until_when;	// in ticks
			uint32_t *jump_to;		// points to timeout instruction
		};
		struct {
			void *closing;	// a closing outlet detected
			term_t why;		// a reason why the outlet closes
		};
		struct {
			proc_t *victim;	// a process to exit
			term_t reason2; //NB: gcc does not complain about duplicate name 'reason'
		};
	};
};

typedef struct capsule_t capsule_t;
struct capsule_t {
	uint32_t *ip;			// instruction pointer
	uint32_t *cp;			// continuation pointer
	term_t regs[NUM_REGS];	// saved registers
	int live;				// # of regs saved
	double fr[NUM_FREGS];	// saved floating-point registers
};

#define MY_QUEUE_NONE				0
#define MY_QUEUE_PENDING_TIMERS		-1
#define MY_QUEUE_HIGH				1
#define MY_QUEUE_NORMAL				2
#define MY_QUEUE_LOW				3
#define MY_QUEUE_TIMED_WAIT			4
#define MY_QUEUE_INF_WAIT			5

// process/outlet links
typedef struct plink_t plink_t;
struct plink_t {
	term_t id;	// either short pid or short oid
	plink_t *next;
};

typedef struct inter_links_t inter_links_t;
struct inter_links_t {
	plink_t init_space[LINK_SPACE_SIZE];	// a few spots to begin with
	memnode_t *nodes;
	plink_t *active;			// active links
	plink_t *free;	// free plink_t
};

struct proc_t {
	// A short pid of the process
	term_t pid;		

	// The process name if assigned
	term_t name;

	// Contains the proc_t structure itself and initial heap&stack
	memnode_t *home_node;	
	
	// The process stack
	uint32_t *init_send;	// valid if stack_node == 0
	uint32_t *stop;
	memnode_t *stack_node;

	// The process heap
	heap_t hp;

	// The process mail box
	uint32_t mb_space[MB_SPACE_SIZE];
	msg_queue_t mailbox;

	// The process dictionary
	term_t dictionary;

	// The group leader
	term_t group_leader;

	// Save a immediate terms and build tuple when requested by process_info()
	term_t init_call_mod;
	term_t init_call_func;
	int init_call_arity;

	slice_result_t result;
	term_t priority;
	int my_queue;
	capsule_t cap;

	// The total number of reductions
	uint32_t total_reds;

	// The last exception stack trace
	term_t stack_trace;

	// The current catch level
	int catch_level;

	// The number of pending timers that were created by the process and have a
	// reference into the process heap. If pending_timers > 0 the process is not
	// destroyed upon exit but rather enters a 'zombie' state. Such zombie
	// processes are destroyed when the last pending timer is fired or cancelled.
	int pending_timers;

	// The famous flag
	term_t trap_exit;

	// Process/outlet links
	inter_links_t links;

	// for exceptions originating in a BIF
	term_t bif_excep_reason;

	// needed to make (raise s,s) work
	term_t last_excep_class;
};

proc_t *proc_make(term_t leader);
void proc_stack_ensure(proc_t *proc, int needed);
void proc_stack_set_top(proc_t *proc, uint32_t *new_top);
uint32_t *proc_stack_top(proc_t *proc);
uint32_t *proc_stack_end(proc_t *proc);
uint32_t *proc_stack_bottom(proc_t *proc);
int are_inter_linked(inter_links_t *links, term_t id);
void inter_links_init(inter_links_t *links);
int inter_link_establish_N(inter_links_t *links, term_t id);
void inter_link_break(inter_links_t *links, term_t id);
void inter_link_break_generic(term_t from, term_t to);
void inter_links_notify(inter_links_t *links, term_t src, term_t reason);
void inter_links_done(inter_links_t *links);
int proc_count_root_regs(proc_t *proc);
void proc_fill_root_regs(proc_t *proc, region_t *root_regs, term_t *rs, int live);
void proc_burn_fat(int gc_loc, proc_t *proc, term_t *rs, int live);
void proc_destroy(proc_t *proc);

// BIF utilities
int proc_spawn_N(proc_t *new_proc, term_t m, term_t f, term_t args);
int proc_spawn_fun0_N(proc_t *new_proc, t_fun_t *f);
term_t proc_set_flag(proc_t *proc, term_t flag, term_t val);
uint32_t proc_estimate_allocated_memory(proc_t *proc);
uint32_t proc_estimate_used_memory(proc_t *proc);

int proc_references_module(proc_t *proc, module_info_t *mi);

//EOF
