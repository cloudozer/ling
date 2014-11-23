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

#include <stdint.h>
#include <math.h>

#include "ling_common.h"

#include "os.h"
#include "heap.h"
#include "proc.h"
#include "code_base.h"
#include "scheduler.h"
#include "term.h"
#include "bignum.h"
#include "mixed.h"
#include "string.h"
#include "atoms.h"
#include "atom_defs.h"
#include "bif.h"
#include "catch_tab.h"
#include "stringify.h"
#include "stdlib.h"
#include "msg_queue.h"
#include "bits.h"
#include "cluster.h"
#include "getput.h"
#include "time.h"
#include "unicode.h"
#include "term_util.h"
#include "list_util.h"
#include "outlet.h"
#include "timers.h"
#include "hash.h"

// needed by purge_module
extern hash_t *registry;

#ifdef TRACE_HARNESS
uint32_t trace_mask = 0; // TRACE_MASK_EXCEPTION;
term_t trace_module = noval;
#endif

// tunable
#define BS_INIT_FAIL_GC_THRESH	8000

#define DELIVER_SIGNALS 	1

int max_backtrace_depth = 8;	// 8 is in exception_SUITE.erl

// Pointers to the code implementing special instructions; needed for stack
// tracing.
void *func_info_label = 0;
void *int_code_end_label = 0;

static opcode_info_t *ling_opcodes = 0;

typedef struct pair_t pair_t;
struct pair_t {
	uint32_t f;
	uint32_t s;
};

typedef struct mfa_t mfa_t;
struct mfa_t {
	term_t mod;
	term_t fun;
	int arity;
};

static int select_val_atoms_compare(const void *a, const void *b);

static term_t get_stack_trace(uint32_t *ip, uint32_t *cp,
			term_t *sp, term_t *sbot, mfa_t *first_mfa, term_t args0, heap_t *hp);

static term_t invoke_bif(export_t *exp, proc_t *proc, term_t *rs, int live);

static int send_to_outlet(outlet_t *outlet, term_t what, proc_t *proc);

void proc_main(proc_t *proc)
{
	// current instruction pointer
	register uint32_t *ip = 0;

#define next() do { \
   	goto *ip[0]; \
} while (0)

	// heap pointer
	register uint32_t *htop = 0;
	uint32_t *hend = 0;

	// continuation pointer
	uint32_t *cp = 0;

	// stack pointers
	term_t *sp = 0;
	term_t *send;

	// registers
	register term_t r0 = noval;
	term_t rs[NUM_REGS];

	// temporary values
	term_t tmp_arg1 = noval;
	term_t tmp_arg2 = noval;

	// binary put context (uninitialized)
	bits_t bpc;

	// floating-point registers
	double fr[NUM_FREGS];

	int reds_left = 0;

	// must be filled in before jumping to raise_from_(arith_)bif
	mfa_t raise_bif_mfa;

#define swap_out() do { \
	rs[0] = r0; \
	heap_set_top0(&proc->hp, htop); \
	proc_stack_set_top(proc, sp); \
} while (0)

#define swap_in() do { \
	hend = heap_end(&proc->hp); \
	htop = heap_top(&proc->hp); \
	r0 = rs[0]; \
} while (0)

#define light_swap_out() do { \
	heap_set_top0(&proc->hp, htop); \
} while (0)

#define light_swap_in() do { \
	hend = heap_end(&proc->hp); \
	htop = heap_top(&proc->hp); \
} while (0)

//
// -4: &&func_info_0
// -3: module
// -2: function
// -1: arity
// ip is here
//
#define local_reduce() do { \
	reds_left--; \
	if (unlikely(reds_left <= 0)) { \
		assert(ip[-4] == shrink_ptr(&&func_info_0)); \
		proc->cap.live = ip[-1]; \
		light_swap_out(); \
		goto yield; \
	} \
	next(); \
} while (0)

#define badarg() do { \
	goto badarg; \
} while (0)

#define bif_error1(reason, mod__, fun__, arg) do { \
	raise_bif_mfa.mod = (mod__); \
	raise_bif_mfa.fun = (fun__); \
	raise_bif_mfa.arity = 1; \
	rs[0] = (arg); \
	rs[1] = (reason); \
	goto raise_from_arith_bif; \
} while (0)

#define bif_error2(reason, mod__, fun__, arg1, arg2)	do { \
	raise_bif_mfa.mod = (mod__); \
	raise_bif_mfa.fun = (fun__); \
	raise_bif_mfa.arity = 2; \
	rs[0] = (arg1); \
	rs[1] = (arg2); \
	rs[2] = (reason); \
	goto raise_from_arith_bif; \
} while (0)

#define raise_error(reason) do { \
	light_swap_out(); \
	rs[1] = A_ERROR; \
	rs[2] = (reason); \
	proc->stack_trace = noval; \
	goto exception; \
} while (0)

	if (proc == 0)
		goto initialize;	// initialization moved to the end of the function
	
	assert(*proc->cap.cp == shrink_ptr(&&int_code_end_0));
	if (nalloc_no_memory())
	{
		// variables (other than proc) are not valid - do not touch
		proc->result.what = SLICE_RESULT_ERROR;
		proc->result.reason = A_NO_MEMORY;

		// proc is swapped out (light)
		proc = scheduler_next(proc, 0);
		goto init_done;
	}

	// We are ready to execute the first iop - report how much time we have
	// spent warming up.
	//uint64_t startup_latency_us = (wall_clock() - start_of_day_wall_clock) /1000;
	//printk("Started in %lu us\n", startup_latency_us);

	// Start counting runtime
	scheduler_runtime_start();

	goto init_done;

yield:
	proc->result.what = SLICE_RESULT_YIELD;

schedule:

	// proc->cap.live must be set before jumping here
	// must be swapped out (light)
	
	proc->cap.ip = ip;
	proc->cap.cp = cp;

	proc_stack_set_top(proc, sp);

	if (proc->cap.live > 0)
	{
		rs[0] = r0;
		memcpy(proc->cap.regs, rs, proc->cap.live*sizeof(term_t));
	}
	memcpy(proc->cap.fr, fr, NUM_FREGS*sizeof(double));

	if (proc->result.what == SLICE_RESULT_YIELD)
		proc_burn_fat(proc, 0, proc->cap.regs, proc->cap.live);

	proc = scheduler_next(proc, reds_left);

init_done:
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	htop = heap_top(&proc->hp);
	hend = heap_end(&proc->hp);

	ip = proc->cap.ip;
	cp = proc->cap.cp;

	if (proc->cap.live > 0)
	{
		memcpy(rs, proc->cap.regs, proc->cap.live*sizeof(term_t));
		r0 = rs[0];
	}
	memcpy(fr, proc->cap.fr, NUM_FREGS*sizeof(double));

#ifdef TRACE_HARNESS
if (unlikely(trace_mask & TRACE_MASK_CALL))
{
	uint32_t *fi = backstep_to_func_info(ip);
	int disp = ip-fi;
	if (fi != 0)
	{
		if (trace_module == noval || fi[1] == trace_module)
		{
			if (disp == 4)
			{
				printk("TRACE: %pt:%pt(", T(fi[1]), T(fi[2]));
				int arity = fi[3];
				for (int i = 0; i < arity; i++)
					printk("%pt%s", T(rs[i]), (i < arity-1) ?", " :"");
				printk(")\n");
			}
			else
				printk("TRACE: %pt:%pt/%d +%d\n", T(fi[1]), T(fi[2]), fi[3], disp);
		}
	}

	reds_left = 1;
}
else
	reds_left = SLICE_REDUCTIONS;
#else /*TRACE_HARNESS*/
	reds_left = SLICE_REDUCTIONS;
#endif

	next();

get_tuple_element_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[0].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
//assert(is_tuple(r0));
term_t elem = peel_tuple(r0)[((ip[1] >> 0) & 255)+1];	// Pos is 0-based
{
	int reg__ = ((ip[1] >> 8) & 255);
	if (reg__ == 0)
		r0 = elem;
	else
		rs[reg__] = elem;
}



ip += 2;
goto *next;
}


move_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
rs[1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


get_tuple_element_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[2].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
//assert(is_tuple(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)])));
term_t elem = peel_tuple(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]))[((ip[1] >> 8) & 255)+1];	// Pos is 0-based
{
	int reg__ = ((ip[1] >> 16) & 255);
	if (reg__ == 0)
		r0 = elem;
	else
		rs[reg__] = elem;
}



ip += 2;
goto *next;
}


move_3: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[3].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[((ip[1] >> 8) & 255)+1] = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);


ip += 2;
goto *next;
}


is_tuple_of_arity_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[4].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t src = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
if (!is_tuple(src) || *peel_tuple(src) != ((ip[2] >> 8) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
tmp_arg1 = shrink_ptr(peel_tuple(src)+1);	// prepare for extract_element


ip += 3;
goto *next;
}


l_new_bs_put_integer_imm_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[5].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
uint32_t bsz = ((ip[3] >> 0) & 255);
assert(bpc.ends-bpc.starts >= bsz);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
uint8_t flags = ((ip[3] >> 8) & 255);
if (is_int(v))
{
	light_swap_out();
	bits_put_integer(&bpc, int_value(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else if (is_boxed(v) && is_bignum(peel_boxed(v)))
{
	light_swap_out();
	bits_put_bignum(&bpc, (bignum_t *)peel_boxed(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 4;
goto *next;
}


move_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[6].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


l_call_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[7].counter++;
#endif


cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

move2_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[8].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[((ip[1] >> 8) & 255)+1] = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
sp[((ip[1] >> 24) & 255)+1] = ((((ip[1] >> 16) & 255) == 0) ?r0 :rs[((ip[1] >> 16) & 255)]);


ip += 2;
goto *next;
}


test_heap_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[9].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (unlikely(hend - htop < ((ip[1] >> 0) & 255)))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, ((ip[1] >> 8) & 255));
		heap_ensure(&proc->hp, ((ip[1] >> 0) & 255), root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, ((ip[1] >> 0) & 255));
	swap_in();
}




ip += 2;
goto *next;
}


move_2: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[10].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r0;
		else
			rs[reg__] = r0;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r0;
	}
}



ip += 2;
goto *next;
}


l_put_tuple_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[11].counter++;
#endif


uint32_t arity = ((ip[1] >> 8) & 255);
term_t tuple;
if (unlikely(arity == 0))
	tuple = ZERO_TUPLE;
else
{
	tuple = tag_tuple(htop);
	*htop++ = arity;

	term_t *pe = (term_t *)ip + 2;
	int n = arity;
	while (n-- > 0)
	{
		if (is_reg(*pe))
		{
			int ri = reg_index(*pe);
			if (ri == 0)
				*htop++ = r0;
			else
				*htop++ = rs[ri];
		}
		else if (is_slot(*pe))
		{
			int si = slot_index(*pe);
			*htop++ = sp[si+1];
		}
		else
			*htop++ = *pe;

		pe++;
	}
}
{
	int reg__ = ((ip[1] >> 0) & 255);
	if (reg__ == 0)
		r0 = tuple;
	else
		rs[reg__] = tuple;
}


ip += arity+2;
next();
}

move2_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[12].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = sp[((ip[2] >> 0) & 255)+1];
	else
		rs[reg__] = sp[((ip[2] >> 0) & 255)+1];
}

{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = sp[((ip[2] >> 16) & 255)+1];
		else
			rs[reg__] = sp[((ip[2] >> 16) & 255)+1];
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = sp[((ip[2] >> 16) & 255)+1];
	}
}



ip += 3;
goto *next;
}


move2_3: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[13].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
	else
		rs[reg__] = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
}

{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = ((((ip[2] >> 16) & 255) == 0) ?r0 :rs[((ip[2] >> 16) & 255)]);
		else
			rs[reg__] = ((((ip[2] >> 16) & 255) == 0) ?r0 :rs[((ip[2] >> 16) & 255)]);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = ((((ip[2] >> 16) & 255) == 0) ?r0 :rs[((ip[2] >> 16) & 255)]);
	}
}



ip += 3;
goto *next;
}


l_call_only_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[14].counter++;
#endif


ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

get_list_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[15].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
assert(is_cons(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)])));
term_t *pair = peel_cons(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]));
{
	int reg__ = ((ip[1] >> 8) & 255);
	if (reg__ == 0)
		r0 = pair[0];
	else
		rs[reg__] = pair[0];
}

{
	int reg__ = ((ip[1] >> 16) & 255);
	if (reg__ == 0)
		r0 = pair[1];
	else
		rs[reg__] = pair[1];
}



ip += 2;
goto *next;
}


l_fetch_2: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[16].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
tmp_arg2 = ((((ip[1] >> 8) & 255) == 0) ?r0 :rs[((ip[1] >> 8) & 255)]);


ip += 2;
goto *next;
}


is_nonempty_list_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[17].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(r0))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_allocate_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[18].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 0+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 1;
goto *next;
}


l_is_eq_exact_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[19].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (tmp_arg1 != tmp_arg2)
{
	if (are_both_immed(tmp_arg1, tmp_arg2))
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
	if (!are_terms_equal(tmp_arg1, tmp_arg2, 1))
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


move_4: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[20].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
rs[2] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


is_nonempty_list_allocate_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[21].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (!is_cons(((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)])))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

int gap = ((ip[2] >> 8) & 255)+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 3;
goto *next;
}


l_select_val2_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[22].counter++;
#endif


term_t v = r0;
if (v == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == (term_t)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[5]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

l_gc_bif1_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[23].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t t = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
swap_out();
term_t r = ((gc_bif_func1_t)(bif_func_t)expand_ptr(ip[2]))
					(t, proc, rs, ((ip[4] >> 0) & 255));
swap_in();
if (r == noval)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_func_t entry = (bif_func_t)expand_ptr(ip[2]);
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);
	assert(bif_exp->arity == 1);
	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = 1;
	rs[0] = t;
	rs[1] = proc->bif_excep_reason;
	goto raise_from_bif;
}
{
	int reg__ = ((ip[4] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 5;
goto *next;
}


l_is_eq_exact_immed_2: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[24].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[3] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


deallocate_return_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[25].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 0+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

get_list_3: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[26].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
assert(is_cons((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1])));
term_t *pair = peel_cons((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]));
{
	int reg__ = ((ip[2] >> 0) & 255);
	if (reg__ == 0)
		r0 = pair[0];
	else
		rs[reg__] = pair[0];
}

sp[((ip[2] >> 8) & 255)+1] = pair[1];


ip += 3;
goto *next;
}


l_allocate_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[27].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 1+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 1;
goto *next;
}


is_nonempty_list_2: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[28].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[1]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_is_eq_exact_immed_3: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[29].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[2] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_is_eq_exact_immed_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[30].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[1] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_110: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[31].counter++;
#endif


cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_tuple_of_arity_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[32].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t src = r0;
if (!is_tuple(src) || *peel_tuple(src) != 2)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
tmp_arg1 = shrink_ptr(peel_tuple(src)+1);	// prepare for extract_element


ip += 2;
goto *next;
}


put_list_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[33].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
r0 = tag_cons(htop);
htop += 2;


ip += 3;
goto *next;
}


l_call_fun_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[34].counter++;
#endif


uint32_t *saved_ip = ip +0 +1;

 
rs[0] = r0;
uint32_t arity = 1;
term_t t = rs[arity];		//what about {x,1023} mapped to {x,255}?
if (!is_boxed(t))
	goto l_call_fun_0_bad_fun;
uint32_t *p = peel_boxed(t);
if (boxed_tag(p) == SUBTAG_FUN)
{
	t_fun_t *fun = (t_fun_t *)p;
	if (fun->fe == 0)
		fatal_error("unloaded funs not implemented");
	int num_free = fun_num_free(p);
	if (fun_arity(p) != arity+num_free)
		goto l_call_fun_0_bad_arity;
	ip = fun->fe->entry;
	term_t *src = fun->frozen;
	term_t *dst = rs + arity;
	int n = num_free;
	while (n-- > 0)
		*dst++ = *src++;
	r0 = rs[0];
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->is_bif)
	{
		swap_out();
		term_t r = invoke_bif(exp->e, proc, rs, arity +1);		// +1 for export
		swap_in();
		if (r == noval)
		{
			raise_bif_mfa.mod = exp->e->module;
			raise_bif_mfa.fun = exp->e->function;
			raise_bif_mfa.arity = exp->e->arity;
			rs[exp->e->arity] = proc->bif_excep_reason;
			goto raise_from_bif;
		}
		r0 = r;
		ip = saved_ip;


		next();
	}
	if (exp->e->entry == 0)
	{
		//rs[0] = r0;
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		light_swap_in();
		r0 = exp->e->module;
		rs[1] = exp->e->function;
		rs[2] = args;
		ip = (EH_UNDEF_EXP)->entry;
	}
	else if (exp->e->arity != arity)
	{
l_call_fun_0_bad_arity:
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		term_t fun_args = heap_tuple2(&proc->hp, t, args);
		term_t reason = heap_tuple2(&proc->hp, A_BADARITY, fun_args);
		light_swap_in();
		raise_error(reason);
	}
	else
		ip = exp->e->entry;
}
else
{
l_call_fun_0_bad_fun:
	light_swap_out();
	term_t reason = heap_tuple2(&proc->hp, A_BADFUN, t);
	light_swap_in();
	raise_error(reason);
}
cp = saved_ip;
local_reduce();


}



l_bs_add_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[35].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
uint32_t bcount1, bcount2;
int x = bits_calc_bit_size(tmp_arg1, 1, &bcount1);
int y = bits_calc_bit_size(tmp_arg2, 1, &bcount2);
if (x == 0 && y == 0)
{
	int64_t bcount = (int64_t)bcount1 + bcount2;
	if (bcount > MAX_BIT_SIZE)
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		raise_error(A_SYSTEM_LIMIT);
	}
	light_swap_out();
	term_t v = int_to_term(bcount, &proc->hp);
	light_swap_in();
	{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = v;
		else
			rs[reg__] = v;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = v;
	}
}

}
else if (x == -BAD_ARG || y == -BAD_ARG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
else
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	raise_error(A_SYSTEM_LIMIT);
}


ip += 3;
goto *next;
}


l_bs_get_integer_small_imm_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[36].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ((ip[3] >> 0) & 255);
assert(bcount <= 32 -3);	//fits_int()
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
uint32_t slack = 32-bcount;
uint8_t flags = ((ip[3] >> 8) & 255);
uint8_t buf[4] = {0, 0, 0, 0};
int v;
if (flags & BSF_LITTLE)
{
	bits_t bs = {
		.data = buf,
		.starts = 0,
		.ends = bcount
	};
	bits_fill(&mc->bs, &bs);
	uint32_t v0 = GET_UINT_32_LE(buf);
	int bo = slack & 7;
	v = (flags & BSF_SIGNED)
		?(int)(v0 << slack) >> (slack +bo)
		:v0 >> bo;
}
else
{
	bits_t bs = {
		.data = buf,
		.starts = slack,
		.ends = 32
	};
	bits_fill(&mc->bs, &bs);
	uint32_t v0 = GET_UINT_32(buf);
	v = (flags & BSF_SIGNED)
		?((int)(v0 << slack)) >> slack
		:v0;
}
{
	int reg__ = ((ip[3] >> 16) & 255);
	if (reg__ == 0)
		r0 = tag_int(v);
	else
		rs[reg__] = tag_int(v);
}



ip += 4;
goto *next;
}


l_fetch_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[37].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = r0;
tmp_arg2 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


l_is_eq_exact_immed_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[38].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (r0 != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


call_bif_8: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[39].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_plusplus2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


move_return_3: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[40].counter++;
#endif


r0 = rs[2];
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_is_eq_exact_immed_6: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[41].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[5] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


deallocate_return_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[42].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 1+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

put_list_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[43].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
rs[1] = tag_cons(htop);
htop += 2;


ip += 3;
goto *next;
}


return_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[44].counter++;
#endif


ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

extract_next_element_2: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[45].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


is_nil_2: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[46].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[1] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


extract_next_element_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[47].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[3] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_bs_start_match2_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[48].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (!is_boxed(bin) || !is_binary(peel_boxed(bin)))
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
uint32_t *bdata = peel_boxed(bin);
int num_slots = ((ip[3] >> 8) & 255) +1;	//NB: +1
if (boxed_tag(bdata) != SUBTAG_MATCH_CTX
			|| match_ctx_num_slots(bdata) < num_slots)
{
	//NB: Live unused, burn some fat?
	light_swap_out();
	bin = bits_bs_start_match2(bin, num_slots, &proc->hp);
	light_swap_in();
}
else
{
	// reused matching context - offset should be saved anyway
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
	mc->saved_offsets[0] = mc->bs.starts;
}
{
	int reg__ = ((ip[3] >> 16) & 255);
	if (reg__ == 0)
		r0 = bin;
	else
		rs[reg__] = bin;
}



ip += 4;
goto *next;
}


l_is_eq_exact_immed_5: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[49].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[4] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


is_tuple_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[50].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_tuple(r0))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_trim_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[51].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t masqueraded_cp = sp[0];
sp += 1;
sp[0] = masqueraded_cp;


ip += 1;
goto *next;
}


extract_next_element2_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[52].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 3;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_bs_get_integer_16_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[53].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends - mc->bs.starts < 16)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t vh, vl;
bits_get_octet(&mc->bs, vh);
bits_get_octet(&mc->bs, vl);
uint32_t v = (vh << 8) | vl;
{
	int reg__ = ((ip[2] >> 0) & 255);
	if (reg__ == 0)
		r0 = tag_int(v);
	else
		rs[reg__] = tag_int(v);
}



ip += 3;
goto *next;
}


deallocate_return_2: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[54].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 2+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_call_last_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[55].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 2+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

extract_next_element_4: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[56].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[5] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_fetch_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[57].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
tmp_arg2 = r0;


ip += 2;
goto *next;
}


l_select_val_atoms_0: ATTRIBUTE_HOT
 {

// NB: self-modifying code: for dynamically loaded modules
// l_select_val_atoms iop points to &&l_select_val_atoms_N;
// after sorting the list it changes to
// &&l_select_val_atoms_N_sorted; statically loaded modules
// use the second label as the list is already in order.

	uint32_t nr_pairs = (((ip[2] >> 8) & 255))/2;
	qsort(ip + 3,
		nr_pairs, 2*sizeof(uint32_t), select_val_atoms_compare);
	
	*ip = shrink_ptr(&&l_select_val_atoms_0_sorted);
}

l_select_val_atoms_0_sorted: {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[58].counter++;
#endif



term_t v = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
pair_t *alpha = (pair_t *)(ip + 3);
pair_t *beta = (pair_t *)((uint32_t *)alpha + ((ip[2] >> 8) & 255));

if (v < alpha->f || v > beta[-1].f)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

//TODO: in BEAM the binary search parameters are
// made into unsigned values for performance

while (beta > alpha+1)	// at least 2 pairs
{
	pair_t *mid = alpha + (beta - alpha +1)/2;
	if ((term_t)mid->f > v)
		beta = mid;
	else
		alpha = mid;
}

assert(beta == alpha+1);
if (alpha->f == v)
{
	ip = (uint32_t *)expand_ptr(alpha->s);
	next();
}

do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


}

put_list_2: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[59].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
rs[2] = tag_cons(htop);
htop += 2;


ip += 3;
goto *next;
}


move_return_5: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[60].counter++;
#endif


r0 = A_FALSE;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

call_bif_47: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[61].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
bif_func_t entry = (bif_func_t)expand_ptr(ip[1]);

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 2;
goto *next;
}


l_new_bs_put_binary_all_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[62].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
// Fail is unused
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && is_binary(peel_boxed(bin)));

bits_t bs;
bits_get_real(peel_boxed(bin), &bs);
bits_copy(&bs, &bpc);


ip += 3;
goto *next;
}


is_nil_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[63].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[2] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_bif2_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[64].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r = ((bif_func2_t)bif_element2)(tmp_arg1, tmp_arg2, proc);
if (r == noval)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 3;
goto *next;
}


l_allocate_2: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[65].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 2+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 1;
goto *next;
}


l_is_eq_exact_immed_7: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[66].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[6] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


move_deallocate_return_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[67].counter++;
#endif


r0 = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
cp = demasquerade_pointer(sp[0]);
sp += ((ip[1] >> 8) & 255)+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[68].counter++;
#endif


r0 = rs[1];
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

call_bif_7: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[69].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_setelement3;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


move_5: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[70].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
rs[3] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


l_fetch_18: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[71].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
tmp_arg2 = rs[3];


ip += 2;
goto *next;
}


call_bif_16: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[72].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_list_to_atom1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


call_bif_15: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[73].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_integer_to_list1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


call_bif_14: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[74].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_lookup2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_increment_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[75].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
int i = (int)((ip[2] >> 8) & 255);
term_t src = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
term_t r;
if (is_int(src))
{
	int v = int_value(src) + i;
	if (likely(fits_int(v)))
		r = tag_int(v);
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		r = tag_boxed(bn);
	}
}
else
{
	light_swap_out();
	r = mixed_add_immed(src, i, &proc->hp);
	light_swap_in();
	if (is_atom(r))
		bif_error2(r, A_ERLANG, APLUS__, src, tag_int(i));
}
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 3;
goto *next;
}


l_bs_init_heap_bin_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[76].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
int osz = ((ip[2] >> 0) & 255);
int needed = WSIZE(t_heap_bin_t) + (osz +3)/4 +
			 WSIZE(t_sub_bin_t) + ((ip[2] >> 8) & 255);
light_swap_out();
uint32_t *p = heap_alloc(&proc->hp, needed);
t_heap_bin_t *hb = (t_heap_bin_t *)p;
box_heap_bin(p, osz, 0);
heap_set_top0(&proc->hp, p);
light_swap_in();
bits_init_buf(hb->data, osz, &bpc);
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(hb);
		else
			rs[reg__] = tag_boxed(hb);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(hb);
	}
}



ip += 3;
goto *next;
}


l_select_tuple_arity_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[77].counter++;
#endif


term_t tuple = r0;
if (!is_tuple(tuple))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t v = *peel_tuple(tuple);
pair_t *alpha = (pair_t *)(ip + 3);
pair_t *beta = (pair_t *)((uint32_t *)alpha + ip[2]);

if (v < alpha->f || v > beta[-1].f)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

//TODO: in BEAM the binary search parameters are
// made into unsigned values for performance

while (beta > alpha+1)	// at least 2 pairs
{
	pair_t *mid = alpha + (beta - alpha +1)/2;
	if ((term_t)mid->f > v)
		beta = mid;
	else
		alpha = mid;
}

assert(beta == alpha+1);
if (alpha->f == v)
{
	ip = (uint32_t *)expand_ptr(alpha->s);
	next();
}

do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


}

put_list_3: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[78].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
rs[3] = tag_cons(htop);
htop += 2;


ip += 3;
goto *next;
}


l_select_tuple_arity2_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[79].counter++;
#endif


term_t v = r0;
if (!is_tuple(v))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
int arity = *peel_tuple(v);
if (arity == ((ip[4] >> 0) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
if (arity == ((ip[4] >> 8) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

move_return_4: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[80].counter++;
#endif


r0 = nil;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_fetch_4: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[81].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
tmp_arg2 = sp[((ip[1] >> 8) & 255)+1];


ip += 2;
goto *next;
}


l_allocate_6: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[82].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 6+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 1;
goto *next;
}


l_call_last_6: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[83].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 6+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

init2_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[84].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[((ip[1] >> 8) & 255)+1] = sp[((ip[1] >> 0) & 255)+1] = nil;


ip += 2;
goto *next;
}


call_bif_9: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[85].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_member2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_move_call_36: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[86].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 3;
ip = (uint32_t *)expand_ptr(ip[2]);
local_reduce();
}

l_new_bs_put_binary_imm_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[87].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
uint32_t bcount = ((ip[2] >> 0) & 255);
//NB: Flags not used
term_t bin = ((((ip[2] >> 16) & 255) == 0) ?r0 :rs[((ip[2] >> 16) & 255)]);
if (!is_boxed(bin) || !is_binary(peel_boxed(bin)))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

bits_t bs;
bits_get_real(peel_boxed(bin), &bs);
if (bs.ends - bs.starts < bcount)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
assert(bpc.ends - bpc.starts >= bcount);
bs.ends = bs.starts +bcount;
bits_copy(&bs, &bpc);


ip += 3;
goto *next;
}


l_make_fun_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[88].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
fun_entry_t *fe = (fun_entry_t *)expand_ptr(ip[1]);

light_swap_out();
int needed = WSIZE(t_fun_t) + fe->num_free;
uint32_t *p = heap_alloc(&proc->hp, needed);
term_t fun = tag_boxed(p);
rs[0] = r0;
box_fun(p,
	fe->num_free,
	fe->arity,
	proc->pid,
	fe->module,
	fe->index,
	fe->uniq,
	fe->old_index,
	fe->old_uniq,
	fe,
	rs);
heap_set_top(&proc->hp, p);
light_swap_in();
r0 = fun;


ip += 2;
goto *next;
}


is_nonempty_list_1: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[89].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[2]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_call_ext_6: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[90].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+506);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_last_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[91].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 1+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_is_ge_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[92].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	if ((int)tmp_arg1 < (int)tmp_arg2)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}
else
{
	if (is_term_smaller(tmp_arg1, tmp_arg2))
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


extract_next_element2_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[93].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 1;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


move_return_48: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[94].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_allocate_3: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[95].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 3+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 1;
goto *next;
}


jump_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[96].counter++;
#endif


do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

l_fetch_10: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[97].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = rs[1];
tmp_arg2 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


is_atom_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[98].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_atom(r0))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_bs_get_binary_imm2_0: ATTRIBUTE_HOT
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[99].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ((ip[3] >> 8) & 255);
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

light_swap_out();
int needed = WSIZE(t_sub_bin_t);
uint32_t *p = heap_alloc(&proc->hp, needed);
t_sub_bin_t *sb = (t_sub_bin_t *)p;
box_sub_bin(p, mc->parent, mc->bs.starts, mc->bs.starts+bcount, 0);
heap_set_top(&proc->hp, p);
light_swap_in();

mc->bs.starts += bcount;
{
	int reg__ = ((ip[3] >> 24) & 255);
	if (reg__ == 0)
		r0 = tag_boxed(sb);
	else
		rs[reg__] = tag_boxed(sb);
}



ip += 4;
goto *next;
}


extract_next_element2_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[100].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 2;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


is_tuple_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[101].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_tuple(rs[2]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


test_arity_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[102].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t src = r0;
assert(is_tuple(src));
if (*peel_tuple(src) != 2)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
tmp_arg1 = shrink_ptr(peel_tuple(src)+1);	// prepare for extract_element


ip += 2;
goto *next;
}


l_move_call_only_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[103].counter++;
#endif


r0 = rs[4];
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_catch_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[104].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[5+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
proc->catch_level++;


ip += 2;
goto *next;
}


l_bs_append_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[105].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
swap_out();
int live = ((ip[3] >> 8) & 255);
rs[live] = tmp_arg1;
rs[live+1] = tmp_arg2;
proc_burn_fat(proc, 0, rs, live +2);
tmp_arg2 = rs[live +1];
tmp_arg1 = rs[live];

// tmp_arg1 - bit size in units
// tmp_arg2 - binary
term_t bin = bits_bs_append(tmp_arg2,
		tmp_arg1, ((ip[3] >> 16) & 255), ((ip[3] >> 0) & 255), &bpc, &proc->hp);
assert(bpc.ends >= bpc.starts);
swap_in();
if (bin == A_SYSTEM_LIMIT || bin == A_BADARG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	raise_error(bin);
}
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = bin;
		else
			rs[reg__] = bin;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = bin;
	}
}



ip += 4;
goto *next;
}


l_select_tuple_arity2_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[106].counter++;
#endif


term_t v = ((((ip[4] >> 0) & 255) == 0) ?r0 :rs[((ip[4] >> 0) & 255)]);
if (!is_tuple(v))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
int arity = *peel_tuple(v);
if (arity == ((ip[4] >> 8) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
if (arity == ((ip[4] >> 16) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

extract_next_element_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[107].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


call_bif_41: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[108].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_update_counter3;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_bs_init_fail_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[109].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
// tmp_arg1 is the byte size of the binary

uint32_t osz;
int x = bits_calc_byte_size(tmp_arg1, &osz);
if (x == -TOO_LONG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	raise_error(A_SYSTEM_LIMIT);
}
if (x < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

if (osz >= BS_INIT_FAIL_GC_THRESH)
{
	swap_out();
	proc_burn_fat(proc, 0, rs, ((ip[2] >> 8) & 255));
	swap_in();
}

binnode_t *node = binnode_make(osz);
int needed = WSIZE(t_proc_bin_t) + WSIZE(t_sub_bin_t) + ((ip[2] >> 0) & 255);
light_swap_out();
uint32_t *p = heap_alloc_N(&proc->hp, needed);
if (p == 0)
{
	binnode_destroy(node);
	no_memory_signal();
}
t_proc_bin_t *pb = (t_proc_bin_t *)p;
box_proc_bin(p, osz, node);
heap_set_top0(&proc->hp, p);
proc_bin_link(&proc->hp.proc_bins, pb, &proc->hp.total_pb_size);
light_swap_in();
bits_get_real(pb, &bpc);
{
	int reg__ = ((ip[2] >> 16) & 255);
	if (reg__ == 0)
		r0 = tag_boxed(pb);
	else
		rs[reg__] = tag_boxed(pb);
}



ip += 3;
goto *next;
}


l_is_function2_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[110].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
uint32_t arity = ((ip[3] >> 0) & 255);
if (!is_boxed(v))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t *p = peel_boxed(v);
if (boxed_tag(p) == SUBTAG_FUN)
{
	if (fun_arity(p) != arity+fun_num_free(p))
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->arity != arity)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}
else
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 4;
goto *next;
}


l_allocate_zero_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[111].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 4+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = 4;
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 1;
goto *next;
}


l_loop_rec_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[112].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t cur = msg_queue_current(&proc->mailbox);
if (cur == noval)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
r0 = cur;


ip += 2;
goto *next;
}


l_select_tuple_arity_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[113].counter++;
#endif


term_t tuple = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
if (!is_tuple(tuple))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t v = *peel_tuple(tuple);
pair_t *alpha = (pair_t *)(ip + 3);
pair_t *beta = (pair_t *)((uint32_t *)alpha + ((ip[2] >> 8) & 255));

if (v < alpha->f || v > beta[-1].f)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

//TODO: in BEAM the binary search parameters are
// made into unsigned values for performance

while (beta > alpha+1)	// at least 2 pairs
{
	pair_t *mid = alpha + (beta - alpha +1)/2;
	if ((term_t)mid->f > v)
		beta = mid;
	else
		alpha = mid;
}

assert(beta == alpha+1);
if (alpha->f == v)
{
	ip = (uint32_t *)expand_ptr(alpha->s);
	next();
}

do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


}

is_nil_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[114].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (r0 != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


move_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[115].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
rs[4] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


extract_next_element3_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[116].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 1;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


remove_message_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[117].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
msg_queue_drop(&proc->mailbox);

// The message queue must be reset as we may enter a different, potentially,
// nested receive statement.
//
// Also 'timeout' below.

msg_queue_reset(&proc->mailbox);


ip += 1;
goto *next;
}


l_move_call_last_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[118].counter++;
#endif


r0 = ((((ip[2] >> 8) & 255) == 0) ?r0 :rs[((ip[2] >> 8) & 255)]);
cp = demasquerade_pointer(sp[0]);
sp += ((ip[2] >> 0) & 255)+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

allocate_init_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[119].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
int gap = ((ip[1] >> 0) & 255)+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


sp[((ip[1] >> 8) & 255)+1] = nil;


ip += 2;
goto *next;
}


move_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[120].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
rs[5] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


l_allocate_zero_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[121].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 3+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = 3;
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 1;
goto *next;
}


l_allocate_zero_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[122].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 1+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = 1;
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 1;
goto *next;
}


move_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[123].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
rs[7] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


is_nil_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[124].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[4] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


catch_end_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[125].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[5+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	assert(rs[1] == A_ERROR || rs[1] == A_THROW || rs[1] == A_EXIT);
	if (rs[1] == A_THROW)
		r0 = rs[2];
	else
	{
		// A very nice spot for a garbage collection:
		// a few live registers and a portion of the
		// stack just gone.
		//
		
		swap_out();
		proc_burn_fat(proc, 0, rs, 3);

		// Both error and exit exceptions are converted to {'EXIT',Reason}
	
		if (rs[1] == A_ERROR)
			rs[2] = heap_tuple2(&proc->hp, rs[2], proc->stack_trace);
		rs[0] = heap_tuple2(&proc->hp, AEXIT__, rs[2]);
		swap_in();
	}
}




ip += 1;
goto *next;
}


is_atom_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[126].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_atom(rs[1]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


init_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[127].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[2] = nil;


ip += 1;
goto *next;
}


l_bif1_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[128].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t r = ((bif_func1_t)(bif_func_t)expand_ptr(ip[2]))(((((ip[3] >> 0) & 255) == 0) ?r0 :rs[((ip[3] >> 0) & 255)]), proc);
if (r == noval)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
{
	int reg__ = ((ip[3] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 4;
goto *next;
}


init_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[129].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[1] = nil;


ip += 1;
goto *next;
}


l_select_val2_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[130].counter++;
#endif


term_t v = rs[6];
if (v == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == (term_t)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[5]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

apply_last_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[131].counter++;
#endif



//
// NB: BEAM uses a function call for this, not a macro
//

int arity = ((ip[1] >> 0) & 255);
term_t m = (arity == 0) ?r0 :rs[arity];
term_t f = rs[arity+1];

term_t this = noval;	// abstract module?
if (!is_atom(f))
	badarg();
if (!is_atom(m))
{
	if (!is_tuple(m))
		badarg();
	uint32_t *p = peel_tuple(m);
	if (p[0] < 1)
		badarg();
	this = m;
	m = p[1];
	if (!is_atom(m))
		badarg();
}

if (this != noval)
{
	if (arity == 0)
		r0 = this;
	else
		rs[arity] = this;
	arity++;
}

export_t *exp = code_base_lookup(m, f, arity);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, arity);
	light_swap_in();
	r0 = m;
	rs[1] = f;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}


cp = demasquerade_pointer(sp[0]);
sp += ((ip[1] >> 8) & 255)+1;
if (unlikely(exp->is_bif))
{
	swap_out();
	term_t r = invoke_bif(exp, proc, rs, arity +2);		// +2 for module, function
	swap_in();
	if (r == noval)
	{
		raise_bif_mfa.mod = exp->module;
		raise_bif_mfa.fun = exp->function;
		raise_bif_mfa.arity = exp->arity;
		rs[exp->arity] = proc->bif_excep_reason;
		goto raise_from_bif;
	}
	r0 = r;
	ip = cp;
	cp = 0; // not to confuse stack tracing
	next();
}
else
{
	ip = exp->entry;
	local_reduce();
}
}

l_bs_get_binary_all_reuse_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[132].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint8_t u = 8;
if (((mc->bs.ends - mc->bs.starts) % u) != 0)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
assert(sizeof(t_sub_bin_t) < sizeof(t_match_ctx_t));
term_t parent = mc->parent;
int64_t starts = mc->bs.starts;
int64_t ends = mc->bs.ends;
uint32_t *p = (uint32_t *)mc;	//same spot
box_sub_bin(p, parent, starts, ends, 0);
//sub_bin and match_ctx share tag (boxed)


ip += 3;
goto *next;
}


self_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[133].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
r0 = proc->pid;


ip += 1;
goto *next;
}


deallocate_return_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[134].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 4+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_fetch_17: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[135].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
tmp_arg2 = rs[2];


ip += 2;
goto *next;
}


set_tuple_element_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[136].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t tuple = r0;
assert(is_tuple(tuple));
(peel_tuple(tuple))[((ip[1] >> 8) & 255)+1] = sp[((ip[1] >> 0) & 255)+1];


ip += 2;
goto *next;
}


l_bs_match_string_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[137].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = ((((ip[3] >> 0) & 255) == 0) ?r0 :rs[((ip[3] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ((ip[3] >> 8) & 255);
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
bits_t bs1 = {
	.data = (uint8_t *)expand_ptr(ip[2]),
	.starts = 0,
	.ends = bcount
};
bits_t bs2 = {
	.data = mc->bs.data,
	.starts = mc->bs.starts,
	.ends = mc->bs.starts +bcount
};
//NB: bits_compare garbles both bs1 and bs2
if (bits_compare(&bs1, &bs2) != 0)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

mc->bs.starts += bcount;


ip += 4;
goto *next;
}


l_allocate_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[138].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
int gap = ip[1]+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 2;
goto *next;
}


l_move_call_only_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[139].counter++;
#endif


r0 = rs[2];
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_select_val_smallints_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[140].counter++;
#endif


term_t v = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
pair_t *alpha = (pair_t *)(ip + 3);
pair_t *beta = (pair_t *)((uint32_t *)alpha + ((ip[2] >> 8) & 255));

if (v < alpha->f || v > beta[-1].f)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

//TODO: in BEAM the binary search parameters are
// made into unsigned values for performance

while (beta > alpha+1)	// at least 2 pairs
{
	pair_t *mid = alpha + (beta - alpha +1)/2;
	if ((term_t)mid->f > v)
		beta = mid;
	else
		alpha = mid;
}

assert(beta == alpha+1);
if (alpha->f == v)
{
	ip = (uint32_t *)expand_ptr(alpha->s);
	next();
}

do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


}

l_bs_get_integer_8_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[141].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends - mc->bs.starts < 8)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t v;
bits_get_octet(&mc->bs, v);
{
	int reg__ = ((ip[2] >> 0) & 255);
	if (reg__ == 0)
		r0 = tag_int(v);
	else
		rs[reg__] = tag_int(v);
}



ip += 3;
goto *next;
}


move_deallocate_return_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[142].counter++;
#endif


r0 = sp[((ip[1] >> 0) & 255)+1];
cp = demasquerade_pointer(sp[0]);
sp += ((ip[1] >> 8) & 255)+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

is_list_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[143].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t l = rs[2];
if (!is_list(l))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_atom_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[144].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_atom(rs[2]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_allocate_zero_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[145].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 2+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = 2;
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 1;
goto *next;
}


send_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[146].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
ip += 1;
term_t dst = r0;

// {Dst,Node} fails except when Node is nonode@nohost
if (is_tuple(r0))
{
	uint32_t *p = peel_tuple(r0);
	if (p[0] != 2 || p[2] != ANONODE_NOHOST__)
		badarg();	// TODO: distributed send
	dst = p[1];
}

if (!is_atom(dst) && !is_short_pid(dst) && !is_short_oid(dst))
	badarg();	// TODO: long pids/oids

proc_t *to_proc = 0;
outlet_t *to_outlet = 0;

if (unlikely(is_atom(dst)))
{
	to_proc = scheduler_process_by_name(dst);
	if (to_proc == 0)
		to_outlet = outlet_lookup_by_name(dst);
}
else if (likely(is_short_pid(dst)))
	to_proc = scheduler_lookup(dst);
else
	to_outlet = outlet_lookup(dst);

if (to_proc != 0)
{
	term_t marshalled_message = rs[1];
	if (to_proc != proc)	// was: dst == proc->pid
	{
		int x = heap_copy_terms_N(&to_proc->hp, &marshalled_message, 1);
		if (x < 0)
			raise_error(A_NOT_DELIVERED);
	}

	if (scheduler_new_local_mail_N(to_proc, marshalled_message) < 0)
		raise_error(A_NOT_DELIVERED);
}
else if (to_outlet != 0)
{
	light_swap_out();
	int rc = send_to_outlet(to_outlet, rs[1], proc);
	if (rc == DELIVER_SIGNALS)
	{
		proc->cap.live = 2;
		goto schedule;
	}
	else if (rc < 0)
	{
		rs[1] = A_ERROR;
		rs[2] = err_to_term(rc);
		proc->stack_trace = noval;
		goto exception;
	}
	light_swap_in();
}

goto *next;
}

allocate_heap_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[147].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (unlikely(hend - htop < ((ip[1] >> 8) & 255)))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, ((ip[1] >> 16) & 255));
		heap_ensure(&proc->hp, ((ip[1] >> 8) & 255), root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, ((ip[1] >> 8) & 255));
	swap_in();
}


int gap = ((ip[1] >> 0) & 255)+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 2;
goto *next;
}


l_is_eq_exact_immed_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[148].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[7] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_trim_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[149].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t masqueraded_cp = sp[0];
sp += 2;
sp[0] = masqueraded_cp;


ip += 1;
goto *next;
}


move_deallocate_return_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[150].counter++;
#endif


r0 = A_OK;
cp = demasquerade_pointer(sp[0]);
sp += ip[1]+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

init3_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[151].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[((ip[1] >> 16) & 255)+1] = sp[((ip[1] >> 8) & 255)+1] = sp[((ip[1] >> 0) & 255)+1] = nil;


ip += 2;
goto *next;
}


deallocate_return_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[152].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 3+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

extract_next_element3_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[153].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 2;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


call_bif_36: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[154].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_reverse2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_bif2_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[155].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t r = ((bif_func2_t)(bif_func_t)expand_ptr(ip[2]))(tmp_arg1, tmp_arg2, proc);
if (r == noval)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 4;
goto *next;
}


is_integer_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[156].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = r0;
if (!is_int(v))
{
	if (!is_boxed(v) || (*peel_boxed(v) & SUBTAG_BIGNUM_MASK) != SUBTAG_BIGNUM)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


move_return_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[157].counter++;
#endif


r0 = A_TRUE;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_allocate_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[158].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 4+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 1;
goto *next;
}


l_bif2_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[159].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r = ((bif_func2_t)bif_and2)(tmp_arg1, tmp_arg2, proc);
if (r == noval)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 3;
goto *next;
}


l_bif2_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[160].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r = ((bif_func2_t)bif_eq_exact2)(tmp_arg1, tmp_arg2, proc);
if (r == noval)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 3;
goto *next;
}


is_atom_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[161].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_atom(rs[3]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_call_last_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[162].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 1+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

extract_next_element_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[163].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[7] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_move_call_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[164].counter++;
#endif


r0 = rs[1];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

move_deallocate_return_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[165].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = demasquerade_pointer(sp[0]);
sp += 0+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_move_call_ext_last_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[166].counter++;
#endif


r0 = sp[((ip[2] >> 8) & 255)+1];
cp = demasquerade_pointer(sp[0]);
sp += ((ip[2] >> 0) & 255)+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_bs_get_binary2_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[167].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

term_t s = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
if (!is_int(s) && !(is_boxed(s) && is_bignum(peel_boxed(s))))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t bcount;
if (bits_calc_bit_size(s, ((ip[4] >> 8) & 255), &bcount) < 0)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

light_swap_out();
int needed = WSIZE(t_sub_bin_t);
uint32_t *p = heap_alloc(&proc->hp, needed);
t_sub_bin_t *sb = (t_sub_bin_t *)p;
box_sub_bin(p, mc->parent, mc->bs.starts, mc->bs.starts+bcount, 0);
heap_set_top(&proc->hp, p);
light_swap_in();

mc->bs.starts += bcount;
{
	int reg__ = ((ip[4] >> 24) & 255);
	if (reg__ == 0)
		r0 = tag_boxed(sb);
	else
		rs[reg__] = tag_boxed(sb);
}



ip += 5;
goto *next;
}


l_is_eq_exact_literal_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[168].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t a = rs[1];
term_t b = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (a != b && !are_terms_equal(a, b, 1))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_24: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[169].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+507);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[170].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+380);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_fetch_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[171].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = rs[4];
tmp_arg2 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


l_move_call_last_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[172].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
cp = demasquerade_pointer(sp[0]);
sp += 2+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_new_bs_put_binary_imm_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[173].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
uint32_t bcount = ((ip[2] >> 0) & 255);
//NB: Flags not used
term_t bin = sp[((ip[2] >> 16) & 255)+1];
if (!is_boxed(bin) || !is_binary(peel_boxed(bin)))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

bits_t bs;
bits_get_real(peel_boxed(bin), &bs);
if (bs.ends - bs.starts < bcount)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
assert(bpc.ends - bpc.starts >= bcount);
bs.ends = bs.starts +bcount;
bits_copy(&bs, &bpc);


ip += 3;
goto *next;
}


l_minus_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[174].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int v = int_value(tmp_arg1) - int_value(tmp_arg2);
	if (likely(fits_int(v)))
	{
		{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_int(v);
	else
		rs[reg__] = tag_int(v);
}

	}
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_boxed(bn);
	else
		rs[reg__] = tag_boxed(bn);
}

	}
}
else
{
	swap_out();
	term_t r = mixed_sub(tmp_arg1, tmp_arg2, &proc->hp);
	swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, AMINUS__, tmp_arg1, tmp_arg2);
	}
	{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}

}


ip += 3;
goto *next;
}


l_call_last_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[175].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 0+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_times_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[176].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int64_t v = (int64_t)int_value(tmp_arg1) * int_value(tmp_arg2);
	if (likely(fits_int(v)))
	{
		{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_int(v);
	else
		rs[reg__] = tag_int(v);
}

	}
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_boxed(bn);
	else
		rs[reg__] = tag_boxed(bn);
}

	}
}
else
{
	light_swap_out();
	term_t r = mixed_mul(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, ATIMES__, tmp_arg1, tmp_arg2);
	}
	{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}

}


ip += 3;
goto *next;
}


l_bs_init_bits_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[177].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
uint32_t bsz = ip[1];
int uneven = (bsz & 7) != 0;
uint32_t osz = (bsz +7) /8;
int needed = (osz <= MAX_HEAP_BIN)
	?WSIZE(t_heap_bin_t) + (osz +3)/4
	:WSIZE(t_proc_bin_t);
if (uneven)
	needed += WSIZE(t_sub_bin_t);
needed += ip[2];
light_swap_out();
uint32_t *p = heap_alloc(&proc->hp, needed);
term_t bin = tag_boxed(p);
int is_writable = 1;
if (osz <= MAX_HEAP_BIN)
{
	box_heap_bin(p, osz, 0);
	is_writable = 0;
}
else
{
	binnode_t *node = binnode_make(osz);
	t_proc_bin_t *pb = (t_proc_bin_t *)p;
	box_proc_bin(p, osz, node);
	proc_bin_link(&proc->hp.proc_bins, pb, &proc->hp.total_pb_size);
}
if (uneven)
{
	term_t parent = bin;
	bin = tag_boxed(p);
	box_sub_bin(p, parent, 0, bsz, is_writable);
}
heap_set_top0(&proc->hp, p);
light_swap_in();
bits_get_real(peel_boxed(bin), &bpc);
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = bin;
		else
			rs[reg__] = bin;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = bin;
	}
}





ip += 5;
goto *next;
}


l_fetch_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[178].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = rs[3];
tmp_arg2 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


l_increment_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[179].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
int i = (int)ip[2];
term_t src = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
term_t r;
if (is_int(src))
{
	int v = int_value(src) + i;
	if (likely(fits_int(v)))
		r = tag_int(v);
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		r = tag_boxed(bn);
	}
}
else
{
	light_swap_out();
	r = mixed_add_immed(src, i, &proc->hp);
	light_swap_in();
	if (is_atom(r))
		bif_error2(r, A_ERLANG, APLUS__, src, tag_int(i));
}
r0 = r;


ip += 4;
goto *next;
}


l_times_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[180].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int64_t v = (int64_t)int_value(tmp_arg1) * int_value(tmp_arg2);
	if (likely(fits_int(v)))
	{
		r0 = tag_int(v);
	}
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		r0 = tag_boxed(bn);
	}
}
else
{
	light_swap_out();
	term_t r = mixed_mul(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, ATIMES__, tmp_arg1, tmp_arg2);
	}
	r0 = r;
}


ip += 3;
goto *next;
}


l_bs_get_integer_32_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[181].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends - mc->bs.starts < 32)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t v;
bits_get_word(&mc->bs, v);
light_swap_out();
term_t r = int_to_term(v, &proc->hp);
light_swap_in();
{
	int reg__ = ((ip[2] >> 16) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 3;
goto *next;
}


l_bs_get_binary_all2_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[182].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

uint32_t bcount = mc->bs.ends - mc->bs.starts;
if ((bcount % ((ip[2] >> 16) & 255)) != 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

light_swap_out();
int needed = WSIZE(t_sub_bin_t);
uint32_t *p = heap_alloc(&proc->hp, needed);
t_sub_bin_t *sb = (t_sub_bin_t *)p;
box_sub_bin(p, mc->parent, mc->bs.starts, mc->bs.ends, 0);
heap_set_top(&proc->hp, p);
light_swap_in();

mc->bs.starts = mc->bs.ends;
{
	int reg__ = ((ip[2] >> 24) & 255);
	if (reg__ == 0)
		r0 = tag_boxed(sb);
	else
		rs[reg__] = tag_boxed(sb);
}



ip += 3;
goto *next;
}


deallocate_return_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[183].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += ip[1]+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_move_call_last_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[184].counter++;
#endif


r0 = sp[((ip[2] >> 8) & 255)+1];
cp = demasquerade_pointer(sp[0]);
sp += ((ip[2] >> 0) & 255)+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_select_val2_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[185].counter++;
#endif


term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (v == A_FALSE)
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == A_TRUE)
	do {
ip = (uint32_t *)expand_ptr(ip[4]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[2]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
}

l_is_eq_exact_literal_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[186].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t a = rs[5];
term_t b = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (a != b && !are_terms_equal(a, b, 1))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


is_binary_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[187].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = rs[2];
if (!is_boxed(v))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t *p = peel_boxed(v);
switch (boxed_tag(p))
{
case SUBTAG_PROC_BIN:
	break;
case SUBTAG_HEAP_BIN:
	break;
case SUBTAG_MATCH_CTX:
{
	t_match_ctx_t *ctx = (t_match_ctx_t *)p;
	if ((ctx->bs.ends-ctx->bs.starts) % 8 != 0)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
	break;
}
case SUBTAG_SUB_BIN:
{
	t_sub_bin_t *sb = (t_sub_bin_t *)p;
	if ((sb->ends - sb->starts) % 8 == 0)
		break;
	/* fall through */
}
default:
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


test_heap_1_put_list_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[188].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
if (unlikely(hend - htop < 2))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, 1);
		heap_ensure(&proc->hp, 2, root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, 2);
	swap_in();
}


term_t hd = sp[1];
term_t tl = r0;
r0 = tag_cons(htop);
*htop++ = hd;
*htop++ = tl;


ip += 1;
goto *next;
}


l_is_lt_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[189].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	if ((int)tmp_arg1 >= (int)tmp_arg2)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}
else
{
	if (!is_term_smaller(tmp_arg1, tmp_arg2))
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


l_plus_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[190].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int v = int_value(tmp_arg1) + int_value(tmp_arg2);
	if (likely(fits_int(v)))
	{
		{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_int(v);
	else
		rs[reg__] = tag_int(v);
}

	}
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_boxed(bn);
	else
		rs[reg__] = tag_boxed(bn);
}

	}
}
else
{
	light_swap_out();
	term_t r = mixed_add(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, APLUS__, tmp_arg1, tmp_arg2);
	}
	{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}

}


ip += 3;
goto *next;
}


l_allocate_zero_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[191].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 5+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = 5;
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 1;
goto *next;
}


l_select_val2_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[192].counter++;
#endif


term_t v = rs[1];
if (v == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == (term_t)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[5]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

extract_next_element3_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[193].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 5;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_select_val2_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[194].counter++;
#endif


term_t v = rs[5];
if (v == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == (term_t)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[5]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

call_bif_18: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[195].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_atom_to_list1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


is_integer_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[196].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_int(v))
{
	if (!is_boxed(v) || (*peel_boxed(v) & SUBTAG_BIGNUM_MASK) != SUBTAG_BIGNUM)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 3;
goto *next;
}


is_binary_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[197].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = r0;
if (!is_boxed(v))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t *p = peel_boxed(v);
switch (boxed_tag(p))
{
case SUBTAG_PROC_BIN:
	break;
case SUBTAG_HEAP_BIN:
	break;
case SUBTAG_MATCH_CTX:
{
	t_match_ctx_t *ctx = (t_match_ctx_t *)p;
	if ((ctx->bs.ends-ctx->bs.starts) % 8 != 0)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
	break;
}
case SUBTAG_SUB_BIN:
{
	t_sub_bin_t *sb = (t_sub_bin_t *)p;
	if ((sb->ends - sb->starts) % 8 == 0)
		break;
	/* fall through */
}
default:
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


deallocate_return_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[198].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 5+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_catch_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[199].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[0+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
proc->catch_level++;


ip += 2;
goto *next;
}


l_trim_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[200].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t masqueraded_cp = sp[0];
sp += 4;
sp[0] = masqueraded_cp;


ip += 1;
goto *next;
}


is_pid_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[201].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = r0;
if (!is_short_pid(v))
{
	if (!is_boxed(v) || boxed_tag(peel_boxed(v)) != SUBTAG_PID)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


l_select_val2_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[202].counter++;
#endif


term_t v = rs[2];
if (v == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == (term_t)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[5]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

is_nonempty_list_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[203].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[4]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_call_last_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[204].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 4+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

is_list_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[205].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t l = r0;
if (!is_list(l))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


extract_next_element3_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[206].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 8;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_select_val2_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[207].counter++;
#endif


term_t v = rs[8];
if (v == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == (term_t)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[5]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

l_int_div_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[208].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int denom = int_value(tmp_arg2);
	if (denom == 0)
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(A_BADARITH, A_ERLANG, A_DIV, tmp_arg1, tmp_arg2);
	}
	int v = int_value(tmp_arg1) / denom;
	assert(fits_int(v));
	r = tag_int(v);
}
else
{
	light_swap_out();
	r = mixed_int_div(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_DIV, tmp_arg1, tmp_arg2);
	}
}
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 3;
goto *next;
}


l_fetch_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[209].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
tmp_arg2 = rs[1];


ip += 2;
goto *next;
}


l_catch_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[210].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[2+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
proc->catch_level++;


ip += 2;
goto *next;
}


catch_end_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[211].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[2+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	assert(rs[1] == A_ERROR || rs[1] == A_THROW || rs[1] == A_EXIT);
	if (rs[1] == A_THROW)
		r0 = rs[2];
	else
	{
		// A very nice spot for a garbage collection:
		// a few live registers and a portion of the
		// stack just gone.
		//
		
		swap_out();
		proc_burn_fat(proc, 0, rs, 3);

		// Both error and exit exceptions are converted to {'EXIT',Reason}
	
		if (rs[1] == A_ERROR)
			rs[2] = heap_tuple2(&proc->hp, rs[2], proc->stack_trace);
		rs[0] = heap_tuple2(&proc->hp, AEXIT__, rs[2]);
		swap_in();
	}
}




ip += 1;
goto *next;
}


l_call_ext_last_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[212].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 0+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_allocate_zero_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[213].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 6+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = 6;
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 1;
goto *next;
}


l_move_call_ext_only_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[214].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[215].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[6] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


init_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[216].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[3] = nil;


ip += 1;
goto *next;
}


try_end_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[217].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[5+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	r0 = rs[1];		// class
	rs[1] = rs[2];  // reason

	//TODO: consider gc round here
}


ip += 1;
goto *next;
}


extract_next_element2_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[218].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 4;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


is_tuple_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[219].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_tuple(rs[1]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_is_ne_exact_immed_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[220].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[2] == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


is_float_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[221].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = r0;
if (!is_boxed(v) || boxed_tag(peel_boxed(v)) != SUBTAG_FLOAT)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


try_end_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[222].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[0+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	r0 = rs[1];		// class
	rs[1] = rs[2];  // reason

	//TODO: consider gc round here
}


ip += 1;
goto *next;
}


l_move_call_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[223].counter++;
#endif


r0 = sp[2];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

extract_next_element_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[224].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[4] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_call_ext_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[225].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+499);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_integer_allocate_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[226].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
if (!is_int(v) && !(is_boxed(v) && is_bignum(peel_boxed(v))))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

int gap = ((ip[2] >> 8) & 255)+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 3;
goto *next;
}


l_fetch_16: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[227].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = rs[5];
tmp_arg2 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


int_code_end_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[228].counter++;
#endif


proc->cap.live = 0;
proc->result.what = SLICE_RESULT_DONE;
light_swap_out();
goto schedule;
}

move_return_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[229].counter++;
#endif


r0 = rs[3];
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_move_call_ext_50: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[230].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 3;
export_t *exp = (export_t *)expand_ptr(ip[2]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[231].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+508);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element2_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[232].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 6;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_catch_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[233].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[4+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
proc->catch_level++;


ip += 2;
goto *next;
}


is_nil_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[234].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[6] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


try_end_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[235].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[4+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	r0 = rs[1];		// class
	rs[1] = rs[2];  // reason

	//TODO: consider gc round here
}


ip += 1;
goto *next;
}


deallocate_return_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[236].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 6+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_is_eq_exact_immed_19: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[237].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[14] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


extract_next_element2_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[238].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 14;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_move_call_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[239].counter++;
#endif


r0 = sp[1];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_ext_only_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[240].counter++;
#endif


r0 = rs[1];
export_t *exp = (preloaded_exports+499);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_port_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[241].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_short_oid(v))
{
	if (!is_boxed(v) || boxed_tag(peel_boxed(v)) != SUBTAG_OID)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 3;
goto *next;
}


l_move_call_ext_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[242].counter++;
#endif


r0 = sp[4];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_is_eq_exact_immed_21: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[243].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[4] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_is_eq_exact_immed_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[244].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[10] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_apply_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[245].counter++;
#endif


term_t args;

// A stub implementation of l_hibernate needs this label
//
l_apply_entry:

args = rs[2];
if (!is_list(args))
	badarg();
int arity = list_len(args);
if (arity < 0 || arity > 255)
	badarg();
export_t *exp = code_base_lookup(r0, rs[1], arity);
if (unlikely(exp == 0 || exp->entry == 0))
 	exp = EH_UNDEF_EXP;
else
{
	heap_list_to_vector(args, rs);
	r0 = rs[0];

	if (exp->is_bif)
	{
		swap_out();
		term_t r = invoke_bif(exp, proc, rs, arity +2);		// +2 for module, function
		swap_in();
		if (r == noval)
		{
			raise_bif_mfa.mod = exp->module;
			raise_bif_mfa.fun = exp->function;
			raise_bif_mfa.arity = exp->arity;
			rs[exp->arity] = proc->bif_excep_reason;
			goto raise_from_bif;
		}
		r0 = r;
		ip += 0 +1;
		next();
	}
}
cp = ip + 0+1;
ip = (uint32_t *)exp->entry;
local_reduce();
}

is_list_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[246].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t l = rs[4];
if (!is_list(l))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_get_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[247].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t r = lookup_process_dictionary((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]), proc->dictionary);
r0 = r;


ip += 2;
goto *next;
}


l_select_val2_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[248].counter++;
#endif


term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (v == A_TRUE)
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == A_FALSE)
	do {
ip = (uint32_t *)expand_ptr(ip[4]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[2]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
}

node_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[249].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[3] = cluster_node;


ip += 1;
goto *next;
}


l_is_eq_exact_immed_27: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[250].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[6] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


deallocate_return_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[251].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 9+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

extract_next_element2_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[252].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 10;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


call_bif_42: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[253].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_match_object2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_select_val2_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[254].counter++;
#endif


term_t v = sp[3];
if (v == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == (term_t)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[5]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

l_move_call_only_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[255].counter++;
#endif


r0 = nil;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_call_ext_80: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[256].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+509);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_is_eq_exact_literal_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[257].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t a = rs[3];
term_t b = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (a != b && !are_terms_equal(a, b, 1))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_bs_init_bits_fail_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[258].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
uint32_t bsz;
int x = bits_calc_bit_size(tmp_arg1, 1, &bsz);
if (x == -TOO_LONG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	raise_error(A_SYSTEM_LIMIT);
}
if (x < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
int uneven = (bsz & 7) != 0;
uint32_t osz = (bsz +7) /8;
int needed = (osz <= MAX_HEAP_BIN)
	?WSIZE(t_heap_bin_t) + (osz +3)/4
	:WSIZE(t_proc_bin_t);
if (uneven)
	needed += WSIZE(t_sub_bin_t);
needed += ((ip[2] >> 0) & 255);
light_swap_out();
uint32_t *p = heap_alloc(&proc->hp, needed);
term_t bin = tag_boxed(p);
int is_writable = 1;
if (osz <= MAX_HEAP_BIN)
{
	box_heap_bin(p, osz, 0);
	is_writable = 0;
}
else
{
	binnode_t *node = binnode_make(osz);
	t_proc_bin_t *pb = (t_proc_bin_t *)p;
	box_proc_bin(p, osz, node);
	proc_bin_link(&proc->hp.proc_bins, pb, &proc->hp.total_pb_size);
}
if (uneven)
{
	term_t parent = bin;
	bin = tag_boxed(p);
	box_sub_bin(p, parent, 0, bsz, is_writable);
}
heap_set_top0(&proc->hp, p);
light_swap_in();
bits_get_real(peel_boxed(bin), &bpc);
{
	int reg__ = ((ip[2] >> 16) & 255);
	if (reg__ == 0)
		r0 = bin;
	else
		rs[reg__] = bin;
}





ip += 3;
goto *next;
}


l_move_call_last_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[259].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
cp = demasquerade_pointer(sp[0]);
sp += 3+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_new_bs_put_binary_all_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[260].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
// Fail is unused
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && is_binary(peel_boxed(bin)));

bits_t bs;
bits_get_real(peel_boxed(bin), &bs);
bits_copy(&bs, &bpc);


ip += 4;
goto *next;
}


l_rem_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[261].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int denom = int_value(tmp_arg2);
	if (denom == 0)
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(A_BADARITH, A_ERLANG, A_REM, tmp_arg1, tmp_arg2);
	}
	int v = int_value(tmp_arg1) % denom;
	// always fits int
	{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_int(v);
	else
		rs[reg__] = tag_int(v);
}

}
else
{
	swap_out();
	term_t r = mixed_rem(tmp_arg1, tmp_arg2, &proc->hp);
	swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_REM, tmp_arg1, tmp_arg2);
	}
	{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}

}


ip += 3;
goto *next;
}


l_move_call_last_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[262].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
cp = demasquerade_pointer(sp[0]);
sp += 1+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

move_return_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[263].counter++;
#endif


r0 = tag_int(6);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_plus_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[264].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int v = int_value(tmp_arg1) + int_value(tmp_arg2);
	if (likely(fits_int(v)))
	{
		sp[((ip[2] >> 8) & 255)+1] = tag_int(v);
	}
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		sp[((ip[2] >> 8) & 255)+1] = tag_boxed(bn);
	}
}
else
{
	light_swap_out();
	term_t r = mixed_add(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, APLUS__, tmp_arg1, tmp_arg2);
	}
	sp[((ip[2] >> 8) & 255)+1] = r;
}


ip += 3;
goto *next;
}


l_bs_put_string_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[265].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
bits_t src;
bits_init_buf((uint8_t *)expand_ptr(ip[1]), 2, &src);
bits_copy(&src, &bpc);


ip += 2;
goto *next;
}


l_new_bs_put_integer_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[266].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t sz = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
uint32_t bsz;
if (bits_calc_bit_size(sz, 1, &bsz) < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

assert(bpc.ends-bpc.starts >= bsz);
term_t v = tag_int(0);
uint8_t flags = 0;
if (is_int(v))
{
	light_swap_out();
	bits_put_integer(&bpc, int_value(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else if (is_boxed(v) && is_bignum(peel_boxed(v)))
{
	light_swap_out();
	bits_put_bignum(&bpc, (bignum_t *)peel_boxed(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 3;
goto *next;
}


move_deallocate_return_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[267].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = demasquerade_pointer(sp[0]);
sp += 4+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

wait_timeout_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[268].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t timeout = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (timeout == A_INFINITY)
{
//TODO: change this into goto to wait iop
ip = (uint32_t *)expand_ptr(ip[1]);
proc->result.what = SLICE_RESULT_WAIT;
proc->result.until_when = LING_INFINITY;
//proc->result.jump_to not set

proc->cap.live = 0;
light_swap_out();
goto schedule;
}

if (!is_int(timeout) && !(is_boxed(timeout) && is_bignum(peel_boxed(timeout))))
	raise_error(A_TIMEOUT_VALUE);

light_swap_out();
int64_t millis = (is_int(timeout))
	?int_value(timeout)
	:bignum_to_int((bignum_t *)peel_boxed(timeout));
light_swap_in();
if (millis < 0 || millis > 0xffffffff)
	raise_error(A_TIMEOUT_VALUE);

if (millis == 0)
{
	ip += 3;
	goto *next;
}

proc->result.what = SLICE_RESULT_WAIT;
uint64_t now = monotonic_clock();
proc->result.until_when = now + (uint64_t)millis *1000000;
proc->result.jump_to = ip + 3;

ip = (uint32_t *)expand_ptr(ip[1]);

proc->cap.live = 0;
light_swap_out();
goto schedule;
}

l_is_eq_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[269].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (tmp_arg1 != tmp_arg2)
{
	if (are_both_immed(tmp_arg1, tmp_arg2))
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
	if (!are_terms_equal(tmp_arg1, tmp_arg2, 0))
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


l_move_call_only_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[270].counter++;
#endif


r0 = rs[3];
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

is_integer_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[271].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = rs[4];
if (!is_int(v))
{
	if (!is_boxed(v) || (*peel_boxed(v) & SUBTAG_BIGNUM_MASK) != SUBTAG_BIGNUM)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


l_fetch_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[272].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = rs[2];
tmp_arg2 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


l_call_fun_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[273].counter++;
#endif


uint32_t *saved_ip = ip +0 +1;

 
rs[0] = r0;
uint32_t arity = 2;
term_t t = rs[arity];		//what about {x,1023} mapped to {x,255}?
if (!is_boxed(t))
	goto l_call_fun_3_bad_fun;
uint32_t *p = peel_boxed(t);
if (boxed_tag(p) == SUBTAG_FUN)
{
	t_fun_t *fun = (t_fun_t *)p;
	if (fun->fe == 0)
		fatal_error("unloaded funs not implemented");
	int num_free = fun_num_free(p);
	if (fun_arity(p) != arity+num_free)
		goto l_call_fun_3_bad_arity;
	ip = fun->fe->entry;
	term_t *src = fun->frozen;
	term_t *dst = rs + arity;
	int n = num_free;
	while (n-- > 0)
		*dst++ = *src++;
	r0 = rs[0];
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->is_bif)
	{
		swap_out();
		term_t r = invoke_bif(exp->e, proc, rs, arity +1);		// +1 for export
		swap_in();
		if (r == noval)
		{
			raise_bif_mfa.mod = exp->e->module;
			raise_bif_mfa.fun = exp->e->function;
			raise_bif_mfa.arity = exp->e->arity;
			rs[exp->e->arity] = proc->bif_excep_reason;
			goto raise_from_bif;
		}
		r0 = r;
		ip = saved_ip;


		next();
	}
	if (exp->e->entry == 0)
	{
		//rs[0] = r0;
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		light_swap_in();
		r0 = exp->e->module;
		rs[1] = exp->e->function;
		rs[2] = args;
		ip = (EH_UNDEF_EXP)->entry;
	}
	else if (exp->e->arity != arity)
	{
l_call_fun_3_bad_arity:
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		term_t fun_args = heap_tuple2(&proc->hp, t, args);
		term_t reason = heap_tuple2(&proc->hp, A_BADARITY, fun_args);
		light_swap_in();
		raise_error(reason);
	}
	else
		ip = exp->e->entry;
}
else
{
l_call_fun_3_bad_fun:
	light_swap_out();
	term_t reason = heap_tuple2(&proc->hp, A_BADFUN, t);
	light_swap_in();
	raise_error(reason);
}
cp = saved_ip;
local_reduce();


}



l_move_call_only_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[274].counter++;
#endif


r0 = rs[5];
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_only_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[275].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_select_val2_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[276].counter++;
#endif


term_t v = rs[4];
if (v == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == (term_t)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[5]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

l_fetch_23: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[277].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
tmp_arg1 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
tmp_arg2 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);


ip += 3;
goto *next;
}


is_nonempty_list_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[278].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[5]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_increment_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[279].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
int i = (int)ip[1];
term_t src = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
term_t r;
if (is_int(src))
{
	int v = int_value(src) + i;
	if (likely(fits_int(v)))
		r = tag_int(v);
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		r = tag_boxed(bn);
	}
}
else
{
	light_swap_out();
	r = mixed_add_immed(src, i, &proc->hp);
	light_swap_in();
	if (is_atom(r))
		bif_error2(r, A_ERLANG, APLUS__, src, tag_int(i));
}
{
	int reg__ = ((ip[2] >> 16) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 3;
goto *next;
}


is_list_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[280].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t l = rs[1];
if (!is_list(l))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


bif1_body_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[281].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t r = ((bif_func1_t)bif_hd1)((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]), proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
r0 = r;


ip += 2;
goto *next;
}


is_nonempty_list_allocate_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[282].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (!is_cons(r0))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

int gap = ip[2]+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 3;
goto *next;
}


l_bs_get_integer_8_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[283].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends - mc->bs.starts < 8)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t v;
bits_get_octet(&mc->bs, v);
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_int(v);
	else
		rs[reg__] = tag_int(v);
}



ip += 3;
goto *next;
}


l_gc_bif1_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[284].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t t = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
swap_out();
term_t r = ((gc_bif_func1_t)gc_bif_length1)
					(t, proc, rs, ((ip[3] >> 0) & 255));
swap_in();
if (r == noval)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_func_t entry = gc_bif_length1;
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);
	assert(bif_exp->arity == 1);
	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = 1;
	rs[0] = t;
	rs[1] = proc->bif_excep_reason;
	goto raise_from_bif;
}
sp[((ip[3] >> 8) & 255)+1] = r;


ip += 4;
goto *next;
}


apply_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[285].counter++;
#endif



//
// NB: BEAM uses a function call for this, not a macro
//

int arity = ((ip[1] >> 0) & 255);
term_t m = (arity == 0) ?r0 :rs[arity];
term_t f = rs[arity+1];

term_t this = noval;	// abstract module?
if (!is_atom(f))
	badarg();
if (!is_atom(m))
{
	if (!is_tuple(m))
		badarg();
	uint32_t *p = peel_tuple(m);
	if (p[0] < 1)
		badarg();
	this = m;
	m = p[1];
	if (!is_atom(m))
		badarg();
}

if (this != noval)
{
	if (arity == 0)
		r0 = this;
	else
		rs[arity] = this;
	arity++;
}

export_t *exp = code_base_lookup(m, f, arity);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, arity);
	light_swap_in();
	r0 = m;
	rs[1] = f;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}


if (unlikely(exp->is_bif))
{
	swap_out();
	term_t r = invoke_bif(exp, proc, rs, arity +2);		// +2 for module, function
	swap_in();
	if (r == noval)
	{
		raise_bif_mfa.mod = exp->module;
		raise_bif_mfa.fun = exp->function;
		raise_bif_mfa.arity = exp->arity;
		rs[exp->arity] = proc->bif_excep_reason;
		goto raise_from_bif;
	}
	r0 = r;
	ip += 2;
	next();
}
else
{
	cp = ip + 2;
	ip = exp->entry;
	local_reduce();
}
}

l_is_ne_exact_literal_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[286].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t a = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
term_t b = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
if (a == b || are_terms_equal(a, b, 1))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 4;
goto *next;
}


get_list_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[287].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
assert(is_cons((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1])));
term_t *pair = peel_cons((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]));
r0 = pair[0];
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = pair[1];
		else
			rs[reg__] = pair[1];
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = pair[1];
	}
}



ip += 3;
goto *next;
}


l_element_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[288].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t tuple = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (!is_tuple(tuple))
	badarg();
uint32_t *data = peel_tuple(tuple);
term_t t = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_int(t))
	badarg();
int pos = int_value(t);
if (pos < 1 || pos > data[0])
	badarg();
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = data[pos];
		else
			rs[reg__] = data[pos];
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = data[pos];
	}
}



ip += 4;
goto *next;
}


l_fetch_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[289].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = sp[((ip[1] >> 0) & 255)+1];
tmp_arg2 = sp[((ip[1] >> 8) & 255)+1];


ip += 2;
goto *next;
}


l_is_ne_exact_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[290].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (tmp_arg1 == tmp_arg2)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
if (!are_both_immed(tmp_arg1, tmp_arg2)
		&& are_terms_equal(tmp_arg1, tmp_arg2, 1))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_call_last_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[291].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 3+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

get_tuple_element_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[292].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
//assert(is_tuple(sp[((ip[1] >> 0) & 255)+1]));
term_t elem = peel_tuple(sp[((ip[1] >> 0) & 255)+1])[((ip[1] >> 8) & 255)+1];	// Pos is 0-based
{
	int reg__ = ((ip[1] >> 16) & 255);
	if (reg__ == 0)
		r0 = elem;
	else
		rs[reg__] = elem;
}



ip += 2;
goto *next;
}


get_tuple_element_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[293].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
//assert(is_tuple(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)])));
term_t elem = peel_tuple(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]))[((ip[1] >> 8) & 255)+1];	// Pos is 0-based
sp[((ip[1] >> 16) & 255)+1] = elem;


ip += 2;
goto *next;
}


call_bif_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[294].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_list_to_binary1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


is_nonempty_list_test_heap_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[295].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (!is_cons(r0))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

if (unlikely(hend - htop < ((ip[2] >> 0) & 255)))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, ((ip[2] >> 8) & 255));
		heap_ensure(&proc->hp, ((ip[2] >> 0) & 255), root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, ((ip[2] >> 0) & 255));
	swap_in();
}




ip += 3;
goto *next;
}


l_move_call_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[296].counter++;
#endif


r0 = rs[2];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

is_integer_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[297].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = rs[3];
if (!is_int(v))
{
	if (!is_boxed(v) || (*peel_boxed(v) & SUBTAG_BIGNUM_MASK) != SUBTAG_BIGNUM)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


call_bif_21: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[298].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_binary_to_list1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_bs_skip_bits_imm2_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[299].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ((ip[2] >> 8) & 255);
if (bcount < 0 || bcount > (mc->bs.ends - mc->bs.starts))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
mc->bs.starts += bcount;


ip += 3;
goto *next;
}


is_nonempty_list_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[300].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[3]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_increment_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[301].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
int i = (int)ip[1];
term_t src = sp[((ip[2] >> 0) & 255)+1];
term_t r;
if (is_int(src))
{
	int v = int_value(src) + i;
	if (likely(fits_int(v)))
		r = tag_int(v);
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		r = tag_boxed(bn);
	}
}
else
{
	light_swap_out();
	r = mixed_add_immed(src, i, &proc->hp);
	light_swap_in();
	if (is_atom(r))
		bif_error2(r, A_ERLANG, APLUS__, src, tag_int(i));
}
{
	int reg__ = ((ip[2] >> 16) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 3;
goto *next;
}


test_arity_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[302].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t src = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
assert(is_tuple(src));
if (*peel_tuple(src) != ((ip[2] >> 8) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
tmp_arg1 = shrink_ptr(peel_tuple(src)+1);	// prepare for extract_element


ip += 3;
goto *next;
}


put_list_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[303].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
rs[4] = tag_cons(htop);
htop += 2;


ip += 3;
goto *next;
}


is_tuple_of_arity_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[304].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t src = sp[((ip[2] >> 0) & 255)+1];
if (!is_tuple(src) || *peel_tuple(src) != ((ip[2] >> 8) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
tmp_arg1 = shrink_ptr(peel_tuple(src)+1);	// prepare for extract_element


ip += 3;
goto *next;
}


move_return_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[305].counter++;
#endif


r0 = tag_int(0);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

is_integer_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[306].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = rs[1];
if (!is_int(v))
{
	if (!is_boxed(v) || (*peel_boxed(v) & SUBTAG_BIGNUM_MASK) != SUBTAG_BIGNUM)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


l_bs_test_unit_8_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[307].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = rs[1];
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (((mc->bs.ends - mc->bs.starts) & 7) != 0)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_bs_put_string_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[308].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
bits_t src;
bits_init_buf((uint8_t *)expand_ptr(ip[1]), 4, &src);
bits_copy(&src, &bpc);


ip += 2;
goto *next;
}


l_bs_test_zero_tail2_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[309].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = rs[1];
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends > mc->bs.starts)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_bs_get_binary_all_reuse_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[310].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint8_t u = ((ip[3] >> 0) & 255);
if (((mc->bs.ends - mc->bs.starts) % u) != 0)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
assert(sizeof(t_sub_bin_t) < sizeof(t_match_ctx_t));
term_t parent = mc->parent;
int64_t starts = mc->bs.starts;
int64_t ends = mc->bs.ends;
uint32_t *p = (uint32_t *)mc;	//same spot
box_sub_bin(p, parent, starts, ends, 0);
//sub_bin and match_ctx share tag (boxed)


ip += 4;
goto *next;
}


l_is_eq_exact_immed_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[311].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[8] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


put_list_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[312].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
htop[0] = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
htop[1] = ((((ip[1] >> 8) & 255) == 0) ?r0 :rs[((ip[1] >> 8) & 255)]);
{
	int reg__ = ((ip[1] >> 16) & 255);
	if (reg__ == 0)
		r0 = tag_cons(htop);
	else
		rs[reg__] = tag_cons(htop);
}

htop += 2;


ip += 2;
goto *next;
}


is_nil_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[313].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[3] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


bif2_body_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[314].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t r = ((bif_func2_t)(bif_func_t)expand_ptr(ip[1]))(tmp_arg1, tmp_arg2, proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
r0 = r;


ip += 2;
goto *next;
}


l_fast_element_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[315].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t tuple = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
if (!is_tuple(tuple))
	badarg();
uint32_t *data = peel_tuple(tuple);
uint32_t pos = ((ip[2] >> 8) & 255);
if (pos > data[0])
	badarg();
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = data[pos];
		else
			rs[reg__] = data[pos];
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = data[pos];
	}
}



ip += 3;
goto *next;
}


l_trim_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[316].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t masqueraded_cp = sp[0];
sp += 5;
sp[0] = masqueraded_cp;


ip += 1;
goto *next;
}


catch_end_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[317].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[0+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	assert(rs[1] == A_ERROR || rs[1] == A_THROW || rs[1] == A_EXIT);
	if (rs[1] == A_THROW)
		r0 = rs[2];
	else
	{
		// A very nice spot for a garbage collection:
		// a few live registers and a portion of the
		// stack just gone.
		//
		
		swap_out();
		proc_burn_fat(proc, 0, rs, 3);

		// Both error and exit exceptions are converted to {'EXIT',Reason}
	
		if (rs[1] == A_ERROR)
			rs[2] = heap_tuple2(&proc->hp, rs[2], proc->stack_trace);
		rs[0] = heap_tuple2(&proc->hp, AEXIT__, rs[2]);
		swap_in();
	}
}




ip += 1;
goto *next;
}


get_tuple_element_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[318].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
//assert(is_tuple(r0));
term_t elem = peel_tuple(r0)[((ip[1] >> 0) & 255)+1];	// Pos is 0-based
sp[((ip[1] >> 8) & 255)+1] = elem;


ip += 2;
goto *next;
}


self_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[319].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[1] = proc->pid;


ip += 1;
goto *next;
}


l_move_call_only_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[320].counter++;
#endif


r0 = rs[7];
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

move_deallocate_return_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[321].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = demasquerade_pointer(sp[0]);
sp += 1+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_bs_test_unit_8_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[322].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = rs[2];
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (((mc->bs.ends - mc->bs.starts) & 7) != 0)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


timeout_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[323].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
msg_queue_reset(&proc->mailbox);


ip += 1;
goto *next;
}


is_binary_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[324].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = rs[1];
if (!is_boxed(v))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t *p = peel_boxed(v);
switch (boxed_tag(p))
{
case SUBTAG_PROC_BIN:
	break;
case SUBTAG_HEAP_BIN:
	break;
case SUBTAG_MATCH_CTX:
{
	t_match_ctx_t *ctx = (t_match_ctx_t *)p;
	if ((ctx->bs.ends-ctx->bs.starts) % 8 != 0)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
	break;
}
case SUBTAG_SUB_BIN:
{
	t_sub_bin_t *sb = (t_sub_bin_t *)p;
	if ((sb->ends - sb->starts) % 8 == 0)
		break;
	/* fall through */
}
default:
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


l_allocate_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[325].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 7+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 1;
goto *next;
}


move_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[326].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
		else
			rs[reg__] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
	}
}



ip += 3;
goto *next;
}


l_bif2_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[327].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r = ((bif_func2_t)bif_less_eq2)(tmp_arg1, tmp_arg2, proc);
if (r == noval)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 3;
goto *next;
}


l_move_call_ext_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[328].counter++;
#endif


r0 = sp[5];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_trim_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[329].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t masqueraded_cp = sp[0];
sp += 3;
sp[0] = masqueraded_cp;


ip += 1;
goto *next;
}


l_is_ne_exact_immed_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[330].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[3] == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


call_bif_29: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[331].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_whereis1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


call_bif_46: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[332].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_monitor2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_catch_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[333].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[6+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
proc->catch_level++;


ip += 2;
goto *next;
}


recv_mark_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[334].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
msg_queue_mark(&proc->mailbox, (uint32_t *)expand_ptr(ip[1]));


ip += 2;
goto *next;
}


l_recv_set_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[335].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
msg_queue_restore(&proc->mailbox, ip +1);
// ip+1 points to the next iop, supposedly l_loop_rec


ip += 1;
goto *next;
}


self_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[336].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[2] = proc->pid;


ip += 1;
goto *next;
}


catch_end_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[337].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[6+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	assert(rs[1] == A_ERROR || rs[1] == A_THROW || rs[1] == A_EXIT);
	if (rs[1] == A_THROW)
		r0 = rs[2];
	else
	{
		// A very nice spot for a garbage collection:
		// a few live registers and a portion of the
		// stack just gone.
		//
		
		swap_out();
		proc_burn_fat(proc, 0, rs, 3);

		// Both error and exit exceptions are converted to {'EXIT',Reason}
	
		if (rs[1] == A_ERROR)
			rs[2] = heap_tuple2(&proc->hp, rs[2], proc->stack_trace);
		rs[0] = heap_tuple2(&proc->hp, AEXIT__, rs[2]);
		swap_in();
	}
}




ip += 1;
goto *next;
}


l_is_eq_exact_literal_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[338].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t a = r0;
term_t b = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (a != b && !are_terms_equal(a, b, 1))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_only_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[339].counter++;
#endif


export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_is_eq_exact_immed_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[340].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[9] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


extract_next_element_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[341].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[8] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_fetch_20: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[342].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
tmp_arg2 = rs[5];


ip += 2;
goto *next;
}


l_is_ne_exact_immed_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[343].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[6] == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


is_nonempty_list_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[344].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[7]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_is_eq_exact_immed_35: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[345].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[8] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_select_val2_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[346].counter++;
#endif


term_t v = rs[3];
if (v == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == (term_t)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[5]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

wait_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[347].counter++;
#endif


//TODO: should we burn process fat here too?
ip = (uint32_t *)expand_ptr(ip[1]);
proc->result.what = SLICE_RESULT_WAIT;
proc->result.until_when = LING_INFINITY;
//proc->result.jump_to not set

proc->cap.live = 0;
light_swap_out();
goto schedule;
}

l_select_val_smallints_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[348].counter++;
#endif


term_t v = r0;
pair_t *alpha = (pair_t *)(ip + 3);
pair_t *beta = (pair_t *)((uint32_t *)alpha + ip[2]);

if (v < alpha->f || v > beta[-1].f)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

//TODO: in BEAM the binary search parameters are
// made into unsigned values for performance

while (beta > alpha+1)	// at least 2 pairs
{
	pair_t *mid = alpha + (beta - alpha +1)/2;
	if ((term_t)mid->f > v)
		beta = mid;
	else
		alpha = mid;
}

assert(beta == alpha+1);
if (alpha->f == v)
{
	ip = (uint32_t *)expand_ptr(alpha->s);
	next();
}

do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


}

move_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[349].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
rs[6] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


is_tuple_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[350].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_tuple(rs[4]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


move_return_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[351].counter++;
#endif


r0 = A_OK;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_move_call_only_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[352].counter++;
#endif


r0 = rs[6];
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_allocate_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[353].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 5+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 1;
goto *next;
}


call_bif_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[354].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_insert2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


extract_next_element3_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[355].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 4;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_move_call_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[356].counter++;
#endif


r0 = rs[4];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_fast_element_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[357].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t tuple = r0;
if (!is_tuple(tuple))
	badarg();
uint32_t *data = peel_tuple(tuple);
uint32_t pos = 1;
if (pos > data[0])
	badarg();
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = data[pos];
		else
			rs[reg__] = data[pos];
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = data[pos];
	}
}



ip += 2;
goto *next;
}


move_jump_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[358].counter++;
#endif


r0 = rs[3];
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

extract_next_element_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[359].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


extract_next_element2_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[360].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 5;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_move_call_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[361].counter++;
#endif


r0 = sp[3];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_only_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[362].counter++;
#endif


r0 = rs[1];
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

is_nonempty_list_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[363].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[9]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_catch_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[364].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[3+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
proc->catch_level++;


ip += 2;
goto *next;
}


call_bif_28: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[365].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_spawn1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_call_ext_28: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[366].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+510);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_integer_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[367].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = rs[2];
if (!is_int(v))
{
	if (!is_boxed(v) || (*peel_boxed(v) & SUBTAG_BIGNUM_MASK) != SUBTAG_BIGNUM)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 2;
goto *next;
}


is_nonempty_list_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[368].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[6]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_is_ne_exact_immed_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[369].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if ((term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]) == A_TRUE)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


try_end_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[370].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[3+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	r0 = rs[1];		// class
	rs[1] = rs[2];  // reason

	//TODO: consider gc round here
}


ip += 1;
goto *next;
}


is_tuple_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[371].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_tuple(rs[3]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


extract_next_element2_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[372].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = sp + 1;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_call_last_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[373].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += ip[2]+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

put_list_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[374].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_cons(htop);
		else
			rs[reg__] = tag_cons(htop);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_cons(htop);
	}
}

htop += 2;


ip += 4;
goto *next;
}


move_return_26: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[375].counter++;
#endif


r0 = rs[5];
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

call_bif_44: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[376].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_safe_fixtable2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


extract_next_element3_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[377].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 3;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_move_call_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[378].counter++;
#endif


r0 = rs[3];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_call_ext_101: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[379].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+511);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

call_bif_17: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[380].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_keyfind3;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


move_jump_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[381].counter++;
#endif


r0 = rs[1];
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

init_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[382].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[4] = nil;


ip += 1;
goto *next;
}


l_call_ext_only_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[383].counter++;
#endif


export_t *exp = (preloaded_exports+512);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

put_list_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[384].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = nil;
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_cons(htop);
		else
			rs[reg__] = tag_cons(htop);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_cons(htop);
	}
}

htop += 2;


ip += 3;
goto *next;
}


get_tuple_element_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[385].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
//assert(is_tuple((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1])));
term_t elem = peel_tuple((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]))[0+1];	// Pos is 0-based
r0 = elem;


ip += 2;
goto *next;
}


l_catch_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[386].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[1+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
proc->catch_level++;


ip += 2;
goto *next;
}


l_bs_test_unit_8_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[387].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (((mc->bs.ends - mc->bs.starts) & 7) != 0)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


loop_rec_end_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[388].counter++;
#endif


msg_queue_next(&proc->mailbox);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

l_band_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[389].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
	r = tmp_arg1 & tmp_arg2;	//tag intact, never a carry
else
{
	light_swap_out();
	r = mixed_band(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_BAND, tmp_arg1, tmp_arg2);
	}
}
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 3;
goto *next;
}


l_move_call_ext_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[390].counter++;
#endif


r0 = rs[2];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_nil_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[391].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[5] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


self_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[392].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[1] = proc->pid;


ip += 1;
goto *next;
}


test_heap_1_put_list_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[393].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (unlikely(hend - htop < 2))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, 1);
		heap_ensure(&proc->hp, 2, root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, 2);
	swap_in();
}


term_t hd = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
term_t tl = r0;
r0 = tag_cons(htop);
*htop++ = hd;
*htop++ = tl;


ip += 2;
goto *next;
}


bif2_body_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[394].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t r = ((bif_func2_t)(bif_func_t)expand_ptr(ip[1]))(tmp_arg1, tmp_arg2, proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
rs[1] = r;


ip += 2;
goto *next;
}


l_bs_skip_bits_imm2_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[395].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ip[2];
if (bcount < 0 || bcount > (mc->bs.ends - mc->bs.starts))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
mc->bs.starts += bcount;


ip += 3;
goto *next;
}


l_is_eq_exact_immed_16: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[396].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[1] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_bs_restore2_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[397].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
int i = ((ip[1] >> 8) & 255);
assert(i >= 0 && i < match_ctx_num_slots(peel_boxed(bin)));
mc->bs.starts = mc->saved_offsets[i];


ip += 2;
goto *next;
}


is_nonempty_list_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[398].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[8]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_move_call_ext_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[399].counter++;
#endif


r0 = rs[1];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

call_bif_27: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[400].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_now0;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_move_call_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[401].counter++;
#endif


r0 = sp[4];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

catch_end_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[402].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[1+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	assert(rs[1] == A_ERROR || rs[1] == A_THROW || rs[1] == A_EXIT);
	if (rs[1] == A_THROW)
		r0 = rs[2];
	else
	{
		// A very nice spot for a garbage collection:
		// a few live registers and a portion of the
		// stack just gone.
		//
		
		swap_out();
		proc_burn_fat(proc, 0, rs, 3);

		// Both error and exit exceptions are converted to {'EXIT',Reason}
	
		if (rs[1] == A_ERROR)
			rs[2] = heap_tuple2(&proc->hp, rs[2], proc->stack_trace);
		rs[0] = heap_tuple2(&proc->hp, AEXIT__, rs[2]);
		swap_in();
	}
}




ip += 1;
goto *next;
}


l_call_ext_last_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[403].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 2+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

get_list_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[404].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
assert(is_cons(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)])));
term_t *pair = peel_cons(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]));
sp[((ip[1] >> 8) & 255)+1] = pair[0];
sp[((ip[1] >> 16) & 255)+1] = pair[1];


ip += 2;
goto *next;
}


move_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[405].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[((ip[1] >> 8) & 255)+1] = sp[((ip[1] >> 0) & 255)+1];


ip += 2;
goto *next;
}


l_allocate_zero_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[406].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 7+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = 7;
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 1;
goto *next;
}


extract_next_element_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[407].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_call_ext_37: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[408].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+513);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_last_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[409].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
cp = demasquerade_pointer(sp[0]);
sp += 1+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[410].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[3] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_call_last_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[411].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 7+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_is_eq_exact_immed_26: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[412].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[5] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


call_bif_25: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[413].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_new2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


move_return_22: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[414].counter++;
#endif


r0 = A_NO;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

call_bif_37: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[415].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_list_to_tuple1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


is_nil_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[416].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[10] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


init_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[417].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[5] = nil;


ip += 1;
goto *next;
}


put_list_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[418].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
rs[5] = tag_cons(htop);
htop += 2;


ip += 3;
goto *next;
}


l_call_ext_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[419].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+514);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_is_ne_exact_immed_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[420].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (r0 == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_jump_on_val_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[421].counter++;
#endif


term_t v = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
if (!is_int(v))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
int base = (int)((ip[2] >> 8) & 255);
int i = int_value(v);
if (i < base || i >= base + (int)((ip[2] >> 16) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t *jump_tab = ip + 3;
ip = expand_ptr(jump_tab[i - base]);
next();
}

l_is_ne_exact_immed_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[422].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
if ((term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]) == (term_t)ip[3])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 4;
goto *next;
}


l_call_last_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[423].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 5+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_call_ext_only_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[424].counter++;
#endif


export_t *exp = (preloaded_exports+499);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[425].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[4] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_call_fun_last_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[426].counter++;
#endif


uint32_t num_slots = ((ip[1] >> 8) & 255);

 
rs[0] = r0;
uint32_t arity = ((ip[1] >> 0) & 255);
term_t t = rs[arity];		//what about {x,1023} mapped to {x,255}?
if (!is_boxed(t))
	goto l_call_fun_last_0_bad_fun;
uint32_t *p = peel_boxed(t);
if (boxed_tag(p) == SUBTAG_FUN)
{
	t_fun_t *fun = (t_fun_t *)p;
	if (fun->fe == 0)
		fatal_error("unloaded funs not implemented");
	int num_free = fun_num_free(p);
	if (fun_arity(p) != arity+num_free)
		goto l_call_fun_last_0_bad_arity;
	ip = fun->fe->entry;
	term_t *src = fun->frozen;
	term_t *dst = rs + arity;
	int n = num_free;
	while (n-- > 0)
		*dst++ = *src++;
	r0 = rs[0];
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->is_bif)
	{
		swap_out();
		term_t r = invoke_bif(exp->e, proc, rs, arity +1);		// +1 for export
		swap_in();
		if (r == noval)
		{
			raise_bif_mfa.mod = exp->e->module;
			raise_bif_mfa.fun = exp->e->function;
			raise_bif_mfa.arity = exp->e->arity;
			rs[exp->e->arity] = proc->bif_excep_reason;
			goto raise_from_bif;
		}
		r0 = r;
		ip = demasquerade_pointer(sp[0]);
sp += num_slots +1;
cp = 0;


		next();
	}
	if (exp->e->entry == 0)
	{
		//rs[0] = r0;
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		light_swap_in();
		r0 = exp->e->module;
		rs[1] = exp->e->function;
		rs[2] = args;
		ip = (EH_UNDEF_EXP)->entry;
	}
	else if (exp->e->arity != arity)
	{
l_call_fun_last_0_bad_arity:
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		term_t fun_args = heap_tuple2(&proc->hp, t, args);
		term_t reason = heap_tuple2(&proc->hp, A_BADARITY, fun_args);
		light_swap_in();
		raise_error(reason);
	}
	else
		ip = exp->e->entry;
}
else
{
l_call_fun_last_0_bad_fun:
	light_swap_out();
	term_t reason = heap_tuple2(&proc->hp, A_BADFUN, t);
	light_swap_in();
	raise_error(reason);
}
cp = demasquerade_pointer(sp[0]);
sp += num_slots +1;
local_reduce();


}



l_call_ext_25: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[427].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+515);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element_15: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[428].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[6] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_move_call_ext_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[429].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+516);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[430].counter++;
#endif


r0 = sp[3];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_nil_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[431].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[7] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_bs_save2_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[432].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
int i = ((ip[1] >> 8) & 255);
assert(i >= 0 && i < match_ctx_num_slots(peel_boxed(bin)));
mc->saved_offsets[i] = mc->bs.starts;


ip += 2;
goto *next;
}


try_end_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[433].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[1+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	r0 = rs[1];		// class
	rs[1] = rs[2];  // reason

	//TODO: consider gc round here
}


ip += 1;
goto *next;
}


l_int_div_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[434].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int denom = int_value(tmp_arg2);
	if (denom == 0)
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(A_BADARITH, A_ERLANG, A_DIV, tmp_arg1, tmp_arg2);
	}
	int v = int_value(tmp_arg1) / denom;
	assert(fits_int(v));
	r = tag_int(v);
}
else
{
	light_swap_out();
	r = mixed_int_div(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_DIV, tmp_arg1, tmp_arg2);
	}
}
r0 = r;


ip += 3;
goto *next;
}


bif1_body_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[435].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t r = ((bif_func1_t)bif_not1)(r0, proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
r0 = r;


ip += 1;
goto *next;
}


l_bor_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[436].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
	r = tmp_arg1 | tmp_arg2;	//tag intact, never a carry
else
{
	light_swap_out();
	r = mixed_bor(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_BOR, tmp_arg1, tmp_arg2);
	}
}
r0 = r;


ip += 3;
goto *next;
}


l_is_eq_exact_immed_18: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[437].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[255] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


extract_next_element_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[438].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[255] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_bsl_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[439].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int64_t v1 = int_value(tmp_arg1);
	int v2 = int_value(tmp_arg2);

	if (v2 <= 32 +TAG_IMMED1_SIZE)
	{
		int64_t v = (v2 <= -64)
			?((v1 < 0) ?-1 :0)
			:((v2 > 0) ?v1 << v2 :v1 >> -v2);
		if (fits_int(v))
			r = tag_int(v);
		else
		{
			light_swap_out();
			bignum_t *bn = bignum_from_int(&proc->hp, v);
			light_swap_in();
			r = tag_boxed(bn);
		}
	}
	else
	{
		light_swap_out();
		r = mixed_bsl_i(v1, v2, &proc->hp);
		light_swap_in();
	}
}
else
{
	light_swap_out();
	r = mixed_bsl(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
}
if (is_atom(r))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_error2(r, A_ERLANG, A_BSL, tmp_arg1, tmp_arg2);
}
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 3;
goto *next;
}


l_move_call_ext_only_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[440].counter++;
#endif


r0 = rs[2];
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

deallocate_return_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[441].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 7+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_jump_on_val_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[442].counter++;
#endif


term_t v = r0;
if (!is_int(v))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
int base = (int)((ip[2] >> 0) & 255);
int i = int_value(v);
if (i < base || i >= base + (int)((ip[2] >> 8) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t *jump_tab = ip + 3;
ip = expand_ptr(jump_tab[i - base]);
next();
}

get_list_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[443].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
assert(is_cons(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)])));
term_t *pair = peel_cons(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]));
sp[((ip[1] >> 8) & 255)+1] = pair[0];
{
	int reg__ = ((ip[1] >> 16) & 255);
	if (reg__ == 0)
		r0 = pair[1];
	else
		rs[reg__] = pair[1];
}



ip += 2;
goto *next;
}


catch_end_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[444].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[3+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	assert(rs[1] == A_ERROR || rs[1] == A_THROW || rs[1] == A_EXIT);
	if (rs[1] == A_THROW)
		r0 = rs[2];
	else
	{
		// A very nice spot for a garbage collection:
		// a few live registers and a portion of the
		// stack just gone.
		//
		
		swap_out();
		proc_burn_fat(proc, 0, rs, 3);

		// Both error and exit exceptions are converted to {'EXIT',Reason}
	
		if (rs[1] == A_ERROR)
			rs[2] = heap_tuple2(&proc->hp, rs[2], proc->stack_trace);
		rs[0] = heap_tuple2(&proc->hp, AEXIT__, rs[2]);
		swap_in();
	}
}




ip += 1;
goto *next;
}


is_nil_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[445].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[8] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


put_list_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[446].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
sp[((ip[2] >> 8) & 255)+1] = tag_cons(htop);
htop += 2;


ip += 3;
goto *next;
}


get_list_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[447].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
assert(is_cons(r0));
term_t *pair = peel_cons(r0);
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = pair[0];
		else
			rs[reg__] = pair[0];
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = pair[0];
	}
}

r0 = pair[1];


ip += 2;
goto *next;
}


is_tuple_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[448].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (!is_tuple((term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2])))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_apply_last_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[449].counter++;
#endif


term_t args = rs[2];
if (!is_list(args))
	badarg();
int arity = list_len(args);
if (arity < 0 || arity > 255)
	badarg();
cp = demasquerade_pointer(sp[0]);
sp += ip[1]+1;
export_t *exp = code_base_lookup(r0, rs[1], arity);
if (unlikely(exp == 0 || exp->entry == 0))
	exp = EH_UNDEF_EXP;
else
{
	heap_list_to_vector(args, rs);
	r0 = rs[0];

	if (exp->is_bif)
	{
		swap_out();
		term_t r = invoke_bif(exp, proc, rs, arity +2);		// +2 for module, function
		swap_in();
		if (r == noval)
		{
			raise_bif_mfa.mod = exp->module;
			raise_bif_mfa.fun = exp->function;
			raise_bif_mfa.arity = exp->arity;
			rs[exp->arity] = proc->bif_excep_reason;
			goto raise_from_bif;
		}
		r0 = r;
		ip = cp;
		cp = 0;
		next();
	}
}
ip = exp->entry;
local_reduce();
}

l_call_ext_43: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[450].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+517);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

call_bif_30: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[451].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_keymember3;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_bsr_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[452].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int64_t v1 = int_value(tmp_arg1);
	int v2 = int_value(tmp_arg2);

	if (v2 >= -32 -TAG_IMMED1_SIZE)
	{
		int64_t v = (v2 >= 64)
			?((v1 < 0) ?-1 :0)
			:((v2 > 0) ?v1 >> v2 :v1 << -v2);
		if (fits_int(v))
			r = tag_int(v);
		else
		{
			light_swap_out();
			bignum_t *bn = bignum_from_int(&proc->hp, v);
			light_swap_in();
			r = tag_boxed(bn);
		}
	}
	else
	{
		light_swap_out();
		r = mixed_bsr_i(v1, v2, &proc->hp);
		light_swap_in();
	}
}
else
{
	light_swap_out();
	r = mixed_bsr(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
}
if (is_atom(r))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_error2(r, A_ERLANG, A_BSR, tmp_arg1, tmp_arg2);
}
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 3;
goto *next;
}


l_move_call_ext_15: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[453].counter++;
#endif


r0 = rs[3];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_bs_skip_bits2_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[454].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
term_t sz = ((((ip[2] >> 8) & 255) == 0) ?r0 :rs[((ip[2] >> 8) & 255)]);
uint32_t bcount;
if (bits_calc_bit_size(sz, ((ip[2] >> 16) & 255), &bcount) < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
if (bcount > (mc->bs.ends - mc->bs.starts))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
mc->bs.starts += bcount;


ip += 3;
goto *next;
}


l_call_ext_71: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[455].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+518);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_is_ne_exact_immed_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[456].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[1] == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


is_list_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[457].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t l = rs[3];
if (!is_list(l))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


init_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[458].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[6] = nil;


ip += 1;
goto *next;
}


call_bif_40: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[459].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_next2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_is_eq_exact_immed_22: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[460].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[3] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


extract_next_element_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[461].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[9] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_is_ne_exact_immed_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[462].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[5] == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_get_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[463].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t r = lookup_process_dictionary((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]), proc->dictionary);
rs[1] = r;


ip += 2;
goto *next;
}


is_nonempty_list_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[464].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[10]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_move_call_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[465].counter++;
#endif


r0 = sp[5];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_fast_element_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[466].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t tuple = r0;
if (!is_tuple(tuple))
	badarg();
uint32_t *data = peel_tuple(tuple);
uint32_t pos = 2;
if (pos > data[0])
	badarg();
r0 = data[pos];


ip += 1;
goto *next;
}


l_move_call_ext_21: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[467].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+507);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_bitstr_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[468].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_boxed(v) || (*peel_boxed(v) & SUBTAG_BINARY_MASK) != SUBTAG_BINARY)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_select_val2_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[469].counter++;
#endif


term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (v == (term_t)ip[3])
	do {
ip = (uint32_t *)expand_ptr(ip[4]);
next();
} while (0);
if (v == (term_t)ip[5])
	do {
ip = (uint32_t *)expand_ptr(ip[6]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[2]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
}

call_bif_31: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[470].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_tuple_to_list1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_call_ext_96: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[471].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+519);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

move_return_17: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[472].counter++;
#endif


r0 = tag_int(10);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_is_eq_exact_immed_15: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[473].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[2] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_51: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[474].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+520);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

call_bif_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[475].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_get_module_info2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


move_jump_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[476].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

l_trim_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[477].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t masqueraded_cp = sp[0];
sp += 6;
sp[0] = masqueraded_cp;


ip += 1;
goto *next;
}


is_function_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[478].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = r0;
if (!is_boxed(v) || (boxed_tag(peel_boxed(v)) != SUBTAG_FUN &&
					 boxed_tag(peel_boxed(v)) != SUBTAG_EXPORT))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_call_ext_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[479].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+505);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

call_bif_26: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[480].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_delete2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


extract_next_element_16: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[481].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[5] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_move_call_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[482].counter++;
#endif


r0 = sp[6];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

extract_next_element2_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[483].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 8;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_allocate_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[484].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 8+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 1;
goto *next;
}


bif1_body_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[485].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t r = ((bif_func1_t)(bif_func_t)expand_ptr(ip[1]))((term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]), proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 4;
goto *next;
}


l_call_ext_last_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[486].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += ip[2]+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_fetch_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[487].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
tmp_arg1 = sp[1];
tmp_arg2 = rs[2];


ip += 1;
goto *next;
}


deallocate_return_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[488].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 10+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

call_bif_38: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[489].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_make_ref0;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


init_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[490].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[10] = nil;


ip += 1;
goto *next;
}


move_return_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[491].counter++;
#endif


r0 = rs[4];
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

is_nil_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[492].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[9] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


move_deallocate_return_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[493].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = demasquerade_pointer(sp[0]);
sp += 3+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_fdiv_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[494].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
double val = fr[((ip[1] >> 8) & 255)];
if (val == 0.0)
	raise_error(A_BADARITH);
double d = fr[((ip[1] >> 0) & 255)] / val;
if (!isfinite(d))
	raise_error(A_BADARITH);
fr[((ip[1] >> 16) & 255)] = d;


ip += 2;
goto *next;
}


l_band_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[495].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
	r = tmp_arg1 & tmp_arg2;	//tag intact, never a carry
else
{
	light_swap_out();
	r = mixed_band(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_BAND, tmp_arg1, tmp_arg2);
	}
}
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 4;
goto *next;
}


l_bs_test_zero_tail2_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[496].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = rs[5];
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends > mc->bs.starts)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


call_bif_22: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[497].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_lookup_element3;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_fcheckerror_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[498].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);

//debug("l_fcheckerror ignored\n");
//TODO: ignore for now


ip += 1;
goto *next;
}


fclearerror_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[499].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);

//debug("fclearerror ignored\n");
//TODO: ignore for now


ip += 1;
goto *next;
}


allocate_heap_zero_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[500].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (unlikely(hend - htop < ((ip[1] >> 8) & 255)))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, ((ip[1] >> 16) & 255));
		heap_ensure(&proc->hp, ((ip[1] >> 8) & 255), root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, ((ip[1] >> 8) & 255));
	swap_in();
}


int gap = ((ip[1] >> 0) & 255)+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = ((ip[1] >> 0) & 255);
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 2;
goto *next;
}


fconv_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[501].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
double converted = term_to_float(v);
if (!isfinite(converted))
	raise_error(A_BADARITH);
fr[0] = converted;


ip += 2;
goto *next;
}


fmove_1_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[502].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
	// s fr
term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(v) && boxed_tag(peel_boxed(v)) == SUBTAG_FLOAT);
uint32_t *tdata = peel_boxed(v);
fr[1] = float_value(tdata);


ip += 2;
goto *next;
}


l_select_val_atoms_1: 
 {

// NB: self-modifying code: for dynamically loaded modules
// l_select_val_atoms iop points to &&l_select_val_atoms_N;
// after sorting the list it changes to
// &&l_select_val_atoms_N_sorted; statically loaded modules
// use the second label as the list is already in order.

	uint32_t nr_pairs = (ip[2])/2;
	qsort(ip + 3,
		nr_pairs, 2*sizeof(uint32_t), select_val_atoms_compare);
	
	*ip = shrink_ptr(&&l_select_val_atoms_1_sorted);
}

l_select_val_atoms_1_sorted: {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[503].counter++;
#endif



term_t v = r0;
pair_t *alpha = (pair_t *)(ip + 3);
pair_t *beta = (pair_t *)((uint32_t *)alpha + ip[2]);

if (v < alpha->f || v > beta[-1].f)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

//TODO: in BEAM the binary search parameters are
// made into unsigned values for performance

while (beta > alpha+1)	// at least 2 pairs
{
	pair_t *mid = alpha + (beta - alpha +1)/2;
	if ((term_t)mid->f > v)
		beta = mid;
	else
		alpha = mid;
}

assert(beta == alpha+1);
if (alpha->f == v)
{
	ip = (uint32_t *)expand_ptr(alpha->s);
	next();
}

do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


}

l_move_call_ext_last_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[504].counter++;
#endif


r0 = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
cp = demasquerade_pointer(sp[0]);
sp += ip[2]+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_bsl_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[505].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int64_t v1 = int_value(tmp_arg1);
	int v2 = int_value(tmp_arg2);

	if (v2 <= 32 +TAG_IMMED1_SIZE)
	{
		int64_t v = (v2 <= -64)
			?((v1 < 0) ?-1 :0)
			:((v2 > 0) ?v1 << v2 :v1 >> -v2);
		if (fits_int(v))
			r = tag_int(v);
		else
		{
			light_swap_out();
			bignum_t *bn = bignum_from_int(&proc->hp, v);
			light_swap_in();
			r = tag_boxed(bn);
		}
	}
	else
	{
		light_swap_out();
		r = mixed_bsl_i(v1, v2, &proc->hp);
		light_swap_in();
	}
}
else
{
	light_swap_out();
	r = mixed_bsl(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
}
if (is_atom(r))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_error2(r, A_ERLANG, A_BSL, tmp_arg1, tmp_arg2);
}
r0 = r;


ip += 3;
goto *next;
}


fmove_2_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[506].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
	// fr s
double dbl = fr[((ip[1] >> 0) & 255)];
term_t v = tag_boxed(htop);
assert(hend - htop >= WSIZE(t_float_t));	// compiler should care
((t_float_t *)htop)->hdr = HDR_IS_NOT_CP | SUBTAG_FLOAT;
((t_float_t *)htop)->val = dbl;
htop += WSIZE(t_float_t);
{
	int reg__ = ((ip[1] >> 8) & 255);
	if (reg__ == 0)
		r0 = v;
	else
		rs[reg__] = v;
}



ip += 2;
goto *next;
}


l_increment_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[507].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
int i = (int)4294967295;
term_t src = r0;
term_t r;
if (is_int(src))
{
	int v = int_value(src) + i;
	if (likely(fits_int(v)))
		r = tag_int(v);
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		r = tag_boxed(bn);
	}
}
else
{
	light_swap_out();
	r = mixed_add_immed(src, i, &proc->hp);
	light_swap_in();
	if (is_atom(r))
		bif_error2(r, A_ERLANG, APLUS__, src, tag_int(i));
}
sp[((ip[1] >> 8) & 255)+1] = r;


ip += 2;
goto *next;
}


l_call_ext_41: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[508].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+521);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[509].counter++;
#endif


r0 = sp[1];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_tuple_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[510].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_tuple(rs[8]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_call_ext_20: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[511].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+516);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

init_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[512].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[7] = nil;


ip += 1;
goto *next;
}


l_move_call_last_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[513].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
cp = demasquerade_pointer(sp[0]);
sp += 0+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_call_ext_38: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[514].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+522);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element3_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[515].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 6;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


move_return_21: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[516].counter++;
#endif


r0 = A_ERROR;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_is_ne_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[517].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (tmp_arg1 == tmp_arg2)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
if (!are_both_immed(tmp_arg1, tmp_arg2)
		&& are_terms_equal(tmp_arg1, tmp_arg2, 0))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


extract_next_element2_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[518].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 7;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_call_ext_66: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[519].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+523);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_nonempty_list_24: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[520].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(sp[1]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


move_jump_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[521].counter++;
#endif


r0 = nil;
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

l_allocate_zero_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[522].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 8+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = 8;
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 1;
goto *next;
}


l_call_ext_23: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[523].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+524);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[524].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+525);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_catch_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[525].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t y = ip[1];
assert(is_slot(y));
sp[slot_index(y)+1] = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
proc->catch_level++;


ip += 3;
goto *next;
}


is_pid_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[526].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_short_pid(v))
{
	if (!is_boxed(v) || boxed_tag(peel_boxed(v)) != SUBTAG_PID)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 3;
goto *next;
}


is_reference_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[527].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_boxed(v) || boxed_tag(peel_boxed(v)) != SUBTAG_REF)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


call_bif_32: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[528].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_process_flag2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


is_nonempty_list_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[529].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[12]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_is_eq_exact_immed_20: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[530].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[13] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_bs_test_zero_tail2_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[531].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends > mc->bs.starts)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_move_call_ext_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[532].counter++;
#endif


r0 = tag_int(1);
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element2_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[533].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 9;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_call_ext_16: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[534].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+352);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_rem_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[535].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int denom = int_value(tmp_arg2);
	if (denom == 0)
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(A_BADARITH, A_ERLANG, A_REM, tmp_arg1, tmp_arg2);
	}
	int v = int_value(tmp_arg1) % denom;
	// always fits int
	{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_int(v);
		else
			rs[reg__] = tag_int(v);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_int(v);
	}
}

}
else
{
	swap_out();
	term_t r = mixed_rem(tmp_arg1, tmp_arg2, &proc->hp);
	swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_REM, tmp_arg1, tmp_arg2);
	}
	{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}

}


ip += 4;
goto *next;
}


l_move_call_ext_38: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[536].counter++;
#endif


r0 = rs[4];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[537].counter++;
#endif


r0 = sp[2];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

catch_end_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[538].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t y = ip[1];
assert(is_slot(y));
sp[slot_index(y)+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	assert(rs[1] == A_ERROR || rs[1] == A_THROW || rs[1] == A_EXIT);
	if (rs[1] == A_THROW)
		r0 = rs[2];
	else
	{
		// A very nice spot for a garbage collection:
		// a few live registers and a portion of the
		// stack just gone.
		//
		
		swap_out();
		proc_burn_fat(proc, 0, rs, 3);

		// Both error and exit exceptions are converted to {'EXIT',Reason}
	
		if (rs[1] == A_ERROR)
			rs[2] = heap_tuple2(&proc->hp, rs[2], proc->stack_trace);
		rs[0] = heap_tuple2(&proc->hp, AEXIT__, rs[2]);
		swap_in();
	}
}




ip += 2;
goto *next;
}


self_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[539].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = proc->pid;
		else
			rs[reg__] = proc->pid;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = proc->pid;
	}
}



ip += 2;
goto *next;
}


l_is_eq_exact_immed_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[540].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[11] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_105: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[541].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+526);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[542].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+527);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_is_eq_exact_immed_30: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[543].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[7] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[544].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+528);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_last_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[545].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 8+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

test_arity_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[546].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t src = sp[((ip[2] >> 0) & 255)+1];
assert(is_tuple(src));
if (*peel_tuple(src) != ((ip[2] >> 8) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
tmp_arg1 = shrink_ptr(peel_tuple(src)+1);	// prepare for extract_element


ip += 3;
goto *next;
}


is_boolean_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[547].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (v != A_FALSE && v != A_TRUE)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_allocate_zero_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[548].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
int gap = ip[1]+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = ip[1];
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 2;
goto *next;
}


l_call_ext_26: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[549].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+529);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_47: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[550].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+530);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_list_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[551].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t l = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_list(l))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


is_nonempty_list_15: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[552].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[14]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_is_eq_exact_literal_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[553].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t a = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
term_t b = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
if (a != b && !are_terms_equal(a, b, 1))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 4;
goto *next;
}


l_move_call_ext_only_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[554].counter++;
#endif


r0 = rs[3];
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

self_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[555].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[2] = proc->pid;


ip += 1;
goto *next;
}


l_call_ext_34: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[556].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+531);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_is_eq_exact_immed_24: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[557].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[15] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_17: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[558].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+532);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_function_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[559].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_boxed(v) || (boxed_tag(peel_boxed(v)) != SUBTAG_FUN &&
					 boxed_tag(peel_boxed(v)) != SUBTAG_EXPORT))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_move_call_15: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[560].counter++;
#endif


r0 = tag_int(0);
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_fetch_19: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[561].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = sp[1];
tmp_arg2 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


move_return_28: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[562].counter++;
#endif


r0 = A_NONE;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

node_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[563].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = cluster_node;
		else
			rs[reg__] = cluster_node;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = cluster_node;
	}
}



ip += 2;
goto *next;
}


l_call_ext_45: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[564].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+533);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_bor_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[565].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
	r = tmp_arg1 | tmp_arg2;	//tag intact, never a carry
else
{
	light_swap_out();
	r = mixed_bor(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_BOR, tmp_arg1, tmp_arg2);
	}
}
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 3;
goto *next;
}


move_return_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[566].counter++;
#endif


r0 = tag_int(1);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

extract_next_element3_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[567].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t *dst;
term_t dst__ = ip[1];
if (is_reg(dst__))
{
	int reg__ = reg_index(dst__);
	assert(reg__ != 0);
	dst = rs + reg__;
}
else
{
	assert(is_slot(dst__));
	dst = sp + slot_index(dst__)+1;
}


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 2;
goto *next;
}


l_call_ext_82: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[568].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+534);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_nonempty_list_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[569].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[11]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_atom_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[570].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_atom(rs[4]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_call_ext_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[571].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+535);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_apply_fun_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[572].counter++;
#endif


uint32_t *saved_ip = ip;
if (!is_boxed(r0))
	goto l_apply_fun_0_bad_fun;
uint32_t *p = peel_boxed(r0);
term_t args = rs[1];
if (!is_list(args))
	badarg();
int arity = list_len(args);
if (arity < 0 || arity > 255)
	goto l_apply_fun_0_bad_fun;
if (boxed_tag(p) == SUBTAG_FUN)
{
	t_fun_t *fun = (t_fun_t *)p;
	if (fun->fe == 0)
		not_implemented("unloaded funs");
	int num_free = fun_num_free(p);
	if (fun_arity(p) != arity + num_free)
		goto l_apply_fun_0_bad_arity;
	memcpy(rs+arity, fun->frozen, num_free*sizeof(term_t));
	ip = fun->fe->entry;
	heap_list_to_vector(args, rs);
	r0 = rs[0];
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->is_bif)
		not_implemented("bifs as exports");
	if (exp->e->entry == 0)
	{
		r0 = exp->e->module;
		rs[1] = exp->e->function;
		rs[2] = args;
		ip = (EH_UNDEF_EXP)->entry;
	}
	else if (exp->e->arity != arity)
	{
l_apply_fun_0_bad_arity:
		light_swap_out();
		term_t fun_args = heap_tuple2(&proc->hp, r0, args);
		term_t reason = heap_tuple2(&proc->hp, A_BADARITY, fun_args);
		light_swap_in();
		raise_error(reason);
	}
	else
	{
		ip = exp->e->entry;
		heap_list_to_vector(args, rs);
		r0 = rs[0];
	}
}
else
{
l_apply_fun_0_bad_fun:
	light_swap_out();
	term_t reason = heap_tuple2(&proc->hp, A_BADFUN, r0);
	light_swap_in();
	raise_error(reason);
}


cp = saved_ip + 0 +1;
local_reduce();
}

l_call_ext_109: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[573].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+536);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element2_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[574].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 13;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


move_return_16: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[575].counter++;
#endif


r0 = tag_int(7);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

is_nil_15: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[576].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[13] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_call_ext_last_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[577].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 3+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_allocate_zero_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[578].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 9+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = 9;
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 1;
goto *next;
}


try_end_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[579].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[2+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	r0 = rs[1];		// class
	rs[1] = rs[2];  // reason

	//TODO: consider gc round here
}


ip += 1;
goto *next;
}


l_fetch_15: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[580].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
tmp_arg2 = rs[4];


ip += 2;
goto *next;
}


l_fast_element_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[581].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t tuple = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (!is_tuple(tuple))
	badarg();
uint32_t *data = peel_tuple(tuple);
uint32_t pos = 2;
if (pos > data[0])
	badarg();
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = data[pos];
		else
			rs[reg__] = data[pos];
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = data[pos];
	}
}



ip += 3;
goto *next;
}


l_allocate_zero_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[582].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 10+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = 10;
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 1;
goto *next;
}


is_nil_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[583].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[11] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_select_val2_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[584].counter++;
#endif


term_t v = rs[7];
if (v == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == (term_t)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[5]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

move_deallocate_return_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[585].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = demasquerade_pointer(sp[0]);
sp += 2+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

extract_next_element_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[586].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[10] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


call_bif_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[587].counter++;
#endif


// r0 - reason
light_swap_out();
proc->stack_trace = noval;
rs[2] = r0;
rs[1] = A_THROW;
goto exception;
}

l_call_ext_64: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[588].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+537);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element3_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[589].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 7;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_move_call_ext_22: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[590].counter++;
#endif


r0 = A_LOGLEVEL;
cp = ip + 1;
export_t *exp = (preloaded_exports+538);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

call_bif_33: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[591].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_pid_to_list1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_call_ext_74: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[592].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+539);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_nil_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[593].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[12] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_move_call_ext_only_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[594].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
export_t *exp = (preloaded_exports+343);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

init_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[595].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[8] = nil;


ip += 1;
goto *next;
}


is_tuple_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[596].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_tuple(rs[7]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_call_ext_31: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[597].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+540);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

try_end_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[598].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[6+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	r0 = rs[1];		// class
	rs[1] = rs[2];  // reason

	//TODO: consider gc round here
}


ip += 1;
goto *next;
}


is_atom_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[599].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (!is_atom((term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2])))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


set_tuple_element_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[600].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t tuple = r0;
assert(is_tuple(tuple));
(peel_tuple(tuple))[ip[2]+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 3;
goto *next;
}


put_list_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[601].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = r0;
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_cons(htop);
		else
			rs[reg__] = tag_cons(htop);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_cons(htop);
	}
}

htop += 2;


ip += 3;
goto *next;
}


l_call_ext_last_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[602].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 4+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_increment_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[603].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
int i = (int)((ip[2] >> 0) & 255);
term_t src = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
term_t r;
if (is_int(src))
{
	int v = int_value(src) + i;
	if (likely(fits_int(v)))
		r = tag_int(v);
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		r = tag_boxed(bn);
	}
}
else
{
	light_swap_out();
	r = mixed_add_immed(src, i, &proc->hp);
	light_swap_in();
	if (is_atom(r))
		bif_error2(r, A_ERLANG, APLUS__, src, tag_int(i));
}
sp[((ip[2] >> 16) & 255)+1] = r;


ip += 3;
goto *next;
}


is_atom_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[604].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_atom(rs[5]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_is_eq_exact_immed_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[605].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[12] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_55: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[606].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+541);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

call_bif_35: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[607].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_unlink1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


call_bif_23: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[608].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_minusminus2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_move_call_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[609].counter++;
#endif


r0 = sp[7];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

move_return_36: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[610].counter++;
#endif


r0 = tag_int(32);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_move_call_only_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[611].counter++;
#endif


r0 = rs[8];
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_call_last_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[612].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 10+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

bif1_body_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[613].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r = ((bif_func1_t)(bif_func_t)expand_ptr(ip[1]))((term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]), proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
rs[1] = r;


ip += 3;
goto *next;
}


init_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[614].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[9] = nil;


ip += 1;
goto *next;
}


call_bif_20: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[615].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_keysearch3;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_is_eq_exact_immed_36: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[616].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
if ((term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]) != (term_t)ip[3])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 4;
goto *next;
}


l_call_ext_42: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[617].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+542);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element_21: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[618].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[7] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_int_bnot_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[619].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t t = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
term_t r;
if (is_int(t))
	r = tag_int(~int_value(t));
else
{
	light_swap_out();
	r = mixed_bnot(t, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error1(r, A_ERLANG, A_BNOT, t);
	}
}
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 5;
goto *next;
}


deallocate_return_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[620].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 8+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

call_bif_34: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[621].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_iolist_to_binary1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_put_tuple_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[622].counter++;
#endif


uint32_t arity = ((ip[1] >> 8) & 255);
term_t tuple;
if (unlikely(arity == 0))
	tuple = ZERO_TUPLE;
else
{
	tuple = tag_tuple(htop);
	*htop++ = arity;

	term_t *pe = (term_t *)ip + 2;
	int n = arity;
	while (n-- > 0)
	{
		if (is_reg(*pe))
		{
			int ri = reg_index(*pe);
			if (ri == 0)
				*htop++ = r0;
			else
				*htop++ = rs[ri];
		}
		else if (is_slot(*pe))
		{
			int si = slot_index(*pe);
			*htop++ = sp[si+1];
		}
		else
			*htop++ = *pe;

		pe++;
	}
}
sp[((ip[1] >> 0) & 255)+1] = tuple;

ip += arity+2;
next();
}

node_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[623].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
r0 = cluster_node;


ip += 1;
goto *next;
}


l_is_eq_exact_immed_25: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[624].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[16] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_move_call_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[625].counter++;
#endif


r0 = rs[5];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

bif2_body_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[626].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t r = ((bif_func2_t)(bif_func_t)expand_ptr(ip[1]))(tmp_arg1, tmp_arg2, proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
rs[2] = r;


ip += 2;
goto *next;
}


init_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[627].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[11] = nil;


ip += 1;
goto *next;
}


extract_next_element2_17: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[628].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t *dst;
term_t dst__ = ip[1];
if (is_reg(dst__))
{
	int reg__ = reg_index(dst__);
	assert(reg__ != 0);
	dst = rs + reg__;
}
else
{
	assert(is_slot(dst__));
	dst = sp + slot_index(dst__)+1;
}


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 2;
goto *next;
}


l_new_bs_put_integer_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[629].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t sz = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
uint32_t bsz;
if (bits_calc_bit_size(sz, ((ip[4] >> 0) & 255), &bsz) < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

assert(bpc.ends-bpc.starts >= bsz);
term_t v = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
uint8_t flags = ((ip[4] >> 8) & 255);
if (is_int(v))
{
	light_swap_out();
	bits_put_integer(&bpc, int_value(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else if (is_boxed(v) && is_bignum(peel_boxed(v)))
{
	light_swap_out();
	bits_put_bignum(&bpc, (bignum_t *)peel_boxed(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 5;
goto *next;
}


l_move_call_16: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[630].counter++;
#endif


r0 = tag_int(2);
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_ext_36: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[631].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+543);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_tuple_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[632].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_tuple(rs[6]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


extract_next_element2_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[633].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 11;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


is_nil_28: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[634].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if ((term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]) != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


put_list_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[635].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = sp[((ip[2] >> 0) & 255)+1];
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_cons(htop);
	else
		rs[reg__] = tag_cons(htop);
}

htop += 2;


ip += 3;
goto *next;
}


get_list_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[636].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
assert(is_cons(sp[((ip[1] >> 0) & 255)+1]));
term_t *pair = peel_cons(sp[((ip[1] >> 0) & 255)+1]);
{
	int reg__ = ((ip[1] >> 8) & 255);
	if (reg__ == 0)
		r0 = pair[0];
	else
		rs[reg__] = pair[0];
}

{
	int reg__ = ((ip[1] >> 16) & 255);
	if (reg__ == 0)
		r0 = pair[1];
	else
		rs[reg__] = pair[1];
}



ip += 2;
goto *next;
}


l_bs_get_integer_16_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[637].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends - mc->bs.starts < 16)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t vh, vl;
bits_get_octet(&mc->bs, vh);
bits_get_octet(&mc->bs, vl);
uint32_t v = (vh << 8) | vl;
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_int(v);
	else
		rs[reg__] = tag_int(v);
}



ip += 3;
goto *next;
}


catch_end_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[638].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[4+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	assert(rs[1] == A_ERROR || rs[1] == A_THROW || rs[1] == A_EXIT);
	if (rs[1] == A_THROW)
		r0 = rs[2];
	else
	{
		// A very nice spot for a garbage collection:
		// a few live registers and a portion of the
		// stack just gone.
		//
		
		swap_out();
		proc_burn_fat(proc, 0, rs, 3);

		// Both error and exit exceptions are converted to {'EXIT',Reason}
	
		if (rs[1] == A_ERROR)
			rs[2] = heap_tuple2(&proc->hp, rs[2], proc->stack_trace);
		rs[0] = heap_tuple2(&proc->hp, AEXIT__, rs[2]);
		swap_in();
	}
}




ip += 1;
goto *next;
}


extract_next_element_17: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[639].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[11] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


try_end_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[640].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t y = ip[1];
assert(is_slot(y));
sp[slot_index(y)+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	r0 = rs[1];		// class
	rs[1] = rs[2];  // reason

	//TODO: consider gc round here
}


ip += 2;
goto *next;
}


call_bif_43: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[641].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_spawn_link1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


is_tuple_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[642].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_tuple(rs[5]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_call_ext_44: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[643].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+544);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[644].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+545);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_trim_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[645].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t masqueraded_cp = sp[0];
sp += 9;
sp[0] = masqueraded_cp;


ip += 1;
goto *next;
}


put_list_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[646].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
htop[0] = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
htop[1] = sp[((ip[1] >> 8) & 255)+1];
sp[((ip[1] >> 16) & 255)+1] = tag_cons(htop);
htop += 2;


ip += 2;
goto *next;
}


l_is_eq_exact_literal_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[647].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t a = rs[6];
term_t b = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (a != b && !are_terms_equal(a, b, 1))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_move_call_ext_44: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[648].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+532);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_get_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[649].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r = lookup_process_dictionary((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]), proc->dictionary);
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 3;
goto *next;
}


call_bif_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[650].counter++;
#endif


// r0 - reason
light_swap_out();
proc->stack_trace = noval;
rs[2] = r0;
rs[1] = A_EXIT;
goto exception;
}

l_call_ext_91: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[651].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+546);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_is_ne_exact_immed_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[652].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[1] == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


is_nil_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[653].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (sp[2] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_move_call_ext_37: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[654].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+332);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

node_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[655].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[2] = cluster_node;


ip += 1;
goto *next;
}


is_nil_17: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[656].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[15] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_33: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[657].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(sp[9]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_is_eq_exact_immed_28: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[658].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[17] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_61: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[659].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+547);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_trim_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[660].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t masqueraded_cp = sp[0];
sp += 7;
sp[0] = masqueraded_cp;


ip += 1;
goto *next;
}


is_nil_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[661].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[14] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


move_jump_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[662].counter++;
#endif


r0 = sp[2];
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

move_jump_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[663].counter++;
#endif


r0 = sp[3];
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

move_jump_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[664].counter++;
#endif


r0 = rs[2];
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

move_return_19: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[665].counter++;
#endif


r0 = A_UNDEFINED;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

bs_context_to_binary_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[666].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = r0;
if (is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX)
{
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

	light_swap_out();
	uint32_t *p = heap_alloc(&proc->hp, WSIZE(t_sub_bin_t));
	t_sub_bin_t *sb = (t_sub_bin_t *)p;

	// Use the offset from the slot zero as a better start
	int64_t starts = mc->saved_offsets[0];

	box_sub_bin(p, mc->parent, starts, mc->bs.ends, 0);
	heap_set_top(&proc->hp, p);
	light_swap_in();

	r0 = tag_boxed(sb);
}


ip += 1;
goto *next;
}


badmatch_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[667].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, r0);
light_swap_in();
raise_error(reason);
}

is_nonempty_list_35: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[668].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(sp[8]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_19: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[669].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(sp[2]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_move_call_25: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[670].counter++;
#endif


r0 = rs[6];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

bif1_body_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[671].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t r = ((bif_func1_t)bif_hd1)((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]), proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
rs[2] = r;


ip += 2;
goto *next;
}


bif1_body_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[672].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t r = ((bif_func1_t)bif_hd1)(r0, proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
rs[1] = r;


ip += 1;
goto *next;
}


bif2_body_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[673].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r = ((bif_func2_t)(bif_func_t)expand_ptr(ip[1]))(tmp_arg1, tmp_arg2, proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 3;
goto *next;
}


is_float_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[674].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_boxed(v) || boxed_tag(peel_boxed(v)) != SUBTAG_FLOAT)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


node_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[675].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[1] = cluster_node;


ip += 1;
goto *next;
}


l_move_call_ext_last_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[676].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
cp = demasquerade_pointer(sp[0]);
sp += 2+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

call_bif_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[677].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_delete1;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


l_call_ext_92: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[678].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+548);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_78: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[679].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+549);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_36: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[680].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+550);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

move_jump_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[681].counter++;
#endif


r0 = A_OK;
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

move_return_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[682].counter++;
#endif


r0 = tag_int(2);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_bs_test_unit_8_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[683].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = rs[3];
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (((mc->bs.ends - mc->bs.starts) & 7) != 0)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


fconv_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[684].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
double converted = term_to_float(v);
if (!isfinite(converted))
	raise_error(A_BADARITH);
fr[((ip[2] >> 0) & 255)] = converted;


ip += 3;
goto *next;
}


deallocate_return_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[685].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 11+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_call_ext_only_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[686].counter++;
#endif


export_t *exp = (preloaded_exports+551);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

move_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[687].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[8] = tag_int(4096);


ip += 1;
goto *next;
}


l_move_call_ext_28: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[688].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+552);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_40: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[689].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+553);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_17: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[690].counter++;
#endif


r0 = tag_int(3);
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_ext_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[691].counter++;
#endif


r0 = tag_int(0);
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element_24: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[692].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = *(term_t *)expand_ptr(tmp_arg1);
		else
			rs[reg__] = *(term_t *)expand_ptr(tmp_arg1);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = *(term_t *)expand_ptr(tmp_arg1);
	}
}

tmp_arg1 += sizeof(term_t);


ip += 2;
goto *next;
}


l_is_ne_exact_immed_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[693].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[4] == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_trim_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[694].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t masqueraded_cp = sp[0];
sp += 8;
sp[0] = masqueraded_cp;


ip += 1;
goto *next;
}


l_call_fun_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[695].counter++;
#endif


uint32_t *saved_ip = ip +1 +1;

 
rs[0] = r0;
uint32_t arity = ((ip[1] >> 0) & 255);
term_t t = rs[arity];		//what about {x,1023} mapped to {x,255}?
if (!is_boxed(t))
	goto l_call_fun_4_bad_fun;
uint32_t *p = peel_boxed(t);
if (boxed_tag(p) == SUBTAG_FUN)
{
	t_fun_t *fun = (t_fun_t *)p;
	if (fun->fe == 0)
		fatal_error("unloaded funs not implemented");
	int num_free = fun_num_free(p);
	if (fun_arity(p) != arity+num_free)
		goto l_call_fun_4_bad_arity;
	ip = fun->fe->entry;
	term_t *src = fun->frozen;
	term_t *dst = rs + arity;
	int n = num_free;
	while (n-- > 0)
		*dst++ = *src++;
	r0 = rs[0];
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->is_bif)
	{
		swap_out();
		term_t r = invoke_bif(exp->e, proc, rs, arity +1);		// +1 for export
		swap_in();
		if (r == noval)
		{
			raise_bif_mfa.mod = exp->e->module;
			raise_bif_mfa.fun = exp->e->function;
			raise_bif_mfa.arity = exp->e->arity;
			rs[exp->e->arity] = proc->bif_excep_reason;
			goto raise_from_bif;
		}
		r0 = r;
		ip = saved_ip;


		next();
	}
	if (exp->e->entry == 0)
	{
		//rs[0] = r0;
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		light_swap_in();
		r0 = exp->e->module;
		rs[1] = exp->e->function;
		rs[2] = args;
		ip = (EH_UNDEF_EXP)->entry;
	}
	else if (exp->e->arity != arity)
	{
l_call_fun_4_bad_arity:
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		term_t fun_args = heap_tuple2(&proc->hp, t, args);
		term_t reason = heap_tuple2(&proc->hp, A_BADARITY, fun_args);
		light_swap_in();
		raise_error(reason);
	}
	else
		ip = exp->e->entry;
}
else
{
l_call_fun_4_bad_fun:
	light_swap_out();
	term_t reason = heap_tuple2(&proc->hp, A_BADFUN, t);
	light_swap_in();
	raise_error(reason);
}
cp = saved_ip;
local_reduce();


}



l_apply_fun_only_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[696].counter++;
#endif


if (!is_boxed(r0))
	goto l_apply_fun_only_0_bad_fun;
uint32_t *p = peel_boxed(r0);
term_t args = rs[1];
if (!is_list(args))
	badarg();
int arity = list_len(args);
if (arity < 0 || arity > 255)
	goto l_apply_fun_only_0_bad_fun;
if (boxed_tag(p) == SUBTAG_FUN)
{
	t_fun_t *fun = (t_fun_t *)p;
	if (fun->fe == 0)
		not_implemented("unloaded funs");
	int num_free = fun_num_free(p);
	if (fun_arity(p) != arity + num_free)
		goto l_apply_fun_only_0_bad_arity;
	memcpy(rs+arity, fun->frozen, num_free*sizeof(term_t));
	ip = fun->fe->entry;
	heap_list_to_vector(args, rs);
	r0 = rs[0];
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->is_bif)
		not_implemented("bifs as exports");
	if (exp->e->entry == 0)
	{
		r0 = exp->e->module;
		rs[1] = exp->e->function;
		rs[2] = args;
		ip = (EH_UNDEF_EXP)->entry;
	}
	else if (exp->e->arity != arity)
	{
l_apply_fun_only_0_bad_arity:
		light_swap_out();
		term_t fun_args = heap_tuple2(&proc->hp, r0, args);
		term_t reason = heap_tuple2(&proc->hp, A_BADARITY, fun_args);
		light_swap_in();
		raise_error(reason);
	}
	else
	{
		ip = exp->e->entry;
		heap_list_to_vector(args, rs);
		r0 = rs[0];
	}
}
else
{
l_apply_fun_only_0_bad_fun:
	light_swap_out();
	term_t reason = heap_tuple2(&proc->hp, A_BADFUN, r0);
	light_swap_in();
	raise_error(reason);
}


local_reduce();
}

l_move_call_ext_26: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[697].counter++;
#endif


r0 = A_LINC;
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_25: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[698].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+554);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_bs_skip_bits_all2_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[699].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = rs[3];
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint8_t u = 8;
uint32_t skipped = mc->bs.ends - mc->bs.starts;
if (skipped % u != 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
mc->bs.starts = mc->bs.ends;


ip += 2;
goto *next;
}


l_call_ext_98: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[700].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+555);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[701].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+556);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

move_return_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[702].counter++;
#endif


r0 = tag_int(3);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

bs_context_to_binary_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[703].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX)
{
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

	light_swap_out();
	uint32_t *p = heap_alloc(&proc->hp, WSIZE(t_sub_bin_t));
	t_sub_bin_t *sb = (t_sub_bin_t *)p;

	// Use the offset from the slot zero as a better start
	int64_t starts = mc->saved_offsets[0];

	box_sub_bin(p, mc->parent, starts, mc->bs.ends, 0);
	heap_set_top(&proc->hp, p);
	light_swap_in();

	{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(sb);
		else
			rs[reg__] = tag_boxed(sb);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(sb);
	}
}

}


ip += 2;
goto *next;
}


is_nonempty_list_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[704].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[13]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_move_call_ext_only_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[705].counter++;
#endif


r0 = rs[1];
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

bif1_body_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[706].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r = ((bif_func1_t)(bif_func_t)expand_ptr(ip[1]))((term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]), proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
r0 = r;


ip += 3;
goto *next;
}


l_move_call_ext_46: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[707].counter++;
#endif


r0 = tag_int(2);
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

test_heap_1_put_list_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[708].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (unlikely(hend - htop < ip[1]))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, 1);
		heap_ensure(&proc->hp, ip[1], root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, ip[1]);
	swap_in();
}


term_t hd = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
term_t tl = r0;
r0 = tag_cons(htop);
*htop++ = hd;
*htop++ = tl;


ip += 3;
goto *next;
}


self_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[709].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[3] = proc->pid;


ip += 1;
goto *next;
}


l_call_ext_88: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[710].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+557);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_86: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[711].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+558);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element_19: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[712].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[13] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_fadd_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[713].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
double d = fr[((ip[1] >> 0) & 255)] + fr[((ip[1] >> 8) & 255)];
if (!isfinite(d))
	raise_error(A_BADARITH);
fr[((ip[1] >> 16) & 255)] = d;


ip += 2;
goto *next;
}


extract_next_element2_16: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[714].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 15;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


move_jump_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[715].counter++;
#endif


r0 = tag_int(0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

move_return_45: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[716].counter++;
#endif


r0 = tag_int(22);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_29: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[717].counter++;
#endif


r0 = tag_int(64);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[718].counter++;
#endif


r0 = tag_int(8);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_deallocate_return_8: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[719].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = demasquerade_pointer(sp[0]);
sp += 5+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

is_bigint_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[720].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_boxed(v) || (*peel_boxed(v) & SUBTAG_BIGNUM_MASK) != SUBTAG_BIGNUM)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


fmove_2_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[721].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
	// fr s
double dbl = fr[((ip[1] >> 0) & 255)];
term_t v = tag_boxed(htop);
assert(hend - htop >= WSIZE(t_float_t));	// compiler should care
((t_float_t *)htop)->hdr = HDR_IS_NOT_CP | SUBTAG_FLOAT;
((t_float_t *)htop)->val = dbl;
htop += WSIZE(t_float_t);
r0 = v;


ip += 2;
goto *next;
}


fmove_1_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[722].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
	// s fr
term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(v) && boxed_tag(peel_boxed(v)) == SUBTAG_FLOAT);
uint32_t *tdata = peel_boxed(v);
fr[((ip[2] >> 0) & 255)] = float_value(tdata);


ip += 3;
goto *next;
}


l_move_call_last_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[723].counter++;
#endif


r0 = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
cp = demasquerade_pointer(sp[0]);
sp += ip[2]+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_is_ne_exact_immed_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[724].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[3] == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_fast_element_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[725].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t tuple = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (!is_tuple(tuple))
	badarg();
uint32_t *data = peel_tuple(tuple);
uint32_t pos = ip[2];
if (pos > data[0])
	badarg();
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = data[pos];
		else
			rs[reg__] = data[pos];
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = data[pos];
	}
}



ip += 4;
goto *next;
}


is_nonempty_list_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[726].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(sp[3]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_is_eq_exact_literal_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[727].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t a = rs[4];
term_t b = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (a != b && !are_terms_equal(a, b, 1))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


test_heap_1_put_list_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[728].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (unlikely(hend - htop < ((ip[1] >> 0) & 255)))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, 1);
		heap_ensure(&proc->hp, ((ip[1] >> 0) & 255), root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, ((ip[1] >> 0) & 255));
	swap_in();
}


term_t hd = sp[((ip[1] >> 8) & 255)+1];
term_t tl = r0;
r0 = tag_cons(htop);
*htop++ = hd;
*htop++ = tl;


ip += 2;
goto *next;
}


l_call_ext_84: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[729].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+397);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_75: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[730].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+559);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_62: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[731].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+560);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_58: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[732].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+315);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_48: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[733].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+561);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_35: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[734].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+380);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[735].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+495);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element2_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[736].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 12;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


is_nil_20: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[737].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (sp[1] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_bs_put_string_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[738].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
bits_t src;
bits_init_buf((uint8_t *)expand_ptr(ip[1]), 1, &src);
bits_copy(&src, &bpc);


ip += 2;
goto *next;
}


is_list_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[739].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t l = rs[5];
if (!is_list(l))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_22: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[740].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(sp[5]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


get_list_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[741].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
assert(is_cons((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1])));
term_t *pair = peel_cons((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]));
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = pair[0];
		else
			rs[reg__] = pair[0];
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = pair[0];
	}
}

{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = pair[1];
		else
			rs[reg__] = pair[1];
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = pair[1];
	}
}



ip += 4;
goto *next;
}


l_move_call_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[742].counter++;
#endif


r0 = tag_int(1);
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_ext_24: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[743].counter++;
#endif


r0 = sp[8];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_16: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[744].counter++;
#endif


r0 = sp[6];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_is_eq_exact_immed_29: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[745].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[18] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_77: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[746].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+562);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_select_val_atoms_2: 
 {

// NB: self-modifying code: for dynamically loaded modules
// l_select_val_atoms iop points to &&l_select_val_atoms_N;
// after sorting the list it changes to
// &&l_select_val_atoms_N_sorted; statically loaded modules
// use the second label as the list is already in order.

	uint32_t nr_pairs = (((ip[2] >> 8) & 255))/2;
	qsort(ip + 3,
		nr_pairs, 2*sizeof(uint32_t), select_val_atoms_compare);
	
	*ip = shrink_ptr(&&l_select_val_atoms_2_sorted);
}

l_select_val_atoms_2_sorted: {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[747].counter++;
#endif



term_t v = sp[((ip[2] >> 0) & 255)+1];
pair_t *alpha = (pair_t *)(ip + 3);
pair_t *beta = (pair_t *)((uint32_t *)alpha + ((ip[2] >> 8) & 255));

if (v < alpha->f || v > beta[-1].f)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

//TODO: in BEAM the binary search parameters are
// made into unsigned values for performance

while (beta > alpha+1)	// at least 2 pairs
{
	pair_t *mid = alpha + (beta - alpha +1)/2;
	if ((term_t)mid->f > v)
		beta = mid;
	else
		alpha = mid;
}

assert(beta == alpha+1);
if (alpha->f == v)
{
	ip = (uint32_t *)expand_ptr(alpha->s);
	next();
}

do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


}

is_nonempty_list_21: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[748].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[18]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_binary_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[749].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_boxed(v))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t *p = peel_boxed(v);
switch (boxed_tag(p))
{
case SUBTAG_PROC_BIN:
	break;
case SUBTAG_HEAP_BIN:
	break;
case SUBTAG_MATCH_CTX:
{
	t_match_ctx_t *ctx = (t_match_ctx_t *)p;
	if ((ctx->bs.ends-ctx->bs.starts) % 8 != 0)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
	break;
}
case SUBTAG_SUB_BIN:
{
	t_sub_bin_t *sb = (t_sub_bin_t *)p;
	if ((sb->ends - sb->starts) % 8 == 0)
		break;
	/* fall through */
}
default:
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 3;
goto *next;
}


l_move_call_ext_only_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[750].counter++;
#endif


r0 = A_TYPE;
export_t *exp = (preloaded_exports+556);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_17: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[751].counter++;
#endif


r0 = A_ERROR;
cp = ip + 1;
export_t *exp = (preloaded_exports+563);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

init_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[752].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[12] = nil;


ip += 1;
goto *next;
}


l_get_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[753].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t r = lookup_process_dictionary((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]), proc->dictionary);
rs[2] = r;


ip += 2;
goto *next;
}


l_bs_test_zero_tail2_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[754].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = rs[3];
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends > mc->bs.starts)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_bs_get_integer_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[755].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
//tmp_arg1: MatchCtx
//tmp_arg2: Sz
assert(is_boxed(tmp_arg1) && boxed_tag(peel_boxed(tmp_arg1)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(tmp_arg1);
if (!is_int(tmp_arg2) && !(is_boxed(tmp_arg2) && is_bignum(peel_boxed(tmp_arg2))))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
uint32_t bcount;
if (bits_calc_bit_size(tmp_arg2, ((ip[2] >> 8) & 255), &bcount) < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
if (mc->bs.ends - mc->bs.starts < bcount)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
uint8_t flags = ((ip[2] >> 16) & 255);
light_swap_out();
term_t v = bits_bs_get_integer_imm(mc,
	bcount, flags & BSF_SIGNED, flags & BSF_LITTLE, &proc->hp);
light_swap_in();
{
	int reg__ = ((ip[2] >> 24) & 255);
	if (reg__ == 0)
		r0 = v;
	else
		rs[reg__] = v;
}



ip += 3;
goto *next;
}


func_info_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[756].counter++;
#endif


light_swap_out();
rs[0] = r0;
term_t args = heap_vector_to_list(&proc->hp, rs, ((ip[3] >> 0) & 255));
proc->stack_trace = get_stack_trace(ip, cp,
		sp, proc_stack_bottom(proc),
		0, args, &proc->hp);
rs[2] = A_FUNCTION_CLAUSE;
rs[1] = A_ERROR;
goto exception;
}

extract_next_element_22: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[757].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[18] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_select_val_atoms_3: 
 {

// NB: self-modifying code: for dynamically loaded modules
// l_select_val_atoms iop points to &&l_select_val_atoms_N;
// after sorting the list it changes to
// &&l_select_val_atoms_N_sorted; statically loaded modules
// use the second label as the list is already in order.

	uint32_t nr_pairs = (ip[3])/2;
	qsort(ip + 4,
		nr_pairs, 2*sizeof(uint32_t), select_val_atoms_compare);
	
	*ip = shrink_ptr(&&l_select_val_atoms_3_sorted);
}

l_select_val_atoms_3_sorted: {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[758].counter++;
#endif



term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
pair_t *alpha = (pair_t *)(ip + 4);
pair_t *beta = (pair_t *)((uint32_t *)alpha + ip[3]);

if (v < alpha->f || v > beta[-1].f)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);

//TODO: in BEAM the binary search parameters are
// made into unsigned values for performance

while (beta > alpha+1)	// at least 2 pairs
{
	pair_t *mid = alpha + (beta - alpha +1)/2;
	if ((term_t)mid->f > v)
		beta = mid;
	else
		alpha = mid;
}

assert(beta == alpha+1);
if (alpha->f == v)
{
	ip = (uint32_t *)expand_ptr(alpha->s);
	next();
}

do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);


}

l_trim_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[759].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t masqueraded_cp = sp[0];
sp += 11;
sp[0] = masqueraded_cp;


ip += 1;
goto *next;
}


is_nil_21: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[760].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[17] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_move_call_ext_last_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[761].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
cp = demasquerade_pointer(sp[0]);
sp += 3+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_fsub_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[762].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
double d = fr[((ip[1] >> 0) & 255)] - fr[((ip[1] >> 8) & 255)];
if (!isfinite(d))
	raise_error(A_BADARITH);
fr[((ip[1] >> 16) & 255)] = d;


ip += 2;
goto *next;
}


l_move_call_ext_35: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[763].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+564);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_31: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[764].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+393);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[765].counter++;
#endif


r0 = sp[7];
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

init_15: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[766].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t y = ip[1];
assert(is_slot(y));
sp[slot_index(y)+1] = nil;


ip += 2;
goto *next;
}


l_wait_timeout_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[767].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
uint32_t millis = ip[2];
if (millis == 0)
{
	ip += 3;
	goto *next;
}
proc->result.what = SLICE_RESULT_WAIT;
uint64_t now = monotonic_clock();
proc->result.until_when = now + (uint64_t)millis * 1000000;
proc->result.jump_to = ip + 3;

ip = (uint32_t *)expand_ptr(ip[1]);

proc->cap.live = 0;
light_swap_out();
goto schedule;
}

l_call_last_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[768].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 9+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_is_eq_exact_immed_31: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[769].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[19] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_104: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[770].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+565);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_65: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[771].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+566);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_59: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[772].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+567);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[773].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+568);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_new_bs_put_integer_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[774].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t sz = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
uint32_t bsz;
if (bits_calc_bit_size(sz, 1, &bsz) < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

assert(bpc.ends-bpc.starts >= bsz);
term_t v = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
uint8_t flags = 0;
if (is_int(v))
{
	light_swap_out();
	bits_put_integer(&bpc, int_value(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else if (is_boxed(v) && is_bignum(peel_boxed(v)))
{
	light_swap_out();
	bits_put_bignum(&bpc, (bignum_t *)peel_boxed(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 4;
goto *next;
}


move_return_34: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[775].counter++;
#endif


r0 = tag_int(14);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[776].counter++;
#endif


r0 = tag_int(4);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_trim_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[777].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
uint32_t masqueraded_cp = sp[0];
sp += ip[1];
sp[0] = masqueraded_cp;


ip += 2;
goto *next;
}


l_move_call_only_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[778].counter++;
#endif


r0 = tag_int(1);
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_ext_last_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[779].counter++;
#endif


r0 = A_LAGER_EVENT;
cp = demasquerade_pointer(sp[0]);
sp += 5+1;
export_t *exp = (preloaded_exports+569);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_last_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[780].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
cp = demasquerade_pointer(sp[0]);
sp += 0+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

is_nonempty_list_29: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[781].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[22]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_23: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[782].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[20]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_17: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[783].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[16]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_call_fun_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[784].counter++;
#endif


uint32_t *saved_ip = ip +0 +1;

 
rs[0] = r0;
uint32_t arity = 3;
term_t t = rs[arity];		//what about {x,1023} mapped to {x,255}?
if (!is_boxed(t))
	goto l_call_fun_2_bad_fun;
uint32_t *p = peel_boxed(t);
if (boxed_tag(p) == SUBTAG_FUN)
{
	t_fun_t *fun = (t_fun_t *)p;
	if (fun->fe == 0)
		fatal_error("unloaded funs not implemented");
	int num_free = fun_num_free(p);
	if (fun_arity(p) != arity+num_free)
		goto l_call_fun_2_bad_arity;
	ip = fun->fe->entry;
	term_t *src = fun->frozen;
	term_t *dst = rs + arity;
	int n = num_free;
	while (n-- > 0)
		*dst++ = *src++;
	r0 = rs[0];
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->is_bif)
	{
		swap_out();
		term_t r = invoke_bif(exp->e, proc, rs, arity +1);		// +1 for export
		swap_in();
		if (r == noval)
		{
			raise_bif_mfa.mod = exp->e->module;
			raise_bif_mfa.fun = exp->e->function;
			raise_bif_mfa.arity = exp->e->arity;
			rs[exp->e->arity] = proc->bif_excep_reason;
			goto raise_from_bif;
		}
		r0 = r;
		ip = saved_ip;


		next();
	}
	if (exp->e->entry == 0)
	{
		//rs[0] = r0;
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		light_swap_in();
		r0 = exp->e->module;
		rs[1] = exp->e->function;
		rs[2] = args;
		ip = (EH_UNDEF_EXP)->entry;
	}
	else if (exp->e->arity != arity)
	{
l_call_fun_2_bad_arity:
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		term_t fun_args = heap_tuple2(&proc->hp, t, args);
		term_t reason = heap_tuple2(&proc->hp, A_BADARITY, fun_args);
		light_swap_in();
		raise_error(reason);
	}
	else
		ip = exp->e->entry;
}
else
{
l_call_fun_2_bad_fun:
	light_swap_out();
	term_t reason = heap_tuple2(&proc->hp, A_BADFUN, t);
	light_swap_in();
	raise_error(reason);
}
cp = saved_ip;
local_reduce();


}



l_apply_only_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[785].counter++;
#endif


term_t args = rs[2];
if (!is_list(args))
	badarg();
int arity = list_len(args);
if (arity < 0 || arity > 255)
	badarg();
export_t *exp = code_base_lookup(r0, rs[1], arity);
if (unlikely(exp == 0 || exp->entry == 0))
	exp = EH_UNDEF_EXP;
else
{
	heap_list_to_vector(args, rs);
	r0 = rs[0];

	if (exp->is_bif)
	{
		swap_out();
		term_t r = invoke_bif(exp, proc, rs, arity +2);		// +2 for module, function
		swap_in();
		if (r == noval)
		{
			raise_bif_mfa.mod = exp->module;
			raise_bif_mfa.fun = exp->function;
			raise_bif_mfa.arity = exp->arity;
			rs[exp->arity] = proc->bif_excep_reason;
			goto raise_from_bif;
		}
		r0 = r;
		ip = cp;
		cp = 0;
		next();
	}
}
ip = exp->entry;
local_reduce();
}

l_fetch_21: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[786].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = sp[2];
tmp_arg2 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 2;
goto *next;
}


l_move_call_ext_47: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[787].counter++;
#endif


r0 = A_SCHEMA;
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_19: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[788].counter++;
#endif


r0 = nil;
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_bs_test_zero_tail2_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[789].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = rs[2];
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends > mc->bs.starts)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


call_bif_45: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[790].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_match2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


bs_init_writable_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[791].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t size;
UNUSED int x = bits_calc_byte_size(r0, &size);
assert(x == 0);
light_swap_out();
r0 = bits_bs_init_writable(size, &proc->hp);
light_swap_in();


ip += 1;
goto *next;
}


l_call_ext_39: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[792].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+570);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

extract_next_element_20: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[793].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[14] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


move_jump_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[794].counter++;
#endif


r0 = A_TRUE;
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

move_jump_4: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[795].counter++;
#endif


r0 = rs[4];
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

move_return_47: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[796].counter++;
#endif


r0 = tag_int(19);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_46: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[797].counter++;
#endif


r0 = A_ALL;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_39: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[798].counter++;
#endif


r0 = tag_int(24);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_33: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[799].counter++;
#endif


r0 = tag_int(65535);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_20: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[800].counter++;
#endif


r0 = tag_int(9);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

is_nil_27: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[801].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[20] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nil_22: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[802].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[19] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_bs_put_string_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[803].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
bits_t src;
bits_init_buf((uint8_t *)expand_ptr(ip[2]), ip[1], &src);
bits_copy(&src, &bpc);


ip += 3;
goto *next;
}


l_bs_put_string_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[804].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
bits_t src;
bits_init_buf((uint8_t *)expand_ptr(ip[1]), 6, &src);
bits_copy(&src, &bpc);


ip += 2;
goto *next;
}


put_list_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[805].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
htop[0] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
htop[1] = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_cons(htop);
	else
		rs[reg__] = tag_cons(htop);
}

htop += 2;


ip += 3;
goto *next;
}


is_nonempty_list_18: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[806].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[15]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_increment_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[807].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
int i = (int)ip[2];
term_t src = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
term_t r;
if (is_int(src))
{
	int v = int_value(src) + i;
	if (likely(fits_int(v)))
		r = tag_int(v);
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		r = tag_boxed(bn);
	}
}
else
{
	light_swap_out();
	r = mixed_add_immed(src, i, &proc->hp);
	light_swap_in();
	if (is_atom(r))
		bif_error2(r, A_ERLANG, APLUS__, src, tag_int(i));
}
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 5;
goto *next;
}


l_times_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[808].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int64_t v = (int64_t)int_value(tmp_arg1) * int_value(tmp_arg2);
	if (likely(fits_int(v)))
	{
		{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_int(v);
		else
			rs[reg__] = tag_int(v);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_int(v);
	}
}

	}
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(bn);
		else
			rs[reg__] = tag_boxed(bn);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(bn);
	}
}

	}
}
else
{
	light_swap_out();
	term_t r = mixed_mul(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, ATIMES__, tmp_arg1, tmp_arg2);
	}
	{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}

}


ip += 4;
goto *next;
}


l_bs_get_integer_imm_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[809].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
//NB: Live not used
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ((ip[3] >> 0) & 255);
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
uint8_t flags = ((ip[3] >> 16) & 255);
light_swap_out();
term_t v = bits_bs_get_integer_imm(mc,
	bcount, flags & BSF_SIGNED, flags & BSF_LITTLE, &proc->hp);
light_swap_in();
{
	int reg__ = ((ip[3] >> 24) & 255);
	if (reg__ == 0)
		r0 = v;
	else
		rs[reg__] = v;
}



ip += 4;
goto *next;
}


l_move_call_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[810].counter++;
#endif


r0 = nil;
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_get_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[811].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t r = lookup_process_dictionary(A_MNESIA_ACTIVITY_STATE, proc->dictionary);
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 2;
goto *next;
}


call_bif_24: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[812].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_re_run3;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


call_bif_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[813].counter++;
#endif


// r0 - reason
light_swap_out();
proc->stack_trace = noval;
rs[2] = r0;
rs[1] = A_ERROR;
goto exception;
}

l_is_eq_exact_immed_34: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[814].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[23] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_move_call_last_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[815].counter++;
#endif


r0 = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
cp = demasquerade_pointer(sp[0]);
sp += 4+1;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_call_ext_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[816].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+571);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_is_ne_exact_immed_7: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[817].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[2] == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


move_jump_5: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[818].counter++;
#endif


r0 = sp[1];
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

move_return_40: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[819].counter++;
#endif


r0 = A_DELETE;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_trim_9: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[820].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
uint32_t masqueraded_cp = sp[0];
sp += 10;
sp[0] = masqueraded_cp;


ip += 1;
goto *next;
}


bs_context_to_binary_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[821].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = rs[1];
if (is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX)
{
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

	light_swap_out();
	uint32_t *p = heap_alloc(&proc->hp, WSIZE(t_sub_bin_t));
	t_sub_bin_t *sb = (t_sub_bin_t *)p;

	// Use the offset from the slot zero as a better start
	int64_t starts = mc->saved_offsets[0];

	box_sub_bin(p, mc->parent, starts, mc->bs.ends, 0);
	heap_set_top(&proc->hp, p);
	light_swap_in();

	rs[1] = tag_boxed(sb);
}


ip += 1;
goto *next;
}


is_nonempty_list_30: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[822].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[26]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_27: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[823].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[24]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_make_export_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[824].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);

//
// TODO:
//
// - remove translation of all erlang:make_fun/3 call to l_make_export
// - add erlang:make_fun3 BIF
// - remove {e,0} as a possible iop argument from static and dynamic code
// loading
// - assert that e != 0 here
//

export_t *e = (export_t *)expand_ptr(ip[1]);
if (e == 0)
{
	e = code_base_lookup_or_create_N(r0, rs[1], int_value(rs[2]));
	if (e == 0)
		raise_error(A_NO_MEMORY);
}

light_swap_out();
int needed = WSIZE(t_export_t);
uint32_t *p = heap_alloc(&proc->hp, needed);
term_t fun = tag_boxed(p);
box_export(p, e);
heap_set_top(&proc->hp, p);
light_swap_in();
r0 = fun;


ip += 2;
goto *next;
}


l_select_val2_11: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[825].counter++;
#endif


term_t v = sp[2];
if (v == (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
if (v == (term_t)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[5]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

is_number_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[826].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_int(v))
{
	if (!is_boxed(v))
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
	uint32_t bt = boxed_tag(peel_boxed(v));
	if (bt != SUBTAG_POS_BIGNUM &&
		bt != SUBTAG_NEG_BIGNUM &&
		bt != SUBTAG_FLOAT)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}


ip += 3;
goto *next;
}


move_deallocate_return_10: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[827].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = demasquerade_pointer(sp[0]);
sp += ip[2]+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_move_call_28: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[828].counter++;
#endif


r0 = A_SCHEMA;
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[829].counter++;
#endif


r0 = sp[8];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_ext_only_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[830].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
export_t *exp = (preloaded_exports+527);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

init_14: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[831].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[15] = nil;


ip += 1;
goto *next;
}


init_13: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[832].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[14] = nil;


ip += 1;
goto *next;
}


init_12: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[833].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[13] = nil;


ip += 1;
goto *next;
}


l_wait_timeout_3: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[834].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
uint32_t millis = 5000;
if (millis == 0)
{
	ip += 2;
	goto *next;
}
proc->result.what = SLICE_RESULT_WAIT;
uint64_t now = monotonic_clock();
proc->result.until_when = now + (uint64_t)millis * 1000000;
proc->result.jump_to = ip + 2;

ip = (uint32_t *)expand_ptr(ip[1]);

proc->cap.live = 0;
light_swap_out();
goto schedule;
}

l_wait_timeout_2: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[835].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
uint32_t millis = 100;
if (millis == 0)
{
	ip += 2;
	goto *next;
}
proc->result.what = SLICE_RESULT_WAIT;
uint64_t now = monotonic_clock();
proc->result.until_when = now + (uint64_t)millis * 1000000;
proc->result.jump_to = ip + 2;

ip = (uint32_t *)expand_ptr(ip[1]);

proc->cap.live = 0;
light_swap_out();
goto schedule;
}

l_wait_timeout_1: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[836].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
uint32_t millis = 10000;
if (millis == 0)
{
	ip += 2;
	goto *next;
}
proc->result.what = SLICE_RESULT_WAIT;
uint64_t now = monotonic_clock();
proc->result.until_when = now + (uint64_t)millis * 1000000;
proc->result.jump_to = ip + 2;

ip = (uint32_t *)expand_ptr(ip[1]);

proc->cap.live = 0;
light_swap_out();
goto schedule;
}

l_bs_skip_bits_all2_0: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[837].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = rs[2];
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint8_t u = 8;
uint32_t skipped = mc->bs.ends - mc->bs.starts;
if (skipped % u != 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
mc->bs.starts = mc->bs.ends;


ip += 2;
goto *next;
}


l_bs_test_zero_tail2_6: 
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[838].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends > mc->bs.starts)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


call_bif_39: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[839].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_get_stacktrace0;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


call_bif_19: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[840].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
bif_func_t entry = cbif_ets_info2;

//
//TODO: The following two assignments are needed for erlang:process_info/2 and
// erlang:process_display/2 only. Check the performance impact.
//
proc->cap.ip = ip;
proc->cap.cp = cp;

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_BIF)
{
export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
assert(bif_exp != 0);
printk("TRACEBIF: %pt:%pt/%d\n", T(bif_exp->module), T(bif_exp->function), bif_exp->arity);
}
#endif

swap_out();
term_t r = ((cbif_func_t)entry)(proc, rs);
swap_in();
if (r == noval)
{
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);

	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = bif_exp->arity;
	rs[bif_exp->arity] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 1;
goto *next;
}


call_bif_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[841].counter++;
#endif


// r0 - pid or oid
// rs[1] - reason
ip += 1;

if (is_short_pid(r0))
{
proc_t *victim = scheduler_lookup(r0);
if (victim != 0)
{
	proc->result.what = SLICE_RESULT_EXIT2;
	proc->result.victim = victim;
	proc->result.reason2 = rs[1];
	r0 = A_TRUE;
	proc->cap.live = 1;
	light_swap_out();
	goto schedule;
}
}
else if (is_short_oid(r0))
{
outlet_t *ol = outlet_lookup(r0);
if (ol != 0)
{
	proc->result.what = SLICE_RESULT_OUTLET_CLOSE;
	proc->result.closing = ol;
	proc->result.why = rs[1];
	r0 = A_TRUE;
	proc->cap.live = 1;
	light_swap_out();
	goto schedule;
}
}
else
	badarg();

r0 = A_TRUE; 	// ignore
next();
}

call_bif_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[842].counter++;
#endif


// r0 - reason
// rs[1] - args
light_swap_out();
proc->stack_trace = get_stack_trace(ip, cp,
		sp, proc_stack_bottom(proc),
		0, rs[1], &proc->hp);
rs[2] = r0;
rs[1] = A_ERROR;
goto exception;
}

call_bif_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[843].counter++;
#endif


// r0 - class
// rs[1] - reason
// rs[2]- stack trace
light_swap_out();

if (!is_list(rs[2]))
	proc->stack_trace = nil;
else
{
	if (list_len(rs[2]) <= max_backtrace_depth)
		proc->stack_trace = rs[2];
	else
	{
		// truncate the stack trace to max_backtrace_depth
		term_t t = rs[2];
		term_t trace[max_backtrace_depth];
		for (int i = 0; i < max_backtrace_depth; i++)
		{
			term_t *cons = peel_cons(t);
			trace[i] = cons[0];
			t = cons[1];
		}
		proc->stack_trace = heap_vector_to_list(&proc->hp,
				trace, max_backtrace_depth);
	}
}

rs[2] = rs[1];
rs[1] = r0;
goto exception;
}

call_bif_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[844].counter++;
#endif


// r0 - module
if (!is_atom(r0))
	badarg();

ip += 1;
module_info_t *module = code_base_module_by_name(r0, 1);
if (module == 0)	// no old code
	badarg();

// Collect all processes that require termination in the purgatory. The
// scheduler will kill them upon yield.
//
hash_index_t hi;
hash_start(registry, &hi);
proc_t *q;
while ((q = hash_next(&hi)) != 0)
	if (proc_references_module(q, module))
		scheduler_add_purged(q);

code_base_purge(module);

r0 = A_TRUE;
proc->cap.live = 1;
light_swap_out();

proc->result.what = SLICE_RESULT_PURGE_PROCS;
goto schedule;
}

l_int_div_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[845].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int denom = int_value(tmp_arg2);
	if (denom == 0)
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(A_BADARITH, A_ERLANG, A_DIV, tmp_arg1, tmp_arg2);
	}
	int v = int_value(tmp_arg1) / denom;
	assert(fits_int(v));
	r = tag_int(v);
}
else
{
	light_swap_out();
	r = mixed_int_div(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_DIV, tmp_arg1, tmp_arg2);
	}
}
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 4;
goto *next;
}


l_bs_put_utf16_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[846].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t point = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
uint8_t flags = ((ip[3] >> 0) & 255);
if (unicode_encode_utf16(point, flags & BSF_LITTLE, &bpc) < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 4;
goto *next;
}


l_bs_get_utf16_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[847].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

uint8_t flags = ((ip[4] >> 0) & 255);
light_swap_out();
term_t point = unicode_decode_utf16(mc, flags & BSF_LITTLE);
light_swap_in();
if (point == A_BADARG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[2]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = point;
		else
			rs[reg__] = point;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = point;
	}
}



ip += 5;
goto *next;
}


l_bs_get_utf16_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[848].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

uint8_t flags = ((ip[2] >> 0) & 255);
light_swap_out();
term_t point = unicode_decode_utf16(mc, flags & BSF_LITTLE);
light_swap_in();
if (point == A_BADARG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = point;
	else
		rs[reg__] = point;
}



ip += 3;
goto *next;
}


l_bs_get_utf16_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[849].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

uint8_t flags = ((ip[2] >> 8) & 255);
light_swap_out();
term_t point = unicode_decode_utf16(mc, flags & BSF_LITTLE);
light_swap_in();
if (point == A_BADARG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
{
	int reg__ = ((ip[2] >> 16) & 255);
	if (reg__ == 0)
		r0 = point;
	else
		rs[reg__] = point;
}



ip += 3;
goto *next;
}


l_allocate_9: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[850].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
int gap = 10+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 1;
goto *next;
}


l_put_tuple_7: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[851].counter++;
#endif


uint32_t arity = ip[2];
term_t tuple;
if (unlikely(arity == 0))
	tuple = ZERO_TUPLE;
else
{
	tuple = tag_tuple(htop);
	*htop++ = arity;

	term_t *pe = (term_t *)ip + 3;
	int n = arity;
	while (n-- > 0)
	{
		if (is_reg(*pe))
		{
			int ri = reg_index(*pe);
			if (ri == 0)
				*htop++ = r0;
			else
				*htop++ = rs[ri];
		}
		else if (is_slot(*pe))
		{
			int si = slot_index(*pe);
			*htop++ = sp[si+1];
		}
		else
			*htop++ = *pe;

		pe++;
	}
}
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tuple;
		else
			rs[reg__] = tuple;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tuple;
	}
}


ip += arity+3;
next();
}

l_put_tuple_5: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[852].counter++;
#endif


uint32_t arity = ((ip[1] >> 0) & 255);
term_t tuple;
if (unlikely(arity == 0))
	tuple = ZERO_TUPLE;
else
{
	tuple = tag_tuple(htop);
	*htop++ = arity;

	term_t *pe = (term_t *)ip + 2;
	int n = arity;
	while (n-- > 0)
	{
		if (is_reg(*pe))
		{
			int ri = reg_index(*pe);
			if (ri == 0)
				*htop++ = r0;
			else
				*htop++ = rs[ri];
		}
		else if (is_slot(*pe))
		{
			int si = slot_index(*pe);
			*htop++ = sp[si+1];
		}
		else
			*htop++ = *pe;

		pe++;
	}
}
r0 = tuple;

ip += arity+2;
next();
}

l_put_tuple_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[853].counter++;
#endif


uint32_t arity = 5;
term_t tuple;
if (unlikely(arity == 0))
	tuple = ZERO_TUPLE;
else
{
	tuple = tag_tuple(htop);
	*htop++ = arity;

	term_t *pe = (term_t *)ip + 1;
	int n = arity;
	while (n-- > 0)
	{
		if (is_reg(*pe))
		{
			int ri = reg_index(*pe);
			if (ri == 0)
				*htop++ = r0;
			else
				*htop++ = rs[ri];
		}
		else if (is_slot(*pe))
		{
			int si = slot_index(*pe);
			*htop++ = sp[si+1];
		}
		else
			*htop++ = *pe;

		pe++;
	}
}
r0 = tuple;

ip += arity+1;
next();
}

l_put_tuple_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[854].counter++;
#endif


uint32_t arity = 4;
term_t tuple;
if (unlikely(arity == 0))
	tuple = ZERO_TUPLE;
else
{
	tuple = tag_tuple(htop);
	*htop++ = arity;

	term_t *pe = (term_t *)ip + 1;
	int n = arity;
	while (n-- > 0)
	{
		if (is_reg(*pe))
		{
			int ri = reg_index(*pe);
			if (ri == 0)
				*htop++ = r0;
			else
				*htop++ = rs[ri];
		}
		else if (is_slot(*pe))
		{
			int si = slot_index(*pe);
			*htop++ = sp[si+1];
		}
		else
			*htop++ = *pe;

		pe++;
	}
}
r0 = tuple;

ip += arity+1;
next();
}

l_put_tuple_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[855].counter++;
#endif


uint32_t arity = 3;
term_t tuple;
if (unlikely(arity == 0))
	tuple = ZERO_TUPLE;
else
{
	tuple = tag_tuple(htop);
	*htop++ = arity;

	term_t *pe = (term_t *)ip + 1;
	int n = arity;
	while (n-- > 0)
	{
		if (is_reg(*pe))
		{
			int ri = reg_index(*pe);
			if (ri == 0)
				*htop++ = r0;
			else
				*htop++ = rs[ri];
		}
		else if (is_slot(*pe))
		{
			int si = slot_index(*pe);
			*htop++ = sp[si+1];
		}
		else
			*htop++ = *pe;

		pe++;
	}
}
r0 = tuple;

ip += arity+1;
next();
}

l_put_tuple_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[856].counter++;
#endif


uint32_t arity = 2;
term_t tuple;
if (unlikely(arity == 0))
	tuple = ZERO_TUPLE;
else
{
	tuple = tag_tuple(htop);
	*htop++ = arity;

	term_t *pe = (term_t *)ip + 1;
	int n = arity;
	while (n-- > 0)
	{
		if (is_reg(*pe))
		{
			int ri = reg_index(*pe);
			if (ri == 0)
				*htop++ = r0;
			else
				*htop++ = rs[ri];
		}
		else if (is_slot(*pe))
		{
			int si = slot_index(*pe);
			*htop++ = sp[si+1];
		}
		else
			*htop++ = *pe;

		pe++;
	}
}
r0 = tuple;

ip += arity+1;
next();
}

l_is_eq_exact_immed_33: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[857].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[20] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_is_eq_exact_immed_32: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[858].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (rs[22] != (term_t)ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_is_eq_exact_immed_23: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[859].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (sp[11] != A_BER)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_is_eq_exact_immed_17: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[860].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (sp[((ip[2] >> 0) & 255)+1] != A_FUNNY854)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_is_eq_exact_immed_10: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[861].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[255] != A_XMERL_SCANNER)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_is_eq_exact_immed_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[862].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]) != (int8_t)((ip[2] >> 8) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_call_ext_108: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[863].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+572);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_107: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[864].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+573);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_106: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[865].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+574);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_103: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[866].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+575);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_102: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[867].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+576);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_100: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[868].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+577);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_99: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[869].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+578);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_97: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[870].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+579);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_95: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[871].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+313);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_94: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[872].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+580);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_93: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[873].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+581);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_90: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[874].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+582);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_89: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[875].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+583);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_87: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[876].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+584);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_85: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[877].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+585);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_83: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[878].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+586);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_81: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[879].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+587);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_79: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[880].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+588);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_76: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[881].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+589);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_73: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[882].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+590);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_72: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[883].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+591);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_70: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[884].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+592);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_69: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[885].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+593);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_68: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[886].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+594);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_67: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[887].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+595);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_63: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[888].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+596);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_60: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[889].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+597);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_57: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[890].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+360);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_56: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[891].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+598);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_54: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[892].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+599);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_53: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[893].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+600);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_52: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[894].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+601);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_50: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[895].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+602);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_49: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[896].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+603);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_46: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[897].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+604);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_33: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[898].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+605);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_32: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[899].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+606);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_30: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[900].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+607);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_29: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[901].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+608);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_27: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[902].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+609);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_22: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[903].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+610);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_21: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[904].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+611);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_19: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[905].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+612);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_18: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[906].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+613);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_15: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[907].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+614);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_13: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[908].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+615);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[909].counter++;
#endif


cp = ip + 1;
export_t *exp = (preloaded_exports+616);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

allocate_init_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[910].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
int gap = ip[1]+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


term_t y = ip[2];
assert(is_slot(y));
sp[slot_index(y)+1] = nil;


ip += 3;
goto *next;
}


extract_next_element_23: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[911].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[15] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


extract_next_element_18: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[912].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[12] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


get_tuple_element_11: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[913].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
//assert(is_tuple((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1])));
term_t elem = peel_tuple((term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]))[ip[2]+1];	// Pos is 0-based
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = elem;
		else
			rs[reg__] = elem;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = elem;
	}
}



ip += 4;
goto *next;
}


get_tuple_element_10: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[914].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
//assert(is_tuple(sp[((ip[1] >> 0) & 255)+1]));
term_t elem = peel_tuple(sp[((ip[1] >> 0) & 255)+1])[((ip[1] >> 8) & 255)+1];	// Pos is 0-based
sp[((ip[1] >> 16) & 255)+1] = elem;


ip += 2;
goto *next;
}


get_tuple_element_9: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[915].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
//assert(is_tuple(r0));
term_t elem = peel_tuple(r0)[ip[1]+1];	// Pos is 0-based
r0 = elem;


ip += 2;
goto *next;
}


get_tuple_element_8: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[916].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
//assert(is_tuple(sp[((ip[1] >> 0) & 255)+1]));
term_t elem = peel_tuple(sp[((ip[1] >> 0) & 255)+1])[((ip[1] >> 8) & 255)+1];	// Pos is 0-based
r0 = elem;


ip += 2;
goto *next;
}


get_tuple_element_7: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[917].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
//assert(is_tuple(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)])));
term_t elem = peel_tuple(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]))[((ip[1] >> 8) & 255)+1];	// Pos is 0-based
r0 = elem;


ip += 2;
goto *next;
}


get_tuple_element_6: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[918].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
//assert(is_tuple(r0));
term_t elem = peel_tuple(r0)[1+1];	// Pos is 0-based
r0 = elem;


ip += 1;
goto *next;
}


set_tuple_element_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[919].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t tuple = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_tuple(tuple));
(peel_tuple(tuple))[ip[3]+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);


ip += 4;
goto *next;
}


l_call_fun_last_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[920].counter++;
#endif


uint32_t num_slots = ip[1];

 
rs[0] = r0;
uint32_t arity = ((ip[2] >> 0) & 255);
term_t t = rs[arity];		//what about {x,1023} mapped to {x,255}?
if (!is_boxed(t))
	goto l_call_fun_last_1_bad_fun;
uint32_t *p = peel_boxed(t);
if (boxed_tag(p) == SUBTAG_FUN)
{
	t_fun_t *fun = (t_fun_t *)p;
	if (fun->fe == 0)
		fatal_error("unloaded funs not implemented");
	int num_free = fun_num_free(p);
	if (fun_arity(p) != arity+num_free)
		goto l_call_fun_last_1_bad_arity;
	ip = fun->fe->entry;
	term_t *src = fun->frozen;
	term_t *dst = rs + arity;
	int n = num_free;
	while (n-- > 0)
		*dst++ = *src++;
	r0 = rs[0];
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->is_bif)
	{
		swap_out();
		term_t r = invoke_bif(exp->e, proc, rs, arity +1);		// +1 for export
		swap_in();
		if (r == noval)
		{
			raise_bif_mfa.mod = exp->e->module;
			raise_bif_mfa.fun = exp->e->function;
			raise_bif_mfa.arity = exp->e->arity;
			rs[exp->e->arity] = proc->bif_excep_reason;
			goto raise_from_bif;
		}
		r0 = r;
		ip = demasquerade_pointer(sp[0]);
sp += num_slots +1;
cp = 0;


		next();
	}
	if (exp->e->entry == 0)
	{
		//rs[0] = r0;
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		light_swap_in();
		r0 = exp->e->module;
		rs[1] = exp->e->function;
		rs[2] = args;
		ip = (EH_UNDEF_EXP)->entry;
	}
	else if (exp->e->arity != arity)
	{
l_call_fun_last_1_bad_arity:
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		term_t fun_args = heap_tuple2(&proc->hp, t, args);
		term_t reason = heap_tuple2(&proc->hp, A_BADARITY, fun_args);
		light_swap_in();
		raise_error(reason);
	}
	else
		ip = exp->e->entry;
}
else
{
l_call_fun_last_1_bad_fun:
	light_swap_out();
	term_t reason = heap_tuple2(&proc->hp, A_BADFUN, t);
	light_swap_in();
	raise_error(reason);
}
cp = demasquerade_pointer(sp[0]);
sp += num_slots +1;
local_reduce();


}



l_fast_element_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[921].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t tuple = r0;
if (!is_tuple(tuple))
	badarg();
uint32_t *data = peel_tuple(tuple);
uint32_t pos = 3;
if (pos > data[0])
	badarg();
r0 = data[pos];


ip += 1;
goto *next;
}


l_bs_test_unit_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[922].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t unit = ((ip[3] >> 0) & 255);
if (((mc->bs.ends - mc->bs.starts) % unit) != 0)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 4;
goto *next;
}


extract_next_element3_9: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[923].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 10;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


extract_next_element3_8: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[924].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 11;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[2] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


extract_next_element2_15: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[925].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t *dst;
dst = rs + 16;


dst[0] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);
dst[1] = *(term_t *)expand_ptr(tmp_arg1);
tmp_arg1 += sizeof(term_t);


ip += 1;
goto *next;
}


l_bs_start_match2_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[926].counter++;
#endif


void *next = (void *)expand_ptr(ip[6]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (!is_boxed(bin) || !is_binary(peel_boxed(bin)))
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
uint32_t *bdata = peel_boxed(bin);
int num_slots = ip[3] +1;	//NB: +1
if (boxed_tag(bdata) != SUBTAG_MATCH_CTX
			|| match_ctx_num_slots(bdata) < num_slots)
{
	//NB: Live unused, burn some fat?
	light_swap_out();
	bin = bits_bs_start_match2(bin, num_slots, &proc->hp);
	light_swap_in();
}
else
{
	// reused matching context - offset should be saved anyway
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
	mc->saved_offsets[0] = mc->bs.starts;
}
{
	term_t dst__ = ip[4];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = bin;
		else
			rs[reg__] = bin;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = bin;
	}
}



ip += 6;
goto *next;
}


l_bs_start_match2_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[927].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (!is_boxed(bin) || !is_binary(peel_boxed(bin)))
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
uint32_t *bdata = peel_boxed(bin);
int num_slots = ((ip[3] >> 8) & 255) +1;	//NB: +1
if (boxed_tag(bdata) != SUBTAG_MATCH_CTX
			|| match_ctx_num_slots(bdata) < num_slots)
{
	//NB: Live unused, burn some fat?
	light_swap_out();
	bin = bits_bs_start_match2(bin, num_slots, &proc->hp);
	light_swap_in();
}
else
{
	// reused matching context - offset should be saved anyway
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
	mc->saved_offsets[0] = mc->bs.starts;
}
r0 = bin;


ip += 4;
goto *next;
}


l_bs_start_match2_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[928].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = sp[2];
if (!is_boxed(bin) || !is_binary(peel_boxed(bin)))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t *bdata = peel_boxed(bin);
int num_slots = 0 +1;	//NB: +1
if (boxed_tag(bdata) != SUBTAG_MATCH_CTX
			|| match_ctx_num_slots(bdata) < num_slots)
{
	//NB: Live unused, burn some fat?
	light_swap_out();
	bin = bits_bs_start_match2(bin, num_slots, &proc->hp);
	light_swap_in();
}
else
{
	// reused matching context - offset should be saved anyway
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
	mc->saved_offsets[0] = mc->bs.starts;
}
r0 = bin;


ip += 2;
goto *next;
}


l_bs_start_match2_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[929].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = r0;
if (!is_boxed(bin) || !is_binary(peel_boxed(bin)))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t *bdata = peel_boxed(bin);
int num_slots = ((ip[2] >> 8) & 255) +1;	//NB: +1
if (boxed_tag(bdata) != SUBTAG_MATCH_CTX
			|| match_ctx_num_slots(bdata) < num_slots)
{
	//NB: Live unused, burn some fat?
	light_swap_out();
	bin = bits_bs_start_match2(bin, num_slots, &proc->hp);
	light_swap_in();
}
else
{
	// reused matching context - offset should be saved anyway
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
	mc->saved_offsets[0] = mc->bs.starts;
}
r0 = bin;


ip += 3;
goto *next;
}


l_bs_get_integer_32_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[930].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends - mc->bs.starts < 32)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
uint32_t v;
bits_get_word(&mc->bs, v);
light_swap_out();
term_t r = int_to_term(v, &proc->hp);
light_swap_in();
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 5;
goto *next;
}


l_bs_get_integer_32_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[931].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends - mc->bs.starts < 32)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t v;
bits_get_word(&mc->bs, v);
light_swap_out();
term_t r = int_to_term(v, &proc->hp);
light_swap_in();
r0 = r;


ip += 3;
goto *next;
}


l_bs_get_integer_32_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[932].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends - mc->bs.starts < 32)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t v;
bits_get_word(&mc->bs, v);
light_swap_out();
term_t r = int_to_term(v, &proc->hp);
light_swap_in();
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 3;
goto *next;
}


l_bor_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[933].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
	r = tmp_arg1 | tmp_arg2;	//tag intact, never a carry
else
{
	light_swap_out();
	r = mixed_bor(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_BOR, tmp_arg1, tmp_arg2);
	}
}
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 4;
goto *next;
}


l_bsr_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[934].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int64_t v1 = int_value(tmp_arg1);
	int v2 = int_value(tmp_arg2);

	if (v2 >= -32 -TAG_IMMED1_SIZE)
	{
		int64_t v = (v2 >= 64)
			?((v1 < 0) ?-1 :0)
			:((v2 > 0) ?v1 >> v2 :v1 << -v2);
		if (fits_int(v))
			r = tag_int(v);
		else
		{
			light_swap_out();
			bignum_t *bn = bignum_from_int(&proc->hp, v);
			light_swap_in();
			r = tag_boxed(bn);
		}
	}
	else
	{
		light_swap_out();
		r = mixed_bsr_i(v1, v2, &proc->hp);
		light_swap_in();
	}
}
else
{
	light_swap_out();
	r = mixed_bsr(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
}
if (is_atom(r))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_error2(r, A_ERLANG, A_BSR, tmp_arg1, tmp_arg2);
}
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 4;
goto *next;
}


l_bs_get_binary_imm2_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[935].counter++;
#endif


void *next = (void *)expand_ptr(ip[6]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ip[3];
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

light_swap_out();
int needed = WSIZE(t_sub_bin_t);
uint32_t *p = heap_alloc(&proc->hp, needed);
t_sub_bin_t *sb = (t_sub_bin_t *)p;
box_sub_bin(p, mc->parent, mc->bs.starts, mc->bs.starts+bcount, 0);
heap_set_top(&proc->hp, p);
light_swap_in();

mc->bs.starts += bcount;
{
	term_t dst__ = ip[4];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(sb);
		else
			rs[reg__] = tag_boxed(sb);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(sb);
	}
}



ip += 6;
goto *next;
}


l_bs_get_binary_imm2_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[936].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t bin = ((((ip[4] >> 0) & 255) == 0) ?r0 :rs[((ip[4] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ip[2];
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

light_swap_out();
int needed = WSIZE(t_sub_bin_t);
uint32_t *p = heap_alloc(&proc->hp, needed);
t_sub_bin_t *sb = (t_sub_bin_t *)p;
box_sub_bin(p, mc->parent, mc->bs.starts, mc->bs.starts+bcount, 0);
heap_set_top(&proc->hp, p);
light_swap_in();

mc->bs.starts += bcount;
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(sb);
		else
			rs[reg__] = tag_boxed(sb);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(sb);
	}
}



ip += 5;
goto *next;
}


l_bs_test_tail_imm2_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[937].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ip[3];
if (mc->bs.ends - mc->bs.starts != bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 4;
goto *next;
}


l_bxor_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[938].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
	r = tag_int(int_value(tmp_arg1) ^ int_value(tmp_arg2));
else
{
	light_swap_out();
	r = mixed_bxor(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_BXOR, tmp_arg1, tmp_arg2);
	}
}
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 4;
goto *next;
}


l_bs_get_float2_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[939].counter++;
#endif


void *next = (void *)expand_ptr(ip[6]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount;
int x = bits_calc_bit_size((term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]), ((ip[5] >> 8) & 255), &bcount);
if (x == -TOO_LONG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	raise_error(A_SYSTEM_LIMIT);
}
if (x < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
uint8_t flags = ((ip[5] >> 16) & 255);
light_swap_out();
term_t v = bits_bs_get_float(mc, bcount, flags & BSF_LITTLE, &proc->hp);
light_swap_in();
if (v == A_BADARG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
{
	term_t dst__ = ip[4];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = v;
		else
			rs[reg__] = v;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = v;
	}
}



ip += 6;
goto *next;
}


move_jump_9: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[940].counter++;
#endif


r0 = A_FUNNY854;
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

move_jump_8: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[941].counter++;
#endif


r0 = A_FALSE;
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

allocate_heap_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[942].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
if (unlikely(hend - htop < ip[2]))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, ((ip[3] >> 0) & 255));
		heap_ensure(&proc->hp, ip[2], root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, ip[2]);
	swap_in();
}


int gap = ip[1]+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 4;
goto *next;
}


move_return_44: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[943].counter++;
#endif


r0 = A_BAD_LEN;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_43: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[944].counter++;
#endif


r0 = tag_int(18);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_42: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[945].counter++;
#endif


r0 = rs[6];
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_41: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[946].counter++;
#endif


r0 = tag_int(17);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_38: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[947].counter++;
#endif


r0 = tag_int(128);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_37: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[948].counter++;
#endif


r0 = tag_int(20);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_35: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[949].counter++;
#endif


r0 = A_NOMATCH;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_32: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[950].counter++;
#endif


r0 = tag_int(15);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_31: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[951].counter++;
#endif


r0 = A_EXPERIMENTER;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_30: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[952].counter++;
#endif


r0 = A_IGNORE;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_27: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[953].counter++;
#endif


r0 = A_EPERM;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_25: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[954].counter++;
#endif


r0 = tag_int(13);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_24: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[955].counter++;
#endif


r0 = tag_int(16);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_23: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[956].counter++;
#endif


r0 = tag_int(12);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_18: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[957].counter++;
#endif


r0 = tag_int(11);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_return_15: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[958].counter++;
#endif


r0 = tag_int(5);
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_new_bs_put_integer_imm_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[959].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
uint32_t bsz = ip[2];
assert(bpc.ends-bpc.starts >= bsz);
term_t v = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
uint8_t flags = ((ip[4] >> 0) & 255);
if (is_int(v))
{
	light_swap_out();
	bits_put_integer(&bpc, int_value(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else if (is_boxed(v) && is_bignum(peel_boxed(v)))
{
	light_swap_out();
	bits_put_bignum(&bpc, (bignum_t *)peel_boxed(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 5;
goto *next;
}


l_new_bs_put_integer_imm_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[960].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
uint32_t bsz = ip[2];
assert(bpc.ends-bpc.starts >= bsz);
term_t v = tag_int(0);
uint8_t flags = 0;
if (is_int(v))
{
	light_swap_out();
	bits_put_integer(&bpc, int_value(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else if (is_boxed(v) && is_bignum(peel_boxed(v)))
{
	light_swap_out();
	bits_put_bignum(&bpc, (bignum_t *)peel_boxed(v), bsz, flags & BSF_LITTLE);
	light_swap_in();
}
else
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 3;
goto *next;
}


l_bs_get_integer_small_imm_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[961].counter++;
#endif


void *next = (void *)expand_ptr(ip[6]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ip[2];
assert(bcount <= 32 -3);	//fits_int()
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
uint32_t slack = 32-bcount;
uint8_t flags = ((ip[5] >> 0) & 255);
uint8_t buf[4] = {0, 0, 0, 0};
int v;
if (flags & BSF_LITTLE)
{
	bits_t bs = {
		.data = buf,
		.starts = 0,
		.ends = bcount
	};
	bits_fill(&mc->bs, &bs);
	uint32_t v0 = GET_UINT_32_LE(buf);
	int bo = slack & 7;
	v = (flags & BSF_SIGNED)
		?(int)(v0 << slack) >> (slack +bo)
		:v0 >> bo;
}
else
{
	bits_t bs = {
		.data = buf,
		.starts = slack,
		.ends = 32
	};
	bits_fill(&mc->bs, &bs);
	uint32_t v0 = GET_UINT_32(buf);
	v = (flags & BSF_SIGNED)
		?((int)(v0 << slack)) >> slack
		:v0;
}
{
	term_t dst__ = ip[4];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_int(v);
		else
			rs[reg__] = tag_int(v);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_int(v);
	}
}



ip += 6;
goto *next;
}


l_rem_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[962].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int denom = int_value(tmp_arg2);
	if (denom == 0)
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(A_BADARITH, A_ERLANG, A_REM, tmp_arg1, tmp_arg2);
	}
	int v = int_value(tmp_arg1) % denom;
	// always fits int
	r0 = tag_int(v);
}
else
{
	swap_out();
	term_t r = mixed_rem(tmp_arg1, tmp_arg2, &proc->hp);
	swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_REM, tmp_arg1, tmp_arg2);
	}
	r0 = r;
}


ip += 3;
goto *next;
}


is_nil_26: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[963].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[18] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nil_25: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[964].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[22] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nil_24: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[965].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (sp[5] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nil_23: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[966].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (sp[6] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nil_19: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[967].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (sp[4] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nil_18: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[968].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (rs[16] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nil_16: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[969].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (sp[3] != nil)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_bsl_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[970].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int64_t v1 = int_value(tmp_arg1);
	int v2 = int_value(tmp_arg2);

	if (v2 <= 32 +TAG_IMMED1_SIZE)
	{
		int64_t v = (v2 <= -64)
			?((v1 < 0) ?-1 :0)
			:((v2 > 0) ?v1 << v2 :v1 >> -v2);
		if (fits_int(v))
			r = tag_int(v);
		else
		{
			light_swap_out();
			bignum_t *bn = bignum_from_int(&proc->hp, v);
			light_swap_in();
			r = tag_boxed(bn);
		}
	}
	else
	{
		light_swap_out();
		r = mixed_bsl_i(v1, v2, &proc->hp);
		light_swap_in();
	}
}
else
{
	light_swap_out();
	r = mixed_bsl(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
}
if (is_atom(r))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_error2(r, A_ERLANG, A_BSL, tmp_arg1, tmp_arg2);
}
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 4;
goto *next;
}


l_fmul_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[971].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
double d = fr[((ip[1] >> 0) & 255)] * fr[((ip[1] >> 8) & 255)];
if (!isfinite(d))
	raise_error(A_BADARITH);
fr[((ip[1] >> 16) & 255)] = d;


ip += 2;
goto *next;
}


is_tuple_of_arity_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[972].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t src = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_tuple(src) || *peel_tuple(src) != ip[3])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
tmp_arg1 = shrink_ptr(peel_tuple(src)+1);	// prepare for extract_element


ip += 4;
goto *next;
}


is_tuple_of_arity_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[973].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t src = r0;
if (!is_tuple(src) || *peel_tuple(src) != ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
tmp_arg1 = shrink_ptr(peel_tuple(src)+1);	// prepare for extract_element


ip += 3;
goto *next;
}


test_arity_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[974].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t src = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_tuple(src));
if (*peel_tuple(src) != ip[3])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
tmp_arg1 = shrink_ptr(peel_tuple(src)+1);	// prepare for extract_element


ip += 4;
goto *next;
}


test_arity_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[975].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t src = r0;
assert(is_tuple(src));
if (*peel_tuple(src) != ip[2])
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
tmp_arg1 = shrink_ptr(peel_tuple(src)+1);	// prepare for extract_element


ip += 3;
goto *next;
}


bs_context_to_binary_8: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[976].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = rs[8];
if (is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX)
{
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

	light_swap_out();
	uint32_t *p = heap_alloc(&proc->hp, WSIZE(t_sub_bin_t));
	t_sub_bin_t *sb = (t_sub_bin_t *)p;

	// Use the offset from the slot zero as a better start
	int64_t starts = mc->saved_offsets[0];

	box_sub_bin(p, mc->parent, starts, mc->bs.ends, 0);
	heap_set_top(&proc->hp, p);
	light_swap_in();

	rs[8] = tag_boxed(sb);
}


ip += 1;
goto *next;
}


bs_context_to_binary_7: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[977].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = rs[4];
if (is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX)
{
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

	light_swap_out();
	uint32_t *p = heap_alloc(&proc->hp, WSIZE(t_sub_bin_t));
	t_sub_bin_t *sb = (t_sub_bin_t *)p;

	// Use the offset from the slot zero as a better start
	int64_t starts = mc->saved_offsets[0];

	box_sub_bin(p, mc->parent, starts, mc->bs.ends, 0);
	heap_set_top(&proc->hp, p);
	light_swap_in();

	rs[4] = tag_boxed(sb);
}


ip += 1;
goto *next;
}


bs_context_to_binary_6: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[978].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = sp[1];
if (is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX)
{
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

	light_swap_out();
	uint32_t *p = heap_alloc(&proc->hp, WSIZE(t_sub_bin_t));
	t_sub_bin_t *sb = (t_sub_bin_t *)p;

	// Use the offset from the slot zero as a better start
	int64_t starts = mc->saved_offsets[0];

	box_sub_bin(p, mc->parent, starts, mc->bs.ends, 0);
	heap_set_top(&proc->hp, p);
	light_swap_in();

	sp[1] = tag_boxed(sb);
}


ip += 1;
goto *next;
}


bs_context_to_binary_5: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[979].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = sp[6];
if (is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX)
{
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

	light_swap_out();
	uint32_t *p = heap_alloc(&proc->hp, WSIZE(t_sub_bin_t));
	t_sub_bin_t *sb = (t_sub_bin_t *)p;

	// Use the offset from the slot zero as a better start
	int64_t starts = mc->saved_offsets[0];

	box_sub_bin(p, mc->parent, starts, mc->bs.ends, 0);
	heap_set_top(&proc->hp, p);
	light_swap_in();

	sp[6] = tag_boxed(sb);
}


ip += 1;
goto *next;
}


bs_context_to_binary_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[980].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = sp[10];
if (is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX)
{
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

	light_swap_out();
	uint32_t *p = heap_alloc(&proc->hp, WSIZE(t_sub_bin_t));
	t_sub_bin_t *sb = (t_sub_bin_t *)p;

	// Use the offset from the slot zero as a better start
	int64_t starts = mc->saved_offsets[0];

	box_sub_bin(p, mc->parent, starts, mc->bs.ends, 0);
	heap_set_top(&proc->hp, p);
	light_swap_in();

	sp[10] = tag_boxed(sb);
}


ip += 1;
goto *next;
}


bs_context_to_binary_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[981].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = rs[2];
if (is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX)
{
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

	light_swap_out();
	uint32_t *p = heap_alloc(&proc->hp, WSIZE(t_sub_bin_t));
	t_sub_bin_t *sb = (t_sub_bin_t *)p;

	// Use the offset from the slot zero as a better start
	int64_t starts = mc->saved_offsets[0];

	box_sub_bin(p, mc->parent, starts, mc->bs.ends, 0);
	heap_set_top(&proc->hp, p);
	light_swap_in();

	rs[2] = tag_boxed(sb);
}


ip += 1;
goto *next;
}


bs_context_to_binary_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[982].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = sp[2];
if (is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX)
{
	t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

	light_swap_out();
	uint32_t *p = heap_alloc(&proc->hp, WSIZE(t_sub_bin_t));
	t_sub_bin_t *sb = (t_sub_bin_t *)p;

	// Use the offset from the slot zero as a better start
	int64_t starts = mc->saved_offsets[0];

	box_sub_bin(p, mc->parent, starts, mc->bs.ends, 0);
	heap_set_top(&proc->hp, p);
	light_swap_in();

	sp[2] = tag_boxed(sb);
}


ip += 1;
goto *next;
}


l_new_bs_put_binary_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[983].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t sz = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
uint32_t bcount;
if (bits_calc_bit_size(sz, ((ip[4] >> 0) & 255), &bcount) < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
assert(bpc.ends - bpc.starts >= bcount);
term_t bin = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
if (!is_boxed(bin) || !is_binary(peel_boxed(bin)))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

bits_t bs;
bits_get_real(peel_boxed(bin), &bs);
if (bs.ends - bs.starts < bcount)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
bs.ends = bs.starts +bcount;
bits_copy(&bs, &bpc);


ip += 5;
goto *next;
}


badmatch_17: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[984].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]));
light_swap_in();
raise_error(reason);
}

badmatch_16: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[985].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, sp[9]);
light_swap_in();
raise_error(reason);
}

badmatch_15: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[986].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, rs[6]);
light_swap_in();
raise_error(reason);
}

badmatch_14: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[987].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, sp[7]);
light_swap_in();
raise_error(reason);
}

badmatch_13: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[988].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, rs[8]);
light_swap_in();
raise_error(reason);
}

badmatch_12: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[989].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, rs[5]);
light_swap_in();
raise_error(reason);
}

badmatch_11: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[990].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, sp[5]);
light_swap_in();
raise_error(reason);
}

badmatch_10: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[991].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, sp[1]);
light_swap_in();
raise_error(reason);
}

badmatch_9: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[992].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, sp[6]);
light_swap_in();
raise_error(reason);
}

badmatch_8: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[993].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, sp[10]);
light_swap_in();
raise_error(reason);
}

badmatch_7: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[994].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, sp[4]);
light_swap_in();
raise_error(reason);
}

badmatch_6: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[995].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, rs[4]);
light_swap_in();
raise_error(reason);
}

badmatch_5: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[996].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, sp[3]);
light_swap_in();
raise_error(reason);
}

badmatch_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[997].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, sp[2]);
light_swap_in();
raise_error(reason);
}

badmatch_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[998].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, rs[2]);
light_swap_in();
raise_error(reason);
}

badmatch_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[999].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, rs[3]);
light_swap_in();
raise_error(reason);
}

badmatch_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1000].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_BADMATCH, rs[1]);
light_swap_in();
raise_error(reason);
}

l_bs_test_unit_8_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1001].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (((mc->bs.ends - mc->bs.starts) & 7) != 0)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_bs_get_utf8_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1002].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

light_swap_out();
term_t point = unicode_decode_utf8(mc);
light_swap_in();
if (point == A_BADARG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[2]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = point;
		else
			rs[reg__] = point;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = point;
	}
}



ip += 4;
goto *next;
}


l_bs_get_utf8_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1003].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = ((((ip[2] >> 0) & 255) == 0) ?r0 :rs[((ip[2] >> 0) & 255)]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

light_swap_out();
term_t point = unicode_decode_utf8(mc);
light_swap_in();
if (point == A_BADARG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = point;
	else
		rs[reg__] = point;
}



ip += 3;
goto *next;
}


l_bs_put_utf8_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1004].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t point = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (unicode_encode_utf8(point, &bpc) < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 3;
goto *next;
}


l_bs_match_string_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1005].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ip[3];
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
bits_t bs1 = {
	.data = (uint8_t *)expand_ptr(ip[4]),
	.starts = 0,
	.ends = bcount
};
bits_t bs2 = {
	.data = mc->bs.data,
	.starts = mc->bs.starts,
	.ends = mc->bs.starts +bcount
};
//NB: bits_compare garbles both bs1 and bs2
if (bits_compare(&bs1, &bs2) != 0)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);

mc->bs.starts += bcount;


ip += 5;
goto *next;
}


l_bs_match_string_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1006].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ip[2];
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
bits_t bs1 = {
	.data = (uint8_t *)expand_ptr(ip[3]),
	.starts = 0,
	.ends = bcount
};
bits_t bs2 = {
	.data = mc->bs.data,
	.starts = mc->bs.starts,
	.ends = mc->bs.starts +bcount
};
//NB: bits_compare garbles both bs1 and bs2
if (bits_compare(&bs1, &bs2) != 0)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

mc->bs.starts += bcount;


ip += 4;
goto *next;
}


l_bs_put_string_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1007].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
bits_t src;
bits_init_buf((uint8_t *)expand_ptr(ip[1]), 3, &src);
bits_copy(&src, &bpc);


ip += 2;
goto *next;
}


fconv_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1008].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
double converted = term_to_float(v);
if (!isfinite(converted))
	raise_error(A_BADARITH);
fr[1] = converted;


ip += 2;
goto *next;
}


l_m_div_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1009].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
double a = term_to_float(tmp_arg1);
double b = term_to_float(tmp_arg2);
term_t r = A_BADARG;
if (b == 0.0 || !isfinite(a) || !isfinite(b))
	goto m_div_badarith;
light_swap_out();
r = heap_float_with_check(&proc->hp, a / b);
light_swap_in();
if (is_atom(r))
{
m_div_badarith:
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_error2(r, A_ERLANG, ADIV__, tmp_arg1, tmp_arg2);
}
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 4;
goto *next;
}


raise_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1010].counter++;
#endif


term_t val1 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);

light_swap_out();

//XXX: retrace stack?
//proc->stack_trace = noval;
rs[2] = val1;

//XXX: a nasty hack for OTP compatibility
assert(is_atom(proc->last_excep_class));
rs[1] = proc->last_excep_class;
proc->last_excep_class = noval;

goto exception;
}

raise_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1011].counter++;
#endif


term_t val1 = rs[2];

light_swap_out();

//XXX: retrace stack?
//proc->stack_trace = noval;
rs[2] = val1;

//XXX: a nasty hack for OTP compatibility
assert(is_atom(proc->last_excep_class));
rs[1] = proc->last_excep_class;
proc->last_excep_class = noval;

goto exception;
}

is_integer_allocate_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1012].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_int(v) && !(is_boxed(v) && is_bignum(peel_boxed(v))))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

int gap = ip[3]+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 4;
goto *next;
}


is_nonempty_list_allocate_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1013].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
if (!is_cons((term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2])))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

int gap = ip[3]+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing




ip += 4;
goto *next;
}


l_fnegate_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1014].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
double val = fr[((ip[1] >> 0) & 255)];
// never overflows
fr[((ip[1] >> 8) & 255)] = -val;


ip += 2;
goto *next;
}


l_bs_validate_unicode_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1015].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t point = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (!is_int(point))
	goto invalid_codepoint;
int n = int_value(point);
if (n < 0 || n > 0x10ffff || (n >= 0xd800 && n <= 0xdfff))
{
invalid_codepoint:
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 3;
goto *next;
}


l_hibernate_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1016].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);

// TODO
// TODO A STUB
// TODO
// TODO stack not dropped - gc not run - process not put to sleep
// TODO

goto l_apply_entry;


ip += 1;
goto *next;
}


put_list_8: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1017].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
htop[0] = r0;
htop[1] = sp[1];
sp[1] = tag_cons(htop);
htop += 2;


ip += 1;
goto *next;
}


is_nonempty_list_41: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1018].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (!is_cons((term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2])))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


is_nonempty_list_40: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1019].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[30]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_39: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1020].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[29]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_38: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1021].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[28]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_37: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1022].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[27]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_36: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1023].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[23]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_34: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1024].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(sp[6]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_32: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1025].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[25]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_31: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1026].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[21]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_28: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1027].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(sp[7]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_26: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1028].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(sp[10]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_25: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1029].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[19]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_20: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1030].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(rs[17]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


is_nonempty_list_16: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1031].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (!is_cons(sp[4]))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


get_list_9: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1032].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
assert(is_cons(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)])));
term_t *pair = peel_cons(((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]));
sp[((ip[1] >> 8) & 255)+1] = pair[0];
r0 = pair[1];


ip += 2;
goto *next;
}


get_list_7: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1033].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
assert(is_cons(r0));
term_t *pair = peel_cons(r0);
sp[((ip[1] >> 0) & 255)+1] = pair[0];
sp[((ip[1] >> 8) & 255)+1] = pair[1];


ip += 2;
goto *next;
}


get_list_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1034].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
assert(is_cons(r0));
term_t *pair = peel_cons(r0);
{
	int reg__ = ((ip[1] >> 0) & 255);
	if (reg__ == 0)
		r0 = pair[0];
	else
		rs[reg__] = pair[0];
}

{
	int reg__ = ((ip[1] >> 8) & 255);
	if (reg__ == 0)
		r0 = pair[1];
	else
		rs[reg__] = pair[1];
}



ip += 2;
goto *next;
}


case_end_11: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1035].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]));
light_swap_in();
raise_error(reason);
}

case_end_10: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1036].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, rs[5]);
light_swap_in();
raise_error(reason);
}

case_end_9: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1037].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, sp[5]);
light_swap_in();
raise_error(reason);
}

case_end_8: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1038].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, sp[1]);
light_swap_in();
raise_error(reason);
}

case_end_7: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1039].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, rs[4]);
light_swap_in();
raise_error(reason);
}

case_end_6: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1040].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, sp[4]);
light_swap_in();
raise_error(reason);
}

case_end_5: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1041].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, rs[3]);
light_swap_in();
raise_error(reason);
}

case_end_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1042].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, sp[2]);
light_swap_in();
raise_error(reason);
}

case_end_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1043].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, sp[3]);
light_swap_in();
raise_error(reason);
}

case_end_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1044].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, rs[2]);
light_swap_in();
raise_error(reason);
}

case_end_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1045].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, rs[1]);
light_swap_in();
raise_error(reason);
}

case_end_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1046].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_CASE_CLAUSE, r0);
light_swap_in();
raise_error(reason);
}

try_case_end_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1047].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_TRY_CLAUSE, (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]));
light_swap_in();
raise_error(reason);
}

try_case_end_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1048].counter++;
#endif


light_swap_out();
term_t reason = heap_tuple2(&proc->hp, A_TRY_CLAUSE, r0);
light_swap_in();
raise_error(reason);
}

apply_last_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1049].counter++;
#endif



//
// NB: BEAM uses a function call for this, not a macro
//

int arity = ((ip[2] >> 0) & 255);
term_t m = (arity == 0) ?r0 :rs[arity];
term_t f = rs[arity+1];

term_t this = noval;	// abstract module?
if (!is_atom(f))
	badarg();
if (!is_atom(m))
{
	if (!is_tuple(m))
		badarg();
	uint32_t *p = peel_tuple(m);
	if (p[0] < 1)
		badarg();
	this = m;
	m = p[1];
	if (!is_atom(m))
		badarg();
}

if (this != noval)
{
	if (arity == 0)
		r0 = this;
	else
		rs[arity] = this;
	arity++;
}

export_t *exp = code_base_lookup(m, f, arity);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, arity);
	light_swap_in();
	r0 = m;
	rs[1] = f;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}


cp = demasquerade_pointer(sp[0]);
sp += ip[1]+1;
if (unlikely(exp->is_bif))
{
	swap_out();
	term_t r = invoke_bif(exp, proc, rs, arity +2);		// +2 for module, function
	swap_in();
	if (r == noval)
	{
		raise_bif_mfa.mod = exp->module;
		raise_bif_mfa.fun = exp->function;
		raise_bif_mfa.arity = exp->arity;
		rs[exp->arity] = proc->bif_excep_reason;
		goto raise_from_bif;
	}
	r0 = r;
	ip = cp;
	cp = 0; // not to confuse stack tracing
	next();
}
else
{
	ip = exp->entry;
	local_reduce();
}
}

l_call_ext_last_5: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1050].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 5+1;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_bs_append_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1051].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
swap_out();
int live = ((ip[4] >> 0) & 255);
rs[live] = tmp_arg1;
rs[live+1] = tmp_arg2;
proc_burn_fat(proc, 0, rs, live +2);
tmp_arg2 = rs[live +1];
tmp_arg1 = rs[live];

// tmp_arg1 - bit size in units
// tmp_arg2 - binary
term_t bin = bits_bs_append(tmp_arg2,
		tmp_arg1, ((ip[4] >> 8) & 255), ip[2], &bpc, &proc->hp);
assert(bpc.ends >= bpc.starts);
swap_in();
if (bin == A_SYSTEM_LIMIT || bin == A_BADARG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	raise_error(bin);
}
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = bin;
		else
			rs[reg__] = bin;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = bin;
	}
}



ip += 5;
goto *next;
}


l_increment_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1052].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
int i = (int)ip[1];
term_t src = r0;
term_t r;
if (is_int(src))
{
	int v = int_value(src) + i;
	if (likely(fits_int(v)))
		r = tag_int(v);
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		r = tag_boxed(bn);
	}
}
else
{
	light_swap_out();
	r = mixed_add_immed(src, i, &proc->hp);
	light_swap_in();
	if (is_atom(r))
		bif_error2(r, A_ERLANG, APLUS__, src, tag_int(i));
}
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = r;
	else
		rs[reg__] = r;
}



ip += 3;
goto *next;
}


l_bs_private_append_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1053].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
// tmp_arg1 - bit size in units
// tmp_arg2 - writable sub bin or a proc bin (refc=1)
assert(is_boxed(tmp_arg2) && boxed_tag(peel_boxed(tmp_arg2)) == SUBTAG_SUB_BIN);
assert(sub_bin_is_writable(peel_boxed(tmp_arg2)));

light_swap_out();
int x = bits_bs_private_append(tmp_arg2,
		tmp_arg1, ((ip[3] >> 0) & 255), &bpc, &proc->hp);
light_swap_in();
if (x < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tmp_arg2;
		else
			rs[reg__] = tmp_arg2;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tmp_arg2;
	}
}



ip += 4;
goto *next;
}


l_bs_init_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1054].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
uint32_t osz = ip[1];
binnode_t *node = binnode_make(osz);
int needed = WSIZE(t_proc_bin_t) + WSIZE(t_sub_bin_t) + ip[2];
light_swap_out();
uint32_t *p = heap_alloc_N(&proc->hp, needed);
if (p == 0)
{
	binnode_destroy(node);
	no_memory_signal();
}
t_proc_bin_t *pb = (t_proc_bin_t *)p;
box_proc_bin(p, osz, node);
heap_set_top0(&proc->hp, p);
proc_bin_link(&proc->hp.proc_bins, pb, &proc->hp.total_pb_size);
light_swap_in();
bits_get_real(pb, &bpc);
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(pb);
		else
			rs[reg__] = tag_boxed(pb);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(pb);
	}
}



ip += 5;
goto *next;
}


l_new_bs_put_float_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1055].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t sz = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_int(sz) || (is_boxed(sz) && is_bignum(peel_boxed(sz))));

uint32_t bcount;
if (bits_calc_bit_size(sz, ((ip[4] >> 0) & 255), &bcount) < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
uint8_t flags = ((ip[4] >> 8) & 255);
term_t v = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
light_swap_out();
int x = bits_bs_put_float(v, bcount, flags & BSF_LITTLE, &bpc);
light_swap_in();
if (x < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 5;
goto *next;
}


l_bs_validate_unicode_retract_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1056].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
assert(is_boxed(tmp_arg2) && boxed_tag(peel_boxed(tmp_arg2)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(tmp_arg2);
if (!is_int(tmp_arg1))	// all bignums are invalid code points
	goto rectract;
int n = int_value(tmp_arg1);
if (n < 0 || n > 0x10ffff || (n >= 0xd800 && n <= 0xdfff))
{
rectract:
	mc->bs.starts -= 32;	// rectract 32 bits
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 2;
goto *next;
}


l_yield_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1057].counter++;
#endif


ip += 0 +1;
r0 = A_TRUE;
proc->cap.live = 1;
light_swap_out();
goto yield;
}

l_apply_fun_last_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1058].counter++;
#endif


int num_slots = ip[1];
if (!is_boxed(r0))
	goto l_apply_fun_last_0_bad_fun;
uint32_t *p = peel_boxed(r0);
term_t args = rs[1];
if (!is_list(args))
	badarg();
int arity = list_len(args);
if (arity < 0 || arity > 255)
	goto l_apply_fun_last_0_bad_fun;
if (boxed_tag(p) == SUBTAG_FUN)
{
	t_fun_t *fun = (t_fun_t *)p;
	if (fun->fe == 0)
		not_implemented("unloaded funs");
	int num_free = fun_num_free(p);
	if (fun_arity(p) != arity + num_free)
		goto l_apply_fun_last_0_bad_arity;
	memcpy(rs+arity, fun->frozen, num_free*sizeof(term_t));
	ip = fun->fe->entry;
	heap_list_to_vector(args, rs);
	r0 = rs[0];
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->is_bif)
		not_implemented("bifs as exports");
	if (exp->e->entry == 0)
	{
		r0 = exp->e->module;
		rs[1] = exp->e->function;
		rs[2] = args;
		ip = (EH_UNDEF_EXP)->entry;
	}
	else if (exp->e->arity != arity)
	{
l_apply_fun_last_0_bad_arity:
		light_swap_out();
		term_t fun_args = heap_tuple2(&proc->hp, r0, args);
		term_t reason = heap_tuple2(&proc->hp, A_BADARITY, fun_args);
		light_swap_in();
		raise_error(reason);
	}
	else
	{
		ip = exp->e->entry;
		heap_list_to_vector(args, rs);
		r0 = rs[0];
	}
}
else
{
l_apply_fun_last_0_bad_fun:
	light_swap_out();
	term_t reason = heap_tuple2(&proc->hp, A_BADFUN, r0);
	light_swap_in();
	raise_error(reason);
}


cp = demasquerade_pointer(sp[0]);
sp += num_slots +1;
local_reduce();
}

l_minus_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1059].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int v = int_value(tmp_arg1) - int_value(tmp_arg2);
	if (likely(fits_int(v)))
	{
		{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_int(v);
		else
			rs[reg__] = tag_int(v);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_int(v);
	}
}

	}
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(bn);
		else
			rs[reg__] = tag_boxed(bn);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(bn);
	}
}

	}
}
else
{
	swap_out();
	term_t r = mixed_sub(tmp_arg1, tmp_arg2, &proc->hp);
	swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, AMINUS__, tmp_arg1, tmp_arg2);
	}
	{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}

}


ip += 4;
goto *next;
}


l_minus_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1060].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int v = int_value(tmp_arg1) - int_value(tmp_arg2);
	if (likely(fits_int(v)))
	{
		r0 = tag_int(v);
	}
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		r0 = tag_boxed(bn);
	}
}
else
{
	swap_out();
	term_t r = mixed_sub(tmp_arg1, tmp_arg2, &proc->hp);
	swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, AMINUS__, tmp_arg1, tmp_arg2);
	}
	r0 = r;
}


ip += 3;
goto *next;
}


init3_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1061].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t y1 = ip[1];
term_t y2 = ip[2];
term_t y3 = ip[3];
assert(is_slot(y1));
assert(is_slot(y2));
assert(is_slot(y3));
sp[slot_index(y1)+1] = nil;
sp[slot_index(y2)+1] = nil;
sp[slot_index(y3)+1] = nil;


ip += 4;
goto *next;
}


l_select_val_smallints_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1062].counter++;
#endif


term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
pair_t *alpha = (pair_t *)(ip + 4);
pair_t *beta = (pair_t *)((uint32_t *)alpha + ip[3]);

if (v < alpha->f || v > beta[-1].f)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);

//TODO: in BEAM the binary search parameters are
// made into unsigned values for performance

while (beta > alpha+1)	// at least 2 pairs
{
	pair_t *mid = alpha + (beta - alpha +1)/2;
	if ((term_t)mid->f > v)
		beta = mid;
	else
		alpha = mid;
}

assert(beta == alpha+1);
if (alpha->f == v)
{
	ip = (uint32_t *)expand_ptr(alpha->s);
	next();
}

do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);


}

l_bif2_5: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1063].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r = ((bif_func2_t)bif_eq2)(tmp_arg1, tmp_arg2, proc);
if (r == noval)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 3;
goto *next;
}


l_bif2_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1064].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r = ((bif_func2_t)bif_or2)(tmp_arg1, tmp_arg2, proc);
if (r == noval)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 3;
goto *next;
}


l_select_val2_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1065].counter++;
#endif


term_t v = ((((ip[4] >> 0) & 255) == 0) ?r0 :rs[((ip[4] >> 0) & 255)]);
if (v == (int8_t)((ip[4] >> 8) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
if (v == (int8_t)((ip[4] >> 16) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

l_select_tuple_arity2_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1066].counter++;
#endif


term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (!is_tuple(v))
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
int arity = *peel_tuple(v);
if (arity == ip[3])
	do {
ip = (uint32_t *)expand_ptr(ip[4]);
next();
} while (0);
if (arity == ip[5])
	do {
ip = (uint32_t *)expand_ptr(ip[6]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[2]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
}

l_select_tuple_arity2_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1067].counter++;
#endif


term_t v = sp[((ip[4] >> 0) & 255)+1];
if (!is_tuple(v))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
int arity = *peel_tuple(v);
if (arity == ((ip[4] >> 8) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
if (arity == ((ip[4] >> 16) & 255))
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
assert((uint32_t *)expand_ptr(ip[1]) != 0);
do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}

init2_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1068].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t y1 = ip[1];
term_t y2 = ip[2];
assert(is_slot(y1));
assert(is_slot(y2));
sp[slot_index(y1)+1] = nil;
sp[slot_index(y2)+1] = nil;


ip += 3;
goto *next;
}


l_bs_skip_bits_imm2_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1069].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ip[3];
if (bcount < 0 || bcount > (mc->bs.ends - mc->bs.starts))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
mc->bs.starts += bcount;


ip += 4;
goto *next;
}


l_bs_restore2_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1070].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
int i = ip[2];
assert(i >= 0 && i < match_ctx_num_slots(peel_boxed(bin)));
mc->bs.starts = mc->saved_offsets[i];


ip += 3;
goto *next;
}


l_bs_restore2_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1071].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
int i = 1;
assert(i >= 0 && i < match_ctx_num_slots(peel_boxed(bin)));
mc->bs.starts = mc->saved_offsets[i];


ip += 1;
goto *next;
}


l_bs_restore2_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1072].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
int i = 0;
assert(i >= 0 && i < match_ctx_num_slots(peel_boxed(bin)));
mc->bs.starts = mc->saved_offsets[i];


ip += 1;
goto *next;
}


l_bs_skip_bits2_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1073].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
term_t sz = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
uint32_t bcount;
if (bits_calc_bit_size(sz, ((ip[4] >> 0) & 255), &bcount) < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
if (bcount > (mc->bs.ends - mc->bs.starts))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
mc->bs.starts += bcount;


ip += 5;
goto *next;
}


l_bs_get_binary_all2_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1074].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

uint32_t bcount = mc->bs.ends - mc->bs.starts;
if ((bcount % ((ip[4] >> 8) & 255)) != 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

light_swap_out();
int needed = WSIZE(t_sub_bin_t);
uint32_t *p = heap_alloc(&proc->hp, needed);
t_sub_bin_t *sb = (t_sub_bin_t *)p;
box_sub_bin(p, mc->parent, mc->bs.starts, mc->bs.ends, 0);
heap_set_top(&proc->hp, p);
light_swap_in();

mc->bs.starts = mc->bs.ends;
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(sb);
		else
			rs[reg__] = tag_boxed(sb);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(sb);
	}
}



ip += 5;
goto *next;
}


l_bs_get_binary_all2_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1075].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

uint32_t bcount = mc->bs.ends - mc->bs.starts;
if ((bcount % 8) != 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

light_swap_out();
int needed = WSIZE(t_sub_bin_t);
uint32_t *p = heap_alloc(&proc->hp, needed);
t_sub_bin_t *sb = (t_sub_bin_t *)p;
box_sub_bin(p, mc->parent, mc->bs.starts, mc->bs.ends, 0);
heap_set_top(&proc->hp, p);
light_swap_in();

mc->bs.starts = mc->bs.ends;
{
	int reg__ = ((ip[2] >> 8) & 255);
	if (reg__ == 0)
		r0 = tag_boxed(sb);
	else
		rs[reg__] = tag_boxed(sb);
}



ip += 3;
goto *next;
}


l_bs_save2_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1076].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
int i = ip[2];
assert(i >= 0 && i < match_ctx_num_slots(peel_boxed(bin)));
mc->saved_offsets[i] = mc->bs.starts;


ip += 3;
goto *next;
}


l_bs_save2_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1077].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t bin = r0;
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
int i = 1;
assert(i >= 0 && i < match_ctx_num_slots(peel_boxed(bin)));
mc->saved_offsets[i] = mc->bs.starts;


ip += 1;
goto *next;
}


is_function2_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1078].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t v = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
term_t a = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
if (!is_int(a))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
int arity = int_value(a);
if (!is_boxed(v))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t *p = peel_boxed(v);
if (boxed_tag(p) == SUBTAG_FUN)
{
	if (fun_arity(p) != arity+fun_num_free(p))
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->arity != arity)
		do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
}
else
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 4;
goto *next;
}


l_bif1_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1079].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t r = ((bif_func1_t)(bif_func_t)expand_ptr(ip[2]))((term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]), proc);
if (r == noval)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
{
	term_t dst__ = ip[4];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 5;
goto *next;
}


l_bif1_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1080].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t r = ((bif_func1_t)(bif_func_t)expand_ptr(ip[2]))(r0, proc);
if (r == noval)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 4;
goto *next;
}


is_nonempty_list_test_heap_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1081].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
if (!is_cons(r0))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

if (unlikely(hend - htop < ip[2]))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, ((ip[3] >> 0) & 255));
		heap_ensure(&proc->hp, ip[2], root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, ip[2]);
	swap_in();
}




ip += 4;
goto *next;
}


allocate_heap_zero_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1082].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
if (unlikely(hend - htop < ip[2]))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, ((ip[3] >> 0) & 255));
		heap_ensure(&proc->hp, ip[2], root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, ip[2]);
	swap_in();
}


int gap = ip[1]+1;
if (unlikely(sp - send < gap))
{
	proc_stack_set_top(proc, sp);
	proc_stack_ensure(proc, gap);
	sp = proc_stack_top(proc);
	send = proc_stack_end(proc);

	// In a rare case proc_stack_ensure() may enlarge the space available for
	// the heap
	//
	hend = heap_end(&proc->hp);
}
sp -= gap;
*sp = masquerade_as_boxed(cp);
cp = 0; // not to confuse stack tracing


int n = ip[1];
term_t *slots = sp+1;
while (n-- > 0)
	*slots++ = nil;


ip += 4;
goto *next;
}


deallocate_return_12: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1083].counter++;
#endif


cp = demasquerade_pointer(sp[0]);
sp += 12+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

move_deallocate_return_9: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1084].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = demasquerade_pointer(sp[0]);
sp += 6+1;
ip = cp;
cp = 0; // not to confuse stack tracing
next();
}

l_bs_init_heap_bin_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1085].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
int osz = ip[1];
int needed = WSIZE(t_heap_bin_t) + (osz +3)/4 +
			 WSIZE(t_sub_bin_t) + ip[2];
light_swap_out();
uint32_t *p = heap_alloc(&proc->hp, needed);
t_heap_bin_t *hb = (t_heap_bin_t *)p;
box_heap_bin(p, osz, 0);
heap_set_top0(&proc->hp, p);
light_swap_in();
bits_init_buf(hb->data, osz, &bpc);
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(hb);
		else
			rs[reg__] = tag_boxed(hb);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(hb);
	}
}



ip += 5;
goto *next;
}


l_call_fun_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1086].counter++;
#endif


uint32_t *saved_ip = ip +0 +1;

 
rs[0] = r0;
uint32_t arity = 0;
term_t t = rs[arity];		//what about {x,1023} mapped to {x,255}?
if (!is_boxed(t))
	goto l_call_fun_1_bad_fun;
uint32_t *p = peel_boxed(t);
if (boxed_tag(p) == SUBTAG_FUN)
{
	t_fun_t *fun = (t_fun_t *)p;
	if (fun->fe == 0)
		fatal_error("unloaded funs not implemented");
	int num_free = fun_num_free(p);
	if (fun_arity(p) != arity+num_free)
		goto l_call_fun_1_bad_arity;
	ip = fun->fe->entry;
	term_t *src = fun->frozen;
	term_t *dst = rs + arity;
	int n = num_free;
	while (n-- > 0)
		*dst++ = *src++;
	r0 = rs[0];
}
else if (boxed_tag(p) == SUBTAG_EXPORT)
{
	t_export_t *exp = (t_export_t *)p;
	if (exp->e->is_bif)
	{
		swap_out();
		term_t r = invoke_bif(exp->e, proc, rs, arity +1);		// +1 for export
		swap_in();
		if (r == noval)
		{
			raise_bif_mfa.mod = exp->e->module;
			raise_bif_mfa.fun = exp->e->function;
			raise_bif_mfa.arity = exp->e->arity;
			rs[exp->e->arity] = proc->bif_excep_reason;
			goto raise_from_bif;
		}
		r0 = r;
		ip = saved_ip;


		next();
	}
	if (exp->e->entry == 0)
	{
		//rs[0] = r0;
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		light_swap_in();
		r0 = exp->e->module;
		rs[1] = exp->e->function;
		rs[2] = args;
		ip = (EH_UNDEF_EXP)->entry;
	}
	else if (exp->e->arity != arity)
	{
l_call_fun_1_bad_arity:
		light_swap_out();
		term_t args = heap_vector_to_list(&proc->hp, rs, arity);
		term_t fun_args = heap_tuple2(&proc->hp, t, args);
		term_t reason = heap_tuple2(&proc->hp, A_BADARITY, fun_args);
		light_swap_in();
		raise_error(reason);
	}
	else
		ip = exp->e->entry;
}
else
{
l_call_fun_1_bad_fun:
	light_swap_out();
	term_t reason = heap_tuple2(&proc->hp, A_BADFUN, t);
	light_swap_in();
	raise_error(reason);
}
cp = saved_ip;
local_reduce();


}



l_new_bs_put_binary_imm_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1087].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
uint32_t bcount = ip[2];
//NB: Flags not used
term_t bin = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
if (!is_boxed(bin) || !is_binary(peel_boxed(bin)))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

bits_t bs;
bits_get_real(peel_boxed(bin), &bs);
if (bs.ends - bs.starts < bcount)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
assert(bpc.ends - bpc.starts >= bcount);
bs.ends = bs.starts +bcount;
bits_copy(&bs, &bpc);


ip += 5;
goto *next;
}


l_new_bs_put_binary_imm_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1088].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
uint32_t bcount = ((ip[2] >> 0) & 255);
//NB: Flags not used
term_t bin = r0;
if (!is_boxed(bin) || !is_binary(peel_boxed(bin)))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

bits_t bs;
bits_get_real(peel_boxed(bin), &bs);
if (bs.ends - bs.starts < bcount)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
assert(bpc.ends - bpc.starts >= bcount);
bs.ends = bs.starts +bcount;
bits_copy(&bs, &bpc);


ip += 3;
goto *next;
}


l_bs_get_integer_imm_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1089].counter++;
#endif


void *next = (void *)expand_ptr(ip[6]);
//NB: Live not used
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint32_t bcount = ip[2];
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[3]);
next();
} while (0);
uint8_t flags = ((ip[5] >> 8) & 255);
light_swap_out();
term_t v = bits_bs_get_integer_imm(mc,
	bcount, flags & BSF_SIGNED, flags & BSF_LITTLE, &proc->hp);
light_swap_in();
{
	term_t dst__ = ip[4];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = v;
		else
			rs[reg__] = v;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = v;
	}
}



ip += 6;
goto *next;
}


l_new_bs_put_float_imm_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1090].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
uint32_t bcount = ip[2];
uint8_t flags = ((ip[4] >> 0) & 255);
term_t v = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
light_swap_out();
int x = bits_bs_put_float(v, bcount, flags & BSF_LITTLE, &bpc);
light_swap_in();
if (x < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 5;
goto *next;
}


l_new_bs_put_float_imm_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1091].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
uint32_t bcount = 64;
uint8_t flags = 0;
term_t v = r0;
light_swap_out();
int x = bits_bs_put_float(v, bcount, flags & BSF_LITTLE, &bpc);
light_swap_in();
if (x < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}


ip += 2;
goto *next;
}


l_move_call_35: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1092].counter++;
#endif


r0 = sp[9];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_34: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1093].counter++;
#endif


r0 = tag_int(42);
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_33: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1094].counter++;
#endif


r0 = A_X;
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_32: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1095].counter++;
#endif


r0 = tag_int(4);
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_31: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1096].counter++;
#endif


r0 = tag_int(1000);
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_30: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1097].counter++;
#endif


r0 = A_ATOM;
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_29: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1098].counter++;
#endif


r0 = A_ETS;
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_27: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1099].counter++;
#endif


r0 = tag_int(6);
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_26: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1100].counter++;
#endif


r0 = A_ADD;
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_24: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1101].counter++;
#endif


r0 = tag_int(9);
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_23: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1102].counter++;
#endif


r0 = A_FALSE;
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_22: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1103].counter++;
#endif


r0 = A_FUNNY862;
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_21: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1104].counter++;
#endif


r0 = tag_int(100);
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_20: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1105].counter++;
#endif


r0 = A_FOO;
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_19: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1106].counter++;
#endif


r0 = sp[13];
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_move_call_18: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1107].counter++;
#endif


r0 = tag_int(12);
cp = ip + 2;
ip = (uint32_t *)expand_ptr(ip[1]);
local_reduce();
}

l_is_eq_exact_literal_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1108].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t a = rs[2];
term_t b = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
if (a != b && !are_terms_equal(a, b, 1))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 3;
goto *next;
}


l_bs_init_bits_fail_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1109].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
uint32_t bsz;
int x = bits_calc_bit_size(tmp_arg1, 1, &bsz);
if (x == -TOO_LONG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[2]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	raise_error(A_SYSTEM_LIMIT);
}
if (x < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[2]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
int uneven = (bsz & 7) != 0;
uint32_t osz = (bsz +7) /8;
int needed = (osz <= MAX_HEAP_BIN)
	?WSIZE(t_heap_bin_t) + (osz +3)/4
	:WSIZE(t_proc_bin_t);
if (uneven)
	needed += WSIZE(t_sub_bin_t);
needed += ip[1];
light_swap_out();
uint32_t *p = heap_alloc(&proc->hp, needed);
term_t bin = tag_boxed(p);
int is_writable = 1;
if (osz <= MAX_HEAP_BIN)
{
	box_heap_bin(p, osz, 0);
	is_writable = 0;
}
else
{
	binnode_t *node = binnode_make(osz);
	t_proc_bin_t *pb = (t_proc_bin_t *)p;
	box_proc_bin(p, osz, node);
	proc_bin_link(&proc->hp.proc_bins, pb, &proc->hp.total_pb_size);
}
if (uneven)
{
	term_t parent = bin;
	bin = tag_boxed(p);
	box_sub_bin(p, parent, 0, bsz, is_writable);
}
heap_set_top0(&proc->hp, p);
light_swap_in();
bits_get_real(peel_boxed(bin), &bpc);
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = bin;
		else
			rs[reg__] = bin;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = bin;
	}
}





ip += 5;
goto *next;
}


l_jump_on_val_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1110].counter++;
#endif


term_t v = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (!is_int(v))
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
int base = (int)ip[3];
int i = int_value(v);
if (i < base || i >= base + (int)ip[4])
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
uint32_t *jump_tab = ip + 5;
ip = expand_ptr(jump_tab[i - base]);
next();
}

l_bs_init_fail_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1111].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
// tmp_arg1 is the byte size of the binary

uint32_t osz;
int x = bits_calc_byte_size(tmp_arg1, &osz);
if (x == -TOO_LONG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[2]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	raise_error(A_SYSTEM_LIMIT);
}
if (x < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[2]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}

if (osz >= BS_INIT_FAIL_GC_THRESH)
{
	swap_out();
	proc_burn_fat(proc, 0, rs, ((ip[4] >> 0) & 255));
	swap_in();
}

binnode_t *node = binnode_make(osz);
int needed = WSIZE(t_proc_bin_t) + WSIZE(t_sub_bin_t) + ip[1];
light_swap_out();
uint32_t *p = heap_alloc_N(&proc->hp, needed);
if (p == 0)
{
	binnode_destroy(node);
	no_memory_signal();
}
t_proc_bin_t *pb = (t_proc_bin_t *)p;
box_proc_bin(p, osz, node);
heap_set_top0(&proc->hp, p);
proc_bin_link(&proc->hp.proc_bins, pb, &proc->hp.total_pb_size);
light_swap_in();
bits_get_real(pb, &bpc);
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(pb);
		else
			rs[reg__] = tag_boxed(pb);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(pb);
	}
}



ip += 5;
goto *next;
}


l_call_ext_only_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1112].counter++;
#endif


export_t *exp = (preloaded_exports+617);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_call_ext_only_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1113].counter++;
#endif


export_t *exp = (preloaded_exports+616);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_only_9: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1114].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
export_t *exp = (preloaded_exports+618);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_only_8: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1115].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
export_t *exp = (preloaded_exports+619);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_only_5: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1116].counter++;
#endif


r0 = A_TYPE;
export_t *exp = (preloaded_exports+608);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

bif1_body_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1117].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t r = ((bif_func1_t)bif_hd1)(sp[2], proc);
if (r == noval)
	raise_error(proc->bif_excep_reason);
rs[2] = r;


ip += 1;
goto *next;
}


l_select_tuple_arity_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1118].counter++;
#endif


term_t tuple = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
if (!is_tuple(tuple))
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
uint32_t v = *peel_tuple(tuple);
pair_t *alpha = (pair_t *)(ip + 4);
pair_t *beta = (pair_t *)((uint32_t *)alpha + ip[3]);

if (v < alpha->f || v > beta[-1].f)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);

//TODO: in BEAM the binary search parameters are
// made into unsigned values for performance

while (beta > alpha+1)	// at least 2 pairs
{
	pair_t *mid = alpha + (beta - alpha +1)/2;
	if ((term_t)mid->f > v)
		beta = mid;
	else
		alpha = mid;
}

assert(beta == alpha+1);
if (alpha->f == v)
{
	ip = (uint32_t *)expand_ptr(alpha->s);
	next();
}

do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);


}

l_fetch_22: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1119].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
tmp_arg2 = sp[6];


ip += 2;
goto *next;
}


l_fetch_11: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1120].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = (int8_t)((ip[1] >> 0) & 255);
tmp_arg2 = sp[((ip[1] >> 8) & 255)+1];


ip += 2;
goto *next;
}


l_fetch_9: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1121].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = sp[((ip[1] >> 0) & 255)+1];
tmp_arg2 = (int8_t)((ip[1] >> 8) & 255);


ip += 2;
goto *next;
}


l_fetch_5: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1122].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = (int8_t)((ip[1] >> 0) & 255);
tmp_arg2 = ((((ip[1] >> 8) & 255) == 0) ?r0 :rs[((ip[1] >> 8) & 255)]);


ip += 2;
goto *next;
}


l_fetch_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1123].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
tmp_arg1 = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
tmp_arg2 = (int8_t)((ip[1] >> 8) & 255);


ip += 2;
goto *next;
}


l_catch_7: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1124].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[20+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
proc->catch_level++;


ip += 2;
goto *next;
}


l_bs_get_integer_8_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1125].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends - mc->bs.starts < 8)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
uint32_t v;
bits_get_octet(&mc->bs, v);
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_int(v);
		else
			rs[reg__] = tag_int(v);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_int(v);
	}
}



ip += 4;
goto *next;
}


l_bs_get_integer_16_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1126].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends - mc->bs.starts < 16)
	do {
ip = (uint32_t *)expand_ptr(ip[2]);
next();
} while (0);
uint32_t vh, vl;
bits_get_octet(&mc->bs, vh);
bits_get_octet(&mc->bs, vl);
uint32_t v = (vh << 8) | vl;
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_int(v);
		else
			rs[reg__] = tag_int(v);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_int(v);
	}
}



ip += 4;
goto *next;
}


move_12: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1127].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
rs[10] = nil;


ip += 1;
goto *next;
}


l_bs_utf8_size_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1128].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t point = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
int sz = 4;
if (is_int(point))
{
int n = int_value(point);
if (n >= 0)
{
	if (n <= 0x7f)
		sz = 1;
	else if (n <= 0x7ff)
		sz = 2;
	else if (n <= 0xffff)
		sz = 3;
}
}
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_int(sz);
		else
			rs[reg__] = tag_int(sz);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_int(sz);
	}
}



ip += 3;
goto *next;
}


l_bs_utf16_size_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1129].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t point = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
int sz = 4;
if (is_int(point))
{
int n = int_value(point);
if (n >= 0)
{
	if (n <= 0xffff)
		sz = 2;
}
}
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_int(sz);
		else
			rs[reg__] = tag_int(sz);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_int(sz);
	}
}



ip += 3;
goto *next;
}


l_move_call_ext_49: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1130].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+620);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_48: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1131].counter++;
#endif


r0 = A_FUNC;
cp = ip + 2;
export_t *exp = (export_t *)expand_ptr(ip[1]);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_45: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1132].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+578);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_43: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1133].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+621);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_42: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1134].counter++;
#endif


r0 = A_AUTO_REPAIR;
cp = ip + 1;
export_t *exp = (preloaded_exports+617);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_41: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1135].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+622);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_40: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1136].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+623);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_39: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1137].counter++;
#endif


r0 = tag_int(10);
cp = ip + 1;
export_t *exp = (preloaded_exports+624);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_34: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1138].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+625);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_33: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1139].counter++;
#endif


r0 = A_FUNKY;
cp = ip + 1;
export_t *exp = (preloaded_exports+626);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_32: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1140].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+627);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_30: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1141].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+553);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_29: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1142].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+594);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_27: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1143].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+611);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_23: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1144].counter++;
#endif


r0 = tag_int(5);
cp = ip + 1;
export_t *exp = (preloaded_exports+624);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_20: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1145].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+572);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_18: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1146].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+628);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_11: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1147].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+616);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

l_move_call_ext_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1148].counter++;
#endif


r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
cp = ip + 2;
export_t *exp = (preloaded_exports+619);
if (unlikely(exp == 0 || exp->entry == 0))
{
	rs[0] = r0;
	light_swap_out();
	term_t args = heap_vector_to_list(&proc->hp, rs, exp->arity);
	light_swap_in();
	r0 = exp->module;
	rs[1] = exp->function;
	rs[2] = args;
	exp = EH_UNDEF_EXP;
}
ip = exp->entry;

reds_left--; \
if (unlikely(reds_left <= 0)) {
	proc->cap.live = exp->arity;
	light_swap_out();
	goto yield;
}
next(); \

;
}

catch_end_7: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1149].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
sp[20+1] = nil;
proc->catch_level--;

if (r0 == noval)
{
	assert(rs[1] == A_ERROR || rs[1] == A_THROW || rs[1] == A_EXIT);
	if (rs[1] == A_THROW)
		r0 = rs[2];
	else
	{
		// A very nice spot for a garbage collection:
		// a few live registers and a portion of the
		// stack just gone.
		//
		
		swap_out();
		proc_burn_fat(proc, 0, rs, 3);

		// Both error and exit exceptions are converted to {'EXIT',Reason}
	
		if (rs[1] == A_ERROR)
			rs[2] = heap_tuple2(&proc->hp, rs[2], proc->stack_trace);
		rs[0] = heap_tuple2(&proc->hp, AEXIT__, rs[2]);
		swap_in();
	}
}




ip += 1;
goto *next;
}


test_heap_1_put_list_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1150].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
if (unlikely(hend - htop < ((ip[1] >> 0) & 255)))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, 1);
		heap_ensure(&proc->hp, ((ip[1] >> 0) & 255), root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, ((ip[1] >> 0) & 255));
	swap_in();
}


term_t hd = (int8_t)((ip[1] >> 8) & 255);
term_t tl = r0;
r0 = tag_cons(htop);
*htop++ = hd;
*htop++ = tl;


ip += 2;
goto *next;
}


l_bs_add_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1151].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
uint32_t bcount1, bcount2;
int x = bits_calc_bit_size(tmp_arg1, 1, &bcount1);
int y = bits_calc_bit_size(tmp_arg2, ((ip[3] >> 0) & 255), &bcount2);
if (x == 0 && y == 0)
{
	int64_t bcount = (int64_t)bcount1 + bcount2;
	if (bcount > MAX_BIT_SIZE)
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		raise_error(A_SYSTEM_LIMIT);
	}
	light_swap_out();
	term_t v = int_to_term(bcount, &proc->hp);
	light_swap_in();
	{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = v;
		else
			rs[reg__] = v;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = v;
	}
}

}
else if (x == -BAD_ARG || y == -BAD_ARG)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
else
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	raise_error(A_SYSTEM_LIMIT);
}


ip += 4;
goto *next;
}


l_band_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1152].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
term_t r;
if (are_both_int(tmp_arg1, tmp_arg2))
	r = tmp_arg1 & tmp_arg2;	//tag intact, never a carry
else
{
	light_swap_out();
	r = mixed_band(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, A_BAND, tmp_arg1, tmp_arg2);
	}
}
r0 = r;


ip += 3;
goto *next;
}


on_load_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1153].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);

//printk("TODO: iop on_load not implemented\n");
printk("TODO: iop {on_load,[]} not implemented\n");

light_swap_out();
term_t op_name = heap_strz(&proc->hp, "on_load");
term_t reason = heap_tuple2(&proc->hp, A_NOT_IMPLEMENTED, op_name);
light_swap_in();

raise_error(reason);


ip += 1;
goto *next;
}


l_get_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1154].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t r = lookup_process_dictionary(A_ASN1_MODULE, proc->dictionary);
sp[((ip[1] >> 0) & 255)+1] = r;


ip += 2;
goto *next;
}


l_get_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1155].counter++;
#endif


void *next = (void *)expand_ptr(ip[1]);
term_t r = lookup_process_dictionary(A_ASN1_MODULE, proc->dictionary);
sp[1] = r;


ip += 1;
goto *next;
}


if_end_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1156].counter++;
#endif


raise_error(A_IF_CLAUSE);
}

l_wait_timeout_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1157].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
uint32_t millis = 3000;
if (millis == 0)
{
	ip += 2;
	goto *next;
}
proc->result.what = SLICE_RESULT_WAIT;
uint64_t now = monotonic_clock();
proc->result.until_when = now + (uint64_t)millis * 1000000;
proc->result.jump_to = ip + 2;

ip = (uint32_t *)expand_ptr(ip[1]);

proc->cap.live = 0;
light_swap_out();
goto schedule;
}

l_wait_timeout_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1158].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
uint32_t millis = 1000;
if (millis == 0)
{
	ip += 2;
	goto *next;
}
proc->result.what = SLICE_RESULT_WAIT;
uint64_t now = monotonic_clock();
proc->result.until_when = now + (uint64_t)millis * 1000000;
proc->result.jump_to = ip + 2;

ip = (uint32_t *)expand_ptr(ip[1]);

proc->cap.live = 0;
light_swap_out();
goto schedule;
}

system_limit_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1159].counter++;
#endif


do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
raise_error(A_SYSTEM_LIMIT);
}

l_plus_3: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1160].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int v = int_value(tmp_arg1) + int_value(tmp_arg2);
	if (likely(fits_int(v)))
	{
		{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_int(v);
		else
			rs[reg__] = tag_int(v);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_int(v);
	}
}

	}
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(bn);
		else
			rs[reg__] = tag_boxed(bn);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(bn);
	}
}

	}
}
else
{
	light_swap_out();
	term_t r = mixed_add(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, APLUS__, tmp_arg1, tmp_arg2);
	}
	{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}

}


ip += 4;
goto *next;
}


l_plus_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1161].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (are_both_int(tmp_arg1, tmp_arg2))
{
	int v = int_value(tmp_arg1) + int_value(tmp_arg2);
	if (likely(fits_int(v)))
	{
		r0 = tag_int(v);
	}
	else
	{
		light_swap_out();
		bignum_t *bn = bignum_from_int(&proc->hp, v);
		light_swap_in();
		r0 = tag_boxed(bn);
	}
}
else
{
	light_swap_out();
	term_t r = mixed_add(tmp_arg1, tmp_arg2, &proc->hp);
	light_swap_in();
	if (is_atom(r))
	{
		do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
		bif_error2(r, A_ERLANG, APLUS__, tmp_arg1, tmp_arg2);
	}
	r0 = r;
}


ip += 3;
goto *next;
}


l_gc_bif3_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1162].counter++;
#endif


void *next = (void *)expand_ptr(ip[6]);
// Src - Arg1
// tmp_arg1 - Arg2
// tmp_arg2 - Arg3
term_t t = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
swap_out();
term_t r = ((gc_bif_func3_t)(bif_func_t)expand_ptr(ip[2]))
					(t, tmp_arg1, tmp_arg2, proc, rs, ((ip[5] >> 0) & 255));
swap_in();
if (r == noval)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_func_t entry = (bif_func_t)expand_ptr(ip[2]);
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);
	assert(bif_exp->arity == 3);
	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = 3;
	rs[0] = t;
	rs[1] = tmp_arg1;
	rs[2] = tmp_arg2;
	rs[3] = proc->bif_excep_reason;
	goto raise_from_bif;
}
{
	term_t dst__ = ip[4];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 6;
goto *next;
}


move2_10: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1163].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
		else
			rs[reg__] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = (term_t)(is_reg(ip[1]) ?(ip[1] == reg0) ?r0 :rs[reg_index(ip[1])] :is_slot(ip[1]) ?sp[slot_index(ip[1])+1] :ip[1]);
	}
}

{
	term_t dst__ = ip[4];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
		else
			rs[reg__] = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
	}
}



ip += 5;
goto *next;
}


move2_9: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1164].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
{
	int reg__ = ((ip[1] >> 0) & 255);
	if (reg__ == 0)
		r0 = r0;
	else
		rs[reg__] = r0;
}

{
	int reg__ = ((ip[1] >> 8) & 255);
	if (reg__ == 0)
		r0 = r0;
	else
		rs[reg__] = r0;
}



ip += 2;
goto *next;
}


move2_8: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1165].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
{
	int reg__ = ((ip[1] >> 0) & 255);
	if (reg__ == 0)
		r0 = r0;
	else
		rs[reg__] = r0;
}

{
	int reg__ = ((ip[1] >> 16) & 255);
	if (reg__ == 0)
		r0 = ((((ip[1] >> 8) & 255) == 0) ?r0 :rs[((ip[1] >> 8) & 255)]);
	else
		rs[reg__] = ((((ip[1] >> 8) & 255) == 0) ?r0 :rs[((ip[1] >> 8) & 255)]);
}



ip += 2;
goto *next;
}


move2_7: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1166].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
{
	int reg__ = ((ip[1] >> 0) & 255);
	if (reg__ == 0)
		r0 = r0;
	else
		rs[reg__] = r0;
}

r0 = ((((ip[1] >> 8) & 255) == 0) ?r0 :rs[((ip[1] >> 8) & 255)]);


ip += 2;
goto *next;
}


move2_6: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1167].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
{
	int reg__ = ((ip[1] >> 8) & 255);
	if (reg__ == 0)
		r0 = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
	else
		rs[reg__] = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
}

{
	int reg__ = ((ip[1] >> 16) & 255);
	if (reg__ == 0)
		r0 = r0;
	else
		rs[reg__] = r0;
}



ip += 2;
goto *next;
}


move2_5: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1168].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
r0 = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
{
	int reg__ = ((ip[1] >> 16) & 255);
	if (reg__ == 0)
		r0 = ((((ip[1] >> 8) & 255) == 0) ?r0 :rs[((ip[1] >> 8) & 255)]);
	else
		rs[reg__] = ((((ip[1] >> 8) & 255) == 0) ?r0 :rs[((ip[1] >> 8) & 255)]);
}



ip += 2;
goto *next;
}


move2_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1169].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[((ip[1] >> 0) & 255)+1] = r0;
sp[((ip[1] >> 16) & 255)+1] = ((((ip[1] >> 8) & 255) == 0) ?r0 :rs[((ip[1] >> 8) & 255)]);


ip += 2;
goto *next;
}


move2_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1170].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
sp[((ip[1] >> 8) & 255)+1] = ((((ip[1] >> 0) & 255) == 0) ?r0 :rs[((ip[1] >> 0) & 255)]);
sp[((ip[1] >> 16) & 255)+1] = r0;


ip += 2;
goto *next;
}


l_bs_skip_bits_all2_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1171].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
uint8_t u = ((ip[3] >> 0) & 255);
uint32_t skipped = mc->bs.ends - mc->bs.starts;
if (skipped % u != 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
mc->bs.starts = mc->bs.ends;


ip += 4;
goto *next;
}


l_bs_test_zero_tail2_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1172].counter++;
#endif


void *next = (void *)expand_ptr(ip[2]);
term_t bin = rs[4];
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);
if (mc->bs.ends > mc->bs.starts)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);


ip += 2;
goto *next;
}


l_bs_get_integer_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1173].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
//tmp_arg1: MatchCtx
//tmp_arg2: Sz
assert(is_boxed(tmp_arg1) && boxed_tag(peel_boxed(tmp_arg1)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(tmp_arg1);
if (!is_int(tmp_arg2) && !(is_boxed(tmp_arg2) && is_bignum(peel_boxed(tmp_arg2))))
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
uint32_t bcount;
if (bits_calc_bit_size(tmp_arg2, ((ip[3] >> 8) & 255), &bcount) < 0)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
if (mc->bs.ends - mc->bs.starts < bcount)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	badarg();
}
uint8_t flags = ((ip[3] >> 16) & 255);
light_swap_out();
term_t v = bits_bs_get_integer_imm(mc,
	bcount, flags & BSF_SIGNED, flags & BSF_LITTLE, &proc->hp);
light_swap_in();
{
	term_t dst__ = ip[2];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = v;
		else
			rs[reg__] = v;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = v;
	}
}



ip += 4;
goto *next;
}


l_bs_get_binary2_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1174].counter++;
#endif


void *next = (void *)expand_ptr(ip[6]);
term_t bin = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
assert(is_boxed(bin) && boxed_tag(peel_boxed(bin)) == SUBTAG_MATCH_CTX);
t_match_ctx_t *mc = (t_match_ctx_t *)peel_boxed(bin);

term_t s = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
if (!is_int(s) && !(is_boxed(s) && is_bignum(peel_boxed(s))))
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
uint32_t bcount;
if (bits_calc_bit_size(s, ((ip[5] >> 8) & 255), &bcount) < 0)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);
if (mc->bs.ends - mc->bs.starts < bcount)
	do {
ip = (uint32_t *)expand_ptr(ip[1]);
next();
} while (0);

light_swap_out();
int needed = WSIZE(t_sub_bin_t);
uint32_t *p = heap_alloc(&proc->hp, needed);
t_sub_bin_t *sb = (t_sub_bin_t *)p;
box_sub_bin(p, mc->parent, mc->bs.starts, mc->bs.starts+bcount, 0);
heap_set_top(&proc->hp, p);
light_swap_in();

mc->bs.starts += bcount;
{
	term_t dst__ = ip[4];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = tag_boxed(sb);
		else
			rs[reg__] = tag_boxed(sb);
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = tag_boxed(sb);
	}
}



ip += 6;
goto *next;
}


fmove_2_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1175].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
	// fr s
double dbl = fr[((ip[2] >> 0) & 255)];
term_t v = tag_boxed(htop);
assert(hend - htop >= WSIZE(t_float_t));	// compiler should care
((t_float_t *)htop)->hdr = HDR_IS_NOT_CP | SUBTAG_FLOAT;
((t_float_t *)htop)->val = dbl;
htop += WSIZE(t_float_t);
{
	term_t dst__ = ip[1];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = v;
		else
			rs[reg__] = v;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = v;
	}
}



ip += 3;
goto *next;
}


l_gc_bif2_0: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1176].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
// tmp_arg1 - Arg1
// tmp_arg2 - Arg2
swap_out();
term_t r = ((gc_bif_func2_t)(bif_func_t)expand_ptr(ip[2]))
					(tmp_arg1, tmp_arg2, proc, rs, ((ip[4] >> 0) & 255));
swap_in();
if (r == noval)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_func_t entry = (bif_func_t)expand_ptr(ip[2]);
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);
	assert(bif_exp->arity == 2);
	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = 2;
	rs[0] = tmp_arg1;
	rs[1] = tmp_arg2;
	rs[2] = proc->bif_excep_reason;
	goto raise_from_bif;
}
{
	term_t dst__ = ip[3];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 5;
goto *next;
}


l_gc_bif1_4: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1177].counter++;
#endif


void *next = (void *)expand_ptr(ip[6]);
term_t t = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
swap_out();
term_t r = ((gc_bif_func1_t)(bif_func_t)expand_ptr(ip[2]))
					(t, proc, rs, ((ip[5] >> 0) & 255));
swap_in();
if (r == noval)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_func_t entry = (bif_func_t)expand_ptr(ip[2]);
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);
	assert(bif_exp->arity == 1);
	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = 1;
	rs[0] = t;
	rs[1] = proc->bif_excep_reason;
	goto raise_from_bif;
}
{
	term_t dst__ = ip[4];
	if (is_reg(dst__))
	{
		int reg__ = reg_index(dst__);
		if (reg__ == 0)
			r0 = r;
		else
			rs[reg__] = r;
	}
	else
	{
		assert(is_slot(dst__));
		sp[slot_index(dst__)+1] = r;
	}
}



ip += 6;
goto *next;
}


l_gc_bif1_2: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1178].counter++;
#endif


void *next = (void *)expand_ptr(ip[5]);
term_t t = (term_t)(is_reg(ip[3]) ?(ip[3] == reg0) ?r0 :rs[reg_index(ip[3])] :is_slot(ip[3]) ?sp[slot_index(ip[3])+1] :ip[3]);
swap_out();
term_t r = ((gc_bif_func1_t)(bif_func_t)expand_ptr(ip[2]))
					(t, proc, rs, ((ip[4] >> 0) & 255));
swap_in();
if (r == noval)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_func_t entry = (bif_func_t)expand_ptr(ip[2]);
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);
	assert(bif_exp->arity == 1);
	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = 1;
	rs[0] = t;
	rs[1] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 5;
goto *next;
}


l_gc_bif1_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1179].counter++;
#endif


void *next = (void *)expand_ptr(ip[4]);
term_t t = (term_t)(is_reg(ip[2]) ?(ip[2] == reg0) ?r0 :rs[reg_index(ip[2])] :is_slot(ip[2]) ?sp[slot_index(ip[2])+1] :ip[2]);
swap_out();
term_t r = ((gc_bif_func1_t)gc_bif_length1)
					(t, proc, rs, ((ip[3] >> 0) & 255));
swap_in();
if (r == noval)
{
	do {
	uint32_t *fail__ = (uint32_t *)expand_ptr(ip[1]);
	if (fail__ != 0)
	{
		ip = fail__;
		next();
	}
} while (0)

;
	bif_func_t entry = gc_bif_length1;
	export_t *bif_exp = code_base_lookup_bif_by_entry(entry);
	assert(bif_exp != 0);
	assert(bif_exp->arity == 1);
	raise_bif_mfa.mod = bif_exp->module;
	raise_bif_mfa.fun = bif_exp->function;
	raise_bif_mfa.arity = 1;
	rs[0] = t;
	rs[1] = proc->bif_excep_reason;
	goto raise_from_bif;
}
r0 = r;


ip += 4;
goto *next;
}


test_heap_1: ATTRIBUTE_COLD
 {
#ifdef EXP_COUNT_IOPS
	ling_opcodes[1180].counter++;
#endif


void *next = (void *)expand_ptr(ip[3]);
if (unlikely(hend - htop < ip[1]))
{
	swap_out();
	int nr_regs = proc_count_root_regs(proc);
	if (nr_regs <= MAX_ROOT_REGS && !proc->hp.suppress_gc)
	{
		region_t root_regs[nr_regs];
		proc_fill_root_regs(proc, root_regs, rs, ((ip[2] >> 0) & 255));
		heap_ensure(&proc->hp, ip[1], root_regs, nr_regs);
	}
	else
		heap_alloc(&proc->hp, ip[1]);
	swap_in();
}




ip += 3;
goto *next;
}




badarg:
{
	light_swap_out();
	rs[1] = A_ERROR;
	rs[2] = A_BADARG;
	proc->stack_trace = noval;
	goto exception;
}

// bif_error1() and bif_error2() end up here
// TODO: simplify, disregard arguments
//
raise_from_arith_bif:
{
	// raise_bif_mfa must be filled
	light_swap_out();

	//rs[0] .. rs[arity-1] - arguments
	//rs[arity] - reason
	
	proc->stack_trace = get_stack_trace(ip, cp,
				sp, proc_stack_bottom(proc),
				0, noval, &proc->hp);

	term_t reason = rs[raise_bif_mfa.arity];
	rs[1] = A_ERROR;
	rs[2] = reason;
	goto exception;
}

raise_from_bif:
{
	// raise_bif_mfa must be filled
	light_swap_out();

	//rs[0] .. rs[arity-1] - arguments
	//rs[arity] - reason
	
	term_t args0 = heap_vector_to_list(&proc->hp,
		   		rs, raise_bif_mfa.arity);
	proc->stack_trace = get_stack_trace(ip, cp,
				sp, proc_stack_bottom(proc),
				&raise_bif_mfa, args0, &proc->hp);

	term_t reason = rs[raise_bif_mfa.arity];
	rs[1] = A_ERROR;
	rs[2] = reason;
	/* FALLTHROUGH */
}

exception:
{
	//
	// must be swapped out (lightly)
	// exception class (rs[1]), reason (rs[2])
	// and proc->stack_trace must be set
	//
	
	proc->last_excep_class = rs[1];		//XXX: save class for possible future (raise s,s)
	
	//if (rs[1] == A_ERROR && proc->stack_trace == noval)
	if ((rs[1] == A_ERROR || rs[1] == A_THROW) && proc->stack_trace == noval)
		proc->stack_trace = get_stack_trace(ip, cp,
				sp, proc_stack_bottom(proc),
				0, noval, &proc->hp);

	uint32_t *sbot = proc_stack_bottom(proc);
	uint32_t *ptr = sp;
	while (ptr < sbot)
	{
		if (is_catch(*ptr))
		{
			int index = catch_index(*ptr);
			do {
				ptr--;
			} while (!is_boxed(*ptr) || !is_cp(peel_boxed(*ptr)));

			sp = ptr;
			ip = catch_jump_to(index);
			cp = 0;

#ifdef TRACE_HARNESS
			if (trace_mask & TRACE_MASK_EXCEPTION)
			{
				if (rs[1] == A_ERROR)
					printk("TRACEEX: caught [%pt] error:%pt stack %pt\n",
							T(proc->pid), T(rs[2]), T(proc->stack_trace));
				else
					printk("TRACEEX: caught [%pt] %pt:%pt\n",
							T(proc->pid), T(rs[1]), T(rs[2]));
			}
#endif

			r0 = noval;

			light_swap_in();
			next();
		}
		ptr++;
	}

#ifdef TRACE_HARNESS
if (trace_mask & TRACE_MASK_EXCEPTION)
{
	if (rs[1] == A_ERROR)
		printk("TRACEEX: [%pt] error:%pt stack %pt\n",
				T(proc->pid), T(rs[2]), T(proc->stack_trace));
	else
		printk("TRACEEX: [%pt] %pt:%pt\n",
				T(proc->pid), T(rs[1]), T(rs[2]));
}
#endif

	if (rs[1] == A_THROW)
		proc->result.what = SLICE_RESULT_THROW;
	else if (rs[1] == A_EXIT)
		proc->result.what = SLICE_RESULT_EXIT;
	else // A_ERROR
	{
		assert(rs[1] == A_ERROR);

		// Attach a stack trace to the error reason
		rs[2] = heap_tuple2(&proc->hp, rs[2], proc->stack_trace);

		proc->result.what = SLICE_RESULT_ERROR;
	}

	proc->result.reason = rs[2];

	proc->cap.live = 0;
	// is swapped out (light)
	goto schedule;
}

initialize:
{
	static opcode_info_t opcodes[] = { 
			{ .label = &&get_tuple_element_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_0",
#endif
	  .arg_size = 1 },

	{ .label = &&move_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_0",
#endif
	  .arg_size = 1 },

	{ .label = &&get_tuple_element_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_1",
#endif
	  .arg_size = 1 },

	{ .label = &&move_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_3",
#endif
	  .arg_size = 1 },

	{ .label = &&is_tuple_of_arity_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_of_arity_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_new_bs_put_integer_imm_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_integer_imm_0",
#endif
	  .arg_size = 3 },

	{ .label = &&move_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_0",
#endif
	  .arg_size = 1 },

	{ .label = &&move2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move2_0",
#endif
	  .arg_size = 1 },

	{ .label = &&test_heap_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_heap_0",
#endif
	  .arg_size = 1 },

	{ .label = &&move_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_put_tuple_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_put_tuple_0",
#endif
	  .arg_size = 1 },

	{ .label = &&move2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move2_1",
#endif
	  .arg_size = 2 },

	{ .label = &&move2_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move2_3",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_only_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_only_0",
#endif
	  .arg_size = 1 },

	{ .label = &&get_list_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_list_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fetch_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_2",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_allocate_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_0",
#endif
	  .arg_size = 1 },

	{ .label = &&move_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_4",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_allocate_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_allocate_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_select_val2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_0",
#endif
	  .arg_size = 5 },

	{ .label = &&l_gc_bif1_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_gc_bif1_0",
#endif
	  .arg_size = 4 },

	{ .label = &&l_is_eq_exact_immed_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_2",
#endif
	  .arg_size = 2 },

	{ .label = &&deallocate_return_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_1",
#endif
	  .arg_size = 0 },

	{ .label = &&get_list_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_list_3",
#endif
	  .arg_size = 2 },

	{ .label = &&l_allocate_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_0",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nonempty_list_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_3",
#endif
	  .arg_size = 2 },

	{ .label = &&l_is_eq_exact_immed_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_110,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_110",
#endif
	  .arg_size = 1 },

	{ .label = &&is_tuple_of_arity_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_of_arity_0",
#endif
	  .arg_size = 1 },

	{ .label = &&put_list_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_fun_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_fun_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_add_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_add_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_integer_small_imm_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_small_imm_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_fetch_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_1",
#endif
	  .arg_size = 2 },

	{ .label = &&call_bif_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_8",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_immed_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_6",
#endif
	  .arg_size = 2 },

	{ .label = &&deallocate_return_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_0",
#endif
	  .arg_size = 0 },

	{ .label = &&put_list_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_1",
#endif
	  .arg_size = 2 },

	{ .label = &&return_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "return_0",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_2",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_2",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_start_match2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_start_match2_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_is_eq_exact_immed_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_5",
#endif
	  .arg_size = 2 },

	{ .label = &&is_tuple_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_trim_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_0",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_get_integer_16_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_16_0",
#endif
	  .arg_size = 2 },

	{ .label = &&deallocate_return_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_2",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_last_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_0",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_fetch_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_select_val_atoms_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val_atoms_0",
#endif
	  .arg_size = 2 },

	{ .label = &&put_list_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_2",
#endif
	  .arg_size = 2 },

	{ .label = &&move_return_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_5",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_47,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_47",
#endif
	  .arg_size = 1 },

	{ .label = &&l_new_bs_put_binary_all_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_binary_all_0",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nil_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bif2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bif2_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_allocate_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_2",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_immed_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_7",
#endif
	  .arg_size = 2 },

	{ .label = &&move_deallocate_return_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_deallocate_return_0",
#endif
	  .arg_size = 1 },

	{ .label = &&move_return_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_1",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_7",
#endif
	  .arg_size = 0 },

	{ .label = &&move_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fetch_18,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_18",
#endif
	  .arg_size = 1 },

	{ .label = &&call_bif_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_16",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_15",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_14",
#endif
	  .arg_size = 0 },

	{ .label = &&l_increment_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_increment_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_init_heap_bin_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_init_heap_bin_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_select_tuple_arity_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_tuple_arity_0",
#endif
	  .arg_size = 2 },

	{ .label = &&put_list_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_3",
#endif
	  .arg_size = 2 },

	{ .label = &&l_select_tuple_arity2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_tuple_arity2_0",
#endif
	  .arg_size = 4 },

	{ .label = &&move_return_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_fetch_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_allocate_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_6",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_last_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_6",
#endif
	  .arg_size = 1 },

	{ .label = &&init2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init2_0",
#endif
	  .arg_size = 1 },

	{ .label = &&call_bif_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_9",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_36,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_36",
#endif
	  .arg_size = 2 },

	{ .label = &&l_new_bs_put_binary_imm_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_binary_imm_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_make_fun_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_make_fun_0",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_6",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_last_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_last_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_ge_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ge_0",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_0",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_48,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_48",
#endif
	  .arg_size = 1 },

	{ .label = &&l_allocate_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_3",
#endif
	  .arg_size = 0 },

	{ .label = &&jump_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "jump_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fetch_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_10",
#endif
	  .arg_size = 1 },

	{ .label = &&is_atom_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_atom_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_get_binary_imm2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_binary_imm2_0",
#endif
	  .arg_size = 3 },

	{ .label = &&extract_next_element2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_2",
#endif
	  .arg_size = 0 },

	{ .label = &&is_tuple_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_2",
#endif
	  .arg_size = 1 },

	{ .label = &&test_arity_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_arity_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_only_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_only_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_catch_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_catch_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_append_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_append_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_select_tuple_arity2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_tuple_arity2_1",
#endif
	  .arg_size = 4 },

	{ .label = &&extract_next_element_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_0",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_41,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_41",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_init_fail_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_init_fail_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_is_function2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_function2_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_allocate_zero_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_zero_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_loop_rec_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_loop_rec_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_select_tuple_arity_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_tuple_arity_1",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nil_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_0",
#endif
	  .arg_size = 1 },

	{ .label = &&move_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_6",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element3_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element3_0",
#endif
	  .arg_size = 0 },

	{ .label = &&remove_message_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "remove_message_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_last_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_last_2",
#endif
	  .arg_size = 2 },

	{ .label = &&allocate_init_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "allocate_init_0",
#endif
	  .arg_size = 1 },

	{ .label = &&move_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_8",
#endif
	  .arg_size = 1 },

	{ .label = &&l_allocate_zero_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_zero_2",
#endif
	  .arg_size = 0 },

	{ .label = &&l_allocate_zero_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_zero_1",
#endif
	  .arg_size = 0 },

	{ .label = &&move_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_11",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_3",
#endif
	  .arg_size = 1 },

	{ .label = &&catch_end_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "catch_end_5",
#endif
	  .arg_size = 0 },

	{ .label = &&is_atom_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_atom_1",
#endif
	  .arg_size = 1 },

	{ .label = &&init_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bif1_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bif1_0",
#endif
	  .arg_size = 3 },

	{ .label = &&init_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_select_val2_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_9",
#endif
	  .arg_size = 5 },

	{ .label = &&apply_last_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "apply_last_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_get_binary_all_reuse_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_binary_all_reuse_0",
#endif
	  .arg_size = 2 },

	{ .label = &&self_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "self_0",
#endif
	  .arg_size = 0 },

	{ .label = &&deallocate_return_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_fetch_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_17",
#endif
	  .arg_size = 1 },

	{ .label = &&set_tuple_element_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "set_tuple_element_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_match_string_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_match_string_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_allocate_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_10",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_only_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_only_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_select_val_smallints_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val_smallints_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_integer_8_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_8_0",
#endif
	  .arg_size = 2 },

	{ .label = &&move_deallocate_return_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_deallocate_return_2",
#endif
	  .arg_size = 1 },

	{ .label = &&is_list_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_list_2",
#endif
	  .arg_size = 1 },

	{ .label = &&is_atom_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_atom_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_allocate_zero_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_zero_0",
#endif
	  .arg_size = 0 },

	{ .label = &&send_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "send_0",
#endif
	  .arg_size = 0 },

	{ .label = &&allocate_heap_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "allocate_heap_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_8",
#endif
	  .arg_size = 2 },

	{ .label = &&l_trim_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_1",
#endif
	  .arg_size = 0 },

	{ .label = &&move_deallocate_return_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_deallocate_return_1",
#endif
	  .arg_size = 1 },

	{ .label = &&init3_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init3_0",
#endif
	  .arg_size = 1 },

	{ .label = &&deallocate_return_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_3",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element3_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element3_2",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_36,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_36",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bif2_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bif2_6",
#endif
	  .arg_size = 3 },

	{ .label = &&is_integer_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_integer_0",
#endif
	  .arg_size = 1 },

	{ .label = &&move_return_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_allocate_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bif2_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bif2_4",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bif2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bif2_1",
#endif
	  .arg_size = 2 },

	{ .label = &&is_atom_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_atom_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_last_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_4",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_6",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_1",
#endif
	  .arg_size = 1 },

	{ .label = &&move_deallocate_return_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_deallocate_return_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_last_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_last_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_binary2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_binary2_0",
#endif
	  .arg_size = 4 },

	{ .label = &&l_is_eq_exact_literal_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_literal_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_24,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_24",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_13",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fetch_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_13",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_last_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_last_3",
#endif
	  .arg_size = 2 },

	{ .label = &&l_new_bs_put_binary_imm_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_binary_imm_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_minus_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_minus_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_last_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_times_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_times_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_init_bits_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_init_bits_0",
#endif
	  .arg_size = 4 },

	{ .label = &&l_fetch_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_14",
#endif
	  .arg_size = 1 },

	{ .label = &&l_increment_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_increment_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_times_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_times_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_integer_32_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_32_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_binary_all2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_binary_all2_0",
#endif
	  .arg_size = 2 },

	{ .label = &&deallocate_return_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_13",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_last_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_last_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_select_val2_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_6",
#endif
	  .arg_size = 4 },

	{ .label = &&l_is_eq_exact_literal_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_literal_5",
#endif
	  .arg_size = 2 },

	{ .label = &&is_binary_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_binary_2",
#endif
	  .arg_size = 1 },

	{ .label = &&test_heap_1_put_list_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_heap_1_put_list_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_lt_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_lt_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_plus_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_plus_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_allocate_zero_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_zero_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_select_val2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_2",
#endif
	  .arg_size = 5 },

	{ .label = &&extract_next_element3_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element3_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_select_val2_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_8",
#endif
	  .arg_size = 5 },

	{ .label = &&call_bif_18,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_18",
#endif
	  .arg_size = 0 },

	{ .label = &&is_integer_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_integer_5",
#endif
	  .arg_size = 2 },

	{ .label = &&is_binary_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_binary_1",
#endif
	  .arg_size = 1 },

	{ .label = &&deallocate_return_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_5",
#endif
	  .arg_size = 0 },

	{ .label = &&l_catch_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_catch_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_trim_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_3",
#endif
	  .arg_size = 0 },

	{ .label = &&is_pid_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_pid_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_select_val2_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_3",
#endif
	  .arg_size = 5 },

	{ .label = &&is_nonempty_list_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_last_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_3",
#endif
	  .arg_size = 1 },

	{ .label = &&is_list_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_list_0",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element3_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element3_7",
#endif
	  .arg_size = 0 },

	{ .label = &&l_select_val2_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_12",
#endif
	  .arg_size = 5 },

	{ .label = &&l_int_div_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_int_div_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_fetch_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_8",
#endif
	  .arg_size = 1 },

	{ .label = &&l_catch_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_catch_2",
#endif
	  .arg_size = 1 },

	{ .label = &&catch_end_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "catch_end_2",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_last_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_last_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_allocate_zero_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_zero_5",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_only_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_only_10",
#endif
	  .arg_size = 2 },

	{ .label = &&extract_next_element_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_5",
#endif
	  .arg_size = 0 },

	{ .label = &&init_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_2",
#endif
	  .arg_size = 0 },

	{ .label = &&try_end_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "try_end_4",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element2_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_3",
#endif
	  .arg_size = 0 },

	{ .label = &&is_tuple_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_ne_exact_immed_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_2",
#endif
	  .arg_size = 2 },

	{ .label = &&is_float_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_float_0",
#endif
	  .arg_size = 1 },

	{ .label = &&try_end_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "try_end_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_4",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_0",
#endif
	  .arg_size = 0 },

	{ .label = &&is_integer_allocate_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_integer_allocate_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_fetch_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_16",
#endif
	  .arg_size = 1 },

	{ .label = &&int_code_end_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "int_code_end_0",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_6",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_50,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_50",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_12",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element2_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_5",
#endif
	  .arg_size = 0 },

	{ .label = &&l_catch_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_catch_4",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_6",
#endif
	  .arg_size = 1 },

	{ .label = &&try_end_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "try_end_2",
#endif
	  .arg_size = 0 },

	{ .label = &&deallocate_return_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_6",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_immed_19,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_19",
#endif
	  .arg_size = 2 },

	{ .label = &&extract_next_element2_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_13",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_only_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_only_1",
#endif
	  .arg_size = 0 },

	{ .label = &&is_port_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_port_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_ext_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_7",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_21,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_21",
#endif
	  .arg_size = 2 },

	{ .label = &&l_is_eq_exact_immed_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_12",
#endif
	  .arg_size = 2 },

	{ .label = &&l_apply_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_apply_0",
#endif
	  .arg_size = 0 },

	{ .label = &&is_list_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_list_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_get_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_get_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_select_val2_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_5",
#endif
	  .arg_size = 4 },

	{ .label = &&node_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "node_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_immed_27,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_27",
#endif
	  .arg_size = 2 },

	{ .label = &&deallocate_return_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_9",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element2_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_10",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_42,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_42",
#endif
	  .arg_size = 0 },

	{ .label = &&l_select_val2_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_13",
#endif
	  .arg_size = 5 },

	{ .label = &&l_move_call_only_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_only_6",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_80,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_80",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_literal_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_literal_4",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_init_bits_fail_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_init_bits_fail_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_last_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_last_5",
#endif
	  .arg_size = 2 },

	{ .label = &&l_new_bs_put_binary_all_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_binary_all_1",
#endif
	  .arg_size = 3 },

	{ .label = &&l_rem_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_rem_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_last_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_last_1",
#endif
	  .arg_size = 2 },

	{ .label = &&move_return_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_14",
#endif
	  .arg_size = 0 },

	{ .label = &&l_plus_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_plus_2",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_put_string_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_put_string_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_new_bs_put_integer_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_integer_0",
#endif
	  .arg_size = 2 },

	{ .label = &&move_deallocate_return_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_deallocate_return_7",
#endif
	  .arg_size = 1 },

	{ .label = &&wait_timeout_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "wait_timeout_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_is_eq_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_only_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_only_2",
#endif
	  .arg_size = 1 },

	{ .label = &&is_integer_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_integer_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fetch_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_12",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_fun_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_fun_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_only_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_only_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_only_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_only_10",
#endif
	  .arg_size = 2 },

	{ .label = &&l_select_val2_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_7",
#endif
	  .arg_size = 5 },

	{ .label = &&l_fetch_23,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_23",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nonempty_list_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_6",
#endif
	  .arg_size = 1 },

	{ .label = &&l_increment_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_increment_3",
#endif
	  .arg_size = 2 },

	{ .label = &&is_list_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_list_1",
#endif
	  .arg_size = 1 },

	{ .label = &&bif1_body_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif1_body_0",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_allocate_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_allocate_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_integer_8_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_8_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_gc_bif1_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_gc_bif1_3",
#endif
	  .arg_size = 3 },

	{ .label = &&apply_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "apply_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_ne_exact_literal_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_literal_0",
#endif
	  .arg_size = 3 },

	{ .label = &&get_list_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_list_2",
#endif
	  .arg_size = 2 },

	{ .label = &&l_element_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_element_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_fetch_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_6",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_ne_exact_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_last_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_2",
#endif
	  .arg_size = 1 },

	{ .label = &&get_tuple_element_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_4",
#endif
	  .arg_size = 1 },

	{ .label = &&get_tuple_element_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_5",
#endif
	  .arg_size = 1 },

	{ .label = &&call_bif_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_12",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nonempty_list_test_heap_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_test_heap_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_0",
#endif
	  .arg_size = 1 },

	{ .label = &&is_integer_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_integer_3",
#endif
	  .arg_size = 1 },

	{ .label = &&call_bif_21,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_21",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_skip_bits_imm2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_skip_bits_imm2_1",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nonempty_list_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_increment_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_increment_1",
#endif
	  .arg_size = 2 },

	{ .label = &&test_arity_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_arity_1",
#endif
	  .arg_size = 2 },

	{ .label = &&put_list_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_4",
#endif
	  .arg_size = 2 },

	{ .label = &&is_tuple_of_arity_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_of_arity_3",
#endif
	  .arg_size = 2 },

	{ .label = &&move_return_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_8",
#endif
	  .arg_size = 0 },

	{ .label = &&is_integer_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_integer_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_test_unit_8_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_unit_8_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_put_string_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_put_string_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_test_zero_tail2_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_zero_tail2_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_get_binary_all_reuse_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_binary_all_reuse_1",
#endif
	  .arg_size = 3 },

	{ .label = &&l_is_eq_exact_immed_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_9",
#endif
	  .arg_size = 2 },

	{ .label = &&put_list_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_5",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_4",
#endif
	  .arg_size = 1 },

	{ .label = &&bif2_body_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif2_body_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fast_element_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fast_element_2",
#endif
	  .arg_size = 2 },

	{ .label = &&l_trim_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_4",
#endif
	  .arg_size = 0 },

	{ .label = &&catch_end_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "catch_end_0",
#endif
	  .arg_size = 0 },

	{ .label = &&get_tuple_element_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_3",
#endif
	  .arg_size = 1 },

	{ .label = &&self_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "self_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_only_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_only_8",
#endif
	  .arg_size = 1 },

	{ .label = &&move_deallocate_return_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_deallocate_return_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_test_unit_8_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_unit_8_1",
#endif
	  .arg_size = 1 },

	{ .label = &&timeout_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "timeout_0",
#endif
	  .arg_size = 0 },

	{ .label = &&is_binary_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_binary_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_allocate_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_7",
#endif
	  .arg_size = 0 },

	{ .label = &&move_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_13",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bif2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bif2_2",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_ext_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_8",
#endif
	  .arg_size = 1 },

	{ .label = &&l_trim_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_2",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_ne_exact_immed_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_4",
#endif
	  .arg_size = 2 },

	{ .label = &&call_bif_29,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_29",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_46,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_46",
#endif
	  .arg_size = 0 },

	{ .label = &&l_catch_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_catch_6",
#endif
	  .arg_size = 1 },

	{ .label = &&recv_mark_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "recv_mark_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_recv_set_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_recv_set_0",
#endif
	  .arg_size = 0 },

	{ .label = &&self_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "self_2",
#endif
	  .arg_size = 0 },

	{ .label = &&catch_end_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "catch_end_6",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_literal_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_literal_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_only_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_only_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_11",
#endif
	  .arg_size = 2 },

	{ .label = &&extract_next_element_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_10",
#endif
	  .arg_size = 0 },

	{ .label = &&l_fetch_20,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_20",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_ne_exact_immed_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_10",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nonempty_list_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_35,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_35",
#endif
	  .arg_size = 2 },

	{ .label = &&l_select_val2_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_4",
#endif
	  .arg_size = 5 },

	{ .label = &&wait_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "wait_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_select_val_smallints_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val_smallints_1",
#endif
	  .arg_size = 2 },

	{ .label = &&move_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_9",
#endif
	  .arg_size = 1 },

	{ .label = &&is_tuple_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_4",
#endif
	  .arg_size = 1 },

	{ .label = &&move_return_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_2",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_only_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_only_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_allocate_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_5",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_11",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element3_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element3_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_12",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fast_element_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fast_element_1",
#endif
	  .arg_size = 1 },

	{ .label = &&move_jump_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_3",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_9",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element2_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_only_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_only_0",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_8",
#endif
	  .arg_size = 1 },

	{ .label = &&l_catch_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_catch_3",
#endif
	  .arg_size = 1 },

	{ .label = &&call_bif_28,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_28",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_28,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_28",
#endif
	  .arg_size = 0 },

	{ .label = &&is_integer_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_integer_2",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_7",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_ne_exact_immed_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_3",
#endif
	  .arg_size = 2 },

	{ .label = &&try_end_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "try_end_5",
#endif
	  .arg_size = 0 },

	{ .label = &&is_tuple_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_3",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element2_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_12",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_last_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_11",
#endif
	  .arg_size = 2 },

	{ .label = &&put_list_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_14",
#endif
	  .arg_size = 3 },

	{ .label = &&move_return_26,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_26",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_44,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_44",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element3_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element3_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_101,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_101",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_17",
#endif
	  .arg_size = 0 },

	{ .label = &&move_jump_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_0",
#endif
	  .arg_size = 1 },

	{ .label = &&init_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_only_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_only_2",
#endif
	  .arg_size = 0 },

	{ .label = &&put_list_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_6",
#endif
	  .arg_size = 2 },

	{ .label = &&get_tuple_element_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_catch_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_catch_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_test_unit_8_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_unit_8_0",
#endif
	  .arg_size = 1 },

	{ .label = &&loop_rec_end_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "loop_rec_end_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_band_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_band_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_ext_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_9",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_5",
#endif
	  .arg_size = 1 },

	{ .label = &&self_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "self_3",
#endif
	  .arg_size = 0 },

	{ .label = &&test_heap_1_put_list_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_heap_1_put_list_1",
#endif
	  .arg_size = 1 },

	{ .label = &&bif2_body_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif2_body_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_skip_bits_imm2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_skip_bits_imm2_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_is_eq_exact_immed_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_16",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_restore2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_restore2_0",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_9",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_6",
#endif
	  .arg_size = 1 },

	{ .label = &&call_bif_27,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_27",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_6",
#endif
	  .arg_size = 1 },

	{ .label = &&catch_end_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "catch_end_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_last_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_last_2",
#endif
	  .arg_size = 1 },

	{ .label = &&get_list_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_list_6",
#endif
	  .arg_size = 1 },

	{ .label = &&move_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_7",
#endif
	  .arg_size = 1 },

	{ .label = &&l_allocate_zero_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_zero_6",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_8",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_37,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_37",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_last_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_last_2",
#endif
	  .arg_size = 2 },

	{ .label = &&extract_next_element_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_12",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_last_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_7",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_26,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_26",
#endif
	  .arg_size = 2 },

	{ .label = &&call_bif_25,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_25",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_22,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_22",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_37,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_37",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_11",
#endif
	  .arg_size = 1 },

	{ .label = &&init_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_4",
#endif
	  .arg_size = 0 },

	{ .label = &&put_list_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_9",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_ne_exact_immed_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_jump_on_val_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_jump_on_val_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_is_ne_exact_immed_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_11",
#endif
	  .arg_size = 3 },

	{ .label = &&l_call_last_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_only_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_only_4",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_11",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_fun_last_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_fun_last_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_25,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_25",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_15",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_14",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_5",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_7",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_save2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_save2_0",
#endif
	  .arg_size = 1 },

	{ .label = &&try_end_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "try_end_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_int_div_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_int_div_0",
#endif
	  .arg_size = 2 },

	{ .label = &&bif1_body_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif1_body_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bor_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bor_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_is_eq_exact_immed_18,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_18",
#endif
	  .arg_size = 2 },

	{ .label = &&extract_next_element_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_7",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bsl_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bsl_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_ext_only_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_only_3",
#endif
	  .arg_size = 1 },

	{ .label = &&deallocate_return_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_7",
#endif
	  .arg_size = 0 },

	{ .label = &&l_jump_on_val_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_jump_on_val_0",
#endif
	  .arg_size = 2 },

	{ .label = &&get_list_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_list_8",
#endif
	  .arg_size = 1 },

	{ .label = &&catch_end_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "catch_end_3",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_8",
#endif
	  .arg_size = 1 },

	{ .label = &&put_list_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_10",
#endif
	  .arg_size = 2 },

	{ .label = &&get_list_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_list_5",
#endif
	  .arg_size = 1 },

	{ .label = &&is_tuple_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_9",
#endif
	  .arg_size = 2 },

	{ .label = &&l_apply_last_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_apply_last_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_43,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_43",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_30,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_30",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bsr_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bsr_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_ext_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_15",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_skip_bits2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_skip_bits2_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_71,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_71",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_ne_exact_immed_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_1",
#endif
	  .arg_size = 2 },

	{ .label = &&is_list_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_list_3",
#endif
	  .arg_size = 1 },

	{ .label = &&init_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_5",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_40,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_40",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_immed_22,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_22",
#endif
	  .arg_size = 2 },

	{ .label = &&extract_next_element_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_13",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_ne_exact_immed_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_8",
#endif
	  .arg_size = 2 },

	{ .label = &&l_get_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_get_1",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_10",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_7",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fast_element_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fast_element_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_21,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_21",
#endif
	  .arg_size = 1 },

	{ .label = &&is_bitstr_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_bitstr_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_select_val2_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_14",
#endif
	  .arg_size = 6 },

	{ .label = &&call_bif_31,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_31",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_96,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_96",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_17",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_immed_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_15",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_51,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_51",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_10",
#endif
	  .arg_size = 0 },

	{ .label = &&move_jump_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_13",
#endif
	  .arg_size = 2 },

	{ .label = &&l_trim_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_5",
#endif
	  .arg_size = 0 },

	{ .label = &&is_function_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_function_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_2",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_26,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_26",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_16",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_8",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element2_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_7",
#endif
	  .arg_size = 0 },

	{ .label = &&l_allocate_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_8",
#endif
	  .arg_size = 0 },

	{ .label = &&bif1_body_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif1_body_7",
#endif
	  .arg_size = 3 },

	{ .label = &&l_call_ext_last_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_last_6",
#endif
	  .arg_size = 2 },

	{ .label = &&l_fetch_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_7",
#endif
	  .arg_size = 0 },

	{ .label = &&deallocate_return_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_10",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_38,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_38",
#endif
	  .arg_size = 0 },

	{ .label = &&init_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_9",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_10",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_9",
#endif
	  .arg_size = 1 },

	{ .label = &&move_deallocate_return_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_deallocate_return_6",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fdiv_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fdiv_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_band_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_band_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_test_zero_tail2_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_zero_tail2_5",
#endif
	  .arg_size = 1 },

	{ .label = &&call_bif_22,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_22",
#endif
	  .arg_size = 0 },

	{ .label = &&l_fcheckerror_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fcheckerror_0",
#endif
	  .arg_size = 0 },

	{ .label = &&fclearerror_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "fclearerror_0",
#endif
	  .arg_size = 0 },

	{ .label = &&allocate_heap_zero_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "allocate_heap_zero_0",
#endif
	  .arg_size = 1 },

	{ .label = &&fconv_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "fconv_0",
#endif
	  .arg_size = 1 },

	{ .label = &&fmove_1_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "fmove_1_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_select_val_atoms_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val_atoms_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_ext_last_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_last_6",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bsl_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bsl_0",
#endif
	  .arg_size = 2 },

	{ .label = &&fmove_2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "fmove_2_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_increment_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_increment_6",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_41,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_41",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_2",
#endif
	  .arg_size = 1 },

	{ .label = &&is_tuple_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_7",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_20,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_20",
#endif
	  .arg_size = 0 },

	{ .label = &&init_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_6",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_last_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_last_4",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_38,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_38",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element3_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element3_6",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_21,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_21",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_ne_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_0",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element2_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_6",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_66,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_66",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nonempty_list_24,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_24",
#endif
	  .arg_size = 1 },

	{ .label = &&move_jump_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_allocate_zero_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_zero_7",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_23,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_23",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_11",
#endif
	  .arg_size = 0 },

	{ .label = &&l_catch_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_catch_8",
#endif
	  .arg_size = 2 },

	{ .label = &&is_pid_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_pid_1",
#endif
	  .arg_size = 2 },

	{ .label = &&is_reference_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_reference_0",
#endif
	  .arg_size = 2 },

	{ .label = &&call_bif_32,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_32",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nonempty_list_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_12",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_20,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_20",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_test_zero_tail2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_zero_tail2_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_4",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element2_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_8",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_16",
#endif
	  .arg_size = 0 },

	{ .label = &&l_rem_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_rem_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_move_call_ext_38,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_38",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_3",
#endif
	  .arg_size = 1 },

	{ .label = &&catch_end_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "catch_end_8",
#endif
	  .arg_size = 1 },

	{ .label = &&self_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "self_6",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_13",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_105,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_105",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_10",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_30,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_30",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_7",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_last_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_8",
#endif
	  .arg_size = 1 },

	{ .label = &&test_arity_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_arity_3",
#endif
	  .arg_size = 2 },

	{ .label = &&is_boolean_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_boolean_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_allocate_zero_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_zero_10",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_26,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_26",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_47,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_47",
#endif
	  .arg_size = 0 },

	{ .label = &&is_list_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_list_6",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nonempty_list_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_15",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_literal_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_literal_7",
#endif
	  .arg_size = 3 },

	{ .label = &&l_move_call_ext_only_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_only_6",
#endif
	  .arg_size = 1 },

	{ .label = &&self_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "self_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_34,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_34",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_immed_24,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_24",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_17",
#endif
	  .arg_size = 0 },

	{ .label = &&is_function_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_function_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_15",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fetch_19,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_19",
#endif
	  .arg_size = 1 },

	{ .label = &&move_return_28,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_28",
#endif
	  .arg_size = 0 },

	{ .label = &&node_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "node_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_45,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_45",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bor_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bor_1",
#endif
	  .arg_size = 2 },

	{ .label = &&move_return_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_7",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element3_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element3_10",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_82,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_82",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nonempty_list_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_11",
#endif
	  .arg_size = 1 },

	{ .label = &&is_atom_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_atom_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_8",
#endif
	  .arg_size = 0 },

	{ .label = &&l_apply_fun_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_apply_fun_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_109,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_109",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element2_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_14",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_16",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_15",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_last_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_last_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_allocate_zero_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_zero_8",
#endif
	  .arg_size = 0 },

	{ .label = &&try_end_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "try_end_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_fetch_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_15",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fast_element_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fast_element_3",
#endif
	  .arg_size = 2 },

	{ .label = &&l_allocate_zero_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_zero_9",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_12",
#endif
	  .arg_size = 1 },

	{ .label = &&l_select_val2_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_10",
#endif
	  .arg_size = 5 },

	{ .label = &&move_deallocate_return_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_deallocate_return_5",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_14",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_6",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_64,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_64",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element3_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element3_5",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_22,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_22",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_33,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_33",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_74,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_74",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_13",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_only_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_only_0",
#endif
	  .arg_size = 1 },

	{ .label = &&init_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_7",
#endif
	  .arg_size = 0 },

	{ .label = &&is_tuple_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_31,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_31",
#endif
	  .arg_size = 0 },

	{ .label = &&try_end_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "try_end_6",
#endif
	  .arg_size = 0 },

	{ .label = &&is_atom_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_atom_6",
#endif
	  .arg_size = 2 },

	{ .label = &&set_tuple_element_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "set_tuple_element_1",
#endif
	  .arg_size = 2 },

	{ .label = &&put_list_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_7",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_last_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_last_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_increment_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_increment_5",
#endif
	  .arg_size = 2 },

	{ .label = &&is_atom_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_atom_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_14",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_55,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_55",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_35,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_35",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_23,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_23",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_10",
#endif
	  .arg_size = 1 },

	{ .label = &&move_return_36,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_36",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_only_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_only_7",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_last_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_10",
#endif
	  .arg_size = 1 },

	{ .label = &&bif1_body_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif1_body_5",
#endif
	  .arg_size = 2 },

	{ .label = &&init_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_8",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_20,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_20",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_immed_36,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_36",
#endif
	  .arg_size = 3 },

	{ .label = &&l_call_ext_42,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_42",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element_21,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_21",
#endif
	  .arg_size = 0 },

	{ .label = &&l_int_bnot_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_int_bnot_0",
#endif
	  .arg_size = 4 },

	{ .label = &&deallocate_return_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_8",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_34,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_34",
#endif
	  .arg_size = 0 },

	{ .label = &&l_put_tuple_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_put_tuple_6",
#endif
	  .arg_size = 1 },

	{ .label = &&node_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "node_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_immed_25,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_25",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_13",
#endif
	  .arg_size = 1 },

	{ .label = &&bif2_body_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif2_body_2",
#endif
	  .arg_size = 1 },

	{ .label = &&init_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_10",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element2_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_17",
#endif
	  .arg_size = 1 },

	{ .label = &&l_new_bs_put_integer_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_integer_2",
#endif
	  .arg_size = 4 },

	{ .label = &&l_move_call_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_16",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_36,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_36",
#endif
	  .arg_size = 1 },

	{ .label = &&is_tuple_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_8",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element2_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_11",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_28,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_28",
#endif
	  .arg_size = 2 },

	{ .label = &&put_list_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_12",
#endif
	  .arg_size = 2 },

	{ .label = &&get_list_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_list_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_get_integer_16_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_16_1",
#endif
	  .arg_size = 2 },

	{ .label = &&catch_end_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "catch_end_4",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_17",
#endif
	  .arg_size = 0 },

	{ .label = &&try_end_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "try_end_7",
#endif
	  .arg_size = 1 },

	{ .label = &&call_bif_43,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_43",
#endif
	  .arg_size = 0 },

	{ .label = &&is_tuple_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_6",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_44,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_44",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_trim_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_8",
#endif
	  .arg_size = 0 },

	{ .label = &&put_list_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_11",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_literal_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_literal_6",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_ext_44,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_44",
#endif
	  .arg_size = 1 },

	{ .label = &&l_get_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_get_6",
#endif
	  .arg_size = 2 },

	{ .label = &&call_bif_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_5",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_91,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_91",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_ne_exact_immed_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_5",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nil_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_10",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_37,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_37",
#endif
	  .arg_size = 1 },

	{ .label = &&node_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "node_2",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_17",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_33,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_33",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_28,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_28",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_61,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_61",
#endif
	  .arg_size = 0 },

	{ .label = &&l_trim_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_6",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_14",
#endif
	  .arg_size = 1 },

	{ .label = &&move_jump_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_7",
#endif
	  .arg_size = 1 },

	{ .label = &&move_jump_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_6",
#endif
	  .arg_size = 1 },

	{ .label = &&move_jump_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_1",
#endif
	  .arg_size = 1 },

	{ .label = &&move_return_19,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_19",
#endif
	  .arg_size = 0 },

	{ .label = &&bs_context_to_binary_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bs_context_to_binary_0",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_0",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nonempty_list_35,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_35",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_19,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_19",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_25,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_25",
#endif
	  .arg_size = 1 },

	{ .label = &&bif1_body_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif1_body_6",
#endif
	  .arg_size = 1 },

	{ .label = &&bif1_body_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif1_body_1",
#endif
	  .arg_size = 0 },

	{ .label = &&bif2_body_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif2_body_3",
#endif
	  .arg_size = 2 },

	{ .label = &&is_float_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_float_1",
#endif
	  .arg_size = 2 },

	{ .label = &&node_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "node_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_last_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_last_4",
#endif
	  .arg_size = 2 },

	{ .label = &&call_bif_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_13",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_92,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_92",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_78,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_78",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_36,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_36",
#endif
	  .arg_size = 0 },

	{ .label = &&move_jump_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_10",
#endif
	  .arg_size = 1 },

	{ .label = &&move_return_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_9",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_test_unit_8_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_unit_8_2",
#endif
	  .arg_size = 1 },

	{ .label = &&fconv_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "fconv_2",
#endif
	  .arg_size = 2 },

	{ .label = &&deallocate_return_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_11",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_only_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_only_0",
#endif
	  .arg_size = 0 },

	{ .label = &&move_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_10",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_28,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_28",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_40,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_40",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_17",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_1",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element_24,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_24",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_ne_exact_immed_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_6",
#endif
	  .arg_size = 2 },

	{ .label = &&l_trim_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_7",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_fun_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_fun_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_apply_fun_only_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_apply_fun_only_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_26,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_26",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_25,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_25",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_skip_bits_all2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_skip_bits_all2_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_98,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_98",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_14",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_11",
#endif
	  .arg_size = 0 },

	{ .label = &&bs_context_to_binary_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bs_context_to_binary_9",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_14",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_only_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_only_7",
#endif
	  .arg_size = 1 },

	{ .label = &&bif1_body_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif1_body_4",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_ext_46,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_46",
#endif
	  .arg_size = 1 },

	{ .label = &&test_heap_1_put_list_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_heap_1_put_list_4",
#endif
	  .arg_size = 2 },

	{ .label = &&self_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "self_5",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_88,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_88",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_86,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_86",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element_19,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_19",
#endif
	  .arg_size = 0 },

	{ .label = &&l_fadd_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fadd_0",
#endif
	  .arg_size = 1 },

	{ .label = &&extract_next_element2_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_16",
#endif
	  .arg_size = 0 },

	{ .label = &&move_jump_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_12",
#endif
	  .arg_size = 1 },

	{ .label = &&move_return_45,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_45",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_29,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_29",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_13",
#endif
	  .arg_size = 0 },

	{ .label = &&move_deallocate_return_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_deallocate_return_8",
#endif
	  .arg_size = 1 },

	{ .label = &&is_bigint_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_bigint_0",
#endif
	  .arg_size = 2 },

	{ .label = &&fmove_2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "fmove_2_0",
#endif
	  .arg_size = 1 },

	{ .label = &&fmove_1_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "fmove_1_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_last_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_last_7",
#endif
	  .arg_size = 3 },

	{ .label = &&l_is_ne_exact_immed_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_9",
#endif
	  .arg_size = 2 },

	{ .label = &&l_fast_element_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fast_element_5",
#endif
	  .arg_size = 3 },

	{ .label = &&is_nonempty_list_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_13",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_literal_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_literal_3",
#endif
	  .arg_size = 2 },

	{ .label = &&test_heap_1_put_list_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_heap_1_put_list_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_ext_84,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_84",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_75,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_75",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_62,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_62",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_58,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_58",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_48,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_48",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_35,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_35",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_10",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element2_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_9",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_20,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_20",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_put_string_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_put_string_1",
#endif
	  .arg_size = 1 },

	{ .label = &&is_list_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_list_5",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_22,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_22",
#endif
	  .arg_size = 1 },

	{ .label = &&get_list_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_list_10",
#endif
	  .arg_size = 3 },

	{ .label = &&l_move_call_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_9",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_24,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_24",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_16",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_29,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_29",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_77,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_77",
#endif
	  .arg_size = 0 },

	{ .label = &&l_select_val_atoms_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val_atoms_2",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nonempty_list_21,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_21",
#endif
	  .arg_size = 1 },

	{ .label = &&is_binary_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_binary_3",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_ext_only_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_only_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_17",
#endif
	  .arg_size = 0 },

	{ .label = &&init_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_11",
#endif
	  .arg_size = 0 },

	{ .label = &&l_get_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_get_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_test_zero_tail2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_zero_tail2_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_get_integer_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_0",
#endif
	  .arg_size = 2 },

	{ .label = &&func_info_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "func_info_0",
#endif
	  .arg_size = 3 },

	{ .label = &&extract_next_element_22,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_22",
#endif
	  .arg_size = 0 },

	{ .label = &&l_select_val_atoms_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val_atoms_3",
#endif
	  .arg_size = 3 },

	{ .label = &&l_trim_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_10",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_21,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_21",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_last_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_last_5",
#endif
	  .arg_size = 2 },

	{ .label = &&l_fsub_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fsub_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_35,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_35",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_31,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_31",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_12",
#endif
	  .arg_size = 1 },

	{ .label = &&init_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_15",
#endif
	  .arg_size = 1 },

	{ .label = &&l_wait_timeout_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_wait_timeout_5",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_last_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_last_9",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_31,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_31",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_104,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_104",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_65,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_65",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_59,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_59",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_5",
#endif
	  .arg_size = 0 },

	{ .label = &&l_new_bs_put_integer_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_integer_1",
#endif
	  .arg_size = 3 },

	{ .label = &&move_return_34,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_34",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_12",
#endif
	  .arg_size = 0 },

	{ .label = &&l_trim_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_11",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_only_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_only_9",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_last_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_last_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_last_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_last_1",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nonempty_list_29,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_29",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_23,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_23",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_17",
#endif
	  .arg_size = 1 },

	{ .label = &&l_call_fun_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_fun_2",
#endif
	  .arg_size = 0 },

	{ .label = &&l_apply_only_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_apply_only_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_fetch_21,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_21",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_47,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_47",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_19,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_19",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_test_zero_tail2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_zero_tail2_0",
#endif
	  .arg_size = 1 },

	{ .label = &&call_bif_45,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_45",
#endif
	  .arg_size = 0 },

	{ .label = &&bs_init_writable_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bs_init_writable_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_39,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_39",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element_20,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_20",
#endif
	  .arg_size = 0 },

	{ .label = &&move_jump_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_11",
#endif
	  .arg_size = 1 },

	{ .label = &&move_jump_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_4",
#endif
	  .arg_size = 1 },

	{ .label = &&move_return_47,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_47",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_46,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_46",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_39,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_39",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_33,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_33",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_20,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_20",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nil_27,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_27",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_22,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_22",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_put_string_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_put_string_5",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_put_string_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_put_string_3",
#endif
	  .arg_size = 1 },

	{ .label = &&put_list_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_13",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nonempty_list_18,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_18",
#endif
	  .arg_size = 1 },

	{ .label = &&l_increment_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_increment_7",
#endif
	  .arg_size = 4 },

	{ .label = &&l_times_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_times_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_get_integer_imm_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_imm_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_move_call_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_11",
#endif
	  .arg_size = 1 },

	{ .label = &&l_get_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_get_5",
#endif
	  .arg_size = 1 },

	{ .label = &&call_bif_24,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_24",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_immed_34,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_34",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_last_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_last_6",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_9",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_ne_exact_immed_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_ne_exact_immed_7",
#endif
	  .arg_size = 2 },

	{ .label = &&move_jump_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_5",
#endif
	  .arg_size = 1 },

	{ .label = &&move_return_40,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_40",
#endif
	  .arg_size = 0 },

	{ .label = &&l_trim_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_trim_9",
#endif
	  .arg_size = 0 },

	{ .label = &&bs_context_to_binary_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bs_context_to_binary_2",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nonempty_list_30,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_30",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_27,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_27",
#endif
	  .arg_size = 1 },

	{ .label = &&l_make_export_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_make_export_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_select_val2_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_11",
#endif
	  .arg_size = 5 },

	{ .label = &&is_number_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_number_0",
#endif
	  .arg_size = 2 },

	{ .label = &&move_deallocate_return_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_deallocate_return_10",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_28,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_28",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_14",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_only_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_only_2",
#endif
	  .arg_size = 1 },

	{ .label = &&init_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_14",
#endif
	  .arg_size = 0 },

	{ .label = &&init_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_13",
#endif
	  .arg_size = 0 },

	{ .label = &&init_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init_12",
#endif
	  .arg_size = 0 },

	{ .label = &&l_wait_timeout_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_wait_timeout_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_wait_timeout_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_wait_timeout_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_wait_timeout_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_wait_timeout_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_skip_bits_all2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_skip_bits_all2_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_test_zero_tail2_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_zero_tail2_6",
#endif
	  .arg_size = 2 },

	{ .label = &&call_bif_39,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_39",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_19,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_19",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_4",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_2",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_1",
#endif
	  .arg_size = 0 },

	{ .label = &&call_bif_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "call_bif_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_int_div_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_int_div_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_put_utf16_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_put_utf16_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_get_utf16_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_utf16_2",
#endif
	  .arg_size = 4 },

	{ .label = &&l_bs_get_utf16_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_utf16_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_utf16_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_utf16_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_allocate_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_allocate_9",
#endif
	  .arg_size = 0 },

	{ .label = &&l_put_tuple_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_put_tuple_7",
#endif
	  .arg_size = 2 },

	{ .label = &&l_put_tuple_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_put_tuple_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_put_tuple_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_put_tuple_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_put_tuple_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_put_tuple_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_put_tuple_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_put_tuple_2",
#endif
	  .arg_size = 0 },

	{ .label = &&l_put_tuple_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_put_tuple_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_is_eq_exact_immed_33,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_33",
#endif
	  .arg_size = 2 },

	{ .label = &&l_is_eq_exact_immed_32,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_32",
#endif
	  .arg_size = 2 },

	{ .label = &&l_is_eq_exact_immed_23,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_23",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_17",
#endif
	  .arg_size = 2 },

	{ .label = &&l_is_eq_exact_immed_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_10",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_immed_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_immed_4",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_108,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_108",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_107,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_107",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_106,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_106",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_103,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_103",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_102,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_102",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_100,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_100",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_99,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_99",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_97,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_97",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_95,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_95",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_94,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_94",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_93,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_93",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_90,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_90",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_89,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_89",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_87,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_87",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_85,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_85",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_83,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_83",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_81,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_81",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_79,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_79",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_76,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_76",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_73,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_73",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_72,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_72",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_70,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_70",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_69,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_69",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_68,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_68",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_67,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_67",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_63,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_63",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_60,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_60",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_57,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_57",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_56,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_56",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_54,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_54",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_53,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_53",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_52,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_52",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_50,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_50",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_49,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_49",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_46,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_46",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_33,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_33",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_32,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_32",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_30,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_30",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_29,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_29",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_27,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_27",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_22,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_22",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_21,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_21",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_19,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_19",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_18,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_18",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_15",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_13",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_1",
#endif
	  .arg_size = 0 },

	{ .label = &&allocate_init_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "allocate_init_1",
#endif
	  .arg_size = 2 },

	{ .label = &&extract_next_element_23,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_23",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element_18,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element_18",
#endif
	  .arg_size = 0 },

	{ .label = &&get_tuple_element_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_11",
#endif
	  .arg_size = 3 },

	{ .label = &&get_tuple_element_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_10",
#endif
	  .arg_size = 1 },

	{ .label = &&get_tuple_element_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_9",
#endif
	  .arg_size = 1 },

	{ .label = &&get_tuple_element_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_8",
#endif
	  .arg_size = 1 },

	{ .label = &&get_tuple_element_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_7",
#endif
	  .arg_size = 1 },

	{ .label = &&get_tuple_element_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_tuple_element_6",
#endif
	  .arg_size = 0 },

	{ .label = &&set_tuple_element_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "set_tuple_element_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_call_fun_last_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_fun_last_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_fast_element_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fast_element_4",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_test_unit_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_unit_0",
#endif
	  .arg_size = 3 },

	{ .label = &&extract_next_element3_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element3_9",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element3_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element3_8",
#endif
	  .arg_size = 0 },

	{ .label = &&extract_next_element2_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "extract_next_element2_15",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_start_match2_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_start_match2_4",
#endif
	  .arg_size = 5 },

	{ .label = &&l_bs_start_match2_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_start_match2_3",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_start_match2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_start_match2_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_start_match2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_start_match2_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_integer_32_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_32_3",
#endif
	  .arg_size = 4 },

	{ .label = &&l_bs_get_integer_32_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_32_2",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_integer_32_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_32_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bor_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bor_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bsr_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bsr_1",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_get_binary_imm2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_binary_imm2_2",
#endif
	  .arg_size = 5 },

	{ .label = &&l_bs_get_binary_imm2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_binary_imm2_1",
#endif
	  .arg_size = 4 },

	{ .label = &&l_bs_test_tail_imm2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_tail_imm2_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bxor_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bxor_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_get_float2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_float2_0",
#endif
	  .arg_size = 5 },

	{ .label = &&move_jump_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_9",
#endif
	  .arg_size = 1 },

	{ .label = &&move_jump_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_jump_8",
#endif
	  .arg_size = 1 },

	{ .label = &&allocate_heap_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "allocate_heap_1",
#endif
	  .arg_size = 3 },

	{ .label = &&move_return_44,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_44",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_43,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_43",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_42,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_42",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_41,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_41",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_38,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_38",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_37,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_37",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_35,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_35",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_32,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_32",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_31,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_31",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_30,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_30",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_27,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_27",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_25,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_25",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_24,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_24",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_23,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_23",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_18,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_18",
#endif
	  .arg_size = 0 },

	{ .label = &&move_return_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_return_15",
#endif
	  .arg_size = 0 },

	{ .label = &&l_new_bs_put_integer_imm_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_integer_imm_2",
#endif
	  .arg_size = 4 },

	{ .label = &&l_new_bs_put_integer_imm_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_integer_imm_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_integer_small_imm_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_small_imm_1",
#endif
	  .arg_size = 5 },

	{ .label = &&l_rem_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_rem_1",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nil_26,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_26",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_25,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_25",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_24,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_24",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_23,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_23",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_19,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_19",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_18,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_18",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nil_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nil_16",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bsl_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bsl_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_fmul_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fmul_0",
#endif
	  .arg_size = 1 },

	{ .label = &&is_tuple_of_arity_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_of_arity_4",
#endif
	  .arg_size = 3 },

	{ .label = &&is_tuple_of_arity_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_tuple_of_arity_2",
#endif
	  .arg_size = 2 },

	{ .label = &&test_arity_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_arity_4",
#endif
	  .arg_size = 3 },

	{ .label = &&test_arity_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_arity_2",
#endif
	  .arg_size = 2 },

	{ .label = &&bs_context_to_binary_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bs_context_to_binary_8",
#endif
	  .arg_size = 0 },

	{ .label = &&bs_context_to_binary_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bs_context_to_binary_7",
#endif
	  .arg_size = 0 },

	{ .label = &&bs_context_to_binary_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bs_context_to_binary_6",
#endif
	  .arg_size = 0 },

	{ .label = &&bs_context_to_binary_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bs_context_to_binary_5",
#endif
	  .arg_size = 0 },

	{ .label = &&bs_context_to_binary_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bs_context_to_binary_4",
#endif
	  .arg_size = 0 },

	{ .label = &&bs_context_to_binary_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bs_context_to_binary_3",
#endif
	  .arg_size = 0 },

	{ .label = &&bs_context_to_binary_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bs_context_to_binary_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_new_bs_put_binary_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_binary_0",
#endif
	  .arg_size = 4 },

	{ .label = &&badmatch_17,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_17",
#endif
	  .arg_size = 1 },

	{ .label = &&badmatch_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_16",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_15,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_15",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_14,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_14",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_13,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_13",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_12",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_11",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_10",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_9",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_8",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_7",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_6",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_5",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_4",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_3",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_2",
#endif
	  .arg_size = 0 },

	{ .label = &&badmatch_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "badmatch_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_test_unit_8_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_unit_8_4",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_utf8_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_utf8_1",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_get_utf8_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_utf8_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_put_utf8_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_put_utf8_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_match_string_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_match_string_2",
#endif
	  .arg_size = 4 },

	{ .label = &&l_bs_match_string_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_match_string_1",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_put_string_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_put_string_4",
#endif
	  .arg_size = 1 },

	{ .label = &&fconv_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "fconv_1",
#endif
	  .arg_size = 1 },

	{ .label = &&l_m_div_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_m_div_0",
#endif
	  .arg_size = 3 },

	{ .label = &&raise_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "raise_1",
#endif
	  .arg_size = 2 },

	{ .label = &&raise_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "raise_0",
#endif
	  .arg_size = 0 },

	{ .label = &&is_integer_allocate_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_integer_allocate_1",
#endif
	  .arg_size = 3 },

	{ .label = &&is_nonempty_list_allocate_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_allocate_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_fnegate_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fnegate_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_validate_unicode_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_validate_unicode_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_hibernate_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_hibernate_0",
#endif
	  .arg_size = 0 },

	{ .label = &&put_list_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "put_list_8",
#endif
	  .arg_size = 0 },

	{ .label = &&is_nonempty_list_41,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_41",
#endif
	  .arg_size = 2 },

	{ .label = &&is_nonempty_list_40,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_40",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_39,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_39",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_38,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_38",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_37,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_37",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_36,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_36",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_34,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_34",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_32,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_32",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_31,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_31",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_28,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_28",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_26,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_26",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_25,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_25",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_20,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_20",
#endif
	  .arg_size = 1 },

	{ .label = &&is_nonempty_list_16,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_16",
#endif
	  .arg_size = 1 },

	{ .label = &&get_list_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_list_9",
#endif
	  .arg_size = 1 },

	{ .label = &&get_list_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_list_7",
#endif
	  .arg_size = 1 },

	{ .label = &&get_list_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "get_list_1",
#endif
	  .arg_size = 1 },

	{ .label = &&case_end_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_11",
#endif
	  .arg_size = 1 },

	{ .label = &&case_end_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_10",
#endif
	  .arg_size = 0 },

	{ .label = &&case_end_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_9",
#endif
	  .arg_size = 0 },

	{ .label = &&case_end_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_8",
#endif
	  .arg_size = 0 },

	{ .label = &&case_end_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_7",
#endif
	  .arg_size = 0 },

	{ .label = &&case_end_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_6",
#endif
	  .arg_size = 0 },

	{ .label = &&case_end_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_5",
#endif
	  .arg_size = 0 },

	{ .label = &&case_end_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_4",
#endif
	  .arg_size = 0 },

	{ .label = &&case_end_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_3",
#endif
	  .arg_size = 0 },

	{ .label = &&case_end_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_2",
#endif
	  .arg_size = 0 },

	{ .label = &&case_end_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_1",
#endif
	  .arg_size = 0 },

	{ .label = &&case_end_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "case_end_0",
#endif
	  .arg_size = 0 },

	{ .label = &&try_case_end_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "try_case_end_1",
#endif
	  .arg_size = 1 },

	{ .label = &&try_case_end_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "try_case_end_0",
#endif
	  .arg_size = 0 },

	{ .label = &&apply_last_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "apply_last_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_call_ext_last_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_last_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_append_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_append_1",
#endif
	  .arg_size = 4 },

	{ .label = &&l_increment_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_increment_4",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_private_append_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_private_append_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_init_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_init_0",
#endif
	  .arg_size = 4 },

	{ .label = &&l_new_bs_put_float_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_float_0",
#endif
	  .arg_size = 4 },

	{ .label = &&l_bs_validate_unicode_retract_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_validate_unicode_retract_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_yield_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_yield_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_apply_fun_last_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_apply_fun_last_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_minus_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_minus_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_minus_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_minus_1",
#endif
	  .arg_size = 2 },

	{ .label = &&init3_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init3_1",
#endif
	  .arg_size = 3 },

	{ .label = &&l_select_val_smallints_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val_smallints_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bif2_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bif2_5",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bif2_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bif2_3",
#endif
	  .arg_size = 2 },

	{ .label = &&l_select_val2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_val2_1",
#endif
	  .arg_size = 4 },

	{ .label = &&l_select_tuple_arity2_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_tuple_arity2_3",
#endif
	  .arg_size = 6 },

	{ .label = &&l_select_tuple_arity2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_tuple_arity2_2",
#endif
	  .arg_size = 4 },

	{ .label = &&init2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "init2_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_skip_bits_imm2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_skip_bits_imm2_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_restore2_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_restore2_3",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_restore2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_restore2_2",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_restore2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_restore2_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_skip_bits2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_skip_bits2_1",
#endif
	  .arg_size = 4 },

	{ .label = &&l_bs_get_binary_all2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_binary_all2_2",
#endif
	  .arg_size = 4 },

	{ .label = &&l_bs_get_binary_all2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_binary_all2_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_save2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_save2_2",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_save2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_save2_1",
#endif
	  .arg_size = 0 },

	{ .label = &&is_function2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_function2_0",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bif1_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bif1_2",
#endif
	  .arg_size = 4 },

	{ .label = &&l_bif1_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bif1_1",
#endif
	  .arg_size = 3 },

	{ .label = &&is_nonempty_list_test_heap_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "is_nonempty_list_test_heap_1",
#endif
	  .arg_size = 3 },

	{ .label = &&allocate_heap_zero_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "allocate_heap_zero_1",
#endif
	  .arg_size = 3 },

	{ .label = &&deallocate_return_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "deallocate_return_12",
#endif
	  .arg_size = 0 },

	{ .label = &&move_deallocate_return_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_deallocate_return_9",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_init_heap_bin_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_init_heap_bin_1",
#endif
	  .arg_size = 4 },

	{ .label = &&l_call_fun_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_fun_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_new_bs_put_binary_imm_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_binary_imm_3",
#endif
	  .arg_size = 4 },

	{ .label = &&l_new_bs_put_binary_imm_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_binary_imm_2",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_get_integer_imm_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_imm_1",
#endif
	  .arg_size = 5 },

	{ .label = &&l_new_bs_put_float_imm_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_float_imm_1",
#endif
	  .arg_size = 4 },

	{ .label = &&l_new_bs_put_float_imm_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_new_bs_put_float_imm_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_35,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_35",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_34,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_34",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_33,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_33",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_32,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_32",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_31,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_31",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_30,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_30",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_29,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_29",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_27,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_27",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_26,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_26",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_24,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_24",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_23,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_23",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_22,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_22",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_21,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_21",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_20,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_20",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_19,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_19",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_18,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_18",
#endif
	  .arg_size = 1 },

	{ .label = &&l_is_eq_exact_literal_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_is_eq_exact_literal_2",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_init_bits_fail_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_init_bits_fail_1",
#endif
	  .arg_size = 4 },

	{ .label = &&l_jump_on_val_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_jump_on_val_2",
#endif
	  .arg_size = 4 },

	{ .label = &&l_bs_init_fail_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_init_fail_1",
#endif
	  .arg_size = 4 },

	{ .label = &&l_call_ext_only_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_only_3",
#endif
	  .arg_size = 0 },

	{ .label = &&l_call_ext_only_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_call_ext_only_1",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_only_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_only_9",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_only_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_only_8",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_only_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_only_5",
#endif
	  .arg_size = 0 },

	{ .label = &&bif1_body_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "bif1_body_2",
#endif
	  .arg_size = 0 },

	{ .label = &&l_select_tuple_arity_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_select_tuple_arity_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_fetch_22,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_22",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fetch_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_11",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fetch_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_9",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fetch_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_5",
#endif
	  .arg_size = 1 },

	{ .label = &&l_fetch_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_fetch_3",
#endif
	  .arg_size = 1 },

	{ .label = &&l_catch_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_catch_7",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_get_integer_8_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_8_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_get_integer_16_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_16_2",
#endif
	  .arg_size = 3 },

	{ .label = &&move_12,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move_12",
#endif
	  .arg_size = 0 },

	{ .label = &&l_bs_utf8_size_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_utf8_size_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_bs_utf16_size_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_utf16_size_0",
#endif
	  .arg_size = 2 },

	{ .label = &&l_move_call_ext_49,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_49",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_48,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_48",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_45,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_45",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_43,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_43",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_42,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_42",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_41,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_41",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_40,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_40",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_39,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_39",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_34,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_34",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_33,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_33",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_32,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_32",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_30,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_30",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_29,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_29",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_27,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_27",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_23,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_23",
#endif
	  .arg_size = 0 },

	{ .label = &&l_move_call_ext_20,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_20",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_18,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_18",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_11,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_11",
#endif
	  .arg_size = 1 },

	{ .label = &&l_move_call_ext_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_move_call_ext_0",
#endif
	  .arg_size = 1 },

	{ .label = &&catch_end_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "catch_end_7",
#endif
	  .arg_size = 0 },

	{ .label = &&test_heap_1_put_list_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_heap_1_put_list_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_add_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_add_1",
#endif
	  .arg_size = 3 },

	{ .label = &&l_band_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_band_1",
#endif
	  .arg_size = 2 },

	{ .label = &&on_load_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "on_load_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_get_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_get_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_get_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_get_0",
#endif
	  .arg_size = 0 },

	{ .label = &&if_end_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "if_end_0",
#endif
	  .arg_size = 0 },

	{ .label = &&l_wait_timeout_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_wait_timeout_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_wait_timeout_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_wait_timeout_0",
#endif
	  .arg_size = 1 },

	{ .label = &&system_limit_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "system_limit_0",
#endif
	  .arg_size = 1 },

	{ .label = &&l_plus_3,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_plus_3",
#endif
	  .arg_size = 3 },

	{ .label = &&l_plus_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_plus_1",
#endif
	  .arg_size = 2 },

	{ .label = &&l_gc_bif3_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_gc_bif3_0",
#endif
	  .arg_size = 5 },

	{ .label = &&move2_10,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move2_10",
#endif
	  .arg_size = 4 },

	{ .label = &&move2_9,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move2_9",
#endif
	  .arg_size = 1 },

	{ .label = &&move2_8,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move2_8",
#endif
	  .arg_size = 1 },

	{ .label = &&move2_7,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move2_7",
#endif
	  .arg_size = 1 },

	{ .label = &&move2_6,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move2_6",
#endif
	  .arg_size = 1 },

	{ .label = &&move2_5,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move2_5",
#endif
	  .arg_size = 1 },

	{ .label = &&move2_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move2_4",
#endif
	  .arg_size = 1 },

	{ .label = &&move2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "move2_2",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_skip_bits_all2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_skip_bits_all2_2",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_test_zero_tail2_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_test_zero_tail2_4",
#endif
	  .arg_size = 1 },

	{ .label = &&l_bs_get_integer_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_integer_1",
#endif
	  .arg_size = 3 },

	{ .label = &&l_bs_get_binary2_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_bs_get_binary2_1",
#endif
	  .arg_size = 5 },

	{ .label = &&fmove_2_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "fmove_2_2",
#endif
	  .arg_size = 2 },

	{ .label = &&l_gc_bif2_0,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_gc_bif2_0",
#endif
	  .arg_size = 4 },

	{ .label = &&l_gc_bif1_4,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_gc_bif1_4",
#endif
	  .arg_size = 5 },

	{ .label = &&l_gc_bif1_2,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_gc_bif1_2",
#endif
	  .arg_size = 4 },

	{ .label = &&l_gc_bif1_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "l_gc_bif1_1",
#endif
	  .arg_size = 3 },

	{ .label = &&test_heap_1,
#if defined(LING_DEBUG) || defined(EXP_COUNT_IOPS) || defined(EXP_RUNTIME_METRICS)
	  .var_name = "test_heap_1",
#endif
	  .arg_size = 2 },


	};
	ling_opcodes = opcodes;	// export to the file scope

	// Let the label &&func_info_0 known on the file scope to let stack tracing
	// work; &&int_code_end is also needed for correct stack tracing.
	//
	func_info_label = &&func_info_0;
	int_code_end_label = &&int_code_end_0;

	// defines the code of preloaded modules
	#include "premod.inc"

	assert(proc == 0); // completing initialization run
}
}

uint32_t *backstep_to_func_info(uint32_t *p)
{
	//TODO: request the code boundaries from code_base_t and
	// watch for runaway pointers
	while (1) {
		while (*p != shrink_ptr(func_info_label))
			p--;
		if (is_atom(p[1]) && is_atom(p[2]) && p[3] < 256)
			return p;
	}
	/* NOTREACHED */
}

opcode_info_t *opcode_lookup(void *label)
{
	for (int i = 0; i < 1181; i++)
		if (ling_opcodes[i].label == label)
			return ling_opcodes + i;
	return 0;
}

opcode_info_t *opcode_get(uint32_t n)
{
	if (n > 1181)
		return 0;
	return ling_opcodes + n;
}

#ifdef EXP_COUNT_IOPS
void print_iop_counters(void)
{
	printk("\n================ iop counters ====================\n");
	for (int i = 0; i < 1181; i++)
	{
		opcode_info_t *oi = &ling_opcodes[i];
		printk("%12ld %s\n", oi->counter, oi->var_name);
	}
}
#endif // EXP_COUNT_IOPS

#ifdef EXP_RUNTIME_METRICS
void print_variant_code_sizes(void)
{
	printk("\n================ variant code sizes ==============\n");
	int num_vars = 1181;
	opcode_info_t *ptr = ling_opcodes;
	while (ptr < ling_opcodes +num_vars)
	{
		printk("%10ld %s\n", (uintptr_t)ptr->label, ptr->var_name);
		ptr++;
	}
}
#endif // EXP_RUNTIME_METRICS

static term_t invoke_bif(export_t *exp, proc_t *proc, term_t *rs, int live)
{
	assert(exp->is_bif);

	switch(exp->is_bif)
	{
	case BIF_TYPE_CALL:
		return ((cbif_func_t)exp->entry)(proc, rs);
	case BIF_TYPE_NORMAL_0:
		return ((bif_func0_t)exp->entry)(proc);
	case BIF_TYPE_NORMAL_1:
		return ((bif_func1_t)exp->entry)(rs[0], proc);
	case BIF_TYPE_NORMAL_2:
		return ((bif_func2_t)exp->entry)(rs[0], rs[1], proc);
	case BIF_TYPE_GC_1:
		return ((gc_bif_func1_t)exp->entry)(rs[0], proc, rs, live);
	case BIF_TYPE_GC_2:
		return ((gc_bif_func2_t)exp->entry)(rs[0], rs[1], proc, rs, live);
	default:
	{
		assert(exp->is_bif == BIF_TYPE_GC_3);
		return ((gc_bif_func3_t)exp->entry)(rs[0], rs[1], rs[2], proc, rs, live);
	}
	}
}

static term_t build_location(uint32_t *ip, heap_t *hp)
{
	// [{file,File},{line,Line}]
	char fn[256 +4];
	int line = code_base_source_line(ip, fn, sizeof(fn));
	if (line == 0)
		return nil;

	term_t file_name = heap_strz_N(hp, fn);
	if (file_name == noval)
		return noval;

	uint32_t *htop = heap_alloc_N(hp, 1 +2);
	if (htop == 0)
		return noval;
	heap_set_top(hp, htop +1 +2);
	htop[0] = 2;
	htop[1] = A_FILE;
	htop[2] = file_name;
	term_t tuple1 = tag_tuple(htop);

	htop = heap_alloc_N(hp, 1 +2);
	if (htop == 0)
		return noval;
	heap_set_top(hp, htop +1 +2);
	htop[0] = 2;
	htop[1] = A_LINE;
	assert(fits_int(line));
	htop[2] = tag_int(line);
	term_t tuple2 = tag_tuple(htop);

	htop = heap_alloc_N(hp, 4);
	if (htop == 0)
		return noval;
	heap_set_top(hp, htop +4);
	htop[0] = tuple1;
	htop[1] = tag_cons(htop +2);
	htop[2] = tuple2;
	htop[3] = nil;

	return tag_cons(htop);
}

static term_t get_stack_trace(uint32_t *ip, uint32_t *cp,
			term_t *sp, term_t *sbot, mfa_t *first_mfa, term_t args0, heap_t *hp)
{
	term_t trace[max_backtrace_depth];
	int ntrace = 0;

	if (first_mfa != 0)
	{
		assert(args0 == noval || list_len(args0) == first_mfa->arity);
		term_t args = (args0 == noval)
			? tag_int(first_mfa->arity)
			: args0;
		args0 = noval;
		uint32_t *htop = heap_alloc_N(hp, 1 +4);
		if (htop == 0)
			return nil;
		heap_set_top(hp, htop +1 +4);
		htop[0] = 4;
		htop[1] = first_mfa->mod;
		htop[2] = first_mfa->fun;
		htop[3] = args;
		htop[4] = nil;

		assert(ntrace < max_backtrace_depth);
		trace[ntrace++] = tag_tuple(htop);
	}

	uint32_t *fi = backstep_to_func_info(ip);
	if (fi == 0)
		return nil;
	int arity = fi[3];


	// length(args0) may be less than the arity as the current function may be a
	// fun body with free variables passed as arguments
	//
	assert(args0 == noval || list_len(args0) <= arity);
	term_t args = (args0 == noval)
		? tag_int(arity)
		: args0;

	term_t loc = build_location(ip, hp);
	if (loc == noval)
		return nil;

	uint32_t *htop = heap_alloc_N(hp, 1 +4);
	if (htop == 0)
		return nil;
	heap_set_top(hp, htop +1 +4);
	htop[0] = 4;
	htop[1] = fi[1];
	htop[2] = fi[2];
	htop[3] = args;
	htop[4] = loc;

	assert(ntrace < max_backtrace_depth);
	trace[ntrace++] = tag_tuple(htop);

	if (cp == 0)	// after allocate, before call/apply
	{
		cp = demasquerade_pointer(sp[0]);
		while (++sp < sbot)
			if (is_boxed(*sp) && is_cp(peel_boxed(*sp)))
				break;
	}

	do {
		if (cp[0] == shrink_ptr(int_code_end_label))
			break;
		uint32_t *fi = backstep_to_func_info(cp);

		term_t loc = build_location(cp -1, hp);		// NB: -1
		if (loc == noval)
			return nil;

		uint32_t *htop = heap_alloc_N(hp, 1 +4);
		if (htop == 0)
			return nil;
		heap_set_top(hp, htop +1 +4);
		htop[0] = 4;
		htop[1] = fi[1];
		htop[2] = fi[2];
		htop[3] = tag_int(fi[3]);
		htop[4] = loc;

		assert(ntrace < max_backtrace_depth);
		trace[ntrace++] = tag_tuple(htop);
		if (ntrace >= max_backtrace_depth)
			break;

		cp = demasquerade_pointer(sp[0]);
		while (++sp < sbot)
			if (is_boxed(*sp) && is_cp(peel_boxed(*sp)))
				break;
	} while (sp < sbot);

	term_t tail = nil;
	uint32_t *cons = heap_alloc_N(hp, ntrace *2);
	if (cons == 0)
		return nil;
	for (int i = ntrace-1; i >= 0; i--)
	{
		cons[0] = trace[i];
		cons[1] = tail;
		tail = tag_cons(cons);
		cons += 2;
	}
	heap_set_top(hp, cons);

	return tail;
}

static int select_val_atoms_compare(const void *a, const void *b)
{
	return *(term_t *)a - *(term_t *)b;
}

static int send_to_outlet(outlet_t *ol, term_t what, proc_t *proc)
{
	// {Pid,{command,Data}}
	// {Pid,{command$,Data}}
	// {Pid,close}
	// {Pid,close$}
	// {Pid,{connect,NewPid}}
	// {Pid,{connect$,NewPid}}
	
	if (!is_tuple(what))
		return -BAD_ARG;

	uint32_t *p = peel_tuple(what);
	if (p[0] != 2 || !is_short_pid(p[1]))
		return -BAD_ARG;

	proc_t *cont_proc = scheduler_lookup(ol->owner);

	if (p[2] == A_CLOSE || p[2] == ACLOSE__)
	{
		if (p[2] == A_CLOSE && p[1] != ol->owner)
			goto badsig;

		if ((p[2] == A_CLOSE && p[1] == ol->owner) || p[2] == ACLOSE__)
		{
			ol->notify_on_close = (p[2] == A_CLOSE);
			proc->result.what = SLICE_RESULT_OUTLET_CLOSE;
			proc->result.closing  = ol;
			proc->result.why = A_NORMAL;
			return DELIVER_SIGNALS;
		}
	}
	else
	{
		if (!is_tuple(p[2]))
			return -BAD_ARG;

		uint32_t *q = peel_tuple(p[2]);
		if (q[0] != 2)
			return -BAD_ARG;

		if (q[1] == A_COMMAND || q[1] == ACOMMAND__)
		{
			// {Pid,{command,Data}}
			// {Pid,{command$,Data}}

			if (q[1] == A_COMMAND && p[1] != ol->owner)
				goto badsig;

			int sz = iolist_size(q[2]);
			if (sz < 0)
			{
				if (q[1] == ACOMMAND__)
					return 0;	// ignore
				else
					goto badsig;
			}

			int x = 0;
			uint8_t *data = outlet_get_send_buffer(ol, sz);
			if (data == 0)
				x = -TOO_LONG;
			if (x == 0)
			{
				//
				// {Pid,{command,Data}} - reply to owner
				// {Pid,{command$,Data}} - reply to pid
				//
				iolist_flatten(q[2], data);
				term_t reply_to = p[1];
				x = outlet_send(ol, sz, reply_to);
			}
			if (x < 0)
			{
				proc->result.what = SLICE_RESULT_OUTLET_CLOSE;
				proc->result.closing = ol;
				proc->result.why = err_to_term(x);
				return DELIVER_SIGNALS;
			}
		}
		else if (q[1] == A_CONNECT || q[1] == ACONNECT__)
		{
			// {Pid,{connect,NewPid}}
			// {Pid,{connect$,NewPid}}

			term_t new_owner = q[2];
			if (q[1] == A_CONNECT)
			{
				if (p[1] != ol->owner)
					goto badsig;

				if (outlet_notify_owner(ol, A_CONNECTED) < 0)
					printk("send_to_outlet: connected msg not delivered to %pt\n", T(ol->owner));
			}
			else	// port_connect()
			{
				// Establish a link to the new owner
				proc_t *owner_proc = scheduler_lookup(new_owner);
				if (owner_proc == 0)
					return -BAD_ARG;
				if (!are_inter_linked(&ol->links, new_owner))
				{
					int x = inter_link_establish_N(&ol->links, new_owner);
					if (x == 0)
						x = inter_link_establish_N(&owner_proc->links, ol->oid);
					if (x < 0)
					{
						ol->notify_on_close = 0;
						proc->result.what = SLICE_RESULT_OUTLET_CLOSE;
						proc->result.closing  = ol;
						proc->result.why = A_NO_MEMORY;
						return DELIVER_SIGNALS;
					}
				}
			}

			ol->owner = new_owner;
		}
		else
			return -BAD_ARG;
	}

	return 0;

badsig:
	if (cont_proc == 0)
		return 0;
	proc->result.what = SLICE_RESULT_EXIT2;
	proc->result.victim = cont_proc;
	proc->result.reason2 = A_BADSIG;
	return DELIVER_SIGNALS;
}

//EOF


