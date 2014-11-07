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

#include "ling_common.h"

#include "atom_defs.h"
#include "atoms.h"
#include "string.h"
#include "code_base.h"
#include "scheduler.h"
#include "list_util.h"
#include "catch_tab.h"

extern void *func_info_label;

// A common denominator between spawn() and spawn_link()
int proc_spawn_N(proc_t *new_proc, term_t m, term_t f, term_t args)
{
	int arity = list_len(args);
	assert(arity >= 0);

	term_t new_regs[arity];
	heap_list_to_vector(args, new_regs);
	int x = heap_copy_terms_N(&new_proc->hp, new_regs, arity);
	if (x < 0)
		return x;

	// save the initial call
	new_proc->init_call_mod = m;
	new_proc->init_call_func = f;
	new_proc->init_call_arity = arity;

	export_t *exp = code_base_lookup(m, f, arity);
	if (exp == 0 || exp->entry == 0)
	{
		exp = EH_UNDEF_EXP;
		new_proc->cap.regs[0] = m;
		new_proc->cap.regs[1] = f;
		term_t new_args = heap_vector_to_list_N(&new_proc->hp, new_regs, arity);
		if (args == noval)
			return -NO_MEMORY;

		new_proc->cap.regs[2] = new_args;
		new_proc->cap.live = 3;
	}
	else
	{
		memcpy(new_proc->cap.regs, new_regs, arity*sizeof(term_t));
		new_proc->cap.live = arity;
	}

	new_proc->cap.ip = exp->entry;
	module_info_t *module = code_base_module_by_name(exp->module, 0);
	assert(module != 0);
	new_proc->cap.cp = module->code_starts + module->code_size-1;

	return scheduler_enlist_N(new_proc);
}

int proc_spawn_fun0_N(proc_t *new_proc, t_fun_t *f)
{
	// the fun is applied to []
	int num_free = fun_num_free((uint32_t *)f);
	memcpy(new_proc->cap.regs, f->frozen, num_free*sizeof(term_t));
	int x = heap_copy_terms_N(&new_proc->hp, new_proc->cap.regs, num_free);
	if (x < 0)
		return x;
	new_proc->cap.live = num_free;

	assert(f->fe->entry != 0);
	//
	// The name of the function implementing the fun is not easily accessible.
	// We assume that the entry points to the instruction right after func_info
	// and we fetch the name from parameters of func_info.
	//
	// 			[-4] &&func_info_0
	// 			[-3] module
	// 			[-2] function
	// 			[-1] arity
	// entry ->
	//
	uint32_t *entry = f->fe->entry;
	if (expand_ptr(entry[-4]) == func_info_label)
	{
		new_proc->init_call_mod = entry[-3];
		new_proc->init_call_func = entry[-2];
		new_proc->init_call_arity = 0;
	}

	new_proc->cap.ip = f->fe->entry;
	module_info_t *module = code_base_module_by_name(f->module, 0);
	assert(module != 0);
	new_proc->cap.cp = module->code_starts + module->code_size-1;

	return scheduler_enlist_N(new_proc);
}

term_t proc_set_flag(proc_t *proc, term_t flag, term_t val)
{
	if (flag == A_TRAP_EXIT)
	{
		if (val != A_TRUE && val != A_FALSE)
			return noval;
		term_t old = proc->trap_exit;
		proc->trap_exit = val;
		return old;
	}
	else if (flag == A_FULLSWEEP_AFTER)
	{
		// non-standard
		if (!is_int(val))
			return noval;
		int n = int_value(val);
		if (n < 0)
			return noval;
		int saved = proc->hp.full_sweep_after;
		proc->hp.full_sweep_after = n;
		return tag_int(saved);
	}
	else if (flag == A_SUPPRESS_GC)
	{
		// non-standard
		if (!is_bool(val))
			return noval;
		int saved = proc->hp.suppress_gc;
		proc->hp.suppress_gc = (val == A_TRUE);
		return (saved) ?A_TRUE :A_FALSE;
	} else if (flag == A_MONITOR_NODES)
	{
		//TODO: undocumented
		return A_FALSE;
	}

	//printk("process_flag(%pt, %pt) ignored\n", T(flag), T(val));
	return A_UNDEFINED;
}

int is_module_code(module_info_t *mi, uint32_t *ptr)
{
	return (ptr >= mi->code_starts && ptr < mi->code_starts +mi->code_size);
}

int has_module_fun(module_info_t *mi, term_t t)
{
	if (is_boxed_fun(t))
	{
		t_fun_t *fun = (t_fun_t *)peel_boxed(t);
		if (fun->fe >= mi->funs_table && fun->fe < mi->funs_table +mi->num_funs)
			return 1;	// fun->fe can be zero
		int num_free = fun_num_free((void *)fun);
		for (int i = 0; i < num_free; i++)
		{
			if (has_module_fun(mi, fun->frozen[i]))
				return 1;
		}
	}
	else if (is_tuple(t))
	{
		uint32_t *p = peel_tuple(t);
		uint32_t sz = *p++;
		for (int i = 0; i < sz; i++)
		{
			if (has_module_fun(mi, p[i]))
				return 1;
		}
	}
	else if (is_cons(t))
	{
		do {
			term_t *cons = peel_cons(t);
			if (has_module_fun(mi, cons[0]))
				return 1;
			t = cons[1];
		} while (is_cons(t));
		if (!is_nil(t) && has_module_fun(mi, t))
			return 1;
	}

	return 0;
}

int proc_references_module(proc_t *proc, module_info_t *mi)
{
	// How a module can be referenced by a process:
	//	1. ip
	//	2. cp
	//	3. code ref on the stack
	//	4. funs reachable from regs and stack
	//	5. catch ref
	//

	if (is_module_code(mi, proc->cap.ip))
		return 1;
	if (is_module_code(mi, proc->cap.cp))
		return 1;	// cp can be zero

	// stack
	uint32_t *sp = proc_stack_top(proc);
	uint32_t *sbot = proc_stack_bottom(proc);
	while (sp < sbot)
	{
		term_t t = *sp++;
		if (is_boxed(t) && is_cp(peel_boxed(t)))
		{
			uint32_t *ptr = demasquerade_pointer(t);
			if (is_module_code(mi, ptr))
				return 1;
		}
		else if (is_catch(t))
		{
			int ord = catch_index(t);
			uint32_t *code = catch_jump_to(ord);
			if (is_module_code(mi, code))
				return 1;
		}
		else
		{
			// maybe a module's fun or contains a module's fun
			if (has_module_fun(mi, t))
				return 1;
		}
	}

	// funs in regs
	for (int i = 0; i < proc->cap.live; i++)
	{
		if (has_module_fun(mi, proc->cap.regs[i]))
			return 1;
	}
	
	return 0;
}


//EOF
