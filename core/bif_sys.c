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

extern int max_backtrace_depth;
extern void *int_code_end_label;

extern hash_t *registry;

term_t fun_get_info(t_fun_t *f, term_t what, heap_t *hp);
term_t export_get_info(t_export_t *x, term_t what);

term_t cbif_system_flag2(proc_t *proc, term_t *regs)
{
	term_t Flag = regs[0];
	term_t Value = regs[1];

	if (!is_atom(Flag))
		badarg(Flag);

	if (Flag == A_BACKTRACE_DEPTH)
	{
		if (!is_int(Value))
			badarg(Value);
		int depth = int_value(Value);
		if (depth < 3)
			badarg(Value);

		int old_depth = max_backtrace_depth;
		max_backtrace_depth = depth;
		
		assert(fits_int(old_depth));
		return tag_int(old_depth);
	}
	else
		badarg(Flag);
}

term_t cbif_statistics1(proc_t *proc, term_t *regs)
{
	term_t What = regs[0];
	if (What == A_CONTEXT_SWITCHES)
	{
		uint64_t ctx_switches = sys_stats_ctx_switches();
		term_t n = int_to_term(ctx_switches, &proc->hp);
		return heap_tuple2(&proc->hp, n, tag_int(0));
	}
	else if (What == A_GARBAGE_COLLECTION)
	{
		uint64_t gc_runs = sys_stats_gc_runs();
		term_t n1 = int_to_term(gc_runs, &proc->hp);
		uint64_t words_reclaimed = sys_stats_gc_words_reclaimed();
		term_t n2 = int_to_term(words_reclaimed, &proc->hp);
		return heap_tuple3(&proc->hp, n1, n2, tag_int(0));
	}
	else if (What == A_IO)
	{
		uint64_t input = sys_stats_io_input();
		term_t n1 = int_to_term(input, &proc->hp);
		term_t r1 = heap_tuple2(&proc->hp, A_INPUT, n1);
		uint64_t output = sys_stats_io_output();
		term_t n2 = int_to_term(output, &proc->hp);
		term_t r2 = heap_tuple2(&proc->hp, A_OUTPUT, n2);
		return heap_tuple2(&proc->hp, r1, r2);
	}
	if (What == A_REDUCTIONS)
	{
		static uint64_t last_reductions = 0;
		uint64_t reductions = sys_stats_reductions();
		term_t n1 = int_to_term(reductions, &proc->hp);
		term_t n2 = int_to_term(reductions - last_reductions, &proc->hp);
		term_t r = heap_tuple2(&proc->hp, n1, n2);
		last_reductions = reductions;
		return r;
	}
	else if (What == A_RUN_QUEUE)
	{
		int run_queue = scheduler_run_queue();
		assert(fits_int(run_queue));
		return tag_int(run_queue);
	}
	else if (What == A_RUNTIME)
	{
		static uint64_t last_runtime = 0;
		if (last_runtime == 0)
			last_runtime = start_of_day_wall_clock /1000000;
		uint64_t runtime_ms = scheduler_runtime_get() /1000000;
		term_t n1 = int_to_term(runtime_ms, &proc->hp);
		term_t n2 = int_to_term(runtime_ms - last_runtime, &proc->hp);
		term_t r = heap_tuple2(&proc->hp, n1, n2);
		last_runtime = runtime_ms;
		return r;
	}
	else if (What == A_WALL_CLOCK)
	{
		static uint64_t last_wclock = 0;
		if (last_wclock == 0)
			last_wclock = start_of_day_wall_clock /1000000;
		uint64_t wclock = (wall_clock() - start_of_day_wall_clock) /1000000;
		term_t n1 = int_to_term(wclock, &proc->hp);
		term_t n2 = int_to_term(wclock - last_wclock, &proc->hp);
		term_t r = heap_tuple2(&proc->hp, n1, n2);
		last_wclock = wclock;
		return r;
	}

	badarg(What);
}

static uint32_t all_proc_allocated(void)
{
	uint32_t mem_size = 0;
	hash_index_t hi;
	hash_start(registry, &hi);
	proc_t *q;
	while ((q = hash_next(&hi)) != 0)
		mem_size += proc_estimate_allocated_memory(q);

	return mem_size;
}

static uint32_t system_memory(void)
{
	uint32_t mem_size = 0;
	mem_size += atoms_estimate_allocated_memory();
	mem_size += total_binary_size;
	mem_size += code_base_estimate_memory();
	mem_size += total_ets_alloc_size;

	//TODO: netif, timers, counters, etc
	
	return mem_size;
}

term_t cbif_memory1(proc_t *proc, term_t *regs)
{
	term_t Type = regs[0];
	if (!is_atom(Type))
		badarg();

	uint32_t mem_size = 0;
	if (Type == A_TOTAL)
		mem_size = all_proc_allocated() + system_memory();
	if (Type == A_PROCESSES)
		mem_size = all_proc_allocated();
	else if (Type == A_PROCESSES_USED)
	{
		hash_index_t hi;
		hash_start(registry, &hi);
		proc_t *q;
		while ((q = hash_next(&hi)) != 0)
			mem_size += proc_estimate_used_memory(q);
	}
	else if (Type == A_SYSTEM)
		mem_size = system_memory();
	else if (Type == A_ATOM)
		mem_size = atoms_estimate_allocated_memory();
	else if (Type == A_ATOM_USED)
		mem_size = atoms_estimate_used_memory();
	else if (Type == A_BINARY)
		mem_size = total_binary_size;
	else if (Type == A_CODE)
		mem_size = code_base_estimate_memory();
	else if (Type == A_ETS)
		mem_size = total_ets_alloc_size;
	else
		badarg(Type);

	return int_to_term(mem_size, &proc->hp);
}

term_t cbif_processes0(proc_t *proc, term_t *regs)
{
	return scheduler_list_processes(&proc->hp);
}

term_t cbif_ports0(proc_t *proc, term_t *regs)
{
	return outlet_all(&proc->hp);
}

term_t cbif_registered0(proc_t *proc, term_t *regs)
{
	return scheduler_list_registered(&proc->hp);
}

term_t cbif_register2(proc_t *proc, term_t *regs)
{
	term_t Name = regs[0];
	term_t PidOid = regs[1];

	if (!is_atom(Name))
		badarg(Name);
	if (!is_short_pid(PidOid) && !is_short_oid(PidOid))
		badarg(PidOid);
	if (Name == A_UNDEFINED)
		badarg(Name);
	
	if (scheduler_process_by_name(Name) != 0 ||
			outlet_lookup_by_name(Name) != 0)
		badarg(Name);

	if (likely(is_short_pid(PidOid)))
	{
		proc_t *reg_proc = scheduler_lookup(PidOid);
		if (reg_proc == 0 || reg_proc->name != noval)
			badarg(PidOid);
		scheduler_register(reg_proc, Name);
	}
	else
	{
		outlet_t *reg_ol = outlet_lookup(PidOid);
		if (reg_ol == 0 || reg_ol->name != noval)
			badarg(PidOid);
		outlet_register(reg_ol, Name);
	}

	return A_TRUE;
}

term_t cbif_unregister1(proc_t *proc, term_t *regs)
{
	term_t RegName = regs[0];
	if (!is_atom(RegName))
		badarg(RegName);

	proc_t *reg_proc = scheduler_process_by_name(RegName);
	if (unlikely(reg_proc == 0))
	{
		outlet_t *ol = outlet_lookup_by_name(RegName);
		if (ol == 0)
			badarg(RegName);
		outlet_unregister(ol);
	}
	else
		scheduler_unregister(reg_proc);

	return A_TRUE;
}

term_t cbif_whereis1(proc_t *proc, term_t *regs)
{
	term_t RegName = regs[0];
	if (!is_atom(RegName))
		badarg(RegName);
	proc_t *reg_proc = scheduler_process_by_name(RegName);
	if (unlikely(reg_proc == 0))
	{
		outlet_t *reg_ol = outlet_lookup_by_name(RegName);
		if (reg_ol == 0)
			return A_UNDEFINED;
		return reg_ol->oid;
	}
	else
		return reg_proc->pid;
}

term_t cbif_display1(proc_t *proc, term_t *regs)
{
	printk("%pt\n", T(regs[0]));
	return A_OK;
}

term_t cbif_process_display2(proc_t *proc, term_t *regs)
{
	term_t Pid = regs[0];
	term_t What = regs[1];

	if (!is_short_pid(Pid))
		badarg(Pid);
	if (What != A_BACKTRACE)
		badarg(What);

	proc_t *probe = scheduler_lookup(Pid);
	if (probe == 0)
		badarg(Pid);

//
// Sample output of 'bt(self())'.
//
// Program counter: 0xb4850864 (code_server:call/2 + 60)
// CP: 0x00000000 (invalid)
// 
// 0xb490e260 Return addr 0xb48c5590 (erl_eval:do_apply/5 + 1344)
// y(0)     Catch 0xb48a1e58 (c:bt/1 + 40)
//
// 0xb490e268 Return addr 0xb48af3c0 (shell:exprs/7 + 368)
// y(0)     []
// y(1)     []
// y(2)     none
//

	uint32_t *fi = backstep_to_func_info(probe->cap.ip);
	int disp = probe->cap.ip -fi;
	if (fi != 0)
		printk("Program counter: 0x%pp (%pt:%pt/%d +%d)\n",
				probe->cap.ip, T(fi[1]), T(fi[2]), fi[3], disp);
	else
		printk("Program counter: 0x%pp\n", probe->cap.ip);
	
	uint32_t *cp = probe->cap.cp;
	if (cp == 0)
		printk("CP: 0x0 (invalid)\n");
	else
	{
		fi = backstep_to_func_info(cp);
		disp = cp -fi;
		if (fi != 0)
			printk("CP: 0x%pp (%pt:%pt/%d +%d)\n",
					cp, T(fi[1]), T(fi[2]), fi[3], disp);
		else
			printk("CP: 0x%pp\n", cp);
	}

	uint32_t *sp = probe->stop;
	uint32_t *sbot = proc_stack_bottom(probe);

	do {
		cp = demasquerade_pointer(sp[0]);
		if (cp[0] == shrink_ptr(int_code_end_label))
			printk("\n%pp Return addr 0x%pp (<terminate process normally>)\n", sp, cp);
		else
		{
			fi = backstep_to_func_info(cp);
			disp = cp -fi;
			if (fi != 0)
				printk("\n0x%pp Return addr 0x%pp (%pt:%pt/%d +%d)\n",
							sp, cp, T(fi[1]), T(fi[2]), fi[3], disp);
			else
				printk("\n0x%pp Return addr 0x%pp\n", sp, cp);
		}
		int y = 0;
		while (++sp < sbot)
		{
			if (is_boxed(*sp) && is_cp(peel_boxed(*sp)))
				break;
			if (is_catch(*sp))
			{
				uint32_t index = catch_index(*sp);
				uint32_t *where = catch_jump_to(index);
				fi = backstep_to_func_info(where);
				disp = where -fi;
				if (fi != 0)
					printk("y(%d)\tCatch 0x%pp (%pt:%pt/%d +%d)\n",
								y, where, T(fi[1]), T(fi[2]), fi[3], disp);
				else
					printk("y(%d)\tCatch 0x%pp\n", y, where);
			}
			else
				printk("y(%d)\t%pt\n", y, T(*sp));
			y++;
		}
	} while (sp < sbot);

	return A_OK;
}

// erlang:loaded()
term_t cbif_loaded0(proc_t *proc, term_t *regs)
{
	return code_base_list_loaded(&proc->hp);
}

// erlang:pre_loaded()
term_t cbif_pre_loaded0(proc_t *proc, term_t *regs)
{
	return code_base_list_pre_loaded(&proc->hp);
}

// erlang:module_loaded(Mod)
term_t cbif_module_loaded1(proc_t *proc, term_t *regs)
{
	term_t Mod = regs[0];
	if (!is_atom(Mod))
		badarg(Mod);

	module_info_t *module = code_base_module_by_name(Mod, 0);
	if (module != 0)
		return A_TRUE;

	return A_FALSE;
}

// erlang:load_module(Module, Binary)
term_t cbif_load_module2(proc_t *proc, term_t *regs)
{
	term_t Mod = regs[0];
	term_t Bin = regs[1];
	if (!is_atom(Mod))
		badarg(Mod);
	if (!is_boxed_binary(Bin))
		badarg(Bin);

	module_info_t *module = code_base_module_by_name(Mod, 0);
	int old_code_not_purged = (code_base_module_by_name(Mod, 1) != 0);

	if ((module != 0) && old_code_not_purged)
		return heap_tuple2(&proc->hp, A_ERROR, A_NOT_PURGED);

	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);
	int ling_size;
	uint8_t *ling_data;
	if ((bs.starts & 7) == 0 && (bs.ends & 7) == 0)
	{
		ling_size = (bs.ends - bs.starts) /8;
		ling_data = bs.data + bs.starts/8;
	}
	else
	{
		if (((bs.ends - bs.starts) & 7) != 0)
			badarg(Bin);
		ling_size = (bs.ends - bs.starts) /8;
		// EXCEPTION POSSIBLE
		ling_data = heap_tmp_buf(&proc->hp, ling_size);
		bits_t to;
		bits_init_buf(ling_data, ling_size, &to);
		bits_copy(&bs, &to);
	}

	// current code -> old code
	if (module != 0)
		code_base_retire(Mod);

	int x = code_base_load_N(Mod, ling_data, ling_size);
	if (x < 0)
	{
		term_t reason = A_BADFILE;
		if (x == -NO_MEMORY)
			reason = A_NOT_LOADED; //XXX: code is retired already
		return heap_tuple2(&proc->hp, A_ERROR, reason);
	}

	return heap_tuple2(&proc->hp, A_MODULE, Mod);
}


term_t cbif_delete_module1(proc_t *proc, term_t *regs)
{
	term_t Mod = regs[0];
	if (!is_atom(Mod))
		badarg(Mod);

	module_info_t *module = code_base_module_by_name(Mod, 1);
	if (module != 0)
		badarg(Mod);

	module = code_base_module_by_name(Mod, 0);
	if (module == 0)
		return A_UNDEFINED;

	code_base_retire(Mod);
	return A_TRUE;
}

term_t cbif_check_old_code1(proc_t *proc, term_t *regs)
{
	term_t Mod = regs[0];
	if (!is_atom(Mod))
		badarg(Mod);

	module_info_t *module = code_base_module_by_name(Mod, 1);
	return (module != 0) ?A_TRUE :A_FALSE;
}

term_t cbif_check_process_code2(proc_t *proc, term_t *regs)
{
	term_t Pid = regs[0];
	if (!is_short_pid(Pid))
		badarg(Pid);
	term_t Mod = regs[1];
	if (!is_atom(Mod))
		badarg(Mod);

	module_info_t *module = code_base_module_by_name(Mod, 1);
	if (module == 0)	// no old code
		return A_FALSE;
	
	proc_t *check_proc = scheduler_lookup(Pid);
	if (check_proc == 0)
		return A_FALSE;	// no process to check

	int in_use = proc_references_module(check_proc, module);
	return (in_use) ?A_TRUE :A_FALSE;
}

term_t cbif_get_module_info2(proc_t *proc, term_t *regs)
{
	term_t Mod = regs[0];
	term_t Item = regs[1];
	if (!is_atom(Mod))
		badarg(Mod);
	if (!is_atom(Item))
		badarg(Item);

	module_info_t *module = code_base_module_by_name(Mod, 0);
	if (module == 0 && (Item == A_EXPORTS || Item == A_IMPORTS))
		return nil;
	if (module == 0)
		badarg(Mod);

	term_t val;
	if (Item == A_EXPORTS)
		val = code_base_list_module_exports(Mod, &proc->hp);
	else if (Item == A_IMPORTS)
		val = nil;
	else if (Item == A_ATTRIBUTES)
	{
		val = nil;
		//TODO
	}
	else if (Item == A_COMPILE)
	{
		val = nil;
		//TODO
	}
	else if (Item == A_FUNCTIONS)
		return nil;		// TODO
	else if (Item == A_MODULE)	// undocumented
		val = Mod;
	else
		badarg(Item);

	return val;
}

term_t cbif_function_exported3(proc_t *proc, term_t *regs)
{
	term_t Mod = regs[0];
	term_t Func = regs[1];
	term_t Arity = regs[2];
	if (!is_atom(Mod) || !is_atom(Func) || !is_int(Arity))
		badarg();
	term_t n = int_value(Arity);
	if (n < 0 || n > 255)
		badarg(Arity);
	export_t *e = code_base_lookup(Mod, Func, n);
	if (e == 0)
		return A_FALSE;
	return (e->entry != 0)
		?A_TRUE
		:A_FALSE;
}

term_t cbif_fun_info1(proc_t *proc, term_t *regs)
{
	term_t FunExp = regs[0];
	if (!is_boxed(FunExp))
		badarg(FunExp);
	uint32_t *tdata = peel_boxed(FunExp);
	if (boxed_tag(tdata) == SUBTAG_FUN)
	{
#define NUM_FUN_INFO	10
		term_t infos[NUM_FUN_INFO] = {
			A_TYPE,
			A_MODULE,
			A_NAME,
			A_ARITY,
			A_ENV,
			A_PID,
			A_INDEX,
			A_NEW_INDEX,
			A_UNIQ,
			A_NEW_UNIQ,
		};

		for (int i = 0; i < NUM_FUN_INFO; i++)
		{
			term_t info = fun_get_info((t_fun_t *)tdata, infos[i], &proc->hp);
			infos[i] = heap_tuple2(&proc->hp, infos[i], info);
		}

		return heap_vector_to_list(&proc->hp, infos, NUM_FUN_INFO);
	}
	else if (boxed_tag(tdata) == SUBTAG_EXPORT)
	{
#define NUM_EXPORT_INFO	4
		term_t infos[NUM_EXPORT_INFO] = {
			A_TYPE,
			A_MODULE,
			A_NAME,
			A_ARITY,
		};

		for (int i = 0; i < NUM_EXPORT_INFO; i++)
		{
			term_t info = export_get_info((t_export_t *)tdata, infos[i]);
			infos[i] = heap_tuple2(&proc->hp, infos[i], info);
		}

		return heap_vector_to_list(&proc->hp, infos, NUM_EXPORT_INFO);
	}

	badarg(FunExp);
}

term_t cbif_fun_info2(proc_t *proc, term_t *regs)
{
	term_t FunExp = regs[0];
	term_t What = regs[1];
	if (!is_boxed(FunExp))
		badarg(FunExp);
	if (!is_atom(What))
		badarg(What);
	uint32_t *tdata = peel_boxed(FunExp);
	if (boxed_tag(tdata) == SUBTAG_FUN)
	{
		term_t info = fun_get_info((t_fun_t *)tdata, What, &proc->hp);
		if (info == noval)
			badarg(What);
		return heap_tuple2(&proc->hp, What, info);
	}
	if (boxed_tag(tdata) == SUBTAG_EXPORT)
	{
		term_t info = export_get_info((t_export_t *)tdata, What);
		if (info == noval)
			badarg(What);
		return heap_tuple2(&proc->hp, What, info);
	}

	badarg(FunExp);
}

term_t cbif_halt2(proc_t *proc, term_t *regs)
{
	term_t Status = regs[0];
	term_t Flush = regs[1];	// ignored
	if (!is_int(Status) && !is_list(Status) && Status != A_ABORT)
		badarg(Status);
	if (!is_bool(Flush))
		badarg(Flush);		//TODO: handle higher up

	if (!is_int(Status))
	{
		if (is_list(Status))
			printk("*** halt: %pt\n", T(Status));
	}

	domain_poweroff();
	
	return A_TRUE;
}

term_t cbif_now0(proc_t *proc, term_t *regs)	// UTC
{
	uint64_t wclock = wall_clock();
	uint64_t wclock_us = wclock / 1000;
	
	static uint64_t saved_wclock_us = 0;
	if (saved_wclock_us >= wclock_us)
		wclock_us = saved_wclock_us+1;
	saved_wclock_us = wclock_us;

	int secs = wclock_us / 1000000;
	int usecs = wclock_us % 1000000;
	
	int msecs = secs / 1000000;
	secs = secs % 1000000;

	return heap_tuple3(&proc->hp,
			tag_int(msecs), tag_int(secs), tag_int(usecs));
}

//date() -> {Year, Month, Day}
term_t cbif_date0(proc_t *proc, term_t *regs)
{
	struct time_exp_t exp;

	uint64_t now = wall_clock()	+ TIMEZONE_NS;
	expand_time(&exp, now);

    /** (1-31) day of the month */
    //int32_t tm_mday;
    /** (0-11) month of the year */
    //int32_t tm_mon;
    /** year since 1900 */
    //int32_t tm_year;

	return heap_tuple3(&proc->hp,
		tag_int(exp.tm_year+1900),
		tag_int(exp.tm_mon+1),
		tag_int(exp.tm_mday));
}

//time() -> {Hour, Minute, Second}
term_t cbif_time0(proc_t *proc, term_t *regs)
{
	struct time_exp_t exp;

	uint64_t now = wall_clock() + TIMEZONE_NS;
	expand_time(&exp, now);

	/** (0-61) seconds past tm_min */
    //int32_t tm_sec;
    /** (0-59) minutes past tm_hour */
    //int32_t tm_min;
    /** (0-23) hours past midnight */
    //int32_t tm_hour;

	return heap_tuple3(&proc->hp,
		tag_int(exp.tm_hour),
		tag_int(exp.tm_min),
		tag_int(exp.tm_sec));
}

//localtime -> {date(),time()}
term_t cbif_localtime0(proc_t *proc, term_t *regs)
{
	struct time_exp_t exp;

	uint64_t now = wall_clock() + TIMEZONE_NS;
	expand_time(&exp, now);

    /** (1-31) day of the month */
    //int32_t tm_mday;
    /** (0-11) month of the year */
    //int32_t tm_mon;
    /** year since 1900 */
    //int32_t tm_year;

	term_t dt = heap_tuple3(&proc->hp,
		tag_int(exp.tm_year+1900),
		tag_int(exp.tm_mon+1),
		tag_int(exp.tm_mday));

	/** (0-61) seconds past tm_min */
    //int32_t tm_sec;
    /** (0-59) minutes past tm_hour */
    //int32_t tm_min;
    /** (0-23) hours past midnight */
    //int32_t tm_hour;

	term_t tm = heap_tuple3(&proc->hp,
		tag_int(exp.tm_hour),
		tag_int(exp.tm_min),
		tag_int(exp.tm_sec));

	return heap_tuple2(&proc->hp, dt, tm);
}

//universaltime -> {date(),time()}
term_t cbif_universaltime0(proc_t *proc, term_t *regs)
{
	struct time_exp_t exp;

	uint64_t now = wall_clock();	// UTC
	expand_time(&exp, now);

    /** (1-31) day of the month */
    //int32_t tm_mday;
    /** (0-11) month of the year */
    //int32_t tm_mon;
    /** year since 1900 */
    //int32_t tm_year;

	term_t dt = heap_tuple3(&proc->hp,
		tag_int(exp.tm_year+1900),
		tag_int(exp.tm_mon+1),
		tag_int(exp.tm_mday));

	/** (0-61) seconds past tm_min */
    //int32_t tm_sec;
    /** (0-59) minutes past tm_hour */
    //int32_t tm_min;
    /** (0-23) hours past midnight */
    //int32_t tm_hour;

	term_t tm = heap_tuple3(&proc->hp,
		tag_int(exp.tm_hour),
		tag_int(exp.tm_min),
		tag_int(exp.tm_sec));

	return heap_tuple2(&proc->hp, dt, tm);
}

term_t cbif_native_name_encoding0(proc_t *proc, term_t *regs)
{
	return A_UTF8;
}

static term_t start_timer(proc_t *proc, term_t *regs, int enveloped)
{
	term_t Time = regs[0];
	term_t Dest = regs[1];
	term_t Msg = regs[2];

	if (!is_int(Time) && !is_boxed_bignum(Time))
		badarg(Time);
	if (!is_atom(Dest) && !is_short_pid(Dest))
		badarg(Dest);

	term_t tref = heap_make_ref(&proc->hp);
	uint64_t ref_id = local_ref_id(tref);

	int64_t millis = (is_int(Time))
		?int_value(Time)
		:bignum_to_int((bignum_t *)peel_boxed(Time));
	if (millis < 0)
		badarg();

	uint64_t now = monotonic_clock();
	uint64_t timeout = now + millis *1000000;

	//printk("*** start_timer: %ld millis ref_id %ld msg %pt\n", millis, ref_id, T(Msg));
	int x = etimer_add(ref_id, timeout, Dest, Msg, proc, enveloped);
	if (x < 0)
	{
		assert(x == -NO_MEMORY);
		no_memory_signal();
	}

	return tref;
}

term_t cbif_start_timer3(proc_t *proc, term_t *regs)
{
	return start_timer(proc, regs, 1);
}

term_t cbif_send_after3(proc_t *proc, term_t *regs)
{
	return start_timer(proc, regs, 0);
}

term_t cbif_cancel_timer1(proc_t *proc, term_t *regs)
{
	term_t TRef = regs[0];
	if (!is_boxed_ref(TRef))
		badarg(TRef);
	uint64_t ref_id = local_ref_id(TRef);

	int64_t left_ns;
	if (etimer_cancel(ref_id, &left_ns) == 0)
		return int_to_term(left_ns / 1000000, &proc->hp);
	else
		return A_FALSE;
}

term_t cbif_read_timer1(proc_t *proc, term_t *regs)
{
	term_t TRef = regs[0];
	if (!is_boxed_ref(TRef))
		badarg(TRef);
	uint64_t ref_id = local_ref_id(TRef);

	int64_t left_ns;
	if (etimer_read(ref_id, &left_ns) == 0)
		return int_to_term(left_ns / 1000000, &proc->hp);
	else
		return A_FALSE;
}

term_t cbif_parent_node0(proc_t *proc, term_t *regs)
{
	return cluster_parent;
}

term_t cbif_node_group0(proc_t *proc, term_t *regs)
{
	return cluster_group;
}

static int from_hex(int ch)
{
	if (ch >= '0' && ch <= '9')
		return ch -'0';
	if (ch >= 'a' && ch <= 'f')
		return ch -'a' +10;
	if (ch >= 'A' && ch <= 'F')
		return ch -'A' +10;
	return -1;
}

term_t cbif_set_secrets2(proc_t *proc, term_t *regs)
{
	term_t S1 = regs[0];
	term_t S2 = regs[1];
	if (!is_boxed_binary(S1))
		badarg(S1);
	if (!is_boxed_binary(S2))
		badarg(S2);

	bits_t bs1, bs2;
	bits_get_real(peel_boxed(S1), &bs1);
	bits_get_real(peel_boxed(S2), &bs2);

	int64_t bit_size1 = bs1.ends -bs1.starts;
	int64_t bit_size2 = bs2.ends -bs1.starts;
	
	if ((bit_size1 &15) != 0 || bit_size1 > CLUSTER_SECRET_MAX *16)
		return A_FALSE;
	if ((bit_size2 &15) != 0 || bit_size2 > CLUSTER_SECRET_MAX *16)
		return A_FALSE;

	int sz1 = bit_size1 /16;
	int sz2 = bit_size2 /16;

	uint8_t secret1[sz1];
	uint8_t secret2[sz2];

	uint8_t *ptr = secret1;
	while (bits_has_octet(&bs1))
	{
		uint8_t a, b;
		bits_get_octet(&bs1, a);
		bits_get_octet(&bs1, b);

		int da = from_hex(a);
		int db = from_hex(b);
		if (da < 0 || db < 0)
			return A_FALSE;
		
		*ptr++ = (da << 4) | db;
	}

	ptr = secret2;
	while (bits_has_octet(&bs2))
	{
		uint8_t a, b;
		bits_get_octet(&bs2, a);
		bits_get_octet(&bs2, b);

		int da = from_hex(a);
		int db = from_hex(b);
		if (da < 0 || db < 0)
			return A_FALSE;
		
		*ptr++ = (da << 4) | db;
	}

	cluster_secret1_size = sz1;
	memcpy(cluster_secret1, secret1, sz1);
	cluster_secret2_size = sz2;
	memcpy(cluster_secret2, secret2, sz2);

	return A_TRUE;
}

term_t cbif_secret1_0(proc_t *proc, term_t *regs)
{
	uint8_t *ptr;
	term_t secret1 = heap_make_bin(&proc->hp, cluster_secret1_size, &ptr);
	memcpy(ptr, cluster_secret1, cluster_secret1_size);
	return secret1;
}

term_t cbif_secret2_0(proc_t *proc, term_t *regs)
{
	uint8_t *ptr;
	term_t secret2 = heap_make_bin(&proc->hp, cluster_secret2_size, &ptr);
	memcpy(ptr, cluster_secret2, cluster_secret2_size);
	return secret2;
}

term_t cbif_disk_info1(proc_t *proc, term_t *regs)
{
	term_t Item = regs[0];
	if (!is_atom(Item))
		badarg(Item);

	disk_info_t *dinfo = disk_get_info();
	if (dinfo == 0)
		return A_UNDEFINED;	// disk not present

	term_t val;
	if (Item == A_INFO)
		val = int_to_term(dinfo->info, &proc->hp);
	else if (Item == A_SECTORS)
		val = int_to_term(dinfo->sectors, &proc->hp);
	else if (Item == A_SECTOR_SIZE)
		val = int_to_term(dinfo->sector_size, &proc->hp);
	else if (Item == A_FEATURES)
	{
		val = nil;
		if (dinfo->barrier)
			val = heap_cons(&proc->hp, A_BARRIER, val);
		if (dinfo->flush)
			val = heap_cons(&proc->hp, A_FLUSH, val);
		if (dinfo->trim)
			val = heap_cons(&proc->hp, A_TRIM, val);
	}
	else
		badarg(Item);

	return heap_tuple2(&proc->hp, Item, val);
}

term_t cbif_new_counter1(proc_t *proc, term_t *regs)
{
	term_t Bits = regs[0];
	if (!is_int(Bits))
		badarg(Bits);
	int nbits = int_value(Bits);
	if (nbits < 1 || nbits > 64)
		badarg(Bits);
	uint64_t mask = (nbits == 64)
		? 0xffffffffffffffff
		: (1ull << nbits) -1;

	term_t cref = heap_make_ref(&proc->hp);
	uint64_t ref_id = local_ref_id(cref);

	//EXCEPTION POSSIBLE
	counter_add(ref_id, mask);
	
	return cref;
}

term_t cbif_read_counter1(proc_t *proc, term_t *regs)
{
	term_t Ref = regs[0];
	if (!is_boxed_ref(Ref))
		badarg(Ref);
	uint64_t ref_id = local_ref_id(Ref);

	uint64_t val;
	if (counter_read(ref_id, &val) < 0)
		badarg(Ref);

	return uint_to_term(val, &proc->hp);
}

term_t cbif_update_counter2(proc_t *proc, term_t *regs)
{
	term_t Ref = regs[0];
	term_t Incr = regs[1];
	if (!is_boxed_ref(Ref))
		badarg(Ref);
	uint64_t ref_id = local_ref_id(Ref);
	if (!is_int(Incr) && !is_boxed_bignum(Incr))
		badarg(Incr);

	uint64_t val;
	if (is_int(Incr))
	{
		if (int_value(Incr) < 0)
			badarg(Incr);
		val = (uint64_t)int_value(Incr);
	}
	else
	{
		bignum_t *bn = (bignum_t *)peel_boxed(Incr);
		if (bignum_is_neg(bn))
			badarg(Incr);
		val = bignum_to_uint(bn);
	}

	if (counter_increment(ref_id, val) < 0)
		badarg(Ref);

	return A_TRUE;
}

term_t cbif_release_counter1(proc_t *proc, term_t *regs)
{
	term_t Ref = regs[0];
	if (!is_boxed_ref(Ref))
		badarg(Ref);
	uint64_t ref_id = local_ref_id(Ref);

	if (counter_remove(ref_id) < 0)
		badarg(Ref);

	return A_TRUE;
}

//EOF
