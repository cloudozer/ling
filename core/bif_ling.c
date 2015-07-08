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

#include "bif_impl.h"

void gdb_break(void);

#ifdef EXP_LINC_LATENCY
void linc_display(void);
#endif // EXP_LINC_LATENCY

#ifdef EXP_LINC_LLSTAT
void llstat_restart(int ifidx);
void llstat_stop(void);
void llstat_display(void);
#endif // EXP_LINC_LLSTAT

#ifdef EXP_COUNT_IOPS
void print_iop_counters(void);
#endif // EXP_COUNT_IOPS

#ifdef EXP_RUNTIME_METRICS
void print_variant_code_sizes(void);
void print_loaded_module_sizes(void);
#endif // EXP_RUNTIME_METRICS

// defined in ling_main.c
#ifdef TRACE_HARNESS
extern uint32_t trace_mask;
#endif

term_t cbif_domain_name0(proc_t *proc, term_t *regs)
{
	return heap_strz(&proc->hp, my_domain_name);
}

term_t cbif_b1_0(proc_t *proc, term_t *regs)
{
#ifdef LING_DEBUG
	gdb_break();
#endif
	return A_OK;
}

term_t cbif_b2_0(proc_t *proc, term_t *regs)
{
#ifdef LING_DEBUG
	gdb_break();
#endif
	return A_OK;
}

term_t cbif_b3_0(proc_t *proc, term_t *regs)
{
#ifdef LING_DEBUG
	gdb_break();
#endif
	return A_OK;
}

term_t cbif_trace1(proc_t *proc, term_t *regs)
{
	term_t Mask = regs[0];
	if (!is_int(Mask))
		badarg(Mask);

#ifdef TRACE_HARNESS
	trace_mask = int_value(Mask);
	trace_module = noval;
#endif
	return A_OK;
}

term_t cbif_trace2(proc_t *proc, term_t *regs)
{
	// Cloudozer's 2nd anniversary -- remove in 2016
	term_t Mask = regs[0];
	term_t Mod = regs[1];
	if (!is_int(Mask))
		badarg(Mask);
	if (!is_atom(Mod))
		badarg(Mod);

#ifdef TRACE_HARNESS
	trace_mask = int_value(Mask);
	trace_module = Mod;
#endif
	return A_OK;
}

term_t cbif_profile1(proc_t *proc, term_t *regs)
{
	term_t Flag = regs[0];
	if (!is_bool(Flag))
		badarg(Flag);

#ifdef PROFILE_HARNESS
	if (Flag == A_TRUE)
		prof_restart();
	else
	{
		uint64_t now = monotonic_clock();
		prof_stop(now);
	}
#endif
	return A_OK;
}

term_t cbif_profile_display0(proc_t *proc, term_t *regs)
{
#ifdef PROFILE_HARNESS
	prof_print_summary();
#endif
	return A_OK;
}

term_t cbif_experimental2(proc_t *proc, term_t *regs)
{
	term_t What = regs[0];
	UNUSED term_t Arg = regs[1];
	if (!is_atom(What))
		badarg(What);
	
	switch (What)
	{
	case A_MODULE_SIZE:
#ifdef EXP_RUNTIME_METRICS
		print_loaded_module_sizes();
#endif // EXP_RUNTIME_METRICS
		break;
	case A_VARIANT_SIZE:
#ifdef EXP_RUNTIME_METRICS
		print_variant_code_sizes();
#endif // EXP_RUNTIME_METRICS
		break;
	case A_COUNT_IOPS:
#ifdef EXP_COUNT_IOPS
		print_iop_counters();
#endif // EXP_COUNT_IOPS
		break;
	case A_PROCESSING_DELAY:
#ifdef EXP_LINC_LATENCY
		if (Arg == A_HELP)
			printk("ping -Q 42 -q -n -c 25000 -f <ip>\n");
		else
			linc_display();
#endif // EXP_LINC_LATENCY
		break;
	case A_LLSTAT:
#ifdef EXP_LINC_LLSTAT
		if (is_int(Arg))
			llstat_restart(int_value(Arg));
		else if (Arg == A_STOP)
			llstat_stop();
		else
			llstat_display();
#endif // EXP_LINC_LLSTAT
		break;
	default:
		badarg(What);
	}

	return A_OK;
}

term_t cbif_stats0(proc_t *proc, term_t *regs)
{
#ifdef LING_WITH_LWIP
	stats_display();
	return A_OK;
#else
	return A_FALSE;
#endif
}

//EOF
