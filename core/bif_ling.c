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

void dump_netmap_state(int what);

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
#if LING_XEN
	case A_NETMAP:
		if (Arg == A_TX)
			dump_netmap_state(0);
		else if (Arg == A_RX)
			dump_netmap_state(1);
		break;
#endif
	case A_GC:
		if (Arg == tag_int(1))
			printk("Pages left: %d\n", mm_alloc_left());
		else if (is_short_pid(Arg))
		{
			proc_t *target = scheduler_lookup(Arg);
			assert(target != 0);

			heap_t *hp = &target->hp;
			memnode_t *node = hp->nodes;
			int ch = 0;
			int size = 0;
			printk("ch");
			while (1)
			{
				while (ch < GC_COHORTS && node == hp->gc_cohorts[ch])
				{
					printk(":%d", size);
					ch++;
					size = 0;
				}
				if (node == 0)
					break;
				size++;
				node = node->next;
			}
			printk(":%d\n", size);
		}
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

#if (1)
//LING_XEN
enum elfnames {
	ElfElf,
	ElfClass,
	ElfData,
	ElfOSABI,
	ElfABIVer,
	ElfType,
	ElfMach,
	ElfEntry,

	Elf2lsb, Elf2msb,
};

enum elfmach {
	EM_NONE = 0,
	EM_386 = 3,
	EM_MIPS = 8,
	EM_AMD = 0x3e,
};

typedef struct {
	term_t term;
	char *lname;
} atom_t;

typedef struct {
	const char *name;
	unsigned long start;
	unsigned long end;
} section_t;

/* keep synchrohized with ling.lds */
extern char _text, _etext;
extern char _data, _edata;
extern char _rodata, _erodata;
extern char _bss, _ebss;

const section_t sections[] = {
	{ ".text",   (uintptr_t)&_text,    (uintptr_t)&_etext },
	{ ".data",   (uintptr_t)&_data,    (uintptr_t)&_edata },
	{ ".rodata", (uintptr_t)&_rodata,  (uintptr_t)&_erodata },
	{ ".bss",    (uintptr_t)&_bss,     (uintptr_t)&_ebss },
};

atom_t elfatoms[] = {
	[ElfElf]   = { noval, "_elf" },
	[ElfClass] = { noval, "_class" },
	[ElfData]  = { noval, "_data" },
	[ElfOSABI] = { noval, "_osabi" },
	[ElfABIVer]= { noval, "_abiver" },
	[ElfType]  = { noval, "_type" },
	[ElfMach]  = { noval, "_machine" },
	[ElfEntry] = { noval, "_entryaddr" },

	[Elf2lsb]  = { noval, "_lsb2compl" },
	[Elf2msb]  = { noval, "_msb2compl" },
};


term_t cbif_ling_execinfo0(proc_t *proc, term_t *regs)
{
#if __x86_64__
	const int elfclass = 64;
	const int elfmach = EM_AMD;
#elif __i386
	const int elfclass = 32;
	const int elfmach = EM_386;
#else
# error "unknown architecture"
#endif
	const int elfdata = (ntohs(42) == 42 ? Elf2msb : Elf2lsb);

	int i;
	for (i = 0; i < sizeof(elfatoms)/sizeof(atom_t); ++i)
	{
		uint8_t *lname = (uint8_t *)elfatoms[i].lname;
		size_t atomlen = strlen((char *)lname + 1);
		assert(atomlen < 256);
		lname[0] = (uint8_t)atomlen;
		elfatoms[i].term = tag_atom(atoms_set(lname));
	}

	const size_t infolen = 3;
	term_t info[infolen];
	info[0] = heap_tuple2(&proc->hp, elfatoms[ElfClass].term, tag_int(elfclass));
	info[1] = heap_tuple2(&proc->hp, elfatoms[ElfData].term, elfatoms[elfdata].term);
	info[2] = heap_tuple2(&proc->hp, elfatoms[ElfMach].term, tag_int(elfmach));
	term_t infolist = heap_vector_to_list(&proc->hp, info, infolen);

	size_t nsect = sizeof(sections)/sizeof(section_t);
	term_t sectvec[nsect];

	for (i = 0; i < nsect; ++i)
	{
		term_t sectname = heap_strz(&proc->hp, sections[i].name);
		term_t startaddr = int_to_term(sections[i].start, &proc->hp);
		term_t endaddr = int_to_term(sections[i].end, &proc->hp);
		sectvec[i] = heap_tuple3(&proc->hp, sectname, startaddr, endaddr);
	}
	term_t sectlist = heap_vector_to_list(&proc->hp, sectvec, nsect);

	return heap_tuple3(&proc->hp, elfatoms[ElfElf].term, infolist, sectlist);
}

term_t cbif_ling_memory2(proc_t *proc, term_t *regs)
{
	term_t From = regs[0];
	term_t To = regs[1];

	if (!is_int(From) || !is_int(To))
		return heap_tuple2(&proc->hp, A_ERROR, A_EINVAL);

	/* this initialization produces a warning, and rightly so */
	uintptr_t from = int_value(From);
	uintptr_t to = int_value(To);
	//printk("%s: from=%x, to=%x\n", __FUNCTION__, from, to);
	if (from > to)
		return heap_tuple2(&proc->hp, A_ERROR, heap_strz(&proc->hp, "From after To"));
	size_t len = to - from;
	//printk("%s: len=%d\n", __FUNCTION__, len);

	uint8_t *data = NULL;
	term_t bin = heap_make_bin(&proc->hp, len, &data);
	//printk("%s: bin=0x%x, data=*%x\n", __FUNCTION__, bin, (uintptr_t)data);

	memcpy(data, (void *)from, len);

	return heap_tuple2(&proc->hp, A_OK, bin);
}

term_t cbif_ling_memb1(proc_t *proc, term_t *regs)
{
	term_t Addr = regs[0];
	if (!is_int(Addr))
		return heap_tuple2(&proc->hp, A_ERROR, A_EINVAL);

	uint8_t *addr = (uint8_t *)int_value(Addr);
	return heap_tuple2(&proc->hp, A_OK, tag_int(addr[0]));
}

term_t cbif_ling_meml1(proc_t *proc, term_t *regs)
{
	term_t Addr = regs[0];
	if (!is_int(Addr))
		return heap_tuple2(&proc->hp, A_ERROR, A_EINVAL);

	uint32_t *addr = (uint32_t *)int_value(Addr);
	return heap_tuple2(&proc->hp, A_OK, tag_int(addr[0]));
}
#else // !LING_XEN

term_t cbif_ling_execinfo0(proc_t *proc, term_t *regs)
{
	bif_not_implemented();
}

term_t cbif_ling_memory2(proc_t *proc, term_t *regs)
{
	bif_not_implemented();
}

term_t cbif_ling_memb1(proc_t *proc, term_t *regs)
{
	bif_not_implemented();
}

term_t cbif_ling_meml1(proc_t *proc, term_t *regs)
{
	bif_not_implemented();
}
#endif

