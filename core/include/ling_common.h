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

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

// likely()/unlikely()
#include "os.h"

#include "lassert.h"
#include "limits.h"
#include "sys_stats.h"

#ifdef LING_DEBUG
uint32_t *__expand_ptr(uint32_t n);
uint32_t __shrink_ptr(void *p);
#endif

#define QUICK_SIZE	4096

#if defined(__x86_64__)
#define PTR_MASK		__INT64_C(0x0000000000000000)
#endif

#ifdef LING_DEBUG
#define expand_ptr(n)	__expand_ptr((n))
#define shrink_ptr(p)	__shrink_ptr((p))
#else /* !LING_DEBUG */
#if defined(__x86_64__)
#define expand_ptr(n)	((uint32_t *)((uint64_t)(n) | PTR_MASK))
#define shrink_ptr(p)	((uint32_t)(uint64_t)(p))
#else /*__x86_64__*/
#define expand_ptr(n)	((uint32_t *)(n))
#define shrink_ptr(p)	((uint32_t)(p))
#endif
#endif

#define LING_INFINITY		(0xffffffffffffffff)

typedef uint8_t byte_t;

extern uint64_t start_of_day_wall_clock;

extern char my_domain_name[];

// returned by recursive functions
#define NO_MEMORY		1
#define BAD_ARG			2
#define TOO_LONG		3
#define TOO_DEEP		4
#define NOT_FOUND		5

#define UNUSED __attribute__((unused))

#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)

// hot/cold attributes on labels need gcc >= 4.8
#if __GNUC__ >= 4 && __GNUC_MINOR__ >= 8
#define ATTRIBUTE_HOT __attribute((__hot__))
#define ATTRIBUTE_COLD __attribute((__cold__))
#else
#define ATTRIBUTE_HOT
#define ATTRIBUTE_COLD
#endif

// A useless macro to wrap terms before passing them to printf-like
// functions.
#define T(t)	((void *)(unsigned long)(t))

#ifdef DEBUG_UNUSED_MEM
// Freed memory is reset to this
#define UNUSED_MEM_SIGN		(0xdeadbeef)
#endif

void printk(const char *fmt, ...); // __attribute__ ((format (printf, 1, 2)));

void fatal_error(const char *fmt, ...) __attribute__ ((noreturn)); // __attribute__ ((format (printf, 1, 2)))

#ifdef LING_DEBUG
#	ifdef LING_XEN
#		define debug(fmt, ...) printk(fmt, ## __VA_ARGS__)
#	else
		int debug(const char *fmt, ...);
#	endif
#else
#define debug(fmt, ...)
#endif

#define ssi(what)				sys_stats_inc((what))
#define ssa(what, n)			sys_stats_add((what), (n))

#define not_implemented(what) \
	fatal_error("not implemented: %s", what)

#ifdef LING_DEBUG
// a cozy place to set breakpoints
void gdb_break(void);
#endif

#ifdef TRACE_HARNESS
#define TRACE_MASK_NONE			0
#define TRACE_MASK_EXCEPTION	1
#define TRACE_MASK_BIF			2
#define TRACE_MASK_CALL			4
#define TRACE_MASK_ALL			(TRACE_MASK_EXCEPTION | TRACE_MASK_BIF | TRACE_MASK_CALL)
extern uint32_t trace_mask;
extern uint32_t trace_module;	// term_t
#endif

void domain_poweroff(int status); // __attribute__ ((noreturn));
void yield(void);

#ifdef LING_DEBUG
enum sched_phase_t {
	PHASE_NONE,
	PHASE_NEXT,
	PHASE_ERLANG,
	PHASE_EVENTS,
};
void phase_expected(enum sched_phase_t phase);
void phase_expected2(enum sched_phase_t phase1, enum sched_phase_t phase2);
#else
#define phase_expected(phase)
#define phase_expected2(phase1, phase2)
#endif

//EOF
