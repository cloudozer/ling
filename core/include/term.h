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

typedef uint32_t term_t;

typedef struct t_float_t t_float_t;
typedef struct t_map_t t_map_t;
typedef struct t_fun_t t_fun_t;
typedef struct t_export_t t_export_t;
typedef struct t_proc_bin_t t_proc_bin_t;
typedef struct t_heap_bin_t t_heap_bin_t;
typedef struct t_sub_bin_t t_sub_bin_t;
typedef struct t_match_ctx_t t_match_ctx_t;
typedef struct t_long_oid_t t_long_oid_t;
typedef struct t_long_pid_t t_long_pid_t;
typedef struct t_long_ref_t t_long_ref_t;
typedef struct t_grave_t t_grave_t;

typedef struct binnode_t binnode_t;
//NB: overlaps memnode_t
struct binnode_t {
	unsigned long refc;	// was: memnode_t *next;
	int index;
	uint8_t *starts;
	uint8_t *ends;
};

typedef struct bits_t bits_t;
struct bits_t {
	uint8_t *data;
	int64_t starts;		// the beginning of the array, bit offset from data
	int64_t ends;		// the end of the array, bit offset from data
};

// Any structures declared below are heap structures and as such they require
// 4-byte alignment as the compiler has expectations about their size.
//
#pragma pack(4)

// an entry of the fun entry of the module
typedef struct fun_entry_t fun_entry_t;

typedef struct export_t export_t;

#ifdef LING_DEBUG
#include "dbg_inst.h"
#endif

// mixed term comparison
int are_terms_equal(term_t a, term_t b, int exact);
int is_term_smaller(term_t a, term_t b);

#define TAG_PRIMARY_SIZE	2
#define TAG_PRIMARY_MASK	0x3

#define PRIMARY_TAG_CONS	0x0
#define PRIMARY_TAG_TUPLE	0x1
#define PRIMARY_TAG_BOXED	0x2
#define PRIMARY_TAG_IMMED	0x3

#define is_cons(t)	(((t) & TAG_PRIMARY_MASK) == PRIMARY_TAG_CONS)
#define is_tuple(t)	(((t) & TAG_PRIMARY_MASK) == PRIMARY_TAG_TUPLE)
#define is_boxed(t)	(((t) & TAG_PRIMARY_MASK) == PRIMARY_TAG_BOXED)
#define is_immed(t)	(((t) & TAG_PRIMARY_MASK) == PRIMARY_TAG_IMMED)

/**
 * Check if a generic 'and' or specific 'sub' is faster for term
 * peeling.
 */

#ifdef LING_DEBUG
#define peel_cons(t)	__peel_cons((t))
#define peel_tuple(t)	__peel_tuple((t))
#define peel_boxed(t)	__peel_boxed((t))
#else /* !LING_DEBUG */
#if 0
#define peel_cons(t)	expand_ptr((uint32_t)(t) & ~TAG_PRIMARY_MASK)
#define peel_tuple(t)	expand_ptr((uint32_t)(t) & ~TAG_PRIMARY_MASK)
#define peel_boxed(t)	expand_ptr((uint32_t)(t) & ~TAG_PRIMARY_MASK)
#else /* !0 */
#define peel_cons(t)	expand_ptr((uint32_t)(t) - PRIMARY_TAG_CONS)
#define peel_tuple(t)	expand_ptr((uint32_t)(t) - PRIMARY_TAG_TUPLE)
#define peel_boxed(t)	expand_ptr((uint32_t)(t) - PRIMARY_TAG_BOXED)
#endif
#endif
#define peel_any(t)		expand_ptr((t) & ~TAG_PRIMARY_MASK)

#define primary_tag(t)	((uint32_t)(t) & TAG_PRIMARY_MASK)

#ifdef LING_DEBUG
#define tag_cons(p)			__tag_ptr((p), PRIMARY_TAG_CONS)
#define tag_tuple(p)		__tag_ptr((p), PRIMARY_TAG_TUPLE)		
#define tag_boxed(p)		__tag_ptr((p), PRIMARY_TAG_BOXED)
#else /* !LING_DEBUG */
#define tag_cons(p)			(shrink_ptr(p) | PRIMARY_TAG_CONS)
#define tag_tuple(p)		(shrink_ptr(p) | PRIMARY_TAG_TUPLE)
#define tag_boxed(p)		(shrink_ptr(p) | PRIMARY_TAG_BOXED)
#endif

//
// 0-arity tuples get a special treatment to provide that all term fragments are
// at least 2 words long to have enough space for burying terms during GC
// 
extern uint32_t zero;
#define ZERO_TUPLE	(tag_tuple(&zero))

#define MAX_TUPLE_ARITY  16777215

// Primary and immediate tagging:
//
// xxxx00	cons
// xxxx01	tuple
// xxxx10	boxed (subtag)
// 000011	atom
// 100011	catch index
// 001011	short pid
// 010011	short oid
// 011011	short eid
// 101011	(unused) overloaded as reg X
// 110011	(unused) overloaded as slot Y
// 111011	nil,noval,rip
// xxx111	small integer

#define nil				0xfffffffb
#define noval			0x0000003b
#define R_I_P			0xffffff3b

#define is_nil(t)		((t) == nil)
#define is_not_nil(t)	(!is_nil(t))

#define TAG_IMMED1_SIZE 3
#define TAG_IMMED1_MASK 0x7
#define TAG_IMMED2_SIZE 6
#define TAG_IMMED2_MASK 0x3f

#define is_int(t)	(((t) & TAG_IMMED1_MASK) == 0x7)
#define is_atom(t)	(((t) & TAG_IMMED2_MASK) == 0x3)
#define is_catch(t)	(((t) & TAG_IMMED2_MASK) == 0x23)

#define is_short_pid(t)		(((t) & TAG_IMMED2_MASK) == 0xb)
#define is_short_oid(t)		(((t) & 0x17) == 0x13)
#define is_short_eid(t)		(((t) & TAG_IMMED2_MASK) == 0x1b)

#define MAX_INT_VALUE	((1 << (32 - TAG_IMMED1_SIZE - 1)) - 1)
#define MIN_INT_VALUE	(-1 << (32 - TAG_IMMED1_SIZE - 1))

#define fits_int(i)		(((i) >= MIN_INT_VALUE) && ((i) <= MAX_INT_VALUE))

#define is_list(t)	(is_cons(t) || is_nil(t))

// Register and slot references; tagged instruction arguments only

//TODO: is_reg_or_slot() may make code faster but requires new bit layout

#define is_reg(t)		(((t) & TAG_IMMED2_MASK) == 0x2b)
#define is_slot(t)		(((t) & TAG_IMMED2_MASK) == 0x33)

#define reg_index(t)	((t) >> TAG_IMMED2_SIZE)
#define slot_index(t)	((t) >> TAG_IMMED2_SIZE)

#define reg0			(reg_as_term(0))
#define reg_as_term(x)	((((uint32_t)(x)) << TAG_IMMED2_SIZE) | 0x2b)
#define slot_as_term(y)	((((uint32_t)(y)) << TAG_IMMED2_SIZE) | 0x33)

// Box subtagging
//
// float, binary, fun, ppp, bignum
//
// x0000		bignum (positive)
// x0001		bignum (negative)
// x0010		float
// x0011		map
// x0100		(unused)
// x0101		(unused)
// x0110		fun
// x0111		export fun
// x1000		long pid
// x1001		long oid
// x1010		long ref
// x1011		R.I.P.
// x1100		binary (proc_bin)
// x1101		binary (heap_bin)
// x1110		binary (match_context)
// x1111		binary (sub_bin)

// The highest bit should be set for all boxed value.  The intention is to
// distinguish ordinary boxed values from continuation pointers masqueraded as
// such.
#define HEADER_CP_MASK			0x80000000
#define HDR_IS_NOT_CP			0x80000000
#define HDR_IS_CP				0x00000000

#define SUBTAG_MASK				0xf
#define SUBTAG_POS_BIGNUM		0x0
#define SUBTAG_NEG_BIGNUM		0x1
#define SUBTAG_FLOAT			0x2
#define SUBTAG_MAP				0x3
#define SUBTAG_FUN				0x6
#define SUBTAG_EXPORT			0x7
#define SUBTAG_PID				0x8
#define SUBTAG_OID				0x9
#define SUBTAG_REF				0xa
#define SUBTAG_RIP				0xb
#define SUBTAG_PROC_BIN			0xc
#define SUBTAG_HEAP_BIN			0xd
#define SUBTAG_MATCH_CTX		0xe
#define SUBTAG_SUB_BIN			0xf

#define SUBTAG_BIGNUM_MASK		0xe
#define SUBTAG_BIGNUM			0

#define SUBTAG_BINARY_MASK		0xc
#define SUBTAG_BINARY			0xc

#define is_cp(p)	((*(uint32_t *)(p) & HEADER_CP_MASK) == HDR_IS_CP)
#define is_bignum(p) ((*(uint32_t *)(p) & SUBTAG_BIGNUM_MASK) == SUBTAG_BIGNUM)
#define is_binary(p) ((*(uint32_t *)(p) & SUBTAG_BINARY_MASK) == SUBTAG_BINARY)

#define is_boxed_binary(t)	(is_boxed((t)) && is_binary(peel_boxed((t))))
#define is_boxed_bignum(t)	(is_boxed((t)) && is_bignum(peel_boxed((t))))

#define is_boxed_float(t)	(is_boxed((t)) && boxed_tag(peel_boxed((t))) == SUBTAG_FLOAT)
#define is_boxed_map(t)		(is_boxed((t)) && boxed_tag(peel_boxed((t))) == SUBTAG_MAP)

#define is_boxed_pid(t)	(is_boxed((t)) && boxed_tag(peel_boxed((t))) == SUBTAG_PID)
#define is_boxed_oid(t)	(is_boxed((t)) && boxed_tag(peel_boxed((t))) == SUBTAG_OID)
#define is_boxed_ref(t)	(is_boxed((t)) && boxed_tag(peel_boxed((t))) == SUBTAG_REF)

#define is_boxed_fun(t)	(is_boxed((t)) && boxed_tag(peel_boxed((t))) == SUBTAG_FUN)

#ifdef LING_DEBUG
#define boxed_tag(p)	__boxed_tag((p))
#else /* !LING_DEBUG */
#define boxed_tag(p)	(*(uint32_t *)(p) & SUBTAG_MASK)
#endif

// shortcut type checks
#define are_both_int(a, b)	(((a) & (b) & TAG_IMMED1_MASK) == 0x7)
#define are_both_immed(a, b) (((a) & (b) & TAG_PRIMARY_MASK) == 0x3)

// immediate term tagging/untagging
#define tag_int(i)			(((term_t)(i) << TAG_IMMED1_SIZE) | 0x7)
#define tag_atom(i)			(((term_t)(i) << TAG_IMMED2_SIZE) | 0x3)
#define tag_catch(i)		(((term_t)(i) << TAG_IMMED2_SIZE) | 0x23)
#define tag_short_pid(i)	(((term_t)(i) << TAG_IMMED2_SIZE) | 0xb)
#define tag_short_oid(i)	(((term_t)(i) << TAG_IMMED2_SIZE) | 0x13)
#define tag_short_eid(i)	(((term_t)(i) << TAG_IMMED2_SIZE) | 0x1b)

#define int_value(t)		((int)(t) >> TAG_IMMED1_SIZE)
#define atom_index(t)		((uint32_t)(t) >> TAG_IMMED2_SIZE)
#define catch_index(t)		((uint32_t)(t) >> TAG_IMMED2_SIZE)

//TODO: rename to short_pid_rank and short_oid_rank
#define short_pid_id(t)		((uint32_t)(t) >> TAG_IMMED2_SIZE)
#define short_oid_id(t)		((uint32_t)(t) >> TAG_IMMED2_SIZE)
//#define short_ref_id(t)		((uint32_t)(t) >> TAG_IMMED2_SIZE)

// masqerading of pointers as immediate terms
#define masquerade_as_boxed(p)	(shrink_ptr(p) | PRIMARY_TAG_BOXED)
#ifdef LING_DEBUG
#define demasquerade_pointer(t)	__demasquerade_pointer((t))
#else /*!LING_DEBUG */
#define demasquerade_pointer(t)	expand_ptr((t) & ~TAG_PRIMARY_MASK)
#endif

// Cons and tuple layouts are dictated by BEAM assembler:
// 		cons cell is head:tail
//		tuple is arity(uint32_t):elem1:elem2:...:elemN

#define WSIZE(s) ((sizeof(s)+3) /4)

struct t_float_t {
	uint32_t hdr;	// subtag == 0x2
	double val;
};

#ifdef LING_DEBUG
#define box_float(p, v)		__box_float(&(p), (v))
#define float_value(p)		__float_value((p))
#else /* !LING_DEBUG */
#define box_float(p, v) do { \
	((t_float_t *)(p))->hdr = HDR_IS_NOT_CP | SUBTAG_FLOAT; \
	((t_float_t *)(p))->val = (v); \
	(p) += WSIZE(t_float_t); \
} while (0)

#define float_value(p)		(((t_float_t *)(p))->val)
#endif

// bignum_t made compatible with a boxed term value layout
typedef struct {
	uint32_t	sign;
	int32_t     alloc;  /* how many digits allocated  */
	int32_t     used;   /* how many digits used       */
	uint16_t    dp[];   /* the digits themselves      */
} bignum_t;

#define MAX_DIGITS	((1 << 20) -2)

#define  MP_ZPOS (HDR_IS_NOT_CP | 0x0)
#define  MP_NEG  (HDR_IS_NOT_CP | 0x1)

#ifdef LING_DEBUG
#define box_bignum(p, s, n, ds)		__box_bignum(&(p), (s), (n), (ds))
#define bignum_sign(p)				__bignum_sign((p))
#define bignum_arity(p)				__bignum_arity((p))
#define bignum_digits(p)			__bignum_digits((p))
#else /* !LING_DEBUG */
#define box_bignum(p, s, n, ds) do { \
	((bignum_t *)(p))->sign = (s); \
	((bignum_t *)(p))->alloc = ((n) +1) & ~1; \
	((bignum_t *)(p))->used = (n); \
	if ((ds) != 0) \
		memcpy(((bignum_t *)(p))->dp, (ds), (n)*sizeof(uint16_t)); \
	(p) += 3 + (((n)*sizeof(uint16_t)+3) /4); \
} while (0)

#define bignum_sign(p)		(((bignum_t *)(p))->sign)
#define bignum_arity(p)		(((bignum_t *)(p))->used)
#define bignum_digits(p)	(((bignum_t *)(p))->dp)
#endif

struct t_fun_t {
	uint32_t hdr;	// num_free:19,arity:8,subtag:4
	term_t pid;
	term_t module;
	uint32_t index;
	uint32_t uniq[4];
	int old_index;
	int old_uniq;
	fun_entry_t *fe;
	term_t frozen[];
};

#ifdef LING_DEBUG
#define box_fun(p, nfree, arity, pid, m, ix, uq, oix, ouq, fe, fr) \
	__box_fun(&(p), (nfree), (arity), (pid), (m), (ix), (uq), (oix), (ouq), (fe), (fr))
#define fun_arity(p)		__fun_arity((p))
#define fun_num_free(p)		__fun_num_free((p))
#else /* !LING_DEBUG */
#define box_fun(p, nfree, arity, pid__, m, ix, uq, oix, ouq, fe__, fr__) do { \
	((t_fun_t *)(p))->hdr = HDR_IS_NOT_CP | ((nfree) << 12) | ((arity) << 4) | SUBTAG_FUN; \
	((t_fun_t *)(p))->pid = (pid__); \
	((t_fun_t *)(p))->module = (m); \
	((t_fun_t *)(p))->index = (ix); \
	memcpy(((t_fun_t *)(p))->uniq, (uq), sizeof(uint32_t)*4); \
	((t_fun_t *)(p))->old_index = (oix); \
	((t_fun_t *)(p))->old_uniq = (ouq); \
	((t_fun_t *)(p))->fe = (fe__); \
	memcpy(((t_fun_t *)(p))->frozen, (fr__), sizeof(uint32_t)*(nfree)); \
	(p) += WSIZE(t_fun_t) + (nfree); \
} while (0)

#define fun_arity(p)		((((t_fun_t *)(p))->hdr >> 4) & 255)
#define fun_num_free(p)		((((t_fun_t *)(p))->hdr >> 12) & 0x7ffff)
#endif

struct t_export_t {
	uint32_t hdr;	// 0:27,tag:4
	export_t *e;
};

#ifdef LING_DEBUG
#define box_export(p, e)		__box_export(&(p), (e))
#else /* !LING_DEBUG */
#define box_export(p, e__) do { \
	((t_export_t *)(p))->hdr = HDR_IS_NOT_CP | SUBTAG_EXPORT; \
	((t_export_t *)(p))->e = (e__); \
	(p) += WSIZE(t_export_t); \
} while (0)
#endif

struct t_map_t {
	uint32_t hdr;	// size:27,tag:4
	term_t keys;	// key tuple
	term_t values[];
};

#ifdef LING_DEBUG
#define box_map(p, sz, k)		__box_map(&(p), (sz), (k))
#define map_size(p)				__map_size((uint32_t *)(p))
#else
#define box_map(p, sz, k) do { \
	((t_map_t *)(p))->hdr = HDR_IS_NOT_CP | ((sz) << 4) | SUBTAG_MAP; \
	((t_map_t *)(p))->keys = (k); \
	(p) += WSIZE(t_map_t) + (sz); \
} while (0)
#define map_size(p)				((((t_map_t *)(p))->hdr >> 4) & 0x7ffffff)
#endif // LING_DEBUG

struct t_proc_bin_t {
	uint32_t hdr;		// subtag only
	uint32_t byte_size;
	binnode_t *node;
	t_proc_bin_t *next;
	t_proc_bin_t **ref;
};

#ifdef LING_DEBUG
#define box_proc_bin(p, size, node)		__box_proc_bin(&(p), (size), (node))
#else /* !LING_DEBUG */
#define box_proc_bin(p, sz, nd) do { \
	((t_proc_bin_t *)(p))->hdr = HDR_IS_NOT_CP | SUBTAG_PROC_BIN; \
	((t_proc_bin_t *)(p))->byte_size = (sz); \
	((t_proc_bin_t *)(p))->node = (nd); \
	(p) += WSIZE(t_proc_bin_t); \
} while (0)
#endif

struct t_heap_bin_t {
	uint32_t hdr;		// subtag only
	uint32_t byte_size;
	uint8_t data[];
};

#ifdef LING_DEBUG
#define box_heap_bin(p, size, data)		__box_heap_bin(&(p), (size), (data))
#else /* !LING_DEBUG */
#define box_heap_bin(p, sz, ds) do { \
	((t_heap_bin_t *)(p))->hdr = HDR_IS_NOT_CP | SUBTAG_HEAP_BIN; \
	((t_heap_bin_t *)(p))->byte_size = (sz); \
	if ((ds) != 0) \
		memcpy(((t_heap_bin_t *)(p))->data, (ds), (sz)); \
	(p) += WSIZE(t_heap_bin_t) + ((sz) + 3) /4; \
} while (0)
#endif

struct t_sub_bin_t {
	uint32_t hdr;		// 0:26,writable:1,subtag:4
	term_t parent;
	int64_t starts;		// bit offset from parent
	int64_t ends;		// bit offset from parent
};

#ifdef LING_DEBUG
#define box_sub_bin(p, parent, from, to, writ) \
			__box_sub_bin(&(p), (parent), (from), (to), (writ))
#define sub_bin_is_writable(p)		__sub_bin_is_writable(p)
#define sub_bin_not_writable(p)		__sub_bin_not_writable(p)
#else /* !LING_DEBUG */
#define box_sub_bin(p, par, from, to, writ) do { \
	((t_sub_bin_t *)(p))->hdr = HDR_IS_NOT_CP | ((writ) << 4) | SUBTAG_SUB_BIN; \
	((t_sub_bin_t *)(p))->parent = (par); \
	((t_sub_bin_t *)(p))->starts = (from); \
	((t_sub_bin_t *)(p))->ends = (to); \
	(p) += WSIZE(t_sub_bin_t); \
} while (0)
#define sub_bin_is_writable(p)		((((t_sub_bin_t *)p)->hdr >> 4) & 1)
#define sub_bin_not_writable(p)		(((t_sub_bin_t *)p)->hdr &= ~16)
#endif

struct t_match_ctx_t {
	uint32_t hdr;	// num_slots:27,tag:4
	bits_t bs;
	term_t parent;
	int64_t saved_offsets[];
};

#ifdef LING_DEBUG
#define box_match_ctx(p, bsp, parent, nslots) \
				__box_match_ctx(&(p), (bsp), (parent), (nslots))
#define match_ctx_num_slots(p) \
				__match_ctx_num_slots(p)
#else /* !LING_DEBUG */
#define box_match_ctx(p, bsp, par, nslots) do { \
	((t_match_ctx_t *)(p))->hdr = HDR_IS_NOT_CP | ((nslots) << 4) | SUBTAG_MATCH_CTX; \
	((t_match_ctx_t *)(p))->parent = (par); \
	((t_match_ctx_t *)(p))->bs = *(bsp); \
	(p) += WSIZE(t_match_ctx_t) + (nslots) *2; \
} while (0)

#define match_ctx_num_slots(p)		((((t_match_ctx_t *)(p))->hdr >> 4) & 0x7ffffff)
#endif

struct t_long_oid_t {
	uint32_t hdr;		//0:7,creation:2,id:18,subtag:4
	term_t node;
};

#ifdef LING_DEBUG
#define box_long_oid(p, node, id, creat)	__box_long_oid(&(p), (node), (id), (creat))
#else /* !LING_DEBUG */
#define box_long_oid(p, nd, id, creat) do { \
	((t_long_oid_t *)(p))->hdr = opr_header((id), (creat), SUBTAG_OID); \
	((t_long_oid_t *)(p))->node = (nd); \
	(p) += WSIZE(t_long_oid_t); \
} while (0)
#endif

struct t_long_pid_t {
#ifdef LING_XEN
	uint32_t hdr;		//id:28,subtag:4
	uint32_t domid;
	uint64_t boxid;
#else
	uint32_t hdr;		//0:7,creation:2,id:18,subtag:4
	term_t node;
	uint32_t serial;
#endif
};

#ifdef LING_XEN
#ifdef LING_DEBUG
#define box_long_pid(p, __boxid, __domid, id) \
	__box_long_pid(&(p), (__boxid), (__domid), (id))
#else /* !LING_DEBUG */
#define box_long_pid(p, __boxid, __domid, id) do { \
	((t_long_pid_t *)(p))->hdr = ((id) << 4) | SUBTAG_PID; \
	((t_long_pid_t *)(p))->domid = (__domid); \
	((t_long_pid_t *)(p))->boxid = (__boxid); \
} while (0)
#endif
#else /* !LING_XEN */
#ifdef LING_DEBUG
#define box_long_pid(p, node, id, serial, creat) \
	__box_long_pid(&(p), (node), (id), (serial), (creat))
#else /* !LING_DEBUG */
#define box_long_pid(p, nd, id, sn, creat) do { \
	((t_long_pid_t *)(p))->hdr = opr_header((id), (creat), SUBTAG_PID); \
	((t_long_pid_t *)(p))->node = (nd); \
	((t_long_pid_t *)(p))->serial = (sn); \
	(p) += WSIZE(t_long_pid_t); \
} while (0)
#endif
#endif

#define long_pid_id(p)					(*(uint32_t *)(p) >> 4)

struct t_long_ref_t {
	uint32_t hdr;		//0:7,creation:2,id:18,subtag:4
	term_t node;
	uint32_t id1;
	uint32_t id2;
};

#ifdef LING_DEBUG
#define box_long_ref(p, node, creat, id0, id1, id2) \
	__box_long_ref(&(p), (node), (creat), (id0), (id1), (id2))
#else /* !LING_DEBUG */
#define box_long_ref(p, nd, creat, id0__, id1__, id2__) do { \
	((t_long_ref_t *)(p))->hdr = opr_header((id0__), (creat), SUBTAG_REF); \
	((t_long_ref_t *)(p))->node = (nd); \
	((t_long_ref_t *)(p))->id1 = (id1__); \
	((t_long_ref_t *)(p))->id2 = (id2__); \
	(p) += WSIZE(t_long_ref_t); \
} while (0)
#endif

#define opr_header(id, creat, tag)		(HDR_IS_NOT_CP | ((creat) << 22) | ((id) << 4) | (tag))
#define opr_hdr_id(p)					((*(uint32_t *)(p) >> 4) & 0x3ffff)
#define opr_hdr_creat(p)				((*(uint32_t *)(p) >> 22) & 0x3)

// grave may overlap with cons, tuple or boxed term data
// cons: epitaph overlaps with cons head - rip is an unused immediate term
// tuple: epitaph overlaps with size field - rip numeric value is huge
// boxed: epitaph overlaps with subtagged header - rip subtag is unused

struct t_grave_t {
	uint32_t epitaph;
	term_t body;
};

#ifdef LING_DEBUG
#define make_grave(p, body)		__make_grave((p), (body))
#else /* !LING_DEBUG */
#define make_grave(p, bd) do { \
	((t_grave_t *)(p))->epitaph = R_I_P; \
	((t_grave_t *)(p))->body = (bd); \
} while (0)
#endif

#pragma pack()
//EOF
