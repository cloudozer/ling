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

/**
 * Debugging instrumentation
 */

#ifdef LING_DEBUG

#include <stdint.h>
#include <math.h>

#include "ling_common.h"

#include "term.h"
#include "string.h"
#include "bits.h"

#define ALIGNED_PTR(p) do { \
	if (((unsigned long)(p) & TAG_PRIMARY_MASK) != 0) \
		fatal_error("unaligned ptr"); \
	} while (0)

#define VALID_SUBTAG(tag) do { \
	if ((tag) == 0x3 || (tag) == 0x4 || (tag) == 0x5) \
		fatal_error("bad subtag"); \
	} while (0)


uint32_t *__expand_ptr(uint32_t n)
{
#if defined(__x86_64__)
	assert(n != UNUSED_MEM_SIGN); 
	return (uint32_t *)((uint64_t)(n) | PTR_MASK);
#else /*__x86_64__*/
	return (uint32_t *)(n);
#endif
}

uint32_t __shrink_ptr(void *p)
{
#if defined(__x86_64__)
	assert(((uint64_t)(p) & __INT64_C(0xffffffff00000000)) == PTR_MASK);
	return (uint32_t)(uint64_t)(p);
#else /*__x86_64__*/
	return (uint32_t)(p);
#endif
}

uint32_t *__peel_cons(term_t t)
{
	if ((t & TAG_PRIMARY_MASK) != PRIMARY_TAG_CONS)
		fatal_error("not a cons");
	uint32_t *p = (uint32_t *)(unsigned long)(t - PRIMARY_TAG_CONS);
	assert(p != 0);
	return p;
}

uint32_t *__peel_tuple(term_t t)
{
	if ((t & TAG_PRIMARY_MASK) != PRIMARY_TAG_TUPLE)
		fatal_error("not a tuple");
	uint32_t *p = (uint32_t *)(unsigned long)(t - PRIMARY_TAG_TUPLE);
	assert(p != 0);
	if (*p == 0)
		assert(p == &zero);	// ZERO_TUPLE
	return p;
}

uint32_t *__peel_boxed(term_t t)
{
	if ((t & TAG_PRIMARY_MASK) != PRIMARY_TAG_BOXED)
		fatal_error("not a boxed");
	uint32_t *p = (uint32_t *)(unsigned long)(t - PRIMARY_TAG_BOXED);
	assert(p != 0);
	return p;
}

term_t __tag_ptr(void *p, uint32_t tag)
{
	assert(p != 0);
	ALIGNED_PTR(p);
	return (__shrink_ptr(p) | tag);
}

uint32_t __boxed_tag(uint32_t *p)
{
	assert(p != 0);
	ALIGNED_PTR(p);
#ifdef DEBUG_UNUSED_MEM
	assert(*p != UNUSED_MEM_SIGN); 
#endif
	uint32_t tag = *p & SUBTAG_MASK;
	VALID_SUBTAG(tag);
	return tag;
}

void __box_float(uint32_t **p, double v)
{
	assert(*p != 0);
	ALIGNED_PTR(*p);
	assert(v == v);		// !isnan
	((t_float_t *)(*p))->hdr = HDR_IS_NOT_CP | SUBTAG_FLOAT;
	((t_float_t *)(*p))->val = v;
	(*p) += WSIZE(t_float_t);
}

double __float_value(uint32_t *p)
{
	assert(p != 0);
	ALIGNED_PTR(p);
	assert(boxed_tag(p) == SUBTAG_FLOAT);
	return ((t_float_t *)p)->val;
}

void __box_bignum(uint32_t **p, uint32_t sign, int arity, uint16_t *digits)
{
	assert(*p != 0);
	ALIGNED_PTR(*p);
	assert(sign == MP_ZPOS || sign == MP_NEG);
	assert(arity > 0);

	// digits are always allocated in pairs
	uint32_t alloc = (arity +1) & ~1;

	((bignum_t *)(*p))->sign = sign;	// acts as a subtag
	((bignum_t *)(*p))->alloc = alloc;
	((bignum_t *)(*p))->used = arity;
	if (digits != 0)
		memcpy(((bignum_t *)(*p))->dp, digits, arity*sizeof(uint16_t));
	(*p) += WSIZE(bignum_t) + alloc /2;
}

uint32_t __bignum_sign(uint32_t *p)
{
	assert(p != 0);
	ALIGNED_PTR(p);
	assert(is_bignum(p));
	return ((bignum_t *)p)->sign;
}

uint32_t __bignum_arity(uint32_t *p)
{
	assert(p != 0);
	ALIGNED_PTR(p);
	assert(is_bignum(p));
	return ((bignum_t *)p)->used;
}

uint16_t *__bignum_digits(uint32_t *p)
{
	assert(p != 0);
	ALIGNED_PTR(p);
	assert(is_bignum(p));
	return ((bignum_t *)p)->dp;
}

void __box_fun(uint32_t **p, int nfree, int arity,
				term_t pid, term_t module, int index, uint32_t *uniq,
			   	term_t old_index, term_t old_uniq,
			   	fun_entry_t *fe, term_t *frozen)
{
	assert(*p != 0);
	ALIGNED_PTR(*p);

	assert(nfree >= 0);
	assert(arity <= 255 && arity >= 0);
	//assert(is_short_pid(pid));
	assert(is_atom(module));
	assert(index >= 0);
	assert(uniq != 0);
	assert(frozen != 0);

	((t_fun_t *)(*p))->hdr = HDR_IS_NOT_CP | (nfree << 12) | (arity << 4) | SUBTAG_FUN;
	((t_fun_t *)(*p))->pid = pid;
	((t_fun_t *)(*p))->module = module;
	((t_fun_t *)(*p))->index = index;
	memcpy(((t_fun_t *)(*p))->uniq, uniq, 4*4);
	((t_fun_t *)(*p))->old_index = old_index;
	((t_fun_t *)(*p))->old_uniq = old_uniq;
	((t_fun_t *)(*p))->fe = fe;
	memcpy(((t_fun_t *)(*p))->frozen, frozen, 4*nfree);
	(*p) += WSIZE(t_fun_t) + nfree;
}

int __fun_arity(uint32_t *p)
{
	assert(p != 0);
	ALIGNED_PTR(p);
	assert(boxed_tag(p) == SUBTAG_FUN);
	return (((t_fun_t *)p)->hdr >> 4) & 255;
}

int __fun_num_free(uint32_t *p)
{
	assert(p != 0);
	ALIGNED_PTR(p);
	assert(boxed_tag(p) == SUBTAG_FUN);
	return (((t_fun_t *)p)->hdr >> 12) & 0x7ffff;
}

void __box_export(uint32_t **p, export_t *e)
{
	assert(*p != 0);
	ALIGNED_PTR(*p);
	assert(e != 0);

	((t_export_t *)(*p))->hdr = HDR_IS_NOT_CP | SUBTAG_EXPORT;
	((t_export_t *)(*p))->e = e;
	(*p) += WSIZE(t_export_t);
}

void __box_proc_bin(uint32_t **p, uint32_t size, binnode_t *node)
{
	assert(*p != 0);
	ALIGNED_PTR(*p);

	assert(node != 0);
	assert(node->ends - node->starts >= size);
	assert(size <= MAX_BYTE_SIZE);

	((t_proc_bin_t *)(*p))->hdr = HDR_IS_NOT_CP | SUBTAG_PROC_BIN;
	((t_proc_bin_t *)(*p))->byte_size = size;
	((t_proc_bin_t *)(*p))->node = node;
	(*p) += WSIZE(t_proc_bin_t);
}

void __box_heap_bin(uint32_t **p, uint32_t size, uint8_t *data)
{
	assert(*p != 0);
	ALIGNED_PTR(*p);

	//assert(data != 0);
	assert(size <= MAX_HEAP_BIN);

	((t_heap_bin_t *)(*p))-> hdr = HDR_IS_NOT_CP | SUBTAG_HEAP_BIN;
	((t_heap_bin_t *)(*p))->byte_size = size;
	if (data != 0)
		memcpy(((t_heap_bin_t *)(*p))->data, data, size);
	(*p) += WSIZE(t_heap_bin_t) + (size+3) /4;
}

void __box_sub_bin(uint32_t **p, term_t parent, int64_t from, int64_t to, int writ)
{
	assert(*p != 0);
	ALIGNED_PTR(*p);

	assert(writ == 0 || writ == 1);
	assert(from >= 0);
	assert(to >= from);
	assert(is_boxed(parent));
	if (*peel_boxed(parent) != R_I_P)
	{
		uint32_t tag = boxed_tag(peel_boxed(parent));
		assert(tag == SUBTAG_PROC_BIN || tag == SUBTAG_HEAP_BIN);
	}

	((t_sub_bin_t *)(*p))->hdr = HDR_IS_NOT_CP | (writ << 4) | SUBTAG_SUB_BIN;
	((t_sub_bin_t *)(*p))->parent = parent;
	((t_sub_bin_t *)(*p))->starts = from;
	((t_sub_bin_t *)(*p))->ends = to;
	(*p) += WSIZE(t_sub_bin_t);
}

int __sub_bin_is_writable(uint32_t *p)
{
	assert(p != 0);
	ALIGNED_PTR(p);
	assert(boxed_tag(p) == SUBTAG_SUB_BIN);
	return (*p & 16) != 0;
}

void __sub_bin_not_writable(uint32_t *p)
{
	assert(p != 0);
	ALIGNED_PTR(p);
	assert(boxed_tag(p) == SUBTAG_SUB_BIN);
	*p &= ~16;
}

void __box_match_ctx(uint32_t **p, bits_t *bsp, term_t parent, int nslots)
{
	assert(*p != 0);
	ALIGNED_PTR(*p);

	assert(is_boxed(parent));
	if (*peel_boxed(parent) != R_I_P)
	{
		uint32_t tag = boxed_tag(peel_boxed(parent));
		assert(tag == SUBTAG_PROC_BIN || tag == SUBTAG_HEAP_BIN);
	}
	assert(nslots > 0);

	((t_match_ctx_t *)(*p))->hdr = HDR_IS_NOT_CP | (nslots << 4) | SUBTAG_MATCH_CTX;
	((t_match_ctx_t *)(*p))->bs = *bsp;
	((t_match_ctx_t *)(*p))->parent = parent;
	(*p) += WSIZE(t_match_ctx_t) + nslots *2;	// offsets are int64_t
}

int __match_ctx_num_slots(uint32_t *p)
{
	assert(p != 0);
	ALIGNED_PTR(p);
	assert(boxed_tag(p) == SUBTAG_MATCH_CTX);
	return (((t_match_ctx_t *)p)->hdr >> 4) & 0x7ffffff;
}

void __box_long_oid(uint32_t **p, term_t node, uint32_t id, int creat)
{
	assert(*p != 0);
	ALIGNED_PTR(*p);

	assert(is_atom(node));
	assert((id & ~0x3ffff) == 0);
	assert(creat >= 0 && creat <= 3);

	((t_long_oid_t *)(*p))->hdr = opr_header(id, creat, SUBTAG_OID);
	((t_long_oid_t *)(*p))->node = node;
	(*p) += WSIZE(t_long_oid_t);
}

void __box_long_pid(uint32_t **p, term_t node,
	   uint32_t id, uint32_t serial, int creat)
{
	assert(*p != 0);
	ALIGNED_PTR(*p);

	assert(is_atom(node));
	assert((id & ~0x7fff) == 0);
	assert(creat >= 0 && creat <= 3);

	((t_long_pid_t *)(*p))->hdr = opr_header(id, creat, SUBTAG_PID);
	((t_long_pid_t *)(*p))->node = node;
	((t_long_pid_t *)(*p))->serial = serial;
	(*p) += WSIZE(t_long_pid_t);
}

void __box_long_ref(uint32_t **p, term_t node,
	   int creat, uint32_t id0, uint32_t id1, uint32_t id2)
{
	assert(*p != 0);
	ALIGNED_PTR(*p);

	assert(is_atom(node));
	assert(creat >= 0 && creat <= 3);
	assert((id0 & ~0x3ffff) == 0);

	((t_long_ref_t *)(*p))->hdr = opr_header(id0, creat, SUBTAG_REF);
	((t_long_ref_t *)(*p))->node = node;
	((t_long_ref_t *)(*p))->id1 = id1;
	((t_long_ref_t *)(*p))->id2 = id2;
	(*p) += WSIZE(t_long_ref_t);
}

void __make_grave(uint32_t *p, term_t body)
{
	assert(p != 0);
	ALIGNED_PTR(p);

	assert(!is_immed(body));

	((t_grave_t *)p)->epitaph = R_I_P;
	((t_grave_t *)p)->body = body;
	// p not moved
}

uint32_t *__demasquerade_pointer(term_t t)
{
	assert(is_boxed(t));
	uint32_t *p = peel_boxed(t);
	assert(is_cp(p));
	return p;
}

#endif /* LING_DEBUG */

//EOF
