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

uint32_t *__peel_cons(term_t t);
uint32_t *__peel_tuple(term_t t);
uint32_t *__peel_boxed(term_t t);

term_t __tag_ptr(void *p, uint32_t tag);
uint32_t __boxed_tag(uint32_t *p);

void __box_float(uint32_t **p, double v);
double __float_value(uint32_t *p);

void __box_bignum(uint32_t **p, uint32_t sign, int arity, uint16_t *digits);
uint32_t __bignum_sign(uint32_t *p);
uint32_t __bignum_arity(uint32_t *p);
uint16_t *__bignum_digits(uint32_t *p);

void __box_fun(uint32_t **p, int nfree, int arity,
				term_t pid, term_t module, int index, uint32_t *uniq,
			   	term_t old_index, term_t old_uniq,
			   	fun_entry_t *fe, term_t *frozen);
int __fun_arity(uint32_t *p);
int __fun_num_free(uint32_t *p);

void __box_export(uint32_t **p, export_t *e);
int __export_arity(uint32_t *p);

void __box_map(uint32_t **p, uint32_t size, term_t keys);
uint32_t __map_size(uint32_t *p);

void __box_proc_bin(uint32_t **p, uint32_t size, binnode_t *node);
void __box_heap_bin(uint32_t **p, uint32_t size, uint8_t *data);

void __box_sub_bin(uint32_t **p, term_t parent, int64_t from, int64_t to, int writ);
int __sub_bin_is_writable(uint32_t *p);
void __sub_bin_not_writable(uint32_t *p);

void __box_match_ctx(uint32_t **p, bits_t *bsp, term_t parent, int nslots);
int __match_ctx_num_slots(uint32_t *p);

void __box_long_oid(uint32_t **p, term_t node, uint32_t id, int creat);
#ifdef LING_XEN
void __box_long_pid(uint32_t **p, uint64_t boxid, uint32_t domid, uint32_t id);
#else /* !LING_XEN */
void __box_long_pid(uint32_t **p, term_t node,
	   uint32_t id, uint32_t serial, int creat);
#endif
void __box_long_ref(uint32_t **p, term_t node,
	   int creat, uint32_t id0, uint32_t id1, uint32_t id2);

void __make_grave(uint32_t *p, term_t body);

uint32_t *__demasquerade_pointer(term_t t);

//EOF
