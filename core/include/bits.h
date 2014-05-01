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

#include "heap.h"

//#define MAX_HEAP_BIN	512
#define MAX_HEAP_BIN	64

#define MAX_BYTE_SIZE	((1u << 29) -1)
#define MAX_BIT_SIZE	(MAX_BYTE_SIZE << 3)

#define BSF_LITTLE	2
#define BSF_SIGNED	4

#define BIT_MASK(nb)	((1 << (nb)) -1)

#define NEGATE_8(d, c) do { \
	uint32_t v = (uint32_t)((uint8_t)~(d)) + (c); \
	(d) = (uint8_t)v; \
	(c) = v >> 8; \
} while (0)

#define NEGATE_16(d, c) do { \
	uint32_t v = (uint32_t)((uint16_t)~(d)) + (c); \
	(d) = (uint16_t)v; \
	(c) = v >> 16; \
} while (0)

#define bits_has_octet(bs)	((bs)->ends - (bs)->starts >= 8)
#define bits_has_word(bs)	((bs)->ends - (bs)->starts >= 32)

#define bits_data_with_padding(bs, off, data_size, padding)	\
		((((off) >= 0) && ((off) < (data_size))) ?(bs)->data[(off)] :(padding))

#define bits_get_octet_with_padding(bs, oct, data_size, padding) do { \
	int32_t off__ = ((bs)->starts < 0) ?((bs)->starts -7) /8 :(bs)->starts /8; \
	if (((bs)->starts & 7) == 0) \
		(oct) = bits_data_with_padding(bs, off__, data_size, padding); \
	else \
	{ \
		int bo__ = (bs)->starts & 7; \
		uint8_t high__ = bits_data_with_padding(bs, off__, data_size, padding); \
		uint8_t low__ = bits_data_with_padding(bs, off__+1, data_size, padding); \
		(oct) = (high__ << bo__) | (low__ >> (8-bo__)); \
	} \
	(bs)->starts += 8; \
} while (0)

#define bits_get_octet(bs, oct) do { \
	int32_t off__ = (bs)->starts /8; \
	if (((bs)->starts & 7) == 0) \
		(oct) = (bs)->data[off__]; \
	else \
	{ \
		int bo__ = (bs)->starts & 7; \
		uint8_t high__ = (bs)->data[off__]; \
		uint8_t low__ = (bs)->data[off__+1]; \
		(oct) = (uint8_t)((high__ << bo__) | (low__ >> (8 -bo__))); \
	} \
	(bs)->starts += 8; \
} while (0)

#define bits_get_word(bs, w) do { \
	int32_t off__ = (bs)->starts /8; \
	if (((bs)->starts & 7) == 0) \
	{ \
		uint8_t *p__ = (bs)->data + off__; \
		(w) = GET_UINT_32(p__); \
	} \
	else \
	{ \
		uint64_t xyzp = (((uint64_t)(bs)->data[off__]) << 32) | \
				(((uint64_t)(bs)->data[off__ +1]) << 24) | \
				(((uint64_t)(bs)->data[off__ +2]) << 16) | \
				(((uint64_t)(bs)->data[off__ +3]) << 8) | \
				((uint64_t)(bs)->data[off__ +4]); \
		int bo__ = (bs)->starts & 7; \
		(w) = xyzp >> (8 -bo__); \
	} \
	(bs)->starts += 32; \
} while (0)

#define bits_put_octet(bs, oct) do { \
	int32_t off__ = (bs)->starts /8; \
	assert(off__ >= 0); \
	if (((bs)->starts & 7) == 0) \
		(bs)->data[off__] = (oct); \
	else \
	{ \
		int bo__ = (bs)->starts & 7; \
		uint8_t mask__ = BIT_MASK(8-bo__); \
		(bs)->data[off__] = ((bs)->data[off__] & ~mask__) | ((oct) >> bo__); \
		(bs)->data[off__+1] = ((bs)->data[off__+1] & mask__) | ((oct) << (8-bo__)); \
	} \
	(bs)->starts += 8; \
} while (0)

#define bits_put_word(bs, w) do { \
	int32_t off__ = (bs)->starts /8; \
	assert(off__ >= 0); \
	uint8_t *p__ = (bs)->data + off__; \
	if (((bs)->starts & 7) == 0) \
		PUT_UINT_32(p__, (w)); \
	else \
	{ \
		int bo__ = (bs)->starts & 7; \
		uint64_t xyzp = ((uint64_t)(w)) << (8 -bo__); \
		uint8_t mask1__ = BIT_MASK(8 -bo__); \
		p__[0] = (p__[0] & ~mask1__) | (xyzp >> 32); \
		p__[1] = (xyzp >> 24) & 255; \
		p__[2] = (xyzp >> 16) & 255; \
		p__[3] = (xyzp >> 8) & 255; \
		uint8_t mask2__ = BIT_MASK(bo__) << (8 -bo__); \
		p__[4] = (p__[4] & ~mask2__) | (xyzp & 255); \
	} \
	(bs)->starts += 32; \
} while (0)

#define bits_is_simple(bs) 	((bs)->starts == 0 && ((bs)->ends & 7) == 0)

extern uint32_t total_binary_size;

void bits_get_real(void *pbin, bits_t *bs);
void bits_init_buf(uint8_t *data, uint32_t data_size, bits_t *bs);

void bits_copy(bits_t *from, bits_t *to);
void bits_fill(bits_t *from, bits_t *to);
void bits_copy_with_padding(bits_t *from, bits_t *to, uint32_t data_size, uint8_t padding);

int bits_compare(bits_t *a, bits_t *b);

uint8_t bits_get_trailer_with_padding(bits_t *bs, int data_size, uint8_t padding);
uint8_t bits_get_trailer(bits_t *bs);
void bits_put_trailer(bits_t *bs, uint8_t trailer, int bcount);

void bits_put_integer(bits_t *bs, int v, uint32_t bcount, int is_le);
void bits_put_bignum(bits_t *bs, bignum_t *bn, uint32_t bcount, int is_le);

term_t bits_bs_get_float(t_match_ctx_t *mc, uint32_t bcount, int is_little, heap_t *hp);

binnode_t *binnode_make(uint32_t size);
binnode_t *binnode_make_N(uint32_t size);
void binnode_destroy(binnode_t *bnode);
void proc_bin_link(t_proc_bin_t **pbs, t_proc_bin_t *pb, int *pb_size);
void proc_bin_unlink(t_proc_bin_t *pb, int *pb_size);

int bits_calc_byte_size(term_t size, uint32_t *ocount);
int bits_calc_bit_size(term_t size, uint8_t unit, uint32_t *bcount);

term_t bits_bs_init_writable(int size, heap_t *hp);
term_t bits_bs_start_match2(term_t bin, int num_slots, heap_t *hp);
term_t bits_bs_append(term_t bin,
			term_t sz, uint8_t unit, int extra, bits_t *bpc, heap_t *hp);
int bits_bs_private_append(term_t bin,
			term_t sz, uint8_t unit, bits_t *bpc, heap_t *hp);

int bits_bs_put_float(term_t v, uint32_t bcount, int is_little, bits_t *bpc);

term_t bits_bs_get_integer_imm(t_match_ctx_t *mc,
		uint32_t bcount, int is_signed, int is_little, heap_t *hp);

//EOF
