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

#include "bits.h"

#include <math.h>

#include "ling_common.h"

#include "term.h"
#include "term_util.h"
#include "atom_defs.h"
#include "limits.h"
#include "nalloc.h"
#include "string.h"
#include "getput.h"
#include "bignum.h"

#define BS_APPEND_SLACK		256

void bits_get_real(void *pbin, bits_t *bs)
{
	switch (boxed_tag(pbin))
	{
	case SUBTAG_PROC_BIN:
	{
		t_proc_bin_t *pb = pbin;
		bs->data = pb->node->starts;
		bs->starts = 0;
		bs->ends = pb->byte_size *8;
		break;
	}
	case SUBTAG_HEAP_BIN:
	{
		t_heap_bin_t *hb = pbin;
		bs->data = hb->data;
		bs->starts = 0;
		bs->ends = hb->byte_size *8;
		break;
	}
	case SUBTAG_MATCH_CTX:
	{
		t_match_ctx_t *mc = pbin;
		*bs = mc->bs;
		break;
	}
	default:
		assert(boxed_tag(pbin) == SUBTAG_SUB_BIN);
		t_sub_bin_t *sb = pbin;
		bits_get_real(peel_boxed(sb->parent), bs);
		bs->starts = sb->starts;
		bs->ends = sb->ends;
		break;
	}
}

void bits_init_buf(uint8_t *data, uint32_t data_size, bits_t *bs)
{
	bs->data = data;
	bs->starts = 0;
	bs->ends = data_size *8;
}

// copies all bits from src to dst
void bits_copy(bits_t *from, bits_t *to)
{
	assert(from->ends-from->starts <= to->ends-to->starts);

	if (from->starts >= 0 && to->starts >= 0 &&
		(from->starts & 7) == 0 && (from->ends & 7) == 0 &&
		(to->starts & 7) == 0 && (to->ends & 7) == 0)
	{
		uint8_t *src = from->data + (from->starts /8);
		uint8_t *dst = to->data + (to->starts /8);
		int len = (from->ends - from->starts) /8;

		// easy memcpy() case
		memcpy(dst, src, len);
		from->starts += len *8;
		to->starts += len *8;
		return;
	}

	// copy one word at a time
	while (bits_has_word(from))
	{
		uint32_t w;
		bits_get_word(from, w);
		bits_put_word(to, w);
	}

	// copy one byte at a time
	while (bits_has_octet(from))
	{
		uint8_t octet;
		bits_get_octet(from, octet);
		bits_put_octet(to, octet);
	}

	// any trailer?
	if (from->ends > from->starts)
	{
		int bcount = from->ends - from->starts;
		uint8_t trailer = bits_get_trailer(from);
		bits_put_trailer(to, trailer, bcount);
	}

	assert(from->ends == from->starts);
}

// copies enough bits from src to fill dst
void bits_fill(bits_t *from, bits_t *to)
{
	int64_t saved_ends = from->ends;
	from->ends = from->starts + (to->ends - to->starts);
	bits_copy(from, to);
	from->ends = saved_ends;
}

void bits_copy_with_padding(bits_t *from, bits_t *to, uint32_t data_size, uint8_t padding)
{
	assert(from->ends-from->starts <= to->ends-to->starts);

	if (from->starts >= 0 && to->starts >= 0 &&
		(from->starts & 7) == 0 && (from->ends & 7) == 0 &&
		(to->starts & 7) == 0 && (to->ends & 7) == 0)
	{
		uint8_t *src = from->data + (from->starts /8);
		uint8_t *dst = to->data + (to->starts /8);
		int len = (from->ends - from->starts) /8;

		if (src + len <= from->data + data_size)
		{
			// easy memcpy() case
			memcpy(dst, src, len);
			from->starts += len *8;
			to->starts += len *8;
			return;
		}
	}

	// copy one byte at a time
	while (bits_has_octet(from))
	{
		uint8_t octet;
		bits_get_octet_with_padding(from, octet, data_size, padding);
		bits_put_octet(to, octet);
	}

	if (from->ends > from->starts)
	{
		int bcount = from->ends - from->starts;
		uint8_t trailer = bits_get_trailer_with_padding(from, data_size, padding);
		bits_put_trailer(to, trailer, bcount);
	}

	assert(from->starts == from->ends);
}

int bits_compare(bits_t *a, bits_t *b)
{
	if (a->starts >= 0 && b->starts >= 0 &&
		(a->starts & 7) == 0 && (a->ends & 7) == 0 &&
		(b->starts & 7) == 0 && (b->ends & 7) == 0)
	{
		// easy memcmp() case
		uint8_t *adata = a->data + (a->starts /8);
		uint8_t *bdata = b->data + (b->starts /8);
		uint32_t alen = (a->ends - a->starts) /8;
		uint32_t blen = (b->ends - b->starts) /8;

		uint32_t prefix_len = (alen > blen) ?blen :alen;

		int d = memcmp(adata, bdata, prefix_len);
		if (d < 0)
			return -1;
		if (d > 0)
		return 1;

		if (alen < blen)
			return -1;
		if (alen > blen)
			return 1;

		return 0;
	}

	// tough case - one word at a time
	while (bits_has_word(a) && bits_has_word(b))
	{
		uint32_t wa, wb;
		bits_get_word(a, wa);
		bits_get_word(b, wb);
		if (wa < wb)
			return -1;
		if (wa > wb)
			return 1;
	}

	// tough case - one byte at a time
	while (bits_has_octet(a) && bits_has_octet(b))
	{
		uint8_t octa, octb;
		bits_get_octet(a, octa);
		bits_get_octet(b, octb);
		if (octa < octb)
			return -1;
		if (octa > octb)
			return 1;
	}

	int64_t lefta = a->ends - a->starts;
	int64_t leftb = b->ends - b->starts;

	if (lefta > 0 && leftb > 0)
	{
		if (lefta > leftb)
			a->ends = a->starts +leftb;
		else
			b->ends = b->starts +lefta;

		uint8_t traila = bits_get_trailer(a);
		uint8_t trailb = bits_get_trailer(b);

		if (traila < trailb)
			return -1;
		if (traila > trailb)
			return 1;
	}

	if (lefta < leftb)
		return -1;
	if (lefta > leftb)
		return 1;

	return 0;
}

uint8_t bits_get_trailer(bits_t *bs)
{
	assert(bs->ends - bs->starts > 0);
	assert(bs->ends - bs->starts < 8);

	int bos = bs->starts & 7;
	int boe = bs->ends & 7;

	int32_t offs = bs->starts /8;
	int32_t offe = bs->ends /8;

	uint8_t trailer;
	if (offs == offe)
	{
		// xxaaayyy
		// x: bos
		// y: 8-boe
		// a: boe-bos

		uint8_t o = bs->data[offs];
		trailer = (o >> (8-boe)) & BIT_MASK(boe-bos);
	}
	else
	{
		assert(offe == offs+1);

		// xxxxxaaa aaaayyyy
		// x: bos
		// y: 8-boe

		uint8_t o1 = bs->data[offs];
		uint8_t o2 = bs->data[offe];
		trailer = ((o1 & BIT_MASK(8 -bos)) << boe) |
			      (o2 >> (8 -boe));
	}

	bs->starts = bs->ends;
	return trailer;
}

uint8_t bits_get_trailer_with_padding(bits_t *bs, int data_size, uint8_t padding)
{
	assert(bs->ends - bs->starts > 0);
	assert(bs->ends - bs->starts < 8);

	int bos = bs->starts & 7;
	int boe = bs->ends & 7;

	int32_t offs = (bs->starts < 0) ?(bs->starts -7) /8 :bs->starts /8;
	int32_t offe = (bs->ends < 0) ?(bs->ends -7) /8 :bs->ends /8;

	uint8_t trailer;
	if (offs == offe)
	{
		// xxaaayyy
		// x: bos
		// y: 8-boe
		// a: boe-bos

		uint8_t o = bits_data_with_padding(bs, offs, data_size, padding);
		trailer = (o >> (8-boe)) & BIT_MASK(boe-bos);
	}
	else
	{
		assert(offe == offs+1);

		// xxxxxaaa aaaayyyy
		// x: bos
		// y: 8-boe

		uint8_t o1 = bits_data_with_padding(bs, offs, data_size, padding);
		uint8_t o2 = bits_data_with_padding(bs, offe, data_size, padding);
		trailer = ((o1 & BIT_MASK(8 -bos)) << boe) |
			      (o2 >> (8 -boe));
	}

	bs->starts = bs->ends;
	return trailer;
}

void bits_put_trailer(bits_t *bs, uint8_t trailer, int bcount)
{
	assert(bcount > 0);
	assert(bcount < 8);
	assert(bs->ends-bs->starts >= bcount);

	int off = bs->starts /8;
	assert(off >= 0);
	int bo = bs->starts & 7;

	if (bo + bcount <= 8)
	{
		// 000ttttt
		// xxttttty
		// x: bo
		// t: bcount
		// y: 8 -bo -bcount
	
		uint8_t mask = BIT_MASK(bcount) << (8 -bo -bcount);
		bs->data[off] &= ~mask;
		bs->data[off] |= trailer << (8 -bo -bcount);
	}
	else
	{
		// 000ttttt
		// xxxxxttt ttyyyyyy
		// x: bo
		// t1: 8-bo
		// t2: bcount +bo -8
		// y: 16 -bcount -bo

		uint8_t mask1 = BIT_MASK(8 -bo);
		bs->data[off] &= ~mask1;
		bs->data[off] |= trailer >> (bcount +bo -8);
		uint8_t mask2 = BIT_MASK(bcount +bo -8) << (16 -bcount -bo);
		bs->data[off+1] &= ~mask2;
		bs->data[off+1] |= trailer << (16 -bcount -bo);
	}
	
	bs->starts += bcount;
}

void bits_put_integer(bits_t *bs, int v, uint32_t bcount, int is_le)
{
	uint8_t quad[4];
	PUT_UINT_32(quad, v);
	uint8_t padding = (v >= 0) ?0 :255;
	bits_t src = {
		.data = quad,
	};

	if (is_le)
	{
		// little-endian
		int64_t ends = 32;
		while (bcount >= 8)
		{
			src.starts = ends -8;
			src.ends = ends;
			bits_copy_with_padding(&src, bs, 4, padding);
			ends -= 8;
			bcount -= 8;
		}

		if (bcount > 0)
		{
			src.starts = ends -bcount;
			src.ends = ends;
			bits_copy_with_padding(&src, bs, 4, padding);
		}
	}
	else
	{
		// big-endian
		src.starts = (int64_t)32 -bcount;
		src.ends = 32;
		bits_copy_with_padding(&src, bs, 4, padding);
	}
}

void bits_put_bignum(bits_t *bs, bignum_t *bn, uint32_t bcount, int is_le)
{
	int arr_size = bn->used*2;
	uint8_t arr[arr_size];
	bignum_to_bytes(bn, arr, arr_size);

	bits_t src = {
		.data = arr,
	};
	int padding = (bn->sign == MP_NEG) ?255 :0;
	if (is_le)
	{
		int64_t ends = arr_size *8;
		while (bcount >= 8)
		{
			src.starts = ends -8;
			src.ends = ends;
			bits_copy_with_padding(&src, bs, arr_size, padding);
			ends -= 8;
			bcount -= 8;
		}

		if (bcount > 0)
		{
			src.starts = ends -bcount;
			src.ends = ends;
			bits_copy_with_padding(&src, bs, arr_size, padding);
		}
	}
	else
	{
		src.starts = (int64_t)arr_size*8 - bcount;
		src.ends = arr_size*8;
		bits_copy_with_padding(&src, bs, arr_size, padding);
	}
}

//-----------------------------------------------------------------------------

//#define BINNODE_TATS
#ifdef BINNODE_STATS
#include "time.h"

uint64_t bn_ts_ns;
int bn_nr_pages_in;
int bn_nr_nodes_in;
int bn_nr_pages_out;
int bn_nr_nodes_out;

static void dump_binnode_stats(void)
{
	uint64_t now = monotonic_clock();
	uint64_t elapsed_ns = now - bn_ts_ns;
	double page_in_rate = (double) bn_nr_pages_in * 1e9 / elapsed_ns;
	double node_in_rate = (double) bn_nr_nodes_in * 1e9 / elapsed_ns;
	double page_out_rate = (double) bn_nr_pages_out * 1e9 / elapsed_ns;
	double node_out_rate = (double) bn_nr_nodes_out * 1e9 / elapsed_ns;

	printk("binnode: ts %lu ms: IN: %.1f nd/s %.1f pg/s OUT: %.1f nd/s %.1f pg/s\n",
			now / 1000 / 1000, node_in_rate, page_in_rate, node_out_rate, page_out_rate);

	bn_ts_ns = now;
	bn_nr_nodes_in = 0;
	bn_nr_pages_in = 0;
	bn_nr_nodes_out = 0;
	bn_nr_pages_out = 0;
}
#endif

uint32_t total_binary_size = 0;

binnode_t *binnode_make(uint32_t size)
{
#ifdef BINNODE_STATS
	bn_nr_pages_out += (size + PAGE_SIZE -1) / PAGE_SIZE;
	if (++bn_nr_nodes_out >= 20)
		dump_binnode_stats();
#endif

	// binnode_t is the extended version of memnode_t; its size by coincidence
	// is the same
	assert(sizeof(binnode_t) == sizeof(memnode_t));

	// nalloc() does not dig zero size
	uint32_t size1 = (size == 0) ?1 :size;

	memnode_t *nd = nalloc(size1);
	binnode_t *bnode = (binnode_t *)nd;
	bnode->refc = 0;

	total_binary_size += bnode->index *PAGE_SIZE;
	return bnode;
}

binnode_t *binnode_make_N(uint32_t size)
{
#ifdef BINNODE_STATS
	bn_nr_pages_out += (size + PAGE_SIZE -1) / PAGE_SIZE;
	if (++bn_nr_nodes_out >= 20)
		dump_binnode_stats();
#endif

	// binnode_t is the extended version of memnode_t; its size by coincidence
	// is the same
	assert(sizeof(binnode_t) == sizeof(memnode_t));
	memnode_t *nd = nalloc_N(size);
	if (nd == 0)
		return 0;
	binnode_t *bnode = (binnode_t *)nd;
	bnode->refc = 0;

	total_binary_size += bnode->index *PAGE_SIZE;
	return bnode;
}

void binnode_destroy(binnode_t *bnode)
{
#ifdef BINNODE_STATS
	bn_nr_pages_in += bnode->index;
	bn_nr_nodes_in++;
#endif

	total_binary_size -= bnode->index *PAGE_SIZE;
	nfree((memnode_t *)bnode);
}

void proc_bin_link(t_proc_bin_t **pbs, t_proc_bin_t *pb, int *pb_size)
{
	pb->next = *pbs;
	if ((*pbs) != 0)
		(*pbs)->ref = &pb->next;
	*pbs = pb;
	pb->ref = pbs;

	pb->node->refc++;

	if (pb_size != 0)
		*pb_size += pb->node->index *PAGE_SIZE;
}

void proc_bin_unlink(t_proc_bin_t *pb, int *pb_size)
{
	if (pb_size != 0)
		*pb_size -= pb->node->index *PAGE_SIZE;

	assert(*pb->ref == pb);
	*pb->ref = pb->next;
	if (pb->next != 0)
		pb->next->ref = pb->ref;

	//debug("unlink: node=0x%pp refc=%d\n", pb->node, pb->node->refc);
	pb->node->refc--;
	if (pb->node->refc == 0)
		binnode_destroy(pb->node);
	pb->node = 0;
}

//-----------------------------------------------------------------------------

int bits_calc_byte_size(term_t size, uint32_t *ocount)
{
	int osz;
	if (is_int(size))
	{
		osz = int_value(size);
		if (osz < 0)
			return -BAD_ARG;
		if (osz > MAX_BYTE_SIZE)
			return -TOO_LONG;
	}
	else if (is_boxed(size) && is_bignum(peel_boxed(size)))
	{
		bignum_t *bn = (bignum_t *)peel_boxed(size);
		if (bn->sign == MP_NEG)
			return -BAD_ARG;
		if (bn->used > 2)
			return -TOO_LONG;
		osz = bignum_to_int(bn);
		if (osz > MAX_BYTE_SIZE)
			return -TOO_LONG;
	}
	else
		return -BAD_ARG;

	*ocount = (uint32_t)osz;
	return 0;
}

int bits_calc_bit_size(term_t size, uint8_t unit, uint32_t *bcount)
{
	int64_t bsz;
	if (is_int(size))
	{
		bsz = (int64_t)int_value(size) *unit;
		if (bsz < 0)
		   return -BAD_ARG;
		if (bsz > MAX_BIT_SIZE)
			return -TOO_LONG;
	}
	else if (is_boxed(size) && is_bignum(peel_boxed(size)))
	{
		bignum_t *bn = (bignum_t *)peel_boxed(size);
		if (bn->sign == MP_NEG)
			return -BAD_ARG;
		if (bn->used > 2)
			return -TOO_LONG;
		bsz = bignum_to_int(bn) *unit;
		if (bsz > MAX_BIT_SIZE)
			return -TOO_LONG;
	}
	else
		return -BAD_ARG;

	*bcount = (uint32_t)bsz;
	return 0;
}

term_t bits_bs_init_writable(int size, heap_t *hp)
{
	uint32_t node_size = size + BS_APPEND_SLACK;
	binnode_t *node = binnode_make(node_size);

	int needed = WSIZE(t_proc_bin_t) + WSIZE(t_sub_bin_t);
	uint32_t *p = heap_alloc_N(hp, needed);
	if (p == 0)
	{
		binnode_destroy(node);
		no_memory_signal();
	}
	t_proc_bin_t *new_pb = (t_proc_bin_t *)p;
	box_proc_bin(p, size, node);

	proc_bin_link(&hp->proc_bins, new_pb, &hp->total_pb_size);

	t_sub_bin_t *new_sb = (t_sub_bin_t *)p;
	box_sub_bin(p, tag_boxed(new_pb), 0, 0, 1);
	heap_set_top(hp, p);
	return tag_boxed(new_sb);
}

term_t bits_bs_start_match2(term_t bin, int num_slots, heap_t *hp)
{
	assert(is_boxed(bin) && is_binary(peel_boxed(bin)));
	uint32_t *pb = peel_boxed(bin);

	bits_t bs;
	bits_get_real(pb, &bs);

	int needed = WSIZE(t_match_ctx_t) + num_slots *2; // slots are int64_t
	uint32_t *p = heap_alloc(hp, needed);
	t_match_ctx_t *mc = (t_match_ctx_t *)p;
	box_match_ctx(p, &bs, bin_parent(bin), num_slots);
	heap_set_top(hp, p);

	mc->saved_offsets[0] = bs.starts;

	return tag_boxed(mc);
}

int bits_bs_private_append(term_t bin,
			term_t sz, uint8_t unit, bits_t *bpc, heap_t *hp)
{
	uint32_t bcount;
	if (bits_calc_bit_size(sz, unit, &bcount) < 0)
		return -BAD_ARG;

	t_sub_bin_t *sb = (t_sub_bin_t *)peel_boxed(bin);

	assert(is_boxed(sb->parent)
		&& boxed_tag(peel_boxed(sb->parent)) == SUBTAG_PROC_BIN);

	t_proc_bin_t *pb = (t_proc_bin_t *)peel_boxed(sb->parent);

	binnode_t *node = pb->node;
	int64_t max_bit_off = (node->ends -node->starts) *8;

	if (sb->ends + bcount > max_bit_off)
	{
		// reallocate node
		int old_size = node->ends -node->starts;
		int new_size = old_size + (bcount +7) /8 + BS_APPEND_SLACK;
		binnode_t *new_node = binnode_make(new_size);
		memcpy(new_node->starts, node->starts, old_size);
		pb->node = new_node; // modified in place
		nfree((memnode_t *)node);
	}

	// the sub bin modified in place
	sb->ends += bcount;

	bits_get_real(sb, bpc);
	bpc->starts = bpc->ends -bcount;

	// the sub bin is still writable
	return 0;
}

term_t bits_bs_append(term_t bin,
			term_t sz, uint8_t unit, int extra, bits_t *bpc, heap_t *hp)
{
	assert(is_int(sz) || (is_boxed(sz) && is_bignum(peel_boxed(sz))));

	if (!is_boxed(bin) || !is_binary(peel_boxed(bin)))
		return A_BADARG;

	uint32_t bcount;
	int x = bits_calc_bit_size(sz, 1, &bcount);
	if (x == -TOO_LONG)
		return A_SYSTEM_LIMIT;
	if (x < 0)
		return A_BADARG;

	term_t result = noval;

	uint32_t *bb = peel_boxed(bin);
	if (boxed_tag(bb) == SUBTAG_SUB_BIN && sub_bin_is_writable(bb))
	{
		// writable sub bin
		t_sub_bin_t *sb = (t_sub_bin_t *)bb;
		sub_bin_not_writable(bb);

		assert(is_boxed(sb->parent)
			&& boxed_tag(peel_boxed(sb->parent)) == SUBTAG_PROC_BIN);
		t_proc_bin_t *pb = (t_proc_bin_t *)peel_boxed(sb->parent);
		//NB: pb->byte_size does not matter; binnode size matters

		binnode_t *node = pb->node;
		int64_t max_bit_off = (node->ends - node->starts) *8;

		if (sb->ends + bcount <= max_bit_off)
		{
			int needed = WSIZE(t_sub_bin_t) + extra;
			uint32_t *p = heap_alloc(hp, needed);
			t_sub_bin_t *new_sb = (t_sub_bin_t *)p;
			box_sub_bin(p, sb->parent, sb->starts, sb->ends+bcount, 1);
			heap_set_top0(hp, p);

			result = tag_boxed(new_sb);
		}
	}

	if (result == noval)
	{
		// copy case
		bits_t bs;
		bits_get_real(bb, &bs);

		if ((bs.ends - bs.starts) % unit != 0)
			return A_BADARG;

		uint32_t node_size = (bs.ends -bs.starts +bcount +7) /8 + BS_APPEND_SLACK;
		binnode_t *node = binnode_make(node_size);

		uint8_t *data = bs.data + bs.starts /8;
		uint32_t data_size = (bs.ends -bs.starts +7) /8;
		memcpy(node->starts, data, data_size);

		int needed = WSIZE(t_proc_bin_t) + WSIZE(t_sub_bin_t) + extra;
		uint32_t *p = heap_alloc_N(hp, needed);
		if (p == 0)
		{
			binnode_destroy(node);
			no_memory_signal();
		}
		t_proc_bin_t *new_pb = (t_proc_bin_t *)p;
		box_proc_bin(p, data_size, node);

		proc_bin_link(&hp->proc_bins, new_pb, &hp->total_pb_size);

		t_sub_bin_t *new_sb = (t_sub_bin_t *)p;
		box_sub_bin(p, tag_boxed(new_pb), 0, bs.ends -bs.starts +bcount, 1);
		heap_set_top0(hp, p);

		result = tag_boxed(new_sb);
	}

	// update the binary put context
	bits_get_real(peel_boxed(result), bpc);
	bpc->starts = bpc->ends -bcount;

	return result;
}

int bits_bs_put_float(term_t v, uint32_t bcount, int is_little, bits_t *bpc)
{
	assert(bpc->ends-bpc->starts >= bcount);
	double dval;

	if (is_int(v))
		dval = (double)int_value(v);
	else if (is_boxed(v))
	{
		uint32_t *vdata = peel_boxed(v);
		if (boxed_tag(vdata) == SUBTAG_FLOAT)
			dval = float_value(vdata);
		else if (is_bignum(vdata))
			dval = bignum_to_double((bignum_t *)vdata);
		else
			return -BAD_ARG;
	}
	else
		return -BAD_ARG;

	uint8_t buf[8];
	bits_t bs;
	if (bcount == 32)
	{
		union {
			float f;
			uint32_t u32;
		} u;
		u.f = (float)dval;
		if (!is_little)
			PUT_UINT_32(buf, u.u32);
		else
			PUT_UINT_32_LE(buf, u.u32);
		bits_init_buf(buf, 4, &bs);
	}
	else if (bcount == 64)
	{
		union {
			double d;
			uint64_t u64;
		} u;
		u.d = dval;
		if (!is_little)
			PUT_UINT_64(buf, u.u64);
		else
			PUT_UINT_64_LE(buf, u.u64);
		bits_init_buf(buf, 8, &bs);
	}
	else
		return -BAD_ARG;
	bits_copy(&bs, bpc);
	return 0;
}

term_t bits_bs_get_integer_imm(t_match_ctx_t *mc,
		uint32_t bcount, int is_signed, int is_little, heap_t *hp)
{
	assert(mc->bs.ends - mc->bs.starts >= bcount);

	if (bcount == 0) // valid
		return tag_int(0);

	int bo = bcount & 7;
	uint32_t size = (bcount +7) /8;
	uint8_t *buf = heap_tmp_buf(hp, size);

	uint32_t sign = MP_ZPOS;
	if (is_little)
	{
		bits_t bs = {
			.data = buf,
			.starts = 0,
			.ends = (int64_t)bcount
		};
		buf[size-1] = 0; //positive sign
		bits_fill(&mc->bs, &bs);

		// fix the last byte
		// xxx00000
		// x: bo
		if (bo > 0)
			buf[size-1] >>= (8-bo);

		int is_neg = (bo == 0)
			?(buf[size-1] & 128) != 0
			:(buf[size-1] & (1 << (bo-1))) != 0;
		if (is_neg && is_signed)
		{
			// 00000xxx
			// x: bo
			if (bo > 0)
				buf[size-1] |= BIT_MASK(8-bo) << bo;
			int carry = 1;
			for (int i = 0; i < size; i++)
				NEGATE_8(buf[i], carry);
			assert(carry == 0);
			sign = MP_NEG;
		}
	}
	else
	{
		bits_t bs = {
			.data = buf,
			.starts = (int64_t)size*8 - bcount,
			.ends = size*8
		};
		buf[0] = 0;	//positive sign
		bits_fill(&mc->bs, &bs);
		int is_neg = (bo == 0)
			?(buf[0] & 128) != 0
			:(buf[0] & (1 << (bo-1))) != 0;
		if (is_neg && is_signed)
		{
			// 000xxxxx
			// x: bo
			if (bo > 0)
				buf[0] |= BIT_MASK(8-bo) << bo;
			int carry = 1;
			for (int i = size-1; i >= 0; i--)
				NEGATE_8(buf[i], carry);
			assert(carry == 0);
			sign = MP_NEG;
		}
	}

	int ndigs = (size +1) /2;
	int needed = WSIZE(bignum_t) + (ndigs*sizeof(uint16_t) +3)/4;
	uint32_t *p = heap_alloc(hp, needed);
	bignum_t *bn = (bignum_t *)p;
	box_bignum(p, sign, ndigs, 0);
	heap_set_top(hp, p);

	if (is_little)
	{
		uint8_t *bp = buf;
		for (int i = 0; i < ndigs; i++)
		{
			assert(bp < buf+size);
			uint16_t l = *bp++;
			uint16_t h = (bp < buf+size) ?*bp++ :0;
			bn->dp[i] = (h << 8) | l;
		}
	}
	else
	{
		uint8_t *bp = buf + size-1;
		for (int i = 0; i < ndigs; i++)
		{
			assert(bp >= buf);
			uint16_t l = *bp--;
			uint16_t h = (bp >= buf) ?*bp-- :0;
			bn->dp[i] = (h << 8) | l;
		}
	}
	
	return bignum_to_term(bn);
}

term_t bits_bs_get_float(t_match_ctx_t *mc, uint32_t bcount, int is_little, heap_t *hp)
{
	if (mc->bs.ends - mc->bs.starts < bcount)
		return A_BADARG;

	// The current position must be restored if NaN/Inf are matched
	int64_t saved_starts = mc->bs.starts;

	double val;
	if (bcount == 64)
	{
		uint8_t buf[8];
		bits_t bs = {
			.data = buf,
			.starts = 0,
			.ends = 64
		};
		bits_fill(&mc->bs, &bs);
		union {
			double d;
			uint64_t u;
		} u;
		if (is_little)
			u.u = GET_UINT_64_LE(buf);
		else
			u.u = GET_UINT_64(buf);
		val = u.d;
	}
	else if (bcount == 32)
	{
		uint8_t buf[4];
		bits_t bs = {
			.data = buf,
			.starts = 0,
			.ends = 32
		};
		bits_fill(&mc->bs, &bs);
		union {
			float d;
			uint32_t u;
		} u;
		if (is_little)
			u.u = GET_UINT_32_LE(buf);
		else
			u.u = GET_UINT_32(buf);
		val = u.d;
	}
	else
		return A_BADARG;

	if (!isfinite(val))
	{
		mc->bs.starts = saved_starts;
		return A_BADARG;
	}

	return heap_float(hp, val);
}

//-----------------------------------------------------------------------------

void bits_test(void)
{
	uint8_t data1[8];
	uint8_t data2[8];

	bits_t bs1, bs2;
	bs1.data = data1;

	bs2.data = data2;

	// simple copy
	bs1.starts = 0;
	bs1.ends = 8;
	bs2.starts = 0;
	bs2.ends = 8;
	data1[0] = 137;
	data2[0] = 0;
	bits_copy(&bs1, &bs2);
	assert(data2[0]==137);
	assert(bs1.starts==bs1.ends);
	assert(bs2.starts==bs2.ends);

	// negative expansion
	bs1.starts = -24;
	bs1.ends = 8;
	bs2.starts = 0;
	bs2.ends = 32;
	data1[0] = 250;
	bits_copy_with_padding(&bs1, &bs2, 1, 255);
	assert(data2[0]==255);
	assert(data2[1]==255);
	assert(data2[2]==255);
	assert(data2[3]==250);
	assert(bs1.starts==bs1.ends);
	assert(bs2.starts==bs2.ends);

	// aligned to split
	data1[0] = 137;	// 10001001
	data2[0] = 202; // 11001010
	data2[1] = 155; // 10011011

	bs1.starts = 0;
	bs1.ends = 8;
	bs2.starts = 5;
	bs2.ends = 13;
	bits_copy(&bs1, &bs2);
	// 11001010 10011011
	// 11001100 01001011
	assert(data2[0]==204);
	assert(data2[1]==75);
	assert(bs1.starts==bs1.ends);
	assert(bs2.starts==bs2.ends);

	// split to aligned
	data1[0] = 204;
	data1[1] = 75;
	data2[0] = 0;

	bs1.starts = 5;
	bs1.ends = 13;
	bs2.starts = 0;
	bs2.ends = 8;
	bits_copy(&bs1, &bs2);
	// 11001100 01001011
	// 10001001
	assert(data2[0]==137);
	assert(bs1.starts==bs1.ends);
	assert(bs2.starts==bs2.ends);

	// trailer
	data1[0] = 14;		// 00001110
	data2[0] = 0x55;	// 01010101

	bs1.starts = 4;
	bs1.ends = 7;
	bs2.starts = 5;
	bs2.ends = 8;
	bits_copy(&bs1, &bs2);
	// 01010111
	assert(data2[0]==0x57);
	assert(bs1.starts==bs1.ends);
	assert(bs2.starts==bs2.ends);

	printk("bits ok\n");
}

//EOF
