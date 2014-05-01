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

//
// The pattern-matching routines use a trivial implementation that does not
// preprocess the patterns except aligning them to a byte boundary.
//
// A preliminary indexing of patterns may make matching faster.
//

static term_t do_compile_pattern(term_t pat, heap_t *hp)
{
	bits_t bs;
	bits_get_real(peel_boxed(pat), &bs);
	if (((bs.ends -bs.starts) & 7) != 0)
		return A_BADARG;
	int sz = (bs.ends -bs.starts) /8;
	if ((bs.starts & 7) == 0)
		return pat;
	else
	{
		// realign
		uint8_t *ptr;
		term_t bin = heap_make_bin(hp, sz, &ptr);
		while (bits_has_octet(&bs))
		{
			uint8_t bb;
			bits_get_octet(&bs, bb);
			*ptr++ = bb;
		}
		return bin;
	}
}

static term_t compile_pattern(term_t pat, heap_t *hp)
{
	term_t result = nil;
	if (is_cons(pat))
	{
		term_t l = pat;
		do {
			term_t *cons = peel_cons(l);
			if (!is_boxed_binary(cons[0]))
				return A_BADARG;
			term_t ctx = do_compile_pattern(cons[0], hp);
			if (is_atom(ctx))
				return ctx;

			result = heap_cons(hp, ctx, result);
			l = cons[1];
		} while (is_cons(l));

		if (l != nil)
			return A_BADARG;
	}
	else if is_boxed_binary(pat)
	{
		term_t ctx = do_compile_pattern(pat, hp);
		if (is_atom(ctx))
			return ctx;

		result = heap_cons(hp, ctx, result);
	}
	else
		return A_BADARG;

	return result;
}

term_t cbif_compile_pattern1(proc_t *proc, term_t *regs)
{
	term_t Pat = regs[0];
	term_t cp = compile_pattern(Pat, &proc->hp);
	if (is_atom(cp))
		fail(cp);

	return heap_tuple2(&proc->hp, A_AC, cp);
}

int exec_match(uint8_t *needle, int needle_len, uint8_t *haystack, int haystack_len, int offset)
{
	assert(needle_len > 0);
	assert(haystack_len > 0);
	uint8_t *starts = haystack +offset;
	do {
		uint8_t *p = starts;
		uint8_t *q = needle;
		while (q < needle +needle_len)
		{
			if (*p != *q)
				break;
			p++;
			q++;
		}
		if (q >= needle +needle_len)
			return starts -haystack;
		starts++;
	} while (starts < haystack +haystack_len);
	return -1;
}

term_t match_common(term_t Subj, term_t Pat, term_t Opts, int collect, proc_t *proc)
{
	if (!is_boxed_binary(Subj))
		badarg(Subj);
	term_t pats;
	if (is_tuple(Pat))
	{
		uint32_t *p = peel_tuple(Pat);
		if (p[0] != 2 || p[1] != A_AC)
			badarg(Pat);
		pats = p[2];
	}
	else
	{
		pats = compile_pattern(Pat, &proc->hp);
		if (is_atom(pats))
			fail(pats);
	}
	if (!is_list(Opts))
		badarg(Opts);

	uint8_t *subject;
	int subj_len;
	int scan_offset = 0;
	int scan_len;

	bits_t bs;
	bits_get_real(peel_boxed(Subj), &bs);
	if (((bs.ends -bs.starts) & 7) != 0)
		badarg(Subj);
	subj_len = (bs.ends -bs.starts) /8;
	if (subj_len == 0)
		return A_NOMATCH;
	scan_len = subj_len;
	if ((bs.starts & 7) == 0)
		subject = bs.data +bs.starts /8;
	else
	{
		// unaligned case
		subject = heap_tmp_buf(&proc->hp, subj_len);
		bits_t dst;
		bits_init_buf((uint8_t *)subject, subj_len, &dst);
		bits_copy(&bs, &dst);
	}

	term_t l = Opts;
	while (is_cons(l))
	{
		term_t *cons = peel_cons(l);
		// {scope,{Start,Len}}
		if (!is_tuple(cons[0]))
			badarg(Opts);
		uint32_t *p = peel_tuple(cons[0]);
		if (p[0] != 2 || p[1] != A_SCOPE || !is_tuple(p[2]))
			badarg(Opts);
		uint32_t *q = peel_tuple(p[2]);
		if (q[0] != 2 || !is_int(q[1]) || !is_int(q[2]))
			badarg(Opts);
		int start = int_value(q[1]);
		int len = int_value(q[2]);
		if (len < 0)
		{
			start += len;
			len = -len;
		}
		if (start < 0 || start > subj_len ||
				start +len < 0 || start +len > subj_len)
			badarg(Opts);
		scan_offset = start;
		scan_len = len;

		l = cons[1];
	}
	if (l != nil)
		badarg(Opts);

	int match_found = 0;
	uint8_t *match_needle = 0;
	int match_start = 0;
	int match_len = 0;

	term_t t = pats;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		assert(is_boxed_binary(cons[0]));
		bits_t bs;
		bits_get_real(peel_boxed(cons[0]), &bs);
		assert((bs.starts & 7) == 0);
		assert((bs.ends & 7) == 0);

		uint8_t *needle = bs.data +bs.starts /8;
		int needle_len = (bs.ends -bs.starts) /8;
		if (needle_len == 0)
			badarg(Pat);

		int start = exec_match(needle, needle_len, subject, scan_len, scan_offset);
		if (start >= 0)
		{
			if (!match_found || (match_found && start < match_start) ||
					(match_found && start == match_start && needle_len > match_len))
			{
				match_found = 1;
				match_needle = needle;
				match_start = start;
				match_len = needle_len;
			}
		}
		t = cons[1];
	}
	assert(t == nil);

	if (collect)
	{
		if (!match_found)
			return nil;
		term_t result = nil;
		int needle_len = match_len;
		do {
			assert(fits_int(match_start));
			assert(fits_int(needle_len));
			term_t m = heap_tuple2(&proc->hp, tag_int(match_start), tag_int(needle_len));
			result = heap_cons(&proc->hp, m, result);

			int start = exec_match(match_needle, needle_len, subject, scan_len, match_start +1);
			if (start < 0)
				break;
			match_start = start;
		} while (1);
		return list_rev(result, &proc->hp);
	}
	else
	{
		if (!match_found)
			return A_NOMATCH;
		assert(fits_int(match_start));
		assert(fits_int(match_len));
		return heap_tuple2(&proc->hp, tag_int(match_start), tag_int(match_len));
	}
}

term_t cbif_match2(proc_t *proc, term_t *regs)
{
	return match_common(regs[0], regs[1], nil, 0, proc);
}

term_t cbif_match3(proc_t *proc, term_t *regs)
{
	return match_common(regs[0], regs[1], regs[2], 0, proc);
}

term_t cbif_matches2(proc_t *proc, term_t *regs)
{
	return match_common(regs[0], regs[1], nil, 1, proc);
}

term_t cbif_matches3(proc_t *proc, term_t *regs)
{
	return match_common(regs[0], regs[1], nil, 1, proc);
}

term_t cbif_longest_common_prefix1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_longest_common_suffix1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_first1(proc_t *proc, term_t *regs)
{
	term_t Subj = regs[0];
	if (!is_boxed_binary(Subj))
		badarg(Subj);
	
	bits_t bs;
	bits_get_real(peel_boxed(Subj), &bs);
	int64_t bit_size = bs.ends -bs.starts;
	if (bit_size < 8 || ((bit_size & 7) != 0))
		badarg(Subj);

	uint8_t bb;
	bits_get_octet(&bs, bb);

	return tag_int(bb);
}

term_t cbif_last1(proc_t *proc, term_t *regs)
{
	term_t Subj = regs[0];
	if (!is_boxed_binary(Subj))
		badarg(Subj);
	
	bits_t bs;
	bits_get_real(peel_boxed(Subj), &bs);
	int64_t bit_size = bs.ends -bs.starts;
	if (bit_size < 8 || ((bit_size & 7) != 0))
		badarg(Subj);

	bs.starts = bs.ends -8;
	uint8_t bb;
	bits_get_octet(&bs, bb);

	return tag_int(bb);
}

term_t cbif_at2(proc_t *proc, term_t *regs)
{
	term_t Subj = regs[0];
	term_t Pos = regs[1];
	if (!is_boxed_binary(Subj))
		badarg(Subj);
	if (!is_int(Pos) && !is_boxed_bignum(Pos))
		badarg(Pos);
	int index = (is_int(Pos))
		?int_value(Pos)
		:bignum_to_int((bignum_t *)peel_boxed(Pos));
	if (index < 0)
		badarg(Pos);
	
	bits_t bs;
	bits_get_real(peel_boxed(Subj), &bs);
	int64_t bit_size = bs.ends -bs.starts;
	if (bit_size < (index +1) *8 || ((bit_size & 7) != 0))
		badarg(Subj);

	bs.starts += index *8;
	uint8_t bb;
	bits_get_octet(&bs, bb);

	return tag_int(bb);
}

static term_t make_part(term_t Bin, term_t Pos, term_t Len, proc_t *proc)
{
	if (!is_boxed_binary(Bin))
		badarg(Bin);
	if (!is_int(Pos))
		badarg(Pos);
	if (!is_int(Len))
		badarg(Len);
	int start = int_value(Pos);
	int len = int_value(Len);
	if (len < 0)
	{
		start += len;
		len = -len;
	}
	if (start < 0)
		badarg();

	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);
	if (bs.starts + start *8 + len *8 > bs.ends)
		badarg();

	bs.starts += start *8;
	bs.ends = bs.starts + len *8;

	int needed = WSIZE(t_sub_bin_t);
	uint32_t *p = heap_alloc(&proc->hp, needed);
	t_sub_bin_t *sb = (t_sub_bin_t *)p;
	box_sub_bin(p, bin_parent(Bin), bs.starts, bs.ends, 0);
	heap_set_top(&proc->hp, p);

	return tag_boxed(sb);
}

term_t cbif_part2(proc_t *proc, term_t *regs)
{
	// see as erlang:binary_part/2
	term_t Bin = regs[0];
	term_t PosLen = regs[1];
	if (!is_tuple(PosLen))
		badarg(PosLen);
	uint32_t *p = peel_tuple(PosLen);
	if (p[0] != 2)
		badarg(PosLen);
	return make_part(Bin, p[1], p[2], proc);
}

term_t cbif_part3(proc_t *proc, term_t *regs)
{
	// see as erlang:binary_part/3
	term_t Bin = regs[0];
	term_t Pos = regs[1];
	term_t Len = regs[2];

	return make_part(Bin, Pos, Len, proc);
}

term_t cbif_bin_to_list1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_bin_to_list2(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_bin_to_list3(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}


// term_t cbif_list_to_bin/1, works exactly as erlang:list_to_binary(proc_t *proc, term_t *regs)
term_t cbif_list_to_bin1(proc_t *proc, term_t *regs)
{
	return cbif_list_to_binary1(proc, regs);
}

term_t cbif_copy1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_copy2(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_referenced_byte_size1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_decode_unsigned1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_decode_unsigned2(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_embedded_buckets0(proc_t *proc, term_t *regs)
{
	return embed_all_buckets(&proc->hp);
}

term_t cbif_list_embedded1(proc_t *proc, term_t *regs)
{
	term_t Bucket = regs[0];
	if (!is_atom(Bucket))
		badarg(Bucket);
	term_t lst = embed_list_bucket(Bucket, &proc->hp);
	if (lst == noval)
		return A_FALSE;
	return lst;
}

term_t cbif_embedded_size2(proc_t *proc, term_t *regs)
{
	term_t Bucket = regs[0];
	term_t Name = regs[1];
	if (!is_atom(Bucket) || !is_atom(Name))
		badarg();
	
	int size;
	uint8_t *data = embed_lookup(Bucket, Name, &size);
	if (data == 0)
		return A_FALSE;

	assert(fits_int(size));
	return tag_int(size);
}

term_t cbif_embedded_part4(proc_t *proc, term_t *regs)
{
	term_t Bucket = regs[0];
	term_t Name = regs[1];
	term_t Pos = regs[2];
	term_t Len = regs[3];
	if (!is_atom(Bucket) || !is_atom(Name))
		badarg();
	if (!is_int(Pos) || !is_int(Len))
		badarg();
	int start = int_value(Pos);
	int len = int_value(Len);
	if (len < 0)
	{
		start += len;
		len = -len;
	}
	if (start < 0)
		badarg();
	
	int size;
	uint8_t *data = embed_lookup(Bucket, Name, &size);
	if (data == 0)
		return A_FALSE;

	if (start +len > size)
		badarg();

	uint8_t *ptr;
	term_t bin = heap_make_bin(&proc->hp, len, &ptr);
	memcpy(ptr, data +start, len);

	return bin;
}

term_t cbif_embedded_part3(proc_t *proc, term_t *regs)
{
	term_t Bucket = regs[0];
	term_t Name = regs[1];
	term_t PosLen = regs[2];
	if (!is_atom(Bucket) || !is_atom(Name) || !is_tuple(PosLen))
		badarg();
	uint32_t *p = peel_tuple(PosLen);
	if (*p != 2)
		badarg(PosLen);
	if (!is_int(p[1]) || !is_int(p[2]))
		badarg();
	int start = int_value(p[1]);
	int len = int_value(p[2]);
	if (len < 0)
	{
		start += len;
		len = -len;
	}
	if (start < 0)
		badarg();
	
	int size;
	uint8_t *data = embed_lookup(Bucket, Name, &size);
	if (data == 0)
		return A_FALSE;

	if (start +len > size)
		badarg();

	uint8_t *ptr;
	term_t bin = heap_make_bin(&proc->hp, len, &ptr);
	memcpy(ptr, data +start, len);

	return bin;
}

term_t cbif_lookup_embedded1(proc_t *proc, term_t *regs)
{
	term_t Name = regs[0];
	if (!is_atom(Name))
		badarg(Name);

	int size;
	uint8_t *data = embed_lookup_simple(Name, &size);
	if (data == 0)
		return A_FALSE;

	uint8_t *to;
	term_t bin = heap_make_bin(&proc->hp, size, &to);
	memcpy(to, data, size);

	return bin;
}

term_t cbif_ip_checksum1(proc_t *proc, term_t *regs)
{
	term_t Hdr = regs[0];
	if (!is_boxed_binary(Hdr))
		badarg(Hdr);

	bits_t bs;
	bits_get_real(peel_boxed(Hdr), &bs);
	if (((bs.ends -bs.starts) & 15) != 0)	// even size
		return A_BADARG;

	int n = (bs.ends -bs.starts) /16;	// in 16-bit words
	if (n > 65536)	// headers are short
		return A_BADARG;

	uint32_t csum = 0;
	if (likely((bs.starts & 7) == 0))
	{
		// aligned case - fast
		uint8_t *ptr = bs.data + bs.starts /8;
		while (n-- > 0)
		{
			csum += GET_UINT_16(ptr);
			ptr += 2;
		} 
	}
	else
	{
		// general case
		while (n-- > 0)
		{
			uint32_t h, l;
			bits_get_octet(&bs, h);
			bits_get_octet(&bs, l);
			csum += ((h << 8) | l);
		}
	}

	while (csum > 0xffff)
	{
		uint32_t carry = csum >> 16;
		csum &= 0xffff;
		csum += carry;
	}
	return tag_int(csum);
}

//EOF
