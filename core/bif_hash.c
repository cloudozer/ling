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

term_t cbif_hash2(proc_t *proc, term_t *regs)
{
	term_t Term = regs[0];
	term_t Range = regs[1];
	if (!is_int(Range))
		badarg(Range);
	int rng = int_value(Range);
	if (rng < 1 || rng >= (1 << 27))
		badarg(Range);
	uint32_t h = simple_hash(Term, 0);
	return tag_int((h % rng) +1);
}

term_t cbif_phash2(proc_t *proc, term_t *regs)
{
	term_t Term = regs[0];
	term_t Range = regs[1];
	if (!is_int(Range) && !is_boxed_bignum(Range))
		badarg(Range);
	int64_t rng = (is_int(Range))
		?int_value(Range)
		:bignum_to_int((bignum_t *)peel_boxed(Range));
	if (rng < 1 || rng > (1ll << 32))
		badarg(Range);
	int64_t h = portable_hash(Term, 0);
	h %= rng;
   	h += 1;
	return int_to_term(h, &proc->hp);
}

term_t cbif_phash2_1(proc_t *proc, term_t *regs)
{
	//
	//TODO: uses the same algorithm as phash/2
	//
	term_t Term = regs[0];
	int64_t h = portable_hash(Term, 0);
	h &= BIT_MASK(27);
	return tag_int(h);
}

term_t cbif_phash2_2(proc_t *proc, term_t *regs)
{
	//
	//TODO: uses the same algorithm as phash/2
	//
	term_t Term = regs[0];
	term_t Range = regs[1];
	if (!is_int(Range) && !is_boxed_bignum(Range))
		badarg(Range);
	int64_t rng = (is_int(Range))
		?int_value(Range)
		:bignum_to_int((bignum_t *)peel_boxed(Range));
	if (rng < 1 || rng > (1ll << 32))
		badarg(Range);
	int64_t h = portable_hash(Term, 0);
	h %= rng;
	return int_to_term(h, &proc->hp);
}

term_t cbif_crc32_2(proc_t *proc, term_t *regs)
{
	term_t OldCrc32 = regs[0];
	term_t Data = regs[1];
	if (!is_int(OldCrc32) && !is_boxed_bignum(OldCrc32))
		badarg(OldCrc32);
	if (!is_boxed_binary(Data) && !is_list(Data))
		badarg(Data);

	int sz = iolist_size(Data);
	if (sz < 0)
		badarg(Data);
	// EXCEPTION POSSIBLE
	uint8_t *buf = heap_tmp_buf(&proc->hp, sz);
	iolist_flatten(Data, buf);

	int64_t init = (is_int(OldCrc32))
		?int_value(OldCrc32)
		:bignum_to_int((bignum_t *)peel_boxed(OldCrc32));

	unsigned long val = crc32((unsigned int)init, buf, sz);

	return int_to_term(val, &proc->hp);
}

term_t cbif_crc32_combine3(proc_t *proc, term_t *regs)
{
	term_t Crc1 = regs[0];
	term_t Crc2 = regs[1];
	term_t Size2 = regs[2];

	if (!is_int(Crc1) && !is_boxed_bignum(Crc1))
		badarg(Crc1); 
	if (!is_int(Crc2) && !is_boxed_bignum(Crc2))
		badarg(Crc2); 
	if (!is_int(Size2) && !is_boxed_bignum(Size2))
		badarg(Size2);

	int64_t c1 = (is_int(Crc1))
		?int_value(Crc1)
		:bignum_to_int((bignum_t *)peel_boxed(Crc1));
	int64_t c2 = (is_int(Crc2))
		?int_value(Crc2)
		:bignum_to_int((bignum_t *)peel_boxed(Crc2));
	int64_t sz = (is_int(Size2))
		?int_value(Size2)
		:bignum_to_int((bignum_t *)peel_boxed(Size2));

	unsigned long val = crc32_combine((unsigned long)c1,
									  (unsigned long)c2,
									  (unsigned long)sz);
	return int_to_term(val, &proc->hp);
}

term_t cbif_adler32_2(proc_t *proc, term_t *regs)
{
	term_t OldAdler32 = regs[0];
	term_t Data = regs[1];
	if (!is_int(OldAdler32) && !is_boxed_bignum(OldAdler32))
		badarg(OldAdler32);
	if (!is_boxed_binary(Data) && !is_list(Data))
		badarg(Data);

	int sz = iolist_size(Data);
	if (sz < 0)
		badarg(Data);
	// EXCEPTION POSSIBLE
	uint8_t *buf = heap_tmp_buf(&proc->hp, sz);
	iolist_flatten(Data, buf);

	int64_t init = (is_int(OldAdler32))
		?int_value(OldAdler32)
		:bignum_to_int((bignum_t *)peel_boxed(OldAdler32));

	unsigned long val = adler32((unsigned long)init, buf, sz);

	return int_to_term(val, &proc->hp);
}

term_t cbif_adler32_combine3(proc_t *proc, term_t *regs)
{
	term_t Adler1 = regs[0];
	term_t Adler2 = regs[1];
	term_t Size2 = regs[2];

	if (!is_int(Adler1) && !is_boxed_bignum(Adler1))
		badarg(Adler1);
	if (!is_int(Adler2) && !is_boxed_bignum(Adler2))
		badarg(Adler2);
	if (!is_int(Size2) && !is_boxed_bignum(Size2))
		badarg(Size2);

	int64_t c1 = (is_int(Adler1))
		?int_value(Adler1)
		:bignum_to_int((bignum_t *)peel_boxed(Adler1));
	int64_t c2 = (is_int(Adler2))
		?int_value(Adler2)
		:bignum_to_int((bignum_t *)peel_boxed(Adler2));
	int64_t sz = (is_int(Size2))
		?int_value(Size2)
		:bignum_to_int((bignum_t *)peel_boxed(Size2));

	unsigned long val = adler32_combine((unsigned long)c1,
									    (unsigned long)c2,
									    (unsigned long)sz);
	return int_to_term(val, &proc->hp);
}

//EOF
