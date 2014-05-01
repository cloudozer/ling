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

#define MD5_CONTEXT_SIZE	92

//TODO
//
// encode_md5_context, decode_md5_context, and update_md5_context functions is
// an overkill. Such functions allow passing context between machines. This will
// never happen. Use approach taken by sha1_* BIFs.
//

static term_t encode_md5_context(struct md5_ctx *ctx, heap_t *hp)
{
	uint8_t *ptr;
	term_t bin = heap_make_bin(hp, MD5_CONTEXT_SIZE, &ptr);

	//
	// MD5 context binary layout
	// + 0: state[0]
	// + 4: state[1]
	// + 8: state[2]
	// +12: state[3]
	// +16: count_low
	// +20: count_high
	// +24: block (64 bytes)
	// +88: index
	// binary size = 92 bytes
	//

	PUT_UINT_32(ptr    , ctx->state[0]);
	PUT_UINT_32(ptr  +4, ctx->state[1]);
	PUT_UINT_32(ptr  +8, ctx->state[2]);
	PUT_UINT_32(ptr +12, ctx->state[3]);
	PUT_UINT_32(ptr +16, ctx->count_low);
	PUT_UINT_32(ptr +20, ctx->count_high);
	memcpy(ptr + 24, ctx->block, sizeof(ctx->block));
	PUT_UINT_32(ptr +88, ctx->index);

	return bin;
}

static int decode_md5_context(term_t Context, struct md5_ctx *ctx)
{
	if (!is_boxed(Context) || !is_binary(peel_boxed(Context)))
		return -BAD_ARG;

	bits_t bs;
	bits_get_real(peel_boxed(Context), &bs);
	if (bs.ends - bs.starts != MD5_CONTEXT_SIZE *8)
		return -BAD_ARG;

	bits_get_word(&bs, ctx->state[0]);
	bits_get_word(&bs, ctx->state[1]);
	bits_get_word(&bs, ctx->state[2]);
	bits_get_word(&bs, ctx->state[3]);
	bits_get_word(&bs, ctx->count_low);
	bits_get_word(&bs, ctx->count_high);
	for (int i = 0; i < sizeof(ctx->block); i++)
		bits_get_octet(&bs, ctx->block[i]);
	bits_get_word(&bs, ctx->index);

	return 0;
}

static int update_md5_context(term_t IoData, struct md5_ctx *ctx)
{
	if (is_list(IoData))
	{
		term_t t = IoData;
		while (is_cons(t))
		{
			term_t *cons = peel_cons(t);
			if (is_int(cons[0]))
			{
				int v = int_value(cons[0]);
				if (v < 0 || v > 255)
					return -BAD_ARG;
				uint8_t b = v;
				md5_update(ctx, 1, &b);
			}
			else
			{
				int x = update_md5_context(cons[0], ctx);
				if (x < 0)
					return x;
			}
			t = cons[1];
		}

		if (t != nil)
		{
			int x = update_md5_context(t, ctx);
			if (x < 0)
				return x;
		}
	}
	else if (is_boxed(IoData) && is_binary(peel_boxed(IoData)))
	{

		bits_t bs;
		bits_get_real(peel_boxed(IoData), &bs);
		if (((bs.ends - bs.starts) & 7) != 0)
			return -BAD_ARG;

		if ((bs.starts & 7) == 0)
		{
			// easy case
			uint8_t *data = bs.data + bs.starts /8;
			size_t len = (bs.ends - bs.starts) /8;
			md5_update(ctx, len, data);
		}
		else
		{
			// tougher case
			while (bits_has_octet(&bs))
			{
				uint8_t o;
				bits_get_octet(&bs, o);
				md5_update(ctx, 1, &o);
			}
		}
	}
	else
		return -BAD_ARG;

	return 0;
}

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

term_t cbif_md5_1(proc_t *proc, term_t *regs)
{
	term_t Data = regs[0];
	if (!is_boxed_binary(Data) && !is_list(Data))
		badarg(Data);

	int sz = iolist_size(Data);
	if (sz < 0)
		badarg(Data);
	// EXCEPTION POSSIBLE
	uint8_t *buf = heap_tmp_buf(&proc->hp, sz);
	iolist_flatten(Data, buf);

	struct md5_ctx ctx;
	md5_init(&ctx);
	md5_update(&ctx, sz, buf);

	uint8_t *ptr;
	term_t bin = heap_make_bin(&proc->hp, MD5_DIGEST_SIZE, &ptr);
	md5_digest(&ctx, MD5_DIGEST_SIZE, ptr);

	return bin;
}

term_t cbif_md5_init0(proc_t *proc, term_t *regs)
{
	struct md5_ctx ctx;
	md5_init(&ctx);

	return encode_md5_context(&ctx, &proc->hp);
}

term_t cbif_md5_update2(proc_t *proc, term_t *regs)
{
	term_t Context = regs[0];
	term_t IoData = regs[1];

	struct md5_ctx ctx;
	if (decode_md5_context(Context, &ctx) < 0)
		badarg(Context);

	int x = update_md5_context(IoData, &ctx);
	if (x == -TOO_DEEP)
		fail(A_SYSTEM_LIMIT);
	else if (x < 0)
		badarg(IoData);

	return encode_md5_context(&ctx, &proc->hp);
}

term_t cbif_md5_final1(proc_t *proc, term_t *regs)
{
	term_t Context = regs[0];

	struct md5_ctx ctx;
	if (decode_md5_context(Context, &ctx) < 0)
		badarg(Context);

	uint8_t *ptr;
	term_t bin = heap_make_bin(&proc->hp, MD5_DIGEST_SIZE, &ptr);
	md5_digest(&ctx, MD5_DIGEST_SIZE, ptr);

	return bin;
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

//EOF
