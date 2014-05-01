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

#include "mixed.h"

#include "ling_common.h"

#include "term.h"
#include "atom_defs.h"
#include "bignum.h"
#include "string.h"
#include "bits.h"
#include "getput.h"

static term_t mixed_mul_1(term_t a, term_t b, heap_t *hp);
static term_t mixed_int_div_1(bignum_t *bn1, bignum_t *bn2, heap_t *hp);
static term_t mixed_rem_1(bignum_t *bn1, bignum_t *bn2, heap_t *hp);
static term_t mixed_add_2(bignum_t *a, bignum_t *b, heap_t *hp);
static term_t mixed_sub_2(bignum_t *a, bignum_t *b, heap_t *hp);

typedef uint16_t (*op_func_t)(uint16_t d1, uint16_t d2);
static term_t bool_op(term_t a, term_t b, op_func_t op, heap_t *hp);
static term_t bool_op_1(bignum_t *bn, int v, op_func_t op, heap_t *hp);
static term_t bool_op_2(bignum_t *bn1, bignum_t *bn2, op_func_t op, heap_t *hp);
static term_t bool_op_3(uint16_t *p1, int sz1, int is_neg1,
						uint16_t *p2, int sz2, int is_neg2, op_func_t op, heap_t *hp);
static uint16_t bool_and(uint16_t d1, uint16_t d2);
static uint16_t bool_or(uint16_t d1, uint16_t d2);
static uint16_t bool_xor(uint16_t d1, uint16_t d2);

static term_t bsl_1(bignum_t *bn, int n, heap_t *hp);
static term_t bsr_1(bignum_t *bn, int n, heap_t *hp);

term_t mixed_add(term_t a, term_t b, heap_t *hp)
{
	assert(!are_both_int(a, b));
	if (is_int(a) && is_boxed(b))
	{
		uint32_t *bdata = peel_boxed(b);
		if (boxed_tag(bdata) == SUBTAG_FLOAT)
			return heap_float_with_check(hp,
						(double)int_value(a) + float_value(bdata));

		if (is_bignum(bdata))
		{
			bignum_t *aa = bignum_from_int(hp, int_value(a));
			return mixed_add_2(aa, (bignum_t *)bdata, hp);
		}
	}
	else if (is_boxed(a) && is_int(b))
	{
		uint32_t *adata = peel_boxed(a);
		if (boxed_tag(adata) == SUBTAG_FLOAT)
			return heap_float_with_check(hp,
				   float_value(adata) + (double)int_value(b));

		if (is_bignum(adata))
		{
			bignum_t *bb = bignum_from_int(hp, int_value(b));
			return mixed_add_2((bignum_t *)adata, bb, hp);
		}
	}
	else if (is_boxed(a) && is_boxed(b))
	{
		uint32_t *adata = peel_boxed(a);
		uint32_t *bdata = peel_boxed(b);

		if (boxed_tag(adata) == SUBTAG_FLOAT)
		{
			if (boxed_tag(bdata) == SUBTAG_FLOAT)
				return heap_float_with_check(hp,
						float_value(adata) + float_value(bdata));
			
			if (is_bignum(bdata))
				return heap_float_with_check(hp,
					float_value(adata) + bignum_to_double((bignum_t *)bdata));
		}

		if (boxed_tag(bdata) == SUBTAG_FLOAT)
		{
			if (boxed_tag(adata) == SUBTAG_FLOAT)
				return heap_float_with_check(hp,
						float_value(adata) + float_value(bdata));

			if (is_bignum(adata))
				return heap_float_with_check(hp,
					bignum_to_double((bignum_t *)adata) + float_value(bdata));
		}

		if (is_bignum(adata) && is_bignum(bdata))
			return mixed_add_2((bignum_t *)adata, (bignum_t *)bdata, hp);
	}

	return A_BADARITH;
}

static term_t mixed_add_2(bignum_t *a, bignum_t *b, heap_t *hp)
{
	bignum_t *r = bignum_add(hp, a, b);

	if (r->used > MAX_DIGITS)
		return A_SYSTEM_LIMIT;

	if (bignum_fits_small_int(r))
		return tag_int(bignum_to_int(r));

	return tag_boxed(r);
}

term_t mixed_add_immed(term_t a, int i, heap_t *hp)
{
	assert(!is_int(a));

	if (is_boxed(a))
	{
		uint32_t *adata = peel_boxed(a);
		if (boxed_tag(adata) == SUBTAG_FLOAT)
			return heap_float_with_check(hp, float_value(adata) + (double)i);
		if (is_bignum(adata))
		{
			bignum_t *ii = bignum_from_int(hp, i);
			return mixed_add_2((bignum_t *)adata, ii, hp);
		}
	}

	return A_BADARITH;
}

term_t mixed_sub(term_t a, term_t b, heap_t *hp)
{
	assert(!are_both_int(a, b));
	if (is_int(a) && is_boxed(b))
	{
		uint32_t *bdata = peel_boxed(b);
		if (boxed_tag(bdata) == SUBTAG_FLOAT)
			return heap_float_with_check(hp, (double)int_value(a) - float_value(bdata));

		if (is_bignum(bdata))
		{
			bignum_t *aa = bignum_from_int(hp, int_value(a));
			return mixed_sub_2(aa, (bignum_t *)bdata, hp);
		}
	}
	else if (is_boxed(a) && is_int(b))
	{
		uint32_t *adata = peel_boxed(a);
		if (boxed_tag(adata) == SUBTAG_FLOAT)
			return heap_float_with_check(hp, float_value(adata) - (double)int_value(b));

		if (is_bignum(adata))
		{
			bignum_t *bb = bignum_from_int(hp, int_value(b));
			return mixed_sub_2((bignum_t *)adata, bb, hp);
		}
	}
	else if (is_boxed(a) && is_boxed(b))
	{
		uint32_t *adata = peel_boxed(a);
		uint32_t *bdata = peel_boxed(b);

		if (boxed_tag(adata) == SUBTAG_FLOAT)
		{
			if (boxed_tag(bdata) == SUBTAG_FLOAT)
				return heap_float_with_check(hp,
						float_value(adata) - float_value(bdata));
			
			if (is_bignum(bdata))
				return heap_float_with_check(hp,
						float_value(adata) - bignum_to_double((bignum_t *)bdata));
		}

		if (boxed_tag(bdata) == SUBTAG_FLOAT)
		{
			if (boxed_tag(adata) == SUBTAG_FLOAT)
				return heap_float_with_check(hp,
						float_value(adata) - float_value(bdata));

			if (is_bignum(adata))
				return heap_float_with_check(hp,
						bignum_to_double((bignum_t *)adata) - float_value(bdata));
		}

		if (is_bignum(adata) && is_bignum(bdata))
			return mixed_sub_2((bignum_t *)adata, (bignum_t *)bdata, hp);
	}

	return A_BADARITH;
}

static term_t mixed_sub_2(bignum_t *a, bignum_t *b, heap_t *hp)
{
	bignum_t *r = bignum_sub(hp, a, b);

	if (r->used > MAX_DIGITS)
		return A_SYSTEM_LIMIT;

	if (bignum_fits_small_int(r))
		return tag_int(bignum_to_int(r));

	return tag_boxed(r);
}

term_t mixed_mul(term_t a, term_t b, heap_t *hp)
{
	assert(!are_both_int(a, b));
	if (is_boxed(a))
		return mixed_mul_1(a, b, hp);

	if (is_boxed(b))	
		return mixed_mul_1(b, a, hp);

	return A_BADARITH;
}

static term_t mixed_mul_1(term_t a, term_t b, heap_t *hp)
{
	assert(is_boxed(a));
	uint32_t *adata = peel_boxed(a);
	if (is_bignum(adata))
	{
		if (is_int(b))
		{
			bignum_t *q = bignum_from_int(hp, int_value(b));
			bignum_t *r = bignum_mul(hp, (bignum_t *)adata, q);

			if (r->used > MAX_DIGITS)
				return A_SYSTEM_LIMIT;

			// a very rare case
			if (bignum_fits_small_int(r))
				return tag_int(bignum_to_int(r));

			return tag_boxed(r);
		}
		else if (is_boxed(b))
		{
			uint32_t *bdata = peel_boxed(b);
			if (is_bignum(bdata))
			{
				bignum_t *bna = (bignum_t *)adata;
				bignum_t *bnb = (bignum_t *)bdata;

				if (bna->used + bnb->used -1 > MAX_DIGITS)
					return A_SYSTEM_LIMIT;

				bignum_t *r = bignum_mul(hp, (bignum_t *)adata, (bignum_t *)bdata);

				if (r->used > MAX_DIGITS)
					return A_SYSTEM_LIMIT;

				assert(!bignum_fits_small_int(r));
				return tag_boxed(r);
			}
			else if (boxed_tag(bdata) == SUBTAG_FLOAT)
				return heap_float_with_check(hp,
					bignum_to_double((bignum_t *)adata) * float_value(bdata));
		}
	}
	else if (boxed_tag(adata) == SUBTAG_FLOAT)
	{
		double d = float_value(adata);
		if (is_int(b))
			return heap_float_with_check(hp, d * int_value(b));
		else if (is_boxed(b))
		{
			uint32_t *bdata = peel_boxed(b);
			if (is_bignum(bdata))
				return heap_float_with_check(hp, d * bignum_to_double((bignum_t *)bdata));
			else if (boxed_tag(bdata) == SUBTAG_FLOAT)
				return heap_float_with_check(hp, d * float_value(bdata));
		}
	}

	return A_BADARITH;
}

term_t mixed_int_div(term_t a, term_t b, heap_t *hp)
{
	assert(!are_both_int(a, b));

	if (is_int(a))
	{
		if (is_boxed(b) && is_bignum(peel_boxed(b)))
		{
			bignum_t *den = bignum_from_int(hp, int_value(a));
			return mixed_int_div_1(den, (bignum_t *)peel_boxed(b), hp);
		}
	}
	else if (is_boxed(a) && is_bignum(peel_boxed(a)))
	{
		if (is_int(b))
		{
			if (int_value(b) == 0)
				return A_BADARITH;
			bignum_t *nom = bignum_from_int(hp, int_value(b));
			return mixed_int_div_1((bignum_t *)peel_boxed(a), nom, hp);
		}
		else if (is_boxed(b) && is_bignum(peel_boxed(b)))
			return mixed_int_div_1((bignum_t *)peel_boxed(a),
				   				   (bignum_t *)peel_boxed(b), hp);
	}

	if ((is_boxed(a) && boxed_tag(peel_boxed(a)) == SUBTAG_FLOAT) ||
		(is_boxed(b) && boxed_tag(peel_boxed(b)) == SUBTAG_FLOAT))
			return A_BADARITH;

	return A_BADARITH;
}

static term_t mixed_int_div_1(bignum_t *bn1, bignum_t *bn2, heap_t *hp)
{
	if (bignum_is_zero(bn2))
		return A_BADARITH;

	bignum_t *remainder; // discarded
	bignum_t *rbn = bignum_div(hp, bn1, bn2, &remainder);

	if (bignum_fits_small_int(rbn))
		return tag_int(bignum_to_int(rbn));

	return tag_boxed(rbn);
}

term_t mixed_rem(term_t a, term_t b, heap_t *hp)
{
	assert(!are_both_int(a, b));

	if (is_int(a))
	{
		if (is_boxed(b) && is_bignum(peel_boxed(b)))
		{
			bignum_t *den = bignum_from_int(hp, int_value(a));
			return mixed_rem_1(den, (bignum_t *)peel_boxed(b), hp);
		}
	}
	else if (is_boxed(a) && is_bignum(peel_boxed(a)))
	{
		if (is_int(b))
		{
			bignum_t *nom = bignum_from_int(hp, int_value(b));
			return mixed_rem_1((bignum_t *)peel_boxed(a), nom, hp);
		}
		else if (is_boxed(b) && is_bignum(peel_boxed(b)))
			return mixed_rem_1((bignum_t *)peel_boxed(a),
				   			   (bignum_t *)peel_boxed(b), hp);
	}

	if ((is_boxed(a) && boxed_tag(peel_boxed(a)) == SUBTAG_FLOAT) ||
		(is_boxed(b) && boxed_tag(peel_boxed(b)) == SUBTAG_FLOAT))
			return A_BADARITH;

	return A_BADARITH;
}

static term_t mixed_rem_1(bignum_t *bn1, bignum_t *bn2, heap_t *hp)
{
	if (bignum_is_zero(bn2))
		return A_BADARITH;

	bignum_t *remainder;
	UNUSED bignum_t *discard = bignum_div(hp, bn1, bn2, &remainder);

	if (bignum_fits_small_int(remainder))
		return tag_int(bignum_to_int(remainder));

	return tag_boxed(remainder);
}

term_t mixed_band(term_t a, term_t b, heap_t *hp)
{
	return bool_op(a, b, bool_and, hp);
}

term_t mixed_bor(term_t a, term_t b, heap_t *hp)
{
	return bool_op(a, b, bool_or, hp);
}

term_t mixed_bxor(term_t a, term_t b, heap_t *hp)
{
	return bool_op(a, b, bool_xor, hp);
}

term_t mixed_bnot(term_t a, heap_t *hp)
{
	assert(!is_int(a));
	if (is_boxed(a) && is_bignum(peel_boxed(a)))
	{
		bignum_t *bn = (bignum_t *)peel_boxed(a);
		int buf_size = bn->used *2 +2;
		uint8_t *buf = heap_tmp_buf(hp, buf_size);
		bignum_to_bytes(bn, buf, buf_size);
		int is_neg = ((buf[0] & 128) == 0);
		uint8_t carry = 1;
		for (int i = buf_size-1; i >= 0; i--)
		{
			buf[i] = ~buf[i];
			if (is_neg)
				NEGATE_8(buf[i], carry);
		}
		while (buf_size > 2 && buf[0] == 0 && buf[1] == 0)
		{
			buf += 2;
			buf_size -= 2;
		}
		int ndigs = buf_size /2;

		if (ndigs > MAX_DIGITS)
			return A_SYSTEM_LIMIT;

		uint16_t *digits;
		term_t r = heap_bignum(hp, (is_neg) ?MP_NEG :MP_ZPOS, ndigs, &digits);
		uint8_t *p = buf + buf_size;
		for (int i = 0; i < ndigs; i++)
		{
			uint16_t l, h;
		    --p; l = *p;
			--p; h = *p;
			uint16_t d = (h << 8) | l;
			digits[i] = d;
		}

		bignum_t *rbn = (bignum_t *)peel_boxed(r);
		return bignum_to_term(rbn);
	}

	return A_BADARITH;
}

static term_t bool_op(term_t a, term_t b, op_func_t op, heap_t *hp)
{
	assert(!are_both_int(a, b));

	if (is_int(a))
	{
		if (is_boxed(b) && is_bignum(peel_boxed(b)))
			return bool_op_1((bignum_t *)peel_boxed(b), int_value(a), op, hp);
	}
	if (is_boxed(a) && is_bignum(peel_boxed(a)))
	{
		if (is_int(b))
			return bool_op_1((bignum_t *)peel_boxed(a), int_value(b), op, hp);
		else if (is_boxed(b) && is_bignum(peel_boxed(b)))
			return bool_op_2((bignum_t *)peel_boxed(a),
				   		 	 (bignum_t *)peel_boxed(b), op, hp);
	}

	return A_BADARITH;
}

static term_t bool_op_1(bignum_t *bn, int v, op_func_t op, heap_t *hp)
{
	int is_neg = 0;
	if (v < 0)
	{
		v = -v;
		is_neg = 1;
	}

	uint16_t pair[2] = {
		(uint16_t)v,
		(uint16_t)(v >> 16)
	};

	return bool_op_3(pair, 2, is_neg,
					 bn->dp, bn->used, bignum_is_neg(bn), op, hp);
}

static term_t bool_op_2(bignum_t *bn1, bignum_t *bn2, op_func_t op, heap_t *hp)
{
	return bool_op_3(bn1->dp, bn1->used, bignum_is_neg(bn1),
					 bn2->dp, bn2->used, bignum_is_neg(bn2), op, hp);
}

static term_t bool_op_3(uint16_t *p1, int sz1, int is_neg1,
						uint16_t *p2, int sz2, int is_neg2, op_func_t op, heap_t *hp)
{
	int rsz = (sz1 > sz2) ?sz1 :sz2;
	int needed = WSIZE(bignum_t) + (rsz*sizeof(uint16_t) +3) /4;
	uint32_t *p = heap_alloc(hp, needed);
	bignum_t *rbn = (bignum_t *)p;
	box_bignum(p, MP_ZPOS, rsz, 0);
	heap_set_top(hp, p);
	uint16_t *rp = rbn->dp;

	uint32_t carry1 = 1;
	uint32_t carry2 = 1;
	while (rsz > 0)
	{
		uint16_t d1, d2;
		if (sz1 > 0)
		{
		    d1 = *p1++;
			if (is_neg1)
				NEGATE_16(d1, carry1);
			sz1--;
		}
		else if (is_neg1)
			d1 = (carry1 == 0) ?0xffff :0;
		else
			d1 = 0;

		if (sz2 > 0)
		{
			d2 = *p2++;
			if (is_neg2)
				NEGATE_16(d2, carry2);
			sz2--;
		}
		else if (is_neg2)
			d2 = (carry2 == 0) ?0xffff :0;
		else
			d2 = 0;

		*rp++ = op(d1, d2);
		rsz--;
	}

	assert(sz1 == 0 && sz2 == 0);

	// fix sign
	assert(rbn->used > 0);
	if (op(is_neg1, is_neg2))
	{
		uint32_t carry = 1;
		for (int i = 0; i < rbn->used; i++)
			NEGATE_16(rbn->dp[i], carry);
		assert(carry == 0);
		rbn->sign = MP_NEG;
	}

	return bignum_to_term(rbn);
}

static uint16_t bool_and(uint16_t d1, uint16_t d2)
{
	return d1 & d2;
}

static uint16_t bool_or(uint16_t d1, uint16_t d2)
{
	return d1 | d2;
}

static uint16_t bool_xor(uint16_t d1, uint16_t d2)
{
	return d1 ^ d2;
}

term_t mixed_bsl_i(int a, int b, heap_t *hp)
{
	bignum_t *bn = bignum_from_int(hp, a);
	if (b < 0)
		return bsr_1(bn, -b, hp);
	else
		return bsl_1(bn, b, hp);
}

term_t mixed_bsl(term_t a, term_t b, heap_t *hp)
{
	assert(!are_both_int(a, b));
	if (is_int(b))
	{
		if (is_boxed(a) && is_bignum(peel_boxed(a)))
		{
			int i = int_value(b);
			if (i < 0)
				return bsr_1((bignum_t *)peel_boxed(a), -i, hp);
			else
				return bsl_1((bignum_t *)peel_boxed(a), i, hp);
		}
	}
	else if (is_boxed_bignum(b))
	{
		if (is_int(a) || (is_boxed(a) && is_bignum(peel_boxed(a))))
		{
			int v = 0;
			if (is_int(a))
			{
				if (int_value(a) < 0)
					v = -1;
			}
			else
			{
				bignum_t *bna = (bignum_t *)peel_boxed(a);
				if (bignum_is_neg(bna))
					v = -1;
			}

			bignum_t *bnb = (bignum_t *)peel_boxed(b);
			if (bignum_is_neg(bnb))
				return tag_int(v);
			else
				return A_SYSTEM_LIMIT;
		}
	}

	return A_BADARITH;
}

term_t mixed_bsr_i(int a, int b, heap_t *hp)
{
	bignum_t *bn = bignum_from_int(hp, a);
	if (b < 0)
		return bsl_1(bn, -b, hp);
	else
		return bsr_1(bn, b, hp);
}

term_t mixed_bsr(term_t a, term_t b, heap_t *hp)
{
	assert(!are_both_int(a, b));
	if (is_int(b))
	{
		if (is_boxed(a) && is_bignum(peel_boxed(a)))
		{
			int i = int_value(b);
			if (i < 0)
				return bsl_1((bignum_t *)peel_boxed(a), -i, hp);
			else
				return bsr_1((bignum_t *)peel_boxed(a), i, hp);
		}
	}
	else if (is_boxed_bignum(b))
	{
		if (is_int(a) || (is_boxed(a) && is_bignum(peel_boxed(a))))
		{
			int v = 0;
			if (is_int(a))
			{
				if (int_value(a) < 0)
					v = -1;
			}
			else
			{
				bignum_t *bna = (bignum_t *)peel_boxed(a);
				if (bignum_is_neg(bna))
					v = -1;
			}

			bignum_t *bnb = (bignum_t *)peel_boxed(b);
			if (bignum_is_neg(bnb))
				return A_SYSTEM_LIMIT;
			else
				return tag_int(v);
		}
	}

	return A_BADARITH;
}

static term_t bsl_1(bignum_t *bn, int n, heap_t *hp)
{
	assert(n >= 0);

	int in_digs = bn->used;
	int out_digs = in_digs + (n +15) /16;
	int bcount = in_digs *16;

	// out_digs may overshoot the number of digits in the resultant bignum by
	// maximum one digits; we preliminary check the system limit taking into
	// account this possibility.
	
	if (out_digs -1 > MAX_DIGITS)
		return A_SYSTEM_LIMIT;

	uint8_t *in_buf = heap_tmp_buf(hp, in_digs *2);
	uint8_t *out_buf = heap_tmp_buf(hp, out_digs *2);

	uint8_t *p = in_buf + in_digs *2;
	for (int i = 0; i < bn->used; i++)
	{
		uint16_t d = bn->dp[i];
		--p; *p = (uint8_t)d;
		--p; *p = (uint8_t)(d >> 8);
	}

	bits_t ibs = {
		.data = in_buf,
		.starts = 0,
		.ends = bcount
	};

	bits_t obs = {
		.data = out_buf,
		.starts = out_digs *16 -n -bcount,
		.ends = out_digs *16 -n
	};

	memset(out_buf, 0, out_digs *2);
	bits_copy(&ibs, &obs);

	// Trim the number of digits in the result before allocating the bignum;
	// otherwise, it may overflow
	while (out_buf[0] == 0 && out_buf[1] == 0)
	{
		out_digs--;
		out_buf += 2;
	}

	if (out_digs > MAX_DIGITS)
		return A_SYSTEM_LIMIT;

	uint16_t *digits;
	term_t r = heap_bignum(hp, bn->sign, out_digs, &digits);

	p = out_buf + out_digs *2;
	for (int i = 0; i < out_digs; i++)
	{
		uint8_t h, l;
		--p; l = *p;
		--p; h = *p;
		digits[i] = (h << 8) | l;
	}
	
	bignum_t *rbn = (bignum_t *)peel_boxed(r);
	return bignum_to_term(rbn);
}

static term_t bsr_1(bignum_t *bn, int n, heap_t *hp)
{
	assert(n >= 0);
	if (n >= bn->used*16)
	{
		if (bignum_is_neg(bn))
			return tag_int(-1);
		else
			return tag_int(0);
	}

	if (n == 0)
		return bignum_to_term(bn);

	int in_digs = bn->used;
	int out_digs = in_digs - n/16;

	int in_size = in_digs *2 +1;
	int out_size = out_digs *2 +1;

	int bcount = in_size *8 -n;

	uint8_t *in_buf = heap_tmp_buf(hp, in_size);
	uint8_t *out_buf = heap_tmp_buf(hp, out_size);

	bignum_to_bytes(bn, in_buf, in_size);

	bits_t ibs = {
		.data = in_buf,
		.starts = 0,
		.ends = bcount
	};

	bits_t obs = {
		.data = out_buf,
		.starts = out_size *8 -bcount,
		.ends = out_size *8
	};

	assert(in_buf[0] == 0 || in_buf[0] == 255);
	memset(out_buf, in_buf[0], out_size);

	bits_copy(&ibs, &obs);

	uint16_t *digits;
	term_t r = heap_bignum(hp, bn->sign, out_digs, &digits);
	int is_neg = bn->sign == MP_NEG;

	uint8_t *p = out_buf + out_size;
	uint32_t carry = 1;
	for (int i = 0; i < out_digs; i++)
	{
		uint8_t h, l;
		--p; l = *p;
		--p; h = *p;
		uint16_t d = (h << 8) | l;
		if (is_neg)
			NEGATE_16(d, carry);
		digits[i] = d;
	}
	
	bignum_t *rbn = (bignum_t *)peel_boxed(r);
	return bignum_to_term(rbn);
}

//EOF
