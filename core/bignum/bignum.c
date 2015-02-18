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

#include "bignum.h"

#include "bits.h"

// A crippled bignum to initialize result pointer when
// calling mpi function(); the alloc size of the bignum
// is zero thus forcing the allocation of the result
//
static bignum_t bogo_bignum = {
	.sign = MP_ZPOS,
	.alloc = 0,
	.used = 0,
};

term_t bignum_to_term(bignum_t *bn)
{
	// get rid of leading zeros
	while (bn->used > 1 && bn->dp[bn->used-1] == 0)
		bn->used--;
	if (bignum_fits_small_int(bn))
		return tag_int(bignum_to_int(bn));
	else
		return tag_boxed(bn);
}

bignum_t *bignum_from_int(heap_t *hp, int64_t z)
{
	bignum_t *bn;
	mp_err err = mp_init(hp, &bn);
	if (err != MP_OKAY)
		return 0;
	err = mp_set_int(hp, &bn, z);
	if (err != MP_OKAY)
		return 0;
	return bn;
}

bignum_t *bignum_from_uint(heap_t *hp, uint64_t u)
{
	// depends on mpi internals
	bignum_t *bn;
	mp_err err = mp_init_size(hp, &bn, 4);
	if (err != MP_OKAY)
		return 0;
	bn->dp[0] = (uint16_t)u;
	bn->dp[1] = (uint16_t)(u >> 16);
	bn->dp[2] = (uint16_t)(u >> 32);
	bn->dp[3] = (uint16_t)(u >> 48);
	if (bn->dp[1]) bn->used = 2;
	if (bn->dp[2]) bn->used = 3;
	if (bn->dp[3]) bn->used = 4;
	return bn;	
}

int bignum_fits_small_int(bignum_t *bn)
{
	if (USED(&bn) < 2)
		return 1;
	if (USED(&bn) > 2)
		return 0;
	
	mp_digit d0 = DIGIT(&bn, 0);
	mp_digit d1 = DIGIT(&bn, 1);

	uint32_t v = (d1 << MP_DIGIT_BIT) | d0;
	
	return ((SIGN(&bn) == MP_ZPOS) && (v <= MAX_INT_VALUE)) ||
		   ((SIGN(&bn) == MP_NEG) && (v <= -MIN_INT_VALUE));
}

int64_t bignum_to_int(bignum_t *bn)
{
	assert(bn->used > 0);
	int i = bn->used-1;
	int64_t z = bn->dp[i];
	while (--i >= 0)
	{
		z *= RADIX;
		z += bn->dp[i];
	}
	if (bn->sign == MP_NEG)
		z = -z;
	return z;
}

uint64_t bignum_to_uint(bignum_t *bn)
{
	assert(bn->sign == MP_ZPOS);
	assert(bn->used > 0);
	int i = bn->used-1;
	uint64_t u = bn->dp[i];
	while (--i >= 0)
	{
		u *= RADIX;
		u += bn->dp[i];
	}
	return u;
}

double bignum_to_double(bignum_t *bn)
{
	double r = 0;
	double radix = 65536;
	for (int i = bn->used-1; i >= 0; i--)
		r = r * radix + bn->dp[i];
	if (bn->sign == MP_NEG)
		r = -r;
	return r;
}

void bignum_to_bytes(bignum_t *bn, uint8_t *arr, int arr_size)
{
	assert(arr_size >= bn->used *2);
	uint8_t *p = arr+arr_size-1;
	int is_neg = bn->sign == MP_NEG;
	uint32_t carry = 1;
	for (int i = 0; i < bn->used; i++)
	{
		uint16_t d = bn->dp[i];
		if (is_neg)
			NEGATE_16(d, carry);
		uint8_t h = (uint8_t)(d >> 8);
		uint8_t l = (uint8_t)d;
		*p-- = l;
		*p-- = h;
	}
	//pad the rest with sign byte
	while (p >= arr && p < arr+arr_size)
		*p-- = (is_neg) ?255 :0;
}

int bignum_compare(bignum_t *bn1, bignum_t *bn2)
{
	return mp_cmp(&bn1, &bn2);
}

bignum_t *bignum_from_str(heap_t *hp, char *str)
{
	bignum_t *bn;
	mp_err err = mp_init(hp, &bn);
	if (err != MP_OKAY)
		return 0;
	err = mp_read_radix(hp, &bn, (unsigned char *)str, 10);
	if (err != MP_OKAY)
		return 0;
	return bn;
}

int bignum_to_str(heap_t *hp, bignum_t *bn, char *str)
{
	mp_err err = mp_toradix(hp, &bn, (unsigned char *)str, 10);
	if (err != MP_OKAY)
		return -1;

	return 0; // Success
}

bignum_t *bignum_add(heap_t *hp, bignum_t *a, bignum_t *b)
{
	bignum_t *r = &bogo_bignum;
	mp_err err = mp_add(hp, &a, &b, &r);
	if (err != MP_OKAY)
		return 0;

	return r;
}

bignum_t *bignum_sub(heap_t *hp, bignum_t *a, bignum_t *b)
{
	bignum_t *r = &bogo_bignum;
	mp_err err = mp_sub(hp, &a, &b, &r);
	if (err != MP_OKAY)
		return 0;

	return r;
}

bignum_t *bignum_mul(heap_t *hp, bignum_t *a, bignum_t *b)
{
	bignum_t *r = &bogo_bignum;
	mp_err err = mp_mul(hp, &a, &b, &r);
	if (err != MP_OKAY)
		return 0;

	return r;
}

bignum_t *bignum_div(heap_t *hp, bignum_t *a, bignum_t *b, bignum_t **r)
{
	bignum_t *q = &bogo_bignum;
	bignum_t *remainder = &bogo_bignum;
	mp_err err = mp_div(hp, &a, &b, &q, &remainder);
	if (err != MP_OKAY)
		return 0;

	*r = remainder;
	return q;
}

