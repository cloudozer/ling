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

#include "mpi.h"

#include "heap.h"

term_t bignum_to_term(bignum_t *bn);
bignum_t *bignum_from_int(heap_t *hp, int64_t z);
bignum_t *bignum_from_uint(heap_t *hp, uint64_t u);
int bignum_fits_small_int(bignum_t *bn);
int64_t bignum_to_int(bignum_t *bn);
uint64_t bignum_to_uint(bignum_t *bn);
double bignum_to_double(bignum_t *bn);
void bignum_to_bytes(bignum_t *bn, uint8_t *arr, int arr_size);

int bignum_compare(bignum_t *bn1, bignum_t *bn2);

bignum_t *bignum_from_str(heap_t *hp, char *str);
#define bignum_str_size(bn)	mp_radix_size(&(bn), 10)
int bignum_to_str(heap_t *hp, bignum_t *bn, char *str);

#define bignum_is_zero(bn)	(mp_cmp_z(&(bn)) == 0)
#define bignum_is_neg(bn)	(mp_cmp_z(&(bn)) < 0)

bignum_t *bignum_add(heap_t *hp, bignum_t *a, bignum_t *b);
bignum_t *bignum_sub(heap_t *hp, bignum_t *a, bignum_t *b);
bignum_t *bignum_mul(heap_t *hp, bignum_t *a, bignum_t *b);
bignum_t *bignum_div(heap_t *hp, bignum_t *a, bignum_t *b, bignum_t **r);

//EOF
