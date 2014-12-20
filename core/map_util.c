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

#include "map_util.h"

#include "ling_common.h"

int map_key_index(term_t k, term_t keys)
{
	assert(is_tuple(keys));
	term_t *p = peel_tuple(keys);
	uint32_t size = *p++;
	if (size == 0)
		return -1;
	term_t *alpha = p;
	term_t *beta = p +size;
	while (beta > alpha+1)
	{
		term_t *mid = alpha + (beta -alpha +1)/2;
		if (is_term_smaller(k, *mid))
			beta = mid;
		else
			alpha = mid;
	}
	assert(beta == alpha+1);
	if (!are_terms_equal(k, *alpha, 1))
		return -1;
	return alpha -p;
}

int map_merge(term_t *ks, term_t *vs, int n,
			  term_t *kvs, term_t nkvs,
			  term_t *ks1, term_t *vs1)
{
	int sz = 0;
	while (n > 0 && nkvs > 0)
	{
		term_t a = *ks;
		term_t b = *kvs;
		if (is_term_smaller(a, b))
		{
			*ks1++ = *ks++;
			*vs1++ = *vs++;
			n--;
		}
		else if (are_terms_equal(a, b, 1))
		{
			ks++; vs++;
			n--;
			*ks1++ = *kvs++;
			*vs1++ = *kvs++;
			nkvs--;
		}
		else
		{
			*ks1++ = *kvs++;
			*vs1++ = *kvs++;
			nkvs--;
		}
		sz++;
	}

	while (n-- > 0)
	{
		*ks1++ = *ks++;
		*vs1++ = *vs++;
		sz++;
	}

	while (nkvs-- > 0)
	{
		*ks1++ = *kvs++;
		*vs1++ = *kvs++;
		sz++;
	}

	return sz;
}

int map_merge_exact(term_t *ks, term_t *vs, int n,
					term_t *kvs, int nkvs,
					term_t *vs1)
{
	int sz = 0;
	while (n > 0 && nkvs > 0)
	{
		term_t a = *ks;
		term_t b = *kvs;
		if (is_term_smaller(a, b))
		{
			ks++;
			*vs1++ = *vs++;
			n--;
		}
		else if (are_terms_equal(a, b, 1))
		{
			ks++; vs++;
			n--;
			kvs++;
			*vs1++ = *kvs++;
			nkvs--;
		}
		else
			return -1;

		sz++;
	}

	while (n-- > 0)
	{
		*vs1++ = *vs++;
		sz++;
	}

	if (nkvs > 0)
		return -1;

	return sz;
}

