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

#include <math.h>

#include "ling_common.h"

#include "term.h"
#include "bits.h"
#include "string.h"
#include "heap.h"
#include "bignum.h"
#include "atom_defs.h"
#include "code_base.h"

#define	LOG_65536	(11.09035488895912495067)

uint32_t zero = 0;	// target for all zero-length tuples

term_t bin_parent(term_t bin)
{
	assert(is_boxed(bin) && is_binary(peel_boxed(bin)));
	uint32_t *pb = peel_boxed(bin);
	if (boxed_tag(pb) == SUBTAG_SUB_BIN)
		return ((t_sub_bin_t *)pb)->parent;
	if (boxed_tag(pb) == SUBTAG_MATCH_CTX)
		return ((t_match_ctx_t *)pb)->parent;

	return bin;
}

double term_to_float(term_t t)
{
	if (is_int(t))
		return (double)int_value(t);
	if (is_boxed(t))
	{
		uint32_t *tdata = peel_boxed(t);
		if (boxed_tag(tdata) == SUBTAG_FLOAT)
			return float_value(tdata);
		else if (is_bignum(tdata))
			return bignum_to_double((bignum_t *)tdata);
	}

	double x = 1.0;
	return (x-x)/(x-x); //NaN
}

term_t float_to_int(double v, heap_t *hp)
{
	if (v > (double)MAX_INT_VALUE || v < (double)MIN_INT_VALUE)
	{
		int sign = MP_ZPOS;
		if (v < 0)
		{
			v = -v;
			sign = MP_NEG;
		}

		int ndigs = log(v)/LOG_65536+1;	// guess the number of digits
		uint16_t *digits;
		term_t r = heap_bignum(hp, sign, ndigs, &digits);
		uint16_t *dp = digits;
		const int radix = 65536;
		while (v != 0)
		{
			double vj = modf(v / radix, &v);
			*dp++ = (uint16_t)(vj * radix);
		}

		bignum_t *bn = (bignum_t *)peel_boxed(r);
		assert(dp <= digits+ndigs);
		bn->used = dp-digits;

		assert(!bignum_fits_small_int(bn));
		return r;
	}
	else
	{
		assert(fits_int((int)v));
		return tag_int((int)v);
	}
}

term_t lookup_process_dictionary(term_t key, term_t dict)
{
	assert(is_list(dict));

	term_t l = dict;
	while (is_cons(l))
	{
		uint32_t *cons = peel_cons(l);
		term_t el = cons[0];
		assert(is_tuple(el));
		uint32_t *tdata = peel_tuple(el);
		assert(*tdata == 2);
		term_t k = tdata[1];
		if (k == key || are_terms_equal(k, key, 0))
			return tdata[2];
		l = cons[1];
	}
	assert(is_nil(l));

	return A_UNDEFINED;
}

term_t fun_get_info(t_fun_t *f, term_t what, heap_t *hp)
{
	switch (what)
	{
	case A_TYPE:
		return A_LOCAL;

	case A_MODULE:
		return f->module;
	
	case A_NAME:
		if (f->fe == 0)	// module unloaded
			return nil;
		return f->fe->name;
	
	case A_ARITY:
		return tag_int(fun_arity((uint32_t *)f) - fun_num_free((uint32_t *)f));
	
	case A_ENV:
		return heap_vector_to_list(hp,
					f->frozen, fun_num_free((uint32_t *)f));
	
	case A_PID:
		return f->pid;
	
	case A_INDEX:
		return tag_int(f->old_index);
	
	case A_NEW_INDEX:
		return tag_int(f->index);
	
	case A_UNIQ:
		return tag_int(f->old_uniq);
	
	case A_NEW_UNIQ:
	{
		uint8_t *data = 0;
		term_t bin = heap_make_bin(hp, sizeof(f->uniq), &data);
		memcpy(data, f->uniq, sizeof(f->uniq));
		return bin;
	}
	default:
		return noval;
	}
}

term_t export_get_info(t_export_t *x, term_t what)
{
	switch (what)
	{
	case A_TYPE:
		return A_EXTERNAL;

	case A_MODULE:
		return x->e->module;
	
	case A_NAME:
		return x->e->function;
	
	case A_ARITY:
		return tag_int(x->e->arity);
	
	case A_ENV:
		return nil;
	
	case A_PID:
	case A_INDEX:
	case A_NEW_INDEX:
	case A_UNIQ:
	case A_NEW_UNIQ:
		return A_UNDEFINED;

	default:
		return noval;
	}
}

term_t err_to_term(int x)
{
	if (x == -NO_MEMORY)
		return A_NO_MEMORY;
	else if (x == -BAD_ARG)
		return A_BADARG;
	else if (x == -TOO_LONG)
		return A_SYSTEM_LIMIT;
	else if (x == -TOO_DEEP);
		return A_SYSTEM_LIMIT;
	assert(x == -NOT_FOUND);
	return A_NOT_FOUND;
}

//EOF
