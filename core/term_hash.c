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

#include "term_util.h"

#include "ling_common.h"

#include "atoms.h"
#include "bits.h"
#include "getput.h"
#include "code_base.h"

const uint32_t C1 = 268440163;
const uint32_t C2 = 268439161;
const uint32_t C3 = 268435459;
const uint32_t C4 = 268436141;
const uint32_t C5 = 268438633;
const uint32_t C6 = 268437017;
const uint32_t C7 = 268438039;
const uint32_t C8 = 268437511;
const uint32_t C9 = 268439627;
const uint32_t C10 = 268440479;
const uint32_t C11 = 268440577;
const uint32_t C12 = 268440581;

// the old hash function described in erl_spec47.ps; used by erlang:hash/2
uint32_t simple_hash(term_t t, uint32_t h)
{
	if (is_immed(t))
	{
		if (is_nil(t))
			return C3 * h +1;
		if (is_int(t))
			return C2 * h + int_value(t);
		else if (is_atom(t))
		{
			uint8_t *print_name = atoms_get(atom_index(t));

			uint32_t a = 0;
			for (int i = 1; i <= print_name[0]; i++)
			{
				uint32_t j = (a << 4) + print_name[i];
				a = (j & 0x0fffffff) ^ ((j >> 24) & 0xf0);
			}
			return C1 * h + a;
		}
		else if (is_short_pid(t))
			return C5 * h + short_pid_id(t);
		else
		{
			assert(is_short_oid(t));
			return C9 * h + short_oid_id(t);
		}
	}
	else if (is_cons(t))
	{
		do {
			term_t *cons = peel_cons(t);
			h = simple_hash(cons[0], h);
			t = cons[1];
		} while (is_cons(t));
		return C8 * simple_hash(t, h);
	}
	else if (is_tuple(t))
	{
		uint32_t *tdata = peel_tuple(t);
		for (int i = 1; i <= tdata[0]; i++)
			h = simple_hash(tdata[i], h);
		return C9 * h + tdata[0];
	}
	else
	{
		assert(is_boxed(t));
		uint32_t *tdata = peel_boxed(t);
		switch (boxed_tag(tdata))
		{
		case SUBTAG_POS_BIGNUM:
		case SUBTAG_NEG_BIGNUM:
		{
			bignum_t *bn = (bignum_t *)tdata;
			uint32_t C = (bn->sign == MP_ZPOS) ?C3 :C2;

			uint16_t *dp = bn->dp;
			uint16_t *end = dp + bn->used;
			int k = 0;
			while (dp < end)
			{
				uint32_t l = *dp++;
				uint32_t h = (dp < end) ?*dp++ :0;
				uint32_t w = (h << 16) | l;
				h = C2 * h + w;
				k++;
			}

			return C * h + k;
		}
		case SUBTAG_FLOAT:
		{
			union {
				double d;
				uint32_t w[2];
			} u;
			u.d = float_value(tdata);
			return C6 * h + (u.w[0] ^ u.w[1]);
		}
		case SUBTAG_FUN:
		{
			t_fun_t *fun = (t_fun_t *)tdata;
			int num_free = fun_num_free(tdata);
			h = C10 * h + num_free;
			h = simple_hash(fun->module, h);
			h = C2 * h + fun->old_index;
			h = C2 * h + fun->old_uniq;
			for (int i = 0; i < num_free; i++)
				h = simple_hash(fun->frozen[i], h);
			return h;
		}
		case SUBTAG_EXPORT:
		{
			t_export_t *exp = (t_export_t *)tdata;
			h = C11 * h + exp->e->arity;
			h = simple_hash(exp->e->module, h);
			return simple_hash(exp->e->function, h);
		}
		case SUBTAG_PID:
			//TODO
		case SUBTAG_OID:
			//TODO
		case SUBTAG_REF:
			//TODO
			not_implemented("hash long pid/oid/ref");
		}

		assert(is_binary(tdata));

		bits_t bs;
		bits_get_real(tdata, &bs);
		if (((bs.ends - bs.starts) & 7) != 0)
			return 0;	//not defined for bitstrings

		if ((bs.starts & 7) == 0)
		{
			int data_size = (bs.ends - bs.starts) /8;
			if (data_size > 15)
				data_size = 15;
			uint8_t *data = bs.data + bs.starts /8;
			for (int i = 0; i < data_size; i++)
				h = C1 * h + data[i];
			return C4 * h + data_size;
		}
		else
		{
			int k = 0;
			while (k < 15 && bits_has_octet(&bs))
			{
				uint8_t b;
			   	bits_get_octet(&bs, b);
				h = C1 * h + b;
				k++;
			}
			return C4 * h + k;
		}
	}
}

// a better hash function used by phash/2
uint32_t portable_hash(term_t t, uint32_t h)
{
	if (is_immed(t))
	{
		if (is_nil(t))
			return C3 * h +1;
		if (is_int(t))
		{
			int v = int_value(t);
			uint32_t y = (v < 0) ?C4: C3;
			if (v < 0)
				v = -v;
			uint8_t quad[4];
			PUT_UINT_32_LE(quad, v);
			for (int i = 0; i < 4; i++)
				h = C2 * h + quad[i];
			return y * h;
		}
		else if (is_atom(t))
		{
			uint8_t *print_name = atoms_get(atom_index(t));

			uint32_t a = 0;
			for (int i = 1; i <= print_name[0]; i++)
			{
				uint32_t j = (a << 4) + print_name[i];
				a = (j & 0x0fffffff) ^ ((j >> 24) & 0xf0);
			}
			return C1 * h + a;
		}
		else if (is_short_pid(t))
			return C5 * h + short_pid_id(t);
		else
		{
			assert(is_short_oid(t));
			return C9 * h + short_oid_id(t);
		}
	}
	else if (is_cons(t))
	{
		do {
			term_t *cons = peel_cons(t);
			h = portable_hash(cons[0], h);
			t = cons[1];
		} while (is_cons(t));
		return C8 * portable_hash(t, h);
	}
	else if (is_tuple(t))
	{
		uint32_t *tdata = peel_tuple(t);
		for (int i = 1; i <= tdata[0]; i++)
			h = portable_hash(tdata[i], h);
		return C9 * h + tdata[0];
	}
	else
	{
		assert(is_boxed(t));
		uint32_t *tdata = peel_boxed(t);
		switch (boxed_tag(tdata))
		{
		case SUBTAG_POS_BIGNUM:
		case SUBTAG_NEG_BIGNUM:
		{
			bignum_t *bn = (bignum_t *)tdata;
			uint32_t y = (bn->sign == MP_ZPOS) ?C3 :C2;

			uint16_t *dp = bn->dp;
			uint16_t *end = dp + bn->used;
			int j = (bn->used +1) /2;
			while (j > 0)
			{
				uint8_t duo[2];
				uint16_t d1 = (dp < end) ?*dp++ :0;
				PUT_UINT_16_LE(duo, d1);
				h = C2 * h + duo[0];
				h = C2 * h + duo[1];

				uint16_t d2 = (dp < end) ?*dp++ :0;
				PUT_UINT_16_LE(duo, d2);
				h = C2 * h + duo[0];
				h = C2 * h + duo[1];

				j--;
			}

			return y * h;
		}
		case SUBTAG_FLOAT:
		{
			union {
				double d;
				uint32_t w[2];
			} u;
			u.d = float_value(tdata);
			return C6 * h + (u.w[0] ^ u.w[1]);
		}
		case SUBTAG_FUN:
		{
			t_fun_t *fun = (t_fun_t *)tdata;
			int num_free = fun_num_free(tdata);
			h = C10 * h + num_free;
			h = portable_hash(fun->module, h);
			h = C2 * h + fun->old_index;
			h = C2 * h + fun->old_uniq;
			for (int i = 0; i < num_free; i++)
				h = portable_hash(fun->frozen[i], h);
			return h;
		}
		case SUBTAG_EXPORT:
		{
			t_export_t *exp = (t_export_t *)tdata;
			h = C11 * h + exp->e->arity;
			h = portable_hash(exp->e->module, h);
			return portable_hash(exp->e->function, h);
		}
		case SUBTAG_PID:
		{
			//
			//XXX: improvisation
			//
			t_long_pid_t *p = (t_long_pid_t *)tdata;
#ifdef LING_XEN
			h = C1 * h + p->boxid;
			h = C2 * h + p->domid;
			return C6 *h + long_pid_id(p);
#else /* !LING_XEN */
			h = C1 * h + opr_hdr_id(tdata);
			h = C2 * h + opr_hdr_creat(tdata);
			h = portable_hash(p->node, h);
			return C6 * h + p->serial;
#endif
		}
		case SUBTAG_OID:
		{
			//
			//XXX: improvisation
			//
			t_long_oid_t *p = (t_long_oid_t *)tdata;
			h = C1 * h + opr_hdr_id(tdata);
			h = C2 * h + opr_hdr_creat(tdata);
			h = portable_hash(p->node, h);
			return C7 * h;
		}
		case SUBTAG_REF:
		{
			//
			//XXX: improvisation
			//
			t_long_ref_t *p = (t_long_ref_t *)tdata;
			h = C1 * h + opr_hdr_id(tdata);
			h = C2 * h + opr_hdr_creat(tdata);
			h = portable_hash(p->node, h);
			h = C8 * h + p->id1;
			return C8 * h + p->id2;
		}
		}

		assert(is_binary(tdata));

		bits_t bs;
		bits_get_real(tdata, &bs);
		if (((bs.ends - bs.starts) & 7) != 0)
			return 0;	//not defined for bitstrings

		if ((bs.starts & 7) == 0)
		{
			int data_size = (bs.ends - bs.starts) /8;
			uint8_t *data = bs.data + bs.starts /8;
			for (int i = 0; i < data_size; i++)
				h = C1 * h + data[i];
			return C4 * h + data_size;
		}
		else
		{
			int k = 0;
			while (bits_has_octet(&bs))
			{
				uint8_t b;
			   	bits_get_octet(&bs, b);
				h = C1 * h + b;
				k++;
			}
			return C4 * h + k;
		}
	}
}

// an even better hash function used by phash2/2
uint32_t portable_hash2(term_t t, uint32_t h)
{
	//TODO
	return h;
}

//EOF
