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

#include "ling_common.h"
#include "term.h"
#include "bignum.h"
#include "string.h"
#include "atoms.h"
#include "bits.h"
#include "code_base.h"

static int is_term_smaller_1(term_t l1, term_t l2);
static int is_term_smaller_2(term_t t1, term_t t2);
static int is_term_smaller_3(uint32_t *bin1, uint32_t *bin2);
static int term_order(term_t t);

static int fun_compare(t_fun_t *f1, t_fun_t *f2);
static int export_compare(t_export_t *x1, t_export_t *x2);
static int pid_compare(t_long_pid_t *p1, t_long_pid_t *p2);
static int oid_compare(t_long_oid_t *o1, t_long_oid_t *o2);
static int ref_compare(t_long_ref_t *r1, t_long_ref_t *r2);

int are_terms_equal(term_t a, term_t b, int exact)
{
	assert(a != b);		// should be checked elsewhere

	if (is_immed(a) || is_immed(b))
	{
		if (exact)
			return 0;
		if (is_int(a) && is_boxed(b))
		{
			uint32_t *term_data = peel_boxed(b);
			return (boxed_tag(term_data) == SUBTAG_FLOAT)
				&& (double)int_value(a) == float_value(term_data);
		}
		else if (is_boxed(a) && is_int(b))
		{
			uint32_t *term_data = peel_boxed(a);
			return (boxed_tag(term_data) == SUBTAG_FLOAT)
				&& (float_value(term_data) == (double)int_value(b));
		}

		return 0;
	}

	if (is_cons(a))
	{
		if (is_cons(b))
		{
			do {
				uint32_t *cons1 = peel_cons(a);
				uint32_t *cons2 = peel_cons(b);

				if (cons1[0] != cons2[0]
						&& !are_terms_equal(cons1[0], cons2[0], exact))
					return 0;
				a = cons1[1];
				b = cons2[1];
			} while (is_cons(a) && is_cons(b));

			return (a == b) || are_terms_equal(a, b, exact);
		}
		else
			return 0;
	}
	else if (is_tuple(a))
	{
		if (is_tuple(b))
		{
			uint32_t *data1 = peel_tuple(a);
			uint32_t *data2 = peel_tuple(b);

			if (data1[0] != data2[0])
				return 0;

			for (int i = 1; i <= data1[0]; i++)
				if (data1[i] != data2[i]
						&& !are_terms_equal(data1[i], data2[i], exact))
					return 0;

			return 1;
		}
		else
			return 0;
	}
	else
	{
		assert(is_boxed(a));
		if (!is_boxed(b))
			return 0;

		uint32_t *term_data1 = peel_boxed(a);
		uint32_t *term_data2 = peel_boxed(b);

		uint32_t subtag = boxed_tag(term_data1);

		if (!exact && subtag == SUBTAG_FLOAT && is_bignum(term_data2))
			return float_value(term_data1) == bignum_to_double((bignum_t *)term_data2);

		if (!exact && is_bignum(term_data1) && boxed_tag(term_data2) == SUBTAG_FLOAT)
			return bignum_to_double((bignum_t *)term_data1) == float_value(term_data2);

		if (subtag != boxed_tag(term_data2) &&
				!(is_binary(term_data1) && is_binary(term_data2)))
			return 0;

		switch (subtag)
		{
		case SUBTAG_POS_BIGNUM:
		case SUBTAG_NEG_BIGNUM:
		{
			bignum_t *bn1 = (bignum_t *)term_data1;
			bignum_t *bn2 = (bignum_t *)term_data2;
			return bignum_compare(bn1, bn2) == 0;
		}
		case SUBTAG_FUN:
		{
			t_fun_t *f1 = (t_fun_t *)term_data1;
			t_fun_t *f2 = (t_fun_t *)term_data2;
			if (f1->module != f2->module ||
				f1->index != f2->index ||
				f1->old_uniq != f2->old_uniq)
					return 0;
			int num_free = fun_num_free(term_data1);
			assert(num_free == fun_num_free(term_data2));
			for (int i = 0; i < num_free; i++)
			{
				term_t v1 = f1->frozen[i];
				term_t v2 = f2->frozen[i];
				if (v1 != v2 && !are_terms_equal(v1, v2, exact))
					return 0;
			}
			return 1;
		}
		case SUBTAG_EXPORT:
		{
			export_t *e1 = ((t_export_t *)term_data1)->e;
			export_t *e2 = ((t_export_t *)term_data2)->e;
			return e1->module == e2->module &&
			   	   e1->function == e2->function &&
				   e1->arity == e2->arity;
		}		
		case SUBTAG_PID:
		{
			t_long_pid_t *pid1 = (t_long_pid_t *)term_data1;
			t_long_pid_t *pid2 = (t_long_pid_t *)term_data2;
			return pid1->node == pid2->node &&
				   pid1->serial == pid2->serial &&
				   opr_hdr_id(pid1) == opr_hdr_id(pid2) &&
				   opr_hdr_creat(pid1) == opr_hdr_creat(pid2);
		}
		case SUBTAG_OID:
		{
			t_long_oid_t *oid1 = (t_long_oid_t *)term_data1;
			t_long_oid_t *oid2 = (t_long_oid_t *)term_data2;
			return oid1->node == oid2->node &&
				   opr_hdr_id(oid1) == opr_hdr_id(oid2) &&
				   opr_hdr_creat(oid1) == opr_hdr_creat(oid2);
		}
		case SUBTAG_REF:
		{
			t_long_ref_t *ref1 = (t_long_ref_t *)term_data1;
			t_long_ref_t *ref2 = (t_long_ref_t *)term_data2;
			return ref1->node == ref2->node &&
				   ref1->id1 == ref2->id1 &&
				   ref1->id2 == ref2->id2 &&
				   opr_hdr_id(ref1) == opr_hdr_id(ref2) &&
				   opr_hdr_creat(ref1) == opr_hdr_creat(ref2);
		}
		case SUBTAG_PROC_BIN:
		case SUBTAG_HEAP_BIN:
		case SUBTAG_MATCH_CTX:
		case SUBTAG_SUB_BIN:
		{
			bits_t bs1, bs2;
			bits_get_real(term_data1, &bs1);
			bits_get_real(term_data2, &bs2);
			return (bits_compare(&bs1, &bs2) == 0);
		}
		default:
			assert(subtag == SUBTAG_FLOAT);
			return float_value(term_data1) == float_value(term_data2);
		}
		return 1;
	}
}

int is_term_smaller(term_t a, term_t b)
{
	if (a == b)
		return 0;

	if (are_both_immed(a, b))
	{
		if (are_both_int(a, b))
			return int_value(a) < int_value(b);

		if (is_int(a))	// !is_int(b)
			return 1;

		if (is_nil(a))	// !is_nil(b)
			return 0;
		if (is_nil(b))	// !is_nil(a)
			return 1;

		if (is_atom(a))
		{
			if (is_int(b))
				return 0;
			else if (is_atom(b))
			{
				uint8_t *print1 = atoms_get(atom_index(a));
				uint8_t *print2 = atoms_get(atom_index(b));
				int short_len = (print1[0] < print2[0])
					? print1[0]
					: print2[0];
				int d = memcmp(print1+1, print2+1, short_len);
				if (d == 0)
					return print1[0] < print2[0];
				return d < 0;
			}
			else
				return 1;
		}
		else if (is_short_oid(a))
		{
			if (is_int(b) || is_atom(b))
				return 0;
			else if (is_short_oid(b))
				return short_oid_id(a) < short_oid_id(b);
			else
				return 1;
		}
		else if (is_short_pid(a))
		{
			if (is_int(b) || is_atom(b) || is_short_oid(b))
				return 0;
			else
			{
				assert(is_short_pid(b));
				return short_pid_id(a) < short_pid_id(b);
			}
		}
	}

	//TODO: comparison of bignum and float: docs mention the
	// number 9007199254740992.0 and a loss of transitivity
	
	if (!is_immed(a) && !is_immed(b) &&
				primary_tag(a) == primary_tag(b))
	{
		if (is_cons(a))
			return is_term_smaller_1(a, b);
		else if (is_tuple(a))
			return is_term_smaller_2(a, b);
		else
		{
			assert(is_boxed(a) && is_boxed(b));
			uint32_t *adata = peel_boxed(a);
			uint32_t *bdata = peel_boxed(b);
			if (boxed_tag(adata) == boxed_tag(bdata) ||
					(is_binary(adata) && is_binary(bdata)) ||
					(is_bignum(adata) && is_bignum(bdata)))
			{
				switch(boxed_tag(adata))
				{
				case SUBTAG_POS_BIGNUM:
				case SUBTAG_NEG_BIGNUM:
					return bignum_compare((bignum_t *)adata,
										  (bignum_t *)bdata) < 0;
				case SUBTAG_FUN:
					return fun_compare((t_fun_t *)adata,
									   (t_fun_t *)bdata) < 0;
				case SUBTAG_EXPORT:
					return export_compare((t_export_t *)adata,
									   	  (t_export_t *)bdata) < 0;

				case SUBTAG_PID:
					return pid_compare((t_long_pid_t *)adata,
									   (t_long_pid_t *)bdata) < 0;

				case SUBTAG_OID:
					return oid_compare((t_long_oid_t *)adata,
									   (t_long_oid_t *)bdata) < 0;

				case SUBTAG_REF:
					return ref_compare((t_long_ref_t *)adata,
									   (t_long_ref_t *)bdata) < 0;

				case SUBTAG_PROC_BIN:
				case SUBTAG_HEAP_BIN:
				case SUBTAG_MATCH_CTX:
				case SUBTAG_SUB_BIN:
					return is_term_smaller_3(adata, bdata);

				default:
					assert(boxed_tag(adata) == SUBTAG_FLOAT);
					return float_value(adata) < float_value(bdata);
				}
			}
		}
	}

	// Number comparison with (mandatory) coercion
	//
	int use_float = (is_boxed(a) && boxed_tag(peel_boxed(a)) == SUBTAG_FLOAT) ||
					(is_boxed(b) && boxed_tag(peel_boxed(b)) == SUBTAG_FLOAT);

	if (use_float)
	{
		if (is_int(a))	// b is always float
			return (double)int_value(a) < float_value(peel_boxed(b));
		else if (is_boxed(a))
		{
			uint32_t *adata = peel_boxed(a);
			if (is_bignum(adata))	// b is always float
				return bignum_to_double((bignum_t *)adata) < float_value(peel_boxed(b));

			if (boxed_tag(adata) == SUBTAG_FLOAT)
			{
				if (is_int(b))
					return float_value(adata) < (double)int_value(b);
				if (is_boxed(b))
				{
					uint32_t *bdata = peel_boxed(b);
					if (is_bignum(bdata))
						return float_value(adata) < bignum_to_double((bignum_t *)bdata);
				}
			}
		}
	}
	else	// use integer
	{
		if (is_int(a))
		{
			if (is_boxed(b))
			{
				uint32_t *bdata = peel_boxed(b);
				if (is_bignum(bdata))
				{
					bignum_t *bbn = (bignum_t *)bdata;
					return !bignum_is_neg(bbn);
				}
				assert(boxed_tag(bdata) != SUBTAG_FLOAT);
			}
		}
		else if (is_boxed(a))
		{
			uint32_t *adata = peel_boxed(a);
			if (is_bignum(adata))
			{
				bignum_t *abn = (bignum_t *)adata;
				if (is_int(b))
					return bignum_is_neg(abn);

				if (is_boxed(b))
				{
					uint32_t *bdata = peel_boxed(b);
					if (is_bignum(bdata))
						return bignum_compare(abn, (bignum_t *)bdata);
					assert(boxed_tag(bdata) != SUBTAG_FLOAT);
				}
			}

			assert(boxed_tag(adata) != SUBTAG_FLOAT);
		}
	}

	// a and b are quaranteed to have different types
	// 
	
	return term_order(a) < term_order(b);
}

static int is_term_smaller_1(term_t l1, term_t l2)
{
	assert(is_cons(l1) && is_cons(l2));

	do {
		term_t *cons1 = peel_cons(l1);
		term_t *cons2 = peel_cons(l2);
		if (is_term_smaller(cons1[0], cons2[0]))
			return 1;
		if (is_term_smaller(cons2[0], cons1[0]))
			return 0;
		l1 = cons1[1];
		l2 = cons2[1];
	} while (is_cons(l1) && is_cons(l2));

	return is_term_smaller(l1, l2);
}

static int is_term_smaller_2(term_t t1, term_t t2)
{
	assert(is_tuple(t1) && is_tuple(t2));
	uint32_t *tdata1 = peel_tuple(t1);
	uint32_t *tdata2 = peel_tuple(t2);

	if (*tdata1 < *tdata2)
		return 1;
	if (*tdata1 > *tdata2)
		return 0;

	for (int i = 1; i <= *tdata1; i++)
	{
		if (is_term_smaller(tdata1[i], tdata2[i]))
			return 1;
		if (is_term_smaller(tdata2[i], tdata1[i]))
			return 0;
	}

	return 0;
}

static int is_term_smaller_3(uint32_t *bin1, uint32_t *bin2)
{
	assert(is_binary(bin1) && is_binary(bin2));
	bits_t bs1, bs2;

	bits_get_real(bin1, &bs1);
	bits_get_real(bin2, &bs2);

	return (bits_compare(&bs1, &bs2) < 0);
}

// number < atom < reference < fun < oid < pid < tuple < empty_list < list < binary
#define TERM_ORDER_NUMBER	0
#define TERM_ORDER_ATOM		1
#define TERM_ORDER_REF		2
#define TERM_ORDER_FUN		3
#define TERM_ORDER_EXPORT	4
#define TERM_ORDER_OID		5
#define TERM_ORDER_PID		6
#define TERM_ORDER_TUPLE	7
#define TERM_ORDER_NIL		8
#define TERM_ORDER_CONS		9
#define TERM_ORDER_BINARY	10

static int term_order(term_t t)
{
	if (is_cons(t))
		return TERM_ORDER_CONS;
	if (is_tuple(t))
		return TERM_ORDER_TUPLE;
	if (is_nil(t))
		return TERM_ORDER_NIL;
	if (is_int(t))
		return TERM_ORDER_NUMBER;
	if (is_atom(t))
		return TERM_ORDER_ATOM;
	if (is_short_pid(t))
		return TERM_ORDER_PID;
	if (is_short_oid(t))
		return TERM_ORDER_OID;
	assert(is_boxed(t));
	switch (boxed_tag(peel_boxed(t)))
	{
	case SUBTAG_POS_BIGNUM:
	case SUBTAG_NEG_BIGNUM:
	case SUBTAG_FLOAT:
		return TERM_ORDER_NUMBER;

	case SUBTAG_FUN:
		return TERM_ORDER_FUN;
	case SUBTAG_EXPORT:
		return TERM_ORDER_EXPORT;

	case SUBTAG_PID:
		return TERM_ORDER_PID;

	case SUBTAG_OID:
		return TERM_ORDER_OID;

	case SUBTAG_REF:
		return TERM_ORDER_REF;

	case SUBTAG_PROC_BIN:
	case SUBTAG_HEAP_BIN:
	case SUBTAG_MATCH_CTX:
	case SUBTAG_SUB_BIN:
		return TERM_ORDER_BINARY;

	default:
		fatal_error("subtag");
	}
}

static int fun_compare(t_fun_t *f1, t_fun_t *f2)
{
	if (f1->old_uniq < f2->old_uniq)
		return -1;
	if (f1->old_uniq > f2->old_uniq)
		return 1;
	if (f1->old_index < f2->old_index)
		return -1;
	if (f1->old_index > f2->old_index)
		return 1;
	int i = 0;
	int n1 = fun_num_free((uint32_t *)f1);
	int n2 = fun_num_free((uint32_t *)f2);
	while (i < n1 && i < n2)
	{
		if (is_term_smaller(f1->frozen[i], f2->frozen[i]))
			return -1;
		if (is_term_smaller(f2->frozen[i], f1->frozen[i]))
			return 1;
		i++;
	}
	if (n1 < n2)
		return -1;
	if (n1 > n2)
		return 1;

	return 0;
}

static int export_compare(t_export_t *x1, t_export_t *x2)
{
	if (is_term_smaller(x1->e->module, x2->e->module))
		return -1;
	if (is_term_smaller(x2->e->module, x1->e->module))
		return 1;
	if (is_term_smaller(x1->e->function, x2->e->function))
		return -1;
	if (is_term_smaller(x2->e->function, x1->e->function))
		return 1;
	if (x1->e->arity < x2->e->arity)
		return -1;
	if (x1->e->arity > x2->e->arity)
		return 1;

	return 0;
}

static int pid_compare(t_long_pid_t *p1, t_long_pid_t *p2)
{
	if (is_term_smaller(p1->node, p2->node))
		return -1;
	if (is_term_smaller(p2->node, p1->node))
		return 1;
	uint32_t creat1 = opr_hdr_creat(p1);
	uint32_t creat2 = opr_hdr_creat(p2);
	if (creat1 < creat2)
		return -1;
	if (creat2 < creat1)
		return 1;
	uint32_t id1 = opr_hdr_id(p1);
	uint32_t id2 = opr_hdr_id(p2);
	if (id1 < id2)
		return -1;
	if (id2 < id1)
		return 1;
	if (p1->serial < p2->serial)
		return -1;
	if (p2->serial < p1->serial)
		return 1;

	return 0;
}

static int oid_compare(t_long_oid_t *o1, t_long_oid_t *o2)
{
	if (is_term_smaller(o1->node, o2->node))
		return -1;
	if (is_term_smaller(o2->node, o1->node))
		return 1;
	uint32_t creat1 = opr_hdr_creat(o1);
	uint32_t creat2 = opr_hdr_creat(o2);
	if (creat1 < creat2)
		return -1;
	if (creat2 < creat1)
		return 1;
	uint32_t id1 = opr_hdr_id(o1);
	uint32_t id2 = opr_hdr_id(o2);
	if (id1 < id2)
		return -1;
	if (id2 < id1)
		return 1;

	return 0;
}

static int ref_compare(t_long_ref_t *r1, t_long_ref_t *r2)
{
	if (is_term_smaller(r1->node, r2->node))
		return -1;
	if (is_term_smaller(r2->node, r1->node))
		return 1;
	uint32_t creat1 = opr_hdr_creat(r1);
	uint32_t creat2 = opr_hdr_creat(r2);
	if (creat1 < creat2)
		return -1;
	if (creat2 < creat1)
		return 1;
	uint32_t id1 = opr_hdr_id(r1);
	uint32_t id2 = opr_hdr_id(r2);
	if (id1 < id2)
		return -1;
	if (id2 < id1)
		return 1;
	if (r1->id1 < r2->id1)
		return -1;
	if (r2->id1 < r1->id1)
		return 1;
	if (r1->id2 < r2->id2)
		return -1;
	if (r2->id2 < r1->id2)
		return 1;

	return 0;
}

//EOF
