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

#include "ets.h"

#include <string.h>
#include <math.h>
#include <stdlib.h>

#include "ling_common.h"

#include "atoms.h"
#include "atom_defs.h"
#include "term.h"
#include "term_util.h"
#include "heap.h"
#include "list_util.h"
#include "cluster.h"
#include "mixed.h"
#include "bits.h"

#define MATCH	0
#define NOMATCH	-1

enum pat_op_t {
	mpo_bind_top_var = 200,
	mpo_skip_top_var,
	mpo_bind_var,
	mpo_skip_var,
	mpo_literal,
	mpo_cons,
	mpo_tuple,
};

typedef struct match_pat_t match_pat_t;
struct match_pat_t {
	enum pat_op_t op;
	match_pat_t *inner;
	union {
		int var;
		term_t lit;
		int arity;
	};
	match_pat_t *next;
};

enum exp_op_t {
	meo_var = 100,
	meo_all_bound,
	meo_matched,
	meo_make_tuple,
	meo_make_cons,
	meo_lit,
	meo_is_atom,
	meo_is_float,
	meo_is_integer,
	meo_is_list,
	meo_is_number,
	meo_is_pid,
	meo_is_port,
	meo_is_reference,
	meo_is_tuple,
	meo_is_binary,
	meo_is_function,
	meo_is_record,
	meo_and,
	meo_or,
	meo_not,
	meo_xor,
	meo_andalso,
	meo_orelse,
	meo_abs,
	meo_element,
	meo_hd,
	meo_length,
	meo_node,
	meo_round,
	meo_size,
	meo_tl,
	meo_trunc,
	meo_plus,
	meo_minus,
	meo_mult,
	meo_int_div,
	meo_div,
	meo_rem,
	meo_band,
	meo_bor,
	meo_bxor,
	meo_bnot,
	meo_bsl,
	meo_bsr,
	meo_gt,
	meo_ge,
	meo_lt,
	meo_le,
	meo_eqex,
	meo_eq,
	meo_neex,
	meo_ne,
	meo_self,
	meo_none,
};

typedef struct match_exp_t match_exp_t;
struct match_exp_t {
	enum exp_op_t op;
	union {
		match_exp_t *arg;	// is_atom, is_float, is_integer, is_list,
							// is_number, is_pid, is_port, is_reference,
							// is_tuple, is_binary, is_function,
							// not, abs, hd, length, node, round, size,
							// tl, trunc, and, or, andalso, orelse
	struct {
		match_exp_t *rec;	// is_record
		term_t rtype;
		int rsize;
	};

	struct {
		match_exp_t *left;	// xor, plus, minus,
		match_exp_t *right;	// mult, int_div, div, rem, band, bor, bxor,
	};						// bsl, bsr, gt, ge, lt, le, eqex, eq, neex,
							// ne, make_cons
	struct {				
		match_exp_t *elts;	// make_tuple
		int arity;
	};

		int var;			// var
		term_t lit;			// lit
	};

	match_exp_t *next;
};

struct ets_match_spec_t {
	match_pat_t *head;
	match_exp_t *guards;
	match_exp_t *body;
	int nr_vars;
	ets_match_spec_t *next;
};

typedef struct eval_ctx_t eval_ctx_t;
struct eval_ctx_t {
	term_t *top_elts;
	int top_arity;
	term_t self;
	int in_body;
	term_t *vars;
	term_t *mvars;
	int nr_vars;
};

typedef struct comp_info_t comp_info_t;
struct comp_info_t {
	int var_names[ETS_MATCH_MAX_NAMES];
	int nr_vars;
	int nr_pats;
	int nr_exps;
	void *mem;
};

static int match_head(match_pat_t *pat, term_t *val, int nv, term_t *vars, heap_t *hp);
static term_t eval_match_exp(match_exp_t *exp, eval_ctx_t *ctx, heap_t *hp);
static int compile_spec1(term_t spec, comp_info_t *ci);
static int compile_pattern_top1(term_t top, comp_info_t *ci);
static int compile_pattern1(term_t *elts, int arity, comp_info_t *ci);
static int compile_pat1(term_t pat, comp_info_t *ci);
static int compile_exp_list1(term_t exps, comp_info_t *ci);
static int compile_exp1(term_t e, comp_info_t *ci);
static ets_match_spec_t *compile_spec2(term_t spec, comp_info_t *ci);
static match_pat_t *compile_pattern_top2(term_t top, comp_info_t *ci);
static match_pat_t *compile_pattern2(term_t *elts, int arity, comp_info_t *ci);
static match_pat_t *compile_pat2(term_t pat, comp_info_t *ci);
static match_exp_t *compile_exp_list2(term_t exps, comp_info_t *ci);
static match_exp_t *compile_exp2(term_t exp, comp_info_t *ci);

term_t ets_match_spec_run(ets_match_spec_t *mspec,
		term_t *elts, int arity, term_t pid, heap_t *hp)
{
	while (mspec != 0)
	{
		term_t vars[mspec->nr_vars];
		term_t mvars[mspec->nr_vars];

		for (int i = 0; i < mspec->nr_vars; i++)
		{
			vars[i] = noval;
			mvars[i] = noval;
		}

		eval_ctx_t evc = {
			.top_elts = elts,
			.top_arity = arity,
			.self = pid,
			.in_body = 0,
			.vars = vars,
			.mvars = mvars,
			.nr_vars = mspec->nr_vars,
		};

		if (match_head(mspec->head, elts, arity, vars, hp) == MATCH)
		{
			match_exp_t *ge = mspec->guards;
			int guards_ok = 1;
			while (ge != 0)
			{
				if (eval_match_exp(ge, &evc, hp) != A_TRUE)
				{
					guards_ok = 0;
					break;
				}
				ge = ge->next;
			}

			if (guards_ok)
			{
				evc.in_body = 1;
				match_exp_t *be = mspec->body;
				// evaluate only the last body expr
				while (be->next != 0)
					be = be->next;
				term_t r = eval_match_exp(be, &evc, hp);
				if (r == noval)
					return AEXIT__;
				return r;
			}
		}

		mspec = mspec->next;
	}

	return noval;	// no match
}

static int match_head(match_pat_t *pat, term_t *val, int nv, term_t *vars, heap_t *hp)
{
	while (nv > 0 && pat != 0)
   	{
	switch (pat->op)
	{
	case mpo_bind_top_var:
	{
		if (nv == 0)
			return ZERO_TUPLE;
		// the only place the heap is touch when matching
		uint32_t *p = heap_alloc(hp, 1 +nv);
		heap_set_top(hp, p +1 +nv);
		p[0] = nv;
		memcpy(p +1, val, nv *sizeof(term_t));
		vars[pat->var] = tag_tuple(p);
		return MATCH;	
	}
	case mpo_skip_top_var:
		return MATCH;

	case mpo_bind_var:
	{
		term_t bound = vars[pat->var];
		if (bound == noval)
			vars[pat->var] = *val;
		else if (bound != *val && !are_terms_equal(bound, *val, 1))		// =:=
			return NOMATCH;
		break;
	}
	case mpo_skip_var:
		break;
	case mpo_cons:
	{
		if (!is_cons(*val))
			return NOMATCH;
		if (match_head(pat->inner, peel_cons(*val), 2, vars, hp) == NOMATCH)
			return NOMATCH;
		break;
	}
	case mpo_tuple:
	{
		if (!is_tuple(*val))
			return NOMATCH;
		uint32_t *p = peel_tuple(*val);
		int arity = *p++;
		if (arity != pat->arity)
			return NOMATCH;
		if (match_head(pat->inner, p, arity, vars, hp) == NOMATCH)
			return NOMATCH;
		break;
	}
	default:
		assert(pat->op == mpo_literal);
		if (*val != pat->lit && !are_terms_equal(*val, pat->lit, 1))	// =:=
			return NOMATCH;
		break;
	}

	pat = pat->next;
	val++;
	nv --;
	}

	if (nv == 0 && pat == 0)
		return MATCH;

	return NOMATCH;
}

static term_t eval_match_exp(match_exp_t *exp, eval_ctx_t *ctx, heap_t *hp)
{
	assert(exp != 0);
	switch (exp->op)
	{
	case meo_var:
	{
		if (ctx->in_body)
		{
			term_t *mp = ctx->mvars + exp->var;
			if (*mp == noval)
			{
				assert(ctx->vars[exp->var] != noval);
				*mp = ctx->vars[exp->var];
				int x = heap_copy_terms_N(hp, mp, 1);
				assert(x != -TOO_DEEP); // heap_copy_terms is recursive
				if (x < 0)
				{
					assert(x == -NO_MEMORY);
					no_memory_signal();
				}
			}
			return *mp;
		}
		else
		{
			assert(ctx->vars[exp->var] != noval);
			return ctx->vars[exp->var];
		}
	}
	case meo_all_bound:
	{
		term_t all = nil;
		for (int i = ctx->nr_vars -1; i >= 0; i--)
		{
			assert(ctx->vars[i] != noval);
			if (ctx->in_body)
			{
				term_t *mp = ctx->mvars + i;
				if (*mp == noval)
				{
					*mp = ctx->vars[i];
					int x = heap_copy_terms_N(hp, mp, 1);
					assert(x != -TOO_DEEP); // heap_copy_terms is recursive
					if (x < 0)
					{
						assert(x == -NO_MEMORY);
						no_memory_signal();
					}
				}
				all = heap_cons(hp, *mp, all);
			}
			else
				all = heap_cons(hp, ctx->vars[i], all);
		}
		return all;
	}
	case meo_matched:
	{
		int arity = ctx->top_arity;
		if (arity == 0)
			return ZERO_TUPLE;

		if (ctx->in_body)
		{
			term_t new_elts[arity];
			memcpy(new_elts, ctx->top_elts, arity *sizeof(term_t));
			int x = heap_copy_terms_N(hp, new_elts, arity);
			assert(x != -TOO_DEEP); // heap_copy_terms is recursive
			if (x < 0)
			{
				assert(x == -NO_MEMORY);
				no_memory_signal();
			}
			uint32_t *p = heap_alloc(hp, 1 +arity);
			heap_set_top(hp, p +1 +arity);
			p[0] = arity;
			memcpy(p +1, new_elts, arity *sizeof(term_t));
			return tag_tuple(p);
		}
		else
		{
			uint32_t *p = heap_alloc(hp, 1 +arity);
			heap_set_top(hp, p +1 +arity);
			p[0] = arity;
			memcpy(p +1, ctx->top_elts, arity *sizeof(term_t));
			return tag_tuple(p);
		}
	}
	case meo_make_tuple:
	{
		if (exp->arity == 0)
			return ZERO_TUPLE;
		term_t new_elts[exp->arity];
		match_exp_t *elt = exp->elts;
		for (int i = 0; i < exp->arity; i++)
		{
			term_t val = eval_match_exp(elt, ctx, hp);
			if (val == noval)
				return noval;
			new_elts[i] = val;
			elt = elt->next;
		}
		assert(elt == 0);

		term_t *p = heap_alloc(hp, 1 +exp->arity);
		heap_set_top(hp, p +1 +exp->arity);
		p[0] = exp->arity;
		memcpy(p +1, new_elts, exp->arity *sizeof(term_t));
		return tag_tuple(p);
	}
	case meo_make_cons:
	{
		term_t hd = eval_match_exp(exp->left, ctx, hp);
		if (hd == 0)
			return noval;
		term_t tl = eval_match_exp(exp->right, ctx, hp);
		if (tl == noval)
			return noval;
		return heap_cons(hp, hd, tl);
	}
	case meo_lit:
		return exp->lit;
	case meo_is_atom:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (is_atom(val)) ?A_TRUE :A_FALSE;
	}
	case meo_is_float:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (is_boxed_float(val)) ?A_TRUE :A_FALSE;
	}
	case meo_is_integer:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (is_int(val) || is_boxed_bignum(val)) ?A_TRUE :A_FALSE;
	}
	case meo_is_list:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (is_list(val)) ?A_TRUE :A_FALSE;
	}
	case meo_is_number:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (is_int(val) || is_boxed_bignum(val) || is_boxed_float(val))
								?A_TRUE :A_FALSE;
	}
	case meo_is_pid:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (is_short_pid(val) || is_boxed_pid(val)) ?A_TRUE :A_FALSE;
	}
	case meo_is_port:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (is_short_oid(val) || is_boxed_oid(val)) ?A_TRUE :A_FALSE;
	}
	case meo_is_reference:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (is_boxed_ref(val)) ?A_TRUE :A_FALSE;
	}
	case meo_is_tuple:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (is_tuple(val)) ?A_TRUE :A_FALSE;
	}
	case meo_is_binary:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (is_boxed_binary(val)) ?A_TRUE :A_FALSE;
	}
	case meo_is_function:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (is_boxed_fun(val)) ?A_TRUE :A_FALSE;
	}
	case meo_is_record:
	{
		term_t val = eval_match_exp(exp->rec, ctx, hp);
		if (val == noval)
			return val;
		if (!is_tuple(val))
			return A_FALSE;
		term_t *p = peel_tuple(val);
		if (*p != exp->rsize || p[1] != exp->rtype)
			return A_FALSE;
		return A_TRUE;
	}
	case meo_and:
	{
		match_exp_t *me = exp->arg;
		assert(me != 0);
		term_t res = A_TRUE;
		do {
			term_t val = eval_match_exp(me, ctx, hp);
			if (val == noval)
				return noval;
			if (val != A_TRUE)
				res = A_FALSE;
			me = me->next;
		} while (me != 0);
		return res;
	}
	case meo_or:
	{
		match_exp_t *me = exp->arg;
		assert(me != 0);
		term_t res = A_FALSE;
		do {
			term_t val = eval_match_exp(me, ctx, hp);
			if (val == noval)
				return noval;
			if (val == A_TRUE)
				res = A_TRUE;
			me = me->next;
		} while (me != 0);
		return res;
	}
	case meo_not:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return val;
		return (val == A_FALSE) ?A_TRUE :A_FALSE;
	}
	case meo_xor:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		return ((l == A_TRUE && r == A_FALSE) || (l == A_FALSE && r == A_TRUE))
									?A_TRUE :A_FALSE;
	}
	case meo_andalso:
	{
		match_exp_t *me = exp->arg;
		assert(me != 0);
		do {
			term_t val = eval_match_exp(me, ctx, hp);
			if (val == noval)
				return noval;
			if (val != A_TRUE)
				return A_FALSE;
			me = me->next;
		} while (me != 0);
		return A_TRUE;
	}
	case meo_orelse:
	{
		match_exp_t *me = exp->arg;
		assert(me != 0);
		do {
			term_t val = eval_match_exp(me, ctx, hp);
			if (val == noval)
				return noval;
			if (val == A_TRUE)
				return A_TRUE;
			me = me->next;
		} while (me != 0);
		return A_FALSE;
	}
	case meo_abs:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return noval;
		if (is_int(val))
		{
			int n = int_value(val);
			if (n < 0)
				n = -n;
			return tag_int(n);	// no overflow
		}
		else if (is_boxed_bignum(val))
		{
			bignum_t *bn = (bignum_t *)peel_boxed(val);
			uint16_t *digs;
			term_t r = heap_bignum(hp, MP_ZPOS, bn->used, &digs);
			memcpy(digs, bn->dp, bn->used *sizeof(uint16_t));
			return r;
		}
		else if (is_boxed_float(val))
		{
			double d = float_value(peel_boxed(val));
			if (d < 0)
				d = -d;
			return heap_float(hp, d);
		}
		else
			return noval;
	}
	case meo_element:
	{
		term_t val = eval_match_exp(exp->left, ctx, hp);
		if (val == noval || !is_tuple(val))
			return noval;
		term_t pos = eval_match_exp(exp->right, ctx, hp);
		if (val == noval || !is_int(pos));
		int n = int_value(pos);
		uint32_t *p = peel_tuple(val);
		int arity = *p++;
		if (n < 1 || n > arity)
			return noval;
		return p[n -1];
	}
	case meo_hd:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval || !is_cons(val))
			return noval;
		return (peel_cons(val))[0];
	}
	case meo_length:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval || !is_list(val))
			return noval;
		int len = list_len(val);
		if (len < 0)
			return noval;
		return int_to_term(len, hp);
	}
	case meo_node:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval || !is_cons(val))
			return noval;
		if (is_short_pid(val) || is_short_oid(val))
			return cluster_node;
		if (is_boxed_pid(val))
		{
			t_long_pid_t *pid = (t_long_pid_t *)peel_boxed(val);
			return pid->node;
		}
		if (is_boxed_oid(val))
		{
			t_long_oid_t *oid = (t_long_oid_t *)peel_boxed(val);
			return oid->node;
		}
		return noval;
	}
	case meo_round:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval || !is_boxed_float(val))
			return noval;
		double d = float_value(peel_boxed(val));
		if (d > 0.0)
			d += 0.5;
		else
			d -= 0.5;
		return float_to_int(d, hp);
	}
	case meo_size:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval || !is_boxed_float(val))
			return noval;
		if (is_tuple(val))
		{
			int arity = *peel_tuple(val);
			assert(fits_int(arity));
			return tag_int(arity);
		}
		else if is_boxed_binary(val)
		{
			bits_t bs;
			bits_get_real(peel_boxed(val), &bs);
			uint32_t sz = (bs.ends - bs.starts) /8;
			return int_to_term(sz, hp);
		}
		else
			return noval;
	}
	case meo_tl:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval || !is_cons(val))
			return noval;
		return (peel_cons(val))[1];
	}
	case meo_trunc:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval || !is_boxed_float(val))
			return noval;
		double d = float_value(peel_boxed(val));
		return float_to_int(d, hp);
	}
	case meo_plus:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
		{
			int i = int_value(l) + int_value(r);
			return int_to_term(i, hp);
		}
		else
		{
			term_t s = mixed_add(l, r, hp);
			if (is_atom(s))
				return noval;
			return s;
		}
	}
	case meo_minus:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
		{
			int i = int_value(l) - int_value(r);
			return int_to_term(i, hp);
		}
		else
		{
			term_t s = mixed_sub(l, r, hp);
			if (is_atom(s))
				return noval;
			return s;
		}
	}
	case meo_mult:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
		{
			int64_t i = (int64_t)int_value(l) * int_value(r);
			return int_to_term(i, hp);
		}
		else
		{
			term_t s = mixed_mul(l, r, hp);
			if (is_atom(s))
				return noval;
			return s;
		}
	}
	case meo_int_div:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
		{
			if (int_value(r) == 0)
				return noval;
			int i = int_value(l) / int_value(r);
			assert(fits_int(i));
			return tag_int(i);
		}
		else
		{
			term_t s = mixed_int_div(l, r, hp);
			if (is_atom(s))
				return noval;
			return s;
		}
	}
	case meo_div:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		double a = term_to_float(l);
		double b = term_to_float(r);
		if (b == 0.0 || !isfinite(a) || !isfinite(b))
			return noval;
		term_t s = heap_float_with_check(hp, a / b);
		if (is_atom(s))
			return noval;
		return s;
	}
	case meo_rem:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
		{
			if (int_value(r) == 0)
				return noval;
			int i = int_value(l) % int_value(r);
			assert(fits_int(i));
			return tag_int(i);
		}
		else
		{
			term_t s = mixed_rem(l, r, hp);
			if (is_atom(s))
				return noval;
			return s;
		}
	}
	case meo_band:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
			return l & r;	// tag intact, never an overflow
		else
		{
			term_t s = mixed_band(l, r, hp);
			if (is_atom(s))
				return noval;
			return s;
		}
	}
	case meo_bor:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
			return l | r;	// tag intact, never an overflow
		else
		{
			term_t s = mixed_bor(l, r, hp);
			if (is_atom(s))
				return noval;
			return s;
		}
	}
	case meo_bxor:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
			return tag_int(int_value(l) ^ int_value(r));
		else
		{
			term_t s = mixed_bxor(l, r, hp);
			if (is_atom(s))
				return noval;
			return s;
		}
	}
	case meo_bnot:
	{
		term_t val = eval_match_exp(exp->arg, ctx, hp);
		if (val == noval)
			return noval;
		if (is_int(val))
			return tag_int(~int_value(val));
		else
		{
			term_t s = mixed_bnot(val, hp);
			if (is_atom(s))
				return noval;
			return s;
		}
	}
	case meo_bsl:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		term_t s;
		if (are_both_int(l, r))
			s = mixed_bsl_i(int_value(l), int_value(r), hp);
		else
			s = mixed_bsl(l, r, hp);
		if (is_atom(s))
			return noval;
		return s;
	}
	case meo_bsr:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		term_t s;
		if (are_both_int(l, r))
			s = mixed_bsr_i(int_value(l), int_value(r), hp);
		else
			s = mixed_bsr(l, r, hp);
		if (is_atom(s))
			return noval;
		return s;
	}
	case meo_gt:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
			return (int_value(l) > int_value(r))
				?A_TRUE :A_FALSE;
		else
			return (is_term_smaller(r, l))
				?A_TRUE :A_FALSE;
	}
	case meo_ge:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
			return (int_value(l) >= int_value(r))
				?A_TRUE :A_FALSE;
		else
			return (!is_term_smaller(l, r))
				?A_TRUE :A_FALSE;
	}
	case meo_lt:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
			return (int_value(l) < int_value(r))
				?A_TRUE :A_FALSE;
		else
			return (is_term_smaller(l, r))
				?A_TRUE :A_FALSE;
	}
	case meo_le:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_int(l, r))
			return (int_value(l) <= int_value(r))
				?A_TRUE :A_FALSE;
		else
			return (!is_term_smaller(r, l))
				?A_TRUE :A_FALSE;
	}
	case meo_eqex:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		else
			return (l == r || are_terms_equal(l, r, 1)) ?A_TRUE :A_FALSE;
	}
	case meo_eq:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		else
			return (l == r || are_terms_equal(l, r, 0)) ?A_TRUE :A_FALSE;
	}
	case meo_neex:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_immed(l, r))
			return (l != r) ?A_TRUE :A_FALSE;
		else
			return (l != r && !are_terms_equal(l, r, 1)) ?A_TRUE :A_FALSE;
	}
	case meo_ne:
	{
		term_t l = eval_match_exp(exp->left, ctx, hp);
		if (l == noval)
			return noval;
		term_t r = eval_match_exp(exp->right, ctx, hp);
		if (r == noval)
			return noval;
		if (are_both_immed(l, r))
			return (l != r) ?A_TRUE :A_FALSE;
		else
			return (l != r && !are_terms_equal(l, r, 0)) ?A_TRUE :A_FALSE;
	}
	default:
		assert(exp->op == meo_self);
		return ctx->self;
	}
}

ets_match_spec_t *ets_match_compile_spec(term_t MatchSpec, heap_t *hp)
{
	if (!is_list(MatchSpec))
		return 0;

	ets_match_spec_t *match_spec = 0;
	ets_match_spec_t **msp = &match_spec;
	term_t t = MatchSpec;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);

		comp_info_t ci;
		memset(&ci, 0, sizeof(ci));

		if (compile_spec1(cons[0], &ci) < 0)
			return 0;

		int sz = sizeof(ets_match_spec_t) +
				 sizeof(match_pat_t) *ci.nr_pats +
				 sizeof(match_exp_t) *ci.nr_exps;

		ci.mem = heap_tmp_buf(hp, sz);
		memset(ci.mem, 0, sz);

		UNUSED void *saved_mem = ci.mem;
		ets_match_spec_t *ms = compile_spec2(cons[0], &ci);
		assert(saved_mem +sz == ci.mem);
		assert(ms != 0);
		assert(ms->next == 0);

		*msp = ms;
		msp = &ms->next;

		//
		// Links from older to newer heap chunks may be created.
		// It does not matter because compiled match specs are
		// temporary and do not survive garbage collection.
		//

		t = cons[1];
	}
	if (!is_nil(t))
		return 0;

	return match_spec;
}

static int int_compare(const void *pa, const void *pb)
{
	int a = *(int *)pa;
	int b = *(int *)pb;
	return a - b;
}

static int compile_spec1(term_t spec, comp_info_t *ci)
{
	if (!is_tuple(spec))
		return -BAD_ARG;
	uint32_t *tdata= peel_tuple(spec);
	if (*tdata != 3)
		return -BAD_ARG;
	term_t head = tdata[1];
	term_t guards = tdata[2];
	term_t body = tdata[3];
	if (body == nil)
		return -BAD_ARG;

	if (compile_pattern_top1(head, ci) < 0)
		return -BAD_ARG;

	// sort variable name to allow faster lookups
	qsort(ci->var_names, ci->nr_vars, sizeof(int), int_compare);

	if (compile_exp_list1(guards, ci) < 0)
		return -BAD_ARG;
	if (compile_exp_list1(body, ci) < 0)
		return -BAD_ARG;

	return 0;
}

static int var_name_from_atom(term_t t)
{
	assert(is_atom(t));
	uint8_t *name = atoms_get(atom_index(t));
	uint8_t len = *name++;
	if (len == 0 || name[0] != '$')
		return -1;
	int num = 0;
	uint8_t *p = name +1;
	while (p < name +len)
	{
		if (*p < '0' || *p > '9')
			return -1;
		num *= 10;
		num += *p - '0';
		if (num > 100000000)
			return -1;
		p++;
	}
	return num;
}

static int compile_pattern_top1(term_t top, comp_info_t *ci)
{
	int num;
	if (top == AVARANY__)	// _
		ci->nr_pats++;
	else if (is_atom(top) && (num = var_name_from_atom(top)) >= 0)
	{
		if (ci->nr_vars >= ETS_MATCH_MAX_NAMES)
			return -BAD_ARG;	// rare
		ci->var_names[ci->nr_vars++] = num;
		ci->nr_pats++;
	}
	else if (is_tuple(top))
	{
		uint32_t *p = peel_tuple(top);
		int arity = *p++;
		if (compile_pattern1(p, arity, ci) < 0)
			return -BAD_ARG;
	}
	else
		return -BAD_ARG;

	return 0;
}

static int compile_pattern1(term_t *elts, int arity, comp_info_t *ci)
{
	ci->nr_pats += arity;
	for (int i = 0; i < arity; i++)
	{
		if (compile_pat1(elts[i], ci) < 0)
			return -BAD_ARG;
	}
	return 0;
}

static int compile_pat1(term_t pat, comp_info_t *ci)
{
	if (is_atom(pat) && pat != AVARANY__)
	{
		int num = var_name_from_atom(pat);
		if (num >= 0)
		{
			int i;
			for (i = 0; i < ci->nr_vars; i++)
				if (ci->var_names[i] == num)
					break;
			if (i >= ci->nr_vars)
			{
				if (ci->nr_vars >= ETS_MATCH_MAX_NAMES)
					return -BAD_ARG;
				ci->var_names[ci->nr_vars++] = num;
			}
		}
	}
	else if (is_tuple(pat))
	{
		uint32_t *p = peel_tuple(pat);
		int arity = *p++;
		if (compile_pattern1(p, arity, ci) < 0)
			return -BAD_ARG;
	}
	else if (is_cons(pat))
	{
		term_t *cons = peel_cons(pat);
		if (compile_pattern1(cons, 2, ci) < 0)
			return -BAD_ARG;
	}

	return 0;
}

static int compile_exp_list1(term_t exps, comp_info_t *ci)
{
	if (!is_list(exps))
		return -BAD_ARG;
	term_t t = exps;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		if (compile_exp1(cons[0], ci) < 0)
			return -BAD_ARG;
		t = cons[1];
	}
	if (!is_nil(t))
		return -BAD_ARG;

	return 0;
}

static int compile_exp1(term_t e, comp_info_t *ci)
{
tail_rec:
	ci->nr_exps++;
	if (is_atom(e))
	{
		int num = var_name_from_atom(e);
		if (num >= 0)
		{
			int i;
			for (i = 0; i < ci->nr_vars; i++)
				if (ci->var_names[i] == num)
					break;
			if (i >= ci->nr_vars)
				return -BAD_ARG;
		}
	}
	else if (is_cons(e))
	{
		term_t *cons = peel_cons(e);
		if (compile_exp1(cons[0], ci) < 0)
			return -BAD_ARG;
		e = cons[1];
		goto tail_rec;
	}
	else if (is_tuple(e))
	{
		uint32_t *p = peel_tuple(e);
		int arity = *p++;
		if (arity == 0)
			return -BAD_ARG;

		if (arity == 1 && is_tuple(p[0]))
		{
			// double braces; build a tuple
			p = peel_tuple(p[0]);
			arity = *p++;
			for (int i = 0; i < arity; i++)
				if (compile_exp1(p[i], ci) < 0)
					return -BAD_ARG;
		}
		else if (arity == 2 && p[0] == A_CONST)
		{
			// {const,Lit} constuct
		}
		else
		{
			// Function:
			//
			// is_atom, is_float, is_integer, is_list, is_number, is_pid,
			// is_port, is_reference, is_tuple, is_binary, is_function,
			// is_record,
			// and, or, not, xor, andalso, orelse,
			// abs, element, hd, tl, length, node, round, size, trunc,
			// plus, minus, mult, int_div, div, rem,
			// band, bor, bxor, bnot, bsl, bsr
			// gt, ge, lt, le, eqex, eq, neex, ne
			// self

			term_t f = p[0];
			if (f == A_IS_ATOM || f == A_IS_FLOAT || f == A_IS_INTEGER ||
				f == A_IS_LIST || f == A_IS_NUMBER || f == A_IS_PID ||
				f == A_IS_PORT || f == A_IS_REFERENCE || f == A_IS_TUPLE ||
				f == A_IS_BINARY || f == A_IS_FUNCTION)
			{
				if (arity != 2 || compile_exp1(p[1], ci) < 0)
					return -BAD_ARG;
			}
			else if (f == A_IS_RECORD)
			{
				if (arity != 3 || !is_atom(p[1]) || !is_int(p[2]) || int_value(p[2]) < 1)
					return -BAD_ARG;
			}
			else if (f == A_AND || f == A_OR || f == A_XOR || f == A_BAND ||
					 f == A_BOR || f == A_BXOR || f == APLUS__ || f == AMINUS__ ||
					 f == ATIMES__ || f == ADIV__ || f == A_DIV || f == A_REM ||
					 f == A_BSL || f == A_BSR ||
					 f == A_ANDALSO || f == A_ORELSE ||
					 f == AGT__ || f == AGE__ || f == ALT__ || f == ALE__ ||
					 f == AEQEX__ || f == AEQ__ || f == ANEEX__ || f == ANE__ ||
					 f == A_ELEMENT)
			{
				if (arity != 3 || compile_exp1(p[1], ci) < 0 || compile_exp1(p[2], ci) < 0)
					return -BAD_ARG;
			}
			else if (f == A_NOT || f == A_BNOT || f == A_ABS || f == A_ROUND ||
					 f == A_TRUNC || f == A_LENGTH || f == A_SIZE || f == A_NODE ||
					 f == A_HD || f == A_TL)
			{
				if (arity != 2 || compile_exp1(p[1], ci) < 0)
					return -BAD_ARG;
			}
			else if (f == A_SELF)
			{
				if (arity != 1)
					return - BAD_ARG;
			}
			else
				return -BAD_ARG;
		}
	}

	return 0;
}

static ets_match_spec_t *compile_spec2(term_t spec, comp_info_t *ci)
{
	assert(is_tuple(spec));
	uint32_t *tdata= peel_tuple(spec);
	assert(*tdata == 3);
	term_t head = tdata[1];
	term_t guards = tdata[2];
	term_t body = tdata[3];

	ets_match_spec_t *ms = (ets_match_spec_t *)ci->mem;
	ci->mem = ms +1;

	ms->head = compile_pattern_top2(head, ci);
	ms->guards = compile_exp_list2(guards, ci);
	ms->body = compile_exp_list2(body, ci);
	ms->nr_vars = ci->nr_vars;
	//ms->next = 0;

	return ms;
}

static int var_index_from_name(int num, comp_info_t *ci)
{
	assert(ci->nr_vars > 0);
	int i;
	for (i = 0; i < ci->nr_vars -1; i++)
		if (ci->var_names[i] == num)
			return i;
	assert(ci->var_names[i] == num);
	return i;
}

static match_pat_t *compile_pattern_top2(term_t top, comp_info_t *ci)
{
	int num;
	if (top == AVARANY__)
	{
		match_pat_t *mp = (match_pat_t *)ci->mem;
		ci->mem = mp +1;

		mp->op = mpo_skip_top_var;
		return mp;
	}
	else if (is_atom(top) && (num = var_name_from_atom(top)) >= 0)
	{
		match_pat_t *mp = (match_pat_t *)ci->mem;
		ci->mem = mp +1;

		int var = var_index_from_name(num, ci);
		mp->op = mpo_bind_top_var;
		mp->var = var;
		return mp;
	}

	assert(is_tuple(top));
	uint32_t *p = peel_tuple(top);
	int arity = *p++;

	return compile_pattern2(p, arity, ci);
}

static match_pat_t *compile_pattern2(term_t *elts, int arity, comp_info_t *ci)
{
	match_pat_t *match_pat = 0;
	match_pat_t **mpp = &match_pat;
	for (int i = 0; i < arity; i++)
	{
		match_pat_t *mp = compile_pat2(elts[i], ci);
		assert(mp != 0);
		assert(mp->next == 0);
		*mpp = mp;
		mpp = &mp->next;
	}
	return match_pat;
}

static match_pat_t *compile_pat2(term_t pat, comp_info_t *ci)
{
	match_pat_t *mp = (match_pat_t *)ci->mem;
	ci->mem = mp +1;

	if (pat == AVARANY__)	// _
	{
		mp->op = mpo_skip_var;
		return mp;
	}
	else if (is_atom(pat))
	{
		int num = var_name_from_atom(pat);
		if (num >= 0)
		{
			int var = var_index_from_name(num, ci);
			mp->op = mpo_bind_var;
			mp->var = var;
			return mp;
		}
	}
	else if (is_tuple(pat))
	{
		uint32_t *p = peel_tuple(pat);
		int arity = *p++;
		mp->op = mpo_tuple;
		mp->inner = compile_pattern2(p, arity, ci);
		mp->arity = arity;
		return mp;
	}
	else if (is_cons(pat))
	{
		term_t *cons = peel_cons(pat);
		mp->op = mpo_cons;
		mp->inner = compile_pattern2(cons, 2, ci);
		//mp->arity = 2;
		return mp;
	}

	mp->op = mpo_literal;
	mp->lit = pat;
	return mp;
}

static match_exp_t *compile_exp_list2(term_t exps, comp_info_t *ci)
{
	match_exp_t *match_exp = 0;
	match_exp_t **mep = &match_exp;

	assert(is_list(exps));
	term_t t = exps;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		match_exp_t *me = compile_exp2(cons[0], ci);
		assert(me != 0);

		*mep = me;
		mep = &me->next;

		t = cons[1];
	}
	assert(is_nil(t));

	return match_exp;
}

static match_exp_t *compile_exp2(term_t exp, comp_info_t *ci)
{
	match_exp_t *me = (match_exp_t *)ci->mem;
	ci->mem = me +1;

	if (exp == AVARENTIRE__)	// $_
	{
		me->op = meo_matched;
		return me;
	}
	else if (exp == AVARALL__)	// $$
	{
		me->op = meo_all_bound;
		return me;
	}
	else if (is_atom(exp))
	{
		int num = var_name_from_atom(exp);
		if (num >= 0)
		{
			int var = var_index_from_name(num, ci);
			me->op = meo_var;
			me->var = var;
			return me;
		}
	}
	else if (is_cons(exp))
	{
		term_t *cons = peel_cons(exp);
		me->op = meo_make_cons;
		me->left = compile_exp2(cons[0], ci);
		me->right = compile_exp2(cons[1], ci);
		return me;
	}
	else if (is_tuple(exp))
	{
		uint32_t *p = peel_tuple(exp);
		int arity = *p++;

		if (arity == 1 && is_tuple(p[0]))
		{
			// double braces; build a tuple
			p = peel_tuple(p[0]);
			arity = *p++;

			me->op = meo_make_tuple;
			//me->elts = 0;
			me->arity = arity;

			for (int i = arity -1; i >= 0; i--)
			{
				match_exp_t *elt = compile_exp2(p[i], ci);
				elt->next = me->elts;
				me->elts = elt;
			}
			return me;
		}
		else if (arity == 2 && p[0] == A_CONST)
		{
			// {const,Lit} constuct
			me->op = meo_lit;
			me->lit = p[1];
			return me;
		}
		else
		{
			// Function:
			//
			// is_atom, is_float, is_integer, is_list, is_number, is_pid,
			// is_port, is_reference, is_tuple, is_binary, is_function,
			// is_record,
			// and, or, not, xor, andalso, orelse,
			// abs, element, hd, tl, length, node, round, size, trunc,
			// plus, minus, mult, int_div, div, rem,
			// band, bor, bxor, bnot, bsl, bsr
			// gt, ge, lt, le, eqex, eq, neex, ne
			// self

			term_t f = p[0];

			enum exp_op_t op = meo_none;

			if (f == A_IS_ATOM) 			op = meo_is_atom;
			else if (f == A_IS_FLOAT)		op = meo_is_float;
			else if (f == A_IS_INTEGER)		op = meo_is_integer;
			else if (f == A_IS_LIST)		op = meo_is_list;
			else if (f == A_IS_NUMBER)		op = meo_is_number;
			else if (f == A_IS_PID)			op = meo_is_pid;
			else if (f == A_IS_PORT)		op = meo_is_port;
			else if (f == A_IS_REFERENCE)	op = meo_is_reference;
			else if (f == A_IS_TUPLE)		op = meo_is_tuple;
			else if (f == A_IS_BINARY)		op = meo_is_binary;
			else if (f == A_IS_FUNCTION)	op = meo_is_function;

			if (op != meo_none)
			{
				me->op = op;
				me->arg = compile_exp2(p[1], ci);
				return me;
			}

			if (f == A_IS_RECORD)
			{
				me->op = meo_is_record;
				me->rtype = p[1];
				me->rsize = int_value(p[2]);
				return me;
			}

			if (f == A_AND)				op = meo_and;
			else if (f == A_OR)			op = meo_or;
			else if (f == A_XOR)		op = meo_xor;
			else if (f == A_BAND)		op = meo_band;
			else if (f == A_BOR)		op = meo_bor;
			else if (f == A_BXOR)		op = meo_bxor;
			else if (f == APLUS__)		op = meo_plus;
			else if (f == AMINUS__)		op = meo_minus;
			else if (f == ATIMES__)		op = meo_mult;
			else if (f == ADIV__)		op = meo_div;
			else if (f == A_DIV)		op = meo_int_div;
			else if (f == A_REM)		op = meo_rem;
			else if (f == A_BSL)		op = meo_bsl;
			else if (f == A_BSR)		op = meo_bsr;
			else if (f == A_ANDALSO)	op = meo_andalso;
			else if (f == A_ORELSE)		op = meo_orelse;
			else if (f == AGT__)		op = meo_gt;
			else if (f == AGE__)		op = meo_ge;
			else if (f == ALT__)		op = meo_lt;
			else if (f == ALE__)		op = meo_le;
			else if (f == AEQEX__)		op = meo_eqex;
			else if (f == AEQ__)		op = meo_eq;
			else if (f == ANEEX__)		op = meo_neex;
			else if (f == ANE__)		op = meo_ne;
			else if (f == A_ELEMENT)	op = meo_element;

			if (op != meo_none)
			{
				me->op = op;
				me->left = compile_exp2(p[1], ci);
			    me->right = compile_exp2(p[2], ci);
				return me;
			}

			if (f == A_NOT)			op = meo_not;
			else if (f == A_BNOT)	op = meo_bnot;
			else if (f == A_ABS)	op = meo_abs;
			else if (f == A_ROUND)	op = meo_round;
			else if (f == A_TRUNC)	op = meo_trunc;
			else if (f == A_LENGTH)	op = meo_length;
			else if (f == A_SIZE)	op = meo_size;
			else if (f == A_NODE)	op = meo_node;
			else if (f == A_HD)		op = meo_hd;
			else if (f == A_TL)		op = meo_tl;

			if (op != meo_none)
			{
				me->op = op;
				me->arg = compile_exp2(p[1], ci);
				return me;
			}

			assert(f == A_SELF);
			me->op = meo_self;
			return me;
		}
	}

	me->op = meo_lit;
	me->lit = exp;
	return me;
}

ets_match_spec_t *ets_match_compile_pattern(term_t MatchPat, int return_object, heap_t *hp)
{
	comp_info_t ci;
	memset(&ci, 0, sizeof(ci));

	if (compile_pattern_top1(MatchPat, &ci) < 0)
		return 0;

	ci.nr_exps += 1;	// fake body

	// sort variable name to allow faster lookups
	qsort(ci.var_names, ci.nr_vars, sizeof(int), int_compare);

	int sz = sizeof(ets_match_spec_t) +
		ci.nr_pats *sizeof(match_pat_t) +
		ci.nr_exps *sizeof(match_exp_t);
	assert(ci.nr_exps == 1);

	ci.mem = heap_tmp_buf(hp, sz);
	memset(ci.mem, 0, sz);
	UNUSED void *saved_mem = ci.mem;

	ets_match_spec_t *ms = (ets_match_spec_t *)ci.mem;
	ci.mem = ms +1;

	ms->head = compile_pattern_top2(MatchPat, &ci);
	ms->guards = 0;
	ms->body = compile_exp2((return_object) ?AVARENTIRE__ :AVARALL__, &ci);
	ms->nr_vars = ci.nr_vars;

	assert(ci.mem == saved_mem +sz);

	return ms;
}

//EOF
