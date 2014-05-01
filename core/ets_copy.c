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

#include "ling_common.h"

#include "stack.h"
#include "string.h"
#include "bits.h"

#define ETS_TERMS_COPY_CRADLE_SIZE	4096

typedef struct ets_deferred_copy_t ets_deferred_copy_t;
struct ets_deferred_copy_t {
	term_t *terms;
	int num;
};

static int ets_terms_copy_size2(stack_t *st);
static uint32_t *terms_copy(stack_t *stack, term_t *terms, int num,
								uint32_t *htop, t_proc_bin_t **pbs);

int ets_terms_copy_size(term_t *terms, int num)
{
	uint32_t cradle[ETS_TERMS_COPY_CRADLE_SIZE];
	stack_t st;
	stack_init(&st, 1, cradle, ETS_TERMS_COPY_CRADLE_SIZE);

	for (int i = 0; i < num; i++)
	{
		uint32_t *push = stack_push_N(&st);
		if (push == 0)
		{
			stack_done(&st);
			no_memory_signal();
		}
		*push = terms[i];
	}

	int wsize = ets_terms_copy_size2(&st);
	if (wsize < 0)
	{
		assert(wsize == -NO_MEMORY);
		stack_done(&st);
		no_memory_signal();
	}

	stack_done(&st);
	return wsize;
}

int ets_terms_copy_size_N(term_t *terms, int num)
{
	uint32_t cradle[ETS_TERMS_COPY_CRADLE_SIZE];
	stack_t st;
	stack_init(&st, 1, cradle, ETS_TERMS_COPY_CRADLE_SIZE);

	for (int i = 0; i < num; i++)
	{
		uint32_t *push = stack_push_N(&st);
		if (push == 0)
		{
			stack_done(&st);
			return -NO_MEMORY;
		}
		*push = terms[i];
	}

	int wsize = ets_terms_copy_size2(&st);

	stack_done(&st);
	return wsize;
}

static int ets_terms_copy_size2(stack_t *st)
{
	int hsize = 0;
pop_term:
	if (stack_is_empty(st))
		return hsize;
	term_t t = (term_t)*stack_pop(st);
tail_recur:
	if (is_immed(t))
		goto pop_term;

	if (is_cons(t))
	{
		while (is_cons(t))
		{
			hsize += 2;
			term_t *cons = peel_cons(t);
			uint32_t *push = stack_push_N(st);
			if (push == 0)
				return -NO_MEMORY;
			*push = cons[0];
			t = cons[1];
		}
		if (t != nil)
			goto tail_recur;

		goto pop_term;
	}

	if (is_tuple(t))
	{
		uint32_t *p = peel_tuple(t);
		int arity = *p++;
		if (arity == 0)
			goto pop_term; // no heap frag
		hsize += 1 +arity;
		for (int i = 0; i < arity -1; i++)
		{
			uint32_t *push = stack_push_N(st);
			if (push == 0)
				return -NO_MEMORY;
			*push = p[i];
		}

		t = p[arity -1];
		goto tail_recur;
	}

	assert(is_boxed(t));
	uint32_t *tdata = peel_boxed(t);
	switch (boxed_tag(tdata))
	{
	case SUBTAG_POS_BIGNUM:
	case SUBTAG_NEG_BIGNUM:
	{
		bignum_t *bn = (bignum_t *)tdata;
		hsize += WSIZE(bignum_t) + (bn->used*sizeof(uint16_t) +3) /4;
		goto pop_term;
	}
	case SUBTAG_FLOAT:
		hsize += WSIZE(t_float_t);
		goto pop_term;

	case SUBTAG_FUN:
	{
		t_fun_t *fun = (t_fun_t *)tdata;
		int num_free = fun_num_free(tdata);
		hsize += WSIZE(t_fun_t) + num_free;

		for (int i = 0; i < num_free -1; i++)
		{
			uint32_t *push = stack_push_N(st);
			if (push == 0)
				return -NO_MEMORY;
			*push = fun->frozen[i];
		}
		if (num_free == 0)
			goto pop_term;

		t = fun->frozen[num_free -1];
		goto tail_recur;
	}
	case SUBTAG_EXPORT:
		hsize += WSIZE(t_export_t);
		goto pop_term;

	case SUBTAG_PID:
		hsize += WSIZE(t_long_pid_t);
		goto pop_term;

	case SUBTAG_OID:
		hsize += WSIZE(t_long_oid_t);
		goto pop_term;

	case SUBTAG_REF:
		hsize += WSIZE(t_long_ref_t);
		goto pop_term;

	case SUBTAG_PROC_BIN:
		hsize += WSIZE(t_proc_bin_t);
		goto pop_term;

	case SUBTAG_HEAP_BIN:
	{
		t_heap_bin_t *hb = (t_heap_bin_t *)tdata;
		hsize += WSIZE(t_heap_bin_t) + (hb->byte_size +3) /4;
		goto pop_term;
	}
	case SUBTAG_MATCH_CTX:
	{
		t_match_ctx_t *mc = (t_match_ctx_t *)tdata;
		int num_slots = match_ctx_num_slots(tdata);

		hsize += WSIZE(t_match_ctx_t) + num_slots*sizeof(int64_t) /4;

		t = mc->parent;
		goto tail_recur;
	}
	default: // SUBTAG_SUB_BIN
	{
		assert(boxed_tag(tdata) == SUBTAG_SUB_BIN);
		t_sub_bin_t *sb = (t_sub_bin_t *)tdata;
		
		hsize += WSIZE(t_sub_bin_t);

		t = sb->parent;
		goto tail_recur;
	}
	}
}

uint32_t *ets_terms_copy_non_recursive_N(term_t *terms, int num,
								uint32_t *htop, t_proc_bin_t **pbs)
{
	uint32_t cradle[256];
	stack_t st;
	stack_init(&st, WSIZE(ets_deferred_copy_t), cradle, 256);

	uint32_t *last_htop = terms_copy(&st, terms, num, htop, pbs);

	stack_done(&st);
	return last_htop;
}

#define DEFER_COPY(st, ts, n) do { \
	ets_deferred_copy_t *push__ = (ets_deferred_copy_t *)stack_push_N((st)); \
	if (push__ == 0) \
		return 0; \
	push__->terms = (ts); \
	push__->num = (n); \
} while (0)

#define EASY_COPY(s) do { \
	memcpy(htop, tdata, sizeof(s)); \
	htop += WSIZE(s); \
} while (0)

static uint32_t *terms_copy(stack_t *stack, term_t *terms, int num,
								uint32_t *htop, t_proc_bin_t **pbs)
{
next_term:
	if (num == 0)
	{
		if (stack_is_empty(stack))
			return htop;
		ets_deferred_copy_t *pop = (ets_deferred_copy_t *)stack_pop(stack);
		terms = pop->terms;
		num = pop->num;
		goto next_term;
	}

	term_t t = terms[0];
	if (is_immed(t))
	{
		terms++;
		num--;
		goto next_term;
	}

	term_t copy = noval;
	if (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		copy = tag_cons(htop);
		term_t *new_cons = htop;
		do {
			new_cons[0] = cons[0];
			new_cons[1] = cons[1];
			htop += 2;

			if (!is_immed(new_cons[0]))
				DEFER_COPY(stack, new_cons, 1);

			term_t tail = new_cons[1];
			if (is_immed(tail))
				break;

			if (!is_cons(tail))
			{
				DEFER_COPY(stack, new_cons +1, 1);
				break;
			}

			new_cons[1] = tag_cons(htop);

			cons = peel_cons(tail);
			new_cons = htop;
		} while (1);
	}
	else if (is_tuple(t))
	{
		uint32_t *p = peel_tuple(t);
		int arity = *p++;
		if (arity == 0)
			copy = ZERO_TUPLE;
		else
		{
			copy = tag_tuple(htop);
			*htop++ = arity;
			memcpy(htop, p, arity *sizeof(term_t));
			DEFER_COPY(stack, htop, arity);
			htop += arity;
		}
	}
	else
	{
		assert(is_boxed(t));
		uint32_t *tdata = peel_boxed(t);
		copy = tag_boxed(htop);
		switch (boxed_tag(tdata))
		{
		case SUBTAG_POS_BIGNUM:
		case SUBTAG_NEG_BIGNUM:
		{
			bignum_t *bn = (bignum_t *)tdata;
			int wsize = WSIZE(bignum_t) + (bn->used*sizeof(uint16_t) +3) /4;
			memcpy(htop, tdata, wsize *sizeof(uint32_t));
			htop += wsize;
			break;
		}
		case SUBTAG_FLOAT:
			EASY_COPY(t_float_t);
			break;

		case SUBTAG_FUN:
		{
			t_fun_t *new_fun = (t_fun_t *)htop;
			int num_free = fun_num_free(tdata);
			int wsize = WSIZE(t_fun_t) + num_free;
			memcpy(new_fun, tdata, wsize *sizeof(uint32_t));
			DEFER_COPY(stack, new_fun->frozen, num_free);
			htop += wsize;
			break;
		}
		case SUBTAG_EXPORT:
			EASY_COPY(t_export_t);
			break;

		case SUBTAG_PID:
			EASY_COPY(t_long_pid_t);
			break;

		case SUBTAG_OID:
			EASY_COPY(t_long_oid_t);
			break;

		case SUBTAG_REF:
			EASY_COPY(t_long_ref_t);
			break;

		case SUBTAG_PROC_BIN:
		{
			t_proc_bin_t *pb = (t_proc_bin_t *)htop;
			memcpy(htop, tdata, sizeof(t_proc_bin_t));

			// 1+ bin node refc
			proc_bin_link(pbs, pb, 0);

			htop += WSIZE(t_proc_bin_t);
			break;
		}
		case SUBTAG_HEAP_BIN:
		{
			t_heap_bin_t *hb = (t_heap_bin_t *)tdata;
			int wsize = WSIZE(t_heap_bin_t) + (hb->byte_size +3) /4;
			memcpy(htop, tdata, wsize*sizeof(uint32_t));
			htop += wsize;
			break;
		}
		case SUBTAG_MATCH_CTX:
		{
			t_match_ctx_t *new_mc = (t_match_ctx_t *)htop;
			memcpy(new_mc, tdata, sizeof(t_match_ctx_t));
			DEFER_COPY(stack, &new_mc->parent, 1);
			htop += WSIZE(t_match_ctx_t);
			break;
		}
		default: // SUBTAG_SUB_BIN
		{
			assert(boxed_tag(tdata) == SUBTAG_SUB_BIN);
			t_sub_bin_t *new_sb = (t_sub_bin_t *)htop;
			memcpy(new_sb, tdata, sizeof(t_sub_bin_t));
			DEFER_COPY(stack, &new_sb->parent, 1);
			htop += WSIZE(t_sub_bin_t);
			break;
		}
		}
	}

	assert(copy != noval);
	*terms++ = copy;
	num--;
	goto next_term;
}

//EOF
