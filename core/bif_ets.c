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

#include "ets.h"

// ets:give_away/3 [?]
term_t cbif_ets_give_away3(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Pid = regs[1];
	term_t GiftData = regs[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (!is_short_pid(Pid))
		badarg(Pid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->owner != proc->pid || tab->owner == Pid)
		badarg(Tid);

	proc_t *new_owner = scheduler_lookup(Pid);
	if (new_owner == 0)
		badarg(Pid);

	//{'ETS-TRANSFER',Tab,FromPid,GiftData}
	term_t new_gift_data = GiftData;
	int x = heap_copy_terms_N(&new_owner->hp, &new_gift_data, 1);
	if (x < 0)
	{
		assert(x == -NO_MEMORY);
		fail(A_NO_MEMORY);
	}
	// EXCEPTION POSSIBLE
	term_t msg = heap_tuple4(&new_owner->hp, AETS_TRANSFER__, tab->tid, proc->pid, new_gift_data);
	x = scheduler_new_local_mail_N(new_owner, msg);
	if (x < 0)
	{
		assert(x == -NO_MEMORY);
		fail(A_NO_MEMORY);
	}

	tab->owner = Pid;
	return A_TRUE;
}

static int update_element_op(term_t *elts, term_t op, int key_pos, int arity, heap_t *hp)
{
	assert(is_tuple(op));
	uint32_t *tdata = peel_tuple(op);
	if (*tdata != 2 || !is_int(tdata[1]))
		return -BAD_ARG;

	int pos = int_value(tdata[1]);
	if (pos < 1 || pos > arity || pos == key_pos)
		return -BAD_ARG;

	elts[pos -1] = tdata[2];
	return 0;
}

// ets:update_element/3 [126]
term_t cbif_ets_update_element3(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Key = regs[1];
	term_t UpdOp = regs[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if ((tab->access == A_PRIVATE || tab->access == A_PROTECTED) &&
			tab->owner != proc->pid)
		badarg(Tid);

	if (tab->type != A_SET && tab->type != A_ORDERED_SET)
		badarg(Tid);

	term_t r = ets_lookup(tab, Key, &proc->hp);
	if (r == nil)
		return A_FALSE;

	assert(is_cons(r));
	term_t *cons = peel_cons(r);
	term_t o = cons[0];
	assert(is_nil(cons[1]));

	assert(is_tuple(o));
	term_t *p = peel_tuple(o);
	int arity = *p++;

	// EXCEPTION POSSIBLE
	term_t *elts = heap_alloc(&proc->hp, arity);
	memcpy(elts, p, arity *sizeof(term_t));
	heap_set_top(&proc->hp, elts +arity);

	if (is_tuple(UpdOp))
	{
		if (update_element_op(elts, UpdOp, tab->key_pos, arity, &proc->hp) < 0)
			badarg(UpdOp);
	}
	else if (is_list(UpdOp))
	{
		term_t t = UpdOp;
		while (is_cons(t))
		{
			term_t *cons = peel_cons(t);
			term_t op = cons[0];
			if (!is_tuple(op))
				break;
			if (update_element_op(elts, op, tab->key_pos, arity, &proc->hp) < 0)
				break;
			t = cons[1];
		}
		if (t != nil)
			badarg(UpdOp);
	}
	else
		badarg(UpdOp);

	ets_insert(tab, elts, arity);
	return A_TRUE;
}

static int update_counter_step(term_t *elts,
		int pos, term_t incr, term_t thresh, term_t set_val, heap_t *hp)
{
	term_t old_val = elts[pos -1];
	term_t new_val;
	if (is_int(old_val) && is_int(incr))
	{
		int s = int_value(old_val) + int_value(incr);
		new_val = int_to_term(s, hp);
	}
	else
		new_val = mixed_add(old_val, incr, hp);

	if (thresh != noval)
	{
		int is_neg = (is_int(incr) && int_value(incr) < 0) ||
			(is_boxed(incr) && (boxed_tag(peel_boxed(incr)) == SUBTAG_NEG_BIGNUM));

		if ((is_neg && is_term_smaller(new_val, thresh)) ||
			(!is_neg && is_term_smaller(thresh, new_val)))
				new_val = set_val;
	}

	if (is_atom(new_val))	// A_BADARITH, A_SYSTEM_LIMIT
		return noval;

	elts[pos -1] = new_val;
	return new_val;
}
		
static term_t update_counter_op(term_t *elts, term_t op, int key_pos, int arity, heap_t *hp)
{
	assert(is_tuple(op));
	uint32_t *tdata = peel_tuple(op);
	if (*tdata == 2)
	{
		// {Pos,Inc}
		if (!is_int(tdata[1]))
			return noval;
		int pos = int_value(tdata[1]);
		if (pos < 1 || pos > arity || pos == key_pos)
			return noval;
		term_t incr = tdata[2];
		if (!is_int(incr) && !is_boxed_bignum(incr))
			return noval;
		return update_counter_step(elts, pos, incr, noval, noval, hp);
	}
	else if (*tdata == 4)
	{
		// {Pos,Incr,Thresh,SetVal}
		if (!is_int(tdata[1]))
			return noval;
		int pos = int_value(tdata[1]);
		if (pos < 1 || pos > arity || pos == key_pos)
			return noval;
		term_t incr = tdata[2];
		term_t thresh = tdata[3];
		term_t set_val = tdata[4];
		if ((!is_int(incr) && !is_boxed_bignum(incr)) ||
			(!is_int(thresh) && !is_boxed_bignum(thresh)) ||
			(!is_int(set_val) && !is_boxed_bignum(set_val)))
				return noval;
		return update_counter_step(elts, pos, incr, thresh, set_val, hp);
	}
	else
		return noval;
}

// ets:update_counter/3 [127]
term_t cbif_ets_update_counter3(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Key = regs[1];
	term_t UpdOp = regs[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if ((tab->access == A_PRIVATE || tab->access == A_PROTECTED) &&
			tab->owner != proc->pid)
		badarg(Tid);

	if (tab->type != A_SET && tab->type != A_ORDERED_SET)
		badarg(Tid);

	term_t r = ets_lookup(tab, Key, &proc->hp);
	if (r == nil)
		badarg(Tid);

	assert(is_cons(r));
	term_t *cons = peel_cons(r);
	term_t o = cons[0];
	assert(is_nil(cons[1]));

	assert(is_tuple(o));
	term_t *p = peel_tuple(o);
	int arity = *p++;

	// EXCEPTION POSSIBLE
	term_t *elts = heap_alloc(&proc->hp, arity);
	memcpy(elts, p, arity *sizeof(term_t));
	heap_set_top(&proc->hp, elts +arity);

	term_t result;
	if (is_int(UpdOp) || is_boxed_bignum(UpdOp))
	{
		int pos = tab->key_pos +1;
		if (pos > arity)
			badarg(UpdOp);
		result = update_counter_step(elts, pos, UpdOp, noval, noval, &proc->hp);
	}
	else if (is_tuple(UpdOp))
		result = update_counter_op(elts, UpdOp, tab->key_pos, arity, &proc->hp);
	else if (is_list(UpdOp))
	{
		term_t vs = nil;
		term_t t = UpdOp;
		while (is_cons(t))
		{
			term_t *cons = peel_cons(t);
			term_t op = cons[0];
			if (!is_tuple(op))
				break;
			term_t v = update_counter_op(elts, op, tab->key_pos, arity, &proc->hp);
			if (v == noval)
				break;
			vs = heap_cons(&proc->hp, v, vs);
			t = cons[1];
		}
		if (t != nil)
			badarg(UpdOp);

		result = nil;
		while (is_cons(vs))
		{
			term_t *cons = peel_cons(vs);
			result = heap_cons(&proc->hp, cons[0], result);
			vs = cons[1];
		}
	}
	else
		badarg(UpdOp);

	if (result == noval)
		badarg();

	ets_insert(tab, elts, arity);
	return result;
}

// ets:setopts/2 [128]
term_t cbif_ets_setopts2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Opts = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (proc->pid != tab->owner)
		badarg(Tid);

	if (is_list(Opts))
	{
		term_t t = Opts;
		while (is_cons(t))
		{
			//NB: options may be applied partially
			term_t *cons = peel_cons(t);
			if (ets_set_opt(tab, cons[0]) < 0)
				badarg(Opts);
			t = cons[1];
		}
		if (!is_nil(t))
			badarg();
	}
	else
	{
		if (ets_set_opt(tab, Opts) < 0)
			badarg(Opts);
	}

	return A_TRUE;
}

// ets:select_delete/2 [129]
term_t cbif_ets_select_delete2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t MatchSpec = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Tid);

	ets_match_spec_t *ms = ets_match_compile_spec(MatchSpec, &proc->hp);
	if (ms == 0)
		badarg(MatchSpec);

	int count = 0;
	ets_match_ctx_t *ctx = ets_match_first(tab, ms, proc->pid, &proc->hp);
	term_t r;
	while ((r = ets_match_next(tab, ctx, &proc->hp)) != noval)
	{
		if (r == A_TRUE)
		{
			ets_match_delete(tab, ctx);
			count++;
		}
	}

	assert(fits_int(count));
	return tag_int(count);
}

// ets:select_reverse/3 [130]
term_t cbif_ets_select_reverse3(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t MatchSpec = regs[1];
	term_t Limit = regs[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (!is_int(Limit) && !is_boxed_bignum(Limit))
		badarg(Limit);
	assert(is_int(Limit)); // TODO
	int lim = int_value(Limit);
	if (lim < 0)
		badarg(Limit);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Tid);

	ets_match_spec_t *ms = ets_match_compile_spec(MatchSpec, &proc->hp);
	if (ms == 0)
		badarg(MatchSpec);

	term_t rs = nil;
	term_t r;
	ets_match_ctx_t *ctx = ets_match_last(tab, ms, proc->pid, &proc->hp);
	int n = lim;
	while (n-- > 0 && (r = ets_match_prev(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	term_t cont = ets_match_make_continuation(tab, MatchSpec, lim, ctx, &proc->hp);
	return heap_tuple2(&proc->hp, list_rev(rs, &proc->hp), cont);
}

// ets:select_reverse/2 [131]
term_t cbif_ets_select_reverse2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t MatchSpec = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Tid);

	ets_match_spec_t *ms = ets_match_compile_spec(MatchSpec, &proc->hp);
	if (ms == 0)
		badarg(MatchSpec);

	term_t rs = nil;
	term_t r;
	ets_match_ctx_t *ctx = ets_match_first(tab, ms, proc->pid, &proc->hp);
	while ((r = ets_match_next(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	// avoid list rev by using first/next instead of last/prev
	return rs;
}

// ets:select_reverse/1 [132]
term_t cbif_ets_select_reverse1(proc_t *proc, term_t *regs)
{
	term_t Cont = regs[0];
	if (Cont == AEOT__)
		return AEOT__;

	if (!is_tuple(Cont))
		badarg(Cont);

	// Cont = {Tid,MatchSpec,Limit,...}
	uint32_t *p = peel_tuple(Cont);
	int arity = *p++;
	if (arity < 3)
		badarg(Cont);

	term_t Tid = p[0];
	term_t MatchSpec = p[1];
	term_t Limit = p[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Cont);
	if (!is_int(Limit) && !is_boxed_bignum(Limit))
		badarg(Cont);
	assert(is_int(Limit)); // TODO
	int lim = int_value(Limit);
	if (lim < 0)
		badarg(Cont);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Cont);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Cont);

	ets_match_spec_t *ms = ets_match_compile_spec(MatchSpec, &proc->hp);
	if (ms == 0)
		badarg(Cont);

	ets_match_ctx_t *ctx = 	ets_match_use_continuation(tab,
								ms, Cont, proc->pid, &proc->hp);
	if (ctx == 0)
		badarg(Cont);

	term_t rs = nil;
	term_t r;
	int n = lim;
	while (n-- > 0 && (r = ets_match_prev(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	term_t cont = ets_match_make_continuation(tab, MatchSpec, lim, ctx, &proc->hp);
	return heap_tuple2(&proc->hp, list_rev(rs, &proc->hp), cont);
}

// ets:select_count/2 [133]
term_t cbif_ets_select_count2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t MatchSpec = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Tid);

	ets_match_spec_t *ms = ets_match_compile_spec(MatchSpec, &proc->hp);
	if (ms == 0)
		badarg(MatchSpec);

	int count = 0;
	ets_match_ctx_t *ctx = ets_match_first(tab, ms, proc->pid, &proc->hp);
	term_t r;
	while ((r = ets_match_next(tab, ctx, &proc->hp)) != noval)
		if (r == A_TRUE)
			count++;

	assert(fits_int(count));
	return tag_int(count);
}

// ets:select/3 [134]
term_t cbif_ets_select3(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t MatchSpec = regs[1];
	term_t Limit = regs[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (!is_int(Limit) && !is_boxed_bignum(Limit))
		badarg(Limit);
	assert(is_int(Limit)); // TODO
	int lim = int_value(Limit);
	if (lim < 0)
		badarg(Limit);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Tid);

	ets_match_spec_t *ms = ets_match_compile_spec(MatchSpec, &proc->hp);
	if (ms == 0)
		badarg(MatchSpec);

	term_t rs = nil;
	term_t r;
	ets_match_ctx_t *ctx = ets_match_first(tab, ms, proc->pid, &proc->hp);
	int n = lim;
	while (n-- > 0 && (r = ets_match_next(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	term_t cont = ets_match_make_continuation(tab, MatchSpec, lim, ctx, &proc->hp);
	return heap_tuple2(&proc->hp, list_rev(rs, &proc->hp), cont);
}

// ets:select/2 [135]
term_t cbif_ets_select2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t MatchSpec = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Tid);

	ets_match_spec_t *ms = ets_match_compile_spec(MatchSpec, &proc->hp);
	if (ms == 0)
		badarg(MatchSpec);

	term_t rs = nil;
	term_t r;
	ets_match_ctx_t *ctx = ets_match_last(tab, ms, proc->pid, &proc->hp);
	while ((r = ets_match_prev(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	// avoid list rev by using last/prev instead of first/next
	return rs; 
}

// ets:select/1 [136]
term_t cbif_ets_select1(proc_t *proc, term_t *regs)
{
	term_t Cont = regs[0];
	if (Cont == AEOT__)
		return AEOT__;

	if (!is_tuple(Cont))
		badarg(Cont);

	// Cont = {Tid,MatchSpec,Limit,...}
	uint32_t *p = peel_tuple(Cont);
	int arity = *p++;
	if (arity < 3)
		badarg(Cont);

	term_t Tid = p[0];
	term_t MatchSpec = p[1];
	term_t Limit = p[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Cont);
	if (!is_int(Limit) && !is_boxed_bignum(Limit))
		badarg(Cont);
	assert(is_int(Limit)); // TODO
	int lim = int_value(Limit);
	if (lim < 0)
		badarg(Cont);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Cont);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Cont);

	ets_match_spec_t *ms = ets_match_compile_spec(MatchSpec, &proc->hp);
	if (ms == 0)
		badarg(Cont);

	ets_match_ctx_t *ctx = ets_match_use_continuation(tab,
								ms, Cont, proc->pid, &proc->hp);
	if (ctx == 0)
		badarg(Cont);

	term_t rs = nil;
	term_t r;
	int n = lim;
	while (n-- > 0 && (r = ets_match_next(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	term_t cont = ets_match_make_continuation(tab, MatchSpec, lim, ctx, &proc->hp);
	return heap_tuple2(&proc->hp, list_rev(rs, &proc->hp), cont);
}

// ets:match_spec_run_r/3 [?]
term_t cbif_ets_match_spec_run_r3(proc_t *proc, term_t *regs)
{
	term_t List = regs[0];
	term_t Ms = regs[1];
	UNUSED term_t Opts = regs[2];

	if (!is_list(List))
		badarg(List);
	if (!is_list(Ms))
		badarg(Ms);

	assert(Opts == nil);	// no documentation

	ets_match_spec_t *ms = ets_match_compile_spec(Ms, &proc->hp);
	if (ms == 0)
		badarg(Ms);

	term_t rs = nil;
	term_t t = List;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		if (is_tuple(cons[0]))
		{
			uint32_t *p = peel_tuple(cons[0]);
			int arity = *p++;
			term_t r = ets_match_spec_run(ms, p, arity, proc->pid, &proc->hp);
			if (r != noval)
				rs = heap_cons(&proc->hp, r, rs);
		}
		t = cons[1];
	}
	if (!is_nil(t))
		badarg(List);

	return rs;
}

// ets:match_spec_compile/1 [137]
term_t cbif_ets_match_spec_compile1(proc_t *proc, term_t *regs)
{
	term_t Ms = regs[0];
	return Ms;	// do nothing
}

// ets:match_object/3 [138]
term_t cbif_ets_match_object3(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t MatchPat = regs[1];
	term_t Limit = regs[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (!is_int(Limit) && !is_boxed_bignum(Limit))
		badarg(Limit);
	assert(is_int(Limit)); // TODO
	int lim = int_value(Limit);
	if (lim < 0)
		badarg(Limit);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Tid);

	ets_match_spec_t *ms = ets_match_compile_pattern(MatchPat, 1, &proc->hp);
	if (ms == 0)
		badarg(MatchSpec);

	term_t rs = nil;
	term_t r;
	ets_match_ctx_t *ctx = ets_match_first(tab, ms, proc->pid, &proc->hp);
	int n = lim;
	while (n-- > 0 && (r = ets_match_next(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	term_t cont = ets_match_make_continuation(tab, MatchPat, lim, ctx, &proc->hp);
	return heap_tuple2(&proc->hp, list_rev(rs, &proc->hp), cont);
}

// ets:match_object/2 [139]
term_t cbif_ets_match_object2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t MatchPat = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Tid);

	ets_match_spec_t *ms = ets_match_compile_pattern(MatchPat, 1, &proc->hp);
	if (ms == 0)
		badarg(MatchSpec);

	term_t rs = nil;
	term_t r;
	ets_match_ctx_t *ctx = ets_match_last(tab, ms, proc->pid, &proc->hp);
	while ((r = ets_match_prev(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	// last/prev used - no need to rev results
	return rs;
}

// ets:match_object/1 [140]
term_t cbif_ets_match_object1(proc_t *proc, term_t *regs)
{
	term_t Cont = regs[0];
	if (!is_tuple(Cont))
		badarg(Cont);

	// Cont = {Tid,MatchPat,Limit,...}
	uint32_t *p = peel_tuple(Cont);
	int arity = *p++;
	if (arity < 3)
		badarg(Cont);

	term_t Tid = p[0];
	term_t MatchPat = p[1];
	term_t Limit = p[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Cont);
	if (!is_int(Limit) && !is_boxed_bignum(Limit))
		badarg(Cont);
	assert(is_int(Limit)); // TODO
	int lim = int_value(Limit);
	if (lim < 0)
		badarg(Cont);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Cont);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Cont);

	ets_match_spec_t *ms = ets_match_compile_pattern(MatchPat, 1, &proc->hp);
	if (ms == 0)
		badarg(Cont);

	ets_match_ctx_t *ctx = ets_match_use_continuation(tab,
								ms, Cont, proc->pid, &proc->hp);
	if (ctx == 0)
		badarg(Cont);

	term_t rs = nil;
	term_t r;
	int n = lim;
	while (n-- > 0 && (r = ets_match_next(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	term_t cont = ets_match_make_continuation(tab, MatchPat, lim, ctx, &proc->hp);
	return heap_tuple2(&proc->hp, list_rev(rs, &proc->hp), cont);
}

// ets:match/3 [141]
term_t cbif_ets_match3(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t MatchPat = regs[1];
	term_t Limit = regs[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (!is_int(Limit) && !is_boxed_bignum(Limit))
		badarg(Limit);
	assert(is_int(Limit)); // TODO
	int lim = int_value(Limit);
	if (lim < 0)
		badarg(Limit);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Tid);

	ets_match_spec_t *ms = ets_match_compile_pattern(MatchPat, 0, &proc->hp);
	if (ms == 0)
		badarg(MatchSpec);

	term_t rs = nil;
	term_t r;
	ets_match_ctx_t *ctx = ets_match_first(tab, ms, proc->pid, &proc->hp);
	int n = lim;
	while (n-- > 0 && (r = ets_match_next(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	term_t cont = ets_match_make_continuation(tab, MatchPat, lim, ctx, &proc->hp);
	return heap_tuple2(&proc->hp, rs, cont);
}

// ets:match/2 [142]
term_t cbif_ets_match2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t MatchPat = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Tid);

	ets_match_spec_t *ms = ets_match_compile_pattern(MatchPat, 0, &proc->hp);
	if (ms == 0)
		badarg(MatchSpec);

	term_t rs = nil;
	term_t r;
	ets_match_ctx_t *ctx = ets_match_first(tab, ms, proc->pid, &proc->hp);
	while ((r = ets_match_next(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	return rs; 
}

// ets:match/1 [143]
term_t cbif_ets_match1(proc_t *proc, term_t *regs)
{
	term_t Cont = regs[0];
	if (Cont == AEOT__)
		return AEOT__;

	if (!is_tuple(Cont))
		badarg(Cont);

	// Cont = {Tid,MatchPat,Limit,...}
	uint32_t *p = peel_tuple(Cont);
	int arity = *p++;
	if (arity < 3)
		badarg(Cont);

	term_t Tid = p[0];
	term_t MatchPat = p[1];
	term_t Limit = p[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Cont);
	if (!is_int(Limit) && !is_boxed_bignum(Limit))
		badarg(Cont);
	assert(is_int(Limit)); // TODO
	int lim = int_value(Limit);
	if (lim < 0)
		badarg(Cont);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Cont);

	if (tab->access == A_PRIVATE && proc->pid != tab->owner)
		badarg(Cont);

	ets_match_spec_t *ms = ets_match_compile_pattern(MatchPat, 0, &proc->hp);
	if (ms == 0)
		badarg(Cont);

	ets_match_ctx_t *ctx = ets_match_use_continuation(tab,
								ms, Cont, proc->pid, &proc->hp);
	if (ctx == 0)
		badarg(Cont);

	term_t rs = nil;
	term_t r;
	int n = lim;
	while (n-- > 0 && (r = ets_match_next(tab, ctx, &proc->hp)) != noval)
		rs = heap_cons(&proc->hp, r, rs);

	term_t cont = ets_match_make_continuation(tab, MatchPat, lim, ctx, &proc->hp);
	return heap_tuple2(&proc->hp, rs, cont);
}

// ets:slot/2 [144]
term_t cbif_ets_slot2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t N = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && tab->owner != proc->pid)
		badarg();

	assert(is_int(N));	//TODO
	int n = int_value(N);

	term_t r = ets_slot(tab, n, &proc->hp);
	if (r == noval)
		badarg();

	return r;
}

// ets:rename/2 [145]
term_t cbif_ets_rename2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t NewName = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (!is_atom(NewName))
		badarg(NewName);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->owner != proc->pid)
		badarg();

	// EXCEPTION POSSIBLE
	ets_table_rename(tab, NewName);
	return tab->tid;
}

// ets:prev/2 [146]
term_t cbif_ets_prev2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Key = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (Key == AEOT__)
		badarg(Key);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && tab->owner != proc->pid)
		badarg();

	term_t prev_key = ets_prev(tab, Key, &proc->hp);
	if (prev_key == noval)	// Key not found
		badarg(Key);

	return prev_key;
}

// ets:next/2 [147]
term_t cbif_ets_next2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Key = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (Key == AEOT__)
		badarg(Key);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if (tab->access == A_PRIVATE && tab->owner != proc->pid)
		badarg();

	term_t next_key = ets_next(tab, Key, &proc->hp);
	if (next_key == noval)	// Key not found
		badarg(Key);

	return next_key;
}

// ets:member/2 [148]
term_t cbif_ets_member2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Key = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);
	
	if (tab->access == A_PRIVATE && tab->owner != proc->pid)
		badarg();

	return ets_member(tab, Key);
}

// ets:last/1 [149]
term_t cbif_ets_last1(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);
	
	if (tab->access == A_PRIVATE && tab->owner != proc->pid)
		badarg();

	return ets_last(tab, &proc->hp);
}

// ets:is_compiled_ms/1 [150]
term_t cbif_ets_is_compiled_ms1(proc_t *proc, term_t *regs)
{
	return A_TRUE;	// is this enough?
}

// ets:insert_new/2 [?]
term_t cbif_ets_insert_new2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Objs = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (!is_tuple(Objs) && !is_list(Objs))
		badarg(Objs);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if ((tab->access == A_PRIVATE || tab->access == A_PROTECTED) &&
			tab->owner != proc->pid)
		badarg(Tid);

	if (is_tuple(Objs))
	{
		uint32_t *p = peel_tuple(Objs);
		int arity = *p++;
		if (arity < tab->key_pos)
			badarg(Objs);
		if (ets_member(tab, p[tab->key_pos -1]) == A_TRUE)
			return A_FALSE;
		ets_insert(tab, p, arity);
	}
	else
	{
		// insert must be atomic even if 'no memory' occurs
		assert(is_list(Objs));

		term_t t = Objs;
		while (is_cons(t))
		{
			term_t *cons = peel_cons(t);
			if (!is_tuple(cons[0]))
				badarg(Objs);
			uint32_t *p = peel_tuple(cons[0]);
			int arity = *p++;
			if (arity < tab->key_pos)
				badarg(Objs);
			if (ets_member(tab, p[tab->key_pos -1]) == A_TRUE)
				return A_FALSE;
			t = cons[1];
		}
		if (!is_nil(t))
			badarg(Objs);

		ets_insert_many(tab, Objs);
	}
	return A_TRUE;

}


// ets:insert/2 [151]
term_t cbif_ets_insert2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Objs = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (!is_tuple(Objs) && !is_list(Objs))
		badarg(Objs);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	if ((tab->access == A_PRIVATE || tab->access == A_PROTECTED) &&
			tab->owner != proc->pid)
		badarg(Tid);

	if (is_tuple(Objs))
	{
		uint32_t *p = peel_tuple(Objs);
		int arity = *p++;
		if (arity < tab->key_pos)
			badarg(Objs);
		ets_insert(tab, p, arity);
	}
	else
	{
		// insert must be atomic even if 'no memory' occurs
		assert(is_list(Objs));

		term_t t = Objs;
		while (is_cons(t))
		{
			term_t *cons = peel_cons(t);
			if (!is_tuple(cons[0]))
				badarg(Objs);
			uint32_t *tdata = peel_tuple(cons[0]);
			if (*tdata < tab->key_pos)
				badarg(Objs);
			t = cons[1];
		}
		if (!is_nil(t))
			badarg(Objs);

		ets_insert_many(tab, Objs);
	}
	return A_TRUE;
}

// ets:lookup_element/3 [152]
term_t cbif_ets_lookup_element3(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Key = regs[1];
	term_t Pos = regs[2];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (!is_int(Pos))
		badarg(Pos);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);
	
	if (tab->access == A_PRIVATE && tab->owner != proc->pid)
		badarg(Tid);

	int pos = int_value(Pos);
	if (pos <= 0)
		badarg(Pos);
	term_t objs = ets_lookup(tab, Key, &proc->hp);
	if (objs == nil)
		badarg(Key);

	assert(is_cons(objs));
	if (tab->type == A_BAG || tab->type == A_DUPLICATE_BAG)
	{
		term_t t = objs;
		term_t vs = nil;
		while (is_cons(t))
		{
			term_t *cons = peel_cons(t);
			assert(is_tuple(cons[0]));
			uint32_t *p = peel_tuple(cons[0]);
			int arity = *p++;
			if (pos > arity)
				badarg(Pos);
			// EXCEPTION POSSIBLE
			vs = heap_cons(&proc->hp, p[pos -1], vs);
			t = cons[1];
		}
		assert(is_nil(t));

		// the time order must be preserved
		t = vs;
		term_t rs = nil;
		while (is_cons(t))
		{
			term_t *cons = peel_cons(t);
			rs = heap_cons(&proc->hp, cons[0], rs);
			t = cons[1];
		}
		return rs;
	}
	else
	{
		term_t *cons = peel_cons(objs);
		term_t obj = cons[0];
		assert(is_nil(cons[1]));
		assert(is_tuple(obj));
		uint32_t *p = peel_tuple(obj);
		int arity = *p++;
		if (pos > arity)
			badarg(Pos);
		return p[pos -1];
	}
}

// ets:lookup/2 [153]
term_t cbif_ets_lookup2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Key = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);
	
	if (tab->access == A_PRIVATE && tab->owner != proc->pid)
		badarg();

	return ets_lookup(tab, Key, &proc->hp);
}

// ets:safe_fixtable/2 [154]
term_t cbif_ets_safe_fixtable2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Flag = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (!is_bool(Flag))
		badarg(Flag);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);

	int x = (Flag == A_TRUE)
		?ets_fix_table(tab, proc->pid)
		:ets_unfix_table(tab, proc->pid);
	if (x < 0)
	{
		assert(x == -TOO_LONG);
		fail(A_SYSTEM_LIMIT);
	}

	return A_TRUE;
}

// ets:info/2 [155]
term_t cbif_ets_info2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Item = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);
	if (!is_atom(Item))
		badarg(Item);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		return A_UNDEFINED;

	if (Item == A_MEMORY)
		return int_to_term(tab->total_alloc, &proc->hp);
	else if (Item == A_OWNER)
		return tab->owner;
	else if (Item == A_HEIR)
		return tab->heir;
	else if (Item == A_NAME)
		return tab->name;
	else if (Item == A_SIZE)
		return int_to_term(tab->count, &proc->hp);
	else if (Item == A_NODE)
		return cluster_node;
	else if (Item == A_NAMED_TABLE)
		return (tab->tid == tab->name) ?A_TRUE :A_FALSE;
	else if (Item == A_TYPE)
		return tab->type;
	else if (Item == A_KEYPOS)
		return tag_int(tab->key_pos);
	else if (Item == A_PROTECTION)
		return tab->access;
	else if (Item == A_COMPRESSED)
		return A_FALSE;
	else if (Item == A_FIXED)
		return (tab->fixed > 0) ?A_TRUE :A_FALSE;
	else if (Item == A_SAFE_FIXED)
		return ets_fix_info(tab, &proc->hp);
	else if (Item == A_STATS);
		return A_FALSE;
	
	badarg(Item);
}

// ets:info/1 [156]
term_t cbif_ets_info1(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		return A_UNDEFINED;

	term_t info = nil;
	term_t t = heap_tuple2(&proc->hp, A_MEMORY,
			int_to_term(tab->total_alloc, &proc->hp));
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_OWNER, tab->owner);
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_HEIR, tab->heir);
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_NAME, tab->name);
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_SIZE,
			int_to_term(tab->count, &proc->hp));
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_NODE, cluster_node);
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_NAMED_TABLE,
			(tab->tid == tab->name) ?A_TRUE :A_FALSE);
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_TYPE, tab->type);
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_KEYPOS, tag_int(tab->key_pos));
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_PROTECTION, tab->access);
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_COMPRESSED, A_FALSE);
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_FIXED,
			(tab->fixed > 0) ?A_TRUE :A_FALSE);
	info = heap_cons(&proc->hp, t, info);
	t = heap_tuple2(&proc->hp, A_SAFE_FIXED,
			ets_fix_info(tab, &proc->hp));
	info = heap_cons(&proc->hp, t, info);

	return info;
}

// ets:first/1 [157]
term_t cbif_ets_first1(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);
	
	if (tab->access == A_PRIVATE && tab->owner != proc->pid)
		badarg();

	return ets_first(tab, &proc->hp);
}

// ets:delete_all_objects/1 [?]
term_t cbif_ets_delete_all_objects1(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);
	
	if ((tab->access == A_PRIVATE || tab->access == A_PROTECTED) &&
			tab->owner != proc->pid)
		badarg(Tid);

	ets_delete_all_objects(tab);
	return A_TRUE;
}

// ets:delete_object/2 [?]
term_t cbif_ets_delete_object2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Obj = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);
	
	if ((tab->access == A_PRIVATE || tab->access == A_PROTECTED) &&
			tab->owner != proc->pid)
		badarg(Tid);

	if (!is_tuple(Obj))
		badarg(Obj);
	uint32_t *p = peel_tuple(Obj);
	int arity = *p++;
	if (arity < tab->key_pos)
		badarg(Obj);

	ets_delete_object(tab, p, arity);
	return A_TRUE;
}

// ets:delete/2 [158]
term_t cbif_ets_delete2(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	term_t Key = regs[1];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);
	
	if ((tab->access == A_PRIVATE || tab->access == A_PROTECTED) &&
			tab->owner != proc->pid)
		badarg(Tid);

	ets_delete(tab, Key);
	return A_TRUE;
}

// ets:delete/1 [159]
term_t cbif_ets_delete1(proc_t *proc, term_t *regs)
{
	term_t Tid = regs[0];
	if (!is_atom(Tid) && !is_int(Tid))
		badarg(Tid);

	ets_table_t *tab = ets_table_lookup(Tid);
	if (tab == 0)
		badarg(Tid);
	
	ets_table_delete(tab);
	return A_TRUE;
}

// ets:new/2 [160]
term_t cbif_ets_new2(proc_t *proc, term_t *regs)
{
	term_t Name = regs[0];
	term_t Opts = regs[1];
	if (!is_atom(Name))
		badarg(Name);
	if (!is_list(Opts))
		badarg(Opts);

	int is_named = 0;
	term_t type = A_SET;
	term_t access = A_PROTECTED;
	int key_pos = 1;
	term_t heir = A_NONE;
	term_t heir_data = noval;
	UNUSED int compressed = 0;
	UNUSED term_t write_concurrency = A_FALSE;
	UNUSED term_t read_concurrency = A_FALSE;

	term_t t = Opts;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		term_t o = cons[0];
		if (o == A_SET)
			type = A_SET;
		else if (o == A_ORDERED_SET)
			type = A_ORDERED_SET;
		else if (o == A_BAG)
			type = A_BAG;
		else if (o == A_DUPLICATE_BAG)
			type = A_DUPLICATE_BAG;
		else if (o == A_PUBLIC)
			access = A_PUBLIC;
		else if (o == A_PROTECTED)
			access = A_PROTECTED;
		else if (o == A_PRIVATE)
			access = A_PRIVATE;
		else if (o == A_NAMED_TABLE)
			is_named = 1;
		else if (o == A_COMPRESSED)
			compressed = 1;
		else if is_tuple(o)
		{
			uint32_t *tdata = peel_tuple(o);
			if (*tdata == 2 && tdata[1] == A_KEYPOS)
			{
				if (!is_int(tdata[2]))
					badarg(Opts);
				key_pos = int_value(tdata[2]);
				if (key_pos < 1)
					badarg(Opts);
			}
			else if (*tdata == 2 && tdata[1] == A_HEIR && tdata[2] == A_NONE)
				heir = A_NONE;
			else if (*tdata == 3 && tdata[1] == A_HEIR)
			{
				if (!is_short_pid(tdata[2]))
					badarg(Opts);
				heir = tdata[2];
				heir_data = tdata[3];
			}
			else if (*tdata == 2 && tdata[1] == A_WRITE_CONCURRENCY && is_bool(tdata[2]))
				write_concurrency = tdata[2];
			else if (*tdata == 2 && tdata[1] == A_READ_CONCURRENCY && is_bool(tdata[2]))
				read_concurrency = tdata[2];
			else
				badarg(Opts);
		}
		else
			badarg(Opts);

		t = cons[1];
	}
	if (!is_nil(t))
		badarg(Opts);

	if (heir != A_NONE && scheduler_lookup(heir) == 0)
	{
		heir = A_NONE;
		heir_data = noval;
	}

	ets_table_t *new_tab = ets_table_make(Name, is_named, type,
			access, key_pos, proc->pid, heir, heir_data);
	if (new_tab == 0)
		badarg(Name);	// Name already taken

	return new_tab->tid;
}

// ets:all/0 [161]
term_t cbif_ets_all0(proc_t *proc, term_t *regs)
{
	return ets_all_tables(&proc->hp);
}

//EOF
