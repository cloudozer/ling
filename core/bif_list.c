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

#include "bif_impl.h"

term_t cbif_member2(proc_t *proc, term_t *regs)
{
	term_t Item = regs[0];
	term_t List = regs[1];

	if (!is_list(List))
		badarg(List);

	term_t l = List;
	while (is_cons(l))
	{
		term_t *cons = peel_cons(l);
		if (cons[0] == Item || are_terms_equal(cons[0], Item, 1))
			return A_TRUE;
		l = cons[1];
	}

	if (!is_nil(l))
		badarg(List);

	return A_FALSE;
}

term_t cbif_reverse2(proc_t *proc, term_t *regs)
{
	//TODO: non-blocking version that processes;
	// a fixed number of elements at a time
	
	term_t List = regs[0];
	term_t Tail = regs[1];

	if (!is_list(List))
		badarg(List);
	if (!is_list(Tail))
		badarg(Tail);

	if (is_nil(List))
		return Tail;
	
	int len = list_len(List);
	if (len < 0)
		badarg(List);	// the first list can not be odd

	uint32_t *htop = proc_burn_fat(proc, len*2, regs, 2);

	// reload after gc
	List = regs[0];
	Tail = regs[1];

	term_t tl = Tail;
	uint32_t *cons = peel_cons(List);
	term_t result = noval;

	do {
		uint32_t *new_cons = htop;
		new_cons[0] = cons[0];
		new_cons[1] = tl;
		htop += 2;

		if (is_nil(cons[1]))
		{
			result = tag_cons(new_cons);
			break;
		}

		tl = tag_cons(new_cons);
		cons = peel_cons(cons[1]);
	} while (1);

	heap_set_top(&proc->hp, htop);
	return result;
}

term_t cbif_keymember3(proc_t *proc, term_t *regs)
{
	term_t Key = regs[0];
	term_t N = regs[1];
	term_t Tuples = regs[2];

	if (!is_int(N))
		badarg(N);
	if (!is_list(Tuples))
		badarg(Tuples);

	int pos = int_value(N);
	if (pos <= 0)
		badarg(N);

	term_t l = Tuples;
	while (is_cons(l))
	{
		term_t *cons = peel_cons(l);
		if (is_tuple(cons[0]))
		{
			uint32_t *tdata = peel_tuple(cons[0]);
			if (*tdata >= pos &&
					(tdata[pos] == Key || are_terms_equal(tdata[pos], Key, 0)))
				return A_TRUE;
		}
		l = cons[1];
	}

	if (!is_nil(l))
		badarg(Tuples);

	return A_FALSE;
}

term_t cbif_keysearch3(proc_t *proc, term_t *regs)
{
	// TODO: may block - make non-blocking?

	term_t Key = regs[0];
	term_t N = regs[1];
	term_t Tuples = regs[2];

	if (!is_int(N))
		badarg(N);
	if (!is_list(Tuples))
		badarg(Tuples);

	int pos = int_value(N);
	if (pos <= 0)
		badarg(N);

	term_t l = Tuples;
	while (is_cons(l))
	{
		uint32_t *cons = peel_cons(l);
		term_t el = cons[0];
		if (is_tuple(el))
		{
			uint32_t *term_data = peel_tuple(el);
			if (*term_data >= pos)
			{
				term_t k = term_data[pos];	// pos is 1-based
				if (k == Key || are_terms_equal(k, Key, 0))
					return heap_tuple2(&proc->hp, A_VALUE, el);
			}
		}
		l = cons[1];
	}

	if (!is_nil(l))
		badarg(Tuples);

	return A_FALSE;
}

term_t cbif_keyfind3(proc_t *proc, term_t *regs)
{
	// TODO: may block - make non-blocking?

	term_t Key = regs[0];
	term_t N = regs[1];
	term_t Tuples = regs[2];

	if (!is_int(N))
		badarg(N);
	if (!is_list(Tuples))
		badarg(Tuples);

	int pos = int_value(N);
	if (pos <= 0)
		badarg(N);

	term_t l = Tuples;
	while (is_cons(l))
	{
		uint32_t *cons = peel_cons(l);
		term_t el = cons[0];
		if (is_tuple(el))
		{
			uint32_t *term_data = peel_tuple(el);
			if (*term_data >= pos)
			{
				term_t k = term_data[pos];	// pos is 1-based
				if (k == Key || are_terms_equal(k, Key, 0))
					return el;
			}
		}
		l = cons[1];
	}

	if (!is_nil(l))
		badarg(Tuples);

	return A_FALSE;
}

//EOF
