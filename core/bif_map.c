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

// maps:values/1 [17]
term_t cbif_values1(proc_t *proc, term_t *regs)
{
	term_t Map = regs[0];
	if (!is_boxed_map(Map))
		badarg();

	term_t out = nil;
	t_map_t *m = (t_map_t *)peel_boxed(Map);
	int n = map_size(m);
	term_t *v = m->values;

	v += n;
	while (n-- > 0)
	{
		v--;
		out = heap_cons(&proc->hp, *v, out);
	}
	
	return out;
}

// maps:update/3 [18]
term_t cbif_update3(proc_t *proc, term_t *regs)
{
	term_t Key   = regs[0];
	term_t Value = regs[1];
	term_t Map   = regs[2];

	if (!is_boxed_map(Map))
		badarg(Map);

	t_map_t *m0 = (t_map_t *)peel_boxed(Map);
	int index = map_key_index(Key, m0->keys);
	if (index < 0)
		badarg(Key);
	int size = map_size(m0);
	int needed = WSIZE(t_map_t) +size;
	uint32_t *p = heap_alloc(&proc->hp, needed);
	t_map_t *m1 = (t_map_t *)p;
	box_map(p, size, m0->keys);
	heap_set_top(&proc->hp, p);

	memcpy(m1->values, m0->values, size *sizeof(term_t));
	m1->values[index] = Value;

	return tag_boxed(m1);
}

// maps:to_list/1 [19]
term_t cbif_to_list1(proc_t *proc, term_t *regs)
{
	term_t Map = regs[0];
	if (!is_boxed_map(Map))
		badarg(Map);

	term_t out = nil;

	t_map_t *m = (t_map_t *)peel_boxed(Map);
	term_t *p = peel_tuple(m->keys);
	int n = *p++;
	term_t *v = m->values;

	p += n;
	v += n;
	while (n-- > 0)
	{
		p--; v--;
		term_t kv = heap_tuple2(&proc->hp, *p, *v);
		out = heap_cons(&proc->hp, kv, out);
	}

	return out;
}

// maps:remove/2 [20]
term_t cbif_remove2(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

// maps:put/3 [21]
term_t cbif_put3(proc_t *proc, term_t *regs)
{
	term_t Key   = regs[0];
	term_t Value = regs[1];
	term_t Map   = regs[2];

	if (!is_boxed_map(Map))
		badarg(Map);

	t_map_t *m0 = (t_map_t *)peel_boxed(Map);
	int index = map_key_index(Key, m0->keys);
	if (index >= 0)
	{
		// same as update/3
		int size = map_size(m0);
		int needed = WSIZE(t_map_t) +size;
		uint32_t *p = heap_alloc(&proc->hp, needed);
		t_map_t *m1 = (t_map_t *)p;
		box_map(p, size, m0->keys);
		heap_set_top(&proc->hp, p);

		memcpy(m1->values, m0->values, size *sizeof(term_t));
		m1->values[index] = Value;

		return tag_boxed(m1);
	}
	else
	{
		uint32_t *q = peel_tuple(m0->keys);
		int size = *q++;
		term_t *ks = q;
	
		term_t kvs[] = {Key,Value};

		int needed = 1 +size+1 +2 +size+1;
		uint32_t *p = heap_alloc(&proc->hp, needed);
		term_t keys = tag_tuple(p);
		*p++ = size+1;
		term_t *ks1 = p;
		p += size+1;
		term_t out = tag_boxed(p);
		term_t *vs1 = p +WSIZE(t_map_t);
		box_map(p, size+1, keys);
		heap_set_top(&proc->hp, p);

		int size1 = map_merge(ks, m0->values, size, kvs, 1, ks1, vs1);
		assert(size1 == size+1);
		
		return out;
	}
}

// maps:new/0 [22]
term_t cbif_new0(proc_t *proc, term_t *regs)
{
	uint32_t *p = heap_alloc(&proc->hp, 2);
	term_t map = tag_boxed(p);
	box_map(p, 0, ZERO_TUPLE);
	heap_set_top(&proc->hp, p);
	return map;
}

// maps:merge/2 [23]
term_t cbif_merge2(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

// maps:keys/1 [24]
term_t cbif_keys1(proc_t *proc, term_t *regs)
{
	term_t Map = regs[0];
	if (!is_boxed_map(Map))
		badarg(Map);

	t_map_t *m = (t_map_t *)peel_boxed(Map);
	uint32_t *p = peel_tuple(m->keys);
	return heap_vector_to_list(&proc->hp, p+1, *p);
}

// maps:is_key/2 [25]
term_t cbif_is_key2(proc_t *proc, term_t *regs)
{
	term_t Key = regs[0];
	term_t Map = regs[1];

	if (!is_boxed_map(Map))
		badarg(Map);

	t_map_t *m = (t_map_t *)peel_boxed(Map);
	int index = map_key_index(Key, m->keys);
	return (index < 0) ?A_FALSE :A_TRUE;
}

// maps:from_list/1 [26]
term_t cbif_from_list1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

// maps:find/2 [27]
term_t cbif_find2(proc_t *proc, term_t *regs)
{
	term_t Key = regs[0];
	term_t Map = regs[1];

	if (!is_boxed_map(Map))
		badarg(Map);

	t_map_t *m = (t_map_t *)peel_boxed(Map);
	int index = map_key_index(Key, m->keys);
	if (index < 0)
		return A_ERROR;
	return m->values[index];
}

// maps:get/2 [28]
term_t cbif_get2(proc_t *proc, term_t *regs)
{
	term_t Key = regs[0];
	term_t Map = regs[1];

	if (!is_boxed_map(Map))
		badarg(Map);

	t_map_t *m = (t_map_t *)peel_boxed(Map);
	int index = map_key_index(Key, m->keys);
	if (index < 0)
		fail(A_BAD_KEY);
	return m->values[index];
}

