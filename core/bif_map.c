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
	term_t Key = regs[0];
	term_t Map = regs[1];

	if (!is_boxed_map(Map))
		badarg(Map);

	t_map_t *m = (t_map_t *)peel_boxed(Map);
	int index = map_key_index(Key, m->keys);
	if (index < 0)
		return Map;

	uint32_t *p = peel_tuple(m->keys);
	int size = *p++;
	term_t *ks = p;

	int needed = 1 +size-1 +WSIZE(t_map_t) +size-1;
	uint32_t *htop = heap_alloc(&proc->hp, needed);
	term_t keys = tag_tuple(htop);
	*htop++ = size-1;
	memcpy(htop, ks, index *sizeof(term_t));
	htop += index;
	memcpy(htop, ks +index +1, (size -index -1) *sizeof(term_t));
	htop += (size -index -1);
	term_t out = tag_boxed(htop);
	t_map_t *m1 = (t_map_t *)htop;
	box_map(htop, size-1, keys);
	heap_set_top(&proc->hp, htop);

	memcpy(m1->values, m->values, index *sizeof(term_t));
	memcpy(m1->values +index +1,
		    m->values +index +1, (size -index -1) *sizeof(term_t));
 
	return out;	
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
	term_t Map1 = regs[0];
	term_t Map2 = regs[1];

	if (!is_boxed_map(Map1))
		badarg(Map1);
	if (!is_boxed_map(Map2))
		badarg(Map2);

	t_map_t *m1 = (t_map_t *)peel_boxed(Map1);
	uint32_t *p1 = peel_tuple(m1->keys);
	int size1 = *p1++;
	term_t *ks1 = p1;
	term_t *vs1 = m1->values;
	t_map_t *m2 = (t_map_t *)peel_boxed(Map2);
	uint32_t *p2 = peel_tuple(m2->keys);
	int size2 = *p2++;
	term_t *ks2 = p2;
	term_t *vs2 = m2->values;

	term_t mks[size1+size2];	//XXX: stack overflow
	term_t mvs[size1+size2];

	term_t *ks3 = mks;
	term_t *vs3 = mvs;

	int ss1 = size1;
	int ss2 = size2;

	int size = 0;
	while (size1 > 0 && size2 > 0)
	{
		term_t a = *ks1;
		term_t b = *ks2;
		if (is_term_smaller(a, b))
		{
			*ks3++ = *ks1++;
			*vs3++ = *vs1++;
			size1--;
		}
		else if (a == b || are_terms_equal(a, b, 1))
		{
			ks1++; vs1++;
			size1--;
			*ks3++ = *ks2++;
			*vs3++ = *vs2++;
			size2--;
		}
		else
		{
			*ks3++ = *ks2++;
			*vs3++ = *vs2++;
			size2--;
		}
		size++;
	}

	while (size1-- > 0)
	{
		*ks3++ = *ks1++;
		*vs3++ = *vs1++;
		size++;
	}

	while (size2-- > 0)
	{
		*ks3++ = *ks2++;
		*vs3++ = *vs2++;
		size++;
	}

	if (size == ss1 || size == ss2)
	{
		// reuse keys
		term_t keys = (size == ss1) ?m1->keys :m2->keys;
		int needed = WSIZE(t_map_t) +size;
		uint32_t *p = heap_alloc(&proc->hp, needed);
		term_t out = tag_boxed(p);
		term_t *values = p +WSIZE(t_map_t);
		box_map(p, size, keys);
		heap_set_top(&proc->hp, p);
		memcpy(values, mvs, size *sizeof(term_t));
		return out;
	}
	else
	{
		// new keys
		int needed = 1 +size +WSIZE(t_map_t) +size;
		uint32_t *p = heap_alloc(&proc->hp, needed);
		term_t keys = tag_tuple(p);
		*p++ = size;
		memcpy(p, mks, size *sizeof(term_t));
		term_t out = tag_boxed(p);
		term_t *values = p +WSIZE(t_map_t);
		box_map(p, size, keys);
		heap_set_top(&proc->hp, p);
		memcpy(values, mvs, size *sizeof(term_t));
		return out;
	}
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
	term_t List = regs[0];
	if (!is_list(List))
		badarg(List);

	int len = list_len(List);
	term_t ks[len];		//XXX: imminent stack overflow
	term_t vs[len];
	int n = 0;

	term_t l = List;
	while (is_cons(l))
	{
		term_t *cons = peel_cons(l);
		if (!is_tuple(cons[0]))
			badarg(List);
		uint32_t *p = peel_tuple(cons[0]);
		if (*p++ != 2)
			badarg(List);
		term_t k = *p++;
		term_t v = *p++;

		if (n == 0 || is_term_smaller(k, ks[0]))
		{
			memmove(ks +1, ks, n *sizeof(term_t));
			memmove(vs +1, vs, n *sizeof(term_t));
			ks[0] = k;
			vs[0] = v;
			n++;
		}
		else
		{
			term_t *alpha = ks;
			term_t *beta = ks +n;
			// *alpha =< k
			while (beta > alpha+1)
			{
				term_t *mid = alpha + (beta -alpha +1)/2;
				if (is_term_smaller(k, *mid))
					beta = mid;
				else
					alpha = mid;
			}
			assert(beta == alpha+1);
			int index = alpha -ks;
			if (k == *alpha || are_terms_equal(k, *alpha, 1))
				vs[index] = v;
			else
			{
				index++;	// ks[index] > k now
				memmove(ks +index +1, ks +index, (n -index) *sizeof(term_t));
				memmove(vs +index +1, vs +index, (n -index) *sizeof(term_t));
				ks[index] = k;
				vs[index] = v;
				n++;
			}
		}
		l = cons[1];
	}

	if (!is_nil(l))
		badarg(List);

	int needed = 1 +n + WSIZE(t_map_t) +n;
	uint32_t *htop = heap_alloc(&proc->hp, needed);
	term_t keys = tag_tuple(htop);
	*htop++ = n;
	memcpy(htop, ks, n *sizeof(term_t));
	htop += n;
	term_t out = tag_boxed(htop);
	term_t *values = htop +WSIZE(t_map_t);
	box_map(htop, n, keys);
	heap_set_top(&proc->hp, htop);
	memcpy(values, vs, n *sizeof(term_t));

	return out;	
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

