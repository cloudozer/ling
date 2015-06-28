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

#include "heap.h"

#include <math.h>

#include "ling_common.h"

#include "nalloc.h"
#include "string.h"
#include "bits.h"
#include "bignum.h"
#include "stack.h"
#include "atom_defs.h"

#include "mm.h"

#define DEFAULT_FULL_SWEEP_AFTER	65535

static int copy_terms(int depth, heap_t *hp, term_t *terms, int num);

void heap_init(heap_t *hp, uint32_t *init_starts, uint32_t *init_ends)
{
	memset(hp, 0, sizeof(*hp));

	hp->init_node.starts = init_starts;
	hp->init_node.ends = init_ends;
	//hp->init_node.next = 0;
	//hp->init_node.index = 0;
	hp->init_node_threshold = init_starts;
	
	hp->nodes = &hp->init_node;
	//hp->total_size = 0;
	//hp->proc_bins = 0;
	//hp->total_pb_size = 0;
	//hp->suppress_gc = 0;

	hp->full_sweep_after = DEFAULT_FULL_SWEEP_AFTER;
	//hp->sweep_after_count = 0;
	//hp->minor_gcs = 0;
	//hp->expected_top = 0;

	hp->gc_scythe = hp->nodes;
	//hp->gc_counters = 0;
}

void heap_reset_init_node_end(heap_t *hp, uint32_t *ends)
{
	assert(ends >= hp->init_node.ends);
	hp->init_node.ends = ends;
}

uint32_t *heap_alloc(heap_t *hp, int needed)
{
	if (hp->nodes->ends - hp->nodes->starts >= needed)
	{
#ifdef LING_DEBUG
		hp->expected_top = hp->nodes->starts +needed;
#endif
		return hp->nodes->starts;
	}
	
	int req_words = heap_chunk_size(needed, hp->total_size);
	memnode_t *new_node = nalloc(req_words*sizeof(uint32_t));
	new_node->next = hp->nodes;
	hp->nodes = new_node;

#ifdef LING_DEBUG
	hp->expected_top = new_node->starts +needed;
#endif
	return new_node->starts;
}

uint32_t *heap_alloc_N(heap_t *hp, int needed)
{
	if (hp->nodes->ends - hp->nodes->starts >= needed)
	{
#ifdef LING_DEBUG
		hp->expected_top = hp->nodes->starts +needed;
#endif
		return hp->nodes->starts;
	}

	int req_words = heap_chunk_size(needed, hp->total_size);
	memnode_t *new_node = nalloc_N(req_words*sizeof(uint32_t));
	if (new_node == 0)
		return 0;
	new_node->next = hp->nodes;
	hp->nodes = new_node;

#ifdef LING_DEBUG
	hp->expected_top = new_node->starts +needed;
#endif
	return new_node->starts;
}

void *heap_top(heap_t *hp)
{
	return hp->nodes->starts;
}

// A version that does not check the expected value
void heap_set_top0(heap_t *hp, uint32_t *new_top)
{
	assert(new_top != 0);
	memnode_t *node = hp->nodes;
	assert(new_top <= node->ends);
	hp->total_size += (new_top - node->starts);
	node->starts = new_top;
}

#ifdef LING_DEBUG
void heap_set_top(heap_t *hp, uint32_t *new_top)
{
	assert(new_top != 0);
	assert(hp->expected_top == 0 || hp->expected_top == new_top);
	memnode_t *node = hp->nodes;
	assert(new_top <= node->ends);
	hp->total_size += (new_top - node->starts);
	node->starts = new_top;
}
#endif

uint32_t *heap_end(heap_t *hp)
{
	return hp->nodes->ends;
}

void heap_done(heap_t *hp)
{
	// Unlink proc bins
	while (hp->proc_bins != 0)
		proc_bin_unlink(hp->proc_bins, &hp->total_pb_size);

	memnode_t *node = hp->nodes;
	while (node != &hp->init_node)
	{
		memnode_t *next_node = node->next;
		nfree(node);
		node = next_node;
	}
}

term_t heap_cons(heap_t *hp, term_t hd, term_t tl)
{
	uint32_t *htop = heap_alloc(hp, 2);
	htop[0] = hd;
	htop[1] = tl;
	heap_set_top(hp, htop +2);
	return tag_cons(htop);
}

term_t heap_tuple2(heap_t *hp, term_t e1, term_t e2)
{
	uint32_t *htop = heap_alloc(hp, 3);
	htop[0] = 2;
	htop[1]= e1;
	htop[2] = e2;
	heap_set_top(hp, htop+3);
	return tag_tuple(htop);
}

term_t heap_tuple3(heap_t *hp, term_t e1, term_t e2, term_t e3)
{
	uint32_t *htop = heap_alloc(hp, 4);
	htop[0] = 3;
	htop[1]= e1;
	htop[2] = e2;
	htop[3] = e3;
	heap_set_top(hp, htop+4);
	return tag_tuple(htop);
}

term_t heap_tuple4(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4)
{
	uint32_t *htop = heap_alloc(hp, 5);
	htop[0] = 4;
	htop[1]= e1;
	htop[2] = e2;
	htop[3] = e3;
	htop[4] = e4;
	heap_set_top(hp, htop+5);
	return tag_tuple(htop);
}

term_t heap_tuple5(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5)
{
	uint32_t *htop = heap_alloc(hp, 6);
	htop[0] = 5;
	htop[1]= e1;
	htop[2] = e2;
	htop[3] = e3;
	htop[4] = e4;
	htop[5] = e5;
	heap_set_top(hp, htop+6);
	return tag_tuple(htop);
}

term_t heap_tuple6(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6)
{
	uint32_t *htop = heap_alloc(hp, 7);
	htop[0] = 6;
	htop[1]= e1;
	htop[2] = e2;
	htop[3] = e3;
	htop[4] = e4;
	htop[5] = e5;
	htop[6] = e6;
	heap_set_top(hp, htop+7);
	return tag_tuple(htop);
}

term_t heap_strz_N(heap_t *hp, const char *s)
{
	return heap_str_N(hp, s, strlen(s));
}

term_t heap_strz(heap_t *hp, const char *s)
{
	return heap_str(hp, s, strlen(s));
}

term_t heap_str_N(heap_t *hp, const char *s, int len)
{
	if (len == 0)
		return nil;
	uint32_t *htop = heap_alloc_N(hp, 2*len);
	if (htop == 0)
		return noval;
	term_t r = tag_cons(htop);
	for (int i = 0; i < len; i++)
	{
		htop[0] = tag_int((uint8_t)s[i]);
		htop[1] = (i == len-1) ?nil :tag_cons(htop+2);
		htop += 2;
	}
	heap_set_top(hp, htop);
	return r;
}

term_t heap_str(heap_t *hp, const char *s, int len)
{
	if (len == 0)
		return nil;
	uint32_t *htop = heap_alloc(hp, 2*len);
	term_t r = tag_cons(htop);
	for (int i = 0; i < len; i++)
	{
		htop[0] = tag_int((uint8_t)s[i]);
		htop[1] = (i == len-1) ?nil :tag_cons(htop+2);
		htop += 2;
	}
	heap_set_top(hp, htop);
	return r;
}

term_t heap_vector_to_list(heap_t *hp, term_t *vec, int num)
{
	if (num == 0)
		return nil;
	uint32_t *htop = heap_alloc(hp, 2*num);
	term_t r = tag_cons(htop);
	for (int i = 0; i < num; i++)
	{
		htop[0] = vec[i];
		htop[1] = (i == num-1) ?nil :tag_cons(htop+2);
		htop += 2;
	}
	heap_set_top(hp, htop);
	return r;
}

term_t heap_vector_to_list_N(heap_t *hp, term_t *vec, int num)
{
	if (num == 0)
		return nil;
	uint32_t *htop = heap_alloc_N(hp, 2*num);
	if (htop == 0)
		return noval;
	term_t r = tag_cons(htop);
	for (int i = 0; i < num; i++)
	{
		htop[0] = vec[i];
		htop[1] = (i == num-1) ?nil :tag_cons(htop+2);
		htop += 2;
	}
	heap_set_top(hp, htop);
	return r;
}

int heap_list_to_vector(term_t lst, term_t *vec)
{
	assert(vec != 0);
	assert(is_list(lst));
	int copied = 0;
	while (is_cons(lst))
	{
		term_t *cons = peel_cons(lst);
		vec[copied++] = cons[0];
		lst = cons[1];
	}
	
	assert(is_nil(lst));
	return copied;
}

int heap_copy_terms_N(heap_t *hp, term_t *terms, int num)
{
	return copy_terms(1, hp, terms, num);
}

static int copy_terms(int depth, heap_t *hp, term_t *terms, int num)
{
	if (depth > HEAP_COPY_TERMS_MAX_DEPTH)
	{
		printk("TODO\n");
		printk("TODO: make heap_copy_terms() non-recursive\n");
		printk("TODO\n");
		printk("\n*** heap_copy_terms: depth = %d, aborting...\n\n", depth);

		return -TOO_DEEP;
	}

	term_t *ptr = terms;
	while (ptr < terms + num)
	{
		uint32_t p_tag = primary_tag(*ptr);
		if (p_tag == PRIMARY_TAG_IMMED)
		{
			ptr++;
			continue;
		}

		uint32_t *term_data = peel_any(*ptr);

		switch (p_tag)
		{
		case PRIMARY_TAG_CONS:
		{
			// To keep the crucial invariant that old node can not point to the
			// new one we need to traverse the list using an unlimited capacity
			// stack and then build the copy from the end.
			
			uint32_t cradle[256];
			stack_t st;
			stack_init(&st, 1, cradle, 256);

			term_t tl;
			uint32_t *cons = term_data;
			do {
				term_t hd = cons[0];
				if (!is_immed(hd))
				{
					int rs = copy_terms(depth+1, hp, &hd, 1);
					if (rs < 0)
					{
						stack_done(&st);
						return rs;
					}
				}

				uint32_t *push = stack_push_N(&st);
				if (push == 0)
				{
					stack_done(&st);
					return -NO_MEMORY;
				}
				*push = hd;

				tl = cons[1];
				if (is_immed(tl))
					break;

				if (!is_cons(tl))
				{
					int rs = copy_terms(depth+1, hp, &tl, 1);
					if (rs < 0)
					{
						stack_done(&st);
						return rs;
					}
					break;
				}
				
				cons = peel_cons(tl);
			} while (1);

			// tl now is nil or the copied odd tail

			do {
				term_t hd = (term_t)*stack_pop(&st);
				uint32_t *htop = heap_alloc_N(hp, 2);
				if (htop == 0)
				{
					stack_done(&st);
					return -NO_MEMORY;
				}
				htop[0] = hd;
				htop[1] = tl;
				heap_set_top(hp, htop +2);

				tl = tag_cons(htop);
			} while (!stack_is_empty(&st));

			*ptr = tl;

			stack_done(&st);
			break;
		}
		case PRIMARY_TAG_TUPLE:
		{
			int arity = *term_data;
			if (unlikely(arity == 0))
				*ptr = ZERO_TUPLE;
			else if (likely(arity <= 1024))
			{
				term_t buf[arity];
				memcpy(buf, term_data+1, arity*sizeof(term_t));
				int rs = copy_terms(depth+1, hp, buf, arity);
				if (rs < 0)
					return rs;

				uint32_t *htop = heap_alloc_N(hp, arity+1);
				if (htop == 0)
					return -NO_MEMORY;
				*ptr = tag_tuple(htop);

				htop[0] = arity;
				memcpy(htop+1, buf, arity*sizeof(term_t));
				heap_set_top(hp, htop+arity+1);
			}
			else
			{
				memnode_t *elts_node = nalloc_N(arity*sizeof(term_t));
				if (elts_node == 0)
					return -NO_MEMORY;
				term_t *new_elts = (term_t *)elts_node->starts;

				memcpy(new_elts, term_data+1, arity*sizeof(term_t));
				int rs = copy_terms(depth+1, hp, new_elts, arity);
				if (rs < 0)
				{
					nfree(elts_node);
					return rs;
				}

				uint32_t *htop = heap_alloc_N(hp, arity+1);
				if (htop == 0)
				{
					nfree(elts_node);
					return -NO_MEMORY;
				}
				*ptr = tag_tuple(htop);

				htop[0] = arity;
				memcpy(htop+1, new_elts, arity*sizeof(term_t));
				heap_set_top(hp, htop+arity+1);

				nfree(elts_node);
			}

			break;
		}
		default:
		{
			assert(p_tag == PRIMARY_TAG_BOXED);
			uint32_t s_tag = boxed_tag(term_data);
			switch (s_tag)
			{
			case SUBTAG_POS_BIGNUM:
			case SUBTAG_NEG_BIGNUM:
			{
				int arity = bignum_arity(term_data);
				int wsize = (sizeof(bignum_t) + arity*sizeof(uint16_t) +3)/4;

				uint32_t *htop = heap_alloc_N(hp, wsize);
				if (htop == 0)
					return -NO_MEMORY;
				*ptr = tag_boxed(htop);

				memcpy(htop, term_data, sizeof(bignum_t) + arity*sizeof(uint16_t));
				heap_set_top(hp, htop + wsize);

				// bignum is shrunk when copied
				((bignum_t *)htop)->alloc = (arity +1) & ~1;

				break;
			}

#define copy_simple(s) do { \
	int wsize = WSIZE(s); \
	uint32_t *htop = heap_alloc_N(hp, wsize); \
	if (htop == 0) \
		return -NO_MEMORY; \
	*ptr = tag_boxed(htop); \
	memcpy(htop, term_data, wsize*sizeof(uint32_t)); \
	heap_set_top(hp, htop + wsize); \
} while (0)

			case SUBTAG_FLOAT:
				copy_simple(t_float_t);
				break;
			case SUBTAG_FUN:
			{
				int num_free = fun_num_free(term_data);
				
				// copy frozen vars first, then allocate the new fun
				term_t new_frozen[num_free];
				memcpy(new_frozen, ((t_fun_t *)term_data)->frozen, num_free*sizeof(term_t));

				int rs = copy_terms(depth+1, hp, new_frozen, num_free);
				if (rs < 0)
					return rs;

				int wsize = WSIZE(t_fun_t) + num_free;
				uint32_t *htop = heap_alloc_N(hp, wsize);
				if (htop == 0)
					return -NO_MEMORY;
				*ptr = tag_boxed(htop);

				memcpy(htop, term_data, sizeof(t_fun_t));
				memcpy(((t_fun_t *)htop)->frozen, new_frozen, num_free*sizeof(term_t));
				heap_set_top(hp, htop + wsize);

				break;
			}
			case SUBTAG_EXPORT:
				copy_simple(t_export_t);
				break;
			case SUBTAG_MAP:
			{
				t_map_t *m = (t_map_t *)term_data;
				int size = map_size(m);
				term_t keys = m->keys;
				int rs = copy_terms(depth+1, hp, &keys, 1);
				if (rs < 0)
					return rs;
				term_t values[size];
				memcpy(values, m->values, size *sizeof(term_t));
				rs = copy_terms(depth+1, hp, values, size);	
				if (rs < 0)
					return rs;
				uint32_t *p = heap_alloc_N(hp, WSIZE(t_map_t) +size);
				if (p == 0)
					return -NO_MEMORY;
				term_t *q = p + WSIZE(t_map_t);
				box_map(p, size, keys);
				memcpy(q, values, size *sizeof(term_t));
				heap_set_top(hp, p);
				
				break;
			}
			case SUBTAG_PID:
				copy_simple(t_long_pid_t);
				break;
			case SUBTAG_OID:
				copy_simple(t_long_oid_t);
				break;
			case SUBTAG_REF:
				copy_simple(t_long_ref_t);
				break;
			case SUBTAG_PROC_BIN:
			{
				int wsize = WSIZE(t_proc_bin_t);
				uint32_t *htop = heap_alloc_N(hp, wsize);
				if (htop == 0)
					return -NO_MEMORY;
				t_proc_bin_t *pb = (t_proc_bin_t *)htop;
				memcpy(htop, term_data, wsize*sizeof(uint32_t));
				heap_set_top(hp, htop + wsize);

				// 1+ bin node refc
				proc_bin_link(&hp->proc_bins, pb, &hp->total_pb_size);

				*ptr = tag_boxed(pb);
				break;
			}
			case SUBTAG_HEAP_BIN:
			{
				int byte_size = ((t_heap_bin_t *)term_data)->byte_size;
				int wsize = (sizeof(t_heap_bin_t) + byte_size +3)/4;

				uint32_t *htop = heap_alloc_N(hp, wsize);
				if (htop == 0)
					return -NO_MEMORY;
				*ptr = tag_boxed(htop);

				memcpy(htop, term_data, sizeof(t_heap_bin_t) + byte_size);
				heap_set_top(hp, htop + wsize);

				break;
			}
			case SUBTAG_MATCH_CTX:
			{
				int num_slots = match_ctx_num_slots(term_data);

				term_t new_parent = ((t_match_ctx_t *)term_data)->parent;
				int rs = copy_terms(depth+1, hp, &new_parent, 1);
				if (rs < 0)
					return rs;

				int wsize = WSIZE(t_match_ctx_t) + num_slots*2;	// slots are int64_t
				uint32_t *htop = heap_alloc_N(hp, wsize);
				if (htop == 0)
					return -NO_MEMORY;
				*ptr = tag_boxed(htop);

				memcpy(htop, term_data, sizeof(t_match_ctx_t) + num_slots*sizeof(int64_t));
				((t_match_ctx_t *)htop)->parent = new_parent;
				heap_set_top(hp, htop + wsize);

				break;
			}
			default: // SUBTAG_SUB_BIN
			{
				assert(boxed_tag(term_data) == SUBTAG_SUB_BIN);
				term_t new_parent = ((t_sub_bin_t *)term_data)->parent;
				int rs = copy_terms(depth+1, hp, &new_parent, 1);
				if (rs < 0)
					return rs;

				int wsize = WSIZE(t_sub_bin_t);
				uint32_t *htop = heap_alloc_N(hp, wsize);
				if (htop == 0)
					return -NO_MEMORY;
				*ptr = tag_boxed(htop);

				memcpy(htop, term_data, sizeof(t_sub_bin_t));
				((t_sub_bin_t *)htop)->parent = new_parent;
				heap_set_top(hp, htop + wsize);

				break;
			}
			}
		}
		}

		ptr++;
	}

	return 0; // Success
}

//
// Creates a binary of appropriate kind and returns the pointer to the binary
// data, so that the caller may fill them in.
//
term_t heap_make_bin(heap_t *hp, int size, uint8_t **ptr)
{
	term_t bin;
	*ptr = 0;

	if (size <= MAX_HEAP_BIN)
	{
		int wsize = WSIZE(t_heap_bin_t) + (size +3)/4;
		uint32_t *htop = heap_alloc(hp, wsize);
		bin = tag_boxed(htop);
		*ptr = ((t_heap_bin_t *)htop)->data;
		box_heap_bin(htop, size, 0);
		heap_set_top(hp, htop);
	}
	else
	{
		binnode_t *node = binnode_make(size);
		*ptr = node->starts;
		uint32_t *htop = heap_alloc_N(hp, WSIZE(t_proc_bin_t));
		if (htop == 0)
		{
			binnode_destroy(node);
			no_memory_signal();
		}
		bin = tag_boxed(htop);
		t_proc_bin_t *pb = (t_proc_bin_t *)htop;
		box_proc_bin(htop, size, node);
		heap_set_top(hp, htop);

		proc_bin_link(&hp->proc_bins, pb, &hp->total_pb_size);
	}

	return bin;
}

term_t heap_make_bin_N(heap_t *hp, int size, uint8_t **ptr)
{
	term_t bin;
	*ptr = 0;

	if (size <= MAX_HEAP_BIN)
	{
		int wsize = WSIZE(t_heap_bin_t) + (size +3)/4;
		uint32_t *htop = heap_alloc_N(hp, wsize);
		if (htop == 0)
			return noval;
		bin = tag_boxed(htop);
		*ptr = ((t_heap_bin_t *)htop)->data;
		box_heap_bin(htop, size, 0);
		heap_set_top(hp, htop);
	}
	else
	{
		binnode_t *node = binnode_make_N(size);
		if (node == 0)
			return noval;
		*ptr = node->starts;
		uint32_t *htop = heap_alloc_N(hp, WSIZE(t_proc_bin_t));
		if (htop == 0)
		{
			binnode_destroy(node);
			return noval;
		}
		bin = tag_boxed(htop);
		t_proc_bin_t *pb = (t_proc_bin_t *)htop;
		box_proc_bin(htop, size, node);
		heap_set_top(hp, htop);

		proc_bin_link(&hp->proc_bins, pb, &hp->total_pb_size);
	}

	return bin;
}

term_t heap_float(heap_t *hp, double val)
{
	assert(isfinite(val));
	uint32_t *htop = heap_alloc(hp, WSIZE(t_float_t));
	term_t result = tag_boxed(htop);
	box_float(htop, val);
	heap_set_top(hp, htop);

	return result;
}

term_t heap_float_with_check(heap_t *hp, double val)
{
	if (!isfinite(val))
		return A_BADARITH;
	return heap_float(hp, val);
}

term_t heap_bignum(heap_t *hp, int sign, int ndigs, uint16_t **dp)
{
	int needed = WSIZE(bignum_t) + (ndigs*sizeof(uint16_t) +3) /4;
	uint32_t *p = heap_alloc(hp, needed);
	bignum_t *bn = (bignum_t *)p;
	box_bignum(p, sign, ndigs, 0);
	heap_set_top(hp, p);

	*dp = bn->dp;
	return tag_boxed(bn);
}

term_t int_to_term(int64_t z, heap_t *hp)
{
	if (fits_int(z))
		return tag_int(z);
	else
	{
		bignum_t *bn = bignum_from_int(hp, z);
		return tag_boxed(bn);
	}
}

term_t uint_to_term(uint64_t u, heap_t *hp)
{
	if (u <= MAX_INT_VALUE)
		return tag_int((int)u);
	else
	{
		bignum_t *bn = bignum_from_uint(hp, u);
		return tag_boxed(bn);
	}
}

uint8_t *heap_tmp_buf(heap_t *hp, int size)
{
	int wsz = (size +3) /4;
	uint32_t *ptr = heap_alloc(hp, wsz);
	heap_set_top(hp, ptr +wsz);
	return (uint8_t *)ptr;
}

uint8_t *heap_tmp_buf_N(heap_t *hp, int size)
{
	int wsz = (size +3) /4;
	uint32_t *ptr = heap_alloc_N(hp, wsz);
	if (ptr == 0)
		return 0;
	heap_set_top(hp, ptr +wsz);
	return (uint8_t *)ptr;
}

term_t heap_make_ref(heap_t *hp)
{
	static uint64_t next_ref_id = 0;
	uint32_t id1 = next_ref_id >> 32;
	uint32_t id2 = (uint32_t)next_ref_id;
	next_ref_id++;

	uint32_t *p = heap_alloc(hp, WSIZE(t_long_ref_t));
	term_t ref = tag_boxed(p);
	box_long_ref(p, A_LOCAL, 0, 0, id1, id2);
	heap_set_top(hp, p);

	return ref;
}

term_t heap_remake_local_ref_N(heap_t *hp, uint64_t ref_id)
{
	uint32_t id1 = ref_id >> 32;
	uint32_t id2 = (uint32_t)ref_id;

	uint32_t *p = heap_alloc_N(hp, WSIZE(t_long_ref_t));
	if (p == 0)
		return noval;
	term_t ref = tag_boxed(p);
	box_long_ref(p, A_LOCAL, 0, 0, id1, id2);
	heap_set_top(hp, p);

	return ref;
}

int ref_is_local(term_t t)
{
	assert(is_boxed(t) && boxed_tag(peel_boxed(t)) == SUBTAG_REF);
	t_long_ref_t *ref = (t_long_ref_t *)peel_boxed(t);
	return ref->node == A_LOCAL;
}

uint64_t local_ref_id(term_t t)
{
	assert(is_boxed(t) && boxed_tag(peel_boxed(t)) == SUBTAG_REF);
	t_long_ref_t *ref = (t_long_ref_t *)peel_boxed(t);
	return ((uint64_t)ref->id1 << 32) | ref->id2;
}

//EOF
