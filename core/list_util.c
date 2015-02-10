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

#include "list_util.h"

#include "ling_common.h"

#include "bits.h"

#define IOLIST_MAX_DEPTH	1000
#define BITS_LIST_MAX_DEPTH	1000

static int64_t iolist_size2(int depth, term_t l);
static int64_t bits_list_size2(int depth, term_t l);

int list_len(term_t t)
{
	assert(is_list(t));
	int len = 0;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		len++;
		t = cons[1];
	}

	if (!is_nil(t))
		return -BAD_ARG;	// odd list

	return len;
}

term_t list_rev(term_t t, heap_t *hp)
{
	term_t r = nil;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		r = heap_cons(hp, cons[0], r);
		t = cons[1];
	}
	assert(is_nil(t));
	return r;
}

int byte_list_size(term_t t)
{
	assert(is_list(t));
	int len = 0;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		if (!is_int(cons[0]))
			return -BAD_ARG;
		int c = int_value(cons[0]);
		if (c < 0 || c > 255)
			return -BAD_ARG;
		len++;
		t = cons[1];
	}

	if (!is_nil(t))
		return -BAD_ARG;	// odd list

	return len;
}

void byte_list_flatten(term_t t, uint8_t *data)
{	
	assert(is_list(t));
	uint8_t *ptr = data;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		assert(is_int(cons[0]));
		*ptr++ = int_value(cons[0]);
		t = cons[1];
	}
	assert(is_nil(t));
}

int64_t iolist_size(term_t l)
{
	return iolist_size2(1, l);
}

static int64_t iolist_size2(int depth, term_t l)
{
	if (depth > IOLIST_MAX_DEPTH)
		return -TOO_DEEP;

	if (is_nil(l))
		return 0;

	if (is_cons(l))
	{
		int64_t size = 0;
		do {
			uint32_t *term_data = peel_cons(l);
			term_t e = term_data[0];
			if (is_int(e))
			{
				if (int_value(e) < 0 || int_value(e) > 255)
					return -BAD_ARG;
				size++;
			}
			else
			{
				if (!is_list(e) && (!is_boxed(e) || !is_binary(peel_boxed(e))))
					return -BAD_ARG;
				int64_t s = iolist_size2(depth+1, e);
				if (s < 0)
					return s;
				size += s;
			}
			l = term_data[1];
			if (is_boxed(l) && is_binary(peel_boxed(l)))
			{
				// odd list with binary tail allowed
				int64_t s = iolist_size2(depth+1, l);
				if (s < 0)
					return s;
				return size +s;
			}	
		} while (is_cons(l));

		if (!is_nil(l))
			return -BAD_ARG;

		return size;
	}
	else if (is_boxed_binary(l))
	{
		bits_t bs;
		bits_get_real(peel_boxed(l), &bs);

		int64_t bit_size = bit_size = bs.ends - bs.starts;
		if ((bit_size & 7) != 0)
			return -1;

		return bit_size /8;
	}
	else
		return -BAD_ARG;
}

//
// Flatten the valid iolist to the buffer of
// appropriate size pointed to by ptr
//
uint8_t *iolist_flatten(term_t l, uint8_t *ptr)
{
	if (is_nil(l))
		return ptr;

	if (is_cons(l))
	{
		do {
			uint32_t *term_data = peel_cons(l);
			term_t e = term_data[0];
			if (is_int(e))
				*ptr++ = int_value(e);
			else
			{
				assert(is_list(e) || (is_boxed(e) && is_binary(peel_boxed(e))));
				ptr = iolist_flatten(e, ptr);
			}
			l = term_data[1];
			if (is_boxed(l) && is_binary(peel_boxed(l)))
				return iolist_flatten(l, ptr);
		} while (is_cons(l));

		assert(is_nil(l));
	}
	else // is_binary()
	{
		bits_t bs, to;
		bits_get_real(peel_boxed(l), &bs);
		bits_init_buf(ptr, (bs.ends +7) /8, &to);
		ptr += (bs.ends - bs.starts) /8;
		bits_copy(&bs, &to);
		assert(bs.starts == bs.ends);
	}
	return ptr;
}

int64_t bits_list_size(term_t l)
{
	return bits_list_size2(1, l);
}

static int64_t bits_list_size2(int depth, term_t l)
{
	if (depth > BITS_LIST_MAX_DEPTH)
		return -TOO_DEEP;

	if (is_nil(l))
		return 0;

	if (is_cons(l))
	{
		int64_t size = 0;
		do {
			uint32_t *term_data = peel_cons(l);
			term_t e = term_data[0];
			if (is_int(e))
			{
				if (int_value(e) < 0 || int_value(e) > 255)
					return -BAD_ARG;
				size += 8;
			}
			else
			{
				if (!is_list(e) && (!is_boxed(e) || !is_binary(peel_boxed(e))))
					return -BAD_ARG;
				int64_t s = bits_list_size2(depth+1, e);
				if (s < 0)
					return s;
				size += s;
			}
			l = term_data[1];
			if (is_boxed(l) && is_binary(peel_boxed(l)))
			{
				// odd list with binary tail allowed
				int64_t s = bits_list_size2(depth+1, l);
				if (s < 0)
					return s;
				size += s;
				if (size > MAX_BIT_SIZE)
					return -TOO_LONG;
				return size;
			}	
		} while (is_cons(l));

		if (!is_nil(l))
			return -BAD_ARG;
		if (size > MAX_BIT_SIZE)
			return -TOO_LONG;
		return size;
	}
	else // is_binary()
	{
		bits_t bs;
		bits_get_real(peel_boxed(l), &bs);
		if (bs.ends - bs.starts > MAX_BIT_SIZE)
			return -TOO_LONG;
		return bs.ends - bs.starts;
	}
}

//
// Flatten the valid bits list to the bits_t context
//
void bits_list_flatten(term_t l, bits_t *bs)
{
	if (is_nil(l))
		return;

	if (is_cons(l))
	{
		do {
			uint32_t *term_data = peel_cons(l);
			term_t e = term_data[0];
			if (is_int(e))
			{
				uint8_t o = int_value(e);
				assert(o >= 0 && o < 256);
				bits_put_octet(bs, o);
			}
			else
			{
				assert(is_list(e) || (is_boxed(e) && is_binary(peel_boxed(e))));
				bits_list_flatten(e, bs);
			}
			l = term_data[1];
			if (is_boxed(l) && is_binary(peel_boxed(l)))
			{
				bits_list_flatten(l, bs);
				return;
			}
		} while (is_cons(l));

		assert(is_nil(l));
	}
	else // is_binary()
	{
		bits_t source;
		bits_get_real(peel_boxed(l), &source);
		bits_copy(&source, bs);
	}
}


//EOF
