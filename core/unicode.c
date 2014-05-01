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

#include "unicode.h"

#include "ling_common.h"

#include <string.h>

#include "bits.h"
#include "atom_defs.h"
#include "getput.h"
#include "list_util.h"
#include "term_util.h"

#define INIT_CRADLE_SIZE	256

static term_t decode_utf8(bits_t *bs);
static term_t decode_utf16(bits_t *bs, int is_little);
static int encode_utf8(int pt, uint8_t buf[4]);
static int encode_utf16(int pt, int is_little, uint8_t buf[4]);

term_t unicode_decode_utf8(t_match_ctx_t *mc)
{
	if (!bits_has_octet(&mc->bs))
		return A_BADARG;

	term_t point = decode_utf8(&mc->bs);
	if (point == A_INCOMPLETE)
		point = A_BADARG;
	return point;
}

term_t unicode_decode_utf16(t_match_ctx_t *mc, int is_little)
{
	if (!bits_has_octet(&mc->bs))
		return A_BADARG;

	term_t point = decode_utf16(&mc->bs, is_little);
	if (point == A_INCOMPLETE)
		point = A_BADARG;
	return point;
}

static term_t decode_utf8(bits_t *bs)
{
	assert(bits_has_octet(bs));
	uint8_t leading_octet;
	bits_get_octet(bs, leading_octet);
	if ((leading_octet & 0x80) == 0)
		return tag_int(leading_octet);
	else if ((leading_octet & 0xe0) == 0xc0)
	{
		uint8_t cont1;
		if (!bits_has_octet(bs))
			return A_INCOMPLETE;
		bits_get_octet(bs, cont1);
		if ((cont1 & 0xc0) != 0x80)
			return A_BADARG;
		uint32_t n = (((uint32_t)(leading_octet & 0x1f)) << 6) | (cont1 & 0x3f);
		if (n <= 0x7f)
			return A_BADARG;	// overlong form
		return tag_int(n);
	}
	else if ((leading_octet & 0xf0) == 0xe0)
	{
		uint8_t cont1, cont2;
		if (!bits_has_octet(bs))
			return A_INCOMPLETE;
		bits_get_octet(bs, cont1);
		if (!bits_has_octet(bs))
			return A_INCOMPLETE;
		bits_get_octet(bs, cont2);
		if ((cont1 & 0xc0) != 0x80 || (cont2 & 0xc0) != 0x80)
			return A_BADARG;
		uint32_t n = (((uint32_t)(leading_octet & 0xf)) << 12) |
					 (((uint32_t)(cont1 & 0x3f)) << 6) |
					 (cont2 & 0x3f);
		if (n <= 0x07ff)
			return A_BADARG;	// overlong form
		if (n >= 0xd800 && n <= 0xdfff)
			return A_BADARG;	// reserved for UTF-16
		return tag_int(n);
	}
	else if ((leading_octet & 0xf8) == 0xf0)
	{
		uint8_t cont1, cont2, cont3;
		if (!bits_has_octet(bs))
			return A_INCOMPLETE;
		bits_get_octet(bs, cont1);
		if (!bits_has_octet(bs))
			return A_INCOMPLETE;
		bits_get_octet(bs, cont2);
		if (!bits_has_octet(bs))
			return A_INCOMPLETE;
		bits_get_octet(bs, cont3);
		if ((cont1 & 0xc0) != 0x80 || (cont2 & 0xc0) != 0x80 || (cont3 & 0xc0) != 0x80)
			return A_BADARG;
		uint32_t n = (((uint32_t)(leading_octet & 0x7)) << 18) |
					 (((uint32_t)(cont1 & 0x3f)) << 12) |
					 (((uint32_t)(cont2 & 0x3f)) << 6) |
					 (cont3 & 0x3f);
		if (n <= 0xffff)
			return A_BADARG;	// overlong form
		if (n > 0x10ffff)
			return A_BADARG;	// restricted by RFC 3629
		return tag_int(n);
	}
	else
		return A_BADARG;
}

static term_t decode_utf16(bits_t *bs, int is_little)
{
	assert(bits_has_octet(bs));
	uint8_t o1, o2;
	bits_get_octet(bs, o1);
	if (!bits_has_octet(bs))
		return A_INCOMPLETE;
	bits_get_octet(bs, o2);
	uint16_t w1 = (is_little)
		?(((uint16_t)o2) << 8) | o1
		:(((uint16_t)o1) << 8) | o2;

	if (w1 >= 0xd800)
	{
		if (w1 <= 0xdbff)
		{
			uint8_t o1, o2;
			if (!bits_has_octet(bs))
				return A_INCOMPLETE;
			bits_get_octet(bs, o1);
			if (!bits_has_octet(bs))
				return A_INCOMPLETE;
			bits_get_octet(bs, o2);
			uint16_t w2 = (is_little)
				?(((uint16_t)o2) << 8) | o1
				:(((uint16_t)o1) << 8) | o2;

			if (w2 < 0xdc00 || w2 > 0xdfff)
				return A_BADARG;

			uint32_t ndash = (((uint32_t)(w1 & 0x3ff)) << 10) | (w2 & 0x3ff);
			return tag_int(ndash + 0x10000);
		}
		else if (w1 <= 0xdfff)
			return A_BADARG;	// unexpected low surrogate
	}

	return tag_int(w1);
}

int unicode_encode_utf8(term_t point, bits_t *bpc)
{
	if (!is_int(point))
		return -BAD_ARG;
	int n = int_value(point);

	uint8_t buf[4];
	int nbuf = encode_utf8(n, buf);
	if (nbuf < 0)
		return nbuf;

	bits_t bs = {
		.data = buf,
		.starts = 0,
		.ends = nbuf *8
	};

	bits_copy(&bs, bpc);
	return 0;
}

static int encode_utf8(int pt, uint8_t buf[4])
{
	if (pt < 0)
		return -BAD_ARG;
	if (pt >= 0xd800 && pt <= 0xdfff)
		return -BAD_ARG;

	if (pt <= 0x7f)
	{
		buf[0] = pt;
		return 1;
	}
	else if (pt <= 0x7ff)
	{
		buf[0] = ((uint8_t)(pt >> 6)) | 0xc0;
		buf[1] = ((uint8_t)pt & 0x3f) | 0x80;
		return 2;
	}
	else if (pt <= 0xffff)
	{
		buf[0] = ((uint8_t)(pt >> 12)) | 0xe0;
		buf[1] = ((uint8_t)(pt >> 6) & 0x3f) | 0x80;
		buf[2] = ((uint8_t)pt & 0x3f) | 0x80;
		return 3;
	}
	else if (pt <= 0x10ffff)
	{
		buf[0] = ((uint8_t)(pt >> 18)) | 0xf0;
		buf[1] = ((uint8_t)(pt >> 12) & 0x3f) | 0x80;
		buf[2] = ((uint8_t)(pt >> 6) & 0x3f) | 0x80;
		buf[3] = ((uint8_t)pt & 0x3f) | 0x80;
		return 4;
	}
	else
		return -BAD_ARG;
}

int unicode_encode_utf16(term_t point, int is_little, bits_t *bpc)
{
	if (!is_int(point))
		return -BAD_ARG;
	int n = int_value(point);

	uint8_t buf[4];
	int nw = encode_utf16(n, is_little, buf);
	if (nw < 0)
		return nw;

	bits_t bs = {
		.data = buf,
		.starts = 0,
		.ends = nw *16
	};

	bits_copy(&bs, bpc);
	return 0;
}

static int encode_utf16(int pt, int is_little, uint8_t buf[4])
{
	if (pt < 0)
		return -BAD_ARG;

	uint16_t w1, w2 = 0;
	int nw = 0;

	if (pt <= 0xffff)
	{
		if (pt >= 0xd800 && pt <= 0xdfff)
			return -BAD_ARG;
		w1 = pt;
		nw = 1;
	}
	else if (pt <= 0x10ffff)
	{
		int ndash = pt - 0x10000;
		w1 = (uint16_t)(ndash >> 10) | 0xd800;
		w2 = ((uint16_t)ndash & 0x3ff) | 0xdc00;
		nw = 2;
	}
	else
		return -BAD_ARG;

	assert(nw > 0);
	if (is_little)
		PUT_UINT_16_LE(buf, w1);
	else
		PUT_UINT_16(buf, w1);
	if (nw > 1)
	{
		if (is_little)
			PUT_UINT_16_LE(buf +2, w2);
		else
			PUT_UINT_16(buf +2, w2);
	}

	return nw;
}

term_t chars_to_list(term_t *datap,
		term_t in_enc, int is_little, int *incomplete, heap_t *hp)
{
	term_t data = *datap;
	term_t more = nil;

	term_t points = nil;

loop:
	if (data == nil)
	{
		if (more == nil)
		{
			*datap = nil;
			return list_rev(points, hp);
		}

		data = more;
		more = nil;
		goto loop;
	}
	else if (is_cons(data))
	{
		term_t *cons = peel_cons(data);
		if (is_int(cons[0]))
		{
			int pt = int_value(cons[0]);
			if (pt < 0 || (pt > 255 && in_enc == A_LATIN1))
				goto error;

			points = heap_cons(hp, tag_int(pt), points);
			data = cons[1];
			goto loop;
		}
		
		data = cons[0];
		if (cons[1] != nil)
			more = heap_cons(hp, cons[1], more);
		goto loop;
	}
	else if (is_boxed_binary(data))
	{
		bits_t bs;
		bits_get_real(peel_boxed(data), &bs);

		if (((bs.ends -bs.starts) &7) != 0)
			goto error;

		term_t point = noval;
		while (bs.ends > bs.starts)
		{
			int64_t saved_starts = bs.starts; //decode_utf8/16 garble starts on error/incomplete

			if (in_enc == A_LATIN1)
			{
				uint8_t pt;
				bits_get_octet(&bs, pt);
				point = tag_int(pt);
			}
			else if (in_enc == A_UTF8)
				point = decode_utf8(&bs);
			else if (in_enc == A_UTF16)
				point = decode_utf16(&bs, is_little);
			else
			{
				assert(in_enc == A_UTF32);
				if (!bits_has_word(&bs))
					point = A_INCOMPLETE;
				else
				{
					if (!is_little)
					{
						uint32_t w;
						bits_get_word(&bs, w);
						point = tag_int(w);
					}
					else
					{
						uint8_t a, b, c, d;
						bits_get_octet(&bs, a);
						bits_get_octet(&bs, b);
						bits_get_octet(&bs, c);
						bits_get_octet(&bs, d);
						point = tag_int(a |
							((uint32_t)b) << 8 |
							((uint32_t)c) << 16 |
							((uint32_t)d) << 24);
					}
				}
			}

			if (!is_int(point))
			{
				bs.starts = saved_starts;
				break;
			}

			points = heap_cons(hp, point, points);
		}

		if (bs.ends > bs.starts)
		{
			assert(is_atom(point));

			uint32_t *p = heap_alloc(hp, WSIZE(t_sub_bin_t));
			term_t leftover = tag_boxed(p);
			box_sub_bin(p, bin_parent(data), bs.starts, bs.ends, 0);
			heap_set_top(hp, p);

			if (more != nil)
				*datap = heap_cons(hp, leftover, more);
			else
				*datap = leftover;

			*incomplete = (point == A_INCOMPLETE);
			return list_rev(points, hp);
		}

		data = more;
		more = nil;
		goto loop;
	}

error:
	*datap = heap_cons(hp, data, more);
	*incomplete = 0;
	return list_rev(points, hp);
}

struct cradle_t {
	uint8_t *data;
	int len, off;
};

static void cradle_init(struct cradle_t *cr, heap_t *hp)
{
	cr->len = INIT_CRADLE_SIZE;
	cr->data = heap_tmp_buf(hp, cr->len);
	cr->off = 0;
}

static term_t cradle_to_bin(struct cradle_t *cr, heap_t *hp)
{
	uint8_t *to;
	term_t bin = heap_make_bin(hp, cr->off, &to);
	memcpy(to, cr->data, cr->off);
	return bin;
}

static int cradle_put(struct cradle_t *cr,
	int pt, term_t out_enc, int is_little_out, heap_t *hp)
{
	while (cr->len -cr->off < 4)
	{
		assert(cr->len > 0);
		int new_len = cr->len *2;
		uint8_t *new_data = heap_tmp_buf(hp, new_len);
		memcpy(new_data, cr->data, cr->off);
		cr->len = new_len;
		cr->data = new_data;
	}

	if (out_enc == A_LATIN1)
	{
		if (pt < 0 || pt > 255)
			return -BAD_ARG;
		cr->data[cr->off] = pt;
		cr->off++;
	}
	else if (out_enc == A_UTF8)
	{
		int ret = encode_utf8(pt, cr->data +cr->off);
		if (ret < 0)
			return ret;
		cr->off += ret;
	}
	else if (out_enc == A_UTF16)
	{
		int ret = encode_utf16(pt, is_little_out, cr->data +cr->off);
		if (ret < 0)
			return ret;
		cr->off += ret*2;
	}
	else
	{
		assert(out_enc == A_UTF32);
		uint8_t *to = cr->data +cr->off;
		if (is_little_out)
			PUT_UINT_32_LE(to, pt);
		else
			PUT_UINT_32(to, pt);
		cr->off += 4;
	}

	return 0;
}

term_t chars_to_binary(term_t *datap,
		term_t in_enc, int is_little_in,
		term_t out_enc, int is_little_out,
		int *incomplete, heap_t *hp)
{
	term_t data = *datap;
	term_t more = nil;

	struct cradle_t points;
	cradle_init(&points, hp);

loop:
	if (data == nil)
	{
		if (more == nil)
		{
			*datap = nil;
			return cradle_to_bin(&points, hp);
		}

		data = more;
		more = nil;
		goto loop;
	}
	else if (is_cons(data))
	{
		term_t *cons = peel_cons(data);
		if (is_int(cons[0]))
		{
			int pt = int_value(cons[0]);
			if (pt < 0 || (pt > 255 && in_enc == A_LATIN1))
				goto error;
			if (cradle_put(&points, pt, out_enc, is_little_out, hp) < 0)
				goto error;

			data = cons[1];
			goto loop;
		}
		
		data = cons[0];
		if (cons[1] != nil)
			more = heap_cons(hp, cons[1], more);
		goto loop;
	}
	else if (is_boxed_binary(data))
	{
		bits_t bs;
		bits_get_real(peel_boxed(data), &bs);

		if (((bs.ends -bs.starts) &7) != 0)
			goto error;

		term_t point = noval;
		while (bs.ends > bs.starts)
		{
			int64_t saved_starts = bs.starts; //decode_utf8/16 garble starts on error/incomplete

			if (in_enc == A_LATIN1)
			{
				uint8_t pt;
				bits_get_octet(&bs, pt);
				point = tag_int(pt);
			}
			else if (in_enc == A_UTF8)
				point = decode_utf8(&bs);
			else if (in_enc == A_UTF16)
				point = decode_utf16(&bs, is_little_in);
			else
			{
				assert(in_enc == A_UTF32);
				if (!bits_has_word(&bs))
					point = A_INCOMPLETE;
				else
				{
					if (!is_little_in)
					{
						uint32_t w;
						bits_get_word(&bs, w);
						point = tag_int(w);
					}
					else
					{
						uint8_t a, b, c, d;
						bits_get_octet(&bs, a);
						bits_get_octet(&bs, b);
						bits_get_octet(&bs, c);
						bits_get_octet(&bs, d);
						point = tag_int(a |
							((uint32_t)b) << 8 |
							((uint32_t)c) << 16 |
							((uint32_t)d) << 24);
					}
				}
			}

			if (!is_int(point))
			{
				bs.starts = saved_starts;
				break;
			}

			if (cradle_put(&points, int_value(point), out_enc, is_little_out, hp) < 0)
				goto error;
		}

		if (bs.ends > bs.starts)
		{
			assert(is_atom(point));

			uint32_t *p = heap_alloc(hp, WSIZE(t_sub_bin_t));
			term_t leftover = tag_boxed(p);
			box_sub_bin(p, bin_parent(data), bs.starts, bs.ends, 0);
			heap_set_top(hp, p);

			if (more != nil)
				*datap = heap_cons(hp, leftover, more);
			else
				*datap = leftover;

			*incomplete = (point == A_INCOMPLETE);
			return cradle_to_bin(&points, hp);
		}

		data = more;
		more = nil;
		goto loop;
	}

error:
	*datap = heap_cons(hp, data, more);
	*incomplete = 0;
	return cradle_to_bin(&points, hp);
}

//EOF
