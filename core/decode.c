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

#include "decode.h"

#include "heap.h"
#include "getput.h"
#include "atom_defs.h"
#include "strings.h"
#include "bits.h"

#include <string.h>
#include <ctype.h>

// TCP_PB_RAW
// TCP_PB_1
// TCP_PB_2
// TCP_PB_4
// TCP_PB_ASN1
// TCP_PB_RM
// TCP_PB_CDR
// TCP_PB_FCGI
// TCP_PB_LINE_LF
// TCP_PB_TPKT
// TCP_PB_HTTP
// TCP_PB_HTTPH
// TCP_PB_SSL_TLS
// TCP_PB_HTTP_BIN
// TCP_PB_HTTPH_BIN
// TCP_PB_9P

static term_t decode_http(bits_t *bs, term_t parent,
		int bin_str, term_t *reason, uint32_t packet_size, heap_t *hp);
static term_t decode_http_header(bits_t *bs, term_t parent,
		int bin_str, term_t *reason, uint32_t packet_size, heap_t *hp);
static term_t decode_tls(bits_t *bs, term_t parent,
		term_t *reason, uint32_t *more_len, uint32_t packet_size, heap_t *hp);

static term_t http_str(char *data, uint32_t dlen,
		term_t parent, int64_t starts, int bin_str, heap_t *hp);
static term_t build_uri(term_t scheme,
		term_t host, int port, term_t path, int bin_str, heap_t *hp);
	
term_t decode_packet_N(int type, bits_t *bs, term_t parent, int as_binary,
		term_t *reason, uint32_t *more_len, uint32_t packet_size, uint32_t line_length, heap_t *hp)
{
uint32_t exp_len = 0;
uint32_t body_len = 0;
if (!bits_has_octet(bs))
	goto more;
int skip_hdr = 0;
int skip_pad = 0;
int64_t saved_starts = bs->starts;

// line_length is alias for packet_size for http* if packet_size is not set
uint32_t http_size = (packet_size == 0)
		?line_length
		:packet_size;

switch (type)
{
case TCP_PB_RAW:
	exp_len = (bs->ends -bs->starts) /8;
	break;

case TCP_PB_1:
	bits_get_octet(bs, body_len);
	exp_len = body_len +1;
	skip_hdr = 1;
	break;

case TCP_PB_2:
{
	uint8_t hb, lb;
   	bits_get_octet(bs, hb);
	if (!bits_has_octet(bs))
		goto more;
	bits_get_octet(bs, lb);
	body_len = ((((uint32_t)hb) << 8) | lb);
	exp_len = body_len +2;
	skip_hdr = 2;
	break;
}
case TCP_PB_4:
	if (!bits_has_word(bs))
		goto more;
	bits_get_word(bs, body_len);
	if (body_len > 0x7fffffff)
		goto inval;
   	exp_len = body_len +4;
	skip_hdr = 4;
	break;

case TCP_PB_ASN1:
{
	uint8_t type;
   	bits_get_octet(bs, type);
	if ((type & 0x1f) == 0x1f)
	{
		// long type tag - skip bytes with 8th bit set
		uint8_t bb;
		do {
		   if (!bits_has_octet(bs))
			   goto more;
		   bits_get_octet(bs, bb);
		} while ((bb & 0x80) != 0);
	}
	if (!bits_has_octet(bs))
		goto more;
	uint8_t len_sel;
   	bits_get_octet(bs, len_sel);
	if (len_sel < 0x80)
	{
		body_len = len_sel;
		exp_len = (bs->starts -saved_starts) /8 +body_len;
	}
	else if (len_sel == 0x80)	// infinite length
	{
		body_len = 0;
		exp_len = (bs->starts -saved_starts) /8;
	}
	else if (len_sel > 0x84)	// 4-byte-max length
		goto inval;	// NB: 0xff is for future extensions
	else
	{
		int n = len_sel -0x80;
		uint32_t ll = 0;
		while (n > 0 && bits_has_octet(bs))
		{
			uint8_t bb;
			bits_get_octet(bs, bb);
			ll <<= 8;
			ll |= bb;
			n--;
		}
		if (n > 0)
			goto more;
		body_len = ll;
		exp_len = (bs->starts -saved_starts) /8 +body_len;
	}
	break;
}
case TCP_PB_RM:
	if (!bits_has_word(bs))
		goto more;
	bits_get_word(bs, body_len);
	body_len &= 0x7fffffff;
	exp_len = body_len +4;
	// NB: a frag, not a complete record
	break;

case TCP_PB_CDR:
	// GIOP 1.1
	//
	// 0	'G'
	// 1	'I'
	// 2	'O'
	// 3	'P'
	// 4	1	(major)
	// 5	1	(minor)
	// 6	-	(flags)
	// 7	-	(value)
	// 8	l0
	// 9	l1
	// 10	l2
	// 11	l3
	
	if (bs->ends -bs->starts < 12 *8)
		goto more;
	uint8_t bb;
	bits_get_octet(bs, bb); if (bb != 'G') goto inval;
	bits_get_octet(bs, bb); if (bb != 'I') goto inval;
	bits_get_octet(bs, bb); if (bb != 'O') goto inval;
	bits_get_octet(bs, bb); if (bb != 'P') goto inval;
	//bits_get_octet(bs, bb); if (bb != 1) goto inval;
	//bits_get_octet(bs, bb); if (bb != 1) goto inval;
	bs->starts += 16;

	uint8_t flags;
   	bits_get_octet(bs, flags);
	bs->starts += 8;	// skip value field
	if (flags & 1)		// endianness
	{
		uint8_t a, b, c, d;
		bits_get_octet(bs, a);
		bits_get_octet(bs, b);
		bits_get_octet(bs, c);
		bits_get_octet(bs, d);
		body_len = MAKE_UINT_32(d, c, b, a);
	}
	else
		bits_get_word(bs, body_len);
	exp_len = body_len +12;
	break;

case TCP_PB_FCGI:
	// FastCGI v1
	//
	// 0     1 byte     version      The version of the FastCGI protocol.
	// 1     1 byte     type The type of record.
	// 2     2 bytes    request_id   The ID number of the request the record belongs to.
	// 4     2 bytes    content_len  The length of the data portion of the record.
	// 6     1 byte     padding_len  The length of the padding portion of the record.
	// 7     1 byte     reserved     Reserved. Must be 0 in FCGI v1.
	
	if (bs->ends -bs->starts < 8 *8)
		goto more;
	bs->starts += 32;	// skip version, type, request_id
	uint8_t clh, cll, pad_len;
	bits_get_octet(bs, clh);
	bits_get_octet(bs, cll);
	bits_get_octet(bs, pad_len);
	//bits_get_octet(bs, reserved);
	//if (reserved != 0)
	//	goto inval;
	bs->starts += 8;
	body_len = ((((uint32_t)clh) << 8) | cll) +pad_len;
	exp_len = body_len +8;
	skip_pad = pad_len;
	break;

case TCP_PB_LINE_LF:
{
	uint32_t ll = 0;
	int was_nl = 0;
	while ((line_length == 0 || ll < line_length) && bits_has_octet(bs))
	{
		uint8_t bb;
		bits_get_octet(bs, bb);
		if (bb == '\n')
		{
			was_nl = 1;
			break;
		}
		ll++;
	}
	if ((line_length == 0 || ll < line_length) && !was_nl)
		goto more;
	exp_len = (bs->starts -saved_starts) /8;
	body_len = exp_len;
	break;
}
case TCP_PB_TPKT:
	// RFC 2126
	//
	// 0	vsn (must be 3)
	// 1	reserved
	// 2	lenh
	// 3	lenl
	
	if (bs->ends -bs->starts < 4 *8)
		goto more;
	uint8_t vsn;
   	bits_get_octet(bs, vsn);
	if (vsn != 3)
		goto inval;
	bs->starts += 8;	// skip reserved
	uint8_t hb, lb;
	bits_get_octet(bs, hb);
	bits_get_octet(bs, lb);
	exp_len = (((uint32_t)hb) << 8) | lb;
	body_len = exp_len -4;
	break;

case TCP_PB_HTTP:
	assert(more_len != 0);
	*more_len = 0;	// undefined length
	return decode_http(bs, parent, 0, reason, http_size, hp);

case TCP_PB_HTTPH:
	assert(more_len != 0);
	*more_len = 0;	// undefined length
	return decode_http_header(bs, parent, 0, reason, http_size, hp);

case TCP_PB_SSL_TLS:
	return decode_tls(bs, parent, reason, more_len, packet_size, hp);

case TCP_PB_HTTP_BIN:
	assert(more_len != 0);
	*more_len = 0;	// undefined length
	return decode_http(bs, parent, 1, reason, http_size, hp);

case TCP_PB_HTTPH_BIN:
	assert(more_len != 0);
	*more_len = 0;	// undefined length
	return decode_http_header(bs, parent, 1, reason, http_size, hp);

default:
	assert(type == TCP_PB_9P);
	if (!bits_has_word(bs))
		goto more;
	uint8_t a, b, c, d;
	bits_get_octet(bs, a);
	bits_get_octet(bs, b);
	bits_get_octet(bs, c);
	bits_get_octet(bs, d);
	exp_len = body_len = MAKE_UINT_32(d, c, b, a);
	skip_hdr = 4;
	break;
}

	bs->starts = saved_starts;
	if (packet_size != 0 && body_len > packet_size)
	{
		debug("Over-sized packet detected: body_len %d packet_size %d\n", body_len, packet_size);
		*reason = A_EMSGSIZE;
		return noval;
	}
	if (exp_len > (bs->ends -bs->starts) /8)
		goto more;

	term_t packet;
	if (parent != noval && as_binary)
	{
		int wsize = WSIZE(t_sub_bin_t);
		uint32_t *p = heap_alloc_N(hp, wsize);
		if (p == 0)
			goto nomem;
		packet = tag_boxed(p);
		box_sub_bin(p, parent, bs->starts +skip_hdr *8, bs->starts +exp_len *8 -skip_pad *8, 0);
		heap_set_top(hp, p);
		bs->starts += exp_len *8;
	}
	else if (as_binary)
	{
		uint8_t *ptr;
		packet = heap_make_bin_N(hp, exp_len -skip_hdr -skip_pad, &ptr);
		if (packet == noval)
			goto nomem;
		bs->starts += skip_hdr *8;
		int n = exp_len -skip_hdr;
		while (n-- > skip_pad)
		{
			uint8_t bb;
			bits_get_octet(bs, bb);
			*ptr++ = bb;
		}
		bs->starts += skip_pad *8;
	}
	else
	{
		bs->starts += skip_hdr *8;
		int n = exp_len -skip_hdr -skip_pad;
		uint32_t *p = heap_alloc_N(hp, 2*n);
		term_t *link = &packet;
		while (n-- > 0)
		{
			*link = tag_cons(p);
			uint8_t bb;
			bits_get_octet(bs, bb);
			*p++ = tag_int(bb);
			link = p++;
		}
		heap_set_top(hp, p);
		*link = nil;
		bs->starts += skip_pad *8;
	}

	return packet;

more:
	assert(more_len != 0);
	*more_len = exp_len;
	return A_MORE;
inval:
	*reason = A_INVALID;
	return noval;
nomem:
	*reason = A_NO_MEMORY;
	return noval;
}

typedef struct atom_str_t atom_str_t;
struct atom_str_t {
	const char *str;
	term_t atom;
	int num;
};

#define DH_STATE_VERB_0			1
#define DH_STATE_VERB			2
#define DH_STATE_STATCODE_0		3
#define DH_STATE_STATCODE		4
#define DH_STATE_STATUS			5
#define DH_STATE_SCHEME_0		6
#define DH_STATE_SCHEME			7
#define DH_STATE_SLASH2			8
#define DH_STATE_SLASH1			9
#define DH_STATE_ODD_SCHEME		10
#define DH_STATE_AUTHORITY		11
#define DH_STATE_VERSION_0		12
#define DH_STATE_VERSION		13
#define DH_STATE_HOST_0			14
#define DH_STATE_HOST			15
#define DH_STATE_ASTERISK		16
#define DH_STATE_PORT_0			17
#define DH_STATE_PORT			18
#define DH_STATE_PATH_0			19
#define DH_STATE_PATH			20
#define DH_STATE_ERROR			21
#define DH_STATE_DONE			22

static term_t decode_http(bits_t *bs, term_t parent,
	int bin_str, term_t *reason, uint32_t packet_size, heap_t *hp)
{
int state = DH_STATE_VERB_0;

atom_str_t known_methods[] = {
	{ .str = "GET", .atom = AGET__ },
	{ .str = "POST", .atom = APOST__ },
	{ .str = "HEAD", .atom = AHEAD__ },
	{ .str = "PUT", .atom = APUT__ },
	{ .str = "DELETE", .atom = ADELETE__ },
	{ .str = "OPTIONS", .atom = AOPTIONS__ },
	{ .str = "TRACE", .atom = ATRACE__ },
	{ .str = 0 }
};

int64_t err_off = bs->starts;

char buf[16384];
char *ptr = buf;
int64_t bit_off = 0;
char *bookmark = 0;

term_t verb = noval;
int vsn_maj = 0;
int vsn_min = 0;
int stat_code = 0;
term_t scheme = noval;
term_t host = noval;
term_t path = noval;
int port = -1;
term_t uri = noval;
term_t packet = noval;

while (1)
{

if (!bits_has_octet(bs))
	goto error;

char ch;
bits_get_octet(bs, ch);

if (ch == '\r')
{
	if (bits_has_octet(bs))
	{
		char peek;
		bits_get_octet(bs, peek);
		if (peek == '\n')
		{
			if (state == DH_STATE_DONE)
			{
				assert(packet != noval);
				return packet;
			}

			if (state != DH_STATE_STATUS && state != DH_STATE_STATCODE)
				goto error;

			if (state == DH_STATE_STATCODE)
			{
				bookmark = ptr;
				bit_off = bs->starts;
			}

			int n = ptr -bookmark;
			term_t status = http_str(bookmark, n, parent, bit_off, bin_str, hp);
			if (status == noval)
				goto nomem;

			// {http_response,Vsn,Code,Status}
			uint32_t *p = heap_alloc_N(hp, 1 +2 +1 +4);
			if (p == 0)
				goto nomem;
			p[0] = 2;
			p[1] = tag_int(vsn_maj);
			p[2] = tag_int(vsn_min);
			term_t vsn = tag_tuple(p);
			p += 3;
			p[0] = 4;
			p[1] = A_HTTP_RESPONSE;
			p[2] = vsn;
			p[3] = tag_int(stat_code);
			p[4] = status;
			heap_set_top(hp, p +1 +4);
			return tag_tuple(p);
		}
		else
			bs->starts -= 8;	// unget
	}
}

if (ptr -buf >= sizeof(buf))
	goto error;
*ptr++ = ch;

switch (state)
{
case DH_STATE_VERB_0:
	if (ch == ' ')
		state = DH_STATE_ERROR;
	else
	{
		bit_off = bs->starts -8;
		bookmark = ptr -1;
		state = DH_STATE_VERB;
	}
	continue;

case DH_STATE_VERB:
	if (ch == ' ')
	{
		int n =  ptr -1 -bookmark;
		// request or response?
		if (n == 8 && strncmp(bookmark, "HTTP/", 5) == 0 && bookmark[6] == '.')
		{
			// response
			char vmaj = bookmark[5];
			char vmin = bookmark[7];
			if (vmaj != '1' || (vmin != '1' && vmin != '0'))
				goto error;
			vsn_maj = vmaj - '0';
			vsn_min = vmin - '0';
			state = DH_STATE_STATCODE_0;
		}
		else
		{
			// request
			atom_str_t *km = known_methods;
			while (km->str != 0)
			{
				if (strncmp(km->str, bookmark, n) == 0 && km->str[n] == 0)
				{
					verb = km->atom;
					break;
				}
				km++;
			}
			if (km->str == 0)
			{
				verb = http_str(bookmark, n, parent, bit_off, bin_str, hp);
				if (verb == noval)
					goto nomem;
			}
			state = DH_STATE_SCHEME_0;
		}
	}
	continue;

case DH_STATE_STATCODE_0:
	if (ch < '0' || ch > '9')
		state = DH_STATE_ERROR;
	else
	{
		stat_code = ch - '0';
		state = DH_STATE_STATCODE;
	}
	continue;

case DH_STATE_STATCODE:
	if (ch == ' ')
	{
		bit_off = bs->starts;
		bookmark = ptr;
		state = DH_STATE_STATUS;
	}
	else if (ch < '0' || ch > '9')
		state = DH_STATE_ERROR;
	else
	{
		stat_code *= 10;
		stat_code += ch - '0';
		if (stat_code >= 1000)
			state = DH_STATE_ERROR;
	}
	continue;

case DH_STATE_STATUS:
	continue;

case DH_STATE_SCHEME_0:
	if (ch == ' ')
		state = DH_STATE_ERROR;
	else if (ch == '*')
		state = DH_STATE_ASTERISK;
	else if (ch == '/')
	{
		bit_off = bs->starts -8;
		bookmark = ptr -1;
		state = DH_STATE_PATH;
	}
	else
	{
		bit_off = bs->starts -8;
		bookmark = ptr -1;
		state = DH_STATE_SCHEME;
	}
	continue;

case DH_STATE_SCHEME:
	if (ch == ':')
	{
		int n = ptr -1 -bookmark;
		if (n == 4 && strncmp(bookmark, "http", 4) == 0)
		{
			scheme = A_HTTP;
			state = DH_STATE_SLASH2;
		}
		else if (n == 5 && strncmp(bookmark, "https", 5) == 0)
		{
			scheme = A_HTTPS;
			state = DH_STATE_SLASH2;
		}
		else
		{
			scheme = http_str(bookmark, n, parent, bit_off, bin_str, hp);
			if (scheme == noval)
				goto nomem;
			bit_off = bs->starts;
			bookmark = ptr;
			state = DH_STATE_ODD_SCHEME;
		}
	}
	else if (ch == ' ')
	{
		// relaxed, to be compatible with OTP
		uri = http_str(bookmark, ptr -1 -bookmark, parent, bit_off, bin_str, hp);
		if (uri == noval)
			goto nomem;
		state = DH_STATE_VERSION_0;
	}
	continue;

case DH_STATE_SLASH2:
	if (ch != '/')	 // must be an authority (host:port)
		state = DH_STATE_ERROR;
	else
		state = DH_STATE_SLASH1;
	continue;

case DH_STATE_SLASH1:
	if (ch != '/')
		state = DH_STATE_ERROR;
	else
	{
		bit_off = bs->starts;
		bookmark = ptr;
		state = DH_STATE_HOST_0;
	}
	continue;

case DH_STATE_ODD_SCHEME:
	if (ch == ' ')
	{
		int n = ptr -1 -bookmark;
		term_t str = http_str(bookmark, n, parent, bit_off, bin_str, hp);
		if (str == noval)
			goto nomem;

		// {scheme,Scheme,Str}
		uint32_t *p = heap_alloc_N(hp, 1 +3);
		p[0] = 3;
		p[1] = A_SCHEME;
		assert(scheme != noval);
		p[2] = scheme;
		p[3] = str;
		heap_set_top(hp, p +1 +3);
		uri = tag_tuple(p);
		state = DH_STATE_VERSION_0;
	}
	continue;

case DH_STATE_AUTHORITY:
	if (ch == ' ')
	{
		int n = ptr -1 -bookmark;
		uri = http_str(bookmark, n, parent, bit_off, bin_str, hp);
		if (uri == noval)
			goto nomem;
		state = DH_STATE_VERSION_0;
	}
	continue;

case DH_STATE_VERSION_0:
	if (ch == ' ')
		state = DH_STATE_ERROR;
	else
	{
		bit_off = bs->starts -8;
		bookmark = ptr -1;
		state = DH_STATE_VERSION;
	}
	continue;

case DH_STATE_VERSION:
	if (ptr -bookmark == 8)
	{
		if (strncmp(bookmark, "HTTP/", 5) != 0 || bookmark[6] != '.')
			goto error;
		char vmaj = bookmark[5];
		char vmin = bookmark[7];
		if (vmaj != '1' || (vmin != '1' && vmin != '0'))
			goto error;
		vsn_maj = vmaj - '0';
		vsn_min = vmin - '0';

		// {http_request,Verb,Uri,Version}
		uint32_t *p = heap_alloc_N(hp, 1 +2 +1 +4);
		if (p == 0)
			goto nomem;
		p[0] = 2;
		p[1] = tag_int(vsn_maj);
		p[2] = tag_int(vsn_min);
		term_t vsn = tag_tuple(p);
		p += 3;
		p[0] = 4;
		p[1] = A_HTTP_REQUEST;
		assert(verb != noval);
		p[2] = verb;
		assert(uri != noval);
		p[3] = uri;
		p[4] = vsn;
		heap_set_top(hp, p +1 +4);
		packet = tag_tuple(p);
		state = DH_STATE_DONE;
	}
	continue;

case DH_STATE_HOST_0:
	if (ch == ' ' || ch == ':')
		state = DH_STATE_ERROR;
	else
	{
		bit_off = bs->starts -8;
		bookmark = ptr -1;
		state = DH_STATE_HOST;
	}
	continue;

case DH_STATE_ASTERISK:
	if (ch != ' ')
		state = DH_STATE_ERROR;
	else
	{
		uri = ATIMES__;
		state = DH_STATE_VERSION_0;
	}
	continue;

case DH_STATE_HOST:
	if (ch == ' ' || ch == '/' || ch == ':')
	{
		int n = ptr -1 -bookmark;
		host = http_str(bookmark, n, parent, bit_off, bin_str, hp);
		if (host == noval)
			goto nomem;
	}

	if (ch == ' ')
	{
		uri = build_uri(scheme, host, port, path, bin_str, hp);
		if (uri == noval)
			goto nomem;
		state = DH_STATE_VERSION_0;
	}
	if (ch == ':')
		state = DH_STATE_PORT_0;
	if (ch == '/')
		state = DH_STATE_PATH_0;
	continue;

case DH_STATE_PORT_0:
	if (ch < '0' || ch > '9')
		state = DH_STATE_ERROR;
	else
	{
		port = ch - '0';
		state = DH_STATE_PORT;
	}
	continue;

case DH_STATE_PORT:
	if (ch == ' ')
	{
		uri = build_uri(scheme, host, port, path, bin_str, hp);
		if (uri == noval)
			goto nomem;
		state = DH_STATE_VERSION_0;
	}
	else if (ch == '/')
		state = DH_STATE_PATH_0;
	else
	{
		if (ch < '0' || ch > '9')
			state = DH_STATE_ERROR;
		port *= 10;
		port += ch - '0';
		if (port >= 65536)
			state = DH_STATE_ERROR;
	}
	continue;

case DH_STATE_PATH_0:
	if (ch == ' ')
	{
		uri = build_uri(scheme, host, port, path, bin_str, hp);
		if (uri == noval)
			goto nomem;
		state = DH_STATE_VERSION_0;
	}
	else
	{
		bit_off = bs->starts -16;
		bookmark = ptr -2;
		state = DH_STATE_PATH;
	}
	continue;

case DH_STATE_PATH:
	if (ch == ' ')
	{
		int n = ptr -1 -bookmark;
		path = http_str(bookmark, n, parent, bit_off, bin_str, hp);
		if (path == noval)
			goto nomem;
		uri = build_uri(scheme, host, port, path, bin_str, hp);
		if (uri == noval)
			goto nomem;
		state = DH_STATE_VERSION_0;
	}
	continue;
}
}

nomem:
*reason = A_NO_MEMORY;
return noval;

error:
{
	term_t s = http_str(buf, ptr -buf, parent, err_off, bin_str, hp);
	if (s == noval)
		return noval;
	uint32_t *htop = heap_alloc_N(hp, 1 +2);
	if (htop == 0)
		return noval;
	htop[0] = 2;
	htop[1] = A_HTTP_ERROR;
	htop[2] = s;
	heap_set_top(hp, htop +1 +2);

	return tag_tuple(htop);
}
}

#define DHH_STATE_FIELD_0	1
#define DHH_STATE_FIELD		2
#define DHH_STATE_FIELD_WS	3
#define DHH_STATE_VALUE_SP	4
#define DHH_STATE_VALUE		5
#define DHH_STATE_ERROR		6

static term_t decode_http_header(bits_t *bs, term_t parent,
		int bin_str, term_t *reason, uint32_t packet_size, heap_t *hp)
{
int state = DHH_STATE_FIELD_0;

// The character case of all field names is standardized, e.g. Sec-Websocket-Key
int uppercase = 1;

// Indexes of known fields from packet_parser.c
#define PP_CACHE_CONTROL		1
#define PP_CONNECTION			2
#define PP_DATE					3
#define PP_PRAGMA				4
#define PP_TRANSFER_ENCODING	5
#define PP_UPGRADE				6
#define PP_VIA					7
#define PP_ACCEPT				8
#define PP_ACCEPT_CHARSET		9
#define PP_ACCEPT_ENCODING		10
#define PP_ACCEPT_LANGUAGE		11
#define PP_AUTHORIZATION		12
#define PP_FROM					13
#define PP_HOST					14
#define PP_IF_MODIFIED_SINCE	15
#define PP_IF_MATCH				16
#define PP_IF_NONE_MATCH		17
#define PP_IF_RANGE				18
#define PP_IF_UNMODIFIED_SINCE	19
#define PP_MAX_FORWARDS			20
#define PP_PROXY_AUTHORIZATION	21
#define PP_RANGE				22
#define PP_REFERER				23
#define PP_USER_AGENT			24
#define PP_AGE					25
#define PP_LOCATION				26
#define PP_PROXY_AUTHENTICATE	27
#define PP_PUBLIC				28
#define PP_RETRY_AFTER			29
#define PP_SERVER				30
#define PP_VARY					31
#define PP_WARNING				32
#define PP_WWW_AUTHENTICATE		33
#define PP_ALLOW				34
#define PP_CONTENT_BASE			35
#define PP_CONTENT_ENCODING		36
#define PP_CONTENT_LANGUAGE		37
#define PP_CONTENT_LENGTH		38
#define PP_CONTENT_LOCATION		39
#define PP_CONTENT_MD5			40
#define PP_CONTENT_RANGE		41
#define PP_CONTENT_TYPE			42
#define PP_ETAG					43
#define PP_EXPIRES				44
#define PP_LAST_MODIFIED		45
#define PP_ACCEPT_RANGES		46
#define PP_SET_COOKIE			47
#define PP_SET_COOKIE2			48
#define PP_X_FORWARDED_FOR		49
#define PP_COOKIE				50
#define PP_KEEP_ALIVE			51
#define PP_PROXY_CONNECTION		52

atom_str_t known_fields[] = {
	{ .str = "Content-Length", .atom = ACONTENT_LENGTH__, .num = PP_CONTENT_LENGTH },
	{ .str = "Content-Type", .atom = ACONTENT_TYPE__, .num = PP_CONTENT_TYPE },
	{ .str = "Accept", .atom = AACCEPT__, .num = PP_ACCEPT },
	{ .str = "Host", .atom = AHOST__, .num = PP_HOST },
	{ .str = "Cache-Control", .atom = ACACHE_CONTROL__, .num = PP_CACHE_CONTROL },
	{ .str = "Connection", .atom = ACONNECTION__, .num = PP_CONNECTION },
	{ .str = "Date", .atom = ADATE__, .num = PP_DATE },
	{ .str = "Pragma", .atom = APRAGMA__, .num = PP_PRAGMA },
	{ .str = "Transfer-Encoding", .atom = ATRANSFER_ENCODING__, .num = PP_TRANSFER_ENCODING },
	{ .str = "Upgrade", .atom = AUPGRADE__, .num = PP_UPGRADE },
	{ .str = "Via", .atom = AVIA__, .num = PP_VIA },
	{ .str = "Accept-Charset", .atom = AACCEPT_CHARSET__, .num = PP_ACCEPT_CHARSET },
	{ .str = "Accept-Encoding", .atom = AACCEPT_ENCODING__, .num = PP_ACCEPT_ENCODING },
	{ .str = "Accept-Language", .atom = AACCEPT_LANGUAGE__, .num = PP_ACCEPT_LANGUAGE },
	{ .str = "Authorization", .atom = AAUTHORIZATION__, .num = PP_AUTHORIZATION },
	{ .str = "From", .atom = AFROM__, .num = PP_FROM },
	{ .str = "If-Modified-Since", .atom = AIF_MODIFIED_SINCE__, .num = PP_IF_MODIFIED_SINCE },
	{ .str = "If-Match", .atom = AIF_MATCH__, .num = PP_IF_MATCH },
	{ .str = "If-None-Match", .atom = AIF_NONE_MATCH__, .num = PP_IF_NONE_MATCH },
	{ .str = "If-Range", .atom = AIF_RANGE__, .num = PP_IF_RANGE },
	{ .str = "If-Unmodified-Since", .atom = AIF_UNMODIFIED_SINCE__, .num = PP_IF_UNMODIFIED_SINCE },
	{ .str = "Max-Forwards", .atom = AMAX_FORWARDS__, .num = PP_MAX_FORWARDS },
	{ .str = "Proxy-Authorization", .atom = APROXY_AUTHORIZATION__, .num = PP_PROXY_AUTHORIZATION },
	{ .str = "Range", .atom = ARANGE__, .num = PP_RANGE },
	{ .str = "Referer", .atom = AREFERER__, .num = PP_REFERER },
	{ .str = "User-Agent", .atom = AUSER_AGENT__, .num = PP_USER_AGENT },
	{ .str = "Age", .atom = AAGE__, .num = PP_AGE },
	{ .str = "Location", .atom = ALOCATION__, .num = PP_LOCATION },
	{ .str = "Proxy-Authenticate", .atom = APROXY_AUTHENTICATE__, .num = PP_PROXY_AUTHENTICATE },
	{ .str = "Public", .atom = APUBLIC__, .num = PP_PUBLIC },
	{ .str = "Retry-After", .atom = ARETRY_AFTER__, .num = PP_RETRY_AFTER },
	{ .str = "Server", .atom = ASERVER__, .num = PP_SERVER },
	{ .str = "Vary", .atom = AVARY__, .num = PP_VARY },
	{ .str = "Warning", .atom = AWARNING__, .num = PP_WARNING },
	{ .str = "Www-Authenticate", .atom = AWWW_AUTHENTICATE__, .num = PP_WWW_AUTHENTICATE },
	{ .str = "Allow", .atom = AALLOW__, .num = PP_ALLOW },
	{ .str = "Content-Base", .atom = ACONTENT_BASE__, .num = PP_CONTENT_BASE },
	{ .str = "Content-Encoding", .atom = ACONTENT_ENCODING__, .num = PP_CONTENT_ENCODING },
	{ .str = "Content-Language", .atom = ACONTENT_LANGUAGE__, .num = PP_CONTENT_LANGUAGE },
	{ .str = "Content-Location", .atom = ACONTENT_LOCATION__, .num = PP_CONTENT_LOCATION },
	{ .str = "Content-Md5", .atom = ACONTENT_MD5__, .num = PP_CONTENT_MD5 },
	{ .str = "Content-Range", .atom = ACONTENT_RANGE__, .num = PP_CONTENT_RANGE },
	{ .str = "Etag", .atom = AETAG__, .num = PP_ETAG },
	{ .str = "Expires", .atom = AEXPIRES__, .num = PP_EXPIRES },
	{ .str = "Last-Modified", .atom = ALAST_MODIFIED__, .num = PP_LAST_MODIFIED },
	{ .str = "Accept-Ranges", .atom = AACCEPT_RANGES__, .num = PP_ACCEPT_RANGES },
	{ .str = "Set-Cookie", .atom = ASET_COOKIE__, .num = PP_SET_COOKIE },
	{ .str = "Set-Cookie2", .atom = ASET_COOKIE2__, .num = PP_SET_COOKIE2 },
	{ .str = "X-Forwarded-For", .atom = AX_FORWARDED_FOR__, .num = PP_X_FORWARDED_FOR },
	{ .str = "Cookie", .atom = ACOOKIE__, .num = PP_COOKIE },
	{ .str = "Keep-Alive", .atom = AKEEP_ALIVE__, .num = PP_KEEP_ALIVE },
	{ .str = "Proxy-Connection", .atom = APROXY_CONNECTION__, .num = PP_PROXY_CONNECTION },
	{ .str = 0 },
};

int64_t err_off = bs->starts;

char buf[16384];
char *ptr = buf;
int64_t bit_off = 0;
char *bookmark = 0;

char fbuf[128];
char *fptr = 0;

term_t field = noval;
// http_header includes a numeric index of a known fields; the index is the
// position of the corresponding string in the array found in packet_parser.c
// (OTP).
int field_num = 0;

while (1)
{

if (!bits_has_octet(bs))
	return A_MORE;

char ch;
bits_get_octet(bs, ch);

if (ch == '\r')
{
	if (!bits_has_octet(bs))
		return A_MORE;
	char peek1;
	bits_get_octet(bs, peek1);
	if (peek1 == '\n')
	{
		if (ptr -buf == 0)
			return A_HTTP_EOH;

		if (!bits_has_octet(bs))
			return A_MORE;
		char peek2;
		bits_get_octet(bs, peek2);
		bs->starts -= 8;	// retract
		if (peek2 != ' ' && peek2 != '\t')
		{
			if (state != DHH_STATE_VALUE_SP && state != DHH_STATE_VALUE)
				goto error;

			int n = ptr -bookmark;
			term_t value = http_str(bookmark, n, parent, bit_off, bin_str, hp);
			if (value == noval)
				goto nomem;

			uint32_t *p = heap_alloc_N(hp, 1 +5);
			if (p == 0)
				goto nomem;
			p[0] = 5;
			p[1] = A_HTTP_HEADER;
			p[2] = tag_int(field_num);
			assert(field != noval);
			p[3] = field;
			p[4] = A_UNDEFINED; 	// reserved
			p[5] = value;
			heap_set_top(hp, p +1 +5);
			return tag_tuple(p);
		}
	}
	bs->starts -= 8;	// retract
}

if (ptr -buf >= sizeof(buf) || (packet_size != 0 && ptr -buf >= packet_size))
{
	*reason = A_EMSGSIZE;
	return noval;
}
*ptr++ = ch;

switch (state)
{
case DHH_STATE_FIELD_0:
	if (ch == ' ')
		state = DHH_STATE_ERROR;
	else
	{
		bit_off = bs->starts -8;
		bookmark = ptr -1;
		fptr = fbuf;
		assert(uppercase);
		*fptr++ = toupper((int)ch);
		uppercase = 0;
		state = DHH_STATE_FIELD;
	}
	continue;

case DHH_STATE_FIELD:
	if (ch == ':')
	{
colon:
		*fptr++ = 0;
		atom_str_t *kf = known_fields;

		while (kf->str != 0)
		{
			if (strcmp(kf->str, fbuf) == 0)
			{
				field = kf->atom;
				field_num = kf->num;
				break;
			}
			kf++;
			field_num++;
		}
		if (field == noval)
		{
			// Use fbuf for non-standard fields too
			int n = fptr -1 -fbuf;	// -1 skip null
			field = http_str(fbuf, n, noval, 0, bin_str, hp);	// never a sub binary
			if (field == noval)
				goto nomem;
			field_num = 0;
		}
		bit_off = bs->starts;
		bookmark = ptr;
		state = DHH_STATE_VALUE_SP;
	}
	else if (ch == ' ' || ch == '\t') 	// whitespace before ':' allowed
		state = DHH_STATE_FIELD_WS;
	else
	{
		if (fptr -fbuf >= sizeof(fbuf))
			state = DHH_STATE_ERROR;
		else if (uppercase)
			*fptr++ = toupper((int)ch);
		else
			*fptr++ = tolower((int)ch);

		uppercase = (ch == '-');	// capitalize after hyphen
	}
	continue;

case DHH_STATE_FIELD_WS:
	if (ch == ':')
		goto colon;
	if (ch != ' ' && ch != '\t')
		state = DHH_STATE_ERROR;
	continue;

case DHH_STATE_VALUE_SP:
	if (ch == ' ')
	{
		bit_off = bs->starts;
		bookmark = ptr;
	}
	state = DHH_STATE_VALUE;
	continue;

case DHH_STATE_VALUE:
	continue;

case DHH_STATE_ERROR:
	continue;
}
}

error:
{
	term_t s = http_str(buf, ptr -buf, parent, err_off, bin_str, hp);
	if (s == noval)
		goto nomem;
	uint32_t *htop = heap_alloc_N(hp, 1 +2);
	if (htop == 0)
		goto nomem;
	htop[0] = 2;
	htop[1] = A_HTTP_ERROR;
	htop[2] = s;
	heap_set_top(hp, htop +1 +2);

	return tag_tuple(htop);
}

nomem:
	*reason = A_NO_MEMORY;
	return noval;
}

static term_t http_str(char *data, uint32_t dlen,
		term_t parent, int64_t starts, int bin_str, heap_t *hp)
{
	if (bin_str == 0)
		return heap_str_N(hp, (const char *)data, dlen);

	if (parent == noval)
	{
		// copy
		uint8_t *ptr;
		term_t bin = heap_make_bin(hp, dlen, &ptr);
		if (bin == noval)
			return noval;
		memcpy(ptr, data, dlen);
		return bin;
	}
	else
	{
		// sub bin
		int wsize = WSIZE(t_sub_bin_t);
		uint32_t *p = heap_alloc_N(hp, wsize);
		if (p == 0)
			return noval;
		term_t str = tag_boxed(p);
		box_sub_bin(p, parent, starts, starts +dlen *8, 0);
		heap_set_top(hp, p);
		return str;
	}
}

static term_t build_uri(term_t scheme,
		term_t host, int port, term_t path, int bin_str, heap_t *hp)
{
	if (host == noval)
	{
		// {abs_path,Path}
		assert(path != noval);
		uint32_t *p = heap_alloc_N(hp, 1 +2);
		if (p == 0)
			return noval;
		p[0] = 2;
		p[1] = A_ABS_PATH;
		p[2] = path;
		heap_set_top(hp, p +1 +2);
		return tag_tuple(p);
	}
	else
	{
		if (path == noval)
			path = http_str("/", 1, noval, 0, bin_str, hp);
		if (path == noval)
			return noval;

		// {absoluteURI,Scheme,Host,Port,Path}
		assert(scheme != noval);
		assert(host != noval);
		assert(path != noval);
		uint32_t *p = heap_alloc_N(hp, 1 +5);
		if (p == 0)
			return noval;
		p[0] = 5;
		p[1] = AABSOLUTEURI__;
		p[2] = scheme;
		p[3] = host;
		p[4] = (port < 0) ?A_UNDEFINED: tag_int(port);
		p[5] = path;
		heap_set_top(hp, p +1 +5);
		return tag_tuple(p);
	}
}

static term_t decode_tls(bits_t *bs, term_t parent,
		term_t *reason, uint32_t *more_len, uint32_t packet_size, heap_t *hp)
{
	uint32_t exp_len = 0;

	// TLS 1.2
	//
	// 0	type
	// 1	vsn maj
	// 2	vsn min
	// 3	lenh
	// 4	lenl
	//
	// {ssl_tls,[],Type,Vsn,Data}

	if (bs->ends -bs->starts < 5 *8)
		goto more;
	uint8_t type, vmaj, vmin;
	bits_get_octet(bs, type);
	bits_get_octet(bs, vmaj);
	bits_get_octet(bs, vmin);

	term_t data;
	if ((type & 0x80) == 0 || vmin != 1)
	{
		uint8_t hb, lb;
		bits_get_octet(bs, hb);
		bits_get_octet(bs, lb);
		uint32_t dlen = (((uint32_t)hb) << 8) | lb;
		exp_len = dlen +5;
		if (packet_size != 0 && dlen > packet_size)	// magic (OTP tests)
		{
			*reason = A_EMSGSIZE;
			return noval;
		}
		if (bs->ends -bs->starts < dlen *8)
			goto more;

		if (parent == noval)
		{
			uint8_t *ptr;
			data = heap_make_bin_N(hp, dlen, &ptr);
			if (data == noval)
				goto nomem;
			while (dlen > 0)
			{
				uint8_t bb;
				bits_get_octet(bs, bb);
				*ptr++ = bb;
				dlen--;
			}
		}
		else
		{
			uint32_t *p = heap_alloc_N(hp, WSIZE(t_sub_bin_t));
			if (p == 0)
				goto nomem;
			data = tag_boxed(p);
			box_sub_bin(p, parent, bs->starts, bs->starts +dlen *8, 0);
			heap_set_top(hp, p);
			bs->starts += dlen *8;
		}
	}
	else
	{
		// a special case of SSLv2 handshake message
		uint8_t hb = type & 0x7f;
		uint8_t lb = vmaj;
		uint32_t dlen = (((uint32_t)hb) << 8) | lb;

		exp_len = dlen +2;
		if (packet_size != 0 && dlen -3 > packet_size)	// magic (OTP tests)
		{
			*reason = A_EMSGSIZE;
			return noval;
		}
		if (bs->ends -bs->starts < (dlen -1) *8)
			goto more;
		bits_get_octet(bs, vmaj);
		bits_get_octet(bs, vmin);

		uint8_t prefix[] = {1, 0, hb, lb-1, vmaj, vmin};
		dlen -= 3;	// type, vmaj, vmin

		uint8_t *ptr;
		data = heap_make_bin(hp, sizeof(prefix) +dlen, &ptr);
		for (int i = 0; i < sizeof(prefix); i++)
			*ptr++ = prefix[i];

		while (dlen-- > 0)
		{
			uint8_t bb;
			bits_get_octet(bs, bb);
			*ptr++ = bb;
		}

		type = 22; // handshake
	}

	uint32_t *p = heap_alloc_N(hp, 1 +2 +1 +5);
	if (p == 0)
		goto nomem;
	p[0] = 2;
	p[1] = tag_int(vmaj);
	p[2] = tag_int(vmin);
	term_t vsn = tag_tuple(p);
	p += 1 +2;
	p[0] = 5;
	p[1] = A_SSL_TLS;
	p[2] = nil;
	p[3] = tag_int(type);
	p[4] = vsn;
	p[5] = data;
	heap_set_top(hp, p +1 +5);
	return tag_tuple(p);

more:
	assert(more_len != 0);
	*more_len = exp_len;
	return A_MORE;

nomem:
	*reason = A_NO_MEMORY;
	return noval;
}

//EOF
