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

#include <ctype.h>

#include "term.h"

#include "string.h"
#include "snprintf.h"

#include "atoms.h"
#include "bignum.h"
#include "bits.h"
#include "code_base.h"

#define MAX_TERM_TO_STR_DEPTH	100

static int printable_chars(term_t l);
static void quote_atom(unsigned char *name, char *buf, int len);

static int term_to_str_1(int depth, term_t t, char *buf, int len);

//
// Converts a term to textual representation
// Returns the length of the resultant string
//
// TODO: make it non-recursive
//
int term_to_str(term_t t, char *buf, int len)
{
	return term_to_str_1(1, t, buf, len);
}

static int term_to_str_1(int depth, term_t t, char *buf, int len)
{
	int taken = 0;

#define EMIT(c)	do { \
	if (taken >= len-1) \
   		goto clip; buf[taken++] = (c); \
	} while (0)

#define RETPRINTF(fmt, args...) do { \
		int written__ = snprintf(buf, len, (fmt), args); \
		if (written__ >= len) \
			written__ = len - 1; \
		return written__; \
	} while (0)

#ifdef LING_DEBUG
#define RETERR(t) do { \
		int written__ = snprintf(buf, len, "#err(0x%08x)", t); \
		if (written__ >= len) \
			written__ = len - 1; \
		return written__; \
	} while (0)
#else
#define RETERR(t) do { \
	result = "#err"; \
} while (0)
#endif

	if (len == 0)
		return 0;

	const char *result;
	char scratch[1024];

	if (depth > MAX_TERM_TO_STR_DEPTH)
	{
		result = "#too-deep";
		goto copy;
	}

#ifdef LING_DEBUG
	if (t == (term_t)0)
	{
		result = "#bad-null";
		goto copy;
	}
#ifdef DEBUG_UNUSED_MEM
	if (!is_immed(t) &&
			*(uint32_t *)peel_any(t) == UNUSED_MEM_SIGN)
	{
		result = "#bad-deadbeef";
		goto copy;
	}
#endif
#endif

	switch (primary_tag(t))
	{
	case PRIMARY_TAG_CONS:
	{
		if (printable_chars(t))
		{
			EMIT('"');
			do {
				term_t *cons = peel_cons(t);
				if (taken >= len-1)
					goto clip;
				buf[taken++] = int_value(cons[0]);
				t = cons[1];
			} while (is_cons(t));
			EMIT('"');
		}
		else
		{
			EMIT('[');
			do {
				term_t *cell = (term_t *)peel_cons(t);
				if (taken >= len-1)
					goto clip;
				int n = term_to_str_1(depth +1, cell[0], buf+taken, len-taken);
				taken += n;

				t = cell[1];
				if (!is_cons(t))
					break;

				EMIT(',');
			} while (1);

			if (t != nil)
			{
				EMIT('|');
				if (taken >= len-1)
					goto clip;
				int n = term_to_str_1(depth +1, t, buf+taken, len-taken);
				taken += n;
			}
			
			EMIT(']');
		}

		buf[taken] = 0;
		return taken;
	}		
	case PRIMARY_TAG_TUPLE:
	{
		EMIT('{');
		uint32_t nelts = *(uint32_t *)peel_tuple(t);
		term_t *elts = ((term_t *)peel_tuple(t))+1;
		int i;
		for (i = 0; i < nelts; i++)
		{
			if (taken >= len-1)
				goto clip;
			int n = term_to_str_1(depth +1, elts[i], buf+taken, len-taken);
			taken += n;
			if (i < nelts-1)
				EMIT(',');
		}
		EMIT('}');
		buf[taken] = 0;
		return taken;
	}
	case PRIMARY_TAG_IMMED:
	{
		if (is_int(t))
			RETPRINTF("%d", int_value(t));
		else if (is_atom(t))
		{
			unsigned char *name = atoms_get(atom_index(t));
			if (name == 0)
				RETERR(t);
			else
			{
				quote_atom(name, scratch, sizeof(scratch));
				result = scratch;
			}
		}
		else if (t == nil)
			result = "[]";
		else if (t == noval)
			result = "#noval";
		else if (t == R_I_P)
			result = "#rip";
		else if (is_short_pid(t))
			RETPRINTF("<0.%d.0>", (int)short_pid_id(t));
		else if (is_short_oid(t))
			RETPRINTF("#Port<0.%d.0>", (int)short_oid_id(t));
		else
			RETERR(t);

		break;
	}
	case PRIMARY_TAG_BOXED:
	{
		void *term_data = peel_boxed(t);

		if (is_cp(term_data))
		{
			result = "#cp";
			goto copy;
		}

		switch (boxed_tag(term_data))
		{
		case SUBTAG_FLOAT:
		{
			double v = float_value(term_data);
			RETPRINTF("%f", v);
		}
		case SUBTAG_POS_BIGNUM:
		case SUBTAG_NEG_BIGNUM:
		{
			bignum_t *bn = term_data;
			if (bignum_str_size(bn) > sizeof(scratch))
				result = "#bignum";
			else
			{
				uint32_t hbuf[1024];
				heap_t hp;
				heap_init(&hp, hbuf, hbuf+1024);
				bignum_to_str(&hp, bn, scratch);
				heap_done(&hp);
				result = scratch;
			}
			break;
		}
		case SUBTAG_FUN:
		{
			//#Fun<erl_eval.6.80247286>
			t_fun_t *fun = term_data;
			unsigned char *mod_name = atoms_get(atom_index(fun->module));
			RETPRINTF("#Fun<%.*s.%d.%d>",
				mod_name[0], mod_name+1, (int)fun->index, (int)fun->uniq[0]);
		}
		case SUBTAG_EXPORT:
		{
			t_export_t *exp = term_data;
			RETPRINTF("#Fun<%pt.%pt.%d>",
				T(exp->e->module), T(exp->e->function), (int)exp->e->arity);
		}

		case SUBTAG_PROC_BIN:
		case SUBTAG_HEAP_BIN:
		case SUBTAG_SUB_BIN:
		case SUBTAG_MATCH_CTX:
		{
			bits_t bs;
			bits_get_real(term_data, &bs);

			EMIT('<');
			EMIT('<');

			int as_string = ((bs.ends - bs.starts) & 7) == 0 &&
							 (bs.ends > bs.starts);
			if (as_string)
			{
				while (bits_has_octet(&bs))
				{
					uint8_t o;
					bits_get_octet(&bs, o);
					if (o < 32 || o >= 127)
					{
						as_string = 0;
						break;
					}
				}
				bits_get_real(term_data, &bs);	// reset the bits context
			}

			if (as_string)
			{
				EMIT('"');
				while (bits_has_octet(&bs))
				{
					uint8_t o;
					bits_get_octet(&bs, o);
					EMIT(o);
				}
				EMIT('"');
			}
			else
			{
				int bytes_printed = 0;
				if (bits_has_octet(&bs))
				{
					while (1)
					{
						uint8_t o;
					    bits_get_octet(&bs, o);

						if (taken >= len-1)
							goto clip;
						int n = snprintf(buf+taken, len-taken, "%d", o);
						if (n >= len-taken)
							return len-1;
						taken += n;

						bytes_printed = 1;

						if (!bits_has_octet(&bs))
							break;

						EMIT(',');
					}
				}

				int last_count = bs.ends - bs.starts;
				if (last_count > 0)
				{
					if (bytes_printed)
						EMIT(',');
					if (taken >= len-1)
						goto clip;
					uint8_t bits = bits_get_trailer(&bs);
					int n = snprintf(buf+taken, len-taken, "%d:%d", bits, last_count);
					if (n >= len-taken)
						return len-1;
					taken += n;
				}
			}
			EMIT('>');
			EMIT('>');

			buf[taken] = 0;
			return taken;
		}
		case SUBTAG_PID:
		{
			t_long_pid_t *pid = term_data;
			uint32_t id = opr_hdr_id(pid);
			uint32_t creat = opr_hdr_creat(pid);
			RETPRINTF("<%pt.%d.%d>", T(pid->node), (int)id, (int)creat);
		}
		case SUBTAG_OID:
		{
			t_long_oid_t *oid = term_data;
			uint32_t id = opr_hdr_id(oid);
			uint32_t creat = opr_hdr_creat(oid);
			RETPRINTF("#Port<%pt.%d.%d>", T(oid->node), (int)id, (int)creat);
		}
		case SUBTAG_REF:
		{
			t_long_ref_t *ref = term_data;
			uint32_t id = opr_hdr_id(ref);
			//uint32_t creat = opr_hdr_creat(ref);
			RETPRINTF("#Ref<%pt.%d.%d.%d>", T(ref->node), (int)id, (int)ref->id1, (int)ref->id2);
		}
		default:
			RETERR(t);
		}
	}
	}

copy:
{
	const char *p1 = result;
	const char *p2 = result + strlen(result);
	while (p1 < p2 && taken < len-1)
		buf[taken++] = *p1++;
}

clip:
	buf[taken] = 0;
	return taken;

}

static int printable_chars(term_t l)
{
	do {
		term_t *cons = peel_cons(l);
		if (!is_int(cons[0]) || !is_list(cons[1]))
			return 0;
		int v = int_value(cons[0]);
		if (v < 32 || v > 127)
			return 0;
		l = cons[1];
	} while (!is_nil(l));

	return 1;
}

static void quote_atom(unsigned char *name, char *buf, int len)
{
	if (len == 0)
		return;

	int quotes_needed = 0;

	if (name[0] == 0)
		quotes_needed = 1;
	else if ((name[1] >= 'A' && name[1] <= 'Z') || name[1] == '_')
		quotes_needed = 1;
	else
	{
		for (int i = 1; i <= name[0]; i++)
		{
			if (!isalnum(name[i]) && (name[i] != '_') && (name[i] != '@'))
			{
				quotes_needed = 1;
				break;
			}
		}
	}

	if (!quotes_needed)
	{
		int count = name[0];
		if (count+1 > len)
			count = len-1;
		memcpy(buf, name+1, count);
		buf[count] = 0;
		return;
	}

	unsigned char *p = name+1;
	unsigned char *pend = p + name[0];
	char *p2 = buf;

#define HOLD(c) do { *p2++ = (c); if (--len == 0) return; } while (0)

	HOLD('\'');

	while (p < pend)
	{
		if (isprint(*p))
		{
			HOLD(*p++);
			continue;
		}
		switch (*p)
		{
		case '\0':
			HOLD('\\');
			HOLD('0');
			break;
		case '\b':
			HOLD('\\');
			HOLD('b');
			break;
		case '\f':
			HOLD('\\');
			HOLD('f');
			break;
		case '\n':
			HOLD('\\');
			HOLD('n');
			break;
		case '\r':
			HOLD('\\');
			HOLD('r');
			break;
		case '\t':
			HOLD('\\');
			HOLD('t');
			break;
		case '\v':
			HOLD('\\');
			HOLD('v');
			break;
		case '\\':
			HOLD('\\');
			HOLD('\\');
			break;
		case '\'':
			HOLD('\\');
			HOLD('\'');
			break;
		case '"':
			HOLD('\\');
			HOLD('\"');
			break;
		default:
			{
				unsigned char a, b, c;
				c = (*p) & 7;
				b = ((*p) >> 3) & 7;
				a = ((*p) >> 6) & 3;
				HOLD('\\');
				HOLD(a + '0');
				HOLD(b + '0');
				HOLD(c + '0');
			}
		}
		p++;
	}
	HOLD('\'');

	*p2 = 0;
}

//EOF
