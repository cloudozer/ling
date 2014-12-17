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

#include "ext_term.h"

#include "ling_common.h"

#include <stdint.h>
#include <math.h>

#include "term.h"
#include "atoms.h"
#include "atom_defs.h"
#include "string.h"
#include "getput.h"
#include "code_base.h"
#include "bits.h"
#include "bignum.h"
#include "snprintf.h"
#include "list_util.h"

#define	MAGIC_EXT			131
#define ATOM_CACHE_REF		82
#define SMALL_INTEGER_EXT	97
#define INTEGER_EXT			98
#define FLOAT_EXT			99
#define	ATOM_EXT			100
#define REFERENCE_EXT		101
#define PORT_EXT			102
#define PID_EXT				103
#define SMALL_TUPLE_EXT		104
#define LARGE_TUPLE_EXT		105
#define NIL_EXT				106
#define STRING_EXT			107
#define LIST_EXT			108
#define BINARY_EXT			109
#define SMALL_BIG_EXT		110
#define LARGE_BIG_EXT		111
#define NEW_REFERENCE_EXT	114
#define SMALL_ATOM_EXT		115
#define MAP_EXT				116
#define	FUN_EXT				117
#define NEW_FUN_EXT			112
#define EXPORT_EXT			113
#define BIT_BINARY_EXT		77
#define NEW_FLOAT_EXT		70

typedef struct ext_term_scan_t ext_term_scan_t;
struct ext_term_scan_t {
	uint8_t *enc_data;
	int enc_len;
	uint32_t *htop;
	int heap_size;
	t_proc_bin_t **pbs;
	int *pb_size;
	int safe;
};

#define GetByte()		(es->enc_len--, *es->enc_data++)
#define GetUint16()		(es->enc_len -= 2, es->enc_data += 2, \
							((es->enc_data[-2] << 8) | es->enc_data[-1]))
#define GetUint32()		(es->enc_len -= 4, es->enc_data += 4, \
   							((es->enc_data[-4] << 24) | (es->enc_data[-3] << 16) | \
						 	(es->enc_data[-2] << 8) | es->enc_data[-1]))

#define More(n)			do { if (es->enc_len < (n)) return -1; } while (0)
#define More2(n)		do { if (es->enc_len < (n)) return noval; } while (0)

#define Skip(n)			(es->enc_len -= (n), es->enc_data += (n))

#define MoreSkip(n)		do { if (es->enc_len < (n)) return -1; \
	   						es->enc_len -= (n); es->enc_data += (n); } while (0)
#define MoreSkip2(n)	do { if (es->enc_len < (n)) return noval; \
	   						es->enc_len -= (n); es->enc_data += (n); } while (0)

static int ext_term_decode_size2(int depth, ext_term_scan_t *es);
static term_t ext_term_decode2(ext_term_scan_t *es);
static term_t complete_bignum_decode(int len, ext_term_scan_t *es);
static term_t complete_fun_decode(int num_free, int arity, term_t pid,
	   term_t module, int index, uint32_t *uniq, int old_index, int old_uniq,
	   fun_entry_t *fe, ext_term_scan_t *es);

double atof(const char *str);

int ext_term_decode_size(uint8_t *enc_data, int enc_len, int safe)
{
	if (enc_len < 1)
		return -1;
	if (enc_data[0] != MAGIC_EXT)
		return -1;
	enc_data++;
	enc_len--;

	ext_term_scan_t es = {
		.enc_data = enc_data,
		.enc_len = enc_len,
		.htop = 0,
		.heap_size = 0,
		.pbs = 0,
		.safe = safe
	};
	if (ext_term_decode_size2(1, &es) < 0)
		return -1;

	return es.heap_size;
}

term_t ext_term_decode(uint32_t *htop, uint32_t expected_heap_size,
		uint8_t *enc_data, int enc_len,
	   	t_proc_bin_t **pbs, int *pb_size, int safe)
{
	if (enc_len < 1)
		return noval;
	if (enc_data[0] != MAGIC_EXT)
		return noval;
	enc_data++;
	enc_len--;

	ext_term_scan_t es = {
		.enc_data = enc_data,
		.enc_len = enc_len,
		.htop = htop,
		.heap_size = 0,
		.pbs = pbs,
		.pb_size = pb_size,
		.safe = safe
	};
	term_t t = ext_term_decode2(&es);
	if (t == noval)
		return noval;

	int heap_size = es.htop - htop;
	if (heap_size != expected_heap_size)
	{
		printk("%s:%d: heap size is %d, expected %d\n",
			   __FILE__, __LINE__, heap_size, expected_heap_size);
		return noval;	
	}

	return t;
}

static int ext_term_decode_size2(int depth, ext_term_scan_t *es)
{
	if (depth > EXT_TERM_MAX_DEPTH)
		return -TOO_DEEP;

	switch(GetByte())
	{
	case ATOM_CACHE_REF:
	{
		MoreSkip(1);
		//reported as not implemented later
		break;
	}
	case SMALL_INTEGER_EXT:
	{
		MoreSkip(1);
		break;
	}
	case ATOM_EXT:
	{
		More(2);
		int n = GetUint16();
		MoreSkip(n);
		break;
	}
	case SMALL_ATOM_EXT:
	{
		More(1);
		int n  = GetByte();
		MoreSkip(n);
		break;
	}
	case MAP_EXT:
	{
		More(4);
		int arity = GetUint32();
		for (int i = 0; i < arity; i++)
		{
			// K:V pair
			int x = ext_term_decode_size2(depth+1, es);
			if (x < 0)
				return x;
			int y = ext_term_decode_size2(depth+1, es);
			if (y < 0)
				return y;
		}
		es->heap_size += 1 +arity +2 +arity;
		break;
	}
	case NIL_EXT:
		break;
	case INTEGER_EXT:
	{
		More(4);
		int z = GetUint32();
		if (fits_int(z))
			break;
		//max 2 digits
		es->heap_size += sizeof(bignum_t) / 4 + 1;
		break;
	}
	case FLOAT_EXT:
	{
		MoreSkip(31);
		es->heap_size += sizeof(t_float_t) / 4;
		break;
	}
	case REFERENCE_EXT:
	{
		More(1);
		if (es->enc_data[0] != SMALL_ATOM_EXT &&
			es->enc_data[0] != ATOM_EXT)
				return -BAD_ARG;
		//Node atom
		term_t node = ext_term_decode2(es);
		if (node == noval)
			return -BAD_ARG;
		MoreSkip(4+1);
		es->heap_size += sizeof(t_long_ref_t) / 4;
		break;
	}
	case PORT_EXT:
	{
		More(1);
		if (es->enc_data[0] != SMALL_ATOM_EXT &&
			es->enc_data[0] != ATOM_EXT)
				return -BAD_ARG;
		//Node atom
		term_t node = ext_term_decode2(es);
		if (node == noval)
			return -BAD_ARG;
		MoreSkip(4+1);
		if (node != A_LOCAL)
			es->heap_size += sizeof(t_long_oid_t) / 4;
		break;
	}
	case PID_EXT:
	{
		More(1);
		if (es->enc_data[0] != SMALL_ATOM_EXT &&
			es->enc_data[0] != ATOM_EXT)
				return -BAD_ARG;
		//Node atom
		term_t node = ext_term_decode2(es);
		if (node == noval)
			return -BAD_ARG;
		MoreSkip(4+4+1);
		if (node != A_LOCAL)
			es->heap_size += sizeof(t_long_pid_t) / 4;
		break;
	}
	case SMALL_TUPLE_EXT:
	{
		More(1);
		int arity = GetByte();
		if (arity == 0)
			break;	// no heap frag
		for (int i = 0; i < arity; i++)
		{
			int x = ext_term_decode_size2(depth+1, es);
			if (x < 0)
				return x;
		}
		es->heap_size += 1 + arity;
		break;
	}
	case LARGE_TUPLE_EXT:
	{
		More(4);
		int arity = GetUint32();
		if (arity == 0)
			break;	// no heap frag
		for (int i = 0; i < arity; i++)
		{
			int x = ext_term_decode_size2(depth+1, es);
			if (x < 0)
				return x;
		}
		es->heap_size += 1 + arity;
		break;
	}
	case STRING_EXT:
	{
		More(2);
		int len = GetUint16();
		if (len < 0)
			return -1;
		MoreSkip(len);
		es->heap_size += 2*len;
		break;
	}
	case LIST_EXT:
	{
		More(4);
		int len = GetUint32();
		if (len < 0)
			return -1;
		for (int i = 0; i < len+1; i++)		//+1 for the tail
		{
			int x = ext_term_decode_size2(depth+1, es);
			if (x < 0)
				return x;
		}
		es->heap_size += 2*len;
		break;
	}
	case BINARY_EXT:
	{
		More(4);
		int len = GetUint32();
		if (len < 0)
			return -1;
		MoreSkip(len);
		if (len > MAX_HEAP_BIN)
			es->heap_size += WSIZE(t_proc_bin_t);
		else
			es->heap_size += (sizeof(t_heap_bin_t) + len+3) / 4;
		break;
	}
	case SMALL_BIG_EXT:
	{
		More(1+1);
		int len = GetByte();
		Skip(1); //sign
		MoreSkip(len);
		es->heap_size += (sizeof(bignum_t) + len+3) / 4;
		break;
	}
	case LARGE_BIG_EXT:
	{
		More(4+1);
		int len = GetUint32();
		if (len < 0)
			return -BAD_ARG;
		Skip(1); //sign
		MoreSkip(len);
		es->heap_size += (sizeof(bignum_t) + len+3) / 4;
		break;
	}
	case NEW_REFERENCE_EXT:
	{
		More(2);
		int len = GetUint16();
		if (len > 3)
			return -1;
		More(1);
		if (es->enc_data[0] != SMALL_ATOM_EXT &&
			es->enc_data[0] != ATOM_EXT)
				return -BAD_ARG;
		//Node atom
		term_t node = ext_term_decode2(es);
		if (node == noval)
			return -BAD_ARG;
		MoreSkip(1+ 4*len);
		es->heap_size += sizeof(t_long_ref_t)/4;
		break;
	}
	case FUN_EXT:	
	{
		More(4);
		int num_free = GetUint32();
		if (num_free < 0)
			return -BAD_ARG;
		//Pid
		int x = ext_term_decode_size2(depth+1, es);
		//Module atom
		if (x == 0)
			x = ext_term_decode_size2(depth+1, es);
		//Index integer
		if (x == 0)
			x = ext_term_decode_size2(depth+1, es);
		//Unique value
		if (x == 0)
			x = ext_term_decode_size2(depth+1, es);
		if (x < 0)
			return x;
		//Free vars
		for (int i = 0; i < num_free; i++);
		{
			x = ext_term_decode_size2(depth+1, es);
			if (x < 0)
				return x;
		}
		es->heap_size += WSIZE(t_fun_t) + num_free;
		break;
	}
	case NEW_FUN_EXT:
	{
		More(4+1+16+4+4);
		unsigned char *saved_ptr = es->enc_data;
		uint32_t expected_size = GetUint32();
		MoreSkip(1+16+4);
		int num_free = GetUint32();
		if (num_free < 0 || num_free > 255)	//NB
			return -BAD_ARG;
		//Module atom
		int x = ext_term_decode_size2(depth+1, es);
		//Old index integer
		if (x == 0)
			x = ext_term_decode_size2(depth+1, es);
		//Old unique value
		if (x == 0)
			x = ext_term_decode_size2(depth+1, es);
		//Pid
		if (x == 0)
			x = ext_term_decode_size2(depth+1, es);
		//Free vars
		if (x < 0)
			return x;
		for (int i = 0; i < num_free; i++)
		{
			x = ext_term_decode_size2(depth+1, es);
			if (x < 0)
				return x;
		}
		if (es->enc_data - saved_ptr != expected_size)
			return -BAD_ARG;
		es->heap_size += WSIZE(t_fun_t) + num_free;
		break;
	}
	case EXPORT_EXT:
	{
		//Module atom
		int x = ext_term_decode_size2(depth+1, es);
		//Function atom
		if (x == 0)
			x = ext_term_decode_size2(depth+1, es);
		//Arity integer
		if (x == 0)
			x = ext_term_decode_size2(depth+1, es);
		if (x < 0)
			return x;
		es->heap_size += WSIZE(t_export_t);
		break;
	}
	case BIT_BINARY_EXT:
	{
		More(4+1);
		int len = GetUint32();
		if (len < 0)
			return -1;
		uint8_t bits = GetByte(); //Bits
		MoreSkip(len);

		if (len > MAX_HEAP_BIN)
			es->heap_size += WSIZE(t_proc_bin_t);
		else
			es->heap_size += (sizeof(t_heap_bin_t) + len+3) / 4;
		// additional sub binary is needed for odd binaries
		if (bits > 0)
			es->heap_size += WSIZE(t_sub_bin_t);
		break;
	}
	case NEW_FLOAT_EXT:
	{
		MoreSkip(8);
		es->heap_size += sizeof(t_float_t) / 4;
		break;
	}
	default:
		return -1;
	}

	return 0;
}

static term_t ext_term_decode2(ext_term_scan_t *es)
{
	switch(GetByte())
	{
	case ATOM_CACHE_REF:
	{
		MoreSkip2(1);

		printk("%s:%d: atom cache not implemented\n", __FILE__, __LINE__);
		return noval;
	}
	case SMALL_INTEGER_EXT:
	{
		More2(1);
		uint8_t z1 = GetByte();	// NB: unsigned
		return tag_int(z1);
	}
	case ATOM_EXT:
	{
		More2(2);
		int n = GetUint16();
		if (n > 255)
		{
			printk("%s:%d long atoms not supported\n", __FILE__, __LINE__);
			return noval;
		}

		// a bit of luck: es->enc_data-1 now points to a valid string with a
		// counter ready to be consumed by atom_exists/atoms_set

		if (es->safe && !atom_exists(es->enc_data-1))
			return noval;

		int index = atoms_set(es->enc_data-1);
		if (index < 0)
			return noval;

		MoreSkip2(n);
		return tag_atom(index);
	}
	case SMALL_ATOM_EXT:
	{
		More2(1);
		int n  = GetByte();

		if (es->safe && !atom_exists(es->enc_data-1))
			return noval;

		int index = atoms_set(es->enc_data-1);
		if (index < 0)
			return noval;

		MoreSkip2(n);
		return tag_atom(index);
	}
	case MAP_EXT:
	{
		More2(4);
		int arity  = GetUint32();
		if (arity < 0)
			return noval;

		// arity
		// key1
		// key2
		// ...
		// hdr
		// keys
		// val1
		// val2
		// ...

		//TODO
		
		term_t keys = tag_tuple(es->htop);
		*es->htop++ = arity;
		term_t *kw = es->htop;
		es->htop += arity;
		term_t map = tag_boxed(es->htop);
		term_t *vw = es->htop;
		box_map(es->htop, arity, keys);

		for (int i = 0; i < arity; i++)
		{
			term_t k = ext_term_decode2(es);
			if (k == noval)
				return noval;
			*kw++ = k;
			term_t v = ext_term_decode2(es);
			if (v == noval)
				return noval;
			*vw++ = v;
		}

		return map;
	}
	case NIL_EXT:
		return nil;

	case INTEGER_EXT:
	{
		More2(4);
		int z = GetUint32();
		if (fits_int(z))
			return tag_int(z);

		int sign = MP_ZPOS;
		if (z < 0)
		{
			z = -z;
			sign = MP_NEG;
		}

		uint16_t digits[2];
		digits[0] = z & 0xffff;
		digits[1] = z >> 16;

		term_t t = tag_boxed(es->htop);
		box_bignum(es->htop, sign, 2, digits);
		return t;
	}
	case FLOAT_EXT: ;
	{
		const char *str = (const char *)es->enc_data;
		MoreSkip2(31);

		double v = atof(str);
		term_t t = tag_boxed(es->htop);
		box_float(es->htop, v);
		return t;
	}
	case REFERENCE_EXT: ;
	{
		term_t node = ext_term_decode2(es);
		if (node == noval || !is_atom(node))
			return noval;

		More2(4);
		uint32_t id = GetUint32();
		More2(1);
		uint32_t creat = GetByte();

		if ((id & ~0x3ffff) != 0 || (creat & ~3) != 0)
			return noval;

		term_t t = tag_boxed(es->htop);
		box_long_ref(es->htop, node, creat, 0, 0, id);
		return t;
	}
	case PORT_EXT:
	{
		term_t node = ext_term_decode2(es);
		if (node == noval || !is_atom(node))
			return noval;

		More2(4);
		uint32_t id = GetUint32();
		More2(1);
		uint32_t creat = GetByte();

		if ((id & ~0x3ffff) != 0 || (creat & ~3) != 0)
			return noval;

		if (node == A_LOCAL)
			return tag_short_oid(id);
		else
		{
			term_t t = tag_boxed(es->htop);
			box_long_oid(es->htop, node, id, creat);
			return t;
		}
	}
	case PID_EXT:
	{
		term_t node = ext_term_decode2(es);
		if (node == noval || !is_atom(node))
			return noval;

		More2(4);
		uint32_t id = GetUint32();
		More2(4);
		uint32_t serial = GetUint32();
		More2(1);
		uint32_t creat = GetByte();

		if ((id & ~0x7fff) != 0 || (creat & ~3) != 0)
			return noval;

		// id contains 15 least significant bits of the rank
		// 			See External Term Format doc
		//
		uint32_t rank = (serial << 15) | id;

		if (node == A_LOCAL)
			return tag_short_pid(rank);
		else
		{
			term_t t = tag_boxed(es->htop);
			box_long_pid(es->htop, node, id, serial, creat);
			return t;
		}
	}
	case SMALL_TUPLE_EXT:
	{
		More2(1);
		uint32_t nelts = GetByte();
		if (nelts == 0)
			return ZERO_TUPLE;
		term_t t = tag_tuple(es->htop);
		*es->htop++ = nelts;
		term_t *elts = (term_t *)es->htop;
		es->htop += nelts;

		for (int i = 0; i < nelts; i++)
		{
			term_t e = ext_term_decode2(es);
			if (e == noval)
				return noval;
			elts[i] = e;
		}

		return t;
	}
	case LARGE_TUPLE_EXT:
	{
		More2(4);
		int nelts  = GetUint32();
		if (nelts < 0)
			return noval;
		if (nelts == 0)
			return ZERO_TUPLE;
		term_t t = tag_tuple(es->htop);
		*es->htop++ = nelts;
		term_t *elts = (term_t *)es->htop;
		es->htop += nelts;

		for (int i = 0; i < nelts; i++)
		{
			term_t e = ext_term_decode2(es);
			if (e == noval)
				return noval;
			elts[i] = e;
		}

		return t;
	}
	case STRING_EXT:
	{
		More2(2);
		uint32_t len = GetUint16();
		uint8_t *str = es->enc_data;
		MoreSkip2(len);

		term_t t = nil;
		for (int i = len-1; i >= 0; i--)
		{
			term_t cons = tag_cons(es->htop);
			*es->htop++ = tag_int(str[i]);
			*es->htop++ = t;
			t = cons;
		}

		return t;
	}
	case LIST_EXT:
	{
		More2(4);
		int len = GetUint32();
		if (len < 0)
			return noval;
		term_t first = noval;
		term_t *ref = &first;
		int left = len;
		while (left > 0)
		{
			term_t *cons = es->htop;
			es->htop += 2;

			term_t v = ext_term_decode2(es);
			if (v == noval)
				return noval;
			cons[0] = v;
			*ref = tag_cons(cons);
			ref = &cons[1];

			left--;
		}

		term_t tail = ext_term_decode2(es);
		if (tail == noval)
			return noval;
		*ref = tail;

		return first;
	}
	case BINARY_EXT:
	{
		More2(4);
		uint32_t len = GetUint32();
		uint8_t *data = es->enc_data;
		MoreSkip2(len);

		term_t t = tag_boxed(es->htop);
		if (len > MAX_HEAP_BIN)
		{
			// off-heap binary
			binnode_t *node = binnode_make(len);
			memcpy(node->starts, data, len);
			t_proc_bin_t *pb = (t_proc_bin_t *)es->htop;
			box_proc_bin(es->htop, len, node);

			proc_bin_link(es->pbs, pb, es->pb_size);
		}
		else
		{
			// heap binary
			box_heap_bin(es->htop, len, data);
		}
		return t;
	}
	case SMALL_BIG_EXT:
	{
		More2(1+1);
		int len = GetByte();
		if (len <= 1)
			return noval;
		return complete_bignum_decode(len, es);
	}
	case LARGE_BIG_EXT:
	{
		More2(4+1);
		int len = GetUint32();
		if (len <= 1)
			return noval;
		return complete_bignum_decode(len, es);
	}
	case NEW_REFERENCE_EXT:
	{
		More2(2);
		int len = GetUint16();
		if (len < 1 || len > 3)
			return noval;

		term_t node = ext_term_decode2(es);
		if (node == noval || !is_atom(node))
			return noval;
		More2(1);
		uint32_t creat = GetByte();

		uint32_t id0 = 0;
		uint32_t id1 = 0;
		uint32_t id2 = 0;

		More2(4);
		id0 = GetUint32();
		if (len > 1)
		{
			More2(4);
			id1 = GetUint32();
		}
		if (len > 2)
		{
			More2(4);
			id2 = GetUint32();
		}

		if ((id0 & ~0x3ffff) != 0 || (creat & ~3) != 0)
			return noval;

		term_t t = tag_boxed(es->htop);
		box_long_ref(es->htop, node, creat, id0, id1, id2);
		return t;
	}
	case FUN_EXT:	
	{
		More2(4);
		uint32_t num_free = GetUint32();
		term_t pid = ext_term_decode2(es);
		if (pid == noval)
			return noval;
		term_t module = ext_term_decode2(es);
		if (module == noval)
			return noval;
		term_t index2 = ext_term_decode2(es);
		if (index2 == noval)
			return noval;
		term_t uniq = ext_term_decode2(es);
		if (uniq == noval || !is_int(uniq))
			return noval;

		if (!is_int(index2))
			return noval;
		int ix = int_value(index2);
		if (ix < 0 || ix > 255)
			return noval;

		//XXX: where is arity? TODO

		// dummy uniq value
		uint32_t zeros[] = {0, 0, 0, 0};

		int old_index = ix;
		int old_uniq = int_value(uniq);

		return complete_fun_decode(num_free, 0, pid, module, ix, zeros, old_index, old_uniq, 0, es);
	}
	case NEW_FUN_EXT:
	{
		More2(4+1+16+4+4);
		unsigned char *saved_ptr = es->enc_data;
		uint32_t size = GetUint32();
		int arity = (uint32_t)GetByte();
		uint32_t uq[4];
		uq[0] = GetUint32();
		uq[1] = GetUint32();
		uq[2] = GetUint32();
		uq[3] = GetUint32();
		int ix = GetUint32();
		if (ix < 0)
			return noval;
		uint32_t num_free = GetUint32();

		term_t module = ext_term_decode2(es);
		if (module == noval || !is_atom(module))
			return noval;
		term_t t_old_index = ext_term_decode2(es);
		if (t_old_index == noval || !is_int(t_old_index))
			return noval;
		term_t t_old_uniq = ext_term_decode2(es);
		if (t_old_uniq == noval || !is_int(t_old_uniq))
			return noval;
		term_t pid = ext_term_decode2(es);
		if (pid == noval)
			return noval;

		int old_index = int_value(t_old_index);
		int old_uniq = int_value(t_old_uniq);

		term_t t = complete_fun_decode(num_free, arity, pid,
				 	  module, ix, uq, old_index, old_uniq, 0, es);
		if (t == noval)
			return noval;
		if (es->enc_data - saved_ptr != size)
			return noval;

		return t;
	}
	case EXPORT_EXT:	
	{
		term_t module = ext_term_decode2(es);
		if (module == noval || !is_atom(module))
			return noval;
		term_t function = ext_term_decode2(es);
		if (function == noval || !is_atom(function))
			return noval;
		term_t arity2 = ext_term_decode2(es);
		if (arity2 == noval)
			return noval;

		if (!is_int(arity2))
			return noval;
		int a = int_value(arity2);
		if (a < 0 || a > 255)
			return noval;

		export_t *e = code_base_lookup_or_create_N(module, function, a);
		if (e == 0)
			no_memory_signal();

		term_t t = tag_boxed(es->htop);
		box_export(es->htop, e);
		return t;
	}
	case BIT_BINARY_EXT:
	{
		More2(4+1);
		int len = GetUint32();
		uint8_t bits = GetByte();
		uint8_t *data = es->enc_data;
		MoreSkip2(len);

		if (bits > 7)
			return noval;
		if (len == 0 && bits > 0)
			return noval;

		term_t bin = tag_boxed(es->htop);
		int is_writable = 1;
		if (len > MAX_HEAP_BIN)
		{
			// off-heap binary
			binnode_t *node = binnode_make(len);
			memcpy(node->starts, data, len);
			t_proc_bin_t *pb = (t_proc_bin_t *)es->htop;
			box_proc_bin(es->htop, len, node);

			proc_bin_link(es->pbs, pb, es->pb_size);
		}
		else
		{
			// heap binary
			box_heap_bin(es->htop, len, data);
			is_writable = 0;
		}

		if (bits > 0)
		{
			term_t t = tag_boxed(es->htop);
			box_sub_bin(es->htop, bin, 0, len*8 -8 +bits, is_writable);
			return t;
		}
		else
			return bin;
	}
	case NEW_FLOAT_EXT:
	{
		More2(8);
		union {
			uint64_t ull;
			double val;
		} u;

		u.ull = GET_UINT_64(es->enc_data);
		Skip(8);

		if (!isfinite(u.val))
			return noval;

		term_t t = tag_boxed(es->htop);
		box_float(es->htop, u.val);
		return t;
	}
	}

	return noval;
}

// The function is implementation of SMALL_BIG_EXT and
// LARGE_BIG_EXT cases for in ext_term_decode2 function.
// A separate function is needed because of the dynamic
// array.

static term_t complete_bignum_decode(int len, ext_term_scan_t *es)
{
	int s = GetByte();
	int sign = (s == 0) ?MP_ZPOS :MP_NEG;
	uint8_t *p = es->enc_data;
	MoreSkip2(len);

	int nrd = (len +1) /2;
	uint16_t digs[nrd];

	uint8_t *end = p +len;
	for (int i = 0; i < nrd; i++)
	{
		uint8_t l = *p++;
		uint8_t h = (p < end) ?*p :0; p++;
		digs[i] = (h << 8) | l;
	}

	term_t t = tag_boxed(es->htop);
	box_bignum(es->htop, sign, nrd, digs);
	return t;
}

static term_t complete_fun_decode(int num_free, int arity, term_t pid,
	   term_t module, int index, uint32_t *uniq, int old_index, int old_uniq,
	   fun_entry_t *fe, ext_term_scan_t *es)
{
	term_t free[num_free];
	for (int i = 0; i < num_free; i++)
	{
		term_t v = ext_term_decode2(es);
		if (v == noval)
			return noval;
		free[i] = v;
	}

	term_t t = tag_boxed(es->htop);
	box_fun(es->htop, num_free, arity, pid,
		   module, index, uniq, old_index, old_uniq, fe, free);

	return t;
}

static int encode_size2(int depth, term_t t, int minor_ver);

int ext_term_encode_size(term_t t, int minor_ver)
{
	int s = encode_size2(1, t, minor_ver);	//+1 for 131
	if (s < 0)
		return s;

	return 1 +s;
}

static int encode_size2(int depth, term_t t, int minor_ver)
{
	if (depth > EXT_TERM_MAX_DEPTH)
		return -TOO_DEEP;
	
	if (is_int(t))
	{
		if (int_value(t) >= 0 && int_value(t) < 256)
			return 1 +1;	//97
		else
			return 1 +4; //98
	}
	else if (is_atom(t))
	{
		uint8_t *aname = atoms_get(atom_index(t));
		return 1 +1 +aname[0]; //115
	}
	else if (is_nil(t))
		return 1; //106
	else if (is_cons(t))
	{
		int len = byte_list_size(t);
		if (len >= 0 && len <= 65535)
			return 1 +2 +len; //107
		else
		{
			int size = 1 +4; //108
			do {
				uint32_t *cons = peel_cons(t);
				int s = encode_size2(depth+1, cons[0], minor_ver);
				if (s < 0)
					return s;
				size += s;
				t = cons[1];
			} while (is_cons(t));

			int s = encode_size2(depth+1, t, minor_ver);
			if (s < 0)
				return s;
			size += s;

			return size;
		}
	}
	else if (is_tuple(t))
	{
		uint32_t *tdata = peel_tuple(t);
		int size = 1;
		if (tdata[0] < 256)
			size += 1; //104
		else
			size += 4; //105
		for (int i = 1; i <= tdata[0]; i++)
		{
			int s = encode_size2(depth+1, tdata[i], minor_ver);
			if (s < 0)
				return s;
			size += s;
		}
		return size;
	}
	else if (is_boxed(t))
	{
		uint32_t *tdata = peel_boxed(t);
		switch (boxed_tag(tdata))
		{
		case SUBTAG_PROC_BIN:
		case SUBTAG_HEAP_BIN:
		case SUBTAG_SUB_BIN:
		case SUBTAG_MATCH_CTX:
		{
			bits_t bs;
			bits_get_real(tdata, &bs);
			int64_t bcount = bs.ends - bs.starts;
			if ((bcount & 7) == 0)
				return 1 +4 + bcount /8;			//109
			else
				return 1 +4 +1 + (bcount +7) /8;	//77
		}
		case SUBTAG_POS_BIGNUM:
		case SUBTAG_NEG_BIGNUM:
		{
			bignum_t *bn = (bignum_t *)tdata;

			int len = bn->used *2;
			uint16_t *last = &bn->dp[bn->used -1];
			do {
				if (*last > 255)
					break;
				if (*last > 0)
				{
					len--;
					break;
				}
				last--;
				len -= 2;
			} while (len > 1);	// bignums are big

			if (len < 256)
				return 1 +1 +1 +len;	//110
			else
				return 1 +4 +1 +len;	//111
		}
		case SUBTAG_FUN:
		{
			t_fun_t *fun = (t_fun_t *)tdata;
			int free_var_size = 0;
			int num_free = fun_num_free(tdata);
			for (int i = 0; i < num_free; i++)
			{
				int s = encode_size2(depth+1, fun->frozen[i], minor_ver);
				if (s < 0)
					return s;
				free_var_size += s;
			}
			term_t t_old_index = tag_int(fun->old_index);
			term_t t_old_uniq = tag_int(fun->old_uniq);
			int size = 1 +4 +1 +16 +4 +4 +free_var_size;

			int s = encode_size2(depth+1, fun->module, minor_ver);
			if (s < 0)
				return s;
			size += s;
			s = encode_size2(depth+1, t_old_index, minor_ver);
			if (s < 0)
				return s;
			size += s;
			s = encode_size2(depth+1, t_old_uniq, minor_ver);
			if (s < 0)
				return s;
			size += s;
			s = encode_size2(depth+1, fun->pid, minor_ver);
			if (s < 0)
				return s;
			size += s;

			return size;
		}
		case SUBTAG_EXPORT:
		{
			t_export_t *exp = (t_export_t *)tdata;
			int size = 1;
			int s = encode_size2(depth+1, exp->e->module, minor_ver);
			if (s < 0)
				return s;
			size += s;
			s = encode_size2(depth+1, exp->e->function, minor_ver);
			if (s < 0)
				return s;
			size += s;
			size += 2; //113

			return size;
		}
		case SUBTAG_PID:
		{
			t_long_pid_t *pid = (t_long_pid_t *)tdata;
			int size = 1;
			int s = encode_size2(depth+1, pid->node, minor_ver);
			if (s < 0)
				return s;
			size += s +4 +4 +1;
			return size;
		}
		case SUBTAG_REF:
		{
			t_long_ref_t *ref = (t_long_ref_t *)tdata;
			int len = 1;
			if (ref->id1 != 0)
				len = 2;
			if (ref->id2 != 0)
				len = 3;
			int size = 1 +2;
		   	int s = encode_size2(depth+1, ref->node, minor_ver);
		   	if (s < 0)
				return s;
			size += s +1 +len*4;
			return size;
		}
		case SUBTAG_OID:
		{
			t_long_oid_t *oid = (t_long_oid_t *)tdata;
			int size = 1;
		   	int s = encode_size2(depth+1, oid->node, minor_ver);
		   	if (s < 0)
				return s;
			size += s +4 +1;
			return size;
		}
		default:
			assert(boxed_tag(tdata) == SUBTAG_FLOAT);
			if (minor_ver == 1)
				return 1 +8;	//70
			else
				return 1 +31;	//99
		}
	}
	else if (is_short_pid(t))
	{
		int size = 1;
		int s = encode_size2(depth+1, A_LOCAL, minor_ver);
	   	if (s < 0)
			return s;
		size += s +4 +4 +1;	//103
		return size;
	}
	else
	{
		assert(is_short_oid(t));
		int size = 1;
	   	int s = encode_size2(depth+1, A_LOCAL, minor_ver);
	   	if (s < 0)
			return s;
		size += s +4 +1; 	//102
		return size;
	}
}

typedef struct enc_ctx_t enc_ctx_t;
struct enc_ctx_t {
	uint8_t *enc_data;
	int left;
	int minor_ver;
};

#define PutByte(b) do { \
	assert(ec->left >= 1); \
	*ec->enc_data++ = (b); \
	ec->left--; \
} while (0)
#define PutUint16(s) do { \
	assert(ec->left >= 2); \
	PUT_UINT_16(ec->enc_data, (s)); \
	ec->enc_data += 2; \
	ec->left -= 2; \
} while (0)
#define PutUint16LE(s) do { \
	assert(ec->left >= 2); \
	PUT_UINT_16_LE(ec->enc_data, (s)); \
	ec->enc_data += 2; \
	ec->left -= 2; \
} while (0)

#define PutUint32(w) do { \
	assert(ec->left >= 4); \
	PUT_UINT_32(ec->enc_data, (w)); \
	ec->enc_data += 4; \
	ec->left -= 4; \
} while (0)
#define PutBytes(p, len) do { \
	assert(ec->left >= (len)); \
	memcpy(ec->enc_data, (p), (len)); \
	ec->enc_data += (len); \
	ec->left -= (len); \
} while (0)

static void encode2(term_t t, enc_ctx_t *ec);

void ext_term_encode(term_t t, uint8_t *enc_data, int expected_size, int minor_ver)
{
	assert(expected_size > 0);
	*enc_data++ = 131;
	expected_size--;

	enc_ctx_t ec = {
		.enc_data = enc_data,
		.left = expected_size,
		.minor_ver = minor_ver
	};

	encode2(t, &ec);
	assert(ec.left == 0);
}

static void encode2(term_t t, enc_ctx_t *ec)
{
	if (is_int(t))
	{
		int v = int_value(t);
		if (v >= 0 && v < 256)
		{
			PutByte(SMALL_INTEGER_EXT);
			PutByte(v);
		}
		else
		{
			PutByte(INTEGER_EXT);
			PutUint32((uint32_t)v);
		}
	}
	else if (is_atom(t))
	{
		uint8_t *aname = atoms_get(atom_index(t));
		uint8_t len = aname[0];
		PutByte(SMALL_ATOM_EXT);
		PutByte(len);
		PutBytes(aname+1, len);
	}
	else if (is_nil(t))
	{
		PutByte(NIL_EXT);
	}
	else if (is_cons(t))
	{
		int len = byte_list_size(t);
		if (len >= 0 && len <= 65535)
		{
			PutByte(STRING_EXT);
			PutUint16(len);
			assert(ec->left >= len);
			byte_list_flatten(t, ec->enc_data);
			ec->enc_data += len;
			ec->left -= len;
		}
		else
		{
			PutByte(LIST_EXT);
			uint8_t *lenp = ec->enc_data;
			PutUint32(0);	// actual length set later
			len = 0;
			do {
				uint32_t *cons = peel_cons(t);
				encode2(cons[0], ec);
				len++;
				t = cons[1];
			} while (is_cons(t));
			encode2(t, ec);

			PUT_UINT_32(lenp, len);
		}
	}
	else if (is_tuple(t))
	{
		uint32_t *tdata = peel_tuple(t);
		uint32_t len = tdata[0];
		if (len < 256)
		{
			PutByte(SMALL_TUPLE_EXT);
			PutByte(len);
		}
		else
		{
			PutByte(LARGE_TUPLE_EXT);
			PutUint32(len);
		}
		for (int i = 1; i <= len; i++)
			encode2(tdata[i], ec);
	}
	else if (is_boxed(t))
	{
		uint32_t *tdata = peel_boxed(t);
		switch (boxed_tag(tdata))
		{
		case SUBTAG_PROC_BIN:
		case SUBTAG_HEAP_BIN:
		case SUBTAG_SUB_BIN:
		case SUBTAG_MATCH_CTX:
		{
			bits_t bs;
			bits_get_real(tdata, &bs);
			int64_t bcount = bs.ends - bs.starts;
			if ((bcount & 7) == 0)
			{
				uint32_t size = bcount /8;
				PutByte(BINARY_EXT);
				PutUint32(size);

				if ((bs.starts & 7) == 0)
					PutBytes(bs.data + bs.starts/8, size);
				else
				{
					while (bits_has_word(&bs))
					{
						uint32_t w;
						bits_get_word(&bs, w);
						PutUint32(w);
					}

					while (bits_has_octet(&bs))
					{
						uint8_t o;
						bits_get_octet(&bs, o);
						PutByte(o);
					}
				}
			}
			else
			{
				uint8_t bo = bcount & 7;
				uint32_t size = (bcount +7) /8;
				PutByte(BIT_BINARY_EXT);
				PutUint32(size);
				PutByte(bo);

				while (bits_has_word(&bs))
				{
					uint32_t w;
					bits_get_word(&bs, w);
					PutUint32(w);
				}

				while (bits_has_octet(&bs))
				{
					uint8_t o;
					bits_get_octet(&bs, o);
					PutByte(o);
				}

				uint8_t trailer = bits_get_trailer(&bs);
				uint8_t last_byte = trailer << (8-bo);
				PutByte(last_byte);
			}
			break;
		}
		case SUBTAG_POS_BIGNUM:
		case SUBTAG_NEG_BIGNUM:
		{
			bignum_t *bn = (bignum_t *)tdata;
			uint8_t sign = (bignum_is_neg(bn)) ?1 :0;

			int len = bn->used *2;
			uint16_t *last = &bn->dp[bn->used -1];
			do {
				if (*last > 255)
					break;
				if (*last > 0)
				{
					len--;
					break;
				}
				last--;
				len -= 2;
			} while (len > 1);	// bignums are big

			if (len < 256)
			{
				PutByte(SMALL_BIG_EXT);
				PutByte(len);
			}
			else
			{
				PutByte(LARGE_BIG_EXT);
				PutUint32(len);
			}
			PutByte(sign);

			int left = len;
			int i = 0;
			while (left > 0)
			{
				uint16_t d = bn->dp[i++];
				uint8_t l = (uint8_t)d;
				uint8_t h = d >> 8;
				PutByte(l); left--; if(left == 0) break;
				PutByte(h); left--;
			}

			break;
		}
		case SUBTAG_FUN:
		{
			t_fun_t *fun = (t_fun_t *)tdata;
			int num_free = fun_num_free(tdata);
			int arity = fun_arity(tdata);

			term_t t_old_index = tag_int(fun->old_index);
			term_t t_old_uniq = tag_int(fun->old_uniq);

			PutByte(NEW_FUN_EXT);
			uint8_t *sizep = ec->enc_data;
			PutUint32(0);	// filled in later
			PutByte(arity);
			PutUint32(fun->uniq[0]);
			PutUint32(fun->uniq[1]);
			PutUint32(fun->uniq[2]);
			PutUint32(fun->uniq[3]);
			PutUint32(fun->index);
			PutUint32(num_free);
			encode2(fun->module, ec);
			encode2(t_old_index, ec);
			encode2(t_old_uniq, ec);
			encode2(fun->pid, ec);
			for (int i = 0; i < num_free; i++)
				encode2(fun->frozen[i], ec);
			uint32_t size = ec->enc_data - sizep;
			PUT_UINT_32(sizep, size);
			break;
		}
		case SUBTAG_EXPORT:
		{
			t_export_t *exp = (t_export_t *)tdata;
			PutByte(EXPORT_EXT);
			encode2(exp->e->module, ec);
			encode2(exp->e->function, ec);
			PutByte(SMALL_INTEGER_EXT);
			PutByte(exp->e->arity);
			break;
		}
		case SUBTAG_MAP:
		{
			t_map_t *map = (t_map_t *)tdata;
			uint32_t *p = peel_tuple(map->keys);
			uint32_t size = *p++;
			PutByte(MAP_EXT);
			PutUint32(size);
			for (int i = 0; i < size; i++)
			{
				encode2(*p++, ec);
				encode2(map->values[i], ec);
			}
			break;		
		}
		case SUBTAG_PID:
		{
			t_long_pid_t *pid = (t_long_pid_t *)tdata;
			uint32_t id = opr_hdr_id(pid);
			uint8_t creat = opr_hdr_creat(pid);
			PutByte(PID_EXT);
			encode2(pid->node, ec);
			PutUint32(id);
			PutUint32(pid->serial);
			PutByte(creat);
			break;
		}
		case SUBTAG_REF:
		{
			t_long_ref_t *ref = (t_long_ref_t *)tdata;
			uint32_t id0 = opr_hdr_id(ref);
			uint8_t creat = opr_hdr_creat(ref);
			int len = 1;
			if (ref->id1 != 0)
				len = 2;
			if (ref->id2 != 0)
				len = 3;
			PutByte(NEW_REFERENCE_EXT);
			PutUint16(len);
			encode2(ref->node, ec);
			PutByte(creat);
			PutUint32(id0);
			if (len > 1)
				PutUint32(ref->id1);
			if (len > 2)
				PutUint32(ref->id2);
			break;
		}
		case SUBTAG_OID:
		{
			t_long_oid_t *oid = (t_long_oid_t *)tdata;
			uint32_t id = opr_hdr_id(oid);
			uint8_t creat = opr_hdr_creat(oid);
			PutByte(PORT_EXT);
			encode2(oid->node, ec);
			PutUint32(id);
			PutByte(creat);
			break;
		}
		default:
			assert(boxed_tag(tdata) == SUBTAG_FLOAT);
			if (ec->minor_ver == 1)
			{
				union {
					double dbl;
					uint64_t u64;
				} u;
				u.dbl = float_value(tdata);
				PutByte(NEW_FLOAT_EXT);
				assert(ec->left >= 8);
				PUT_UINT_64(ec->enc_data, u.u64);
				ec->enc_data += 8;
				ec->left -= 8;
			}
			else
			{
				PutByte(FLOAT_EXT);
				assert(ec->left >= 31);
				memset(ec->enc_data, 0, 31);
				snprintf((char *)ec->enc_data, 31, "%.20e", float_value(tdata));
				ec->enc_data += 31;
				ec->left -= 31;
			}
			break;
		}
	}
	else if (is_short_pid(t))
	{
		PutByte(PID_EXT);
		encode2(A_LOCAL, ec);

		// id contains 15 least significant bits of the rank, serial - the rest
		// 			See External Term Format doc
		//
		uint32_t rank = short_pid_id(t);
		uint32_t id = rank & 0x7fff;
		uint32_t serial = rank >> 15;

		PutUint32(id);
		PutUint32(serial);	// serial
		PutByte(0);			// creation
	}
	else
	{
		assert(is_short_oid(t));
		PutByte(PORT_EXT);
		encode2(A_LOCAL, ec);
		uint32_t id = short_oid_id(t);
		PutUint32(id);
		PutByte(0);		// creation
	}
}

//EOF
