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

double strtod(const char *str, char **endptr);

static int term_compare(const void *p1, const void *p2)
{
	term_t a = *(term_t *)p1;
	term_t b = *(term_t *)p2;

	if (is_term_smaller(a, b))
		return -1;
	if (is_term_smaller(b, a))
		return 1;

	return 0;
}

term_t bif_is_pid1(term_t Pid, proc_t *proc)
{
	if (is_short_pid(Pid) || is_boxed_pid(Pid))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_is_port1(term_t Port, proc_t *proc)
{
	if (is_short_oid(Port) || is_boxed_oid(Port))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_is_reference1(term_t Ref, proc_t *proc)
{
	if (is_boxed_ref(Ref))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_is_binary1(term_t Bin, proc_t *proc)
{
	if (is_boxed_binary(Bin))
		return A_TRUE;

	return A_FALSE;
}

// NB: it is possible to reuse bif_is_binary1 code
term_t bif_is_bitstring1(term_t Bin, proc_t *proc)
{
	if (is_boxed_binary(Bin))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_is_list1(term_t List, proc_t *proc)
{
	if (is_list(List))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_is_integer1(term_t N, proc_t *proc)
{
	if (is_int(N))
		return A_TRUE;
	if (is_boxed_bignum(N))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_is_float1(term_t N, proc_t *proc)
{
	if (is_boxed_float(N))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_is_number1(term_t N, proc_t *proc)
{
	if (is_int(N))
		return A_TRUE;
	if (is_boxed_bignum(N))
		return A_TRUE;
	if (is_boxed_float(N))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_is_atom1(term_t Atom, proc_t *proc)
{
	if (is_atom(Atom))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_is_boolean1(term_t Val, proc_t *proc)
{
	if (Val == A_TRUE || Val == A_FALSE)
		return A_TRUE;

	return A_FALSE;
}

term_t bif_is_tuple1(term_t Tuple, proc_t *proc)
{
	if (is_tuple(Tuple))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_is_function1(term_t Fun, proc_t *proc)
{
	if (is_boxed(Fun))
	{
		uint32_t stag = boxed_tag(peel_boxed(Fun));
		if (stag == SUBTAG_FUN || stag == SUBTAG_EXPORT)
			return A_TRUE;
	}

	return A_FALSE;
}

term_t bif_is_function2(term_t Fun, term_t Arity, proc_t *proc)
{
	if (!is_int(Arity))
		badarg(Arity);
	int arity = int_value(Arity);
	if (arity < 0)
		badarg(Arity);

	if (is_boxed(Fun))
	{
		uint32_t *tdata = peel_boxed(Fun);
		uint32_t stag = boxed_tag(tdata);
		if (stag == SUBTAG_FUN)
			return (fun_arity(tdata) == arity + fun_num_free(tdata))
				?A_TRUE
				:A_FALSE;
		else if (stag == SUBTAG_EXPORT)
		{
			t_export_t *exp = (t_export_t *)tdata;
			return (exp->e->arity == arity)
				?A_TRUE
				:A_FALSE;
		}
	}

	return A_FALSE;
}

term_t bif_and2(term_t A, term_t B, proc_t *proc)
{
	if (!is_bool(A))
		badarg(A);
	if (!is_bool(B))
		badarg(B);
	if (A == A_TRUE && B == A_TRUE)
		return A_TRUE;

	return A_FALSE;
}

term_t bif_or2(term_t A, term_t B, proc_t *proc)
{
	if (!is_bool(A))
		badarg(A);
	if (!is_bool(B))
		badarg(B);
	if (A == A_TRUE || B == A_TRUE)
		return A_TRUE;

	return A_FALSE;
}

term_t bif_xor2(term_t A, term_t B, proc_t *proc)
{
	if (!is_bool(A))
		badarg(A);
	if (!is_bool(B))
		badarg(B);
	if ((A == A_TRUE) ^ (B == A_TRUE))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_not1(term_t Term, proc_t *proc)
{
	//TODO: should this be an instruction?
	
	if (Term == A_TRUE)
		return A_FALSE;
	if (Term == A_FALSE)
		return A_TRUE;

	badarg(Term);
}

term_t bif_less2(term_t A, term_t B, proc_t *proc)
{
	if (is_term_smaller(A, B))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_more2(term_t A, term_t B, proc_t *proc)
{
	if (is_term_smaller(B, A))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_less_eq2(term_t A, term_t B, proc_t *proc)
{
	if (!is_term_smaller(B, A))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_more_eq2(term_t A, term_t B, proc_t *proc)
{
	if (!is_term_smaller(A, B))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_eq_exact2(term_t A, term_t B, proc_t *proc)
{
	if (A == B || are_terms_equal(A, B, 1))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_eq2(term_t A, term_t B, proc_t *proc)
{
	if (A == B || are_terms_equal(A, B, 0))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_ne_exact2(term_t A, term_t B, proc_t *proc)
{
	if (A != B && !are_terms_equal(A, B, 1))
		return A_TRUE;

	return A_FALSE;
}

term_t bif_ne2(term_t A, term_t B, proc_t *proc)
{
	if (A != B && !are_terms_equal(A, B, 0))
		return A_TRUE;

	return A_FALSE;
}

term_t gc_bif_length1(term_t List, proc_t *proc, term_t *regs, int live)
{
	if (!is_list(List))
		badarg(List);
	int len = list_len(List);
	if (len < 0)
		badarg(List); // odd list
	return int_to_term(len, &proc->hp);
}

term_t bif_size1(term_t TupleBin, proc_t *proc)
{
	if (is_tuple(TupleBin))
	{
		int arity = *peel_tuple(TupleBin);
		assert(fits_int(arity)); //TODO
		return tag_int(arity);
	}
	else if (is_boxed(TupleBin))
	{
		uint32_t *tdata = peel_boxed(TupleBin);
		if (is_binary(tdata))
		{
			bits_t bs;
			bits_get_real(tdata, &bs);
			uint32_t sz = (bs.ends - bs.starts) /8;
			return int_to_term(sz, &proc->hp);
		}
	}

	badarg(TupleBin);
}

term_t bif_tuple_size1(term_t Tuple, proc_t *proc)
{
	if (!is_tuple(Tuple))
		badarg(Tuple);
	int arity = *peel_tuple(Tuple);
	return int_to_term(arity, &proc->hp);
}

term_t bif_map_size1(term_t Map, proc_t *proc)
{
	if (!is_boxed_map(Map))
		badarg(Map);
	int size = map_size(peel_boxed(Map));
	return int_to_term(size, &proc->hp);
}

term_t gc_bif_byte_size1(term_t Bin, proc_t *proc, term_t *regs, int live)
{
	if (!is_boxed(Bin) || !is_binary(peel_boxed(Bin)))
		badarg(Bin);
	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);
	int sz = (bs.ends - bs.starts +7)/8;
	assert(fits_int(sz));
	return tag_int(sz);
}

term_t gc_bif_bit_size1(term_t Bin, proc_t *proc, term_t *regs, int live)
{
	if (!is_boxed(Bin) || !is_binary(peel_boxed(Bin)))
		badarg(Bin);
	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);
	uint32_t bcount = bs.ends-bs.starts;
	return int_to_term(bcount, &proc->hp);
}

term_t gc_bif_float1(term_t Value, proc_t *proc, term_t *regs, int live)
{
	double converted;
	if (is_int(Value))
		converted = (double)int_value(Value);
	else if (is_boxed(Value))
	{
		uint32_t *tdata = peel_boxed(Value);
		if (is_bignum(tdata))
		{
			converted = bignum_to_double((bignum_t *)tdata);
			if (!isfinite(converted))
				badarg(Value);
		}
		else if (boxed_tag(tdata) == SUBTAG_FLOAT)
			return Value;
		else
			badarg(Value);
	}
	else
		badarg(Value);

	return heap_float(&proc->hp, converted);
}

term_t gc_bif_trunc1(term_t Value, proc_t *proc, term_t *regs, int live)
{
	if (is_int(Value) || is_boxed_bignum(Value))
		return Value;
	if (!is_boxed(Value))
		badarg(Value);
	uint32_t *tdata = peel_boxed(Value);
	if (boxed_tag(tdata) != SUBTAG_FLOAT)
		badarg(Value);
	double v = float_value(tdata);

	return float_to_int(v, &proc->hp);
}

term_t gc_bif_round1(term_t Value, proc_t *proc, term_t *regs, int live)
{
	// The standard approach is to round numbers like 1.5, 3.5, -0.5 to the
	// nearest even number. The routine rounds them to an even number if the
	// original number, if positive. And to an odd one, if negative.

	if (is_int(Value))
		return Value;	// no rounding needed
	if (!is_boxed(Value))
		badarg(Value);
	uint32_t *tdata = peel_boxed(Value);
	if (is_bignum(tdata))
		return Value;	// no rounding needed
	if (boxed_tag(tdata) != SUBTAG_FLOAT)
		badarg(Value);
	double v = float_value(tdata);

	if (v > 0.0)
		v += 0.5;
	else
		v -= 0.5;

	return float_to_int(v, &proc->hp);
}

term_t gc_bif_abs1(term_t Value, proc_t *proc, term_t *regs, int live)
{
	if (is_int(Value))
	{
		int v = int_value(Value);
		if (v < 0)
			v = -v;
		return tag_int(v);	//no overflow
	}
	else if (is_boxed(Value))
	{
		uint32_t *tdata = peel_boxed(Value);
		if (boxed_tag(tdata) == SUBTAG_FLOAT)
		{
			double v = float_value(tdata);
			if (v < 0)
				v = -v;
			return heap_float(&proc->hp, v);
		}
		if (is_bignum(tdata))
		{
			bignum_t *bn = (bignum_t *)tdata;
			if (bignum_is_neg(bn))
			{
				uint16_t *digits;
				term_t r = heap_bignum(&proc->hp, MP_ZPOS, bn->used, &digits);
				memcpy(digits, bn->dp, bn->used*2);
				return r;
			}
			else
				return Value;
		}
	}
	
	badarg(Value);
}

term_t cbif_pid_to_list1(proc_t *proc, term_t *regs)
{
	char buf[256];
	
	//TODO: use term_to_str() here?

	term_t Pid = regs[0];
	if (is_short_pid(Pid))
	{
		snprintf(buf, sizeof(buf), "<0.%d.0>", short_pid_id(Pid));
		return heap_strz(&proc->hp, buf);
	}
	else if (is_boxed(Pid))
	{
		uint32_t *pdata = peel_boxed(Pid);
		if (boxed_tag(pdata) == SUBTAG_PID)
		{
			t_long_pid_t *p = (t_long_pid_t *)pdata;
			snprintf(buf, sizeof(buf), "<%pt.%d.%d.%d>",
					T(p->node), opr_hdr_id(pdata),
				   	p->serial, opr_hdr_creat(pdata));
			return heap_strz(&proc->hp, buf);
		}
	}

	badarg(Pid);
}

term_t cbif_list_to_pid1(proc_t *proc, term_t *regs)
{
	term_t List = regs[0];
	if (!is_list(List))
		badarg(List);
	int len = byte_list_size(List);
	if (len < 0)
		badarg(List);
	char pad[len+1];
	byte_list_flatten(List, (uint8_t *)pad);
	pad[len] = 0;

	// <0.0.0>
	if (len < 4 || pad[0] != '<')
		badarg(List);
	char *p = pad;
	int n1 = strtoi64(p+1, &p, 10);
	if (*p != '.')
		badarg(List);
	int n2 = strtoi64(p+1, &p, 10);
	if (*p != '.')
		badarg(List);
	int n3 = strtoi64(p+1, &p, 10);
	if (*p != '>')
		badarg(List);

	if (n1 != 0 || n3 != 0)
		not_implemented("list_to_pid for long pids");

	return tag_short_pid(n2);
}

term_t cbif_fun_to_list1(proc_t *proc, term_t *regs)
{
	term_t Fun = regs[0];
	if (!is_boxed(Fun))
		badarg(Fun);
	int btag = boxed_tag(peel_boxed(Fun));
	if (btag != SUBTAG_FUN && btag != SUBTAG_EXPORT)
		badarg(Fun);

	char buf[512];
	term_to_str(Fun, buf, sizeof(buf));
	return heap_strz(&proc->hp, buf);
}

term_t cbif_port_to_list1(proc_t *proc, term_t *regs)
{
	term_t Oid = regs[0];
	if (!is_short_oid(Oid) &&
			!(is_boxed(Oid) && boxed_tag(peel_boxed(Oid)) == SUBTAG_OID))
		badarg(Ref);

	char buf[512];
	term_to_str(Oid, buf, sizeof(buf));
	return heap_strz(&proc->hp, buf);
}

term_t cbif_ref_to_list1(proc_t *proc, term_t *regs)
{
	term_t Ref = regs[0];
	if (!(is_boxed(Ref) && boxed_tag(peel_boxed(Ref)) == SUBTAG_REF))
		badarg(Ref);

	char buf[512];
	term_to_str(Ref, buf, sizeof(buf));
	return heap_strz(&proc->hp, buf);
}

term_t cbif_list_to_atom1(proc_t *proc, term_t *regs)
{
	term_t List = regs[0];
	if (!is_list(List))
		badarg(List);
	int len = byte_list_size(List);
	if (len < 0 || len > 255)
		badarg(List);
	uint8_t print_name[len +1];
	print_name[0] = len;
	byte_list_flatten(List, print_name +1);

	int index = atoms_set(print_name);
	return tag_atom(index);
}

term_t cbif_list_to_existing_atom1(proc_t *proc, term_t *regs)
{
	term_t List = regs[0];
	if (!is_list(List))
		badarg(List);
	int len = byte_list_size(List);
	if (len < 0 || len > 255)
		badarg(List);
	uint8_t print_name[len +1];
	print_name[0] = len;
	byte_list_flatten(List, print_name +1);

	if (!atom_exists(print_name))
		badarg(List);

	int index = atoms_set(print_name);
	return tag_atom(index);
}

term_t cbif_atom_to_list1(proc_t *proc, term_t *regs)
{
	term_t Atom = regs[0];
	if (!is_atom(Atom))
		badarg(Atom);

	uint8_t *print_name = atoms_get(atom_index(Atom));
	return heap_str(&proc->hp, (const char *)print_name+1, print_name[0]);
}

term_t cbif_integer_to_list1(proc_t *proc, term_t *regs)
{
	term_t N = regs[0];
	if (is_int(N))
	{
		int v = int_value(N);
		char buf[16];
		char *ptr = buf + sizeof(buf)-1;
		*ptr-- = 0;
		int is_neg = v < 0;
		if (v < 0)
			v = -v;
		do {
			int d = v % 10;
			v /= 10;
			*ptr-- = '0'+d;
		} while (v > 0);
		if (is_neg)
			*ptr = '-';
		else
			ptr++;
		return heap_strz(&proc->hp, ptr);
	}
	else if (is_boxed(N) && is_bignum(peel_boxed(N)))
	{
		//TODO: unbounded alloc: burn fat
		bignum_t *bn = (bignum_t *)peel_boxed(N);
		int n = bignum_str_size(bn);
		char buf[n];
		bignum_to_str(&proc->hp, bn, buf);
		return heap_strz(&proc->hp, buf);
	}

	badarg(N);
}

term_t cbif_list_to_float1(proc_t *proc, term_t *regs)
{
	term_t List = regs[0];
	if (!is_list(List))
		badarg(List);
	int len = byte_list_size(List);
	if (len < 0)
		badarg(List);
	char pad[len+1];
	byte_list_flatten(List, (uint8_t *)pad);
	pad[len] = 0;

	if (strchr(pad, '.') == 0)
		badarg(List);

	char *e;
	double v = strtod(pad, &e);
	if (e != pad+len)
		badarg(List);

	return heap_float(&proc->hp, v);
}

term_t cbif_float_to_list1(proc_t *proc, term_t *regs)
{
	term_t Val = regs[0];
	if (!is_boxed(Val))
		badarg(Val);
	uint32_t *tdata = peel_boxed(Val);
	if (boxed_tag(tdata) != SUBTAG_FLOAT)
		badarg(Val);
	char pad[256];
	snprintf(pad, sizeof(pad), "%.20e", float_value(tdata));

	return heap_strz(&proc->hp, pad);
}

term_t cbif_binary_to_list1(proc_t *proc, term_t *regs)
{
	term_t Bin = regs[0];
	if (!is_boxed(Bin) || !is_binary(peel_boxed(Bin)))
		badarg(Bin);

	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);
	if (((bs.ends - bs.starts) & 7) != 0)
		badarg(Bin);

	int size = (bs.ends - bs.starts) /8;
	uint32_t *htop = heap_alloc(&proc->hp, size*2);//proc_burn_fat(proc, size*2, regs, 1);

	// reload after gc
	Bin = regs[0];
	bits_get_real(peel_boxed(Bin), &bs);

	term_t t = nil;
	term_t *tail = &t;
	while (bs.starts < bs.ends)
	{
		uint8_t o;
		bits_get_octet(&bs, o);

		*tail = tag_cons(htop);
		htop[0] = tag_int(o);
		htop[1] = nil;
		tail = &htop[1];
		htop += 2;
	}

	heap_set_top(&proc->hp, htop);
	return t;
}

term_t cbif_binary_to_list3(proc_t *proc, term_t *regs)
{
	term_t Bin = regs[0];
	term_t Start = regs[1];
	term_t Stop = regs[2];
	if (!is_boxed(Bin) || !is_binary(peel_boxed(Bin)))
		badarg(Bin);
	if (!is_int(Start) && !(is_boxed(Start) && is_bignum(peel_boxed(Start))))
		badarg(Start);
	if (!is_int(Stop) && !(is_boxed(Stop) && is_bignum(peel_boxed(Stop))))
		badarg(Stop);

	int beg = (is_int(Start))
		?int_value(Start)
		:bignum_to_int((bignum_t *)peel_boxed(Start));
	if (beg <= 0)
		badarg(Start);
	int end = (is_int(Stop))
		?int_value(Stop)
		:bignum_to_int((bignum_t *)peel_boxed(Stop));
	if (end <= 0 || end < beg)
		badarg(Stop);

	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);
	uint32_t bcount = bs.ends-bs.starts;
	if ((bcount & 7) != 0)
		badarg(Bin);

	if (end > bcount /8)
		badarg(Stop);

	int size = end -beg +1;
	uint32_t *htop = heap_alloc(&proc->hp, size*2);//proc_burn_fat(proc, size*2, regs, 3);

	// reload after gc
	Bin = regs[0];
	bits_get_real(peel_boxed(Bin), &bs);
	bs.ends = bs.starts + end*8;
	bs.starts = bs.starts + beg*8 -8;

	term_t t = nil;
	term_t *tail = &t;
	while (bs.starts < bs.ends)
	{
		uint8_t o;
		bits_get_octet(&bs, o);

		*tail = tag_cons(htop);
		htop[0] = tag_int(o);
		htop[1] = nil;
		tail = &htop[1];
		htop += 2;
	}

	heap_set_top(&proc->hp, htop);
	return t;
}

term_t cbif_bitstring_to_list1(proc_t *proc, term_t *regs)
{
	term_t Bin = regs[0];
	if (!is_boxed(Bin) || !is_binary(peel_boxed(Bin)))
		badarg(Bin);

	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);

	int size = (bs.ends - bs.starts +7) /8;
	int needed = 2*size;
	if (((bs.ends - bs.starts) & 7) != 0)
		needed += WSIZE(t_sub_bin_t) +2; // +2 for the last cons
	uint32_t *htop = heap_alloc(&proc->hp, needed);//proc_burn_fat(proc, needed, regs, 1);

	// reload after gc
	Bin = regs[0];
	bits_get_real(peel_boxed(Bin), &bs);

	term_t t = nil;
	term_t *tail = &t;
	while (bits_has_octet(&bs))
	{
		uint8_t o;
		bits_get_octet(&bs, o);

		*tail = tag_cons(htop);
		htop[0] = tag_int(o);
		htop[1] = nil;
		tail = &htop[1];
		htop += 2;
	}

	if (bs.starts < bs.ends)
	{
		term_t odd = tag_boxed(htop);
		box_sub_bin(htop, bin_parent(Bin), bs.starts, bs.ends, 0);
		*tail = tag_cons(htop);
		htop[0] = odd;
		htop[1] = nil;
		htop += 2;
	}

	heap_set_top0(&proc->hp, htop);
	return t;
}

term_t cbif_list_to_binary1(proc_t *proc, term_t *regs)
{
	term_t IoList = regs[0];
	if (!is_list(IoList))
		badarg(IoList);

	int64_t size = iolist_size(IoList);
	if (size == -TOO_DEEP)
		fail(A_SYSTEM_LIMIT);
	if (size > MAX_BYTE_SIZE)
		fail(A_SYSTEM_LIMIT);
	if (size < 0)
		badarg(IoList);

	uint8_t *ptr;
	term_t bin = heap_make_bin(&proc->hp, size, &ptr);

	UNUSED uint8_t *check_ptr = iolist_flatten(IoList, ptr); // never fails
	assert(check_ptr == ptr+size);

	return bin;
}

term_t cbif_list_to_bitstring1(proc_t *proc, term_t *regs)
{
	term_t BsList = regs[0];
	if (!is_list(BsList))
		badarg(BsList);

	int64_t bcount = bits_list_size(BsList);
	if (bcount == -TOO_DEEP || bcount == -TOO_LONG)
		fail(A_SYSTEM_LIMIT);
	if (bcount < 0)
		badarg(BsList);

	uint32_t sz = (bcount +7) /8;
	int uneven = (bcount & 7) != 0;
	bits_t bs;
	uint32_t *p;
	term_t bin;

	int writable = 1;
	if (sz > MAX_HEAP_BIN)
	{
		binnode_t *node = binnode_make(sz);

		int needed = WSIZE(t_proc_bin_t);
		if (uneven)
			needed += WSIZE(t_sub_bin_t);

		p = heap_alloc_N(&proc->hp, needed);
		if (p == 0)
		{
			binnode_destroy(node);
			no_memory_signal();
		}
		t_proc_bin_t *pb = (t_proc_bin_t *)p;
		box_proc_bin(p, sz, node);

		proc_bin_link(&proc->hp.proc_bins, pb, &proc->hp.total_pb_size);

		bs.data = node->starts;
		bin = tag_boxed(pb);
	}
	else
	{
		int needed = WSIZE(t_heap_bin_t) + (sz +3) /4;
		if (uneven)
			needed += WSIZE(t_sub_bin_t);

		p = heap_alloc(&proc->hp, needed);
		t_heap_bin_t *hb = (t_heap_bin_t *)p;
		box_heap_bin(p, sz, 0);

		writable = 0;	// sub bins of heap bin are not writable

		bs.data = hb->data;
		bin = tag_boxed(hb);
	}
	bs.starts = 0;
	bs.ends = bcount;

	bits_list_flatten(BsList, &bs);	// never fails

	if (uneven)
	{
		t_sub_bin_t *sb = (t_sub_bin_t *)p;
		box_sub_bin(p, bin, 0, bcount, writable);
		bin = tag_boxed(sb);
	}

	heap_set_top(&proc->hp, p);
	return bin;
}

term_t cbif_to_integer1(proc_t *proc, term_t *regs)
{
	term_t Str = regs[0];
	if (!is_list(Str))
		return heap_tuple2(&proc->hp, A_ERROR, A_NOT_A_LIST);

	term_t start = Str;
	int is_neg = 0;
	if (is_cons(start) && (peel_cons(start))[0] == tag_int('-'))
	{
		is_neg = 1;
		start = (peel_cons(start))[1];
	}

	int64_t acc = 0;
	int ndigs = 0;
	int is_big = 0;
	term_t l = start;

	while (is_cons(l))
	{
		term_t *cons = peel_cons(l);
		if (!is_int(cons[0]))
			break;
		int d = int_value(cons[0]);
		if (d < '0' || d > '9')
			break;
		ndigs++;
		if (!is_big)
		{
			acc *= 10;
			acc += d-'0';
			if (!fits_int(acc))
				is_big = 1;
		}
		l = cons[1];
	}

	if (is_big)
	{
		int len = ndigs;
		if (is_neg)
			len++;
		char pad[len+1];
		pad[len] = 0;
		char *ptr = pad;
		if (is_neg)
			*ptr++ = '-';
		l = start;
		int left = ndigs;
		do {
			term_t *cons = peel_cons(l);
			*ptr++ = int_value(cons[0]);
			l = cons[1];
			left--;
		} while (left > 0);
		assert(ptr == pad+len);

		bignum_t *bn = bignum_from_str(&proc->hp, pad);
		return heap_tuple2(&proc->hp, tag_boxed(bn), l);
	}
	else
	{
		if (ndigs == 0)
			return heap_tuple2(&proc->hp, A_ERROR, A_NO_INTEGER);

		return heap_tuple2(&proc->hp, tag_int(acc), l);
	}
}

term_t cbif_binary_to_term0_2(proc_t *proc, term_t *regs)
{
	term_t Bin = regs[0];
	term_t Safe = regs[1];
	if (!is_boxed(Bin) || !is_binary(peel_boxed(Bin)))
		badarg(Bin);
	if (!is_bool(Safe))
		badarg(Safe);

	//if (Safe == A_TRUE)
	//	printk("binary_to_term() safe option ignored\n");

	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);

	if (((bs.ends - bs.starts) & 7) != 0)
		badarg(Bin);

	uint8_t *enc_data = 0;
	int enc_len = (bs.ends - bs.starts) /8;
	
	if ((bs.starts & 7) != 0)
	{
		// happens rarely, make dynamic allocation
		enc_data = heap_tmp_buf(&proc->hp, enc_len);

		bits_t dst;
		bits_init_buf(enc_data, enc_len, &dst);
		bits_copy(&bs, &dst);
	}
	else
		enc_data = bs.data + bs.starts /8;

	int needed = ext_term_decode_size(enc_data, enc_len, Safe == A_TRUE);
	if (needed < 0)
		badarg(Bin);

	uint32_t *p = heap_alloc(&proc->hp, needed);	//TODO: burn fat?
	term_t decoded = ext_term_decode(p,
			needed, enc_data, enc_len,
		   	&proc->hp.proc_bins, &proc->hp.total_pb_size, Safe == A_TRUE);
	
	if (decoded == noval)
		badarg(Bin);
	heap_set_top(&proc->hp, p+needed);

	return decoded;
}

term_t cbif_term_to_binary0_3(proc_t *proc, term_t *regs)
{
	term_t Term = regs[0];
	term_t Comp = regs[1];
	term_t Ver = regs[2];
	if (!is_int(Comp) || int_value(Comp) < 0 || int_value(Comp) > 9)
		badarg(Comp);
	if (!is_int(Ver) || int_value(Ver) < 0 || int_value(Ver) > 1)
		badarg(Ver);

	UNUSED int comp_level = int_value(Comp);
	int minor_ver = int_value(Ver);

	//if (comp_level > 0)
	//	printk("term_to_binary() compressed option ignored\n");

	int size = ext_term_encode_size(Term, minor_ver);
	if (size == -TOO_DEEP)
		fail(A_SYSTEM_LIMIT);
	assert(size > 0);
	if (size > MAX_BYTE_SIZE)
		fail(A_SYSTEM_LIMIT);
	uint8_t *data;
	term_t bin = heap_make_bin(&proc->hp, size, &data);
	ext_term_encode(Term, data, size, minor_ver);
	return bin;
}

term_t cbif_external_size0_2(proc_t *proc, term_t *regs)
{
	term_t Term = regs[0];
	term_t Ver = regs[1];
	if (!is_int(Ver) || int_value(Ver) < 0 || int_value(Ver) > 1)
		badarg(Ver);

	int minor_ver = int_value(Ver);

	int size = ext_term_encode_size(Term, minor_ver);
	if (size < -TOO_DEEP)
		fail(A_SYSTEM_LIMIT);
	assert(size > 0);
	return int_to_term(size, &proc->hp);
}

term_t cbif_tuple_to_list1(proc_t *proc, term_t *regs)
{
	term_t Tuple = regs[0];
	if (!is_tuple(Tuple))
		badarg(Tuple);
	uint32_t *tdata = peel_tuple(Tuple);
	return heap_vector_to_list(&proc->hp, tdata+1, *tdata);
}

term_t cbif_list_to_tuple1(proc_t *proc, term_t *regs)
{
	term_t List = regs[0];
	if (!is_list(List))
		badarg(List);

	if (List == nil)
		return ZERO_TUPLE;

	int arity = list_len(List);
	if (arity < 0)
		badarg(List);

	//TODO: unbounded alloc, consider burn_fat() instead
	uint32_t *htop = heap_alloc(&proc->hp, arity+1);
	heap_set_top(&proc->hp, htop+arity+1);

	*htop = arity;
	heap_list_to_vector(List, htop+1);

	return tag_tuple(htop);
}

term_t cbif_iolist_to_binary1(proc_t *proc, term_t *regs)
{
	term_t IoList = regs[0];
	if (!is_list(IoList) &&
			(!is_boxed(IoList) || !is_binary(peel_boxed(IoList))))
		badarg(IoList);

	int64_t size = iolist_size(IoList);
	if (size == -TOO_DEEP)
	{
		printk("\nTODO\n");
		printk("TODO: make iolist_to_binary() non-recursive\n");
		printk("TODO\n\n");
		printk("*** iolist_to_binary() fails b/c iolist is too deep\n\n");
		fail(A_SYSTEM_LIMIT);
	}
	if (size > MAX_BYTE_SIZE)
		fail(A_SYSTEM_LIMIT);
	if (size < 0)
		badarg(IoList);

	uint8_t *ptr;
	term_t bin = heap_make_bin(&proc->hp, size, &ptr);

	UNUSED uint8_t *check_ptr = iolist_flatten(IoList, ptr); // never fails
	assert(check_ptr == ptr+size);

	return bin;
}

term_t cbif_iolist_size1(proc_t *proc, term_t *regs)
{
	term_t IoList = regs[0];
	if (!is_list(IoList) &&
			(!is_boxed(IoList) || !is_binary(peel_boxed(IoList))))
		badarg(IoList);

	int64_t size = iolist_size(IoList);
	if (size == -TOO_DEEP)
		fail(A_SYSTEM_LIMIT);
	if (size < 0)
		badarg(IoList);

	return int_to_term(size, &proc->hp);
}

term_t cbif_split_binary2(proc_t *proc, term_t *regs)
{
	term_t Bin = regs[0];
	term_t Pos = regs[1];
	if (!is_boxed_binary(Bin))
		badarg(Bin);
	if (!is_int(Pos))
		badarg(Pos);	//TODO: bignum fitting 32 bits should work
	int nu = int_value(Pos);
	if (nu < 0)
		badarg(Pos);
	uint32_t bcount = nu*8;

	uint32_t *pb = peel_boxed(Bin);
	bits_t bs;
	bits_get_real(pb, &bs);
	if (bs.ends - bs.starts < bcount)
		badarg(Pos);

	term_t parent = bin_parent(Bin);
	int needed = WSIZE(t_sub_bin_t) + WSIZE(t_sub_bin_t) +3; //+3 for the tuple
	uint32_t *p = heap_alloc(&proc->hp, needed);
	t_sub_bin_t *sb1 = (t_sub_bin_t *)p;
	box_sub_bin(p, parent, bs.starts, bs.starts+bcount, 0);
	t_sub_bin_t *sb2= (t_sub_bin_t *)p;
	box_sub_bin(p, parent, bs.starts+bcount, bs.ends, 0);
	term_t t = tag_tuple(p);
	*p++ = 2;
	*p++ = tag_boxed(sb1);
	*p++ = tag_boxed(sb2);
	heap_set_top(&proc->hp, p);

	return t;
}	

term_t gc_bif_binary_part2(term_t Bin, term_t PosLen, proc_t *proc, term_t *regs, int live)
{
	if (!is_boxed(Bin) || !is_binary(peel_boxed(Bin)))
		badarg(Bin);
	if (!is_tuple(PosLen))
		badarg(PosLen);
	uint32_t *tdata = peel_tuple(PosLen);
	if (*tdata != 2)
		badarg(PosLen);
	if (!is_int(tdata[1]) || !is_int(tdata[2]))
		badarg(PosLen);
	int start = int_value(tdata[1]);
	int len = int_value(tdata[2]);
	if (len < 0)
	{
		start += len;
		len = -len;
	}
	if (start < 0)
		badarg();

	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);
	if (bs.starts + start *8 + len *8 > bs.ends)
		badarg();

	bs.starts += start *8;
	bs.ends = bs.starts + len *8;

	int needed = WSIZE(t_sub_bin_t);
	uint32_t *p = heap_alloc(&proc->hp, needed);
	t_sub_bin_t *sb = (t_sub_bin_t *)p;
	box_sub_bin(p, bin_parent(Bin), bs.starts, bs.ends, 0);
	heap_set_top(&proc->hp, p);

	return tag_boxed(sb);
}

term_t gc_bif_binary_part3(term_t Bin, term_t Pos, term_t Len, proc_t *proc, term_t *regs, int live)
{
	if (!is_boxed(Bin) || !is_binary(peel_boxed(Bin)))
		badarg(Bin);
	if (!is_int(Pos) || !is_int(Len))
		badarg(PosLen);
	int start = int_value(Pos);
	int len = int_value(Len);
	if (len < 0)
	{
		start += len;
		len = -len;
	}
	if (start < 0)
		badarg();

	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);
	if (bs.starts + start *8 + len *8 > bs.ends)
		badarg();

	bs.starts += start *8;
	bs.ends = bs.starts + len *8;

	int needed = WSIZE(t_sub_bin_t);
	uint32_t *p = heap_alloc(&proc->hp, needed);
	t_sub_bin_t *sb = (t_sub_bin_t *)p;
	box_sub_bin(p, bin_parent(Bin), bs.starts, bs.ends, 0);
	heap_set_top(&proc->hp, p);

	return tag_boxed(sb);
}

term_t cbif_plusplus2(proc_t *proc, term_t *regs)
{
	//TODO: should this be an instruction?

	term_t As = regs[0];
	term_t Bs = regs[1];

	if (!is_list(As))
		badarg(As);
	//if (!is_list(Bs))
	//	badarg(Bs);

	if (is_nil(As))
		return Bs;
	if (is_nil(Bs))
		return As;
	
	int len = list_len(As);
	if (len < 0)
		badarg(As);	// the first list can not be odd

	//TODO: unbounded alloc, use proc_burn_fat()
	//
	uint32_t *htop = heap_alloc(&proc->hp, len*2);
	term_t result = tag_cons(htop);

	uint32_t *term_data = peel_cons(As);
	do {
		htop[0] = term_data[0];
		htop[1] = term_data[1];

		term_t *tail_ref = &htop[1];
		htop += 2;

		if (is_nil(*tail_ref))
		{
			*tail_ref = Bs;	// cons the second list
			break;
		}

		assert(is_cons(*tail_ref));
		term_data = peel_cons(*tail_ref);
		*tail_ref = tag_cons(htop);

	} while (1);

	heap_set_top(&proc->hp, htop);
	return result;
}

term_t cbif_minusminus2(proc_t *proc, term_t *regs)
{
	term_t As = regs[0];
	term_t Bs = regs[1];
	if (!is_list(As))
		badarg(As);
	if (!is_list(Bs))
		badarg(Bs);

	if (is_nil(Bs))
	{
		if (list_len(As) < 0)
			badarg(As);
		return As;
	}

	int n = list_len(Bs);
	if (n < 0)
		badarg(Bs);

	if (is_nil(As))
		return nil;

	// The length of the second list is expected to be small
	//
	term_t bums[n];
	heap_list_to_vector(Bs, bums);
	qsort(bums, n, sizeof(term_t), term_compare);

	// Copy the first list to the resultant list checking
	// the bums list for each item; upon the match the item
	// is not copied and the bum is dropped
	//
	term_t *acons = peel_cons(As);
	term_t res_r = nil;

	//TODO: semi-unbounded alloc, use proc_burn_fat()
	//
	do {
		int copy = 1;

		term_t *alpha = bums;
		term_t *beta = bums +n;
		if (n > 0 &&
			!is_term_smaller(acons[0], alpha[0]) &&
			!is_term_smaller(beta[-1], acons[0]))
		{
			while (beta > alpha+1)
			{
				term_t *mid = alpha + (beta-alpha)/2;
				if (is_term_smaller(acons[0], mid[0]))
					beta = mid;
				else
					alpha = mid;
			}

			assert(beta == alpha+1);
			if (acons[0] == alpha[0]
					|| are_terms_equal(acons[0], alpha[0], 1))
			{
				memmove(alpha, alpha+1, (bums +n -alpha -1)*sizeof(term_t));
				n--;
				copy = 0;
			}
		}

		if (copy)
			res_r = heap_cons(&proc->hp, acons[0], res_r);

		if (!is_cons(acons[1]))
			break;

		acons = peel_cons(acons[1]);
	} while (1);

	if (!is_nil(acons[1]))
		badarg(As);

	//TODO: rev may be avoided using list prealloc
	return list_rev(res_r, &proc->hp);
}

term_t bif_hd1(term_t List, proc_t *proc)
{
	if (!is_cons(List))
		badarg(List);
	
	return *peel_cons(List);
}

term_t bif_tl1(term_t List, proc_t *proc)
{
	if (!is_cons(List))
		badarg(List);
	
	return (peel_cons(List))[1];
}

term_t bif_element2(term_t N, term_t Tuple, proc_t *proc)
{
	if (!is_int(N))
		badarg(N);
	if (!is_tuple(Tuple))
		badarg(Tuple);

	uint32_t *tdata = peel_tuple(Tuple);
	int pos = int_value(N);
	if (pos <= 0 || pos > *tdata)
		badarg(N);

	return tdata[pos];
}

term_t cbif_setelement3(proc_t *proc, term_t *regs)
{
	term_t Index = regs[0];
	term_t Tuple = regs[1];
	term_t Val = regs[2];

	if (!is_int(Index))
		badarg(Index);
	if (!is_tuple(Tuple))
		badarg(Tuple);

	int pos = int_value(Index);
	if (pos <= 0)
		badarg(Index);

	uint32_t *tdata = peel_tuple(Tuple);
	uint32_t arity = *tdata;
	//TODO: unbounded alloc, consider burn_fat()
	uint32_t *htop = heap_alloc(&proc->hp, arity+1);
	heap_set_top(&proc->hp, htop+arity+1);

	*htop = arity;
	memcpy(htop+1, tdata+1, arity*sizeof(term_t));

	htop[pos] = Val;

	return tag_tuple(htop);
}

term_t cbif_make_tuple2(proc_t *proc, term_t *regs)
{
	term_t N = regs[0];
	term_t InitVal = regs[1];

	if (!is_int(N))
		badarg(N);

	int arity = int_value(N);
	if (arity < 0)
		badarg(N);
	if (arity == 0)
		return ZERO_TUPLE;

	//TODO: consider burn_fat()
	uint32_t *htop = heap_alloc(&proc->hp, arity+1);
	heap_set_top(&proc->hp, htop+arity+1);

	*htop = arity;
	for (int i = 1; i <= arity; i++)
		htop[i] = InitVal;

	return tag_tuple(htop);
}

term_t cbif_make_tuple3(proc_t *proc, term_t *regs)
{
	term_t N = regs[0];
	term_t InitVal = regs[1];
	term_t DefList = regs[2];

	if (!is_int(N))
		badarg(N);
	if (!is_list(DefList))
		badarg(DefList);

	int arity = int_value(N);
	if (arity < 0)
		badarg(N);
	if (arity == 0 && DefList != nil)
		badarg(DefList);
	if (arity == 0)
		return ZERO_TUPLE;

	//TODO: consider burn_fat()
	uint32_t *htop = heap_alloc(&proc->hp, arity+1);
	heap_set_top(&proc->hp, htop+arity+1);

	*htop = arity;
	for (int i = 1; i <= arity; i++)
		htop[i] = InitVal;

	term_t l = DefList;
	while (is_cons(l))
	{
		term_t *cons = peel_cons(l);
		if (!is_tuple(cons[0]))
			badarg(DefList);
		uint32_t *tdata = peel_tuple(cons[0]);
		if (*tdata != 2 || !is_int(tdata[1]))
			badarg(DefList);
		int pos = int_value(tdata[1]);
		if (pos <= 0 || pos > arity)
			badarg(DefList);
		htop[pos] = tdata[2];

		l = cons[1];
	}

	if (!is_nil(l))
		badarg(DefList);

	return tag_tuple(htop);
}

term_t cbif_append_element2(proc_t *proc, term_t *regs)
{
	term_t Tuple = regs[0];
	term_t Elem = regs[1];

	if (!is_tuple(Tuple))
		badarg(Tuple);
	uint32_t *tdata = peel_tuple(Tuple);
	uint32_t arity = *tdata;

	//TODO: consider burn_fat()
	uint32_t *htop = heap_alloc(&proc->hp, arity+1+1);
	heap_set_top(&proc->hp, htop+arity+1+1);

	*htop = arity+1;
	memcpy(htop+1, tdata+1, arity*sizeof(term_t));
	htop[arity+1] = Elem;

	return tag_tuple(htop);
}

term_t cbif_make_ref0(proc_t *proc, term_t *regs)
{
	return heap_make_ref(&proc->hp);
}

term_t bif_node1(term_t Thing, proc_t *proc)
{
	if (is_short_pid(Thing) || is_short_oid(Thing))
		return ANONODE_NOHOST__;

	if (is_boxed(Thing))
	{
		uint32_t tag = boxed_tag(peel_boxed(Thing));
		if (tag == SUBTAG_PID || tag == SUBTAG_OID || tag == SUBTAG_REF)
			return ANONODE_NOHOST__;
	}

	badarg(Thing);
}

term_t cbif_sin1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, sin(d));
}

term_t cbif_cos1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, cos(d));
}

term_t cbif_tan1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, tan(d));
}

term_t cbif_asin1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, asin(d));
}

term_t cbif_acos1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, acos(d));
}

term_t cbif_atan1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, atan(d));
}

term_t cbif_atan2_2(proc_t *proc, term_t *regs)
{
	term_t X = regs[0];
	term_t Y = regs[1];
	double d1 = term_to_float(X);
	double d2 = term_to_float(Y);
	if (!isfinite(d1))
		badarg(X);
	if (!isfinite(d2))
		badarg(Y);
	return heap_float(&proc->hp, atan2(d1, d2));
}

term_t cbif_sinh1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, sinh(d));
}

term_t cbif_cosh1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, cosh(d));
}

term_t cbif_tanh1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, tanh(d));
}

term_t cbif_asinh1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_acosh1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_atanh1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_exp1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, exp(d));
}

term_t cbif_log1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, log(d));
}

term_t cbif_log10_1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, log10(d));
}

term_t cbif_pow2(proc_t *proc, term_t *regs)
{
	term_t X = regs[0];
	term_t Y = regs[1];
	double d1 = term_to_float(X);
	double d2 = term_to_float(Y);
	if (!isfinite(d1))
		badarg(X);
	if (!isfinite(d2))
		badarg(Y);
	return heap_float(&proc->hp, pow(d1, d2));
}

term_t cbif_sqrt1(proc_t *proc, term_t *regs)
{
	term_t V = regs[0];
	double d = term_to_float(V);
	if (!isfinite(d))
		badarg(V);
	return heap_float(&proc->hp, sqrt(d));
}

term_t cbif_erf1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_erfc1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

//EOF
