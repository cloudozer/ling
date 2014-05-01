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

/**
 *
 *
 *
 */

#include "ling_common.h"

#include "heap.h"
#include "stringify.h"
#include "atoms.h"
#include "bignum.h"
#include "string.h"
#include "bits.h"

void stringify_test(void)
{
	uint32_t hspace[1024];
	heap_t hp;
	heap_init(&hp, hspace, (void *)hspace +sizeof(hspace));

	char str[1024];

	//small integer
	term_t zero = tag_int(0);
	term_t ten = tag_int(10);
	term_t neg = tag_int(-12345);
	term_t max_int = tag_int(MAX_INT_VALUE);
	term_t min_int = tag_int(MIN_INT_VALUE);
	
	term_to_str(zero, str, sizeof(str));
	printk("int(0) [%s]\n", str);
	term_to_str(ten, str, sizeof(str));
	printk("int(10) [%s]\n", str);
	term_to_str(neg, str, sizeof(str));
	printk("int(-12345) [%s]\n", str);
	term_to_str(max_int, str, sizeof(str));
	printk("int(MAX_INT_VALUE) [%s]\n", str);
	term_to_str(min_int, str, sizeof(str));
	printk("int(MIN_INT_VALUE) [%s]\n", str);
	
	//bignum
	term_t last_bn = tag_boxed(bignum_from_int(&hp, MIN_INT_VALUE-1));
	term_t first_bn = tag_boxed(bignum_from_int(&hp, MAX_INT_VALUE+1));
	term_t bn1 = tag_boxed(bignum_from_int(&hp, 0x7fffffff));

	term_to_str(last_bn, str, sizeof(str));
	printk("bignum(MIN_INT_VALUE-1) [%s]\n", str);	
	term_to_str(first_bn, str, sizeof(str));
	printk("bignum(MAX_INT_VALUE+1) [%s]\n",  str);
	term_to_str(bn1, str, sizeof(str));
	printk("bignum(0x7fffffff) [%s]\n", str);
	
	//atoms
	term_t a1 = tag_atom(1);
	term_t a2 = tag_atom(atoms_set((unsigned char *)"\05ab\0cd"));
	term_t a3 = tag_atom(atoms_set((unsigned char *)"\0"));
	term_t a4 = tag_atom(7);
	
	term_to_str(a1, str, sizeof(str));
	printk("atom(1) [%s]\n", str);
	term_to_str(a2, str, sizeof(str));
	printk("atom(nul) [%s]\n", str);
	term_to_str(a3, str, sizeof(str));
	printk("atom('') [%s]\n", str);
	term_to_str(a4, str, sizeof(str));
	printk("atom(7) [%s]\n", str);

	//short pid
	term_t pid1 = tag_short_pid(123);	
	term_to_str(pid1, str, sizeof(str));
	printk("pid(123) [%s]\n", str);

	//short oid
	term_t oid1 = tag_short_oid(456);	
	term_to_str(oid1, str, sizeof(str));
	printk("oid(456) [%s]\n", str);

	//float
	uint32_t *p = heap_alloc(&hp, 3+3+3);

	term_t dbl1 = tag_boxed(p);
	box_float(p, 3.14);
	term_t dbl2 = tag_boxed(p);
	box_float(p, 1.25e8);
	term_t dbl3 = tag_boxed(p);
	box_float(p, -73.1234);

	heap_set_top(&hp, p);

	term_to_str(dbl1, str, sizeof(str));
	printk("float(3.14) [%s]\n", str);
	term_to_str(dbl2, str, sizeof(str));
	printk("float(1.25e8) [%s]\n", str);
	term_to_str(dbl3, str, sizeof(str));
	printk("float(-73.1234) [%s]\n", str);
	
	//binary

// NB: manual size values below no longer valid
//
//	byte_t data[] = {55,34,21,13,8,2,1,1};
//	uint8_t hey[] = "Hey there, cowboy";
//
//	p = heap_alloc(&hp, 5+ 4+ 2+ (sizeof(hey)-1+3)/4);
//
//	binnode_t *bnode = binnode_make(sizeof(data));
//	memcpy(bnode->starts, data, sizeof(data));
//
//	term_t bin1 = tag_boxed(p);
//	box_proc_bin(p, sizeof(data), bnode);
//
//	term_t bin2 = tag_boxed(p);
//
//	t_proc_bin_t *pb = (t_proc_bin_t *)p;
//	box_sub_bin(p, bin1, 13, 34, 0);
//	proc_bin_link(&hp.proc_bins, pb, &hp.total_pb_size);
//
//	term_t bin3 = tag_boxed(p);
//	box_heap_bin(p, sizeof(hey)-1, hey);
//
//	heap_set_top(&hp, p);
//
//	term_to_str(bin1, str, sizeof(str));
//	printk("binary(pb) [%s]\n", str);
//	term_to_str(bin2, str, sizeof(str));
//	printk("binary(odd) [%s]\n", str);
//	term_to_str(bin3, str, sizeof(str));
//	printk("binary(hey) [%s]\n", str);
	
	//lists
	p = heap_alloc(&hp, 2+2+2+2+2);

	term_t c1 = tag_cons(p);
	*p++ = tag_int(5);
	*p++ = nil;
	term_t c2 = tag_cons(p);
	*p++ = tag_int(4);
	*p++ = c1;
	term_t c3 = tag_cons(p);
	*p++ = tag_int(3);
	*p++ = c2;
	term_t c4 = tag_cons(p);
	*p++ = tag_int(2);
	*p++ = c3;
	term_t c5 = tag_cons(p);
	*p++ = tag_int(1);
	*p++ = c4;

	heap_set_top(&hp, p);

	term_to_str(c5, str, sizeof(str));
	printk("list(ord5) [%s]\n", str);
	term_to_str(nil, str, sizeof(str));
	printk("list(empty) [%s]\n", str);
	
	//tuple
	p = heap_alloc(&hp, 1+3);

	term_t tup1 = ZERO_TUPLE;
	term_t tup2 = tag_tuple(p);
	*p++ = 3;
	*p++ = tag_int(1);
	*p++ = tag_int(2);
	*p++ = tag_int(3);
	
	heap_set_top(&hp, p);

	term_to_str(tup1, str, sizeof(str));
	printk("tuple(empty) [%s]\n", str);
	term_to_str(tup2, str, sizeof(str));
	printk("tuple(3) [%s]\n", str);

	heap_done(&hp);
}

/*EOF*/

