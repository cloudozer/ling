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

#include "bif_impl.h"

term_t cbif_bin_is_7bit1(proc_t *proc, term_t *regs)
{
	printk("TODO: bin_is_7bit(%pt) called\n", T(regs[0]));
	bif_not_implemented();
}

term_t cbif_characters_to_list2(proc_t *proc, term_t *regs)
{
	term_t Data = regs[0];
	term_t InEnc  = regs[1];

	term_t in_enc = InEnc;
	int is_little = 0;
	if (is_tuple(InEnc))
	{
		uint32_t *p = peel_tuple(InEnc);
		if (*p == 2)
		{
			in_enc = p[1];
			if (p[2] != A_LITTLE && p[2] != A_BIG)
				badarg(InEnc);
			is_little = p[2] == A_LITTLE;
		}
	}
	if (in_enc == A_UNICODE)
		in_enc = A_UTF8;
	if (in_enc != A_LATIN1 && in_enc != A_UTF8 &&
		in_enc != A_UTF16 && in_enc != A_UTF32)
		badarg(InEnc);

	term_t rest = Data;
	int incomplete = 0;
	term_t encoded = chars_to_list(&rest, in_enc, is_little, &incomplete, &proc->hp);

	if (rest == nil)
		return encoded;
	
	term_t tag = (incomplete) ?A_INCOMPLETE :A_ERROR;
	return heap_tuple3(&proc->hp, tag, encoded, rest);
}

term_t cbif_characters_to_binary3(proc_t *proc, term_t *regs)
{
	term_t Data = regs[0];
	term_t InEnc = regs[1];
	term_t OutEnc = regs[2];

	term_t in_enc = InEnc;
	int is_little_in = 0;
	if (is_tuple(InEnc))
	{
		uint32_t *p = peel_tuple(InEnc);
		if (*p == 2)
		{
			in_enc = p[1];
			if (p[2] != A_LITTLE && p[2] != A_BIG)
				badarg(InEnc);
			is_little_in = p[2] == A_LITTLE;
		}
	}
	if (in_enc == A_UNICODE)
		in_enc = A_UTF8;
	if (in_enc != A_LATIN1 && in_enc != A_UTF8 &&
		in_enc != A_UTF16 && in_enc != A_UTF32)
		badarg(InEnc);

	term_t out_enc = OutEnc;
	int is_little_out = 0;
	if (is_tuple(OutEnc))
	{
		uint32_t *p = peel_tuple(OutEnc);
		if (*p == 2)
		{
			out_enc = p[1];
			if (p[2] != A_LITTLE && p[2] != A_BIG)
				badarg(OutEnc);
			is_little_out = p[2] == A_LITTLE;
		}
	}
	if (out_enc == A_UNICODE)
		out_enc = A_UTF8;
	if (out_enc != A_LATIN1 && out_enc != A_UTF8 &&
		out_enc != A_UTF16 && out_enc != A_UTF32)
		badarg(OutEnc);

	term_t rest = Data;
	int incomplete = 0;
	term_t encoded = chars_to_binary(&rest, in_enc, is_little_in,
					   out_enc, is_little_out, &incomplete, &proc->hp);

	if (rest == nil)
		return encoded;
	
	term_t tag = (incomplete) ?A_INCOMPLETE :A_ERROR;
	return heap_tuple3(&proc->hp, tag, encoded, rest);
}

term_t cbif_dflag_unicode_io1(proc_t *proc, term_t *regs)
{
	term_t Pid = regs[0];
	if (!is_short_pid(Pid))
		badarg(Pid);

	//TODO
	return A_TRUE;
}

term_t cbif_printable_range0(proc_t *proc, term_t *regs)
{
	//TODO
	return A_LATIN1;
}

//EOF
