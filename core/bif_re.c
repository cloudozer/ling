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

#include <pcre.h>

#define OVEC_SIZE	255

static heap_t *use_for_pcre_malloc = 0;

static void *pcre_needs_malloc(size_t sz)
{
	assert(use_for_pcre_malloc != 0);
	//debug("PCRE malloc [%d]\n", sz);
	return heap_tmp_buf(use_for_pcre_malloc, sz);
}

static void pcre_needs_free(void *ptr)
{
	assert(use_for_pcre_malloc != 0);
	// Do nothing, let GC take care of this
	//debug("PCRE free %pp\n", ptr);
}

static void *pcre_needs_stack_malloc(size_t sz)
{
	memnode_t *node = nalloc_N(sz);
	//debug("PCRE stack malloc [%d] %pp\n", sz, node);
	if (node == 0)
		return 0;
	return node +1;
}

static void pcre_needs_stack_free(void *ptr)
{
	assert(ptr != 0);
	//debug("PCRE stack free %pp\n", ptr);
	memnode_t *node = (memnode_t *)ptr -1;
	nfree(node);
}

void pcre_init(void)
{
	pcre_malloc = pcre_needs_malloc;
	pcre_free = pcre_needs_free;
	pcre_stack_malloc = pcre_needs_stack_malloc;
	pcre_stack_free = pcre_needs_stack_free;
}

static const char *re_flatten(term_t re, int options, heap_t *hp)
{
	char *str;
	if (options & PCRE_UTF8)
	{
		int incomplete = 0;
		term_t encoded = chars_to_binary(&re,
			A_UTF8, 0, A_UTF8, 0, &incomplete, hp);
		if (re != nil)
			return 0;

		bits_t bs;
		assert(is_boxed(encoded));
		bits_get_real(peel_boxed(encoded), &bs);
		assert(bs.starts == 0);
		int sz = bs.ends /8;
		str = (char *)heap_tmp_buf(hp, sz +1);
		memcpy(str, bs.data, sz);
		str[sz] = 0;
	}
	else
	{
		int sz = iolist_size(re);
		if (sz < 0)
			return 0;
		str = (char *)heap_tmp_buf(hp, sz +1);
		iolist_flatten(re, (uint8_t *)str);
		str[sz] = 0;
	}
	return str;
}

static term_t re_compile(term_t re, int options, heap_t *hp)
{
	const char *pattern = re_flatten(re, options, hp);
	if (pattern == 0)
		return noval;

	int errcode;
	const char *errmsg;
	int erroff;

	use_for_pcre_malloc = hp;	// make PCRE use the process heap
	pcre *context = pcre_compile2(pattern, options, &errcode, &errmsg, &erroff, 0);
	use_for_pcre_malloc = 0;

	if (context != 0)
	{
		// {ok,{re_pattern,Bin}}
		size_t len;
		pcre_fullinfo(context, 0, PCRE_INFO_SIZE, &len);
		int capture_count;
		pcre_fullinfo(context, 0, PCRE_INFO_CAPTURECOUNT, &capture_count);
		uint8_t *to;
		term_t bin = heap_make_bin(hp, len, &to);
		memcpy(to, context, len);
		// NB: freeing the context is noop
		int unicode = ((options & PCRE_UTF8) != 0);
		return heap_tuple2(hp, A_OK,
			heap_tuple4(hp, A_RE_PATTERN, tag_int(capture_count), tag_int(unicode), bin));
	}
	else
	{
		if (errcode == PCRE_ERROR_NOMEMORY)
			no_memory_signal();

		// {error,{ErrMsg,ErrOff}}
		return heap_tuple2(hp, A_ERROR,
				heap_tuple2(hp, heap_strz(hp, errmsg), tag_int(erroff)));
	}
}

term_t cbif_re_compile1(proc_t *proc, term_t *regs)
{
	term_t Re = regs[0];
	if (!is_list(Re) && !is_boxed_binary(Re))
		badarg(Re);

	term_t result = re_compile(Re, 0, &proc->hp);
	if (result == noval)
		badarg(Re);

	return result;
}

term_t cbif_re_compile2(proc_t *proc, term_t *regs)
{
	term_t Re = regs[0];
	term_t Opts = regs[1];

	if (!is_list(Re) && !is_boxed_binary(Re))
		badarg(Re);
	if (!is_list(Opts))
		badarg(Re);

	int options = 0;
	term_t t = Opts;
	while (is_cons(t)) {
		term_t *cons = peel_cons(t);
		term_t opt = cons[0];
		if (opt == A_UNICODE)
			options |= PCRE_UTF8;
		else if (opt == A_ANCHORED)
			options |= PCRE_ANCHORED;
		else if (opt == A_CASELESS)
			options |= PCRE_CASELESS;
		else if (opt == A_DOLLAR_ENDONLY)
			options |= PCRE_DOLLAR_ENDONLY;
		else if (opt == A_DOTALL)
			options |= PCRE_DOTALL;
		else if (opt == A_EXTENDED)
			options |= PCRE_EXTENDED;
		else if (opt == A_FIRSTLINE)
			options |= PCRE_FIRSTLINE;
		else if (opt == A_MULTILINE)
			options |= PCRE_MULTILINE;
		else if (opt == A_NO_AUTO_CAPTURE)
			options |= PCRE_NO_AUTO_CAPTURE;
		else if (opt == A_DUPNAMES)
			options |= PCRE_DUPNAMES;
		else if (opt == A_UNGREEDY)
			options |= PCRE_UNGREEDY;
		else if (opt == A_BSR_ANYCRLF)
			options |= PCRE_BSR_ANYCRLF;
		else if (opt == A_BSR_UNICODE)
			options |= PCRE_BSR_UNICODE;
		else if (is_tuple(opt))
		{
			uint32_t *p = peel_tuple(opt);
			if (p[0] == 2 && p[1] == A_NEWLINE)
			{
				if (p[2] == A_CR)
					options |= PCRE_NEWLINE_CR;
				else if (p[2] == A_LF)
					options |= PCRE_NEWLINE_LF;
				else if (p[2] == A_CRLF)
					options |= PCRE_NEWLINE_CRLF;
				else if (p[2] == A_ANYCRLF)
					options |= PCRE_NEWLINE_ANYCRLF;
				else if (p[2] == A_ANY)
					options |= PCRE_NEWLINE_ANY;
				else
					badarg(Opts);
			}
			else
				badarg(Opts);
		}
		else
			badarg(Opts);

		t = cons[1];
	}

	term_t result = re_compile(Re, options, &proc->hp);
	if (result == noval)
		badarg(Re);

	return result;
}

static term_t captured_item(int index, const char *subject,
	   int *ovec, int num_match, term_t type, int unicode, heap_t *hp)
{
	if (type == A_INDEX)
	{
		if (index < 0 || index >= num_match)
			return heap_tuple2(hp, tag_int(-1), tag_int(0));

		int from = ovec[2*index];
		int to = ovec[2*index +1];
		return heap_tuple2(hp, tag_int(from), tag_int(to -from));
	}
	else if (type == A_LIST)
	{
		const char *str;
		use_for_pcre_malloc = hp;
		int rc = pcre_get_substring(subject, ovec, num_match, index, &str);
		use_for_pcre_malloc = 0;
		if (rc == PCRE_ERROR_NOMEMORY)
			no_memory_signal();
		if (rc == PCRE_ERROR_NOSUBSTRING)
			return nil;
		assert(rc >= 0);
		if (unicode)
		{
			uint8_t *to;
			term_t bin = heap_make_bin(hp, rc, &to);
			memcpy(to, str, rc);

			term_t rest = bin;
			int incomplete = 0;
			term_t encoded = chars_to_list(&rest, A_UTF8, 0, &incomplete, hp);

			if (rest == nil)
				return encoded;
	
			term_t tag = (incomplete) ?A_INCOMPLETE :A_ERROR;
			return heap_tuple3(hp, tag, encoded, rest);
		}
		else
			return heap_str(hp, str, rc);
	}
	else
	{
		assert(type == A_BINARY);
		const char *str;
		use_for_pcre_malloc = hp;
		int rc = pcre_get_substring(subject, ovec, num_match, index, &str);
		use_for_pcre_malloc = 0;
		if (rc == PCRE_ERROR_NOMEMORY)
			no_memory_signal();
		if (rc == PCRE_ERROR_NOSUBSTRING)
		{
			// <<>>
			uint8_t *dummy_ptr;
			return heap_make_bin(hp, 0, &dummy_ptr);
		}
		assert(rc >= 0);
		uint8_t *to;
		term_t bin = heap_make_bin(hp, rc, &to);
		memcpy(to, str, rc);
		return bin;
	}
}

static term_t captured_list(pcre *context, const char *subject,
	int *ovec, int num_match, term_t spec, term_t type, int unicode, heap_t *hp)
{
	term_t ms = nil;
	if (spec == A_ALL || spec == A_ALL_BUT_FIRST)
	{
		int start_index = (spec == A_ALL) ?0 :1;
		for (int i = start_index; i < num_match; i++)
		{
			term_t el = captured_item(i, subject, ovec, num_match, type, unicode, hp);
			ms = heap_cons(hp, el, ms);
		}
		ms = list_rev(ms, hp);
	}
	else if (spec == A_FIRST)
		ms = heap_cons(hp, captured_item(0, subject, ovec, num_match, type, unicode, hp), nil);
	else if (is_list(spec))
	{
		term_t t = spec;
		while (is_cons(t))
		{
			term_t *cons = peel_cons(t);

			int index;
			if (is_int(cons[0]))
				index = int_value(cons[0]);
			else if (is_atom(cons[0]))
			{
				uint8_t *name = atoms_get(atom_index(cons[0]));
				assert(name != 0);
				// convert to strz
				int len = name[0];
				char buf[len +1];
				memcpy(buf, name +1, len);
				buf[len] = 0;
				index = pcre_get_stringnumber(context, buf);
			}
			else if (is_cons(cons[0]))
			{
				int len = byte_list_size(cons[0]);
				if (len < 0)
					return noval;
				char buf[len +1];
				byte_list_flatten(cons[0], (uint8_t *)buf);
				buf[len] = 0;
				index = pcre_get_stringnumber(context, buf);
			}
			else	// including nil
				return noval;

			term_t el = captured_item(index, subject, ovec, num_match, type, unicode, hp);
			ms = heap_cons(hp, el, ms);
			t = cons[1];
		}
		if (t != nil)
			return noval;
		ms = list_rev(ms, hp);
	}
	else
		return noval;

	return ms;
}

static term_t re_exec(term_t Subj, term_t Re, int compile_opts, int exec_opts,
				int global, term_t spec, term_t type, int exec_offset, heap_t *hp)
{
	// compile the regex
	pcre *context;

	if (is_tuple(Re))
	{
		if (compile_opts != 0)
			return noval;

		uint32_t *p = peel_tuple(Re);
		if (p[0] != 4 || p[1] != A_RE_PATTERN || !is_boxed_binary(p[4]))
			return noval;
		bits_t bs;
		bits_get_real(peel_boxed(p[4]), &bs);
		if (((bs.ends -bs.starts) & 7) != 0)
			return noval;
		assert((bs.starts & 7) == 0);	// Do we really care about the unaligned case?
		context = (pcre *)(bs.data +bs.starts /8);
		// Restore the compile options
		unsigned long old_compile_opts;
		UNUSED int rs = pcre_fullinfo(context, 0, PCRE_INFO_OPTIONS, &old_compile_opts);
		assert(rs == 0);
		compile_opts = (int)old_compile_opts;
	}
	else
	{
		const char *pattern = re_flatten(Re, compile_opts, hp);
		if (pattern == 0)
			return noval;

		int errcode;
		const char *errmsg;
		int erroff;

		use_for_pcre_malloc = hp;	// make PCRE use the process heap
		context = pcre_compile2(pattern, compile_opts, &errcode, &errmsg, &erroff, 0);
		use_for_pcre_malloc = 0;

		if (context == 0)
			return noval;		//NB: no memory is masked
	}

	int unicode = ((compile_opts & PCRE_UTF8) != 0);

	//NB: do not use re_flatten() to avoid extra copying
	
	char *subject;
	int subj_len;

	if (unicode)
	{
		int incomplete = 0;
		term_t encoded = chars_to_binary(&Subj,
				A_UTF8, 0, A_UTF8, 0, &incomplete, hp);
		if (Subj != nil)
			return noval;

		bits_t bs;
		assert(is_boxed_binary(encoded));
		bits_get_real(peel_boxed(encoded), &bs);
		assert(bs.starts == 0);
		subject = (char *)bs.data;
		subj_len = bs.ends /8;
	}
	else
	{
		subj_len = iolist_size(Subj);
		if (subj_len < 0)
			return noval;
		subject = (char *)heap_tmp_buf(hp, subj_len);
		iolist_flatten(Subj, (uint8_t *)subject);
	}

	if (!global)
	{
		int ovec[OVEC_SIZE];

		use_for_pcre_malloc = hp;
		int ret_code = pcre_exec(context,
				0, subject, subj_len, exec_offset, exec_opts, ovec, OVEC_SIZE);
		use_for_pcre_malloc = 0;	

		if (ret_code == PCRE_ERROR_NOMATCH || ret_code == PCRE_ERROR_MATCHLIMIT)
			return A_NOMATCH;
		if (ret_code == PCRE_ERROR_NOMEMORY)
				no_memory_signal();
		if (ret_code < 0)
			return noval;

		// We have a match
	
		if (spec == A_NONE)
			return A_MATCH;

		term_t cl = captured_list(context, subject, ovec, ret_code, spec, type, unicode, hp);
		if (cl == noval)
			return noval;

		return heap_tuple2(hp, A_MATCH, cl);
	}
	else
	{
		term_t gs = nil;
		do {
			int ovec[OVEC_SIZE];
			use_for_pcre_malloc = hp;
			int ret_code = pcre_exec(context,
					0, subject, subj_len, exec_offset, exec_opts, ovec, OVEC_SIZE);
			use_for_pcre_malloc = 0;	

			if (ret_code == PCRE_ERROR_NOMATCH || ret_code == PCRE_ERROR_MATCHLIMIT)
				break;
			if (ret_code == PCRE_ERROR_NOMEMORY)
				no_memory_signal();
			if (ret_code < 0)
				return noval;

			// We have another match
			assert(ret_code > 0);

			if (spec == A_NONE)
				return A_MATCH;

			term_t cl = captured_list(context, subject, ovec, ret_code, spec, type, unicode, hp);
			if (cl == noval)
				return noval;
			gs = heap_cons(hp, cl, gs);

			if (ovec[1] > ovec[0])
			{
				exec_offset = ovec[1];
				continue;
			}

			// A zero-length match - retry with 'anchored' and 'notempty' options
			assert(ovec[1] == ovec[1]);

			int saved_offset = ovec[0];
			use_for_pcre_malloc = hp;
			int rc = pcre_exec(context, 0, subject, subj_len, exec_offset,
					exec_opts | PCRE_ANCHORED | PCRE_NOTEMPTY, ovec, OVEC_SIZE);
			use_for_pcre_malloc = 0;	

			if (rc == PCRE_ERROR_NOMEMORY)
				no_memory_signal();
			if (rc == PCRE_ERROR_NOMATCH)
			{
				// No non-empty match at this offset
				
				if (unicode && exec_offset < subj_len)
				{
					uint8_t pt = subject[exec_offset];
					assert((pt & 0x80) == 0 || (pt & 0xc0) == 0xc0);
					if (pt < 0x80)
						exec_offset = saved_offset +1;
					else if (pt < 0xe0)
						exec_offset = saved_offset +2;
					else if (pt < 0xf0)
						exec_offset = saved_offset +3;
					else if (pt < 0xf8)
						exec_offset = saved_offset +4;
					else if (pt < 0xfc)
						exec_offset = saved_offset +5;	// never happnes
					else
						exec_offset = saved_offset +6;	// never happens
				}
				else
					exec_offset = saved_offset +1;
			}
			else
			{
				assert(rc > 0);

				term_t cl = captured_list(context, subject, ovec, rc, spec, type, unicode, hp);
				if (cl == noval)
					return noval;
				gs = heap_cons(hp, cl, gs);

				exec_offset = ovec[1];
			}
		} while (exec_offset <= subj_len);

		if (gs == nil)
			return A_NOMATCH;

		return heap_tuple2(hp, A_MATCH, list_rev(gs, hp));
	}
}

term_t cbif_re_run2(proc_t *proc, term_t *regs)
{
	term_t Subj = regs[0];
	term_t Re = regs[1];

	if (!is_list(Subj) && !is_boxed_binary(Subj))
		badarg(Subj);

	term_t result = re_exec(Subj, Re, 0, 0, 0, A_ALL, A_INDEX, 0, &proc->hp);
	if (result == noval)
		badarg(Re);

	return result;
}

term_t cbif_re_run3(proc_t *proc, term_t *regs)
{
	term_t Subj = regs[0];
	term_t Re = regs[1];
	term_t Opts = regs[2];

	if (!is_list(Subj) && !is_boxed_binary(Subj))
		badarg(Subj);
	if (!is_list(Opts))
		badarg(Opts);

	int compile_opts = 0;
	int exec_opts = 0;

	int exec_offset = 0;
	int global = 0;

	term_t capture_value_spec = A_ALL;
	term_t capture_type = A_INDEX;

	term_t t = Opts;
	while (is_cons(t))
	{
		term_t *cons = peel_cons(t);
		term_t opt = cons[0];
		if (opt == A_ANCHORED)
			exec_opts |= PCRE_ANCHORED;
		else if (opt == A_NOTBOL)
			exec_opts |= PCRE_NOTBOL;
		else if (opt == A_NOTEOL)
			exec_opts |= PCRE_NOTEOL;
		else if (opt == A_NOTEMPTY)
			exec_opts |= PCRE_NOTEMPTY;
		else if (opt == A_BSR_ANYCRLF)
			exec_opts |= PCRE_BSR_ANYCRLF;
		else if (opt == A_BSR_UNICODE)
			exec_opts |= PCRE_BSR_UNICODE;
		else if (opt == A_UNICODE)
			compile_opts |= PCRE_UTF8;
		else if (opt == A_CASELESS)
			compile_opts |= PCRE_CASELESS;
		else if (opt == A_DOLLAR_ENDONLY)
			compile_opts |= PCRE_DOLLAR_ENDONLY;
		else if (opt == A_DOTALL)
			compile_opts |= PCRE_DOTALL;
		else if (opt == A_EXTENDED)
			compile_opts |= PCRE_EXTENDED;
		else if (opt == A_FIRSTLINE)
			compile_opts |= PCRE_FIRSTLINE;
		else if (opt == A_MULTILINE)
			compile_opts |= PCRE_MULTILINE;
		else if (opt == A_NO_AUTO_CAPTURE)
			compile_opts |= PCRE_NO_AUTO_CAPTURE;
		else if (opt == A_DUPNAMES)
			compile_opts |= PCRE_DUPNAMES;
		else if (opt == A_UNGREEDY)
			compile_opts |= PCRE_UNGREEDY;
		else if (opt == A_GLOBAL)
			global = 1;
		else if (is_tuple(opt))
		{
			uint32_t *p = peel_tuple(opt);
			if (p[0] == 2 && p[1] == A_NEWLINE)
			{
				if (p[2] == A_CR)
					exec_opts |= PCRE_NEWLINE_CR;
				else if (p[2] == A_LF)
					exec_opts |= PCRE_NEWLINE_LF;
				else if (p[2] == A_CRLF)
					exec_opts |= PCRE_NEWLINE_CRLF;
				else if (p[2] == A_ANYCRLF)
					exec_opts |= PCRE_NEWLINE_ANYCRLF;
				else if (p[2] == A_ANY)
					exec_opts |= PCRE_NEWLINE_ANY;
				else
					badarg(Opts);
			}
			else if (p[0] == 2 && p[1] == A_CAPTURE)
			{
				if (!is_list(p[2]) && p[2] != A_ALL && p[2] != A_FIRST &&
						p[2] != A_ALL_BUT_FIRST && p[2] != A_NONE)
					badarg(Opts);
				capture_value_spec = p[2];
			}
			else if (p[0] == 3 && p[1] == A_CAPTURE)
			{
				if ((!is_list(p[2]) && p[2] != A_ALL && p[2] != A_FIRST &&
								p[2] != A_ALL_BUT_FIRST && p[2] != A_NONE) ||
				   		(p[3] != A_INDEX && p[3] != A_LIST && p[3] != A_BINARY))
					badarg(Opts);
				capture_value_spec = p[2];
				capture_type = p[3];
			}
			else if (p[0] == 2 && p[1] == A_OFFSET)
			{
				if (!is_int(p[2]))
					badarg(Opts);
				exec_offset = int_value(p[2]);
				if (exec_offset < 0)
					badarg(Opts);
			}
			else
				badarg(Opts);
		}
		else
			badarg(Opts);

		t = cons[1];
	}

	if (t != nil)
		badarg(Opts);

	term_t result = re_exec(Subj, Re, compile_opts, exec_opts,
			global, capture_value_spec, capture_type, exec_offset, &proc->hp);
	if (result == noval)
		badarg(Re);

	return result;
}

