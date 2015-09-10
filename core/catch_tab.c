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

#include "catch_tab.h"

#include "ling_common.h"

#include <string.h>

// 1024 is not enough or ejabberd?
#define CATCH_REFS_SIZE	4*1024

typedef struct catch_ref_t catch_ref_t;
struct catch_ref_t {
	uint32_t *code;
	uint32_t ord;
};

#include "catch_tab.inc"
static int nr_catch_refs = INIT_NR_CATCH_REFS;
static uint32_t next_ord_base = INIT_NR_CATCH_REFS;

void catches_init(void)
{
	nr_catch_refs = INIT_NR_CATCH_REFS;
	next_ord_base = INIT_NR_CATCH_REFS;
}

uint32_t catches_next_base(void)
{
	return next_ord_base;
}

uint32_t catches_append_block(uint32_t **labels, int nr_labs)
{
	if (nr_catch_refs + nr_labs > CATCH_REFS_SIZE)
		fatal_error("too many catches: %d more requested", nr_labs);

	for (int i = 0; i < nr_labs; i++)
	{
		catch_refs[nr_catch_refs+i].code = labels[i];
		catch_refs[nr_catch_refs+i].ord = next_ord_base+i;
	}

	uint32_t saved_next_ord_base = next_ord_base;

	nr_catch_refs += nr_labs;
	next_ord_base += nr_labs;

	return saved_next_ord_base;
}

void catches_remove_block(uint32_t start, int block_size)
{
	if (block_size == 0)
		return;

	assert(nr_catch_refs > 0);
	catch_ref_t *alpha = catch_refs;
	catch_ref_t *beta = catch_refs + nr_catch_refs;

	assert(start >= alpha->ord);
	assert(start <= beta[-1].ord);

	while (beta > alpha+1)
	{
		catch_ref_t *mid = alpha + (beta-alpha)/2;
		if (mid->ord > start)
			beta = mid;
		else
			alpha = mid;
	}

	assert(beta == alpha+1);
	assert(alpha->ord == start);

	// .ord are sequential within a block
	assert(alpha[block_size -1].ord = alpha->ord +block_size -1);

	int tail_size = (void *)(alpha +nr_catch_refs) - (void *)(alpha +block_size);
	memmove(alpha, alpha +block_size, tail_size);
	nr_catch_refs -= block_size;
	//NB: next_ord_base stays the same
}

uint32_t *catch_jump_to(int ord)
{
	assert(nr_catch_refs > 0);
	catch_ref_t *alpha = catch_refs;
	catch_ref_t *beta = catch_refs + nr_catch_refs;

	assert(ord >= alpha->ord);
	assert(ord <= beta[-1].ord);

	while (beta > alpha+1)
	{
		catch_ref_t *mid = alpha + (beta-alpha)/2;
		if (mid->ord > ord)
			beta = mid;
		else
			alpha = mid;
	}

	assert(beta == alpha+1);
	assert(alpha->ord == ord);
	return alpha->code;
}

void catches_attach_preloaded_code(int start_index,
						int end_index, uint32_t *code)
{
	if (code_base_fixed_already())
		return;

	for (int i = start_index; i < end_index; i++)
	{
		uint32_t offset = shrink_ptr(catch_refs[i].code);
		catch_refs[i].code = code + offset;
	}
}

//EOF
