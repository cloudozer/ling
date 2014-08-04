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

#include <stdint.h>

#include "term.h"
#include "heap.h"
#include "atoms.h"
#include "atom_defs.h"
#include "embed.h"

static embed_buck_t *find_bucket(term_t bucket);

int embed_init(void)
{
	// atoms must be initialised by now

	// bucket_raw -> bucket
	// name_raw -> name

	embed_buck_t *ptr_b = embed_bucks;
	embed_buck_t *end_b = ptr_b +nr_embed_bucks;
	while (ptr_b < end_b)
	{
		ptr_b->bucket = tag_atom(atoms_set(ptr_b->bucket_raw));
		ptr_b++;
	}

	embed_bin_t *ptr_i = embed_bins;
	embed_bin_t *end_i = embed_bins +nr_embed_bins;
	while (ptr_i < end_i)
	{
		ptr_i->name = tag_atom(atoms_set(ptr_i->name_raw));
		ptr_i++;
	}

	return 0;
}

term_t embed_all_buckets(heap_t *hp)
{
	term_t buckets = nil;
	embed_buck_t *ptr = embed_bucks;
	embed_buck_t *end = ptr +nr_embed_bucks;
	while (ptr < end)
	{
		buckets = heap_cons(hp, ptr->bucket, buckets);
		ptr++;
	}
	return buckets;
}

term_t embed_list_bucket(term_t bucket, heap_t *hp)
{
	embed_buck_t *eb = find_bucket(bucket);
	if (eb == 0)
		return noval;

	term_t names = nil;
	for (int i = eb->start_index; i < eb->end_index; i++)
		names = heap_cons(hp, embed_bins[i].name, names);
	return names;
}

uint8_t *embed_lookup_simple(term_t name, int *size)
{
	*size = 0;

	//NB: traverse the list in reverse order to provide for proper shadowing of
	//    the files with the same name.
	
	embed_bin_t *ptr = embed_bins +nr_embed_bins -1;
	while (ptr >= embed_bins)
	{
		if (ptr->name == name)
		{
			*size = ptr->ends - ptr->starts;
			return ptr->starts;
		}
		ptr--;
	}
	return 0;
}

uint8_t *embed_lookup(term_t bucket, term_t name, int *size)
{
	*size = 0;

	embed_buck_t *eb = find_bucket(bucket);
	if (eb == 0)
		return 0;
	
	embed_bin_t *ptr = embed_bins +eb->start_index;
	embed_bin_t *end = embed_bins +eb->end_index;
	while (ptr < end)
	{
		if (ptr->name == name)
		{
			*size = ptr->ends - ptr->starts;
			return ptr->starts;
		}
		ptr++;
	}
	return 0;
}

static embed_buck_t *find_bucket(term_t bucket)
{
	// the list of buckets contains a dozen items - use linear search
	embed_buck_t *ptr = embed_bucks;
	embed_buck_t *end = ptr +nr_embed_bucks;
	while (ptr < end)
	{
		if (ptr->bucket == bucket)
			return ptr;
		ptr++;
	}
	return 0;
}

//EOF
