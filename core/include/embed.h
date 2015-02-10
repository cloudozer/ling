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

#pragma once

#include "term.h"
#include "heap.h"

//
// embed_buck_t and embed_bin_t both have a '_raw' fields that are resolved to
// terms at runtime. It is possible to resolve them during compile time but this
// greatly increases the build time.
//

typedef struct embed_buck_t embed_buck_t;
struct embed_buck_t {
	uint8_t *bucket_raw;	//see above
	term_t bucket;
	int start_index;
	int end_index;
};

typedef struct embed_bin_t embed_bin_t;
struct embed_bin_t {
	uint8_t *name_raw;		//see above
	term_t name;
	uint8_t *starts;
	uint8_t *ends;
};

// These are four (of many) undefined symbols of vmling.o. They represent files
// embedded into the vmling image.
extern embed_buck_t *embed_bucks;
extern int nr_embed_bucks;
extern embed_bin_t *embed_bins;
extern int nr_embed_bins;

int embed_init(void);
term_t embed_all_buckets(heap_t *hp);
term_t embed_list_bucket(term_t bucket, heap_t *hp);
uint8_t *embed_lookup_simple(term_t name, int *size);
uint8_t *embed_lookup(term_t bucket, term_t name, int *size);

//EOF
