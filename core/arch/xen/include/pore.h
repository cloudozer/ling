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
#include "nalloc.h"

#if LING_XEN

#include "grant.h"
#include "xenstore.h"
#include "xen/io/xs_wire.h"

typedef struct proc_t proc_t;

typedef struct pore_t pore_t;
struct pore_t {
	term_t eid;
	term_t tag;		// A_XENSTORE, etc
	term_t owner;	// pid
	void (*destroy_private)(pore_t *);
	memnode_t *home;
	uint32_t evtchn;
	pore_t **ref;
	pore_t *next;
};

typedef struct pore_xs_t pore_xs_t;
struct pore_xs_t {
	pore_t parent;
	struct xenstore_domain_interface *intf;
};

#define NUM_STRAW_REFS 		8
#define STRAW_RING_SIZE		(NUM_STRAW_REFS/2*PAGE_SIZE -8)

//NB: straw_ring_t must fit NUM_STRAW_REFS pages - not checked
typedef struct straw_ring_t straw_ring_t;
struct straw_ring_t {
	int32_t in_prod;
	int32_t in_cons;
	int32_t out_cons;
	int32_t out_prod;
	uint8_t input[STRAW_RING_SIZE];
	uint8_t output[STRAW_RING_SIZE];
};

typedef struct pore_straw_t pore_straw_t;
struct pore_straw_t {
	pore_t parent;
	int active;
	struct gnttab_map_grant_ref page_map[NUM_STRAW_REFS];
	uint32_t ring_refs[NUM_STRAW_REFS];
	straw_ring_t *shared;
};

pore_t *pore_make_N(term_t tag,
	uint32_t size, term_t owner, void (*destroy_private)(pore_t *), uint32_t evtchn);
pore_t *pore_lookup(term_t eid);
void pore_destroy(pore_t *pore);
void pore_destroy_owned_by(term_t pid);

#endif
