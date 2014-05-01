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

#include "ets.h"

#include <stdint.h>

#include "ling_common.h"

#include "nalloc.h"
#include "bits.h"
#include "limits.h"

#define ETS_ALLOC_INIT_SIZE 16384
#define ETS_ALLOC_EXT_SIZE	16384

static ets_plane_t *ets_current_plane = 0;
uint32_t total_ets_alloc_size = 0;

void ets_alloc_init(void)
{
	assert(sizeof(ets_plane_t) == sizeof(memnode_t));
	ets_current_plane = (ets_plane_t *)nalloc(ETS_ALLOC_INIT_SIZE -sizeof(ets_plane_t));
	total_ets_alloc_size += ets_current_plane->index *PAGE_SIZE;
}

uint32_t *ets_alloc(int wsize)
{
	assert(wsize > 0);
	assert(ets_current_plane != 0);
	if (ets_current_plane->ends - ets_current_plane->starts < WSIZE(ets_lot_t) + wsize)
	{
		// switch to a new plane - release current plane if unreferenced
		if (ets_current_plane->refc == 0)
		{
			total_ets_alloc_size -= ets_current_plane->index *PAGE_SIZE;
			nfree((memnode_t *)ets_current_plane);
		}

		int size = wsize *sizeof(uint32_t) +sizeof(ets_lot_t) +sizeof(ets_plane_t);
		if (size < ETS_ALLOC_EXT_SIZE)
			size = ETS_ALLOC_EXT_SIZE;
		// EXCEPTION POSSIBLE
		ets_current_plane = (ets_plane_t *)nalloc(size -sizeof(ets_plane_t));
		ets_current_plane->refc = 0;
		total_ets_alloc_size += ets_current_plane->index *PAGE_SIZE;
		assert(ets_current_plane->ends - ets_current_plane->starts >= WSIZE(ets_lot_t) + wsize);
	}

	ets_lot_t *lot = (ets_lot_t *)ets_current_plane->starts;
	ets_current_plane->starts += WSIZE(ets_lot_t) + wsize;

	lot->my_plane = ets_current_plane;
	ets_current_plane->refc++;
	lot->proc_bins = 0;
	lot->wsize = wsize;

	return (uint32_t *)(lot +1);
}

uint32_t *ets_alloc_N(int wsize)
{
	assert(wsize > 0);
	assert(ets_current_plane != 0);
	if (ets_current_plane->ends - ets_current_plane->starts < WSIZE(ets_lot_t) + wsize)
	{
		// switch to a new plane - release current plane if unreferenced
		if (ets_current_plane->refc == 0)
		{
			total_ets_alloc_size -= ets_current_plane->index *PAGE_SIZE;
			nfree((memnode_t *)ets_current_plane);
		}

		int size = wsize *sizeof(uint32_t) +sizeof(ets_lot_t) +sizeof(ets_plane_t);
		if (size < ETS_ALLOC_EXT_SIZE)
			size = ETS_ALLOC_EXT_SIZE;
		ets_plane_t *new_plane = (ets_plane_t *)nalloc_N(size -sizeof(ets_plane_t));
		if (new_plane == 0)
			return 0;
		ets_current_plane = new_plane;
		ets_current_plane->refc = 0;
		total_ets_alloc_size += ets_current_plane->index *PAGE_SIZE;
		assert(ets_current_plane->ends - ets_current_plane->starts >= WSIZE(ets_lot_t) + wsize);
	}

	ets_lot_t *lot = (ets_lot_t *)ets_current_plane->starts;
	ets_current_plane->starts += WSIZE(ets_lot_t) +wsize;

	lot->my_plane = ets_current_plane;
	ets_current_plane->refc++;
	lot->proc_bins = 0;
	lot->wsize = wsize;

	return (uint32_t *)(lot +1);
}

void ets_free(uint32_t *p)
{
	ets_lot_t *lot = (ets_lot_t *)p -1;

	// Unlink embedded proc bins
	while (lot->proc_bins != 0)
		proc_bin_unlink(lot->proc_bins, 0);

	assert(lot->my_plane->refc > 0);
	lot->my_plane->refc--;
	if (lot->my_plane->refc == 0)
	{
		// my_plane may be the current plane
		if (lot->my_plane == ets_current_plane)
			ets_current_plane->starts = (uint32_t *)(ets_current_plane +1);
		else
		{
			total_ets_alloc_size -= lot->my_plane->index *PAGE_SIZE;
			nfree((memnode_t *)lot->my_plane);
		}
	}
}

//EOF
