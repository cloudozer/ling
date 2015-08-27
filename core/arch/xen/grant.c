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

#include "ling_common.h"
#include "ling_xen.h"
#include "xen/grant_table.h"

#include "mm.h"

#define NR_GRANT_PAGES		32

#define NR_RESERVED_ENTRIES	8

//
// Valid refs: NR_RESERVED_ENTRIES .. NR_GRANT_ENTRIES -1
//

#define NR_GRANT_ENTRIES	(NR_GRANT_PAGES*PAGE_SIZE / sizeof(grant_entry_t))
#define NO_GRANT_ENTRY		((grant_ref_t)-1)

static grant_entry_t *grant_entries = 0;
static grant_ref_t free_list[NR_GRANT_ENTRIES];
static grant_ref_t free_entry = NO_GRANT_ENTRY;

void grants_init(void)
{
	unsigned long frames[NR_GRANT_PAGES];
	free_entry = NO_GRANT_ENTRY;

	gnttab_setup_table_t op;
	op.dom = DOMID_SELF;
	op.nr_frames = NR_GRANT_PAGES;
	set_xen_guest_handle(op.frame_list, frames);
	int rs = HYPERVISOR_grant_table_op(GNTTABOP_setup_table, &op, 1);
	if (rs < 0)
		fatal_error("grants_init: setup_table failed: %d\n", rs);

	for (int i = NR_GRANT_ENTRIES-1; i >= NR_RESERVED_ENTRIES; i--)
	{
		free_list[i] = free_entry;
		free_entry = i;
	}

	grant_entries = mm_alloc_pages(NR_GRANT_PAGES);
	if (grant_entries == 0)
		fatal_error("grants_init: grant entries page allocation failed\n");
	for (int i = 0; i < NR_GRANT_PAGES; i++)
	{
		unsigned long ma_grant_table = frames[i] << PAGE_SHIFT;
		rs = HYPERVISOR_update_va_mapping((unsigned long)grant_entries + i*PAGE_SIZE,
			__pte(ma_grant_table | 7), UVMF_INVLPG);
		if (rs < 0)
			fatal_error("grants_init: update mapping failed: %d\n", rs);
	}
}

void grants_allow_access(grant_ref_t *ref, domid_t domid, unsigned long frame)
{
	*ref = NO_GRANT_ENTRY;
	
	if (free_entry == NO_GRANT_ENTRY)
		fatal_error("grants_allow_access: all taken\n");
	
	*ref = free_entry;
	free_entry = free_list[free_entry];
	
	grant_entry_t *entry = &grant_entries[*ref];
	entry->domid = domid;
	entry->frame = frame;
	wmb();
	entry->flags = GTF_permit_access;
}

void grants_end_access(grant_ref_t ref)
{
	uint16_t flags, nflags;
	
	nflags = grant_entries[ref].flags;
	do {
		if ((flags = nflags) & (GTF_reading|GTF_writing))
			fatal_error("grants_end_access: still in use\n");
	} while ((nflags = synch_cmpxchg(&grant_entries[ref].flags, flags, 0)) != flags);
	
	free_list[ref] = free_entry;
	free_entry = ref;
}

