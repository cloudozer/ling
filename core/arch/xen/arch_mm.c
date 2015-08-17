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

#include "arch_mm.h"

#include "string.h"

static pgentry_t new_pt_page(unsigned long *pt_pfn,
	pgentry_t *higher_tab, unsigned int higher_off, int level)
{
	// *pt_pfn is already mapped by domain builder
	// remap *pt_pfn as readonly, suitable for page table/directory
	// update higher_tab[higher_off] with reference to *pt_pfn
	// (*pt_pfn)++;

	pgentry_t prot_e, prot_t;
	prot_e = prot_t = 0;

    switch ( level )
    {
    case L1_FRAME:
        prot_e = L1_PROT;
        prot_t = L2_PROT;
        break;
    case L2_FRAME:
        prot_e = L2_PROT;
        prot_t = L3_PROT;
        break;
#if defined(__x86_64__)
	case L3_FRAME:
		prot_e = L3_PROT;
		prot_t = L4_PROT;
		break;
#endif
    default:
    	fatal_error("new_pt_page: level?");
    }

	void *pt_va = pfn_to_virt(*pt_pfn);
	memset(pt_va, 0, PAGE_SIZE);	// all entries not present
	
	unsigned long pte0 = pfn_to_mfn(*pt_pfn) << PAGE_SHIFT | (prot_e & ~_PAGE_RW);
	HYPERVISOR_update_va_mapping((unsigned long)pt_va, __pte(pte0), UVMF_INVLPG);
	
	pgentry_t pte = (pfn_to_mfn(*pt_pfn) << PAGE_SHIFT) | prot_t;
	
	struct mmu_update mu;
	mu.ptr = (virt_to_mfn(higher_tab) << PAGE_SHIFT) + sizeof(pgentry_t)*higher_off;
	mu.val = pte;
	HYPERVISOR_mmu_update(&mu, 1, 0, DOMID_SELF);
	
	(*pt_pfn)++;

	return pte;
}

static void build_page_tables(pgentry_t *top_table,
	unsigned long *pt_pfn, void *start_va, void *end_va)
{

	//
	//	*pt_pfn - unused pages already mapped by domain builder (512K+)
	//	carve new page tables/directories from *pt_pfn as needed
	//	start_va - end_va - range of frames to map
	//
	
	struct mmu_update mmu_updates[L1_PAGETABLE_ENTRIES];
	int count = 0;

	while (start_va + PAGE_SIZE <= end_va)
	{
		pgentry_t *tab = top_table;
		unsigned int offset;
		pgentry_t pte;

#if defined(__x86_64__)
		offset = l4_table_offset((unsigned long)start_va);
		pte = tab[offset];
		if ((pte & _PAGE_PRESENT) == 0)
			pte = new_pt_page(pt_pfn, tab, offset, L3_FRAME);
		tab = pte_to_virt(pte);
#endif

		offset = l3_table_offset((unsigned long)start_va);
		pte = tab[offset];
		if ((pte & _PAGE_PRESENT) == 0)
			pte = new_pt_page(pt_pfn, tab, offset, L2_FRAME);
		tab = pte_to_virt(pte);

		offset = l2_table_offset((unsigned long)start_va);
		pte = tab[offset];
		if ((pte & _PAGE_PRESENT) == 0)
			pte = new_pt_page(pt_pfn, tab, offset, L1_FRAME);
		tab = pte_to_virt(pte);

		offset = l1_table_offset((unsigned long)start_va);
		pte = tab[offset];
		if ((pte & _PAGE_PRESENT) == 0)
		{
			unsigned long pt_mfn = virt_to_mfn(tab);
			mmu_updates[count].ptr = ((pgentry_t)pt_mfn << PAGE_SHIFT) + sizeof(pgentry_t)*offset;
			mmu_updates[count].val = virt_to_mfn(start_va) << PAGE_SHIFT | L1_PROT;
			count++;
		}
		
		start_va += PAGE_SIZE;
		
		if (count == L1_PAGETABLE_ENTRIES || start_va + PAGE_SIZE > end_va)
		{
			int rc = HYPERVISOR_mmu_update(mmu_updates, count, 0, DOMID_SELF);
			if (rc < 0)
				fatal_error("build_page_tables: mmu_update failed: %d", rc);
			count = 0;
		}
	}
}

void arch_mm_init(unsigned long *pt_pfn,
	unsigned long pt_base, unsigned long nr_pt_frames, unsigned long nr_pages)
{
	unsigned long start_pfn = (nr_pt_frames - NOT_L1_FRAMES) * L1_PAGETABLE_ENTRIES;
	unsigned long end_pfn = nr_pages;	

	build_page_tables((pgentry_t *)pt_base,
		pt_pfn, pfn_to_virt(start_pfn), pfn_to_virt(end_pfn));
}

/*EOF*/

