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

#include <string.h>

#include "arch_mm.h"
#include "grant.h"

#define GC_RESERVE_RATIO	64
#define GC_MAX_RESERVED		4096

#define MS_BITMAP_PAGES		1

/* page allocator data */
static void *free_pages_start = 0;
static void *free_pages_end = 0;
static int nr_reserved_pages = 0;

/* mapping space data */
static uint32_t *mapping_space_bitmap = 0;
static void *mapping_space_base = 0;

static pgentry_t new_pt_page(unsigned long *pt_pfn,
	pgentry_t *higher_tab, unsigned int higher_off, int level);

static void build_page_tables(pgentry_t *top_table,
	unsigned long *pt_pfn, void *start_va, void *ms_base, void *end_va);

static int bitmap_index(int nr_pages);
static void bitmap_mark(int index, int nr_bits, int pen);

void mm_init(unsigned long nr_pages, unsigned long pt_base, unsigned long nr_pt_frames)
{
	unsigned long pt_pfn = virt_to_pfn(pt_base) + nr_pt_frames;	// is it always so?

	// Allocate mapping space bitmap
	mapping_space_bitmap = pfn_to_virt(pt_pfn);
	mapping_space_base = pfn_to_virt(nr_pages);
	pt_pfn += MS_BITMAP_PAGES;
	memset(mapping_space_bitmap, 0, MS_BITMAP_PAGES *PAGE_SIZE);

	uint32_t ms_pages = MS_BITMAP_PAGES *PAGE_SIZE *8;

	unsigned long start_pfn = (nr_pt_frames - NOT_L1_FRAMES) * L1_PAGETABLE_ENTRIES;

	build_page_tables((pgentry_t *)pt_base,
		&pt_pfn, pfn_to_virt(start_pfn), pfn_to_virt(nr_pages), pfn_to_virt(nr_pages + ms_pages));

	/* Initialize page allocator */
	free_pages_start = pfn_to_virt(pt_pfn);
	free_pages_end = pfn_to_virt(nr_pages);

	/* GC cannot run if all pages are handed to other allocators; make a reserve */
	nr_reserved_pages = (free_pages_end - free_pages_start) /PAGE_SIZE /GC_RESERVE_RATIO;
	if (nr_reserved_pages > GC_MAX_RESERVED)
		nr_reserved_pages = GC_MAX_RESERVED;
}

/**
 * Page allocator
 *
 * Gives away whole pages of memory; never expects them back
 *
 */

void *mm_alloc_pages(int nr_pages)
{
	int size = nr_pages *PAGE_SIZE;
	if (free_pages_start + size > free_pages_end -nr_reserved_pages *PAGE_SIZE)
		return 0;
	void *mem = free_pages_start;
	free_pages_start += size;
	return mem;
}

int mm_alloc_left(void)
{
	return (free_pages_end - free_pages_start) / PAGE_SIZE;
}

// The unallocated space may be used as a temporary buffer provided that no
// allocation happen during its use. mm_alloc_left() returns the size of the
// buffer. The garbage collection region stack is the primary use of the buffer.
//
void *mm_alloc_tmp(void)
{
	return free_pages_start;
}

// Maps pages from another domain identified by domid. Returns an address of the
// mapped pages and fills in the handles array to allow clean unmapping later.
//	
void *ms_map_pages(grant_ref_t *refs, int nr_refs, domid_t domid, grant_handle_t *handles)
{
	// Find nr_refs consecutive zero bits in the bitmap
	int index = bitmap_index(nr_refs);
	if (index < 0)
		fatal_error("Cannot allocate %d pages from the mapping space", nr_refs);
	bitmap_mark(index, nr_refs, 1);

	void *addr = mapping_space_base + PAGE_SIZE*index;
	struct gnttab_map_grant_ref op[nr_refs];
	for (int i = 0; i < nr_refs; i++)
	{
		op[i].ref = refs[i];
		op[i].dom = domid;
		op[i].flags = GNTMAP_host_map;
		op[i].host_addr = (unsigned long)addr + PAGE_SIZE*i;
	}

	int rs = HYPERVISOR_grant_table_op(GNTTABOP_map_grant_ref, op, nr_refs);
	assert(rs == 0);

	for (int i = 0; i < nr_refs; i++)
	{
		assert(op[i].status == GNTST_okay);
		handles[i] = op[i].handle;
		rmb();	//dark
	}

	return addr;
}

// Unmap foreign pages. The handles array must be the one filled by ms_map_pages.
//
void ms_unmap_pages(void *addr, int nr_pages, grant_handle_t *handles)
{
	int index = (addr - mapping_space_base) /PAGE_SIZE;
	assert(index >= 0 && index < MS_BITMAP_PAGES *PAGE_SIZE *8);
	bitmap_mark(index, nr_pages, 0);

	struct gnttab_unmap_grant_ref op[nr_pages];
	for (int i = 0; i < nr_pages; i++)
	{
		op[i].host_addr = (unsigned long)addr + PAGE_SIZE*i;
		op[i].dev_bus_addr = 0;
		op[i].handle = handles[i];
	}

	int rs = HYPERVISOR_grant_table_op(GNTTABOP_unmap_grant_ref, op, nr_pages);
	assert(rs == 0);

	for (int i = 0; i < nr_pages; i++)
	{
		assert(op[i].status == GNTST_okay);
		rmb();
	}
}

static int bitmap_index(int nr_pages)
{
	int count = MS_BITMAP_PAGES *PAGE_SIZE /4;
	int w;
	for (w = 0; w < count; w++)
		if (mapping_space_bitmap[w] != 0xffffffff)
			break;
	if (w >= count)
		return -1;

	int b = 0;
	int nz = 0;
	do {
		if ((mapping_space_bitmap[w] & (1 << b)) == 0)
			nz++;
		else
			nz = 0;
		b++;
		if (b >= 32)
		{
			w++;
			if (w >= count)
				return -1;
			b = 0;
		}
	} while (nz < nr_pages);

	return w *32 +b -nr_pages;
}

static void bitmap_mark(int index, int nr_bits, int pen)
{
	int w = index / 32;
	int b = index % 32;
	while (nr_bits > 0)
	{
		int mask = (1 << b);
		assert(((mapping_space_bitmap[w] & mask) == 0) == (pen == 1));
		mapping_space_bitmap[w] &= ~mask;
		mapping_space_bitmap[w] |= (pen) ?mask :0;
		nr_bits--;
		b++;
		if (b >= 32)
		{
			w++;
			b = 0;
		}
	}
}

static void build_page_tables(pgentry_t *top_table,
	unsigned long *pt_pfn, void *start_va, void *ms_base, void *end_va)
{

	//
	//	*pt_pfn - unused pages already mapped by domain builder (512K+)
	//	carve new page tables/directories from *pt_pfn as needed
	//	start_va - end_va - range of frames to map
	//
	//	Pages between ms_base and end_va is the 'mapping space' for
	//	mapping pages from other domains.
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
			void *val = (start_va >= ms_base) ?0 :start_va;
			mmu_updates[count].val = virt_to_mfn(val) << PAGE_SHIFT | L1_PROT;
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

