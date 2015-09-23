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
#include "string.h"

#define GC_RESERVE_RATIO	64
#define GC_MAX_RESERVED		4096

/* page allocator data */
static void *free_pages_start = 0;
static void *free_pages_end = 0;
static int nr_reserved_pages = 0;

void *data_section_backup = 0;
extern char _data, _edata;

void *mm_alloc_pages(int nr_pages);

void mm_init(unsigned long nr_pages, unsigned long pt_base, unsigned long nr_pt_frames)
{
	unsigned long pt_pfn = virt_to_pfn(pt_base) + nr_pt_frames;	// is it always so?

	arch_mm_init(&pt_pfn, pt_base, nr_pt_frames, nr_pages);

	/* Initialize page allocator */
	free_pages_start = pfn_to_virt(pt_pfn);
	free_pages_end = pfn_to_virt(nr_pages);

	/* GC cannot run if all pages are handed to other allocators; make a reserve */
	nr_reserved_pages = (free_pages_end - free_pages_start) /PAGE_SIZE /GC_RESERVE_RATIO;
	if (nr_reserved_pages > GC_MAX_RESERVED)
		nr_reserved_pages = GC_MAX_RESERVED;

	/* make a backup of .data section */
	size_t datalen = &_edata - &_data;
	void *backup = mm_alloc_pages(1 + datalen / PAGE_SIZE);
	memcpy(backup, &_data, datalen);
	data_section_backup = backup;
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

/*EOF*/
