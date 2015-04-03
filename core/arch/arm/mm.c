//
//
//

#include "ling_common.h"

#include "mm.h"

#define MEMORY_END		((void *)(256*1024*1024))
#define	GC_RESERVE		(16*1024*1024)

extern char _membrk;

static void *free_page = &_membrk;

void *mm_alloc_pages(int nr_pages)
{
	if (free_page + nr_pages *PAGE_SIZE > MEMORY_END -GC_RESERVE)
		return 0;
	void *allocated = free_page;
	free_page += nr_pages *PAGE_SIZE;
	return allocated;
}

int mm_alloc_left(void)
{
	return (MEMORY_END -free_page) /PAGE_SIZE;
}

void *mm_alloc_tmp(void)
{
	return free_page;
}

