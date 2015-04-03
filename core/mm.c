//
//
//


#include "ling_common.h"

#include "mm.h"

#if 0

#define MEMORY_END		((void *)(256*1024*1024))
#define	GC_RESERVE		(16*1024*1024)

extern char _membrk;

static void *free_page = &_membrk;
#endif

extern void *calloc(size_t count, size_t size);

void *mm_alloc_pages(int nr_pages)
{
	return calloc(nr_pages, PAGE_SIZE);
#if 0
	if (free_page + nr_pages *PAGE_SIZE > MEMORY_END -GC_RESERVE)
		return 0;
	void *allocated = free_page;
	free_page += nr_pages *PAGE_SIZE;
	return allocated;
#endif
}

int mm_alloc_left(void)
{
	return 100000;
#if 0
	return (MEMORY_END -free_page) /PAGE_SIZE;
#endif
}

void *mm_alloc_tmp(void)
{
	return calloc(1, PAGE_SIZE);
#if 0
	return free_page;
#endif
}

