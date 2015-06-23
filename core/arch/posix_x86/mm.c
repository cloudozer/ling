//
//
//


#include "ling_common.h"

#include "mm.h"

#include <sys/mman.h>

#define MEMORY_SIZE		(256*1024*1024)
#ifdef __APPLE__
#	define MMAP_ADDR NULL
#elif __linux
#	define MMAP_ADDR 0x10000000
#endif

#if 0

#define MEMORY_END		((void *)(256*1024*1024))

extern char _membrk;

static void *free_page = &_membrk;
#endif

#define	GC_RESERVE		(16*1024*1024)

extern void exit(int) __attribute__((noreturn));

extern void *calloc(size_t count, size_t size);

static void *mem_start;
static void *mem_end;
static void *free_page;
#define MEMORY_END		mem_end

void mm_init(void)
{
#if 0
	// wipe out bss segment
	extern uint8_t _bss_start;
	extern uint8_t _bss_end;
	memset(&_bss_start, 0, &_bss_end - &_bss_start);
#endif
	mem_start = mmap(MMAP_ADDR, MEMORY_SIZE, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_ANON, -1, 0);
	if (mem_start == MAP_FAILED) {
		exit(43);
	}
	mem_end = (void *)((char *)mem_start + MEMORY_SIZE);
	free_page = mem_start;
}

void *mm_alloc_pages(int nr_pages)
{
	if (free_page + nr_pages * PAGE_SIZE > MEMORY_END -GC_RESERVE)
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

