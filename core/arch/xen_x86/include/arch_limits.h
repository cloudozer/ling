
#ifndef __ARCH_LIMITS_H__
#define __ARCH_LIMITS_H__

#define __PAGE_SHIFT      12

#ifdef __ASSEMBLY__
#define __PAGE_SIZE       (1 << __PAGE_SHIFT)
#else /* ASSEMBLY */
#ifdef __x86_64__
#define __PAGE_SIZE       (1UL << __PAGE_SHIFT)
#else
#define __PAGE_SIZE       (1ULL << __PAGE_SHIFT)
#endif
#endif /* ASSEMBLY */

#define __STACK_SIZE_PAGE_ORDER  4
#define __STACK_SIZE             (__PAGE_SIZE * (1 << __STACK_SIZE_PAGE_ORDER))

#define STACK_SIZE        __STACK_SIZE
          
#endif /* __ARCH_LIMITS_H__ */
