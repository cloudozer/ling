/* OPTIONAL: Pools to replace heap allocation
 * Optional: Pools can be used instead of the heap for mem_malloc. If
 * so, these should be defined here, in increasing order according to 
 * the pool element size.
 *
 * LWIP_MALLOC_MEMPOOL(number_elements, element_size)
 */

#if MEM_USE_POOLS
LWIP_MALLOC_MEMPOOL_START
LWIP_MALLOC_MEMPOOL(4096, 256)
LWIP_MALLOC_MEMPOOL(4096, 512)
LWIP_MALLOC_MEMPOOL(4096, 1024)
LWIP_MALLOC_MEMPOOL(4096, 2048)
LWIP_MALLOC_MEMPOOL_END
#endif /* MEM_USE_POOLS */

//EOF
