#pragma once

#define NM_CACHE_ALIGN	128

#define NETMAP_BUF_SIZE	2048

struct netmap_slot {
	uint32_t buf_idx; /* buffer index */
	uint16_t len;	  /* packet length, to be copied to/from the hw ring */
	uint16_t flags;	  /* buf changed, etc. */
    uint64_t ptr; 	  /* grant reference or indirect buffer */
};

struct netmap_ring {
	/*
	 * buf_ofs is meant to be used through macros.
	 * It contains the offset of the buffer region from this
	 * descriptor.
	 */
	const int64_t	buf_ofs;
	const uint32_t	num_slots;	/* number of slots in the ring. */
	const uint32_t	nr_buf_size;
	const uint16_t	ringid;
	const uint16_t	dir;		/* 0: tx, 1: rx */

	uint32_t	head;		/* (u) first user slot */
	uint32_t    cur;		/* (u) wakeup point */
	uint32_t	tail;		/* (k) first kernel slot */

	uint32_t	flags;

	//struct timeval	ts;
	uint8_t		ts[8+8];	//two longs, not used

	/* opaque room for a mutex or similar object */
	uint8_t		sem[128] __attribute__((__aligned__(NM_CACHE_ALIGN)));	//not used

	/* the slots follow. This struct has variable size */
	struct netmap_slot slot[0];	/* array of slots. */
};

#define	NETMAP_RING_NEXT(r, i)				\
	((i)+1 == (r)->num_slots ? 0 : (i) + 1 )

/*
 * check if space is available in the ring.
 */
static inline int
nm_ring_empty(struct netmap_ring *ring)
{
	return (ring->cur == ring->tail);
}

static inline int
nm_ring_space(struct netmap_ring *ring)
{
    int ret = ring->tail - ring->cur;
    if (ret < 0)
            ret += ring->num_slots;
    return ret;
}

