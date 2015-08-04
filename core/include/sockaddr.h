#ifndef __LING_CORE_SOCKADDR_H__
#define __LING_CORE_SOCKADDR_H__

#include <stdint.h>

#include "lassert.h"

#ifdef LING_WITH_LWIP

# include "lwip/ip_addr.h"
# include "lwip/netif.h"

# undef LWIP_SOCKET
# define LWIP_SOCKET 1
# include "lwip/sockets.h"
# undef LWIP_SOCKET

# undef send
# undef sendto

#else
# include <netinet/in.h>

typedef struct ip_addr {
    uint32_t addr;
} ip_addr_t;

typedef struct ip6_addr {
    uint32_t addr[4];
} ip6_addr_t;

typedef union {
    ip_addr_t ip4;
    ip6_addr_t ip6;
} ipX_addr_t;

static inline void
ip_addr_set_zero(ip_addr_t *ipaddr) {
    ipaddr->addr = 0;
}

#define ip_addr_set(dest, src) ((dest)->addr = \
                                    ((src) == NULL ? 0 : \
                                    (src)->addr))

typedef void (*netif_status_callback_fn)();

struct netif;
#endif //LING_WITH_LWIP

typedef struct {
    union {
        struct sockaddr     saddr;
        struct sockaddr_in  in;
        struct sockaddr_in6 in6;
    };
} saddr_t;


static inline uint16_t
sockaddr_port(const struct sockaddr *sa)
{
	switch (sa->sa_family)
	{
	case AF_INET: return ntohs(((const struct sockaddr_in *)sa)->sin_port);
	case AF_INET6: return ntohs(((const struct sockaddr_in6 *)sa)->sin6_port);
	default: return 0;	  /* TODO */
	}
}

static inline uint16_t
get_ipv6_nth(const uint8_t addr[16], unsigned nth)
{
	assert(nth < 8);
	nth *= 2;
	return ((uint16_t)addr[nth] << 8) | (uint16_t)addr[nth + 1];
}

static inline void
sockaddrin_to_ipaddr(const struct sockaddr_in *sin, ip_addr_t *addr)
{
	addr->addr = sin->sin_addr.s_addr;
}

static inline void
sockaddrin6_to_ip6addr(const struct sockaddr_in6 *sin6, ip6_addr_t *addr)
{
	uint32_t *addrptr = (uint32_t *)sin6->sin6_addr.s6_addr;
	addr->addr[0] = addrptr[0];
	addr->addr[1] = addrptr[1];
	addr->addr[2] = addrptr[2];
	addr->addr[3] = addrptr[3];
}

static inline void
saddr_to_ipaddr(const saddr_t *saddr, ipX_addr_t *addr)
{
	switch (saddr->saddr.sa_family) {
	case AF_INET:   sockaddrin_to_ipaddr(&saddr->in, &addr->ip4); break;
	case AF_INET6:  sockaddrin6_to_ip6addr(&saddr->in6, &addr->ip6); break;
	default: assert(0); /* unknown sockaddr.sa_family */
	}
}

#endif //__LING_CORE_SOCKADDR_H__
