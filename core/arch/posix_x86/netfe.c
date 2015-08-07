//
// Low-level network interface STUBS
//

#include "ling_common.h"

#include "netfe.h"
#include "ol_inet.h"
#include "getput.h"

#include <string.h>
#include <errno.h>

#ifdef __linux__
# include <linux/if.h>
# include <linux/if_packet.h>
#else
# include <net/if.h>  // must come before <ifaddrs.h>, see `man getifaddrs`
# include <net/if_dl.h>
#endif

#include <ifaddrs.h>

netfe_t *netfe_get_eth_by_index(int index)
{
	//TODO
	fatal_error("not implemented");
}

void netfe_get_mac(netfe_t *fe, uint8_t *mac, int mac_len)
{
	//TODO
	fatal_error("not implemented");
}

void netfe_attach_lwip_netif(netfe_t *fe, struct netif *nf)
{
	//TODO
	fatal_error("not implemented");
}

void netfe_attach_outlet(netfe_t *fe, outlet_t *ol)
{
	//TODO
	fatal_error("not implemented");
}

void netfe_detach_outlet(netfe_t *fe)
{
	//TODO
	fatal_error("not implemented");
}

void netfe_output(netfe_t *fe, uint8_t *packet, int pack_len)
{
	//TODO
	fatal_error("not implemented");
}

#if __APPLE__
# define AF_LINKLAYER AF_LINK
#elif __linux__
# define AF_LINKLAYER AF_PACKET
#endif

int build_getifaddrs_reply(char *buf, int len)
{
	assert(buf);
	assert(len > 0);
#define CHECK_BUF(nnew)   \
	do { if (reply + (nnew) > buf + len) goto nomem; } while (0)

#define PUT_REPLY_SOCKADDR4(addr, opt) \
	do { \
		CHECK_BUF(2 + 4 + 1); \
		*reply++ = (opt); \
		*reply++ = INET_AF_INET; \
		sockaddrin_to_ipaddr(&(addr)->in, (ip_addr_t *)reply); \
		reply += 4; \
	} while (0)
#define PUT_REPLY_SOCKADDR6(addr, opt) \
	do { \
		CHECK_BUF(2 + 16 + 1); \
		*reply++ = (opt); \
		*reply++ = INET_AF_INET6; \
		sockaddrin6_to_ip6addr(&(addr)->in6, (ip6_addr_t *)reply); \
		reply += 16; \
	} while (0)

	int ret;
	struct ifaddrs *iflist = NULL, *ifaddr;
	char *reply = buf;
	*reply++ = INET_REP_OK;
	len--;

	ret = getifaddrs(&iflist);
	if (ret)
	{
		debug("%s: ifaddrlist() failed: %s\n", strerror(errno));
		return -1;
	}
	ifaddr = iflist;

	while (ifaddr)
	{
		size_t ifnamlen = strlen(ifaddr->ifa_name);
		CHECK_BUF(ifnamlen + 2); // two NUL bytes: one for the name, one for the opts

		memcpy(reply, ifaddr->ifa_name, ifnamlen); reply += ifnamlen;
		*reply++ = '\0'; // end of name

		/* INET_IFOPT_FLAGS */
		CHECK_BUF(1 + 4 + 1);
		*reply++ = INET_IFOPT_FLAGS;
		uint32_t ifflags = 0;
		if (ifaddr->ifa_flags & IFF_UP)          ifflags |= INET_IFF_UP;
		if (ifaddr->ifa_flags & IFF_LOOPBACK)    ifflags |= INET_IFF_LOOPBACK;
		if (ifaddr->ifa_flags & IFF_BROADCAST)   ifflags |= INET_IFF_BROADCAST;
		if (ifaddr->ifa_flags & IFF_POINTOPOINT) ifflags |= INET_IFF_POINTTOPOINT;
		if (ifaddr->ifa_flags & IFF_RUNNING)     ifflags |= INET_IFF_RUNNING;
		if (ifaddr->ifa_flags & IFF_MULTICAST)   ifflags |= INET_IFF_MULTICAST;
		PUT_UINT_32(reply, ifflags);
		reply += 4;

		/* INET_IFOPT_ADDR */
		saddr_t *saddr = (saddr_t *)ifaddr->ifa_addr;
		saddr_t *netmask = (saddr_t *)ifaddr->ifa_netmask;
		saddr_t *broadaddr = (saddr_t *)ifaddr->ifa_broadaddr;
		debug("%s: %s, sa_family = %d\n", __FUNCTION__,
		      ifaddr->ifa_name, saddr->saddr.sa_family);
		switch (saddr->saddr.sa_family)
		{
		case AF_INET:
			PUT_REPLY_SOCKADDR4(saddr, INET_IFOPT_ADDR);

			if (netmask)
				PUT_REPLY_SOCKADDR4(netmask, INET_IFOPT_NETMASK);

			if (broadaddr && (ifflags & INET_IFF_BROADCAST))
				PUT_REPLY_SOCKADDR4(broadaddr, INET_IFOPT_BROADADDR);
			break;

		case AF_INET6:
			PUT_REPLY_SOCKADDR6(saddr, INET_IFOPT_ADDR);

			if (netmask)
				PUT_REPLY_SOCKADDR6(netmask, INET_IFOPT_NETMASK);

			if (broadaddr && (ifflags & INET_IFF_BROADCAST))
				PUT_REPLY_SOCKADDR6(broadaddr, INET_IFOPT_BROADADDR);
			break;

		case AF_LINKLAYER: {
#if __APPLE__
			struct sockaddr_dl *sll = (struct sockaddr_dl *)ifaddr->ifa_addr;
			size_t lladdrlen = sll->sdl_alen;
			char *lladdr = LLADDR(sll);
#elif __linux__
			struct sockaddr_ll *sll = (struct sockaddr_ll *)ifaddr->ifa_addr;
			size_t lladdrlen = sll->sll_halen;
			char *lladdr = (char *)sll->sll_addr;
#endif
			if (lladdrlen == 0) break;

			CHECK_BUF(1 + 2 + lladdrlen + 1);
			*reply++ = INET_IFOPT_HWADDR;

			PUT_UINT_16(reply, (uint16_t)lladdrlen);
			reply += 2;

			memcpy(reply, lladdr, lladdrlen);
			reply += lladdrlen;
			} break;
		}

		*reply++ = '\0'; // end of options

		ifaddr = ifaddr->ifa_next;
	}
	CHECK_BUF(1);
	*reply++ = '\0';

	freeifaddrs(iflist);
	return reply - buf;
nomem:
	freeifaddrs(iflist);
	return -ENOMEM; // must be negative to signal error
}

