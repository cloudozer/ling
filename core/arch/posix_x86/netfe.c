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

int build_getifaddrs_reply(char *buf, int len)
{
	assert(buf);
	assert(len > 0);
#define CHECK_BUF(nnew)   \
	do { if (cur + (nnew) > buf + len) goto nomem; } while (0)

	int ret;
	struct ifaddrs *iflist = NULL, *ifaddr;
	char *cur = buf;

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
		debug("%s: found %s\n", __FUNCTION__, ifaddr->ifa_name);
		CHECK_BUF(ifnamlen + 2); // two NUL bytes: one for the name, one for the opts

		memcpy(cur, ifaddr->ifa_name, ifnamlen); cur += ifnamlen;
		*cur++ = '\0';

		/* INET_IFOPT_ADDR */
		inet_sockaddr *saddr = (inet_sockaddr *)ifaddr->ifa_addr;
		debug("%s: ifa_addr.sa_family = %d\n", __FUNCTION__, saddr->saddr.sa_family);
		switch (saddr->saddr.sa_family)
		{
		case AF_INET:
			CHECK_BUF(2 + 4 + 1);
			*cur++ = INET_IFOPT_ADDR;
			*cur++ = INET_AF_INET;

			sockaddrin_to_ipaddr(&saddr->in, (ip_addr_t *)cur);
			cur += 4;
			break;
		case AF_INET6:
			CHECK_BUF(2 + 16 + 1);
			*cur++ = INET_IFOPT_ADDR;
			*cur++ = INET_AF_INET6;

			sockaddrin6_to_ip6addr(&saddr->in6, (ip6_addr_t *)cur);
			cur += 16;
			break;
#if defined(__APPLE__)
		case AF_LINK: {
			struct sockaddr_dl *sll = (struct sockaddr_dl *)ifaddr->ifa_addr;
			assert(sll->sdl_family == AF_LINK);

			//debug("%s: sdl_alen = %d\n", __FUNCTION__, sll->sdl_alen);
			sll->sdl_alen = 6;
			assert(sll->sdl_alen == 6);  // must be Ethernet address

			CHECK_BUF(1 + 2 + sll->sdl_alen + 1);
			*cur++ = INET_IFOPT_HWADDR;

			PUT_UINT_16(cur, ((uint16_t)sll->sdl_alen));
			cur += 2;

			memcpy(cur, LLADDR(sll), sll->sdl_alen);
			cur += sll->sdl_alen;
			} break;
#endif
#if defined(__linux__)
		case AF_PACKET: {
			struct sockaddr_ll *sll = (struct sockaddr_ll *)ifaddr->ifa_addr;
			assert(sll->sll_family == AF_PACKET);
			assert(sll->sll_halen == 6); // must be Ethernet

			CHECK_BUF(1 + 2 + sll->sll_halen + 1);
			*cur++ = INET_IFOPT_HWADDR;

			PUT_UINT_16(cur, (uint16_t)sll->sll_halen);
			cur += 2;

			memcpy(cur, sll->sll_addr, sll->sll_halen);
			cur += sll->sll_halen;
			} break;
#endif
		}

		*cur++ = '\0';

		ifaddr = ifaddr->ifa_next;
	}
	CHECK_BUF(1);
	*cur++ = '\0';

	freeifaddrs(iflist);
	return cur - buf;
nomem:
	freeifaddrs(iflist);
	return -ENOMEM; // must be negative to signal error
}

