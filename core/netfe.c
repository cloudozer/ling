//
// Low-level network interface STUBS
//

#include "ling_common.h"

#include "netfe.h"

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
	//TODO
	fatal_error("not implemented");
}

//EOF
