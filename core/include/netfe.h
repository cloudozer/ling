#pragma once

#include <stdint.h>

#include "lwip/netif.h"

#define ETH_MTU		1500
#define ETH_ALEN	6

typedef struct outlet_t outlet_t;

typedef struct netfe_t netfe_t;

netfe_t *netfe_get_eth_by_index(int index);
void netfe_get_mac(netfe_t *fe, uint8_t *mac, int mac_len);
void netfe_attach_lwip_netif(netfe_t *fe, struct netif *nf);
void netfe_attach_outlet(netfe_t *fe, outlet_t *ol);
void netfe_detach_outlet(netfe_t *fe);
void netfe_output(netfe_t *fe, uint8_t *packet, int pack_len);
int build_getifaddrs_reply(char *buf, int len);

//EOF
