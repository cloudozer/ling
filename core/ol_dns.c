// Copyright (c) 2013-2014 Cloudozer LLP. All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
// 
// * Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// 
// * Redistributions in any form must be accompanied by information on how to
// obtain complete source code for the LING software and any accompanying
// software that uses the LING software. The source code must either be included
// in the distribution or be available for no more than the cost of distribution
// plus a nominal fee, and must be freely redistributable under reasonable
// conditions.  For an executable file, complete source code means the source
// code for all modules it contains. It does not include source code for modules
// or files that typically accompany the major components of the operating
// system on which the executable file runs.
// 
// THIS SOFTWARE IS PROVIDED BY CLOUDOZER LLP ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE
// DISCLAIMED. IN NO EVENT SHALL CLOUDOZER LLP BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

//
//
//

#include "outlet.h"

#include "ling_common.h"

#include "lwip/err.h"
#include "lwip/ip4.h"
#include "lwip/ip6.h"
#include "lwip/dns.h"

#include "getput.h"

#include <string.h>

#define OP_GETHOSTBYNAME	1
#define OP_GETHOSTBYADDR	2
#define OP_CANCEL_REQUEST	3

#define PROTO_IPV4	1
#define PROTO_IPV6	2

#define UNIT_ERROR	0
#define UNIT_IPV4	4
#define UNIT_IPV6	16

#define MAX_PENDING_REQS	16

static uint8_t *ol_dns_get_send_buffer(outlet_t *ol, int len);
static int ol_dns_send(outlet_t *ol, int len, term_t reply_to);

typedef struct dns_req_t dns_req_t;
struct dns_req_t {
	uint32_t serial;
	term_t oid;
};

static dns_req_t pending_requests[MAX_PENDING_REQS];
static int num_pending = 0;

static outlet_vtab_t ol_dns_vtab = {
	.get_send_buffer = ol_dns_get_send_buffer,
	.send = ol_dns_send,
};

outlet_t *ol_dns_factory(proc_t *cont_proc, uint32_t bit_opts)
{
	int extra = 0;
	return outlet_make_N(&ol_dns_vtab, cont_proc, bit_opts, extra);
}

#if LWIP_DNS
static void send_reply(outlet_t *ol,
		uint32_t serial, const char *name, ip_addr_t *ipaddr)
{
	// Serial:32
	// Unit:8		=4
	// Naddr:32		=1
	// (B0..B3)
	// Nnames:32	=1
	// Name0		=name
	
	int name_len = strlen(name);
	int pack_size = 4 +1 +4 +4 +4 +name_len +1;
	uint8_t packet[pack_size];
	uint8_t *p = packet;
	PUT_UINT_32(p, serial);
	p += 4;
	*p++ = UNIT_IPV4;
	PUT_UINT_32(p, 1);
	p += 4;
	uint32_t a = PP_HTONL(ipaddr->addr);
	PUT_UINT_32(p, a);
	p += 4;
	PUT_UINT_32(p, 1);
	p += 4;
	strcpy((char *)p, name);

	outlet_pass_new_data(ol, packet, pack_size);
}

static void error_reply(outlet_t *ol, uint32_t serial, const char *error)
{
	// Serial:32
	// Unit:8			=0
	// Error/binary		="DNS error"

	int pack_size = 1 +strlen(error) +1;
	uint8_t packet[pack_size];
	uint8_t *p = packet;
	PUT_UINT_32(p, serial);
	p += 4;
	*p++ = UNIT_ERROR;
	strcpy((char *)p, error);

	outlet_pass_new_data(ol, packet, pack_size);
}

static void found_cb(const char *name, ip_addr_t *ipaddr, void *arg)
{
	dns_req_t *dr = (dns_req_t *)arg;
	uint32_t serial = dr->serial;
	term_t oid = dr->oid;

	if (dr < pending_requests +num_pending -1)
		*dr = pending_requests[num_pending -1];
	num_pending--;

	outlet_t *ol = outlet_lookup(oid);
	if (ol == 0)
		return;

	// PackSz:32
	// Serial:32
	// Unit:8			0,4,16
	// if (Unit == 0)
	// {
	//   Str/binary
	// }
	// else
	// {
	//   Naddr:32
	//   if (Unit == 4)
	//   {
	//     (B0..B3) x Naddr
	//   }
	//   if (Unit == 16)
	//   {
	//     (B0..B15) x Naddr
	//   }
	//   Nnames:32		>= 1
	//   Name0/binary
	//   Alias x (Nnames -1)
	// }
	
	if (ipaddr != 0)
		send_reply(ol, serial, name, ipaddr);
	else
		error_reply(ol, serial, "DNS error");
}
#endif /* LWIP_DNS */

static int find_pending(uint32_t serial)
{
	for (int i = 0; i < num_pending; i++)
		if (pending_requests[i].serial == serial)
			return i;
	return -1;
}

static uint8_t *ol_dns_get_send_buffer(outlet_t *ol, int len)
{
	if (len > ol->max_send_bufsize)
		return 0;
	return ol->send_buffer;
}

static int ol_dns_send(outlet_t *ol, int len, term_t reply_to)
{
	assert(len <= ol->max_send_bufsize);

	// Serial:32
	// Op:8			1,2,3
	// if (Op == 1)
	// {
	//   Proto:8		1,2
	//   Namez/binary
	// }
	// if (Op == 2)
	// {
	//   Proto:8		1,2
	//   if (Proto == 1)
	//   {
	//     B0..B3
	//   }
	//   if (Proto == 2)
	//   {
	//     B0..B15
	//   }
	// }
	// if (Op == 3)
	// {
	// }
	
	if (len < 4 +1)
		return -BAD_ARG;

	uint8_t *p = ol->send_buffer;
	uint32_t left = len;
	uint32_t serial = GET_UINT_32(p);
	p += 4;
	left -= 4;
	uint8_t op = *p++;
	left--;

	if (left < 1)
		return -BAD_ARG;
	uint8_t proto = *p++;
	left--; 
	if (proto != PROTO_IPV4)
	{
		error_reply(ol, serial, "IPv6 not supported");
		return 0;
	}

	if (op == OP_GETHOSTBYNAME)
#if LWIP_DNS
	{
		if (left < 1 +1)		// hostname must have at least 1 char
			return -BAD_ARG;

		if (p[left -1] != 0)	// not null-terminated
			return -BAD_ARG;
		const char *host = (const char *)p;
		ip_addr_t ipaddr;

		if (find_pending(serial) != -1)
			return -BAD_ARG;	// duplicate serial

		if (num_pending >= MAX_PENDING_REQS)
			return -TOO_LONG;
		dns_req_t *dr = pending_requests +num_pending;
		num_pending++;
		dr->serial = serial;
		dr->oid = ol->oid;

		err_t rc = dns_gethostbyname(host, &ipaddr, found_cb, dr);
		if (rc == ERR_OK)
		{
			// immediate reply
			send_reply(ol, serial, host, &ipaddr);
			num_pending--;
		}
		else
			assert(rc == ERR_INPROGRESS);
	}
#else /* LWIP_DNS */
		fatal_error("OP_GETHOSTBYNAME not supported");
#endif
	else if (op == OP_GETHOSTBYADDR)
#if LWIP_DNS
	{
		//TODO
		error_reply(ol, serial, "gethostbyaddr() not supported");
		return 0;
	}
#else /* LWIP_DNS */
		fatal_error("OP_GETHOSTBYADDR not supported");
#endif
	else
	{
		if (op != OP_CANCEL_REQUEST)
			return -BAD_ARG;

		int index = find_pending(serial);
		if (index < 0)
			return -BAD_ARG;

		if (index < num_pending -1)
			pending_requests[index] = pending_requests[num_pending -1];
		num_pending--;
	}

	return 0;
}

//EOF
