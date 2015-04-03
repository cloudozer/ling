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

/**
 *
 *
 *
 */

#include <string.h>

#include "ling_common.h"

#include "ling_xen.h"

#include "errno.h"
#include "xen/io/xs_wire.h"

#include "event.h"
#include "strings.h"

static struct xenstore_domain_interface *store_intf = 0;
static uint32_t store_port = 0;
static uint32_t req_id = 0;

//static void handle_store_events(evtchn_port_t port, void *data, void *regs);

static int xenstore_error(const char *str, size_t len);

void xenstore_init(struct xenstore_domain_interface *intf, uint32_t port)
{
	store_intf = intf;
	store_port = port;
}

//static void handle_store_events(evtchn_port_t port, void *data, void *regs)
//{
//	//TODO
//}

static void xenstore_request(char *message, size_t len)
{
	if (len > XENSTORE_RING_SIZE)
		fatal_error("xenstore_request: too big");
	
	int prod = store_intf->req_prod;
	while (len > 0)
	{
		while (prod - store_intf->req_cons >= sizeof(store_intf->req))
			mb();
			
		store_intf->req[MASK_XENSTORE_IDX(prod++)] = *message++;
		len--;
	}
	wmb();
	store_intf->req_prod = prod;
}

static void xenstore_response(char *buffer, size_t len)
{
	int cons = store_intf->rsp_cons;
	while (len > 0)
	{
		while (store_intf->rsp_prod - cons == 0)
			mb();
		
		*buffer++ = store_intf->rsp[MASK_XENSTORE_IDX(cons++)];
		len--;
	}
	store_intf->rsp_cons = cons;
}

int xenstore_write(const char *key, char *value)
{
	char buf[XENSTORE_RING_SIZE];

	size_t klen = strlen(key);
	size_t vlen = strlen(value);
	
	struct xsd_sockmsg msg;
	msg.type = XS_WRITE;
	msg.req_id = req_id++;
	msg.tx_id = 0;
	msg.len = klen+1 + vlen;
	
	xenstore_request((char *)&msg, sizeof(msg));
	xenstore_request((char *)key, klen+1);
	xenstore_request((char *)value, vlen);
	event_kick(store_port);

	xenstore_response((char *)&msg, sizeof(msg));
	xenstore_response(buf, msg.len);
	if (msg.type == XS_ERROR)
		return xenstore_error(buf, msg.len);
	return 0;
}

int xenstore_read(const char *key, char *value, size_t len)
{
	size_t klen = strlen(key);
	
	struct xsd_sockmsg msg;
	msg.type = XS_READ;
	msg.req_id = req_id++;
	msg.tx_id = 0;
	msg.len = klen+1;

	xenstore_request((char *)&msg, sizeof(msg));
	xenstore_request((char *)key, klen+1);
	event_kick(store_port);

	xenstore_response((char *)&msg, sizeof(msg));
	if (msg.type == XS_ERROR)
	{
		char buf[XENSTORE_RING_SIZE];
		xenstore_response(buf, msg.len);
		return xenstore_error(buf, msg.len);
	}
	else
	{
		if (msg.len+1 > len)
			return -1;
		xenstore_response(value, msg.len);
		value[msg.len] = 0;
	}
	return 0;
}

int xenstore_read_int(int *result, const char *key)
{
	char buf[XENSTORE_RING_SIZE];
	int rs = xenstore_read(key, buf, sizeof(buf));
	if (rs != 0)
		return rs;

	*result = atoi64(buf);
	return 0;
}

int xenstore_read_u32(uint32_t *result, const char *key)
{
	char buf[XENSTORE_RING_SIZE];
	int rs = xenstore_read(key, buf, sizeof(buf));
	if (rs != 0)
		return rs;

	*result = atoi64(buf);
	return 0;
}

int xenstore_read_u64(uint64_t *result, const char *key)
{
	char buf[XENSTORE_RING_SIZE];
	int rs = xenstore_read(key, buf, sizeof(buf));
	if (rs != 0)
		return rs;

	*result = atoi64(buf);
	return 0;
}

int xenstore_read_dir(const char *key, char *value, size_t len)
{
	size_t klen = strlen(key);
	
	struct xsd_sockmsg msg;
	msg.type = XS_DIRECTORY;
	msg.req_id = req_id++;
	msg.tx_id = 0;
	msg.len = klen+1;

	xenstore_request((char *)&msg, sizeof(msg));
	xenstore_request((char *)key, klen+1);
	event_kick(store_port);

	xenstore_response((char *)&msg, sizeof(msg));
	if (msg.type == XS_ERROR)
	{
		char buf[XENSTORE_RING_SIZE];
		xenstore_response(buf, msg.len);
		return xenstore_error(buf, msg.len);
	}
	else
	{
		if (msg.len+1 > len)
			return -1;
		xenstore_response(value, msg.len);
		value[msg.len] = 0;		// add empty string at the end
	}
	return 0;
}

void xenstore_write_uint(const char *key, unsigned int n)
{
	char buf[256];
	char *val = i64toa(n, buf, sizeof(buf));
	xenstore_write(key, val);
}

static int xenstore_error(const char *str, size_t len)
{
	int i;
	for (i = 0; i < sizeof(xsd_errors)/sizeof(struct xsd_errors); i++)
	{
		if (strncmp(str, xsd_errors[i].errstring, len) == 0)
			return xsd_errors[i].errnum;
	}
	return -1;
}

/*EOF*/
