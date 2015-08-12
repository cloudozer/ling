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

#include "xenstore.h"
#include "ling_common.h"

#include <string.h>
#include "event.h"
#include "strings.h"

#include "atom_defs.h"
#include "outlet.h"
#include "scheduler.h"
#include "term_util.h"

static struct xenstore_domain_interface *store_intf = 0;
static uint32_t store_port = 0;
static uint32_t req_id = 1;
static struct outlet_t *attached_outlet = 0;

static void xstore_int(uint32_t port, void *data);

static int xenstore_error(const char *str, int len);

void xenstore_init(struct xenstore_domain_interface *intf, uint32_t port)
{
	store_intf = intf;
	store_port = port;
}

void xstore_attach(outlet_t *ol)
{
	if (attached_outlet != 0) 
		printk("*** Port %pt is stealing control over Xenstore from %pt\n",
				T(ol->oid), T(attached_outlet->oid));
	attached_outlet = ol;
	event_bind(store_port, xstore_int, 0);
}

void xstore_detach(outlet_t *ol)
{
	assert(attached_outlet == ol);
	event_unbind(store_port);
	attached_outlet = 0;
}

static term_t op_to_term(uint32_t op)
{
	if (op == XS_READ) 						return A_READ;
	else if (op == XS_WRITE)				return A_WRITE;
	else if (op == XS_MKDIR)				return A_MKDIR;
	else if (op == XS_RM)					return A_RM;
	else if (op == XS_DIRECTORY)			return A_DIRECTORY;
	else if (op == XS_GET_PERMS)			return A_GET_PERMS;
	else if (op == XS_SET_PERMS)			return A_SET_PERMS;
	else if (op == XS_WATCH)				return A_WATCH;
	else if (op == XS_WATCH_EVENT)			return A_WATCH_EVENT;
	else if (op == XS_UNWATCH)				return A_UNWATCH;
	else if (op == XS_TRANSACTION_START)	return A_TRANSACTION_START;
	else if (op == XS_TRANSACTION_END)		return A_TRANSACTION_END;
	assert(op == XS_ERROR);	
	return A_ERROR;
}

static void xstore_int(uint32_t port, void *data)
{
	assert(attached_outlet != 0);
	while (store_intf->rsp_prod > store_intf->rsp_cons)
	{
		struct xsd_sockmsg msg;
		xenstore_response((char *)&msg, sizeof(msg));
		proc_t *cont_proc = scheduler_lookup(attached_outlet->owner);
		assert(cont_proc != 0);
		term_t data = nil;
		if (msg.len > 0)
		{
			char payload[msg.len];
			xenstore_response(payload, msg.len);
			uint32_t pl_len = msg.len;
			if (payload[pl_len-1] == 0)
				pl_len--;
			data = heap_str_N(&cont_proc->hp, payload, pl_len);
		}
		if (msg.type != XS_WATCH_EVENT)	
		{
			assert(attached_outlet->xstore_pend_req_id == msg.req_id);
			assert(attached_outlet->xstore_pend_tx_id == msg.tx_id);
		}
		// {watch,Port,Path}
		uint32_t *p = 0;
		if (data != noval)
			p = heap_alloc_N(&cont_proc->hp, 1+3);
		if (data == noval || p == 0)
		{
			scheduler_signal_exit_N(cont_proc, attached_outlet->oid, A_NO_MEMORY);
			break;
		}
		else
		{
			heap_set_top(&cont_proc->hp, p+1+3);
			p[0] = 3;
			p[1] = op_to_term(msg.type);
			p[2] = attached_outlet->oid,
			p[3] = data;
			int x = scheduler_new_local_mail_N(cont_proc, tag_tuple(p));
			if (x < 0)
			{
				scheduler_signal_exit_N(cont_proc, attached_outlet->oid, err_to_term(x));
				break;
			}
		}
	}
}

void xenstore_request(char *message, int len)
{
	assert(len <= XENSTORE_RING_SIZE);
	
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

void xenstore_complete(void)
{
	event_kick(store_port);
}

void xenstore_response(char *buffer, int len)
{
	while (len > 0)
	{
		while (store_intf->rsp_prod - store_intf->rsp_cons == 0)
			mb();
		
		*buffer++ = store_intf->rsp[MASK_XENSTORE_IDX(store_intf->rsp_cons++)];
		len--;
		mb();
	}
}

int xenstore_write(const char *key, char *value)
{
	char buf[XENSTORE_RING_SIZE];

	int klen = strlen(key);
	int vlen = strlen(value);
	
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

int xenstore_read(const char *key, char *value, int len)
{
	int klen = strlen(key);
	
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

int xenstore_read_dir(const char *key, char *value, int len)
{
	int klen = strlen(key);
	
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

int xenstore_write_uint(const char *key, unsigned int n)
{
	char buf[256];
	char *val = i64toa(n, buf, sizeof(buf));
	return xenstore_write(key, val);
}

static int xenstore_error(const char *str, int len)
{
	int i;
	for (i = 0; i < sizeof(xsd_errors)/sizeof(struct xsd_errors); i++)
	{
		if (strncmp(str, xsd_errors[i].errstring, len) == 0)
			return xsd_errors[i].errnum;
	}
	return -1;
}

