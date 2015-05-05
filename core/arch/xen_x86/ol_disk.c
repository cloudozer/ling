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
#include "disk.h"

#include "ling_common.h"

#include "xen/io/blkif.h"

#include "scheduler.h"
#include "term_util.h"
#include "getput.h"
#include "string.h"
#include "atom_defs.h"

#define MAX_OUT_REQS	32

static term_t ol_disk_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp);
static void ol_disk_destroy_private(outlet_t *ol);

static outlet_vtab_t ol_disk_vtab = {
	.control = ol_disk_control,
	.destroy_private = ol_disk_destroy_private,
};

outlet_t *ol_disk_factory(proc_t *cont_proc, uint32_t bit_opts)
{
	int extra = MAX_OUT_REQS *sizeof(disk_req_t);
	outlet_t *new_ol = outlet_make_N(&ol_disk_vtab, cont_proc, bit_opts, extra);

	//
	// The free area in the home node is currently desginated as a send_buffer.
	// Disregard this as the disk driver does not use the send buffer.
	//

	//new_ol->free_reqs = 0;
	disk_req_t *req = (disk_req_t *)(new_ol +1);
	while (req +1 <= (disk_req_t *)new_ol->home_node->ends)
	{
		req->outlet = new_ol;	// never changes
		req->next = new_ol->free_reqs;
		new_ol->free_reqs = req;
		req++;
	}
	//new_ol->out_reqs = 0;

	return new_ol;
}

static int op_status(int16_t ll_stat)
{
	if (ll_stat == BLKIF_RSP_OKAY)
		return DISK_REP_OK;
	else if (ll_stat == BLKIF_RSP_ERROR)
		return DISK_REP_ERROR;
	assert(ll_stat == BLKIF_RSP_EOPNOTSUPP);
	return DISK_REP_ENOTSUP;
}

static void disk_async(proc_t *caller,
			term_t reply, term_t oid, uint16_t tag, term_t result)
{
	//
	// no need to marshal: reply is always an atom, result is either an atom or
	// a binary allocated on the caller's heap
	//
 
	uint32_t *p = heap_alloc_N(&caller->hp, 1 +4);
	if (p == 0)
		goto nomem;
	heap_set_top(&caller->hp, p +1 +4);
	p[0] = 4;
	p[1] = reply;
	p[2] = oid;
	p[3] = tag_int(tag);
	p[4] = result;
	term_t msg = tag_tuple(p);

	int x = scheduler_new_local_mail_N(caller, msg);
	if (x < 0)
		scheduler_signal_exit_N(caller, oid, err_to_term(x));

	return;
nomem:
	scheduler_signal_exit_N(caller, oid, A_NO_MEMORY);
}

static void allocate_request(disk_req_t *req)
{
	outlet_t *ol = req->outlet;
	assert(ol->free_reqs == req);

	ol->free_reqs = req->next;

	req->next = ol->out_reqs;
	if (ol->out_reqs)
		ol->out_reqs->ref = &req->next;
	ol->out_reqs = req;
	req->ref = &ol->out_reqs;
}

static void release_request(disk_req_t *req)
{
	outlet_t *ol = req->outlet;
	if (req->next)
		req->next->ref = req->ref;
	*req->ref = req->next;

	req->next = ol->free_reqs;
	ol->free_reqs = req;
}

static void read_complete_cb(int16_t ll_stat, void *info, void *arg)
{
	disk_req_t *req = (disk_req_t *)arg;
	outlet_t *ol = req->outlet;
	uint16_t async_tag = req->async_tag;
	uint32_t num_sectors = req->num_sectors;
	term_t reply_to = req->reply_to;
	release_request(req);

	// {disk_async,Port,Tag,Data}
	// {disk_async_error,Port,Tag,Error}

	proc_t *caller = scheduler_lookup(reply_to);
	if (caller == 0)
		return;

	int status = op_status(ll_stat);
	if (status == DISK_REP_OK)
	{
		uint8_t *ptr;
		term_t bin = heap_make_bin_N(&caller->hp,
							num_sectors *SECTOR_SIZE, &ptr);
		if (bin == noval)
			goto nomem;
		disk_retrieve_data(info, ptr, num_sectors);
		disk_async(caller, A_DISK_ASYNC, ol->oid, async_tag, bin);
	}
	else
	{
		term_t err = (DISK_REP_ERROR) ?A_EIO :A_ENOTSUP;
		disk_async(caller, A_DISK_ASYNC_ERROR, ol->oid, async_tag, err);
	}

	return;
nomem:
	scheduler_signal_exit_N(caller, ol->oid, A_NO_MEMORY);
}

static void simple_complete_cb(int16_t ll_stat, void *info, void *arg)
{
	disk_req_t *req = (disk_req_t *)arg;
	outlet_t *ol = req->outlet;
	uint16_t async_tag = req->async_tag;
	term_t reply_to = req->reply_to;
	release_request(req);

	// {disk_async,Port,Tag,ok}
	// {disk_async_error,Port,Tag,Error}

	proc_t *caller = scheduler_lookup(reply_to);
	if (caller == 0)
		return;

	int status = op_status(ll_stat);
	if (status == DISK_REP_OK)
		disk_async(caller, A_DISK_ASYNC, ol->oid, async_tag, A_OK);
	else
	{
		term_t err = (DISK_REP_ERROR) ?A_EIO :A_ENOTSUP;
		disk_async(caller, A_DISK_ASYNC_ERROR, ol->oid, async_tag, err);
	}
}

#define REPLY_DISK_ERROR(err)	do { \
	*reply++ = DISK_REP_ERROR; \
	strcpy(reply, err); \
	reply += strlen(err); \
} while (0)

static term_t ol_disk_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp)
{
	char rbuf[256];
	char *reply = rbuf;

	switch (op)
	{
	case DISK_REQ_READ:
	{
		// async_tag[2] sector_start[8] num_sectors[4]

		if (dlen != 2 +8 +4)
			goto error;
		uint16_t async_tag = GET_UINT_16(data);
		uint64_t sector_start = GET_UINT_64(data +2);
		uint32_t num_sectors = GET_UINT_32(data +2 +8);

		if (ol->free_reqs == 0)
			goto error;

		// Allocate the disk_req_t later as disk_read may fail
		disk_req_t *req = ol->free_reqs;

		req->async_tag = async_tag;
		req->num_sectors = num_sectors;
		req->reply_to = reply_to;

		if (disk_read(sector_start, num_sectors, read_complete_cb, req) < 0)
			goto error;

		// Callback is imminent - allocate
		allocate_request(req);

		*reply++ = DISK_REP_OK;
		break;
	}
	case DISK_REQ_WRITE:
	{
		// async_tag[2] sector_start[8] num_sectors[4] data[n]

		if (dlen <= 2 +8 +4)
			goto error;
		uint16_t async_tag = GET_UINT_16(data);
		uint64_t sector_start = GET_UINT_64(data +2);
		uint32_t num_sectors = GET_UINT_32(data +2 +8);
		
		dlen -= (2 +8 +4);
		data += (2 +8 +4);

		if (dlen != num_sectors *SECTOR_SIZE)
			goto error;

		if (ol->free_reqs == 0)
			goto error;

		// Allocate the disk_req_t later as disk_write may fail
		disk_req_t *req = ol->free_reqs;

		req->async_tag = async_tag;
		req->num_sectors = num_sectors;
		req->reply_to = reply_to;

		if (disk_write(sector_start, data, dlen, simple_complete_cb, req) < 0)
			goto error;

		// Callback is imminent - allocate
		allocate_request(req);

		*reply++ = DISK_REP_OK;
		break;
	}
	case DISK_REQ_BARRIER:
	{
		// async_tag[2]

		if (dlen != 2)
			goto error;
		uint16_t async_tag = GET_UINT_16(data);
		
		if (ol->free_reqs == 0)
			goto error;

		// Allocate the disk_req_t later as disk_write may fail
		disk_req_t *req = ol->free_reqs;

		req->async_tag = async_tag;
		req->reply_to = reply_to;

		if (disk_barrier(simple_complete_cb, req) < 0)
			goto error;

		// Callback is imminent - allocate
		allocate_request(req);

		*reply++ = DISK_REP_OK;
		break;
	}
	case DISK_REQ_FLUSH:
	{
		// async_tag[2]

		if (dlen != 2)
			goto error;
		uint16_t async_tag = GET_UINT_16(data);
		
		if (ol->free_reqs == 0)
			goto error;

		// Allocate the disk_req_t later as disk_write may fail
		disk_req_t *req = ol->free_reqs;

		req->async_tag = async_tag;
		req->reply_to = reply_to;

		if (disk_flush(simple_complete_cb, req) < 0)
			goto error;

		// Callback is imminent - allocate
		allocate_request(req);

		*reply++ = DISK_REP_OK;
		break;
	}
	case DISK_REQ_TRIM:
		REPLY_DISK_ERROR("enotsup");
		break;

	default:
error:
	REPLY_DISK_ERROR("einval");
	}

	int rlen = reply -rbuf;
	assert(rlen >= 1 && rlen <= sizeof(rbuf));
	term_t result = heap_str_N(hp, rbuf, rlen);
	if (result == noval)
		return A_NO_MEMORY;

	return result;
}

static void ol_disk_destroy_private(outlet_t *ol)
{
	// force completion of outstanding requests
	disk_req_t *req = ol->out_reqs;
	while (req != 0)
	{
		proc_t *caller = scheduler_lookup(req->reply_to);
		if (caller != 0)
			disk_async(caller,
				A_DISK_ASYNC_ERROR, ol->oid, req->async_tag, A_CLOSED);
		req = req->next;
	}
}

//EOF
