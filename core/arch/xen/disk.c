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

#include "disk.h"
#include "ling_common.h"

#include "xen/io/xenbus.h"
#include "xen/io/blkif.h"
#include "xen/io/protocols.h"

#include "mm.h"
#include "xenstore.h"
#include "grant.h"
#include "event.h"

#include <string.h>

// magic number; tap:aio specific?
#define DISK_HANDLE		51712
#define DISK_BASE		"device/vbd/51712"

static blkif_front_ring_t vbd_ring;
static grant_ref_t vbd_ring_ref;
static int vbd_is_readonly;

static uint32_t disk_port;

static disk_info_t vbd_info;

//#define BLK_RING_SIZE __RING_SIZE((struct blkif_sring *)0, PAGE_SIZE)
#define BLK_RING_SIZE		32
#define NR_DISK_BUFFERS		64

static void *disk_buffers[NR_DISK_BUFFERS];
static grant_ref_t disk_buf_refs[NR_DISK_BUFFERS];
static int free_bufs[NR_DISK_BUFFERS];
static int free_buf_head;
static int nr_free_bufs;

typedef struct req_info_t req_info_t;
struct req_info_t {
	complete_func_t complete;
	void *arg;
	int nr_sectors;
	int used_bufs[BLKIF_MAX_SEGMENTS_PER_REQUEST];
};

static req_info_t out_reqs[BLK_RING_SIZE];
static int free_reqs[BLK_RING_SIZE];
static int free_req_head;

static void disk_int(uint32_t port, void *data);

int disk_vbd_is_present(void)
{
	static int is_cached = 0;
	static int vbd_is_present = 0;
	if (is_cached)
		return vbd_is_present;

	int n;
	int rs = xenstore_read_int(&n, DISK_BASE "/backend-id");
	vbd_is_present = (rs == 0);
	is_cached = 1;

	return vbd_is_present;
}

void disk_init(void)
{
	domid_t backend_id;
	int n;
	int rs = xenstore_read_int(&n, DISK_BASE "/backend-id");
	assert(rs == 0);
	backend_id = n;

	// Set up a shared ring
	blkif_sring_t *shared = (blkif_sring_t *)mm_alloc_page();
	assert(shared != 0);
	memset(shared, 0, PAGE_SIZE);

	SHARED_RING_INIT(shared);
	FRONT_RING_INIT(&vbd_ring, shared, PAGE_SIZE);

	grants_allow_access(&vbd_ring_ref, backend_id, virt_to_mfn(shared));

	// Allocate an event channel
	disk_port = event_alloc_unbound(backend_id);
	event_bind(disk_port, disk_int, 0);

	rs = xenstore_write_uint(DISK_BASE "/ring-ref", vbd_ring_ref);
	assert(rs == 0);
	rs = xenstore_write_uint(DISK_BASE "/event-channel", disk_port);
	assert(rs == 0);
	rs = xenstore_write(DISK_BASE "/protocol", XEN_IO_PROTO_ABI_NATIVE); // mini-os
	assert(rs == 0);
	rs = xenstore_write_uint(DISK_BASE "/state", XenbusStateConnected);
	assert(rs == 0);

	// Retrieve the backend config location
	char xs_key[1024];
	rs = xenstore_read(DISK_BASE "/backend", xs_key, sizeof(xs_key));
	assert(rs == 0);
	int xs_key_len = strlen(xs_key);

	char buf[256];

	strcat(xs_key, "/mode");
	rs = xenstore_read(xs_key, buf, sizeof(buf));
	assert(rs == 0);
	xs_key[xs_key_len] = 0;
	vbd_is_readonly = (*buf != 'w');

	int i;
	for (i = 0; i < NR_DISK_BUFFERS; i++)
	{
		disk_buffers[i] = mm_alloc_page();
		assert(disk_buffers[i] != 0);
		unsigned long mfn = virt_to_mfn(disk_buffers[i]);
		grants_allow_access(&disk_buf_refs[i], backend_id, mfn);
	}

	free_buf_head = -1;
	for (i = NR_DISK_BUFFERS; i >= 0; i--)
	{
		free_bufs[i] = free_buf_head;
		free_buf_head = i;
	}
	nr_free_bufs = NR_DISK_BUFFERS;
	
	free_req_head = -1;
	for (i = BLK_RING_SIZE -1; i >= 0; i--)
	{
		free_reqs[i] = free_req_head;
		free_req_head = i;
	}

	// Verify backend state

	//
	// The "right" way to implement this is to use XS_WATCH. We resort to
	// polling to keep xenstore interface synchronous and thus simple. One day
	// we need to check what is the cost of polling here.
	//

	//
	// We have to notify the user that we are going to busy-wait for the disk
	// to connect. This is necessary because the process may stall.
	//
	printk("Attaching vbd...\n");

	int state;
	strcat(xs_key, "/state");
try_again:
	rs = xenstore_read_int(&state, xs_key);
	assert(rs == 0);
	if (state != XenbusStateConnected)
	{
		//printk("[%s]=%d, retrying...\n", xs_key, state);
		goto try_again;
	}
	xs_key[xs_key_len] = 0;

	//
	// The state is read ~25 times. It may be worthwhile not to wait for a
	// connected state and complete initialisation later, when creating an
	// outlet.
	//

	// Read additional info
	strcat(xs_key, "/info");
	rs = xenstore_read_u32(&vbd_info.info, xs_key);
	assert(rs == 0);
	xs_key[xs_key_len] = 0;

	strcat(xs_key, "/sectors");
	rs = xenstore_read_u64(&vbd_info.sectors, xs_key);
	assert(rs == 0);
	xs_key[xs_key_len] = 0;

	strcat(xs_key, "/sector-size");
	rs = xenstore_read_u32(&vbd_info.sector_size, xs_key);
	assert(rs == 0);
	xs_key[xs_key_len] = 0;

	assert(vbd_info.sector_size == SECTOR_SIZE);

	printk("vbd: info %d sectors %lu of size %u\n",
					vbd_info.info, vbd_info.sectors, vbd_info.sector_size);

	strcat(xs_key, "/feature-barrier");
	rs = xenstore_read_int(&n, xs_key);
	vbd_info.barrier = (rs == 0);
	xs_key[xs_key_len] = 0;

	strcat(xs_key, "/feature-flush-cache");
	rs = xenstore_read_int(&n, xs_key);
	vbd_info.flush = (rs == 0);
	xs_key[xs_key_len] = 0;

	strcat(xs_key, "/feature-trim");
	rs = xenstore_read_int(&n, xs_key);
	vbd_info.trim = (rs == 0);
	xs_key[xs_key_len] = 0;

	printk("vbd: barrier %d flush-cache %d trim %d\n",
				vbd_info.barrier, vbd_info.flush, vbd_info.trim);
}

int disk_read(uint64_t sector_start, uint32_t num_sectors,
					complete_func_t complete_cb, void *callback_arg)
{
	assert(num_sectors > 0);
	assert(num_sectors <= BLKIF_MAX_SEGMENTS_PER_REQUEST *SECTORS_PER_PAGE);

	if (sector_start >= vbd_info.sectors ||
		sector_start +num_sectors > vbd_info.sectors)
			return -BAD_ARG;

	if (free_req_head == -1)
		return -TOO_LONG;

	int req_info_idx = free_req_head;
	free_req_head = free_reqs[free_req_head];
	assert(req_info_idx != -1);

	req_info_t *rinfo = &out_reqs[req_info_idx];
	rinfo->complete = complete_cb;
	rinfo->arg = callback_arg;
	rinfo->nr_sectors = num_sectors;

	int nr_segs = 0;

	RING_IDX prod = vbd_ring.req_prod_pvt;
	blkif_request_t *req = RING_GET_REQUEST(&vbd_ring, prod);

	req->operation = BLKIF_OP_READ;

	while (num_sectors > 0)
	{
		int seg_size = num_sectors;
		if (seg_size > SECTORS_PER_PAGE)
			seg_size = SECTORS_PER_PAGE;
		
		int buffer = free_buf_head;
		free_buf_head = free_bufs[free_buf_head];
		assert(buffer != -1);
		nr_free_bufs--;

		req->seg[nr_segs].gref = disk_buf_refs[buffer];
		req->seg[nr_segs].first_sect = 0;
		req->seg[nr_segs].last_sect = seg_size -1;
		
		rinfo->used_bufs[nr_segs++] = buffer;

		num_sectors -= seg_size;
	}

	req->nr_segments = nr_segs;
	req->handle = DISK_HANDLE;
	req->id = req_info_idx;
	req->sector_number = sector_start;
	vbd_ring.req_prod_pvt = prod +1;
	wmb();
	int notify;
	RING_PUSH_REQUESTS_AND_CHECK_NOTIFY(&vbd_ring, notify);
	if (notify)
		event_kick(disk_port);

	return 0;
}

int disk_write(uint64_t sector_start, uint8_t *data, int dlen,
					complete_func_t complete_cb, void *callback_arg)
{
	assert(dlen > 0 && dlen <= BLKIF_MAX_SEGMENTS_PER_REQUEST *PAGE_SIZE);
	assert((dlen & (SECTOR_SIZE -1)) == 0);

	int nr_sectors = dlen / SECTOR_SIZE;
	if ((nr_sectors +SECTORS_PER_PAGE -1) / SECTORS_PER_PAGE > nr_free_bufs)
		return -TOO_LONG;

	if (free_req_head == -1)
		return -TOO_LONG;

	int req_info_idx = free_req_head;
	free_req_head = free_reqs[free_req_head];
	assert(req_info_idx != -1);
	req_info_t *rinfo = &out_reqs[req_info_idx];
	rinfo->complete = complete_cb;
	rinfo->arg = callback_arg;
	rinfo->nr_sectors = 0;
	int nr_segs = 0;

	while (nr_sectors > 0)
	{
		int seg_size = nr_sectors;
		if (seg_size > SECTORS_PER_PAGE)
			seg_size = SECTORS_PER_PAGE;
		
		int buffer = free_buf_head;
		free_buf_head = free_bufs[free_buf_head];
		assert(buffer != -1);
		nr_free_bufs--;
		
		memcpy(disk_buffers[buffer], data, seg_size *SECTOR_SIZE);
		data += seg_size *SECTOR_SIZE;
		nr_sectors -= seg_size;

		rinfo->used_bufs[nr_segs++] = buffer;
		rinfo->nr_sectors += seg_size;
	}

	RING_IDX prod = vbd_ring.req_prod_pvt;
	blkif_request_t *req = RING_GET_REQUEST(&vbd_ring, prod);

	req->operation = BLKIF_OP_WRITE;

	int i;
	for (i = 0; i < nr_segs; i++)
	{
		int buffer = rinfo->used_bufs[i];
		req->seg[i].gref = disk_buf_refs[buffer];
		req->seg[i].first_sect = 0;
		req->seg[i].last_sect = SECTORS_PER_PAGE -1;
	}
	req->seg[nr_segs -1].last_sect =
			(rinfo->nr_sectors -1) & (SECTORS_PER_PAGE -1);

	req->nr_segments = nr_segs;
	req->handle = DISK_HANDLE;
	req->id = req_info_idx;
	req->sector_number = sector_start;
	vbd_ring.req_prod_pvt = prod +1;
	wmb();
	int notify;
	RING_PUSH_REQUESTS_AND_CHECK_NOTIFY(&vbd_ring, notify);
	if (notify)
		event_kick(disk_port);

	return 0;
}

int disk_barrier(complete_func_t complete_cb, void *callback_arg)
{
	if (!vbd_info.barrier)
		return -NOT_FOUND;	// not the best error code

	int req_info_idx = free_req_head;
	free_req_head = free_reqs[free_req_head];
	assert(req_info_idx != -1);

	req_info_t *rinfo = &out_reqs[req_info_idx];
	rinfo->complete = complete_cb;
	rinfo->arg = callback_arg;
	rinfo->nr_sectors = 0;

	RING_IDX prod = vbd_ring.req_prod_pvt;
	blkif_request_t *req = RING_GET_REQUEST(&vbd_ring, prod);

	req->operation = BLKIF_OP_WRITE_BARRIER;
	req->nr_segments = 0;
	req->handle = DISK_HANDLE;
	req->id = req_info_idx;
	req->sector_number = 0;
	vbd_ring.req_prod_pvt = prod +1;
	wmb();
	int notify;
	RING_PUSH_REQUESTS_AND_CHECK_NOTIFY(&vbd_ring, notify);
	if (notify)
		event_kick(disk_port);

	return 0;
}

int disk_flush(complete_func_t complete_cb, void *callback_arg)
{
	if (!vbd_info.flush)
		return -NOT_FOUND;	// not the best error code

	int req_info_idx = free_req_head;
	free_req_head = free_reqs[free_req_head];
	assert(req_info_idx != -1);

	req_info_t *rinfo = &out_reqs[req_info_idx];
	rinfo->complete = complete_cb;
	rinfo->arg = callback_arg;
	rinfo->nr_sectors = 0;

	RING_IDX prod = vbd_ring.req_prod_pvt;
	blkif_request_t *req = RING_GET_REQUEST(&vbd_ring, prod);

	req->operation = BLKIF_OP_FLUSH_DISKCACHE;
	req->nr_segments = 0;
	req->handle = DISK_HANDLE;
	req->id = req_info_idx;
	req->sector_number = 0;
	vbd_ring.req_prod_pvt = prod +1;
	wmb();
	int notify;
	RING_PUSH_REQUESTS_AND_CHECK_NOTIFY(&vbd_ring, notify);
	if (notify)
		event_kick(disk_port);

	return 0;
}

static void disk_int(uint32_t port, void *data)
{
	RING_IDX prod, cons;

try_harder:
	prod = vbd_ring.sring->rsp_prod;
	rmb();	// magic
	cons = vbd_ring.rsp_cons;

	while (cons != prod)
	{
		blkif_response_t *rsp = RING_GET_RESPONSE(&vbd_ring, cons);
		req_info_t *rinfo = &out_reqs[rsp->id];

		if (rinfo->complete)
			rinfo->complete(rsp->status, rinfo, rinfo->arg);

		int nr_segs = (rinfo->nr_sectors +SECTORS_PER_PAGE -1) / SECTORS_PER_PAGE;
		
		int i;
		for (i = 0; i < nr_segs; i++)
		{
			int buffer = rinfo->used_bufs[i];
			free_bufs[buffer] = free_buf_head;
			free_buf_head = buffer;
			nr_free_bufs++;
		}

		free_reqs[rsp->id] = free_req_head;
		free_req_head = rsp->id;

		vbd_ring.rsp_cons = ++cons;
	}

	int more;
	RING_FINAL_CHECK_FOR_RESPONSES(&vbd_ring, more);
	if (more)
		goto try_harder;
}

void disk_retrieve_data(void *info, uint8_t *data, int expected_sectors)
{
	req_info_t *rinfo = (req_info_t *)info;
	assert(rinfo->nr_sectors == expected_sectors);
	int nr_segs = (rinfo->nr_sectors +SECTORS_PER_PAGE -1) / SECTORS_PER_PAGE;
	int sectors_left = rinfo->nr_sectors;
		
	int i;
	for (i = 0; i < nr_segs; i++)
	{
		int seg_size = sectors_left;
		if (seg_size > SECTORS_PER_PAGE)
			seg_size = SECTORS_PER_PAGE;
		int buffer = rinfo->used_bufs[i];
		memcpy(data, disk_buffers[buffer], seg_size *SECTOR_SIZE);
		data += seg_size *SECTOR_SIZE;
		sectors_left -= seg_size;
	}
	assert(sectors_left == 0);
}

disk_info_t *disk_get_info(void)
{
	if (!disk_vbd_is_present())
		return 0;

	return &vbd_info;
}

