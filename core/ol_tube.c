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

#include "outlet.h"

#include "ling_common.h"

#include "tube.h"
#include "getput.h"
#include "atom_defs.h"

#define TUBE_REQ_OPEN	1
#define TUBE_REQ_ATTACH	2

#define TUBE_REP_OK		0
#define TUBE_REP_ERROR	1

static term_t ol_tube_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp);
static void ol_tube_destroy_private(outlet_t *ol);

static outlet_vtab_t ol_tube_vtab = {
	.control = ol_tube_control,
	.destroy_private = ol_tube_destroy_private,
};

outlet_t *ol_tube_factory(proc_t *cont_proc, uint32_t bit_opts)
{
	return outlet_make_N(&ol_tube_vtab, cont_proc, bit_opts, 0);
}

static term_t ol_tube_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp)
{
	uint8_t rbuf[256];
	uint8_t *reply = rbuf;

	if (op == TUBE_REQ_OPEN)
	{
		// peer_domid[4]
		if (dlen != 4)
			goto error;
		uint32_t peer_domid = GET_UINT_32(data);
		assert(ol->tube == 0);
		ol->tube = tube_make(peer_domid);
		if (ol->tube == 0)
			return A_NO_MEMORY;
		// page_ref[4] evtchn_tx[4] evtchn_rx[4]
		*reply++ = TUBE_REP_OK;
		uint32_t page_ref, evtchn_tx, evtchn_rx;
		tube_info(ol->tube, &page_ref, &evtchn_tx, &evtchn_rx);
		PUT_UINT_32(reply, page_ref);
		reply += 4;
		PUT_UINT_32(reply, evtchn_tx);
		reply += 4;
		PUT_UINT_32(reply, evtchn_rx);
		reply += 4;
	}
	else if (op == TUBE_REQ_ATTACH)
	{
		// peer_domid[4] page_ref[4] evtchn_tx[4] evtchn_rx[4]
		if (dlen != 4 +4 +4 +4)
			goto error;
		uint32_t peer_domid = GET_UINT_32(data);
		uint32_t page_ref   = GET_UINT_32(data +4);
		uint32_t evtchn_tx  = GET_UINT_32(data +8);
		uint32_t evtchn_rx  = GET_UINT_32(data +12);
		assert(ol->tube == 0);
		ol->tube = tube_attach(peer_domid, page_ref, evtchn_rx, evtchn_tx);
		if (ol->tube == 0)
			return A_NO_MEMORY;
		*reply++ = TUBE_REP_OK;
	}
	else
	{
error:
		*reply++ = TUBE_REP_ERROR;
	}

	int rlen = reply-rbuf;
	assert(rlen >= 1 && rlen <= sizeof(rbuf));
	term_t result = heap_str_N(hp, (char *)rbuf, rlen);
	return (result == noval) ?A_NO_MEMORY :result;
}

static void ol_tube_destroy_private(outlet_t *ol)
{
	if (ol->tube == 0)
		return;
	tube_destroy(ol->tube);
}

