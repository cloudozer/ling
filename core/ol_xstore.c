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

#if LING_XEN

#include "xenstore.h"
#include "xen/io/xs_wire.h"
#include "getput.h"

static term_t ol_xstore_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp);
static int ol_xstore_attach(outlet_t *ol);
static void ol_xstore_detach(outlet_t *ol);

static outlet_vtab_t ol_xstore_vtab = {
	.control = ol_xstore_control,
	.attach	 = ol_xstore_attach,
	.detach  = ol_xstore_detach,
};

static uint32_t next_request_id = 1;

outlet_t *ol_xstore_factory(proc_t *cont_proc, uint32_t bit_opts)
{
	return outlet_make_N(&ol_xstore_vtab, cont_proc, bit_opts, 0);
}

static term_t ol_xstore_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp)
{
	assert(op == XS_READ || op == XS_WRITE || op == XS_MKDIR || op == XS_RM || op == XS_DIRECTORY ||
		   op == XS_GET_PERMS || op == XS_SET_PERMS || op == XS_WATCH || op == XS_UNWATCH ||
		   op == XS_TRANSACTION_START || op == XS_TRANSACTION_END);
	assert(dlen >= 4);
	uint32_t tx_id = GET_UINT_32(data);
	uint8_t *payload = data+4;
	int pl_len = dlen-4;

	uint32_t req_id = next_request_id++;
	struct xsd_sockmsg msg = {
		.type = op,
		.req_id = req_id,
		.tx_id = tx_id,
		.len = pl_len,
	};

	xenstore_request((char *)&msg, sizeof(msg));
	xenstore_request((char *)payload, pl_len);
	xenstore_complete();

	ol->xstore_pend_op	   = op;
	ol->xstore_pend_req_id = req_id;
	ol->xstore_pend_tx_id  = tx_id;

	return nil;
}

static int ol_xstore_attach(outlet_t *ol)
{
	xstore_attach(ol);
	return 0;
}

static void ol_xstore_detach(outlet_t *ol)
{
	xstore_detach(ol);
}

#endif
