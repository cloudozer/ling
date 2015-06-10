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

#include <string.h>

#include "ol_inet.h"
#include "getput.h"
#include "atom_defs.h"

static uint8_t *ol_vif_get_send_buffer(outlet_t *ol, int len);
static int ol_vif_send(outlet_t *ol, int len, term_t reply_to);
static term_t ol_vif_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp);
static void ol_vif_new_data(outlet_t *ol, uint8_t *data, int dlen);
static int ol_vif_attach(outlet_t *ol);
static void ol_vif_detach(outlet_t *ol);

static outlet_vtab_t ol_vif_vtab = {
	.get_send_buffer = ol_vif_get_send_buffer,
	.send = ol_vif_send,
	.control = ol_vif_control,
	.new_data = ol_vif_new_data,
	.attach = ol_vif_attach,
	.detach = ol_vif_detach,
};

outlet_t *ol_vif_factory(proc_t *cont_proc, uint32_t bit_opts)
{
	return outlet_make_N(&ol_vif_vtab, cont_proc, bit_opts, 0);
}

static uint8_t *ol_vif_get_send_buffer(outlet_t *ol, int len)
{
	if (len > ol->max_send_bufsize)
		return 0; // not likely to happen due to small MTU size
	return ol->send_buffer;
}

static int ol_vif_send(outlet_t *ol, int len, term_t reply_to)
{
	assert(len <= ol->max_send_bufsize);
	
	if (ol->front_end == 0)
		return -BAD_ARG; 

	netfe_output(ol->front_end, ol->send_buffer, len);
	return 0;
}

static term_t ol_vif_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp)
{
	char rbuf[64];
	char *reply = rbuf;

	//printk("vif: op %d dlen %d\n", op, dlen);
	switch (op)
	{
	case VIF_REQ_OPEN:
	{
		if (dlen != 4)
			goto error;
		int index = GET_UINT_32(data);
		netfe_t *attach_to = netfe_get_eth_by_index(index);
		if (attach_to == 0)
			goto error;
		ol->front_end = attach_to;
		netfe_attach_outlet(attach_to, ol);
		*reply++ = VIF_REP_OK;
		break;
	}
	
	case VIF_REQ_SETOPTS:
	{
		if (dlen != 1 +4)
			goto error;
		int opt = data[0];
		if (opt == VIF_OPT_MAX_MQ_LEN)
		{
			int max_mq_len = GET_UINT_32(data +1);
			ol->max_mq_len = max_mq_len;
		}
		else
			goto error;
		*reply++ = VIF_REP_OK;
		break;
	}

	default:
error:
		*reply++ = VIF_REP_ERROR;
		char *einval = "einval";
		strcpy(reply, einval);
		reply += strlen(einval);
	}

	int rlen = reply -rbuf;
	assert(rlen >= 1 && rlen <= sizeof(rbuf));
	term_t result = heap_str_N(hp, rbuf, rlen);
	if (result == noval)
		return A_NO_MEMORY;

	return result;
}

static void ol_vif_new_data(outlet_t *ol, uint8_t *data, int dlen)
{
	outlet_pass_new_data(ol, data, dlen);
}

static int ol_vif_attach(outlet_t *ol)
{
	// the driver attaches to the front end using control call
	return 0;
}

static void ol_vif_detach(outlet_t *ol)
{
	if (ol->front_end != 0)
		netfe_detach_outlet(ol->front_end);
	ol->front_end = 0;
}

//EOF
