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

static uint8_t *ol_echo_get_send_buffer(outlet_t *ol, int len);
static int ol_echo_send(outlet_t *ol, int len, term_t reply_to);

static outlet_vtab_t ol_echo_vtab = {
	.get_send_buffer = ol_echo_get_send_buffer,
	.send = ol_echo_send,
};

outlet_t *ol_echo_factory(proc_t *cont_proc, uint32_t bit_opts)
{
	int extra = 0;
	return outlet_make_N(&ol_echo_vtab, cont_proc, bit_opts, extra);
}

static uint8_t *ol_echo_get_send_buffer(outlet_t *ol, int len)
{
	if (len > ol->max_send_bufsize)
		return 0;
	return ol->send_buffer;
}

static int ol_echo_send(outlet_t *ol, int len, term_t reply_to)
{
	assert(len <= ol->max_send_bufsize);
	outlet_pass_new_data(ol, ol->send_buffer, len);
	return 0;
}

//EOF
