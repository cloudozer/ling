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

#include "console.h"
#include "snprintf.h"
#include "atom_defs.h"

#include <string.h>

#define CONS_MIN_SEND_BUF	65536

#define CONSOLE_OP_GET_UNICODE_STATE	101
#define CONSOLE_OP_SET_UNICODE_STATE	102

static uint8_t *ol_console_get_send_buffer(outlet_t *ol, int len);
static int ol_console_send(outlet_t *ol, int len, term_t reply_to);
static void ol_console_new_data(outlet_t *ol, uint8_t *data, int dlen);
static term_t ol_console_control(outlet_t *ol, uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp);
static int ol_console_attach(outlet_t *ol);
static void ol_console_detach(outlet_t *ol);

static outlet_vtab_t ol_console_vtab = {
	.get_send_buffer = ol_console_get_send_buffer,
	.send = ol_console_send,
	.new_data = ol_console_new_data,
	.control = ol_console_control,
	.attach = ol_console_attach,
	.detach = ol_console_detach,
};

outlet_t *ol_console_factory(proc_t *cont_proc, uint32_t bit_opts)
{
	int extra = CONS_MIN_SEND_BUF;
	return outlet_make_N(&ol_console_vtab, cont_proc, bit_opts, extra);
}

#define OP_PUTC	0
#define OP_MOVE 1
#define OP_INSC 2
#define OP_DELC 3
#define OP_BEEP 4

static uint8_t *ol_console_get_send_buffer(outlet_t *ol, int len)
{
	if (len > ol->max_send_bufsize)
	{
		printk("*** Too much data sent to console:\n");
		printk("*** %d byte(s) sent - bufsize %d byte(s)\n", len, (int)ol->max_send_bufsize);
		printk("*** console will now close\n");
		return 0;	// do not resize
	}
	return ol->send_buffer;
}

static int ol_console_send(outlet_t *ol, int len, term_t reply_to)
{
	assert(len <= ol->max_send_bufsize);
	uint8_t *data = ol->send_buffer;

	assert(len > 0);
	switch (data[0])
	{
	case OP_PUTC:
		console_write((char *)data +1, len -1);
		break;
	case OP_MOVE:
	{
		assert(len == 3);
		int n = ((int)(int8_t)data[1]) << 8 | data[2];
		char seq[64];
		if (n > 0)
			snprintf(seq, sizeof(seq), "\x1b[%dC", n);
		else if (n < 0)
			snprintf(seq, sizeof(seq), "\x1b[%dD", -n);
		if (n != 0)
			console_write(seq, strlen(seq));
		break;
	}
	case OP_INSC:
	{
		console_write((char *)data +1, len -1);
		break;
	}
	case OP_DELC:
	{
		assert(len == 3);
		int n = ((int)(int8_t)data[1]) << 8 | data[2];
		char seq[64];
		if (n > 0)
			snprintf(seq, sizeof(seq), "\x1b[%dP", n);
		else
			snprintf(seq, sizeof(seq), "\x1b[%dD\x1b[%dP", -n, -n);
		console_write(seq, strlen(seq));
		break;
	}
	default:
		assert(data[0] == OP_BEEP);
		char beep = 7;
		console_write(&beep, 1);
	}

	return 0;
}

static void ol_console_new_data(outlet_t *ol, uint8_t *data, int dlen)
{
	outlet_pass_new_data(ol, data, dlen);
}

static term_t ol_console_control(outlet_t *ol,
		uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp)
{
	switch (op)
	{
	case CONSOLE_OP_GET_UNICODE_STATE:
		break;

	case CONSOLE_OP_SET_UNICODE_STATE:
		assert(dlen == 1);
		ol->unicode_state = *data;
		break;

	default:
		return nil;	// error
	}

	char us = ol->unicode_state;
	term_t result = heap_str_N(hp, &us, 1);
	if (result == noval)
		return A_NO_MEMORY;
	return result;
}

static int ol_console_attach(outlet_t *ol)
{
	console_attach(ol);
	return 0;
}

static void ol_console_detach(outlet_t *ol)
{
	console_detach(ol);
}

//EOF
