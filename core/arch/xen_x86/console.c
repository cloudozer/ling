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
 
#include "console.h"

#include "ling_common.h"
#include "ling_xen.h"
#include "xen/io/console.h"

#include "event.h"

//#define DEBUG_CONSOLE

#define	CTRLA	('a'-96)
#define CTRLT	('t'-96)
#define CTRLB	('b'-96)
#define CTRLQ	('q'-96)
#define CTRLF	('f'-96)

static struct {
	int is_initialized;
	struct xencons_interface *intf;
	outlet_t *attached;
	
	uint32_t chan;
} console = {
	.is_initialized = 0
};

void stringify_test(void);
void beam_load_test(uint8_t *beam_data, uint32_t beam_size);
void bits_test(void);
void nalloc_dump_stats(void);

//extern uint8_t fibo_beam_start[];
//extern uint8_t fibo_beam_end[];

#ifdef DEBUG_CONSOLE
static int debug_key(int key)
{
	//printk("key=%d\n", key);

	if (key == CTRLQ)
		domain_poweroff();

	if (key == CTRLT)
	{
		stringify_test();
		return 1;
	}
	if (key == CTRLB)
	{
		//beam_load_test(fibo_beam_start, fibo_beam_end - fibo_beam_start);
		//bits_test();
		//nalloc_dump_stats();
#ifdef LING_DEBUG
		gdb_break();
#endif
		return 1;
	}
	return 0;
}
#endif

static void console_int(uint32_t port, void *data)
{
	XENCONS_RING_IDX cons, prod;
	cons = console.intf->in_cons;
	prod = console.intf->in_prod;
	rmb();

	if (prod == cons)
		return;

	uint32_t in_size = prod - cons;
	uint8_t buf[in_size];
	uint8_t *ptr = buf;

	ssa(SYS_STATS_IO_INPUT, in_size);

	while (prod > cons)
	{
		int idx = MASK_XENCONS_IDX(cons++, console.intf->in);
#ifdef DEBUG_CONSOLE
		if (debug_key(console.intf->in[idx]))
		{
			in_size--;
			continue;
		}
#endif
		*ptr++ = console.intf->in[idx];
	}

	console.intf->in_cons = prod;
	wmb();

	if (console.attached)
		outlet_new_data(console.attached, buf, in_size);
}

void console_init(struct xencons_interface *intf, uint32_t chan)
{
	if (console.is_initialized)
		fatal_error("console_init: already initialized");

	console.intf = intf;
	console.attached = 0;
	console.chan = chan;
	
	event_bind(console.chan, console_int, 0);

	//
	// ECMA-48 modes:
	//
	// - set insert mode
	//
	// NB: mode changes not undone on shutdown
	//
	char modes[] = "\x1b[4h";
	console_write(modes, sizeof(modes) -1);

	console.is_initialized = true;
}

int console_is_initialized(void)
{
	return console.is_initialized;
}

void console_attach(outlet_t *ol)
{
	if (console.attached != 0)
		printk("*** Port %pt is stealing control over Xen console from %pt\n",
				T(ol->oid), T(console.attached->oid));
	console.attached = ol;
}

void console_detach(outlet_t *ol)
{
	if (console.attached == ol)
		console.attached = 0;
}

int console_write(char *msg, int len)
{
	static int was_cr = 0;

	int sent = 0;
	while (sent < len)
	{
		XENCONS_RING_IDX cons, prod;
		cons = console.intf->out_cons;
		rmb();
		prod = console.intf->out_prod;

//	while ((sent < len) && (prod - cons < sizeof(console.intf->out)))
//		console.intf->out[MASK_XENCONS_IDX(prod++, console.intf->out)] = msg[sent++];
//
// It may be possible to use stty or ESC sequence instead of this nastiness
// 

		while ((sent < len) && (prod - cons < sizeof(console.intf->out))) {
			if (msg[sent] == '\n' && !was_cr)
			{
				int idx = MASK_XENCONS_IDX(prod, console.intf->out);
				console.intf->out[idx] = '\r';
				prod++;
				if (prod - cons >= sizeof(console.intf->out))
					break;
			}
			was_cr = (msg[sent] == '\r');
			int idx = MASK_XENCONS_IDX(prod, console.intf->out);
			console.intf->out[idx] = msg[sent++];
			prod++;
		}

		console.intf->out_prod = prod;
		wmb();

		event_kick(console.chan);
	}
	ssa(SYS_STATS_IO_OUTPUT, len);

	// XXX: 0 is probably not the desired value
	return 0;
}

void console_done(void)
{
	// (attempt to) restore the terminal mode
	char modes[] = "\x1b[4l";
	console_write(modes, sizeof(modes) -1);

	while (console.intf->out_cons < console.intf->out_prod)
	{
		HYPERVISOR_sched_op(SCHEDOP_yield, 0);
		mb();
	}
}

/*EOF*/
