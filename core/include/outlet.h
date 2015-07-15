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

#pragma once

//
//
//

#include "term.h"
#include "proc.h"

#include "netfe.h"

#if LING_WITH_LIBUV
# include <uv.h>
#endif

#define PB_DEFAULT		3

#define PB_INOUT_OFF	0
#define PB_INOUT_SIZE	2
#define PB_BINARY_OFF	2
#define PB_BINARY_SIZE	1
#define PB_EOF_OFF		3
#define PB_EOF_SIZE		1
#define PB_PACKET_OFF	4
#define PB_PACKET_SIZE	4
#define PB_LINE_OFF		8
#define PB_LINE_SIZE	10

#define PB_VALUE(bits, off, sz) \
	(((bits) >> (off)) & ((1 << (sz)) -1))


#ifndef OUTLET_DEFINED
typedef struct outlet_t outlet_t;
#define OUTLET_DEFINED 1
#endif

typedef struct outlet_vtab_t outlet_vtab_t;
struct outlet_vtab_t {
	uint8_t *(*get_send_buffer)(outlet_t *ol, int len);
	int (*send)(outlet_t *ol, int len, term_t reply_to);
	void (*new_data)(outlet_t *ol, uint8_t *data, int dlen);
	term_t (*control)(outlet_t *ol, uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp);
	int (*attach)(outlet_t *ol);
	void (*detach)(outlet_t *ol);
	void (*destroy_private)(outlet_t *ol);
};

#if LING_WITH_LWIP
typedef struct acc_pend_t acc_pend_t;
struct acc_pend_t {
	outlet_t *outlet;
union {
	// an acceptor process that called gen_tcp:accept()
	struct {
		term_t reply_to;
		int timeout_set;
	};
	// an accepted TCP connection waiting for an accept() call
	struct {
		struct tcp_pcb *pcb;
		struct pbuf *ante;
	};
};
	acc_pend_t *prev;
	acc_pend_t *next;
};
#endif

typedef struct disk_req_t disk_req_t;
struct disk_req_t {
	outlet_t *outlet;
	uint16_t async_tag;
	uint32_t num_sectors;
	term_t reply_to;
	disk_req_t **ref;
	disk_req_t *next;
};

struct outlet_t {
	term_t oid;
	term_t name;

	outlet_vtab_t *vtab;

	term_t owner; // pid

	inter_links_t links;

	term_t data; // a sandbox for port_set_data()/port_get_data()

	memnode_t *home_node;
	
	outlet_t **ref;
	outlet_t *next;

	// Options
	uint32_t inout;		// 1, 2 or 3
	uint32_t binary;	// 1 or 0
	uint32_t packet;	// 0, 1, 2, 4, or TCP_PB_*
	uint32_t line;		// 0 or 1-1023
	uint32_t eof;		// 1 or 0
	
	// Go away silently or notify the owner with {Port,closed}
	uint32_t notify_on_close;

#if LING_WITH_LWIP
	union {
		struct ip_pcb *ip;
		struct udp_pcb *udp;
		struct tcp_pcb *tcp;
	};
#elif LING_WITH_LIBUV
    int family;             // INET_AF_INET | INET_AF_INET6
    union {
        uv_udp_t *udp_conn;
    };
#endif

	// More options
	int active;				// INET_ACTIVE* | INET_PASSIVE | INET_ONCE
	int deliver;			// INET_DELIVER_PORT | INET_DELIVER_TERM*
	int buffer;				// ignore
	int header;				// ignore
	int packet_size;		//
	int exit_on_close;		// 1* | 0

	// Non-standard option
	int max_mq_len;			// maximum message queue length

	// TCP - connection mode
	int cr_in_progress;
	term_t cr_reply_to;
	int cr_timeout_set;
	int send_in_progress;
	uint32_t max_send_bufsize;	// ol_console, ol_dns
	uint8_t *send_buffer;		// ol_console, ol_dns
	memnode_t *send_buf_node;
	int send_buf_ack;
	int send_buf_off;
	int send_buf_left;			// ol_console, ol_dns
	term_t send_reply_to;
	int send_timeout_set;
	uint32_t send_timeout;		// SO_SNDTIMEO
	uint32_t recv_expected_size;
	uint32_t recv_bufsize;
	uint32_t max_recv_bufsize;
	uint8_t *recv_buffer;
	memnode_t *recv_buf_node;
	int recv_buf_off;
	term_t empty_queue_in_progress;
	term_t empty_queue_reply_to;
	int peer_close_detected;

#if LING_WITH_LWIP
	// TCP - listening mode
	int backlog;
	acc_pend_t *free_pends;
	memnode_t *pend_nodes;
	acc_pend_t *accepting;
	acc_pend_t *accepted;
#endif

	// Disk
	disk_req_t *out_reqs; 
	disk_req_t *free_reqs;

	// Console
	int unicode_state;	// 0 or 1

	// VIF
	netfe_t *front_end;
};

typedef outlet_t *(*outlet_factory_func_t)(proc_t *cont_proc, uint32_t bit_opts);
outlet_factory_func_t outlet_resolve_driver(term_t name);

outlet_t *outlet_make_N(outlet_vtab_t *vtab, proc_t *cont_proc, int32_t bit_opts, uint32_t extra);

uint8_t *outlet_get_send_buffer(outlet_t *ol, int len);
int outlet_send(outlet_t *ol, int len, term_t reply_to);
void outlet_new_data(outlet_t *ol, uint8_t *data, int dlen);
term_t outlet_control(outlet_t *ol, uint32_t op, uint8_t *data, int dlen, term_t reply_to, heap_t *hp);
int outlet_attach(outlet_t *ol);
void outlet_detach(outlet_t *ol);
void outlet_destroy_private(outlet_t *ol);

outlet_t *outlet_lookup(term_t oid);
outlet_t *outlet_lookup_by_name(term_t name);
term_t outlet_all(heap_t *hp);

void outlet_register(outlet_t *ol, term_t name);
void outlet_unregister(outlet_t *ol);

void outlet_pass_new_data(outlet_t *ol, uint8_t *data, int dlen);

int outlet_signal_exit_N(outlet_t *ol, term_t src, term_t reason);
void outlet_close(outlet_t *ol, term_t reason);
void outlet_destroy(outlet_t *ol);
int outlet_notify_owner(outlet_t *ol, term_t what);

//EOF
