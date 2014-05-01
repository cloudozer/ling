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

#include "outlet.h"

#include "lwip/err.h"

#include "decode.h"

#define INET_INFINITY			0xffffffff

// request codes (erlang:port_control/3)
#define INET_REQ_OPEN          1
#define INET_REQ_CLOSE         2
#define INET_REQ_CONNECT       3
#define INET_REQ_PEER          4
#define INET_REQ_NAME          5
#define INET_REQ_BIND          6
#define INET_REQ_SETOPTS       7
#define INET_REQ_GETOPTS       8
#define INET_REQ_GETIX         9
// #define INET_REQ_GETIF      10 OBSOLETE
#define INET_REQ_GETSTAT       11
#define INET_REQ_GETHOSTNAME   12
#define INET_REQ_FDOPEN        13
#define INET_REQ_GETFD         14
#define INET_REQ_GETTYPE       15
#define INET_REQ_GETSTATUS     16
#define INET_REQ_GETSERVBYNAME 17
#define INET_REQ_GETSERVBYPORT 18
#define INET_REQ_SETNAME       19
#define INET_REQ_SETPEER       20
#define INET_REQ_GETIFLIST     21
#define INET_REQ_IFGET         22
#define INET_REQ_IFSET         23
#define INET_REQ_SUBSCRIBE     24
#define INET_REQ_GETIFADDRS    25
#define INET_REQ_ACCEPT        26
#define INET_REQ_LISTEN        27
#define INET_REQ_IGNOREFD      28

// TCP requests
//#define TCP_REQ_ACCEPT         40 MOVED
//#define TCP_REQ_LISTEN         41 MERGED
#define TCP_REQ_RECV           42
#define TCP_REQ_UNRECV         43
#define TCP_REQ_SHUTDOWN       44
// UDP and SCTP requests
#define PACKET_REQ_RECV        60
//#define SCTP_REQ_LISTEN        61 MERGED
#define SCTP_REQ_BINDX	       62 // Multi-home SCTP bind
#define SCTP_REQ_PEELOFF       63

// family codes to open
#define INET_AF_INET         1
#define INET_AF_INET6        2
#define INET_AF_ANY          3 // Fake for ANY in any address family
#define INET_AF_LOOPBACK     4 // Fake for LOOPBACK in any address family

// type codes to open and gettype - INET_REQ_GETTYPE
#define INET_TYPE_STREAM     1
#define INET_TYPE_DGRAM      2
#define INET_TYPE_SEQPACKET  3

// reply codes for *_REQ_*
#define INET_REP_ERROR    0
#define INET_REP_OK       1
#define INET_REP          2

// INET TCP and UDP options:
#define INET_OPT_REUSEADDR      0
#define INET_OPT_KEEPALIVE      1
#define INET_OPT_DONTROUTE      2
#define INET_OPT_LINGER         3
#define INET_OPT_BROADCAST      4
#define INET_OPT_OOBINLINE      5
#define INET_OPT_SNDBUF         6
#define INET_OPT_RCVBUF         7
#define INET_OPT_PRIORITY       8
#define INET_OPT_TOS            9
#define TCP_OPT_NODELAY         10
#define UDP_OPT_MULTICAST_IF    11
#define UDP_OPT_MULTICAST_TTL   12
#define UDP_OPT_MULTICAST_LOOP  13
#define UDP_OPT_ADD_MEMBERSHIP  14
#define UDP_OPT_DROP_MEMBERSHIP 15
// "Local" options: codes start from 20:
#define INET_LOPT_BUFFER        20
#define INET_LOPT_HEADER        21
#define INET_LOPT_ACTIVE        22
#define INET_LOPT_PACKET        23
#define INET_LOPT_MODE          24
#define INET_LOPT_DELIVER       25
#define INET_LOPT_EXITONCLOSE   26
#define INET_LOPT_TCP_HIWTRMRK  27
#define INET_LOPT_TCP_LOWTRMRK  28
#define INET_LOPT_BIT8          29
#define INET_LOPT_TCP_SEND_TIMEOUT 30
#define INET_LOPT_TCP_DELAY_SEND   31
#define INET_LOPT_PACKET_SIZE   32
#define INET_LOPT_READ_PACKETS  33
#define INET_OPT_RAW            34
#define INET_LOPT_TCP_SEND_TIMEOUT_CLOSE 35

// active socket, INET_LOPT_ACTIVE
#define INET_PASSIVE		0
#define INET_ACTIVE			1
#define INET_ONCE			2	// Active once then passive

// socket modes, INET_LOPT_MODE
#define INET_MODE_LIST		0
#define INET_MODE_BINARY	1

// deliver mode, INET_LOPT_DELIVER
#define INET_DELIVER_PORT	0
#define INET_DELIVER_TERM	1

// subscribe codes, INET_REQ_SUBSCRIBE
#define INET_SUBS_EMPTY_OUT_Q	1

// interface options, INET_REQ_IFGET and INET_REQ_IFSET
#define INET_IFOPT_ADDR      1
#define INET_IFOPT_BROADADDR 2
#define INET_IFOPT_DSTADDR   3
#define INET_IFOPT_MTU       4
#define INET_IFOPT_NETMASK   5
#define INET_IFOPT_FLAGS     6
#define INET_IFOPT_HWADDR    7 // where supported (e.g linux)

// interface stuff, INET_IFOPT_FLAGS
#define INET_IFF_UP            0x0001
#define INET_IFF_BROADCAST     0x0002
#define INET_IFF_LOOPBACK      0x0004
#define INET_IFF_POINTTOPOINT  0x0008
#define INET_IFF_RUNNING       0x0010
#define INET_IFF_MULTICAST     0x0020
//
#define INET_IFF_DOWN          0x0100
#define INET_IFF_NBROADCAST    0x0200
#define INET_IFF_NPOINTTOPOINT 0x0800

// getstat, INET_REQ_GETSTAT
#define INET_STAT_RECV_CNT		1
#define INET_STAT_RECV_MAX		2
#define INET_STAT_RECV_AVG		3
#define INET_STAT_RECV_DVI		4
#define INET_STAT_SEND_CNT		5
#define INET_STAT_SEND_MAX		6
#define INET_STAT_SEND_AVG		7
#define INET_STAT_SEND_PEND		8
#define INET_STAT_RECV_OCT		9
#define INET_STAT_SEND_OCT		10

// Raw socket interface
#define VIF_REQ_OPEN	100

#define VIF_REP_ERROR   0
#define VIF_REP_OK      1

void inet_set_default_opts(outlet_t *ol);
int inet_set_opt(outlet_t *ol, int opt, uint32_t val);
int inet_get_opt(outlet_t *ol, int opt, uint32_t *val);

void inet_async(term_t oid, term_t reply_to, uint16_t ref, term_t reply);
void inet_async2(term_t oid, term_t reply_to, uint16_t ref, term_t a, term_t b);
void inet_async_error(term_t oid, term_t reply_to, uint16_t ref, term_t err);

void inet_reply(term_t oid, term_t reply_ty, term_t reply);
void inet_reply_error(term_t oid, term_t reply_to, term_t reason);

term_t lwip_err_to_term(err_t err);

//EOF
