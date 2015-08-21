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

#include "bif.h"

#include "ling_common.h"

#include <math.h>
#include <stdlib.h>

#include <nettle/md5.h>
#include <nettle/sha.h>
#include <nettle/hmac.h>
#include <nettle/aes.h>
#include <nettle/cbc.h>
#include <nettle/ctr.h>

#include "crc32.h"
#include "adler32.h"

#include "mm.h"
#include "atom_defs.h"
#include "bits.h"
#include "getput.h"
#include "mixed.h"
#include "term_util.h"
#include "list_util.h"
#include "map_util.h"
#include "unicode.h"
#include "catch_tab.h"

#include "atoms.h"
#include "ext_term.h"
#include "time.h"
#include "scheduler.h"
#include "code_base.h"
#include "string.h"
#include "strings.h"
#include "bignum.h"
#include "snprintf.h"
#include "stringify.h"
#include "hash.h"
#include "cluster.h"
#include "decode.h"

#include "event.h"

#include "monitors.h"
#include "timers.h"
#include "console.h"
#include "netfe.h"
#include "ser_cons.h"
#include "timers.h"
#include "ets.h"
#include "counters.h"
#include "embed.h"
#include "mtwist.h"
#include "prof.h"

#include "outlet.h"
#include "netif.h"

#include "disk.h"

#ifdef LING_XEN
# include "xenstore.h"
# include "xen/io/xs_wire.h"
# include "pore.h"
#endif

#ifdef LING_WITH_LWIP
#include "lwip/ip_addr.h"
#include "lwip/stats.h"
#include "lwip/netif.h"
#endif

#define fail(reason) do { \
	proc->bif_excep_reason = (reason); \
	return noval; \
} while (0)

#define bif_not_implemented() do { \
	proc->bif_excep_reason = A_NOT_IMPLEMENTED; \
	return noval; \
} while (0)

#define badarg(arg) do { \
	proc->bif_excep_reason = A_BADARG; \
	return noval; \
} while (0)

