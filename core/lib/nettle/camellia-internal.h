/* camellia-internal.h
 *
 * The camellia block cipher.
 */

/* Copyright (C) 2006,2007
 * NTT (Nippon Telegraph and Telephone Corporation).
 *
 * Copyright (C) 2010 Niels Möller
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

/*
 * Algorithm Specification 
 *  http://info.isl.ntt.co.jp/crypt/eng/camellia/specifications.html
 */

/* Based on camellia.c ver 1.2.0, see
   http://info.isl.ntt.co.jp/crypt/eng/camellia/dl/camellia-LGPL-1.2.0.tar.gz.
 */

#ifndef NETTLE_CAMELLIA_INTERNAL_H_INCLUDED
#define NETTLE_CAMELLIA_INTERNAL_H_INCLUDED

#include "camellia.h"

/* Name mangling */
#define _camellia_crypt _nettle_camellia_crypt
#define _camellia_table _nettle_camellia_table

/*
 *  macros
 */

/* Destructive rotation of 128 bit values. */
#define ROTL128(bits, xl, xr) do {		\
    uint64_t __rol128_t = (xl);			     \
    (xl) = ((xl) << (bits)) | ((xr) >> (64 - (bits)));	   \
    (xr) = ((xr) << (bits)) | (__rol128_t >> (64 - (bits)));	\
  } while (0)

struct camellia_table
{
  uint32_t sp1110[256];
  uint32_t sp0222[256];
  uint32_t sp3033[256];
  uint32_t sp4404[256];
};

void
_camellia_crypt(const struct camellia_ctx *ctx,
		const struct camellia_table *T,
		unsigned length, uint8_t *dst,
		const uint8_t *src);

extern const struct camellia_table _camellia_table;

#endif /* NETTLE_CAMELLIA_INTERNAL_H_INCLUDED */
