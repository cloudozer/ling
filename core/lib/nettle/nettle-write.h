/* nettle-write.h
 *
 * Prototypes for some internal functions to write out word-sized data
 * to byte arrays. */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2010 Niels Möller
 *  
 * The nettle library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * The nettle library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with the nettle library; see the file COPYING.LIB.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301, USA.
 */

#ifndef NETTLE_WRITE_H_INCLUDED
#define NETTLE_WRITE_H_INCLUDED

#include "nettle-stdint.h"

/* Write the word array at SRC to the byte array at DST, using little
   endian (le) or big endian (be) byte order, and truncating the
   result to LENGTH bytes. */

/* FIXME: Use a macro shortcut to memcpy for native endianness. */
void
_nettle_write_be32(unsigned length, uint8_t *dst,
		   uint32_t *src);
void
_nettle_write_le32(unsigned length, uint8_t *dst,
		   uint32_t *src);

void
_nettle_write_le64(unsigned length, uint8_t *dst,
		   uint64_t *src);

#endif /* NETTLE_WRITE_H_INCLUDED */
