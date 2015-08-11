/* base16-encode.c
 *
 * Hex encoding.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2002 Niels Möller
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
 
#if HAVE_CONFIG_H
# include "config.h"
#endif

#include "base16.h"


static const uint8_t
hex_digits[16] = "0123456789abcdef";

#define DIGIT(x) (hex_digits[(x) & 0xf])

/* Encodes a single byte. Always stores two digits in dst[0] and dst[1]. */
void
base16_encode_single(uint8_t *dst,
		     uint8_t src)
{
  dst[0] = DIGIT(src/0x10);
  dst[1] = DIGIT(src);
}

/* Always stores BASE16_ENCODE_LENGTH(length) digits in dst. */
void
base16_encode_update(uint8_t *dst,
		     unsigned length,
		     const uint8_t *src)
{
  unsigned i;
  
  for (i = 0, dst; i<length; i++, dst += 2)
    base16_encode_single(dst, src[i]);
}
