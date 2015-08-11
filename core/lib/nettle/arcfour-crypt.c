/* arcfour-crypt.c
 *
 * The arcfour/rc4 stream cipher.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2001, 2004 Niels Möller
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

#include <assert.h>

#include "arcfour.h"

void
arcfour_crypt(struct arcfour_ctx *ctx,
	      unsigned length, uint8_t *dst,
	      const uint8_t *src)
{
  register uint8_t i, j;
  register int si, sj;

  i = ctx->i; j = ctx->j;
  while(length--)
    {
      i++; i &= 0xff;
      si = ctx->S[i];
      j += si; j &= 0xff;
      sj = ctx->S[i] = ctx->S[j];
      ctx->S[j] = si;
      *dst++ = *src++ ^ ctx->S[ (si + sj) & 0xff ];
    }
  ctx->i = i; ctx->j = j;
}
