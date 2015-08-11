/* arcfour.c
 *
 * The arcfour/rc4 stream cipher.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2001 Niels Möller
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

#define SWAP(a,b) do { int _t = a; a = b; b = _t; } while(0)

void
arcfour_set_key(struct arcfour_ctx *ctx,
		unsigned length, const uint8_t *key)
{
  unsigned i, j, k;
  
  assert(length >= ARCFOUR_MIN_KEY_SIZE);
  assert(length <= ARCFOUR_MAX_KEY_SIZE);

  /* Initialize context */
  for (i = 0; i<256; i++)
    ctx->S[i] = i;

  for (i = j = k = 0; i<256; i++)
    {
      j += ctx->S[i] + key[k]; j &= 0xff;
      SWAP(ctx->S[i], ctx->S[j]);
      /* Repeat key as needed */
      k = (k + 1) % length;
    }
  ctx->i = ctx->j = 0;
}

