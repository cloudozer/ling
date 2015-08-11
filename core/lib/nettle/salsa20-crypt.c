/* salsa20-crypt.c
 *
 * The Salsa20 stream cipher.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2012 Simon Josefsson
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

/* Based on:
   salsa20-ref.c version 20051118
   D. J. Bernstein
   Public domain.
*/

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <string.h>

#include "salsa20.h"

#include "macros.h"
#include "memxor.h"

void
salsa20_crypt(struct salsa20_ctx *ctx,
	      unsigned length,
	      uint8_t *c,
	      const uint8_t *m)
{
  if (!length)
    return;
  
  for (;;)
    {
      uint32_t x[_SALSA20_INPUT_LENGTH];

      _salsa20_core (x, ctx->input, 20);

      ctx->input[9] += (++ctx->input[8] == 0);

      /* stopping at 2^70 length per nonce is user's responsibility */
      
      if (length <= SALSA20_BLOCK_SIZE)
	{
	  memxor3 (c, m, (uint8_t *) x, length);
	  return;
	}
      memxor3 (c, m, (uint8_t *) x, SALSA20_BLOCK_SIZE);

      length -= SALSA20_BLOCK_SIZE;
      c += SALSA20_BLOCK_SIZE;
      m += SALSA20_BLOCK_SIZE;
  }
}
