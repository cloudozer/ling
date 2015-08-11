/* salsa20-set-key.c
 *
 * The Salsa20 stream cipher.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2012 Simon Josefsson, Niels Möller
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

#include <assert.h>

#include "salsa20.h"

#include "macros.h"

void
salsa20_set_key(struct salsa20_ctx *ctx,
		unsigned length, const uint8_t *key)
{
  static const uint32_t sigma[4] = {
    /* "expand 32-byte k" */
    0x61707865, 0x3320646e, 0x79622d32, 0x6b206574
  };
  static const uint32_t tau[4] = {
    /* "expand 16-byte k" */
    0x61707865, 0x3120646e, 0x79622d36, 0x6b206574
  };
  const uint32_t *constants;
  
  assert (length == SALSA20_MIN_KEY_SIZE || length == SALSA20_MAX_KEY_SIZE);

  ctx->input[1] = LE_READ_UINT32(key + 0);
  ctx->input[2] = LE_READ_UINT32(key + 4);
  ctx->input[3] = LE_READ_UINT32(key + 8);
  ctx->input[4] = LE_READ_UINT32(key + 12);
  if (length == SALSA20_MAX_KEY_SIZE) { /* recommended */
    ctx->input[11] = LE_READ_UINT32(key + 16);
    ctx->input[12] = LE_READ_UINT32(key + 20);
    ctx->input[13] = LE_READ_UINT32(key + 24);
    ctx->input[14] = LE_READ_UINT32(key + 28);
    constants = sigma;
  } else { /* kbits == 128 */
    ctx->input[11] = ctx->input[1];
    ctx->input[12] = ctx->input[2];
    ctx->input[13] = ctx->input[3];
    ctx->input[14] = ctx->input[4];
    constants = tau;
  }
  ctx->input[0]  = constants[0];
  ctx->input[5]  = constants[1];
  ctx->input[10] = constants[2];
  ctx->input[15] = constants[3];
}

void
salsa20_set_iv(struct salsa20_ctx *ctx, const uint8_t *iv)
{
  ctx->input[6] = LE_READ_UINT32(iv + 0);
  ctx->input[7] = LE_READ_UINT32(iv + 4);
  ctx->input[8] = 0;
  ctx->input[9] = 0;
}
