/* umac128.c
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2013 Niels Möller
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
#include <string.h>

#include "umac.h"

#include "macros.h"

void
umac128_set_key (struct umac128_ctx *ctx, const uint8_t *key)
{
  _umac_set_key (ctx->l1_key, ctx->l2_key, ctx->l3_key1, ctx->l3_key2,
		 &ctx->pdf_key, key, 4);

  /* Clear nonce */
  memset (ctx->nonce, 0, sizeof(ctx->nonce));
  ctx->nonce_length = sizeof(ctx->nonce);

  /* Initialize buffer */
  ctx->count = ctx->index = 0;
}

void
umac128_set_nonce (struct umac128_ctx *ctx,
		  unsigned nonce_length, const uint8_t *nonce)
{
  assert (nonce_length > 0);
  assert (nonce_length <= AES_BLOCK_SIZE);

  memcpy (ctx->nonce, nonce, nonce_length);
  memset (ctx->nonce + nonce_length, 0, AES_BLOCK_SIZE - nonce_length);

  ctx->nonce_length = nonce_length;
}

#define UMAC128_BLOCK(ctx, block) do {					\
    uint64_t __umac128_y[4];						\
    _umac_nh_n (__umac128_y, 4, ctx->l1_key, UMAC_DATA_SIZE, block);	\
    __umac128_y[0] += 8*UMAC_DATA_SIZE;					\
    __umac128_y[1] += 8*UMAC_DATA_SIZE;					\
    __umac128_y[2] += 8*UMAC_DATA_SIZE;					\
    __umac128_y[3] += 8*UMAC_DATA_SIZE;					\
    _umac_l2 (ctx->l2_key, ctx->l2_state, 4, ctx->count++, __umac128_y); \
  } while (0)

void
umac128_update (struct umac128_ctx *ctx,
	       unsigned length, const uint8_t *data)
{
  MD_UPDATE (ctx, length, data, UMAC128_BLOCK, (void)0);
}


void
umac128_digest (struct umac128_ctx *ctx,
	       unsigned length, uint8_t *digest)
{
  uint32_t tag[4];
  unsigned i;

  assert (length > 0);
  assert (length <= 16);

  if (ctx->index > 0 || ctx->count == 0)
    {
      /* Zero pad to multiple of 32 */
      uint64_t y[4];
      unsigned pad = (ctx->index > 0) ? 31 & - ctx->index : 32;
      memset (ctx->block + ctx->index, 0, pad);

      _umac_nh_n (y, 4, ctx->l1_key, ctx->index + pad, ctx->block);
      y[0] += 8 * ctx->index;
      y[1] += 8 * ctx->index;
      y[2] += 8 * ctx->index;
      y[3] += 8 * ctx->index;
      _umac_l2 (ctx->l2_key, ctx->l2_state, 4, ctx->count++, y);
    }
  assert (ctx->count > 0);

  aes_encrypt (&ctx->pdf_key, AES_BLOCK_SIZE,
	       (uint8_t *) tag, ctx->nonce);

  INCREMENT (ctx->nonce_length, ctx->nonce);

  _umac_l2_final (ctx->l2_key, ctx->l2_state, 4, ctx->count);
  for (i = 0; i < 4; i++)
    tag[i] ^= ctx->l3_key2[i] ^ _umac_l3 (ctx->l3_key1 + 8*i,
					  ctx->l2_state + 2*i);

  memcpy (digest, tag, length);

  /* Reinitialize */
  ctx->count = ctx->index = 0;
}
