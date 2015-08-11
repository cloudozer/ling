/* hmac-ripemd160.c
 *
 * HMAC-RIPEMD160 message authentication code.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2011 Niels Möller
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

#include "hmac.h"

void
hmac_ripemd160_set_key(struct hmac_ripemd160_ctx *ctx,
		       unsigned key_length, const uint8_t *key)
{
  HMAC_SET_KEY(ctx, &nettle_ripemd160, key_length, key);
}

void
hmac_ripemd160_update(struct hmac_ripemd160_ctx *ctx,
		      unsigned length, const uint8_t *data)
{
  ripemd160_update(&ctx->state, length, data);
}

void
hmac_ripemd160_digest(struct hmac_ripemd160_ctx *ctx,
		      unsigned length, uint8_t *digest)
{
  HMAC_DIGEST(ctx, &nettle_ripemd160, length, digest);
}
