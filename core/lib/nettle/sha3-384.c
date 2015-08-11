/* sha3-384.c
 *
 * The sha3 hash function, 384 bit output.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2012 Niels Möller
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

#include <stddef.h>
#include <string.h>

#include "sha3.h"

#include "nettle-write.h"

void
sha3_384_init (struct sha3_384_ctx *ctx)
{
  memset (&ctx->state, 0, offsetof (struct sha3_384_ctx, block));
}

void
sha3_384_update (struct sha3_384_ctx *ctx,
		 unsigned length,
		 const uint8_t *data)
{
  ctx->index = _sha3_update (&ctx->state, SHA3_384_DATA_SIZE, ctx->block,
			     ctx->index, length, data);
}

void
sha3_384_digest(struct sha3_384_ctx *ctx,
		unsigned length,
		uint8_t *digest)
{
  _sha3_pad (&ctx->state, SHA3_384_DATA_SIZE, ctx->block, ctx->index);
  _nettle_write_le64 (length, digest, ctx->state.a);
  sha3_384_init (ctx);
}
