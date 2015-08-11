/* md4.h
 *
 * The MD4 hash function, described in RFC 1320.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2003 Niels Möller, Marcus Comstedt
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

/* Based on the public domain md5 code, and modified by Marcus
   Comstedt */

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>
#include <string.h>

#include "md4.h"

#include "macros.h"
#include "nettle-write.h"

/* A block, treated as a sequence of 32-bit words. */
#define MD4_DATA_LENGTH 16

static void
md4_transform(uint32_t *digest, const uint32_t *data);

static void
md4_compress(struct md4_ctx *ctx, const uint8_t *block);

/* FIXME: Could be an alias for md5_init */
void
md4_init(struct md4_ctx *ctx)
{
  /* Same constants as for md5. */
  const uint32_t iv[_MD4_DIGEST_LENGTH] =
    {
      0x67452301,
      0xefcdab89,
      0x98badcfe,
      0x10325476,
    };
  memcpy(ctx->state, iv, sizeof(ctx->state));
  
  ctx->count_low = ctx->count_high = 0;
  ctx->index = 0;
}

void
md4_update(struct md4_ctx *ctx,
	   unsigned length,
	   const uint8_t *data)
{
  MD_UPDATE(ctx, length, data, md4_compress, MD_INCR(ctx));
}

void
md4_digest(struct md4_ctx *ctx,
	   unsigned length,
	   uint8_t *digest)
{
  uint32_t data[MD4_DATA_LENGTH];
  unsigned i;

  assert(length <= MD4_DIGEST_SIZE);

  MD_PAD(ctx, 8, md4_compress);
  for (i = 0; i < MD4_DATA_LENGTH - 2; i++)
    data[i] = LE_READ_UINT32(ctx->block + 4*i);

  /* There are 512 = 2^9 bits in one block 
   * Little-endian order => Least significant word first */

  data[MD4_DATA_LENGTH-1] = (ctx->count_high << 9) | (ctx->count_low >> 23);
  data[MD4_DATA_LENGTH-2] = (ctx->count_low << 9) | (ctx->index << 3);
  md4_transform(ctx->state, data);

  _nettle_write_le32(length, digest, ctx->state);
  md4_init(ctx);
}

/* MD4 functions */
#define F(x, y, z) (((y) & (x)) | ((z) & ~(x)))
#define G(x, y, z) (((y) & (x)) | ((z) & (x)) | ((y) & (z)))
#define H(x, y, z) ((x) ^ (y) ^ (z))

#define ROUND(f, w, x, y, z, data, s) \
( w += f(x, y, z) + data,  w = w<<s | w>>(32-s) )

/* Perform the MD4 transformation on one full block of 16 32-bit words. */
   
static void
md4_transform(uint32_t *digest, const uint32_t *data)
{
  uint32_t a, b, c, d;
  a = digest[0];
  b = digest[1];
  c = digest[2];
  d = digest[3];

  ROUND(F, a, b, c, d, data[ 0], 3);
  ROUND(F, d, a, b, c, data[ 1], 7);
  ROUND(F, c, d, a, b, data[ 2], 11);
  ROUND(F, b, c, d, a, data[ 3], 19);
  ROUND(F, a, b, c, d, data[ 4], 3);
  ROUND(F, d, a, b, c, data[ 5], 7);
  ROUND(F, c, d, a, b, data[ 6], 11);
  ROUND(F, b, c, d, a, data[ 7], 19);
  ROUND(F, a, b, c, d, data[ 8], 3);
  ROUND(F, d, a, b, c, data[ 9], 7);
  ROUND(F, c, d, a, b, data[10], 11);
  ROUND(F, b, c, d, a, data[11], 19);
  ROUND(F, a, b, c, d, data[12], 3);
  ROUND(F, d, a, b, c, data[13], 7);
  ROUND(F, c, d, a, b, data[14], 11);
  ROUND(F, b, c, d, a, data[15], 19);

  ROUND(G, a, b, c, d, data[ 0] + 0x5a827999, 3);
  ROUND(G, d, a, b, c, data[ 4] + 0x5a827999, 5);
  ROUND(G, c, d, a, b, data[ 8] + 0x5a827999, 9);
  ROUND(G, b, c, d, a, data[12] + 0x5a827999, 13);
  ROUND(G, a, b, c, d, data[ 1] + 0x5a827999, 3);
  ROUND(G, d, a, b, c, data[ 5] + 0x5a827999, 5);
  ROUND(G, c, d, a, b, data[ 9] + 0x5a827999, 9);
  ROUND(G, b, c, d, a, data[13] + 0x5a827999, 13);
  ROUND(G, a, b, c, d, data[ 2] + 0x5a827999, 3);
  ROUND(G, d, a, b, c, data[ 6] + 0x5a827999, 5);
  ROUND(G, c, d, a, b, data[10] + 0x5a827999, 9);
  ROUND(G, b, c, d, a, data[14] + 0x5a827999, 13);
  ROUND(G, a, b, c, d, data[ 3] + 0x5a827999, 3);
  ROUND(G, d, a, b, c, data[ 7] + 0x5a827999, 5);
  ROUND(G, c, d, a, b, data[11] + 0x5a827999, 9);
  ROUND(G, b, c, d, a, data[15] + 0x5a827999, 13);

  ROUND(H, a, b, c, d, data[ 0] + 0x6ed9eba1, 3);
  ROUND(H, d, a, b, c, data[ 8] + 0x6ed9eba1, 9);
  ROUND(H, c, d, a, b, data[ 4] + 0x6ed9eba1, 11);
  ROUND(H, b, c, d, a, data[12] + 0x6ed9eba1, 15);
  ROUND(H, a, b, c, d, data[ 2] + 0x6ed9eba1, 3);
  ROUND(H, d, a, b, c, data[10] + 0x6ed9eba1, 9);
  ROUND(H, c, d, a, b, data[ 6] + 0x6ed9eba1, 11);
  ROUND(H, b, c, d, a, data[14] + 0x6ed9eba1, 15);
  ROUND(H, a, b, c, d, data[ 1] + 0x6ed9eba1, 3);
  ROUND(H, d, a, b, c, data[ 9] + 0x6ed9eba1, 9);
  ROUND(H, c, d, a, b, data[ 5] + 0x6ed9eba1, 11);
  ROUND(H, b, c, d, a, data[13] + 0x6ed9eba1, 15);
  ROUND(H, a, b, c, d, data[ 3] + 0x6ed9eba1, 3);
  ROUND(H, d, a, b, c, data[11] + 0x6ed9eba1, 9);
  ROUND(H, c, d, a, b, data[ 7] + 0x6ed9eba1, 11);
  ROUND(H, b, c, d, a, data[15] + 0x6ed9eba1, 15);

  digest[0] += a;
  digest[1] += b;
  digest[2] += c;
  digest[3] += d;
}

static void
md4_compress(struct md4_ctx *ctx, const uint8_t *block)
{
  uint32_t data[MD4_DATA_LENGTH];
  unsigned i;
  
  /* Endian independent conversion */
  for (i = 0; i<16; i++, block += 4)
    data[i] = LE_READ_UINT32(block);

  md4_transform(ctx->state, data);
}
