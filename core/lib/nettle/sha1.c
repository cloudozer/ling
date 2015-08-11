/* sha1.c
 *
 * The sha1 hash function.
 * Defined by http://www.itl.nist.gov/fipspubs/fip180-1.htm.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2001 Peter Gutmann, Andrew Kuchling, Niels Möller
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

/* Here's the first paragraph of Peter Gutmann's posting,
 * <30ajo5$oe8@ccu2.auckland.ac.nz>: 
 *
 * The following is my SHA (FIPS 180) code updated to allow use of the "fixed"
 * SHA, thanks to Jim Gillogly and an anonymous contributor for the information on
 * what's changed in the new version.  The fix is a simple change which involves
 * adding a single rotate in the initial expansion function.  It is unknown
 * whether this is an optimal solution to the problem which was discovered in the
 * SHA or whether it's simply a bandaid which fixes the problem with a minimum of
 * effort (for example the reengineering of a great many Capstone chips).
 */

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "sha1.h"

#include "macros.h"
#include "nettle-write.h"

/* Initialize the SHA values */
void
sha1_init(struct sha1_ctx *ctx)
{
  /* FIXME: Put the buffer last in the struct, and arrange so that we
     can initialize with a single memcpy. */
  static const uint32_t iv[_SHA1_DIGEST_LENGTH] = 
    {
      /* SHA initial values */
      0x67452301L,
      0xEFCDAB89L,
      0x98BADCFEL,
      0x10325476L,
      0xC3D2E1F0L,
    };

  memcpy(ctx->state, iv, sizeof(ctx->state));
  ctx->count_low = ctx->count_high = 0;
  
  /* Initialize buffer */
  ctx->index = 0;
}

#define COMPRESS(ctx, data) (_nettle_sha1_compress((ctx)->state, data))

void
sha1_update(struct sha1_ctx *ctx,
	    unsigned length, const uint8_t *data)
{
  MD_UPDATE (ctx, length, data, COMPRESS, MD_INCR(ctx));
}
	  
void
sha1_digest(struct sha1_ctx *ctx,
	    unsigned length,
	    uint8_t *digest)
{
  uint32_t high, low;

  assert(length <= SHA1_DIGEST_SIZE);

  MD_PAD(ctx, 8, COMPRESS);

  /* There are 512 = 2^9 bits in one block */  
  high = (ctx->count_high << 9) | (ctx->count_low >> 23);
  low = (ctx->count_low << 9) | (ctx->index << 3);

  /* append the 64 bit count */
  WRITE_UINT32(ctx->block + (SHA1_DATA_SIZE - 8), high);
  WRITE_UINT32(ctx->block + (SHA1_DATA_SIZE - 4), low);
  _nettle_sha1_compress(ctx->state, ctx->block);

  _nettle_write_be32(length, digest, ctx->state);
  sha1_init(ctx);
}
