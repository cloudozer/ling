/* knuth-lfib.c
 *
 * A "lagged fibonacci" pseudorandomness generator.
 *
 * Described in Knuth, TAOCP, 3.6
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2002 Niels Möller
 *
 * Includes code copied verbatim from Knuth's TAOCP.
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

/* NOTE: This generator is totally inappropriate for cryptographic
 * applications. It is useful for generating deterministic but
 * random-looking test data, and is used by the Nettle testsuite. */

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>
#include <stdlib.h>

#include "knuth-lfib.h"

#include "macros.h"

#define KK _KNUTH_LFIB_KK
#define LL 37
#define MM (1UL << 30)
#define TT 70

void
knuth_lfib_init(struct knuth_lfib_ctx *ctx, uint32_t seed)
{
  uint32_t t,j;
  uint32_t x[2*KK - 1];
  uint32_t ss = (seed + 2) & (MM-2);

  for (j = 0; j<KK; j++)
    {
      x[j] = ss;
      ss <<= 1;  if (ss >= MM) ss -= (MM-2);
    }
  for (;j< 2*KK-1; j++)
    x[j] = 0;

  x[1]++;

  ss = seed & (MM-1);
  for (t = TT-1; t; )
    {
      for (j = KK-1; j>0; j--)
        x[j+j] = x[j];
      for (j = 2*KK-2; j > KK-LL; j-= 2)
        x[2*KK-1-j] = x[j] & ~1;
      for (j = 2*KK-2; j>=KK; j--)
        if (x[j] & 1)
          {
            x[j-(KK-LL)] = (x[j - (KK-LL)] - x[j]) & (MM-1);
            x[j-KK] = (x[j-KK] - x[j]) & (MM-1);
          }
      if (ss & 1)
        {
          for (j=KK; j>0; j--)
            x[j] = x[j-1];
          x[0] = x[KK];
          if (x[KK] & 1)
            x[LL] = (x[LL] - x[KK]) & (MM-1);
        }
      if (ss)
        ss >>= 1;
      else
        t--;
    }
  for (j=0; j<LL; j++)
    ctx->x[j+KK-LL] = x[j];
  for (; j<KK; j++)
    ctx->x[j-LL] = x[j];

  ctx->index = 0;
}     

/* Get's a single number in the range 0 ... 2^30-1 */
uint32_t
knuth_lfib_get(struct knuth_lfib_ctx *ctx)
{
  uint32_t value;
  assert(ctx->index < KK);
  
  value = ctx->x[ctx->index];
  ctx->x[ctx->index] -= ctx->x[(ctx->index + KK - LL) % KK];
  ctx->x[ctx->index] &= (MM-1);
  
  ctx->index = (ctx->index + 1) % KK;

  return value;
} 

/* NOTE: Not at all optimized. */
void
knuth_lfib_get_array(struct knuth_lfib_ctx *ctx,
		     unsigned n, uint32_t *a)
{
  unsigned i;
  
  for (i = 0; i<n; i++)
    a[i] = knuth_lfib_get(ctx);
}

/* NOTE: Not at all optimized. */
void
knuth_lfib_random(struct knuth_lfib_ctx *ctx,
		  unsigned n, uint8_t *dst)
{
  /* Use 24 bits from each number, xoring together some of the
     bits. */
  
  for (; n >= 3; n-=3, dst += 3)
    {
      uint32_t value = knuth_lfib_get(ctx);

      /* Xor the most significant octet (containing 6 significant bits)
       * into the lower octet. */
      value ^= (value >> 24);

      WRITE_UINT24(dst, value);
    }
  if (n)
    {
      /* We need one or two octets more */
      uint32_t value = knuth_lfib_get(ctx);
      switch (n)
	{
	case 1:
	  *dst++ = value & 0xff;
	  break;
	case 2:
	  WRITE_UINT16(dst, value);
	  break;
	default:
	  abort();
	}
    }
}
