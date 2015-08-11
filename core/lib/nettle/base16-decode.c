/* base16-encode.c
 *
 * Hex decoding.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2002 Niels Möller
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
#include <stdlib.h>

#include "base16.h"

void
base16_decode_init(struct base16_decode_ctx *ctx)
{
  ctx->word = ctx->bits = 0;
}

enum { HEX_INVALID = -1, HEX_SPACE=-2 };

static const signed char
hex_decode_table[0x80] =
  {
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -2, -2, -1, -1, -2, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
    -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  };

/* Decodes a single byte. Returns amount of output (0 or 1), or -1 on
 * errors. */
int
base16_decode_single(struct base16_decode_ctx *ctx,
		     uint8_t *dst,
		     uint8_t src)
{
  int digit;

  if (src >= 0x80)
    return -1;

  digit = hex_decode_table[src];
  switch (digit)
    {
    case -1:
      return -1;
    case -2:
      return 0;
    default:
      assert(digit >= 0);
      assert(digit < 0x10);

      if (ctx->bits)
	{
	  *dst = (ctx->word << 4) | digit;
	  ctx->bits = 0;
	  return 1;
	}
      else
	{
	  ctx->word = digit;
	  ctx->bits = 4;
	  return 0;
	}
    }
}

int
base16_decode_update(struct base16_decode_ctx *ctx,
		     unsigned *dst_length,
		     uint8_t *dst,
		     unsigned src_length,
		     const uint8_t *src)
{
  unsigned done;
  unsigned i;

  assert(*dst_length >= BASE16_DECODE_LENGTH(src_length));
  
  for (i = 0, done = 0; i<src_length; i++)
    switch(base16_decode_single(ctx, dst + done, src[i]))
      {
      case -1:
	return 0;
      case 1:
	done++;
	/* Fall through */
      case 0:
	break;
      default:
	abort();
      }
  
  assert(done <= BASE16_DECODE_LENGTH(src_length));

  *dst_length = done;
  return 1;
}

int
base16_decode_final(struct base16_decode_ctx *ctx)
{
  return ctx->bits == 0;
}
