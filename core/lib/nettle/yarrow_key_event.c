/* yarrow_key_event.c
 *
 * Exampel entropy estimator for key-like input events. */

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

#include "yarrow.h"

void
yarrow_key_event_init(struct yarrow_key_event_ctx *ctx)
{
  unsigned i;
  
  ctx->index = 0;
  ctx->previous = 0;
  
  for (i = 0; i < YARROW_KEY_EVENT_BUFFER; i++)
    ctx->chars[i] = 0;  
}

unsigned
yarrow_key_event_estimate(struct yarrow_key_event_ctx *ctx,
			  unsigned key, unsigned time)
{
  unsigned entropy = 0;
  unsigned i;

  /* Look at timing first. */
  if (ctx->previous && (time > ctx->previous) )
    {
      if ( (time - ctx->previous) >= 256)
        entropy++;
    }
  ctx->previous = time;

  if (!key)
    return entropy;
  
  for (i = 0; i < YARROW_KEY_EVENT_BUFFER; i++)
    if (key == ctx->chars[i])
      /* This is a recent character. Ignore it. */
      return entropy;

  /* Count one bit of entropy, unless this was one of the initial 16
   * characters. */
  if (ctx->chars[ctx->index])
    entropy++;
  
  /* Remember the character. */
  
  ctx->chars[ctx->index] = key;
  ctx->index = (ctx->index + 1) % YARROW_KEY_EVENT_BUFFER;

  return entropy;
}

