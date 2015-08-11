/* umac-poly64.c
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

#include "umac.h"

static uint64_t
poly64_mul (uint32_t kh, uint32_t kl, uint64_t y)
{
  uint64_t yl, yh, pl, ph, ml, mh;
  yl = y & 0xffffffff;
  yh = y >> 32;
  pl = yl * kl;
  ph = yh * kh;
  ml = yh * kl + yl * kh; /* No overflow, thanks to special form */
  mh = ml >> 32;
  ml <<= 32;
  pl += ml;
  ph += mh + (pl < ml);

  /* Reduce, using 2^64 = UMAC_P64_OFFSET (mod p) */
  assert (ph < ((uint64_t) 1 << 57));
  ph *= UMAC_P64_OFFSET;
  pl += ph;
  if (pl < ph)
    pl += UMAC_P64_OFFSET;

  return pl;
}

uint64_t
_umac_poly64 (uint32_t kh, uint32_t kl, uint64_t y, uint64_t m)
{
  if ( (m >> 32) == 0xffffffff)
    {
      y = poly64_mul (kh, kl, y);
      if (y == 0)
	y = UMAC_P64 - 1;
      else
	y--;
      m -= UMAC_P64_OFFSET;
    }
  y = poly64_mul (kh, kl, y);
  y += m;
  if (y < m)
    y += UMAC_P64_OFFSET;

  return y;
}
