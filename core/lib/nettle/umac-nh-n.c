/* umac-nh-n.c
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
_umac_nh_n (uint64_t *out, unsigned n, const uint32_t *key,
	    unsigned length, const uint8_t *msg)
{
  assert (length > 0);
  assert (length <= 1024);
  assert (length % 32 == 0);

  memset(out, 0, n*sizeof(*out));
  
  for (;length > 0; length -= 32, msg += 32, key += 8)
    {
      uint32_t a0, a1, b0, b1;
      unsigned i;
      a0 = LE_READ_UINT32 (msg);
      a1 = LE_READ_UINT32 (msg + 4);
      b0 = LE_READ_UINT32 (msg + 16);
      b1 = LE_READ_UINT32 (msg + 20);
      for (i = 0; i < n; i++)
	out[i] += (uint64_t) (a0 + key[0+4*i]) * (b0 + key[4+4*i])
	  + (uint64_t) (a1 + key[1+4*i]) * (b1 + key[5+4*i]);
      a0 = LE_READ_UINT32 (msg + 8);
      a1 = LE_READ_UINT32 (msg + 12);
      b0 = LE_READ_UINT32 (msg + 24);
      b1 = LE_READ_UINT32 (msg + 28);
      for (i = 0; i < n; i++)
	out[i] += (uint64_t) (a0 + key[2+4*i]) * (b0 + key[6+4*i])
	  + (uint64_t) (a1 + key[3+4*i]) * (b1 + key[7+4*i]);      
    }
}
