/* sec-tabselect.c */

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

/* Development of Nettle's ECC support was funded by the .SE Internet Fund. */

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>

#include "ecc-internal.h"

/* Copy the k'th element of the table out tn elements, each of size
   rn. Always read complete table. Similar to gmp's mpn_tabselect. */
/* FIXME: Should we need to volatile declare anything? */
void
sec_tabselect (mp_limb_t *rp, mp_size_t rn,
	       const mp_limb_t *table, unsigned tn,
	       unsigned k)
{
  const mp_limb_t *end = table + tn * rn;
  const mp_limb_t *p;
  mp_size_t i;
  
  assert (k < tn);
  mpn_zero (rp, rn);
  for (p = table; p < end; p += rn, k--)
    {
      mp_limb_t mask = - (mp_limb_t) (k == 0);
      for (i = 0; i < rn; i++)
	rp[i] += mask & p[i];
    }
}
