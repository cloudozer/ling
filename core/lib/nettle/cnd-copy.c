/* cnd-copy.c */

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

#include "ecc-internal.h"

void
cnd_copy (int cnd, mp_limb_t *rp, const mp_limb_t *ap, mp_size_t n)
{
  mp_limb_t mask, keep;
  mp_size_t i;

  mask = -(mp_limb_t) (cnd !=0);
  keep = ~mask;

  for (i = 0; i < n; i++)
    rp[i] = (rp[i] & keep) + (ap[i] & mask);
}
