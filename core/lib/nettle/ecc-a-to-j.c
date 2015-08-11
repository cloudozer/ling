/* ecc-a-to-j.c */

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

#include "ecc.h"
#include "ecc-internal.h"

void
ecc_a_to_j (const struct ecc_curve *ecc,
	    int initial,
	    mp_limb_t *r, const mp_limb_t *p)
{
  if (ecc->use_redc && initial)
    {
      mpn_copyd (r + ecc->size, p, 2*ecc->size);

      mpn_zero (r, ecc->size);
      ecc->modp (ecc, r);

      mpn_zero (r + ecc->size, ecc->size);
      ecc->modp (ecc, r + ecc->size);
    }
  else if (r != p)
    mpn_copyi (r, p, 2*ecc->size);

  mpn_copyi (r + 2*ecc->size, ecc->unit, ecc->size);
}
