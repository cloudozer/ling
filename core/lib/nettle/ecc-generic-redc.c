/* ecc-generic-redc.c */

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

void
ecc_generic_redc (const struct ecc_curve *ecc, mp_limb_t *rp)
{
  unsigned i;
  mp_limb_t hi, cy;
  unsigned shift = ecc->size * GMP_NUMB_BITS - ecc->bit_size;
  mp_size_t k = ecc->redc_size;
  
  assert (k != 0);
  if (k > 0)    
    {
      /* Use that 1 = p + 1, and that at least one low limb of p + 1 is zero. */
      for (i = 0; i < ecc->size; i++)
	rp[i] = mpn_addmul_1 (rp + i + k,
			      ecc->redc_ppm1, ecc->size - k, rp[i]);
      hi = mpn_add_n (rp, rp, rp + ecc->size, ecc->size);
      if (shift > 0)
	{
	  hi = (hi << shift) | (rp[ecc->size - 1] >> (GMP_NUMB_BITS - shift));
	  rp[ecc->size - 1] = (rp[ecc->size - 1]
			       & (((mp_limb_t) 1 << (GMP_NUMB_BITS - shift)) - 1))
	    + mpn_addmul_1 (rp, ecc->Bmodp_shifted, ecc->size-1, hi);
	  
	}
      else
	{
	  cy = cnd_sub_n (hi, rp, ecc->p, ecc->size);
	  assert (cy == hi);      
	}
    }
  else
    {
      /* Use that 1 = - (p - 1), and that at least one low limb of p -
	 1 is zero. */
      k = -k;
      for (i = 0; i < ecc->size; i++)
	rp[i] = mpn_submul_1 (rp + i + k,
			      ecc->redc_ppm1, ecc->size - k, rp[i]);
      hi = mpn_sub_n (rp, rp + ecc->size, rp, ecc->size);
      cy = cnd_add_n (hi, rp, ecc->p, ecc->size);
      assert (cy == hi);

      if (shift > 0)
	{
	  /* Result is always < 2p, provided that
	     2^shift * Bmodp_shifted <= p */
	  hi = (rp[ecc->size - 1] >> (GMP_NUMB_BITS - shift));
	  rp[ecc->size - 1] = (rp[ecc->size - 1]
			       & (((mp_limb_t) 1 << (GMP_NUMB_BITS - shift)) - 1))
	    + mpn_addmul_1 (rp, ecc->Bmodp_shifted, ecc->size-1, hi);
	}
    }
}
