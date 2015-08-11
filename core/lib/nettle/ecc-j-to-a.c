/* ecc-j-to-a.c */

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

mp_size_t
ecc_j_to_a_itch (const struct ecc_curve *ecc)
{
  /* Needs 2*ecc->size + scratch for ecc_modq_inv */
  return ECC_J_TO_A_ITCH (ecc->size);
}

void
ecc_j_to_a (const struct ecc_curve *ecc,
	    int flags,
	    mp_limb_t *r, const mp_limb_t *p,
	    mp_limb_t *scratch)
{
#define izp   scratch
#define up   (scratch + ecc->size)
#define iz2p (scratch + ecc->size)
#define iz3p (scratch + 2*ecc->size)
#define izBp (scratch + 3*ecc->size)
#define tp    scratch

  mp_limb_t cy;

  if (ecc->use_redc)
    {
      /* Set v = (r_z / B^2)^-1,

	 r_x = p_x v^2 / B^3 =  ((v/B * v)/B * p_x)/B
	 r_y = p_y v^3 / B^4 = (((v/B * v)/B * v)/B * p_x)/B

	 Skip the first redc, if we want to stay in Montgomery
	 representation.
      */

      mpn_copyi (up, p + 2*ecc->size, ecc->size);
      mpn_zero (up + ecc->size, ecc->size);
      ecc->redc (ecc, up);
      mpn_zero (up + ecc->size, ecc->size);
      ecc->redc (ecc, up);

      ecc_modp_inv (ecc, izp, up, up + ecc->size);

      if (flags & 1)
	{
	  /* Divide this common factor by B */
	  mpn_copyi (izBp, izp, ecc->size);
	  mpn_zero (izBp + ecc->size, ecc->size);
	  ecc->redc (ecc, izBp);

	  ecc_modp_mul (ecc, iz2p, izp, izBp);
	}
      else
	ecc_modp_sqr (ecc, iz2p, izp);	
    }
  else
    {
      /* Set s = p_z^{-1}, r_x = p_x s^2, r_y = p_y s^3 */

      mpn_copyi (up, p+2*ecc->size, ecc->size); /* p_z */
      ecc_modp_inv (ecc, izp, up, up + ecc->size);

      ecc_modp_sqr (ecc, iz2p, izp);
    }

  ecc_modp_mul (ecc, iz3p, iz2p, p);
  /* ecc_modp (and ecc_modp_mul) may return a value up to 2p - 1, so
     do a conditional subtraction. */
  cy = mpn_sub_n (r, iz3p, ecc->p, ecc->size);
  cnd_copy (cy, r, iz3p, ecc->size);

  if (flags & 2)
    /* Skip y coordinate */
    return;

  ecc_modp_mul (ecc, iz3p, iz2p, izp);
  ecc_modp_mul (ecc, tp, iz3p, p + ecc->size);
  /* And a similar subtraction. */
  cy = mpn_sub_n (r + ecc->size, tp, ecc->p, ecc->size);
  cnd_copy (cy, r + ecc->size, tp, ecc->size);

#undef izp
#undef up
#undef iz2p
#undef iz3p
#undef tp
}
