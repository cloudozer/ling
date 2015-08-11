/* ecc-modp.c */

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

/* Routines for modp arithmetic. All values are ecc->size limbs, but
   not necessarily < p. */

void
ecc_modp_add (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap, const mp_limb_t *bp)
{
  mp_limb_t cy;
  cy = mpn_add_n (rp, ap, bp, ecc->size);
  cy = cnd_add_n (cy, rp, ecc->Bmodp, ecc->size);
  cy = cnd_add_n (cy, rp, ecc->Bmodp, ecc->size);
  assert (cy == 0);  
}

void
ecc_modp_sub (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap, const mp_limb_t *bp)
{
  mp_limb_t cy;
  cy = mpn_sub_n (rp, ap, bp, ecc->size);
  cy = cnd_sub_n (cy, rp, ecc->Bmodp, ecc->size);
  cy = cnd_sub_n (cy, rp, ecc->Bmodp, ecc->size);
  assert (cy == 0);  
}

void
ecc_modp_sub_1 (const struct ecc_curve *ecc, mp_limb_t *rp,
		const mp_limb_t *ap, mp_limb_t b)
{
  mp_size_t i;

  for (i = 0; i < ecc->size; i++)
    {
      mp_limb_t cy = ap[i] < b;
      rp[i] = ap[i] - b;
      b = cy;
    }
  b = cnd_sub_n (b, rp, ecc->Bmodp, ecc->size);
  assert (b == 0);    
}

void
ecc_modp_mul_1 (const struct ecc_curve *ecc, mp_limb_t *rp,
		const mp_limb_t *ap, mp_limb_t b)
{
  mp_limb_t hi;

  assert (b <= 0xffffffff);
  hi = mpn_mul_1 (rp, ap, ecc->size, b);
  hi = mpn_addmul_1 (rp, ecc->Bmodp, ecc->size, hi);
  assert (hi <= 1);
  hi = cnd_add_n (hi, rp, ecc->Bmodp, ecc->size);
  /* Sufficient if b < B^size / p */
  assert (hi == 0);
}

void
ecc_modp_addmul_1 (const struct ecc_curve *ecc, mp_limb_t *rp,
		   const mp_limb_t *ap, mp_limb_t b)
{
  mp_limb_t hi;

  assert (b <= 0xffffffff);
  hi = mpn_addmul_1 (rp, ap, ecc->size, b);
  hi = mpn_addmul_1 (rp, ecc->Bmodp, ecc->size, hi);
  assert (hi <= 1);
  hi = cnd_add_n (hi, rp, ecc->Bmodp, ecc->size);
  /* Sufficient roughly if b < B^size / p */
  assert (hi == 0);
}
  
void
ecc_modp_submul_1 (const struct ecc_curve *ecc, mp_limb_t *rp,
		   const mp_limb_t *ap, mp_limb_t b)
{
  mp_limb_t hi;

  assert (b <= 0xffffffff);
  hi = mpn_submul_1 (rp, ap, ecc->size, b);
  hi = mpn_submul_1 (rp, ecc->Bmodp, ecc->size, hi);
  assert (hi <= 1);
  hi = cnd_sub_n (hi, rp, ecc->Bmodp, ecc->size);
  /* Sufficient roughly if b < B^size / p */
  assert (hi == 0);
}

/* NOTE: mul and sqr needs 2*ecc->size limbs at rp */
void
ecc_modp_mul (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap, const mp_limb_t *bp)
{
  mpn_mul_n (rp, ap, bp, ecc->size);
  ecc->reduce (ecc, rp);
}

void
ecc_modp_sqr (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap)
{
  mpn_sqr (rp, ap, ecc->size);
  ecc->reduce (ecc, rp);
}

void
ecc_modp_inv (const struct ecc_curve *ecc, mp_limb_t *rp, mp_limb_t *ap,
	      mp_limb_t *scratch)
{
  sec_modinv (rp, ap, ecc->size, ecc->p, ecc->pp1h, ecc->bit_size, scratch);
}

