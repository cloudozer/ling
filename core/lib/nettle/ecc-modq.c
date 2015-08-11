/* ecc-modq.c */

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

/* Arithmetic mod q, the group order. */

void
ecc_modq_add (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap, const mp_limb_t *bp)
{
  mp_limb_t cy;
  cy = mpn_add_n (rp, ap, bp, ecc->size);
  cy = cnd_add_n (cy, rp, ecc->Bmodq, ecc->size);
  cy = cnd_add_n (cy, rp, ecc->Bmodq, ecc->size);
  assert (cy == 0);  
}

void
ecc_modq_mul (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap, const mp_limb_t *bp)
{
  mpn_mul_n (rp, ap, bp, ecc->size);
  ecc->modq (ecc, rp);
}

void
ecc_modq_inv (const struct ecc_curve *ecc, mp_limb_t *rp, mp_limb_t *ap,
	      mp_limb_t *scratch)
{
  sec_modinv (rp, ap, ecc->size, ecc->q, ecc->qp1h, ecc->bit_size, scratch);
}
