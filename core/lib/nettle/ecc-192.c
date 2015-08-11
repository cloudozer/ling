/* ecc-192.c */

/* Compile time constant (but machine dependent) tables. */

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

#define USE_REDC 0

#include "ecc-192.h"

#if HAVE_NATIVE_ecc_192_modp

#define ecc_192_modp nettle_ecc_192_modp
void
ecc_192_modp (const struct ecc_curve *ecc, mp_limb_t *rp);

/* Use that p = 2^{192} - 2^64 - 1, to eliminate 128 bits at a time. */

#elif GMP_NUMB_BITS == 32
/* p is 6 limbs, p = B^6 - B^2 - 1 */
static void
ecc_192_modp (const struct ecc_curve *ecc UNUSED, mp_limb_t *rp)
{
  mp_limb_t cy;

  /* Reduce from 12 to 9 limbs (top limb small)*/
  cy = mpn_add_n (rp + 2, rp + 2, rp + 8, 4);
  cy = sec_add_1 (rp + 6, rp + 6, 2, cy);
  cy += mpn_add_n (rp + 4, rp + 4, rp + 8, 4);
  assert (cy <= 2);

  rp[8] = cy;

  /* Reduce from 9 to 6 limbs */
  cy = mpn_add_n (rp, rp, rp + 6, 3);
  cy = sec_add_1 (rp + 3, rp + 3, 2, cy);
  cy += mpn_add_n (rp + 2, rp + 2, rp + 6, 3);
  cy = sec_add_1 (rp + 5, rp + 5, 1, cy);
  
  assert (cy <= 1);
  cy = cnd_add_n (cy, rp, ecc_Bmodp, 6);
  assert (cy == 0);  
}
#elif GMP_NUMB_BITS == 64
/* p is 3 limbs, p = B^3 - B - 1 */
static void
ecc_192_modp (const struct ecc_curve *ecc UNUSED, mp_limb_t *rp)
{
  mp_limb_t cy;

  /* Reduce from 6 to 5 limbs (top limb small)*/
  cy = mpn_add_n (rp + 1, rp + 1, rp + 4, 2);
  cy = sec_add_1 (rp + 3, rp + 3, 1, cy);
  cy += mpn_add_n (rp + 2, rp + 2, rp + 4, 2);
  assert (cy <= 2);

  rp[4] = cy;

  /* Reduce from 5 to 4 limbs (high limb small) */
  cy = mpn_add_n (rp, rp, rp + 3, 2);
  cy = sec_add_1 (rp + 2, rp + 2, 1, cy);
  cy += mpn_add_n (rp + 1, rp + 1, rp + 3, 2);

  assert (cy <= 1);
  cy = cnd_add_n (cy, rp, ecc_Bmodp, 3);
  assert (cy == 0);  
}
  
#else
#define ecc_192_modp ecc_generic_modp
#endif

const struct ecc_curve nettle_secp_192r1 =
{
  192,
  ECC_LIMB_SIZE,
  ECC_BMODP_SIZE,
  ECC_BMODQ_SIZE,
  USE_REDC,
  ECC_REDC_SIZE,
  ECC_PIPPENGER_K,
  ECC_PIPPENGER_C,
  ecc_p,
  ecc_b,
  ecc_q,
  ecc_g,
  ecc_redc_g,
  ecc_192_modp,
  ecc_generic_redc,
  ecc_192_modp,
  ecc_generic_modq,
  ecc_Bmodp,
  ecc_Bmodp_shifted,
  ecc_pp1h,
  ecc_redc_ppm1,
  ecc_unit,
  ecc_Bmodq,
  ecc_Bmodq_shifted,
  ecc_qp1h,
  ecc_table
};

