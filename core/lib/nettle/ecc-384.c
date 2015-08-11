/* ecc-384.c.c */

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

#include "ecc-384.h"

#if HAVE_NATIVE_ecc_384_modp
#define ecc_384_modp nettle_ecc_384_modp
void
ecc_384_modp (const struct ecc_curve *ecc, mp_limb_t *rp);
#elif GMP_NUMB_BITS == 32

/* Use that 2^{384} = 2^{128} + 2^{96} - 2^{32} + 1, and eliminate 256
   bits at a time.

   We can get carry == 2 in the first iteration, and I think *only* in
   the first iteration. */

/* p is 12 limbs, and B^12 - p = B^4 + B^3 - B + 1. We can eliminate
   almost 8 at a time. Do only 7, to avoid additional carry
   propagation, followed by 5. */
static void
ecc_384_modp (const struct ecc_curve *ecc, mp_limb_t *rp)
{
  mp_limb_t cy, bw;

  /* Reduce from 24 to 17 limbs. */
  cy = mpn_add_n (rp + 4, rp + 4, rp + 16, 8);
  cy = sec_add_1 (rp + 12, rp + 12, 3, cy);

  bw = mpn_sub_n (rp + 5, rp + 5, rp + 16, 8);
  bw = sec_sub_1 (rp + 13, rp + 13, 3, bw);

  cy += mpn_add_n (rp + 7, rp + 7, rp + 16, 8);
  cy = sec_add_1 (rp + 15, rp + 15, 1, cy);

  cy += mpn_add_n (rp + 8, rp + 8, rp + 16, 8);
  assert (bw <= cy);
  cy -= bw;

  assert (cy <= 2);  
  rp[16] = cy;

  /* Reduce from 17 to 12 limbs */
  cy = mpn_add_n (rp, rp, rp + 12, 5);
  cy = sec_add_1 (rp + 5, rp + 5, 3, cy);
  
  bw = mpn_sub_n (rp + 1, rp + 1, rp + 12, 5);
  bw = sec_sub_1 (rp + 6, rp + 6, 6, bw);
  
  cy += mpn_add_n (rp + 3, rp + 3, rp + 12, 5);
  cy = sec_add_1 (rp + 8, rp + 8, 1, cy);

  cy += mpn_add_n (rp + 4, rp + 4, rp + 12, 5);
  cy = sec_add_1 (rp + 9, rp + 9, 3, cy);

  assert (cy >= bw);
  cy -= bw;
  assert (cy <= 1);
  cy = cnd_add_n (cy, rp, ecc->Bmodp, ECC_LIMB_SIZE);
  assert (cy == 0);
}
#elif GMP_NUMB_BITS == 64
/* p is 6 limbs, and B^6 - p = B^2 + 2^32 (B - 1) + 1. Eliminate 3
   (almost 4) limbs at a time. */
static void
ecc_384_modp (const struct ecc_curve *ecc, mp_limb_t *rp)
{
  mp_limb_t tp[6];
  mp_limb_t cy;

  /* Reduce from 12 to 9 limbs */
  tp[0] = 0; /* FIXME: Could use mpn_sub_nc */
  mpn_copyi (tp + 1, rp + 8, 3);
  tp[4] = rp[11] - mpn_sub_n (tp, tp, rp + 8, 4);
  tp[5] = mpn_lshift (tp, tp, 5, 32);

  cy = mpn_add_n (rp + 2, rp + 2, rp + 8, 4);
  cy = sec_add_1 (rp + 6, rp + 6, 2, cy);

  cy += mpn_add_n (rp + 2, rp + 2, tp, 6);
  cy += mpn_add_n (rp + 4, rp + 4, rp + 8, 4);

  assert (cy <= 2);
  rp[8] = cy;

  /* Reduce from 9 to 6 limbs */
  tp[0] = 0;
  mpn_copyi (tp + 1, rp + 6, 2);
  tp[3] = rp[8] - mpn_sub_n (tp, tp, rp + 6, 3);
  tp[4] = mpn_lshift (tp, tp, 4, 32);

  cy = mpn_add_n (rp, rp, rp + 6, 3);
  cy = sec_add_1 (rp + 3, rp + 3, 2, cy);
  cy += mpn_add_n (rp, rp, tp, 5);
  cy += mpn_add_n (rp + 2, rp + 2, rp + 6, 3);

  cy = sec_add_1 (rp + 5, rp + 5, 1, cy);
  assert (cy <= 1);

  cy = cnd_add_n (cy, rp, ecc->Bmodp, ECC_LIMB_SIZE);
  assert (cy == 0);  
}
#else
#define ecc_384_modp ecc_generic_modp
#endif
  
const struct ecc_curve nettle_secp_384r1 =
{
  384,
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
  ecc_384_modp,
  ECC_REDC_SIZE != 0 ? ecc_generic_redc : NULL,
  ecc_384_modp,
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
