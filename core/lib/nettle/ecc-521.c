/* ecc-521.c.c */

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

#include "ecc-internal.h"

#define USE_REDC 0

#include "ecc-521.h"

#if HAVE_NATIVE_ecc_521_modp
#define ecc_521_modp nettle_ecc_521_modp
void
ecc_521_modp (const struct ecc_curve *ecc, mp_limb_t *rp);

#else

#define B_SHIFT (521 % GMP_NUMB_BITS)
#define BMODP_SHIFT (GMP_NUMB_BITS - B_SHIFT)
#define BMODP ((mp_limb_t) 1 << BMODP_SHIFT)

/* Result may be *slightly* larger than 2^521 */
static void
ecc_521_modp (const struct ecc_curve *ecc UNUSED, mp_limb_t *rp)
{
  /* FIXME: Should use mpn_addlsh_n_ip1 */
  mp_limb_t hi;
  /* Reduce from 2*ECC_LIMB_SIZE to ECC_LIMB_SIZE + 1 */
  rp[ECC_LIMB_SIZE]
    = mpn_addmul_1 (rp, rp + ECC_LIMB_SIZE, ECC_LIMB_SIZE, BMODP);
  hi = mpn_addmul_1 (rp, rp + ECC_LIMB_SIZE, 1, BMODP);
  hi = sec_add_1 (rp + 1, rp + 1, ECC_LIMB_SIZE - 1, hi);

  /* Combine hi with top bits, and add in. */
  hi = (hi << BMODP_SHIFT) | (rp[ECC_LIMB_SIZE-1] >> B_SHIFT);
  rp[ECC_LIMB_SIZE-1] = (rp[ECC_LIMB_SIZE-1]
			 & (((mp_limb_t) 1 << B_SHIFT)-1))
    + sec_add_1 (rp, rp, ECC_LIMB_SIZE - 1, hi);
}
#endif

const struct ecc_curve nettle_secp_521r1 =
{
  521,
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
  ecc_521_modp,
  ecc_generic_redc,
  ecc_521_modp,
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

