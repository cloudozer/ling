/* ecc-224.c.c */

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

#if HAVE_NATIVE_ecc_224_modp

#define USE_REDC 0
#define ecc_224_modp nettle_ecc_224_modp
void
ecc_224_modp (const struct ecc_curve *ecc, mp_limb_t *rp);

#else
#define USE_REDC (ECC_REDC_SIZE != 0)
#define ecc_224_modp ecc_generic_modp
#endif

#include "ecc-224.h"

const struct ecc_curve nettle_secp_224r1 =
{
  224,
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
  ecc_224_modp,
  ecc_generic_redc,
  USE_REDC ? ecc_generic_redc : ecc_224_modp,
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
