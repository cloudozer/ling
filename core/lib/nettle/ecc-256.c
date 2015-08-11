/* ecc-256.c.c */

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

#if HAVE_NATIVE_ecc_256_redc
# define USE_REDC 1
#else
# define USE_REDC (ECC_REDC_SIZE != 0)
#endif

#include "ecc-256.h"

#if HAVE_NATIVE_ecc_256_redc
# define ecc_256_redc nettle_ecc_256_redc
void
ecc_256_redc (const struct ecc_curve *ecc, mp_limb_t *rp);
#else /* !HAVE_NATIVE_ecc_256_redc */
# define ecc_256_redc ecc_generic_redc
#endif

#if ECC_BMODP_SIZE < ECC_LIMB_SIZE
#define ecc_256_modp ecc_generic_modp
#define ecc_256_modq ecc_generic_modq
#elif GMP_NUMB_BITS == 64

static void
ecc_256_modp (const struct ecc_curve *ecc, mp_limb_t *rp)
{
  mp_limb_t u1, u0;
  mp_size_t n;

  n = 2*ecc->size;
  u1 = rp[--n];
  u0 = rp[n-1];

  /* This is not particularly fast, but should work well with assembly implementation. */
  for (; n >= ecc->size; n--)
    {
      mp_limb_t q2, q1, q0, t, cy;

      /* <q2, q1, q0> = v * u1 + <u1,u0>, with v = 2^32 - 1:

	   +---+---+
	   | u1| u0|
	   +---+---+
	       |-u1|
	     +-+-+-+
	     | u1|
       +---+-+-+-+-+
       | q2| q1| q0|
       +---+---+---+
      */
      q1 = u1 - (u1 > u0);
      q0 = u0 - u1;
      t = u1 << 32;
      q0 += t;
      t = (u1 >> 32) + (q0 < t) + 1;
      q1 += t;
      q2 = q1 < t;

      /* Compute candidate remainder */
      u1 = u0 + (q1 << 32) - q1;
      t = -(mp_limb_t) (u1 > q0);
      u1 -= t & 0xffffffff;
      q1 += t;
      q2 += t + (q1 < t);

      assert (q2 < 2);

      /* We multiply by two low limbs of p, 2^96 - 1, so we could use
	 shifts rather than mul. */
      t = mpn_submul_1 (rp + n - 4, ecc->p, 2, q1);
      t += cnd_sub_n (q2, rp + n - 3, ecc->p, 1);
      t += (-q2) & 0xffffffff;

      u0 = rp[n-2];
      cy = (u0 < t);
      u0 -= t;
      t = (u1 < cy);
      u1 -= cy;
      u1 += cnd_add_n (t, rp + n - 4, ecc->p, 3);
      u1 -= (-t) & 0xffffffff;
    }
  rp[2] = u0;
  rp[3] = u1;
}

static void
ecc_256_modq (const struct ecc_curve *ecc, mp_limb_t *rp)
{
  mp_limb_t u2, u1, u0;
  mp_size_t n;

  n = 2*ecc->size;
  u2 = rp[--n];
  u1 = rp[n-1];

  /* This is not particularly fast, but should work well with assembly implementation. */
  for (; n >= ecc->size; n--)
    {
      mp_limb_t q2, q1, q0, t, c1, c0;

      u0 = rp[n-2];
      
      /* <q2, q1, q0> = v * u2 + <u2,u1>, same method as above.

	   +---+---+
	   | u2| u1|
	   +---+---+
	       |-u2|
	     +-+-+-+
	     | u2|
       +---+-+-+-+-+
       | q2| q1| q0|
       +---+---+---+
      */
      q1 = u2 - (u2 > u1);
      q0 = u1 - u2;
      t = u2 << 32;
      q0 += t;
      t = (u2 >> 32) + (q0 < t) + 1;
      q1 += t;
      q2 = q1 < t;

      /* Compute candidate remainder, <u1, u0> - <q2, q1> * (2^128 - 2^96 + 2^64 - 1)
         <u1, u0> + 2^64 q2 + (2^96 - 2^64 + 1) q1 (mod 2^128)

	   +---+---+
	   | u1| u0|
	   +---+---+
	   | q2| q1|
	   +---+---+
	   |-q1|
	 +-+-+-+
	 | q1|
       --+-+-+-+---+
           | u2| u1|
	   +---+---+
      */	 
      u2 = u1 + q2 - q1;
      u1 = u0 + q1;
      u2 += (u1 < q1);
      u2 += (q1 << 32);

      t = -(mp_limb_t) (u2 >= q0);
      q1 += t;
      q2 += t + (q1 < t);
      u1 += t;
      u2 += (t << 32) + (u1 < t);

      assert (q2 < 2);

      c0 = cnd_sub_n (q2, rp + n - 3, ecc->q, 1);
      c0 += (-q2) & ecc->q[1];
      t = mpn_submul_1 (rp + n - 4, ecc->q, 2, q1);
      c0 += t;
      c1 = c0 < t;
      
      /* Construct underflow condition. */
      c1 += (u1 < c0);
      t = - (mp_limb_t) (u2 < c1);

      u1 -= c0;
      u2 -= c1;

      /* Conditional add of p */
      u1 += t;
      u2 += (t<<32) + (u0 < t);

      t = cnd_add_n (t, rp + n - 4, ecc->q, 2);
      u1 += t;
      u2 += (u1 < t);
    }
  rp[2] = u1;
  rp[3] = u2;
}
      
#else
#error Unsupported parameters
#endif

const struct ecc_curve nettle_secp_256r1 =
{
  256,
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
  ecc_256_modp,
  ecc_256_redc,
  USE_REDC ? ecc_256_redc : ecc_256_modp,
  ecc_256_modq,
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
