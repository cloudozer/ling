/* ecc-ecdsa-verify.c */

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
#include <stdlib.h>

#include "ecdsa.h"
#include "ecc-internal.h"

/* Low-level ECDSA verify */

static int
zero_p (const mp_limb_t *xp, mp_size_t n)
{
  while (n > 0)
    if (xp[--n] > 0)
      return 0;
  return 1;
}

static int
ecdsa_in_range (const struct ecc_curve *ecc, const mp_limb_t *xp)
{
  return !zero_p (xp, ecc->size)
    && mpn_cmp (xp, ecc->q, ecc->size) < 0;
}

mp_size_t
ecc_ecdsa_verify_itch (const struct ecc_curve *ecc)
{
  /* Largest storage need is for the ecc_mul_a call, 6 * ecc->size +
     ECC_MUL_A_ITCH (size) */
  return ECC_ECDSA_VERIFY_ITCH (ecc->size);
}

/* FIXME: Use faster primitives, not requiring side-channel silence. */
int
ecc_ecdsa_verify (const struct ecc_curve *ecc,
		  const mp_limb_t *pp, /* Public key */
		  unsigned length, const uint8_t *digest,
		  const mp_limb_t *rp, const mp_limb_t *sp,
		  mp_limb_t *scratch)
{
  /* Procedure, according to RFC 6090, "KT-I". q denotes the group
     order.

     1. Check 0 < r, s < q.

     2. s' <-- s^{-1}  (mod q)

     3. u1  <-- h * s' (mod q)

     4. u2  <-- r * s' (mod q)

     5. R = u1 G + u2 Y

     6. Signature is valid if R_x = r (mod q).
  */

#define P2 scratch
#define P1 (scratch + 3*ecc->size)
#define sinv (scratch + 3*ecc->size)
#define u2 (scratch + 4*ecc->size)
#define hp (scratch + 4*ecc->size)
#define u1 (scratch + 6*ecc->size)

  if (! (ecdsa_in_range (ecc, rp)
	 && ecdsa_in_range (ecc, sp)))
    return 0;

  /* FIXME: Micro optimizations: Either simultaneous multiplication.
     Or convert to projective coordinates (can be done without
     division, I think), and write an ecc_add_ppp. */
  
  /* Compute sinv, use P2 as scratch */
  mpn_copyi (sinv + ecc->size, sp, ecc->size);
  ecc_modq_inv (ecc, sinv, sinv + ecc->size, P2);

  /* u2 = r / s, P2 = u2 * Y */
  ecc_modq_mul (ecc, u2, rp, sinv);

   /* Total storage: 5*ecc->size + ECC_MUL_A_ITCH (ecc->size) */
  ecc_mul_a (ecc, 1, P2, u2, pp, u2 + ecc->size);

  /* u1 = h / s, P1 = u1 * G */
  ecc_hash (ecc, hp, length, digest);
  ecc_modq_mul (ecc, u1, hp, sinv);

  /* u = 0 can happen only if h = 0 or h = q, which is extremely
     unlikely. */
  if (!zero_p (u1, ecc->size))
    {
      /* Total storage: 6*ecc->size + ECC_MUL_G_ITCH (ecc->size) */
      ecc_mul_g (ecc, P1, u1, u1 + ecc->size);

      /* NOTE: ecc_add_jjj and/or ecc_j_to_a will produce garbage in
	 case u1 G = +/- u2 V. However, anyone who gets his or her
	 hands on a signature where this happens during verification,
	 can also get the private key as z = +/- u1 / u_2 (mod q). And
	 then it doesn't matter very much if verification of
	 signatures with that key succeeds or fails.

	 u1 G = - u2 V can never happen for a correctly generated
	 signature, since it implies k = 0.

	 u1 G = u2 V is possible, if we are unlucky enough to get h /
	 s_1 = z. Hitting that is about as unlikely as finding the
	 private key by guessing.
       */
      /* Total storage: 6*ecc->size + ECC_ADD_JJJ_ITCH (ecc->size) */
      ecc_add_jjj (ecc, P1, P1, P2, u1);
    }
  ecc_j_to_a (ecc, 3, P2, P1, u1);

  if (mpn_cmp (P2, ecc->q, ecc->size) >= 0)
    mpn_sub_n (P2, P2, ecc->q, ecc->size);

  return (mpn_cmp (rp, P2, ecc->size) == 0);
#undef P2
#undef P1
#undef sinv
#undef u2
#undef hp
#undef u1
}
