/* ecc-ecdsa-sign.c */

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

/* Low-level ECDSA signing */

mp_size_t
ecc_ecdsa_sign_itch (const struct ecc_curve *ecc)
{
  /* Needs 3*ecc->size + scratch for ecc_mul_g. */
  return ECC_ECDSA_SIGN_ITCH (ecc->size);
}

/* NOTE: Caller should check if r or s is zero. */
void
ecc_ecdsa_sign (const struct ecc_curve *ecc,
		const mp_limb_t *zp,
		/* Random nonce, must be invertible mod ecc group
		   order. */
		const mp_limb_t *kp,
		unsigned length, const uint8_t *digest,
		mp_limb_t *rp, mp_limb_t *sp,
		mp_limb_t *scratch)
{
  mp_limb_t cy;
#define P	    scratch
#define kinv	    scratch                /* Needs 5*ecc->size for computation */
#define hp	    (scratch  + ecc->size) /* NOTE: ecc->size + 1 limbs! */
#define tp	    (scratch + 2*ecc->size)
  /* Procedure, according to RFC 6090, "KT-I". q denotes the group
     order.

     1. k <-- uniformly random, 0 < k < q

     2. R <-- (r_x, r_y) = k g

     3. s1 <-- r_x mod q

     4. s2 <-- (h + z*s1)/k mod q.
  */

  ecc_mul_g (ecc, P, kp, P + 3*ecc->size);
  /* x coordinate only */
  ecc_j_to_a (ecc, 3, rp, P, P + 3*ecc->size);

  /* We need to reduce x coordinate mod ecc->q. It should already
     be < 2*ecc->q, so one subtraction should suffice. */
  cy = mpn_sub_n (scratch, rp, ecc->q, ecc->size);
  cnd_copy (cy == 0, rp, scratch, ecc->size);

  /* Invert k, uses 5 * ecc->size including scratch */
  mpn_copyi (hp, kp, ecc->size);
  ecc_modq_inv (ecc, kinv, hp, tp);
  
  /* Process hash digest */
  ecc_hash (ecc, hp, length, digest);

  ecc_modq_mul (ecc, tp, zp, rp);
  ecc_modq_add (ecc, hp, hp, tp);
  ecc_modq_mul (ecc, tp, hp, kinv);

  mpn_copyi (sp, tp, ecc->size);
#undef P
#undef hp
#undef kinv
#undef tp
}
