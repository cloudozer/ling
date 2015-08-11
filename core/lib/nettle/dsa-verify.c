/* dsa-verify.c
 *
 * The DSA publickey algorithm.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2002, 2003 Niels Möller
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

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdlib.h>

#include "dsa.h"

#include "bignum.h"

int
_dsa_verify(const struct dsa_public_key *key,
	    unsigned digest_size,
	    const uint8_t *digest,
	    const struct dsa_signature *signature)
{
  mpz_t w;
  mpz_t tmp;
  mpz_t v;

  int res;

  if (mpz_sizeinbase(key->q, 2) != 8 * digest_size)
    return 0;

  /* Check that r and s are in the proper range */
  if (mpz_sgn(signature->r) <= 0 || mpz_cmp(signature->r, key->q) >= 0)
    return 0;

  if (mpz_sgn(signature->s) <= 0 || mpz_cmp(signature->s, key->q) >= 0)
    return 0;

  mpz_init(w);

  /* Compute w = s^-1 (mod q) */

  /* NOTE: In gmp-2, mpz_invert sometimes generates negative inverses,
   * so we need gmp-3 or better. */
  if (!mpz_invert(w, signature->s, key->q))
    {
      mpz_clear(w);
      return 0;
    }

  mpz_init(tmp);
  mpz_init(v);

  /* The message digest */
  nettle_mpz_set_str_256_u(tmp, digest_size, digest);
  
  /* v = g^{w * h (mod q)} (mod p)  */
  mpz_mul(tmp, tmp, w);
  mpz_fdiv_r(tmp, tmp, key->q);

  mpz_powm(v, key->g, tmp, key->p);

  /* y^{w * r (mod q) } (mod p) */
  mpz_mul(tmp, signature->r, w);
  mpz_fdiv_r(tmp, tmp, key->q);

  mpz_powm(tmp, key->y, tmp, key->p);

  /* v = (g^{w * h} * y^{w * r} (mod p) ) (mod q) */
  mpz_mul(v, v, tmp);
  mpz_fdiv_r(v, v, key->p);

  mpz_fdiv_r(v, v, key->q);

  res = !mpz_cmp(v, signature->r);

  mpz_clear(w);
  mpz_clear(tmp);
  mpz_clear(v);

  return res;
}
