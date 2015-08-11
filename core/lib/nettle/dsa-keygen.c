/* dsa-keygen.c
 *
 * Generation of DSA keypairs
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2002 Niels Möller
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

#include <assert.h>
#include <stdlib.h>

#include "dsa.h"

#include "bignum.h"
#include "nettle-internal.h"


/* Valid sizes, according to FIPS 186-3 are (1024, 160), (2048. 224),
   (2048, 256), (3072, 256). Currenty, we use only q_bits of 160 or
   256. */
int
dsa_generate_keypair(struct dsa_public_key *pub,
		     struct dsa_private_key *key,
		     void *random_ctx, nettle_random_func *random,
		     void *progress_ctx, nettle_progress_func *progress,
		     unsigned p_bits, unsigned q_bits)
{
  mpz_t p0, p0q, r;
  unsigned p0_bits;
  unsigned a;

  switch (q_bits)
    {
    case 160:
      if (p_bits < DSA_SHA1_MIN_P_BITS)
	return 0;
      break;
    case 256:
      if (p_bits < DSA_SHA256_MIN_P_BITS)
	return 0;
      break;
    default:
      return 0;
    }

  mpz_init (p0);
  mpz_init (p0q);
  mpz_init (r);

  nettle_random_prime (pub->q, q_bits, 0, random_ctx, random,
		       progress_ctx, progress);

  p0_bits = (p_bits + 3)/2;
  
  nettle_random_prime (p0, p0_bits, 0,
		       random_ctx, random,
		       progress_ctx, progress);

  if (progress)
    progress (progress_ctx, 'q');
  
  /* Generate p = 2 r q p0 + 1, such that 2^{n-1} < p < 2^n.
   *
   * We select r in the range i + 1 < r <= 2i, with i = floor (2^{n-2} / (p0 q). */

  mpz_mul (p0q, p0, pub->q);

  _nettle_generate_pocklington_prime (pub->p, r, p_bits, 0,
				      random_ctx, random,
				      p0, pub->q, p0q);

  if (progress)
    progress (progress_ctx, 'p');

  mpz_mul (r, r, p0);

  for (a = 2; ; a++)
    {
      mpz_set_ui (pub->g, a);
      mpz_powm (pub->g, pub->g, r, pub->p);
      if (mpz_cmp_ui (pub->g, 1) != 0)
	break;
    }

  if (progress)
    progress (progress_ctx, 'g');

  mpz_set(r, pub->q);
  mpz_sub_ui(r, r, 2);
  nettle_mpz_random(key->x, random_ctx, random, r);

  mpz_add_ui(key->x, key->x, 1);

  mpz_powm(pub->y, pub->g, key->x, pub->p);

  if (progress)
    progress (progress_ctx, '\n');
  
  mpz_clear (p0);
  mpz_clear (p0q);
  mpz_clear (r);

  return 1;
}
