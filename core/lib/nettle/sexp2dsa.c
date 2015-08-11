/* sexp2dsa.c
 *
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

#include <string.h>

#include "dsa.h"

#include "bignum.h"
#include "sexp.h"

#define GET(x, l, v)				\
do {						\
  if (!nettle_mpz_set_sexp((x), (l), (v))	\
      || mpz_sgn(x) <= 0)			\
    return 0;					\
} while(0)

/* Iterator should point past the algorithm tag, e.g.
 *
 *   (public-key (dsa (p |xxxx|) ...)
 *                    ^ here
 */

int
dsa_keypair_from_sexp_alist(struct dsa_public_key *pub,
			    struct dsa_private_key *priv,
			    unsigned p_max_bits,
			    unsigned q_bits,
			    struct sexp_iterator *i)
{
  static const uint8_t * const names[5]
    = { "p", "q", "g", "y", "x" };
  struct sexp_iterator values[5];
  unsigned nvalues = priv ? 5 : 4;
  
  if (!sexp_iterator_assoc(i, nvalues, names, values))
    return 0;

  if (priv)
    GET(priv->x, q_bits, &values[4]);
  
  GET(pub->p, p_max_bits, &values[0]);
  GET(pub->q, q_bits, &values[1]);
  if (mpz_sizeinbase(pub->q, 2) != q_bits)
    return 0;
  GET(pub->g, p_max_bits, &values[2]);
  GET(pub->y, p_max_bits, &values[3]);
  
  return 1;
}

int
dsa_sha1_keypair_from_sexp(struct dsa_public_key *pub,
			   struct dsa_private_key *priv,
			   unsigned p_max_bits, 
			   unsigned length, const uint8_t *expr)
{
  struct sexp_iterator i;

  return sexp_iterator_first(&i, length, expr)
    && sexp_iterator_check_type(&i, priv ? "private-key" : "public-key")
    && sexp_iterator_check_type(&i, "dsa")
    && dsa_keypair_from_sexp_alist(pub, priv, p_max_bits, DSA_SHA1_Q_BITS, &i);
}

int
dsa_sha256_keypair_from_sexp(struct dsa_public_key *pub,
			     struct dsa_private_key *priv,
			     unsigned p_max_bits, 
			     unsigned length, const uint8_t *expr)
{
  struct sexp_iterator i;

  return sexp_iterator_first(&i, length, expr)
    && sexp_iterator_check_type(&i, priv ? "private-key" : "public-key")
    && sexp_iterator_check_type(&i, "dsa-sha256")
    && dsa_keypair_from_sexp_alist(pub, priv, p_max_bits, DSA_SHA256_Q_BITS, &i);
}

int
dsa_signature_from_sexp(struct dsa_signature *rs,
			struct sexp_iterator *i,
			unsigned q_bits)
{
  static const uint8_t * const names[2] = { "r", "s" };
  struct sexp_iterator values[2];

  if (!sexp_iterator_assoc(i, 2, names, values))
    return 0;

  GET(rs->r, q_bits, &values[0]);
  GET(rs->s, q_bits, &values[1]);

  return 1;
}
