/* rsa-verify.c
 *
 * Verifying RSA signatures.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2001, 2003 Niels Möller
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

#include "rsa.h"

#include "bignum.h"

int
_rsa_verify(const struct rsa_public_key *key,
	    const mpz_t m,
	    const mpz_t s)
{
  int res;
  
  mpz_t m1;
  
  if ( (mpz_sgn(s) <= 0)
       || (mpz_cmp(s, key->n) >= 0) )
    return 0;
       
  mpz_init(m1);
  
  mpz_powm(m1, s, key->e, key->n);

  res = !mpz_cmp(m, m1);

  mpz_clear(m1);

  return res;
}
