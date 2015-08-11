/* rsa.c
 *
 * The RSA publickey algorithm.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2001 Niels Möller
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

void
rsa_public_key_init(struct rsa_public_key *key)
{
  mpz_init(key->n);
  mpz_init(key->e);

  /* Not really necessary, but it seems cleaner to initialize all the
   * storage. */
  key->size = 0;
}

void
rsa_public_key_clear(struct rsa_public_key *key)
{
  mpz_clear(key->n);
  mpz_clear(key->e);
}

/* Computes the size, in octets, of a the modulo. Returns 0 if the
 * modulo is too small to be useful. */

unsigned
_rsa_check_size(mpz_t n)
{
  /* Round upwards */
  unsigned size = (mpz_sizeinbase(n, 2) + 7) / 8;

  if (size < RSA_MINIMUM_N_OCTETS)
    return 0;

  return size;
}

int
rsa_public_key_prepare(struct rsa_public_key *key)
{
  key->size = _rsa_check_size(key->n);
  
  return (key->size > 0);
}
