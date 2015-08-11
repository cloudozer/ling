/* dsa.h
 *
 * The DSA publickey algorithm.
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

#include "dsa.h"

#include "bignum.h"

void
dsa_public_key_init(struct dsa_public_key *key)
{
  mpz_init(key->p);
  mpz_init(key->q);
  mpz_init(key->g);
  mpz_init(key->y);
}

void
dsa_public_key_clear(struct dsa_public_key *key)
{
  mpz_clear(key->p);
  mpz_clear(key->q);
  mpz_clear(key->g);
  mpz_clear(key->y);
}


void
dsa_private_key_init(struct dsa_private_key *key)
{
  mpz_init(key->x);
}

void
dsa_private_key_clear(struct dsa_private_key *key)
{
  mpz_clear(key->x);
}


void
dsa_signature_init(struct dsa_signature *signature)
{
  mpz_init(signature->r);
  mpz_init(signature->s);
}

void
dsa_signature_clear(struct dsa_signature *signature)
{
  mpz_clear(signature->r);
  mpz_clear(signature->s);
}
