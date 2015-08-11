/* rsa-decrypt.c
 *
 * The RSA publickey algorithm. PKCS#1 encryption.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2001, 2012 Niels Möller
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

#include "pkcs1.h"

int
rsa_decrypt(const struct rsa_private_key *key,
	    unsigned *length, uint8_t *message,
	    const mpz_t gibberish)
{
  mpz_t m;
  int res;

  mpz_init(m);
  rsa_compute_root(key, m, gibberish);

  res = pkcs1_decrypt (key->size, m, length, message);
  mpz_clear(m);
  return res;
}
