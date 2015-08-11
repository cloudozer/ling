/* rsa-encrypt.c
 *
 * The RSA publickey algorithm. PKCS#1 encryption.
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

#include "pkcs1.h"

int
rsa_encrypt(const struct rsa_public_key *key,
	    /* For padding */
	    void *random_ctx, nettle_random_func *random,
	    unsigned length, const uint8_t *message,
	    mpz_t gibberish)
{
  if (pkcs1_encrypt (key->size, random_ctx, random,
		     length, message, gibberish))
    {
      mpz_powm(gibberish, gibberish, key->e, key->n);
      return 1;
    }
  else
    return 0;
}
