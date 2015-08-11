/* pkcs1-encrypt.c
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

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "pkcs1.h"

#include "bignum.h"
#include "nettle-internal.h"

int
pkcs1_encrypt (unsigned key_size,
	       /* For padding */
	       void *random_ctx, nettle_random_func *random,
	       unsigned length, const uint8_t *message,
	       mpz_t m)
{
  TMP_DECL(em, uint8_t, NETTLE_MAX_BIGNUM_SIZE);
  unsigned padding;
  unsigned i;

  /* The message is encoded as a string of the same length as the
   * modulo n, of the form
   *
   *   00 02 pad 00 message
   *
   * where padding should be at least 8 pseudorandomly generated
   * *non-zero* octets. */
     
  if (length + 11 > key_size)
    /* Message too long for this key. */
    return 0;

  /* At least 8 octets of random padding */
  padding = key_size - length - 3;
  assert(padding >= 8);
  
  TMP_ALLOC(em, key_size - 1);
  em[0] = 2;

  random(random_ctx, padding, em + 1);

  /* Replace 0-octets with 1 */
  for (i = 0; i<padding; i++)
    if (!em[i+1])
      em[i+1] = 1;

  em[padding+1] = 0;
  memcpy(em + padding + 2, message, length);

  nettle_mpz_set_str_256_u(m, key_size - 1, em);
  return 1;
}
