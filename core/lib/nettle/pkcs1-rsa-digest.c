/* pkcs1-rsa-digest.c
 *
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2001, 2003, 2012 Niels Möller
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

#include "pkcs1.h"

#include "bignum.h"
#include "nettle-internal.h"

int
pkcs1_rsa_digest_encode(mpz_t m, unsigned key_size,
			unsigned di_length, const uint8_t *digest_info)
{
  TMP_DECL(em, uint8_t, NETTLE_MAX_BIGNUM_SIZE);
  TMP_ALLOC(em, key_size);

  if (_pkcs1_signature_prefix(key_size, em,
			      di_length, digest_info, 0))
    {
      nettle_mpz_set_str_256_u(m, key_size, em);
      return 1;
    }
  else
    return 0;
}
