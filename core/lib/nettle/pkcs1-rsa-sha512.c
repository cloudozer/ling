/* pkcs1-rsa-sha512.c
 *
 * PKCS stuff for rsa-sha512.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2001, 2003, 2006, 2010 Niels Möller
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

#include "rsa.h"

#include "bignum.h"
#include "pkcs1.h"

#include "nettle-internal.h"

/* From RFC 3447, Public-Key Cryptography Standards (PKCS) #1: RSA
 * Cryptography Specifications Version 2.1.
 *
 *     id-sha512    OBJECT IDENTIFIER ::=
 *       {joint-iso-itu-t(2) country(16) us(840) organization(1)
 *         gov(101) csor(3) nistalgorithm(4) hashalgs(2) 3}
 */

static const uint8_t
sha512_prefix[] =
{
  /* 19 octets prefix, 64 octets hash, total 83 */
  0x30,      81, /* SEQUENCE */
    0x30,    13, /* SEQUENCE */
      0x06,   9, /* OBJECT IDENTIFIER */
        0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x03,
      0x05,   0, /* NULL */
    0x04,    64  /* OCTET STRING */
      /* Here comes the raw hash value, 64 octets */
};

int
pkcs1_rsa_sha512_encode(mpz_t m, unsigned key_size, struct sha512_ctx *hash)
{
  uint8_t *p;
  TMP_DECL(em, uint8_t, NETTLE_MAX_BIGNUM_SIZE);
  TMP_ALLOC(em, key_size);

  p = _pkcs1_signature_prefix(key_size, em,
			      sizeof(sha512_prefix),
			      sha512_prefix,
			      SHA512_DIGEST_SIZE);
  if (p)
    {
      sha512_digest(hash, SHA512_DIGEST_SIZE, p);
      nettle_mpz_set_str_256_u(m, key_size, em);
      return 1;
    }
  else
    return 0;
}

int
pkcs1_rsa_sha512_encode_digest(mpz_t m, unsigned key_size, const uint8_t *digest)
{
  uint8_t *p;
  TMP_DECL(em, uint8_t, NETTLE_MAX_BIGNUM_SIZE);
  TMP_ALLOC(em, key_size);

  p = _pkcs1_signature_prefix(key_size, em,
			      sizeof(sha512_prefix),
			      sha512_prefix,
			      SHA512_DIGEST_SIZE);
  if (p)
    {
      memcpy(p, digest, SHA512_DIGEST_SIZE);
      nettle_mpz_set_str_256_u(m, key_size, em);
      return 1;
    }
  else
    return 0;
}
