/* dsa-sha256-verify.c
 *
 * The DSA publickey algorithm, using SHA-256 (FIPS186-3).
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2010 Niels Möller
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

#include <stdlib.h>

#include "dsa.h"

int
dsa_sha256_verify_digest(const struct dsa_public_key *key,
			 const uint8_t *digest,
			 const struct dsa_signature *signature)
{
  return _dsa_verify(key, SHA256_DIGEST_SIZE, digest, signature);
}

int
dsa_sha256_verify(const struct dsa_public_key *key,
		  struct sha256_ctx *hash,
		  const struct dsa_signature *signature)
{
  uint8_t digest[SHA256_DIGEST_SIZE];
  sha256_digest(hash, sizeof(digest), digest);

  return _dsa_verify(key, sizeof(digest), digest, signature);
}
