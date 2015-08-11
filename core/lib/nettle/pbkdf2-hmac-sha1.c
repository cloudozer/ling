/* pbkdf2-hmac-sha1.c
 *
 * PKCS #5 PBKDF2 used with HMAC-SHA1, see RFC 2898.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2012 Simon Josefsson
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

#include "pbkdf2.h"

#include "hmac.h"

void
pbkdf2_hmac_sha1 (unsigned key_length, const uint8_t *key,
		  unsigned iterations,
		  unsigned salt_length, const uint8_t *salt,
		  unsigned length, uint8_t *dst)
{
  struct hmac_sha1_ctx sha1ctx;

  hmac_sha1_set_key (&sha1ctx, key_length, key);
  PBKDF2 (&sha1ctx, hmac_sha1_update, hmac_sha1_digest,
	  SHA1_DIGEST_SIZE, iterations, salt_length, salt, length, dst);
}
