/* pbkdf2.c
 *
 * PKCS #5 password-based key derivation function PBKDF2, see RFC 2898.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2012 Simon Josefsson, Niels Möller
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

#include "pbkdf2.h"

#include "macros.h"
#include "memxor.h"
#include "nettle-internal.h"

void
pbkdf2 (void *mac_ctx,
	nettle_hash_update_func *update,
	nettle_hash_digest_func *digest,
	unsigned digest_size, unsigned iterations,
	unsigned salt_length, const uint8_t *salt,
	unsigned length, uint8_t *dst)
{
  TMP_DECL(U, uint8_t, NETTLE_MAX_HASH_DIGEST_SIZE);
  TMP_DECL(T, uint8_t, NETTLE_MAX_HASH_DIGEST_SIZE);
  
  unsigned i;

  assert (iterations > 0);

  if (length == 0)
    return;

  TMP_ALLOC (U, digest_size);
  TMP_ALLOC (T, digest_size);

  for (i = 1;;
       i++, dst += digest_size, length -= digest_size)
    {
      uint8_t tmp[4];
      uint8_t *prev;
      unsigned u;
      
      WRITE_UINT32 (tmp, i);
      
      update (mac_ctx, salt_length, salt);
      update (mac_ctx, sizeof(tmp), tmp);
      digest (mac_ctx, digest_size, T);

      prev = T;
      
      for (u = 1; u < iterations; u++, prev = U)
	{
	  update (mac_ctx, digest_size, prev);
	  digest (mac_ctx, digest_size, U);

	  memxor (T, U, digest_size);
	}

      if (length <= digest_size)
	{
	  memcpy (dst, T, length);
	  return;
	}
      memcpy (dst, T, digest_size);
    }
}
