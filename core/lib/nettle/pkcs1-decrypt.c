/* pkcs1-decrypt.c
 *
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

#include <string.h>

#include "pkcs1.h"

#include "bignum.h"
#include "nettle-internal.h"

int
pkcs1_decrypt (unsigned key_size,
	       const mpz_t m,
	       unsigned *length, uint8_t *message)
{
  TMP_DECL(em, uint8_t, NETTLE_MAX_BIGNUM_SIZE);
  uint8_t *terminator;
  unsigned padding;
  unsigned message_length;

  TMP_ALLOC(em, key_size);
  nettle_mpz_get_str_256(key_size, em, m);

  /* Check format */
  if (em[0] || em[1] != 2)
    return 0;

  terminator = memchr(em + 2, 0, key_size - 2);

  if (!terminator)
    return 0;
  
  padding = terminator - (em + 2);
  if (padding < 8)
    return 0;

  message_length = key_size - 3 - padding;

  if (*length < message_length)
    return 0;
  
  memcpy(message, terminator + 1, message_length);
  *length = message_length;

  return 1;
}
	       
