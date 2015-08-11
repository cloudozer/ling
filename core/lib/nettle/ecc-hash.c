/* ecdsa-hash.c */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2013 Niels Möller
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

/* Development of Nettle's ECC support was funded by the .SE Internet Fund. */

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include "ecc-internal.h"

/* Convert hash value to an integer. If the digest is larger than
   the ecc bit size, then we must truncate it and use the leftmost
   bits. */

/* NOTE: We don't considered the hash value to be secret, so it's ok
   if the running time of this conversion depends on h.

   Requires ecc->size + 1 limbs, the extra limb may be needed for
   unusual limb sizes.
*/
void
ecc_hash (const struct ecc_curve *ecc,
	  mp_limb_t *hp,
	  unsigned length, const uint8_t *digest)
{
  if (length > ((unsigned) ecc->bit_size + 7) / 8)
    length = (ecc->bit_size + 7) / 8;

  mpn_set_base256 (hp, ecc->size + 1, digest, length);

  if (8 * length > ecc->bit_size)
    /* We got a few extra bits, at the low end. Discard them. */
    mpn_rshift (hp, hp, ecc->size + 1, 8*length - ecc->bit_size);
}
