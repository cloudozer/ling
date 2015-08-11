/* ecc-random.c */

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

#include <assert.h>

#include "ecc.h"
#include "ecc-internal.h"
#include "nettle-internal.h"

static int
zero_p (const struct ecc_curve *ecc,
	const mp_limb_t *xp)
{
  mp_limb_t t;
  mp_size_t i;

  for (i = t = 0; i < ecc->size; i++)
    t |= xp[i];

  return t == 0;
}

static int
ecdsa_in_range (const struct ecc_curve *ecc,
		const mp_limb_t *xp, mp_limb_t *scratch)
{
  /* Check if 0 < x < q, with data independent timing. */
  return !zero_p (ecc, xp)
    & (mpn_sub_n (scratch, xp, ecc->q, ecc->size) != 0);
}

void
ecc_modq_random (const struct ecc_curve *ecc, mp_limb_t *xp,
		 void *ctx, nettle_random_func *random, mp_limb_t *scratch)
{
  uint8_t *buf = (uint8_t *) scratch;
  unsigned nbytes = (ecc->bit_size + 7)/8;

  /* The bytes ought to fit in the scratch area, unless we have very
     unusual limb and byte sizes. */
  assert (nbytes <= ecc->size * sizeof (mp_limb_t));

  do
    {
      /* q and p are of the same bitsize. */
      random (ctx, nbytes, buf);
      buf[0] &= 0xff >> (nbytes * 8 - ecc->bit_size);

      mpn_set_base256 (xp, ecc->size, buf, nbytes);
    }
  while (!ecdsa_in_range (ecc, xp, scratch));
}

void
ecc_scalar_random (struct ecc_scalar *x,
		   void *random_ctx, nettle_random_func *random)
{
  TMP_DECL (scratch, mp_limb_t, ECC_MODQ_RANDOM_ITCH (ECC_MAX_SIZE));
  TMP_ALLOC (scratch, ECC_MODQ_RANDOM_ITCH (x->ecc->size));

  ecc_modq_random (x->ecc, x->p, random_ctx, random, scratch);
}


