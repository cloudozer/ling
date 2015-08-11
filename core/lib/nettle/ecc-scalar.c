/* ecc-scalar.c */

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

#include "ecc.h"
#include "ecc-internal.h"

void
ecc_scalar_init (struct ecc_scalar *s, const struct ecc_curve *ecc)
{
  s->ecc = ecc;
  s->p = gmp_alloc_limbs (ecc->size);
}

void
ecc_scalar_clear (struct ecc_scalar *s)
{
  gmp_free_limbs (s->p, s->ecc->size);
}

int
ecc_scalar_set (struct ecc_scalar *s, const mpz_t z)
{
  mp_size_t size = s->ecc->size;

  if (mpz_sgn (z) <= 0 || mpz_limbs_cmp (z, s->ecc->q, size) >= 0)
    return 0;

  mpz_limbs_copy (s->p, z, size);
  return 1;
}

void
ecc_scalar_get (const struct ecc_scalar *s, mpz_t z)
{
  mpz_set_n (z, s->p, s->ecc->size);  
}
