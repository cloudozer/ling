/* ecc-point.c */

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
ecc_point_init (struct ecc_point *p, const struct ecc_curve *ecc)
{
  p->ecc = ecc;
  p->p = gmp_alloc_limbs (2*ecc->size);
}

void
ecc_point_clear (struct ecc_point *p)
{
  gmp_free_limbs (p->p, 2*p->ecc->size);
}

int
ecc_point_set (struct ecc_point *p, const mpz_t x, const mpz_t y)
{
  mp_size_t size;  
  mpz_t lhs, rhs;
  mpz_t t;
  int res;

  size = p->ecc->size;
  
  if (mpz_sgn (x) < 0 || mpz_limbs_cmp (x, p->ecc->p, size) >= 0
      || mpz_sgn (y) < 0 || mpz_limbs_cmp (y, p->ecc->p, size) >= 0)
    return 0;

  mpz_init (lhs);
  mpz_init (rhs);

  /* Check that y^2 = x^3 - 3*x + b (mod p) */
  mpz_mul (lhs, y, y);
  mpz_mul (rhs, x, x);
  mpz_sub_ui (rhs, rhs, 3);
  mpz_mul (rhs, rhs, x);
  mpz_add (rhs, rhs, mpz_roinit_n (t, p->ecc->b, size));

  res = mpz_congruent_p (lhs, rhs, mpz_roinit_n (t, p->ecc->p, size));

  mpz_clear (lhs);
  mpz_clear (rhs);

  if (!res)
    return 0;

  mpz_limbs_copy (p->p, x, size);
  mpz_limbs_copy (p->p + size, y, size);

  return 1;
}

void
ecc_point_get (const struct ecc_point *p, mpz_t x, mpz_t y)
{
  mp_size_t size = p->ecc->size;
  if (x)
    mpz_set_n (x, p->p, size);
  if (y)
    mpz_set_n (y, p->p + size, size);
}
