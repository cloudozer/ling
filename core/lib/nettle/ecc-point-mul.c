/* ecc-point-mul.c */

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

void
ecc_point_mul (struct ecc_point *r, const struct ecc_scalar *n,
	       const struct ecc_point *p)
{
  mp_limb_t size = p->ecc->size;
  mp_size_t itch = 3*size + ECC_MUL_A_ITCH (size);
  mp_limb_t *scratch = gmp_alloc_limbs (itch);

  assert (n->ecc == p->ecc);
  assert (r->ecc == p->ecc);

  ecc_mul_a (p->ecc, 1, scratch, n->p, p->p, scratch + 3*size);
  ecc_j_to_a (r->ecc, 1, r->p, scratch, scratch + 3*size);
  gmp_free_limbs (scratch, itch);
}
