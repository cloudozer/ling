/* ecc-point-mul-g.c */

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

void
ecc_point_mul_g (struct ecc_point *r, const struct ecc_scalar *n)
{
  TMP_DECL(scratch, mp_limb_t, 3*ECC_MAX_SIZE + ECC_MUL_G_ITCH (ECC_MAX_SIZE));
  mp_limb_t size = r->ecc->size;
  mp_size_t itch = 3*size + ECC_MUL_G_ITCH (size);

  assert (r->ecc == n->ecc);

  TMP_ALLOC (scratch, itch);

  ecc_mul_g (r->ecc, scratch, n->p, scratch + 3*size);
  ecc_j_to_a (r->ecc, 1, r->p, scratch, scratch + 3*size);
}
