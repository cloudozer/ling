/* ecc-mul-g.c */

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

mp_size_t
ecc_mul_g_itch (const struct ecc_curve *ecc)
{
  /* Needs 3*ecc->size + scratch for ecc_add_jja. */
  return ECC_MUL_G_ITCH (ecc->size);
}

void
ecc_mul_g (const struct ecc_curve *ecc, mp_limb_t *r,
	   const mp_limb_t *np, mp_limb_t *scratch)
{
  /* Scratch need determined by the ecc_add_jja call. Current total is
     9 * ecc->size, at most 648 bytes. */
#define tp scratch
#define scratch_out (scratch + 3*ecc->size)

  unsigned k, c;
  unsigned i, j;
  unsigned bit_rows;

  int is_zero;

  k = ecc->pippenger_k;
  c = ecc->pippenger_c;

  bit_rows = (ecc->bit_size + k - 1) / k;
  
  mpn_zero (r, 3*ecc->size);
  
  for (i = k, is_zero = 1; i-- > 0; )
    {
      ecc_dup_jj (ecc, r, r, scratch);
      for (j = 0; j * c < bit_rows; j++)
	{
	  unsigned bits;
	  /* Avoid the mp_bitcnt_t type for compatibility with older GMP
	     versions. */
	  unsigned bit_index;
	  
	  /* Extract c bits from n, stride k, starting at i + kcj,
	     ending at i + k (cj + c - 1)*/
	  for (bits = 0, bit_index = i + k*(c*j+c); bit_index > i + k*c*j; )
	    {
	      mp_size_t limb_index;
	      unsigned shift;
	      
	      bit_index -= k;

	      limb_index = bit_index / GMP_NUMB_BITS;
	      if (limb_index >= ecc->size)
		continue;

	      shift = bit_index % GMP_NUMB_BITS;
	      bits = (bits << 1) | ((np[limb_index] >> shift) & 1);
	    }
	  sec_tabselect (tp, 2*ecc->size,
			 (ecc->pippenger_table
			  + (2*ecc->size * (mp_size_t) j << c)),
			 1<<c, bits);
	  cnd_copy (is_zero, r, tp, 2*ecc->size);
	  cnd_copy (is_zero, r + 2*ecc->size, ecc->unit, ecc->size);
	  
	  ecc_add_jja (ecc, tp, r, tp, scratch_out);
	  /* Use the sum when valid. ecc_add_jja produced garbage if
	     is_zero != 0 or bits == 0, . */	  
	  cnd_copy (bits & (is_zero - 1), r, tp, 3*ecc->size);
	  is_zero &= (bits == 0);
	}
    }
#undef tp
#undef scratch_out
}
