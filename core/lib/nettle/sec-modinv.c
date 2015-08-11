/* sec-modinv.c */

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

#include "ecc-internal.h"

static void
cnd_neg (int cnd, mp_limb_t *rp, const mp_limb_t *ap, mp_size_t n)
{
  mp_limb_t cy = (cnd != 0);
  mp_limb_t mask = -cy;
  mp_size_t i;

  for (i = 0; i < n; i++)
    {
      mp_limb_t r = (ap[i] ^ mask) + cy;
      cy = r < cy;
      rp[i] = r;
    }
}

static void
cnd_swap (int cnd, mp_limb_t *ap, mp_limb_t *bp, mp_size_t n)
{
  mp_limb_t mask = - (mp_limb_t) (cnd != 0);
  mp_size_t i;
  for (i = 0; i < n; i++)
    {
      mp_limb_t a, b, t;
      a = ap[i];
      b = bp[i];
      t = (a ^ b) & mask;
      ap[i] = a ^ t;
      bp[i] = b ^ t;
    }
}

/* Compute a^{-1} mod m, with running time depending only on the size.
   Also needs (m+1)/2, and m must be odd. */
void
sec_modinv (mp_limb_t *vp, mp_limb_t *ap, mp_size_t n,
	    const mp_limb_t *mp, const mp_limb_t *mp1h, mp_size_t bit_size,
	    mp_limb_t *scratch)
{
#define bp scratch
#define dp (scratch + n)
#define up (scratch + 2*n)

  /* Avoid the mp_bitcnt_t type for compatibility with older GMP
     versions. */  
  unsigned i;

  /* Maintain

       a = u * orig_a (mod m)
       b = v * orig_a (mod m)

     and b odd at all times. Initially,

       a = a_orig, u = 1
       b = m,      v = 0
     */

  assert (ap != vp);

  up[0] = 1;
  mpn_zero (up+1, n - 1);
  mpn_copyi (bp, mp, n);
  mpn_zero (vp, n);

  for (i = bit_size + GMP_NUMB_BITS * n; i-- > 0; )
    {
      mp_limb_t odd, swap, cy;
      
      /* Always maintain b odd. The logic of the iteration is as
	 follows. For a, b:

	   odd = a & 1
	   a -= odd * b
	   if (underflow from a-b)
	     {
	       b += a, assigns old a
	       a = B^n-a
	     }
	   
	   a /= 2

	 For u, v:

	   if (underflow from a - b)
	     swap u, v
	   u -= odd * v
	   if (underflow from u - v)
	     u += m

	   u /= 2
	   if (a one bit was shifted out)
	     u += (m+1)/2

	 As long as a > 0, the quantity

	   (bitsize of a) + (bitsize of b)

	 is reduced by at least one bit per iteration, hence after
         (bit_size of orig_a) + (bit_size of m) - 1 iterations we
         surely have a = 0. Then b = gcd(orig_a, m) and if b = 1 then
         also v = orig_a^{-1} (mod m)
      */

      assert (bp[0] & 1);
      odd = ap[0] & 1;

      /* Which variant is fastest depends on the speed of the various
	 cnd_* functions. Assembly implementation would help. */
#if 1
      swap = cnd_sub_n (odd, ap, bp, n);
      cnd_add_n (swap, bp, ap, n);
      cnd_neg (swap, ap, ap, n);
#else
      swap = odd & mpn_sub_n (dp, ap, bp, n);
      cnd_copy (swap, bp, ap, n);
      cnd_neg (swap, dp, dp, n);
      cnd_copy (odd, ap, dp, n);
#endif

#if 1
      cnd_swap (swap, up, vp, n);
      cy = cnd_sub_n (odd, up, vp, n);
      cy -= cnd_add_n (cy, up, mp, n);
#else
      cy = cnd_sub_n (odd, up, vp, n);
      cnd_add_n (swap, vp, up, n);
      cnd_neg (swap, up, up, n);
      cnd_add_n (cy ^ swap, up, mp, n);
#endif
      cy = mpn_rshift (ap, ap, n, 1);
      assert (cy == 0);
      cy = mpn_rshift (up, up, n, 1);
      cy = cnd_add_n (cy, up, mp1h, n);
      assert (cy == 0);
    }
  assert ( (ap[0] | ap[n-1]) == 0);
#undef bp
#undef dp
#undef up
}
