/* ecc-ecdsa-verify.c */

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
#include <stdlib.h>

#include "ecdsa.h"
#include "ecc-internal.h"

int
ecdsa_verify (const struct ecc_point *pub,
	      unsigned length, const uint8_t *digest,
	      const struct dsa_signature *signature)
{
  mp_limb_t size = pub->ecc->size;
  mp_size_t itch = 2*size + ECC_ECDSA_VERIFY_ITCH (size);
  /* For ECC_MUL_A_WBITS == 0, at most 1512 bytes. With
     ECC_MUL_A_WBITS == 4, currently needs 67 * ecc->size, at most
     4824 bytes. Don't use stack allocation for this. */
  mp_limb_t *scratch = gmp_alloc_limbs (itch);
  int res;

#define rp scratch
#define sp (scratch + size)
#define scratch_out (scratch + 2*size)

  if (mpz_sgn (signature->r) <= 0 || mpz_size (signature->r) > size
      || mpz_sgn (signature->s) <= 0 || mpz_size (signature->s) > size)
    return 0;
  
  mpz_limbs_copy (rp, signature->r, size);
  mpz_limbs_copy (sp, signature->s, size);

  res = ecc_ecdsa_verify (pub->ecc, pub->p, length, digest, rp, sp, scratch_out);

  gmp_free_limbs (scratch, itch);

  return res;
#undef rp
#undef sp
#undef scratch_out
}
