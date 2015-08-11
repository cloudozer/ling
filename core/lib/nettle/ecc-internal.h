/* ecc-internal.h */

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

#ifndef NETTLE_ECC_INTERNAL_H_INCLUDED
#define NETTLE_ECC_INTERNAL_H_INCLUDED

#include <gmp.h>

#include "nettle-types.h"
#include "ecc-curve.h"
#include "gmp-glue.h"

/* Name mangling */
#define ecc_generic_modp _nettle_ecc_generic_modp
#define ecc_generic_redc _nettle_ecc_generic_redc
#define ecc_generic_modq _nettle_ecc_generic_modq
#define ecc_modp_add _nettle_ecc_modp_add
#define ecc_modp_sub _nettle_ecc_modp_sub
#define ecc_modp_sub_1 _nettle_ecc_modp_sub_1
#define ecc_modp_mul_1 _nettle_ecc_modp_mul_1
#define ecc_modp_addmul_1 _nettle_ecc_modp_addmul_1
#define ecc_modp_submul_1 _nettle_ecc_modp_submul_1
#define ecc_modp_mul _nettle_ecc_modp_mul
#define ecc_modp_sqr _nettle_ecc_modp_sqr
#define ecc_modp_inv _nettle_ecc_modp_inv
#define ecc_modq_mul _nettle_ecc_modq_mul
#define ecc_modq_add _nettle_ecc_modq_add
#define ecc_modq_inv _nettle_ecc_modq_inv
#define ecc_modq_random _nettle_ecc_modq_random
#define ecc_mod _nettle_ecc_mod
#define ecc_hash _nettle_ecc_hash
#define cnd_copy _nettle_cnd_copy
#define sec_add_1 _nettle_sec_add_1
#define sec_sub_1 _nettle_sec_sub_1
#define sec_tabselect _nettle_sec_tabselect
#define sec_modinv _nettle_sec_modinv

#define ECC_MAX_SIZE ((521 + GMP_NUMB_BITS - 1) / GMP_NUMB_BITS)

/* Window size for ecc_mul_a. Using 4 bits seems like a good choice,
   for both Intel x86_64 and ARM Cortex A9. For the larger curves, of
   384 and 521 bits, we could improve seepd by a few percent if we go
   up to 5 bits, but I don't think that's worth doubling the
   storage. */
#define ECC_MUL_A_WBITS 4

/* Reduces from 2*ecc->size to ecc->size. */
/* Required to return a result < 2q. This property is inherited by
   modp_mul and modp_add. */
typedef void ecc_mod_func (const struct ecc_curve *ecc, mp_limb_t *rp);

/* Represents an elliptic curve of the form

     y^2 = x^3 - 3x + b (mod p)
*/
struct ecc_curve
{
  unsigned short bit_size;
  /* Limb size of elements in the base field, size of a point is
     2*size in affine coordinates and 3*size in jacobian
     coordinates. */
  unsigned short size;
  unsigned short Bmodp_size;
  unsigned short Bmodq_size;
  unsigned short use_redc;
  /* +k if p+1 has k low zero limbs, -k if p-1 has k low zero
     limbs. */
  short redc_size;
  unsigned short pippenger_k;
  unsigned short pippenger_c;

  /* The prime p. */
  const mp_limb_t *p;
  const mp_limb_t *b;
  /* Group order. */
  const mp_limb_t *q;
  /* Generator, x coordinate followed by y (affine coordinates). */
  const mp_limb_t *g;
  /* Generator with coordinates in Montgomery form. */
  const mp_limb_t *redc_g;

  ecc_mod_func *modp;
  ecc_mod_func *redc;
  ecc_mod_func *reduce;
  ecc_mod_func *modq;

  /* B^size mod p. Expected to have at least 32 leading zeros
     (equality for secp_256r1). */
  const mp_limb_t *Bmodp;
  /* 2^{bit_size} - p, same value as above, but shifted. */
  const mp_limb_t *Bmodp_shifted;
  /* (p+1)/2 */
  const mp_limb_t *pp1h;
  /* p +/- 1, for redc, excluding |redc_size| low limbs. */
  const mp_limb_t *redc_ppm1;
  /* For redc, same as Bmodp, otherwise 1. */
  const mp_limb_t *unit;

  /* Similarly, B^size mod q */
  const mp_limb_t *Bmodq;
  /* 2^{bit_size} - q, same value as above, but shifted. */
  const mp_limb_t *Bmodq_shifted;
  /* (q+1)/2 */
  const mp_limb_t *qp1h;
  
  /* Tables for multiplying by the generator, size determined by k and
     c. The first 2^c entries are defined by

       T[  j_0 +   j_1 2 +     ... + j_{c-1} 2^{c-1} ]
         = j_0 g + j_1 2^k g + ... + j_{c-1} 2^{k(c-1)} g

     The following entries differ by powers of 2^{kc},

       T[i] = 2^{kc} T[i-2^c]
  */  
  const mp_limb_t *pippenger_table;
};

/* In-place reduction. */
ecc_mod_func ecc_generic_modp;
ecc_mod_func ecc_generic_redc;
ecc_mod_func ecc_generic_modq;


void
ecc_modp_add (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap, const mp_limb_t *bp);
void
ecc_modp_sub (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap, const mp_limb_t *bp);

void
ecc_modp_sub_1 (const struct ecc_curve *ecc, mp_limb_t *rp,
		const mp_limb_t *ap, mp_limb_t b);

void
ecc_modp_mul_1 (const struct ecc_curve *ecc, mp_limb_t *rp,
		const mp_limb_t *ap, const mp_limb_t b);

void
ecc_modp_addmul_1 (const struct ecc_curve *ecc, mp_limb_t *rp,
		   const mp_limb_t *ap, mp_limb_t b);
void
ecc_modp_submul_1 (const struct ecc_curve *ecc, mp_limb_t *rp,
		   const mp_limb_t *ap, mp_limb_t b);

/* NOTE: mul and sqr needs 2*ecc->size limbs at rp */
void
ecc_modp_mul (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap, const mp_limb_t *bp);

void
ecc_modp_sqr (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap);

void
ecc_modp_inv (const struct ecc_curve *ecc, mp_limb_t *rp, mp_limb_t *ap,
	      mp_limb_t *scratch);

/* mod q operations. */
void
ecc_modq_mul (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap, const mp_limb_t *bp);
void
ecc_modq_add (const struct ecc_curve *ecc, mp_limb_t *rp,
	      const mp_limb_t *ap, const mp_limb_t *bp);

void
ecc_modq_inv (const struct ecc_curve *ecc, mp_limb_t *rp, mp_limb_t *ap,
	      mp_limb_t *scratch);

void
ecc_modq_random (const struct ecc_curve *ecc, mp_limb_t *xp,
		 void *ctx, nettle_random_func *random, mp_limb_t *scratch);

void
ecc_mod (mp_limb_t *rp, mp_size_t rn, mp_size_t mn,
	 const mp_limb_t *bp, mp_size_t bn,
	 const mp_limb_t *b_shifted, unsigned shift);

void
ecc_hash (const struct ecc_curve *ecc,
	  mp_limb_t *hp,
	  unsigned length, const uint8_t *digest);

void
cnd_copy (int cnd, mp_limb_t *rp, const mp_limb_t *ap, mp_size_t n);

mp_limb_t
sec_add_1 (mp_limb_t *rp, mp_limb_t *ap, mp_size_t n, mp_limb_t b);

mp_limb_t
sec_sub_1 (mp_limb_t *rp, mp_limb_t *ap, mp_size_t n, mp_limb_t b);

void
sec_tabselect (mp_limb_t *rp, mp_size_t rn,
	       const mp_limb_t *table, unsigned tn,
	       unsigned k);

void
sec_modinv (mp_limb_t *vp, mp_limb_t *ap, mp_size_t n,
	    const mp_limb_t *mp, const mp_limb_t *mp1h, mp_size_t bit_size,
	    mp_limb_t *scratch);

/* Current scratch needs: */
#define ECC_MODINV_ITCH(size) (3*(size))
#define ECC_J_TO_A_ITCH(size) (5*(size))
#define ECC_DUP_JA_ITCH(size) (5*(size))
#define ECC_DUP_JJ_ITCH(size) (5*(size))
#define ECC_ADD_JJA_ITCH(size) (6*(size))
#define ECC_ADD_JJJ_ITCH(size) (8*(size))
#define ECC_MUL_G_ITCH(size) (9*(size))
#if ECC_MUL_A_WBITS == 0
#define ECC_MUL_A_ITCH(size) (12*(size))
#else
#define ECC_MUL_A_ITCH(size) \
  (((3 << ECC_MUL_A_WBITS) + 11) * (size))
#endif
#define ECC_ECDSA_SIGN_ITCH(size) (12*(size))
#define ECC_ECDSA_VERIFY_ITCH(size) \
  (6*(size) + ECC_MUL_A_ITCH ((size)))
#define ECC_MODQ_RANDOM_ITCH(size) (size)
#define ECC_HASH_ITCH(size) (1+(size))

#endif /* NETTLE_ECC_INTERNAL_H_INCLUDED */
