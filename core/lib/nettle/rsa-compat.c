/* rsa-compat.c
 *
 * The RSA publickey algorithm, RSAREF compatible interface.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2001 Niels Möller
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

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include "rsa-compat.h"

#include "bignum.h"
#include "md5.h"

int
R_SignInit(R_SIGNATURE_CTX *ctx,
           int digestAlgorithm)
{
  if (digestAlgorithm != DA_MD5)
    return RE_DIGEST_ALGORITHM;

  md5_init(&ctx->hash);

  return 0;
}

int
R_SignUpdate(R_SIGNATURE_CTX *ctx,
             const uint8_t *data,
             /* Length is an unsigned char according to rsaref.txt,
              * but that must be a typo. */
             unsigned length)
{
  md5_update(&ctx->hash, length, data);

  return RE_SUCCESS;
}

int
R_SignFinal(R_SIGNATURE_CTX *ctx,
            uint8_t *signature,
            unsigned *length,
            R_RSA_PRIVATE_KEY *key)
{
  struct rsa_private_key k;
  int res;
  
  nettle_mpz_init_set_str_256_u(k.p,
				MAX_RSA_MODULUS_LEN, key->prime[0]);
  nettle_mpz_init_set_str_256_u(k.q,
				MAX_RSA_MODULUS_LEN, key->prime[1]);
  nettle_mpz_init_set_str_256_u(k.a,
				MAX_RSA_MODULUS_LEN, key->primeExponent[0]);
  nettle_mpz_init_set_str_256_u(k.b,
				MAX_RSA_MODULUS_LEN, key->primeExponent[1]);
  nettle_mpz_init_set_str_256_u(k.c,
				MAX_RSA_MODULUS_LEN, key->coefficient);

  if (rsa_private_key_prepare(&k) && (k.size <= MAX_RSA_MODULUS_LEN))
    {
      mpz_t s;
      mpz_init(s);

      if (rsa_md5_sign(&k, &ctx->hash, s))
	{
	  nettle_mpz_get_str_256(k.size, signature, s);
	  *length = k.size;

	  res = RE_SUCCESS;
	}
      else
	res = RE_PRIVATE_KEY;

      mpz_clear(s);
    }
  else
    res = RE_PRIVATE_KEY;
  
  mpz_clear(k.p);
  mpz_clear(k.q);
  mpz_clear(k.a);
  mpz_clear(k.b);
  mpz_clear(k.c);

  return res;
}

int
R_VerifyInit(R_SIGNATURE_CTX *ctx,
             int digestAlgorithm)
{
  return R_SignInit(ctx, digestAlgorithm);
}

int
R_VerifyUpdate(R_SIGNATURE_CTX *ctx,
               const uint8_t *data,
               /* Length is an unsigned char according to rsaref.txt,
                * but that must be a typo. */
               unsigned length)
{
  return R_SignUpdate(ctx, data, length);
}

int
R_VerifyFinal(R_SIGNATURE_CTX *ctx,
              uint8_t *signature,
              unsigned length,
              R_RSA_PUBLIC_KEY *key)
{
  struct rsa_public_key k;
  int res;

  nettle_mpz_init_set_str_256_u(k.n,
				MAX_RSA_MODULUS_LEN, key->modulus);
  nettle_mpz_init_set_str_256_u(k.e,
				MAX_RSA_MODULUS_LEN, key->exponent);
  
  if (rsa_public_key_prepare(&k) && (k.size == length))
    {
      mpz_t s;
  
      nettle_mpz_init_set_str_256_u(s,
				    k.size, signature);
      res = rsa_md5_verify(&k, &ctx->hash, s)
	? RE_SUCCESS : RE_SIGNATURE;

      mpz_clear(s);
    }
  else
    res = RE_PUBLIC_KEY;

  mpz_clear(k.n);
  mpz_clear(k.e);

  return res;
}
