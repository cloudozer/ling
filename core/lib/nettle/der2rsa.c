/* der2rsa.c
 *
 * Decoding of keys in PKCS#1 format.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2005 Niels Möller
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

#include "rsa.h"

#include "bignum.h"
#include "asn1.h"

#define GET(i, x, l)					\
(asn1_der_iterator_next((i)) == ASN1_ITERATOR_PRIMITIVE	\
 && (i)->type == ASN1_INTEGER				\
 && asn1_der_get_bignum((i), (x), (l))			\
 && mpz_sgn((x)) > 0)

int
rsa_public_key_from_der_iterator(struct rsa_public_key *pub,
				 unsigned limit,
				 struct asn1_der_iterator *i)
{
  /* RSAPublicKey ::= SEQUENCE {
         modulus           INTEGER,  -- n
	 publicExponent    INTEGER   -- e
      }
  */

  return (i->type == ASN1_SEQUENCE
	  && asn1_der_decode_constructed_last(i) == ASN1_ITERATOR_PRIMITIVE
	  && asn1_der_get_bignum(i, pub->n, limit) 
	  && mpz_sgn(pub->n) > 0
	  && GET(i, pub->e, limit)
	  && asn1_der_iterator_next(i) == ASN1_ITERATOR_END
	  && rsa_public_key_prepare(pub));
}

int
rsa_private_key_from_der_iterator(struct rsa_public_key *pub,
				  struct rsa_private_key *priv,
				  unsigned limit,
				  struct asn1_der_iterator *i)
{
  /* RSAPrivateKey ::= SEQUENCE {
         version           Version,
	 modulus           INTEGER,  -- n
	 publicExponent    INTEGER,  -- e
	 privateExponent   INTEGER,  -- d
	 prime1            INTEGER,  -- p
	 prime2            INTEGER,  -- q
	 exponent1         INTEGER,  -- d mod (p-1)
	 exponent2         INTEGER,  -- d mod (q-1)
	 coefficient       INTEGER,  -- (inverse of q) mod p
	 otherPrimeInfos   OtherPrimeInfos OPTIONAL
    }
  */

  uint32_t version;
  
  if (i->type != ASN1_SEQUENCE)
    return 0;

  if (asn1_der_decode_constructed_last(i) == ASN1_ITERATOR_PRIMITIVE
      && i->type == ASN1_INTEGER
      && asn1_der_get_uint32(i, &version)
      && version <= 1
      && GET(i, pub->n, limit)
      && GET(i, pub->e, limit)
      && rsa_public_key_prepare(pub)
      && GET(i, priv->d, limit)
      && GET(i, priv->p, limit)
      && GET(i, priv->q, limit)
      && GET(i, priv->a, limit)
      && GET(i, priv->b, limit)
      && GET(i, priv->c, limit)
      && rsa_private_key_prepare(priv))
    {
      if (version == 1)
	{
	  /* otherPrimeInfos must be present. We ignore the contents */
	  if (!(asn1_der_iterator_next(i) == ASN1_ITERATOR_CONSTRUCTED
		&& i->type == ASN1_SEQUENCE))
	    return 0;
	}

      return (asn1_der_iterator_next(i) == ASN1_ITERATOR_END);
    }
  
  return 0;
}

int
rsa_keypair_from_der(struct rsa_public_key *pub,
		     struct rsa_private_key *priv,
		     unsigned limit, 
		     unsigned length, const uint8_t *data)
{
  struct asn1_der_iterator i;
  enum asn1_iterator_result res;

  res = asn1_der_iterator_first(&i, length, data);

  if (res != ASN1_ITERATOR_CONSTRUCTED)
    return 0;

  if (priv)
    return rsa_private_key_from_der_iterator(pub, priv, limit, &i);
  else
    return rsa_public_key_from_der_iterator(pub, limit, &i);    
}
