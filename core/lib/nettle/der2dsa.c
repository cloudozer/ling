/* der2dsa.c
 *
 * Decoding of DSA keys in OpenSSL and X509.1 format.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2005, 2009 Niels Möller, Magnus Holmgren
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

#include "dsa.h"

#include "bignum.h"
#include "asn1.h"

#define GET(i, x, l)					\
(asn1_der_iterator_next((i)) == ASN1_ITERATOR_PRIMITIVE	\
 && (i)->type == ASN1_INTEGER				\
 && asn1_der_get_bignum((i), (x), (l))			\
 && mpz_sgn((x)) > 0)

int
dsa_params_from_der_iterator(struct dsa_public_key *pub,
			     unsigned p_max_bits,
			     struct asn1_der_iterator *i)
{
  /* Dss-Parms ::= SEQUENCE {
	 p  INTEGER,
	 q  INTEGER,
	 g  INTEGER
     }
  */
  return (i->type == ASN1_INTEGER
	  && asn1_der_get_bignum(i, pub->p, p_max_bits)
	  && mpz_sgn(pub->p) > 0
	  && GET(i, pub->q, DSA_SHA1_Q_BITS)
	  && GET(i, pub->g, p_max_bits)
	  && asn1_der_iterator_next(i) == ASN1_ITERATOR_END);
}

int
dsa_public_key_from_der_iterator(struct dsa_public_key *pub,
				 unsigned p_max_bits,
				 struct asn1_der_iterator *i)
{
  /* DSAPublicKey ::= INTEGER
  */

  return (i->type == ASN1_INTEGER
	  && asn1_der_get_bignum(i, pub->y, p_max_bits)
	  && mpz_sgn(pub->y) > 0);
}

int
dsa_openssl_private_key_from_der_iterator(struct dsa_public_key *pub,
					  struct dsa_private_key *priv,
					  unsigned p_max_bits,
					  struct asn1_der_iterator *i)
{
  /* DSAPrivateKey ::= SEQUENCE {
         version           Version,
	 p                 INTEGER,
	 q                 INTEGER,
	 g                 INTEGER,
	 pub_key           INTEGER,  -- y
	 priv_key          INTEGER,  -- x
    }
  */

  uint32_t version;
  
  return (i->type == ASN1_SEQUENCE
	  && asn1_der_decode_constructed_last(i) == ASN1_ITERATOR_PRIMITIVE
	  && i->type == ASN1_INTEGER
	  && asn1_der_get_uint32(i, &version)
	  && version == 0
	  && GET(i, pub->p, p_max_bits)
	  && GET(i, pub->q, DSA_SHA1_Q_BITS)
	  && GET(i, pub->g, p_max_bits)
	  && GET(i, pub->y, p_max_bits)
	  && GET(i, priv->x, DSA_SHA1_Q_BITS)
	  && asn1_der_iterator_next(i) == ASN1_ITERATOR_END);
}

int
dsa_openssl_private_key_from_der(struct dsa_public_key *pub,
		     struct dsa_private_key *priv,
		     unsigned p_max_bits,
		     unsigned length, const uint8_t *data)
{
  struct asn1_der_iterator i;
  enum asn1_iterator_result res;

  res = asn1_der_iterator_first(&i, length, data);

  return (res == ASN1_ITERATOR_CONSTRUCTED
	  && dsa_openssl_private_key_from_der_iterator(pub, priv, p_max_bits, &i));
}
