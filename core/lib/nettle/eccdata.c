/* eccdata.c */

/* Generate compile time constant (but machine dependent) tables. */

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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mini-gmp.c"

/* Affine coordinates, for simplicity. Infinity point represented as x
   == y == 0. */
struct ecc_point
{
  mpz_t x;
  mpz_t y;
};

/* Represents an elliptic curve of the form

     y^2 = x^3 - 3x + b (mod p)
*/
struct ecc_curve
{
  unsigned bit_size;
  unsigned pippenger_k;
  unsigned pippenger_c;

  /* Prime */
  mpz_t p;
  mpz_t b;

  /* Curve order */
  mpz_t q;
  struct ecc_point g;

  /* Table for pippenger's algorithm.
     Element

       i 2^c + j_0 + j_1 2 + j_2 2^2 + ... + j_{c-1} 2^{c-1}

     holds

       2^{ikc} ( j_0 + j_1 2^k + j_2 2^{2k} + ... + j_{c-1} 2^{(c-1)k}) g
   */
  mp_size_t table_size;
  struct ecc_point *table;

  /* If non-NULL, holds 2g, 3g, 4g */
  struct ecc_point *ref;
};

static void
ecc_init (struct ecc_point *p)
{
  mpz_init (p->x);
  mpz_init (p->y);
}

static void
ecc_clear (struct ecc_point *p)
{
  mpz_clear (p->x);
  mpz_clear (p->y);
}

static int
ecc_zero_p (const struct ecc_point *p)
{
  return mpz_sgn (p->x) == 0 && mpz_sgn (p->y) == 0;
}

static int
ecc_equal_p (const struct ecc_point *p, const struct ecc_point *q)
{
  return mpz_cmp (p->x, q->x) == 0 && mpz_cmp (p->y, q->y) == 0;
}

static void
ecc_set_zero (struct ecc_point *r)
{
  mpz_set_ui (r->x, 0);
  mpz_set_ui (r->y, 0);
}

static void
ecc_set (struct ecc_point *r, const struct ecc_point *p)
{
  mpz_set (r->x, p->x);
  mpz_set (r->y, p->y);
}

static void
ecc_dup (const struct ecc_curve *ecc,
	 struct ecc_point *r, const struct ecc_point *p)
{
  if (ecc_zero_p (p))
    ecc_set_zero (r);

  else
    {
      mpz_t m, t, x, y;
  
      mpz_init (m);
      mpz_init (t);
      mpz_init (x);
      mpz_init (y);

      /* m = (2 y)^-1 */
      mpz_mul_ui (m, p->y, 2);
      mpz_invert (m, m, ecc->p);

      /* t = 3 (x^2 - 1) * m */
      mpz_mul (t, p->x, p->x);
      mpz_mod (t, t, ecc->p);
      mpz_sub_ui (t, t, 1);
      mpz_mul_ui (t, t, 3);
      mpz_mul (t, t, m);

      /* x' = t^2 - 2 x */
      mpz_mul (x, t, t);
      /* mpz_submul_ui (x, p->x, 2); not available in mini-gmp */
      mpz_mul_ui (m, p->x, 2);
      mpz_sub (x, x, m);
      mpz_mod (x, x, ecc->p);

      /* y' = (x - x') * t - y */
      mpz_sub (y, p->x, x);
      mpz_mul (y, y, t);
      mpz_sub (y, y, p->y);
      mpz_mod (y, y, ecc->p);

      mpz_swap (x, r->x);
      mpz_swap (y, r->y);
  
      mpz_clear (m);
      mpz_clear (t);
      mpz_clear (x);
      mpz_clear (y);
    }
}

static void
ecc_add (const struct ecc_curve *ecc,
	 struct ecc_point *r, const struct ecc_point *p, const struct ecc_point *q)
{
  if (ecc_zero_p (p))
    ecc_set (r, q);

  else if (ecc_zero_p (q))
    ecc_set (r, p);

  else if (mpz_cmp (p->x, q->x) == 0)
    {
      if (mpz_cmp (p->y, q->y) == 0)
	ecc_dup (ecc, r, p);
      else
	ecc_set_zero (r);
    }
  else
    {
      mpz_t s, t, x, y;
      mpz_init (s);
      mpz_init (t);
      mpz_init (x);
      mpz_init (y);

      /* t = (q_y - p_y) / (q_x - p_x) */
      mpz_sub (t, q->x, p->x);
      mpz_invert (t, t, ecc->p);
      mpz_sub (s, q->y, p->y);
      mpz_mul (t, t, s);
      mpz_mod (t, t, ecc->p);

      /* x' = t^2 - p_x - q_x */
      mpz_mul (x, t, t);
      mpz_sub (x, x, p->x);
      mpz_sub (x, x, q->x);
      mpz_mod (x, x, ecc->p);

      /* y' = (x - x') * t - y */
      mpz_sub (y, p->x, x);
      mpz_mul (y, y, t);
      mpz_sub (y, y, p->y);
      mpz_mod (y, y, ecc->p);

      mpz_swap (x, r->x);
      mpz_swap (y, r->y);

      mpz_clear (s);
      mpz_clear (t);
      mpz_clear (x);
      mpz_clear (y);
    }
}

static void 
ecc_mul_binary (const struct ecc_curve *ecc,
		struct ecc_point *r, const mpz_t n, const struct ecc_point *p)
{
  /* Avoid the mp_bitcnt_t type for compatibility with older GMP
     versions. */
  unsigned k;

  assert (r != p);
  assert (mpz_sgn (n) > 0);

  ecc_set (r, p);

  /* Index of highest one bit */
  for (k = mpz_sizeinbase (n, 2) - 1; k-- > 0; )
    {
      ecc_dup (ecc, r, r);
      if (mpz_tstbit (n, k))
	ecc_add (ecc, r, r, p);
    }  
}

static struct ecc_point *
ecc_alloc (size_t n)
{
  struct ecc_point *p = malloc (n * sizeof(*p));
  size_t i;

  if (!p)
    {
      fprintf (stderr, "Virtual memory exhausted.\n");
      exit (EXIT_FAILURE);
    }
  for (i = 0; i < n; i++)
    ecc_init (&p[i]);

  return p;
}

static void
ecc_set_str (struct ecc_point *p,
	     const char *x, const char *y)
{
  mpz_set_str (p->x, x, 16);
  mpz_set_str (p->y, y, 16);  
}

static void
ecc_curve_init_str (struct ecc_curve *ecc,
		    const char *p, const char *b, const char *q,
		    const char *gx, const char *gy)
{
  mpz_init_set_str (ecc->p, p, 16);
  mpz_init_set_str (ecc->b, b, 16);
  mpz_init_set_str (ecc->q, q, 16);
  ecc_init (&ecc->g);
  ecc_set_str (&ecc->g, gx, gy);

  ecc->pippenger_k = 0;
  ecc->pippenger_c = 0;
  ecc->table = NULL;

  ecc->ref = NULL;
}

static void
ecc_curve_init (struct ecc_curve *ecc, unsigned bit_size)
{
  switch (bit_size)
    {
    case 192:      
      ecc_curve_init_str (ecc,
			  /* p = 2^{192} - 2^{64} - 1 */
			  "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE"
			  "FFFFFFFFFFFFFFFF",

			  "64210519e59c80e70fa7e9ab72243049"
			  "feb8deecc146b9b1", 

			  "ffffffffffffffffffffffff99def836"
			  "146bc9b1b4d22831",

			  "188da80eb03090f67cbf20eb43a18800"
			  "f4ff0afd82ff1012",

			  "07192b95ffc8da78631011ed6b24cdd5"
			  "73f977a11e794811");
      ecc->ref = ecc_alloc (3);
      ecc_set_str (&ecc->ref[0], /* 2 g */
		   "dafebf5828783f2ad35534631588a3f629a70fb16982a888",
		   "dd6bda0d993da0fa46b27bbc141b868f59331afa5c7e93ab");
      
      ecc_set_str (&ecc->ref[1], /* 3 g */
		   "76e32a2557599e6edcd283201fb2b9aadfd0d359cbb263da",
		   "782c37e372ba4520aa62e0fed121d49ef3b543660cfd05fd");

      ecc_set_str (&ecc->ref[2], /* 4 g */
		   "35433907297cc378b0015703374729d7a4fe46647084e4ba",
		   "a2649984f2135c301ea3acb0776cd4f125389b311db3be32");

      break;
    case 224:
      ecc_curve_init_str (ecc,
			  /* p = 2^{224} - 2^{96} + 1 */
			  "ffffffffffffffffffffffffffffffff"
			  "000000000000000000000001",

			  "b4050a850c04b3abf54132565044b0b7"
			  "d7bfd8ba270b39432355ffb4",

			  "ffffffffffffffffffffffffffff16a2"
			  "e0b8f03e13dd29455c5c2a3d",

			  "b70e0cbd6bb4bf7f321390b94a03c1d3"
			  "56c21122343280d6115c1d21",

			  "bd376388b5f723fb4c22dfe6cd4375a0"
			  "5a07476444d5819985007e34");

      ecc->ref = ecc_alloc (3);
      ecc_set_str (&ecc->ref[0], /* 2 g */
		   "706a46dc76dcb76798e60e6d89474788d16dc18032d268fd1a704fa6",
		   "1c2b76a7bc25e7702a704fa986892849fca629487acf3709d2e4e8bb");
      
      ecc_set_str (&ecc->ref[1], /* 3 g */
		   "df1b1d66a551d0d31eff822558b9d2cc75c2180279fe0d08fd896d04",
		   "a3f7f03cadd0be444c0aa56830130ddf77d317344e1af3591981a925");

      ecc_set_str (&ecc->ref[2], /* 4 g */
		   "ae99feebb5d26945b54892092a8aee02912930fa41cd114e40447301",
		   "482580a0ec5bc47e88bc8c378632cd196cb3fa058a7114eb03054c9");

      break;
    case 256:
      ecc_curve_init_str (ecc,
			  /* p = 2^{256} - 2^{224} + 2^{192} + 2^{96} - 1 */
			  "FFFFFFFF000000010000000000000000"
			  "00000000FFFFFFFFFFFFFFFFFFFFFFFF",

			  "5AC635D8AA3A93E7B3EBBD55769886BC"
			  "651D06B0CC53B0F63BCE3C3E27D2604B",

			  "FFFFFFFF00000000FFFFFFFFFFFFFFFF"
			  "BCE6FAADA7179E84F3B9CAC2FC632551",

			  "6B17D1F2E12C4247F8BCE6E563A440F2"
			  "77037D812DEB33A0F4A13945D898C296",

			  "4FE342E2FE1A7F9B8EE7EB4A7C0F9E16"
			  "2BCE33576B315ECECBB6406837BF51F5");

      ecc->ref = ecc_alloc (3);
      ecc_set_str (&ecc->ref[0], /* 2 g */
		   "7cf27b188d034f7e8a52380304b51ac3c08969e277f21b35a60b48fc47669978",
		   "7775510db8ed040293d9ac69f7430dbba7dade63ce982299e04b79d227873d1");
      
      ecc_set_str (&ecc->ref[1], /* 3 g */
		   "5ecbe4d1a6330a44c8f7ef951d4bf165e6c6b721efada985fb41661bc6e7fd6c",
		   "8734640c4998ff7e374b06ce1a64a2ecd82ab036384fb83d9a79b127a27d5032");

      ecc_set_str (&ecc->ref[2], /* 4 g */
		   "e2534a3532d08fbba02dde659ee62bd0031fe2db785596ef509302446b030852",
		   "e0f1575a4c633cc719dfee5fda862d764efc96c3f30ee0055c42c23f184ed8c6");

      break;
    case 384:
      ecc_curve_init_str (ecc,
			  /* p = 2^{384} - 2^{128} - 2^{96} + 2^{32} - 1 */
			  "ffffffffffffffffffffffffffffffff"
			  "fffffffffffffffffffffffffffffffe"
			  "ffffffff0000000000000000ffffffff",
			  
			  "b3312fa7e23ee7e4988e056be3f82d19"
			  "181d9c6efe8141120314088f5013875a"
			  "c656398d8a2ed19d2a85c8edd3ec2aef",
			  
			  "ffffffffffffffffffffffffffffffff"
			  "ffffffffffffffffc7634d81f4372ddf"
			  "581a0db248b0a77aecec196accc52973",
			  
			  "aa87ca22be8b05378eb1c71ef320ad74"
			  "6e1d3b628ba79b9859f741e082542a38"
			  "5502f25dbf55296c3a545e3872760ab7",
			  
			  "3617de4a96262c6f5d9e98bf9292dc29"
			  "f8f41dbd289a147ce9da3113b5f0b8c0"
			  "0a60b1ce1d7e819d7a431d7c90ea0e5f");

      ecc->ref = ecc_alloc (3);
      ecc_set_str (&ecc->ref[0], /* 2 g */
		   "8d999057ba3d2d969260045c55b97f089025959a6f434d651d207d19fb96e9e4fe0e86ebe0e64f85b96a9c75295df61",
		   "8e80f1fa5b1b3cedb7bfe8dffd6dba74b275d875bc6cc43e904e505f256ab4255ffd43e94d39e22d61501e700a940e80");

      ecc_set_str (&ecc->ref[1], /* 3 g */
		   "77a41d4606ffa1464793c7e5fdc7d98cb9d3910202dcd06bea4f240d3566da6b408bbae5026580d02d7e5c70500c831",
		   "c995f7ca0b0c42837d0bbe9602a9fc998520b41c85115aa5f7684c0edc111eacc24abd6be4b5d298b65f28600a2f1df1");

      ecc_set_str (&ecc->ref[2], /* 4 g */
		   "138251cd52ac9298c1c8aad977321deb97e709bd0b4ca0aca55dc8ad51dcfc9d1589a1597e3a5120e1efd631c63e1835",
		   "cacae29869a62e1631e8a28181ab56616dc45d918abc09f3ab0e63cf792aa4dced7387be37bba569549f1c02b270ed67");

      break;
    case 521:
      ecc_curve_init_str (ecc,			  
			  "1ff" /* p = 2^{521} - 1 */
			  "ffffffffffffffffffffffffffffffff"
			  "ffffffffffffffffffffffffffffffff"
			  "ffffffffffffffffffffffffffffffff"
			  "ffffffffffffffffffffffffffffffff",

			  "051"
			  "953eb9618e1c9a1f929a21a0b68540ee"
			  "a2da725b99b315f3b8b489918ef109e1"
			  "56193951ec7e937b1652c0bd3bb1bf07"
			  "3573df883d2c34f1ef451fd46b503f00",

			  "1ff"
			  "ffffffffffffffffffffffffffffffff"
			  "fffffffffffffffffffffffffffffffa"
			  "51868783bf2f966b7fcc0148f709a5d0"
			  "3bb5c9b8899c47aebb6fb71e91386409",

			  "c6"
			  "858e06b70404e9cd9e3ecb662395b442"
			  "9c648139053fb521f828af606b4d3dba"
			  "a14b5e77efe75928fe1dc127a2ffa8de"
			  "3348b3c1856a429bf97e7e31c2e5bd66",

			  "118"
			  "39296a789a3bc0045c8a5fb42c7d1bd9"
			  "98f54449579b446817afbd17273e662c"
			  "97ee72995ef42640c550b9013fad0761"
			  "353c7086a272c24088be94769fd16650");

      ecc->ref = ecc_alloc (3);
      ecc_set_str (&ecc->ref[0], /* 2 g */
		   "433c219024277e7e682fcb288148c282747403279b1ccc06352c6e5505d769be97b3b204da6ef55507aa104a3a35c5af41cf2fa364d60fd967f43e3933ba6d783d",
		   "f4bb8cc7f86db26700a7f3eceeeed3f0b5c6b5107c4da97740ab21a29906c42dbbb3e377de9f251f6b93937fa99a3248f4eafcbe95edc0f4f71be356d661f41b02");
      
      ecc_set_str (&ecc->ref[1], /* 3 g */
		   "1a73d352443de29195dd91d6a64b5959479b52a6e5b123d9ab9e5ad7a112d7a8dd1ad3f164a3a4832051da6bd16b59fe21baeb490862c32ea05a5919d2ede37ad7d",
		   "13e9b03b97dfa62ddd9979f86c6cab814f2f1557fa82a9d0317d2f8ab1fa355ceec2e2dd4cf8dc575b02d5aced1dec3c70cf105c9bc93a590425f588ca1ee86c0e5");

      ecc_set_str (&ecc->ref[2], /* 4 g */
		   "35b5df64ae2ac204c354b483487c9070cdc61c891c5ff39afc06c5d55541d3ceac8659e24afe3d0750e8b88e9f078af066a1d5025b08e5a5e2fbc87412871902f3",
		   "82096f84261279d2b673e0178eb0b4abb65521aef6e6e32e1b5ae63fe2f19907f279f283e54ba385405224f750a95b85eebb7faef04699d1d9e21f47fc346e4d0d");

      break;
    default:
      fprintf (stderr, "No known curve for size %d\n", bit_size);
      exit(EXIT_FAILURE);     
    }
  ecc->bit_size = bit_size;
}

static void
ecc_pippenger_precompute (struct ecc_curve *ecc, unsigned k, unsigned c)
{
  unsigned p = (ecc->bit_size + k-1) / k;
  unsigned M = (p + c-1)/c;
  unsigned i, j;

  ecc->pippenger_k = k;
  ecc->pippenger_c = c;
  ecc->table_size = M << c;
  ecc->table = ecc_alloc (ecc->table_size);
  
  /* Compute the first 2^c entries */
  ecc_set_zero (&ecc->table[0]);
  ecc_set (&ecc->table[1], &ecc->g);

  for (j = 2; j < (1U<<c); j <<= 1)
    {
      /* T[j] = 2^k T[j/2] */
      ecc_dup (ecc, &ecc->table[j], &ecc->table[j/2]);
      for (i = 1; i < k; i++)
	ecc_dup (ecc, &ecc->table[j], &ecc->table[j]);

      for (i = 1; i < j; i++)
	ecc_add (ecc, &ecc->table[j + i], &ecc->table[j], &ecc->table[i]);
    }
  for (j = 1<<c; j < ecc->table_size; j++)
    {
      /* T[j] = 2^{kc} T[j-2^c] */
      ecc_dup (ecc, &ecc->table[j], &ecc->table[j - (1<<c)]);
      for (i = 1; i < k*c; i++)
	ecc_dup (ecc, &ecc->table[j], &ecc->table[j]);
    }
}

static void
ecc_mul_pippenger (const struct ecc_curve *ecc,
		   struct ecc_point *r, const mpz_t n_input)
{
  mpz_t n;
  unsigned k, c;
  unsigned i, j;
  unsigned bit_rows;

  mpz_init (n);
  
  mpz_mod (n, n_input, ecc->q);
  ecc_set_zero (r);

  k = ecc->pippenger_k;
  c = ecc->pippenger_c;

  bit_rows = (ecc->bit_size + k - 1) / k;

  for (i = k; i-- > 0; )
    {
      ecc_dup (ecc, r, r);
      for (j = 0; j * c < bit_rows; j++)
	{
	  unsigned bits;
	  mp_size_t bit_index;
	  
	  /* Extract c bits of the exponent, stride k, starting at i + kcj, ending at
	    i + k (cj + c - 1)*/
	  for (bits = 0, bit_index = i + k*(c*j+c); bit_index > i + k*c*j; )
	    {
	      bit_index -= k;
	      bits = (bits << 1) | mpz_tstbit (n, bit_index);
	    }

	  ecc_add (ecc, r, r, &ecc->table[(j << c) | bits]);
	}
    }
  mpz_clear (n);
}

#define ASSERT_EQUAL(p, q) do {						\
    if (!ecc_equal_p (p, q))						\
      {									\
	fprintf (stderr, "%s:%d: ASSERT_EQUAL (%s, %s) failed.\n",	\
		 __FILE__, __LINE__, #p, #q);				\
	fprintf (stderr, "p = (");					\
	mpz_out_str (stderr, 16, (p)->x);				\
	fprintf (stderr, ",\n     ");					\
	mpz_out_str (stderr, 16, (p)->y);				\
	fprintf (stderr, ")\nq = (");					\
	mpz_out_str (stderr, 16, (q)->x);				\
	fprintf (stderr, ",\n     ");					\
	mpz_out_str (stderr, 16, (q)->y);				\
	fprintf (stderr, ")\n");					\
	abort();							\
      }									\
  } while (0)

#define ASSERT_ZERO(p) do {						\
    if (!ecc_zero_p (p))						\
      {									\
	fprintf (stderr, "%s:%d: ASSERT_ZERO (%s) failed.\n",		\
		 __FILE__, __LINE__, #p);				\
	fprintf (stderr, "p = (");					\
	mpz_out_str (stderr, 16, (p)->x);				\
	fprintf (stderr, ",\n     ");					\
	mpz_out_str (stderr, 16, (p)->y);				\
	fprintf (stderr, ")\n");					\
	abort();							\
      }									\
  } while (0)

static void
ecc_curve_check (const struct ecc_curve *ecc)
{
  struct ecc_point p, q;
  mpz_t n;

  ecc_init (&p);
  ecc_init (&q);
  mpz_init (n);

  ecc_dup (ecc, &p, &ecc->g);
  if (ecc->ref)
    ASSERT_EQUAL (&p, &ecc->ref[0]);
  else
    {
      fprintf (stderr, "g2 = ");
      mpz_out_str (stderr, 16, p.x);
      fprintf (stderr, "\n     ");
      mpz_out_str (stderr, 16, p.y);
      fprintf (stderr, "\n");
    }
  ecc_add (ecc, &q, &p, &ecc->g);
  if (ecc->ref)
    ASSERT_EQUAL (&q, &ecc->ref[1]);
  else
    {
      fprintf (stderr, "g3 = ");
      mpz_out_str (stderr, 16, q.x);
      fprintf (stderr, "\n     ");
      mpz_out_str (stderr, 16, q.y);
      fprintf (stderr, "\n");
    }

  ecc_add (ecc, &q, &q, &ecc->g);
  if (ecc->ref)
    ASSERT_EQUAL (&q, &ecc->ref[2]);
  else
    {
      fprintf (stderr, "g4 = ");
      mpz_out_str (stderr, 16, q.x);
      fprintf (stderr, "\n     ");
      mpz_out_str (stderr, 16, q.y);
      fprintf (stderr, "\n");
    }

  ecc_dup (ecc, &q, &p);
  if (ecc->ref)
    ASSERT_EQUAL (&q, &ecc->ref[2]);
  else
    {
      fprintf (stderr, "g4 = ");
      mpz_out_str (stderr, 16, q.x);
      fprintf (stderr, "\n     ");
      mpz_out_str (stderr, 16, q.y);
      fprintf (stderr, "\n");
    }

  ecc_mul_binary (ecc, &p, ecc->q, &ecc->g);
  ASSERT_ZERO (&p);

  ecc_mul_pippenger (ecc, &q, ecc->q);
  ASSERT_ZERO (&q);

  ecc_clear (&p);
  ecc_clear (&q);
  mpz_clear (n);
}

static void
output_digits (const mpz_t x,
	       unsigned size, unsigned bits_per_limb)
{  
  mpz_t t;
  mpz_t mask;
  mpz_t limb;
  unsigned i;
  const char *suffix;

  mpz_init (t);
  mpz_init (mask);
  mpz_init (limb);

  mpz_setbit (mask, bits_per_limb);
  mpz_sub_ui (mask, mask, 1);

  suffix = bits_per_limb > 32 ? "ULL" : "UL";

  mpz_init_set (t, x);

  for (i = 0; i < size; i++)
    {
      if ( (i % 8) == 0)
	printf("\n ");
      
      mpz_and (limb, mask, t);
      printf (" 0x");
      mpz_out_str (stdout, 16, limb);
      printf ("%s,", suffix);
      mpz_tdiv_q_2exp (t, t, bits_per_limb);
    }

  mpz_clear (t);
  mpz_clear (mask);
  mpz_clear (limb);
}

static void
output_bignum (const char *name, const mpz_t x,
	       unsigned size, unsigned bits_per_limb)
{  
  printf ("static const mp_limb_t %s[%d] = {", name, size);
  output_digits (x, size, bits_per_limb);
  printf("\n};\n");
}

static void
output_point (const char *name, const struct ecc_point *p,
	      unsigned size, unsigned bits_per_limb)
{
  if (name)
    printf("static const mp_limb_t %s[%u] = {", name, 2*size);

  output_digits (p->x, size, bits_per_limb);
  output_digits (p->y, size, bits_per_limb);

  if (name)
    printf("\n};\n");
}

static void
output_point_redc (const char *name, const struct ecc_curve *ecc,
		   const struct ecc_point *p,
		   unsigned size, unsigned bits_per_limb)
{
  mpz_t t;
  mpz_init (t);

  if (name)
    printf("static const mp_limb_t %s[%u] = {", name, 2*size);
    
  mpz_mul_2exp (t, p->x, size * bits_per_limb);
  mpz_mod (t, t, ecc->p);
      
  output_digits (t, size, bits_per_limb);

  mpz_mul_2exp (t, p->y, size * bits_per_limb);
  mpz_mod (t, t, ecc->p);
      
  output_digits (t, size, bits_per_limb);

  if (name)
    printf("\n};\n");

  mpz_clear (t);
}

static unsigned
output_modulo (const char *name, const mpz_t x,
	       unsigned size, unsigned bits_per_limb)
{
  mpz_t mod;
  unsigned bits;

  mpz_init (mod);

  mpz_setbit (mod, bits_per_limb * size);
  mpz_mod (mod, mod, x);

  bits = mpz_sizeinbase (mod, 2);
  assert (bits <= size * bits_per_limb - 32);

  output_bignum (name, mod, size, bits_per_limb);
  
  mpz_clear (mod);
  return bits;
}

static void
output_curve (const struct ecc_curve *ecc, unsigned bits_per_limb)
{
  unsigned limb_size = (ecc->bit_size + bits_per_limb - 1)/bits_per_limb;
  unsigned i;
  unsigned bits;
  int redc_limbs;
  mpz_t t;

  mpz_init (t);

  printf ("/* For NULL. */\n#include <stddef.h>\n");

  printf ("#define ECC_LIMB_SIZE %u\n", limb_size);
  printf ("#define ECC_PIPPENGER_K %u\n", ecc->pippenger_k);
  printf ("#define ECC_PIPPENGER_C %u\n", ecc->pippenger_c);

  output_bignum ("ecc_p", ecc->p, limb_size, bits_per_limb);
  output_bignum ("ecc_b", ecc->b, limb_size, bits_per_limb);
  output_bignum ("ecc_q", ecc->q, limb_size, bits_per_limb);
  output_point ("ecc_g", &ecc->g, limb_size, bits_per_limb);
  output_point_redc ("ecc_redc_g", ecc, &ecc->g, limb_size, bits_per_limb);
  
  bits = output_modulo ("ecc_Bmodp", ecc->p, limb_size, bits_per_limb);
  printf ("#define ECC_BMODP_SIZE %u\n",
	  (bits + bits_per_limb - 1) / bits_per_limb);
  bits = output_modulo ("ecc_Bmodq", ecc->q, limb_size, bits_per_limb);
  printf ("#define ECC_BMODQ_SIZE %u\n",
	  (bits + bits_per_limb - 1) / bits_per_limb);

  if (ecc->bit_size < limb_size * bits_per_limb)
    {
      int shift;

      mpz_set_ui (t, 0);
      mpz_setbit (t, ecc->bit_size);
      mpz_sub (t, t, ecc->p);      
      output_bignum ("ecc_Bmodp_shifted", t, limb_size, bits_per_limb);

      shift = limb_size * bits_per_limb - ecc->bit_size;
      if (shift > 0)
	{
	  /* Check condition for reducing hi limbs. If s is the
	     normalization shift and n is the bit size (so that s + n
	     = limb_size * bite_per_limb), then we need

	       (2^n - 1) + (2^s - 1) (2^n - p) < 2p

	     or equivalently,

	       2^s (2^n - p) <= p

	     To a allow a carry limb to be added in at the same time,
	     substitute s+1 for s.
	  */
	  /* FIXME: For ecdsa verify, we actually need the stricter
	     inequality < 2 q. */
	  mpz_mul_2exp (t, t, shift + 1);
	  if (mpz_cmp (t, ecc->p) > 0)
	    {
	      fprintf (stderr, "Reduction condition failed for %u-bit curve.\n",
		       ecc->bit_size);
	      exit (EXIT_FAILURE);
	    }
	}
      mpz_set_ui (t, 0);
      mpz_setbit (t, ecc->bit_size);
      mpz_sub (t, t, ecc->q);      
      output_bignum ("ecc_Bmodq_shifted", t, limb_size, bits_per_limb);      
    }
  else
    {
      printf ("#define ecc_Bmodp_shifted ecc_Bmodp\n");
      printf ("#define ecc_Bmodq_shifted ecc_Bmodq\n");
    }

  mpz_add_ui (t, ecc->p, 1);
  mpz_fdiv_q_2exp (t, t, 1);
  output_bignum ("ecc_pp1h", t, limb_size, bits_per_limb);      

  mpz_add_ui (t, ecc->q, 1);
  mpz_fdiv_q_2exp (t, t, 1);
  output_bignum ("ecc_qp1h", t, limb_size, bits_per_limb);  
  
  /* Trailing zeros in p+1 correspond to trailing ones in p. */
  redc_limbs = mpz_scan0 (ecc->p, 0) / bits_per_limb;
  if (redc_limbs > 0)
    {
      mpz_add_ui (t, ecc->p, 1);
      mpz_fdiv_q_2exp (t, t, redc_limbs * bits_per_limb);
      output_bignum ("ecc_redc_ppm1", t, limb_size - redc_limbs, bits_per_limb);
    }
  else
    {    
      /* Trailing zeros in p-1 correspond to zeros just above the low
	 bit of p */
      redc_limbs = mpz_scan1 (ecc->p, 1) / bits_per_limb;
      if (redc_limbs > 0)
	{
	  printf ("#define ecc_redc_ppm1 (ecc_p + %d)\n",
		  redc_limbs);
	  redc_limbs = -redc_limbs;
	}
      else
	printf ("#define ecc_redc_ppm1 NULL\n");
    }
  printf ("#define ECC_REDC_SIZE %d\n", redc_limbs);

  printf ("#if USE_REDC\n");
  printf ("#define ecc_unit ecc_Bmodp\n");

  printf ("static const mp_limb_t ecc_table[%lu] = {",
	 (unsigned long) (2*ecc->table_size * limb_size));
  for (i = 0; i < ecc->table_size; i++)
    output_point_redc (NULL, ecc, &ecc->table[i], limb_size, bits_per_limb);

  printf("\n};\n");

  printf ("#else\n");

  mpz_init_set_ui (t, 1);
  output_bignum ("ecc_unit", t, limb_size, bits_per_limb);
  
  printf ("static const mp_limb_t ecc_table[%lu] = {",
	 (unsigned long) (2*ecc->table_size * limb_size));
  for (i = 0; i < ecc->table_size; i++)
    output_point (NULL, &ecc->table[i], limb_size, bits_per_limb);

  printf("\n};\n");
  printf ("#endif\n");
  
  mpz_clear (t);
}

int
main (int argc, char **argv)
{
  struct ecc_curve ecc;

  if (argc < 4)
    {
      fprintf (stderr, "Usage: %s CURVE-BITS K C [BITS-PER-LIMB]\n", argv[0]);
      return EXIT_FAILURE;
    }

  ecc_curve_init (&ecc, atoi(argv[1]));

  ecc_pippenger_precompute (&ecc, atoi(argv[2]), atoi(argv[3]));

  fprintf (stderr, "Table size: %lu entries\n",
	   (unsigned long) ecc.table_size);

  ecc_curve_check (&ecc);

  if (argc > 4)
    output_curve (&ecc, atoi(argv[4]));

  return EXIT_SUCCESS;
}
