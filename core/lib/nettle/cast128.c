/* cast128.c
 *
 * The CAST-128 block cipher, described in RFC 2144.
 */

/*	CAST-128 in C
 *	Written by Steve Reid <sreid@sea-to-sky.net>
 *	100% Public Domain - no warranty
 *	Released 1997.10.11
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

#include <assert.h>

#include "cast128.h"
#include "cast128_sboxes.h"

#include "macros.h"

#define CAST_SMALL_KEY 10
#define CAST_SMALL_ROUNDS 12
#define CAST_FULL_ROUNDS 16

/* Macros to access 8-bit bytes out of a 32-bit word */
#define U8a(x) ( (uint8_t) (x>>24) )
#define U8b(x) ( (uint8_t) ((x>>16)&0xff) )
#define U8c(x) ( (uint8_t) ((x>>8)&0xff) )
#define U8d(x) ( (uint8_t) ((x)&0xff) )

/* CAST-128 uses three different round functions */
#define F1(l, r, i) do {				\
    t = ROTL32(ctx->keys[i+16], ctx->keys[i] + r);	\
    l ^= ((cast_sbox1[U8a(t)] ^ cast_sbox2[U8b(t)])	\
	  - cast_sbox3[U8c(t)]) + cast_sbox4[U8d(t)];	\
  } while (0)
#define F2(l, r, i) do {				\
    t = ROTL32( ctx->keys[i+16], ctx->keys[i] ^ r);	\
    l ^= ((cast_sbox1[U8a(t)] - cast_sbox2[U8b(t)])	\
	  + cast_sbox3[U8c(t)]) ^ cast_sbox4[U8d(t)];	\
  } while (0)
#define F3(l, r, i) do { \
    t = ROTL32(ctx->keys[i+16], ctx->keys[i] - r);	\
    l ^= ((cast_sbox1[U8a(t)] + cast_sbox2[U8b(t)])	\
	  ^ cast_sbox3[U8c(t)]) - cast_sbox4[U8d(t)];	\
  } while (0)


/***** Encryption Function *****/

void
cast128_encrypt(const struct cast128_ctx *ctx,
		unsigned length, uint8_t *dst,
		const uint8_t *src)
{
  FOR_BLOCKS(length, dst, src, CAST128_BLOCK_SIZE)
    {
      uint32_t t, l, r;

      /* Get inblock into l,r */
      l = READ_UINT32(src);
      r = READ_UINT32(src+4);

      /* Do the work */
      F1(l, r,  0);
      F2(r, l,  1);
      F3(l, r,  2);
      F1(r, l,  3);
      F2(l, r,  4);
      F3(r, l,  5);
      F1(l, r,  6);
      F2(r, l,  7);
      F3(l, r,  8);
      F1(r, l,  9);
      F2(l, r, 10);
      F3(r, l, 11);
      /* Only do full 16 rounds if key length > 80 bits */
      if (ctx->rounds > 12) {
	F1(l, r, 12);
	F2(r, l, 13);
	F3(l, r, 14);
	F1(r, l, 15);
      }
      /* Put l,r into outblock */
      WRITE_UINT32(dst, r);
      WRITE_UINT32(dst + 4, l);
      /* Wipe clean */
      t = l = r = 0;
    }
}


/***** Decryption Function *****/

void
cast128_decrypt(const struct cast128_ctx *ctx,
		unsigned length, uint8_t *dst,
		const uint8_t *src)
{
  FOR_BLOCKS(length, dst, src, CAST128_BLOCK_SIZE)
    {
      uint32_t t, l, r;

      /* Get inblock into l,r */
      r = READ_UINT32(src);
      l = READ_UINT32(src+4);

      /* Do the work */
      /* Only do full 16 rounds if key length > 80 bits */
      if (ctx->rounds > 12) {
	F1(r, l, 15);
	F3(l, r, 14);
	F2(r, l, 13);
	F1(l, r, 12);
      }
      F3(r, l, 11);
      F2(l, r, 10);
      F1(r, l,  9);
      F3(l, r,  8);
      F2(r, l,  7);
      F1(l, r,  6);
      F3(r, l,  5);
      F2(l, r,  4);
      F1(r, l,  3);
      F3(l, r,  2);
      F2(r, l,  1);
      F1(l, r,  0);

      /* Put l,r into outblock */
      WRITE_UINT32(dst, l);
      WRITE_UINT32(dst + 4, r);

      /* Wipe clean */
      t = l = r = 0;
    }
}

/***** Key Schedule *****/

void
cast128_set_key(struct cast128_ctx *ctx,
		unsigned keybytes, const uint8_t *rawkey)
{
  uint32_t t[4], z[4], x[4];
  unsigned i;

  /* Set number of rounds to 12 or 16, depending on key length */
  ctx->rounds = (keybytes <= CAST_SMALL_KEY)
    ? CAST_SMALL_ROUNDS : CAST_FULL_ROUNDS;

  /* Copy key to workspace x */
  for (i = 0; i < 4; i++) {
    x[i] = 0;
    if ((i*4+0) < keybytes) x[i] = (uint32_t)rawkey[i*4+0] << 24;
    if ((i*4+1) < keybytes) x[i] |= (uint32_t)rawkey[i*4+1] << 16;
    if ((i*4+2) < keybytes) x[i] |= (uint32_t)rawkey[i*4+2] << 8;
    if ((i*4+3) < keybytes) x[i] |= (uint32_t)rawkey[i*4+3];
  }
  /* FIXME: For the shorter key sizes, the last 4 subkeys are not
     used, and need not be generated, nor stored. */
  /* Generate 32 subkeys, four at a time */
  for (i = 0; i < 32; i+=4) {
    switch (i & 4) {
    case 0:
      t[0] = z[0] = x[0] ^ cast_sbox5[U8b(x[3])]
	^ cast_sbox6[U8d(x[3])] ^ cast_sbox7[U8a(x[3])]
	^ cast_sbox8[U8c(x[3])] ^ cast_sbox7[U8a(x[2])];
      t[1] = z[1] = x[2] ^ cast_sbox5[U8a(z[0])]
	^ cast_sbox6[U8c(z[0])] ^ cast_sbox7[U8b(z[0])]
	^ cast_sbox8[U8d(z[0])] ^ cast_sbox8[U8c(x[2])];
      t[2] = z[2] = x[3] ^ cast_sbox5[U8d(z[1])]
	^ cast_sbox6[U8c(z[1])] ^ cast_sbox7[U8b(z[1])]
	^ cast_sbox8[U8a(z[1])] ^ cast_sbox5[U8b(x[2])];
      t[3] = z[3] = x[1] ^ cast_sbox5[U8c(z[2])] ^
	cast_sbox6[U8b(z[2])] ^ cast_sbox7[U8d(z[2])]
	^ cast_sbox8[U8a(z[2])] ^ cast_sbox6[U8d(x[2])];
      break;
    case 4:
      t[0] = x[0] = z[2] ^ cast_sbox5[U8b(z[1])]
	^ cast_sbox6[U8d(z[1])] ^ cast_sbox7[U8a(z[1])]
	^ cast_sbox8[U8c(z[1])] ^ cast_sbox7[U8a(z[0])];
      t[1] = x[1] = z[0] ^ cast_sbox5[U8a(x[0])]
	^ cast_sbox6[U8c(x[0])] ^ cast_sbox7[U8b(x[0])]
	^ cast_sbox8[U8d(x[0])] ^ cast_sbox8[U8c(z[0])];
      t[2] = x[2] = z[1] ^ cast_sbox5[U8d(x[1])]
	^ cast_sbox6[U8c(x[1])] ^ cast_sbox7[U8b(x[1])]
	^ cast_sbox8[U8a(x[1])] ^ cast_sbox5[U8b(z[0])];
      t[3] = x[3] = z[3] ^ cast_sbox5[U8c(x[2])]
	^ cast_sbox6[U8b(x[2])] ^ cast_sbox7[U8d(x[2])]
	^ cast_sbox8[U8a(x[2])] ^ cast_sbox6[U8d(z[0])];
      break;
    }
    switch (i & 12) {
    case 0:
    case 12:
      ctx->keys[i+0] = cast_sbox5[U8a(t[2])] ^ cast_sbox6[U8b(t[2])]
	^ cast_sbox7[U8d(t[1])] ^ cast_sbox8[U8c(t[1])];
      ctx->keys[i+1] = cast_sbox5[U8c(t[2])] ^ cast_sbox6[U8d(t[2])]
	^ cast_sbox7[U8b(t[1])] ^ cast_sbox8[U8a(t[1])];
      ctx->keys[i+2] = cast_sbox5[U8a(t[3])] ^ cast_sbox6[U8b(t[3])]
	^ cast_sbox7[U8d(t[0])] ^ cast_sbox8[U8c(t[0])];
      ctx->keys[i+3] = cast_sbox5[U8c(t[3])] ^ cast_sbox6[U8d(t[3])]
	^ cast_sbox7[U8b(t[0])] ^ cast_sbox8[U8a(t[0])];
      break;
    case 4:
    case 8:
      ctx->keys[i+0] = cast_sbox5[U8d(t[0])] ^ cast_sbox6[U8c(t[0])]
	^ cast_sbox7[U8a(t[3])] ^ cast_sbox8[U8b(t[3])];
      ctx->keys[i+1] = cast_sbox5[U8b(t[0])] ^ cast_sbox6[U8a(t[0])]
	^ cast_sbox7[U8c(t[3])] ^ cast_sbox8[U8d(t[3])];
      ctx->keys[i+2] = cast_sbox5[U8d(t[1])] ^ cast_sbox6[U8c(t[1])]
	^ cast_sbox7[U8a(t[2])] ^ cast_sbox8[U8b(t[2])];
      ctx->keys[i+3] = cast_sbox5[U8b(t[1])] ^ cast_sbox6[U8a(t[1])]
	^ cast_sbox7[U8c(t[2])] ^ cast_sbox8[U8d(t[2])];
      break;
    }
    switch (i & 12) {
    case 0:
      ctx->keys[i+0] ^= cast_sbox5[U8c(z[0])];
      ctx->keys[i+1] ^= cast_sbox6[U8c(z[1])];
      ctx->keys[i+2] ^= cast_sbox7[U8b(z[2])];
      ctx->keys[i+3] ^= cast_sbox8[U8a(z[3])];
      break;
    case 4:
      ctx->keys[i+0] ^= cast_sbox5[U8a(x[2])];
      ctx->keys[i+1] ^= cast_sbox6[U8b(x[3])];
      ctx->keys[i+2] ^= cast_sbox7[U8d(x[0])];
      ctx->keys[i+3] ^= cast_sbox8[U8d(x[1])];
      break;
    case 8:
      ctx->keys[i+0] ^= cast_sbox5[U8b(z[2])];
      ctx->keys[i+1] ^= cast_sbox6[U8a(z[3])];
      ctx->keys[i+2] ^= cast_sbox7[U8c(z[0])];
      ctx->keys[i+3] ^= cast_sbox8[U8c(z[1])];
      break;
    case 12:
      ctx->keys[i+0] ^= cast_sbox5[U8d(x[0])];
      ctx->keys[i+1] ^= cast_sbox6[U8d(x[1])];
      ctx->keys[i+2] ^= cast_sbox7[U8a(x[2])];
      ctx->keys[i+3] ^= cast_sbox8[U8b(x[3])];
      break;
    }
    if (i >= 16) {
      ctx->keys[i+0] &= 31;
      ctx->keys[i+1] &= 31;
      ctx->keys[i+2] &= 31;
      ctx->keys[i+3] &= 31;
    }
  }
  /* Wipe clean */
  for (i = 0; i < 4; i++) {
    t[i] = x[i] = z[i] = 0;
  }
}
