/* nettle-internal.c
 *
 * Things that are used only by the testsuite and benchmark, and
 * subject to change.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2002 Niels Möller
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
#include <stdlib.h>

#include "nettle-internal.h"
#include "blowfish.h"
#include "des.h"
#include "gcm.h"
#include "salsa20.h"

/* DES uses a different signature for the key set function. We ignore
   the return value indicating weak keys. */
static void
des_set_key_hack(void *ctx, unsigned length, const uint8_t *key)
{
  assert(length == DES_KEY_SIZE);
  des_set_key(ctx, key);
}

static void
des3_set_key_hack(void *ctx, unsigned length, const uint8_t *key)
{
  assert(length == DES3_KEY_SIZE);
  des3_set_key(ctx, key);
}

/* NOTE: A bit ugly. Ignores weak keys, and pretends the set:key
   functions have no return value. */
const struct nettle_cipher
nettle_des = {
  "des", sizeof(struct des_ctx),
  DES_BLOCK_SIZE, DES_KEY_SIZE,
  des_set_key_hack, des_set_key_hack,
  (nettle_crypt_func *) des_encrypt,
  (nettle_crypt_func *) des_decrypt
};

const struct nettle_cipher
nettle_des3 = {
 "des3", sizeof(struct des3_ctx),
 DES3_BLOCK_SIZE, DES3_KEY_SIZE,
 des3_set_key_hack, des3_set_key_hack,
 (nettle_crypt_func *) des3_encrypt,
 (nettle_crypt_func *) des3_decrypt
};

/* NOTE: This is not as nice as one might think, as we pretend
   blowfish_set_key has no return value. */
const struct nettle_cipher
nettle_blowfish128 = _NETTLE_CIPHER(blowfish, BLOWFISH, 128);

/* Sets a fix zero iv. For benchmarking only. */
static void
salsa20_set_key_hack(void *ctx, unsigned length, const uint8_t *key)
{
  static const uint8_t iv[SALSA20_IV_SIZE];
  salsa20_set_key (ctx, length, key);
  salsa20_set_iv (ctx, iv);
}

/* Claim zero block size, to classify as a stream cipher. */
const struct nettle_cipher
nettle_salsa20 = {
  "salsa20", sizeof(struct salsa20_ctx),
  0, SALSA20_KEY_SIZE,
  salsa20_set_key_hack, salsa20_set_key_hack,
  (nettle_crypt_func *) salsa20_crypt,
  (nettle_crypt_func *) salsa20_crypt
};

const struct nettle_cipher
nettle_salsa20r12 = {
  "salsa20r12", sizeof(struct salsa20_ctx),
  0, SALSA20_KEY_SIZE,
  salsa20_set_key_hack, salsa20_set_key_hack,
  (nettle_crypt_func *) salsa20r12_crypt,
  (nettle_crypt_func *) salsa20r12_crypt
};

const struct nettle_aead
nettle_gcm_aes128 = _NETTLE_AEAD(gcm, GCM, aes, 128);
const struct nettle_aead
nettle_gcm_aes192 = _NETTLE_AEAD(gcm, GCM, aes, 192);
const struct nettle_aead
nettle_gcm_aes256 = _NETTLE_AEAD(gcm, GCM, aes, 256);
