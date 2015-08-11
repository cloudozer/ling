/* gcm_aes.c
 *
 * Galois counter mode using AES as the underlying cipher.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2011 Niels Möller
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

#include "gcm.h"

void
gcm_aes_set_key(struct gcm_aes_ctx *ctx, unsigned length, const uint8_t *key)
{
  GCM_SET_KEY(ctx, aes_set_encrypt_key, aes_encrypt, length, key);
}

void
gcm_aes_set_iv(struct gcm_aes_ctx *ctx,
	       unsigned length, const uint8_t *iv)
{
  GCM_SET_IV(ctx, length, iv);
}

void
gcm_aes_update(struct gcm_aes_ctx *ctx, unsigned length, const uint8_t *data)
{
  GCM_UPDATE(ctx, length, data);
}

void
gcm_aes_encrypt(struct gcm_aes_ctx *ctx,
		unsigned length, uint8_t *dst, const uint8_t *src)
{
  GCM_ENCRYPT(ctx, aes_encrypt, length, dst, src);
}

void
gcm_aes_decrypt(struct gcm_aes_ctx *ctx,
		unsigned length, uint8_t *dst, const uint8_t *src)
{
  GCM_DECRYPT(ctx, aes_encrypt, length, dst, src);
}

void
gcm_aes_digest(struct gcm_aes_ctx *ctx,
	       unsigned length, uint8_t *digest)
{
  GCM_DIGEST(ctx, aes_encrypt, length, digest);
  
}
