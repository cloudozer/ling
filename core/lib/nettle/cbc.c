/* cbc.c
 *
 * Cipher block chaining mode.
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
#include <stdlib.h>
#include <string.h>

#include "cbc.h"

#include "memxor.h"
#include "nettle-internal.h"

void
cbc_encrypt(void *ctx, nettle_crypt_func *f,
	    unsigned block_size, uint8_t *iv,
	    unsigned length, uint8_t *dst,
	    const uint8_t *src)
{
  assert(!(length % block_size));

  for ( ; length; length -= block_size, src += block_size, dst += block_size)
    {
      memxor(iv, src, block_size);
      f(ctx, block_size, dst, iv);
      memcpy(iv, dst, block_size);
    }
}

/* Don't allocate any more space than this on the stack */
#define CBC_BUFFER_LIMIT 512

void
cbc_decrypt(void *ctx, nettle_crypt_func *f,
	    unsigned block_size, uint8_t *iv,
	    unsigned length, uint8_t *dst,
	    const uint8_t *src)
{
  assert(!(length % block_size));

  if (!length)
    return;

  if (src != dst)
    {
      /* Decrypt in ECB mode */
      f(ctx, length, dst, src);

      /* XOR the cryptotext, shifted one block */
      memxor(dst, iv, block_size);
      memxor(dst + block_size, src, length - block_size);
      memcpy(iv, src + length - block_size, block_size);
    }

  else
    {
      /* For in-place CBC, we decrypt into a temporary buffer of size
       * at most CBC_BUFFER_LIMIT, and process that amount of data at
       * a time. */
      
      /* NOTE: We assume that block_size <= CBC_BUFFER_LIMIT, and we
	 depend on memxor3 working from the end of the area, allowing
	 certain overlapping operands. */ 

      TMP_DECL(buffer, uint8_t, CBC_BUFFER_LIMIT);
      TMP_DECL(initial_iv, uint8_t, NETTLE_MAX_CIPHER_BLOCK_SIZE);

      unsigned buffer_size;

      if (length <= CBC_BUFFER_LIMIT)
	buffer_size = length;
      else
	buffer_size
	  = CBC_BUFFER_LIMIT - (CBC_BUFFER_LIMIT % block_size);

      TMP_ALLOC(buffer, buffer_size);
      TMP_ALLOC(initial_iv, block_size);

      for ( ; length > buffer_size;
	    length -= buffer_size, src += buffer_size, dst += buffer_size)
	{
	  f(ctx, buffer_size, buffer, src);
	  memcpy(initial_iv, iv, block_size);
	  memcpy(iv, src + buffer_size - block_size, block_size);
	  memxor3(dst + block_size, buffer + block_size, src,
		  buffer_size - block_size);
	  memxor3(dst, buffer, initial_iv, block_size);
	}

      f(ctx, length, buffer, src);
      memcpy(initial_iv, iv, block_size);
      /* Copies last block */
      memcpy(iv, src + length - block_size, block_size);
      /* Writes all but first block, reads all but last block. */
      memxor3(dst + block_size, buffer + block_size, src,
	      length - block_size);
      /* Writes first block. */
      memxor3(dst, buffer, initial_iv, block_size);
    }
}

#if 0
#include "twofish.h"
#include "aes.h"

static void foo(void)
{
  struct CBC_CTX(struct twofish_ctx, TWOFISH_BLOCK_SIZE) ctx;
  uint8_t src[TWOFISH_BLOCK_SIZE];
  uint8_t dst[TWOFISH_BLOCK_SIZE];
  
  CBC_ENCRYPT(&ctx, twofish_encrypt, TWOFISH_BLOCK_SIZE, dst, src);

  /* Should result in a warning */
  CBC_ENCRYPT(&ctx, aes_encrypt, TWOFISH_BLOCK_SIZE, dst, src);
  
}

static void foo2(void)
{
  struct twofish_ctx ctx;
  uint8_t iv[TWOFISH_BLOCK_SIZE];
  uint8_t src[TWOFISH_BLOCK_SIZE];
  uint8_t dst[TWOFISH_BLOCK_SIZE];
  
  CBC_ENCRYPT2(&ctx, twofish_encrypt, TWOFISH_BLOCK_SIZE, iv, TWOFISH_BLOCK_SIZE, dst, src);
  /* Should result in a warning */
  CBC_ENCRYPT2(&ctx, aes_encrypt, TWOFISH_BLOCK_SIZE, iv, TWOFISH_BLOCK_SIZE, dst, src);
}

#endif
