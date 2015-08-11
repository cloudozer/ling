/* sha3.c
 *
 * The sha3 hash function.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2012 Niels Möller
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
#include <string.h>

#include "sha3.h"

#include "macros.h"
#include "memxor.h"

static void
sha3_absorb (struct sha3_state *state, unsigned length, const uint8_t *data)
{
  assert ( (length & 7) == 0);
#if WORDS_BIGENDIAN
  {    
    uint64_t *p;
    for (p = state->a; length > 0; p++, length -= 8, data += 8)
      *p ^= LE_READ_UINT64 (data);
  }
#else /* !WORDS_BIGENDIAN */
  memxor ((uint8_t *) state->a, data, length);
#endif

  sha3_permute (state);
}

unsigned
_sha3_update (struct sha3_state *state,
	      unsigned block_size, uint8_t *block,
	      unsigned pos,
	      unsigned length, const uint8_t *data)
{
  if (pos > 0)
    {
      unsigned left = block_size - pos;
      if (length < left)
	{
	  memcpy (block + pos, data, length);
	  return pos + length;
	}
      else
	{
	  memcpy (block + pos, data, left);
	  data += left;
	  length -= left;
	  sha3_absorb (state, block_size, block);
	}
    }
  for (; length >= block_size; length -= block_size, data += block_size)
    sha3_absorb (state, block_size, data);

  memcpy (block, data, length);
  return length;
}

void
_sha3_pad (struct sha3_state *state,
	   unsigned block_size, uint8_t *block, unsigned pos)
{
  assert (pos < block_size);
  block[pos++] = 1;

  memset (block + pos, 0, block_size - pos);
  block[block_size - 1] |= 0x80;

  sha3_absorb (state, block_size, block);  
}
