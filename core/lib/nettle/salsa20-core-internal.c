/* salsa20-core-internal.c
 *
 * Internal interface to the Salsa20 core function.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2012 Simon Josefsson, Niels Möller
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

/* Based on:
   salsa20-ref.c version 20051118
   D. J. Bernstein
   Public domain.
*/

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>
#include <string.h>

#include "salsa20.h"

#include "macros.h"

#ifndef SALSA20_DEBUG
# define SALSA20_DEBUG 0
#endif

#if SALSA20_DEBUG
# include <stdio.h>
# define DEBUG(i) do {				\
    unsigned debug_j;				\
    for (debug_j = 0; debug_j < 16; debug_j++)	\
      {						\
	if (debug_j == 0)			\
	  fprintf(stderr, "%2d:", (i));		\
	else if (debug_j % 4 == 0)		\
	  fprintf(stderr, "\n   ");		\
	fprintf(stderr, " %8x", x[debug_j]);	\
      }						\
    fprintf(stderr, "\n");			\
  } while (0)
#else
# define DEBUG(i)
#endif

#ifdef WORDS_BIGENDIAN
#define LE_SWAP32(v)				\
  ((ROTL32(8,  v) & 0x00FF00FFUL) |		\
   (ROTL32(24, v) & 0xFF00FF00UL))
#else
#define LE_SWAP32(v) (v)
#endif

#define QROUND(x0, x1, x2, x3) do { \
  x1 ^= ROTL32(7, x0 + x3);	    \
  x2 ^= ROTL32(9, x1 + x0);	    \
  x3 ^= ROTL32(13, x2 + x1);	    \
  x0 ^= ROTL32(18, x3 + x2);	    \
  } while(0)

void
_salsa20_core(uint32_t *dst, const uint32_t *src, unsigned rounds)
{
  uint32_t x[_SALSA20_INPUT_LENGTH];
  unsigned i;

  assert ( (rounds & 1) == 0);

  memcpy (x, src, sizeof(x));
  for (i = 0; i < rounds;i += 2)
    {
      DEBUG (i);
      QROUND(x[0], x[4], x[8], x[12]);
      QROUND(x[5], x[9], x[13], x[1]);
      QROUND(x[10], x[14], x[2], x[6]);
      QROUND(x[15], x[3], x[7], x[11]);

      DEBUG (i+1);
      QROUND(x[0], x[1], x[2], x[3]);
      QROUND(x[5], x[6], x[7], x[4]);
      QROUND(x[10], x[11], x[8], x[9]);
      QROUND(x[15], x[12], x[13], x[14]);
    }
  DEBUG (i);

  for (i = 0; i < _SALSA20_INPUT_LENGTH; i++)
    {
      uint32_t t = x[i] + src[i];
      dst[i] = LE_SWAP32 (t);
    }
}
