/* memxor.c
 *
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 1991, 1993, 1995 Free Software Foundation, Inc.
 * Copyright (C) 2010 Niels Möller
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

/* Implementation inspired by memcmp in glibc, contributed to the FSF
   by Torbjorn Granlund.
 */

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <limits.h>

#include "memxor.h"

typedef unsigned long int word_t;

#if SIZEOF_LONG & (SIZEOF_LONG - 1)
#error Word size must be a power of two
#endif

#define ALIGN_OFFSET(p) ((uintptr_t) (p) % sizeof(word_t))

#ifndef WORDS_BIGENDIAN
#define MERGE(w0, sh_1, w1, sh_2) (((w0) >> (sh_1)) | ((w1) << (sh_2)))
#else
#define MERGE(w0, sh_1, w1, sh_2) (((w0) << (sh_1)) | ((w1) >> (sh_2)))
#endif

#define WORD_T_THRESH 16

/* XOR word-aligned areas. n is the number of words, not bytes. */
static void
memxor_common_alignment (word_t *dst, const word_t *src, size_t n)
{
  /* FIXME: Require n > 0? */
  /* FIXME: Unroll four times, like memcmp? Probably not worth the
     effort. */

  if (n & 1)
    {
      *dst++ ^= *src++;
      n--;
    }
  for (; n >= 2; dst += 2, src += 2, n -= 2)
    {
      dst[0] ^= src[0];
      dst[1] ^= src[1];
    }
}

/* XOR *un-aligned* src-area onto aligned dst area. n is number of
   words, not bytes. Assumes we can read complete words at the start
   and end of the src operand. */
static void
memxor_different_alignment (word_t *dst, const uint8_t *src, size_t n)
{
  size_t i;
  int shl, shr;
  const word_t *src_word;
  unsigned offset = ALIGN_OFFSET (src);
  word_t s0, s1;

  shl = CHAR_BIT * offset;
  shr = CHAR_BIT * (sizeof(word_t) - offset);

  src_word = (const word_t *) ((uintptr_t) src & -SIZEOF_LONG);

  /* FIXME: Unroll four times, like memcmp? */
  i = n & 1;
  s0 = src_word[i];
  if (i)
    {
      s1 = src_word[0];
      dst[0] ^= MERGE (s1, shl, s0, shr);
    }

  for (; i < n; i += 2)
    {
      s1 = src_word[i+1];
      dst[i] ^= MERGE(s0, shl, s1, shr);
      s0 = src_word[i+2];
      dst[i+1] ^= MERGE(s1, shl, s0, shr);
    }
}

/* Performance, Intel SU1400 (x86_64): 0.25 cycles/byte aligned, 0.45
   cycles/byte unaligned. */

/* XOR LEN bytes starting at SRCADDR onto DESTADDR. Result undefined
   if the source overlaps with the destination. Return DESTADDR. */
uint8_t *
memxor(uint8_t *dst, const uint8_t *src, size_t n)
{
  uint8_t *orig_dst = dst;

  if (n >= WORD_T_THRESH)
    {
      /* There are at least some bytes to compare.  No need to test
	 for N == 0 in this alignment loop.  */
      while (ALIGN_OFFSET (dst))
	{
	  *dst++ ^= *src++;
	  n--;
	}
      if (ALIGN_OFFSET (src))
	memxor_different_alignment ((word_t *) dst, src, n / sizeof(word_t));
      else
	memxor_common_alignment ((word_t *) dst, (const word_t *) src, n / sizeof(word_t));

      dst += n & -SIZEOF_LONG;
      src += n & -SIZEOF_LONG;
      n = n & (SIZEOF_LONG - 1);
    }
  for (; n > 0; n--)
    *dst++ ^= *src++;

  return orig_dst;
}


/* XOR word-aligned areas. n is the number of words, not bytes. */
static void
memxor3_common_alignment (word_t *dst,
			  const word_t *a, const word_t *b, size_t n)
{
  /* FIXME: Require n > 0? */
  while (n-- > 0)
    dst[n] = a[n] ^ b[n];
}

static void
memxor3_different_alignment_b (word_t *dst,
			       const word_t *a, const uint8_t *b, unsigned offset, size_t n)
{
  int shl, shr;
  const word_t *b_word;

  word_t s0, s1;

  shl = CHAR_BIT * offset;
  shr = CHAR_BIT * (sizeof(word_t) - offset);

  b_word = (const word_t *) ((uintptr_t) b & -SIZEOF_LONG);

  if (n & 1)
    {
      n--;
      s1 = b_word[n];
      s0 = b_word[n+1];
      dst[n] = a[n] ^ MERGE (s1, shl, s0, shr);
    }
  else
    s1 = b_word[n];
  
  while (n > 0)
    {
      n -= 2;
      s0 = b_word[n+1]; 
      dst[n+1] = a[n+1] ^ MERGE(s0, shl, s1, shr);
      s1 = b_word[n];
      dst[n] = a[n] ^ MERGE(s1, shl, s0, shr);
    }
}

static void
memxor3_different_alignment_ab (word_t *dst,
				const uint8_t *a, const uint8_t *b,
				unsigned offset, size_t n)
{
  int shl, shr;
  const word_t *a_word;
  const word_t *b_word;
  
  word_t s0, s1;

  shl = CHAR_BIT * offset;
  shr = CHAR_BIT * (sizeof(word_t) - offset);

  a_word = (const word_t *) ((uintptr_t) a & -SIZEOF_LONG);
  b_word = (const word_t *) ((uintptr_t) b & -SIZEOF_LONG);

  if (n & 1)
    {
      n--;
      s1 = a_word[n] ^ b_word[n];
      s0 = a_word[n+1] ^ b_word[n+1];
      dst[n] = MERGE (s1, shl, s0, shr);
    }
  else    
    s1 = a_word[n] ^ b_word[n];
  
  while (n > 0)
    {
      n -= 2;
      s0 = a_word[n+1] ^ b_word[n+1]; 
      dst[n+1] = MERGE(s0, shl, s1, shr);
      s1 = a_word[n] ^ b_word[n];
      dst[n] = MERGE(s1, shl, s0, shr);
    }
}

static void
memxor3_different_alignment_all (word_t *dst,
				 const uint8_t *a, const uint8_t *b,
				 unsigned a_offset, unsigned b_offset,
				 size_t n)
{
  int al, ar, bl, br;
  const word_t *a_word;
  const word_t *b_word;
  
  word_t a0, a1, b0, b1;

  al = CHAR_BIT * a_offset;
  ar = CHAR_BIT * (sizeof(word_t) - a_offset);
  bl = CHAR_BIT * b_offset;
  br = CHAR_BIT * (sizeof(word_t) - b_offset);

  a_word = (const word_t *) ((uintptr_t) a & -SIZEOF_LONG);
  b_word = (const word_t *) ((uintptr_t) b & -SIZEOF_LONG);

  if (n & 1)
    {
      n--;
      a1 = a_word[n]; a0 = a_word[n+1];
      b1 = b_word[n]; b0 = b_word[n+1];
      
      dst[n] = MERGE (a1, al, a0, ar) ^ MERGE (b1, bl, b0, br);
    }
  else    
    {
      a1 = a_word[n];
      b1 = b_word[n];
    }
  
  while (n > 0)
    {
      n -= 2;
      a0 = a_word[n+1]; b0 = b_word[n+1]; 
      dst[n+1] = MERGE(a0, al, a1, ar) ^ MERGE(b0, bl, b1, br);
      a1 = a_word[n]; b1 = b_word[n];
      dst[n] = MERGE(a1, al, a0, ar) ^ MERGE(b1, bl, b0, br);
    }
}

/* Current implementation processes data in descending order, to
   support overlapping operation with one of the sources overlapping
   the start of the destination area. This feature is used only
   internally by cbc decrypt, and it is not advertised or documented
   to nettle users. */
uint8_t *
memxor3(uint8_t *dst, const uint8_t *a, const uint8_t *b, size_t n)
{
  if (n >= WORD_T_THRESH)
    {
      unsigned i;
      unsigned a_offset;
      unsigned b_offset;
      size_t nwords;

      for (i = ALIGN_OFFSET(dst + n); i > 0; i--)
	{
	  n--;
	  dst[n] = a[n] ^ b[n];
	}

      a_offset = ALIGN_OFFSET(a + n);
      b_offset = ALIGN_OFFSET(b + n);

      nwords = n / sizeof (word_t);
      n %= sizeof (word_t);

      if (a_offset == b_offset)
	{
	  if (!a_offset)
	    memxor3_common_alignment((word_t *) (dst + n),
				     (const word_t *) (a + n),
				     (const word_t *) (b + n), nwords);
	  else
	    memxor3_different_alignment_ab((word_t *) (dst + n),
					   a + n, b + n, a_offset,
					   nwords);
	}
      else if (!a_offset)
	memxor3_different_alignment_b((word_t *) (dst + n),
				      (const word_t *) (a + n), b + n,
				      b_offset, nwords);
      else if (!b_offset)
	memxor3_different_alignment_b((word_t *) (dst + n),
				      (const word_t *) (b + n), a + n,
				      a_offset, nwords);
      else
	memxor3_different_alignment_all((word_t *) (dst + n), a + n, b + n,
					a_offset, b_offset, nwords);
					
    }
  while (n-- > 0)
    dst[n] = a[n] ^ b[n];

  return dst;
}
