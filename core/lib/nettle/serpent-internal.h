/* serpent-internal-h
 *
 * The serpent block cipher.
 *
 * For more details on this algorithm, see the Serpent website at
 * http://www.cl.cam.ac.uk/~rja14/serpent.html
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2011  Niels Möller
 * Copyright (C) 2010, 2011  Simon Josefsson
 * Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
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

/* This file is derived from cipher/serpent.c in Libgcrypt v1.4.6.
   The adaption to Nettle was made by Simon Josefsson on 2010-12-07
   with final touches on 2011-05-30.  Changes include replacing
   libgcrypt with nettle in the license template, renaming
   serpent_context to serpent_ctx, renaming u32 to uint32_t, removing
   libgcrypt stubs and selftests, modifying entry function prototypes,
   using FOR_BLOCKS to iterate through data in encrypt/decrypt, using
   LE_READ_UINT32 and LE_WRITE_UINT32 to access data in
   encrypt/decrypt, and running indent on the code. */

#ifndef NETTLE_SERPENT_INTERNAL_H_INCLUDED
#define NETTLE_SERPENT_INTERNAL_H_INCLUDED

#define KEYXOR(x0,x1,x2,x3, subkey)		       \
  do {						       \
    (x0) ^= (subkey)[0];			       \
    (x1) ^= (subkey)[1];			       \
    (x2) ^= (subkey)[2];			       \
    (x3) ^= (subkey)[3];			       \
  } while (0)

#if HAVE_NATIVE_64_BIT
/* Operate independently on both halves of a 64-bit word. */
#define DROTL32(n,x) \
  (((x) << (n) & ~((((uint64_t) 1 << (n))-1) << 32)) \
   |(((x) >> (32-(n))) & ~((((uint64_t) 1 << (32-(n)))-1) << (n))))

#define KEYXOR64(x0,x1,x2,x3, subkey)		       \
  do {						       \
    uint64_t _sk;				       \
    _sk = (subkey)[0]; _sk |= _sk << 32; (x0) ^= _sk;    \
    _sk = (subkey)[1]; _sk |= _sk << 32; (x1) ^= _sk;    \
    _sk = (subkey)[2]; _sk |= _sk << 32; (x2) ^= _sk;    \
    _sk = (subkey)[3]; _sk |= _sk << 32; (x3) ^= _sk;    \
  } while (0)

#define DRSHIFT32(n,x) \
  ( ((x) << (n)) & ~((((uint64_t) 1 << (n)) - 1) << 32))
#endif /* HAVE_NATIVE_64_BIT */

#endif /* NETTLE_SERPENT_INTERNAL_H_INCLUDED */

