/* base64-meta.c */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2002 Dan Egnor, Niels Möller
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

#include "nettle-meta.h"

#include "base64.h"

/* Same as the macros with the same name */
static unsigned
base64_encode_length(unsigned length)
{
  return BASE64_ENCODE_LENGTH(length);
}

static unsigned
base64_decode_length(unsigned length)
{
  return BASE64_DECODE_LENGTH(length);
}

const struct nettle_armor nettle_base64
= _NETTLE_ARMOR(base64, BASE64);
