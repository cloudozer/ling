/* nettle-meta-hashes.c */

/* nettle, low-level cryptographics library
 *  
 * Copyright (C) 2011 Daniel Kahn Gillmor
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

#include <stddef.h>
#include "nettle-meta.h"

const struct nettle_hash * const nettle_hashes[] = {
  &nettle_md2,
  &nettle_md4,
  &nettle_md5,
  &nettle_ripemd160,
  &nettle_sha1,
  &nettle_sha224,
  &nettle_sha256,
  &nettle_sha384,
  &nettle_sha512,
  NULL
};
