/* nettle-meta-ciphers.c */

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

const struct nettle_cipher * const nettle_ciphers[] = {
  &nettle_aes128,
  &nettle_aes192,
  &nettle_aes256,
  &nettle_arcfour128,
  &nettle_camellia128,
  &nettle_camellia192,
  &nettle_camellia256,
  &nettle_cast128,
  &nettle_serpent128,
  &nettle_serpent192,
  &nettle_serpent256,
  &nettle_twofish128,
  &nettle_twofish192,
  &nettle_twofish256,
  &nettle_arctwo40,
  &nettle_arctwo64,
  &nettle_arctwo128,
  &nettle_arctwo_gutmann128,
  NULL
};
