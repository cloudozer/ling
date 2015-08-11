/* realloc.c
 *
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

#include <stdlib.h>
#include <stdio.h>

#include "realloc.h"

/* NOTE: Calling libc realloc with size == 0 is not required to
   totally free the object, it is allowed to return a valid
   pointer. */
void *
nettle_realloc(void *ctx UNUSED, void *p, unsigned length)
{
  if (length > 0)
    return realloc(p, length);

  free(p);
  return NULL;
}

void *
nettle_xrealloc(void *ctx UNUSED, void *p, unsigned length)
{
  if (length > 0)
    {
      void *n = realloc(p, length);
      if (!n)
	{
	  fprintf(stderr, "Virtual memory exhausted.\n");
	  abort();
	}
      return n;
    }
  free(p);
  return NULL;
}
