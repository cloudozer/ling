/* sexp2bignum.c
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

#include "sexp.h"
#include "bignum.h"

int
nettle_mpz_set_sexp(mpz_t x, unsigned limit, struct sexp_iterator *i)
{
  if (i->type == SEXP_ATOM
      && i->atom_length
      && !i->display)
    {
      /* Allow some extra here, for leading sign octets. */
      if (limit && (8 * i->atom_length > (16 + limit)))
	return 0;
      
      nettle_mpz_set_str_256_s(x, i->atom_length, i->atom);

      /* FIXME: How to interpret a limit for negative numbers? */
      if (limit && mpz_sizeinbase(x, 2) > limit)
	return 0;
      
      return sexp_iterator_next(i);
    }
  else
    return 0;
}
