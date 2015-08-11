/* dsa2sexp.c
 *
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2002, 2009 Niels Möller, Magnus Holmgren
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

#include "dsa.h"

#include "sexp.h"

int
dsa_keypair_to_sexp(struct nettle_buffer *buffer,
		    const char *algorithm_name,
		    const struct dsa_public_key *pub,
		    const struct dsa_private_key *priv)
{
  if (!algorithm_name)
    algorithm_name = "dsa";
  
  if (priv)
    return sexp_format(buffer,
		       "(private-key(%0s(p%b)(q%b)"
		       "(g%b)(y%b)(x%b)))",
		       algorithm_name, pub->p, pub->q,
		       pub->g, pub->y, priv->x);
  else
    return sexp_format(buffer,
		       "(public-key(%0s(p%b)(q%b)"
		       "(g%b)(y%b)))",
		       algorithm_name, pub->p, pub->q,
		       pub->g, pub->y);
}
