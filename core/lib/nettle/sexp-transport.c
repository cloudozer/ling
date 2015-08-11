/* sexp-transport.c
 *
 * Parsing s-expressions in transport format.
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

#include <assert.h>
#include <string.h>

#include "sexp.h"

#include "base64.h"

/* NOTE: Decodes the input string in place */
int
sexp_transport_iterator_first(struct sexp_iterator *iterator,
			      unsigned length, uint8_t *input)
{
  /* We first base64 decode any transport encoded sexp at the start of
   * the input. */

  unsigned in = 0;
  unsigned out = 0;

  while (in < length)
    switch(input[in])
      {
      case ' ':  /* SPC, TAB, LF, CR */
      case '\t':
      case '\n':
      case '\r':
	in++;
	break;
	  
      case ';':  /* Comments */
	while (++in < length && input[in] != '\n')
	  ;
	break;
	  
      case '{':
	{
	  /* Found transport encoding */
	  struct base64_decode_ctx ctx;
	  unsigned coded_length;
	  unsigned end;

	  for (end = ++in; end < length && input[end] != '}'; end++)
	    ;

	  if (end == length)
	    return 0;
	    
	  base64_decode_init(&ctx);
	  coded_length = end - in;
	  
	  if (base64_decode_update(&ctx, &coded_length, input + out,
				   coded_length, input + in)
	      && base64_decode_final(&ctx))
	    {	  
	      out += coded_length;
	      in = end + 1;
	    }
	  else
	    return 0;
	  
	  break;
	}
      default:
	/* Expression isn't in transport encoding. Rest of the input
	 * should be in canonical encoding. */
	goto transport_done;
      }
  
 transport_done:

  /* Here, we have two, possibly empty, input parts in canonical
   * encoding:
   *
   * 0...out-1,  in...length -1
   *
   * If the input was already in canonical encoding, out = 0 and in =
   * amount of leading space.
   *
   * If all input was in transport encoding, in == length.
   */
  if (!out)
    {
      input += in;
      length -= in;
    }
  else if (in == length)
    length = out;
  else if (out == in)
    /* Unusual case, nothing happens */
    ;
  else
    {
      /* Both parts non-empty */
      assert(out < in);
      memmove(input + out, input + in, length - in);
      length -= (in - out);
    }

  return sexp_iterator_first(iterator, length, input);
}
