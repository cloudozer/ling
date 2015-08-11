/* sexp-transport-format.c
 *
 * Writing s-expressions in transport format.
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

#include "base64.h"
#include "buffer.h"

unsigned
sexp_transport_vformat(struct nettle_buffer *buffer,
		       const char *format, va_list args)
{
  unsigned start = 0;
  unsigned length;
  unsigned base64_length;

  if (buffer)
    {
      if (!NETTLE_BUFFER_PUTC(buffer, '{'))
	return 0;

      start = buffer->size;
    }
  
  length = sexp_vformat(buffer, format, args);

  if (!length)
    return 0;

  base64_length = BASE64_ENCODE_RAW_LENGTH(length);

  if (buffer)
    {
      if (!nettle_buffer_space(buffer, base64_length - length))
	return 0;

      base64_encode_raw(buffer->contents + start,
			length, buffer->contents + start);
      
      if (!NETTLE_BUFFER_PUTC(buffer, '}'))
	return 0;
    }
  
  return base64_length + 2;
}

unsigned
sexp_transport_format(struct nettle_buffer *buffer,
		      const char *format, ...)
{
  unsigned done;
  va_list args;

  va_start(args, format);
  done = sexp_transport_vformat(buffer, format, args);
  va_end(args);
  
  return done;
}
