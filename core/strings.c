// Copyright (c) 2013-2014 Cloudozer LLP. All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
// 
// * Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// 
// * Redistributions in any form must be accompanied by information on how to
// obtain complete source code for the LING software and any accompanying
// software that uses the LING software. The source code must either be included
// in the distribution or be available for no more than the cost of distribution
// plus a nominal fee, and must be freely redistributable under reasonable
// conditions.  For an executable file, complete source code means the source
// code for all modules it contains. It does not include source code for modules
// or files that typically accompany the major components of the operating
// system on which the executable file runs.
// 
// THIS SOFTWARE IS PROVIDED BY CLOUDOZER LLP ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE
// DISCLAIMED. IN NO EVENT SHALL CLOUDOZER LLP BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

/**
 *
 *
 *
 */

#include <stdint.h>

#include "ctype.h"

int64_t strtoi64(const char *nptr, char **endptr, int base)
{
    const char *s;
    int64_t acc;
    int64_t val;
    int neg, any;
    char c;

    /*
     * Skip white space and pick up leading +/- sign if any.
     * If base is 0, allow 0x for hex and 0 for octal, else
     * assume decimal; if base is already 16, allow 0x.
     */
    s = nptr;
    do {
		c = *s++;
    } while (isspace((int)c));
    if (c == '-') {
		neg = 1;
		c = *s++;
    } else {
		neg = 0;
		if (c == '+')
	    	c = *s++;
    }
    if ((base == 0 || base == 16) &&
		c == '0' && (*s == 'x' || *s == 'X')) {
	    c = s[1];
	    s += 2;
	    base = 16;
    }
    if (base == 0)
		base = c == '0' ? 8 : 10;
    acc = any = 0;
    if (base < 2 || base > 36) {
        if (endptr != 0)
	    	*endptr = (char *)nptr;
        return acc;
    }

    val = 0;
    for ( ; ; c = *s++) {
        if (c >= '0' && c <= '9')
	    	c -= '0';
		else if (c >= 'A' && c <= 'Z')
		    c -= 'A' - 10;
		else if (c >= 'a' && c <= 'z')
	    	c -= 'a' - 10;
		else
		    break;
		if (c >= base)
		    break;
		val *= base;
        if ( (any < 0)	/* already noted an over/under flow - short circuit */
           || (neg && (val > acc || (val -= c) > acc)) /* underflow */
           || (!neg && (val < acc || (val += c) < acc))) {       /* overflow */
            any = -1;	/* once noted, over/underflows never go away */
        } else {
            acc = val;
	    	any = 1;
        }
    }

    if (any < 0)
		acc = neg ? INT64_MIN : INT64_MAX;
    if (endptr != 0)
		*endptr = (char *)(any ? s - 1 : nptr);
    return (acc);
}

int64_t atoi64(const char *buf)
{
    return strtoi64(buf, 0, 10);
}

char *i64toa(int64_t n, char *buf, int len)
{
	uint64_t nn;
	if (len < sizeof(int) * 3 + 2)
		return 0;
    char *start = buf + len - 1;
    int negative = 0;
    if (n < 0) {
		negative = 1;
		nn = -n;
    }
    else
    	nn = n;
    *start = 0;
    do {
		*--start = (char)('0' + (nn % 10));
		nn /= 10;
    } while (nn);
    if (negative) {
		*--start = '-';
    }
    return start;
}

/*EOF*/

