/*
 * double strtodx (char *string, char **endPtr, int radix)
 *	This procedure converts a floating-point number from an ASCII
 *	decimal representation to internal double-precision format.
 *
 * Original sources taken from 386bsd and modified for variable radix
 * by Serge Vakulenko, <vak@kiae.su>.
 *
 * Arguments:
 * string
 *      A decimal ASCII floating-point number, optionally preceded
 *      by white space. Must have form "-I.FE-X", where I is the integer
 *      part of the mantissa, F is the fractional part of the mantissa,
 *      and X is the exponent.  Either of the signs may be "+", "-", or
 *      omitted.  Either I or F may be omitted, or both.  The decimal point
 *      isn't necessary unless F is present. The "E" may actually be an "e",
 *      or "E", "S", "s", "F", "f", "D", "d", "L", "l".
 *      E and X may both be omitted (but not just one).
 *
 * endPtr
 *      If non-NULL, store terminating character's address here.
 *
 * radix
 *      Radix of floating point, one of 2, 8, 10, 16.
 *
 * The return value is the double-precision floating-point
 * representation of the characters in string.  If endPtr isn't
 * NULL, then *endPtr is filled in with the address of the
 * next character after the last one that was part of the
 * floating-point number.
 */

#include <stdlib.h>
#include <ctype.h>

double strtod(const char *string, char **endPtr)
{
	int sign = 0, expSign = 0, i;
	double fraction, dblExp;
	register const char *p;
	register char c;

	/* Exponent read from "EX" field. */
	int exp = 0;

	/* Exponent that derives from the fractional part.  Under normal
	 * circumstances, it is the negative of the number of digits in F.
	 * However, if I is very long, the last digits of I get dropped
	 * (otherwise a long I with a large negative exponent could cause an
	 * unnecessary overflow on I alone).  In this case, fracExp is
	 * incremented one for each dropped digit. */
	int fracExp = 0;

	/* Number of digits in mantissa. */
	int mantSize;

	/* Number of mantissa digits BEFORE decimal point. */
	int decPt;

	/* Temporarily holds location of exponent in string. */
	const char *pExp;

	/* Largest possible base 10 exponent.
	 * Any exponent larger than this will already
	 * produce underflow or overflow, so there's
	 * no need to worry about additional digits. */
	static int maxExponent = 307;

	/* Table giving binary powers of 10.
	 * Entry is 10^2^i.  Used to convert decimal
	 * exponents into floating-point numbers. */
	static double powersOf10[] = {
		1e1, 1e2, 1e4, 1e8, 1e16, 1e32, 1e64, 1e128, 1e256
	};

	/*
	 * Strip off leading blanks and check for a sign.
	 */
	p = string;
	while (*p==' ' || *p=='\t')
		++p;
	if (*p == '-') {
		sign = 1;
		++p;
	} else if (*p == '+')
		++p;

	/*
	 * Count the number of digits in the mantissa (including the decimal
	 * point), and also locate the decimal point.
	 */
	decPt = -1;
	for (mantSize=0; ; ++mantSize) {
		c = *p;
		if (! isdigit ((int)c)) {
			if (c != '.' || decPt >= 0)
				break;
			decPt = mantSize;
		}
		++p;
	}

	/*
	 * Now suck up the digits in the mantissa.  Use two integers to
	 * collect 9 digits each (this is faster than using floating-point).
	 * If the mantissa has more than 18 digits, ignore the extras, since
	 * they can't affect the value anyway.
	 */
	pExp = p;
	p -= mantSize;
	if (decPt < 0)
		decPt = mantSize;
	else
		--mantSize;             /* One of the digits was the point. */

	if (mantSize > 2 * 9)
		mantSize = 2 * 9;
	fracExp = decPt - mantSize;
	if (mantSize == 0) {
		fraction = 0.0;
		p = string;
		goto done;
	} else {
		int frac1, frac2;

		for (frac1=0; mantSize>9; --mantSize) {
			c = *p++;
			if (c == '.')
				c = *p++;
			frac1 = frac1 * 10 + (c - '0');
		}
		for (frac2=0; mantSize>0; --mantSize) {
			c = *p++;
			if (c == '.')
				c = *p++;
			frac2 = frac2 * 10 + (c - '0');
		}
		fraction = (double) 1000000000 * frac1 + frac2;
	}

	/*
	 * Skim off the exponent.
	 */
	p = pExp;
	if (*p=='E' || *p=='e' || *p=='S' || *p=='s' || *p=='F' || *p=='f' ||
	     *p=='D' || *p=='d' || *p=='L' || *p=='l') {
		++p;
		if (*p == '-') {
			expSign = 1;
			++p;
		} else if (*p == '+')
			++p;
		while (isdigit ((int)*p))
			exp = exp * 10 + (*p++ - '0');
	}
	if (expSign)
		exp = fracExp - exp;
	else
		exp = fracExp + exp;

	/*
	 * Generate a floating-point number that represents the exponent.
	 * Do this by processing the exponent one bit at a time to combine
	 * many powers of 2 of 10. Then combine the exponent with the
	 * fraction.
	 */
	if (exp < 0) {
		expSign = 1;
		exp = -exp;
	} else
		expSign = 0;
	if (exp > maxExponent)
		exp = maxExponent;
	dblExp = 1.0;
	for (i=0; exp; exp>>=1, ++i)
		if (exp & 01)
			dblExp *= powersOf10[i];
	if (expSign)
		fraction /= dblExp;
	else
		fraction *= dblExp;

done:
	if (endPtr)
		*endPtr = (char*) p;

	return sign ? -fraction : fraction;
}
