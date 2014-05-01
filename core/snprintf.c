/* Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "snprintf.h"

#include <stdint.h>
#include <math.h>
#include <ctype.h>
#include <string.h>

// %pt format specifier to format Erlang terms adds coupling between Erlang and C
// lib code. Thus it is compiled in only if TERM_FMT_SPEC is defined.
#define TERM_FMT_SPEC

#ifdef TERM_FMT_SPEC
#include "stringify.h"

#define TERM_BUF_SIZE	4096
#endif

typedef enum {
    NO = 0, YES = 1
} boolean_e;

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif
#define NUL '\0'

static const char null_string[] = "(null)";
#define S_NULL ((char *)null_string)
#define S_NULL_LEN 6

#define FLOAT_DIGITS 6
#define EXPONENT_LENGTH 10

/*
 * NUM_BUF_SIZE is the size of the buffer used for arithmetic conversions
 *
 * NOTICE: this is a magic number; do not decrease it
 */
#define NUM_BUF_SIZE 512

/*
 * cvt - IEEE floating point formatting routines.
 *       Derived from UNIX V7, Copyright(C) Caldera International Inc.
 */

/*
 *    ecvt converts to decimal
 *      the number of digits is specified by ndigit
 *      decpt is set to the position of the decimal point
 *      sign is set to 0 for positive, 1 for negative
 */

#define NDIG 512

/* buf must have at least NDIG bytes */
static char *cvt(double arg, int ndigits, int *decpt, int *sign, 
                     int eflag, char *buf)
{
    register int r2;
    double fi, fj;
    register char *p, *p1;
    
    if (ndigits >= NDIG - 1)
        ndigits = NDIG - 2;
    r2 = 0;
    *sign = 0;
    p = &buf[0];
    if (arg < 0) {
        *sign = 1;
        arg = -arg;
    }
    arg = modf(arg, &fi);
    p1 = &buf[NDIG];
    /*
     * Do integer part
     */
    if (fi != 0) {
        p1 = &buf[NDIG];
        while (p1 > &buf[0] && fi != 0) {
            fj = modf(fi / 10, &fi);
            *--p1 = (int) ((fj + .03) * 10) + '0';
            r2++;
        }
        while (p1 < &buf[NDIG])
            *p++ = *p1++;
    }
    else if (arg > 0) {
        while ((fj = arg * 10) < 1) {
            arg = fj;
            r2--;
        }
    }
    p1 = &buf[ndigits];
    if (eflag == 0)
        p1 += r2;
    if (p1 < &buf[0]) {
        *decpt = -ndigits;
        buf[0] = '\0';
        return (buf);
    }
    *decpt = r2;
    while (p <= p1 && p < &buf[NDIG]) {
        arg *= 10;
        arg = modf(arg, &fj);
        *p++ = (int) fj + '0';
    }
    if (p1 >= &buf[NDIG]) {
        buf[NDIG - 1] = '\0';
        return (buf);
    }
    p = p1;
    *p1 += 5;
    while (*p1 > '9') {
        *p1 = '0';
        if (p1 > buf)
            ++ * --p1;
        else {
            *p1 = '1';
            (*decpt)++;
            if (eflag == 0) {
                if (p > buf)
                    *p = '0';
                p++;
            }
        }
    }
    *p = '\0';
    return (buf);
}

static char *ecvt(double arg, int ndigits, int *decpt, int *sign, char *buf)
{
    return (cvt(arg, ndigits, decpt, sign, 1, buf));
}

static char *fcvt(double arg, int ndigits, int *decpt, int *sign, char *buf)
{
    return (cvt(arg, ndigits, decpt, sign, 0, buf));
}

/*
 * gcvt  - Floating output conversion to
 * minimal length string
 */

static char *gcvt(double number, int ndigit, char *buf, boolean_e altform)
{
    int sign, decpt;
    register char *p1, *p2;
    register int i;
    char buf1[NDIG];

    p1 = ecvt(number, ndigit, &decpt, &sign, buf1);
    p2 = buf;
    if (sign)
        *p2++ = '-';
    for (i = ndigit - 1; i > 0 && p1[i] == '0'; i--)
        ndigit--;
    if ((decpt >= 0 && decpt - ndigit > 4)
        || (decpt < 0 && decpt < -3)) {                /* use E-style */
        decpt--;
        *p2++ = *p1++;
        *p2++ = '.';
        for (i = 1; i < ndigit; i++)
            *p2++ = *p1++;
        *p2++ = 'e';
        if (decpt < 0) {
            decpt = -decpt;
            *p2++ = '-';
        }
        else
            *p2++ = '+';
        if (decpt / 100 > 0)
            *p2++ = decpt / 100 + '0';
        if (decpt / 10 > 0)
            *p2++ = (decpt % 100) / 10 + '0';
        *p2++ = decpt % 10 + '0';
    }
    else {
        if (decpt <= 0) {
            if (*p1 != '0')
                *p2++ = '.';
            while (decpt < 0) {
                decpt++;
                *p2++ = '0';
            }
        }
        for (i = 1; i <= ndigit; i++) {
            *p2++ = *p1++;
            if (i == decpt)
                *p2++ = '.';
        }
        if (ndigit < decpt) {
            while (ndigit++ < decpt)
                *p2++ = '0';
            *p2++ = '.';
        }
    }
    if (p2[-1] == '.' && !altform)
        p2--;
    *p2 = '\0';
    return (buf);
}

/*
 * The INS_CHAR macro inserts a character in the buffer and writes
 * the buffer back to disk if necessary
 * It uses the char pointers sp and bep:
 *      sp points to the next available character in the buffer
 *      bep points to the end-of-buffer+1
 * While using this macro, note that the nextb pointer is NOT updated.
 *
 * NOTE: Evaluation of the c argument should not have any side-effects
 */
#define INS_CHAR(c, sp, bep, cc)                    \
{                                                   \
    if (sp) {                                       \
        if (sp >= bep) {                            \
            vbuff->curpos = sp;                     \
            if (flush_func(vbuff))                  \
                return -1;                          \
            sp = vbuff->curpos;                     \
            bep = vbuff->endpos;                    \
        }                                           \
        *sp++ = (c);                                \
    }                                               \
    cc++;                                           \
}

#define NUM(c) (c - '0')

#define STR_TO_DEC(str, num)                        \
    num = NUM(*str++);                              \
    while (isdigit(*str))                       \
    {                                               \
        num *= 10 ;                                 \
        num += NUM(*str++);                         \
    }

/*
 * This macro does zero padding so that the precision
 * requirement is satisfied. The padding is done by
 * adding '0's to the left of the string that is going
 * to be printed. We don't allow precision to be large
 * enough that we continue past the start of s.
 *
 * NOTE: this makes use of the magic info that s is
 * always based on num_buf with a size of NUM_BUF_SIZE.
 */
#define FIX_PRECISION(adjust, precision, s, s_len)  \
    if (adjust) {                                   \
        size_t p = (precision + 1 < NUM_BUF_SIZE) \
                     ? precision : NUM_BUF_SIZE - 1;  \
        while (s_len < p)                           \
        {                                           \
            *--s = '0';                             \
            s_len++;                                \
        }                                           \
    }

/*
 * Macro that does padding. The padding is done by printing
 * the character ch.
 */
#define PAD(width, len, ch)                         \
do                                                  \
{                                                   \
    INS_CHAR(ch, sp, bep, cc);                      \
    width--;                                        \
}                                                   \
while (width > len)

/*
 * Prefix the character ch to the string str
 * Increase length
 * Set the has_prefix flag
 */
#define PREFIX(str, length, ch)                     \
    *--str = ch;                                    \
    length++;                                       \
    has_prefix=YES;


/*
 * Convert num to its decimal format.
 * Return value:
 *   - a pointer to a string containing the number (no sign)
 *   - len contains the length of the string
 *   - is_negative is set to TRUE or FALSE depending on the sign
 *     of the number (always set to FALSE if is_unsigned is TRUE)
 *
 * The caller provides a buffer for the string: that is the buf_end argument
 * which is a pointer to the END of the buffer + 1 (i.e. if the buffer
 * is declared as buf[ 100 ], buf_end should be &buf[ 100 ])
 *
 * Note: we have 2 versions. One is used when we need to use quads
 * (conv_10_quad), the other when we don't (conv_10). We're assuming the
 * latter is faster.
 */
static char *conv_10(register int32_t num, register int is_unsigned,
                     register int *is_negative, char *buf_end,
                     register size_t *len)
{
    register char *p = buf_end;
    register uint32_t magnitude = num;

    if (is_unsigned) {
        *is_negative = FALSE;
    }
    else {
        *is_negative = (num < 0);

        /*
         * On a 2's complement machine, negating the most negative integer 
         * results in a number that cannot be represented as a signed integer.
         * Here is what we do to obtain the number's magnitude:
         *      a. add 1 to the number
         *      b. negate it (becomes positive)
         *      c. convert it to unsigned
         *      d. add 1
         */
        if (*is_negative) {
            int32_t t = num + 1;
            magnitude = ((uint32_t) -t) + 1;
        }
    }

    /*
     * We use a do-while loop so that we write at least 1 digit 
     */
    do {
        register uint32_t new_magnitude = magnitude / 10;

        *--p = (char) (magnitude - new_magnitude * 10 + '0');
        magnitude = new_magnitude;
    }
    while (magnitude);

    *len = buf_end - p;
    return (p);
}

static char *conv_10_quad(int64_t num, register int is_unsigned,
                     register int *is_negative, char *buf_end,
                     register size_t *len)
{
    register char *p = buf_end;
    uint64_t magnitude = num;

    /*
     * We see if we can use the faster non-quad version by checking the
     * number against the largest long value it can be. If <=, we
     * punt to the quicker version.
     */
    if ((magnitude <= UINT32_MAX && is_unsigned)
        || (num <= INT32_MAX && num >= INT32_MIN && !is_unsigned))
            return(conv_10((int32_t)num, is_unsigned, is_negative, buf_end, len));

    if (is_unsigned) {
        *is_negative = FALSE;
    }
    else {
        *is_negative = (num < 0);

        /*
         * On a 2's complement machine, negating the most negative integer 
         * results in a number that cannot be represented as a signed integer.
         * Here is what we do to obtain the number's magnitude:
         *      a. add 1 to the number
         *      b. negate it (becomes positive)
         *      c. convert it to unsigned
         *      d. add 1
         */
        if (*is_negative) {
            int64_t t = num + 1;
            magnitude = ((uint64_t) -t) + 1;
        }
    }

    /*
     * We use a do-while loop so that we write at least 1 digit 
     */
    do {
        uint64_t new_magnitude = magnitude / 10;

        *--p = (char) (magnitude - new_magnitude * 10 + '0');
        magnitude = new_magnitude;
    }
    while (magnitude);

    *len = buf_end - p;
    return (p);
}

/*
 * Convert a floating point number to a string formats 'f', 'e' or 'E'.
 * The result is placed in buf, and len denotes the length of the string
 * The sign is returned in the is_negative argument (and is not placed
 * in buf).
 */
static char *conv_fp(register char format, register double num,
    boolean_e add_dp, int precision, int *is_negative,
    char *buf, size_t *len)
{
    register char *s = buf;
    register char *p;
    int decimal_point;
    char buf1[NDIG];

    if (format == 'f')
        p = fcvt(num, precision, &decimal_point, is_negative, buf1);
    else /* either e or E format */
        p = ecvt(num, precision + 1, &decimal_point, is_negative, buf1);

    /*
     * Check for Infinity and NaN
     */
    if (isalpha(*p)) {
        *len = strlen(p);
        memcpy(buf, p, *len + 1);
        *is_negative = FALSE;
        return (buf);
    }

    if (format == 'f') {
        if (decimal_point <= 0) {
            *s++ = '0';
            if (precision > 0) {
                *s++ = '.';
                while (decimal_point++ < 0)
                    *s++ = '0';
            }
            else if (add_dp)
                *s++ = '.';
        }
        else {
            while (decimal_point-- > 0)
                *s++ = *p++;
            if (precision > 0 || add_dp)
                *s++ = '.';
        }
    }
    else {
        *s++ = *p++;
        if (precision > 0 || add_dp)
            *s++ = '.';
    }

    /*
     * copy the rest of p, the NUL is NOT copied
     */
    while (*p)
        *s++ = *p++;

    if (format != 'f') {
        char temp[EXPONENT_LENGTH];        /* for exponent conversion */
        size_t t_len;
        int exponent_is_negative;

        *s++ = format;                /* either e or E */

		//
		// XXX: without the check the routine converts 0.0 to "0.0e-1"
		//
		if (num != 0.0)
			decimal_point--;
        if (decimal_point != 0) {
            p = conv_10((int32_t) decimal_point, FALSE, &exponent_is_negative,
                        &temp[EXPONENT_LENGTH], &t_len);
            *s++ = exponent_is_negative ? '-' : '+';

            /*
             * Make sure the exponent has at least 2 digits
             */
            if (t_len == 1)
                *s++ = '0';
            while (t_len--)
                *s++ = *p++;
        }
        else {
            *s++ = '+';
            *s++ = '0';
            *s++ = '0';
        }
    }

    *len = s - buf;
    return (buf);
}


/*
 * Convert num to a base X number where X is a power of 2. nbits determines X.
 * For example, if nbits is 3, we do base 8 conversion
 * Return value:
 *      a pointer to a string containing the number
 *
 * The caller provides a buffer for the string: that is the buf_end argument
 * which is a pointer to the END of the buffer + 1 (i.e. if the buffer
 * is declared as buf[ 100 ], buf_end should be &buf[ 100 ])
 *
 * As with conv_10, we have a faster version which is used when
 * the number isn't quad size.
 */
static char *conv_p2(register uint32_t num, register int nbits,
                     char format, char *buf_end, register size_t *len)
{
    register int mask = (1 << nbits) - 1;
    register char *p = buf_end;
    static const char low_digits[] = "0123456789abcdef";
    static const char upper_digits[] = "0123456789ABCDEF";
    register const char *digits = (format == 'X') ? upper_digits : low_digits;

    do {
        *--p = digits[num & mask];
        num >>= nbits;
    }
    while (num);

    *len = buf_end - p;
    return (p);
}

static char *conv_p2_quad(uint64_t num, register int nbits,
                     char format, char *buf_end, register size_t *len)
{
    register int mask = (1 << nbits) - 1;
    register char *p = buf_end;
    static const char low_digits[] = "0123456789abcdef";
    static const char upper_digits[] = "0123456789ABCDEF";
    register const char *digits = (format == 'X') ? upper_digits : low_digits;

    if (num <= UINT32_MAX)
        return(conv_p2((uint32_t)num, nbits, format, buf_end, len));

    do {
        *--p = digits[num & mask];
        num >>= nbits;
    }
    while (num);

    *len = buf_end - p;
    return (p);
}

/*
 * Do format conversion placing the output in buffer
 */
int vformatter(int (*flush_func)(vformatter_buff_t *),
    vformatter_buff_t *vbuff, const char *fmt, va_list ap)
{
    register char *sp;
    register char *bep;
    register int cc = 0;
    register size_t i;

    register char *s = NULL;
    char *q;
    size_t s_len = 0;

    register size_t min_width = 0;
    size_t precision = 0;
    enum {
        LEFT, RIGHT
    } adjust;
    char pad_char;
    char prefix_char;

    double fp_num;
    int64_t i_quad = 0;
    uint64_t ui_quad;
    int32_t i_num = 0;
    uint32_t ui_num = 0;

    char num_buf[NUM_BUF_SIZE];
    char char_buf[2];                /* for printing %% and %<unknown> */
#ifdef TERM_FMT_SPEC
	char term_buf[TERM_BUF_SIZE];
#endif

    enum var_type_enum {
            IS_QUAD, IS_LONG, IS_SHORT, IS_INT
    };
    enum var_type_enum var_type = IS_INT;

    /*
     * Flag variables
     */
    boolean_e alternate_form;
    boolean_e print_sign;
    boolean_e print_blank;
    boolean_e adjust_precision;
    boolean_e adjust_width;
    int is_negative;

    sp = vbuff->curpos;
    bep = vbuff->endpos;

    while (*fmt) {
        if (*fmt != '%') {
            INS_CHAR(*fmt, sp, bep, cc);
        }
        else {
            /*
             * Default variable settings
             */
            boolean_e print_something = YES;
            adjust = RIGHT;
            alternate_form = print_sign = print_blank = NO;
            pad_char = ' ';
            prefix_char = NUL;

            fmt++;

            /*
             * Try to avoid checking for flags, width or precision
             */
            if (!islower(*fmt)) {
                /*
                 * Recognize flags: -, #, BLANK, +
                 */
                for (;; fmt++) {
                    if (*fmt == '-')
                        adjust = LEFT;
                    else if (*fmt == '+')
                        print_sign = YES;
                    else if (*fmt == '#')
                        alternate_form = YES;
                    else if (*fmt == ' ')
                        print_blank = YES;
                    else if (*fmt == '0')
                        pad_char = '0';
                    else
                        break;
                }

                /*
                 * Check if a width was specified
                 */
                if (isdigit(*fmt)) {
                    STR_TO_DEC(fmt, min_width);
                    adjust_width = YES;
                }
                else if (*fmt == '*') {
                    int v = va_arg(ap, int);
                    fmt++;
                    adjust_width = YES;
                    if (v < 0) {
                        adjust = LEFT;
                        min_width = (size_t)(-v);
                    }
                    else
                        min_width = (size_t)v;
                }
                else
                    adjust_width = NO;

                /*
                 * Check if a precision was specified
                 */
                if (*fmt == '.') {
                    adjust_precision = YES;
                    fmt++;
                    if (isdigit(*fmt)) {
                        STR_TO_DEC(fmt, precision);
                    }
                    else if (*fmt == '*') {
                        int v = va_arg(ap, int);
                        fmt++;
                        precision = (v < 0) ? 0 : (size_t)v;
                    }
                    else
                        precision = 0;
                }
                else
                    adjust_precision = NO;
            }
            else
                adjust_precision = adjust_width = NO;

            /*
             * Modifier check.  In same cases, OFF_T_FMT can be
             * "lld" and INT64_T_FMT can be "ld" (that is, off_t is
             * "larger" than int64). Check that case 1st.
             * Note that if OFF_T_FMT is "d",
             * the first if condition is never true. If INT64_T_FMT
             * is "d' then the second if condition is never true.
             */
            if ((sizeof(OFF_T_FMT) > sizeof(INT64_T_FMT)) &&
                ((sizeof(OFF_T_FMT) == 4 &&
                 fmt[0] == OFF_T_FMT[0] &&
                 fmt[1] == OFF_T_FMT[1]) ||
                (sizeof(OFF_T_FMT) == 3 &&
                 fmt[0] == OFF_T_FMT[0]) ||
                (sizeof(OFF_T_FMT) > 4 &&
                 strncmp(fmt, OFF_T_FMT, 
                         sizeof(OFF_T_FMT) - 2) == 0))) {
                /* Need to account for trailing 'd' and null in sizeof() */
                var_type = IS_QUAD;
                fmt += (sizeof(OFF_T_FMT) - 2);
            }
            else if ((sizeof(INT64_T_FMT) == 4 &&
                 fmt[0] == INT64_T_FMT[0] &&
                 fmt[1] == INT64_T_FMT[1]) ||
                (sizeof(INT64_T_FMT) == 3 &&
                 fmt[0] == INT64_T_FMT[0]) ||
                (sizeof(INT64_T_FMT) > 4 &&
                 strncmp(fmt, INT64_T_FMT, 
                         sizeof(INT64_T_FMT) - 2) == 0)) {
                /* Need to account for trailing 'd' and null in sizeof() */
                var_type = IS_QUAD;
                fmt += (sizeof(INT64_T_FMT) - 2);
            }
            else if (*fmt == 'q') {
                var_type = IS_QUAD;
                fmt++;
            }
            else if (*fmt == 'l') {
                var_type = IS_LONG;
                fmt++;
            }
            else if (*fmt == 'h') {
                var_type = IS_SHORT;
                fmt++;
            }
            else {
                var_type = IS_INT;
            }

            /*
             * Argument extraction and printing.
             * First we determine the argument type.
             * Then, we convert the argument to a string.
             * On exit from the switch, s points to the string that
             * must be printed, s_len has the length of the string
             * The precision requirements, if any, are reflected in s_len.
             *
             * NOTE: pad_char may be set to '0' because of the 0 flag.
             *   It is reset to ' ' by non-numeric formats
             */
            switch (*fmt) {
            case 'u':
                if (var_type == IS_QUAD) {
                    i_quad = va_arg(ap, uint64_t);
                    s = conv_10_quad(i_quad, 1, &is_negative,
                            &num_buf[NUM_BUF_SIZE], &s_len);
                }
                else {
                    if (var_type == IS_LONG)
                        i_num = (int32_t) va_arg(ap, uint32_t);
                    else if (var_type == IS_SHORT)
                        i_num = (int32_t) (unsigned short) va_arg(ap, unsigned int);
                    else
                        i_num = (int32_t) va_arg(ap, unsigned int);
                    s = conv_10(i_num, 1, &is_negative,
                            &num_buf[NUM_BUF_SIZE], &s_len);
                }
                FIX_PRECISION(adjust_precision, precision, s, s_len);
                break;

            case 'd':
            case 'i':
                if (var_type == IS_QUAD) {
                    i_quad = va_arg(ap, int64_t);
                    s = conv_10_quad(i_quad, 0, &is_negative,
                            &num_buf[NUM_BUF_SIZE], &s_len);
                }
                else {
                    if (var_type == IS_LONG)
                        i_num = va_arg(ap, int32_t);
                    else if (var_type == IS_SHORT)
                        i_num = (short) va_arg(ap, int);
                    else
                        i_num = va_arg(ap, int);
                    s = conv_10(i_num, 0, &is_negative,
                            &num_buf[NUM_BUF_SIZE], &s_len);
                }
                FIX_PRECISION(adjust_precision, precision, s, s_len);

                if (is_negative)
                    prefix_char = '-';
                else if (print_sign)
                    prefix_char = '+';
                else if (print_blank)
                    prefix_char = ' ';
                break;


            case 'o':
                if (var_type == IS_QUAD) {
                    ui_quad = va_arg(ap, uint64_t);
                    s = conv_p2_quad(ui_quad, 3, *fmt,
                            &num_buf[NUM_BUF_SIZE], &s_len);
                }
                else {
                    if (var_type == IS_LONG)
                        ui_num = va_arg(ap, uint32_t);
                    else if (var_type == IS_SHORT)
                        ui_num = (unsigned short) va_arg(ap, unsigned int);
                    else
                        ui_num = va_arg(ap, unsigned int);
                    s = conv_p2(ui_num, 3, *fmt,
                            &num_buf[NUM_BUF_SIZE], &s_len);
                }
                FIX_PRECISION(adjust_precision, precision, s, s_len);
                if (alternate_form && *s != '0') {
                    *--s = '0';
                    s_len++;
                }
                break;


            case 'x':
            case 'X':
                if (var_type == IS_QUAD) {
                    ui_quad = va_arg(ap, uint64_t);
                    s = conv_p2_quad(ui_quad, 4, *fmt,
                            &num_buf[NUM_BUF_SIZE], &s_len);
                }
                else {
                    if (var_type == IS_LONG)
                        ui_num = va_arg(ap, uint32_t);
                    else if (var_type == IS_SHORT)
                        ui_num = (unsigned short) va_arg(ap, unsigned int);
                    else
                        ui_num = va_arg(ap, unsigned int);
                    s = conv_p2(ui_num, 4, *fmt,
                            &num_buf[NUM_BUF_SIZE], &s_len);
                }
                FIX_PRECISION(adjust_precision, precision, s, s_len);
                if (alternate_form && ui_num != 0) {
                    *--s = *fmt;        /* 'x' or 'X' */
                    *--s = '0';
                    s_len += 2;
                }
                break;


            case 's':
                s = va_arg(ap, char *);
                if (s != NULL) {
                    if (!adjust_precision) {
                        s_len = strlen(s);
                    }
                    else {
                        /* From the C library standard in section 7.9.6.1:
                         * ...if the precision is specified, no more then
                         * that many characters are written.  If the
                         * precision is not specified or is greater
                         * than the size of the array, the array shall
                         * contain a null character.
                         *
                         * My reading is is precision is specified and
                         * is less then or equal to the size of the
                         * array, no null character is required.  So
                         * we can't do a strlen.
                         *
                         * This figures out the length of the string
                         * up to the precision.  Once it's long enough
                         * for the specified precision, we don't care
                         * anymore.
                         *
                         * NOTE: you must do the length comparison
                         * before the check for the null character.
                         * Otherwise, you'll check one beyond the
                         * last valid character.
                         */
                        const char *walk;

                        for (walk = s, s_len = 0;
                             (s_len < precision) && (*walk != '\0');
                             ++walk, ++s_len);
                    }
                }
                else {
                    s = S_NULL;
                    s_len = S_NULL_LEN;
                }
                pad_char = ' ';
                break;


            case 'f':
            case 'e':
            case 'E':
                fp_num = va_arg(ap, double);
                /*
                 * We use &num_buf[ 1 ], so that we have room for the sign
                 */
                s = NULL;
#ifdef HAVE_ISNAN
                if (isnan(fp_num)) {
                    s = "nan";
                    s_len = 3;
                }
#endif
#ifdef HAVE_ISINF
                if (!s && isinf(fp_num)) {
                    s = "inf";
                    s_len = 3;
                }
#endif
                if (!s) {
                    s = conv_fp(*fmt, fp_num, alternate_form,
                                (int)((adjust_precision == NO) ? FLOAT_DIGITS : precision),
                                &is_negative, &num_buf[1], &s_len);
                    if (is_negative)
                        prefix_char = '-';
                    else if (print_sign)
                        prefix_char = '+';
                    else if (print_blank)
                        prefix_char = ' ';
                }
                break;


            case 'g':
            case 'G':
                if (adjust_precision == NO)
                    precision = FLOAT_DIGITS;
                else if (precision == 0)
                    precision = 1;
                /*
                 * * We use &num_buf[ 1 ], so that we have room for the sign
                 */
                s = gcvt(va_arg(ap, double), (int) precision, &num_buf[1],
                            alternate_form);
                if (*s == '-')
                    prefix_char = *s++;
                else if (print_sign)
                    prefix_char = '+';
                else if (print_blank)
                    prefix_char = ' ';

                s_len = strlen(s);

                if (alternate_form && (q = strchr(s, '.')) == NULL) {
                    s[s_len++] = '.';
                    s[s_len] = '\0'; /* delimit for following strchr() */
                }
                if (*fmt == 'G' && (q = strchr(s, 'e')) != NULL)
                    *q = 'E';
                break;


            case 'c':
                char_buf[0] = (char) (va_arg(ap, int));
                s = &char_buf[0];
                s_len = 1;
                pad_char = ' ';
                break;


            case '%':
                char_buf[0] = '%';
                s = &char_buf[0];
                s_len = 1;
                pad_char = ' ';
                break;


            case 'n':
                if (var_type == IS_QUAD)
                    *(va_arg(ap, int64_t *)) = cc;
                else if (var_type == IS_LONG)
                    *(va_arg(ap, long *)) = cc;
                else if (var_type == IS_SHORT)
                    *(va_arg(ap, short *)) = cc;
                else
                    *(va_arg(ap, int *)) = cc;
                print_something = NO;
                break;

                /*
                 * This is where we extend the printf format, with a second
                 * type specifier
                 */
            case 'p':
                switch(*++fmt) {
                /*
                 * If the pointer size is equal to or smaller than the size
                 * of the largest unsigned int, we convert the pointer to a
                 * hex number, otherwise we print "%p" to indicate that we
                 * don't handle "%p".
                 */
                case 'p':
				{
                    ui_num = (unsigned long) va_arg(ap, void *);
                    s = conv_p2(ui_num, 4, 'x',
                           &num_buf[NUM_BUF_SIZE], &s_len);
                    pad_char = ' ';
                    break;
				}
                case 't':
#ifdef TERM_FMT_SPEC
				/*
				 * Print the string representation of the Erlang term.
				 */
				{
					term_t t = va_arg(ap, term_t);
					s_len = term_to_str(t, term_buf, TERM_BUF_SIZE);
					if (adjust_precision && precision < s_len)
						s_len = precision;
					s = term_buf;
					pad_char = ' ';
					break;
				}
#else /* TERM_FRM_SPEC */
                    char_buf[0] = '0';
                    s = &char_buf[0];
                    s_len = 1;
                    pad_char = ' ';
                    break;
#endif
                case NUL:
                    /* if %p ends the string, oh well ignore it */
                    continue;

                default:
                    s = "bogus %p";
                    s_len = 8;
                    prefix_char = NUL;
                    (void)va_arg(ap, void *); /* skip the bogus argument on the stack */
                    break;
                }
                break;

            case NUL:
                /*
                 * The last character of the format string was %.
                 * We ignore it.
                 */
                continue;


                /*
                 * The default case is for unrecognized %'s.
                 * We print %<char> to help the user identify what
                 * option is not understood.
                 * This is also useful in case the user wants to pass
                 * the output of format_converter to another function
                 * that understands some other %<char> (like syslog).
                 * Note that we can't point s inside fmt because the
                 * unknown <char> could be preceded by width etc.
                 */
            default:
                char_buf[0] = '%';
                char_buf[1] = *fmt;
                s = char_buf;
                s_len = 2;
                pad_char = ' ';
                break;
            }

            if (prefix_char != NUL && s != S_NULL && s != char_buf) {
                *--s = prefix_char;
                s_len++;
            }

            if (adjust_width && adjust == RIGHT && min_width > s_len) {
                if (pad_char == '0' && prefix_char != NUL) {
                    INS_CHAR(*s, sp, bep, cc);
                    s++;
                    s_len--;
                    min_width--;
                }
                PAD(min_width, s_len, pad_char);
            }

            /*
             * Print the string s. 
             */
            if (print_something == YES) {
                for (i = s_len; i != 0; i--) {
                      INS_CHAR(*s, sp, bep, cc);
                    s++;
                }
            }

            if (adjust_width && adjust == LEFT && min_width > s_len)
                PAD(min_width, s_len, pad_char);
        }
        fmt++;
    }
    vbuff->curpos = sp;

    return cc;
}


static int snprintf_flush(vformatter_buff_t *vbuff)
{
    /* if the buffer fills we have to abort immediately, there is no way
     * to "flush" an snprintf... there's nowhere to flush it to.
     */
    return -1;
}


int snprintf(char *buf, size_t len, const char *format, ...)
{
    int cc;
    va_list ap;
    vformatter_buff_t vbuff;

    if (len == 0) {
        /* NOTE: This is a special case; we just want to return the number
         * of chars that would be written (minus \0) if the buffer
         * size was infinite. We leverage the fact that INS_CHAR
         * just does actual inserts iff the buffer pointer is non-NULL.
         * In this case, we don't care what buf is; it can be NULL, since
         * we don't touch it at all.
         */
        vbuff.curpos = NULL;
        vbuff.endpos = NULL;
    } else {
        /* save one byte for nul terminator */
        vbuff.curpos = buf;
        vbuff.endpos = buf + len - 1;
    }
    va_start(ap, format);
    cc = vformatter(snprintf_flush, &vbuff, format, ap);
    va_end(ap);
    if (len != 0) {
        *vbuff.curpos = '\0';
    }
    return (cc == -1) ? (int)len - 1 : cc;
}


int vsnprintf(char *buf, size_t len, const char *format,
                               va_list ap)
{
    int cc;
    vformatter_buff_t vbuff;

    if (len == 0) {
        /* See above note */
        vbuff.curpos = NULL;
        vbuff.endpos = NULL;
    } else {
        /* save one byte for nul terminator */
        vbuff.curpos = buf;
        vbuff.endpos = buf + len - 1;
    }
    cc = vformatter(snprintf_flush, &vbuff, format, ap);
    if (len != 0) {
        *vbuff.curpos = '\0';
    }
    return (cc == -1) ? (int)len - 1 : cc;
}

//EOF
