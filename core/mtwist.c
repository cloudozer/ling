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

/*
 * C library functions for generating pseudorandom numbers using the
 * Mersenne Twist algorithm.  See M. Matsumoto and T. Nishimura,
 * "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
 * Pseudo-Random Number Generator", ACM Transactions on Modeling and
 * Computer Simulation, Vol. 8, No. 1, January 1998, pp 3--30.
 *
 * The Web page on the Mersenne Twist algorithm is at:
 *
 * http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
 *
 * These functions were written by Geoff Kuenning, Claremont, CA.
 *
 * IMPORTANT NOTE: this implementation assumes a modern compiler.  In
 * particular, it assumes that the "inline" keyword is available, and
 * that the "inttypes.h" header file is present.
 *
 * IMPORTANT NOTE: this software requires access to a 32-bit type.
 * The Mersenne Twist algorithms are not guaranteed to produce correct
 * results with a 64-bit type.
 *
 * This software is based on LGPL-ed code by Takuji Nishimura.  It has
 * also been heavily influenced by code written by Shawn Cokus, and
 * somewhat influenced by code written by Richard J. Wagner.  It is
 * therefore also distributed under the LGPL:
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.  You should have
 * received a copy of the GNU Library General Public License along
 * with this library; if not, write to the Free Foundation, Inc., 59
 * Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 * $Log: mtwist.c,v $
 * Revision 1.23  2010-12-11 00:28:18+13  geoff
 * Add support for GENERATE_CODE_IN_HEADER.  Fix the URL for the original
 * Web page.
 *
 * Revision 1.22  2010-06-24 20:53:59+12  geoff
 * Switch to using types and formats from inttypes.h.  Get rid of all
 * compilation options.
 *
 * Revision 1.21  2010-06-24 00:29:38-07  geoff
 * Correctly save and restore the state vector even if longs are wider
 * than 32 bits.
 *
 * Revision 1.20  2007-10-26 00:21:06-07  geoff
 * Use the new mt_u32bit_t type.
 *
 * Revision 1.19  2003/09/11 05:55:19  geoff
 * Get rid of some minor compiler warnings.
 *
 * Revision 1.18  2003/09/11 05:50:53  geoff
 * Don't #define inline to nothing, since that breaks standard include
 * files.  Instead, use MT_INLINE as a synonym.
 *
 * Revision 1.17  2002/10/31 22:07:10  geoff
 * Make WIN32 detection work with GCC as well as MS C
 *
 * Revision 1.16  2002/10/31 22:04:59  geoff
 * Fix a typo in the WIN32 option
 *
 * Revision 1.15  2002/10/31 06:01:43  geoff
 * Incorporate Joseph Brill's Windows-portability changes
 *
 * Revision 1.14  2002/10/30 07:39:53  geoff
 * Reintroduce the old seeding functions (so that old code will still
 * produce the same results), and give the new versions new names.
 *
 * Revision 1.13  2002/10/30 01:08:26  geoff
 * Switch to M&T's new initialization method
 *
 * Revision 1.12  2001/06/18 05:40:12  geoff
 * Prefix the compile options with MT_.
 *
 * Revision 1.11  2001/06/14 10:26:59  geoff
 * Invert the sense of the #define flags so that the default is the
 * normal case (if gcc is normal!).  Also default MT_MACHINE_BITS to 32.
 *
 * Revision 1.10  2001/06/14 10:10:38  geoff
 * Move the RNG functions into the header file so they can be inlined.
 * Add saving/loading of state.  Add a function that marks the PRNG as
 * initialized while also calculating critical constants.  Run the
 * refresh routine whenever seed32 is called.  Add functions to seed
 * based on /dev/random or the time.
 *
 * Revision 1.9  2001/06/11 10:00:04  geoff
 * Major changes to improve flexibility and performance, and to prepare
 * for inlining.  This code is about as fast as it can get without
 * inlining the various PRNG functions.  Add seed/goodseed/bestseed for
 * seeding from random start values.  Add the refresh routine a la Cokus,
 * but optimize it by unrolling loops.  Change getstate to return a
 * complete state pointer, since knowing the position in the state vector
 * is critical to restoring state.  Add more macros to improve
 * readability.  Rename certain macros in preparation for inlining.  Get
 * rid of leftover optimizer-bug stuff.  Stop using mtwist_guts.h;
 * instead use direct code (via macros) and the refresh function.
 *
 * Revision 1.8  2001/04/23 08:36:03  geoff
 * Move the #defined code into a header file to ease stepping with a debugger.
 *
 * Revision 1.7  2001/04/23 08:00:13  geoff
 * Add code to work around optimizer bug
 *
 * Revision 1.6  2001/04/14 01:33:32  geoff
 * Clarify the license
 *
 * Revision 1.5  2001/04/09 08:45:00  geoff
 * Rename default_state to mt_default_state, and make it global so that
 * the random-distribution code can use it.
 *
 * Revision 1.4  2001/04/07 23:24:11  geoff
 * My guess in the commentary for the last delta was right: it's faster
 * on a x86 to convert the two halves of the PRN to double, multiplying
 * them by the appropriate value to scale them, and then add them as
 * doubles.  I suspect the reason is that there is no instruction to
 * convert a 64-bit value directly to a double, so the work of building
 * the long long (which isn't easy anyway, without assembly access) is
 * worse than wasted.  So add support for MT_MACHINE_BITS, and only go
 * the via-long-long route on a true 64-bit machine.
 *
 * Revision 1.3  2001/04/07 23:09:38  geoff
 * Get rid of MT_INLINE.  Convert all of the code to use preprocessor
 * macros for the guts of the PRNG code.  Take advantage of the
 * conversion to get rid of unnecessary calls initialization tests.  Also
 * clean up the generation of long-double pseudorandom numbers on
 * machines that have the long long type (by converting first to a long
 * long, then to a double, saving one floating-point operation).  The
 * latter change might be a mistake on 32-bit machines.  The code is now
 * much faster as a result of macro-izing.
 *
 * Revision 1.2  2001/04/07 22:21:41  geoff
 * Make the long-double code a hair faster by always having a 64-bit
 * conversion constant.  Add commentary to the PRNG loop.
 *
 * Revision 1.1  2001/04/07 09:43:41  geoff
 * Initial revision
 *
 */

#include "mtwist.h"

/*
 * The following values are fundamental parameters of the algorithm.
 * With the exception of the two masks, all of them were found
 * experimentally using methods described in Matsumoto and Nishimura's
 * paper.  They are exceedingly magic; don't change them.
 */

#define MT_STATE_SIZE	624		/* Size of the MT state vector */

#define RECURRENCE_OFFSET 397		/* Offset into state space for the */
					/* ..recurrence relation.  The */
					/* ..recurrence mashes together two */
					/* ..values that are separated by */
					/* ..this offset in the state */
					/* ..space. */
#define MATRIX_A	0x9908b0df	/* Constant vector A for the */
					/* ..recurrence relation.  The */
					/* ..mashed-together value is */
					/* ..multiplied by this vector to */
					/* ..get a new value that will be */
					/* ..stored into the state space. */

/*
 * Width of an unsigned int.  Don't change this even if your ints are 64 bits.
 */
#define BIT_WIDTH	32		/* Work with 32-bit words */

/*
 * Masks for extracting the bits to be mashed together.  The widths of these
 * masks are also fundamental parameters of the algorithm, determined
 * experimentally -- but of course the masks themselves are simply bit
 * selectors.
 */
#define UPPER_MASK	0x80000000	/* Most significant w-r bits */
#define LOWER_MASK	0x7fffffff	/* Least significant r bits */

/*
 * Macro to simplify code in the generation loop.  This function
 * combines the top bit of x with the bottom 31 bits of y.
 */
#define COMBINE_BITS(x, y) \
			(((x) & UPPER_MASK) | ((y) & LOWER_MASK))

/*
 * Another generation-simplification macro.  This one does the magic
 * scrambling function.
 */
#define MATRIX_MULTIPLY(original, new) \
			((original) ^ ((new) >> 1) \
			  ^ matrix_decider[(new) & 0x1])

/*
 * Parameters of Knuth's PRNG (Line 25, Table 1, p. 102 of "The Art of
 * Computer Programming, Vol. 2, 2nd ed, 1981).
 */
#define KNUTH_MULTIPLIER_OLD \
			69069

/*
 * Parameters of Knuth's PRNG (p. 106 of "The Art of Computer
 * Programming, Vol. 2, 3rd ed).
 */
#define KNUTH_MULTIPLIER_NEW \
			1812433253ul
#define KNUTH_SHIFT	30		// Even on a 64-bit machine!

typedef struct
    {
    uint32_t		statevec[MT_STATE_SIZE];
					/* Vector holding current state */
    int			stateptr;	/* Next state entry to be used */
    int			initialized;	/* NZ if state was initialized */
    }
			mt_state;

/*
 * Many applications need only a single PRNG, so it's a nuisance to have to
 * specify a state.  For those applications, we will provide a default
 * state, and functions to use it.
 */
mt_state		mt_default_state;

/*
 * In the recurrence relation, the new value is XORed with MATRIX_A only if
 * the lower bit is nonzero.  Since most modern machines don't like to
 * branch, it's vastly faster to handle this decision by indexing into an
 * array.  The chosen bit is used as an index into the following vector,
 * which produces either zero or MATRIX_A and thus the desired effect.
 */
static uint32_t	matrix_decider[2] =
			  {0x0, MATRIX_A};

/*
 * Generate 624 more random values.  This function is called when the
 * state vector has been exhausted.  It generates another batch of
 * pseudo-random values.  The performance of this function is critical
 * to the performance of the Mersenne Twist PRNG, so it has been
 * highly optimized.
 */
static void mts_refresh(
    register mt_state*	state)		/* State for the PRNG */
    {
    register int	i;		/* Index into the state */
    register uint32_t*
			state_ptr;	/* Next place to get from state */
    register uint32_t
			value1;		/* Scratch val picked up from state */
    register uint32_t
			value2;		/* Scratch val picked up from state */

    /*
     * Now generate the new pseudorandom values by applying the
     * recurrence relation.  We use two loops and a final
     * 2-statement sequence so that we can handle the wraparound
     * explicitly, rather than having to use the relatively slow
     * modulus operator.
     *
     * In essence, the recurrence relation concatenates bits
     * chosen from the current random value (last time around)
     * with the immediately preceding one.  Then it
     * matrix-multiplies the concatenated bits with a value
     * RECURRENCE_OFFSET away and a constant matrix.  The matrix
     * multiplication reduces to a shift and two XORs.
     *
     * Some comments on the optimizations are in order:
     *
     * Strictly speaking, none of the optimizations should be
     * necessary.  All could conceivably be done by a really good
     * compiler.  However, the compilers available to me aren't quite
     * smart enough, so hand optimization needs to be done.
     *
     * Shawn Cokus was the first to achieve a major speedup.  In the
     * original code, the first value given to COMBINE_BITS (in my
     * characterization) was re-fetched from the state array, rather
     * than being carried in a scratch variable.  Cokus noticed that
     * the first argument to COMBINE_BITS could be saved in a register
     * in the previous loop iteration, getting rid of the need for an
     * expensive memory reference.
     *
     * Cokus also switched to using pointers to access the state
     * array and broke the original loop into two so that he could
     * avoid using the expensive modulus operator.  Cokus used three
     * pointers; Richard J. Wagner noticed that the offsets between
     * the three were constant, so that they could be collapsed into a
     * single pointer and constant-offset accesses.  This is clearly
     * faster on x86 architectures, and is the same cost on RISC
     * machines.  A secondary benefit is that Cokus' version was
     * register-starved on the x86, while Wagner's version was not.
     *
     * I made several smaller improvements to these observations.
     * First, I reversed the contents of the state vector.  In the
     * current version of the code, this change doesn't directly
     * affect the performance of the refresh loop, but it has the nice
     * side benefit that an all-zero state structure represents an
     * uninitialized generator.  It also slightly speeds up the
     * random-number routines, since they can compare the state
     * pointer against zero instead of against a constant (this makes
     * the biggest difference on RISC machines).
     *
     * Second, I returned to Matsumoto and Nishimura's original
     * technique of using a lookup table to decide whether to xor the
     * constant vector A (MATRIX_A in this code) with the newly
     * computed value.  Cokus and Wagner had used the ?: operator,
     * which requires a test and branch.  Modern machines don't like
     * branches, so the table lookup is faster.
     *
     * Third, in the Cokus and Wagner versions the loop ends with a
     * statement similar to "value1 = value2", which is necessary to
     * carry the fetched value into the next loop iteration.  I
     * recognized that if the loop were unrolled so that it generates
     * two values per iteration, a bit of variable renaming would get
     * rid of that assignment.  A nice side effect is that the
     * overhead of loop control becomes only half as large.
     *
     * It is possible to improve the code's performance somewhat
     * further.  In particular, since the second loop's loop count
     * factors into 2*2*3*3*11, it could be unrolled yet further.
     * That's easy to do, too: just change the "/ 2" into a division
     * by whatever factor you choose, and then use cut-and-paste to
     * duplicate the code in the body.  To remove a few more cycles,
     * fix the code to decrement state_ptr by the unrolling factor, and
     * adjust the various offsets appropriately.  However, the payoff
     * will be small.  At the moment, the x86 version of the loop is
     * 25 instructions, of which 3 are involved in loop control
     * (including the decrementing of state_ptr).  Further unrolling by
     * a factor of 2 would thus produce only about a 6% speedup.
     *
     * The logical extension of the unrolling
     * approach would be to remove the loops and create 624
     * appropriate copies of the body.  However, I think that doing
     * the latter is a bit excessive!
     *
     * I suspect that a superior optimization would be to simplify the
     * mathematical operations involved in the recurrence relation.
     * However, I have no idea whether such a simplification is
     * feasible.
     */
    state_ptr = &state->statevec[MT_STATE_SIZE - 1];
    value1 = *state_ptr;
    for (i = (MT_STATE_SIZE - RECURRENCE_OFFSET) / 2;  --i >= 0;  )
	{
	state_ptr -= 2;
	value2 = state_ptr[1];
	value1 = COMBINE_BITS(value1, value2);
	state_ptr[2] =
	  MATRIX_MULTIPLY(state_ptr[-RECURRENCE_OFFSET + 2], value1);
	value1 = state_ptr[0];
	value2 = COMBINE_BITS(value2, value1);
	state_ptr[1] =
	  MATRIX_MULTIPLY(state_ptr[-RECURRENCE_OFFSET + 1], value2);
	}
    value2 = *--state_ptr;
    value1 = COMBINE_BITS(value1, value2);
    state_ptr[1] =
      MATRIX_MULTIPLY(state_ptr[-RECURRENCE_OFFSET + 1], value1);

    for (i = (RECURRENCE_OFFSET - 1) / 2;  --i >= 0;  )
	{
	state_ptr -= 2;
	value1 = state_ptr[1];
	value2 = COMBINE_BITS(value2, value1);
	state_ptr[2] =
	  MATRIX_MULTIPLY(state_ptr[MT_STATE_SIZE - RECURRENCE_OFFSET + 2],
	    value2);
	value2 = state_ptr[0];
	value1 = COMBINE_BITS(value1, value2);
	state_ptr[1] =
	  MATRIX_MULTIPLY(state_ptr[MT_STATE_SIZE - RECURRENCE_OFFSET + 1],
	    value1);
	}

    /*
     * The final entry in the table requires the "previous" value
     * to be gotten from the other end of the state vector, so it
     * must be handled specially.
     */
    value1 = COMBINE_BITS(value2, state->statevec[MT_STATE_SIZE - 1]);
    *state_ptr =
      MATRIX_MULTIPLY(state_ptr[MT_STATE_SIZE - RECURRENCE_OFFSET], value1);

    /*
     * Now that refresh is complete, reset the state pointer to allow more
     * pseudorandom values to be fetched from the state array.
     */
    state->stateptr = MT_STATE_SIZE;
    }

/*
 * Initialize a Mersenne Twist PRNG from a 32-bit seed, using
 * Matsumoto and Nishimura's newer reference implementation (Jan. 9,
 * 2002).
 */
static void mts_seed(
    mt_state*		state,		/* State vector to initialize */
    uint32_t		seed)		/* 32-bit seed to start from */
    {
    int			i;		/* Loop index */
    uint32_t		nextval;	/* Next value being calculated */

    /*
     * Fill the state vector using Knuth's PRNG.  Be sure to mask down
     * to 32 bits in case we're running on a machine with 64-bit
     * ints.
     */
    state->statevec[MT_STATE_SIZE - 1] = seed & 0xffffffffUL;
    for (i = MT_STATE_SIZE - 2;  i >= 0;  i--)
	{
	nextval = state->statevec[i + 1] >> KNUTH_SHIFT;
	nextval ^= state->statevec[i + 1];
	nextval *= KNUTH_MULTIPLIER_NEW;
	nextval += (MT_STATE_SIZE - 1) - i;
	state->statevec[i] = nextval & 0xffffffffUL;
	}

    state->stateptr = MT_STATE_SIZE;

    /*
     * Matsumoto and Nishimura's implementation refreshes the PRNG
     * immediately after running the Knuth algorithm.  This is
     * probably a good thing, since Knuth's PRNG doesn't generate very
     * good numbers.
     */
    mts_refresh(state);
    }

void mt_seed(
    uint32_t		seed)		/* 32-bit seed to start from */
    {
    mts_seed(&mt_default_state, seed);
    }

/*
 * Tempering parameters.  These are perhaps the most magic of all the magic
 * values in the algorithm.  The values are again experimentally determined.
 * The values generated by the recurrence relation (constants above) are not
 * equidistributed in 623-space.  For some reason, the tempering process
 * produces that effect.  Don't ask me why.  Read the paper if you can
 * understand the math.  Or just trust these magic numbers.
 */
#define MT_TEMPERING_MASK_B 0x9d2c5680
#define MT_TEMPERING_MASK_C 0xefc60000
#define MT_TEMPERING_SHIFT_U(y) \
			(y >> 11)
#define MT_TEMPERING_SHIFT_S(y) \
			(y << 7)
#define MT_TEMPERING_SHIFT_T(y) \
			(y << 15)
#define MT_TEMPERING_SHIFT_L(y) \
			(y >> 18)

/*
 * Macros to do the tempering.  MT_PRE_TEMPER does all but the last step;
 * it's useful for situations where the final step can be incorporated
 * into a return statement.  MT_FINAL_TEMPER does that final step (not as
 * an assignment).  MT_TEMPER does the entire process.  Note that
 * MT_PRE_TEMPER and MT_TEMPER both modify their arguments.
 */
#define MT_PRE_TEMPER(value)						\
    do									\
	{								\
	value ^= MT_TEMPERING_SHIFT_U(value);				\
	value ^= MT_TEMPERING_SHIFT_S(value) & MT_TEMPERING_MASK_B;	\
	value ^= MT_TEMPERING_SHIFT_T(value) & MT_TEMPERING_MASK_C;	\
	}								\
	while (0)
#define MT_FINAL_TEMPER(value) \
			((value) ^ MT_TEMPERING_SHIFT_L(value))

uint32_t mt_lrand()
    {
    register uint32_t	random_value;	/* Pseudorandom value generated */

    if (mt_default_state.stateptr <= 0)
	mts_refresh(&mt_default_state);

    random_value = mt_default_state.statevec[--mt_default_state.stateptr];
    MT_PRE_TEMPER(random_value);

    return MT_FINAL_TEMPER(random_value);
    }

//EOF
