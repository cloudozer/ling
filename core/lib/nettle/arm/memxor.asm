C -*- mode: asm; asm-comment-char: ?C; -*-
C nettle, low-level cryptographics library
C
C Copyright (C) 2013, Niels Möller
C
C The nettle library is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as published by
C the Free Software Foundation; either version 2.1 of the License, or (at your
C option) any later version.
C
C The nettle library is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
C or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
C License for more details.
C
C You should have received a copy of the GNU Lesser General Public License
C along with the nettle library; see the file COPYING.LIB.  If not, write to
C the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
C MA 02111-1301, USA.

C Possible speedups:
C
C The ldm instruction can do load two registers per cycle,
C if the address is two-word aligned. Or three registers in two
C cycles, regardless of alignment.

C Register usage:

define(<DST>, <r0>)
define(<SRC>, <r1>)
define(<N>, <r2>)
define(<CNT>, <r6>)
define(<TNC>, <r12>)

	.syntax unified

	.file "memxor.asm"

	.text
	.arm

	C memxor(uint8_t *dst, const uint8_t *src, size_t n)
	.align 4
PROLOGUE(memxor)
	cmp	N, #0
	beq	.Lmemxor_done

	cmp	N, #7
	bcs	.Lmemxor_large

	C Simple byte loop
.Lmemxor_bytes:
	ldrb	r3, [SRC], #+1
	ldrb	r12, [DST]
	eor	r3, r12
	strb	r3, [DST], #+1
	subs	N, #1
	bne	.Lmemxor_bytes

.Lmemxor_done:
	bx	lr

.Lmemxor_align_loop:
	ldrb	r3, [SRC], #+1
	ldrb	r12, [DST]
	eor	r3, r12
	strb	r3, [DST], #+1
	sub	N, #1

.Lmemxor_large:
	tst	DST, #3
	bne	.Lmemxor_align_loop

	C We have at least 4 bytes left to do here.
	sub	N, #4

	ands	r3, SRC, #3
	beq	.Lmemxor_same

	C Different alignment case.
	C     v original SRC
	C +-------+------+
	C |SRC    |SRC+4 |
	C +---+---+------+
	C     |DST    |
	C     +-------+
	C
	C With little-endian, we need to do
	C DST[i] ^= (SRC[i] >> CNT) ^ (SRC[i+1] << TNC)

	push	{r4,r5,r6}
	
	lsl	CNT, r3, #3
	bic	SRC, #3
	rsb	TNC, CNT, #32

	ldr	r4, [SRC], #+4

	tst	N, #4
	itet	eq
	moveq	r5, r4
	subne	N, #4
	beq	.Lmemxor_odd

.Lmemxor_word_loop:
	ldr	r5, [SRC], #+4
	ldr	r3, [DST]
	eor	r3, r3, r4, lsr CNT
	eor	r3, r3, r5, lsl TNC
	str	r3, [DST], #+4
.Lmemxor_odd:
	ldr	r4, [SRC], #+4
	ldr	r3, [DST]
	eor	r3, r3, r5, lsr CNT
	eor	r3, r3, r4, lsl TNC
	str	r3, [DST], #+4
	subs	N, #8
	bcs	.Lmemxor_word_loop
	adds	N, #8
	beq	.Lmemxor_odd_done

	C We have TNC/8 left-over bytes in r4, high end
	lsr	r4, CNT
	ldr	r3, [DST]
	eor	r3, r4

	pop	{r4,r5,r6}

	C Store bytes, one by one.
.Lmemxor_leftover:
	strb	r3, [DST], #+1
	subs	N, #1
	beq	.Lmemxor_done
	subs	TNC, #8
	lsr	r3, #8
	bne	.Lmemxor_leftover
	b	.Lmemxor_bytes
.Lmemxor_odd_done:
	pop	{r4,r5,r6}
	bx	lr

.Lmemxor_same:
	push	{r4,r5,r6,r7,r8,r10,r11,r14}	C lr is the link register

	subs	N, #8
	bcc	.Lmemxor_same_end

	ldmia	SRC!, {r3, r4, r5}
	C Keep address for loads in r14
	mov	r14, DST
	ldmia	r14!, {r6, r7, r8}
	subs	N, #12
	eor	r10, r3, r6
	eor	r11, r4, r7
	eor	r12, r5, r8
	bcc	.Lmemxor_same_final_store
	subs	N, #12
	ldmia	r14!, {r6, r7, r8}
	bcc	.Lmemxor_same_wind_down

	C 6 cycles per iteration, 0.50 cycles/byte. For this speed,
	C loop starts at offset 0x11c in the object file.

.Lmemxor_same_loop:
	C r10-r12 contains values to be stored at DST
	C r6-r8 contains values read from r14, in advance
	ldmia	SRC!, {r3, r4, r5}
	subs	N, #12
	stmia	DST!, {r10, r11, r12}
	eor	r10, r3, r6
	eor	r11, r4, r7
	eor	r12, r5, r8
	ldmia	r14!, {r6, r7, r8}
	bcs	.Lmemxor_same_loop

.Lmemxor_same_wind_down:
	C Wind down code
	ldmia	SRC!, {r3, r4, r5}
	stmia	DST!, {r10, r11, r12}
	eor	r10, r3, r6
	eor	r11, r4, r7
	eor	r12, r5, r8
.Lmemxor_same_final_store:
	stmia	DST!, {r10, r11, r12}
	
.Lmemxor_same_end:
	C We have 0-11 bytes left to do, and N holds number of bytes -12.
	adds	N, #4
	bcc	.Lmemxor_same_lt_8
	C Do 8 bytes more, leftover is in N
	ldmia	SRC!, {r3, r4}
	ldmia	DST, {r6, r7}
	eor	r3, r6
	eor	r4, r7
	stmia	DST!, {r3, r4}
	pop	{r4,r5,r6,r7,r8,r10,r11,r14}
	beq	.Lmemxor_done
	b	.Lmemxor_bytes

.Lmemxor_same_lt_8:
	pop	{r4,r5,r6,r7,r8,r10,r11,r14}
	adds	N, #4
	bcc	.Lmemxor_same_lt_4

	ldr	r3, [SRC], #+4
	ldr	r12, [DST]
	eor	r3, r12
	str	r3, [DST], #+4
	beq	.Lmemxor_done
	b	.Lmemxor_bytes

.Lmemxor_same_lt_4:
	adds	N, #4
	beq	.Lmemxor_done
	b	.Lmemxor_bytes
	
EPILOGUE(memxor)

define(<DST>, <r0>)
define(<AP>, <r1>)
define(<BP>, <r2>)
define(<N>, <r3>)
undefine(<CNT>)
undefine(<TNC>)

C Temporaries r4-r7
define(<ACNT>, <r8>)
define(<ATNC>, <r10>)
define(<BCNT>, <r11>)
define(<BTNC>, <r12>)

	C memxor3(uint8_t *dst, const uint8_t *a, const uint8_t *b, size_t n)
	.align 2
PROLOGUE(memxor3)
	cmp	N, #0
	beq	.Lmemxor3_ret

	push	{r4,r5,r6,r7,r8,r10,r11}
	cmp	N, #7

	add	AP, N
	add	BP, N
	add	DST, N

	bcs	.Lmemxor3_large

	C Simple byte loop
.Lmemxor3_bytes:
	ldrb	r4, [AP, #-1]!
	ldrb	r5, [BP, #-1]!
	eor	r4, r5
	strb	r4, [DST, #-1]!
	subs	N, #1
	bne	.Lmemxor3_bytes

.Lmemxor3_done:
	pop	{r4,r5,r6,r7,r8,r10,r11}
.Lmemxor3_ret:
	bx	lr

.Lmemxor3_align_loop:
	ldrb	r4, [AP, #-1]!
	ldrb	r5, [BP, #-1]!
	eor	r5, r4
	strb	r5, [DST, #-1]!
	sub	N, #1

.Lmemxor3_large:
	tst	DST, #3
	bne	.Lmemxor3_align_loop

	C We have at least 4 bytes left to do here.
	sub	N, #4
	ands	ACNT, AP, #3
	lsl	ACNT, #3
	beq	.Lmemxor3_a_aligned

	ands	BCNT, BP, #3
	lsl	BCNT, #3
	bne	.Lmemxor3_uu

	C Swap
	mov	r4, AP
	mov	AP, BP
	mov	BP, r4

.Lmemxor3_au:
	C NOTE: We have the relevant shift count in ACNT, not BCNT

	C AP is aligned, BP is not
	C           v original SRC
	C +-------+------+
	C |SRC-4  |SRC   |
	C +---+---+------+
	C     |DST-4  |
	C     +-------+
	C
	C With little-endian, we need to do
	C DST[i-i] ^= (SRC[i-i] >> CNT) ^ (SRC[i] << TNC)
	rsb	ATNC, ACNT, #32
	bic	BP, #3

	ldr	r4, [BP]

	tst	N, #4
	itet	eq
	moveq	r5, r4
	subne	N, #4
	beq	.Lmemxor3_au_odd

.Lmemxor3_au_loop:
	ldr	r5, [BP, #-4]!
	ldr	r6, [AP, #-4]!
	eor	r6, r6, r4, lsl ATNC
	eor	r6, r6, r5, lsr ACNT
	str	r6, [DST, #-4]!
.Lmemxor3_au_odd:
	ldr	r4, [BP, #-4]!
	ldr	r6, [AP, #-4]!
	eor	r6, r6, r5, lsl ATNC
	eor	r6, r6, r4, lsr ACNT
	str	r6, [DST, #-4]!
	subs	N, #8
	bcs	.Lmemxor3_au_loop
	adds	N, #8
	beq	.Lmemxor3_done

	C Leftover bytes in r4, low end
	ldr	r5, [AP, #-4]
	eor	r4, r5, r4, lsl ATNC

.Lmemxor3_au_leftover:
	C Store a byte at a time
	ror	r4, #24
	strb	r4, [DST, #-1]!
	subs	N, #1
	beq	.Lmemxor3_done
	subs	ACNT, #8
	sub	AP, #1
	bne	.Lmemxor3_au_leftover
	b	.Lmemxor3_bytes

.Lmemxor3_a_aligned:
	ands	ACNT, BP, #3
	lsl	ACNT, #3
	bne	.Lmemxor3_au ;

	C a, b and dst all have the same alignment.
	subs	N, #8
	bcc	.Lmemxor3_aligned_word_end

	C This loop runs at 8 cycles per iteration. It has been
	C observed running at only 7 cycles, for this speed, the loop
	C started at offset 0x2ac in the object file.

	C FIXME: consider software pipelining, similarly to the memxor
	C loop.
	
.Lmemxor3_aligned_word_loop:
	ldmdb	AP!, {r4,r5,r6}
	ldmdb	BP!, {r7,r8,r10}
	subs	N, #12
	eor	r4, r7
	eor	r5, r8
	eor	r6, r10
	stmdb	DST!, {r4, r5,r6}
	bcs	.Lmemxor3_aligned_word_loop

.Lmemxor3_aligned_word_end:
	C We have 0-11 bytes left to do, and N holds number of bytes -12.
	adds	N, #4
	bcc	.Lmemxor3_aligned_lt_8
	C Do 8 bytes more, leftover is in N
	ldmdb	AP!, {r4, r5}
	ldmdb	BP!, {r6, r7}
	eor	r4, r6
	eor	r5, r7
	stmdb	DST!, {r4,r5}
	beq	.Lmemxor3_done
	b	.Lmemxor3_bytes

.Lmemxor3_aligned_lt_8:
	adds	N, #4
	bcc	.Lmemxor3_aligned_lt_4

	ldr	r4, [AP,#-4]!
	ldr	r5, [BP,#-4]!
	eor	r4, r5
	str	r4, [DST,#-4]!
	beq	.Lmemxor3_done
	b	.Lmemxor3_bytes

.Lmemxor3_aligned_lt_4:
	adds	N, #4	
	beq	.Lmemxor3_done
	b	.Lmemxor3_bytes

.Lmemxor3_uu:

	cmp	ACNT, BCNT
	bic	AP, #3
	bic	BP, #3
	rsb	ATNC, ACNT, #32

	bne	.Lmemxor3_uud

	C AP and BP are unaligned in the same way

	ldr	r4, [AP]
	ldr	r6, [BP]
	eor	r4, r6

	tst	N, #4
	itet	eq
	moveq	r5, r4
	subne	N, #4
	beq	.Lmemxor3_uu_odd

.Lmemxor3_uu_loop:
	ldr	r5, [AP, #-4]!
	ldr	r6, [BP, #-4]!
	eor	r5, r6
	lsl	r4, ATNC
	eor	r4, r4, r5, lsr ACNT
	str	r4, [DST, #-4]!
.Lmemxor3_uu_odd:
	ldr	r4, [AP, #-4]!
	ldr	r6, [BP, #-4]!
	eor	r4, r6
	lsl	r5, ATNC
	eor	r5, r5, r4, lsr ACNT
	str	r5, [DST, #-4]!
	subs	N, #8
	bcs	.Lmemxor3_uu_loop
	adds	N, #8
	beq	.Lmemxor3_done

	C Leftover bytes in a4, low end
	ror	r4, ACNT
.Lmemxor3_uu_leftover:
	ror	r4, #24
	strb	r4, [DST, #-1]!
	subs	N, #1
	beq	.Lmemxor3_done
	subs	ACNT, #8
	bne	.Lmemxor3_uu_leftover
	b	.Lmemxor3_bytes

.Lmemxor3_uud:
	C Both AP and BP unaligned, and in different ways
	rsb	BTNC, BCNT, #32

	ldr	r4, [AP]
	ldr	r6, [BP]

	tst	N, #4
	ittet	eq
	moveq	r5, r4
	moveq	r7, r6
	subne	N, #4
	beq	.Lmemxor3_uud_odd

.Lmemxor3_uud_loop:
	ldr	r5, [AP, #-4]!
	ldr	r7, [BP, #-4]!
	lsl	r4, ATNC
	eor	r4, r4, r6, lsl BTNC
	eor	r4, r4, r5, lsr ACNT
	eor	r4, r4, r7, lsr BCNT
	str	r4, [DST, #-4]!
.Lmemxor3_uud_odd:
	ldr	r4, [AP, #-4]!
	ldr	r6, [BP, #-4]!
	lsl	r5, ATNC
	eor	r5, r5, r7, lsl BTNC
	eor	r5, r5, r4, lsr ACNT
	eor	r5, r5, r6, lsr BCNT
	str	r5, [DST, #-4]!
	subs	N, #8
	bcs	.Lmemxor3_uud_loop
	adds	N, #8
	beq	.Lmemxor3_done

	C FIXME: More clever left-over handling? For now, just adjust pointers.
	add	AP, AP,	ACNT, lsr #3
	add	BP, BP, BCNT, lsr #3
	b	.Lmemxor3_bytes
EPILOGUE(memxor3)
