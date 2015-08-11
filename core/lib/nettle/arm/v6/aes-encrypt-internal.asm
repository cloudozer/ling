C nettle, low-level cryptographics library
C 
C Copyright (C) 2013 Niels Möller
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

include_src(<arm/aes.m4>)

C	Benchmarked at at 693, 824, 950 cycles/block on cortex A9,
C	for 128, 192 and 256 bit key sizes.

C	Possible improvements: More efficient load and store with
C	aligned accesses. Better scheduling.

define(<CTX>, <r0>)
define(<TABLE>, <r1>)
define(<LENGTH>, <r2>)
define(<DST>, <r3>)
define(<SRC>, <r12>)

define(<W0>, <r4>)
define(<W1>, <r5>)
define(<W2>, <r6>)
define(<W3>, <r7>)
define(<T0>, <r8>)
define(<KEY>, <r10>)
define(<ROUND>, <r11>)

define(<X0>, <r2>)	C Overlaps LENGTH, SRC, DST
define(<X1>, <r3>)
define(<X2>, <r12>)
define(<X3>, <r14>)	C lr


	.file "aes-encrypt-internal.asm"
	
	C _aes_encrypt(struct aes_context *ctx, 
	C	       const struct aes_table *T,
	C	       unsigned length, uint8_t *dst,
	C	       uint8_t *src)
	.text
	.align 2
PROLOGUE(_nettle_aes_encrypt)
	teq	LENGTH, #0
	beq	.Lend
	ldr	SRC, [sp]

	push	{r4,r5,r6,r7,r8,r10,r11,lr}
.Lblock_loop:
	mov	KEY, CTX
	AES_LOAD(SRC,KEY,W0)
	AES_LOAD(SRC,KEY,W1)
	AES_LOAD(SRC,KEY,W2)
	AES_LOAD(SRC,KEY,W3)

	push	{LENGTH, DST, SRC}
	ldr	ROUND, [CTX, #+AES_NROUNDS]
	add	TABLE, TABLE, #AES_TABLE0

	b	.Lentry
	.align 2
.Lround_loop:
	C	Transform X -> W
	AES_ENCRYPT_ROUND(X0, X1, X2, X3, W0, W1, W2, W3, KEY)
	
.Lentry:
	subs	ROUND, ROUND,#2
	C	Transform W -> X
	AES_ENCRYPT_ROUND(W0, W1, W2, W3, X0, X1, X2, X3, KEY)

	bne	.Lround_loop

	sub	TABLE, TABLE, #AES_TABLE0
	C	Final round
	AES_FINAL_ROUND(X0, X1, X2, X3, KEY, W0)
	AES_FINAL_ROUND(X1, X2, X3, X0, KEY, W1)
	AES_FINAL_ROUND(X2, X3, X0, X1, KEY, W2)
	AES_FINAL_ROUND(X3, X0, X1, X2, KEY, W3)

	pop	{LENGTH, DST, SRC}
	
	AES_STORE(DST,W0)
	AES_STORE(DST,W1)
	AES_STORE(DST,W2)
	AES_STORE(DST,W3)

	subs	LENGTH, LENGTH, #16
	bhi	.Lblock_loop

	pop	{r4,r5,r6,r7,r8,r10,r11,pc}
	
.Lend:
	bx	lr
EPILOGUE(_nettle_aes_encrypt)
