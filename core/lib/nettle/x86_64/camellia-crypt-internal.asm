C nettle, low-level cryptographics library
C 
C Copyright (C) 2010, Niels Möller
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

C Performance, cycles per block
C
C	        Intel SU4100
C                 C  asm
C Camellia-128  415  347
C Camellia-256  543  461

C Register usage:

define(<CTX>, <%rdi>)
define(<TABLE>, <%rsi>)
define(<LENGTH>, <%rdx>)
define(<DST>, <%rcx>)
define(<SRC>, <%r8>)

C Camellia state
define(<I0>, <%rax>)
define(<I1>, <%rbx>) C callee-save
define(<KEY>, <%r9>)
define(<TMP>, <%rbp>) C callee-save
define(<CNT>, <%r10>)
define(<IL>,  <%r11>)
define(<IR>,  <%r12>) C callee-save

define(<SP1110>, <(TABLE,$1,4)>)
define(<SP0222>, <1024(TABLE,$1,4)>)
define(<SP3033>, <2048(TABLE,$1,4)>)
define(<SP4404>, <3072(TABLE,$1,4)>)

C ROUND(x, y, key-offset)
define(<ROUND>, <
	C Byte 0,1
	movzbl	LREG($1), XREG(TMP)
	movl	SP1110(TMP), XREG(IR)
	movzbl	HREG($1), XREG(TMP)
	xorl	SP4404(TMP), XREG(IR)
	ror	<$>32, $1

	C Byte 4,5
	movzbl	LREG($1), XREG(TMP)
	movl	SP4404(TMP), XREG(IL)
	movzbl	HREG($1), XREG(TMP)
	xorl	SP3033(TMP), XREG(IL)
	rol	<$>16, $1

	C Byte 2,3
	movzbl	LREG($1), XREG(TMP)
	xorl	SP3033(TMP), XREG(IR)
	movzbl	HREG($1), XREG(TMP)
	xorl	SP0222(TMP), XREG(IR)
	ror	<$>32, $1

	C Byte 6,7
	movzbl	LREG($1), XREG(TMP)
	xorl	SP0222(TMP), XREG(IL)
	movzbl	HREG($1), XREG(TMP)
	xorl	SP1110(TMP), XREG(IL)
	ror	<$>16, $1

	C 76543210
	
	xorl	XREG(IL), XREG(IR)
	rorl	<$>8, XREG(IL)
	xorl	XREG(IR), XREG(IL)
	shl	<$>32, IR
	or	IL, IR
	xor	$3(KEY), $2
	xor	IR, $2
>)

C FL(x, key-offset)
define(<FL>, <
	mov	$1, TMP
	shr	<$>32, TMP
	andl	$2 + 4(KEY), XREG(TMP)
	roll	<$>1, XREG(TMP)
C 	xorl	XREG(TMP), XREG($1)
	xor	TMP, $1
	movl	$2(KEY), XREG(TMP)
	orl	XREG($1), XREG(TMP)
	shl	<$>32, TMP
	xor	TMP, $1
>)
C FLINV(x0, key-offset)
define(<FLINV>, <
	movl	$2(KEY), XREG(TMP)
	orl	XREG($1), XREG(TMP)
	shl	<$>32, TMP
	xor	TMP, $1
	mov	$1, TMP
	shr	<$>32, TMP
	andl	$2 + 4(KEY), XREG(TMP)
	roll	<$>1, XREG(TMP)
C	xorl	XREG(TMP), XREG($1)
	xor	TMP, $1	
>)

	.file "camellia-encrypt-internal.asm"
	
	C _camellia_crypt(struct camellia_context *ctx, 
	C	          const struct camellia_table *T,
	C	          unsigned length, uint8_t *dst,
	C	          uint8_t *src)
	.text
	ALIGN(16)
PROLOGUE(_nettle_camellia_crypt)

	W64_ENTRY(5, 0)
	test	LENGTH, LENGTH
	jz	.Lend

	push	%rbx
	push	%rbp
	push	%r12
	
.Lblock_loop:
	C Load data, note that we'll happily do unaligned loads
	mov	(SRC), I0
	bswap	I0
	mov	8(SRC), I1
	bswap	I1
	add	$16, SRC
	mov	CTX, KEY
	movl	(KEY), XREG(CNT)
	sub	$8, CNT

	C 	Whitening using first subkey 
	xor	8(KEY), I0
	add	$16, KEY

	ROUND(I0, I1, 0)
	ROUND(I1, I0, 8)
	ROUND(I0, I1, 16)
	ROUND(I1, I0, 24)
	ROUND(I0, I1, 32) 
	ROUND(I1, I0, 40)
	
.Lround_loop:
	add	$64, KEY
	FL(I0, -16)
	FLINV(I1, -8)
	ROUND(I0, I1, 0)
	ROUND(I1, I0, 8)
	ROUND(I0, I1, 16)
	ROUND(I1, I0, 24)
	ROUND(I0, I1, 32) 
	ROUND(I1, I0, 40)

	sub 	$8, CNT	
	ja	.Lround_loop

	bswap	I0
	mov	I0, 8(DST)
	xor	48(KEY), I1
	bswap	I1
	mov	I1, (DST)
	add	$16, DST
	sub	$16, LENGTH

	ja	.Lblock_loop

	pop	%r12
	pop	%rbp
	pop	%rbx
.Lend:
	W64_EXIT(5, 0)
	ret
EPILOGUE(_nettle_camellia_crypt)
