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

	.file "ecc-224-modp.asm"

define(<RP>, <%rsi>)
define(<T0>, <%rdi>) C Overlaps unused ecc input
define(<T1>, <%rcx>)
define(<H0>, <%rax>)
define(<H1>, <%rdx>)
define(<H2>, <%r8>)
define(<F0>, <%r9>)
define(<F1>, <%r10>)
define(<F2>, <%r11>)

PROLOGUE(nettle_ecc_224_modp)
	W64_ENTRY(2, 0)
	mov	48(RP), H0
	mov	56(RP), H1
	C Set (F2,F1,F0)  <--  (H1,H0) << 32
	mov	H0, F0
	mov	H0, F1
	shl	$32, F0
	shr	$32, F1
	mov	H1, F2
	mov	H1, T0
	shl	$32, T0
	shr	$32, F2
	or	T0, F1

	xor	H2, H2
	mov	16(RP), T0
	mov	24(RP), T1
	sub	F0, T0
	sbb	F1, T1
	sbb	F2, H0
	sbb	$0, H1		C No further borrow

	adc	32(RP), H0
	adc	40(RP), H1
	adc	$0, H2

	C Set (F2,F1,F0)  <--  (H2,H1,H0) << 32
	C To free registers, add in T1, T0 as soon as H0, H1 have been copied
	mov	H0, F0
	mov	H0, F1
	add	T0, H0
	mov	H1, F2
	mov	H1, T0
	adc	T1, H1
	mov	H2, T1
	adc	$0, H2

	C Shift 32 bits
	shl	$32, F0
	shr	$32, F1
	shl	$32, T0
	shr	$32, F2
	shl	$32, T1
	or	T0, F1
	or	T1, F2

	mov	(RP), T0
	mov	8(RP), T1
	sub	F0, T0
	sbb	F1, T1
	sbb	F2, H0
	sbb	$0, H1
	sbb	$0, H2

	C We now have H2, H1, H0, T1, T0, with 33 bits left to reduce
	C Set F0       <-- (H2, H1) >> 32
	C Set (F2,F1)  <-- (H2, H1 & 0xffffffff00000000)
	C H1  <--  H1 & 0xffffffff

	mov	H1, F0
	mov	H1, F1
	mov	H2, F2
	movl	XREG(H1), XREG(H1)	C Clears high 32 bits
	sub	H1, F1			C Clears low 32 bits
	shr	$32, F0
	shl	$32, H2
	or	H2, F0

	sub	F0, T0
	sbb	$0, F1
	sbb	$0, F2
	add	F1, T1
	adc	F2, H0
	adc	$0, H1

	mov	T0, (RP)
	mov	T1, 8(RP)
	mov	H0, 16(RP)
	mov	H1, 24(RP)

	W64_EXIT(2, 0)
	ret
EPILOGUE(nettle_ecc_224_modp)
