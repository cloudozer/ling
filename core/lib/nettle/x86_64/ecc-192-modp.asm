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

	.file "ecc-192-modp.asm"

define(<RP>, <%rsi>)
define(<T0>, <%rdi>) C Overlaps unused ecc input
define(<T1>, <%rcx>)
define(<T2>, <%rdx>)
define(<T3>, <%r8>)
define(<H>, <%r9>)
define(<C1>, <%r10>)
define(<C2>, <%r11>)

	C ecc_192_modp (const struct ecc_curve *ecc, mp_limb_t *rp)
	.text
	ALIGN(16)
PROLOGUE(nettle_ecc_192_modp)
	W64_ENTRY(2, 0)
	mov	16(RP), T2
	mov	24(RP), T3
	mov	40(RP), H
	xor	C1, C1
	xor	C2, C2

	add	H, T2
	adc	H, T3
	C Carry to be added in at T1 and T2
	setc	LREG(C2)
	
	mov	8(RP), T1
	mov	32(RP), H
	adc	H, T1
	adc	H, T2
	C Carry to be added in at T0 and T1
	setc	LREG(C1)
	
	mov	(RP), T0
	adc	T3, T0
	adc	T3, T1
	adc	$0, C2

	C Add in C1 and C2
	add	C1, T1
	adc	C2, T2
	setc	LREG(C1)

	C Fold final carry.
	adc	$0, T0
	adc	C1, T1
	adc	$0, T2

	mov	T0, (RP)
	mov	T1, 8(RP)
	mov	T2, 16(RP)

	W64_EXIT(2, 0)
	ret
EPILOGUE(nettle_ecc_192_modp)
