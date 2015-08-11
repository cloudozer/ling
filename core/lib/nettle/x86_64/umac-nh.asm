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

define(<KEY>, <%rdi>)
define(<LENGTH>, <%rsi>)
define(<MSG>, <%rdx>)

define(<XA>, <%xmm0>)
define(<XB>, <%xmm1>)
define(<XK0>, <%xmm2>)
define(<XK1>, <%xmm3>)
define(<XY>, <%xmm4>)
define(<XT0>, <%xmm5>)
define(<XT1>, <%xmm6>)

C FIXME: Would be nice if we could force the key array to be 16-byte
C aligned.

	.file "umac-nh.asm"
	
	C umac_nh(const uint32_t *key, unsigned length, const uint8_t *msg)
	.text
	ALIGN(16)
PROLOGUE(_nettle_umac_nh)
	W64_ENTRY(3, 7)
	pxor	XY, XY
.Loop:
	movups	(KEY), XK0
	movups	16(KEY), XK1
	movups	(MSG), XA
	movups	16(MSG), XB
	paddd	XK0, XA
	paddd	XK1, XB
	pshufd	$0x31, XA, XT0
	pshufd	$0x31, XB, XT1
	pmuludq	XT0, XT1
	paddq	XT1, XY	
	pmuludq	XA, XB
	paddq	XB, XY
	C Length is only 32 bits
	subl	$32, XREG(LENGTH)
	lea	32(KEY), KEY
	lea	32(MSG), MSG
	ja	.Loop

	pshufd	$0xe, XY, XT0
	paddq	XT0, XY
	C Really a movq, but write as movd to please Apple's assembler
	movd	XY, %rax
	W64_EXIT(3, 7)
	ret
EPILOGUE(_nettle_umac_nh)
