C nettle, low-level cryptographics library
C 
C Copyright (C) 2004, 2008 Niels Möller
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

C Register usage. KVALUE and INPUT share a register.
define(<SA>,<%eax>)dnl
define(<SB>,<%r8d>)dnl
define(<SC>,<%ecx>)dnl
define(<SD>,<%edx>)dnl
define(<SE>,<%r9d>)dnl
define(<DATA>,<%rsp>)dnl
define(<TMP>,<%r10d>)dnl
define(<TMP2>,<%r11d>)dnl			C  Used by F3
define(<KVALUE>, <%esi>)dnl			

C Arguments
define(<STATE>,<%rdi>)dnl
define(<INPUT>,<%rsi>)dnl

C Constants
define(<K1VALUE>, <<$>0x5A827999>)dnl		C  Rounds  0-19
define(<K2VALUE>, <<$>0x6ED9EBA1>)dnl		C  Rounds 20-39
define(<K3VALUE>, <<$>0x8F1BBCDC>)dnl		C  Rounds 40-59
define(<K4VALUE>, <<$>0xCA62C1D6>)dnl		C  Rounds 60-79
	
C Reads the input into register, byteswaps it, and stores it in the DATA array.
C SWAP(index, register)
define(<SWAP>, <
	movl	OFFSET($1)(INPUT), $2
	bswap	$2
	movl	$2, OFFSET($1) (DATA)
>)dnl

C expand(i) is the expansion function
C
C   W[i] = (W[i - 16] ^ W[i - 14] ^ W[i - 8] ^ W[i - 3]) <<< 1
C
C where W[i] is stored in DATA[i mod 16].
C
C Result is stored back in W[i], and also left in TMP, the only
C register that is used.
define(<EXPAND>, <
	movl	OFFSET(eval($1 % 16)) (DATA), TMP
	xorl	OFFSET(eval(($1 +  2) % 16)) (DATA), TMP
	xorl	OFFSET(eval(($1 +  8) % 16)) (DATA), TMP
	xorl	OFFSET(eval(($1 + 13) % 16)) (DATA), TMP
	roll	<$>1, TMP
	movl	TMP, OFFSET(eval($1 % 16)) (DATA)>)dnl
define(<NOEXPAND>, <OFFSET($1) (DATA)>)dnl

C The f functions,
C
C  f1(x,y,z) = z ^ (x & (y ^ z))
C  f2(x,y,z) = x ^ y ^ z
C  f3(x,y,z) = (x & y) | (z & (x | y))
C  f4 = f2
C
C The macro Fk(x,y,z) computes = fk(x,y,z). 
C Result is left in TMP.
define(<F1>, <
	movl	$3, TMP
	xorl	$2, TMP
	andl	$1, TMP
	xorl	$3, TMP>)dnl
define(<F2>, <
	movl	$1, TMP
	xorl	$2, TMP
	xorl	$3, TMP>)dnl
C Uses TMP2
define(<F3>, <
	movl	$1, TMP2
	andl	$2, TMP2
	movl	$1, TMP
	orl	$2, TMP
	andl	$3, TMP
	orl	TMP2, TMP>)dnl

C The form of one sha1 round is
C
C   a' = e + a <<< 5 + f( b, c, d ) + k + w;
C   b' = a;
C   c' = b <<< 30;
C   d' = c;
C   e' = d;
C
C where <<< denotes rotation. We permute our variables, so that we
C instead get
C
C   e += a <<< 5 + f( b, c, d ) + k + w;
C   b <<<= 30
C
C ROUND(a,b,c,d,e,f,w)
define(<ROUND>, <
	addl	KVALUE, $5
	addl	ifelse($7,,TMP,$7), $5
	$6($2,$3,$4)
	addl	TMP, $5

C Using the TMP register could be avoided, by rotating $1 in place,
C adding, and then rotating back.
	movl	$1, TMP
	roll	<$>5, TMP
	addl	TMP, $5
	roll	<$>30, $2>)dnl

	.file "sha1-compress.asm"

	C _nettle_sha1_compress(uint32_t *state, uint8_t *input)
	
	.text
	ALIGN(16)
PROLOGUE(_nettle_sha1_compress)
	C save all registers that need to be saved
	W64_ENTRY(2, 0)
	
	sub	$68, %rsp	C  %rsp = W

	C Load and byteswap data
	SWAP( 0, SA) SWAP( 1, SB) SWAP( 2, SC) SWAP( 3, SD)
	SWAP( 4, SA) SWAP( 5, SB) SWAP( 6, SC) SWAP( 7, SD)
	SWAP( 8, SA) SWAP( 9, SB) SWAP(10, SC) SWAP(11, SD)
	SWAP(12, SA) SWAP(13, SB) SWAP(14, SC) SWAP(15, SD)

	C Load the state vector
	movl	  (STATE), SA
	movl	 4(STATE), SB
	movl	 8(STATE), SC
	movl	12(STATE), SD
	movl	16(STATE), SE

	movl	K1VALUE, KVALUE
	ROUND(SA, SB, SC, SD, SE, <F1>, NOEXPAND( 0))
	ROUND(SE, SA, SB, SC, SD, <F1>, NOEXPAND( 1))
	ROUND(SD, SE, SA, SB, SC, <F1>, NOEXPAND( 2))
	ROUND(SC, SD, SE, SA, SB, <F1>, NOEXPAND( 3))
	ROUND(SB, SC, SD, SE, SA, <F1>, NOEXPAND( 4))

	ROUND(SA, SB, SC, SD, SE, <F1>, NOEXPAND( 5))
	ROUND(SE, SA, SB, SC, SD, <F1>, NOEXPAND( 6))
	ROUND(SD, SE, SA, SB, SC, <F1>, NOEXPAND( 7))
	ROUND(SC, SD, SE, SA, SB, <F1>, NOEXPAND( 8))
	ROUND(SB, SC, SD, SE, SA, <F1>, NOEXPAND( 9))

	ROUND(SA, SB, SC, SD, SE, <F1>, NOEXPAND(10))
	ROUND(SE, SA, SB, SC, SD, <F1>, NOEXPAND(11))
	ROUND(SD, SE, SA, SB, SC, <F1>, NOEXPAND(12))
	ROUND(SC, SD, SE, SA, SB, <F1>, NOEXPAND(13))
	ROUND(SB, SC, SD, SE, SA, <F1>, NOEXPAND(14))

	ROUND(SA, SB, SC, SD, SE, <F1>, NOEXPAND(15))
	EXPAND(16) ROUND(SE, SA, SB, SC, SD, <F1>)
	EXPAND(17) ROUND(SD, SE, SA, SB, SC, <F1>)
	EXPAND(18) ROUND(SC, SD, SE, SA, SB, <F1>)
	EXPAND(19) ROUND(SB, SC, SD, SE, SA, <F1>)

	movl	K2VALUE, KVALUE
	EXPAND(20) ROUND(SA, SB, SC, SD, SE, <F2>)
	EXPAND(21) ROUND(SE, SA, SB, SC, SD, <F2>)
	EXPAND(22) ROUND(SD, SE, SA, SB, SC, <F2>)
	EXPAND(23) ROUND(SC, SD, SE, SA, SB, <F2>)
	EXPAND(24) ROUND(SB, SC, SD, SE, SA, <F2>)

	EXPAND(25) ROUND(SA, SB, SC, SD, SE, <F2>)
	EXPAND(26) ROUND(SE, SA, SB, SC, SD, <F2>)
	EXPAND(27) ROUND(SD, SE, SA, SB, SC, <F2>)
	EXPAND(28) ROUND(SC, SD, SE, SA, SB, <F2>)
	EXPAND(29) ROUND(SB, SC, SD, SE, SA, <F2>)

	EXPAND(30) ROUND(SA, SB, SC, SD, SE, <F2>)
	EXPAND(31) ROUND(SE, SA, SB, SC, SD, <F2>)
	EXPAND(32) ROUND(SD, SE, SA, SB, SC, <F2>)
	EXPAND(33) ROUND(SC, SD, SE, SA, SB, <F2>)
	EXPAND(34) ROUND(SB, SC, SD, SE, SA, <F2>)

	EXPAND(35) ROUND(SA, SB, SC, SD, SE, <F2>)
	EXPAND(36) ROUND(SE, SA, SB, SC, SD, <F2>)
	EXPAND(37) ROUND(SD, SE, SA, SB, SC, <F2>)
	EXPAND(38) ROUND(SC, SD, SE, SA, SB, <F2>)
	EXPAND(39) ROUND(SB, SC, SD, SE, SA, <F2>)

	movl	K3VALUE, KVALUE
	EXPAND(40) ROUND(SA, SB, SC, SD, SE, <F3>)
	EXPAND(41) ROUND(SE, SA, SB, SC, SD, <F3>)
	EXPAND(42) ROUND(SD, SE, SA, SB, SC, <F3>)
	EXPAND(43) ROUND(SC, SD, SE, SA, SB, <F3>)
	EXPAND(44) ROUND(SB, SC, SD, SE, SA, <F3>)

	EXPAND(45) ROUND(SA, SB, SC, SD, SE, <F3>)
	EXPAND(46) ROUND(SE, SA, SB, SC, SD, <F3>)
	EXPAND(47) ROUND(SD, SE, SA, SB, SC, <F3>)
	EXPAND(48) ROUND(SC, SD, SE, SA, SB, <F3>)
	EXPAND(49) ROUND(SB, SC, SD, SE, SA, <F3>)

	EXPAND(50) ROUND(SA, SB, SC, SD, SE, <F3>)
	EXPAND(51) ROUND(SE, SA, SB, SC, SD, <F3>)
	EXPAND(52) ROUND(SD, SE, SA, SB, SC, <F3>)
	EXPAND(53) ROUND(SC, SD, SE, SA, SB, <F3>)
	EXPAND(54) ROUND(SB, SC, SD, SE, SA, <F3>)

	EXPAND(55) ROUND(SA, SB, SC, SD, SE, <F3>)
	EXPAND(56) ROUND(SE, SA, SB, SC, SD, <F3>)
	EXPAND(57) ROUND(SD, SE, SA, SB, SC, <F3>)
	EXPAND(58) ROUND(SC, SD, SE, SA, SB, <F3>)
	EXPAND(59) ROUND(SB, SC, SD, SE, SA, <F3>)

	movl	K4VALUE, KVALUE
	EXPAND(60) ROUND(SA, SB, SC, SD, SE, <F2>)
	EXPAND(61) ROUND(SE, SA, SB, SC, SD, <F2>)
	EXPAND(62) ROUND(SD, SE, SA, SB, SC, <F2>)
	EXPAND(63) ROUND(SC, SD, SE, SA, SB, <F2>)
	EXPAND(64) ROUND(SB, SC, SD, SE, SA, <F2>)

	EXPAND(65) ROUND(SA, SB, SC, SD, SE, <F2>)
	EXPAND(66) ROUND(SE, SA, SB, SC, SD, <F2>)
	EXPAND(67) ROUND(SD, SE, SA, SB, SC, <F2>)
	EXPAND(68) ROUND(SC, SD, SE, SA, SB, <F2>)
	EXPAND(69) ROUND(SB, SC, SD, SE, SA, <F2>)

	EXPAND(70) ROUND(SA, SB, SC, SD, SE, <F2>)
	EXPAND(71) ROUND(SE, SA, SB, SC, SD, <F2>)
	EXPAND(72) ROUND(SD, SE, SA, SB, SC, <F2>)
	EXPAND(73) ROUND(SC, SD, SE, SA, SB, <F2>)
	EXPAND(74) ROUND(SB, SC, SD, SE, SA, <F2>)

	EXPAND(75) ROUND(SA, SB, SC, SD, SE, <F2>)
	EXPAND(76) ROUND(SE, SA, SB, SC, SD, <F2>)
	EXPAND(77) ROUND(SD, SE, SA, SB, SC, <F2>)
	EXPAND(78) ROUND(SC, SD, SE, SA, SB, <F2>)
	EXPAND(79) ROUND(SB, SC, SD, SE, SA, <F2>)

	C Update the state vector
	addl	SA,   (STATE) 
	addl	SB,  4(STATE) 
	addl	SC,  8(STATE) 
	addl	SD, 12(STATE) 
	addl	SE, 16(STATE)

	add	$68, %rsp
	W64_EXIT(2, 0)
	ret
EPILOGUE(_nettle_sha1_compress)
