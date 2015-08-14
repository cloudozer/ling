



































		
		








	







	















		
		



		
		





















	.file "sha3-permute.asm"
	
	
	.text
	.align 4

.globl _nettle_sha3_permute
_nettle_sha3_permute:
	
    
  
  
	push	%rbp
	push	%r12
	push	%r13
	push	%r14

	movl	$24, %r8d
	lea	.rc-8(%rip), %r14
	movq	(%rdi), %rax
	movups	8(%rdi), %xmm0
	movups	24(%rdi), %xmm1
	movq	%rax, %r10

	movq	40(%rdi), %rcx
	movdqa	%xmm0, %xmm10
	movups	48(%rdi), %xmm2
	movdqa	%xmm1, %xmm11
	movups	64(%rdi), %xmm3
	xorq	%rcx, %r10
	
	movq	80(%rdi), %rdx
	pxor	%xmm2, %xmm10
	movups	88(%rdi), %xmm4
	pxor	%xmm3, %xmm11
	movups	104(%rdi), %xmm5
	xorq	%rdx, %r10

	movq	120(%rdi), %rbp
	pxor	%xmm4, %xmm10
	movups	128(%rdi), %xmm6
	pxor	%xmm5, %xmm11
	movups	144(%rdi), %xmm7
	xorq	%rbp, %r10

	movq	160(%rdi), %r9
	pxor	%xmm6, %xmm10
	movups	168(%rdi), %xmm8
	pxor	%xmm7, %xmm11
	movups	184(%rdi), %xmm9
	xorq	%r9, %r10
	pxor	%xmm8, %xmm10
	pxor	%xmm9, %xmm11
	
	.align 4

.Loop:
	
	
	
	
	
	

	
	
	
	
	
	

	pshufd	$0x4e,	%xmm11, %xmm11		
	movdqa	%xmm10, %xmm13
	
	movq	%r10, (%rdi)
	movq	(%rdi), %xmm12

	punpcklqdq	%xmm10, %xmm12	
	punpckhqdq	%xmm11, %xmm13	
	punpcklqdq	%xmm12, %xmm11	
	
	movq	%xmm11, (%rdi)
	movq	(%rdi), %r11

	
	movq	%xmm10, (%rdi)
	movq	(%rdi), %r12

	rolq	$1, %r12
	xorq	%r12, %r11

	
	movdqa	%xmm13, %xmm14
	movdqa	%xmm13, %xmm15
	psllq	$1, %xmm14
	psrlq	$63, %xmm15
	pxor	%xmm14, %xmm12
	pxor	%xmm15, %xmm12		
	
	movdqa	%xmm11, %xmm10
	psrlq	$63, %xmm11
	psllq	$1, %xmm10
	pxor	%xmm11, %xmm13
	pxor	%xmm10, %xmm13	

	xorq	%r11, %rax
	xorq	%r11, %rcx
	xorq	%r11, %rdx
	xorq	%r11, %rbp
	xorq	%r11, %r9
	pxor	%xmm12, %xmm0
	pxor	%xmm12, %xmm2
	pxor	%xmm12, %xmm4
	pxor	%xmm12, %xmm6
	pxor	%xmm12, %xmm8
	pxor	%xmm13, %xmm1
	pxor	%xmm13, %xmm3
	pxor	%xmm13, %xmm5
	pxor	%xmm13, %xmm7
	pxor	%xmm13, %xmm9

	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

	
	movdqa	%xmm0, %xmm14
	movdqa	%xmm0, %xmm15
	movdqa	%xmm0, %xmm12
	psllq	$1, %xmm0
	psrlq	$63, %xmm14
	psllq	$62, %xmm15
	por	%xmm0, %xmm14	
	psrlq	$2, %xmm12
	por	%xmm15, %xmm12		

	movdqa	%xmm1, %xmm0
	movdqa	%xmm1, %xmm15
	psllq	$28, %xmm0
	psrlq	$36, %xmm15
	por	%xmm15, %xmm0	
	movdqa	%xmm1, %xmm15
	psllq	$27, %xmm1
	psrlq	$37, %xmm15
	por	%xmm15, %xmm1	
	
	punpcklqdq	%xmm14, %xmm0
	punpckhqdq	%xmm12, %xmm1

	
	
        
	
	
	
	
	rolq	$36, %rcx
	
	movq	%rcx, (%rdi)
	movq	(%rdi), %xmm14

	
	movq	%xmm2, (%rdi)
	movq	(%rdi), %rcx

	rolq	$44, %rcx		
	
	movdqa	%xmm2, %xmm15
	psllq	$6, %xmm2
	psrlq	$58, %xmm15

	por	%xmm2, %xmm15
	movdqa	%xmm3, %xmm2
	
	movdqa	%xmm2, %xmm12
	psllq	$20, %xmm2
	psrlq	$44, %xmm12

	por	%xmm12, %xmm2
	punpckhqdq	%xmm15, %xmm2	
	
	movdqa	%xmm3, %xmm15
	psllq	$55, %xmm3
	psrlq	$9, %xmm15

	por	%xmm3, %xmm15
	movdqa %xmm14, %xmm3
	punpcklqdq	%xmm15, %xmm3	

	
	
        
	
	
	
	

	rolq	$42, %rdx		
	pshufd	$0x4e,	%xmm4, %xmm14
	
	movq	%rdx, (%rdi)
	movq	(%rdi), %xmm4

	
	movq	%xmm14, (%rdi)
	movq	(%rdi), %rdx

	rolq	$43, %rdx		

	punpcklqdq	%xmm5, %xmm4
	
	movdqa	%xmm4, %xmm15
	psllq	$25, %xmm4
	psrlq	$39, %xmm15

	por	%xmm15, %xmm4		
	
	movdqa	%xmm5, %xmm12
	psllq	$39, %xmm5
	psrlq	$25, %xmm12

	por	%xmm5, %xmm12
	
	movdqa	%xmm14, %xmm5
	psllq	$10, %xmm14
	psrlq	$54, %xmm5

	por	%xmm14, %xmm5
	punpckhqdq	%xmm12, %xmm5	
	
	
	
	
	
	
	
	
	
	

	pshufd	$0x4e,	%xmm7, %xmm14
	rolq	$41, %rbp
	
	movq	%rbp, (%rdi)
	movq	(%rdi), %xmm15

	
	movq	%xmm7, (%rdi)
	movq	(%rdi), %rbp

	rolq	$21, %rbp		
	pshufd	$0x4e,	%xmm6, %xmm7
	
	movdqa	%xmm6, %xmm12
	psllq	$45, %xmm6
	psrlq	$19, %xmm12

	por	%xmm12, %xmm6
	
	movdqa	%xmm14, %xmm13
	psllq	$8, %xmm14
	psrlq	$56, %xmm13

	por	%xmm13, %xmm14
	punpcklqdq	%xmm14, %xmm6	
	
	movdqa	%xmm7, %xmm12
	psllq	$15, %xmm7
	psrlq	$49, %xmm12

	por	%xmm12, %xmm7
	punpcklqdq	%xmm15, %xmm7	
	
	
	
	
	
	
	
	
	

	rolq	$18, %r9
	
	movq	%r9, (%rdi)
	movq	(%rdi), %xmm14

	pshufd	$0x4e,	%xmm9, %xmm15
	movd	%xmm15, %r9
	rolq	$14, %r9		
	
	movdqa	%xmm9, %xmm15
	psllq	$56, %xmm9
	psrlq	$8, %xmm15

	por	%xmm15, %xmm9
	
	movdqa	%xmm8, %xmm12
	
	movdqa	%xmm12, %xmm15
	psllq	$2, %xmm12
	psrlq	$62, %xmm15

	por	%xmm15, %xmm12
	punpcklqdq	%xmm12, %xmm9	

	
	movdqa	%xmm8, %xmm15
	psllq	$61, %xmm8
	psrlq	$3, %xmm15

	por	%xmm15, %xmm8
	psrldq	$8, %xmm8
	punpcklqdq	%xmm14, %xmm8	

	
	
	movq	%rcx, %r12
	notq	%r12
	andq	%rdx, %r12
	movq	%rdx, %r13
	notq	%r13
	andq	%rbp, %r13
	movq	%rbp, %r11
	notq	%r11
	andq	%r9, %r11
	xorq	%r11, %rdx
	movq	%r9, %r10
	notq	%r10
	andq	%rax, %r10
	xorq	%r10, %rbp
	movq	%rax, %r11
	notq	%r11
	andq	%rcx, %r11
	xorq	%r11, %r9
	xorq	%r12, %rax
	xorq	%r13, %rcx

	movdqa	%xmm2, %xmm14
	pandn	%xmm4, %xmm14
	movdqa	%xmm4, %xmm15
	pandn	%xmm6, %xmm15
	movdqa	%xmm6, %xmm12
	pandn	%xmm8, %xmm12
	pxor	%xmm12, %xmm4
	movdqa	%xmm8, %xmm13
	pandn	%xmm0, %xmm13
	pxor	%xmm13, %xmm6
	movdqa	%xmm0, %xmm12
	pandn	%xmm2, %xmm12
	pxor	%xmm12, %xmm8
	pxor	%xmm14, %xmm0
	pxor	%xmm15, %xmm2

	movdqa	%xmm3, %xmm14
	pandn	%xmm5, %xmm14
	movdqa	%xmm5, %xmm15
	pandn	%xmm7, %xmm15
	movdqa	%xmm7, %xmm12
	pandn	%xmm9, %xmm12
	pxor	%xmm12, %xmm5
	movdqa	%xmm9, %xmm13
	pandn	%xmm1, %xmm13
	pxor	%xmm13, %xmm7
	movdqa	%xmm1, %xmm12
	pandn	%xmm3, %xmm12
	pxor	%xmm12, %xmm9
	pxor	%xmm14, %xmm1
	pxor	%xmm15, %xmm3

	xorq	(%r14, %r8, 8), %rax

	
	
	
	
	
	movq	%rcx, (%rdi)
	movq	(%rdi), %xmm10

	
	movq	%rbp, (%rdi)
	movq	(%rdi), %xmm11

	
	movq	%rdx, (%rdi)
	movq	(%rdi), %xmm14

	
	movq	%r9, (%rdi)
	movq	(%rdi), %xmm15

	movq	%rax, %r10
	punpcklqdq	%xmm14, %xmm10
	punpcklqdq	%xmm15, %xmm11
	
	movq	%xmm0, (%rdi)
	movq	(%rdi), %rcx

	
	movq	%xmm1, (%rdi)
	movq	(%rdi), %rbp

	psrldq	$8, %xmm0
	psrldq	$8, %xmm1
	xorq	%rcx, %r10
	xorq	%rbp, %r10
	
	movq	%xmm0, (%rdi)
	movq	(%rdi), %rdx

	
	movq	%xmm1, (%rdi)
	movq	(%rdi), %r9


	movdqa	%xmm10, %xmm0
	movdqa	%xmm11, %xmm1

	
	movdqa	%xmm2, %xmm14
	punpcklqdq	%xmm4, %xmm2
	xorq	%rdx, %r10
	xorq	%r9, %r10
	punpckhqdq	%xmm14, %xmm4
	pshufd	$0x4e,	%xmm4, %xmm4

	
	movdqa	%xmm7, %xmm14
	punpcklqdq	%xmm9, %xmm7
	pxor	%xmm2, %xmm10
	pxor	%xmm4, %xmm10
	punpckhqdq	%xmm14, %xmm9
	pshufd	$0x4e,	%xmm9, %xmm9

	
	movdqa	%xmm3, %xmm14
	movdqa	%xmm5, %xmm15
	movdqa	%xmm6, %xmm3
	movdqa	%xmm8, %xmm5
	pxor	%xmm7, %xmm11
	pxor	%xmm9, %xmm11
	punpcklqdq	%xmm8, %xmm3
	punpckhqdq	%xmm6, %xmm5
	pshufd	$0x4e,	%xmm5, %xmm5
	movdqa	%xmm14, %xmm6
	movdqa	%xmm15, %xmm8
	pxor	%xmm3, %xmm11
	pxor	%xmm5, %xmm11
	punpcklqdq	%xmm15, %xmm6
	punpckhqdq	%xmm14, %xmm8
	pshufd	$0x4e,	%xmm8, %xmm8

	decl	%r8d
	pxor	%xmm6, %xmm10
	pxor	%xmm8, %xmm10
	jnz	.Loop

	movq	%rax, (%rdi)
	movups	%xmm0, 8(%rdi)
	movups	%xmm1, 24(%rdi)

	movq	%rcx, 40(%rdi)
	movups	%xmm2, 48(%rdi)
	movups	%xmm3, 64(%rdi)
		               
	movq	%rdx, 80(%rdi)
	movups	%xmm4, 88(%rdi)
	movups	%xmm5, 104(%rdi)
		               
	movq	%rbp, 120(%rdi)
	movups	%xmm6, 128(%rdi)
	movups	%xmm7, 144(%rdi)
		               
	movq	%r9, 160(%rdi)
	movups	%xmm8, 168(%rdi)
	movups	%xmm9, 184(%rdi)

	pop	%r14
	pop	%r13
	pop	%r12
	pop	%rbp
	
    
  
  
	ret



.align 4

.rc:	
	.quad	0x8000000080008008
	.quad	0x0000000080000001
	.quad	0x8000000000008080
	.quad	0x8000000080008081
	.quad	0x800000008000000A
	.quad	0x000000000000800A
	.quad	0x8000000000000080
	.quad	0x8000000000008002
	.quad	0x8000000000008003
	.quad	0x8000000000008089
	.quad	0x800000000000008B
	.quad	0x000000008000808B
	.quad	0x000000008000000A
	.quad	0x0000000080008009
	.quad	0x0000000000000088
	.quad	0x000000000000008A
	.quad	0x8000000000008009
	.quad	0x8000000080008081
	.quad	0x0000000080000001
	.quad	0x000000000000808B
	.quad	0x8000000080008000
	.quad	0x800000000000808A
	.quad	0x0000000000008082
	.quad	0x0000000000000001


