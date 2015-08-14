





















































	





	




































































































	.file "serpent-decrypt.asm"
	
	
	
	
	.text
	.align 4

.globl _nettle_serpent_decrypt
_nettle_serpent_decrypt:
        
	
    
  
  
	push	%rbx
	push	%rbp
	push	%r12
	push	%r13
	push	%r14

	lea	(%rcx, %rsi), %rcx
	lea	(%rdx, %rsi), %rdx
	neg	%rsi
	jz	.Lend

	cmp	$-64, %rsi
	ja	.Lblock_loop

	pcmpeqd	%xmm8, %xmm8

.Lwblock_loop:
	movups	(%rcx, %rsi), %xmm0
	movups	16(%rcx, %rsi), %xmm1
	movups	32(%rcx, %rsi), %xmm2
	movups	48(%rcx, %rsi), %xmm3

	
	movdqa		%xmm0, %xmm9
	punpcklqdq	%xmm2, %xmm9			
	punpckhqdq	%xmm2, %xmm0			
	pshufd		$0xd8, %xmm9, %xmm9		
	pshufd		$0xd8, %xmm0, %xmm10		
	
	movdqa		%xmm1, %xmm11
	punpcklqdq	%xmm3, %xmm11			
	punpckhqdq	%xmm3, %xmm1			
	pshufd		$0xd8, %xmm11, %xmm11		
	pshufd		$0xd8, %xmm1, %xmm12		

	movdqa		%xmm9, %xmm0
	punpckldq	%xmm11, %xmm0			
	movdqa		%xmm9, %xmm1
	punpckhdq	%xmm11, %xmm1			

	movdqa		%xmm10, %xmm2
	punpckldq	%xmm12, %xmm2			
	movdqa		%xmm10, %xmm3
	punpckhdq	%xmm12, %xmm3			


	mov	$384, %r13

	
	
	movups	128(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm1
	pxor	%xmm11, %xmm2
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm3
	pxor	%xmm9, %xmm0


	jmp	.Lwround_start

	.align 4


.Lwround_loop:
	
	
	movdqa	%xmm2, %xmm9
	pslld	$10, %xmm2
	psrld	$22, %xmm9
	por	%xmm9, %xmm2

	
	movdqa	%xmm0, %xmm9
	pslld	$27, %xmm0
	psrld	$5, %xmm9
	por	%xmm9, %xmm0

	movdqa	%xmm1, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm3, %xmm2
	pxor	%xmm9, %xmm2
	pxor	%xmm1, %xmm0
	pxor	%xmm3, %xmm0
	
	movdqa	%xmm3, %xmm9
	pslld	$25, %xmm3
	psrld	$7, %xmm9
	por	%xmm9, %xmm3

	
	movdqa	%xmm1, %xmm9
	pslld	$31, %xmm1
	psrld	$1, %xmm9
	por	%xmm9, %xmm1

	movdqa	%xmm0, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm2, %xmm3
	pxor	%xmm9, %xmm3
	pxor	%xmm0, %xmm1
	pxor	%xmm2, %xmm1
	
	movdqa	%xmm2, %xmm9
	pslld	$29, %xmm2
	psrld	$3, %xmm9
	por	%xmm9, %xmm2

	
	movdqa	%xmm0, %xmm9
	pslld	$19, %xmm0
	psrld	$13, %xmm9
	por	%xmm9, %xmm0


.Lwround_start:
	
	movdqa	%xmm0, %xmm7
	pand	%xmm1, %xmm7
	movdqa	%xmm1, %xmm6
	pxor	%xmm3, %xmm6
	por	%xmm7, %xmm6
	movdqa	%xmm0, %xmm5
	por	%xmm3, %xmm5
	pand	%xmm2, %xmm5
	pxor	%xmm5, %xmm6
	por	%xmm2, %xmm7
	movdqa	%xmm0, %xmm4
	por	%xmm1, %xmm4
	pand	%xmm3, %xmm4
	pxor	%xmm4, %xmm7
	pxor	%xmm1, %xmm4
	movdqa	%xmm3, %xmm5
	pxor	%xmm7, %xmm5
	
	pxor	%xmm8, %xmm5

	por	%xmm4, %xmm5
	pxor	%xmm2, %xmm4
	pxor	%xmm0, %xmm5
	por	%xmm5, %xmm3
	pxor	%xmm3, %xmm4

	
	movups	112(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm5
	pxor	%xmm11, %xmm6
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm7
	pxor	%xmm9, %xmm4


	
	
	movdqa	%xmm6, %xmm9
	pslld	$10, %xmm6
	psrld	$22, %xmm9
	por	%xmm9, %xmm6

	
	movdqa	%xmm4, %xmm9
	pslld	$27, %xmm4
	psrld	$5, %xmm9
	por	%xmm9, %xmm4

	movdqa	%xmm5, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm7, %xmm6
	pxor	%xmm9, %xmm6
	pxor	%xmm5, %xmm4
	pxor	%xmm7, %xmm4
	
	movdqa	%xmm7, %xmm9
	pslld	$25, %xmm7
	psrld	$7, %xmm9
	por	%xmm9, %xmm7

	
	movdqa	%xmm5, %xmm9
	pslld	$31, %xmm5
	psrld	$1, %xmm9
	por	%xmm9, %xmm5

	movdqa	%xmm4, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm6, %xmm7
	pxor	%xmm9, %xmm7
	pxor	%xmm4, %xmm5
	pxor	%xmm6, %xmm5
	
	movdqa	%xmm6, %xmm9
	pslld	$29, %xmm6
	psrld	$3, %xmm9
	por	%xmm9, %xmm6

	
	movdqa	%xmm4, %xmm9
	pslld	$19, %xmm4
	psrld	$13, %xmm9
	por	%xmm9, %xmm4


	
	movdqa	%xmm4, %xmm2
	pxor	%xmm6, %xmm2
	
	pxor	%xmm8, %xmm6

	movdqa	%xmm5, %xmm0
	pxor	%xmm7, %xmm0
	movdqa	%xmm4, %xmm1
	por	%xmm6, %xmm1
	pxor	%xmm0, %xmm1
	movdqa	%xmm5, %xmm3
	pand	%xmm2, %xmm3
	por	%xmm7, %xmm3
	por	%xmm6, %xmm7
	por	%xmm5, %xmm6
	pand	%xmm4, %xmm6
	movdqa	%xmm6, %xmm0
	pxor	%xmm3, %xmm0
	
	pxor	%xmm8, %xmm0

	pand	%xmm2, %xmm3
	pxor	%xmm6, %xmm3
	pxor	%xmm1, %xmm4
	pxor	%xmm4, %xmm3
	pand	%xmm0, %xmm5
	pxor	%xmm5, %xmm2
	pxor	%xmm7, %xmm2

	
	movups	96(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm1
	pxor	%xmm11, %xmm2
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm3
	pxor	%xmm9, %xmm0

	
	
	
	movdqa	%xmm2, %xmm9
	pslld	$10, %xmm2
	psrld	$22, %xmm9
	por	%xmm9, %xmm2

	
	movdqa	%xmm0, %xmm9
	pslld	$27, %xmm0
	psrld	$5, %xmm9
	por	%xmm9, %xmm0

	movdqa	%xmm1, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm3, %xmm2
	pxor	%xmm9, %xmm2
	pxor	%xmm1, %xmm0
	pxor	%xmm3, %xmm0
	
	movdqa	%xmm3, %xmm9
	pslld	$25, %xmm3
	psrld	$7, %xmm9
	por	%xmm9, %xmm3

	
	movdqa	%xmm1, %xmm9
	pslld	$31, %xmm1
	psrld	$1, %xmm9
	por	%xmm9, %xmm1

	movdqa	%xmm0, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm2, %xmm3
	pxor	%xmm9, %xmm3
	pxor	%xmm0, %xmm1
	pxor	%xmm2, %xmm1
	
	movdqa	%xmm2, %xmm9
	pslld	$29, %xmm2
	psrld	$3, %xmm9
	por	%xmm9, %xmm2

	
	movdqa	%xmm0, %xmm9
	pslld	$19, %xmm0
	psrld	$13, %xmm9
	por	%xmm9, %xmm0


	
	movdqa	%xmm0, %xmm5
	pand	%xmm3, %xmm5
	movdqa	%xmm2, %xmm7
	pxor	%xmm5, %xmm7
	movdqa	%xmm1, %xmm4
	pand	%xmm7, %xmm4
	movdqa	%xmm0, %xmm6
	pxor	%xmm3, %xmm6
	pxor	%xmm1, %xmm3
	pxor	%xmm6, %xmm4
	pand	%xmm0, %xmm2
	pand	%xmm4, %xmm0
	por	%xmm1, %xmm2
	pxor	%xmm4, %xmm5
	pxor	%xmm2, %xmm5
	movdqa	%xmm4, %xmm6
	por	%xmm5, %xmm6
	pxor	%xmm7, %xmm6
	pxor	%xmm3, %xmm6
	
	pxor	%xmm8, %xmm1

	por	%xmm0, %xmm1
	pxor	%xmm1, %xmm7

	
	movups	80(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm5
	pxor	%xmm11, %xmm6
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm7
	pxor	%xmm9, %xmm4


	
	
	movdqa	%xmm6, %xmm9
	pslld	$10, %xmm6
	psrld	$22, %xmm9
	por	%xmm9, %xmm6

	
	movdqa	%xmm4, %xmm9
	pslld	$27, %xmm4
	psrld	$5, %xmm9
	por	%xmm9, %xmm4

	movdqa	%xmm5, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm7, %xmm6
	pxor	%xmm9, %xmm6
	pxor	%xmm5, %xmm4
	pxor	%xmm7, %xmm4
	
	movdqa	%xmm7, %xmm9
	pslld	$25, %xmm7
	psrld	$7, %xmm9
	por	%xmm9, %xmm7

	
	movdqa	%xmm5, %xmm9
	pslld	$31, %xmm5
	psrld	$1, %xmm9
	por	%xmm9, %xmm5

	movdqa	%xmm4, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm6, %xmm7
	pxor	%xmm9, %xmm7
	pxor	%xmm4, %xmm5
	pxor	%xmm6, %xmm5
	
	movdqa	%xmm6, %xmm9
	pslld	$29, %xmm6
	psrld	$3, %xmm9
	por	%xmm9, %xmm6

	
	movdqa	%xmm4, %xmm9
	pslld	$19, %xmm4
	psrld	$13, %xmm9
	por	%xmm9, %xmm4


	
	movdqa	%xmm6, %xmm1
	pxor	%xmm7, %xmm1
	movdqa	%xmm6, %xmm2
	por	%xmm7, %xmm2
	pxor	%xmm5, %xmm2
	por	%xmm7, %xmm5
	movdqa	%xmm4, %xmm0
	pxor	%xmm2, %xmm0
	pxor	%xmm2, %xmm7
	pand	%xmm4, %xmm2
	pxor	%xmm2, %xmm1
	pxor	%xmm4, %xmm2
	por	%xmm6, %xmm2
	pand	%xmm5, %xmm4
	movdqa	%xmm4, %xmm3
	pxor	%xmm7, %xmm3
	
	pxor	%xmm8, %xmm4

	por	%xmm1, %xmm4
	pxor	%xmm4, %xmm0
	pxor	%xmm5, %xmm4
	pxor	%xmm4, %xmm2

	
	movups	64(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm1
	pxor	%xmm11, %xmm2
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm3
	pxor	%xmm9, %xmm0

	
	
	
	movdqa	%xmm2, %xmm9
	pslld	$10, %xmm2
	psrld	$22, %xmm9
	por	%xmm9, %xmm2

	
	movdqa	%xmm0, %xmm9
	pslld	$27, %xmm0
	psrld	$5, %xmm9
	por	%xmm9, %xmm0

	movdqa	%xmm1, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm3, %xmm2
	pxor	%xmm9, %xmm2
	pxor	%xmm1, %xmm0
	pxor	%xmm3, %xmm0
	
	movdqa	%xmm3, %xmm9
	pslld	$25, %xmm3
	psrld	$7, %xmm9
	por	%xmm9, %xmm3

	
	movdqa	%xmm1, %xmm9
	pslld	$31, %xmm1
	psrld	$1, %xmm9
	por	%xmm9, %xmm1

	movdqa	%xmm0, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm2, %xmm3
	pxor	%xmm9, %xmm3
	pxor	%xmm0, %xmm1
	pxor	%xmm2, %xmm1
	
	movdqa	%xmm2, %xmm9
	pslld	$29, %xmm2
	psrld	$3, %xmm9
	por	%xmm9, %xmm2

	
	movdqa	%xmm0, %xmm9
	pslld	$19, %xmm0
	psrld	$13, %xmm9
	por	%xmm9, %xmm0


	
	movdqa	%xmm2, %xmm7
	por	%xmm3, %xmm7
	movdqa	%xmm1, %xmm4
	pand	%xmm7, %xmm4
	movdqa	%xmm0, %xmm6
	por	%xmm3, %xmm6
	movdqa	%xmm2, %xmm5
	pxor	%xmm6, %xmm5
	pxor	%xmm5, %xmm4
	pxor	%xmm0, %xmm3
	pxor	%xmm3, %xmm7
	pxor	%xmm1, %xmm6
	pand	%xmm5, %xmm6
	pxor	%xmm3, %xmm6
	pxor	%xmm0, %xmm5
	por	%xmm4, %xmm3
	pand	%xmm3, %xmm5
	pxor	%xmm1, %xmm5
	pand	%xmm6, %xmm0
	por	%xmm1, %xmm0
	pxor	%xmm0, %xmm7

	
	movups	48(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm5
	pxor	%xmm11, %xmm6
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm7
	pxor	%xmm9, %xmm4


	
	
	movdqa	%xmm6, %xmm9
	pslld	$10, %xmm6
	psrld	$22, %xmm9
	por	%xmm9, %xmm6

	
	movdqa	%xmm4, %xmm9
	pslld	$27, %xmm4
	psrld	$5, %xmm9
	por	%xmm9, %xmm4

	movdqa	%xmm5, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm7, %xmm6
	pxor	%xmm9, %xmm6
	pxor	%xmm5, %xmm4
	pxor	%xmm7, %xmm4
	
	movdqa	%xmm7, %xmm9
	pslld	$25, %xmm7
	psrld	$7, %xmm9
	por	%xmm9, %xmm7

	
	movdqa	%xmm5, %xmm9
	pslld	$31, %xmm5
	psrld	$1, %xmm9
	por	%xmm9, %xmm5

	movdqa	%xmm4, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm6, %xmm7
	pxor	%xmm9, %xmm7
	pxor	%xmm4, %xmm5
	pxor	%xmm6, %xmm5
	
	movdqa	%xmm6, %xmm9
	pslld	$29, %xmm6
	psrld	$3, %xmm9
	por	%xmm9, %xmm6

	
	movdqa	%xmm4, %xmm9
	pslld	$19, %xmm4
	psrld	$13, %xmm9
	por	%xmm9, %xmm4


	
	movdqa	%xmm4, %xmm0
	pxor	%xmm7, %xmm0
	movdqa	%xmm6, %xmm2
	pxor	%xmm7, %xmm2
	movdqa	%xmm5, %xmm1
	por	%xmm2, %xmm1
	pxor	%xmm1, %xmm0
	movdqa	%xmm7, %xmm1
	por	%xmm0, %xmm1
	pand	%xmm5, %xmm1
	
	pxor	%xmm8, %xmm7

	movdqa	%xmm4, %xmm3
	por	%xmm6, %xmm3
	pand	%xmm3, %xmm2
	pxor	%xmm2, %xmm1
	pand	%xmm5, %xmm3
	pand	%xmm6, %xmm4
	por	%xmm7, %xmm4
	pxor	%xmm4, %xmm3
	pand	%xmm3, %xmm6
	pxor	%xmm4, %xmm6
	movdqa	%xmm0, %xmm2
	pxor	%xmm1, %xmm2
	pxor	%xmm6, %xmm2

	
	movups	32(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm1
	pxor	%xmm11, %xmm2
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm3
	pxor	%xmm9, %xmm0

	
	
	
	movdqa	%xmm2, %xmm9
	pslld	$10, %xmm2
	psrld	$22, %xmm9
	por	%xmm9, %xmm2

	
	movdqa	%xmm0, %xmm9
	pslld	$27, %xmm0
	psrld	$5, %xmm9
	por	%xmm9, %xmm0

	movdqa	%xmm1, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm3, %xmm2
	pxor	%xmm9, %xmm2
	pxor	%xmm1, %xmm0
	pxor	%xmm3, %xmm0
	
	movdqa	%xmm3, %xmm9
	pslld	$25, %xmm3
	psrld	$7, %xmm9
	por	%xmm9, %xmm3

	
	movdqa	%xmm1, %xmm9
	pslld	$31, %xmm1
	psrld	$1, %xmm9
	por	%xmm9, %xmm1

	movdqa	%xmm0, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm2, %xmm3
	pxor	%xmm9, %xmm3
	pxor	%xmm0, %xmm1
	pxor	%xmm2, %xmm1
	
	movdqa	%xmm2, %xmm9
	pslld	$29, %xmm2
	psrld	$3, %xmm9
	por	%xmm9, %xmm2

	
	movdqa	%xmm0, %xmm9
	pslld	$19, %xmm0
	psrld	$13, %xmm9
	por	%xmm9, %xmm0


	
	movdqa	%xmm1, %xmm5
	por	%xmm3, %xmm5
	pxor	%xmm2, %xmm5
	movdqa	%xmm0, %xmm7
	pxor	%xmm1, %xmm7
	movdqa	%xmm0, %xmm4
	por	%xmm5, %xmm4
	pand	%xmm7, %xmm4
	pxor	%xmm4, %xmm1
	pxor	%xmm5, %xmm7
	pand	%xmm3, %xmm1
	movdqa	%xmm0, %xmm6
	pand	%xmm2, %xmm6
	por	%xmm6, %xmm5
	por	%xmm3, %xmm6
	pxor	%xmm4, %xmm6
	
	pxor	%xmm8, %xmm6

	pxor	%xmm1, %xmm5
	pxor	%xmm5, %xmm4
	pxor	%xmm2, %xmm4
	por	%xmm6, %xmm0
	pxor	%xmm0, %xmm4

	
	movups	16(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm5
	pxor	%xmm11, %xmm6
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm7
	pxor	%xmm9, %xmm4


	
	
	movdqa	%xmm6, %xmm9
	pslld	$10, %xmm6
	psrld	$22, %xmm9
	por	%xmm9, %xmm6

	
	movdqa	%xmm4, %xmm9
	pslld	$27, %xmm4
	psrld	$5, %xmm9
	por	%xmm9, %xmm4

	movdqa	%xmm5, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm7, %xmm6
	pxor	%xmm9, %xmm6
	pxor	%xmm5, %xmm4
	pxor	%xmm7, %xmm4
	
	movdqa	%xmm7, %xmm9
	pslld	$25, %xmm7
	psrld	$7, %xmm9
	por	%xmm9, %xmm7

	
	movdqa	%xmm5, %xmm9
	pslld	$31, %xmm5
	psrld	$1, %xmm9
	por	%xmm9, %xmm5

	movdqa	%xmm4, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm6, %xmm7
	pxor	%xmm9, %xmm7
	pxor	%xmm4, %xmm5
	pxor	%xmm6, %xmm5
	
	movdqa	%xmm6, %xmm9
	pslld	$29, %xmm6
	psrld	$3, %xmm9
	por	%xmm9, %xmm6

	
	movdqa	%xmm4, %xmm9
	pslld	$19, %xmm4
	psrld	$13, %xmm9
	por	%xmm9, %xmm4


	
	movdqa	%xmm4, %xmm0
	pxor	%xmm6, %xmm0
	movdqa	%xmm4, %xmm2
	por	%xmm5, %xmm2
	movdqa	%xmm6, %xmm1
	pxor	%xmm7, %xmm1
	pxor	%xmm1, %xmm2
	pand	%xmm6, %xmm1
	por	%xmm5, %xmm6
	pxor	%xmm7, %xmm5
	por	%xmm4, %xmm1
	pand	%xmm6, %xmm5
	pxor	%xmm5, %xmm1
	por	%xmm2, %xmm4
	pxor	%xmm1, %xmm4
	movdqa	%xmm2, %xmm5
	pand	%xmm4, %xmm5
	
	pxor	%xmm8, %xmm2

	por	%xmm2, %xmm7
	pxor	%xmm6, %xmm7
	movdqa	%xmm4, %xmm3
	pxor	%xmm7, %xmm3
	por	%xmm7, %xmm5
	pxor	%xmm5, %xmm0

	
	movups	(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm1
	pxor	%xmm11, %xmm2
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm3
	pxor	%xmm9, %xmm0


	sub	$128, %r13
	jnc	.Lwround_loop

	
	movdqa		%xmm0, %xmm9
	punpcklqdq	%xmm2, %xmm9			
	punpckhqdq	%xmm2, %xmm0			
	pshufd		$0xd8, %xmm9, %xmm9		
	pshufd		$0xd8, %xmm0, %xmm10		
	
	movdqa		%xmm1, %xmm11
	punpcklqdq	%xmm3, %xmm11			
	punpckhqdq	%xmm3, %xmm1			
	pshufd		$0xd8, %xmm11, %xmm11		
	pshufd		$0xd8, %xmm1, %xmm12		

	movdqa		%xmm9, %xmm0
	punpckldq	%xmm11, %xmm0			
	movdqa		%xmm9, %xmm1
	punpckhdq	%xmm11, %xmm1			

	movdqa		%xmm10, %xmm2
	punpckldq	%xmm12, %xmm2			
	movdqa		%xmm10, %xmm3
	punpckhdq	%xmm12, %xmm3			


	movups	%xmm0, (%rdx, %rsi)
	movups	%xmm1, 16(%rdx, %rsi)
	movups	%xmm2, 32(%rdx, %rsi)
	movups	%xmm3, 48(%rdx, %rsi)

	
	add	$64, %rsi
	jz	.Lend

	cmp	$-64, %rsi
	jbe	.Lwblock_loop

.Lblock_loop:
	movl	(%rcx, %rsi), %eax
	movl	4(%rcx, %rsi), %ebx
	movl	8(%rcx, %rsi), %ebp
	movl	12(%rcx, %rsi), %r8d

	xor	512(%rdi), %eax
	xor	516(%rdi), %ebx
	xor	520(%rdi), %ebp
	xor	524(%rdi), %r8d

	mov	$384, %r13
	jmp	.Lround_start

	.align 4

.Lround_loop:
	
	rol	$10, %ebp
	rol	$27, %eax
	mov	%ebx, %r14d
	shl	$7, %r14d
	xor	%r8d, %ebp
	xor	%r14d, %ebp
	xor	%ebx, %eax
	xor	%r8d, %eax
	rol	$25, %r8d
	rol	$31, %ebx
	mov	%eax, %r14d
	shl	$3, %r14d
	xor	%ebp, %r8d
	xor	%r14d, %r8d
	xor	%eax, %ebx
	xor	%ebp, %ebx
	rol	$29, %ebp
	rol	$19, %eax

.Lround_start:
	
	mov	%eax, %r12d
	and	%ebx, %r12d
	mov	%ebx, %r11d
	xor	%r8d, %r11d
	or	%r12d, %r11d
	mov	%eax, %r10d
	or	%r8d, %r10d
	and	%ebp, %r10d
	xor	%r10d, %r11d
	or	%ebp, %r12d
	mov	%eax, %r9d
	or	%ebx, %r9d
	and	%r8d, %r9d
	xor	%r9d, %r12d
	xor	%ebx, %r9d
	mov	%r8d, %r10d
	xor	%r12d, %r10d
	not	%r10d
	or	%r9d, %r10d
	xor	%ebp, %r9d
	xor	%eax, %r10d
	or	%r10d, %r8d
	xor	%r8d, %r9d

	xor	112(%rdi, %r13), %r9d
	xor	116(%rdi, %r13), %r10d
	xor	120(%rdi, %r13), %r11d
	xor	124(%rdi, %r13), %r12d

	
	rol	$10, %r11d
	rol	$27, %r9d
	mov	%r10d, %r14d
	shl	$7, %r14d
	xor	%r12d, %r11d
	xor	%r14d, %r11d
	xor	%r10d, %r9d
	xor	%r12d, %r9d
	rol	$25, %r12d
	rol	$31, %r10d
	mov	%r9d, %r14d
	shl	$3, %r14d
	xor	%r11d, %r12d
	xor	%r14d, %r12d
	xor	%r9d, %r10d
	xor	%r11d, %r10d
	rol	$29, %r11d
	rol	$19, %r9d

	
	mov	%r9d, %ebp
	xor	%r11d, %ebp
	not	%r11d
	mov	%r10d, %eax
	xor	%r12d, %eax
	mov	%r9d, %ebx
	or	%r11d, %ebx
	xor	%eax, %ebx
	mov	%r10d, %r8d
	and	%ebp, %r8d
	or	%r12d, %r8d
	or	%r11d, %r12d
	or	%r10d, %r11d
	and	%r9d, %r11d
	mov	%r11d, %eax
	xor	%r8d, %eax
	not	%eax
	and	%ebp, %r8d
	xor	%r11d, %r8d
	xor	%ebx, %r9d
	xor	%r9d, %r8d
	and	%eax, %r10d
	xor	%r10d, %ebp
	xor	%r12d, %ebp

	xor	 96(%rdi, %r13), %eax
	xor	100(%rdi, %r13), %ebx
	xor	104(%rdi, %r13), %ebp
	xor	108(%rdi, %r13), %r8d

	
	rol	$10, %ebp
	rol	$27, %eax
	mov	%ebx, %r14d
	shl	$7, %r14d
	xor	%r8d, %ebp
	xor	%r14d, %ebp
	xor	%ebx, %eax
	xor	%r8d, %eax
	rol	$25, %r8d
	rol	$31, %ebx
	mov	%eax, %r14d
	shl	$3, %r14d
	xor	%ebp, %r8d
	xor	%r14d, %r8d
	xor	%eax, %ebx
	xor	%ebp, %ebx
	rol	$29, %ebp
	rol	$19, %eax

	
	mov	%eax, %r10d
	and	%r8d, %r10d
	mov	%ebp, %r12d
	xor	%r10d, %r12d
	mov	%ebx, %r9d
	and	%r12d, %r9d
	mov	%eax, %r11d
	xor	%r8d, %r11d
	xor	%ebx, %r8d
	xor	%r11d, %r9d
	and	%eax, %ebp
	and	%r9d, %eax
	or	%ebx, %ebp
	xor	%r9d, %r10d
	xor	%ebp, %r10d
	mov	%r9d, %r11d
	or	%r10d, %r11d
	xor	%r12d, %r11d
	xor	%r8d, %r11d
	not	%ebx
	or	%eax, %ebx
	xor	%ebx, %r12d

	xor	80(%rdi, %r13), %r9d
	xor	84(%rdi, %r13), %r10d
	xor	88(%rdi, %r13), %r11d
	xor	92(%rdi, %r13), %r12d

	
	rol	$10, %r11d
	rol	$27, %r9d
	mov	%r10d, %r14d
	shl	$7, %r14d
	xor	%r12d, %r11d
	xor	%r14d, %r11d
	xor	%r10d, %r9d
	xor	%r12d, %r9d
	rol	$25, %r12d
	rol	$31, %r10d
	mov	%r9d, %r14d
	shl	$3, %r14d
	xor	%r11d, %r12d
	xor	%r14d, %r12d
	xor	%r9d, %r10d
	xor	%r11d, %r10d
	rol	$29, %r11d
	rol	$19, %r9d

	
	mov	%r11d, %ebx
	xor	%r12d, %ebx
	mov	%r11d, %ebp
	or	%r12d, %ebp
	xor	%r10d, %ebp
	or	%r12d, %r10d
	mov	%r9d, %eax
	xor	%ebp, %eax
	xor	%ebp, %r12d
	and	%r9d, %ebp
	xor	%ebp, %ebx
	xor	%r9d, %ebp
	or	%r11d, %ebp
	and	%r10d, %r9d
	mov	%r9d, %r8d
	xor	%r12d, %r8d
	not	%r9d
	or	%ebx, %r9d
	xor	%r9d, %eax
	xor	%r10d, %r9d
	xor	%r9d, %ebp

	xor	64(%rdi, %r13), %eax
	xor	68(%rdi, %r13), %ebx
	xor	72(%rdi, %r13), %ebp
	xor	76(%rdi, %r13), %r8d

	
	rol	$10, %ebp
	rol	$27, %eax
	mov	%ebx, %r14d
	shl	$7, %r14d
	xor	%r8d, %ebp
	xor	%r14d, %ebp
	xor	%ebx, %eax
	xor	%r8d, %eax
	rol	$25, %r8d
	rol	$31, %ebx
	mov	%eax, %r14d
	shl	$3, %r14d
	xor	%ebp, %r8d
	xor	%r14d, %r8d
	xor	%eax, %ebx
	xor	%ebp, %ebx
	rol	$29, %ebp
	rol	$19, %eax

	
	mov	%ebp, %r12d
	or	%r8d, %r12d
	mov	%ebx, %r9d
	and	%r12d, %r9d
	mov	%eax, %r11d
	or	%r8d, %r11d
	mov	%ebp, %r10d
	xor	%r11d, %r10d
	xor	%r10d, %r9d
	xor	%eax, %r8d
	xor	%r8d, %r12d
	xor	%ebx, %r11d
	and	%r10d, %r11d
	xor	%r8d, %r11d
	xor	%eax, %r10d
	or	%r9d, %r8d
	and	%r8d, %r10d
	xor	%ebx, %r10d
	and	%r11d, %eax
	or	%ebx, %eax
	xor	%eax, %r12d

	xor	48(%rdi, %r13), %r9d
	xor	52(%rdi, %r13), %r10d
	xor	56(%rdi, %r13), %r11d
	xor	60(%rdi, %r13), %r12d

	
	rol	$10, %r11d
	rol	$27, %r9d
	mov	%r10d, %r14d
	shl	$7, %r14d
	xor	%r12d, %r11d
	xor	%r14d, %r11d
	xor	%r10d, %r9d
	xor	%r12d, %r9d
	rol	$25, %r12d
	rol	$31, %r10d
	mov	%r9d, %r14d
	shl	$3, %r14d
	xor	%r11d, %r12d
	xor	%r14d, %r12d
	xor	%r9d, %r10d
	xor	%r11d, %r10d
	rol	$29, %r11d
	rol	$19, %r9d

	
	mov	%r9d, %eax
	xor	%r12d, %eax
	mov	%r11d, %ebp
	xor	%r12d, %ebp
	mov	%r10d, %ebx
	or	%ebp, %ebx
	xor	%ebx, %eax
	mov	%r12d, %ebx
	or	%eax, %ebx
	and	%r10d, %ebx
	not	%r12d
	mov	%r9d, %r8d
	or	%r11d, %r8d
	and	%r8d, %ebp
	xor	%ebp, %ebx
	and	%r10d, %r8d
	and	%r11d, %r9d
	or	%r12d, %r9d
	xor	%r9d, %r8d
	and	%r8d, %r11d
	xor	%r9d, %r11d
	mov	%eax, %ebp
	xor	%ebx, %ebp
	xor	%r11d, %ebp

	xor	32(%rdi, %r13), %eax
	xor	36(%rdi, %r13), %ebx
	xor	40(%rdi, %r13), %ebp
	xor	44(%rdi, %r13), %r8d

	
	rol	$10, %ebp
	rol	$27, %eax
	mov	%ebx, %r14d
	shl	$7, %r14d
	xor	%r8d, %ebp
	xor	%r14d, %ebp
	xor	%ebx, %eax
	xor	%r8d, %eax
	rol	$25, %r8d
	rol	$31, %ebx
	mov	%eax, %r14d
	shl	$3, %r14d
	xor	%ebp, %r8d
	xor	%r14d, %r8d
	xor	%eax, %ebx
	xor	%ebp, %ebx
	rol	$29, %ebp
	rol	$19, %eax

	
	mov	%ebx, %r10d
	or	%r8d, %r10d
	xor	%ebp, %r10d
	mov	%eax, %r12d
	xor	%ebx, %r12d
	mov	%eax, %r9d
	or	%r10d, %r9d
	and	%r12d, %r9d
	xor	%r9d, %ebx
	xor	%r10d, %r12d
	and	%r8d, %ebx
	mov	%eax, %r11d
	and	%ebp, %r11d
	or	%r11d, %r10d
	or	%r8d, %r11d
	xor	%r9d, %r11d
	not	%r11d
	xor	%ebx, %r10d
	xor	%r10d, %r9d
	xor	%ebp, %r9d
	or	%r11d, %eax
	xor	%eax, %r9d

	xor	16(%rdi, %r13), %r9d
	xor	20(%rdi, %r13), %r10d
	xor	24(%rdi, %r13), %r11d
	xor	28(%rdi, %r13), %r12d

	
	rol	$10, %r11d
	rol	$27, %r9d
	mov	%r10d, %r14d
	shl	$7, %r14d
	xor	%r12d, %r11d
	xor	%r14d, %r11d
	xor	%r10d, %r9d
	xor	%r12d, %r9d
	rol	$25, %r12d
	rol	$31, %r10d
	mov	%r9d, %r14d
	shl	$3, %r14d
	xor	%r11d, %r12d
	xor	%r14d, %r12d
	xor	%r9d, %r10d
	xor	%r11d, %r10d
	rol	$29, %r11d
	rol	$19, %r9d

	
	mov	%r9d, %eax
	xor	%r11d, %eax
	mov	%r9d, %ebp
	or	%r10d, %ebp
	mov	%r11d, %ebx
	xor	%r12d, %ebx
	xor	%ebx, %ebp
	and	%r11d, %ebx
	or	%r10d, %r11d
	xor	%r12d, %r10d
	or	%r9d, %ebx
	and	%r11d, %r10d
	xor	%r10d, %ebx
	or	%ebp, %r9d
	xor	%ebx, %r9d
	mov	%ebp, %r10d
	and	%r9d, %r10d
	not	%ebp
	or	%ebp, %r12d
	xor	%r11d, %r12d
	mov	%r9d, %r8d
	xor	%r12d, %r8d
	or	%r12d, %r10d
	xor	%r10d, %eax

	xor	  (%rdi, %r13), %eax
	xor	 4(%rdi, %r13), %ebx
	xor	 8(%rdi, %r13), %ebp
	xor	12(%rdi, %r13), %r8d
	sub	$128, %r13
	jnc	.Lround_loop

	movl	%eax, (%rdx, %rsi)
	movl	%ebx, 4(%rdx, %rsi)
	movl	%ebp, 8(%rdx, %rsi)
	movl	%r8d, 12(%rdx, %rsi)
	add	$16, %rsi
	jnc	.Lblock_loop
	
.Lend:
	pop	%r14
	pop	%r13
	pop	%r12
	pop	%rbp
	pop	%rbx
	
    
  
  
	ret


