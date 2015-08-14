





















































	





	




















































































	





















	.file "serpent-encrypt.asm"
	
	
	
	
	.text
	.align 4

.globl _nettle_serpent_encrypt
_nettle_serpent_encrypt:
        
	
    
  
  
	push	%rbx
	push	%rbp
	push	%r12
	push	%r13
	push	%r14

	lea	(%rcx, %rsi), %rcx
	lea	(%rdx, %rsi), %rdx
	neg	%rsi
	jz	.Lend

	
	lea	512(%rdi), %rdi

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


	mov	$-512, %r13
	jmp	.Lwround_start

	.align 4

.Lwround_loop:
	
	
	movdqa	%xmm0, %xmm9
	pslld	$13, %xmm0
	psrld	$19, %xmm9
	por	%xmm9, %xmm0

	
	movdqa	%xmm2, %xmm9
	pslld	$3, %xmm2
	psrld	$29, %xmm9
	por	%xmm9, %xmm2

	pxor	%xmm0, %xmm1
	pxor	%xmm2, %xmm1
	movdqa	%xmm0, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm2, %xmm3
	pxor	%xmm9, %xmm3
	
	movdqa	%xmm1, %xmm9
	pslld	$1, %xmm1
	psrld	$31, %xmm9
	por	%xmm9, %xmm1

	
	movdqa	%xmm3, %xmm9
	pslld	$7, %xmm3
	psrld	$25, %xmm9
	por	%xmm9, %xmm3

	pxor	%xmm1, %xmm0
	pxor	%xmm3, %xmm0
	movdqa	%xmm1, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm3, %xmm2
	pxor	%xmm9, %xmm2
	
	movdqa	%xmm0, %xmm9
	pslld	$5, %xmm0
	psrld	$27, %xmm9
	por	%xmm9, %xmm0

	
	movdqa	%xmm2, %xmm9
	pslld	$22, %xmm2
	psrld	$10, %xmm9
	por	%xmm9, %xmm2


.Lwround_start:
	
	movups	(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm1
	pxor	%xmm11, %xmm2
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm3
	pxor	%xmm9, %xmm0

	
	movdqa	%xmm1, %xmm7	
	pxor 	%xmm2, %xmm7
	movdqa	%xmm0, %xmm4	
	por	%xmm3, %xmm4
	movdqa	%xmm0, %xmm5	
	pxor	%xmm1, %xmm5
	pxor	%xmm4, %xmm7	
	movdqa	%xmm2, %xmm6	
	por	%xmm7, %xmm6
	pxor	%xmm3, %xmm0	
	pand	%xmm3, %xmm6	
	pxor	%xmm2, %xmm3	
	por	%xmm1, %xmm2	
	movdqa	%xmm5, %xmm4	
	pand	%xmm2, %xmm4
	pxor	%xmm4, %xmm6	
	pand	%xmm6, %xmm4	
	pxor	%xmm2, %xmm4	
	pand	%xmm0, %xmm1	
	pxor	%xmm0, %xmm4	
	
	pxor	%xmm8, %xmm4
	
	movdqa	%xmm4, %xmm5	
	pxor	%xmm1, %xmm5
	pxor	%xmm3, %xmm5	

	
	
	movdqa	%xmm4, %xmm9
	pslld	$13, %xmm4
	psrld	$19, %xmm9
	por	%xmm9, %xmm4

	
	movdqa	%xmm6, %xmm9
	pslld	$3, %xmm6
	psrld	$29, %xmm9
	por	%xmm9, %xmm6

	pxor	%xmm4, %xmm5
	pxor	%xmm6, %xmm5
	movdqa	%xmm4, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm6, %xmm7
	pxor	%xmm9, %xmm7
	
	movdqa	%xmm5, %xmm9
	pslld	$1, %xmm5
	psrld	$31, %xmm9
	por	%xmm9, %xmm5

	
	movdqa	%xmm7, %xmm9
	pslld	$7, %xmm7
	psrld	$25, %xmm9
	por	%xmm9, %xmm7

	pxor	%xmm5, %xmm4
	pxor	%xmm7, %xmm4
	movdqa	%xmm5, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm7, %xmm6
	pxor	%xmm9, %xmm6
	
	movdqa	%xmm4, %xmm9
	pslld	$5, %xmm4
	psrld	$27, %xmm9
	por	%xmm9, %xmm4

	
	movdqa	%xmm6, %xmm9
	pslld	$22, %xmm6
	psrld	$10, %xmm9
	por	%xmm9, %xmm6



	
	movups	16(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm5
	pxor	%xmm11, %xmm6
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm7
	pxor	%xmm9, %xmm4

	
	movdqa	%xmm4, %xmm1	
	por	%xmm7, %xmm1 
	movdqa	%xmm6, %xmm2	
	pxor	%xmm7, %xmm2
	movdqa	%xmm5, %xmm0	
	
	pxor	%xmm8, %xmm0

	movdqa	%xmm4, %xmm3	
	pxor	%xmm6, %xmm3
	por	%xmm4, %xmm0	
	pand	%xmm7, %xmm3	
	movdqa	%xmm1, %xmm4	
	pand	%xmm2, %xmm4
	por	%xmm5, %xmm3	
	pxor	%xmm0, %xmm2	
	pxor	%xmm4, %xmm3	
	movdqa	%xmm1, %xmm4	
	pxor	%xmm3, %xmm4
	pxor	%xmm2, %xmm4	
	movdqa	%xmm5, %xmm1	
	pand	%xmm7, %xmm1
	pxor	%xmm4, %xmm1	
	movdqa	%xmm1, %xmm7	
	por	%xmm3, %xmm7
	
	pxor	%xmm8, %xmm3
	
	pand 	%xmm7, %xmm0	
	pxor	%xmm6, %xmm0	

	
	
	movdqa	%xmm0, %xmm9
	pslld	$13, %xmm0
	psrld	$19, %xmm9
	por	%xmm9, %xmm0

	
	movdqa	%xmm2, %xmm9
	pslld	$3, %xmm2
	psrld	$29, %xmm9
	por	%xmm9, %xmm2

	pxor	%xmm0, %xmm1
	pxor	%xmm2, %xmm1
	movdqa	%xmm0, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm2, %xmm3
	pxor	%xmm9, %xmm3
	
	movdqa	%xmm1, %xmm9
	pslld	$1, %xmm1
	psrld	$31, %xmm9
	por	%xmm9, %xmm1

	
	movdqa	%xmm3, %xmm9
	pslld	$7, %xmm3
	psrld	$25, %xmm9
	por	%xmm9, %xmm3

	pxor	%xmm1, %xmm0
	pxor	%xmm3, %xmm0
	movdqa	%xmm1, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm3, %xmm2
	pxor	%xmm9, %xmm2
	
	movdqa	%xmm0, %xmm9
	pslld	$5, %xmm0
	psrld	$27, %xmm9
	por	%xmm9, %xmm0

	
	movdqa	%xmm2, %xmm9
	pslld	$22, %xmm2
	psrld	$10, %xmm9
	por	%xmm9, %xmm2



	
	movups	32(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm1
	pxor	%xmm11, %xmm2
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm3
	pxor	%xmm9, %xmm0

	
	movdqa	%xmm0, %xmm6	
	por	%xmm2, %xmm6
	movdqa	%xmm0, %xmm5
	pxor	%xmm1, %xmm5
	movdqa	%xmm3, %xmm7
	pxor	%xmm6, %xmm7
	movdqa	%xmm5, %xmm4
	pxor	%xmm7, %xmm4
	por	%xmm0, %xmm3
	pxor	%xmm4, %xmm2
	movdqa	%xmm1, %xmm0
	pxor	%xmm2, %xmm0
	por	%xmm1, %xmm2
	pand	%xmm6, %xmm0
	pxor	%xmm2, %xmm7
	por	%xmm7, %xmm5
	pxor	%xmm0, %xmm5
	movdqa	%xmm7, %xmm6
	pxor	%xmm5, %xmm6
	pxor	%xmm1, %xmm6
	
	pxor	%xmm8, %xmm7

	pxor	%xmm3, %xmm6

	
	
	movdqa	%xmm4, %xmm9
	pslld	$13, %xmm4
	psrld	$19, %xmm9
	por	%xmm9, %xmm4

	
	movdqa	%xmm6, %xmm9
	pslld	$3, %xmm6
	psrld	$29, %xmm9
	por	%xmm9, %xmm6

	pxor	%xmm4, %xmm5
	pxor	%xmm6, %xmm5
	movdqa	%xmm4, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm6, %xmm7
	pxor	%xmm9, %xmm7
	
	movdqa	%xmm5, %xmm9
	pslld	$1, %xmm5
	psrld	$31, %xmm9
	por	%xmm9, %xmm5

	
	movdqa	%xmm7, %xmm9
	pslld	$7, %xmm7
	psrld	$25, %xmm9
	por	%xmm9, %xmm7

	pxor	%xmm5, %xmm4
	pxor	%xmm7, %xmm4
	movdqa	%xmm5, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm7, %xmm6
	pxor	%xmm9, %xmm6
	
	movdqa	%xmm4, %xmm9
	pslld	$5, %xmm4
	psrld	$27, %xmm9
	por	%xmm9, %xmm4

	
	movdqa	%xmm6, %xmm9
	pslld	$22, %xmm6
	psrld	$10, %xmm9
	por	%xmm9, %xmm6



	
	movups	48(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm5
	pxor	%xmm11, %xmm6
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm7
	pxor	%xmm9, %xmm4

	
	movdqa	%xmm4, %xmm1
	pxor	%xmm6, %xmm1
	movdqa	%xmm4, %xmm0
	por	%xmm7, %xmm0
	movdqa	%xmm4, %xmm3
	pand	%xmm7, %xmm3
	pand	%xmm0, %xmm1
	por	%xmm5, %xmm3
	movdqa	%xmm4, %xmm2
	pand	%xmm5, %xmm2
	por	%xmm6, %xmm2
	movdqa	%xmm7, %xmm6
	pxor	%xmm1, %xmm6
	pxor	%xmm3, %xmm1
	por	%xmm6, %xmm4
	pxor	%xmm5, %xmm6
	pand	%xmm7, %xmm3
	pxor	%xmm3, %xmm0
	movdqa	%xmm2, %xmm3
	pxor	%xmm6, %xmm3
	pxor	%xmm0, %xmm2
	por	%xmm3, %xmm7
	pand	%xmm7, %xmm5
	movdqa	%xmm4, %xmm0
	pxor	%xmm5, %xmm0

	
	
	movdqa	%xmm0, %xmm9
	pslld	$13, %xmm0
	psrld	$19, %xmm9
	por	%xmm9, %xmm0

	
	movdqa	%xmm2, %xmm9
	pslld	$3, %xmm2
	psrld	$29, %xmm9
	por	%xmm9, %xmm2

	pxor	%xmm0, %xmm1
	pxor	%xmm2, %xmm1
	movdqa	%xmm0, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm2, %xmm3
	pxor	%xmm9, %xmm3
	
	movdqa	%xmm1, %xmm9
	pslld	$1, %xmm1
	psrld	$31, %xmm9
	por	%xmm9, %xmm1

	
	movdqa	%xmm3, %xmm9
	pslld	$7, %xmm3
	psrld	$25, %xmm9
	por	%xmm9, %xmm3

	pxor	%xmm1, %xmm0
	pxor	%xmm3, %xmm0
	movdqa	%xmm1, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm3, %xmm2
	pxor	%xmm9, %xmm2
	
	movdqa	%xmm0, %xmm9
	pslld	$5, %xmm0
	psrld	$27, %xmm9
	por	%xmm9, %xmm0

	
	movdqa	%xmm2, %xmm9
	pslld	$22, %xmm2
	psrld	$10, %xmm9
	por	%xmm9, %xmm2



	
	movups	64(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm1
	pxor	%xmm11, %xmm2
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm3
	pxor	%xmm9, %xmm0

	
	movdqa	%xmm0, %xmm7
	por	%xmm1, %xmm7
	movdqa	%xmm1, %xmm6
	por	%xmm2, %xmm6
	pxor	%xmm0, %xmm6
	pand	%xmm3, %xmm7
	movdqa	%xmm1, %xmm4
	pxor	%xmm3, %xmm4
	por	%xmm6, %xmm3
	pand	%xmm3, %xmm0
	pand	%xmm2, %xmm1
	pxor	%xmm7, %xmm2
	pxor	%xmm6, %xmm7
	por	%xmm1, %xmm6
	movdqa	%xmm7, %xmm5
	pand	%xmm4, %xmm5
	pxor	%xmm5, %xmm6
	pxor	%xmm4, %xmm5
	por	%xmm1, %xmm5
	pxor	%xmm0, %xmm5
	pand	%xmm3, %xmm4
	pxor	%xmm2, %xmm4
	
	pxor	%xmm8, %xmm4


	
	
	movdqa	%xmm4, %xmm9
	pslld	$13, %xmm4
	psrld	$19, %xmm9
	por	%xmm9, %xmm4

	
	movdqa	%xmm6, %xmm9
	pslld	$3, %xmm6
	psrld	$29, %xmm9
	por	%xmm9, %xmm6

	pxor	%xmm4, %xmm5
	pxor	%xmm6, %xmm5
	movdqa	%xmm4, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm6, %xmm7
	pxor	%xmm9, %xmm7
	
	movdqa	%xmm5, %xmm9
	pslld	$1, %xmm5
	psrld	$31, %xmm9
	por	%xmm9, %xmm5

	
	movdqa	%xmm7, %xmm9
	pslld	$7, %xmm7
	psrld	$25, %xmm9
	por	%xmm9, %xmm7

	pxor	%xmm5, %xmm4
	pxor	%xmm7, %xmm4
	movdqa	%xmm5, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm7, %xmm6
	pxor	%xmm9, %xmm6
	
	movdqa	%xmm4, %xmm9
	pslld	$5, %xmm4
	psrld	$27, %xmm9
	por	%xmm9, %xmm4

	
	movdqa	%xmm6, %xmm9
	pslld	$22, %xmm6
	psrld	$10, %xmm9
	por	%xmm9, %xmm6



	
	movups	80(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm5
	pxor	%xmm11, %xmm6
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm7
	pxor	%xmm9, %xmm4

	
	movdqa	%xmm5, %xmm0
	por	%xmm7, %xmm0
	pxor	%xmm6, %xmm0
	movdqa	%xmm5, %xmm6
	pxor	%xmm7, %xmm6
	movdqa	%xmm4, %xmm2
	pxor	%xmm6, %xmm2
	pand	%xmm6, %xmm4
	pxor	%xmm4, %xmm0
	movdqa	%xmm5, %xmm3
	por	%xmm2, %xmm3
	por	%xmm0, %xmm5
	
	pxor	%xmm8, %xmm0

	por	%xmm0, %xmm4
	pxor	%xmm6, %xmm3
	pxor	%xmm4, %xmm3
	movdqa	%xmm7, %xmm1
	por	%xmm0, %xmm1
	pxor	%xmm1, %xmm7
	pxor	%xmm2, %xmm1
	por	%xmm7, %xmm2
	pxor	%xmm5, %xmm2

	
	
	movdqa	%xmm0, %xmm9
	pslld	$13, %xmm0
	psrld	$19, %xmm9
	por	%xmm9, %xmm0

	
	movdqa	%xmm2, %xmm9
	pslld	$3, %xmm2
	psrld	$29, %xmm9
	por	%xmm9, %xmm2

	pxor	%xmm0, %xmm1
	pxor	%xmm2, %xmm1
	movdqa	%xmm0, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm2, %xmm3
	pxor	%xmm9, %xmm3
	
	movdqa	%xmm1, %xmm9
	pslld	$1, %xmm1
	psrld	$31, %xmm9
	por	%xmm9, %xmm1

	
	movdqa	%xmm3, %xmm9
	pslld	$7, %xmm3
	psrld	$25, %xmm9
	por	%xmm9, %xmm3

	pxor	%xmm1, %xmm0
	pxor	%xmm3, %xmm0
	movdqa	%xmm1, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm3, %xmm2
	pxor	%xmm9, %xmm2
	
	movdqa	%xmm0, %xmm9
	pslld	$5, %xmm0
	psrld	$27, %xmm9
	por	%xmm9, %xmm0

	
	movdqa	%xmm2, %xmm9
	pslld	$22, %xmm2
	psrld	$10, %xmm9
	por	%xmm9, %xmm2



	
	movups	96(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm1
	pxor	%xmm11, %xmm2
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm3
	pxor	%xmm9, %xmm0

	
	movdqa	%xmm0, %xmm4
	pxor	%xmm3, %xmm4
	movdqa	%xmm0, %xmm5
	pand	%xmm3, %xmm5
	movdqa	%xmm0, %xmm6
	por	%xmm2, %xmm6
	por	%xmm1, %xmm3
	pxor	%xmm2, %xmm3
	pxor	%xmm1, %xmm0
	movdqa	%xmm1, %xmm7
	por	%xmm2, %xmm7
	pxor	%xmm1, %xmm2
	pand	%xmm4, %xmm7
	pxor	%xmm2, %xmm5
	
	pxor	%xmm8, %xmm5

	pand	%xmm5, %xmm4
	pand	%xmm5, %xmm1
	pxor	%xmm7, %xmm1
	pxor	%xmm3, %xmm7
	pxor	%xmm1, %xmm6
	
	pxor	%xmm8, %xmm6

	pxor	%xmm6, %xmm4
	pxor	%xmm0, %xmm4

	
	
	movdqa	%xmm4, %xmm9
	pslld	$13, %xmm4
	psrld	$19, %xmm9
	por	%xmm9, %xmm4

	
	movdqa	%xmm6, %xmm9
	pslld	$3, %xmm6
	psrld	$29, %xmm9
	por	%xmm9, %xmm6

	pxor	%xmm4, %xmm5
	pxor	%xmm6, %xmm5
	movdqa	%xmm4, %xmm9
	pslld	$3, %xmm9
	pxor	%xmm6, %xmm7
	pxor	%xmm9, %xmm7
	
	movdqa	%xmm5, %xmm9
	pslld	$1, %xmm5
	psrld	$31, %xmm9
	por	%xmm9, %xmm5

	
	movdqa	%xmm7, %xmm9
	pslld	$7, %xmm7
	psrld	$25, %xmm9
	por	%xmm9, %xmm7

	pxor	%xmm5, %xmm4
	pxor	%xmm7, %xmm4
	movdqa	%xmm5, %xmm9
	pslld	$7, %xmm9
	pxor	%xmm7, %xmm6
	pxor	%xmm9, %xmm6
	
	movdqa	%xmm4, %xmm9
	pslld	$5, %xmm4
	psrld	$27, %xmm9
	por	%xmm9, %xmm4

	
	movdqa	%xmm6, %xmm9
	pslld	$22, %xmm6
	psrld	$10, %xmm9
	por	%xmm9, %xmm6



	
	movups	112(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm5
	pxor	%xmm11, %xmm6
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm7
	pxor	%xmm9, %xmm4

	
	movdqa	%xmm4, %xmm0
	pand	%xmm6, %xmm0
	movdqa	%xmm5, %xmm3
	por	%xmm0, %xmm3	
	pxor	%xmm6, %xmm3
	movdqa	%xmm7, %xmm1
	pandn	%xmm4, %xmm1	
	pxor	%xmm1, %xmm3
	movdqa	%xmm6, %xmm1
	por	%xmm3, %xmm1
	pxor	%xmm4, %xmm1
	movdqa	%xmm4, %xmm2
	pand	%xmm5, %xmm2
	pxor	%xmm2, %xmm6
	por	%xmm7, %xmm2
	pxor	%xmm2, %xmm1
	movdqa	%xmm5, %xmm2
	por	%xmm0, %xmm2	
	pand	%xmm3, %xmm2
	pxor	%xmm1, %xmm5
	por	%xmm5, %xmm2
	pxor	%xmm4, %xmm2
	pxor	%xmm1, %xmm0
	
	pxor	%xmm8, %xmm7
	
	por	%xmm7, %xmm0
	pxor	%xmm6, %xmm0

	add	$128, %r13
	jnz	.Lwround_loop

	
	
	movups	(%rdi, %r13), %xmm9
	pshufd	$0x55, %xmm9, %xmm10
	pshufd	$0xaa, %xmm9, %xmm11
	pxor	%xmm10, %xmm1
	pxor	%xmm11, %xmm2
	pshufd	$0xff, %xmm9, %xmm10
	pshufd	$0x00, %xmm9, %xmm9
	pxor	%xmm10, %xmm3
	pxor	%xmm9, %xmm0


	
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

	mov	$-512, %r13
	jmp	.Lround_start
	
	.align 4

.Lround_loop:
	
	rol	$13, %eax
	rol	$3, %ebp
	xor	%eax, %ebx
	xor	%ebp, %ebx
	mov	%eax, %r14d
	shl	$3, %r14d
	xor	%ebp, %r8d
	xor	%r14d, %r8d
	rol	%ebx
	rol	$7, %r8d
	xor	%ebx, %eax
	xor	%r8d, %eax
	mov	%ebx, %r14d
	shl	$7, %r14d
	xor	%r8d, %ebp
	xor	%r14d, %ebp
	rol	$5, %eax
	rol	$22, %ebp

.Lround_start:
	xor	  (%rdi, %r13), %eax
	xor	 4(%rdi, %r13), %ebx
	xor	 8(%rdi, %r13), %ebp
	xor	12(%rdi, %r13), %r8d
	
	mov	%ebx, %r12d	
	xor 	%ebp, %r12d
	mov	%eax, %r9d	
	or	%r8d, %r9d
	mov	%eax, %r10d	
	xor	%ebx, %r10d
	xor	%r9d, %r12d	
	mov	%ebp, %r11d	
	or	%r12d, %r11d
	xor	%r8d, %eax	
	and	%r8d, %r11d	
	xor	%ebp, %r8d	
	or	%ebx, %ebp	
	mov	%r10d, %r9d	
	and	%ebp, %r9d
	xor	%r9d, %r11d	
	and	%r11d, %r9d	
	xor	%ebp, %r9d	
	and	%eax, %ebx	
	xor	%eax, %r9d	
	not	%r9d	
	mov	%r9d, %r10d	
	xor	%ebx, %r10d
	xor	%r8d, %r10d	

	
	rol	$13, %r9d
	rol	$3, %r11d
	xor	%r9d, %r10d
	xor	%r11d, %r10d
	mov	%r9d, %r14d
	shl	$3, %r14d
	xor	%r11d, %r12d
	xor	%r14d, %r12d
	rol	%r10d
	rol	$7, %r12d
	xor	%r10d, %r9d
	xor	%r12d, %r9d
	mov	%r10d, %r14d
	shl	$7, %r14d
	xor	%r12d, %r11d
	xor	%r14d, %r11d
	rol	$5, %r9d
	rol	$22, %r11d

	
	xor	16(%rdi, %r13), %r9d
	xor	20(%rdi, %r13), %r10d
	xor	24(%rdi, %r13), %r11d
	xor	28(%rdi, %r13), %r12d
	
	mov	%r9d, %ebx	
	or	%r12d, %ebx 
	mov	%r11d, %ebp	
	xor	%r12d, %ebp
	mov	%r10d, %eax	
	not	%eax
	mov	%r9d, %r8d	
	xor	%r11d, %r8d
	or	%r9d, %eax	
	and	%r12d, %r8d	
	mov	%ebx, %r9d	
	and	%ebp, %r9d
	or	%r10d, %r8d	
	xor	%eax, %ebp	
	xor	%r9d, %r8d	
	mov	%ebx, %r9d	
	xor	%r8d, %r9d
	xor	%ebp, %r9d	
	mov	%r10d, %ebx	
	and	%r12d, %ebx
	xor	%r9d, %ebx	
	mov	%ebx, %r12d	
	or	%r8d, %r12d
	not	%r8d	
	and 	%r12d, %eax	
	xor	%r11d, %eax	

	
	rol	$13, %eax
	rol	$3, %ebp
	xor	%eax, %ebx
	xor	%ebp, %ebx
	mov	%eax, %r14d
	shl	$3, %r14d
	xor	%ebp, %r8d
	xor	%r14d, %r8d
	rol	%ebx
	rol	$7, %r8d
	xor	%ebx, %eax
	xor	%r8d, %eax
	mov	%ebx, %r14d
	shl	$7, %r14d
	xor	%r8d, %ebp
	xor	%r14d, %ebp
	rol	$5, %eax
	rol	$22, %ebp


	xor	32(%rdi, %r13), %eax
	xor	36(%rdi, %r13), %ebx
	xor	40(%rdi, %r13), %ebp
	xor	44(%rdi, %r13), %r8d
	
	mov	%eax, %r11d	
	or	%ebp, %r11d
	mov	%eax, %r10d
	xor	%ebx, %r10d
	mov	%r8d, %r12d
	xor	%r11d, %r12d
	mov	%r10d, %r9d
	xor	%r12d, %r9d
	or	%eax, %r8d
	xor	%r9d, %ebp
	mov	%ebx, %eax
	xor	%ebp, %eax
	or	%ebx, %ebp
	and	%r11d, %eax
	xor	%ebp, %r12d
	or	%r12d, %r10d
	xor	%eax, %r10d
	mov	%r12d, %r11d
	xor	%r10d, %r11d
	xor	%ebx, %r11d
	not	%r12d
	xor	%r8d, %r11d

	
	rol	$13, %r9d
	rol	$3, %r11d
	xor	%r9d, %r10d
	xor	%r11d, %r10d
	mov	%r9d, %r14d
	shl	$3, %r14d
	xor	%r11d, %r12d
	xor	%r14d, %r12d
	rol	%r10d
	rol	$7, %r12d
	xor	%r10d, %r9d
	xor	%r12d, %r9d
	mov	%r10d, %r14d
	shl	$7, %r14d
	xor	%r12d, %r11d
	xor	%r14d, %r11d
	rol	$5, %r9d
	rol	$22, %r11d


	xor	48(%rdi, %r13), %r9d
	xor	52(%rdi, %r13), %r10d
	xor	56(%rdi, %r13), %r11d
	xor	60(%rdi, %r13), %r12d
	
	mov	%r9d, %ebx
	xor	%r11d, %ebx
	mov	%r9d, %eax
	or	%r12d, %eax
	mov	%r9d, %r8d
	and	%r12d, %r8d
	and	%eax, %ebx
	or	%r10d, %r8d
	mov	%r9d, %ebp
	and	%r10d, %ebp
	or	%r11d, %ebp
	mov	%r12d, %r11d
	xor	%ebx, %r11d
	xor	%r8d, %ebx
	or	%r11d, %r9d
	xor	%r10d, %r11d
	and	%r12d, %r8d
	xor	%r8d, %eax
	mov	%ebp, %r8d
	xor	%r11d, %r8d
	xor	%eax, %ebp
	or	%r8d, %r12d
	and	%r12d, %r10d
	mov	%r9d, %eax
	xor	%r10d, %eax

	
	rol	$13, %eax
	rol	$3, %ebp
	xor	%eax, %ebx
	xor	%ebp, %ebx
	mov	%eax, %r14d
	shl	$3, %r14d
	xor	%ebp, %r8d
	xor	%r14d, %r8d
	rol	%ebx
	rol	$7, %r8d
	xor	%ebx, %eax
	xor	%r8d, %eax
	mov	%ebx, %r14d
	shl	$7, %r14d
	xor	%r8d, %ebp
	xor	%r14d, %ebp
	rol	$5, %eax
	rol	$22, %ebp


	xor	64(%rdi, %r13), %eax
	xor	68(%rdi, %r13), %ebx
	xor	72(%rdi, %r13), %ebp
	xor	76(%rdi, %r13), %r8d
	
	mov	%eax, %r12d
	or	%ebx, %r12d
	mov	%ebx, %r11d
	or	%ebp, %r11d
	xor	%eax, %r11d
	and	%r8d, %r12d
	mov	%ebx, %r9d
	xor	%r8d, %r9d
	or	%r11d, %r8d
	and	%r8d, %eax
	and	%ebp, %ebx
	xor	%r12d, %ebp
	xor	%r11d, %r12d
	or	%ebx, %r11d
	mov	%r12d, %r10d
	and	%r9d, %r10d
	xor	%r10d, %r11d
	xor	%r9d, %r10d
	or	%ebx, %r10d
	xor	%eax, %r10d
	and	%r8d, %r9d
	xor	%ebp, %r9d
	not	%r9d

	
	rol	$13, %r9d
	rol	$3, %r11d
	xor	%r9d, %r10d
	xor	%r11d, %r10d
	mov	%r9d, %r14d
	shl	$3, %r14d
	xor	%r11d, %r12d
	xor	%r14d, %r12d
	rol	%r10d
	rol	$7, %r12d
	xor	%r10d, %r9d
	xor	%r12d, %r9d
	mov	%r10d, %r14d
	shl	$7, %r14d
	xor	%r12d, %r11d
	xor	%r14d, %r11d
	rol	$5, %r9d
	rol	$22, %r11d


	xor	80(%rdi, %r13), %r9d
	xor	84(%rdi, %r13), %r10d
	xor	88(%rdi, %r13), %r11d
	xor	92(%rdi, %r13), %r12d
	
	mov	%r10d, %eax
	or	%r12d, %eax
	xor	%r11d, %eax
	mov	%r10d, %r11d
	xor	%r12d, %r11d
	mov	%r9d, %ebp
	xor	%r11d, %ebp
	and	%r11d, %r9d
	xor	%r9d, %eax
	mov	%r10d, %r8d
	or	%ebp, %r8d
	or	%eax, %r10d
	not	%eax
	or	%eax, %r9d
	xor	%r11d, %r8d
	xor	%r9d, %r8d
	mov	%r12d, %ebx
	or	%eax, %ebx
	xor	%ebx, %r12d
	xor	%ebp, %ebx
	or	%r12d, %ebp
	xor	%r10d, %ebp

	
	rol	$13, %eax
	rol	$3, %ebp
	xor	%eax, %ebx
	xor	%ebp, %ebx
	mov	%eax, %r14d
	shl	$3, %r14d
	xor	%ebp, %r8d
	xor	%r14d, %r8d
	rol	%ebx
	rol	$7, %r8d
	xor	%ebx, %eax
	xor	%r8d, %eax
	mov	%ebx, %r14d
	shl	$7, %r14d
	xor	%r8d, %ebp
	xor	%r14d, %ebp
	rol	$5, %eax
	rol	$22, %ebp


	xor	96(%rdi, %r13), %eax
	xor	100(%rdi, %r13), %ebx
	xor	104(%rdi, %r13), %ebp
	xor	108(%rdi, %r13), %r8d
	
	mov	%eax, %r9d
	xor	%r8d, %r9d
	mov	%eax, %r10d
	and	%r8d, %r10d
	mov	%eax, %r11d
	or	%ebp, %r11d
	or	%ebx, %r8d
	xor	%ebp, %r8d
	xor	%ebx, %eax
	mov	%ebx, %r12d
	or	%ebp, %r12d
	xor	%ebx, %ebp
	and	%r9d, %r12d
	xor	%ebp, %r10d
	not	%r10d
	and	%r10d, %r9d
	and	%r10d, %ebx
	xor	%r12d, %ebx
	xor	%r8d, %r12d
	xor	%ebx, %r11d
	not	%r11d
	xor	%r11d, %r9d
	xor	%eax, %r9d

	
	rol	$13, %r9d
	rol	$3, %r11d
	xor	%r9d, %r10d
	xor	%r11d, %r10d
	mov	%r9d, %r14d
	shl	$3, %r14d
	xor	%r11d, %r12d
	xor	%r14d, %r12d
	rol	%r10d
	rol	$7, %r12d
	xor	%r10d, %r9d
	xor	%r12d, %r9d
	mov	%r10d, %r14d
	shl	$7, %r14d
	xor	%r12d, %r11d
	xor	%r14d, %r11d
	rol	$5, %r9d
	rol	$22, %r11d


	xor	112(%rdi, %r13), %r9d
	xor	116(%rdi, %r13), %r10d
	xor	120(%rdi, %r13), %r11d
	xor	124(%rdi, %r13), %r12d
	
	mov	%r9d, %eax
	and	%r11d, %eax
	mov	%r10d, %r8d
	or	%eax, %r8d	
	xor	%r11d, %r8d
	mov	%r12d, %ebx
	not	%ebx	
	and	%r9d, %ebx
	xor	%ebx, %r8d
	mov	%r11d, %ebx
	or	%r8d, %ebx
	xor	%r9d, %ebx
	mov	%r9d, %ebp
	and	%r10d, %ebp
	xor	%ebp, %r11d
	or	%r12d, %ebp
	xor	%ebp, %ebx
	mov	%r10d, %ebp
	or	%eax, %ebp	
	and	%r8d, %ebp
	xor	%ebx, %r10d
	or	%r10d, %ebp
	xor	%r9d, %ebp
	xor	%ebx, %eax
	not	%r12d	
	or	%r12d, %eax
	xor	%r11d, %eax

	add	$128, %r13
	jnz	.Lround_loop

	
	xor	  (%rdi, %r13), %eax
	xor	 4(%rdi, %r13), %ebx
	xor	 8(%rdi, %r13), %ebp
	xor	12(%rdi, %r13), %r8d

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


