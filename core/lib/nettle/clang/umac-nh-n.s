






























































	.file "umac-nh-n.asm"
	
	
	
	.text
	.align 4

.globl __nettle_umac_nh_n
__nettle_umac_nh_n:
	
    
  
  
	pxor	%xmm12, %xmm12
	cmp	$3, %rsi
	jc	.Lnh2
	je	.Lnh3

.Lnh4:
	movups	(%rdx), %xmm4
	movups	16(%rdx), %xmm10
	movups	32(%rdx), %xmm6	
	lea	48(%rdx), %rdx
	
	movaps	%xmm4, %xmm8
	movaps	%xmm4, %xmm5
	
	punpcklqdq %xmm10, %xmm4	
	punpckhqdq %xmm10, %xmm5	
	movaps	%xmm10, %xmm11
	punpcklqdq %xmm6, %xmm10	
	punpckhqdq %xmm6, %xmm11	

	movaps	%xmm12, %xmm13
	
.Loop4:
	movups	(%r8), %xmm8
	movups	16(%r8), %xmm9

	pshufd	$0xee, %xmm9, %xmm3	
	pshufd	$0x44, %xmm9, %xmm2	
	pshufd	$0xee, %xmm8, %xmm1	
	pshufd	$0x44, %xmm8, %xmm0	

	paddd	%xmm0, %xmm4
	paddd	%xmm1, %xmm5
	paddd	%xmm2, %xmm10
	paddd 	%xmm3, %xmm11

	pshufd	$0xf5,	%xmm4, %xmm8
	pshufd	$0xf5,	%xmm10, %xmm9
	pmuludq %xmm4, %xmm10
	pmuludq %xmm8, %xmm9
	paddq	%xmm10, %xmm12
	paddq	%xmm9, %xmm12

	pshufd	$0xf5,	%xmm5, %xmm8
	pshufd	$0xf5,	%xmm11, %xmm9
	pmuludq %xmm5, %xmm11
	pmuludq %xmm8, %xmm9
	paddq	%xmm11, %xmm12
	paddq	%xmm9, %xmm12

	movaps	%xmm6, %xmm4
	movaps	%xmm6, %xmm5
	movups	(%rdx), %xmm10
	movups	16(%rdx), %xmm6
	punpcklqdq %xmm10, %xmm4	
	punpckhqdq %xmm10, %xmm5	
	movaps	%xmm10, %xmm11

	punpcklqdq %xmm6, %xmm10	
	punpckhqdq %xmm6, %xmm11	

	paddd	%xmm4, %xmm0
	paddd	%xmm5, %xmm1
	paddd	%xmm10, %xmm2
	paddd	%xmm11, %xmm3

	pshufd	$0xf5,	%xmm0, %xmm8
	pshufd	$0xf5,	%xmm2, %xmm9
	pmuludq %xmm0, %xmm2
	pmuludq %xmm8, %xmm9
	paddq	%xmm2, %xmm13
	paddq	%xmm9, %xmm13

	pshufd	$0xf5,	%xmm1, %xmm8
	pshufd	$0xf5,	%xmm3, %xmm9
	pmuludq %xmm1, %xmm3
	pmuludq %xmm8, %xmm9
	paddq	%xmm3, %xmm13
	paddq	%xmm9, %xmm13

	subl	$32, %ecx
	lea	32(%r8), %r8
	lea	32(%rdx), %rdx
	ja	.Loop4

	movups	%xmm12, (%rdi)
	movups	%xmm13, 16(%rdi)

	
    
  
  
	ret
	
.Lnh3:
	movups	(%rdx), %xmm4
	movups	16(%rdx), %xmm5
	movaps	%xmm12, %xmm13
.Loop3:
	lea	32(%rdx), %rdx
	movups	(%r8), %xmm8
	movups	16(%r8), %xmm9
	movups	(%rdx), %xmm6
	movups	16(%rdx), %xmm7
	pshufd	$0xee, %xmm9, %xmm3	
	pshufd	$0x44, %xmm9, %xmm2	
	pshufd	$0xee, %xmm8, %xmm1	
	pshufd	$0x44, %xmm8, %xmm0	

	
	paddd	%xmm6, %xmm8
	paddd	%xmm7, %xmm9
	pshufd	$0xf5,	%xmm8, %xmm10
	pshufd	$0xf5,	%xmm9, %xmm11
	pmuludq	%xmm8, %xmm9
	pmuludq	%xmm10, %xmm11
	paddq	%xmm9, %xmm13
	paddq	%xmm11, %xmm13

	
	movaps	%xmm4, %xmm8
	punpcklqdq %xmm5, %xmm4	
	punpckhqdq %xmm5, %xmm8	
	paddd	%xmm4, %xmm0
	paddd	%xmm8, %xmm1
	movaps	%xmm6, %xmm4
	movaps	%xmm5, %xmm8
	punpcklqdq %xmm6, %xmm5	
	punpckhqdq %xmm6, %xmm8	
	paddd	%xmm5, %xmm2
	paddd	%xmm8, %xmm3

	pshufd	$0xf5,	%xmm0, %xmm8
	pshufd	$0xf5,	%xmm2, %xmm9
	pmuludq %xmm0, %xmm2
	pmuludq %xmm8, %xmm9
	paddq	%xmm2, %xmm12
	paddq	%xmm9, %xmm12
	
	pshufd	$0xf5,	%xmm1, %xmm8
	pshufd	$0xf5,	%xmm3, %xmm9
	pmuludq %xmm1, %xmm3
	pmuludq %xmm8, %xmm9
	paddq	%xmm3, %xmm12
	paddq	%xmm9, %xmm12
	subl	$32, %ecx
	lea	32(%r8), %r8
	movaps	%xmm6, %xmm4
	movaps	%xmm7, %xmm5

	ja	.Loop3

	pshufd	$0xe, %xmm13, %xmm8
	paddq	%xmm8, %xmm13
	movups	%xmm12, (%rdi)
	movlpd	%xmm13, 16(%rdi)

	
    
  
  
	ret
	
.Lnh2:
	
	
	movups	(%rdx), %xmm4
	lea	16(%rdx), %rdx
.Loop2:
	movups	(%r8), %xmm0
	movups	16(%r8), %xmm1
	pshufd	$0xee, %xmm1, %xmm3	
	pshufd	$0x44, %xmm1, %xmm2	
	pshufd	$0xee, %xmm0, %xmm1	
	pshufd	$0x44, %xmm0, %xmm0	

	movups	(%rdx), %xmm5
	movups	16(%rdx), %xmm6
	movaps	%xmm4, %xmm8
	punpcklqdq %xmm5, %xmm4	
	punpckhqdq %xmm5, %xmm8	
	paddd	%xmm4, %xmm0
	paddd	%xmm8, %xmm1
	movaps	%xmm6, %xmm4
	movaps	%xmm5, %xmm8
	punpcklqdq %xmm6, %xmm5	
	punpckhqdq %xmm6, %xmm8	
	paddd	%xmm5, %xmm2
	paddd	%xmm8, %xmm3

	pshufd	$0xf5,	%xmm0, %xmm8
	pshufd	$0xf5,	%xmm2, %xmm9
	pmuludq %xmm0, %xmm2
	pmuludq %xmm8, %xmm9
	paddq	%xmm2, %xmm12
	paddq	%xmm9, %xmm12
	
	pshufd	$0xf5,	%xmm1, %xmm8
	pshufd	$0xf5,	%xmm3, %xmm9
	pmuludq %xmm1, %xmm3
	pmuludq %xmm8, %xmm9
	paddq	%xmm3, %xmm12
	paddq	%xmm9, %xmm12
	subl	$32, %ecx
	lea	32(%r8), %r8
	lea	32(%rdx), %rdx

	ja	.Loop2

	movups	%xmm12, (%rdi)
.Lend:
	
    
  
  
	ret



