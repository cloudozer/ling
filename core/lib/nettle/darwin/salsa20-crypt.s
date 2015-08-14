






































































	.file "salsa20-crypt.asm"
	
	
	
	.text
	.align 4

.globl _nettle_salsa20_crypt
_nettle_salsa20_crypt:
	
    
  
  	

	test	%rsi, %rsi
	jz	.Lend

	
	mov	$-1, %eax
	movd	%eax, %xmm6
	pshufd	$0x09, %xmm6, %xmm8	
	pshufd	$0x41, %xmm6, %xmm7	
	pshufd	$0x22, %xmm6, %xmm6	
	
.Lblock_loop:
	movups	(%rdi), %xmm0
	movups	16(%rdi), %xmm1
	movups	32(%rdi), %xmm2
	movups	48(%rdi), %xmm3

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	movaps	%xmm0, %xmm4
	pxor	%xmm1, %xmm0
	pand	%xmm6, %xmm0
	pxor	%xmm0, %xmm1
	pxor	%xmm4, %xmm0

	
	movaps	%xmm2, %xmm4
	pxor	%xmm3, %xmm2
	pand	%xmm6, %xmm2
	pxor	%xmm2, %xmm3
	pxor	%xmm4, %xmm2

	
	movaps	%xmm1, %xmm4
	pxor	%xmm3, %xmm1
	pand	%xmm7, %xmm1
	pxor	%xmm1, %xmm3
	pxor	%xmm4, %xmm1

	
	movaps	%xmm0, %xmm4
	pxor	%xmm2, %xmm0
	pand	%xmm8, %xmm0
	pxor	%xmm0, %xmm2
	pxor	%xmm4, %xmm0
	

	movl	$10, %eax
	.align 4

.Loop:
	
	movaps	%xmm3, %xmm4		
	paddd	%xmm0, %xmm4		
	movaps	%xmm4, %xmm5		
	pslld	$7, %xmm4	
	psrld	$25, %xmm5	
	pxor	%xmm4, %xmm1		
	pxor	%xmm5, %xmm1		

	movaps	%xmm0, %xmm4		
	paddd	%xmm1, %xmm4		
	movaps	%xmm4, %xmm5		
	pslld	$9, %xmm4	
	psrld	$23, %xmm5	
	pxor	%xmm4, %xmm2		
	pxor	%xmm5, %xmm2		

	movaps	%xmm1, %xmm4		
	paddd	%xmm2, %xmm4		
	movaps	%xmm4, %xmm5		
	pslld	$13, %xmm4	
	psrld	$19, %xmm5	
	pxor	%xmm4, %xmm3		
	pxor	%xmm5, %xmm3		

	movaps	%xmm2, %xmm4		
	paddd	%xmm3, %xmm4		
	movaps	%xmm4, %xmm5		
	pslld	$18, %xmm4	
	psrld	$14, %xmm5	
	pxor	%xmm4, %xmm0		
	pxor	%xmm5, %xmm0		

	
	
	
	
	
	
	
	
	

	pshufd	$0x93, %xmm1, %xmm1	
	pshufd	$0x4e, %xmm2, %xmm2	
	pshufd	$0x39, %xmm3, %xmm3	

	
	movaps	%xmm1, %xmm4		
	paddd	%xmm0, %xmm4		
	movaps	%xmm4, %xmm5		
	pslld	$7, %xmm4	
	psrld	$25, %xmm5	
	pxor	%xmm4, %xmm3		
	pxor	%xmm5, %xmm3		

	movaps	%xmm0, %xmm4		
	paddd	%xmm3, %xmm4		
	movaps	%xmm4, %xmm5		
	pslld	$9, %xmm4	
	psrld	$23, %xmm5	
	pxor	%xmm4, %xmm2		
	pxor	%xmm5, %xmm2		

	movaps	%xmm3, %xmm4		
	paddd	%xmm2, %xmm4		
	movaps	%xmm4, %xmm5		
	pslld	$13, %xmm4	
	psrld	$19, %xmm5	
	pxor	%xmm4, %xmm1		
	pxor	%xmm5, %xmm1		

	movaps	%xmm2, %xmm4		
	paddd	%xmm1, %xmm4		
	movaps	%xmm4, %xmm5		
	pslld	$18, %xmm4	
	psrld	$14, %xmm5	
	pxor	%xmm4, %xmm0		
	pxor	%xmm5, %xmm0		


	
	pshufd	$0x39, %xmm1, %xmm1	
	pshufd	$0x4e, %xmm2, %xmm2	
	pshufd	$0x93, %xmm3, %xmm3	

	decl	%eax
	jnz	.Loop

	
	movaps	%xmm0, %xmm4
	pxor	%xmm2, %xmm0
	pand	%xmm8, %xmm0
	pxor	%xmm0, %xmm2
	pxor	%xmm4, %xmm0
	
	
	movaps	%xmm1, %xmm4
	pxor	%xmm3, %xmm1
	pand	%xmm7, %xmm1
	pxor	%xmm1, %xmm3
	pxor	%xmm4, %xmm1

	
	movaps	%xmm0, %xmm4
	pxor	%xmm1, %xmm0
	pand	%xmm6, %xmm0
	pxor	%xmm0, %xmm1
	pxor	%xmm4, %xmm0

	
	movaps	%xmm2, %xmm4
	pxor	%xmm3, %xmm2
	pand	%xmm6, %xmm2
	pxor	%xmm2, %xmm3
	pxor	%xmm4, %xmm2


	movups	(%rdi), %xmm4
	movups	16(%rdi), %xmm5
	paddd	%xmm4, %xmm0
	paddd	%xmm5, %xmm1
	movups	32(%rdi), %xmm4
	movups	48(%rdi), %xmm5
	paddd	%xmm4, %xmm2
	paddd	%xmm5, %xmm3

	
	incq	32(%rdi)

	cmp	$64, %rsi
	jc	.Lfinal_xor

	movups	48(%rcx), %xmm5
	pxor	%xmm5, %xmm3
	movups	%xmm3, 48(%rdx)
.Lxor3:
	movups	32(%rcx), %xmm4
	pxor	%xmm4, %xmm2
	movups	%xmm2, 32(%rdx)
.Lxor2:
	movups	16(%rcx), %xmm5
	pxor	%xmm5, %xmm1
	movups	%xmm1, 16(%rdx)
.Lxor1:
	movups	(%rcx), %xmm4	
	pxor	%xmm4, %xmm0
	movups	%xmm0, (%rdx)

	lea	64(%rcx), %rcx
	lea	64(%rdx), %rdx
	sub	$64, %rsi
	ja	.Lblock_loop
.Lend:
	
    
  
  
	ret

.Lfinal_xor:
	cmp	$32, %rsi
	jz	.Lxor2
	jc	.Llt32
	cmp	$48, %rsi
	jz	.Lxor3
	jc	.Llt48
	movaps	%xmm3, %xmm4
	call	.Lpartial
	jmp	.Lxor3
.Llt48:
	movaps	%xmm2, %xmm4
	call	.Lpartial
	jmp	.Lxor2
.Llt32:
	cmp	$16, %rsi
	jz	.Lxor1
	jc	.Llt16
	movaps	%xmm1, %xmm4
	call	.Lpartial
	jmp	.Lxor1
.Llt16:
	movaps	%xmm0, %xmm4
	call	.Lpartial
	jmp	.Lend

.Lpartial:
	mov	%rsi, %r9
	and	$-16, %r9
	test	$8, %rsi
	jz	.Llt8
	
	
	
	
	
	movd	%xmm4, %r8
	xor	(%rcx, %r9), %r8
	mov	%r8, (%rdx, %r9)
	lea	8(%r9), %r9
	pshufd	$0xee, %xmm4, %xmm4		
.Llt8:
	
	movd	%xmm4, %r8
	test	$4, %rsi
	jz	.Llt4
	mov	%r8d, %eax
	xor	(%rcx, %r9), %eax
	mov	%eax, (%rdx, %r9)
	lea	4(%r9), %r9
	shr	$32, %r8
.Llt4:
	test	$2, %rsi
	jz	.Llt2
	mov	%r8w, %ax
	xor	(%rcx, %r9), %ax
	mov	%ax, (%rdx, %r9)
	lea	2(%r9), %r9
	shr	$16, %r8d
.Llt2:
	test	$1, %rsi
	jz	.Lret
	xor	(%rcx, %r9), %r8b
	mov	%r8b, (%rdx, %r9)

.Lret:
	ret




