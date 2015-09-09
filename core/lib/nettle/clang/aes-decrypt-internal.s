



























































		



 








	.file "aes-decrypt-internal.asm"
	
	
	
	
	
	.text
	.align 4

.globl __nettle_aes_decrypt
__nettle_aes_decrypt:
	
    
  
  
	test	%edx, %edx
	jz	.Lend

        
	push	%rbx
	push	%rbp
	push	%r12
	push	%r13
	push	%r14
	push	%r15	

	mov	%rcx, %r9
	movl	%edx, %r13d
	shrl	$4, %r13d
.Lblock_loop:
	mov	%rdi,%r14
	
	
	movl	(%r8),%eax
	movl	4(%r8),%ebx
	movl	8(%r8),%ecx
	movl	12(%r8),%edx
	
	xorl	(%r14),%eax
	xorl	4(%r14),%ebx
	xorl	8(%r14),%ecx
	xorl	12(%r14),%edx
	add	$16, %r8	

	
	movl	240 (%rdi), %r15d
	subl	$1, %r15d

	add	$16,%r14		
	.align 4

.Lround_loop:
	
	movzb	%al, %rbp
	movl	256 (%rsi, %rbp, 4),%r10d
	movzb	%dh, %ebp
	xorl	1280 (%rsi, %rbp, 4),%r10d
	movl	%ecx,%ebp
	shr	$16,%rbp
	and	$0xff,%rbp
	xorl	2304 (%rsi, %rbp, 4),%r10d
	movl	%ebx,%ebp
	shr	$24,%rbp
	xorl	3328 (%rsi, %rbp, 4),%r10d
	
	movzb	%bl, %rbp
	movl	256 (%rsi, %rbp, 4),%r11d
	movzb	%ah, %ebp
	xorl	1280 (%rsi, %rbp, 4),%r11d
	movl	%edx,%ebp
	shr	$16,%rbp
	and	$0xff,%rbp
	xorl	2304 (%rsi, %rbp, 4),%r11d
	movl	%ecx,%ebp
	shr	$24,%rbp
	xorl	3328 (%rsi, %rbp, 4),%r11d
	
	movzb	%cl, %rbp
	movl	256 (%rsi, %rbp, 4),%r12d
	movzb	%bh, %ebp
	xorl	1280 (%rsi, %rbp, 4),%r12d
	movl	%eax,%ebp
	shr	$16,%rbp
	and	$0xff,%rbp
	xorl	2304 (%rsi, %rbp, 4),%r12d
	movl	%edx,%ebp
	shr	$24,%rbp
	xorl	3328 (%rsi, %rbp, 4),%r12d
	
	movzb	%dl, %rbp
	movl	256 (%rsi, %rbp, 4),%edx
	movzb	%ch, %ebp
	xorl	1280 (%rsi, %rbp, 4),%edx
	movl	%ebx,%ebp
	shr	$16,%rbp
	and	$0xff,%rbp
	xorl	2304 (%rsi, %rbp, 4),%edx
	movl	%eax,%ebp
	shr	$24,%rbp
	xorl	3328 (%rsi, %rbp, 4),%edx

	movl	%r10d, %eax
	movl	%r11d, %ebx
	movl	%r12d, %ecx

	xorl	(%r14),%eax	
	xorl	4(%r14),%ebx
	xorl	8(%r14),%ecx
	xorl	12(%r14),%edx

	add	$16,%r14	
	decl	%r15d
	jnz	.Lround_loop

	
	
	movzb	%al,%rbp
	movzbl	(%rsi, %rbp), %r10d
	movl	%edx,%ebp
	andl	$0x0000ff00,%ebp
	orl	%ebp, %r10d
	movl	%ecx,%ebp
	andl	$0x00ff0000,%ebp
	orl	%ebp, %r10d
	movl	%ebx,%ebp
	andl	$0xff000000,%ebp
	orl	%ebp, %r10d
	roll	$8, %r10d
	
	movzb	%bl,%rbp
	movzbl	(%rsi, %rbp), %r11d
	movl	%eax,%ebp
	andl	$0x0000ff00,%ebp
	orl	%ebp, %r11d
	movl	%edx,%ebp
	andl	$0x00ff0000,%ebp
	orl	%ebp, %r11d
	movl	%ecx,%ebp
	andl	$0xff000000,%ebp
	orl	%ebp, %r11d
	roll	$8, %r11d
	
	movzb	%cl,%rbp
	movzbl	(%rsi, %rbp), %r12d
	movl	%ebx,%ebp
	andl	$0x0000ff00,%ebp
	orl	%ebp, %r12d
	movl	%eax,%ebp
	andl	$0x00ff0000,%ebp
	orl	%ebp, %r12d
	movl	%edx,%ebp
	andl	$0xff000000,%ebp
	orl	%ebp, %r12d
	roll	$8, %r12d
	
	movzb	%dl,%rbp
	movzbl	(%rsi, %rbp), %edx
	movl	%ecx,%ebp
	andl	$0x0000ff00,%ebp
	orl	%ebp, %edx
	movl	%ebx,%ebp
	andl	$0x00ff0000,%ebp
	orl	%ebp, %edx
	movl	%eax,%ebp
	andl	$0xff000000,%ebp
	orl	%ebp, %edx
	roll	$8, %edx

	
	mov	$3, %r15d
.Lsubst:
	
	movzb	%r10b,%rbp
	movb	(%rsi, %rbp),%r10b
	roll	$8,%r10d

	movzb  %r11b,%rbp
	movb	(%rsi, %rbp),%r11b
	roll	$8,%r11d

	movzb  %r12b,%rbp
	movb	(%rsi, %rbp),%r12b
	roll	$8,%r12d

	movzb  %dl,%rbp
	movb	(%rsi, %rbp),%dl
	roll	$8,%edx

	decl	%r15d
	jnz	.Lsubst

	
	
	xorl	(%r14),%r10d
	xorl	4(%r14),%r11d
	xorl	8(%r14),%r12d
	xorl	12(%r14),%edx

	movl	%r10d,(%r9)
	movl	%r11d,4(%r9)
	movl	%r12d,8(%r9)
	movl	%edx,12(%r9)
	
	add	$16, %r9
	decl	%r13d

	jnz	.Lblock_loop

	pop	%r15	
	pop	%r14
	pop	%r13
	pop	%r12
	pop	%rbp
	pop	%rbx
.Lend:
	
    
  
  
	ret



