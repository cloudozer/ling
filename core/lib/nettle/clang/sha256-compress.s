



































	.file "sha256-compress.asm"












	






















	
	

	.text
	.align 4


.globl __nettle_sha256_compress
__nettle_sha256_compress:
	
    
  
  

	sub	$120, %rsp
	mov	%rbx, 64(%rsp)
	mov	%rdi, 72(%rsp)	
	mov	%rbp, 80(%rsp)
	mov	%r12, 88(%rsp)
	mov	%r13, 96(%rsp)
	mov	%r14, 104(%rsp)
	mov	%r15, 112(%rsp)

	movl	(%rdi),   %eax
	movl	4(%rdi),  %ebx
	movl	8(%rdi),  %ecx
	movl	12(%rdi), %r8d
	movl	16(%rdi), %r9d
	movl	20(%rdi), %r10d
	movl	24(%rdi), %r11d
	movl	28(%rdi), %r12d
	xor	%r14, %r14
	.align 4


.Loop1:
	
	movl	(%rsi, %r14, 4), %r15d
	bswapl	%r15d
	movl	%r15d, (%rsp, %r14, 4)
 
	movl	%r9d, %r13d
	movl	%r9d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r12d
	addl	%edi, %r12d
	movl	%r11d, %r13d
	xorl	%r10d, %r13d
	andl	%r9d, %r13d
	xorl	%r11d, %r13d
	addl	(%rdx,%r14,4), %r12d
	addl	%r13d, %r12d
	addl	%r12d, %r8d

	movl	%eax, %r13d
	movl	%eax, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r12d
	movl	%eax, %r13d
	movl	%eax, %edi
	andl	%ebx, %r13d
	xorl	%ebx, %edi
	addl	%r13d, %r12d
	andl	%ecx, %edi
	addl	%edi, %r12d

	
	movl	4(%rsi, %r14, 4), %r15d
	bswapl	%r15d
	movl	%r15d, 4(%rsp, %r14, 4)
 
	movl	%r8d, %r13d
	movl	%r8d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r11d
	addl	%edi, %r11d
	movl	%r10d, %r13d
	xorl	%r9d, %r13d
	andl	%r8d, %r13d
	xorl	%r10d, %r13d
	addl	4(%rdx,%r14,4), %r11d
	addl	%r13d, %r11d
	addl	%r11d, %ecx

	movl	%r12d, %r13d
	movl	%r12d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r11d
	movl	%r12d, %r13d
	movl	%r12d, %edi
	andl	%eax, %r13d
	xorl	%eax, %edi
	addl	%r13d, %r11d
	andl	%ebx, %edi
	addl	%edi, %r11d

	
	movl	8(%rsi, %r14, 4), %r15d
	bswapl	%r15d
	movl	%r15d, 8(%rsp, %r14, 4)
 
	movl	%ecx, %r13d
	movl	%ecx, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r10d
	addl	%edi, %r10d
	movl	%r9d, %r13d
	xorl	%r8d, %r13d
	andl	%ecx, %r13d
	xorl	%r9d, %r13d
	addl	8(%rdx,%r14,4), %r10d
	addl	%r13d, %r10d
	addl	%r10d, %ebx

	movl	%r11d, %r13d
	movl	%r11d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r10d
	movl	%r11d, %r13d
	movl	%r11d, %edi
	andl	%r12d, %r13d
	xorl	%r12d, %edi
	addl	%r13d, %r10d
	andl	%eax, %edi
	addl	%edi, %r10d

	
	movl	12(%rsi, %r14, 4), %r15d
	bswapl	%r15d
	movl	%r15d, 12(%rsp, %r14, 4)
 
	movl	%ebx, %r13d
	movl	%ebx, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r9d
	addl	%edi, %r9d
	movl	%r8d, %r13d
	xorl	%ecx, %r13d
	andl	%ebx, %r13d
	xorl	%r8d, %r13d
	addl	12(%rdx,%r14,4), %r9d
	addl	%r13d, %r9d
	addl	%r9d, %eax

	movl	%r10d, %r13d
	movl	%r10d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r9d
	movl	%r10d, %r13d
	movl	%r10d, %edi
	andl	%r11d, %r13d
	xorl	%r11d, %edi
	addl	%r13d, %r9d
	andl	%r12d, %edi
	addl	%edi, %r9d

	
	movl	16(%rsi, %r14, 4), %r15d
	bswapl	%r15d
	movl	%r15d, 16(%rsp, %r14, 4)
 
	movl	%eax, %r13d
	movl	%eax, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r8d
	addl	%edi, %r8d
	movl	%ecx, %r13d
	xorl	%ebx, %r13d
	andl	%eax, %r13d
	xorl	%ecx, %r13d
	addl	16(%rdx,%r14,4), %r8d
	addl	%r13d, %r8d
	addl	%r8d, %r12d

	movl	%r9d, %r13d
	movl	%r9d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r8d
	movl	%r9d, %r13d
	movl	%r9d, %edi
	andl	%r10d, %r13d
	xorl	%r10d, %edi
	addl	%r13d, %r8d
	andl	%r11d, %edi
	addl	%edi, %r8d

	
	movl	20(%rsi, %r14, 4), %r15d
	bswapl	%r15d
	movl	%r15d, 20(%rsp, %r14, 4)
 
	movl	%r12d, %r13d
	movl	%r12d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %ecx
	addl	%edi, %ecx
	movl	%ebx, %r13d
	xorl	%eax, %r13d
	andl	%r12d, %r13d
	xorl	%ebx, %r13d
	addl	20(%rdx,%r14,4), %ecx
	addl	%r13d, %ecx
	addl	%ecx, %r11d

	movl	%r8d, %r13d
	movl	%r8d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %ecx
	movl	%r8d, %r13d
	movl	%r8d, %edi
	andl	%r9d, %r13d
	xorl	%r9d, %edi
	addl	%r13d, %ecx
	andl	%r10d, %edi
	addl	%edi, %ecx

	
	movl	24(%rsi, %r14, 4), %r15d
	bswapl	%r15d
	movl	%r15d, 24(%rsp, %r14, 4)
 
	movl	%r11d, %r13d
	movl	%r11d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %ebx
	addl	%edi, %ebx
	movl	%eax, %r13d
	xorl	%r12d, %r13d
	andl	%r11d, %r13d
	xorl	%eax, %r13d
	addl	24(%rdx,%r14,4), %ebx
	addl	%r13d, %ebx
	addl	%ebx, %r10d

	movl	%ecx, %r13d
	movl	%ecx, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %ebx
	movl	%ecx, %r13d
	movl	%ecx, %edi
	andl	%r8d, %r13d
	xorl	%r8d, %edi
	addl	%r13d, %ebx
	andl	%r9d, %edi
	addl	%edi, %ebx

	
	movl	28(%rsi, %r14, 4), %r15d
	bswapl	%r15d
	movl	%r15d, 28(%rsp, %r14, 4)
 
	movl	%r10d, %r13d
	movl	%r10d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %eax
	addl	%edi, %eax
	movl	%r12d, %r13d
	xorl	%r11d, %r13d
	andl	%r10d, %r13d
	xorl	%r12d, %r13d
	addl	28(%rdx,%r14,4), %eax
	addl	%r13d, %eax
	addl	%eax, %r9d

	movl	%ebx, %r13d
	movl	%ebx, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %eax
	movl	%ebx, %r13d
	movl	%ebx, %edi
	andl	%ecx, %r13d
	xorl	%ecx, %edi
	addl	%r13d, %eax
	andl	%r8d, %edi
	addl	%edi, %eax

	add	$8, %r14
	cmp	$16, %r14
	jne	.Loop1

.Loop2:
	
	movl	(%rsp), %r15d
	movl	56(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	4(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	36(%rsp), %r15d
	movl	%r15d, (%rsp)
 
	movl	%r9d, %r13d
	movl	%r9d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r12d
	addl	%edi, %r12d
	movl	%r11d, %r13d
	xorl	%r10d, %r13d
	andl	%r9d, %r13d
	xorl	%r11d, %r13d
	addl	(%rdx,%r14,4), %r12d
	addl	%r13d, %r12d
	addl	%r12d, %r8d

	movl	%eax, %r13d
	movl	%eax, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r12d
	movl	%eax, %r13d
	movl	%eax, %edi
	andl	%ebx, %r13d
	xorl	%ebx, %edi
	addl	%r13d, %r12d
	andl	%ecx, %edi
	addl	%edi, %r12d

	
	movl	4(%rsp), %r15d
	movl	60(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	8(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	40(%rsp), %r15d
	movl	%r15d, 4(%rsp)
 
	movl	%r8d, %r13d
	movl	%r8d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r11d
	addl	%edi, %r11d
	movl	%r10d, %r13d
	xorl	%r9d, %r13d
	andl	%r8d, %r13d
	xorl	%r10d, %r13d
	addl	4(%rdx,%r14,4), %r11d
	addl	%r13d, %r11d
	addl	%r11d, %ecx

	movl	%r12d, %r13d
	movl	%r12d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r11d
	movl	%r12d, %r13d
	movl	%r12d, %edi
	andl	%eax, %r13d
	xorl	%eax, %edi
	addl	%r13d, %r11d
	andl	%ebx, %edi
	addl	%edi, %r11d

	
	movl	8(%rsp), %r15d
	movl	(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	12(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	44(%rsp), %r15d
	movl	%r15d, 8(%rsp)
 
	movl	%ecx, %r13d
	movl	%ecx, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r10d
	addl	%edi, %r10d
	movl	%r9d, %r13d
	xorl	%r8d, %r13d
	andl	%ecx, %r13d
	xorl	%r9d, %r13d
	addl	8(%rdx,%r14,4), %r10d
	addl	%r13d, %r10d
	addl	%r10d, %ebx

	movl	%r11d, %r13d
	movl	%r11d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r10d
	movl	%r11d, %r13d
	movl	%r11d, %edi
	andl	%r12d, %r13d
	xorl	%r12d, %edi
	addl	%r13d, %r10d
	andl	%eax, %edi
	addl	%edi, %r10d

	
	movl	12(%rsp), %r15d
	movl	4(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	16(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	48(%rsp), %r15d
	movl	%r15d, 12(%rsp)
 
	movl	%ebx, %r13d
	movl	%ebx, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r9d
	addl	%edi, %r9d
	movl	%r8d, %r13d
	xorl	%ecx, %r13d
	andl	%ebx, %r13d
	xorl	%r8d, %r13d
	addl	12(%rdx,%r14,4), %r9d
	addl	%r13d, %r9d
	addl	%r9d, %eax

	movl	%r10d, %r13d
	movl	%r10d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r9d
	movl	%r10d, %r13d
	movl	%r10d, %edi
	andl	%r11d, %r13d
	xorl	%r11d, %edi
	addl	%r13d, %r9d
	andl	%r12d, %edi
	addl	%edi, %r9d

	
	movl	16(%rsp), %r15d
	movl	8(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	20(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	52(%rsp), %r15d
	movl	%r15d, 16(%rsp)
 
	movl	%eax, %r13d
	movl	%eax, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r8d
	addl	%edi, %r8d
	movl	%ecx, %r13d
	xorl	%ebx, %r13d
	andl	%eax, %r13d
	xorl	%ecx, %r13d
	addl	16(%rdx,%r14,4), %r8d
	addl	%r13d, %r8d
	addl	%r8d, %r12d

	movl	%r9d, %r13d
	movl	%r9d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r8d
	movl	%r9d, %r13d
	movl	%r9d, %edi
	andl	%r10d, %r13d
	xorl	%r10d, %edi
	addl	%r13d, %r8d
	andl	%r11d, %edi
	addl	%edi, %r8d

	
	movl	20(%rsp), %r15d
	movl	12(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	24(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	56(%rsp), %r15d
	movl	%r15d, 20(%rsp)
 
	movl	%r12d, %r13d
	movl	%r12d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %ecx
	addl	%edi, %ecx
	movl	%ebx, %r13d
	xorl	%eax, %r13d
	andl	%r12d, %r13d
	xorl	%ebx, %r13d
	addl	20(%rdx,%r14,4), %ecx
	addl	%r13d, %ecx
	addl	%ecx, %r11d

	movl	%r8d, %r13d
	movl	%r8d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %ecx
	movl	%r8d, %r13d
	movl	%r8d, %edi
	andl	%r9d, %r13d
	xorl	%r9d, %edi
	addl	%r13d, %ecx
	andl	%r10d, %edi
	addl	%edi, %ecx

	
	movl	24(%rsp), %r15d
	movl	16(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	28(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	60(%rsp), %r15d
	movl	%r15d, 24(%rsp)
 
	movl	%r11d, %r13d
	movl	%r11d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %ebx
	addl	%edi, %ebx
	movl	%eax, %r13d
	xorl	%r12d, %r13d
	andl	%r11d, %r13d
	xorl	%eax, %r13d
	addl	24(%rdx,%r14,4), %ebx
	addl	%r13d, %ebx
	addl	%ebx, %r10d

	movl	%ecx, %r13d
	movl	%ecx, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %ebx
	movl	%ecx, %r13d
	movl	%ecx, %edi
	andl	%r8d, %r13d
	xorl	%r8d, %edi
	addl	%r13d, %ebx
	andl	%r9d, %edi
	addl	%edi, %ebx

	
	movl	28(%rsp), %r15d
	movl	20(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	32(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	(%rsp), %r15d
	movl	%r15d, 28(%rsp)
 
	movl	%r10d, %r13d
	movl	%r10d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %eax
	addl	%edi, %eax
	movl	%r12d, %r13d
	xorl	%r11d, %r13d
	andl	%r10d, %r13d
	xorl	%r12d, %r13d
	addl	28(%rdx,%r14,4), %eax
	addl	%r13d, %eax
	addl	%eax, %r9d

	movl	%ebx, %r13d
	movl	%ebx, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %eax
	movl	%ebx, %r13d
	movl	%ebx, %edi
	andl	%ecx, %r13d
	xorl	%ecx, %edi
	addl	%r13d, %eax
	andl	%r8d, %edi
	addl	%edi, %eax

	
	movl	32(%rsp), %r15d
	movl	24(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	36(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	4(%rsp), %r15d
	movl	%r15d, 32(%rsp)
 
	movl	%r9d, %r13d
	movl	%r9d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r12d
	addl	%edi, %r12d
	movl	%r11d, %r13d
	xorl	%r10d, %r13d
	andl	%r9d, %r13d
	xorl	%r11d, %r13d
	addl	32(%rdx,%r14,4), %r12d
	addl	%r13d, %r12d
	addl	%r12d, %r8d

	movl	%eax, %r13d
	movl	%eax, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r12d
	movl	%eax, %r13d
	movl	%eax, %edi
	andl	%ebx, %r13d
	xorl	%ebx, %edi
	addl	%r13d, %r12d
	andl	%ecx, %edi
	addl	%edi, %r12d

	
	movl	36(%rsp), %r15d
	movl	28(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	40(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	8(%rsp), %r15d
	movl	%r15d, 36(%rsp)
 
	movl	%r8d, %r13d
	movl	%r8d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r11d
	addl	%edi, %r11d
	movl	%r10d, %r13d
	xorl	%r9d, %r13d
	andl	%r8d, %r13d
	xorl	%r10d, %r13d
	addl	36(%rdx,%r14,4), %r11d
	addl	%r13d, %r11d
	addl	%r11d, %ecx

	movl	%r12d, %r13d
	movl	%r12d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r11d
	movl	%r12d, %r13d
	movl	%r12d, %edi
	andl	%eax, %r13d
	xorl	%eax, %edi
	addl	%r13d, %r11d
	andl	%ebx, %edi
	addl	%edi, %r11d

	
	movl	40(%rsp), %r15d
	movl	32(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	44(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	12(%rsp), %r15d
	movl	%r15d, 40(%rsp)
 
	movl	%ecx, %r13d
	movl	%ecx, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r10d
	addl	%edi, %r10d
	movl	%r9d, %r13d
	xorl	%r8d, %r13d
	andl	%ecx, %r13d
	xorl	%r9d, %r13d
	addl	40(%rdx,%r14,4), %r10d
	addl	%r13d, %r10d
	addl	%r10d, %ebx

	movl	%r11d, %r13d
	movl	%r11d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r10d
	movl	%r11d, %r13d
	movl	%r11d, %edi
	andl	%r12d, %r13d
	xorl	%r12d, %edi
	addl	%r13d, %r10d
	andl	%eax, %edi
	addl	%edi, %r10d

	
	movl	44(%rsp), %r15d
	movl	36(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	48(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	16(%rsp), %r15d
	movl	%r15d, 44(%rsp)
 
	movl	%ebx, %r13d
	movl	%ebx, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r9d
	addl	%edi, %r9d
	movl	%r8d, %r13d
	xorl	%ecx, %r13d
	andl	%ebx, %r13d
	xorl	%r8d, %r13d
	addl	44(%rdx,%r14,4), %r9d
	addl	%r13d, %r9d
	addl	%r9d, %eax

	movl	%r10d, %r13d
	movl	%r10d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r9d
	movl	%r10d, %r13d
	movl	%r10d, %edi
	andl	%r11d, %r13d
	xorl	%r11d, %edi
	addl	%r13d, %r9d
	andl	%r12d, %edi
	addl	%edi, %r9d

	
	movl	48(%rsp), %r15d
	movl	40(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	52(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	20(%rsp), %r15d
	movl	%r15d, 48(%rsp)
 
	movl	%eax, %r13d
	movl	%eax, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %r8d
	addl	%edi, %r8d
	movl	%ecx, %r13d
	xorl	%ebx, %r13d
	andl	%eax, %r13d
	xorl	%ecx, %r13d
	addl	48(%rdx,%r14,4), %r8d
	addl	%r13d, %r8d
	addl	%r8d, %r12d

	movl	%r9d, %r13d
	movl	%r9d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %r8d
	movl	%r9d, %r13d
	movl	%r9d, %edi
	andl	%r10d, %r13d
	xorl	%r10d, %edi
	addl	%r13d, %r8d
	andl	%r11d, %edi
	addl	%edi, %r8d

	
	movl	52(%rsp), %r15d
	movl	44(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	56(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	24(%rsp), %r15d
	movl	%r15d, 52(%rsp)
 
	movl	%r12d, %r13d
	movl	%r12d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %ecx
	addl	%edi, %ecx
	movl	%ebx, %r13d
	xorl	%eax, %r13d
	andl	%r12d, %r13d
	xorl	%ebx, %r13d
	addl	52(%rdx,%r14,4), %ecx
	addl	%r13d, %ecx
	addl	%ecx, %r11d

	movl	%r8d, %r13d
	movl	%r8d, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %ecx
	movl	%r8d, %r13d
	movl	%r8d, %edi
	andl	%r9d, %r13d
	xorl	%r9d, %edi
	addl	%r13d, %ecx
	andl	%r10d, %edi
	addl	%edi, %ecx

	
	movl	56(%rsp), %r15d
	movl	48(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	60(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	28(%rsp), %r15d
	movl	%r15d, 56(%rsp)
 
	movl	%r11d, %r13d
	movl	%r11d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %ebx
	addl	%edi, %ebx
	movl	%eax, %r13d
	xorl	%r12d, %r13d
	andl	%r11d, %r13d
	xorl	%eax, %r13d
	addl	56(%rdx,%r14,4), %ebx
	addl	%r13d, %ebx
	addl	%ebx, %r10d

	movl	%ecx, %r13d
	movl	%ecx, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %ebx
	movl	%ecx, %r13d
	movl	%ecx, %edi
	andl	%r8d, %r13d
	xorl	%r8d, %edi
	addl	%r13d, %ebx
	andl	%r9d, %edi
	addl	%edi, %ebx

	
	movl	60(%rsp), %r15d
	movl	52(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$10, %r13d
	roll	$13, %edi
	xorl	%edi, %r13d
	roll	$2, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	movl	(%rsp), %r13d
	movl	%r13d, %edi
	shrl	$3, %r13d
	roll	$14, %edi
	xorl	%edi, %r13d
	roll	$11, %edi
	xorl	%edi, %r13d
	addl	%r13d, %r15d
	addl	32(%rsp), %r15d
	movl	%r15d, 60(%rsp)
 
	movl	%r10d, %r13d
	movl	%r10d, %edi
	roll	$7, %r13d
	roll	$21, %edi
	xorl	%r13d, %edi
	roll	$19, %r13d
	xorl	%r13d, %edi
	addl	%r15d, %eax
	addl	%edi, %eax
	movl	%r12d, %r13d
	xorl	%r11d, %r13d
	andl	%r10d, %r13d
	xorl	%r12d, %r13d
	addl	60(%rdx,%r14,4), %eax
	addl	%r13d, %eax
	addl	%eax, %r9d

	movl	%ebx, %r13d
	movl	%ebx, %edi
	roll	$10, %r13d
	roll	$19, %edi
	xorl	%r13d, %edi
	roll	$20, %r13d
	xorl	%r13d, %edi
	addl	%edi, %eax
	movl	%ebx, %r13d
	movl	%ebx, %edi
	andl	%ecx, %r13d
	xorl	%ecx, %edi
	addl	%r13d, %eax
	andl	%r8d, %edi
	addl	%edi, %eax

	add	$16, %r14
	cmp	$64, %r14
	jne	.Loop2

	mov	72(%rsp), %rdi

	addl	%eax, (%rdi)
	addl	%ebx, 4(%rdi)
	addl	%ecx, 8(%rdi)
	addl	%r8d, 12(%rdi)
	addl	%r9d, 16(%rdi)
	addl	%r10d, 20(%rdi)
	addl	%r11d, 24(%rdi)
	addl	%r12d, 28(%rdi)

	mov	64(%rsp), %rbx
	mov	80(%rsp), %rbp
	mov	88(%rsp), %r12
	mov	96(%rsp), %r13
	mov	104(%rsp),%r14
	mov	112(%rsp),%r15

	add	$120, %rsp
	
    
  
  
	ret



