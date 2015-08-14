



































	.file "sha512-compress.asm"












	






















	
	

	.text
	.align 4


.globl __nettle_sha512_compress
__nettle_sha512_compress:
	
    
  
  

	sub	$184, %rsp
	mov	%rbx, 128(%rsp)
	mov	%rdi, 136(%rsp)	
	mov	%rbp, 144(%rsp)
	mov	%r12, 152(%rsp)
	mov	%r13, 160(%rsp)
	mov	%r14, 168(%rsp)
	mov	%r15, 176(%rsp)

	mov	(%rdi),   %rax
	mov	8(%rdi),  %rbx
	mov	16(%rdi),  %rcx
	mov	24(%rdi), %r8
	mov	32(%rdi), %r9
	mov	40(%rdi), %r10
	mov	48(%rdi), %r11
	mov	56(%rdi), %r12
	xor	%r14, %r14
	.align 4


.Loop1:
	
	mov	(%rsi, %r14, 8), %r15
	bswap	%r15
	mov	%r15, (%rsp, %r14, 8)
 
	mov	%r9, %r13
	mov	%r9, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r12
	add	%rdi, %r12
	mov	%r11, %r13
	xor	%r10, %r13
	and	%r9, %r13
	xor	%r11, %r13
	add	(%rdx,%r14,8), %r12
	add	%r13, %r12
	add	%r12, %r8

	mov	%rax, %r13
	mov	%rax, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r12
	mov	%rax, %r13
	mov	%rax, %rdi
	and	%rbx, %r13
	xor	%rbx, %rdi
	add	%r13, %r12
	and	%rcx, %rdi
	add	%rdi, %r12

	
	mov	8(%rsi, %r14, 8), %r15
	bswap	%r15
	mov	%r15, 8(%rsp, %r14, 8)
 
	mov	%r8, %r13
	mov	%r8, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r11
	add	%rdi, %r11
	mov	%r10, %r13
	xor	%r9, %r13
	and	%r8, %r13
	xor	%r10, %r13
	add	8(%rdx,%r14,8), %r11
	add	%r13, %r11
	add	%r11, %rcx

	mov	%r12, %r13
	mov	%r12, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r11
	mov	%r12, %r13
	mov	%r12, %rdi
	and	%rax, %r13
	xor	%rax, %rdi
	add	%r13, %r11
	and	%rbx, %rdi
	add	%rdi, %r11

	
	mov	16(%rsi, %r14, 8), %r15
	bswap	%r15
	mov	%r15, 16(%rsp, %r14, 8)
 
	mov	%rcx, %r13
	mov	%rcx, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r10
	add	%rdi, %r10
	mov	%r9, %r13
	xor	%r8, %r13
	and	%rcx, %r13
	xor	%r9, %r13
	add	16(%rdx,%r14,8), %r10
	add	%r13, %r10
	add	%r10, %rbx

	mov	%r11, %r13
	mov	%r11, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r10
	mov	%r11, %r13
	mov	%r11, %rdi
	and	%r12, %r13
	xor	%r12, %rdi
	add	%r13, %r10
	and	%rax, %rdi
	add	%rdi, %r10

	
	mov	24(%rsi, %r14, 8), %r15
	bswap	%r15
	mov	%r15, 24(%rsp, %r14, 8)
 
	mov	%rbx, %r13
	mov	%rbx, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r9
	add	%rdi, %r9
	mov	%r8, %r13
	xor	%rcx, %r13
	and	%rbx, %r13
	xor	%r8, %r13
	add	24(%rdx,%r14,8), %r9
	add	%r13, %r9
	add	%r9, %rax

	mov	%r10, %r13
	mov	%r10, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r9
	mov	%r10, %r13
	mov	%r10, %rdi
	and	%r11, %r13
	xor	%r11, %rdi
	add	%r13, %r9
	and	%r12, %rdi
	add	%rdi, %r9

	
	mov	32(%rsi, %r14, 8), %r15
	bswap	%r15
	mov	%r15, 32(%rsp, %r14, 8)
 
	mov	%rax, %r13
	mov	%rax, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r8
	add	%rdi, %r8
	mov	%rcx, %r13
	xor	%rbx, %r13
	and	%rax, %r13
	xor	%rcx, %r13
	add	32(%rdx,%r14,8), %r8
	add	%r13, %r8
	add	%r8, %r12

	mov	%r9, %r13
	mov	%r9, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r8
	mov	%r9, %r13
	mov	%r9, %rdi
	and	%r10, %r13
	xor	%r10, %rdi
	add	%r13, %r8
	and	%r11, %rdi
	add	%rdi, %r8

	
	mov	40(%rsi, %r14, 8), %r15
	bswap	%r15
	mov	%r15, 40(%rsp, %r14, 8)
 
	mov	%r12, %r13
	mov	%r12, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %rcx
	add	%rdi, %rcx
	mov	%rbx, %r13
	xor	%rax, %r13
	and	%r12, %r13
	xor	%rbx, %r13
	add	40(%rdx,%r14,8), %rcx
	add	%r13, %rcx
	add	%rcx, %r11

	mov	%r8, %r13
	mov	%r8, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %rcx
	mov	%r8, %r13
	mov	%r8, %rdi
	and	%r9, %r13
	xor	%r9, %rdi
	add	%r13, %rcx
	and	%r10, %rdi
	add	%rdi, %rcx

	
	mov	48(%rsi, %r14, 8), %r15
	bswap	%r15
	mov	%r15, 48(%rsp, %r14, 8)
 
	mov	%r11, %r13
	mov	%r11, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %rbx
	add	%rdi, %rbx
	mov	%rax, %r13
	xor	%r12, %r13
	and	%r11, %r13
	xor	%rax, %r13
	add	48(%rdx,%r14,8), %rbx
	add	%r13, %rbx
	add	%rbx, %r10

	mov	%rcx, %r13
	mov	%rcx, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %rbx
	mov	%rcx, %r13
	mov	%rcx, %rdi
	and	%r8, %r13
	xor	%r8, %rdi
	add	%r13, %rbx
	and	%r9, %rdi
	add	%rdi, %rbx

	
	mov	56(%rsi, %r14, 8), %r15
	bswap	%r15
	mov	%r15, 56(%rsp, %r14, 8)
 
	mov	%r10, %r13
	mov	%r10, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %rax
	add	%rdi, %rax
	mov	%r12, %r13
	xor	%r11, %r13
	and	%r10, %r13
	xor	%r12, %r13
	add	56(%rdx,%r14,8), %rax
	add	%r13, %rax
	add	%rax, %r9

	mov	%rbx, %r13
	mov	%rbx, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %rax
	mov	%rbx, %r13
	mov	%rbx, %rdi
	and	%rcx, %r13
	xor	%rcx, %rdi
	add	%r13, %rax
	and	%r8, %rdi
	add	%rdi, %rax

	add	$8, %r14
	cmp	$16, %r14
	jne	.Loop1

.Loop2:
	
	mov	(%rsp), %r15
	mov	112(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	8(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	72(%rsp), %r15
	mov	%r15, (%rsp)
 
	mov	%r9, %r13
	mov	%r9, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r12
	add	%rdi, %r12
	mov	%r11, %r13
	xor	%r10, %r13
	and	%r9, %r13
	xor	%r11, %r13
	add	(%rdx,%r14,8), %r12
	add	%r13, %r12
	add	%r12, %r8

	mov	%rax, %r13
	mov	%rax, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r12
	mov	%rax, %r13
	mov	%rax, %rdi
	and	%rbx, %r13
	xor	%rbx, %rdi
	add	%r13, %r12
	and	%rcx, %rdi
	add	%rdi, %r12

	
	mov	8(%rsp), %r15
	mov	120(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	16(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	80(%rsp), %r15
	mov	%r15, 8(%rsp)
 
	mov	%r8, %r13
	mov	%r8, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r11
	add	%rdi, %r11
	mov	%r10, %r13
	xor	%r9, %r13
	and	%r8, %r13
	xor	%r10, %r13
	add	8(%rdx,%r14,8), %r11
	add	%r13, %r11
	add	%r11, %rcx

	mov	%r12, %r13
	mov	%r12, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r11
	mov	%r12, %r13
	mov	%r12, %rdi
	and	%rax, %r13
	xor	%rax, %rdi
	add	%r13, %r11
	and	%rbx, %rdi
	add	%rdi, %r11

	
	mov	16(%rsp), %r15
	mov	(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	24(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	88(%rsp), %r15
	mov	%r15, 16(%rsp)
 
	mov	%rcx, %r13
	mov	%rcx, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r10
	add	%rdi, %r10
	mov	%r9, %r13
	xor	%r8, %r13
	and	%rcx, %r13
	xor	%r9, %r13
	add	16(%rdx,%r14,8), %r10
	add	%r13, %r10
	add	%r10, %rbx

	mov	%r11, %r13
	mov	%r11, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r10
	mov	%r11, %r13
	mov	%r11, %rdi
	and	%r12, %r13
	xor	%r12, %rdi
	add	%r13, %r10
	and	%rax, %rdi
	add	%rdi, %r10

	
	mov	24(%rsp), %r15
	mov	8(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	32(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	96(%rsp), %r15
	mov	%r15, 24(%rsp)
 
	mov	%rbx, %r13
	mov	%rbx, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r9
	add	%rdi, %r9
	mov	%r8, %r13
	xor	%rcx, %r13
	and	%rbx, %r13
	xor	%r8, %r13
	add	24(%rdx,%r14,8), %r9
	add	%r13, %r9
	add	%r9, %rax

	mov	%r10, %r13
	mov	%r10, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r9
	mov	%r10, %r13
	mov	%r10, %rdi
	and	%r11, %r13
	xor	%r11, %rdi
	add	%r13, %r9
	and	%r12, %rdi
	add	%rdi, %r9

	
	mov	32(%rsp), %r15
	mov	16(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	40(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	104(%rsp), %r15
	mov	%r15, 32(%rsp)
 
	mov	%rax, %r13
	mov	%rax, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r8
	add	%rdi, %r8
	mov	%rcx, %r13
	xor	%rbx, %r13
	and	%rax, %r13
	xor	%rcx, %r13
	add	32(%rdx,%r14,8), %r8
	add	%r13, %r8
	add	%r8, %r12

	mov	%r9, %r13
	mov	%r9, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r8
	mov	%r9, %r13
	mov	%r9, %rdi
	and	%r10, %r13
	xor	%r10, %rdi
	add	%r13, %r8
	and	%r11, %rdi
	add	%rdi, %r8

	
	mov	40(%rsp), %r15
	mov	24(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	48(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	112(%rsp), %r15
	mov	%r15, 40(%rsp)
 
	mov	%r12, %r13
	mov	%r12, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %rcx
	add	%rdi, %rcx
	mov	%rbx, %r13
	xor	%rax, %r13
	and	%r12, %r13
	xor	%rbx, %r13
	add	40(%rdx,%r14,8), %rcx
	add	%r13, %rcx
	add	%rcx, %r11

	mov	%r8, %r13
	mov	%r8, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %rcx
	mov	%r8, %r13
	mov	%r8, %rdi
	and	%r9, %r13
	xor	%r9, %rdi
	add	%r13, %rcx
	and	%r10, %rdi
	add	%rdi, %rcx

	
	mov	48(%rsp), %r15
	mov	32(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	56(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	120(%rsp), %r15
	mov	%r15, 48(%rsp)
 
	mov	%r11, %r13
	mov	%r11, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %rbx
	add	%rdi, %rbx
	mov	%rax, %r13
	xor	%r12, %r13
	and	%r11, %r13
	xor	%rax, %r13
	add	48(%rdx,%r14,8), %rbx
	add	%r13, %rbx
	add	%rbx, %r10

	mov	%rcx, %r13
	mov	%rcx, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %rbx
	mov	%rcx, %r13
	mov	%rcx, %rdi
	and	%r8, %r13
	xor	%r8, %rdi
	add	%r13, %rbx
	and	%r9, %rdi
	add	%rdi, %rbx

	
	mov	56(%rsp), %r15
	mov	40(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	64(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	(%rsp), %r15
	mov	%r15, 56(%rsp)
 
	mov	%r10, %r13
	mov	%r10, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %rax
	add	%rdi, %rax
	mov	%r12, %r13
	xor	%r11, %r13
	and	%r10, %r13
	xor	%r12, %r13
	add	56(%rdx,%r14,8), %rax
	add	%r13, %rax
	add	%rax, %r9

	mov	%rbx, %r13
	mov	%rbx, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %rax
	mov	%rbx, %r13
	mov	%rbx, %rdi
	and	%rcx, %r13
	xor	%rcx, %rdi
	add	%r13, %rax
	and	%r8, %rdi
	add	%rdi, %rax

	
	mov	64(%rsp), %r15
	mov	48(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	72(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	8(%rsp), %r15
	mov	%r15, 64(%rsp)
 
	mov	%r9, %r13
	mov	%r9, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r12
	add	%rdi, %r12
	mov	%r11, %r13
	xor	%r10, %r13
	and	%r9, %r13
	xor	%r11, %r13
	add	64(%rdx,%r14,8), %r12
	add	%r13, %r12
	add	%r12, %r8

	mov	%rax, %r13
	mov	%rax, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r12
	mov	%rax, %r13
	mov	%rax, %rdi
	and	%rbx, %r13
	xor	%rbx, %rdi
	add	%r13, %r12
	and	%rcx, %rdi
	add	%rdi, %r12

	
	mov	72(%rsp), %r15
	mov	56(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	80(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	16(%rsp), %r15
	mov	%r15, 72(%rsp)
 
	mov	%r8, %r13
	mov	%r8, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r11
	add	%rdi, %r11
	mov	%r10, %r13
	xor	%r9, %r13
	and	%r8, %r13
	xor	%r10, %r13
	add	72(%rdx,%r14,8), %r11
	add	%r13, %r11
	add	%r11, %rcx

	mov	%r12, %r13
	mov	%r12, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r11
	mov	%r12, %r13
	mov	%r12, %rdi
	and	%rax, %r13
	xor	%rax, %rdi
	add	%r13, %r11
	and	%rbx, %rdi
	add	%rdi, %r11

	
	mov	80(%rsp), %r15
	mov	64(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	88(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	24(%rsp), %r15
	mov	%r15, 80(%rsp)
 
	mov	%rcx, %r13
	mov	%rcx, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r10
	add	%rdi, %r10
	mov	%r9, %r13
	xor	%r8, %r13
	and	%rcx, %r13
	xor	%r9, %r13
	add	80(%rdx,%r14,8), %r10
	add	%r13, %r10
	add	%r10, %rbx

	mov	%r11, %r13
	mov	%r11, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r10
	mov	%r11, %r13
	mov	%r11, %rdi
	and	%r12, %r13
	xor	%r12, %rdi
	add	%r13, %r10
	and	%rax, %rdi
	add	%rdi, %r10

	
	mov	88(%rsp), %r15
	mov	72(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	96(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	32(%rsp), %r15
	mov	%r15, 88(%rsp)
 
	mov	%rbx, %r13
	mov	%rbx, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r9
	add	%rdi, %r9
	mov	%r8, %r13
	xor	%rcx, %r13
	and	%rbx, %r13
	xor	%r8, %r13
	add	88(%rdx,%r14,8), %r9
	add	%r13, %r9
	add	%r9, %rax

	mov	%r10, %r13
	mov	%r10, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r9
	mov	%r10, %r13
	mov	%r10, %rdi
	and	%r11, %r13
	xor	%r11, %rdi
	add	%r13, %r9
	and	%r12, %rdi
	add	%rdi, %r9

	
	mov	96(%rsp), %r15
	mov	80(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	104(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	40(%rsp), %r15
	mov	%r15, 96(%rsp)
 
	mov	%rax, %r13
	mov	%rax, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %r8
	add	%rdi, %r8
	mov	%rcx, %r13
	xor	%rbx, %r13
	and	%rax, %r13
	xor	%rcx, %r13
	add	96(%rdx,%r14,8), %r8
	add	%r13, %r8
	add	%r8, %r12

	mov	%r9, %r13
	mov	%r9, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %r8
	mov	%r9, %r13
	mov	%r9, %rdi
	and	%r10, %r13
	xor	%r10, %rdi
	add	%r13, %r8
	and	%r11, %rdi
	add	%rdi, %r8

	
	mov	104(%rsp), %r15
	mov	88(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	112(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	48(%rsp), %r15
	mov	%r15, 104(%rsp)
 
	mov	%r12, %r13
	mov	%r12, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %rcx
	add	%rdi, %rcx
	mov	%rbx, %r13
	xor	%rax, %r13
	and	%r12, %r13
	xor	%rbx, %r13
	add	104(%rdx,%r14,8), %rcx
	add	%r13, %rcx
	add	%rcx, %r11

	mov	%r8, %r13
	mov	%r8, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %rcx
	mov	%r8, %r13
	mov	%r8, %rdi
	and	%r9, %r13
	xor	%r9, %rdi
	add	%r13, %rcx
	and	%r10, %rdi
	add	%rdi, %rcx

	
	mov	112(%rsp), %r15
	mov	96(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	120(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	56(%rsp), %r15
	mov	%r15, 112(%rsp)
 
	mov	%r11, %r13
	mov	%r11, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %rbx
	add	%rdi, %rbx
	mov	%rax, %r13
	xor	%r12, %r13
	and	%r11, %r13
	xor	%rax, %r13
	add	112(%rdx,%r14,8), %rbx
	add	%r13, %rbx
	add	%rbx, %r10

	mov	%rcx, %r13
	mov	%rcx, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %rbx
	mov	%rcx, %r13
	mov	%rcx, %rdi
	and	%r8, %r13
	xor	%r8, %rdi
	add	%r13, %rbx
	and	%r9, %rdi
	add	%rdi, %rbx

	
	mov	120(%rsp), %r15
	mov	104(%rsp), %r13
	mov	%r13, %rdi
	shr	$6, %r13
	rol	$3, %rdi
	xor	%rdi, %r13
	rol	$42, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	mov	(%rsp), %r13
	mov	%r13, %rdi
	shr	$7, %r13
	rol	$56, %rdi
	xor	%rdi, %r13
	rol	$7, %rdi
	xor	%rdi, %r13
	add	%r13, %r15
	add	64(%rsp), %r15
	mov	%r15, 120(%rsp)
 
	mov	%r10, %r13
	mov	%r10, %rdi
	rol	$23, %r13
	rol	$46, %rdi
	xor	%r13, %rdi
	rol	$27, %r13
	xor	%r13, %rdi
	add	%r15, %rax
	add	%rdi, %rax
	mov	%r12, %r13
	xor	%r11, %r13
	and	%r10, %r13
	xor	%r12, %r13
	add	120(%rdx,%r14,8), %rax
	add	%r13, %rax
	add	%rax, %r9

	mov	%rbx, %r13
	mov	%rbx, %rdi
	rol	$25, %r13
	rol	$30, %rdi
	xor	%r13, %rdi
	rol	$11, %r13
	xor	%r13, %rdi
	add	%rdi, %rax
	mov	%rbx, %r13
	mov	%rbx, %rdi
	and	%rcx, %r13
	xor	%rcx, %rdi
	add	%r13, %rax
	and	%r8, %rdi
	add	%rdi, %rax

	add	$16, %r14
	cmp	$80, %r14
	jne	.Loop2

	mov	136(%rsp), %rdi

	add	%rax, (%rdi)
	add	%rbx, 8(%rdi)
	add	%rcx, 16(%rdi)
	add	%r8, 24(%rdi)
	add	%r9, 32(%rdi)
	add	%r10, 40(%rdi)
	add	%r11, 48(%rdi)
	add	%r12, 56(%rdi)

	mov	128(%rsp), %rbx
	mov	144(%rsp), %rbp
	mov	152(%rsp), %r12
	mov	160(%rsp), %r13
	mov	168(%rsp),%r14
	mov	176(%rsp),%r15

	add	$184, %rsp
	
    
  
  
	ret



