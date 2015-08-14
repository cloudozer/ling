




















































 

 


 














	.file "camellia-encrypt-internal.asm"
	
	
	
	
	
	.text
	.align 4

.globl __nettle_camellia_crypt
__nettle_camellia_crypt:

	
    
  
  
	test	%rdx, %rdx
	jz	.Lend

	push	%rbx
	push	%rbp
	push	%r12
	
.Lblock_loop:
	
	mov	(%r8), %rax
	bswap	%rax
	mov	8(%r8), %rbx
	bswap	%rbx
	add	$16, %r8
	mov	%rdi, %r9
	movl	(%r9), %r10d
	sub	$8, %r10

	
	xor	8(%r9), %rax
	add	$16, %r9

	
	
	movzbl	%al, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rax

	
	movzbl	%al, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rax

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	0(%r9), %rbx
	xor	%r12, %rbx

	
	
	movzbl	%bl, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rbx

	
	movzbl	%bl, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rbx

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	8(%r9), %rax
	xor	%r12, %rax

	
	
	movzbl	%al, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rax

	
	movzbl	%al, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rax

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	16(%r9), %rbx
	xor	%r12, %rbx

	
	
	movzbl	%bl, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rbx

	
	movzbl	%bl, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rbx

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	24(%r9), %rax
	xor	%r12, %rax

	
	
	movzbl	%al, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rax

	
	movzbl	%al, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rax

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	32(%r9), %rbx
	xor	%r12, %rbx
 
	
	
	movzbl	%bl, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rbx

	
	movzbl	%bl, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rbx

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	40(%r9), %rax
	xor	%r12, %rax

	
.Lround_loop:
	add	$64, %r9
	
	mov	%rax, %rbp
	shr	$32, %rbp
	andl	-16 + 4(%r9), %ebp
	roll	$1, %ebp

	xor	%rbp, %rax
	movl	-16(%r9), %ebp
	orl	%eax, %ebp
	shl	$32, %rbp
	xor	%rbp, %rax

	
	movl	-8(%r9), %ebp
	orl	%ebx, %ebp
	shl	$32, %rbp
	xor	%rbp, %rbx
	mov	%rbx, %rbp
	shr	$32, %rbp
	andl	-8 + 4(%r9), %ebp
	roll	$1, %ebp

	xor	%rbp, %rbx	

	
	
	movzbl	%al, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rax

	
	movzbl	%al, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rax

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	0(%r9), %rbx
	xor	%r12, %rbx

	
	
	movzbl	%bl, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rbx

	
	movzbl	%bl, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rbx

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	8(%r9), %rax
	xor	%r12, %rax

	
	
	movzbl	%al, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rax

	
	movzbl	%al, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rax

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	16(%r9), %rbx
	xor	%r12, %rbx

	
	
	movzbl	%bl, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rbx

	
	movzbl	%bl, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rbx

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	24(%r9), %rax
	xor	%r12, %rax

	
	
	movzbl	%al, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rax

	
	movzbl	%al, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%ah, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rax

	
	movzbl	%al, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%ah, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rax

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	32(%r9), %rbx
	xor	%r12, %rbx
 
	
	
	movzbl	%bl, %ebp
	movl	(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	3072(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	movl	3072(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	2048(%rsi,%rbp,4), %r11d
	rol	$16, %rbx

	
	movzbl	%bl, %ebp
	xorl	2048(%rsi,%rbp,4), %r12d
	movzbl	%bh, %ebp
	xorl	1024(%rsi,%rbp,4), %r12d
	ror	$32, %rbx

	
	movzbl	%bl, %ebp
	xorl	1024(%rsi,%rbp,4), %r11d
	movzbl	%bh, %ebp
	xorl	(%rsi,%rbp,4), %r11d
	ror	$16, %rbx

	
	
	xorl	%r11d, %r12d
	rorl	$8, %r11d
	xorl	%r12d, %r11d
	shl	$32, %r12
	or	%r11, %r12
	xor	40(%r9), %rax
	xor	%r12, %rax


	sub 	$8, %r10	
	ja	.Lround_loop

	bswap	%rax
	mov	%rax, 8(%rcx)
	xor	48(%r9), %rbx
	bswap	%rbx
	mov	%rbx, (%rcx)
	add	$16, %rcx
	sub	$16, %rdx

	ja	.Lblock_loop

	pop	%r12
	pop	%rbp
	pop	%rbx
.Lend:
	
    
  
  
	ret



