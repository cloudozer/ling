




































 







 



	.file "memxor.asm"

	.text

	
	
	.align 4


.globl _memxor
_memxor:
	
    
  
  
	mov	%rdx, %r10
	mov	%rdi, %rdx
	jmp 	.Lmemxor3_entry


	
	
	.align 4

	
.globl _memxor3
_memxor3:
	
    
  
  
	
	mov	%rcx, %r10
.Lmemxor3_entry:
	test	%r10, %r10
	
	
	mov	%rdi, %rax
	jz	.Ldone
	add 	%r10, %rdi
	and	$7, %rdi
	
	jz	.Laligned

	cmp	$8, %r10
	jc	.Lfinal_next

	
	
	
	
.Lalign_loop:
	
	sub	$1, %r10
	movb	(%rsi, %r10), %r8b
	xorb	(%rdx, %r10), %r8b
	movb	%r8b, (%rax, %r10)
	sub	$1, %rdi
	jnz	.Lalign_loop

.Laligned:

	
	
	mov	%rsi, %r8
	sub	%rdx, %r8
	test	$7, %r8
	jnz	.Lno_shift_case
	mov	%rsi, %rcx
	sub	%rax, %rcx
	and	$7, %rcx
	jz	.Lno_shift_case
	sub	%rcx, %rsi
	sub	%rcx, %rdx
	shl	$3, %rcx

	
	test	$8, %r10
	jnz	.Lshift_odd
	mov	(%rsi, %r10), %rdi
	xor	(%rdx, %r10), %rdi
	jmp	.Lshift_next

.Lshift_odd:
	mov	-8(%rsi, %r10), %rdi
	mov	(%rsi, %r10), %r11
	xor	-8(%rdx, %r10), %rdi
	xor	(%rdx, %r10), %r11
	mov	%rdi, %r8
	shr	%cl, %r8
	neg	%cl
	shl	%cl, %r11
	neg	%cl
	
	or	%r11, %r8
	mov	%r8, -8(%rax, %r10)
	sub	$8, %r10
	jz	.Ldone
	jmp 	.Lshift_next

	.align 4


.Lshift_loop:
	mov	8(%rsi, %r10), %r11
	xor	8(%rdx, %r10), %r11
	mov	%r11, %r8
	shr	%cl, %r8
	neg	%cl
	shl	%cl, %rdi
	neg	%cl
	or	%rdi, %r8
	mov	%r8, 8(%rax, %r10)

	mov	(%rsi, %r10), %rdi
	xor	(%rdx, %r10), %rdi
	mov	%rdi, %r8
	shr	%cl, %r8
	neg	%cl
	shl	%cl, %r11
	neg 	%cl
	or	%r11, %r8
	mov	%r8, (%rax, %r10)
.Lshift_next:
	sub	$16, %r10
	
	


	jnc	.Lshift_loop

	add	$15, %r10
	jnc	.Ldone

	shr	$3, %rcx
	add	%rcx, %rsi
	add	%rcx, %rdx
	jmp	.Lfinal_loop
	
.Lno_shift_case:
	
	
	test	$8, %r10
	jz	.Lword_next

	sub	$8, %r10
	jz	.Lone_word

	mov	(%rsi, %r10), %r8
	xor	(%rdx, %r10), %r8
	mov	%r8, (%rax, %r10)
	
	jmp	.Lword_next

	.align 4


.Lword_loop:
	mov	8(%rsi, %r10), %r8
	mov	(%rsi, %r10), %r9
	xor	8(%rdx, %r10), %r8
	xor	(%rdx, %r10), %r9
	mov	%r8, 8(%rax, %r10)
	mov	%r9, (%rax, %r10)

.Lword_next:
	sub	$16, %r10
	ja	.Lword_loop	
	jnz	.Lfinal

	
	mov	8(%rsi, %r10), %r8
	xor	8(%rdx, %r10), %r8
	mov	%r8, 8(%rax, %r10)
	
.Lone_word:
	mov	(%rsi, %r10), %r8
	xor	(%rdx, %r10), %r8
	mov	%r8, (%rax, %r10)

	
	
    
  
  
	ret

.Lfinal:
	add	$15, %r10

.Lfinal_loop:
	movb	(%rsi, %r10), %r8b
	xorb	(%rdx, %r10), %r8b
	movb	%r8b, (%rax, %r10)
.Lfinal_next:
	sub	$1, %r10
	jnc	.Lfinal_loop

.Ldone:
	
	
    
  
  
	ret

	
	




