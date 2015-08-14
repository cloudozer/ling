








































	







































	.file "sha1-compress.asm"

	
	
	.text
	.align 4

.globl __nettle_sha1_compress
__nettle_sha1_compress:
	
	
    
  
  
	
	sub	$68, %rsp	

	
	
	movl	(%rsi), %eax
	bswap	%eax
	movl	%eax,  (%rsp)
 
	movl	4(%rsi), %r8d
	bswap	%r8d
	movl	%r8d, 4 (%rsp)
 
	movl	8(%rsi), %ecx
	bswap	%ecx
	movl	%ecx, 8 (%rsp)
 
	movl	12(%rsi), %edx
	bswap	%edx
	movl	%edx, 12 (%rsp)

	
	movl	16(%rsi), %eax
	bswap	%eax
	movl	%eax, 16 (%rsp)
 
	movl	20(%rsi), %r8d
	bswap	%r8d
	movl	%r8d, 20 (%rsp)
 
	movl	24(%rsi), %ecx
	bswap	%ecx
	movl	%ecx, 24 (%rsp)
 
	movl	28(%rsi), %edx
	bswap	%edx
	movl	%edx, 28 (%rsp)

	
	movl	32(%rsi), %eax
	bswap	%eax
	movl	%eax, 32 (%rsp)
 
	movl	36(%rsi), %r8d
	bswap	%r8d
	movl	%r8d, 36 (%rsp)
 
	movl	40(%rsi), %ecx
	bswap	%ecx
	movl	%ecx, 40 (%rsp)
 
	movl	44(%rsi), %edx
	bswap	%edx
	movl	%edx, 44 (%rsp)

	
	movl	48(%rsi), %eax
	bswap	%eax
	movl	%eax, 48 (%rsp)
 
	movl	52(%rsi), %r8d
	bswap	%r8d
	movl	%r8d, 52 (%rsp)
 
	movl	56(%rsi), %ecx
	bswap	%ecx
	movl	%ecx, 56 (%rsp)
 
	movl	60(%rsi), %edx
	bswap	%edx
	movl	%edx, 60 (%rsp)


	
	movl	  (%rdi), %eax
	movl	 4(%rdi), %r8d
	movl	 8(%rdi), %ecx
	movl	12(%rdi), %edx
	movl	16(%rdi), %r9d

	movl	$0x5A827999, %esi
	
	addl	%esi, %r9d
	addl	(%rsp), %r9d
	
	movl	%edx, %r10d
	xorl	%ecx, %r10d
	andl	%r8d, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	addl	%esi, %edx
	addl	4 (%rsp), %edx
	
	movl	%ecx, %r10d
	xorl	%r8d, %r10d
	andl	%eax, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	addl	%esi, %ecx
	addl	8 (%rsp), %ecx
	
	movl	%r8d, %r10d
	xorl	%eax, %r10d
	andl	%r9d, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	addl	%esi, %r8d
	addl	12 (%rsp), %r8d
	
	movl	%eax, %r10d
	xorl	%r9d, %r10d
	andl	%edx, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	addl	%esi, %eax
	addl	16 (%rsp), %eax
	
	movl	%r9d, %r10d
	xorl	%edx, %r10d
	andl	%ecx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	addl	%esi, %r9d
	addl	20 (%rsp), %r9d
	
	movl	%edx, %r10d
	xorl	%ecx, %r10d
	andl	%r8d, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	addl	%esi, %edx
	addl	24 (%rsp), %edx
	
	movl	%ecx, %r10d
	xorl	%r8d, %r10d
	andl	%eax, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	addl	%esi, %ecx
	addl	28 (%rsp), %ecx
	
	movl	%r8d, %r10d
	xorl	%eax, %r10d
	andl	%r9d, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	addl	%esi, %r8d
	addl	32 (%rsp), %r8d
	
	movl	%eax, %r10d
	xorl	%r9d, %r10d
	andl	%edx, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	addl	%esi, %eax
	addl	36 (%rsp), %eax
	
	movl	%r9d, %r10d
	xorl	%edx, %r10d
	andl	%ecx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	addl	%esi, %r9d
	addl	40 (%rsp), %r9d
	
	movl	%edx, %r10d
	xorl	%ecx, %r10d
	andl	%r8d, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	addl	%esi, %edx
	addl	44 (%rsp), %edx
	
	movl	%ecx, %r10d
	xorl	%r8d, %r10d
	andl	%eax, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	addl	%esi, %ecx
	addl	48 (%rsp), %ecx
	
	movl	%r8d, %r10d
	xorl	%eax, %r10d
	andl	%r9d, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	addl	%esi, %r8d
	addl	52 (%rsp), %r8d
	
	movl	%eax, %r10d
	xorl	%r9d, %r10d
	andl	%edx, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	addl	%esi, %eax
	addl	56 (%rsp), %eax
	
	movl	%r9d, %r10d
	xorl	%edx, %r10d
	andl	%ecx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	addl	%esi, %r9d
	addl	60 (%rsp), %r9d
	
	movl	%edx, %r10d
	xorl	%ecx, %r10d
	andl	%r8d, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d,  (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%ecx, %r10d
	xorl	%r8d, %r10d
	andl	%eax, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	4 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 4 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r8d, %r10d
	xorl	%eax, %r10d
	andl	%r9d, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	8 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 8 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%eax, %r10d
	xorl	%r9d, %r10d
	andl	%edx, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	12 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 12 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%r9d, %r10d
	xorl	%edx, %r10d
	andl	%ecx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	movl	$0x6ED9EBA1, %esi
	
	movl	16 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 16 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r10d
	xorl	%ecx, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	20 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 20 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r10d
	xorl	%r8d, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	24 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 24 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r10d
	xorl	%eax, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	28 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 28 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r10d
	xorl	%r9d, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	32 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 32 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r10d
	xorl	%edx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	movl	36 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 36 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r10d
	xorl	%ecx, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	40 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 40 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r10d
	xorl	%r8d, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	44 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 44 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r10d
	xorl	%eax, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	48 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 48 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r10d
	xorl	%r9d, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	52 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 52 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r10d
	xorl	%edx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	movl	56 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 56 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r10d
	xorl	%ecx, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	60 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 60 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r10d
	xorl	%r8d, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d,  (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r10d
	xorl	%eax, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	4 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 4 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r10d
	xorl	%r9d, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	8 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 8 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r10d
	xorl	%edx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	movl	12 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 12 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r10d
	xorl	%ecx, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	16 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 16 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r10d
	xorl	%r8d, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	20 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 20 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r10d
	xorl	%eax, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	24 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 24 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r10d
	xorl	%r9d, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	28 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 28 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r10d
	xorl	%edx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	movl	$0x8F1BBCDC, %esi
	
	movl	32 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 32 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r11d
	andl	%ecx, %r11d
	movl	%r8d, %r10d
	orl	%ecx, %r10d
	andl	%edx, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	36 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 36 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r11d
	andl	%r8d, %r11d
	movl	%eax, %r10d
	orl	%r8d, %r10d
	andl	%ecx, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	40 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 40 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r11d
	andl	%eax, %r11d
	movl	%r9d, %r10d
	orl	%eax, %r10d
	andl	%r8d, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	44 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 44 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r11d
	andl	%r9d, %r11d
	movl	%edx, %r10d
	orl	%r9d, %r10d
	andl	%eax, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	48 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 48 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r11d
	andl	%edx, %r11d
	movl	%ecx, %r10d
	orl	%edx, %r10d
	andl	%r9d, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	movl	52 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 52 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r11d
	andl	%ecx, %r11d
	movl	%r8d, %r10d
	orl	%ecx, %r10d
	andl	%edx, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	56 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 56 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r11d
	andl	%r8d, %r11d
	movl	%eax, %r10d
	orl	%r8d, %r10d
	andl	%ecx, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	60 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 60 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r11d
	andl	%eax, %r11d
	movl	%r9d, %r10d
	orl	%eax, %r10d
	andl	%r8d, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d,  (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r11d
	andl	%r9d, %r11d
	movl	%edx, %r10d
	orl	%r9d, %r10d
	andl	%eax, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	4 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 4 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r11d
	andl	%edx, %r11d
	movl	%ecx, %r10d
	orl	%edx, %r10d
	andl	%r9d, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	movl	8 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 8 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r11d
	andl	%ecx, %r11d
	movl	%r8d, %r10d
	orl	%ecx, %r10d
	andl	%edx, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	12 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 12 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r11d
	andl	%r8d, %r11d
	movl	%eax, %r10d
	orl	%r8d, %r10d
	andl	%ecx, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	16 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 16 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r11d
	andl	%eax, %r11d
	movl	%r9d, %r10d
	orl	%eax, %r10d
	andl	%r8d, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	20 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 20 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r11d
	andl	%r9d, %r11d
	movl	%edx, %r10d
	orl	%r9d, %r10d
	andl	%eax, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	24 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 24 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r11d
	andl	%edx, %r11d
	movl	%ecx, %r10d
	orl	%edx, %r10d
	andl	%r9d, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	movl	28 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 28 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r11d
	andl	%ecx, %r11d
	movl	%r8d, %r10d
	orl	%ecx, %r10d
	andl	%edx, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	32 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 32 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r11d
	andl	%r8d, %r11d
	movl	%eax, %r10d
	orl	%r8d, %r10d
	andl	%ecx, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	36 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 36 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r11d
	andl	%eax, %r11d
	movl	%r9d, %r10d
	orl	%eax, %r10d
	andl	%r8d, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	40 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 40 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r11d
	andl	%r9d, %r11d
	movl	%edx, %r10d
	orl	%r9d, %r10d
	andl	%eax, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	44 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 44 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r11d
	andl	%edx, %r11d
	movl	%ecx, %r10d
	orl	%edx, %r10d
	andl	%r9d, %r10d
	orl	%r11d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	movl	$0xCA62C1D6, %esi
	
	movl	48 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 48 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r10d
	xorl	%ecx, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	52 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 52 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r10d
	xorl	%r8d, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	56 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 56 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r10d
	xorl	%eax, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	60 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 60 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r10d
	xorl	%r9d, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d,  (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r10d
	xorl	%edx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	movl	4 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 4 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r10d
	xorl	%ecx, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	8 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 8 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r10d
	xorl	%r8d, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	12 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 12 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r10d
	xorl	%eax, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	16 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 16 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r10d
	xorl	%r9d, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	20 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 20 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r10d
	xorl	%edx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	movl	24 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 24 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r10d
	xorl	%ecx, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	28 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 28 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r10d
	xorl	%r8d, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	32 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 32 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r10d
	xorl	%eax, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	36 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 36 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r10d
	xorl	%r9d, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	40 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	xorl	8 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 40 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r10d
	xorl	%edx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	movl	44 (%rsp), %r10d
	xorl	52 (%rsp), %r10d
	xorl	12 (%rsp), %r10d
	xorl	32 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 44 (%rsp) 
	addl	%esi, %r9d
	addl	%r10d, %r9d
	
	movl	%r8d, %r10d
	xorl	%ecx, %r10d
	xorl	%edx, %r10d
	addl	%r10d, %r9d



	movl	%eax, %r10d
	roll	$5, %r10d
	addl	%r10d, %r9d
	roll	$30, %r8d
	
	movl	48 (%rsp), %r10d
	xorl	56 (%rsp), %r10d
	xorl	16 (%rsp), %r10d
	xorl	36 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 48 (%rsp) 
	addl	%esi, %edx
	addl	%r10d, %edx
	
	movl	%eax, %r10d
	xorl	%r8d, %r10d
	xorl	%ecx, %r10d
	addl	%r10d, %edx



	movl	%r9d, %r10d
	roll	$5, %r10d
	addl	%r10d, %edx
	roll	$30, %eax
	
	movl	52 (%rsp), %r10d
	xorl	60 (%rsp), %r10d
	xorl	20 (%rsp), %r10d
	xorl	40 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 52 (%rsp) 
	addl	%esi, %ecx
	addl	%r10d, %ecx
	
	movl	%r9d, %r10d
	xorl	%eax, %r10d
	xorl	%r8d, %r10d
	addl	%r10d, %ecx



	movl	%edx, %r10d
	roll	$5, %r10d
	addl	%r10d, %ecx
	roll	$30, %r9d
	
	movl	56 (%rsp), %r10d
	xorl	 (%rsp), %r10d
	xorl	24 (%rsp), %r10d
	xorl	44 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 56 (%rsp) 
	addl	%esi, %r8d
	addl	%r10d, %r8d
	
	movl	%edx, %r10d
	xorl	%r9d, %r10d
	xorl	%eax, %r10d
	addl	%r10d, %r8d



	movl	%ecx, %r10d
	roll	$5, %r10d
	addl	%r10d, %r8d
	roll	$30, %edx
	
	movl	60 (%rsp), %r10d
	xorl	4 (%rsp), %r10d
	xorl	28 (%rsp), %r10d
	xorl	48 (%rsp), %r10d
	roll	$1, %r10d
	movl	%r10d, 60 (%rsp) 
	addl	%esi, %eax
	addl	%r10d, %eax
	
	movl	%ecx, %r10d
	xorl	%edx, %r10d
	xorl	%r9d, %r10d
	addl	%r10d, %eax



	movl	%r8d, %r10d
	roll	$5, %r10d
	addl	%r10d, %eax
	roll	$30, %ecx

	
	addl	%eax,   (%rdi) 
	addl	%r8d,  4(%rdi) 
	addl	%ecx,  8(%rdi) 
	addl	%edx, 12(%rdi) 
	addl	%r9d, 16(%rdi)

	add	$68, %rsp
	
    
  
  
	ret



