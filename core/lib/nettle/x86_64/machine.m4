C OFFSET(i)
C Expands to 4*i, or to the empty string if i is zero
define(<OFFSET>, <ifelse($1,0,,eval(4*$1))>)

C OFFSET64(i)
C Expands to 8*i, or to the empty string if i is zero
define(<OFFSET64>, <ifelse($1,0,,eval(8*$1))>)

dnl LREG(reg) gives the 8-bit register corresponding to the given 64-bit register.
define(<LREG>,<ifelse(
	$1, %rax, %al,
	$1, %rbx, %bl,
	$1, %rcx, %cl,
	$1, %rdx, %dl,
	$1, %rsi, %sil,
	$1, %rdi, %dil,
	$1, %rbp, %bpl,
	$1, %r8, %r8b,
	$1, %r9, %r9b,
	$1, %r10, %r10b,
	$1, %r11, %r11b,
	$1, %r12, %r12b,
	$1, %r13, %r13b,
	$1, %r14, %r14b,
	$1, %r15, %r15b)>)dnl

define(<HREG>,<ifelse(
	$1, %rax, %ah,
	$1, %rbx, %bh,
	$1, %rcx, %ch,
	$1, %rdx, %dh)>)dnl

define(<WREG>,<ifelse(
	$1, %rax, %ax,
	$1, %rbx, %bx,
	$1, %rcx, %cx,
	$1, %rdx, %dx,
	$1, %rsi, %si,
	$1, %rdi, %di,
	$1, %rbp, %bp,
	$1, %r8, %r8w,
	$1, %r9, %r9w,
	$1, %r10, %r10w,
	$1, %r11, %r11w,
	$1, %r12, %r12w,
	$1, %r13, %r13w,
	$1, %r14, %r14w,
	$1, %r15, %r15w)>)dnl

define(<XREG>,<ifelse(
	$1, %rax, %eax,
	$1, %rbx, %ebx,
	$1, %rcx, %ecx,
	$1, %rdx, %edx,
	$1, %rsi, %esi,
	$1, %rdi, %edi,
	$1, %rbp, %ebp,
	$1, %r8, %r8d,
	$1, %r9, %r9d,
	$1, %r10, %r10d,
	$1, %r11, %r11d,
	$1, %r12, %r12d,
	$1, %r13, %r13d,
	$1, %r14, %r14d,
	$1, %r15, %r15d)>)dnl

dnl W64_ENTRY(nargs, xmm_used)
define(<W64_ENTRY>, <
  changequote([,])dnl
  ifelse(<<<<<<<<<<<<<<<< ignored; only for balancing)
  ifelse(W64_ABI,yes,[
    ifelse(eval($2 > 6), 1, [
      sub	[$]eval(8 + 16*($2 - 6)), %rsp
      movdqu	%xmm6, 0(%rsp)
    ])
    ifelse(eval($2 > 7), 1, [
      movdqu	%xmm7, 16(%rsp)
    ])
    ifelse(eval($2 > 8), 1, [
      movdqu	%xmm8, 32(%rsp)
    ])
    ifelse(eval($2 > 9), 1, [
      movdqu	%xmm9, 48(%rsp)
    ])
    ifelse(eval($2 > 10), 1, [
      movdqu	%xmm10, 64(%rsp)
    ])
    ifelse(eval($2 > 11), 1, [
      movdqu	%xmm11, 80(%rsp)
    ])
    ifelse(eval($2 > 12), 1, [
      movdqu	%xmm12, 96(%rsp)
    ])
    ifelse(eval($2 > 13), 1, [
      movdqu	%xmm13, 112(%rsp)
    ])
    ifelse(eval($2 > 14), 1, [
      movdqu	%xmm14, 128(%rsp)
    ])
    ifelse(eval($2 > 15), 1, [
      movdqu	%xmm15, 144(%rsp)
    ])
    ifelse(eval($1 >= 1), 1, [
      push	%rdi
      mov	%rcx, %rdi
    ])
    ifelse(eval($1 >= 2), 1, [
      push	%rsi
      mov	%rdx, %rsi
    ])
    ifelse(eval($1 >= 3), 1, [
      mov	%r8, %rdx
    ])
    ifelse(eval($1 >= 4), 1, [
      mov	%r9, %rcx
    ])
    ifelse(eval($1 >= 5), 1, [
      ifelse(eval($2 > 6), 1, [
        mov	eval(8 + 16*($2 - 6) + 56)(%rsp), %r8
      ], [
        mov	56(%rsp), %r8
      ])
    ])
  ])
  changequote(<,>)dnl
>)

dnl W64_EXIT(nargs, xmm_used)
define(<W64_EXIT>, <
  changequote([,])dnl
  ifelse(<<<<<<<<<<<< ignored; only for balancing)
  ifelse(W64_ABI,yes,[
    ifelse(eval($1 >= 2), 1, [
      pop	%rsi
    ])
    ifelse(eval($1 >= 1), 1, [
      pop	%rdi
    ])
    ifelse(eval($2 > 15), 1, [
      movdqu	144(%rsp), %xmm15
    ])
    ifelse(eval($2 > 14), 1, [
      movdqu	128(%rsp), %xmm14
    ])
    ifelse(eval($2 > 13), 1, [
      movdqu	112(%rsp), %xmm13
    ])
    ifelse(eval($2 > 12), 1, [
      movdqu	96(%rsp), %xmm12
    ])
    ifelse(eval($2 > 11), 1, [
      movdqu	80(%rsp), %xmm11
    ])
    ifelse(eval($2 > 10), 1, [
      movdqu	64(%rsp), %xmm10
    ])
    ifelse(eval($2 > 9), 1, [
      movdqu	48(%rsp), %xmm9
    ])
    ifelse(eval($2 > 8), 1, [
      movdqu	32(%rsp), %xmm8
    ])
    ifelse(eval($2 > 7), 1, [
      movdqu	16(%rsp), %xmm7
    ])
    ifelse(eval($2 > 6), 1, [
      movdqu	0(%rsp), %xmm6
      add	[$]eval(8 + 16*($2 - 6)), %rsp
    ])
  ])
  changequote(<,>)dnl
>)
