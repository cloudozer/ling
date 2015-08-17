/*
  Copyright (c) 2013-2014 Cloudozer LLP. All rights reserved.
  
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  
  * Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
  
  * Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
  
  * Redistributions in any form must be accompanied by information on how to
  obtain complete source code for the LING software and any accompanying
  software that uses the LING software. The source code must either be included
  in the distribution or be available for no more than the cost of distribution
  plus a nominal fee, and must be freely redistributable under reasonable
  conditions.  For an executable file, complete source code means the source
  code for all modules it contains. It does not include source code for modules
  or files that typically accompany the major components of the operating
  system on which the executable file runs.
  
  THIS SOFTWARE IS PROVIDED BY CLOUDOZER LLP ``AS IS'' AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE
  DISCLAIMED. IN NO EVENT SHALL CLOUDOZER LLP BE LIABLE FOR ANY DIRECT,
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "xen/xen.h"

#include "limits.h"

	.section __xen_guest
	
	.ascii	"GUEST_OS=Ling"
	.ascii	",XEN_VER=xen-3.0"
	.ascii	",VIRT_BASE=0x0"
	.ascii	",ELF_PADDR_OFFSET=0x0"
	.ascii	",HYPERCALL_PAGE=0x1"
#if !defined(__x86_64__)
	.ascii	",PAE=yes"
#endif
	.ascii	",LOADER=generic"
	.byte	0

/*
	NB: for x86_64, even if there is no PAE=yes indicated above, PAE is still
	used but in 32-bit mode.
*/
	
	.text

	.globl	_start, hypercall_page
	.globl	shared_info
	.globl	grant_table
	
	.org	0x0000
_start:

#if defined(__x86_64__)

	cld
	movq stack_data(%rip), %rsp
	andq $(~(STACK_SIZE -1)), %rsp
	movq %rsi, %rdi
	call start_ling

stack_data:
	.quad stack +2*STACK_SIZE

#else /*__x86_64__*/
	
	cld
	lss stack_data, %esp
	andl $(~(STACK_SIZE-1)), %esp
	push %esi
	call start_ling

stack_data:
	.long stack + 2*STACK_SIZE, FLAT_KERNEL_SS

#endif

	.org	0x1000
hypercall_page:

	.org	0x2000
shared_info:

	.org	0x3000
grant_table:

	.org	0x4000
	.end

