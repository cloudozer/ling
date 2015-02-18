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

.global _start
.global reset_handler
.global undef_handler
.global swi_handler
.global prefetch_handler
.global data_abort_handler
.global irq_handler
.global fiq_handler

.global _stack_bottom, _irq_stack_bottom
.global _vectors_start, _vectors_end

_start:
		ldr	sp, =_stack_bottom
		bl setup_vectors
		mrs r0, cpsr
		bic r1, r0, #0x1f
		orr r1, r1, #0x12
		msr cpsr, r1
		ldr sp, =_irq_stack_bottom
		bic r0, r0, #0x80
		msr cpsr, r0

		@ enable VFP
		mrc p15, 0, r0, c1, c0, 2
		orr r0, r0, #0xf00000
		mcr p15, 0, r0, c1, c0, 2 
		mov r0, #0x40000000       
		fmxr fpexc,r0           

		bl	start_ling
		b	.

_vectors_start:
		ldr pc, vector0
		ldr	pc, vector1
		ldr pc, vector2
		ldr pc, vector3
		ldr pc, vector4
		b .
		ldr pc, vector6
		ldr pc, vector7

vector0: .word _start
vector1: .word undef_handler
vector2: .word swi_handler
vector3: .word prefetch_abort_handler
vector4: .word data_abort_handler
vector6: .word irq_handler
vector7: .word fiq_handler
_vectors_end:

;@EOF
