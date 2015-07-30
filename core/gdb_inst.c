// Copyright (c) 2013-2014 Cloudozer LLP. All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
// 
// * Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// 
// * Redistributions in any form must be accompanied by information on how to
// obtain complete source code for the LING software and any accompanying
// software that uses the LING software. The source code must either be included
// in the distribution or be available for no more than the cost of distribution
// plus a nominal fee, and must be freely redistributable under reasonable
// conditions.  For an executable file, complete source code means the source
// code for all modules it contains. It does not include source code for modules
// or files that typically accompany the major components of the operating
// system on which the executable file runs.
// 
// THIS SOFTWARE IS PROVIDED BY CLOUDOZER LLP ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE
// DISCLAIMED. IN NO EVENT SHALL CLOUDOZER LLP BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

//
// Gdb helper routines
//

#ifdef LING_DEBUG

#include "ling_common.h"

#include "term.h"
#include "stringify.h"
#include "snprintf.h"
#include "code_base.h"
#include "string.h"
#include "atoms.h"

extern void *int_code_end_label;

void __ts(uint32_t *ip, uint32_t *cp, term_t *sp, term_t *sbot)
{
	//term_t *sbot = proc_stack_bottom(proc);

	uint32_t *fi = backstep_to_func_info(ip);
	if (fi == 0)
	{
		printk("No current function\n");
		return;
	}
	else
		printk("* %pt:%pt/%d +%ld\n", T(fi[1]), T(fi[2]), fi[3], ip-fi);

	if (cp == 0)	// after allocate, before call/apply
	{
		cp = demasquerade_pointer(sp[0]);
		while (++sp < sbot)
			if (is_boxed(*sp) && is_cp(peel_boxed(*sp)))
				break;
	}

	do {
		if (cp[0] == shrink_ptr(int_code_end_label))
			break;
		uint32_t *fi = backstep_to_func_info(cp);

		printk("  %pt:%pt/%d\n", T(fi[1]), T(fi[2]), fi[3]);

		cp = demasquerade_pointer(sp[0]);
		while (++sp < sbot)
			if (is_boxed(*sp) && is_cp(peel_boxed(*sp)))
				break;
	} while (sp < sbot);
}

const char *__t(term_t t)
{
	static char buf[512];
	term_to_str(t, buf, sizeof(buf));
	return buf;
}

void __da(uint32_t *ip)
{
	if (((unsigned long)ip & 3) != 0)
		printk("da: misaligned ptr\n");
	module_info_t *module = code_base_module_by_ip(ip);
	if (module == 0)
		printk("da: not a code\n");

	uint32_t *fi = backstep_to_func_info(ip);
	// 0:func_info 1:module 2:function 3:arity
	uint32_t offset = ip - fi;

	printk("---- %pt:%pt/%d +%d:\n", T(fi[1]), T(fi[2]), fi[3], offset);

	for (int n = 0; n < 16; n++)
	{
		opcode_info_t *oi = opcode_lookup(expand_ptr(*ip));
		if (oi == 0) {
			printk("bad op: 0x%08x\n", *ip);
			ip++;
		}
		else
		{
			printk("%s", oi->var_name);
			for (int i = 1; i <= oi->arg_size; i++)
				printk(" 0x%08x", ip[i]);
			printk("\n");
			ip += oi->arg_size +1;
		}
	}
}

void __ih(heap_t *hp)
{
	printk("---- heap %d total_size\n", hp->total_size);
	memnode_t *node = hp->nodes;
	uint32_t total_size = 0;
	while (node != 0)
	{
		uint32_t *th = (node == &hp->init_node)
			? hp->init_node_threshold
			: NODE_THRESHOLD(node);
			
		uint32_t size = node->starts - th;
		printk("node 0x%pp - 0x%pp used %d\n", th, node->ends, size);

		total_size += size;
		node = node->next;
	}
	printk("total node size %d\n", total_size);
}

// GDB needs malloc/free to prepare arguments for calls
void *malloc(size_t size)
{
	memnode_t *node = nalloc(size);
	void *ptr = (char *)node + sizeof(memnode_t);
	debug("GDB malloc(%ld) -> %p\n", size, ptr);
	return ptr;
}

void free(void *ptr)
{
	if (ptr == 0)
		return;
	debug("GDB free(*%p)\n", ptr);
	memnode_t *node = (memnode_t *)(ptr - sizeof(memnode_t));
	nfree(node);
}

#endif	//LING_DEBUG

void gdb_break(void)
{
}

//EOF
