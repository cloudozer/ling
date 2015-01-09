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

#include "code_base.h"

#include "ling_common.h"

#include "bif.h"
#include "heap.h"
#include "getput.h"
#include "string.h"
#include "ext_term.h"
#include "atoms.h"
#include "atom_defs.h"
#include "catch_tab.h"
#include "hash.h"
#include "limits.h"

#include "code_base.inc"

static hash_t *exports_map;
static hash_t *modules_map;

// A chain of node for dynamically added export entries
static memnode_t *exports_nodes = 0;

// Literal nodes of purged modules
static memnode_t *orphaned_literals = 0;

#define EXP_NODE_SIZE	4096

static int decode_literals(module_info_t *mi,
	   			uint8_t *enc_lits, term_t *literals);

void code_base_init(void)
{
	exports_map = hash_make();

	// TODO: add custom hash functions and devise a better function for exports

	for (int i = 0; i < NUM_PRE_EXPS; i++)
	{
		//NB: dependency on export_t layout, also below
		hash_set(exports_map,
			 preloaded_exports+i, 3*sizeof(uint32_t),
			 preloaded_exports+i);
	}

	modules_map = hash_make();
	for (int i = 0; i < NUM_PRE_MODS; i++)
	{
		hash_set(modules_map,
			preloaded_modules+i, 2*sizeof(uint32_t),
			preloaded_modules+i);
	}

	//exports_nodes = 0;
}

export_t *code_base_lookup(term_t m, term_t f, int arity)
{
	export_t key = {.module=m, .function=f, .arity=arity};

	return (export_t *)hash_get(exports_map, &key, 3*sizeof(uint32_t));
}

//#define TRACE_LOOKUP_OR_CREATE
export_t *code_base_lookup_or_create_N(term_t m, term_t f, int arity)
{
#ifdef TRACE_LOOKUP_OR_CREATE
	const char *action = "found";
#endif
	export_t key = {.module=m, .function=f, .arity=arity};

	export_t *exp = (export_t *)hash_get(exports_map, &key, 3*sizeof(uint32_t));
	if (exp == 0)
	{
		// add a new entry
		if (exports_nodes == 0 ||
				(exports_nodes->ends - exports_nodes->starts) < WSIZE(export_t))
		{
			memnode_t *node = nalloc_N(EXP_NODE_SIZE - sizeof(memnode_t));
			if (node == 0)
				return 0;
			node->next = exports_nodes;
			exports_nodes = node;
		}

		exp = (export_t *)exports_nodes->starts;

		exp->module = m;
		exp->function = f;
		exp->arity = arity;

		int x = hash_set_N(exports_map, exp, 3*sizeof(uint32_t), exp);
		if (x < 0)
			return 0;

		// updated after hash_set() to take into account possible 'no memory'
		exports_nodes->starts += WSIZE(export_t);

		exp->is_bif = 0;
		exp->entry = 0;

#ifdef TRACE_LOOKUP_OR_CREATE
		action = "created";
#endif
	}

#ifdef TRACE_LOOKUP_OR_CREATE
	printk("lookup_or_create: %t:%t/%d %s 0x%08x\r\n", m, f, arity, action, exp);
#endif
	return exp;
}

export_t *code_base_lookup_bif_by_entry(void *entry)
{
	// happens on exceptions only - speed does not matter
	export_t *ptr = preloaded_exports;
	while (ptr < preloaded_exports + NUM_BIF_EXPS)
	{
		if (ptr->entry == entry)
			return ptr;
		ptr++;
	}

	return 0;
}

module_info_t *code_base_module_by_ip(uint32_t *ip)
{
	hash_index_t hi;
	hash_start(modules_map, &hi);
	module_info_t *module;
	while ((module = (module_info_t *)hash_next(&hi)) != 0) {
		if (ip >= module->code_starts &&
			ip < module->code_starts + module->code_size)
		return module;
	}
	return 0;
}

module_info_t *code_base_module_by_name(term_t mod, int is_old)
{
	module_info_t mi = {.name = mod, .is_old=is_old};
	return (module_info_t *)hash_get(modules_map, &mi, 2*sizeof(uint32_t));
}

uint32_t code_base_source_line(uint32_t *ip, char *fname, int size)
{
	module_info_t *module = code_base_module_by_ip(ip);
	if (module == 0 || module->line_refs == 0)
		return 0;

	uint32_t offset = ip - module->code_starts;
	line_info_t *alpha = module->line_refs;
	line_info_t *beta = alpha +module->num_line_refs;
	if (offset < alpha->offset)
		return 0;
	while (beta -alpha > 1)
	{
		line_info_t *mid = alpha + (beta -alpha) /2;
		if (offset < mid->offset)
			beta = mid;
		else
			alpha = mid;
	}

	//assert(beta -alpha == 1);
	uint32_t loc = alpha->location;

	int file_index = loc_file(loc);
	if (file_index == 0)
	{
		uint8_t *s = atoms_get(atom_index(module->name));
		if (s[0] +4 +1 > size)
			fname[0] = 0;
		else
		{
			memcpy(fname, s+1, s[0]);
			char *ptr = fname + s[0];
			*ptr++ = '.';
			*ptr++ = 'e';
			*ptr++ = 'r';
			*ptr++ = 'l';
			*ptr = 0;
		}
	}
	else
	{
		uint8_t *s = atoms_get(atom_index(module->file_names[file_index-1]));
		memcpy(fname, s+1, s[0]);
		*(fname + s[0]) = 0;
	}

	return loc_line(loc);
}

// The code of preloaded modules is defined within a scope of proc_main() to
// make opcode labels visible. An unfortunate consequence is that the code is
// not visible outside the function. The following routine is called in the very
// beginning of proc_main() to update entries of the initial export table and
// the funs table of each preloaded module. In addition the literal terms are
// fixed.

void module_fix_preloaded_code(module_info_t *mi,
						uint32_t *code_starts, uint32_t code_size,
						uint32_t *lit_fixups[], int num_fixups)
{
	mi->code_starts = code_starts;
	mi->code_size = code_size;

	uint8_t *enc_lits = (uint8_t *)literals_blob_start + mi->lit_blob_offset;

	term_t literals[mi->num_lits];
	decode_literals(mi, enc_lits, literals);	//TODO: return value ignored

	for (int i = 0; i < num_fixups; i++)
	{
		uint32_t index = *lit_fixups[i];
		*lit_fixups[i] = (uint32_t) literals[index];
	}

	for (int i = 0; i < mi->num_funs; i++)
	{
		unsigned long offset = (unsigned long)mi->funs_table[i].entry;
		mi->funs_table[i].entry = code_starts + offset;
	}

	for (int i = mi->exp_start_index; i < mi->exp_end_index; i++)
	{
		unsigned long offset = (unsigned long)preloaded_exports[i].entry;
		preloaded_exports[i].entry = code_starts + offset;
	}
}

//void set_entry_range(int start, int end, uint32_t *entry)
//{
//	for (int i = start; i < end; i++)
//		preloaded_exports[i].entry = entry;
//}

static int decode_literals(module_info_t *mi,
	   			uint8_t *enc_lits, term_t *literals)
{
	if (mi->num_lits == 0)
	{
		mi->lit_node = 0;	// no literal pool
		return 0; // Success
	}

	struct {
		uint8_t *blob_ptr;
		int enc_size;
		int heap_size;
	} literal_table[mi->num_lits];

	int total_heap_size = 0;

	uint8_t *ptr = enc_lits;
	for (int i = 0; i < mi->num_lits; i++)
	{
		//TODO: be more cautious here as the code
		// is used for dynamic code loading too

		int enc_size = GET_UINT_32(ptr);
		ptr += 4;
		int heap_size = ext_term_decode_size(ptr, enc_size, 0);
		if (heap_size < 0)
			fatal_error("bad literals blob [index %d enc_size %d]", i, enc_size);

		literal_table[i].blob_ptr = ptr;
		literal_table[i].enc_size = enc_size;
		literal_table[i].heap_size = heap_size;

		total_heap_size += heap_size;
		ptr += enc_size;
	}

	mi->lit_node = nalloc_N(total_heap_size*sizeof(uint32_t));
	if (mi->lit_node == 0)
		return -NO_MEMORY;

	// the size of the node may be larger then total_heap_size; tighten up
	// the node to make it easier to find references inside the literals pool.
	mi->lit_node->starts = mi->lit_node->ends - total_heap_size;

	heap_init(&mi->lit_heap, mi->lit_node->starts, mi->lit_node->ends);
	uint32_t *htop = heap_alloc(&mi->lit_heap, total_heap_size);

	for (int i = 0; i < mi->num_lits; i++)
	{
		int heap_size = literal_table[i].heap_size;
		literals[i] = ext_term_decode(htop,
			heap_size,
			literal_table[i].blob_ptr,
		    literal_table[i].enc_size,
			&mi->lit_heap.proc_bins,
			&mi->lit_heap.total_pb_size,
			0);	// !safe
		htop += heap_size;
	}

	// the literal heap is all full
	heap_set_top(&mi->lit_heap, htop);
	return 0; // Success;
}

//
//
//
void code_base_retire(term_t mod_name)
{
#ifdef LING_DEBUG
	// the retired version, if any should be purged
	module_info_t *retired = code_base_module_by_name(mod_name, 1);
	assert(retired == 0);
#endif
	module_info_t *module = code_base_module_by_name(mod_name, 0);
	assert(module != 0);

	// Change the version to 'old'
	hash_set(modules_map, module, 2*sizeof(uint32_t), 0);
	module->is_old = 1;
	hash_set(modules_map, module, 2*sizeof(uint32_t), module);

	// Clean up the exports table: preloaded_exports and exports_nodes
	export_t *ptr = preloaded_exports + NUM_BIF_EXPS;
	export_t *end = preloaded_exports + NUM_PRE_EXPS;
	while (ptr < end)
	{
		if (ptr->module == mod_name)
			ptr->entry = 0;
		ptr++;
	}

	memnode_t *node = exports_nodes;
	while (node != 0)
	{
		ptr = (export_t *)NODE_THRESHOLD(node);
		end = (export_t *)node->starts;
		while (ptr < end)
		{
			if (ptr->module == mod_name)
				ptr->entry = 0;
			ptr++;
		}

		node = node->next;
	}

	//NB: funs keep references to the retired module code
}

//
// Removes the old version of the module
//

void code_base_purge(module_info_t *module)
{
	assert(module->is_old);

	// Clean up the exports table: preloaded_exports and exports_nodes
	export_t *ptr = preloaded_exports + NUM_BIF_EXPS;
	export_t *end = preloaded_exports + NUM_PRE_EXPS;
	while (ptr < end)
	{
		if (!ptr->is_bif &&
			(uint32_t *)ptr->entry > module->code_starts &&
		    (uint32_t *)ptr->entry <= module->code_starts +module->code_size)
		{
			assert(ptr->module == module->name);
			ptr->entry = 0;
		}
		ptr++;
	}

	memnode_t *node = exports_nodes;
	while (node != 0)
	{
		ptr = (export_t *)NODE_THRESHOLD(node);
		end = (export_t *)node->starts;
		while (ptr < end)
		{
			if (!ptr->is_bif &&
				(uint32_t *)ptr->entry > module->code_starts &&
				(uint32_t *)ptr->entry <= module->code_starts +module->code_size)
			{
				assert(ptr->module == module->name);
				ptr->entry = 0;
			}
			ptr++;
		}

		node = node->next;
	}

	// cleanup catch table
	catches_remove_block(module->catch_block_base,
						 module->catch_block_size);

	// forget about the module (version)
	hash_set(modules_map, module, 2*sizeof(uint32_t), 0);

	// Running processes can have references to the module's literals node and
	// in most cases they will. Keep the literals node of the purge module. Such
	// nodes can be garbage collected later, if needed.
	//
	if (module->lit_node != 0)
	{
		module->lit_node->next = orphaned_literals;
		orphaned_literals = module->lit_node;
		module->lit_node = 0;
	}

	nfree(module->my_node);
}

//
// Loads a module from a binary
//
 
int code_base_load_N(term_t mod_name, uint8_t *ling_data, int data_size)
{
#ifdef LING_DEBUG
	// the previous version should be retired first
	module_info_t *existing = code_base_module_by_name(mod_name, 0);
	assert(existing == 0);
#endif

#define ATOMS_CHUNK		MAKE_UINT_32('A', 't', 'o', 'm')
#define ATOMS_IDX		0
#define EXPORTS_CHUNK	MAKE_UINT_32('E', 'x', 'p', 'T')
#define EXPORTS_IDX		1
#define IMPORTS_CHUNK	MAKE_UINT_32('I', 'm', 'p', 'T')
#define IMPORTS_IDX		2
#define CODE_CHUNK		MAKE_UINT_32('C', 'o', 'd', 'e')
#define CODE_IDX		3
#define CATCHES_CHUNK	MAKE_UINT_32('C', 'a', 't', 'T')
#define CATCHES_IDX		4
#define LAMBDAS_CHUNK	MAKE_UINT_32('F', 'u', 'n', 'T')
#define LAMBDAS_IDX		5
#define STRTAB_CHUNK	MAKE_UINT_32('S', 't', 'r', 'T')
#define STRTAB_IDX		6
#define LITERALS_CHUNK	MAKE_UINT_32('L', 'i', 't', 'T')
#define LITERALS_IDX	7
#define ATTRS_CHUNK		MAKE_UINT_32('A', 't', 't', 'r')
#define ATTRS_IDX		8
#define CINFS_CHUNK		MAKE_UINT_32('C', 'I', 'n', 'f')
#define CINFS_IDX		9
#define LINE_CHUNK		MAKE_UINT_32('L', 'i', 'n', 'e')
#define LINE_IDX		10
#define ABST_CHUNK		MAKE_UINT_32('A', 'b', 's', 't')

#define MAX_CHUNKS		11

	//debug("Loading module %pt...", T(mod_name));

	struct {
		uint8_t *data;
		int size;
	} chunks[MAX_CHUNKS];

	memset(chunks, 0, sizeof(chunks));

	int left = data_size;
	uint8_t *p = ling_data;

	if (left < 12)
		return -1;

	if (GET_UINT_32(p) != MAKE_UINT_32('F', 'O', 'R', '1'))
	{
		debug("No IFF FOR1 tag found\r\n");
		return -1;
	}
	left -= 4;
	p += 4;

	uint32_t form_size = GET_UINT_32(p);
	left -= 4;
	p += 4;

	if (form_size != left)
	{
		debug("IFF form size mismatch: found %d, expected %d\r\n", form_size, left);
		return -1;
	}

	if (GET_UINT_32(p) != MAKE_UINT_32('L', 'I', 'N', 'G'))
	{
		debug("No LING tag found\r\n");
		return -1;
	}
	left -= 4;
	p += 4;

	while (left > 0)
	{
		if (left < 8)
			return -1;

		uint32_t chunk_name = GET_UINT_32(p);
		left -= 4;
		p += 4;

		uint32_t chunk_size = GET_UINT_32(p);
		left -= 4;
		p += 4;

		uint32_t aligned_size = (chunk_size + 3) & ~3;
		if (aligned_size > left)
			return -1;
		uint8_t *chunk_data = p;
		left -= aligned_size;
		p += aligned_size;

		if (chunk_name == ATOMS_CHUNK)
		{
			if (chunks[ATOMS_IDX].data != 0)
			{
				debug("Duplicate atoms chunk found\r\n");
				return -1;
			}
			chunks[ATOMS_IDX].data = chunk_data;
			chunks[ATOMS_IDX].size = chunk_size;
		}
		else if (chunk_name == EXPORTS_CHUNK)
		{
			if (chunks[EXPORTS_IDX].data != 0)
			{
				debug("Duplicate exports chunk found\r\n");
				return -1;
			}
			chunks[EXPORTS_IDX].data = chunk_data;
			chunks[EXPORTS_IDX].size = chunk_size;
		}
		else if (chunk_name == IMPORTS_CHUNK)
		{
			if (chunks[IMPORTS_IDX].data != 0)
			{
				debug("Duplicate imports chunk found\r\n");
				return -1;
			}
			chunks[IMPORTS_IDX].data = chunk_data;
			chunks[IMPORTS_IDX].size = chunk_size;
		}
		else if (chunk_name == CODE_CHUNK)
		{
			if (chunks[CODE_IDX].data != 0)
			{
				debug("Duplicate code chunk found\r\n");
				return -1;
			}
			chunks[CODE_IDX].data = chunk_data;
			chunks[CODE_IDX].size = chunk_size;
		}
		else if (chunk_name == CATCHES_CHUNK)
		{
			if (chunks[CATCHES_IDX].data != 0)
			{
				debug("Duplicate catches chunk found\r\n");
				return -1;
			}
			chunks[CATCHES_IDX].data = chunk_data;
			chunks[CATCHES_IDX].size = chunk_size;
		}
		else if (chunk_name == LAMBDAS_CHUNK)
		{
			if (chunks[LAMBDAS_IDX].data != 0)
			{
				debug("Duplicate lambdas chunk found\r\n");
				return -1;
			}
			chunks[LAMBDAS_IDX].data = chunk_data;
			chunks[LAMBDAS_IDX].size = chunk_size;
		}
		else if (chunk_name == STRTAB_CHUNK)
		{
			if (chunks[STRTAB_IDX].data != 0)
			{
				debug("Duplicate string chunk found\r\n");
				return -1;
			}
			chunks[STRTAB_IDX].data = chunk_data;
			chunks[STRTAB_IDX].size = chunk_size;
		}
		else if (chunk_name == LITERALS_CHUNK)
		{
			if (chunks[LITERALS_IDX].data != 0)
			{
				debug("Duplicate literals chunk found\r\n");
				return -1;
			}
			chunks[LITERALS_IDX].data = chunk_data;
			chunks[LITERALS_IDX].size = chunk_size;
		}

		else if (chunk_name == ATTRS_CHUNK)
		{
			if (chunks[ATTRS_IDX].data != 0)
			{
				debug("Duplicate attributes chunk found\r\n");
				return -1;
			}
			chunks[ATTRS_IDX].data = chunk_data;
			chunks[ATTRS_IDX].size = chunk_size;
		}
		else if (chunk_name == CINFS_CHUNK)
		{
			if (chunks[CINFS_IDX].data != 0)
			{
				debug("Duplicate compile info chunk found\r\n");
				return -BAD_ARG;
			}
			chunks[CINFS_IDX].data = chunk_data;
			chunks[CINFS_IDX].size = chunk_size;
		}
		else if (chunk_name == LINE_CHUNK)
		{
			if (chunks[LINE_IDX].data != 0)
			{
				debug("Duplicate line info chunk found\r\n");
				return -BAD_ARG;
			}
			chunks[LINE_IDX].data = chunk_data;
			chunks[LINE_IDX].size = chunk_size;
		}
		else if (chunk_name == ABST_CHUNK)
		{
			//skip
		}
		else
		{
			debug("Unknown chunk found: type 0x%08x, size %d\r\n",
				   				chunk_name, chunk_size);
			return -BAD_ARG;
		}
	}

	if (chunks[ATOMS_IDX].data == 0 ||
		chunks[EXPORTS_IDX].data == 0 ||
		chunks[IMPORTS_IDX].data == 0 ||
		chunks[CODE_IDX].data == 0 ||
		chunks[CATCHES_IDX].data == 0 ||
		chunks[STRTAB_IDX].data == 0 ||
		chunks[LITERALS_IDX].data == 0)
	{
		debug("Mandatory chunk not found\r\n");
		return -BAD_ARG;
	}

	//-------- Atoms --------
	
	p = chunks[ATOMS_IDX].data;
	left = chunks[ATOMS_IDX].size;

	if (left < 4)
		return -BAD_ARG;

	uint32_t nr_atoms = GET_UINT_32(p);
	p += 4;
	left -= 4;

	term_t atoms[nr_atoms];

	int i = 0;
	while (left > 0)
	{
		int name_size = p[0];
		if (name_size+1 > left)
			return -BAD_ARG;
		atoms[i] = tag_atom(atoms_set(p));

		p += name_size+1;
		left -= name_size+1;
		i++;
	}

	if (i != nr_atoms)
		return -BAD_ARG;

	//-------- Imports --------

	p = chunks[IMPORTS_IDX].data;
	left = chunks[IMPORTS_IDX].size;

	if (left < 4)
		return -BAD_ARG;

	uint32_t nr_imports = GET_UINT_32(p);
	p += 4;
	left -= 4;

	export_t *imports[nr_imports];

	i = 0;
	while (left > 0)
	{
		if (left < 12)
			return -BAD_ARG;
		uint32_t a_mod = GET_UINT_32(p);
		uint32_t a_fun = GET_UINT_32(p+4);
		uint32_t arity = GET_UINT_32(p+8);
		p += 12;
		left -= 12;

		if (a_mod >= nr_atoms || a_fun >= nr_atoms)
			return -BAD_ARG;

		term_t m = atoms[a_mod];
		term_t f = atoms[a_fun];

		imports[i] = code_base_lookup_or_create_N(m, f, arity);
		if (imports[i] == 0)
			return -NO_MEMORY;
		i++;
	}

	if (i != nr_imports)
		return -BAD_ARG;

	//-------- Lambdas - 1 --------
	
	p = chunks[LAMBDAS_IDX].data;
	left = chunks[LAMBDAS_IDX].size;

	if (left < 4)
		return -BAD_ARG;

	uint32_t nr_lambdas = GET_UINT_32(p);

	//-------- Code - 1 --------
	
	p = chunks[CODE_IDX].data;
	left = chunks[CODE_IDX].size;

	if (left < 4)
		return -BAD_ARG;

	uint32_t code_size = GET_UINT_32(p);

	//-------- Line info --------
	
	uint32_t nr_line_refs = 0;
	uint32_t nr_file_names = 0;

	if (chunks[LINE_IDX].data != 0)
	{
		p = chunks[LINE_IDX].data;
		left = chunks[LINE_IDX].size;

		if (left < 2 *4)
			return -BAD_ARG;
		
		nr_line_refs = GET_UINT_32(p);
		p += 4;
		nr_file_names = GET_UINT_32(p);
	}

	//-------- Allocating module_info_t --------
	//
	// There must be enough space for module_info_t struct and:
	// - the code
	// - the fun table
	// - the string table
	// - the attributes blob
	// - the compile info blob
	// - the source line table
	// - the source file names
	//
	
	uint32_t node_size = sizeof(module_info_t)
		+ nr_line_refs * sizeof(line_info_t)
		+ nr_file_names * sizeof(term_t)
		+ WALIGN(chunks[STRTAB_IDX].size)
		+ WALIGN(chunks[ATTRS_IDX].size)
		+ WALIGN(chunks[CINFS_IDX].size)
		+ nr_lambdas *sizeof(fun_entry_t)
		+ code_size *sizeof(uint32_t);

	memnode_t *node = nalloc_N(node_size);
	if (node == 0)
		return -NO_MEMORY;

	//
	// NB: any error hereinafter must nfree the node
	//

	//TODO: check if the sequence of areas matters for caches
	
	module_info_t *module = (module_info_t *)node->starts;
	node->starts = (uint32_t *)((uint8_t *)node->starts + sizeof(module_info_t));

	uint8_t *str_space = (uint8_t *)node->starts;
	node->starts = (uint32_t *)((uint8_t *)node->starts + WALIGN(chunks[STRTAB_IDX].size));

	uint8_t *attrs_blob = (uint8_t *)node->starts;
	node->starts = (uint32_t *)((uint8_t *)node->starts + WALIGN(chunks[ATTRS_IDX].size));

	uint8_t *cinfo_blob = (uint8_t *)node->starts;
	node->starts = (uint32_t *)((uint8_t *)node->starts + WALIGN(chunks[CINFS_IDX].size));

	fun_entry_t *funs_table = (fun_entry_t *)node->starts;
	node->starts = (uint32_t *)(funs_table + nr_lambdas);

	line_info_t *line_refs = (line_info_t *)node->starts;
	node->starts = (uint32_t *)(line_refs + nr_line_refs);

	term_t *file_names = (term_t *)node->starts;
	node->starts = (uint32_t *)(file_names + nr_file_names);

	uint32_t *code_starts = node->starts;
	assert(node->starts + code_size <= node->ends);

	memset(module, 0, sizeof(module_info_t));

	module->name = mod_name;
	//module->is_old = 0;
	
	module->my_node = node;		// is allocated dynamically

	module->code_starts = code_starts;
	module->code_size = code_size;

	module->num_funs = nr_lambdas;
	module->funs_table = funs_table;

	//-------- Catches - 1 --------

	p = chunks[CATCHES_IDX].data;
	left = chunks[CATCHES_IDX].size;

	if (left < 4)
	{
		nfree(node);
		return -BAD_ARG;
	}

	uint32_t nr_catches = GET_UINT_32(p);
	p += 4;
	left -= 4;

	uint32_t *catches[nr_catches];

	i = 0;
	while (left > 0)
	{
		if (left < 4)
		{
			nfree(node);
			return -BAD_ARG;
		}

		uint32_t offset = GET_UINT_32(p);
		p += 4;
		left -= 4;

		catches[i++] = code_starts + offset;
	}

	if (i != nr_catches)
	{
		nfree(node);
		return -BAD_ARG;
	}

	module->catch_block_base = catches_next_base();
	module->catch_block_size = nr_catches;

	//-------- Literals --------
	
	p = chunks[LITERALS_IDX].data;
	left = chunks[LITERALS_IDX].size;

	if (left < 4)
	{
		nfree(node);
		return -BAD_ARG;
	}

	uint32_t nr_lits = GET_UINT_32(p);
	p += 4;
	//left -= 4;

	// decode_literals() is used by statically loaded modules too;
	// in that case .num_lists is set in the inializer (code_base.inc)
	module->num_lits = nr_lits;

	term_t literals[nr_lits];
	int x = decode_literals(module, p, literals);
	if (x < 0)
	{
		assert(module->lit_node == 0);
		nfree(node);
		return x;
	}

	//-------- String table --------

	p = chunks[STRTAB_IDX].data;
	left = chunks[STRTAB_IDX].size;

	memcpy(str_space, p, left);
	module->str_space = str_space;

	//-------- Attributes --------
	
	p = chunks[ATTRS_IDX].data;
	left = chunks[ATTRS_IDX].size;

	memcpy(attrs_blob, p, left);
	module->attrs_data = attrs_blob;
	module->attrs_size = left;

	//-------- Compile info --------

	p = chunks[CINFS_IDX].data;
	left = chunks[CINFS_IDX].size;

	memcpy(cinfo_blob, p, left);
	module->cinfo_data = cinfo_blob;
	module->cinfo_size = left;

	//-------- Line info --------
	
	p = chunks[LINE_IDX].data +8;
	left = chunks[LINE_IDX].size -8;

	for (int i = 0; i < nr_line_refs; i++)
	{
		if (left < 2 *4)
			return -BAD_ARG;
		uint32_t offset = GET_UINT_32(p);
		p += 4;
		uint32_t location = GET_UINT_32(p);
		p += 4;
		left -= 8;

		line_refs[i].offset = offset;
		line_refs[i].location = location;

		//debug("offset %d location %08x\r\n", offset, location);
	}

	for (int i = 0; i < nr_file_names; i++)
	{
		if (left < 4)
			return -BAD_ARG;
		uint32_t name = GET_UINT_32(p);
		p += 4;
		left -= 4;
		file_names[i] = atoms[name];
	}

	if (left > 0)
		return -BAD_ARG;

	module->line_refs = line_refs;
	module->num_line_refs = nr_line_refs;
	module->file_names = file_names;

	//-------- Lambdas - 2 --------
	
	p = chunks[LAMBDAS_IDX].data +4;	// skip nr_lambdas
	left = chunks[LAMBDAS_IDX].size -4;

	fun_entry_t *fe = funs_table;
	while (left > 0)
	{
		if (left < 6*sizeof(uint32_t))
		{
			nfree(node);
			return -BAD_ARG;
		}

		int a_fun = GET_UINT_32(p);
		p += 4;
		int arity = GET_UINT_32(p);
		p += 4;
		uint32_t offset = GET_UINT_32(p);
		p += 4;
		uint32_t index = GET_UINT_32(p);
		p += 4;
		int nfree = GET_UINT_32(p);
		p += 4;
		uint32_t ouniq = GET_UINT_32(p);
		p += 4;
		left -= 6*4;

		fe->arity = arity;
		fe->module = mod_name;
		fe->index = index;
		fe->uniq[0] = 0; //TODO
		fe->uniq[1] = 0; //TODO
		fe->uniq[2] = 0; //TODO
		fe->uniq[3] = 0; //TODO
		fe->old_index = index;
		fe->old_uniq = ouniq;
		fe->num_free = nfree;
		fe->name = atoms[a_fun];	// compiler-generated name 
		fe->entry = code_starts + offset;
		fe++;
	}

	if (fe != funs_table + nr_lambdas)
	{
		nfree(node);
		return -BAD_ARG;
	}

	//-------- Code - 2 --------

	p = chunks[CODE_IDX].data +4;	// skip code_size
	left = chunks[CODE_IDX].size -4;

	//TODO: check for overruns

#if LING_DEBUG
	int bif_not_impl = 0;
#endif

	uint32_t *c = code_starts;
	while (left > 0)
	{
		if (*p < 0x80)
		{
			opcode_info_t *oi = opcode_get(*p++);
			*c++ = shrink_ptr(oi->label);
			left--;
		}
		else if (*p < 0xe0)
		{
			switch(*p & 0xf0)
			{
			case 0x80:
				*c++ = reg_as_term(*p++ & 0xf);
				left--;
				break;
			case 0x90:
				*c++ = slot_as_term(*p++ & 0xf);
				left--;
				break;
			case 0xa0:
				*c++ = ((p[0] & 0xf) << 8) | p[1];
				p += 2;
				left -= 2;
				break;
			case 0xb0:
				*c++ = ((p[0] & 0xf) << 16) | (p[1] << 8) | p[2];
				p += 3;
				left -= 3;
				break;
			case 0xc0:
				*c++ = atoms[((p[0] & 0xf) << 8) | p[1]];
				p += 2;
				left -= 2;
				break;
			case 0xd0:
				*c++ = shrink_ptr(imports[((p[0] & 0xf) << 8) | p[1]]);
				p += 2;
				left -= 2;
				break;
			}
		}
		else if (*p < 0xf4)
		{
			int u10 = ((p[0] & 0x3) << 8) | p[1];

			switch(*p & 0xfc)
			{
			case 0xe0:
				*c++ = literals[u10];
				break;
			case 0xe4:
#ifdef LING_DEBUG
				if (imports[u10]->entry == 0)
				{
					export_t *e = imports[u10];
					printk("BIF %pt:%pt/%d not implemented\r\n",
								T(e->module), T(e->function), e->arity);
					bif_not_impl = 1;
				}
#endif
				*c++ = shrink_ptr(imports[u10]->entry);
				break;
			case 0xe8:
			{
				opcode_info_t *oi = opcode_get(u10);
				*c++ = shrink_ptr(oi->label);
				break;
			}
			case 0xec:
			{
				int i = ((((int32_t) p[0]) << 30) >> 22) | p[1];
				*c++ = tag_int(i);
				break;
			}
			case 0xf0:
				*c++ = shrink_ptr(funs_table + u10);
				break;
			}

			p += 2;
			left -= 2;
		}
		else
		{
			switch(*p)
			{
			case 0xf4:
				*c++ = tag_catch(module->catch_block_base + p[1]);
				p += 2;
				left -= 2;
				break;
			case 0xf5:
				*c++ = shrink_ptr(str_space + p[1]);
				p += 2;
				left -= 2;	
				break;

			case 0xf6:
				/* {export,none} */
				*c++ = 0;
				p += 1;
				left -= 1;
				break;

			case 0xf7:
				/* unused */
				nfree(module->lit_node);
				nfree(module->my_node);
				return -1;

			case 0xf8:
				*c++ = tag_int((int32_t) GET_UINT_32(p+1));
				p += 5;
				left -= 5;
				break;
			case 0xf9:
				*c++ = GET_UINT_32(p+1);
				p += 5;
				left -= 5;
				break;
			case 0xfa:
				*c++ = reg_as_term(p[1]);
				p += 2;
				left -= 2;
				break;
			case 0xfb:
				*c++ = slot_as_term(p[1]);
				p += 2;
				left -= 2;
				break;
			case 0xfc:
				*c++ = nil;
				p += 1;
				left -= 1;
				break;
			case 0xfd:
				*c++ = 0;
				p += 1;
				left -= 1;
				break;
			case 0xfe:
				*c++ = shrink_ptr(code_starts + ((p[1] << 8) | p[2]));
				p += 3;
				left -= 3;
				break;
			case 0xff:
				// extended tag: rarely happens in the wild
				
				p += 1;
				left -= 1;

				uint32_t u32 = GET_UINT_32(p+1);
				//debug("Unlikely extended tag found: subtag %d index %d\r\n", p[0], u32);

				switch(p[0])
				{
				case 0:
					*c++ = shrink_ptr(code_starts + u32);
					break;
				case 1:
					*c++ = atoms[u32];
					break;
				case 2:
					*c++ = shrink_ptr(imports[u32]);
					break;
				case 3:
					*c++ = literals[u32];
					break;
				case 4:
					*c++ = shrink_ptr(imports[u32]->entry);
					break;
				case 5:
					*c++ = shrink_ptr(funs_table + u32);
					break;
				case 6:
					*c++ = tag_catch(module->catch_block_base + u32);
					break;
				case 7:
					*c++ = shrink_ptr(str_space + u32);
					break;
				case 8:
					*c++ = slot_as_term(u32);
					break;
				case 9:
				{
					opcode_info_t *oi = opcode_get(u32);
					*c++ = shrink_ptr(oi->label);
					break;
				}
				default:
					/* unused */
					nfree(module->lit_node);
					nfree(module->my_node);
					return -BAD_ARG;
				}

				p += 5;
				left -= 5;
			}
		}
	}

#ifdef LING_DEBUG
	assert(bif_not_impl == 0);
#endif

	//-------- Catches - 2 --------
	
	catches_append_block(catches, nr_catches);

	//-------- Update export table --------

	p = chunks[EXPORTS_IDX].data;
	left = chunks[EXPORTS_IDX].size;

	if (left < 4)
	{
		catches_remove_block(module->catch_block_base, nr_catches);
		nfree(module->lit_node);
		nfree(module->my_node);
		return -BAD_ARG;
	}

	uint32_t nr_exports = GET_UINT_32(p);
	p += 4;
	left -= 4;

	struct {
		export_t *e;
		uint32_t *code;
	} exports[nr_exports];

	i = 0;
	while (left > 0)
	{
		if (left < 12)
			break;

		uint32_t a_fun = GET_UINT_32(p);
		uint32_t arity = GET_UINT_32(p+4);
		uint32_t offset = GET_UINT_32(p+8);

		//debug("export: %pt/%d offset %d\r\n", T(atoms[a_fun]), arity, offset);

		if (a_fun >= nr_atoms || arity > 255 || offset >= code_size)
			break;

		export_t *e = code_base_lookup_or_create_N(mod_name, atoms[a_fun], arity);
		if (e == 0)
			break;

		if (e->entry != 0)
		{
			assert(e->is_bif);

			// in R16B01 many BIFs are reimplemented as NIFs
			//
			//debug("Warning: %pt:%pt/%d is implemented as BIF\n",
			//				T(mod_name), T(atoms[a_fun]), arity);
		}

		exports[i].e = e;
		exports[i].code = code_starts + offset;
		i++;

		p += 12;
		left -= 12;
	}

	if (left > 0 || i != nr_exports)
	{
		catches_remove_block(module->catch_block_base, nr_catches);
		nfree(module->lit_node);
		nfree(module->my_node);
		return -BAD_ARG;
	}

	for (i = 0; i < nr_exports; i++)
	{
		// Functions that reimplement BIFs are ignored
		if (!exports[i].e->is_bif)
			exports[i].e->entry = exports[i].code;
	}

	x = hash_set_N(modules_map, module, 2*sizeof(uint32_t), module);
	if (x < 0)
	{
		catches_remove_block(module->catch_block_base, nr_catches);
		nfree(module->lit_node);
		nfree(module->my_node);
		return x;
	}

	//debug("done\r\n");
	return 0;
}

uint32_t code_base_estimate_memory(void)
{
	uint32_t mem_size = 0;
	mem_size += sizeof(preloaded_exports);

	mem_size += hash_mem_size(exports_map);
	mem_size += hash_mem_size(modules_map);
	memnode_t *node = exports_nodes;
	while (node != 0)
	{
		mem_size += node->index *PAGE_SIZE;
		node = node->next;
	}
	hash_index_t hi;
	hash_start(modules_map, &hi);
	module_info_t *m;
	while ((m = hash_next(&hi)) != 0)
	{
		if (m->my_node != 0)
			mem_size += m->my_node->index *PAGE_SIZE;
		else
		{
			mem_size += m->code_size *sizeof(uint32_t);
			mem_size += m->attrs_size;
			mem_size += m->cinfo_size;
			mem_size += m->num_line_refs *sizeof(line_info_t);
		}
	}
	mem_size += sizeof(preloaded_modules);
	mem_size += (literals_blob_end - literals_blob_start);

	// literals of purged modules
	node = orphaned_literals;
	while (node != 0)
	{
		mem_size += node->index *PAGE_SIZE;
		node = node->next;
	}

	return mem_size;
}

term_t code_base_list_module_exports(term_t mod, heap_t *hp)
{
	term_t ms = nil;
	hash_index_t hi;
	hash_start(exports_map, &hi);
	export_t *exp;
	while ((exp = hash_next(&hi)) != 0)
	{
		// Check that the export table entry is not empty. Importing unmodified
		// Erlang/OTP modules sometimes results in dangling entries. Not
		// checking used to make error_handler to loop forever.
		//
		if (exp->module == mod && exp->entry != 0)
		{
			// EXCEPTION POSSIBLE
			term_t t = heap_tuple2(hp, exp->function, tag_int(exp->arity));
			// EXCEPTION POSSIBLE
			ms = heap_cons(hp, t, ms);
		}
	}
	return ms;
}

term_t code_base_list_all_loaded(heap_t *hp)
{
	hash_index_t hi;
	hash_start(modules_map, &hi);
	term_t all = nil;
	module_info_t *m;
	while ((m = hash_next(&hi)) != 0)
	{
		if (m->is_old && code_base_module_by_name(m->name, 0) != 0)
			continue;	// only include old module if there is no new code

		term_t src = (m->my_node == 0) ?A_PRELOADED :A_EMBEDDED;
		all = heap_cons(hp, heap_tuple2(hp, m->name, src), all);
	}
	return all;
}

term_t code_base_list_loaded(heap_t *hp)
{
	hash_index_t hi;
	hash_start(modules_map, &hi);
	term_t loaded = nil;
	module_info_t *m;
	while ((m = hash_next(&hi)) != 0)
	{
		if (m->is_old && code_base_module_by_name(m->name, 0) != 0)
			continue;	// only include old module if there is no new code

		loaded = heap_cons(hp, m->name, loaded);
	}
	return loaded;
}

term_t code_base_list_pre_loaded(heap_t *hp)
{
	hash_index_t hi;
	hash_start(modules_map, &hi);
	term_t pre = nil;
	module_info_t *m;
	while ((m = hash_next(&hi)) != 0)
	{
		if (m->my_node != 0)
			continue;	// not preloaded
		if (m->is_old && code_base_module_by_name(m->name, 0) != 0)
			continue;	// only include old module if there is no new code

		pre = heap_cons(hp, m->name, pre);
	}
	return pre;
}

#ifdef RUNTIME_METRICS
void print_loaded_module_sizes(void)
{
	hash_index_t hi;
	hash_start(modules_map, &hi);
	module_info_t *module;
	printk("\n================ loaded module sizes =============\n");
	while ((module = (module_info_t *)hash_next(&hi)) != 0)
		printk("%pt %d\n", T(module->name), module->code_size);
		
	printk("NB: sizes are in 32-bit words\n");		
}
#endif //RUNTIME_METRICS

//EOF
