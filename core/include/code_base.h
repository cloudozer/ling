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

#pragma once

#include <stdint.h>

#include "term.h"
#include "heap.h"

typedef struct opcode_info_t opcode_info_t;
struct opcode_info_t {
	void *label;
#if defined(LING_DEBUG) || defined(COUNT_IOPS) || defined(RUNTIME_METRICS)
	const char *var_name;
#endif
	int arg_size;
#ifdef COUNT_IOPS
	uint64_t counter;
#endif
};

opcode_info_t *opcode_lookup(void *label);
opcode_info_t *opcode_get(uint32_t n);

uint32_t *backstep_to_func_info(uint32_t *ip);

extern uint8_t literals_blob_start[];
extern uint8_t literals_blob_end[];

#define BIF_TYPE_NONE		0
#define BIF_TYPE_CALL		1
#define	BIF_TYPE_NORMAL_0	2
#define	BIF_TYPE_NORMAL_1	3
#define	BIF_TYPE_NORMAL_2	4
#define	BIF_TYPE_GC_1		5
#define	BIF_TYPE_GC_2		6
#define	BIF_TYPE_GC_3		7

//typedef struct export_t export_t;		-- typedefed in term.h
struct export_t {
	//NB: the first three fields are used as a hash key
	term_t module;
	term_t function;
	int32_t arity;		// export key size is fixed

	int is_bif;  // BIF_TYPE_
	void *entry; // (uint32_t *) or (bif_func_t)
};

//typedef struct fun_entry_t fun_entry_t;
struct fun_entry_t {
	int arity;
	term_t module;
	uint32_t index;
	uint32_t uniq[4];
	int old_index;
	int old_uniq;
	int num_free;

	// added reluctantly - eunit parses this
	term_t name;

	uint32_t *entry;
};

#define make_loc(file, line) 	((file) << 24 | line)
#define loc_file(loc)			((loc) >> 24)
#define loc_line(loc)			((loc) & ((1 << 24) -1))

typedef struct line_info_t line_info_t;
struct line_info_t {
	uint32_t offset;
	uint32_t location;	// findex:8,line:24
};

typedef struct module_info_t module_info_t;
struct module_info_t {
	term_t name;	//NB: hash key do not move
	int32_t is_old;	// key size is 8

	uint32_t *code_starts;
	uint32_t code_size;

	// fun table
	int num_funs;
	fun_entry_t *funs_table;

	// initial exports table range
	int exp_start_index;
	int exp_end_index;

	// when literals are decoded their total heap requirement
	// is determined and lit_node is allocated to house all
	// literals. The literal heap uses lit_node as its initial
	// buffer and thus it nevers allocates more nodes.
	//
	memnode_t *lit_node;
	heap_t lit_heap;

	// initial literal blob range
	int lit_blob_offset;
	int num_lits;
	term_t *lits_table;

	// string table
	uint8_t *str_space;

	// attributes blob
	uint8_t *attrs_data;
	uint32_t attrs_size;

	// compile info blob
	uint8_t *cinfo_data;
	uint32_t cinfo_size;

	// catch block
	uint32_t catch_block_base;
	uint32_t catch_block_size;

	// line info
	line_info_t *line_refs;
	uint32_t num_line_refs;
	term_t *file_names;

	// != 0 -> the module is dynamically allocated
	memnode_t *my_node;
};

#include "mod_info.inc"

extern export_t preloaded_exports[];
extern module_info_t preloaded_modules[];

void code_base_init(void);
export_t *code_base_lookup(term_t m, term_t f, int arity);
export_t *code_base_lookup_or_create_N(term_t m, term_t f, int arity);
export_t *code_base_lookup_bif_by_entry(void *entry);
module_info_t *code_base_module_by_ip(uint32_t *ip);
module_info_t *code_base_module_by_name(term_t mod, int is_old);

uint32_t code_base_source_line(uint32_t *ip, char *fname, int size);

void module_fix_preloaded_code(module_info_t *mi,
						uint32_t *code_starts, uint32_t code_size,
						uint32_t *lit_fixups[], int num_fixups);
void code_base_dont_fix_anymore(void);
int code_base_fixed_already(void);

void code_base_retire(term_t mod_name);
void code_base_purge(module_info_t *module);

term_t code_base_all_embedded(heap_t *hp);
uint8_t *code_base_embedded_bin(term_t mod_name, int *size);
int code_base_load_N(term_t mod_name, uint8_t *ling_data, int data_size);

uint32_t code_base_estimate_memory(void);
term_t code_base_list_module_exports(term_t mod, heap_t *hp);
term_t code_base_list_all_loaded(heap_t *hp);	// code:all_loaded()
term_t code_base_list_loaded(heap_t *hp);		// erlang:loaded()
term_t code_base_list_pre_loaded(heap_t *hp);	// erlang:pre_loaded()

//EOF
