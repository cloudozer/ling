%% Copyright (c) 2013-2014 Cloudozer LLP. All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%% 
%% * Redistributions of source code must retain the above copyright notice, this
%% list of conditions and the following disclaimer.
%% 
%% * Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
%% 
%% * Redistributions in any form must be accompanied by information on how to
%% obtain complete source code for the LING software and any accompanying
%% software that uses the LING software. The source code must either be included
%% in the distribution or be available for no more than the cost of distribution
%% plus a nominal fee, and must be freely redistributable under reasonable
%% conditions.  For an executable file, complete source code means the source
%% code for all modules it contains. It does not include source code for modules
%% or files that typically accompany the major components of the operating
%% system on which the executable file runs.
%% 
%% THIS SOFTWARE IS PROVIDED BY CLOUDOZER LLP ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE
%% DISCLAIMED. IN NO EVENT SHALL CLOUDOZER LLP BE LIABLE FOR ANY DIRECT,
%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


-module(ling_iopvars).
-export([var_order/0]).
-export([fit_args/2]).
-export([var_args/2]).
-export([var_index/2]).
-export([var_by_index/1]).

var_order() -> [
		{get_tuple_element,0},
	{move,0},
	{get_tuple_element,1},
	{move,3},
	{is_tuple_of_arity,1},
	{l_new_bs_put_integer_imm,0},
	{move,1},
	{l_call,0},
	{move2,0},
	{test_heap,0},
	{move,2},
	{l_put_tuple,0},
	{move2,1},
	{move2,3},
	{l_call_only,0},
	{get_list,0},
	{l_fetch,2},
	{is_nonempty_list,0},
	{l_allocate,1},
	{l_is_eq_exact,0},
	{move,4},
	{is_nonempty_list_allocate,1},
	{l_select_val2,0},
	{l_gc_bif1,0},
	{l_is_eq_exact_immed,2},
	{deallocate_return,1},
	{get_list,3},
	{l_allocate,0},
	{is_nonempty_list,2},
	{l_is_eq_exact_immed,3},
	{l_is_eq_exact_immed,0},
	{l_call_ext,110},
	{is_tuple_of_arity,0},
	{put_list,0},
	{l_call_fun,0},
	{l_bs_add,0},
	{l_bs_get_integer_small_imm,0},
	{l_fetch,0},
	{l_is_eq_exact_immed,1},
	{call_bif,8},
	{move_return,3},
	{l_is_eq_exact_immed,6},
	{deallocate_return,0},
	{put_list,1},
	{return,0},
	{extract_next_element,2},
	{is_nil,2},
	{extract_next_element,1},
	{l_bs_start_match2,0},
	{l_is_eq_exact_immed,5},
	{is_tuple,0},
	{l_trim,0},
	{extract_next_element2,1},
	{l_bs_get_integer_16,0},
	{deallocate_return,2},
	{l_call_last,0},
	{extract_next_element,4},
	{l_fetch,1},
	{l_select_val_atoms,0},
	{put_list,2},
	{move_return,5},
	{call_bif,47},
	{l_new_bs_put_binary_all,0},
	{is_nil,1},
	{l_bif2,0},
	{l_allocate,2},
	{l_is_eq_exact_immed,7},
	{move_deallocate_return,0},
	{move_return,1},
	{call_bif,7},
	{move,5},
	{l_fetch,18},
	{call_bif,16},
	{call_bif,15},
	{call_bif,14},
	{l_increment,0},
	{l_bs_init_heap_bin,0},
	{l_select_tuple_arity,0},
	{put_list,3},
	{l_select_tuple_arity2,0},
	{move_return,4},
	{l_fetch,4},
	{l_allocate,6},
	{l_call_last,6},
	{init2,0},
	{call_bif,9},
	{l_move_call,36},
	{l_new_bs_put_binary_imm,1},
	{l_make_fun,0},
	{is_nonempty_list,1},
	{l_call_ext,6},
	{l_call_ext_last,0},
	{l_is_ge,0},
	{extract_next_element2,0},
	{move_return,48},
	{l_allocate,3},
	{jump,0},
	{l_fetch,10},
	{is_atom,0},
	{l_bs_get_binary_imm2,0},
	{extract_next_element2,2},
	{is_tuple,2},
	{test_arity,0},
	{l_move_call_only,3},
	{l_catch,5},
	{l_bs_append,0},
	{l_select_tuple_arity2,1},
	{extract_next_element,0},
	{call_bif,41},
	{l_bs_init_fail,0},
	{l_is_function2,0},
	{l_allocate_zero,3},
	{l_loop_rec,0},
	{l_select_tuple_arity,1},
	{is_nil,0},
	{move,6},
	{extract_next_element3,0},
	{remove_message,0},
	{l_move_call_last,2},
	{allocate_init,0},
	{move,8},
	{l_allocate_zero,2},
	{l_allocate_zero,1},
	{move,11},
	{is_nil,3},
	{catch_end,5},
	{is_atom,1},
	{init,0},
	{l_bif1,0},
	{init,1},
	{l_select_val2,9},
	{apply_last,0},
	{l_bs_get_binary_all_reuse,0},
	{self,0},
	{deallocate_return,4},
	{l_fetch,17},
	{set_tuple_element,0},
	{l_bs_match_string,0},
	{l_allocate,10},
	{l_move_call_only,1},
	{l_select_val_smallints,0},
	{l_bs_get_integer_8,0},
	{move_deallocate_return,2},
	{is_list,2},
	{is_atom,2},
	{l_allocate_zero,0},
	{send,0},
	{allocate_heap,0},
	{l_is_eq_exact_immed,8},
	{l_trim,1},
	{move_deallocate_return,1},
	{init3,0},
	{deallocate_return,3},
	{extract_next_element3,2},
	{call_bif,36},
	{l_bif2,6},
	{is_integer,0},
	{move_return,0},
	{l_allocate,4},
	{l_bif2,4},
	{l_bif2,1},
	{is_atom,3},
	{l_call_last,4},
	{extract_next_element,6},
	{l_move_call,1},
	{move_deallocate_return,3},
	{l_move_call_ext_last,0},
	{l_bs_get_binary2,0},
	{l_is_eq_exact_literal,1},
	{l_call_ext,24},
	{l_move_call_ext,13},
	{l_fetch,13},
	{l_move_call_last,3},
	{l_new_bs_put_binary_imm,0},
	{l_minus,0},
	{l_call_last,1},
	{l_times,1},
	{l_bs_init_bits,0},
	{l_fetch,14},
	{l_increment,2},
	{l_times,0},
	{l_bs_get_integer_32,0},
	{l_bs_get_binary_all2,0},
	{deallocate_return,13},
	{l_move_call_last,0},
	{l_select_val2,6},
	{l_is_eq_exact_literal,5},
	{is_binary,2},
	{test_heap_1_put_list,0},
	{l_is_lt,0},
	{l_plus,0},
	{l_allocate_zero,4},
	{l_select_val2,2},
	{extract_next_element3,4},
	{l_select_val2,8},
	{call_bif,18},
	{is_integer,5},
	{is_binary,1},
	{deallocate_return,5},
	{l_catch,0},
	{l_trim,3},
	{is_pid,0},
	{l_select_val2,3},
	{is_nonempty_list,4},
	{l_call_last,3},
	{is_list,0},
	{extract_next_element3,7},
	{l_select_val2,12},
	{l_int_div,1},
	{l_fetch,8},
	{l_catch,2},
	{catch_end,2},
	{l_call_ext_last,1},
	{l_allocate_zero,5},
	{l_move_call_ext_only,10},
	{extract_next_element,5},
	{init,2},
	{try_end,4},
	{extract_next_element2,3},
	{is_tuple,1},
	{l_is_ne_exact_immed,2},
	{is_float,0},
	{try_end,1},
	{l_move_call,4},
	{extract_next_element,3},
	{l_call_ext,0},
	{is_integer_allocate,0},
	{l_fetch,16},
	{int_code_end,0},
	{move_return,6},
	{l_move_call_ext,50},
	{l_call_ext,12},
	{extract_next_element2,5},
	{l_catch,4},
	{is_nil,6},
	{try_end,2},
	{deallocate_return,6},
	{l_is_eq_exact_immed,19},
	{extract_next_element2,13},
	{l_move_call,3},
	{l_move_call_ext_only,1},
	{is_port,0},
	{l_move_call_ext,7},
	{l_is_eq_exact_immed,21},
	{l_is_eq_exact_immed,12},
	{l_apply,0},
	{is_list,4},
	{l_get,2},
	{l_select_val2,5},
	{node,3},
	{l_is_eq_exact_immed,27},
	{deallocate_return,9},
	{extract_next_element2,10},
	{call_bif,42},
	{l_select_val2,13},
	{l_move_call_only,6},
	{l_call_ext,80},
	{l_is_eq_exact_literal,4},
	{l_bs_init_bits_fail,0},
	{l_move_call_last,5},
	{l_new_bs_put_binary_all,1},
	{l_rem,0},
	{l_move_call_last,1},
	{move_return,14},
	{l_plus,2},
	{l_bs_put_string,2},
	{l_new_bs_put_integer,0},
	{move_deallocate_return,7},
	{wait_timeout,0},
	{l_is_eq,0},
	{l_move_call_only,2},
	{is_integer,4},
	{l_fetch,12},
	{l_call_fun,3},
	{l_move_call_only,4},
	{l_move_call_only,10},
	{l_select_val2,7},
	{l_fetch,23},
	{is_nonempty_list,6},
	{l_increment,3},
	{is_list,1},
	{bif1_body,0},
	{is_nonempty_list_allocate,0},
	{l_bs_get_integer_8,1},
	{l_gc_bif1,3},
	{apply,0},
	{l_is_ne_exact_literal,0},
	{get_list,2},
	{l_element,0},
	{l_fetch,6},
	{l_is_ne_exact,0},
	{l_call_last,2},
	{get_tuple_element,4},
	{get_tuple_element,5},
	{call_bif,12},
	{is_nonempty_list_test_heap,0},
	{l_move_call,0},
	{is_integer,3},
	{call_bif,21},
	{l_bs_skip_bits_imm2,1},
	{is_nonempty_list,3},
	{l_increment,1},
	{test_arity,1},
	{put_list,4},
	{is_tuple_of_arity,3},
	{move_return,8},
	{is_integer,1},
	{l_bs_test_unit_8,3},
	{l_bs_put_string,0},
	{l_bs_test_zero_tail2,3},
	{l_bs_get_binary_all_reuse,1},
	{l_is_eq_exact_immed,9},
	{put_list,5},
	{is_nil,4},
	{bif2_body,0},
	{l_fast_element,2},
	{l_trim,4},
	{catch_end,0},
	{get_tuple_element,3},
	{self,1},
	{l_move_call_only,8},
	{move_deallocate_return,4},
	{l_bs_test_unit_8,1},
	{timeout,0},
	{is_binary,0},
	{l_allocate,7},
	{move,13},
	{l_bif2,2},
	{l_move_call_ext,8},
	{l_trim,2},
	{l_is_ne_exact_immed,4},
	{call_bif,29},
	{call_bif,46},
	{l_catch,6},
	{recv_mark,0},
	{l_recv_set,0},
	{self,2},
	{catch_end,6},
	{l_is_eq_exact_literal,0},
	{l_call_ext_only,5},
	{l_is_eq_exact_immed,11},
	{extract_next_element,10},
	{l_fetch,20},
	{l_is_ne_exact_immed,10},
	{is_nonempty_list,5},
	{l_is_eq_exact_immed,35},
	{l_select_val2,4},
	{wait,0},
	{l_select_val_smallints,1},
	{move,9},
	{is_tuple,4},
	{move_return,2},
	{l_move_call_only,5},
	{l_allocate,5},
	{call_bif,11},
	{extract_next_element3,3},
	{l_move_call,12},
	{l_fast_element,1},
	{move_jump,3},
	{extract_next_element,9},
	{extract_next_element2,4},
	{l_move_call,2},
	{l_move_call_only,0},
	{is_nonempty_list,8},
	{l_catch,3},
	{call_bif,28},
	{l_call_ext,28},
	{is_integer,2},
	{is_nonempty_list,7},
	{l_is_ne_exact_immed,3},
	{try_end,5},
	{is_tuple,3},
	{extract_next_element2,12},
	{l_call_last,11},
	{put_list,14},
	{move_return,26},
	{call_bif,44},
	{extract_next_element3,1},
	{l_move_call,5},
	{l_call_ext,101},
	{call_bif,17},
	{move_jump,0},
	{init,3},
	{l_call_ext_only,2},
	{put_list,6},
	{get_tuple_element,2},
	{l_catch,1},
	{l_bs_test_unit_8,0},
	{loop_rec_end,0},
	{l_band,0},
	{l_move_call_ext,9},
	{is_nil,5},
	{self,3},
	{test_heap_1_put_list,1},
	{bif2_body,1},
	{l_bs_skip_bits_imm2,0},
	{l_is_eq_exact_immed,16},
	{l_bs_restore2,0},
	{is_nonempty_list,9},
	{l_move_call_ext,6},
	{call_bif,27},
	{l_move_call,6},
	{catch_end,1},
	{l_call_ext_last,2},
	{get_list,6},
	{move,7},
	{l_allocate_zero,6},
	{extract_next_element,8},
	{l_call_ext,37},
	{l_move_call_ext_last,2},
	{extract_next_element,12},
	{l_call_last,7},
	{l_is_eq_exact_immed,26},
	{call_bif,25},
	{move_return,22},
	{call_bif,37},
	{is_nil,11},
	{init,4},
	{put_list,9},
	{l_call_ext,4},
	{l_is_ne_exact_immed,0},
	{l_jump_on_val,1},
	{l_is_ne_exact_immed,11},
	{l_call_last,5},
	{l_call_ext_only,4},
	{extract_next_element,11},
	{l_call_fun_last,0},
	{l_call_ext,25},
	{extract_next_element,15},
	{l_move_call_ext,14},
	{l_move_call_ext,5},
	{is_nil,7},
	{l_bs_save2,0},
	{try_end,0},
	{l_int_div,0},
	{bif1_body,3},
	{l_bor,0},
	{l_is_eq_exact_immed,18},
	{extract_next_element,7},
	{l_bsl,1},
	{l_move_call_ext_only,3},
	{deallocate_return,7},
	{l_jump_on_val,0},
	{get_list,8},
	{catch_end,3},
	{is_nil,8},
	{put_list,10},
	{get_list,5},
	{is_tuple,9},
	{l_apply_last,0},
	{l_call_ext,43},
	{call_bif,30},
	{l_bsr,0},
	{l_move_call_ext,15},
	{l_bs_skip_bits2,0},
	{l_call_ext,71},
	{l_is_ne_exact_immed,1},
	{is_list,3},
	{init,5},
	{call_bif,40},
	{l_is_eq_exact_immed,22},
	{extract_next_element,13},
	{l_is_ne_exact_immed,8},
	{l_get,1},
	{is_nonempty_list,10},
	{l_move_call,7},
	{l_fast_element,0},
	{l_move_call_ext,21},
	{is_bitstr,0},
	{l_select_val2,14},
	{call_bif,31},
	{l_call_ext,96},
	{move_return,17},
	{l_is_eq_exact_immed,15},
	{l_call_ext,51},
	{call_bif,10},
	{move_jump,13},
	{l_trim,5},
	{is_function,0},
	{l_call_ext,2},
	{call_bif,26},
	{extract_next_element,16},
	{l_move_call,8},
	{extract_next_element2,7},
	{l_allocate,8},
	{bif1_body,7},
	{l_call_ext_last,6},
	{l_fetch,7},
	{deallocate_return,10},
	{call_bif,38},
	{init,9},
	{move_return,10},
	{is_nil,9},
	{move_deallocate_return,6},
	{l_fdiv,0},
	{l_band,2},
	{l_bs_test_zero_tail2,5},
	{call_bif,22},
	{l_fcheckerror,0},
	{fclearerror,0},
	{allocate_heap_zero,0},
	{fconv,0},
	{fmove_1,0},
	{l_select_val_atoms,1},
	{l_move_call_ext_last,6},
	{l_bsl,0},
	{fmove_2,1},
	{l_increment,6},
	{l_call_ext,41},
	{l_move_call_ext,2},
	{is_tuple,7},
	{l_call_ext,20},
	{init,6},
	{l_move_call_last,4},
	{l_call_ext,38},
	{extract_next_element3,6},
	{move_return,21},
	{l_is_ne,0},
	{extract_next_element2,6},
	{l_call_ext,66},
	{is_nonempty_list,24},
	{move_jump,2},
	{l_allocate_zero,7},
	{l_call_ext,23},
	{l_call_ext,11},
	{l_catch,8},
	{is_pid,1},
	{is_reference,0},
	{call_bif,32},
	{is_nonempty_list,12},
	{l_is_eq_exact_immed,20},
	{l_bs_test_zero_tail2,1},
	{l_move_call_ext,4},
	{extract_next_element2,8},
	{l_call_ext,16},
	{l_rem,2},
	{l_move_call_ext,38},
	{l_move_call_ext,3},
	{catch_end,8},
	{self,6},
	{l_is_eq_exact_immed,13},
	{l_call_ext,105},
	{l_move_call_ext,10},
	{l_is_eq_exact_immed,30},
	{l_call_ext,7},
	{l_call_last,8},
	{test_arity,3},
	{is_boolean,0},
	{l_allocate_zero,10},
	{l_call_ext,26},
	{l_call_ext,47},
	{is_list,6},
	{is_nonempty_list,15},
	{l_is_eq_exact_literal,7},
	{l_move_call_ext_only,6},
	{self,4},
	{l_call_ext,34},
	{l_is_eq_exact_immed,24},
	{l_call_ext,17},
	{is_function,1},
	{l_move_call,15},
	{l_fetch,19},
	{move_return,28},
	{node,4},
	{l_call_ext,45},
	{l_bor,1},
	{move_return,7},
	{extract_next_element3,10},
	{l_call_ext,82},
	{is_nonempty_list,11},
	{is_atom,4},
	{l_call_ext,8},
	{l_apply_fun,0},
	{l_call_ext,109},
	{extract_next_element2,14},
	{move_return,16},
	{is_nil,15},
	{l_call_ext_last,3},
	{l_allocate_zero,8},
	{try_end,3},
	{l_fetch,15},
	{l_fast_element,3},
	{l_allocate_zero,9},
	{is_nil,12},
	{l_select_val2,10},
	{move_deallocate_return,5},
	{extract_next_element,14},
	{call_bif,6},
	{l_call_ext,64},
	{extract_next_element3,5},
	{l_move_call_ext,22},
	{call_bif,33},
	{l_call_ext,74},
	{is_nil,13},
	{l_move_call_ext_only,0},
	{init,7},
	{is_tuple,5},
	{l_call_ext,31},
	{try_end,6},
	{is_atom,6},
	{set_tuple_element,1},
	{put_list,7},
	{l_call_ext_last,4},
	{l_increment,5},
	{is_atom,5},
	{l_is_eq_exact_immed,14},
	{l_call_ext,55},
	{call_bif,35},
	{call_bif,23},
	{l_move_call,10},
	{move_return,36},
	{l_move_call_only,7},
	{l_call_last,10},
	{bif1_body,5},
	{init,8},
	{call_bif,20},
	{l_is_eq_exact_immed,36},
	{l_call_ext,42},
	{extract_next_element,21},
	{l_int_bnot,0},
	{deallocate_return,8},
	{call_bif,34},
	{l_put_tuple,6},
	{node,0},
	{l_is_eq_exact_immed,25},
	{l_move_call,13},
	{bif2_body,2},
	{init,10},
	{extract_next_element2,17},
	{l_new_bs_put_integer,2},
	{l_move_call,16},
	{l_move_call_ext,36},
	{is_tuple,8},
	{extract_next_element2,11},
	{is_nil,28},
	{put_list,12},
	{get_list,4},
	{l_bs_get_integer_16,1},
	{catch_end,4},
	{extract_next_element,17},
	{try_end,7},
	{call_bif,43},
	{is_tuple,6},
	{l_call_ext,44},
	{l_call_ext,3},
	{l_trim,8},
	{put_list,11},
	{l_is_eq_exact_literal,6},
	{l_move_call_ext,44},
	{l_get,6},
	{call_bif,5},
	{l_call_ext,91},
	{l_is_ne_exact_immed,5},
	{is_nil,10},
	{l_move_call_ext,37},
	{node,2},
	{is_nil,17},
	{is_nonempty_list,33},
	{l_is_eq_exact_immed,28},
	{l_call_ext,61},
	{l_trim,6},
	{is_nil,14},
	{move_jump,7},
	{move_jump,6},
	{move_jump,1},
	{move_return,19},
	{bs_context_to_binary,0},
	{badmatch,0},
	{is_nonempty_list,35},
	{is_nonempty_list,19},
	{l_move_call,25},
	{bif1_body,6},
	{bif1_body,1},
	{bif2_body,3},
	{is_float,1},
	{node,1},
	{l_move_call_ext_last,4},
	{call_bif,13},
	{l_call_ext,92},
	{l_call_ext,78},
	{l_call_ext,36},
	{move_jump,10},
	{move_return,9},
	{l_bs_test_unit_8,2},
	{fconv,2},
	{deallocate_return,11},
	{l_call_ext_only,0},
	{move,10},
	{l_move_call_ext,28},
	{l_call_ext,40},
	{l_move_call,17},
	{l_move_call_ext,1},
	{extract_next_element,24},
	{l_is_ne_exact_immed,6},
	{l_trim,7},
	{l_call_fun,4},
	{l_apply_fun_only,0},
	{l_move_call_ext,26},
	{l_move_call_ext,25},
	{l_bs_skip_bits_all2,1},
	{l_call_ext,98},
	{l_call_ext,14},
	{move_return,11},
	{bs_context_to_binary,9},
	{is_nonempty_list,14},
	{l_move_call_ext_only,7},
	{bif1_body,4},
	{l_move_call_ext,46},
	{test_heap_1_put_list,4},
	{self,5},
	{l_call_ext,88},
	{l_call_ext,86},
	{extract_next_element,19},
	{l_fadd,0},
	{extract_next_element2,16},
	{move_jump,12},
	{move_return,45},
	{move_return,29},
	{move_return,13},
	{move_deallocate_return,8},
	{is_bigint,0},
	{fmove_2,0},
	{fmove_1,1},
	{l_move_call_last,7},
	{l_is_ne_exact_immed,9},
	{l_fast_element,5},
	{is_nonempty_list,13},
	{l_is_eq_exact_literal,3},
	{test_heap_1_put_list,3},
	{l_call_ext,84},
	{l_call_ext,75},
	{l_call_ext,62},
	{l_call_ext,58},
	{l_call_ext,48},
	{l_call_ext,35},
	{l_call_ext,10},
	{extract_next_element2,9},
	{is_nil,20},
	{l_bs_put_string,1},
	{is_list,5},
	{is_nonempty_list,22},
	{get_list,10},
	{l_move_call,9},
	{l_move_call_ext,24},
	{l_move_call_ext,16},
	{l_is_eq_exact_immed,29},
	{l_call_ext,77},
	{l_select_val_atoms,2},
	{is_nonempty_list,21},
	{is_binary,3},
	{l_move_call_ext_only,4},
	{l_move_call_ext,17},
	{init,11},
	{l_get,3},
	{l_bs_test_zero_tail2,2},
	{l_bs_get_integer,0},
	{func_info,0},
	{extract_next_element,22},
	{l_select_val_atoms,3},
	{l_trim,10},
	{is_nil,21},
	{l_move_call_ext_last,5},
	{l_fsub,0},
	{l_move_call_ext,35},
	{l_move_call_ext,31},
	{l_move_call_ext,12},
	{init,15},
	{l_wait_timeout,5},
	{l_call_last,9},
	{l_is_eq_exact_immed,31},
	{l_call_ext,104},
	{l_call_ext,65},
	{l_call_ext,59},
	{l_call_ext,5},
	{l_new_bs_put_integer,1},
	{move_return,34},
	{move_return,12},
	{l_trim,11},
	{l_move_call_only,9},
	{l_move_call_ext_last,3},
	{l_move_call_ext_last,1},
	{is_nonempty_list,29},
	{is_nonempty_list,23},
	{is_nonempty_list,17},
	{l_call_fun,2},
	{l_apply_only,0},
	{l_fetch,21},
	{l_move_call_ext,47},
	{l_move_call_ext,19},
	{l_bs_test_zero_tail2,0},
	{call_bif,45},
	{bs_init_writable,0},
	{l_call_ext,39},
	{extract_next_element,20},
	{move_jump,11},
	{move_jump,4},
	{move_return,47},
	{move_return,46},
	{move_return,39},
	{move_return,33},
	{move_return,20},
	{is_nil,27},
	{is_nil,22},
	{l_bs_put_string,5},
	{l_bs_put_string,3},
	{put_list,13},
	{is_nonempty_list,18},
	{l_increment,7},
	{l_times,2},
	{l_bs_get_integer_imm,0},
	{l_move_call,11},
	{l_get,5},
	{call_bif,24},
	{call_bif,3},
	{l_is_eq_exact_immed,34},
	{l_move_call_last,6},
	{l_call_ext,9},
	{l_is_ne_exact_immed,7},
	{move_jump,5},
	{move_return,40},
	{l_trim,9},
	{bs_context_to_binary,2},
	{is_nonempty_list,30},
	{is_nonempty_list,27},
	{l_make_export,0},
	{l_select_val2,11},
	{is_number,0},
	{move_deallocate_return,10},
	{l_move_call,28},
	{l_move_call,14},
	{l_move_call_ext_only,2},
	{init,14},
	{init,13},
	{init,12},
	{l_wait_timeout,3},
	{l_wait_timeout,2},
	{l_wait_timeout,1},
	{l_bs_skip_bits_all2,0},
	{l_bs_test_zero_tail2,6},
	{call_bif,39},
	{call_bif,19},
	{call_bif,4},
	{call_bif,2},
	{call_bif,1},
	{call_bif,0},
	{l_int_div,2},
	{l_bs_put_utf16,0},
	{l_bs_get_utf16,2},
	{l_bs_get_utf16,1},
	{l_bs_get_utf16,0},
	{l_allocate,9},
	{l_put_tuple,7},
	{l_put_tuple,5},
	{l_put_tuple,4},
	{l_put_tuple,3},
	{l_put_tuple,2},
	{l_put_tuple,1},
	{l_is_eq_exact_immed,33},
	{l_is_eq_exact_immed,32},
	{l_is_eq_exact_immed,23},
	{l_is_eq_exact_immed,17},
	{l_is_eq_exact_immed,10},
	{l_is_eq_exact_immed,4},
	{l_call_ext,108},
	{l_call_ext,107},
	{l_call_ext,106},
	{l_call_ext,103},
	{l_call_ext,102},
	{l_call_ext,100},
	{l_call_ext,99},
	{l_call_ext,97},
	{l_call_ext,95},
	{l_call_ext,94},
	{l_call_ext,93},
	{l_call_ext,90},
	{l_call_ext,89},
	{l_call_ext,87},
	{l_call_ext,85},
	{l_call_ext,83},
	{l_call_ext,81},
	{l_call_ext,79},
	{l_call_ext,76},
	{l_call_ext,73},
	{l_call_ext,72},
	{l_call_ext,70},
	{l_call_ext,69},
	{l_call_ext,68},
	{l_call_ext,67},
	{l_call_ext,63},
	{l_call_ext,60},
	{l_call_ext,57},
	{l_call_ext,56},
	{l_call_ext,54},
	{l_call_ext,53},
	{l_call_ext,52},
	{l_call_ext,50},
	{l_call_ext,49},
	{l_call_ext,46},
	{l_call_ext,33},
	{l_call_ext,32},
	{l_call_ext,30},
	{l_call_ext,29},
	{l_call_ext,27},
	{l_call_ext,22},
	{l_call_ext,21},
	{l_call_ext,19},
	{l_call_ext,18},
	{l_call_ext,15},
	{l_call_ext,13},
	{l_call_ext,1},
	{allocate_init,1},
	{extract_next_element,23},
	{extract_next_element,18},
	{get_tuple_element,11},
	{get_tuple_element,10},
	{get_tuple_element,9},
	{get_tuple_element,8},
	{get_tuple_element,7},
	{get_tuple_element,6},
	{set_tuple_element,2},
	{l_call_fun_last,1},
	{l_fast_element,4},
	{l_bs_test_unit,0},
	{extract_next_element3,9},
	{extract_next_element3,8},
	{extract_next_element2,15},
	{l_bs_start_match2,4},
	{l_bs_start_match2,3},
	{l_bs_start_match2,2},
	{l_bs_start_match2,1},
	{l_bs_get_integer_32,3},
	{l_bs_get_integer_32,2},
	{l_bs_get_integer_32,1},
	{l_bor,2},
	{l_bsr,1},
	{l_bs_get_binary_imm2,2},
	{l_bs_get_binary_imm2,1},
	{l_bs_test_tail_imm2,0},
	{l_bxor,0},
	{l_bs_get_float2,0},
	{move_jump,9},
	{move_jump,8},
	{allocate_heap,1},
	{move_return,44},
	{move_return,43},
	{move_return,42},
	{move_return,41},
	{move_return,38},
	{move_return,37},
	{move_return,35},
	{move_return,32},
	{move_return,31},
	{move_return,30},
	{move_return,27},
	{move_return,25},
	{move_return,24},
	{move_return,23},
	{move_return,18},
	{move_return,15},
	{l_new_bs_put_integer_imm,2},
	{l_new_bs_put_integer_imm,1},
	{l_bs_get_integer_small_imm,1},
	{l_rem,1},
	{is_nil,26},
	{is_nil,25},
	{is_nil,24},
	{is_nil,23},
	{is_nil,19},
	{is_nil,18},
	{is_nil,16},
	{l_bsl,2},
	{l_fmul,0},
	{is_tuple_of_arity,4},
	{is_tuple_of_arity,2},
	{test_arity,4},
	{test_arity,2},
	{bs_context_to_binary,8},
	{bs_context_to_binary,7},
	{bs_context_to_binary,6},
	{bs_context_to_binary,5},
	{bs_context_to_binary,4},
	{bs_context_to_binary,3},
	{bs_context_to_binary,1},
	{l_new_bs_put_binary,0},
	{badmatch,17},
	{badmatch,16},
	{badmatch,15},
	{badmatch,14},
	{badmatch,13},
	{badmatch,12},
	{badmatch,11},
	{badmatch,10},
	{badmatch,9},
	{badmatch,8},
	{badmatch,7},
	{badmatch,6},
	{badmatch,5},
	{badmatch,4},
	{badmatch,3},
	{badmatch,2},
	{badmatch,1},
	{l_bs_test_unit_8,4},
	{l_bs_get_utf8,1},
	{l_bs_get_utf8,0},
	{l_bs_put_utf8,0},
	{l_bs_match_string,2},
	{l_bs_match_string,1},
	{l_bs_put_string,4},
	{fconv,1},
	{l_m_div,0},
	{raise,1},
	{raise,0},
	{is_integer_allocate,1},
	{is_nonempty_list_allocate,2},
	{l_fnegate,0},
	{l_bs_validate_unicode,0},
	{l_hibernate,0},
	{put_list,8},
	{is_nonempty_list,41},
	{is_nonempty_list,40},
	{is_nonempty_list,39},
	{is_nonempty_list,38},
	{is_nonempty_list,37},
	{is_nonempty_list,36},
	{is_nonempty_list,34},
	{is_nonempty_list,32},
	{is_nonempty_list,31},
	{is_nonempty_list,28},
	{is_nonempty_list,26},
	{is_nonempty_list,25},
	{is_nonempty_list,20},
	{is_nonempty_list,16},
	{get_list,9},
	{get_list,7},
	{get_list,1},
	{case_end,11},
	{case_end,10},
	{case_end,9},
	{case_end,8},
	{case_end,7},
	{case_end,6},
	{case_end,5},
	{case_end,4},
	{case_end,3},
	{case_end,2},
	{case_end,1},
	{case_end,0},
	{try_case_end,1},
	{try_case_end,0},
	{apply_last,1},
	{l_call_ext_last,5},
	{l_bs_append,1},
	{l_increment,4},
	{l_bs_private_append,0},
	{l_bs_init,0},
	{l_new_bs_put_float,0},
	{l_bs_validate_unicode_retract,0},
	{l_yield,0},
	{l_apply_fun_last,0},
	{l_minus,2},
	{l_minus,1},
	{init3,1},
	{l_select_val_smallints,2},
	{l_bif2,5},
	{l_bif2,3},
	{l_select_val2,1},
	{l_select_tuple_arity2,3},
	{l_select_tuple_arity2,2},
	{init2,1},
	{l_bs_skip_bits_imm2,2},
	{l_bs_restore2,3},
	{l_bs_restore2,2},
	{l_bs_restore2,1},
	{l_bs_skip_bits2,1},
	{l_bs_get_binary_all2,2},
	{l_bs_get_binary_all2,1},
	{l_bs_save2,2},
	{l_bs_save2,1},
	{is_function2,0},
	{l_bif1,2},
	{l_bif1,1},
	{is_nonempty_list_test_heap,1},
	{allocate_heap_zero,1},
	{deallocate_return,12},
	{move_deallocate_return,9},
	{l_bs_init_heap_bin,1},
	{l_call_fun,1},
	{l_new_bs_put_binary_imm,3},
	{l_new_bs_put_binary_imm,2},
	{l_bs_get_integer_imm,1},
	{l_new_bs_put_float_imm,1},
	{l_new_bs_put_float_imm,0},
	{l_move_call,35},
	{l_move_call,34},
	{l_move_call,33},
	{l_move_call,32},
	{l_move_call,31},
	{l_move_call,30},
	{l_move_call,29},
	{l_move_call,27},
	{l_move_call,26},
	{l_move_call,24},
	{l_move_call,23},
	{l_move_call,22},
	{l_move_call,21},
	{l_move_call,20},
	{l_move_call,19},
	{l_move_call,18},
	{l_is_eq_exact_literal,2},
	{l_bs_init_bits_fail,1},
	{l_jump_on_val,2},
	{l_bs_init_fail,1},
	{l_call_ext_only,3},
	{l_call_ext_only,1},
	{l_move_call_ext_only,9},
	{l_move_call_ext_only,8},
	{l_move_call_ext_only,5},
	{bif1_body,2},
	{l_select_tuple_arity,2},
	{l_fetch,22},
	{l_fetch,11},
	{l_fetch,9},
	{l_fetch,5},
	{l_fetch,3},
	{l_catch,7},
	{l_bs_get_integer_8,2},
	{l_bs_get_integer_16,2},
	{move,12},
	{l_bs_utf8_size,0},
	{l_bs_utf16_size,0},
	{l_move_call_ext,49},
	{l_move_call_ext,48},
	{l_move_call_ext,45},
	{l_move_call_ext,43},
	{l_move_call_ext,42},
	{l_move_call_ext,41},
	{l_move_call_ext,40},
	{l_move_call_ext,39},
	{l_move_call_ext,34},
	{l_move_call_ext,33},
	{l_move_call_ext,32},
	{l_move_call_ext,30},
	{l_move_call_ext,29},
	{l_move_call_ext,27},
	{l_move_call_ext,23},
	{l_move_call_ext,20},
	{l_move_call_ext,18},
	{l_move_call_ext,11},
	{l_move_call_ext,0},
	{catch_end,7},
	{test_heap_1_put_list,2},
	{l_bs_add,1},
	{l_band,1},
	{on_load,0},
	{l_get,4},
	{l_get,0},
	{if_end,0},
	{l_wait_timeout,4},
	{l_wait_timeout,0},
	{system_limit,0},
	{l_plus,3},
	{l_plus,1},
	{l_gc_bif3,0},
	{move2,10},
	{move2,9},
	{move2,8},
	{move2,7},
	{move2,6},
	{move2,5},
	{move2,4},
	{move2,2},
	{l_bs_skip_bits_all2,2},
	{l_bs_test_zero_tail2,4},
	{l_bs_get_integer,1},
	{l_bs_get_binary2,1},
	{fmove_2,2},
	{l_gc_bif2,0},
	{l_gc_bif1,4},
	{l_gc_bif1,2},
	{l_gc_bif1,1},
	{test_heap,1}

].


fit_args(allocate_heap, [NumSlots,HeapNeeded,Live]) when NumSlots >= 0, NumSlots =< 255, HeapNeeded >= 0, HeapNeeded =< 255, Live >= 0, Live =< 255 -> 0;
fit_args(allocate_heap, [_,_,Live]) when Live >= 0, Live =< 255 -> 1;
fit_args(allocate_heap_zero, [NumSlots,HeapNeeded,Live]) when NumSlots >= 0, NumSlots =< 255, HeapNeeded >= 0, HeapNeeded =< 255, Live >= 0, Live =< 255 -> 0;
fit_args(allocate_heap_zero, [_,_,Live]) when Live >= 0, Live =< 255 -> 1;
fit_args(allocate_init, [Arg0,{y,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(allocate_init, [_,_]) -> 1;
fit_args(apply, [Arg0]) when Arg0 >= 0, Arg0 =< 255 -> 0;
fit_args(apply_last, [Arg0,Arg1]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(apply_last, [Arg0,_]) when Arg0 >= 0, Arg0 =< 255 -> 1;
fit_args(badmatch, [{x,0}]) -> 0;
fit_args(badmatch, [{y,8}]) -> 16;
fit_args(badmatch, [{x,6}]) -> 15;
fit_args(badmatch, [{y,6}]) -> 14;
fit_args(badmatch, [{x,8}]) -> 13;
fit_args(badmatch, [{x,5}]) -> 12;
fit_args(badmatch, [{y,4}]) -> 11;
fit_args(badmatch, [{y,0}]) -> 10;
fit_args(badmatch, [{y,5}]) -> 9;
fit_args(badmatch, [{y,9}]) -> 8;
fit_args(badmatch, [{y,3}]) -> 7;
fit_args(badmatch, [{x,4}]) -> 6;
fit_args(badmatch, [{y,2}]) -> 5;
fit_args(badmatch, [{y,1}]) -> 4;
fit_args(badmatch, [{x,2}]) -> 3;
fit_args(badmatch, [{x,3}]) -> 2;
fit_args(badmatch, [{x,1}]) -> 1;
fit_args(badmatch, [_]) -> 17;
fit_args(bif1_body, [{b,{erlang,'not',1}},{x,0},{x,0}]) -> 3;
fit_args(bif1_body, [{b,{erlang,hd,1}},{x,0},{x,1}]) -> 1;
fit_args(bif1_body, [{b,{erlang,hd,1}},{y,1},{x,2}]) -> 2;
fit_args(bif1_body, [{b,{erlang,hd,1}},_,{x,0}]) -> 0;
fit_args(bif1_body, [{b,{erlang,hd,1}},_,{x,2}]) -> 6;
fit_args(bif1_body, [{b,_},_,{x,1}]) -> 5;
fit_args(bif1_body, [{b,_},_,{x,0}]) -> 4;
fit_args(bif1_body, [{b,_},_,_]) -> 7;
fit_args(bif2_body, [{b,_},{x,0}]) -> 0;
fit_args(bif2_body, [{b,_},{x,1}]) -> 1;
fit_args(bif2_body, [{b,_},{x,2}]) -> 2;
fit_args(bif2_body, [{b,_},_]) -> 3;
fit_args(bs_context_to_binary, [{x,0}]) -> 0;
fit_args(bs_context_to_binary, [{x,1}]) -> 2;
fit_args(bs_context_to_binary, [{x,8}]) -> 8;
fit_args(bs_context_to_binary, [{x,4}]) -> 7;
fit_args(bs_context_to_binary, [{y,0}]) -> 6;
fit_args(bs_context_to_binary, [{y,5}]) -> 5;
fit_args(bs_context_to_binary, [{y,9}]) -> 4;
fit_args(bs_context_to_binary, [{x,2}]) -> 3;
fit_args(bs_context_to_binary, [{y,1}]) -> 1;
fit_args(bs_context_to_binary, [_]) -> 9;
fit_args(bs_init_writable, []) -> 0;
fit_args(call_bif, [{b,{erlang,'++',2}}]) -> 8;
fit_args(call_bif, [{b,{erlang,setelement,3}}]) -> 7;
fit_args(call_bif, [{b,{erlang,list_to_atom,1}}]) -> 16;
fit_args(call_bif, [{b,{erlang,integer_to_list,1}}]) -> 15;
fit_args(call_bif, [{b,{ets,lookup,2}}]) -> 14;
fit_args(call_bif, [{b,{lists,member,2}}]) -> 9;
fit_args(call_bif, [{b,{ets,update_counter,3}}]) -> 41;
fit_args(call_bif, [{b,{lists,reverse,2}}]) -> 36;
fit_args(call_bif, [{b,{erlang,atom_to_list,1}}]) -> 18;
fit_args(call_bif, [{b,{ets,match_object,2}}]) -> 42;
fit_args(call_bif, [{b,{erlang,list_to_binary,1}}]) -> 12;
fit_args(call_bif, [{b,{erlang,binary_to_list,1}}]) -> 21;
fit_args(call_bif, [{b,{erlang,whereis,1}}]) -> 29;
fit_args(call_bif, [{b,{erlang,monitor,2}}]) -> 46;
fit_args(call_bif, [{b,{ets,insert,2}}]) -> 11;
fit_args(call_bif, [{b,{erlang,spawn,1}}]) -> 28;
fit_args(call_bif, [{b,{ets,safe_fixtable,2}}]) -> 44;
fit_args(call_bif, [{b,{lists,keyfind,3}}]) -> 17;
fit_args(call_bif, [{b,{erlang,now,0}}]) -> 27;
fit_args(call_bif, [{b,{ets,new,2}}]) -> 25;
fit_args(call_bif, [{b,{erlang,list_to_tuple,1}}]) -> 37;
fit_args(call_bif, [{b,{lists,keymember,3}}]) -> 30;
fit_args(call_bif, [{b,{ets,next,2}}]) -> 40;
fit_args(call_bif, [{b,{erlang,tuple_to_list,1}}]) -> 31;
fit_args(call_bif, [{b,{erlang,get_module_info,2}}]) -> 10;
fit_args(call_bif, [{b,{ets,delete,2}}]) -> 26;
fit_args(call_bif, [{b,{erlang,make_ref,0}}]) -> 38;
fit_args(call_bif, [{b,{ets,lookup_element,3}}]) -> 22;
fit_args(call_bif, [{b,{erlang,process_flag,2}}]) -> 32;
fit_args(call_bif, [{b,{erlang,throw,1}}]) -> 6;
fit_args(call_bif, [{b,{erlang,pid_to_list,1}}]) -> 33;
fit_args(call_bif, [{b,{erlang,unlink,1}}]) -> 35;
fit_args(call_bif, [{b,{erlang,'--',2}}]) -> 23;
fit_args(call_bif, [{b,{lists,keysearch,3}}]) -> 20;
fit_args(call_bif, [{b,{erlang,iolist_to_binary,1}}]) -> 34;
fit_args(call_bif, [{b,{erlang,spawn_link,1}}]) -> 43;
fit_args(call_bif, [{b,{erlang,exit,1}}]) -> 5;
fit_args(call_bif, [{b,{ets,delete,1}}]) -> 13;
fit_args(call_bif, [{b,{ets,match,2}}]) -> 45;
fit_args(call_bif, [{b,{re,run,3}}]) -> 24;
fit_args(call_bif, [{b,{erlang,error,1}}]) -> 3;
fit_args(call_bif, [{b,{erlang,get_stacktrace,0}}]) -> 39;
fit_args(call_bif, [{b,{ets,info,2}}]) -> 19;
fit_args(call_bif, [{b,{erlang,exit,2}}]) -> 4;
fit_args(call_bif, [{b,{erlang,error,2}}]) -> 2;
fit_args(call_bif, [{b,{erlang,raise,3}}]) -> 1;
fit_args(call_bif, [{b,{erlang,purge_module,1}}]) -> 0;
fit_args(call_bif, [{b,_}]) -> 47;
fit_args(case_end, [{x,5}]) -> 10;
fit_args(case_end, [{y,4}]) -> 9;
fit_args(case_end, [{y,0}]) -> 8;
fit_args(case_end, [{x,4}]) -> 7;
fit_args(case_end, [{y,3}]) -> 6;
fit_args(case_end, [{x,3}]) -> 5;
fit_args(case_end, [{y,1}]) -> 4;
fit_args(case_end, [{y,2}]) -> 3;
fit_args(case_end, [{x,2}]) -> 2;
fit_args(case_end, [{x,1}]) -> 1;
fit_args(case_end, [{x,0}]) -> 0;
fit_args(case_end, [_]) -> 11;
fit_args(catch_end, [{y,5}]) -> 5;
fit_args(catch_end, [{y,2}]) -> 2;
fit_args(catch_end, [{y,0}]) -> 0;
fit_args(catch_end, [{y,6}]) -> 6;
fit_args(catch_end, [{y,1}]) -> 1;
fit_args(catch_end, [{y,3}]) -> 3;
fit_args(catch_end, [{y,4}]) -> 4;
fit_args(catch_end, [{y,20}]) -> 7;
fit_args(catch_end, [_]) -> 8;
fit_args(deallocate_return, [0]) -> 1;
fit_args(deallocate_return, [1]) -> 0;
fit_args(deallocate_return, [2]) -> 2;
fit_args(deallocate_return, [4]) -> 4;
fit_args(deallocate_return, [3]) -> 3;
fit_args(deallocate_return, [5]) -> 5;
fit_args(deallocate_return, [6]) -> 6;
fit_args(deallocate_return, [9]) -> 9;
fit_args(deallocate_return, [7]) -> 7;
fit_args(deallocate_return, [10]) -> 10;
fit_args(deallocate_return, [8]) -> 8;
fit_args(deallocate_return, [11]) -> 11;
fit_args(deallocate_return, [12]) -> 12;
fit_args(deallocate_return, [_]) -> 13;
fit_args(extract_next_element, [{x,2}]) -> 2;
fit_args(extract_next_element, [{x,3}]) -> 1;
fit_args(extract_next_element, [{x,5}]) -> 4;
fit_args(extract_next_element, [{x,1}]) -> 0;
fit_args(extract_next_element, [{x,7}]) -> 6;
fit_args(extract_next_element, [{x,6}]) -> 5;
fit_args(extract_next_element, [{x,4}]) -> 3;
fit_args(extract_next_element, [{x,8}]) -> 10;
fit_args(extract_next_element, [{y,0}]) -> 9;
fit_args(extract_next_element, [{y,1}]) -> 8;
fit_args(extract_next_element, [{y,2}]) -> 12;
fit_args(extract_next_element, [{y,3}]) -> 11;
fit_args(extract_next_element, [{y,5}]) -> 15;
fit_args(extract_next_element, [{x,255}]) -> 7;
fit_args(extract_next_element, [{x,9}]) -> 13;
fit_args(extract_next_element, [{y,4}]) -> 16;
fit_args(extract_next_element, [{x,10}]) -> 14;
fit_args(extract_next_element, [{y,6}]) -> 21;
fit_args(extract_next_element, [{x,11}]) -> 17;
fit_args(extract_next_element, [{x,13}]) -> 19;
fit_args(extract_next_element, [{x,18}]) -> 22;
fit_args(extract_next_element, [{x,14}]) -> 20;
fit_args(extract_next_element, [{x,15}]) -> 23;
fit_args(extract_next_element, [{x,12}]) -> 18;
fit_args(extract_next_element, [_]) -> 24;
fit_args(extract_next_element2, [{x,3}]) -> 1;
fit_args(extract_next_element2, [{x,1}]) -> 0;
fit_args(extract_next_element2, [{x,2}]) -> 2;
fit_args(extract_next_element2, [{x,4}]) -> 3;
fit_args(extract_next_element2, [{x,6}]) -> 5;
fit_args(extract_next_element2, [{x,14}]) -> 13;
fit_args(extract_next_element2, [{x,10}]) -> 10;
fit_args(extract_next_element2, [{x,5}]) -> 4;
fit_args(extract_next_element2, [{y,0}]) -> 12;
fit_args(extract_next_element2, [{x,8}]) -> 7;
fit_args(extract_next_element2, [{x,7}]) -> 6;
fit_args(extract_next_element2, [{x,9}]) -> 8;
fit_args(extract_next_element2, [{x,13}]) -> 14;
fit_args(extract_next_element2, [{x,11}]) -> 11;
fit_args(extract_next_element2, [{x,15}]) -> 16;
fit_args(extract_next_element2, [{x,12}]) -> 9;
fit_args(extract_next_element2, [{x,16}]) -> 15;
fit_args(extract_next_element2, [_]) -> 17;
fit_args(extract_next_element3, [{x,1}]) -> 0;
fit_args(extract_next_element3, [{x,2}]) -> 2;
fit_args(extract_next_element3, [{x,5}]) -> 4;
fit_args(extract_next_element3, [{x,8}]) -> 7;
fit_args(extract_next_element3, [{x,4}]) -> 3;
fit_args(extract_next_element3, [{x,3}]) -> 1;
fit_args(extract_next_element3, [{x,6}]) -> 6;
fit_args(extract_next_element3, [{x,7}]) -> 5;
fit_args(extract_next_element3, [{x,10}]) -> 9;
fit_args(extract_next_element3, [{x,11}]) -> 8;
fit_args(extract_next_element3, [_]) -> 10;
fit_args(fclearerror, []) -> 0;
fit_args(fconv, [_,{fr,0}]) -> 0;
fit_args(fconv, [_,{fr,1}]) -> 1;
fit_args(fconv, [_,{fr,_}]) -> 2;
fit_args(fmove_1, [_,{fr,1}]) -> 0;
fit_args(fmove_1, [_,{fr,_}]) -> 1;
fit_args(fmove_2, [{fr,_},{x,0}]) -> 0;
fit_args(fmove_2, [{fr,_},{x,_}]) -> 1;
fit_args(fmove_2, [{fr,_},_]) -> 2;
fit_args(func_info, [_,_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(get_list, [{x,0},{y,Arg1},{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 7;
fit_args(get_list, [{x,0},{x,_},{x,_}]) -> 1;
fit_args(get_list, [{x,0},_,{x,0}]) -> 5;
fit_args(get_list, [{x,_},{y,Arg1},{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 9;
fit_args(get_list, [{x,_},{x,_},{x,_}]) -> 0;
fit_args(get_list, [{x,_},{y,Arg1},{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 6;
fit_args(get_list, [{x,_},{y,Arg1},{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 8;
fit_args(get_list, [{y,Arg0},{x,_},{x,_}]) when Arg0 >= 0, Arg0 =< 255 -> 4;
fit_args(get_list, [_,{x,0},_]) -> 2;
fit_args(get_list, [_,{x,_},{y,Arg2}]) when Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(get_list, [_,_,_]) -> 10;
fit_args(get_tuple_element, [{x,0},1,{x,0}]) -> 6;
fit_args(get_tuple_element, [{x,0},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(get_tuple_element, [{x,0},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(get_tuple_element, [{x,0},_,{x,0}]) -> 9;
fit_args(get_tuple_element, [{y,Arg0},Arg1,{x,0}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 8;
fit_args(get_tuple_element, [{x,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 7;
fit_args(get_tuple_element, [{x,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(get_tuple_element, [{y,Arg0},Arg1,{x,_}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 4;
fit_args(get_tuple_element, [{x,_},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 5;
fit_args(get_tuple_element, [{y,Arg0},Arg1,{y,Arg2}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 10;
fit_args(get_tuple_element, [_,0,{x,0}]) -> 2;
fit_args(get_tuple_element, [_,_,_]) -> 11;
fit_args(if_end, []) -> 0;
fit_args(init, [{y,1}]) -> 0;
fit_args(init, [{y,0}]) -> 1;
fit_args(init, [{y,2}]) -> 2;
fit_args(init, [{y,3}]) -> 3;
fit_args(init, [{y,4}]) -> 4;
fit_args(init, [{y,5}]) -> 5;
fit_args(init, [{y,9}]) -> 9;
fit_args(init, [{y,6}]) -> 6;
fit_args(init, [{y,7}]) -> 7;
fit_args(init, [{y,8}]) -> 8;
fit_args(init, [{y,10}]) -> 10;
fit_args(init, [{y,11}]) -> 11;
fit_args(init, [{y,14}]) -> 14;
fit_args(init, [{y,13}]) -> 13;
fit_args(init, [{y,12}]) -> 12;
fit_args(init, [_]) -> 15;
fit_args(init2, [{y,Arg0},{y,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(init2, [_,_]) -> 1;
fit_args(init3, [{y,Arg0},{y,Arg1},{y,Arg2}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(init3, [_,_,_]) -> 1;
fit_args(int_code_end, []) -> 0;
fit_args(is_atom, [{f,_},{x,0}]) -> 0;
fit_args(is_atom, [{f,_},{x,1}]) -> 1;
fit_args(is_atom, [{f,_},{x,2}]) -> 2;
fit_args(is_atom, [{f,_},{x,3}]) -> 3;
fit_args(is_atom, [{f,_},{x,4}]) -> 4;
fit_args(is_atom, [{f,_},{x,5}]) -> 5;
fit_args(is_atom, [{f,_},_]) -> 6;
fit_args(is_bigint, [{f,_},_]) -> 0;
fit_args(is_binary, [{f,_},{x,2}]) -> 2;
fit_args(is_binary, [{f,_},{x,0}]) -> 1;
fit_args(is_binary, [{f,_},{x,1}]) -> 0;
fit_args(is_binary, [{f,_},_]) -> 3;
fit_args(is_bitstr, [{f,_},_]) -> 0;
fit_args(is_boolean, [{f,_},_]) -> 0;
fit_args(is_float, [{f,_},{x,0}]) -> 0;
fit_args(is_float, [{f,_},_]) -> 1;
fit_args(is_function, [{f,_},{x,0}]) -> 0;
fit_args(is_function, [{f,_},_]) -> 1;
fit_args(is_function2, [{f,_},_,_]) -> 0;
fit_args(is_integer, [{f,_},{x,0}]) -> 0;
fit_args(is_integer, [{f,_},{x,4}]) -> 4;
fit_args(is_integer, [{f,_},{x,3}]) -> 3;
fit_args(is_integer, [{f,_},{x,1}]) -> 1;
fit_args(is_integer, [{f,_},{x,2}]) -> 2;
fit_args(is_integer, [{f,_},_]) -> 5;
fit_args(is_integer_allocate, [{f,_},{x,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(is_integer_allocate, [{f,_},_,_]) -> 1;
fit_args(is_list, [{f,_},{x,2}]) -> 2;
fit_args(is_list, [{f,_},{x,0}]) -> 0;
fit_args(is_list, [{f,_},{x,4}]) -> 4;
fit_args(is_list, [{f,_},{x,1}]) -> 1;
fit_args(is_list, [{f,_},{x,3}]) -> 3;
fit_args(is_list, [{f,_},{x,5}]) -> 5;
fit_args(is_list, [{f,_},_]) -> 6;
fit_args(is_nil, [{f,_},{x,1}]) -> 2;
fit_args(is_nil, [{f,_},{x,2}]) -> 1;
fit_args(is_nil, [{f,_},{x,0}]) -> 0;
fit_args(is_nil, [{f,_},{x,4}]) -> 3;
fit_args(is_nil, [{f,_},{x,6}]) -> 6;
fit_args(is_nil, [{f,_},{x,3}]) -> 4;
fit_args(is_nil, [{f,_},{x,5}]) -> 5;
fit_args(is_nil, [{f,_},{x,10}]) -> 11;
fit_args(is_nil, [{f,_},{x,7}]) -> 7;
fit_args(is_nil, [{f,_},{x,8}]) -> 8;
fit_args(is_nil, [{f,_},{x,9}]) -> 9;
fit_args(is_nil, [{f,_},{x,13}]) -> 15;
fit_args(is_nil, [{f,_},{x,11}]) -> 12;
fit_args(is_nil, [{f,_},{x,12}]) -> 13;
fit_args(is_nil, [{f,_},{y,1}]) -> 10;
fit_args(is_nil, [{f,_},{x,15}]) -> 17;
fit_args(is_nil, [{f,_},{x,14}]) -> 14;
fit_args(is_nil, [{f,_},{y,0}]) -> 20;
fit_args(is_nil, [{f,_},{x,17}]) -> 21;
fit_args(is_nil, [{f,_},{x,20}]) -> 27;
fit_args(is_nil, [{f,_},{x,19}]) -> 22;
fit_args(is_nil, [{f,_},{x,18}]) -> 26;
fit_args(is_nil, [{f,_},{x,22}]) -> 25;
fit_args(is_nil, [{f,_},{y,4}]) -> 24;
fit_args(is_nil, [{f,_},{y,5}]) -> 23;
fit_args(is_nil, [{f,_},{y,3}]) -> 19;
fit_args(is_nil, [{f,_},{x,16}]) -> 18;
fit_args(is_nil, [{f,_},{y,2}]) -> 16;
fit_args(is_nil, [{f,_},_]) -> 28;
fit_args(is_nonempty_list, [{f,_},{x,0}]) -> 0;
fit_args(is_nonempty_list, [{f,_},{x,1}]) -> 2;
fit_args(is_nonempty_list, [{f,_},{x,2}]) -> 1;
fit_args(is_nonempty_list, [{f,_},{x,4}]) -> 4;
fit_args(is_nonempty_list, [{f,_},{x,5}]) -> 6;
fit_args(is_nonempty_list, [{f,_},{x,3}]) -> 3;
fit_args(is_nonempty_list, [{f,_},{x,7}]) -> 5;
fit_args(is_nonempty_list, [{f,_},{x,9}]) -> 8;
fit_args(is_nonempty_list, [{f,_},{x,6}]) -> 7;
fit_args(is_nonempty_list, [{f,_},{x,8}]) -> 9;
fit_args(is_nonempty_list, [{f,_},{x,10}]) -> 10;
fit_args(is_nonempty_list, [{f,_},{y,0}]) -> 24;
fit_args(is_nonempty_list, [{f,_},{x,12}]) -> 12;
fit_args(is_nonempty_list, [{f,_},{x,14}]) -> 15;
fit_args(is_nonempty_list, [{f,_},{x,11}]) -> 11;
fit_args(is_nonempty_list, [{f,_},{y,8}]) -> 33;
fit_args(is_nonempty_list, [{f,_},{y,7}]) -> 35;
fit_args(is_nonempty_list, [{f,_},{y,1}]) -> 19;
fit_args(is_nonempty_list, [{f,_},{x,13}]) -> 14;
fit_args(is_nonempty_list, [{f,_},{y,2}]) -> 13;
fit_args(is_nonempty_list, [{f,_},{y,4}]) -> 22;
fit_args(is_nonempty_list, [{f,_},{x,18}]) -> 21;
fit_args(is_nonempty_list, [{f,_},{x,22}]) -> 29;
fit_args(is_nonempty_list, [{f,_},{x,20}]) -> 23;
fit_args(is_nonempty_list, [{f,_},{x,16}]) -> 17;
fit_args(is_nonempty_list, [{f,_},{x,15}]) -> 18;
fit_args(is_nonempty_list, [{f,_},{x,26}]) -> 30;
fit_args(is_nonempty_list, [{f,_},{x,24}]) -> 27;
fit_args(is_nonempty_list, [{f,_},{x,30}]) -> 40;
fit_args(is_nonempty_list, [{f,_},{x,29}]) -> 39;
fit_args(is_nonempty_list, [{f,_},{x,28}]) -> 38;
fit_args(is_nonempty_list, [{f,_},{x,27}]) -> 37;
fit_args(is_nonempty_list, [{f,_},{x,23}]) -> 36;
fit_args(is_nonempty_list, [{f,_},{y,5}]) -> 34;
fit_args(is_nonempty_list, [{f,_},{x,25}]) -> 32;
fit_args(is_nonempty_list, [{f,_},{x,21}]) -> 31;
fit_args(is_nonempty_list, [{f,_},{y,6}]) -> 28;
fit_args(is_nonempty_list, [{f,_},{y,9}]) -> 26;
fit_args(is_nonempty_list, [{f,_},{x,19}]) -> 25;
fit_args(is_nonempty_list, [{f,_},{x,17}]) -> 20;
fit_args(is_nonempty_list, [{f,_},{y,3}]) -> 16;
fit_args(is_nonempty_list, [{f,_},_]) -> 41;
fit_args(is_nonempty_list_allocate, [{f,_},{x,0},_]) -> 0;
fit_args(is_nonempty_list_allocate, [{f,_},{x,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(is_nonempty_list_allocate, [{f,_},_,_]) -> 2;
fit_args(is_nonempty_list_test_heap, [{f,_},Arg1,Arg2]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(is_nonempty_list_test_heap, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(is_number, [{f,_},_]) -> 0;
fit_args(is_pid, [{f,_},{x,0}]) -> 0;
fit_args(is_pid, [{f,_},_]) -> 1;
fit_args(is_port, [{f,_},_]) -> 0;
fit_args(is_reference, [{f,_},_]) -> 0;
fit_args(is_tuple, [{f,_},{x,0}]) -> 0;
fit_args(is_tuple, [{f,_},{x,2}]) -> 2;
fit_args(is_tuple, [{f,_},{x,1}]) -> 1;
fit_args(is_tuple, [{f,_},{x,4}]) -> 4;
fit_args(is_tuple, [{f,_},{x,3}]) -> 3;
fit_args(is_tuple, [{f,_},{x,8}]) -> 7;
fit_args(is_tuple, [{f,_},{x,7}]) -> 5;
fit_args(is_tuple, [{f,_},{x,6}]) -> 8;
fit_args(is_tuple, [{f,_},{x,5}]) -> 6;
fit_args(is_tuple, [{f,_},_]) -> 9;
fit_args(is_tuple_of_arity, [{f,_},{x,0},2]) -> 0;
fit_args(is_tuple_of_arity, [{f,_},{x,0},_]) -> 2;
fit_args(is_tuple_of_arity, [{f,_},{x,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(is_tuple_of_arity, [{f,_},{y,Arg1},Arg2]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(is_tuple_of_arity, [{f,_},_,_]) -> 4;
fit_args(jump, [{f,_}]) -> 0;
fit_args(l_allocate, [0]) -> 1;
fit_args(l_allocate, [1]) -> 0;
fit_args(l_allocate, [2]) -> 2;
fit_args(l_allocate, [6]) -> 6;
fit_args(l_allocate, [3]) -> 3;
fit_args(l_allocate, [4]) -> 4;
fit_args(l_allocate, [7]) -> 7;
fit_args(l_allocate, [5]) -> 5;
fit_args(l_allocate, [8]) -> 8;
fit_args(l_allocate, [10]) -> 9;
fit_args(l_allocate, [_]) -> 10;
fit_args(l_allocate_zero, [4]) -> 3;
fit_args(l_allocate_zero, [3]) -> 2;
fit_args(l_allocate_zero, [1]) -> 1;
fit_args(l_allocate_zero, [2]) -> 0;
fit_args(l_allocate_zero, [5]) -> 4;
fit_args(l_allocate_zero, [6]) -> 5;
fit_args(l_allocate_zero, [7]) -> 6;
fit_args(l_allocate_zero, [8]) -> 7;
fit_args(l_allocate_zero, [9]) -> 8;
fit_args(l_allocate_zero, [10]) -> 9;
fit_args(l_allocate_zero, [_]) -> 10;
fit_args(l_apply, []) -> 0;
fit_args(l_apply_fun, []) -> 0;
fit_args(l_apply_fun_last, [_]) -> 0;
fit_args(l_apply_fun_only, []) -> 0;
fit_args(l_apply_last, [_]) -> 0;
fit_args(l_apply_only, []) -> 0;
fit_args(l_band, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_band, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_band, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_bif1, [{f,_},{b,_},{x,0},_]) -> 1;
fit_args(l_bif1, [{f,_},{b,_},{x,_},{x,_}]) -> 0;
fit_args(l_bif1, [{f,_},{b,_},_,_]) -> 2;
fit_args(l_bif2, [{f,_},{b,{erlang,element,2}},_]) -> 0;
fit_args(l_bif2, [{f,_},{b,{erlang,'and',2}},_]) -> 4;
fit_args(l_bif2, [{f,_},{b,{erlang,'=:=',2}},_]) -> 1;
fit_args(l_bif2, [{f,_},{b,{erlang,'=<',2}},_]) -> 2;
fit_args(l_bif2, [{f,_},{b,{erlang,'==',2}},_]) -> 5;
fit_args(l_bif2, [{f,_},{b,{erlang,'or',2}},_]) -> 3;
fit_args(l_bif2, [{f,_},{b,_},_]) -> 6;
fit_args(l_bor, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bor, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_bor, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_bs_add, [{f,_},1,_]) -> 0;
fit_args(l_bs_add, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_bs_append, [{f,_},Arg1,Arg2,Arg3,_]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_append, [{f,_},_,Arg2,Arg3,_]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_get_binary2, [{f,_},_,Arg2,_,Arg4,Arg5,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255, Arg5 >= 0, Arg5 =< 255 -> 0;
fit_args(l_bs_get_binary2, [{f,_},_,Arg2,_,Arg4,Arg5,_]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255, Arg5 >= 0, Arg5 =< 255 -> 1;
fit_args(l_bs_get_binary_all2, [{f,_},{x,0},Arg2,8,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_get_binary_all2, [{f,_},{x,_},Arg2,Arg3,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_get_binary_all2, [{f,_},_,Arg2,Arg3,_]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 2;
fit_args(l_bs_get_binary_all_reuse, [_,{f,_},8]) -> 0;
fit_args(l_bs_get_binary_all_reuse, [_,{f,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_get_binary_imm2, [{f,_},{x,_},Arg2,_,0,_]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_get_binary_imm2, [{f,_},_,Arg2,Arg3,Arg4,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255, Arg4 >= 0, Arg4 =< 255 -> 0;
fit_args(l_bs_get_binary_imm2, [{f,_},_,Arg2,_,Arg4,_]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 2;
fit_args(l_bs_get_float2, [{f,_},_,Arg2,_,Arg4,Arg5,_]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255, Arg5 >= 0, Arg5 =< 255 -> 0;
fit_args(l_bs_get_integer, [{f,_},Arg1,Arg2,Arg3,{x,_}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_get_integer, [{f,_},Arg1,Arg2,Arg3,_]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_get_integer_16, [{x,0},{f,_},{x,_}]) -> 0;
fit_args(l_bs_get_integer_16, [{x,_},{f,_},{x,_}]) -> 1;
fit_args(l_bs_get_integer_16, [_,{f,_},_]) -> 2;
fit_args(l_bs_get_integer_32, [{x,0},{f,_},Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_get_integer_32, [{x,_},{f,_},Arg2,{x,0}]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_bs_get_integer_32, [{x,_},{f,_},Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_get_integer_32, [_,{f,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(l_bs_get_integer_8, [{x,0},{f,_},{x,_}]) -> 0;
fit_args(l_bs_get_integer_8, [{x,_},{f,_},{x,_}]) -> 1;
fit_args(l_bs_get_integer_8, [_,{f,_},_]) -> 2;
fit_args(l_bs_get_integer_imm, [_,Arg1,Arg2,{f,_},Arg4,{x,_}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 0;
fit_args(l_bs_get_integer_imm, [_,_,Arg2,{f,_},Arg4,_]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 1;
fit_args(l_bs_get_integer_small_imm, [_,Arg1,{f,_},Arg3,{x,_}]) when Arg1 >= 0, Arg1 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_get_integer_small_imm, [_,_,{f,_},Arg3,_]) when Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_get_utf16, [{x,0},{f,_},Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_get_utf16, [{x,_},{f,_},Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_get_utf16, [_,{f,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_bs_get_utf8, [{x,_},{f,_},{x,_}]) -> 0;
fit_args(l_bs_get_utf8, [_,{f,_},_]) -> 1;
fit_args(l_bs_init, [_,_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_init_bits, [_,_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_init_bits_fail, [Arg0,{f,_},Arg2,{x,_}]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_init_bits_fail, [_,{f,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_init_fail, [Arg0,{f,_},Arg2,{x,_}]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_init_fail, [_,{f,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_init_heap_bin, [Arg0,Arg1,Arg2,_]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_init_heap_bin, [_,_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_match_string, [{x,0},{f,_},_,{str,_}]) -> 1;
fit_args(l_bs_match_string, [{x,_},{f,_},Arg2,{str,_}]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_match_string, [_,{f,_},_,{str,_}]) -> 2;
fit_args(l_bs_private_append, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_put_string, [2,{str,_}]) -> 2;
fit_args(l_bs_put_string, [4,{str,_}]) -> 0;
fit_args(l_bs_put_string, [1,{str,_}]) -> 1;
fit_args(l_bs_put_string, [6,{str,_}]) -> 3;
fit_args(l_bs_put_string, [3,{str,_}]) -> 4;
fit_args(l_bs_put_string, [_,{str,_}]) -> 5;
fit_args(l_bs_put_utf16, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_put_utf8, [{f,_},_]) -> 0;
fit_args(l_bs_restore2, [{x,0},1]) -> 2;
fit_args(l_bs_restore2, [{x,0},0]) -> 1;
fit_args(l_bs_restore2, [{x,_},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_restore2, [_,_]) -> 3;
fit_args(l_bs_save2, [{x,0},1]) -> 1;
fit_args(l_bs_save2, [{x,_},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_save2, [_,_]) -> 2;
fit_args(l_bs_skip_bits2, [{f,_},{x,_},{x,_},Arg3]) when Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_skip_bits2, [{f,_},_,_,Arg3]) when Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_skip_bits_all2, [{f,_},{x,3},8]) -> 1;
fit_args(l_bs_skip_bits_all2, [{f,_},{x,2},8]) -> 0;
fit_args(l_bs_skip_bits_all2, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_bs_skip_bits_imm2, [{f,_},{x,0},_]) -> 0;
fit_args(l_bs_skip_bits_imm2, [{f,_},{x,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_skip_bits_imm2, [{f,_},_,_]) -> 2;
fit_args(l_bs_start_match2, [{y,1},{f,_},0,0,{x,0}]) -> 2;
fit_args(l_bs_start_match2, [{x,0},{f,_},Arg2,Arg3,{x,0}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_start_match2, [_,{f,_},Arg2,Arg3,{x,0}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 3;
fit_args(l_bs_start_match2, [_,{f,_},Arg2,Arg3,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_start_match2, [_,{f,_},Arg2,_,_]) when Arg2 >= 0, Arg2 =< 255 -> 4;
fit_args(l_bs_test_tail_imm2, [{f,_},_,_]) -> 0;
fit_args(l_bs_test_unit, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_test_unit_8, [{f,_},{x,1}]) -> 3;
fit_args(l_bs_test_unit_8, [{f,_},{x,2}]) -> 1;
fit_args(l_bs_test_unit_8, [{f,_},{x,0}]) -> 0;
fit_args(l_bs_test_unit_8, [{f,_},{x,3}]) -> 2;
fit_args(l_bs_test_unit_8, [{f,_},_]) -> 4;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,1}]) -> 3;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,5}]) -> 5;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,0}]) -> 1;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,3}]) -> 2;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,2}]) -> 0;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,4}]) -> 4;
fit_args(l_bs_test_zero_tail2, [{f,_},_]) -> 6;
fit_args(l_bs_utf16_size, [_,_]) -> 0;
fit_args(l_bs_utf8_size, [_,_]) -> 0;
fit_args(l_bs_validate_unicode, [{f,_},_]) -> 0;
fit_args(l_bs_validate_unicode_retract, [{f,_}]) -> 0;
fit_args(l_bsl, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bsl, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_bsl, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_bsr, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bsr, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_bxor, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_call, [{f,_}]) -> 0;
fit_args(l_call_ext, [{e,{linc,lookup,2}}]) -> 6;
fit_args(l_call_ext, [{e,{proplists,get_value,2}}]) -> 24;
fit_args(l_call_ext, [{e,{lists,reverse,1}}]) -> 0;
fit_args(l_call_ext, [{e,{lists,concat,1}}]) -> 12;
fit_args(l_call_ext, [{e,{ets,tab2list,1}}]) -> 80;
fit_args(l_call_ext, [{e,{os,type,0}}]) -> 28;
fit_args(l_call_ext, [{e,{dict,fetch,2}}]) -> 101;
fit_args(l_call_ext, [{e,{dict,store,3}}]) -> 37;
fit_args(l_call_ext, [{e,{lists,foldl,3}}]) -> 4;
fit_args(l_call_ext, [{e,{lists,mapfoldl,3}}]) -> 25;
fit_args(l_call_ext, [{e,{lists,delete,2}}]) -> 43;
fit_args(l_call_ext, [{e,{lists,flatlength,1}}]) -> 71;
fit_args(l_call_ext, [{e,{os,timestamp,0}}]) -> 96;
fit_args(l_call_ext, [{e,{lists,last,1}}]) -> 51;
fit_args(l_call_ext, [{e,{lists,foreach,2}}]) -> 2;
fit_args(l_call_ext, [{e,{filename,dirname,1}}]) -> 41;
fit_args(l_call_ext, [{e,{proplists,get_value,3}}]) -> 20;
fit_args(l_call_ext, [{e,{ordsets,from_list,1}}]) -> 38;
fit_args(l_call_ext, [{e,{lists,dropwhile,2}}]) -> 66;
fit_args(l_call_ext, [{e,{lists,filter,2}}]) -> 23;
fit_args(l_call_ext, [{e,{ordsets,union,2}}]) -> 11;
fit_args(l_call_ext, [{e,{erlang,list_to_integer,1}}]) -> 16;
fit_args(l_call_ext, [{e,{gb_sets,empty,0}}]) -> 105;
fit_args(l_call_ext, [{e,{lists,sort,1}}]) -> 7;
fit_args(l_call_ext, [{e,{filename,join,1}}]) -> 26;
fit_args(l_call_ext, [{e,{gb_trees,lookup,2}}]) -> 47;
fit_args(l_call_ext, [{e,{dict,new,0}}]) -> 34;
fit_args(l_call_ext, [{e,{mnesia_lib,set,2}}]) -> 17;
fit_args(l_call_ext, [{e,{gb_trees,empty,0}}]) -> 45;
fit_args(l_call_ext, [{e,{gb_trees,insert,3}}]) -> 82;
fit_args(l_call_ext, [{e,{lists,map,2}}]) -> 8;
fit_args(l_call_ext, [{e,{ordsets,intersection,2}}]) -> 109;
fit_args(l_call_ext, [{e,{ordsets,subtract,2}}]) -> 64;
fit_args(l_call_ext, [{e,{lists,splitwith,2}}]) -> 74;
fit_args(l_call_ext, [{e,{dict,find,2}}]) -> 31;
fit_args(l_call_ext, [{e,{filename,basename,1}}]) -> 55;
fit_args(l_call_ext, [{e,{lists,keydelete,3}}]) -> 42;
fit_args(l_call_ext, [{e,{file,read_file_info,1}}]) -> 44;
fit_args(l_call_ext, [{e,{file,close,1}}]) -> 3;
fit_args(l_call_ext, [{e,{ofp_v4_enum,to_atom,2}}]) -> 91;
fit_args(l_call_ext, [{e,{lager_util,format_time,0}}]) -> 61;
fit_args(l_call_ext, [{e,{gb_trees,from_orddict,1}}]) -> 92;
fit_args(l_call_ext, [{e,{lists,append,1}}]) -> 78;
fit_args(l_call_ext, [{e,{sofs,to_external,1}}]) -> 36;
fit_args(l_call_ext, [{e,{lists,duplicate,2}}]) -> 40;
fit_args(l_call_ext, [{e,{ordsets,is_element,2}}]) -> 98;
fit_args(l_call_ext, [{e,{ofp_v4_enum,to_int,2}}]) -> 14;
fit_args(l_call_ext, [{e,{sets,is_element,2}}]) -> 88;
fit_args(l_call_ext, [{e,{gb_trees,to_list,1}}]) -> 86;
fit_args(l_call_ext, [{e,{erlang,term_to_binary,1}}]) -> 84;
fit_args(l_call_ext, [{e,{ofp_utils,padding,2}}]) -> 75;
fit_args(l_call_ext, [{e,{gb_trees,get,2}}]) -> 62;
fit_args(l_call_ext, [{e,{erlang,binary_to_term,1}}]) -> 58;
fit_args(l_call_ext, [{e,{erl_syntax,atom,1}}]) -> 48;
fit_args(l_call_ext, [{e,{erlang,put,2}}]) -> 35;
fit_args(l_call_ext, [{e,{lists,flatten,1}}]) -> 10;
fit_args(l_call_ext, [{e,{mnesia_monitor,use_dir,0}}]) -> 77;
fit_args(l_call_ext, [{e,{file,format_error,1}}]) -> 104;
fit_args(l_call_ext, [{e,{gen_tcp,send,2}}]) -> 65;
fit_args(l_call_ext, [{e,{mnesia_lib,exists,1}}]) -> 59;
fit_args(l_call_ext, [{e,{file,open,2}}]) -> 5;
fit_args(l_call_ext, [{e,{gen_tcp,accept,1}}]) -> 39;
fit_args(l_call_ext, [{e,{filename,join,2}}]) -> 9;
fit_args(l_call_ext, [{e,{asn1ct_name,new,1}}]) -> 108;
fit_args(l_call_ext, [{e,{beam_utils,code_at,2}}]) -> 107;
fit_args(l_call_ext, [{e,{cerl,var_name,1}}]) -> 106;
fit_args(l_call_ext, [{e,{xref_utils,xset,2}}]) -> 103;
fit_args(l_call_ext, [{e,{cerl,c_tuple,1}}]) -> 102;
fit_args(l_call_ext, [{e,{mnesia_lib,intersect,2}}]) -> 100;
fit_args(l_call_ext, [{e,{lists,sublist,3}}]) -> 99;
fit_args(l_call_ext, [{e,{ordsets,add_element,2}}]) -> 97;
fit_args(l_call_ext, [{e,{erlang,binary_to_atom,2}}]) -> 95;
fit_args(l_call_ext, [{e,{ofp_v3_enum,to_atom,2}}]) -> 94;
fit_args(l_call_ext, [{e,{lists,keyreplace,4}}]) -> 93;
fit_args(l_call_ext, [{e,{erl_syntax,get_pos,1}}]) -> 90;
fit_args(l_call_ext, [{e,{inet,sockname,1}}]) -> 89;
fit_args(l_call_ext, [{e,{file,write,2}}]) -> 87;
fit_args(l_call_ext, [{e,{erl_syntax,atom_value,1}}]) -> 85;
fit_args(l_call_ext, [{e,{mnesia_schema,list2cs,1}}]) -> 83;
fit_args(l_call_ext, [{e,{test_server,fail,1}}]) -> 81;
fit_args(l_call_ext, [{e,{inet,port,1}}]) -> 79;
fit_args(l_call_ext, [{e,{mnesia_lib,cs_to_storage_type,2}}]) -> 76;
fit_args(l_call_ext, [{e,{asn1ct_gen,type,1}}]) -> 73;
fit_args(l_call_ext, [{e,{file,rename,2}}]) -> 72;
fit_args(l_call_ext, [{e,{asn1ct_gen,list2name,1}}]) -> 70;
fit_args(l_call_ext, [{e,{sofs,family_union,2}}]) -> 69;
fit_args(l_call_ext, [{e,{lists,seq,2}}]) -> 68;
fit_args(l_call_ext, [{e,{file,read,2}}]) -> 67;
fit_args(l_call_ext, [{e,{inet,getopts,2}}]) -> 63;
fit_args(l_call_ext, [{e,{cerl,get_ann,1}}]) -> 60;
fit_args(l_call_ext, [{e,{erlang,max,2}}]) -> 57;
fit_args(l_call_ext, [{e,{file,read_file,1}}]) -> 56;
fit_args(l_call_ext, [{e,{asn1ct_gen,get_inner,1}}]) -> 54;
fit_args(l_call_ext, [{e,{ssh_channel,cache_lookup,2}}]) -> 53;
fit_args(l_call_ext, [{e,{file,make_dir,1}}]) -> 52;
fit_args(l_call_ext, [{e,{random,uniform,1}}]) -> 50;
fit_args(l_call_ext, [{e,{io,format,3}}]) -> 49;
fit_args(l_call_ext, [{e,{asn1ct_gen,mk_var,1}}]) -> 46;
fit_args(l_call_ext, [{e,{prettypr,beside,2}}]) -> 33;
fit_args(l_call_ext, [{e,{asn1_db,dbget,2}}]) -> 32;
fit_args(l_call_ext, [{e,{string,tokens,2}}]) -> 30;
fit_args(l_call_ext, [{e,{ofp_v3_enum,to_int,2}}]) -> 29;
fit_args(l_call_ext, [{e,{prettypr,floating,1}}]) -> 27;
fit_args(l_call_ext, [{e,{erl_syntax,type,1}}]) -> 22;
fit_args(l_call_ext, [{e,{test_server,lookup_config,2}}]) -> 21;
fit_args(l_call_ext, [{e,{test_server,timetrap_cancel,1}}]) -> 19;
fit_args(l_call_ext, [{e,{file,delete,1}}]) -> 18;
fit_args(l_call_ext, [{e,{gen_tcp,close,1}}]) -> 15;
fit_args(l_call_ext, [{e,{test_server,timetrap,1}}]) -> 13;
fit_args(l_call_ext, [{e,{asn1ct_gen,emit,1}}]) -> 1;
fit_args(l_call_ext, [{e,_}]) -> 110;
fit_args(l_call_ext_last, [{e,_},1]) -> 0;
fit_args(l_call_ext_last, [{e,_},0]) -> 1;
fit_args(l_call_ext_last, [{e,_},2]) -> 2;
fit_args(l_call_ext_last, [{e,_},3]) -> 3;
fit_args(l_call_ext_last, [{e,_},4]) -> 4;
fit_args(l_call_ext_last, [{e,_},5]) -> 5;
fit_args(l_call_ext_last, [{e,_},_]) -> 6;
fit_args(l_call_ext_only, [{e,{gen_server,call,2}}]) -> 2;
fit_args(l_call_ext_only, [{e,{lists,reverse,1}}]) -> 4;
fit_args(l_call_ext_only, [{e,{gen_server,call,3}}]) -> 0;
fit_args(l_call_ext_only, [{e,{mnesia_monitor,get_env,1}}]) -> 3;
fit_args(l_call_ext_only, [{e,{asn1ct_gen,emit,1}}]) -> 1;
fit_args(l_call_ext_only, [{e,_}]) -> 5;
fit_args(l_call_fun, [1]) -> 0;
fit_args(l_call_fun, [2]) -> 3;
fit_args(l_call_fun, [3]) -> 2;
fit_args(l_call_fun, [0]) -> 1;
fit_args(l_call_fun, [Arg0]) when Arg0 >= 0, Arg0 =< 255 -> 4;
fit_args(l_call_fun_last, [Arg0,Arg1]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_call_fun_last, [Arg0,_]) when Arg0 >= 0, Arg0 =< 255 -> 1;
fit_args(l_call_last, [{f,_},2]) -> 0;
fit_args(l_call_last, [{f,_},6]) -> 6;
fit_args(l_call_last, [{f,_},1]) -> 4;
fit_args(l_call_last, [{f,_},0]) -> 1;
fit_args(l_call_last, [{f,_},4]) -> 3;
fit_args(l_call_last, [{f,_},3]) -> 2;
fit_args(l_call_last, [{f,_},7]) -> 7;
fit_args(l_call_last, [{f,_},5]) -> 5;
fit_args(l_call_last, [{f,_},8]) -> 8;
fit_args(l_call_last, [{f,_},10]) -> 10;
fit_args(l_call_last, [{f,_},9]) -> 9;
fit_args(l_call_last, [{f,_},_]) -> 11;
fit_args(l_call_only, [{f,_}]) -> 0;
fit_args(l_catch, [{y,5},_]) -> 5;
fit_args(l_catch, [{y,0},_]) -> 0;
fit_args(l_catch, [{y,2},_]) -> 2;
fit_args(l_catch, [{y,4},_]) -> 4;
fit_args(l_catch, [{y,6},_]) -> 6;
fit_args(l_catch, [{y,3},_]) -> 3;
fit_args(l_catch, [{y,1},_]) -> 1;
fit_args(l_catch, [{y,20},_]) -> 7;
fit_args(l_catch, [_,_]) -> 8;
fit_args(l_element, [_,_,_]) -> 0;
fit_args(l_fadd, [{fr,_},{fr,_},{fr,_}]) -> 0;
fit_args(l_fast_element, [{x,0},2,{x,0}]) -> 0;
fit_args(l_fast_element, [{x,0},3,{x,0}]) -> 4;
fit_args(l_fast_element, [{x,0},1,_]) -> 1;
fit_args(l_fast_element, [{x,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_fast_element, [_,2,_]) -> 3;
fit_args(l_fast_element, [_,_,_]) -> 5;
fit_args(l_fcheckerror, []) -> 0;
fit_args(l_fdiv, [{fr,_},{fr,_},{fr,_}]) -> 0;
fit_args(l_fetch, [{y,0},{x,2}]) -> 7;
fit_args(l_fetch, [{i,Arg0},{y,Arg1}]) when Arg0 >= -128, Arg0 =< 127, Arg1 >= 0, Arg1 =< 255 -> 11;
fit_args(l_fetch, [{i,Arg0},{x,_}]) when Arg0 >= -128, Arg0 =< 127 -> 5;
fit_args(l_fetch, [{x,0},_]) -> 0;
fit_args(l_fetch, [{x,1},_]) -> 10;
fit_args(l_fetch, [{x,4},_]) -> 13;
fit_args(l_fetch, [{x,3},_]) -> 14;
fit_args(l_fetch, [{x,5},_]) -> 16;
fit_args(l_fetch, [{x,2},_]) -> 12;
fit_args(l_fetch, [{y,0},_]) -> 19;
fit_args(l_fetch, [{y,1},_]) -> 21;
fit_args(l_fetch, [{y,Arg0},{i,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= -128, Arg1 =< 127 -> 9;
fit_args(l_fetch, [{x,_},{i,Arg1}]) when Arg1 >= -128, Arg1 =< 127 -> 3;
fit_args(l_fetch, [{x,_},{x,_}]) -> 2;
fit_args(l_fetch, [{x,_},{y,Arg1}]) when Arg1 >= 0, Arg1 =< 255 -> 4;
fit_args(l_fetch, [{y,Arg0},{y,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 6;
fit_args(l_fetch, [_,{x,0}]) -> 1;
fit_args(l_fetch, [_,{x,3}]) -> 18;
fit_args(l_fetch, [_,{x,2}]) -> 17;
fit_args(l_fetch, [_,{x,1}]) -> 8;
fit_args(l_fetch, [_,{x,5}]) -> 20;
fit_args(l_fetch, [_,{x,4}]) -> 15;
fit_args(l_fetch, [_,{y,5}]) -> 22;
fit_args(l_fetch, [_,_]) -> 23;
fit_args(l_fmul, [{fr,_},{fr,_},{fr,_}]) -> 0;
fit_args(l_fnegate, [{fr,_},{fr,_}]) -> 0;
fit_args(l_fsub, [{fr,_},{fr,_},{fr,_}]) -> 0;
fit_args(l_gc_bif1, [{f,_},{b,{erlang,length,1}},_,Arg3,{x,0}]) when Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_gc_bif1, [{f,_},{b,{erlang,length,1}},_,Arg3,{y,Arg4}]) when Arg3 >= 0, Arg3 =< 255, Arg4 >= 0, Arg4 =< 255 -> 3;
fit_args(l_gc_bif1, [{f,_},{b,_},_,Arg3,{x,0}]) when Arg3 >= 0, Arg3 =< 255 -> 2;
fit_args(l_gc_bif1, [{f,_},{b,_},_,Arg3,{x,_}]) when Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_gc_bif1, [{f,_},{b,_},_,Arg3,_]) when Arg3 >= 0, Arg3 =< 255 -> 4;
fit_args(l_gc_bif2, [{f,_},{b,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_gc_bif3, [{f,_},{b,_},_,Arg3,_]) when Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_get, [{a,asn1_module},{y,0}]) -> 0;
fit_args(l_get, [{a,asn1_module},{y,Arg1}]) when Arg1 >= 0, Arg1 =< 255 -> 4;
fit_args(l_get, [{a,mnesia_activity_state},_]) -> 5;
fit_args(l_get, [_,{x,0}]) -> 2;
fit_args(l_get, [_,{x,1}]) -> 1;
fit_args(l_get, [_,{x,2}]) -> 3;
fit_args(l_get, [_,_]) -> 6;
fit_args(l_hibernate, []) -> 0;
fit_args(l_increment, [{x,0},4294967295,Arg2,{y,Arg3}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 6;
fit_args(l_increment, [{x,0},_,Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 4;
fit_args(l_increment, [{x,_},Arg1,Arg2,_]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_increment, [{x,_},_,Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(l_increment, [{y,Arg0},_,Arg2,{x,_}]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_increment, [_,Arg1,Arg2,{y,Arg3}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 5;
fit_args(l_increment, [_,_,Arg2,{x,0}]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_increment, [_,_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 7;
fit_args(l_int_bnot, [{f,_},_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_int_div, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_int_div, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_int_div, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_is_eq, [{f,_}]) -> 0;
fit_args(l_is_eq_exact, [{f,_}]) -> 0;
fit_args(l_is_eq_exact_immed, [{f,_},{y,10},{a,ber}]) -> 23;
fit_args(l_is_eq_exact_immed, [{f,_},{x,255},{a,xmerl_scanner}]) -> 10;
fit_args(l_is_eq_exact_immed, [{f,_},{x,3},_]) -> 2;
fit_args(l_is_eq_exact_immed, [{f,_},{x,2},_]) -> 3;
fit_args(l_is_eq_exact_immed, [{f,_},{x,1},_]) -> 0;
fit_args(l_is_eq_exact_immed, [{f,_},{x,0},_]) -> 1;
fit_args(l_is_eq_exact_immed, [{f,_},{x,5},_]) -> 6;
fit_args(l_is_eq_exact_immed, [{f,_},{x,4},_]) -> 5;
fit_args(l_is_eq_exact_immed, [{f,_},{x,6},_]) -> 7;
fit_args(l_is_eq_exact_immed, [{f,_},{x,7},_]) -> 8;
fit_args(l_is_eq_exact_immed, [{f,_},{x,14},_]) -> 19;
fit_args(l_is_eq_exact_immed, [{f,_},{y,3},_]) -> 21;
fit_args(l_is_eq_exact_immed, [{f,_},{x,10},_]) -> 12;
fit_args(l_is_eq_exact_immed, [{f,_},{y,5},_]) -> 27;
fit_args(l_is_eq_exact_immed, [{f,_},{x,8},_]) -> 9;
fit_args(l_is_eq_exact_immed, [{f,_},{x,9},_]) -> 11;
fit_args(l_is_eq_exact_immed, [{f,_},{y,7},_]) -> 35;
fit_args(l_is_eq_exact_immed, [{f,_},{y,0},_]) -> 16;
fit_args(l_is_eq_exact_immed, [{f,_},{y,4},_]) -> 26;
fit_args(l_is_eq_exact_immed, [{f,_},{x,255},_]) -> 18;
fit_args(l_is_eq_exact_immed, [{f,_},{y,2},_]) -> 22;
fit_args(l_is_eq_exact_immed, [{f,_},{y,1},_]) -> 15;
fit_args(l_is_eq_exact_immed, [{f,_},{x,13},_]) -> 20;
fit_args(l_is_eq_exact_immed, [{f,_},{x,11},_]) -> 13;
fit_args(l_is_eq_exact_immed, [{f,_},{y,6},_]) -> 30;
fit_args(l_is_eq_exact_immed, [{f,_},{x,15},_]) -> 24;
fit_args(l_is_eq_exact_immed, [{f,_},{x,12},_]) -> 14;
fit_args(l_is_eq_exact_immed, [{f,_},{x,16},_]) -> 25;
fit_args(l_is_eq_exact_immed, [{f,_},{x,17},_]) -> 28;
fit_args(l_is_eq_exact_immed, [{f,_},{x,18},_]) -> 29;
fit_args(l_is_eq_exact_immed, [{f,_},{x,19},_]) -> 31;
fit_args(l_is_eq_exact_immed, [{f,_},{x,23},_]) -> 34;
fit_args(l_is_eq_exact_immed, [{f,_},{x,20},_]) -> 33;
fit_args(l_is_eq_exact_immed, [{f,_},{x,22},_]) -> 32;
fit_args(l_is_eq_exact_immed, [{f,_},{y,Arg1},{a,asn1_NOVALUE}]) when Arg1 >= 0, Arg1 =< 255 -> 17;
fit_args(l_is_eq_exact_immed, [{f,_},{x,_},{i,Arg2}]) when Arg2 >= -128, Arg2 =< 127 -> 4;
fit_args(l_is_eq_exact_immed, [{f,_},_,_]) -> 36;
fit_args(l_is_eq_exact_literal, [{f,_},{x,1},_]) -> 1;
fit_args(l_is_eq_exact_literal, [{f,_},{x,5},_]) -> 5;
fit_args(l_is_eq_exact_literal, [{f,_},{x,3},_]) -> 4;
fit_args(l_is_eq_exact_literal, [{f,_},{x,0},_]) -> 0;
fit_args(l_is_eq_exact_literal, [{f,_},{x,6},_]) -> 6;
fit_args(l_is_eq_exact_literal, [{f,_},{x,4},_]) -> 3;
fit_args(l_is_eq_exact_literal, [{f,_},{x,2},_]) -> 2;
fit_args(l_is_eq_exact_literal, [{f,_},_,_]) -> 7;
fit_args(l_is_function2, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_is_ge, [{f,_}]) -> 0;
fit_args(l_is_lt, [{f,_}]) -> 0;
fit_args(l_is_ne, [{f,_}]) -> 0;
fit_args(l_is_ne_exact, [{f,_}]) -> 0;
fit_args(l_is_ne_exact_immed, [{f,_},{x,2},_]) -> 2;
fit_args(l_is_ne_exact_immed, [{f,_},{x,3},_]) -> 4;
fit_args(l_is_ne_exact_immed, [{f,_},{x,6},_]) -> 10;
fit_args(l_is_ne_exact_immed, [{f,_},{x,0},_]) -> 0;
fit_args(l_is_ne_exact_immed, [{f,_},{x,1},_]) -> 1;
fit_args(l_is_ne_exact_immed, [{f,_},{x,5},_]) -> 8;
fit_args(l_is_ne_exact_immed, [{f,_},{y,0},_]) -> 5;
fit_args(l_is_ne_exact_immed, [{f,_},{x,4},_]) -> 6;
fit_args(l_is_ne_exact_immed, [{f,_},{y,2},_]) -> 9;
fit_args(l_is_ne_exact_immed, [{f,_},{y,1},_]) -> 7;
fit_args(l_is_ne_exact_immed, [{f,_},_,{a,true}]) -> 3;
fit_args(l_is_ne_exact_immed, [{f,_},_,_]) -> 11;
fit_args(l_is_ne_exact_literal, [{f,_},_,_]) -> 0;
fit_args(l_jump_on_val, [{x,0},{f,_},Arg2,Arg3]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_jump_on_val, [{x,_},{f,_},Arg2,Arg3]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_jump_on_val, [_,{f,_},_,_]) -> 2;
fit_args(l_loop_rec, [{f,_}]) -> 0;
fit_args(l_m_div, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_make_export, [{e,_}]) -> 0;
fit_args(l_make_fun, [{fu,_}]) -> 0;
fit_args(l_minus, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_minus, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_minus, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_move_call, [{x,1},{f,_}]) -> 1;
fit_args(l_move_call, [{y,1},{f,_}]) -> 4;
fit_args(l_move_call, [{y,0},{f,_}]) -> 3;
fit_args(l_move_call, [{x,2},{f,_}]) -> 0;
fit_args(l_move_call, [{x,4},{f,_}]) -> 12;
fit_args(l_move_call, [{y,2},{f,_}]) -> 2;
fit_args(l_move_call, [{x,3},{f,_}]) -> 5;
fit_args(l_move_call, [{y,3},{f,_}]) -> 6;
fit_args(l_move_call, [{y,4},{f,_}]) -> 7;
fit_args(l_move_call, [{y,5},{f,_}]) -> 8;
fit_args(l_move_call, [{y,6},{f,_}]) -> 10;
fit_args(l_move_call, [{x,5},{f,_}]) -> 13;
fit_args(l_move_call, [{x,6},{f,_}]) -> 25;
fit_args(l_move_call, [nil,{f,_}]) -> 11;
fit_args(l_move_call, [{a,schema},{f,_}]) -> 28;
fit_args(l_move_call, [{y,7},{f,_}]) -> 14;
fit_args(l_move_call, [{y,8},{f,_}]) -> 35;
fit_args(l_move_call, [{a,x},{f,_}]) -> 33;
fit_args(l_move_call, [{a,atom},{f,_}]) -> 30;
fit_args(l_move_call, [{a,ets},{f,_}]) -> 29;
fit_args(l_move_call, [{a,add},{f,_}]) -> 26;
fit_args(l_move_call, [{a,false},{f,_}]) -> 23;
fit_args(l_move_call, [{a,endDocument},{f,_}]) -> 22;
fit_args(l_move_call, [{a,foo},{f,_}]) -> 20;
fit_args(l_move_call, [{y,12},{f,_}]) -> 19;
fit_args(l_move_call, [{smallint,0},{f,_}]) -> 15;
fit_args(l_move_call, [{smallint,2},{f,_}]) -> 16;
fit_args(l_move_call, [{smallint,3},{f,_}]) -> 17;
fit_args(l_move_call, [{smallint,1},{f,_}]) -> 9;
fit_args(l_move_call, [{smallint,42},{f,_}]) -> 34;
fit_args(l_move_call, [{smallint,4},{f,_}]) -> 32;
fit_args(l_move_call, [{smallint,1000},{f,_}]) -> 31;
fit_args(l_move_call, [{smallint,6},{f,_}]) -> 27;
fit_args(l_move_call, [{smallint,9},{f,_}]) -> 24;
fit_args(l_move_call, [{smallint,100},{f,_}]) -> 21;
fit_args(l_move_call, [{smallint,12},{f,_}]) -> 18;
fit_args(l_move_call, [_,{f,_}]) -> 36;
fit_args(l_move_call_ext, [{a,loglevel},{e,{lager_mochiglobal,get,2}}]) -> 22;
fit_args(l_move_call_ext, [{a,error},{e,{lager_util,level_to_num,1}}]) -> 17;
fit_args(l_move_call_ext, [{a,auto_repair},{e,{mnesia_monitor,get_env,1}}]) -> 42;
fit_args(l_move_call_ext, [{a,funky},{e,{estone_SUITE,req,2}}]) -> 33;
fit_args(l_move_call_ext, [{y,3},{e,_}]) -> 7;
fit_args(l_move_call_ext, [{y,4},{e,_}]) -> 8;
fit_args(l_move_call_ext, [{x,2},{e,_}]) -> 9;
fit_args(l_move_call_ext, [{x,1},{e,_}]) -> 6;
fit_args(l_move_call_ext, [{y,2},{e,_}]) -> 5;
fit_args(l_move_call_ext, [{x,3},{e,_}]) -> 15;
fit_args(l_move_call_ext, [{y,0},{e,_}]) -> 2;
fit_args(l_move_call_ext, [{x,4},{e,_}]) -> 38;
fit_args(l_move_call_ext, [{y,1},{e,_}]) -> 3;
fit_args(l_move_call_ext, [{a,linc},{e,_}]) -> 26;
fit_args(l_move_call_ext, [{y,7},{e,_}]) -> 24;
fit_args(l_move_call_ext, [{y,5},{e,_}]) -> 16;
fit_args(l_move_call_ext, [{y,6},{e,_}]) -> 12;
fit_args(l_move_call_ext, [{a,schema},{e,_}]) -> 47;
fit_args(l_move_call_ext, [nil,{e,_}]) -> 19;
fit_args(l_move_call_ext, [{a,func},{e,_}]) -> 48;
fit_args(l_move_call_ext, [{smallint,10},{e,{test_server,seconds,1}}]) -> 39;
fit_args(l_move_call_ext, [{smallint,5},{e,{test_server,seconds,1}}]) -> 23;
fit_args(l_move_call_ext, [{smallint,1},{e,_}]) -> 4;
fit_args(l_move_call_ext, [{smallint,0},{e,_}]) -> 1;
fit_args(l_move_call_ext, [{smallint,2},{e,_}]) -> 46;
fit_args(l_move_call_ext, [_,{e,{erlang,put,2}}]) -> 13;
fit_args(l_move_call_ext, [_,{e,{proplists,get_value,3}}]) -> 14;
fit_args(l_move_call_ext, [_,{e,{proplists,get_value,2}}]) -> 21;
fit_args(l_move_call_ext, [_,{e,{io_lib,format,2}}]) -> 10;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,dbg_out,2}}]) -> 36;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,set,2}}]) -> 44;
fit_args(l_move_call_ext, [_,{e,{erlang,erase,1}}]) -> 37;
fit_args(l_move_call_ext, [_,{e,{lager,dispatch_log,9}}]) -> 28;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,verbose,2}}]) -> 25;
fit_args(l_move_call_ext, [_,{e,{timer,sleep,1}}]) -> 35;
fit_args(l_move_call_ext, [_,{e,{erlang,system_info,1}}]) -> 31;
fit_args(l_move_call_ext, [_,{e,{application,start,1}}]) -> 49;
fit_args(l_move_call_ext, [_,{e,{lists,sublist,3}}]) -> 45;
fit_args(l_move_call_ext, [_,{e,{io,format,1}}]) -> 43;
fit_args(l_move_call_ext, [_,{e,{meck,expect,3}}]) -> 41;
fit_args(l_move_call_ext, [_,{e,{gen_tcp,connect,3}}]) -> 40;
fit_args(l_move_call_ext, [_,{e,{ofp_v3_utils,flow_add,3}}]) -> 34;
fit_args(l_move_call_ext, [_,{e,{ofp_v4_utils,flow_add,3}}]) -> 32;
fit_args(l_move_call_ext, [_,{e,{lists,duplicate,2}}]) -> 30;
fit_args(l_move_call_ext, [_,{e,{lists,seq,2}}]) -> 29;
fit_args(l_move_call_ext, [_,{e,{test_server,lookup_config,2}}]) -> 27;
fit_args(l_move_call_ext, [_,{e,{asn1ct_name,new,1}}]) -> 20;
fit_args(l_move_call_ext, [_,{e,{prettypr,text,1}}]) -> 18;
fit_args(l_move_call_ext, [_,{e,{asn1ct_gen,emit,1}}]) -> 11;
fit_args(l_move_call_ext, [_,{e,{io,format,2}}]) -> 0;
fit_args(l_move_call_ext, [_,{e,_}]) -> 50;
fit_args(l_move_call_ext_last, [{e,{gen_event,notify,2}},5,{a,lager_event}]) -> 3;
fit_args(l_move_call_ext_last, [{e,_},1,_]) -> 2;
fit_args(l_move_call_ext_last, [{e,_},2,_]) -> 4;
fit_args(l_move_call_ext_last, [{e,_},3,_]) -> 5;
fit_args(l_move_call_ext_last, [{e,_},0,_]) -> 1;
fit_args(l_move_call_ext_last, [{e,_},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_move_call_ext_last, [{e,_},_,_]) -> 6;
fit_args(l_move_call_ext_only, [{e,{lists,reverse,1}},{x,1}]) -> 1;
fit_args(l_move_call_ext_only, [{e,{ofp_v4_enum,to_int,2}},{a,type}]) -> 4;
fit_args(l_move_call_ext_only, [{e,{ofp_v3_enum,to_int,2}},{a,type}]) -> 5;
fit_args(l_move_call_ext_only, [{e,{erlang,get_module_info,1}},_]) -> 0;
fit_args(l_move_call_ext_only, [{e,{io_lib,format,2}},_]) -> 2;
fit_args(l_move_call_ext_only, [{e,{eunit,test,1}},_]) -> 9;
fit_args(l_move_call_ext_only, [{e,{io,format,2}},_]) -> 8;
fit_args(l_move_call_ext_only, [{e,_},{x,2}]) -> 3;
fit_args(l_move_call_ext_only, [{e,_},{x,3}]) -> 6;
fit_args(l_move_call_ext_only, [{e,_},{x,1}]) -> 7;
fit_args(l_move_call_ext_only, [{e,_},_]) -> 10;
fit_args(l_move_call_last, [{f,_},2,_]) -> 3;
fit_args(l_move_call_last, [{f,_},3,_]) -> 5;
fit_args(l_move_call_last, [{f,_},1,_]) -> 1;
fit_args(l_move_call_last, [{f,_},0,_]) -> 4;
fit_args(l_move_call_last, [{f,_},4,_]) -> 6;
fit_args(l_move_call_last, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_move_call_last, [{f,_},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_move_call_last, [{f,_},_,_]) -> 7;
fit_args(l_move_call_only, [{f,_},{x,4}]) -> 3;
fit_args(l_move_call_only, [{f,_},{x,2}]) -> 1;
fit_args(l_move_call_only, [{f,_},nil]) -> 6;
fit_args(l_move_call_only, [{f,_},{x,3}]) -> 2;
fit_args(l_move_call_only, [{f,_},{x,5}]) -> 4;
fit_args(l_move_call_only, [{f,_},{x,7}]) -> 8;
fit_args(l_move_call_only, [{f,_},{x,6}]) -> 5;
fit_args(l_move_call_only, [{f,_},{x,1}]) -> 0;
fit_args(l_move_call_only, [{f,_},{x,8}]) -> 7;
fit_args(l_move_call_only, [{f,_},{smallint,1}]) -> 9;
fit_args(l_move_call_only, [{f,_},_]) -> 10;
fit_args(l_new_bs_put_binary, [{f,_},_,Arg2,Arg3,_]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_new_bs_put_binary_all, [{f,_},_,8]) -> 0;
fit_args(l_new_bs_put_binary_all, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_new_bs_put_binary_imm, [{f,_},Arg1,Arg2,{x,0}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_new_bs_put_binary_imm, [{f,_},Arg1,Arg2,{x,_}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_new_bs_put_binary_imm, [{f,_},Arg1,Arg2,{y,Arg3}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_new_bs_put_binary_imm, [{f,_},_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(l_new_bs_put_float, [{f,_},_,Arg2,Arg3,_]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_new_bs_put_float_imm, [{f,_},64,0,{x,0}]) -> 0;
fit_args(l_new_bs_put_float_imm, [{f,_},_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_new_bs_put_integer, [{f,_},_,1,0,{smallint,0}]) -> 0;
fit_args(l_new_bs_put_integer, [{f,_},_,1,0,_]) -> 1;
fit_args(l_new_bs_put_integer, [{f,_},_,Arg2,Arg3,_]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 2;
fit_args(l_new_bs_put_integer_imm, [{f,_},Arg1,Arg2,_]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_new_bs_put_integer_imm, [{f,_},_,0,{smallint,0}]) -> 1;
fit_args(l_new_bs_put_integer_imm, [{f,_},_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_plus, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_plus, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_plus, [{f,_},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_plus, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 3;
fit_args(l_put_tuple, [{x,0},5]) -> 4;
fit_args(l_put_tuple, [{x,0},4]) -> 3;
fit_args(l_put_tuple, [{x,0},3]) -> 2;
fit_args(l_put_tuple, [{x,0},2]) -> 1;
fit_args(l_put_tuple, [{x,0},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 5;
fit_args(l_put_tuple, [{x,_},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_put_tuple, [{y,Arg0},Arg1]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 6;
fit_args(l_put_tuple, [_,_]) -> 7;
fit_args(l_recv_set, []) -> 0;
fit_args(l_rem, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_rem, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_rem, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_select_tuple_arity, [{x,0},{f,_},_]) -> 0;
fit_args(l_select_tuple_arity, [{x,_},{f,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_select_tuple_arity, [_,{f,_},_]) -> 2;
fit_args(l_select_tuple_arity2, [{x,0},{f,_},Arg2,{f,_},Arg4,{f,_}]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 0;
fit_args(l_select_tuple_arity2, [{x,_},{f,_},Arg2,{f,_},Arg4,{f,_}]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 1;
fit_args(l_select_tuple_arity2, [{y,Arg0},{f,_},Arg2,{f,_},Arg4,{f,_}]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 2;
fit_args(l_select_tuple_arity2, [_,{f,_},_,{f,_},_,{f,_}]) -> 3;
fit_args(l_select_val2, [{x,0},{f,_},_,{f,_},_,{f,_}]) -> 0;
fit_args(l_select_val2, [{x,6},{f,_},_,{f,_},_,{f,_}]) -> 9;
fit_args(l_select_val2, [{x,1},{f,_},_,{f,_},_,{f,_}]) -> 2;
fit_args(l_select_val2, [{x,5},{f,_},_,{f,_},_,{f,_}]) -> 8;
fit_args(l_select_val2, [{x,2},{f,_},_,{f,_},_,{f,_}]) -> 3;
fit_args(l_select_val2, [{x,8},{f,_},_,{f,_},_,{f,_}]) -> 12;
fit_args(l_select_val2, [{y,2},{f,_},_,{f,_},_,{f,_}]) -> 13;
fit_args(l_select_val2, [{x,4},{f,_},_,{f,_},_,{f,_}]) -> 7;
fit_args(l_select_val2, [{x,3},{f,_},_,{f,_},_,{f,_}]) -> 4;
fit_args(l_select_val2, [{x,7},{f,_},_,{f,_},_,{f,_}]) -> 10;
fit_args(l_select_val2, [{y,1},{f,_},_,{f,_},_,{f,_}]) -> 11;
fit_args(l_select_val2, [{x,_},{f,_},{i,Arg2},{f,_},{i,Arg4},{f,_}]) when Arg2 >= -128, Arg2 =< 127, Arg4 >= -128, Arg4 =< 127 -> 1;
fit_args(l_select_val2, [_,{f,_},{a,false},{f,_},{a,true},{f,_}]) -> 6;
fit_args(l_select_val2, [_,{f,_},{a,true},{f,_},{a,false},{f,_}]) -> 5;
fit_args(l_select_val2, [_,{f,_},_,{f,_},_,{f,_}]) -> 14;
fit_args(l_select_val_atoms, [{x,0},{f,_},_]) -> 1;
fit_args(l_select_val_atoms, [{x,_},{f,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_select_val_atoms, [{y,Arg0},{f,_},Arg2]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_select_val_atoms, [_,{f,_},_]) -> 3;
fit_args(l_select_val_smallints, [{x,0},{f,_},_]) -> 1;
fit_args(l_select_val_smallints, [{x,_},{f,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_select_val_smallints, [_,{f,_},_]) -> 2;
fit_args(l_times, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_times, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_times, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_trim, [1]) -> 0;
fit_args(l_trim, [2]) -> 1;
fit_args(l_trim, [4]) -> 3;
fit_args(l_trim, [5]) -> 4;
fit_args(l_trim, [3]) -> 2;
fit_args(l_trim, [6]) -> 5;
fit_args(l_trim, [9]) -> 8;
fit_args(l_trim, [7]) -> 6;
fit_args(l_trim, [8]) -> 7;
fit_args(l_trim, [11]) -> 10;
fit_args(l_trim, [10]) -> 9;
fit_args(l_trim, [_]) -> 11;
fit_args(l_wait_timeout, [{f,_},5000]) -> 3;
fit_args(l_wait_timeout, [{f,_},100]) -> 2;
fit_args(l_wait_timeout, [{f,_},10000]) -> 1;
fit_args(l_wait_timeout, [{f,_},3000]) -> 4;
fit_args(l_wait_timeout, [{f,_},1000]) -> 0;
fit_args(l_wait_timeout, [{f,_},_]) -> 5;
fit_args(l_yield, []) -> 0;
fit_args(loop_rec_end, [{f,_}]) -> 0;
fit_args(move, [nil,{x,10}]) -> 12;
fit_args(move, [{x,0},_]) -> 2;
fit_args(move, [{smallint,4096},{x,8}]) -> 10;
fit_args(move, [{x,_},{y,Arg1}]) when Arg1 >= 0, Arg1 =< 255 -> 3;
fit_args(move, [{y,Arg0},{y,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 7;
fit_args(move, [_,{x,1}]) -> 0;
fit_args(move, [_,{x,0}]) -> 1;
fit_args(move, [_,{x,2}]) -> 4;
fit_args(move, [_,{x,3}]) -> 5;
fit_args(move, [_,{x,4}]) -> 6;
fit_args(move, [_,{x,5}]) -> 8;
fit_args(move, [_,{x,7}]) -> 11;
fit_args(move, [_,{x,6}]) -> 9;
fit_args(move, [_,_]) -> 13;
fit_args(move2, [{x,0},{x,_},{x,0},{x,_}]) -> 9;
fit_args(move2, [{x,0},{x,_},{x,_},{x,0}]) -> 7;
fit_args(move2, [{x,0},{x,_},{x,_},{x,_}]) -> 8;
fit_args(move2, [{x,0},{y,Arg1},{x,_},{y,Arg3}]) when Arg1 >= 0, Arg1 =< 255, Arg3 >= 0, Arg3 =< 255 -> 4;
fit_args(move2, [{x,_},{x,0},{x,_},{x,_}]) -> 5;
fit_args(move2, [{x,_},{x,_},{x,0},{x,_}]) -> 6;
fit_args(move2, [{x,_},{y,Arg1},{x,0},{y,Arg3}]) when Arg1 >= 0, Arg1 =< 255, Arg3 >= 0, Arg3 =< 255 -> 2;
fit_args(move2, [{x,_},{y,Arg1},{x,_},{y,Arg3}]) when Arg1 >= 0, Arg1 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(move2, [{y,Arg0},{x,_},{y,Arg2},_]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(move2, [{x,_},{x,_},{x,_},_]) -> 3;
fit_args(move2, [_,_,_,_]) -> 10;
fit_args(move_deallocate_return, [{a,ok},_]) -> 1;
fit_args(move_deallocate_return, [{x,_},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(move_deallocate_return, [{y,Arg0},Arg1]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(move_deallocate_return, [_,0]) -> 3;
fit_args(move_deallocate_return, [_,4]) -> 7;
fit_args(move_deallocate_return, [_,1]) -> 4;
fit_args(move_deallocate_return, [_,3]) -> 6;
fit_args(move_deallocate_return, [_,2]) -> 5;
fit_args(move_deallocate_return, [_,5]) -> 8;
fit_args(move_deallocate_return, [_,6]) -> 9;
fit_args(move_deallocate_return, [_,_]) -> 10;
fit_args(move_jump, [{f,_},{x,3}]) -> 3;
fit_args(move_jump, [{f,_},{x,1}]) -> 0;
fit_args(move_jump, [{f,_},nil]) -> 2;
fit_args(move_jump, [{f,_},{y,1}]) -> 7;
fit_args(move_jump, [{f,_},{y,2}]) -> 6;
fit_args(move_jump, [{f,_},{x,2}]) -> 1;
fit_args(move_jump, [{f,_},{a,ok}]) -> 10;
fit_args(move_jump, [{f,_},{a,true}]) -> 11;
fit_args(move_jump, [{f,_},{x,4}]) -> 4;
fit_args(move_jump, [{f,_},{y,0}]) -> 5;
fit_args(move_jump, [{f,_},{a,asn1_NOVALUE}]) -> 9;
fit_args(move_jump, [{f,_},{a,false}]) -> 8;
fit_args(move_jump, [{f,_},{smallint,0}]) -> 12;
fit_args(move_jump, [{f,_},_]) -> 13;
fit_args(move_return, [{x,2}]) -> 3;
fit_args(move_return, [{a,false}]) -> 5;
fit_args(move_return, [{x,1}]) -> 1;
fit_args(move_return, [nil]) -> 4;
fit_args(move_return, [{a,true}]) -> 0;
fit_args(move_return, [{x,3}]) -> 6;
fit_args(move_return, [{a,ok}]) -> 2;
fit_args(move_return, [{x,5}]) -> 26;
fit_args(move_return, [{a,no}]) -> 22;
fit_args(move_return, [{x,4}]) -> 10;
fit_args(move_return, [{a,error}]) -> 21;
fit_args(move_return, [{a,none}]) -> 28;
fit_args(move_return, [{a,undefined}]) -> 19;
fit_args(move_return, [{a,all}]) -> 46;
fit_args(move_return, [{a,delete}]) -> 40;
fit_args(move_return, [{a,bad_len}]) -> 44;
fit_args(move_return, [{x,6}]) -> 42;
fit_args(move_return, [{a,nomatch}]) -> 35;
fit_args(move_return, [{a,experimenter}]) -> 31;
fit_args(move_return, [{a,ignore}]) -> 30;
fit_args(move_return, [{a,eperm}]) -> 27;
fit_args(move_return, [{smallint,6}]) -> 14;
fit_args(move_return, [{smallint,0}]) -> 8;
fit_args(move_return, [{smallint,10}]) -> 17;
fit_args(move_return, [{smallint,1}]) -> 7;
fit_args(move_return, [{smallint,7}]) -> 16;
fit_args(move_return, [{smallint,32}]) -> 36;
fit_args(move_return, [{smallint,2}]) -> 9;
fit_args(move_return, [{smallint,3}]) -> 11;
fit_args(move_return, [{smallint,22}]) -> 45;
fit_args(move_return, [{smallint,64}]) -> 29;
fit_args(move_return, [{smallint,8}]) -> 13;
fit_args(move_return, [{smallint,14}]) -> 34;
fit_args(move_return, [{smallint,4}]) -> 12;
fit_args(move_return, [{smallint,19}]) -> 47;
fit_args(move_return, [{smallint,24}]) -> 39;
fit_args(move_return, [{smallint,65535}]) -> 33;
fit_args(move_return, [{smallint,9}]) -> 20;
fit_args(move_return, [{smallint,18}]) -> 43;
fit_args(move_return, [{smallint,17}]) -> 41;
fit_args(move_return, [{smallint,128}]) -> 38;
fit_args(move_return, [{smallint,20}]) -> 37;
fit_args(move_return, [{smallint,15}]) -> 32;
fit_args(move_return, [{smallint,13}]) -> 25;
fit_args(move_return, [{smallint,16}]) -> 24;
fit_args(move_return, [{smallint,12}]) -> 23;
fit_args(move_return, [{smallint,11}]) -> 18;
fit_args(move_return, [{smallint,5}]) -> 15;
fit_args(move_return, [_]) -> 48;
fit_args(node, [{x,3}]) -> 3;
fit_args(node, [{x,0}]) -> 0;
fit_args(node, [{x,2}]) -> 2;
fit_args(node, [{x,1}]) -> 1;
fit_args(node, [_]) -> 4;
fit_args(on_load, []) -> 0;
fit_args(put_list, [{x,0},{y,0},{y,0}]) -> 8;
fit_args(put_list, [{x,_},{x,_},{x,_}]) -> 5;
fit_args(put_list, [{x,_},{y,Arg1},{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 11;
fit_args(put_list, [_,nil,_]) -> 6;
fit_args(put_list, [_,{x,0},_]) -> 7;
fit_args(put_list, [_,{x,_},{y,Arg2}]) when Arg2 >= 0, Arg2 =< 255 -> 10;
fit_args(put_list, [_,{y,Arg1},{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 12;
fit_args(put_list, [_,{x,_},{x,_}]) -> 13;
fit_args(put_list, [_,_,{x,0}]) -> 0;
fit_args(put_list, [_,_,{x,1}]) -> 1;
fit_args(put_list, [_,_,{x,2}]) -> 2;
fit_args(put_list, [_,_,{x,3}]) -> 3;
fit_args(put_list, [_,_,{x,4}]) -> 4;
fit_args(put_list, [_,_,{x,5}]) -> 9;
fit_args(put_list, [_,_,_]) -> 14;
fit_args(raise, [{x,2},{x,1}]) -> 0;
fit_args(raise, [_,_]) -> 1;
fit_args(recv_mark, [{f,_}]) -> 0;
fit_args(remove_message, []) -> 0;
fit_args(return, []) -> 0;
fit_args(self, [{x,0}]) -> 0;
fit_args(self, [{x,1}]) -> 1;
fit_args(self, [{x,2}]) -> 2;
fit_args(self, [{y,0}]) -> 3;
fit_args(self, [{y,1}]) -> 4;
fit_args(self, [{x,3}]) -> 5;
fit_args(self, [_]) -> 6;
fit_args(send, []) -> 0;
fit_args(set_tuple_element, [{y,Arg0},{x,0},Arg2]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(set_tuple_element, [_,{x,0},_]) -> 1;
fit_args(set_tuple_element, [_,_,_]) -> 2;
fit_args(system_limit, [{f,_}]) -> 0;
fit_args(test_arity, [{f,_},{x,0},2]) -> 0;
fit_args(test_arity, [{f,_},{x,0},_]) -> 2;
fit_args(test_arity, [{f,_},{x,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(test_arity, [{f,_},{y,Arg1},Arg2]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(test_arity, [{f,_},_,_]) -> 4;
fit_args(test_heap, [HeapNeeded,Live]) when HeapNeeded >= 0, HeapNeeded =< 255, Live >= 0, Live =< 255 -> 0;
fit_args(test_heap, [_,Live]) when Live >= 0, Live =< 255 -> 1;
fit_args(test_heap_1_put_list, [2,{y,0}]) -> 0;
fit_args(test_heap_1_put_list, [2,_]) -> 1;
fit_args(test_heap_1_put_list, [Arg0,{i,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= -128, Arg1 =< 127 -> 2;
fit_args(test_heap_1_put_list, [Arg0,{y,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 3;
fit_args(test_heap_1_put_list, [_,_]) -> 4;
fit_args(timeout, []) -> 0;
fit_args(try_case_end, [{x,0}]) -> 0;
fit_args(try_case_end, [_]) -> 1;
fit_args(try_end, [{y,5}]) -> 4;
fit_args(try_end, [{y,0}]) -> 1;
fit_args(try_end, [{y,4}]) -> 2;
fit_args(try_end, [{y,3}]) -> 5;
fit_args(try_end, [{y,1}]) -> 0;
fit_args(try_end, [{y,2}]) -> 3;
fit_args(try_end, [{y,6}]) -> 6;
fit_args(try_end, [_]) -> 7;
fit_args(wait, [{f,_}]) -> 0;
fit_args(wait_timeout, [{f,_},_]) -> 0;
fit_args(Op, As) -> erlang:error({nofit,Op,As}).

var_args(get_tuple_element, 0) -> [{x,0},u8,x8];
var_args(move, 0) -> [t,{x,1}];
var_args(get_tuple_element, 1) -> [x8,u8,x8];
var_args(move, 3) -> [x8,y8];
var_args(is_tuple_of_arity, 1) -> [f,x8,u8];
var_args(l_new_bs_put_integer_imm, 0) -> [f,u8,u8,t];
var_args(move, 1) -> [t,{x,0}];
var_args(l_call, 0) -> [f];
var_args(move2, 0) -> [x8,y8,x8,y8];
var_args(test_heap, 0) -> [u8,u8];
var_args(move, 2) -> [{x,0},t];
var_args(l_put_tuple, 0) -> [x8,u8];
var_args(move2, 1) -> [y8,x8,y8,t];
var_args(move2, 3) -> [x8,x8,x8,t];
var_args(l_call_only, 0) -> [f];
var_args(get_list, 0) -> [x8,x8,x8];
var_args(l_fetch, 2) -> [x8,x8];
var_args(is_nonempty_list, 0) -> [f,{x,0}];
var_args(l_allocate, 1) -> [{u,0}];
var_args(l_is_eq_exact, 0) -> [f];
var_args(move, 4) -> [t,{x,2}];
var_args(is_nonempty_list_allocate, 1) -> [f,x8,u8];
var_args(l_select_val2, 0) -> [{x,0},f,t,f,t,f];
var_args(l_gc_bif1, 0) -> [f,b,t,u8,x8];
var_args(l_is_eq_exact_immed, 2) -> [f,{x,3},t];
var_args(deallocate_return, 1) -> [{u,0}];
var_args(get_list, 3) -> [t,x8,y8];
var_args(l_allocate, 0) -> [{u,1}];
var_args(is_nonempty_list, 2) -> [f,{x,1}];
var_args(l_is_eq_exact_immed, 3) -> [f,{x,2},t];
var_args(l_is_eq_exact_immed, 0) -> [f,{x,1},t];
var_args(l_call_ext, 110) -> [e];
var_args(is_tuple_of_arity, 0) -> [f,{x,0},{u,2}];
var_args(put_list, 0) -> [t,t,{x,0}];
var_args(l_call_fun, 0) -> [{u8,1}];
var_args(l_bs_add, 0) -> [f,{u8,1},t];
var_args(l_bs_get_integer_small_imm, 0) -> [t,u8,f,u8,x8];
var_args(l_fetch, 0) -> [{x,0},t];
var_args(l_is_eq_exact_immed, 1) -> [f,{x,0},t];
var_args(call_bif, 8) -> [{b,{erlang,'++',2}}];
var_args(move_return, 3) -> [{x,2}];
var_args(l_is_eq_exact_immed, 6) -> [f,{x,5},t];
var_args(deallocate_return, 0) -> [{u,1}];
var_args(put_list, 1) -> [t,t,{x,1}];
var_args(return, 0) -> [];
var_args(extract_next_element, 2) -> [{x,2}];
var_args(is_nil, 2) -> [f,{x,1}];
var_args(extract_next_element, 1) -> [{x,3}];
var_args(l_bs_start_match2, 0) -> [t,f,u8,u8,x8];
var_args(l_is_eq_exact_immed, 5) -> [f,{x,4},t];
var_args(is_tuple, 0) -> [f,{x,0}];
var_args(l_trim, 0) -> [{u,1}];
var_args(extract_next_element2, 1) -> [{x,3}];
var_args(l_bs_get_integer_16, 0) -> [{x,0},f,x8];
var_args(deallocate_return, 2) -> [{u,2}];
var_args(l_call_last, 0) -> [f,{u,2}];
var_args(extract_next_element, 4) -> [{x,5}];
var_args(l_fetch, 1) -> [t,{x,0}];
var_args(l_select_val_atoms, 0) -> [x8,f,u8];
var_args(put_list, 2) -> [t,t,{x,2}];
var_args(move_return, 5) -> [{a,false}];
var_args(call_bif, 47) -> [b];
var_args(l_new_bs_put_binary_all, 0) -> [f,t,{u8,8}];
var_args(is_nil, 1) -> [f,{x,2}];
var_args(l_bif2, 0) -> [f,{b,{erlang,element,2}},t];
var_args(l_allocate, 2) -> [{u,2}];
var_args(l_is_eq_exact_immed, 7) -> [f,{x,6},t];
var_args(move_deallocate_return, 0) -> [x8,u8];
var_args(move_return, 1) -> [{x,1}];
var_args(call_bif, 7) -> [{b,{erlang,setelement,3}}];
var_args(move, 5) -> [t,{x,3}];
var_args(l_fetch, 18) -> [t,{x,3}];
var_args(call_bif, 16) -> [{b,{erlang,list_to_atom,1}}];
var_args(call_bif, 15) -> [{b,{erlang,integer_to_list,1}}];
var_args(call_bif, 14) -> [{b,{ets,lookup,2}}];
var_args(l_increment, 0) -> [x8,u8,u8,t];
var_args(l_bs_init_heap_bin, 0) -> [u8,u8,u8,t];
var_args(l_select_tuple_arity, 0) -> [{x,0},f,u32];
var_args(put_list, 3) -> [t,t,{x,3}];
var_args(l_select_tuple_arity2, 0) -> [{x,0},f,u8,f,u8,f];
var_args(move_return, 4) -> [nil];
var_args(l_fetch, 4) -> [x8,y8];
var_args(l_allocate, 6) -> [{u,6}];
var_args(l_call_last, 6) -> [f,{u,6}];
var_args(init2, 0) -> [y8,y8];
var_args(call_bif, 9) -> [{b,{lists,member,2}}];
var_args(l_move_call, 36) -> [t,f];
var_args(l_new_bs_put_binary_imm, 1) -> [f,u8,u8,x8];
var_args(l_make_fun, 0) -> [fu];
var_args(is_nonempty_list, 1) -> [f,{x,2}];
var_args(l_call_ext, 6) -> [{e,{linc,lookup,2}}];
var_args(l_call_ext_last, 0) -> [e,{u,1}];
var_args(l_is_ge, 0) -> [f];
var_args(extract_next_element2, 0) -> [{x,1}];
var_args(move_return, 48) -> [t];
var_args(l_allocate, 3) -> [{u,3}];
var_args(jump, 0) -> [f];
var_args(l_fetch, 10) -> [{x,1},t];
var_args(is_atom, 0) -> [f,{x,0}];
var_args(l_bs_get_binary_imm2, 0) -> [f,t,u8,u8,u8,x8];
var_args(extract_next_element2, 2) -> [{x,2}];
var_args(is_tuple, 2) -> [f,{x,2}];
var_args(test_arity, 0) -> [f,{x,0},{u,2}];
var_args(l_move_call_only, 3) -> [f,{x,4}];
var_args(l_catch, 5) -> [{y,5},t];
var_args(l_bs_append, 0) -> [f,u8,u8,u8,t];
var_args(l_select_tuple_arity2, 1) -> [x8,f,u8,f,u8,f];
var_args(extract_next_element, 0) -> [{x,1}];
var_args(call_bif, 41) -> [{b,{ets,update_counter,3}}];
var_args(l_bs_init_fail, 0) -> [u8,f,u8,x8];
var_args(l_is_function2, 0) -> [f,t,u8];
var_args(l_allocate_zero, 3) -> [{u,4}];
var_args(l_loop_rec, 0) -> [f];
var_args(l_select_tuple_arity, 1) -> [x8,f,u8];
var_args(is_nil, 0) -> [f,{x,0}];
var_args(move, 6) -> [t,{x,4}];
var_args(extract_next_element3, 0) -> [{x,1}];
var_args(remove_message, 0) -> [];
var_args(l_move_call_last, 2) -> [f,u8,x8];
var_args(allocate_init, 0) -> [u8,y8];
var_args(move, 8) -> [t,{x,5}];
var_args(l_allocate_zero, 2) -> [{u,3}];
var_args(l_allocate_zero, 1) -> [{u,1}];
var_args(move, 11) -> [t,{x,7}];
var_args(is_nil, 3) -> [f,{x,4}];
var_args(catch_end, 5) -> [{y,5}];
var_args(is_atom, 1) -> [f,{x,1}];
var_args(init, 0) -> [{y,1}];
var_args(l_bif1, 0) -> [f,b,x8,x8];
var_args(init, 1) -> [{y,0}];
var_args(l_select_val2, 9) -> [{x,6},f,t,f,t,f];
var_args(apply_last, 0) -> [u8,u8];
var_args(l_bs_get_binary_all_reuse, 0) -> [t,f,{u8,8}];
var_args(self, 0) -> [{x,0}];
var_args(deallocate_return, 4) -> [{u,4}];
var_args(l_fetch, 17) -> [t,{x,2}];
var_args(set_tuple_element, 0) -> [y8,{x,0},u8];
var_args(l_bs_match_string, 0) -> [x8,f,u8,str];
var_args(l_allocate, 10) -> [u32];
var_args(l_move_call_only, 1) -> [f,{x,2}];
var_args(l_select_val_smallints, 0) -> [x8,f,u8];
var_args(l_bs_get_integer_8, 0) -> [{x,0},f,x8];
var_args(move_deallocate_return, 2) -> [y8,u8];
var_args(is_list, 2) -> [f,{x,2}];
var_args(is_atom, 2) -> [f,{x,2}];
var_args(l_allocate_zero, 0) -> [{u,2}];
var_args(send, 0) -> [];
var_args(allocate_heap, 0) -> [u8,u8,u8];
var_args(l_is_eq_exact_immed, 8) -> [f,{x,7},t];
var_args(l_trim, 1) -> [{u,2}];
var_args(move_deallocate_return, 1) -> [{a,ok},u32];
var_args(init3, 0) -> [y8,y8,y8];
var_args(deallocate_return, 3) -> [{u,3}];
var_args(extract_next_element3, 2) -> [{x,2}];
var_args(call_bif, 36) -> [{b,{lists,reverse,2}}];
var_args(l_bif2, 6) -> [f,b,t];
var_args(is_integer, 0) -> [f,{x,0}];
var_args(move_return, 0) -> [{a,true}];
var_args(l_allocate, 4) -> [{u,4}];
var_args(l_bif2, 4) -> [f,{b,{erlang,'and',2}},t];
var_args(l_bif2, 1) -> [f,{b,{erlang,'=:=',2}},t];
var_args(is_atom, 3) -> [f,{x,3}];
var_args(l_call_last, 4) -> [f,{u,1}];
var_args(extract_next_element, 6) -> [{x,7}];
var_args(l_move_call, 1) -> [{x,1},f];
var_args(move_deallocate_return, 3) -> [t,{u,0}];
var_args(l_move_call_ext_last, 0) -> [e,u8,y8];
var_args(l_bs_get_binary2, 0) -> [f,t,u8,t,u8,u8,x8];
var_args(l_is_eq_exact_literal, 1) -> [f,{x,1},t];
var_args(l_call_ext, 24) -> [{e,{proplists,get_value,2}}];
var_args(l_move_call_ext, 13) -> [t,{e,{erlang,put,2}}];
var_args(l_fetch, 13) -> [{x,4},t];
var_args(l_move_call_last, 3) -> [f,{u,2},t];
var_args(l_new_bs_put_binary_imm, 0) -> [f,u8,u8,y8];
var_args(l_minus, 0) -> [f,u8,x8];
var_args(l_call_last, 1) -> [f,{u,0}];
var_args(l_times, 1) -> [f,u8,x8];
var_args(l_bs_init_bits, 0) -> [u32,u32,u8,t];
var_args(l_fetch, 14) -> [{x,3},t];
var_args(l_increment, 2) -> [t,u32,u8,{x,0}];
var_args(l_times, 0) -> [f,u8,{x,0}];
var_args(l_bs_get_integer_32, 0) -> [x8,f,u8,x8];
var_args(l_bs_get_binary_all2, 0) -> [f,x8,u8,u8,x8];
var_args(deallocate_return, 13) -> [u32];
var_args(l_move_call_last, 0) -> [f,u8,y8];
var_args(l_select_val2, 6) -> [t,f,{a,false},f,{a,true},f];
var_args(l_is_eq_exact_literal, 5) -> [f,{x,5},t];
var_args(is_binary, 2) -> [f,{x,2}];
var_args(test_heap_1_put_list, 0) -> [{u,2},{y,0}];
var_args(l_is_lt, 0) -> [f];
var_args(l_plus, 0) -> [f,u8,x8];
var_args(l_allocate_zero, 4) -> [{u,5}];
var_args(l_select_val2, 2) -> [{x,1},f,t,f,t,f];
var_args(extract_next_element3, 4) -> [{x,5}];
var_args(l_select_val2, 8) -> [{x,5},f,t,f,t,f];
var_args(call_bif, 18) -> [{b,{erlang,atom_to_list,1}}];
var_args(is_integer, 5) -> [f,t];
var_args(is_binary, 1) -> [f,{x,0}];
var_args(deallocate_return, 5) -> [{u,5}];
var_args(l_catch, 0) -> [{y,0},t];
var_args(l_trim, 3) -> [{u,4}];
var_args(is_pid, 0) -> [f,{x,0}];
var_args(l_select_val2, 3) -> [{x,2},f,t,f,t,f];
var_args(is_nonempty_list, 4) -> [f,{x,4}];
var_args(l_call_last, 3) -> [f,{u,4}];
var_args(is_list, 0) -> [f,{x,0}];
var_args(extract_next_element3, 7) -> [{x,8}];
var_args(l_select_val2, 12) -> [{x,8},f,t,f,t,f];
var_args(l_int_div, 1) -> [f,u8,x8];
var_args(l_fetch, 8) -> [t,{x,1}];
var_args(l_catch, 2) -> [{y,2},t];
var_args(catch_end, 2) -> [{y,2}];
var_args(l_call_ext_last, 1) -> [e,{u,0}];
var_args(l_allocate_zero, 5) -> [{u,6}];
var_args(l_move_call_ext_only, 10) -> [e,t];
var_args(extract_next_element, 5) -> [{x,6}];
var_args(init, 2) -> [{y,2}];
var_args(try_end, 4) -> [{y,5}];
var_args(extract_next_element2, 3) -> [{x,4}];
var_args(is_tuple, 1) -> [f,{x,1}];
var_args(l_is_ne_exact_immed, 2) -> [f,{x,2},t];
var_args(is_float, 0) -> [f,{x,0}];
var_args(try_end, 1) -> [{y,0}];
var_args(l_move_call, 4) -> [{y,1},f];
var_args(extract_next_element, 3) -> [{x,4}];
var_args(l_call_ext, 0) -> [{e,{lists,reverse,1}}];
var_args(is_integer_allocate, 0) -> [f,x8,u8];
var_args(l_fetch, 16) -> [{x,5},t];
var_args(int_code_end, 0) -> [];
var_args(move_return, 6) -> [{x,3}];
var_args(l_move_call_ext, 50) -> [t,e];
var_args(l_call_ext, 12) -> [{e,{lists,concat,1}}];
var_args(extract_next_element2, 5) -> [{x,6}];
var_args(l_catch, 4) -> [{y,4},t];
var_args(is_nil, 6) -> [f,{x,6}];
var_args(try_end, 2) -> [{y,4}];
var_args(deallocate_return, 6) -> [{u,6}];
var_args(l_is_eq_exact_immed, 19) -> [f,{x,14},t];
var_args(extract_next_element2, 13) -> [{x,14}];
var_args(l_move_call, 3) -> [{y,0},f];
var_args(l_move_call_ext_only, 1) -> [{e,{lists,reverse,1}},{x,1}];
var_args(is_port, 0) -> [f,t];
var_args(l_move_call_ext, 7) -> [{y,3},e];
var_args(l_is_eq_exact_immed, 21) -> [f,{y,3},t];
var_args(l_is_eq_exact_immed, 12) -> [f,{x,10},t];
var_args(l_apply, 0) -> [];
var_args(is_list, 4) -> [f,{x,4}];
var_args(l_get, 2) -> [t,{x,0}];
var_args(l_select_val2, 5) -> [t,f,{a,true},f,{a,false},f];
var_args(node, 3) -> [{x,3}];
var_args(l_is_eq_exact_immed, 27) -> [f,{y,5},t];
var_args(deallocate_return, 9) -> [{u,9}];
var_args(extract_next_element2, 10) -> [{x,10}];
var_args(call_bif, 42) -> [{b,{ets,match_object,2}}];
var_args(l_select_val2, 13) -> [{y,2},f,t,f,t,f];
var_args(l_move_call_only, 6) -> [f,nil];
var_args(l_call_ext, 80) -> [{e,{ets,tab2list,1}}];
var_args(l_is_eq_exact_literal, 4) -> [f,{x,3},t];
var_args(l_bs_init_bits_fail, 0) -> [u8,f,u8,x8];
var_args(l_move_call_last, 5) -> [f,{u,3},t];
var_args(l_new_bs_put_binary_all, 1) -> [f,t,u8];
var_args(l_rem, 0) -> [f,u8,x8];
var_args(l_move_call_last, 1) -> [f,{u,1},t];
var_args(move_return, 14) -> [{smallint,6}];
var_args(l_plus, 2) -> [f,u8,y8];
var_args(l_bs_put_string, 2) -> [{u,2},str];
var_args(l_new_bs_put_integer, 0) -> [f,t,{u8,1},{u8,0},{smallint,0}];
var_args(move_deallocate_return, 7) -> [t,{u,4}];
var_args(wait_timeout, 0) -> [f,t];
var_args(l_is_eq, 0) -> [f];
var_args(l_move_call_only, 2) -> [f,{x,3}];
var_args(is_integer, 4) -> [f,{x,4}];
var_args(l_fetch, 12) -> [{x,2},t];
var_args(l_call_fun, 3) -> [{u8,2}];
var_args(l_move_call_only, 4) -> [f,{x,5}];
var_args(l_move_call_only, 10) -> [f,t];
var_args(l_select_val2, 7) -> [{x,4},f,t,f,t,f];
var_args(l_fetch, 23) -> [t,t];
var_args(is_nonempty_list, 6) -> [f,{x,5}];
var_args(l_increment, 3) -> [x8,u32,u8,x8];
var_args(is_list, 1) -> [f,{x,1}];
var_args(bif1_body, 0) -> [{b,{erlang,hd,1}},t,{x,0}];
var_args(is_nonempty_list_allocate, 0) -> [f,{x,0},u32];
var_args(l_bs_get_integer_8, 1) -> [x8,f,x8];
var_args(l_gc_bif1, 3) -> [f,{b,{erlang,length,1}},t,u8,y8];
var_args(apply, 0) -> [u8];
var_args(l_is_ne_exact_literal, 0) -> [f,t,t];
var_args(get_list, 2) -> [t,{x,0},t];
var_args(l_element, 0) -> [t,t,t];
var_args(l_fetch, 6) -> [y8,y8];
var_args(l_is_ne_exact, 0) -> [f];
var_args(l_call_last, 2) -> [f,{u,3}];
var_args(get_tuple_element, 4) -> [y8,u8,x8];
var_args(get_tuple_element, 5) -> [x8,u8,y8];
var_args(call_bif, 12) -> [{b,{erlang,list_to_binary,1}}];
var_args(is_nonempty_list_test_heap, 0) -> [f,u8,u8];
var_args(l_move_call, 0) -> [{x,2},f];
var_args(is_integer, 3) -> [f,{x,3}];
var_args(call_bif, 21) -> [{b,{erlang,binary_to_list,1}}];
var_args(l_bs_skip_bits_imm2, 1) -> [f,x8,u8];
var_args(is_nonempty_list, 3) -> [f,{x,3}];
var_args(l_increment, 1) -> [y8,u32,u8,x8];
var_args(test_arity, 1) -> [f,x8,u8];
var_args(put_list, 4) -> [t,t,{x,4}];
var_args(is_tuple_of_arity, 3) -> [f,y8,u8];
var_args(move_return, 8) -> [{smallint,0}];
var_args(is_integer, 1) -> [f,{x,1}];
var_args(l_bs_test_unit_8, 3) -> [f,{x,1}];
var_args(l_bs_put_string, 0) -> [{u,4},str];
var_args(l_bs_test_zero_tail2, 3) -> [f,{x,1}];
var_args(l_bs_get_binary_all_reuse, 1) -> [t,f,u8];
var_args(l_is_eq_exact_immed, 9) -> [f,{x,8},t];
var_args(put_list, 5) -> [x8,x8,x8];
var_args(is_nil, 4) -> [f,{x,3}];
var_args(bif2_body, 0) -> [b,{x,0}];
var_args(l_fast_element, 2) -> [x8,u8,t];
var_args(l_trim, 4) -> [{u,5}];
var_args(catch_end, 0) -> [{y,0}];
var_args(get_tuple_element, 3) -> [{x,0},u8,y8];
var_args(self, 1) -> [{x,1}];
var_args(l_move_call_only, 8) -> [f,{x,7}];
var_args(move_deallocate_return, 4) -> [t,{u,1}];
var_args(l_bs_test_unit_8, 1) -> [f,{x,2}];
var_args(timeout, 0) -> [];
var_args(is_binary, 0) -> [f,{x,1}];
var_args(l_allocate, 7) -> [{u,7}];
var_args(move, 13) -> [t,t];
var_args(l_bif2, 2) -> [f,{b,{erlang,'=<',2}},t];
var_args(l_move_call_ext, 8) -> [{y,4},e];
var_args(l_trim, 2) -> [{u,3}];
var_args(l_is_ne_exact_immed, 4) -> [f,{x,3},t];
var_args(call_bif, 29) -> [{b,{erlang,whereis,1}}];
var_args(call_bif, 46) -> [{b,{erlang,monitor,2}}];
var_args(l_catch, 6) -> [{y,6},t];
var_args(recv_mark, 0) -> [f];
var_args(l_recv_set, 0) -> [];
var_args(self, 2) -> [{x,2}];
var_args(catch_end, 6) -> [{y,6}];
var_args(l_is_eq_exact_literal, 0) -> [f,{x,0},t];
var_args(l_call_ext_only, 5) -> [e];
var_args(l_is_eq_exact_immed, 11) -> [f,{x,9},t];
var_args(extract_next_element, 10) -> [{x,8}];
var_args(l_fetch, 20) -> [t,{x,5}];
var_args(l_is_ne_exact_immed, 10) -> [f,{x,6},t];
var_args(is_nonempty_list, 5) -> [f,{x,7}];
var_args(l_is_eq_exact_immed, 35) -> [f,{y,7},t];
var_args(l_select_val2, 4) -> [{x,3},f,t,f,t,f];
var_args(wait, 0) -> [f];
var_args(l_select_val_smallints, 1) -> [{x,0},f,u32];
var_args(move, 9) -> [t,{x,6}];
var_args(is_tuple, 4) -> [f,{x,4}];
var_args(move_return, 2) -> [{a,ok}];
var_args(l_move_call_only, 5) -> [f,{x,6}];
var_args(l_allocate, 5) -> [{u,5}];
var_args(call_bif, 11) -> [{b,{ets,insert,2}}];
var_args(extract_next_element3, 3) -> [{x,4}];
var_args(l_move_call, 12) -> [{x,4},f];
var_args(l_fast_element, 1) -> [{x,0},{u,1},t];
var_args(move_jump, 3) -> [f,{x,3}];
var_args(extract_next_element, 9) -> [{y,0}];
var_args(extract_next_element2, 4) -> [{x,5}];
var_args(l_move_call, 2) -> [{y,2},f];
var_args(l_move_call_only, 0) -> [f,{x,1}];
var_args(is_nonempty_list, 8) -> [f,{x,9}];
var_args(l_catch, 3) -> [{y,3},t];
var_args(call_bif, 28) -> [{b,{erlang,spawn,1}}];
var_args(l_call_ext, 28) -> [{e,{os,type,0}}];
var_args(is_integer, 2) -> [f,{x,2}];
var_args(is_nonempty_list, 7) -> [f,{x,6}];
var_args(l_is_ne_exact_immed, 3) -> [f,t,{a,true}];
var_args(try_end, 5) -> [{y,3}];
var_args(is_tuple, 3) -> [f,{x,3}];
var_args(extract_next_element2, 12) -> [{y,0}];
var_args(l_call_last, 11) -> [f,u32];
var_args(put_list, 14) -> [t,t,t];
var_args(move_return, 26) -> [{x,5}];
var_args(call_bif, 44) -> [{b,{ets,safe_fixtable,2}}];
var_args(extract_next_element3, 1) -> [{x,3}];
var_args(l_move_call, 5) -> [{x,3},f];
var_args(l_call_ext, 101) -> [{e,{dict,fetch,2}}];
var_args(call_bif, 17) -> [{b,{lists,keyfind,3}}];
var_args(move_jump, 0) -> [f,{x,1}];
var_args(init, 3) -> [{y,3}];
var_args(l_call_ext_only, 2) -> [{e,{gen_server,call,2}}];
var_args(put_list, 6) -> [t,nil,t];
var_args(get_tuple_element, 2) -> [t,{u,0},{x,0}];
var_args(l_catch, 1) -> [{y,1},t];
var_args(l_bs_test_unit_8, 0) -> [f,{x,0}];
var_args(loop_rec_end, 0) -> [f];
var_args(l_band, 0) -> [f,u8,x8];
var_args(l_move_call_ext, 9) -> [{x,2},e];
var_args(is_nil, 5) -> [f,{x,5}];
var_args(self, 3) -> [{y,0}];
var_args(test_heap_1_put_list, 1) -> [{u,2},t];
var_args(bif2_body, 1) -> [b,{x,1}];
var_args(l_bs_skip_bits_imm2, 0) -> [f,{x,0},u32];
var_args(l_is_eq_exact_immed, 16) -> [f,{y,0},t];
var_args(l_bs_restore2, 0) -> [x8,u8];
var_args(is_nonempty_list, 9) -> [f,{x,8}];
var_args(l_move_call_ext, 6) -> [{x,1},e];
var_args(call_bif, 27) -> [{b,{erlang,now,0}}];
var_args(l_move_call, 6) -> [{y,3},f];
var_args(catch_end, 1) -> [{y,1}];
var_args(l_call_ext_last, 2) -> [e,{u,2}];
var_args(get_list, 6) -> [x8,y8,y8];
var_args(move, 7) -> [y8,y8];
var_args(l_allocate_zero, 6) -> [{u,7}];
var_args(extract_next_element, 8) -> [{y,1}];
var_args(l_call_ext, 37) -> [{e,{dict,store,3}}];
var_args(l_move_call_ext_last, 2) -> [e,{u,1},t];
var_args(extract_next_element, 12) -> [{y,2}];
var_args(l_call_last, 7) -> [f,{u,7}];
var_args(l_is_eq_exact_immed, 26) -> [f,{y,4},t];
var_args(call_bif, 25) -> [{b,{ets,new,2}}];
var_args(move_return, 22) -> [{a,no}];
var_args(call_bif, 37) -> [{b,{erlang,list_to_tuple,1}}];
var_args(is_nil, 11) -> [f,{x,10}];
var_args(init, 4) -> [{y,4}];
var_args(put_list, 9) -> [t,t,{x,5}];
var_args(l_call_ext, 4) -> [{e,{lists,foldl,3}}];
var_args(l_is_ne_exact_immed, 0) -> [f,{x,0},t];
var_args(l_jump_on_val, 1) -> [x8,f,u8,u8];
var_args(l_is_ne_exact_immed, 11) -> [f,t,t];
var_args(l_call_last, 5) -> [f,{u,5}];
var_args(l_call_ext_only, 4) -> [{e,{lists,reverse,1}}];
var_args(extract_next_element, 11) -> [{y,3}];
var_args(l_call_fun_last, 0) -> [u8,u8];
var_args(l_call_ext, 25) -> [{e,{lists,mapfoldl,3}}];
var_args(extract_next_element, 15) -> [{y,5}];
var_args(l_move_call_ext, 14) -> [t,{e,{proplists,get_value,3}}];
var_args(l_move_call_ext, 5) -> [{y,2},e];
var_args(is_nil, 7) -> [f,{x,7}];
var_args(l_bs_save2, 0) -> [x8,u8];
var_args(try_end, 0) -> [{y,1}];
var_args(l_int_div, 0) -> [f,u8,{x,0}];
var_args(bif1_body, 3) -> [{b,{erlang,'not',1}},{x,0},{x,0}];
var_args(l_bor, 0) -> [f,u8,{x,0}];
var_args(l_is_eq_exact_immed, 18) -> [f,{x,255},t];
var_args(extract_next_element, 7) -> [{x,255}];
var_args(l_bsl, 1) -> [f,u8,x8];
var_args(l_move_call_ext_only, 3) -> [e,{x,2}];
var_args(deallocate_return, 7) -> [{u,7}];
var_args(l_jump_on_val, 0) -> [{x,0},f,u8,u8];
var_args(get_list, 8) -> [x8,y8,x8];
var_args(catch_end, 3) -> [{y,3}];
var_args(is_nil, 8) -> [f,{x,8}];
var_args(put_list, 10) -> [t,x8,y8];
var_args(get_list, 5) -> [{x,0},t,{x,0}];
var_args(is_tuple, 9) -> [f,t];
var_args(l_apply_last, 0) -> [u32];
var_args(l_call_ext, 43) -> [{e,{lists,delete,2}}];
var_args(call_bif, 30) -> [{b,{lists,keymember,3}}];
var_args(l_bsr, 0) -> [f,u8,x8];
var_args(l_move_call_ext, 15) -> [{x,3},e];
var_args(l_bs_skip_bits2, 0) -> [f,x8,x8,u8];
var_args(l_call_ext, 71) -> [{e,{lists,flatlength,1}}];
var_args(l_is_ne_exact_immed, 1) -> [f,{x,1},t];
var_args(is_list, 3) -> [f,{x,3}];
var_args(init, 5) -> [{y,5}];
var_args(call_bif, 40) -> [{b,{ets,next,2}}];
var_args(l_is_eq_exact_immed, 22) -> [f,{y,2},t];
var_args(extract_next_element, 13) -> [{x,9}];
var_args(l_is_ne_exact_immed, 8) -> [f,{x,5},t];
var_args(l_get, 1) -> [t,{x,1}];
var_args(is_nonempty_list, 10) -> [f,{x,10}];
var_args(l_move_call, 7) -> [{y,4},f];
var_args(l_fast_element, 0) -> [{x,0},{u,2},{x,0}];
var_args(l_move_call_ext, 21) -> [t,{e,{proplists,get_value,2}}];
var_args(is_bitstr, 0) -> [f,t];
var_args(l_select_val2, 14) -> [t,f,t,f,t,f];
var_args(call_bif, 31) -> [{b,{erlang,tuple_to_list,1}}];
var_args(l_call_ext, 96) -> [{e,{os,timestamp,0}}];
var_args(move_return, 17) -> [{smallint,10}];
var_args(l_is_eq_exact_immed, 15) -> [f,{y,1},t];
var_args(l_call_ext, 51) -> [{e,{lists,last,1}}];
var_args(call_bif, 10) -> [{b,{erlang,get_module_info,2}}];
var_args(move_jump, 13) -> [f,t];
var_args(l_trim, 5) -> [{u,6}];
var_args(is_function, 0) -> [f,{x,0}];
var_args(l_call_ext, 2) -> [{e,{lists,foreach,2}}];
var_args(call_bif, 26) -> [{b,{ets,delete,2}}];
var_args(extract_next_element, 16) -> [{y,4}];
var_args(l_move_call, 8) -> [{y,5},f];
var_args(extract_next_element2, 7) -> [{x,8}];
var_args(l_allocate, 8) -> [{u,8}];
var_args(bif1_body, 7) -> [b,t,t];
var_args(l_call_ext_last, 6) -> [e,u32];
var_args(l_fetch, 7) -> [{y,0},{x,2}];
var_args(deallocate_return, 10) -> [{u,10}];
var_args(call_bif, 38) -> [{b,{erlang,make_ref,0}}];
var_args(init, 9) -> [{y,9}];
var_args(move_return, 10) -> [{x,4}];
var_args(is_nil, 9) -> [f,{x,9}];
var_args(move_deallocate_return, 6) -> [t,{u,3}];
var_args(l_fdiv, 0) -> [fr,fr,fr];
var_args(l_band, 2) -> [f,u8,t];
var_args(l_bs_test_zero_tail2, 5) -> [f,{x,5}];
var_args(call_bif, 22) -> [{b,{ets,lookup_element,3}}];
var_args(l_fcheckerror, 0) -> [];
var_args(fclearerror, 0) -> [];
var_args(allocate_heap_zero, 0) -> [u8,u8,u8];
var_args(fconv, 0) -> [t,{fr,0}];
var_args(fmove_1, 0) -> [t,{fr,1}];
var_args(l_select_val_atoms, 1) -> [{x,0},f,u32];
var_args(l_move_call_ext_last, 6) -> [e,u32,t];
var_args(l_bsl, 0) -> [f,u8,{x,0}];
var_args(fmove_2, 1) -> [fr,x8];
var_args(l_increment, 6) -> [{x,0},{u,4294967295},u8,y8];
var_args(l_call_ext, 41) -> [{e,{filename,dirname,1}}];
var_args(l_move_call_ext, 2) -> [{y,0},e];
var_args(is_tuple, 7) -> [f,{x,8}];
var_args(l_call_ext, 20) -> [{e,{proplists,get_value,3}}];
var_args(init, 6) -> [{y,6}];
var_args(l_move_call_last, 4) -> [f,{u,0},t];
var_args(l_call_ext, 38) -> [{e,{ordsets,from_list,1}}];
var_args(extract_next_element3, 6) -> [{x,6}];
var_args(move_return, 21) -> [{a,error}];
var_args(l_is_ne, 0) -> [f];
var_args(extract_next_element2, 6) -> [{x,7}];
var_args(l_call_ext, 66) -> [{e,{lists,dropwhile,2}}];
var_args(is_nonempty_list, 24) -> [f,{y,0}];
var_args(move_jump, 2) -> [f,nil];
var_args(l_allocate_zero, 7) -> [{u,8}];
var_args(l_call_ext, 23) -> [{e,{lists,filter,2}}];
var_args(l_call_ext, 11) -> [{e,{ordsets,union,2}}];
var_args(l_catch, 8) -> [t,t];
var_args(is_pid, 1) -> [f,t];
var_args(is_reference, 0) -> [f,t];
var_args(call_bif, 32) -> [{b,{erlang,process_flag,2}}];
var_args(is_nonempty_list, 12) -> [f,{x,12}];
var_args(l_is_eq_exact_immed, 20) -> [f,{x,13},t];
var_args(l_bs_test_zero_tail2, 1) -> [f,{x,0}];
var_args(l_move_call_ext, 4) -> [{smallint,1},e];
var_args(extract_next_element2, 8) -> [{x,9}];
var_args(l_call_ext, 16) -> [{e,{erlang,list_to_integer,1}}];
var_args(l_rem, 2) -> [f,u8,t];
var_args(l_move_call_ext, 38) -> [{x,4},e];
var_args(l_move_call_ext, 3) -> [{y,1},e];
var_args(catch_end, 8) -> [t];
var_args(self, 6) -> [t];
var_args(l_is_eq_exact_immed, 13) -> [f,{x,11},t];
var_args(l_call_ext, 105) -> [{e,{gb_sets,empty,0}}];
var_args(l_move_call_ext, 10) -> [t,{e,{io_lib,format,2}}];
var_args(l_is_eq_exact_immed, 30) -> [f,{y,6},t];
var_args(l_call_ext, 7) -> [{e,{lists,sort,1}}];
var_args(l_call_last, 8) -> [f,{u,8}];
var_args(test_arity, 3) -> [f,y8,u8];
var_args(is_boolean, 0) -> [f,t];
var_args(l_allocate_zero, 10) -> [u32];
var_args(l_call_ext, 26) -> [{e,{filename,join,1}}];
var_args(l_call_ext, 47) -> [{e,{gb_trees,lookup,2}}];
var_args(is_list, 6) -> [f,t];
var_args(is_nonempty_list, 15) -> [f,{x,14}];
var_args(l_is_eq_exact_literal, 7) -> [f,t,t];
var_args(l_move_call_ext_only, 6) -> [e,{x,3}];
var_args(self, 4) -> [{y,1}];
var_args(l_call_ext, 34) -> [{e,{dict,new,0}}];
var_args(l_is_eq_exact_immed, 24) -> [f,{x,15},t];
var_args(l_call_ext, 17) -> [{e,{mnesia_lib,set,2}}];
var_args(is_function, 1) -> [f,t];
var_args(l_move_call, 15) -> [{smallint,0},f];
var_args(l_fetch, 19) -> [{y,0},t];
var_args(move_return, 28) -> [{a,none}];
var_args(node, 4) -> [t];
var_args(l_call_ext, 45) -> [{e,{gb_trees,empty,0}}];
var_args(l_bor, 1) -> [f,u8,x8];
var_args(move_return, 7) -> [{smallint,1}];
var_args(extract_next_element3, 10) -> [t];
var_args(l_call_ext, 82) -> [{e,{gb_trees,insert,3}}];
var_args(is_nonempty_list, 11) -> [f,{x,11}];
var_args(is_atom, 4) -> [f,{x,4}];
var_args(l_call_ext, 8) -> [{e,{lists,map,2}}];
var_args(l_apply_fun, 0) -> [];
var_args(l_call_ext, 109) -> [{e,{ordsets,intersection,2}}];
var_args(extract_next_element2, 14) -> [{x,13}];
var_args(move_return, 16) -> [{smallint,7}];
var_args(is_nil, 15) -> [f,{x,13}];
var_args(l_call_ext_last, 3) -> [e,{u,3}];
var_args(l_allocate_zero, 8) -> [{u,9}];
var_args(try_end, 3) -> [{y,2}];
var_args(l_fetch, 15) -> [t,{x,4}];
var_args(l_fast_element, 3) -> [t,{u,2},t];
var_args(l_allocate_zero, 9) -> [{u,10}];
var_args(is_nil, 12) -> [f,{x,11}];
var_args(l_select_val2, 10) -> [{x,7},f,t,f,t,f];
var_args(move_deallocate_return, 5) -> [t,{u,2}];
var_args(extract_next_element, 14) -> [{x,10}];
var_args(call_bif, 6) -> [{b,{erlang,throw,1}}];
var_args(l_call_ext, 64) -> [{e,{ordsets,subtract,2}}];
var_args(extract_next_element3, 5) -> [{x,7}];
var_args(l_move_call_ext, 22) -> [{a,loglevel},{e,{lager_mochiglobal,get,2}}];
var_args(call_bif, 33) -> [{b,{erlang,pid_to_list,1}}];
var_args(l_call_ext, 74) -> [{e,{lists,splitwith,2}}];
var_args(is_nil, 13) -> [f,{x,12}];
var_args(l_move_call_ext_only, 0) -> [{e,{erlang,get_module_info,1}},t];
var_args(init, 7) -> [{y,7}];
var_args(is_tuple, 5) -> [f,{x,7}];
var_args(l_call_ext, 31) -> [{e,{dict,find,2}}];
var_args(try_end, 6) -> [{y,6}];
var_args(is_atom, 6) -> [f,t];
var_args(set_tuple_element, 1) -> [t,{x,0},u32];
var_args(put_list, 7) -> [t,{x,0},t];
var_args(l_call_ext_last, 4) -> [e,{u,4}];
var_args(l_increment, 5) -> [t,u8,u8,y8];
var_args(is_atom, 5) -> [f,{x,5}];
var_args(l_is_eq_exact_immed, 14) -> [f,{x,12},t];
var_args(l_call_ext, 55) -> [{e,{filename,basename,1}}];
var_args(call_bif, 35) -> [{b,{erlang,unlink,1}}];
var_args(call_bif, 23) -> [{b,{erlang,'--',2}}];
var_args(l_move_call, 10) -> [{y,6},f];
var_args(move_return, 36) -> [{smallint,32}];
var_args(l_move_call_only, 7) -> [f,{x,8}];
var_args(l_call_last, 10) -> [f,{u,10}];
var_args(bif1_body, 5) -> [b,t,{x,1}];
var_args(init, 8) -> [{y,8}];
var_args(call_bif, 20) -> [{b,{lists,keysearch,3}}];
var_args(l_is_eq_exact_immed, 36) -> [f,t,t];
var_args(l_call_ext, 42) -> [{e,{lists,keydelete,3}}];
var_args(extract_next_element, 21) -> [{y,6}];
var_args(l_int_bnot, 0) -> [f,t,u8,t];
var_args(deallocate_return, 8) -> [{u,8}];
var_args(call_bif, 34) -> [{b,{erlang,iolist_to_binary,1}}];
var_args(l_put_tuple, 6) -> [y8,u8];
var_args(node, 0) -> [{x,0}];
var_args(l_is_eq_exact_immed, 25) -> [f,{x,16},t];
var_args(l_move_call, 13) -> [{x,5},f];
var_args(bif2_body, 2) -> [b,{x,2}];
var_args(init, 10) -> [{y,10}];
var_args(extract_next_element2, 17) -> [t];
var_args(l_new_bs_put_integer, 2) -> [f,t,u8,u8,t];
var_args(l_move_call, 16) -> [{smallint,2},f];
var_args(l_move_call_ext, 36) -> [t,{e,{mnesia_lib,dbg_out,2}}];
var_args(is_tuple, 8) -> [f,{x,6}];
var_args(extract_next_element2, 11) -> [{x,11}];
var_args(is_nil, 28) -> [f,t];
var_args(put_list, 12) -> [t,y8,x8];
var_args(get_list, 4) -> [y8,x8,x8];
var_args(l_bs_get_integer_16, 1) -> [x8,f,x8];
var_args(catch_end, 4) -> [{y,4}];
var_args(extract_next_element, 17) -> [{x,11}];
var_args(try_end, 7) -> [t];
var_args(call_bif, 43) -> [{b,{erlang,spawn_link,1}}];
var_args(is_tuple, 6) -> [f,{x,5}];
var_args(l_call_ext, 44) -> [{e,{file,read_file_info,1}}];
var_args(l_call_ext, 3) -> [{e,{file,close,1}}];
var_args(l_trim, 8) -> [{u,9}];
var_args(put_list, 11) -> [x8,y8,y8];
var_args(l_is_eq_exact_literal, 6) -> [f,{x,6},t];
var_args(l_move_call_ext, 44) -> [t,{e,{mnesia_lib,set,2}}];
var_args(l_get, 6) -> [t,t];
var_args(call_bif, 5) -> [{b,{erlang,exit,1}}];
var_args(l_call_ext, 91) -> [{e,{ofp_v4_enum,to_atom,2}}];
var_args(l_is_ne_exact_immed, 5) -> [f,{y,0},t];
var_args(is_nil, 10) -> [f,{y,1}];
var_args(l_move_call_ext, 37) -> [t,{e,{erlang,erase,1}}];
var_args(node, 2) -> [{x,2}];
var_args(is_nil, 17) -> [f,{x,15}];
var_args(is_nonempty_list, 33) -> [f,{y,8}];
var_args(l_is_eq_exact_immed, 28) -> [f,{x,17},t];
var_args(l_call_ext, 61) -> [{e,{lager_util,format_time,0}}];
var_args(l_trim, 6) -> [{u,7}];
var_args(is_nil, 14) -> [f,{x,14}];
var_args(move_jump, 7) -> [f,{y,1}];
var_args(move_jump, 6) -> [f,{y,2}];
var_args(move_jump, 1) -> [f,{x,2}];
var_args(move_return, 19) -> [{a,undefined}];
var_args(bs_context_to_binary, 0) -> [{x,0}];
var_args(badmatch, 0) -> [{x,0}];
var_args(is_nonempty_list, 35) -> [f,{y,7}];
var_args(is_nonempty_list, 19) -> [f,{y,1}];
var_args(l_move_call, 25) -> [{x,6},f];
var_args(bif1_body, 6) -> [{b,{erlang,hd,1}},t,{x,2}];
var_args(bif1_body, 1) -> [{b,{erlang,hd,1}},{x,0},{x,1}];
var_args(bif2_body, 3) -> [b,t];
var_args(is_float, 1) -> [f,t];
var_args(node, 1) -> [{x,1}];
var_args(l_move_call_ext_last, 4) -> [e,{u,2},t];
var_args(call_bif, 13) -> [{b,{ets,delete,1}}];
var_args(l_call_ext, 92) -> [{e,{gb_trees,from_orddict,1}}];
var_args(l_call_ext, 78) -> [{e,{lists,append,1}}];
var_args(l_call_ext, 36) -> [{e,{sofs,to_external,1}}];
var_args(move_jump, 10) -> [f,{a,ok}];
var_args(move_return, 9) -> [{smallint,2}];
var_args(l_bs_test_unit_8, 2) -> [f,{x,3}];
var_args(fconv, 2) -> [t,fr];
var_args(deallocate_return, 11) -> [{u,11}];
var_args(l_call_ext_only, 0) -> [{e,{gen_server,call,3}}];
var_args(move, 10) -> [{smallint,4096},{x,8}];
var_args(l_move_call_ext, 28) -> [t,{e,{lager,dispatch_log,9}}];
var_args(l_call_ext, 40) -> [{e,{lists,duplicate,2}}];
var_args(l_move_call, 17) -> [{smallint,3},f];
var_args(l_move_call_ext, 1) -> [{smallint,0},e];
var_args(extract_next_element, 24) -> [t];
var_args(l_is_ne_exact_immed, 6) -> [f,{x,4},t];
var_args(l_trim, 7) -> [{u,8}];
var_args(l_call_fun, 4) -> [u8];
var_args(l_apply_fun_only, 0) -> [];
var_args(l_move_call_ext, 26) -> [{a,linc},e];
var_args(l_move_call_ext, 25) -> [t,{e,{mnesia_lib,verbose,2}}];
var_args(l_bs_skip_bits_all2, 1) -> [f,{x,3},{u8,8}];
var_args(l_call_ext, 98) -> [{e,{ordsets,is_element,2}}];
var_args(l_call_ext, 14) -> [{e,{ofp_v4_enum,to_int,2}}];
var_args(move_return, 11) -> [{smallint,3}];
var_args(bs_context_to_binary, 9) -> [t];
var_args(is_nonempty_list, 14) -> [f,{x,13}];
var_args(l_move_call_ext_only, 7) -> [e,{x,1}];
var_args(bif1_body, 4) -> [b,t,{x,0}];
var_args(l_move_call_ext, 46) -> [{smallint,2},e];
var_args(test_heap_1_put_list, 4) -> [u32,t];
var_args(self, 5) -> [{x,3}];
var_args(l_call_ext, 88) -> [{e,{sets,is_element,2}}];
var_args(l_call_ext, 86) -> [{e,{gb_trees,to_list,1}}];
var_args(extract_next_element, 19) -> [{x,13}];
var_args(l_fadd, 0) -> [fr,fr,fr];
var_args(extract_next_element2, 16) -> [{x,15}];
var_args(move_jump, 12) -> [f,{smallint,0}];
var_args(move_return, 45) -> [{smallint,22}];
var_args(move_return, 29) -> [{smallint,64}];
var_args(move_return, 13) -> [{smallint,8}];
var_args(move_deallocate_return, 8) -> [t,{u,5}];
var_args(is_bigint, 0) -> [f,t];
var_args(fmove_2, 0) -> [fr,{x,0}];
var_args(fmove_1, 1) -> [t,fr];
var_args(l_move_call_last, 7) -> [f,u32,t];
var_args(l_is_ne_exact_immed, 9) -> [f,{y,2},t];
var_args(l_fast_element, 5) -> [t,u32,t];
var_args(is_nonempty_list, 13) -> [f,{y,2}];
var_args(l_is_eq_exact_literal, 3) -> [f,{x,4},t];
var_args(test_heap_1_put_list, 3) -> [u8,y8];
var_args(l_call_ext, 84) -> [{e,{erlang,term_to_binary,1}}];
var_args(l_call_ext, 75) -> [{e,{ofp_utils,padding,2}}];
var_args(l_call_ext, 62) -> [{e,{gb_trees,get,2}}];
var_args(l_call_ext, 58) -> [{e,{erlang,binary_to_term,1}}];
var_args(l_call_ext, 48) -> [{e,{erl_syntax,atom,1}}];
var_args(l_call_ext, 35) -> [{e,{erlang,put,2}}];
var_args(l_call_ext, 10) -> [{e,{lists,flatten,1}}];
var_args(extract_next_element2, 9) -> [{x,12}];
var_args(is_nil, 20) -> [f,{y,0}];
var_args(l_bs_put_string, 1) -> [{u,1},str];
var_args(is_list, 5) -> [f,{x,5}];
var_args(is_nonempty_list, 22) -> [f,{y,4}];
var_args(get_list, 10) -> [t,t,t];
var_args(l_move_call, 9) -> [{smallint,1},f];
var_args(l_move_call_ext, 24) -> [{y,7},e];
var_args(l_move_call_ext, 16) -> [{y,5},e];
var_args(l_is_eq_exact_immed, 29) -> [f,{x,18},t];
var_args(l_call_ext, 77) -> [{e,{mnesia_monitor,use_dir,0}}];
var_args(l_select_val_atoms, 2) -> [y8,f,u8];
var_args(is_nonempty_list, 21) -> [f,{x,18}];
var_args(is_binary, 3) -> [f,t];
var_args(l_move_call_ext_only, 4) -> [{e,{ofp_v4_enum,to_int,2}},{a,type}];
var_args(l_move_call_ext, 17) -> [{a,error},{e,{lager_util,level_to_num,1}}];
var_args(init, 11) -> [{y,11}];
var_args(l_get, 3) -> [t,{x,2}];
var_args(l_bs_test_zero_tail2, 2) -> [f,{x,3}];
var_args(l_bs_get_integer, 0) -> [f,u8,u8,u8,x8];
var_args(func_info, 0) -> [t,t,u8];
var_args(extract_next_element, 22) -> [{x,18}];
var_args(l_select_val_atoms, 3) -> [t,f,u32];
var_args(l_trim, 10) -> [{u,11}];
var_args(is_nil, 21) -> [f,{x,17}];
var_args(l_move_call_ext_last, 5) -> [e,{u,3},t];
var_args(l_fsub, 0) -> [fr,fr,fr];
var_args(l_move_call_ext, 35) -> [t,{e,{timer,sleep,1}}];
var_args(l_move_call_ext, 31) -> [t,{e,{erlang,system_info,1}}];
var_args(l_move_call_ext, 12) -> [{y,6},e];
var_args(init, 15) -> [t];
var_args(l_wait_timeout, 5) -> [f,u32];
var_args(l_call_last, 9) -> [f,{u,9}];
var_args(l_is_eq_exact_immed, 31) -> [f,{x,19},t];
var_args(l_call_ext, 104) -> [{e,{file,format_error,1}}];
var_args(l_call_ext, 65) -> [{e,{gen_tcp,send,2}}];
var_args(l_call_ext, 59) -> [{e,{mnesia_lib,exists,1}}];
var_args(l_call_ext, 5) -> [{e,{file,open,2}}];
var_args(l_new_bs_put_integer, 1) -> [f,t,{u8,1},{u8,0},t];
var_args(move_return, 34) -> [{smallint,14}];
var_args(move_return, 12) -> [{smallint,4}];
var_args(l_trim, 11) -> [u32];
var_args(l_move_call_only, 9) -> [f,{smallint,1}];
var_args(l_move_call_ext_last, 3) -> [{e,{gen_event,notify,2}},{u,5},{a,lager_event}];
var_args(l_move_call_ext_last, 1) -> [e,{u,0},t];
var_args(is_nonempty_list, 29) -> [f,{x,22}];
var_args(is_nonempty_list, 23) -> [f,{x,20}];
var_args(is_nonempty_list, 17) -> [f,{x,16}];
var_args(l_call_fun, 2) -> [{u8,3}];
var_args(l_apply_only, 0) -> [];
var_args(l_fetch, 21) -> [{y,1},t];
var_args(l_move_call_ext, 47) -> [{a,schema},e];
var_args(l_move_call_ext, 19) -> [nil,e];
var_args(l_bs_test_zero_tail2, 0) -> [f,{x,2}];
var_args(call_bif, 45) -> [{b,{ets,match,2}}];
var_args(bs_init_writable, 0) -> [];
var_args(l_call_ext, 39) -> [{e,{gen_tcp,accept,1}}];
var_args(extract_next_element, 20) -> [{x,14}];
var_args(move_jump, 11) -> [f,{a,true}];
var_args(move_jump, 4) -> [f,{x,4}];
var_args(move_return, 47) -> [{smallint,19}];
var_args(move_return, 46) -> [{a,all}];
var_args(move_return, 39) -> [{smallint,24}];
var_args(move_return, 33) -> [{smallint,65535}];
var_args(move_return, 20) -> [{smallint,9}];
var_args(is_nil, 27) -> [f,{x,20}];
var_args(is_nil, 22) -> [f,{x,19}];
var_args(l_bs_put_string, 5) -> [u32,str];
var_args(l_bs_put_string, 3) -> [{u,6},str];
var_args(put_list, 13) -> [t,x8,x8];
var_args(is_nonempty_list, 18) -> [f,{x,15}];
var_args(l_increment, 7) -> [t,u32,u8,t];
var_args(l_times, 2) -> [f,u8,t];
var_args(l_bs_get_integer_imm, 0) -> [t,u8,u8,f,u8,x8];
var_args(l_move_call, 11) -> [nil,f];
var_args(l_get, 5) -> [{a,mnesia_activity_state},t];
var_args(call_bif, 24) -> [{b,{re,run,3}}];
var_args(call_bif, 3) -> [{b,{erlang,error,1}}];
var_args(l_is_eq_exact_immed, 34) -> [f,{x,23},t];
var_args(l_move_call_last, 6) -> [f,{u,4},t];
var_args(l_call_ext, 9) -> [{e,{filename,join,2}}];
var_args(l_is_ne_exact_immed, 7) -> [f,{y,1},t];
var_args(move_jump, 5) -> [f,{y,0}];
var_args(move_return, 40) -> [{a,delete}];
var_args(l_trim, 9) -> [{u,10}];
var_args(bs_context_to_binary, 2) -> [{x,1}];
var_args(is_nonempty_list, 30) -> [f,{x,26}];
var_args(is_nonempty_list, 27) -> [f,{x,24}];
var_args(l_make_export, 0) -> [e];
var_args(l_select_val2, 11) -> [{y,1},f,t,f,t,f];
var_args(is_number, 0) -> [f,t];
var_args(move_deallocate_return, 10) -> [t,u32];
var_args(l_move_call, 28) -> [{a,schema},f];
var_args(l_move_call, 14) -> [{y,7},f];
var_args(l_move_call_ext_only, 2) -> [{e,{io_lib,format,2}},t];
var_args(init, 14) -> [{y,14}];
var_args(init, 13) -> [{y,13}];
var_args(init, 12) -> [{y,12}];
var_args(l_wait_timeout, 3) -> [f,{u,5000}];
var_args(l_wait_timeout, 2) -> [f,{u,100}];
var_args(l_wait_timeout, 1) -> [f,{u,10000}];
var_args(l_bs_skip_bits_all2, 0) -> [f,{x,2},{u8,8}];
var_args(l_bs_test_zero_tail2, 6) -> [f,t];
var_args(call_bif, 39) -> [{b,{erlang,get_stacktrace,0}}];
var_args(call_bif, 19) -> [{b,{ets,info,2}}];
var_args(call_bif, 4) -> [{b,{erlang,exit,2}}];
var_args(call_bif, 2) -> [{b,{erlang,error,2}}];
var_args(call_bif, 1) -> [{b,{erlang,raise,3}}];
var_args(call_bif, 0) -> [{b,{erlang,purge_module,1}}];
var_args(l_int_div, 2) -> [f,u8,t];
var_args(l_bs_put_utf16, 0) -> [f,u8,t];
var_args(l_bs_get_utf16, 2) -> [t,f,u8,t];
var_args(l_bs_get_utf16, 1) -> [{x,0},f,u8,x8];
var_args(l_bs_get_utf16, 0) -> [x8,f,u8,x8];
var_args(l_allocate, 9) -> [{u,10}];
var_args(l_put_tuple, 7) -> [t,u32];
var_args(l_put_tuple, 5) -> [{x,0},u8];
var_args(l_put_tuple, 4) -> [{x,0},{u,5}];
var_args(l_put_tuple, 3) -> [{x,0},{u,4}];
var_args(l_put_tuple, 2) -> [{x,0},{u,3}];
var_args(l_put_tuple, 1) -> [{x,0},{u,2}];
var_args(l_is_eq_exact_immed, 33) -> [f,{x,20},t];
var_args(l_is_eq_exact_immed, 32) -> [f,{x,22},t];
var_args(l_is_eq_exact_immed, 23) -> [f,{y,10},{a,ber}];
var_args(l_is_eq_exact_immed, 17) -> [f,y8,{a,asn1_NOVALUE}];
var_args(l_is_eq_exact_immed, 10) -> [f,{x,255},{a,xmerl_scanner}];
var_args(l_is_eq_exact_immed, 4) -> [f,x8,i8];
var_args(l_call_ext, 108) -> [{e,{asn1ct_name,new,1}}];
var_args(l_call_ext, 107) -> [{e,{beam_utils,code_at,2}}];
var_args(l_call_ext, 106) -> [{e,{cerl,var_name,1}}];
var_args(l_call_ext, 103) -> [{e,{xref_utils,xset,2}}];
var_args(l_call_ext, 102) -> [{e,{cerl,c_tuple,1}}];
var_args(l_call_ext, 100) -> [{e,{mnesia_lib,intersect,2}}];
var_args(l_call_ext, 99) -> [{e,{lists,sublist,3}}];
var_args(l_call_ext, 97) -> [{e,{ordsets,add_element,2}}];
var_args(l_call_ext, 95) -> [{e,{erlang,binary_to_atom,2}}];
var_args(l_call_ext, 94) -> [{e,{ofp_v3_enum,to_atom,2}}];
var_args(l_call_ext, 93) -> [{e,{lists,keyreplace,4}}];
var_args(l_call_ext, 90) -> [{e,{erl_syntax,get_pos,1}}];
var_args(l_call_ext, 89) -> [{e,{inet,sockname,1}}];
var_args(l_call_ext, 87) -> [{e,{file,write,2}}];
var_args(l_call_ext, 85) -> [{e,{erl_syntax,atom_value,1}}];
var_args(l_call_ext, 83) -> [{e,{mnesia_schema,list2cs,1}}];
var_args(l_call_ext, 81) -> [{e,{test_server,fail,1}}];
var_args(l_call_ext, 79) -> [{e,{inet,port,1}}];
var_args(l_call_ext, 76) -> [{e,{mnesia_lib,cs_to_storage_type,2}}];
var_args(l_call_ext, 73) -> [{e,{asn1ct_gen,type,1}}];
var_args(l_call_ext, 72) -> [{e,{file,rename,2}}];
var_args(l_call_ext, 70) -> [{e,{asn1ct_gen,list2name,1}}];
var_args(l_call_ext, 69) -> [{e,{sofs,family_union,2}}];
var_args(l_call_ext, 68) -> [{e,{lists,seq,2}}];
var_args(l_call_ext, 67) -> [{e,{file,read,2}}];
var_args(l_call_ext, 63) -> [{e,{inet,getopts,2}}];
var_args(l_call_ext, 60) -> [{e,{cerl,get_ann,1}}];
var_args(l_call_ext, 57) -> [{e,{erlang,max,2}}];
var_args(l_call_ext, 56) -> [{e,{file,read_file,1}}];
var_args(l_call_ext, 54) -> [{e,{asn1ct_gen,get_inner,1}}];
var_args(l_call_ext, 53) -> [{e,{ssh_channel,cache_lookup,2}}];
var_args(l_call_ext, 52) -> [{e,{file,make_dir,1}}];
var_args(l_call_ext, 50) -> [{e,{random,uniform,1}}];
var_args(l_call_ext, 49) -> [{e,{io,format,3}}];
var_args(l_call_ext, 46) -> [{e,{asn1ct_gen,mk_var,1}}];
var_args(l_call_ext, 33) -> [{e,{prettypr,beside,2}}];
var_args(l_call_ext, 32) -> [{e,{asn1_db,dbget,2}}];
var_args(l_call_ext, 30) -> [{e,{string,tokens,2}}];
var_args(l_call_ext, 29) -> [{e,{ofp_v3_enum,to_int,2}}];
var_args(l_call_ext, 27) -> [{e,{prettypr,floating,1}}];
var_args(l_call_ext, 22) -> [{e,{erl_syntax,type,1}}];
var_args(l_call_ext, 21) -> [{e,{test_server,lookup_config,2}}];
var_args(l_call_ext, 19) -> [{e,{test_server,timetrap_cancel,1}}];
var_args(l_call_ext, 18) -> [{e,{file,delete,1}}];
var_args(l_call_ext, 15) -> [{e,{gen_tcp,close,1}}];
var_args(l_call_ext, 13) -> [{e,{test_server,timetrap,1}}];
var_args(l_call_ext, 1) -> [{e,{asn1ct_gen,emit,1}}];
var_args(allocate_init, 1) -> [u32,t];
var_args(extract_next_element, 23) -> [{x,15}];
var_args(extract_next_element, 18) -> [{x,12}];
var_args(get_tuple_element, 11) -> [t,u32,t];
var_args(get_tuple_element, 10) -> [y8,u8,y8];
var_args(get_tuple_element, 9) -> [{x,0},u32,{x,0}];
var_args(get_tuple_element, 8) -> [y8,u8,{x,0}];
var_args(get_tuple_element, 7) -> [x8,u8,{x,0}];
var_args(get_tuple_element, 6) -> [{x,0},{u,1},{x,0}];
var_args(set_tuple_element, 2) -> [t,t,u32];
var_args(l_call_fun_last, 1) -> [u8,u32];
var_args(l_fast_element, 4) -> [{x,0},{u,3},{x,0}];
var_args(l_bs_test_unit, 0) -> [f,t,u8];
var_args(extract_next_element3, 9) -> [{x,10}];
var_args(extract_next_element3, 8) -> [{x,11}];
var_args(extract_next_element2, 15) -> [{x,16}];
var_args(l_bs_start_match2, 4) -> [t,f,u8,u32,t];
var_args(l_bs_start_match2, 3) -> [t,f,u8,u8,{x,0}];
var_args(l_bs_start_match2, 2) -> [{y,1},f,{u8,0},{u,0},{x,0}];
var_args(l_bs_start_match2, 1) -> [{x,0},f,u8,u8,{x,0}];
var_args(l_bs_get_integer_32, 3) -> [t,f,u8,t];
var_args(l_bs_get_integer_32, 2) -> [x8,f,u8,{x,0}];
var_args(l_bs_get_integer_32, 1) -> [{x,0},f,u8,x8];
var_args(l_bor, 2) -> [f,u8,t];
var_args(l_bsr, 1) -> [f,u8,t];
var_args(l_bs_get_binary_imm2, 2) -> [f,t,u8,u32,u8,t];
var_args(l_bs_get_binary_imm2, 1) -> [f,x8,u8,u32,{u8,0},t];
var_args(l_bs_test_tail_imm2, 0) -> [f,t,u32];
var_args(l_bxor, 0) -> [f,u8,t];
var_args(l_bs_get_float2, 0) -> [f,t,u8,t,u8,u8,t];
var_args(move_jump, 9) -> [f,{a,asn1_NOVALUE}];
var_args(move_jump, 8) -> [f,{a,false}];
var_args(allocate_heap, 1) -> [u32,u32,u8];
var_args(move_return, 44) -> [{a,bad_len}];
var_args(move_return, 43) -> [{smallint,18}];
var_args(move_return, 42) -> [{x,6}];
var_args(move_return, 41) -> [{smallint,17}];
var_args(move_return, 38) -> [{smallint,128}];
var_args(move_return, 37) -> [{smallint,20}];
var_args(move_return, 35) -> [{a,nomatch}];
var_args(move_return, 32) -> [{smallint,15}];
var_args(move_return, 31) -> [{a,experimenter}];
var_args(move_return, 30) -> [{a,ignore}];
var_args(move_return, 27) -> [{a,eperm}];
var_args(move_return, 25) -> [{smallint,13}];
var_args(move_return, 24) -> [{smallint,16}];
var_args(move_return, 23) -> [{smallint,12}];
var_args(move_return, 18) -> [{smallint,11}];
var_args(move_return, 15) -> [{smallint,5}];
var_args(l_new_bs_put_integer_imm, 2) -> [f,u32,u8,t];
var_args(l_new_bs_put_integer_imm, 1) -> [f,u32,{u8,0},{smallint,0}];
var_args(l_bs_get_integer_small_imm, 1) -> [t,u32,f,u8,t];
var_args(l_rem, 1) -> [f,u8,{x,0}];
var_args(is_nil, 26) -> [f,{x,18}];
var_args(is_nil, 25) -> [f,{x,22}];
var_args(is_nil, 24) -> [f,{y,4}];
var_args(is_nil, 23) -> [f,{y,5}];
var_args(is_nil, 19) -> [f,{y,3}];
var_args(is_nil, 18) -> [f,{x,16}];
var_args(is_nil, 16) -> [f,{y,2}];
var_args(l_bsl, 2) -> [f,u8,t];
var_args(l_fmul, 0) -> [fr,fr,fr];
var_args(is_tuple_of_arity, 4) -> [f,t,u32];
var_args(is_tuple_of_arity, 2) -> [f,{x,0},u32];
var_args(test_arity, 4) -> [f,t,u32];
var_args(test_arity, 2) -> [f,{x,0},u32];
var_args(bs_context_to_binary, 8) -> [{x,8}];
var_args(bs_context_to_binary, 7) -> [{x,4}];
var_args(bs_context_to_binary, 6) -> [{y,0}];
var_args(bs_context_to_binary, 5) -> [{y,5}];
var_args(bs_context_to_binary, 4) -> [{y,9}];
var_args(bs_context_to_binary, 3) -> [{x,2}];
var_args(bs_context_to_binary, 1) -> [{y,1}];
var_args(l_new_bs_put_binary, 0) -> [f,t,u8,u8,t];
var_args(badmatch, 17) -> [t];
var_args(badmatch, 16) -> [{y,8}];
var_args(badmatch, 15) -> [{x,6}];
var_args(badmatch, 14) -> [{y,6}];
var_args(badmatch, 13) -> [{x,8}];
var_args(badmatch, 12) -> [{x,5}];
var_args(badmatch, 11) -> [{y,4}];
var_args(badmatch, 10) -> [{y,0}];
var_args(badmatch, 9) -> [{y,5}];
var_args(badmatch, 8) -> [{y,9}];
var_args(badmatch, 7) -> [{y,3}];
var_args(badmatch, 6) -> [{x,4}];
var_args(badmatch, 5) -> [{y,2}];
var_args(badmatch, 4) -> [{y,1}];
var_args(badmatch, 3) -> [{x,2}];
var_args(badmatch, 2) -> [{x,3}];
var_args(badmatch, 1) -> [{x,1}];
var_args(l_bs_test_unit_8, 4) -> [f,t];
var_args(l_bs_get_utf8, 1) -> [t,f,t];
var_args(l_bs_get_utf8, 0) -> [x8,f,x8];
var_args(l_bs_put_utf8, 0) -> [f,t];
var_args(l_bs_match_string, 2) -> [t,f,u32,str];
var_args(l_bs_match_string, 1) -> [{x,0},f,u32,str];
var_args(l_bs_put_string, 4) -> [{u,3},str];
var_args(fconv, 1) -> [t,{fr,1}];
var_args(l_m_div, 0) -> [f,u8,t];
var_args(raise, 1) -> [t,t];
var_args(raise, 0) -> [{x,2},{x,1}];
var_args(is_integer_allocate, 1) -> [f,t,u32];
var_args(is_nonempty_list_allocate, 2) -> [f,t,u32];
var_args(l_fnegate, 0) -> [fr,fr];
var_args(l_bs_validate_unicode, 0) -> [f,t];
var_args(l_hibernate, 0) -> [];
var_args(put_list, 8) -> [{x,0},{y,0},{y,0}];
var_args(is_nonempty_list, 41) -> [f,t];
var_args(is_nonempty_list, 40) -> [f,{x,30}];
var_args(is_nonempty_list, 39) -> [f,{x,29}];
var_args(is_nonempty_list, 38) -> [f,{x,28}];
var_args(is_nonempty_list, 37) -> [f,{x,27}];
var_args(is_nonempty_list, 36) -> [f,{x,23}];
var_args(is_nonempty_list, 34) -> [f,{y,5}];
var_args(is_nonempty_list, 32) -> [f,{x,25}];
var_args(is_nonempty_list, 31) -> [f,{x,21}];
var_args(is_nonempty_list, 28) -> [f,{y,6}];
var_args(is_nonempty_list, 26) -> [f,{y,9}];
var_args(is_nonempty_list, 25) -> [f,{x,19}];
var_args(is_nonempty_list, 20) -> [f,{x,17}];
var_args(is_nonempty_list, 16) -> [f,{y,3}];
var_args(get_list, 9) -> [x8,y8,{x,0}];
var_args(get_list, 7) -> [{x,0},y8,y8];
var_args(get_list, 1) -> [{x,0},x8,x8];
var_args(case_end, 11) -> [t];
var_args(case_end, 10) -> [{x,5}];
var_args(case_end, 9) -> [{y,4}];
var_args(case_end, 8) -> [{y,0}];
var_args(case_end, 7) -> [{x,4}];
var_args(case_end, 6) -> [{y,3}];
var_args(case_end, 5) -> [{x,3}];
var_args(case_end, 4) -> [{y,1}];
var_args(case_end, 3) -> [{y,2}];
var_args(case_end, 2) -> [{x,2}];
var_args(case_end, 1) -> [{x,1}];
var_args(case_end, 0) -> [{x,0}];
var_args(try_case_end, 1) -> [t];
var_args(try_case_end, 0) -> [{x,0}];
var_args(apply_last, 1) -> [u8,u32];
var_args(l_call_ext_last, 5) -> [e,{u,5}];
var_args(l_bs_append, 1) -> [f,u32,u8,u8,t];
var_args(l_increment, 4) -> [{x,0},u32,u8,x8];
var_args(l_bs_private_append, 0) -> [f,u8,t];
var_args(l_bs_init, 0) -> [u32,u32,u8,t];
var_args(l_new_bs_put_float, 0) -> [f,t,u8,u8,t];
var_args(l_bs_validate_unicode_retract, 0) -> [f];
var_args(l_yield, 0) -> [];
var_args(l_apply_fun_last, 0) -> [u32];
var_args(l_minus, 2) -> [f,u8,t];
var_args(l_minus, 1) -> [f,u8,{x,0}];
var_args(init3, 1) -> [t,t,t];
var_args(l_select_val_smallints, 2) -> [t,f,u32];
var_args(l_bif2, 5) -> [f,{b,{erlang,'==',2}},t];
var_args(l_bif2, 3) -> [f,{b,{erlang,'or',2}},t];
var_args(l_select_val2, 1) -> [x8,f,i8,f,i8,f];
var_args(l_select_tuple_arity2, 3) -> [t,f,u32,f,u32,f];
var_args(l_select_tuple_arity2, 2) -> [y8,f,u8,f,u8,f];
var_args(init2, 1) -> [t,t];
var_args(l_bs_skip_bits_imm2, 2) -> [f,t,u32];
var_args(l_bs_restore2, 3) -> [t,u32];
var_args(l_bs_restore2, 2) -> [{x,0},{u,1}];
var_args(l_bs_restore2, 1) -> [{x,0},{u,0}];
var_args(l_bs_skip_bits2, 1) -> [f,t,t,u8];
var_args(l_bs_get_binary_all2, 2) -> [f,t,u8,u8,t];
var_args(l_bs_get_binary_all2, 1) -> [f,{x,0},u8,{u8,8},x8];
var_args(l_bs_save2, 2) -> [t,u32];
var_args(l_bs_save2, 1) -> [{x,0},{u,1}];
var_args(is_function2, 0) -> [f,t,t];
var_args(l_bif1, 2) -> [f,b,t,t];
var_args(l_bif1, 1) -> [f,b,{x,0},t];
var_args(is_nonempty_list_test_heap, 1) -> [f,u32,u8];
var_args(allocate_heap_zero, 1) -> [u32,u32,u8];
var_args(deallocate_return, 12) -> [{u,12}];
var_args(move_deallocate_return, 9) -> [t,{u,6}];
var_args(l_bs_init_heap_bin, 1) -> [u32,u32,u8,t];
var_args(l_call_fun, 1) -> [{u8,0}];
var_args(l_new_bs_put_binary_imm, 3) -> [f,u32,u8,t];
var_args(l_new_bs_put_binary_imm, 2) -> [f,u8,u8,{x,0}];
var_args(l_bs_get_integer_imm, 1) -> [t,u32,u8,f,u8,t];
var_args(l_new_bs_put_float_imm, 1) -> [f,u32,u8,t];
var_args(l_new_bs_put_float_imm, 0) -> [f,{u,64},{u8,0},{x,0}];
var_args(l_move_call, 35) -> [{y,8},f];
var_args(l_move_call, 34) -> [{smallint,42},f];
var_args(l_move_call, 33) -> [{a,x},f];
var_args(l_move_call, 32) -> [{smallint,4},f];
var_args(l_move_call, 31) -> [{smallint,1000},f];
var_args(l_move_call, 30) -> [{a,atom},f];
var_args(l_move_call, 29) -> [{a,ets},f];
var_args(l_move_call, 27) -> [{smallint,6},f];
var_args(l_move_call, 26) -> [{a,add},f];
var_args(l_move_call, 24) -> [{smallint,9},f];
var_args(l_move_call, 23) -> [{a,false},f];
var_args(l_move_call, 22) -> [{a,endDocument},f];
var_args(l_move_call, 21) -> [{smallint,100},f];
var_args(l_move_call, 20) -> [{a,foo},f];
var_args(l_move_call, 19) -> [{y,12},f];
var_args(l_move_call, 18) -> [{smallint,12},f];
var_args(l_is_eq_exact_literal, 2) -> [f,{x,2},t];
var_args(l_bs_init_bits_fail, 1) -> [u32,f,u8,t];
var_args(l_jump_on_val, 2) -> [t,f,u32,u32];
var_args(l_bs_init_fail, 1) -> [u32,f,u8,t];
var_args(l_call_ext_only, 3) -> [{e,{mnesia_monitor,get_env,1}}];
var_args(l_call_ext_only, 1) -> [{e,{asn1ct_gen,emit,1}}];
var_args(l_move_call_ext_only, 9) -> [{e,{eunit,test,1}},t];
var_args(l_move_call_ext_only, 8) -> [{e,{io,format,2}},t];
var_args(l_move_call_ext_only, 5) -> [{e,{ofp_v3_enum,to_int,2}},{a,type}];
var_args(bif1_body, 2) -> [{b,{erlang,hd,1}},{y,1},{x,2}];
var_args(l_select_tuple_arity, 2) -> [t,f,u32];
var_args(l_fetch, 22) -> [t,{y,5}];
var_args(l_fetch, 11) -> [i8,y8];
var_args(l_fetch, 9) -> [y8,i8];
var_args(l_fetch, 5) -> [i8,x8];
var_args(l_fetch, 3) -> [x8,i8];
var_args(l_catch, 7) -> [{y,20},t];
var_args(l_bs_get_integer_8, 2) -> [t,f,t];
var_args(l_bs_get_integer_16, 2) -> [t,f,t];
var_args(move, 12) -> [nil,{x,10}];
var_args(l_bs_utf8_size, 0) -> [t,t];
var_args(l_bs_utf16_size, 0) -> [t,t];
var_args(l_move_call_ext, 49) -> [t,{e,{application,start,1}}];
var_args(l_move_call_ext, 48) -> [{a,func},e];
var_args(l_move_call_ext, 45) -> [t,{e,{lists,sublist,3}}];
var_args(l_move_call_ext, 43) -> [t,{e,{io,format,1}}];
var_args(l_move_call_ext, 42) -> [{a,auto_repair},{e,{mnesia_monitor,get_env,1}}];
var_args(l_move_call_ext, 41) -> [t,{e,{meck,expect,3}}];
var_args(l_move_call_ext, 40) -> [t,{e,{gen_tcp,connect,3}}];
var_args(l_move_call_ext, 39) -> [{smallint,10},{e,{test_server,seconds,1}}];
var_args(l_move_call_ext, 34) -> [t,{e,{ofp_v3_utils,flow_add,3}}];
var_args(l_move_call_ext, 33) -> [{a,funky},{e,{estone_SUITE,req,2}}];
var_args(l_move_call_ext, 32) -> [t,{e,{ofp_v4_utils,flow_add,3}}];
var_args(l_move_call_ext, 30) -> [t,{e,{lists,duplicate,2}}];
var_args(l_move_call_ext, 29) -> [t,{e,{lists,seq,2}}];
var_args(l_move_call_ext, 27) -> [t,{e,{test_server,lookup_config,2}}];
var_args(l_move_call_ext, 23) -> [{smallint,5},{e,{test_server,seconds,1}}];
var_args(l_move_call_ext, 20) -> [t,{e,{asn1ct_name,new,1}}];
var_args(l_move_call_ext, 18) -> [t,{e,{prettypr,text,1}}];
var_args(l_move_call_ext, 11) -> [t,{e,{asn1ct_gen,emit,1}}];
var_args(l_move_call_ext, 0) -> [t,{e,{io,format,2}}];
var_args(catch_end, 7) -> [{y,20}];
var_args(test_heap_1_put_list, 2) -> [u8,i8];
var_args(l_bs_add, 1) -> [f,u8,t];
var_args(l_band, 1) -> [f,u8,{x,0}];
var_args(on_load, 0) -> [];
var_args(l_get, 4) -> [{a,asn1_module},y8];
var_args(l_get, 0) -> [{a,asn1_module},{y,0}];
var_args(if_end, 0) -> [];
var_args(l_wait_timeout, 4) -> [f,{u,3000}];
var_args(l_wait_timeout, 0) -> [f,{u,1000}];
var_args(system_limit, 0) -> [f];
var_args(l_plus, 3) -> [f,u8,t];
var_args(l_plus, 1) -> [f,u8,{x,0}];
var_args(l_gc_bif3, 0) -> [f,b,t,u8,t];
var_args(move2, 10) -> [t,t,t,t];
var_args(move2, 9) -> [{x,0},x8,{x,0},x8];
var_args(move2, 8) -> [{x,0},x8,x8,x8];
var_args(move2, 7) -> [{x,0},x8,x8,{x,0}];
var_args(move2, 6) -> [x8,x8,{x,0},x8];
var_args(move2, 5) -> [x8,{x,0},x8,x8];
var_args(move2, 4) -> [{x,0},y8,x8,y8];
var_args(move2, 2) -> [x8,y8,{x,0},y8];
var_args(l_bs_skip_bits_all2, 2) -> [f,t,u8];
var_args(l_bs_test_zero_tail2, 4) -> [f,{x,4}];
var_args(l_bs_get_integer, 1) -> [f,u8,u8,u8,t];
var_args(l_bs_get_binary2, 1) -> [f,t,u8,t,u8,u8,t];
var_args(fmove_2, 2) -> [fr,t];
var_args(l_gc_bif2, 0) -> [f,b,u8,t];
var_args(l_gc_bif1, 4) -> [f,b,t,u8,t];
var_args(l_gc_bif1, 2) -> [f,b,t,u8,{x,0}];
var_args(l_gc_bif1, 1) -> [f,{b,{erlang,length,1}},t,u8,{x,0}];
var_args(test_heap, 1) -> [u32,u8];

var_args(Op, No) -> erlang:error({novar,Op,No}).

var_index(get_tuple_element, 0) -> 0;
var_index(move, 0) -> 1;
var_index(get_tuple_element, 1) -> 2;
var_index(move, 3) -> 3;
var_index(is_tuple_of_arity, 1) -> 4;
var_index(l_new_bs_put_integer_imm, 0) -> 5;
var_index(move, 1) -> 6;
var_index(l_call, 0) -> 7;
var_index(move2, 0) -> 8;
var_index(test_heap, 0) -> 9;
var_index(move, 2) -> 10;
var_index(l_put_tuple, 0) -> 11;
var_index(move2, 1) -> 12;
var_index(move2, 3) -> 13;
var_index(l_call_only, 0) -> 14;
var_index(get_list, 0) -> 15;
var_index(l_fetch, 2) -> 16;
var_index(is_nonempty_list, 0) -> 17;
var_index(l_allocate, 1) -> 18;
var_index(l_is_eq_exact, 0) -> 19;
var_index(move, 4) -> 20;
var_index(is_nonempty_list_allocate, 1) -> 21;
var_index(l_select_val2, 0) -> 22;
var_index(l_gc_bif1, 0) -> 23;
var_index(l_is_eq_exact_immed, 2) -> 24;
var_index(deallocate_return, 1) -> 25;
var_index(get_list, 3) -> 26;
var_index(l_allocate, 0) -> 27;
var_index(is_nonempty_list, 2) -> 28;
var_index(l_is_eq_exact_immed, 3) -> 29;
var_index(l_is_eq_exact_immed, 0) -> 30;
var_index(l_call_ext, 110) -> 31;
var_index(is_tuple_of_arity, 0) -> 32;
var_index(put_list, 0) -> 33;
var_index(l_call_fun, 0) -> 34;
var_index(l_bs_add, 0) -> 35;
var_index(l_bs_get_integer_small_imm, 0) -> 36;
var_index(l_fetch, 0) -> 37;
var_index(l_is_eq_exact_immed, 1) -> 38;
var_index(call_bif, 8) -> 39;
var_index(move_return, 3) -> 40;
var_index(l_is_eq_exact_immed, 6) -> 41;
var_index(deallocate_return, 0) -> 42;
var_index(put_list, 1) -> 43;
var_index(return, 0) -> 44;
var_index(extract_next_element, 2) -> 45;
var_index(is_nil, 2) -> 46;
var_index(extract_next_element, 1) -> 47;
var_index(l_bs_start_match2, 0) -> 48;
var_index(l_is_eq_exact_immed, 5) -> 49;
var_index(is_tuple, 0) -> 50;
var_index(l_trim, 0) -> 51;
var_index(extract_next_element2, 1) -> 52;
var_index(l_bs_get_integer_16, 0) -> 53;
var_index(deallocate_return, 2) -> 54;
var_index(l_call_last, 0) -> 55;
var_index(extract_next_element, 4) -> 56;
var_index(l_fetch, 1) -> 57;
var_index(l_select_val_atoms, 0) -> 58;
var_index(put_list, 2) -> 59;
var_index(move_return, 5) -> 60;
var_index(call_bif, 47) -> 61;
var_index(l_new_bs_put_binary_all, 0) -> 62;
var_index(is_nil, 1) -> 63;
var_index(l_bif2, 0) -> 64;
var_index(l_allocate, 2) -> 65;
var_index(l_is_eq_exact_immed, 7) -> 66;
var_index(move_deallocate_return, 0) -> 67;
var_index(move_return, 1) -> 68;
var_index(call_bif, 7) -> 69;
var_index(move, 5) -> 70;
var_index(l_fetch, 18) -> 71;
var_index(call_bif, 16) -> 72;
var_index(call_bif, 15) -> 73;
var_index(call_bif, 14) -> 74;
var_index(l_increment, 0) -> 75;
var_index(l_bs_init_heap_bin, 0) -> 76;
var_index(l_select_tuple_arity, 0) -> 77;
var_index(put_list, 3) -> 78;
var_index(l_select_tuple_arity2, 0) -> 79;
var_index(move_return, 4) -> 80;
var_index(l_fetch, 4) -> 81;
var_index(l_allocate, 6) -> 82;
var_index(l_call_last, 6) -> 83;
var_index(init2, 0) -> 84;
var_index(call_bif, 9) -> 85;
var_index(l_move_call, 36) -> 86;
var_index(l_new_bs_put_binary_imm, 1) -> 87;
var_index(l_make_fun, 0) -> 88;
var_index(is_nonempty_list, 1) -> 89;
var_index(l_call_ext, 6) -> 90;
var_index(l_call_ext_last, 0) -> 91;
var_index(l_is_ge, 0) -> 92;
var_index(extract_next_element2, 0) -> 93;
var_index(move_return, 48) -> 94;
var_index(l_allocate, 3) -> 95;
var_index(jump, 0) -> 96;
var_index(l_fetch, 10) -> 97;
var_index(is_atom, 0) -> 98;
var_index(l_bs_get_binary_imm2, 0) -> 99;
var_index(extract_next_element2, 2) -> 100;
var_index(is_tuple, 2) -> 101;
var_index(test_arity, 0) -> 102;
var_index(l_move_call_only, 3) -> 103;
var_index(l_catch, 5) -> 104;
var_index(l_bs_append, 0) -> 105;
var_index(l_select_tuple_arity2, 1) -> 106;
var_index(extract_next_element, 0) -> 107;
var_index(call_bif, 41) -> 108;
var_index(l_bs_init_fail, 0) -> 109;
var_index(l_is_function2, 0) -> 110;
var_index(l_allocate_zero, 3) -> 111;
var_index(l_loop_rec, 0) -> 112;
var_index(l_select_tuple_arity, 1) -> 113;
var_index(is_nil, 0) -> 114;
var_index(move, 6) -> 115;
var_index(extract_next_element3, 0) -> 116;
var_index(remove_message, 0) -> 117;
var_index(l_move_call_last, 2) -> 118;
var_index(allocate_init, 0) -> 119;
var_index(move, 8) -> 120;
var_index(l_allocate_zero, 2) -> 121;
var_index(l_allocate_zero, 1) -> 122;
var_index(move, 11) -> 123;
var_index(is_nil, 3) -> 124;
var_index(catch_end, 5) -> 125;
var_index(is_atom, 1) -> 126;
var_index(init, 0) -> 127;
var_index(l_bif1, 0) -> 128;
var_index(init, 1) -> 129;
var_index(l_select_val2, 9) -> 130;
var_index(apply_last, 0) -> 131;
var_index(l_bs_get_binary_all_reuse, 0) -> 132;
var_index(self, 0) -> 133;
var_index(deallocate_return, 4) -> 134;
var_index(l_fetch, 17) -> 135;
var_index(set_tuple_element, 0) -> 136;
var_index(l_bs_match_string, 0) -> 137;
var_index(l_allocate, 10) -> 138;
var_index(l_move_call_only, 1) -> 139;
var_index(l_select_val_smallints, 0) -> 140;
var_index(l_bs_get_integer_8, 0) -> 141;
var_index(move_deallocate_return, 2) -> 142;
var_index(is_list, 2) -> 143;
var_index(is_atom, 2) -> 144;
var_index(l_allocate_zero, 0) -> 145;
var_index(send, 0) -> 146;
var_index(allocate_heap, 0) -> 147;
var_index(l_is_eq_exact_immed, 8) -> 148;
var_index(l_trim, 1) -> 149;
var_index(move_deallocate_return, 1) -> 150;
var_index(init3, 0) -> 151;
var_index(deallocate_return, 3) -> 152;
var_index(extract_next_element3, 2) -> 153;
var_index(call_bif, 36) -> 154;
var_index(l_bif2, 6) -> 155;
var_index(is_integer, 0) -> 156;
var_index(move_return, 0) -> 157;
var_index(l_allocate, 4) -> 158;
var_index(l_bif2, 4) -> 159;
var_index(l_bif2, 1) -> 160;
var_index(is_atom, 3) -> 161;
var_index(l_call_last, 4) -> 162;
var_index(extract_next_element, 6) -> 163;
var_index(l_move_call, 1) -> 164;
var_index(move_deallocate_return, 3) -> 165;
var_index(l_move_call_ext_last, 0) -> 166;
var_index(l_bs_get_binary2, 0) -> 167;
var_index(l_is_eq_exact_literal, 1) -> 168;
var_index(l_call_ext, 24) -> 169;
var_index(l_move_call_ext, 13) -> 170;
var_index(l_fetch, 13) -> 171;
var_index(l_move_call_last, 3) -> 172;
var_index(l_new_bs_put_binary_imm, 0) -> 173;
var_index(l_minus, 0) -> 174;
var_index(l_call_last, 1) -> 175;
var_index(l_times, 1) -> 176;
var_index(l_bs_init_bits, 0) -> 177;
var_index(l_fetch, 14) -> 178;
var_index(l_increment, 2) -> 179;
var_index(l_times, 0) -> 180;
var_index(l_bs_get_integer_32, 0) -> 181;
var_index(l_bs_get_binary_all2, 0) -> 182;
var_index(deallocate_return, 13) -> 183;
var_index(l_move_call_last, 0) -> 184;
var_index(l_select_val2, 6) -> 185;
var_index(l_is_eq_exact_literal, 5) -> 186;
var_index(is_binary, 2) -> 187;
var_index(test_heap_1_put_list, 0) -> 188;
var_index(l_is_lt, 0) -> 189;
var_index(l_plus, 0) -> 190;
var_index(l_allocate_zero, 4) -> 191;
var_index(l_select_val2, 2) -> 192;
var_index(extract_next_element3, 4) -> 193;
var_index(l_select_val2, 8) -> 194;
var_index(call_bif, 18) -> 195;
var_index(is_integer, 5) -> 196;
var_index(is_binary, 1) -> 197;
var_index(deallocate_return, 5) -> 198;
var_index(l_catch, 0) -> 199;
var_index(l_trim, 3) -> 200;
var_index(is_pid, 0) -> 201;
var_index(l_select_val2, 3) -> 202;
var_index(is_nonempty_list, 4) -> 203;
var_index(l_call_last, 3) -> 204;
var_index(is_list, 0) -> 205;
var_index(extract_next_element3, 7) -> 206;
var_index(l_select_val2, 12) -> 207;
var_index(l_int_div, 1) -> 208;
var_index(l_fetch, 8) -> 209;
var_index(l_catch, 2) -> 210;
var_index(catch_end, 2) -> 211;
var_index(l_call_ext_last, 1) -> 212;
var_index(l_allocate_zero, 5) -> 213;
var_index(l_move_call_ext_only, 10) -> 214;
var_index(extract_next_element, 5) -> 215;
var_index(init, 2) -> 216;
var_index(try_end, 4) -> 217;
var_index(extract_next_element2, 3) -> 218;
var_index(is_tuple, 1) -> 219;
var_index(l_is_ne_exact_immed, 2) -> 220;
var_index(is_float, 0) -> 221;
var_index(try_end, 1) -> 222;
var_index(l_move_call, 4) -> 223;
var_index(extract_next_element, 3) -> 224;
var_index(l_call_ext, 0) -> 225;
var_index(is_integer_allocate, 0) -> 226;
var_index(l_fetch, 16) -> 227;
var_index(int_code_end, 0) -> 228;
var_index(move_return, 6) -> 229;
var_index(l_move_call_ext, 50) -> 230;
var_index(l_call_ext, 12) -> 231;
var_index(extract_next_element2, 5) -> 232;
var_index(l_catch, 4) -> 233;
var_index(is_nil, 6) -> 234;
var_index(try_end, 2) -> 235;
var_index(deallocate_return, 6) -> 236;
var_index(l_is_eq_exact_immed, 19) -> 237;
var_index(extract_next_element2, 13) -> 238;
var_index(l_move_call, 3) -> 239;
var_index(l_move_call_ext_only, 1) -> 240;
var_index(is_port, 0) -> 241;
var_index(l_move_call_ext, 7) -> 242;
var_index(l_is_eq_exact_immed, 21) -> 243;
var_index(l_is_eq_exact_immed, 12) -> 244;
var_index(l_apply, 0) -> 245;
var_index(is_list, 4) -> 246;
var_index(l_get, 2) -> 247;
var_index(l_select_val2, 5) -> 248;
var_index(node, 3) -> 249;
var_index(l_is_eq_exact_immed, 27) -> 250;
var_index(deallocate_return, 9) -> 251;
var_index(extract_next_element2, 10) -> 252;
var_index(call_bif, 42) -> 253;
var_index(l_select_val2, 13) -> 254;
var_index(l_move_call_only, 6) -> 255;
var_index(l_call_ext, 80) -> 256;
var_index(l_is_eq_exact_literal, 4) -> 257;
var_index(l_bs_init_bits_fail, 0) -> 258;
var_index(l_move_call_last, 5) -> 259;
var_index(l_new_bs_put_binary_all, 1) -> 260;
var_index(l_rem, 0) -> 261;
var_index(l_move_call_last, 1) -> 262;
var_index(move_return, 14) -> 263;
var_index(l_plus, 2) -> 264;
var_index(l_bs_put_string, 2) -> 265;
var_index(l_new_bs_put_integer, 0) -> 266;
var_index(move_deallocate_return, 7) -> 267;
var_index(wait_timeout, 0) -> 268;
var_index(l_is_eq, 0) -> 269;
var_index(l_move_call_only, 2) -> 270;
var_index(is_integer, 4) -> 271;
var_index(l_fetch, 12) -> 272;
var_index(l_call_fun, 3) -> 273;
var_index(l_move_call_only, 4) -> 274;
var_index(l_move_call_only, 10) -> 275;
var_index(l_select_val2, 7) -> 276;
var_index(l_fetch, 23) -> 277;
var_index(is_nonempty_list, 6) -> 278;
var_index(l_increment, 3) -> 279;
var_index(is_list, 1) -> 280;
var_index(bif1_body, 0) -> 281;
var_index(is_nonempty_list_allocate, 0) -> 282;
var_index(l_bs_get_integer_8, 1) -> 283;
var_index(l_gc_bif1, 3) -> 284;
var_index(apply, 0) -> 285;
var_index(l_is_ne_exact_literal, 0) -> 286;
var_index(get_list, 2) -> 287;
var_index(l_element, 0) -> 288;
var_index(l_fetch, 6) -> 289;
var_index(l_is_ne_exact, 0) -> 290;
var_index(l_call_last, 2) -> 291;
var_index(get_tuple_element, 4) -> 292;
var_index(get_tuple_element, 5) -> 293;
var_index(call_bif, 12) -> 294;
var_index(is_nonempty_list_test_heap, 0) -> 295;
var_index(l_move_call, 0) -> 296;
var_index(is_integer, 3) -> 297;
var_index(call_bif, 21) -> 298;
var_index(l_bs_skip_bits_imm2, 1) -> 299;
var_index(is_nonempty_list, 3) -> 300;
var_index(l_increment, 1) -> 301;
var_index(test_arity, 1) -> 302;
var_index(put_list, 4) -> 303;
var_index(is_tuple_of_arity, 3) -> 304;
var_index(move_return, 8) -> 305;
var_index(is_integer, 1) -> 306;
var_index(l_bs_test_unit_8, 3) -> 307;
var_index(l_bs_put_string, 0) -> 308;
var_index(l_bs_test_zero_tail2, 3) -> 309;
var_index(l_bs_get_binary_all_reuse, 1) -> 310;
var_index(l_is_eq_exact_immed, 9) -> 311;
var_index(put_list, 5) -> 312;
var_index(is_nil, 4) -> 313;
var_index(bif2_body, 0) -> 314;
var_index(l_fast_element, 2) -> 315;
var_index(l_trim, 4) -> 316;
var_index(catch_end, 0) -> 317;
var_index(get_tuple_element, 3) -> 318;
var_index(self, 1) -> 319;
var_index(l_move_call_only, 8) -> 320;
var_index(move_deallocate_return, 4) -> 321;
var_index(l_bs_test_unit_8, 1) -> 322;
var_index(timeout, 0) -> 323;
var_index(is_binary, 0) -> 324;
var_index(l_allocate, 7) -> 325;
var_index(move, 13) -> 326;
var_index(l_bif2, 2) -> 327;
var_index(l_move_call_ext, 8) -> 328;
var_index(l_trim, 2) -> 329;
var_index(l_is_ne_exact_immed, 4) -> 330;
var_index(call_bif, 29) -> 331;
var_index(call_bif, 46) -> 332;
var_index(l_catch, 6) -> 333;
var_index(recv_mark, 0) -> 334;
var_index(l_recv_set, 0) -> 335;
var_index(self, 2) -> 336;
var_index(catch_end, 6) -> 337;
var_index(l_is_eq_exact_literal, 0) -> 338;
var_index(l_call_ext_only, 5) -> 339;
var_index(l_is_eq_exact_immed, 11) -> 340;
var_index(extract_next_element, 10) -> 341;
var_index(l_fetch, 20) -> 342;
var_index(l_is_ne_exact_immed, 10) -> 343;
var_index(is_nonempty_list, 5) -> 344;
var_index(l_is_eq_exact_immed, 35) -> 345;
var_index(l_select_val2, 4) -> 346;
var_index(wait, 0) -> 347;
var_index(l_select_val_smallints, 1) -> 348;
var_index(move, 9) -> 349;
var_index(is_tuple, 4) -> 350;
var_index(move_return, 2) -> 351;
var_index(l_move_call_only, 5) -> 352;
var_index(l_allocate, 5) -> 353;
var_index(call_bif, 11) -> 354;
var_index(extract_next_element3, 3) -> 355;
var_index(l_move_call, 12) -> 356;
var_index(l_fast_element, 1) -> 357;
var_index(move_jump, 3) -> 358;
var_index(extract_next_element, 9) -> 359;
var_index(extract_next_element2, 4) -> 360;
var_index(l_move_call, 2) -> 361;
var_index(l_move_call_only, 0) -> 362;
var_index(is_nonempty_list, 8) -> 363;
var_index(l_catch, 3) -> 364;
var_index(call_bif, 28) -> 365;
var_index(l_call_ext, 28) -> 366;
var_index(is_integer, 2) -> 367;
var_index(is_nonempty_list, 7) -> 368;
var_index(l_is_ne_exact_immed, 3) -> 369;
var_index(try_end, 5) -> 370;
var_index(is_tuple, 3) -> 371;
var_index(extract_next_element2, 12) -> 372;
var_index(l_call_last, 11) -> 373;
var_index(put_list, 14) -> 374;
var_index(move_return, 26) -> 375;
var_index(call_bif, 44) -> 376;
var_index(extract_next_element3, 1) -> 377;
var_index(l_move_call, 5) -> 378;
var_index(l_call_ext, 101) -> 379;
var_index(call_bif, 17) -> 380;
var_index(move_jump, 0) -> 381;
var_index(init, 3) -> 382;
var_index(l_call_ext_only, 2) -> 383;
var_index(put_list, 6) -> 384;
var_index(get_tuple_element, 2) -> 385;
var_index(l_catch, 1) -> 386;
var_index(l_bs_test_unit_8, 0) -> 387;
var_index(loop_rec_end, 0) -> 388;
var_index(l_band, 0) -> 389;
var_index(l_move_call_ext, 9) -> 390;
var_index(is_nil, 5) -> 391;
var_index(self, 3) -> 392;
var_index(test_heap_1_put_list, 1) -> 393;
var_index(bif2_body, 1) -> 394;
var_index(l_bs_skip_bits_imm2, 0) -> 395;
var_index(l_is_eq_exact_immed, 16) -> 396;
var_index(l_bs_restore2, 0) -> 397;
var_index(is_nonempty_list, 9) -> 398;
var_index(l_move_call_ext, 6) -> 399;
var_index(call_bif, 27) -> 400;
var_index(l_move_call, 6) -> 401;
var_index(catch_end, 1) -> 402;
var_index(l_call_ext_last, 2) -> 403;
var_index(get_list, 6) -> 404;
var_index(move, 7) -> 405;
var_index(l_allocate_zero, 6) -> 406;
var_index(extract_next_element, 8) -> 407;
var_index(l_call_ext, 37) -> 408;
var_index(l_move_call_ext_last, 2) -> 409;
var_index(extract_next_element, 12) -> 410;
var_index(l_call_last, 7) -> 411;
var_index(l_is_eq_exact_immed, 26) -> 412;
var_index(call_bif, 25) -> 413;
var_index(move_return, 22) -> 414;
var_index(call_bif, 37) -> 415;
var_index(is_nil, 11) -> 416;
var_index(init, 4) -> 417;
var_index(put_list, 9) -> 418;
var_index(l_call_ext, 4) -> 419;
var_index(l_is_ne_exact_immed, 0) -> 420;
var_index(l_jump_on_val, 1) -> 421;
var_index(l_is_ne_exact_immed, 11) -> 422;
var_index(l_call_last, 5) -> 423;
var_index(l_call_ext_only, 4) -> 424;
var_index(extract_next_element, 11) -> 425;
var_index(l_call_fun_last, 0) -> 426;
var_index(l_call_ext, 25) -> 427;
var_index(extract_next_element, 15) -> 428;
var_index(l_move_call_ext, 14) -> 429;
var_index(l_move_call_ext, 5) -> 430;
var_index(is_nil, 7) -> 431;
var_index(l_bs_save2, 0) -> 432;
var_index(try_end, 0) -> 433;
var_index(l_int_div, 0) -> 434;
var_index(bif1_body, 3) -> 435;
var_index(l_bor, 0) -> 436;
var_index(l_is_eq_exact_immed, 18) -> 437;
var_index(extract_next_element, 7) -> 438;
var_index(l_bsl, 1) -> 439;
var_index(l_move_call_ext_only, 3) -> 440;
var_index(deallocate_return, 7) -> 441;
var_index(l_jump_on_val, 0) -> 442;
var_index(get_list, 8) -> 443;
var_index(catch_end, 3) -> 444;
var_index(is_nil, 8) -> 445;
var_index(put_list, 10) -> 446;
var_index(get_list, 5) -> 447;
var_index(is_tuple, 9) -> 448;
var_index(l_apply_last, 0) -> 449;
var_index(l_call_ext, 43) -> 450;
var_index(call_bif, 30) -> 451;
var_index(l_bsr, 0) -> 452;
var_index(l_move_call_ext, 15) -> 453;
var_index(l_bs_skip_bits2, 0) -> 454;
var_index(l_call_ext, 71) -> 455;
var_index(l_is_ne_exact_immed, 1) -> 456;
var_index(is_list, 3) -> 457;
var_index(init, 5) -> 458;
var_index(call_bif, 40) -> 459;
var_index(l_is_eq_exact_immed, 22) -> 460;
var_index(extract_next_element, 13) -> 461;
var_index(l_is_ne_exact_immed, 8) -> 462;
var_index(l_get, 1) -> 463;
var_index(is_nonempty_list, 10) -> 464;
var_index(l_move_call, 7) -> 465;
var_index(l_fast_element, 0) -> 466;
var_index(l_move_call_ext, 21) -> 467;
var_index(is_bitstr, 0) -> 468;
var_index(l_select_val2, 14) -> 469;
var_index(call_bif, 31) -> 470;
var_index(l_call_ext, 96) -> 471;
var_index(move_return, 17) -> 472;
var_index(l_is_eq_exact_immed, 15) -> 473;
var_index(l_call_ext, 51) -> 474;
var_index(call_bif, 10) -> 475;
var_index(move_jump, 13) -> 476;
var_index(l_trim, 5) -> 477;
var_index(is_function, 0) -> 478;
var_index(l_call_ext, 2) -> 479;
var_index(call_bif, 26) -> 480;
var_index(extract_next_element, 16) -> 481;
var_index(l_move_call, 8) -> 482;
var_index(extract_next_element2, 7) -> 483;
var_index(l_allocate, 8) -> 484;
var_index(bif1_body, 7) -> 485;
var_index(l_call_ext_last, 6) -> 486;
var_index(l_fetch, 7) -> 487;
var_index(deallocate_return, 10) -> 488;
var_index(call_bif, 38) -> 489;
var_index(init, 9) -> 490;
var_index(move_return, 10) -> 491;
var_index(is_nil, 9) -> 492;
var_index(move_deallocate_return, 6) -> 493;
var_index(l_fdiv, 0) -> 494;
var_index(l_band, 2) -> 495;
var_index(l_bs_test_zero_tail2, 5) -> 496;
var_index(call_bif, 22) -> 497;
var_index(l_fcheckerror, 0) -> 498;
var_index(fclearerror, 0) -> 499;
var_index(allocate_heap_zero, 0) -> 500;
var_index(fconv, 0) -> 501;
var_index(fmove_1, 0) -> 502;
var_index(l_select_val_atoms, 1) -> 503;
var_index(l_move_call_ext_last, 6) -> 504;
var_index(l_bsl, 0) -> 505;
var_index(fmove_2, 1) -> 506;
var_index(l_increment, 6) -> 507;
var_index(l_call_ext, 41) -> 508;
var_index(l_move_call_ext, 2) -> 509;
var_index(is_tuple, 7) -> 510;
var_index(l_call_ext, 20) -> 511;
var_index(init, 6) -> 512;
var_index(l_move_call_last, 4) -> 513;
var_index(l_call_ext, 38) -> 514;
var_index(extract_next_element3, 6) -> 515;
var_index(move_return, 21) -> 516;
var_index(l_is_ne, 0) -> 517;
var_index(extract_next_element2, 6) -> 518;
var_index(l_call_ext, 66) -> 519;
var_index(is_nonempty_list, 24) -> 520;
var_index(move_jump, 2) -> 521;
var_index(l_allocate_zero, 7) -> 522;
var_index(l_call_ext, 23) -> 523;
var_index(l_call_ext, 11) -> 524;
var_index(l_catch, 8) -> 525;
var_index(is_pid, 1) -> 526;
var_index(is_reference, 0) -> 527;
var_index(call_bif, 32) -> 528;
var_index(is_nonempty_list, 12) -> 529;
var_index(l_is_eq_exact_immed, 20) -> 530;
var_index(l_bs_test_zero_tail2, 1) -> 531;
var_index(l_move_call_ext, 4) -> 532;
var_index(extract_next_element2, 8) -> 533;
var_index(l_call_ext, 16) -> 534;
var_index(l_rem, 2) -> 535;
var_index(l_move_call_ext, 38) -> 536;
var_index(l_move_call_ext, 3) -> 537;
var_index(catch_end, 8) -> 538;
var_index(self, 6) -> 539;
var_index(l_is_eq_exact_immed, 13) -> 540;
var_index(l_call_ext, 105) -> 541;
var_index(l_move_call_ext, 10) -> 542;
var_index(l_is_eq_exact_immed, 30) -> 543;
var_index(l_call_ext, 7) -> 544;
var_index(l_call_last, 8) -> 545;
var_index(test_arity, 3) -> 546;
var_index(is_boolean, 0) -> 547;
var_index(l_allocate_zero, 10) -> 548;
var_index(l_call_ext, 26) -> 549;
var_index(l_call_ext, 47) -> 550;
var_index(is_list, 6) -> 551;
var_index(is_nonempty_list, 15) -> 552;
var_index(l_is_eq_exact_literal, 7) -> 553;
var_index(l_move_call_ext_only, 6) -> 554;
var_index(self, 4) -> 555;
var_index(l_call_ext, 34) -> 556;
var_index(l_is_eq_exact_immed, 24) -> 557;
var_index(l_call_ext, 17) -> 558;
var_index(is_function, 1) -> 559;
var_index(l_move_call, 15) -> 560;
var_index(l_fetch, 19) -> 561;
var_index(move_return, 28) -> 562;
var_index(node, 4) -> 563;
var_index(l_call_ext, 45) -> 564;
var_index(l_bor, 1) -> 565;
var_index(move_return, 7) -> 566;
var_index(extract_next_element3, 10) -> 567;
var_index(l_call_ext, 82) -> 568;
var_index(is_nonempty_list, 11) -> 569;
var_index(is_atom, 4) -> 570;
var_index(l_call_ext, 8) -> 571;
var_index(l_apply_fun, 0) -> 572;
var_index(l_call_ext, 109) -> 573;
var_index(extract_next_element2, 14) -> 574;
var_index(move_return, 16) -> 575;
var_index(is_nil, 15) -> 576;
var_index(l_call_ext_last, 3) -> 577;
var_index(l_allocate_zero, 8) -> 578;
var_index(try_end, 3) -> 579;
var_index(l_fetch, 15) -> 580;
var_index(l_fast_element, 3) -> 581;
var_index(l_allocate_zero, 9) -> 582;
var_index(is_nil, 12) -> 583;
var_index(l_select_val2, 10) -> 584;
var_index(move_deallocate_return, 5) -> 585;
var_index(extract_next_element, 14) -> 586;
var_index(call_bif, 6) -> 587;
var_index(l_call_ext, 64) -> 588;
var_index(extract_next_element3, 5) -> 589;
var_index(l_move_call_ext, 22) -> 590;
var_index(call_bif, 33) -> 591;
var_index(l_call_ext, 74) -> 592;
var_index(is_nil, 13) -> 593;
var_index(l_move_call_ext_only, 0) -> 594;
var_index(init, 7) -> 595;
var_index(is_tuple, 5) -> 596;
var_index(l_call_ext, 31) -> 597;
var_index(try_end, 6) -> 598;
var_index(is_atom, 6) -> 599;
var_index(set_tuple_element, 1) -> 600;
var_index(put_list, 7) -> 601;
var_index(l_call_ext_last, 4) -> 602;
var_index(l_increment, 5) -> 603;
var_index(is_atom, 5) -> 604;
var_index(l_is_eq_exact_immed, 14) -> 605;
var_index(l_call_ext, 55) -> 606;
var_index(call_bif, 35) -> 607;
var_index(call_bif, 23) -> 608;
var_index(l_move_call, 10) -> 609;
var_index(move_return, 36) -> 610;
var_index(l_move_call_only, 7) -> 611;
var_index(l_call_last, 10) -> 612;
var_index(bif1_body, 5) -> 613;
var_index(init, 8) -> 614;
var_index(call_bif, 20) -> 615;
var_index(l_is_eq_exact_immed, 36) -> 616;
var_index(l_call_ext, 42) -> 617;
var_index(extract_next_element, 21) -> 618;
var_index(l_int_bnot, 0) -> 619;
var_index(deallocate_return, 8) -> 620;
var_index(call_bif, 34) -> 621;
var_index(l_put_tuple, 6) -> 622;
var_index(node, 0) -> 623;
var_index(l_is_eq_exact_immed, 25) -> 624;
var_index(l_move_call, 13) -> 625;
var_index(bif2_body, 2) -> 626;
var_index(init, 10) -> 627;
var_index(extract_next_element2, 17) -> 628;
var_index(l_new_bs_put_integer, 2) -> 629;
var_index(l_move_call, 16) -> 630;
var_index(l_move_call_ext, 36) -> 631;
var_index(is_tuple, 8) -> 632;
var_index(extract_next_element2, 11) -> 633;
var_index(is_nil, 28) -> 634;
var_index(put_list, 12) -> 635;
var_index(get_list, 4) -> 636;
var_index(l_bs_get_integer_16, 1) -> 637;
var_index(catch_end, 4) -> 638;
var_index(extract_next_element, 17) -> 639;
var_index(try_end, 7) -> 640;
var_index(call_bif, 43) -> 641;
var_index(is_tuple, 6) -> 642;
var_index(l_call_ext, 44) -> 643;
var_index(l_call_ext, 3) -> 644;
var_index(l_trim, 8) -> 645;
var_index(put_list, 11) -> 646;
var_index(l_is_eq_exact_literal, 6) -> 647;
var_index(l_move_call_ext, 44) -> 648;
var_index(l_get, 6) -> 649;
var_index(call_bif, 5) -> 650;
var_index(l_call_ext, 91) -> 651;
var_index(l_is_ne_exact_immed, 5) -> 652;
var_index(is_nil, 10) -> 653;
var_index(l_move_call_ext, 37) -> 654;
var_index(node, 2) -> 655;
var_index(is_nil, 17) -> 656;
var_index(is_nonempty_list, 33) -> 657;
var_index(l_is_eq_exact_immed, 28) -> 658;
var_index(l_call_ext, 61) -> 659;
var_index(l_trim, 6) -> 660;
var_index(is_nil, 14) -> 661;
var_index(move_jump, 7) -> 662;
var_index(move_jump, 6) -> 663;
var_index(move_jump, 1) -> 664;
var_index(move_return, 19) -> 665;
var_index(bs_context_to_binary, 0) -> 666;
var_index(badmatch, 0) -> 667;
var_index(is_nonempty_list, 35) -> 668;
var_index(is_nonempty_list, 19) -> 669;
var_index(l_move_call, 25) -> 670;
var_index(bif1_body, 6) -> 671;
var_index(bif1_body, 1) -> 672;
var_index(bif2_body, 3) -> 673;
var_index(is_float, 1) -> 674;
var_index(node, 1) -> 675;
var_index(l_move_call_ext_last, 4) -> 676;
var_index(call_bif, 13) -> 677;
var_index(l_call_ext, 92) -> 678;
var_index(l_call_ext, 78) -> 679;
var_index(l_call_ext, 36) -> 680;
var_index(move_jump, 10) -> 681;
var_index(move_return, 9) -> 682;
var_index(l_bs_test_unit_8, 2) -> 683;
var_index(fconv, 2) -> 684;
var_index(deallocate_return, 11) -> 685;
var_index(l_call_ext_only, 0) -> 686;
var_index(move, 10) -> 687;
var_index(l_move_call_ext, 28) -> 688;
var_index(l_call_ext, 40) -> 689;
var_index(l_move_call, 17) -> 690;
var_index(l_move_call_ext, 1) -> 691;
var_index(extract_next_element, 24) -> 692;
var_index(l_is_ne_exact_immed, 6) -> 693;
var_index(l_trim, 7) -> 694;
var_index(l_call_fun, 4) -> 695;
var_index(l_apply_fun_only, 0) -> 696;
var_index(l_move_call_ext, 26) -> 697;
var_index(l_move_call_ext, 25) -> 698;
var_index(l_bs_skip_bits_all2, 1) -> 699;
var_index(l_call_ext, 98) -> 700;
var_index(l_call_ext, 14) -> 701;
var_index(move_return, 11) -> 702;
var_index(bs_context_to_binary, 9) -> 703;
var_index(is_nonempty_list, 14) -> 704;
var_index(l_move_call_ext_only, 7) -> 705;
var_index(bif1_body, 4) -> 706;
var_index(l_move_call_ext, 46) -> 707;
var_index(test_heap_1_put_list, 4) -> 708;
var_index(self, 5) -> 709;
var_index(l_call_ext, 88) -> 710;
var_index(l_call_ext, 86) -> 711;
var_index(extract_next_element, 19) -> 712;
var_index(l_fadd, 0) -> 713;
var_index(extract_next_element2, 16) -> 714;
var_index(move_jump, 12) -> 715;
var_index(move_return, 45) -> 716;
var_index(move_return, 29) -> 717;
var_index(move_return, 13) -> 718;
var_index(move_deallocate_return, 8) -> 719;
var_index(is_bigint, 0) -> 720;
var_index(fmove_2, 0) -> 721;
var_index(fmove_1, 1) -> 722;
var_index(l_move_call_last, 7) -> 723;
var_index(l_is_ne_exact_immed, 9) -> 724;
var_index(l_fast_element, 5) -> 725;
var_index(is_nonempty_list, 13) -> 726;
var_index(l_is_eq_exact_literal, 3) -> 727;
var_index(test_heap_1_put_list, 3) -> 728;
var_index(l_call_ext, 84) -> 729;
var_index(l_call_ext, 75) -> 730;
var_index(l_call_ext, 62) -> 731;
var_index(l_call_ext, 58) -> 732;
var_index(l_call_ext, 48) -> 733;
var_index(l_call_ext, 35) -> 734;
var_index(l_call_ext, 10) -> 735;
var_index(extract_next_element2, 9) -> 736;
var_index(is_nil, 20) -> 737;
var_index(l_bs_put_string, 1) -> 738;
var_index(is_list, 5) -> 739;
var_index(is_nonempty_list, 22) -> 740;
var_index(get_list, 10) -> 741;
var_index(l_move_call, 9) -> 742;
var_index(l_move_call_ext, 24) -> 743;
var_index(l_move_call_ext, 16) -> 744;
var_index(l_is_eq_exact_immed, 29) -> 745;
var_index(l_call_ext, 77) -> 746;
var_index(l_select_val_atoms, 2) -> 747;
var_index(is_nonempty_list, 21) -> 748;
var_index(is_binary, 3) -> 749;
var_index(l_move_call_ext_only, 4) -> 750;
var_index(l_move_call_ext, 17) -> 751;
var_index(init, 11) -> 752;
var_index(l_get, 3) -> 753;
var_index(l_bs_test_zero_tail2, 2) -> 754;
var_index(l_bs_get_integer, 0) -> 755;
var_index(func_info, 0) -> 756;
var_index(extract_next_element, 22) -> 757;
var_index(l_select_val_atoms, 3) -> 758;
var_index(l_trim, 10) -> 759;
var_index(is_nil, 21) -> 760;
var_index(l_move_call_ext_last, 5) -> 761;
var_index(l_fsub, 0) -> 762;
var_index(l_move_call_ext, 35) -> 763;
var_index(l_move_call_ext, 31) -> 764;
var_index(l_move_call_ext, 12) -> 765;
var_index(init, 15) -> 766;
var_index(l_wait_timeout, 5) -> 767;
var_index(l_call_last, 9) -> 768;
var_index(l_is_eq_exact_immed, 31) -> 769;
var_index(l_call_ext, 104) -> 770;
var_index(l_call_ext, 65) -> 771;
var_index(l_call_ext, 59) -> 772;
var_index(l_call_ext, 5) -> 773;
var_index(l_new_bs_put_integer, 1) -> 774;
var_index(move_return, 34) -> 775;
var_index(move_return, 12) -> 776;
var_index(l_trim, 11) -> 777;
var_index(l_move_call_only, 9) -> 778;
var_index(l_move_call_ext_last, 3) -> 779;
var_index(l_move_call_ext_last, 1) -> 780;
var_index(is_nonempty_list, 29) -> 781;
var_index(is_nonempty_list, 23) -> 782;
var_index(is_nonempty_list, 17) -> 783;
var_index(l_call_fun, 2) -> 784;
var_index(l_apply_only, 0) -> 785;
var_index(l_fetch, 21) -> 786;
var_index(l_move_call_ext, 47) -> 787;
var_index(l_move_call_ext, 19) -> 788;
var_index(l_bs_test_zero_tail2, 0) -> 789;
var_index(call_bif, 45) -> 790;
var_index(bs_init_writable, 0) -> 791;
var_index(l_call_ext, 39) -> 792;
var_index(extract_next_element, 20) -> 793;
var_index(move_jump, 11) -> 794;
var_index(move_jump, 4) -> 795;
var_index(move_return, 47) -> 796;
var_index(move_return, 46) -> 797;
var_index(move_return, 39) -> 798;
var_index(move_return, 33) -> 799;
var_index(move_return, 20) -> 800;
var_index(is_nil, 27) -> 801;
var_index(is_nil, 22) -> 802;
var_index(l_bs_put_string, 5) -> 803;
var_index(l_bs_put_string, 3) -> 804;
var_index(put_list, 13) -> 805;
var_index(is_nonempty_list, 18) -> 806;
var_index(l_increment, 7) -> 807;
var_index(l_times, 2) -> 808;
var_index(l_bs_get_integer_imm, 0) -> 809;
var_index(l_move_call, 11) -> 810;
var_index(l_get, 5) -> 811;
var_index(call_bif, 24) -> 812;
var_index(call_bif, 3) -> 813;
var_index(l_is_eq_exact_immed, 34) -> 814;
var_index(l_move_call_last, 6) -> 815;
var_index(l_call_ext, 9) -> 816;
var_index(l_is_ne_exact_immed, 7) -> 817;
var_index(move_jump, 5) -> 818;
var_index(move_return, 40) -> 819;
var_index(l_trim, 9) -> 820;
var_index(bs_context_to_binary, 2) -> 821;
var_index(is_nonempty_list, 30) -> 822;
var_index(is_nonempty_list, 27) -> 823;
var_index(l_make_export, 0) -> 824;
var_index(l_select_val2, 11) -> 825;
var_index(is_number, 0) -> 826;
var_index(move_deallocate_return, 10) -> 827;
var_index(l_move_call, 28) -> 828;
var_index(l_move_call, 14) -> 829;
var_index(l_move_call_ext_only, 2) -> 830;
var_index(init, 14) -> 831;
var_index(init, 13) -> 832;
var_index(init, 12) -> 833;
var_index(l_wait_timeout, 3) -> 834;
var_index(l_wait_timeout, 2) -> 835;
var_index(l_wait_timeout, 1) -> 836;
var_index(l_bs_skip_bits_all2, 0) -> 837;
var_index(l_bs_test_zero_tail2, 6) -> 838;
var_index(call_bif, 39) -> 839;
var_index(call_bif, 19) -> 840;
var_index(call_bif, 4) -> 841;
var_index(call_bif, 2) -> 842;
var_index(call_bif, 1) -> 843;
var_index(call_bif, 0) -> 844;
var_index(l_int_div, 2) -> 845;
var_index(l_bs_put_utf16, 0) -> 846;
var_index(l_bs_get_utf16, 2) -> 847;
var_index(l_bs_get_utf16, 1) -> 848;
var_index(l_bs_get_utf16, 0) -> 849;
var_index(l_allocate, 9) -> 850;
var_index(l_put_tuple, 7) -> 851;
var_index(l_put_tuple, 5) -> 852;
var_index(l_put_tuple, 4) -> 853;
var_index(l_put_tuple, 3) -> 854;
var_index(l_put_tuple, 2) -> 855;
var_index(l_put_tuple, 1) -> 856;
var_index(l_is_eq_exact_immed, 33) -> 857;
var_index(l_is_eq_exact_immed, 32) -> 858;
var_index(l_is_eq_exact_immed, 23) -> 859;
var_index(l_is_eq_exact_immed, 17) -> 860;
var_index(l_is_eq_exact_immed, 10) -> 861;
var_index(l_is_eq_exact_immed, 4) -> 862;
var_index(l_call_ext, 108) -> 863;
var_index(l_call_ext, 107) -> 864;
var_index(l_call_ext, 106) -> 865;
var_index(l_call_ext, 103) -> 866;
var_index(l_call_ext, 102) -> 867;
var_index(l_call_ext, 100) -> 868;
var_index(l_call_ext, 99) -> 869;
var_index(l_call_ext, 97) -> 870;
var_index(l_call_ext, 95) -> 871;
var_index(l_call_ext, 94) -> 872;
var_index(l_call_ext, 93) -> 873;
var_index(l_call_ext, 90) -> 874;
var_index(l_call_ext, 89) -> 875;
var_index(l_call_ext, 87) -> 876;
var_index(l_call_ext, 85) -> 877;
var_index(l_call_ext, 83) -> 878;
var_index(l_call_ext, 81) -> 879;
var_index(l_call_ext, 79) -> 880;
var_index(l_call_ext, 76) -> 881;
var_index(l_call_ext, 73) -> 882;
var_index(l_call_ext, 72) -> 883;
var_index(l_call_ext, 70) -> 884;
var_index(l_call_ext, 69) -> 885;
var_index(l_call_ext, 68) -> 886;
var_index(l_call_ext, 67) -> 887;
var_index(l_call_ext, 63) -> 888;
var_index(l_call_ext, 60) -> 889;
var_index(l_call_ext, 57) -> 890;
var_index(l_call_ext, 56) -> 891;
var_index(l_call_ext, 54) -> 892;
var_index(l_call_ext, 53) -> 893;
var_index(l_call_ext, 52) -> 894;
var_index(l_call_ext, 50) -> 895;
var_index(l_call_ext, 49) -> 896;
var_index(l_call_ext, 46) -> 897;
var_index(l_call_ext, 33) -> 898;
var_index(l_call_ext, 32) -> 899;
var_index(l_call_ext, 30) -> 900;
var_index(l_call_ext, 29) -> 901;
var_index(l_call_ext, 27) -> 902;
var_index(l_call_ext, 22) -> 903;
var_index(l_call_ext, 21) -> 904;
var_index(l_call_ext, 19) -> 905;
var_index(l_call_ext, 18) -> 906;
var_index(l_call_ext, 15) -> 907;
var_index(l_call_ext, 13) -> 908;
var_index(l_call_ext, 1) -> 909;
var_index(allocate_init, 1) -> 910;
var_index(extract_next_element, 23) -> 911;
var_index(extract_next_element, 18) -> 912;
var_index(get_tuple_element, 11) -> 913;
var_index(get_tuple_element, 10) -> 914;
var_index(get_tuple_element, 9) -> 915;
var_index(get_tuple_element, 8) -> 916;
var_index(get_tuple_element, 7) -> 917;
var_index(get_tuple_element, 6) -> 918;
var_index(set_tuple_element, 2) -> 919;
var_index(l_call_fun_last, 1) -> 920;
var_index(l_fast_element, 4) -> 921;
var_index(l_bs_test_unit, 0) -> 922;
var_index(extract_next_element3, 9) -> 923;
var_index(extract_next_element3, 8) -> 924;
var_index(extract_next_element2, 15) -> 925;
var_index(l_bs_start_match2, 4) -> 926;
var_index(l_bs_start_match2, 3) -> 927;
var_index(l_bs_start_match2, 2) -> 928;
var_index(l_bs_start_match2, 1) -> 929;
var_index(l_bs_get_integer_32, 3) -> 930;
var_index(l_bs_get_integer_32, 2) -> 931;
var_index(l_bs_get_integer_32, 1) -> 932;
var_index(l_bor, 2) -> 933;
var_index(l_bsr, 1) -> 934;
var_index(l_bs_get_binary_imm2, 2) -> 935;
var_index(l_bs_get_binary_imm2, 1) -> 936;
var_index(l_bs_test_tail_imm2, 0) -> 937;
var_index(l_bxor, 0) -> 938;
var_index(l_bs_get_float2, 0) -> 939;
var_index(move_jump, 9) -> 940;
var_index(move_jump, 8) -> 941;
var_index(allocate_heap, 1) -> 942;
var_index(move_return, 44) -> 943;
var_index(move_return, 43) -> 944;
var_index(move_return, 42) -> 945;
var_index(move_return, 41) -> 946;
var_index(move_return, 38) -> 947;
var_index(move_return, 37) -> 948;
var_index(move_return, 35) -> 949;
var_index(move_return, 32) -> 950;
var_index(move_return, 31) -> 951;
var_index(move_return, 30) -> 952;
var_index(move_return, 27) -> 953;
var_index(move_return, 25) -> 954;
var_index(move_return, 24) -> 955;
var_index(move_return, 23) -> 956;
var_index(move_return, 18) -> 957;
var_index(move_return, 15) -> 958;
var_index(l_new_bs_put_integer_imm, 2) -> 959;
var_index(l_new_bs_put_integer_imm, 1) -> 960;
var_index(l_bs_get_integer_small_imm, 1) -> 961;
var_index(l_rem, 1) -> 962;
var_index(is_nil, 26) -> 963;
var_index(is_nil, 25) -> 964;
var_index(is_nil, 24) -> 965;
var_index(is_nil, 23) -> 966;
var_index(is_nil, 19) -> 967;
var_index(is_nil, 18) -> 968;
var_index(is_nil, 16) -> 969;
var_index(l_bsl, 2) -> 970;
var_index(l_fmul, 0) -> 971;
var_index(is_tuple_of_arity, 4) -> 972;
var_index(is_tuple_of_arity, 2) -> 973;
var_index(test_arity, 4) -> 974;
var_index(test_arity, 2) -> 975;
var_index(bs_context_to_binary, 8) -> 976;
var_index(bs_context_to_binary, 7) -> 977;
var_index(bs_context_to_binary, 6) -> 978;
var_index(bs_context_to_binary, 5) -> 979;
var_index(bs_context_to_binary, 4) -> 980;
var_index(bs_context_to_binary, 3) -> 981;
var_index(bs_context_to_binary, 1) -> 982;
var_index(l_new_bs_put_binary, 0) -> 983;
var_index(badmatch, 17) -> 984;
var_index(badmatch, 16) -> 985;
var_index(badmatch, 15) -> 986;
var_index(badmatch, 14) -> 987;
var_index(badmatch, 13) -> 988;
var_index(badmatch, 12) -> 989;
var_index(badmatch, 11) -> 990;
var_index(badmatch, 10) -> 991;
var_index(badmatch, 9) -> 992;
var_index(badmatch, 8) -> 993;
var_index(badmatch, 7) -> 994;
var_index(badmatch, 6) -> 995;
var_index(badmatch, 5) -> 996;
var_index(badmatch, 4) -> 997;
var_index(badmatch, 3) -> 998;
var_index(badmatch, 2) -> 999;
var_index(badmatch, 1) -> 1000;
var_index(l_bs_test_unit_8, 4) -> 1001;
var_index(l_bs_get_utf8, 1) -> 1002;
var_index(l_bs_get_utf8, 0) -> 1003;
var_index(l_bs_put_utf8, 0) -> 1004;
var_index(l_bs_match_string, 2) -> 1005;
var_index(l_bs_match_string, 1) -> 1006;
var_index(l_bs_put_string, 4) -> 1007;
var_index(fconv, 1) -> 1008;
var_index(l_m_div, 0) -> 1009;
var_index(raise, 1) -> 1010;
var_index(raise, 0) -> 1011;
var_index(is_integer_allocate, 1) -> 1012;
var_index(is_nonempty_list_allocate, 2) -> 1013;
var_index(l_fnegate, 0) -> 1014;
var_index(l_bs_validate_unicode, 0) -> 1015;
var_index(l_hibernate, 0) -> 1016;
var_index(put_list, 8) -> 1017;
var_index(is_nonempty_list, 41) -> 1018;
var_index(is_nonempty_list, 40) -> 1019;
var_index(is_nonempty_list, 39) -> 1020;
var_index(is_nonempty_list, 38) -> 1021;
var_index(is_nonempty_list, 37) -> 1022;
var_index(is_nonempty_list, 36) -> 1023;
var_index(is_nonempty_list, 34) -> 1024;
var_index(is_nonempty_list, 32) -> 1025;
var_index(is_nonempty_list, 31) -> 1026;
var_index(is_nonempty_list, 28) -> 1027;
var_index(is_nonempty_list, 26) -> 1028;
var_index(is_nonempty_list, 25) -> 1029;
var_index(is_nonempty_list, 20) -> 1030;
var_index(is_nonempty_list, 16) -> 1031;
var_index(get_list, 9) -> 1032;
var_index(get_list, 7) -> 1033;
var_index(get_list, 1) -> 1034;
var_index(case_end, 11) -> 1035;
var_index(case_end, 10) -> 1036;
var_index(case_end, 9) -> 1037;
var_index(case_end, 8) -> 1038;
var_index(case_end, 7) -> 1039;
var_index(case_end, 6) -> 1040;
var_index(case_end, 5) -> 1041;
var_index(case_end, 4) -> 1042;
var_index(case_end, 3) -> 1043;
var_index(case_end, 2) -> 1044;
var_index(case_end, 1) -> 1045;
var_index(case_end, 0) -> 1046;
var_index(try_case_end, 1) -> 1047;
var_index(try_case_end, 0) -> 1048;
var_index(apply_last, 1) -> 1049;
var_index(l_call_ext_last, 5) -> 1050;
var_index(l_bs_append, 1) -> 1051;
var_index(l_increment, 4) -> 1052;
var_index(l_bs_private_append, 0) -> 1053;
var_index(l_bs_init, 0) -> 1054;
var_index(l_new_bs_put_float, 0) -> 1055;
var_index(l_bs_validate_unicode_retract, 0) -> 1056;
var_index(l_yield, 0) -> 1057;
var_index(l_apply_fun_last, 0) -> 1058;
var_index(l_minus, 2) -> 1059;
var_index(l_minus, 1) -> 1060;
var_index(init3, 1) -> 1061;
var_index(l_select_val_smallints, 2) -> 1062;
var_index(l_bif2, 5) -> 1063;
var_index(l_bif2, 3) -> 1064;
var_index(l_select_val2, 1) -> 1065;
var_index(l_select_tuple_arity2, 3) -> 1066;
var_index(l_select_tuple_arity2, 2) -> 1067;
var_index(init2, 1) -> 1068;
var_index(l_bs_skip_bits_imm2, 2) -> 1069;
var_index(l_bs_restore2, 3) -> 1070;
var_index(l_bs_restore2, 2) -> 1071;
var_index(l_bs_restore2, 1) -> 1072;
var_index(l_bs_skip_bits2, 1) -> 1073;
var_index(l_bs_get_binary_all2, 2) -> 1074;
var_index(l_bs_get_binary_all2, 1) -> 1075;
var_index(l_bs_save2, 2) -> 1076;
var_index(l_bs_save2, 1) -> 1077;
var_index(is_function2, 0) -> 1078;
var_index(l_bif1, 2) -> 1079;
var_index(l_bif1, 1) -> 1080;
var_index(is_nonempty_list_test_heap, 1) -> 1081;
var_index(allocate_heap_zero, 1) -> 1082;
var_index(deallocate_return, 12) -> 1083;
var_index(move_deallocate_return, 9) -> 1084;
var_index(l_bs_init_heap_bin, 1) -> 1085;
var_index(l_call_fun, 1) -> 1086;
var_index(l_new_bs_put_binary_imm, 3) -> 1087;
var_index(l_new_bs_put_binary_imm, 2) -> 1088;
var_index(l_bs_get_integer_imm, 1) -> 1089;
var_index(l_new_bs_put_float_imm, 1) -> 1090;
var_index(l_new_bs_put_float_imm, 0) -> 1091;
var_index(l_move_call, 35) -> 1092;
var_index(l_move_call, 34) -> 1093;
var_index(l_move_call, 33) -> 1094;
var_index(l_move_call, 32) -> 1095;
var_index(l_move_call, 31) -> 1096;
var_index(l_move_call, 30) -> 1097;
var_index(l_move_call, 29) -> 1098;
var_index(l_move_call, 27) -> 1099;
var_index(l_move_call, 26) -> 1100;
var_index(l_move_call, 24) -> 1101;
var_index(l_move_call, 23) -> 1102;
var_index(l_move_call, 22) -> 1103;
var_index(l_move_call, 21) -> 1104;
var_index(l_move_call, 20) -> 1105;
var_index(l_move_call, 19) -> 1106;
var_index(l_move_call, 18) -> 1107;
var_index(l_is_eq_exact_literal, 2) -> 1108;
var_index(l_bs_init_bits_fail, 1) -> 1109;
var_index(l_jump_on_val, 2) -> 1110;
var_index(l_bs_init_fail, 1) -> 1111;
var_index(l_call_ext_only, 3) -> 1112;
var_index(l_call_ext_only, 1) -> 1113;
var_index(l_move_call_ext_only, 9) -> 1114;
var_index(l_move_call_ext_only, 8) -> 1115;
var_index(l_move_call_ext_only, 5) -> 1116;
var_index(bif1_body, 2) -> 1117;
var_index(l_select_tuple_arity, 2) -> 1118;
var_index(l_fetch, 22) -> 1119;
var_index(l_fetch, 11) -> 1120;
var_index(l_fetch, 9) -> 1121;
var_index(l_fetch, 5) -> 1122;
var_index(l_fetch, 3) -> 1123;
var_index(l_catch, 7) -> 1124;
var_index(l_bs_get_integer_8, 2) -> 1125;
var_index(l_bs_get_integer_16, 2) -> 1126;
var_index(move, 12) -> 1127;
var_index(l_bs_utf8_size, 0) -> 1128;
var_index(l_bs_utf16_size, 0) -> 1129;
var_index(l_move_call_ext, 49) -> 1130;
var_index(l_move_call_ext, 48) -> 1131;
var_index(l_move_call_ext, 45) -> 1132;
var_index(l_move_call_ext, 43) -> 1133;
var_index(l_move_call_ext, 42) -> 1134;
var_index(l_move_call_ext, 41) -> 1135;
var_index(l_move_call_ext, 40) -> 1136;
var_index(l_move_call_ext, 39) -> 1137;
var_index(l_move_call_ext, 34) -> 1138;
var_index(l_move_call_ext, 33) -> 1139;
var_index(l_move_call_ext, 32) -> 1140;
var_index(l_move_call_ext, 30) -> 1141;
var_index(l_move_call_ext, 29) -> 1142;
var_index(l_move_call_ext, 27) -> 1143;
var_index(l_move_call_ext, 23) -> 1144;
var_index(l_move_call_ext, 20) -> 1145;
var_index(l_move_call_ext, 18) -> 1146;
var_index(l_move_call_ext, 11) -> 1147;
var_index(l_move_call_ext, 0) -> 1148;
var_index(catch_end, 7) -> 1149;
var_index(test_heap_1_put_list, 2) -> 1150;
var_index(l_bs_add, 1) -> 1151;
var_index(l_band, 1) -> 1152;
var_index(on_load, 0) -> 1153;
var_index(l_get, 4) -> 1154;
var_index(l_get, 0) -> 1155;
var_index(if_end, 0) -> 1156;
var_index(l_wait_timeout, 4) -> 1157;
var_index(l_wait_timeout, 0) -> 1158;
var_index(system_limit, 0) -> 1159;
var_index(l_plus, 3) -> 1160;
var_index(l_plus, 1) -> 1161;
var_index(l_gc_bif3, 0) -> 1162;
var_index(move2, 10) -> 1163;
var_index(move2, 9) -> 1164;
var_index(move2, 8) -> 1165;
var_index(move2, 7) -> 1166;
var_index(move2, 6) -> 1167;
var_index(move2, 5) -> 1168;
var_index(move2, 4) -> 1169;
var_index(move2, 2) -> 1170;
var_index(l_bs_skip_bits_all2, 2) -> 1171;
var_index(l_bs_test_zero_tail2, 4) -> 1172;
var_index(l_bs_get_integer, 1) -> 1173;
var_index(l_bs_get_binary2, 1) -> 1174;
var_index(fmove_2, 2) -> 1175;
var_index(l_gc_bif2, 0) -> 1176;
var_index(l_gc_bif1, 4) -> 1177;
var_index(l_gc_bif1, 2) -> 1178;
var_index(l_gc_bif1, 1) -> 1179;
var_index(test_heap, 1) -> 1180;

var_index(Op, No) -> erlang:error({noindex,Op,No}).

var_by_index(0) -> {get_tuple_element, 0};
var_by_index(1) -> {move, 0};
var_by_index(2) -> {get_tuple_element, 1};
var_by_index(3) -> {move, 3};
var_by_index(4) -> {is_tuple_of_arity, 1};
var_by_index(5) -> {l_new_bs_put_integer_imm, 0};
var_by_index(6) -> {move, 1};
var_by_index(7) -> {l_call, 0};
var_by_index(8) -> {move2, 0};
var_by_index(9) -> {test_heap, 0};
var_by_index(10) -> {move, 2};
var_by_index(11) -> {l_put_tuple, 0};
var_by_index(12) -> {move2, 1};
var_by_index(13) -> {move2, 3};
var_by_index(14) -> {l_call_only, 0};
var_by_index(15) -> {get_list, 0};
var_by_index(16) -> {l_fetch, 2};
var_by_index(17) -> {is_nonempty_list, 0};
var_by_index(18) -> {l_allocate, 1};
var_by_index(19) -> {l_is_eq_exact, 0};
var_by_index(20) -> {move, 4};
var_by_index(21) -> {is_nonempty_list_allocate, 1};
var_by_index(22) -> {l_select_val2, 0};
var_by_index(23) -> {l_gc_bif1, 0};
var_by_index(24) -> {l_is_eq_exact_immed, 2};
var_by_index(25) -> {deallocate_return, 1};
var_by_index(26) -> {get_list, 3};
var_by_index(27) -> {l_allocate, 0};
var_by_index(28) -> {is_nonempty_list, 2};
var_by_index(29) -> {l_is_eq_exact_immed, 3};
var_by_index(30) -> {l_is_eq_exact_immed, 0};
var_by_index(31) -> {l_call_ext, 110};
var_by_index(32) -> {is_tuple_of_arity, 0};
var_by_index(33) -> {put_list, 0};
var_by_index(34) -> {l_call_fun, 0};
var_by_index(35) -> {l_bs_add, 0};
var_by_index(36) -> {l_bs_get_integer_small_imm, 0};
var_by_index(37) -> {l_fetch, 0};
var_by_index(38) -> {l_is_eq_exact_immed, 1};
var_by_index(39) -> {call_bif, 8};
var_by_index(40) -> {move_return, 3};
var_by_index(41) -> {l_is_eq_exact_immed, 6};
var_by_index(42) -> {deallocate_return, 0};
var_by_index(43) -> {put_list, 1};
var_by_index(44) -> {return, 0};
var_by_index(45) -> {extract_next_element, 2};
var_by_index(46) -> {is_nil, 2};
var_by_index(47) -> {extract_next_element, 1};
var_by_index(48) -> {l_bs_start_match2, 0};
var_by_index(49) -> {l_is_eq_exact_immed, 5};
var_by_index(50) -> {is_tuple, 0};
var_by_index(51) -> {l_trim, 0};
var_by_index(52) -> {extract_next_element2, 1};
var_by_index(53) -> {l_bs_get_integer_16, 0};
var_by_index(54) -> {deallocate_return, 2};
var_by_index(55) -> {l_call_last, 0};
var_by_index(56) -> {extract_next_element, 4};
var_by_index(57) -> {l_fetch, 1};
var_by_index(58) -> {l_select_val_atoms, 0};
var_by_index(59) -> {put_list, 2};
var_by_index(60) -> {move_return, 5};
var_by_index(61) -> {call_bif, 47};
var_by_index(62) -> {l_new_bs_put_binary_all, 0};
var_by_index(63) -> {is_nil, 1};
var_by_index(64) -> {l_bif2, 0};
var_by_index(65) -> {l_allocate, 2};
var_by_index(66) -> {l_is_eq_exact_immed, 7};
var_by_index(67) -> {move_deallocate_return, 0};
var_by_index(68) -> {move_return, 1};
var_by_index(69) -> {call_bif, 7};
var_by_index(70) -> {move, 5};
var_by_index(71) -> {l_fetch, 18};
var_by_index(72) -> {call_bif, 16};
var_by_index(73) -> {call_bif, 15};
var_by_index(74) -> {call_bif, 14};
var_by_index(75) -> {l_increment, 0};
var_by_index(76) -> {l_bs_init_heap_bin, 0};
var_by_index(77) -> {l_select_tuple_arity, 0};
var_by_index(78) -> {put_list, 3};
var_by_index(79) -> {l_select_tuple_arity2, 0};
var_by_index(80) -> {move_return, 4};
var_by_index(81) -> {l_fetch, 4};
var_by_index(82) -> {l_allocate, 6};
var_by_index(83) -> {l_call_last, 6};
var_by_index(84) -> {init2, 0};
var_by_index(85) -> {call_bif, 9};
var_by_index(86) -> {l_move_call, 36};
var_by_index(87) -> {l_new_bs_put_binary_imm, 1};
var_by_index(88) -> {l_make_fun, 0};
var_by_index(89) -> {is_nonempty_list, 1};
var_by_index(90) -> {l_call_ext, 6};
var_by_index(91) -> {l_call_ext_last, 0};
var_by_index(92) -> {l_is_ge, 0};
var_by_index(93) -> {extract_next_element2, 0};
var_by_index(94) -> {move_return, 48};
var_by_index(95) -> {l_allocate, 3};
var_by_index(96) -> {jump, 0};
var_by_index(97) -> {l_fetch, 10};
var_by_index(98) -> {is_atom, 0};
var_by_index(99) -> {l_bs_get_binary_imm2, 0};
var_by_index(100) -> {extract_next_element2, 2};
var_by_index(101) -> {is_tuple, 2};
var_by_index(102) -> {test_arity, 0};
var_by_index(103) -> {l_move_call_only, 3};
var_by_index(104) -> {l_catch, 5};
var_by_index(105) -> {l_bs_append, 0};
var_by_index(106) -> {l_select_tuple_arity2, 1};
var_by_index(107) -> {extract_next_element, 0};
var_by_index(108) -> {call_bif, 41};
var_by_index(109) -> {l_bs_init_fail, 0};
var_by_index(110) -> {l_is_function2, 0};
var_by_index(111) -> {l_allocate_zero, 3};
var_by_index(112) -> {l_loop_rec, 0};
var_by_index(113) -> {l_select_tuple_arity, 1};
var_by_index(114) -> {is_nil, 0};
var_by_index(115) -> {move, 6};
var_by_index(116) -> {extract_next_element3, 0};
var_by_index(117) -> {remove_message, 0};
var_by_index(118) -> {l_move_call_last, 2};
var_by_index(119) -> {allocate_init, 0};
var_by_index(120) -> {move, 8};
var_by_index(121) -> {l_allocate_zero, 2};
var_by_index(122) -> {l_allocate_zero, 1};
var_by_index(123) -> {move, 11};
var_by_index(124) -> {is_nil, 3};
var_by_index(125) -> {catch_end, 5};
var_by_index(126) -> {is_atom, 1};
var_by_index(127) -> {init, 0};
var_by_index(128) -> {l_bif1, 0};
var_by_index(129) -> {init, 1};
var_by_index(130) -> {l_select_val2, 9};
var_by_index(131) -> {apply_last, 0};
var_by_index(132) -> {l_bs_get_binary_all_reuse, 0};
var_by_index(133) -> {self, 0};
var_by_index(134) -> {deallocate_return, 4};
var_by_index(135) -> {l_fetch, 17};
var_by_index(136) -> {set_tuple_element, 0};
var_by_index(137) -> {l_bs_match_string, 0};
var_by_index(138) -> {l_allocate, 10};
var_by_index(139) -> {l_move_call_only, 1};
var_by_index(140) -> {l_select_val_smallints, 0};
var_by_index(141) -> {l_bs_get_integer_8, 0};
var_by_index(142) -> {move_deallocate_return, 2};
var_by_index(143) -> {is_list, 2};
var_by_index(144) -> {is_atom, 2};
var_by_index(145) -> {l_allocate_zero, 0};
var_by_index(146) -> {send, 0};
var_by_index(147) -> {allocate_heap, 0};
var_by_index(148) -> {l_is_eq_exact_immed, 8};
var_by_index(149) -> {l_trim, 1};
var_by_index(150) -> {move_deallocate_return, 1};
var_by_index(151) -> {init3, 0};
var_by_index(152) -> {deallocate_return, 3};
var_by_index(153) -> {extract_next_element3, 2};
var_by_index(154) -> {call_bif, 36};
var_by_index(155) -> {l_bif2, 6};
var_by_index(156) -> {is_integer, 0};
var_by_index(157) -> {move_return, 0};
var_by_index(158) -> {l_allocate, 4};
var_by_index(159) -> {l_bif2, 4};
var_by_index(160) -> {l_bif2, 1};
var_by_index(161) -> {is_atom, 3};
var_by_index(162) -> {l_call_last, 4};
var_by_index(163) -> {extract_next_element, 6};
var_by_index(164) -> {l_move_call, 1};
var_by_index(165) -> {move_deallocate_return, 3};
var_by_index(166) -> {l_move_call_ext_last, 0};
var_by_index(167) -> {l_bs_get_binary2, 0};
var_by_index(168) -> {l_is_eq_exact_literal, 1};
var_by_index(169) -> {l_call_ext, 24};
var_by_index(170) -> {l_move_call_ext, 13};
var_by_index(171) -> {l_fetch, 13};
var_by_index(172) -> {l_move_call_last, 3};
var_by_index(173) -> {l_new_bs_put_binary_imm, 0};
var_by_index(174) -> {l_minus, 0};
var_by_index(175) -> {l_call_last, 1};
var_by_index(176) -> {l_times, 1};
var_by_index(177) -> {l_bs_init_bits, 0};
var_by_index(178) -> {l_fetch, 14};
var_by_index(179) -> {l_increment, 2};
var_by_index(180) -> {l_times, 0};
var_by_index(181) -> {l_bs_get_integer_32, 0};
var_by_index(182) -> {l_bs_get_binary_all2, 0};
var_by_index(183) -> {deallocate_return, 13};
var_by_index(184) -> {l_move_call_last, 0};
var_by_index(185) -> {l_select_val2, 6};
var_by_index(186) -> {l_is_eq_exact_literal, 5};
var_by_index(187) -> {is_binary, 2};
var_by_index(188) -> {test_heap_1_put_list, 0};
var_by_index(189) -> {l_is_lt, 0};
var_by_index(190) -> {l_plus, 0};
var_by_index(191) -> {l_allocate_zero, 4};
var_by_index(192) -> {l_select_val2, 2};
var_by_index(193) -> {extract_next_element3, 4};
var_by_index(194) -> {l_select_val2, 8};
var_by_index(195) -> {call_bif, 18};
var_by_index(196) -> {is_integer, 5};
var_by_index(197) -> {is_binary, 1};
var_by_index(198) -> {deallocate_return, 5};
var_by_index(199) -> {l_catch, 0};
var_by_index(200) -> {l_trim, 3};
var_by_index(201) -> {is_pid, 0};
var_by_index(202) -> {l_select_val2, 3};
var_by_index(203) -> {is_nonempty_list, 4};
var_by_index(204) -> {l_call_last, 3};
var_by_index(205) -> {is_list, 0};
var_by_index(206) -> {extract_next_element3, 7};
var_by_index(207) -> {l_select_val2, 12};
var_by_index(208) -> {l_int_div, 1};
var_by_index(209) -> {l_fetch, 8};
var_by_index(210) -> {l_catch, 2};
var_by_index(211) -> {catch_end, 2};
var_by_index(212) -> {l_call_ext_last, 1};
var_by_index(213) -> {l_allocate_zero, 5};
var_by_index(214) -> {l_move_call_ext_only, 10};
var_by_index(215) -> {extract_next_element, 5};
var_by_index(216) -> {init, 2};
var_by_index(217) -> {try_end, 4};
var_by_index(218) -> {extract_next_element2, 3};
var_by_index(219) -> {is_tuple, 1};
var_by_index(220) -> {l_is_ne_exact_immed, 2};
var_by_index(221) -> {is_float, 0};
var_by_index(222) -> {try_end, 1};
var_by_index(223) -> {l_move_call, 4};
var_by_index(224) -> {extract_next_element, 3};
var_by_index(225) -> {l_call_ext, 0};
var_by_index(226) -> {is_integer_allocate, 0};
var_by_index(227) -> {l_fetch, 16};
var_by_index(228) -> {int_code_end, 0};
var_by_index(229) -> {move_return, 6};
var_by_index(230) -> {l_move_call_ext, 50};
var_by_index(231) -> {l_call_ext, 12};
var_by_index(232) -> {extract_next_element2, 5};
var_by_index(233) -> {l_catch, 4};
var_by_index(234) -> {is_nil, 6};
var_by_index(235) -> {try_end, 2};
var_by_index(236) -> {deallocate_return, 6};
var_by_index(237) -> {l_is_eq_exact_immed, 19};
var_by_index(238) -> {extract_next_element2, 13};
var_by_index(239) -> {l_move_call, 3};
var_by_index(240) -> {l_move_call_ext_only, 1};
var_by_index(241) -> {is_port, 0};
var_by_index(242) -> {l_move_call_ext, 7};
var_by_index(243) -> {l_is_eq_exact_immed, 21};
var_by_index(244) -> {l_is_eq_exact_immed, 12};
var_by_index(245) -> {l_apply, 0};
var_by_index(246) -> {is_list, 4};
var_by_index(247) -> {l_get, 2};
var_by_index(248) -> {l_select_val2, 5};
var_by_index(249) -> {node, 3};
var_by_index(250) -> {l_is_eq_exact_immed, 27};
var_by_index(251) -> {deallocate_return, 9};
var_by_index(252) -> {extract_next_element2, 10};
var_by_index(253) -> {call_bif, 42};
var_by_index(254) -> {l_select_val2, 13};
var_by_index(255) -> {l_move_call_only, 6};
var_by_index(256) -> {l_call_ext, 80};
var_by_index(257) -> {l_is_eq_exact_literal, 4};
var_by_index(258) -> {l_bs_init_bits_fail, 0};
var_by_index(259) -> {l_move_call_last, 5};
var_by_index(260) -> {l_new_bs_put_binary_all, 1};
var_by_index(261) -> {l_rem, 0};
var_by_index(262) -> {l_move_call_last, 1};
var_by_index(263) -> {move_return, 14};
var_by_index(264) -> {l_plus, 2};
var_by_index(265) -> {l_bs_put_string, 2};
var_by_index(266) -> {l_new_bs_put_integer, 0};
var_by_index(267) -> {move_deallocate_return, 7};
var_by_index(268) -> {wait_timeout, 0};
var_by_index(269) -> {l_is_eq, 0};
var_by_index(270) -> {l_move_call_only, 2};
var_by_index(271) -> {is_integer, 4};
var_by_index(272) -> {l_fetch, 12};
var_by_index(273) -> {l_call_fun, 3};
var_by_index(274) -> {l_move_call_only, 4};
var_by_index(275) -> {l_move_call_only, 10};
var_by_index(276) -> {l_select_val2, 7};
var_by_index(277) -> {l_fetch, 23};
var_by_index(278) -> {is_nonempty_list, 6};
var_by_index(279) -> {l_increment, 3};
var_by_index(280) -> {is_list, 1};
var_by_index(281) -> {bif1_body, 0};
var_by_index(282) -> {is_nonempty_list_allocate, 0};
var_by_index(283) -> {l_bs_get_integer_8, 1};
var_by_index(284) -> {l_gc_bif1, 3};
var_by_index(285) -> {apply, 0};
var_by_index(286) -> {l_is_ne_exact_literal, 0};
var_by_index(287) -> {get_list, 2};
var_by_index(288) -> {l_element, 0};
var_by_index(289) -> {l_fetch, 6};
var_by_index(290) -> {l_is_ne_exact, 0};
var_by_index(291) -> {l_call_last, 2};
var_by_index(292) -> {get_tuple_element, 4};
var_by_index(293) -> {get_tuple_element, 5};
var_by_index(294) -> {call_bif, 12};
var_by_index(295) -> {is_nonempty_list_test_heap, 0};
var_by_index(296) -> {l_move_call, 0};
var_by_index(297) -> {is_integer, 3};
var_by_index(298) -> {call_bif, 21};
var_by_index(299) -> {l_bs_skip_bits_imm2, 1};
var_by_index(300) -> {is_nonempty_list, 3};
var_by_index(301) -> {l_increment, 1};
var_by_index(302) -> {test_arity, 1};
var_by_index(303) -> {put_list, 4};
var_by_index(304) -> {is_tuple_of_arity, 3};
var_by_index(305) -> {move_return, 8};
var_by_index(306) -> {is_integer, 1};
var_by_index(307) -> {l_bs_test_unit_8, 3};
var_by_index(308) -> {l_bs_put_string, 0};
var_by_index(309) -> {l_bs_test_zero_tail2, 3};
var_by_index(310) -> {l_bs_get_binary_all_reuse, 1};
var_by_index(311) -> {l_is_eq_exact_immed, 9};
var_by_index(312) -> {put_list, 5};
var_by_index(313) -> {is_nil, 4};
var_by_index(314) -> {bif2_body, 0};
var_by_index(315) -> {l_fast_element, 2};
var_by_index(316) -> {l_trim, 4};
var_by_index(317) -> {catch_end, 0};
var_by_index(318) -> {get_tuple_element, 3};
var_by_index(319) -> {self, 1};
var_by_index(320) -> {l_move_call_only, 8};
var_by_index(321) -> {move_deallocate_return, 4};
var_by_index(322) -> {l_bs_test_unit_8, 1};
var_by_index(323) -> {timeout, 0};
var_by_index(324) -> {is_binary, 0};
var_by_index(325) -> {l_allocate, 7};
var_by_index(326) -> {move, 13};
var_by_index(327) -> {l_bif2, 2};
var_by_index(328) -> {l_move_call_ext, 8};
var_by_index(329) -> {l_trim, 2};
var_by_index(330) -> {l_is_ne_exact_immed, 4};
var_by_index(331) -> {call_bif, 29};
var_by_index(332) -> {call_bif, 46};
var_by_index(333) -> {l_catch, 6};
var_by_index(334) -> {recv_mark, 0};
var_by_index(335) -> {l_recv_set, 0};
var_by_index(336) -> {self, 2};
var_by_index(337) -> {catch_end, 6};
var_by_index(338) -> {l_is_eq_exact_literal, 0};
var_by_index(339) -> {l_call_ext_only, 5};
var_by_index(340) -> {l_is_eq_exact_immed, 11};
var_by_index(341) -> {extract_next_element, 10};
var_by_index(342) -> {l_fetch, 20};
var_by_index(343) -> {l_is_ne_exact_immed, 10};
var_by_index(344) -> {is_nonempty_list, 5};
var_by_index(345) -> {l_is_eq_exact_immed, 35};
var_by_index(346) -> {l_select_val2, 4};
var_by_index(347) -> {wait, 0};
var_by_index(348) -> {l_select_val_smallints, 1};
var_by_index(349) -> {move, 9};
var_by_index(350) -> {is_tuple, 4};
var_by_index(351) -> {move_return, 2};
var_by_index(352) -> {l_move_call_only, 5};
var_by_index(353) -> {l_allocate, 5};
var_by_index(354) -> {call_bif, 11};
var_by_index(355) -> {extract_next_element3, 3};
var_by_index(356) -> {l_move_call, 12};
var_by_index(357) -> {l_fast_element, 1};
var_by_index(358) -> {move_jump, 3};
var_by_index(359) -> {extract_next_element, 9};
var_by_index(360) -> {extract_next_element2, 4};
var_by_index(361) -> {l_move_call, 2};
var_by_index(362) -> {l_move_call_only, 0};
var_by_index(363) -> {is_nonempty_list, 8};
var_by_index(364) -> {l_catch, 3};
var_by_index(365) -> {call_bif, 28};
var_by_index(366) -> {l_call_ext, 28};
var_by_index(367) -> {is_integer, 2};
var_by_index(368) -> {is_nonempty_list, 7};
var_by_index(369) -> {l_is_ne_exact_immed, 3};
var_by_index(370) -> {try_end, 5};
var_by_index(371) -> {is_tuple, 3};
var_by_index(372) -> {extract_next_element2, 12};
var_by_index(373) -> {l_call_last, 11};
var_by_index(374) -> {put_list, 14};
var_by_index(375) -> {move_return, 26};
var_by_index(376) -> {call_bif, 44};
var_by_index(377) -> {extract_next_element3, 1};
var_by_index(378) -> {l_move_call, 5};
var_by_index(379) -> {l_call_ext, 101};
var_by_index(380) -> {call_bif, 17};
var_by_index(381) -> {move_jump, 0};
var_by_index(382) -> {init, 3};
var_by_index(383) -> {l_call_ext_only, 2};
var_by_index(384) -> {put_list, 6};
var_by_index(385) -> {get_tuple_element, 2};
var_by_index(386) -> {l_catch, 1};
var_by_index(387) -> {l_bs_test_unit_8, 0};
var_by_index(388) -> {loop_rec_end, 0};
var_by_index(389) -> {l_band, 0};
var_by_index(390) -> {l_move_call_ext, 9};
var_by_index(391) -> {is_nil, 5};
var_by_index(392) -> {self, 3};
var_by_index(393) -> {test_heap_1_put_list, 1};
var_by_index(394) -> {bif2_body, 1};
var_by_index(395) -> {l_bs_skip_bits_imm2, 0};
var_by_index(396) -> {l_is_eq_exact_immed, 16};
var_by_index(397) -> {l_bs_restore2, 0};
var_by_index(398) -> {is_nonempty_list, 9};
var_by_index(399) -> {l_move_call_ext, 6};
var_by_index(400) -> {call_bif, 27};
var_by_index(401) -> {l_move_call, 6};
var_by_index(402) -> {catch_end, 1};
var_by_index(403) -> {l_call_ext_last, 2};
var_by_index(404) -> {get_list, 6};
var_by_index(405) -> {move, 7};
var_by_index(406) -> {l_allocate_zero, 6};
var_by_index(407) -> {extract_next_element, 8};
var_by_index(408) -> {l_call_ext, 37};
var_by_index(409) -> {l_move_call_ext_last, 2};
var_by_index(410) -> {extract_next_element, 12};
var_by_index(411) -> {l_call_last, 7};
var_by_index(412) -> {l_is_eq_exact_immed, 26};
var_by_index(413) -> {call_bif, 25};
var_by_index(414) -> {move_return, 22};
var_by_index(415) -> {call_bif, 37};
var_by_index(416) -> {is_nil, 11};
var_by_index(417) -> {init, 4};
var_by_index(418) -> {put_list, 9};
var_by_index(419) -> {l_call_ext, 4};
var_by_index(420) -> {l_is_ne_exact_immed, 0};
var_by_index(421) -> {l_jump_on_val, 1};
var_by_index(422) -> {l_is_ne_exact_immed, 11};
var_by_index(423) -> {l_call_last, 5};
var_by_index(424) -> {l_call_ext_only, 4};
var_by_index(425) -> {extract_next_element, 11};
var_by_index(426) -> {l_call_fun_last, 0};
var_by_index(427) -> {l_call_ext, 25};
var_by_index(428) -> {extract_next_element, 15};
var_by_index(429) -> {l_move_call_ext, 14};
var_by_index(430) -> {l_move_call_ext, 5};
var_by_index(431) -> {is_nil, 7};
var_by_index(432) -> {l_bs_save2, 0};
var_by_index(433) -> {try_end, 0};
var_by_index(434) -> {l_int_div, 0};
var_by_index(435) -> {bif1_body, 3};
var_by_index(436) -> {l_bor, 0};
var_by_index(437) -> {l_is_eq_exact_immed, 18};
var_by_index(438) -> {extract_next_element, 7};
var_by_index(439) -> {l_bsl, 1};
var_by_index(440) -> {l_move_call_ext_only, 3};
var_by_index(441) -> {deallocate_return, 7};
var_by_index(442) -> {l_jump_on_val, 0};
var_by_index(443) -> {get_list, 8};
var_by_index(444) -> {catch_end, 3};
var_by_index(445) -> {is_nil, 8};
var_by_index(446) -> {put_list, 10};
var_by_index(447) -> {get_list, 5};
var_by_index(448) -> {is_tuple, 9};
var_by_index(449) -> {l_apply_last, 0};
var_by_index(450) -> {l_call_ext, 43};
var_by_index(451) -> {call_bif, 30};
var_by_index(452) -> {l_bsr, 0};
var_by_index(453) -> {l_move_call_ext, 15};
var_by_index(454) -> {l_bs_skip_bits2, 0};
var_by_index(455) -> {l_call_ext, 71};
var_by_index(456) -> {l_is_ne_exact_immed, 1};
var_by_index(457) -> {is_list, 3};
var_by_index(458) -> {init, 5};
var_by_index(459) -> {call_bif, 40};
var_by_index(460) -> {l_is_eq_exact_immed, 22};
var_by_index(461) -> {extract_next_element, 13};
var_by_index(462) -> {l_is_ne_exact_immed, 8};
var_by_index(463) -> {l_get, 1};
var_by_index(464) -> {is_nonempty_list, 10};
var_by_index(465) -> {l_move_call, 7};
var_by_index(466) -> {l_fast_element, 0};
var_by_index(467) -> {l_move_call_ext, 21};
var_by_index(468) -> {is_bitstr, 0};
var_by_index(469) -> {l_select_val2, 14};
var_by_index(470) -> {call_bif, 31};
var_by_index(471) -> {l_call_ext, 96};
var_by_index(472) -> {move_return, 17};
var_by_index(473) -> {l_is_eq_exact_immed, 15};
var_by_index(474) -> {l_call_ext, 51};
var_by_index(475) -> {call_bif, 10};
var_by_index(476) -> {move_jump, 13};
var_by_index(477) -> {l_trim, 5};
var_by_index(478) -> {is_function, 0};
var_by_index(479) -> {l_call_ext, 2};
var_by_index(480) -> {call_bif, 26};
var_by_index(481) -> {extract_next_element, 16};
var_by_index(482) -> {l_move_call, 8};
var_by_index(483) -> {extract_next_element2, 7};
var_by_index(484) -> {l_allocate, 8};
var_by_index(485) -> {bif1_body, 7};
var_by_index(486) -> {l_call_ext_last, 6};
var_by_index(487) -> {l_fetch, 7};
var_by_index(488) -> {deallocate_return, 10};
var_by_index(489) -> {call_bif, 38};
var_by_index(490) -> {init, 9};
var_by_index(491) -> {move_return, 10};
var_by_index(492) -> {is_nil, 9};
var_by_index(493) -> {move_deallocate_return, 6};
var_by_index(494) -> {l_fdiv, 0};
var_by_index(495) -> {l_band, 2};
var_by_index(496) -> {l_bs_test_zero_tail2, 5};
var_by_index(497) -> {call_bif, 22};
var_by_index(498) -> {l_fcheckerror, 0};
var_by_index(499) -> {fclearerror, 0};
var_by_index(500) -> {allocate_heap_zero, 0};
var_by_index(501) -> {fconv, 0};
var_by_index(502) -> {fmove_1, 0};
var_by_index(503) -> {l_select_val_atoms, 1};
var_by_index(504) -> {l_move_call_ext_last, 6};
var_by_index(505) -> {l_bsl, 0};
var_by_index(506) -> {fmove_2, 1};
var_by_index(507) -> {l_increment, 6};
var_by_index(508) -> {l_call_ext, 41};
var_by_index(509) -> {l_move_call_ext, 2};
var_by_index(510) -> {is_tuple, 7};
var_by_index(511) -> {l_call_ext, 20};
var_by_index(512) -> {init, 6};
var_by_index(513) -> {l_move_call_last, 4};
var_by_index(514) -> {l_call_ext, 38};
var_by_index(515) -> {extract_next_element3, 6};
var_by_index(516) -> {move_return, 21};
var_by_index(517) -> {l_is_ne, 0};
var_by_index(518) -> {extract_next_element2, 6};
var_by_index(519) -> {l_call_ext, 66};
var_by_index(520) -> {is_nonempty_list, 24};
var_by_index(521) -> {move_jump, 2};
var_by_index(522) -> {l_allocate_zero, 7};
var_by_index(523) -> {l_call_ext, 23};
var_by_index(524) -> {l_call_ext, 11};
var_by_index(525) -> {l_catch, 8};
var_by_index(526) -> {is_pid, 1};
var_by_index(527) -> {is_reference, 0};
var_by_index(528) -> {call_bif, 32};
var_by_index(529) -> {is_nonempty_list, 12};
var_by_index(530) -> {l_is_eq_exact_immed, 20};
var_by_index(531) -> {l_bs_test_zero_tail2, 1};
var_by_index(532) -> {l_move_call_ext, 4};
var_by_index(533) -> {extract_next_element2, 8};
var_by_index(534) -> {l_call_ext, 16};
var_by_index(535) -> {l_rem, 2};
var_by_index(536) -> {l_move_call_ext, 38};
var_by_index(537) -> {l_move_call_ext, 3};
var_by_index(538) -> {catch_end, 8};
var_by_index(539) -> {self, 6};
var_by_index(540) -> {l_is_eq_exact_immed, 13};
var_by_index(541) -> {l_call_ext, 105};
var_by_index(542) -> {l_move_call_ext, 10};
var_by_index(543) -> {l_is_eq_exact_immed, 30};
var_by_index(544) -> {l_call_ext, 7};
var_by_index(545) -> {l_call_last, 8};
var_by_index(546) -> {test_arity, 3};
var_by_index(547) -> {is_boolean, 0};
var_by_index(548) -> {l_allocate_zero, 10};
var_by_index(549) -> {l_call_ext, 26};
var_by_index(550) -> {l_call_ext, 47};
var_by_index(551) -> {is_list, 6};
var_by_index(552) -> {is_nonempty_list, 15};
var_by_index(553) -> {l_is_eq_exact_literal, 7};
var_by_index(554) -> {l_move_call_ext_only, 6};
var_by_index(555) -> {self, 4};
var_by_index(556) -> {l_call_ext, 34};
var_by_index(557) -> {l_is_eq_exact_immed, 24};
var_by_index(558) -> {l_call_ext, 17};
var_by_index(559) -> {is_function, 1};
var_by_index(560) -> {l_move_call, 15};
var_by_index(561) -> {l_fetch, 19};
var_by_index(562) -> {move_return, 28};
var_by_index(563) -> {node, 4};
var_by_index(564) -> {l_call_ext, 45};
var_by_index(565) -> {l_bor, 1};
var_by_index(566) -> {move_return, 7};
var_by_index(567) -> {extract_next_element3, 10};
var_by_index(568) -> {l_call_ext, 82};
var_by_index(569) -> {is_nonempty_list, 11};
var_by_index(570) -> {is_atom, 4};
var_by_index(571) -> {l_call_ext, 8};
var_by_index(572) -> {l_apply_fun, 0};
var_by_index(573) -> {l_call_ext, 109};
var_by_index(574) -> {extract_next_element2, 14};
var_by_index(575) -> {move_return, 16};
var_by_index(576) -> {is_nil, 15};
var_by_index(577) -> {l_call_ext_last, 3};
var_by_index(578) -> {l_allocate_zero, 8};
var_by_index(579) -> {try_end, 3};
var_by_index(580) -> {l_fetch, 15};
var_by_index(581) -> {l_fast_element, 3};
var_by_index(582) -> {l_allocate_zero, 9};
var_by_index(583) -> {is_nil, 12};
var_by_index(584) -> {l_select_val2, 10};
var_by_index(585) -> {move_deallocate_return, 5};
var_by_index(586) -> {extract_next_element, 14};
var_by_index(587) -> {call_bif, 6};
var_by_index(588) -> {l_call_ext, 64};
var_by_index(589) -> {extract_next_element3, 5};
var_by_index(590) -> {l_move_call_ext, 22};
var_by_index(591) -> {call_bif, 33};
var_by_index(592) -> {l_call_ext, 74};
var_by_index(593) -> {is_nil, 13};
var_by_index(594) -> {l_move_call_ext_only, 0};
var_by_index(595) -> {init, 7};
var_by_index(596) -> {is_tuple, 5};
var_by_index(597) -> {l_call_ext, 31};
var_by_index(598) -> {try_end, 6};
var_by_index(599) -> {is_atom, 6};
var_by_index(600) -> {set_tuple_element, 1};
var_by_index(601) -> {put_list, 7};
var_by_index(602) -> {l_call_ext_last, 4};
var_by_index(603) -> {l_increment, 5};
var_by_index(604) -> {is_atom, 5};
var_by_index(605) -> {l_is_eq_exact_immed, 14};
var_by_index(606) -> {l_call_ext, 55};
var_by_index(607) -> {call_bif, 35};
var_by_index(608) -> {call_bif, 23};
var_by_index(609) -> {l_move_call, 10};
var_by_index(610) -> {move_return, 36};
var_by_index(611) -> {l_move_call_only, 7};
var_by_index(612) -> {l_call_last, 10};
var_by_index(613) -> {bif1_body, 5};
var_by_index(614) -> {init, 8};
var_by_index(615) -> {call_bif, 20};
var_by_index(616) -> {l_is_eq_exact_immed, 36};
var_by_index(617) -> {l_call_ext, 42};
var_by_index(618) -> {extract_next_element, 21};
var_by_index(619) -> {l_int_bnot, 0};
var_by_index(620) -> {deallocate_return, 8};
var_by_index(621) -> {call_bif, 34};
var_by_index(622) -> {l_put_tuple, 6};
var_by_index(623) -> {node, 0};
var_by_index(624) -> {l_is_eq_exact_immed, 25};
var_by_index(625) -> {l_move_call, 13};
var_by_index(626) -> {bif2_body, 2};
var_by_index(627) -> {init, 10};
var_by_index(628) -> {extract_next_element2, 17};
var_by_index(629) -> {l_new_bs_put_integer, 2};
var_by_index(630) -> {l_move_call, 16};
var_by_index(631) -> {l_move_call_ext, 36};
var_by_index(632) -> {is_tuple, 8};
var_by_index(633) -> {extract_next_element2, 11};
var_by_index(634) -> {is_nil, 28};
var_by_index(635) -> {put_list, 12};
var_by_index(636) -> {get_list, 4};
var_by_index(637) -> {l_bs_get_integer_16, 1};
var_by_index(638) -> {catch_end, 4};
var_by_index(639) -> {extract_next_element, 17};
var_by_index(640) -> {try_end, 7};
var_by_index(641) -> {call_bif, 43};
var_by_index(642) -> {is_tuple, 6};
var_by_index(643) -> {l_call_ext, 44};
var_by_index(644) -> {l_call_ext, 3};
var_by_index(645) -> {l_trim, 8};
var_by_index(646) -> {put_list, 11};
var_by_index(647) -> {l_is_eq_exact_literal, 6};
var_by_index(648) -> {l_move_call_ext, 44};
var_by_index(649) -> {l_get, 6};
var_by_index(650) -> {call_bif, 5};
var_by_index(651) -> {l_call_ext, 91};
var_by_index(652) -> {l_is_ne_exact_immed, 5};
var_by_index(653) -> {is_nil, 10};
var_by_index(654) -> {l_move_call_ext, 37};
var_by_index(655) -> {node, 2};
var_by_index(656) -> {is_nil, 17};
var_by_index(657) -> {is_nonempty_list, 33};
var_by_index(658) -> {l_is_eq_exact_immed, 28};
var_by_index(659) -> {l_call_ext, 61};
var_by_index(660) -> {l_trim, 6};
var_by_index(661) -> {is_nil, 14};
var_by_index(662) -> {move_jump, 7};
var_by_index(663) -> {move_jump, 6};
var_by_index(664) -> {move_jump, 1};
var_by_index(665) -> {move_return, 19};
var_by_index(666) -> {bs_context_to_binary, 0};
var_by_index(667) -> {badmatch, 0};
var_by_index(668) -> {is_nonempty_list, 35};
var_by_index(669) -> {is_nonempty_list, 19};
var_by_index(670) -> {l_move_call, 25};
var_by_index(671) -> {bif1_body, 6};
var_by_index(672) -> {bif1_body, 1};
var_by_index(673) -> {bif2_body, 3};
var_by_index(674) -> {is_float, 1};
var_by_index(675) -> {node, 1};
var_by_index(676) -> {l_move_call_ext_last, 4};
var_by_index(677) -> {call_bif, 13};
var_by_index(678) -> {l_call_ext, 92};
var_by_index(679) -> {l_call_ext, 78};
var_by_index(680) -> {l_call_ext, 36};
var_by_index(681) -> {move_jump, 10};
var_by_index(682) -> {move_return, 9};
var_by_index(683) -> {l_bs_test_unit_8, 2};
var_by_index(684) -> {fconv, 2};
var_by_index(685) -> {deallocate_return, 11};
var_by_index(686) -> {l_call_ext_only, 0};
var_by_index(687) -> {move, 10};
var_by_index(688) -> {l_move_call_ext, 28};
var_by_index(689) -> {l_call_ext, 40};
var_by_index(690) -> {l_move_call, 17};
var_by_index(691) -> {l_move_call_ext, 1};
var_by_index(692) -> {extract_next_element, 24};
var_by_index(693) -> {l_is_ne_exact_immed, 6};
var_by_index(694) -> {l_trim, 7};
var_by_index(695) -> {l_call_fun, 4};
var_by_index(696) -> {l_apply_fun_only, 0};
var_by_index(697) -> {l_move_call_ext, 26};
var_by_index(698) -> {l_move_call_ext, 25};
var_by_index(699) -> {l_bs_skip_bits_all2, 1};
var_by_index(700) -> {l_call_ext, 98};
var_by_index(701) -> {l_call_ext, 14};
var_by_index(702) -> {move_return, 11};
var_by_index(703) -> {bs_context_to_binary, 9};
var_by_index(704) -> {is_nonempty_list, 14};
var_by_index(705) -> {l_move_call_ext_only, 7};
var_by_index(706) -> {bif1_body, 4};
var_by_index(707) -> {l_move_call_ext, 46};
var_by_index(708) -> {test_heap_1_put_list, 4};
var_by_index(709) -> {self, 5};
var_by_index(710) -> {l_call_ext, 88};
var_by_index(711) -> {l_call_ext, 86};
var_by_index(712) -> {extract_next_element, 19};
var_by_index(713) -> {l_fadd, 0};
var_by_index(714) -> {extract_next_element2, 16};
var_by_index(715) -> {move_jump, 12};
var_by_index(716) -> {move_return, 45};
var_by_index(717) -> {move_return, 29};
var_by_index(718) -> {move_return, 13};
var_by_index(719) -> {move_deallocate_return, 8};
var_by_index(720) -> {is_bigint, 0};
var_by_index(721) -> {fmove_2, 0};
var_by_index(722) -> {fmove_1, 1};
var_by_index(723) -> {l_move_call_last, 7};
var_by_index(724) -> {l_is_ne_exact_immed, 9};
var_by_index(725) -> {l_fast_element, 5};
var_by_index(726) -> {is_nonempty_list, 13};
var_by_index(727) -> {l_is_eq_exact_literal, 3};
var_by_index(728) -> {test_heap_1_put_list, 3};
var_by_index(729) -> {l_call_ext, 84};
var_by_index(730) -> {l_call_ext, 75};
var_by_index(731) -> {l_call_ext, 62};
var_by_index(732) -> {l_call_ext, 58};
var_by_index(733) -> {l_call_ext, 48};
var_by_index(734) -> {l_call_ext, 35};
var_by_index(735) -> {l_call_ext, 10};
var_by_index(736) -> {extract_next_element2, 9};
var_by_index(737) -> {is_nil, 20};
var_by_index(738) -> {l_bs_put_string, 1};
var_by_index(739) -> {is_list, 5};
var_by_index(740) -> {is_nonempty_list, 22};
var_by_index(741) -> {get_list, 10};
var_by_index(742) -> {l_move_call, 9};
var_by_index(743) -> {l_move_call_ext, 24};
var_by_index(744) -> {l_move_call_ext, 16};
var_by_index(745) -> {l_is_eq_exact_immed, 29};
var_by_index(746) -> {l_call_ext, 77};
var_by_index(747) -> {l_select_val_atoms, 2};
var_by_index(748) -> {is_nonempty_list, 21};
var_by_index(749) -> {is_binary, 3};
var_by_index(750) -> {l_move_call_ext_only, 4};
var_by_index(751) -> {l_move_call_ext, 17};
var_by_index(752) -> {init, 11};
var_by_index(753) -> {l_get, 3};
var_by_index(754) -> {l_bs_test_zero_tail2, 2};
var_by_index(755) -> {l_bs_get_integer, 0};
var_by_index(756) -> {func_info, 0};
var_by_index(757) -> {extract_next_element, 22};
var_by_index(758) -> {l_select_val_atoms, 3};
var_by_index(759) -> {l_trim, 10};
var_by_index(760) -> {is_nil, 21};
var_by_index(761) -> {l_move_call_ext_last, 5};
var_by_index(762) -> {l_fsub, 0};
var_by_index(763) -> {l_move_call_ext, 35};
var_by_index(764) -> {l_move_call_ext, 31};
var_by_index(765) -> {l_move_call_ext, 12};
var_by_index(766) -> {init, 15};
var_by_index(767) -> {l_wait_timeout, 5};
var_by_index(768) -> {l_call_last, 9};
var_by_index(769) -> {l_is_eq_exact_immed, 31};
var_by_index(770) -> {l_call_ext, 104};
var_by_index(771) -> {l_call_ext, 65};
var_by_index(772) -> {l_call_ext, 59};
var_by_index(773) -> {l_call_ext, 5};
var_by_index(774) -> {l_new_bs_put_integer, 1};
var_by_index(775) -> {move_return, 34};
var_by_index(776) -> {move_return, 12};
var_by_index(777) -> {l_trim, 11};
var_by_index(778) -> {l_move_call_only, 9};
var_by_index(779) -> {l_move_call_ext_last, 3};
var_by_index(780) -> {l_move_call_ext_last, 1};
var_by_index(781) -> {is_nonempty_list, 29};
var_by_index(782) -> {is_nonempty_list, 23};
var_by_index(783) -> {is_nonempty_list, 17};
var_by_index(784) -> {l_call_fun, 2};
var_by_index(785) -> {l_apply_only, 0};
var_by_index(786) -> {l_fetch, 21};
var_by_index(787) -> {l_move_call_ext, 47};
var_by_index(788) -> {l_move_call_ext, 19};
var_by_index(789) -> {l_bs_test_zero_tail2, 0};
var_by_index(790) -> {call_bif, 45};
var_by_index(791) -> {bs_init_writable, 0};
var_by_index(792) -> {l_call_ext, 39};
var_by_index(793) -> {extract_next_element, 20};
var_by_index(794) -> {move_jump, 11};
var_by_index(795) -> {move_jump, 4};
var_by_index(796) -> {move_return, 47};
var_by_index(797) -> {move_return, 46};
var_by_index(798) -> {move_return, 39};
var_by_index(799) -> {move_return, 33};
var_by_index(800) -> {move_return, 20};
var_by_index(801) -> {is_nil, 27};
var_by_index(802) -> {is_nil, 22};
var_by_index(803) -> {l_bs_put_string, 5};
var_by_index(804) -> {l_bs_put_string, 3};
var_by_index(805) -> {put_list, 13};
var_by_index(806) -> {is_nonempty_list, 18};
var_by_index(807) -> {l_increment, 7};
var_by_index(808) -> {l_times, 2};
var_by_index(809) -> {l_bs_get_integer_imm, 0};
var_by_index(810) -> {l_move_call, 11};
var_by_index(811) -> {l_get, 5};
var_by_index(812) -> {call_bif, 24};
var_by_index(813) -> {call_bif, 3};
var_by_index(814) -> {l_is_eq_exact_immed, 34};
var_by_index(815) -> {l_move_call_last, 6};
var_by_index(816) -> {l_call_ext, 9};
var_by_index(817) -> {l_is_ne_exact_immed, 7};
var_by_index(818) -> {move_jump, 5};
var_by_index(819) -> {move_return, 40};
var_by_index(820) -> {l_trim, 9};
var_by_index(821) -> {bs_context_to_binary, 2};
var_by_index(822) -> {is_nonempty_list, 30};
var_by_index(823) -> {is_nonempty_list, 27};
var_by_index(824) -> {l_make_export, 0};
var_by_index(825) -> {l_select_val2, 11};
var_by_index(826) -> {is_number, 0};
var_by_index(827) -> {move_deallocate_return, 10};
var_by_index(828) -> {l_move_call, 28};
var_by_index(829) -> {l_move_call, 14};
var_by_index(830) -> {l_move_call_ext_only, 2};
var_by_index(831) -> {init, 14};
var_by_index(832) -> {init, 13};
var_by_index(833) -> {init, 12};
var_by_index(834) -> {l_wait_timeout, 3};
var_by_index(835) -> {l_wait_timeout, 2};
var_by_index(836) -> {l_wait_timeout, 1};
var_by_index(837) -> {l_bs_skip_bits_all2, 0};
var_by_index(838) -> {l_bs_test_zero_tail2, 6};
var_by_index(839) -> {call_bif, 39};
var_by_index(840) -> {call_bif, 19};
var_by_index(841) -> {call_bif, 4};
var_by_index(842) -> {call_bif, 2};
var_by_index(843) -> {call_bif, 1};
var_by_index(844) -> {call_bif, 0};
var_by_index(845) -> {l_int_div, 2};
var_by_index(846) -> {l_bs_put_utf16, 0};
var_by_index(847) -> {l_bs_get_utf16, 2};
var_by_index(848) -> {l_bs_get_utf16, 1};
var_by_index(849) -> {l_bs_get_utf16, 0};
var_by_index(850) -> {l_allocate, 9};
var_by_index(851) -> {l_put_tuple, 7};
var_by_index(852) -> {l_put_tuple, 5};
var_by_index(853) -> {l_put_tuple, 4};
var_by_index(854) -> {l_put_tuple, 3};
var_by_index(855) -> {l_put_tuple, 2};
var_by_index(856) -> {l_put_tuple, 1};
var_by_index(857) -> {l_is_eq_exact_immed, 33};
var_by_index(858) -> {l_is_eq_exact_immed, 32};
var_by_index(859) -> {l_is_eq_exact_immed, 23};
var_by_index(860) -> {l_is_eq_exact_immed, 17};
var_by_index(861) -> {l_is_eq_exact_immed, 10};
var_by_index(862) -> {l_is_eq_exact_immed, 4};
var_by_index(863) -> {l_call_ext, 108};
var_by_index(864) -> {l_call_ext, 107};
var_by_index(865) -> {l_call_ext, 106};
var_by_index(866) -> {l_call_ext, 103};
var_by_index(867) -> {l_call_ext, 102};
var_by_index(868) -> {l_call_ext, 100};
var_by_index(869) -> {l_call_ext, 99};
var_by_index(870) -> {l_call_ext, 97};
var_by_index(871) -> {l_call_ext, 95};
var_by_index(872) -> {l_call_ext, 94};
var_by_index(873) -> {l_call_ext, 93};
var_by_index(874) -> {l_call_ext, 90};
var_by_index(875) -> {l_call_ext, 89};
var_by_index(876) -> {l_call_ext, 87};
var_by_index(877) -> {l_call_ext, 85};
var_by_index(878) -> {l_call_ext, 83};
var_by_index(879) -> {l_call_ext, 81};
var_by_index(880) -> {l_call_ext, 79};
var_by_index(881) -> {l_call_ext, 76};
var_by_index(882) -> {l_call_ext, 73};
var_by_index(883) -> {l_call_ext, 72};
var_by_index(884) -> {l_call_ext, 70};
var_by_index(885) -> {l_call_ext, 69};
var_by_index(886) -> {l_call_ext, 68};
var_by_index(887) -> {l_call_ext, 67};
var_by_index(888) -> {l_call_ext, 63};
var_by_index(889) -> {l_call_ext, 60};
var_by_index(890) -> {l_call_ext, 57};
var_by_index(891) -> {l_call_ext, 56};
var_by_index(892) -> {l_call_ext, 54};
var_by_index(893) -> {l_call_ext, 53};
var_by_index(894) -> {l_call_ext, 52};
var_by_index(895) -> {l_call_ext, 50};
var_by_index(896) -> {l_call_ext, 49};
var_by_index(897) -> {l_call_ext, 46};
var_by_index(898) -> {l_call_ext, 33};
var_by_index(899) -> {l_call_ext, 32};
var_by_index(900) -> {l_call_ext, 30};
var_by_index(901) -> {l_call_ext, 29};
var_by_index(902) -> {l_call_ext, 27};
var_by_index(903) -> {l_call_ext, 22};
var_by_index(904) -> {l_call_ext, 21};
var_by_index(905) -> {l_call_ext, 19};
var_by_index(906) -> {l_call_ext, 18};
var_by_index(907) -> {l_call_ext, 15};
var_by_index(908) -> {l_call_ext, 13};
var_by_index(909) -> {l_call_ext, 1};
var_by_index(910) -> {allocate_init, 1};
var_by_index(911) -> {extract_next_element, 23};
var_by_index(912) -> {extract_next_element, 18};
var_by_index(913) -> {get_tuple_element, 11};
var_by_index(914) -> {get_tuple_element, 10};
var_by_index(915) -> {get_tuple_element, 9};
var_by_index(916) -> {get_tuple_element, 8};
var_by_index(917) -> {get_tuple_element, 7};
var_by_index(918) -> {get_tuple_element, 6};
var_by_index(919) -> {set_tuple_element, 2};
var_by_index(920) -> {l_call_fun_last, 1};
var_by_index(921) -> {l_fast_element, 4};
var_by_index(922) -> {l_bs_test_unit, 0};
var_by_index(923) -> {extract_next_element3, 9};
var_by_index(924) -> {extract_next_element3, 8};
var_by_index(925) -> {extract_next_element2, 15};
var_by_index(926) -> {l_bs_start_match2, 4};
var_by_index(927) -> {l_bs_start_match2, 3};
var_by_index(928) -> {l_bs_start_match2, 2};
var_by_index(929) -> {l_bs_start_match2, 1};
var_by_index(930) -> {l_bs_get_integer_32, 3};
var_by_index(931) -> {l_bs_get_integer_32, 2};
var_by_index(932) -> {l_bs_get_integer_32, 1};
var_by_index(933) -> {l_bor, 2};
var_by_index(934) -> {l_bsr, 1};
var_by_index(935) -> {l_bs_get_binary_imm2, 2};
var_by_index(936) -> {l_bs_get_binary_imm2, 1};
var_by_index(937) -> {l_bs_test_tail_imm2, 0};
var_by_index(938) -> {l_bxor, 0};
var_by_index(939) -> {l_bs_get_float2, 0};
var_by_index(940) -> {move_jump, 9};
var_by_index(941) -> {move_jump, 8};
var_by_index(942) -> {allocate_heap, 1};
var_by_index(943) -> {move_return, 44};
var_by_index(944) -> {move_return, 43};
var_by_index(945) -> {move_return, 42};
var_by_index(946) -> {move_return, 41};
var_by_index(947) -> {move_return, 38};
var_by_index(948) -> {move_return, 37};
var_by_index(949) -> {move_return, 35};
var_by_index(950) -> {move_return, 32};
var_by_index(951) -> {move_return, 31};
var_by_index(952) -> {move_return, 30};
var_by_index(953) -> {move_return, 27};
var_by_index(954) -> {move_return, 25};
var_by_index(955) -> {move_return, 24};
var_by_index(956) -> {move_return, 23};
var_by_index(957) -> {move_return, 18};
var_by_index(958) -> {move_return, 15};
var_by_index(959) -> {l_new_bs_put_integer_imm, 2};
var_by_index(960) -> {l_new_bs_put_integer_imm, 1};
var_by_index(961) -> {l_bs_get_integer_small_imm, 1};
var_by_index(962) -> {l_rem, 1};
var_by_index(963) -> {is_nil, 26};
var_by_index(964) -> {is_nil, 25};
var_by_index(965) -> {is_nil, 24};
var_by_index(966) -> {is_nil, 23};
var_by_index(967) -> {is_nil, 19};
var_by_index(968) -> {is_nil, 18};
var_by_index(969) -> {is_nil, 16};
var_by_index(970) -> {l_bsl, 2};
var_by_index(971) -> {l_fmul, 0};
var_by_index(972) -> {is_tuple_of_arity, 4};
var_by_index(973) -> {is_tuple_of_arity, 2};
var_by_index(974) -> {test_arity, 4};
var_by_index(975) -> {test_arity, 2};
var_by_index(976) -> {bs_context_to_binary, 8};
var_by_index(977) -> {bs_context_to_binary, 7};
var_by_index(978) -> {bs_context_to_binary, 6};
var_by_index(979) -> {bs_context_to_binary, 5};
var_by_index(980) -> {bs_context_to_binary, 4};
var_by_index(981) -> {bs_context_to_binary, 3};
var_by_index(982) -> {bs_context_to_binary, 1};
var_by_index(983) -> {l_new_bs_put_binary, 0};
var_by_index(984) -> {badmatch, 17};
var_by_index(985) -> {badmatch, 16};
var_by_index(986) -> {badmatch, 15};
var_by_index(987) -> {badmatch, 14};
var_by_index(988) -> {badmatch, 13};
var_by_index(989) -> {badmatch, 12};
var_by_index(990) -> {badmatch, 11};
var_by_index(991) -> {badmatch, 10};
var_by_index(992) -> {badmatch, 9};
var_by_index(993) -> {badmatch, 8};
var_by_index(994) -> {badmatch, 7};
var_by_index(995) -> {badmatch, 6};
var_by_index(996) -> {badmatch, 5};
var_by_index(997) -> {badmatch, 4};
var_by_index(998) -> {badmatch, 3};
var_by_index(999) -> {badmatch, 2};
var_by_index(1000) -> {badmatch, 1};
var_by_index(1001) -> {l_bs_test_unit_8, 4};
var_by_index(1002) -> {l_bs_get_utf8, 1};
var_by_index(1003) -> {l_bs_get_utf8, 0};
var_by_index(1004) -> {l_bs_put_utf8, 0};
var_by_index(1005) -> {l_bs_match_string, 2};
var_by_index(1006) -> {l_bs_match_string, 1};
var_by_index(1007) -> {l_bs_put_string, 4};
var_by_index(1008) -> {fconv, 1};
var_by_index(1009) -> {l_m_div, 0};
var_by_index(1010) -> {raise, 1};
var_by_index(1011) -> {raise, 0};
var_by_index(1012) -> {is_integer_allocate, 1};
var_by_index(1013) -> {is_nonempty_list_allocate, 2};
var_by_index(1014) -> {l_fnegate, 0};
var_by_index(1015) -> {l_bs_validate_unicode, 0};
var_by_index(1016) -> {l_hibernate, 0};
var_by_index(1017) -> {put_list, 8};
var_by_index(1018) -> {is_nonempty_list, 41};
var_by_index(1019) -> {is_nonempty_list, 40};
var_by_index(1020) -> {is_nonempty_list, 39};
var_by_index(1021) -> {is_nonempty_list, 38};
var_by_index(1022) -> {is_nonempty_list, 37};
var_by_index(1023) -> {is_nonempty_list, 36};
var_by_index(1024) -> {is_nonempty_list, 34};
var_by_index(1025) -> {is_nonempty_list, 32};
var_by_index(1026) -> {is_nonempty_list, 31};
var_by_index(1027) -> {is_nonempty_list, 28};
var_by_index(1028) -> {is_nonempty_list, 26};
var_by_index(1029) -> {is_nonempty_list, 25};
var_by_index(1030) -> {is_nonempty_list, 20};
var_by_index(1031) -> {is_nonempty_list, 16};
var_by_index(1032) -> {get_list, 9};
var_by_index(1033) -> {get_list, 7};
var_by_index(1034) -> {get_list, 1};
var_by_index(1035) -> {case_end, 11};
var_by_index(1036) -> {case_end, 10};
var_by_index(1037) -> {case_end, 9};
var_by_index(1038) -> {case_end, 8};
var_by_index(1039) -> {case_end, 7};
var_by_index(1040) -> {case_end, 6};
var_by_index(1041) -> {case_end, 5};
var_by_index(1042) -> {case_end, 4};
var_by_index(1043) -> {case_end, 3};
var_by_index(1044) -> {case_end, 2};
var_by_index(1045) -> {case_end, 1};
var_by_index(1046) -> {case_end, 0};
var_by_index(1047) -> {try_case_end, 1};
var_by_index(1048) -> {try_case_end, 0};
var_by_index(1049) -> {apply_last, 1};
var_by_index(1050) -> {l_call_ext_last, 5};
var_by_index(1051) -> {l_bs_append, 1};
var_by_index(1052) -> {l_increment, 4};
var_by_index(1053) -> {l_bs_private_append, 0};
var_by_index(1054) -> {l_bs_init, 0};
var_by_index(1055) -> {l_new_bs_put_float, 0};
var_by_index(1056) -> {l_bs_validate_unicode_retract, 0};
var_by_index(1057) -> {l_yield, 0};
var_by_index(1058) -> {l_apply_fun_last, 0};
var_by_index(1059) -> {l_minus, 2};
var_by_index(1060) -> {l_minus, 1};
var_by_index(1061) -> {init3, 1};
var_by_index(1062) -> {l_select_val_smallints, 2};
var_by_index(1063) -> {l_bif2, 5};
var_by_index(1064) -> {l_bif2, 3};
var_by_index(1065) -> {l_select_val2, 1};
var_by_index(1066) -> {l_select_tuple_arity2, 3};
var_by_index(1067) -> {l_select_tuple_arity2, 2};
var_by_index(1068) -> {init2, 1};
var_by_index(1069) -> {l_bs_skip_bits_imm2, 2};
var_by_index(1070) -> {l_bs_restore2, 3};
var_by_index(1071) -> {l_bs_restore2, 2};
var_by_index(1072) -> {l_bs_restore2, 1};
var_by_index(1073) -> {l_bs_skip_bits2, 1};
var_by_index(1074) -> {l_bs_get_binary_all2, 2};
var_by_index(1075) -> {l_bs_get_binary_all2, 1};
var_by_index(1076) -> {l_bs_save2, 2};
var_by_index(1077) -> {l_bs_save2, 1};
var_by_index(1078) -> {is_function2, 0};
var_by_index(1079) -> {l_bif1, 2};
var_by_index(1080) -> {l_bif1, 1};
var_by_index(1081) -> {is_nonempty_list_test_heap, 1};
var_by_index(1082) -> {allocate_heap_zero, 1};
var_by_index(1083) -> {deallocate_return, 12};
var_by_index(1084) -> {move_deallocate_return, 9};
var_by_index(1085) -> {l_bs_init_heap_bin, 1};
var_by_index(1086) -> {l_call_fun, 1};
var_by_index(1087) -> {l_new_bs_put_binary_imm, 3};
var_by_index(1088) -> {l_new_bs_put_binary_imm, 2};
var_by_index(1089) -> {l_bs_get_integer_imm, 1};
var_by_index(1090) -> {l_new_bs_put_float_imm, 1};
var_by_index(1091) -> {l_new_bs_put_float_imm, 0};
var_by_index(1092) -> {l_move_call, 35};
var_by_index(1093) -> {l_move_call, 34};
var_by_index(1094) -> {l_move_call, 33};
var_by_index(1095) -> {l_move_call, 32};
var_by_index(1096) -> {l_move_call, 31};
var_by_index(1097) -> {l_move_call, 30};
var_by_index(1098) -> {l_move_call, 29};
var_by_index(1099) -> {l_move_call, 27};
var_by_index(1100) -> {l_move_call, 26};
var_by_index(1101) -> {l_move_call, 24};
var_by_index(1102) -> {l_move_call, 23};
var_by_index(1103) -> {l_move_call, 22};
var_by_index(1104) -> {l_move_call, 21};
var_by_index(1105) -> {l_move_call, 20};
var_by_index(1106) -> {l_move_call, 19};
var_by_index(1107) -> {l_move_call, 18};
var_by_index(1108) -> {l_is_eq_exact_literal, 2};
var_by_index(1109) -> {l_bs_init_bits_fail, 1};
var_by_index(1110) -> {l_jump_on_val, 2};
var_by_index(1111) -> {l_bs_init_fail, 1};
var_by_index(1112) -> {l_call_ext_only, 3};
var_by_index(1113) -> {l_call_ext_only, 1};
var_by_index(1114) -> {l_move_call_ext_only, 9};
var_by_index(1115) -> {l_move_call_ext_only, 8};
var_by_index(1116) -> {l_move_call_ext_only, 5};
var_by_index(1117) -> {bif1_body, 2};
var_by_index(1118) -> {l_select_tuple_arity, 2};
var_by_index(1119) -> {l_fetch, 22};
var_by_index(1120) -> {l_fetch, 11};
var_by_index(1121) -> {l_fetch, 9};
var_by_index(1122) -> {l_fetch, 5};
var_by_index(1123) -> {l_fetch, 3};
var_by_index(1124) -> {l_catch, 7};
var_by_index(1125) -> {l_bs_get_integer_8, 2};
var_by_index(1126) -> {l_bs_get_integer_16, 2};
var_by_index(1127) -> {move, 12};
var_by_index(1128) -> {l_bs_utf8_size, 0};
var_by_index(1129) -> {l_bs_utf16_size, 0};
var_by_index(1130) -> {l_move_call_ext, 49};
var_by_index(1131) -> {l_move_call_ext, 48};
var_by_index(1132) -> {l_move_call_ext, 45};
var_by_index(1133) -> {l_move_call_ext, 43};
var_by_index(1134) -> {l_move_call_ext, 42};
var_by_index(1135) -> {l_move_call_ext, 41};
var_by_index(1136) -> {l_move_call_ext, 40};
var_by_index(1137) -> {l_move_call_ext, 39};
var_by_index(1138) -> {l_move_call_ext, 34};
var_by_index(1139) -> {l_move_call_ext, 33};
var_by_index(1140) -> {l_move_call_ext, 32};
var_by_index(1141) -> {l_move_call_ext, 30};
var_by_index(1142) -> {l_move_call_ext, 29};
var_by_index(1143) -> {l_move_call_ext, 27};
var_by_index(1144) -> {l_move_call_ext, 23};
var_by_index(1145) -> {l_move_call_ext, 20};
var_by_index(1146) -> {l_move_call_ext, 18};
var_by_index(1147) -> {l_move_call_ext, 11};
var_by_index(1148) -> {l_move_call_ext, 0};
var_by_index(1149) -> {catch_end, 7};
var_by_index(1150) -> {test_heap_1_put_list, 2};
var_by_index(1151) -> {l_bs_add, 1};
var_by_index(1152) -> {l_band, 1};
var_by_index(1153) -> {on_load, 0};
var_by_index(1154) -> {l_get, 4};
var_by_index(1155) -> {l_get, 0};
var_by_index(1156) -> {if_end, 0};
var_by_index(1157) -> {l_wait_timeout, 4};
var_by_index(1158) -> {l_wait_timeout, 0};
var_by_index(1159) -> {system_limit, 0};
var_by_index(1160) -> {l_plus, 3};
var_by_index(1161) -> {l_plus, 1};
var_by_index(1162) -> {l_gc_bif3, 0};
var_by_index(1163) -> {move2, 10};
var_by_index(1164) -> {move2, 9};
var_by_index(1165) -> {move2, 8};
var_by_index(1166) -> {move2, 7};
var_by_index(1167) -> {move2, 6};
var_by_index(1168) -> {move2, 5};
var_by_index(1169) -> {move2, 4};
var_by_index(1170) -> {move2, 2};
var_by_index(1171) -> {l_bs_skip_bits_all2, 2};
var_by_index(1172) -> {l_bs_test_zero_tail2, 4};
var_by_index(1173) -> {l_bs_get_integer, 1};
var_by_index(1174) -> {l_bs_get_binary2, 1};
var_by_index(1175) -> {fmove_2, 2};
var_by_index(1176) -> {l_gc_bif2, 0};
var_by_index(1177) -> {l_gc_bif1, 4};
var_by_index(1178) -> {l_gc_bif1, 2};
var_by_index(1179) -> {l_gc_bif1, 1};
var_by_index(1180) -> {test_heap, 1};

var_by_index(Index) -> erlang:error({novarat,Index}).

%%EOF


