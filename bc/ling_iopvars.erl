
-module(ling_iopvars).
-export([var_order/0]).
-export([fit_args/2]).
-export([var_args/2]).
-export([var_index/2]).
-export([var_by_index/1]).

var_order() -> [
		{move,0},
	{move,1},
	{l_call,0},
	{test_heap,0},
	{move,2},
	{badmatch,0},
	{move,3},
	{l_put_tuple,0},
	{move,4},
	{move2,1},
	{get_tuple_element,0},
	{call_bif,7},
	{l_bs_start_match2,0},
	{l_bs_test_zero_tail2,0},
	{l_bs_match_string,0},
	{put_list,0},
	{is_tuple_of_arity,1},
	{get_tuple_element,1},
	{move2,0},
	{l_call_only,0},
	{l_is_eq_exact_immed,0},
	{l_is_eq_exact_immed,1},
	{is_tuple_of_arity,0},
	{l_move_call_ext,0},
	{put_list,1},
	{move,5},
	{get_list,0},
	{l_put_tuple,1},
	{l_call_ext,89},
	{l_is_ge,0},
	{l_make_fun,0},
	{l_move_call_last,0},
	{extract_next_element2,0},
	{is_tuple_of_arity,2},
	{return,0},
	{l_move_call_ext,1},
	{l_fetch,0},
	{call_bif,3},
	{move_deallocate_return,0},
	{l_trim,0},
	{l_allocate,0},
	{move2,3},
	{l_fetch,1},
	{put_list,2},
	{is_nil,0},
	{is_nonempty_list,0},
	{extract_next_element,0},
	{move_return,28},
	{l_allocate,1},
	{l_is_eq_exact_immed,2},
	{jump,0},
	{deallocate_return,0},
	{get_list,1},
	{case_end,0},
	{call_bif,8},
	{move2,2},
	{l_move_call,34},
	{l_is_eq_exact,0},
	{get_tuple_element,2},
	{l_allocate,2},
	{move_return,0},
	{l_is_eq_exact_immed,3},
	{l_select_val2,0},
	{l_fetch,2},
	{get_tuple_element,3},
	{init2,0},
	{get_list,2},
	{l_is_eq_exact_immed,4},
	{init,0},
	{deallocate_return,1},
	{put_list,3},
	{call_bif,9},
	{extract_next_element,1},
	{is_tuple_of_arity,3},
	{l_is_eq_exact_literal,0},
	{get_tuple_element,4},
	{move2,4},
	{l_move_call_ext,41},
	{l_fetch,3},
	{init,1},
	{l_put_tuple,2},
	{move2,5},
	{l_allocate,3},
	{deallocate_return,2},
	{l_fetch,4},
	{init3,0},
	{is_nonempty_list,1},
	{l_is_eq_exact_immed,5},
	{l_allocate_zero,0},
	{call_bif,6},
	{l_trim,1},
	{allocate_init,0},
	{call_bif,45},
	{allocate_heap,0},
	{test_heap_1_put_list,0},
	{l_allocate_zero,1},
	{move_deallocate_return,1},
	{init,2},
	{l_call_last,0},
	{move_return,1},
	{is_nonempty_list,2},
	{l_move_call,0},
	{is_tuple,0},
	{is_list,0},
	{l_is_eq_exact_immed,6},
	{l_call_last,1},
	{deallocate_return,3},
	{is_nonempty_list_allocate,0},
	{l_move_call_only,0},
	{call_bif,5},
	{extract_next_element,2},
	{l_increment,0},
	{l_gc_bif1,0},
	{move,6},
	{l_is_lt,0},
	{l_trim,2},
	{l_select_val_atoms,0},
	{l_call_last,2},
	{move_deallocate_return,2},
	{is_nonempty_list,3},
	{l_new_bs_put_integer_imm,0},
	{is_nil,1},
	{extract_next_element2,1},
	{l_move_call_only,9},
	{l_select_val2,2},
	{remove_message,0},
	{l_move_call_only,1},
	{init,3},
	{l_catch,0},
	{l_allocate_zero,2},
	{extract_next_element3,0},
	{get_tuple_element,5},
	{l_call_ext,0},
	{l_fetch,5},
	{move_jump,12},
	{extract_next_element,3},
	{is_nil,2},
	{move2,6},
	{l_allocate,4},
	{catch_end,0},
	{test_arity,0},
	{l_allocate_zero,3},
	{l_bs_start_match2,1},
	{l_is_eq_exact_immed,7},
	{move_return,2},
	{put_list,4},
	{move_return,3},
	{l_move_call_ext,2},
	{move_return,4},
	{deallocate_return,4},
	{l_call_last,3},
	{move2,7},
	{l_is_eq_exact_immed,8},
	{l_plus,0},
	{move,7},
	{l_put_tuple,3},
	{call_bif,2},
	{l_select_tuple_arity2,0},
	{is_nonempty_list,4},
	{init,4},
	{is_nonempty_list,5},
	{get_list,3},
	{l_call_fun,0},
	{l_call_last,4},
	{l_move_call,1},
	{move_return,5},
	{l_bs_get_binary_all_reuse,0},
	{test_arity,1},
	{bif1_body,0},
	{l_move_call_only,2},
	{l_move_call_last,1},
	{is_nonempty_list,6},
	{l_bs_test_zero_tail2,1},
	{l_is_eq,0},
	{send,0},
	{set_tuple_element,0},
	{l_catch,1},
	{l_call_ext_only,3},
	{call_bif,10},
	{l_is_ne,0},
	{l_move_call_only,3},
	{l_select_val2,1},
	{l_move_call_ext_last,0},
	{l_select_val_smallints,0},
	{l_move_call_ext,3},
	{l_is_ne_exact_immed,0},
	{l_increment,1},
	{l_bs_add,0},
	{bif2_body,0},
	{is_nonempty_list,7},
	{extract_next_element,4},
	{case_end,1},
	{l_bs_match_string,1},
	{l_is_eq_exact_immed,9},
	{extract_next_element2,2},
	{move_deallocate_return,3},
	{extract_next_element2,3},
	{l_loop_rec,0},
	{l_call_ext,1},
	{l_select_tuple_arity2,1},
	{l_move_call,2},
	{l_move_call,3},
	{l_move_call_ext,4},
	{l_bs_get_utf16,0},
	{l_select_val_atoms,1},
	{l_trim,3},
	{init,5},
	{l_bs_restore2,0},
	{catch_end,1},
	{is_nil,3},
	{l_move_call,4},
	{l_bs_init_heap_bin,0},
	{is_nil,4},
	{is_nonempty_list,8},
	{wait,0},
	{l_call_ext_last,0},
	{l_allocate_zero,4},
	{loop_rec_end,0},
	{call_bif,11},
	{deallocate_return,5},
	{move,8},
	{l_fetch,6},
	{is_nonempty_list,9},
	{extract_next_element,5},
	{l_bs_start_match2,2},
	{l_allocate_zero,5},
	{l_select_val2,3},
	{l_allocate,5},
	{init,6},
	{l_move_call_ext,5},
	{badmatch,1},
	{l_plus,1},
	{l_call_last,5},
	{l_move_call_ext,6},
	{l_call_last,6},
	{l_move_call_ext,7},
	{badmatch,2},
	{l_move_call_ext,8},
	{l_call_fun_last,0},
	{l_move_call_ext,9},
	{l_increment,2},
	{bs_context_to_binary,0},
	{call_bif,12},
	{int_code_end,0},
	{l_trim,4},
	{l_move_call_ext_only,0},
	{put_list,5},
	{l_bs_save2,0},
	{call_bif,13},
	{extract_next_element,6},
	{move_deallocate_return,4},
	{l_is_ne_exact_immed,1},
	{extract_next_element2,4},
	{l_select_val2,4},
	{l_gc_bif1,1},
	{l_increment,3},
	{self,0},
	{l_put_tuple,4},
	{extract_next_element,7},
	{get_tuple_element,6},
	{call_bif,14},
	{is_tuple,1},
	{l_bs_test_unit_8,0},
	{l_move_call_last,2},
	{badmatch,3},
	{l_move_call,5},
	{l_is_eq_exact_immed,11},
	{l_move_call,6},
	{is_nil,5},
	{l_bs_test_zero_tail2,2},
	{is_nonempty_list_allocate,1},
	{l_call_ext,2},
	{l_bs_test_zero_tail2,3},
	{l_catch,2},
	{l_move_call_ext,10},
	{call_bif,15},
	{is_atom,0},
	{l_move_call_ext_only,6},
	{l_fast_element,0},
	{is_nonempty_list,10},
	{l_select_val_smallints,1},
	{call_bif,16},
	{l_call_ext,3},
	{move_return,6},
	{l_call_ext_last,1},
	{is_nil,6},
	{allocate_heap_zero,0},
	{is_nonempty_list,11},
	{l_call_ext_last,2},
	{call_bif,17},
	{self,1},
	{extract_next_element,8},
	{init,7},
	{case_end,2},
	{l_minus,0},
	{extract_next_element3,1},
	{is_nil,7},
	{l_move_call,7},
	{call_bif,18},
	{raise,0},
	{catch_end,2},
	{l_is_eq_exact_literal,1},
	{l_is_eq_exact_literal,7},
	{l_move_call_ext,11},
	{extract_next_element2,5},
	{extract_next_element,24},
	{l_allocate,6},
	{l_bif2,0},
	{try_end,0},
	{l_call_fun,1},
	{call_bif,19},
	{deallocate_return,6},
	{l_call_ext,4},
	{l_move_call_last,3},
	{l_move_call_ext_only,1},
	{l_put_tuple,5},
	{l_band,0},
	{l_move_call,8},
	{l_trim,5},
	{extract_next_element,9},
	{call_bif,20},
	{l_call_ext,5},
	{l_call_ext,6},
	{l_minus,1},
	{l_bs_get_binary_all2,0},
	{get_tuple_element,7},
	{l_is_eq_exact_immed,12},
	{test_heap_1_put_list,1},
	{extract_next_element3,2},
	{is_integer,0},
	{extract_next_element2,6},
	{l_catch,3},
	{is_nil,8},
	{l_bif2,1},
	{move_deallocate_return,5},
	{l_move_call_only,4},
	{l_bsr,0},
	{move_jump,0},
	{is_list,1},
	{l_move_call,9},
	{l_bs_get_integer_small_imm,0},
	{l_is_eq_exact_immed,13},
	{apply,0},
	{l_call_ext,7},
	{l_fast_element,2},
	{l_bs_get_integer_8,0},
	{l_bif2,2},
	{l_fetch,7},
	{set_tuple_element,1},
	{try_end,1},
	{l_is_eq_exact_literal,2},
	{l_is_eq_exact_immed,10},
	{extract_next_element2,7},
	{l_is_eq_exact_literal,3},
	{l_bsl,0},
	{l_allocate_zero,6},
	{is_nonempty_list,12},
	{l_times,0},
	{l_select_tuple_arity,0},
	{l_fmul,0},
	{l_call_ext,8},
	{l_bs_match_string,2},
	{move,9},
	{call_bif,21},
	{l_move_call_ext,12},
	{l_put_tuple,6},
	{l_times,1},
	{l_bs_init_fail,0},
	{l_move_call_ext_only,2},
	{l_is_eq_exact_immed,14},
	{l_is_eq_exact_immed,15},
	{l_move_call_ext,14},
	{l_move_call_ext,13},
	{l_call_ext,9},
	{call_bif,22},
	{extract_next_element,10},
	{is_nil,9},
	{l_fetch,8},
	{node,0},
	{l_call_last,7},
	{l_bs_get_binary2,0},
	{is_tuple,2},
	{l_call_fun,2},
	{get_list,4},
	{test_arity,2},
	{l_bs_get_integer_8,1},
	{l_bs_test_zero_tail2,5},
	{catch_end,3},
	{l_bif2,3},
	{l_is_ne_exact_immed,2},
	{l_allocate_zero,9},
	{call_bif,23},
	{l_is_ne_exact,0},
	{l_bif2,4},
	{is_binary,0},
	{l_is_eq_exact_immed,16},
	{l_bs_get_integer_32,0},
	{extract_next_element,11},
	{l_call_ext,10},
	{is_atom,1},
	{l_select_val2,7},
	{l_fetch,9},
	{l_fcheckerror,0},
	{l_new_bs_put_binary_all,0},
	{fclearerror,0},
	{extract_next_element3,3},
	{system_limit,0},
	{node,1},
	{is_nonempty_list,38},
	{extract_next_element,12},
	{get_list,5},
	{l_move_call,10},
	{move_deallocate_return,6},
	{l_move_call_last,4},
	{l_new_bs_put_binary_all,1},
	{l_is_eq_exact_immed,17},
	{timeout,0},
	{deallocate_return,7},
	{l_get,0},
	{l_select_val2,14},
	{l_fetch,10},
	{l_move_call,11},
	{l_move_call_ext_last,1},
	{is_nil,10},
	{l_fetch,11},
	{l_select_val2,5},
	{is_float,1},
	{call_bif,24},
	{l_call_ext,11},
	{l_is_eq_exact_immed,36},
	{l_select_val2,8},
	{l_get,1},
	{call_bif,25},
	{l_bs_restore2,1},
	{l_move_call,12},
	{l_band,1},
	{l_bsl,1},
	{l_fast_element,1},
	{is_binary,1},
	{l_move_call_ext,16},
	{l_get,2},
	{extract_next_element,13},
	{is_nonempty_list,13},
	{l_is_eq_exact_immed,18},
	{l_call_ext,12},
	{l_move_call,13},
	{l_move_call_ext,17},
	{l_rem,0},
	{move2,8},
	{l_call_ext,13},
	{l_allocate_zero,7},
	{l_call_last,8},
	{is_nil,11},
	{l_gc_bif1,2},
	{l_is_ne_exact_immed,10},
	{l_fetch,22},
	{l_increment,4},
	{extract_next_element3,4},
	{is_nil,30},
	{extract_next_element3,10},
	{l_bs_append,0},
	{is_nonempty_list,14},
	{is_integer,5},
	{l_move_call_ext,18},
	{call_bif,26},
	{l_trim,6},
	{is_nil,12},
	{l_call_ext,14},
	{l_bor,0},
	{move_return,7},
	{is_list,2},
	{l_call_ext,15},
	{is_nil,13},
	{l_catch,4},
	{l_fadd,0},
	{l_gc_bif1,5},
	{l_element,1},
	{extract_next_element2,17},
	{call_bif,27},
	{l_allocate,7},
	{l_move_call_only,5},
	{l_move_call_ext_last,4},
	{l_move_call_ext,20},
	{l_move_call_ext,19},
	{is_nonempty_list,15},
	{call_bif,29},
	{call_bif,28},
	{is_integer,1},
	{bif1_body,1},
	{l_call_ext,16},
	{l_is_ne_exact_immed,3},
	{is_nonempty_list,16},
	{l_is_eq_exact_immed,19},
	{l_call_ext_last,3},
	{l_move_call_ext,21},
	{l_fetch,12},
	{fmove_1,0},
	{l_move_call_ext,22},
	{bif1_body,2},
	{move_jump,1},
	{l_bs_get_utf8,0},
	{case_end,10},
	{bif2_body,1},
	{l_move_call_ext,23},
	{l_bs_skip_bits_all2,0},
	{l_call_ext,17},
	{l_is_eq_exact_immed,20},
	{fconv,0},
	{l_bor,1},
	{call_bif,30},
	{l_is_eq_exact_literal,4},
	{l_move_call_ext_last,2},
	{l_bs_init_bits_fail,0},
	{call_bif,31},
	{extract_next_element3,5},
	{extract_next_element2,8},
	{l_is_eq_exact_immed,22},
	{apply_last,0},
	{l_move_call_ext_only,3},
	{call_bif,32},
	{call_bif,4},
	{is_atom,2},
	{call_bif,33},
	{put_list,6},
	{put_list,8},
	{l_bs_match_string,3},
	{l_make_export,0},
	{extract_next_element,14},
	{l_catch,5},
	{init,8},
	{l_increment,8},
	{move_deallocate_return,7},
	{l_call_fun,3},
	{l_select_val2,6},
	{l_new_bs_put_integer,0},
	{fmove_2,0},
	{call_bif,34},
	{badmatch,4},
	{is_atom,3},
	{l_move_call,14},
	{fmove_2,1},
	{l_bs_test_zero_tail2,4},
	{fmove_1,1},
	{l_move_call_ext,24},
	{is_integer_allocate,0},
	{l_call_ext_last,4},
	{call_bif,35},
	{l_fetch,13},
	{l_fast_element,3},
	{l_call_ext,18},
	{l_move_call,15},
	{l_move_call_ext,25},
	{is_tuple,9},
	{l_trim,7},
	{is_list,6},
	{l_fetch,14},
	{deallocate_return,8},
	{l_is_eq_exact_immed,23},
	{l_call_ext,19},
	{extract_next_element,15},
	{l_fetch,15},
	{l_move_call_last,5},
	{is_tuple,3},
	{is_nonempty_list,17},
	{l_move_call_ext,15},
	{l_get,3},
	{extract_next_element,16},
	{extract_next_element2,9},
	{try_end,2},
	{if_end,0},
	{fmove_1,2},
	{call_bif,36},
	{move_return,8},
	{l_move_call,16},
	{l_is_ne_exact_immed,4},
	{l_bs_skip_bits_imm2,0},
	{call_bif,38},
	{call_bif,37},
	{move_jump,2},
	{try_end,4},
	{try_end,3},
	{l_move_call_ext_last,3},
	{call_bif,39},
	{self,2},
	{l_call_ext,23},
	{l_call_ext,22},
	{l_call_ext,21},
	{l_call_ext,20},
	{is_list,3},
	{is_nonempty_list,19},
	{is_nonempty_list,18},
	{try_end,5},
	{catch_end,4},
	{l_call_ext,25},
	{l_call_ext,24},
	{extract_next_element2,10},
	{init,9},
	{l_bif1,0},
	{l_call_ext,26},
	{get_tuple_element,8},
	{l_select_val_atoms,2},
	{test_arity,3},
	{case_end,3},
	{bif1_body,3},
	{l_is_eq_exact_immed,24},
	{l_select_val2,9},
	{l_bs_get_utf16,1},
	{is_tuple,4},
	{l_call_ext,28},
	{l_call_ext,27},
	{extract_next_element,17},
	{l_move_call_ext,26},
	{l_get,6},
	{l_call_ext,29},
	{is_integer,2},
	{badmatch,5},
	{l_bs_put_string,1},
	{try_case_end,0},
	{l_fdiv,0},
	{get_list,6},
	{l_call_ext_last,6},
	{l_bif1,1},
	{put_list,9},
	{move_return,9},
	{case_end,4},
	{self,5},
	{l_call_ext,30},
	{l_bs_test_unit_8,1},
	{l_gc_bif1,3},
	{move_deallocate_return,9},
	{l_is_eq_exact_immed,25},
	{call_bif,41},
	{call_bif,40},
	{extract_next_element,18},
	{extract_next_element2,11},
	{is_nil,14},
	{l_move_call_only,6},
	{l_bs_restore2,2},
	{l_move_call,18},
	{l_move_call,17},
	{bif1_body,5},
	{move,10},
	{l_move_call_ext,28},
	{l_move_call_ext,29},
	{l_bs_get_integer,0},
	{is_atom,6},
	{is_integer,3},
	{l_allocate_zero,8},
	{is_nil,15},
	{is_list,4},
	{case_end,5},
	{l_increment,7},
	{l_is_eq_exact_immed,26},
	{l_increment,5},
	{l_fsub,0},
	{get_tuple_element,9},
	{fconv,1},
	{call_bif,42},
	{l_bsr,1},
	{l_move_call_ext,30},
	{call_bif,43},
	{l_call_ext,31},
	{extract_next_element3,6},
	{badmatch,6},
	{put_list,7},
	{l_move_call_ext,32},
	{l_move_call_ext,31},
	{l_call_ext,33},
	{l_call_ext,32},
	{extract_next_element2,12},
	{is_integer,4},
	{move_return,10},
	{l_rem,1},
	{l_bs_put_string,0},
	{is_nonempty_list,20},
	{move_deallocate_return,8},
	{l_move_call,19},
	{l_is_eq_exact_literal,5},
	{l_call_ext_only,0},
	{l_plus,2},
	{l_increment,6},
	{l_int_div,0},
	{l_bs_get_binary_imm2,0},
	{l_is_eq_exact_literal,6},
	{l_move_call_ext,33},
	{node,4},
	{l_call_ext,34},
	{put_list,10},
	{l_move_call,20},
	{init,10},
	{catch_end,5},
	{badmatch,15},
	{bif1_body,6},
	{l_fetch,16},
	{test_heap_1_put_list,2},
	{l_allocate,9},
	{l_yield,0},
	{is_atom,4},
	{l_move_call,21},
	{l_fetch,17},
	{l_bif2,5},
	{l_is_eq_exact_immed,27},
	{get_list,7},
	{l_bs_get_binary_all2,1},
	{call_bif,44},
	{node,2},
	{l_call_ext,37},
	{l_call_ext,36},
	{l_call_ext,35},
	{l_call_last,9},
	{is_nil,16},
	{is_list,5},
	{case_end,6},
	{l_new_bs_put_float_imm,1},
	{l_move_call,22},
	{l_catch,6},
	{l_move_call_last,6},
	{l_move_call_ext,35},
	{l_bs_append,1},
	{l_call_ext,38},
	{case_end,7},
	{is_nonempty_list,21},
	{l_select_tuple_arity,3},
	{l_move_call_ext,36},
	{l_is_ne_exact_immed,5},
	{l_jump_on_val,0},
	{l_bs_get_integer_32,1},
	{l_bs_skip_bits2,1},
	{is_function,1},
	{l_gc_bif1,4},
	{l_call_ext,39},
	{l_is_eq_exact_immed,21},
	{is_nonempty_list,22},
	{l_int_div,1},
	{l_is_eq_exact_immed,28},
	{l_is_ne_exact_immed,6},
	{l_call_ext,40},
	{extract_next_element2,13},
	{move_jump,3},
	{move_return,11},
	{badmatch,7},
	{l_bs_test_unit_8,3},
	{l_bs_test_unit_8,2},
	{l_move_call,24},
	{l_move_call,23},
	{l_move_call_ext_only,4},
	{l_move_call_ext,37},
	{l_is_eq_exact_immed,29},
	{l_call_ext,41},
	{move_return,12},
	{is_nil,18},
	{is_nil,17},
	{is_nonempty_list,23},
	{l_move_call,27},
	{l_move_call,26},
	{l_move_call,25},
	{l_select_tuple_arity,1},
	{l_fetch,18},
	{bif1_body,7},
	{l_move_call_ext,38},
	{call_bif,1},
	{l_call_ext,48},
	{l_call_ext,47},
	{l_call_ext,46},
	{l_call_ext,45},
	{l_call_ext,44},
	{l_call_ext,43},
	{l_call_ext,42},
	{extract_next_element3,7},
	{l_bs_save2,1},
	{bif2_body,2},
	{is_binary,3},
	{l_move_call_ext,39},
	{deallocate_return,12},
	{l_catch,8},
	{l_allocate,8},
	{l_call_ext,50},
	{l_call_ext,49},
	{l_trim,8},
	{is_nil,19},
	{case_end,8},
	{l_call_fun,4},
	{l_gc_bif1,6},
	{l_bs_skip_bits2,0},
	{l_move_call_ext,40},
	{l_call_ext,53},
	{l_call_ext,52},
	{l_call_ext,51},
	{extract_next_element,19},
	{l_is_ne_exact_literal,0},
	{l_move_call_ext_only,5},
	{l_call_ext_last,5},
	{l_select_tuple_arity2,2},
	{bs_context_to_binary,5},
	{l_select_val2,10},
	{l_fetch,19},
	{init,15},
	{l_get,4},
	{l_call_ext,54},
	{move_return,13},
	{badmatch,8},
	{l_bs_test_unit_8,4},
	{is_pid,1},
	{is_boolean,0},
	{bif1_body,4},
	{l_bs_get_binary2,1},
	{put_list,12},
	{l_call_ext,58},
	{l_call_ext,57},
	{l_call_ext,56},
	{l_call_ext,55},
	{extract_next_element2,14},
	{move_jump,4},
	{move_return,14},
	{l_move_call_only,7},
	{bs_context_to_binary,1},
	{l_call_ext,65},
	{l_call_ext,64},
	{l_call_ext,63},
	{l_call_ext,62},
	{l_call_ext,61},
	{l_call_ext,60},
	{l_call_ext,59},
	{extract_next_element,20},
	{l_move_call_last,7},
	{extract_next_element2,15},
	{move_return,15},
	{l_move_call,28},
	{init,11},
	{l_element,0},
	{l_call_ext,68},
	{l_call_ext,67},
	{l_call_ext,66},
	{l_bs_start_match2,3},
	{move_jump,6},
	{move_jump,5},
	{is_nonempty_list_test_heap,0},
	{catch_end,6},
	{l_get,5},
	{l_bs_skip_bits_all2,1},
	{l_wait_timeout,2},
	{is_nonempty_list_test_heap,1},
	{l_call_ext,70},
	{l_call_ext,69},
	{l_move_call_only,8},
	{case_end,9},
	{is_pid,0},
	{l_new_bs_put_float_imm,0},
	{l_move_call,29},
	{l_select_tuple_arity,2},
	{catch_end,8},
	{l_bif2,6},
	{bif2_body,3},
	{node,3},
	{bs_init_writable,0},
	{l_call_ext,73},
	{l_call_ext,72},
	{l_call_ext,71},
	{extract_next_element,21},
	{move_jump,7},
	{move_return,17},
	{move_return,16},
	{l_new_bs_put_integer_imm,1},
	{bs_context_to_binary,2},
	{put_list,11},
	{is_nonempty_list,25},
	{is_nonempty_list,24},
	{try_end,6},
	{l_bs_private_append,0},
	{deallocate_return,9},
	{l_move_call,30},
	{l_call_ext_only,1},
	{l_apply,0},
	{l_move_call_ext,27},
	{l_bs_get_integer_imm,0},
	{test_heap_1_put_list,3},
	{self,3},
	{is_tuple,5},
	{l_call_ext,76},
	{l_call_ext,75},
	{l_call_ext,74},
	{l_is_ne_exact_immed,7},
	{extract_next_element3,8},
	{l_move_call,33},
	{l_move_call,32},
	{l_move_call,31},
	{l_catch,7},
	{catch_end,7},
	{l_is_ne_exact_immed,8},
	{l_select_val2,11},
	{l_fetch,20},
	{l_call_ext,77},
	{extract_next_element3,9},
	{move_jump,8},
	{l_bs_get_utf8,1},
	{is_nonempty_list,26},
	{is_binary,2},
	{l_fetch,21},
	{l_bs_skip_bits_all2,2},
	{self,4},
	{l_call_ext,80},
	{l_call_ext,79},
	{l_call_ext,78},
	{l_call_last,10},
	{l_new_bs_put_integer,1},
	{move_return,18},
	{is_nil,20},
	{recv_mark,0},
	{bs_context_to_binary,3},
	{badmatch,10},
	{badmatch,9},
	{is_function,0},
	{l_recv_set,0},
	{l_bs_get_integer_16,0},
	{move2,9},
	{l_call_ext,88},
	{l_call_ext,87},
	{l_call_ext,86},
	{l_call_ext,85},
	{l_call_ext,84},
	{l_call_ext,83},
	{l_call_ext,82},
	{l_call_ext,81},
	{l_is_eq_exact_immed,30},
	{extract_next_element2,16},
	{l_bs_get_float2,0},
	{move_jump,9},
	{move_return,19},
	{l_trim,11},
	{is_nil,21},
	{l_select_val2,12},
	{is_atom,5},
	{l_move_call_ext,34},
	{is_float,0},
	{l_is_ne_exact_immed,9},
	{l_minus,2},
	{l_fast_element,4},
	{move_return,22},
	{move_return,21},
	{move_return,20},
	{is_nonempty_list,28},
	{is_nonempty_list,27},
	{l_bif1,2},
	{deallocate_return,11},
	{deallocate_return,10},
	{l_bs_init_bits,0},
	{get_list,8},
	{l_is_eq_exact_immed,31},
	{get_list,10},
	{is_tuple,6},
	{l_call_last,11},
	{extract_next_element,22},
	{move_return,24},
	{move_return,23},
	{badmatch,11},
	{l_select_val2,13},
	{l_call_ext_only,2},
	{get_tuple_element,10},
	{move,12},
	{l_gc_bif1,7},
	{wait_timeout,0},
	{badmatch,12},
	{is_nonempty_list,29},
	{l_times,2},
	{l_apply_fun,0},
	{l_is_eq_exact_immed,32},
	{l_is_eq_exact_immed,33},
	{l_bs_test_tail_imm2,0},
	{l_bs_get_integer_32,2},
	{move_return,25},
	{is_nil,22},
	{badmatch,13},
	{is_integer_allocate,1},
	{l_is_eq_exact_immed,34},
	{get_list,9},
	{is_tuple,8},
	{is_tuple,7},
	{extract_next_element,23},
	{move_jump,10},
	{is_nil,23},
	{bs_context_to_binary,4},
	{badmatch,14},
	{is_nonempty_list,32},
	{is_nonempty_list,31},
	{is_nonempty_list,30},
	{l_bs_init_fail,1},
	{move_jump,11},
	{is_nil,24},
	{is_nonempty_list,34},
	{is_nonempty_list,33},
	{try_end,7},
	{init,12},
	{l_bs_add,1},
	{l_wait_timeout,0},
	{l_fast_element,5},
	{test_heap_1_put_list,4},
	{l_gc_bif2,0},
	{l_bs_put_utf16,0},
	{l_is_eq_exact_immed,35},
	{move_return,27},
	{move_return,26},
	{l_trim,9},
	{is_nil,25},
	{l_bs_validate_unicode,0},
	{is_nonempty_list,35},
	{l_bs_init,0},
	{l_jump_on_val,1},
	{move,11},
	{l_bs_utf16_size,0},
	{l_bs_get_binary2,2},
	{l_bs_restore2,3},
	{is_nil,26},
	{raise,1},
	{l_int_bnot,0},
	{is_nil,29},
	{is_nil,28},
	{is_nil,27},
	{is_nonempty_list,37},
	{is_nonempty_list,36},
	{l_bs_save2,2},
	{l_bs_get_binary_imm2,1},
	{is_bitstr,0},
	{l_new_bs_put_binary_all,2},
	{l_new_bs_put_binary,0},
	{fmove_2,2},
	{is_reference,0},
	{is_port,0},
	{is_number,0},
	{move,13},
	{l_bs_get_binary_all_reuse,1},
	{init,13},
	{l_wait_timeout,1},
	{l_select_tuple_arity,4},
	{l_trim,10},
	{l_bs_put_utf8,0},
	{init,14},
	{l_fnegate,0},
	{l_bs_get_integer_imm,1},
	{l_jump_on_val,2},
	{l_bs_utf8_size,0},
	{l_bs_get_binary_imm2,2},
	{l_bs_validate_unicode_retract,0},
	{l_bxor,0},
	{l_new_bs_put_float,0},
	{l_apply_last,0},
	{l_is_function2,0},
	{l_gc_bif3,0},
	{l_bor,2},
	{l_new_bs_put_binary_imm,0},
	{l_bs_get_integer_8,2},
	{l_bs_start_match2,4},
	{l_rem,2},
	{l_bs_get_integer_small_imm,1},
	{l_bsl,2},
	{l_apply_only,0},
	{on_load,0},
	{move2,10},
	{l_int_div,2},
	{l_bs_test_unit,0},
	{l_m_div,0},
	{l_hibernate,0},
	{l_apply_fun_last,0},
	{is_function2,0},
	{l_apply_fun_only,0},
	{l_band,2},
	{is_bigint,0},
	{test_heap,1},
	{func_info,0},
	{call_bif,0},
	{l_bs_get_utf16,2},
	{l_put_tuple,7},
	{get_tuple_element,11},
	{allocate_init,1},
	{l_call_fun_last,1},
	{set_tuple_element,2},
	{allocate_heap,1},
	{is_tuple_of_arity,4},
	{test_arity,4},
	{l_bs_match_string,4},
	{is_nonempty_list_allocate,2},
	{l_bs_append,2},
	{try_case_end,1},
	{init3,1},
	{l_select_val_smallints,2},
	{l_select_tuple_arity2,3},
	{init2,1},
	{l_bs_get_binary_all2,2},
	{is_nonempty_list_test_heap,2},
	{allocate_heap_zero,1},
	{l_bs_init_heap_bin,1},
	{l_plus,3},
	{l_bs_get_integer,1}

].


fit_args(allocate_heap, [NumSlots,HeapNeeded,Live]) when NumSlots >= 0, NumSlots =< 255, HeapNeeded >= 0, HeapNeeded =< 255, Live >= 0, Live =< 255 -> 0;
fit_args(allocate_heap, [_,_,Live]) when Live >= 0, Live =< 255 -> 1;
fit_args(allocate_heap_zero, [NumSlots,HeapNeeded,Live]) when NumSlots >= 0, NumSlots =< 255, HeapNeeded >= 0, HeapNeeded =< 255, Live >= 0, Live =< 255 -> 0;
fit_args(allocate_heap_zero, [_,_,Live]) when Live >= 0, Live =< 255 -> 1;
fit_args(allocate_init, [_,{y,0}]) -> 0;
fit_args(allocate_init, [_,_]) -> 1;
fit_args(apply, [Arg0]) when Arg0 >= 0, Arg0 =< 255 -> 0;
fit_args(apply_last, [Arg0,_]) when Arg0 >= 0, Arg0 =< 255 -> 0;
fit_args(badmatch, [{x,0}]) -> 0;
fit_args(badmatch, [{x,3}]) -> 1;
fit_args(badmatch, [{x,2}]) -> 2;
fit_args(badmatch, [{x,1}]) -> 3;
fit_args(badmatch, [{y,2}]) -> 4;
fit_args(badmatch, [{y,3}]) -> 5;
fit_args(badmatch, [{x,4}]) -> 6;
fit_args(badmatch, [{y,4}]) -> 7;
fit_args(badmatch, [{y,0}]) -> 8;
fit_args(badmatch, [{y,9}]) -> 10;
fit_args(badmatch, [{x,5}]) -> 9;
fit_args(badmatch, [{y,5}]) -> 11;
fit_args(badmatch, [{y,6}]) -> 12;
fit_args(badmatch, [{y,1}]) -> 13;
fit_args(badmatch, [{x,8}]) -> 14;
fit_args(badmatch, [_]) -> 15;
fit_args(bif1_body, [{b,{erlang,hd,1}},{x,0},{x,1}]) -> 1;
fit_args(bif1_body, [{b,{erlang,hd,1}},{y,1},{x,2}]) -> 2;
fit_args(bif1_body, [{b,{erlang,hd,1}},{y,Arg1},{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 4;
fit_args(bif1_body, [{b,{erlang,hd,1}},_,{x,0}]) -> 0;
fit_args(bif1_body, [{b,_},{x,0},{x,0}]) -> 3;
fit_args(bif1_body, [{b,_},_,{x,1}]) -> 5;
fit_args(bif1_body, [{b,_},_,{x,0}]) -> 6;
fit_args(bif1_body, [{b,_},_,_]) -> 7;
fit_args(bif2_body, [{b,_},{x,0}]) -> 0;
fit_args(bif2_body, [{b,_},{x,1}]) -> 1;
fit_args(bif2_body, [{b,_},{x,2}]) -> 2;
fit_args(bif2_body, [{b,_},_]) -> 3;
fit_args(bs_context_to_binary, [{x,0}]) -> 0;
fit_args(bs_context_to_binary, [{x,1}]) -> 1;
fit_args(bs_context_to_binary, [{x,2}]) -> 2;
fit_args(bs_context_to_binary, [{y,0}]) -> 3;
fit_args(bs_context_to_binary, [{x,4}]) -> 4;
fit_args(bs_context_to_binary, [_]) -> 5;
fit_args(bs_init_writable, []) -> 0;
fit_args(call_bif, [{b,{erlang,iolist_to_binary,1}}]) -> 7;
fit_args(call_bif, [{b,{erlang,error,1}}]) -> 3;
fit_args(call_bif, [{b,{erlang,setelement,3}}]) -> 8;
fit_args(call_bif, [{b,{erlang,'++',2}}]) -> 9;
fit_args(call_bif, [{b,{erlang,throw,1}}]) -> 6;
fit_args(call_bif, [{b,{erlang,exit,1}}]) -> 5;
fit_args(call_bif, [{b,{erlang,error,2}}]) -> 2;
fit_args(call_bif, [{b,{lists,member,2}}]) -> 10;
fit_args(call_bif, [{b,{ets,insert,2}}]) -> 11;
fit_args(call_bif, [{b,{erlang,get_module_info,2}}]) -> 12;
fit_args(call_bif, [{b,{erlang,list_to_binary,1}}]) -> 13;
fit_args(call_bif, [{b,{ets,delete,1}}]) -> 14;
fit_args(call_bif, [{b,{lists,keysearch,3}}]) -> 15;
fit_args(call_bif, [{b,{ets,info,2}}]) -> 16;
fit_args(call_bif, [{b,{erlang,list_to_atom,1}}]) -> 17;
fit_args(call_bif, [{b,{erlang,integer_to_list,1}}]) -> 18;
fit_args(call_bif, [{b,{ets,lookup,2}}]) -> 19;
fit_args(call_bif, [{b,{erlang,atom_to_list,1}}]) -> 20;
fit_args(call_bif, [{b,{ets,lookup_element,3}}]) -> 21;
fit_args(call_bif, [{b,{erlang,binary_to_list,1}}]) -> 22;
fit_args(call_bif, [{b,{erlang,'--',2}}]) -> 23;
fit_args(call_bif, [{b,{re,run,3}}]) -> 24;
fit_args(call_bif, [{b,{erlang,process_flag,2}}]) -> 25;
fit_args(call_bif, [{b,{erlang,process_info,2}}]) -> 26;
fit_args(call_bif, [{b,{erlang,unlink,1}}]) -> 27;
fit_args(call_bif, [{b,{erlang,tuple_to_list,1}}]) -> 29;
fit_args(call_bif, [{b,{erlang,whereis,1}}]) -> 28;
fit_args(call_bif, [{b,{lists,keyfind,3}}]) -> 30;
fit_args(call_bif, [{b,{erlang,list_to_tuple,1}}]) -> 31;
fit_args(call_bif, [{b,{ets,new,2}}]) -> 32;
fit_args(call_bif, [{b,{erlang,exit,2}}]) -> 4;
fit_args(call_bif, [{b,{erlang,make_ref,0}}]) -> 33;
fit_args(call_bif, [{b,{lists,reverse,2}}]) -> 34;
fit_args(call_bif, [{b,{lists,keymember,3}}]) -> 35;
fit_args(call_bif, [{b,{erlang,now,0}}]) -> 36;
fit_args(call_bif, [{b,{erlang,spawn_link,1}}]) -> 38;
fit_args(call_bif, [{b,{erlang,get_stacktrace,0}}]) -> 37;
fit_args(call_bif, [{b,{ets,delete,2}}]) -> 39;
fit_args(call_bif, [{b,{ets,match_object,2}}]) -> 41;
fit_args(call_bif, [{b,{ets,safe_fixtable,2}}]) -> 40;
fit_args(call_bif, [{b,{ets,next,2}}]) -> 42;
fit_args(call_bif, [{b,{ets,match,2}}]) -> 43;
fit_args(call_bif, [{b,{erlang,monitor,2}}]) -> 44;
fit_args(call_bif, [{b,{erlang,raise,3}}]) -> 1;
fit_args(call_bif, [{b,{erlang,purge_module,1}}]) -> 0;
fit_args(call_bif, [{b,_}]) -> 45;
fit_args(case_end, [{x,0}]) -> 0;
fit_args(case_end, [{x,1}]) -> 1;
fit_args(case_end, [{x,2}]) -> 2;
fit_args(case_end, [{y,2}]) -> 3;
fit_args(case_end, [{x,3}]) -> 4;
fit_args(case_end, [{y,1}]) -> 5;
fit_args(case_end, [{y,3}]) -> 6;
fit_args(case_end, [{x,4}]) -> 7;
fit_args(case_end, [{y,4}]) -> 8;
fit_args(case_end, [{y,0}]) -> 9;
fit_args(case_end, [_]) -> 10;
fit_args(catch_end, [{y,0}]) -> 0;
fit_args(catch_end, [{y,1}]) -> 1;
fit_args(catch_end, [{y,2}]) -> 2;
fit_args(catch_end, [{y,3}]) -> 3;
fit_args(catch_end, [{y,4}]) -> 4;
fit_args(catch_end, [{y,5}]) -> 5;
fit_args(catch_end, [{y,6}]) -> 6;
fit_args(catch_end, [{y,20}]) -> 7;
fit_args(catch_end, [_]) -> 8;
fit_args(deallocate_return, [1]) -> 0;
fit_args(deallocate_return, [0]) -> 1;
fit_args(deallocate_return, [2]) -> 2;
fit_args(deallocate_return, [3]) -> 3;
fit_args(deallocate_return, [4]) -> 4;
fit_args(deallocate_return, [5]) -> 5;
fit_args(deallocate_return, [6]) -> 6;
fit_args(deallocate_return, [7]) -> 7;
fit_args(deallocate_return, [8]) -> 8;
fit_args(deallocate_return, [9]) -> 9;
fit_args(deallocate_return, [11]) -> 11;
fit_args(deallocate_return, [10]) -> 10;
fit_args(deallocate_return, [_]) -> 12;
fit_args(extract_next_element, [{x,1}]) -> 0;
fit_args(extract_next_element, [{x,3}]) -> 1;
fit_args(extract_next_element, [{x,2}]) -> 2;
fit_args(extract_next_element, [{x,4}]) -> 3;
fit_args(extract_next_element, [{x,5}]) -> 4;
fit_args(extract_next_element, [{x,6}]) -> 5;
fit_args(extract_next_element, [{x,255}]) -> 6;
fit_args(extract_next_element, [{x,7}]) -> 7;
fit_args(extract_next_element, [{y,1}]) -> 8;
fit_args(extract_next_element, [{y,0}]) -> 9;
fit_args(extract_next_element, [{x,8}]) -> 10;
fit_args(extract_next_element, [{y,3}]) -> 11;
fit_args(extract_next_element, [{y,2}]) -> 12;
fit_args(extract_next_element, [{x,9}]) -> 13;
fit_args(extract_next_element, [{x,10}]) -> 14;
fit_args(extract_next_element, [{y,4}]) -> 15;
fit_args(extract_next_element, [{y,5}]) -> 16;
fit_args(extract_next_element, [{x,11}]) -> 17;
fit_args(extract_next_element, [{x,12}]) -> 18;
fit_args(extract_next_element, [{x,13}]) -> 19;
fit_args(extract_next_element, [{x,14}]) -> 20;
fit_args(extract_next_element, [{y,6}]) -> 21;
fit_args(extract_next_element, [{x,18}]) -> 22;
fit_args(extract_next_element, [{x,15}]) -> 23;
fit_args(extract_next_element, [_]) -> 24;
fit_args(extract_next_element2, [{x,1}]) -> 0;
fit_args(extract_next_element2, [{x,3}]) -> 1;
fit_args(extract_next_element2, [{x,4}]) -> 2;
fit_args(extract_next_element2, [{x,2}]) -> 3;
fit_args(extract_next_element2, [{x,5}]) -> 4;
fit_args(extract_next_element2, [{x,6}]) -> 5;
fit_args(extract_next_element2, [{x,8}]) -> 6;
fit_args(extract_next_element2, [{x,7}]) -> 7;
fit_args(extract_next_element2, [{x,9}]) -> 8;
fit_args(extract_next_element2, [{x,12}]) -> 9;
fit_args(extract_next_element2, [{x,10}]) -> 10;
fit_args(extract_next_element2, [{x,11}]) -> 11;
fit_args(extract_next_element2, [{y,0}]) -> 12;
fit_args(extract_next_element2, [{x,13}]) -> 13;
fit_args(extract_next_element2, [{x,14}]) -> 14;
fit_args(extract_next_element2, [{x,16}]) -> 15;
fit_args(extract_next_element2, [{x,15}]) -> 16;
fit_args(extract_next_element2, [_]) -> 17;
fit_args(extract_next_element3, [{x,1}]) -> 0;
fit_args(extract_next_element3, [{x,3}]) -> 1;
fit_args(extract_next_element3, [{x,2}]) -> 2;
fit_args(extract_next_element3, [{x,5}]) -> 3;
fit_args(extract_next_element3, [{x,4}]) -> 4;
fit_args(extract_next_element3, [{x,7}]) -> 5;
fit_args(extract_next_element3, [{x,6}]) -> 6;
fit_args(extract_next_element3, [{x,8}]) -> 7;
fit_args(extract_next_element3, [{x,11}]) -> 8;
fit_args(extract_next_element3, [{x,10}]) -> 9;
fit_args(extract_next_element3, [_]) -> 10;
fit_args(fclearerror, []) -> 0;
fit_args(fconv, [_,{fr,0}]) -> 0;
fit_args(fconv, [_,{fr,_}]) -> 1;
fit_args(fmove_1, [{x,_},{fr,_}]) -> 0;
fit_args(fmove_1, [_,{fr,1}]) -> 1;
fit_args(fmove_1, [_,{fr,_}]) -> 2;
fit_args(fmove_2, [{fr,_},{x,0}]) -> 1;
fit_args(fmove_2, [{fr,_},{x,_}]) -> 0;
fit_args(fmove_2, [{fr,_},_]) -> 2;
fit_args(func_info, [_,_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(get_list, [{x,0},{x,_},{x,_}]) -> 1;
fit_args(get_list, [{x,0},{y,Arg1},{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 7;
fit_args(get_list, [{x,0},_,{x,0}]) -> 5;
fit_args(get_list, [{x,_},{y,Arg1},{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 9;
fit_args(get_list, [{x,_},{x,_},{x,_}]) -> 0;
fit_args(get_list, [{y,Arg0},{x,_},{x,_}]) when Arg0 >= 0, Arg0 =< 255 -> 4;
fit_args(get_list, [{x,_},{y,Arg1},{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 6;
fit_args(get_list, [{x,_},{y,Arg1},{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 8;
fit_args(get_list, [_,{x,0},_]) -> 2;
fit_args(get_list, [_,{x,_},{y,Arg2}]) when Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(get_list, [_,_,_]) -> 10;
fit_args(get_tuple_element, [{x,0},1,{x,0}]) -> 6;
fit_args(get_tuple_element, [{x,0},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(get_tuple_element, [{x,0},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(get_tuple_element, [{x,0},_,{x,0}]) -> 8;
fit_args(get_tuple_element, [{x,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 7;
fit_args(get_tuple_element, [{y,Arg0},Arg1,{x,0}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 9;
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
fit_args(init, [{y,6}]) -> 6;
fit_args(init, [{y,7}]) -> 7;
fit_args(init, [{y,8}]) -> 8;
fit_args(init, [{y,9}]) -> 9;
fit_args(init, [{y,10}]) -> 10;
fit_args(init, [{y,11}]) -> 11;
fit_args(init, [{y,12}]) -> 12;
fit_args(init, [{y,13}]) -> 13;
fit_args(init, [{y,14}]) -> 14;
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
fit_args(is_binary, [{f,_},{x,1}]) -> 0;
fit_args(is_binary, [{f,_},{x,0}]) -> 1;
fit_args(is_binary, [{f,_},{x,2}]) -> 2;
fit_args(is_binary, [{f,_},_]) -> 3;
fit_args(is_bitstr, [{f,_},_]) -> 0;
fit_args(is_boolean, [{f,_},_]) -> 0;
fit_args(is_float, [{f,_},{x,0}]) -> 0;
fit_args(is_float, [{f,_},_]) -> 1;
fit_args(is_function, [{f,_},{x,0}]) -> 0;
fit_args(is_function, [{f,_},_]) -> 1;
fit_args(is_function2, [{f,_},_,_]) -> 0;
fit_args(is_integer, [{f,_},{x,0}]) -> 0;
fit_args(is_integer, [{f,_},{x,1}]) -> 1;
fit_args(is_integer, [{f,_},{x,2}]) -> 2;
fit_args(is_integer, [{f,_},{x,4}]) -> 3;
fit_args(is_integer, [{f,_},{x,3}]) -> 4;
fit_args(is_integer, [{f,_},_]) -> 5;
fit_args(is_integer_allocate, [{f,_},{x,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(is_integer_allocate, [{f,_},_,_]) -> 1;
fit_args(is_list, [{f,_},{x,0}]) -> 0;
fit_args(is_list, [{f,_},{x,1}]) -> 1;
fit_args(is_list, [{f,_},{x,2}]) -> 2;
fit_args(is_list, [{f,_},{x,3}]) -> 3;
fit_args(is_list, [{f,_},{x,4}]) -> 4;
fit_args(is_list, [{f,_},{x,5}]) -> 5;
fit_args(is_list, [{f,_},_]) -> 6;
fit_args(is_nil, [{f,_},{x,0}]) -> 0;
fit_args(is_nil, [{f,_},{x,2}]) -> 1;
fit_args(is_nil, [{f,_},{x,1}]) -> 2;
fit_args(is_nil, [{f,_},{x,4}]) -> 3;
fit_args(is_nil, [{f,_},{x,3}]) -> 4;
fit_args(is_nil, [{f,_},{x,5}]) -> 5;
fit_args(is_nil, [{f,_},{x,6}]) -> 6;
fit_args(is_nil, [{f,_},{x,7}]) -> 7;
fit_args(is_nil, [{f,_},{x,8}]) -> 8;
fit_args(is_nil, [{f,_},{x,9}]) -> 9;
fit_args(is_nil, [{f,_},{y,1}]) -> 10;
fit_args(is_nil, [{f,_},{x,10}]) -> 11;
fit_args(is_nil, [{f,_},{x,11}]) -> 12;
fit_args(is_nil, [{f,_},{x,12}]) -> 13;
fit_args(is_nil, [{f,_},{x,13}]) -> 14;
fit_args(is_nil, [{f,_},{x,14}]) -> 15;
fit_args(is_nil, [{f,_},{y,2}]) -> 16;
fit_args(is_nil, [{f,_},{x,16}]) -> 18;
fit_args(is_nil, [{f,_},{x,15}]) -> 17;
fit_args(is_nil, [{f,_},{y,3}]) -> 19;
fit_args(is_nil, [{f,_},{y,0}]) -> 20;
fit_args(is_nil, [{f,_},{x,17}]) -> 21;
fit_args(is_nil, [{f,_},{y,5}]) -> 22;
fit_args(is_nil, [{f,_},{x,19}]) -> 23;
fit_args(is_nil, [{f,_},{y,4}]) -> 24;
fit_args(is_nil, [{f,_},{x,22}]) -> 25;
fit_args(is_nil, [{f,_},{x,18}]) -> 26;
fit_args(is_nil, [{f,_},{x,21}]) -> 29;
fit_args(is_nil, [{f,_},{x,20}]) -> 28;
fit_args(is_nil, [{f,_},{x,23}]) -> 27;
fit_args(is_nil, [{f,_},_]) -> 30;
fit_args(is_nonempty_list, [{f,_},{x,0}]) -> 0;
fit_args(is_nonempty_list, [{f,_},{x,2}]) -> 1;
fit_args(is_nonempty_list, [{f,_},{x,1}]) -> 2;
fit_args(is_nonempty_list, [{f,_},{x,3}]) -> 3;
fit_args(is_nonempty_list, [{f,_},{x,4}]) -> 4;
fit_args(is_nonempty_list, [{f,_},{x,7}]) -> 5;
fit_args(is_nonempty_list, [{f,_},{x,5}]) -> 6;
fit_args(is_nonempty_list, [{f,_},{x,6}]) -> 7;
fit_args(is_nonempty_list, [{f,_},{x,9}]) -> 8;
fit_args(is_nonempty_list, [{f,_},{x,8}]) -> 9;
fit_args(is_nonempty_list, [{f,_},{x,10}]) -> 10;
fit_args(is_nonempty_list, [{f,_},{x,11}]) -> 11;
fit_args(is_nonempty_list, [{f,_},{x,12}]) -> 12;
fit_args(is_nonempty_list, [{f,_},{y,2}]) -> 13;
fit_args(is_nonempty_list, [{f,_},{x,13}]) -> 14;
fit_args(is_nonempty_list, [{f,_},{y,3}]) -> 15;
fit_args(is_nonempty_list, [{f,_},{x,14}]) -> 16;
fit_args(is_nonempty_list, [{f,_},{y,1}]) -> 17;
fit_args(is_nonempty_list, [{f,_},{x,16}]) -> 19;
fit_args(is_nonempty_list, [{f,_},{x,15}]) -> 18;
fit_args(is_nonempty_list, [{f,_},{x,17}]) -> 20;
fit_args(is_nonempty_list, [{f,_},{y,4}]) -> 21;
fit_args(is_nonempty_list, [{f,_},{x,18}]) -> 22;
fit_args(is_nonempty_list, [{f,_},{x,20}]) -> 23;
fit_args(is_nonempty_list, [{f,_},{x,19}]) -> 25;
fit_args(is_nonempty_list, [{f,_},{y,9}]) -> 24;
fit_args(is_nonempty_list, [{f,_},{y,0}]) -> 26;
fit_args(is_nonempty_list, [{f,_},{y,6}]) -> 28;
fit_args(is_nonempty_list, [{f,_},{x,24}]) -> 27;
fit_args(is_nonempty_list, [{f,_},{x,21}]) -> 29;
fit_args(is_nonempty_list, [{f,_},{y,8}]) -> 32;
fit_args(is_nonempty_list, [{f,_},{x,22}]) -> 31;
fit_args(is_nonempty_list, [{f,_},{x,25}]) -> 30;
fit_args(is_nonempty_list, [{f,_},{y,7}]) -> 34;
fit_args(is_nonempty_list, [{f,_},{y,5}]) -> 33;
fit_args(is_nonempty_list, [{f,_},{x,26}]) -> 35;
fit_args(is_nonempty_list, [{f,_},{x,27}]) -> 37;
fit_args(is_nonempty_list, [{f,_},{x,23}]) -> 36;
fit_args(is_nonempty_list, [{f,_},_]) -> 38;
fit_args(is_nonempty_list_allocate, [{f,_},{x,0},_]) -> 0;
fit_args(is_nonempty_list_allocate, [{f,_},{x,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(is_nonempty_list_allocate, [{f,_},_,_]) -> 2;
fit_args(is_nonempty_list_test_heap, [{f,_},5,1]) -> 0;
fit_args(is_nonempty_list_test_heap, [{f,_},Arg1,Arg2]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(is_nonempty_list_test_heap, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(is_number, [{f,_},_]) -> 0;
fit_args(is_pid, [{f,_},{x,0}]) -> 0;
fit_args(is_pid, [{f,_},_]) -> 1;
fit_args(is_port, [{f,_},_]) -> 0;
fit_args(is_reference, [{f,_},_]) -> 0;
fit_args(is_tuple, [{f,_},{x,0}]) -> 0;
fit_args(is_tuple, [{f,_},{x,1}]) -> 1;
fit_args(is_tuple, [{f,_},{x,2}]) -> 2;
fit_args(is_tuple, [{f,_},{x,3}]) -> 3;
fit_args(is_tuple, [{f,_},{x,4}]) -> 4;
fit_args(is_tuple, [{f,_},{x,7}]) -> 5;
fit_args(is_tuple, [{f,_},{x,5}]) -> 6;
fit_args(is_tuple, [{f,_},{x,6}]) -> 8;
fit_args(is_tuple, [{f,_},{y,4}]) -> 7;
fit_args(is_tuple, [{f,_},_]) -> 9;
fit_args(is_tuple_of_arity, [{f,_},{x,0},2]) -> 0;
fit_args(is_tuple_of_arity, [{f,_},{x,0},_]) -> 2;
fit_args(is_tuple_of_arity, [{f,_},{x,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(is_tuple_of_arity, [{f,_},{y,Arg1},Arg2]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(is_tuple_of_arity, [{f,_},_,_]) -> 4;
fit_args(jump, [{f,_}]) -> 0;
fit_args(l_allocate, [1]) -> 0;
fit_args(l_allocate, [0]) -> 1;
fit_args(l_allocate, [2]) -> 2;
fit_args(l_allocate, [3]) -> 3;
fit_args(l_allocate, [4]) -> 4;
fit_args(l_allocate, [5]) -> 5;
fit_args(l_allocate, [6]) -> 6;
fit_args(l_allocate, [7]) -> 7;
fit_args(l_allocate, [8]) -> 8;
fit_args(l_allocate, [_]) -> 9;
fit_args(l_allocate_zero, [2]) -> 0;
fit_args(l_allocate_zero, [1]) -> 1;
fit_args(l_allocate_zero, [3]) -> 2;
fit_args(l_allocate_zero, [4]) -> 3;
fit_args(l_allocate_zero, [6]) -> 4;
fit_args(l_allocate_zero, [5]) -> 5;
fit_args(l_allocate_zero, [7]) -> 6;
fit_args(l_allocate_zero, [8]) -> 7;
fit_args(l_allocate_zero, [9]) -> 8;
fit_args(l_allocate_zero, [_]) -> 9;
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
fit_args(l_bif2, [{f,_},{b,{erlang,'=:=',2}},_]) -> 1;
fit_args(l_bif2, [{f,_},{b,{erlang,'=<',2}},_]) -> 2;
fit_args(l_bif2, [{f,_},{b,{erlang,'or',2}},_]) -> 3;
fit_args(l_bif2, [{f,_},{b,{erlang,'and',2}},_]) -> 4;
fit_args(l_bif2, [{f,_},{b,{erlang,'==',2}},_]) -> 5;
fit_args(l_bif2, [{f,_},{b,_},_]) -> 6;
fit_args(l_bor, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bor, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_bor, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_bs_add, [{f,_},1,_]) -> 0;
fit_args(l_bs_add, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_bs_append, [{f,_},Arg1,Arg2,Arg3,{x,0}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_append, [{f,_},Arg1,Arg2,Arg3,{x,_}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_append, [{f,_},_,Arg2,Arg3,_]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 2;
fit_args(l_bs_get_binary2, [{f,_},{x,0},Arg2,{x,_},Arg4,0,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 1;
fit_args(l_bs_get_binary2, [{f,_},{x,_},Arg2,_,Arg4,0,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 0;
fit_args(l_bs_get_binary2, [{f,_},_,Arg2,_,Arg4,Arg5,_]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255, Arg5 >= 0, Arg5 =< 255 -> 2;
fit_args(l_bs_get_binary_all2, [{f,_},{x,0},Arg2,Arg3,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_get_binary_all2, [{f,_},{x,_},Arg2,Arg3,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_get_binary_all2, [{f,_},_,Arg2,Arg3,_]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 2;
fit_args(l_bs_get_binary_all_reuse, [_,{f,_},8]) -> 0;
fit_args(l_bs_get_binary_all_reuse, [_,{f,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_get_binary_imm2, [{f,_},{x,0},Arg2,Arg3,Arg4,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255, Arg4 >= 0, Arg4 =< 255 -> 1;
fit_args(l_bs_get_binary_imm2, [{f,_},{x,_},Arg2,Arg3,0,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_get_binary_imm2, [{f,_},_,Arg2,_,Arg4,_]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 2;
fit_args(l_bs_get_float2, [{f,_},_,Arg2,_,Arg4,Arg5,_]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255, Arg5 >= 0, Arg5 =< 255 -> 0;
fit_args(l_bs_get_integer, [{f,_},Arg1,Arg2,Arg3,{x,_}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_get_integer, [{f,_},Arg1,Arg2,Arg3,_]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_get_integer_16, [_,{f,_},_]) -> 0;
fit_args(l_bs_get_integer_32, [{x,0},{f,_},Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_get_integer_32, [{x,_},{f,_},Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_get_integer_32, [_,{f,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_bs_get_integer_8, [{x,0},{f,_},{x,_}]) -> 1;
fit_args(l_bs_get_integer_8, [{x,_},{f,_},{x,_}]) -> 0;
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
fit_args(l_bs_init_bits_fail, [_,{f,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_init_fail, [Arg0,{f,_},Arg2,{x,_}]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_init_fail, [_,{f,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_init_heap_bin, [Arg0,Arg1,Arg2,_]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_init_heap_bin, [_,_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_bs_match_string, [{x,1},{f,_},_,{str,_}]) -> 0;
fit_args(l_bs_match_string, [{x,0},{f,_},_,{str,_}]) -> 2;
fit_args(l_bs_match_string, [{x,_},{f,_},Arg2,{str,_}]) when Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(l_bs_match_string, [_,{f,_},8,{str,_}]) -> 1;
fit_args(l_bs_match_string, [_,{f,_},_,{str,_}]) -> 4;
fit_args(l_bs_private_append, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_put_string, [1,{str,_}]) -> 0;
fit_args(l_bs_put_string, [_,{str,_}]) -> 1;
fit_args(l_bs_put_utf16, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_put_utf8, [{f,_},_]) -> 0;
fit_args(l_bs_restore2, [{x,0},0]) -> 1;
fit_args(l_bs_restore2, [{x,0},1]) -> 2;
fit_args(l_bs_restore2, [{x,_},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_restore2, [_,_]) -> 3;
fit_args(l_bs_save2, [{x,0},1]) -> 1;
fit_args(l_bs_save2, [{x,_},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_save2, [_,_]) -> 2;
fit_args(l_bs_skip_bits2, [{f,_},{x,_},{x,_},Arg3]) when Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_skip_bits2, [{f,_},_,_,Arg3]) when Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_skip_bits_all2, [{f,_},{x,2},8]) -> 0;
fit_args(l_bs_skip_bits_all2, [{f,_},{x,3},8]) -> 1;
fit_args(l_bs_skip_bits_all2, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_bs_skip_bits_imm2, [{f,_},_,_]) -> 0;
fit_args(l_bs_start_match2, [{x,0},{f,_},1,0,{x,1}]) -> 0;
fit_args(l_bs_start_match2, [{x,0},{f,_},Arg2,Arg3,{x,0}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 2;
fit_args(l_bs_start_match2, [_,{f,_},Arg2,Arg3,{x,0}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 3;
fit_args(l_bs_start_match2, [_,{f,_},Arg2,Arg3,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_start_match2, [_,{f,_},Arg2,_,_]) when Arg2 >= 0, Arg2 =< 255 -> 4;
fit_args(l_bs_test_tail_imm2, [{f,_},_,_]) -> 0;
fit_args(l_bs_test_unit, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_test_unit_8, [{f,_},{x,0}]) -> 0;
fit_args(l_bs_test_unit_8, [{f,_},{x,3}]) -> 1;
fit_args(l_bs_test_unit_8, [{f,_},{x,1}]) -> 3;
fit_args(l_bs_test_unit_8, [{f,_},{x,2}]) -> 2;
fit_args(l_bs_test_unit_8, [{f,_},_]) -> 4;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,1}]) -> 0;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,2}]) -> 1;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,0}]) -> 2;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,3}]) -> 3;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,4}]) -> 4;
fit_args(l_bs_test_zero_tail2, [{f,_},_]) -> 5;
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
fit_args(l_call_ext, [{e,{lists,reverse,1}}]) -> 0;
fit_args(l_call_ext, [{e,{asn1ct_gen,emit,1}}]) -> 1;
fit_args(l_call_ext, [{e,{lists,foreach,2}}]) -> 2;
fit_args(l_call_ext, [{e,{file,close,1}}]) -> 3;
fit_args(l_call_ext, [{e,{file,open,2}}]) -> 4;
fit_args(l_call_ext, [{e,{lists,map,2}}]) -> 5;
fit_args(l_call_ext, [{e,{lists,foldl,3}}]) -> 6;
fit_args(l_call_ext, [{e,{filename,join,2}}]) -> 7;
fit_args(l_call_ext, [{e,{lists,sort,1}}]) -> 8;
fit_args(l_call_ext, [{e,{ordsets,union,2}}]) -> 9;
fit_args(l_call_ext, [{e,{lists,flatten,1}}]) -> 10;
fit_args(l_call_ext, [{e,{test_server,timetrap,1}}]) -> 11;
fit_args(l_call_ext, [{e,{lists,concat,1}}]) -> 12;
fit_args(l_call_ext, [{e,{test_server,timetrap_cancel,1}}]) -> 13;
fit_args(l_call_ext, [{e,{file,delete,1}}]) -> 14;
fit_args(l_call_ext, [{e,{mnesia_lib,set,2}}]) -> 15;
fit_args(l_call_ext, [{e,{test_server,lookup_config,2}}]) -> 16;
fit_args(l_call_ext, [{e,{erl_syntax,type,1}}]) -> 17;
fit_args(l_call_ext, [{e,{lists,mapfoldl,3}}]) -> 18;
fit_args(l_call_ext, [{e,{prettypr,floating,1}}]) -> 19;
fit_args(l_call_ext, [{e,{dict,find,2}}]) -> 23;
fit_args(l_call_ext, [{e,{erlang,list_to_integer,1}}]) -> 22;
fit_args(l_call_ext, [{e,{string,tokens,2}}]) -> 21;
fit_args(l_call_ext, [{e,{proplists,get_value,3}}]) -> 20;
fit_args(l_call_ext, [{e,{filename,join,1}}]) -> 25;
fit_args(l_call_ext, [{e,{lists,filter,2}}]) -> 24;
fit_args(l_call_ext, [{e,{asn1_db,dbget,2}}]) -> 26;
fit_args(l_call_ext, [{e,{proplists,get_value,2}}]) -> 28;
fit_args(l_call_ext, [{e,{prettypr,beside,2}}]) -> 27;
fit_args(l_call_ext, [{e,{dict,new,0}}]) -> 29;
fit_args(l_call_ext, [{e,{sofs,to_external,1}}]) -> 30;
fit_args(l_call_ext, [{e,{ordsets,from_list,1}}]) -> 31;
fit_args(l_call_ext, [{e,{erlang,put,2}}]) -> 33;
fit_args(l_call_ext, [{e,{dict,store,3}}]) -> 32;
fit_args(l_call_ext, [{e,{lists,duplicate,2}}]) -> 34;
fit_args(l_call_ext, [{e,{asn1ct_gen,mk_var,1}}]) -> 37;
fit_args(l_call_ext, [{e,{lists,delete,2}}]) -> 36;
fit_args(l_call_ext, [{e,{gb_trees,empty,0}}]) -> 35;
fit_args(l_call_ext, [{e,{gb_trees,lookup,2}}]) -> 38;
fit_args(l_call_ext, [{e,{file,read_file_info,1}}]) -> 39;
fit_args(l_call_ext, [{e,{io,format,3}}]) -> 40;
fit_args(l_call_ext, [{e,{lists,keydelete,3}}]) -> 41;
fit_args(l_call_ext, [{e,{filename,basename,1}}]) -> 48;
fit_args(l_call_ext, [{e,{erlang,binary_to_term,1}}]) -> 47;
fit_args(l_call_ext, [{e,{lists,last,1}}]) -> 46;
fit_args(l_call_ext, [{e,{asn1ct_gen,get_inner,1}}]) -> 45;
fit_args(l_call_ext, [{e,{erl_syntax,atom,1}}]) -> 44;
fit_args(l_call_ext, [{e,{file,read_file,1}}]) -> 43;
fit_args(l_call_ext, [{e,{ssh_channel,cache_lookup,2}}]) -> 42;
fit_args(l_call_ext, [{e,{file,read,2}}]) -> 50;
fit_args(l_call_ext, [{e,{file,make_dir,1}}]) -> 49;
fit_args(l_call_ext, [{e,{gb_trees,get,2}}]) -> 53;
fit_args(l_call_ext, [{e,{cerl,get_ann,1}}]) -> 52;
fit_args(l_call_ext, [{e,{mnesia_lib,exists,1}}]) -> 51;
fit_args(l_call_ext, [{e,{ordsets,subtract,2}}]) -> 54;
fit_args(l_call_ext, [{e,{erlang,term_to_binary,1}}]) -> 58;
fit_args(l_call_ext, [{e,{file,write,2}}]) -> 57;
fit_args(l_call_ext, [{e,{asn1ct_gen,list2name,1}}]) -> 56;
fit_args(l_call_ext, [{e,{sofs,family_union,2}}]) -> 55;
fit_args(l_call_ext, [{e,{mnesia_lib,cs_to_storage_type,2}}]) -> 65;
fit_args(l_call_ext, [{e,{file,rename,2}}]) -> 64;
fit_args(l_call_ext, [{e,{filename,dirname,1}}]) -> 63;
fit_args(l_call_ext, [{e,{lists,append,1}}]) -> 62;
fit_args(l_call_ext, [{e,{asn1ct_gen,type,1}}]) -> 61;
fit_args(l_call_ext, [{e,{test_server,fail,1}}]) -> 60;
fit_args(l_call_ext, [{e,{random,uniform,1}}]) -> 59;
fit_args(l_call_ext, [{e,{lists,dropwhile,2}}]) -> 68;
fit_args(l_call_ext, [{e,{mnesia_monitor,use_dir,0}}]) -> 67;
fit_args(l_call_ext, [{e,{lists,splitwith,2}}]) -> 66;
fit_args(l_call_ext, [{e,{mnesia_schema,list2cs,1}}]) -> 70;
fit_args(l_call_ext, [{e,{gb_trees,insert,3}}]) -> 69;
fit_args(l_call_ext, [{e,{gb_trees,to_list,1}}]) -> 73;
fit_args(l_call_ext, [{e,{os,type,0}}]) -> 72;
fit_args(l_call_ext, [{e,{erl_syntax,atom_value,1}}]) -> 71;
fit_args(l_call_ext, [{e,{gb_trees,from_orddict,1}}]) -> 76;
fit_args(l_call_ext, [{e,{sets,is_element,2}}]) -> 75;
fit_args(l_call_ext, [{e,{erl_syntax,get_pos,1}}]) -> 74;
fit_args(l_call_ext, [{e,{erlang,max,2}}]) -> 77;
fit_args(l_call_ext, [{e,{file,get_cwd,0}}]) -> 80;
fit_args(l_call_ext, [{e,{lists,sublist,3}}]) -> 79;
fit_args(l_call_ext, [{e,{file,write_file,2}}]) -> 78;
fit_args(l_call_ext, [{e,{ordsets,intersection,2}}]) -> 88;
fit_args(l_call_ext, [{e,{asn1ct_name,new,1}}]) -> 87;
fit_args(l_call_ext, [{e,{beam_utils,code_at,2}}]) -> 86;
fit_args(l_call_ext, [{e,{cerl,var_name,1}}]) -> 85;
fit_args(l_call_ext, [{e,{gb_sets,empty,0}}]) -> 84;
fit_args(l_call_ext, [{e,{xref_utils,xset,2}}]) -> 83;
fit_args(l_call_ext, [{e,{cerl,c_tuple,1}}]) -> 82;
fit_args(l_call_ext, [{e,{mnesia_lib,intersect,2}}]) -> 81;
fit_args(l_call_ext, [{e,_}]) -> 89;
fit_args(l_call_ext_last, [{e,_},1]) -> 0;
fit_args(l_call_ext_last, [{e,_},0]) -> 1;
fit_args(l_call_ext_last, [{e,_},2]) -> 2;
fit_args(l_call_ext_last, [{e,_},3]) -> 3;
fit_args(l_call_ext_last, [{e,_},4]) -> 4;
fit_args(l_call_ext_last, [{e,_},5]) -> 5;
fit_args(l_call_ext_last, [{e,_},_]) -> 6;
fit_args(l_call_ext_only, [{e,{gen_server,call,3}}]) -> 0;
fit_args(l_call_ext_only, [{e,{asn1ct_gen,emit,1}}]) -> 1;
fit_args(l_call_ext_only, [{e,{mnesia_monitor,get_env,1}}]) -> 2;
fit_args(l_call_ext_only, [{e,_}]) -> 3;
fit_args(l_call_fun, [1]) -> 0;
fit_args(l_call_fun, [3]) -> 1;
fit_args(l_call_fun, [2]) -> 2;
fit_args(l_call_fun, [0]) -> 3;
fit_args(l_call_fun, [Arg0]) when Arg0 >= 0, Arg0 =< 255 -> 4;
fit_args(l_call_fun_last, [Arg0,Arg1]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_call_fun_last, [Arg0,_]) when Arg0 >= 0, Arg0 =< 255 -> 1;
fit_args(l_call_last, [{f,_},2]) -> 0;
fit_args(l_call_last, [{f,_},0]) -> 1;
fit_args(l_call_last, [{f,_},3]) -> 2;
fit_args(l_call_last, [{f,_},4]) -> 3;
fit_args(l_call_last, [{f,_},1]) -> 4;
fit_args(l_call_last, [{f,_},6]) -> 5;
fit_args(l_call_last, [{f,_},5]) -> 6;
fit_args(l_call_last, [{f,_},7]) -> 7;
fit_args(l_call_last, [{f,_},8]) -> 8;
fit_args(l_call_last, [{f,_},9]) -> 9;
fit_args(l_call_last, [{f,_},10]) -> 10;
fit_args(l_call_last, [{f,_},_]) -> 11;
fit_args(l_call_only, [{f,_}]) -> 0;
fit_args(l_catch, [{y,0},_]) -> 0;
fit_args(l_catch, [{y,1},_]) -> 1;
fit_args(l_catch, [{y,2},_]) -> 2;
fit_args(l_catch, [{y,3},_]) -> 3;
fit_args(l_catch, [{y,4},_]) -> 4;
fit_args(l_catch, [{y,5},_]) -> 5;
fit_args(l_catch, [{y,6},_]) -> 6;
fit_args(l_catch, [{y,20},_]) -> 7;
fit_args(l_catch, [_,_]) -> 8;
fit_args(l_element, [_,{x,0},{x,1}]) -> 0;
fit_args(l_element, [_,_,_]) -> 1;
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
fit_args(l_fetch, [{i,Arg0},{x,_}]) when Arg0 >= -128, Arg0 =< 127 -> 5;
fit_args(l_fetch, [{i,Arg0},{y,Arg1}]) when Arg0 >= -128, Arg0 =< 127, Arg1 >= 0, Arg1 =< 255 -> 10;
fit_args(l_fetch, [{x,0},_]) -> 0;
fit_args(l_fetch, [{x,1},_]) -> 11;
fit_args(l_fetch, [{x,4},_]) -> 12;
fit_args(l_fetch, [{x,3},_]) -> 13;
fit_args(l_fetch, [{x,2},_]) -> 14;
fit_args(l_fetch, [{x,5},_]) -> 17;
fit_args(l_fetch, [{y,0},_]) -> 19;
fit_args(l_fetch, [{x,_},{i,Arg1}]) when Arg1 >= -128, Arg1 =< 127 -> 3;
fit_args(l_fetch, [{y,Arg0},{i,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= -128, Arg1 =< 127 -> 9;
fit_args(l_fetch, [{x,_},{x,_}]) -> 2;
fit_args(l_fetch, [{x,_},{y,Arg1}]) when Arg1 >= 0, Arg1 =< 255 -> 4;
fit_args(l_fetch, [{y,Arg0},{y,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 6;
fit_args(l_fetch, [_,{x,0}]) -> 1;
fit_args(l_fetch, [_,{x,1}]) -> 8;
fit_args(l_fetch, [_,{x,4}]) -> 15;
fit_args(l_fetch, [_,{x,3}]) -> 16;
fit_args(l_fetch, [_,{x,2}]) -> 18;
fit_args(l_fetch, [_,{y,5}]) -> 20;
fit_args(l_fetch, [_,{x,5}]) -> 21;
fit_args(l_fetch, [_,_]) -> 22;
fit_args(l_fmul, [{fr,_},{fr,_},{fr,_}]) -> 0;
fit_args(l_fnegate, [{fr,_},{fr,_}]) -> 0;
fit_args(l_fsub, [{fr,_},{fr,_},{fr,_}]) -> 0;
fit_args(l_gc_bif1, [{f,_},{b,{erlang,byte_size,1}},{x,1},2,{x,0}]) -> 3;
fit_args(l_gc_bif1, [{f,_},{b,{erlang,length,1}},{x,_},Arg3,{y,Arg4}]) when Arg3 >= 0, Arg3 =< 255, Arg4 >= 0, Arg4 =< 255 -> 4;
fit_args(l_gc_bif1, [{f,_},{b,{erlang,length,1}},_,Arg3,{x,0}]) when Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_gc_bif1, [{f,_},{b,_},{x,0},1,{x,0}]) -> 2;
fit_args(l_gc_bif1, [{f,_},{b,_},{x,0},Arg3,{y,Arg4}]) when Arg3 >= 0, Arg3 =< 255, Arg4 >= 0, Arg4 =< 255 -> 6;
fit_args(l_gc_bif1, [{f,_},{b,_},_,Arg3,{x,0}]) when Arg3 >= 0, Arg3 =< 255 -> 5;
fit_args(l_gc_bif1, [{f,_},{b,_},_,Arg3,{x,_}]) when Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_gc_bif1, [{f,_},{b,_},_,Arg3,_]) when Arg3 >= 0, Arg3 =< 255 -> 7;
fit_args(l_gc_bif2, [{f,_},{b,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_gc_bif3, [{f,_},{b,_},_,Arg3,_]) when Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_get, [{a,asn1_module},{y,0}]) -> 0;
fit_args(l_get, [{a,asn1_module},{y,Arg1}]) when Arg1 >= 0, Arg1 =< 255 -> 4;
fit_args(l_get, [{a,mnesia_activity_state},_]) -> 5;
fit_args(l_get, [_,{x,1}]) -> 1;
fit_args(l_get, [_,{x,0}]) -> 2;
fit_args(l_get, [_,{x,2}]) -> 3;
fit_args(l_get, [_,_]) -> 6;
fit_args(l_hibernate, []) -> 0;
fit_args(l_increment, [{x,0},Arg1,Arg2,{x,_}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 5;
fit_args(l_increment, [{x,0},_,Arg2,{x,0}]) when Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(l_increment, [{y,Arg0},4294967295,Arg2,{x,0}]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 4;
fit_args(l_increment, [{x,_},4294967295,Arg2,{x,0}]) when Arg2 >= 0, Arg2 =< 255 -> 6;
fit_args(l_increment, [{x,_},Arg1,Arg2,_]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_increment, [{y,Arg0},Arg1,Arg2,_]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_increment, [{x,_},_,Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 7;
fit_args(l_increment, [_,4294967295,Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_increment, [_,_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 8;
fit_args(l_int_bnot, [{f,_},_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_int_div, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_int_div, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_int_div, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_is_eq, [{f,_}]) -> 0;
fit_args(l_is_eq_exact, [{f,_}]) -> 0;
fit_args(l_is_eq_exact_immed, [{f,_},{x,255},{a,xmerl_scanner}]) -> 10;
fit_args(l_is_eq_exact_immed, [{f,_},{y,10},{a,ber}]) -> 21;
fit_args(l_is_eq_exact_immed, [{f,_},{x,0},_]) -> 0;
fit_args(l_is_eq_exact_immed, [{f,_},{x,1},_]) -> 1;
fit_args(l_is_eq_exact_immed, [{f,_},{x,3},_]) -> 2;
fit_args(l_is_eq_exact_immed, [{f,_},{x,2},_]) -> 4;
fit_args(l_is_eq_exact_immed, [{f,_},{x,4},_]) -> 5;
fit_args(l_is_eq_exact_immed, [{f,_},{x,5},_]) -> 6;
fit_args(l_is_eq_exact_immed, [{f,_},{x,6},_]) -> 7;
fit_args(l_is_eq_exact_immed, [{f,_},{x,7},_]) -> 8;
fit_args(l_is_eq_exact_immed, [{f,_},{x,8},_]) -> 9;
fit_args(l_is_eq_exact_immed, [{f,_},{x,9},_]) -> 11;
fit_args(l_is_eq_exact_immed, [{f,_},{x,10},_]) -> 12;
fit_args(l_is_eq_exact_immed, [{f,_},{x,11},_]) -> 13;
fit_args(l_is_eq_exact_immed, [{f,_},{x,12},_]) -> 14;
fit_args(l_is_eq_exact_immed, [{f,_},{y,0},_]) -> 16;
fit_args(l_is_eq_exact_immed, [{f,_},{y,1},_]) -> 17;
fit_args(l_is_eq_exact_immed, [{f,_},{x,255},_]) -> 18;
fit_args(l_is_eq_exact_immed, [{f,_},{x,13},_]) -> 19;
fit_args(l_is_eq_exact_immed, [{f,_},{y,3},_]) -> 20;
fit_args(l_is_eq_exact_immed, [{f,_},{x,14},_]) -> 22;
fit_args(l_is_eq_exact_immed, [{f,_},{y,2},_]) -> 23;
fit_args(l_is_eq_exact_immed, [{f,_},{x,15},_]) -> 24;
fit_args(l_is_eq_exact_immed, [{f,_},{x,16},_]) -> 25;
fit_args(l_is_eq_exact_immed, [{f,_},{y,4},_]) -> 26;
fit_args(l_is_eq_exact_immed, [{f,_},{x,17},_]) -> 27;
fit_args(l_is_eq_exact_immed, [{f,_},{y,5},_]) -> 28;
fit_args(l_is_eq_exact_immed, [{f,_},{x,18},_]) -> 29;
fit_args(l_is_eq_exact_immed, [{f,_},{x,19},_]) -> 30;
fit_args(l_is_eq_exact_immed, [{f,_},{x,22},_]) -> 31;
fit_args(l_is_eq_exact_immed, [{f,_},{x,23},_]) -> 32;
fit_args(l_is_eq_exact_immed, [{f,_},{x,20},_]) -> 33;
fit_args(l_is_eq_exact_immed, [{f,_},{y,6},_]) -> 34;
fit_args(l_is_eq_exact_immed, [{f,_},{y,7},_]) -> 35;
fit_args(l_is_eq_exact_immed, [{f,_},{x,_},{i,Arg2}]) when Arg2 >= -128, Arg2 =< 127 -> 3;
fit_args(l_is_eq_exact_immed, [{f,_},{y,Arg1},{a,asn1_NOVALUE}]) when Arg1 >= 0, Arg1 =< 255 -> 15;
fit_args(l_is_eq_exact_immed, [{f,_},_,_]) -> 36;
fit_args(l_is_eq_exact_literal, [{f,_},{x,0},_]) -> 0;
fit_args(l_is_eq_exact_literal, [{f,_},{x,1},_]) -> 1;
fit_args(l_is_eq_exact_literal, [{f,_},{x,4},_]) -> 2;
fit_args(l_is_eq_exact_literal, [{f,_},{x,2},_]) -> 3;
fit_args(l_is_eq_exact_literal, [{f,_},{x,3},_]) -> 4;
fit_args(l_is_eq_exact_literal, [{f,_},{x,6},_]) -> 5;
fit_args(l_is_eq_exact_literal, [{f,_},{x,5},_]) -> 6;
fit_args(l_is_eq_exact_literal, [{f,_},_,_]) -> 7;
fit_args(l_is_function2, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_is_ge, [{f,_}]) -> 0;
fit_args(l_is_lt, [{f,_}]) -> 0;
fit_args(l_is_ne, [{f,_}]) -> 0;
fit_args(l_is_ne_exact, [{f,_}]) -> 0;
fit_args(l_is_ne_exact_immed, [{f,_},{x,0},_]) -> 0;
fit_args(l_is_ne_exact_immed, [{f,_},{x,1},_]) -> 1;
fit_args(l_is_ne_exact_immed, [{f,_},{x,2},_]) -> 2;
fit_args(l_is_ne_exact_immed, [{f,_},{x,3},_]) -> 4;
fit_args(l_is_ne_exact_immed, [{f,_},{x,4},_]) -> 5;
fit_args(l_is_ne_exact_immed, [{f,_},{y,0},_]) -> 6;
fit_args(l_is_ne_exact_immed, [{f,_},{y,1},_]) -> 7;
fit_args(l_is_ne_exact_immed, [{f,_},{y,2},_]) -> 8;
fit_args(l_is_ne_exact_immed, [{f,_},{x,5},_]) -> 9;
fit_args(l_is_ne_exact_immed, [{f,_},_,{a,true}]) -> 3;
fit_args(l_is_ne_exact_immed, [{f,_},_,_]) -> 10;
fit_args(l_is_ne_exact_literal, [{f,_},_,_]) -> 0;
fit_args(l_jump_on_val, [{x,0},{f,_},Arg2,Arg3]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_jump_on_val, [{x,_},{f,_},Arg2,Arg3]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_jump_on_val, [_,{f,_},_,_]) -> 2;
fit_args(l_loop_rec, [{f,_}]) -> 0;
fit_args(l_m_div, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_make_export, [{e,_}]) -> 0;
fit_args(l_make_fun, [{fu,_}]) -> 0;
fit_args(l_minus, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_minus, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_minus, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_move_call, [{x,2},{f,_}]) -> 0;
fit_args(l_move_call, [{x,1},{f,_}]) -> 1;
fit_args(l_move_call, [{y,0},{f,_}]) -> 2;
fit_args(l_move_call, [{y,2},{f,_}]) -> 3;
fit_args(l_move_call, [{y,1},{f,_}]) -> 4;
fit_args(l_move_call, [{y,3},{f,_}]) -> 5;
fit_args(l_move_call, [{x,3},{f,_}]) -> 6;
fit_args(l_move_call, [{y,5},{f,_}]) -> 7;
fit_args(l_move_call, [{y,4},{f,_}]) -> 8;
fit_args(l_move_call, [{y,6},{f,_}]) -> 10;
fit_args(l_move_call, [{x,4},{f,_}]) -> 11;
fit_args(l_move_call, [nil,{f,_}]) -> 12;
fit_args(l_move_call, [{x,5},{f,_}]) -> 13;
fit_args(l_move_call, [{y,7},{f,_}]) -> 14;
fit_args(l_move_call, [{y,12},{f,_}]) -> 19;
fit_args(l_move_call, [{a,foo},{f,_}]) -> 20;
fit_args(l_move_call, [{a,false},{f,_}]) -> 21;
fit_args(l_move_call, [{a,endDocument},{f,_}]) -> 22;
fit_args(l_move_call, [{x,6},{f,_}]) -> 24;
fit_args(l_move_call, [{a,ets},{f,_}]) -> 27;
fit_args(l_move_call, [{a,schema},{f,_}]) -> 26;
fit_args(l_move_call, [{a,x},{f,_}]) -> 29;
fit_args(l_move_call, [{y,8},{f,_}]) -> 31;
fit_args(l_move_call, [{smallint,1},{f,_}]) -> 9;
fit_args(l_move_call, [{smallint,0},{f,_}]) -> 15;
fit_args(l_move_call, [{smallint,3},{f,_}]) -> 16;
fit_args(l_move_call, [{smallint,2},{f,_}]) -> 18;
fit_args(l_move_call, [{smallint,12},{f,_}]) -> 17;
fit_args(l_move_call, [{smallint,9},{f,_}]) -> 23;
fit_args(l_move_call, [{smallint,6},{f,_}]) -> 25;
fit_args(l_move_call, [{smallint,100},{f,_}]) -> 28;
fit_args(l_move_call, [{smallint,1000},{f,_}]) -> 30;
fit_args(l_move_call, [{smallint,4},{f,_}]) -> 33;
fit_args(l_move_call, [{smallint,42},{f,_}]) -> 32;
fit_args(l_move_call, [_,{f,_}]) -> 34;
fit_args(l_move_call_ext, [{a,funky},{e,{estone_SUITE,req,2}}]) -> 27;
fit_args(l_move_call_ext, [{a,auto_repair},{e,{mnesia_monitor,get_env,1}}]) -> 34;
fit_args(l_move_call_ext, [{y,0},{e,_}]) -> 3;
fit_args(l_move_call_ext, [{y,1},{e,_}]) -> 4;
fit_args(l_move_call_ext, [{y,2},{e,_}]) -> 5;
fit_args(l_move_call_ext, [{y,3},{e,_}]) -> 7;
fit_args(l_move_call_ext, [{y,4},{e,_}]) -> 8;
fit_args(l_move_call_ext, [{x,2},{e,_}]) -> 10;
fit_args(l_move_call_ext, [{x,1},{e,_}]) -> 11;
fit_args(l_move_call_ext, [{y,6},{e,_}]) -> 13;
fit_args(l_move_call_ext, [{y,5},{e,_}]) -> 16;
fit_args(l_move_call_ext, [nil,{e,_}]) -> 19;
fit_args(l_move_call_ext, [{x,3},{e,_}]) -> 22;
fit_args(l_move_call_ext, [{y,7},{e,_}]) -> 24;
fit_args(l_move_call_ext, [{x,4},{e,_}]) -> 37;
fit_args(l_move_call_ext, [{a,schema},{e,_}]) -> 38;
fit_args(l_move_call_ext, [{a,func},{e,_}]) -> 39;
fit_args(l_move_call_ext, [{smallint,0},{e,{lists,seq,2}}]) -> 15;
fit_args(l_move_call_ext, [{smallint,1},{e,_}]) -> 9;
fit_args(l_move_call_ext, [{smallint,2},{e,_}]) -> 40;
fit_args(l_move_call_ext, [_,{e,{re,split,3}}]) -> 0;
fit_args(l_move_call_ext, [_,{e,{re,replace,4}}]) -> 1;
fit_args(l_move_call_ext, [_,{e,{io,format,2}}]) -> 2;
fit_args(l_move_call_ext, [_,{e,{asn1ct_gen,emit,1}}]) -> 6;
fit_args(l_move_call_ext, [_,{e,{io_lib,format,2}}]) -> 12;
fit_args(l_move_call_ext, [_,{e,{erlang,put,2}}]) -> 14;
fit_args(l_move_call_ext, [_,{e,{prettypr,text,1}}]) -> 17;
fit_args(l_move_call_ext, [_,{e,{asn1ct_name,new,1}}]) -> 18;
fit_args(l_move_call_ext, [_,{e,{proplists,get_value,2}}]) -> 20;
fit_args(l_move_call_ext, [_,{e,{proplists,get_value,3}}]) -> 21;
fit_args(l_move_call_ext, [_,{e,{test_server,seconds,1}}]) -> 23;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,verbose,2}}]) -> 25;
fit_args(l_move_call_ext, [_,{e,{erlang,system_info,1}}]) -> 26;
fit_args(l_move_call_ext, [_,{e,{lists,seq,2}}]) -> 28;
fit_args(l_move_call_ext, [_,{e,{lists,duplicate,2}}]) -> 29;
fit_args(l_move_call_ext, [_,{e,{erlang,binary_to_term,1}}]) -> 30;
fit_args(l_move_call_ext, [_,{e,{test_server,lookup_config,2}}]) -> 32;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,dbg_out,2}}]) -> 31;
fit_args(l_move_call_ext, [_,{e,{erlang,erase,1}}]) -> 33;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,set,2}}]) -> 35;
fit_args(l_move_call_ext, [_,{e,{lists,sublist,3}}]) -> 36;
fit_args(l_move_call_ext, [_,{e,_}]) -> 41;
fit_args(l_move_call_ext_last, [{e,_},0,_]) -> 1;
fit_args(l_move_call_ext_last, [{e,_},1,_]) -> 2;
fit_args(l_move_call_ext_last, [{e,_},2,_]) -> 3;
fit_args(l_move_call_ext_last, [{e,_},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_move_call_ext_last, [{e,_},_,_]) -> 4;
fit_args(l_move_call_ext_only, [{e,{erlang,get_module_info,1}},_]) -> 0;
fit_args(l_move_call_ext_only, [{e,{io_lib,format,2}},_]) -> 1;
fit_args(l_move_call_ext_only, [{e,{lists,reverse,1}},_]) -> 2;
fit_args(l_move_call_ext_only, [{e,{io,format,2}},_]) -> 5;
fit_args(l_move_call_ext_only, [{e,_},{x,2}]) -> 3;
fit_args(l_move_call_ext_only, [{e,_},{x,1}]) -> 4;
fit_args(l_move_call_ext_only, [{e,_},_]) -> 6;
fit_args(l_move_call_last, [{f,_},1,_]) -> 1;
fit_args(l_move_call_last, [{f,_},2,_]) -> 2;
fit_args(l_move_call_last, [{f,_},0,_]) -> 3;
fit_args(l_move_call_last, [{f,_},3,_]) -> 4;
fit_args(l_move_call_last, [{f,_},4,_]) -> 5;
fit_args(l_move_call_last, [{f,_},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_move_call_last, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 6;
fit_args(l_move_call_last, [{f,_},_,_]) -> 7;
fit_args(l_move_call_only, [{f,_},{x,1}]) -> 0;
fit_args(l_move_call_only, [{f,_},{x,2}]) -> 1;
fit_args(l_move_call_only, [{f,_},{x,4}]) -> 2;
fit_args(l_move_call_only, [{f,_},{x,3}]) -> 3;
fit_args(l_move_call_only, [{f,_},{x,5}]) -> 4;
fit_args(l_move_call_only, [{f,_},{x,6}]) -> 5;
fit_args(l_move_call_only, [{f,_},nil]) -> 6;
fit_args(l_move_call_only, [{f,_},{x,7}]) -> 7;
fit_args(l_move_call_only, [{f,_},{x,8}]) -> 8;
fit_args(l_move_call_only, [{f,_},_]) -> 9;
fit_args(l_new_bs_put_binary, [{f,_},_,Arg2,Arg3,_]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_new_bs_put_binary_all, [{f,_},{x,0},8]) -> 0;
fit_args(l_new_bs_put_binary_all, [{f,_},_,8]) -> 1;
fit_args(l_new_bs_put_binary_all, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_new_bs_put_binary_imm, [{f,_},_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_new_bs_put_float, [{f,_},_,Arg2,Arg3,_]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_new_bs_put_float_imm, [{f,_},64,0,{x,0}]) -> 0;
fit_args(l_new_bs_put_float_imm, [{f,_},_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_new_bs_put_integer, [{f,_},_,1,0,_]) -> 0;
fit_args(l_new_bs_put_integer, [{f,_},_,Arg2,Arg3,_]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_new_bs_put_integer_imm, [{f,_},_,0,_]) -> 0;
fit_args(l_new_bs_put_integer_imm, [{f,_},_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_plus, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_plus, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_plus, [{f,_},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_plus, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 3;
fit_args(l_put_tuple, [{x,0},2]) -> 1;
fit_args(l_put_tuple, [{x,0},3]) -> 2;
fit_args(l_put_tuple, [{x,0},4]) -> 3;
fit_args(l_put_tuple, [{x,0},5]) -> 4;
fit_args(l_put_tuple, [{x,0},_]) -> 5;
fit_args(l_put_tuple, [{x,_},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_put_tuple, [{y,Arg0},Arg1]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 6;
fit_args(l_put_tuple, [_,_]) -> 7;
fit_args(l_recv_set, []) -> 0;
fit_args(l_rem, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_rem, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_rem, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_select_tuple_arity, [{x,0},{f,_},8]) -> 1;
fit_args(l_select_tuple_arity, [{x,0},{f,_},10]) -> 2;
fit_args(l_select_tuple_arity, [{x,_},{f,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(l_select_tuple_arity, [_,{f,_},6]) -> 0;
fit_args(l_select_tuple_arity, [_,{f,_},_]) -> 4;
fit_args(l_select_tuple_arity2, [{x,0},{f,_},Arg2,{f,_},Arg4,{f,_}]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 0;
fit_args(l_select_tuple_arity2, [{x,_},{f,_},Arg2,{f,_},Arg4,{f,_}]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 1;
fit_args(l_select_tuple_arity2, [{y,Arg0},{f,_},Arg2,{f,_},Arg4,{f,_}]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 2;
fit_args(l_select_tuple_arity2, [_,{f,_},_,{f,_},_,{f,_}]) -> 3;
fit_args(l_select_val2, [{x,0},{f,_},_,{f,_},_,{f,_}]) -> 0;
fit_args(l_select_val2, [{x,1},{f,_},_,{f,_},_,{f,_}]) -> 2;
fit_args(l_select_val2, [{x,2},{f,_},_,{f,_},_,{f,_}]) -> 3;
fit_args(l_select_val2, [{x,3},{f,_},_,{f,_},_,{f,_}]) -> 4;
fit_args(l_select_val2, [{x,4},{f,_},_,{f,_},_,{f,_}]) -> 7;
fit_args(l_select_val2, [{x,5},{f,_},_,{f,_},_,{f,_}]) -> 8;
fit_args(l_select_val2, [{x,6},{f,_},_,{f,_},_,{f,_}]) -> 9;
fit_args(l_select_val2, [{x,7},{f,_},_,{f,_},_,{f,_}]) -> 10;
fit_args(l_select_val2, [{y,1},{f,_},_,{f,_},_,{f,_}]) -> 11;
fit_args(l_select_val2, [{x,8},{f,_},_,{f,_},_,{f,_}]) -> 12;
fit_args(l_select_val2, [{y,2},{f,_},_,{f,_},_,{f,_}]) -> 13;
fit_args(l_select_val2, [{x,_},{f,_},{i,Arg2},{f,_},{i,Arg4},{f,_}]) when Arg2 >= -128, Arg2 =< 127, Arg4 >= -128, Arg4 =< 127 -> 1;
fit_args(l_select_val2, [_,{f,_},{a,true},{f,_},{a,false},{f,_}]) -> 5;
fit_args(l_select_val2, [_,{f,_},{a,false},{f,_},{a,true},{f,_}]) -> 6;
fit_args(l_select_val2, [_,{f,_},_,{f,_},_,{f,_}]) -> 14;
fit_args(l_select_val_atoms, [{x,0},{f,_},_]) -> 1;
fit_args(l_select_val_atoms, [{x,_},{f,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_select_val_atoms, [_,{f,_},_]) -> 2;
fit_args(l_select_val_smallints, [{x,0},{f,_},_]) -> 1;
fit_args(l_select_val_smallints, [{x,_},{f,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_select_val_smallints, [_,{f,_},_]) -> 2;
fit_args(l_times, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_times, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_times, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_trim, [1]) -> 0;
fit_args(l_trim, [2]) -> 1;
fit_args(l_trim, [3]) -> 2;
fit_args(l_trim, [4]) -> 3;
fit_args(l_trim, [5]) -> 4;
fit_args(l_trim, [6]) -> 5;
fit_args(l_trim, [7]) -> 6;
fit_args(l_trim, [8]) -> 7;
fit_args(l_trim, [9]) -> 8;
fit_args(l_trim, [11]) -> 9;
fit_args(l_trim, [10]) -> 10;
fit_args(l_trim, [_]) -> 11;
fit_args(l_wait_timeout, [{f,_},1000]) -> 0;
fit_args(l_wait_timeout, [{f,_},1]) -> 1;
fit_args(l_wait_timeout, [{f,_},_]) -> 2;
fit_args(l_yield, []) -> 0;
fit_args(loop_rec_end, [{f,_}]) -> 0;
fit_args(move, [nil,{x,10}]) -> 11;
fit_args(move, [{x,0},_]) -> 4;
fit_args(move, [{x,_},{y,Arg1}]) when Arg1 >= 0, Arg1 =< 255 -> 3;
fit_args(move, [{y,Arg0},{y,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 7;
fit_args(move, [_,{x,1}]) -> 0;
fit_args(move, [_,{x,0}]) -> 1;
fit_args(move, [_,{x,2}]) -> 2;
fit_args(move, [_,{x,3}]) -> 5;
fit_args(move, [_,{x,4}]) -> 6;
fit_args(move, [_,{x,5}]) -> 8;
fit_args(move, [_,{x,6}]) -> 9;
fit_args(move, [_,{x,7}]) -> 10;
fit_args(move, [_,{x,8}]) -> 12;
fit_args(move, [_,_]) -> 13;
fit_args(move2, [{x,0},{x,_},{x,0},{x,_}]) -> 9;
fit_args(move2, [{x,0},{x,_},{x,_},{x,0}]) -> 7;
fit_args(move2, [{x,0},{y,Arg1},{x,_},{y,Arg3}]) when Arg1 >= 0, Arg1 =< 255, Arg3 >= 0, Arg3 =< 255 -> 4;
fit_args(move2, [{x,0},{x,_},{x,_},{x,_}]) -> 8;
fit_args(move2, [{x,_},{y,Arg1},{x,0},{y,Arg3}]) when Arg1 >= 0, Arg1 =< 255, Arg3 >= 0, Arg3 =< 255 -> 2;
fit_args(move2, [{x,_},{x,_},{x,0},{x,_}]) -> 6;
fit_args(move2, [{x,_},{x,_},{x,_},{x,0}]) -> 5;
fit_args(move2, [{x,_},{y,Arg1},{x,_},{y,Arg3}]) when Arg1 >= 0, Arg1 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(move2, [{y,Arg0},{x,_},{y,Arg2},_]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(move2, [{x,_},_,{x,_},{x,_}]) -> 3;
fit_args(move2, [_,_,_,_]) -> 10;
fit_args(move_deallocate_return, [{a,ok},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(move_deallocate_return, [{x,_},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(move_deallocate_return, [{y,Arg0},Arg1]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(move_deallocate_return, [_,0]) -> 3;
fit_args(move_deallocate_return, [_,1]) -> 4;
fit_args(move_deallocate_return, [_,2]) -> 5;
fit_args(move_deallocate_return, [_,3]) -> 6;
fit_args(move_deallocate_return, [_,4]) -> 7;
fit_args(move_deallocate_return, [_,5]) -> 8;
fit_args(move_deallocate_return, [_,_]) -> 9;
fit_args(move_jump, [{f,_},{x,1}]) -> 0;
fit_args(move_jump, [{f,_},{x,2}]) -> 1;
fit_args(move_jump, [{f,_},nil]) -> 2;
fit_args(move_jump, [{f,_},{x,4}]) -> 3;
fit_args(move_jump, [{f,_},{y,0}]) -> 4;
fit_args(move_jump, [{f,_},{y,2}]) -> 6;
fit_args(move_jump, [{f,_},{y,1}]) -> 5;
fit_args(move_jump, [{f,_},{x,3}]) -> 7;
fit_args(move_jump, [{f,_},{a,false}]) -> 8;
fit_args(move_jump, [{f,_},{a,asn1_NOVALUE}]) -> 9;
fit_args(move_jump, [{f,_},{a,true}]) -> 10;
fit_args(move_jump, [{f,_},{smallint,0}]) -> 11;
fit_args(move_jump, [{f,_},_]) -> 12;
fit_args(move_return, [{a,true}]) -> 0;
fit_args(move_return, [{x,1}]) -> 1;
fit_args(move_return, [{a,ok}]) -> 2;
fit_args(move_return, [{x,2}]) -> 3;
fit_args(move_return, [nil]) -> 4;
fit_args(move_return, [{a,false}]) -> 5;
fit_args(move_return, [{x,3}]) -> 6;
fit_args(move_return, [{x,4}]) -> 7;
fit_args(move_return, [{a,no}]) -> 13;
fit_args(move_return, [{a,undefined}]) -> 15;
fit_args(move_return, [{a,error}]) -> 17;
fit_args(move_return, [{x,5}]) -> 16;
fit_args(move_return, [{a,ignore}]) -> 18;
fit_args(move_return, [{a,none}]) -> 20;
fit_args(move_return, [{a,nomatch}]) -> 24;
fit_args(move_return, [{x,6}]) -> 26;
fit_args(move_return, [{smallint,1}]) -> 8;
fit_args(move_return, [{smallint,2}]) -> 9;
fit_args(move_return, [{smallint,0}]) -> 10;
fit_args(move_return, [{smallint,3}]) -> 11;
fit_args(move_return, [{smallint,4}]) -> 12;
fit_args(move_return, [{smallint,8}]) -> 14;
fit_args(move_return, [{smallint,5}]) -> 19;
fit_args(move_return, [{smallint,16}]) -> 22;
fit_args(move_return, [{smallint,6}]) -> 21;
fit_args(move_return, [{smallint,7}]) -> 23;
fit_args(move_return, [{smallint,64}]) -> 25;
fit_args(move_return, [{smallint,128}]) -> 27;
fit_args(move_return, [_]) -> 28;
fit_args(node, [{x,0}]) -> 0;
fit_args(node, [{x,1}]) -> 1;
fit_args(node, [{x,2}]) -> 2;
fit_args(node, [{x,3}]) -> 3;
fit_args(node, [_]) -> 4;
fit_args(on_load, []) -> 0;
fit_args(put_list, [{x,0},_,{y,0}]) -> 6;
fit_args(put_list, [{x,0},_,_]) -> 10;
fit_args(put_list, [{smallint,10},{x,0},_]) -> 7;
fit_args(put_list, [{x,_},_,{x,_}]) -> 4;
fit_args(put_list, [{y,Arg0},_,{x,_}]) when Arg0 >= 0, Arg0 =< 255 -> 8;
fit_args(put_list, [{x,_},_,{y,Arg2}]) when Arg2 >= 0, Arg2 =< 255 -> 9;
fit_args(put_list, [{y,Arg0},_,{y,Arg2}]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 11;
fit_args(put_list, [_,_,{x,0}]) -> 0;
fit_args(put_list, [_,_,{x,1}]) -> 1;
fit_args(put_list, [_,_,{x,2}]) -> 2;
fit_args(put_list, [_,_,{x,3}]) -> 3;
fit_args(put_list, [_,_,{x,4}]) -> 5;
fit_args(put_list, [_,_,_]) -> 12;
fit_args(raise, [{x,2},{x,1}]) -> 0;
fit_args(raise, [_,_]) -> 1;
fit_args(recv_mark, [{f,_}]) -> 0;
fit_args(remove_message, []) -> 0;
fit_args(return, []) -> 0;
fit_args(self, [{x,0}]) -> 0;
fit_args(self, [{x,1}]) -> 1;
fit_args(self, [{x,2}]) -> 2;
fit_args(self, [{x,3}]) -> 3;
fit_args(self, [{y,0}]) -> 4;
fit_args(self, [_]) -> 5;
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
fit_args(try_end, [{y,1}]) -> 0;
fit_args(try_end, [{y,0}]) -> 1;
fit_args(try_end, [{y,2}]) -> 2;
fit_args(try_end, [{y,4}]) -> 4;
fit_args(try_end, [{y,5}]) -> 3;
fit_args(try_end, [{y,3}]) -> 5;
fit_args(try_end, [{y,6}]) -> 6;
fit_args(try_end, [_]) -> 7;
fit_args(wait, [{f,_}]) -> 0;
fit_args(wait_timeout, [{f,_},_]) -> 0;
fit_args(Op, As) -> erlang:error({nofit,Op,As}).

var_args(move, 0) -> [t,{x,1}];
var_args(move, 1) -> [t,{x,0}];
var_args(l_call, 0) -> [f];
var_args(test_heap, 0) -> [u8,u8];
var_args(move, 2) -> [t,{x,2}];
var_args(badmatch, 0) -> [{x,0}];
var_args(move, 3) -> [x8,y8];
var_args(l_put_tuple, 0) -> [x8,u8];
var_args(move, 4) -> [{x,0},t];
var_args(move2, 1) -> [y8,x8,y8,t];
var_args(get_tuple_element, 0) -> [{x,0},u8,x8];
var_args(call_bif, 7) -> [{b,{erlang,iolist_to_binary,1}}];
var_args(l_bs_start_match2, 0) -> [{x,0},f,{u8,1},{u,0},{x,1}];
var_args(l_bs_test_zero_tail2, 0) -> [f,{x,1}];
var_args(l_bs_match_string, 0) -> [{x,1},f,u32,str];
var_args(put_list, 0) -> [t,t,{x,0}];
var_args(is_tuple_of_arity, 1) -> [f,x8,u8];
var_args(get_tuple_element, 1) -> [x8,u8,x8];
var_args(move2, 0) -> [x8,y8,x8,y8];
var_args(l_call_only, 0) -> [f];
var_args(l_is_eq_exact_immed, 0) -> [f,{x,0},t];
var_args(l_is_eq_exact_immed, 1) -> [f,{x,1},t];
var_args(is_tuple_of_arity, 0) -> [f,{x,0},{u,2}];
var_args(l_move_call_ext, 0) -> [t,{e,{re,split,3}}];
var_args(put_list, 1) -> [t,t,{x,1}];
var_args(move, 5) -> [t,{x,3}];
var_args(get_list, 0) -> [x8,x8,x8];
var_args(l_put_tuple, 1) -> [{x,0},{u,2}];
var_args(l_call_ext, 89) -> [e];
var_args(l_is_ge, 0) -> [f];
var_args(l_make_fun, 0) -> [fu];
var_args(l_move_call_last, 0) -> [f,u8,y8];
var_args(extract_next_element2, 0) -> [{x,1}];
var_args(is_tuple_of_arity, 2) -> [f,{x,0},u32];
var_args(return, 0) -> [];
var_args(l_move_call_ext, 1) -> [t,{e,{re,replace,4}}];
var_args(l_fetch, 0) -> [{x,0},t];
var_args(call_bif, 3) -> [{b,{erlang,error,1}}];
var_args(move_deallocate_return, 0) -> [x8,u8];
var_args(l_trim, 0) -> [{u,1}];
var_args(l_allocate, 0) -> [{u,1}];
var_args(move2, 3) -> [x8,t,x8,x8];
var_args(l_fetch, 1) -> [t,{x,0}];
var_args(put_list, 2) -> [t,t,{x,2}];
var_args(is_nil, 0) -> [f,{x,0}];
var_args(is_nonempty_list, 0) -> [f,{x,0}];
var_args(extract_next_element, 0) -> [{x,1}];
var_args(move_return, 28) -> [t];
var_args(l_allocate, 1) -> [{u,0}];
var_args(l_is_eq_exact_immed, 2) -> [f,{x,3},t];
var_args(jump, 0) -> [f];
var_args(deallocate_return, 0) -> [{u,1}];
var_args(get_list, 1) -> [{x,0},x8,x8];
var_args(case_end, 0) -> [{x,0}];
var_args(call_bif, 8) -> [{b,{erlang,setelement,3}}];
var_args(move2, 2) -> [x8,y8,{x,0},y8];
var_args(l_move_call, 34) -> [t,f];
var_args(l_is_eq_exact, 0) -> [f];
var_args(get_tuple_element, 2) -> [t,{u,0},{x,0}];
var_args(l_allocate, 2) -> [{u,2}];
var_args(move_return, 0) -> [{a,true}];
var_args(l_is_eq_exact_immed, 3) -> [f,x8,i8];
var_args(l_select_val2, 0) -> [{x,0},f,t,f,t,f];
var_args(l_fetch, 2) -> [x8,x8];
var_args(get_tuple_element, 3) -> [{x,0},u8,y8];
var_args(init2, 0) -> [y8,y8];
var_args(get_list, 2) -> [t,{x,0},t];
var_args(l_is_eq_exact_immed, 4) -> [f,{x,2},t];
var_args(init, 0) -> [{y,1}];
var_args(deallocate_return, 1) -> [{u,0}];
var_args(put_list, 3) -> [t,t,{x,3}];
var_args(call_bif, 9) -> [{b,{erlang,'++',2}}];
var_args(extract_next_element, 1) -> [{x,3}];
var_args(is_tuple_of_arity, 3) -> [f,y8,u8];
var_args(l_is_eq_exact_literal, 0) -> [f,{x,0},t];
var_args(get_tuple_element, 4) -> [y8,u8,x8];
var_args(move2, 4) -> [{x,0},y8,x8,y8];
var_args(l_move_call_ext, 41) -> [t,e];
var_args(l_fetch, 3) -> [x8,i8];
var_args(init, 1) -> [{y,0}];
var_args(l_put_tuple, 2) -> [{x,0},{u,3}];
var_args(move2, 5) -> [x8,x8,x8,{x,0}];
var_args(l_allocate, 3) -> [{u,3}];
var_args(deallocate_return, 2) -> [{u,2}];
var_args(l_fetch, 4) -> [x8,y8];
var_args(init3, 0) -> [y8,y8,y8];
var_args(is_nonempty_list, 1) -> [f,{x,2}];
var_args(l_is_eq_exact_immed, 5) -> [f,{x,4},t];
var_args(l_allocate_zero, 0) -> [{u,2}];
var_args(call_bif, 6) -> [{b,{erlang,throw,1}}];
var_args(l_trim, 1) -> [{u,2}];
var_args(allocate_init, 0) -> [u32,{y,0}];
var_args(call_bif, 45) -> [b];
var_args(allocate_heap, 0) -> [u8,u8,u8];
var_args(test_heap_1_put_list, 0) -> [{u,2},{y,0}];
var_args(l_allocate_zero, 1) -> [{u,1}];
var_args(move_deallocate_return, 1) -> [{a,ok},u8];
var_args(init, 2) -> [{y,2}];
var_args(l_call_last, 0) -> [f,{u,2}];
var_args(move_return, 1) -> [{x,1}];
var_args(is_nonempty_list, 2) -> [f,{x,1}];
var_args(l_move_call, 0) -> [{x,2},f];
var_args(is_tuple, 0) -> [f,{x,0}];
var_args(is_list, 0) -> [f,{x,0}];
var_args(l_is_eq_exact_immed, 6) -> [f,{x,5},t];
var_args(l_call_last, 1) -> [f,{u,0}];
var_args(deallocate_return, 3) -> [{u,3}];
var_args(is_nonempty_list_allocate, 0) -> [f,{x,0},u32];
var_args(l_move_call_only, 0) -> [f,{x,1}];
var_args(call_bif, 5) -> [{b,{erlang,exit,1}}];
var_args(extract_next_element, 2) -> [{x,2}];
var_args(l_increment, 0) -> [x8,u8,u8,t];
var_args(l_gc_bif1, 0) -> [f,b,t,u8,x8];
var_args(move, 6) -> [t,{x,4}];
var_args(l_is_lt, 0) -> [f];
var_args(l_trim, 2) -> [{u,3}];
var_args(l_select_val_atoms, 0) -> [x8,f,u8];
var_args(l_call_last, 2) -> [f,{u,3}];
var_args(move_deallocate_return, 2) -> [y8,u8];
var_args(is_nonempty_list, 3) -> [f,{x,3}];
var_args(l_new_bs_put_integer_imm, 0) -> [f,u32,{u8,0},t];
var_args(is_nil, 1) -> [f,{x,2}];
var_args(extract_next_element2, 1) -> [{x,3}];
var_args(l_move_call_only, 9) -> [f,t];
var_args(l_select_val2, 2) -> [{x,1},f,t,f,t,f];
var_args(remove_message, 0) -> [];
var_args(l_move_call_only, 1) -> [f,{x,2}];
var_args(init, 3) -> [{y,3}];
var_args(l_catch, 0) -> [{y,0},t];
var_args(l_allocate_zero, 2) -> [{u,3}];
var_args(extract_next_element3, 0) -> [{x,1}];
var_args(get_tuple_element, 5) -> [x8,u8,y8];
var_args(l_call_ext, 0) -> [{e,{lists,reverse,1}}];
var_args(l_fetch, 5) -> [i8,x8];
var_args(move_jump, 12) -> [f,t];
var_args(extract_next_element, 3) -> [{x,4}];
var_args(is_nil, 2) -> [f,{x,1}];
var_args(move2, 6) -> [x8,x8,{x,0},x8];
var_args(l_allocate, 4) -> [{u,4}];
var_args(catch_end, 0) -> [{y,0}];
var_args(test_arity, 0) -> [f,{x,0},{u,2}];
var_args(l_allocate_zero, 3) -> [{u,4}];
var_args(l_bs_start_match2, 1) -> [t,f,u8,u8,x8];
var_args(l_is_eq_exact_immed, 7) -> [f,{x,6},t];
var_args(move_return, 2) -> [{a,ok}];
var_args(put_list, 4) -> [x8,t,x8];
var_args(move_return, 3) -> [{x,2}];
var_args(l_move_call_ext, 2) -> [t,{e,{io,format,2}}];
var_args(move_return, 4) -> [nil];
var_args(deallocate_return, 4) -> [{u,4}];
var_args(l_call_last, 3) -> [f,{u,4}];
var_args(move2, 7) -> [{x,0},x8,x8,{x,0}];
var_args(l_is_eq_exact_immed, 8) -> [f,{x,7},t];
var_args(l_plus, 0) -> [f,u8,x8];
var_args(move, 7) -> [y8,y8];
var_args(l_put_tuple, 3) -> [{x,0},{u,4}];
var_args(call_bif, 2) -> [{b,{erlang,error,2}}];
var_args(l_select_tuple_arity2, 0) -> [{x,0},f,u8,f,u8,f];
var_args(is_nonempty_list, 4) -> [f,{x,4}];
var_args(init, 4) -> [{y,4}];
var_args(is_nonempty_list, 5) -> [f,{x,7}];
var_args(get_list, 3) -> [t,x8,y8];
var_args(l_call_fun, 0) -> [{u8,1}];
var_args(l_call_last, 4) -> [f,{u,1}];
var_args(l_move_call, 1) -> [{x,1},f];
var_args(move_return, 5) -> [{a,false}];
var_args(l_bs_get_binary_all_reuse, 0) -> [t,f,{u8,8}];
var_args(test_arity, 1) -> [f,x8,u8];
var_args(bif1_body, 0) -> [{b,{erlang,hd,1}},t,{x,0}];
var_args(l_move_call_only, 2) -> [f,{x,4}];
var_args(l_move_call_last, 1) -> [f,{u,1},t];
var_args(is_nonempty_list, 6) -> [f,{x,5}];
var_args(l_bs_test_zero_tail2, 1) -> [f,{x,2}];
var_args(l_is_eq, 0) -> [f];
var_args(send, 0) -> [];
var_args(set_tuple_element, 0) -> [y8,{x,0},u8];
var_args(l_catch, 1) -> [{y,1},t];
var_args(l_call_ext_only, 3) -> [e];
var_args(call_bif, 10) -> [{b,{lists,member,2}}];
var_args(l_is_ne, 0) -> [f];
var_args(l_move_call_only, 3) -> [f,{x,3}];
var_args(l_select_val2, 1) -> [x8,f,i8,f,i8,f];
var_args(l_move_call_ext_last, 0) -> [e,u8,y8];
var_args(l_select_val_smallints, 0) -> [x8,f,u8];
var_args(l_move_call_ext, 3) -> [{y,0},e];
var_args(l_is_ne_exact_immed, 0) -> [f,{x,0},t];
var_args(l_increment, 1) -> [y8,u8,u8,t];
var_args(l_bs_add, 0) -> [f,{u8,1},t];
var_args(bif2_body, 0) -> [b,{x,0}];
var_args(is_nonempty_list, 7) -> [f,{x,6}];
var_args(extract_next_element, 4) -> [{x,5}];
var_args(case_end, 1) -> [{x,1}];
var_args(l_bs_match_string, 1) -> [t,f,{u,8},str];
var_args(l_is_eq_exact_immed, 9) -> [f,{x,8},t];
var_args(extract_next_element2, 2) -> [{x,4}];
var_args(move_deallocate_return, 3) -> [t,{u,0}];
var_args(extract_next_element2, 3) -> [{x,2}];
var_args(l_loop_rec, 0) -> [f];
var_args(l_call_ext, 1) -> [{e,{asn1ct_gen,emit,1}}];
var_args(l_select_tuple_arity2, 1) -> [x8,f,u8,f,u8,f];
var_args(l_move_call, 2) -> [{y,0},f];
var_args(l_move_call, 3) -> [{y,2},f];
var_args(l_move_call_ext, 4) -> [{y,1},e];
var_args(l_bs_get_utf16, 0) -> [x8,f,u8,x8];
var_args(l_select_val_atoms, 1) -> [{x,0},f,u32];
var_args(l_trim, 3) -> [{u,4}];
var_args(init, 5) -> [{y,5}];
var_args(l_bs_restore2, 0) -> [x8,u8];
var_args(catch_end, 1) -> [{y,1}];
var_args(is_nil, 3) -> [f,{x,4}];
var_args(l_move_call, 4) -> [{y,1},f];
var_args(l_bs_init_heap_bin, 0) -> [u8,u8,u8,t];
var_args(is_nil, 4) -> [f,{x,3}];
var_args(is_nonempty_list, 8) -> [f,{x,9}];
var_args(wait, 0) -> [f];
var_args(l_call_ext_last, 0) -> [e,{u,1}];
var_args(l_allocate_zero, 4) -> [{u,6}];
var_args(loop_rec_end, 0) -> [f];
var_args(call_bif, 11) -> [{b,{ets,insert,2}}];
var_args(deallocate_return, 5) -> [{u,5}];
var_args(move, 8) -> [t,{x,5}];
var_args(l_fetch, 6) -> [y8,y8];
var_args(is_nonempty_list, 9) -> [f,{x,8}];
var_args(extract_next_element, 5) -> [{x,6}];
var_args(l_bs_start_match2, 2) -> [{x,0},f,u8,u8,{x,0}];
var_args(l_allocate_zero, 5) -> [{u,5}];
var_args(l_select_val2, 3) -> [{x,2},f,t,f,t,f];
var_args(l_allocate, 5) -> [{u,5}];
var_args(init, 6) -> [{y,6}];
var_args(l_move_call_ext, 5) -> [{y,2},e];
var_args(badmatch, 1) -> [{x,3}];
var_args(l_plus, 1) -> [f,u8,{x,0}];
var_args(l_call_last, 5) -> [f,{u,6}];
var_args(l_move_call_ext, 6) -> [t,{e,{asn1ct_gen,emit,1}}];
var_args(l_call_last, 6) -> [f,{u,5}];
var_args(l_move_call_ext, 7) -> [{y,3},e];
var_args(badmatch, 2) -> [{x,2}];
var_args(l_move_call_ext, 8) -> [{y,4},e];
var_args(l_call_fun_last, 0) -> [u8,u8];
var_args(l_move_call_ext, 9) -> [{smallint,1},e];
var_args(l_increment, 2) -> [t,{u,4294967295},u8,x8];
var_args(bs_context_to_binary, 0) -> [{x,0}];
var_args(call_bif, 12) -> [{b,{erlang,get_module_info,2}}];
var_args(int_code_end, 0) -> [];
var_args(l_trim, 4) -> [{u,5}];
var_args(l_move_call_ext_only, 0) -> [{e,{erlang,get_module_info,1}},t];
var_args(put_list, 5) -> [t,t,{x,4}];
var_args(l_bs_save2, 0) -> [x8,u8];
var_args(call_bif, 13) -> [{b,{erlang,list_to_binary,1}}];
var_args(extract_next_element, 6) -> [{x,255}];
var_args(move_deallocate_return, 4) -> [t,{u,1}];
var_args(l_is_ne_exact_immed, 1) -> [f,{x,1},t];
var_args(extract_next_element2, 4) -> [{x,5}];
var_args(l_select_val2, 4) -> [{x,3},f,t,f,t,f];
var_args(l_gc_bif1, 1) -> [f,{b,{erlang,length,1}},t,u8,{x,0}];
var_args(l_increment, 3) -> [{x,0},u32,u8,{x,0}];
var_args(self, 0) -> [{x,0}];
var_args(l_put_tuple, 4) -> [{x,0},{u,5}];
var_args(extract_next_element, 7) -> [{x,7}];
var_args(get_tuple_element, 6) -> [{x,0},{u,1},{x,0}];
var_args(call_bif, 14) -> [{b,{ets,delete,1}}];
var_args(is_tuple, 1) -> [f,{x,1}];
var_args(l_bs_test_unit_8, 0) -> [f,{x,0}];
var_args(l_move_call_last, 2) -> [f,{u,2},t];
var_args(badmatch, 3) -> [{x,1}];
var_args(l_move_call, 5) -> [{y,3},f];
var_args(l_is_eq_exact_immed, 11) -> [f,{x,9},t];
var_args(l_move_call, 6) -> [{x,3},f];
var_args(is_nil, 5) -> [f,{x,5}];
var_args(l_bs_test_zero_tail2, 2) -> [f,{x,0}];
var_args(is_nonempty_list_allocate, 1) -> [f,x8,u8];
var_args(l_call_ext, 2) -> [{e,{lists,foreach,2}}];
var_args(l_bs_test_zero_tail2, 3) -> [f,{x,3}];
var_args(l_catch, 2) -> [{y,2},t];
var_args(l_move_call_ext, 10) -> [{x,2},e];
var_args(call_bif, 15) -> [{b,{lists,keysearch,3}}];
var_args(is_atom, 0) -> [f,{x,0}];
var_args(l_move_call_ext_only, 6) -> [e,t];
var_args(l_fast_element, 0) -> [{x,0},{u,2},{x,0}];
var_args(is_nonempty_list, 10) -> [f,{x,10}];
var_args(l_select_val_smallints, 1) -> [{x,0},f,u32];
var_args(call_bif, 16) -> [{b,{ets,info,2}}];
var_args(l_call_ext, 3) -> [{e,{file,close,1}}];
var_args(move_return, 6) -> [{x,3}];
var_args(l_call_ext_last, 1) -> [e,{u,0}];
var_args(is_nil, 6) -> [f,{x,6}];
var_args(allocate_heap_zero, 0) -> [u8,u8,u8];
var_args(is_nonempty_list, 11) -> [f,{x,11}];
var_args(l_call_ext_last, 2) -> [e,{u,2}];
var_args(call_bif, 17) -> [{b,{erlang,list_to_atom,1}}];
var_args(self, 1) -> [{x,1}];
var_args(extract_next_element, 8) -> [{y,1}];
var_args(init, 7) -> [{y,7}];
var_args(case_end, 2) -> [{x,2}];
var_args(l_minus, 0) -> [f,u8,x8];
var_args(extract_next_element3, 1) -> [{x,3}];
var_args(is_nil, 7) -> [f,{x,7}];
var_args(l_move_call, 7) -> [{y,5},f];
var_args(call_bif, 18) -> [{b,{erlang,integer_to_list,1}}];
var_args(raise, 0) -> [{x,2},{x,1}];
var_args(catch_end, 2) -> [{y,2}];
var_args(l_is_eq_exact_literal, 1) -> [f,{x,1},t];
var_args(l_is_eq_exact_literal, 7) -> [f,t,t];
var_args(l_move_call_ext, 11) -> [{x,1},e];
var_args(extract_next_element2, 5) -> [{x,6}];
var_args(extract_next_element, 24) -> [t];
var_args(l_allocate, 6) -> [{u,6}];
var_args(l_bif2, 0) -> [f,{b,{erlang,element,2}},t];
var_args(try_end, 0) -> [{y,1}];
var_args(l_call_fun, 1) -> [{u8,3}];
var_args(call_bif, 19) -> [{b,{ets,lookup,2}}];
var_args(deallocate_return, 6) -> [{u,6}];
var_args(l_call_ext, 4) -> [{e,{file,open,2}}];
var_args(l_move_call_last, 3) -> [f,{u,0},t];
var_args(l_move_call_ext_only, 1) -> [{e,{io_lib,format,2}},t];
var_args(l_put_tuple, 5) -> [{x,0},u32];
var_args(l_band, 0) -> [f,u8,x8];
var_args(l_move_call, 8) -> [{y,4},f];
var_args(l_trim, 5) -> [{u,6}];
var_args(extract_next_element, 9) -> [{y,0}];
var_args(call_bif, 20) -> [{b,{erlang,atom_to_list,1}}];
var_args(l_call_ext, 5) -> [{e,{lists,map,2}}];
var_args(l_call_ext, 6) -> [{e,{lists,foldl,3}}];
var_args(l_minus, 1) -> [f,u8,{x,0}];
var_args(l_bs_get_binary_all2, 0) -> [f,x8,u8,u8,x8];
var_args(get_tuple_element, 7) -> [x8,u8,{x,0}];
var_args(l_is_eq_exact_immed, 12) -> [f,{x,10},t];
var_args(test_heap_1_put_list, 1) -> [{u,2},t];
var_args(extract_next_element3, 2) -> [{x,2}];
var_args(is_integer, 0) -> [f,{x,0}];
var_args(extract_next_element2, 6) -> [{x,8}];
var_args(l_catch, 3) -> [{y,3},t];
var_args(is_nil, 8) -> [f,{x,8}];
var_args(l_bif2, 1) -> [f,{b,{erlang,'=:=',2}},t];
var_args(move_deallocate_return, 5) -> [t,{u,2}];
var_args(l_move_call_only, 4) -> [f,{x,5}];
var_args(l_bsr, 0) -> [f,u8,x8];
var_args(move_jump, 0) -> [f,{x,1}];
var_args(is_list, 1) -> [f,{x,1}];
var_args(l_move_call, 9) -> [{smallint,1},f];
var_args(l_bs_get_integer_small_imm, 0) -> [t,u8,f,u8,x8];
var_args(l_is_eq_exact_immed, 13) -> [f,{x,11},t];
var_args(apply, 0) -> [u8];
var_args(l_call_ext, 7) -> [{e,{filename,join,2}}];
var_args(l_fast_element, 2) -> [x8,u8,t];
var_args(l_bs_get_integer_8, 0) -> [x8,f,x8];
var_args(l_bif2, 2) -> [f,{b,{erlang,'=<',2}},t];
var_args(l_fetch, 7) -> [{y,0},{x,2}];
var_args(set_tuple_element, 1) -> [t,{x,0},u32];
var_args(try_end, 1) -> [{y,0}];
var_args(l_is_eq_exact_literal, 2) -> [f,{x,4},t];
var_args(l_is_eq_exact_immed, 10) -> [f,{x,255},{a,xmerl_scanner}];
var_args(extract_next_element2, 7) -> [{x,7}];
var_args(l_is_eq_exact_literal, 3) -> [f,{x,2},t];
var_args(l_bsl, 0) -> [f,u8,{x,0}];
var_args(l_allocate_zero, 6) -> [{u,7}];
var_args(is_nonempty_list, 12) -> [f,{x,12}];
var_args(l_times, 0) -> [f,u8,x8];
var_args(l_select_tuple_arity, 0) -> [t,f,{u,6}];
var_args(l_fmul, 0) -> [fr,fr,fr];
var_args(l_call_ext, 8) -> [{e,{lists,sort,1}}];
var_args(l_bs_match_string, 2) -> [{x,0},f,u32,str];
var_args(move, 9) -> [t,{x,6}];
var_args(call_bif, 21) -> [{b,{ets,lookup_element,3}}];
var_args(l_move_call_ext, 12) -> [t,{e,{io_lib,format,2}}];
var_args(l_put_tuple, 6) -> [y8,u8];
var_args(l_times, 1) -> [f,u8,{x,0}];
var_args(l_bs_init_fail, 0) -> [u8,f,u8,x8];
var_args(l_move_call_ext_only, 2) -> [{e,{lists,reverse,1}},t];
var_args(l_is_eq_exact_immed, 14) -> [f,{x,12},t];
var_args(l_is_eq_exact_immed, 15) -> [f,y8,{a,asn1_NOVALUE}];
var_args(l_move_call_ext, 14) -> [t,{e,{erlang,put,2}}];
var_args(l_move_call_ext, 13) -> [{y,6},e];
var_args(l_call_ext, 9) -> [{e,{ordsets,union,2}}];
var_args(call_bif, 22) -> [{b,{erlang,binary_to_list,1}}];
var_args(extract_next_element, 10) -> [{x,8}];
var_args(is_nil, 9) -> [f,{x,9}];
var_args(l_fetch, 8) -> [t,{x,1}];
var_args(node, 0) -> [{x,0}];
var_args(l_call_last, 7) -> [f,{u,7}];
var_args(l_bs_get_binary2, 0) -> [f,x8,u8,t,u8,{u8,0},x8];
var_args(is_tuple, 2) -> [f,{x,2}];
var_args(l_call_fun, 2) -> [{u8,2}];
var_args(get_list, 4) -> [y8,x8,x8];
var_args(test_arity, 2) -> [f,{x,0},u32];
var_args(l_bs_get_integer_8, 1) -> [{x,0},f,x8];
var_args(l_bs_test_zero_tail2, 5) -> [f,t];
var_args(catch_end, 3) -> [{y,3}];
var_args(l_bif2, 3) -> [f,{b,{erlang,'or',2}},t];
var_args(l_is_ne_exact_immed, 2) -> [f,{x,2},t];
var_args(l_allocate_zero, 9) -> [u32];
var_args(call_bif, 23) -> [{b,{erlang,'--',2}}];
var_args(l_is_ne_exact, 0) -> [f];
var_args(l_bif2, 4) -> [f,{b,{erlang,'and',2}},t];
var_args(is_binary, 0) -> [f,{x,1}];
var_args(l_is_eq_exact_immed, 16) -> [f,{y,0},t];
var_args(l_bs_get_integer_32, 0) -> [x8,f,u8,x8];
var_args(extract_next_element, 11) -> [{y,3}];
var_args(l_call_ext, 10) -> [{e,{lists,flatten,1}}];
var_args(is_atom, 1) -> [f,{x,1}];
var_args(l_select_val2, 7) -> [{x,4},f,t,f,t,f];
var_args(l_fetch, 9) -> [y8,i8];
var_args(l_fcheckerror, 0) -> [];
var_args(l_new_bs_put_binary_all, 0) -> [f,{x,0},{u8,8}];
var_args(fclearerror, 0) -> [];
var_args(extract_next_element3, 3) -> [{x,5}];
var_args(system_limit, 0) -> [f];
var_args(node, 1) -> [{x,1}];
var_args(is_nonempty_list, 38) -> [f,t];
var_args(extract_next_element, 12) -> [{y,2}];
var_args(get_list, 5) -> [{x,0},t,{x,0}];
var_args(l_move_call, 10) -> [{y,6},f];
var_args(move_deallocate_return, 6) -> [t,{u,3}];
var_args(l_move_call_last, 4) -> [f,{u,3},t];
var_args(l_new_bs_put_binary_all, 1) -> [f,t,{u8,8}];
var_args(l_is_eq_exact_immed, 17) -> [f,{y,1},t];
var_args(timeout, 0) -> [];
var_args(deallocate_return, 7) -> [{u,7}];
var_args(l_get, 0) -> [{a,asn1_module},{y,0}];
var_args(l_select_val2, 14) -> [t,f,t,f,t,f];
var_args(l_fetch, 10) -> [i8,y8];
var_args(l_move_call, 11) -> [{x,4},f];
var_args(l_move_call_ext_last, 1) -> [e,{u,0},t];
var_args(is_nil, 10) -> [f,{y,1}];
var_args(l_fetch, 11) -> [{x,1},t];
var_args(l_select_val2, 5) -> [t,f,{a,true},f,{a,false},f];
var_args(is_float, 1) -> [f,t];
var_args(call_bif, 24) -> [{b,{re,run,3}}];
var_args(l_call_ext, 11) -> [{e,{test_server,timetrap,1}}];
var_args(l_is_eq_exact_immed, 36) -> [f,t,t];
var_args(l_select_val2, 8) -> [{x,5},f,t,f,t,f];
var_args(l_get, 1) -> [t,{x,1}];
var_args(call_bif, 25) -> [{b,{erlang,process_flag,2}}];
var_args(l_bs_restore2, 1) -> [{x,0},{u,0}];
var_args(l_move_call, 12) -> [nil,f];
var_args(l_band, 1) -> [f,u8,{x,0}];
var_args(l_bsl, 1) -> [f,u8,x8];
var_args(l_fast_element, 1) -> [{x,0},{u,1},t];
var_args(is_binary, 1) -> [f,{x,0}];
var_args(l_move_call_ext, 16) -> [{y,5},e];
var_args(l_get, 2) -> [t,{x,0}];
var_args(extract_next_element, 13) -> [{x,9}];
var_args(is_nonempty_list, 13) -> [f,{y,2}];
var_args(l_is_eq_exact_immed, 18) -> [f,{x,255},t];
var_args(l_call_ext, 12) -> [{e,{lists,concat,1}}];
var_args(l_move_call, 13) -> [{x,5},f];
var_args(l_move_call_ext, 17) -> [t,{e,{prettypr,text,1}}];
var_args(l_rem, 0) -> [f,u8,x8];
var_args(move2, 8) -> [{x,0},x8,x8,x8];
var_args(l_call_ext, 13) -> [{e,{test_server,timetrap_cancel,1}}];
var_args(l_allocate_zero, 7) -> [{u,8}];
var_args(l_call_last, 8) -> [f,{u,8}];
var_args(is_nil, 11) -> [f,{x,10}];
var_args(l_gc_bif1, 2) -> [f,b,{x,0},{u8,1},{x,0}];
var_args(l_is_ne_exact_immed, 10) -> [f,t,t];
var_args(l_fetch, 22) -> [t,t];
var_args(l_increment, 4) -> [y8,{u,4294967295},u8,{x,0}];
var_args(extract_next_element3, 4) -> [{x,4}];
var_args(is_nil, 30) -> [f,t];
var_args(extract_next_element3, 10) -> [t];
var_args(l_bs_append, 0) -> [f,u8,u8,u8,x8];
var_args(is_nonempty_list, 14) -> [f,{x,13}];
var_args(is_integer, 5) -> [f,t];
var_args(l_move_call_ext, 18) -> [t,{e,{asn1ct_name,new,1}}];
var_args(call_bif, 26) -> [{b,{erlang,process_info,2}}];
var_args(l_trim, 6) -> [{u,7}];
var_args(is_nil, 12) -> [f,{x,11}];
var_args(l_call_ext, 14) -> [{e,{file,delete,1}}];
var_args(l_bor, 0) -> [f,u8,{x,0}];
var_args(move_return, 7) -> [{x,4}];
var_args(is_list, 2) -> [f,{x,2}];
var_args(l_call_ext, 15) -> [{e,{mnesia_lib,set,2}}];
var_args(is_nil, 13) -> [f,{x,12}];
var_args(l_catch, 4) -> [{y,4},t];
var_args(l_fadd, 0) -> [fr,fr,fr];
var_args(l_gc_bif1, 5) -> [f,b,t,u8,{x,0}];
var_args(l_element, 1) -> [t,t,t];
var_args(extract_next_element2, 17) -> [t];
var_args(call_bif, 27) -> [{b,{erlang,unlink,1}}];
var_args(l_allocate, 7) -> [{u,7}];
var_args(l_move_call_only, 5) -> [f,{x,6}];
var_args(l_move_call_ext_last, 4) -> [e,u32,t];
var_args(l_move_call_ext, 20) -> [t,{e,{proplists,get_value,2}}];
var_args(l_move_call_ext, 19) -> [nil,e];
var_args(is_nonempty_list, 15) -> [f,{y,3}];
var_args(call_bif, 29) -> [{b,{erlang,tuple_to_list,1}}];
var_args(call_bif, 28) -> [{b,{erlang,whereis,1}}];
var_args(is_integer, 1) -> [f,{x,1}];
var_args(bif1_body, 1) -> [{b,{erlang,hd,1}},{x,0},{x,1}];
var_args(l_call_ext, 16) -> [{e,{test_server,lookup_config,2}}];
var_args(l_is_ne_exact_immed, 3) -> [f,t,{a,true}];
var_args(is_nonempty_list, 16) -> [f,{x,14}];
var_args(l_is_eq_exact_immed, 19) -> [f,{x,13},t];
var_args(l_call_ext_last, 3) -> [e,{u,3}];
var_args(l_move_call_ext, 21) -> [t,{e,{proplists,get_value,3}}];
var_args(l_fetch, 12) -> [{x,4},t];
var_args(fmove_1, 0) -> [x8,fr];
var_args(l_move_call_ext, 22) -> [{x,3},e];
var_args(bif1_body, 2) -> [{b,{erlang,hd,1}},{y,1},{x,2}];
var_args(move_jump, 1) -> [f,{x,2}];
var_args(l_bs_get_utf8, 0) -> [x8,f,x8];
var_args(case_end, 10) -> [t];
var_args(bif2_body, 1) -> [b,{x,1}];
var_args(l_move_call_ext, 23) -> [t,{e,{test_server,seconds,1}}];
var_args(l_bs_skip_bits_all2, 0) -> [f,{x,2},{u8,8}];
var_args(l_call_ext, 17) -> [{e,{erl_syntax,type,1}}];
var_args(l_is_eq_exact_immed, 20) -> [f,{y,3},t];
var_args(fconv, 0) -> [t,{fr,0}];
var_args(l_bor, 1) -> [f,u8,x8];
var_args(call_bif, 30) -> [{b,{lists,keyfind,3}}];
var_args(l_is_eq_exact_literal, 4) -> [f,{x,3},t];
var_args(l_move_call_ext_last, 2) -> [e,{u,1},t];
var_args(l_bs_init_bits_fail, 0) -> [u32,f,u8,t];
var_args(call_bif, 31) -> [{b,{erlang,list_to_tuple,1}}];
var_args(extract_next_element3, 5) -> [{x,7}];
var_args(extract_next_element2, 8) -> [{x,9}];
var_args(l_is_eq_exact_immed, 22) -> [f,{x,14},t];
var_args(apply_last, 0) -> [u8,u32];
var_args(l_move_call_ext_only, 3) -> [e,{x,2}];
var_args(call_bif, 32) -> [{b,{ets,new,2}}];
var_args(call_bif, 4) -> [{b,{erlang,exit,2}}];
var_args(is_atom, 2) -> [f,{x,2}];
var_args(call_bif, 33) -> [{b,{erlang,make_ref,0}}];
var_args(put_list, 6) -> [{x,0},t,{y,0}];
var_args(put_list, 8) -> [y8,t,x8];
var_args(l_bs_match_string, 3) -> [x8,f,u8,str];
var_args(l_make_export, 0) -> [e];
var_args(extract_next_element, 14) -> [{x,10}];
var_args(l_catch, 5) -> [{y,5},t];
var_args(init, 8) -> [{y,8}];
var_args(l_increment, 8) -> [t,u32,u8,t];
var_args(move_deallocate_return, 7) -> [t,{u,4}];
var_args(l_call_fun, 3) -> [{u8,0}];
var_args(l_select_val2, 6) -> [t,f,{a,false},f,{a,true},f];
var_args(l_new_bs_put_integer, 0) -> [f,t,{u8,1},{u8,0},t];
var_args(fmove_2, 0) -> [fr,x8];
var_args(call_bif, 34) -> [{b,{lists,reverse,2}}];
var_args(badmatch, 4) -> [{y,2}];
var_args(is_atom, 3) -> [f,{x,3}];
var_args(l_move_call, 14) -> [{y,7},f];
var_args(fmove_2, 1) -> [fr,{x,0}];
var_args(l_bs_test_zero_tail2, 4) -> [f,{x,4}];
var_args(fmove_1, 1) -> [t,{fr,1}];
var_args(l_move_call_ext, 24) -> [{y,7},e];
var_args(is_integer_allocate, 0) -> [f,x8,u8];
var_args(l_call_ext_last, 4) -> [e,{u,4}];
var_args(call_bif, 35) -> [{b,{lists,keymember,3}}];
var_args(l_fetch, 13) -> [{x,3},t];
var_args(l_fast_element, 3) -> [t,{u,2},t];
var_args(l_call_ext, 18) -> [{e,{lists,mapfoldl,3}}];
var_args(l_move_call, 15) -> [{smallint,0},f];
var_args(l_move_call_ext, 25) -> [t,{e,{mnesia_lib,verbose,2}}];
var_args(is_tuple, 9) -> [f,t];
var_args(l_trim, 7) -> [{u,8}];
var_args(is_list, 6) -> [f,t];
var_args(l_fetch, 14) -> [{x,2},t];
var_args(deallocate_return, 8) -> [{u,8}];
var_args(l_is_eq_exact_immed, 23) -> [f,{y,2},t];
var_args(l_call_ext, 19) -> [{e,{prettypr,floating,1}}];
var_args(extract_next_element, 15) -> [{y,4}];
var_args(l_fetch, 15) -> [t,{x,4}];
var_args(l_move_call_last, 5) -> [f,{u,4},t];
var_args(is_tuple, 3) -> [f,{x,3}];
var_args(is_nonempty_list, 17) -> [f,{y,1}];
var_args(l_move_call_ext, 15) -> [{smallint,0},{e,{lists,seq,2}}];
var_args(l_get, 3) -> [t,{x,2}];
var_args(extract_next_element, 16) -> [{y,5}];
var_args(extract_next_element2, 9) -> [{x,12}];
var_args(try_end, 2) -> [{y,2}];
var_args(if_end, 0) -> [];
var_args(fmove_1, 2) -> [t,fr];
var_args(call_bif, 36) -> [{b,{erlang,now,0}}];
var_args(move_return, 8) -> [{smallint,1}];
var_args(l_move_call, 16) -> [{smallint,3},f];
var_args(l_is_ne_exact_immed, 4) -> [f,{x,3},t];
var_args(l_bs_skip_bits_imm2, 0) -> [f,t,u32];
var_args(call_bif, 38) -> [{b,{erlang,spawn_link,1}}];
var_args(call_bif, 37) -> [{b,{erlang,get_stacktrace,0}}];
var_args(move_jump, 2) -> [f,nil];
var_args(try_end, 4) -> [{y,4}];
var_args(try_end, 3) -> [{y,5}];
var_args(l_move_call_ext_last, 3) -> [e,{u,2},t];
var_args(call_bif, 39) -> [{b,{ets,delete,2}}];
var_args(self, 2) -> [{x,2}];
var_args(l_call_ext, 23) -> [{e,{dict,find,2}}];
var_args(l_call_ext, 22) -> [{e,{erlang,list_to_integer,1}}];
var_args(l_call_ext, 21) -> [{e,{string,tokens,2}}];
var_args(l_call_ext, 20) -> [{e,{proplists,get_value,3}}];
var_args(is_list, 3) -> [f,{x,3}];
var_args(is_nonempty_list, 19) -> [f,{x,16}];
var_args(is_nonempty_list, 18) -> [f,{x,15}];
var_args(try_end, 5) -> [{y,3}];
var_args(catch_end, 4) -> [{y,4}];
var_args(l_call_ext, 25) -> [{e,{filename,join,1}}];
var_args(l_call_ext, 24) -> [{e,{lists,filter,2}}];
var_args(extract_next_element2, 10) -> [{x,10}];
var_args(init, 9) -> [{y,9}];
var_args(l_bif1, 0) -> [f,b,x8,x8];
var_args(l_call_ext, 26) -> [{e,{asn1_db,dbget,2}}];
var_args(get_tuple_element, 8) -> [{x,0},u32,{x,0}];
var_args(l_select_val_atoms, 2) -> [t,f,u32];
var_args(test_arity, 3) -> [f,y8,u8];
var_args(case_end, 3) -> [{y,2}];
var_args(bif1_body, 3) -> [b,{x,0},{x,0}];
var_args(l_is_eq_exact_immed, 24) -> [f,{x,15},t];
var_args(l_select_val2, 9) -> [{x,6},f,t,f,t,f];
var_args(l_bs_get_utf16, 1) -> [{x,0},f,u8,x8];
var_args(is_tuple, 4) -> [f,{x,4}];
var_args(l_call_ext, 28) -> [{e,{proplists,get_value,2}}];
var_args(l_call_ext, 27) -> [{e,{prettypr,beside,2}}];
var_args(extract_next_element, 17) -> [{x,11}];
var_args(l_move_call_ext, 26) -> [t,{e,{erlang,system_info,1}}];
var_args(l_get, 6) -> [t,t];
var_args(l_call_ext, 29) -> [{e,{dict,new,0}}];
var_args(is_integer, 2) -> [f,{x,2}];
var_args(badmatch, 5) -> [{y,3}];
var_args(l_bs_put_string, 1) -> [u32,str];
var_args(try_case_end, 0) -> [{x,0}];
var_args(l_fdiv, 0) -> [fr,fr,fr];
var_args(get_list, 6) -> [x8,y8,y8];
var_args(l_call_ext_last, 6) -> [e,u32];
var_args(l_bif1, 1) -> [f,b,{x,0},t];
var_args(put_list, 9) -> [x8,t,y8];
var_args(move_return, 9) -> [{smallint,2}];
var_args(case_end, 4) -> [{x,3}];
var_args(self, 5) -> [t];
var_args(l_call_ext, 30) -> [{e,{sofs,to_external,1}}];
var_args(l_bs_test_unit_8, 1) -> [f,{x,3}];
var_args(l_gc_bif1, 3) -> [f,{b,{erlang,byte_size,1}},{x,1},{u8,2},{x,0}];
var_args(move_deallocate_return, 9) -> [t,u32];
var_args(l_is_eq_exact_immed, 25) -> [f,{x,16},t];
var_args(call_bif, 41) -> [{b,{ets,match_object,2}}];
var_args(call_bif, 40) -> [{b,{ets,safe_fixtable,2}}];
var_args(extract_next_element, 18) -> [{x,12}];
var_args(extract_next_element2, 11) -> [{x,11}];
var_args(is_nil, 14) -> [f,{x,13}];
var_args(l_move_call_only, 6) -> [f,nil];
var_args(l_bs_restore2, 2) -> [{x,0},{u,1}];
var_args(l_move_call, 18) -> [{smallint,2},f];
var_args(l_move_call, 17) -> [{smallint,12},f];
var_args(bif1_body, 5) -> [b,t,{x,1}];
var_args(move, 10) -> [t,{x,7}];
var_args(l_move_call_ext, 28) -> [t,{e,{lists,seq,2}}];
var_args(l_move_call_ext, 29) -> [t,{e,{lists,duplicate,2}}];
var_args(l_bs_get_integer, 0) -> [f,u8,u8,u8,x8];
var_args(is_atom, 6) -> [f,t];
var_args(is_integer, 3) -> [f,{x,4}];
var_args(l_allocate_zero, 8) -> [{u,9}];
var_args(is_nil, 15) -> [f,{x,14}];
var_args(is_list, 4) -> [f,{x,4}];
var_args(case_end, 5) -> [{y,1}];
var_args(l_increment, 7) -> [x8,u32,u8,x8];
var_args(l_is_eq_exact_immed, 26) -> [f,{y,4},t];
var_args(l_increment, 5) -> [{x,0},u8,u8,x8];
var_args(l_fsub, 0) -> [fr,fr,fr];
var_args(get_tuple_element, 9) -> [y8,u8,{x,0}];
var_args(fconv, 1) -> [t,fr];
var_args(call_bif, 42) -> [{b,{ets,next,2}}];
var_args(l_bsr, 1) -> [f,u8,t];
var_args(l_move_call_ext, 30) -> [t,{e,{erlang,binary_to_term,1}}];
var_args(call_bif, 43) -> [{b,{ets,match,2}}];
var_args(l_call_ext, 31) -> [{e,{ordsets,from_list,1}}];
var_args(extract_next_element3, 6) -> [{x,6}];
var_args(badmatch, 6) -> [{x,4}];
var_args(put_list, 7) -> [{smallint,10},{x,0},t];
var_args(l_move_call_ext, 32) -> [t,{e,{test_server,lookup_config,2}}];
var_args(l_move_call_ext, 31) -> [t,{e,{mnesia_lib,dbg_out,2}}];
var_args(l_call_ext, 33) -> [{e,{erlang,put,2}}];
var_args(l_call_ext, 32) -> [{e,{dict,store,3}}];
var_args(extract_next_element2, 12) -> [{y,0}];
var_args(is_integer, 4) -> [f,{x,3}];
var_args(move_return, 10) -> [{smallint,0}];
var_args(l_rem, 1) -> [f,u8,{x,0}];
var_args(l_bs_put_string, 0) -> [{u,1},str];
var_args(is_nonempty_list, 20) -> [f,{x,17}];
var_args(move_deallocate_return, 8) -> [t,{u,5}];
var_args(l_move_call, 19) -> [{y,12},f];
var_args(l_is_eq_exact_literal, 5) -> [f,{x,6},t];
var_args(l_call_ext_only, 0) -> [{e,{gen_server,call,3}}];
var_args(l_plus, 2) -> [f,u8,y8];
var_args(l_increment, 6) -> [x8,{u,4294967295},u8,{x,0}];
var_args(l_int_div, 0) -> [f,u8,{x,0}];
var_args(l_bs_get_binary_imm2, 0) -> [f,x8,u8,u8,{u8,0},x8];
var_args(l_is_eq_exact_literal, 6) -> [f,{x,5},t];
var_args(l_move_call_ext, 33) -> [t,{e,{erlang,erase,1}}];
var_args(node, 4) -> [t];
var_args(l_call_ext, 34) -> [{e,{lists,duplicate,2}}];
var_args(put_list, 10) -> [{x,0},t,t];
var_args(l_move_call, 20) -> [{a,foo},f];
var_args(init, 10) -> [{y,10}];
var_args(catch_end, 5) -> [{y,5}];
var_args(badmatch, 15) -> [t];
var_args(bif1_body, 6) -> [b,t,{x,0}];
var_args(l_fetch, 16) -> [t,{x,3}];
var_args(test_heap_1_put_list, 2) -> [u8,i8];
var_args(l_allocate, 9) -> [u32];
var_args(l_yield, 0) -> [];
var_args(is_atom, 4) -> [f,{x,4}];
var_args(l_move_call, 21) -> [{a,false},f];
var_args(l_fetch, 17) -> [{x,5},t];
var_args(l_bif2, 5) -> [f,{b,{erlang,'==',2}},t];
var_args(l_is_eq_exact_immed, 27) -> [f,{x,17},t];
var_args(get_list, 7) -> [{x,0},y8,y8];
var_args(l_bs_get_binary_all2, 1) -> [f,{x,0},u8,u8,x8];
var_args(call_bif, 44) -> [{b,{erlang,monitor,2}}];
var_args(node, 2) -> [{x,2}];
var_args(l_call_ext, 37) -> [{e,{asn1ct_gen,mk_var,1}}];
var_args(l_call_ext, 36) -> [{e,{lists,delete,2}}];
var_args(l_call_ext, 35) -> [{e,{gb_trees,empty,0}}];
var_args(l_call_last, 9) -> [f,{u,9}];
var_args(is_nil, 16) -> [f,{y,2}];
var_args(is_list, 5) -> [f,{x,5}];
var_args(case_end, 6) -> [{y,3}];
var_args(l_new_bs_put_float_imm, 1) -> [f,u32,u8,t];
var_args(l_move_call, 22) -> [{a,endDocument},f];
var_args(l_catch, 6) -> [{y,6},t];
var_args(l_move_call_last, 6) -> [f,u8,x8];
var_args(l_move_call_ext, 35) -> [t,{e,{mnesia_lib,set,2}}];
var_args(l_bs_append, 1) -> [f,u8,u8,u8,{x,0}];
var_args(l_call_ext, 38) -> [{e,{gb_trees,lookup,2}}];
var_args(case_end, 7) -> [{x,4}];
var_args(is_nonempty_list, 21) -> [f,{y,4}];
var_args(l_select_tuple_arity, 3) -> [x8,f,u8];
var_args(l_move_call_ext, 36) -> [t,{e,{lists,sublist,3}}];
var_args(l_is_ne_exact_immed, 5) -> [f,{x,4},t];
var_args(l_jump_on_val, 0) -> [x8,f,u8,u8];
var_args(l_bs_get_integer_32, 1) -> [{x,0},f,u8,x8];
var_args(l_bs_skip_bits2, 1) -> [f,t,t,u8];
var_args(is_function, 1) -> [f,t];
var_args(l_gc_bif1, 4) -> [f,{b,{erlang,length,1}},x8,u8,y8];
var_args(l_call_ext, 39) -> [{e,{file,read_file_info,1}}];
var_args(l_is_eq_exact_immed, 21) -> [f,{y,10},{a,ber}];
var_args(is_nonempty_list, 22) -> [f,{x,18}];
var_args(l_int_div, 1) -> [f,u8,x8];
var_args(l_is_eq_exact_immed, 28) -> [f,{y,5},t];
var_args(l_is_ne_exact_immed, 6) -> [f,{y,0},t];
var_args(l_call_ext, 40) -> [{e,{io,format,3}}];
var_args(extract_next_element2, 13) -> [{x,13}];
var_args(move_jump, 3) -> [f,{x,4}];
var_args(move_return, 11) -> [{smallint,3}];
var_args(badmatch, 7) -> [{y,4}];
var_args(l_bs_test_unit_8, 3) -> [f,{x,1}];
var_args(l_bs_test_unit_8, 2) -> [f,{x,2}];
var_args(l_move_call, 24) -> [{x,6},f];
var_args(l_move_call, 23) -> [{smallint,9},f];
var_args(l_move_call_ext_only, 4) -> [e,{x,1}];
var_args(l_move_call_ext, 37) -> [{x,4},e];
var_args(l_is_eq_exact_immed, 29) -> [f,{x,18},t];
var_args(l_call_ext, 41) -> [{e,{lists,keydelete,3}}];
var_args(move_return, 12) -> [{smallint,4}];
var_args(is_nil, 18) -> [f,{x,16}];
var_args(is_nil, 17) -> [f,{x,15}];
var_args(is_nonempty_list, 23) -> [f,{x,20}];
var_args(l_move_call, 27) -> [{a,ets},f];
var_args(l_move_call, 26) -> [{a,schema},f];
var_args(l_move_call, 25) -> [{smallint,6},f];
var_args(l_select_tuple_arity, 1) -> [{x,0},f,{u,8}];
var_args(l_fetch, 18) -> [t,{x,2}];
var_args(bif1_body, 7) -> [b,t,t];
var_args(l_move_call_ext, 38) -> [{a,schema},e];
var_args(call_bif, 1) -> [{b,{erlang,raise,3}}];
var_args(l_call_ext, 48) -> [{e,{filename,basename,1}}];
var_args(l_call_ext, 47) -> [{e,{erlang,binary_to_term,1}}];
var_args(l_call_ext, 46) -> [{e,{lists,last,1}}];
var_args(l_call_ext, 45) -> [{e,{asn1ct_gen,get_inner,1}}];
var_args(l_call_ext, 44) -> [{e,{erl_syntax,atom,1}}];
var_args(l_call_ext, 43) -> [{e,{file,read_file,1}}];
var_args(l_call_ext, 42) -> [{e,{ssh_channel,cache_lookup,2}}];
var_args(extract_next_element3, 7) -> [{x,8}];
var_args(l_bs_save2, 1) -> [{x,0},{u,1}];
var_args(bif2_body, 2) -> [b,{x,2}];
var_args(is_binary, 3) -> [f,t];
var_args(l_move_call_ext, 39) -> [{a,func},e];
var_args(deallocate_return, 12) -> [u32];
var_args(l_catch, 8) -> [t,t];
var_args(l_allocate, 8) -> [{u,8}];
var_args(l_call_ext, 50) -> [{e,{file,read,2}}];
var_args(l_call_ext, 49) -> [{e,{file,make_dir,1}}];
var_args(l_trim, 8) -> [{u,9}];
var_args(is_nil, 19) -> [f,{y,3}];
var_args(case_end, 8) -> [{y,4}];
var_args(l_call_fun, 4) -> [u8];
var_args(l_gc_bif1, 6) -> [f,b,{x,0},u8,y8];
var_args(l_bs_skip_bits2, 0) -> [f,x8,x8,u8];
var_args(l_move_call_ext, 40) -> [{smallint,2},e];
var_args(l_call_ext, 53) -> [{e,{gb_trees,get,2}}];
var_args(l_call_ext, 52) -> [{e,{cerl,get_ann,1}}];
var_args(l_call_ext, 51) -> [{e,{mnesia_lib,exists,1}}];
var_args(extract_next_element, 19) -> [{x,13}];
var_args(l_is_ne_exact_literal, 0) -> [f,t,t];
var_args(l_move_call_ext_only, 5) -> [{e,{io,format,2}},t];
var_args(l_call_ext_last, 5) -> [e,{u,5}];
var_args(l_select_tuple_arity2, 2) -> [y8,f,u8,f,u8,f];
var_args(bs_context_to_binary, 5) -> [t];
var_args(l_select_val2, 10) -> [{x,7},f,t,f,t,f];
var_args(l_fetch, 19) -> [{y,0},t];
var_args(init, 15) -> [t];
var_args(l_get, 4) -> [{a,asn1_module},y8];
var_args(l_call_ext, 54) -> [{e,{ordsets,subtract,2}}];
var_args(move_return, 13) -> [{a,no}];
var_args(badmatch, 8) -> [{y,0}];
var_args(l_bs_test_unit_8, 4) -> [f,t];
var_args(is_pid, 1) -> [f,t];
var_args(is_boolean, 0) -> [f,t];
var_args(bif1_body, 4) -> [{b,{erlang,hd,1}},y8,x8];
var_args(l_bs_get_binary2, 1) -> [f,{x,0},u8,x8,u8,{u8,0},x8];
var_args(put_list, 12) -> [t,t,t];
var_args(l_call_ext, 58) -> [{e,{erlang,term_to_binary,1}}];
var_args(l_call_ext, 57) -> [{e,{file,write,2}}];
var_args(l_call_ext, 56) -> [{e,{asn1ct_gen,list2name,1}}];
var_args(l_call_ext, 55) -> [{e,{sofs,family_union,2}}];
var_args(extract_next_element2, 14) -> [{x,14}];
var_args(move_jump, 4) -> [f,{y,0}];
var_args(move_return, 14) -> [{smallint,8}];
var_args(l_move_call_only, 7) -> [f,{x,7}];
var_args(bs_context_to_binary, 1) -> [{x,1}];
var_args(l_call_ext, 65) -> [{e,{mnesia_lib,cs_to_storage_type,2}}];
var_args(l_call_ext, 64) -> [{e,{file,rename,2}}];
var_args(l_call_ext, 63) -> [{e,{filename,dirname,1}}];
var_args(l_call_ext, 62) -> [{e,{lists,append,1}}];
var_args(l_call_ext, 61) -> [{e,{asn1ct_gen,type,1}}];
var_args(l_call_ext, 60) -> [{e,{test_server,fail,1}}];
var_args(l_call_ext, 59) -> [{e,{random,uniform,1}}];
var_args(extract_next_element, 20) -> [{x,14}];
var_args(l_move_call_last, 7) -> [f,u32,t];
var_args(extract_next_element2, 15) -> [{x,16}];
var_args(move_return, 15) -> [{a,undefined}];
var_args(l_move_call, 28) -> [{smallint,100},f];
var_args(init, 11) -> [{y,11}];
var_args(l_element, 0) -> [t,{x,0},{x,1}];
var_args(l_call_ext, 68) -> [{e,{lists,dropwhile,2}}];
var_args(l_call_ext, 67) -> [{e,{mnesia_monitor,use_dir,0}}];
var_args(l_call_ext, 66) -> [{e,{lists,splitwith,2}}];
var_args(l_bs_start_match2, 3) -> [t,f,u8,u8,{x,0}];
var_args(move_jump, 6) -> [f,{y,2}];
var_args(move_jump, 5) -> [f,{y,1}];
var_args(is_nonempty_list_test_heap, 0) -> [f,{u,5},{u8,1}];
var_args(catch_end, 6) -> [{y,6}];
var_args(l_get, 5) -> [{a,mnesia_activity_state},t];
var_args(l_bs_skip_bits_all2, 1) -> [f,{x,3},{u8,8}];
var_args(l_wait_timeout, 2) -> [f,u32];
var_args(is_nonempty_list_test_heap, 1) -> [f,u8,u8];
var_args(l_call_ext, 70) -> [{e,{mnesia_schema,list2cs,1}}];
var_args(l_call_ext, 69) -> [{e,{gb_trees,insert,3}}];
var_args(l_move_call_only, 8) -> [f,{x,8}];
var_args(case_end, 9) -> [{y,0}];
var_args(is_pid, 0) -> [f,{x,0}];
var_args(l_new_bs_put_float_imm, 0) -> [f,{u,64},{u8,0},{x,0}];
var_args(l_move_call, 29) -> [{a,x},f];
var_args(l_select_tuple_arity, 2) -> [{x,0},f,{u,10}];
var_args(catch_end, 8) -> [t];
var_args(l_bif2, 6) -> [f,b,t];
var_args(bif2_body, 3) -> [b,t];
var_args(node, 3) -> [{x,3}];
var_args(bs_init_writable, 0) -> [];
var_args(l_call_ext, 73) -> [{e,{gb_trees,to_list,1}}];
var_args(l_call_ext, 72) -> [{e,{os,type,0}}];
var_args(l_call_ext, 71) -> [{e,{erl_syntax,atom_value,1}}];
var_args(extract_next_element, 21) -> [{y,6}];
var_args(move_jump, 7) -> [f,{x,3}];
var_args(move_return, 17) -> [{a,error}];
var_args(move_return, 16) -> [{x,5}];
var_args(l_new_bs_put_integer_imm, 1) -> [f,u32,u8,t];
var_args(bs_context_to_binary, 2) -> [{x,2}];
var_args(put_list, 11) -> [y8,t,y8];
var_args(is_nonempty_list, 25) -> [f,{x,19}];
var_args(is_nonempty_list, 24) -> [f,{y,9}];
var_args(try_end, 6) -> [{y,6}];
var_args(l_bs_private_append, 0) -> [f,u8,t];
var_args(deallocate_return, 9) -> [{u,9}];
var_args(l_move_call, 30) -> [{smallint,1000},f];
var_args(l_call_ext_only, 1) -> [{e,{asn1ct_gen,emit,1}}];
var_args(l_apply, 0) -> [];
var_args(l_move_call_ext, 27) -> [{a,funky},{e,{estone_SUITE,req,2}}];
var_args(l_bs_get_integer_imm, 0) -> [t,u8,u8,f,u8,x8];
var_args(test_heap_1_put_list, 3) -> [u8,y8];
var_args(self, 3) -> [{x,3}];
var_args(is_tuple, 5) -> [f,{x,7}];
var_args(l_call_ext, 76) -> [{e,{gb_trees,from_orddict,1}}];
var_args(l_call_ext, 75) -> [{e,{sets,is_element,2}}];
var_args(l_call_ext, 74) -> [{e,{erl_syntax,get_pos,1}}];
var_args(l_is_ne_exact_immed, 7) -> [f,{y,1},t];
var_args(extract_next_element3, 8) -> [{x,11}];
var_args(l_move_call, 33) -> [{smallint,4},f];
var_args(l_move_call, 32) -> [{smallint,42},f];
var_args(l_move_call, 31) -> [{y,8},f];
var_args(l_catch, 7) -> [{y,20},t];
var_args(catch_end, 7) -> [{y,20}];
var_args(l_is_ne_exact_immed, 8) -> [f,{y,2},t];
var_args(l_select_val2, 11) -> [{y,1},f,t,f,t,f];
var_args(l_fetch, 20) -> [t,{y,5}];
var_args(l_call_ext, 77) -> [{e,{erlang,max,2}}];
var_args(extract_next_element3, 9) -> [{x,10}];
var_args(move_jump, 8) -> [f,{a,false}];
var_args(l_bs_get_utf8, 1) -> [t,f,t];
var_args(is_nonempty_list, 26) -> [f,{y,0}];
var_args(is_binary, 2) -> [f,{x,2}];
var_args(l_fetch, 21) -> [t,{x,5}];
var_args(l_bs_skip_bits_all2, 2) -> [f,t,u8];
var_args(self, 4) -> [{y,0}];
var_args(l_call_ext, 80) -> [{e,{file,get_cwd,0}}];
var_args(l_call_ext, 79) -> [{e,{lists,sublist,3}}];
var_args(l_call_ext, 78) -> [{e,{file,write_file,2}}];
var_args(l_call_last, 10) -> [f,{u,10}];
var_args(l_new_bs_put_integer, 1) -> [f,t,u8,u8,t];
var_args(move_return, 18) -> [{a,ignore}];
var_args(is_nil, 20) -> [f,{y,0}];
var_args(recv_mark, 0) -> [f];
var_args(bs_context_to_binary, 3) -> [{y,0}];
var_args(badmatch, 10) -> [{y,9}];
var_args(badmatch, 9) -> [{x,5}];
var_args(is_function, 0) -> [f,{x,0}];
var_args(l_recv_set, 0) -> [];
var_args(l_bs_get_integer_16, 0) -> [t,f,t];
var_args(move2, 9) -> [{x,0},x8,{x,0},x8];
var_args(l_call_ext, 88) -> [{e,{ordsets,intersection,2}}];
var_args(l_call_ext, 87) -> [{e,{asn1ct_name,new,1}}];
var_args(l_call_ext, 86) -> [{e,{beam_utils,code_at,2}}];
var_args(l_call_ext, 85) -> [{e,{cerl,var_name,1}}];
var_args(l_call_ext, 84) -> [{e,{gb_sets,empty,0}}];
var_args(l_call_ext, 83) -> [{e,{xref_utils,xset,2}}];
var_args(l_call_ext, 82) -> [{e,{cerl,c_tuple,1}}];
var_args(l_call_ext, 81) -> [{e,{mnesia_lib,intersect,2}}];
var_args(l_is_eq_exact_immed, 30) -> [f,{x,19},t];
var_args(extract_next_element2, 16) -> [{x,15}];
var_args(l_bs_get_float2, 0) -> [f,t,u8,t,u8,u8,t];
var_args(move_jump, 9) -> [f,{a,asn1_NOVALUE}];
var_args(move_return, 19) -> [{smallint,5}];
var_args(l_trim, 11) -> [u32];
var_args(is_nil, 21) -> [f,{x,17}];
var_args(l_select_val2, 12) -> [{x,8},f,t,f,t,f];
var_args(is_atom, 5) -> [f,{x,5}];
var_args(l_move_call_ext, 34) -> [{a,auto_repair},{e,{mnesia_monitor,get_env,1}}];
var_args(is_float, 0) -> [f,{x,0}];
var_args(l_is_ne_exact_immed, 9) -> [f,{x,5},t];
var_args(l_minus, 2) -> [f,u8,t];
var_args(l_fast_element, 4) -> [{x,0},{u,3},{x,0}];
var_args(move_return, 22) -> [{smallint,16}];
var_args(move_return, 21) -> [{smallint,6}];
var_args(move_return, 20) -> [{a,none}];
var_args(is_nonempty_list, 28) -> [f,{y,6}];
var_args(is_nonempty_list, 27) -> [f,{x,24}];
var_args(l_bif1, 2) -> [f,b,t,t];
var_args(deallocate_return, 11) -> [{u,11}];
var_args(deallocate_return, 10) -> [{u,10}];
var_args(l_bs_init_bits, 0) -> [u32,u32,u8,t];
var_args(get_list, 8) -> [x8,y8,x8];
var_args(l_is_eq_exact_immed, 31) -> [f,{x,22},t];
var_args(get_list, 10) -> [t,t,t];
var_args(is_tuple, 6) -> [f,{x,5}];
var_args(l_call_last, 11) -> [f,u32];
var_args(extract_next_element, 22) -> [{x,18}];
var_args(move_return, 24) -> [{a,nomatch}];
var_args(move_return, 23) -> [{smallint,7}];
var_args(badmatch, 11) -> [{y,5}];
var_args(l_select_val2, 13) -> [{y,2},f,t,f,t,f];
var_args(l_call_ext_only, 2) -> [{e,{mnesia_monitor,get_env,1}}];
var_args(get_tuple_element, 10) -> [y8,u8,y8];
var_args(move, 12) -> [t,{x,8}];
var_args(l_gc_bif1, 7) -> [f,b,t,u8,t];
var_args(wait_timeout, 0) -> [f,t];
var_args(badmatch, 12) -> [{y,6}];
var_args(is_nonempty_list, 29) -> [f,{x,21}];
var_args(l_times, 2) -> [f,u8,t];
var_args(l_apply_fun, 0) -> [];
var_args(l_is_eq_exact_immed, 32) -> [f,{x,23},t];
var_args(l_is_eq_exact_immed, 33) -> [f,{x,20},t];
var_args(l_bs_test_tail_imm2, 0) -> [f,t,u32];
var_args(l_bs_get_integer_32, 2) -> [t,f,u8,t];
var_args(move_return, 25) -> [{smallint,64}];
var_args(is_nil, 22) -> [f,{y,5}];
var_args(badmatch, 13) -> [{y,1}];
var_args(is_integer_allocate, 1) -> [f,t,u32];
var_args(l_is_eq_exact_immed, 34) -> [f,{y,6},t];
var_args(get_list, 9) -> [x8,y8,{x,0}];
var_args(is_tuple, 8) -> [f,{x,6}];
var_args(is_tuple, 7) -> [f,{y,4}];
var_args(extract_next_element, 23) -> [{x,15}];
var_args(move_jump, 10) -> [f,{a,true}];
var_args(is_nil, 23) -> [f,{x,19}];
var_args(bs_context_to_binary, 4) -> [{x,4}];
var_args(badmatch, 14) -> [{x,8}];
var_args(is_nonempty_list, 32) -> [f,{y,8}];
var_args(is_nonempty_list, 31) -> [f,{x,22}];
var_args(is_nonempty_list, 30) -> [f,{x,25}];
var_args(l_bs_init_fail, 1) -> [u32,f,u8,t];
var_args(move_jump, 11) -> [f,{smallint,0}];
var_args(is_nil, 24) -> [f,{y,4}];
var_args(is_nonempty_list, 34) -> [f,{y,7}];
var_args(is_nonempty_list, 33) -> [f,{y,5}];
var_args(try_end, 7) -> [t];
var_args(init, 12) -> [{y,12}];
var_args(l_bs_add, 1) -> [f,u8,t];
var_args(l_wait_timeout, 0) -> [f,{u,1000}];
var_args(l_fast_element, 5) -> [t,u32,t];
var_args(test_heap_1_put_list, 4) -> [u32,t];
var_args(l_gc_bif2, 0) -> [f,b,u8,t];
var_args(l_bs_put_utf16, 0) -> [f,u8,t];
var_args(l_is_eq_exact_immed, 35) -> [f,{y,7},t];
var_args(move_return, 27) -> [{smallint,128}];
var_args(move_return, 26) -> [{x,6}];
var_args(l_trim, 9) -> [{u,11}];
var_args(is_nil, 25) -> [f,{x,22}];
var_args(l_bs_validate_unicode, 0) -> [f,t];
var_args(is_nonempty_list, 35) -> [f,{x,26}];
var_args(l_bs_init, 0) -> [u32,u32,u8,t];
var_args(l_jump_on_val, 1) -> [{x,0},f,u8,u8];
var_args(move, 11) -> [nil,{x,10}];
var_args(l_bs_utf16_size, 0) -> [t,t];
var_args(l_bs_get_binary2, 2) -> [f,t,u8,t,u8,u8,t];
var_args(l_bs_restore2, 3) -> [t,u32];
var_args(is_nil, 26) -> [f,{x,18}];
var_args(raise, 1) -> [t,t];
var_args(l_int_bnot, 0) -> [f,t,u8,t];
var_args(is_nil, 29) -> [f,{x,21}];
var_args(is_nil, 28) -> [f,{x,20}];
var_args(is_nil, 27) -> [f,{x,23}];
var_args(is_nonempty_list, 37) -> [f,{x,27}];
var_args(is_nonempty_list, 36) -> [f,{x,23}];
var_args(l_bs_save2, 2) -> [t,u32];
var_args(l_bs_get_binary_imm2, 1) -> [f,{x,0},u8,u8,u8,x8];
var_args(is_bitstr, 0) -> [f,t];
var_args(l_new_bs_put_binary_all, 2) -> [f,t,u8];
var_args(l_new_bs_put_binary, 0) -> [f,t,u8,u8,t];
var_args(fmove_2, 2) -> [fr,t];
var_args(is_reference, 0) -> [f,t];
var_args(is_port, 0) -> [f,t];
var_args(is_number, 0) -> [f,t];
var_args(move, 13) -> [t,t];
var_args(l_bs_get_binary_all_reuse, 1) -> [t,f,u8];
var_args(init, 13) -> [{y,13}];
var_args(l_wait_timeout, 1) -> [f,{u,1}];
var_args(l_select_tuple_arity, 4) -> [t,f,u32];
var_args(l_trim, 10) -> [{u,10}];
var_args(l_bs_put_utf8, 0) -> [f,t];
var_args(init, 14) -> [{y,14}];
var_args(l_fnegate, 0) -> [fr,fr];
var_args(l_bs_get_integer_imm, 1) -> [t,u32,u8,f,u8,t];
var_args(l_jump_on_val, 2) -> [t,f,u32,u32];
var_args(l_bs_utf8_size, 0) -> [t,t];
var_args(l_bs_get_binary_imm2, 2) -> [f,t,u8,u32,u8,t];
var_args(l_bs_validate_unicode_retract, 0) -> [f];
var_args(l_bxor, 0) -> [f,u8,t];
var_args(l_new_bs_put_float, 0) -> [f,t,u8,u8,t];
var_args(l_apply_last, 0) -> [u32];
var_args(l_is_function2, 0) -> [f,t,u8];
var_args(l_gc_bif3, 0) -> [f,b,t,u8,t];
var_args(l_bor, 2) -> [f,u8,t];
var_args(l_new_bs_put_binary_imm, 0) -> [f,u32,u8,t];
var_args(l_bs_get_integer_8, 2) -> [t,f,t];
var_args(l_bs_start_match2, 4) -> [t,f,u8,u32,t];
var_args(l_rem, 2) -> [f,u8,t];
var_args(l_bs_get_integer_small_imm, 1) -> [t,u32,f,u8,t];
var_args(l_bsl, 2) -> [f,u8,t];
var_args(l_apply_only, 0) -> [];
var_args(on_load, 0) -> [];
var_args(move2, 10) -> [t,t,t,t];
var_args(l_int_div, 2) -> [f,u8,t];
var_args(l_bs_test_unit, 0) -> [f,t,u8];
var_args(l_m_div, 0) -> [f,u8,t];
var_args(l_hibernate, 0) -> [];
var_args(l_apply_fun_last, 0) -> [u32];
var_args(is_function2, 0) -> [f,t,t];
var_args(l_apply_fun_only, 0) -> [];
var_args(l_band, 2) -> [f,u8,t];
var_args(is_bigint, 0) -> [f,t];
var_args(test_heap, 1) -> [u32,u8];
var_args(func_info, 0) -> [t,t,u8];
var_args(call_bif, 0) -> [{b,{erlang,purge_module,1}}];
var_args(l_bs_get_utf16, 2) -> [t,f,u8,t];
var_args(l_put_tuple, 7) -> [t,u32];
var_args(get_tuple_element, 11) -> [t,u32,t];
var_args(allocate_init, 1) -> [u32,t];
var_args(l_call_fun_last, 1) -> [u8,u32];
var_args(set_tuple_element, 2) -> [t,t,u32];
var_args(allocate_heap, 1) -> [u32,u32,u8];
var_args(is_tuple_of_arity, 4) -> [f,t,u32];
var_args(test_arity, 4) -> [f,t,u32];
var_args(l_bs_match_string, 4) -> [t,f,u32,str];
var_args(is_nonempty_list_allocate, 2) -> [f,t,u32];
var_args(l_bs_append, 2) -> [f,u32,u8,u8,t];
var_args(try_case_end, 1) -> [t];
var_args(init3, 1) -> [t,t,t];
var_args(l_select_val_smallints, 2) -> [t,f,u32];
var_args(l_select_tuple_arity2, 3) -> [t,f,u32,f,u32,f];
var_args(init2, 1) -> [t,t];
var_args(l_bs_get_binary_all2, 2) -> [f,t,u8,u8,t];
var_args(is_nonempty_list_test_heap, 2) -> [f,u32,u8];
var_args(allocate_heap_zero, 1) -> [u32,u32,u8];
var_args(l_bs_init_heap_bin, 1) -> [u32,u32,u8,t];
var_args(l_plus, 3) -> [f,u8,t];
var_args(l_bs_get_integer, 1) -> [f,u8,u8,u8,t];

var_args(Op, No) -> erlang:error({novar,Op,No}).

var_index(move, 0) -> 0;
var_index(move, 1) -> 1;
var_index(l_call, 0) -> 2;
var_index(test_heap, 0) -> 3;
var_index(move, 2) -> 4;
var_index(badmatch, 0) -> 5;
var_index(move, 3) -> 6;
var_index(l_put_tuple, 0) -> 7;
var_index(move, 4) -> 8;
var_index(move2, 1) -> 9;
var_index(get_tuple_element, 0) -> 10;
var_index(call_bif, 7) -> 11;
var_index(l_bs_start_match2, 0) -> 12;
var_index(l_bs_test_zero_tail2, 0) -> 13;
var_index(l_bs_match_string, 0) -> 14;
var_index(put_list, 0) -> 15;
var_index(is_tuple_of_arity, 1) -> 16;
var_index(get_tuple_element, 1) -> 17;
var_index(move2, 0) -> 18;
var_index(l_call_only, 0) -> 19;
var_index(l_is_eq_exact_immed, 0) -> 20;
var_index(l_is_eq_exact_immed, 1) -> 21;
var_index(is_tuple_of_arity, 0) -> 22;
var_index(l_move_call_ext, 0) -> 23;
var_index(put_list, 1) -> 24;
var_index(move, 5) -> 25;
var_index(get_list, 0) -> 26;
var_index(l_put_tuple, 1) -> 27;
var_index(l_call_ext, 89) -> 28;
var_index(l_is_ge, 0) -> 29;
var_index(l_make_fun, 0) -> 30;
var_index(l_move_call_last, 0) -> 31;
var_index(extract_next_element2, 0) -> 32;
var_index(is_tuple_of_arity, 2) -> 33;
var_index(return, 0) -> 34;
var_index(l_move_call_ext, 1) -> 35;
var_index(l_fetch, 0) -> 36;
var_index(call_bif, 3) -> 37;
var_index(move_deallocate_return, 0) -> 38;
var_index(l_trim, 0) -> 39;
var_index(l_allocate, 0) -> 40;
var_index(move2, 3) -> 41;
var_index(l_fetch, 1) -> 42;
var_index(put_list, 2) -> 43;
var_index(is_nil, 0) -> 44;
var_index(is_nonempty_list, 0) -> 45;
var_index(extract_next_element, 0) -> 46;
var_index(move_return, 28) -> 47;
var_index(l_allocate, 1) -> 48;
var_index(l_is_eq_exact_immed, 2) -> 49;
var_index(jump, 0) -> 50;
var_index(deallocate_return, 0) -> 51;
var_index(get_list, 1) -> 52;
var_index(case_end, 0) -> 53;
var_index(call_bif, 8) -> 54;
var_index(move2, 2) -> 55;
var_index(l_move_call, 34) -> 56;
var_index(l_is_eq_exact, 0) -> 57;
var_index(get_tuple_element, 2) -> 58;
var_index(l_allocate, 2) -> 59;
var_index(move_return, 0) -> 60;
var_index(l_is_eq_exact_immed, 3) -> 61;
var_index(l_select_val2, 0) -> 62;
var_index(l_fetch, 2) -> 63;
var_index(get_tuple_element, 3) -> 64;
var_index(init2, 0) -> 65;
var_index(get_list, 2) -> 66;
var_index(l_is_eq_exact_immed, 4) -> 67;
var_index(init, 0) -> 68;
var_index(deallocate_return, 1) -> 69;
var_index(put_list, 3) -> 70;
var_index(call_bif, 9) -> 71;
var_index(extract_next_element, 1) -> 72;
var_index(is_tuple_of_arity, 3) -> 73;
var_index(l_is_eq_exact_literal, 0) -> 74;
var_index(get_tuple_element, 4) -> 75;
var_index(move2, 4) -> 76;
var_index(l_move_call_ext, 41) -> 77;
var_index(l_fetch, 3) -> 78;
var_index(init, 1) -> 79;
var_index(l_put_tuple, 2) -> 80;
var_index(move2, 5) -> 81;
var_index(l_allocate, 3) -> 82;
var_index(deallocate_return, 2) -> 83;
var_index(l_fetch, 4) -> 84;
var_index(init3, 0) -> 85;
var_index(is_nonempty_list, 1) -> 86;
var_index(l_is_eq_exact_immed, 5) -> 87;
var_index(l_allocate_zero, 0) -> 88;
var_index(call_bif, 6) -> 89;
var_index(l_trim, 1) -> 90;
var_index(allocate_init, 0) -> 91;
var_index(call_bif, 45) -> 92;
var_index(allocate_heap, 0) -> 93;
var_index(test_heap_1_put_list, 0) -> 94;
var_index(l_allocate_zero, 1) -> 95;
var_index(move_deallocate_return, 1) -> 96;
var_index(init, 2) -> 97;
var_index(l_call_last, 0) -> 98;
var_index(move_return, 1) -> 99;
var_index(is_nonempty_list, 2) -> 100;
var_index(l_move_call, 0) -> 101;
var_index(is_tuple, 0) -> 102;
var_index(is_list, 0) -> 103;
var_index(l_is_eq_exact_immed, 6) -> 104;
var_index(l_call_last, 1) -> 105;
var_index(deallocate_return, 3) -> 106;
var_index(is_nonempty_list_allocate, 0) -> 107;
var_index(l_move_call_only, 0) -> 108;
var_index(call_bif, 5) -> 109;
var_index(extract_next_element, 2) -> 110;
var_index(l_increment, 0) -> 111;
var_index(l_gc_bif1, 0) -> 112;
var_index(move, 6) -> 113;
var_index(l_is_lt, 0) -> 114;
var_index(l_trim, 2) -> 115;
var_index(l_select_val_atoms, 0) -> 116;
var_index(l_call_last, 2) -> 117;
var_index(move_deallocate_return, 2) -> 118;
var_index(is_nonempty_list, 3) -> 119;
var_index(l_new_bs_put_integer_imm, 0) -> 120;
var_index(is_nil, 1) -> 121;
var_index(extract_next_element2, 1) -> 122;
var_index(l_move_call_only, 9) -> 123;
var_index(l_select_val2, 2) -> 124;
var_index(remove_message, 0) -> 125;
var_index(l_move_call_only, 1) -> 126;
var_index(init, 3) -> 127;
var_index(l_catch, 0) -> 128;
var_index(l_allocate_zero, 2) -> 129;
var_index(extract_next_element3, 0) -> 130;
var_index(get_tuple_element, 5) -> 131;
var_index(l_call_ext, 0) -> 132;
var_index(l_fetch, 5) -> 133;
var_index(move_jump, 12) -> 134;
var_index(extract_next_element, 3) -> 135;
var_index(is_nil, 2) -> 136;
var_index(move2, 6) -> 137;
var_index(l_allocate, 4) -> 138;
var_index(catch_end, 0) -> 139;
var_index(test_arity, 0) -> 140;
var_index(l_allocate_zero, 3) -> 141;
var_index(l_bs_start_match2, 1) -> 142;
var_index(l_is_eq_exact_immed, 7) -> 143;
var_index(move_return, 2) -> 144;
var_index(put_list, 4) -> 145;
var_index(move_return, 3) -> 146;
var_index(l_move_call_ext, 2) -> 147;
var_index(move_return, 4) -> 148;
var_index(deallocate_return, 4) -> 149;
var_index(l_call_last, 3) -> 150;
var_index(move2, 7) -> 151;
var_index(l_is_eq_exact_immed, 8) -> 152;
var_index(l_plus, 0) -> 153;
var_index(move, 7) -> 154;
var_index(l_put_tuple, 3) -> 155;
var_index(call_bif, 2) -> 156;
var_index(l_select_tuple_arity2, 0) -> 157;
var_index(is_nonempty_list, 4) -> 158;
var_index(init, 4) -> 159;
var_index(is_nonempty_list, 5) -> 160;
var_index(get_list, 3) -> 161;
var_index(l_call_fun, 0) -> 162;
var_index(l_call_last, 4) -> 163;
var_index(l_move_call, 1) -> 164;
var_index(move_return, 5) -> 165;
var_index(l_bs_get_binary_all_reuse, 0) -> 166;
var_index(test_arity, 1) -> 167;
var_index(bif1_body, 0) -> 168;
var_index(l_move_call_only, 2) -> 169;
var_index(l_move_call_last, 1) -> 170;
var_index(is_nonempty_list, 6) -> 171;
var_index(l_bs_test_zero_tail2, 1) -> 172;
var_index(l_is_eq, 0) -> 173;
var_index(send, 0) -> 174;
var_index(set_tuple_element, 0) -> 175;
var_index(l_catch, 1) -> 176;
var_index(l_call_ext_only, 3) -> 177;
var_index(call_bif, 10) -> 178;
var_index(l_is_ne, 0) -> 179;
var_index(l_move_call_only, 3) -> 180;
var_index(l_select_val2, 1) -> 181;
var_index(l_move_call_ext_last, 0) -> 182;
var_index(l_select_val_smallints, 0) -> 183;
var_index(l_move_call_ext, 3) -> 184;
var_index(l_is_ne_exact_immed, 0) -> 185;
var_index(l_increment, 1) -> 186;
var_index(l_bs_add, 0) -> 187;
var_index(bif2_body, 0) -> 188;
var_index(is_nonempty_list, 7) -> 189;
var_index(extract_next_element, 4) -> 190;
var_index(case_end, 1) -> 191;
var_index(l_bs_match_string, 1) -> 192;
var_index(l_is_eq_exact_immed, 9) -> 193;
var_index(extract_next_element2, 2) -> 194;
var_index(move_deallocate_return, 3) -> 195;
var_index(extract_next_element2, 3) -> 196;
var_index(l_loop_rec, 0) -> 197;
var_index(l_call_ext, 1) -> 198;
var_index(l_select_tuple_arity2, 1) -> 199;
var_index(l_move_call, 2) -> 200;
var_index(l_move_call, 3) -> 201;
var_index(l_move_call_ext, 4) -> 202;
var_index(l_bs_get_utf16, 0) -> 203;
var_index(l_select_val_atoms, 1) -> 204;
var_index(l_trim, 3) -> 205;
var_index(init, 5) -> 206;
var_index(l_bs_restore2, 0) -> 207;
var_index(catch_end, 1) -> 208;
var_index(is_nil, 3) -> 209;
var_index(l_move_call, 4) -> 210;
var_index(l_bs_init_heap_bin, 0) -> 211;
var_index(is_nil, 4) -> 212;
var_index(is_nonempty_list, 8) -> 213;
var_index(wait, 0) -> 214;
var_index(l_call_ext_last, 0) -> 215;
var_index(l_allocate_zero, 4) -> 216;
var_index(loop_rec_end, 0) -> 217;
var_index(call_bif, 11) -> 218;
var_index(deallocate_return, 5) -> 219;
var_index(move, 8) -> 220;
var_index(l_fetch, 6) -> 221;
var_index(is_nonempty_list, 9) -> 222;
var_index(extract_next_element, 5) -> 223;
var_index(l_bs_start_match2, 2) -> 224;
var_index(l_allocate_zero, 5) -> 225;
var_index(l_select_val2, 3) -> 226;
var_index(l_allocate, 5) -> 227;
var_index(init, 6) -> 228;
var_index(l_move_call_ext, 5) -> 229;
var_index(badmatch, 1) -> 230;
var_index(l_plus, 1) -> 231;
var_index(l_call_last, 5) -> 232;
var_index(l_move_call_ext, 6) -> 233;
var_index(l_call_last, 6) -> 234;
var_index(l_move_call_ext, 7) -> 235;
var_index(badmatch, 2) -> 236;
var_index(l_move_call_ext, 8) -> 237;
var_index(l_call_fun_last, 0) -> 238;
var_index(l_move_call_ext, 9) -> 239;
var_index(l_increment, 2) -> 240;
var_index(bs_context_to_binary, 0) -> 241;
var_index(call_bif, 12) -> 242;
var_index(int_code_end, 0) -> 243;
var_index(l_trim, 4) -> 244;
var_index(l_move_call_ext_only, 0) -> 245;
var_index(put_list, 5) -> 246;
var_index(l_bs_save2, 0) -> 247;
var_index(call_bif, 13) -> 248;
var_index(extract_next_element, 6) -> 249;
var_index(move_deallocate_return, 4) -> 250;
var_index(l_is_ne_exact_immed, 1) -> 251;
var_index(extract_next_element2, 4) -> 252;
var_index(l_select_val2, 4) -> 253;
var_index(l_gc_bif1, 1) -> 254;
var_index(l_increment, 3) -> 255;
var_index(self, 0) -> 256;
var_index(l_put_tuple, 4) -> 257;
var_index(extract_next_element, 7) -> 258;
var_index(get_tuple_element, 6) -> 259;
var_index(call_bif, 14) -> 260;
var_index(is_tuple, 1) -> 261;
var_index(l_bs_test_unit_8, 0) -> 262;
var_index(l_move_call_last, 2) -> 263;
var_index(badmatch, 3) -> 264;
var_index(l_move_call, 5) -> 265;
var_index(l_is_eq_exact_immed, 11) -> 266;
var_index(l_move_call, 6) -> 267;
var_index(is_nil, 5) -> 268;
var_index(l_bs_test_zero_tail2, 2) -> 269;
var_index(is_nonempty_list_allocate, 1) -> 270;
var_index(l_call_ext, 2) -> 271;
var_index(l_bs_test_zero_tail2, 3) -> 272;
var_index(l_catch, 2) -> 273;
var_index(l_move_call_ext, 10) -> 274;
var_index(call_bif, 15) -> 275;
var_index(is_atom, 0) -> 276;
var_index(l_move_call_ext_only, 6) -> 277;
var_index(l_fast_element, 0) -> 278;
var_index(is_nonempty_list, 10) -> 279;
var_index(l_select_val_smallints, 1) -> 280;
var_index(call_bif, 16) -> 281;
var_index(l_call_ext, 3) -> 282;
var_index(move_return, 6) -> 283;
var_index(l_call_ext_last, 1) -> 284;
var_index(is_nil, 6) -> 285;
var_index(allocate_heap_zero, 0) -> 286;
var_index(is_nonempty_list, 11) -> 287;
var_index(l_call_ext_last, 2) -> 288;
var_index(call_bif, 17) -> 289;
var_index(self, 1) -> 290;
var_index(extract_next_element, 8) -> 291;
var_index(init, 7) -> 292;
var_index(case_end, 2) -> 293;
var_index(l_minus, 0) -> 294;
var_index(extract_next_element3, 1) -> 295;
var_index(is_nil, 7) -> 296;
var_index(l_move_call, 7) -> 297;
var_index(call_bif, 18) -> 298;
var_index(raise, 0) -> 299;
var_index(catch_end, 2) -> 300;
var_index(l_is_eq_exact_literal, 1) -> 301;
var_index(l_is_eq_exact_literal, 7) -> 302;
var_index(l_move_call_ext, 11) -> 303;
var_index(extract_next_element2, 5) -> 304;
var_index(extract_next_element, 24) -> 305;
var_index(l_allocate, 6) -> 306;
var_index(l_bif2, 0) -> 307;
var_index(try_end, 0) -> 308;
var_index(l_call_fun, 1) -> 309;
var_index(call_bif, 19) -> 310;
var_index(deallocate_return, 6) -> 311;
var_index(l_call_ext, 4) -> 312;
var_index(l_move_call_last, 3) -> 313;
var_index(l_move_call_ext_only, 1) -> 314;
var_index(l_put_tuple, 5) -> 315;
var_index(l_band, 0) -> 316;
var_index(l_move_call, 8) -> 317;
var_index(l_trim, 5) -> 318;
var_index(extract_next_element, 9) -> 319;
var_index(call_bif, 20) -> 320;
var_index(l_call_ext, 5) -> 321;
var_index(l_call_ext, 6) -> 322;
var_index(l_minus, 1) -> 323;
var_index(l_bs_get_binary_all2, 0) -> 324;
var_index(get_tuple_element, 7) -> 325;
var_index(l_is_eq_exact_immed, 12) -> 326;
var_index(test_heap_1_put_list, 1) -> 327;
var_index(extract_next_element3, 2) -> 328;
var_index(is_integer, 0) -> 329;
var_index(extract_next_element2, 6) -> 330;
var_index(l_catch, 3) -> 331;
var_index(is_nil, 8) -> 332;
var_index(l_bif2, 1) -> 333;
var_index(move_deallocate_return, 5) -> 334;
var_index(l_move_call_only, 4) -> 335;
var_index(l_bsr, 0) -> 336;
var_index(move_jump, 0) -> 337;
var_index(is_list, 1) -> 338;
var_index(l_move_call, 9) -> 339;
var_index(l_bs_get_integer_small_imm, 0) -> 340;
var_index(l_is_eq_exact_immed, 13) -> 341;
var_index(apply, 0) -> 342;
var_index(l_call_ext, 7) -> 343;
var_index(l_fast_element, 2) -> 344;
var_index(l_bs_get_integer_8, 0) -> 345;
var_index(l_bif2, 2) -> 346;
var_index(l_fetch, 7) -> 347;
var_index(set_tuple_element, 1) -> 348;
var_index(try_end, 1) -> 349;
var_index(l_is_eq_exact_literal, 2) -> 350;
var_index(l_is_eq_exact_immed, 10) -> 351;
var_index(extract_next_element2, 7) -> 352;
var_index(l_is_eq_exact_literal, 3) -> 353;
var_index(l_bsl, 0) -> 354;
var_index(l_allocate_zero, 6) -> 355;
var_index(is_nonempty_list, 12) -> 356;
var_index(l_times, 0) -> 357;
var_index(l_select_tuple_arity, 0) -> 358;
var_index(l_fmul, 0) -> 359;
var_index(l_call_ext, 8) -> 360;
var_index(l_bs_match_string, 2) -> 361;
var_index(move, 9) -> 362;
var_index(call_bif, 21) -> 363;
var_index(l_move_call_ext, 12) -> 364;
var_index(l_put_tuple, 6) -> 365;
var_index(l_times, 1) -> 366;
var_index(l_bs_init_fail, 0) -> 367;
var_index(l_move_call_ext_only, 2) -> 368;
var_index(l_is_eq_exact_immed, 14) -> 369;
var_index(l_is_eq_exact_immed, 15) -> 370;
var_index(l_move_call_ext, 14) -> 371;
var_index(l_move_call_ext, 13) -> 372;
var_index(l_call_ext, 9) -> 373;
var_index(call_bif, 22) -> 374;
var_index(extract_next_element, 10) -> 375;
var_index(is_nil, 9) -> 376;
var_index(l_fetch, 8) -> 377;
var_index(node, 0) -> 378;
var_index(l_call_last, 7) -> 379;
var_index(l_bs_get_binary2, 0) -> 380;
var_index(is_tuple, 2) -> 381;
var_index(l_call_fun, 2) -> 382;
var_index(get_list, 4) -> 383;
var_index(test_arity, 2) -> 384;
var_index(l_bs_get_integer_8, 1) -> 385;
var_index(l_bs_test_zero_tail2, 5) -> 386;
var_index(catch_end, 3) -> 387;
var_index(l_bif2, 3) -> 388;
var_index(l_is_ne_exact_immed, 2) -> 389;
var_index(l_allocate_zero, 9) -> 390;
var_index(call_bif, 23) -> 391;
var_index(l_is_ne_exact, 0) -> 392;
var_index(l_bif2, 4) -> 393;
var_index(is_binary, 0) -> 394;
var_index(l_is_eq_exact_immed, 16) -> 395;
var_index(l_bs_get_integer_32, 0) -> 396;
var_index(extract_next_element, 11) -> 397;
var_index(l_call_ext, 10) -> 398;
var_index(is_atom, 1) -> 399;
var_index(l_select_val2, 7) -> 400;
var_index(l_fetch, 9) -> 401;
var_index(l_fcheckerror, 0) -> 402;
var_index(l_new_bs_put_binary_all, 0) -> 403;
var_index(fclearerror, 0) -> 404;
var_index(extract_next_element3, 3) -> 405;
var_index(system_limit, 0) -> 406;
var_index(node, 1) -> 407;
var_index(is_nonempty_list, 38) -> 408;
var_index(extract_next_element, 12) -> 409;
var_index(get_list, 5) -> 410;
var_index(l_move_call, 10) -> 411;
var_index(move_deallocate_return, 6) -> 412;
var_index(l_move_call_last, 4) -> 413;
var_index(l_new_bs_put_binary_all, 1) -> 414;
var_index(l_is_eq_exact_immed, 17) -> 415;
var_index(timeout, 0) -> 416;
var_index(deallocate_return, 7) -> 417;
var_index(l_get, 0) -> 418;
var_index(l_select_val2, 14) -> 419;
var_index(l_fetch, 10) -> 420;
var_index(l_move_call, 11) -> 421;
var_index(l_move_call_ext_last, 1) -> 422;
var_index(is_nil, 10) -> 423;
var_index(l_fetch, 11) -> 424;
var_index(l_select_val2, 5) -> 425;
var_index(is_float, 1) -> 426;
var_index(call_bif, 24) -> 427;
var_index(l_call_ext, 11) -> 428;
var_index(l_is_eq_exact_immed, 36) -> 429;
var_index(l_select_val2, 8) -> 430;
var_index(l_get, 1) -> 431;
var_index(call_bif, 25) -> 432;
var_index(l_bs_restore2, 1) -> 433;
var_index(l_move_call, 12) -> 434;
var_index(l_band, 1) -> 435;
var_index(l_bsl, 1) -> 436;
var_index(l_fast_element, 1) -> 437;
var_index(is_binary, 1) -> 438;
var_index(l_move_call_ext, 16) -> 439;
var_index(l_get, 2) -> 440;
var_index(extract_next_element, 13) -> 441;
var_index(is_nonempty_list, 13) -> 442;
var_index(l_is_eq_exact_immed, 18) -> 443;
var_index(l_call_ext, 12) -> 444;
var_index(l_move_call, 13) -> 445;
var_index(l_move_call_ext, 17) -> 446;
var_index(l_rem, 0) -> 447;
var_index(move2, 8) -> 448;
var_index(l_call_ext, 13) -> 449;
var_index(l_allocate_zero, 7) -> 450;
var_index(l_call_last, 8) -> 451;
var_index(is_nil, 11) -> 452;
var_index(l_gc_bif1, 2) -> 453;
var_index(l_is_ne_exact_immed, 10) -> 454;
var_index(l_fetch, 22) -> 455;
var_index(l_increment, 4) -> 456;
var_index(extract_next_element3, 4) -> 457;
var_index(is_nil, 30) -> 458;
var_index(extract_next_element3, 10) -> 459;
var_index(l_bs_append, 0) -> 460;
var_index(is_nonempty_list, 14) -> 461;
var_index(is_integer, 5) -> 462;
var_index(l_move_call_ext, 18) -> 463;
var_index(call_bif, 26) -> 464;
var_index(l_trim, 6) -> 465;
var_index(is_nil, 12) -> 466;
var_index(l_call_ext, 14) -> 467;
var_index(l_bor, 0) -> 468;
var_index(move_return, 7) -> 469;
var_index(is_list, 2) -> 470;
var_index(l_call_ext, 15) -> 471;
var_index(is_nil, 13) -> 472;
var_index(l_catch, 4) -> 473;
var_index(l_fadd, 0) -> 474;
var_index(l_gc_bif1, 5) -> 475;
var_index(l_element, 1) -> 476;
var_index(extract_next_element2, 17) -> 477;
var_index(call_bif, 27) -> 478;
var_index(l_allocate, 7) -> 479;
var_index(l_move_call_only, 5) -> 480;
var_index(l_move_call_ext_last, 4) -> 481;
var_index(l_move_call_ext, 20) -> 482;
var_index(l_move_call_ext, 19) -> 483;
var_index(is_nonempty_list, 15) -> 484;
var_index(call_bif, 29) -> 485;
var_index(call_bif, 28) -> 486;
var_index(is_integer, 1) -> 487;
var_index(bif1_body, 1) -> 488;
var_index(l_call_ext, 16) -> 489;
var_index(l_is_ne_exact_immed, 3) -> 490;
var_index(is_nonempty_list, 16) -> 491;
var_index(l_is_eq_exact_immed, 19) -> 492;
var_index(l_call_ext_last, 3) -> 493;
var_index(l_move_call_ext, 21) -> 494;
var_index(l_fetch, 12) -> 495;
var_index(fmove_1, 0) -> 496;
var_index(l_move_call_ext, 22) -> 497;
var_index(bif1_body, 2) -> 498;
var_index(move_jump, 1) -> 499;
var_index(l_bs_get_utf8, 0) -> 500;
var_index(case_end, 10) -> 501;
var_index(bif2_body, 1) -> 502;
var_index(l_move_call_ext, 23) -> 503;
var_index(l_bs_skip_bits_all2, 0) -> 504;
var_index(l_call_ext, 17) -> 505;
var_index(l_is_eq_exact_immed, 20) -> 506;
var_index(fconv, 0) -> 507;
var_index(l_bor, 1) -> 508;
var_index(call_bif, 30) -> 509;
var_index(l_is_eq_exact_literal, 4) -> 510;
var_index(l_move_call_ext_last, 2) -> 511;
var_index(l_bs_init_bits_fail, 0) -> 512;
var_index(call_bif, 31) -> 513;
var_index(extract_next_element3, 5) -> 514;
var_index(extract_next_element2, 8) -> 515;
var_index(l_is_eq_exact_immed, 22) -> 516;
var_index(apply_last, 0) -> 517;
var_index(l_move_call_ext_only, 3) -> 518;
var_index(call_bif, 32) -> 519;
var_index(call_bif, 4) -> 520;
var_index(is_atom, 2) -> 521;
var_index(call_bif, 33) -> 522;
var_index(put_list, 6) -> 523;
var_index(put_list, 8) -> 524;
var_index(l_bs_match_string, 3) -> 525;
var_index(l_make_export, 0) -> 526;
var_index(extract_next_element, 14) -> 527;
var_index(l_catch, 5) -> 528;
var_index(init, 8) -> 529;
var_index(l_increment, 8) -> 530;
var_index(move_deallocate_return, 7) -> 531;
var_index(l_call_fun, 3) -> 532;
var_index(l_select_val2, 6) -> 533;
var_index(l_new_bs_put_integer, 0) -> 534;
var_index(fmove_2, 0) -> 535;
var_index(call_bif, 34) -> 536;
var_index(badmatch, 4) -> 537;
var_index(is_atom, 3) -> 538;
var_index(l_move_call, 14) -> 539;
var_index(fmove_2, 1) -> 540;
var_index(l_bs_test_zero_tail2, 4) -> 541;
var_index(fmove_1, 1) -> 542;
var_index(l_move_call_ext, 24) -> 543;
var_index(is_integer_allocate, 0) -> 544;
var_index(l_call_ext_last, 4) -> 545;
var_index(call_bif, 35) -> 546;
var_index(l_fetch, 13) -> 547;
var_index(l_fast_element, 3) -> 548;
var_index(l_call_ext, 18) -> 549;
var_index(l_move_call, 15) -> 550;
var_index(l_move_call_ext, 25) -> 551;
var_index(is_tuple, 9) -> 552;
var_index(l_trim, 7) -> 553;
var_index(is_list, 6) -> 554;
var_index(l_fetch, 14) -> 555;
var_index(deallocate_return, 8) -> 556;
var_index(l_is_eq_exact_immed, 23) -> 557;
var_index(l_call_ext, 19) -> 558;
var_index(extract_next_element, 15) -> 559;
var_index(l_fetch, 15) -> 560;
var_index(l_move_call_last, 5) -> 561;
var_index(is_tuple, 3) -> 562;
var_index(is_nonempty_list, 17) -> 563;
var_index(l_move_call_ext, 15) -> 564;
var_index(l_get, 3) -> 565;
var_index(extract_next_element, 16) -> 566;
var_index(extract_next_element2, 9) -> 567;
var_index(try_end, 2) -> 568;
var_index(if_end, 0) -> 569;
var_index(fmove_1, 2) -> 570;
var_index(call_bif, 36) -> 571;
var_index(move_return, 8) -> 572;
var_index(l_move_call, 16) -> 573;
var_index(l_is_ne_exact_immed, 4) -> 574;
var_index(l_bs_skip_bits_imm2, 0) -> 575;
var_index(call_bif, 38) -> 576;
var_index(call_bif, 37) -> 577;
var_index(move_jump, 2) -> 578;
var_index(try_end, 4) -> 579;
var_index(try_end, 3) -> 580;
var_index(l_move_call_ext_last, 3) -> 581;
var_index(call_bif, 39) -> 582;
var_index(self, 2) -> 583;
var_index(l_call_ext, 23) -> 584;
var_index(l_call_ext, 22) -> 585;
var_index(l_call_ext, 21) -> 586;
var_index(l_call_ext, 20) -> 587;
var_index(is_list, 3) -> 588;
var_index(is_nonempty_list, 19) -> 589;
var_index(is_nonempty_list, 18) -> 590;
var_index(try_end, 5) -> 591;
var_index(catch_end, 4) -> 592;
var_index(l_call_ext, 25) -> 593;
var_index(l_call_ext, 24) -> 594;
var_index(extract_next_element2, 10) -> 595;
var_index(init, 9) -> 596;
var_index(l_bif1, 0) -> 597;
var_index(l_call_ext, 26) -> 598;
var_index(get_tuple_element, 8) -> 599;
var_index(l_select_val_atoms, 2) -> 600;
var_index(test_arity, 3) -> 601;
var_index(case_end, 3) -> 602;
var_index(bif1_body, 3) -> 603;
var_index(l_is_eq_exact_immed, 24) -> 604;
var_index(l_select_val2, 9) -> 605;
var_index(l_bs_get_utf16, 1) -> 606;
var_index(is_tuple, 4) -> 607;
var_index(l_call_ext, 28) -> 608;
var_index(l_call_ext, 27) -> 609;
var_index(extract_next_element, 17) -> 610;
var_index(l_move_call_ext, 26) -> 611;
var_index(l_get, 6) -> 612;
var_index(l_call_ext, 29) -> 613;
var_index(is_integer, 2) -> 614;
var_index(badmatch, 5) -> 615;
var_index(l_bs_put_string, 1) -> 616;
var_index(try_case_end, 0) -> 617;
var_index(l_fdiv, 0) -> 618;
var_index(get_list, 6) -> 619;
var_index(l_call_ext_last, 6) -> 620;
var_index(l_bif1, 1) -> 621;
var_index(put_list, 9) -> 622;
var_index(move_return, 9) -> 623;
var_index(case_end, 4) -> 624;
var_index(self, 5) -> 625;
var_index(l_call_ext, 30) -> 626;
var_index(l_bs_test_unit_8, 1) -> 627;
var_index(l_gc_bif1, 3) -> 628;
var_index(move_deallocate_return, 9) -> 629;
var_index(l_is_eq_exact_immed, 25) -> 630;
var_index(call_bif, 41) -> 631;
var_index(call_bif, 40) -> 632;
var_index(extract_next_element, 18) -> 633;
var_index(extract_next_element2, 11) -> 634;
var_index(is_nil, 14) -> 635;
var_index(l_move_call_only, 6) -> 636;
var_index(l_bs_restore2, 2) -> 637;
var_index(l_move_call, 18) -> 638;
var_index(l_move_call, 17) -> 639;
var_index(bif1_body, 5) -> 640;
var_index(move, 10) -> 641;
var_index(l_move_call_ext, 28) -> 642;
var_index(l_move_call_ext, 29) -> 643;
var_index(l_bs_get_integer, 0) -> 644;
var_index(is_atom, 6) -> 645;
var_index(is_integer, 3) -> 646;
var_index(l_allocate_zero, 8) -> 647;
var_index(is_nil, 15) -> 648;
var_index(is_list, 4) -> 649;
var_index(case_end, 5) -> 650;
var_index(l_increment, 7) -> 651;
var_index(l_is_eq_exact_immed, 26) -> 652;
var_index(l_increment, 5) -> 653;
var_index(l_fsub, 0) -> 654;
var_index(get_tuple_element, 9) -> 655;
var_index(fconv, 1) -> 656;
var_index(call_bif, 42) -> 657;
var_index(l_bsr, 1) -> 658;
var_index(l_move_call_ext, 30) -> 659;
var_index(call_bif, 43) -> 660;
var_index(l_call_ext, 31) -> 661;
var_index(extract_next_element3, 6) -> 662;
var_index(badmatch, 6) -> 663;
var_index(put_list, 7) -> 664;
var_index(l_move_call_ext, 32) -> 665;
var_index(l_move_call_ext, 31) -> 666;
var_index(l_call_ext, 33) -> 667;
var_index(l_call_ext, 32) -> 668;
var_index(extract_next_element2, 12) -> 669;
var_index(is_integer, 4) -> 670;
var_index(move_return, 10) -> 671;
var_index(l_rem, 1) -> 672;
var_index(l_bs_put_string, 0) -> 673;
var_index(is_nonempty_list, 20) -> 674;
var_index(move_deallocate_return, 8) -> 675;
var_index(l_move_call, 19) -> 676;
var_index(l_is_eq_exact_literal, 5) -> 677;
var_index(l_call_ext_only, 0) -> 678;
var_index(l_plus, 2) -> 679;
var_index(l_increment, 6) -> 680;
var_index(l_int_div, 0) -> 681;
var_index(l_bs_get_binary_imm2, 0) -> 682;
var_index(l_is_eq_exact_literal, 6) -> 683;
var_index(l_move_call_ext, 33) -> 684;
var_index(node, 4) -> 685;
var_index(l_call_ext, 34) -> 686;
var_index(put_list, 10) -> 687;
var_index(l_move_call, 20) -> 688;
var_index(init, 10) -> 689;
var_index(catch_end, 5) -> 690;
var_index(badmatch, 15) -> 691;
var_index(bif1_body, 6) -> 692;
var_index(l_fetch, 16) -> 693;
var_index(test_heap_1_put_list, 2) -> 694;
var_index(l_allocate, 9) -> 695;
var_index(l_yield, 0) -> 696;
var_index(is_atom, 4) -> 697;
var_index(l_move_call, 21) -> 698;
var_index(l_fetch, 17) -> 699;
var_index(l_bif2, 5) -> 700;
var_index(l_is_eq_exact_immed, 27) -> 701;
var_index(get_list, 7) -> 702;
var_index(l_bs_get_binary_all2, 1) -> 703;
var_index(call_bif, 44) -> 704;
var_index(node, 2) -> 705;
var_index(l_call_ext, 37) -> 706;
var_index(l_call_ext, 36) -> 707;
var_index(l_call_ext, 35) -> 708;
var_index(l_call_last, 9) -> 709;
var_index(is_nil, 16) -> 710;
var_index(is_list, 5) -> 711;
var_index(case_end, 6) -> 712;
var_index(l_new_bs_put_float_imm, 1) -> 713;
var_index(l_move_call, 22) -> 714;
var_index(l_catch, 6) -> 715;
var_index(l_move_call_last, 6) -> 716;
var_index(l_move_call_ext, 35) -> 717;
var_index(l_bs_append, 1) -> 718;
var_index(l_call_ext, 38) -> 719;
var_index(case_end, 7) -> 720;
var_index(is_nonempty_list, 21) -> 721;
var_index(l_select_tuple_arity, 3) -> 722;
var_index(l_move_call_ext, 36) -> 723;
var_index(l_is_ne_exact_immed, 5) -> 724;
var_index(l_jump_on_val, 0) -> 725;
var_index(l_bs_get_integer_32, 1) -> 726;
var_index(l_bs_skip_bits2, 1) -> 727;
var_index(is_function, 1) -> 728;
var_index(l_gc_bif1, 4) -> 729;
var_index(l_call_ext, 39) -> 730;
var_index(l_is_eq_exact_immed, 21) -> 731;
var_index(is_nonempty_list, 22) -> 732;
var_index(l_int_div, 1) -> 733;
var_index(l_is_eq_exact_immed, 28) -> 734;
var_index(l_is_ne_exact_immed, 6) -> 735;
var_index(l_call_ext, 40) -> 736;
var_index(extract_next_element2, 13) -> 737;
var_index(move_jump, 3) -> 738;
var_index(move_return, 11) -> 739;
var_index(badmatch, 7) -> 740;
var_index(l_bs_test_unit_8, 3) -> 741;
var_index(l_bs_test_unit_8, 2) -> 742;
var_index(l_move_call, 24) -> 743;
var_index(l_move_call, 23) -> 744;
var_index(l_move_call_ext_only, 4) -> 745;
var_index(l_move_call_ext, 37) -> 746;
var_index(l_is_eq_exact_immed, 29) -> 747;
var_index(l_call_ext, 41) -> 748;
var_index(move_return, 12) -> 749;
var_index(is_nil, 18) -> 750;
var_index(is_nil, 17) -> 751;
var_index(is_nonempty_list, 23) -> 752;
var_index(l_move_call, 27) -> 753;
var_index(l_move_call, 26) -> 754;
var_index(l_move_call, 25) -> 755;
var_index(l_select_tuple_arity, 1) -> 756;
var_index(l_fetch, 18) -> 757;
var_index(bif1_body, 7) -> 758;
var_index(l_move_call_ext, 38) -> 759;
var_index(call_bif, 1) -> 760;
var_index(l_call_ext, 48) -> 761;
var_index(l_call_ext, 47) -> 762;
var_index(l_call_ext, 46) -> 763;
var_index(l_call_ext, 45) -> 764;
var_index(l_call_ext, 44) -> 765;
var_index(l_call_ext, 43) -> 766;
var_index(l_call_ext, 42) -> 767;
var_index(extract_next_element3, 7) -> 768;
var_index(l_bs_save2, 1) -> 769;
var_index(bif2_body, 2) -> 770;
var_index(is_binary, 3) -> 771;
var_index(l_move_call_ext, 39) -> 772;
var_index(deallocate_return, 12) -> 773;
var_index(l_catch, 8) -> 774;
var_index(l_allocate, 8) -> 775;
var_index(l_call_ext, 50) -> 776;
var_index(l_call_ext, 49) -> 777;
var_index(l_trim, 8) -> 778;
var_index(is_nil, 19) -> 779;
var_index(case_end, 8) -> 780;
var_index(l_call_fun, 4) -> 781;
var_index(l_gc_bif1, 6) -> 782;
var_index(l_bs_skip_bits2, 0) -> 783;
var_index(l_move_call_ext, 40) -> 784;
var_index(l_call_ext, 53) -> 785;
var_index(l_call_ext, 52) -> 786;
var_index(l_call_ext, 51) -> 787;
var_index(extract_next_element, 19) -> 788;
var_index(l_is_ne_exact_literal, 0) -> 789;
var_index(l_move_call_ext_only, 5) -> 790;
var_index(l_call_ext_last, 5) -> 791;
var_index(l_select_tuple_arity2, 2) -> 792;
var_index(bs_context_to_binary, 5) -> 793;
var_index(l_select_val2, 10) -> 794;
var_index(l_fetch, 19) -> 795;
var_index(init, 15) -> 796;
var_index(l_get, 4) -> 797;
var_index(l_call_ext, 54) -> 798;
var_index(move_return, 13) -> 799;
var_index(badmatch, 8) -> 800;
var_index(l_bs_test_unit_8, 4) -> 801;
var_index(is_pid, 1) -> 802;
var_index(is_boolean, 0) -> 803;
var_index(bif1_body, 4) -> 804;
var_index(l_bs_get_binary2, 1) -> 805;
var_index(put_list, 12) -> 806;
var_index(l_call_ext, 58) -> 807;
var_index(l_call_ext, 57) -> 808;
var_index(l_call_ext, 56) -> 809;
var_index(l_call_ext, 55) -> 810;
var_index(extract_next_element2, 14) -> 811;
var_index(move_jump, 4) -> 812;
var_index(move_return, 14) -> 813;
var_index(l_move_call_only, 7) -> 814;
var_index(bs_context_to_binary, 1) -> 815;
var_index(l_call_ext, 65) -> 816;
var_index(l_call_ext, 64) -> 817;
var_index(l_call_ext, 63) -> 818;
var_index(l_call_ext, 62) -> 819;
var_index(l_call_ext, 61) -> 820;
var_index(l_call_ext, 60) -> 821;
var_index(l_call_ext, 59) -> 822;
var_index(extract_next_element, 20) -> 823;
var_index(l_move_call_last, 7) -> 824;
var_index(extract_next_element2, 15) -> 825;
var_index(move_return, 15) -> 826;
var_index(l_move_call, 28) -> 827;
var_index(init, 11) -> 828;
var_index(l_element, 0) -> 829;
var_index(l_call_ext, 68) -> 830;
var_index(l_call_ext, 67) -> 831;
var_index(l_call_ext, 66) -> 832;
var_index(l_bs_start_match2, 3) -> 833;
var_index(move_jump, 6) -> 834;
var_index(move_jump, 5) -> 835;
var_index(is_nonempty_list_test_heap, 0) -> 836;
var_index(catch_end, 6) -> 837;
var_index(l_get, 5) -> 838;
var_index(l_bs_skip_bits_all2, 1) -> 839;
var_index(l_wait_timeout, 2) -> 840;
var_index(is_nonempty_list_test_heap, 1) -> 841;
var_index(l_call_ext, 70) -> 842;
var_index(l_call_ext, 69) -> 843;
var_index(l_move_call_only, 8) -> 844;
var_index(case_end, 9) -> 845;
var_index(is_pid, 0) -> 846;
var_index(l_new_bs_put_float_imm, 0) -> 847;
var_index(l_move_call, 29) -> 848;
var_index(l_select_tuple_arity, 2) -> 849;
var_index(catch_end, 8) -> 850;
var_index(l_bif2, 6) -> 851;
var_index(bif2_body, 3) -> 852;
var_index(node, 3) -> 853;
var_index(bs_init_writable, 0) -> 854;
var_index(l_call_ext, 73) -> 855;
var_index(l_call_ext, 72) -> 856;
var_index(l_call_ext, 71) -> 857;
var_index(extract_next_element, 21) -> 858;
var_index(move_jump, 7) -> 859;
var_index(move_return, 17) -> 860;
var_index(move_return, 16) -> 861;
var_index(l_new_bs_put_integer_imm, 1) -> 862;
var_index(bs_context_to_binary, 2) -> 863;
var_index(put_list, 11) -> 864;
var_index(is_nonempty_list, 25) -> 865;
var_index(is_nonempty_list, 24) -> 866;
var_index(try_end, 6) -> 867;
var_index(l_bs_private_append, 0) -> 868;
var_index(deallocate_return, 9) -> 869;
var_index(l_move_call, 30) -> 870;
var_index(l_call_ext_only, 1) -> 871;
var_index(l_apply, 0) -> 872;
var_index(l_move_call_ext, 27) -> 873;
var_index(l_bs_get_integer_imm, 0) -> 874;
var_index(test_heap_1_put_list, 3) -> 875;
var_index(self, 3) -> 876;
var_index(is_tuple, 5) -> 877;
var_index(l_call_ext, 76) -> 878;
var_index(l_call_ext, 75) -> 879;
var_index(l_call_ext, 74) -> 880;
var_index(l_is_ne_exact_immed, 7) -> 881;
var_index(extract_next_element3, 8) -> 882;
var_index(l_move_call, 33) -> 883;
var_index(l_move_call, 32) -> 884;
var_index(l_move_call, 31) -> 885;
var_index(l_catch, 7) -> 886;
var_index(catch_end, 7) -> 887;
var_index(l_is_ne_exact_immed, 8) -> 888;
var_index(l_select_val2, 11) -> 889;
var_index(l_fetch, 20) -> 890;
var_index(l_call_ext, 77) -> 891;
var_index(extract_next_element3, 9) -> 892;
var_index(move_jump, 8) -> 893;
var_index(l_bs_get_utf8, 1) -> 894;
var_index(is_nonempty_list, 26) -> 895;
var_index(is_binary, 2) -> 896;
var_index(l_fetch, 21) -> 897;
var_index(l_bs_skip_bits_all2, 2) -> 898;
var_index(self, 4) -> 899;
var_index(l_call_ext, 80) -> 900;
var_index(l_call_ext, 79) -> 901;
var_index(l_call_ext, 78) -> 902;
var_index(l_call_last, 10) -> 903;
var_index(l_new_bs_put_integer, 1) -> 904;
var_index(move_return, 18) -> 905;
var_index(is_nil, 20) -> 906;
var_index(recv_mark, 0) -> 907;
var_index(bs_context_to_binary, 3) -> 908;
var_index(badmatch, 10) -> 909;
var_index(badmatch, 9) -> 910;
var_index(is_function, 0) -> 911;
var_index(l_recv_set, 0) -> 912;
var_index(l_bs_get_integer_16, 0) -> 913;
var_index(move2, 9) -> 914;
var_index(l_call_ext, 88) -> 915;
var_index(l_call_ext, 87) -> 916;
var_index(l_call_ext, 86) -> 917;
var_index(l_call_ext, 85) -> 918;
var_index(l_call_ext, 84) -> 919;
var_index(l_call_ext, 83) -> 920;
var_index(l_call_ext, 82) -> 921;
var_index(l_call_ext, 81) -> 922;
var_index(l_is_eq_exact_immed, 30) -> 923;
var_index(extract_next_element2, 16) -> 924;
var_index(l_bs_get_float2, 0) -> 925;
var_index(move_jump, 9) -> 926;
var_index(move_return, 19) -> 927;
var_index(l_trim, 11) -> 928;
var_index(is_nil, 21) -> 929;
var_index(l_select_val2, 12) -> 930;
var_index(is_atom, 5) -> 931;
var_index(l_move_call_ext, 34) -> 932;
var_index(is_float, 0) -> 933;
var_index(l_is_ne_exact_immed, 9) -> 934;
var_index(l_minus, 2) -> 935;
var_index(l_fast_element, 4) -> 936;
var_index(move_return, 22) -> 937;
var_index(move_return, 21) -> 938;
var_index(move_return, 20) -> 939;
var_index(is_nonempty_list, 28) -> 940;
var_index(is_nonempty_list, 27) -> 941;
var_index(l_bif1, 2) -> 942;
var_index(deallocate_return, 11) -> 943;
var_index(deallocate_return, 10) -> 944;
var_index(l_bs_init_bits, 0) -> 945;
var_index(get_list, 8) -> 946;
var_index(l_is_eq_exact_immed, 31) -> 947;
var_index(get_list, 10) -> 948;
var_index(is_tuple, 6) -> 949;
var_index(l_call_last, 11) -> 950;
var_index(extract_next_element, 22) -> 951;
var_index(move_return, 24) -> 952;
var_index(move_return, 23) -> 953;
var_index(badmatch, 11) -> 954;
var_index(l_select_val2, 13) -> 955;
var_index(l_call_ext_only, 2) -> 956;
var_index(get_tuple_element, 10) -> 957;
var_index(move, 12) -> 958;
var_index(l_gc_bif1, 7) -> 959;
var_index(wait_timeout, 0) -> 960;
var_index(badmatch, 12) -> 961;
var_index(is_nonempty_list, 29) -> 962;
var_index(l_times, 2) -> 963;
var_index(l_apply_fun, 0) -> 964;
var_index(l_is_eq_exact_immed, 32) -> 965;
var_index(l_is_eq_exact_immed, 33) -> 966;
var_index(l_bs_test_tail_imm2, 0) -> 967;
var_index(l_bs_get_integer_32, 2) -> 968;
var_index(move_return, 25) -> 969;
var_index(is_nil, 22) -> 970;
var_index(badmatch, 13) -> 971;
var_index(is_integer_allocate, 1) -> 972;
var_index(l_is_eq_exact_immed, 34) -> 973;
var_index(get_list, 9) -> 974;
var_index(is_tuple, 8) -> 975;
var_index(is_tuple, 7) -> 976;
var_index(extract_next_element, 23) -> 977;
var_index(move_jump, 10) -> 978;
var_index(is_nil, 23) -> 979;
var_index(bs_context_to_binary, 4) -> 980;
var_index(badmatch, 14) -> 981;
var_index(is_nonempty_list, 32) -> 982;
var_index(is_nonempty_list, 31) -> 983;
var_index(is_nonempty_list, 30) -> 984;
var_index(l_bs_init_fail, 1) -> 985;
var_index(move_jump, 11) -> 986;
var_index(is_nil, 24) -> 987;
var_index(is_nonempty_list, 34) -> 988;
var_index(is_nonempty_list, 33) -> 989;
var_index(try_end, 7) -> 990;
var_index(init, 12) -> 991;
var_index(l_bs_add, 1) -> 992;
var_index(l_wait_timeout, 0) -> 993;
var_index(l_fast_element, 5) -> 994;
var_index(test_heap_1_put_list, 4) -> 995;
var_index(l_gc_bif2, 0) -> 996;
var_index(l_bs_put_utf16, 0) -> 997;
var_index(l_is_eq_exact_immed, 35) -> 998;
var_index(move_return, 27) -> 999;
var_index(move_return, 26) -> 1000;
var_index(l_trim, 9) -> 1001;
var_index(is_nil, 25) -> 1002;
var_index(l_bs_validate_unicode, 0) -> 1003;
var_index(is_nonempty_list, 35) -> 1004;
var_index(l_bs_init, 0) -> 1005;
var_index(l_jump_on_val, 1) -> 1006;
var_index(move, 11) -> 1007;
var_index(l_bs_utf16_size, 0) -> 1008;
var_index(l_bs_get_binary2, 2) -> 1009;
var_index(l_bs_restore2, 3) -> 1010;
var_index(is_nil, 26) -> 1011;
var_index(raise, 1) -> 1012;
var_index(l_int_bnot, 0) -> 1013;
var_index(is_nil, 29) -> 1014;
var_index(is_nil, 28) -> 1015;
var_index(is_nil, 27) -> 1016;
var_index(is_nonempty_list, 37) -> 1017;
var_index(is_nonempty_list, 36) -> 1018;
var_index(l_bs_save2, 2) -> 1019;
var_index(l_bs_get_binary_imm2, 1) -> 1020;
var_index(is_bitstr, 0) -> 1021;
var_index(l_new_bs_put_binary_all, 2) -> 1022;
var_index(l_new_bs_put_binary, 0) -> 1023;
var_index(fmove_2, 2) -> 1024;
var_index(is_reference, 0) -> 1025;
var_index(is_port, 0) -> 1026;
var_index(is_number, 0) -> 1027;
var_index(move, 13) -> 1028;
var_index(l_bs_get_binary_all_reuse, 1) -> 1029;
var_index(init, 13) -> 1030;
var_index(l_wait_timeout, 1) -> 1031;
var_index(l_select_tuple_arity, 4) -> 1032;
var_index(l_trim, 10) -> 1033;
var_index(l_bs_put_utf8, 0) -> 1034;
var_index(init, 14) -> 1035;
var_index(l_fnegate, 0) -> 1036;
var_index(l_bs_get_integer_imm, 1) -> 1037;
var_index(l_jump_on_val, 2) -> 1038;
var_index(l_bs_utf8_size, 0) -> 1039;
var_index(l_bs_get_binary_imm2, 2) -> 1040;
var_index(l_bs_validate_unicode_retract, 0) -> 1041;
var_index(l_bxor, 0) -> 1042;
var_index(l_new_bs_put_float, 0) -> 1043;
var_index(l_apply_last, 0) -> 1044;
var_index(l_is_function2, 0) -> 1045;
var_index(l_gc_bif3, 0) -> 1046;
var_index(l_bor, 2) -> 1047;
var_index(l_new_bs_put_binary_imm, 0) -> 1048;
var_index(l_bs_get_integer_8, 2) -> 1049;
var_index(l_bs_start_match2, 4) -> 1050;
var_index(l_rem, 2) -> 1051;
var_index(l_bs_get_integer_small_imm, 1) -> 1052;
var_index(l_bsl, 2) -> 1053;
var_index(l_apply_only, 0) -> 1054;
var_index(on_load, 0) -> 1055;
var_index(move2, 10) -> 1056;
var_index(l_int_div, 2) -> 1057;
var_index(l_bs_test_unit, 0) -> 1058;
var_index(l_m_div, 0) -> 1059;
var_index(l_hibernate, 0) -> 1060;
var_index(l_apply_fun_last, 0) -> 1061;
var_index(is_function2, 0) -> 1062;
var_index(l_apply_fun_only, 0) -> 1063;
var_index(l_band, 2) -> 1064;
var_index(is_bigint, 0) -> 1065;
var_index(test_heap, 1) -> 1066;
var_index(func_info, 0) -> 1067;
var_index(call_bif, 0) -> 1068;
var_index(l_bs_get_utf16, 2) -> 1069;
var_index(l_put_tuple, 7) -> 1070;
var_index(get_tuple_element, 11) -> 1071;
var_index(allocate_init, 1) -> 1072;
var_index(l_call_fun_last, 1) -> 1073;
var_index(set_tuple_element, 2) -> 1074;
var_index(allocate_heap, 1) -> 1075;
var_index(is_tuple_of_arity, 4) -> 1076;
var_index(test_arity, 4) -> 1077;
var_index(l_bs_match_string, 4) -> 1078;
var_index(is_nonempty_list_allocate, 2) -> 1079;
var_index(l_bs_append, 2) -> 1080;
var_index(try_case_end, 1) -> 1081;
var_index(init3, 1) -> 1082;
var_index(l_select_val_smallints, 2) -> 1083;
var_index(l_select_tuple_arity2, 3) -> 1084;
var_index(init2, 1) -> 1085;
var_index(l_bs_get_binary_all2, 2) -> 1086;
var_index(is_nonempty_list_test_heap, 2) -> 1087;
var_index(allocate_heap_zero, 1) -> 1088;
var_index(l_bs_init_heap_bin, 1) -> 1089;
var_index(l_plus, 3) -> 1090;
var_index(l_bs_get_integer, 1) -> 1091;

var_index(Op, No) -> erlang:error({noindex,Op,No}).

var_by_index(0) -> {move, 0};
var_by_index(1) -> {move, 1};
var_by_index(2) -> {l_call, 0};
var_by_index(3) -> {test_heap, 0};
var_by_index(4) -> {move, 2};
var_by_index(5) -> {badmatch, 0};
var_by_index(6) -> {move, 3};
var_by_index(7) -> {l_put_tuple, 0};
var_by_index(8) -> {move, 4};
var_by_index(9) -> {move2, 1};
var_by_index(10) -> {get_tuple_element, 0};
var_by_index(11) -> {call_bif, 7};
var_by_index(12) -> {l_bs_start_match2, 0};
var_by_index(13) -> {l_bs_test_zero_tail2, 0};
var_by_index(14) -> {l_bs_match_string, 0};
var_by_index(15) -> {put_list, 0};
var_by_index(16) -> {is_tuple_of_arity, 1};
var_by_index(17) -> {get_tuple_element, 1};
var_by_index(18) -> {move2, 0};
var_by_index(19) -> {l_call_only, 0};
var_by_index(20) -> {l_is_eq_exact_immed, 0};
var_by_index(21) -> {l_is_eq_exact_immed, 1};
var_by_index(22) -> {is_tuple_of_arity, 0};
var_by_index(23) -> {l_move_call_ext, 0};
var_by_index(24) -> {put_list, 1};
var_by_index(25) -> {move, 5};
var_by_index(26) -> {get_list, 0};
var_by_index(27) -> {l_put_tuple, 1};
var_by_index(28) -> {l_call_ext, 89};
var_by_index(29) -> {l_is_ge, 0};
var_by_index(30) -> {l_make_fun, 0};
var_by_index(31) -> {l_move_call_last, 0};
var_by_index(32) -> {extract_next_element2, 0};
var_by_index(33) -> {is_tuple_of_arity, 2};
var_by_index(34) -> {return, 0};
var_by_index(35) -> {l_move_call_ext, 1};
var_by_index(36) -> {l_fetch, 0};
var_by_index(37) -> {call_bif, 3};
var_by_index(38) -> {move_deallocate_return, 0};
var_by_index(39) -> {l_trim, 0};
var_by_index(40) -> {l_allocate, 0};
var_by_index(41) -> {move2, 3};
var_by_index(42) -> {l_fetch, 1};
var_by_index(43) -> {put_list, 2};
var_by_index(44) -> {is_nil, 0};
var_by_index(45) -> {is_nonempty_list, 0};
var_by_index(46) -> {extract_next_element, 0};
var_by_index(47) -> {move_return, 28};
var_by_index(48) -> {l_allocate, 1};
var_by_index(49) -> {l_is_eq_exact_immed, 2};
var_by_index(50) -> {jump, 0};
var_by_index(51) -> {deallocate_return, 0};
var_by_index(52) -> {get_list, 1};
var_by_index(53) -> {case_end, 0};
var_by_index(54) -> {call_bif, 8};
var_by_index(55) -> {move2, 2};
var_by_index(56) -> {l_move_call, 34};
var_by_index(57) -> {l_is_eq_exact, 0};
var_by_index(58) -> {get_tuple_element, 2};
var_by_index(59) -> {l_allocate, 2};
var_by_index(60) -> {move_return, 0};
var_by_index(61) -> {l_is_eq_exact_immed, 3};
var_by_index(62) -> {l_select_val2, 0};
var_by_index(63) -> {l_fetch, 2};
var_by_index(64) -> {get_tuple_element, 3};
var_by_index(65) -> {init2, 0};
var_by_index(66) -> {get_list, 2};
var_by_index(67) -> {l_is_eq_exact_immed, 4};
var_by_index(68) -> {init, 0};
var_by_index(69) -> {deallocate_return, 1};
var_by_index(70) -> {put_list, 3};
var_by_index(71) -> {call_bif, 9};
var_by_index(72) -> {extract_next_element, 1};
var_by_index(73) -> {is_tuple_of_arity, 3};
var_by_index(74) -> {l_is_eq_exact_literal, 0};
var_by_index(75) -> {get_tuple_element, 4};
var_by_index(76) -> {move2, 4};
var_by_index(77) -> {l_move_call_ext, 41};
var_by_index(78) -> {l_fetch, 3};
var_by_index(79) -> {init, 1};
var_by_index(80) -> {l_put_tuple, 2};
var_by_index(81) -> {move2, 5};
var_by_index(82) -> {l_allocate, 3};
var_by_index(83) -> {deallocate_return, 2};
var_by_index(84) -> {l_fetch, 4};
var_by_index(85) -> {init3, 0};
var_by_index(86) -> {is_nonempty_list, 1};
var_by_index(87) -> {l_is_eq_exact_immed, 5};
var_by_index(88) -> {l_allocate_zero, 0};
var_by_index(89) -> {call_bif, 6};
var_by_index(90) -> {l_trim, 1};
var_by_index(91) -> {allocate_init, 0};
var_by_index(92) -> {call_bif, 45};
var_by_index(93) -> {allocate_heap, 0};
var_by_index(94) -> {test_heap_1_put_list, 0};
var_by_index(95) -> {l_allocate_zero, 1};
var_by_index(96) -> {move_deallocate_return, 1};
var_by_index(97) -> {init, 2};
var_by_index(98) -> {l_call_last, 0};
var_by_index(99) -> {move_return, 1};
var_by_index(100) -> {is_nonempty_list, 2};
var_by_index(101) -> {l_move_call, 0};
var_by_index(102) -> {is_tuple, 0};
var_by_index(103) -> {is_list, 0};
var_by_index(104) -> {l_is_eq_exact_immed, 6};
var_by_index(105) -> {l_call_last, 1};
var_by_index(106) -> {deallocate_return, 3};
var_by_index(107) -> {is_nonempty_list_allocate, 0};
var_by_index(108) -> {l_move_call_only, 0};
var_by_index(109) -> {call_bif, 5};
var_by_index(110) -> {extract_next_element, 2};
var_by_index(111) -> {l_increment, 0};
var_by_index(112) -> {l_gc_bif1, 0};
var_by_index(113) -> {move, 6};
var_by_index(114) -> {l_is_lt, 0};
var_by_index(115) -> {l_trim, 2};
var_by_index(116) -> {l_select_val_atoms, 0};
var_by_index(117) -> {l_call_last, 2};
var_by_index(118) -> {move_deallocate_return, 2};
var_by_index(119) -> {is_nonempty_list, 3};
var_by_index(120) -> {l_new_bs_put_integer_imm, 0};
var_by_index(121) -> {is_nil, 1};
var_by_index(122) -> {extract_next_element2, 1};
var_by_index(123) -> {l_move_call_only, 9};
var_by_index(124) -> {l_select_val2, 2};
var_by_index(125) -> {remove_message, 0};
var_by_index(126) -> {l_move_call_only, 1};
var_by_index(127) -> {init, 3};
var_by_index(128) -> {l_catch, 0};
var_by_index(129) -> {l_allocate_zero, 2};
var_by_index(130) -> {extract_next_element3, 0};
var_by_index(131) -> {get_tuple_element, 5};
var_by_index(132) -> {l_call_ext, 0};
var_by_index(133) -> {l_fetch, 5};
var_by_index(134) -> {move_jump, 12};
var_by_index(135) -> {extract_next_element, 3};
var_by_index(136) -> {is_nil, 2};
var_by_index(137) -> {move2, 6};
var_by_index(138) -> {l_allocate, 4};
var_by_index(139) -> {catch_end, 0};
var_by_index(140) -> {test_arity, 0};
var_by_index(141) -> {l_allocate_zero, 3};
var_by_index(142) -> {l_bs_start_match2, 1};
var_by_index(143) -> {l_is_eq_exact_immed, 7};
var_by_index(144) -> {move_return, 2};
var_by_index(145) -> {put_list, 4};
var_by_index(146) -> {move_return, 3};
var_by_index(147) -> {l_move_call_ext, 2};
var_by_index(148) -> {move_return, 4};
var_by_index(149) -> {deallocate_return, 4};
var_by_index(150) -> {l_call_last, 3};
var_by_index(151) -> {move2, 7};
var_by_index(152) -> {l_is_eq_exact_immed, 8};
var_by_index(153) -> {l_plus, 0};
var_by_index(154) -> {move, 7};
var_by_index(155) -> {l_put_tuple, 3};
var_by_index(156) -> {call_bif, 2};
var_by_index(157) -> {l_select_tuple_arity2, 0};
var_by_index(158) -> {is_nonempty_list, 4};
var_by_index(159) -> {init, 4};
var_by_index(160) -> {is_nonempty_list, 5};
var_by_index(161) -> {get_list, 3};
var_by_index(162) -> {l_call_fun, 0};
var_by_index(163) -> {l_call_last, 4};
var_by_index(164) -> {l_move_call, 1};
var_by_index(165) -> {move_return, 5};
var_by_index(166) -> {l_bs_get_binary_all_reuse, 0};
var_by_index(167) -> {test_arity, 1};
var_by_index(168) -> {bif1_body, 0};
var_by_index(169) -> {l_move_call_only, 2};
var_by_index(170) -> {l_move_call_last, 1};
var_by_index(171) -> {is_nonempty_list, 6};
var_by_index(172) -> {l_bs_test_zero_tail2, 1};
var_by_index(173) -> {l_is_eq, 0};
var_by_index(174) -> {send, 0};
var_by_index(175) -> {set_tuple_element, 0};
var_by_index(176) -> {l_catch, 1};
var_by_index(177) -> {l_call_ext_only, 3};
var_by_index(178) -> {call_bif, 10};
var_by_index(179) -> {l_is_ne, 0};
var_by_index(180) -> {l_move_call_only, 3};
var_by_index(181) -> {l_select_val2, 1};
var_by_index(182) -> {l_move_call_ext_last, 0};
var_by_index(183) -> {l_select_val_smallints, 0};
var_by_index(184) -> {l_move_call_ext, 3};
var_by_index(185) -> {l_is_ne_exact_immed, 0};
var_by_index(186) -> {l_increment, 1};
var_by_index(187) -> {l_bs_add, 0};
var_by_index(188) -> {bif2_body, 0};
var_by_index(189) -> {is_nonempty_list, 7};
var_by_index(190) -> {extract_next_element, 4};
var_by_index(191) -> {case_end, 1};
var_by_index(192) -> {l_bs_match_string, 1};
var_by_index(193) -> {l_is_eq_exact_immed, 9};
var_by_index(194) -> {extract_next_element2, 2};
var_by_index(195) -> {move_deallocate_return, 3};
var_by_index(196) -> {extract_next_element2, 3};
var_by_index(197) -> {l_loop_rec, 0};
var_by_index(198) -> {l_call_ext, 1};
var_by_index(199) -> {l_select_tuple_arity2, 1};
var_by_index(200) -> {l_move_call, 2};
var_by_index(201) -> {l_move_call, 3};
var_by_index(202) -> {l_move_call_ext, 4};
var_by_index(203) -> {l_bs_get_utf16, 0};
var_by_index(204) -> {l_select_val_atoms, 1};
var_by_index(205) -> {l_trim, 3};
var_by_index(206) -> {init, 5};
var_by_index(207) -> {l_bs_restore2, 0};
var_by_index(208) -> {catch_end, 1};
var_by_index(209) -> {is_nil, 3};
var_by_index(210) -> {l_move_call, 4};
var_by_index(211) -> {l_bs_init_heap_bin, 0};
var_by_index(212) -> {is_nil, 4};
var_by_index(213) -> {is_nonempty_list, 8};
var_by_index(214) -> {wait, 0};
var_by_index(215) -> {l_call_ext_last, 0};
var_by_index(216) -> {l_allocate_zero, 4};
var_by_index(217) -> {loop_rec_end, 0};
var_by_index(218) -> {call_bif, 11};
var_by_index(219) -> {deallocate_return, 5};
var_by_index(220) -> {move, 8};
var_by_index(221) -> {l_fetch, 6};
var_by_index(222) -> {is_nonempty_list, 9};
var_by_index(223) -> {extract_next_element, 5};
var_by_index(224) -> {l_bs_start_match2, 2};
var_by_index(225) -> {l_allocate_zero, 5};
var_by_index(226) -> {l_select_val2, 3};
var_by_index(227) -> {l_allocate, 5};
var_by_index(228) -> {init, 6};
var_by_index(229) -> {l_move_call_ext, 5};
var_by_index(230) -> {badmatch, 1};
var_by_index(231) -> {l_plus, 1};
var_by_index(232) -> {l_call_last, 5};
var_by_index(233) -> {l_move_call_ext, 6};
var_by_index(234) -> {l_call_last, 6};
var_by_index(235) -> {l_move_call_ext, 7};
var_by_index(236) -> {badmatch, 2};
var_by_index(237) -> {l_move_call_ext, 8};
var_by_index(238) -> {l_call_fun_last, 0};
var_by_index(239) -> {l_move_call_ext, 9};
var_by_index(240) -> {l_increment, 2};
var_by_index(241) -> {bs_context_to_binary, 0};
var_by_index(242) -> {call_bif, 12};
var_by_index(243) -> {int_code_end, 0};
var_by_index(244) -> {l_trim, 4};
var_by_index(245) -> {l_move_call_ext_only, 0};
var_by_index(246) -> {put_list, 5};
var_by_index(247) -> {l_bs_save2, 0};
var_by_index(248) -> {call_bif, 13};
var_by_index(249) -> {extract_next_element, 6};
var_by_index(250) -> {move_deallocate_return, 4};
var_by_index(251) -> {l_is_ne_exact_immed, 1};
var_by_index(252) -> {extract_next_element2, 4};
var_by_index(253) -> {l_select_val2, 4};
var_by_index(254) -> {l_gc_bif1, 1};
var_by_index(255) -> {l_increment, 3};
var_by_index(256) -> {self, 0};
var_by_index(257) -> {l_put_tuple, 4};
var_by_index(258) -> {extract_next_element, 7};
var_by_index(259) -> {get_tuple_element, 6};
var_by_index(260) -> {call_bif, 14};
var_by_index(261) -> {is_tuple, 1};
var_by_index(262) -> {l_bs_test_unit_8, 0};
var_by_index(263) -> {l_move_call_last, 2};
var_by_index(264) -> {badmatch, 3};
var_by_index(265) -> {l_move_call, 5};
var_by_index(266) -> {l_is_eq_exact_immed, 11};
var_by_index(267) -> {l_move_call, 6};
var_by_index(268) -> {is_nil, 5};
var_by_index(269) -> {l_bs_test_zero_tail2, 2};
var_by_index(270) -> {is_nonempty_list_allocate, 1};
var_by_index(271) -> {l_call_ext, 2};
var_by_index(272) -> {l_bs_test_zero_tail2, 3};
var_by_index(273) -> {l_catch, 2};
var_by_index(274) -> {l_move_call_ext, 10};
var_by_index(275) -> {call_bif, 15};
var_by_index(276) -> {is_atom, 0};
var_by_index(277) -> {l_move_call_ext_only, 6};
var_by_index(278) -> {l_fast_element, 0};
var_by_index(279) -> {is_nonempty_list, 10};
var_by_index(280) -> {l_select_val_smallints, 1};
var_by_index(281) -> {call_bif, 16};
var_by_index(282) -> {l_call_ext, 3};
var_by_index(283) -> {move_return, 6};
var_by_index(284) -> {l_call_ext_last, 1};
var_by_index(285) -> {is_nil, 6};
var_by_index(286) -> {allocate_heap_zero, 0};
var_by_index(287) -> {is_nonempty_list, 11};
var_by_index(288) -> {l_call_ext_last, 2};
var_by_index(289) -> {call_bif, 17};
var_by_index(290) -> {self, 1};
var_by_index(291) -> {extract_next_element, 8};
var_by_index(292) -> {init, 7};
var_by_index(293) -> {case_end, 2};
var_by_index(294) -> {l_minus, 0};
var_by_index(295) -> {extract_next_element3, 1};
var_by_index(296) -> {is_nil, 7};
var_by_index(297) -> {l_move_call, 7};
var_by_index(298) -> {call_bif, 18};
var_by_index(299) -> {raise, 0};
var_by_index(300) -> {catch_end, 2};
var_by_index(301) -> {l_is_eq_exact_literal, 1};
var_by_index(302) -> {l_is_eq_exact_literal, 7};
var_by_index(303) -> {l_move_call_ext, 11};
var_by_index(304) -> {extract_next_element2, 5};
var_by_index(305) -> {extract_next_element, 24};
var_by_index(306) -> {l_allocate, 6};
var_by_index(307) -> {l_bif2, 0};
var_by_index(308) -> {try_end, 0};
var_by_index(309) -> {l_call_fun, 1};
var_by_index(310) -> {call_bif, 19};
var_by_index(311) -> {deallocate_return, 6};
var_by_index(312) -> {l_call_ext, 4};
var_by_index(313) -> {l_move_call_last, 3};
var_by_index(314) -> {l_move_call_ext_only, 1};
var_by_index(315) -> {l_put_tuple, 5};
var_by_index(316) -> {l_band, 0};
var_by_index(317) -> {l_move_call, 8};
var_by_index(318) -> {l_trim, 5};
var_by_index(319) -> {extract_next_element, 9};
var_by_index(320) -> {call_bif, 20};
var_by_index(321) -> {l_call_ext, 5};
var_by_index(322) -> {l_call_ext, 6};
var_by_index(323) -> {l_minus, 1};
var_by_index(324) -> {l_bs_get_binary_all2, 0};
var_by_index(325) -> {get_tuple_element, 7};
var_by_index(326) -> {l_is_eq_exact_immed, 12};
var_by_index(327) -> {test_heap_1_put_list, 1};
var_by_index(328) -> {extract_next_element3, 2};
var_by_index(329) -> {is_integer, 0};
var_by_index(330) -> {extract_next_element2, 6};
var_by_index(331) -> {l_catch, 3};
var_by_index(332) -> {is_nil, 8};
var_by_index(333) -> {l_bif2, 1};
var_by_index(334) -> {move_deallocate_return, 5};
var_by_index(335) -> {l_move_call_only, 4};
var_by_index(336) -> {l_bsr, 0};
var_by_index(337) -> {move_jump, 0};
var_by_index(338) -> {is_list, 1};
var_by_index(339) -> {l_move_call, 9};
var_by_index(340) -> {l_bs_get_integer_small_imm, 0};
var_by_index(341) -> {l_is_eq_exact_immed, 13};
var_by_index(342) -> {apply, 0};
var_by_index(343) -> {l_call_ext, 7};
var_by_index(344) -> {l_fast_element, 2};
var_by_index(345) -> {l_bs_get_integer_8, 0};
var_by_index(346) -> {l_bif2, 2};
var_by_index(347) -> {l_fetch, 7};
var_by_index(348) -> {set_tuple_element, 1};
var_by_index(349) -> {try_end, 1};
var_by_index(350) -> {l_is_eq_exact_literal, 2};
var_by_index(351) -> {l_is_eq_exact_immed, 10};
var_by_index(352) -> {extract_next_element2, 7};
var_by_index(353) -> {l_is_eq_exact_literal, 3};
var_by_index(354) -> {l_bsl, 0};
var_by_index(355) -> {l_allocate_zero, 6};
var_by_index(356) -> {is_nonempty_list, 12};
var_by_index(357) -> {l_times, 0};
var_by_index(358) -> {l_select_tuple_arity, 0};
var_by_index(359) -> {l_fmul, 0};
var_by_index(360) -> {l_call_ext, 8};
var_by_index(361) -> {l_bs_match_string, 2};
var_by_index(362) -> {move, 9};
var_by_index(363) -> {call_bif, 21};
var_by_index(364) -> {l_move_call_ext, 12};
var_by_index(365) -> {l_put_tuple, 6};
var_by_index(366) -> {l_times, 1};
var_by_index(367) -> {l_bs_init_fail, 0};
var_by_index(368) -> {l_move_call_ext_only, 2};
var_by_index(369) -> {l_is_eq_exact_immed, 14};
var_by_index(370) -> {l_is_eq_exact_immed, 15};
var_by_index(371) -> {l_move_call_ext, 14};
var_by_index(372) -> {l_move_call_ext, 13};
var_by_index(373) -> {l_call_ext, 9};
var_by_index(374) -> {call_bif, 22};
var_by_index(375) -> {extract_next_element, 10};
var_by_index(376) -> {is_nil, 9};
var_by_index(377) -> {l_fetch, 8};
var_by_index(378) -> {node, 0};
var_by_index(379) -> {l_call_last, 7};
var_by_index(380) -> {l_bs_get_binary2, 0};
var_by_index(381) -> {is_tuple, 2};
var_by_index(382) -> {l_call_fun, 2};
var_by_index(383) -> {get_list, 4};
var_by_index(384) -> {test_arity, 2};
var_by_index(385) -> {l_bs_get_integer_8, 1};
var_by_index(386) -> {l_bs_test_zero_tail2, 5};
var_by_index(387) -> {catch_end, 3};
var_by_index(388) -> {l_bif2, 3};
var_by_index(389) -> {l_is_ne_exact_immed, 2};
var_by_index(390) -> {l_allocate_zero, 9};
var_by_index(391) -> {call_bif, 23};
var_by_index(392) -> {l_is_ne_exact, 0};
var_by_index(393) -> {l_bif2, 4};
var_by_index(394) -> {is_binary, 0};
var_by_index(395) -> {l_is_eq_exact_immed, 16};
var_by_index(396) -> {l_bs_get_integer_32, 0};
var_by_index(397) -> {extract_next_element, 11};
var_by_index(398) -> {l_call_ext, 10};
var_by_index(399) -> {is_atom, 1};
var_by_index(400) -> {l_select_val2, 7};
var_by_index(401) -> {l_fetch, 9};
var_by_index(402) -> {l_fcheckerror, 0};
var_by_index(403) -> {l_new_bs_put_binary_all, 0};
var_by_index(404) -> {fclearerror, 0};
var_by_index(405) -> {extract_next_element3, 3};
var_by_index(406) -> {system_limit, 0};
var_by_index(407) -> {node, 1};
var_by_index(408) -> {is_nonempty_list, 38};
var_by_index(409) -> {extract_next_element, 12};
var_by_index(410) -> {get_list, 5};
var_by_index(411) -> {l_move_call, 10};
var_by_index(412) -> {move_deallocate_return, 6};
var_by_index(413) -> {l_move_call_last, 4};
var_by_index(414) -> {l_new_bs_put_binary_all, 1};
var_by_index(415) -> {l_is_eq_exact_immed, 17};
var_by_index(416) -> {timeout, 0};
var_by_index(417) -> {deallocate_return, 7};
var_by_index(418) -> {l_get, 0};
var_by_index(419) -> {l_select_val2, 14};
var_by_index(420) -> {l_fetch, 10};
var_by_index(421) -> {l_move_call, 11};
var_by_index(422) -> {l_move_call_ext_last, 1};
var_by_index(423) -> {is_nil, 10};
var_by_index(424) -> {l_fetch, 11};
var_by_index(425) -> {l_select_val2, 5};
var_by_index(426) -> {is_float, 1};
var_by_index(427) -> {call_bif, 24};
var_by_index(428) -> {l_call_ext, 11};
var_by_index(429) -> {l_is_eq_exact_immed, 36};
var_by_index(430) -> {l_select_val2, 8};
var_by_index(431) -> {l_get, 1};
var_by_index(432) -> {call_bif, 25};
var_by_index(433) -> {l_bs_restore2, 1};
var_by_index(434) -> {l_move_call, 12};
var_by_index(435) -> {l_band, 1};
var_by_index(436) -> {l_bsl, 1};
var_by_index(437) -> {l_fast_element, 1};
var_by_index(438) -> {is_binary, 1};
var_by_index(439) -> {l_move_call_ext, 16};
var_by_index(440) -> {l_get, 2};
var_by_index(441) -> {extract_next_element, 13};
var_by_index(442) -> {is_nonempty_list, 13};
var_by_index(443) -> {l_is_eq_exact_immed, 18};
var_by_index(444) -> {l_call_ext, 12};
var_by_index(445) -> {l_move_call, 13};
var_by_index(446) -> {l_move_call_ext, 17};
var_by_index(447) -> {l_rem, 0};
var_by_index(448) -> {move2, 8};
var_by_index(449) -> {l_call_ext, 13};
var_by_index(450) -> {l_allocate_zero, 7};
var_by_index(451) -> {l_call_last, 8};
var_by_index(452) -> {is_nil, 11};
var_by_index(453) -> {l_gc_bif1, 2};
var_by_index(454) -> {l_is_ne_exact_immed, 10};
var_by_index(455) -> {l_fetch, 22};
var_by_index(456) -> {l_increment, 4};
var_by_index(457) -> {extract_next_element3, 4};
var_by_index(458) -> {is_nil, 30};
var_by_index(459) -> {extract_next_element3, 10};
var_by_index(460) -> {l_bs_append, 0};
var_by_index(461) -> {is_nonempty_list, 14};
var_by_index(462) -> {is_integer, 5};
var_by_index(463) -> {l_move_call_ext, 18};
var_by_index(464) -> {call_bif, 26};
var_by_index(465) -> {l_trim, 6};
var_by_index(466) -> {is_nil, 12};
var_by_index(467) -> {l_call_ext, 14};
var_by_index(468) -> {l_bor, 0};
var_by_index(469) -> {move_return, 7};
var_by_index(470) -> {is_list, 2};
var_by_index(471) -> {l_call_ext, 15};
var_by_index(472) -> {is_nil, 13};
var_by_index(473) -> {l_catch, 4};
var_by_index(474) -> {l_fadd, 0};
var_by_index(475) -> {l_gc_bif1, 5};
var_by_index(476) -> {l_element, 1};
var_by_index(477) -> {extract_next_element2, 17};
var_by_index(478) -> {call_bif, 27};
var_by_index(479) -> {l_allocate, 7};
var_by_index(480) -> {l_move_call_only, 5};
var_by_index(481) -> {l_move_call_ext_last, 4};
var_by_index(482) -> {l_move_call_ext, 20};
var_by_index(483) -> {l_move_call_ext, 19};
var_by_index(484) -> {is_nonempty_list, 15};
var_by_index(485) -> {call_bif, 29};
var_by_index(486) -> {call_bif, 28};
var_by_index(487) -> {is_integer, 1};
var_by_index(488) -> {bif1_body, 1};
var_by_index(489) -> {l_call_ext, 16};
var_by_index(490) -> {l_is_ne_exact_immed, 3};
var_by_index(491) -> {is_nonempty_list, 16};
var_by_index(492) -> {l_is_eq_exact_immed, 19};
var_by_index(493) -> {l_call_ext_last, 3};
var_by_index(494) -> {l_move_call_ext, 21};
var_by_index(495) -> {l_fetch, 12};
var_by_index(496) -> {fmove_1, 0};
var_by_index(497) -> {l_move_call_ext, 22};
var_by_index(498) -> {bif1_body, 2};
var_by_index(499) -> {move_jump, 1};
var_by_index(500) -> {l_bs_get_utf8, 0};
var_by_index(501) -> {case_end, 10};
var_by_index(502) -> {bif2_body, 1};
var_by_index(503) -> {l_move_call_ext, 23};
var_by_index(504) -> {l_bs_skip_bits_all2, 0};
var_by_index(505) -> {l_call_ext, 17};
var_by_index(506) -> {l_is_eq_exact_immed, 20};
var_by_index(507) -> {fconv, 0};
var_by_index(508) -> {l_bor, 1};
var_by_index(509) -> {call_bif, 30};
var_by_index(510) -> {l_is_eq_exact_literal, 4};
var_by_index(511) -> {l_move_call_ext_last, 2};
var_by_index(512) -> {l_bs_init_bits_fail, 0};
var_by_index(513) -> {call_bif, 31};
var_by_index(514) -> {extract_next_element3, 5};
var_by_index(515) -> {extract_next_element2, 8};
var_by_index(516) -> {l_is_eq_exact_immed, 22};
var_by_index(517) -> {apply_last, 0};
var_by_index(518) -> {l_move_call_ext_only, 3};
var_by_index(519) -> {call_bif, 32};
var_by_index(520) -> {call_bif, 4};
var_by_index(521) -> {is_atom, 2};
var_by_index(522) -> {call_bif, 33};
var_by_index(523) -> {put_list, 6};
var_by_index(524) -> {put_list, 8};
var_by_index(525) -> {l_bs_match_string, 3};
var_by_index(526) -> {l_make_export, 0};
var_by_index(527) -> {extract_next_element, 14};
var_by_index(528) -> {l_catch, 5};
var_by_index(529) -> {init, 8};
var_by_index(530) -> {l_increment, 8};
var_by_index(531) -> {move_deallocate_return, 7};
var_by_index(532) -> {l_call_fun, 3};
var_by_index(533) -> {l_select_val2, 6};
var_by_index(534) -> {l_new_bs_put_integer, 0};
var_by_index(535) -> {fmove_2, 0};
var_by_index(536) -> {call_bif, 34};
var_by_index(537) -> {badmatch, 4};
var_by_index(538) -> {is_atom, 3};
var_by_index(539) -> {l_move_call, 14};
var_by_index(540) -> {fmove_2, 1};
var_by_index(541) -> {l_bs_test_zero_tail2, 4};
var_by_index(542) -> {fmove_1, 1};
var_by_index(543) -> {l_move_call_ext, 24};
var_by_index(544) -> {is_integer_allocate, 0};
var_by_index(545) -> {l_call_ext_last, 4};
var_by_index(546) -> {call_bif, 35};
var_by_index(547) -> {l_fetch, 13};
var_by_index(548) -> {l_fast_element, 3};
var_by_index(549) -> {l_call_ext, 18};
var_by_index(550) -> {l_move_call, 15};
var_by_index(551) -> {l_move_call_ext, 25};
var_by_index(552) -> {is_tuple, 9};
var_by_index(553) -> {l_trim, 7};
var_by_index(554) -> {is_list, 6};
var_by_index(555) -> {l_fetch, 14};
var_by_index(556) -> {deallocate_return, 8};
var_by_index(557) -> {l_is_eq_exact_immed, 23};
var_by_index(558) -> {l_call_ext, 19};
var_by_index(559) -> {extract_next_element, 15};
var_by_index(560) -> {l_fetch, 15};
var_by_index(561) -> {l_move_call_last, 5};
var_by_index(562) -> {is_tuple, 3};
var_by_index(563) -> {is_nonempty_list, 17};
var_by_index(564) -> {l_move_call_ext, 15};
var_by_index(565) -> {l_get, 3};
var_by_index(566) -> {extract_next_element, 16};
var_by_index(567) -> {extract_next_element2, 9};
var_by_index(568) -> {try_end, 2};
var_by_index(569) -> {if_end, 0};
var_by_index(570) -> {fmove_1, 2};
var_by_index(571) -> {call_bif, 36};
var_by_index(572) -> {move_return, 8};
var_by_index(573) -> {l_move_call, 16};
var_by_index(574) -> {l_is_ne_exact_immed, 4};
var_by_index(575) -> {l_bs_skip_bits_imm2, 0};
var_by_index(576) -> {call_bif, 38};
var_by_index(577) -> {call_bif, 37};
var_by_index(578) -> {move_jump, 2};
var_by_index(579) -> {try_end, 4};
var_by_index(580) -> {try_end, 3};
var_by_index(581) -> {l_move_call_ext_last, 3};
var_by_index(582) -> {call_bif, 39};
var_by_index(583) -> {self, 2};
var_by_index(584) -> {l_call_ext, 23};
var_by_index(585) -> {l_call_ext, 22};
var_by_index(586) -> {l_call_ext, 21};
var_by_index(587) -> {l_call_ext, 20};
var_by_index(588) -> {is_list, 3};
var_by_index(589) -> {is_nonempty_list, 19};
var_by_index(590) -> {is_nonempty_list, 18};
var_by_index(591) -> {try_end, 5};
var_by_index(592) -> {catch_end, 4};
var_by_index(593) -> {l_call_ext, 25};
var_by_index(594) -> {l_call_ext, 24};
var_by_index(595) -> {extract_next_element2, 10};
var_by_index(596) -> {init, 9};
var_by_index(597) -> {l_bif1, 0};
var_by_index(598) -> {l_call_ext, 26};
var_by_index(599) -> {get_tuple_element, 8};
var_by_index(600) -> {l_select_val_atoms, 2};
var_by_index(601) -> {test_arity, 3};
var_by_index(602) -> {case_end, 3};
var_by_index(603) -> {bif1_body, 3};
var_by_index(604) -> {l_is_eq_exact_immed, 24};
var_by_index(605) -> {l_select_val2, 9};
var_by_index(606) -> {l_bs_get_utf16, 1};
var_by_index(607) -> {is_tuple, 4};
var_by_index(608) -> {l_call_ext, 28};
var_by_index(609) -> {l_call_ext, 27};
var_by_index(610) -> {extract_next_element, 17};
var_by_index(611) -> {l_move_call_ext, 26};
var_by_index(612) -> {l_get, 6};
var_by_index(613) -> {l_call_ext, 29};
var_by_index(614) -> {is_integer, 2};
var_by_index(615) -> {badmatch, 5};
var_by_index(616) -> {l_bs_put_string, 1};
var_by_index(617) -> {try_case_end, 0};
var_by_index(618) -> {l_fdiv, 0};
var_by_index(619) -> {get_list, 6};
var_by_index(620) -> {l_call_ext_last, 6};
var_by_index(621) -> {l_bif1, 1};
var_by_index(622) -> {put_list, 9};
var_by_index(623) -> {move_return, 9};
var_by_index(624) -> {case_end, 4};
var_by_index(625) -> {self, 5};
var_by_index(626) -> {l_call_ext, 30};
var_by_index(627) -> {l_bs_test_unit_8, 1};
var_by_index(628) -> {l_gc_bif1, 3};
var_by_index(629) -> {move_deallocate_return, 9};
var_by_index(630) -> {l_is_eq_exact_immed, 25};
var_by_index(631) -> {call_bif, 41};
var_by_index(632) -> {call_bif, 40};
var_by_index(633) -> {extract_next_element, 18};
var_by_index(634) -> {extract_next_element2, 11};
var_by_index(635) -> {is_nil, 14};
var_by_index(636) -> {l_move_call_only, 6};
var_by_index(637) -> {l_bs_restore2, 2};
var_by_index(638) -> {l_move_call, 18};
var_by_index(639) -> {l_move_call, 17};
var_by_index(640) -> {bif1_body, 5};
var_by_index(641) -> {move, 10};
var_by_index(642) -> {l_move_call_ext, 28};
var_by_index(643) -> {l_move_call_ext, 29};
var_by_index(644) -> {l_bs_get_integer, 0};
var_by_index(645) -> {is_atom, 6};
var_by_index(646) -> {is_integer, 3};
var_by_index(647) -> {l_allocate_zero, 8};
var_by_index(648) -> {is_nil, 15};
var_by_index(649) -> {is_list, 4};
var_by_index(650) -> {case_end, 5};
var_by_index(651) -> {l_increment, 7};
var_by_index(652) -> {l_is_eq_exact_immed, 26};
var_by_index(653) -> {l_increment, 5};
var_by_index(654) -> {l_fsub, 0};
var_by_index(655) -> {get_tuple_element, 9};
var_by_index(656) -> {fconv, 1};
var_by_index(657) -> {call_bif, 42};
var_by_index(658) -> {l_bsr, 1};
var_by_index(659) -> {l_move_call_ext, 30};
var_by_index(660) -> {call_bif, 43};
var_by_index(661) -> {l_call_ext, 31};
var_by_index(662) -> {extract_next_element3, 6};
var_by_index(663) -> {badmatch, 6};
var_by_index(664) -> {put_list, 7};
var_by_index(665) -> {l_move_call_ext, 32};
var_by_index(666) -> {l_move_call_ext, 31};
var_by_index(667) -> {l_call_ext, 33};
var_by_index(668) -> {l_call_ext, 32};
var_by_index(669) -> {extract_next_element2, 12};
var_by_index(670) -> {is_integer, 4};
var_by_index(671) -> {move_return, 10};
var_by_index(672) -> {l_rem, 1};
var_by_index(673) -> {l_bs_put_string, 0};
var_by_index(674) -> {is_nonempty_list, 20};
var_by_index(675) -> {move_deallocate_return, 8};
var_by_index(676) -> {l_move_call, 19};
var_by_index(677) -> {l_is_eq_exact_literal, 5};
var_by_index(678) -> {l_call_ext_only, 0};
var_by_index(679) -> {l_plus, 2};
var_by_index(680) -> {l_increment, 6};
var_by_index(681) -> {l_int_div, 0};
var_by_index(682) -> {l_bs_get_binary_imm2, 0};
var_by_index(683) -> {l_is_eq_exact_literal, 6};
var_by_index(684) -> {l_move_call_ext, 33};
var_by_index(685) -> {node, 4};
var_by_index(686) -> {l_call_ext, 34};
var_by_index(687) -> {put_list, 10};
var_by_index(688) -> {l_move_call, 20};
var_by_index(689) -> {init, 10};
var_by_index(690) -> {catch_end, 5};
var_by_index(691) -> {badmatch, 15};
var_by_index(692) -> {bif1_body, 6};
var_by_index(693) -> {l_fetch, 16};
var_by_index(694) -> {test_heap_1_put_list, 2};
var_by_index(695) -> {l_allocate, 9};
var_by_index(696) -> {l_yield, 0};
var_by_index(697) -> {is_atom, 4};
var_by_index(698) -> {l_move_call, 21};
var_by_index(699) -> {l_fetch, 17};
var_by_index(700) -> {l_bif2, 5};
var_by_index(701) -> {l_is_eq_exact_immed, 27};
var_by_index(702) -> {get_list, 7};
var_by_index(703) -> {l_bs_get_binary_all2, 1};
var_by_index(704) -> {call_bif, 44};
var_by_index(705) -> {node, 2};
var_by_index(706) -> {l_call_ext, 37};
var_by_index(707) -> {l_call_ext, 36};
var_by_index(708) -> {l_call_ext, 35};
var_by_index(709) -> {l_call_last, 9};
var_by_index(710) -> {is_nil, 16};
var_by_index(711) -> {is_list, 5};
var_by_index(712) -> {case_end, 6};
var_by_index(713) -> {l_new_bs_put_float_imm, 1};
var_by_index(714) -> {l_move_call, 22};
var_by_index(715) -> {l_catch, 6};
var_by_index(716) -> {l_move_call_last, 6};
var_by_index(717) -> {l_move_call_ext, 35};
var_by_index(718) -> {l_bs_append, 1};
var_by_index(719) -> {l_call_ext, 38};
var_by_index(720) -> {case_end, 7};
var_by_index(721) -> {is_nonempty_list, 21};
var_by_index(722) -> {l_select_tuple_arity, 3};
var_by_index(723) -> {l_move_call_ext, 36};
var_by_index(724) -> {l_is_ne_exact_immed, 5};
var_by_index(725) -> {l_jump_on_val, 0};
var_by_index(726) -> {l_bs_get_integer_32, 1};
var_by_index(727) -> {l_bs_skip_bits2, 1};
var_by_index(728) -> {is_function, 1};
var_by_index(729) -> {l_gc_bif1, 4};
var_by_index(730) -> {l_call_ext, 39};
var_by_index(731) -> {l_is_eq_exact_immed, 21};
var_by_index(732) -> {is_nonempty_list, 22};
var_by_index(733) -> {l_int_div, 1};
var_by_index(734) -> {l_is_eq_exact_immed, 28};
var_by_index(735) -> {l_is_ne_exact_immed, 6};
var_by_index(736) -> {l_call_ext, 40};
var_by_index(737) -> {extract_next_element2, 13};
var_by_index(738) -> {move_jump, 3};
var_by_index(739) -> {move_return, 11};
var_by_index(740) -> {badmatch, 7};
var_by_index(741) -> {l_bs_test_unit_8, 3};
var_by_index(742) -> {l_bs_test_unit_8, 2};
var_by_index(743) -> {l_move_call, 24};
var_by_index(744) -> {l_move_call, 23};
var_by_index(745) -> {l_move_call_ext_only, 4};
var_by_index(746) -> {l_move_call_ext, 37};
var_by_index(747) -> {l_is_eq_exact_immed, 29};
var_by_index(748) -> {l_call_ext, 41};
var_by_index(749) -> {move_return, 12};
var_by_index(750) -> {is_nil, 18};
var_by_index(751) -> {is_nil, 17};
var_by_index(752) -> {is_nonempty_list, 23};
var_by_index(753) -> {l_move_call, 27};
var_by_index(754) -> {l_move_call, 26};
var_by_index(755) -> {l_move_call, 25};
var_by_index(756) -> {l_select_tuple_arity, 1};
var_by_index(757) -> {l_fetch, 18};
var_by_index(758) -> {bif1_body, 7};
var_by_index(759) -> {l_move_call_ext, 38};
var_by_index(760) -> {call_bif, 1};
var_by_index(761) -> {l_call_ext, 48};
var_by_index(762) -> {l_call_ext, 47};
var_by_index(763) -> {l_call_ext, 46};
var_by_index(764) -> {l_call_ext, 45};
var_by_index(765) -> {l_call_ext, 44};
var_by_index(766) -> {l_call_ext, 43};
var_by_index(767) -> {l_call_ext, 42};
var_by_index(768) -> {extract_next_element3, 7};
var_by_index(769) -> {l_bs_save2, 1};
var_by_index(770) -> {bif2_body, 2};
var_by_index(771) -> {is_binary, 3};
var_by_index(772) -> {l_move_call_ext, 39};
var_by_index(773) -> {deallocate_return, 12};
var_by_index(774) -> {l_catch, 8};
var_by_index(775) -> {l_allocate, 8};
var_by_index(776) -> {l_call_ext, 50};
var_by_index(777) -> {l_call_ext, 49};
var_by_index(778) -> {l_trim, 8};
var_by_index(779) -> {is_nil, 19};
var_by_index(780) -> {case_end, 8};
var_by_index(781) -> {l_call_fun, 4};
var_by_index(782) -> {l_gc_bif1, 6};
var_by_index(783) -> {l_bs_skip_bits2, 0};
var_by_index(784) -> {l_move_call_ext, 40};
var_by_index(785) -> {l_call_ext, 53};
var_by_index(786) -> {l_call_ext, 52};
var_by_index(787) -> {l_call_ext, 51};
var_by_index(788) -> {extract_next_element, 19};
var_by_index(789) -> {l_is_ne_exact_literal, 0};
var_by_index(790) -> {l_move_call_ext_only, 5};
var_by_index(791) -> {l_call_ext_last, 5};
var_by_index(792) -> {l_select_tuple_arity2, 2};
var_by_index(793) -> {bs_context_to_binary, 5};
var_by_index(794) -> {l_select_val2, 10};
var_by_index(795) -> {l_fetch, 19};
var_by_index(796) -> {init, 15};
var_by_index(797) -> {l_get, 4};
var_by_index(798) -> {l_call_ext, 54};
var_by_index(799) -> {move_return, 13};
var_by_index(800) -> {badmatch, 8};
var_by_index(801) -> {l_bs_test_unit_8, 4};
var_by_index(802) -> {is_pid, 1};
var_by_index(803) -> {is_boolean, 0};
var_by_index(804) -> {bif1_body, 4};
var_by_index(805) -> {l_bs_get_binary2, 1};
var_by_index(806) -> {put_list, 12};
var_by_index(807) -> {l_call_ext, 58};
var_by_index(808) -> {l_call_ext, 57};
var_by_index(809) -> {l_call_ext, 56};
var_by_index(810) -> {l_call_ext, 55};
var_by_index(811) -> {extract_next_element2, 14};
var_by_index(812) -> {move_jump, 4};
var_by_index(813) -> {move_return, 14};
var_by_index(814) -> {l_move_call_only, 7};
var_by_index(815) -> {bs_context_to_binary, 1};
var_by_index(816) -> {l_call_ext, 65};
var_by_index(817) -> {l_call_ext, 64};
var_by_index(818) -> {l_call_ext, 63};
var_by_index(819) -> {l_call_ext, 62};
var_by_index(820) -> {l_call_ext, 61};
var_by_index(821) -> {l_call_ext, 60};
var_by_index(822) -> {l_call_ext, 59};
var_by_index(823) -> {extract_next_element, 20};
var_by_index(824) -> {l_move_call_last, 7};
var_by_index(825) -> {extract_next_element2, 15};
var_by_index(826) -> {move_return, 15};
var_by_index(827) -> {l_move_call, 28};
var_by_index(828) -> {init, 11};
var_by_index(829) -> {l_element, 0};
var_by_index(830) -> {l_call_ext, 68};
var_by_index(831) -> {l_call_ext, 67};
var_by_index(832) -> {l_call_ext, 66};
var_by_index(833) -> {l_bs_start_match2, 3};
var_by_index(834) -> {move_jump, 6};
var_by_index(835) -> {move_jump, 5};
var_by_index(836) -> {is_nonempty_list_test_heap, 0};
var_by_index(837) -> {catch_end, 6};
var_by_index(838) -> {l_get, 5};
var_by_index(839) -> {l_bs_skip_bits_all2, 1};
var_by_index(840) -> {l_wait_timeout, 2};
var_by_index(841) -> {is_nonempty_list_test_heap, 1};
var_by_index(842) -> {l_call_ext, 70};
var_by_index(843) -> {l_call_ext, 69};
var_by_index(844) -> {l_move_call_only, 8};
var_by_index(845) -> {case_end, 9};
var_by_index(846) -> {is_pid, 0};
var_by_index(847) -> {l_new_bs_put_float_imm, 0};
var_by_index(848) -> {l_move_call, 29};
var_by_index(849) -> {l_select_tuple_arity, 2};
var_by_index(850) -> {catch_end, 8};
var_by_index(851) -> {l_bif2, 6};
var_by_index(852) -> {bif2_body, 3};
var_by_index(853) -> {node, 3};
var_by_index(854) -> {bs_init_writable, 0};
var_by_index(855) -> {l_call_ext, 73};
var_by_index(856) -> {l_call_ext, 72};
var_by_index(857) -> {l_call_ext, 71};
var_by_index(858) -> {extract_next_element, 21};
var_by_index(859) -> {move_jump, 7};
var_by_index(860) -> {move_return, 17};
var_by_index(861) -> {move_return, 16};
var_by_index(862) -> {l_new_bs_put_integer_imm, 1};
var_by_index(863) -> {bs_context_to_binary, 2};
var_by_index(864) -> {put_list, 11};
var_by_index(865) -> {is_nonempty_list, 25};
var_by_index(866) -> {is_nonempty_list, 24};
var_by_index(867) -> {try_end, 6};
var_by_index(868) -> {l_bs_private_append, 0};
var_by_index(869) -> {deallocate_return, 9};
var_by_index(870) -> {l_move_call, 30};
var_by_index(871) -> {l_call_ext_only, 1};
var_by_index(872) -> {l_apply, 0};
var_by_index(873) -> {l_move_call_ext, 27};
var_by_index(874) -> {l_bs_get_integer_imm, 0};
var_by_index(875) -> {test_heap_1_put_list, 3};
var_by_index(876) -> {self, 3};
var_by_index(877) -> {is_tuple, 5};
var_by_index(878) -> {l_call_ext, 76};
var_by_index(879) -> {l_call_ext, 75};
var_by_index(880) -> {l_call_ext, 74};
var_by_index(881) -> {l_is_ne_exact_immed, 7};
var_by_index(882) -> {extract_next_element3, 8};
var_by_index(883) -> {l_move_call, 33};
var_by_index(884) -> {l_move_call, 32};
var_by_index(885) -> {l_move_call, 31};
var_by_index(886) -> {l_catch, 7};
var_by_index(887) -> {catch_end, 7};
var_by_index(888) -> {l_is_ne_exact_immed, 8};
var_by_index(889) -> {l_select_val2, 11};
var_by_index(890) -> {l_fetch, 20};
var_by_index(891) -> {l_call_ext, 77};
var_by_index(892) -> {extract_next_element3, 9};
var_by_index(893) -> {move_jump, 8};
var_by_index(894) -> {l_bs_get_utf8, 1};
var_by_index(895) -> {is_nonempty_list, 26};
var_by_index(896) -> {is_binary, 2};
var_by_index(897) -> {l_fetch, 21};
var_by_index(898) -> {l_bs_skip_bits_all2, 2};
var_by_index(899) -> {self, 4};
var_by_index(900) -> {l_call_ext, 80};
var_by_index(901) -> {l_call_ext, 79};
var_by_index(902) -> {l_call_ext, 78};
var_by_index(903) -> {l_call_last, 10};
var_by_index(904) -> {l_new_bs_put_integer, 1};
var_by_index(905) -> {move_return, 18};
var_by_index(906) -> {is_nil, 20};
var_by_index(907) -> {recv_mark, 0};
var_by_index(908) -> {bs_context_to_binary, 3};
var_by_index(909) -> {badmatch, 10};
var_by_index(910) -> {badmatch, 9};
var_by_index(911) -> {is_function, 0};
var_by_index(912) -> {l_recv_set, 0};
var_by_index(913) -> {l_bs_get_integer_16, 0};
var_by_index(914) -> {move2, 9};
var_by_index(915) -> {l_call_ext, 88};
var_by_index(916) -> {l_call_ext, 87};
var_by_index(917) -> {l_call_ext, 86};
var_by_index(918) -> {l_call_ext, 85};
var_by_index(919) -> {l_call_ext, 84};
var_by_index(920) -> {l_call_ext, 83};
var_by_index(921) -> {l_call_ext, 82};
var_by_index(922) -> {l_call_ext, 81};
var_by_index(923) -> {l_is_eq_exact_immed, 30};
var_by_index(924) -> {extract_next_element2, 16};
var_by_index(925) -> {l_bs_get_float2, 0};
var_by_index(926) -> {move_jump, 9};
var_by_index(927) -> {move_return, 19};
var_by_index(928) -> {l_trim, 11};
var_by_index(929) -> {is_nil, 21};
var_by_index(930) -> {l_select_val2, 12};
var_by_index(931) -> {is_atom, 5};
var_by_index(932) -> {l_move_call_ext, 34};
var_by_index(933) -> {is_float, 0};
var_by_index(934) -> {l_is_ne_exact_immed, 9};
var_by_index(935) -> {l_minus, 2};
var_by_index(936) -> {l_fast_element, 4};
var_by_index(937) -> {move_return, 22};
var_by_index(938) -> {move_return, 21};
var_by_index(939) -> {move_return, 20};
var_by_index(940) -> {is_nonempty_list, 28};
var_by_index(941) -> {is_nonempty_list, 27};
var_by_index(942) -> {l_bif1, 2};
var_by_index(943) -> {deallocate_return, 11};
var_by_index(944) -> {deallocate_return, 10};
var_by_index(945) -> {l_bs_init_bits, 0};
var_by_index(946) -> {get_list, 8};
var_by_index(947) -> {l_is_eq_exact_immed, 31};
var_by_index(948) -> {get_list, 10};
var_by_index(949) -> {is_tuple, 6};
var_by_index(950) -> {l_call_last, 11};
var_by_index(951) -> {extract_next_element, 22};
var_by_index(952) -> {move_return, 24};
var_by_index(953) -> {move_return, 23};
var_by_index(954) -> {badmatch, 11};
var_by_index(955) -> {l_select_val2, 13};
var_by_index(956) -> {l_call_ext_only, 2};
var_by_index(957) -> {get_tuple_element, 10};
var_by_index(958) -> {move, 12};
var_by_index(959) -> {l_gc_bif1, 7};
var_by_index(960) -> {wait_timeout, 0};
var_by_index(961) -> {badmatch, 12};
var_by_index(962) -> {is_nonempty_list, 29};
var_by_index(963) -> {l_times, 2};
var_by_index(964) -> {l_apply_fun, 0};
var_by_index(965) -> {l_is_eq_exact_immed, 32};
var_by_index(966) -> {l_is_eq_exact_immed, 33};
var_by_index(967) -> {l_bs_test_tail_imm2, 0};
var_by_index(968) -> {l_bs_get_integer_32, 2};
var_by_index(969) -> {move_return, 25};
var_by_index(970) -> {is_nil, 22};
var_by_index(971) -> {badmatch, 13};
var_by_index(972) -> {is_integer_allocate, 1};
var_by_index(973) -> {l_is_eq_exact_immed, 34};
var_by_index(974) -> {get_list, 9};
var_by_index(975) -> {is_tuple, 8};
var_by_index(976) -> {is_tuple, 7};
var_by_index(977) -> {extract_next_element, 23};
var_by_index(978) -> {move_jump, 10};
var_by_index(979) -> {is_nil, 23};
var_by_index(980) -> {bs_context_to_binary, 4};
var_by_index(981) -> {badmatch, 14};
var_by_index(982) -> {is_nonempty_list, 32};
var_by_index(983) -> {is_nonempty_list, 31};
var_by_index(984) -> {is_nonempty_list, 30};
var_by_index(985) -> {l_bs_init_fail, 1};
var_by_index(986) -> {move_jump, 11};
var_by_index(987) -> {is_nil, 24};
var_by_index(988) -> {is_nonempty_list, 34};
var_by_index(989) -> {is_nonempty_list, 33};
var_by_index(990) -> {try_end, 7};
var_by_index(991) -> {init, 12};
var_by_index(992) -> {l_bs_add, 1};
var_by_index(993) -> {l_wait_timeout, 0};
var_by_index(994) -> {l_fast_element, 5};
var_by_index(995) -> {test_heap_1_put_list, 4};
var_by_index(996) -> {l_gc_bif2, 0};
var_by_index(997) -> {l_bs_put_utf16, 0};
var_by_index(998) -> {l_is_eq_exact_immed, 35};
var_by_index(999) -> {move_return, 27};
var_by_index(1000) -> {move_return, 26};
var_by_index(1001) -> {l_trim, 9};
var_by_index(1002) -> {is_nil, 25};
var_by_index(1003) -> {l_bs_validate_unicode, 0};
var_by_index(1004) -> {is_nonempty_list, 35};
var_by_index(1005) -> {l_bs_init, 0};
var_by_index(1006) -> {l_jump_on_val, 1};
var_by_index(1007) -> {move, 11};
var_by_index(1008) -> {l_bs_utf16_size, 0};
var_by_index(1009) -> {l_bs_get_binary2, 2};
var_by_index(1010) -> {l_bs_restore2, 3};
var_by_index(1011) -> {is_nil, 26};
var_by_index(1012) -> {raise, 1};
var_by_index(1013) -> {l_int_bnot, 0};
var_by_index(1014) -> {is_nil, 29};
var_by_index(1015) -> {is_nil, 28};
var_by_index(1016) -> {is_nil, 27};
var_by_index(1017) -> {is_nonempty_list, 37};
var_by_index(1018) -> {is_nonempty_list, 36};
var_by_index(1019) -> {l_bs_save2, 2};
var_by_index(1020) -> {l_bs_get_binary_imm2, 1};
var_by_index(1021) -> {is_bitstr, 0};
var_by_index(1022) -> {l_new_bs_put_binary_all, 2};
var_by_index(1023) -> {l_new_bs_put_binary, 0};
var_by_index(1024) -> {fmove_2, 2};
var_by_index(1025) -> {is_reference, 0};
var_by_index(1026) -> {is_port, 0};
var_by_index(1027) -> {is_number, 0};
var_by_index(1028) -> {move, 13};
var_by_index(1029) -> {l_bs_get_binary_all_reuse, 1};
var_by_index(1030) -> {init, 13};
var_by_index(1031) -> {l_wait_timeout, 1};
var_by_index(1032) -> {l_select_tuple_arity, 4};
var_by_index(1033) -> {l_trim, 10};
var_by_index(1034) -> {l_bs_put_utf8, 0};
var_by_index(1035) -> {init, 14};
var_by_index(1036) -> {l_fnegate, 0};
var_by_index(1037) -> {l_bs_get_integer_imm, 1};
var_by_index(1038) -> {l_jump_on_val, 2};
var_by_index(1039) -> {l_bs_utf8_size, 0};
var_by_index(1040) -> {l_bs_get_binary_imm2, 2};
var_by_index(1041) -> {l_bs_validate_unicode_retract, 0};
var_by_index(1042) -> {l_bxor, 0};
var_by_index(1043) -> {l_new_bs_put_float, 0};
var_by_index(1044) -> {l_apply_last, 0};
var_by_index(1045) -> {l_is_function2, 0};
var_by_index(1046) -> {l_gc_bif3, 0};
var_by_index(1047) -> {l_bor, 2};
var_by_index(1048) -> {l_new_bs_put_binary_imm, 0};
var_by_index(1049) -> {l_bs_get_integer_8, 2};
var_by_index(1050) -> {l_bs_start_match2, 4};
var_by_index(1051) -> {l_rem, 2};
var_by_index(1052) -> {l_bs_get_integer_small_imm, 1};
var_by_index(1053) -> {l_bsl, 2};
var_by_index(1054) -> {l_apply_only, 0};
var_by_index(1055) -> {on_load, 0};
var_by_index(1056) -> {move2, 10};
var_by_index(1057) -> {l_int_div, 2};
var_by_index(1058) -> {l_bs_test_unit, 0};
var_by_index(1059) -> {l_m_div, 0};
var_by_index(1060) -> {l_hibernate, 0};
var_by_index(1061) -> {l_apply_fun_last, 0};
var_by_index(1062) -> {is_function2, 0};
var_by_index(1063) -> {l_apply_fun_only, 0};
var_by_index(1064) -> {l_band, 2};
var_by_index(1065) -> {is_bigint, 0};
var_by_index(1066) -> {test_heap, 1};
var_by_index(1067) -> {func_info, 0};
var_by_index(1068) -> {call_bif, 0};
var_by_index(1069) -> {l_bs_get_utf16, 2};
var_by_index(1070) -> {l_put_tuple, 7};
var_by_index(1071) -> {get_tuple_element, 11};
var_by_index(1072) -> {allocate_init, 1};
var_by_index(1073) -> {l_call_fun_last, 1};
var_by_index(1074) -> {set_tuple_element, 2};
var_by_index(1075) -> {allocate_heap, 1};
var_by_index(1076) -> {is_tuple_of_arity, 4};
var_by_index(1077) -> {test_arity, 4};
var_by_index(1078) -> {l_bs_match_string, 4};
var_by_index(1079) -> {is_nonempty_list_allocate, 2};
var_by_index(1080) -> {l_bs_append, 2};
var_by_index(1081) -> {try_case_end, 1};
var_by_index(1082) -> {init3, 1};
var_by_index(1083) -> {l_select_val_smallints, 2};
var_by_index(1084) -> {l_select_tuple_arity2, 3};
var_by_index(1085) -> {init2, 1};
var_by_index(1086) -> {l_bs_get_binary_all2, 2};
var_by_index(1087) -> {is_nonempty_list_test_heap, 2};
var_by_index(1088) -> {allocate_heap_zero, 1};
var_by_index(1089) -> {l_bs_init_heap_bin, 1};
var_by_index(1090) -> {l_plus, 3};
var_by_index(1091) -> {l_bs_get_integer, 1};

var_by_index(Index) -> erlang:error({novarat,Index}).

%%EOF


