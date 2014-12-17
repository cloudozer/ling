
-module(ling_iopvars).
-export([var_order/0]).
-export([fit_args/2]).
-export([var_args/2]).
-export([var_index/2]).
-export([var_by_index/1]).

var_order() -> [
		{move,0},
	{move,1},
	{test_heap,0},
	{l_call,0},
	{move,2},
	{move,3},
	{l_put_tuple,0},
	{get_tuple_element,0},
	{move2,1},
	{badmatch,0},
	{put_list,0},
	{is_tuple_of_arity,1},
	{get_tuple_element,1},
	{move2,0},
	{l_call_only,0},
	{move,4},
	{l_is_eq_exact_immed,0},
	{l_is_eq_exact_immed,1},
	{is_tuple_of_arity,0},
	{put_list,1},
	{get_list,0},
	{l_put_tuple,1},
	{l_is_ge,0},
	{l_call_ext,102},
	{l_make_fun,0},
	{return,0},
	{is_tuple_of_arity,2},
	{l_move_call_last,0},
	{extract_next_element2,0},
	{l_fetch,0},
	{call_bif,3},
	{move_deallocate_return,0},
	{l_trim,0},
	{l_allocate,0},
	{move2,3},
	{put_list,2},
	{is_nil,0},
	{l_fetch,1},
	{is_nonempty_list,0},
	{move_return,29},
	{l_allocate,1},
	{extract_next_element,0},
	{deallocate_return,0},
	{case_end,0},
	{l_is_eq_exact_immed,2},
	{jump,0},
	{get_list,1},
	{call_bif,7},
	{move2,2},
	{get_tuple_element,2},
	{l_is_eq_exact,0},
	{l_move_call,34},
	{l_allocate,2},
	{move_return,0},
	{l_is_eq_exact_immed,3},
	{get_tuple_element,3},
	{l_fetch,2},
	{move,5},
	{init2,0},
	{init,0},
	{l_is_eq_exact_immed,4},
	{deallocate_return,1},
	{call_bif,8},
	{put_list,3},
	{extract_next_element,1},
	{move2,4},
	{is_tuple_of_arity,3},
	{l_fetch,3},
	{get_tuple_element,4},
	{l_allocate,3},
	{init,1},
	{move2,5},
	{deallocate_return,2},
	{l_put_tuple,2},
	{l_fetch,4},
	{l_move_call_ext,44},
	{is_nonempty_list,1},
	{l_is_eq_exact_immed,5},
	{l_trim,1},
	{init3,0},
	{l_allocate_zero,0},
	{allocate_init,0},
	{call_bif,6},
	{test_heap_1_put_list,0},
	{allocate_heap,0},
	{l_allocate_zero,1},
	{init,2},
	{get_list,2},
	{move_deallocate_return,1},
	{call_bif,5},
	{l_move_call,0},
	{call_bif,48},
	{l_call_last,0},
	{move_return,1},
	{l_is_eq_exact_literal,0},
	{is_nonempty_list,2},
	{is_tuple,0},
	{is_nonempty_list_allocate,0},
	{deallocate_return,3},
	{l_is_eq_exact_immed,6},
	{l_call_last,1},
	{is_list,0},
	{l_move_call_only,0},
	{l_select_val_atoms,0},
	{extract_next_element,2},
	{get_list,3},
	{l_trim,2},
	{l_call_last,2},
	{l_increment,0},
	{move,6},
	{move_deallocate_return,2},
	{l_is_lt,0},
	{is_nonempty_list,3},
	{l_select_val2,3},
	{l_select_val2,0},
	{l_select_val2,4},
	{init,3},
	{is_nil,1},
	{l_move_call_only,1},
	{extract_next_element2,1},
	{l_bs_start_match2,0},
	{l_move_call_only,9},
	{extract_next_element3,0},
	{remove_message,0},
	{get_tuple_element,5},
	{l_fetch,5},
	{l_new_bs_put_integer_imm,0},
	{l_catch,0},
	{l_allocate_zero,2},
	{move_return,2},
	{l_call_ext,0},
	{l_allocate,4},
	{move2,6},
	{extract_next_element,3},
	{l_select_val2,1},
	{test_arity,0},
	{is_nil,2},
	{move_jump,13},
	{l_allocate_zero,3},
	{catch_end,0},
	{deallocate_return,4},
	{l_is_eq_exact_immed,7},
	{move_return,3},
	{move_return,4},
	{l_move_call_ext,0},
	{put_list,4},
	{l_put_tuple,3},
	{move,7},
	{move2,7},
	{l_is_eq_exact_immed,8},
	{call_bif,2},
	{l_select_tuple_arity2,0},
	{is_nonempty_list,4},
	{l_plus,0},
	{l_call_last,3},
	{is_nonempty_list,5},
	{get_list,4},
	{init,4},
	{move_return,5},
	{l_move_call_only,2},
	{l_increment,3},
	{is_nonempty_list,6},
	{l_bs_get_binary_all_reuse,0},
	{l_call_last,4},
	{bif1_body,0},
	{l_gc_bif1,0},
	{l_call_fun,0},
	{test_arity,1},
	{set_tuple_element,0},
	{l_move_call,1},
	{l_bs_match_string,0},
	{l_move_call_last,1},
	{l_bs_test_zero_tail2,0},
	{call_bif,9},
	{send,0},
	{l_is_eq,0},
	{l_move_call_only,3},
	{l_catch,1},
	{l_is_ne,0},
	{l_bs_add,0},
	{l_select_val2,2},
	{l_move_call_ext,1},
	{l_call_ext_only,4},
	{l_select_val_smallints,0},
	{bif2_body,0},
	{is_nonempty_list,7},
	{l_is_ne_exact_immed,0},
	{l_is_eq_exact_immed,9},
	{l_move_call_ext_last,0},
	{extract_next_element2,2},
	{case_end,1},
	{extract_next_element2,3},
	{extract_next_element,4},
	{l_move_call,2},
	{l_select_val_atoms,1},
	{move_deallocate_return,3},
	{l_move_call_ext,2},
	{l_loop_rec,0},
	{l_move_call,3},
	{l_select_tuple_arity2,1},
	{is_nil,3},
	{l_trim,3},
	{l_bs_get_utf16,0},
	{l_bs_restore2,0},
	{init,5},
	{l_move_call,4},
	{catch_end,1},
	{is_nonempty_list,8},
	{is_nonempty_list,9},
	{is_nil,4},
	{l_call_ext_last,0},
	{l_fetch,7},
	{deallocate_return,5},
	{l_bs_init_heap_bin,0},
	{l_allocate_zero,4},
	{wait,0},
	{badmatch,1},
	{call_bif,10},
	{loop_rec_end,0},
	{l_allocate,5},
	{extract_next_element,5},
	{l_move_call_ext,3},
	{move,8},
	{l_select_val2,5},
	{l_allocate_zero,5},
	{l_bs_start_match2,1},
	{l_move_call_ext,4},
	{l_move_call_ext,5},
	{badmatch,2},
	{l_call_last,5},
	{extract_next_element2,4},
	{l_increment,1},
	{l_call_fun_last,0},
	{l_plus,1},
	{init,6},
	{l_move_call_ext,6},
	{l_gc_bif1,1},
	{l_call_last,6},
	{l_call_ext,1},
	{l_trim,4},
	{bs_context_to_binary,0},
	{put_list,5},
	{l_bs_save2,0},
	{move_deallocate_return,4},
	{l_increment,2},
	{extract_next_element,6},
	{call_bif,11},
	{int_code_end,0},
	{l_move_call_ext_only,0},
	{call_bif,12},
	{get_tuple_element,6},
	{l_is_ne_exact_immed,1},
	{l_is_eq_exact_immed,11},
	{call_bif,14},
	{call_bif,13},
	{l_move_call,5},
	{l_put_tuple,4},
	{l_bs_test_zero_tail2,1},
	{self,0},
	{extract_next_element,7},
	{is_nil,5},
	{is_nonempty_list_allocate,1},
	{l_move_call,6},
	{badmatch,3},
	{is_tuple,1},
	{l_select_val2,7},
	{l_move_call_last,2},
	{l_move_call_ext,7},
	{l_bs_test_zero_tail2,2},
	{l_gc_bif1,3},
	{is_atom,0},
	{is_nil,6},
	{is_nonempty_list,10},
	{l_call_ext_last,1},
	{l_select_val_smallints,1},
	{l_fast_element,0},
	{extract_next_element,8},
	{l_bs_match_string,1},
	{is_nonempty_list,11},
	{l_catch,2},
	{l_gc_bif1,4},
	{l_call_ext,2},
	{l_move_call,7},
	{l_bs_test_unit_8,0},
	{call_bif,16},
	{call_bif,15},
	{raise,0},
	{l_call_ext,3},
	{is_nil,7},
	{extract_next_element3,1},
	{move_return,6},
	{l_call_ext_last,2},
	{l_move_call_ext,8},
	{call_bif,17},
	{allocate_heap_zero,0},
	{call_bif,18},
	{l_allocate,6},
	{case_end,2},
	{deallocate_return,6},
	{l_is_eq_exact_literal,7},
	{self,1},
	{get_list,7},
	{init,7},
	{l_minus,0},
	{l_move_call_ext_only,1},
	{call_bif,19},
	{extract_next_element2,5},
	{l_call_fun,1},
	{catch_end,2},
	{l_move_call_ext_only,7},
	{call_bif,20},
	{extract_next_element,9},
	{l_move_call_last,3},
	{l_call_ext,4},
	{try_end,0},
	{l_move_call_ext,9},
	{l_band,0},
	{l_trim,5},
	{l_bif2,0},
	{l_move_call,8},
	{is_nil,8},
	{l_fetch,6},
	{l_call_ext,5},
	{test_heap_1_put_list,1},
	{l_is_eq_exact_immed,12},
	{l_call_ext,6},
	{try_end,1},
	{l_gc_bif1,2},
	{l_bs_get_binary2,0},
	{l_minus,1},
	{l_bs_get_binary_all2,0},
	{l_is_eq_exact_immed,13},
	{is_integer,0},
	{get_tuple_element,7},
	{extract_next_element2,6},
	{l_move_call_only,4},
	{l_is_eq_exact_literal,1},
	{l_catch,3},
	{move_jump,0},
	{is_list,1},
	{extract_next_element2,7},
	{l_bsr,0},
	{l_bif2,1},
	{l_move_call,9},
	{l_call_ext,8},
	{l_call_ext,7},
	{apply,0},
	{l_is_eq_exact_immed,14},
	{set_tuple_element,1},
	{l_bs_test_zero_tail2,3},
	{move_deallocate_return,5},
	{l_bif2,2},
	{l_times,0},
	{l_is_eq_exact_literal,2},
	{l_fast_element,2},
	{l_fetch,8},
	{l_is_eq_exact_immed,10},
	{l_is_eq_exact_literal,3},
	{l_fmul,0},
	{l_move_call_ext,10},
	{call_bif,21},
	{extract_next_element,27},
	{extract_next_element,10},
	{is_nil,9},
	{l_times,1},
	{l_move_call_ext,11},
	{l_allocate_zero,6},
	{l_bs_init_fail,0},
	{l_bs_get_integer_8,0},
	{test_arity,2},
	{extract_next_element3,2},
	{is_nonempty_list,12},
	{l_is_eq_exact_immed,15},
	{l_call_ext,9},
	{l_bsl,0},
	{l_move_call_ext_only,2},
	{l_bs_get_integer_8,1},
	{l_put_tuple,6},
	{is_tuple,2},
	{l_is_ne_exact,0},
	{l_select_val2,6},
	{move,9},
	{l_move_call_ext,12},
	{l_call_last,7},
	{l_call_fun,2},
	{node,0},
	{is_binary,0},
	{l_select_val2,9},
	{l_is_ne_exact_immed,2},
	{l_new_bs_put_binary_all,0},
	{l_bs_test_zero_tail2,5},
	{l_bif2,3},
	{extract_next_element3,3},
	{l_move_call_ext_last,1},
	{get_list,5},
	{l_bif2,4},
	{l_bs_get_integer_32,0},
	{call_bif,22},
	{l_call_ext,10},
	{l_new_bs_put_binary_all,1},
	{catch_end,3},
	{extract_next_element,11},
	{l_fcheckerror,0},
	{fclearerror,0},
	{is_atom,1},
	{l_is_eq_exact_immed,16},
	{l_allocate_zero,9},
	{extract_next_element,12},
	{extract_next_element3,4},
	{get_list,6},
	{l_bs_append,0},
	{l_is_eq_exact_immed,17},
	{node,1},
	{deallocate_return,7},
	{l_bs_get_integer_32,1},
	{timeout,0},
	{l_is_eq_exact_immed,36},
	{l_move_call_ext,14},
	{l_fetch,9},
	{l_move_call,10},
	{l_move_call_ext,15},
	{is_float,1},
	{l_rem,0},
	{l_get,0},
	{move_deallocate_return,6},
	{is_nonempty_list,39},
	{l_fetch,10},
	{call_bif,23},
	{l_call_ext,11},
	{l_fast_element,1},
	{l_move_call,11},
	{l_band,1},
	{l_allocate_zero,7},
	{is_nonempty_list,13},
	{is_binary,1},
	{l_select_tuple_arity,1},
	{l_fetch,11},
	{l_call_ext,12},
	{is_nonempty_list,14},
	{call_bif,24},
	{extract_next_element,13},
	{l_trim,6},
	{is_nil,10},
	{l_move_call,12},
	{l_get,1},
	{l_is_eq_exact_immed,18},
	{is_nil,11},
	{l_select_val2,10},
	{l_bs_get_integer_small_imm,0},
	{l_bsl,1},
	{l_get,2},
	{l_call_ext,13},
	{l_call_last,8},
	{l_is_eq_exact_immed,19},
	{l_move_call_last,4},
	{l_bor,0},
	{l_allocate,7},
	{l_call_ext,14},
	{is_integer,1},
	{is_nil,30},
	{is_nil,13},
	{is_nil,12},
	{l_move_call_only,5},
	{l_move_call,13},
	{move2,8},
	{l_gc_bif1,5},
	{call_bif,25},
	{move_return,7},
	{is_list,2},
	{l_call_ext,15},
	{l_move_call_ext,17},
	{l_move_call_ext,16},
	{l_catch,4},
	{call_bif,27},
	{call_bif,26},
	{extract_next_element2,8},
	{is_nonempty_list,15},
	{l_move_call_ext,18},
	{l_select_val2,17},
	{l_element,1},
	{l_fadd,0},
	{extract_next_element3,11},
	{l_put_tuple,5},
	{bif1_body,1},
	{call_bif,28},
	{move_jump,1},
	{l_is_ne_exact_immed,3},
	{call_bif,29},
	{l_call_ext,16},
	{l_select_val2,8},
	{is_nonempty_list,16},
	{fmove_1,0},
	{l_fetch,12},
	{extract_next_element2,18},
	{l_bs_get_utf8,0},
	{is_atom,2},
	{bif1_body,2},
	{l_is_ne_exact_immed,11},
	{l_call_ext_last,3},
	{l_move_call_ext,19},
	{bif2_body,1},
	{l_move_call_ext,20},
	{call_bif,30},
	{l_call_ext,17},
	{l_is_eq_exact_immed,20},
	{l_bs_skip_bits_all2,0},
	{call_bif,31},
	{case_end,10},
	{l_select_tuple_arity,0},
	{l_move_call_ext,21},
	{l_is_eq_exact_immed,21},
	{fconv,0},
	{l_catch,5},
	{put_list,8},
	{l_move_call_ext_last,2},
	{call_bif,4},
	{extract_next_element3,5},
	{put_list,6},
	{l_is_eq_exact_literal,4},
	{l_select_tuple_arity,2},
	{system_limit,0},
	{l_call_ext_last,4},
	{l_fetch,23},
	{extract_next_element,14},
	{is_binary,2},
	{l_bor,1},
	{call_bif,32},
	{l_call_ext,18},
	{is_integer_allocate,0},
	{l_is_eq_exact_immed,23},
	{l_move_call_ext_only,3},
	{badmatch,4},
	{l_call_fun,3},
	{l_bs_init_bits_fail,0},
	{l_make_export,0},
	{is_integer,6},
	{call_bif,34},
	{call_bif,33},
	{apply_last,0},
	{fmove_2,0},
	{fmove_2,1},
	{fmove_1,1},
	{call_bif,35},
	{l_call_ext,19},
	{extract_next_element,15},
	{try_end,2},
	{deallocate_return,8},
	{l_fetch,13},
	{l_fast_element,3},
	{l_move_call_ext,22},
	{is_tuple,3},
	{l_bs_restore2,1},
	{l_move_call_ext,23},
	{l_bs_test_zero_tail2,4},
	{l_new_bs_put_integer,0},
	{extract_next_element,16},
	{l_trim,7},
	{is_list,6},
	{is_nonempty_list,17},
	{l_fetch,14},
	{extract_next_element2,9},
	{is_atom,3},
	{l_fetch,15},
	{is_integer,2},
	{l_move_call,15},
	{l_move_call,14},
	{l_call_ext,21},
	{l_call_ext,20},
	{try_end,3},
	{l_move_call_ext,13},
	{l_call_ext_last,6},
	{case_end,3},
	{is_nonempty_list,18},
	{try_end,4},
	{init,8},
	{if_end,0},
	{move_deallocate_return,7},
	{call_bif,39},
	{call_bif,38},
	{call_bif,37},
	{call_bif,36},
	{try_end,5},
	{is_nonempty_list_test_heap,0},
	{l_move_call_ext,24},
	{l_move_call_ext_last,3},
	{l_call_ext,25},
	{l_call_ext,24},
	{l_call_ext,23},
	{l_call_ext,22},
	{l_get,3},
	{put_list,9},
	{self,2},
	{l_call_ext,28},
	{l_call_ext,27},
	{l_call_ext,26},
	{extract_next_element,17},
	{is_list,3},
	{catch_end,4},
	{l_move_call_last,5},
	{l_is_ne_exact_immed,4},
	{is_tuple,4},
	{extract_next_element2,10},
	{move_jump,2},
	{l_bs_skip_bits_imm2,0},
	{l_fdiv,0},
	{get_tuple_element,8},
	{l_bif1,0},
	{l_bs_get_utf16,1},
	{extract_next_element,18},
	{is_integer,3},
	{l_bs_test_unit_8,3},
	{is_nonempty_list,19},
	{l_bif1,1},
	{is_tuple,10},
	{bif1_body,3},
	{get_list,8},
	{l_select_val2,11},
	{badmatch,5},
	{l_move_call,16},
	{l_is_eq_exact_immed,24},
	{l_call_ext,30},
	{l_call_ext,29},
	{l_is_eq_exact_immed,25},
	{l_move_call_ext,25},
	{l_select_val_atoms,2},
	{extract_next_element2,11},
	{move_return,8},
	{case_end,4},
	{self,5},
	{l_call_ext,31},
	{extract_next_element3,6},
	{l_bs_get_binary_imm2,0},
	{l_bs_test_unit_8,1},
	{is_list,4},
	{l_bs_restore2,2},
	{is_atom,6},
	{l_move_call_ext_last,5},
	{bif1_body,5},
	{call_bif,41},
	{call_bif,40},
	{is_nil,14},
	{l_move_call_only,6},
	{move_deallocate_return,8},
	{init,9},
	{l_plus,2},
	{test_arity,3},
	{l_get,6},
	{l_rem,1},
	{l_call_ext,32},
	{is_nil,15},
	{case_end,5},
	{l_move_call_ext,27},
	{l_move_call_ext,28},
	{call_bif,42},
	{l_call_ext,33},
	{is_nil,16},
	{l_fsub,0},
	{l_is_eq_exact_literal,5},
	{l_jump_on_val,0},
	{l_move_call_ext,29},
	{get_tuple_element,9},
	{l_move_call_last,6},
	{call_bif,43},
	{is_integer,4},
	{l_bs_put_string,0},
	{is_nonempty_list,20},
	{l_bif2,5},
	{l_move_call_ext,30},
	{put_list,7},
	{l_move_call_ext,31},
	{bif1_body,6},
	{l_int_div,0},
	{node,4},
	{l_bsr,1},
	{l_increment,4},
	{l_put_tuple,7},
	{l_call_ext,34},
	{badmatch,6},
	{case_end,6},
	{l_is_eq_exact_literal,6},
	{l_call_ext_only,0},
	{move,10},
	{l_increment,5},
	{l_allocate,9},
	{l_call_ext,35},
	{l_allocate_zero,8},
	{call_bif,44},
	{l_call_ext,36},
	{l_call_last,9},
	{move_return,9},
	{is_list,5},
	{l_move_call,17},
	{catch_end,5},
	{l_is_eq_exact_immed,26},
	{l_fetch,16},
	{l_put_tuple,8},
	{l_call_ext,38},
	{l_call_ext,37},
	{put_list,10},
	{l_move_call,18},
	{test_heap_1_put_list,2},
	{get_list,9},
	{l_bs_skip_bits2,0},
	{l_fetch,17},
	{call_bif,45},
	{node,2},
	{extract_next_element2,12},
	{is_nil,17},
	{try_case_end,0},
	{l_yield,0},
	{l_bs_get_binary_all2,1},
	{is_atom,4},
	{l_move_call,19},
	{l_call_ext_only,1},
	{l_move_call_ext,33},
	{l_is_eq_exact_immed,27},
	{move_return,10},
	{l_move_call_ext,34},
	{l_bs_get_integer,0},
	{l_bs_append,1},
	{l_move_call_ext,35},
	{l_bs_get_binary2,1},
	{call_bif,46},
	{l_is_eq_exact_immed,22},
	{extract_next_element,19},
	{extract_next_element3,7},
	{is_nonempty_list,22},
	{is_nonempty_list,21},
	{l_move_call,20},
	{l_is_ne_exact_immed,5},
	{l_fetch,18},
	{l_catch,6},
	{call_bif,47},
	{l_allocate,8},
	{l_call_ext,40},
	{l_call_ext,39},
	{l_is_ne_exact_immed,6},
	{badmatch,7},
	{case_end,7},
	{is_binary,3},
	{init,10},
	{l_gc_bif1,6},
	{l_int_div,1},
	{l_is_eq_exact_immed,29},
	{l_is_eq_exact_immed,28},
	{is_function,1},
	{fconv,1},
	{fmove_1,3},
	{l_call_ext,42},
	{l_call_ext,41},
	{move_jump,3},
	{is_nil,18},
	{case_end,8},
	{l_select_val2,12},
	{l_bs_save2,1},
	{l_move_call,23},
	{l_move_call,22},
	{l_move_call,21},
	{l_move_call_ext,36},
	{deallocate_return,12},
	{bif2_body,2},
	{l_move_call_ext,37},
	{l_move_call_ext_last,4},
	{call_bif,1},
	{l_call_ext,49},
	{l_call_ext,48},
	{l_call_ext,47},
	{l_call_ext,46},
	{l_call_ext,45},
	{l_call_ext,44},
	{l_call_ext,43},
	{l_move_call_ext_only,4},
	{l_is_ne_exact_immed,7},
	{l_move_call_ext,38},
	{l_gc_bif1,7},
	{l_call_ext,52},
	{l_call_ext,51},
	{l_call_ext,50},
	{is_nil,19},
	{is_nonempty_list,23},
	{l_call_fun,4},
	{l_move_call_ext_only,5},
	{l_move_call_ext,39},
	{is_pid,1},
	{bif1_body,4},
	{l_call_ext,55},
	{l_call_ext,54},
	{l_call_ext,53},
	{l_trim,8},
	{l_bs_test_unit_8,2},
	{l_move_call,24},
	{l_move_call_ext,42},
	{l_move_call_ext,41},
	{l_move_call_ext,40},
	{l_get,4},
	{l_increment,8},
	{is_boolean,0},
	{l_bs_get_integer_small_imm,1},
	{l_call_ext,57},
	{l_call_ext,56},
	{extract_next_element,20},
	{extract_next_element2,13},
	{move_jump,4},
	{bs_context_to_binary,1},
	{l_bs_put_string,2},
	{l_call_ext_last,5},
	{bif1_body,7},
	{init,11},
	{l_fetch,19},
	{badmatch,16},
	{bs_init_writable,0},
	{l_call_ext,58},
	{l_move_call_only,7},
	{put_list,12},
	{case_end,9},
	{l_bs_private_append,0},
	{l_move_call,25},
	{l_is_ne_exact_literal,0},
	{l_call_ext,65},
	{l_call_ext,64},
	{l_call_ext,63},
	{l_call_ext,62},
	{l_call_ext,61},
	{l_call_ext,60},
	{l_call_ext,59},
	{extract_next_element2,14},
	{move_jump,5},
	{move_return,11},
	{badmatch,9},
	{badmatch,8},
	{l_move_call,26},
	{move_deallocate_return,10},
	{l_call_ext,68},
	{l_call_ext,67},
	{l_call_ext,66},
	{extract_next_element3,8},
	{l_bs_start_match2,2},
	{move_jump,6},
	{move_return,12},
	{put_list,11},
	{deallocate_return,9},
	{l_wait_timeout,2},
	{l_get,5},
	{l_element,0},
	{l_bs_skip_bits_all2,1},
	{l_select_tuple_arity2,2},
	{l_is_eq_exact_immed,30},
	{l_bif2,6},
	{l_call_ext,73},
	{l_call_ext,72},
	{l_call_ext,71},
	{l_call_ext,70},
	{l_call_ext,69},
	{extract_next_element2,15},
	{move_jump,7},
	{move_return,14},
	{move_return,13},
	{l_move_call_only,8},
	{is_nonempty_list,24},
	{l_move_call,28},
	{l_move_call,27},
	{l_call_ext_only,2},
	{fmove_1,2},
	{bif2_body,3},
	{l_new_bs_put_integer_imm,1},
	{self,3},
	{node,3},
	{l_call_ext,76},
	{l_call_ext,75},
	{l_call_ext,74},
	{move_return,15},
	{is_nonempty_list,25},
	{is_pid,0},
	{l_new_bs_put_float_imm,0},
	{l_move_call,30},
	{l_move_call,29},
	{l_move_call_ext,26},
	{test_heap_1_put_list,3},
	{catch_end,6},
	{l_is_ne_exact_immed,8},
	{l_move_call_ext_only,6},
	{l_catch,8},
	{l_call_ext,77},
	{extract_next_element3,9},
	{bs_context_to_binary,2},
	{l_new_bs_put_float_imm,1},
	{l_move_call,31},
	{l_move_call_last,7},
	{l_fetch,20},
	{l_is_ne_exact_immed,9},
	{init,16},
	{l_gc_bif1,9},
	{l_fetch,21},
	{l_catch,7},
	{is_tuple,5},
	{l_call_ext,80},
	{l_call_ext,79},
	{l_call_ext,78},
	{l_is_eq_exact_immed,31},
	{move_return,17},
	{move_return,16},
	{is_nil,20},
	{recv_mark,0},
	{l_bs_get_utf8,1},
	{is_nonempty_list,26},
	{try_end,6},
	{deallocate_return,10},
	{l_bs_get_integer_imm,0},
	{l_move_call,32},
	{catch_end,7},
	{l_recv_set,0},
	{move2,9},
	{l_bs_skip_bits_all2,2},
	{l_new_bs_put_integer,1},
	{test_heap_1_put_list,4},
	{self,4},
	{l_call_ext,84},
	{l_call_ext,83},
	{l_call_ext,82},
	{l_call_ext,81},
	{move_jump,8},
	{l_move_call,33},
	{l_apply,0},
	{get_list,10},
	{l_select_val2,13},
	{l_increment,7},
	{l_call_ext,92},
	{l_call_ext,91},
	{l_call_ext,90},
	{l_call_ext,89},
	{l_call_ext,88},
	{l_call_ext,87},
	{l_call_ext,86},
	{l_call_ext,85},
	{l_call_last,11},
	{extract_next_element3,10},
	{l_bs_get_float2,0},
	{move_jump,9},
	{move_return,18},
	{is_nil,21},
	{badmatch,11},
	{badmatch,10},
	{l_bs_put_string,1},
	{is_nonempty_list,27},
	{l_select_val2,14},
	{l_move_call_ext,32},
	{is_float,0},
	{l_increment,6},
	{get_list,11},
	{is_tuple,6},
	{l_call_ext,95},
	{l_call_ext,94},
	{l_call_ext,93},
	{extract_next_element,21},
	{l_fast_element,4},
	{extract_next_element2,16},
	{is_integer,5},
	{move_return,20},
	{move_return,19},
	{badmatch,12},
	{is_function,0},
	{init,12},
	{l_gc_bif1,8},
	{l_is_eq_exact_immed,32},
	{l_select_val2,15},
	{l_fetch,22},
	{l_call_ext,101},
	{l_call_ext,100},
	{l_call_ext,99},
	{l_call_ext,98},
	{l_call_ext,97},
	{l_call_ext,96},
	{extract_next_element,22},
	{bs_context_to_binary,3},
	{is_nonempty_list,28},
	{get_list,12},
	{try_end,7},
	{l_minus,2},
	{l_times,2},
	{l_bif1,2},
	{deallocate_return,11},
	{l_apply_fun,0},
	{l_call_ext_only,3},
	{l_bs_get_integer_16,0},
	{l_bs_init_bits,0},
	{move,12},
	{wait_timeout,0},
	{l_select_val2,16},
	{l_call_last,10},
	{is_nonempty_list,29},
	{is_atom,5},
	{l_bs_get_integer_32,2},
	{l_trim,11},
	{move_deallocate_return,9},
	{is_map,0},
	{extract_next_element,23},
	{extract_next_element2,17},
	{move_jump,10},
	{move_return,21},
	{l_trim,9},
	{is_nil,22},
	{bs_context_to_binary,4},
	{badmatch,13},
	{is_nonempty_list,30},
	{l_new_bs_put_float_imm,2},
	{l_bs_init_fail,1},
	{l_is_ne_exact_immed,10},
	{l_bs_restore2,3},
	{l_fast_element,5},
	{is_bitstr,0},
	{is_integer_allocate,1},
	{catch_end,8},
	{move_jump,11},
	{is_nil,23},
	{is_nonempty_list,32},
	{is_nonempty_list,31},
	{l_move_call_ext,43},
	{l_is_eq_exact_immed,34},
	{l_is_eq_exact_immed,33},
	{l_move_call_last,8},
	{l_bs_test_tail_imm2,0},
	{fconv,2},
	{l_bs_save2,2},
	{l_jump_on_val,1},
	{l_bs_add,1},
	{is_tuple,8},
	{is_tuple,7},
	{move_return,24},
	{move_return,23},
	{move_return,22},
	{bs_context_to_binary,6},
	{badmatch,14},
	{is_nonempty_list,35},
	{is_nonempty_list,34},
	{is_nonempty_list,33},
	{l_wait_timeout,0},
	{l_gc_bif2,0},
	{get_tuple_element,10},
	{l_bs_put_utf16,0},
	{l_new_bs_put_binary,0},
	{l_bs_validate_unicode,0},
	{l_bs_get_binary2,2},
	{is_tuple,9},
	{extract_next_element,26},
	{extract_next_element,25},
	{extract_next_element,24},
	{move_return,27},
	{move_return,26},
	{move_return,25},
	{is_nil,24},
	{bs_context_to_binary,5},
	{move,11},
	{l_bs_utf16_size,0},
	{l_is_eq_exact_immed,35},
	{raise,1},
	{move_jump,12},
	{move_return,28},
	{is_nil,27},
	{is_nil,26},
	{is_nil,25},
	{is_reference,0},
	{l_int_bnot,0},
	{l_bs_get_binary_imm2,1},
	{l_is_function2,0},
	{fmove_2,2},
	{is_nil,28},
	{badmatch,15},
	{is_nonempty_list,37},
	{is_nonempty_list,36},
	{init,14},
	{init,13},
	{is_port,0},
	{is_nil,29},
	{l_new_bs_put_binary_all,2},
	{is_nonempty_list,38},
	{is_number,0},
	{l_select_tuple_arity,3},
	{l_bs_get_binary_all_reuse,1},
	{l_wait_timeout,1},
	{l_trim,10},
	{l_bs_put_utf8,0},
	{l_fnegate,0},
	{l_bs_init,0},
	{l_apply_last,0},
	{l_jump_on_val,2},
	{move,13},
	{l_bs_utf8_size,0},
	{l_bs_get_binary_imm2,2},
	{l_new_bs_put_float,0},
	{l_bs_validate_unicode_retract,0},
	{init,15},
	{l_bxor,0},
	{l_gc_bif3,0},
	{l_bs_match_string,2},
	{l_bs_get_integer_imm,1},
	{l_bs_get_integer_8,2},
	{l_bor,2},
	{l_bs_start_match2,3},
	{l_new_bs_put_binary_imm,0},
	{l_rem,2},
	{l_bs_get_integer_small_imm,2},
	{l_bsl,2},
	{on_load,0},
	{move2,10},
	{l_int_div,2},
	{l_bs_test_unit,0},
	{l_select_val_atoms,3},
	{l_m_div,0},
	{l_hibernate,0},
	{l_apply_fun_last,0},
	{l_select_val_smallints,2},
	{update_map_assoc,0},
	{get_map_elements,0},
	{has_map_fields,0},
	{is_function2,0},
	{l_apply_only,0},
	{l_apply_fun_only,0},
	{l_band,2},
	{update_map_exact,0},
	{get_map_element,0},
	{has_map_field,0},
	{is_bigint,0},
	{test_heap,1},
	{func_info,0},
	{call_bif,0},
	{l_bs_get_utf16,2},
	{l_put_tuple,9},
	{get_tuple_element,11},
	{allocate_init,1},
	{l_call_fun_last,1},
	{set_tuple_element,2},
	{l_bsr,2},
	{l_bs_get_integer_32,3},
	{allocate_heap,1},
	{is_tuple_of_arity,4},
	{test_arity,4},
	{is_nonempty_list_allocate,2},
	{l_bs_append,2},
	{try_case_end,1},
	{init3,1},
	{l_select_tuple_arity2,3},
	{init2,1},
	{l_bs_get_binary_all2,2},
	{is_nonempty_list_test_heap,1},
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
fit_args(badmatch, [{x,5}]) -> 9;
fit_args(badmatch, [{y,0}]) -> 8;
fit_args(badmatch, [{y,9}]) -> 11;
fit_args(badmatch, [{y,6}]) -> 10;
fit_args(badmatch, [{x,7}]) -> 12;
fit_args(badmatch, [{y,5}]) -> 13;
fit_args(badmatch, [{y,1}]) -> 14;
fit_args(badmatch, [{x,6}]) -> 15;
fit_args(badmatch, [_]) -> 16;
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
fit_args(bs_context_to_binary, [{x,7}]) -> 4;
fit_args(bs_context_to_binary, [{x,4}]) -> 5;
fit_args(bs_context_to_binary, [_]) -> 6;
fit_args(bs_init_writable, []) -> 0;
fit_args(call_bif, [{b,{erlang,error,1}}]) -> 3;
fit_args(call_bif, [{b,{erlang,setelement,3}}]) -> 7;
fit_args(call_bif, [{b,{erlang,'++',2}}]) -> 8;
fit_args(call_bif, [{b,{erlang,throw,1}}]) -> 6;
fit_args(call_bif, [{b,{erlang,exit,1}}]) -> 5;
fit_args(call_bif, [{b,{erlang,error,2}}]) -> 2;
fit_args(call_bif, [{b,{lists,member,2}}]) -> 9;
fit_args(call_bif, [{b,{ets,insert,2}}]) -> 10;
fit_args(call_bif, [{b,{erlang,get_module_info,2}}]) -> 11;
fit_args(call_bif, [{b,{erlang,binary_to_list,1}}]) -> 12;
fit_args(call_bif, [{b,{erlang,list_to_binary,1}}]) -> 14;
fit_args(call_bif, [{b,{ets,delete,1}}]) -> 13;
fit_args(call_bif, [{b,{lists,keysearch,3}}]) -> 16;
fit_args(call_bif, [{b,{ets,info,2}}]) -> 15;
fit_args(call_bif, [{b,{erlang,list_to_atom,1}}]) -> 17;
fit_args(call_bif, [{b,{erlang,integer_to_list,1}}]) -> 18;
fit_args(call_bif, [{b,{erlang,atom_to_list,1}}]) -> 19;
fit_args(call_bif, [{b,{ets,lookup,2}}]) -> 20;
fit_args(call_bif, [{b,{ets,lookup_element,3}}]) -> 21;
fit_args(call_bif, [{b,{erlang,'--',2}}]) -> 22;
fit_args(call_bif, [{b,{re,run,3}}]) -> 23;
fit_args(call_bif, [{b,{erlang,process_flag,2}}]) -> 24;
fit_args(call_bif, [{b,{erlang,process_info,2}}]) -> 25;
fit_args(call_bif, [{b,{erlang,tuple_to_list,1}}]) -> 27;
fit_args(call_bif, [{b,{erlang,unlink,1}}]) -> 26;
fit_args(call_bif, [{b,{erlang,whereis,1}}]) -> 28;
fit_args(call_bif, [{b,{lists,keyfind,3}}]) -> 29;
fit_args(call_bif, [{b,{erlang,iolist_to_binary,1}}]) -> 30;
fit_args(call_bif, [{b,{erlang,list_to_tuple,1}}]) -> 31;
fit_args(call_bif, [{b,{erlang,exit,2}}]) -> 4;
fit_args(call_bif, [{b,{ets,new,2}}]) -> 32;
fit_args(call_bif, [{b,{erlang,make_ref,0}}]) -> 34;
fit_args(call_bif, [{b,{lists,reverse,2}}]) -> 33;
fit_args(call_bif, [{b,{lists,keymember,3}}]) -> 35;
fit_args(call_bif, [{b,{erlang,spawn_link,1}}]) -> 39;
fit_args(call_bif, [{b,{erlang,now,0}}]) -> 38;
fit_args(call_bif, [{b,{erlang,get_stacktrace,0}}]) -> 37;
fit_args(call_bif, [{b,{ets,delete,2}}]) -> 36;
fit_args(call_bif, [{b,{ets,match_object,2}}]) -> 41;
fit_args(call_bif, [{b,{ets,safe_fixtable,2}}]) -> 40;
fit_args(call_bif, [{b,{ets,next,2}}]) -> 42;
fit_args(call_bif, [{b,{ets,match,2}}]) -> 43;
fit_args(call_bif, [{b,{erlang,monitor,2}}]) -> 44;
fit_args(call_bif, [{b,{erlang,fun_info,2}}]) -> 45;
fit_args(call_bif, [{b,{erlang,garbage_collect,0}}]) -> 46;
fit_args(call_bif, [{b,{erlang,register,2}}]) -> 47;
fit_args(call_bif, [{b,{erlang,raise,3}}]) -> 1;
fit_args(call_bif, [{b,{erlang,purge_module,1}}]) -> 0;
fit_args(call_bif, [{b,_}]) -> 48;
fit_args(case_end, [{x,0}]) -> 0;
fit_args(case_end, [{x,1}]) -> 1;
fit_args(case_end, [{x,2}]) -> 2;
fit_args(case_end, [{y,2}]) -> 3;
fit_args(case_end, [{x,3}]) -> 4;
fit_args(case_end, [{y,1}]) -> 5;
fit_args(case_end, [{y,3}]) -> 6;
fit_args(case_end, [{x,4}]) -> 7;
fit_args(case_end, [{y,0}]) -> 8;
fit_args(case_end, [{y,4}]) -> 9;
fit_args(case_end, [_]) -> 10;
fit_args(catch_end, [{y,0}]) -> 0;
fit_args(catch_end, [{y,1}]) -> 1;
fit_args(catch_end, [{y,2}]) -> 2;
fit_args(catch_end, [{y,3}]) -> 3;
fit_args(catch_end, [{y,4}]) -> 4;
fit_args(catch_end, [{y,5}]) -> 5;
fit_args(catch_end, [{y,6}]) -> 6;
fit_args(catch_end, [{y,19}]) -> 7;
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
fit_args(deallocate_return, [10]) -> 10;
fit_args(deallocate_return, [11]) -> 11;
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
fit_args(extract_next_element, [{y,5}]) -> 15;
fit_args(extract_next_element, [{y,4}]) -> 16;
fit_args(extract_next_element, [{x,11}]) -> 17;
fit_args(extract_next_element, [{x,12}]) -> 18;
fit_args(extract_next_element, [{x,13}]) -> 19;
fit_args(extract_next_element, [{x,14}]) -> 20;
fit_args(extract_next_element, [{y,6}]) -> 21;
fit_args(extract_next_element, [{x,18}]) -> 22;
fit_args(extract_next_element, [{x,16}]) -> 23;
fit_args(extract_next_element, [{x,22}]) -> 26;
fit_args(extract_next_element, [{y,7}]) -> 25;
fit_args(extract_next_element, [{x,15}]) -> 24;
fit_args(extract_next_element, [_]) -> 27;
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
fit_args(extract_next_element2, [{x,13}]) -> 12;
fit_args(extract_next_element2, [{x,14}]) -> 13;
fit_args(extract_next_element2, [{x,16}]) -> 14;
fit_args(extract_next_element2, [{y,0}]) -> 15;
fit_args(extract_next_element2, [{x,15}]) -> 16;
fit_args(extract_next_element2, [{x,17}]) -> 17;
fit_args(extract_next_element2, [_]) -> 18;
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
fit_args(extract_next_element3, [{x,9}]) -> 10;
fit_args(extract_next_element3, [_]) -> 11;
fit_args(fclearerror, []) -> 0;
fit_args(fconv, [_,{fr,0}]) -> 0;
fit_args(fconv, [_,{fr,1}]) -> 1;
fit_args(fconv, [_,{fr,_}]) -> 2;
fit_args(fmove_1, [{x,_},{fr,_}]) -> 0;
fit_args(fmove_1, [_,{fr,1}]) -> 1;
fit_args(fmove_1, [_,{fr,0}]) -> 2;
fit_args(fmove_1, [_,{fr,_}]) -> 3;
fit_args(fmove_2, [{fr,_},{x,0}]) -> 0;
fit_args(fmove_2, [{fr,_},{x,_}]) -> 1;
fit_args(fmove_2, [{fr,_},_]) -> 2;
fit_args(func_info, [_,_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(get_list, [{x,0},{x,0},_]) -> 3;
fit_args(get_list, [{x,0},{x,_},{x,_}]) -> 1;
fit_args(get_list, [{x,0},{y,Arg1},{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 9;
fit_args(get_list, [{x,0},_,{x,0}]) -> 6;
fit_args(get_list, [{x,_},{x,0},{x,_}]) -> 2;
fit_args(get_list, [{x,_},{y,Arg1},{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 11;
fit_args(get_list, [{x,_},{x,_},{x,_}]) -> 0;
fit_args(get_list, [{y,Arg0},{x,_},{x,_}]) when Arg0 >= 0, Arg0 =< 255 -> 5;
fit_args(get_list, [{x,_},{y,Arg1},{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 8;
fit_args(get_list, [{x,_},{y,Arg1},{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 10;
fit_args(get_list, [_,{x,0},_]) -> 7;
fit_args(get_list, [_,{x,_},{y,Arg2}]) when Arg2 >= 0, Arg2 =< 255 -> 4;
fit_args(get_list, [_,_,_]) -> 12;
fit_args(get_map_element, [{f,_},_,_,_]) -> 0;
fit_args(get_map_elements, [{f,_},_,_]) -> 0;
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
fit_args(has_map_field, [{f,_},_,_]) -> 0;
fit_args(has_map_fields, [{f,_},_,_]) -> 0;
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
fit_args(init, [{y,14}]) -> 14;
fit_args(init, [{y,13}]) -> 13;
fit_args(init, [{y,16}]) -> 15;
fit_args(init, [_]) -> 16;
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
fit_args(is_integer, [{f,_},{x,5}]) -> 5;
fit_args(is_integer, [{f,_},_]) -> 6;
fit_args(is_integer_allocate, [{f,_},{x,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(is_integer_allocate, [{f,_},_,_]) -> 1;
fit_args(is_list, [{f,_},{x,0}]) -> 0;
fit_args(is_list, [{f,_},{x,1}]) -> 1;
fit_args(is_list, [{f,_},{x,2}]) -> 2;
fit_args(is_list, [{f,_},{x,3}]) -> 3;
fit_args(is_list, [{f,_},{x,4}]) -> 4;
fit_args(is_list, [{f,_},{x,5}]) -> 5;
fit_args(is_list, [{f,_},_]) -> 6;
fit_args(is_map, [{f,_},_]) -> 0;
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
fit_args(is_nil, [{f,_},{x,10}]) -> 10;
fit_args(is_nil, [{f,_},{y,1}]) -> 11;
fit_args(is_nil, [{f,_},{x,11}]) -> 13;
fit_args(is_nil, [{f,_},{x,12}]) -> 12;
fit_args(is_nil, [{f,_},{x,13}]) -> 14;
fit_args(is_nil, [{f,_},{y,2}]) -> 15;
fit_args(is_nil, [{f,_},{x,14}]) -> 16;
fit_args(is_nil, [{f,_},{x,15}]) -> 17;
fit_args(is_nil, [{f,_},{x,16}]) -> 18;
fit_args(is_nil, [{f,_},{y,3}]) -> 19;
fit_args(is_nil, [{f,_},{x,17}]) -> 20;
fit_args(is_nil, [{f,_},{y,0}]) -> 21;
fit_args(is_nil, [{f,_},{x,19}]) -> 22;
fit_args(is_nil, [{f,_},{y,4}]) -> 23;
fit_args(is_nil, [{f,_},{x,22}]) -> 24;
fit_args(is_nil, [{f,_},{x,21}]) -> 27;
fit_args(is_nil, [{f,_},{y,5}]) -> 26;
fit_args(is_nil, [{f,_},{x,18}]) -> 25;
fit_args(is_nil, [{f,_},{x,20}]) -> 28;
fit_args(is_nil, [{f,_},{x,23}]) -> 29;
fit_args(is_nil, [{f,_},_]) -> 30;
fit_args(is_nonempty_list, [{f,_},{x,0}]) -> 0;
fit_args(is_nonempty_list, [{f,_},{x,2}]) -> 1;
fit_args(is_nonempty_list, [{f,_},{x,1}]) -> 2;
fit_args(is_nonempty_list, [{f,_},{x,3}]) -> 3;
fit_args(is_nonempty_list, [{f,_},{x,7}]) -> 4;
fit_args(is_nonempty_list, [{f,_},{x,4}]) -> 5;
fit_args(is_nonempty_list, [{f,_},{x,5}]) -> 6;
fit_args(is_nonempty_list, [{f,_},{x,6}]) -> 7;
fit_args(is_nonempty_list, [{f,_},{x,9}]) -> 8;
fit_args(is_nonempty_list, [{f,_},{x,8}]) -> 9;
fit_args(is_nonempty_list, [{f,_},{x,10}]) -> 10;
fit_args(is_nonempty_list, [{f,_},{x,11}]) -> 11;
fit_args(is_nonempty_list, [{f,_},{x,12}]) -> 12;
fit_args(is_nonempty_list, [{f,_},{x,13}]) -> 13;
fit_args(is_nonempty_list, [{f,_},{y,2}]) -> 14;
fit_args(is_nonempty_list, [{f,_},{y,3}]) -> 15;
fit_args(is_nonempty_list, [{f,_},{x,14}]) -> 16;
fit_args(is_nonempty_list, [{f,_},{x,15}]) -> 17;
fit_args(is_nonempty_list, [{f,_},{y,1}]) -> 18;
fit_args(is_nonempty_list, [{f,_},{x,16}]) -> 19;
fit_args(is_nonempty_list, [{f,_},{x,17}]) -> 20;
fit_args(is_nonempty_list, [{f,_},{y,4}]) -> 22;
fit_args(is_nonempty_list, [{f,_},{x,18}]) -> 21;
fit_args(is_nonempty_list, [{f,_},{x,20}]) -> 23;
fit_args(is_nonempty_list, [{f,_},{x,19}]) -> 24;
fit_args(is_nonempty_list, [{f,_},{y,9}]) -> 25;
fit_args(is_nonempty_list, [{f,_},{y,0}]) -> 26;
fit_args(is_nonempty_list, [{f,_},{y,6}]) -> 27;
fit_args(is_nonempty_list, [{f,_},{x,24}]) -> 28;
fit_args(is_nonempty_list, [{f,_},{x,21}]) -> 29;
fit_args(is_nonempty_list, [{f,_},{x,22}]) -> 30;
fit_args(is_nonempty_list, [{f,_},{y,7}]) -> 32;
fit_args(is_nonempty_list, [{f,_},{x,25}]) -> 31;
fit_args(is_nonempty_list, [{f,_},{y,8}]) -> 35;
fit_args(is_nonempty_list, [{f,_},{x,26}]) -> 34;
fit_args(is_nonempty_list, [{f,_},{y,5}]) -> 33;
fit_args(is_nonempty_list, [{f,_},{x,30}]) -> 37;
fit_args(is_nonempty_list, [{f,_},{x,23}]) -> 36;
fit_args(is_nonempty_list, [{f,_},{x,28}]) -> 38;
fit_args(is_nonempty_list, [{f,_},_]) -> 39;
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
fit_args(is_tuple, [{f,_},{x,1}]) -> 1;
fit_args(is_tuple, [{f,_},{x,2}]) -> 2;
fit_args(is_tuple, [{f,_},{x,3}]) -> 3;
fit_args(is_tuple, [{f,_},{x,4}]) -> 4;
fit_args(is_tuple, [{f,_},{x,7}]) -> 5;
fit_args(is_tuple, [{f,_},{x,5}]) -> 6;
fit_args(is_tuple, [{f,_},{x,6}]) -> 8;
fit_args(is_tuple, [{f,_},{y,4}]) -> 7;
fit_args(is_tuple, [{f,_},{x,8}]) -> 9;
fit_args(is_tuple, [{f,_},_]) -> 10;
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
fit_args(l_bs_get_binary2, [{f,_},{x,_},Arg2,_,Arg4,0,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 1;
fit_args(l_bs_get_binary2, [{f,_},_,Arg2,{x,_},Arg4,0,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 0;
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
fit_args(l_bs_get_integer_32, [{x,_},{f,_},Arg2,{x,0}]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_bs_get_integer_32, [{x,_},{f,_},Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_get_integer_32, [_,{f,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(l_bs_get_integer_8, [{x,0},{f,_},{x,_}]) -> 0;
fit_args(l_bs_get_integer_8, [{x,_},{f,_},{x,_}]) -> 1;
fit_args(l_bs_get_integer_8, [_,{f,_},_]) -> 2;
fit_args(l_bs_get_integer_imm, [_,Arg1,Arg2,{f,_},Arg4,{x,_}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 0;
fit_args(l_bs_get_integer_imm, [_,_,Arg2,{f,_},Arg4,_]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 1;
fit_args(l_bs_get_integer_small_imm, [{x,0},Arg1,{f,_},Arg3,{x,_}]) when Arg1 >= 0, Arg1 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_get_integer_small_imm, [{x,_},Arg1,{f,_},Arg3,{x,_}]) when Arg1 >= 0, Arg1 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_get_integer_small_imm, [_,_,{f,_},Arg3,_]) when Arg3 >= 0, Arg3 =< 255 -> 2;
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
fit_args(l_bs_match_string, [{x,0},{f,_},_,{str,_}]) -> 1;
fit_args(l_bs_match_string, [{x,_},{f,_},Arg2,{str,_}]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_match_string, [_,{f,_},_,{str,_}]) -> 2;
fit_args(l_bs_private_append, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_put_string, [1,{str,_}]) -> 0;
fit_args(l_bs_put_string, [4,{str,_}]) -> 1;
fit_args(l_bs_put_string, [_,{str,_}]) -> 2;
fit_args(l_bs_put_utf16, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_put_utf8, [{f,_},_]) -> 0;
fit_args(l_bs_restore2, [{x,0},0]) -> 1;
fit_args(l_bs_restore2, [{x,0},1]) -> 2;
fit_args(l_bs_restore2, [{x,_},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_restore2, [_,_]) -> 3;
fit_args(l_bs_save2, [{x,0},1]) -> 1;
fit_args(l_bs_save2, [{x,_},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bs_save2, [_,_]) -> 2;
fit_args(l_bs_skip_bits2, [{f,_},_,_,Arg3]) when Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_skip_bits_all2, [{f,_},{x,2},8]) -> 0;
fit_args(l_bs_skip_bits_all2, [{f,_},{x,3},8]) -> 1;
fit_args(l_bs_skip_bits_all2, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_bs_skip_bits_imm2, [{f,_},_,_]) -> 0;
fit_args(l_bs_start_match2, [{x,0},{f,_},Arg2,Arg3,{x,0}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_bs_start_match2, [_,{f,_},Arg2,Arg3,{x,0}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 2;
fit_args(l_bs_start_match2, [_,{f,_},Arg2,Arg3,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_start_match2, [_,{f,_},Arg2,_,_]) when Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(l_bs_test_tail_imm2, [{f,_},_,_]) -> 0;
fit_args(l_bs_test_unit, [{f,_},_,Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_test_unit_8, [{f,_},{x,0}]) -> 0;
fit_args(l_bs_test_unit_8, [{f,_},{x,3}]) -> 1;
fit_args(l_bs_test_unit_8, [{f,_},{x,1}]) -> 2;
fit_args(l_bs_test_unit_8, [{f,_},_]) -> 3;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,2}]) -> 0;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,0}]) -> 1;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,3}]) -> 2;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,1}]) -> 3;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,4}]) -> 4;
fit_args(l_bs_test_zero_tail2, [{f,_},_]) -> 5;
fit_args(l_bs_utf16_size, [_,_]) -> 0;
fit_args(l_bs_utf8_size, [_,_]) -> 0;
fit_args(l_bs_validate_unicode, [{f,_},_]) -> 0;
fit_args(l_bs_validate_unicode_retract, [{f,_}]) -> 0;
fit_args(l_bsl, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bsl, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_bsl, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_bsr, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_bsr, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_bsr, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_bxor, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_call, [{f,_}]) -> 0;
fit_args(l_call_ext, [{e,{lists,reverse,1}}]) -> 0;
fit_args(l_call_ext, [{e,{asn1ct_gen,emit,1}}]) -> 1;
fit_args(l_call_ext, [{e,{lists,foreach,2}}]) -> 2;
fit_args(l_call_ext, [{e,{file,close,1}}]) -> 3;
fit_args(l_call_ext, [{e,{file,open,2}}]) -> 4;
fit_args(l_call_ext, [{e,{lists,foldl,3}}]) -> 5;
fit_args(l_call_ext, [{e,{lists,map,2}}]) -> 6;
fit_args(l_call_ext, [{e,{filename,join,2}}]) -> 8;
fit_args(l_call_ext, [{e,{lists,sort,1}}]) -> 7;
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
fit_args(l_call_ext, [{e,{dict,find,2}}]) -> 21;
fit_args(l_call_ext, [{e,{proplists,get_value,2}}]) -> 20;
fit_args(l_call_ext, [{e,{erlang,list_to_integer,1}}]) -> 25;
fit_args(l_call_ext, [{e,{asn1_db,dbget,2}}]) -> 24;
fit_args(l_call_ext, [{e,{string,tokens,2}}]) -> 23;
fit_args(l_call_ext, [{e,{prettypr,beside,2}}]) -> 22;
fit_args(l_call_ext, [{e,{filename,join,1}}]) -> 28;
fit_args(l_call_ext, [{e,{dict,new,0}}]) -> 27;
fit_args(l_call_ext, [{e,{proplists,get_value,3}}]) -> 26;
fit_args(l_call_ext, [{e,{lists,filter,2}}]) -> 30;
fit_args(l_call_ext, [{e,{dict,store,3}}]) -> 29;
fit_args(l_call_ext, [{e,{sofs,to_external,1}}]) -> 31;
fit_args(l_call_ext, [{e,{ordsets,from_list,1}}]) -> 32;
fit_args(l_call_ext, [{e,{unicode,characters_to_binary,1}}]) -> 33;
fit_args(l_call_ext, [{e,{gb_trees,lookup,2}}]) -> 34;
fit_args(l_call_ext, [{e,{erlang,put,2}}]) -> 35;
fit_args(l_call_ext, [{e,{gb_trees,empty,0}}]) -> 36;
fit_args(l_call_ext, [{e,{lists,last,1}}]) -> 38;
fit_args(l_call_ext, [{e,{lists,delete,2}}]) -> 37;
fit_args(l_call_ext, [{e,{lists,keydelete,3}}]) -> 40;
fit_args(l_call_ext, [{e,{io,format,3}}]) -> 39;
fit_args(l_call_ext, [{e,{filename,basename,1}}]) -> 42;
fit_args(l_call_ext, [{e,{cerl,get_ann,1}}]) -> 41;
fit_args(l_call_ext, [{e,{erlang,binary_to_term,1}}]) -> 49;
fit_args(l_call_ext, [{e,{file,write,2}}]) -> 48;
fit_args(l_call_ext, [{e,{file,read_file_info,1}}]) -> 47;
fit_args(l_call_ext, [{e,{erl_syntax,atom,1}}]) -> 46;
fit_args(l_call_ext, [{e,{ordsets,subtract,2}}]) -> 45;
fit_args(l_call_ext, [{e,{file,read_file,1}}]) -> 44;
fit_args(l_call_ext, [{e,{ssh_channel,cache_lookup,2}}]) -> 43;
fit_args(l_call_ext, [{e,{file,read,2}}]) -> 52;
fit_args(l_call_ext, [{e,{file,make_dir,1}}]) -> 51;
fit_args(l_call_ext, [{e,{lists,duplicate,2}}]) -> 50;
fit_args(l_call_ext, [{e,{asn1ct_gen,mk_var,1}}]) -> 55;
fit_args(l_call_ext, [{e,{erlang,term_to_binary,1}}]) -> 54;
fit_args(l_call_ext, [{e,{mnesia_lib,exists,1}}]) -> 53;
fit_args(l_call_ext, [{e,{gb_trees,get,2}}]) -> 57;
fit_args(l_call_ext, [{e,{asn1ct_gen,get_inner,1}}]) -> 56;
fit_args(l_call_ext, [{e,{sofs,family_union,2}}]) -> 58;
fit_args(l_call_ext, [{e,{mnesia_lib,cs_to_storage_type,2}}]) -> 65;
fit_args(l_call_ext, [{e,{file,rename,2}}]) -> 64;
fit_args(l_call_ext, [{e,{erlang,max,2}}]) -> 63;
fit_args(l_call_ext, [{e,{filename,dirname,1}}]) -> 62;
fit_args(l_call_ext, [{e,{lists,append,1}}]) -> 61;
fit_args(l_call_ext, [{e,{test_server,fail,1}}]) -> 60;
fit_args(l_call_ext, [{e,{random,uniform,1}}]) -> 59;
fit_args(l_call_ext, [{e,{mnesia_monitor,use_dir,0}}]) -> 68;
fit_args(l_call_ext, [{e,{gb_trees,insert,3}}]) -> 67;
fit_args(l_call_ext, [{e,{lists,splitwith,2}}]) -> 66;
fit_args(l_call_ext, [{e,{mnesia_schema,list2cs,1}}]) -> 73;
fit_args(l_call_ext, [{e,{lists,dropwhile,2}}]) -> 72;
fit_args(l_call_ext, [{e,{gb_trees,from_orddict,1}}]) -> 71;
fit_args(l_call_ext, [{e,{gb_trees,to_list,1}}]) -> 70;
fit_args(l_call_ext, [{e,{unicode,characters_to_list,1}}]) -> 69;
fit_args(l_call_ext, [{e,{os,type,0}}]) -> 76;
fit_args(l_call_ext, [{e,{asn1ct_gen,type,1}}]) -> 75;
fit_args(l_call_ext, [{e,{erl_syntax,atom_value,1}}]) -> 74;
fit_args(l_call_ext, [{e,{sets,is_element,2}}]) -> 77;
fit_args(l_call_ext, [{e,{erl_syntax,get_pos,1}}]) -> 80;
fit_args(l_call_ext, [{e,{cerl,var_name,1}}]) -> 79;
fit_args(l_call_ext, [{e,{asn1ct_gen,list2name,1}}]) -> 78;
fit_args(l_call_ext, [{e,{file,get_cwd,0}}]) -> 84;
fit_args(l_call_ext, [{e,{asn1ct,use_legacy_types,0}}]) -> 83;
fit_args(l_call_ext, [{e,{gb_sets,empty,0}}]) -> 82;
fit_args(l_call_ext, [{e,{file,write_file,2}}]) -> 81;
fit_args(l_call_ext, [{e,{ordsets,intersection,2}}]) -> 92;
fit_args(l_call_ext, [{e,{asn1ct_name,start,0}}]) -> 91;
fit_args(l_call_ext, [{e,{lists,sublist,3}}]) -> 90;
fit_args(l_call_ext, [{e,{beam_utils,code_at,2}}]) -> 89;
fit_args(l_call_ext, [{e,{ordsets,is_element,2}}]) -> 88;
fit_args(l_call_ext, [{e,{xref_utils,xset,2}}]) -> 87;
fit_args(l_call_ext, [{e,{dict,fetch,2}}]) -> 86;
fit_args(l_call_ext, [{e,{mnesia_lib,intersect,2}}]) -> 85;
fit_args(l_call_ext, [{e,{gb_sets,is_member,2}}]) -> 95;
fit_args(l_call_ext, [{e,{cerl,c_tuple,1}}]) -> 94;
fit_args(l_call_ext, [{e,{lists,seq,2}}]) -> 93;
fit_args(l_call_ext, [{e,{ets,tab2list,1}}]) -> 101;
fit_args(l_call_ext, [{e,{orddict,find,2}}]) -> 100;
fit_args(l_call_ext, [{e,{proplists,get_bool,2}}]) -> 99;
fit_args(l_call_ext, [{e,{mnesia,abort,1}}]) -> 98;
fit_args(l_call_ext, [{e,{rpc,call,4}}]) -> 97;
fit_args(l_call_ext, [{e,{sofs,difference,2}}]) -> 96;
fit_args(l_call_ext, [{e,_}]) -> 102;
fit_args(l_call_ext_last, [{e,_},1]) -> 0;
fit_args(l_call_ext_last, [{e,_},2]) -> 1;
fit_args(l_call_ext_last, [{e,_},0]) -> 2;
fit_args(l_call_ext_last, [{e,_},3]) -> 3;
fit_args(l_call_ext_last, [{e,_},4]) -> 4;
fit_args(l_call_ext_last, [{e,_},5]) -> 5;
fit_args(l_call_ext_last, [{e,_},_]) -> 6;
fit_args(l_call_ext_only, [{e,{gen_server,call,3}}]) -> 0;
fit_args(l_call_ext_only, [{e,{ssh_bits,encode,2}}]) -> 1;
fit_args(l_call_ext_only, [{e,{asn1ct_gen,emit,1}}]) -> 2;
fit_args(l_call_ext_only, [{e,{mnesia_monitor,get_env,1}}]) -> 3;
fit_args(l_call_ext_only, [{e,_}]) -> 4;
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
fit_args(l_catch, [{y,19},_]) -> 7;
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
fit_args(l_fetch, [{y,0},{x,2}]) -> 6;
fit_args(l_fetch, [{i,Arg0},{x,_}]) when Arg0 >= -128, Arg0 =< 127 -> 5;
fit_args(l_fetch, [{i,Arg0},{y,Arg1}]) when Arg0 >= -128, Arg0 =< 127, Arg1 >= 0, Arg1 =< 255 -> 10;
fit_args(l_fetch, [{x,0},_]) -> 0;
fit_args(l_fetch, [{x,1},_]) -> 11;
fit_args(l_fetch, [{x,4},_]) -> 12;
fit_args(l_fetch, [{x,3},_]) -> 13;
fit_args(l_fetch, [{x,2},_]) -> 15;
fit_args(l_fetch, [{x,5},_]) -> 16;
fit_args(l_fetch, [{y,0},_]) -> 19;
fit_args(l_fetch, [{x,_},{i,Arg1}]) when Arg1 >= -128, Arg1 =< 127 -> 3;
fit_args(l_fetch, [{y,Arg0},{i,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= -128, Arg1 =< 127 -> 9;
fit_args(l_fetch, [{x,_},{x,_}]) -> 2;
fit_args(l_fetch, [{x,_},{y,Arg1}]) when Arg1 >= 0, Arg1 =< 255 -> 4;
fit_args(l_fetch, [{y,Arg0},{y,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 7;
fit_args(l_fetch, [_,{x,0}]) -> 1;
fit_args(l_fetch, [_,{x,1}]) -> 8;
fit_args(l_fetch, [_,{x,4}]) -> 14;
fit_args(l_fetch, [_,{x,3}]) -> 17;
fit_args(l_fetch, [_,{x,2}]) -> 18;
fit_args(l_fetch, [_,{y,5}]) -> 20;
fit_args(l_fetch, [_,{x,5}]) -> 21;
fit_args(l_fetch, [_,{y,0}]) -> 22;
fit_args(l_fetch, [_,_]) -> 23;
fit_args(l_fmul, [{fr,_},{fr,_},{fr,_}]) -> 0;
fit_args(l_fnegate, [{fr,_},{fr,_}]) -> 0;
fit_args(l_fsub, [{fr,_},{fr,_},{fr,_}]) -> 0;
fit_args(l_gc_bif1, [{f,_},{b,{erlang,length,1}},{x,_},Arg3,{y,Arg4}]) when Arg3 >= 0, Arg3 =< 255, Arg4 >= 0, Arg4 =< 255 -> 6;
fit_args(l_gc_bif1, [{f,_},{b,_},{x,0},1,{x,0}]) -> 2;
fit_args(l_gc_bif1, [{f,_},{b,_},{x,0},2,{x,0}]) -> 8;
fit_args(l_gc_bif1, [{f,_},{b,_},{x,0},Arg3,{x,_}]) when Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_gc_bif1, [{f,_},{b,_},{x,0},Arg3,{y,Arg4}]) when Arg3 >= 0, Arg3 =< 255, Arg4 >= 0, Arg4 =< 255 -> 7;
fit_args(l_gc_bif1, [{f,_},{b,_},{x,_},Arg3,{x,0}]) when Arg3 >= 0, Arg3 =< 255 -> 1;
fit_args(l_gc_bif1, [{f,_},{b,_},{y,Arg2},Arg3,{x,0}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 5;
fit_args(l_gc_bif1, [{f,_},{b,_},{x,_},Arg3,{x,_}]) when Arg3 >= 0, Arg3 =< 255 -> 3;
fit_args(l_gc_bif1, [{f,_},{b,_},{y,Arg2},Arg3,{x,_}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 4;
fit_args(l_gc_bif1, [{f,_},{b,_},_,Arg3,_]) when Arg3 >= 0, Arg3 =< 255 -> 9;
fit_args(l_gc_bif2, [{f,_},{b,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_gc_bif3, [{f,_},{b,_},_,Arg3,_]) when Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_get, [{a,asn1_module},{y,0}]) -> 0;
fit_args(l_get, [{a,asn1_module},{y,Arg1}]) when Arg1 >= 0, Arg1 =< 255 -> 4;
fit_args(l_get, [{a,mnesia_activity_state},_]) -> 5;
fit_args(l_get, [_,{x,0}]) -> 1;
fit_args(l_get, [_,{x,1}]) -> 2;
fit_args(l_get, [_,{x,2}]) -> 3;
fit_args(l_get, [_,_]) -> 6;
fit_args(l_hibernate, []) -> 0;
fit_args(l_increment, [{x,0},Arg1,Arg2,{x,_}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 4;
fit_args(l_increment, [{x,0},_,Arg2,{y,Arg3}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 7;
fit_args(l_increment, [{y,Arg0},Arg1,Arg2,{x,_}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_increment, [{y,Arg0},Arg1,Arg2,{y,Arg3}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 6;
fit_args(l_increment, [{x,_},Arg1,Arg2,_]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_increment, [{x,_},_,Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 5;
fit_args(l_increment, [_,4294967295,Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_increment, [_,_,Arg2,{x,0}]) when Arg2 >= 0, Arg2 =< 255 -> 3;
fit_args(l_increment, [_,_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 8;
fit_args(l_int_bnot, [{f,_},_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_int_div, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_int_div, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_int_div, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_is_eq, [{f,_}]) -> 0;
fit_args(l_is_eq_exact, [{f,_}]) -> 0;
fit_args(l_is_eq_exact_immed, [{f,_},{x,255},{a,xmerl_scanner}]) -> 10;
fit_args(l_is_eq_exact_immed, [{f,_},{y,10},{a,ber}]) -> 22;
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
fit_args(l_is_eq_exact_immed, [{f,_},{x,14},_]) -> 21;
fit_args(l_is_eq_exact_immed, [{f,_},{y,2},_]) -> 23;
fit_args(l_is_eq_exact_immed, [{f,_},{x,15},_]) -> 24;
fit_args(l_is_eq_exact_immed, [{f,_},{x,16},_]) -> 25;
fit_args(l_is_eq_exact_immed, [{f,_},{y,4},_]) -> 26;
fit_args(l_is_eq_exact_immed, [{f,_},{x,17},_]) -> 27;
fit_args(l_is_eq_exact_immed, [{f,_},{y,5},_]) -> 29;
fit_args(l_is_eq_exact_immed, [{f,_},{x,18},_]) -> 28;
fit_args(l_is_eq_exact_immed, [{f,_},{x,19},_]) -> 30;
fit_args(l_is_eq_exact_immed, [{f,_},{x,22},_]) -> 31;
fit_args(l_is_eq_exact_immed, [{f,_},{x,20},_]) -> 32;
fit_args(l_is_eq_exact_immed, [{f,_},{y,7},_]) -> 34;
fit_args(l_is_eq_exact_immed, [{f,_},{x,23},_]) -> 33;
fit_args(l_is_eq_exact_immed, [{f,_},{y,6},_]) -> 35;
fit_args(l_is_eq_exact_immed, [{f,_},{x,_},{i,Arg2}]) when Arg2 >= -128, Arg2 =< 127 -> 3;
fit_args(l_is_eq_exact_immed, [{f,_},{y,Arg1},{a,asn1_NOVALUE}]) when Arg1 >= 0, Arg1 =< 255 -> 15;
fit_args(l_is_eq_exact_immed, [{f,_},_,_]) -> 36;
fit_args(l_is_eq_exact_literal, [{f,_},{x,0},_]) -> 0;
fit_args(l_is_eq_exact_literal, [{f,_},{x,1},_]) -> 1;
fit_args(l_is_eq_exact_literal, [{f,_},{x,2},_]) -> 2;
fit_args(l_is_eq_exact_literal, [{f,_},{x,4},_]) -> 3;
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
fit_args(l_is_ne_exact_immed, [{f,_},{y,2},_]) -> 8;
fit_args(l_is_ne_exact_immed, [{f,_},{y,1},_]) -> 9;
fit_args(l_is_ne_exact_immed, [{f,_},{x,5},_]) -> 10;
fit_args(l_is_ne_exact_immed, [{f,_},{x,_},{i,Arg2}]) when Arg2 >= -128, Arg2 =< 127 -> 7;
fit_args(l_is_ne_exact_immed, [{f,_},_,{a,true}]) -> 3;
fit_args(l_is_ne_exact_immed, [{f,_},_,_]) -> 11;
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
fit_args(l_move_call, [{y,2},{f,_}]) -> 2;
fit_args(l_move_call, [{y,0},{f,_}]) -> 3;
fit_args(l_move_call, [{y,1},{f,_}]) -> 4;
fit_args(l_move_call, [{y,3},{f,_}]) -> 5;
fit_args(l_move_call, [{x,3},{f,_}]) -> 6;
fit_args(l_move_call, [{y,5},{f,_}]) -> 7;
fit_args(l_move_call, [{y,4},{f,_}]) -> 8;
fit_args(l_move_call, [{y,6},{f,_}]) -> 10;
fit_args(l_move_call, [{x,5},{f,_}]) -> 11;
fit_args(l_move_call, [nil,{f,_}]) -> 12;
fit_args(l_move_call, [{x,4},{f,_}]) -> 13;
fit_args(l_move_call, [{y,7},{f,_}]) -> 15;
fit_args(l_move_call, [{a,foo},{f,_}]) -> 17;
fit_args(l_move_call, [{y,12},{f,_}]) -> 18;
fit_args(l_move_call, [{a,endDocument},{f,_}]) -> 19;
fit_args(l_move_call, [{a,ets},{f,_}]) -> 23;
fit_args(l_move_call, [{a,schema},{f,_}]) -> 22;
fit_args(l_move_call, [{x,6},{f,_}]) -> 21;
fit_args(l_move_call, [{a,false},{f,_}]) -> 24;
fit_args(l_move_call, [{a,x},{f,_}]) -> 28;
fit_args(l_move_call, [{y,8},{f,_}]) -> 29;
fit_args(l_move_call, [{a,true},{f,_}]) -> 33;
fit_args(l_move_call, [{smallint,1},{f,_}]) -> 9;
fit_args(l_move_call, [{smallint,0},{f,_}]) -> 14;
fit_args(l_move_call, [{smallint,2},{f,_}]) -> 16;
fit_args(l_move_call, [{smallint,3},{f,_}]) -> 20;
fit_args(l_move_call, [{smallint,4},{f,_}]) -> 25;
fit_args(l_move_call, [{smallint,100},{f,_}]) -> 26;
fit_args(l_move_call, [{smallint,9},{f,_}]) -> 27;
fit_args(l_move_call, [{smallint,1000},{f,_}]) -> 30;
fit_args(l_move_call, [{smallint,42},{f,_}]) -> 31;
fit_args(l_move_call, [{smallint,12},{f,_}]) -> 32;
fit_args(l_move_call, [_,{f,_}]) -> 34;
fit_args(l_move_call_ext, [{a,funky},{e,{estone_SUITE,req,2}}]) -> 26;
fit_args(l_move_call_ext, [{a,auto_repair},{e,{mnesia_monitor,get_env,1}}]) -> 32;
fit_args(l_move_call_ext, [{y,0},{e,_}]) -> 1;
fit_args(l_move_call_ext, [{y,1},{e,_}]) -> 2;
fit_args(l_move_call_ext, [{y,2},{e,_}]) -> 3;
fit_args(l_move_call_ext, [{y,4},{e,_}]) -> 4;
fit_args(l_move_call_ext, [{y,3},{e,_}]) -> 5;
fit_args(l_move_call_ext, [{x,2},{e,_}]) -> 7;
fit_args(l_move_call_ext, [{x,1},{e,_}]) -> 8;
fit_args(l_move_call_ext, [{y,6},{e,_}]) -> 10;
fit_args(l_move_call_ext, [{y,5},{e,_}]) -> 14;
fit_args(l_move_call_ext, [{x,3},{e,_}]) -> 17;
fit_args(l_move_call_ext, [nil,{e,_}]) -> 18;
fit_args(l_move_call_ext, [{y,7},{e,_}]) -> 22;
fit_args(l_move_call_ext, [{x,4},{e,_}]) -> 33;
fit_args(l_move_call_ext, [{a,schema},{e,_}]) -> 37;
fit_args(l_move_call_ext, [{a,func},{e,_}]) -> 39;
fit_args(l_move_call_ext, [{smallint,0},{e,{lists,seq,2}}]) -> 13;
fit_args(l_move_call_ext, [{smallint,1000},{e,{test_server,sleep,1}}]) -> 43;
fit_args(l_move_call_ext, [{smallint,1},{e,_}]) -> 6;
fit_args(l_move_call_ext, [{smallint,2},{e,_}]) -> 38;
fit_args(l_move_call_ext, [_,{e,{io,format,2}}]) -> 0;
fit_args(l_move_call_ext, [_,{e,{asn1ct_gen,emit,1}}]) -> 9;
fit_args(l_move_call_ext, [_,{e,{io_lib,format,2}}]) -> 11;
fit_args(l_move_call_ext, [_,{e,{erlang,put,2}}]) -> 12;
fit_args(l_move_call_ext, [_,{e,{prettypr,text,1}}]) -> 15;
fit_args(l_move_call_ext, [_,{e,{proplists,get_value,3}}]) -> 16;
fit_args(l_move_call_ext, [_,{e,{asn1ct_name,new,1}}]) -> 19;
fit_args(l_move_call_ext, [_,{e,{test_server,seconds,1}}]) -> 20;
fit_args(l_move_call_ext, [_,{e,{proplists,get_value,2}}]) -> 21;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,verbose,2}}]) -> 23;
fit_args(l_move_call_ext, [_,{e,{lists,duplicate,2}}]) -> 24;
fit_args(l_move_call_ext, [_,{e,{erlang,system_info,1}}]) -> 25;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,dbg_out,2}}]) -> 27;
fit_args(l_move_call_ext, [_,{e,{erlang,erase,1}}]) -> 28;
fit_args(l_move_call_ext, [_,{e,{lists,seq,2}}]) -> 29;
fit_args(l_move_call_ext, [_,{e,{test_server,lookup_config,2}}]) -> 30;
fit_args(l_move_call_ext, [_,{e,{erlang,binary_to_term,1}}]) -> 31;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,set,2}}]) -> 34;
fit_args(l_move_call_ext, [_,{e,{lists,sublist,3}}]) -> 35;
fit_args(l_move_call_ext, [_,{e,{re,replace,4}}]) -> 36;
fit_args(l_move_call_ext, [_,{e,{re,split,3}}]) -> 42;
fit_args(l_move_call_ext, [_,{e,{io,format,1}}]) -> 41;
fit_args(l_move_call_ext, [_,{e,{io,fwrite,2}}]) -> 40;
fit_args(l_move_call_ext, [_,{e,_}]) -> 44;
fit_args(l_move_call_ext_last, [{e,_},0,_]) -> 1;
fit_args(l_move_call_ext_last, [{e,_},1,_]) -> 2;
fit_args(l_move_call_ext_last, [{e,_},2,_]) -> 3;
fit_args(l_move_call_ext_last, [{e,_},3,_]) -> 4;
fit_args(l_move_call_ext_last, [{e,_},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_move_call_ext_last, [{e,_},_,_]) -> 5;
fit_args(l_move_call_ext_only, [{e,{erlang,get_module_info,1}},_]) -> 0;
fit_args(l_move_call_ext_only, [{e,{io_lib,format,2}},_]) -> 1;
fit_args(l_move_call_ext_only, [{e,{lists,reverse,1}},_]) -> 2;
fit_args(l_move_call_ext_only, [{e,{io,format,2}},_]) -> 5;
fit_args(l_move_call_ext_only, [{e,_},{x,2}]) -> 3;
fit_args(l_move_call_ext_only, [{e,_},{x,1}]) -> 4;
fit_args(l_move_call_ext_only, [{e,_},{x,3}]) -> 6;
fit_args(l_move_call_ext_only, [{e,_},_]) -> 7;
fit_args(l_move_call_last, [{f,_},1,_]) -> 1;
fit_args(l_move_call_last, [{f,_},2,_]) -> 2;
fit_args(l_move_call_last, [{f,_},0,_]) -> 3;
fit_args(l_move_call_last, [{f,_},3,_]) -> 4;
fit_args(l_move_call_last, [{f,_},5,_]) -> 6;
fit_args(l_move_call_last, [{f,_},4,_]) -> 7;
fit_args(l_move_call_last, [{f,_},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_move_call_last, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 5;
fit_args(l_move_call_last, [{f,_},_,_]) -> 8;
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
fit_args(l_new_bs_put_float_imm, [{f,_},32,0,_]) -> 1;
fit_args(l_new_bs_put_float_imm, [{f,_},_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 2;
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
fit_args(l_put_tuple, [{x,0},6]) -> 5;
fit_args(l_put_tuple, [{x,0},7]) -> 7;
fit_args(l_put_tuple, [{x,0},_]) -> 8;
fit_args(l_put_tuple, [{x,_},Arg1]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_put_tuple, [{y,Arg0},Arg1]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 6;
fit_args(l_put_tuple, [_,_]) -> 9;
fit_args(l_recv_set, []) -> 0;
fit_args(l_rem, [{f,_},Arg1,{x,0}]) when Arg1 >= 0, Arg1 =< 255 -> 1;
fit_args(l_rem, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 0;
fit_args(l_rem, [{f,_},Arg1,_]) when Arg1 >= 0, Arg1 =< 255 -> 2;
fit_args(l_select_tuple_arity, [{x,0},{f,_},6]) -> 0;
fit_args(l_select_tuple_arity, [{x,0},{f,_},_]) -> 2;
fit_args(l_select_tuple_arity, [{x,_},{f,_},Arg2]) when Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_select_tuple_arity, [_,{f,_},_]) -> 3;
fit_args(l_select_tuple_arity2, [{x,0},{f,_},Arg2,{f,_},Arg4,{f,_}]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 0;
fit_args(l_select_tuple_arity2, [{x,_},{f,_},Arg2,{f,_},Arg4,{f,_}]) when Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 1;
fit_args(l_select_tuple_arity2, [{y,Arg0},{f,_},Arg2,{f,_},Arg4,{f,_}]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255, Arg4 >= 0, Arg4 =< 255 -> 2;
fit_args(l_select_tuple_arity2, [_,{f,_},_,{f,_},_,{f,_}]) -> 3;
fit_args(l_select_val2, [{x,0},{f,_},{a,false},{f,_},{a,true},{f,_}]) -> 0;
fit_args(l_select_val2, [{x,0},{f,_},{a,true},{f,_},{a,false},{f,_}]) -> 1;
fit_args(l_select_val2, [{x,0},{f,_},_,{f,_},_,{f,_}]) -> 3;
fit_args(l_select_val2, [{x,1},{f,_},_,{f,_},_,{f,_}]) -> 4;
fit_args(l_select_val2, [{x,2},{f,_},_,{f,_},_,{f,_}]) -> 5;
fit_args(l_select_val2, [{x,3},{f,_},_,{f,_},_,{f,_}]) -> 7;
fit_args(l_select_val2, [{x,4},{f,_},_,{f,_},_,{f,_}]) -> 9;
fit_args(l_select_val2, [{x,5},{f,_},_,{f,_},_,{f,_}]) -> 10;
fit_args(l_select_val2, [{x,6},{f,_},_,{f,_},_,{f,_}]) -> 11;
fit_args(l_select_val2, [{x,7},{f,_},_,{f,_},_,{f,_}]) -> 12;
fit_args(l_select_val2, [{x,8},{f,_},_,{f,_},_,{f,_}]) -> 13;
fit_args(l_select_val2, [{y,3},{f,_},_,{f,_},_,{f,_}]) -> 14;
fit_args(l_select_val2, [{y,1},{f,_},_,{f,_},_,{f,_}]) -> 15;
fit_args(l_select_val2, [{x,9},{f,_},_,{f,_},_,{f,_}]) -> 16;
fit_args(l_select_val2, [{x,_},{f,_},{i,Arg2},{f,_},{i,Arg4},{f,_}]) when Arg2 >= -128, Arg2 =< 127, Arg4 >= -128, Arg4 =< 127 -> 2;
fit_args(l_select_val2, [_,{f,_},{a,true},{f,_},{a,false},{f,_}]) -> 6;
fit_args(l_select_val2, [_,{f,_},{a,false},{f,_},{a,true},{f,_}]) -> 8;
fit_args(l_select_val2, [_,{f,_},_,{f,_},_,{f,_}]) -> 17;
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
fit_args(move, [{x,0},_]) -> 2;
fit_args(move, [{x,_},{y,Arg1}]) when Arg1 >= 0, Arg1 =< 255 -> 3;
fit_args(move, [{y,Arg0},{y,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 7;
fit_args(move, [_,{x,1}]) -> 0;
fit_args(move, [_,{x,0}]) -> 1;
fit_args(move, [_,{x,2}]) -> 4;
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
fit_args(move2, [{x,_},{x,0},{x,_},{x,_}]) -> 5;
fit_args(move2, [{x,_},{y,Arg1},{x,0},{y,Arg3}]) when Arg1 >= 0, Arg1 =< 255, Arg3 >= 0, Arg3 =< 255 -> 2;
fit_args(move2, [{x,_},{x,_},{x,0},{x,_}]) -> 6;
fit_args(move2, [{x,_},{y,Arg1},{x,_},{y,Arg3}]) when Arg1 >= 0, Arg1 =< 255, Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(move2, [{y,Arg0},{x,_},{y,Arg2},_]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(move2, [{x,_},{x,_},{x,_},_]) -> 3;
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
fit_args(move_deallocate_return, [_,7]) -> 9;
fit_args(move_deallocate_return, [_,_]) -> 10;
fit_args(move_jump, [{f,_},{x,1}]) -> 0;
fit_args(move_jump, [{f,_},{x,2}]) -> 1;
fit_args(move_jump, [{f,_},nil]) -> 2;
fit_args(move_jump, [{f,_},{x,4}]) -> 3;
fit_args(move_jump, [{f,_},{y,0}]) -> 4;
fit_args(move_jump, [{f,_},{y,1}]) -> 5;
fit_args(move_jump, [{f,_},{y,2}]) -> 6;
fit_args(move_jump, [{f,_},{x,3}]) -> 7;
fit_args(move_jump, [{f,_},{a,false}]) -> 8;
fit_args(move_jump, [{f,_},{a,asn1_NOVALUE}]) -> 9;
fit_args(move_jump, [{f,_},{a,true}]) -> 10;
fit_args(move_jump, [{f,_},{y,3}]) -> 12;
fit_args(move_jump, [{f,_},{smallint,0}]) -> 11;
fit_args(move_jump, [{f,_},_]) -> 13;
fit_args(move_return, [{a,true}]) -> 0;
fit_args(move_return, [{x,1}]) -> 1;
fit_args(move_return, [{x,2}]) -> 2;
fit_args(move_return, [{a,ok}]) -> 3;
fit_args(move_return, [nil]) -> 4;
fit_args(move_return, [{a,false}]) -> 5;
fit_args(move_return, [{x,3}]) -> 6;
fit_args(move_return, [{x,4}]) -> 7;
fit_args(move_return, [{a,error}]) -> 14;
fit_args(move_return, [{a,no}]) -> 13;
fit_args(move_return, [{a,undefined}]) -> 15;
fit_args(move_return, [{a,none}]) -> 16;
fit_args(move_return, [{x,5}]) -> 18;
fit_args(move_return, [{a,ignore}]) -> 19;
fit_args(move_return, [{x,6}]) -> 24;
fit_args(move_return, [{a,nomatch}]) -> 23;
fit_args(move_return, [{smallint,1}]) -> 8;
fit_args(move_return, [{smallint,2}]) -> 9;
fit_args(move_return, [{smallint,0}]) -> 10;
fit_args(move_return, [{smallint,3}]) -> 11;
fit_args(move_return, [{smallint,4}]) -> 12;
fit_args(move_return, [{smallint,8}]) -> 17;
fit_args(move_return, [{smallint,16}]) -> 20;
fit_args(move_return, [{smallint,64}]) -> 21;
fit_args(move_return, [{smallint,5}]) -> 22;
fit_args(move_return, [{smallint,6}]) -> 27;
fit_args(move_return, [{smallint,128}]) -> 26;
fit_args(move_return, [{smallint,7}]) -> 25;
fit_args(move_return, [{smallint,32}]) -> 28;
fit_args(move_return, [_]) -> 29;
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
fit_args(try_end, [{y,5}]) -> 2;
fit_args(try_end, [{y,3}]) -> 3;
fit_args(try_end, [{y,2}]) -> 4;
fit_args(try_end, [{y,4}]) -> 5;
fit_args(try_end, [{y,6}]) -> 6;
fit_args(try_end, [_]) -> 7;
fit_args(update_map_assoc, [_,_,_]) -> 0;
fit_args(update_map_exact, [{f,_},_,_,_]) -> 0;
fit_args(wait, [{f,_}]) -> 0;
fit_args(wait_timeout, [{f,_},_]) -> 0;
fit_args(Op, As) -> erlang:error({nofit,Op,As}).

var_args(move, 0) -> [t,{x,1}];
var_args(move, 1) -> [t,{x,0}];
var_args(test_heap, 0) -> [u8,u8];
var_args(l_call, 0) -> [f];
var_args(move, 2) -> [{x,0},t];
var_args(move, 3) -> [x8,y8];
var_args(l_put_tuple, 0) -> [x8,u8];
var_args(get_tuple_element, 0) -> [{x,0},u8,x8];
var_args(move2, 1) -> [y8,x8,y8,t];
var_args(badmatch, 0) -> [{x,0}];
var_args(put_list, 0) -> [t,t,{x,0}];
var_args(is_tuple_of_arity, 1) -> [f,x8,u8];
var_args(get_tuple_element, 1) -> [x8,u8,x8];
var_args(move2, 0) -> [x8,y8,x8,y8];
var_args(l_call_only, 0) -> [f];
var_args(move, 4) -> [t,{x,2}];
var_args(l_is_eq_exact_immed, 0) -> [f,{x,0},t];
var_args(l_is_eq_exact_immed, 1) -> [f,{x,1},t];
var_args(is_tuple_of_arity, 0) -> [f,{x,0},{u,2}];
var_args(put_list, 1) -> [t,t,{x,1}];
var_args(get_list, 0) -> [x8,x8,x8];
var_args(l_put_tuple, 1) -> [{x,0},{u,2}];
var_args(l_is_ge, 0) -> [f];
var_args(l_call_ext, 102) -> [e];
var_args(l_make_fun, 0) -> [fu];
var_args(return, 0) -> [];
var_args(is_tuple_of_arity, 2) -> [f,{x,0},u32];
var_args(l_move_call_last, 0) -> [f,u8,y8];
var_args(extract_next_element2, 0) -> [{x,1}];
var_args(l_fetch, 0) -> [{x,0},t];
var_args(call_bif, 3) -> [{b,{erlang,error,1}}];
var_args(move_deallocate_return, 0) -> [x8,u8];
var_args(l_trim, 0) -> [{u,1}];
var_args(l_allocate, 0) -> [{u,1}];
var_args(move2, 3) -> [x8,x8,x8,t];
var_args(put_list, 2) -> [t,t,{x,2}];
var_args(is_nil, 0) -> [f,{x,0}];
var_args(l_fetch, 1) -> [t,{x,0}];
var_args(is_nonempty_list, 0) -> [f,{x,0}];
var_args(move_return, 29) -> [t];
var_args(l_allocate, 1) -> [{u,0}];
var_args(extract_next_element, 0) -> [{x,1}];
var_args(deallocate_return, 0) -> [{u,1}];
var_args(case_end, 0) -> [{x,0}];
var_args(l_is_eq_exact_immed, 2) -> [f,{x,3},t];
var_args(jump, 0) -> [f];
var_args(get_list, 1) -> [{x,0},x8,x8];
var_args(call_bif, 7) -> [{b,{erlang,setelement,3}}];
var_args(move2, 2) -> [x8,y8,{x,0},y8];
var_args(get_tuple_element, 2) -> [t,{u,0},{x,0}];
var_args(l_is_eq_exact, 0) -> [f];
var_args(l_move_call, 34) -> [t,f];
var_args(l_allocate, 2) -> [{u,2}];
var_args(move_return, 0) -> [{a,true}];
var_args(l_is_eq_exact_immed, 3) -> [f,x8,i8];
var_args(get_tuple_element, 3) -> [{x,0},u8,y8];
var_args(l_fetch, 2) -> [x8,x8];
var_args(move, 5) -> [t,{x,3}];
var_args(init2, 0) -> [y8,y8];
var_args(init, 0) -> [{y,1}];
var_args(l_is_eq_exact_immed, 4) -> [f,{x,2},t];
var_args(deallocate_return, 1) -> [{u,0}];
var_args(call_bif, 8) -> [{b,{erlang,'++',2}}];
var_args(put_list, 3) -> [t,t,{x,3}];
var_args(extract_next_element, 1) -> [{x,3}];
var_args(move2, 4) -> [{x,0},y8,x8,y8];
var_args(is_tuple_of_arity, 3) -> [f,y8,u8];
var_args(l_fetch, 3) -> [x8,i8];
var_args(get_tuple_element, 4) -> [y8,u8,x8];
var_args(l_allocate, 3) -> [{u,3}];
var_args(init, 1) -> [{y,0}];
var_args(move2, 5) -> [x8,{x,0},x8,x8];
var_args(deallocate_return, 2) -> [{u,2}];
var_args(l_put_tuple, 2) -> [{x,0},{u,3}];
var_args(l_fetch, 4) -> [x8,y8];
var_args(l_move_call_ext, 44) -> [t,e];
var_args(is_nonempty_list, 1) -> [f,{x,2}];
var_args(l_is_eq_exact_immed, 5) -> [f,{x,4},t];
var_args(l_trim, 1) -> [{u,2}];
var_args(init3, 0) -> [y8,y8,y8];
var_args(l_allocate_zero, 0) -> [{u,2}];
var_args(allocate_init, 0) -> [u32,{y,0}];
var_args(call_bif, 6) -> [{b,{erlang,throw,1}}];
var_args(test_heap_1_put_list, 0) -> [{u,2},{y,0}];
var_args(allocate_heap, 0) -> [u8,u8,u8];
var_args(l_allocate_zero, 1) -> [{u,1}];
var_args(init, 2) -> [{y,2}];
var_args(get_list, 2) -> [x8,{x,0},x8];
var_args(move_deallocate_return, 1) -> [{a,ok},u8];
var_args(call_bif, 5) -> [{b,{erlang,exit,1}}];
var_args(l_move_call, 0) -> [{x,2},f];
var_args(call_bif, 48) -> [b];
var_args(l_call_last, 0) -> [f,{u,2}];
var_args(move_return, 1) -> [{x,1}];
var_args(l_is_eq_exact_literal, 0) -> [f,{x,0},t];
var_args(is_nonempty_list, 2) -> [f,{x,1}];
var_args(is_tuple, 0) -> [f,{x,0}];
var_args(is_nonempty_list_allocate, 0) -> [f,{x,0},u32];
var_args(deallocate_return, 3) -> [{u,3}];
var_args(l_is_eq_exact_immed, 6) -> [f,{x,5},t];
var_args(l_call_last, 1) -> [f,{u,0}];
var_args(is_list, 0) -> [f,{x,0}];
var_args(l_move_call_only, 0) -> [f,{x,1}];
var_args(l_select_val_atoms, 0) -> [x8,f,u8];
var_args(extract_next_element, 2) -> [{x,2}];
var_args(get_list, 3) -> [{x,0},{x,0},t];
var_args(l_trim, 2) -> [{u,3}];
var_args(l_call_last, 2) -> [f,{u,3}];
var_args(l_increment, 0) -> [x8,u8,u8,t];
var_args(move, 6) -> [t,{x,4}];
var_args(move_deallocate_return, 2) -> [y8,u8];
var_args(l_is_lt, 0) -> [f];
var_args(is_nonempty_list, 3) -> [f,{x,3}];
var_args(l_select_val2, 3) -> [{x,0},f,t,f,t,f];
var_args(l_select_val2, 0) -> [{x,0},f,{a,false},f,{a,true},f];
var_args(l_select_val2, 4) -> [{x,1},f,t,f,t,f];
var_args(init, 3) -> [{y,3}];
var_args(is_nil, 1) -> [f,{x,2}];
var_args(l_move_call_only, 1) -> [f,{x,2}];
var_args(extract_next_element2, 1) -> [{x,3}];
var_args(l_bs_start_match2, 0) -> [t,f,u8,u8,x8];
var_args(l_move_call_only, 9) -> [f,t];
var_args(extract_next_element3, 0) -> [{x,1}];
var_args(remove_message, 0) -> [];
var_args(get_tuple_element, 5) -> [x8,u8,y8];
var_args(l_fetch, 5) -> [i8,x8];
var_args(l_new_bs_put_integer_imm, 0) -> [f,u32,{u8,0},t];
var_args(l_catch, 0) -> [{y,0},t];
var_args(l_allocate_zero, 2) -> [{u,3}];
var_args(move_return, 2) -> [{x,2}];
var_args(l_call_ext, 0) -> [{e,{lists,reverse,1}}];
var_args(l_allocate, 4) -> [{u,4}];
var_args(move2, 6) -> [x8,x8,{x,0},x8];
var_args(extract_next_element, 3) -> [{x,4}];
var_args(l_select_val2, 1) -> [{x,0},f,{a,true},f,{a,false},f];
var_args(test_arity, 0) -> [f,{x,0},{u,2}];
var_args(is_nil, 2) -> [f,{x,1}];
var_args(move_jump, 13) -> [f,t];
var_args(l_allocate_zero, 3) -> [{u,4}];
var_args(catch_end, 0) -> [{y,0}];
var_args(deallocate_return, 4) -> [{u,4}];
var_args(l_is_eq_exact_immed, 7) -> [f,{x,6},t];
var_args(move_return, 3) -> [{a,ok}];
var_args(move_return, 4) -> [nil];
var_args(l_move_call_ext, 0) -> [t,{e,{io,format,2}}];
var_args(put_list, 4) -> [x8,t,x8];
var_args(l_put_tuple, 3) -> [{x,0},{u,4}];
var_args(move, 7) -> [y8,y8];
var_args(move2, 7) -> [{x,0},x8,x8,{x,0}];
var_args(l_is_eq_exact_immed, 8) -> [f,{x,7},t];
var_args(call_bif, 2) -> [{b,{erlang,error,2}}];
var_args(l_select_tuple_arity2, 0) -> [{x,0},f,u8,f,u8,f];
var_args(is_nonempty_list, 4) -> [f,{x,7}];
var_args(l_plus, 0) -> [f,u8,x8];
var_args(l_call_last, 3) -> [f,{u,4}];
var_args(is_nonempty_list, 5) -> [f,{x,4}];
var_args(get_list, 4) -> [t,x8,y8];
var_args(init, 4) -> [{y,4}];
var_args(move_return, 5) -> [{a,false}];
var_args(l_move_call_only, 2) -> [f,{x,4}];
var_args(l_increment, 3) -> [t,u32,u8,{x,0}];
var_args(is_nonempty_list, 6) -> [f,{x,5}];
var_args(l_bs_get_binary_all_reuse, 0) -> [t,f,{u8,8}];
var_args(l_call_last, 4) -> [f,{u,1}];
var_args(bif1_body, 0) -> [{b,{erlang,hd,1}},t,{x,0}];
var_args(l_gc_bif1, 0) -> [f,b,{x,0},u8,x8];
var_args(l_call_fun, 0) -> [{u8,1}];
var_args(test_arity, 1) -> [f,x8,u8];
var_args(set_tuple_element, 0) -> [y8,{x,0},u8];
var_args(l_move_call, 1) -> [{x,1},f];
var_args(l_bs_match_string, 0) -> [x8,f,u8,str];
var_args(l_move_call_last, 1) -> [f,{u,1},t];
var_args(l_bs_test_zero_tail2, 0) -> [f,{x,2}];
var_args(call_bif, 9) -> [{b,{lists,member,2}}];
var_args(send, 0) -> [];
var_args(l_is_eq, 0) -> [f];
var_args(l_move_call_only, 3) -> [f,{x,3}];
var_args(l_catch, 1) -> [{y,1},t];
var_args(l_is_ne, 0) -> [f];
var_args(l_bs_add, 0) -> [f,{u8,1},t];
var_args(l_select_val2, 2) -> [x8,f,i8,f,i8,f];
var_args(l_move_call_ext, 1) -> [{y,0},e];
var_args(l_call_ext_only, 4) -> [e];
var_args(l_select_val_smallints, 0) -> [x8,f,u8];
var_args(bif2_body, 0) -> [b,{x,0}];
var_args(is_nonempty_list, 7) -> [f,{x,6}];
var_args(l_is_ne_exact_immed, 0) -> [f,{x,0},t];
var_args(l_is_eq_exact_immed, 9) -> [f,{x,8},t];
var_args(l_move_call_ext_last, 0) -> [e,u8,y8];
var_args(extract_next_element2, 2) -> [{x,4}];
var_args(case_end, 1) -> [{x,1}];
var_args(extract_next_element2, 3) -> [{x,2}];
var_args(extract_next_element, 4) -> [{x,5}];
var_args(l_move_call, 2) -> [{y,2},f];
var_args(l_select_val_atoms, 1) -> [{x,0},f,u32];
var_args(move_deallocate_return, 3) -> [t,{u,0}];
var_args(l_move_call_ext, 2) -> [{y,1},e];
var_args(l_loop_rec, 0) -> [f];
var_args(l_move_call, 3) -> [{y,0},f];
var_args(l_select_tuple_arity2, 1) -> [x8,f,u8,f,u8,f];
var_args(is_nil, 3) -> [f,{x,4}];
var_args(l_trim, 3) -> [{u,4}];
var_args(l_bs_get_utf16, 0) -> [x8,f,u8,x8];
var_args(l_bs_restore2, 0) -> [x8,u8];
var_args(init, 5) -> [{y,5}];
var_args(l_move_call, 4) -> [{y,1},f];
var_args(catch_end, 1) -> [{y,1}];
var_args(is_nonempty_list, 8) -> [f,{x,9}];
var_args(is_nonempty_list, 9) -> [f,{x,8}];
var_args(is_nil, 4) -> [f,{x,3}];
var_args(l_call_ext_last, 0) -> [e,{u,1}];
var_args(l_fetch, 7) -> [y8,y8];
var_args(deallocate_return, 5) -> [{u,5}];
var_args(l_bs_init_heap_bin, 0) -> [u8,u8,u8,t];
var_args(l_allocate_zero, 4) -> [{u,6}];
var_args(wait, 0) -> [f];
var_args(badmatch, 1) -> [{x,3}];
var_args(call_bif, 10) -> [{b,{ets,insert,2}}];
var_args(loop_rec_end, 0) -> [f];
var_args(l_allocate, 5) -> [{u,5}];
var_args(extract_next_element, 5) -> [{x,6}];
var_args(l_move_call_ext, 3) -> [{y,2},e];
var_args(move, 8) -> [t,{x,5}];
var_args(l_select_val2, 5) -> [{x,2},f,t,f,t,f];
var_args(l_allocate_zero, 5) -> [{u,5}];
var_args(l_bs_start_match2, 1) -> [{x,0},f,u8,u8,{x,0}];
var_args(l_move_call_ext, 4) -> [{y,4},e];
var_args(l_move_call_ext, 5) -> [{y,3},e];
var_args(badmatch, 2) -> [{x,2}];
var_args(l_call_last, 5) -> [f,{u,6}];
var_args(extract_next_element2, 4) -> [{x,5}];
var_args(l_increment, 1) -> [y8,u8,u8,x8];
var_args(l_call_fun_last, 0) -> [u8,u8];
var_args(l_plus, 1) -> [f,u8,{x,0}];
var_args(init, 6) -> [{y,6}];
var_args(l_move_call_ext, 6) -> [{smallint,1},e];
var_args(l_gc_bif1, 1) -> [f,b,x8,u8,{x,0}];
var_args(l_call_last, 6) -> [f,{u,5}];
var_args(l_call_ext, 1) -> [{e,{asn1ct_gen,emit,1}}];
var_args(l_trim, 4) -> [{u,5}];
var_args(bs_context_to_binary, 0) -> [{x,0}];
var_args(put_list, 5) -> [t,t,{x,4}];
var_args(l_bs_save2, 0) -> [x8,u8];
var_args(move_deallocate_return, 4) -> [t,{u,1}];
var_args(l_increment, 2) -> [t,{u,4294967295},u8,x8];
var_args(extract_next_element, 6) -> [{x,255}];
var_args(call_bif, 11) -> [{b,{erlang,get_module_info,2}}];
var_args(int_code_end, 0) -> [];
var_args(l_move_call_ext_only, 0) -> [{e,{erlang,get_module_info,1}},t];
var_args(call_bif, 12) -> [{b,{erlang,binary_to_list,1}}];
var_args(get_tuple_element, 6) -> [{x,0},{u,1},{x,0}];
var_args(l_is_ne_exact_immed, 1) -> [f,{x,1},t];
var_args(l_is_eq_exact_immed, 11) -> [f,{x,9},t];
var_args(call_bif, 14) -> [{b,{erlang,list_to_binary,1}}];
var_args(call_bif, 13) -> [{b,{ets,delete,1}}];
var_args(l_move_call, 5) -> [{y,3},f];
var_args(l_put_tuple, 4) -> [{x,0},{u,5}];
var_args(l_bs_test_zero_tail2, 1) -> [f,{x,0}];
var_args(self, 0) -> [{x,0}];
var_args(extract_next_element, 7) -> [{x,7}];
var_args(is_nil, 5) -> [f,{x,5}];
var_args(is_nonempty_list_allocate, 1) -> [f,x8,u8];
var_args(l_move_call, 6) -> [{x,3},f];
var_args(badmatch, 3) -> [{x,1}];
var_args(is_tuple, 1) -> [f,{x,1}];
var_args(l_select_val2, 7) -> [{x,3},f,t,f,t,f];
var_args(l_move_call_last, 2) -> [f,{u,2},t];
var_args(l_move_call_ext, 7) -> [{x,2},e];
var_args(l_bs_test_zero_tail2, 2) -> [f,{x,3}];
var_args(l_gc_bif1, 3) -> [f,b,x8,u8,x8];
var_args(is_atom, 0) -> [f,{x,0}];
var_args(is_nil, 6) -> [f,{x,6}];
var_args(is_nonempty_list, 10) -> [f,{x,10}];
var_args(l_call_ext_last, 1) -> [e,{u,2}];
var_args(l_select_val_smallints, 1) -> [{x,0},f,u32];
var_args(l_fast_element, 0) -> [{x,0},{u,2},{x,0}];
var_args(extract_next_element, 8) -> [{y,1}];
var_args(l_bs_match_string, 1) -> [{x,0},f,u32,str];
var_args(is_nonempty_list, 11) -> [f,{x,11}];
var_args(l_catch, 2) -> [{y,2},t];
var_args(l_gc_bif1, 4) -> [f,b,y8,u8,x8];
var_args(l_call_ext, 2) -> [{e,{lists,foreach,2}}];
var_args(l_move_call, 7) -> [{y,5},f];
var_args(l_bs_test_unit_8, 0) -> [f,{x,0}];
var_args(call_bif, 16) -> [{b,{lists,keysearch,3}}];
var_args(call_bif, 15) -> [{b,{ets,info,2}}];
var_args(raise, 0) -> [{x,2},{x,1}];
var_args(l_call_ext, 3) -> [{e,{file,close,1}}];
var_args(is_nil, 7) -> [f,{x,7}];
var_args(extract_next_element3, 1) -> [{x,3}];
var_args(move_return, 6) -> [{x,3}];
var_args(l_call_ext_last, 2) -> [e,{u,0}];
var_args(l_move_call_ext, 8) -> [{x,1},e];
var_args(call_bif, 17) -> [{b,{erlang,list_to_atom,1}}];
var_args(allocate_heap_zero, 0) -> [u8,u8,u8];
var_args(call_bif, 18) -> [{b,{erlang,integer_to_list,1}}];
var_args(l_allocate, 6) -> [{u,6}];
var_args(case_end, 2) -> [{x,2}];
var_args(deallocate_return, 6) -> [{u,6}];
var_args(l_is_eq_exact_literal, 7) -> [f,t,t];
var_args(self, 1) -> [{x,1}];
var_args(get_list, 7) -> [t,{x,0},t];
var_args(init, 7) -> [{y,7}];
var_args(l_minus, 0) -> [f,u8,x8];
var_args(l_move_call_ext_only, 1) -> [{e,{io_lib,format,2}},t];
var_args(call_bif, 19) -> [{b,{erlang,atom_to_list,1}}];
var_args(extract_next_element2, 5) -> [{x,6}];
var_args(l_call_fun, 1) -> [{u8,3}];
var_args(catch_end, 2) -> [{y,2}];
var_args(l_move_call_ext_only, 7) -> [e,t];
var_args(call_bif, 20) -> [{b,{ets,lookup,2}}];
var_args(extract_next_element, 9) -> [{y,0}];
var_args(l_move_call_last, 3) -> [f,{u,0},t];
var_args(l_call_ext, 4) -> [{e,{file,open,2}}];
var_args(try_end, 0) -> [{y,1}];
var_args(l_move_call_ext, 9) -> [t,{e,{asn1ct_gen,emit,1}}];
var_args(l_band, 0) -> [f,u8,x8];
var_args(l_trim, 5) -> [{u,6}];
var_args(l_bif2, 0) -> [f,{b,{erlang,element,2}},t];
var_args(l_move_call, 8) -> [{y,4},f];
var_args(is_nil, 8) -> [f,{x,8}];
var_args(l_fetch, 6) -> [{y,0},{x,2}];
var_args(l_call_ext, 5) -> [{e,{lists,foldl,3}}];
var_args(test_heap_1_put_list, 1) -> [{u,2},t];
var_args(l_is_eq_exact_immed, 12) -> [f,{x,10},t];
var_args(l_call_ext, 6) -> [{e,{lists,map,2}}];
var_args(try_end, 1) -> [{y,0}];
var_args(l_gc_bif1, 2) -> [f,b,{x,0},{u8,1},{x,0}];
var_args(l_bs_get_binary2, 0) -> [f,t,u8,x8,u8,{u8,0},x8];
var_args(l_minus, 1) -> [f,u8,{x,0}];
var_args(l_bs_get_binary_all2, 0) -> [f,x8,u8,u8,x8];
var_args(l_is_eq_exact_immed, 13) -> [f,{x,11},t];
var_args(is_integer, 0) -> [f,{x,0}];
var_args(get_tuple_element, 7) -> [x8,u8,{x,0}];
var_args(extract_next_element2, 6) -> [{x,8}];
var_args(l_move_call_only, 4) -> [f,{x,5}];
var_args(l_is_eq_exact_literal, 1) -> [f,{x,1},t];
var_args(l_catch, 3) -> [{y,3},t];
var_args(move_jump, 0) -> [f,{x,1}];
var_args(is_list, 1) -> [f,{x,1}];
var_args(extract_next_element2, 7) -> [{x,7}];
var_args(l_bsr, 0) -> [f,u8,x8];
var_args(l_bif2, 1) -> [f,{b,{erlang,'=:=',2}},t];
var_args(l_move_call, 9) -> [{smallint,1},f];
var_args(l_call_ext, 8) -> [{e,{filename,join,2}}];
var_args(l_call_ext, 7) -> [{e,{lists,sort,1}}];
var_args(apply, 0) -> [u8];
var_args(l_is_eq_exact_immed, 14) -> [f,{x,12},t];
var_args(set_tuple_element, 1) -> [t,{x,0},u32];
var_args(l_bs_test_zero_tail2, 3) -> [f,{x,1}];
var_args(move_deallocate_return, 5) -> [t,{u,2}];
var_args(l_bif2, 2) -> [f,{b,{erlang,'=<',2}},t];
var_args(l_times, 0) -> [f,u8,{x,0}];
var_args(l_is_eq_exact_literal, 2) -> [f,{x,2},t];
var_args(l_fast_element, 2) -> [x8,u8,t];
var_args(l_fetch, 8) -> [t,{x,1}];
var_args(l_is_eq_exact_immed, 10) -> [f,{x,255},{a,xmerl_scanner}];
var_args(l_is_eq_exact_literal, 3) -> [f,{x,4},t];
var_args(l_fmul, 0) -> [fr,fr,fr];
var_args(l_move_call_ext, 10) -> [{y,6},e];
var_args(call_bif, 21) -> [{b,{ets,lookup_element,3}}];
var_args(extract_next_element, 27) -> [t];
var_args(extract_next_element, 10) -> [{x,8}];
var_args(is_nil, 9) -> [f,{x,9}];
var_args(l_times, 1) -> [f,u8,x8];
var_args(l_move_call_ext, 11) -> [t,{e,{io_lib,format,2}}];
var_args(l_allocate_zero, 6) -> [{u,7}];
var_args(l_bs_init_fail, 0) -> [u8,f,u8,x8];
var_args(l_bs_get_integer_8, 0) -> [{x,0},f,x8];
var_args(test_arity, 2) -> [f,{x,0},u32];
var_args(extract_next_element3, 2) -> [{x,2}];
var_args(is_nonempty_list, 12) -> [f,{x,12}];
var_args(l_is_eq_exact_immed, 15) -> [f,y8,{a,asn1_NOVALUE}];
var_args(l_call_ext, 9) -> [{e,{ordsets,union,2}}];
var_args(l_bsl, 0) -> [f,u8,{x,0}];
var_args(l_move_call_ext_only, 2) -> [{e,{lists,reverse,1}},t];
var_args(l_bs_get_integer_8, 1) -> [x8,f,x8];
var_args(l_put_tuple, 6) -> [y8,u8];
var_args(is_tuple, 2) -> [f,{x,2}];
var_args(l_is_ne_exact, 0) -> [f];
var_args(l_select_val2, 6) -> [t,f,{a,true},f,{a,false},f];
var_args(move, 9) -> [t,{x,6}];
var_args(l_move_call_ext, 12) -> [t,{e,{erlang,put,2}}];
var_args(l_call_last, 7) -> [f,{u,7}];
var_args(l_call_fun, 2) -> [{u8,2}];
var_args(node, 0) -> [{x,0}];
var_args(is_binary, 0) -> [f,{x,1}];
var_args(l_select_val2, 9) -> [{x,4},f,t,f,t,f];
var_args(l_is_ne_exact_immed, 2) -> [f,{x,2},t];
var_args(l_new_bs_put_binary_all, 0) -> [f,{x,0},{u8,8}];
var_args(l_bs_test_zero_tail2, 5) -> [f,t];
var_args(l_bif2, 3) -> [f,{b,{erlang,'or',2}},t];
var_args(extract_next_element3, 3) -> [{x,5}];
var_args(l_move_call_ext_last, 1) -> [e,{u,0},t];
var_args(get_list, 5) -> [y8,x8,x8];
var_args(l_bif2, 4) -> [f,{b,{erlang,'and',2}},t];
var_args(l_bs_get_integer_32, 0) -> [x8,f,u8,x8];
var_args(call_bif, 22) -> [{b,{erlang,'--',2}}];
var_args(l_call_ext, 10) -> [{e,{lists,flatten,1}}];
var_args(l_new_bs_put_binary_all, 1) -> [f,t,{u8,8}];
var_args(catch_end, 3) -> [{y,3}];
var_args(extract_next_element, 11) -> [{y,3}];
var_args(l_fcheckerror, 0) -> [];
var_args(fclearerror, 0) -> [];
var_args(is_atom, 1) -> [f,{x,1}];
var_args(l_is_eq_exact_immed, 16) -> [f,{y,0},t];
var_args(l_allocate_zero, 9) -> [u32];
var_args(extract_next_element, 12) -> [{y,2}];
var_args(extract_next_element3, 4) -> [{x,4}];
var_args(get_list, 6) -> [{x,0},t,{x,0}];
var_args(l_bs_append, 0) -> [f,u8,u8,u8,x8];
var_args(l_is_eq_exact_immed, 17) -> [f,{y,1},t];
var_args(node, 1) -> [{x,1}];
var_args(deallocate_return, 7) -> [{u,7}];
var_args(l_bs_get_integer_32, 1) -> [{x,0},f,u8,x8];
var_args(timeout, 0) -> [];
var_args(l_is_eq_exact_immed, 36) -> [f,t,t];
var_args(l_move_call_ext, 14) -> [{y,5},e];
var_args(l_fetch, 9) -> [y8,i8];
var_args(l_move_call, 10) -> [{y,6},f];
var_args(l_move_call_ext, 15) -> [t,{e,{prettypr,text,1}}];
var_args(is_float, 1) -> [f,t];
var_args(l_rem, 0) -> [f,u8,x8];
var_args(l_get, 0) -> [{a,asn1_module},{y,0}];
var_args(move_deallocate_return, 6) -> [t,{u,3}];
var_args(is_nonempty_list, 39) -> [f,t];
var_args(l_fetch, 10) -> [i8,y8];
var_args(call_bif, 23) -> [{b,{re,run,3}}];
var_args(l_call_ext, 11) -> [{e,{test_server,timetrap,1}}];
var_args(l_fast_element, 1) -> [{x,0},{u,1},t];
var_args(l_move_call, 11) -> [{x,5},f];
var_args(l_band, 1) -> [f,u8,{x,0}];
var_args(l_allocate_zero, 7) -> [{u,8}];
var_args(is_nonempty_list, 13) -> [f,{x,13}];
var_args(is_binary, 1) -> [f,{x,0}];
var_args(l_select_tuple_arity, 1) -> [x8,f,u8];
var_args(l_fetch, 11) -> [{x,1},t];
var_args(l_call_ext, 12) -> [{e,{lists,concat,1}}];
var_args(is_nonempty_list, 14) -> [f,{y,2}];
var_args(call_bif, 24) -> [{b,{erlang,process_flag,2}}];
var_args(extract_next_element, 13) -> [{x,9}];
var_args(l_trim, 6) -> [{u,7}];
var_args(is_nil, 10) -> [f,{x,10}];
var_args(l_move_call, 12) -> [nil,f];
var_args(l_get, 1) -> [t,{x,0}];
var_args(l_is_eq_exact_immed, 18) -> [f,{x,255},t];
var_args(is_nil, 11) -> [f,{y,1}];
var_args(l_select_val2, 10) -> [{x,5},f,t,f,t,f];
var_args(l_bs_get_integer_small_imm, 0) -> [{x,0},u8,f,u8,x8];
var_args(l_bsl, 1) -> [f,u8,x8];
var_args(l_get, 2) -> [t,{x,1}];
var_args(l_call_ext, 13) -> [{e,{test_server,timetrap_cancel,1}}];
var_args(l_call_last, 8) -> [f,{u,8}];
var_args(l_is_eq_exact_immed, 19) -> [f,{x,13},t];
var_args(l_move_call_last, 4) -> [f,{u,3},t];
var_args(l_bor, 0) -> [f,u8,{x,0}];
var_args(l_allocate, 7) -> [{u,7}];
var_args(l_call_ext, 14) -> [{e,{file,delete,1}}];
var_args(is_integer, 1) -> [f,{x,1}];
var_args(is_nil, 30) -> [f,t];
var_args(is_nil, 13) -> [f,{x,11}];
var_args(is_nil, 12) -> [f,{x,12}];
var_args(l_move_call_only, 5) -> [f,{x,6}];
var_args(l_move_call, 13) -> [{x,4},f];
var_args(move2, 8) -> [{x,0},x8,x8,x8];
var_args(l_gc_bif1, 5) -> [f,b,y8,u8,{x,0}];
var_args(call_bif, 25) -> [{b,{erlang,process_info,2}}];
var_args(move_return, 7) -> [{x,4}];
var_args(is_list, 2) -> [f,{x,2}];
var_args(l_call_ext, 15) -> [{e,{mnesia_lib,set,2}}];
var_args(l_move_call_ext, 17) -> [{x,3},e];
var_args(l_move_call_ext, 16) -> [t,{e,{proplists,get_value,3}}];
var_args(l_catch, 4) -> [{y,4},t];
var_args(call_bif, 27) -> [{b,{erlang,tuple_to_list,1}}];
var_args(call_bif, 26) -> [{b,{erlang,unlink,1}}];
var_args(extract_next_element2, 8) -> [{x,9}];
var_args(is_nonempty_list, 15) -> [f,{y,3}];
var_args(l_move_call_ext, 18) -> [nil,e];
var_args(l_select_val2, 17) -> [t,f,t,f,t,f];
var_args(l_element, 1) -> [t,t,t];
var_args(l_fadd, 0) -> [fr,fr,fr];
var_args(extract_next_element3, 11) -> [t];
var_args(l_put_tuple, 5) -> [{x,0},{u,6}];
var_args(bif1_body, 1) -> [{b,{erlang,hd,1}},{x,0},{x,1}];
var_args(call_bif, 28) -> [{b,{erlang,whereis,1}}];
var_args(move_jump, 1) -> [f,{x,2}];
var_args(l_is_ne_exact_immed, 3) -> [f,t,{a,true}];
var_args(call_bif, 29) -> [{b,{lists,keyfind,3}}];
var_args(l_call_ext, 16) -> [{e,{test_server,lookup_config,2}}];
var_args(l_select_val2, 8) -> [t,f,{a,false},f,{a,true},f];
var_args(is_nonempty_list, 16) -> [f,{x,14}];
var_args(fmove_1, 0) -> [x8,fr];
var_args(l_fetch, 12) -> [{x,4},t];
var_args(extract_next_element2, 18) -> [t];
var_args(l_bs_get_utf8, 0) -> [x8,f,x8];
var_args(is_atom, 2) -> [f,{x,2}];
var_args(bif1_body, 2) -> [{b,{erlang,hd,1}},{y,1},{x,2}];
var_args(l_is_ne_exact_immed, 11) -> [f,t,t];
var_args(l_call_ext_last, 3) -> [e,{u,3}];
var_args(l_move_call_ext, 19) -> [t,{e,{asn1ct_name,new,1}}];
var_args(bif2_body, 1) -> [b,{x,1}];
var_args(l_move_call_ext, 20) -> [t,{e,{test_server,seconds,1}}];
var_args(call_bif, 30) -> [{b,{erlang,iolist_to_binary,1}}];
var_args(l_call_ext, 17) -> [{e,{erl_syntax,type,1}}];
var_args(l_is_eq_exact_immed, 20) -> [f,{y,3},t];
var_args(l_bs_skip_bits_all2, 0) -> [f,{x,2},{u8,8}];
var_args(call_bif, 31) -> [{b,{erlang,list_to_tuple,1}}];
var_args(case_end, 10) -> [t];
var_args(l_select_tuple_arity, 0) -> [{x,0},f,{u,6}];
var_args(l_move_call_ext, 21) -> [t,{e,{proplists,get_value,2}}];
var_args(l_is_eq_exact_immed, 21) -> [f,{x,14},t];
var_args(fconv, 0) -> [t,{fr,0}];
var_args(l_catch, 5) -> [{y,5},t];
var_args(put_list, 8) -> [y8,t,x8];
var_args(l_move_call_ext_last, 2) -> [e,{u,1},t];
var_args(call_bif, 4) -> [{b,{erlang,exit,2}}];
var_args(extract_next_element3, 5) -> [{x,7}];
var_args(put_list, 6) -> [{x,0},t,{y,0}];
var_args(l_is_eq_exact_literal, 4) -> [f,{x,3},t];
var_args(l_select_tuple_arity, 2) -> [{x,0},f,u32];
var_args(system_limit, 0) -> [f];
var_args(l_call_ext_last, 4) -> [e,{u,4}];
var_args(l_fetch, 23) -> [t,t];
var_args(extract_next_element, 14) -> [{x,10}];
var_args(is_binary, 2) -> [f,{x,2}];
var_args(l_bor, 1) -> [f,u8,x8];
var_args(call_bif, 32) -> [{b,{ets,new,2}}];
var_args(l_call_ext, 18) -> [{e,{lists,mapfoldl,3}}];
var_args(is_integer_allocate, 0) -> [f,x8,u8];
var_args(l_is_eq_exact_immed, 23) -> [f,{y,2},t];
var_args(l_move_call_ext_only, 3) -> [e,{x,2}];
var_args(badmatch, 4) -> [{y,2}];
var_args(l_call_fun, 3) -> [{u8,0}];
var_args(l_bs_init_bits_fail, 0) -> [u32,f,u8,t];
var_args(l_make_export, 0) -> [e];
var_args(is_integer, 6) -> [f,t];
var_args(call_bif, 34) -> [{b,{erlang,make_ref,0}}];
var_args(call_bif, 33) -> [{b,{lists,reverse,2}}];
var_args(apply_last, 0) -> [u8,u32];
var_args(fmove_2, 0) -> [fr,{x,0}];
var_args(fmove_2, 1) -> [fr,x8];
var_args(fmove_1, 1) -> [t,{fr,1}];
var_args(call_bif, 35) -> [{b,{lists,keymember,3}}];
var_args(l_call_ext, 19) -> [{e,{prettypr,floating,1}}];
var_args(extract_next_element, 15) -> [{y,5}];
var_args(try_end, 2) -> [{y,5}];
var_args(deallocate_return, 8) -> [{u,8}];
var_args(l_fetch, 13) -> [{x,3},t];
var_args(l_fast_element, 3) -> [t,{u,2},t];
var_args(l_move_call_ext, 22) -> [{y,7},e];
var_args(is_tuple, 3) -> [f,{x,3}];
var_args(l_bs_restore2, 1) -> [{x,0},{u,0}];
var_args(l_move_call_ext, 23) -> [t,{e,{mnesia_lib,verbose,2}}];
var_args(l_bs_test_zero_tail2, 4) -> [f,{x,4}];
var_args(l_new_bs_put_integer, 0) -> [f,t,{u8,1},{u8,0},t];
var_args(extract_next_element, 16) -> [{y,4}];
var_args(l_trim, 7) -> [{u,8}];
var_args(is_list, 6) -> [f,t];
var_args(is_nonempty_list, 17) -> [f,{x,15}];
var_args(l_fetch, 14) -> [t,{x,4}];
var_args(extract_next_element2, 9) -> [{x,12}];
var_args(is_atom, 3) -> [f,{x,3}];
var_args(l_fetch, 15) -> [{x,2},t];
var_args(is_integer, 2) -> [f,{x,2}];
var_args(l_move_call, 15) -> [{y,7},f];
var_args(l_move_call, 14) -> [{smallint,0},f];
var_args(l_call_ext, 21) -> [{e,{dict,find,2}}];
var_args(l_call_ext, 20) -> [{e,{proplists,get_value,2}}];
var_args(try_end, 3) -> [{y,3}];
var_args(l_move_call_ext, 13) -> [{smallint,0},{e,{lists,seq,2}}];
var_args(l_call_ext_last, 6) -> [e,u32];
var_args(case_end, 3) -> [{y,2}];
var_args(is_nonempty_list, 18) -> [f,{y,1}];
var_args(try_end, 4) -> [{y,2}];
var_args(init, 8) -> [{y,8}];
var_args(if_end, 0) -> [];
var_args(move_deallocate_return, 7) -> [t,{u,4}];
var_args(call_bif, 39) -> [{b,{erlang,spawn_link,1}}];
var_args(call_bif, 38) -> [{b,{erlang,now,0}}];
var_args(call_bif, 37) -> [{b,{erlang,get_stacktrace,0}}];
var_args(call_bif, 36) -> [{b,{ets,delete,2}}];
var_args(try_end, 5) -> [{y,4}];
var_args(is_nonempty_list_test_heap, 0) -> [f,u8,u8];
var_args(l_move_call_ext, 24) -> [t,{e,{lists,duplicate,2}}];
var_args(l_move_call_ext_last, 3) -> [e,{u,2},t];
var_args(l_call_ext, 25) -> [{e,{erlang,list_to_integer,1}}];
var_args(l_call_ext, 24) -> [{e,{asn1_db,dbget,2}}];
var_args(l_call_ext, 23) -> [{e,{string,tokens,2}}];
var_args(l_call_ext, 22) -> [{e,{prettypr,beside,2}}];
var_args(l_get, 3) -> [t,{x,2}];
var_args(put_list, 9) -> [x8,t,y8];
var_args(self, 2) -> [{x,2}];
var_args(l_call_ext, 28) -> [{e,{filename,join,1}}];
var_args(l_call_ext, 27) -> [{e,{dict,new,0}}];
var_args(l_call_ext, 26) -> [{e,{proplists,get_value,3}}];
var_args(extract_next_element, 17) -> [{x,11}];
var_args(is_list, 3) -> [f,{x,3}];
var_args(catch_end, 4) -> [{y,4}];
var_args(l_move_call_last, 5) -> [f,u8,x8];
var_args(l_is_ne_exact_immed, 4) -> [f,{x,3},t];
var_args(is_tuple, 4) -> [f,{x,4}];
var_args(extract_next_element2, 10) -> [{x,10}];
var_args(move_jump, 2) -> [f,nil];
var_args(l_bs_skip_bits_imm2, 0) -> [f,t,u32];
var_args(l_fdiv, 0) -> [fr,fr,fr];
var_args(get_tuple_element, 8) -> [{x,0},u32,{x,0}];
var_args(l_bif1, 0) -> [f,b,x8,x8];
var_args(l_bs_get_utf16, 1) -> [{x,0},f,u8,x8];
var_args(extract_next_element, 18) -> [{x,12}];
var_args(is_integer, 3) -> [f,{x,4}];
var_args(l_bs_test_unit_8, 3) -> [f,t];
var_args(is_nonempty_list, 19) -> [f,{x,16}];
var_args(l_bif1, 1) -> [f,b,{x,0},t];
var_args(is_tuple, 10) -> [f,t];
var_args(bif1_body, 3) -> [b,{x,0},{x,0}];
var_args(get_list, 8) -> [x8,y8,y8];
var_args(l_select_val2, 11) -> [{x,6},f,t,f,t,f];
var_args(badmatch, 5) -> [{y,3}];
var_args(l_move_call, 16) -> [{smallint,2},f];
var_args(l_is_eq_exact_immed, 24) -> [f,{x,15},t];
var_args(l_call_ext, 30) -> [{e,{lists,filter,2}}];
var_args(l_call_ext, 29) -> [{e,{dict,store,3}}];
var_args(l_is_eq_exact_immed, 25) -> [f,{x,16},t];
var_args(l_move_call_ext, 25) -> [t,{e,{erlang,system_info,1}}];
var_args(l_select_val_atoms, 2) -> [y8,f,u8];
var_args(extract_next_element2, 11) -> [{x,11}];
var_args(move_return, 8) -> [{smallint,1}];
var_args(case_end, 4) -> [{x,3}];
var_args(self, 5) -> [t];
var_args(l_call_ext, 31) -> [{e,{sofs,to_external,1}}];
var_args(extract_next_element3, 6) -> [{x,6}];
var_args(l_bs_get_binary_imm2, 0) -> [f,x8,u8,u8,{u8,0},x8];
var_args(l_bs_test_unit_8, 1) -> [f,{x,3}];
var_args(is_list, 4) -> [f,{x,4}];
var_args(l_bs_restore2, 2) -> [{x,0},{u,1}];
var_args(is_atom, 6) -> [f,t];
var_args(l_move_call_ext_last, 5) -> [e,u32,t];
var_args(bif1_body, 5) -> [b,t,{x,1}];
var_args(call_bif, 41) -> [{b,{ets,match_object,2}}];
var_args(call_bif, 40) -> [{b,{ets,safe_fixtable,2}}];
var_args(is_nil, 14) -> [f,{x,13}];
var_args(l_move_call_only, 6) -> [f,nil];
var_args(move_deallocate_return, 8) -> [t,{u,5}];
var_args(init, 9) -> [{y,9}];
var_args(l_plus, 2) -> [f,u8,y8];
var_args(test_arity, 3) -> [f,y8,u8];
var_args(l_get, 6) -> [t,t];
var_args(l_rem, 1) -> [f,u8,{x,0}];
var_args(l_call_ext, 32) -> [{e,{ordsets,from_list,1}}];
var_args(is_nil, 15) -> [f,{y,2}];
var_args(case_end, 5) -> [{y,1}];
var_args(l_move_call_ext, 27) -> [t,{e,{mnesia_lib,dbg_out,2}}];
var_args(l_move_call_ext, 28) -> [t,{e,{erlang,erase,1}}];
var_args(call_bif, 42) -> [{b,{ets,next,2}}];
var_args(l_call_ext, 33) -> [{e,{unicode,characters_to_binary,1}}];
var_args(is_nil, 16) -> [f,{x,14}];
var_args(l_fsub, 0) -> [fr,fr,fr];
var_args(l_is_eq_exact_literal, 5) -> [f,{x,6},t];
var_args(l_jump_on_val, 0) -> [x8,f,u8,u8];
var_args(l_move_call_ext, 29) -> [t,{e,{lists,seq,2}}];
var_args(get_tuple_element, 9) -> [y8,u8,{x,0}];
var_args(l_move_call_last, 6) -> [f,{u,5},t];
var_args(call_bif, 43) -> [{b,{ets,match,2}}];
var_args(is_integer, 4) -> [f,{x,3}];
var_args(l_bs_put_string, 0) -> [{u,1},str];
var_args(is_nonempty_list, 20) -> [f,{x,17}];
var_args(l_bif2, 5) -> [f,{b,{erlang,'==',2}},t];
var_args(l_move_call_ext, 30) -> [t,{e,{test_server,lookup_config,2}}];
var_args(put_list, 7) -> [{smallint,10},{x,0},t];
var_args(l_move_call_ext, 31) -> [t,{e,{erlang,binary_to_term,1}}];
var_args(bif1_body, 6) -> [b,t,{x,0}];
var_args(l_int_div, 0) -> [f,u8,{x,0}];
var_args(node, 4) -> [t];
var_args(l_bsr, 1) -> [f,u8,{x,0}];
var_args(l_increment, 4) -> [{x,0},u8,u8,x8];
var_args(l_put_tuple, 7) -> [{x,0},{u,7}];
var_args(l_call_ext, 34) -> [{e,{gb_trees,lookup,2}}];
var_args(badmatch, 6) -> [{x,4}];
var_args(case_end, 6) -> [{y,3}];
var_args(l_is_eq_exact_literal, 6) -> [f,{x,5},t];
var_args(l_call_ext_only, 0) -> [{e,{gen_server,call,3}}];
var_args(move, 10) -> [t,{x,7}];
var_args(l_increment, 5) -> [x8,u32,u8,x8];
var_args(l_allocate, 9) -> [u32];
var_args(l_call_ext, 35) -> [{e,{erlang,put,2}}];
var_args(l_allocate_zero, 8) -> [{u,9}];
var_args(call_bif, 44) -> [{b,{erlang,monitor,2}}];
var_args(l_call_ext, 36) -> [{e,{gb_trees,empty,0}}];
var_args(l_call_last, 9) -> [f,{u,9}];
var_args(move_return, 9) -> [{smallint,2}];
var_args(is_list, 5) -> [f,{x,5}];
var_args(l_move_call, 17) -> [{a,foo},f];
var_args(catch_end, 5) -> [{y,5}];
var_args(l_is_eq_exact_immed, 26) -> [f,{y,4},t];
var_args(l_fetch, 16) -> [{x,5},t];
var_args(l_put_tuple, 8) -> [{x,0},u32];
var_args(l_call_ext, 38) -> [{e,{lists,last,1}}];
var_args(l_call_ext, 37) -> [{e,{lists,delete,2}}];
var_args(put_list, 10) -> [{x,0},t,t];
var_args(l_move_call, 18) -> [{y,12},f];
var_args(test_heap_1_put_list, 2) -> [u8,i8];
var_args(get_list, 9) -> [{x,0},y8,y8];
var_args(l_bs_skip_bits2, 0) -> [f,t,t,u8];
var_args(l_fetch, 17) -> [t,{x,3}];
var_args(call_bif, 45) -> [{b,{erlang,fun_info,2}}];
var_args(node, 2) -> [{x,2}];
var_args(extract_next_element2, 12) -> [{x,13}];
var_args(is_nil, 17) -> [f,{x,15}];
var_args(try_case_end, 0) -> [{x,0}];
var_args(l_yield, 0) -> [];
var_args(l_bs_get_binary_all2, 1) -> [f,{x,0},u8,u8,x8];
var_args(is_atom, 4) -> [f,{x,4}];
var_args(l_move_call, 19) -> [{a,endDocument},f];
var_args(l_call_ext_only, 1) -> [{e,{ssh_bits,encode,2}}];
var_args(l_move_call_ext, 33) -> [{x,4},e];
var_args(l_is_eq_exact_immed, 27) -> [f,{x,17},t];
var_args(move_return, 10) -> [{smallint,0}];
var_args(l_move_call_ext, 34) -> [t,{e,{mnesia_lib,set,2}}];
var_args(l_bs_get_integer, 0) -> [f,u8,u8,u8,x8];
var_args(l_bs_append, 1) -> [f,u8,u8,u8,{x,0}];
var_args(l_move_call_ext, 35) -> [t,{e,{lists,sublist,3}}];
var_args(l_bs_get_binary2, 1) -> [f,x8,u8,t,u8,{u8,0},x8];
var_args(call_bif, 46) -> [{b,{erlang,garbage_collect,0}}];
var_args(l_is_eq_exact_immed, 22) -> [f,{y,10},{a,ber}];
var_args(extract_next_element, 19) -> [{x,13}];
var_args(extract_next_element3, 7) -> [{x,8}];
var_args(is_nonempty_list, 22) -> [f,{y,4}];
var_args(is_nonempty_list, 21) -> [f,{x,18}];
var_args(l_move_call, 20) -> [{smallint,3},f];
var_args(l_is_ne_exact_immed, 5) -> [f,{x,4},t];
var_args(l_fetch, 18) -> [t,{x,2}];
var_args(l_catch, 6) -> [{y,6},t];
var_args(call_bif, 47) -> [{b,{erlang,register,2}}];
var_args(l_allocate, 8) -> [{u,8}];
var_args(l_call_ext, 40) -> [{e,{lists,keydelete,3}}];
var_args(l_call_ext, 39) -> [{e,{io,format,3}}];
var_args(l_is_ne_exact_immed, 6) -> [f,{y,0},t];
var_args(badmatch, 7) -> [{y,4}];
var_args(case_end, 7) -> [{x,4}];
var_args(is_binary, 3) -> [f,t];
var_args(init, 10) -> [{y,10}];
var_args(l_gc_bif1, 6) -> [f,{b,{erlang,length,1}},x8,u8,y8];
var_args(l_int_div, 1) -> [f,u8,x8];
var_args(l_is_eq_exact_immed, 29) -> [f,{y,5},t];
var_args(l_is_eq_exact_immed, 28) -> [f,{x,18},t];
var_args(is_function, 1) -> [f,t];
var_args(fconv, 1) -> [t,{fr,1}];
var_args(fmove_1, 3) -> [t,fr];
var_args(l_call_ext, 42) -> [{e,{filename,basename,1}}];
var_args(l_call_ext, 41) -> [{e,{cerl,get_ann,1}}];
var_args(move_jump, 3) -> [f,{x,4}];
var_args(is_nil, 18) -> [f,{x,16}];
var_args(case_end, 8) -> [{y,0}];
var_args(l_select_val2, 12) -> [{x,7},f,t,f,t,f];
var_args(l_bs_save2, 1) -> [{x,0},{u,1}];
var_args(l_move_call, 23) -> [{a,ets},f];
var_args(l_move_call, 22) -> [{a,schema},f];
var_args(l_move_call, 21) -> [{x,6},f];
var_args(l_move_call_ext, 36) -> [t,{e,{re,replace,4}}];
var_args(deallocate_return, 12) -> [u32];
var_args(bif2_body, 2) -> [b,{x,2}];
var_args(l_move_call_ext, 37) -> [{a,schema},e];
var_args(l_move_call_ext_last, 4) -> [e,{u,3},t];
var_args(call_bif, 1) -> [{b,{erlang,raise,3}}];
var_args(l_call_ext, 49) -> [{e,{erlang,binary_to_term,1}}];
var_args(l_call_ext, 48) -> [{e,{file,write,2}}];
var_args(l_call_ext, 47) -> [{e,{file,read_file_info,1}}];
var_args(l_call_ext, 46) -> [{e,{erl_syntax,atom,1}}];
var_args(l_call_ext, 45) -> [{e,{ordsets,subtract,2}}];
var_args(l_call_ext, 44) -> [{e,{file,read_file,1}}];
var_args(l_call_ext, 43) -> [{e,{ssh_channel,cache_lookup,2}}];
var_args(l_move_call_ext_only, 4) -> [e,{x,1}];
var_args(l_is_ne_exact_immed, 7) -> [f,x8,i8];
var_args(l_move_call_ext, 38) -> [{smallint,2},e];
var_args(l_gc_bif1, 7) -> [f,b,{x,0},u8,y8];
var_args(l_call_ext, 52) -> [{e,{file,read,2}}];
var_args(l_call_ext, 51) -> [{e,{file,make_dir,1}}];
var_args(l_call_ext, 50) -> [{e,{lists,duplicate,2}}];
var_args(is_nil, 19) -> [f,{y,3}];
var_args(is_nonempty_list, 23) -> [f,{x,20}];
var_args(l_call_fun, 4) -> [u8];
var_args(l_move_call_ext_only, 5) -> [{e,{io,format,2}},t];
var_args(l_move_call_ext, 39) -> [{a,func},e];
var_args(is_pid, 1) -> [f,t];
var_args(bif1_body, 4) -> [{b,{erlang,hd,1}},y8,x8];
var_args(l_call_ext, 55) -> [{e,{asn1ct_gen,mk_var,1}}];
var_args(l_call_ext, 54) -> [{e,{erlang,term_to_binary,1}}];
var_args(l_call_ext, 53) -> [{e,{mnesia_lib,exists,1}}];
var_args(l_trim, 8) -> [{u,9}];
var_args(l_bs_test_unit_8, 2) -> [f,{x,1}];
var_args(l_move_call, 24) -> [{a,false},f];
var_args(l_move_call_ext, 42) -> [t,{e,{re,split,3}}];
var_args(l_move_call_ext, 41) -> [t,{e,{io,format,1}}];
var_args(l_move_call_ext, 40) -> [t,{e,{io,fwrite,2}}];
var_args(l_get, 4) -> [{a,asn1_module},y8];
var_args(l_increment, 8) -> [t,u32,u8,t];
var_args(is_boolean, 0) -> [f,t];
var_args(l_bs_get_integer_small_imm, 1) -> [x8,u8,f,u8,x8];
var_args(l_call_ext, 57) -> [{e,{gb_trees,get,2}}];
var_args(l_call_ext, 56) -> [{e,{asn1ct_gen,get_inner,1}}];
var_args(extract_next_element, 20) -> [{x,14}];
var_args(extract_next_element2, 13) -> [{x,14}];
var_args(move_jump, 4) -> [f,{y,0}];
var_args(bs_context_to_binary, 1) -> [{x,1}];
var_args(l_bs_put_string, 2) -> [u32,str];
var_args(l_call_ext_last, 5) -> [e,{u,5}];
var_args(bif1_body, 7) -> [b,t,t];
var_args(init, 11) -> [{y,11}];
var_args(l_fetch, 19) -> [{y,0},t];
var_args(badmatch, 16) -> [t];
var_args(bs_init_writable, 0) -> [];
var_args(l_call_ext, 58) -> [{e,{sofs,family_union,2}}];
var_args(l_move_call_only, 7) -> [f,{x,7}];
var_args(put_list, 12) -> [t,t,t];
var_args(case_end, 9) -> [{y,4}];
var_args(l_bs_private_append, 0) -> [f,u8,t];
var_args(l_move_call, 25) -> [{smallint,4},f];
var_args(l_is_ne_exact_literal, 0) -> [f,t,t];
var_args(l_call_ext, 65) -> [{e,{mnesia_lib,cs_to_storage_type,2}}];
var_args(l_call_ext, 64) -> [{e,{file,rename,2}}];
var_args(l_call_ext, 63) -> [{e,{erlang,max,2}}];
var_args(l_call_ext, 62) -> [{e,{filename,dirname,1}}];
var_args(l_call_ext, 61) -> [{e,{lists,append,1}}];
var_args(l_call_ext, 60) -> [{e,{test_server,fail,1}}];
var_args(l_call_ext, 59) -> [{e,{random,uniform,1}}];
var_args(extract_next_element2, 14) -> [{x,16}];
var_args(move_jump, 5) -> [f,{y,1}];
var_args(move_return, 11) -> [{smallint,3}];
var_args(badmatch, 9) -> [{x,5}];
var_args(badmatch, 8) -> [{y,0}];
var_args(l_move_call, 26) -> [{smallint,100},f];
var_args(move_deallocate_return, 10) -> [t,u32];
var_args(l_call_ext, 68) -> [{e,{mnesia_monitor,use_dir,0}}];
var_args(l_call_ext, 67) -> [{e,{gb_trees,insert,3}}];
var_args(l_call_ext, 66) -> [{e,{lists,splitwith,2}}];
var_args(extract_next_element3, 8) -> [{x,11}];
var_args(l_bs_start_match2, 2) -> [t,f,u8,u8,{x,0}];
var_args(move_jump, 6) -> [f,{y,2}];
var_args(move_return, 12) -> [{smallint,4}];
var_args(put_list, 11) -> [y8,t,y8];
var_args(deallocate_return, 9) -> [{u,9}];
var_args(l_wait_timeout, 2) -> [f,u32];
var_args(l_get, 5) -> [{a,mnesia_activity_state},t];
var_args(l_element, 0) -> [t,{x,0},{x,1}];
var_args(l_bs_skip_bits_all2, 1) -> [f,{x,3},{u8,8}];
var_args(l_select_tuple_arity2, 2) -> [y8,f,u8,f,u8,f];
var_args(l_is_eq_exact_immed, 30) -> [f,{x,19},t];
var_args(l_bif2, 6) -> [f,b,t];
var_args(l_call_ext, 73) -> [{e,{mnesia_schema,list2cs,1}}];
var_args(l_call_ext, 72) -> [{e,{lists,dropwhile,2}}];
var_args(l_call_ext, 71) -> [{e,{gb_trees,from_orddict,1}}];
var_args(l_call_ext, 70) -> [{e,{gb_trees,to_list,1}}];
var_args(l_call_ext, 69) -> [{e,{unicode,characters_to_list,1}}];
var_args(extract_next_element2, 15) -> [{y,0}];
var_args(move_jump, 7) -> [f,{x,3}];
var_args(move_return, 14) -> [{a,error}];
var_args(move_return, 13) -> [{a,no}];
var_args(l_move_call_only, 8) -> [f,{x,8}];
var_args(is_nonempty_list, 24) -> [f,{x,19}];
var_args(l_move_call, 28) -> [{a,x},f];
var_args(l_move_call, 27) -> [{smallint,9},f];
var_args(l_call_ext_only, 2) -> [{e,{asn1ct_gen,emit,1}}];
var_args(fmove_1, 2) -> [t,{fr,0}];
var_args(bif2_body, 3) -> [b,t];
var_args(l_new_bs_put_integer_imm, 1) -> [f,u32,u8,t];
var_args(self, 3) -> [{x,3}];
var_args(node, 3) -> [{x,3}];
var_args(l_call_ext, 76) -> [{e,{os,type,0}}];
var_args(l_call_ext, 75) -> [{e,{asn1ct_gen,type,1}}];
var_args(l_call_ext, 74) -> [{e,{erl_syntax,atom_value,1}}];
var_args(move_return, 15) -> [{a,undefined}];
var_args(is_nonempty_list, 25) -> [f,{y,9}];
var_args(is_pid, 0) -> [f,{x,0}];
var_args(l_new_bs_put_float_imm, 0) -> [f,{u,64},{u8,0},{x,0}];
var_args(l_move_call, 30) -> [{smallint,1000},f];
var_args(l_move_call, 29) -> [{y,8},f];
var_args(l_move_call_ext, 26) -> [{a,funky},{e,{estone_SUITE,req,2}}];
var_args(test_heap_1_put_list, 3) -> [u8,y8];
var_args(catch_end, 6) -> [{y,6}];
var_args(l_is_ne_exact_immed, 8) -> [f,{y,2},t];
var_args(l_move_call_ext_only, 6) -> [e,{x,3}];
var_args(l_catch, 8) -> [t,t];
var_args(l_call_ext, 77) -> [{e,{sets,is_element,2}}];
var_args(extract_next_element3, 9) -> [{x,10}];
var_args(bs_context_to_binary, 2) -> [{x,2}];
var_args(l_new_bs_put_float_imm, 1) -> [f,{u,32},{u8,0},t];
var_args(l_move_call, 31) -> [{smallint,42},f];
var_args(l_move_call_last, 7) -> [f,{u,4},t];
var_args(l_fetch, 20) -> [t,{y,5}];
var_args(l_is_ne_exact_immed, 9) -> [f,{y,1},t];
var_args(init, 16) -> [t];
var_args(l_gc_bif1, 9) -> [f,b,t,u8,t];
var_args(l_fetch, 21) -> [t,{x,5}];
var_args(l_catch, 7) -> [{y,19},t];
var_args(is_tuple, 5) -> [f,{x,7}];
var_args(l_call_ext, 80) -> [{e,{erl_syntax,get_pos,1}}];
var_args(l_call_ext, 79) -> [{e,{cerl,var_name,1}}];
var_args(l_call_ext, 78) -> [{e,{asn1ct_gen,list2name,1}}];
var_args(l_is_eq_exact_immed, 31) -> [f,{x,22},t];
var_args(move_return, 17) -> [{smallint,8}];
var_args(move_return, 16) -> [{a,none}];
var_args(is_nil, 20) -> [f,{x,17}];
var_args(recv_mark, 0) -> [f];
var_args(l_bs_get_utf8, 1) -> [t,f,t];
var_args(is_nonempty_list, 26) -> [f,{y,0}];
var_args(try_end, 6) -> [{y,6}];
var_args(deallocate_return, 10) -> [{u,10}];
var_args(l_bs_get_integer_imm, 0) -> [t,u8,u8,f,u8,x8];
var_args(l_move_call, 32) -> [{smallint,12},f];
var_args(catch_end, 7) -> [{y,19}];
var_args(l_recv_set, 0) -> [];
var_args(move2, 9) -> [{x,0},x8,{x,0},x8];
var_args(l_bs_skip_bits_all2, 2) -> [f,t,u8];
var_args(l_new_bs_put_integer, 1) -> [f,t,u8,u8,t];
var_args(test_heap_1_put_list, 4) -> [u32,t];
var_args(self, 4) -> [{y,0}];
var_args(l_call_ext, 84) -> [{e,{file,get_cwd,0}}];
var_args(l_call_ext, 83) -> [{e,{asn1ct,use_legacy_types,0}}];
var_args(l_call_ext, 82) -> [{e,{gb_sets,empty,0}}];
var_args(l_call_ext, 81) -> [{e,{file,write_file,2}}];
var_args(move_jump, 8) -> [f,{a,false}];
var_args(l_move_call, 33) -> [{a,true},f];
var_args(l_apply, 0) -> [];
var_args(get_list, 10) -> [x8,y8,x8];
var_args(l_select_val2, 13) -> [{x,8},f,t,f,t,f];
var_args(l_increment, 7) -> [{x,0},u32,u8,y8];
var_args(l_call_ext, 92) -> [{e,{ordsets,intersection,2}}];
var_args(l_call_ext, 91) -> [{e,{asn1ct_name,start,0}}];
var_args(l_call_ext, 90) -> [{e,{lists,sublist,3}}];
var_args(l_call_ext, 89) -> [{e,{beam_utils,code_at,2}}];
var_args(l_call_ext, 88) -> [{e,{ordsets,is_element,2}}];
var_args(l_call_ext, 87) -> [{e,{xref_utils,xset,2}}];
var_args(l_call_ext, 86) -> [{e,{dict,fetch,2}}];
var_args(l_call_ext, 85) -> [{e,{mnesia_lib,intersect,2}}];
var_args(l_call_last, 11) -> [f,u32];
var_args(extract_next_element3, 10) -> [{x,9}];
var_args(l_bs_get_float2, 0) -> [f,t,u8,t,u8,u8,t];
var_args(move_jump, 9) -> [f,{a,asn1_NOVALUE}];
var_args(move_return, 18) -> [{x,5}];
var_args(is_nil, 21) -> [f,{y,0}];
var_args(badmatch, 11) -> [{y,9}];
var_args(badmatch, 10) -> [{y,6}];
var_args(l_bs_put_string, 1) -> [{u,4},str];
var_args(is_nonempty_list, 27) -> [f,{y,6}];
var_args(l_select_val2, 14) -> [{y,3},f,t,f,t,f];
var_args(l_move_call_ext, 32) -> [{a,auto_repair},{e,{mnesia_monitor,get_env,1}}];
var_args(is_float, 0) -> [f,{x,0}];
var_args(l_increment, 6) -> [y8,u8,u8,y8];
var_args(get_list, 11) -> [x8,y8,{x,0}];
var_args(is_tuple, 6) -> [f,{x,5}];
var_args(l_call_ext, 95) -> [{e,{gb_sets,is_member,2}}];
var_args(l_call_ext, 94) -> [{e,{cerl,c_tuple,1}}];
var_args(l_call_ext, 93) -> [{e,{lists,seq,2}}];
var_args(extract_next_element, 21) -> [{y,6}];
var_args(l_fast_element, 4) -> [{x,0},{u,3},{x,0}];
var_args(extract_next_element2, 16) -> [{x,15}];
var_args(is_integer, 5) -> [f,{x,5}];
var_args(move_return, 20) -> [{smallint,16}];
var_args(move_return, 19) -> [{a,ignore}];
var_args(badmatch, 12) -> [{x,7}];
var_args(is_function, 0) -> [f,{x,0}];
var_args(init, 12) -> [{y,12}];
var_args(l_gc_bif1, 8) -> [f,b,{x,0},{u8,2},{x,0}];
var_args(l_is_eq_exact_immed, 32) -> [f,{x,20},t];
var_args(l_select_val2, 15) -> [{y,1},f,t,f,t,f];
var_args(l_fetch, 22) -> [t,{y,0}];
var_args(l_call_ext, 101) -> [{e,{ets,tab2list,1}}];
var_args(l_call_ext, 100) -> [{e,{orddict,find,2}}];
var_args(l_call_ext, 99) -> [{e,{proplists,get_bool,2}}];
var_args(l_call_ext, 98) -> [{e,{mnesia,abort,1}}];
var_args(l_call_ext, 97) -> [{e,{rpc,call,4}}];
var_args(l_call_ext, 96) -> [{e,{sofs,difference,2}}];
var_args(extract_next_element, 22) -> [{x,18}];
var_args(bs_context_to_binary, 3) -> [{y,0}];
var_args(is_nonempty_list, 28) -> [f,{x,24}];
var_args(get_list, 12) -> [t,t,t];
var_args(try_end, 7) -> [t];
var_args(l_minus, 2) -> [f,u8,t];
var_args(l_times, 2) -> [f,u8,t];
var_args(l_bif1, 2) -> [f,b,t,t];
var_args(deallocate_return, 11) -> [{u,11}];
var_args(l_apply_fun, 0) -> [];
var_args(l_call_ext_only, 3) -> [{e,{mnesia_monitor,get_env,1}}];
var_args(l_bs_get_integer_16, 0) -> [t,f,t];
var_args(l_bs_init_bits, 0) -> [u32,u32,u8,t];
var_args(move, 12) -> [t,{x,8}];
var_args(wait_timeout, 0) -> [f,t];
var_args(l_select_val2, 16) -> [{x,9},f,t,f,t,f];
var_args(l_call_last, 10) -> [f,{u,10}];
var_args(is_nonempty_list, 29) -> [f,{x,21}];
var_args(is_atom, 5) -> [f,{x,5}];
var_args(l_bs_get_integer_32, 2) -> [x8,f,u8,{x,0}];
var_args(l_trim, 11) -> [u32];
var_args(move_deallocate_return, 9) -> [t,{u,7}];
var_args(is_map, 0) -> [f,t];
var_args(extract_next_element, 23) -> [{x,16}];
var_args(extract_next_element2, 17) -> [{x,17}];
var_args(move_jump, 10) -> [f,{a,true}];
var_args(move_return, 21) -> [{smallint,64}];
var_args(l_trim, 9) -> [{u,11}];
var_args(is_nil, 22) -> [f,{x,19}];
var_args(bs_context_to_binary, 4) -> [{x,7}];
var_args(badmatch, 13) -> [{y,5}];
var_args(is_nonempty_list, 30) -> [f,{x,22}];
var_args(l_new_bs_put_float_imm, 2) -> [f,u32,u8,t];
var_args(l_bs_init_fail, 1) -> [u32,f,u8,t];
var_args(l_is_ne_exact_immed, 10) -> [f,{x,5},t];
var_args(l_bs_restore2, 3) -> [t,u32];
var_args(l_fast_element, 5) -> [t,u32,t];
var_args(is_bitstr, 0) -> [f,t];
var_args(is_integer_allocate, 1) -> [f,t,u32];
var_args(catch_end, 8) -> [t];
var_args(move_jump, 11) -> [f,{smallint,0}];
var_args(is_nil, 23) -> [f,{y,4}];
var_args(is_nonempty_list, 32) -> [f,{y,7}];
var_args(is_nonempty_list, 31) -> [f,{x,25}];
var_args(l_move_call_ext, 43) -> [{smallint,1000},{e,{test_server,sleep,1}}];
var_args(l_is_eq_exact_immed, 34) -> [f,{y,7},t];
var_args(l_is_eq_exact_immed, 33) -> [f,{x,23},t];
var_args(l_move_call_last, 8) -> [f,u32,t];
var_args(l_bs_test_tail_imm2, 0) -> [f,t,u32];
var_args(fconv, 2) -> [t,fr];
var_args(l_bs_save2, 2) -> [t,u32];
var_args(l_jump_on_val, 1) -> [{x,0},f,u8,u8];
var_args(l_bs_add, 1) -> [f,u8,t];
var_args(is_tuple, 8) -> [f,{x,6}];
var_args(is_tuple, 7) -> [f,{y,4}];
var_args(move_return, 24) -> [{x,6}];
var_args(move_return, 23) -> [{a,nomatch}];
var_args(move_return, 22) -> [{smallint,5}];
var_args(bs_context_to_binary, 6) -> [t];
var_args(badmatch, 14) -> [{y,1}];
var_args(is_nonempty_list, 35) -> [f,{y,8}];
var_args(is_nonempty_list, 34) -> [f,{x,26}];
var_args(is_nonempty_list, 33) -> [f,{y,5}];
var_args(l_wait_timeout, 0) -> [f,{u,1000}];
var_args(l_gc_bif2, 0) -> [f,b,u8,t];
var_args(get_tuple_element, 10) -> [y8,u8,y8];
var_args(l_bs_put_utf16, 0) -> [f,u8,t];
var_args(l_new_bs_put_binary, 0) -> [f,t,u8,u8,t];
var_args(l_bs_validate_unicode, 0) -> [f,t];
var_args(l_bs_get_binary2, 2) -> [f,t,u8,t,u8,u8,t];
var_args(is_tuple, 9) -> [f,{x,8}];
var_args(extract_next_element, 26) -> [{x,22}];
var_args(extract_next_element, 25) -> [{y,7}];
var_args(extract_next_element, 24) -> [{x,15}];
var_args(move_return, 27) -> [{smallint,6}];
var_args(move_return, 26) -> [{smallint,128}];
var_args(move_return, 25) -> [{smallint,7}];
var_args(is_nil, 24) -> [f,{x,22}];
var_args(bs_context_to_binary, 5) -> [{x,4}];
var_args(move, 11) -> [nil,{x,10}];
var_args(l_bs_utf16_size, 0) -> [t,t];
var_args(l_is_eq_exact_immed, 35) -> [f,{y,6},t];
var_args(raise, 1) -> [t,t];
var_args(move_jump, 12) -> [f,{y,3}];
var_args(move_return, 28) -> [{smallint,32}];
var_args(is_nil, 27) -> [f,{x,21}];
var_args(is_nil, 26) -> [f,{y,5}];
var_args(is_nil, 25) -> [f,{x,18}];
var_args(is_reference, 0) -> [f,t];
var_args(l_int_bnot, 0) -> [f,t,u8,t];
var_args(l_bs_get_binary_imm2, 1) -> [f,{x,0},u8,u8,u8,x8];
var_args(l_is_function2, 0) -> [f,t,u8];
var_args(fmove_2, 2) -> [fr,t];
var_args(is_nil, 28) -> [f,{x,20}];
var_args(badmatch, 15) -> [{x,6}];
var_args(is_nonempty_list, 37) -> [f,{x,30}];
var_args(is_nonempty_list, 36) -> [f,{x,23}];
var_args(init, 14) -> [{y,14}];
var_args(init, 13) -> [{y,13}];
var_args(is_port, 0) -> [f,t];
var_args(is_nil, 29) -> [f,{x,23}];
var_args(l_new_bs_put_binary_all, 2) -> [f,t,u8];
var_args(is_nonempty_list, 38) -> [f,{x,28}];
var_args(is_number, 0) -> [f,t];
var_args(l_select_tuple_arity, 3) -> [t,f,u32];
var_args(l_bs_get_binary_all_reuse, 1) -> [t,f,u8];
var_args(l_wait_timeout, 1) -> [f,{u,1}];
var_args(l_trim, 10) -> [{u,10}];
var_args(l_bs_put_utf8, 0) -> [f,t];
var_args(l_fnegate, 0) -> [fr,fr];
var_args(l_bs_init, 0) -> [u32,u32,u8,t];
var_args(l_apply_last, 0) -> [u32];
var_args(l_jump_on_val, 2) -> [t,f,u32,u32];
var_args(move, 13) -> [t,t];
var_args(l_bs_utf8_size, 0) -> [t,t];
var_args(l_bs_get_binary_imm2, 2) -> [f,t,u8,u32,u8,t];
var_args(l_new_bs_put_float, 0) -> [f,t,u8,u8,t];
var_args(l_bs_validate_unicode_retract, 0) -> [f];
var_args(init, 15) -> [{y,16}];
var_args(l_bxor, 0) -> [f,u8,t];
var_args(l_gc_bif3, 0) -> [f,b,t,u8,t];
var_args(l_bs_match_string, 2) -> [t,f,u32,str];
var_args(l_bs_get_integer_imm, 1) -> [t,u32,u8,f,u8,t];
var_args(l_bs_get_integer_8, 2) -> [t,f,t];
var_args(l_bor, 2) -> [f,u8,t];
var_args(l_bs_start_match2, 3) -> [t,f,u8,u32,t];
var_args(l_new_bs_put_binary_imm, 0) -> [f,u32,u8,t];
var_args(l_rem, 2) -> [f,u8,t];
var_args(l_bs_get_integer_small_imm, 2) -> [t,u32,f,u8,t];
var_args(l_bsl, 2) -> [f,u8,t];
var_args(on_load, 0) -> [];
var_args(move2, 10) -> [t,t,t,t];
var_args(l_int_div, 2) -> [f,u8,t];
var_args(l_bs_test_unit, 0) -> [f,t,u8];
var_args(l_select_val_atoms, 3) -> [t,f,u32];
var_args(l_m_div, 0) -> [f,u8,t];
var_args(l_hibernate, 0) -> [];
var_args(l_apply_fun_last, 0) -> [u32];
var_args(l_select_val_smallints, 2) -> [t,f,u32];
var_args(update_map_assoc, 0) -> [t,t,u32];
var_args(get_map_elements, 0) -> [f,t,u32];
var_args(has_map_fields, 0) -> [f,t,u32];
var_args(is_function2, 0) -> [f,t,t];
var_args(l_apply_only, 0) -> [];
var_args(l_apply_fun_only, 0) -> [];
var_args(l_band, 2) -> [f,u8,t];
var_args(update_map_exact, 0) -> [f,t,t,u32];
var_args(get_map_element, 0) -> [f,t,t,t];
var_args(has_map_field, 0) -> [f,t,t];
var_args(is_bigint, 0) -> [f,t];
var_args(test_heap, 1) -> [u32,u8];
var_args(func_info, 0) -> [t,t,u8];
var_args(call_bif, 0) -> [{b,{erlang,purge_module,1}}];
var_args(l_bs_get_utf16, 2) -> [t,f,u8,t];
var_args(l_put_tuple, 9) -> [t,u32];
var_args(get_tuple_element, 11) -> [t,u32,t];
var_args(allocate_init, 1) -> [u32,t];
var_args(l_call_fun_last, 1) -> [u8,u32];
var_args(set_tuple_element, 2) -> [t,t,u32];
var_args(l_bsr, 2) -> [f,u8,t];
var_args(l_bs_get_integer_32, 3) -> [t,f,u8,t];
var_args(allocate_heap, 1) -> [u32,u32,u8];
var_args(is_tuple_of_arity, 4) -> [f,t,u32];
var_args(test_arity, 4) -> [f,t,u32];
var_args(is_nonempty_list_allocate, 2) -> [f,t,u32];
var_args(l_bs_append, 2) -> [f,u32,u8,u8,t];
var_args(try_case_end, 1) -> [t];
var_args(init3, 1) -> [t,t,t];
var_args(l_select_tuple_arity2, 3) -> [t,f,u32,f,u32,f];
var_args(init2, 1) -> [t,t];
var_args(l_bs_get_binary_all2, 2) -> [f,t,u8,u8,t];
var_args(is_nonempty_list_test_heap, 1) -> [f,u32,u8];
var_args(allocate_heap_zero, 1) -> [u32,u32,u8];
var_args(l_bs_init_heap_bin, 1) -> [u32,u32,u8,t];
var_args(l_plus, 3) -> [f,u8,t];
var_args(l_bs_get_integer, 1) -> [f,u8,u8,u8,t];

var_args(Op, No) -> erlang:error({novar,Op,No}).

var_index(move, 0) -> 0;
var_index(move, 1) -> 1;
var_index(test_heap, 0) -> 2;
var_index(l_call, 0) -> 3;
var_index(move, 2) -> 4;
var_index(move, 3) -> 5;
var_index(l_put_tuple, 0) -> 6;
var_index(get_tuple_element, 0) -> 7;
var_index(move2, 1) -> 8;
var_index(badmatch, 0) -> 9;
var_index(put_list, 0) -> 10;
var_index(is_tuple_of_arity, 1) -> 11;
var_index(get_tuple_element, 1) -> 12;
var_index(move2, 0) -> 13;
var_index(l_call_only, 0) -> 14;
var_index(move, 4) -> 15;
var_index(l_is_eq_exact_immed, 0) -> 16;
var_index(l_is_eq_exact_immed, 1) -> 17;
var_index(is_tuple_of_arity, 0) -> 18;
var_index(put_list, 1) -> 19;
var_index(get_list, 0) -> 20;
var_index(l_put_tuple, 1) -> 21;
var_index(l_is_ge, 0) -> 22;
var_index(l_call_ext, 102) -> 23;
var_index(l_make_fun, 0) -> 24;
var_index(return, 0) -> 25;
var_index(is_tuple_of_arity, 2) -> 26;
var_index(l_move_call_last, 0) -> 27;
var_index(extract_next_element2, 0) -> 28;
var_index(l_fetch, 0) -> 29;
var_index(call_bif, 3) -> 30;
var_index(move_deallocate_return, 0) -> 31;
var_index(l_trim, 0) -> 32;
var_index(l_allocate, 0) -> 33;
var_index(move2, 3) -> 34;
var_index(put_list, 2) -> 35;
var_index(is_nil, 0) -> 36;
var_index(l_fetch, 1) -> 37;
var_index(is_nonempty_list, 0) -> 38;
var_index(move_return, 29) -> 39;
var_index(l_allocate, 1) -> 40;
var_index(extract_next_element, 0) -> 41;
var_index(deallocate_return, 0) -> 42;
var_index(case_end, 0) -> 43;
var_index(l_is_eq_exact_immed, 2) -> 44;
var_index(jump, 0) -> 45;
var_index(get_list, 1) -> 46;
var_index(call_bif, 7) -> 47;
var_index(move2, 2) -> 48;
var_index(get_tuple_element, 2) -> 49;
var_index(l_is_eq_exact, 0) -> 50;
var_index(l_move_call, 34) -> 51;
var_index(l_allocate, 2) -> 52;
var_index(move_return, 0) -> 53;
var_index(l_is_eq_exact_immed, 3) -> 54;
var_index(get_tuple_element, 3) -> 55;
var_index(l_fetch, 2) -> 56;
var_index(move, 5) -> 57;
var_index(init2, 0) -> 58;
var_index(init, 0) -> 59;
var_index(l_is_eq_exact_immed, 4) -> 60;
var_index(deallocate_return, 1) -> 61;
var_index(call_bif, 8) -> 62;
var_index(put_list, 3) -> 63;
var_index(extract_next_element, 1) -> 64;
var_index(move2, 4) -> 65;
var_index(is_tuple_of_arity, 3) -> 66;
var_index(l_fetch, 3) -> 67;
var_index(get_tuple_element, 4) -> 68;
var_index(l_allocate, 3) -> 69;
var_index(init, 1) -> 70;
var_index(move2, 5) -> 71;
var_index(deallocate_return, 2) -> 72;
var_index(l_put_tuple, 2) -> 73;
var_index(l_fetch, 4) -> 74;
var_index(l_move_call_ext, 44) -> 75;
var_index(is_nonempty_list, 1) -> 76;
var_index(l_is_eq_exact_immed, 5) -> 77;
var_index(l_trim, 1) -> 78;
var_index(init3, 0) -> 79;
var_index(l_allocate_zero, 0) -> 80;
var_index(allocate_init, 0) -> 81;
var_index(call_bif, 6) -> 82;
var_index(test_heap_1_put_list, 0) -> 83;
var_index(allocate_heap, 0) -> 84;
var_index(l_allocate_zero, 1) -> 85;
var_index(init, 2) -> 86;
var_index(get_list, 2) -> 87;
var_index(move_deallocate_return, 1) -> 88;
var_index(call_bif, 5) -> 89;
var_index(l_move_call, 0) -> 90;
var_index(call_bif, 48) -> 91;
var_index(l_call_last, 0) -> 92;
var_index(move_return, 1) -> 93;
var_index(l_is_eq_exact_literal, 0) -> 94;
var_index(is_nonempty_list, 2) -> 95;
var_index(is_tuple, 0) -> 96;
var_index(is_nonempty_list_allocate, 0) -> 97;
var_index(deallocate_return, 3) -> 98;
var_index(l_is_eq_exact_immed, 6) -> 99;
var_index(l_call_last, 1) -> 100;
var_index(is_list, 0) -> 101;
var_index(l_move_call_only, 0) -> 102;
var_index(l_select_val_atoms, 0) -> 103;
var_index(extract_next_element, 2) -> 104;
var_index(get_list, 3) -> 105;
var_index(l_trim, 2) -> 106;
var_index(l_call_last, 2) -> 107;
var_index(l_increment, 0) -> 108;
var_index(move, 6) -> 109;
var_index(move_deallocate_return, 2) -> 110;
var_index(l_is_lt, 0) -> 111;
var_index(is_nonempty_list, 3) -> 112;
var_index(l_select_val2, 3) -> 113;
var_index(l_select_val2, 0) -> 114;
var_index(l_select_val2, 4) -> 115;
var_index(init, 3) -> 116;
var_index(is_nil, 1) -> 117;
var_index(l_move_call_only, 1) -> 118;
var_index(extract_next_element2, 1) -> 119;
var_index(l_bs_start_match2, 0) -> 120;
var_index(l_move_call_only, 9) -> 121;
var_index(extract_next_element3, 0) -> 122;
var_index(remove_message, 0) -> 123;
var_index(get_tuple_element, 5) -> 124;
var_index(l_fetch, 5) -> 125;
var_index(l_new_bs_put_integer_imm, 0) -> 126;
var_index(l_catch, 0) -> 127;
var_index(l_allocate_zero, 2) -> 128;
var_index(move_return, 2) -> 129;
var_index(l_call_ext, 0) -> 130;
var_index(l_allocate, 4) -> 131;
var_index(move2, 6) -> 132;
var_index(extract_next_element, 3) -> 133;
var_index(l_select_val2, 1) -> 134;
var_index(test_arity, 0) -> 135;
var_index(is_nil, 2) -> 136;
var_index(move_jump, 13) -> 137;
var_index(l_allocate_zero, 3) -> 138;
var_index(catch_end, 0) -> 139;
var_index(deallocate_return, 4) -> 140;
var_index(l_is_eq_exact_immed, 7) -> 141;
var_index(move_return, 3) -> 142;
var_index(move_return, 4) -> 143;
var_index(l_move_call_ext, 0) -> 144;
var_index(put_list, 4) -> 145;
var_index(l_put_tuple, 3) -> 146;
var_index(move, 7) -> 147;
var_index(move2, 7) -> 148;
var_index(l_is_eq_exact_immed, 8) -> 149;
var_index(call_bif, 2) -> 150;
var_index(l_select_tuple_arity2, 0) -> 151;
var_index(is_nonempty_list, 4) -> 152;
var_index(l_plus, 0) -> 153;
var_index(l_call_last, 3) -> 154;
var_index(is_nonempty_list, 5) -> 155;
var_index(get_list, 4) -> 156;
var_index(init, 4) -> 157;
var_index(move_return, 5) -> 158;
var_index(l_move_call_only, 2) -> 159;
var_index(l_increment, 3) -> 160;
var_index(is_nonempty_list, 6) -> 161;
var_index(l_bs_get_binary_all_reuse, 0) -> 162;
var_index(l_call_last, 4) -> 163;
var_index(bif1_body, 0) -> 164;
var_index(l_gc_bif1, 0) -> 165;
var_index(l_call_fun, 0) -> 166;
var_index(test_arity, 1) -> 167;
var_index(set_tuple_element, 0) -> 168;
var_index(l_move_call, 1) -> 169;
var_index(l_bs_match_string, 0) -> 170;
var_index(l_move_call_last, 1) -> 171;
var_index(l_bs_test_zero_tail2, 0) -> 172;
var_index(call_bif, 9) -> 173;
var_index(send, 0) -> 174;
var_index(l_is_eq, 0) -> 175;
var_index(l_move_call_only, 3) -> 176;
var_index(l_catch, 1) -> 177;
var_index(l_is_ne, 0) -> 178;
var_index(l_bs_add, 0) -> 179;
var_index(l_select_val2, 2) -> 180;
var_index(l_move_call_ext, 1) -> 181;
var_index(l_call_ext_only, 4) -> 182;
var_index(l_select_val_smallints, 0) -> 183;
var_index(bif2_body, 0) -> 184;
var_index(is_nonempty_list, 7) -> 185;
var_index(l_is_ne_exact_immed, 0) -> 186;
var_index(l_is_eq_exact_immed, 9) -> 187;
var_index(l_move_call_ext_last, 0) -> 188;
var_index(extract_next_element2, 2) -> 189;
var_index(case_end, 1) -> 190;
var_index(extract_next_element2, 3) -> 191;
var_index(extract_next_element, 4) -> 192;
var_index(l_move_call, 2) -> 193;
var_index(l_select_val_atoms, 1) -> 194;
var_index(move_deallocate_return, 3) -> 195;
var_index(l_move_call_ext, 2) -> 196;
var_index(l_loop_rec, 0) -> 197;
var_index(l_move_call, 3) -> 198;
var_index(l_select_tuple_arity2, 1) -> 199;
var_index(is_nil, 3) -> 200;
var_index(l_trim, 3) -> 201;
var_index(l_bs_get_utf16, 0) -> 202;
var_index(l_bs_restore2, 0) -> 203;
var_index(init, 5) -> 204;
var_index(l_move_call, 4) -> 205;
var_index(catch_end, 1) -> 206;
var_index(is_nonempty_list, 8) -> 207;
var_index(is_nonempty_list, 9) -> 208;
var_index(is_nil, 4) -> 209;
var_index(l_call_ext_last, 0) -> 210;
var_index(l_fetch, 7) -> 211;
var_index(deallocate_return, 5) -> 212;
var_index(l_bs_init_heap_bin, 0) -> 213;
var_index(l_allocate_zero, 4) -> 214;
var_index(wait, 0) -> 215;
var_index(badmatch, 1) -> 216;
var_index(call_bif, 10) -> 217;
var_index(loop_rec_end, 0) -> 218;
var_index(l_allocate, 5) -> 219;
var_index(extract_next_element, 5) -> 220;
var_index(l_move_call_ext, 3) -> 221;
var_index(move, 8) -> 222;
var_index(l_select_val2, 5) -> 223;
var_index(l_allocate_zero, 5) -> 224;
var_index(l_bs_start_match2, 1) -> 225;
var_index(l_move_call_ext, 4) -> 226;
var_index(l_move_call_ext, 5) -> 227;
var_index(badmatch, 2) -> 228;
var_index(l_call_last, 5) -> 229;
var_index(extract_next_element2, 4) -> 230;
var_index(l_increment, 1) -> 231;
var_index(l_call_fun_last, 0) -> 232;
var_index(l_plus, 1) -> 233;
var_index(init, 6) -> 234;
var_index(l_move_call_ext, 6) -> 235;
var_index(l_gc_bif1, 1) -> 236;
var_index(l_call_last, 6) -> 237;
var_index(l_call_ext, 1) -> 238;
var_index(l_trim, 4) -> 239;
var_index(bs_context_to_binary, 0) -> 240;
var_index(put_list, 5) -> 241;
var_index(l_bs_save2, 0) -> 242;
var_index(move_deallocate_return, 4) -> 243;
var_index(l_increment, 2) -> 244;
var_index(extract_next_element, 6) -> 245;
var_index(call_bif, 11) -> 246;
var_index(int_code_end, 0) -> 247;
var_index(l_move_call_ext_only, 0) -> 248;
var_index(call_bif, 12) -> 249;
var_index(get_tuple_element, 6) -> 250;
var_index(l_is_ne_exact_immed, 1) -> 251;
var_index(l_is_eq_exact_immed, 11) -> 252;
var_index(call_bif, 14) -> 253;
var_index(call_bif, 13) -> 254;
var_index(l_move_call, 5) -> 255;
var_index(l_put_tuple, 4) -> 256;
var_index(l_bs_test_zero_tail2, 1) -> 257;
var_index(self, 0) -> 258;
var_index(extract_next_element, 7) -> 259;
var_index(is_nil, 5) -> 260;
var_index(is_nonempty_list_allocate, 1) -> 261;
var_index(l_move_call, 6) -> 262;
var_index(badmatch, 3) -> 263;
var_index(is_tuple, 1) -> 264;
var_index(l_select_val2, 7) -> 265;
var_index(l_move_call_last, 2) -> 266;
var_index(l_move_call_ext, 7) -> 267;
var_index(l_bs_test_zero_tail2, 2) -> 268;
var_index(l_gc_bif1, 3) -> 269;
var_index(is_atom, 0) -> 270;
var_index(is_nil, 6) -> 271;
var_index(is_nonempty_list, 10) -> 272;
var_index(l_call_ext_last, 1) -> 273;
var_index(l_select_val_smallints, 1) -> 274;
var_index(l_fast_element, 0) -> 275;
var_index(extract_next_element, 8) -> 276;
var_index(l_bs_match_string, 1) -> 277;
var_index(is_nonempty_list, 11) -> 278;
var_index(l_catch, 2) -> 279;
var_index(l_gc_bif1, 4) -> 280;
var_index(l_call_ext, 2) -> 281;
var_index(l_move_call, 7) -> 282;
var_index(l_bs_test_unit_8, 0) -> 283;
var_index(call_bif, 16) -> 284;
var_index(call_bif, 15) -> 285;
var_index(raise, 0) -> 286;
var_index(l_call_ext, 3) -> 287;
var_index(is_nil, 7) -> 288;
var_index(extract_next_element3, 1) -> 289;
var_index(move_return, 6) -> 290;
var_index(l_call_ext_last, 2) -> 291;
var_index(l_move_call_ext, 8) -> 292;
var_index(call_bif, 17) -> 293;
var_index(allocate_heap_zero, 0) -> 294;
var_index(call_bif, 18) -> 295;
var_index(l_allocate, 6) -> 296;
var_index(case_end, 2) -> 297;
var_index(deallocate_return, 6) -> 298;
var_index(l_is_eq_exact_literal, 7) -> 299;
var_index(self, 1) -> 300;
var_index(get_list, 7) -> 301;
var_index(init, 7) -> 302;
var_index(l_minus, 0) -> 303;
var_index(l_move_call_ext_only, 1) -> 304;
var_index(call_bif, 19) -> 305;
var_index(extract_next_element2, 5) -> 306;
var_index(l_call_fun, 1) -> 307;
var_index(catch_end, 2) -> 308;
var_index(l_move_call_ext_only, 7) -> 309;
var_index(call_bif, 20) -> 310;
var_index(extract_next_element, 9) -> 311;
var_index(l_move_call_last, 3) -> 312;
var_index(l_call_ext, 4) -> 313;
var_index(try_end, 0) -> 314;
var_index(l_move_call_ext, 9) -> 315;
var_index(l_band, 0) -> 316;
var_index(l_trim, 5) -> 317;
var_index(l_bif2, 0) -> 318;
var_index(l_move_call, 8) -> 319;
var_index(is_nil, 8) -> 320;
var_index(l_fetch, 6) -> 321;
var_index(l_call_ext, 5) -> 322;
var_index(test_heap_1_put_list, 1) -> 323;
var_index(l_is_eq_exact_immed, 12) -> 324;
var_index(l_call_ext, 6) -> 325;
var_index(try_end, 1) -> 326;
var_index(l_gc_bif1, 2) -> 327;
var_index(l_bs_get_binary2, 0) -> 328;
var_index(l_minus, 1) -> 329;
var_index(l_bs_get_binary_all2, 0) -> 330;
var_index(l_is_eq_exact_immed, 13) -> 331;
var_index(is_integer, 0) -> 332;
var_index(get_tuple_element, 7) -> 333;
var_index(extract_next_element2, 6) -> 334;
var_index(l_move_call_only, 4) -> 335;
var_index(l_is_eq_exact_literal, 1) -> 336;
var_index(l_catch, 3) -> 337;
var_index(move_jump, 0) -> 338;
var_index(is_list, 1) -> 339;
var_index(extract_next_element2, 7) -> 340;
var_index(l_bsr, 0) -> 341;
var_index(l_bif2, 1) -> 342;
var_index(l_move_call, 9) -> 343;
var_index(l_call_ext, 8) -> 344;
var_index(l_call_ext, 7) -> 345;
var_index(apply, 0) -> 346;
var_index(l_is_eq_exact_immed, 14) -> 347;
var_index(set_tuple_element, 1) -> 348;
var_index(l_bs_test_zero_tail2, 3) -> 349;
var_index(move_deallocate_return, 5) -> 350;
var_index(l_bif2, 2) -> 351;
var_index(l_times, 0) -> 352;
var_index(l_is_eq_exact_literal, 2) -> 353;
var_index(l_fast_element, 2) -> 354;
var_index(l_fetch, 8) -> 355;
var_index(l_is_eq_exact_immed, 10) -> 356;
var_index(l_is_eq_exact_literal, 3) -> 357;
var_index(l_fmul, 0) -> 358;
var_index(l_move_call_ext, 10) -> 359;
var_index(call_bif, 21) -> 360;
var_index(extract_next_element, 27) -> 361;
var_index(extract_next_element, 10) -> 362;
var_index(is_nil, 9) -> 363;
var_index(l_times, 1) -> 364;
var_index(l_move_call_ext, 11) -> 365;
var_index(l_allocate_zero, 6) -> 366;
var_index(l_bs_init_fail, 0) -> 367;
var_index(l_bs_get_integer_8, 0) -> 368;
var_index(test_arity, 2) -> 369;
var_index(extract_next_element3, 2) -> 370;
var_index(is_nonempty_list, 12) -> 371;
var_index(l_is_eq_exact_immed, 15) -> 372;
var_index(l_call_ext, 9) -> 373;
var_index(l_bsl, 0) -> 374;
var_index(l_move_call_ext_only, 2) -> 375;
var_index(l_bs_get_integer_8, 1) -> 376;
var_index(l_put_tuple, 6) -> 377;
var_index(is_tuple, 2) -> 378;
var_index(l_is_ne_exact, 0) -> 379;
var_index(l_select_val2, 6) -> 380;
var_index(move, 9) -> 381;
var_index(l_move_call_ext, 12) -> 382;
var_index(l_call_last, 7) -> 383;
var_index(l_call_fun, 2) -> 384;
var_index(node, 0) -> 385;
var_index(is_binary, 0) -> 386;
var_index(l_select_val2, 9) -> 387;
var_index(l_is_ne_exact_immed, 2) -> 388;
var_index(l_new_bs_put_binary_all, 0) -> 389;
var_index(l_bs_test_zero_tail2, 5) -> 390;
var_index(l_bif2, 3) -> 391;
var_index(extract_next_element3, 3) -> 392;
var_index(l_move_call_ext_last, 1) -> 393;
var_index(get_list, 5) -> 394;
var_index(l_bif2, 4) -> 395;
var_index(l_bs_get_integer_32, 0) -> 396;
var_index(call_bif, 22) -> 397;
var_index(l_call_ext, 10) -> 398;
var_index(l_new_bs_put_binary_all, 1) -> 399;
var_index(catch_end, 3) -> 400;
var_index(extract_next_element, 11) -> 401;
var_index(l_fcheckerror, 0) -> 402;
var_index(fclearerror, 0) -> 403;
var_index(is_atom, 1) -> 404;
var_index(l_is_eq_exact_immed, 16) -> 405;
var_index(l_allocate_zero, 9) -> 406;
var_index(extract_next_element, 12) -> 407;
var_index(extract_next_element3, 4) -> 408;
var_index(get_list, 6) -> 409;
var_index(l_bs_append, 0) -> 410;
var_index(l_is_eq_exact_immed, 17) -> 411;
var_index(node, 1) -> 412;
var_index(deallocate_return, 7) -> 413;
var_index(l_bs_get_integer_32, 1) -> 414;
var_index(timeout, 0) -> 415;
var_index(l_is_eq_exact_immed, 36) -> 416;
var_index(l_move_call_ext, 14) -> 417;
var_index(l_fetch, 9) -> 418;
var_index(l_move_call, 10) -> 419;
var_index(l_move_call_ext, 15) -> 420;
var_index(is_float, 1) -> 421;
var_index(l_rem, 0) -> 422;
var_index(l_get, 0) -> 423;
var_index(move_deallocate_return, 6) -> 424;
var_index(is_nonempty_list, 39) -> 425;
var_index(l_fetch, 10) -> 426;
var_index(call_bif, 23) -> 427;
var_index(l_call_ext, 11) -> 428;
var_index(l_fast_element, 1) -> 429;
var_index(l_move_call, 11) -> 430;
var_index(l_band, 1) -> 431;
var_index(l_allocate_zero, 7) -> 432;
var_index(is_nonempty_list, 13) -> 433;
var_index(is_binary, 1) -> 434;
var_index(l_select_tuple_arity, 1) -> 435;
var_index(l_fetch, 11) -> 436;
var_index(l_call_ext, 12) -> 437;
var_index(is_nonempty_list, 14) -> 438;
var_index(call_bif, 24) -> 439;
var_index(extract_next_element, 13) -> 440;
var_index(l_trim, 6) -> 441;
var_index(is_nil, 10) -> 442;
var_index(l_move_call, 12) -> 443;
var_index(l_get, 1) -> 444;
var_index(l_is_eq_exact_immed, 18) -> 445;
var_index(is_nil, 11) -> 446;
var_index(l_select_val2, 10) -> 447;
var_index(l_bs_get_integer_small_imm, 0) -> 448;
var_index(l_bsl, 1) -> 449;
var_index(l_get, 2) -> 450;
var_index(l_call_ext, 13) -> 451;
var_index(l_call_last, 8) -> 452;
var_index(l_is_eq_exact_immed, 19) -> 453;
var_index(l_move_call_last, 4) -> 454;
var_index(l_bor, 0) -> 455;
var_index(l_allocate, 7) -> 456;
var_index(l_call_ext, 14) -> 457;
var_index(is_integer, 1) -> 458;
var_index(is_nil, 30) -> 459;
var_index(is_nil, 13) -> 460;
var_index(is_nil, 12) -> 461;
var_index(l_move_call_only, 5) -> 462;
var_index(l_move_call, 13) -> 463;
var_index(move2, 8) -> 464;
var_index(l_gc_bif1, 5) -> 465;
var_index(call_bif, 25) -> 466;
var_index(move_return, 7) -> 467;
var_index(is_list, 2) -> 468;
var_index(l_call_ext, 15) -> 469;
var_index(l_move_call_ext, 17) -> 470;
var_index(l_move_call_ext, 16) -> 471;
var_index(l_catch, 4) -> 472;
var_index(call_bif, 27) -> 473;
var_index(call_bif, 26) -> 474;
var_index(extract_next_element2, 8) -> 475;
var_index(is_nonempty_list, 15) -> 476;
var_index(l_move_call_ext, 18) -> 477;
var_index(l_select_val2, 17) -> 478;
var_index(l_element, 1) -> 479;
var_index(l_fadd, 0) -> 480;
var_index(extract_next_element3, 11) -> 481;
var_index(l_put_tuple, 5) -> 482;
var_index(bif1_body, 1) -> 483;
var_index(call_bif, 28) -> 484;
var_index(move_jump, 1) -> 485;
var_index(l_is_ne_exact_immed, 3) -> 486;
var_index(call_bif, 29) -> 487;
var_index(l_call_ext, 16) -> 488;
var_index(l_select_val2, 8) -> 489;
var_index(is_nonempty_list, 16) -> 490;
var_index(fmove_1, 0) -> 491;
var_index(l_fetch, 12) -> 492;
var_index(extract_next_element2, 18) -> 493;
var_index(l_bs_get_utf8, 0) -> 494;
var_index(is_atom, 2) -> 495;
var_index(bif1_body, 2) -> 496;
var_index(l_is_ne_exact_immed, 11) -> 497;
var_index(l_call_ext_last, 3) -> 498;
var_index(l_move_call_ext, 19) -> 499;
var_index(bif2_body, 1) -> 500;
var_index(l_move_call_ext, 20) -> 501;
var_index(call_bif, 30) -> 502;
var_index(l_call_ext, 17) -> 503;
var_index(l_is_eq_exact_immed, 20) -> 504;
var_index(l_bs_skip_bits_all2, 0) -> 505;
var_index(call_bif, 31) -> 506;
var_index(case_end, 10) -> 507;
var_index(l_select_tuple_arity, 0) -> 508;
var_index(l_move_call_ext, 21) -> 509;
var_index(l_is_eq_exact_immed, 21) -> 510;
var_index(fconv, 0) -> 511;
var_index(l_catch, 5) -> 512;
var_index(put_list, 8) -> 513;
var_index(l_move_call_ext_last, 2) -> 514;
var_index(call_bif, 4) -> 515;
var_index(extract_next_element3, 5) -> 516;
var_index(put_list, 6) -> 517;
var_index(l_is_eq_exact_literal, 4) -> 518;
var_index(l_select_tuple_arity, 2) -> 519;
var_index(system_limit, 0) -> 520;
var_index(l_call_ext_last, 4) -> 521;
var_index(l_fetch, 23) -> 522;
var_index(extract_next_element, 14) -> 523;
var_index(is_binary, 2) -> 524;
var_index(l_bor, 1) -> 525;
var_index(call_bif, 32) -> 526;
var_index(l_call_ext, 18) -> 527;
var_index(is_integer_allocate, 0) -> 528;
var_index(l_is_eq_exact_immed, 23) -> 529;
var_index(l_move_call_ext_only, 3) -> 530;
var_index(badmatch, 4) -> 531;
var_index(l_call_fun, 3) -> 532;
var_index(l_bs_init_bits_fail, 0) -> 533;
var_index(l_make_export, 0) -> 534;
var_index(is_integer, 6) -> 535;
var_index(call_bif, 34) -> 536;
var_index(call_bif, 33) -> 537;
var_index(apply_last, 0) -> 538;
var_index(fmove_2, 0) -> 539;
var_index(fmove_2, 1) -> 540;
var_index(fmove_1, 1) -> 541;
var_index(call_bif, 35) -> 542;
var_index(l_call_ext, 19) -> 543;
var_index(extract_next_element, 15) -> 544;
var_index(try_end, 2) -> 545;
var_index(deallocate_return, 8) -> 546;
var_index(l_fetch, 13) -> 547;
var_index(l_fast_element, 3) -> 548;
var_index(l_move_call_ext, 22) -> 549;
var_index(is_tuple, 3) -> 550;
var_index(l_bs_restore2, 1) -> 551;
var_index(l_move_call_ext, 23) -> 552;
var_index(l_bs_test_zero_tail2, 4) -> 553;
var_index(l_new_bs_put_integer, 0) -> 554;
var_index(extract_next_element, 16) -> 555;
var_index(l_trim, 7) -> 556;
var_index(is_list, 6) -> 557;
var_index(is_nonempty_list, 17) -> 558;
var_index(l_fetch, 14) -> 559;
var_index(extract_next_element2, 9) -> 560;
var_index(is_atom, 3) -> 561;
var_index(l_fetch, 15) -> 562;
var_index(is_integer, 2) -> 563;
var_index(l_move_call, 15) -> 564;
var_index(l_move_call, 14) -> 565;
var_index(l_call_ext, 21) -> 566;
var_index(l_call_ext, 20) -> 567;
var_index(try_end, 3) -> 568;
var_index(l_move_call_ext, 13) -> 569;
var_index(l_call_ext_last, 6) -> 570;
var_index(case_end, 3) -> 571;
var_index(is_nonempty_list, 18) -> 572;
var_index(try_end, 4) -> 573;
var_index(init, 8) -> 574;
var_index(if_end, 0) -> 575;
var_index(move_deallocate_return, 7) -> 576;
var_index(call_bif, 39) -> 577;
var_index(call_bif, 38) -> 578;
var_index(call_bif, 37) -> 579;
var_index(call_bif, 36) -> 580;
var_index(try_end, 5) -> 581;
var_index(is_nonempty_list_test_heap, 0) -> 582;
var_index(l_move_call_ext, 24) -> 583;
var_index(l_move_call_ext_last, 3) -> 584;
var_index(l_call_ext, 25) -> 585;
var_index(l_call_ext, 24) -> 586;
var_index(l_call_ext, 23) -> 587;
var_index(l_call_ext, 22) -> 588;
var_index(l_get, 3) -> 589;
var_index(put_list, 9) -> 590;
var_index(self, 2) -> 591;
var_index(l_call_ext, 28) -> 592;
var_index(l_call_ext, 27) -> 593;
var_index(l_call_ext, 26) -> 594;
var_index(extract_next_element, 17) -> 595;
var_index(is_list, 3) -> 596;
var_index(catch_end, 4) -> 597;
var_index(l_move_call_last, 5) -> 598;
var_index(l_is_ne_exact_immed, 4) -> 599;
var_index(is_tuple, 4) -> 600;
var_index(extract_next_element2, 10) -> 601;
var_index(move_jump, 2) -> 602;
var_index(l_bs_skip_bits_imm2, 0) -> 603;
var_index(l_fdiv, 0) -> 604;
var_index(get_tuple_element, 8) -> 605;
var_index(l_bif1, 0) -> 606;
var_index(l_bs_get_utf16, 1) -> 607;
var_index(extract_next_element, 18) -> 608;
var_index(is_integer, 3) -> 609;
var_index(l_bs_test_unit_8, 3) -> 610;
var_index(is_nonempty_list, 19) -> 611;
var_index(l_bif1, 1) -> 612;
var_index(is_tuple, 10) -> 613;
var_index(bif1_body, 3) -> 614;
var_index(get_list, 8) -> 615;
var_index(l_select_val2, 11) -> 616;
var_index(badmatch, 5) -> 617;
var_index(l_move_call, 16) -> 618;
var_index(l_is_eq_exact_immed, 24) -> 619;
var_index(l_call_ext, 30) -> 620;
var_index(l_call_ext, 29) -> 621;
var_index(l_is_eq_exact_immed, 25) -> 622;
var_index(l_move_call_ext, 25) -> 623;
var_index(l_select_val_atoms, 2) -> 624;
var_index(extract_next_element2, 11) -> 625;
var_index(move_return, 8) -> 626;
var_index(case_end, 4) -> 627;
var_index(self, 5) -> 628;
var_index(l_call_ext, 31) -> 629;
var_index(extract_next_element3, 6) -> 630;
var_index(l_bs_get_binary_imm2, 0) -> 631;
var_index(l_bs_test_unit_8, 1) -> 632;
var_index(is_list, 4) -> 633;
var_index(l_bs_restore2, 2) -> 634;
var_index(is_atom, 6) -> 635;
var_index(l_move_call_ext_last, 5) -> 636;
var_index(bif1_body, 5) -> 637;
var_index(call_bif, 41) -> 638;
var_index(call_bif, 40) -> 639;
var_index(is_nil, 14) -> 640;
var_index(l_move_call_only, 6) -> 641;
var_index(move_deallocate_return, 8) -> 642;
var_index(init, 9) -> 643;
var_index(l_plus, 2) -> 644;
var_index(test_arity, 3) -> 645;
var_index(l_get, 6) -> 646;
var_index(l_rem, 1) -> 647;
var_index(l_call_ext, 32) -> 648;
var_index(is_nil, 15) -> 649;
var_index(case_end, 5) -> 650;
var_index(l_move_call_ext, 27) -> 651;
var_index(l_move_call_ext, 28) -> 652;
var_index(call_bif, 42) -> 653;
var_index(l_call_ext, 33) -> 654;
var_index(is_nil, 16) -> 655;
var_index(l_fsub, 0) -> 656;
var_index(l_is_eq_exact_literal, 5) -> 657;
var_index(l_jump_on_val, 0) -> 658;
var_index(l_move_call_ext, 29) -> 659;
var_index(get_tuple_element, 9) -> 660;
var_index(l_move_call_last, 6) -> 661;
var_index(call_bif, 43) -> 662;
var_index(is_integer, 4) -> 663;
var_index(l_bs_put_string, 0) -> 664;
var_index(is_nonempty_list, 20) -> 665;
var_index(l_bif2, 5) -> 666;
var_index(l_move_call_ext, 30) -> 667;
var_index(put_list, 7) -> 668;
var_index(l_move_call_ext, 31) -> 669;
var_index(bif1_body, 6) -> 670;
var_index(l_int_div, 0) -> 671;
var_index(node, 4) -> 672;
var_index(l_bsr, 1) -> 673;
var_index(l_increment, 4) -> 674;
var_index(l_put_tuple, 7) -> 675;
var_index(l_call_ext, 34) -> 676;
var_index(badmatch, 6) -> 677;
var_index(case_end, 6) -> 678;
var_index(l_is_eq_exact_literal, 6) -> 679;
var_index(l_call_ext_only, 0) -> 680;
var_index(move, 10) -> 681;
var_index(l_increment, 5) -> 682;
var_index(l_allocate, 9) -> 683;
var_index(l_call_ext, 35) -> 684;
var_index(l_allocate_zero, 8) -> 685;
var_index(call_bif, 44) -> 686;
var_index(l_call_ext, 36) -> 687;
var_index(l_call_last, 9) -> 688;
var_index(move_return, 9) -> 689;
var_index(is_list, 5) -> 690;
var_index(l_move_call, 17) -> 691;
var_index(catch_end, 5) -> 692;
var_index(l_is_eq_exact_immed, 26) -> 693;
var_index(l_fetch, 16) -> 694;
var_index(l_put_tuple, 8) -> 695;
var_index(l_call_ext, 38) -> 696;
var_index(l_call_ext, 37) -> 697;
var_index(put_list, 10) -> 698;
var_index(l_move_call, 18) -> 699;
var_index(test_heap_1_put_list, 2) -> 700;
var_index(get_list, 9) -> 701;
var_index(l_bs_skip_bits2, 0) -> 702;
var_index(l_fetch, 17) -> 703;
var_index(call_bif, 45) -> 704;
var_index(node, 2) -> 705;
var_index(extract_next_element2, 12) -> 706;
var_index(is_nil, 17) -> 707;
var_index(try_case_end, 0) -> 708;
var_index(l_yield, 0) -> 709;
var_index(l_bs_get_binary_all2, 1) -> 710;
var_index(is_atom, 4) -> 711;
var_index(l_move_call, 19) -> 712;
var_index(l_call_ext_only, 1) -> 713;
var_index(l_move_call_ext, 33) -> 714;
var_index(l_is_eq_exact_immed, 27) -> 715;
var_index(move_return, 10) -> 716;
var_index(l_move_call_ext, 34) -> 717;
var_index(l_bs_get_integer, 0) -> 718;
var_index(l_bs_append, 1) -> 719;
var_index(l_move_call_ext, 35) -> 720;
var_index(l_bs_get_binary2, 1) -> 721;
var_index(call_bif, 46) -> 722;
var_index(l_is_eq_exact_immed, 22) -> 723;
var_index(extract_next_element, 19) -> 724;
var_index(extract_next_element3, 7) -> 725;
var_index(is_nonempty_list, 22) -> 726;
var_index(is_nonempty_list, 21) -> 727;
var_index(l_move_call, 20) -> 728;
var_index(l_is_ne_exact_immed, 5) -> 729;
var_index(l_fetch, 18) -> 730;
var_index(l_catch, 6) -> 731;
var_index(call_bif, 47) -> 732;
var_index(l_allocate, 8) -> 733;
var_index(l_call_ext, 40) -> 734;
var_index(l_call_ext, 39) -> 735;
var_index(l_is_ne_exact_immed, 6) -> 736;
var_index(badmatch, 7) -> 737;
var_index(case_end, 7) -> 738;
var_index(is_binary, 3) -> 739;
var_index(init, 10) -> 740;
var_index(l_gc_bif1, 6) -> 741;
var_index(l_int_div, 1) -> 742;
var_index(l_is_eq_exact_immed, 29) -> 743;
var_index(l_is_eq_exact_immed, 28) -> 744;
var_index(is_function, 1) -> 745;
var_index(fconv, 1) -> 746;
var_index(fmove_1, 3) -> 747;
var_index(l_call_ext, 42) -> 748;
var_index(l_call_ext, 41) -> 749;
var_index(move_jump, 3) -> 750;
var_index(is_nil, 18) -> 751;
var_index(case_end, 8) -> 752;
var_index(l_select_val2, 12) -> 753;
var_index(l_bs_save2, 1) -> 754;
var_index(l_move_call, 23) -> 755;
var_index(l_move_call, 22) -> 756;
var_index(l_move_call, 21) -> 757;
var_index(l_move_call_ext, 36) -> 758;
var_index(deallocate_return, 12) -> 759;
var_index(bif2_body, 2) -> 760;
var_index(l_move_call_ext, 37) -> 761;
var_index(l_move_call_ext_last, 4) -> 762;
var_index(call_bif, 1) -> 763;
var_index(l_call_ext, 49) -> 764;
var_index(l_call_ext, 48) -> 765;
var_index(l_call_ext, 47) -> 766;
var_index(l_call_ext, 46) -> 767;
var_index(l_call_ext, 45) -> 768;
var_index(l_call_ext, 44) -> 769;
var_index(l_call_ext, 43) -> 770;
var_index(l_move_call_ext_only, 4) -> 771;
var_index(l_is_ne_exact_immed, 7) -> 772;
var_index(l_move_call_ext, 38) -> 773;
var_index(l_gc_bif1, 7) -> 774;
var_index(l_call_ext, 52) -> 775;
var_index(l_call_ext, 51) -> 776;
var_index(l_call_ext, 50) -> 777;
var_index(is_nil, 19) -> 778;
var_index(is_nonempty_list, 23) -> 779;
var_index(l_call_fun, 4) -> 780;
var_index(l_move_call_ext_only, 5) -> 781;
var_index(l_move_call_ext, 39) -> 782;
var_index(is_pid, 1) -> 783;
var_index(bif1_body, 4) -> 784;
var_index(l_call_ext, 55) -> 785;
var_index(l_call_ext, 54) -> 786;
var_index(l_call_ext, 53) -> 787;
var_index(l_trim, 8) -> 788;
var_index(l_bs_test_unit_8, 2) -> 789;
var_index(l_move_call, 24) -> 790;
var_index(l_move_call_ext, 42) -> 791;
var_index(l_move_call_ext, 41) -> 792;
var_index(l_move_call_ext, 40) -> 793;
var_index(l_get, 4) -> 794;
var_index(l_increment, 8) -> 795;
var_index(is_boolean, 0) -> 796;
var_index(l_bs_get_integer_small_imm, 1) -> 797;
var_index(l_call_ext, 57) -> 798;
var_index(l_call_ext, 56) -> 799;
var_index(extract_next_element, 20) -> 800;
var_index(extract_next_element2, 13) -> 801;
var_index(move_jump, 4) -> 802;
var_index(bs_context_to_binary, 1) -> 803;
var_index(l_bs_put_string, 2) -> 804;
var_index(l_call_ext_last, 5) -> 805;
var_index(bif1_body, 7) -> 806;
var_index(init, 11) -> 807;
var_index(l_fetch, 19) -> 808;
var_index(badmatch, 16) -> 809;
var_index(bs_init_writable, 0) -> 810;
var_index(l_call_ext, 58) -> 811;
var_index(l_move_call_only, 7) -> 812;
var_index(put_list, 12) -> 813;
var_index(case_end, 9) -> 814;
var_index(l_bs_private_append, 0) -> 815;
var_index(l_move_call, 25) -> 816;
var_index(l_is_ne_exact_literal, 0) -> 817;
var_index(l_call_ext, 65) -> 818;
var_index(l_call_ext, 64) -> 819;
var_index(l_call_ext, 63) -> 820;
var_index(l_call_ext, 62) -> 821;
var_index(l_call_ext, 61) -> 822;
var_index(l_call_ext, 60) -> 823;
var_index(l_call_ext, 59) -> 824;
var_index(extract_next_element2, 14) -> 825;
var_index(move_jump, 5) -> 826;
var_index(move_return, 11) -> 827;
var_index(badmatch, 9) -> 828;
var_index(badmatch, 8) -> 829;
var_index(l_move_call, 26) -> 830;
var_index(move_deallocate_return, 10) -> 831;
var_index(l_call_ext, 68) -> 832;
var_index(l_call_ext, 67) -> 833;
var_index(l_call_ext, 66) -> 834;
var_index(extract_next_element3, 8) -> 835;
var_index(l_bs_start_match2, 2) -> 836;
var_index(move_jump, 6) -> 837;
var_index(move_return, 12) -> 838;
var_index(put_list, 11) -> 839;
var_index(deallocate_return, 9) -> 840;
var_index(l_wait_timeout, 2) -> 841;
var_index(l_get, 5) -> 842;
var_index(l_element, 0) -> 843;
var_index(l_bs_skip_bits_all2, 1) -> 844;
var_index(l_select_tuple_arity2, 2) -> 845;
var_index(l_is_eq_exact_immed, 30) -> 846;
var_index(l_bif2, 6) -> 847;
var_index(l_call_ext, 73) -> 848;
var_index(l_call_ext, 72) -> 849;
var_index(l_call_ext, 71) -> 850;
var_index(l_call_ext, 70) -> 851;
var_index(l_call_ext, 69) -> 852;
var_index(extract_next_element2, 15) -> 853;
var_index(move_jump, 7) -> 854;
var_index(move_return, 14) -> 855;
var_index(move_return, 13) -> 856;
var_index(l_move_call_only, 8) -> 857;
var_index(is_nonempty_list, 24) -> 858;
var_index(l_move_call, 28) -> 859;
var_index(l_move_call, 27) -> 860;
var_index(l_call_ext_only, 2) -> 861;
var_index(fmove_1, 2) -> 862;
var_index(bif2_body, 3) -> 863;
var_index(l_new_bs_put_integer_imm, 1) -> 864;
var_index(self, 3) -> 865;
var_index(node, 3) -> 866;
var_index(l_call_ext, 76) -> 867;
var_index(l_call_ext, 75) -> 868;
var_index(l_call_ext, 74) -> 869;
var_index(move_return, 15) -> 870;
var_index(is_nonempty_list, 25) -> 871;
var_index(is_pid, 0) -> 872;
var_index(l_new_bs_put_float_imm, 0) -> 873;
var_index(l_move_call, 30) -> 874;
var_index(l_move_call, 29) -> 875;
var_index(l_move_call_ext, 26) -> 876;
var_index(test_heap_1_put_list, 3) -> 877;
var_index(catch_end, 6) -> 878;
var_index(l_is_ne_exact_immed, 8) -> 879;
var_index(l_move_call_ext_only, 6) -> 880;
var_index(l_catch, 8) -> 881;
var_index(l_call_ext, 77) -> 882;
var_index(extract_next_element3, 9) -> 883;
var_index(bs_context_to_binary, 2) -> 884;
var_index(l_new_bs_put_float_imm, 1) -> 885;
var_index(l_move_call, 31) -> 886;
var_index(l_move_call_last, 7) -> 887;
var_index(l_fetch, 20) -> 888;
var_index(l_is_ne_exact_immed, 9) -> 889;
var_index(init, 16) -> 890;
var_index(l_gc_bif1, 9) -> 891;
var_index(l_fetch, 21) -> 892;
var_index(l_catch, 7) -> 893;
var_index(is_tuple, 5) -> 894;
var_index(l_call_ext, 80) -> 895;
var_index(l_call_ext, 79) -> 896;
var_index(l_call_ext, 78) -> 897;
var_index(l_is_eq_exact_immed, 31) -> 898;
var_index(move_return, 17) -> 899;
var_index(move_return, 16) -> 900;
var_index(is_nil, 20) -> 901;
var_index(recv_mark, 0) -> 902;
var_index(l_bs_get_utf8, 1) -> 903;
var_index(is_nonempty_list, 26) -> 904;
var_index(try_end, 6) -> 905;
var_index(deallocate_return, 10) -> 906;
var_index(l_bs_get_integer_imm, 0) -> 907;
var_index(l_move_call, 32) -> 908;
var_index(catch_end, 7) -> 909;
var_index(l_recv_set, 0) -> 910;
var_index(move2, 9) -> 911;
var_index(l_bs_skip_bits_all2, 2) -> 912;
var_index(l_new_bs_put_integer, 1) -> 913;
var_index(test_heap_1_put_list, 4) -> 914;
var_index(self, 4) -> 915;
var_index(l_call_ext, 84) -> 916;
var_index(l_call_ext, 83) -> 917;
var_index(l_call_ext, 82) -> 918;
var_index(l_call_ext, 81) -> 919;
var_index(move_jump, 8) -> 920;
var_index(l_move_call, 33) -> 921;
var_index(l_apply, 0) -> 922;
var_index(get_list, 10) -> 923;
var_index(l_select_val2, 13) -> 924;
var_index(l_increment, 7) -> 925;
var_index(l_call_ext, 92) -> 926;
var_index(l_call_ext, 91) -> 927;
var_index(l_call_ext, 90) -> 928;
var_index(l_call_ext, 89) -> 929;
var_index(l_call_ext, 88) -> 930;
var_index(l_call_ext, 87) -> 931;
var_index(l_call_ext, 86) -> 932;
var_index(l_call_ext, 85) -> 933;
var_index(l_call_last, 11) -> 934;
var_index(extract_next_element3, 10) -> 935;
var_index(l_bs_get_float2, 0) -> 936;
var_index(move_jump, 9) -> 937;
var_index(move_return, 18) -> 938;
var_index(is_nil, 21) -> 939;
var_index(badmatch, 11) -> 940;
var_index(badmatch, 10) -> 941;
var_index(l_bs_put_string, 1) -> 942;
var_index(is_nonempty_list, 27) -> 943;
var_index(l_select_val2, 14) -> 944;
var_index(l_move_call_ext, 32) -> 945;
var_index(is_float, 0) -> 946;
var_index(l_increment, 6) -> 947;
var_index(get_list, 11) -> 948;
var_index(is_tuple, 6) -> 949;
var_index(l_call_ext, 95) -> 950;
var_index(l_call_ext, 94) -> 951;
var_index(l_call_ext, 93) -> 952;
var_index(extract_next_element, 21) -> 953;
var_index(l_fast_element, 4) -> 954;
var_index(extract_next_element2, 16) -> 955;
var_index(is_integer, 5) -> 956;
var_index(move_return, 20) -> 957;
var_index(move_return, 19) -> 958;
var_index(badmatch, 12) -> 959;
var_index(is_function, 0) -> 960;
var_index(init, 12) -> 961;
var_index(l_gc_bif1, 8) -> 962;
var_index(l_is_eq_exact_immed, 32) -> 963;
var_index(l_select_val2, 15) -> 964;
var_index(l_fetch, 22) -> 965;
var_index(l_call_ext, 101) -> 966;
var_index(l_call_ext, 100) -> 967;
var_index(l_call_ext, 99) -> 968;
var_index(l_call_ext, 98) -> 969;
var_index(l_call_ext, 97) -> 970;
var_index(l_call_ext, 96) -> 971;
var_index(extract_next_element, 22) -> 972;
var_index(bs_context_to_binary, 3) -> 973;
var_index(is_nonempty_list, 28) -> 974;
var_index(get_list, 12) -> 975;
var_index(try_end, 7) -> 976;
var_index(l_minus, 2) -> 977;
var_index(l_times, 2) -> 978;
var_index(l_bif1, 2) -> 979;
var_index(deallocate_return, 11) -> 980;
var_index(l_apply_fun, 0) -> 981;
var_index(l_call_ext_only, 3) -> 982;
var_index(l_bs_get_integer_16, 0) -> 983;
var_index(l_bs_init_bits, 0) -> 984;
var_index(move, 12) -> 985;
var_index(wait_timeout, 0) -> 986;
var_index(l_select_val2, 16) -> 987;
var_index(l_call_last, 10) -> 988;
var_index(is_nonempty_list, 29) -> 989;
var_index(is_atom, 5) -> 990;
var_index(l_bs_get_integer_32, 2) -> 991;
var_index(l_trim, 11) -> 992;
var_index(move_deallocate_return, 9) -> 993;
var_index(is_map, 0) -> 994;
var_index(extract_next_element, 23) -> 995;
var_index(extract_next_element2, 17) -> 996;
var_index(move_jump, 10) -> 997;
var_index(move_return, 21) -> 998;
var_index(l_trim, 9) -> 999;
var_index(is_nil, 22) -> 1000;
var_index(bs_context_to_binary, 4) -> 1001;
var_index(badmatch, 13) -> 1002;
var_index(is_nonempty_list, 30) -> 1003;
var_index(l_new_bs_put_float_imm, 2) -> 1004;
var_index(l_bs_init_fail, 1) -> 1005;
var_index(l_is_ne_exact_immed, 10) -> 1006;
var_index(l_bs_restore2, 3) -> 1007;
var_index(l_fast_element, 5) -> 1008;
var_index(is_bitstr, 0) -> 1009;
var_index(is_integer_allocate, 1) -> 1010;
var_index(catch_end, 8) -> 1011;
var_index(move_jump, 11) -> 1012;
var_index(is_nil, 23) -> 1013;
var_index(is_nonempty_list, 32) -> 1014;
var_index(is_nonempty_list, 31) -> 1015;
var_index(l_move_call_ext, 43) -> 1016;
var_index(l_is_eq_exact_immed, 34) -> 1017;
var_index(l_is_eq_exact_immed, 33) -> 1018;
var_index(l_move_call_last, 8) -> 1019;
var_index(l_bs_test_tail_imm2, 0) -> 1020;
var_index(fconv, 2) -> 1021;
var_index(l_bs_save2, 2) -> 1022;
var_index(l_jump_on_val, 1) -> 1023;
var_index(l_bs_add, 1) -> 1024;
var_index(is_tuple, 8) -> 1025;
var_index(is_tuple, 7) -> 1026;
var_index(move_return, 24) -> 1027;
var_index(move_return, 23) -> 1028;
var_index(move_return, 22) -> 1029;
var_index(bs_context_to_binary, 6) -> 1030;
var_index(badmatch, 14) -> 1031;
var_index(is_nonempty_list, 35) -> 1032;
var_index(is_nonempty_list, 34) -> 1033;
var_index(is_nonempty_list, 33) -> 1034;
var_index(l_wait_timeout, 0) -> 1035;
var_index(l_gc_bif2, 0) -> 1036;
var_index(get_tuple_element, 10) -> 1037;
var_index(l_bs_put_utf16, 0) -> 1038;
var_index(l_new_bs_put_binary, 0) -> 1039;
var_index(l_bs_validate_unicode, 0) -> 1040;
var_index(l_bs_get_binary2, 2) -> 1041;
var_index(is_tuple, 9) -> 1042;
var_index(extract_next_element, 26) -> 1043;
var_index(extract_next_element, 25) -> 1044;
var_index(extract_next_element, 24) -> 1045;
var_index(move_return, 27) -> 1046;
var_index(move_return, 26) -> 1047;
var_index(move_return, 25) -> 1048;
var_index(is_nil, 24) -> 1049;
var_index(bs_context_to_binary, 5) -> 1050;
var_index(move, 11) -> 1051;
var_index(l_bs_utf16_size, 0) -> 1052;
var_index(l_is_eq_exact_immed, 35) -> 1053;
var_index(raise, 1) -> 1054;
var_index(move_jump, 12) -> 1055;
var_index(move_return, 28) -> 1056;
var_index(is_nil, 27) -> 1057;
var_index(is_nil, 26) -> 1058;
var_index(is_nil, 25) -> 1059;
var_index(is_reference, 0) -> 1060;
var_index(l_int_bnot, 0) -> 1061;
var_index(l_bs_get_binary_imm2, 1) -> 1062;
var_index(l_is_function2, 0) -> 1063;
var_index(fmove_2, 2) -> 1064;
var_index(is_nil, 28) -> 1065;
var_index(badmatch, 15) -> 1066;
var_index(is_nonempty_list, 37) -> 1067;
var_index(is_nonempty_list, 36) -> 1068;
var_index(init, 14) -> 1069;
var_index(init, 13) -> 1070;
var_index(is_port, 0) -> 1071;
var_index(is_nil, 29) -> 1072;
var_index(l_new_bs_put_binary_all, 2) -> 1073;
var_index(is_nonempty_list, 38) -> 1074;
var_index(is_number, 0) -> 1075;
var_index(l_select_tuple_arity, 3) -> 1076;
var_index(l_bs_get_binary_all_reuse, 1) -> 1077;
var_index(l_wait_timeout, 1) -> 1078;
var_index(l_trim, 10) -> 1079;
var_index(l_bs_put_utf8, 0) -> 1080;
var_index(l_fnegate, 0) -> 1081;
var_index(l_bs_init, 0) -> 1082;
var_index(l_apply_last, 0) -> 1083;
var_index(l_jump_on_val, 2) -> 1084;
var_index(move, 13) -> 1085;
var_index(l_bs_utf8_size, 0) -> 1086;
var_index(l_bs_get_binary_imm2, 2) -> 1087;
var_index(l_new_bs_put_float, 0) -> 1088;
var_index(l_bs_validate_unicode_retract, 0) -> 1089;
var_index(init, 15) -> 1090;
var_index(l_bxor, 0) -> 1091;
var_index(l_gc_bif3, 0) -> 1092;
var_index(l_bs_match_string, 2) -> 1093;
var_index(l_bs_get_integer_imm, 1) -> 1094;
var_index(l_bs_get_integer_8, 2) -> 1095;
var_index(l_bor, 2) -> 1096;
var_index(l_bs_start_match2, 3) -> 1097;
var_index(l_new_bs_put_binary_imm, 0) -> 1098;
var_index(l_rem, 2) -> 1099;
var_index(l_bs_get_integer_small_imm, 2) -> 1100;
var_index(l_bsl, 2) -> 1101;
var_index(on_load, 0) -> 1102;
var_index(move2, 10) -> 1103;
var_index(l_int_div, 2) -> 1104;
var_index(l_bs_test_unit, 0) -> 1105;
var_index(l_select_val_atoms, 3) -> 1106;
var_index(l_m_div, 0) -> 1107;
var_index(l_hibernate, 0) -> 1108;
var_index(l_apply_fun_last, 0) -> 1109;
var_index(l_select_val_smallints, 2) -> 1110;
var_index(update_map_assoc, 0) -> 1111;
var_index(get_map_elements, 0) -> 1112;
var_index(has_map_fields, 0) -> 1113;
var_index(is_function2, 0) -> 1114;
var_index(l_apply_only, 0) -> 1115;
var_index(l_apply_fun_only, 0) -> 1116;
var_index(l_band, 2) -> 1117;
var_index(update_map_exact, 0) -> 1118;
var_index(get_map_element, 0) -> 1119;
var_index(has_map_field, 0) -> 1120;
var_index(is_bigint, 0) -> 1121;
var_index(test_heap, 1) -> 1122;
var_index(func_info, 0) -> 1123;
var_index(call_bif, 0) -> 1124;
var_index(l_bs_get_utf16, 2) -> 1125;
var_index(l_put_tuple, 9) -> 1126;
var_index(get_tuple_element, 11) -> 1127;
var_index(allocate_init, 1) -> 1128;
var_index(l_call_fun_last, 1) -> 1129;
var_index(set_tuple_element, 2) -> 1130;
var_index(l_bsr, 2) -> 1131;
var_index(l_bs_get_integer_32, 3) -> 1132;
var_index(allocate_heap, 1) -> 1133;
var_index(is_tuple_of_arity, 4) -> 1134;
var_index(test_arity, 4) -> 1135;
var_index(is_nonempty_list_allocate, 2) -> 1136;
var_index(l_bs_append, 2) -> 1137;
var_index(try_case_end, 1) -> 1138;
var_index(init3, 1) -> 1139;
var_index(l_select_tuple_arity2, 3) -> 1140;
var_index(init2, 1) -> 1141;
var_index(l_bs_get_binary_all2, 2) -> 1142;
var_index(is_nonempty_list_test_heap, 1) -> 1143;
var_index(allocate_heap_zero, 1) -> 1144;
var_index(l_bs_init_heap_bin, 1) -> 1145;
var_index(l_plus, 3) -> 1146;
var_index(l_bs_get_integer, 1) -> 1147;

var_index(Op, No) -> erlang:error({noindex,Op,No}).

var_by_index(0) -> {move, 0};
var_by_index(1) -> {move, 1};
var_by_index(2) -> {test_heap, 0};
var_by_index(3) -> {l_call, 0};
var_by_index(4) -> {move, 2};
var_by_index(5) -> {move, 3};
var_by_index(6) -> {l_put_tuple, 0};
var_by_index(7) -> {get_tuple_element, 0};
var_by_index(8) -> {move2, 1};
var_by_index(9) -> {badmatch, 0};
var_by_index(10) -> {put_list, 0};
var_by_index(11) -> {is_tuple_of_arity, 1};
var_by_index(12) -> {get_tuple_element, 1};
var_by_index(13) -> {move2, 0};
var_by_index(14) -> {l_call_only, 0};
var_by_index(15) -> {move, 4};
var_by_index(16) -> {l_is_eq_exact_immed, 0};
var_by_index(17) -> {l_is_eq_exact_immed, 1};
var_by_index(18) -> {is_tuple_of_arity, 0};
var_by_index(19) -> {put_list, 1};
var_by_index(20) -> {get_list, 0};
var_by_index(21) -> {l_put_tuple, 1};
var_by_index(22) -> {l_is_ge, 0};
var_by_index(23) -> {l_call_ext, 102};
var_by_index(24) -> {l_make_fun, 0};
var_by_index(25) -> {return, 0};
var_by_index(26) -> {is_tuple_of_arity, 2};
var_by_index(27) -> {l_move_call_last, 0};
var_by_index(28) -> {extract_next_element2, 0};
var_by_index(29) -> {l_fetch, 0};
var_by_index(30) -> {call_bif, 3};
var_by_index(31) -> {move_deallocate_return, 0};
var_by_index(32) -> {l_trim, 0};
var_by_index(33) -> {l_allocate, 0};
var_by_index(34) -> {move2, 3};
var_by_index(35) -> {put_list, 2};
var_by_index(36) -> {is_nil, 0};
var_by_index(37) -> {l_fetch, 1};
var_by_index(38) -> {is_nonempty_list, 0};
var_by_index(39) -> {move_return, 29};
var_by_index(40) -> {l_allocate, 1};
var_by_index(41) -> {extract_next_element, 0};
var_by_index(42) -> {deallocate_return, 0};
var_by_index(43) -> {case_end, 0};
var_by_index(44) -> {l_is_eq_exact_immed, 2};
var_by_index(45) -> {jump, 0};
var_by_index(46) -> {get_list, 1};
var_by_index(47) -> {call_bif, 7};
var_by_index(48) -> {move2, 2};
var_by_index(49) -> {get_tuple_element, 2};
var_by_index(50) -> {l_is_eq_exact, 0};
var_by_index(51) -> {l_move_call, 34};
var_by_index(52) -> {l_allocate, 2};
var_by_index(53) -> {move_return, 0};
var_by_index(54) -> {l_is_eq_exact_immed, 3};
var_by_index(55) -> {get_tuple_element, 3};
var_by_index(56) -> {l_fetch, 2};
var_by_index(57) -> {move, 5};
var_by_index(58) -> {init2, 0};
var_by_index(59) -> {init, 0};
var_by_index(60) -> {l_is_eq_exact_immed, 4};
var_by_index(61) -> {deallocate_return, 1};
var_by_index(62) -> {call_bif, 8};
var_by_index(63) -> {put_list, 3};
var_by_index(64) -> {extract_next_element, 1};
var_by_index(65) -> {move2, 4};
var_by_index(66) -> {is_tuple_of_arity, 3};
var_by_index(67) -> {l_fetch, 3};
var_by_index(68) -> {get_tuple_element, 4};
var_by_index(69) -> {l_allocate, 3};
var_by_index(70) -> {init, 1};
var_by_index(71) -> {move2, 5};
var_by_index(72) -> {deallocate_return, 2};
var_by_index(73) -> {l_put_tuple, 2};
var_by_index(74) -> {l_fetch, 4};
var_by_index(75) -> {l_move_call_ext, 44};
var_by_index(76) -> {is_nonempty_list, 1};
var_by_index(77) -> {l_is_eq_exact_immed, 5};
var_by_index(78) -> {l_trim, 1};
var_by_index(79) -> {init3, 0};
var_by_index(80) -> {l_allocate_zero, 0};
var_by_index(81) -> {allocate_init, 0};
var_by_index(82) -> {call_bif, 6};
var_by_index(83) -> {test_heap_1_put_list, 0};
var_by_index(84) -> {allocate_heap, 0};
var_by_index(85) -> {l_allocate_zero, 1};
var_by_index(86) -> {init, 2};
var_by_index(87) -> {get_list, 2};
var_by_index(88) -> {move_deallocate_return, 1};
var_by_index(89) -> {call_bif, 5};
var_by_index(90) -> {l_move_call, 0};
var_by_index(91) -> {call_bif, 48};
var_by_index(92) -> {l_call_last, 0};
var_by_index(93) -> {move_return, 1};
var_by_index(94) -> {l_is_eq_exact_literal, 0};
var_by_index(95) -> {is_nonempty_list, 2};
var_by_index(96) -> {is_tuple, 0};
var_by_index(97) -> {is_nonempty_list_allocate, 0};
var_by_index(98) -> {deallocate_return, 3};
var_by_index(99) -> {l_is_eq_exact_immed, 6};
var_by_index(100) -> {l_call_last, 1};
var_by_index(101) -> {is_list, 0};
var_by_index(102) -> {l_move_call_only, 0};
var_by_index(103) -> {l_select_val_atoms, 0};
var_by_index(104) -> {extract_next_element, 2};
var_by_index(105) -> {get_list, 3};
var_by_index(106) -> {l_trim, 2};
var_by_index(107) -> {l_call_last, 2};
var_by_index(108) -> {l_increment, 0};
var_by_index(109) -> {move, 6};
var_by_index(110) -> {move_deallocate_return, 2};
var_by_index(111) -> {l_is_lt, 0};
var_by_index(112) -> {is_nonempty_list, 3};
var_by_index(113) -> {l_select_val2, 3};
var_by_index(114) -> {l_select_val2, 0};
var_by_index(115) -> {l_select_val2, 4};
var_by_index(116) -> {init, 3};
var_by_index(117) -> {is_nil, 1};
var_by_index(118) -> {l_move_call_only, 1};
var_by_index(119) -> {extract_next_element2, 1};
var_by_index(120) -> {l_bs_start_match2, 0};
var_by_index(121) -> {l_move_call_only, 9};
var_by_index(122) -> {extract_next_element3, 0};
var_by_index(123) -> {remove_message, 0};
var_by_index(124) -> {get_tuple_element, 5};
var_by_index(125) -> {l_fetch, 5};
var_by_index(126) -> {l_new_bs_put_integer_imm, 0};
var_by_index(127) -> {l_catch, 0};
var_by_index(128) -> {l_allocate_zero, 2};
var_by_index(129) -> {move_return, 2};
var_by_index(130) -> {l_call_ext, 0};
var_by_index(131) -> {l_allocate, 4};
var_by_index(132) -> {move2, 6};
var_by_index(133) -> {extract_next_element, 3};
var_by_index(134) -> {l_select_val2, 1};
var_by_index(135) -> {test_arity, 0};
var_by_index(136) -> {is_nil, 2};
var_by_index(137) -> {move_jump, 13};
var_by_index(138) -> {l_allocate_zero, 3};
var_by_index(139) -> {catch_end, 0};
var_by_index(140) -> {deallocate_return, 4};
var_by_index(141) -> {l_is_eq_exact_immed, 7};
var_by_index(142) -> {move_return, 3};
var_by_index(143) -> {move_return, 4};
var_by_index(144) -> {l_move_call_ext, 0};
var_by_index(145) -> {put_list, 4};
var_by_index(146) -> {l_put_tuple, 3};
var_by_index(147) -> {move, 7};
var_by_index(148) -> {move2, 7};
var_by_index(149) -> {l_is_eq_exact_immed, 8};
var_by_index(150) -> {call_bif, 2};
var_by_index(151) -> {l_select_tuple_arity2, 0};
var_by_index(152) -> {is_nonempty_list, 4};
var_by_index(153) -> {l_plus, 0};
var_by_index(154) -> {l_call_last, 3};
var_by_index(155) -> {is_nonempty_list, 5};
var_by_index(156) -> {get_list, 4};
var_by_index(157) -> {init, 4};
var_by_index(158) -> {move_return, 5};
var_by_index(159) -> {l_move_call_only, 2};
var_by_index(160) -> {l_increment, 3};
var_by_index(161) -> {is_nonempty_list, 6};
var_by_index(162) -> {l_bs_get_binary_all_reuse, 0};
var_by_index(163) -> {l_call_last, 4};
var_by_index(164) -> {bif1_body, 0};
var_by_index(165) -> {l_gc_bif1, 0};
var_by_index(166) -> {l_call_fun, 0};
var_by_index(167) -> {test_arity, 1};
var_by_index(168) -> {set_tuple_element, 0};
var_by_index(169) -> {l_move_call, 1};
var_by_index(170) -> {l_bs_match_string, 0};
var_by_index(171) -> {l_move_call_last, 1};
var_by_index(172) -> {l_bs_test_zero_tail2, 0};
var_by_index(173) -> {call_bif, 9};
var_by_index(174) -> {send, 0};
var_by_index(175) -> {l_is_eq, 0};
var_by_index(176) -> {l_move_call_only, 3};
var_by_index(177) -> {l_catch, 1};
var_by_index(178) -> {l_is_ne, 0};
var_by_index(179) -> {l_bs_add, 0};
var_by_index(180) -> {l_select_val2, 2};
var_by_index(181) -> {l_move_call_ext, 1};
var_by_index(182) -> {l_call_ext_only, 4};
var_by_index(183) -> {l_select_val_smallints, 0};
var_by_index(184) -> {bif2_body, 0};
var_by_index(185) -> {is_nonempty_list, 7};
var_by_index(186) -> {l_is_ne_exact_immed, 0};
var_by_index(187) -> {l_is_eq_exact_immed, 9};
var_by_index(188) -> {l_move_call_ext_last, 0};
var_by_index(189) -> {extract_next_element2, 2};
var_by_index(190) -> {case_end, 1};
var_by_index(191) -> {extract_next_element2, 3};
var_by_index(192) -> {extract_next_element, 4};
var_by_index(193) -> {l_move_call, 2};
var_by_index(194) -> {l_select_val_atoms, 1};
var_by_index(195) -> {move_deallocate_return, 3};
var_by_index(196) -> {l_move_call_ext, 2};
var_by_index(197) -> {l_loop_rec, 0};
var_by_index(198) -> {l_move_call, 3};
var_by_index(199) -> {l_select_tuple_arity2, 1};
var_by_index(200) -> {is_nil, 3};
var_by_index(201) -> {l_trim, 3};
var_by_index(202) -> {l_bs_get_utf16, 0};
var_by_index(203) -> {l_bs_restore2, 0};
var_by_index(204) -> {init, 5};
var_by_index(205) -> {l_move_call, 4};
var_by_index(206) -> {catch_end, 1};
var_by_index(207) -> {is_nonempty_list, 8};
var_by_index(208) -> {is_nonempty_list, 9};
var_by_index(209) -> {is_nil, 4};
var_by_index(210) -> {l_call_ext_last, 0};
var_by_index(211) -> {l_fetch, 7};
var_by_index(212) -> {deallocate_return, 5};
var_by_index(213) -> {l_bs_init_heap_bin, 0};
var_by_index(214) -> {l_allocate_zero, 4};
var_by_index(215) -> {wait, 0};
var_by_index(216) -> {badmatch, 1};
var_by_index(217) -> {call_bif, 10};
var_by_index(218) -> {loop_rec_end, 0};
var_by_index(219) -> {l_allocate, 5};
var_by_index(220) -> {extract_next_element, 5};
var_by_index(221) -> {l_move_call_ext, 3};
var_by_index(222) -> {move, 8};
var_by_index(223) -> {l_select_val2, 5};
var_by_index(224) -> {l_allocate_zero, 5};
var_by_index(225) -> {l_bs_start_match2, 1};
var_by_index(226) -> {l_move_call_ext, 4};
var_by_index(227) -> {l_move_call_ext, 5};
var_by_index(228) -> {badmatch, 2};
var_by_index(229) -> {l_call_last, 5};
var_by_index(230) -> {extract_next_element2, 4};
var_by_index(231) -> {l_increment, 1};
var_by_index(232) -> {l_call_fun_last, 0};
var_by_index(233) -> {l_plus, 1};
var_by_index(234) -> {init, 6};
var_by_index(235) -> {l_move_call_ext, 6};
var_by_index(236) -> {l_gc_bif1, 1};
var_by_index(237) -> {l_call_last, 6};
var_by_index(238) -> {l_call_ext, 1};
var_by_index(239) -> {l_trim, 4};
var_by_index(240) -> {bs_context_to_binary, 0};
var_by_index(241) -> {put_list, 5};
var_by_index(242) -> {l_bs_save2, 0};
var_by_index(243) -> {move_deallocate_return, 4};
var_by_index(244) -> {l_increment, 2};
var_by_index(245) -> {extract_next_element, 6};
var_by_index(246) -> {call_bif, 11};
var_by_index(247) -> {int_code_end, 0};
var_by_index(248) -> {l_move_call_ext_only, 0};
var_by_index(249) -> {call_bif, 12};
var_by_index(250) -> {get_tuple_element, 6};
var_by_index(251) -> {l_is_ne_exact_immed, 1};
var_by_index(252) -> {l_is_eq_exact_immed, 11};
var_by_index(253) -> {call_bif, 14};
var_by_index(254) -> {call_bif, 13};
var_by_index(255) -> {l_move_call, 5};
var_by_index(256) -> {l_put_tuple, 4};
var_by_index(257) -> {l_bs_test_zero_tail2, 1};
var_by_index(258) -> {self, 0};
var_by_index(259) -> {extract_next_element, 7};
var_by_index(260) -> {is_nil, 5};
var_by_index(261) -> {is_nonempty_list_allocate, 1};
var_by_index(262) -> {l_move_call, 6};
var_by_index(263) -> {badmatch, 3};
var_by_index(264) -> {is_tuple, 1};
var_by_index(265) -> {l_select_val2, 7};
var_by_index(266) -> {l_move_call_last, 2};
var_by_index(267) -> {l_move_call_ext, 7};
var_by_index(268) -> {l_bs_test_zero_tail2, 2};
var_by_index(269) -> {l_gc_bif1, 3};
var_by_index(270) -> {is_atom, 0};
var_by_index(271) -> {is_nil, 6};
var_by_index(272) -> {is_nonempty_list, 10};
var_by_index(273) -> {l_call_ext_last, 1};
var_by_index(274) -> {l_select_val_smallints, 1};
var_by_index(275) -> {l_fast_element, 0};
var_by_index(276) -> {extract_next_element, 8};
var_by_index(277) -> {l_bs_match_string, 1};
var_by_index(278) -> {is_nonempty_list, 11};
var_by_index(279) -> {l_catch, 2};
var_by_index(280) -> {l_gc_bif1, 4};
var_by_index(281) -> {l_call_ext, 2};
var_by_index(282) -> {l_move_call, 7};
var_by_index(283) -> {l_bs_test_unit_8, 0};
var_by_index(284) -> {call_bif, 16};
var_by_index(285) -> {call_bif, 15};
var_by_index(286) -> {raise, 0};
var_by_index(287) -> {l_call_ext, 3};
var_by_index(288) -> {is_nil, 7};
var_by_index(289) -> {extract_next_element3, 1};
var_by_index(290) -> {move_return, 6};
var_by_index(291) -> {l_call_ext_last, 2};
var_by_index(292) -> {l_move_call_ext, 8};
var_by_index(293) -> {call_bif, 17};
var_by_index(294) -> {allocate_heap_zero, 0};
var_by_index(295) -> {call_bif, 18};
var_by_index(296) -> {l_allocate, 6};
var_by_index(297) -> {case_end, 2};
var_by_index(298) -> {deallocate_return, 6};
var_by_index(299) -> {l_is_eq_exact_literal, 7};
var_by_index(300) -> {self, 1};
var_by_index(301) -> {get_list, 7};
var_by_index(302) -> {init, 7};
var_by_index(303) -> {l_minus, 0};
var_by_index(304) -> {l_move_call_ext_only, 1};
var_by_index(305) -> {call_bif, 19};
var_by_index(306) -> {extract_next_element2, 5};
var_by_index(307) -> {l_call_fun, 1};
var_by_index(308) -> {catch_end, 2};
var_by_index(309) -> {l_move_call_ext_only, 7};
var_by_index(310) -> {call_bif, 20};
var_by_index(311) -> {extract_next_element, 9};
var_by_index(312) -> {l_move_call_last, 3};
var_by_index(313) -> {l_call_ext, 4};
var_by_index(314) -> {try_end, 0};
var_by_index(315) -> {l_move_call_ext, 9};
var_by_index(316) -> {l_band, 0};
var_by_index(317) -> {l_trim, 5};
var_by_index(318) -> {l_bif2, 0};
var_by_index(319) -> {l_move_call, 8};
var_by_index(320) -> {is_nil, 8};
var_by_index(321) -> {l_fetch, 6};
var_by_index(322) -> {l_call_ext, 5};
var_by_index(323) -> {test_heap_1_put_list, 1};
var_by_index(324) -> {l_is_eq_exact_immed, 12};
var_by_index(325) -> {l_call_ext, 6};
var_by_index(326) -> {try_end, 1};
var_by_index(327) -> {l_gc_bif1, 2};
var_by_index(328) -> {l_bs_get_binary2, 0};
var_by_index(329) -> {l_minus, 1};
var_by_index(330) -> {l_bs_get_binary_all2, 0};
var_by_index(331) -> {l_is_eq_exact_immed, 13};
var_by_index(332) -> {is_integer, 0};
var_by_index(333) -> {get_tuple_element, 7};
var_by_index(334) -> {extract_next_element2, 6};
var_by_index(335) -> {l_move_call_only, 4};
var_by_index(336) -> {l_is_eq_exact_literal, 1};
var_by_index(337) -> {l_catch, 3};
var_by_index(338) -> {move_jump, 0};
var_by_index(339) -> {is_list, 1};
var_by_index(340) -> {extract_next_element2, 7};
var_by_index(341) -> {l_bsr, 0};
var_by_index(342) -> {l_bif2, 1};
var_by_index(343) -> {l_move_call, 9};
var_by_index(344) -> {l_call_ext, 8};
var_by_index(345) -> {l_call_ext, 7};
var_by_index(346) -> {apply, 0};
var_by_index(347) -> {l_is_eq_exact_immed, 14};
var_by_index(348) -> {set_tuple_element, 1};
var_by_index(349) -> {l_bs_test_zero_tail2, 3};
var_by_index(350) -> {move_deallocate_return, 5};
var_by_index(351) -> {l_bif2, 2};
var_by_index(352) -> {l_times, 0};
var_by_index(353) -> {l_is_eq_exact_literal, 2};
var_by_index(354) -> {l_fast_element, 2};
var_by_index(355) -> {l_fetch, 8};
var_by_index(356) -> {l_is_eq_exact_immed, 10};
var_by_index(357) -> {l_is_eq_exact_literal, 3};
var_by_index(358) -> {l_fmul, 0};
var_by_index(359) -> {l_move_call_ext, 10};
var_by_index(360) -> {call_bif, 21};
var_by_index(361) -> {extract_next_element, 27};
var_by_index(362) -> {extract_next_element, 10};
var_by_index(363) -> {is_nil, 9};
var_by_index(364) -> {l_times, 1};
var_by_index(365) -> {l_move_call_ext, 11};
var_by_index(366) -> {l_allocate_zero, 6};
var_by_index(367) -> {l_bs_init_fail, 0};
var_by_index(368) -> {l_bs_get_integer_8, 0};
var_by_index(369) -> {test_arity, 2};
var_by_index(370) -> {extract_next_element3, 2};
var_by_index(371) -> {is_nonempty_list, 12};
var_by_index(372) -> {l_is_eq_exact_immed, 15};
var_by_index(373) -> {l_call_ext, 9};
var_by_index(374) -> {l_bsl, 0};
var_by_index(375) -> {l_move_call_ext_only, 2};
var_by_index(376) -> {l_bs_get_integer_8, 1};
var_by_index(377) -> {l_put_tuple, 6};
var_by_index(378) -> {is_tuple, 2};
var_by_index(379) -> {l_is_ne_exact, 0};
var_by_index(380) -> {l_select_val2, 6};
var_by_index(381) -> {move, 9};
var_by_index(382) -> {l_move_call_ext, 12};
var_by_index(383) -> {l_call_last, 7};
var_by_index(384) -> {l_call_fun, 2};
var_by_index(385) -> {node, 0};
var_by_index(386) -> {is_binary, 0};
var_by_index(387) -> {l_select_val2, 9};
var_by_index(388) -> {l_is_ne_exact_immed, 2};
var_by_index(389) -> {l_new_bs_put_binary_all, 0};
var_by_index(390) -> {l_bs_test_zero_tail2, 5};
var_by_index(391) -> {l_bif2, 3};
var_by_index(392) -> {extract_next_element3, 3};
var_by_index(393) -> {l_move_call_ext_last, 1};
var_by_index(394) -> {get_list, 5};
var_by_index(395) -> {l_bif2, 4};
var_by_index(396) -> {l_bs_get_integer_32, 0};
var_by_index(397) -> {call_bif, 22};
var_by_index(398) -> {l_call_ext, 10};
var_by_index(399) -> {l_new_bs_put_binary_all, 1};
var_by_index(400) -> {catch_end, 3};
var_by_index(401) -> {extract_next_element, 11};
var_by_index(402) -> {l_fcheckerror, 0};
var_by_index(403) -> {fclearerror, 0};
var_by_index(404) -> {is_atom, 1};
var_by_index(405) -> {l_is_eq_exact_immed, 16};
var_by_index(406) -> {l_allocate_zero, 9};
var_by_index(407) -> {extract_next_element, 12};
var_by_index(408) -> {extract_next_element3, 4};
var_by_index(409) -> {get_list, 6};
var_by_index(410) -> {l_bs_append, 0};
var_by_index(411) -> {l_is_eq_exact_immed, 17};
var_by_index(412) -> {node, 1};
var_by_index(413) -> {deallocate_return, 7};
var_by_index(414) -> {l_bs_get_integer_32, 1};
var_by_index(415) -> {timeout, 0};
var_by_index(416) -> {l_is_eq_exact_immed, 36};
var_by_index(417) -> {l_move_call_ext, 14};
var_by_index(418) -> {l_fetch, 9};
var_by_index(419) -> {l_move_call, 10};
var_by_index(420) -> {l_move_call_ext, 15};
var_by_index(421) -> {is_float, 1};
var_by_index(422) -> {l_rem, 0};
var_by_index(423) -> {l_get, 0};
var_by_index(424) -> {move_deallocate_return, 6};
var_by_index(425) -> {is_nonempty_list, 39};
var_by_index(426) -> {l_fetch, 10};
var_by_index(427) -> {call_bif, 23};
var_by_index(428) -> {l_call_ext, 11};
var_by_index(429) -> {l_fast_element, 1};
var_by_index(430) -> {l_move_call, 11};
var_by_index(431) -> {l_band, 1};
var_by_index(432) -> {l_allocate_zero, 7};
var_by_index(433) -> {is_nonempty_list, 13};
var_by_index(434) -> {is_binary, 1};
var_by_index(435) -> {l_select_tuple_arity, 1};
var_by_index(436) -> {l_fetch, 11};
var_by_index(437) -> {l_call_ext, 12};
var_by_index(438) -> {is_nonempty_list, 14};
var_by_index(439) -> {call_bif, 24};
var_by_index(440) -> {extract_next_element, 13};
var_by_index(441) -> {l_trim, 6};
var_by_index(442) -> {is_nil, 10};
var_by_index(443) -> {l_move_call, 12};
var_by_index(444) -> {l_get, 1};
var_by_index(445) -> {l_is_eq_exact_immed, 18};
var_by_index(446) -> {is_nil, 11};
var_by_index(447) -> {l_select_val2, 10};
var_by_index(448) -> {l_bs_get_integer_small_imm, 0};
var_by_index(449) -> {l_bsl, 1};
var_by_index(450) -> {l_get, 2};
var_by_index(451) -> {l_call_ext, 13};
var_by_index(452) -> {l_call_last, 8};
var_by_index(453) -> {l_is_eq_exact_immed, 19};
var_by_index(454) -> {l_move_call_last, 4};
var_by_index(455) -> {l_bor, 0};
var_by_index(456) -> {l_allocate, 7};
var_by_index(457) -> {l_call_ext, 14};
var_by_index(458) -> {is_integer, 1};
var_by_index(459) -> {is_nil, 30};
var_by_index(460) -> {is_nil, 13};
var_by_index(461) -> {is_nil, 12};
var_by_index(462) -> {l_move_call_only, 5};
var_by_index(463) -> {l_move_call, 13};
var_by_index(464) -> {move2, 8};
var_by_index(465) -> {l_gc_bif1, 5};
var_by_index(466) -> {call_bif, 25};
var_by_index(467) -> {move_return, 7};
var_by_index(468) -> {is_list, 2};
var_by_index(469) -> {l_call_ext, 15};
var_by_index(470) -> {l_move_call_ext, 17};
var_by_index(471) -> {l_move_call_ext, 16};
var_by_index(472) -> {l_catch, 4};
var_by_index(473) -> {call_bif, 27};
var_by_index(474) -> {call_bif, 26};
var_by_index(475) -> {extract_next_element2, 8};
var_by_index(476) -> {is_nonempty_list, 15};
var_by_index(477) -> {l_move_call_ext, 18};
var_by_index(478) -> {l_select_val2, 17};
var_by_index(479) -> {l_element, 1};
var_by_index(480) -> {l_fadd, 0};
var_by_index(481) -> {extract_next_element3, 11};
var_by_index(482) -> {l_put_tuple, 5};
var_by_index(483) -> {bif1_body, 1};
var_by_index(484) -> {call_bif, 28};
var_by_index(485) -> {move_jump, 1};
var_by_index(486) -> {l_is_ne_exact_immed, 3};
var_by_index(487) -> {call_bif, 29};
var_by_index(488) -> {l_call_ext, 16};
var_by_index(489) -> {l_select_val2, 8};
var_by_index(490) -> {is_nonempty_list, 16};
var_by_index(491) -> {fmove_1, 0};
var_by_index(492) -> {l_fetch, 12};
var_by_index(493) -> {extract_next_element2, 18};
var_by_index(494) -> {l_bs_get_utf8, 0};
var_by_index(495) -> {is_atom, 2};
var_by_index(496) -> {bif1_body, 2};
var_by_index(497) -> {l_is_ne_exact_immed, 11};
var_by_index(498) -> {l_call_ext_last, 3};
var_by_index(499) -> {l_move_call_ext, 19};
var_by_index(500) -> {bif2_body, 1};
var_by_index(501) -> {l_move_call_ext, 20};
var_by_index(502) -> {call_bif, 30};
var_by_index(503) -> {l_call_ext, 17};
var_by_index(504) -> {l_is_eq_exact_immed, 20};
var_by_index(505) -> {l_bs_skip_bits_all2, 0};
var_by_index(506) -> {call_bif, 31};
var_by_index(507) -> {case_end, 10};
var_by_index(508) -> {l_select_tuple_arity, 0};
var_by_index(509) -> {l_move_call_ext, 21};
var_by_index(510) -> {l_is_eq_exact_immed, 21};
var_by_index(511) -> {fconv, 0};
var_by_index(512) -> {l_catch, 5};
var_by_index(513) -> {put_list, 8};
var_by_index(514) -> {l_move_call_ext_last, 2};
var_by_index(515) -> {call_bif, 4};
var_by_index(516) -> {extract_next_element3, 5};
var_by_index(517) -> {put_list, 6};
var_by_index(518) -> {l_is_eq_exact_literal, 4};
var_by_index(519) -> {l_select_tuple_arity, 2};
var_by_index(520) -> {system_limit, 0};
var_by_index(521) -> {l_call_ext_last, 4};
var_by_index(522) -> {l_fetch, 23};
var_by_index(523) -> {extract_next_element, 14};
var_by_index(524) -> {is_binary, 2};
var_by_index(525) -> {l_bor, 1};
var_by_index(526) -> {call_bif, 32};
var_by_index(527) -> {l_call_ext, 18};
var_by_index(528) -> {is_integer_allocate, 0};
var_by_index(529) -> {l_is_eq_exact_immed, 23};
var_by_index(530) -> {l_move_call_ext_only, 3};
var_by_index(531) -> {badmatch, 4};
var_by_index(532) -> {l_call_fun, 3};
var_by_index(533) -> {l_bs_init_bits_fail, 0};
var_by_index(534) -> {l_make_export, 0};
var_by_index(535) -> {is_integer, 6};
var_by_index(536) -> {call_bif, 34};
var_by_index(537) -> {call_bif, 33};
var_by_index(538) -> {apply_last, 0};
var_by_index(539) -> {fmove_2, 0};
var_by_index(540) -> {fmove_2, 1};
var_by_index(541) -> {fmove_1, 1};
var_by_index(542) -> {call_bif, 35};
var_by_index(543) -> {l_call_ext, 19};
var_by_index(544) -> {extract_next_element, 15};
var_by_index(545) -> {try_end, 2};
var_by_index(546) -> {deallocate_return, 8};
var_by_index(547) -> {l_fetch, 13};
var_by_index(548) -> {l_fast_element, 3};
var_by_index(549) -> {l_move_call_ext, 22};
var_by_index(550) -> {is_tuple, 3};
var_by_index(551) -> {l_bs_restore2, 1};
var_by_index(552) -> {l_move_call_ext, 23};
var_by_index(553) -> {l_bs_test_zero_tail2, 4};
var_by_index(554) -> {l_new_bs_put_integer, 0};
var_by_index(555) -> {extract_next_element, 16};
var_by_index(556) -> {l_trim, 7};
var_by_index(557) -> {is_list, 6};
var_by_index(558) -> {is_nonempty_list, 17};
var_by_index(559) -> {l_fetch, 14};
var_by_index(560) -> {extract_next_element2, 9};
var_by_index(561) -> {is_atom, 3};
var_by_index(562) -> {l_fetch, 15};
var_by_index(563) -> {is_integer, 2};
var_by_index(564) -> {l_move_call, 15};
var_by_index(565) -> {l_move_call, 14};
var_by_index(566) -> {l_call_ext, 21};
var_by_index(567) -> {l_call_ext, 20};
var_by_index(568) -> {try_end, 3};
var_by_index(569) -> {l_move_call_ext, 13};
var_by_index(570) -> {l_call_ext_last, 6};
var_by_index(571) -> {case_end, 3};
var_by_index(572) -> {is_nonempty_list, 18};
var_by_index(573) -> {try_end, 4};
var_by_index(574) -> {init, 8};
var_by_index(575) -> {if_end, 0};
var_by_index(576) -> {move_deallocate_return, 7};
var_by_index(577) -> {call_bif, 39};
var_by_index(578) -> {call_bif, 38};
var_by_index(579) -> {call_bif, 37};
var_by_index(580) -> {call_bif, 36};
var_by_index(581) -> {try_end, 5};
var_by_index(582) -> {is_nonempty_list_test_heap, 0};
var_by_index(583) -> {l_move_call_ext, 24};
var_by_index(584) -> {l_move_call_ext_last, 3};
var_by_index(585) -> {l_call_ext, 25};
var_by_index(586) -> {l_call_ext, 24};
var_by_index(587) -> {l_call_ext, 23};
var_by_index(588) -> {l_call_ext, 22};
var_by_index(589) -> {l_get, 3};
var_by_index(590) -> {put_list, 9};
var_by_index(591) -> {self, 2};
var_by_index(592) -> {l_call_ext, 28};
var_by_index(593) -> {l_call_ext, 27};
var_by_index(594) -> {l_call_ext, 26};
var_by_index(595) -> {extract_next_element, 17};
var_by_index(596) -> {is_list, 3};
var_by_index(597) -> {catch_end, 4};
var_by_index(598) -> {l_move_call_last, 5};
var_by_index(599) -> {l_is_ne_exact_immed, 4};
var_by_index(600) -> {is_tuple, 4};
var_by_index(601) -> {extract_next_element2, 10};
var_by_index(602) -> {move_jump, 2};
var_by_index(603) -> {l_bs_skip_bits_imm2, 0};
var_by_index(604) -> {l_fdiv, 0};
var_by_index(605) -> {get_tuple_element, 8};
var_by_index(606) -> {l_bif1, 0};
var_by_index(607) -> {l_bs_get_utf16, 1};
var_by_index(608) -> {extract_next_element, 18};
var_by_index(609) -> {is_integer, 3};
var_by_index(610) -> {l_bs_test_unit_8, 3};
var_by_index(611) -> {is_nonempty_list, 19};
var_by_index(612) -> {l_bif1, 1};
var_by_index(613) -> {is_tuple, 10};
var_by_index(614) -> {bif1_body, 3};
var_by_index(615) -> {get_list, 8};
var_by_index(616) -> {l_select_val2, 11};
var_by_index(617) -> {badmatch, 5};
var_by_index(618) -> {l_move_call, 16};
var_by_index(619) -> {l_is_eq_exact_immed, 24};
var_by_index(620) -> {l_call_ext, 30};
var_by_index(621) -> {l_call_ext, 29};
var_by_index(622) -> {l_is_eq_exact_immed, 25};
var_by_index(623) -> {l_move_call_ext, 25};
var_by_index(624) -> {l_select_val_atoms, 2};
var_by_index(625) -> {extract_next_element2, 11};
var_by_index(626) -> {move_return, 8};
var_by_index(627) -> {case_end, 4};
var_by_index(628) -> {self, 5};
var_by_index(629) -> {l_call_ext, 31};
var_by_index(630) -> {extract_next_element3, 6};
var_by_index(631) -> {l_bs_get_binary_imm2, 0};
var_by_index(632) -> {l_bs_test_unit_8, 1};
var_by_index(633) -> {is_list, 4};
var_by_index(634) -> {l_bs_restore2, 2};
var_by_index(635) -> {is_atom, 6};
var_by_index(636) -> {l_move_call_ext_last, 5};
var_by_index(637) -> {bif1_body, 5};
var_by_index(638) -> {call_bif, 41};
var_by_index(639) -> {call_bif, 40};
var_by_index(640) -> {is_nil, 14};
var_by_index(641) -> {l_move_call_only, 6};
var_by_index(642) -> {move_deallocate_return, 8};
var_by_index(643) -> {init, 9};
var_by_index(644) -> {l_plus, 2};
var_by_index(645) -> {test_arity, 3};
var_by_index(646) -> {l_get, 6};
var_by_index(647) -> {l_rem, 1};
var_by_index(648) -> {l_call_ext, 32};
var_by_index(649) -> {is_nil, 15};
var_by_index(650) -> {case_end, 5};
var_by_index(651) -> {l_move_call_ext, 27};
var_by_index(652) -> {l_move_call_ext, 28};
var_by_index(653) -> {call_bif, 42};
var_by_index(654) -> {l_call_ext, 33};
var_by_index(655) -> {is_nil, 16};
var_by_index(656) -> {l_fsub, 0};
var_by_index(657) -> {l_is_eq_exact_literal, 5};
var_by_index(658) -> {l_jump_on_val, 0};
var_by_index(659) -> {l_move_call_ext, 29};
var_by_index(660) -> {get_tuple_element, 9};
var_by_index(661) -> {l_move_call_last, 6};
var_by_index(662) -> {call_bif, 43};
var_by_index(663) -> {is_integer, 4};
var_by_index(664) -> {l_bs_put_string, 0};
var_by_index(665) -> {is_nonempty_list, 20};
var_by_index(666) -> {l_bif2, 5};
var_by_index(667) -> {l_move_call_ext, 30};
var_by_index(668) -> {put_list, 7};
var_by_index(669) -> {l_move_call_ext, 31};
var_by_index(670) -> {bif1_body, 6};
var_by_index(671) -> {l_int_div, 0};
var_by_index(672) -> {node, 4};
var_by_index(673) -> {l_bsr, 1};
var_by_index(674) -> {l_increment, 4};
var_by_index(675) -> {l_put_tuple, 7};
var_by_index(676) -> {l_call_ext, 34};
var_by_index(677) -> {badmatch, 6};
var_by_index(678) -> {case_end, 6};
var_by_index(679) -> {l_is_eq_exact_literal, 6};
var_by_index(680) -> {l_call_ext_only, 0};
var_by_index(681) -> {move, 10};
var_by_index(682) -> {l_increment, 5};
var_by_index(683) -> {l_allocate, 9};
var_by_index(684) -> {l_call_ext, 35};
var_by_index(685) -> {l_allocate_zero, 8};
var_by_index(686) -> {call_bif, 44};
var_by_index(687) -> {l_call_ext, 36};
var_by_index(688) -> {l_call_last, 9};
var_by_index(689) -> {move_return, 9};
var_by_index(690) -> {is_list, 5};
var_by_index(691) -> {l_move_call, 17};
var_by_index(692) -> {catch_end, 5};
var_by_index(693) -> {l_is_eq_exact_immed, 26};
var_by_index(694) -> {l_fetch, 16};
var_by_index(695) -> {l_put_tuple, 8};
var_by_index(696) -> {l_call_ext, 38};
var_by_index(697) -> {l_call_ext, 37};
var_by_index(698) -> {put_list, 10};
var_by_index(699) -> {l_move_call, 18};
var_by_index(700) -> {test_heap_1_put_list, 2};
var_by_index(701) -> {get_list, 9};
var_by_index(702) -> {l_bs_skip_bits2, 0};
var_by_index(703) -> {l_fetch, 17};
var_by_index(704) -> {call_bif, 45};
var_by_index(705) -> {node, 2};
var_by_index(706) -> {extract_next_element2, 12};
var_by_index(707) -> {is_nil, 17};
var_by_index(708) -> {try_case_end, 0};
var_by_index(709) -> {l_yield, 0};
var_by_index(710) -> {l_bs_get_binary_all2, 1};
var_by_index(711) -> {is_atom, 4};
var_by_index(712) -> {l_move_call, 19};
var_by_index(713) -> {l_call_ext_only, 1};
var_by_index(714) -> {l_move_call_ext, 33};
var_by_index(715) -> {l_is_eq_exact_immed, 27};
var_by_index(716) -> {move_return, 10};
var_by_index(717) -> {l_move_call_ext, 34};
var_by_index(718) -> {l_bs_get_integer, 0};
var_by_index(719) -> {l_bs_append, 1};
var_by_index(720) -> {l_move_call_ext, 35};
var_by_index(721) -> {l_bs_get_binary2, 1};
var_by_index(722) -> {call_bif, 46};
var_by_index(723) -> {l_is_eq_exact_immed, 22};
var_by_index(724) -> {extract_next_element, 19};
var_by_index(725) -> {extract_next_element3, 7};
var_by_index(726) -> {is_nonempty_list, 22};
var_by_index(727) -> {is_nonempty_list, 21};
var_by_index(728) -> {l_move_call, 20};
var_by_index(729) -> {l_is_ne_exact_immed, 5};
var_by_index(730) -> {l_fetch, 18};
var_by_index(731) -> {l_catch, 6};
var_by_index(732) -> {call_bif, 47};
var_by_index(733) -> {l_allocate, 8};
var_by_index(734) -> {l_call_ext, 40};
var_by_index(735) -> {l_call_ext, 39};
var_by_index(736) -> {l_is_ne_exact_immed, 6};
var_by_index(737) -> {badmatch, 7};
var_by_index(738) -> {case_end, 7};
var_by_index(739) -> {is_binary, 3};
var_by_index(740) -> {init, 10};
var_by_index(741) -> {l_gc_bif1, 6};
var_by_index(742) -> {l_int_div, 1};
var_by_index(743) -> {l_is_eq_exact_immed, 29};
var_by_index(744) -> {l_is_eq_exact_immed, 28};
var_by_index(745) -> {is_function, 1};
var_by_index(746) -> {fconv, 1};
var_by_index(747) -> {fmove_1, 3};
var_by_index(748) -> {l_call_ext, 42};
var_by_index(749) -> {l_call_ext, 41};
var_by_index(750) -> {move_jump, 3};
var_by_index(751) -> {is_nil, 18};
var_by_index(752) -> {case_end, 8};
var_by_index(753) -> {l_select_val2, 12};
var_by_index(754) -> {l_bs_save2, 1};
var_by_index(755) -> {l_move_call, 23};
var_by_index(756) -> {l_move_call, 22};
var_by_index(757) -> {l_move_call, 21};
var_by_index(758) -> {l_move_call_ext, 36};
var_by_index(759) -> {deallocate_return, 12};
var_by_index(760) -> {bif2_body, 2};
var_by_index(761) -> {l_move_call_ext, 37};
var_by_index(762) -> {l_move_call_ext_last, 4};
var_by_index(763) -> {call_bif, 1};
var_by_index(764) -> {l_call_ext, 49};
var_by_index(765) -> {l_call_ext, 48};
var_by_index(766) -> {l_call_ext, 47};
var_by_index(767) -> {l_call_ext, 46};
var_by_index(768) -> {l_call_ext, 45};
var_by_index(769) -> {l_call_ext, 44};
var_by_index(770) -> {l_call_ext, 43};
var_by_index(771) -> {l_move_call_ext_only, 4};
var_by_index(772) -> {l_is_ne_exact_immed, 7};
var_by_index(773) -> {l_move_call_ext, 38};
var_by_index(774) -> {l_gc_bif1, 7};
var_by_index(775) -> {l_call_ext, 52};
var_by_index(776) -> {l_call_ext, 51};
var_by_index(777) -> {l_call_ext, 50};
var_by_index(778) -> {is_nil, 19};
var_by_index(779) -> {is_nonempty_list, 23};
var_by_index(780) -> {l_call_fun, 4};
var_by_index(781) -> {l_move_call_ext_only, 5};
var_by_index(782) -> {l_move_call_ext, 39};
var_by_index(783) -> {is_pid, 1};
var_by_index(784) -> {bif1_body, 4};
var_by_index(785) -> {l_call_ext, 55};
var_by_index(786) -> {l_call_ext, 54};
var_by_index(787) -> {l_call_ext, 53};
var_by_index(788) -> {l_trim, 8};
var_by_index(789) -> {l_bs_test_unit_8, 2};
var_by_index(790) -> {l_move_call, 24};
var_by_index(791) -> {l_move_call_ext, 42};
var_by_index(792) -> {l_move_call_ext, 41};
var_by_index(793) -> {l_move_call_ext, 40};
var_by_index(794) -> {l_get, 4};
var_by_index(795) -> {l_increment, 8};
var_by_index(796) -> {is_boolean, 0};
var_by_index(797) -> {l_bs_get_integer_small_imm, 1};
var_by_index(798) -> {l_call_ext, 57};
var_by_index(799) -> {l_call_ext, 56};
var_by_index(800) -> {extract_next_element, 20};
var_by_index(801) -> {extract_next_element2, 13};
var_by_index(802) -> {move_jump, 4};
var_by_index(803) -> {bs_context_to_binary, 1};
var_by_index(804) -> {l_bs_put_string, 2};
var_by_index(805) -> {l_call_ext_last, 5};
var_by_index(806) -> {bif1_body, 7};
var_by_index(807) -> {init, 11};
var_by_index(808) -> {l_fetch, 19};
var_by_index(809) -> {badmatch, 16};
var_by_index(810) -> {bs_init_writable, 0};
var_by_index(811) -> {l_call_ext, 58};
var_by_index(812) -> {l_move_call_only, 7};
var_by_index(813) -> {put_list, 12};
var_by_index(814) -> {case_end, 9};
var_by_index(815) -> {l_bs_private_append, 0};
var_by_index(816) -> {l_move_call, 25};
var_by_index(817) -> {l_is_ne_exact_literal, 0};
var_by_index(818) -> {l_call_ext, 65};
var_by_index(819) -> {l_call_ext, 64};
var_by_index(820) -> {l_call_ext, 63};
var_by_index(821) -> {l_call_ext, 62};
var_by_index(822) -> {l_call_ext, 61};
var_by_index(823) -> {l_call_ext, 60};
var_by_index(824) -> {l_call_ext, 59};
var_by_index(825) -> {extract_next_element2, 14};
var_by_index(826) -> {move_jump, 5};
var_by_index(827) -> {move_return, 11};
var_by_index(828) -> {badmatch, 9};
var_by_index(829) -> {badmatch, 8};
var_by_index(830) -> {l_move_call, 26};
var_by_index(831) -> {move_deallocate_return, 10};
var_by_index(832) -> {l_call_ext, 68};
var_by_index(833) -> {l_call_ext, 67};
var_by_index(834) -> {l_call_ext, 66};
var_by_index(835) -> {extract_next_element3, 8};
var_by_index(836) -> {l_bs_start_match2, 2};
var_by_index(837) -> {move_jump, 6};
var_by_index(838) -> {move_return, 12};
var_by_index(839) -> {put_list, 11};
var_by_index(840) -> {deallocate_return, 9};
var_by_index(841) -> {l_wait_timeout, 2};
var_by_index(842) -> {l_get, 5};
var_by_index(843) -> {l_element, 0};
var_by_index(844) -> {l_bs_skip_bits_all2, 1};
var_by_index(845) -> {l_select_tuple_arity2, 2};
var_by_index(846) -> {l_is_eq_exact_immed, 30};
var_by_index(847) -> {l_bif2, 6};
var_by_index(848) -> {l_call_ext, 73};
var_by_index(849) -> {l_call_ext, 72};
var_by_index(850) -> {l_call_ext, 71};
var_by_index(851) -> {l_call_ext, 70};
var_by_index(852) -> {l_call_ext, 69};
var_by_index(853) -> {extract_next_element2, 15};
var_by_index(854) -> {move_jump, 7};
var_by_index(855) -> {move_return, 14};
var_by_index(856) -> {move_return, 13};
var_by_index(857) -> {l_move_call_only, 8};
var_by_index(858) -> {is_nonempty_list, 24};
var_by_index(859) -> {l_move_call, 28};
var_by_index(860) -> {l_move_call, 27};
var_by_index(861) -> {l_call_ext_only, 2};
var_by_index(862) -> {fmove_1, 2};
var_by_index(863) -> {bif2_body, 3};
var_by_index(864) -> {l_new_bs_put_integer_imm, 1};
var_by_index(865) -> {self, 3};
var_by_index(866) -> {node, 3};
var_by_index(867) -> {l_call_ext, 76};
var_by_index(868) -> {l_call_ext, 75};
var_by_index(869) -> {l_call_ext, 74};
var_by_index(870) -> {move_return, 15};
var_by_index(871) -> {is_nonempty_list, 25};
var_by_index(872) -> {is_pid, 0};
var_by_index(873) -> {l_new_bs_put_float_imm, 0};
var_by_index(874) -> {l_move_call, 30};
var_by_index(875) -> {l_move_call, 29};
var_by_index(876) -> {l_move_call_ext, 26};
var_by_index(877) -> {test_heap_1_put_list, 3};
var_by_index(878) -> {catch_end, 6};
var_by_index(879) -> {l_is_ne_exact_immed, 8};
var_by_index(880) -> {l_move_call_ext_only, 6};
var_by_index(881) -> {l_catch, 8};
var_by_index(882) -> {l_call_ext, 77};
var_by_index(883) -> {extract_next_element3, 9};
var_by_index(884) -> {bs_context_to_binary, 2};
var_by_index(885) -> {l_new_bs_put_float_imm, 1};
var_by_index(886) -> {l_move_call, 31};
var_by_index(887) -> {l_move_call_last, 7};
var_by_index(888) -> {l_fetch, 20};
var_by_index(889) -> {l_is_ne_exact_immed, 9};
var_by_index(890) -> {init, 16};
var_by_index(891) -> {l_gc_bif1, 9};
var_by_index(892) -> {l_fetch, 21};
var_by_index(893) -> {l_catch, 7};
var_by_index(894) -> {is_tuple, 5};
var_by_index(895) -> {l_call_ext, 80};
var_by_index(896) -> {l_call_ext, 79};
var_by_index(897) -> {l_call_ext, 78};
var_by_index(898) -> {l_is_eq_exact_immed, 31};
var_by_index(899) -> {move_return, 17};
var_by_index(900) -> {move_return, 16};
var_by_index(901) -> {is_nil, 20};
var_by_index(902) -> {recv_mark, 0};
var_by_index(903) -> {l_bs_get_utf8, 1};
var_by_index(904) -> {is_nonempty_list, 26};
var_by_index(905) -> {try_end, 6};
var_by_index(906) -> {deallocate_return, 10};
var_by_index(907) -> {l_bs_get_integer_imm, 0};
var_by_index(908) -> {l_move_call, 32};
var_by_index(909) -> {catch_end, 7};
var_by_index(910) -> {l_recv_set, 0};
var_by_index(911) -> {move2, 9};
var_by_index(912) -> {l_bs_skip_bits_all2, 2};
var_by_index(913) -> {l_new_bs_put_integer, 1};
var_by_index(914) -> {test_heap_1_put_list, 4};
var_by_index(915) -> {self, 4};
var_by_index(916) -> {l_call_ext, 84};
var_by_index(917) -> {l_call_ext, 83};
var_by_index(918) -> {l_call_ext, 82};
var_by_index(919) -> {l_call_ext, 81};
var_by_index(920) -> {move_jump, 8};
var_by_index(921) -> {l_move_call, 33};
var_by_index(922) -> {l_apply, 0};
var_by_index(923) -> {get_list, 10};
var_by_index(924) -> {l_select_val2, 13};
var_by_index(925) -> {l_increment, 7};
var_by_index(926) -> {l_call_ext, 92};
var_by_index(927) -> {l_call_ext, 91};
var_by_index(928) -> {l_call_ext, 90};
var_by_index(929) -> {l_call_ext, 89};
var_by_index(930) -> {l_call_ext, 88};
var_by_index(931) -> {l_call_ext, 87};
var_by_index(932) -> {l_call_ext, 86};
var_by_index(933) -> {l_call_ext, 85};
var_by_index(934) -> {l_call_last, 11};
var_by_index(935) -> {extract_next_element3, 10};
var_by_index(936) -> {l_bs_get_float2, 0};
var_by_index(937) -> {move_jump, 9};
var_by_index(938) -> {move_return, 18};
var_by_index(939) -> {is_nil, 21};
var_by_index(940) -> {badmatch, 11};
var_by_index(941) -> {badmatch, 10};
var_by_index(942) -> {l_bs_put_string, 1};
var_by_index(943) -> {is_nonempty_list, 27};
var_by_index(944) -> {l_select_val2, 14};
var_by_index(945) -> {l_move_call_ext, 32};
var_by_index(946) -> {is_float, 0};
var_by_index(947) -> {l_increment, 6};
var_by_index(948) -> {get_list, 11};
var_by_index(949) -> {is_tuple, 6};
var_by_index(950) -> {l_call_ext, 95};
var_by_index(951) -> {l_call_ext, 94};
var_by_index(952) -> {l_call_ext, 93};
var_by_index(953) -> {extract_next_element, 21};
var_by_index(954) -> {l_fast_element, 4};
var_by_index(955) -> {extract_next_element2, 16};
var_by_index(956) -> {is_integer, 5};
var_by_index(957) -> {move_return, 20};
var_by_index(958) -> {move_return, 19};
var_by_index(959) -> {badmatch, 12};
var_by_index(960) -> {is_function, 0};
var_by_index(961) -> {init, 12};
var_by_index(962) -> {l_gc_bif1, 8};
var_by_index(963) -> {l_is_eq_exact_immed, 32};
var_by_index(964) -> {l_select_val2, 15};
var_by_index(965) -> {l_fetch, 22};
var_by_index(966) -> {l_call_ext, 101};
var_by_index(967) -> {l_call_ext, 100};
var_by_index(968) -> {l_call_ext, 99};
var_by_index(969) -> {l_call_ext, 98};
var_by_index(970) -> {l_call_ext, 97};
var_by_index(971) -> {l_call_ext, 96};
var_by_index(972) -> {extract_next_element, 22};
var_by_index(973) -> {bs_context_to_binary, 3};
var_by_index(974) -> {is_nonempty_list, 28};
var_by_index(975) -> {get_list, 12};
var_by_index(976) -> {try_end, 7};
var_by_index(977) -> {l_minus, 2};
var_by_index(978) -> {l_times, 2};
var_by_index(979) -> {l_bif1, 2};
var_by_index(980) -> {deallocate_return, 11};
var_by_index(981) -> {l_apply_fun, 0};
var_by_index(982) -> {l_call_ext_only, 3};
var_by_index(983) -> {l_bs_get_integer_16, 0};
var_by_index(984) -> {l_bs_init_bits, 0};
var_by_index(985) -> {move, 12};
var_by_index(986) -> {wait_timeout, 0};
var_by_index(987) -> {l_select_val2, 16};
var_by_index(988) -> {l_call_last, 10};
var_by_index(989) -> {is_nonempty_list, 29};
var_by_index(990) -> {is_atom, 5};
var_by_index(991) -> {l_bs_get_integer_32, 2};
var_by_index(992) -> {l_trim, 11};
var_by_index(993) -> {move_deallocate_return, 9};
var_by_index(994) -> {is_map, 0};
var_by_index(995) -> {extract_next_element, 23};
var_by_index(996) -> {extract_next_element2, 17};
var_by_index(997) -> {move_jump, 10};
var_by_index(998) -> {move_return, 21};
var_by_index(999) -> {l_trim, 9};
var_by_index(1000) -> {is_nil, 22};
var_by_index(1001) -> {bs_context_to_binary, 4};
var_by_index(1002) -> {badmatch, 13};
var_by_index(1003) -> {is_nonempty_list, 30};
var_by_index(1004) -> {l_new_bs_put_float_imm, 2};
var_by_index(1005) -> {l_bs_init_fail, 1};
var_by_index(1006) -> {l_is_ne_exact_immed, 10};
var_by_index(1007) -> {l_bs_restore2, 3};
var_by_index(1008) -> {l_fast_element, 5};
var_by_index(1009) -> {is_bitstr, 0};
var_by_index(1010) -> {is_integer_allocate, 1};
var_by_index(1011) -> {catch_end, 8};
var_by_index(1012) -> {move_jump, 11};
var_by_index(1013) -> {is_nil, 23};
var_by_index(1014) -> {is_nonempty_list, 32};
var_by_index(1015) -> {is_nonempty_list, 31};
var_by_index(1016) -> {l_move_call_ext, 43};
var_by_index(1017) -> {l_is_eq_exact_immed, 34};
var_by_index(1018) -> {l_is_eq_exact_immed, 33};
var_by_index(1019) -> {l_move_call_last, 8};
var_by_index(1020) -> {l_bs_test_tail_imm2, 0};
var_by_index(1021) -> {fconv, 2};
var_by_index(1022) -> {l_bs_save2, 2};
var_by_index(1023) -> {l_jump_on_val, 1};
var_by_index(1024) -> {l_bs_add, 1};
var_by_index(1025) -> {is_tuple, 8};
var_by_index(1026) -> {is_tuple, 7};
var_by_index(1027) -> {move_return, 24};
var_by_index(1028) -> {move_return, 23};
var_by_index(1029) -> {move_return, 22};
var_by_index(1030) -> {bs_context_to_binary, 6};
var_by_index(1031) -> {badmatch, 14};
var_by_index(1032) -> {is_nonempty_list, 35};
var_by_index(1033) -> {is_nonempty_list, 34};
var_by_index(1034) -> {is_nonempty_list, 33};
var_by_index(1035) -> {l_wait_timeout, 0};
var_by_index(1036) -> {l_gc_bif2, 0};
var_by_index(1037) -> {get_tuple_element, 10};
var_by_index(1038) -> {l_bs_put_utf16, 0};
var_by_index(1039) -> {l_new_bs_put_binary, 0};
var_by_index(1040) -> {l_bs_validate_unicode, 0};
var_by_index(1041) -> {l_bs_get_binary2, 2};
var_by_index(1042) -> {is_tuple, 9};
var_by_index(1043) -> {extract_next_element, 26};
var_by_index(1044) -> {extract_next_element, 25};
var_by_index(1045) -> {extract_next_element, 24};
var_by_index(1046) -> {move_return, 27};
var_by_index(1047) -> {move_return, 26};
var_by_index(1048) -> {move_return, 25};
var_by_index(1049) -> {is_nil, 24};
var_by_index(1050) -> {bs_context_to_binary, 5};
var_by_index(1051) -> {move, 11};
var_by_index(1052) -> {l_bs_utf16_size, 0};
var_by_index(1053) -> {l_is_eq_exact_immed, 35};
var_by_index(1054) -> {raise, 1};
var_by_index(1055) -> {move_jump, 12};
var_by_index(1056) -> {move_return, 28};
var_by_index(1057) -> {is_nil, 27};
var_by_index(1058) -> {is_nil, 26};
var_by_index(1059) -> {is_nil, 25};
var_by_index(1060) -> {is_reference, 0};
var_by_index(1061) -> {l_int_bnot, 0};
var_by_index(1062) -> {l_bs_get_binary_imm2, 1};
var_by_index(1063) -> {l_is_function2, 0};
var_by_index(1064) -> {fmove_2, 2};
var_by_index(1065) -> {is_nil, 28};
var_by_index(1066) -> {badmatch, 15};
var_by_index(1067) -> {is_nonempty_list, 37};
var_by_index(1068) -> {is_nonempty_list, 36};
var_by_index(1069) -> {init, 14};
var_by_index(1070) -> {init, 13};
var_by_index(1071) -> {is_port, 0};
var_by_index(1072) -> {is_nil, 29};
var_by_index(1073) -> {l_new_bs_put_binary_all, 2};
var_by_index(1074) -> {is_nonempty_list, 38};
var_by_index(1075) -> {is_number, 0};
var_by_index(1076) -> {l_select_tuple_arity, 3};
var_by_index(1077) -> {l_bs_get_binary_all_reuse, 1};
var_by_index(1078) -> {l_wait_timeout, 1};
var_by_index(1079) -> {l_trim, 10};
var_by_index(1080) -> {l_bs_put_utf8, 0};
var_by_index(1081) -> {l_fnegate, 0};
var_by_index(1082) -> {l_bs_init, 0};
var_by_index(1083) -> {l_apply_last, 0};
var_by_index(1084) -> {l_jump_on_val, 2};
var_by_index(1085) -> {move, 13};
var_by_index(1086) -> {l_bs_utf8_size, 0};
var_by_index(1087) -> {l_bs_get_binary_imm2, 2};
var_by_index(1088) -> {l_new_bs_put_float, 0};
var_by_index(1089) -> {l_bs_validate_unicode_retract, 0};
var_by_index(1090) -> {init, 15};
var_by_index(1091) -> {l_bxor, 0};
var_by_index(1092) -> {l_gc_bif3, 0};
var_by_index(1093) -> {l_bs_match_string, 2};
var_by_index(1094) -> {l_bs_get_integer_imm, 1};
var_by_index(1095) -> {l_bs_get_integer_8, 2};
var_by_index(1096) -> {l_bor, 2};
var_by_index(1097) -> {l_bs_start_match2, 3};
var_by_index(1098) -> {l_new_bs_put_binary_imm, 0};
var_by_index(1099) -> {l_rem, 2};
var_by_index(1100) -> {l_bs_get_integer_small_imm, 2};
var_by_index(1101) -> {l_bsl, 2};
var_by_index(1102) -> {on_load, 0};
var_by_index(1103) -> {move2, 10};
var_by_index(1104) -> {l_int_div, 2};
var_by_index(1105) -> {l_bs_test_unit, 0};
var_by_index(1106) -> {l_select_val_atoms, 3};
var_by_index(1107) -> {l_m_div, 0};
var_by_index(1108) -> {l_hibernate, 0};
var_by_index(1109) -> {l_apply_fun_last, 0};
var_by_index(1110) -> {l_select_val_smallints, 2};
var_by_index(1111) -> {update_map_assoc, 0};
var_by_index(1112) -> {get_map_elements, 0};
var_by_index(1113) -> {has_map_fields, 0};
var_by_index(1114) -> {is_function2, 0};
var_by_index(1115) -> {l_apply_only, 0};
var_by_index(1116) -> {l_apply_fun_only, 0};
var_by_index(1117) -> {l_band, 2};
var_by_index(1118) -> {update_map_exact, 0};
var_by_index(1119) -> {get_map_element, 0};
var_by_index(1120) -> {has_map_field, 0};
var_by_index(1121) -> {is_bigint, 0};
var_by_index(1122) -> {test_heap, 1};
var_by_index(1123) -> {func_info, 0};
var_by_index(1124) -> {call_bif, 0};
var_by_index(1125) -> {l_bs_get_utf16, 2};
var_by_index(1126) -> {l_put_tuple, 9};
var_by_index(1127) -> {get_tuple_element, 11};
var_by_index(1128) -> {allocate_init, 1};
var_by_index(1129) -> {l_call_fun_last, 1};
var_by_index(1130) -> {set_tuple_element, 2};
var_by_index(1131) -> {l_bsr, 2};
var_by_index(1132) -> {l_bs_get_integer_32, 3};
var_by_index(1133) -> {allocate_heap, 1};
var_by_index(1134) -> {is_tuple_of_arity, 4};
var_by_index(1135) -> {test_arity, 4};
var_by_index(1136) -> {is_nonempty_list_allocate, 2};
var_by_index(1137) -> {l_bs_append, 2};
var_by_index(1138) -> {try_case_end, 1};
var_by_index(1139) -> {init3, 1};
var_by_index(1140) -> {l_select_tuple_arity2, 3};
var_by_index(1141) -> {init2, 1};
var_by_index(1142) -> {l_bs_get_binary_all2, 2};
var_by_index(1143) -> {is_nonempty_list_test_heap, 1};
var_by_index(1144) -> {allocate_heap_zero, 1};
var_by_index(1145) -> {l_bs_init_heap_bin, 1};
var_by_index(1146) -> {l_plus, 3};
var_by_index(1147) -> {l_bs_get_integer, 1};

var_by_index(Index) -> erlang:error({novarat,Index}).

%%EOF


