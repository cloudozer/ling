
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
	{move2,1},
	{get_tuple_element,0},
	{badmatch,0},
	{put_list,0},
	{is_tuple_of_arity,1},
	{get_tuple_element,1},
	{move2,0},
	{move,4},
	{l_call_only,0},
	{l_is_eq_exact_immed,0},
	{l_is_eq_exact_immed,1},
	{is_tuple_of_arity,0},
	{put_list,1},
	{get_list,0},
	{l_put_tuple,1},
	{l_is_ge,0},
	{l_call_ext,105},
	{l_make_fun,0},
	{l_move_call_last,0},
	{extract_next_element2,0},
	{is_tuple_of_arity,2},
	{return,0},
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
	{l_is_eq_exact_immed,2},
	{l_allocate,1},
	{jump,0},
	{move_return,36},
	{deallocate_return,0},
	{case_end,0},
	{get_list,1},
	{call_bif,7},
	{move2,2},
	{l_is_eq_exact,0},
	{l_move_call,35},
	{get_tuple_element,2},
	{l_allocate,2},
	{move_return,0},
	{l_is_eq_exact_immed,3},
	{l_select_val2,0},
	{get_tuple_element,3},
	{move,5},
	{l_fetch,2},
	{init2,0},
	{get_list,2},
	{l_is_eq_exact_immed,4},
	{init,0},
	{deallocate_return,1},
	{put_list,3},
	{call_bif,8},
	{extract_next_element,1},
	{is_tuple_of_arity,3},
	{get_tuple_element,4},
	{move2,4},
	{l_fetch,3},
	{init,1},
	{l_put_tuple,2},
	{move2,5},
	{l_allocate,3},
	{deallocate_return,2},
	{l_fetch,4},
	{init3,0},
	{is_nonempty_list,1},
	{l_move_call_ext,49},
	{l_is_eq_exact_immed,5},
	{l_allocate_zero,0},
	{call_bif,6},
	{l_trim,1},
	{allocate_init,0},
	{allocate_heap,0},
	{test_heap_1_put_list,0},
	{l_allocate_zero,1},
	{init,2},
	{move_deallocate_return,1},
	{l_call_last,0},
	{call_bif,48},
	{l_is_eq_exact_literal,0},
	{is_nonempty_list,2},
	{l_move_call,0},
	{move_return,1},
	{is_tuple,0},
	{l_is_eq_exact_immed,6},
	{is_list,0},
	{l_call_last,1},
	{deallocate_return,3},
	{is_nonempty_list_allocate,0},
	{l_move_call_only,0},
	{call_bif,5},
	{extract_next_element,2},
	{l_increment,0},
	{move,6},
	{l_is_lt,0},
	{l_trim,2},
	{l_gc_bif1,0},
	{l_select_val_atoms,0},
	{l_call_last,2},
	{move_deallocate_return,2},
	{is_nonempty_list,3},
	{l_bs_start_match2,0},
	{l_new_bs_put_integer_imm,0},
	{is_nil,1},
	{extract_next_element2,1},
	{l_select_val2,2},
	{remove_message,0},
	{l_move_call_only,10},
	{l_move_call_only,1},
	{init,3},
	{l_catch,0},
	{l_allocate_zero,2},
	{extract_next_element3,0},
	{get_tuple_element,5},
	{l_call_ext,0},
	{move_jump,12},
	{l_fetch,5},
	{extract_next_element,3},
	{is_nil,2},
	{move2,6},
	{l_allocate,4},
	{catch_end,0},
	{test_arity,0},
	{l_allocate_zero,3},
	{l_is_eq_exact_immed,7},
	{move_return,2},
	{move_return,3},
	{l_move_call_ext,0},
	{put_list,4},
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
	{l_bs_match_string,0},
	{bif1_body,0},
	{l_move_call_only,2},
	{l_move_call_last,1},
	{is_nonempty_list,6},
	{l_bs_test_zero_tail2,0},
	{l_is_eq,0},
	{send,0},
	{set_tuple_element,0},
	{l_catch,1},
	{call_bif,9},
	{l_is_ne,0},
	{l_move_call_only,3},
	{l_move_call_ext_last,0},
	{l_select_val2,1},
	{l_select_val_smallints,0},
	{l_move_call_ext,1},
	{l_call_ext_only,4},
	{l_is_ne_exact_immed,0},
	{l_increment,1},
	{l_bs_add,0},
	{bif2_body,0},
	{is_nonempty_list,7},
	{extract_next_element,4},
	{case_end,1},
	{l_is_eq_exact_immed,9},
	{extract_next_element2,2},
	{move_deallocate_return,3},
	{extract_next_element2,3},
	{l_loop_rec,0},
	{l_call_ext,1},
	{l_select_tuple_arity2,1},
	{l_move_call,2},
	{l_move_call,3},
	{l_move_call_ext,2},
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
	{call_bif,10},
	{deallocate_return,5},
	{move,8},
	{l_fetch,6},
	{is_nonempty_list,9},
	{extract_next_element,5},
	{l_select_val2,3},
	{l_allocate_zero,5},
	{l_allocate,5},
	{init,6},
	{l_move_call_ext,3},
	{badmatch,1},
	{l_call_last,5},
	{l_move_call_ext,4},
	{l_plus,1},
	{l_bs_start_match2,1},
	{l_move_call_ext,5},
	{l_call_last,6},
	{badmatch,2},
	{l_move_call_ext,6},
	{l_call_fun_last,0},
	{l_move_call_ext,7},
	{l_trim,4},
	{l_increment,2},
	{put_list,5},
	{call_bif,11},
	{int_code_end,0},
	{l_move_call_ext_only,0},
	{l_bs_save2,0},
	{call_bif,12},
	{extract_next_element,6},
	{bs_context_to_binary,0},
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
	{call_bif,13},
	{is_tuple,1},
	{l_move_call_last,2},
	{l_move_call,5},
	{l_is_eq_exact_immed,11},
	{badmatch,3},
	{l_move_call,6},
	{is_nil,5},
	{is_nonempty_list_allocate,1},
	{l_call_ext,2},
	{l_bs_test_zero_tail2,1},
	{l_catch,2},
	{l_move_call_ext,8},
	{call_bif,14},
	{is_atom,0},
	{l_bs_test_zero_tail2,2},
	{l_move_call_ext_only,6},
	{l_select_val_smallints,1},
	{l_fast_element,0},
	{is_nonempty_list,10},
	{l_bs_match_string,1},
	{call_bif,15},
	{l_call_ext,3},
	{move_return,6},
	{l_bs_test_unit_8,0},
	{l_call_ext_last,1},
	{is_nil,6},
	{allocate_heap_zero,0},
	{is_nonempty_list,11},
	{l_call_ext_last,2},
	{call_bif,16},
	{self,1},
	{extract_next_element,8},
	{init,7},
	{case_end,2},
	{extract_next_element3,1},
	{is_nil,7},
	{l_move_call,7},
	{call_bif,17},
	{raise,0},
	{catch_end,2},
	{l_is_eq_exact_literal,1},
	{l_is_eq_exact_literal,7},
	{l_move_call_ext,9},
	{l_minus,0},
	{extract_next_element2,5},
	{l_allocate,6},
	{l_bif2,0},
	{try_end,0},
	{l_call_fun,1},
	{call_bif,18},
	{deallocate_return,6},
	{l_call_ext,4},
	{l_move_call_last,3},
	{l_put_tuple,5},
	{l_move_call_ext_only,1},
	{l_band,0},
	{l_move_call,8},
	{l_trim,5},
	{extract_next_element,9},
	{call_bif,19},
	{l_call_ext,5},
	{l_call_ext,6},
	{l_bs_get_binary_all2,0},
	{get_tuple_element,7},
	{l_minus,1},
	{l_is_eq_exact_immed,12},
	{extract_next_element3,2},
	{is_integer,0},
	{test_heap_1_put_list,1},
	{extract_next_element2,6},
	{l_catch,3},
	{extract_next_element,26},
	{l_bif2,1},
	{is_nil,8},
	{l_move_call_only,4},
	{move_deallocate_return,5},
	{move_jump,0},
	{is_list,1},
	{l_move_call,9},
	{l_bsr,0},
	{l_call_ext,7},
	{l_is_eq_exact_immed,13},
	{apply,0},
	{l_fast_element,2},
	{l_fetch,7},
	{l_bs_get_integer_small_imm,0},
	{l_bif2,2},
	{l_bs_get_integer_8,0},
	{l_bs_test_zero_tail2,3},
	{set_tuple_element,1},
	{try_end,1},
	{l_is_eq_exact_literal,2},
	{l_is_eq_exact_immed,10},
	{extract_next_element2,7},
	{l_is_eq_exact_literal,3},
	{l_select_tuple_arity,0},
	{l_allocate_zero,6},
	{l_fmul,0},
	{l_call_ext,8},
	{l_bsl,0},
	{is_nonempty_list,12},
	{move,9},
	{call_bif,20},
	{l_move_call_ext,10},
	{l_times,0},
	{l_move_call_ext_only,2},
	{l_put_tuple,6},
	{l_bs_init_fail,0},
	{l_is_eq_exact_immed,14},
	{l_times,1},
	{l_is_eq_exact_immed,15},
	{l_move_call_ext,11},
	{l_call_ext,9},
	{l_move_call_ext,12},
	{l_fetch,8},
	{extract_next_element,10},
	{is_nil,9},
	{call_bif,21},
	{node,0},
	{l_call_last,7},
	{l_bs_get_binary2,0},
	{is_tuple,2},
	{l_call_fun,2},
	{test_arity,2},
	{get_list,4},
	{l_bs_get_integer_8,1},
	{l_bs_test_zero_tail2,5},
	{catch_end,3},
	{l_bif2,3},
	{call_bif,22},
	{l_is_ne_exact_immed,2},
	{l_is_ne_exact,0},
	{l_bif2,4},
	{is_binary,0},
	{l_allocate_zero,9},
	{l_is_eq_exact_immed,16},
	{extract_next_element,11},
	{l_bs_get_integer_32,0},
	{l_call_ext,10},
	{is_atom,1},
	{l_select_val2,7},
	{l_fcheckerror,0},
	{fclearerror,0},
	{l_fetch,9},
	{l_new_bs_put_binary_all,0},
	{extract_next_element3,3},
	{node,1},
	{extract_next_element,12},
	{l_move_call,10},
	{get_list,5},
	{move_deallocate_return,6},
	{l_move_call_last,4},
	{l_new_bs_put_binary_all,1},
	{l_is_eq_exact_immed,17},
	{timeout,0},
	{deallocate_return,7},
	{l_get,0},
	{l_move_call,11},
	{l_fetch,10},
	{l_move_call_ext_last,1},
	{is_nil,10},
	{l_fetch,11},
	{call_bif,23},
	{l_call_ext,11},
	{is_float,1},
	{l_select_val2,5},
	{l_select_val2,8},
	{l_is_eq_exact_immed,36},
	{call_bif,24},
	{l_band,1},
	{l_get,1},
	{is_binary,1},
	{l_fast_element,1},
	{l_get,2},
	{is_nonempty_list,39},
	{l_move_call_ext,14},
	{extract_next_element,13},
	{is_nonempty_list,13},
	{l_move_call,12},
	{l_call_ext,12},
	{l_is_eq_exact_immed,18},
	{l_move_call,13},
	{l_move_call_ext,15},
	{l_rem,0},
	{move2,8},
	{l_bsl,1},
	{l_call_ext,13},
	{l_allocate_zero,7},
	{l_call_last,8},
	{is_nil,11},
	{l_gc_bif1,2},
	{l_is_ne_exact_immed,10},
	{extract_next_element3,4},
	{l_increment,4},
	{is_nonempty_list,14},
	{is_nil,30},
	{extract_next_element3,10},
	{l_bs_append,0},
	{call_bif,25},
	{l_trim,6},
	{is_nil,12},
	{l_move_call_ext,16},
	{is_integer,5},
	{l_call_ext,14},
	{l_bor,0},
	{move_return,7},
	{is_list,2},
	{l_select_val2,15},
	{l_call_ext,15},
	{is_nil,13},
	{l_catch,4},
	{l_element,1},
	{extract_next_element2,17},
	{l_gc_bif1,5},
	{l_fadd,0},
	{call_bif,26},
	{l_allocate,7},
	{l_move_call_only,5},
	{is_nonempty_list,15},
	{l_move_call_ext,17},
	{l_move_call_ext,18},
	{call_bif,28},
	{call_bif,27},
	{is_integer,1},
	{bif1_body,1},
	{l_is_ne_exact_immed,3},
	{l_call_ext,16},
	{l_call_ext_last,3},
	{l_fetch,12},
	{l_move_call_ext,19},
	{l_is_eq_exact_immed,19},
	{is_nonempty_list,16},
	{bif1_body,2},
	{fmove_1,0},
	{l_move_call_ext,20},
	{call_bif,29},
	{move_jump,1},
	{l_bs_get_utf8,0},
	{l_move_call_ext,21},
	{l_bs_skip_bits_all2,0},
	{bif2_body,1},
	{l_call_ext,17},
	{call_bif,30},
	{fconv,0},
	{l_is_eq_exact_literal,4},
	{l_is_eq_exact_immed,20},
	{call_bif,31},
	{extract_next_element3,5},
	{extract_next_element2,8},
	{system_limit,0},
	{l_move_call_ext_last,2},
	{l_fetch,23},
	{call_bif,32},
	{call_bif,4},
	{apply_last,0},
	{is_atom,2},
	{l_move_call_ext_only,3},
	{l_bor,1},
	{l_is_eq_exact_immed,22},
	{put_list,6},
	{call_bif,33},
	{extract_next_element,14},
	{l_catch,5},
	{init,8},
	{put_list,8},
	{l_make_export,0},
	{l_select_val2,6},
	{move_deallocate_return,7},
	{l_call_fun,3},
	{call_bif,34},
	{badmatch,4},
	{is_atom,3},
	{l_move_call,14},
	{l_bs_test_zero_tail2,4},
	{fmove_1,1},
	{fmove_2,0},
	{l_new_bs_put_integer,0},
	{fmove_2,1},
	{call_bif,35},
	{is_integer_allocate,0},
	{l_call_ext_last,4},
	{l_move_call_ext,22},
	{l_call_ext,18},
	{l_fast_element,3},
	{l_move_call,15},
	{l_move_call_ext,23},
	{l_fetch,13},
	{l_trim,7},
	{l_bs_restore2,1},
	{deallocate_return,8},
	{is_list,6},
	{l_call_ext,19},
	{extract_next_element,15},
	{l_fetch,14},
	{l_is_eq_exact_immed,23},
	{is_tuple,3},
	{is_nonempty_list,17},
	{l_move_call_ext,13},
	{l_fetch,15},
	{l_move_call_last,5},
	{extract_next_element,16},
	{extract_next_element2,9},
	{try_end,2},
	{if_end,0},
	{l_get,3},
	{call_bif,36},
	{case_end,11},
	{l_move_call,16},
	{call_bif,38},
	{call_bif,37},
	{move_jump,2},
	{try_end,4},
	{try_end,3},
	{l_is_ne_exact_immed,4},
	{call_bif,39},
	{self,2},
	{l_call_ext,23},
	{l_call_ext,22},
	{l_call_ext,21},
	{l_call_ext,20},
	{move_return,8},
	{is_list,3},
	{is_nonempty_list,18},
	{try_end,5},
	{catch_end,4},
	{l_move_call_ext_last,3},
	{l_call_ext,25},
	{l_call_ext,24},
	{extract_next_element2,10},
	{is_nonempty_list,19},
	{init,9},
	{l_call_ext,26},
	{case_end,3},
	{bif1_body,3},
	{get_tuple_element,8},
	{test_arity,3},
	{l_bs_skip_bits_imm2,0},
	{l_bif1,0},
	{l_bs_get_utf16,1},
	{is_tuple,4},
	{l_call_ext,28},
	{l_call_ext,27},
	{extract_next_element,17},
	{l_bs_init_bits_fail,0},
	{l_select_val_atoms,2},
	{l_is_eq_exact_immed,24},
	{l_select_val2,9},
	{l_call_ext,29},
	{is_integer,2},
	{badmatch,5},
	{try_case_end,0},
	{l_fdiv,0},
	{is_tuple,10},
	{l_get,6},
	{l_call_ext_last,6},
	{l_bif1,1},
	{l_move_call_ext,24},
	{l_move_call_ext_last,5},
	{get_list,6},
	{case_end,4},
	{put_list,9},
	{l_call_ext,30},
	{l_bs_test_unit_8,1},
	{l_gc_bif1,3},
	{self,5},
	{l_increment,9},
	{call_bif,41},
	{call_bif,40},
	{extract_next_element,18},
	{extract_next_element2,11},
	{move_return,9},
	{is_nil,14},
	{l_move_call_only,6},
	{l_bs_restore2,2},
	{l_move_call,18},
	{l_move_call,17},
	{l_is_eq_exact_immed,25},
	{bif1_body,5},
	{move,10},
	{is_integer,3},
	{l_allocate_zero,8},
	{is_nil,15},
	{is_list,4},
	{case_end,5},
	{is_atom,6},
	{l_move_call_ext,26},
	{call_bif,42},
	{get_tuple_element,9},
	{l_move_call_ext,27},
	{l_is_eq_exact_immed,26},
	{l_increment,5},
	{l_fsub,0},
	{l_increment,7},
	{put_list,7},
	{l_move_call_ext,28},
	{call_bif,43},
	{l_call_ext,31},
	{extract_next_element3,6},
	{l_bsr,1},
	{badmatch,6},
	{l_move_call_ext,30},
	{l_move_call_ext,29},
	{l_call_ext,33},
	{l_call_ext,32},
	{extract_next_element2,12},
	{is_integer,4},
	{is_nonempty_list,20},
	{l_move_call,19},
	{l_is_eq_exact_literal,5},
	{l_call_ext_only,0},
	{l_plus,2},
	{move_deallocate_return,8},
	{move_return,10},
	{l_is_eq_exact_literal,6},
	{l_int_div,0},
	{l_rem,1},
	{l_bs_get_binary_imm2,0},
	{l_move_call_ext,31},
	{l_call_ext,34},
	{l_move_call,20},
	{init,10},
	{catch_end,5},
	{node,4},
	{put_list,10},
	{l_allocate,9},
	{l_bif2,5},
	{is_atom,4},
	{l_move_call,21},
	{l_fetch,16},
	{l_increment,6},
	{bif1_body,6},
	{l_fetch,17},
	{test_heap_1_put_list,2},
	{call_bif,44},
	{node,2},
	{l_call_ext,37},
	{l_call_ext,36},
	{l_call_ext,35},
	{l_call_last,9},
	{is_nil,16},
	{l_bs_put_string,0},
	{is_list,5},
	{case_end,6},
	{l_yield,0},
	{l_move_call,22},
	{l_catch,6},
	{get_list,7},
	{l_bs_get_binary_all2,1},
	{l_is_eq_exact_immed,27},
	{l_call_ext,38},
	{case_end,7},
	{is_nonempty_list,21},
	{l_move_call_ext,33},
	{l_select_tuple_arity,3},
	{l_move_call_ext,34},
	{l_bs_append,1},
	{l_bs_get_integer,0},
	{l_move_call_last,6},
	{call_bif,46},
	{call_bif,45},
	{l_call_ext,39},
	{l_is_eq_exact_immed,21},
	{is_nonempty_list,22},
	{is_function,1},
	{l_gc_bif1,4},
	{l_is_ne_exact_immed,5},
	{l_jump_on_val,0},
	{call_bif,47},
	{l_call_ext,40},
	{l_is_ne_exact_immed,6},
	{extract_next_element2,13},
	{move_jump,3},
	{badmatch,7},
	{l_bs_test_unit_8,2},
	{l_move_call,24},
	{l_move_call,23},
	{l_is_eq_exact_immed,28},
	{l_call_ext,41},
	{is_nil,18},
	{is_nil,17},
	{is_nonempty_list,23},
	{l_move_call,27},
	{l_move_call,26},
	{l_move_call,25},
	{l_select_tuple_arity,1},
	{l_fetch,18},
	{l_move_call_ext,35},
	{l_bs_get_integer_32,1},
	{fmove_1,3},
	{l_int_div,1},
	{l_is_eq_exact_immed,29},
	{l_move_call_ext_only,4},
	{l_move_call_ext,36},
	{call_bif,1},
	{l_call_ext,48},
	{l_call_ext,47},
	{l_call_ext,46},
	{l_call_ext,45},
	{l_call_ext,44},
	{l_call_ext,43},
	{l_call_ext,42},
	{extract_next_element3,7},
	{move_return,11},
	{l_bs_test_unit_8,3},
	{l_bs_save2,1},
	{bif2_body,2},
	{is_binary,3},
	{l_move_call_ext,37},
	{bif1_body,7},
	{l_allocate,8},
	{l_call_ext,50},
	{l_call_ext,49},
	{move_return,12},
	{l_trim,8},
	{is_nil,19},
	{case_end,8},
	{fconv,1},
	{deallocate_return,12},
	{l_catch,8},
	{l_move_call_ext_last,4},
	{l_gc_bif1,6},
	{move_deallocate_return,10},
	{l_move_call_ext,38},
	{l_call_ext,53},
	{l_call_ext,52},
	{l_call_ext,51},
	{extract_next_element,19},
	{l_call_fun,4},
	{l_move_call_ext_only,5},
	{l_move_call_ext,40},
	{l_move_call_ext,39},
	{l_bs_put_string,2},
	{l_is_ne_exact_literal,0},
	{l_call_ext,54},
	{move_return,13},
	{badmatch,8},
	{l_move_call_ext,42},
	{l_move_call_ext,41},
	{l_bs_test_unit_8,4},
	{is_pid,1},
	{is_boolean,0},
	{l_get,4},
	{bif1_body,4},
	{l_fetch,19},
	{bs_context_to_binary,5},
	{l_select_tuple_arity2,2},
	{l_call_ext_last,5},
	{l_select_val2,10},
	{l_call_ext,58},
	{l_call_ext,57},
	{l_call_ext,56},
	{l_call_ext,55},
	{extract_next_element2,14},
	{move_jump,4},
	{l_move_call_only,7},
	{l_bs_skip_bits2,0},
	{put_list,12},
	{l_bs_get_binary2,1},
	{l_call_ext,65},
	{l_call_ext,64},
	{l_call_ext,63},
	{l_call_ext,62},
	{l_call_ext,61},
	{l_call_ext,60},
	{l_call_ext,59},
	{extract_next_element,20},
	{extract_next_element2,15},
	{move_return,14},
	{bs_context_to_binary,1},
	{l_move_call,28},
	{init,11},
	{l_bs_skip_bits2,1},
	{l_move_call_last,7},
	{l_call_ext,68},
	{l_call_ext,67},
	{l_call_ext,66},
	{move_jump,6},
	{move_jump,5},
	{move_return,15},
	{is_nonempty_list_test_heap,0},
	{l_move_call_ext,44},
	{catch_end,6},
	{l_get,5},
	{l_bs_skip_bits_all2,1},
	{l_bs_start_match2,2},
	{l_move_call_ext,46},
	{l_wait_timeout,2},
	{l_element,0},
	{l_move_call_ext,47},
	{l_move_call_ext,48},
	{l_call_ext,70},
	{l_call_ext,69},
	{l_move_call_only,8},
	{case_end,9},
	{is_pid,0},
	{l_move_call,29},
	{l_select_tuple_arity,2},
	{is_nonempty_list_test_heap,1},
	{catch_end,8},
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
	{is_nonempty_list,25},
	{is_nonempty_list,24},
	{try_end,6},
	{l_bs_private_append,0},
	{deallocate_return,9},
	{l_new_bs_put_float_imm,0},
	{l_move_call,30},
	{l_call_ext_only,1},
	{l_apply,0},
	{l_move_call_ext,25},
	{fmove_1,2},
	{put_list,11},
	{l_bif2,6},
	{bif2_body,3},
	{self,3},
	{is_tuple,5},
	{l_call_ext,76},
	{l_call_ext,75},
	{l_call_ext,74},
	{extract_next_element3,8},
	{l_new_bs_put_float_imm,1},
	{l_move_call,33},
	{l_move_call,32},
	{l_move_call,31},
	{l_catch,7},
	{catch_end,7},
	{l_is_ne_exact_immed,7},
	{test_heap_1_put_list,3},
	{l_call_ext,77},
	{extract_next_element3,9},
	{move_jump,8},
	{is_nonempty_list,26},
	{l_move_call,34},
	{is_binary,2},
	{l_fetch,21},
	{l_fetch,20},
	{l_bs_skip_bits_all2,2},
	{l_is_ne_exact_immed,8},
	{l_bs_get_utf8,1},
	{l_bs_get_integer_imm,0},
	{l_select_val2,11},
	{self,4},
	{l_call_ext,79},
	{l_call_ext,78},
	{l_call_last,10},
	{move_return,18},
	{is_nil,20},
	{recv_mark,0},
	{badmatch,10},
	{badmatch,9},
	{is_function,0},
	{l_recv_set,0},
	{l_new_bs_put_integer,1},
	{badmatch,17},
	{move2,9},
	{l_call_ext,88},
	{l_call_ext,87},
	{l_call_ext,86},
	{l_call_ext,85},
	{l_call_ext,84},
	{l_call_ext,83},
	{l_call_ext,82},
	{l_call_ext,81},
	{l_call_ext,80},
	{extract_next_element2,16},
	{move_jump,9},
	{is_nil,21},
	{l_bs_put_string,1},
	{l_select_val2,12},
	{is_atom,5},
	{l_bs_init_bits_fail,1},
	{l_bs_get_integer_16,0},
	{l_move_call_ext,32},
	{is_float,0},
	{l_is_eq_exact_immed,30},
	{l_bs_get_float2,0},
	{l_trim,11},
	{l_call_ext,91},
	{l_call_ext,90},
	{l_call_ext,89},
	{l_is_ne_exact_immed,9},
	{l_fast_element,4},
	{move_return,20},
	{move_return,19},
	{bs_context_to_binary,3},
	{is_nonempty_list,28},
	{is_nonempty_list,27},
	{deallocate_return,11},
	{deallocate_return,10},
	{l_bs_init_bits,0},
	{l_increment,8},
	{l_bif1,2},
	{l_minus,2},
	{is_tuple,6},
	{l_call_ext,98},
	{l_call_ext,97},
	{l_call_ext,96},
	{l_call_ext,95},
	{l_call_ext,94},
	{l_call_ext,93},
	{l_call_ext,92},
	{l_call_last,11},
	{l_is_eq_exact_immed,31},
	{extract_next_element,22},
	{move_return,21},
	{badmatch,11},
	{case_end,10},
	{get_list,10},
	{get_list,8},
	{l_select_val2,13},
	{l_call_ext_only,2},
	{l_fetch,22},
	{l_call_ext,104},
	{l_call_ext,103},
	{l_call_ext,102},
	{l_call_ext,101},
	{l_call_ext,100},
	{l_call_ext,99},
	{l_is_eq_exact_immed,32},
	{get_tuple_element,10},
	{wait_timeout,0},
	{move_return,22},
	{badmatch,12},
	{is_nonempty_list,29},
	{l_times,2},
	{l_select_val2,14},
	{l_apply_fun,0},
	{init,17},
	{l_gc_bif1,7},
	{move,12},
	{l_is_eq_exact_immed,33},
	{l_bs_get_integer_32,2},
	{move_return,24},
	{move_return,23},
	{is_nil,22},
	{badmatch,13},
	{move_deallocate_return,9},
	{l_new_bs_put_float_imm,2},
	{l_call_ext_only,3},
	{is_integer_allocate,1},
	{is_tuple,8},
	{is_tuple,7},
	{l_is_eq_exact_immed,34},
	{extract_next_element,23},
	{move_jump,10},
	{is_nil,23},
	{bs_context_to_binary,4},
	{badmatch,14},
	{is_nonempty_list,32},
	{is_nonempty_list,31},
	{is_nonempty_list,30},
	{get_list,9},
	{l_bs_init_fail,1},
	{l_move_call_ext,43},
	{l_fast_element,5},
	{l_bs_test_tail_imm2,0},
	{move_return,25},
	{is_nil,24},
	{l_move_call_only,9},
	{fconv,2},
	{is_nonempty_list,34},
	{is_nonempty_list,33},
	{l_move_call_ext,45},
	{init,12},
	{test_heap_1_put_list,4},
	{l_bs_add,1},
	{l_wait_timeout,0},
	{l_gc_bif2,0},
	{try_end,7},
	{l_bs_put_utf16,0},
	{is_tuple,9},
	{l_is_eq_exact_immed,35},
	{extract_next_element,25},
	{extract_next_element,24},
	{move_jump,11},
	{move_return,27},
	{move_return,26},
	{l_trim,9},
	{is_nil,25},
	{l_bs_validate_unicode,0},
	{is_nonempty_list,35},
	{l_jump_on_val,1},
	{move,11},
	{l_bs_utf16_size,0},
	{l_bs_get_binary2,2},
	{move_return,29},
	{move_return,28},
	{is_nil,26},
	{raise,1},
	{l_bs_restore2,3},
	{l_int_bnot,0},
	{l_bs_get_binary_imm2,1},
	{is_bitstr,0},
	{move_return,35},
	{move_return,34},
	{move_return,33},
	{move_return,32},
	{move_return,31},
	{move_return,30},
	{is_nil,29},
	{is_nil,28},
	{is_nil,27},
	{l_new_bs_put_binary_all,2},
	{l_new_bs_put_binary,0},
	{badmatch,16},
	{badmatch,15},
	{is_nonempty_list,37},
	{is_nonempty_list,36},
	{l_bs_save2,2},
	{fmove_2,2},
	{is_reference,0},
	{is_port,0},
	{is_nonempty_list,38},
	{is_number,0},
	{l_select_tuple_arity,4},
	{move,13},
	{l_bs_get_binary_all_reuse,1},
	{init,13},
	{l_wait_timeout,1},
	{l_trim,10},
	{l_fnegate,0},
	{l_bs_init,0},
	{l_jump_on_val,2},
	{init,14},
	{l_bs_put_utf8,0},
	{l_bs_utf8_size,0},
	{l_bxor,0},
	{l_new_bs_put_float,0},
	{l_bs_validate_unicode_retract,0},
	{l_apply_last,0},
	{init,16},
	{init,15},
	{l_bs_get_binary_imm2,2},
	{l_is_function2,0},
	{l_gc_bif3,0},
	{l_bs_match_string,2},
	{l_bs_get_integer_imm,1},
	{l_bor,2},
	{l_new_bs_put_binary_imm,0},
	{l_bs_get_integer_8,2},
	{l_bs_start_match2,3},
	{l_rem,2},
	{l_bs_get_integer_small_imm,1},
	{l_bsl,2},
	{l_apply_only,0},
	{on_load,0},
	{move2,10},
	{l_int_div,2},
	{l_bs_test_unit,0},
	{l_select_val_atoms,3},
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
	{l_bsr,2},
	{l_bs_get_integer_32,3},
	{allocate_heap,1},
	{is_tuple_of_arity,4},
	{test_arity,4},
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
fit_args(badmatch, [{y,8}]) -> 16;
fit_args(badmatch, [{x,6}]) -> 15;
fit_args(badmatch, [_]) -> 17;
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
fit_args(call_bif, [{b,{erlang,error,1}}]) -> 3;
fit_args(call_bif, [{b,{erlang,setelement,3}}]) -> 7;
fit_args(call_bif, [{b,{erlang,'++',2}}]) -> 8;
fit_args(call_bif, [{b,{erlang,throw,1}}]) -> 6;
fit_args(call_bif, [{b,{erlang,exit,1}}]) -> 5;
fit_args(call_bif, [{b,{erlang,error,2}}]) -> 2;
fit_args(call_bif, [{b,{lists,member,2}}]) -> 9;
fit_args(call_bif, [{b,{ets,insert,2}}]) -> 10;
fit_args(call_bif, [{b,{erlang,get_module_info,2}}]) -> 11;
fit_args(call_bif, [{b,{erlang,list_to_binary,1}}]) -> 12;
fit_args(call_bif, [{b,{ets,delete,1}}]) -> 13;
fit_args(call_bif, [{b,{lists,keysearch,3}}]) -> 14;
fit_args(call_bif, [{b,{ets,info,2}}]) -> 15;
fit_args(call_bif, [{b,{erlang,list_to_atom,1}}]) -> 16;
fit_args(call_bif, [{b,{erlang,integer_to_list,1}}]) -> 17;
fit_args(call_bif, [{b,{ets,lookup,2}}]) -> 18;
fit_args(call_bif, [{b,{erlang,atom_to_list,1}}]) -> 19;
fit_args(call_bif, [{b,{ets,lookup_element,3}}]) -> 20;
fit_args(call_bif, [{b,{erlang,binary_to_list,1}}]) -> 21;
fit_args(call_bif, [{b,{erlang,'--',2}}]) -> 22;
fit_args(call_bif, [{b,{re,run,3}}]) -> 23;
fit_args(call_bif, [{b,{erlang,process_flag,2}}]) -> 24;
fit_args(call_bif, [{b,{erlang,process_info,2}}]) -> 25;
fit_args(call_bif, [{b,{erlang,unlink,1}}]) -> 26;
fit_args(call_bif, [{b,{erlang,tuple_to_list,1}}]) -> 28;
fit_args(call_bif, [{b,{erlang,whereis,1}}]) -> 27;
fit_args(call_bif, [{b,{erlang,iolist_to_binary,1}}]) -> 29;
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
fit_args(call_bif, [{b,{erlang,garbage_collect,0}}]) -> 46;
fit_args(call_bif, [{b,{erlang,fun_info,2}}]) -> 45;
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
fit_args(case_end, [{y,4}]) -> 8;
fit_args(case_end, [{y,0}]) -> 9;
fit_args(case_end, [{y,7}]) -> 10;
fit_args(case_end, [_]) -> 11;
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
fit_args(extract_next_element, [{x,17}]) -> 25;
fit_args(extract_next_element, [{y,7}]) -> 24;
fit_args(extract_next_element, [_]) -> 26;
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
fit_args(init, [{y,15}]) -> 16;
fit_args(init, [{y,16}]) -> 15;
fit_args(init, [_]) -> 17;
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
fit_args(is_nonempty_list, [{f,_},{x,15}]) -> 18;
fit_args(is_nonempty_list, [{f,_},{x,16}]) -> 19;
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
fit_args(is_nonempty_list, [{f,_},{x,29}]) -> 38;
fit_args(is_nonempty_list, [{f,_},_]) -> 39;
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
fit_args(is_tuple, [{f,_},{y,1}]) -> 9;
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
fit_args(l_bs_get_integer_32, [{x,_},{f,_},Arg2,{x,0}]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_bs_get_integer_32, [{x,_},{f,_},Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_bs_get_integer_32, [_,{f,_},Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 3;
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
fit_args(l_bs_skip_bits2, [{f,_},{x,_},{x,_},Arg3]) when Arg3 >= 0, Arg3 =< 255 -> 0;
fit_args(l_bs_skip_bits2, [{f,_},_,_,Arg3]) when Arg3 >= 0, Arg3 =< 255 -> 1;
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
fit_args(l_bs_test_unit_8, [{f,_},{x,2}]) -> 2;
fit_args(l_bs_test_unit_8, [{f,_},{x,1}]) -> 3;
fit_args(l_bs_test_unit_8, [{f,_},_]) -> 4;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,2}]) -> 0;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,3}]) -> 1;
fit_args(l_bs_test_zero_tail2, [{f,_},{x,0}]) -> 2;
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
fit_args(l_call_ext, [{e,{file,get_cwd,0}}]) -> 79;
fit_args(l_call_ext, [{e,{file,write_file,2}}]) -> 78;
fit_args(l_call_ext, [{e,{ordsets,intersection,2}}]) -> 88;
fit_args(l_call_ext, [{e,{lists,sublist,3}}]) -> 87;
fit_args(l_call_ext, [{e,{asn1ct_name,new,1}}]) -> 86;
fit_args(l_call_ext, [{e,{beam_utils,code_at,2}}]) -> 85;
fit_args(l_call_ext, [{e,{cerl,var_name,1}}]) -> 84;
fit_args(l_call_ext, [{e,{gb_sets,empty,0}}]) -> 83;
fit_args(l_call_ext, [{e,{xref_utils,xset,2}}]) -> 82;
fit_args(l_call_ext, [{e,{cerl,c_tuple,1}}]) -> 81;
fit_args(l_call_ext, [{e,{mnesia_lib,intersect,2}}]) -> 80;
fit_args(l_call_ext, [{e,{ordsets,is_element,2}}]) -> 91;
fit_args(l_call_ext, [{e,{lists,seq,2}}]) -> 90;
fit_args(l_call_ext, [{e,{dict,fetch,2}}]) -> 89;
fit_args(l_call_ext, [{e,{ets,tab2list,1}}]) -> 98;
fit_args(l_call_ext, [{e,{orddict,find,2}}]) -> 97;
fit_args(l_call_ext, [{e,{unicode,characters_to_binary,1}}]) -> 96;
fit_args(l_call_ext, [{e,{proplists,get_bool,2}}]) -> 95;
fit_args(l_call_ext, [{e,{mnesia,abort,1}}]) -> 94;
fit_args(l_call_ext, [{e,{rpc,call,4}}]) -> 93;
fit_args(l_call_ext, [{e,{sofs,difference,2}}]) -> 92;
fit_args(l_call_ext, [{e,{sets,add_element,2}}]) -> 104;
fit_args(l_call_ext, [{e,{sofs,relation_to_family,1}}]) -> 103;
fit_args(l_call_ext, [{e,{mnesia_lib,val,1}}]) -> 102;
fit_args(l_call_ext, [{e,{erl_scan,token_info,2}}]) -> 101;
fit_args(l_call_ext, [{e,{asn1ct_name,clear,0}}]) -> 100;
fit_args(l_call_ext, [{e,{asn1ct_gen_ber_bin_v2,decode_class,1}}]) -> 99;
fit_args(l_call_ext, [{e,_}]) -> 105;
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
fit_args(l_call_ext_only, [{e,{lists,reverse,1}}]) -> 3;
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
fit_args(l_fetch, [{x,2},_]) -> 15;
fit_args(l_fetch, [{x,5},_]) -> 16;
fit_args(l_fetch, [{y,0},_]) -> 19;
fit_args(l_fetch, [{x,_},{i,Arg1}]) when Arg1 >= -128, Arg1 =< 127 -> 3;
fit_args(l_fetch, [{y,Arg0},{i,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= -128, Arg1 =< 127 -> 9;
fit_args(l_fetch, [{x,_},{x,_}]) -> 2;
fit_args(l_fetch, [{x,_},{y,Arg1}]) when Arg1 >= 0, Arg1 =< 255 -> 4;
fit_args(l_fetch, [{y,Arg0},{y,Arg1}]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255 -> 6;
fit_args(l_fetch, [_,{x,0}]) -> 1;
fit_args(l_fetch, [_,{x,1}]) -> 8;
fit_args(l_fetch, [_,{x,4}]) -> 14;
fit_args(l_fetch, [_,{x,3}]) -> 17;
fit_args(l_fetch, [_,{x,2}]) -> 18;
fit_args(l_fetch, [_,{x,5}]) -> 21;
fit_args(l_fetch, [_,{y,5}]) -> 20;
fit_args(l_fetch, [_,{y,0}]) -> 22;
fit_args(l_fetch, [_,_]) -> 23;
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
fit_args(l_increment, [{x,0},_,Arg2,{y,Arg3}]) when Arg2 >= 0, Arg2 =< 255, Arg3 >= 0, Arg3 =< 255 -> 8;
fit_args(l_increment, [{y,Arg0},4294967295,Arg2,{x,0}]) when Arg0 >= 0, Arg0 =< 255, Arg2 >= 0, Arg2 =< 255 -> 4;
fit_args(l_increment, [{x,_},4294967295,Arg2,{x,0}]) when Arg2 >= 0, Arg2 =< 255 -> 6;
fit_args(l_increment, [{x,_},Arg1,Arg2,_]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_increment, [{y,Arg0},Arg1,Arg2,_]) when Arg0 >= 0, Arg0 =< 255, Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 1;
fit_args(l_increment, [{x,_},_,Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 7;
fit_args(l_increment, [_,4294967295,Arg2,{x,_}]) when Arg2 >= 0, Arg2 =< 255 -> 2;
fit_args(l_increment, [_,_,Arg2,_]) when Arg2 >= 0, Arg2 =< 255 -> 9;
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
fit_args(l_is_eq_exact_immed, [{f,_},{x,12},_]) -> 15;
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
fit_args(l_is_eq_exact_immed, [{f,_},{y,Arg1},{a,asn1_NOVALUE}]) when Arg1 >= 0, Arg1 =< 255 -> 14;
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
fit_args(l_move_call, [{a,true},{f,_}]) -> 34;
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
fit_args(l_move_call, [_,{f,_}]) -> 35;
fit_args(l_move_call_ext, [{a,funky},{e,{estone_SUITE,req,2}}]) -> 25;
fit_args(l_move_call_ext, [{a,auto_repair},{e,{mnesia_monitor,get_env,1}}]) -> 32;
fit_args(l_move_call_ext, [{a,available_internal_state},{e,{erts_debug,set_internal_state,2}}]) -> 45;
fit_args(l_move_call_ext, [{y,0},{e,_}]) -> 1;
fit_args(l_move_call_ext, [{y,1},{e,_}]) -> 2;
fit_args(l_move_call_ext, [{y,2},{e,_}]) -> 3;
fit_args(l_move_call_ext, [{y,3},{e,_}]) -> 5;
fit_args(l_move_call_ext, [{y,4},{e,_}]) -> 6;
fit_args(l_move_call_ext, [{x,2},{e,_}]) -> 8;
fit_args(l_move_call_ext, [{x,1},{e,_}]) -> 9;
fit_args(l_move_call_ext, [{y,6},{e,_}]) -> 11;
fit_args(l_move_call_ext, [{y,5},{e,_}]) -> 14;
fit_args(l_move_call_ext, [nil,{e,_}]) -> 18;
fit_args(l_move_call_ext, [{x,3},{e,_}]) -> 20;
fit_args(l_move_call_ext, [{y,7},{e,_}]) -> 22;
fit_args(l_move_call_ext, [{x,4},{e,_}]) -> 36;
fit_args(l_move_call_ext, [{a,schema},{e,_}]) -> 37;
fit_args(l_move_call_ext, [{a,func},{e,_}]) -> 38;
fit_args(l_move_call_ext, [{y,8},{e,_}]) -> 47;
fit_args(l_move_call_ext, [{a,id},{e,_}]) -> 48;
fit_args(l_move_call_ext, [{smallint,0},{e,{lists,seq,2}}]) -> 13;
fit_args(l_move_call_ext, [{smallint,1000},{e,{test_server,sleep,1}}]) -> 43;
fit_args(l_move_call_ext, [{smallint,1},{e,_}]) -> 7;
fit_args(l_move_call_ext, [{smallint,2},{e,_}]) -> 39;
fit_args(l_move_call_ext, [{smallint,3},{e,_}]) -> 44;
fit_args(l_move_call_ext, [_,{e,{io,format,2}}]) -> 0;
fit_args(l_move_call_ext, [_,{e,{asn1ct_gen,emit,1}}]) -> 4;
fit_args(l_move_call_ext, [_,{e,{io_lib,format,2}}]) -> 10;
fit_args(l_move_call_ext, [_,{e,{erlang,put,2}}]) -> 12;
fit_args(l_move_call_ext, [_,{e,{prettypr,text,1}}]) -> 15;
fit_args(l_move_call_ext, [_,{e,{asn1ct_name,new,1}}]) -> 16;
fit_args(l_move_call_ext, [_,{e,{proplists,get_value,2}}]) -> 17;
fit_args(l_move_call_ext, [_,{e,{proplists,get_value,3}}]) -> 19;
fit_args(l_move_call_ext, [_,{e,{test_server,seconds,1}}]) -> 21;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,verbose,2}}]) -> 23;
fit_args(l_move_call_ext, [_,{e,{erlang,system_info,1}}]) -> 24;
fit_args(l_move_call_ext, [_,{e,{lists,duplicate,2}}]) -> 26;
fit_args(l_move_call_ext, [_,{e,{lists,seq,2}}]) -> 27;
fit_args(l_move_call_ext, [_,{e,{test_server,lookup_config,2}}]) -> 28;
fit_args(l_move_call_ext, [_,{e,{erlang,binary_to_term,1}}]) -> 30;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,dbg_out,2}}]) -> 29;
fit_args(l_move_call_ext, [_,{e,{erlang,erase,1}}]) -> 31;
fit_args(l_move_call_ext, [_,{e,{lists,sublist,3}}]) -> 33;
fit_args(l_move_call_ext, [_,{e,{mnesia_lib,set,2}}]) -> 34;
fit_args(l_move_call_ext, [_,{e,{re,replace,4}}]) -> 35;
fit_args(l_move_call_ext, [_,{e,{re,split,3}}]) -> 40;
fit_args(l_move_call_ext, [_,{e,{io,format,1}}]) -> 42;
fit_args(l_move_call_ext, [_,{e,{io,fwrite,2}}]) -> 41;
fit_args(l_move_call_ext, [_,{e,{asn1ct_name,curr,1}}]) -> 46;
fit_args(l_move_call_ext, [_,{e,_}]) -> 49;
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
fit_args(l_move_call_ext_only, [{e,_},_]) -> 6;
fit_args(l_move_call_last, [{f,_},1,_]) -> 1;
fit_args(l_move_call_last, [{f,_},2,_]) -> 2;
fit_args(l_move_call_last, [{f,_},0,_]) -> 3;
fit_args(l_move_call_last, [{f,_},3,_]) -> 4;
fit_args(l_move_call_last, [{f,_},4,_]) -> 6;
fit_args(l_move_call_last, [{f,_},Arg1,{y,Arg2}]) when Arg1 >= 0, Arg1 =< 255, Arg2 >= 0, Arg2 =< 255 -> 0;
fit_args(l_move_call_last, [{f,_},Arg1,{x,_}]) when Arg1 >= 0, Arg1 =< 255 -> 5;
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
fit_args(l_move_call_only, [{f,_},{smallint,1}]) -> 9;
fit_args(l_move_call_only, [{f,_},_]) -> 10;
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
fit_args(l_select_val2, [{y,3},{f,_},_,{f,_},_,{f,_}]) -> 14;
fit_args(l_select_val2, [{x,_},{f,_},{i,Arg2},{f,_},{i,Arg4},{f,_}]) when Arg2 >= -128, Arg2 =< 127, Arg4 >= -128, Arg4 =< 127 -> 1;
fit_args(l_select_val2, [_,{f,_},{a,true},{f,_},{a,false},{f,_}]) -> 5;
fit_args(l_select_val2, [_,{f,_},{a,false},{f,_},{a,true},{f,_}]) -> 6;
fit_args(l_select_val2, [_,{f,_},_,{f,_},_,{f,_}]) -> 15;
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
fit_args(move_deallocate_return, [_,7]) -> 9;
fit_args(move_deallocate_return, [_,_]) -> 10;
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
fit_args(move_return, [{a,undefined}]) -> 14;
fit_args(move_return, [{a,error}]) -> 17;
fit_args(move_return, [{x,5}]) -> 16;
fit_args(move_return, [{a,ignore}]) -> 18;
fit_args(move_return, [{a,none}]) -> 19;
fit_args(move_return, [{a,nomatch}]) -> 25;
fit_args(move_return, [{x,6}]) -> 26;
fit_args(move_return, [{a,'$end_of_table'}]) -> 35;
fit_args(move_return, [{a,yes}]) -> 30;
fit_args(move_return, [{smallint,1}]) -> 8;
fit_args(move_return, [{smallint,2}]) -> 9;
fit_args(move_return, [{smallint,0}]) -> 10;
fit_args(move_return, [{smallint,3}]) -> 11;
fit_args(move_return, [{smallint,4}]) -> 12;
fit_args(move_return, [{smallint,8}]) -> 15;
fit_args(move_return, [{smallint,16}]) -> 20;
fit_args(move_return, [{smallint,5}]) -> 21;
fit_args(move_return, [{smallint,6}]) -> 22;
fit_args(move_return, [{smallint,64}]) -> 24;
fit_args(move_return, [{smallint,7}]) -> 23;
fit_args(move_return, [{smallint,128}]) -> 27;
fit_args(move_return, [{smallint,12}]) -> 29;
fit_args(move_return, [{smallint,10}]) -> 28;
fit_args(move_return, [{smallint,13}]) -> 34;
fit_args(move_return, [{smallint,11}]) -> 33;
fit_args(move_return, [{smallint,32}]) -> 32;
fit_args(move_return, [{smallint,9}]) -> 31;
fit_args(move_return, [_]) -> 36;
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
var_args(test_heap, 0) -> [u8,u8];
var_args(l_call, 0) -> [f];
var_args(move, 2) -> [{x,0},t];
var_args(move, 3) -> [x8,y8];
var_args(l_put_tuple, 0) -> [x8,u8];
var_args(move2, 1) -> [y8,x8,y8,t];
var_args(get_tuple_element, 0) -> [{x,0},u8,x8];
var_args(badmatch, 0) -> [{x,0}];
var_args(put_list, 0) -> [t,t,{x,0}];
var_args(is_tuple_of_arity, 1) -> [f,x8,u8];
var_args(get_tuple_element, 1) -> [x8,u8,x8];
var_args(move2, 0) -> [x8,y8,x8,y8];
var_args(move, 4) -> [t,{x,2}];
var_args(l_call_only, 0) -> [f];
var_args(l_is_eq_exact_immed, 0) -> [f,{x,0},t];
var_args(l_is_eq_exact_immed, 1) -> [f,{x,1},t];
var_args(is_tuple_of_arity, 0) -> [f,{x,0},{u,2}];
var_args(put_list, 1) -> [t,t,{x,1}];
var_args(get_list, 0) -> [x8,x8,x8];
var_args(l_put_tuple, 1) -> [{x,0},{u,2}];
var_args(l_is_ge, 0) -> [f];
var_args(l_call_ext, 105) -> [e];
var_args(l_make_fun, 0) -> [fu];
var_args(l_move_call_last, 0) -> [f,u8,y8];
var_args(extract_next_element2, 0) -> [{x,1}];
var_args(is_tuple_of_arity, 2) -> [f,{x,0},u32];
var_args(return, 0) -> [];
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
var_args(l_is_eq_exact_immed, 2) -> [f,{x,3},t];
var_args(l_allocate, 1) -> [{u,0}];
var_args(jump, 0) -> [f];
var_args(move_return, 36) -> [t];
var_args(deallocate_return, 0) -> [{u,1}];
var_args(case_end, 0) -> [{x,0}];
var_args(get_list, 1) -> [{x,0},x8,x8];
var_args(call_bif, 7) -> [{b,{erlang,setelement,3}}];
var_args(move2, 2) -> [x8,y8,{x,0},y8];
var_args(l_is_eq_exact, 0) -> [f];
var_args(l_move_call, 35) -> [t,f];
var_args(get_tuple_element, 2) -> [t,{u,0},{x,0}];
var_args(l_allocate, 2) -> [{u,2}];
var_args(move_return, 0) -> [{a,true}];
var_args(l_is_eq_exact_immed, 3) -> [f,x8,i8];
var_args(l_select_val2, 0) -> [{x,0},f,t,f,t,f];
var_args(get_tuple_element, 3) -> [{x,0},u8,y8];
var_args(move, 5) -> [t,{x,3}];
var_args(l_fetch, 2) -> [x8,x8];
var_args(init2, 0) -> [y8,y8];
var_args(get_list, 2) -> [t,{x,0},t];
var_args(l_is_eq_exact_immed, 4) -> [f,{x,2},t];
var_args(init, 0) -> [{y,1}];
var_args(deallocate_return, 1) -> [{u,0}];
var_args(put_list, 3) -> [t,t,{x,3}];
var_args(call_bif, 8) -> [{b,{erlang,'++',2}}];
var_args(extract_next_element, 1) -> [{x,3}];
var_args(is_tuple_of_arity, 3) -> [f,y8,u8];
var_args(get_tuple_element, 4) -> [y8,u8,x8];
var_args(move2, 4) -> [{x,0},y8,x8,y8];
var_args(l_fetch, 3) -> [x8,i8];
var_args(init, 1) -> [{y,0}];
var_args(l_put_tuple, 2) -> [{x,0},{u,3}];
var_args(move2, 5) -> [x8,x8,x8,{x,0}];
var_args(l_allocate, 3) -> [{u,3}];
var_args(deallocate_return, 2) -> [{u,2}];
var_args(l_fetch, 4) -> [x8,y8];
var_args(init3, 0) -> [y8,y8,y8];
var_args(is_nonempty_list, 1) -> [f,{x,2}];
var_args(l_move_call_ext, 49) -> [t,e];
var_args(l_is_eq_exact_immed, 5) -> [f,{x,4},t];
var_args(l_allocate_zero, 0) -> [{u,2}];
var_args(call_bif, 6) -> [{b,{erlang,throw,1}}];
var_args(l_trim, 1) -> [{u,2}];
var_args(allocate_init, 0) -> [u32,{y,0}];
var_args(allocate_heap, 0) -> [u8,u8,u8];
var_args(test_heap_1_put_list, 0) -> [{u,2},{y,0}];
var_args(l_allocate_zero, 1) -> [{u,1}];
var_args(init, 2) -> [{y,2}];
var_args(move_deallocate_return, 1) -> [{a,ok},u8];
var_args(l_call_last, 0) -> [f,{u,2}];
var_args(call_bif, 48) -> [b];
var_args(l_is_eq_exact_literal, 0) -> [f,{x,0},t];
var_args(is_nonempty_list, 2) -> [f,{x,1}];
var_args(l_move_call, 0) -> [{x,2},f];
var_args(move_return, 1) -> [{x,1}];
var_args(is_tuple, 0) -> [f,{x,0}];
var_args(l_is_eq_exact_immed, 6) -> [f,{x,5},t];
var_args(is_list, 0) -> [f,{x,0}];
var_args(l_call_last, 1) -> [f,{u,0}];
var_args(deallocate_return, 3) -> [{u,3}];
var_args(is_nonempty_list_allocate, 0) -> [f,{x,0},u32];
var_args(l_move_call_only, 0) -> [f,{x,1}];
var_args(call_bif, 5) -> [{b,{erlang,exit,1}}];
var_args(extract_next_element, 2) -> [{x,2}];
var_args(l_increment, 0) -> [x8,u8,u8,t];
var_args(move, 6) -> [t,{x,4}];
var_args(l_is_lt, 0) -> [f];
var_args(l_trim, 2) -> [{u,3}];
var_args(l_gc_bif1, 0) -> [f,b,t,u8,x8];
var_args(l_select_val_atoms, 0) -> [x8,f,u8];
var_args(l_call_last, 2) -> [f,{u,3}];
var_args(move_deallocate_return, 2) -> [y8,u8];
var_args(is_nonempty_list, 3) -> [f,{x,3}];
var_args(l_bs_start_match2, 0) -> [t,f,u8,u8,x8];
var_args(l_new_bs_put_integer_imm, 0) -> [f,u32,{u8,0},t];
var_args(is_nil, 1) -> [f,{x,2}];
var_args(extract_next_element2, 1) -> [{x,3}];
var_args(l_select_val2, 2) -> [{x,1},f,t,f,t,f];
var_args(remove_message, 0) -> [];
var_args(l_move_call_only, 10) -> [f,t];
var_args(l_move_call_only, 1) -> [f,{x,2}];
var_args(init, 3) -> [{y,3}];
var_args(l_catch, 0) -> [{y,0},t];
var_args(l_allocate_zero, 2) -> [{u,3}];
var_args(extract_next_element3, 0) -> [{x,1}];
var_args(get_tuple_element, 5) -> [x8,u8,y8];
var_args(l_call_ext, 0) -> [{e,{lists,reverse,1}}];
var_args(move_jump, 12) -> [f,t];
var_args(l_fetch, 5) -> [i8,x8];
var_args(extract_next_element, 3) -> [{x,4}];
var_args(is_nil, 2) -> [f,{x,1}];
var_args(move2, 6) -> [x8,x8,{x,0},x8];
var_args(l_allocate, 4) -> [{u,4}];
var_args(catch_end, 0) -> [{y,0}];
var_args(test_arity, 0) -> [f,{x,0},{u,2}];
var_args(l_allocate_zero, 3) -> [{u,4}];
var_args(l_is_eq_exact_immed, 7) -> [f,{x,6},t];
var_args(move_return, 2) -> [{a,ok}];
var_args(move_return, 3) -> [{x,2}];
var_args(l_move_call_ext, 0) -> [t,{e,{io,format,2}}];
var_args(put_list, 4) -> [x8,t,x8];
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
var_args(l_bs_match_string, 0) -> [x8,f,u8,str];
var_args(bif1_body, 0) -> [{b,{erlang,hd,1}},t,{x,0}];
var_args(l_move_call_only, 2) -> [f,{x,4}];
var_args(l_move_call_last, 1) -> [f,{u,1},t];
var_args(is_nonempty_list, 6) -> [f,{x,5}];
var_args(l_bs_test_zero_tail2, 0) -> [f,{x,2}];
var_args(l_is_eq, 0) -> [f];
var_args(send, 0) -> [];
var_args(set_tuple_element, 0) -> [y8,{x,0},u8];
var_args(l_catch, 1) -> [{y,1},t];
var_args(call_bif, 9) -> [{b,{lists,member,2}}];
var_args(l_is_ne, 0) -> [f];
var_args(l_move_call_only, 3) -> [f,{x,3}];
var_args(l_move_call_ext_last, 0) -> [e,u8,y8];
var_args(l_select_val2, 1) -> [x8,f,i8,f,i8,f];
var_args(l_select_val_smallints, 0) -> [x8,f,u8];
var_args(l_move_call_ext, 1) -> [{y,0},e];
var_args(l_call_ext_only, 4) -> [e];
var_args(l_is_ne_exact_immed, 0) -> [f,{x,0},t];
var_args(l_increment, 1) -> [y8,u8,u8,t];
var_args(l_bs_add, 0) -> [f,{u8,1},t];
var_args(bif2_body, 0) -> [b,{x,0}];
var_args(is_nonempty_list, 7) -> [f,{x,6}];
var_args(extract_next_element, 4) -> [{x,5}];
var_args(case_end, 1) -> [{x,1}];
var_args(l_is_eq_exact_immed, 9) -> [f,{x,8},t];
var_args(extract_next_element2, 2) -> [{x,4}];
var_args(move_deallocate_return, 3) -> [t,{u,0}];
var_args(extract_next_element2, 3) -> [{x,2}];
var_args(l_loop_rec, 0) -> [f];
var_args(l_call_ext, 1) -> [{e,{asn1ct_gen,emit,1}}];
var_args(l_select_tuple_arity2, 1) -> [x8,f,u8,f,u8,f];
var_args(l_move_call, 2) -> [{y,0},f];
var_args(l_move_call, 3) -> [{y,2},f];
var_args(l_move_call_ext, 2) -> [{y,1},e];
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
var_args(call_bif, 10) -> [{b,{ets,insert,2}}];
var_args(deallocate_return, 5) -> [{u,5}];
var_args(move, 8) -> [t,{x,5}];
var_args(l_fetch, 6) -> [y8,y8];
var_args(is_nonempty_list, 9) -> [f,{x,8}];
var_args(extract_next_element, 5) -> [{x,6}];
var_args(l_select_val2, 3) -> [{x,2},f,t,f,t,f];
var_args(l_allocate_zero, 5) -> [{u,5}];
var_args(l_allocate, 5) -> [{u,5}];
var_args(init, 6) -> [{y,6}];
var_args(l_move_call_ext, 3) -> [{y,2},e];
var_args(badmatch, 1) -> [{x,3}];
var_args(l_call_last, 5) -> [f,{u,6}];
var_args(l_move_call_ext, 4) -> [t,{e,{asn1ct_gen,emit,1}}];
var_args(l_plus, 1) -> [f,u8,{x,0}];
var_args(l_bs_start_match2, 1) -> [{x,0},f,u8,u8,{x,0}];
var_args(l_move_call_ext, 5) -> [{y,3},e];
var_args(l_call_last, 6) -> [f,{u,5}];
var_args(badmatch, 2) -> [{x,2}];
var_args(l_move_call_ext, 6) -> [{y,4},e];
var_args(l_call_fun_last, 0) -> [u8,u8];
var_args(l_move_call_ext, 7) -> [{smallint,1},e];
var_args(l_trim, 4) -> [{u,5}];
var_args(l_increment, 2) -> [t,{u,4294967295},u8,x8];
var_args(put_list, 5) -> [t,t,{x,4}];
var_args(call_bif, 11) -> [{b,{erlang,get_module_info,2}}];
var_args(int_code_end, 0) -> [];
var_args(l_move_call_ext_only, 0) -> [{e,{erlang,get_module_info,1}},t];
var_args(l_bs_save2, 0) -> [x8,u8];
var_args(call_bif, 12) -> [{b,{erlang,list_to_binary,1}}];
var_args(extract_next_element, 6) -> [{x,255}];
var_args(bs_context_to_binary, 0) -> [{x,0}];
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
var_args(call_bif, 13) -> [{b,{ets,delete,1}}];
var_args(is_tuple, 1) -> [f,{x,1}];
var_args(l_move_call_last, 2) -> [f,{u,2},t];
var_args(l_move_call, 5) -> [{y,3},f];
var_args(l_is_eq_exact_immed, 11) -> [f,{x,9},t];
var_args(badmatch, 3) -> [{x,1}];
var_args(l_move_call, 6) -> [{x,3},f];
var_args(is_nil, 5) -> [f,{x,5}];
var_args(is_nonempty_list_allocate, 1) -> [f,x8,u8];
var_args(l_call_ext, 2) -> [{e,{lists,foreach,2}}];
var_args(l_bs_test_zero_tail2, 1) -> [f,{x,3}];
var_args(l_catch, 2) -> [{y,2},t];
var_args(l_move_call_ext, 8) -> [{x,2},e];
var_args(call_bif, 14) -> [{b,{lists,keysearch,3}}];
var_args(is_atom, 0) -> [f,{x,0}];
var_args(l_bs_test_zero_tail2, 2) -> [f,{x,0}];
var_args(l_move_call_ext_only, 6) -> [e,t];
var_args(l_select_val_smallints, 1) -> [{x,0},f,u32];
var_args(l_fast_element, 0) -> [{x,0},{u,2},{x,0}];
var_args(is_nonempty_list, 10) -> [f,{x,10}];
var_args(l_bs_match_string, 1) -> [{x,0},f,u32,str];
var_args(call_bif, 15) -> [{b,{ets,info,2}}];
var_args(l_call_ext, 3) -> [{e,{file,close,1}}];
var_args(move_return, 6) -> [{x,3}];
var_args(l_bs_test_unit_8, 0) -> [f,{x,0}];
var_args(l_call_ext_last, 1) -> [e,{u,0}];
var_args(is_nil, 6) -> [f,{x,6}];
var_args(allocate_heap_zero, 0) -> [u8,u8,u8];
var_args(is_nonempty_list, 11) -> [f,{x,11}];
var_args(l_call_ext_last, 2) -> [e,{u,2}];
var_args(call_bif, 16) -> [{b,{erlang,list_to_atom,1}}];
var_args(self, 1) -> [{x,1}];
var_args(extract_next_element, 8) -> [{y,1}];
var_args(init, 7) -> [{y,7}];
var_args(case_end, 2) -> [{x,2}];
var_args(extract_next_element3, 1) -> [{x,3}];
var_args(is_nil, 7) -> [f,{x,7}];
var_args(l_move_call, 7) -> [{y,5},f];
var_args(call_bif, 17) -> [{b,{erlang,integer_to_list,1}}];
var_args(raise, 0) -> [{x,2},{x,1}];
var_args(catch_end, 2) -> [{y,2}];
var_args(l_is_eq_exact_literal, 1) -> [f,{x,1},t];
var_args(l_is_eq_exact_literal, 7) -> [f,t,t];
var_args(l_move_call_ext, 9) -> [{x,1},e];
var_args(l_minus, 0) -> [f,u8,x8];
var_args(extract_next_element2, 5) -> [{x,6}];
var_args(l_allocate, 6) -> [{u,6}];
var_args(l_bif2, 0) -> [f,{b,{erlang,element,2}},t];
var_args(try_end, 0) -> [{y,1}];
var_args(l_call_fun, 1) -> [{u8,3}];
var_args(call_bif, 18) -> [{b,{ets,lookup,2}}];
var_args(deallocate_return, 6) -> [{u,6}];
var_args(l_call_ext, 4) -> [{e,{file,open,2}}];
var_args(l_move_call_last, 3) -> [f,{u,0},t];
var_args(l_put_tuple, 5) -> [{x,0},u32];
var_args(l_move_call_ext_only, 1) -> [{e,{io_lib,format,2}},t];
var_args(l_band, 0) -> [f,u8,x8];
var_args(l_move_call, 8) -> [{y,4},f];
var_args(l_trim, 5) -> [{u,6}];
var_args(extract_next_element, 9) -> [{y,0}];
var_args(call_bif, 19) -> [{b,{erlang,atom_to_list,1}}];
var_args(l_call_ext, 5) -> [{e,{lists,map,2}}];
var_args(l_call_ext, 6) -> [{e,{lists,foldl,3}}];
var_args(l_bs_get_binary_all2, 0) -> [f,x8,u8,u8,x8];
var_args(get_tuple_element, 7) -> [x8,u8,{x,0}];
var_args(l_minus, 1) -> [f,u8,{x,0}];
var_args(l_is_eq_exact_immed, 12) -> [f,{x,10},t];
var_args(extract_next_element3, 2) -> [{x,2}];
var_args(is_integer, 0) -> [f,{x,0}];
var_args(test_heap_1_put_list, 1) -> [{u,2},t];
var_args(extract_next_element2, 6) -> [{x,8}];
var_args(l_catch, 3) -> [{y,3},t];
var_args(extract_next_element, 26) -> [t];
var_args(l_bif2, 1) -> [f,{b,{erlang,'=:=',2}},t];
var_args(is_nil, 8) -> [f,{x,8}];
var_args(l_move_call_only, 4) -> [f,{x,5}];
var_args(move_deallocate_return, 5) -> [t,{u,2}];
var_args(move_jump, 0) -> [f,{x,1}];
var_args(is_list, 1) -> [f,{x,1}];
var_args(l_move_call, 9) -> [{smallint,1},f];
var_args(l_bsr, 0) -> [f,u8,x8];
var_args(l_call_ext, 7) -> [{e,{filename,join,2}}];
var_args(l_is_eq_exact_immed, 13) -> [f,{x,11},t];
var_args(apply, 0) -> [u8];
var_args(l_fast_element, 2) -> [x8,u8,t];
var_args(l_fetch, 7) -> [{y,0},{x,2}];
var_args(l_bs_get_integer_small_imm, 0) -> [t,u8,f,u8,x8];
var_args(l_bif2, 2) -> [f,{b,{erlang,'=<',2}},t];
var_args(l_bs_get_integer_8, 0) -> [x8,f,x8];
var_args(l_bs_test_zero_tail2, 3) -> [f,{x,1}];
var_args(set_tuple_element, 1) -> [t,{x,0},u32];
var_args(try_end, 1) -> [{y,0}];
var_args(l_is_eq_exact_literal, 2) -> [f,{x,4},t];
var_args(l_is_eq_exact_immed, 10) -> [f,{x,255},{a,xmerl_scanner}];
var_args(extract_next_element2, 7) -> [{x,7}];
var_args(l_is_eq_exact_literal, 3) -> [f,{x,2},t];
var_args(l_select_tuple_arity, 0) -> [t,f,{u,6}];
var_args(l_allocate_zero, 6) -> [{u,7}];
var_args(l_fmul, 0) -> [fr,fr,fr];
var_args(l_call_ext, 8) -> [{e,{lists,sort,1}}];
var_args(l_bsl, 0) -> [f,u8,{x,0}];
var_args(is_nonempty_list, 12) -> [f,{x,12}];
var_args(move, 9) -> [t,{x,6}];
var_args(call_bif, 20) -> [{b,{ets,lookup_element,3}}];
var_args(l_move_call_ext, 10) -> [t,{e,{io_lib,format,2}}];
var_args(l_times, 0) -> [f,u8,{x,0}];
var_args(l_move_call_ext_only, 2) -> [{e,{lists,reverse,1}},t];
var_args(l_put_tuple, 6) -> [y8,u8];
var_args(l_bs_init_fail, 0) -> [u8,f,u8,x8];
var_args(l_is_eq_exact_immed, 14) -> [f,y8,{a,asn1_NOVALUE}];
var_args(l_times, 1) -> [f,u8,x8];
var_args(l_is_eq_exact_immed, 15) -> [f,{x,12},t];
var_args(l_move_call_ext, 11) -> [{y,6},e];
var_args(l_call_ext, 9) -> [{e,{ordsets,union,2}}];
var_args(l_move_call_ext, 12) -> [t,{e,{erlang,put,2}}];
var_args(l_fetch, 8) -> [t,{x,1}];
var_args(extract_next_element, 10) -> [{x,8}];
var_args(is_nil, 9) -> [f,{x,9}];
var_args(call_bif, 21) -> [{b,{erlang,binary_to_list,1}}];
var_args(node, 0) -> [{x,0}];
var_args(l_call_last, 7) -> [f,{u,7}];
var_args(l_bs_get_binary2, 0) -> [f,x8,u8,t,u8,{u8,0},x8];
var_args(is_tuple, 2) -> [f,{x,2}];
var_args(l_call_fun, 2) -> [{u8,2}];
var_args(test_arity, 2) -> [f,{x,0},u32];
var_args(get_list, 4) -> [y8,x8,x8];
var_args(l_bs_get_integer_8, 1) -> [{x,0},f,x8];
var_args(l_bs_test_zero_tail2, 5) -> [f,t];
var_args(catch_end, 3) -> [{y,3}];
var_args(l_bif2, 3) -> [f,{b,{erlang,'or',2}},t];
var_args(call_bif, 22) -> [{b,{erlang,'--',2}}];
var_args(l_is_ne_exact_immed, 2) -> [f,{x,2},t];
var_args(l_is_ne_exact, 0) -> [f];
var_args(l_bif2, 4) -> [f,{b,{erlang,'and',2}},t];
var_args(is_binary, 0) -> [f,{x,1}];
var_args(l_allocate_zero, 9) -> [u32];
var_args(l_is_eq_exact_immed, 16) -> [f,{y,0},t];
var_args(extract_next_element, 11) -> [{y,3}];
var_args(l_bs_get_integer_32, 0) -> [x8,f,u8,x8];
var_args(l_call_ext, 10) -> [{e,{lists,flatten,1}}];
var_args(is_atom, 1) -> [f,{x,1}];
var_args(l_select_val2, 7) -> [{x,4},f,t,f,t,f];
var_args(l_fcheckerror, 0) -> [];
var_args(fclearerror, 0) -> [];
var_args(l_fetch, 9) -> [y8,i8];
var_args(l_new_bs_put_binary_all, 0) -> [f,{x,0},{u8,8}];
var_args(extract_next_element3, 3) -> [{x,5}];
var_args(node, 1) -> [{x,1}];
var_args(extract_next_element, 12) -> [{y,2}];
var_args(l_move_call, 10) -> [{y,6},f];
var_args(get_list, 5) -> [{x,0},t,{x,0}];
var_args(move_deallocate_return, 6) -> [t,{u,3}];
var_args(l_move_call_last, 4) -> [f,{u,3},t];
var_args(l_new_bs_put_binary_all, 1) -> [f,t,{u8,8}];
var_args(l_is_eq_exact_immed, 17) -> [f,{y,1},t];
var_args(timeout, 0) -> [];
var_args(deallocate_return, 7) -> [{u,7}];
var_args(l_get, 0) -> [{a,asn1_module},{y,0}];
var_args(l_move_call, 11) -> [{x,4},f];
var_args(l_fetch, 10) -> [i8,y8];
var_args(l_move_call_ext_last, 1) -> [e,{u,0},t];
var_args(is_nil, 10) -> [f,{y,1}];
var_args(l_fetch, 11) -> [{x,1},t];
var_args(call_bif, 23) -> [{b,{re,run,3}}];
var_args(l_call_ext, 11) -> [{e,{test_server,timetrap,1}}];
var_args(is_float, 1) -> [f,t];
var_args(l_select_val2, 5) -> [t,f,{a,true},f,{a,false},f];
var_args(l_select_val2, 8) -> [{x,5},f,t,f,t,f];
var_args(l_is_eq_exact_immed, 36) -> [f,t,t];
var_args(call_bif, 24) -> [{b,{erlang,process_flag,2}}];
var_args(l_band, 1) -> [f,u8,{x,0}];
var_args(l_get, 1) -> [t,{x,1}];
var_args(is_binary, 1) -> [f,{x,0}];
var_args(l_fast_element, 1) -> [{x,0},{u,1},t];
var_args(l_get, 2) -> [t,{x,0}];
var_args(is_nonempty_list, 39) -> [f,t];
var_args(l_move_call_ext, 14) -> [{y,5},e];
var_args(extract_next_element, 13) -> [{x,9}];
var_args(is_nonempty_list, 13) -> [f,{y,2}];
var_args(l_move_call, 12) -> [nil,f];
var_args(l_call_ext, 12) -> [{e,{lists,concat,1}}];
var_args(l_is_eq_exact_immed, 18) -> [f,{x,255},t];
var_args(l_move_call, 13) -> [{x,5},f];
var_args(l_move_call_ext, 15) -> [t,{e,{prettypr,text,1}}];
var_args(l_rem, 0) -> [f,u8,x8];
var_args(move2, 8) -> [{x,0},x8,x8,x8];
var_args(l_bsl, 1) -> [f,u8,x8];
var_args(l_call_ext, 13) -> [{e,{test_server,timetrap_cancel,1}}];
var_args(l_allocate_zero, 7) -> [{u,8}];
var_args(l_call_last, 8) -> [f,{u,8}];
var_args(is_nil, 11) -> [f,{x,10}];
var_args(l_gc_bif1, 2) -> [f,b,{x,0},{u8,1},{x,0}];
var_args(l_is_ne_exact_immed, 10) -> [f,t,t];
var_args(extract_next_element3, 4) -> [{x,4}];
var_args(l_increment, 4) -> [y8,{u,4294967295},u8,{x,0}];
var_args(is_nonempty_list, 14) -> [f,{x,13}];
var_args(is_nil, 30) -> [f,t];
var_args(extract_next_element3, 10) -> [t];
var_args(l_bs_append, 0) -> [f,u8,u8,u8,x8];
var_args(call_bif, 25) -> [{b,{erlang,process_info,2}}];
var_args(l_trim, 6) -> [{u,7}];
var_args(is_nil, 12) -> [f,{x,11}];
var_args(l_move_call_ext, 16) -> [t,{e,{asn1ct_name,new,1}}];
var_args(is_integer, 5) -> [f,t];
var_args(l_call_ext, 14) -> [{e,{file,delete,1}}];
var_args(l_bor, 0) -> [f,u8,{x,0}];
var_args(move_return, 7) -> [{x,4}];
var_args(is_list, 2) -> [f,{x,2}];
var_args(l_select_val2, 15) -> [t,f,t,f,t,f];
var_args(l_call_ext, 15) -> [{e,{mnesia_lib,set,2}}];
var_args(is_nil, 13) -> [f,{x,12}];
var_args(l_catch, 4) -> [{y,4},t];
var_args(l_element, 1) -> [t,t,t];
var_args(extract_next_element2, 17) -> [t];
var_args(l_gc_bif1, 5) -> [f,b,t,u8,{x,0}];
var_args(l_fadd, 0) -> [fr,fr,fr];
var_args(call_bif, 26) -> [{b,{erlang,unlink,1}}];
var_args(l_allocate, 7) -> [{u,7}];
var_args(l_move_call_only, 5) -> [f,{x,6}];
var_args(is_nonempty_list, 15) -> [f,{y,3}];
var_args(l_move_call_ext, 17) -> [t,{e,{proplists,get_value,2}}];
var_args(l_move_call_ext, 18) -> [nil,e];
var_args(call_bif, 28) -> [{b,{erlang,tuple_to_list,1}}];
var_args(call_bif, 27) -> [{b,{erlang,whereis,1}}];
var_args(is_integer, 1) -> [f,{x,1}];
var_args(bif1_body, 1) -> [{b,{erlang,hd,1}},{x,0},{x,1}];
var_args(l_is_ne_exact_immed, 3) -> [f,t,{a,true}];
var_args(l_call_ext, 16) -> [{e,{test_server,lookup_config,2}}];
var_args(l_call_ext_last, 3) -> [e,{u,3}];
var_args(l_fetch, 12) -> [{x,4},t];
var_args(l_move_call_ext, 19) -> [t,{e,{proplists,get_value,3}}];
var_args(l_is_eq_exact_immed, 19) -> [f,{x,13},t];
var_args(is_nonempty_list, 16) -> [f,{x,14}];
var_args(bif1_body, 2) -> [{b,{erlang,hd,1}},{y,1},{x,2}];
var_args(fmove_1, 0) -> [x8,fr];
var_args(l_move_call_ext, 20) -> [{x,3},e];
var_args(call_bif, 29) -> [{b,{erlang,iolist_to_binary,1}}];
var_args(move_jump, 1) -> [f,{x,2}];
var_args(l_bs_get_utf8, 0) -> [x8,f,x8];
var_args(l_move_call_ext, 21) -> [t,{e,{test_server,seconds,1}}];
var_args(l_bs_skip_bits_all2, 0) -> [f,{x,2},{u8,8}];
var_args(bif2_body, 1) -> [b,{x,1}];
var_args(l_call_ext, 17) -> [{e,{erl_syntax,type,1}}];
var_args(call_bif, 30) -> [{b,{lists,keyfind,3}}];
var_args(fconv, 0) -> [t,{fr,0}];
var_args(l_is_eq_exact_literal, 4) -> [f,{x,3},t];
var_args(l_is_eq_exact_immed, 20) -> [f,{y,3},t];
var_args(call_bif, 31) -> [{b,{erlang,list_to_tuple,1}}];
var_args(extract_next_element3, 5) -> [{x,7}];
var_args(extract_next_element2, 8) -> [{x,9}];
var_args(system_limit, 0) -> [f];
var_args(l_move_call_ext_last, 2) -> [e,{u,1},t];
var_args(l_fetch, 23) -> [t,t];
var_args(call_bif, 32) -> [{b,{ets,new,2}}];
var_args(call_bif, 4) -> [{b,{erlang,exit,2}}];
var_args(apply_last, 0) -> [u8,u32];
var_args(is_atom, 2) -> [f,{x,2}];
var_args(l_move_call_ext_only, 3) -> [e,{x,2}];
var_args(l_bor, 1) -> [f,u8,x8];
var_args(l_is_eq_exact_immed, 22) -> [f,{x,14},t];
var_args(put_list, 6) -> [{x,0},t,{y,0}];
var_args(call_bif, 33) -> [{b,{erlang,make_ref,0}}];
var_args(extract_next_element, 14) -> [{x,10}];
var_args(l_catch, 5) -> [{y,5},t];
var_args(init, 8) -> [{y,8}];
var_args(put_list, 8) -> [y8,t,x8];
var_args(l_make_export, 0) -> [e];
var_args(l_select_val2, 6) -> [t,f,{a,false},f,{a,true},f];
var_args(move_deallocate_return, 7) -> [t,{u,4}];
var_args(l_call_fun, 3) -> [{u8,0}];
var_args(call_bif, 34) -> [{b,{lists,reverse,2}}];
var_args(badmatch, 4) -> [{y,2}];
var_args(is_atom, 3) -> [f,{x,3}];
var_args(l_move_call, 14) -> [{y,7},f];
var_args(l_bs_test_zero_tail2, 4) -> [f,{x,4}];
var_args(fmove_1, 1) -> [t,{fr,1}];
var_args(fmove_2, 0) -> [fr,{x,0}];
var_args(l_new_bs_put_integer, 0) -> [f,t,{u8,1},{u8,0},t];
var_args(fmove_2, 1) -> [fr,x8];
var_args(call_bif, 35) -> [{b,{lists,keymember,3}}];
var_args(is_integer_allocate, 0) -> [f,x8,u8];
var_args(l_call_ext_last, 4) -> [e,{u,4}];
var_args(l_move_call_ext, 22) -> [{y,7},e];
var_args(l_call_ext, 18) -> [{e,{lists,mapfoldl,3}}];
var_args(l_fast_element, 3) -> [t,{u,2},t];
var_args(l_move_call, 15) -> [{smallint,0},f];
var_args(l_move_call_ext, 23) -> [t,{e,{mnesia_lib,verbose,2}}];
var_args(l_fetch, 13) -> [{x,3},t];
var_args(l_trim, 7) -> [{u,8}];
var_args(l_bs_restore2, 1) -> [{x,0},{u,0}];
var_args(deallocate_return, 8) -> [{u,8}];
var_args(is_list, 6) -> [f,t];
var_args(l_call_ext, 19) -> [{e,{prettypr,floating,1}}];
var_args(extract_next_element, 15) -> [{y,4}];
var_args(l_fetch, 14) -> [t,{x,4}];
var_args(l_is_eq_exact_immed, 23) -> [f,{y,2},t];
var_args(is_tuple, 3) -> [f,{x,3}];
var_args(is_nonempty_list, 17) -> [f,{y,1}];
var_args(l_move_call_ext, 13) -> [{smallint,0},{e,{lists,seq,2}}];
var_args(l_fetch, 15) -> [{x,2},t];
var_args(l_move_call_last, 5) -> [f,u8,x8];
var_args(extract_next_element, 16) -> [{y,5}];
var_args(extract_next_element2, 9) -> [{x,12}];
var_args(try_end, 2) -> [{y,2}];
var_args(if_end, 0) -> [];
var_args(l_get, 3) -> [t,{x,2}];
var_args(call_bif, 36) -> [{b,{erlang,now,0}}];
var_args(case_end, 11) -> [t];
var_args(l_move_call, 16) -> [{smallint,3},f];
var_args(call_bif, 38) -> [{b,{erlang,spawn_link,1}}];
var_args(call_bif, 37) -> [{b,{erlang,get_stacktrace,0}}];
var_args(move_jump, 2) -> [f,nil];
var_args(try_end, 4) -> [{y,4}];
var_args(try_end, 3) -> [{y,5}];
var_args(l_is_ne_exact_immed, 4) -> [f,{x,3},t];
var_args(call_bif, 39) -> [{b,{ets,delete,2}}];
var_args(self, 2) -> [{x,2}];
var_args(l_call_ext, 23) -> [{e,{dict,find,2}}];
var_args(l_call_ext, 22) -> [{e,{erlang,list_to_integer,1}}];
var_args(l_call_ext, 21) -> [{e,{string,tokens,2}}];
var_args(l_call_ext, 20) -> [{e,{proplists,get_value,3}}];
var_args(move_return, 8) -> [{smallint,1}];
var_args(is_list, 3) -> [f,{x,3}];
var_args(is_nonempty_list, 18) -> [f,{x,15}];
var_args(try_end, 5) -> [{y,3}];
var_args(catch_end, 4) -> [{y,4}];
var_args(l_move_call_ext_last, 3) -> [e,{u,2},t];
var_args(l_call_ext, 25) -> [{e,{filename,join,1}}];
var_args(l_call_ext, 24) -> [{e,{lists,filter,2}}];
var_args(extract_next_element2, 10) -> [{x,10}];
var_args(is_nonempty_list, 19) -> [f,{x,16}];
var_args(init, 9) -> [{y,9}];
var_args(l_call_ext, 26) -> [{e,{asn1_db,dbget,2}}];
var_args(case_end, 3) -> [{y,2}];
var_args(bif1_body, 3) -> [b,{x,0},{x,0}];
var_args(get_tuple_element, 8) -> [{x,0},u32,{x,0}];
var_args(test_arity, 3) -> [f,y8,u8];
var_args(l_bs_skip_bits_imm2, 0) -> [f,t,u32];
var_args(l_bif1, 0) -> [f,b,x8,x8];
var_args(l_bs_get_utf16, 1) -> [{x,0},f,u8,x8];
var_args(is_tuple, 4) -> [f,{x,4}];
var_args(l_call_ext, 28) -> [{e,{proplists,get_value,2}}];
var_args(l_call_ext, 27) -> [{e,{prettypr,beside,2}}];
var_args(extract_next_element, 17) -> [{x,11}];
var_args(l_bs_init_bits_fail, 0) -> [u8,f,u8,x8];
var_args(l_select_val_atoms, 2) -> [y8,f,u8];
var_args(l_is_eq_exact_immed, 24) -> [f,{x,15},t];
var_args(l_select_val2, 9) -> [{x,6},f,t,f,t,f];
var_args(l_call_ext, 29) -> [{e,{dict,new,0}}];
var_args(is_integer, 2) -> [f,{x,2}];
var_args(badmatch, 5) -> [{y,3}];
var_args(try_case_end, 0) -> [{x,0}];
var_args(l_fdiv, 0) -> [fr,fr,fr];
var_args(is_tuple, 10) -> [f,t];
var_args(l_get, 6) -> [t,t];
var_args(l_call_ext_last, 6) -> [e,u32];
var_args(l_bif1, 1) -> [f,b,{x,0},t];
var_args(l_move_call_ext, 24) -> [t,{e,{erlang,system_info,1}}];
var_args(l_move_call_ext_last, 5) -> [e,u32,t];
var_args(get_list, 6) -> [x8,y8,y8];
var_args(case_end, 4) -> [{x,3}];
var_args(put_list, 9) -> [x8,t,y8];
var_args(l_call_ext, 30) -> [{e,{sofs,to_external,1}}];
var_args(l_bs_test_unit_8, 1) -> [f,{x,3}];
var_args(l_gc_bif1, 3) -> [f,{b,{erlang,byte_size,1}},{x,1},{u8,2},{x,0}];
var_args(self, 5) -> [t];
var_args(l_increment, 9) -> [t,u32,u8,t];
var_args(call_bif, 41) -> [{b,{ets,match_object,2}}];
var_args(call_bif, 40) -> [{b,{ets,safe_fixtable,2}}];
var_args(extract_next_element, 18) -> [{x,12}];
var_args(extract_next_element2, 11) -> [{x,11}];
var_args(move_return, 9) -> [{smallint,2}];
var_args(is_nil, 14) -> [f,{x,13}];
var_args(l_move_call_only, 6) -> [f,nil];
var_args(l_bs_restore2, 2) -> [{x,0},{u,1}];
var_args(l_move_call, 18) -> [{smallint,2},f];
var_args(l_move_call, 17) -> [{smallint,12},f];
var_args(l_is_eq_exact_immed, 25) -> [f,{x,16},t];
var_args(bif1_body, 5) -> [b,t,{x,1}];
var_args(move, 10) -> [t,{x,7}];
var_args(is_integer, 3) -> [f,{x,4}];
var_args(l_allocate_zero, 8) -> [{u,9}];
var_args(is_nil, 15) -> [f,{x,14}];
var_args(is_list, 4) -> [f,{x,4}];
var_args(case_end, 5) -> [{y,1}];
var_args(is_atom, 6) -> [f,t];
var_args(l_move_call_ext, 26) -> [t,{e,{lists,duplicate,2}}];
var_args(call_bif, 42) -> [{b,{ets,next,2}}];
var_args(get_tuple_element, 9) -> [y8,u8,{x,0}];
var_args(l_move_call_ext, 27) -> [t,{e,{lists,seq,2}}];
var_args(l_is_eq_exact_immed, 26) -> [f,{y,4},t];
var_args(l_increment, 5) -> [{x,0},u8,u8,x8];
var_args(l_fsub, 0) -> [fr,fr,fr];
var_args(l_increment, 7) -> [x8,u32,u8,x8];
var_args(put_list, 7) -> [{smallint,10},{x,0},t];
var_args(l_move_call_ext, 28) -> [t,{e,{test_server,lookup_config,2}}];
var_args(call_bif, 43) -> [{b,{ets,match,2}}];
var_args(l_call_ext, 31) -> [{e,{ordsets,from_list,1}}];
var_args(extract_next_element3, 6) -> [{x,6}];
var_args(l_bsr, 1) -> [f,u8,{x,0}];
var_args(badmatch, 6) -> [{x,4}];
var_args(l_move_call_ext, 30) -> [t,{e,{erlang,binary_to_term,1}}];
var_args(l_move_call_ext, 29) -> [t,{e,{mnesia_lib,dbg_out,2}}];
var_args(l_call_ext, 33) -> [{e,{erlang,put,2}}];
var_args(l_call_ext, 32) -> [{e,{dict,store,3}}];
var_args(extract_next_element2, 12) -> [{y,0}];
var_args(is_integer, 4) -> [f,{x,3}];
var_args(is_nonempty_list, 20) -> [f,{x,17}];
var_args(l_move_call, 19) -> [{y,12},f];
var_args(l_is_eq_exact_literal, 5) -> [f,{x,6},t];
var_args(l_call_ext_only, 0) -> [{e,{gen_server,call,3}}];
var_args(l_plus, 2) -> [f,u8,y8];
var_args(move_deallocate_return, 8) -> [t,{u,5}];
var_args(move_return, 10) -> [{smallint,0}];
var_args(l_is_eq_exact_literal, 6) -> [f,{x,5},t];
var_args(l_int_div, 0) -> [f,u8,{x,0}];
var_args(l_rem, 1) -> [f,u8,{x,0}];
var_args(l_bs_get_binary_imm2, 0) -> [f,x8,u8,u8,{u8,0},x8];
var_args(l_move_call_ext, 31) -> [t,{e,{erlang,erase,1}}];
var_args(l_call_ext, 34) -> [{e,{lists,duplicate,2}}];
var_args(l_move_call, 20) -> [{a,foo},f];
var_args(init, 10) -> [{y,10}];
var_args(catch_end, 5) -> [{y,5}];
var_args(node, 4) -> [t];
var_args(put_list, 10) -> [{x,0},t,t];
var_args(l_allocate, 9) -> [u32];
var_args(l_bif2, 5) -> [f,{b,{erlang,'==',2}},t];
var_args(is_atom, 4) -> [f,{x,4}];
var_args(l_move_call, 21) -> [{a,false},f];
var_args(l_fetch, 16) -> [{x,5},t];
var_args(l_increment, 6) -> [x8,{u,4294967295},u8,{x,0}];
var_args(bif1_body, 6) -> [b,t,{x,0}];
var_args(l_fetch, 17) -> [t,{x,3}];
var_args(test_heap_1_put_list, 2) -> [u8,i8];
var_args(call_bif, 44) -> [{b,{erlang,monitor,2}}];
var_args(node, 2) -> [{x,2}];
var_args(l_call_ext, 37) -> [{e,{asn1ct_gen,mk_var,1}}];
var_args(l_call_ext, 36) -> [{e,{lists,delete,2}}];
var_args(l_call_ext, 35) -> [{e,{gb_trees,empty,0}}];
var_args(l_call_last, 9) -> [f,{u,9}];
var_args(is_nil, 16) -> [f,{y,2}];
var_args(l_bs_put_string, 0) -> [{u,1},str];
var_args(is_list, 5) -> [f,{x,5}];
var_args(case_end, 6) -> [{y,3}];
var_args(l_yield, 0) -> [];
var_args(l_move_call, 22) -> [{a,endDocument},f];
var_args(l_catch, 6) -> [{y,6},t];
var_args(get_list, 7) -> [{x,0},y8,y8];
var_args(l_bs_get_binary_all2, 1) -> [f,{x,0},u8,u8,x8];
var_args(l_is_eq_exact_immed, 27) -> [f,{x,17},t];
var_args(l_call_ext, 38) -> [{e,{gb_trees,lookup,2}}];
var_args(case_end, 7) -> [{x,4}];
var_args(is_nonempty_list, 21) -> [f,{y,4}];
var_args(l_move_call_ext, 33) -> [t,{e,{lists,sublist,3}}];
var_args(l_select_tuple_arity, 3) -> [x8,f,u8];
var_args(l_move_call_ext, 34) -> [t,{e,{mnesia_lib,set,2}}];
var_args(l_bs_append, 1) -> [f,u8,u8,u8,{x,0}];
var_args(l_bs_get_integer, 0) -> [f,u8,u8,u8,x8];
var_args(l_move_call_last, 6) -> [f,{u,4},t];
var_args(call_bif, 46) -> [{b,{erlang,garbage_collect,0}}];
var_args(call_bif, 45) -> [{b,{erlang,fun_info,2}}];
var_args(l_call_ext, 39) -> [{e,{file,read_file_info,1}}];
var_args(l_is_eq_exact_immed, 21) -> [f,{y,10},{a,ber}];
var_args(is_nonempty_list, 22) -> [f,{x,18}];
var_args(is_function, 1) -> [f,t];
var_args(l_gc_bif1, 4) -> [f,{b,{erlang,length,1}},x8,u8,y8];
var_args(l_is_ne_exact_immed, 5) -> [f,{x,4},t];
var_args(l_jump_on_val, 0) -> [x8,f,u8,u8];
var_args(call_bif, 47) -> [{b,{erlang,register,2}}];
var_args(l_call_ext, 40) -> [{e,{io,format,3}}];
var_args(l_is_ne_exact_immed, 6) -> [f,{y,0},t];
var_args(extract_next_element2, 13) -> [{x,13}];
var_args(move_jump, 3) -> [f,{x,4}];
var_args(badmatch, 7) -> [{y,4}];
var_args(l_bs_test_unit_8, 2) -> [f,{x,2}];
var_args(l_move_call, 24) -> [{x,6},f];
var_args(l_move_call, 23) -> [{smallint,9},f];
var_args(l_is_eq_exact_immed, 28) -> [f,{y,5},t];
var_args(l_call_ext, 41) -> [{e,{lists,keydelete,3}}];
var_args(is_nil, 18) -> [f,{x,16}];
var_args(is_nil, 17) -> [f,{x,15}];
var_args(is_nonempty_list, 23) -> [f,{x,20}];
var_args(l_move_call, 27) -> [{a,ets},f];
var_args(l_move_call, 26) -> [{a,schema},f];
var_args(l_move_call, 25) -> [{smallint,6},f];
var_args(l_select_tuple_arity, 1) -> [{x,0},f,{u,8}];
var_args(l_fetch, 18) -> [t,{x,2}];
var_args(l_move_call_ext, 35) -> [t,{e,{re,replace,4}}];
var_args(l_bs_get_integer_32, 1) -> [{x,0},f,u8,x8];
var_args(fmove_1, 3) -> [t,fr];
var_args(l_int_div, 1) -> [f,u8,x8];
var_args(l_is_eq_exact_immed, 29) -> [f,{x,18},t];
var_args(l_move_call_ext_only, 4) -> [e,{x,1}];
var_args(l_move_call_ext, 36) -> [{x,4},e];
var_args(call_bif, 1) -> [{b,{erlang,raise,3}}];
var_args(l_call_ext, 48) -> [{e,{filename,basename,1}}];
var_args(l_call_ext, 47) -> [{e,{erlang,binary_to_term,1}}];
var_args(l_call_ext, 46) -> [{e,{lists,last,1}}];
var_args(l_call_ext, 45) -> [{e,{asn1ct_gen,get_inner,1}}];
var_args(l_call_ext, 44) -> [{e,{erl_syntax,atom,1}}];
var_args(l_call_ext, 43) -> [{e,{file,read_file,1}}];
var_args(l_call_ext, 42) -> [{e,{ssh_channel,cache_lookup,2}}];
var_args(extract_next_element3, 7) -> [{x,8}];
var_args(move_return, 11) -> [{smallint,3}];
var_args(l_bs_test_unit_8, 3) -> [f,{x,1}];
var_args(l_bs_save2, 1) -> [{x,0},{u,1}];
var_args(bif2_body, 2) -> [b,{x,2}];
var_args(is_binary, 3) -> [f,t];
var_args(l_move_call_ext, 37) -> [{a,schema},e];
var_args(bif1_body, 7) -> [b,t,t];
var_args(l_allocate, 8) -> [{u,8}];
var_args(l_call_ext, 50) -> [{e,{file,read,2}}];
var_args(l_call_ext, 49) -> [{e,{file,make_dir,1}}];
var_args(move_return, 12) -> [{smallint,4}];
var_args(l_trim, 8) -> [{u,9}];
var_args(is_nil, 19) -> [f,{y,3}];
var_args(case_end, 8) -> [{y,4}];
var_args(fconv, 1) -> [t,{fr,1}];
var_args(deallocate_return, 12) -> [u32];
var_args(l_catch, 8) -> [t,t];
var_args(l_move_call_ext_last, 4) -> [e,{u,3},t];
var_args(l_gc_bif1, 6) -> [f,b,{x,0},u8,y8];
var_args(move_deallocate_return, 10) -> [t,u32];
var_args(l_move_call_ext, 38) -> [{a,func},e];
var_args(l_call_ext, 53) -> [{e,{gb_trees,get,2}}];
var_args(l_call_ext, 52) -> [{e,{cerl,get_ann,1}}];
var_args(l_call_ext, 51) -> [{e,{mnesia_lib,exists,1}}];
var_args(extract_next_element, 19) -> [{x,13}];
var_args(l_call_fun, 4) -> [u8];
var_args(l_move_call_ext_only, 5) -> [{e,{io,format,2}},t];
var_args(l_move_call_ext, 40) -> [t,{e,{re,split,3}}];
var_args(l_move_call_ext, 39) -> [{smallint,2},e];
var_args(l_bs_put_string, 2) -> [u32,str];
var_args(l_is_ne_exact_literal, 0) -> [f,t,t];
var_args(l_call_ext, 54) -> [{e,{ordsets,subtract,2}}];
var_args(move_return, 13) -> [{a,no}];
var_args(badmatch, 8) -> [{y,0}];
var_args(l_move_call_ext, 42) -> [t,{e,{io,format,1}}];
var_args(l_move_call_ext, 41) -> [t,{e,{io,fwrite,2}}];
var_args(l_bs_test_unit_8, 4) -> [f,t];
var_args(is_pid, 1) -> [f,t];
var_args(is_boolean, 0) -> [f,t];
var_args(l_get, 4) -> [{a,asn1_module},y8];
var_args(bif1_body, 4) -> [{b,{erlang,hd,1}},y8,x8];
var_args(l_fetch, 19) -> [{y,0},t];
var_args(bs_context_to_binary, 5) -> [t];
var_args(l_select_tuple_arity2, 2) -> [y8,f,u8,f,u8,f];
var_args(l_call_ext_last, 5) -> [e,{u,5}];
var_args(l_select_val2, 10) -> [{x,7},f,t,f,t,f];
var_args(l_call_ext, 58) -> [{e,{erlang,term_to_binary,1}}];
var_args(l_call_ext, 57) -> [{e,{file,write,2}}];
var_args(l_call_ext, 56) -> [{e,{asn1ct_gen,list2name,1}}];
var_args(l_call_ext, 55) -> [{e,{sofs,family_union,2}}];
var_args(extract_next_element2, 14) -> [{x,14}];
var_args(move_jump, 4) -> [f,{y,0}];
var_args(l_move_call_only, 7) -> [f,{x,7}];
var_args(l_bs_skip_bits2, 0) -> [f,x8,x8,u8];
var_args(put_list, 12) -> [t,t,t];
var_args(l_bs_get_binary2, 1) -> [f,{x,0},u8,x8,u8,{u8,0},x8];
var_args(l_call_ext, 65) -> [{e,{mnesia_lib,cs_to_storage_type,2}}];
var_args(l_call_ext, 64) -> [{e,{file,rename,2}}];
var_args(l_call_ext, 63) -> [{e,{filename,dirname,1}}];
var_args(l_call_ext, 62) -> [{e,{lists,append,1}}];
var_args(l_call_ext, 61) -> [{e,{asn1ct_gen,type,1}}];
var_args(l_call_ext, 60) -> [{e,{test_server,fail,1}}];
var_args(l_call_ext, 59) -> [{e,{random,uniform,1}}];
var_args(extract_next_element, 20) -> [{x,14}];
var_args(extract_next_element2, 15) -> [{x,16}];
var_args(move_return, 14) -> [{a,undefined}];
var_args(bs_context_to_binary, 1) -> [{x,1}];
var_args(l_move_call, 28) -> [{smallint,100},f];
var_args(init, 11) -> [{y,11}];
var_args(l_bs_skip_bits2, 1) -> [f,t,t,u8];
var_args(l_move_call_last, 7) -> [f,u32,t];
var_args(l_call_ext, 68) -> [{e,{lists,dropwhile,2}}];
var_args(l_call_ext, 67) -> [{e,{mnesia_monitor,use_dir,0}}];
var_args(l_call_ext, 66) -> [{e,{lists,splitwith,2}}];
var_args(move_jump, 6) -> [f,{y,2}];
var_args(move_jump, 5) -> [f,{y,1}];
var_args(move_return, 15) -> [{smallint,8}];
var_args(is_nonempty_list_test_heap, 0) -> [f,{u,5},{u8,1}];
var_args(l_move_call_ext, 44) -> [{smallint,3},e];
var_args(catch_end, 6) -> [{y,6}];
var_args(l_get, 5) -> [{a,mnesia_activity_state},t];
var_args(l_bs_skip_bits_all2, 1) -> [f,{x,3},{u8,8}];
var_args(l_bs_start_match2, 2) -> [t,f,u8,u8,{x,0}];
var_args(l_move_call_ext, 46) -> [t,{e,{asn1ct_name,curr,1}}];
var_args(l_wait_timeout, 2) -> [f,u32];
var_args(l_element, 0) -> [t,{x,0},{x,1}];
var_args(l_move_call_ext, 47) -> [{y,8},e];
var_args(l_move_call_ext, 48) -> [{a,id},e];
var_args(l_call_ext, 70) -> [{e,{mnesia_schema,list2cs,1}}];
var_args(l_call_ext, 69) -> [{e,{gb_trees,insert,3}}];
var_args(l_move_call_only, 8) -> [f,{x,8}];
var_args(case_end, 9) -> [{y,0}];
var_args(is_pid, 0) -> [f,{x,0}];
var_args(l_move_call, 29) -> [{a,x},f];
var_args(l_select_tuple_arity, 2) -> [{x,0},f,{u,10}];
var_args(is_nonempty_list_test_heap, 1) -> [f,u8,u8];
var_args(catch_end, 8) -> [t];
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
var_args(is_nonempty_list, 25) -> [f,{x,19}];
var_args(is_nonempty_list, 24) -> [f,{y,9}];
var_args(try_end, 6) -> [{y,6}];
var_args(l_bs_private_append, 0) -> [f,u8,t];
var_args(deallocate_return, 9) -> [{u,9}];
var_args(l_new_bs_put_float_imm, 0) -> [f,{u,64},{u8,0},{x,0}];
var_args(l_move_call, 30) -> [{smallint,1000},f];
var_args(l_call_ext_only, 1) -> [{e,{asn1ct_gen,emit,1}}];
var_args(l_apply, 0) -> [];
var_args(l_move_call_ext, 25) -> [{a,funky},{e,{estone_SUITE,req,2}}];
var_args(fmove_1, 2) -> [t,{fr,0}];
var_args(put_list, 11) -> [y8,t,y8];
var_args(l_bif2, 6) -> [f,b,t];
var_args(bif2_body, 3) -> [b,t];
var_args(self, 3) -> [{x,3}];
var_args(is_tuple, 5) -> [f,{x,7}];
var_args(l_call_ext, 76) -> [{e,{gb_trees,from_orddict,1}}];
var_args(l_call_ext, 75) -> [{e,{sets,is_element,2}}];
var_args(l_call_ext, 74) -> [{e,{erl_syntax,get_pos,1}}];
var_args(extract_next_element3, 8) -> [{x,11}];
var_args(l_new_bs_put_float_imm, 1) -> [f,{u,32},{u8,0},t];
var_args(l_move_call, 33) -> [{smallint,4},f];
var_args(l_move_call, 32) -> [{smallint,42},f];
var_args(l_move_call, 31) -> [{y,8},f];
var_args(l_catch, 7) -> [{y,20},t];
var_args(catch_end, 7) -> [{y,20}];
var_args(l_is_ne_exact_immed, 7) -> [f,{y,1},t];
var_args(test_heap_1_put_list, 3) -> [u8,y8];
var_args(l_call_ext, 77) -> [{e,{erlang,max,2}}];
var_args(extract_next_element3, 9) -> [{x,10}];
var_args(move_jump, 8) -> [f,{a,false}];
var_args(is_nonempty_list, 26) -> [f,{y,0}];
var_args(l_move_call, 34) -> [{a,true},f];
var_args(is_binary, 2) -> [f,{x,2}];
var_args(l_fetch, 21) -> [t,{x,5}];
var_args(l_fetch, 20) -> [t,{y,5}];
var_args(l_bs_skip_bits_all2, 2) -> [f,t,u8];
var_args(l_is_ne_exact_immed, 8) -> [f,{y,2},t];
var_args(l_bs_get_utf8, 1) -> [t,f,t];
var_args(l_bs_get_integer_imm, 0) -> [t,u8,u8,f,u8,x8];
var_args(l_select_val2, 11) -> [{y,1},f,t,f,t,f];
var_args(self, 4) -> [{y,0}];
var_args(l_call_ext, 79) -> [{e,{file,get_cwd,0}}];
var_args(l_call_ext, 78) -> [{e,{file,write_file,2}}];
var_args(l_call_last, 10) -> [f,{u,10}];
var_args(move_return, 18) -> [{a,ignore}];
var_args(is_nil, 20) -> [f,{y,0}];
var_args(recv_mark, 0) -> [f];
var_args(badmatch, 10) -> [{y,9}];
var_args(badmatch, 9) -> [{x,5}];
var_args(is_function, 0) -> [f,{x,0}];
var_args(l_recv_set, 0) -> [];
var_args(l_new_bs_put_integer, 1) -> [f,t,u8,u8,t];
var_args(badmatch, 17) -> [t];
var_args(move2, 9) -> [{x,0},x8,{x,0},x8];
var_args(l_call_ext, 88) -> [{e,{ordsets,intersection,2}}];
var_args(l_call_ext, 87) -> [{e,{lists,sublist,3}}];
var_args(l_call_ext, 86) -> [{e,{asn1ct_name,new,1}}];
var_args(l_call_ext, 85) -> [{e,{beam_utils,code_at,2}}];
var_args(l_call_ext, 84) -> [{e,{cerl,var_name,1}}];
var_args(l_call_ext, 83) -> [{e,{gb_sets,empty,0}}];
var_args(l_call_ext, 82) -> [{e,{xref_utils,xset,2}}];
var_args(l_call_ext, 81) -> [{e,{cerl,c_tuple,1}}];
var_args(l_call_ext, 80) -> [{e,{mnesia_lib,intersect,2}}];
var_args(extract_next_element2, 16) -> [{x,15}];
var_args(move_jump, 9) -> [f,{a,asn1_NOVALUE}];
var_args(is_nil, 21) -> [f,{x,17}];
var_args(l_bs_put_string, 1) -> [{u,4},str];
var_args(l_select_val2, 12) -> [{x,8},f,t,f,t,f];
var_args(is_atom, 5) -> [f,{x,5}];
var_args(l_bs_init_bits_fail, 1) -> [u32,f,u8,t];
var_args(l_bs_get_integer_16, 0) -> [t,f,t];
var_args(l_move_call_ext, 32) -> [{a,auto_repair},{e,{mnesia_monitor,get_env,1}}];
var_args(is_float, 0) -> [f,{x,0}];
var_args(l_is_eq_exact_immed, 30) -> [f,{x,19},t];
var_args(l_bs_get_float2, 0) -> [f,t,u8,t,u8,u8,t];
var_args(l_trim, 11) -> [u32];
var_args(l_call_ext, 91) -> [{e,{ordsets,is_element,2}}];
var_args(l_call_ext, 90) -> [{e,{lists,seq,2}}];
var_args(l_call_ext, 89) -> [{e,{dict,fetch,2}}];
var_args(l_is_ne_exact_immed, 9) -> [f,{x,5},t];
var_args(l_fast_element, 4) -> [{x,0},{u,3},{x,0}];
var_args(move_return, 20) -> [{smallint,16}];
var_args(move_return, 19) -> [{a,none}];
var_args(bs_context_to_binary, 3) -> [{y,0}];
var_args(is_nonempty_list, 28) -> [f,{y,6}];
var_args(is_nonempty_list, 27) -> [f,{x,24}];
var_args(deallocate_return, 11) -> [{u,11}];
var_args(deallocate_return, 10) -> [{u,10}];
var_args(l_bs_init_bits, 0) -> [u32,u32,u8,t];
var_args(l_increment, 8) -> [{x,0},u32,u8,y8];
var_args(l_bif1, 2) -> [f,b,t,t];
var_args(l_minus, 2) -> [f,u8,t];
var_args(is_tuple, 6) -> [f,{x,5}];
var_args(l_call_ext, 98) -> [{e,{ets,tab2list,1}}];
var_args(l_call_ext, 97) -> [{e,{orddict,find,2}}];
var_args(l_call_ext, 96) -> [{e,{unicode,characters_to_binary,1}}];
var_args(l_call_ext, 95) -> [{e,{proplists,get_bool,2}}];
var_args(l_call_ext, 94) -> [{e,{mnesia,abort,1}}];
var_args(l_call_ext, 93) -> [{e,{rpc,call,4}}];
var_args(l_call_ext, 92) -> [{e,{sofs,difference,2}}];
var_args(l_call_last, 11) -> [f,u32];
var_args(l_is_eq_exact_immed, 31) -> [f,{x,22},t];
var_args(extract_next_element, 22) -> [{x,18}];
var_args(move_return, 21) -> [{smallint,5}];
var_args(badmatch, 11) -> [{y,5}];
var_args(case_end, 10) -> [{y,7}];
var_args(get_list, 10) -> [t,t,t];
var_args(get_list, 8) -> [x8,y8,x8];
var_args(l_select_val2, 13) -> [{y,2},f,t,f,t,f];
var_args(l_call_ext_only, 2) -> [{e,{mnesia_monitor,get_env,1}}];
var_args(l_fetch, 22) -> [t,{y,0}];
var_args(l_call_ext, 104) -> [{e,{sets,add_element,2}}];
var_args(l_call_ext, 103) -> [{e,{sofs,relation_to_family,1}}];
var_args(l_call_ext, 102) -> [{e,{mnesia_lib,val,1}}];
var_args(l_call_ext, 101) -> [{e,{erl_scan,token_info,2}}];
var_args(l_call_ext, 100) -> [{e,{asn1ct_name,clear,0}}];
var_args(l_call_ext, 99) -> [{e,{asn1ct_gen_ber_bin_v2,decode_class,1}}];
var_args(l_is_eq_exact_immed, 32) -> [f,{x,23},t];
var_args(get_tuple_element, 10) -> [y8,u8,y8];
var_args(wait_timeout, 0) -> [f,t];
var_args(move_return, 22) -> [{smallint,6}];
var_args(badmatch, 12) -> [{y,6}];
var_args(is_nonempty_list, 29) -> [f,{x,21}];
var_args(l_times, 2) -> [f,u8,t];
var_args(l_select_val2, 14) -> [{y,3},f,t,f,t,f];
var_args(l_apply_fun, 0) -> [];
var_args(init, 17) -> [t];
var_args(l_gc_bif1, 7) -> [f,b,t,u8,t];
var_args(move, 12) -> [t,{x,8}];
var_args(l_is_eq_exact_immed, 33) -> [f,{x,20},t];
var_args(l_bs_get_integer_32, 2) -> [x8,f,u8,{x,0}];
var_args(move_return, 24) -> [{smallint,64}];
var_args(move_return, 23) -> [{smallint,7}];
var_args(is_nil, 22) -> [f,{y,5}];
var_args(badmatch, 13) -> [{y,1}];
var_args(move_deallocate_return, 9) -> [t,{u,7}];
var_args(l_new_bs_put_float_imm, 2) -> [f,u32,u8,t];
var_args(l_call_ext_only, 3) -> [{e,{lists,reverse,1}}];
var_args(is_integer_allocate, 1) -> [f,t,u32];
var_args(is_tuple, 8) -> [f,{x,6}];
var_args(is_tuple, 7) -> [f,{y,4}];
var_args(l_is_eq_exact_immed, 34) -> [f,{y,6},t];
var_args(extract_next_element, 23) -> [{x,15}];
var_args(move_jump, 10) -> [f,{a,true}];
var_args(is_nil, 23) -> [f,{x,19}];
var_args(bs_context_to_binary, 4) -> [{x,4}];
var_args(badmatch, 14) -> [{x,8}];
var_args(is_nonempty_list, 32) -> [f,{y,8}];
var_args(is_nonempty_list, 31) -> [f,{x,22}];
var_args(is_nonempty_list, 30) -> [f,{x,25}];
var_args(get_list, 9) -> [x8,y8,{x,0}];
var_args(l_bs_init_fail, 1) -> [u32,f,u8,t];
var_args(l_move_call_ext, 43) -> [{smallint,1000},{e,{test_server,sleep,1}}];
var_args(l_fast_element, 5) -> [t,u32,t];
var_args(l_bs_test_tail_imm2, 0) -> [f,t,u32];
var_args(move_return, 25) -> [{a,nomatch}];
var_args(is_nil, 24) -> [f,{y,4}];
var_args(l_move_call_only, 9) -> [f,{smallint,1}];
var_args(fconv, 2) -> [t,fr];
var_args(is_nonempty_list, 34) -> [f,{y,7}];
var_args(is_nonempty_list, 33) -> [f,{y,5}];
var_args(l_move_call_ext, 45) -> [{a,available_internal_state},{e,{erts_debug,set_internal_state,2}}];
var_args(init, 12) -> [{y,12}];
var_args(test_heap_1_put_list, 4) -> [u32,t];
var_args(l_bs_add, 1) -> [f,u8,t];
var_args(l_wait_timeout, 0) -> [f,{u,1000}];
var_args(l_gc_bif2, 0) -> [f,b,u8,t];
var_args(try_end, 7) -> [t];
var_args(l_bs_put_utf16, 0) -> [f,u8,t];
var_args(is_tuple, 9) -> [f,{y,1}];
var_args(l_is_eq_exact_immed, 35) -> [f,{y,7},t];
var_args(extract_next_element, 25) -> [{x,17}];
var_args(extract_next_element, 24) -> [{y,7}];
var_args(move_jump, 11) -> [f,{smallint,0}];
var_args(move_return, 27) -> [{smallint,128}];
var_args(move_return, 26) -> [{x,6}];
var_args(l_trim, 9) -> [{u,11}];
var_args(is_nil, 25) -> [f,{x,22}];
var_args(l_bs_validate_unicode, 0) -> [f,t];
var_args(is_nonempty_list, 35) -> [f,{x,26}];
var_args(l_jump_on_val, 1) -> [{x,0},f,u8,u8];
var_args(move, 11) -> [nil,{x,10}];
var_args(l_bs_utf16_size, 0) -> [t,t];
var_args(l_bs_get_binary2, 2) -> [f,t,u8,t,u8,u8,t];
var_args(move_return, 29) -> [{smallint,12}];
var_args(move_return, 28) -> [{smallint,10}];
var_args(is_nil, 26) -> [f,{x,18}];
var_args(raise, 1) -> [t,t];
var_args(l_bs_restore2, 3) -> [t,u32];
var_args(l_int_bnot, 0) -> [f,t,u8,t];
var_args(l_bs_get_binary_imm2, 1) -> [f,{x,0},u8,u8,u8,x8];
var_args(is_bitstr, 0) -> [f,t];
var_args(move_return, 35) -> [{a,'$end_of_table'}];
var_args(move_return, 34) -> [{smallint,13}];
var_args(move_return, 33) -> [{smallint,11}];
var_args(move_return, 32) -> [{smallint,32}];
var_args(move_return, 31) -> [{smallint,9}];
var_args(move_return, 30) -> [{a,yes}];
var_args(is_nil, 29) -> [f,{x,21}];
var_args(is_nil, 28) -> [f,{x,20}];
var_args(is_nil, 27) -> [f,{x,23}];
var_args(l_new_bs_put_binary_all, 2) -> [f,t,u8];
var_args(l_new_bs_put_binary, 0) -> [f,t,u8,u8,t];
var_args(badmatch, 16) -> [{y,8}];
var_args(badmatch, 15) -> [{x,6}];
var_args(is_nonempty_list, 37) -> [f,{x,27}];
var_args(is_nonempty_list, 36) -> [f,{x,23}];
var_args(l_bs_save2, 2) -> [t,u32];
var_args(fmove_2, 2) -> [fr,t];
var_args(is_reference, 0) -> [f,t];
var_args(is_port, 0) -> [f,t];
var_args(is_nonempty_list, 38) -> [f,{x,29}];
var_args(is_number, 0) -> [f,t];
var_args(l_select_tuple_arity, 4) -> [t,f,u32];
var_args(move, 13) -> [t,t];
var_args(l_bs_get_binary_all_reuse, 1) -> [t,f,u8];
var_args(init, 13) -> [{y,13}];
var_args(l_wait_timeout, 1) -> [f,{u,1}];
var_args(l_trim, 10) -> [{u,10}];
var_args(l_fnegate, 0) -> [fr,fr];
var_args(l_bs_init, 0) -> [u32,u32,u8,t];
var_args(l_jump_on_val, 2) -> [t,f,u32,u32];
var_args(init, 14) -> [{y,14}];
var_args(l_bs_put_utf8, 0) -> [f,t];
var_args(l_bs_utf8_size, 0) -> [t,t];
var_args(l_bxor, 0) -> [f,u8,t];
var_args(l_new_bs_put_float, 0) -> [f,t,u8,u8,t];
var_args(l_bs_validate_unicode_retract, 0) -> [f];
var_args(l_apply_last, 0) -> [u32];
var_args(init, 16) -> [{y,15}];
var_args(init, 15) -> [{y,16}];
var_args(l_bs_get_binary_imm2, 2) -> [f,t,u8,u32,u8,t];
var_args(l_is_function2, 0) -> [f,t,u8];
var_args(l_gc_bif3, 0) -> [f,b,t,u8,t];
var_args(l_bs_match_string, 2) -> [t,f,u32,str];
var_args(l_bs_get_integer_imm, 1) -> [t,u32,u8,f,u8,t];
var_args(l_bor, 2) -> [f,u8,t];
var_args(l_new_bs_put_binary_imm, 0) -> [f,u32,u8,t];
var_args(l_bs_get_integer_8, 2) -> [t,f,t];
var_args(l_bs_start_match2, 3) -> [t,f,u8,u32,t];
var_args(l_rem, 2) -> [f,u8,t];
var_args(l_bs_get_integer_small_imm, 1) -> [t,u32,f,u8,t];
var_args(l_bsl, 2) -> [f,u8,t];
var_args(l_apply_only, 0) -> [];
var_args(on_load, 0) -> [];
var_args(move2, 10) -> [t,t,t,t];
var_args(l_int_div, 2) -> [f,u8,t];
var_args(l_bs_test_unit, 0) -> [f,t,u8];
var_args(l_select_val_atoms, 3) -> [t,f,u32];
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
var_args(l_bsr, 2) -> [f,u8,t];
var_args(l_bs_get_integer_32, 3) -> [t,f,u8,t];
var_args(allocate_heap, 1) -> [u32,u32,u8];
var_args(is_tuple_of_arity, 4) -> [f,t,u32];
var_args(test_arity, 4) -> [f,t,u32];
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
var_index(test_heap, 0) -> 2;
var_index(l_call, 0) -> 3;
var_index(move, 2) -> 4;
var_index(move, 3) -> 5;
var_index(l_put_tuple, 0) -> 6;
var_index(move2, 1) -> 7;
var_index(get_tuple_element, 0) -> 8;
var_index(badmatch, 0) -> 9;
var_index(put_list, 0) -> 10;
var_index(is_tuple_of_arity, 1) -> 11;
var_index(get_tuple_element, 1) -> 12;
var_index(move2, 0) -> 13;
var_index(move, 4) -> 14;
var_index(l_call_only, 0) -> 15;
var_index(l_is_eq_exact_immed, 0) -> 16;
var_index(l_is_eq_exact_immed, 1) -> 17;
var_index(is_tuple_of_arity, 0) -> 18;
var_index(put_list, 1) -> 19;
var_index(get_list, 0) -> 20;
var_index(l_put_tuple, 1) -> 21;
var_index(l_is_ge, 0) -> 22;
var_index(l_call_ext, 105) -> 23;
var_index(l_make_fun, 0) -> 24;
var_index(l_move_call_last, 0) -> 25;
var_index(extract_next_element2, 0) -> 26;
var_index(is_tuple_of_arity, 2) -> 27;
var_index(return, 0) -> 28;
var_index(l_fetch, 0) -> 29;
var_index(call_bif, 3) -> 30;
var_index(move_deallocate_return, 0) -> 31;
var_index(l_trim, 0) -> 32;
var_index(l_allocate, 0) -> 33;
var_index(move2, 3) -> 34;
var_index(l_fetch, 1) -> 35;
var_index(put_list, 2) -> 36;
var_index(is_nil, 0) -> 37;
var_index(is_nonempty_list, 0) -> 38;
var_index(extract_next_element, 0) -> 39;
var_index(l_is_eq_exact_immed, 2) -> 40;
var_index(l_allocate, 1) -> 41;
var_index(jump, 0) -> 42;
var_index(move_return, 36) -> 43;
var_index(deallocate_return, 0) -> 44;
var_index(case_end, 0) -> 45;
var_index(get_list, 1) -> 46;
var_index(call_bif, 7) -> 47;
var_index(move2, 2) -> 48;
var_index(l_is_eq_exact, 0) -> 49;
var_index(l_move_call, 35) -> 50;
var_index(get_tuple_element, 2) -> 51;
var_index(l_allocate, 2) -> 52;
var_index(move_return, 0) -> 53;
var_index(l_is_eq_exact_immed, 3) -> 54;
var_index(l_select_val2, 0) -> 55;
var_index(get_tuple_element, 3) -> 56;
var_index(move, 5) -> 57;
var_index(l_fetch, 2) -> 58;
var_index(init2, 0) -> 59;
var_index(get_list, 2) -> 60;
var_index(l_is_eq_exact_immed, 4) -> 61;
var_index(init, 0) -> 62;
var_index(deallocate_return, 1) -> 63;
var_index(put_list, 3) -> 64;
var_index(call_bif, 8) -> 65;
var_index(extract_next_element, 1) -> 66;
var_index(is_tuple_of_arity, 3) -> 67;
var_index(get_tuple_element, 4) -> 68;
var_index(move2, 4) -> 69;
var_index(l_fetch, 3) -> 70;
var_index(init, 1) -> 71;
var_index(l_put_tuple, 2) -> 72;
var_index(move2, 5) -> 73;
var_index(l_allocate, 3) -> 74;
var_index(deallocate_return, 2) -> 75;
var_index(l_fetch, 4) -> 76;
var_index(init3, 0) -> 77;
var_index(is_nonempty_list, 1) -> 78;
var_index(l_move_call_ext, 49) -> 79;
var_index(l_is_eq_exact_immed, 5) -> 80;
var_index(l_allocate_zero, 0) -> 81;
var_index(call_bif, 6) -> 82;
var_index(l_trim, 1) -> 83;
var_index(allocate_init, 0) -> 84;
var_index(allocate_heap, 0) -> 85;
var_index(test_heap_1_put_list, 0) -> 86;
var_index(l_allocate_zero, 1) -> 87;
var_index(init, 2) -> 88;
var_index(move_deallocate_return, 1) -> 89;
var_index(l_call_last, 0) -> 90;
var_index(call_bif, 48) -> 91;
var_index(l_is_eq_exact_literal, 0) -> 92;
var_index(is_nonempty_list, 2) -> 93;
var_index(l_move_call, 0) -> 94;
var_index(move_return, 1) -> 95;
var_index(is_tuple, 0) -> 96;
var_index(l_is_eq_exact_immed, 6) -> 97;
var_index(is_list, 0) -> 98;
var_index(l_call_last, 1) -> 99;
var_index(deallocate_return, 3) -> 100;
var_index(is_nonempty_list_allocate, 0) -> 101;
var_index(l_move_call_only, 0) -> 102;
var_index(call_bif, 5) -> 103;
var_index(extract_next_element, 2) -> 104;
var_index(l_increment, 0) -> 105;
var_index(move, 6) -> 106;
var_index(l_is_lt, 0) -> 107;
var_index(l_trim, 2) -> 108;
var_index(l_gc_bif1, 0) -> 109;
var_index(l_select_val_atoms, 0) -> 110;
var_index(l_call_last, 2) -> 111;
var_index(move_deallocate_return, 2) -> 112;
var_index(is_nonempty_list, 3) -> 113;
var_index(l_bs_start_match2, 0) -> 114;
var_index(l_new_bs_put_integer_imm, 0) -> 115;
var_index(is_nil, 1) -> 116;
var_index(extract_next_element2, 1) -> 117;
var_index(l_select_val2, 2) -> 118;
var_index(remove_message, 0) -> 119;
var_index(l_move_call_only, 10) -> 120;
var_index(l_move_call_only, 1) -> 121;
var_index(init, 3) -> 122;
var_index(l_catch, 0) -> 123;
var_index(l_allocate_zero, 2) -> 124;
var_index(extract_next_element3, 0) -> 125;
var_index(get_tuple_element, 5) -> 126;
var_index(l_call_ext, 0) -> 127;
var_index(move_jump, 12) -> 128;
var_index(l_fetch, 5) -> 129;
var_index(extract_next_element, 3) -> 130;
var_index(is_nil, 2) -> 131;
var_index(move2, 6) -> 132;
var_index(l_allocate, 4) -> 133;
var_index(catch_end, 0) -> 134;
var_index(test_arity, 0) -> 135;
var_index(l_allocate_zero, 3) -> 136;
var_index(l_is_eq_exact_immed, 7) -> 137;
var_index(move_return, 2) -> 138;
var_index(move_return, 3) -> 139;
var_index(l_move_call_ext, 0) -> 140;
var_index(put_list, 4) -> 141;
var_index(move_return, 4) -> 142;
var_index(deallocate_return, 4) -> 143;
var_index(l_call_last, 3) -> 144;
var_index(move2, 7) -> 145;
var_index(l_is_eq_exact_immed, 8) -> 146;
var_index(l_plus, 0) -> 147;
var_index(move, 7) -> 148;
var_index(l_put_tuple, 3) -> 149;
var_index(call_bif, 2) -> 150;
var_index(l_select_tuple_arity2, 0) -> 151;
var_index(is_nonempty_list, 4) -> 152;
var_index(init, 4) -> 153;
var_index(is_nonempty_list, 5) -> 154;
var_index(get_list, 3) -> 155;
var_index(l_call_fun, 0) -> 156;
var_index(l_call_last, 4) -> 157;
var_index(l_move_call, 1) -> 158;
var_index(move_return, 5) -> 159;
var_index(l_bs_get_binary_all_reuse, 0) -> 160;
var_index(test_arity, 1) -> 161;
var_index(l_bs_match_string, 0) -> 162;
var_index(bif1_body, 0) -> 163;
var_index(l_move_call_only, 2) -> 164;
var_index(l_move_call_last, 1) -> 165;
var_index(is_nonempty_list, 6) -> 166;
var_index(l_bs_test_zero_tail2, 0) -> 167;
var_index(l_is_eq, 0) -> 168;
var_index(send, 0) -> 169;
var_index(set_tuple_element, 0) -> 170;
var_index(l_catch, 1) -> 171;
var_index(call_bif, 9) -> 172;
var_index(l_is_ne, 0) -> 173;
var_index(l_move_call_only, 3) -> 174;
var_index(l_move_call_ext_last, 0) -> 175;
var_index(l_select_val2, 1) -> 176;
var_index(l_select_val_smallints, 0) -> 177;
var_index(l_move_call_ext, 1) -> 178;
var_index(l_call_ext_only, 4) -> 179;
var_index(l_is_ne_exact_immed, 0) -> 180;
var_index(l_increment, 1) -> 181;
var_index(l_bs_add, 0) -> 182;
var_index(bif2_body, 0) -> 183;
var_index(is_nonempty_list, 7) -> 184;
var_index(extract_next_element, 4) -> 185;
var_index(case_end, 1) -> 186;
var_index(l_is_eq_exact_immed, 9) -> 187;
var_index(extract_next_element2, 2) -> 188;
var_index(move_deallocate_return, 3) -> 189;
var_index(extract_next_element2, 3) -> 190;
var_index(l_loop_rec, 0) -> 191;
var_index(l_call_ext, 1) -> 192;
var_index(l_select_tuple_arity2, 1) -> 193;
var_index(l_move_call, 2) -> 194;
var_index(l_move_call, 3) -> 195;
var_index(l_move_call_ext, 2) -> 196;
var_index(l_bs_get_utf16, 0) -> 197;
var_index(l_select_val_atoms, 1) -> 198;
var_index(l_trim, 3) -> 199;
var_index(init, 5) -> 200;
var_index(l_bs_restore2, 0) -> 201;
var_index(catch_end, 1) -> 202;
var_index(is_nil, 3) -> 203;
var_index(l_move_call, 4) -> 204;
var_index(l_bs_init_heap_bin, 0) -> 205;
var_index(is_nil, 4) -> 206;
var_index(is_nonempty_list, 8) -> 207;
var_index(wait, 0) -> 208;
var_index(l_call_ext_last, 0) -> 209;
var_index(l_allocate_zero, 4) -> 210;
var_index(loop_rec_end, 0) -> 211;
var_index(call_bif, 10) -> 212;
var_index(deallocate_return, 5) -> 213;
var_index(move, 8) -> 214;
var_index(l_fetch, 6) -> 215;
var_index(is_nonempty_list, 9) -> 216;
var_index(extract_next_element, 5) -> 217;
var_index(l_select_val2, 3) -> 218;
var_index(l_allocate_zero, 5) -> 219;
var_index(l_allocate, 5) -> 220;
var_index(init, 6) -> 221;
var_index(l_move_call_ext, 3) -> 222;
var_index(badmatch, 1) -> 223;
var_index(l_call_last, 5) -> 224;
var_index(l_move_call_ext, 4) -> 225;
var_index(l_plus, 1) -> 226;
var_index(l_bs_start_match2, 1) -> 227;
var_index(l_move_call_ext, 5) -> 228;
var_index(l_call_last, 6) -> 229;
var_index(badmatch, 2) -> 230;
var_index(l_move_call_ext, 6) -> 231;
var_index(l_call_fun_last, 0) -> 232;
var_index(l_move_call_ext, 7) -> 233;
var_index(l_trim, 4) -> 234;
var_index(l_increment, 2) -> 235;
var_index(put_list, 5) -> 236;
var_index(call_bif, 11) -> 237;
var_index(int_code_end, 0) -> 238;
var_index(l_move_call_ext_only, 0) -> 239;
var_index(l_bs_save2, 0) -> 240;
var_index(call_bif, 12) -> 241;
var_index(extract_next_element, 6) -> 242;
var_index(bs_context_to_binary, 0) -> 243;
var_index(move_deallocate_return, 4) -> 244;
var_index(l_is_ne_exact_immed, 1) -> 245;
var_index(extract_next_element2, 4) -> 246;
var_index(l_select_val2, 4) -> 247;
var_index(l_gc_bif1, 1) -> 248;
var_index(l_increment, 3) -> 249;
var_index(self, 0) -> 250;
var_index(l_put_tuple, 4) -> 251;
var_index(extract_next_element, 7) -> 252;
var_index(get_tuple_element, 6) -> 253;
var_index(call_bif, 13) -> 254;
var_index(is_tuple, 1) -> 255;
var_index(l_move_call_last, 2) -> 256;
var_index(l_move_call, 5) -> 257;
var_index(l_is_eq_exact_immed, 11) -> 258;
var_index(badmatch, 3) -> 259;
var_index(l_move_call, 6) -> 260;
var_index(is_nil, 5) -> 261;
var_index(is_nonempty_list_allocate, 1) -> 262;
var_index(l_call_ext, 2) -> 263;
var_index(l_bs_test_zero_tail2, 1) -> 264;
var_index(l_catch, 2) -> 265;
var_index(l_move_call_ext, 8) -> 266;
var_index(call_bif, 14) -> 267;
var_index(is_atom, 0) -> 268;
var_index(l_bs_test_zero_tail2, 2) -> 269;
var_index(l_move_call_ext_only, 6) -> 270;
var_index(l_select_val_smallints, 1) -> 271;
var_index(l_fast_element, 0) -> 272;
var_index(is_nonempty_list, 10) -> 273;
var_index(l_bs_match_string, 1) -> 274;
var_index(call_bif, 15) -> 275;
var_index(l_call_ext, 3) -> 276;
var_index(move_return, 6) -> 277;
var_index(l_bs_test_unit_8, 0) -> 278;
var_index(l_call_ext_last, 1) -> 279;
var_index(is_nil, 6) -> 280;
var_index(allocate_heap_zero, 0) -> 281;
var_index(is_nonempty_list, 11) -> 282;
var_index(l_call_ext_last, 2) -> 283;
var_index(call_bif, 16) -> 284;
var_index(self, 1) -> 285;
var_index(extract_next_element, 8) -> 286;
var_index(init, 7) -> 287;
var_index(case_end, 2) -> 288;
var_index(extract_next_element3, 1) -> 289;
var_index(is_nil, 7) -> 290;
var_index(l_move_call, 7) -> 291;
var_index(call_bif, 17) -> 292;
var_index(raise, 0) -> 293;
var_index(catch_end, 2) -> 294;
var_index(l_is_eq_exact_literal, 1) -> 295;
var_index(l_is_eq_exact_literal, 7) -> 296;
var_index(l_move_call_ext, 9) -> 297;
var_index(l_minus, 0) -> 298;
var_index(extract_next_element2, 5) -> 299;
var_index(l_allocate, 6) -> 300;
var_index(l_bif2, 0) -> 301;
var_index(try_end, 0) -> 302;
var_index(l_call_fun, 1) -> 303;
var_index(call_bif, 18) -> 304;
var_index(deallocate_return, 6) -> 305;
var_index(l_call_ext, 4) -> 306;
var_index(l_move_call_last, 3) -> 307;
var_index(l_put_tuple, 5) -> 308;
var_index(l_move_call_ext_only, 1) -> 309;
var_index(l_band, 0) -> 310;
var_index(l_move_call, 8) -> 311;
var_index(l_trim, 5) -> 312;
var_index(extract_next_element, 9) -> 313;
var_index(call_bif, 19) -> 314;
var_index(l_call_ext, 5) -> 315;
var_index(l_call_ext, 6) -> 316;
var_index(l_bs_get_binary_all2, 0) -> 317;
var_index(get_tuple_element, 7) -> 318;
var_index(l_minus, 1) -> 319;
var_index(l_is_eq_exact_immed, 12) -> 320;
var_index(extract_next_element3, 2) -> 321;
var_index(is_integer, 0) -> 322;
var_index(test_heap_1_put_list, 1) -> 323;
var_index(extract_next_element2, 6) -> 324;
var_index(l_catch, 3) -> 325;
var_index(extract_next_element, 26) -> 326;
var_index(l_bif2, 1) -> 327;
var_index(is_nil, 8) -> 328;
var_index(l_move_call_only, 4) -> 329;
var_index(move_deallocate_return, 5) -> 330;
var_index(move_jump, 0) -> 331;
var_index(is_list, 1) -> 332;
var_index(l_move_call, 9) -> 333;
var_index(l_bsr, 0) -> 334;
var_index(l_call_ext, 7) -> 335;
var_index(l_is_eq_exact_immed, 13) -> 336;
var_index(apply, 0) -> 337;
var_index(l_fast_element, 2) -> 338;
var_index(l_fetch, 7) -> 339;
var_index(l_bs_get_integer_small_imm, 0) -> 340;
var_index(l_bif2, 2) -> 341;
var_index(l_bs_get_integer_8, 0) -> 342;
var_index(l_bs_test_zero_tail2, 3) -> 343;
var_index(set_tuple_element, 1) -> 344;
var_index(try_end, 1) -> 345;
var_index(l_is_eq_exact_literal, 2) -> 346;
var_index(l_is_eq_exact_immed, 10) -> 347;
var_index(extract_next_element2, 7) -> 348;
var_index(l_is_eq_exact_literal, 3) -> 349;
var_index(l_select_tuple_arity, 0) -> 350;
var_index(l_allocate_zero, 6) -> 351;
var_index(l_fmul, 0) -> 352;
var_index(l_call_ext, 8) -> 353;
var_index(l_bsl, 0) -> 354;
var_index(is_nonempty_list, 12) -> 355;
var_index(move, 9) -> 356;
var_index(call_bif, 20) -> 357;
var_index(l_move_call_ext, 10) -> 358;
var_index(l_times, 0) -> 359;
var_index(l_move_call_ext_only, 2) -> 360;
var_index(l_put_tuple, 6) -> 361;
var_index(l_bs_init_fail, 0) -> 362;
var_index(l_is_eq_exact_immed, 14) -> 363;
var_index(l_times, 1) -> 364;
var_index(l_is_eq_exact_immed, 15) -> 365;
var_index(l_move_call_ext, 11) -> 366;
var_index(l_call_ext, 9) -> 367;
var_index(l_move_call_ext, 12) -> 368;
var_index(l_fetch, 8) -> 369;
var_index(extract_next_element, 10) -> 370;
var_index(is_nil, 9) -> 371;
var_index(call_bif, 21) -> 372;
var_index(node, 0) -> 373;
var_index(l_call_last, 7) -> 374;
var_index(l_bs_get_binary2, 0) -> 375;
var_index(is_tuple, 2) -> 376;
var_index(l_call_fun, 2) -> 377;
var_index(test_arity, 2) -> 378;
var_index(get_list, 4) -> 379;
var_index(l_bs_get_integer_8, 1) -> 380;
var_index(l_bs_test_zero_tail2, 5) -> 381;
var_index(catch_end, 3) -> 382;
var_index(l_bif2, 3) -> 383;
var_index(call_bif, 22) -> 384;
var_index(l_is_ne_exact_immed, 2) -> 385;
var_index(l_is_ne_exact, 0) -> 386;
var_index(l_bif2, 4) -> 387;
var_index(is_binary, 0) -> 388;
var_index(l_allocate_zero, 9) -> 389;
var_index(l_is_eq_exact_immed, 16) -> 390;
var_index(extract_next_element, 11) -> 391;
var_index(l_bs_get_integer_32, 0) -> 392;
var_index(l_call_ext, 10) -> 393;
var_index(is_atom, 1) -> 394;
var_index(l_select_val2, 7) -> 395;
var_index(l_fcheckerror, 0) -> 396;
var_index(fclearerror, 0) -> 397;
var_index(l_fetch, 9) -> 398;
var_index(l_new_bs_put_binary_all, 0) -> 399;
var_index(extract_next_element3, 3) -> 400;
var_index(node, 1) -> 401;
var_index(extract_next_element, 12) -> 402;
var_index(l_move_call, 10) -> 403;
var_index(get_list, 5) -> 404;
var_index(move_deallocate_return, 6) -> 405;
var_index(l_move_call_last, 4) -> 406;
var_index(l_new_bs_put_binary_all, 1) -> 407;
var_index(l_is_eq_exact_immed, 17) -> 408;
var_index(timeout, 0) -> 409;
var_index(deallocate_return, 7) -> 410;
var_index(l_get, 0) -> 411;
var_index(l_move_call, 11) -> 412;
var_index(l_fetch, 10) -> 413;
var_index(l_move_call_ext_last, 1) -> 414;
var_index(is_nil, 10) -> 415;
var_index(l_fetch, 11) -> 416;
var_index(call_bif, 23) -> 417;
var_index(l_call_ext, 11) -> 418;
var_index(is_float, 1) -> 419;
var_index(l_select_val2, 5) -> 420;
var_index(l_select_val2, 8) -> 421;
var_index(l_is_eq_exact_immed, 36) -> 422;
var_index(call_bif, 24) -> 423;
var_index(l_band, 1) -> 424;
var_index(l_get, 1) -> 425;
var_index(is_binary, 1) -> 426;
var_index(l_fast_element, 1) -> 427;
var_index(l_get, 2) -> 428;
var_index(is_nonempty_list, 39) -> 429;
var_index(l_move_call_ext, 14) -> 430;
var_index(extract_next_element, 13) -> 431;
var_index(is_nonempty_list, 13) -> 432;
var_index(l_move_call, 12) -> 433;
var_index(l_call_ext, 12) -> 434;
var_index(l_is_eq_exact_immed, 18) -> 435;
var_index(l_move_call, 13) -> 436;
var_index(l_move_call_ext, 15) -> 437;
var_index(l_rem, 0) -> 438;
var_index(move2, 8) -> 439;
var_index(l_bsl, 1) -> 440;
var_index(l_call_ext, 13) -> 441;
var_index(l_allocate_zero, 7) -> 442;
var_index(l_call_last, 8) -> 443;
var_index(is_nil, 11) -> 444;
var_index(l_gc_bif1, 2) -> 445;
var_index(l_is_ne_exact_immed, 10) -> 446;
var_index(extract_next_element3, 4) -> 447;
var_index(l_increment, 4) -> 448;
var_index(is_nonempty_list, 14) -> 449;
var_index(is_nil, 30) -> 450;
var_index(extract_next_element3, 10) -> 451;
var_index(l_bs_append, 0) -> 452;
var_index(call_bif, 25) -> 453;
var_index(l_trim, 6) -> 454;
var_index(is_nil, 12) -> 455;
var_index(l_move_call_ext, 16) -> 456;
var_index(is_integer, 5) -> 457;
var_index(l_call_ext, 14) -> 458;
var_index(l_bor, 0) -> 459;
var_index(move_return, 7) -> 460;
var_index(is_list, 2) -> 461;
var_index(l_select_val2, 15) -> 462;
var_index(l_call_ext, 15) -> 463;
var_index(is_nil, 13) -> 464;
var_index(l_catch, 4) -> 465;
var_index(l_element, 1) -> 466;
var_index(extract_next_element2, 17) -> 467;
var_index(l_gc_bif1, 5) -> 468;
var_index(l_fadd, 0) -> 469;
var_index(call_bif, 26) -> 470;
var_index(l_allocate, 7) -> 471;
var_index(l_move_call_only, 5) -> 472;
var_index(is_nonempty_list, 15) -> 473;
var_index(l_move_call_ext, 17) -> 474;
var_index(l_move_call_ext, 18) -> 475;
var_index(call_bif, 28) -> 476;
var_index(call_bif, 27) -> 477;
var_index(is_integer, 1) -> 478;
var_index(bif1_body, 1) -> 479;
var_index(l_is_ne_exact_immed, 3) -> 480;
var_index(l_call_ext, 16) -> 481;
var_index(l_call_ext_last, 3) -> 482;
var_index(l_fetch, 12) -> 483;
var_index(l_move_call_ext, 19) -> 484;
var_index(l_is_eq_exact_immed, 19) -> 485;
var_index(is_nonempty_list, 16) -> 486;
var_index(bif1_body, 2) -> 487;
var_index(fmove_1, 0) -> 488;
var_index(l_move_call_ext, 20) -> 489;
var_index(call_bif, 29) -> 490;
var_index(move_jump, 1) -> 491;
var_index(l_bs_get_utf8, 0) -> 492;
var_index(l_move_call_ext, 21) -> 493;
var_index(l_bs_skip_bits_all2, 0) -> 494;
var_index(bif2_body, 1) -> 495;
var_index(l_call_ext, 17) -> 496;
var_index(call_bif, 30) -> 497;
var_index(fconv, 0) -> 498;
var_index(l_is_eq_exact_literal, 4) -> 499;
var_index(l_is_eq_exact_immed, 20) -> 500;
var_index(call_bif, 31) -> 501;
var_index(extract_next_element3, 5) -> 502;
var_index(extract_next_element2, 8) -> 503;
var_index(system_limit, 0) -> 504;
var_index(l_move_call_ext_last, 2) -> 505;
var_index(l_fetch, 23) -> 506;
var_index(call_bif, 32) -> 507;
var_index(call_bif, 4) -> 508;
var_index(apply_last, 0) -> 509;
var_index(is_atom, 2) -> 510;
var_index(l_move_call_ext_only, 3) -> 511;
var_index(l_bor, 1) -> 512;
var_index(l_is_eq_exact_immed, 22) -> 513;
var_index(put_list, 6) -> 514;
var_index(call_bif, 33) -> 515;
var_index(extract_next_element, 14) -> 516;
var_index(l_catch, 5) -> 517;
var_index(init, 8) -> 518;
var_index(put_list, 8) -> 519;
var_index(l_make_export, 0) -> 520;
var_index(l_select_val2, 6) -> 521;
var_index(move_deallocate_return, 7) -> 522;
var_index(l_call_fun, 3) -> 523;
var_index(call_bif, 34) -> 524;
var_index(badmatch, 4) -> 525;
var_index(is_atom, 3) -> 526;
var_index(l_move_call, 14) -> 527;
var_index(l_bs_test_zero_tail2, 4) -> 528;
var_index(fmove_1, 1) -> 529;
var_index(fmove_2, 0) -> 530;
var_index(l_new_bs_put_integer, 0) -> 531;
var_index(fmove_2, 1) -> 532;
var_index(call_bif, 35) -> 533;
var_index(is_integer_allocate, 0) -> 534;
var_index(l_call_ext_last, 4) -> 535;
var_index(l_move_call_ext, 22) -> 536;
var_index(l_call_ext, 18) -> 537;
var_index(l_fast_element, 3) -> 538;
var_index(l_move_call, 15) -> 539;
var_index(l_move_call_ext, 23) -> 540;
var_index(l_fetch, 13) -> 541;
var_index(l_trim, 7) -> 542;
var_index(l_bs_restore2, 1) -> 543;
var_index(deallocate_return, 8) -> 544;
var_index(is_list, 6) -> 545;
var_index(l_call_ext, 19) -> 546;
var_index(extract_next_element, 15) -> 547;
var_index(l_fetch, 14) -> 548;
var_index(l_is_eq_exact_immed, 23) -> 549;
var_index(is_tuple, 3) -> 550;
var_index(is_nonempty_list, 17) -> 551;
var_index(l_move_call_ext, 13) -> 552;
var_index(l_fetch, 15) -> 553;
var_index(l_move_call_last, 5) -> 554;
var_index(extract_next_element, 16) -> 555;
var_index(extract_next_element2, 9) -> 556;
var_index(try_end, 2) -> 557;
var_index(if_end, 0) -> 558;
var_index(l_get, 3) -> 559;
var_index(call_bif, 36) -> 560;
var_index(case_end, 11) -> 561;
var_index(l_move_call, 16) -> 562;
var_index(call_bif, 38) -> 563;
var_index(call_bif, 37) -> 564;
var_index(move_jump, 2) -> 565;
var_index(try_end, 4) -> 566;
var_index(try_end, 3) -> 567;
var_index(l_is_ne_exact_immed, 4) -> 568;
var_index(call_bif, 39) -> 569;
var_index(self, 2) -> 570;
var_index(l_call_ext, 23) -> 571;
var_index(l_call_ext, 22) -> 572;
var_index(l_call_ext, 21) -> 573;
var_index(l_call_ext, 20) -> 574;
var_index(move_return, 8) -> 575;
var_index(is_list, 3) -> 576;
var_index(is_nonempty_list, 18) -> 577;
var_index(try_end, 5) -> 578;
var_index(catch_end, 4) -> 579;
var_index(l_move_call_ext_last, 3) -> 580;
var_index(l_call_ext, 25) -> 581;
var_index(l_call_ext, 24) -> 582;
var_index(extract_next_element2, 10) -> 583;
var_index(is_nonempty_list, 19) -> 584;
var_index(init, 9) -> 585;
var_index(l_call_ext, 26) -> 586;
var_index(case_end, 3) -> 587;
var_index(bif1_body, 3) -> 588;
var_index(get_tuple_element, 8) -> 589;
var_index(test_arity, 3) -> 590;
var_index(l_bs_skip_bits_imm2, 0) -> 591;
var_index(l_bif1, 0) -> 592;
var_index(l_bs_get_utf16, 1) -> 593;
var_index(is_tuple, 4) -> 594;
var_index(l_call_ext, 28) -> 595;
var_index(l_call_ext, 27) -> 596;
var_index(extract_next_element, 17) -> 597;
var_index(l_bs_init_bits_fail, 0) -> 598;
var_index(l_select_val_atoms, 2) -> 599;
var_index(l_is_eq_exact_immed, 24) -> 600;
var_index(l_select_val2, 9) -> 601;
var_index(l_call_ext, 29) -> 602;
var_index(is_integer, 2) -> 603;
var_index(badmatch, 5) -> 604;
var_index(try_case_end, 0) -> 605;
var_index(l_fdiv, 0) -> 606;
var_index(is_tuple, 10) -> 607;
var_index(l_get, 6) -> 608;
var_index(l_call_ext_last, 6) -> 609;
var_index(l_bif1, 1) -> 610;
var_index(l_move_call_ext, 24) -> 611;
var_index(l_move_call_ext_last, 5) -> 612;
var_index(get_list, 6) -> 613;
var_index(case_end, 4) -> 614;
var_index(put_list, 9) -> 615;
var_index(l_call_ext, 30) -> 616;
var_index(l_bs_test_unit_8, 1) -> 617;
var_index(l_gc_bif1, 3) -> 618;
var_index(self, 5) -> 619;
var_index(l_increment, 9) -> 620;
var_index(call_bif, 41) -> 621;
var_index(call_bif, 40) -> 622;
var_index(extract_next_element, 18) -> 623;
var_index(extract_next_element2, 11) -> 624;
var_index(move_return, 9) -> 625;
var_index(is_nil, 14) -> 626;
var_index(l_move_call_only, 6) -> 627;
var_index(l_bs_restore2, 2) -> 628;
var_index(l_move_call, 18) -> 629;
var_index(l_move_call, 17) -> 630;
var_index(l_is_eq_exact_immed, 25) -> 631;
var_index(bif1_body, 5) -> 632;
var_index(move, 10) -> 633;
var_index(is_integer, 3) -> 634;
var_index(l_allocate_zero, 8) -> 635;
var_index(is_nil, 15) -> 636;
var_index(is_list, 4) -> 637;
var_index(case_end, 5) -> 638;
var_index(is_atom, 6) -> 639;
var_index(l_move_call_ext, 26) -> 640;
var_index(call_bif, 42) -> 641;
var_index(get_tuple_element, 9) -> 642;
var_index(l_move_call_ext, 27) -> 643;
var_index(l_is_eq_exact_immed, 26) -> 644;
var_index(l_increment, 5) -> 645;
var_index(l_fsub, 0) -> 646;
var_index(l_increment, 7) -> 647;
var_index(put_list, 7) -> 648;
var_index(l_move_call_ext, 28) -> 649;
var_index(call_bif, 43) -> 650;
var_index(l_call_ext, 31) -> 651;
var_index(extract_next_element3, 6) -> 652;
var_index(l_bsr, 1) -> 653;
var_index(badmatch, 6) -> 654;
var_index(l_move_call_ext, 30) -> 655;
var_index(l_move_call_ext, 29) -> 656;
var_index(l_call_ext, 33) -> 657;
var_index(l_call_ext, 32) -> 658;
var_index(extract_next_element2, 12) -> 659;
var_index(is_integer, 4) -> 660;
var_index(is_nonempty_list, 20) -> 661;
var_index(l_move_call, 19) -> 662;
var_index(l_is_eq_exact_literal, 5) -> 663;
var_index(l_call_ext_only, 0) -> 664;
var_index(l_plus, 2) -> 665;
var_index(move_deallocate_return, 8) -> 666;
var_index(move_return, 10) -> 667;
var_index(l_is_eq_exact_literal, 6) -> 668;
var_index(l_int_div, 0) -> 669;
var_index(l_rem, 1) -> 670;
var_index(l_bs_get_binary_imm2, 0) -> 671;
var_index(l_move_call_ext, 31) -> 672;
var_index(l_call_ext, 34) -> 673;
var_index(l_move_call, 20) -> 674;
var_index(init, 10) -> 675;
var_index(catch_end, 5) -> 676;
var_index(node, 4) -> 677;
var_index(put_list, 10) -> 678;
var_index(l_allocate, 9) -> 679;
var_index(l_bif2, 5) -> 680;
var_index(is_atom, 4) -> 681;
var_index(l_move_call, 21) -> 682;
var_index(l_fetch, 16) -> 683;
var_index(l_increment, 6) -> 684;
var_index(bif1_body, 6) -> 685;
var_index(l_fetch, 17) -> 686;
var_index(test_heap_1_put_list, 2) -> 687;
var_index(call_bif, 44) -> 688;
var_index(node, 2) -> 689;
var_index(l_call_ext, 37) -> 690;
var_index(l_call_ext, 36) -> 691;
var_index(l_call_ext, 35) -> 692;
var_index(l_call_last, 9) -> 693;
var_index(is_nil, 16) -> 694;
var_index(l_bs_put_string, 0) -> 695;
var_index(is_list, 5) -> 696;
var_index(case_end, 6) -> 697;
var_index(l_yield, 0) -> 698;
var_index(l_move_call, 22) -> 699;
var_index(l_catch, 6) -> 700;
var_index(get_list, 7) -> 701;
var_index(l_bs_get_binary_all2, 1) -> 702;
var_index(l_is_eq_exact_immed, 27) -> 703;
var_index(l_call_ext, 38) -> 704;
var_index(case_end, 7) -> 705;
var_index(is_nonempty_list, 21) -> 706;
var_index(l_move_call_ext, 33) -> 707;
var_index(l_select_tuple_arity, 3) -> 708;
var_index(l_move_call_ext, 34) -> 709;
var_index(l_bs_append, 1) -> 710;
var_index(l_bs_get_integer, 0) -> 711;
var_index(l_move_call_last, 6) -> 712;
var_index(call_bif, 46) -> 713;
var_index(call_bif, 45) -> 714;
var_index(l_call_ext, 39) -> 715;
var_index(l_is_eq_exact_immed, 21) -> 716;
var_index(is_nonempty_list, 22) -> 717;
var_index(is_function, 1) -> 718;
var_index(l_gc_bif1, 4) -> 719;
var_index(l_is_ne_exact_immed, 5) -> 720;
var_index(l_jump_on_val, 0) -> 721;
var_index(call_bif, 47) -> 722;
var_index(l_call_ext, 40) -> 723;
var_index(l_is_ne_exact_immed, 6) -> 724;
var_index(extract_next_element2, 13) -> 725;
var_index(move_jump, 3) -> 726;
var_index(badmatch, 7) -> 727;
var_index(l_bs_test_unit_8, 2) -> 728;
var_index(l_move_call, 24) -> 729;
var_index(l_move_call, 23) -> 730;
var_index(l_is_eq_exact_immed, 28) -> 731;
var_index(l_call_ext, 41) -> 732;
var_index(is_nil, 18) -> 733;
var_index(is_nil, 17) -> 734;
var_index(is_nonempty_list, 23) -> 735;
var_index(l_move_call, 27) -> 736;
var_index(l_move_call, 26) -> 737;
var_index(l_move_call, 25) -> 738;
var_index(l_select_tuple_arity, 1) -> 739;
var_index(l_fetch, 18) -> 740;
var_index(l_move_call_ext, 35) -> 741;
var_index(l_bs_get_integer_32, 1) -> 742;
var_index(fmove_1, 3) -> 743;
var_index(l_int_div, 1) -> 744;
var_index(l_is_eq_exact_immed, 29) -> 745;
var_index(l_move_call_ext_only, 4) -> 746;
var_index(l_move_call_ext, 36) -> 747;
var_index(call_bif, 1) -> 748;
var_index(l_call_ext, 48) -> 749;
var_index(l_call_ext, 47) -> 750;
var_index(l_call_ext, 46) -> 751;
var_index(l_call_ext, 45) -> 752;
var_index(l_call_ext, 44) -> 753;
var_index(l_call_ext, 43) -> 754;
var_index(l_call_ext, 42) -> 755;
var_index(extract_next_element3, 7) -> 756;
var_index(move_return, 11) -> 757;
var_index(l_bs_test_unit_8, 3) -> 758;
var_index(l_bs_save2, 1) -> 759;
var_index(bif2_body, 2) -> 760;
var_index(is_binary, 3) -> 761;
var_index(l_move_call_ext, 37) -> 762;
var_index(bif1_body, 7) -> 763;
var_index(l_allocate, 8) -> 764;
var_index(l_call_ext, 50) -> 765;
var_index(l_call_ext, 49) -> 766;
var_index(move_return, 12) -> 767;
var_index(l_trim, 8) -> 768;
var_index(is_nil, 19) -> 769;
var_index(case_end, 8) -> 770;
var_index(fconv, 1) -> 771;
var_index(deallocate_return, 12) -> 772;
var_index(l_catch, 8) -> 773;
var_index(l_move_call_ext_last, 4) -> 774;
var_index(l_gc_bif1, 6) -> 775;
var_index(move_deallocate_return, 10) -> 776;
var_index(l_move_call_ext, 38) -> 777;
var_index(l_call_ext, 53) -> 778;
var_index(l_call_ext, 52) -> 779;
var_index(l_call_ext, 51) -> 780;
var_index(extract_next_element, 19) -> 781;
var_index(l_call_fun, 4) -> 782;
var_index(l_move_call_ext_only, 5) -> 783;
var_index(l_move_call_ext, 40) -> 784;
var_index(l_move_call_ext, 39) -> 785;
var_index(l_bs_put_string, 2) -> 786;
var_index(l_is_ne_exact_literal, 0) -> 787;
var_index(l_call_ext, 54) -> 788;
var_index(move_return, 13) -> 789;
var_index(badmatch, 8) -> 790;
var_index(l_move_call_ext, 42) -> 791;
var_index(l_move_call_ext, 41) -> 792;
var_index(l_bs_test_unit_8, 4) -> 793;
var_index(is_pid, 1) -> 794;
var_index(is_boolean, 0) -> 795;
var_index(l_get, 4) -> 796;
var_index(bif1_body, 4) -> 797;
var_index(l_fetch, 19) -> 798;
var_index(bs_context_to_binary, 5) -> 799;
var_index(l_select_tuple_arity2, 2) -> 800;
var_index(l_call_ext_last, 5) -> 801;
var_index(l_select_val2, 10) -> 802;
var_index(l_call_ext, 58) -> 803;
var_index(l_call_ext, 57) -> 804;
var_index(l_call_ext, 56) -> 805;
var_index(l_call_ext, 55) -> 806;
var_index(extract_next_element2, 14) -> 807;
var_index(move_jump, 4) -> 808;
var_index(l_move_call_only, 7) -> 809;
var_index(l_bs_skip_bits2, 0) -> 810;
var_index(put_list, 12) -> 811;
var_index(l_bs_get_binary2, 1) -> 812;
var_index(l_call_ext, 65) -> 813;
var_index(l_call_ext, 64) -> 814;
var_index(l_call_ext, 63) -> 815;
var_index(l_call_ext, 62) -> 816;
var_index(l_call_ext, 61) -> 817;
var_index(l_call_ext, 60) -> 818;
var_index(l_call_ext, 59) -> 819;
var_index(extract_next_element, 20) -> 820;
var_index(extract_next_element2, 15) -> 821;
var_index(move_return, 14) -> 822;
var_index(bs_context_to_binary, 1) -> 823;
var_index(l_move_call, 28) -> 824;
var_index(init, 11) -> 825;
var_index(l_bs_skip_bits2, 1) -> 826;
var_index(l_move_call_last, 7) -> 827;
var_index(l_call_ext, 68) -> 828;
var_index(l_call_ext, 67) -> 829;
var_index(l_call_ext, 66) -> 830;
var_index(move_jump, 6) -> 831;
var_index(move_jump, 5) -> 832;
var_index(move_return, 15) -> 833;
var_index(is_nonempty_list_test_heap, 0) -> 834;
var_index(l_move_call_ext, 44) -> 835;
var_index(catch_end, 6) -> 836;
var_index(l_get, 5) -> 837;
var_index(l_bs_skip_bits_all2, 1) -> 838;
var_index(l_bs_start_match2, 2) -> 839;
var_index(l_move_call_ext, 46) -> 840;
var_index(l_wait_timeout, 2) -> 841;
var_index(l_element, 0) -> 842;
var_index(l_move_call_ext, 47) -> 843;
var_index(l_move_call_ext, 48) -> 844;
var_index(l_call_ext, 70) -> 845;
var_index(l_call_ext, 69) -> 846;
var_index(l_move_call_only, 8) -> 847;
var_index(case_end, 9) -> 848;
var_index(is_pid, 0) -> 849;
var_index(l_move_call, 29) -> 850;
var_index(l_select_tuple_arity, 2) -> 851;
var_index(is_nonempty_list_test_heap, 1) -> 852;
var_index(catch_end, 8) -> 853;
var_index(node, 3) -> 854;
var_index(bs_init_writable, 0) -> 855;
var_index(l_call_ext, 73) -> 856;
var_index(l_call_ext, 72) -> 857;
var_index(l_call_ext, 71) -> 858;
var_index(extract_next_element, 21) -> 859;
var_index(move_jump, 7) -> 860;
var_index(move_return, 17) -> 861;
var_index(move_return, 16) -> 862;
var_index(l_new_bs_put_integer_imm, 1) -> 863;
var_index(bs_context_to_binary, 2) -> 864;
var_index(is_nonempty_list, 25) -> 865;
var_index(is_nonempty_list, 24) -> 866;
var_index(try_end, 6) -> 867;
var_index(l_bs_private_append, 0) -> 868;
var_index(deallocate_return, 9) -> 869;
var_index(l_new_bs_put_float_imm, 0) -> 870;
var_index(l_move_call, 30) -> 871;
var_index(l_call_ext_only, 1) -> 872;
var_index(l_apply, 0) -> 873;
var_index(l_move_call_ext, 25) -> 874;
var_index(fmove_1, 2) -> 875;
var_index(put_list, 11) -> 876;
var_index(l_bif2, 6) -> 877;
var_index(bif2_body, 3) -> 878;
var_index(self, 3) -> 879;
var_index(is_tuple, 5) -> 880;
var_index(l_call_ext, 76) -> 881;
var_index(l_call_ext, 75) -> 882;
var_index(l_call_ext, 74) -> 883;
var_index(extract_next_element3, 8) -> 884;
var_index(l_new_bs_put_float_imm, 1) -> 885;
var_index(l_move_call, 33) -> 886;
var_index(l_move_call, 32) -> 887;
var_index(l_move_call, 31) -> 888;
var_index(l_catch, 7) -> 889;
var_index(catch_end, 7) -> 890;
var_index(l_is_ne_exact_immed, 7) -> 891;
var_index(test_heap_1_put_list, 3) -> 892;
var_index(l_call_ext, 77) -> 893;
var_index(extract_next_element3, 9) -> 894;
var_index(move_jump, 8) -> 895;
var_index(is_nonempty_list, 26) -> 896;
var_index(l_move_call, 34) -> 897;
var_index(is_binary, 2) -> 898;
var_index(l_fetch, 21) -> 899;
var_index(l_fetch, 20) -> 900;
var_index(l_bs_skip_bits_all2, 2) -> 901;
var_index(l_is_ne_exact_immed, 8) -> 902;
var_index(l_bs_get_utf8, 1) -> 903;
var_index(l_bs_get_integer_imm, 0) -> 904;
var_index(l_select_val2, 11) -> 905;
var_index(self, 4) -> 906;
var_index(l_call_ext, 79) -> 907;
var_index(l_call_ext, 78) -> 908;
var_index(l_call_last, 10) -> 909;
var_index(move_return, 18) -> 910;
var_index(is_nil, 20) -> 911;
var_index(recv_mark, 0) -> 912;
var_index(badmatch, 10) -> 913;
var_index(badmatch, 9) -> 914;
var_index(is_function, 0) -> 915;
var_index(l_recv_set, 0) -> 916;
var_index(l_new_bs_put_integer, 1) -> 917;
var_index(badmatch, 17) -> 918;
var_index(move2, 9) -> 919;
var_index(l_call_ext, 88) -> 920;
var_index(l_call_ext, 87) -> 921;
var_index(l_call_ext, 86) -> 922;
var_index(l_call_ext, 85) -> 923;
var_index(l_call_ext, 84) -> 924;
var_index(l_call_ext, 83) -> 925;
var_index(l_call_ext, 82) -> 926;
var_index(l_call_ext, 81) -> 927;
var_index(l_call_ext, 80) -> 928;
var_index(extract_next_element2, 16) -> 929;
var_index(move_jump, 9) -> 930;
var_index(is_nil, 21) -> 931;
var_index(l_bs_put_string, 1) -> 932;
var_index(l_select_val2, 12) -> 933;
var_index(is_atom, 5) -> 934;
var_index(l_bs_init_bits_fail, 1) -> 935;
var_index(l_bs_get_integer_16, 0) -> 936;
var_index(l_move_call_ext, 32) -> 937;
var_index(is_float, 0) -> 938;
var_index(l_is_eq_exact_immed, 30) -> 939;
var_index(l_bs_get_float2, 0) -> 940;
var_index(l_trim, 11) -> 941;
var_index(l_call_ext, 91) -> 942;
var_index(l_call_ext, 90) -> 943;
var_index(l_call_ext, 89) -> 944;
var_index(l_is_ne_exact_immed, 9) -> 945;
var_index(l_fast_element, 4) -> 946;
var_index(move_return, 20) -> 947;
var_index(move_return, 19) -> 948;
var_index(bs_context_to_binary, 3) -> 949;
var_index(is_nonempty_list, 28) -> 950;
var_index(is_nonempty_list, 27) -> 951;
var_index(deallocate_return, 11) -> 952;
var_index(deallocate_return, 10) -> 953;
var_index(l_bs_init_bits, 0) -> 954;
var_index(l_increment, 8) -> 955;
var_index(l_bif1, 2) -> 956;
var_index(l_minus, 2) -> 957;
var_index(is_tuple, 6) -> 958;
var_index(l_call_ext, 98) -> 959;
var_index(l_call_ext, 97) -> 960;
var_index(l_call_ext, 96) -> 961;
var_index(l_call_ext, 95) -> 962;
var_index(l_call_ext, 94) -> 963;
var_index(l_call_ext, 93) -> 964;
var_index(l_call_ext, 92) -> 965;
var_index(l_call_last, 11) -> 966;
var_index(l_is_eq_exact_immed, 31) -> 967;
var_index(extract_next_element, 22) -> 968;
var_index(move_return, 21) -> 969;
var_index(badmatch, 11) -> 970;
var_index(case_end, 10) -> 971;
var_index(get_list, 10) -> 972;
var_index(get_list, 8) -> 973;
var_index(l_select_val2, 13) -> 974;
var_index(l_call_ext_only, 2) -> 975;
var_index(l_fetch, 22) -> 976;
var_index(l_call_ext, 104) -> 977;
var_index(l_call_ext, 103) -> 978;
var_index(l_call_ext, 102) -> 979;
var_index(l_call_ext, 101) -> 980;
var_index(l_call_ext, 100) -> 981;
var_index(l_call_ext, 99) -> 982;
var_index(l_is_eq_exact_immed, 32) -> 983;
var_index(get_tuple_element, 10) -> 984;
var_index(wait_timeout, 0) -> 985;
var_index(move_return, 22) -> 986;
var_index(badmatch, 12) -> 987;
var_index(is_nonempty_list, 29) -> 988;
var_index(l_times, 2) -> 989;
var_index(l_select_val2, 14) -> 990;
var_index(l_apply_fun, 0) -> 991;
var_index(init, 17) -> 992;
var_index(l_gc_bif1, 7) -> 993;
var_index(move, 12) -> 994;
var_index(l_is_eq_exact_immed, 33) -> 995;
var_index(l_bs_get_integer_32, 2) -> 996;
var_index(move_return, 24) -> 997;
var_index(move_return, 23) -> 998;
var_index(is_nil, 22) -> 999;
var_index(badmatch, 13) -> 1000;
var_index(move_deallocate_return, 9) -> 1001;
var_index(l_new_bs_put_float_imm, 2) -> 1002;
var_index(l_call_ext_only, 3) -> 1003;
var_index(is_integer_allocate, 1) -> 1004;
var_index(is_tuple, 8) -> 1005;
var_index(is_tuple, 7) -> 1006;
var_index(l_is_eq_exact_immed, 34) -> 1007;
var_index(extract_next_element, 23) -> 1008;
var_index(move_jump, 10) -> 1009;
var_index(is_nil, 23) -> 1010;
var_index(bs_context_to_binary, 4) -> 1011;
var_index(badmatch, 14) -> 1012;
var_index(is_nonempty_list, 32) -> 1013;
var_index(is_nonempty_list, 31) -> 1014;
var_index(is_nonempty_list, 30) -> 1015;
var_index(get_list, 9) -> 1016;
var_index(l_bs_init_fail, 1) -> 1017;
var_index(l_move_call_ext, 43) -> 1018;
var_index(l_fast_element, 5) -> 1019;
var_index(l_bs_test_tail_imm2, 0) -> 1020;
var_index(move_return, 25) -> 1021;
var_index(is_nil, 24) -> 1022;
var_index(l_move_call_only, 9) -> 1023;
var_index(fconv, 2) -> 1024;
var_index(is_nonempty_list, 34) -> 1025;
var_index(is_nonempty_list, 33) -> 1026;
var_index(l_move_call_ext, 45) -> 1027;
var_index(init, 12) -> 1028;
var_index(test_heap_1_put_list, 4) -> 1029;
var_index(l_bs_add, 1) -> 1030;
var_index(l_wait_timeout, 0) -> 1031;
var_index(l_gc_bif2, 0) -> 1032;
var_index(try_end, 7) -> 1033;
var_index(l_bs_put_utf16, 0) -> 1034;
var_index(is_tuple, 9) -> 1035;
var_index(l_is_eq_exact_immed, 35) -> 1036;
var_index(extract_next_element, 25) -> 1037;
var_index(extract_next_element, 24) -> 1038;
var_index(move_jump, 11) -> 1039;
var_index(move_return, 27) -> 1040;
var_index(move_return, 26) -> 1041;
var_index(l_trim, 9) -> 1042;
var_index(is_nil, 25) -> 1043;
var_index(l_bs_validate_unicode, 0) -> 1044;
var_index(is_nonempty_list, 35) -> 1045;
var_index(l_jump_on_val, 1) -> 1046;
var_index(move, 11) -> 1047;
var_index(l_bs_utf16_size, 0) -> 1048;
var_index(l_bs_get_binary2, 2) -> 1049;
var_index(move_return, 29) -> 1050;
var_index(move_return, 28) -> 1051;
var_index(is_nil, 26) -> 1052;
var_index(raise, 1) -> 1053;
var_index(l_bs_restore2, 3) -> 1054;
var_index(l_int_bnot, 0) -> 1055;
var_index(l_bs_get_binary_imm2, 1) -> 1056;
var_index(is_bitstr, 0) -> 1057;
var_index(move_return, 35) -> 1058;
var_index(move_return, 34) -> 1059;
var_index(move_return, 33) -> 1060;
var_index(move_return, 32) -> 1061;
var_index(move_return, 31) -> 1062;
var_index(move_return, 30) -> 1063;
var_index(is_nil, 29) -> 1064;
var_index(is_nil, 28) -> 1065;
var_index(is_nil, 27) -> 1066;
var_index(l_new_bs_put_binary_all, 2) -> 1067;
var_index(l_new_bs_put_binary, 0) -> 1068;
var_index(badmatch, 16) -> 1069;
var_index(badmatch, 15) -> 1070;
var_index(is_nonempty_list, 37) -> 1071;
var_index(is_nonempty_list, 36) -> 1072;
var_index(l_bs_save2, 2) -> 1073;
var_index(fmove_2, 2) -> 1074;
var_index(is_reference, 0) -> 1075;
var_index(is_port, 0) -> 1076;
var_index(is_nonempty_list, 38) -> 1077;
var_index(is_number, 0) -> 1078;
var_index(l_select_tuple_arity, 4) -> 1079;
var_index(move, 13) -> 1080;
var_index(l_bs_get_binary_all_reuse, 1) -> 1081;
var_index(init, 13) -> 1082;
var_index(l_wait_timeout, 1) -> 1083;
var_index(l_trim, 10) -> 1084;
var_index(l_fnegate, 0) -> 1085;
var_index(l_bs_init, 0) -> 1086;
var_index(l_jump_on_val, 2) -> 1087;
var_index(init, 14) -> 1088;
var_index(l_bs_put_utf8, 0) -> 1089;
var_index(l_bs_utf8_size, 0) -> 1090;
var_index(l_bxor, 0) -> 1091;
var_index(l_new_bs_put_float, 0) -> 1092;
var_index(l_bs_validate_unicode_retract, 0) -> 1093;
var_index(l_apply_last, 0) -> 1094;
var_index(init, 16) -> 1095;
var_index(init, 15) -> 1096;
var_index(l_bs_get_binary_imm2, 2) -> 1097;
var_index(l_is_function2, 0) -> 1098;
var_index(l_gc_bif3, 0) -> 1099;
var_index(l_bs_match_string, 2) -> 1100;
var_index(l_bs_get_integer_imm, 1) -> 1101;
var_index(l_bor, 2) -> 1102;
var_index(l_new_bs_put_binary_imm, 0) -> 1103;
var_index(l_bs_get_integer_8, 2) -> 1104;
var_index(l_bs_start_match2, 3) -> 1105;
var_index(l_rem, 2) -> 1106;
var_index(l_bs_get_integer_small_imm, 1) -> 1107;
var_index(l_bsl, 2) -> 1108;
var_index(l_apply_only, 0) -> 1109;
var_index(on_load, 0) -> 1110;
var_index(move2, 10) -> 1111;
var_index(l_int_div, 2) -> 1112;
var_index(l_bs_test_unit, 0) -> 1113;
var_index(l_select_val_atoms, 3) -> 1114;
var_index(l_m_div, 0) -> 1115;
var_index(l_hibernate, 0) -> 1116;
var_index(l_apply_fun_last, 0) -> 1117;
var_index(is_function2, 0) -> 1118;
var_index(l_apply_fun_only, 0) -> 1119;
var_index(l_band, 2) -> 1120;
var_index(is_bigint, 0) -> 1121;
var_index(test_heap, 1) -> 1122;
var_index(func_info, 0) -> 1123;
var_index(call_bif, 0) -> 1124;
var_index(l_bs_get_utf16, 2) -> 1125;
var_index(l_put_tuple, 7) -> 1126;
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
var_index(l_select_val_smallints, 2) -> 1140;
var_index(l_select_tuple_arity2, 3) -> 1141;
var_index(init2, 1) -> 1142;
var_index(l_bs_get_binary_all2, 2) -> 1143;
var_index(is_nonempty_list_test_heap, 2) -> 1144;
var_index(allocate_heap_zero, 1) -> 1145;
var_index(l_bs_init_heap_bin, 1) -> 1146;
var_index(l_plus, 3) -> 1147;
var_index(l_bs_get_integer, 1) -> 1148;

var_index(Op, No) -> erlang:error({noindex,Op,No}).

var_by_index(0) -> {move, 0};
var_by_index(1) -> {move, 1};
var_by_index(2) -> {test_heap, 0};
var_by_index(3) -> {l_call, 0};
var_by_index(4) -> {move, 2};
var_by_index(5) -> {move, 3};
var_by_index(6) -> {l_put_tuple, 0};
var_by_index(7) -> {move2, 1};
var_by_index(8) -> {get_tuple_element, 0};
var_by_index(9) -> {badmatch, 0};
var_by_index(10) -> {put_list, 0};
var_by_index(11) -> {is_tuple_of_arity, 1};
var_by_index(12) -> {get_tuple_element, 1};
var_by_index(13) -> {move2, 0};
var_by_index(14) -> {move, 4};
var_by_index(15) -> {l_call_only, 0};
var_by_index(16) -> {l_is_eq_exact_immed, 0};
var_by_index(17) -> {l_is_eq_exact_immed, 1};
var_by_index(18) -> {is_tuple_of_arity, 0};
var_by_index(19) -> {put_list, 1};
var_by_index(20) -> {get_list, 0};
var_by_index(21) -> {l_put_tuple, 1};
var_by_index(22) -> {l_is_ge, 0};
var_by_index(23) -> {l_call_ext, 105};
var_by_index(24) -> {l_make_fun, 0};
var_by_index(25) -> {l_move_call_last, 0};
var_by_index(26) -> {extract_next_element2, 0};
var_by_index(27) -> {is_tuple_of_arity, 2};
var_by_index(28) -> {return, 0};
var_by_index(29) -> {l_fetch, 0};
var_by_index(30) -> {call_bif, 3};
var_by_index(31) -> {move_deallocate_return, 0};
var_by_index(32) -> {l_trim, 0};
var_by_index(33) -> {l_allocate, 0};
var_by_index(34) -> {move2, 3};
var_by_index(35) -> {l_fetch, 1};
var_by_index(36) -> {put_list, 2};
var_by_index(37) -> {is_nil, 0};
var_by_index(38) -> {is_nonempty_list, 0};
var_by_index(39) -> {extract_next_element, 0};
var_by_index(40) -> {l_is_eq_exact_immed, 2};
var_by_index(41) -> {l_allocate, 1};
var_by_index(42) -> {jump, 0};
var_by_index(43) -> {move_return, 36};
var_by_index(44) -> {deallocate_return, 0};
var_by_index(45) -> {case_end, 0};
var_by_index(46) -> {get_list, 1};
var_by_index(47) -> {call_bif, 7};
var_by_index(48) -> {move2, 2};
var_by_index(49) -> {l_is_eq_exact, 0};
var_by_index(50) -> {l_move_call, 35};
var_by_index(51) -> {get_tuple_element, 2};
var_by_index(52) -> {l_allocate, 2};
var_by_index(53) -> {move_return, 0};
var_by_index(54) -> {l_is_eq_exact_immed, 3};
var_by_index(55) -> {l_select_val2, 0};
var_by_index(56) -> {get_tuple_element, 3};
var_by_index(57) -> {move, 5};
var_by_index(58) -> {l_fetch, 2};
var_by_index(59) -> {init2, 0};
var_by_index(60) -> {get_list, 2};
var_by_index(61) -> {l_is_eq_exact_immed, 4};
var_by_index(62) -> {init, 0};
var_by_index(63) -> {deallocate_return, 1};
var_by_index(64) -> {put_list, 3};
var_by_index(65) -> {call_bif, 8};
var_by_index(66) -> {extract_next_element, 1};
var_by_index(67) -> {is_tuple_of_arity, 3};
var_by_index(68) -> {get_tuple_element, 4};
var_by_index(69) -> {move2, 4};
var_by_index(70) -> {l_fetch, 3};
var_by_index(71) -> {init, 1};
var_by_index(72) -> {l_put_tuple, 2};
var_by_index(73) -> {move2, 5};
var_by_index(74) -> {l_allocate, 3};
var_by_index(75) -> {deallocate_return, 2};
var_by_index(76) -> {l_fetch, 4};
var_by_index(77) -> {init3, 0};
var_by_index(78) -> {is_nonempty_list, 1};
var_by_index(79) -> {l_move_call_ext, 49};
var_by_index(80) -> {l_is_eq_exact_immed, 5};
var_by_index(81) -> {l_allocate_zero, 0};
var_by_index(82) -> {call_bif, 6};
var_by_index(83) -> {l_trim, 1};
var_by_index(84) -> {allocate_init, 0};
var_by_index(85) -> {allocate_heap, 0};
var_by_index(86) -> {test_heap_1_put_list, 0};
var_by_index(87) -> {l_allocate_zero, 1};
var_by_index(88) -> {init, 2};
var_by_index(89) -> {move_deallocate_return, 1};
var_by_index(90) -> {l_call_last, 0};
var_by_index(91) -> {call_bif, 48};
var_by_index(92) -> {l_is_eq_exact_literal, 0};
var_by_index(93) -> {is_nonempty_list, 2};
var_by_index(94) -> {l_move_call, 0};
var_by_index(95) -> {move_return, 1};
var_by_index(96) -> {is_tuple, 0};
var_by_index(97) -> {l_is_eq_exact_immed, 6};
var_by_index(98) -> {is_list, 0};
var_by_index(99) -> {l_call_last, 1};
var_by_index(100) -> {deallocate_return, 3};
var_by_index(101) -> {is_nonempty_list_allocate, 0};
var_by_index(102) -> {l_move_call_only, 0};
var_by_index(103) -> {call_bif, 5};
var_by_index(104) -> {extract_next_element, 2};
var_by_index(105) -> {l_increment, 0};
var_by_index(106) -> {move, 6};
var_by_index(107) -> {l_is_lt, 0};
var_by_index(108) -> {l_trim, 2};
var_by_index(109) -> {l_gc_bif1, 0};
var_by_index(110) -> {l_select_val_atoms, 0};
var_by_index(111) -> {l_call_last, 2};
var_by_index(112) -> {move_deallocate_return, 2};
var_by_index(113) -> {is_nonempty_list, 3};
var_by_index(114) -> {l_bs_start_match2, 0};
var_by_index(115) -> {l_new_bs_put_integer_imm, 0};
var_by_index(116) -> {is_nil, 1};
var_by_index(117) -> {extract_next_element2, 1};
var_by_index(118) -> {l_select_val2, 2};
var_by_index(119) -> {remove_message, 0};
var_by_index(120) -> {l_move_call_only, 10};
var_by_index(121) -> {l_move_call_only, 1};
var_by_index(122) -> {init, 3};
var_by_index(123) -> {l_catch, 0};
var_by_index(124) -> {l_allocate_zero, 2};
var_by_index(125) -> {extract_next_element3, 0};
var_by_index(126) -> {get_tuple_element, 5};
var_by_index(127) -> {l_call_ext, 0};
var_by_index(128) -> {move_jump, 12};
var_by_index(129) -> {l_fetch, 5};
var_by_index(130) -> {extract_next_element, 3};
var_by_index(131) -> {is_nil, 2};
var_by_index(132) -> {move2, 6};
var_by_index(133) -> {l_allocate, 4};
var_by_index(134) -> {catch_end, 0};
var_by_index(135) -> {test_arity, 0};
var_by_index(136) -> {l_allocate_zero, 3};
var_by_index(137) -> {l_is_eq_exact_immed, 7};
var_by_index(138) -> {move_return, 2};
var_by_index(139) -> {move_return, 3};
var_by_index(140) -> {l_move_call_ext, 0};
var_by_index(141) -> {put_list, 4};
var_by_index(142) -> {move_return, 4};
var_by_index(143) -> {deallocate_return, 4};
var_by_index(144) -> {l_call_last, 3};
var_by_index(145) -> {move2, 7};
var_by_index(146) -> {l_is_eq_exact_immed, 8};
var_by_index(147) -> {l_plus, 0};
var_by_index(148) -> {move, 7};
var_by_index(149) -> {l_put_tuple, 3};
var_by_index(150) -> {call_bif, 2};
var_by_index(151) -> {l_select_tuple_arity2, 0};
var_by_index(152) -> {is_nonempty_list, 4};
var_by_index(153) -> {init, 4};
var_by_index(154) -> {is_nonempty_list, 5};
var_by_index(155) -> {get_list, 3};
var_by_index(156) -> {l_call_fun, 0};
var_by_index(157) -> {l_call_last, 4};
var_by_index(158) -> {l_move_call, 1};
var_by_index(159) -> {move_return, 5};
var_by_index(160) -> {l_bs_get_binary_all_reuse, 0};
var_by_index(161) -> {test_arity, 1};
var_by_index(162) -> {l_bs_match_string, 0};
var_by_index(163) -> {bif1_body, 0};
var_by_index(164) -> {l_move_call_only, 2};
var_by_index(165) -> {l_move_call_last, 1};
var_by_index(166) -> {is_nonempty_list, 6};
var_by_index(167) -> {l_bs_test_zero_tail2, 0};
var_by_index(168) -> {l_is_eq, 0};
var_by_index(169) -> {send, 0};
var_by_index(170) -> {set_tuple_element, 0};
var_by_index(171) -> {l_catch, 1};
var_by_index(172) -> {call_bif, 9};
var_by_index(173) -> {l_is_ne, 0};
var_by_index(174) -> {l_move_call_only, 3};
var_by_index(175) -> {l_move_call_ext_last, 0};
var_by_index(176) -> {l_select_val2, 1};
var_by_index(177) -> {l_select_val_smallints, 0};
var_by_index(178) -> {l_move_call_ext, 1};
var_by_index(179) -> {l_call_ext_only, 4};
var_by_index(180) -> {l_is_ne_exact_immed, 0};
var_by_index(181) -> {l_increment, 1};
var_by_index(182) -> {l_bs_add, 0};
var_by_index(183) -> {bif2_body, 0};
var_by_index(184) -> {is_nonempty_list, 7};
var_by_index(185) -> {extract_next_element, 4};
var_by_index(186) -> {case_end, 1};
var_by_index(187) -> {l_is_eq_exact_immed, 9};
var_by_index(188) -> {extract_next_element2, 2};
var_by_index(189) -> {move_deallocate_return, 3};
var_by_index(190) -> {extract_next_element2, 3};
var_by_index(191) -> {l_loop_rec, 0};
var_by_index(192) -> {l_call_ext, 1};
var_by_index(193) -> {l_select_tuple_arity2, 1};
var_by_index(194) -> {l_move_call, 2};
var_by_index(195) -> {l_move_call, 3};
var_by_index(196) -> {l_move_call_ext, 2};
var_by_index(197) -> {l_bs_get_utf16, 0};
var_by_index(198) -> {l_select_val_atoms, 1};
var_by_index(199) -> {l_trim, 3};
var_by_index(200) -> {init, 5};
var_by_index(201) -> {l_bs_restore2, 0};
var_by_index(202) -> {catch_end, 1};
var_by_index(203) -> {is_nil, 3};
var_by_index(204) -> {l_move_call, 4};
var_by_index(205) -> {l_bs_init_heap_bin, 0};
var_by_index(206) -> {is_nil, 4};
var_by_index(207) -> {is_nonempty_list, 8};
var_by_index(208) -> {wait, 0};
var_by_index(209) -> {l_call_ext_last, 0};
var_by_index(210) -> {l_allocate_zero, 4};
var_by_index(211) -> {loop_rec_end, 0};
var_by_index(212) -> {call_bif, 10};
var_by_index(213) -> {deallocate_return, 5};
var_by_index(214) -> {move, 8};
var_by_index(215) -> {l_fetch, 6};
var_by_index(216) -> {is_nonempty_list, 9};
var_by_index(217) -> {extract_next_element, 5};
var_by_index(218) -> {l_select_val2, 3};
var_by_index(219) -> {l_allocate_zero, 5};
var_by_index(220) -> {l_allocate, 5};
var_by_index(221) -> {init, 6};
var_by_index(222) -> {l_move_call_ext, 3};
var_by_index(223) -> {badmatch, 1};
var_by_index(224) -> {l_call_last, 5};
var_by_index(225) -> {l_move_call_ext, 4};
var_by_index(226) -> {l_plus, 1};
var_by_index(227) -> {l_bs_start_match2, 1};
var_by_index(228) -> {l_move_call_ext, 5};
var_by_index(229) -> {l_call_last, 6};
var_by_index(230) -> {badmatch, 2};
var_by_index(231) -> {l_move_call_ext, 6};
var_by_index(232) -> {l_call_fun_last, 0};
var_by_index(233) -> {l_move_call_ext, 7};
var_by_index(234) -> {l_trim, 4};
var_by_index(235) -> {l_increment, 2};
var_by_index(236) -> {put_list, 5};
var_by_index(237) -> {call_bif, 11};
var_by_index(238) -> {int_code_end, 0};
var_by_index(239) -> {l_move_call_ext_only, 0};
var_by_index(240) -> {l_bs_save2, 0};
var_by_index(241) -> {call_bif, 12};
var_by_index(242) -> {extract_next_element, 6};
var_by_index(243) -> {bs_context_to_binary, 0};
var_by_index(244) -> {move_deallocate_return, 4};
var_by_index(245) -> {l_is_ne_exact_immed, 1};
var_by_index(246) -> {extract_next_element2, 4};
var_by_index(247) -> {l_select_val2, 4};
var_by_index(248) -> {l_gc_bif1, 1};
var_by_index(249) -> {l_increment, 3};
var_by_index(250) -> {self, 0};
var_by_index(251) -> {l_put_tuple, 4};
var_by_index(252) -> {extract_next_element, 7};
var_by_index(253) -> {get_tuple_element, 6};
var_by_index(254) -> {call_bif, 13};
var_by_index(255) -> {is_tuple, 1};
var_by_index(256) -> {l_move_call_last, 2};
var_by_index(257) -> {l_move_call, 5};
var_by_index(258) -> {l_is_eq_exact_immed, 11};
var_by_index(259) -> {badmatch, 3};
var_by_index(260) -> {l_move_call, 6};
var_by_index(261) -> {is_nil, 5};
var_by_index(262) -> {is_nonempty_list_allocate, 1};
var_by_index(263) -> {l_call_ext, 2};
var_by_index(264) -> {l_bs_test_zero_tail2, 1};
var_by_index(265) -> {l_catch, 2};
var_by_index(266) -> {l_move_call_ext, 8};
var_by_index(267) -> {call_bif, 14};
var_by_index(268) -> {is_atom, 0};
var_by_index(269) -> {l_bs_test_zero_tail2, 2};
var_by_index(270) -> {l_move_call_ext_only, 6};
var_by_index(271) -> {l_select_val_smallints, 1};
var_by_index(272) -> {l_fast_element, 0};
var_by_index(273) -> {is_nonempty_list, 10};
var_by_index(274) -> {l_bs_match_string, 1};
var_by_index(275) -> {call_bif, 15};
var_by_index(276) -> {l_call_ext, 3};
var_by_index(277) -> {move_return, 6};
var_by_index(278) -> {l_bs_test_unit_8, 0};
var_by_index(279) -> {l_call_ext_last, 1};
var_by_index(280) -> {is_nil, 6};
var_by_index(281) -> {allocate_heap_zero, 0};
var_by_index(282) -> {is_nonempty_list, 11};
var_by_index(283) -> {l_call_ext_last, 2};
var_by_index(284) -> {call_bif, 16};
var_by_index(285) -> {self, 1};
var_by_index(286) -> {extract_next_element, 8};
var_by_index(287) -> {init, 7};
var_by_index(288) -> {case_end, 2};
var_by_index(289) -> {extract_next_element3, 1};
var_by_index(290) -> {is_nil, 7};
var_by_index(291) -> {l_move_call, 7};
var_by_index(292) -> {call_bif, 17};
var_by_index(293) -> {raise, 0};
var_by_index(294) -> {catch_end, 2};
var_by_index(295) -> {l_is_eq_exact_literal, 1};
var_by_index(296) -> {l_is_eq_exact_literal, 7};
var_by_index(297) -> {l_move_call_ext, 9};
var_by_index(298) -> {l_minus, 0};
var_by_index(299) -> {extract_next_element2, 5};
var_by_index(300) -> {l_allocate, 6};
var_by_index(301) -> {l_bif2, 0};
var_by_index(302) -> {try_end, 0};
var_by_index(303) -> {l_call_fun, 1};
var_by_index(304) -> {call_bif, 18};
var_by_index(305) -> {deallocate_return, 6};
var_by_index(306) -> {l_call_ext, 4};
var_by_index(307) -> {l_move_call_last, 3};
var_by_index(308) -> {l_put_tuple, 5};
var_by_index(309) -> {l_move_call_ext_only, 1};
var_by_index(310) -> {l_band, 0};
var_by_index(311) -> {l_move_call, 8};
var_by_index(312) -> {l_trim, 5};
var_by_index(313) -> {extract_next_element, 9};
var_by_index(314) -> {call_bif, 19};
var_by_index(315) -> {l_call_ext, 5};
var_by_index(316) -> {l_call_ext, 6};
var_by_index(317) -> {l_bs_get_binary_all2, 0};
var_by_index(318) -> {get_tuple_element, 7};
var_by_index(319) -> {l_minus, 1};
var_by_index(320) -> {l_is_eq_exact_immed, 12};
var_by_index(321) -> {extract_next_element3, 2};
var_by_index(322) -> {is_integer, 0};
var_by_index(323) -> {test_heap_1_put_list, 1};
var_by_index(324) -> {extract_next_element2, 6};
var_by_index(325) -> {l_catch, 3};
var_by_index(326) -> {extract_next_element, 26};
var_by_index(327) -> {l_bif2, 1};
var_by_index(328) -> {is_nil, 8};
var_by_index(329) -> {l_move_call_only, 4};
var_by_index(330) -> {move_deallocate_return, 5};
var_by_index(331) -> {move_jump, 0};
var_by_index(332) -> {is_list, 1};
var_by_index(333) -> {l_move_call, 9};
var_by_index(334) -> {l_bsr, 0};
var_by_index(335) -> {l_call_ext, 7};
var_by_index(336) -> {l_is_eq_exact_immed, 13};
var_by_index(337) -> {apply, 0};
var_by_index(338) -> {l_fast_element, 2};
var_by_index(339) -> {l_fetch, 7};
var_by_index(340) -> {l_bs_get_integer_small_imm, 0};
var_by_index(341) -> {l_bif2, 2};
var_by_index(342) -> {l_bs_get_integer_8, 0};
var_by_index(343) -> {l_bs_test_zero_tail2, 3};
var_by_index(344) -> {set_tuple_element, 1};
var_by_index(345) -> {try_end, 1};
var_by_index(346) -> {l_is_eq_exact_literal, 2};
var_by_index(347) -> {l_is_eq_exact_immed, 10};
var_by_index(348) -> {extract_next_element2, 7};
var_by_index(349) -> {l_is_eq_exact_literal, 3};
var_by_index(350) -> {l_select_tuple_arity, 0};
var_by_index(351) -> {l_allocate_zero, 6};
var_by_index(352) -> {l_fmul, 0};
var_by_index(353) -> {l_call_ext, 8};
var_by_index(354) -> {l_bsl, 0};
var_by_index(355) -> {is_nonempty_list, 12};
var_by_index(356) -> {move, 9};
var_by_index(357) -> {call_bif, 20};
var_by_index(358) -> {l_move_call_ext, 10};
var_by_index(359) -> {l_times, 0};
var_by_index(360) -> {l_move_call_ext_only, 2};
var_by_index(361) -> {l_put_tuple, 6};
var_by_index(362) -> {l_bs_init_fail, 0};
var_by_index(363) -> {l_is_eq_exact_immed, 14};
var_by_index(364) -> {l_times, 1};
var_by_index(365) -> {l_is_eq_exact_immed, 15};
var_by_index(366) -> {l_move_call_ext, 11};
var_by_index(367) -> {l_call_ext, 9};
var_by_index(368) -> {l_move_call_ext, 12};
var_by_index(369) -> {l_fetch, 8};
var_by_index(370) -> {extract_next_element, 10};
var_by_index(371) -> {is_nil, 9};
var_by_index(372) -> {call_bif, 21};
var_by_index(373) -> {node, 0};
var_by_index(374) -> {l_call_last, 7};
var_by_index(375) -> {l_bs_get_binary2, 0};
var_by_index(376) -> {is_tuple, 2};
var_by_index(377) -> {l_call_fun, 2};
var_by_index(378) -> {test_arity, 2};
var_by_index(379) -> {get_list, 4};
var_by_index(380) -> {l_bs_get_integer_8, 1};
var_by_index(381) -> {l_bs_test_zero_tail2, 5};
var_by_index(382) -> {catch_end, 3};
var_by_index(383) -> {l_bif2, 3};
var_by_index(384) -> {call_bif, 22};
var_by_index(385) -> {l_is_ne_exact_immed, 2};
var_by_index(386) -> {l_is_ne_exact, 0};
var_by_index(387) -> {l_bif2, 4};
var_by_index(388) -> {is_binary, 0};
var_by_index(389) -> {l_allocate_zero, 9};
var_by_index(390) -> {l_is_eq_exact_immed, 16};
var_by_index(391) -> {extract_next_element, 11};
var_by_index(392) -> {l_bs_get_integer_32, 0};
var_by_index(393) -> {l_call_ext, 10};
var_by_index(394) -> {is_atom, 1};
var_by_index(395) -> {l_select_val2, 7};
var_by_index(396) -> {l_fcheckerror, 0};
var_by_index(397) -> {fclearerror, 0};
var_by_index(398) -> {l_fetch, 9};
var_by_index(399) -> {l_new_bs_put_binary_all, 0};
var_by_index(400) -> {extract_next_element3, 3};
var_by_index(401) -> {node, 1};
var_by_index(402) -> {extract_next_element, 12};
var_by_index(403) -> {l_move_call, 10};
var_by_index(404) -> {get_list, 5};
var_by_index(405) -> {move_deallocate_return, 6};
var_by_index(406) -> {l_move_call_last, 4};
var_by_index(407) -> {l_new_bs_put_binary_all, 1};
var_by_index(408) -> {l_is_eq_exact_immed, 17};
var_by_index(409) -> {timeout, 0};
var_by_index(410) -> {deallocate_return, 7};
var_by_index(411) -> {l_get, 0};
var_by_index(412) -> {l_move_call, 11};
var_by_index(413) -> {l_fetch, 10};
var_by_index(414) -> {l_move_call_ext_last, 1};
var_by_index(415) -> {is_nil, 10};
var_by_index(416) -> {l_fetch, 11};
var_by_index(417) -> {call_bif, 23};
var_by_index(418) -> {l_call_ext, 11};
var_by_index(419) -> {is_float, 1};
var_by_index(420) -> {l_select_val2, 5};
var_by_index(421) -> {l_select_val2, 8};
var_by_index(422) -> {l_is_eq_exact_immed, 36};
var_by_index(423) -> {call_bif, 24};
var_by_index(424) -> {l_band, 1};
var_by_index(425) -> {l_get, 1};
var_by_index(426) -> {is_binary, 1};
var_by_index(427) -> {l_fast_element, 1};
var_by_index(428) -> {l_get, 2};
var_by_index(429) -> {is_nonempty_list, 39};
var_by_index(430) -> {l_move_call_ext, 14};
var_by_index(431) -> {extract_next_element, 13};
var_by_index(432) -> {is_nonempty_list, 13};
var_by_index(433) -> {l_move_call, 12};
var_by_index(434) -> {l_call_ext, 12};
var_by_index(435) -> {l_is_eq_exact_immed, 18};
var_by_index(436) -> {l_move_call, 13};
var_by_index(437) -> {l_move_call_ext, 15};
var_by_index(438) -> {l_rem, 0};
var_by_index(439) -> {move2, 8};
var_by_index(440) -> {l_bsl, 1};
var_by_index(441) -> {l_call_ext, 13};
var_by_index(442) -> {l_allocate_zero, 7};
var_by_index(443) -> {l_call_last, 8};
var_by_index(444) -> {is_nil, 11};
var_by_index(445) -> {l_gc_bif1, 2};
var_by_index(446) -> {l_is_ne_exact_immed, 10};
var_by_index(447) -> {extract_next_element3, 4};
var_by_index(448) -> {l_increment, 4};
var_by_index(449) -> {is_nonempty_list, 14};
var_by_index(450) -> {is_nil, 30};
var_by_index(451) -> {extract_next_element3, 10};
var_by_index(452) -> {l_bs_append, 0};
var_by_index(453) -> {call_bif, 25};
var_by_index(454) -> {l_trim, 6};
var_by_index(455) -> {is_nil, 12};
var_by_index(456) -> {l_move_call_ext, 16};
var_by_index(457) -> {is_integer, 5};
var_by_index(458) -> {l_call_ext, 14};
var_by_index(459) -> {l_bor, 0};
var_by_index(460) -> {move_return, 7};
var_by_index(461) -> {is_list, 2};
var_by_index(462) -> {l_select_val2, 15};
var_by_index(463) -> {l_call_ext, 15};
var_by_index(464) -> {is_nil, 13};
var_by_index(465) -> {l_catch, 4};
var_by_index(466) -> {l_element, 1};
var_by_index(467) -> {extract_next_element2, 17};
var_by_index(468) -> {l_gc_bif1, 5};
var_by_index(469) -> {l_fadd, 0};
var_by_index(470) -> {call_bif, 26};
var_by_index(471) -> {l_allocate, 7};
var_by_index(472) -> {l_move_call_only, 5};
var_by_index(473) -> {is_nonempty_list, 15};
var_by_index(474) -> {l_move_call_ext, 17};
var_by_index(475) -> {l_move_call_ext, 18};
var_by_index(476) -> {call_bif, 28};
var_by_index(477) -> {call_bif, 27};
var_by_index(478) -> {is_integer, 1};
var_by_index(479) -> {bif1_body, 1};
var_by_index(480) -> {l_is_ne_exact_immed, 3};
var_by_index(481) -> {l_call_ext, 16};
var_by_index(482) -> {l_call_ext_last, 3};
var_by_index(483) -> {l_fetch, 12};
var_by_index(484) -> {l_move_call_ext, 19};
var_by_index(485) -> {l_is_eq_exact_immed, 19};
var_by_index(486) -> {is_nonempty_list, 16};
var_by_index(487) -> {bif1_body, 2};
var_by_index(488) -> {fmove_1, 0};
var_by_index(489) -> {l_move_call_ext, 20};
var_by_index(490) -> {call_bif, 29};
var_by_index(491) -> {move_jump, 1};
var_by_index(492) -> {l_bs_get_utf8, 0};
var_by_index(493) -> {l_move_call_ext, 21};
var_by_index(494) -> {l_bs_skip_bits_all2, 0};
var_by_index(495) -> {bif2_body, 1};
var_by_index(496) -> {l_call_ext, 17};
var_by_index(497) -> {call_bif, 30};
var_by_index(498) -> {fconv, 0};
var_by_index(499) -> {l_is_eq_exact_literal, 4};
var_by_index(500) -> {l_is_eq_exact_immed, 20};
var_by_index(501) -> {call_bif, 31};
var_by_index(502) -> {extract_next_element3, 5};
var_by_index(503) -> {extract_next_element2, 8};
var_by_index(504) -> {system_limit, 0};
var_by_index(505) -> {l_move_call_ext_last, 2};
var_by_index(506) -> {l_fetch, 23};
var_by_index(507) -> {call_bif, 32};
var_by_index(508) -> {call_bif, 4};
var_by_index(509) -> {apply_last, 0};
var_by_index(510) -> {is_atom, 2};
var_by_index(511) -> {l_move_call_ext_only, 3};
var_by_index(512) -> {l_bor, 1};
var_by_index(513) -> {l_is_eq_exact_immed, 22};
var_by_index(514) -> {put_list, 6};
var_by_index(515) -> {call_bif, 33};
var_by_index(516) -> {extract_next_element, 14};
var_by_index(517) -> {l_catch, 5};
var_by_index(518) -> {init, 8};
var_by_index(519) -> {put_list, 8};
var_by_index(520) -> {l_make_export, 0};
var_by_index(521) -> {l_select_val2, 6};
var_by_index(522) -> {move_deallocate_return, 7};
var_by_index(523) -> {l_call_fun, 3};
var_by_index(524) -> {call_bif, 34};
var_by_index(525) -> {badmatch, 4};
var_by_index(526) -> {is_atom, 3};
var_by_index(527) -> {l_move_call, 14};
var_by_index(528) -> {l_bs_test_zero_tail2, 4};
var_by_index(529) -> {fmove_1, 1};
var_by_index(530) -> {fmove_2, 0};
var_by_index(531) -> {l_new_bs_put_integer, 0};
var_by_index(532) -> {fmove_2, 1};
var_by_index(533) -> {call_bif, 35};
var_by_index(534) -> {is_integer_allocate, 0};
var_by_index(535) -> {l_call_ext_last, 4};
var_by_index(536) -> {l_move_call_ext, 22};
var_by_index(537) -> {l_call_ext, 18};
var_by_index(538) -> {l_fast_element, 3};
var_by_index(539) -> {l_move_call, 15};
var_by_index(540) -> {l_move_call_ext, 23};
var_by_index(541) -> {l_fetch, 13};
var_by_index(542) -> {l_trim, 7};
var_by_index(543) -> {l_bs_restore2, 1};
var_by_index(544) -> {deallocate_return, 8};
var_by_index(545) -> {is_list, 6};
var_by_index(546) -> {l_call_ext, 19};
var_by_index(547) -> {extract_next_element, 15};
var_by_index(548) -> {l_fetch, 14};
var_by_index(549) -> {l_is_eq_exact_immed, 23};
var_by_index(550) -> {is_tuple, 3};
var_by_index(551) -> {is_nonempty_list, 17};
var_by_index(552) -> {l_move_call_ext, 13};
var_by_index(553) -> {l_fetch, 15};
var_by_index(554) -> {l_move_call_last, 5};
var_by_index(555) -> {extract_next_element, 16};
var_by_index(556) -> {extract_next_element2, 9};
var_by_index(557) -> {try_end, 2};
var_by_index(558) -> {if_end, 0};
var_by_index(559) -> {l_get, 3};
var_by_index(560) -> {call_bif, 36};
var_by_index(561) -> {case_end, 11};
var_by_index(562) -> {l_move_call, 16};
var_by_index(563) -> {call_bif, 38};
var_by_index(564) -> {call_bif, 37};
var_by_index(565) -> {move_jump, 2};
var_by_index(566) -> {try_end, 4};
var_by_index(567) -> {try_end, 3};
var_by_index(568) -> {l_is_ne_exact_immed, 4};
var_by_index(569) -> {call_bif, 39};
var_by_index(570) -> {self, 2};
var_by_index(571) -> {l_call_ext, 23};
var_by_index(572) -> {l_call_ext, 22};
var_by_index(573) -> {l_call_ext, 21};
var_by_index(574) -> {l_call_ext, 20};
var_by_index(575) -> {move_return, 8};
var_by_index(576) -> {is_list, 3};
var_by_index(577) -> {is_nonempty_list, 18};
var_by_index(578) -> {try_end, 5};
var_by_index(579) -> {catch_end, 4};
var_by_index(580) -> {l_move_call_ext_last, 3};
var_by_index(581) -> {l_call_ext, 25};
var_by_index(582) -> {l_call_ext, 24};
var_by_index(583) -> {extract_next_element2, 10};
var_by_index(584) -> {is_nonempty_list, 19};
var_by_index(585) -> {init, 9};
var_by_index(586) -> {l_call_ext, 26};
var_by_index(587) -> {case_end, 3};
var_by_index(588) -> {bif1_body, 3};
var_by_index(589) -> {get_tuple_element, 8};
var_by_index(590) -> {test_arity, 3};
var_by_index(591) -> {l_bs_skip_bits_imm2, 0};
var_by_index(592) -> {l_bif1, 0};
var_by_index(593) -> {l_bs_get_utf16, 1};
var_by_index(594) -> {is_tuple, 4};
var_by_index(595) -> {l_call_ext, 28};
var_by_index(596) -> {l_call_ext, 27};
var_by_index(597) -> {extract_next_element, 17};
var_by_index(598) -> {l_bs_init_bits_fail, 0};
var_by_index(599) -> {l_select_val_atoms, 2};
var_by_index(600) -> {l_is_eq_exact_immed, 24};
var_by_index(601) -> {l_select_val2, 9};
var_by_index(602) -> {l_call_ext, 29};
var_by_index(603) -> {is_integer, 2};
var_by_index(604) -> {badmatch, 5};
var_by_index(605) -> {try_case_end, 0};
var_by_index(606) -> {l_fdiv, 0};
var_by_index(607) -> {is_tuple, 10};
var_by_index(608) -> {l_get, 6};
var_by_index(609) -> {l_call_ext_last, 6};
var_by_index(610) -> {l_bif1, 1};
var_by_index(611) -> {l_move_call_ext, 24};
var_by_index(612) -> {l_move_call_ext_last, 5};
var_by_index(613) -> {get_list, 6};
var_by_index(614) -> {case_end, 4};
var_by_index(615) -> {put_list, 9};
var_by_index(616) -> {l_call_ext, 30};
var_by_index(617) -> {l_bs_test_unit_8, 1};
var_by_index(618) -> {l_gc_bif1, 3};
var_by_index(619) -> {self, 5};
var_by_index(620) -> {l_increment, 9};
var_by_index(621) -> {call_bif, 41};
var_by_index(622) -> {call_bif, 40};
var_by_index(623) -> {extract_next_element, 18};
var_by_index(624) -> {extract_next_element2, 11};
var_by_index(625) -> {move_return, 9};
var_by_index(626) -> {is_nil, 14};
var_by_index(627) -> {l_move_call_only, 6};
var_by_index(628) -> {l_bs_restore2, 2};
var_by_index(629) -> {l_move_call, 18};
var_by_index(630) -> {l_move_call, 17};
var_by_index(631) -> {l_is_eq_exact_immed, 25};
var_by_index(632) -> {bif1_body, 5};
var_by_index(633) -> {move, 10};
var_by_index(634) -> {is_integer, 3};
var_by_index(635) -> {l_allocate_zero, 8};
var_by_index(636) -> {is_nil, 15};
var_by_index(637) -> {is_list, 4};
var_by_index(638) -> {case_end, 5};
var_by_index(639) -> {is_atom, 6};
var_by_index(640) -> {l_move_call_ext, 26};
var_by_index(641) -> {call_bif, 42};
var_by_index(642) -> {get_tuple_element, 9};
var_by_index(643) -> {l_move_call_ext, 27};
var_by_index(644) -> {l_is_eq_exact_immed, 26};
var_by_index(645) -> {l_increment, 5};
var_by_index(646) -> {l_fsub, 0};
var_by_index(647) -> {l_increment, 7};
var_by_index(648) -> {put_list, 7};
var_by_index(649) -> {l_move_call_ext, 28};
var_by_index(650) -> {call_bif, 43};
var_by_index(651) -> {l_call_ext, 31};
var_by_index(652) -> {extract_next_element3, 6};
var_by_index(653) -> {l_bsr, 1};
var_by_index(654) -> {badmatch, 6};
var_by_index(655) -> {l_move_call_ext, 30};
var_by_index(656) -> {l_move_call_ext, 29};
var_by_index(657) -> {l_call_ext, 33};
var_by_index(658) -> {l_call_ext, 32};
var_by_index(659) -> {extract_next_element2, 12};
var_by_index(660) -> {is_integer, 4};
var_by_index(661) -> {is_nonempty_list, 20};
var_by_index(662) -> {l_move_call, 19};
var_by_index(663) -> {l_is_eq_exact_literal, 5};
var_by_index(664) -> {l_call_ext_only, 0};
var_by_index(665) -> {l_plus, 2};
var_by_index(666) -> {move_deallocate_return, 8};
var_by_index(667) -> {move_return, 10};
var_by_index(668) -> {l_is_eq_exact_literal, 6};
var_by_index(669) -> {l_int_div, 0};
var_by_index(670) -> {l_rem, 1};
var_by_index(671) -> {l_bs_get_binary_imm2, 0};
var_by_index(672) -> {l_move_call_ext, 31};
var_by_index(673) -> {l_call_ext, 34};
var_by_index(674) -> {l_move_call, 20};
var_by_index(675) -> {init, 10};
var_by_index(676) -> {catch_end, 5};
var_by_index(677) -> {node, 4};
var_by_index(678) -> {put_list, 10};
var_by_index(679) -> {l_allocate, 9};
var_by_index(680) -> {l_bif2, 5};
var_by_index(681) -> {is_atom, 4};
var_by_index(682) -> {l_move_call, 21};
var_by_index(683) -> {l_fetch, 16};
var_by_index(684) -> {l_increment, 6};
var_by_index(685) -> {bif1_body, 6};
var_by_index(686) -> {l_fetch, 17};
var_by_index(687) -> {test_heap_1_put_list, 2};
var_by_index(688) -> {call_bif, 44};
var_by_index(689) -> {node, 2};
var_by_index(690) -> {l_call_ext, 37};
var_by_index(691) -> {l_call_ext, 36};
var_by_index(692) -> {l_call_ext, 35};
var_by_index(693) -> {l_call_last, 9};
var_by_index(694) -> {is_nil, 16};
var_by_index(695) -> {l_bs_put_string, 0};
var_by_index(696) -> {is_list, 5};
var_by_index(697) -> {case_end, 6};
var_by_index(698) -> {l_yield, 0};
var_by_index(699) -> {l_move_call, 22};
var_by_index(700) -> {l_catch, 6};
var_by_index(701) -> {get_list, 7};
var_by_index(702) -> {l_bs_get_binary_all2, 1};
var_by_index(703) -> {l_is_eq_exact_immed, 27};
var_by_index(704) -> {l_call_ext, 38};
var_by_index(705) -> {case_end, 7};
var_by_index(706) -> {is_nonempty_list, 21};
var_by_index(707) -> {l_move_call_ext, 33};
var_by_index(708) -> {l_select_tuple_arity, 3};
var_by_index(709) -> {l_move_call_ext, 34};
var_by_index(710) -> {l_bs_append, 1};
var_by_index(711) -> {l_bs_get_integer, 0};
var_by_index(712) -> {l_move_call_last, 6};
var_by_index(713) -> {call_bif, 46};
var_by_index(714) -> {call_bif, 45};
var_by_index(715) -> {l_call_ext, 39};
var_by_index(716) -> {l_is_eq_exact_immed, 21};
var_by_index(717) -> {is_nonempty_list, 22};
var_by_index(718) -> {is_function, 1};
var_by_index(719) -> {l_gc_bif1, 4};
var_by_index(720) -> {l_is_ne_exact_immed, 5};
var_by_index(721) -> {l_jump_on_val, 0};
var_by_index(722) -> {call_bif, 47};
var_by_index(723) -> {l_call_ext, 40};
var_by_index(724) -> {l_is_ne_exact_immed, 6};
var_by_index(725) -> {extract_next_element2, 13};
var_by_index(726) -> {move_jump, 3};
var_by_index(727) -> {badmatch, 7};
var_by_index(728) -> {l_bs_test_unit_8, 2};
var_by_index(729) -> {l_move_call, 24};
var_by_index(730) -> {l_move_call, 23};
var_by_index(731) -> {l_is_eq_exact_immed, 28};
var_by_index(732) -> {l_call_ext, 41};
var_by_index(733) -> {is_nil, 18};
var_by_index(734) -> {is_nil, 17};
var_by_index(735) -> {is_nonempty_list, 23};
var_by_index(736) -> {l_move_call, 27};
var_by_index(737) -> {l_move_call, 26};
var_by_index(738) -> {l_move_call, 25};
var_by_index(739) -> {l_select_tuple_arity, 1};
var_by_index(740) -> {l_fetch, 18};
var_by_index(741) -> {l_move_call_ext, 35};
var_by_index(742) -> {l_bs_get_integer_32, 1};
var_by_index(743) -> {fmove_1, 3};
var_by_index(744) -> {l_int_div, 1};
var_by_index(745) -> {l_is_eq_exact_immed, 29};
var_by_index(746) -> {l_move_call_ext_only, 4};
var_by_index(747) -> {l_move_call_ext, 36};
var_by_index(748) -> {call_bif, 1};
var_by_index(749) -> {l_call_ext, 48};
var_by_index(750) -> {l_call_ext, 47};
var_by_index(751) -> {l_call_ext, 46};
var_by_index(752) -> {l_call_ext, 45};
var_by_index(753) -> {l_call_ext, 44};
var_by_index(754) -> {l_call_ext, 43};
var_by_index(755) -> {l_call_ext, 42};
var_by_index(756) -> {extract_next_element3, 7};
var_by_index(757) -> {move_return, 11};
var_by_index(758) -> {l_bs_test_unit_8, 3};
var_by_index(759) -> {l_bs_save2, 1};
var_by_index(760) -> {bif2_body, 2};
var_by_index(761) -> {is_binary, 3};
var_by_index(762) -> {l_move_call_ext, 37};
var_by_index(763) -> {bif1_body, 7};
var_by_index(764) -> {l_allocate, 8};
var_by_index(765) -> {l_call_ext, 50};
var_by_index(766) -> {l_call_ext, 49};
var_by_index(767) -> {move_return, 12};
var_by_index(768) -> {l_trim, 8};
var_by_index(769) -> {is_nil, 19};
var_by_index(770) -> {case_end, 8};
var_by_index(771) -> {fconv, 1};
var_by_index(772) -> {deallocate_return, 12};
var_by_index(773) -> {l_catch, 8};
var_by_index(774) -> {l_move_call_ext_last, 4};
var_by_index(775) -> {l_gc_bif1, 6};
var_by_index(776) -> {move_deallocate_return, 10};
var_by_index(777) -> {l_move_call_ext, 38};
var_by_index(778) -> {l_call_ext, 53};
var_by_index(779) -> {l_call_ext, 52};
var_by_index(780) -> {l_call_ext, 51};
var_by_index(781) -> {extract_next_element, 19};
var_by_index(782) -> {l_call_fun, 4};
var_by_index(783) -> {l_move_call_ext_only, 5};
var_by_index(784) -> {l_move_call_ext, 40};
var_by_index(785) -> {l_move_call_ext, 39};
var_by_index(786) -> {l_bs_put_string, 2};
var_by_index(787) -> {l_is_ne_exact_literal, 0};
var_by_index(788) -> {l_call_ext, 54};
var_by_index(789) -> {move_return, 13};
var_by_index(790) -> {badmatch, 8};
var_by_index(791) -> {l_move_call_ext, 42};
var_by_index(792) -> {l_move_call_ext, 41};
var_by_index(793) -> {l_bs_test_unit_8, 4};
var_by_index(794) -> {is_pid, 1};
var_by_index(795) -> {is_boolean, 0};
var_by_index(796) -> {l_get, 4};
var_by_index(797) -> {bif1_body, 4};
var_by_index(798) -> {l_fetch, 19};
var_by_index(799) -> {bs_context_to_binary, 5};
var_by_index(800) -> {l_select_tuple_arity2, 2};
var_by_index(801) -> {l_call_ext_last, 5};
var_by_index(802) -> {l_select_val2, 10};
var_by_index(803) -> {l_call_ext, 58};
var_by_index(804) -> {l_call_ext, 57};
var_by_index(805) -> {l_call_ext, 56};
var_by_index(806) -> {l_call_ext, 55};
var_by_index(807) -> {extract_next_element2, 14};
var_by_index(808) -> {move_jump, 4};
var_by_index(809) -> {l_move_call_only, 7};
var_by_index(810) -> {l_bs_skip_bits2, 0};
var_by_index(811) -> {put_list, 12};
var_by_index(812) -> {l_bs_get_binary2, 1};
var_by_index(813) -> {l_call_ext, 65};
var_by_index(814) -> {l_call_ext, 64};
var_by_index(815) -> {l_call_ext, 63};
var_by_index(816) -> {l_call_ext, 62};
var_by_index(817) -> {l_call_ext, 61};
var_by_index(818) -> {l_call_ext, 60};
var_by_index(819) -> {l_call_ext, 59};
var_by_index(820) -> {extract_next_element, 20};
var_by_index(821) -> {extract_next_element2, 15};
var_by_index(822) -> {move_return, 14};
var_by_index(823) -> {bs_context_to_binary, 1};
var_by_index(824) -> {l_move_call, 28};
var_by_index(825) -> {init, 11};
var_by_index(826) -> {l_bs_skip_bits2, 1};
var_by_index(827) -> {l_move_call_last, 7};
var_by_index(828) -> {l_call_ext, 68};
var_by_index(829) -> {l_call_ext, 67};
var_by_index(830) -> {l_call_ext, 66};
var_by_index(831) -> {move_jump, 6};
var_by_index(832) -> {move_jump, 5};
var_by_index(833) -> {move_return, 15};
var_by_index(834) -> {is_nonempty_list_test_heap, 0};
var_by_index(835) -> {l_move_call_ext, 44};
var_by_index(836) -> {catch_end, 6};
var_by_index(837) -> {l_get, 5};
var_by_index(838) -> {l_bs_skip_bits_all2, 1};
var_by_index(839) -> {l_bs_start_match2, 2};
var_by_index(840) -> {l_move_call_ext, 46};
var_by_index(841) -> {l_wait_timeout, 2};
var_by_index(842) -> {l_element, 0};
var_by_index(843) -> {l_move_call_ext, 47};
var_by_index(844) -> {l_move_call_ext, 48};
var_by_index(845) -> {l_call_ext, 70};
var_by_index(846) -> {l_call_ext, 69};
var_by_index(847) -> {l_move_call_only, 8};
var_by_index(848) -> {case_end, 9};
var_by_index(849) -> {is_pid, 0};
var_by_index(850) -> {l_move_call, 29};
var_by_index(851) -> {l_select_tuple_arity, 2};
var_by_index(852) -> {is_nonempty_list_test_heap, 1};
var_by_index(853) -> {catch_end, 8};
var_by_index(854) -> {node, 3};
var_by_index(855) -> {bs_init_writable, 0};
var_by_index(856) -> {l_call_ext, 73};
var_by_index(857) -> {l_call_ext, 72};
var_by_index(858) -> {l_call_ext, 71};
var_by_index(859) -> {extract_next_element, 21};
var_by_index(860) -> {move_jump, 7};
var_by_index(861) -> {move_return, 17};
var_by_index(862) -> {move_return, 16};
var_by_index(863) -> {l_new_bs_put_integer_imm, 1};
var_by_index(864) -> {bs_context_to_binary, 2};
var_by_index(865) -> {is_nonempty_list, 25};
var_by_index(866) -> {is_nonempty_list, 24};
var_by_index(867) -> {try_end, 6};
var_by_index(868) -> {l_bs_private_append, 0};
var_by_index(869) -> {deallocate_return, 9};
var_by_index(870) -> {l_new_bs_put_float_imm, 0};
var_by_index(871) -> {l_move_call, 30};
var_by_index(872) -> {l_call_ext_only, 1};
var_by_index(873) -> {l_apply, 0};
var_by_index(874) -> {l_move_call_ext, 25};
var_by_index(875) -> {fmove_1, 2};
var_by_index(876) -> {put_list, 11};
var_by_index(877) -> {l_bif2, 6};
var_by_index(878) -> {bif2_body, 3};
var_by_index(879) -> {self, 3};
var_by_index(880) -> {is_tuple, 5};
var_by_index(881) -> {l_call_ext, 76};
var_by_index(882) -> {l_call_ext, 75};
var_by_index(883) -> {l_call_ext, 74};
var_by_index(884) -> {extract_next_element3, 8};
var_by_index(885) -> {l_new_bs_put_float_imm, 1};
var_by_index(886) -> {l_move_call, 33};
var_by_index(887) -> {l_move_call, 32};
var_by_index(888) -> {l_move_call, 31};
var_by_index(889) -> {l_catch, 7};
var_by_index(890) -> {catch_end, 7};
var_by_index(891) -> {l_is_ne_exact_immed, 7};
var_by_index(892) -> {test_heap_1_put_list, 3};
var_by_index(893) -> {l_call_ext, 77};
var_by_index(894) -> {extract_next_element3, 9};
var_by_index(895) -> {move_jump, 8};
var_by_index(896) -> {is_nonempty_list, 26};
var_by_index(897) -> {l_move_call, 34};
var_by_index(898) -> {is_binary, 2};
var_by_index(899) -> {l_fetch, 21};
var_by_index(900) -> {l_fetch, 20};
var_by_index(901) -> {l_bs_skip_bits_all2, 2};
var_by_index(902) -> {l_is_ne_exact_immed, 8};
var_by_index(903) -> {l_bs_get_utf8, 1};
var_by_index(904) -> {l_bs_get_integer_imm, 0};
var_by_index(905) -> {l_select_val2, 11};
var_by_index(906) -> {self, 4};
var_by_index(907) -> {l_call_ext, 79};
var_by_index(908) -> {l_call_ext, 78};
var_by_index(909) -> {l_call_last, 10};
var_by_index(910) -> {move_return, 18};
var_by_index(911) -> {is_nil, 20};
var_by_index(912) -> {recv_mark, 0};
var_by_index(913) -> {badmatch, 10};
var_by_index(914) -> {badmatch, 9};
var_by_index(915) -> {is_function, 0};
var_by_index(916) -> {l_recv_set, 0};
var_by_index(917) -> {l_new_bs_put_integer, 1};
var_by_index(918) -> {badmatch, 17};
var_by_index(919) -> {move2, 9};
var_by_index(920) -> {l_call_ext, 88};
var_by_index(921) -> {l_call_ext, 87};
var_by_index(922) -> {l_call_ext, 86};
var_by_index(923) -> {l_call_ext, 85};
var_by_index(924) -> {l_call_ext, 84};
var_by_index(925) -> {l_call_ext, 83};
var_by_index(926) -> {l_call_ext, 82};
var_by_index(927) -> {l_call_ext, 81};
var_by_index(928) -> {l_call_ext, 80};
var_by_index(929) -> {extract_next_element2, 16};
var_by_index(930) -> {move_jump, 9};
var_by_index(931) -> {is_nil, 21};
var_by_index(932) -> {l_bs_put_string, 1};
var_by_index(933) -> {l_select_val2, 12};
var_by_index(934) -> {is_atom, 5};
var_by_index(935) -> {l_bs_init_bits_fail, 1};
var_by_index(936) -> {l_bs_get_integer_16, 0};
var_by_index(937) -> {l_move_call_ext, 32};
var_by_index(938) -> {is_float, 0};
var_by_index(939) -> {l_is_eq_exact_immed, 30};
var_by_index(940) -> {l_bs_get_float2, 0};
var_by_index(941) -> {l_trim, 11};
var_by_index(942) -> {l_call_ext, 91};
var_by_index(943) -> {l_call_ext, 90};
var_by_index(944) -> {l_call_ext, 89};
var_by_index(945) -> {l_is_ne_exact_immed, 9};
var_by_index(946) -> {l_fast_element, 4};
var_by_index(947) -> {move_return, 20};
var_by_index(948) -> {move_return, 19};
var_by_index(949) -> {bs_context_to_binary, 3};
var_by_index(950) -> {is_nonempty_list, 28};
var_by_index(951) -> {is_nonempty_list, 27};
var_by_index(952) -> {deallocate_return, 11};
var_by_index(953) -> {deallocate_return, 10};
var_by_index(954) -> {l_bs_init_bits, 0};
var_by_index(955) -> {l_increment, 8};
var_by_index(956) -> {l_bif1, 2};
var_by_index(957) -> {l_minus, 2};
var_by_index(958) -> {is_tuple, 6};
var_by_index(959) -> {l_call_ext, 98};
var_by_index(960) -> {l_call_ext, 97};
var_by_index(961) -> {l_call_ext, 96};
var_by_index(962) -> {l_call_ext, 95};
var_by_index(963) -> {l_call_ext, 94};
var_by_index(964) -> {l_call_ext, 93};
var_by_index(965) -> {l_call_ext, 92};
var_by_index(966) -> {l_call_last, 11};
var_by_index(967) -> {l_is_eq_exact_immed, 31};
var_by_index(968) -> {extract_next_element, 22};
var_by_index(969) -> {move_return, 21};
var_by_index(970) -> {badmatch, 11};
var_by_index(971) -> {case_end, 10};
var_by_index(972) -> {get_list, 10};
var_by_index(973) -> {get_list, 8};
var_by_index(974) -> {l_select_val2, 13};
var_by_index(975) -> {l_call_ext_only, 2};
var_by_index(976) -> {l_fetch, 22};
var_by_index(977) -> {l_call_ext, 104};
var_by_index(978) -> {l_call_ext, 103};
var_by_index(979) -> {l_call_ext, 102};
var_by_index(980) -> {l_call_ext, 101};
var_by_index(981) -> {l_call_ext, 100};
var_by_index(982) -> {l_call_ext, 99};
var_by_index(983) -> {l_is_eq_exact_immed, 32};
var_by_index(984) -> {get_tuple_element, 10};
var_by_index(985) -> {wait_timeout, 0};
var_by_index(986) -> {move_return, 22};
var_by_index(987) -> {badmatch, 12};
var_by_index(988) -> {is_nonempty_list, 29};
var_by_index(989) -> {l_times, 2};
var_by_index(990) -> {l_select_val2, 14};
var_by_index(991) -> {l_apply_fun, 0};
var_by_index(992) -> {init, 17};
var_by_index(993) -> {l_gc_bif1, 7};
var_by_index(994) -> {move, 12};
var_by_index(995) -> {l_is_eq_exact_immed, 33};
var_by_index(996) -> {l_bs_get_integer_32, 2};
var_by_index(997) -> {move_return, 24};
var_by_index(998) -> {move_return, 23};
var_by_index(999) -> {is_nil, 22};
var_by_index(1000) -> {badmatch, 13};
var_by_index(1001) -> {move_deallocate_return, 9};
var_by_index(1002) -> {l_new_bs_put_float_imm, 2};
var_by_index(1003) -> {l_call_ext_only, 3};
var_by_index(1004) -> {is_integer_allocate, 1};
var_by_index(1005) -> {is_tuple, 8};
var_by_index(1006) -> {is_tuple, 7};
var_by_index(1007) -> {l_is_eq_exact_immed, 34};
var_by_index(1008) -> {extract_next_element, 23};
var_by_index(1009) -> {move_jump, 10};
var_by_index(1010) -> {is_nil, 23};
var_by_index(1011) -> {bs_context_to_binary, 4};
var_by_index(1012) -> {badmatch, 14};
var_by_index(1013) -> {is_nonempty_list, 32};
var_by_index(1014) -> {is_nonempty_list, 31};
var_by_index(1015) -> {is_nonempty_list, 30};
var_by_index(1016) -> {get_list, 9};
var_by_index(1017) -> {l_bs_init_fail, 1};
var_by_index(1018) -> {l_move_call_ext, 43};
var_by_index(1019) -> {l_fast_element, 5};
var_by_index(1020) -> {l_bs_test_tail_imm2, 0};
var_by_index(1021) -> {move_return, 25};
var_by_index(1022) -> {is_nil, 24};
var_by_index(1023) -> {l_move_call_only, 9};
var_by_index(1024) -> {fconv, 2};
var_by_index(1025) -> {is_nonempty_list, 34};
var_by_index(1026) -> {is_nonempty_list, 33};
var_by_index(1027) -> {l_move_call_ext, 45};
var_by_index(1028) -> {init, 12};
var_by_index(1029) -> {test_heap_1_put_list, 4};
var_by_index(1030) -> {l_bs_add, 1};
var_by_index(1031) -> {l_wait_timeout, 0};
var_by_index(1032) -> {l_gc_bif2, 0};
var_by_index(1033) -> {try_end, 7};
var_by_index(1034) -> {l_bs_put_utf16, 0};
var_by_index(1035) -> {is_tuple, 9};
var_by_index(1036) -> {l_is_eq_exact_immed, 35};
var_by_index(1037) -> {extract_next_element, 25};
var_by_index(1038) -> {extract_next_element, 24};
var_by_index(1039) -> {move_jump, 11};
var_by_index(1040) -> {move_return, 27};
var_by_index(1041) -> {move_return, 26};
var_by_index(1042) -> {l_trim, 9};
var_by_index(1043) -> {is_nil, 25};
var_by_index(1044) -> {l_bs_validate_unicode, 0};
var_by_index(1045) -> {is_nonempty_list, 35};
var_by_index(1046) -> {l_jump_on_val, 1};
var_by_index(1047) -> {move, 11};
var_by_index(1048) -> {l_bs_utf16_size, 0};
var_by_index(1049) -> {l_bs_get_binary2, 2};
var_by_index(1050) -> {move_return, 29};
var_by_index(1051) -> {move_return, 28};
var_by_index(1052) -> {is_nil, 26};
var_by_index(1053) -> {raise, 1};
var_by_index(1054) -> {l_bs_restore2, 3};
var_by_index(1055) -> {l_int_bnot, 0};
var_by_index(1056) -> {l_bs_get_binary_imm2, 1};
var_by_index(1057) -> {is_bitstr, 0};
var_by_index(1058) -> {move_return, 35};
var_by_index(1059) -> {move_return, 34};
var_by_index(1060) -> {move_return, 33};
var_by_index(1061) -> {move_return, 32};
var_by_index(1062) -> {move_return, 31};
var_by_index(1063) -> {move_return, 30};
var_by_index(1064) -> {is_nil, 29};
var_by_index(1065) -> {is_nil, 28};
var_by_index(1066) -> {is_nil, 27};
var_by_index(1067) -> {l_new_bs_put_binary_all, 2};
var_by_index(1068) -> {l_new_bs_put_binary, 0};
var_by_index(1069) -> {badmatch, 16};
var_by_index(1070) -> {badmatch, 15};
var_by_index(1071) -> {is_nonempty_list, 37};
var_by_index(1072) -> {is_nonempty_list, 36};
var_by_index(1073) -> {l_bs_save2, 2};
var_by_index(1074) -> {fmove_2, 2};
var_by_index(1075) -> {is_reference, 0};
var_by_index(1076) -> {is_port, 0};
var_by_index(1077) -> {is_nonempty_list, 38};
var_by_index(1078) -> {is_number, 0};
var_by_index(1079) -> {l_select_tuple_arity, 4};
var_by_index(1080) -> {move, 13};
var_by_index(1081) -> {l_bs_get_binary_all_reuse, 1};
var_by_index(1082) -> {init, 13};
var_by_index(1083) -> {l_wait_timeout, 1};
var_by_index(1084) -> {l_trim, 10};
var_by_index(1085) -> {l_fnegate, 0};
var_by_index(1086) -> {l_bs_init, 0};
var_by_index(1087) -> {l_jump_on_val, 2};
var_by_index(1088) -> {init, 14};
var_by_index(1089) -> {l_bs_put_utf8, 0};
var_by_index(1090) -> {l_bs_utf8_size, 0};
var_by_index(1091) -> {l_bxor, 0};
var_by_index(1092) -> {l_new_bs_put_float, 0};
var_by_index(1093) -> {l_bs_validate_unicode_retract, 0};
var_by_index(1094) -> {l_apply_last, 0};
var_by_index(1095) -> {init, 16};
var_by_index(1096) -> {init, 15};
var_by_index(1097) -> {l_bs_get_binary_imm2, 2};
var_by_index(1098) -> {l_is_function2, 0};
var_by_index(1099) -> {l_gc_bif3, 0};
var_by_index(1100) -> {l_bs_match_string, 2};
var_by_index(1101) -> {l_bs_get_integer_imm, 1};
var_by_index(1102) -> {l_bor, 2};
var_by_index(1103) -> {l_new_bs_put_binary_imm, 0};
var_by_index(1104) -> {l_bs_get_integer_8, 2};
var_by_index(1105) -> {l_bs_start_match2, 3};
var_by_index(1106) -> {l_rem, 2};
var_by_index(1107) -> {l_bs_get_integer_small_imm, 1};
var_by_index(1108) -> {l_bsl, 2};
var_by_index(1109) -> {l_apply_only, 0};
var_by_index(1110) -> {on_load, 0};
var_by_index(1111) -> {move2, 10};
var_by_index(1112) -> {l_int_div, 2};
var_by_index(1113) -> {l_bs_test_unit, 0};
var_by_index(1114) -> {l_select_val_atoms, 3};
var_by_index(1115) -> {l_m_div, 0};
var_by_index(1116) -> {l_hibernate, 0};
var_by_index(1117) -> {l_apply_fun_last, 0};
var_by_index(1118) -> {is_function2, 0};
var_by_index(1119) -> {l_apply_fun_only, 0};
var_by_index(1120) -> {l_band, 2};
var_by_index(1121) -> {is_bigint, 0};
var_by_index(1122) -> {test_heap, 1};
var_by_index(1123) -> {func_info, 0};
var_by_index(1124) -> {call_bif, 0};
var_by_index(1125) -> {l_bs_get_utf16, 2};
var_by_index(1126) -> {l_put_tuple, 7};
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
var_by_index(1140) -> {l_select_val_smallints, 2};
var_by_index(1141) -> {l_select_tuple_arity2, 3};
var_by_index(1142) -> {init2, 1};
var_by_index(1143) -> {l_bs_get_binary_all2, 2};
var_by_index(1144) -> {is_nonempty_list_test_heap, 2};
var_by_index(1145) -> {allocate_heap_zero, 1};
var_by_index(1146) -> {l_bs_init_heap_bin, 1};
var_by_index(1147) -> {l_plus, 3};
var_by_index(1148) -> {l_bs_get_integer, 1};

var_by_index(Index) -> erlang:error({novarat,Index}).

%%EOF


