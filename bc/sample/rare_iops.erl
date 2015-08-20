-module(rare_iops).
-compile(export_all).

%%------------------------------------------------------------------------------
%%
%% Add this file to spec/otp_beams to take into account rarely used iops:
%%
%%	is_function2 (dynamic arity)
%%	l_bs_test_unit (when unit is not 8)
%%	system_limit
%%	l_gc_bif2, l_gc_bif3
%%
%%------------------------------------------------------------------------------

a(F, A) when is_function(F, A) -> ok.

b(<<_,_/binary-unit:16>>) -> ok.

c(<<_:4294967296,0,_/binary>>) -> ok. % 1 bsl 3

% gc_bif2, gc_bif3 
d(X, Y, Z) ->
	_ = erlang:binary_part(X, Y),
	erlang:binary_part(X, Y, Z).

% l_fnegate
e(X) when is_float(X) ->
	-X.

%%% update_map_assoc
f1(Map) -> Map#{a => foo}.
%%
%%% update_map_exact
f2(Map) -> Map#{a := foo}.
%%
%%% get_map_element
f3(#{a := A}) -> A.
%%
%%% get_map_elements
f4(#{a := A, b := B}) -> {A,B}.
%%
%%% has_map_field
f5(#{a := _}) -> yes.
%%
%%% has_map_fields
f6(#{a := _, b := _}) -> two.

%% l_bs_init_bits
g(X) -> <<127:7, X:64>>.
