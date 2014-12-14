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

%%EOF
