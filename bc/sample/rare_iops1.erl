-module(rare_iops1).
-compile(export_all).

apply(Fun, As) ->
	erlang:apply(Fun, As).

foo(16#1234567800) -> big1;
foo(16#1234567801) -> big2;
foo(16#1234567802) -> big3.

bar(A) -> A bxor 2.

