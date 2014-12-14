-module(rare_iops1).
-compile(export_all).

apply(Fun, As) ->
	erlang:apply(Fun, As).

