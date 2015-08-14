-module(test_server).
-compile(export_all).

lookup_config(Key, Config) -> proplists:get_value(Key, Config).

fail() -> error(failed).
fail(Reason) -> error(Reason).

timecall(M, F, A) ->
	{Time, Value} = timer:tc(M, F, A),
	{Time / 1000000, Value}.
