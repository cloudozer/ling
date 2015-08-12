-module(test).
-export([run/1, run/2, play/0]).

score() -> [
	{exception,[pending_errors,exception_with_heap_frag]},
	{big, [big_literals]},
	{binary, [terms_float,deep]},
	{decode_packet, []},
	%{erl_lint,[]}, %% all tests fail
	{estone,[]},
	{lists, []},
	{tuple,[t_insert_element,t_delete_element]},
	{unicode,[exceptions,binaries_errors,random_lists]}, %% can't survive quad run
	{bif, [auto_imports,os_env]} %% can't survive double run
].

play() ->
	{Ok, Fail} =
		lists:foldl(
			fun({Suite, Cases}, {Ok, Fail}) ->
				{O, F} = run(Suite, Cases),
				{Ok + O, Fail + F}
			end,
			{0, 0},
			score()
		),
	io:format("Grand Total: ~p/~p\n", [Ok, Fail]),
	halt(Fail).

run(Suite) ->
	run(Suite, []).

run(Suite, Skip) ->
	Module = erlang:list_to_atom(lists:concat([Suite,"_SUITE"])),
	Groups = Module:groups(),
	Config = [{data_dir, lists:concat(["priv/",Module,"_data"])}],
	Module:init_per_suite(Config),

	{Ok, Fail} = lists:foldl(
		fun
			({group, Group}, {Ok, Fail}) ->
				{_, _, Cases} = lists:keyfind(Group, 1, Groups),
				Res = [exec(Module, C, Config) || C <- Cases],
				{
					Ok   + length([ R || R <- Res, R == ok  ]),
					Fail + length([ R || R <- Res, R == fail])
				};
			(Case, {Ok, Fail}) ->
				case exec(Module, Case, Config) of
					ok ->
						{Ok + 1, Fail};
					fail ->
						{Ok, Fail + 1}
				end			
		end,
		{0, 0},
		lists:subtract(Module:all(), Skip)
	),
	Module:end_per_suite(Config),
	io:format("Total: ~p, Ok: ~p, Failed: ~p\n", [Ok + Fail, Ok, Fail]),
	{Ok, Fail}.

exec(Module, Case, Config) ->
	try
		io:format("~p ", [Case]),
		Module:Case(Config),
		io:format("ok\n", []),
		ok
	catch
		_Class:Error ->
			io:format("~p\n~p\n", [Error, erlang:get_stacktrace()]),
			fail
	end.
