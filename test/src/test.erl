-module(test).
-export([run/1, run/2, play/0]).

score() -> [
%	{lists, all},
	{bif, [binary_to_atom]}
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
	io:format("Grand Total: ~p/~p\n", [Ok, Fail]).

run(Suite) ->
	run(Suite, all).

run(Suite, CaseList) ->
	Module = erlang:list_to_atom(lists:concat([Suite,"_SUITE"])),
	Groups = Module:groups(),
	Config = [{data_dir, lists:concat(["priv/",Module,"_data"])}],
	Module:init_per_suite(Config),

	CL =
		case CaseList of
			all -> Module:all();
			_ -> CaseList
		end,

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
		CL
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
