-module(tube_server).
-export([start_link/0]).

start_link() ->
	Me = xenstore:domid(),
	TipTop = "data/tip",
	ok = xenstore:mkdir(TipTop),
	ok = xenstore:set_perms(TipTop, [lc("b", Me)]),
	NockTop = "data/nock",
	ok = xenstore:mkdir(NockTop),
	ok = xenstore:set_perms(NockTop, [lc("r", Me)]),
	Pid =
	spawn_link(fun() -> ok = xenstore:watch(TipTop),
						loop(TipTop) end), {ok,Pid}.

loop(TipTop) -> receive {watch,WatchKey} ->
							watch_event(WatchKey, TipTop),
							loop(TipTop) end.

watch_event(WatchKey, TipTop) ->
	true = lists:prefix(TipTop, WatchKey),
	Suffix = lists:nthtail(length(TipTop), WatchKey),
	case string:tokens(Suffix, "/") of
		[X,Y,"nock"] ->
			{ok,NockDir} = xenstore:read(WatchKey),
			tube:accept(NockDir, lc([TipTop,"/",X,"/",Y]));
		_ -> ignore end.

lc(X)    -> lists:concat(X).
lc(X, Y) -> lists:concat([X,Y]).

