-module(strawman).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(NUM_STRAW_REFS, 4).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(sm, {top}).

init(Args) ->
	process_flag(trap_exit, true),
	Me = xenstore:domid(),
	StrawTop = "data/straw",
	ok = xenstore:mkdir(StrawTop),
	ok = xenstore:set_perms(StrawTop, [lc("b", Me)]),
	WartsTop = "data/warts",
	ok = xenstore:mkdir(WartsTop),
	ok = xenstore:set_perms(WartsTop, [lc("r", Me)]),
	ok = xenstore:watch(StrawTop),
	{ok,#sm{top =StrawTop}}.

handle_call({open,Domid}, _From, St) ->
	Me = xenstore:domid(),
	WartsDir = lc(["/local/domain/",Me,"/data/warts",Domid]),
	StrawDir = lc(["/local/domain/",Domid,"/data/straw",Me]),
	case xenstore:read(WartsDir) of
		{ok,_}	  -> {reply,{error,exists},St};
		{error,_} ->
			case xenstore:mkdir(StrawDir) of
				ok -> do_open(Domid, WartsDir, StrawDir, St);
				_  -> {reply,{error,not_found},St} end end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({watch,WatchKey}, #sm{top =StrawTop} =St) ->
	case lists:prefix(StrawTop, WatchKey) of
		true  -> Suffux = lists:nthtail(length(StrawTop), WatchKey),
				 case string:tokens(Suffix, "/") of
					[X,"warts"] ->
						%% peer wants to communicate
						{ok,WartsDir} = xenstore:read(WatchKey),
						Domid = list_to_integer(X),
						knock_knock(Domid, WartsDir, lc([StrawTop,"/",X]), St);
					_ -> {noreply,St} end;
		false -> straw_state(WatchKey, St) end;

handle_info(_Mgs, St) ->
	io:format("strawman: msg ~p\n", [_Msg]),
	{noreply,St}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_open(Domid, WartsDir, StrawDir, #sm{straws =Straws} =St) ->
	%% StrawDir exists, WartsDir does not
	{ok,Tid} = xenstore:transaction(),
	ok = xenstore:mkdir(WartsDir, Tid),
	ok = xenstore:write(lc(WartsDir, "/straw"), StrawDir, Tid),
	ok = xenstore:write(lc(WartsDir, "/state"), ?STATE_INIT_WAIT, Tid),
	ok = xenstore:write(lc(StrawDir, "/warts", WartsDir, Tid), %% wakes up peer
	ok = xenstore:commit(Tid),
	StrawState = lc(StrawDir, "/state"),
	ok = xenstore:watch(StrawState),
	case wait_peer(StrawState, ?STATE_INITIALISED) of
		{error,_} =Error -> %% peer gone
			ok = xenstore:delete(WartsDir),
			ok = xenstore:unwatch(StrawState),
			{reply,Error,St};
		ok ->
			InRefs =
			lists:map(fun(N) -> {ok,Ref} = xenstore:read_integer(lc([StrawDir,"/in-ref-",N])),
								Ref end, lists:seq(1, ?NUM_STRAW_REFS)),
			OutRefs =
			lists:map(fun(N) -> {ok,Ref} = xenstore:read_integer(lc([StrawDir,"/out-ref-",N])),
								Ref end, lists:seq(1, ?NUM_STRAW_REFS)),
			{ok,Channel} = xenstore:read_integer(lc(StrawDir, "/event-channel")),
			StrawPore = pore_straw:open(Domid, InRefs, OutRefs, Channel),
			ok = xenstore:write(lc(WartsDir, "/state"), ?STATE_CONNECTED),
			case wait_peer(StrawState, ?STATE_CONNECTED) of
				{error,_} =Error ->
					true = pore:close(StrawPore),
					ok = xenstore:delete(WartsDir),
					ok = xenstore:unwatch(StrawState),
					{reply,Error,St};
				ok ->
					%% StrawState is being watched
					SI = {StrawPore,StrawState,WartsDir},
					{reply,{ok,StrawPore},St#sm{straws =[SI|Straws]}} end end.

knock_knock(Domid, WartsDir, StrawDir, #sm{straws =Straws} =St) ->
	WartsState = lc(WartsDir, "/state"),
	ok = xenstore:watch(WartsState),
	StrawState = lc(StrawDir, "/state"),
	ok = xenstore:write(StrawState, ?STATE_INITIALISING),
	case wait_peer(WartsState, ?STATE_INIT_WAIT) of
		{error,_} ->
			ok = xenstore:delete(StrawDir),
			ok = xenstore:unwatch(WartsState),
			{noreply,St};
		ok ->
			StrawPore = pore_straw:open(),
			InRefs  = pore_straw:refs(0),
			OutRefs = pore_straw:refs(1),
			Channel = pore_straw:channel(),
			
			{ok,Tid} = xenstore:transaction(),
			lists:foreach(fun({N,Ref}) -> ok = xenstore:write(lc([StrawDir,"/in-ref-",N]), Ref, Tid) end,
							lists:zip(lists:seq(1, ?NUM_STRAW_REFS), InRefs)),
			lists:foreach(fun({N,Ref}) -> ok = xenstore:write(lc([StrawDir,"/out-ref-",N]), Ref, Tid) end,
							lists:zip(lists:seq(1, ?NUM_STRAW_REFS), OutRefs)),
			ok = xenstore:write(lc(StrawDir, "/event-channel"), Channel, Tid),
			ok = xenstore:write(StrawState, ?STATE_INITIALISED, Tid),
			ok = xenstore:commit(Tid),
			ok = wait_peer(WartsState, ?STATE_CONNECTED),

			SI = {StrawPore,WartsState,StrawDir},
			St1 = St#sm{straws = [SI|Straws]},

			%%TODO

			{noreply,St1} end end.

straw_state(WatchKey, #sm{straws =Straws} =St) ->
	{StrawPort,StatePath,_} = lists:keyfind(WatchKey, 2, Straws),
	case xenstore:read(StatePath) of
		{ok,?STATE_CONNECTED} -> {noreply,St};
		_ -> St1 = close_straw(StraPore, St), {noreply,St1} end.

close_straw(StrawPore, #sm{straws =Straws} =St) ->
	{value,{_,StatePath,DataDir},Straws1} = lists:keytake(StrawPore, 1, Straws),
	ok = xenstore:unwatch(StatePath),
	true = pore:close(StrawPore),
	ok = xenstore:delete(DataDir),
	St#sm{straws =Straws1}.

%%TODO: the function is the same as in tube_server.erl
wait_peer(Path, Target) ->
	receive {watch,Path} ->
		case xenstore:read(Path) of
			{error,enoent} -> wait_peer(Path, Target);		%% ignore
			{ok,Target}	   -> ok;
			{ok,_}		   -> wait_peer(Path, Target);
			{error,_} =Err -> Err end end.

lc(X) -> lists:concat(X).
lc(X, Y) -> lists:concat([X,Y]).

