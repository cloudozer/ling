-module(tube_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("tube.hrl").

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

-record(ts, {top,pend =[],acc =[],tubes =[]}).

init(_Args) ->
	process_flag(trap_exit, true),
	Me = xenstore:domid(),

	TipTop = "data/tip",
	ok = xenstore:mkdir(TipTop),
	ok = xenstore:set_perms(TipTop, [lc("b", Me)]),

	NockTop = "data/nock",
	ok = xenstore:mkdir(NockTop),
	ok = xenstore:set_perms(NockTop, [lc("r", Me)]),

	xenstore:watch(TipTop),
	{ok,#ts{top =TipTop}}.

handle_call({open,Domid,Tid}, {Caller,_} =_From, St) ->
	Me = xenstore:domid(),
	NockDir = lc(["/local/domain/",Me,"/data/nock/",Domid,"/",Tid]),
	TipDir  = lc(["/local/domain/",Domid,"/data/tip/",Me,"/",Tid]),
	case xenstore:read(NockDir) of
		{ok,_} 	  -> {reply,{error,exists},St};
		{error,_} ->
			case xenstore:mkdir(TipDir) of
				ok -> do_open(Domid, NockDir, TipDir, Caller, St);
				_  -> {reply,{error,not_found},St} end end;

handle_call(accept, {Caller,_} =_From, #ts{pend =[{Tube,MyState}|Pend]} =St) ->
	port_connect(Tube, Caller),
	ok = xenstore:write(MyState, ?STATE_CONNECTED),
	{reply,{ok,Tube},St#ts{pend =Pend}};

handle_call(accept, From, #ts{acc =Acc} =St) ->
	{noreply,St#ts{acc =[From|Acc]}};

handle_call({close,Tube}, _From, St) ->
	St1 = close_tube(Tube, St),
	{reply,ok,St1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({watch,WatchKey}, #ts{top =TipTop} =St) ->
	case lists:prefix(TipTop, WatchKey) of
		true  -> Suffix = lists:nthtail(length(TipTop), WatchKey),
				 case string:tokens(Suffix, "/") of
					[X,Y,"nock"] ->
						%% peer wants to communicate
						{ok,NockDir} = xenstore:read(WatchKey),
						Domid = list_to_integer(X),
						knock_knock(Domid, NockDir, lc([TipTop,"/",X,"/",Y]), St);
					_ -> {noreply,St} end;
		false -> tube_state(WatchKey, St) end;

handle_info(_Msg, St) ->
	io:format("tube_server: msg ~p\n", [_Msg]),
	{noreply,St}.

terminate(_Reason, #ts{top =TipTop,tubes =Tubes}) ->
	lists:foreach(fun({_,StatePath,_}) -> ok = xenstore:unwatch(StatePath) end, Tubes),
	ok = xenstore:unwatch(TipTop).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_open(Domid, NockDir, TipDir, Caller, #ts{tubes =Tubes} =St) ->
	%% TipDir exists, NockDir does not
	{ok,Tid} = xenstore:transaction(),
	ok = xenstore:mkdir(NockDir, Tid),
	ok = xenstore:write(lc(NockDir, "/tip"), TipDir, Tid),
	ok = xenstore:write(lc(NockDir, "/state"), ?STATE_INIT_WAIT, Tid),
	ok = xenstore:write(lc(TipDir, "/nock"), NockDir, Tid), %% wakes up peer
	ok = xenstore:commit(Tid),
	TipState = lc(TipDir, "/state"),
	ok = xenstore:watch(TipState),
	case wait_peer(TipState, ?STATE_INITIALISED) of
		{error,_} =Error -> %% peer gone
			ok = xenstore:delete(NockDir),
			ok = xenstore:unwatch(TipState),
			{reply,Error,St};
		ok ->
			{ok,RingRef} = xenstore:read_integer(lc(TipDir, "/ring-ref")),
			{ok,ChanTx}  = xenstore:read_integer(lc(TipDir, "/event-channel-tx")),
			{ok,ChanRx}  = xenstore:read_integer(lc(TipDir, "/event-channel-rx")),
			Tube = erlang:open_port(tube, ?TUBE_PORT_OPTS),
			Info = binary_to_list(<<Domid:32,RingRef:32,ChanTx:32,ChanRx:32>>),
			[?TUBE_REP_OK] = erlang:port_control(Tube, ?TUBE_REQ_ATTACH, Info),
			true = port_connect(Tube, Caller),
			ok = xenstore:write(lc(NockDir, "/state"), ?STATE_CONNECTED),
			case wait_peer(TipState, ?STATE_CONNECTED) of
				{error,_} =Error ->
					true = port_connect(Tube, self()), %% spare the caller
					true = erlang:port_close(Tube),
					ok = xenstore:delete(NockDir),
					ok = xenstore:unwatch(TipState),
					{reply,Error,St};
				ok ->
					%% TipState is being watched
					TI = {Tube,TipState,NockDir},
					{reply,{ok,Tube},St#ts{tubes =[TI|Tubes]}} end end.

wait_peer(Path, Target) ->
	receive {watch,Path} ->
		case xenstore:read(Path) of
			{error,enoent} -> wait_peer(Path, Target);		%% ignore
			{ok,Target}	   -> ok;
			{ok,_}		   -> wait_peer(Path, Target);
			{error,_} =Err -> Err end end.

knock_knock(Domid, NockDir, TipDir, #ts{tubes =Tubes} =St) ->
	NockState = lc(NockDir, "/state"),
	ok = xenstore:watch(NockState),
	TipState = lc(TipDir, "/state"),
	ok = xenstore:write(TipState, ?STATE_INITIALISING),
	case wait_peer(NockState, ?STATE_INIT_WAIT) of
		{error,_} ->
			ok = xenstore:delete(TipDir),
			ok = xenstore:unwatch(NockState),
			{noreply,St};
		ok ->
			Tube = erlang:open_port(tube, ?TUBE_PORT_OPTS),
			Info = binary_to_list(<<Domid:32>>),
			[?TUBE_REP_OK|Data] = erlang:port_control(Tube, ?TUBE_REQ_OPEN, Info),
			<<RingRef:32,ChanTx:32,ChanRx:32>> = list_to_binary(Data),

			{ok,Tid} = xenstore:transaction(),
			ok = xenstore:write(lc(TipDir, "/ring-ref"), RingRef, Tid),
			ok = xenstore:write(lc(TipDir, "/event-channel-tx"), ChanTx, Tid),
			ok = xenstore:write(lc(TipDir, "/event-channel-rx"), ChanRx, Tid),
			ok = xenstore:write(TipState, ?STATE_INITIALISED, Tid),
			ok = xenstore:commit(Tid),
			ok = wait_peer(NockState, ?STATE_CONNECTED),

			TI = {Tube,NockState,TipDir},
			St1 = St#ts{tubes = [TI|Tubes]},

			case St1#ts.acc of
				[{Caller,_} =From|Acc1] ->
					port_connect(Tube, Caller),
					ok = xenstore:write(TipState, ?STATE_CONNECTED),
					gen_server:reply(From, {ok,Tube}),
					{noreply,St1#ts{acc =Acc1}};
				[] ->
					%% Tube not connected (temporarily connected to tube_server)
					{noreply,St1#ts{pend =[{Tube,TipState}|St#ts.pend]}} end end.

tube_state(WatchKey, #ts{tubes =Tubes} =St) ->
	{Tube,StatePath,_} = lists:keyfind(WatchKey, 2, Tubes),
	case xenstore:read(StatePath) of
		{ok,?STATE_CONNECTED} -> {noreply,St};
		_ -> St1 = close_tube(Tube, St), {noreply,St1} end.

close_tube(Tube, #ts{tubes =Tubes} =St) ->
	{value,{_,StatePath,DataDir},Tubes1} = lists:keytake(Tube, 1, Tubes),
	ok = xenstore:unwatch(StatePath),
	true = unlink(Tube),
	true = port_close(Tube),
	ok = xenstore:delete(DataDir),
	Pend1 = lists:delete(Tube, St#ts.pend),
	St#ts{pend =Pend1,tubes =Tubes1}.

lc(X) -> lists:concat(X).
lc(X, Y) -> lists:concat([X,Y]).

