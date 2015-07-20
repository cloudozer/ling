-module(tube_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("tube.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([enlist/3]).

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

enlist(Tube, StatePath, DataDir) ->
	gen_server:call(?SERVER, {enlist,Tube,StatePath,DataDir}).

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

handle_call(accept, _From, #ts{pend =[Tube|Pend]} =St) ->
	{reply,{ok,Tube},St#ts{pend =Pend}};

handle_call(accept, From, #ts{acc =Acc} =St) ->
	{noreply,St#ts{acc =[From|Acc]}};

handle_call({enlist,Tube,StatePath,DataDir}, _From, St) ->
	St1 = do_enlist(Tube, StatePath, DataDir, St),
	{reply,ok,St1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({watch,WatchKey}, St) ->
	St1 = watch_event(WatchKey, St),
    {noreply,St1};

handle_info(_Msg, St) ->
	io:format("tube_server: msg ~p\n", [_Msg]),
	{noreply,St}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_enlist(Tube, StatePath, DataDir, #ts{tubes =TI} =St) ->
	ok = xenstore:watch(StatePath),
	true = link(Tube),	%% catch exit signal
	St#ts{tubes =[{Tube,StatePath,DataDir}|TI]}.

watch_event(WatchKey, #ts{top =TipTop, tubes =TI} =St) ->
	case lists:prefix(TipTop, WatchKey) of
		true ->
			Suffix = lists:nthtail(length(TipTop), WatchKey),
			case string:tokens(Suffix, "/") of
				[X,Y,"nock"] ->
					{ok,NockDir} = xenstore:read(WatchKey),
					Domid = list_to_integer(X),
					knock_knock(Domid, NockDir, lc([TipTop,"/",X,"/",Y]), St);
				_ -> St end;
		false ->
			{value,{Tube,StatePath,DataDir},TI1} = lists:keytake(WatchKey, 2, TI),
			case xenstore:read(StatePath) of
				{ok,?STATE_CONNECTED} -> St;
				{error,eacces} ->
					ok = xenstore:unwatch(StatePath),
					true = unlink(Tube),
					true = port_close(Tube),
					?g("Port ~w closed\n", [Tube]),
					ok = xenstore:delete(DataDir),
					?g("Tube data ~s deleted\n", [DataDir]),
					Pend1 = lists:delete(Tube, St#ts.pend),
					St#ts{pend =Pend1,tubes =TI1} end end.

knock_knock(Domid, NockDir, TipDir, St) ->
	?g("Accepting tube request from ~w, nock = ~s\n", [Domid,NockDir]),
	NockState = lc(NockDir, "/state"),
	TipState = lc(TipDir, "/state"),
	ok = xenstore:write(TipState, ?STATE_INITIALISING),
	?g("Tip state = Initialising\n", []),
	case tube:wait_peer(NockState, ?STATE_INIT_WAIT) of
		{error,_} ->
			?g("Error while waiting for the nock (to start waiting)\n", []),
			ok = xenstore:delete(TipDir),
			?g("Tip data deleted\n", []),
			St;
		ok ->
			Tube = erlang:open_port(tube, ?TUBE_PORT_OPTS),
			?g("Port ~w open\n", [Tube]),
			Info = binary_to_list(<<Domid:32>>),
			[?TUBE_REP_OK|Data] = erlang:port_control(Tube, ?TUBE_REQ_OPEN, Info),
			<<RingRef:32,ChanTx:32,ChanRx:32>> = list_to_binary(Data),

			?g("ring-ref = ~w\n", [RingRef]),
			?g("event-channel-tx = ~w\n", [ChanTx]),
			?g("event-channel-rx = ~w\n", [ChanRx]),

			{ok,Tid} = xenstore:transaction(),
			ok = xenstore:write(lc(TipDir, "/ring-ref"), RingRef, Tid),
			ok = xenstore:write(lc(TipDir, "/event-channel-tx"), ChanTx, Tid),
			ok = xenstore:write(lc(TipDir, "/event-channel-rx"), ChanRx, Tid),
			ok = xenstore:write(TipState, ?STATE_INITIALISED, Tid),
			ok = xenstore:commit(Tid),
			?g("Tip state = Initialised\n", []),

			case tube:wait_peer(NockState, ?STATE_CONNECTED) of
				{error,_} ->
					?g("Error while waiting for the nock to connect\n", []),
					true = unlink(Tube),
					true = erlang:port_close(Tube),
					?g("Port ~w closed\n", [Tube]),
					ok = xenstore:delete(TipDir),
					?g("Tip data destroyed\n", []),
					St;
				ok ->
					?g("Nock connected\n", []),
					ok = xenstore:write(TipState, ?STATE_CONNECTED),
					?g("Tip state = Connected\n", []),
					St1 = do_enlist(Tube, NockState, TipDir, St),
					?g("Tube enlisted\n", []),
					accepted(Tube, St1) end end.

accepted(Tube, #ts{acc =[From|Acc]} =St) ->
	?g("Tube ~w accepted by ~w\n", [Tube,From]),
	gen_server:reply(From, {ok,Tube}),
	St#ts{acc =Acc};

accepted(Tube, #ts{pend =Pend} =St) ->
	St#ts{pend =[Tube|Pend]}.

lc(X) -> lists:concat(X).
lc(X, Y) -> lists:concat([X,Y]).

