-module(strawman).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([short_straw/2,short_straw/4]).

-include("xenstore.hrl").

-define(NUM_STRAW_REFS, 8).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([open/1]).
-export([split/1]).

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

open(Domid) ->
	gen_server:call(?SERVER, {open,Domid}).

split(Domid) ->
	gen_server:call(?SERVER, {split,Domid}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(sm, {top,straws =[]}).

init(_Args) ->
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
	WartsDir = lc(["/local/domain/",Me,"/data/warts/",Domid]),
	StrawDir = lc(["/local/domain/",Domid,"/data/straw/",Me]),
	case xenstore:read(WartsDir) of
		{ok,_}	  -> {reply,{error,exists},St};
		{error,_} ->
			case xenstore:mkdir(StrawDir) of
				ok -> do_open(Domid, WartsDir, StrawDir, St);
				_  -> {reply,{error,not_found},St} end end;

handle_call({split,Domid}, _From, #sm{straws =Straws} =St) ->
	case lists:keyfind(Domid, 1, Straws) of
		{_,StrawProc,_,_} -> {reply,{ok,StrawProc},St};
		false -> {reply,{error,not_found},St} end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({watch,WatchKey}, #sm{top =StrawTop} =St) ->
	case lists:prefix(StrawTop, WatchKey) of
		true  -> Suffix = lists:nthtail(length(StrawTop), WatchKey),
				 case string:tokens(Suffix, "/") of
					[X,"warts"] ->
						%% peer wants to communicate
						{ok,WartsDir} = xenstore:read(WatchKey),
						Domid = list_to_integer(X),
						knock_knock(Domid, WartsDir, lc([StrawTop,"/",X]), St);
					_ -> {noreply,St} end;
		false -> straw_state(WatchKey, St) end;

handle_info(_Msg, St) ->
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
	ok = xenstore:write(lc(StrawDir, "/warts"), WartsDir, Tid), %% wakes up peer
	ok = xenstore:commit(Tid),
	erlang:display({state,'INIT_WAIT'}),
	StrawState = lc(StrawDir, "/state"),
	ok = xenstore:watch(StrawState),
	erlang:display({wait,'INITIALISED'}),
	case xenstore:wait(StrawState, ?STATE_INITIALISED) of
		{error,_} =Error -> %% peer gone
			ok = xenstore:delete(WartsDir),
			ok = xenstore:unwatch(StrawState),
			{reply,Error,St};
		ok ->
			erlang:display(done_waiting),
			Refs =
			lists:map(fun(N) -> {ok,Ref} = xenstore:read_integer(lc([StrawDir,"/ring-ref-",N])),
								Ref end, lists:seq(1, ?NUM_STRAW_REFS)),
			{ok,Channel} = xenstore:read_integer(lc(StrawDir, "/event-channel")),
			StrawProc = spawn_link(?MODULE, short_straw, [self(),Domid,Refs,Channel]),
			receive {ready,StrawProc} -> ok end,
			ok = xenstore:write(lc(WartsDir, "/state"), ?STATE_CONNECTED),
			erlang:display({wait,'CONNECTED'}),
			case xenstore:wait(StrawState, ?STATE_CONNECTED) of
				{error,_} =Error ->
					exit(StrawProc),
					ok = xenstore:delete(WartsDir),
					ok = xenstore:unwatch(StrawState),
					{reply,Error,St};
				ok ->
					erlang:display(done_waiting),
					%% StrawState is being watched
					SI = {Domid,StrawProc,StrawState,WartsDir},
					{reply,ok,St#sm{straws =[SI|Straws]}} end end.

knock_knock(Domid, WartsDir, StrawDir, #sm{straws =Straws} =St) ->
	StrawState = lc(StrawDir, "/state"),
	ok = xenstore:write(StrawState, ?STATE_INITIALISING),
	erlang:display({state,'INITIALISING'}),
	WartsState = lc(WartsDir, "/state"),
	ok = xenstore:watch(WartsState),
	erlang:display({wait,'INIT_WAIT'}),
	case xenstore:wait(WartsState, ?STATE_INIT_WAIT) of
		{error,_} ->
			ok = xenstore:delete(StrawDir),
			ok = xenstore:unwatch(WartsState),
			{noreply,St};
		ok ->
			erlang:display(done_waiting),
			StrawProc = spawn_link(?MODULE, short_straw, [self(),Domid]),
			receive {ready,StrawProc,Refs,Channel} -> ok end,
			erlang:display({ready,StrawProc,Refs,Channel}),
			{ok,Tid} = xenstore:transaction(),
			lists:foreach(fun({N,Ref}) -> ok = xenstore:write(lc([StrawDir,"/ring-ref-",N]), Ref, Tid) end,
							lists:zip(lists:seq(1, ?NUM_STRAW_REFS), Refs)),
			ok = xenstore:write(lc(StrawDir, "/event-channel"), Channel, Tid),
			ok = xenstore:write(StrawState, ?STATE_INITIALISED, Tid),
			ok = xenstore:commit(Tid),
			erlang:display({state,'INITIAIALISED'}),
			erlang:display({wait,'CONNECTED'}),
			ok = xenstore:wait(WartsState, ?STATE_CONNECTED),
			erlang:display(done_waiting),
			ok = xenstore:write(StrawState, ?STATE_CONNECTED),
			erlang:display({state,'CONNECTED'}),
			SI = {Domid,StrawProc,WartsState,StrawDir},
			St1 = St#sm{straws = [SI|Straws]},
			{noreply,St1} end.

straw_state(WatchKey, #sm{straws =Straws} =St) ->
	{_,StrawProc,StatePath,_} = lists:keyfind(WatchKey, 3, Straws),
	case xenstore:read(StatePath) of
		{ok,?STATE_CONNECTED} -> {noreply,St};
		_ -> close_straw(StrawProc, St) end.

close_straw(StrawProc, #sm{straws =Straws} =St) ->
	{value,{_,_,StatePath,DataDir},Straws1} = lists:keytake(StrawProc, 2, Straws),
	ok = xenstore:unwatch(StatePath),
	exit(StrawProc),
	ok = xenstore:delete(DataDir),
	{noreply,St#sm{straws =Straws1}}.

short_straw(ReplyTo, Domid, Refs, Channel) ->
	Pore = pore_straw:open(Domid, Refs, Channel),
	erlang:display({pore_open,Pore}),
	ReplyTo ! {ready,self()},
	looper(Pore).

short_straw(ReplyTo, Domid) ->
	Pore = pore_straw:open(Domid),
	erlang:display({pore_open,Pore}),
	{Refs,Channel} = pore_straw:info(Pore),
	erlang:display({pore_info,Refs,Channel}),
	ReplyTo ! {ready,self(),Refs,Channel},
	looper(Pore).

looper(Pore) ->
	{IA,OA} = pore_straw:avail(Pore),
	looper(Pore, IA, OA, undefined, [], 0, [], 0).

looper(Pore, _IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz) when OutSz > 0, OA > 0 ->
	{Chip,OutBuf1,OutSz1} = chip(OA, OutBuf, OutSz),
	erlang:display({straw_write,Chip}),
	ok = pore_straw:write(Pore, Chip),
	true = pore:poke(Pore),
	{IA1,OA1} = pore_straw:avail(Pore),
	erlang:display({avail,IA1,OA1}),
	looper(Pore, IA1, OA1, ExpSz, InBuf, InSz, OutBuf1, OutSz1);

looper(Pore, IA, OA, undefined, InBuf, InSz, OutBuf, OutSz) when InSz >= 4 ->
	{<<ExpSz:32>>,InBuf1,InSz1} = chip(4, InBuf, InSz),
	looper(Pore, IA, OA, ExpSz, InBuf1, InSz1, OutBuf, OutSz);

looper(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz) when ExpSz =/= undefined, InSz >= ExpSz ->
	{Chip,InBuf1,InSz1} = chip(ExpSz, InBuf, InSz),
	deliver(binary_to_term(Chip)),
	looper(Pore, IA, OA, ExpSz, InBuf1, InSz1, OutBuf, OutSz);

looper(Pore, IA, _OA, ExpSz, InBuf, InSz, OutBuf, OutSz) when IA > 0 ->
	Data = pore_straw:read(Pore),
	erlang:display({straw_read,Data}),
	true = pore:poke(Pore),
	{IA1,OA1} = pore_straw:avail(Pore),
	erlang:display({avail,IA1,OA1}),
	looper(Pore, IA1, OA1, ExpSz, [InBuf,Data], InSz+iolist_size(Data), OutBuf, OutSz);

looper(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz) ->
	receive
	{envelope,_,_} =Envelope ->
		EnvBin = term_to_binary(Envelope),
		Sz = byte_size(EnvBin),
		OutBuf1 = [OutBuf,<<Sz:32>>,EnvBin],
		OutSz1 = OutSz + 4 + Sz,
		looper(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf1, OutSz1);
		
	{irq,Pore} ->
		{IA1,OA1} = pore_straw:avail(Pore),
		erlang:display({irq,IA1,OA1}),
		looper(Pore, IA1, OA1, ExpSz, InBuf, InSz, OutBuf, OutSz) end.

deliver({envelope,Addressee,Message}) -> Addressee ! Message.

chip(N, Buf, Sz) when Sz =< N -> {iolist_to_binary(Buf),[],0};
chip(N, Buf, Sz) when is_binary(Buf) ->
	<<Chip:(N)/binary,Buf1/binary>> =Buf,
	{Chip,Buf1,Sz-N};
chip(N, Buf, Sz) -> chip(N, iolist_to_binary(Buf), Sz).

lc(X) -> lists:concat(X).
lc(X, Y) -> lists:concat([X,Y]).

