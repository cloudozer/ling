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
	case lists:keyfind(Domid, 2, Straws) of
		{_,_,StrawProc,_,_} -> {reply,{ok,StrawProc},St};
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

handle_info({'EXIT',_,peer_closed}, St) -> {noreply,St};
handle_info(_Msg, St) -> erlang:display({strawman,msg,_Msg}), {noreply,St}.

terminate(shutdown, #sm{straws =Straws}) ->
	ok = close_straws(Straws).

code_change(_OldVsn, St, _Extra) -> {ok,St}.

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
	StrawState = lc(StrawDir, "/state"),
	ok = xenstore:watch(StrawState),
	case xenstore:wait(StrawState, ?STATE_INITIALISED) of
		{error,_} =Error -> %% peer gone
			ok = xenstore:delete(WartsDir),
			ok = xenstore:unwatch(StrawState),
			{reply,Error,St};
		ok ->
			Refs =
			lists:map(fun(N) -> {ok,Ref} = xenstore:read_integer(lc([StrawDir,"/ring-ref-",N])),
								Ref end, lists:seq(1, ?NUM_STRAW_REFS)),
			{ok,Channel} = xenstore:read_integer(lc(StrawDir, "/event-channel")),
			StrawProc = spawn_link(?MODULE, short_straw, [self(),Domid,Refs,Channel]),
			receive {ready,StrawProc} -> ok end,
			ok = xenstore:write(lc(WartsDir, "/state"), ?STATE_CONNECTED),
			case xenstore:wait(StrawState, ?STATE_CONNECTED) of
				{error,_} =Error ->
					ok = xenstore:delete(WartsDir),
					ok = xenstore:unwatch(StrawState),
					exit(StrawProc, peer_closed),
					{reply,Error,St};
				ok ->
					%% StrawState is being watched
					SI = {passive,Domid,StrawProc,StrawState,WartsDir},
					{reply,ok,St#sm{straws =[SI|Straws]}} end end.

knock_knock(Domid, WartsDir, StrawDir, #sm{straws =Straws} =St) ->
	StrawState = lc(StrawDir, "/state"),
	ok = xenstore:write(StrawState, ?STATE_INITIALISING),
	WartsState = lc(WartsDir, "/state"),
	ok = xenstore:watch(WartsState),
	case xenstore:wait(WartsState, ?STATE_INIT_WAIT) of
		{error,_} ->
			ok = xenstore:delete(StrawDir),
			ok = xenstore:unwatch(WartsState),
			{noreply,St};
		ok ->
			StrawProc = spawn_link(?MODULE, short_straw, [self(),Domid]),
			receive {ready,StrawProc,Refs,Channel} -> ok end,
			{ok,Tid} = xenstore:transaction(),
			lists:foreach(fun({N,Ref}) -> ok = xenstore:write(lc([StrawDir,"/ring-ref-",N]), Ref, Tid) end,
							lists:zip(lists:seq(1, ?NUM_STRAW_REFS), Refs)),
			ok = xenstore:write(lc(StrawDir, "/event-channel"), Channel, Tid),
			ok = xenstore:write(StrawState, ?STATE_INITIALISED, Tid),
			ok = xenstore:commit(Tid),
			ok = xenstore:wait(WartsState, ?STATE_CONNECTED),
			ok = xenstore:write(StrawState, ?STATE_CONNECTED),
			SI = {active,Domid,StrawProc,WartsState,StrawDir},
			St1 = St#sm{straws = [SI|Straws]},
			{noreply,St1} end.

%%--------------------------------------
%% Active					Passive
%% ======					=======
%% state=CLOSING
%%							unmap refs
%%							state=CLOSED
%%							wait=CLOSED
%% wait=CLOSED
%% end access to refs
%% state=CLOSED
%%--------------------------------------
%% Active					Passive
%% ======					=======
%%							unmap refs
%%							state=CLOSED
%%							wait=CLOSED
%% wait=CLOSED
%%--------------------------------------

straw_state(WatchKey, #sm{straws =Straws} =St) ->
	SI = lists:keyfind(WatchKey, 4, Straws),
	straw_state1(SI, St).

straw_state1(false, St) -> {noreply,St};
straw_state1({_,_,_,StatePath,_} =SI, St) ->
	straw_state1(xenstore:read(StatePath), SI, St).

straw_state1({ok,?STATE_CONNECTED}, _, St) -> {noreply,St};
straw_state1({ok,_}, {active,_,_,_,_}, St) -> {noreply,St};	%% see chart above
straw_state1(_, {_,Domid,StrawProc,StatePath,DataDir}, #sm{straws =Straws} =St) ->
	ok = xenstore:unwatch(StatePath),
	exit(StrawProc, peer_closed),
	ok = xenstore:delete(DataDir),
	io:format("strawman: connection to domain ~w lost\n", [Domid]),
	Straws1 = lists:keydelete(StrawProc, 3, Straws),
	{noreply,St#sm{straws =Straws1}}.

close_straws([]) -> ok;
close_straws([{Mode,Domid,StrawProc,StatePath,DataDir}|Straws]) ->
	if Mode =:= active ->
		ok = xenstore:delete(DataDir),
		xenstore:watch(StatePath, ?STATE_CLOSED);
			true -> ok end,
	exit(StrawProc, shutdown),
	io:format("strawman: connection to domain ~w closed\n", [Domid]),
	close_straws(Straws).

short_straw(ReplyTo, Domid, Refs, Channel) ->
	Pore = pore_straw:open(Domid, Refs, Channel),
	ReplyTo ! {ready,self()},
	looper(Pore).

short_straw(ReplyTo, Domid) ->
	Pore = pore_straw:open(Domid),
	{Refs,Channel} = pore_straw:info(Pore),
	ReplyTo ! {ready,self(),Refs,Channel},
	looper(Pore).

looper(Pore) ->
	{IA,OA} = pore_straw:avail(Pore),
	looper(Pore, IA, OA, undefined, [], 0, [], 0).

looper(Pore, _IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz) when OutSz > 0, OA > 0 ->
	{Chip,OutBuf1,OutSz1} = chip(OA, OutBuf, OutSz),
	ok = pore_straw:write(Pore, Chip),
	true = pore:poke(Pore),
	{IA1,OA1} = pore_straw:avail(Pore),
	looper(Pore, IA1, OA1, ExpSz, InBuf, InSz, OutBuf1, OutSz1);

looper(Pore, IA, OA, undefined, InBuf, InSz, OutBuf, OutSz) when InSz >= 4 ->
	{<<ExpSz:32>>,InBuf1,InSz1} = chip(4, InBuf, InSz),
	looper(Pore, IA, OA, ExpSz, InBuf1, InSz1, OutBuf, OutSz);

looper(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz) when ExpSz =/= undefined, InSz >= ExpSz ->
	{Chip,InBuf1,InSz1} = chip(ExpSz, InBuf, InSz),
	deliver(binary_to_term(Chip)),
	looper(Pore, IA, OA, undefined, InBuf1, InSz1, OutBuf, OutSz);

looper(Pore, IA, _OA, ExpSz, InBuf, InSz, OutBuf, OutSz) when IA > 0 ->
	Data = pore_straw:read(Pore),
	true = pore:poke(Pore),
	{IA1,OA1} = pore_straw:avail(Pore),
	looper(Pore, IA1, OA1, ExpSz, [InBuf,Data], InSz+iolist_size(Data), OutBuf, OutSz);

looper(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz) ->
	receive
	{envelope,_,_} =Envelope ->
		EnvBin = term_to_binary(Envelope),
		Sz = byte_size(EnvBin),
		OutBuf1 = [OutBuf,<<Sz:32>>,EnvBin],
		OutSz1 = OutSz + 4 + Sz,
		io:format("Stuffing an envelope (~w bytes) for delivery...\n", [Sz]),
		looper(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf1, OutSz1);
		
	{irq,Pore} ->
		{IA1,OA1} = pore_straw:avail(Pore),
		looper(Pore, IA1, OA1, ExpSz, InBuf, InSz, OutBuf, OutSz) end.

deliver({envelope,Addressee,Message}) ->
	Addressee ! Message.

chip(N, Buf, Sz) when Sz =< N -> {iolist_to_binary(Buf),[],0};
chip(N, Buf, Sz) when is_binary(Buf) ->
	<<Chip:(N)/binary,Buf1/binary>> =Buf,
	{Chip,Buf1,Sz-N};
chip(N, Buf, Sz) -> chip(N, iolist_to_binary(Buf), Sz).

lc(X) -> lists:concat(X).
lc(X, Y) -> lists:concat([X,Y]).

