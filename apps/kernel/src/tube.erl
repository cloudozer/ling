-module(tube).
-export([open/1,open/2]).
-export([accept/0]).
-export([close/1]).

-export([wait_peer/2]).

-include("tube.hrl").

open(Domid) -> open(Domid, 0).
open(Domid, Tid) when is_integer(Domid), is_integer(Tid) ->
	Me = xenstore:domid(),
	?g("Opening tube ~w from ~w to ~w\n", [Tid,Me,Domid]),
	NockDir = lc(["/local/domain/",Me,"/data/nock/",Domid,"/",Tid]),
	TipDir  = lc(["/local/domain/",Domid,"/data/tip/",Me,"/",Tid]),
	case xenstore:read(NockDir) of
		{ok,_} -> {error,exists};
		_      -> case xenstore:mkdir(TipDir) of
					ok -> open1(Domid, NockDir, TipDir);
					_  -> {error,not_found} end end;
open(_, _) -> {error,badarg}.

open1(Domid, NockDir, TipDir) ->
	%% TipDir exists, NockDir does not
	ok = xenstore:mkdir(NockDir),
	ok = xenstore:write(lc(NockDir, "/tip"), TipDir),
	ok = xenstore:write(lc(NockDir, "/state"), ?STATE_INIT_WAIT),
	?g("Nock state = InitWait, waking up the tip\n", []),
	ok = xenstore:write(lc(TipDir, "/nock"), NockDir),	%% wake-up peer
	TipState = lc(TipDir, "/state"),
	case wait_peer(TipState, ?STATE_INITIALISED) of
		{error,_} =Error -> %% peer gone while waiting for a tube to initialise
			?g("Error while waiting for the tip to initialise\n", []),
			ok = xenstore:delete(NockDir),
			?g("Nock data deleted\n", []),
			Error;
		ok ->
			{ok,RingRef} = xenstore:read_integer(lc(TipDir, "/ring-ref")),
			{ok,ChanTx}  = xenstore:read_integer(lc(TipDir, "/event-channel-tx")),
			{ok,ChanRx}  = xenstore:read_integer(lc(TipDir, "/event-channel-rx")),

			?g("Tip: ring-ref = ~w\n", [RingRef]),
			?g("Tip: event-channel-tx = ~w\n", [ChanTx]),
			?g("Tip: event-channel-rx = ~w\n", [ChanRx]),

			Tube = erlang:open_port(tube, ?TUBE_PORT_OPTS),
			?g("Port ~w open\n", [Tube]),
			Info = binary_to_list(<<Domid:32,RingRef:32,ChanTx:32,ChanRx:32>>),
			[?TUBE_REP_OK] = erlang:port_control(Tube, ?TUBE_REQ_ATTACH, Info),
			?g("Port ~w attached successfully\n", [Tube]),

			ok = xenstore:write(lc(NockDir, "/state"), ?STATE_CONNECTED),
			case wait_peer(TipState, ?STATE_CONNECTED) of
				{error,_} =Error ->
					?g("Error while waiting for the tip to connect\n", []),
					true = unlink(Tube),
					true = erlang:port_close(Tube),
					?g("Port ~w closed\n", [Tube]),
					ok = xenstore:delete(NockDir),
					?g("Nock data destroyed\n", []),
					Error;
				ok ->
					?g("Tip connected\n", []),
					ok = tube_server:enlist(Tube, TipState, NockDir),
					?g("Tube ~w enlisted with tube_server\n", [Tube]),
					{ok,Tube} end end.

accept() -> case gen_server:call(tube_server, accept, infinity) of
				{ok,Tube} =X -> true = port_connect(Tube, self()), X;
					Error -> Error end.
					
close(_Tube) -> throw(todo).

%%state(?STATE_INITIALISING) -> "Initialising";
%%state(?STATE_INIT_WAIT)	 -> "InitWait";
%%state(?STATE_INITIALISED)  -> "Initialised";
%%state(?STATE_CONNECTED)	 -> "Connected";
%%state(?STATE_CLOSING)	     -> "Closing";
%%state(?STATE_CLOSED)	     -> "Closed".

wait_peer(Path, Target) ->
	ok = xenstore:watch(Path),
	Result = wait_peer1(Path, Target),
	ok = xenstore:unwatch(Path),
	Result.

wait_peer1(Path, Target) ->
	receive {watch,Path} ->
		case xenstore:read(Path) of
			{error,enoent} -> wait_peer1(Path, Target);		%% ignore
			{ok,Target}	   -> ok;
			{ok,_}		   -> wait_peer1(Path, Target);
			{error,_} =Err -> Err end end.

lc(X)    -> lists:concat(X).
lc(X, Y) -> lists:concat([X,Y]).

