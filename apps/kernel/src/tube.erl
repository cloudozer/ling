-module(tube).
-export([open/1,open/2]).
-export([accept/0]).
-export([can_send/1]).
-export([close/1]).

-include("tube.hrl").

open(Domid) -> open(Domid, 0).
open(Domid, Tid) when is_integer(Domid), is_integer(Tid) ->
	gen_server:call(tube_server, {open,Domid,Tid});
open(_, _) -> {error,badarg}.

accept() -> gen_server:call(tube_server, accept, infinity).

%% Blocks until at least one send slot is available
can_send(Tube) when is_port(Tube) ->
	[?TUBE_REP_OK] = erlang:port_control(Tube, ?TUBE_REQ_SEND_SLOTS, []),
	receive {can_send,Tube,closed}   -> {error,closed};
			{can_send,Tube,NumSlots} -> {ok,NumSlots} end;
can_send(_) -> {error,badarg}.

close(Tube) when is_port(Tube) ->
	gen_server:close(tube_server, {close,Tube});
close(_) -> {error,badarg}.

