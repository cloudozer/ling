-module(tube).
-export([open/1,open/2]).
-export([accept/0]).
-export([close/1]).

open(Domid) -> open(Domid, 0).
open(Domid, Tid) when is_integer(Domid), is_integer(Tid) ->
	gen_server:call(tube_server, {open,Domid,Tid});
open(_, _) -> {error,badarg}.

accept() -> gen_server:call(tube_server, accept, infinity).

close(Tube) when is_port(Tube) ->
	gen_server:close(tube_server, {close,Tube});
close(_) -> {error,badarg}.

