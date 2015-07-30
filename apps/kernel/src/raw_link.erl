-module(raw_link).

-export([getserv/1, getaddr/1]).
-export([open/1, open/2]).
-export([connect/3, send/2, send/4, recv/2, recv/3, close/1]).

-include("inet_int.hrl").

-define(RECBUF, (8*1024)).

getserv(Service) -> inet_udp:getserv(Service).
getaddr(Address) -> inet_udp:getaddr(Address).

open(Port) -> open(Port, []).

open(Port, Opts) ->
    case inet:udp_options(
       [{port,Port}, {recbuf, ?RECBUF} | Opts],
       raw) of
    {error, Reason} -> exit(Reason);
    {ok, #udp_opts{fd=Fd,
               ifaddr=BAddr,
               port=BPort,
               opts=SockOpts}} when ?port(BPort) ->
        inet:open(Fd,BAddr,BPort,SockOpts,udp,raw,link,?MODULE);
    {ok, _} -> exit(badarg)
    end.

send(S,{A,B,C,D},P,Data) when ?ip(A,B,C,D), ?port(P) ->
    prim_inet:sendto(S, {A,B,C,D}, P, Data).

send(S, Data) ->
    prim_inet:sendto(S, {0,0,0,0}, 0, Data).

connect(S, {A,B,C,D}, P) when ?ip(A,B,C,D), ?port(P) ->
    prim_inet:connect(S, {A,B,C,D}, P).

recv(S,Len) ->
    prim_inet:recvfrom(S, Len).

recv(S,Len,Time) ->
    prim_inet:recvfrom(S, Len, Time).

-spec close(inet:socket()) -> ok.
close(S) ->
    inet:udp_close(S).

