%% Copyright (c) 2013-2014 Cloudozer LLP. All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%% 
%% * Redistributions of source code must retain the above copyright notice, this
%% list of conditions and the following disclaimer.
%% 
%% * Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
%% 
%% * Redistributions in any form must be accompanied by information on how to
%% obtain complete source code for the LING software and any accompanying
%% software that uses the LING software. The source code must either be included
%% in the distribution or be available for no more than the cost of distribution
%% plus a nominal fee, and must be freely redistributable under reasonable
%% conditions.  For an executable file, complete source code means the source
%% code for all modules it contains. It does not include source code for modules
%% or files that typically accompany the major components of the operating
%% system on which the executable file runs.
%% 
%% THIS SOFTWARE IS PROVIDED BY CLOUDOZER LLP ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE
%% DISCLAIMED. IN NO EVENT SHALL CLOUDOZER LLP BE LIABLE FOR ANY DIRECT,
%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(xenstore).
-define(SERVER, ?MODULE).

-include("xenstore.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([read/1,read_integer/1,write/2,write/3,mkdir/1,mkdir/2,delete/1,delete/2,list/1]).
-export([get_perms/1,set_perms/2,set_perms/3,watch/1,unwatch/1]).
-export([transaction/0,commit/1,rollback/1]).
-export([domid/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	Pid = spawn_link(fun() -> XS = pore_xs:open(),
							  looper(XS) end),
	
	spawn(fun() -> link(Pid),
				   process_flag(trap_exit, true),
				   receive X -> io:format("MONITOR: ~p\n", [X]) end end),

	register(?SERVER, Pid),
	{ok,Pid}.

read(Key) when is_list(Key) ->
	call({self(),?XS_READ,Key,0});
read(_) -> {error,badarg}.

read_integer(Key) ->
	case read(Key) of {ok,S} -> try {ok,list_to_integer(S)}
							    catch _:_ -> {error,not_integer} end;
					  Error  -> Error end.

write(Key, Value) -> write(Key, Value, 0).
write(Key, Value, Tid) when is_integer(Value) ->
	write(Key, integer_to_list(Value), Tid);
write(Key, Value, Tid) when is_list(Key), is_list(Value), is_integer(Tid) ->
	call({self(),?XS_WRITE,[Key,Value],Tid});
write(_, _, _) -> {error,badarg}.

mkdir(Path) -> mkdir(Path, 0).
mkdir(Path, Tid) when is_list(Path), is_integer(Tid) ->
	call({self(),?XS_MKDIR,Path,Tid});
mkdir(_, _) -> {error,badarg}.

delete(Path) -> delete(Path, 0).
delete(Path, Tid) when is_list(Path), is_integer(Tid) ->
	call({self(),?XS_RM,Path,Tid});
delete(_, _) -> {error,badarg}.

list(Path) when is_list(Path) ->
	call({self(),?XS_DIRECTORY,Path,0});
list(_) -> {error,badarg}.

get_perms(Path) when is_list(Path) ->
	call({self(),?XS_GET_PERMS,Path,0});
get_perms(_) -> {error,badarg}.

set_perms(Path, Perms) -> set_perms(Path, Perms, 0).
set_perms(Path, [X|_] =Perms, Tid) when is_list(Path), is_list(Perms),
										is_list(X), is_integer(Tid) ->
	call({self(),?XS_SET_PERMS,[Path|Perms],Tid});
set_perms(_, _, _) -> {error,badarg}.

watch(Path) when is_list(Path) ->
	call({self(),?XS_WATCH,[Path,token()],0});
watch(_) -> {error,badarg}.

unwatch(Path) when is_list(Path) ->
	{ok,Tokens} = call({self(),unwatch,Path}),
	lists:foreach(fun(Token) -> call({self(),?XS_UNWATCH,[Path,Token],0}) end, Tokens);
unwatch(_) -> {error,badarg}.

transaction() ->
	call({self(),?XS_TRANSACTION_START,[],0}).

commit(Tid) when is_integer(Tid) ->
	call({self(),?XS_TRANSACTION_END,"T",Tid});
commit(_) -> {error,badarg}.

rollback(Tid) when is_integer(Tid) ->
	call({self(),?XS_TRANSACTION_END,"F",Tid});
rollback(_) -> {error,badarg}.

domid() ->
	{ok,Value} = xenstore:read("domid"),
	list_to_integer(Value).

call(Msg) ->
	case whereis(?SERVER) of
		undefined -> {error,not_started};
		Pid -> Pid ! Msg,
			   receive ok =X		-> X;
					   {ok,_} =X	-> X;
					   {error,_} =X -> X end end.

%% ------------------------------------------------------------------

looper(XS) -> looper(XS, ?XS_RING_SIZE, 0, undefined, [], 0, [], 0, undefined, [], {[],1}, []).

looper(XS, QA, _RA, ExpSz, InBuf, InSz, OutBuf, OutSz, HH, Calls, RR, Ws) when OutSz > 0, QA > 0 ->
	{Chip,OutBuf1,OutSz1} = chip(QA, OutBuf, OutSz),
	ok = pore_xs:write(XS, Chip),
	true = pore:poke(XS),
	{QA1,RA1} = pore_xs:avail(XS),
	looper(XS, QA1, RA1, ExpSz, InBuf, InSz, OutBuf1, OutSz1, HH, Calls, RR, Ws);

looper(XS, QA, RA, undefined, InBuf, InSz, OutBuf, OutSz, undefined, Calls, RR, Ws) when InSz >= 16 ->
	{<<Op:32/little,
	   Rid:32/little,
	   Tid:32/little,
	   ExpSz:32/little>>,InBuf1,InSz1} = chip(16, InBuf, InSz),
	looper(XS, QA, RA, ExpSz, InBuf1, InSz1, OutBuf, OutSz, {Op,Rid,Tid}, Calls, RR, Ws);

looper(XS, QA, RA, ExpSz, InBuf, InSz, OutBuf, OutSz, {Op,Rid,_} =HH, Calls, RR, Ws)
										when ExpSz =/= undefined, InSz >= ExpSz ->
	{Chip,InBuf1,InSz1} = chip(ExpSz, InBuf, InSz),
	RR1 = rrid(Rid, RR),
	Resp = unpack(op_tag(Op), binary_to_list(Chip)),
	looper1(Resp, XS, QA, RA, InBuf1, InSz1, OutBuf, OutSz, HH, Calls, RR1, Ws);

looper(XS, _QA, RA, ExpSz, InBuf, InSz, OutBuf, OutSz, HH, Calls, RR, Ws) when RA > 0 ->
	Data = pore_xs:read(XS),
	true = pore:poke(XS),
	{QA1,RA1} = pore_xs:avail(XS),
	looper(XS, QA1, RA1, ExpSz, [InBuf,Data], InSz + iolist_size(Data), OutBuf, OutSz, HH, Calls, RR, Ws);

looper(XS, QA, RA, ExpSz, InBuf, InSz, OutBuf, OutSz, HH, Calls, RR, Ws) ->
	receive
	{From,Op,PL,Tid} ->
		{Rid,RR1} = rid(RR),
		Trailer = trailer(Op, PL),
		TSz = iolist_size(Trailer),
		SockMsg = <<Op:32/little,Rid:32/little,Tid:32/little,TSz:32/little>>,
		OutBuf1 = [OutBuf,SockMsg,Trailer],
		OutSz1 = OutSz + 16 + TSz,
		Calls1 = [{From,Rid,PL}|Calls],
		looper(XS, QA, RA, ExpSz, InBuf, InSz, OutBuf1, OutSz1, HH, Calls1, RR1, Ws);

	{From,unwatch,Path} ->
		{Ws1,Ws2} =
		lists:partition(fun({_,Path1,From1}) -> From1 =:= From andalso
												Path1 =:= Path end, Ws),
		Tokens = [ Token || {Token,_,_} <- Ws1 ],
		From ! {ok,Tokens},
		looper(XS, QA, RA, ExpSz, InBuf, InSz, OutBuf, OutSz, HH, Calls, RR, Ws2);

	{irq,XS} ->
		{QA1,RA1} = pore_xs:avail(XS),
		looper(XS, QA1, RA1, ExpSz, InBuf, InSz, OutBuf, OutSz, HH, Calls, RR, Ws) end.

looper1({ok,[Path,Token]}, XS, QA, RA, InBuf, InSz, OutBuf, OutSz, {?XS_WATCH_EVENT,_,_}, Calls, RR, Ws) ->
	case lists:keyfind(Token, 1, Ws) of
		{_,_,From} -> From ! {watch,Path}; _ -> ok end,
	looper(XS, QA, RA, undefined, InBuf, InSz, OutBuf, OutSz, undefined, Calls, RR, Ws);

looper1(ok, XS, QA, RA, InBuf, InSz, OutBuf, OutSz, {?XS_WATCH,Rid,_}, Calls, RR, Ws) ->
	{value,{From,_,[Path,Token]},Calls1} = lists:keytake(Rid, 2, Calls),
	From ! ok,
	Ws1 = [{Token,Path,From}|Ws],
	looper(XS, QA, RA, undefined, InBuf, InSz, OutBuf, OutSz, undefined, Calls1, RR, Ws1);

looper1(Resp, XS, QA, RA, InBuf, InSz, OutBuf, OutSz, {_,Rid,_}, Calls, RR, Ws) ->
	{value,{From,_,_},Calls1} = lists:keytake(Rid, 2, Calls),
	From ! Resp,
	looper(XS, QA, RA, undefined, InBuf, InSz, OutBuf, OutSz, undefined, Calls1, RR, Ws).

chip(N, Buf, Sz) when Sz =< N -> {iolist_to_binary(Buf),[],0};
chip(N, Buf, Sz) when is_binary(Buf) ->
	<<Chip:(N)/binary,Buf1/binary>> =Buf,
	{Chip,Buf1,Sz-N};
chip(N, Buf, Sz) -> chip(N, iolist_to_binary(Buf), Sz).

op_tag(?XS_ERROR)	  -> error;
op_tag(?XS_READ)	  -> read;
op_tag(?XS_WRITE)	  -> write;
op_tag(?XS_MKDIR)	  -> mkdir;
op_tag(?XS_RM)		  -> rm;
op_tag(?XS_DIRECTORY) -> directory;
op_tag(?XS_GET_PERMS) -> get_perms;
op_tag(?XS_SET_PERMS) -> set_perms;
op_tag(?XS_WATCH)	  -> watch;
op_tag(?XS_UNWATCH)	  -> unwatch;
op_tag(?XS_WATCH_EVENT) 	  -> watch_event;
op_tag(?XS_TRANSACTION_START) -> transaction_start;
op_tag(?XS_TRANSACTION_END)   -> transaction_end.

trailer(?XS_WRITE, [P,V]) ->
	list_to_binary([P,0,V]);	 %% XS_WRITE is special
trailer(_, [V|_] = PL) when is_list(V) ->
	list_to_binary([ [X,0] || X <- PL ]);
trailer(_, V) ->
	list_to_binary([V,0]).

unpack(error, What) ->
	%% What ends with 0
	What1 = string:substr(What, 1, length(What)-1),
	{error,list_to_atom(string:to_lower(What1))};
unpack(transaction_start, What) ->
	{ok,list_to_integer(What)};
unpack(directory, What) ->
	{ok,string:tokens(What, [0])};
unpack(_Tag, "OK\0") -> ok;
unpack(_Tag, What) ->
	case lists:member(0, What) of
		false -> {ok,What};
		true  -> {ok,string:tokens(What, [0])} end.

rid({[],NR}) -> {NR,{[],NR+1}};
rid({[Rid|Rids],NR}) -> {Rid,{Rids,NR}}.

rrid(Rid, {Rids,NR}) -> {[Rid|Rids],NR}.

token() ->
	<<A,B,C,D,E,F,G,H>> = crypto:rand_bytes(8),
	lists:flatten(io_lib:format("~.16b~.16b~.16b~.16b~.16b~.16b~.16b~.16b", [A,B,C,D,E,F,G,H])).

