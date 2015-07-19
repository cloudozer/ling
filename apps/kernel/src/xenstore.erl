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
-behaviour(gen_server).
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
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

read(Key) when is_list(Key) ->
	gen_server:call(?SERVER, {?XS_READ,Key,0});
read(_) -> {error,badarg}.

read_integer(Key) ->
	case read(Key) of {ok,S} -> try {ok,list_to_integer(S)}
							    catch _:_ -> {error,not_integer} end;
					  Error  -> Error end.

write(Key, Value) -> write(Key, Value, 0).
write(Key, Value, Tid) when is_integer(Value) ->
	write(Key, integer_to_list(Value), Tid);
write(Key, Value, Tid) when is_list(Key), is_list(Value), is_integer(Tid) ->
	gen_server:call(?SERVER, {?XS_WRITE,[Key,Value],Tid});
write(_, _, _) -> {error,badarg}.

mkdir(Path) -> mkdir(Path, 0).
mkdir(Path, Tid) when is_list(Path), is_integer(Tid) -> 
	gen_server:call(?SERVER, {?XS_MKDIR,Path,Tid});
mkdir(_, _) -> {error,badarg}.

delete(Path) -> delete(Path, 0).
delete(Path, Tid) when is_list(Path), is_integer(Tid) -> 
	gen_server:call(?SERVER, {?XS_RM,Path,Tid});
delete(_, _) -> {error,badarg}.

list(Path) when is_list(Path) ->
	gen_server:call(?SERVER, {?XS_DIRECTORY,Path,0});
list(_) -> {error,badarg}.

get_perms(Path) when is_list(Path) ->
	gen_server:call(?SERVER, {?XS_GET_PERMS,Path,0});
get_perms(_) -> {error,badarg}.

set_perms(Path, Perms) -> set_perms(Path, Perms, 0).
set_perms(Path, [X|_] =Perms, Tid) when is_list(Path), is_list(Perms),
										is_list(X), is_integer(Tid) ->
	gen_server:call(?SERVER, {?XS_SET_PERMS,[Path|Perms],Tid});
set_perms(_, _, _) -> {error,badarg}.

watch(Path) when is_list(Path) ->
	Caller = self(),
	gen_server:call(?SERVER, {watch,Path,Caller});
watch(_) -> {error,badarg}.

unwatch(Path) when is_list(Path) ->
	Caller = self(),
	gen_server:call(?SERVER, {unwatch,Path,Caller});
unwatch(_) -> {error,badarg}.

transaction() ->
	gen_server:call(?SERVER, {?XS_TRANSACTION_START,[],0}).

commit(Tid) when is_integer(Tid) ->
	gen_server:call(?SERVER, {?XS_TRANSACTION_END,"T",Tid});
commit(_) -> {error,badarg}.

rollback(Tid) when is_integer(Tid) ->
	gen_server:call(?SERVER, {?XS_TRANSACTION_END,"F",Tid});
rollback(_) -> {error,badarg}.

domid() ->
	{ok,Value} = xenstore:read("domid"),
	list_to_integer(Value).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	XS = open_port(xenstore, []),
    {ok,{XS,[]}}.

handle_call({watch,Path,Caller}, _From, {XS,Watches} =St) ->
	Token = integer_to_list(length(Watches)),
	case ctl_cmd(XS, ?XS_WATCH, 0, [Path,Token]) of
		ok -> {reply,ok,{XS,[{Token,Path,Caller}|Watches]}};
		Error -> {reply,Error,St} end;

handle_call({unwatch,Path,Caller}, _From, {XS,Watches}) ->
	{Watches1,Q} = stop_watches(XS, Path, Caller, Watches),
	{reply,Q,{XS,Watches1}};

handle_call({Op,PL,Tid}, _From, {XS,_} =St) ->
	Q = ctl_cmd(XS, Op, Tid, PL),
	{reply,Q,St};

handle_call(_Request, _From, St) ->
    {reply,ok,St}.

handle_cast(_Msg, St) ->
    {noreply,St}.

handle_info({watch_event,_,Result}, {_,Watches} =St) ->
	[Path,Token] = string:tokens(Result, [0]),
	Callers = [ Caller || {Token1,_,Caller} <- Watches, Token1 =:= Token ],
	lists:foreach(fun(Caller) -> Caller ! {watch,Path} end, Callers),
    {noreply,St}.

terminate(_Reason, {XS,_}) ->
	port_close(XS),
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok,St}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

ctl_cmd(XS, Op, Tid, PL) ->
	Data = pack(Op, Tid, PL),
	port_control(XS, Op, Data),
	Tag = op_tag(Op),
	receive {error,XS,What} -> unpack(error, What);
			{Tag,XS,What}   -> unpack(Tag, What) end.

pack(?XS_WRITE, Tid, [P,V]) ->
	list_to_binary([<<Tid:32>>,P,0,V]);	%% XS_WRITE is special
pack(_, Tid, [V|_] = PL) when is_list(V) ->
	list_to_binary([<<Tid:32>>,[ [X,0] || X <- PL ]]);
pack(_, Tid, V) ->
	list_to_binary([<<Tid:32>>,V,0]).

op_tag(?XS_READ)	  -> read;
op_tag(?XS_WRITE)	  -> write;
op_tag(?XS_MKDIR)	  -> mkdir;
op_tag(?XS_RM)		  -> rm;
op_tag(?XS_DIRECTORY) -> directory;
op_tag(?XS_GET_PERMS) -> get_perms;
op_tag(?XS_SET_PERMS) -> set_perms;
op_tag(?XS_WATCH)	  -> watch;
op_tag(?XS_UNWATCH)	  -> unwatch;
op_tag(?XS_TRANSACTION_START) -> transaction_start;
op_tag(?XS_TRANSACTION_END)   -> transaction_end.

unpack(error, What) ->
	{error,list_to_atom(string:to_lower(What))};
unpack(transaction_start, What) ->
	{ok,list_to_integer(What)};
unpack(directory, What) ->
	case lists:member(0, What) of
		false -> {ok,[What]};
		true  -> {ok,string:tokens(What, [0])} end;
unpack(_Tag, "OK") -> ok;
unpack(_Tag, What) ->
	case lists:member(0, What) of
		false -> {ok,What};
		true  -> {ok,string:tokens(What, [0])} end.

stop_watches(XS, Path, Caller, Watches) -> stop_watches(XS, Path, Caller, Watches, [], ok).
stop_watches(_XS, _Path, _Caller, [], Acc, Last) -> {lists:reverse(Acc),Last};
stop_watches(XS, Path, Caller, [{Token,Path,Caller}|Watches], Acc, Last) ->
	case ctl_cmd(XS, ?XS_UNWATCH, 0, [Path,Token]) of
		ok  -> stop_watches(XS, Path, Caller, Watches, Acc, Last);
		Err -> stop_watches(XS, Path, Caller, Watches, Acc, Err) end;
stop_watches(XS, Path, Caller, [Watch|Watches], Acc, Last) ->
	stop_watches(XS, Path, Caller, Watches, [Watch|Acc], Last).

