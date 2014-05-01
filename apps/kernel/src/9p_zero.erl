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

-module('9p_zero').

-export([is_local/1]).
-export([connect/1,connect/2]).
-export([listen/1]).
-export([accept/2,accept/3]).
-export([send/3]).
-export([recv/2,recv/3]).
-export([set_maximum_size/3]).
-export([activate/2]).
-export([close/2]).

-include("9p.hrl").

is_local(_) -> true.

connect(TransConf) ->
	connect(TransConf, infinity).

connect(Realm, Timeout) when is_atom(Realm) ->
	%erlang:display({'9p_zero',connect,Realm}),
	Realm ! {connect,self()},
	receive
	{connected,Peer} ->
		{ok,Peer}
	after Timeout ->
		{error,timeout}
	end.

listen(Realm) when is_atom(Realm) ->
	%erlang:display({'9p_zero',listen,Realm}),
	Pid = spawn_link(fun() -> loop({[],[]}) end),
	register(Realm, Pid),
	{ok,Realm}.
	
accept(Realm, TransConf) ->
	accept(Realm, TransConf, infinity).

accept(Realm, _TransConf, Timeout) ->
	%erlang:display({'9p_zero',accept,Realm}),
	Realm ! {accept,self()},
	receive
	{accepted,Peer} ->
		{ok,Peer}
	after Timeout ->
		{error,timeout}
	end.

send(Peer, Msg, _TransConf) ->
	%erlang:display({'9p_zero',send,Peer,Msg}),
	Peer ! {'9p_zero',self(),Msg},
	ok.

recv(Peer, TransConf) ->
	recv(Peer, TransConf, infinity).

recv(Peer, _TransConf, Timeout) ->
	%erlang:display({'9p_zero',recv,Peer}),
	receive
	{'9p_zero',Peer,Msg} ->
		{ok,Msg}
	after Timeout ->
		{error,timeout}
	end.

set_maximum_size(_Peer, _MSize, _TransConf) ->
	ok.

activate(_Peer, _TransConf) ->
	{ok,{'9p_zero',undefined,undefined}}.

close(Realm, _TransConf) when is_atom(Realm) -> %% "listening socket"
	case whereis(Realm) of
	undefined ->
		ok;
	Pid ->
		exit(Pid, normal)
	end;
close(_Peer, _TransConf) ->
	ok.

%%------------------------------------------------------------------------------

loop({Connecting,Accepting}) ->
	receive
	{connect,Pid} when Accepting =:= [] ->
		loop({Connecting ++ [Pid],Accepting});

	{connect,Pid} ->
		Peer = hd(Accepting),
		Peer ! {accepted,Pid},
		Pid ! {connected,Peer},
		loop({Connecting,tl(Accepting)});

	{accept,Pid} when Connecting =:= [] ->
		loop({Connecting,Accepting ++ [Pid]});

	{accept,Pid} ->
		Peer = hd(Connecting),
		Pid ! {accepted,Peer},
		Peer ! {connected,Pid},
		loop({tl(Connecting),Accepting})

	end.

%%EOF
