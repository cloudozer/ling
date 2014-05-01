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

-module('9p_tcp').

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

-define(CONCURRENCY, 1).

is_local({localhost,_}) -> true;
is_local({{127,0,0,_},_}) -> true; %% IPv6?
is_local(_) -> false.

connect(TransConf) ->
	connect(TransConf, infinity).

connect({Host,Port}, Timeout) ->
	gen_tcp:connect(Host, Port, [{active,false},
								 {mode,binary},
								 {recbuf,?MSIZE},
								 {packet,'9p'}], Timeout);
connect(_, _) ->
	{error,einval}.

listen({Addr,Port}) when is_integer(Port) ->
	gen_tcp:listen(Port, [{ip,Addr},
						  {active,false},
					 	  {mode,binary},
						  {packet,'9p'}]).

accept(Sock, TransConf) ->
	accept(Sock, TransConf, infinity).

accept(ListenSock, _TransConf, Timeout) ->
	case gen_tcp:accept(ListenSock, Timeout) of
	{ok,Sock} =Res ->

		%%
		%% 9p_mounter multiplexes many file operations over a single
		%% connection. Many such connections may be active. Set the size of the
		%% receive buffer high to prevent overlflowing.
		%%

		inet:setopts(Sock, [{recbuf,?MSIZE *?CONCURRENCY}]),
		Res;
	Other ->
		Other
	end.

send(Sock, Msg, _TransConf) when is_tuple(Msg) ->
	gen_tcp:send(Sock, '9p':encode(Msg)).

recv(Sock, TransConf) ->
	recv(Sock, TransConf, infinity).

recv(Sock, _TransConf, Timeout) ->
	gen_tcp:recv(Sock, 0, Timeout).

set_maximum_size(Sock, MSize, _TransConf) ->
	%% NB: recbuf not set -- MSize may be 4Gb
	inet:setopts(Sock, [{packet_size,MSize}]).

activate(Sock, _TransConf) ->
	inet:setopts(Sock, [{active,true}]),
	{ok,{tcp,tcp_closed,tcp_error}}.

close(Sock, _TransConf) ->
	gen_tcp:close(Sock).

%%EOF
