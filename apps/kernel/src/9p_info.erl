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

%%%
%%% @doc Accessors for 9p connection parameters. All functions of a callback
%%% module that implements an exported directory take and opaque `Conn' parameter.
%%% The '9p_info':* functions provide access to various bits of information stored
%%% in the `Conn' structure. The primary purpose is to make informed decisions
%%% about permissions of files and directories.
%%%
-module('9p_info').
-export([version/1]).
-export([peer_node/1,peer_group/1]).
-export([auth_path/1,auth_user/1,auth_user_id/1]).
-export([unix_user/1,unix_group/1]).

-include("9p_info.hrl").

-type opaque() :: any().

%% @doc Returns the negotiated version of the 9p protocol. '9P2000.e' is Erlang
%% extension version used by Erlang on Xen nodes. Linux clients based on v9fs
%% kernel module use '9P2000.u' version.
-spec version(Conn) -> '9P2000.e' | '9P2000.u' when
	Conn :: opaque().

version(#ac{ver =e}) -> '9P2000.e';
version(#ac{ver =u}) -> '9P2000.u'.

%% @doc Returns the node id of the connected peer discovered using the MUMBLE
%% authentication exchange.
-spec peer_node(Conn) -> Node | undefined when
	Conn :: opaque(),
	Node :: atom().

peer_node(#ac{peer_node =N}) -> N.

%% @doc Returns the node group of the connected peer discovered using the MUMBLE
%% authentication exchange.
-spec peer_group(Conn) -> Group | undefined when
	Conn :: opaque(),
	Group :: atom().

peer_group(#ac{peer_group =G}) -> G.

%% @doc Returns the path (aname) indicate in the auth message.
-spec auth_path(Conn) -> Path | undefined when
	Conn :: opaque(),
	Path :: binary().

auth_path(#ac{auth_path =AP}) -> AP.

%% @doc Returns the user name indicated in the auth message.
-spec auth_user(Conn) -> User | undefined when
	Conn :: opaque(),
	User :: binary().

auth_user(#ac{auth_user =AU}) -> AU.

%% @doc Returns the numeric used id that may be indicated in the auth message
%% ('9P2000.u' only).
-spec auth_user_id(Conn) -> UserId | undefined when
	Conn :: opaque(),
	UserId :: integer().

auth_user_id(#ac{auth_uid =ID}) -> ID.

%% @doc Returns the Unix user id of the connected peer discovered during a
%% successful MUNGE authentication exchange.
-spec unix_user(Conn) -> UserId | undefined when
	Conn :: opaque(),
	UserId :: integer().

unix_user(#ac{unix_user =UU}) -> UU.

%% @doc Returns the Unix group id of the connected peer discovered during a
%% successful MUNGE authentication exchange.
-spec unix_group(Conn) -> GroupId | undefined when
	Conn :: opaque(),
	GroupId :: integer().

unix_group(#ac{unix_group =UG}) -> UG.

%%EOF
