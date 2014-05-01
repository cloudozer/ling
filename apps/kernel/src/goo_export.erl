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
%%% @doc A GooFS to 9p interface. The module implements callbacks needed for
%%% exporting a GooFS formatted volume. The module functions areonly called
%%% indirectly by '9p_server'.
%%%
%%% ModConf is not used.
%%%
%%% @end
%%% 

-module(goo_export).

-include("9p.hrl").

%% Callbacks
-export([list_dir/2,find/3]).
-export([create/3,remove/3]).
-export([rename/4]).
-export([read/5,write/5]).
-export([truncate/4]).
-export([top_stat/2,file_stat/3]).

%% -spec top_granted(binary() | {binary(),integer()} | undefined, any(), any())
%% -> boolean().
%%
%% top_granted(User, Conn, Bucket) -> true.
%%
%% -spec file_granted(binary(), binary() | {binary(),integer()} | undefined,
%% any(), any()) -> boolean().
%%
%% file_granted(File, User, Conn, Bucket) -> true.

-spec list_dir(any(), any()) -> [{binary(),any()}].

list_dir(_Conn, _) ->
	goofs:list_files().

-spec find(binary(), any(), any()) -> {found,_} | false.

find(File, _Conn, _) ->
	case goofs:file_index(File) of
	not_found ->
		false;
	FileIndex ->
		{found,FileIndex}
	end.

-spec create(binary(), any(), any()) -> boolean().

create(Name, _Conn, _) ->
	case goofs:create_file(Name) of
	ok ->
		true;
	{error,_} ->
		false
	end.

-spec remove(any(), any(), any()) -> boolean().

remove(Name, _Conn, _) ->
	case goofs:delete_file(Name) of
	ok ->
		true;
	{error,_} ->
		false
	end.

-spec rename(any(), binary(), any(), any()) -> boolean().

rename(Name, NewName, _Conn, _) ->
	case goofs:rename_file(Name, NewName) of
	ok ->
		true;
	{error,_} ->
		false
	end.

-spec read(any(), integer(), integer(), any(), any()) -> binary().

read(Name, Offset, Count, _Conn, _) ->
	case goofs:read_file(Name, Offset, Count) of
	{ok,Data} ->
		Data;
	_ ->
		<<>>
	end.

-spec write(any(), integer(), binary(), any(), any()) -> integer().
 
write(Name, Offset, Data, _Conn, _) ->
	case goofs:write_file(Name, Offset, Data) of
	{ok,N} ->
		N;
	_ ->
		0
	end.

-spec truncate(any(), integer(), any(), any()) -> integer().

truncate(Name, Size, _Conn, _) ->
	{ok,NewSize} = goofs:resize_file(Name, Size),
	NewSize.

-spec top_stat(any(), any()) -> #stat{}.

top_stat(_Conn, _) ->
	{ok,Stat} = goofs:root_stat(),
	Stat.

-spec file_stat(any(), any(), any()) -> #stat{}.

file_stat(Name, _Conn, _) ->
	{ok,Stat} = goofs:file_stat(Name),
	Stat.

%%EOF
