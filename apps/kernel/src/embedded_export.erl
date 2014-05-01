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
%%% @doc A data blob bucket export module. The module implements callbacks needed
%%% for exporting a single bucket of named data blobs. The module functions are
%%% only called indirectly by '9p_server'.
%%%
%%% ModConf is an atom, the bucket name.
%%%
%%% @end
%%% 

-module(embedded_export).

-include("9p.hrl").

%% Callbacks
-export([list_dir/2,find/3]).
-export([read/5]).
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

list_dir(_Conn, Bucket) when is_atom(Bucket) ->
	AFiles = binary:list_embedded(Bucket),
	[{to_bin(A),A} || A <- AFiles].

-spec find(binary(), any(), any()) -> {found,_} | false.

find(File, _Conn, Bucket) when is_atom(Bucket) ->
	AName = to_atom(File),
	case binary:embedded_size(Bucket, AName) of
	false ->
		false;
	_Size ->
		{found,AName}
	end.

%% -spec create(binary(), any(), any()) -> boolean().
%%
%% create(Name, Conn, ModConf) -> false.

%% -spec remove(any(), any(), any()) -> boolean().
%%
%% remove(Name, Conn, ModConf) -> false.

-spec read(any(), integer(), integer(), any(), any()) -> binary().

read(File, Offset, Count, _Conn, Bucket) when is_atom(Bucket) ->
	case binary:embedded_size(Bucket, to_atom(File)) of
	false ->
		false;
	Size ->
		N = if Offset +Count > Size -> Size -Offset;
			true -> Count end,
		binary:embedded_part(Bucket, to_atom(File), Offset, N)
	end.

%% -spec write(any(), integer(), binary(), any(), any()) -> integer().
%% 
%% write(File, Offset, Data, Conn, Bucket) when is_atom(Bucket) -> 0.

-spec truncate(any(), integer(), any(), any()) -> integer().

truncate(File, _Size, _Conn, Bucket) when is_atom(Bucket) ->
	binary:embedded_size(Bucket, to_atom(File)). %% no truncation

-spec top_stat(any(), any()) -> #stat{}.

top_stat(_Conn, Bucket) when is_atom(Bucket) ->
	#stat{name =to_bin(Bucket),length =0}.

-spec file_stat(any(), any(), any()) -> #stat{}.

file_stat(File, _Conn, Bucket) when is_atom(Bucket) ->
	Length = binary:embedded_size(Bucket, to_atom(File)),
	#stat{name =to_bin(File),length =Length}.

%%------------------------------------------------------------------------------

to_bin(A) when is_atom(A) ->
	list_to_binary(atom_to_list(A));
to_bin(B) ->
	B.

to_atom(B) when is_binary(B) ->
	list_to_atom(binary_to_list(B));
to_atom(A) ->
	A.

%%EOF
