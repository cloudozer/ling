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

-module(erl_prim_loader).

-export([start/3]).
-export([get_file/1]).
-export([get_files/2]).
-export([read_file_info/1]).
-export([set_path/1,get_path/0]).
-export([list_dir/1]).

-export([get_cwd/1,get_cwd/0]).

-export([set_primary_archive/4]).

-include("file.hrl").

start(_Id, _Loader, _Hosts) ->	%% {ok,Pid} | {error,What}
	Self = self(),
	Pid = spawn_link(fun() -> loop(Self, []) end),
	register(?MODULE, Pid),
	{ok,Pid}.

get_file(Filename) -> %% {ok,Bin,Fullname} | error
	File = strip_path(Filename),
	case binary:lookup_embedded(list_to_atom(File)) of
	false ->
		case whereis('9p_mounter') of
		undefined ->
			error;
		_ ->
			case prim_file:read_file(Filename) of
			{ok,Data} ->
				{ok,Data,Filename};
			{error,_} ->
				error
			end
		end;
	Bin ->
		{ok,Bin,embedded}
	end.

get_files(_ModFiles, _Fun) -> %% ok | {error,Mod}
	%%erlang:display({erl_prim_loader,get_files,_ModFiles,_Fun}),
	{error,all}.

%%
%% In LING the filesystem emerged as a part of the boot process. The code_server
%% asks erl_prim_loader regarding path element. We have to fake most of the
%% answers.
%%
read_file_info(".") -> 
	{ok,#file_info{type =directory,
				   access =read}};
read_file_info("/erlang") ->		%% code:root_dir()
	{ok,#file_info{type =directory,
				   access =read}};
read_file_info("/erlang/lib") ->	%% code:lib_dir()
	{ok,#file_info{type =directory,
				   access =read}};
read_file_info(Filename) -> %% {ok,FileInfo} | error
	case strip_path(Filename) of
	"ebin" -> %% always successful
		{ok,#file_info{type =directory,
					   access =read}};
	File ->
		case binary:lookup_embedded(list_to_atom(File)) of
		false ->
			error;
		Bin ->
			{ok,#file_info{size =byte_size(Bin),
					       type =regular,
						   access =read}}
		end
	end.

set_path(Path) -> %% ok
	erl_prim_loader ! {request,self(),set_path,Path},
	receive {reply,Reply} -> Reply end.

get_path() -> %% {ok,Path}
	erl_prim_loader ! {request,self(),get_path},
	receive {reply,Reply} -> Reply end.

list_dir(Dir) -> %% {ok,Filenames} | error
	case whereis('9p_mounter') of
	undefined ->
		error;
	_ ->
		case prim_file:list_dir(Dir) of
		{error,_} ->
			error;
		Other ->
			Other
		end
	end.

get_cwd(_) ->
	get_cwd().

get_cwd() -> %% {ok,Dir} | {error,Posix}
	case whereis('9p_mounter') of
	undefined ->
		{error,enoent};
	_ ->
		prim_file:get_cwd()
	end.

set_primary_archive(_, _, _, _) ->
	{error,not_supported}.

loop(Parent, Path) ->
	receive
	{request,From,get_path} ->
		From ! {reply,{ok,Path}},
		loop(Parent, Path);
	{request,From,set_path,NewPath} ->
		From ! {reply,ok},
		loop(Parent, NewPath);
	{'EXIT',Parent,W} -> %% why, who sets trap_exit?
		exit(W)
	end.

strip_path(Filename) ->
	strip_path(Filename, []).

strip_path([], Acc) ->
	rev(Acc);
strip_path([$/|Filename], _Acc) ->
	strip_path(Filename, []);
strip_path([Any|Filename], Acc) ->
	strip_path(Filename, [Any|Acc]).

rev(Ls) -> rev(Ls, []).

rev([], Rs) -> Rs;
rev([L|Ls], Rs) -> rev(Ls, [L|Rs]).

%%EOF
