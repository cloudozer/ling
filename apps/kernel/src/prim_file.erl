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

%%%----------------------------------------------------------------------------
%%% @author Maxim Kharchenko <mk@cloudozer.com>
%%% @copyright 2012, Cloudozer LLP, All Rights Reserved
%%% @doc
%%%		The bridge between 9p layer and file server. The module translates file
%%%		operations into 9p requests handling multiple 9p mounts. The module
%%%		interface is the same as prim_file module of Erlang/OTP. The
%%%		implementation is completely different.
%%%
%%%		The Port parameter passed to some API functions is always ignored.
%%% @end
%%% @since 2012-12-1 by Maxim Kharchenko
%%%----------------------------------------------------------------------------
-module(prim_file).
-author('mk@cloudozer.com').

%%------------------------------------------------------------------------------
%% API exports
%%------------------------------------------------------------------------------

-export([pread/2,pread/3,pwrite/2,pwrite/3]).
-export([copy/3]).
-export([read_line/1]).

-export([open/2,close/1]).
-export([read/2,write/2]).

-export([open/1,open/3]).
-export([read_file/1,read_file/2,write_file/2]).
-export([get_cwd/0,get_cwd/1,get_cwd/2,set_cwd/1,set_cwd/2]).
-export([delete/1,delete/2]).
-export([make_dir/1,make_dir/2]).
-export([del_dir/1,del_dir/2]).
-export([altname/1,altname/2]).
-export([list_dir/1,list_dir/2]).
-export([read_link_info/1,read_link_info/2,read_link_info/3]).
-export([read_file_info/1,read_file_info/2,read_file_info/3]).
-export([write_file_info/2,write_file_info/3,write_file_info/4]).
-export([rename/2,rename/3]).
-export([make_link/2,make_link/3]).
-export([make_symlink/2,make_symlink/3]).
-export([read_link/1,read_link/2]).

-export([start/0,stop/1]).

-export([position/2,truncate/1]).
-export([sendfile/10]).
-export([sync/1,datasync/1]).
-export([advise/4]).

%% who needs this?
-export([ipread_s32bu_p32bu/3]).

%%------------------------------------------------------------------------------

-include("file.hrl").

-define(S_IFMT,		8#0170000).	%% File type mask
-define(S_IFDIR,	8#0040000).	%% Directory
-define(S_IFCHR,	8#0020000).	%% Character device
-define(S_IFBLK,	8#0060000).	%% Block device
-define(S_IFREG,	8#0100000).	%% Regular file
-define(S_IFIFO,	8#0010000).	%% FIFO
-define(S_IFLNK,	8#0120000).	%% Symbolic link
-define(S_IFSOCK,	8#0140000).	%% Socket

-type name() :: list() | binary().
-type mode() :: read |
				write |
			   	append |
			   	exclusive |
			   	binary |
				{delayed_write,_} | delayed_write |
				{read_ahead,_} | read_ahead |
				compressed.
-type file_info_option() :: {time,local} |
				{time,universal} |
				{time,posix}.
-type at() :: integer()
            | {bof, Offset :: integer()}
            | {cur, Offset :: integer()}
            | {eof, Offset :: integer()}
            | bof
            | cur
            | eof.

%%------------------------------------------------------------------------------
%% Public functions
%%------------------------------------------------------------------------------

%% @doc Opens a file.
%%
%% Modes delayed_write, read_ahead, and compressed are ignored.
%%
-spec open(name(), [mode()]) -> {ok,#file_descriptor{}} | {error,_}.

open(File, Modes) when (is_list(File) orelse is_binary(File)), is_list(Modes) ->

	%% Modes:
	%%
	%% read - must exist
	%% write - create if not exist or truncate existing
	%% read write - create if not exist
	%% append - create if not exist
	%% read append - create if not exist
   	%% write exclusive - create if not exist or fail if exists
	%% append exclusive - create if not exist or fail if exists
	%%

	case open_mode(Modes) of
	{ReadFormat,PosixModes} ->

		case '9p_mounter':canonicalise(File) of
		{ok,[]} -> %% root
			{error,eisdir};

		{ok,SFile} ->

			case goto_file(SFile) of
			{error,enoent} ->

				case lists:member(creat, PosixModes) of
				false ->
					{error,enoent};
				true ->
					{SDir,Base} = dir_base(SFile),
					case goto_dir(SDir) of
					{ok,{local,_,_}} ->
						{error,eperm};

					{ok,{ConnPid,Fid}} ->
						case '9p':create(ConnPid, Fid, Base, PosixModes) of
						{ok,_Qid,_Iounit} ->
							{ok,#file_descriptor{module =?MODULE,
												 data ={ConnPid,Fid,ReadFormat}}};
						{error,_} =Error ->
							'9p':clunk(ConnPid, Fid),
							Error
						end;
					{error,_} =Error ->
						Error
					end
				end;

			{error,_} =Error ->
				Error;
					
			{ok,{ConnPid,Fid}} ->

				case is_exclusive_write(PosixModes) of
				true ->
					{error,eexist};
				false ->

					case '9p':open(ConnPid, Fid, PosixModes) of
					{ok,_Qid,_Iounit} ->
						{ok,#file_descriptor{module =?MODULE,
											 data ={ConnPid,Fid,ReadFormat}}};
					Error ->
						'9p':clunk(ConnPid, Fid),
						Error
					end
				end
			end;

		{error,_} =Error ->
			Error
		end;

	Error ->
		{error,Error}
	end;
open(_, _) ->
	{error,badarg}.

%% @doc Opens a file. Port is ignored.
%% @see open/2.
-spec open(_, name(), [mode()]) -> {ok,#file_descriptor{}} | {error,_}.

open(_Port, File, ModeList) ->
	open(File, ModeList).

%% @doc Opens a port for subsequent API calls. Noop.

open(_PortOpts) ->
	no_port.

%% @doc Closes a file descriptor.
-spec close(#file_descriptor{}) -> ok | {error,_}.

close(#file_descriptor{module =?MODULE, data ={ConnPid,Fid,_}}) ->
	'9p':clunk(ConnPid, Fid).

%% @doc Reads maximum of N bytes from the file descriptor.
-spec read(#file_descriptor{}, integer()) -> {ok,binary() | list()} | eof |
{error,_}.

read(#file_descriptor{module =?MODULE,data ={ConnPid,Fid,Format}}, N)
		when is_integer(N), N >= 0 ->
	case '9p':read(ConnPid, Fid, N) of
	{ok,_} =Res when Format =:= binary ->
		Res;

	{ok,Bin} ->
		{ok,binary_to_list(Bin)};

	Other ->
		Other	%% {error,_} or eof
	end;
read(_, _) ->
	{error,badarg}.

%% @doc Write data to the file descriptor.
-spec write(#file_descriptor{}, binary()) -> ok | {error,_}.

write(#file_descriptor{module =?MODULE,data ={ConnPid,Fid,_}}, Data)
		when (is_binary(Data) orelse is_list(Data)) ->
	try
		DataBin = iolist_to_binary(Data),
		'9p':write(ConnPid, Fid, DataBin)

	catch error:Reason ->
		{error,Reason}
	end;
write(_, _) ->
	{error,badarg}.

%% @doc Reads entire contents of a named file.
%% @see read_file/1.
-spec read_file(_, name()) -> {ok,binary()} | {error,_}.

read_file(_Port, File) ->
	read_file(File).

%% @doc Reads entire contents of a named file.
-spec read_file(name()) -> {ok,binary()} | {error,_}.

read_file(File) when (is_list(File) orelse is_binary(File)) ->
	case '9p_mounter':canonicalise(File) of
	{ok,[]} -> %% root
		{error,eisdir};
	{ok,SFile} ->
		case goto_file(SFile) of
		{ok,{ConnPid,Fid}} ->
			Result = case '9p':open(ConnPid, Fid, [rdonly]) of
			{ok,_Qid,_Iounit} ->
				'9p':read_all(ConnPid, Fid);
			{error,_} =Error ->
				Error
			end,
			'9p':clunk(ConnPid, Fid),
			Result;
		{error,_} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end;
read_file(_) ->
	{error,badarg}.

%% @doc Replaces contents of the named file with Data.
-spec write_file(name(), binary()) -> ok | {error,_}. 

write_file(File, Data) when (is_list(File) orelse is_binary(File)),
		is_binary(Data) ->
	case '9p_mounter':canonicalise(File) of
	{ok,[]} -> %% root
		{error,eisdir};
	{ok,SFile} ->
		case goto_file(SFile) of
		{ok,{ConnPid,Fid}} ->
			Result = case '9p':open(ConnPid, Fid, [wronly,trunc]) of
			{ok,_Qid,_Iounit} ->
				'9p':write(ConnPid, Fid, Data);
			{error,_} =Error ->
				Error
			end,
			'9p':clunk(ConnPid, Fid),
			Result;

		{error,enoent} ->
			{SDir,Base} = dir_base(SFile),
			case goto_dir(SDir) of
			{ok,{local,_,_}} ->
				{error,eperm};

			{ok,{ConnPid,DFid}} ->
				case '9p':create(ConnPid, DFid, Base, [wronly]) of
				{ok,_Qid,_Iounit} ->
					'9p':write(ConnPid, DFid, Data);
				{error,_} =Error ->
					Error
				end;
			{error,_} =Error ->
				Error
			end;

		{error,_} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end;
write_file(_, _) ->
	{error,badarg}.

%% @doc Retrieves the current working directory.
-spec get_cwd() -> {ok,string()} | {error,_}.

get_cwd() ->
	get_cwd_int().

%% @doc Retrieves the current working directory.
%% @see get_cwd/0
-spec get_cwd(string()) -> {ok,string()} | {error,_}.

get_cwd([_,$:|_]) ->
	{error,enotsup};	%% c:\
get_cwd(_) ->
	get_cwd_int().

%% @doc Retrieves the current working directory.
%% @see get_cwd/0
-spec get_cwd(_, string()) -> {ok,string()} | {error,_}.

get_cwd(_, [_,$:|_]) ->
	{error,enotsup};	%% c:\
get_cwd(_, _) ->
   	get_cwd_int().

get_cwd_int() ->
	case '9p_mounter':get_cwd() of
	{ok,SPath} ->
		{ok,binary_to_list(unsplit(SPath))};
	Error ->
		Error
	end.

%% @doc Sets the current working directory.
%% @see set_cwd/1.
-spec set_cwd(_, name()) -> ok | {error,_}.
 
set_cwd(_, Dir) ->
	set_cwd(Dir).

%% @doc Sets the current working directory.
-spec set_cwd(name()) -> ok | {error,_}.

set_cwd(Dir) when (is_list(Dir) orelse is_binary(Dir)) ->
	case '9p_mounter':canonicalise(Dir) of
	{ok,SPath} ->

		case goto_dir(SPath) of
		{ok,{local,_,_}} ->
			'9p_mounter':set_cwd(SPath);
		{ok,{ConnPid,Fid}} ->
			'9p':clunk(ConnPid,Fid),
			'9p_mounter':set_cwd(SPath);
		{error,_} =Error ->
			Error
		end;

	{error,_} =Error ->
		Error
	end;
set_cwd(_) ->
	{error,badarg}.

%% @doc Attempts to delete the named file.
%% @see delete/1.
-spec delete(_, name()) -> ok | {error,_}.

delete(_Port, File) ->
	delete(File).

%% @doc Attempts to delete the named file.
-spec delete(name()) -> ok | {error,_}.

delete(File) when (is_list(File) orelse is_binary(File)) ->
	case '9p_mounter':canonicalise(File) of
	{ok,SFile} ->
		case goto_file(SFile) of
		{ok,{ConnPid,Fid}} ->
			'9p':remove(ConnPid, Fid);
		{error,_} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end;
delete(_) ->
	{error,badarg}.

%% @doc Creates a named directory.
%% @see make_dir/1.
-spec make_dir(_, name()) -> ok | {error,_}.

make_dir(_Port, Dir) ->
	make_dir(Dir).

%% @doc Creates a named directory.
-spec make_dir(name()) -> ok | {error,_}.

make_dir(Dir) when (is_list(Dir) orelse is_binary(Dir)) ->
	case '9p_mounter':canonicalise(Dir) of
	{ok,[]} ->
		{error,eexist};
	{ok,SPath} ->
		{SDir,Base} = dir_base(SPath),
		case goto_dir(SDir) of
		{ok,{local,_,_}} ->
			{error,eperm};

		{ok,{ConnPid,DFid}} ->
			R = case '9p':mkdir(ConnPid, DFid, Base) of
			{ok,_} ->
				ok;
			{error,_} =Error ->
				Error
			end,
			'9p':clunk(ConnPid, DFid),
			R;
		{error,_} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end;
make_dir(_) ->
	{error,badarg}.

%% @doc Attempts to remove the named directory.
%% @see del_dir/1.
-spec del_dir(_, name()) -> ok | {error,_}.

del_dir(_Port, Dir) ->
	del_dir(Dir).

%% @doc Attempts to remove the named directory.
-spec del_dir(name()) -> ok | {error,_}.

del_dir(Dir) when (is_list(Dir) orelse is_binary(Dir)) ->
	case '9p_mounter':canonicalise(Dir) of
	{ok,SDir} ->
		case goto_dir(SDir) of
		{ok,{local,_,_}} ->
			{error,eperm};
		{ok,{ConnPid,Fid}} ->
			'9p':remove(ConnPid, Fid);
		{error,_} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end;
del_dir(_) ->
	{error,badarg}.

%% @doc Arcane name mangling. Unsupported.
%% @see altname/1.
-spec altname(_, name()) -> {error,_}.

altname(_, Dir) ->
	altname(Dir).

%% @doc Arcane name mangling. Unsupported.
-spec altname(name()) -> {error,enotsup}.

altname(Dir) when (is_binary(Dir) orelse is_list(Dir)) ->
	{error,enotsup};
altname(_) ->
	{error,badarg}.

%% @doc Gets the listing of the directory.
%% @see list_dir/1.
-spec list_dir(_, name()) -> {ok,[string()]} | {error,_}.

list_dir(_, Dir) ->
	list_dir(Dir).

%% @doc Gets the listing of the directory.
-spec list_dir(name()) -> {ok,[string()]} | {error,_}.

list_dir(Dir) when (is_list(Dir) orelse is_binary(Dir)) ->
	case '9p_mounter':canonicalise(Dir) of
	{ok,SDir} ->

		case '9p_mounter':resolve_to_dir(SDir) of
		{walk,Places} ->
			case union_all(fun({local,_,_} =Spec) ->
					list_local(Spec);

				({ConnPid,Fid,WalkTo}) ->
					case '9p':walk(ConnPid, Fid, WalkTo) of
					{ok,NewFid,Qids} when length(Qids) < length(WalkTo) ->
						'9p':clunk(ConnPid, NewFid),
						{error,enoent};
					{ok,NewFid,[]} ->
						dir_list(ConnPid, NewFid);
					{ok,NewFid,Qids} ->
						case '9p':qid_type(lists:last(Qids)) of
						dir ->
							dir_list(ConnPid, NewFid);
						_ ->
							{error,eperm}
						end;
					{error,_} =Error ->
						Error
					end
				end, Places) of

			{ok,LL} ->
				{ok,lists:usort(lists:concat(LL))};
			{error,_} =Error ->
				Error
			end;

		{error,_} =Error ->
			Error
		end;

	{error,_} =Error ->
		Error
	end;
list_dir(_) ->
	{error,badarg}.

list_local({local,Prio,N}) ->
	case '9p_mounter':local_path_element(Prio, N +1) of
	{ok,Name} ->
		{ok,[binary_to_list(Name)]};
	{error,_} =Error ->	
		Error
	end.

dir_list(ConnPid, Fid) ->
	case '9p':open(ConnPid, Fid, [rdonly]) of
	{ok,_Qid,_Iounit} ->
		case '9p':read_dir(ConnPid, Fid) of
		{ok,Ents} ->
			{ok,[binary_to_list(Name) || {_,_,_,Name} <- Ents,
					Name =/= <<".">>, Name =/= <<"..">>]};
		{error,_} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end.

%% @doc Reads a file info NOT following symlinks.
%% @see read_link_info/3.
-spec read_link_info(name()) -> {ok,#file_info{}} | {error,_}.

read_link_info(File) when (is_list(File) orelse is_binary(File)) ->
	read_link_info_1(File, local).

%% @doc Reads a file info NOT following symlinks.
%% @see read_link_info/3.
-spec read_link_info(_, name()) -> {ok,#file_info{}} | {error,_}.

read_link_info(_Port, File) when (is_list(File) orelse is_binary(File)) ->
	read_link_info_1(File, local);
read_link_info(_, _) ->
	{error,badarg}.

%% @doc Reads a file info NOT following symlinks.
%% @see read_link_info/3.
-spec read_link_info(_, name(), [file_info_option()]) -> {ok,#file_info{}} |
{error,_}.

read_link_info(_Port, File, [{time,Type}])
		when (is_list(File) orelse is_binary(File))  ->
	read_link_info_1(File, Type);
read_link_info(_, _, _) ->
	{error,badarg}.

read_link_info_1(File, TimeType) ->
	case '9p_mounter':canonicalise(File) of
	{ok,SFile} ->
		case goto_both(SFile) of
		{ok,{local,_,_}} ->
			Info = local_file_info(SFile, TimeType),
			{ok,Info};

		{ok,{ConnPid,Fid}} ->
			case '9p':getattr(ConnPid, Fid, [all]) of
			{ok,Props} ->
				'9p':clunk(ConnPid, Fid),
				{ok,props_to_file_info(Props, TimeType)};
			{error,_} =Error ->
				'9p':clunk(ConnPid, Fid),
				Error
			end;
		{error,_} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end.

%% @doc Reads a file info following symlinks.
%% @see read_file_info/3.
-spec read_file_info(name()) -> {ok,#file_info{}} | {errorm_}.

read_file_info(File) when (is_list(File) orelse is_binary(File)) ->
	read_file_info_1(File, local).

%% @doc Reads a file info following symlinks.
%% @see read_file_info/3.
-spec read_file_info(_, name()) -> {ok,#file_info{}} | {error,_}.

read_file_info(_Port, File) when (is_list(File) orelse is_binary(File)) ->
	read_file_info_1(File, local);
read_file_info(_, _) ->
	{error,badarg}.

%% @doc Reads a file info following symlinks.
-spec read_file_info(_, name(), [file_info_option()]) -> {ok,#file_info{}} |
{error,_}.

read_file_info(_Port, File, [{time,Type}])
		when (is_list(File) orelse is_binary(File)) ->
	read_file_info_1(File, Type);
read_file_info(_, _, _) ->
	{error,badarg}.

read_file_info_1(File, TimeType) ->
	case read_link_info_1(File, TimeType) of
	{ok,#file_info{type =symlink}} ->
		case follow_symlink(File) of
		{ok,Target} ->
			AbsTarget = filename:absname(Target, filename:dirname(File)),
			read_link_info_1(AbsTarget, TimeType);
		{error,_} =Error ->
			Error
		end;
	Other ->
		Other
	end.

follow_symlink(File) ->
	case '9p_mounter':canonicalise(File) of
	{ok,SFile} ->
		case '9p_mounter':resolve_to_file(SFile) of
		{walk,Places} ->
			case union(fun(Place) -> walk_to(Place, any) end,
					Places, fun dump_clunk/1) of
			{ok,{ConnPid,Fid}} ->
				R = '9p':read_link(ConnPid, Fid),
				'9p':clunk(ConnPid, Fid),
				R;
			{error,_} =Error ->
				Error
			end
		end;

	{error,_} =Error ->
		Error
	end.

%% @doc Update the file information of the named file.
%% @see write_file_info/4.
-spec write_file_info(name(), #file_info{}) -> ok | {error,_}.

write_file_info(File, Info) when (is_list(File) orelse is_binary(File)) ->
	write_file_info_1(File, Info, local).

%% @doc Update the file information of the named file.
%% @see write_file_info/4.
-spec write_file_info(_, name(), #file_info{}) -> ok | {error,_}.

write_file_info(_Port, File, Info) when (is_list(File) orelse is_binary(File)) ->
	write_file_info_1(File, Info, local);
write_file_info(_, _, _) ->
	{error,badarg}.

%% @doc Update the file information of the named file.
-spec write_file_info(_, name(), #file_info{}, [file_info_option()]) -> ok |
{error,_}.

write_file_info(_Port, File, Info, [{time,Type}])
		when (is_list(File) orelse is_binary(File)) ->
	write_file_info_1(File, Info, Type);
write_file_info(_, _, _, _) ->
	{error,badarg}.

write_file_info_1(File, Info, TimeType) ->
	Props = file_info_to_props(Info, TimeType),
	case '9p_mounter':canonicalise(File) of
	{ok,SFile} ->
		case goto_file(SFile) of
		{ok,{ConnPid,Fid}} ->
			R = '9p':setattr(ConnPid, Fid, Props),
			'9p':clunk(ConnPid, Fid),
			R;
		{error,_} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end.

%% @doc Renames/moves a filesystem object.
%% @see rename/2.
-spec rename(_, name(), name()) -> ok | {error,_}.

rename(_Port, Src, Dst) ->
	rename(Src, Dst).

%% @doc Renames/moves a filesystem object.
%% @see rename/2.
-spec rename(name(), name()) -> ok | {error,_}.

rename(Src, Dst) when (is_list(Src) orelse is_binary(Src)),
					  (is_list(Dst) orelse is_binary(Dst)) ->

	case get_source(Src) of
	{ok,{SrcConnPid,SrcFid}} ->
		case get_destination(Dst) of
		{ok,{DstConnPid,DstFid},_NewName} when DstConnPid =/= SrcConnPid ->
			%%
			%% Moving objects between filesystems not supported
			%%
			'9p':clunk(SrcConnPid, SrcFid),
			'9p':clunk(DstConnPid, DstFid),
			{error,exdev};

		{ok,{DstConnPid,DstFid},NewName} ->
			R = '9p':rename(SrcConnPid, SrcFid, DstFid, NewName),
			'9p':clunk(SrcConnPid, SrcFid),
			'9p':clunk(DstConnPid, DstFid),
			R;
		
		{error,_} =Error ->
			'9p':clunk(SrcConnPid, SrcFid),
			Error
		end;

	{error,_} =Error ->
		Error
	end;
rename(_, _) ->
	{error,badarg}.	

get_source(Src) ->
	case '9p_mounter':canonicalise(Src) of
	{ok,SPath} ->
		goto_both(SPath);
	{error,_} =Error ->
		Error
	end.

get_destination(Dst) ->
	case '9p_mounter':canonicalise(Dst) of
	{ok,SPath} ->
		{SDir,Base} = dir_base(SPath),
		case goto_dir(SDir) of
		{ok,{local,_,_}} ->
			{error,eperm};
		{ok,Spec} ->
			{ok,Spec,Base};
		{error,_} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end.

%% @doc Creates a hard link.
%% @see make_link/2.
-spec make_link(_, name(), name()) -> ok | {error,_}.

make_link(_Port, Existing, New) ->
	make_link(Existing, New).

%% @doc Creates a hard link.
-spec make_link(name(), name()) -> ok | {error,_}.

make_link(Src, Dst) when (is_list(Src) orelse is_binary(Src)),
					     (is_list(Dst) orelse is_binary(Dst)) ->
	case get_source(Src) of
	{ok,{SrcConnPid,SrcFid}} ->
		case get_destination(Dst) of
		{ok,{DstConnPid,DstFid},_NewName} when DstConnPid =/= SrcConnPid ->
			%%
			%% Linking objects between filesystems not supported
			%%
			'9p':clunk(SrcConnPid, SrcFid),
			'9p':clunk(DstConnPid, DstFid),
			{error,exdev};

		{ok,{DstConnPid,DstFid},NewName} ->
			R = '9p':link(SrcConnPid, DstFid, SrcFid, NewName),
			'9p':clunk(SrcConnPid, SrcFid),
			'9p':clunk(DstConnPid, DstFid),
			R;
		
		{error,_} =Error ->
			'9p':clunk(SrcConnPid, SrcFid),
			Error
		end;

	{error,_} =Error ->
		Error
	end;
make_link(_, _) ->
	{error,badarg}.

%% @doc Creates a symbolink link.
%% @see make_symlink/2.
-spec make_symlink(_, name(), name()) -> ok | {error,_}.

make_symlink(_Port, Target, File) ->
	make_symlink(Target, File).

%% @doc Creates a symbolink link.
-spec make_symlink(name(), name()) -> ok | {error,_}.

make_symlink(Target, File) when (is_list(Target) orelse is_binary(Target)),
								(is_list(File) orelse is_binary(File)) ->
	case get_destination(File) of
	{ok,{ConnPid,Fid},NewName} ->
		case get_target(Target) of
		{ok,TargBin} ->
			case '9p':symlink(ConnPid, Fid, NewName, TargBin) of
			{ok,_Qid} ->
				'9p':clunk(ConnPid, Fid),
				ok;
			{error,_} =Error ->
				'9p':clunk(ConnPid, Fid),
				Error
			end;
		{error,_} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end;
make_symlink(_, _) ->
	{error,badarg}.

get_target(Target) when is_list(Target) ->
	case catch list_to_binary(Target) of
	{'EXIT',_} ->
		{error,badarg};
	TargBin ->
		{ok,TargBin}
	end;
get_target(Target) ->
	{ok,Target}.

%% @doc Resolves a symbolic link.
%% @see read_link/1.
-spec read_link(_, name()) -> {ok,binary()} | {error,_}.

read_link(_Port, File) ->
	read_link(File).

%% @doc Resolves a symbolic link.
-spec read_link(name()) -> {ok,binary()} | {error,_}.

read_link(File) when (is_list(File) orelse is_binary(File)) ->
	follow_symlink(File);
read_link(_) ->
	{error,badarg}.

%% @doc Moves the current position of the file descriptor.
-spec position(#file_descriptor{}, at()) -> {ok,integer()} | {error,_}.

position(#file_descriptor{module =?MODULE,data ={ConnPid,Fid,_}}, At) ->
	position_1(ConnPid, Fid, At);
position(_, _) ->
	{error,badarg}.

position_1(ConnPid, Fid, eof) ->
	position_1(ConnPid, Fid, {eof,0});
position_1(ConnPid, Fid, {eof,N}) when is_integer(N) ->
	case '9p':getattr(ConnPid, Fid, [size]) of
	{ok,[{size,Size}]} ->
		position_1(ConnPid, Fid, {bof,Size +N});
	{error,_} =Error ->
		Error
	end;
position_1(ConnPid, Fid, At) ->
	'9p':seek(ConnPid, Fid, At).

%% @doc Truncates the file at the current position.
-spec truncate(#file_descriptor{}) -> ok | {error,_}.

truncate(#file_descriptor{module =?MODULE,data ={ConnPid,Fid,_}}) ->
	case '9p':seek(ConnPid, Fid, cur) of
	{ok,NewSize} ->
		'9p':setattr(ConnPid, Fid, [{size,NewSize}]);
	{error,_} =Error ->
		Error
	end.

%%------------------------------------------------------------------------------

%% Not supported -- file:sendfile will revert to fallback
sendfile(_Fd, _Sock, _Offs, _Bytes, _ChunkSz, _, _, _, _, _) ->
	{error,enotsup}.

%% @doc Sync.
-spec sync(#file_descriptor{}) -> ok | {error,_}.

sync(#file_descriptor{module =?MODULE,data ={ConnPid,Fid,_}}) ->
	'9p':fsync(ConnPid, Fid);
sync(_) ->
	{error,badarg}.
	
%% @doc Data sync.
-spec datasync(#file_descriptor{}) -> ok | {error,_}.

datasync(#file_descriptor{module =?MODULE}) ->
	ok;
datasync(_) ->
	{error,badarg}.

%% @doc Advise.
-spec advise(_, _, _, _) -> ok | {error,_}.

advise(#file_descriptor{module=?MODULE}, _, _, Advise) ->
	case lists:member(Advise, [normal,
							   sequential,
							   random,
							   no_reuse,
							   will_need,
							   dont_need]) of
	false ->
		{error,einval};
	true ->
		ok
	end;
advise(_, _, _, _) ->
	{error,badarg}.

%%TODO A parallel implementation can be attempted, similar to pread()

%%
%% The behaviour is slightly different from file:pread()/pwrite(). The current
%% position is left untouched (the documentation says it becomes undefined).
%%

pwrite(#file_descriptor{module=?MODULE,data={ConnPid,Fid,_}}, LocBytes)
		when is_list(LocBytes) ->
	pwrite_1(ConnPid, Fid, LocBytes, 0);
pwrite(_, _) ->
	{error,badarg}.

pwrite_1(_ConnPid, _Fid, [], _N) ->
	ok;
pwrite_1(ConnPid, Fid, [{At,Data}|LocBytes], N) when is_integer(At) ->
	Bin = iolist_to_binary(Data),
	case '9p':write(ConnPid, Fid, At, Bin) of
	ok ->
		pwrite_1(ConnPid, Fid, LocBytes, N+1);
	{error,Reason} ->
		{error,{N,Reason}}
	end;
pwrite_1(_, _, _, N) ->
	{error,{N,badarg}}.

pwrite(#file_descriptor{module=?MODULE,data={ConnPid,Fid,_}}, At, Data)
		when is_integer(At) ->
	Bin = iolist_to_binary(Data),
	'9p':write(ConnPid, Fid, At, Bin);
pwrite(_, _, _) ->
	{error,badarg}.

pread(#file_descriptor{module=?MODULE,data={ConnPid,Fid,DF}}, LocNums)
		when is_list(LocNums) ->

	case pread_args(LocNums) of
	ok ->
		union_all(fun({At,N}) ->
			case '9p':read(ConnPid, Fid, At, N) of
			{ok,Data} when DF =:= list ->
				{ok,binary_to_list(Data)};
			eof ->
				{ok,eof};
			Other ->
				Other
			end
		end, LocNums);
	Reason ->
		{error,Reason}
	end;

pread(_, _) ->
	{error,badarg}.

pread_args([]) -> ok;
pread_args([{At,N}|Args]) when is_integer(At), is_integer(N) -> pread_args(Args);
pread_args(_) -> badarg.

pread(#file_descriptor{module =?MODULE,data ={ConnPid,Fid,DF}}, At, N)
		when is_integer(At), is_integer(N) ->
	case '9p':read(ConnPid, Fid, At, N) of
	{ok,Data} when DF =:= list ->
		{ok,binary_to_list(Data)};
	Other ->
		Other
	end;
pread(_, _, _) ->
	{error,badarg}.

copy(#file_descriptor{module =?MODULE,data ={SrcConnPid,SrcFid,_}},
     #file_descriptor{module =?MODULE,data ={DstConnPid,DstFid,_}},
     Length)
  when is_integer(Length), Length >= 0;
       is_atom(Length) ->

	copy_int(SrcConnPid, SrcFid, DstConnPid, DstFid, 0, Length);
copy(_, _, _) ->
	{error,badarg}.

%% Left may be 'infinity'
copy_int(_, _, _, _, Copied, 0) ->
	{ok,Copied};
copy_int(SrcConnPid, SrcFid, DstConnPid, DstFid, Copied, Left) ->
	MaxSize = max_size(65536, Left),
	case '9p':read(SrcConnPid, SrcFid, MaxSize) of
	{ok,Data} ->
		case '9p':write(DstConnPid, DstFid, Data) of
		ok ->
			N = byte_size(Data),
			copy_int(SrcConnPid, SrcFid, DstConnPid, DstFid,
										Copied +N, left(Left, N));
		{error,_} =Error ->
			Error
		end;
	eof ->
		{ok,Copied};
	{error,_} =Error ->
		Error
	end.

max_size(Sz, infinity) -> Sz;
max_size(Sz, Left) when Sz > Left -> Left;
max_size(Sz, _Left) -> Sz.

left(infinity, _N) -> infinity;
left(Left, N) -> Left -N.

%%NB: this is slow and cannot be made much faster

read_line(#file_descriptor{module=?MODULE,data={ConnPid,Fid,_}}) ->
	read_line_1(ConnPid, Fid, []);
read_line(_) ->
	{error,badarg}.

read_line_1(ConnPid, Fid, Acc) ->
	case '9p':read(ConnPid, Fid, 1) of
	eof when Acc =:= [] ->
		eof;
	eof ->
		{ok,lists:reverse(Acc)};
	{ok,<<$\n>>} ->
		case Acc of
		[$\r|SansCr] ->
			{ok,lists:reverse([$\n|SansCr])};
		_ ->
			{ok,lists:reverse(Acc)}
		end;
	{ok,<<Any>>} ->
		read_line_1(ConnPid, Fid, [Any|Acc]);
	Error ->
		Error
	end.

%% adapted from file.erl - same ugliness in two places
ipread_s32bu_p32bu(File, Pos, Infinity) when is_atom(Infinity) ->
    ipread_s32bu_p32bu(File, Pos, (1 bsl 31)-1);
ipread_s32bu_p32bu(File, Pos, MaxSize) 
  when is_integer(MaxSize), MaxSize >= 0 ->
    if
	MaxSize < (1 bsl 31) ->
	    case pread(File, Pos, 8) of
		{ok, Header} ->
		    ipread_s32bu_p32bu_2(File, Header, MaxSize);
		Error ->
		    Error
	    end;
	true ->
	    {error, einval}
    end;
ipread_s32bu_p32bu(_File, _Pos, _MaxSize) ->
    {error, badarg}.

ipread_s32bu_p32bu_2(_File, 
		     <<0:32/big-unsigned, Pos:32/big-unsigned>>,
		     _MaxSize) ->
    {ok, {0, Pos, eof}};
ipread_s32bu_p32bu_2(File, 
		     <<Size:32/big-unsigned, Pos:32/big-unsigned>>,
		     MaxSize) 
  when Size =< MaxSize ->
    case pread(File, Pos, Size) of
	{ok, Data} ->
	    {ok, {Size, Pos, Data}};
	eof ->
	    {ok, {Size, Pos, eof}};
	Error ->
	    Error
    end;
ipread_s32bu_p32bu_2(_File, 
		     <<_:8/binary>>,
		     _MaxSize) ->
    eof;
ipread_s32bu_p32bu_2(_File,
		     <<_/binary>>,
		     _MaxSize) ->
    eof;
ipread_s32bu_p32bu_2(File,
		    Header,
		    MaxSize) when is_list(Header) ->
    ipread_s32bu_p32bu_2(File, list_to_binary(Header), MaxSize).

%%------------------------------------------------------------------------------

start() ->

	%% Change working directory, if asked to
	case init:get_argument(home) of
	{ok,[[HomeDir]]} ->
		set_cwd(HomeDir);
	_ ->
		ok
	end,

	{ok,noport}.

stop(_Port) ->
	ok.

%%------------------------------------------------------------------------------
%% Union functions
%%------------------------------------------------------------------------------

is_exclusive_write(Modes) ->
	lists:member(excl, Modes) andalso
		(lists:member(rdwr, Modes) orelse lists:member(wronly, Modes)).

goto_file(SFile) ->
	case '9p_mounter':resolve_to_file(SFile) of
	{walk,Places} ->
		union(fun(Place) -> walk_to(Place, file) end,
				Places, fun dump_clunk/1);
	{error,_} =Error ->
		Error
	end.

goto_dir(SDir) ->
	case '9p_mounter':resolve_to_dir(SDir) of
	{walk,Places} ->
		union(fun(Place) -> walk_to(Place, dir) end,
				Places, fun dump_clunk/1);
	{error,_} =Error ->
		Error
	end.

goto_both(SPath) ->
	case '9p_mounter':resolve_to_both(SPath) of
	{walk,Places} ->
		union(fun(Place) -> walk_to(Place, any) end,
				Places, fun dump_clunk/1);
	{error,_} =Error ->
		Error
	end.

walk_to({local,_,_} =Spec, any) ->
	{ok,Spec};
walk_to({local,_,_} =Spec, dir) ->
	{ok,Spec};
walk_to({local,_,_}, file) ->
	{error,eisdir};

walk_to({ConnPid,Fid,WalkTo}, ReqType) ->
	case '9p':walk(ConnPid, Fid, WalkTo) of
	{ok,NewFid,Qids} when length(Qids) < length(WalkTo) ->
		'9p':clunk(ConnPid, NewFid),
		{error,enoent};
	{ok,NewFid,[]} ->
		{ok,{ConnPid,NewFid}}; %% clone
	{ok,NewFid,_Qids} when ReqType =:= any ->
		{ok,{ConnPid,NewFid}};
	{ok,NewFid,Qids} ->
		case '9p':qid_type(lists:last(Qids)) of
		link ->
			{ok,{ConnPid,NewFid}};
		ReqType ->
			{ok,{ConnPid,NewFid}};
		dir ->
			{error,eisdir};
		file ->
			{error,enotdir}
		end;
	{error,_} ->
		{error,enoent}
	end.

dump_clunk({ok,{local,_,_}}) ->
	ok;
dump_clunk({ok,{ConnPid,Fid}}) ->
	'9p':clunk(ConnPid, Fid).

%%------------------------------------------------------------------------------

union(Fun, [Arg], _Dump) ->
	Fun(Arg);
union(Fun, List, Dump) ->
	Run = fanout(Fun, List),
	collect(Run, length(List), Dump).

union_all(Fun, [Arg]) ->
	case Fun(Arg) of
	{ok,Res} ->
		{ok,[Res]};
	{error,_} =Error ->
		Error
	end;
union_all(Fun, List) ->
	Run = fanout(Fun, List),
	collect_all(Run, length(List)).

fanout(Fun, List) ->
	Self = self(),
	Run = make_ref(),
	lists:foldl(fun(Arg, Prio) ->
		spawn(fun() -> Self ! {Run,Prio,Fun(Arg)} end),
		Prio +1
	end, 1, List),
	Run.

collect(Run, N, Dump) ->
	collect(Run, N, Dump, []).

collect(_Run, 0, Dump, Acc) ->
	{_,Results} = lists:unzip(lists:keysort(1, Acc)),
	OKs = lists:filter(fun({Tag,_}) -> Tag =:= ok end, Results),
	case lists:reverse(OKs) of
	[] ->
		[LastError|_] = Results,
		LastError;
	[LastOk|OtherOk] ->
		lists:foreach(Dump, OtherOk),
		LastOk
	end;
collect(Run, N, Dump, Acc) ->
	receive
	{Run,Prio,Res} ->
		collect(Run, N -1, Dump, [{Prio,Res}|Acc])
	end.

collect_all(Run, N) ->
	collect_all(Run, N, []).

collect_all(_Run, 0, Acc) ->
	{_,Results} = lists:unzip(lists:keysort(1, Acc)),
	case [R || {ok,R} <- Results] of
	[] ->
		[LastError|_] = Results,
		LastError;
	Rs ->
		{ok,Rs}
	end;
collect_all(Run, N, Acc) ->
	receive
	{Run,Prio,Res} ->
		collect_all(Run, N -1, [{Prio,Res}|Acc])
	end.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

unsplit(SPath) ->
	case [[$/,Elem] || Elem <- SPath] of
	[] ->
		<<"/">>;
	L ->
		list_to_binary(L)
	end.

open_mode(Modes) ->
	R = lists:member(read, Modes),
	W = lists:member(write, Modes),
	A = lists:member(append, Modes),
	if R and W -> open_mode([rdwr,creat], Modes);
	   R and A -> open_mode([rdwr,creat,append], Modes);
	   R -> 	  open_mode([rdonly], Modes);
	   W -> 	  open_mode([wronly,creat,trunc], Modes);
	   A ->		  open_mode([wronly,creat,append], Modes);
	   true ->	  open_mode([rdonly], Modes)
	end.

open_mode(Acc, Modes) -> open_mode(Acc, Modes, list).

open_mode(Acc, [], ReadFormat) ->
	{ReadFormat,Acc};
open_mode(Acc, [read|Modes], ReadFormat) ->
	open_mode(Acc, Modes, ReadFormat);
open_mode(Acc, [write|Modes], ReadFormat) ->
	open_mode(Acc, Modes, ReadFormat);
open_mode(Acc, [append|Modes], ReadFormat) ->
	open_mode(Acc, Modes, ReadFormat);
open_mode(Acc, [raw|Modes], ReadFormat) ->
	open_mode(Acc, Modes, ReadFormat);
open_mode(Acc, [binary|Modes], _RF) ->
	open_mode(Acc, Modes, binary);
open_mode(Acc, [exclusive|Modes], ReadFormat) ->
	open_mode([excl|Acc], Modes, ReadFormat);
open_mode(Acc, [compressed|Modes], ReadFormat) ->
	open_mode(Acc, Modes, ReadFormat);	%% ignore silently - dets wants it
open_mode(Acc, [delayed_write|Modes], ReadFormat) ->
	open_mode(Acc, Modes, ReadFormat);	%% ignore silently
open_mode(Acc, [{delayed_write,_,_}|Modes], ReadFormat) ->
	open_mode(Acc, Modes, ReadFormat);	%% ignore silently
open_mode(Acc, [read_ahead|Modes], ReadFormat) ->
	open_mode(Acc, Modes, ReadFormat);	%% ignore silently
open_mode(Acc, [{read_ahead,_}|Modes], ReadFormat) ->
	open_mode(Acc, Modes, ReadFormat);	%% ignore silently
open_mode(_Acc, _Ms, _RF) ->
	badarg.

dir_base(SFile) ->
	[Base|Rid] = lists:reverse(SFile),
	{lists:reverse(Rid),Base}.

props_to_file_info(Props, TimeType) ->
	Mode = proplists:get_value(mode, Props, 0),
	Rdev = proplists:get_value(rdev, Props, 0),
	#file_info{size =proplists:get_value(size, Props),
	   type =file_type(Mode band ?S_IFMT),
	   access =file_access(Mode band 8#600),
	   atime =from_seconds(proplists:get_value(atime_sec, Props), TimeType),
	   mtime =from_seconds(proplists:get_value(mtime_sec, Props), TimeType),
	   ctime =from_seconds(proplists:get_value(ctime_sec, Props), TimeType),
	   mode =Mode band 8#700,
	   links =proplists:get_value(nlink, Props),
	   major_device =Rdev bsr 8,
	   minor_device =Rdev band 255,
	   inode =proplists:get_value(ino, Props),
	   uid =proplists:get_value(uid, Props),
	   gid =proplists:get_value(gid, Props)}.

file_info_to_props(FileInfo, TimeType) ->
	if FileInfo#file_info.atime =/= undefined ->
			[{atime,to_seconds(FileInfo#file_info.atime, TimeType)
					*1000000000}];
	 			true -> [] end
		++
	if FileInfo#file_info.mtime =/= undefined ->
			[{mtime,to_seconds(FileInfo#file_info.mtime, TimeType)
					*1000000000}];
	 			true -> [] end
		++
	if FileInfo#file_info.mode =/= undefined ->
			[{mode,FileInfo#file_info.mode}];
				true -> [] end
		++
	if FileInfo#file_info.uid =/= undefined ->
			[{uid,FileInfo#file_info.uid}];
				true -> [] end
		++
	if FileInfo#file_info.gid =/= undefined ->
			[{gid,FileInfo#file_info.gid}];
				true -> [] end.

file_type(?S_IFDIR) -> directory;
file_type(?S_IFCHR) -> device;
file_type(?S_IFBLK) -> device;
file_type(?S_IFREG) -> regular;
file_type(?S_IFIFO) -> other;
file_type(?S_IFLNK) -> symlink;
file_type(?S_IFSOCK) -> other;
file_type(_) -> undefined.

file_access(8#400) -> read;
file_access(8#200) -> write;
file_access(8#600) -> read_write;
file_access(_) -> none.

posixtime_to_universaltime(Seconds) ->
	Epoch = {{1970,1,1},{0,0,0}},
	calendar:gregorian_seconds_to_datetime(Seconds +
		calendar:datetime_to_gregorian_seconds(Epoch)).

universaltime_to_posixtime(DateTime) ->
	Epoch = {{1970,1,1},{0,0,0}},
	calendar:datetime_to_gregorian_seconds(DateTime) -
		calendar:datetime_to_gregorian_seconds(Epoch).

from_seconds(undefined, _) ->
	undefined;
from_seconds(Seconds, posix) when is_integer(Seconds) ->
    Seconds;
from_seconds(Seconds, universal) when is_integer(Seconds) ->
	posixtime_to_universaltime(Seconds);
from_seconds(Seconds, local) when is_integer(Seconds) ->
	posixtime_to_universaltime(Seconds). %% local is UTC

to_seconds(Seconds, posix) when is_integer(Seconds) ->
    Seconds;
to_seconds({_,_} = DateTime, universal) ->
	universaltime_to_posixtime(DateTime);
to_seconds({_,_} = DateTime, local) ->
	universaltime_to_posixtime(DateTime). %% local is UTC

local_file_info(_SPath, TimeType) ->
	#file_info{size =0,
			   type =directory,
			   access =read,
			   atime =from_seconds(0, TimeType),
			   mtime =from_seconds(0, TimeType),
			   ctime =from_seconds(0, TimeType),
			   mode =?S_IFDIR bor 8#755,
			   uid =0,
			   gid =0}.

%%EOF
