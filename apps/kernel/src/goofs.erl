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

-module(goofs).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("9p.hrl").

-define(DEF_NUM_FILES, 254).
-define(JOURNAL_SIZE, 32).
-define(MIN_EXT_SIZE, 8192).
-define(ALLOC_ENTRY_SIZE, 4).
-define(ALLOC_PER_SECTOR, 128).
-define(JOUR_ENTRY_SIZE, 16).
-define(JOUR_PER_SECTOR, 32).
-define(ROOTREC_UNUSED, 488).
-define(SUPERB_UNUSED, 492).

-define(FORMAT_IN_PROGRESS, 16#ffff).

-define(MAX_CHUNK_SIZE, 64).	%% up to 88

-define(GOO_EXP_NAME, <<"$$goo$$">>).
-define(GOO_EXP_PATH, <<"/$$goo$$">>).

-define(FLUSH_EVERY_MS, 2000).

-record(f, {index,
			length,
			version,
			objid,
			mode,
			atime,
			mtime,
			name}).

-record(g, {nr_files,
			nr_extents,
			extent_size,
			filedir_starts,
			journal_starts,
			atab_starts,
			data_starts,
			root,
			files,
			unused,
			alloc,
			free,
			can_flush =false}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

-export([list_files/0,file_index/1]).
-export([root_stat/0,file_stat/1]).
-export([create_file/1,delete_file/1]).
-export([rename_file/2]).
-export([resize_file/2]).
-export([read_file/3,write_file/3]).

-export([check_consistency/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(MountAt, RawOpts) ->
	case init_opts(RawOpts) of
	Opts when is_list(Opts) ->
		Args = {MountAt,Opts},
		gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []);
	Error ->
		{error,Error}
	end.

list_files() ->
	gen_server:call(?SERVER, list_files).

file_index(Name) ->
	gen_server:call(?SERVER, {file_index,Name}).

root_stat() ->
	gen_server:call(?SERVER, root_stat).

file_stat(Name) ->
	gen_server:call(?SERVER, {file_stat,Name}).

create_file(Name) ->
	gen_server:call(?SERVER, {create_file,Name}).

delete_file(Name) ->
	gen_server:call(?SERVER, {delete_file,Name}).

rename_file(Name, NewName) ->
	gen_server:call(?SERVER, {rename_file,Name,NewName}).

resize_file(Name, Size) ->
	gen_server:call(?SERVER, {resize_file,Name,Size}).

read_file(Name, Offset, Count) ->
	gen_server:call(?SERVER, {read_file,Name,Offset,Count}).

write_file(Name, Offset, Data) ->
	gen_server:call(?SERVER, {write_file,Name,Offset,Data}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({MountAt,Opts}) ->
	DiskProps = erlang:disk_info(),
	if DiskProps =:= undefined ->
		{stop,no_disk};
	true ->
		case attach_volume(DiskProps) of
		{ok,St} ->
			mount_files(MountAt, St);
		{error,not_initialised} ->
			NumFiles = proplists:get_value(files, Opts, ?DEF_NUM_FILES),
			case init_volume(NumFiles, DiskProps) of
			{ok,_St} ->
				{ok,St1} = attach_volume(DiskProps),
				mount_files(MountAt, St1);
			{error,Reason} ->
				{stop,Reason}
			end;
		{error,Error} ->
			{stop,Error}
		end
	end.

set_flush_timer() ->
	erlang:send_after(?FLUSH_EVERY_MS, self(), disk_flush).

mount_files(MountAt, St) ->
	
	case '9p_server':add_export(?GOO_EXP_NAME, goo_export, []) of
	ok ->

		%%
		%% It may be possible to reuse the existing local connection instead of
		%% opening a new one.
		%%

		Map = [{list_to_binary(MountAt),?GOO_EXP_PATH}],
		case '9p_mounter':add_connection('9p_zero', '9p_zero', Map, []) of
		ok ->

			if St#g.can_flush ->
				set_flush_timer();
					true -> ok end,

			{ok,St};
		{error,_} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end.

handle_call(check, _From, St) ->
	lists:foreach(fun(Index) ->
		[] = [X || {_,Idx,_} =X <- St#g.alloc, Idx =:= Index]
	end, St#g.unused),
	{reply,ok,St};

handle_call(list_files, _From, #g{files =Files} =St) ->
	Reply = [{Name,Index} || #f{index =Index,name =Name} <- Files],
	{reply,Reply,root_accessed(St)};

handle_call({file_index,Name}, _From, #g{files =Files} =St) ->
	case lists:keyfind(Name, #f.name, Files) of
	false ->
		{reply,not_found,St};
	#f{index =Index} ->
		{reply,Index,St}
	end;

handle_call(root_stat, _From, #g{root =Root} =St) ->
	#f{version =Version,
	   objid =ObjId,
	   mode =Mode,
	   atime =ATime,
	   mtime =MTime} = Root,

	Reply = {ok,
		#stat{qid =qid(?QTDIR, Version, ObjId),
			  mode =Mode,
			  atime =ATime,
			  mtime =MTime,
			  length =0,
			  name = <<"/">>}},
	{reply,Reply,St};

handle_call({file_stat,What}, _From, #g{files =Files} =St) ->
	Col = if is_binary(What) -> #f.name;
			true -> #f.index end,
	case lists:keyfind(What, Col, Files) of
	false ->
		{error,enoent};

	#f{length =Length,
	   version =Version,
	   objid =ObjId,
	   mode =Mode,
	   atime =ATime,
	   mtime =MTime,
	   name =Name} ->

		Reply = {ok,
			#stat{qid =qid(?QTFILE, Version, ObjId),
				  mode =Mode,
				  atime =ATime,
				  mtime =MTime,
				  length =Length,
				  name =Name}},
		{reply,Reply,St}
	end;

handle_call({create_file,Name}, _From, #g{root =Root,files =Files} =St) ->
	case lists:keyfind(Name, #f.name, Files) of
	false when St#g.unused =:= [] ->
		{reply,{error,too_many_files},St};
	false ->
		[Index|Unused] = St#g.unused,

		TS = '9p':timestamp(),
		File = #f{index =Index,
				  length =0,
				  version =0,
				  objid =crypto:rand_bytes(8),
				  mode =8#644, %% magic
				  atime =TS,
				  mtime =TS,
				  name =Name},

		Root1 = Root#f{version =Root#f.version +1,
					   atime =TS,
					   mtime =TS},
		ok = transact([{St#g.filedir_starts +Index,pack_file_entry(File)},
				  	   {1,pack_root_record(Root1)}], St),
		St1 = St#g{root =Root1,files =[File|Files],unused =Unused},
		{reply,ok,St1};

	_ ->
		{reply,{error,already_exists},St}
	end;

handle_call({delete_file,What}, _From, #g{root =Root,
										  files =Files,
										  alloc =Alloc,
										  free =Free} =St) ->
	Col = if is_binary(What) -> #f.name;
			true -> #f.index end,
	case lists:keytake(What, Col, Files) of
	false ->
		{reply,{error,not_found},St};

	{value,#f{index =Index},Files1} ->

		{Alloc0,Alloc1} = lists:partition(fun({_,Idx,_}) ->
									Idx =:= Index end, Alloc),
		ExtsToFree = [Ext || {Ext,_,_} <- Alloc0],
		AllocTasks = alloc_update_tasks(ExtsToFree, Alloc1, St),

		TS = '9p':timestamp(),
		Root1 = Root#f{version =Root#f.version +1,
					   atime =TS,
					   mtime =TS},
		ok = transact([{St#g.filedir_starts +Index,all_zeros()},
				  	   {1,pack_root_record(Root1)}] ++ AllocTasks, St),
		St1 = St#g{root =Root1,
				   files =Files1,
				   unused =[Index|St#g.unused],
				   alloc = Alloc1,
				   free = ExtsToFree ++ Free},
		{reply,ok,St1}
	end;

handle_call({rename_file,What,NewName}, _From, #g{root =Root,
												  files =Files} =St) ->
	Col = if is_binary(What) -> #f.name;
			true -> #f.index end,
	case lists:keytake(What, Col, Files) of
	false ->
		{reply,{error,not_found},St};

	{value,#f{name =NewName},_} ->
		{reply,ok,St};	%% same name

	{value,#f{index =Index} =File,Files1} ->

		File1 = File#f{name =NewName},

		case lists:keytake(NewName, #f.name, Files1) of
		{value,#f{index =Index2},Files2} ->

			%% destructive rename

			{Alloc0,Alloc1} = lists:partition(fun({_,Idx,_}) ->
									Idx =:= Index2 end, St#g.alloc),
			ExtsToFree = [Ext || {Ext,_,_} <- Alloc0],
			AllocTasks = alloc_update_tasks(ExtsToFree, Alloc1, St),

			TS = '9p':timestamp(),
			Root1 = Root#f{version =Root#f.version +1,
					   atime =TS,
					   mtime =TS},
			DirTasks = [{St#g.filedir_starts +Index2,all_zeros()},
						{St#g.filedir_starts +Index,pack_file_entry(File1)},
				  		{1,pack_root_record(Root1)}],
			St1 = St#g{root =Root1,
					   files =[File1|Files2],
					   unused =[Index2|St#g.unused],
					   alloc =Alloc1,
					   free = ExtsToFree ++ St#g.free},
			ok = transact(AllocTasks ++ DirTasks, St1),
			{reply,ok,St1};
		false ->

			%% the file's times are not updated

			TS = '9p':timestamp(),
			Root1 = Root#f{version =Root#f.version +1,
					   atime =TS,
					   mtime =TS},
			Tasks = [{St#g.filedir_starts +Index,pack_file_entry(File1)},
				  	 {1,pack_root_record(Root1)}],
			St1 = St#g{root =Root1,
					   files =[File1|Files1]},
			ok = transact(Tasks, St1),
			{reply,ok,St1}
		end
	end;

handle_call({resize_file,What,ReqSize}, _From, #g{extent_size =ExtSz,
												  files =Files,
												  alloc =Alloc,
												  free =Free} =St) ->
	Col = if is_binary(What) -> #f.name;
			true -> #f.index end,
	case lists:keytake(What, Col, Files) of
	false ->
		{reply,{error,not_found},St};

	{value,#f{length =ReqSize},_} -> %% unchanged
		{reply,{ok,ReqSize},St};

	{value,#f{index =Index,
			  length =Length} =File,Files1} when Length > ReqSize ->

		NumExt = size_to_extents(ReqSize, ExtSz),
		{Alloc0,Alloc1} = lists:partition(fun({_,Index1,Offset}) ->
								Index1 =:= Index andalso Offset >= NumExt end, Alloc),
		Exts = [Ext || {Ext,_,_} <- Alloc0],
		case resize_it(File#f{length =ReqSize}, Files1,
						Exts, Alloc1, Exts ++ Free, St) of
		{ok,St1} ->
			{reply,{ok,ReqSize},St1};
		_ ->
			{reply,{ok,Length},St}	%% length left unchanged
		end;

	{value,#f{length =Length} =File,Files1} ->

		case grow_it(File, Files1, ReqSize, St) of
		{ok,St1} ->
			{reply,{ok,ReqSize},St1};
		_ ->
			{reply,{ok,Length},St}	%% length untouched
		end
	end;

handle_call({read_file,What,Offset,Count}, _From, #g{files =Files} =St) ->
	Col = if is_binary(What) -> #f.name;
			true -> #f.index end,
	case lists:keytake(What, Col, Files) of
	false ->
		{reply,{error,not_found},St};

	{value,#f{length =Length,index =Index} =File,Files1} ->

		%% prune reading range
		Count1 = if Offset >= Length ->
				0;
			Offset +Count > Length ->
				Length -Offset;
			true ->
				Count
			end,

		if Count1 =:= 0 ->
			{reply,{ok,<<>>},St};
		true ->

			SecStart = Offset div 512,
			SecEnd = (Offset +Count1 -1) div 512, %% inclusive
			ForeSkip = Offset rem 512,

			case read_sector_range(SecStart, SecEnd, Index, St) of
			{ok,Chunks} ->
				B = list_to_binary(Chunks),
				<<_:(ForeSkip)/binary,Data:(Count1)/binary,_/binary>> = B,

				File1 = File#f{atime ='9p':timestamp()},
				FTasks = [{St#g.filedir_starts +Index,
								pack_file_entry(File1)}],
				ok = transact(FTasks, St),

				{reply,{ok,Data},St#g{files =[File1|Files1]}};

			{error,_} =Error ->
				{reply,Error,St}
			end
		end
	end;

handle_call({write_file,What,Offset,Data}, _From, St) ->
	do_write_file(What, Offset, Data, St).

do_write_file(_, _, <<>>, St) ->
	{reply,ok,St};

do_write_file(What, Offset, Data, #g{files =Files} =St) ->
	Col = if is_binary(What) -> #f.name;
			true -> #f.index end,
	case lists:keytake(What, Col, Files) of
	false ->
		{reply,{error,not_found},St};

	{value,#f{length =Length,index =Index} =File,Files1} ->

		Count = byte_size(Data),
		if Offset +Count > Length ->
		
			case grow_it(File, Files1, Offset +Count, St) of
			{ok,St1} ->
				do_write_file(What, Offset, Data, St1);		%% retry 

			{error,_} =Error ->
				{reply,Error,St}
			end;
		true ->

			case patch_to_whole_sector(Data,
							Offset, Length, Index, St) of
			{ok,Data1} ->
				Start = Offset div 512,
				Res = case write_sector_range(Start, Data1, Index, St) of
				{ok,_} ->

					%%
					%% It is possible to produce a more accurate count of how
					%% many bytes were actually written by examining the value
					%% returned by write_sector_rang(). We are taking an easy path.
					%%
					{ok,Count};

				Other ->	
					Other
				end,

				TS = '9p':timestamp(),
				File1 = File#f{version =File#f.version +1,
							   atime =TS,
							   mtime =TS},
				FTasks = [{St#g.filedir_starts +Index,
								pack_file_entry(File1)}],
				ok = transact(FTasks, St),

				{reply,Res,St#g{files =[File1|Files1]}};
				
			{error,_} =Error ->
				{reply,Error,St}
			end
		end
	end.

patch_to_whole_sector(Data, Offset, Length, Index, St) ->
	case Offset rem 512 of
	0 ->
		patch_to_whole_aft(Data, Offset, Length, Index, St);
	ChipSz ->
		FileSec = Offset div 512,
		PhySec = file_to_phy_sector(FileSec, Index, St),
		case disk:read(PhySec, 1) of
		{ok,<<Chip:(ChipSz)/binary,_/binary>>} ->
			Data1 = <<Chip/binary,Data/binary>>,
			patch_to_whole_aft(Data1, Offset -ChipSz, Length, Index, St);
		{error,_} =Error ->
			Error
		end
	end.

patch_to_whole_aft(Data, Offset, Length, Index, St) ->
	Count = byte_size(Data),
	ExtraSz = (Offset +Count) rem 512,

	if ExtraSz =:= 0 ->
		{ok,Data};

	Offset +Count =:= Length ->
		
		%% No need to read the sector - pad with zeros
		ChipSz = 512 -ExtraSz,
		Chip = <<0:ChipSz/unit:8>>,
		Data1 = <<Data/binary,Chip/binary>>,
		{ok,Data1};
		
	true ->
		FileSec = (Offset +Count -1) div 512,
		PhySec = file_to_phy_sector(FileSec, Index, St),
		case disk:read(PhySec, 1) of
		{ok,<<_:(ExtraSz)/binary,Chip/binary>>} ->
			Data1 = <<Data/binary,Chip/binary>>,
			{ok,Data1};
		{error,_} =Error ->
			Error
		end
	end.

file_to_phy_sector(FileSec, Index, #g{extent_size =ExtSz,
									  alloc =Alloc} =St) ->
	ExtOff = FileSec div ExtSz,
	SecOff = FileSec rem ExtSz,
	[Ext] = [Ext || {Ext,Index1,ExtOff1} <- Alloc,
						Index1 =:= Index, ExtOff1 =:= ExtOff],
	extent_starts(Ext, St) +SecOff.

resize_it(#f{version =Version,index =Index} =File, Files, Exts, Alloc, Free, St) ->
	TS = '9p':timestamp(),
	File1 = File#f{version =Version +1,
				   atime =TS,
				   mtime =TS},
	FileTasks = [{St#g.filedir_starts +Index,
							pack_file_entry(File1)}],
	AllocTasks = alloc_update_tasks(Exts, Alloc, St),
	St1 = St#g{files =[File1|Files],
			   alloc =Alloc,
			   free =Free},
	case transact(FileTasks ++ AllocTasks, St1) of
	ok ->
		{ok,St1};
	{error,_} =Error ->
		Error
	end.

grow_it(#f{length =Length,index =Index} =File, Files1, GrowSize, #g{extent_size =ExtSz,
											   		  				alloc =Alloc,
											   		  				free =Free} =St) ->
	OldNumExt = size_to_extents(Length, ExtSz),
	NumExt = size_to_extents(GrowSize, ExtSz),
	Delta = NumExt -OldNumExt,
	if Delta > length(Free) ->
		{error,no_data_space};

	true ->
		{Exts,Free1} = lists:split(Delta, Free),
		{Alloc0,_} = lists:mapfoldl(fun(Ext, Off) ->
				{{Ext,Index,Off},Off +1}
		end, OldNumExt, Exts),
		Alloc1 = Alloc0 ++ Alloc,

		resize_it(File#f{length =GrowSize}, Files1,
								Exts, Alloc1, Free1, St)
	end.

handle_cast(_Msg, St) ->
    {noreply,St}.

handle_info(disk_flush, St) ->
	%erlang:display(disk_flush),
	disk:flush(),
	set_flush_timer(),
    {noreply,St}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

init_opts(Args) ->
	init_opts(Args, []).

init_opts([], Acc) ->
	lists:reverse(Acc);
init_opts(["files",S|Args], Acc) ->
	init_opts(Args, [{files,list_to_integer(S)}|Acc]);
init_opts(_, _) ->
	badarg.

attach_volume(DiskProps) ->

	case clean_volume(DiskProps) of
	{ok,St} ->
		Features = proplists:get_value(features, DiskProps),
		CanFlush = lists:member(flush, Features),

		{ok,Root} = read_root_record(St),
		{ok,Files,Unused} = read_directory(St),
		{ok,Alloc,Free} = read_alloc_table(St),

		{ok,St#g{root =Root,
				 files =Files,
				 unused =Unused,
				 alloc =Alloc,
				 free =Free,
				 can_flush =CanFlush}};

	{error,_} =Error ->
		Error
	end.

clean_volume(_DiskProps) ->

	case read_superblock() of
	{clean,St} ->
		{ok,St};

	{format_in_progress,St} ->
		error_logger:info_msg("Incomplete format detected, retrying...~n"),
		complete_format(St);

	{incomplete_transaction,TransLen,St} ->
		error_logger:info_msg("Incomplete transaction detected, rerunning...~n"),
		complete_transaction(TransLen, St);

	{error,_} =Error ->
		Error
	end.

init_volume(NumFiles, DiskProps) ->
	NumSectors = proplists:get_value(sectors, DiskProps),
	NumDataSectors = NumSectors
			- 1				%% superblock
			- 1				%% root record
			- NumFiles		%% file directory
			- ?JOURNAL_SIZE	%% transaction journal
			- 512,			%% allocation table (max)

	ExtentSize = extent_size(?MIN_EXT_SIZE, NumDataSectors),
	NumExtents = NumDataSectors div ExtentSize,
	AllocTabSize = (NumExtents + ?ALLOC_PER_SECTOR -1) div ?ALLOC_PER_SECTOR,

	St = #g{nr_files =NumFiles,
			nr_extents =NumExtents,
			extent_size =ExtentSize,
			filedir_starts =1 +1,
			journal_starts =1 +1 +NumFiles,
			atab_starts =1 +1 +NumFiles +?JOURNAL_SIZE,
			data_starts =1 +1 +NumFiles +?JOURNAL_SIZE +AllocTabSize},

	%% Transaction starts
	ok = update_superblock(?FORMAT_IN_PROGRESS, St),

	complete_format(St).

complete_format(#g{nr_files =NumFiles,
				   extent_size =ExtentSize,
				   nr_extents =NumExtents} =St) ->

	AllocTabSize = (NumExtents + ?ALLOC_PER_SECTOR -1) div ?ALLOC_PER_SECTOR,

	%% root record
	ok = init_root_record(St),

	%% file directory
	lists:foreach(fun(FileIndex) ->
		ok = disk:write(St#g.filedir_starts +FileIndex, all_zeros())
	end, lists:seq(0, NumFiles -1)),

	%% allocation table
	lists:foreach(fun(N) ->
		ok = disk:write(St#g.atab_starts +N, all_ones())
	end, lists:seq(0, AllocTabSize -1)),

	%% complete transaction
	ok = update_superblock(0, St),

	error_logger:info_report(["Disk formatted successfully",
							  {nr_files,NumFiles},
							  {extent_size,ExtentSize},
							  {nr_extents,NumExtents}]),
	{ok,St}.

complete_transaction(TransLen, #g{} =St) ->

	%% transaction interrupted - time to retry

	case read_journal(TransLen, St) of
	{ok,Journal} ->

		lists:foreach(fun({SourceSec,TargetSec}) ->
			{ok,Data} = disk:read(SourceSec, 1),
			ok = disk:write(TargetSec, Data)
		end, Journal),
		
		ok = update_superblock(0, St),

		error_logger:info_msg("Transaction completed successfully~n"),
		{ok,St};
	{error,_} =Error ->
		Error
	end.

extent_size(ExtSz, NumDataSectors) when ExtSz * 65536 >= NumDataSectors ->
	ExtSz;
extent_size(ExtSz, NumDataSectors) ->
	extent_size(ExtSz * 2, NumDataSectors).

%%------------------------------------------------------------------------------

read_superblock() ->
	{Magic,Version} = magic_version(),

	case disk:read(0, 1) of
	{ok,<<NumFiles:16,
		  NumExtents:16,
		  ExtentSize:32,
		  Magic:64,
		  Version:16,
		  TransLen:16,
		  _:(?SUPERB_UNUSED)/binary>>} ->

		AllocTabSize = (NumExtents + ?ALLOC_PER_SECTOR -1) div ?ALLOC_PER_SECTOR,
		St = #g{nr_files =NumFiles,
				nr_extents =NumExtents,
				extent_size =ExtentSize,
				filedir_starts =1 +1,
				journal_starts =1 +1 +NumFiles,
				atab_starts =1 +1 +NumFiles +?JOURNAL_SIZE,
				data_starts =1 +1 +NumFiles +?JOURNAL_SIZE +AllocTabSize},

		case TransLen of
		0 ->
			{clean,St};
		
		?FORMAT_IN_PROGRESS ->
			{format_in_progress,St};

		N ->
			{incomplete_transaction,N,St}
		end;

	{ok,Data} ->
		ZZ = all_zeros(),
		if Data =:= ZZ ->
			{error,not_initialised};
		true ->
			{error,unknown_filesystem}
		end;

	{error,_} =Error ->	
		Error
	end.
	
update_superblock(TransLen, #g{nr_files =NumFiles,
							   nr_extents =NumExtents,
							   extent_size =ExtentSize}) ->
	%%
	%% In the better world, we would have used a disk barrier here.
	%% Unfortunately, it takes prohibitely long. Stanard Linux filesystems have
	%% barriers disabled by default.
	%%
	%%disk:barrier(),

	{Magic,Version} = magic_version(),
	Zeros = << <<Z>> || Z <- lists:duplicate(?SUPERB_UNUSED, 0) >>,
	Super = <<NumFiles:16,
			  NumExtents:16,
			  ExtentSize:32,
			  Magic:64,
			  Version:16,
			  TransLen:16,
			  Zeros/binary>>,
	disk:write(0, Super).

init_root_record(_St) ->

	TS = '9p':timestamp(),
	F = #f{version =0,
		   objid =crypto:rand_bytes(8),
		   mode =?DMDIR bor 8#755,	%% magic
		   atime =TS,
		   mtime =TS},

	disk:write(1, pack_root_record(F)).

read_root_record(_St) ->
	case disk:read(1, 1) of
	{ok,<<Version:32,
		  ObjId:8/binary,
		  Mode:32,
		  ATime:32,
		  MTime:32,_/binary>>} ->
		
		F = #f{version =Version,
			   objid =ObjId,
			   mode =Mode,
			   atime =ATime,
			   mtime =MTime},
		{ok,F};

	{error,_} =Error ->
		Error
	end.

pack_root_record(#f{version =Version,
					objid =ObjId,
					mode =Mode,
					atime =ATime,
					mtime =MTime}) ->
	Padding = << <<0>> || _ <- lists:duplicate(?ROOTREC_UNUSED, 0)>>,
	<<Version:32,
	  ObjId:8/binary,
	  Mode:32,
	  ATime:32,
	  MTime:32,
	  Padding/binary>>.

pack_file_entry(#f{length =Length,
				   version =Version,
				   objid =ObjId,
				   mode =Mode,
				   atime =ATime,
				   mtime =MTime,
				   name =Name}) ->
	L = 512 -2 -8 -4 -8 -4 -4 -4 -2 -size(Name),
	Padding = << <<0>> || _ <- lists:duplicate(L, 0)>>,
	<<16#ffff:16,	%% occupied
	  Length:64,
	  Version:32,
	  ObjId:8/binary,
	  Mode:32,
	  ATime:32,
	  MTime:32,
	  (size(Name)):16,Name/binary,
	  Padding/binary>>.

read_directory(#g{filedir_starts =Start,nr_files =NumFiles}) ->

	{Files,Unused} = lists:foldl(fun(Index, {Files,Unused}) ->
		case disk:read(Start +Index, 1) of
		{ok,<<0:16,_/binary>>} ->
			{Files,[Index|Unused]};

		{ok,<<16#ffff:16,	%% occupied
			  Length:64,
			  Version:32,
			  ObjId:8/binary,
			  Mode:32,
			  ATime:32,
			  MTime:32,
			  NSz:16,Name:(NSz)/binary,
			  _/binary>>} ->

			File = #f{index =Index,
					  length =Length,
					  version =Version,
					  objid =ObjId,
					  mode =Mode,
					  atime =ATime,
					  mtime =MTime,
					  name =Name},

			{[File|Files],Unused}
		end
	end, {[],[]}, lists:seq(0, NumFiles -1)),

	{ok,lists:reverse(Files),lists:reverse(Unused)}.

read_alloc_table(#g{nr_extents =NumExtents,atab_starts =Sec}) ->
	case read_alloc_table(NumExtents, Sec, []) of
	{ok,T} ->
		{_,RAlloc,RFree} = lists:foldl(fun({16#ffff,_}, {N,Alloc,Free}) ->
				{N +1,Alloc,[N|Free]};
			({FileIndex,Offset}, {N,Alloc,Free}) ->
				{N +1,[{N,FileIndex,Offset}|Alloc],Free}
		end, {0,[],[]}, T),
		{ok,lists:reverse(RAlloc),lists:reverse(RFree)};
		
	{error,_} =Error ->
		Error
	end.

read_alloc_table(0, _Sec, Acc) ->
	{ok,lists:concat(lists:reverse(Acc))};
read_alloc_table(N, Sec, Acc) ->
	case disk:read(Sec, 1) of
	{ok,Data} when N >= ?ALLOC_PER_SECTOR ->
		Tab = [{Index,Offset} || <<Index:16,Offset:16>> <= Data],
		read_alloc_table(N - ?ALLOC_PER_SECTOR, Sec +1, [Tab|Acc]);

	{ok,Data} ->
		Data1 = binary:part(Data, 0, N*?ALLOC_ENTRY_SIZE),
		Tab = [{Index,Offset} || <<Index:16,Offset:16>> <= Data1],
		read_alloc_table(0, Sec +1, [Tab|Acc]);
	
	{error,_} =Error ->
		Error
	end.

read_journal(N, #g{journal_starts =Sec}) ->
	read_journal(N, Sec, []).

read_journal(0, _Sec, Acc) ->
	{ok,lists:concat(lists:reverse(Acc))};
read_journal(N, Sec, Acc) ->
	case disk:read(Sec, 1) of
	{ok,Data} when N >= ?JOUR_PER_SECTOR ->
		Tab = [{SSec,TSec} || <<SSec:64,TSec:64>> <= Data],
		read_journal(N - ?JOUR_PER_SECTOR, Sec +1, [Tab|Acc]);

	{ok,Data} ->
		Data1 =	binary:part(Data, 0, N*?JOUR_ENTRY_SIZE),
		Tab = [{SSec,TSec} || <<SSec:64,TSec:64>> <= Data1],
		read_journal(0, Sec +1, [Tab|Acc]);

	{error,_} =Error ->
		Error
	end.

root_accessed(#g{root =Root} =St) ->
	Root1 = Root#f{atime ='9p':timestamp()},
	ok = transact([{1,pack_root_record(Root1)}], St),
	St#g{root =Root1}.

read_sector_range(Start, End, Index, St) ->
	read_sector_range(Start, End -Start +1, Index, St, []).

read_sector_range(Start, Left, Index, #g{extent_size =ExtSz,
										 alloc =Alloc} =St, Acc) ->
	ExtOff = Start div ExtSz,
	SecOff = Start rem ExtSz,
	[Ext] = [Ext || {Ext,Index1,ExtOff1} <- Alloc,
						Index1 =:= Index, ExtOff1 =:= ExtOff],
	ExtStart = extent_starts(Ext, St) +SecOff,
	ExtLeft = ExtSz -SecOff,
	read_range_from_ext(Start, Left, Index, St, Acc, ExtStart, ExtLeft).

read_range_from_ext(_, 0, _, _, Acc, _, _) ->
	{ok,lists:reverse(Acc)};
read_range_from_ext(Start, Left, Index, St, Acc, _, 0) ->
	read_sector_range(Start, Left, Index, St, Acc); 
read_range_from_ext(Start, Left, Index, St, Acc, ExtStart, ExtLeft) ->
	ChunkSz = lists:min([Left,ExtLeft,?MAX_CHUNK_SIZE]),
	%io:format("read_range_from_ext: ~w ~w~n", [ExtStart,ChunkSz]),
	case disk:read(ExtStart, ChunkSz) of
	{ok,Chunk} ->
		read_range_from_ext(Start +ChunkSz, Left -ChunkSz,
				Index, St, [Chunk|Acc], ExtStart +ChunkSz, ExtLeft -ChunkSz);
	{error,_} =Error ->
		Error
	end.

write_sector_range(Start, Data, Index, St) ->
	write_sector_range(Start, Data, Index, St, 0).

write_sector_range(Start, Data, Index, #g{extent_size =ExtSz,
										  alloc =Alloc} =St, Total) ->
	ExtOff = Start div ExtSz,
	SecOff = Start rem ExtSz,
	[Ext] = [X || {X,Index1,ExtOff1} <- Alloc,
						Index1 =:= Index, ExtOff1 =:= ExtOff],
	ExtStart = extent_starts(Ext, St) +SecOff,
	ExtLeft = ExtSz -SecOff,
	write_range_to_ext(Start, Data, Index, St, Total, ExtStart, ExtLeft).

write_range_to_ext(_, <<>>, _, _, Total, _, _) ->

	{ok,Total};

write_range_to_ext(Start, Data, Index, St, Total, _, 0) ->
	write_sector_range(Start, Data, Index, St, Total); 
write_range_to_ext(Start, Data, Index, St, Total, ExtStart, ExtLeft) ->
	Left = byte_size(Data) div 512,
	ChunkSz = lists:min([Left,ExtLeft,?MAX_CHUNK_SIZE]),
	%io:format("write_range_to_ext: ~w ~w~n", [ExtStart,ChunkSz]),
	ChunkByteSz = ChunkSz *512,
	<<Chunk:(ChunkByteSz)/binary,Rest/binary>> =Data,
	case disk:write(ExtStart, Chunk) of
	ok ->
		write_range_to_ext(Start +ChunkSz, Rest,
				Index, St, Total +ChunkByteSz, ExtStart +ChunkSz, ExtLeft -ChunkSz);
	{error,_} =Error ->
		Error
	end.

%%------------------------------------------------------------------------------

extent_starts(Ext, #g{extent_size=Sz,data_starts =Start}) ->
	Start + Ext *Sz.

alloc_update_tasks(Exts, Alloc, #g{atab_starts =Start}) ->
	AffSecs = lists:usort([Ext div ?ALLOC_PER_SECTOR
					|| Ext <- Exts]),
	lists:map(fun(S) ->
		From = S *?ALLOC_PER_SECTOR,
		To = From +?ALLOC_PER_SECTOR -1,
		Range = [T || {N,_,_} =T <- Alloc, N >= From, N =< To],

		Data = << <<I:16,O:16>>
				|| {_,I,O} <- fill_in_range(lists:keysort(1, Range), From, To)>>,
		{Start +S,Data}
	end, AffSecs).

fill_in_range(Range, From, To) ->
	fill_in_range(Range, From, To +1, []).

fill_in_range(_Range, N, N, Acc) ->
	lists:reverse(Acc);
fill_in_range([{N,_,_} =T|Range], N, M, Acc) ->
	fill_in_range(Range, N +1, M, [T|Acc]);
fill_in_range(Range, N, M, Acc) ->
	fill_in_range(Range, N +1, M, [{N,65535,65535}|Acc]).

transact([{Sec,Data}], _St) ->	%% atomic
	disk:write(Sec, Data);

transact(Writes, _St)
		when length(Writes) > ?JOURNAL_SIZE * ?JOUR_PER_SECTOR ->
	{error,transaction_too_long};

transact(Writes, #g{extent_size =ExtSize,free =Free})
		when length(Writes) > length(Free) * ExtSize ->
	{error,no_space_left};

transact(Writes, #g{free =Free} =St) ->
	case write_trans_temp(Writes, Free, St) of
	{ok,Journal} ->
		case write_trans_journal(Journal, St) of
		ok ->
			update_superblock(length(Journal), St),
			%% Stage 1 complete

			ok = write_trans_real(Writes, St),
			update_superblock(0, St);
			%% Stage 2 complete

		{error, _} =Error ->
			Error
		end;
	{error,_} =Error ->
		Error
	end.	

write_trans_temp(Writes, [Ext|Exts], #g{extent_size =N} =St) ->
	Starts = extent_starts(Ext, St),
	write_trans_temp(Writes, Starts, N, [], Exts, St).

write_trans_temp([], _Sec, _N, Acc, _Exts, _St) ->
	{ok,lists:reverse(Acc)};
write_trans_temp(Writes, _Sec, 0, Acc, [Ext|Exts], St) ->
	Starts = extent_starts(Ext, St),
	write_trans_temp(Writes, Starts, St#g.extent_size, Acc, Exts, St);
write_trans_temp([{Target,Data}|Writes], Sec, N, Acc, Exts, St) ->
	case disk:write(Sec, Data) of
	ok ->
		write_trans_temp(Writes, Sec +1, N -1, [{Sec,Target}|Acc], Exts, St);
	{error,_} =Error ->
		Error
	end.

write_trans_journal(Journal, #g{journal_starts =Sec})
		when length(Journal) * ?JOUR_ENTRY_SIZE =< 32768 ->	%% disk driver limit <44K
	B = << <<Src:64,Tgt:64>> || {Src,Tgt} <- Journal >>,
	{Data,_} = align_to_sector(B),
	disk:write(Sec, Data).

write_trans_real([], _St) ->
	ok;
write_trans_real([{Target,Data}|Writes], St) ->
	case disk:write(Target, Data) of
	ok ->
		write_trans_real(Writes, St);
	{error,_} =Error ->
		Error
	end.

magic_version() ->
	{16#2f476f6f2f46532f,1}.

all_zeros() ->
	<<0:512/unit:8>>.

all_ones() ->
	<< <<255>> || _ <- lists:duplicate(512, 255)>>.

align_to_sector(B) ->
	Sz = byte_size(B),
	N = (Sz +512 -1) div 512,
	ASz = N * 512,
	if ASz =:= Sz ->
		{B,N};
	true ->
		Padding = <<0:(ASz -Sz)/unit:8>>,
		{<<B/binary,Padding/binary>>,N}
	end.

size_to_extents(Size, ExtSz) ->
	Secs = (Size +512 -1) div 512,
	(Secs +ExtSz -1) div ExtSz.

qid(Type, Version, ObjId) ->
	<<Type,
	  Version:32,
	  ObjId:8/binary>>.

%%------------------------------------------------------------------------------

check_consistency() ->
	case erlang:disk_info() of
	undefined ->
		{error,no_disk};
	DiskProps ->
		check_consistency(DiskProps)
	end.

check_consistency(DiskProps) ->
	NumSectors = proplists:get_value(sectors, DiskProps),
	case read_superblock() of
	{clean,St} ->
		{ok,Root} = read_root_record(St),
		{ok,Files,Unused} = read_directory(St),
		{ok,Alloc,Free} = read_alloc_table(St),

		St1 = St#g{root =Root,files =Files,unused =Unused,alloc =Alloc,free =Free},
		case catch run_consistency_checks(St1, NumSectors) of
		{'EXIT',Reason} ->
			io:format("*** Consistency check failed: ~p~n~p~n",
								[Reason,erlang:get_stacktrace()]),
			{error,Reason};
		ok ->
			ok
		end;
		
	{format_in_progress,_} ->
		io:format("Incomplete format detected - cannot check consistency~n"),
		{warning,format_in_progress};
	{incomplete_transaction,_,_} ->
		io:format("Incomplete transaction detected - cannot check consistency~n"),
		{warning,incomplete_transaction};
	{error,_} =Error ->
		Error
	end.

run_consistency_checks(#g{nr_files =NumFiles,
						  nr_extents =NumExtents,
						  extent_size =ExtSz,
						  filedir_starts =DirStarts,
						  journal_starts =JournalStarts,
						  atab_starts =AllocTabStarts,
						  data_starts =DataStarts,
						  files =Files,
						  unused =Unused,
						  alloc =Alloc,
						  free =Free}, NumSectors) ->
	true = DataStarts + NumExtents *ExtSz =< NumSectors,
	true = NumFiles >= 254,
	true = DirStarts +NumFiles =< JournalStarts,
	true = JournalStarts + ?JOURNAL_SIZE =< AllocTabStarts,
	true = DataStarts >= AllocTabStarts,

	true = length(Files) + length(Unused) =:= NumFiles,

	true = lists:seq(0, NumFiles -1) =:=
		   lists:sort(Unused ++ [Index || #f{index =Index} <- Files]),

	true = lists:seq(0, NumExtents -1) =:=
		   lists:sort(Free ++ [Ext || {Ext,_,_} <- Alloc]),

	lists:foreach(fun(#f{index =Index,length =Length}) ->
		NumExt = size_to_extents(Length, ExtSz),
		true = lists:seq(0, NumExt -1) =:=
			lists:sort([Offset || {_,Index1,Offset} <- Alloc,
					Index1 =:= Index])
	end, Files),
	
	ok.

%%EOF

