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
%%% @docfile "doc/app/9p.edoc"
%%%

-module('9p').
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("9p.hrl").

-record(st, {sock,
		 	 trans_mod,
			 trans_conf,
			 data_tag,
			 close_tag,
			 error_tag,
			 is_local,
			 ver =e,
			 msize,
			 auth_user = <<"root">>,
			 auth_user_id = 0,
			 auth_path = <<"/">>,
			 unix_uid = 0,
			 unix_gid = 0,
			 afid,
			 session_key,
			 state,
			 mounter ='9p_mounter',
			 terminator,
			 close_result =ok,
			 attach_to,
			 attach_fids =[],
		 	 tags = [],
		 	 next_tag = 0,
		 	 fids = [],
		 	 next_fid = 0,
			 pending = [],
			 offsets = []}).

-define(CONN_TIMEO, 5000).

-define(NOGROUP, -1).

-define(DIR_MODE, 8#755).
-define(FILE_MODE, 8#755).

-record(setattr, {mode =0, uid =0, gid =0, size =0, atime_sec =0, atime_nsec =0,
		mtime_sec =0, mtime_nsec =0, ctime_sec =0, ctime_nsec =0}).

%% NB: The following constants are part of POSIX spec.
-define(S_IFMT,		8#0170000).	%% File type mask
-define(S_IFDIR,	8#0040000).	%% Directory
-define(S_IFCHR,	8#0020000).	%% Character device
-define(S_IFBLK,	8#0060000).	%% Block device
-define(S_IFREG,	8#0100000).	%% Regular file
-define(S_IFIFO,	8#0010000).	%% FIFO
-define(S_IFLNK,	8#0120000).	%% Symbolic link
-define(S_IFSOCK,	8#0140000).	%% Socket

-type option() :: {version,'9P2000.e'} | {version,'9P2000.L'} |
				  {msize,integer()} |
				  {auth_user,binary()} | {auth_user_id,integer()} |
				  {auth_path,binary()} |
				  {unix_uid,integer()} | {unix_gid,integer()}.
-type flag() :: rdonly | wronly | rdwr | excl | trunc | append | sync.
-type fid() :: integer().
-type qid() :: integer().
-type attr() :: mode | nlink | uid | gid | rdev | atime | mtime | ctime |
				ino | size | blocks | btime | gen | data_version.
-type at() :: bof | {bof,Offset} | cur | {cur,Offset} | eof | {eof,Offset}.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3,start_link/4]).
-export([close/1]).

-export([walk/3]).
-export([open/2,open/3]).
-export([create/3,create/4]).
-export([read/2,read/3,read/4]).
-export([read_all/2]).
-export([simple_read/3]).
-export([write/3,write/4]).
-export([simple_write/4]).
-export([clunk/2]).
-export([remove/2]).

%%NB: make directory only supported by 9P2000.L
-export([mkdir/3,mkdir/4,mkdir/5]).

-export([read_dir/2]).
-export([read_link/2]).

-export([getattr/2,getattr/3]).
-export([setattr/3]).

-export([seek/3]).

-export([symlink/4,symlink/5]).
-export([link/4]).

-export([rename/4]).
-export([renameat/5]).

-export([fsync/2]).

%misc
-export([encode/1,decode/1]).
-export([encode_stat/1]).

-export([qid_type/1]).

-export([timestamp/0]).

-export([ensure_loaded/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Initiates a new 9p connection.
%% @see start_link/4.
-spec start_link(module(), any(), [binary()]) -> {ok,pid()} | {error,_}.

start_link(TransMod, TransConf, AttachTo) ->
	start_link(TransMod, TransConf, AttachTo, []).

%% @doc Initiates a new 9p connection. After version negotation and
%% authentication exchange tries to attach to all locations listed in
%% `AttachTo'.
%%
%% The function is usually called by 9p mounter. Otherwise, the 9p mounter
%% should be manually notified when the connection closes.
-spec start_link(module(), any(), [binary()], [option()]) -> {ok,pid()} |
{error,_}.

start_link(TransMod, TransConf, AttachTo, Opts) ->
    gen_server:start_link(?MODULE, [TransMod,TransConf,AttachTo,Opts], []).

%% @doc Terminates the 9p connection. All pending operations are 'flushed' before
%% closing the connection.
-spec close(pid()) -> ok.

close(ConnPid) when is_pid(ConnPid) ->
	gen_server:call(ConnPid, close);
close(_) ->
	{error,badarg}.

%% @doc Walks the tree starting with the Fid. Returns the Fid pointing to the
%% last reachable name in the list. The caller should check that the length of
%% returned list of qids is the same as the number of names. A shorter list
%% indicates a (partial) error. It is possible to 'walk' to an empty list
%% effectively cloning the fid.
-spec walk(pid(), fid(), [binary()]) -> {ok,fid(),[qid()]} | {error,any}.

walk(ConnPid, Fid, Names) when is_integer(Fid), is_list(Names) ->
	gen_server:call(ConnPid, {walk,Fid,Names});
walk(_, _, _) ->
	{error,badarg}.

%% @doc Opens the fid for subsequent read/write operations.
%% @see open/3.
-spec open(pid(), fid()) -> {ok,qid(),integer()} | {error,_}.

open(ConnPid, Fid) ->
	open(ConnPid, Fid, [rdonly]).

%% @doc Opens the fid for subsequent read/write operations.
-spec open(pid(),fid(), [flag()]) -> {ok,qid(),integer()} | {error,_}.

open(ConnPid, Fid, Flags) when is_integer(Fid), is_list(Flags) ->
	gen_server:call(ConnPid, {open,Fid,Flags});
open(_, _, _) ->
	{error,badarg}.

%% @doc Creates a file named Name in the directory referenced to by the Fid.
%% @see create/4.
-spec create(pid(), fid(), binary()) -> {ok,qid(),integer()} | {error,_}.

create(ConnPid, Fid, Name) ->
	create(ConnPid, Fid, Name, [rdonly]).

%% @doc Creates a file named Name in the directory referenced to by the Fid.
-spec create(pid(), fid(), binary(), [flag()]) -> {ok,qid(),integer()} |
{error,_}.

create(ConnPid, Fid, Name, Flags) when is_integer(Fid),
		is_binary(Name), is_list(Flags) ->
	gen_server:call(ConnPid, {create,Fid,Name,Flags});
create(_, _, _, _) ->
	{error,badarg}.

%% @doc Reads a chunk of data from the file referenced by the Fid. Returns eof
%% if there is not data to be read.
-spec read(pid(), fid()) -> {ok,binary()} | eof | {error,_}.

read(ConnPid, Fid) when is_integer(Fid) ->
	read_chunk(ConnPid, Fid, ?CSIZE);
read(_, _) ->
	{error,badarg}.

%% @doc Read N bytes from the file referenced by the Fid. Less than N bytes may
%% be returned if the end of file is reached. Returns eof if there is no data to
%% be read.
-spec read(pid(), fid(), integer()) -> {ok,binary()} | eof | {error,_}.

read(ConnPid, Fid, N) when is_integer(Fid), is_integer(N) ->
	read1(ConnPid, Fid, N, []);
read(_, _, _) ->
	{error,badarg}.

read1(_ConnPid, _Fid, 0, Acc) ->
	{ok,list_to_binary(lists:reverse(Acc))};
read1(ConnPid, Fid, N, Acc) ->
	N1 = if N =< ?CSIZE -> N; true -> ?CSIZE end,
	case read_chunk(ConnPid, Fid, N1) of
	eof when Acc =:= [] ->
		eof;
	eof ->
		{ok,list_to_binary(lists:reverse(Acc))};
	{ok,Data} ->
		Sz = byte_size(Data),
		read1(ConnPid, Fid, N -Sz, [Data|Acc]);
	Error ->
		Error
	end.

%% @doc Read N bytes from the file referenced by the Fid at the known offset.
%% Less than N bytes may be returned if the end of file is reached. Returns
%% eof if there is no data to % be read.
-spec read(pid(), fid(), integer(), integer()) -> {ok,binary()} | eof | {error,_}.

read(ConnPid, Fid, Offset, N)
		when is_integer(Fid), is_integer(Offset), is_integer(N) ->
	read1(ConnPid, Fid, Offset, N, []);
read(_, _, _, _) ->
	{error,badarg}.

read1(_ConnPid, _Fid, _Offset, 0, Acc) ->
	{ok,list_to_binary(lists:reverse(Acc))};
read1(ConnPid, Fid, Offset, N, Acc) ->
	N1 = if N =< ?CSIZE -> N; true -> ?CSIZE end,
	case read_chunk(ConnPid, Fid, Offset, N1) of
	eof when Acc =:= [] ->
		eof;
	eof ->
		{ok,list_to_binary(lists:reverse(Acc))};
	{ok,Data} ->
		Sz = byte_size(Data),
		read1(ConnPid, Fid, Offset +Sz, N -Sz, [Data|Acc]);
	Error ->
		Error
	end.

read_chunk(ConnPid, Fid, Offset, N) ->	%% N =< ?CSIZE
	gen_server:call(ConnPid, {read,Fid,Offset,N}).

%% @doc Reads the entire contents of the file referenced by Fid. Unlike read/2
%% read/3 returns `{ok,<<>>}' if the file is empty.
-spec read_all(pid(), fid()) -> {ok,binary()} | {error,_}.

read_all(ConnPid, Fid) when is_integer(Fid) ->
	read_all_1(ConnPid, Fid, []);
read_all(_, _) ->
	{error,badarg}.

read_all_1(ConnPid, Fid, Acc) ->
	case read_chunk(ConnPid, Fid, ?CSIZE) of
	eof ->
		{ok,list_to_binary(lists:reverse(Acc))};
	{ok,Data} ->
		read_all_1(ConnPid, Fid, [Data|Acc]);
	Error ->
		Error
	end.

read_chunk(ConnPid, Fid, N) ->	%% N =< ?CSIZE
	gen_server:call(ConnPid, {read,Fid,N}).

%% @doc Walks to the file, opens it, and reads its entire contents. The
%% operation fails if the size of the file does not fit a single 9p frame.
-spec simple_read(pid(), fid(), [binary()]) -> {ok,binary()} | {error,_}.

simple_read(ConnPid, Fid, WalkTo) when is_integer(Fid), is_list(WalkTo) ->
	gen_server:call(ConnPid, {simple_read,Fid,WalkTo});
simple_read(_, _, _) ->
	{error,badarg}.

%% @doc Write `Data' to the file referenced by the Fid.
-spec write(pid(), fid(), binary()) -> ok | {error,_}.

write(ConnPid, Fid, Data) when is_integer(Fid), is_binary(Data) ->
	write1(ConnPid, Fid, Data);
write(_, _, _) ->
	{error,badarg}.

write1(ConnPid, Fid, <<Chunk:(?CSIZE)/binary,Data/binary>>) ->
	case write_chunk(ConnPid, Fid, Chunk) of
	{ok,_} ->
		write1(ConnPid, Fid, Data);
	Error ->
		Error
	end;
write1(ConnPid, Fid, Data) ->
	case write_chunk(ConnPid, Fid, Data) of
	{ok,_} ->
		ok;
	Error ->
		Error
	end.

write_chunk(_, _, <<>>) ->
	ok;
write_chunk(ConnPid, Fid, Data) -> %% byte_size(Data) =< ?CSIZE
	gen_server:call(ConnPid, {write,Fid,Data}).

%% @doc Write `Data' to the file referenced by the Fid at the known offset
-spec write(pid(), fid(), integer(), binary()) -> ok | {error,_}.

write(ConnPid, Fid, Offset, Data)
		when is_integer(Fid), is_integer(Offset), is_binary(Data) ->
	write1(ConnPid, Fid, Offset, Data);
write(_, _, _, _) ->
	{error,badarg}.

write1(ConnPid, Fid, Offset, <<Chunk:(?CSIZE)/binary,Data/binary>>) ->
	case write_chunk(ConnPid, Fid, Offset, Chunk) of
	{ok,?CSIZE} ->
		write1(ConnPid, Fid, Offset +?CSIZE, Data);
	{ok,_} ->
		{error,eio};	%% incomplete write
	Error ->
		Error
	end;
write1(ConnPid, Fid, Offset, Data) ->
	case write_chunk(ConnPid, Fid, Offset, Data) of
	{ok,_} ->
		ok;
	Error ->
		Error
	end.

write_chunk(_, _, _, <<>>) ->
	ok;
write_chunk(ConnPid, Fid, Offset, Data) -> %% byte_size(Data) =< ?CSIZE
	gen_server:call(ConnPid, {write,Fid,Offset,Data}).

%% @doc Walks to the file, opens it, and overwrites it contents with Data.
-spec simple_write(pid(), fid(), [binary()], binary()) -> ok | {error,_}.

simple_write(ConnPid, Fid, WalkTo, Data)
		when is_integer(Fid), is_list(WalkTo), is_binary(Data) ->
	gen_server:call(ConnPid, {simple_write,Fid,WalkTo,Data});
simple_write(_, _, _, _) ->
	{error,badarg}.

%% @doc Closes the Fid.
-spec clunk(pid(), fid()) -> ok | {error,_}.

clunk(ConnPid, Fid) when is_integer(Fid) ->
	gen_server:call(ConnPid, {clunk,Fid});
clunk(_, _) ->
	{error,badarg}.

%% @doc Removes the object referenced by the Fid.
-spec remove(pid(), fid()) -> ok | {error,_}.

remove(ConnPid, Fid) when is_integer(Fid) ->
	gen_server:call(ConnPid, {remove,Fid});
remove(_, _) ->
	{error,badarg}.

-spec mkdir(pid(), fid(), binary()) -> {ok,qid()} | {error,_}.

mkdir(ConnPid, DFid, Name) ->
	mkdir(ConnPid, DFid, Name, []).

-spec mkdir(pid(), fid(), binary(), [isvtx]) -> {ok,qid()} | {error,_}.

mkdir(ConnPid, DFid, Name, ModeList)
		when is_integer(DFid), is_binary(Name), is_list(ModeList) ->
	mkdir(ConnPid, DFid, Name, ModeList, ?NOGROUP);
mkdir(_, _, _, _) ->
	{error,badarg}.

%% @doc Creates a directory named `Name' in the directory referenced by the
%% DFid. `isvtx' mode adds a 'sticky' bit to the new directory.
-spec mkdir(pid(), fid(), binary(), [isvtx], integer()) -> {ok,qid()} |
{error,_}.

mkdir(ConnPid, DFid, Name, ModeList, Gid) when is_integer(DFid),
		is_binary(Name), is_list(ModeList), is_integer(Gid) ->
	case dir_mode(ModeList) of
	Mode when is_integer(Mode) ->
		gen_server:call(ConnPid, {mkdir,DFid,Name,Mode,Gid});
	Reason ->
		{error,Reason}
	end;
mkdir(_, _, _, _, _) ->
	{error,badarg}.

%% @doc Reads the contents of the directory. The type of entries is not
%% documented. Use Qid to distinguish between files and directories.
-spec read_dir(pid(), fid()) -> {ok,[{qid(),_,integer(),binary()}]} | {error,_}.

read_dir(ConnPid, Fid) when is_integer(Fid) ->
	read_dir_1(ConnPid, Fid, 0, []);
read_dir(_, _) ->
	{error,badarg}.

read_dir_1(ConnPid, Fid, Offset, Acc) ->
	case read_dir_chunk(ConnPid, Fid, Offset) of
	{ok,[]} ->
		{ok,lists:concat(Acc)};
	{ok,Ents} ->
		{_,NewOffs,_,_} = lists:last(Ents),
		read_dir_1(ConnPid, Fid, NewOffs, [Ents|Acc]);
	Error ->
		Error
	end.

read_dir_chunk(ConnPid, Fid, Offset) ->
	gen_server:call(ConnPid, {read_dir,Fid,Offset}).

read_link(ConnPid, Fid) ->
	gen_server:call(ConnPid, {read_link,Fid}).

%% @doc Retrieves attributes of the object pointed to by the Fid. Attempts to
%% read  all attributes are returned except btime, gen, and data version. Not
%% all attributes may be returned depending on the server capabilities.
%% @see getattr/3.
-spec getattr(pid(), fid()) -> {ok,[{atom(),_}]} | {error,_}.

getattr(ConnPid, Fid) ->
	getattr(ConnPid, Fid, [basic]).

%% @doc Retrives selected attributes of the object referenced by the Fid. The
%% following attribute names are recongized:
%%<pre>
%% mode    % protection
%% nlink   % number of hard links
%% uid     % user ID of owner
%% gid     % group ID of owner
%% rdev    % device ID (if special file)
%% atime   % time of last access
%% mtime   % time of last modification
%% ctime   % time of last status change
%% ino     % inode number
%% size    % total size, in bytes
%% blocks  % number of 512M blocks allocated
%% btime   % reserved for future use
%% gen     % reserved for future use
%% data_version % reserved for future use</pre>
%%
-spec getattr(pid(), fid(), [attr()]) -> {ok,[{attr(),_}]} | {error,_}.

getattr(ConnPid, Fid, Flds) when is_integer(Fid), is_list(Flds) ->
	case attr_mask(Flds) of
	ReqMask when is_integer(ReqMask) ->
		gen_server:call(ConnPid, {getattr,Fid,ReqMask});
	Reason ->
		{error,Reason}
	end;
getattr(_, _, _) ->
	{error,badarg}.

setattr(ConnPid, Fid, Props)
		when is_pid(ConnPid), is_integer(Fid), is_list(Props) ->
	case attr_props(Props) of
	{Valid,Rec} ->
		gen_server:call(ConnPid, {setattr,Fid,Valid,Rec#setattr.mode,
										 	Rec#setattr.uid,
										 	Rec#setattr.gid,
										 	Rec#setattr.size,
										 	Rec#setattr.atime_sec,
										 	Rec#setattr.atime_nsec,
										 	Rec#setattr.mtime_sec,
											Rec#setattr.mtime_nsec});
	Reason ->
		{error,Reason}
	end;
setattr(_, _, _) ->
	{error,badarg}.

symlink(ConnPid, DFid, Name, Target) ->
	symlink(ConnPid, DFid, Name, Target, ?NOGROUP).

symlink(ConnPid, DFid, Name, Target, Gid) when is_integer(DFid),
		is_binary(Name), is_binary(Target), is_integer(Gid) ->
	gen_server:call(ConnPid, {symlink,DFid,Name,Target,Gid});
symlink(_, _, _, _, _) ->
	{error,badarg}.

link(ConnPid, DFid, Fid, Name) when is_integer(DFid), is_integer(Fid), is_binary(Name) ->
	gen_server:call(ConnPid, {link,DFid,Fid,Name});
link(_, _, _, _) ->
	{error,badarg}.

%% @doc Changes the current position of the file referenced by the Fid. The
%% operation is handled on the client side.
-spec seek(pid(), fid(), at()) -> {ok,integer()} | {error,_}.

seek(ConnPid, Fid, At) when is_pid(ConnPid), is_integer(Fid) ->
	gen_server:call(ConnPid, {seek,Fid,At});
seek(_, _, _) ->
	{error,badarg}.

rename(ConnPid, Fid, DFid, Name) when is_integer(Fid), is_integer(DFid) ->
	gen_server:call(ConnPid, {rename,Fid,DFid,Name});
rename(_, _, _, _) ->
	{error,badarg}.

renameat(ConnPid, DFid1, Name1, DFid2, Name2) when is_integer(DFid1), is_integer(DFid2) ->
	gen_server:call(ConnPid, {renameat,DFid1,Name1,DFid2,Name2});
renameat(_, _, _, _, _) ->
	{error,badarg}.

fsync(ConnPid, Fid) when is_pid(ConnPid), is_integer(Fid) ->
	gen_server:call(ConnPid, {fsync,Fid});
fsync(_, _) ->
	{error,badarg}.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @private
init([TransMod,TransConf,AttachTo,Opts]) ->

	%% intercept lost transport conditions
	process_flag(trap_exit, true),

	%% interpret options
	St = start_opts(Opts),

	%% open a transport connection
	case TransMod:connect(TransConf, ?CONN_TIMEO) of
	{ok,Sock} ->

		%% initate version negotiation
		#st{ver =Ver,
			msize =MSize} = St,
		VerMsg = {tversion,?NOTAG,MSize,version_bin(Ver)},
		ok = TransMod:send(Sock, VerMsg, TransConf),

		%% activate the transport
		{ok,{DataTag,CloseTag,ErrorTag}} = TransMod:activate(Sock, TransConf),

		%% local connection flag
		IsLocal = TransMod:is_local(TransConf),
		
		%% enter main loop
		{ok,St#st{sock =Sock,
				  trans_mod =TransMod,
				  trans_conf =TransConf,
				  data_tag =DataTag,
				  close_tag =CloseTag,
				  error_tag =ErrorTag,
				  is_local =IsLocal,
				  attach_to =AttachTo,
				  state =initial}};

	{error,Reason} ->
		{stop,Reason}
	end.

%% @private
handle_call(_Req, _From, #st{state =closing} =St) ->
	%% do not accept any requests when closing
	{reply,{error,closed},St};

handle_call(close, From, #st{sock =Sock,
							 pending =[],
	   						 attach_fids =[]}) ->
	%% terminate connection immediately -- not likely
	io:format("9p: terminate: closing ~p~n", [Sock]),
	gen_server:reply(From, ok),
	exit(terminated);	%% fall through the floor

handle_call(close, From, #st{sock =Sock} =St) ->
	%% terminate connection -- linger
	io:format("9p: terminate: lingering ~p~n", [Sock]),
	clunk_attaches(St),
	flush_pending(St),
	{noreply,St#st{state =closing,
				   terminator =From}};

handle_call({seek,Fid,Offset}, _From, St) ->
	%% seek requests are handled by client
	{Reply,St1} = seek_fid(Fid, Offset, St),
	{reply,Reply,St1};

handle_call({fsync,_Fid}, _From, #st{ver =e} =St) ->
	%%
	%% fsync is noop for 9P2000.e
	%%
	%% 9p_server does not cache writes. The disk driver may cache writes and
	%% there disk:flush() to flush that cache. Yet the protocol does not allow
	%% such command. Should we introduce the command to 9P2000.e and make it
	%% more hard-disk-oriented and less compatible?
	%%
	{reply,ok,St};

handle_call(Req, From, St) ->
	%% client 9p requests -- require a reply
	{Tag,Msg,St1} = prepare_message(Req, St),
	dispatch_loop(Tag, From, Msg, St1).

%% @private
handle_cast({internal,Req}, St) ->
	{Tag,Msg,St1} = prepare_message(Req, St),
	dispatch_loop(Tag, none, Msg, St1).

%% @private
%% transport connection lost -- attempt to recover the session
handle_info({'EXIT',_Sock,Why}, #st{ver =e}) ->
	?dbg("9p: transport ~p lost: ~p: recovering...~n",
									[_Sock,Why]),
	%%recover_session(Why, St),
	exit(Why);

%% session recovery not supported -- go away
handle_info({'EXIT',_Sock,Why}, _St) ->
	?dbg("9p: transport ~p lost: ~p: exiting...~n",
									[_Sock,Why]),
	exit(Why);

%% message is not serialized -- local bypass
handle_info({Tag,Sock,Msg}, #st{sock =Sock,data_tag =Tag} =St) when is_tuple(Msg) ->
	%%io:format("9p: client: incoming ~p\n", [Msg]),
	case incoming(Msg, St) of
	{continue,St1} ->
		{noreply,St1};
	{exit,ExitReason} ->
		exit(ExitReason)
	end;

%% ordinary network serialized message
handle_info({Tag,Sock,Data}, #st{sock =Sock,data_tag =Tag} =St) when is_binary(Data) ->
	Decoded =decode(Data),
	%%io:format("9p: client: incoming ~p\n", [Decoded]),
	case incoming(Decoded, St) of
	{continue,St1} ->
		{noreply,St1};
	{exit,ExitReason} ->
		exit(ExitReason)
	end;

%% the remote end closed the connection
handle_info({Tag,Sock}, #st{sock =Sock,close_tag =Tag}) ->
	?dbg("9p: transport ~p closed: exiting...~n",
								[Sock]),
	exit(closed);

%% a transport error occured
handle_info({Tag,Sock,Reason}, #st{sock =Sock,error_tag =Tag}) ->
	?dbg("9p: transport ~p error: ~p: exiting...~n",
								[Sock,Reason]),
	exit(Reason).

terminate(_Reason, #st{sock =Sock,
					   trans_mod =TransMod,
					   trans_conf =TransConf}) ->
	TransMod:close(Sock, TransConf).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

dispatch_loop(Tag, From, Msg, #st{sock =Sock,
					  			  trans_mod =TransMod,
					   			  trans_conf =TransConf} =St) ->
	TransMod:send(Sock, Msg, TransConf),
	{noreply,St#st{pending=[{Tag,From,Msg}|St#st.pending]}}.

clunk_attaches(#st{attach_fids =Fids}) ->
	lists:foreach(fun(Fid) ->
		self() ! {internal,{clunk,Fid}}
	end, Fids).

flush_pending(#st{pending =Ps}) ->
	lists:foreach(fun({Tag,From,Req}) ->
		io:format("9p: terminate: flushing ~p\n", [Req]),
		self() ! {internal,{flush,Tag}},
		if From =/= none ->
				From ! {reply,{error,closed}};
			true -> ok
		end
	end, Ps).

incoming({rversion,?NOTAG,_MSize,<<"unknown">>}, _St) ->
	{exit,bad_version};

%% diod returns Rerror instead of Rversion as per protocol
incoming({rlerror,?NOTAG,_}, #st{state =initial}) ->
	{exit,bad_version};

incoming({rversion,?NOTAG,MSize,VerBin}, #st{sock =Sock,
											 trans_mod =TransMod,
											 trans_conf =TransConf,
											 is_local =IsLocal,
											 ver =Ver,
											 auth_user =AUser,
										 	 auth_path =APath,
										 	 auth_user_id =AUserId} =St) ->
	true = VerBin =:= version_bin(Ver),

	%% Pass on the msize value to the transport layer
	ok = TransMod:set_maximum_size(Sock, MSize, TransConf),

	if not IsLocal ->
		AuthMsg = if Ver =:= e ->
			{auth,AUser,APath};
		true ->
			{auth,AUser,APath,AUserId}
		end,
		gen_server:cast(self(), {internal,AuthMsg}),
		{continue,St#st{msize =MSize}};
	true ->
		%% local connection are auto-authenticated
		action(start_attaching, none, St)
	end;

%% all tagged operations end here
incoming(Msg, #st{pending =Pending} =St) ->
	Tag = element(2, Msg),
	case lists:keytake(Tag, 1, Pending) of
	false ->
		%% The tag is currently being flushed
		case [T || {T,_,{tflush,_,OldTag}} <- Pending, OldTag =:= Tag] of
		[] ->
			{exit,bad_tag};
		_ ->
			{continue,St}
		end;
	{value,{_,From,Req},Pending1} ->

		St1 = St#st{pending =Pending1,
					tags =[Tag|St#st.tags]},

		action(mixer(Msg, Req, St1#st.state), From, St1)
	end.

%%
%%	state changes (9P2000.L):
%%
%%	<initial>
%%		tversion-rversion/rlerror
%%		tauth-rauth/rlerror
%%		twrite-rwrite/rlerror
%%		tattach(afid)-rattach/rlerror
%%	<normal>
%%		tattach(-1)-rattach/rlerror
%%		tattach(-1)-rattach/rlerror
%%		tattach(-1)-rattach/rlerror
%%		...
%%	<closing>
%%		tflush-rflush/rlerror
%%		tflush-rflush/rlerror
%%		tflush-rflush/rlerror
%%		

action({write_auth,AFid}, _From, #st{ver =e} =St) ->
	SessionKey = '9p_auth':generate_session_key(),
	Mumble = '9p_auth':mumble(SessionKey),
	WriteMsg = {write,AFid,0,Mumble},
	gen_server:cast(self(), {internal,WriteMsg}),
	{continue,St#st{afid =AFid,
					session_key =SessionKey}};

action({write_auth,AFid}, _From, #st{ver =l,
									 unix_uid =Uuid,
									 unix_gid =Ugid} =St) ->
	Munge = '9p_auth':munge(Uuid, Ugid),
	WriteMsg = {write,AFid,0,Munge},
	gen_server:cast(self(), {internal,WriteMsg}),
	{continue,St#st{afid =AFid}};

action(auth_failed, _From, _St) ->
	{exit,bad_auth};

action(start_attaching, _From, #st{ver =e,
								   attach_to =AttachTo} =St) ->
	%% 9P2000.e does not require a completed first attach
	lists:foreach(fun(AInfo) ->
		{Path,User,_} = attach_info(AInfo, none),
		AttMsg = {attach,?NOFID,User,Path},
		gen_server:cast(self(), {internal,AttMsg})
	end, AttachTo),

	if St#st.afid =/= undefined ->
		%% afid no longer needed
		gen_server:cast(self(), {internal,{clunk,St#st.afid}}),
		{continue,St#st{state =normal,
						afid =undefined}};
	true ->
		{continue,St#st{state =normal}}
	end;

action(start_attaching, _From, #st{ver =l,
								   afid =AFid,
								   unix_uid =UnixUid,
								   attach_to =[AInfo|AttachTo]} =St) ->
	%% Uid must be the same as unix_uid for diod
	{AName,User,Uid} = attach_info(AInfo, UnixUid),
	AttMsg = {attach,AFid,User,AName,Uid},
	gen_server:cast(self(), {internal,AttMsg}),
	{continue,St#st{attach_to =AttachTo}};

%% attach operation completed sucessfully - proceed to the rest
action({attach_more,Fid,AName,Type}, _From, #st{ver =l,
										        mounter =Mounter,
										   		afid =AFid,
										   		unix_uid =UnixUid,
										   		attach_to =AttachTo} =St) ->
	%% notify the mounter
	gen_server:cast(Mounter, {'9p_attached',self(),Fid,AName,Type}),

	%% clunk afid -- no longer needed
	gen_server:cast(self(), {internal,{clunk,AFid}}),

	%% all attaches are 'grandfathered'
	lists:foreach(fun(AInfo) ->
		{Path,User,Uid} = attach_info(AInfo, UnixUid),
		AttMsg = {attach,?NOFID,User,Path,Uid},
		gen_server:cast(self(), {internal,AttMsg})
	end, AttachTo),

	{continue,St#st{state =normal,
					afid =undefined,
					attach_fids =[Fid]}};

action({attached,Fid,AName,Type}, _From, #st{mounter =Mounter,
										     attach_fids =AttachFids} =St) ->

	%%
	%% Notify 9p mounter about the successful attach operation. The 9p
	%% mounter in its turn will modify the mounting table and notify
	%% processes that may be waiting for a certain local name to appear.
	%%

	gen_server:cast(Mounter, {'9p_attached',self(),Fid,AName,Type}),
	{continue,St#st{attach_fids =[Fid|AttachFids]}};

action({forget,Fid}, _From, St) ->
	%% may happen to afid only
	Fids = [Fid|St#st.fids],
	{continue,St#st{fids =Fids}};

action({release,Fid,Reply}, From, St) ->
	reply(From, Reply),
	Fids = [Fid|St#st.fids],
	Offsets = lists:keydelete(Fid, 1, St#st.offsets),
	AttachFids = lists:delete(Fid, St#st.attach_fids),
	{continue,St#st{fids =Fids,
					offsets =Offsets,
					attach_fids =AttachFids}};

action({offset,{Fid,NewOffs},Reply}, From, St) ->
	reply(From, Reply),
	Offsets = lists:keystore(Fid, 1, St#st.offsets, {Fid,NewOffs}),
	{continue,St#st{offsets=Offsets}};

action({reply,Reply}, From, St) ->
	reply(From, Reply),
	{continue,St};

action({flushed,Tag}, _From, #st{pending =Pending} =St) ->
	case lists:keydelete(Tag, 1, Pending) of
	[] when St#st.attach_fids =:= [] ->
	
		%%
		%% All pending tags are flushed and attach fids are clunked -- notify
		%% the process that is terminating the connection
		%%
		
		do_terminate(ok, St);
	Ps ->
		{continue,St#st{pending =Ps}}
	end;

action({clunked,Fid,Res}, _From, #st{attach_fids =AttachFids} =St) ->
	case lists:delete(Fid, AttachFids) of
	[] when St#st.pending =:= [] ->
		do_terminate(Res, St);
	Fids ->
		NewRes = combine_res(Res, St#st.close_result),
		{continue,St#st{attach_fids =Fids,
						close_result =NewRes}}
	end.

reply(none, _) ->
	ok;
reply(From, Reply) ->
	gen_server:reply(From, Reply).

do_terminate(LastRes, #st{close_result =Res,terminator =Killer}) ->
	gen_server:reply(Killer, combine_res(LastRes, Res)),
	{exit,terminated}.

combine_res(ok, Res) -> Res;
combine_res(Res, ok) -> Res;
combine_res(Res, _) -> Res.

mixer({rauth,_,_}, {tauth,_,AFid,_,_}, _) -> {write_auth,AFid};
mixer({rauth,_,_}, {tauth,_,AFid,_,_,_}, _) -> {write_auth,AFid};
mixer({rwrite,_,_}, _, initial) -> start_attaching;
mixer({rerror,_,_}, _, initial) -> auth_failed;
mixer({rlerror,_,_}, _, initial) -> auth_failed;
mixer({rattach,_,Qid}, {tattach,_,Fid,_,_,AName,_}, initial) ->
	{attach_more,Fid,AName,qid_type(Qid)};
mixer({rclunk,_}, {tclunk,_,Fid}, initial) -> {forget,Fid};

mixer({rattach,_,Qid}, {tattach,_,Fid,_,_,AName,_}, _) ->
	{attached,Fid,AName,qid_type(Qid)};
mixer({rattach,_,Qid}, {tattach,_,Fid,_,_,AName}, _) ->
	{attached,Fid,AName,qid_type(Qid)};
mixer({rclunk,_}, {tclunk,_,Fid}, normal) -> {release,Fid,ok};
mixer({rwalk,_,Qids}, {twalk,_,_,NewFid,_}, _) -> {reply,{ok,NewFid,Qids}}; %% NewFid
mixer({rerror,_,Error}, {twalk,_,_,NewFid,_}, _) -> {release,NewFid,{error,Error}};
mixer({rlerror,_,Posix}, {twalk,_,_,NewFid,_}, _) -> {release,NewFid,{error,Posix}};
mixer({rstat,_,Stat}, {tstat,_,_,Mask}, _) -> {reply,{ok,filter_stat(Stat, Mask)}};
mixer({rwstat,_}, _, _) -> {reply,ok};
mixer(#rgetattr{}=Rec, _, _) -> {reply,{ok,fold_attrs(Rec)}};
mixer({rsetattr,_}, _, _) -> {reply,ok};
mixer({rlopen,_,Qid,Iounit}, {tlopen,_,Fid,_}, _) -> {offset,{Fid,0},{ok,Qid,Iounit}};
mixer({ropen,_,Qid,Iounit}, {topen,_,Fid,_}, _) -> {offset,{Fid,0},{ok,Qid,Iounit}};
mixer({rcreate,_,Qid,Iounit}, {tcreate,_,Fid,_,_,_}, _) -> {offset,{Fid,0},{ok,Qid,Iounit}};
mixer({rlcreate,_,Qid,Iounit}, {tlcreate,_,Fid,_,_,_,_}, _) -> {offset,{Fid,0},{ok,Qid,Iounit}};
mixer({rread,_,Data}, {tread,_,_,Offs,_,read_dir}, _) -> {reply,{ok,dir_ents(Data, Offs)}};
mixer({rread,_,<<>>}, _, _) -> {reply,eof};
mixer({rread,_,Data}, {tread,_,Fid,Offset,_,read}, _) -> {offset,{Fid,Offset +byte_size(Data)},{ok,Data}};
mixer({rread,_,Data}, {tread,_,_,_,_,pread}, _) -> {reply,{ok,Data}};
mixer({rwrite,_,N}, {twrite,_,Fid,Offset,_,write}, _) -> {offset,{Fid,Offset +N},{ok,N}};
mixer({rwrite,_,N}, {twrite,_,_,_,_,pwrite}, _) -> {reply,{ok,N}};
mixer({rmkdir,_,Qid}, _, _) -> {reply,{ok,Qid}};
mixer({rreaddir,_,Ents}, _, _) -> {reply,{ok,Ents}};
mixer({rremove,_}, {tremove,_,Fid}, _) -> {release,Fid,ok};
mixer({rreadlink,_,Target}, _, _) -> {reply,{ok,Target}};
mixer({rsymlink,_,Qid}, _, _) -> {reply,{ok,Qid}};
mixer({rlink,_}, _, _) -> {reply,ok};
mixer({rrename,_}, _, _) -> {reply,ok};
mixer({rrenameat,_}, _, _) -> {reply,ok};
mixer({rfsync,_}, _, _) -> {reply,ok};
mixer({rerror,_,Error}, _, normal) -> {reply,{error,Error}};
mixer({rlerror,_,Posix}, _, normal) -> {reply,{error,Posix}};

mixer({rflush,_}, {tflush,_,OldTag}, closing) -> {flushed,OldTag};
mixer({rclunk,_}, {tclunk,_,Fid}, closing) -> {clunked,Fid,ok};
mixer({rlerror,_,Posix}, {tclunk,_,Fid}, closing) -> {clunked,Fid,{error,Posix}}.

%%------------------------------------------------------------------------------

prepare_message({auth,User,AName}, St) ->
	{Tag,AFid,St1} = pick_tag_fid(St),
	{Tag,{tauth,Tag,AFid,User,AName},St1};
prepare_message({auth,User,AName,Uid}, St) ->
	{Tag,AFid,St1} = pick_tag_fid(St),
	{Tag,{tauth,Tag,AFid,User,AName,Uid},St1};

prepare_message({attach,AFid,User,AName}, St) ->
	{Tag,Fid,St1} = pick_tag_fid(St),
	{Tag,{tattach,Tag,Fid,AFid,User,AName},St1};
prepare_message({attach,AFid,User,AName,Uid}, St) ->
	{Tag,Fid,St1} = pick_tag_fid(St),
	{Tag,{tattach,Tag,Fid,AFid,User,AName,Uid},St1};

prepare_message({walk,Fid,Names}, St) ->
	{Tag,NewFid,St1} = pick_tag_fid(St),
	WalkReq = {twalk,Tag,Fid,NewFid,Names},
	{Tag,WalkReq,St1};

prepare_message({open,Fid,Flags}, #st{ver =e} =St) ->
	{Tag,St1} = pick_tag(St),
	OpenReq = {topen,Tag,Fid,Flags},
	{Tag,OpenReq,St1};

prepare_message({open,Fid,Flags}, St) ->
	{Tag,St1} = pick_tag(St),
	OpenReq = {tlopen,Tag,Fid,Flags},
	{Tag,OpenReq,St1};

prepare_message({create,Fid,Name,Flags}, #st{ver =e} =St) ->
	{Tag,St1} = pick_tag(St),
	CreatReq = {tcreate,Tag,Fid,Name,?FILE_MODE,bit_flags_old(Flags)},
	{Tag,CreatReq,St1};

prepare_message({create,Fid,Name,Flags}, St) ->
	{Tag,St1} = pick_tag(St),
	CreatReq = {tlcreate,Tag,Fid,Name,bit_flags(Flags),?FILE_MODE,?NOGROUP},
	{Tag,CreatReq,St1};

prepare_message({read,Fid,N}, St) ->
	case lists:keyfind(Fid, 1, St#st.offsets) of
	false ->
		{error,ebadf};
	{_,Offset} ->
		{Tag,St1} = pick_tag(St),
		ReadReq = {tread,Tag,Fid,Offset,N,read},
		{Tag,ReadReq,St1}
	end;

%% offset known (pread)
prepare_message({read,Fid,Offset,N}, St) ->
	{Tag,St1} = pick_tag(St),
	ReadReq = {tread,Tag,Fid,Offset,N,pread},
	{Tag,ReadReq,St1};

prepare_message({simple_read,Fid,WalkTo}, St) ->
	{Tag,St1} = pick_tag(St),
	SimpleReadReq = {tsread,Tag,Fid,WalkTo},
	{Tag,SimpleReadReq,St1};

prepare_message({write,Fid,Data}, St) ->
	case lists:keyfind(Fid, 1, St#st.offsets) of
	false ->
		{error,ebadf};
	{_,Offset} ->
		{Tag,St1} = pick_tag(St),
		WriteReq = {twrite,Tag,Fid,Offset,Data,write},
		{Tag,WriteReq,St1}
	end;

%% NB: writing to auth fid uses this clause

%% offset known (pwrite)
prepare_message({write,Fid,Offset,Data}, St) ->
	{Tag,St1} = pick_tag(St),
	WriteReq = {twrite,Tag,Fid,Offset,Data,pwrite},
	{Tag,WriteReq,St1};

prepare_message({simple_write,Fid,WalkTo,Data}, St) ->
	{Tag,St1} = pick_tag(St),
	SimpleWriteReq = {tswrite,Tag,Fid,WalkTo,Data},
	{Tag,SimpleWriteReq,St1};

prepare_message({clunk,Fid}, St) ->
	{Tag,St1} = pick_tag(St),
	ClunkReq = {tclunk,Tag,Fid},
	{Tag,ClunkReq,St1};

prepare_message({flush,OldTag}, St) ->
	{Tag,St1} = pick_tag(St),
	FlushReq = {tflush,Tag,OldTag},
	{Tag,FlushReq,St1};

prepare_message({remove,Fid}, St) ->
	{Tag,St1} = pick_tag(St),
	RemoveReq = {tremove,Tag,Fid},
	{Tag,RemoveReq,St1};

prepare_message({mkdir,DFid,Name,Mode,Gid}, #st{ver =l} =St) ->
	{Tag,St1} = pick_tag(St),
	MkdirReq = {tmkdir,Tag,DFid,Name,Mode,Gid},
	{Tag,MkdirReq,St1};

prepare_message({mkdir,DFid,Name,Mode,_Gid}, #st{ver =e} =St) ->
	{Tag,St1} = pick_tag(St),
	MkdirReq = {tcreate,Tag,DFid,Name,?DMDIR bor Mode,0},	%% mode/perms mess
	{Tag,MkdirReq,St1};

prepare_message({read_dir,DFid,Offset}, #st{ver =e} =St) ->
	{Tag,St1} = pick_tag(St),
	ReadDirReq = {tread,Tag,DFid,Offset,?CSIZE,read_dir},
	{Tag,ReadDirReq,St1};

prepare_message({read_dir,DFid,Offset}, St) ->
	{Tag,St1} = pick_tag(St),
	ReadDirReq = {treaddir,Tag,DFid,Offset,?CSIZE},
	{Tag,ReadDirReq,St1};

prepare_message({read_link,Fid}, #st{ver =l} =St) ->
	{Tag,St1} = pick_tag(St),
	ReadLinkReq = {treadlink,Tag,Fid},
	{Tag,ReadLinkReq,St1};

prepare_message({getattr,Fid,ReqMask}, #st{ver =e} =St) ->
	{Tag,St1} = pick_tag(St),
	StatReq = {tstat,Tag,Fid,ReqMask},
	{Tag,StatReq,St1};

prepare_message({getattr,Fid,ReqMask}, St) ->
	{Tag,St1} = pick_tag(St),
	GetAttrReq = {tgetattr,Tag,Fid,ReqMask},
	{Tag,GetAttrReq,St1};

prepare_message({setattr,Fid,Valid,Mode,_Uid,_Gid,Size,
				ATimeSec,_ATimeNSec,MTimeSec,_MTimeNSec}, #st{ver =e} =St) ->
	{Tag,St1} = pick_tag(St),
	TS = '9p':timestamp(),
	Spec = [{Valid band   1,#stat.mode,  Mode},
			{Valid band   8,#stat.length,Size},
			{Valid band  16,#stat.atime, TS},
			{Valid band  32,#stat.mtime, TS},
			{Valid band 128,#stat.atime, ATimeSec},
			{Valid band 256,#stat.mtime, MTimeSec}],
	Stat = lists:foldl(fun({0,_,_}, Stat) ->
				Stat;
			({_,Idx,Val}, Stat) ->
				setelement(Idx, Stat, Val)
			end, dummy_valid_stat(), Spec),
	StatReq = {twstat,Tag,Fid,Stat},
	{Tag,StatReq,St1};

prepare_message({setattr,Fid,Valid,Mode,Uid,Gid,Size,
				ATimeSec,ATimeNSec,MTimeSec,MTimeNSec}, St) ->
	{Tag,St1} = pick_tag(St),
	SetAttrReq = {tsetattr,Tag,Fid,Valid,Mode,Uid,Gid,Size,
				ATimeSec,ATimeNSec,MTimeSec,MTimeNSec},
	{Tag,SetAttrReq,St1};

prepare_message({symlink,DFid,Name,Target,Gid}, #st{ver =l} =St) ->
	{Tag,St1} = pick_tag(St),
	SymlinkReq = {tsymlink,Tag,DFid,Name,Target,Gid},
	{Tag,SymlinkReq,St1};

prepare_message({link,DFid,Fid,Name}, #st{ver =l} =St) ->
	{Tag,St1} = pick_tag(St),
	LinkReq = {tlink,Tag,DFid,Fid,Name},
	{Tag,LinkReq,St1};

prepare_message({rename,Fid,_DFid,Name}, #st{ver =e} =St) ->

	%%
	%% We assume that DFid is the parent directory of the Fid. Difficult to
	%% check.
	%%

	{Tag,St1} = pick_tag(St),
	Stat = dummy_valid_stat(),
	StatReq = {twstat,Tag,Fid,Stat#stat{name =Name}},
	{Tag,StatReq,St1};

prepare_message({rename,Fid,DFid,Name}, #st{ver =l} =St) ->
	{Tag,St1} = pick_tag(St),
	RenameReq = {trename,Tag,Fid,DFid,Name},
	{Tag,RenameReq,St1};

prepare_message({renameat,DFid1,Name1,DFid2,Name2}, #st{ver =l} =St) ->
	{Tag,St1} = pick_tag(St),
	RenameatReq = {trenameat,Tag,DFid1,Name1,DFid2,Name2},
	{Tag,RenameatReq,St1};

prepare_message({fsync,Fid}, #st{ver =l} =St) ->
	{Tag,St1} = pick_tag(St),
	FsyncReq = {tfsync,Tag,Fid},
	{Tag,FsyncReq,St1}.

dummy_valid_stat() ->
	#stat{qid = <<0,0,0,0,0,0,0,0,0,0,0,0,0>>,
		  mode = -1,
		  atime = -1,
		  mtime = -1,
		  length = -1,
		  name = <<>>}.

seek_fid(Fid, bof, St) ->
	Offsets = lists:keystore(Fid, 1, St#st.offsets, {Fid,0}),
	{{ok,0},St#st{offsets=Offsets}};

seek_fid(_Fid, {bof,Offs}, St) when Offs < 0 ->
	{{error,einval},St};

seek_fid(Fid, {bof,Offs}, St) ->
	Offsets = lists:keystore(Fid, 1, St#st.offsets, {Fid,Offs}),
	{{ok,Offs},St#st{offsets=Offsets}};

seek_fid(Fid, cur, St) ->
	Reply =case lists:keyfind(Fid, 1, St#st.offsets) of
	false ->
		{error,ebadf};
	{_,Offs} ->
		{ok,Offs}
	end,
	{Reply,St};

seek_fid(Fid, {cur,N}, St) ->
	case lists:keyfind(Fid, 1, St#st.offsets) of
	false ->
		{{error,ebadf},St};
	{_,Offs} when Offs +N < 0 ->
		{{error,einval},St};
	{_,Offs} ->
		Offsets = lists:keystore(Fid, 1, St#st.offsets, {Fid,Offs +N}),
		{{ok,Offs +N},St#st{offsets=Offsets}}
	end;

seek_fid(_Fid, Offs, St) when is_integer(Offs), Offs < 0 ->
	{{error,einval},St};

seek_fid(Fid, Offs, St) when is_integer(Offs) ->
	Offsets = lists:keystore(Fid, 1, St#st.offsets, {Fid,Offs}),
	{{ok,Offs},St#st{offsets=Offsets}}.

%% NB: information lost on conversion
qtype_to_type_mask(<<?QTDIR,_/binary>>) -> ?S_IFDIR;
qtype_to_type_mask(<<?QTAPPEND,_/binary>>) -> ?S_IFREG;
qtype_to_type_mask(<<?QTEXCL,_/binary>>) -> ?S_IFREG;
qtype_to_type_mask(<<?QTMOUNT,_/binary>>) -> 0;
qtype_to_type_mask(<<?QTAUTH,_/binary>>) -> 0;
qtype_to_type_mask(<<?QTTMP,_/binary>>) -> ?S_IFREG;
qtype_to_type_mask(<<?QTLINK,_/binary>>) -> ?S_IFLNK;
qtype_to_type_mask(<<?QTFILE,_/binary>>) -> ?S_IFREG.

filter_stat(Stat, Mask) ->
	lists:concat([filter_stat_mode(Stat, Mask band 1),
				  filter_stat_atime(Stat, Mask band 32),
				  filter_stat_mtime(Stat, Mask band 64),
				  filter_stat_size(Stat, Mask band 512)]).

filter_stat_mode(_, 0) -> [];
filter_stat_mode(#stat{mode =Mode,qid =Qid}, _) ->

	%% The type must be combined with the mode field; prim_file looks for the
	%% type information in the mode field.

	TypeMask = qtype_to_type_mask(Qid),

	[{mode,TypeMask bor Mode}].

filter_stat_atime(_, 0) -> [];
filter_stat_atime(#stat{atime =T}, _) -> [{atime_sec,T},{atime_nsec,0}].

filter_stat_mtime(_, 0) -> [];
filter_stat_mtime(#stat{mtime =T}, _) -> [{mtime_sec,T},{mtime_nsec,0}].

filter_stat_size(_, 0) -> [];
filter_stat_size(#stat{length =Sz}, _) -> [{size,Sz}].

attach_info({_,_,_} =AInfo, _Uid) -> AInfo;
attach_info({AName,User}, Uid) -> {AName,User,Uid};
attach_info(AName, Uid) when is_binary(AName) -> {AName,<<>>,Uid}.

dir_ents(Data, StartOff) ->
	{Ents,_} = lists:mapfoldl(fun(<<_T:16/little,
							 		_Dev:32/little,
							 		Qid:13/binary,
							 		_Mode:32/little,
							 		_Atime:32/little,
									_Mtime:32/little,
									_Length:64/little,
									NSz:16/little,Name:(NSz)/binary,
									_MoreNames/binary>> =Stat, Off) ->
		EntSz = byte_size(Stat) +2,	%% +2 stat entry size
		Type = binary:first(Qid),
		{{Qid,Off +EntSz,Type,Name},Off +EntSz}
	end, StartOff, [Stat || <<Sz:16/little,Stat:(Sz)/binary>> <= Data]),
	Ents.

%% @private
qid_type(<<?QTDIR,_/binary>>) -> dir;
qid_type(<<?QTAPPEND,_/binary>>) -> append;
qid_type(<<?QTEXCL,_/binary>>) -> excl;
qid_type(<<?QTMOUNT,_/binary>>) -> mount;
qid_type(<<?QTAUTH,_/binary>>) -> auth;
qid_type(<<?QTTMP,_/binary>>) -> tmp;
qid_type(<<?QTLINK,_/binary>>) -> link;
qid_type(<<?QTFILE,_/binary>>) -> file.

%%
%% Version defaults to '9P2000.e' and msize for that version is set to to the
%% maximum possible value to avoid fragmentation when sending processes
%% messages as 9p frames.
%%

start_opts(Opts) ->
	start_opts(Opts, #st{}).

start_opts([], St) ->
	start_opts_1(St);
start_opts([{version,'9P2000.e'}|Opts], St) ->
	start_opts(Opts, St#st{ver =e});
start_opts([{version,'9P2000.L'}|Opts], St) ->
	start_opts(Opts, St#st{ver =l});
start_opts([{version,'9p2000.L'}|Opts], St) ->
	start_opts(Opts, St#st{ver =l});
start_opts([{mounter,MounterPid}|Opts], St) when is_pid(MounterPid) ->
	start_opts(Opts, St#st{mounter =MounterPid});	%% for debugging
start_opts([{msize,MSize}|Opts], St) when is_integer(MSize) ->
	start_opts(Opts, St#st{msize =MSize});
start_opts([{auth_user,AUser}|Opts], St) when is_binary(AUser) ->
	start_opts(Opts, St#st{auth_user =AUser});
start_opts([{auth_user_id,AUserId}|Opts], St) when is_integer(AUserId) ->
	start_opts(Opts, St#st{auth_user_id =AUserId});
start_opts([{auth_path,APath}|Opts], St) when is_binary(APath) ->
	start_opts(Opts, St#st{auth_user =APath});
start_opts([{unix_uid,Uid}|Opts], St) when is_integer(Uid) ->
	start_opts(Opts, St#st{unix_uid =Uid});
start_opts([{unix_gid,Gid}|Opts], St) when is_integer(Gid) ->
	start_opts(Opts, St#st{unix_gid =Gid}).

start_opts_1(#st{ver =e,msize =undefined} =St) -> St#st{msize =8192};
start_opts_1(#st{ver =l,msize =undefined} =St) -> St#st{msize =8192}.

version_bin(e) -> <<"9P2000.e">>;
version_bin(l) -> <<"9P2000.L">>.

pick_tag_fid(St) ->
	{Tag,St1} = pick_tag(St),
	{Fid,St2} = pick_fid(St1),
	{Tag,Fid,St2}.

pick_tag(#st{tags=[],next_tag=Tag} =St) ->
	{Tag,St#st{next_tag=Tag+1}};
pick_tag(#st{tags=[Tag|Tags]} =St) ->
	{Tag,St#st{tags=Tags}}.

pick_fid(#st{fids=[],next_fid=Fid} =St) ->
	{Fid,St#st{next_fid=Fid+1}};
pick_fid(#st{fids=[Fid|Fids]} =St) ->
	{Fid,St#st{fids=Fids}}.

%%-------- session recovery ----------------------------------------------------

%% recover_session(_Reason, #st{trans_mod =TransMod,
%% 							 trans_conf =TransConf,
%% 						 	 msize =MSize} =St) ->
%% 	
%% 	{ok,NewSoc} = TransMod:connect(TransConf, ?CONN_TIMEO) of
%% 
%% 	VerMsg = {tversion,?NOTAG,MSize,version_bin(e)},
%% 	ok = TransMod:send(NewSock, VerMsg, TransConf),
%% 
%%  %%NB: recv() may return decoded messages
%% 	{ok,Reply1} = TransMod:recv(NewSock, TransConf),
%% 	{rversion,_,MSize,_} = decode(Reply1),
%% 
%% 	SessMsg = {tsession,?NOTAG,SessionKey},
%% 	ok = TransMod:send(NewSock, SessMsg, TransConf),
%% 
%% 	{ok,Reply2} = TransMod:recv(NewSock, TransConf),
%% 	case decode(Reply2) of
%% 	{rsession,?NOTAG} ->
%% 
%% 		%% session recovered -- resubmit all pending requests

%%-------- decode/encode -------------------------------------------------------

%% @private

%% Tversion tag[2] msize[4] version[s]
decode(<<?Tversion,Tag:16/little,
				   MSize:32/little,
				   VSz:16/little,Version:(VSz)/binary>>) ->
	{tversion,Tag,MSize,Version};

%% Rversion tag[2] msize[4] version[s]
decode(<<?Rversion,Tag:16/little,
				   MSize:32/little,
				   VSz:16/little,Version:(VSz)/binary>>) ->
	{rversion,Tag,MSize,Version};

%% 9P2000.e:
%% Tsession tag[2] key[8]
decode(<<?Tsession,Tag:16/little,
				   SessionKey:8/binary>>) ->
	{tsession,Tag,SessionKey};

%% 9P2000.e:
%% Rsession tag[2]
decode(<<?Rsession,Tag:16/little>>) ->
	{rsession,Tag};

%% Rerror tag[2] message[s]
decode(<<?Rerror,Tag:16/little,
				 MSz:16/little,Msg:(MSz)/binary>>) ->
	{rerror,Tag,Msg};

%% 9P2000.u:
%% Rerror tag[2] message[s]
decode(<<?Rerror,Tag:16/little,
				 MSz:16/little,Msg:(MSz)/binary,
				 Posix:32/little>>) ->
	{rerror,Tag,Msg,posix_error(Posix)};

%% Tflush tag[2] oldtag[2]
decode(<<?Tflush,Tag:16/little,
				 OldTag:16/little>>) ->
	{tflush,Tag,OldTag};

%% Rflush tag[2]
decode(<<?Rflush,Tag:16/little>>) ->
	{rflush,Tag};

%% Twalk tag[2] fid[4] newfid[4] nwname[2] nwname*(wname[s])
decode(<<?Twalk,Tag:16/little,
				Fid:32/little,
				NewFid:32/little,
				N:16/little,NamesBin/binary>>) ->
	Names = [Name || <<Sz:16/little,Name:(Sz)/binary>> <= NamesBin],
	N = length(Names),
	{twalk,Tag,Fid,NewFid,Names};

%% Rwalk tag[2] nwqid[2] nwqid*(wqid[13])
decode(<<?Rwalk,Tag:16/little,
				N:16/little,QidBin/binary>>) ->
	Qids = [Qid || <<Qid:13/binary>> <= QidBin],
	N = length(Qids),
	{rwalk,Tag,Qids};

%% Tread tag[2] fid[4] offset[8] count[4]
decode(<<?Tread,Tag:16/little,
				Fid:32/little,
				Offset:64/little,
				Count:32/little>>) ->
	{tread,Tag,Fid,Offset,Count,noflag};

%% Rread tag[2] count[4] data[count]
decode(<<?Rread,Tag:16/little,
				N:32/little,Data:(N)/binary>>) ->
	{rread,Tag,Data};

%% Twrite tag[2] fid[4] offset[8] count[4] data[count]
decode(<<?Twrite,Tag:16/little,
				 Fid:32/little,
				 Offset:64/little,
				 Count:32/little,
				 Data:(Count)/binary>>) ->
	{twrite,Tag,Fid,Offset,Data,noflag};

%% Rwrite tag[2] count[4]
decode(<<?Rwrite,Tag:16/little,
				 N:32/little>>) ->
	{rwrite,Tag,N};

%% Tclunk tag[2] fid[4]
decode(<<?Tclunk,Tag:16/little,
				 Fid:32/little>>) ->
	{tclunk,Tag,Fid};

%% Rclunk tag[2]
decode(<<?Rclunk,Tag:16/little>>) ->
	{rclunk,Tag};

%% 9P2000.e:
%% Tsread tag[2] fid[4] nwname[2] nwname*(wname[s])
decode(<<?Tsread,Tag:16/little,
				 Fid:32/little,
				 N:16/little,NamesBin/binary>>) ->
	Names = [Name || <<Sz:16/little,Name:(Sz)/binary>> <= NamesBin],
	true = length(Names) =:= N,
	{tsread,Tag,Fid,Names};

%% Rsread tag[2] count[4] data[count]
decode(<<?Rsread,Tag:16/little,
				 Count:32/little,Data:(Count)/binary>>) ->
	{rsread,Tag,Data};

%% Tswrite tag[2] fid[4] nwname[2] nwname*(wname[s]) count[4] data[count]
decode(<<?Tswrite,Tag:16/little,
				  Fid:32/little,
				  N:16/little,NamesData/binary>>) ->
	{Names,Data} = decode_names_data(N, NamesData),
	{tswrite,Tag,Fid,Names,Data};

%% Rswrite tag[2] count[4]
decode(<<?Rswrite,Tag:16/little,
				  Count:32/little>>) ->
	{rswrite,Tag,Count};

%% Tremove tag[2] fid[4]
decode(<<?Tremove,Tag:16/little,
				  Fid:32/little>>) ->
	{tremove,Tag,Fid};

%% Rremove tag[2]
decode(<<?Rremove,Tag:16/little>>) ->
	{rremove,Tag};

%% 9P2000:
%% Tauth tag[2] afid[4] uname[s] aname[s]
decode(<<?Tauth,Tag:16/little,
				AFid:32/little,
				USz:16/little,UName:(USz)/binary,
				ASz:16/little,AName:(ASz)/binary>>) ->
	{tauth,Tag,AFid,UName,AName};

%% 9P2000.u/9P2000.L:
%% Tauth tag[2] afid[4] uname[s] aname[s] n_uname[4]
decode(<<?Tauth,Tag:16/little,
				AFid:32/little,
				USz:16/little,UName:(USz)/binary,
				ASz:16/little,AName:(ASz)/binary,
				Uid:32/little>>) ->
	{tauth,Tag,AFid,UName,AName,Uid};

%% Rauth tag[2] aqid[13]
decode(<<?Rauth,Tag:16/little,
				AQid:13/binary>>) ->
	{rauth,Tag,AQid};

%% 9P2000:
%% Tattach tag[2] fid[4] afid[4] uname[s] aname[s]
decode(<<?Tattach,Tag:16/little,
				  Fid:32/little,
				  AFid:32/little,
				  USz:16/little,UName:(USz)/binary,
				  ASz:16/little,AName:(ASz)/binary>>) ->
	{tattach,Tag,Fid,AFid,UName,AName};

%% 9P2000.u/9P2000.L:
%% Tattach tag[2] fid[4] afid[4] uname[s] aname[s] n_uname[4]
decode(<<?Tattach,Tag:16/little,
				  Fid:32/little,
				  AFid:32/little,
				  USz:16/little,UName:(USz)/binary,
				  ASz:16/little,AName:(ASz)/binary,
				  Uid:32/little>>) ->
	{tattach,Tag,Fid,AFid,UName,AName,Uid};

%% Rattach tag[2] qid[13]
decode(<<?Rattach,Tag:16/little,
				  Qid:13/binary>>) ->
	{rattach,Tag,Qid};

%% Rlerror tag[2] ecode[4]
decode(<<?Rlerror,Tag:16/little,
				  ErrNo:32/little>>) ->
	{rlerror,Tag,posix_error(ErrNo)};

%% Tstatfs tag[2] fid[4]
decode(<<?Tstatfs,Tag:16/little,
				  Fid:32/little>>) ->
	{tstatfs,Tag,Fid};

%% Rstatfs tag[2] type[4] bsize[4] blocks[8] bfree[8] bavail[8]
%%                 files[8] ffree[8] fsid[8] namelen[4]
decode(<<?Rstatfs,Tag:16/little,
				  Type:32/little,
				  BSize:32/little,
				  Blocks:64/little,
				  BFree:64/little,
				  BAvail:64/little,
				  Files:64/little,
				  FFree:64/little,
				  FSid:64/little,
				  NameLen:32/little>>) ->
	#rstatfs{tag=Tag,type=Type,bsize=BSize,
			 blocks=Blocks,bfree=BFree,bavail=BAvail,
			 files=Files,ffree=FFree,fsid=FSid,namelen=NameLen};

%% Topen tag[2] fid[4] mode[1]
decode(<<?Topen,Tag:16/little,
				Fid:32/little,
				Mode>>) ->
	{topen,Tag,Fid,bits_to_flags_old(Mode)};

%% Rlopen tag[2] qid[13] iounit[4]
decode(<<?Ropen,Tag:16/little,
				 Qid:13/binary,
				 Iounit:32/little>>) ->
	{ropen,Tag,Qid,Iounit};

%% 9P2000.L:
%% Tlopen tag[2] fid[4] flags[4]
decode(<<?Tlopen,Tag:16/little,
				 Fid:32/little,
				 Bits:32/little>>) ->
	{tlopen,Tag,Fid,bits_to_flags(Bits)};
	
%% 9P2000.L:
%% Rlopen tag[2] qid[13] iounit[4]
decode(<<?Rlopen,Tag:16/little,
				 Qid:13/binary,
				 Iounit:32/little>>) ->
	{rlopen,Tag,Qid,Iounit};

%% Tcreate tag[2] fid[4] name[s] perm[4] mode[1]
decode(<<?Tcreate,Tag:16/little,
				  Fid:32/little,
				  Sz:16/little,Name:(Sz)/binary,
				  Perm:32/little,
				  Mode>>) ->
	{tcreate,Tag,Fid,Name,Perm,Mode};

%% 9P2000.u:
%% Tcreate tag[2] fid[4] name[s] perm[4] mode[1] extension[s]
decode(<<?Tcreate,Tag:16/little,
				  Fid:32/little,
				  Sz:16/little,Name:(Sz)/binary,
				  Perm:32/little,
				  Mode,
				  ESz:16/little,Ext:(ESz)/binary>>) ->
	{tcreate,Tag,Fid,Name,Perm,Mode,Ext};

%% Rcreate tag[2] qid[13] iounit[4]
decode(<<?Rcreate,Tag:16/little,
				  Qid:13/binary,
				  Iounit:32/little>>) ->
	{rcreate,Tag,Qid,Iounit};

%% 9P2000.L:
%% Tlcreate tag[2] fid[4] name[s] flags[4] mode[4] gid[4]
decode(<<?Tlcreate,Tag:16/little,
				   Fid:32/little,
				   Sz:16/little,Name:(Sz)/binary,
				   Flags:32/little,
				   Mode:32/little,
				   Gid:32/little>>) ->
	%% 9p2000.L protocol doc says Flags are ignored by diod
	{tlcreate,Tag,Fid,Name,Flags,Mode,Gid};

%% 9P2000.L:
%% Rlcreate tag[2] qid[13] iounit[4]
decode(<<?Rlcreate,Tag:16/little,
				   Qid:13/binary,
				   Iounit:32/little>>) ->
	{rlcreate,Tag,Qid,Iounit};

%% Tsymlink tag[2] fid[4] name[s] symtgt[s] gid[4]
decode(<<?Tsymlink,Tag:16/little,
				   Fid:32/little,
				   NSz:16/little,Name:(NSz)/binary,
				   TSz:16/little,Target:(TSz)/binary,
				   Gid:32/little>>) ->
	{tsymlink,Tag,Fid,Name,Target,Gid};

%% Rsymlink tag[2] qid[13]
decode(<<?Rsymlink,Tag:16/little,
				   Qid:13/binary>>) ->
	{rsymlink,Tag,Qid};

%% Tmknod tag[2] dfid[4] name[s] mode[4] major[4] minor[4] gid[4]
decode(<<?Tmknod,Tag:16/little,
				 DFid:32/little,
				 Sz:16/little,Name:(Sz)/binary,
				 Mode:32/little,
				 Major:32/little,
				 Minor:32/little,
				 Gid:32/little>>) ->
	{tmknod,Tag,DFid,Name,Mode,Major,Minor,Gid};

%% Rmknod tag[2] qid[13]
decode(<<?Rmknod,Tag:16/little,
				 Qid:13/binary>>) ->
	{rmknod,Tag,Qid};

%% Trename tag[2] fid[4] dfid[4] name[s]
decode(<<?Trename,Tag:16/little,
				  Fid:32/little,
				  DFid:32/little,
				  Sz:16/little,Name:(Sz)/binary>>) ->
	{trename,Tag,Fid,DFid,Name};

%% Rrename tag[2]
decode(<<?Rrename,Tag:16/little>>) ->
	{rrename,Tag};

%% Treadlink tag[2] fid[4]
decode(<<?Treadlink,Tag:16/little,
					Fid:32/little>>) ->
	{treadlink,Tag,Fid};

%% Rreadlink tag[2] target[s]
decode(<<?Rreadlink,Tag:16/little,
					TSz:16/little,Target:(TSz)/binary>>) ->
	{rreadlink,Tag,Target};

%% Tstat tag[2] fid[4]
decode(<<?Tstat,Tag:16/little,
				Fid:32/little>>) ->
	{tstat,Tag,Fid,nomask};

%% Rstat tag[2] stat[n]
decode(<<?Rstat,Tag:16/little,
				Sz:16/little,StatB:(Sz)/binary>>) ->
	{rstat,Tag,decode_stat(StatB)};

%% Twstat tag[2] fid[4] stat[n]
decode(<<?Twstat,Tag:16/little,
				 Fid:32/little,
				 Sz:16/little,Stat:(Sz)/binary>>) ->
	{twstat,Tag,Fid,decode_stat(Stat)};

%% Rwstat tag[2]
decode(<<?Rwstat,Tag:16/little>>) ->
	{rwstat,Tag};

%% Tgetattr tag[2] fid[4] request_mask[8]
decode(<<?Tgetattr,Tag:16/little,
				   Fid:32/little,
				   Mask:64/little>>) ->
	{tgetattr,Tag,Fid,Mask};

%% Rgetattr tag[2] valid[8] qid[13] mode[4] uid[4] gid[4] nlink[8]
%%                  rdev[8] size[8] blksize[8] blocks[8]
%%	                atime_sec[8] atime_nsec[8] mtime_sec[8] mtime_nsec[8]
%%					ctime_sec[8] ctime_nsec[8] btime_sec[8] btime_nsec[8]
%%					gen[8] data_version[8]
decode(<<?Rgetattr,Tag:16/little,
				   Valid:64/little,
				   _QidType,
				   _QidVer:32/little,
				   QidPath:64/little,
				   Mode:32/little,
				   Uid:32/little,
				   Gid:32/little,
				   NLink:64/little,
				   RDev:64/little,
				   Size:64/little,
				   BlkSize:64/little,
				   Blocks:64/little,
				   ATimeSec:64/little,
				   ATimeNSec:64/little,
				   MTimeSec:64/little,
				   MTimeNSec:64/little,
				   CTimeSec:64/little,
				   CTimeNSec:64/little,
				   BTimeSec:64/little,
				   BTimeNSec:64/little,
				   Gen:64/little,
				   DataVersion:64/little>>) ->
	#rgetattr{tag=Tag,valid=Valid,mode=Mode,uid=Uid,gid=Gid,
			  nlink=NLink,rdev=RDev,ino=QidPath,size=Size,
			  blksize=BlkSize,blocks=Blocks,
			  atime_sec=ATimeSec,atime_nsec=ATimeNSec,
			  mtime_sec=MTimeSec,mtime_nsec=MTimeNSec,
			  ctime_sec=CTimeSec,ctime_nsec=CTimeNSec,
			  btime_sec=BTimeSec,btime_nsec=BTimeNSec,
			  gen=Gen,data_version=DataVersion};

%% Tsetattr tag[2] fid[4] valid[4] mode[4] uid[4] gid[4] size[8]
%%                 atime_sec[8] atime_nsec[8] mtime_sec[8] mtime_nsec[8]
decode(<<?Tsetattr,Tag:16/little,
				   Fid:32/little,
				   Valid:64/little,
				   Mode:32/little,
				   Uid:32/little,
				   Gid:32/little,
				   Size:64/little,
				   ATimeSec:64/little,
				   ATimeNSec:64/little,
				   MTimeSec:64/little,
				   MTimeNSec:64/little>>) ->
	{tsetattr,Tag,Fid,Valid,Mode,Uid,Gid,Size,
			ATimeSec,ATimeNSec,MTimeSec,MTimeNSec};

%% Rsetattr tag[2] 
decode(<<?Rsetattr,Tag:16/little>>) ->
	{rsetattr,Tag};

%% Txattrwalk tag[2] fid[4] attrfid[4] name[s]
decode(<<?Txattrwalk,Tag:16/little,
					 Fid:32/little,
					 AttrFid:32/little,
					 Sz:16/little,Name:(Sz)/binary>>) ->
	{txattrwalk,Tag,Fid,AttrFid,Name};

%% Rxattrwalk tag[2] size[8]
decode(<<?Rxattrwalk,Tag:16/little,
					 Size:64/little>>) ->
	{rxattrwalk,Tag,Size};

%% Txattrcreate tag[2] fid[4] name[s] size[8] flag[4]
decode(<<?Txattrcreate,Tag:16/little,
					   Fid:32/little,
					   Sz:16/little,Name:(Sz)/binary,
					   Size:64/little,
					   Flag:32/little>>) ->
	{txattrcreate,Tag,Fid,Name,Size,Flag};

%% Rxattrcreate tag[2]
decode(<<?Rxattrcreate,Tag:16/little>>) ->
	{rxattrcreate,Tag};

%% Treaddir tag[2] fid[4] offset[8] count[4]
decode(<<?Treaddir,Tag:16/little,
				   Fid:32/little,
				   Offset:64/little,
				   Count:32/little>>) ->
	{treaddir,Tag,Fid,Offset,Count};

%% Rreaddir tag[2] count[4] data[count]
decode(<<?Rreaddir,Tag:16/little,
				   DSz:32/little,Data:(DSz)/binary>>) ->
	Ents = [{Qid,Offset,Type,Name} ||
				<<Qid:13/binary,
				  Offset:64/little,
				  Type,
				  NSz:16/little,Name:(NSz)/binary>> <= Data],
	{rreaddir,Tag,Ents};

%% Tfsync tag[2] fid[4]
decode(<<?Tfsync,Tag:16/little,
				 Fid:32/little>>) ->
	{tfsync,Tag,Fid};

%% Rfsync tag[2]
decode(<<?Rfsync,Tag:16/little>>) ->
	{rfsync,Tag};

%% Tlock tag[2] fid[4] type[1] flags[4] start[8] length[8]
%%				proc_id[4] client_id[s]
decode(<<?Tlock,Tag:16/little,
				Fid:32/little,
				Type,
				Flags:32/little,
				Start:64/little,
				Length:64/little,
				ProcId:32/little,
				Sz:16/little,ClientId:(Sz)/binary>>) ->
	{tlock,Tag,Fid,Type,Flags,Start,Length,ProcId,ClientId};

%% Rlock tag[2] status[1]
decode(<<?Rlock,Tag:16/little,
				Status>>) ->
	{rlock,Tag,lock_stat(Status)};

%% Tgetlock tag[2] fid[4] type[1] start[8] length[8] proc_id[4] client_id[s]
decode(<<?Tgetlock,Tag:16/little,
				   Fid:32/little,
				   Type,
				   Start:64/little,
				   Length:64/little,
				   ProcId:32/little,
				   Sz:16/little,ClientId:(Sz)/binary>>) ->
	{tgetlock,Tag,Fid,Type,Start,Length,ProcId,ClientId};

%% Rgetlock tag[2] type[1] start[8] length[8] proc_id[4] client_id[s]
decode(<<?Rgetlock,Tag:16/little,
				   Type,
				   Start:64/little,
				   Length:64/little,
				   ProcId:32/little,
				   Sz:16/little,ClientId:(Sz)/binary>>) ->
	{rgetlock,Tag,Type,Start,Length,ProcId,ClientId};

%% Tlink tag[2] dfid[4] fid[4] name[s]
decode(<<?Tlink,Tag:16/little,
				DFid:32/little,
				Fid:32/little,
				Sz:16/little,Name:(Sz)/binary>>) ->
	{tlink,Tag,DFid,Fid,Name};

%% Rlink tag[2]
decode(<<?Rlink,Tag:16/little>>) ->
	{rlink,Tag};

%% Tmkdir tag[2] dfid[4] name[s] mode[4] gid[4]
decode(<<?Tmkdir,Tag:16/little,
				 DFid:32/little,
				 Sz:16/little,Name:(Sz)/binary,
				 Mode:32/little,
				 Gid:32/little>>) ->
	{tmkdir,Tag,DFid,Name,Mode,Gid};

%% Rmkdir tag[2] qid[13]
decode(<<?Rmkdir,Tag:16/little,
				 Qid:13/binary>>) ->
	{rmkdir,Tag,Qid};

%% Trenameat tag[2] olddirfid[4] oldname[s] newdirfid[4] newname[s]
decode(<<?Trenameat,Tag:16/little,
					OldDFid:32/little,
					OSz:16/little,OldName:(OSz)/binary,
					NewDFid:32/little,
					NSz:16/little,NewName:(NSz)/binary>>) ->
	{trenameat,Tag,OldDFid,OldName,NewDFid,NewName};

%% Rrenameat tag[2]
decode(<<?Rrenameat,Tag:16/little>>) ->
	{rrenameat,Tag};

%% Tunlinkat tag[2] dirfd[4] name[s] flags[4]
decode(<<?Tunlinkat,Tag:16/little,
					DFid:32/little,
					Sz:16/little,Name:(Sz)/binary,
					Flags:32/little>>) ->
	{tunlinkat,Tag,DFid,Name,Flags};

%% Runlinkat tag[2]
decode(<<?Runlinkat,Tag:16/little>>) ->
	{runlinkat,Tag}.

%% a helper to decode: nwname*(wname[s]) count[4] data[count]
decode_names_data(N, NamesData) ->
	decode_names_data(N, NamesData, []).

decode_names_data(0, <<Count:32/little,Data:(Count)/binary>>, Names) ->
	{lists:reverse(Names),Data};
decode_names_data(N, <<Sz:16/little,Name:(Sz)/binary,More/binary>>, Names) ->
	decode_names_data(N -1, More, [Name|Names]).

%% @private
encode(Req) ->
	Bin = encode1(Req),
	<<(size(Bin)+4):32/little,Bin/binary>>.

%% Tversion tag[2] msize[4] version[s]
encode1({tversion,Tag,MSize,Version}) ->
	<<?Tversion,Tag:16/little,MSize:32/little,
				(size(Version)):16/little,Version/binary>>;

%% Rversion tag[2] msize[4] version[s]
encode1({rversion,Tag,MSize,Version}) ->
	<<?Rversion,Tag:16/little,MSize:32/little,
				(size(Version)):16/little,Version/binary>>;

%% 9P2000.e:
%% Tsession tag[2] key[8]
encode1({tsession,Tag,SessionKey}) ->
	<<?Tsession,Tag:16/little,
				SessionKey:8/binary>>;

%% 9P2000.e:
%% Rsession tag[2]
encode1({rsession,Tag}) ->
	<<?Rsession,Tag:16/little>>;

%% Rerror tag[2] message[s]
encode1({rerror,Tag,Msg}) ->
	<<?Rerror,Tag:16/little,
			  (size(Msg)):16/little,Msg/binary>>;

%% 9P2000.u:
%% Rerror tag[2] message[s] errno[4]
encode1({rerror,Tag,Msg,Error}) ->
	<<?Rerror,Tag:16/little,
			  (size(Msg)):16/little,Msg/binary,
			  (error_posix(Error)):32/little>>;

%% Tflush tag[2] oldtag[2]
encode1({tflush,Tag,OldTag}) ->
	<<?Tflush,Tag:16/little,
			  OldTag:16/little>>;

%% Rflush tag[2]
encode1({rflush,Tag}) ->
	<<?Rflush,Tag:16/little>>;

%% Twalk tag[2] fid[4] newfid[4] nwname[2] nwname*(wname[s])
encode1({twalk,Tag,Fid,NewFid,Names}) ->
	NamesBin = << <<(size(N)):16/little,N/binary>>
										|| N <- Names >>,
	<<?Twalk,Tag:16/little,
			 Fid:32/little,
			 NewFid:32/little,
			 (length(Names)):16/little,NamesBin/binary>>;

%% Rwalk tag[2] nwqid[2] nwqid*(wqid[13])
encode1({rwalk,Tag,Qids}) ->
	QidsBin = << <<Qid:13/binary>> || Qid <- Qids >>,
	<<?Rwalk,Tag:16/little,
			 (length(Qids)):16/little,QidsBin/binary>>;

%% Tread tag[2] fid[4] offset[8] count[4]
encode1({tread,Tag,Fid,Offset,N,_Flag}) ->
	<<?Tread,Tag:16/little,
			 Fid:32/little,
			 Offset:64/little,
			 N:32/little>>;

%% Rread tag[2] count[4] data[count]
encode1({rread,Tag,Data}) ->
	<<?Rread,Tag:16/little,
			 (size(Data)):32/little,Data/binary>>;

%% Twrite tag[2] fid[4] offset[8] count[4] data[count]
encode1({twrite,Tag,Fid,Offset,Data,_Flag}) ->
	<<?Twrite,Tag:16/little,
			  Fid:32/little,
			  Offset:64/little,
			  (size(Data)):32/little,Data/binary>>;

%% Rwrite tag[2] count[4]
encode1({rwrite,Tag,Count}) ->
	<<?Rwrite,Tag:16/little,
			  Count:32/little>>;

%% Tclunk tag[2] fid[4]
encode1({tclunk,Tag,Fid}) ->
	<<?Tclunk,Tag:16/little,
			  Fid:32/little>>;

%% Rclunk tag[2]
encode1({rclunk,Tag}) ->
	<<?Rclunk,Tag:16/little>>;

%% 9P2000.e:
%% Tsread tag[2] fid[4] nwname[2] nwname*(wname[s])
encode1({tsread,Tag,Fid,Names}) ->
	NamesBin = << <<(size(N)):16/little,N/binary>>
										|| N <- Names >>,
	<<?Tsread,Tag:16/little,
			  Fid:32/little,
			  (length(Names)):16/little,NamesBin/binary>>;

%% 9P2000.e:
%% Rsread tag[2] count[4] data[count]
encode1({rsread,Tag,Data}) ->
	<<?Rsread,Tag:16/little,
			  (size(Data)):32/little,Data/binary>>;

%% Tswrite tag[2] fid[4] nwname[2] nwname*(wname[s]) count[4] data[count]
encode1({tswrite,Tag,Fid,Names,Data}) ->
	NamesBin = << <<(size(N)):16/little,N/binary>>
										|| N <- Names >>,
	<<?Tswrite,Tag:16/little,
			   Fid:32/little,
			   (length(Names)):16/little,NamesBin/binary,
			   (size(Data)):32/little,Data/binary>>;

%% Rswrite tag[2] count[4]
encode1({rswrite,Tag,Count}) ->
	<<?Rswrite,Tag:16/little,
			   Count:32/little>>;

%% Tremove tag[2] fid[4]
encode1({tremove,Tag,Fid}) ->
	<<?Tremove,Tag:16/little,
			   Fid:32/little>>;

%% Rremove tag[2]
encode1({rremove,Tag}) ->
	<<?Rremove,Tag:16/little>>;

%% 9P2000:
%% Tauth tag[2] afid[4] uname[s] aname[s]
encode1({tauth,Tag,AFid,UName,AName}) ->
	<<?Tauth,Tag:16/little,
			 AFid:32/little,
			 (size(UName)):16/little,UName/binary,
			 (size(AName)):16/little,AName/binary>>;

%% 9P2000.u:
%% Tauth tag[2] afid[4] uname[s] aname[s] n_uname[4]
encode1({tauth,Tag,AFid,UName,AName,Uid}) ->
	<<?Tauth,Tag:16/little,
			 AFid:32/little,
			 (size(UName)):16/little,UName/binary,
			 (size(AName)):16/little,AName/binary,
			 Uid:32/little>>;

%% Rauth tag[2] aqid[13]
encode1({rauth,Tag,Qid}) ->
	<<?Rauth,Tag:16/little,
			 Qid:13/binary>>;

%% 9P2000:
%% Tattach tag[2] fid[4] afid[4] uname[s] aname[s]
encode1({tattach,Tag,Fid,AFid,UName,AName}) ->
	<<?Tattach,Tag:16/little,
			   Fid:32/little,AFid:32/little,
			   (size(UName)):16/little,UName/binary,
			   (size(AName)):16/little,AName/binary>>;

%% 9P2000.u:
%% Tattach tag[2] fid[4] afid[4] uname[s] aname[s] n_uname[4]
encode1({tattach,Tag,Fid,AFid,UName,AName,Uid}) ->
	<<?Tattach,Tag:16/little,
			   Fid:32/little,AFid:32/little,
			   (size(UName)):16/little,UName/binary,
			   (size(AName)):16/little,AName/binary,
			   Uid:32/little>>;

%% Rattach tag[2] aqid[13]
encode1({rattach,Tag,Qid}) ->
	<<?Rattach,Tag:16/little,
			   Qid:13/binary>>;

%% Rlerror tag[2] ecode[4]
encode1({rlerror,Tag,Error}) ->
	<<?Rlerror,Tag:16/little,
			   (error_posix(Error)):32/little>>;

%% Tstatfs tag[2] fid[4]
encode1({tstatfs,Tag}) ->
	<<?Tstatfs,Tag:16/little>>;

%% Rstatfs tag[2] type[4] bsize[4] blocks[8] bfree[8] bavail[8]
%%                files[8] ffree[8] fsid[8] namelen[4]
encode1(#rstatfs{tag=Tag,type=Type,bsize=BSize,
				blocks=Blocks,bfree=BFree,bavail=BAvail,
				files=Files,ffree=FFree,fsid=FSid,namelen=NameLen}) ->
	<<?Rstatfs,Tag:16/little,
			   Type:32/little,
			   BSize:32/little,
			   Blocks:64/little,
			   BFree:64/little,
			   BAvail:64/little,
			   Files:64/little,
			   FFree:64/little,
			   FSid:64/little,
			   NameLen:32/little>>;

%% Topen tag[2] fid[4] mode[1]
encode1({topen,Tag,Fid,Flags}) ->
	Mode = bit_flags_old(Flags),
	<<?Topen,Tag:16/little,
			 Fid:32/little,
			 Mode>>;

%% Ropen tag[2] qid[13] iounit[4]
encode1({ropen,Tag,Qid,Iounit}) ->
		<<?Ropen,Tag:16/little,
				 Qid:13/binary,
				 Iounit:32/little>>;

%% 9P2000.L:
%% Tlopen tag[2] fid[4] flags[4]
encode1({tlopen,Tag,Fid,Flags}) ->
	Mode = bit_flags(Flags),
	<<?Tlopen,Tag:16/little,
			  Fid:32/little,
			  Mode:32/little>>;

%% 9P2000.L:
%% Rlopen tag[2] qid[13] iounit[4]
encode1({rlopen,Tag,Qid,Iounit}) ->
		<<?Rlopen,Tag:16/little,
				  Qid:13/binary,
				  Iounit:32/little>>;

%% Tcreate tag[2] fid[4] name[s] perm[4] mode[4]
encode1({tcreate,Tag,Fid,Name,Perm,Mode}) ->
	<<?Tcreate,Tag:16/little,
			   Fid:32/little,
			   (size(Name)):16/little,Name/binary,
			   Perm:32/little,
			   Mode>>;

%% 9P2000.L:
%% Rcreate tag[2] qid[13] iounit[4]
encode1({rcreate,Tag,Qid,Iounit}) ->
	<<?Rcreate,Tag:16/little,
			   Qid:13/binary,
			   Iounit:32/little>>;

%% 9P2000.L:
%% Tlcreate tag[2] fid[4] name[s] flags[4] mode[4] gid[4]
encode1({tlcreate,Tag,Fid,Name,Flags,Mode,Gid}) ->
	<<?Tlcreate,Tag:16/little,
				Fid:32/little,
				(size(Name)):16/little,Name/binary,
				Flags:32/little,
				Mode:32/little,
				Gid:32/little>>;

%% 9P2000.L:
%% Rlcreate tag[2] qid[13] iounit[4]
encode1({rlcreate,Tag,Qid,Iounit}) ->
	<<?Rlcreate,Tag:16/little,
				Qid:13/binary,
				Iounit:32/little>>;

%% Tsymlink tag[2] fid[4] name[s] symtgt[s] gid[4]
encode1({tsymlink,Tag,DFid,Name,Target,Gid}) ->
	<<?Tsymlink,Tag:16/little,
				DFid:32/little,
				(size(Name)):16/little,Name/binary,
				(size(Target)):16/little,Target/binary,
				Gid:32/little>>;

%% Rsymlink tag[2] qid[13]
encode1({rsymlink,Tag,Qid}) ->
	<<?Rsymlink,Tag:16/little,
				Qid:13/binary>>;

%% Tmknod tag[2] dfid[4] name[s] mode[4] major[4] minor[4] gid[4]
encode1({tmknod,Tag,DFid,Name,Mode,Major,Minor,Gid}) ->
	<<?Tmknod,Tag:16/little,
			  DFid:32/little,
			  (size(Name)):16/little,Name/binary,
			  Mode:32/little,
			  Major:32/little,
			  Minor:32/little,
			  Gid:32/little>>;

%% Rmknod tag[2] qid[13]
encode1({rmknod,Tag,Qid}) ->
	<<?Rmknod,Tag:16/little,
			  Qid:13/binary>>;

%% Trename tag[2] fid[4] dfid[4] name[s]
encode1({trename,Tag,Fid,DFid,Name}) ->
	<<?Trename,Tag:16/little,
			   Fid:32/little,
			   DFid:32/little,
			   (size(Name)):16/little,Name/binary>>;

%% Rrename tag[2]
encode1({rrename,Tag}) ->
	<<?Rrename,Tag:16/little>>;

%% Treadlink tag[2] fid[4]
encode1({treadlink,Tag,Fid}) ->
	<<?Treadlink,Tag:16/little,
				 Fid:32/little>>;

%% Rreadlink tag[2] target[s]
encode1({rreadlink,Tag,Target}) ->
	<<?Rreadlink,Tag:16/little,
				 (size(Target)):16/little,Target/binary>>;

%% Tstat tag[2] fid[4]
encode1({tstat,Tag,Fid}) ->
	<<?Tstat,Tag:16/little,
			 Fid:32/little>>;

%% Tstat with ValidMask
encode1({tstat,Tag,Fid,_ValidMask}) ->
	<<?Tstat,Tag:16/little,
			 Fid:32/little>>;

%% Rstat tag[2] stat[n]
encode1({rstat,Tag,Stat}) ->
	StatB = encode_stat(Stat),
	<<?Rstat,Tag:16/little,
			 (size(StatB)):16/little,StatB/binary>>;

%% Twstat tag[2] fid[4] stat[n]
encode1({twstat,Tag,Fid,Stat}) ->
	StatB = encode_stat(Stat),
	<<?Twstat,Tag:16/little,
			  Fid:32/little,
			  (size(StatB)):16/little,StatB/binary>>;

%% Rwstat tag[2]
encode1({rwstat,Tag}) ->
	<<?Rwstat,Tag:16/little>>;

%% Tgetattr tag[2] fid[4] request_mask[8]
encode1({tgetattr,Tag,Fid,Mask}) ->
	<<?Tgetattr,Tag:16/little,
				Fid:32/little,
				Mask:64/little>>;

%% Rgetattr tag[2] valid[8] qid[13] mode[4] uid[4] gid[4] nlink[8]
%%                  rdev[8] size[8] blksize[8] blocks[8]
%%	                atime_sec[8] atime_nsec[8] mtime_sec[8] mtime_nsec[8]
%%					ctime_sec[8] ctime_nsec[8] btime_sec[8] btime_nsec[8]
%%					gen[8] data_version[8]
encode1(#rgetattr{tag=Tag,valid=Valid,mode=Mode,uid=Uid,gid=Gid,
			nlink=NLink,rdev=RDev,ino=QidPath,size=Size,
			blksize=BlkSize,blocks=Blocks,
			atime_sec=ATimeSec,atime_nsec=ATimeNSec,
			mtime_sec=MTimeSec,mtime_nsec=MTimeNSec,
			ctime_sec=CTimeSec,ctime_nsec=CTimeNSec,
			btime_sec=BTimeSec,btime_nsec=BTimeNSec,
			gen=Gen,data_version=DataVersion}) ->
		<<?Rgetattr,Tag:16/little,
					Valid:64/little,
					0,				%% QidType,
					0:32/little,	%% QidVer
					QidPath:64/little,
					Mode:32/little,
					Uid:32/little,
					Gid:32/little,
					NLink:64/little,
					RDev:64/little,
					Size:64/little,
					BlkSize:64/little,
					Blocks:64/little,
					ATimeSec:64/little,
					ATimeNSec:64/little,
					MTimeSec:64/little,
					MTimeNSec:64/little,
					CTimeSec:64/little,
					CTimeNSec:64/little,
					BTimeSec:64/little,
					BTimeNSec:64/little,
					Gen:64/little,
					DataVersion:64/little>>;

%% Tsetattr tag[2] fid[4] valid[4] mode[4] uid[4] gid[4] size[8]
%%                  atime_sec[8] atime_nsec[8] mtime_sec[8] mtime_nsec[8]
encode1({tsetattr,Tag,Fid,Valid,Mode,Uid,Gid,Size,
				ATimeSec,ATimeNSec,MTimeSec,MTimeNSec}) ->
	<<?Tsetattr,Tag:16/little,
				Fid:32/little,
				Valid:32/little,
				Mode:32/little,
				Uid:32/little,
				Gid:32/little,
				Size:64/little,
				ATimeSec:64/little,
				ATimeNSec:64/little,
				MTimeSec:64/little,
				MTimeNSec:64/little>>;

%% Rsetattr tag[2] 
encode1({rsetattr,Tag}) ->
	<<?Rsetattr,Tag:16/little>>;

%% Txattrwalk tag[2] fid[4] attrfid[4] name[s]
encode1({txattrwalk,Tag,Fid,AttrFid,Name}) ->
	<<?Txattrwalk,Tag:16/little,
				  Fid:32/little,
				  AttrFid:32/little,
				  (size(Name)):16/little,Name/binary>>;

%% Rxattrwalk tag[2] size[8]
encode1({rxattrwalk,Tag,Size}) ->
	<<?Rxattrwalk,Tag:16/little,
				  Size:64/little>>;

%% Txattrcreate tag[2] fid[4] name[s] size[8] flag[4]
encode1({txattrcreate,Tag,Fid,Name,Size,Flag}) ->
	<<?Txattrcreate,Tag:16/little,
				    Fid:32/little,
					(size(Name)):16/little,Name/binary,
					Size:64/little,
					Flag:32/little>>;

%% Rxattrcreate tag[2]
encode1({rxattrcreate,Tag}) ->
	<<?Rxattrcreate,Tag:16/little>>;

%% Treaddir tag[2] fid[4] offset[8] count[4]
encode1({treaddir,Tag,Fid,Offset,N}) ->
	<<?Treaddir,Tag:16/little,
				Fid:32/little,
				Offset:64/little,
				N:32/little>>;

%% Rreaddir tag[2] count[4] data[count]
encode1({rreaddir,Tag,Ents}) ->
	EntsBin = << <<Qid:13/binary,
				   Offset:64/little,
				   Type,
				   (size(Name)):16/little,Name/binary>>
				   		|| {Qid,Offset,Type,Name} <- Ents >>,

	<<?Rreaddir,Tag:16/little,
				(size(EntsBin)):32/little,EntsBin/binary>>;

%% Tfsync tag[2] fid[4]
encode1({tfsync,Tag,Fid}) ->
	<<?Tfsync,Tag:16/little,
			  Fid:32/little>>;

%% Rfsync tag[2]
encode1({rfsync,Tag}) ->
	<<?Rfsync,Tag:16/little>>;

%% Tlock tag[2] fid[4] type[1] flags[4] start[8] length[8]
%%				 proc_id[4] client_id[s]
encode1({tlock,Tag,Fid,Type,Flags,Start,Length,ProcId,ClientId}) ->
	<<?Tlock,Tag:16/little,
			 Fid:32/little,
			 Type,
			 Flags:32/little,
			 Start:64/little,
			 Length:64/little,
			 ProcId:32/little,
			 ClientId:32/little>>;

%% Rlock tag[2] status[1]
encode1({rlock,Tag,Status}) ->
	<<?Rlock,Tag:16/little,(lock_code(Status))>>;

%% Tgetlock tag[2] fid[4] type[1] start[8] length[8]
%%					proc_id[4] client_id[s]
encode1({tgetlock,Tag,Fid,Type,Start,Length,ProcId,ClientId}) ->
	<<?Tgetlock,Tag:16/little,
				Fid:32/little,
				Type,
				Start:64/little,
				Length:64/little,
				ProcId:32/little,
				ClientId:32/little>>;

%% Rgetlock tag[2] type[1] start[8] length[8] proc_id[4] client_id[s]
encode1({rgetlock,Tag,Type,Start,Length,ProcId,ClientId}) ->
	<<?Rgetlock,Tag:16/little,
				Type,
				Start:64/little,
				Length:64/little,
				ProcId:32/little,
				(size(ClientId)):16/little,ClientId/binary>>;

%% Tlink tag[2] dfid[4] fid[4] name[s]
encode1({tlink,Tag,DFid,Fid,Name}) ->
	<<?Tlink,Tag:16/little,
			 DFid:32/little,
			 Fid:32/little,
			 (size(Name)):16/little,Name/binary>>;

%% Rlink tag[2]
encode1({rlink,Tag}) ->
	<<?Rlink,Tag:16/little>>;

%% Tmkdir tag[2] dfid[4] name[s] mode[4] gid[4]
encode1({tmkdir,Tag,DFid,Name,Mode,Gid}) ->
	<<?Tmkdir,Tag:16/little,
			  DFid:32/little,
			  (size(Name)):16/little,Name/binary,
			  Mode:32/little,
			  Gid:32/little>>;

%% Rmkdir tag[2] qid[13]
encode1({rmkdir,Tag,Qid}) ->
	<<?Rmkdir,Tag:16/little,
			  Qid:13/binary>>;

%% Trenameat tag[2] olddirfid[4] oldname[s] newdirfid[4] newname[s]
encode1({trenameat,Tag,OldDirFid,OldName,NewDirFid,NewName}) ->
	<<?Trenameat,Tag:16/little,
				 OldDirFid:32/little,
				 (size(OldName)):16/little,OldName/binary,
				 NewDirFid:32/little,
				 (size(NewName)):16/little,NewName/binary>>;

%% Rrenameat tag[2]
encode1({rrenameat,Tag}) ->
	<<?Rrenameat,Tag:16/little>>;

%% Tunlinkat tag[2] dirfd[4] name[s] flags[4]
encode1({tunlinkat,Tag,DirFid,Name,Flags}) ->
	<<?Tunlinkat,Tag:16/little,
				 DirFid:32/little,
				 (size(Name)):16/little,Name/binary,
				 Flags:32/little>>;

%% Runlinkat tag[2]
encode1({runlinkat,Tag}) ->
	<<?Runlinkat,Tag:16/little>>.

%% 9P2000:
bits_to_flags_old(Bits) ->
	Tab = [{16#10,trunc},
		   {16#40,rclose}],

	Base = case Bits band 3 of
		0 -> [rdonly];
		1 -> [wronly];
		2 -> [rdwr]
	end,

	lists:foldl(fun({Mask,Flag}, Flags) when Bits band Mask =/= 0 ->
						[Flag|Flags];
				   (_, Flags) ->
						Flags
				end, Base, Tab).

bits_to_flags(Bits) ->
	Tab = [{8#100,creat},
		   {8#200,excl},
		   {8#400,noctty},
		   {8#1000,trunc},
		   {8#2000,append},
		   {8#4000,ndelay},
		   {8#10000,sync},
		   {8#20000,fsync},
		   {8#40000,async}],

	Base = case Bits band 3 of
		0 -> [rdonly];
		1 -> [wronly];
		2 -> [rdwr]
	end,

	lists:foldl(fun({Mask,Flag}, Flags) when Bits band Mask =/= 0 ->
						[Flag|Flags];
				   (_, Flags) ->
						Flags
				end, Base, Tab).

lock_stat(0) -> success;
lock_stat(1) -> blocked;
lock_stat(2) -> error;
lock_stat(3) -> grace.

lock_code(success) -> 0;
lock_code(blocked) -> 1;
lock_code(error) -> 2;
lock_code(grace) -> 3.

posix_error(1) -> eperm;
posix_error(2) -> enoent;
posix_error(3) -> esrch;
posix_error(4) -> eintr;
posix_error(5) -> eio;
posix_error(6) -> enxio;
posix_error(7) -> e2big;
posix_error(8) -> enoexec;
posix_error(9) -> ebadf;
posix_error(10) -> echild;
posix_error(11) -> eagain;
posix_error(12) -> enomem;
posix_error(13) -> eacces;
posix_error(14) -> efault;
posix_error(15) -> enotblk;
posix_error(16) -> ebusy;
posix_error(17) -> eexist;
posix_error(18) -> exdev;
posix_error(19) -> enodev;
posix_error(20) -> enotdir;
posix_error(21) -> eisdir;
posix_error(22) -> einval;
posix_error(23) -> enfile;
posix_error(24) -> emfile;
posix_error(25) -> enotty;
posix_error(26) -> etxtbsy;
posix_error(27) -> efbig;
posix_error(28) -> enospc;
posix_error(29) -> espipe;
posix_error(30) -> erofs;
posix_error(31) -> emlink;
posix_error(32) -> epipe;
posix_error(33) -> edom;
posix_error(34) -> erange;

posix_error(38) -> enosys;
posix_error(39) -> enotempty;
posix_error(61) -> enodata;
posix_error(88) -> enotsock;
posix_error(90) -> emsgsize;
posix_error(94) -> esocktnosupport;
posix_error(95) -> eopnotsupp;
posix_error(97) -> eafnosupport;
posix_error(98) -> eaddrinuse;
posix_error(104) -> econnreset;
posix_error(110) -> etimedout;
posix_error(111) -> econnrefused;

posix_error(N) -> {posix,N}.

error_posix(eperm) -> 1;
error_posix(enoent) -> 2;
error_posix(esrch) -> 3;
error_posix(eintr) -> 4;
error_posix(eio) -> 5;
error_posix(enxio) -> 6;
error_posix(e2big) -> 7;
error_posix(enoexec) -> 8;
error_posix(ebadf) -> 9;
error_posix(echild) -> 10;
error_posix(eagain) -> 11;
error_posix(enomem) -> 12;
error_posix(eacces) -> 13;
error_posix(efault) -> 14;
error_posix(enotblk) -> 15;
error_posix(ebusy) -> 16;
error_posix(eexist) -> 17;
error_posix(exdev) -> 18;
error_posix(enodev) -> 19;
error_posix(enotdir) -> 20;
error_posix(eisdir) -> 21;
error_posix(einval) -> 22;
error_posix(enfile) -> 23;
error_posix(emfile) -> 24;
error_posix(enotty) -> 25;
error_posix(etxtbsy) -> 26;
error_posix(efbig) -> 27;
error_posix(enospc) -> 28;
error_posix(espipe) -> 29;
error_posix(erofs) -> 30;
error_posix(emlink) -> 31;
error_posix(epipe) -> 32;
error_posix(edom) -> 33;
error_posix(erange) -> 34;

error_posix(enosys) -> 38;
error_posix(enotempty) -> 39;
error_posix(enodata) -> 61;
error_posix(enotsock) -> 88;
error_posix(emsgsize) -> 90;
error_posix(esocktnosupport) -> 94;
error_posix(eopnotsupp) -> 95;
error_posix(eafnosupport) -> 97;
error_posix(eaddrinuse) -> 98;
error_posix(econnreset) -> 104;
error_posix(etimedout) -> 110;
error_posix(econnrefused) -> 111.

attr_mask(Flds) ->
	attr_mask(Flds, 0).

attr_mask([], Mask) -> Mask;
attr_mask([mode|Flds], Mask) -> 	attr_mask(Flds, Mask bor 1);
attr_mask([nlink|Flds], Mask) -> 	attr_mask(Flds, Mask bor 2);
attr_mask([uid|Flds], Mask) -> 		attr_mask(Flds, Mask bor 4);
attr_mask([gid|Flds], Mask) -> 		attr_mask(Flds, Mask bor 8);
attr_mask([rdev|Flds], Mask) -> 	attr_mask(Flds, Mask bor 16);
attr_mask([atime|Flds], Mask) -> 	attr_mask(Flds, Mask bor 32);
attr_mask([mtime|Flds], Mask) -> 	attr_mask(Flds, Mask bor 64);
attr_mask([ctime|Flds], Mask) -> 	attr_mask(Flds, Mask bor 128);
attr_mask([ino|Flds], Mask) -> 		attr_mask(Flds, Mask bor 256);
attr_mask([size|Flds], Mask) -> 	attr_mask(Flds, Mask bor 512);
attr_mask([blocks|Flds], Mask) ->	attr_mask(Flds, Mask bor 1024);
attr_mask([btime|Flds], Mask) -> 	attr_mask(Flds, Mask bor 2048);
attr_mask([gen|Flds], Mask) -> 		attr_mask(Flds, Mask bor 4096);
attr_mask([data_version|Flds], Mask) -> attr_mask(Flds, Mask bor 8192);
attr_mask([basic|Flds], Mask) -> 	attr_mask(Flds, Mask bor 16#07ff);
attr_mask([all|Flds], Mask) -> 		attr_mask(Flds, Mask bor 16#3fff);
attr_mask(_, _) -> badarg.

fold_attrs(#rgetattr{valid=Valid}=Rec) ->
	Spec = [{1,mode,#rgetattr.mode},
			{2,nlink,#rgetattr.nlink},
			{4,uid,#rgetattr.uid},
			{8,gid,#rgetattr.gid},
			{16,rdev,#rgetattr.rdev},
			{32,atime_sec,#rgetattr.atime_sec},
			{32,atime_nsec,#rgetattr.atime_nsec},
			{64,mtime_sec,#rgetattr.mtime_sec},
			{64,mtime_nsec,#rgetattr.mtime_nsec},
			{128,ctime_sec,#rgetattr.ctime_sec},
			{128,ctime_nsec,#rgetattr.ctime_nsec},
			{256,ino,#rgetattr.ino},
			{512,size,#rgetattr.size},
			{1024,blocks,#rgetattr.blocks},
			{2048,btime_sec,#rgetattr.btime_sec},
			{2048,btime_nsec,#rgetattr.btime_nsec},
			{4096,gen,#rgetattr.gen},
			{8192,data_version,#rgetattr.data_version}],
	lists:foldl(fun({Mask,Fld,Idx}, Acc) when Valid band Mask =/= 0 ->
				[{Fld,element(Idx, Rec)}|Acc];
			(_, Acc) ->
				Acc
		end, [], Spec).

%% 9P2000:
bit_flags_old(Flags) ->

	%%
	%%	Plan 9 man page 5
	%%

	{Base,Bits} =
	lists:foldl(fun(rdonly, {none,Bits}) -> {0,Bits};
				   (wronly, {none,Bits}) -> {1,Bits};
				   (rdwr,	{none,Bits}) -> {2,Bits};
				   (trunc,	{Base,Bits}) -> {Base,Bits bor 16#10};
				   (rclose,	{Base,Bits}) -> {Base,Bits bor 16#40};
				   (excl,	{Base,Bits}) -> {Base,Bits};	%%XXX
				   (append,	{Base,Bits}) -> {Base,Bits};	%%XXX
				   (creat,	{Base,Bits}) -> {Base,Bits}		%%XXX

		end, {none,0}, Flags),

	Base bor Bits.

bit_flags(Flags) ->

	%%
	%% from fcntl.h:
	%%

	{Base,Bits} =
	lists:foldl(fun(rdonly, {none,Bits}) -> {0,Bits};
				   (wronly, {none,Bits}) -> {1,Bits};
				   (rdwr, 	{none,Bits}) -> {2,Bits};
				   (creat,  {Base,Bits}) -> {Base,Bits bor 8#100};
				   (excl, 	{Base,Bits}) -> {Base,Bits bor 8#200};
				   (noctty,	{Base,Bits}) -> {Base,Bits bor 8#400};
				   (trunc, 		{Base,Bits}) -> {Base,Bits bor 8#1000};
				   (append, 	{Base,Bits}) -> {Base,Bits bor 8#2000};
				   (nonblock, 	{Base,Bits}) -> {Base,Bits bor 8#4000};
				   (ndelay, 	{Base,Bits}) -> {Base,Bits bor 8#4000};
				   (sync, 	{Base,Bits}) -> {Base,Bits bor 8#10000};
				   (fsync, 	{Base,Bits}) -> {Base,Bits bor 8#20000};
				   (async, 	{Base,Bits}) -> {Base,Bits bor 8#40000}

		end, {none,0}, Flags),

	Base bor Bits.

dir_mode([]) -> ?DIR_MODE;
dir_mode([isvtx]) -> 8#1000 bor ?DIR_MODE.

attr_props(Props) ->
	attr_props(Props, #setattr{}, 0).

attr_props([], Rec, Valid) ->
	{Valid,Rec};
attr_props([{mode,Mode}|Props], Rec, Valid) when is_integer(Mode), Mode >= 0 ->
	attr_props(Props, Rec#setattr{mode=Mode}, Valid bor 1);
attr_props([{uid,Uid}|Props], Rec, Valid) when is_integer(Uid), Uid >= 0 ->
	attr_props(Props, Rec#setattr{uid=Uid}, Valid bor 2);
attr_props([{gid,Gid}|Props], Rec, Valid) when is_integer(Gid), Gid >= 0 ->
	attr_props(Props, Rec#setattr{gid=Gid}, Valid bor 4);
attr_props([{size,Size}|Props], Rec, Valid) when is_integer(Size), Size >= 0 ->
	attr_props(Props, Rec#setattr{size=Size}, Valid bor 8);
attr_props([atime|Props], Rec, Valid) ->
	attr_props(Props, Rec, Valid bor 16);
attr_props([mtime|Props], Rec, Valid) ->
	attr_props(Props, Rec, Valid bor 32);
attr_props([ctime|Props], Rec, Valid) ->
	attr_props(Props, Rec, Valid bor 64);
attr_props([{atime,NSec}|Props], Rec, Valid)
					when is_integer(NSec), NSec >= 0 ->
	ATime1 = NSec div 1000000000,
	ATime2 = NSec rem 1000000000,
	attr_props(Props, Rec#setattr{atime_sec=ATime1,
								  atime_nsec=ATime2}, Valid bor 16 bor 128);
attr_props([{mtime,NSec}|Props], Rec, Valid)
					when is_integer(NSec), NSec >= 0 ->
	MTime1 = NSec div 1000000000,
	MTime2 = NSec rem 1000000000,
	attr_props(Props, Rec#setattr{mtime_sec=MTime1,
								  mtime_nsec=MTime2}, Valid bor 32 bor 256);
attr_props(_, _, _) ->
	badarg.

decode_stat(<<Sz:16/little,Stat:(Sz)/binary>>) ->
	case Stat of
	<<Type:16/little,
	  Dev:32/little,
	  Qid:13/binary,
	  Mode:32/little,
	  Atime:32/little,
	  Mtime:32/little,
	  Length:64/little,
	  NSz:16/little,Name:(NSz)/binary,
	  USz:16/little,Uid:(USz)/binary,
	  GSz:16/little,Gid:(GSz)/binary,
	  MSz:16/little,Muid:(MSz)/binary>> ->

			#stat{ver =e,		%% 9P2000.e
				  type =Type,
				  dev =Dev,
				  qid =Qid,
				  mode =Mode,
				  atime =Atime,
				  mtime =Mtime,
				  length =Length,
				  name =Name,
				  uid =Uid,
				  gid =Gid,
				  muid =Muid};

	<<Type:16/little,
	  Dev:32/little,
	  Qid:13/binary,
	  Mode:32/little,
	  Atime:32/little,
	  Mtime:32/little,
	  Length:64/little,
	  NSz:16/little,Name:(NSz)/binary,
	  USz:16/little,Uid:(USz)/binary,
	  GSz:16/little,Gid:(GSz)/binary,
	  MSz:16/little,Muid:(MSz)/binary,
	  ESz:16/little,Ext:(ESz)/binary,
	  NumUid:32/little,
	  NumGid:32/little,
	  NumMuid:32/little>> ->

			#stat{ver =u,		%% 9P2000.u
				  type =Type,
				  dev =Dev,
				  qid =Qid,
				  mode =Mode,
				  atime =Atime,
				  mtime =Mtime,
				  length =Length,
				  name =Name,
				  uid =Uid,
				  gid =Gid,
				  muid =Muid,
				  ext =Ext,
				  num_uid =NumUid,
				  num_gid =NumGid,
				  num_muid =NumMuid}
	end.

%% @private
encode_stat(#stat{ver =e,		%% 9P2000.e
				  type =Type,
		  		  dev =Dev,
				  qid =Qid,
				  mode =Mode,
				  atime =Atime,
				  mtime =Mtime,
				  length =Length,
				  name =Name,
				  uid =Uid,
				  gid =Gid,
				  muid =Muid}) ->
	Stat = <<Type:16/little,
	  		 Dev:32/little,
	  		 Qid:13/binary,
			 Mode:32/little,
	  		 Atime:32/little,
	  		 Mtime:32/little,
	  		 Length:64/little,
	  		 (size(Name)):16/little,Name/binary,
	  		 (size(Uid)):16/little,Uid/binary,
	  		 (size(Gid)):16/little,Gid/binary,
	  		 (size(Muid)):16/little,Muid/binary>>,
	<<(size(Stat)):16/little,Stat/binary>>;

encode_stat(#stat{ver =u,		%% 9P2000.u
				  type =Type,
		  		  dev =Dev,
				  qid =Qid,
				  mode =Mode,
				  atime =Atime,
				  mtime =Mtime,
				  length =Length,
				  name =Name,
				  uid =Uid,
				  gid =Gid,
				  muid =Muid,
			  	  ext =Ext,
			  	  num_uid =NumUid,
			  	  num_gid =NumGid,
			  	  num_muid =NumMuid}) ->
	Stat = <<Type:16/little,
	  		 Dev:32/little,
	  		 Qid:13/binary,
			 Mode:32/little,
	  		 Atime:32/little,
	  		 Mtime:32/little,
	  		 Length:64/little,
	  		 (size(Name)):16/little,Name/binary,
	  		 (size(Uid)):16/little,Uid/binary,
	  		 (size(Gid)):16/little,Gid/binary,
	  		 (size(Muid)):16/little,Muid/binary,
		 	 (size(Ext)):16/little,Ext/binary,
			 NumUid:32/little,
			 NumGid:32/little,
			 NumMuid:32/little>>,
	<<(size(Stat)):16/little,Stat/binary>>.

timestamp() ->
	{Mega,Secs,_} = now(),
	Mega *1000000 +Secs.

%%
%% Code loading uses the 9p machinery. Thus we cannot rely on code:* when
%% introducing new transports and exports. Transport and export modules must be
%% 'embedded' or loaded before calling into 9p functions.
%%

%% @private
ensure_loaded(Mod) ->
	case erlang:module_loaded(Mod) of
	true ->
		ok;
	false ->
		File = list_to_atom(atom_to_list(Mod) ++ ".ling"),
		case binary:lookup_embedded(File) of
		false ->
			erlang:error({module_not_embedded,Mod});
		Bin ->
			{module,_} = erlang:load_module(Mod, Bin)
		end
	end.

%%EOF
