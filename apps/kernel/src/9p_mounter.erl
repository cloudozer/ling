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
%%% @docfile "doc/app/9p_mounter.edoc"
%%%

-module('9p_mounter').
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("9p.hrl").

-record(st, {expected =[],
			 subs =[],
			 mnt_map =[],
			 min_prio =10,
			 max_prio =11,
			 cwd =none}).

-type fid() :: integer().
-type mount() :: {binary(),binary()}.
-type option() :: '9p':option() | submerge | sync.
-type event() :: sync | {exists,binary()} | {gone,binary()}.

-type name() :: binary() | string().
-type spath() :: [binary()].
-type place() :: {pid(),fid(),[binary()]} | {local,integer(),integer()}.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).
-export([add_connection/3,add_connection/4]).

%% new API
-export([get_cwd/0,set_cwd/1]).
-export([canonicalise/1]).
-export([resolve_to_dir/1,resolve_to_file/1,resolve_to_both/1]).
-export([local_path_element/2]).

-export([subscribe/2]).
-export([show_map/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(LocalMap, Opts) ->
    {ok,_} = Res = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
	add_connection('9p_zero', '9p_zero', LocalMap, Opts),
	Res.

%% @doc Opens a new connection to a 9p server, attaches to remote names, and maps
%% them to the local hierarchy. @see add_connection/4.
-spec add_connection(module(), any(), [mount()]) -> {ok,pid()} | {error,_}.

add_connection(TransMod, TransConf, Mounts) ->
	add_connection(TransMod, TransConf, Mounts, []).

%% @doc Opens a new connection to a 9p server, attaches to remote names, and maps
%% them to the local hierarchy.
%% Options:
%% - sync - wait for all mounting operation to complete;
%% - submerge - add mounts with the lowest priority;
%% - any '9p':start_link/3 option.
-spec add_connection(module(), any(), [mount()], [option()]) -> {ok,pid()} |
{error,_}.

add_connection(TransMod, TransConf, Mounts, Opts)
		when is_atom(TransMod), is_list(Mounts), is_list(Opts) ->
	case check_opts(Opts) of
	{ConnOpts,Submerge,Sync} ->
		%% code loading uses 9p_mounter, preload the transport module and other
		%% modules the transport module references
		preload_modules(TransMod),

		if Submerge ->
			gen_server:call(?SERVER,
				{add_connection_submerge,TransMod,TransConf,Mounts,ConnOpts});
		true ->
			gen_server:call(?SERVER,
				{add_connection,TransMod,TransConf,Mounts,ConnOpts})
		end,
		if Sync ->
			{ok,Ref} = subscribe(sync, self()),
			receive {Ref,sync} -> ok end;
		true ->
			ok
		end;
	Error ->
		{error,Error}
	end;
add_connection(_, _, _, _) ->
	{error,badarg}.

preload_modules(TransMod) ->
	'9p':ensure_loaded(TransMod),
	preload_modules_extra(TransMod).

preload_modules_extra('9p_tcp') ->
	'9p':ensure_loaded(gen_tcp),
	'9p':ensure_loaded(inet_tcp),
	'9p':ensure_loaded(inet_gethost_native);
preload_modules_extra(_) ->
	true.

%% @doc Sets the current working directory.
-spec set_cwd(spath()) -> ok | {error,_}.

set_cwd(SPath) when is_list(SPath) ->
	gen_server:call(?SERVER, {set_cwd,SPath});
set_cwd(_) ->
	{error,badarg}.

%% @doc Get the current working directory.
-spec get_cwd() -> {ok,spath()} | none.

get_cwd() ->
	gen_server:call(?SERVER, get_cwd).

%% @doc Converts absolute or relative paths into a splitted path, which is
%% absolute and does not contain ".." elements.
-spec canonicalise(name()) -> {ok,spath()} | {error,_}.

canonicalise(Path) when is_binary(Path) ->
	gen_server:call(?SERVER, {canonicalise,Path});
canonicalise(Path) when is_list(Path) ->
	case catch list_to_binary(Path) of
	{'EXIT',_} ->
		{error,badarg};
	PathBin ->
		gen_server:call(?SERVER, {canonicalise,PathBin})
	end;
canonicalise(_) ->
	{error,badarg}.

%% @doc Queries the mounting map and returns the walking instructions to get to
%% the directory.
-spec resolve_to_dir(spath()) -> {walk,[place()]} | {error,_}.

resolve_to_dir(SPath) when is_list(SPath) ->
	gen_server:call(?SERVER, {resolve_to_dir,SPath});
resolve_to_dir(_) ->
	{error,badarg}.

%% @doc Queries the mounting map and returns the walking instructions to get to
%% the named filesystem object. Excludes 'local' fids.
-spec resolve_to_both(spath()) -> {walk,[place()]} | {error,_}.

resolve_to_both(SPath) when is_list(SPath) ->
	gen_server:call(?SERVER, {resolve_to_both,SPath});
resolve_to_both(_) ->
	{error,badarg}.

%% @doc Queries the mounting map and return the walking instructions to reach
%% the file referenced by SPath.
-spec resolve_to_file(spath()) -> {walk,[place()]} | {error,_}.

resolve_to_file(SPath) when is_list(SPath) ->
	gen_server:call(?SERVER, {resolve_to_file,SPath});
resolve_to_file(_) ->
	{error,badarg}.

%% @doc Retrieves the Nth local path element of the mapping with the priority
%% Prio.
-spec local_path_element(integer(), integer()) -> {ok,binary()} | {error,_}.

local_path_element(Prio, N) when is_integer(Prio), is_integer(N) ->
	gen_server:call(?SERVER, {local_path_element,Prio,N});
local_path_element(_, _) ->
	{error,badarg}.

%% @doc Subscribes to a 9p-related event. The returned reference identifies the
%% subscription. The Pid is guaranteed to receive exactly one {Ref,Event}
%% message when the event happens or a condition is met.
%% Events:
%% 	- sync - all mounting operations complete;
%%  - {exists,Path} - the path (now) exists;
%%  - {gone,Path} - the path does not exist (now).
-spec subscribe(event(), pid()) -> {ok,reference()} | {error,any}.

subscribe(sync =Event, Pid) when is_pid(Pid) ->
	gen_server:call(?SERVER, {subscribe,Event,Pid});
subscribe({exists,Path}, Pid) when is_binary(Path), is_pid(Pid) ->
	{error,not_implemented};
subscribe({gone,Path}, Pid) when is_binary(Path), is_pid(Pid) ->
	{error,not_implemented};
subscribe(_, _) ->
	{error,badarg}.

%% @doc Prints out the current mounting map. For debugging only.
show_map() ->
	gen_server:call(?SERVER, show_map).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @private
init(_Args) ->
	process_flag(trap_exit, true),
    {ok,#st{}}.

%% @private
handle_call({add_connection,TransMod,TransConf,Mounts,ConnOpts},
		_From, #st{expected =Expected0} =St0) ->
	AttachTo = [AName || {_,AName} <- Mounts],
	{ok,ConnPid} = '9p':start_link(TransMod, TransConf, AttachTo, ConnOpts),

	%% Adds a priority values. It is not enough to rely on ordering of
	%% ConnPid because mounts on the same connection still can shadow one
	%% another.

	{Expected,St1} = lists:mapfoldl(fun({LocPath,AName}, St) ->
		{Prio,St1} = pick_max_prio(St),
		{{Prio,ConnPid,AName,split(LocPath)},St1}
	end, St0, Mounts),
	
	{reply,{ok,ConnPid},St1#st{expected =Expected0 ++ Expected}};

handle_call({add_connection_submerge,TransMod,TransConf,Mounts,ConnOpts},
		_From, #st{expected =Expected0} =St0) ->
	AttachTo = [AName || {_,AName} <- Mounts],
	{ok,ConnPid} = '9p':start_link(TransMod, TransConf, AttachTo, ConnOpts),

	%% @see comment above

	{Expected,St1} = lists:mapfoldr(fun({LocPath,AName}, St) ->	%%NB: mapfoldr
		{Prio,St1} = pick_min_prio(St),
		{{Prio,ConnPid,AName,split(LocPath)},St1}
	end, St0, Mounts),
	
	{reply,{ok,ConnPid},St1#st{expected =Expected0 ++ Expected}};

handle_call({set_cwd,SPath}, _From, St) ->
	{reply,ok,St#st{cwd =SPath}};

handle_call(get_cwd, _From, #st{cwd =none} =St) ->
	{reply,{error,enoent},St};

handle_call(get_cwd, _From, #st{cwd =SPath} =St) ->
	{reply,{ok,SPath},St};

handle_call({canonicalise,Path}, _From, #st{cwd =Cwd} =St) ->
	Reply = case canonical_path(Path, Cwd) of
	Error when is_atom(Error) ->
		{error,Error};
	SPath ->
		{ok,SPath}
	end,
	{reply,Reply,St};

handle_call({resolve_to_dir,SPath}, _From, #st{mnt_map =MntMap} =St) ->
	Reply = case get_places(SPath, MntMap, dir) of
	[] ->
		{error,enoent};
	Places ->
		{walk,Places}
	end,
	{reply,Reply,St};

handle_call({resolve_to_file,SPath}, _From, #st{mnt_map =MntMap} =St) ->
	Reply = case get_places(SPath, MntMap, file) of
	[] ->
		{error,enoent};
	Places ->
		{walk,Places}
	end,
	{reply,Reply,St};

handle_call({resolve_to_both,SPath}, _From, #st{mnt_map =MntMap} =St) ->
	Reply = case get_places(SPath, MntMap, any) of
	[] ->
		{error,enoent};
	Places ->
		{walk,Places}
	end,
	{reply,Reply,St};

handle_call({local_path_element,Prio,N}, _From, #st{mnt_map =MntMap} =St) ->
	Reply = case lists:keyfind(Prio, 1, MntMap) of
	{_,Prefix,_,_,_} when length(Prefix) >= N ->
		{ok,lists:nth(N, Prefix)};
	_ ->
		{error,enoent}
	end,
	{reply,Reply,St};

%%------------------------------------------------------------------------------

handle_call({subscribe,Event,Pid}, _From, St) ->
	Ref = make_ref(),
	St1 = subscribe_event(Event, Ref, Pid, St),
	{reply,{ok,Ref},St1};

%%DBG
handle_call(show_map, _From, St) ->
	io:format("--------------------------------------------------------------------~n", []),
	io:format("Prio | Point                              | Conn       |  Fid | Type~n", []),
	io:format("--------------------------------------------------------------------~n", []),
	lists:foreach(fun({Prio,LocSplit,ConnPid,Fid,Type}) ->	
		io:format("~4w | ~-34s | ~10w | ~4w | ~w~n",
						[Prio,unsplit(LocSplit),ConnPid,Fid,Type])
	end, St#st.mnt_map),
	io:format("--------------------------------------------------------------------~n", []),
	{reply,ok,St}.

%% @private
handle_cast({'9p_attached',ConnPid,Fid,AName,Type}, St) ->

	%%TODO: generate event when new paths appear

	{Mounted,Expected} = lists:partition(fun({_,ConnPid1,AName1,_}) ->
				(ConnPid1 =:= ConnPid) and (AName1 =:= AName)
			end, St#st.expected),

	%% there must be only one
	[{Prio,_,_,LocSplit}] = Mounted,

	%% sort to restore mounting priorities
	M = {Prio,LocSplit,ConnPid,Fid,Type},
	MntMap = lists:keysort(1, [M] ++ St#st.mnt_map),
	%?dbg("9p: mounter: new map: ~p~n", [MntMap]),

	St1 = if Expected =:= [] ->
		dispatch_event(sync, St#st{expected =[],
								   mnt_map =MntMap});
		true ->
			St#st{expected =Expected,
				  mnt_map =MntMap}
	end,

    {noreply, St1}.

%% @private
handle_info({'EXIT',ConnPid,_Reason}, St) ->
	?dbg("9p: mounter: connection ~w closed: ~p~n", [ConnPid,_Reason]),

	%%TODO: generate event when paths go away

	%% ConnPid may appear in the mounting map
	MntMap = [M || {_,_,ConnPid1,_,_} =M <- St#st.mnt_map,
					ConnPid1 =/= ConnPid],
	{noreply,St#st{mnt_map =MntMap}}.

terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

pick_min_prio(#st{min_prio =MinPrio} =St) ->
	{MinPrio,St#st{min_prio =MinPrio -1}}.

pick_max_prio(#st{max_prio =MaxPrio} =St) ->
	{MaxPrio,St#st{max_prio =MaxPrio +1}}.

check_opts(Opts) ->
	check_opts(Opts, false, false, []).

check_opts([], Sync, Submerge, ConnOpts) ->
	{ConnOpts,Submerge,Sync};
check_opts([sync|Opts], _, Submerge, Acc) ->
	check_opts(Opts, true, Submerge, Acc);
check_opts([submerge|Opts], Sync, _, Acc) ->
	check_opts(Opts, Sync, true, Acc);
check_opts([Opt|Opts], Sync, Submerge, Acc) ->
	check_opts(Opts, Sync, Submerge, [Opt|Acc]).

get_places(SPath, MntMap, ReqType) ->
	N = length(SPath),
	lists:foldl(fun({_Prio,Prefix,_ConnPid,_Fid,Type}, Places)
				when length(Prefix) =:= N,
					Type =/= ReqType, ReqType =/= any ->
			Places;

		({Prio,Prefix,_ConnPid,_Fid,_Type}, Places)
				when length(Prefix) > N ->
			case lists:prefix(SPath, Prefix) of
			true ->
				Place = {local,Prio,N},
				[Place|Places];
			false ->
				Places
			end;

		({_Prio,Prefix,ConnPid,Fid,_Type}, Places) ->
			K = length(Prefix),
			case lists:prefix(Prefix, SPath) of
			true ->
				WalkTo = lists:nthtail(K, SPath),
				Place = {ConnPid,Fid,WalkTo},
				[Place|Places];
			false ->
				Places
			end;
		(_, Places) ->
			Places
		end, [], MntMap).

split(Path) ->
	[El || El <- binary:split(Path, <<"/">>, [global]),
			El =/= <<>>, El =/= <<".">>].

unsplit(Prefix) ->
	list_to_binary([[$/,Part] || Part <- Prefix]).

is_absolute(Path) ->
	binary:first(Path) =:= $/.

canonical_path(<<>>, _) ->
	enoent;
canonical_path(Path, Cwd) ->
	case is_absolute(Path) of
	true ->
		fix_ups(split(Path));
	false when Cwd =:= none ->
		enoent;
	false ->
		Prefix = case Cwd of
			{P,_} -> P; P -> P end,
		fix_ups(Prefix ++ split(Path))
	end.

fix_ups(SPath) ->
	fix_ups(lists:reverse(SPath), []).

fix_ups([], Acc) ->
	Acc;
fix_ups([<<"..">>,_EatMe|Htaps], Acc) ->
	fix_ups(Htaps, Acc);
fix_ups([Elem|Htaps], Acc) ->
	fix_ups(Htaps, [Elem|Acc]).

%%-------- events --------------------------------------------------------------

subscribe_event(sync =Event, Ref, Pid, #st{expected =[]} =St) ->
	Pid ! {Ref,Event},
	St;
subscribe_event(Event, Ref, Pid, #st{subs =Subs} =St) ->
	St#st{subs =[{Event,Ref,Pid}|Subs]}.

dispatch_event(Event, #st{subs =Subs} =St) ->
	{Fired,Waiting} = lists:partition(fun({Event1,_,_}) ->
				Event1 =:= Event
		end, Subs),

	lists:foreach(fun({_,Ref,Pid}) ->
		Pid ! {Ref,Event}
	end, Fired),

	St#st{subs =Waiting}.

%%EOF
