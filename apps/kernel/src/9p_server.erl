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
%%% @docfile "doc/app/9p_server.edoc"
%%%
-module('9p_server').

-export([start_link/0]).
-export([add_listener/3,add_listener/4,remove_listener/1]).
-export([add_export/3,remove_export/1]).
-export([trace_messages/1,trace_messages/0]).

-include("9p.hrl").
-include("9p_info.hrl").
-include("9p_auth.hrl").

%% Default number of processes per listener
-define(DEFAULT_NUM_ACCEPTORS, 4).

-type name() :: binary().
-type mod_conf() :: any().

%% a main loop state
-record(st, {listeners =[],
			 exports =[],
		 	 sessions =[],
			 trace_messages =false}).

%% a listener loop state
-record(ls, {lsock,
			 trans_mod,
			 trans_conf}).

%% an active fid info
-record(fid, {type,		% root, auth, dir, file
			  path,
			  exp_mod,
			  mod_conf,
			  name,
			  alias,
			  cached}).

%%-------- public interface ---------------------------------------------------

%% @doc Starts the '9p_server' and sets up listeners.
-spec start_link() -> {ok,pid()}.

start_link() ->

	% Start '9p_server'
	Pid  = spawn_link(fun() -> loop(#st{}) end),
	register(?MODULE, Pid),

	% Add local listener. The listener is needed for code loading and thus must
	% be started as soon as possible. A remote listener is added when the
	% external network interface is configured.
	%
	%add_listener(local, '9p_tcp', {{127,0,0,1},564}),
	add_listener(local, '9p_zero', '9p_zero'),

	% Named blob buckets are exported automatically
	Buckets = binary:embedded_buckets(),
	lists:foreach(fun(Buck) ->
		Name = list_to_binary(atom_to_list(Buck)),
		ModConf = Buck,
		add_export(Name, embedded_export, ModConf)
	end, Buckets),

	{ok,Pid}.

%% @doc Add a 9p server listener. `TransMod' and `TranConf' specify the transport
%% endpoint. `Id' is an arbitrary listener identifier.

-spec add_listener(Id, TransMod, TransConf) -> ok | {error,Error} when
	Id :: any(),
	TransMod :: module(),
	TransConf :: mod_conf(),
	Error :: any().

add_listener(Id, TransMod, TransConf) ->
	add_listener(Id, TransMod, TransConf, ?DEFAULT_NUM_ACCEPTORS).

%% @doc Add a 9p server listener. `TransMod' and `TranConf' specify the
%% transport endpoint. `Id' is an arbitrary listener identifier. `NumAcc' is
%% the number of acceptor processes simultaneously waiting for incoming
%% connections on the endpoint.

-spec add_listener(Id, TransMod, TransConf, NumAcc) ->
		ok | {error,Error} when
	Id :: any(),
	TransMod :: module(),
	TransConf :: mod_conf(),
	NumAcc :: integer(),
	Error :: any().

add_listener(Id, TransMod, TransConf, NumAcc)
		when is_atom(TransMod), is_integer(NumAcc) ->
	%% code loading uses 9p_server, preload the transport module
	'9p':ensure_loaded(TransMod),
	request({add_listener,Id,TransMod,TransConf,NumAcc}).

%% @doc Remove the listener identified by `Id'.

-spec remove_listener(Id) -> ok when
	Id :: any().

remove_listener(Id) ->
	request({remove_listener,Id}).

%% @doc Adds an exported directory to the 9p server. `Name' is the name of the
%% directory without the leading slash. For example, a directory named
%% `<<"proc">>' is exported as `/proc' by the server.

-spec add_export(Name, ExpMod, ModConf) -> ok | {error,Error} when
	Name :: name(),
	ExpMod :: module(),
	ModConf :: mod_conf(),
	Error :: any().

add_export(Name, ExpMod, ModConf) when is_binary(Name), is_atom(ExpMod) ->
	%% code loading uses 9p_server, preload the export module
	'9p':ensure_loaded(ExpMod),
	request({add_export,Name,ExpMod,ModConf}).

%% @doc Stops exporting the directory.

-spec remove_export(Name) -> ok when
	Name :: name().

remove_export(Name) when is_binary(Name) ->
	request({remove_export,Name}).

%% @doc Starts/stops message tracing. The flag affect only new connections.
-spec trace_messages(boolean()) -> boolean().

trace_messages(Flag) ->
	request({trace_messages,Flag}).

%% @doc Retrieve the current trace messages flag.
-spec trace_messages() -> boolean().

trace_messages() ->
	request(trace_messages).

%%-------- private interface --------------------------------------------------

find_export([]) ->
	#fid{type =root,path =[]};
find_export([_] =Path) ->
	request({find_export,Path});
find_export([_,_] =Path) ->
	request({find_export,Path});
find_export(_) ->
	invalid.

list_exports() ->
	request(list_exports).

%%-------- main loop ----------------------------------------------------------

loop(St) ->
	receive
	{request,From,Req} ->
		case handle(From, Req, St) of
		{reply,Reply,St1} ->
			From ! {reply,Reply},
			loop(expire_dropped_sessions(St1))
		end
	end.

expire_dropped_sessions(St) ->
	TS = '9p':timestamp(),
	receive
	{session_state,SK,_St,ExpireOn} when ExpireOn >= TS ->
		Ss =lists:keydelete(SK, 1, St#st.sessions),
		expire_dropped_sessions(St#st{sessions =Ss})
	after 0 ->
		St
	end.

handle(_From,
		{add_listener,Id,TransMod,TransConf,NumAcc},
	   	#st{listeners =Ls} =St) ->
	case lists:keymember(Id, 1, Ls) of
	false ->
		case start_listener(NumAcc, TransMod, TransConf) of
		{ok,Pid} ->
			L = {Id,Pid},
			St1 = St#st{listeners =[L|Ls]},
			{reply,ok,St1};
		Error ->
			{reply,Error,St}
		end;
	true ->
		{reply,{error,already_exists},St}
	end;

handle(_From, {remove_listener,Id}, #st{listeners =Ls} =St) ->
	case lists:keytake(1, Id, Ls) of
	{value,{_,Pid},Ls1} ->
		exit(Pid),
		{reply,ok,St#st{listeners =Ls1}};
	_ ->
		{reply,ok,St}
	end;

handle(_From, {add_export,Name,ExpMod,ModConf}, #st{exports =Es} =St) ->
	case lists:keymember(Name, 1, Es) of
	false ->
		E = {Name,ExpMod,ModConf},
		St1 = St#st{exports =[E|Es]},
		{reply,ok,St1};
	true ->
		{reply,{error,already_exists},St}
	end;

handle(_From, {remove_export,Name}, #st{exports =Es} =St) ->
	St1 = St#st{exports =lists:keydelete(Name, 1, Es)},
	{reply,ok,St1};

handle(_From, {find_export,[Name] =Path}, #st{exports =Es} =St) ->
	case lists:keyfind(Name, 1, Es) of
	false ->
		{reply,not_found,St};
	{_,ExpMod,ExpConf} ->
		{reply,#fid{type =dir,
					path =Path,
					exp_mod =ExpMod,
					mod_conf =ExpConf,
					name =Name},St}
	end;
handle(_From, {find_export,[Name,File] =Path}, #st{exports =Es} =St) ->
	case lists:keyfind(Name, 1, Es) of
	false ->
		{reply,not_found,St};
	{_,ExpMod,ExpConf} ->
		{reply,#fid{type =file,
					path =Path,
					exp_mod =ExpMod,
					mod_conf =ExpConf,
					name =File},St}
	end;

handle(_From, list_exports, #st{exports =Es} =St) ->
	{reply,[Name || {Name,_,_} <- Es],St};

handle(_From, {trace_messages,Flag}, #st{trace_messages =OldFlag} =St) ->
	{reply,OldFlag,St#st{trace_messages =Flag}};

handle(_From, trace_messages, #st{trace_messages =OldFlag} =St) ->
	{reply,OldFlag,St};

handle(_From, {new_session,Key,AbortFun}, #st{sessions =Ss} =St) ->
	St1 = St#st{sessions =[{Key,AbortFun}|Ss]},
	{reply,ok,St1};

handle(_From, {drop_session,Key}, #st{sessions =Ss} =St) ->
	St1 = St#st{sessions =lists:keydelete(Key, 1, Ss)},
	{reply,ok,St1};

handle(_From, {recover_session,Key}, #st{sessions =Ss} =St) ->
	case lists:keytake(Key, 1, Ss) of
	{value,{_,AbortFun},Ss1} ->

		%% Forcibly close the old transport connection to ensure receive does
		%% not block forever. 
		AbortFun(),

		receive
		{session_state,Key,RecState,_} ->
			{reply,{recovered,RecState},St#st{sessions =Ss1}}
		end;
	_ ->
		{reply,false,St}	%% not found
	end.

%%-------- helpers ------------------------------------------------------------

request(Req) ->
	case whereis(?MODULE) of
	undefined ->
		{error,not_started};
	Pid ->
		Pid ! {request,self(),Req},
		receive
		{reply,Reply} ->
			Reply
		end
	end.

%%-------- listener -----------------------------------------------------------

start_listener(NumAcc, TransMod, TransConf) ->
	case TransMod:listen(TransConf) of
	{ok,LSock} ->
		Pid = spawn(fun() ->

				%% Trap acceptor exits and respawn them
				process_flag(trap_exit, true),
				
				%% Spawn NumAcc acceptor processes
				add_acceptors(LSock, NumAcc, TransMod, TransConf),

				listener_loop(#ls{lsock =LSock,
								  trans_mod =TransMod,
								  trans_conf =TransConf})
		end),
		{ok,Pid};
	Error ->
		Error
	end.

listener_loop(#ls{lsock =LSock,
				  trans_mod =TransMod,
				  trans_conf =TransConf} =St) ->
	receive
	{'EXIT',_Pid,{no_respawn,_}} ->
		listener_loop(St);	

	{'EXIT',_Pid,_Reason} ->
		?dbg("9p acceptor exits (~p) and is being respawned~n", [_Reason]),
		add_acceptors(LSock, 1, TransMod, TransConf),
		listener_loop(St);

	_Msg ->
		?dbg("listener_loop: msg ~p~n", [_Msg]),
		listener_loop(St)
	end.

add_acceptors(_, 0, _, _) ->
	ok;
add_acceptors(LSock, NumAcc, TransMod, TransConf) ->
	start_acceptor(LSock, TransMod, TransConf),
	add_acceptors(LSock, NumAcc -1, TransMod, TransConf).

%%-------- acceptor -----------------------------------------------------------

start_acceptor(LSock, TransMod, TransConf) ->
	Pid = spawn_link(fun() ->
		case TransMod:accept(LSock, TransConf) of
		{ok,NewSock} ->
			Trace = '9p_server':trace_messages(),
			case TransMod:is_local(TransConf) of
			false ->
				acceptor_loop(#ac{sock =NewSock,
							  	  trans_mod =TransMod,
							  	  trans_conf =TransConf,
								  trace =Trace,
								  started ='9p':timestamp(),
							  	  is_local =false});
			true ->
				acceptor_loop(#ac{sock =NewSock,
								  trans_mod =TransMod,
								  trans_conf =TransConf,
								  trace =Trace,
								  started ='9p':timestamp(),
								  is_local =true,
								  basic_auth =true,
								  peer_node =erlang:node(),
							   	  peer_group =erlang:node_group()})
			end;
		{error,closed} ->
			erlang:error(no_respawn)
		end
	end),
	{ok,Pid}.

receive_message(Sock, TransMod, TransConf) ->
	case TransMod:recv(Sock, TransConf) of
	{ok,Packet} when is_binary(Packet) ->
		{ok,'9p':decode(Packet)};
	Other ->
		Other
	end.

acceptor_loop(#ac{sock =Sock,
				  session_key =SK,
				  trans_mod =TransMod,
				  trans_conf =TransConf} =St) ->
	case receive_message(Sock, TransMod, TransConf) of
	{ok,Decoded} ->
		NewState =
		try
			if St#ac.trace -> io:format("[~w] ---> ~P~n", [St#ac.ver,Decoded,5]);
				true -> ok end,

			%erlang:display({'--->',Decoded}),
			{Reply,St1} = handle_msg(Decoded, St),
			%erlang:display({'<---',Reply}),

			if St#ac.trace -> io:format("[~w] <--- ~P~n", [St#ac.ver,Reply,5]);
				true -> ok end,

			TransMod:send(Sock, Reply, TransConf),
			St1

		catch error:Reason ->
			erlang:display({'*** 9p exception',Reason,erlang:get_stacktrace()}),
			%?dbg("*** 9p exception: ~p~n~p~n",
			%			[Reason,erlang:get_stacktrace()]),
			Tag = element(2, Decoded),
			{Error,_} = protocol_error(Tag, St),
			TransMod:send(Sock, Error, TransConf),

			if SK =/= undefined ->

				%%
				%% The session that was aborted due to exception cannot be restored.
				%% Make 9p server forget about this session.
				%%

				request({drop_session,SK});
			true ->
				ok
			end,

			erlang:error(Reason) %% rethrow
		end,

		%% This cannot be put inside try-catch; disables tail recursion
		acceptor_loop(NewState);

	_Error ->

		%%
		%% The transport connection is either lost or forcibly closed the 9p
		%% server. The latter happens if 9p server calls AbortFun.
		%%

		if SK =/= undefined ->

			%% The dropped sessions can not be recovered after 30s
			ExpireOn = '9p':timestamp() +30,
			'9p_server' ! {session_state,SK,St,ExpireOn};

		true ->
			ok
		end
	end.

%% Tsession tag[2] key[8]
handle_msg({tsession,?NOTAG,Key}, #ac{sock =Sock,ver =e} =St) ->
	case request({recover_session,Key}) of
	{recovered,RecState} ->
		{{rsession,?NOTAG},RecState#ac{sock =Sock}};
	_ ->
		{{rerror,?NOTAG,<<"cannot recover">>},St}
	end;

%% eoxmount is a wrapper over a mount command that adds MUNGE authentication.
%% eoxmount pre-authenticates a socket and passes it mount as a command-line
%% argument. This results in a duplicate version negotiation.
%%
%% Luckily, it works. The only fid that is active between the first and the
%% second version exchange is afid. It gets erased by the redundant version
%% message. The basic_auth flag is preserved however and subsequent attaches
%% proceed without an error.

%% Tversion tag[2] msize[4] version[s]
handle_msg({tversion,?NOTAG,MSize,<<"9P2000.e">> =Version}, St) ->
	{{rversion,?NOTAG,MSize,Version},St#ac{ver =e,
										   msize =MSize,
										   fids =dict:new()}};
handle_msg({tversion,?NOTAG,MSize,<<"9P2000.u">> =Version},
   		#ac{is_local =false} =St) ->
	{{rversion,?NOTAG,MSize,Version},St#ac{ver =u,
										   msize =MSize,
										   fids =dict:new()}};
handle_msg({tversion,?NOTAG,MSize,_}, St) ->
	{{rversion,?NOTAG,MSize,<<"unknown">>},St};

%% Any other message must have a valid Tag
handle_msg(Msg, St) when element(2, Msg) =:= ?NOTAG ->
	return_error(?NOTAG, <<"invalid tag">>, einval, St);

%% Tauth tag[2] afid[4] uname[s] aname[s]
handle_msg({tauth,Tag,_,_,_}, #ac{is_local =true} =St) ->
	return_error(Tag,
		<<"Local connections are auto-authenticated">>, eperm, St);

handle_msg({tauth,Tag,AFid,UName,AName}, #ac{fids =Fids} =St) ->
	true = AFid =/= ?NOFID,
	false = dict:is_key(AFid, Fids),
	case valid_is_aname(AName) of
	true ->
		Finfo = #fid{type =auth},
		{{rauth,Tag,auth_qid()},St#ac{fids =dict:store(AFid, Finfo, Fids),
									  auth_path =AName,
									  auth_user =UName}};
	false ->
		invalid_aname(Tag, St)
	end;

%% 9P2000.u:
%% Tauth tag[2] afid[4] uname[s] aname[s] n_uname[4]
handle_msg({tauth,Tag,AFid,UName,AName,Uid}, #ac{ver =u,fids =Fids} =St) ->
	true = AFid =/= ?NOFID,
	false = dict:is_key(AFid, Fids),
	case valid_is_aname(AName) of
	true ->
		Finfo = #fid{type =auth},
		{{rauth,Tag,auth_qid()},St#ac{fids =dict:store(AFid, Finfo, Fids),
									  auth_path =AName,
									  auth_user =UName,
									  auth_uid =Uid}};
	false ->
		invalid_aname(Tag, St)
	end;

%% Tflush tag[2] oldtag[2]
handle_msg({tflush,Tag,_OldTag}, St) ->
	not_supported(Tag, St); %% no outstanding tags

%% Tclunk tag[2] fid[4]
handle_msg({tclunk,Tag,Fid}, #ac{fids =Fids} =St) ->
	St1 = St#ac{fids = dict:erase(Fid, Fids)},
	{{rclunk,Tag},St1};

%% Tattach tag[2] fid[4] afid[4] uname[s] aname[s]
handle_msg({tattach,Tag,Fid,AFid,UName,AName}, St) ->
	attach(Tag, Fid, AFid, UName, AName, undefined, St);

%% 9P2000.u:
%% Tattach tag[2] fid[4] afid[4] uname[s] aname[s] n_uname[4]
handle_msg({tattach,Tag,Fid,AFid,UName,AName,Uid}, St) ->
	attach(Tag, Fid, AFid, UName, AName, Uid, St);

%% Twalk tag[2] fid[4] newfid[4] nwname[2] nwname*(wname[s])
handle_msg({twalk,Tag,_Fid,_NewFid,_Names}, #ac{basic_auth =false} =St) ->
	not_authorized(Tag, St);

handle_msg({twalk,Tag,Fid,Fid,[]}, St) ->
	{{rwalk,Tag,[]},St};

handle_msg({twalk,Tag,Fid,NewFid,[]}, #ac{fids =Fids} =St) ->
	Finfo = dict:fetch(Fid, Fids),
	false = dict:is_key(NewFid, Fids),
	St1 = St#ac{fids =dict:store(NewFid, Finfo, Fids)},
	{{rwalk,Tag,[]},St1};

handle_msg({twalk,Tag,Fid,NewFid,Names}, #ac{fids =Fids} =St) ->
	true = (Fid =:= NewFid) orelse (not dict:is_key(NewFid, Fids)),
	case dict:fetch(Fid, Fids) of
	#fid{type =auth} ->
		invalid_fid(Tag, St);
	#fid{path =Path} ->
		walk(Tag, NewFid, Path, Names, St)
	end;

%% Topen tag[2] fid[4] mode[1]
handle_msg({topen,Tag,Fid,Flags}, #ac{fids =Fids} =St) ->
	Truncate = lists:member(trunc, Flags),
	Finfo = dict:fetch(Fid, Fids),
	if Truncate ->
		#fid{exp_mod =ExpMod,mod_conf =ModConf,alias =Alias} =Finfo,
		case erlang:function_exported(ExpMod, truncate, 4) of
		true ->
			ExpMod:truncate(Alias, 0, St, ModConf);
		false ->
			ok %% ignore
		end;
	true ->
		ok
	end,
	Qid = dummy_qid(Finfo),
	{{ropen,Tag,Qid,St#ac.msize -?IOHDRSZ},St};

%% Tlcreate tag[2] fid[4] name[s] perm[4] mode[1]
handle_msg({tcreate,Tag,_Fid,_Name,Perm,_Mode}, St) when Perm band ?DMDIR =/= 0 ->
	%% cannot create directories
	not_supported(Tag, St);
handle_msg({tcreate,Tag,Fid,Name,_Perm,_Mode}, St) ->
	create(Tag, Fid, Name, St);

%% Tlcreate tag[2] fid[4] name[s] perm[4] mode[1] extension[s]
handle_msg({tcreate,Tag,Fid,Name,_Perm,_Mode,Ext}, #ac{ver =u} =St) ->
	true = Ext =:= <<>>,
	create(Tag, Fid, Name, St);

%% Tread tag[2] fid[4] offset[8] count[4]
handle_msg({tread,Tag,Fid,Offset,N,_Flag}, #ac{fids =Fids} =St) ->
	case dict:fetch(Fid, Fids) of
	#fid{cached =Cached} when Cached =/= undefined,
							  Offset > 0 ->
		%% use cache
		case cache_slice(Cached, Offset, N) of
		Data when is_binary(Data) ->
			{{rread,Tag,Data},St};
		misaligned ->
			misaligned_offset(Tag, St)
		end;

	#fid{type =root} =Finfo ->
		Stats = [encoded_stat(#fid{type =dir,path =[Name]}, St)
					|| Name <- list_exports()],
		case cache_slice(Stats, Offset, N) of
		Data when is_binary(Data) ->
			St1 = St#ac{fids = dict:store(Fid, Finfo#fid{cached =Stats}, Fids)},
			{{rread,Tag,Data},St1};
		misaligned ->
			misaligned_offset(Tag, St)
		end;

	#fid{type =dir,path =DirPath,exp_mod =ExpMod,mod_conf =ModConf} =Finfo ->
		Stats = [encoded_stat(#fid{type =file,path =DirPath ++ [Name],
										exp_mod =ExpMod,
										mod_conf =ModConf,
										name =Name,
										alias =Alias}, St)
					|| {Name,Alias} <- ExpMod:list_dir(St, ModConf)],
		case cache_slice(Stats, Offset, N) of
		Data when is_binary(Data) ->
			St1 = St#ac{fids = dict:store(Fid, Finfo#fid{cached =Stats}, Fids)},
			{{rread,Tag,Data},St1};
		misaligned ->
			misaligned_offset(Tag, St)
		end;

	#fid{type =file,exp_mod =ExpMod,mod_conf =ModConf,name =File} =Finfo ->
		case erlang:function_exported(ExpMod, read, 5) of
		false ->
			{{rread,Tag,<<>>},St};
		true ->
			case ExpMod:read(File, Offset, N, St, ModConf) of
			{cache,Cached} ->
				case cache_slice(Cached, Offset, N) of
				Data when is_binary(Data) ->
					St1 = St#ac{fids = dict:store(Fid,
							Finfo#fid{cached =Cached}, Fids)},
					{{rread,Tag,Data},St1};
				misaligned ->
					misaligned_offset(Tag, St)
				end;
			Data ->
				{{rread,Tag,Data},St}
			end
		end;

	_ -> %% auth
		invalid_fid(Tag, St)
	end;

%% Twrite tag[2] fid[4] offset[8] count[4] data[count]
handle_msg({twrite,Tag,Fid,Offset,Data,_Flag}, #ac{fids =Fids} =St) ->
	case dict:fetch(Fid, Fids) of
	#fid{type =file,exp_mod =ExpMod,mod_conf =ModConf,alias =Alias} ->
		Count = case erlang:function_exported(ExpMod, write, 5) of
		true ->
			ExpMod:write(Alias, Offset, Data, St, ModConf);
		false ->
			0
		end,
		{{rwrite,Tag,Count},St};

	#fid{type =auth} when St#ac.ver =:= e ->

		%%
		%% 9P2000.e client - use MUMBLE scheme
		%%

		MyParent = erlang:parent_node(),
		MyGroup = erlang:node_group(),

		case '9p_auth':unmumble(Data) of
		#mumble{node =N,node_group =G,session_key =SK} ->
			if N =:= MyParent; G =:= MyGroup ->

				%%
				%% Announce the new session to 9p_server to be able to recover
				%% when the transport connection is reestablished. The abort fun
				%% passed along is used by the 9p server to hijack the
				%% connection, if needed.
				%%

				#ac{sock =Sock,trans_mod =TransMod,trans_conf =ModConf} = St,
				AbortFun = fun() ->
						TransMod:close(Sock, ModConf)
				end,
				request({new_session,SK,AbortFun}),

				{{rwrite,Tag,size(Data)},St#ac{basic_auth =true,
											   session_key =SK,
											   peer_node =N,
										   	   peer_group =G}};
			true ->
				not_authorized(Tag, St)
			end;
		_ ->
			not_authorized(Tag, St)
		end;

	#fid{type =auth} when St#ac.ver =:= u ->

		%%
		%% 9P2000.u client - use MUNGE scheme
		%%

		case '9p_auth':unmunge(Data) of
		#munge{uid =Uid,gid =Gid} ->
			{{rwrite,Tag,size(Data)},St#ac{basic_auth =true,
										   unix_user =Uid,
										   unix_group =Gid}};
		_ ->
			not_authorized(Tag, St)
		end;
	_ ->
		cannot_write(Tag, St)
	end;

%% Tsread tag[2] fid[4] nwname[2] nwname*(wname[s])
handle_msg({tsread,Tag,Fid,Names}, #ac{fids =Fids} =St) ->
	case dict:fetch(Fid, Fids) of
	#fid{type =auth} ->
		invalid_fid(Tag, St);
	#fid{path =Path} =Finfo0 ->
		case walk_path(Path, Names, Finfo0, St) of
		{walked,Finfo,_Qids} ->
			Data = case Finfo of
			#fid{type =root} ->
				list_to_binary([encoded_stat(#fid{type =dir,path =[Dir]}, St)
						|| Dir <- list_exports()]);
			#fid{type =dir,
					exp_mod =ExpMod,mod_conf =ModConf} ->
				list_to_binary([encoded_stat(#fid{type =file,path =Path ++ [File],
											exp_mod =ExpMod,
											mod_conf =ModConf,
											name =File,
											alias =Alias}, St)
									|| {File,Alias} <- ExpMod:list_dir(St, ModConf)]);
			#fid{type =file,
					exp_mod =ExpMod,mod_conf =ModConf,alias =Alias} ->
				file_contents(Alias, ExpMod, ModConf, St)
			end,
			if size(Data) > St#ac.msize -?IOHDRSZ ->
				return_error(Tag, <<"too big">>, e2big, St);
			true ->
				{{rsread,Tag,Data},St}
			end;
		_ ->
			cannot_walk(Tag, St)
		end
	end;

%% Tswrite tag[2] fid[4] nwname[2] nwname*(wname[s]) count[4] data[count]
handle_msg({tswrite,Tag,Fid,Names,Data}, #ac{fids =Fids} =St) ->
	case dict:fetch(Fid, Fids) of
	#fid{type =auth} ->
		invalid_fid(Tag, St);
	#fid{path =Path} =Finfo0 ->
		case walk_path(Path, Names, Finfo0, St) of
		{walked,#fid{type =file,
				exp_mod =ExpMod,mod_conf =ModConf,name =File},_Qids} ->

			Count = case erlang:function_exported(ExpMod, write, 5) of
			true ->
				ExpMod:write(File, 0, Data, St, ModConf);
			false ->
				0
			end,
			{{rswrite,Tag,Count},St};
		_ ->
			cannot_write(Tag, St)
		end
	end;

%% Tremove tag[2] fid[4]
handle_msg({tremove,Tag,Fid}, #ac{fids =Fids} =St) ->
	case dict:fetch(Fid, Fids) of
	#fid{type =file,exp_mod =ExpMod,mod_conf =ModConf,alias =Alias} ->
		Deleted = case erlang:function_exported(ExpMod, remove, 3) of
		true ->
			ExpMod:remove(Alias, St, ModConf);
		false ->
			false
		end,
		if Deleted ->
			St1 = St#ac{fids =dict:erase(Fid, Fids)},
			{{rremove,Tag},St1};
		true ->
			not_removed(Tag, St)
		end;
	_ ->
		not_removed(Tag, St)
	end;

%% Tstat tag[2] fid[4]
handle_msg({tstat,Tag,Fid,_Mask}, #ac{fids =Fids} =St) ->
	case dict:fetch(Fid, Fids) of
	#fid{type =auth} ->
		invalid_fid(Tag, St);
	Finfo ->
		Stat = finfo_stat(Finfo, St),
		{{rstat,Tag,Stat},St}
	end;

%% size[4] Twstat tag[2] fid[4] stat[n]
handle_msg({twstat,Tag,Fid,#stat{length =NewLength,
								 name =NewName}}, #ac{fids =Fids} =St) ->
	case dict:fetch(Fid, Fids) of
	#fid{type =file,exp_mod =ExpMod,mod_conf =ModConf,alias =Alias} ->
		if NewLength =/= -1 ->
			case erlang:function_exported(ExpMod, truncate, 4) of
			true ->
				ExpMod:truncate(Alias, NewLength, St, ModConf);
			false ->
				ok %% ignore
			end;
		true ->
			ok
		end,
		if NewName =/= <<>> ->
			case erlang:function_exported(ExpMod, rename, 4) of
			true ->
				ExpMod:rename(Alias, NewName, St, ModConf);
			false ->
				ok %% ignore
			end;
		true ->
			ok
		end;
	_ ->
		ok	%% ignore
	end,
	{{rwstat,Tag},St}.

%% handle attach request for both versions
attach(Tag, _Fid, _AFid, _UName, _AName, _Uid, #ac{basic_auth =false} =St) ->
	not_authorized(Tag, St);
attach(Tag, Fid, _AFid, UName, AName, Uid, #ac{fids =Fids} =St) ->
	Path = [P || P <- binary:split(AName, <<"/">>, [global]),
					P =/= <<>>,
				   	P =/= <<"..">>],
	case find_existing(Path, St) of
	invalid ->
		invalid_path(Tag, St);
	not_found ->
		not_found(Tag, St);
	Finfo ->
		case is_authorized(a_user(UName, Uid), Finfo, St) of
		true ->
			Qid = dummy_qid(Finfo),
			false = dict:is_key(Fid, Fids),
			St1 = St#ac{fids =dict:store(Fid, Finfo, Fids)},
			{{rattach,Tag,Qid},St1};
		false ->
			not_authorized(Tag, St)
		end
	end.

%% perform create operation for both versions
create(Tag, Fid, Name, #ac{fids =Fids} =St) ->
	case dict:fetch(Fid, Fids) of
	#fid{type =dir,path =DirPath,
				   exp_mod =ExpMod,
				   mod_conf =ModConf} ->
		case erlang:function_exported(ExpMod, create, 3) andalso
			ExpMod:create(Name, St, ModConf) of
		true ->
			Finfo = #fid{type =file,path =DirPath ++ [Name],
									exp_mod =ExpMod,
									mod_conf =ModConf,
									name =Name,
									alias =Name},
			Qid = dummy_qid(Finfo),
			St1 = St#ac{fids =dict:store(Fid, Finfo, Fids)},
			{{rcreate,Tag,Qid,St#ac.msize -?IOHDRSZ},St1};
		false ->
			io:format("ExpMod = ~p ModConf = ~p DirPath ~p~n",
								[ExpMod,ModConf,DirPath]),
			return_error(Tag, <<"cannot create">>, eacces, St)
		end;
	_ ->
		invalid_fid(Tag, St)
	end.

% handle walk request recursively
walk(Tag, NewFid, Path, Names, #ac{fids =Fids} =St) ->
	case walk_path(Path, Names, none, St) of
	{_,Finfo,Qids} ->	 %% includes incomplete walk
		St1 = St#ac{fids =dict:store(NewFid, Finfo, Fids)},
		{{rwalk,Tag,Qids},St1};
	cannot_walk ->
		cannot_walk(Tag, St)
	end.
	
walk_path(_Path, [], Finfo0, _St) ->
	{walked,Finfo0,[]};
walk_path(Path, Names, Finfo0, St) ->
	walk_path(Path, Names, St, Finfo0, []).

walk_path([], [<<"..">>|Names], St, Last, Qids) ->
	walk_path_1([], Names, St, Last, Qids);	%% up from root allowed
walk_path(Path, [<<"..">>|Names], St, Last, Qids) ->
	NewPath = lists:reverse(tl(lists:reverse(Path))),
	walk_path_1(NewPath, Names, St, Last, Qids);
walk_path(Path, [Name|Names], St, Last, Qids) ->
	walk_path_1(Path ++ [Name], Names, St, Last, Qids);
walk_path(_Path, [], _St, Last, Qids) ->
	{walked,Last,Qids}.

walk_path_1(NewPath, Names, St, Last, Qids) ->
	case find_existing(NewPath, St) of
	invalid ->
		incomplete_walk(Last, Qids);
	not_found ->
		incomplete_walk(Last, Qids);
	Finfo ->
		case is_authorized(undefined, Finfo, St) of
		false ->
			incomplete_walk(Last, Qids);
		true ->
			Qid = dummy_qid(Finfo),
			walk_path(NewPath, Names, St, Finfo, Qids ++ [Qid])
		end
	end.

incomplete_walk(_Last, []) ->
	cannot_walk;
incomplete_walk(Last, Qids) ->
	{incomplete,Last,Qids}.

%% error branches

not_found(Tag, St) ->
	return_error(Tag, <<"not_found">>, enoent, St).

not_supported(Tag, St) ->
	return_error(Tag, <<"not_supported">>, eopnotsupp, St).

not_authorized(Tag, St) ->
	return_error(Tag, <<"not authorized">>, eperm, St).

invalid_path(Tag, St) ->
	return_error(Tag, <<"invalid path">>, einval, St).

invalid_fid(Tag, St) ->
	return_error(Tag, <<"invalid fid">>, ebadf, St).

invalid_aname(Tag, St) ->
	return_error(Tag, <<"invalid aname">>, einval, St).

cannot_write(Tag, St) ->
	return_error(Tag, <<"cannot write">>, eacces, St).

not_removed(Tag, St) ->
	return_error(Tag, <<"cannot remove">>, eacces, St).

misaligned_offset(Tag, St) ->
	return_error(Tag, <<"misaligned offset">>, einval, St).

cannot_walk(Tag, St) ->
	return_error(Tag, <<"cannot walk">>, eacces, St).

protocol_error(Tag, St) ->
	return_error(Tag, <<"protocol error">>, einval, St).

return_error(Tag, Msg, Error, #ac{ver =u} =St) ->
	{{rerror,Tag,Msg,Error},St};
return_error(Tag, Msg, _Error, St) ->
	{{rerror,Tag,Msg},St}.

%%-------- acceptor helpers ---------------------------------------------------

a_user(UserName, undefined) ->
	UserName;
a_user(UserName, Uid) ->
	{UserName,Uid}.

is_authorized(User, #fid{type =dir,exp_mod =ExpMod,mod_conf =ModConf}, Conn) ->
	ExpMod:module_info(),
	case erlang:function_exported(ExpMod, top_granted, 3) of
	false ->
		%% defaults to true
		true;
	true ->
		ExpMod:top_granted(User, Conn, ModConf)
	end;
is_authorized(User, #fid{type =file,
						 exp_mod =ExpMod,
						 mod_conf =ModConf,
						 alias =Alias}, Conn) ->
	case erlang:function_exported(ExpMod, file_granted, 4) of
	false ->
		%% defaults to true
		true;
	true ->
		ExpMod:file_granted(Alias, User, Conn, ModConf)
	end;
is_authorized(_User, _Finfo, _ModConf) ->
	true.

find_existing(Path, Conn) ->
	case find_export(Path) of
	#fid{type =file,exp_mod =ExpMod,mod_conf =ModConf,name =File} =Finfo ->
		case ExpMod:find(File, Conn, ModConf) of
		false ->
			not_found;
		{found,Alias} ->
			Finfo#fid{alias =Alias}
		end;
	Other ->
		Other
	end.

dummy_qid(#fid{type =root}) ->
	dummy_dir_qid();
dummy_qid(#fid{type =auth}) ->
	auth_qid();
dummy_qid(#fid{type =dir}) ->
	dummy_dir_qid();
dummy_qid(#fid{type =file}) ->
	dummy_file_qid().

dummy_dir_qid() ->
	<<?QTDIR,
	  0:32/little,
	  0:32/little,
	  0:32/little>>.

auth_qid() ->
	<<?QTAUTH,
	  0:32/little,
	  0:32/little,
	  0:32/little>>.

dummy_file_qid() ->
	<<?QTFILE,
	  0:32/little,
	  0:32/little,
	  0:32/little>>.

finfo_stat(#fid{type =root}, St) ->
	#stat{ver =St#ac.ver,
		  qid =dummy_dir_qid(),
		  mode =?DMDIR bor 8#755,
		  atime =St#ac.started,
		  mtime =St#ac.started,
		  length =0,
		  name = <<"/">>,
		  uid =to_bin(erlang:node()),
		  gid =to_bin(erlang:node_group()),
		  muid =to_bin(erlang:node())};

finfo_stat(#fid{type =dir,path =Path,exp_mod =ExpMod,mod_conf =ModConf}, St) ->
	S = ExpMod:top_stat(St, ModConf),
	dir_stat_defaults(Path, St#ac.started, S#stat{ver =St#ac.ver});
	
finfo_stat(#fid{type =file,path =Path,exp_mod =ExpMod,mod_conf =ModConf,alias =Alias}, St) ->
	S = ExpMod:file_stat(Alias, St, ModConf),
	file_stat_defaults(Path, St#ac.started, S#stat{ver =St#ac.ver}).

dir_stat_defaults(Path, Started, #stat{qid =undefined} =Stat) ->
	dir_stat_defaults(Path, Started, Stat#stat{qid =dummy_dir_qid()});
dir_stat_defaults(Path, Started, #stat{mode =undefined} =Stat) ->
	dir_stat_defaults(Path, Started, Stat#stat{mode =?DMDIR bor 8#755});
dir_stat_defaults(Path, Started, #stat{atime =undefined} =Stat) ->
	dir_stat_defaults(Path, Started, Stat#stat{atime =Started});
dir_stat_defaults(Path, Started, #stat{mtime =undefined} =Stat) ->
	dir_stat_defaults(Path, Started, Stat#stat{mtime =Started});
dir_stat_defaults(_, _, Stat) ->
	Stat.

file_stat_defaults(Path, Started, #stat{qid =undefined} =Stat) ->
	file_stat_defaults(Path, Started, Stat#stat{qid =dummy_file_qid()});
file_stat_defaults(Path, Started, #stat{mode =undefined} =Stat) ->
	file_stat_defaults(Path, Started, Stat#stat{mode =8#644});
file_stat_defaults(Path, Started, #stat{atime =undefined} =Stat) ->
	file_stat_defaults(Path, Started, Stat#stat{atime =Started});
file_stat_defaults(Path, Started, #stat{mtime =undefined} =Stat) ->
	file_stat_defaults(Path, Started, Stat#stat{mtime =Started});
file_stat_defaults(_, _, Stat) ->
	Stat.
	
encoded_stat(Finfo, St) ->
	'9p':encode_stat(finfo_stat(Finfo, St)).

to_bin(A) ->
	list_to_binary(atom_to_list(A)).

%% @doc Takes a slice of cached data. There are two types of cached data - a
%% binary (simple) and a list of binaries (more intricate). The complexity of
%% latter case is that the offset must match the beginning of a binary and the
%% resultant slice must not have partial binaries.
%% @end
cache_slice(Data, Offset, Count) when is_binary(Data) ->
	Sz = byte_size(Data),
	if Offset >= Sz ->
			<<>>;
		Offset +Count >= Sz ->
			binary:part(Data, Offset, Sz -Offset);
		true ->
			binary:part(Data, Offset, Count)
	end;
cache_slice(Records, Offset, Count) when is_list(Records) ->
	rec_list_slice(Records, Offset, Count, 0).

rec_list_slice([], _, _, _) ->
	<<>>;
rec_list_slice(Rs, Offset, Count, Offset) ->
	rec_list_collect(Rs, Count, 0, []);
rec_list_slice(_Rs, Offset, _Count, O) when O > Offset ->
	misaligned;
rec_list_slice([R|Rs], Offset, Count, O) ->
	rec_list_slice(Rs, Offset, Count, O +byte_size(R)).

rec_list_collect([], _, _, Acc) ->
	list_to_binary(lists:reverse(Acc));
rec_list_collect([R|Rs], Count, C, Acc) when C +byte_size(R) =< Count ->
	rec_list_collect(Rs, Count, C +byte_size(R), [R|Acc]);
rec_list_collect(_, _, _, Acc) ->
	list_to_binary(lists:reverse(Acc)).

valid_is_aname(<<$/,_/binary>>) -> true;
valid_is_aname(_) -> false.

file_contents(Alias, ExpMod, ModConf, Conn) ->
	case erlang:function_exported(ExpMod, read, 5) of
	true ->
		Count = ExpMod:file_size(Alias, Conn, ModConf),
		case ExpMod:read(Alias, 0, Count, Conn, ModConf) of
		{cache,Cache} ->
			list_to_binary(Cache);
		Data ->
			Data
		end;
	false ->
		<<>>
	end.

%EOF
