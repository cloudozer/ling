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

-module(disk_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(st, {port,
			 sectors,
			 sector_size,
			 next_tag =0,
			 tags =[],
			 pending =[]}).

-define(DISK_REQ_READ,		0).
-define(DISK_REQ_WRITE,		1).
-define(DISK_REQ_BARRIER,	2).
-define(DISK_REQ_FLUSH,		3).

-define(DISK_REP_OK,		0).
-define(DISK_REP_ERROR,		1).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	case erlang:disk_info() of
	undefined ->
		%% no disk present
		ignore;

	DiskInfo ->

		Port = open_port(disk, []),

		Sectors = proplists:get_value(sectors, DiskInfo),
		SectSize = proplists:get_value(sector_size, DiskInfo),
		{ok,#st{port =Port,
				sectors =Sectors,
				sector_size =SectSize}}
	end.

handle_call({read,Start,NumSect}, _From, #st{sectors =TotalSect} =St)
	when Start < 0; Start >= TotalSect;
			NumSect =< 0; Start +NumSect > TotalSect ->
	{reply,{error,badarg},St};

handle_call({read,Start,NumSect}, From, #st{port =Port,pending =Pending} =St) ->
	{Tag,St1} = pick_tag(St),
	Ctl = <<Tag:16,Start:64,NumSect:32>>,
	case erlang:port_control(Port, ?DISK_REQ_READ, Ctl) of
	[?DISK_REP_OK] ->
		{noreply,St1#st{pending =[{Tag,From}|Pending]}};
	[?DISK_REP_ERROR|ErrMsg] ->
		{reply,{error,list_to_atom(ErrMsg)},St}
	end;

handle_call({write,Start,Data}, From, #st{port =Port,
										  sectors =TotalSect,
										  sector_size =SectSize,
										  pending =Pending} =St) ->
	case check_args(Start, Data, TotalSect, SectSize) of
	{ok,NumSect} ->
		{Tag,St1} = pick_tag(St),
		Ctl = <<Tag:16,Start:64,NumSect:32>>,
		case erlang:port_control(Port, ?DISK_REQ_WRITE, [Ctl,Data]) of
		[?DISK_REP_OK] ->
			{noreply,St1#st{pending =[{Tag,From}|Pending]}};
		[?DISK_REP_ERROR|ErrMsg] ->
			{reply,{error,list_to_atom(ErrMsg)},St}
		end;
	Error ->
		{reply,{error,Error},St}
	end;

handle_call(barrier, From, #st{port =Port,
							   pending =Pending} =St) ->
	{Tag,St1} = pick_tag(St),
	Ctl = <<Tag:16>>,
	case erlang:port_control(Port, ?DISK_REQ_BARRIER, Ctl) of
	[?DISK_REP_OK] ->
		{noreply,St1#st{pending =[{Tag,From}|Pending]}};
	[?DISK_REP_ERROR|ErrMsg] ->
		{reply,{error,list_to_atom(ErrMsg)},St}
	end;

handle_call(flush, From, #st{port =Port,
							   pending =Pending} =St) ->
	{Tag,St1} = pick_tag(St),
	Ctl = <<Tag:16>>,
	case erlang:port_control(Port, ?DISK_REQ_FLUSH, Ctl) of
	[?DISK_REP_OK] ->
		{noreply,St1#st{pending =[{Tag,From}|Pending]}};
	[?DISK_REP_ERROR|ErrMsg] ->
		{reply,{error,list_to_atom(ErrMsg)},St}
	end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({disk_async,Port,Tag,Result}, #st{port =Port,
											  pending =Pending,
											  tags =Tags} =St) ->
	{value,{_,From},Pend1} = lists:keytake(Tag, 1, Pending),
	case Result of 
	ok ->
		gen_server:reply(From, ok);
	Value ->
		gen_server:reply(From, {ok,Value})
	end,
    {noreply,St#st{pending =Pend1,tags =[Tag|Tags]}};

handle_info({disk_async_error,Port,Tag,Error}, #st{port =Port,
												   pending =Pending,
												   tags =Tags} =St) ->
	{value,{_,From},Pend1} = lists:keytake(Tag, 1, Pending),
	gen_server:reply(From, {error,Error}),
    {noreply,St#st{pending =Pend1,tags =[Tag|Tags]}}.

terminate(_Reason, _State) ->
	%% The disk port is linked to the process and will close automatically
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

check_args(Start, _Data, _TotalSect, _SectSize) when Start < 0 ->
	badarg;
check_args(_Start, Data, _TotalSect, SectSize)
		when byte_size(Data) rem SectSize =/= 0 ->
	badarg;
check_args(Start, Data, TotalSect, SectSize) ->
	NumSect = byte_size(Data) div SectSize,
	if Start +NumSect > TotalSect ->
		badarg;
	true ->
		{ok,NumSect}
	end.

pick_tag(#st{tags =[T|Ts]} =St) ->
	{T,St#st{tags =Ts}};
pick_tag(#st{next_tag =T} =St) ->
	{T,St#st{next_tag =T +1}}.

%%EOF
