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

-module(console).
-export([start/0]).

-define(COLUMNS, 0).

-record(cons, {port,
			   input = [],
			   delayed = none,
			   cont}).

start() ->
	Pid = spawn(fun() ->
			Port = erlang:open_port({fd,0,1}, []),
			St = #cons{port=Port},
			loop(St) end),
	{ok,Pid}.

loop(#cons{port=Port} = State) ->
	receive
	{io_request,From,ReplyAs,Req} =IoReq ->
		case io_request(Req, State) of
		{done,Reply,NewState} ->
			From ! {io_reply,ReplyAs,Reply},
			loop(NewState);
		{delay,Cont} ->
			case State#cons.delayed of
			none ->
				ok;
			{_,PendFrom,PendReplyAs,_} ->
				%% Only one outstanding get_until request is allowed
				PendFrom ! {io_reply,PendReplyAs,{error,aborted}}
			end,
			loop(State#cons{input=[],delayed=IoReq,cont=Cont})
		end;

	{Port,{data,Data}} when is_list(Data) ->
		case State#cons.delayed of
		none ->
			NI = State#cons.input ++ Data,
			loop(State#cons{input=NI});
		{_,From,ReplyAs,{get_until,Prompt,Mod,Func,Xargs}} ->
			Cont = State#cons.cont,
			case apply(Mod, Func, [Cont,Data|Xargs]) of
			{done,Reply,Chars} ->
				From ! {io_reply,ReplyAs,Reply},
				loop(State#cons{input=Chars,delayed=none});
			{more,NewCont} ->
				prompt(State#cons.port, Prompt),
				loop(State#cons{cont=NewCont})
			end
		end
	end.

io_request({put_chars,Mod,Fun,Args}, State) ->
	IoList = apply(Mod, Fun, Args),
	put_chars(State#cons.port, IoList),
	{done,ok,State};

io_request({put_chars,Chars}, State) ->
	put_chars(State#cons.port, Chars),
	{done,ok,State};

io_request({put_chars,_Encoding,Chars}, State) ->
	put_chars(State#cons.port, Chars),	%% TODO: encoding ignored
	{done,ok,State};

io_request({get_until,Prompt,Mod,Func,Xargs}, State) ->
	get_until(Prompt, Mod, Func, Xargs, State);

io_request({get_until,_Encoding,Prompt,Mod,Func,Xargs}, State) ->
	get_until(Prompt, Mod, Func, Xargs, State); %% TODO: encoding ignored

io_request({requests,Reqs}, State) ->
	requests(Reqs, State);

io_request({get_geometry,columns}, State) ->
	{done,?COLUMNS,State};

io_request(X, State) ->
	erlang:display({bad_io_req,X}),
	{done,{error,unsupported},State}.

put_chars(Port, <<Chunk:128/binary,Chars/binary>>) ->
	Port ! {self(),{command,Chunk}},
	put_chars(Port, Chars);
put_chars(Port, Chars) ->
	Port ! {self(),{command,Chars}}.

prompt(_Port, '') ->
	ok;
prompt(Port, A) when is_atom(A) ->
	put_chars(Port, atom_to_list(A));
prompt(Port, L) when is_list(L) ->
	put_chars(Port, L).

get_until(Prompt, Mod, Func, Xargs, #cons{input=Input} =State) ->
	prompt(State#cons.port, Prompt),
	case apply(Mod, Func, [[],Input|Xargs]) of
	{done,Res,Chars} ->
		{done,Res,State#cons{input=Chars}};
	{more,Cont} ->
		{delay,Cont}
	end.

requests(Reqs, State) ->
	requests(Reqs, {done,ok,State}, State).

requests([], Last, _State) ->
	Last;
requests([Req|Reqs], _, State) ->
	case io_request(Req, State) of
	{done,{error,_},_} = Last ->
		Last;
	{done,_,NewState} = Last ->
		requests(Reqs, Last, NewState)
	end.

%%EOF
