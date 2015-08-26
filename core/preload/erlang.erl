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

-module(erlang).
-export([get_module_info/1]).
-export([raise/3,throw/1,exit/1,exit/2,error/1,error/2]).
-export(['+'/2,'-'/2,'*'/2,'/'/2,'div'/2,'rem'/2]).
-export(['-'/1,'+'/1]).
-export(['bsl'/2,'bsr'/2]).
-export(['band'/2,'bor'/2,'bxor'/2,'bnot'/1]).
-export([apply/3,apply/2,make_fun/3]).
-export([yield/0]).
-export(['!'/2,remote_send/2,send_msg/2]).
-export([send/2,send/3]).
%-export([send_nosuspend/2,send_nosuspend/3]).
-export([self/0,node/0]).
-export([setnode/2]).
-export([nodes/0,nodes/1,monitor_node/2,monitor_node/3]).
-export([disconnect_node/1]).
-export([get_cookie/0]).
-export([min/2,max/2]).
-export([binary_to_integer/1,binary_to_integer/2]).
-export([integer_to_binary/1,integer_to_binary/2]).
-export([integer_to_list/2]).
-export([list_to_integer/1,list_to_integer/2,list_to_integer/3]).
-export([binary_to_term/1,binary_to_term/2]).
-export([term_to_binary/1,term_to_binary/2]).
-export([external_size/1,external_size/2]).
-export([spawn_opt/4,spawn_opt/2,spawn_opt/5]).
-export([spawn/4,spawn_link/4]).
-export([is_alive/0]).
-export([open_port/2,port_close/1]).
-export([port_command/2,port_command/3]).
-export([port_connect/2]).

-export([decode_packet/3]).

-export([disk_info/0]).

-export([new_counter/0,update_counter/1]).

-export([system_monitor/0,system_monitor/1,system_monitor/2]).

-export([system_info/1]).
-export([memory/0]).
-export([process_info/1]).
-export([port_info/1]).
-export([put/2,get/0,get_keys/1,erase/0,erase/1]).
-export([demonitor/2]).
-export([match_spec_test/3]).

-export([trace_pattern/2,trace_pattern/3,trace_info/2,trace_delivered/1,trace/3]).

-export([localtime_to_universaltime/1,localtime_to_universaltime/2]).
-export([universaltime_to_localtime/1]).

-export([call_on_load_function/1,finish_after_on_load/2]).

-export([halt/0,halt/1,halt/2]).

-export([atom_to_binary/2,binary_to_atom/2,binary_to_existing_atom/2]).

-export([crc32/1,adler32/1]).

-export([nif_error/1]).

%% Not sure what are these. Special handling in otp.tab hints that they may be
%% widespead.
-export([dt_spread_tag/1,dt_restore_tag/1]).

-export([is_builtin/3]).

-export([load_module/2]).
-export([purge_module/1]).

-compile({no_auto_import,[halt/2,port_command/3]}).

-define(LING_COMPAT_REL, 17).
-define(LING_COMPAT_OTP_RELEASE, "17").	%% do not forget to update this

-define(SLICE_REDUCTIONS, 1000).

get_module_info(Module) when is_atom(Module) ->
	Items = [exports,imports,attributes,compile],
	[{Item,erlang:get_module_info(Module, Item)}
			|| Item <- Items].

raise(Class, Reason, StackTrace) ->
	erlang:raise(Class, Reason, StackTrace).

throw(What) ->
	erlang:throw(What).

exit(What) ->
	erlang:exit(What).

exit(Pid, Reason) ->
	erlang:exit(Pid, Reason).

error(Reason) ->
	erlang:error(Reason).

error(Reason, Args) ->
	erlang:error(Reason, Args).

'+'(A, B) -> A + B.
'-'(A, B) -> A - B.
'*'(A, B) -> A * B.
'/'(A, B) -> A / B.
'div'(A, B) -> A div B.
'rem'(A, B) -> A rem B.
'-'(A) -> -A.
'+'(A) -> A.

'bsl'(A, B) -> A bsl B.
'bsr'(A, B) -> A bsr B.
'band'(A, B) -> A band B.
'bor'(A, B) -> A bor B.
'bxor'(A, B) -> A bxor B.
'bnot'(A) -> bnot A.

apply(M, F, As) ->
	erlang:apply(M, F, As).

apply(Fun, As) ->
	erlang:apply(Fun, As).

make_fun(M, F, A) ->
	erlang:make_fun(M, F, A).

yield() ->
	erlang:yield().

send_msg(Pid, Msg) ->
	Pid ! Msg.

'!'(Pid, Msg) ->
	Pid ! Msg.

%% '!' routes calls here for tuples and long Pids
remote_send({_,_,_}, _Msg) -> erlang:error(not_implemented);
remote_send({DomId,Name}, Msg) when is_integer(DomId), is_atom(Name) ->
	case erlang:container() =:= DomId of
		true  -> Name ! Msg;	%% local
		false -> Straw = container_straw(DomId),
				 Straw ! {envelope,Name,Msg} end;
remote_send(Pid, Msg) when is_pid(Pid) ->
	case erlang:machine() =:= erlang:machine(Pid) of
		false -> erlang:error(not_implemented);
		true  -> DomId = erlang:container(Pid),
				 Straw = container_straw(DomId),
				 Straw ! {envelope,Pid,Msg} end;
remote_send(Dst, Msg) -> erlang:error(badarg, [Dst,Msg]).

container_straw(DomId) ->
	case strawman:split(DomId) of
		{error,_} ->
			case strawman:open(DomId) of
				{error,_} -> erlang:error({bad_container,DomId});
				ok -> {ok,Straw} = strawman:split(DomId), Straw end;
		{ok,Straw} -> Straw end.

send(Dst, Msg) ->
	Dst ! Msg,
	Msg.

send(Dst, Msg, _Opts) ->
	Dst ! Msg,
	ok.

self() ->
	erlang:self().

setnode(_Arg1, _Arg2) ->
	erlang:error(not_implemented). %%TODO

node() ->
	erlang:node().

nodes() ->
	[]. %%TODO

nodes(_Arg) ->
	[]. %%TODO

monitor_node(_Node, _Flag) ->
   	true. %%TODO

monitor_node(_Node, _Flag, _Opts) ->
	true. %%TODO

disconnect_node(_Node) ->
	ignored. %%TODO

get_cookie() ->
	nocookie. %%XXX

min(A, B) when A < B -> A;
min(_, B) -> B.

max(A, B) when A > B -> A;
max(_, B) -> B.

binary_to_integer(Bin) -> erlang:list_to_integer(binary_to_list(Bin)).
binary_to_integer(Bin, Base) -> erlang:list_to_integer(binary_to_list(Bin), Base). 

integer_to_binary(I) -> list_to_binary(integer_to_list(I)).
integer_to_binary(I, Base) -> list_to_binary(erlang:integer_to_list(I, Base)).

integer_to_list(I, 10) ->
    erlang:integer_to_list(I);
integer_to_list(I, Base) 
  when is_integer(I), is_integer(Base), Base >= 2, Base =< 1+$Z-$A+10 ->
    if I < 0 ->
	    [$-|integer_to_list(-I, Base, [])];
       true ->
	    integer_to_list(I, Base, [])
    end;
integer_to_list(I, Base) ->
    erlang:error(badarg, [I, Base]).

integer_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 10 ->
		 [D-10+$A|R0];
	    true ->
		 [D+$0|R0]
	 end,
    if I1 =:= 0 ->
	    R1;
       true ->
	    integer_to_list(I1, Base, R1)
    end.

list_to_integer(L) ->
	erlang:list_to_integer(L, 10).

%%list_to_integer(L, 10) ->
%%    erlang:list_to_integer(L);
list_to_integer(L, Base)
  when is_list(L), is_integer(Base), Base >= 2, Base =< 1+$Z-$A+10 ->
    case list_to_integer_sign(L, Base) of 
	I when is_integer(I) ->
	    I;
	Fault ->
	    erlang:error(Fault, [L,Base])
    end;
list_to_integer(L, Base) ->
    erlang:error(badarg, [L,Base]).

list_to_integer_sign([$-|[_|_]=L], Base) ->
    case list_to_integer(L, Base, 0) of
	I when is_integer(I) ->
	    -I;
	I ->
	    I
    end;
list_to_integer_sign([$+|[_|_]=L], Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign([_|_]=L, Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign(_, _) ->
    badarg.

list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $0, D =< $9, D < Base+$0 ->
    list_to_integer(L, Base, I*Base + D-$0);
list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $A, D < Base+$A-10 ->
    list_to_integer(L, Base, I*Base + D-$A+10);
list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $a, D < Base+$a-10 ->
    list_to_integer(L, Base, I*Base + D-$a+10);
list_to_integer([], _, I) ->
    I;
list_to_integer(_, _, _) ->
    badarg.

binary_to_term(T) ->
	erlang:'binary_to_term$'(T, false).

binary_to_term(T, []) ->
	erlang:'binary_to_term$'(T, false);
binary_to_term(T, [safe]) ->
	erlang:'binary_to_term$'(T, true);
binary_to_term(T, Opts) ->
	erlang:error(badarg, [T,Opts]).

term_to_binary(T) ->
	%% defaults to no compression and minor version 1
	erlang:'term_to_binary$'(T, 0, 1).

term_to_binary(T, Opts) when is_list(Opts) ->
	{Compression,Version} = lists:foldl(fun(compressed, {_C,V}) ->
		{6,V};	%% default compression level
	({compressed,L}, {_C,V}) when is_integer(L), L >=0, L =< 9 ->
		{L,V};
	({minor_version,M}, {C,_V}) when is_integer(M), M >= 0, M =< 1 ->
		{C,M};
	(_, _) ->
		erlang:error(badarg, [T,Opts])
	end, {0,1}, Opts),	%% default minor version is 1 since R17
	erlang:'term_to_binary$'(T, Compression, Version);

term_to_binary(T, Opts) ->
	erlang:error(badarg, [T,Opts]).

external_size(T) ->
	erlang:'external_size$'(T, 1).	%% default version is 1 since R17

external_size(T, []) ->
	erlang:'external_size$'(T, 1);
external_size(T, [{minor_version,Ver}]) when is_integer(Ver), Ver >= 0, Ver =< 1 ->
	erlang:'external_size$'(T, Ver);
external_size(T, Opts) ->
	erlang:error(badarg, [T,Opts]).

spawn_opt(Module, Function, Args, Opts) ->
	Pid = case lists:member(link, Opts) of
	true ->
		spawn_link(Module, Function, Args);
	false ->
		spawn(Module, Function, Args)
	end,

	%%XXX: is this atomic?

	lists:foreach(fun(link) ->
		ok;
	(monitor) ->
		erlang:monitor(process, Pid);
	({Opt,Val}) ->
		erlang:process_flag(Pid, Opt, Val)
	end, Opts),

	Pid.

spawn_opt(Fun, Opts) when is_function(Fun, 0) ->
	Pid = case lists:member(link, Opts) of
	true ->
		spawn_link(Fun);
	false ->
		spawn(Fun)
	end,

	lists:foreach(fun(link) ->
		ok;
	(monitor) ->
		erlang:monitor(process, Pid);
	({Opt,Val}) ->
		erlang:process_flag(Pid, Opt, Val)
	end, Opts),

	Pid.

spawn_opt(x_Node, _, _, _, _) ->
	erlang:error(not_implemented). %%TODO

spawn(Node, M, F, A) when Node =:= erlang:node() ->
	spawn(M, F, A);
spawn(_Node, _, _, _) ->
	erlang:error(not_implemented). %%TODO

spawn_link(Node, M, F, A) when Node =:= erlang:node() ->
	spawn_link(M, F, A);
spawn_link(_Node, _, _, _) ->
	erlang:error(not_implemented). %%TODO

is_alive() ->
	false.		%% how to do distribution is not decided yet

-define(PORT_BITS_DEFAULT, 16#3).
-define(PORT_BITS_INOUT_OFFSET, 0).
-define(PORT_BITS_INOUT_SIZE, 2).
-define(PORT_BITS_BINARY_OFFSET, 2).
-define(PORT_BITS_BINARY_SIZE, 1).
-define(PORT_BITS_EOF_OFFSET, 3).
-define(PORT_BITS_EOF_SIZE, 1).
-define(PORT_BITS_PACKET_OFFSET, 4).
-define(PORT_BITS_PACKET_SIZE, 4).
-define(PORT_BITS_LINE_OFFSET, 8).
-define(PORT_BITS_LINE_SIZE, 10).

bit_field(Value, Offset, Size, Acc) ->
	Mask = (1 bsl Size) -1,
	MaskedValue = Value band Mask,
	MaskedAcc = Acc band (bnot (Mask bsl Offset)),
    MaskedAcc bor (MaskedValue bsl Offset).

port_options(Opts) ->
	%erlang:display({opts,Opts}),
	port_options(Opts, ?PORT_BITS_DEFAULT).

%% stream/{packet,N} encoding:
%%
%% 0 - stream 
%% 1 - {packet,1}
%% 2 - {packet,2}
%% 3 - {packet,4}

port_options([], Acc) ->
	Acc;
port_options([{packet,4}|Opts], Acc) ->
	port_options(Opts, bit_field(3, ?PORT_BITS_PACKET_OFFSET,
									?PORT_BITS_PACKET_SIZE, Acc));
port_options([{packet,N}|Opts], Acc) when N =:= 2; N =:= 1 ->
	port_options(Opts, bit_field(N, ?PORT_BITS_PACKET_OFFSET,
									  ?PORT_BITS_PACKET_SIZE, Acc));
port_options([stream|Opts], Acc) ->
	port_options(Opts, bit_field(0, ?PORT_BITS_PACKET_OFFSET,
									?PORT_BITS_PACKET_SIZE, Acc));
port_options([{line,L}|Opts], Acc) when L > 0, L =< 1023 ->
	port_options(Opts, bit_field(L, ?PORT_BITS_LINE_OFFSET,
									?PORT_BITS_LINE_SIZE, Acc));
port_options([{cd,Dir}|Opts], Acc) when is_list(Dir) ->
	%% option ignored
	port_options(Opts, Acc);
port_options([{env,Env}|Opts], Acc) when is_list(Env) ->
	%% option ignored
	port_options(Opts, Acc);
port_options([{args,Args}|Opts], Acc) when is_list(Args) ->
	%% option ignored
	port_options(Opts, Acc);
port_options([{arg0,Arg0}|Opts], Acc) when is_list(Arg0) ->
	%% option ignored
	port_options(Opts, Acc);
port_options([exit_status|Opts], Acc) ->
	%% option ignored
	port_options(Opts, Acc);
port_options([use_stdio|Opts], Acc) ->
	%% option ignored
	port_options(Opts, Acc);
port_options([nouse_stdio|Opts], Acc) ->
	%% option ignored
	port_options(Opts, Acc);
port_options([stderr_to_stdout|Opts], Acc) ->
	%% option ignored
	port_options(Opts, Acc);
port_options([overlapped_io|Opts], Acc) ->
	%% option ignored
	port_options(Opts, Acc);
port_options([in|Opts], Acc) ->
	port_options(Opts, bit_field(1, ?PORT_BITS_INOUT_OFFSET,
									?PORT_BITS_INOUT_SIZE, Acc));
port_options([out|Opts], Acc) ->
	port_options(Opts, bit_field(2, ?PORT_BITS_INOUT_OFFSET,
									?PORT_BITS_INOUT_SIZE, Acc));
port_options([binary|Opts], Acc) ->
	port_options(Opts, bit_field(1, ?PORT_BITS_BINARY_OFFSET,
									?PORT_BITS_BINARY_SIZE, Acc));
port_options([eof|Opts], Acc) ->
	port_options(Opts, bit_field(1, ?PORT_BITS_EOF_OFFSET,
									?PORT_BITS_EOF_SIZE, Acc));
port_options([hide|Opts], Acc) ->
	%% option ignored
	port_options(Opts, Acc);
port_options(_, _) ->
	erlang:error(badarg).

open_port(PortName, Opts) ->
	Drv = port_driver(PortName),
	BitOpts = port_options(Opts),
	erlang:port_open(Drv, BitOpts).

port_driver(echo)	  -> echo;
port_driver(vif)	  -> vif;
port_driver(tube)	  -> tube;
port_driver(disk)	  -> disk;
port_driver({fd,2,2}) -> dumb_console;
port_driver({spawn,console}) -> console;
port_driver({spawn,"inet_gethost " ++ _}) -> dns;
port_driver({spawn_driver,"udp_inet"}) -> udp;
port_driver({spawn_driver,"tcp_inet"}) -> tcp;
port_driver(PortName) ->
	erlang:display({no_driver,PortName}),
	erlang:error(badarg).

port_command(Port, Data) ->
	port_command(Port, Data, []).

port_command(Port, Data, Opts) when is_port(Port), is_list(Opts) ->
	case cmd_opts(Opts) of
	{_,true} ->
		case erlang:port_is_busy(Port) of
		true ->
			false;
		false ->
			Port ! {erlang:self(),{'command$',Data}},
			true
		end;
	_ ->
		Port ! {erlang:self(),{'command$',Data}},
		true
	end.

cmd_opts(Opts) ->
	cmd_opts(Opts, {false,false}).

cmd_opts([], Acc) ->
	Acc;
cmd_opts([force|Opts], {_,NS}) ->
	cmd_opts(Opts, {true,NS});
cmd_opts([nosuspend|Opts], {F,_}) ->
	cmd_opts(Opts, {F,true});
cmd_opts(_, _) ->
	erlang:error(badarg).

port_connect(Port, Pid) when is_pid(Pid) ->
	Port ! {erlang:self(),{'connect$',Pid}},
	true.

port_close(Port) when is_atom(Port); is_port(Port) ->
	Port ! {erlang:self(),'close$'},
	true;	%% silent close, no notification
port_close(_) ->
	erlang:error(badarg).

decode_packet(Type, Bin, Opts) when is_bitstring(Bin), is_list(Opts) ->
	{PackSize,LineLen} = decode_packet_opts(Opts),
	erlang:'decode_packet$'(Type, Bin, PackSize, LineLen);
decode_packet(_, _, _) ->
	erlang:error(badarg).

decode_packet_opts(Opts) ->
	decode_packet_opts(Opts, {0,0}).

decode_packet_opts([], Acc) ->
	Acc;
decode_packet_opts([{packet_size,Sz}|Opts], {_,LineLen}) when is_integer(Sz), Sz >= 0 ->
	decode_packet_opts(Opts, {Sz,LineLen});
decode_packet_opts([{line_length,Ln}|Opts], {PackSize,_}) when is_integer(Ln), Ln >= 0 ->
	decode_packet_opts(Opts, {PackSize,Ln});
decode_packet_opts(_, _) ->
	erlang:error(badarg).

disk_info() ->
	case erlang:disk_info(info) of
	undefined ->
		undefined;	%% disk not present
	{info,_} =Info ->
		[Info|
			[erlang:disk_info(Item)
				|| Item <- [sectors,sector_size,features]]]
	end.

new_counter() ->
	erlang:new_counter(64).

update_counter(Id) ->
	erlang:update_counter(Id, 1).

system_monitor() ->
	erlang:display({not_implemented,system_monitor}),
	undefined. %%TODO

system_monitor(Arg) ->
	erlang:display({not_implemented,system_monitor,Arg}),
	undefined. %%TODO

system_monitor(Pid, Opts) ->
	erlang:display({not_implemented,system_monitor,Pid,Opts}),
	undefined. %%TODO

system_info(os_type) -> {?LING_PLATFORM,?LING_OS};
system_info(os_version) -> {7,7,7};
system_info(version) -> "6.3";	%% Eshell version?

system_info(system_version) ->
	lists:flatten(["Erlang [ling-",?LING_VER,"]\n"]);

system_info(compat_rel) -> ?LING_COMPAT_REL;
system_info(otp_release) -> ?LING_COMPAT_OTP_RELEASE;
system_info(modified_timing_level) -> undefined;
system_info(wordsize) -> 4;
system_info({wordsize,internal}) -> 4;
system_info({wordsize,external}) -> 4;
system_info(smp_support) -> false;
system_info(snifs) -> [];
system_info(heap_type) -> private;
system_info(schedulers) -> 1;
system_info(schedulers_online) -> 1;
system_info(context_reductions) -> ?SLICE_REDUCTIONS;
system_info({allocator,ets_alloc}) -> false;
system_info(ets_always_compress) -> false;
system_info(thread_pool_size) -> 0;
system_info(process_count) -> length(processes());
system_info(hipe_architecture) -> undefined;
system_info(machine) -> "LING";
system_info(logical_processors) -> 1;
system_info(break_ignored) -> false.

memory() ->
	ProcMem = erlang:memory(processes),
	SysMem = erlang:memory(system),
	OtherTypes = [processes_used,
				  atom,
				  atom_used,
				  binary,
				  code,
				  ets],
	[{total,ProcMem+SysMem},
	 {processes,ProcMem},
	 {system,SysMem}]
			++
	 [{Type,erlang:memory(Type)}
			|| Type <- OtherTypes].

process_info(Pid) ->
	case is_process_alive(Pid) of
	false ->
		undefined;

	true ->
		Items = [current_function,
				 initial_call,
				 status,
				 message_queue_len,
				 messages,
				 links,
				 dictionary,
				 trap_exit,
				 error_handler,
				 priority,
				 group_leader,
				 total_heap_size,
				 heap_size,
				 stack_size,
				 reductions,
				 garbage_collection],

		InfoTuples = [erlang:process_info(Pid, Item)
				|| Item <- Items],

		case erlang:process_info(Pid, registered_name) of
		[] ->
			InfoTuples;
		NameTuple ->
			[NameTuple|InfoTuples]
		end
	end.

port_info(Port) ->
	Items = [id,
			 connected,
			 links],

	case erlang:port_info(Port, registered_name) of
	undefined ->
		undefined;

	[] ->
		[erlang:port_info(Port, Item) || Item <- Items];

	RegName ->
		[{registered_name,RegName}] ++
			[erlang:port_info(Port, Item) || Item <- Items]

	end.

put(Key, Val) ->
	Dict = erlang:'get_dictionary$'(),
	case proplists:get_value(Key, Dict) of
	undefinded=OldVal ->
		erlang:'set_dictionary$'([{Key,Val}|Dict]),
		OldVal;
	OldVal ->
		CleanDict = proplists:delete(Key, Dict),
		erlang:'set_dictionary$'([{Key,Val}|CleanDict]),
		OldVal
	end.

get() ->
	erlang:'get_dictionary$'().


%% compiler wants it to be a BIF
%%
%%get(Key) ->
%%	proplists:get_value(Key, erlang:'$get_dictionary$'()).

erase() ->
	Dict = erlang:'get_dictionary$'(),
	erlang:'set_dictionary$'([]),
	Dict.

erase(Key) ->
	Dict = erlang:'get_dictionary$'(),
	case proplists:get_value(Key, Dict) of
	undefinded=OldVal ->
		OldVal;
	OldVal ->
		CleanDict = proplists:delete(Key, Dict),
		erlang:'set_dictionary$'(CleanDict),
		OldVal
	end.

get_keys(Val) ->
	[Key || {Key,Val1} <- erlang:'get_dictionary$'(), Val =:= Val1].

demonitor(MRef, []) ->
	erlang:demonitor(MRef);
demonitor(MRef, [flush]) ->
	erlang:demonitor(MRef),
	receive
		{_,MRef,_,_,_} ->
			true
	after 0 ->
			true
	end.

match_spec_test(Term, Ms, table) ->
	try
		case ets:match_spec_run([Term], Ms) of
		[] ->
			{ok,false,none,none};	%% {ok,Result,Flags,Messages};
		[Result] ->
			{ok,Result,none,none}
		end
	catch error:badarg ->
		{error,[{error,"Human-readable error descriptions not supported"}]}
	end.

trace_pattern(MFA, MatchSpec) ->
	trace_pattern(MFA, MatchSpec, []).

trace_pattern(_MFA, _MatchSpec, _FlagList) ->
	erlang:error(not_supported).

trace_info(_PidOrFunc, _Item) ->
	erlang:error(not_supported).

trace_delivered(_Tracee) ->
	erlang:error(not_supported).

trace(_PidSpec, _How, _FlagList) ->
	erlang:error(not_supported).

localtime_to_universaltime(DateTime) ->
	localtime_to_universaltime(DateTime, false).

localtime_to_universaltime(DateTime, _IsDst) ->
	%% local time is always UTC
	DateTime.

universaltime_to_localtime(DateTime) ->
	%% local time is always UTC
	DateTime.

dt_spread_tag(_) -> true.
dt_restore_tag(_) -> true.

call_on_load_function(_) -> ok.
finish_after_on_load(_, _) -> ok.

halt() ->
	halt(0, []).

halt(Status) ->
	halt(Status, []).

halt(Status, [{flush,Flush}]) ->
	erlang:'halt$'(Status, Flush);
halt(Status, []) ->
	erlang:'halt$'(Status, true);
halt(_, _) ->
	erlang:error(badarg).

atom_to_binary(Atom, Enc) ->
	Chars = atom_to_list(Atom),
	unicode:characters_to_binary(Chars, latin1, Enc).

binary_to_atom(Bin, _) when byte_size(Bin) > 255 -> erlang:error(system_limit); %% relaxed
binary_to_atom(Bin, Enc) ->
	List = unicode:characters_to_list(Bin, Enc),
	list_to_atom(List).

binary_to_existing_atom(Bin, _) when byte_size(Bin) > 255 -> erlang:error(system_limit); %% relaxed
binary_to_existing_atom(Bin, Enc) ->
	List = unicode:characters_to_list(Bin, Enc),
	list_to_existing_atom(List).

crc32(Data) ->
	erlang:crc32(0, Data).

adler32(Data) ->
	erlang:adler32(1, Data).

is_builtin(M, F, A) ->
	ling_bifs:is_builtin(M, F, A).

load_module(Mod, <<"FOR1",_:32,"LING",_/binary>> =Bin) ->
	erlang:'load_module$'(Mod, Bin);
load_module(Mod, <<"FOR1",_:32,"BEAM",_/binary>> =Bin) ->
	{ok,L} = ling_code:beam_to_ling(Bin),
	{ok,S} = ling_code:ling_to_specs(L),
	LingBin = ling_lib:specs_to_binary(S),
	erlang:'load_module$'(Mod, LingBin);
load_module(_, _) ->
	{error,badfile}.

purge_module(Mod) ->
	erlang:purge_module(Mod).

nif_error(Error) ->
	erlang:error({nifs_not_supported,Error}).

%EOF
