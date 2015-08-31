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

-module(ling_disasm).
-export([beam/1]).

-include("ling_code.hrl").

-define(tag_u, 0).
-define(tag_i, 1).
-define(tag_a, 2).
-define(tag_x, 3).
-define(tag_y, 4).
-define(tag_f, 5).
-define(tag_h, 6).
-define(tag_z, 7).

-define(CODE_CHUNK, "Code").
-define(STR_CHUNK, "StrT").
-define(LIT_CHUNK, "LitT").
-define(FUN_CHUNK, "FunT").
-define(ATTR_CHUNK, "Attr").
-define(CINF_CHUNK, "CInf").
-define(LINE_CHUNK, "Line").
-define(ABST_CHUNK, "Abst").

-record(da, {atoms,literals,lambdas,imports,exports}).

beam(Beam) -> %% {ok,#m{}}

	ChunkNames = [?CODE_CHUNK,
		atoms,indexed_imports,labeled_exports,?STR_CHUNK,abstract_code],

	{ok,{Module,[{?CODE_CHUNK,HdrCodeBin},
				 {atoms,Atoms},
				 {indexed_imports,Imports},
				 {labeled_exports,Exports},
				 {?STR_CHUNK,StrTabBin},
				 {abstract_code,_AbstCode}]}} = 

			 beam_lib:chunks(Beam, ChunkNames),

	%% AbstCode = no_abstract_code | {raw_abstract_v1,Forms}
	
	%%TODO: make this an option
	AbstCode = no_abstract_code,

	Literals = case optional_chunk(Beam, ?LIT_CHUNK) of
	missing ->
		[];
	LitBin ->
		decode_literals(LitBin)
	end,

	Lambdas = case optional_chunk(Beam, ?FUN_CHUNK) of
	missing ->
		[];
	FunBin ->
		decode_lambdas(FunBin, Atoms)
	end,

	%%io:format("Lambdas=~p~n", [Lambdas]),

	Attrs = case optional_chunk(Beam, ?ATTR_CHUNK) of
	missing ->
		undefined;
	AttrBin ->
		binary_to_term(AttrBin)
	end,

	CInfs = case optional_chunk(Beam, ?CINF_CHUNK) of
	missing ->
		undefined;
	CInfBin ->
		binary_to_term(CInfBin)
	end,

	<<16:32,				%% header size
	  0:32,					%% BEAM format number
	  _MaxOpCode:32,
	  _NumLabels:32,
	  _NumFunctions:32,CodeBin/binary>> = HdrCodeBin,
	
	St = #da{atoms=Atoms,
			 literals=Literals,
			 lambdas=Lambdas,
			 imports=Imports,
			 exports=Exports},
	
	Code = disasm_code(CodeBin, St),

	LineInfo = case optional_chunk(Beam, ?LINE_CHUNK) of
	missing ->
		undefined;
	LineBin ->
		decode_line_info(LineBin)
	end,

	lists:foreach(fun(Op) ->
		%%io:format("~p~n", [Op]),
		validate(Op)
	end, Code),

	{ok,#m{mod_name=Module,
		   code =Code,
		   exports =Exports,
		   lambdas =Lambdas,
	   	   strings =StrTabBin,
	   	   attrs =Attrs,
	       compile_info =CInfs,
	   	   line_info =LineInfo,
		   abst_code =AbstCode}}.

optional_chunk(Beam, ChunkName) ->
	case beam_lib:chunks(Beam, [ChunkName]) of
	{error,beam_lib,{missing_chunk,_,_}} ->
		missing;
	{ok,{_,[{_,Chunk}]}} ->
		Chunk
	end.

decode_literals(<<_:32,Compressed/binary>>) ->
	<<_:32,Tab/binary>> = case catch zlib:uncompress(Compressed) of
	{'EXIT',_} -> %% BEAM uses noop zlib
		Compressed;
	X ->
		X
	end,
	decode_literals_1(Tab, 0).

decode_literals_1(<<Sz:32,Ext:Sz/binary,Rest/binary>>, Index) ->
	[{Index,binary_to_term(Ext)}|decode_literals_1(Rest, Index+1)];
decode_literals_1(<<>>, _) -> [].

decode_lambdas(<<_:32,Tab/binary>>, Atoms) ->
	decode_lambdas_1(Tab, Atoms, 0). %% NB: sorting order is not by Index

decode_lambdas_1(<<F:32,A:32,Lab:32,Index:32,NFree:32,OldUniq:32,More/binary>>,
		Atoms, OldIndex) ->
	Info = {lookup(F, Atoms),A,Lab,Index,NFree,OldUniq},
	[Info|decode_lambdas_1(More, Atoms, OldIndex+1)];
decode_lambdas_1(<<>>, _, _) -> [].

disasm_code(Code, St) ->
	disasm_code(Code, St, []).

disasm_code(<<>>, _St, Acc) ->
	lists:reverse(Acc);
disasm_code(<<OpCode,Code/binary>>, St, Acc) ->
	{OpName,Arity} = beam_opcodes:opname(OpCode),
	{Args,More} =  decode_args(Code, Arity, [], St),
	disasm_code(More, St, [decorate(OpName, Args, St)|Acc]).

decode_line_info(<<0:32,	%% Version
				   Flags:32,
				   NI:32,
				   NT:32,
				   NF:32,LBin/binary>>) ->
	decode_line_info(LBin, {Flags,NI,NT,NF}, 1, nofile, []).

decode_line_info(LBin, {_,_,NT,_}=D, T, CF, Acc) when T =< NT ->
	case decode_arg(LBin, nostate) of
	{{a,FileIndex},More} ->
		decode_line_info(More, D, T, FileIndex, Acc);
	{{i,Line},More} ->
		decode_line_info(More, D, T+1, CF, [{CF,Line}|Acc])
	end;
decode_line_info(LBin, D, _, _, Acc) ->
	Items = lists:reverse(Acc),
	decode_line_info2(LBin, D, Items, 1, []).

decode_line_info2(<<Sz:16,Name:Sz/binary,LBin/binary>>,
							{_,_,_,NF}=D, Items, F, Acc) when F =< NF ->
	NameStr = binary_to_list(Name),
	decode_line_info2(LBin, D, Items, F+1, [NameStr|Acc]);
decode_line_info2(_, {Flags,NI,_,_}, Items, _, Acc) ->
	Files = lists:reverse(Acc),
	{Flags,NI,Items,Files}.

decode_args(Code, 0, Acc, _St) ->
	{lists:reverse(Acc),Code};
decode_args(Code, N, Acc, St) ->
	{Arg,MoreCode} = decode_arg(Code, St),
	decode_args(MoreCode, N-1, [Arg|Acc], St).

decode_arg(<<0:4,0:1,?tag_z:3,Float:64/float,Code/binary>>, _St) ->
	{{float,Float},Code};
decode_arg(<<1:4,0:1,?tag_z:3,Code/binary>>, St) ->
	{Len,MoreCode} = decode_arg(Code, St),
	{List,EvenMore} = decode_list(MoreCode, Len, St),
	{{list,List},EvenMore};
decode_arg(<<2:4,0:1,?tag_z:3,Code/binary>>, St) ->
	{R,MoreCode} = decode_arg(Code, St),
	{{fr,R},MoreCode};
decode_arg(<<3:4,0:1,?tag_z:3,LenCode/binary>>, St) ->
	{Len = 2,Code} = decode_arg(LenCode, St),
	case decode_list(Code, 2*Len, St) of
	{[0,Words,1,Floats],MoreCode} ->
		{{alloc,[{words,Words},{floats,Floats}]},MoreCode};
	{[1,Floats,0,Words],MoreCode} ->
		{{alloc,[{words,Words},{floats,Floats}]},MoreCode}
	end;
decode_arg(<<4:4,0:1,?tag_z:3,Code/binary>>, St) ->
	{Index,MoreCode} = decode_arg(Code, St),
	{{literal,lookup(Index, St#da.literals)},MoreCode};

decode_arg(<<V:4,0:1,Tag:3,Code/binary>>, St) ->
	{decode_tag(Tag, V, St),Code};
decode_arg(<<V1:3,1:2,Tag:3,V2,Code/binary>>, St) ->
	{decode_tag(Tag, (V1 bsl 8) bor V2, St),Code};
decode_arg(<<31:5,Tag:3,Code/binary>>, St) ->
	{S,BytesCode} = decode_arg(Code, St),
	Size = S+9,
	<<Bytes:Size/binary,MoreCode/binary>> = BytesCode,
	{decode_tag(Tag, bytes_to_value(Bytes), St),MoreCode};
decode_arg(<<S:3,3:2,Tag:3,Code/binary>>, St) ->
	Size = S+2,
	<<Bytes:Size/binary,MoreCode/binary>> = Code,
	{decode_tag(Tag, bytes_to_value(Bytes), St),MoreCode}.

decode_tag(?tag_x, X, _St) -> {x,X};
decode_tag(?tag_y, Y, _St) -> {y,Y};
decode_tag(?tag_a, Index, nostate) -> {a,Index};
decode_tag(?tag_a, 0, _St) -> nil;
decode_tag(?tag_a, Index, St) -> {a,lookup(Index, St#da.atoms)};
decode_tag(?tag_i, I, _St) -> {i,I};
decode_tag(?tag_f, L, _St) -> {f,L};
decode_tag(?tag_u, N, _St) -> N.

bytes_to_value(<<Digit,_/binary>> = Bytes) when Digit > 127 ->
	negative_bytes_to_value(Bytes, 0, 1);
bytes_to_value(Bytes) ->
	bytes_to_value(Bytes, 0).

bytes_to_value(<<>>, Acc) ->
	Acc;
bytes_to_value(<<N,Bytes/binary>>, Acc) ->
	bytes_to_value(Bytes, (Acc bsl 8) bor N).

negative_bytes_to_value(<<>>, Acc, Z) ->
	Acc - Z;
negative_bytes_to_value(<<N,Bytes/binary>>, Acc, Z) ->
	negative_bytes_to_value(Bytes, (Acc bsl 8) bor N, (Z bsl 8)).

decode_list(Code, N, St) ->
	decode_list(Code, N, St, []).

decode_list(Code, 0, _St, Acc) ->
	{lists:reverse(Acc),Code};
decode_list(Code, N, St, Acc) ->
	{Arg,MoreCode} = decode_arg(Code, St),
	decode_list(MoreCode, N-1, St, [Arg|Acc]).

lookup(Key, List) ->
	{_, Val} = lists:keyfind(Key, 1, List),
	Val.

lookup_import(E, St) ->
	{_,M,F,A} = lists:keyfind(E+1, 1, St#da.imports),
	[{a,M},{a,F},A].

decorate(call_ext=Op, [_A,E], St) ->
	{Op,lookup_import(E, St)};
decorate(call_ext_last=Op, [_A,E,D], St) ->
	{Op,lookup_import(E, St) ++ [D]};
decorate(call_ext_only=Op, [_A,E], St) ->
	{Op,lookup_import(E, St)};

decorate(bif0=Op, [E,D], St) ->
	[M,F,A] = lookup_import(E, St),
	{Op,[M,F,A,D]};
decorate(bif1=Op, [Fail,E,S,D], St) ->
	[M,F,A] = lookup_import(E, St),
	{Op,[Fail,M,F,A,S,D]};
decorate(bif2=Op, [Fail,E,S1,S2,D], St) ->
	[M,F,A] = lookup_import(E, St),
	{Op,[Fail,M,F,A,S1,S2,D]};

decorate(gc_bif1=Op, [Fail,Live,E,S,D], St) ->
	[M,F,A] = lookup_import(E, St),
	{Op,[Fail,Live,M,F,A,S,D]};
decorate(gc_bif2=Op, [Fail,Live,E,S1,S2,D], St) ->
	[M,F,A] = lookup_import(E, St),
	{Op,[Fail,Live,M,F,A,S1,S2,D]};
decorate(gc_bif3=Op, [Fail,Live,E,S1,S2,S3,D], St) ->
	[M,F,A] = lookup_import(E, St),
	{Op,[Fail,Live,M,F,A,S1,S2,S3,D]};

%decorate(make_fun2=Op, [OldIndex], St) ->
%	{_,_,_,_,NumFree,_} = lookup(OldIndex, St#da.lambdas),
%	{Op,[OldIndex,NumFree]};

decorate(is_function2=Op,[{f,_}=F,{x,_}=X,{i,N}], _St) ->
	{Op,[F,X,N]};
decorate(is_function2=Op,[{f,_}=F,{y,_}=Y,{i,N}], _St) ->
	{Op,[F,Y,N]};

decorate(OpName, [], _St) ->
	OpName;
decorate(OpName, Args, _St) ->
	{OpName,Args}.

-define(U(X), (is_integer(X) andalso X >= 0)).

validate({allocate,[U1,U2]}) when ?U(U1), ?U(U2) -> ok;
validate({allocate_heap,[U1,{alloc,_},U2]}) when ?U(U1), ?U(U2) -> ok;
validate({allocate_heap,[U1,U2,U3]}) when ?U(U1), ?U(U2), ?U(U3) -> ok;
validate({allocate_heap_zero,[U1,{alloc,_},U2]}) when ?U(U1), ?U(U2) -> ok;
validate({allocate_heap_zero,[U1,U2,U3]}) when ?U(U1), ?U(U2), ?U(U3) -> ok;
validate({allocate_zero,[U1,U2]}) when ?U(U1), ?U(U2) -> ok;
validate({apply,[U]}) when ?U(U) -> ok;
validate({apply_last,[U1,U2]}) when ?U(U1), ?U(U2) -> ok;
validate({badmatch,[S]}) -> src(S);
validate({bif0,[{a,_},{a,_},U,D]}) when ?U(U) -> dst(D);
validate({bif1,[{f,_},{a,_},{a,_},U,S,D]}) when ?U(U) -> src(S), dst(D);
validate({bif2,[{f,_},{a,_},{a,_},U,S1,S2,D]}) when ?U(U) -> src(S1), src(S2), dst(D);
validate({bs_add,[{f,_},S1,S2,U,{x,_}]}) when ?U(U) -> src(S1), src(S2);
validate({bs_append,[{f,_},S,U1,U2,U3,_D,U4,{x,_}]}) when ?U(U1), ?U(U2), ?U(U3), ?U(U4) -> src(S); %%, dst(D);
validate({bs_context_to_binary,[D]}) -> dst(D);
validate({bs_get_binary2,[{f,_},{x,_},U1,S,U2,U3,{x,_}]}) when ?U(U1), ?U(U2), ?U(U3) -> src(S);
validate({bs_get_float2,[{f,_},{x,_},U1,S,U2,U3,{x,_}]}) when ?U(U1), ?U(U2), ?U(U3) -> src(S);
validate({bs_get_integer2,[{f,_},{x,_},U1,S,U2,U3,{x,_}]}) when ?U(U1), ?U(U2), ?U(U3) -> src(S);
validate({bs_get_utf16,[{f,_},{x,_},U1,U2,{x,_}]}) when ?U(U1), ?U(U2) -> ok;
validate({bs_get_utf32,[{f,_},{x,_},U1,U2,{x,_}]}) when ?U(U1), ?U(U2) -> ok;
validate({bs_get_utf8,[{f,_},{x,_},U1,U2,{x,_}]}) when ?U(U1), ?U(U2) -> ok;
validate({bs_init2,[{f,_},U1,U2,U3,U4,{x,_}]}) when ?U(U1), ?U(U2), ?U(U3), ?U(U4) -> ok;
validate({bs_init2,[{f,_},{x,_},U1,U2,U3,{x,_}]}) when ?U(U1), ?U(U2), ?U(U3) -> ok;
validate({bs_init_bits,[{f,_},U1,U2,U3,U4,{x,_}]}) when ?U(U1), ?U(U2), ?U(U3), ?U(U4) -> ok;
validate({bs_init_bits,[{f,_},{x,_},U1,U2,U3,{x,_}]}) when ?U(U1), ?U(U2), ?U(U3) -> ok;
validate(bs_init_writable) -> ok;
validate({bs_match_string,[{f,_},{x,_},U1,U2]}) when ?U(U1), ?U(U2) -> ok;
validate({bs_private_append,[{f,_},S,U1,D,U2,{x,_}]}) when ?U(U1), ?U(U2) -> src(S), dst(D);
validate({bs_put_binary,[{f,_},S1,U1,U2,S2]}) when ?U(U1), ?U(U2) -> src(S1), src(S2);
validate({bs_put_float,[{f,_},S1,U1,U2,S2]}) when ?U(U1), ?U(U2) -> src(S1), src(S2);
validate({bs_put_integer,[{f,_},S1,U1,U2,S2]}) when ?U(U1), ?U(U2) -> src(S1), src(S2);
validate({bs_put_string,[U1,U2]}) when ?U(U1), ?U(U2) -> ok;
validate({bs_put_utf16,[{f,_},U,D]}) when ?U(U) -> dst(D);
validate({bs_put_utf32,[{f,_},U,D]}) when ?U(U) -> dst(D);
validate({bs_put_utf8,[{f,_},U,D]}) when ?U(U) -> dst(D);
validate({bs_restore2,[{x,_},{a,_}]}) -> ok;
validate({bs_restore2,[{x,_},U]}) when ?U(U) -> ok;
validate({bs_save2,[{x,_},{a,_}]}) -> ok;
validate({bs_save2,[{x,_},U]}) when ?U(U) -> ok;
validate({bs_skip_bits2,[{f,_},{x,_},S,U1,U2]}) when ?U(U1), ?U(U2) -> src(S);
validate({bs_skip_utf16,[{f,_},{x,_},U1,U2]}) when ?U(U1), ?U(U2) -> ok;
validate({bs_skip_utf32,[{f,_},{x,_},U1,U2]}) when ?U(U1), ?U(U2) -> ok;
validate({bs_skip_utf8,[{f,_},{x,_},U1,U2]}) when ?U(U1), ?U(U2) -> ok;
validate({bs_start_match2,[{f,_},D,U1,U2,{x,_}]}) when ?U(U1), ?U(U2) -> dst(D);
validate({bs_test_tail2,[{f,_},{x,_},U]}) when ?U(U) -> ok;
validate({bs_test_unit,[{f,_},{x,_},U]}) when ?U(U) -> ok;
validate({bs_utf16_size,[{f,_},S,{x,_}]}) -> src(S);
validate({bs_utf8_size,[{f,_},S,{x,_}]}) -> src(S);
validate({call,[U,{f,_}]}) when ?U(U) -> ok;
validate({call_ext,[{a,_},{a,_},U]}) when ?U(U) -> ok;
validate({call_ext_last,[{a,_},{a,_},U1,U2]}) when ?U(U1), ?U(U2) -> ok;
validate({call_ext_only,[{a,_},{a,_},U]}) when ?U(U) -> ok;
validate({call_fun,[U]}) when ?U(U) -> ok;
validate({call_last,[U1,{f,_},U2]}) when ?U(U1), ?U(U2) -> ok;
validate({call_only,[U,{f,_}]}) when ?U(U) -> ok;
validate({case_end,[D]}) -> src(D);
validate({'catch',[{y,_},{f,_}]}) -> ok;
validate({catch_end,[{y,_}]}) -> ok;
validate({deallocate,[U]}) when ?U(U) -> ok;
validate({fadd,[{f,_},{fr,_},{fr,_},{fr,_}]}) -> ok;
validate({fcheckerror,[{f,_}]}) -> ok;
validate(fclearerror) -> ok;
validate({fconv,[S,{fr,_}]}) -> src(S);
validate({fdiv,[{f,_},{fr,_},{fr,_},{fr,_}]}) -> ok;
validate({fmove,[S,{fr,_}]}) -> src(S);
validate({fmove,[{fr,_},{fr,_}]}) -> ok;
validate({fmove,[{fr,_},D]}) -> dst(D);
validate({fmul,[{f,_},{fr,_},{fr,_},{fr,_}]}) -> ok;
validate({fnegate,[{f,_},{fr,_},{fr,_}]}) -> ok;
validate({fsub,[{f,_},{fr,_},{fr,_},{fr,_}]}) -> ok;
validate({func_info,[{a,_},{a,_},U]}) when ?U(U) -> ok;
validate({gc_bif1,[{f,_},U1,{a,_},{a,_},U2,S,D]}) when ?U(U1), ?U(U2) -> src(S), dst(D);
validate({gc_bif2,[{f,_},U1,{a,_},{a,_},U2,S1,S2,D]}) when ?U(U1), ?U(U2) -> src(S1), src(S2), dst(D);
validate({gc_bif3,[{f,_},U1,{a,_},{a,_},U2,S1,S2,S3,D]}) when ?U(U1), ?U(U2) -> src(S1), src(S2), src(S3), dst(D);
validate({get_list,[D1,D2,D3]}) -> dst(D1), dst(D2), dst(D3);
validate({get_tuple_element,[D1,U,D2]}) when ?U(U) -> dst(D1), dst(D2);
validate(if_end) -> ok;
validate({init,[{y,_}]}) -> ok;
validate(int_code_end) -> ok;
validate({is_atom,[{f,_},D]}) -> dst(D);
validate({is_binary,[{f,_},D]}) -> dst(D);
validate({is_bitstr,[{f,_},D]}) -> dst(D);
validate({is_boolean,[{f,_},D]}) -> dst(D);
validate({is_eq,[{f,_},S1,S2]}) -> src(S1), src(S2);
validate({is_eq_exact,[{f,_},S1,S2]}) -> src(S1), src(S2);
validate({is_float,[{f,_},D]}) -> dst(D);
validate({is_function,[{f,_},D]}) -> dst(D);
validate({is_function2,[{f,_},D,U]}) when ?U(U) -> dst(D);
validate({is_function2,[{f,_},D1,D2]}) -> dst(D1), dst(D2);
validate({is_ge,[{f,_},S1,S2]}) -> src(S1), src(S2);
validate({is_integer,[{f,_},D]}) -> dst(D);
validate({is_list,[{f,_},D]}) -> dst(D);
validate({is_lt,[{f,_},S1,S2]}) -> src(S1), src(S2);
validate({is_ne,[{f,_},S1,S2]}) -> src(S1), src(S2);
validate({is_ne_exact,[{f,_},S1,S2]}) -> src(S1), src(S2);
validate({is_nil,[{f,_},D]}) -> dst(D);
validate({is_nonempty_list,[{f,_},D]}) -> dst(D);
validate({is_number,[{f,_},D]}) -> dst(D);
validate({is_pid,[{f,_},D]}) -> dst(D);
validate({is_port,[{f,_},D]}) -> dst(D);
validate({is_reference,[{f,_},D]}) -> dst(D);
validate({is_tuple,[{f,_},D]}) -> dst(D);
validate({jump,[{f,_}]}) -> ok;
validate({label,[U]}) when ?U(U) -> ok;
validate({loop_rec,[{f,_},{x,_}]}) -> ok;
validate({loop_rec_end,[{f,_}]}) -> ok;
validate({make_fun2,[U]}) when ?U(U) -> ok;
validate({move,[S,D]}) -> src(S), dst(D);
validate(on_load) -> ok;
validate({put,[S]}) -> src(S);
validate({put_list,[S1,S2,D]}) -> src(S1), src(S2), dst(D);
validate({put_tuple,[U,D]}) when ?U(U) -> dst(D);
validate({raise,[D1,D2]}) -> dst(D1), dst(D2);
validate({recv_mark,[{f,_}]}) -> ok;
validate({recv_set,[{f,_}]}) -> ok;
validate(remove_message) -> ok;
validate(return) -> ok;
validate({select_tuple_arity,[D,{f,_},{list,_}]}) -> dst(D);
validate({select_val,[D,{f,_},{list,_}]}) -> dst(D);
validate(send) -> ok;
validate({set_tuple_element,[S,{x,_},U]}) when ?U(U) -> src(S);
validate({test_arity,[{f,_},D,U]}) when ?U(U) -> dst(D);
validate({test_heap,[{alloc,_},U]}) when ?U(U) -> ok;
validate({test_heap,[U1,U2]}) when ?U(U1), ?U(U2) -> ok;
validate(timeout) -> ok;
validate({trim,[U1,U2]}) when ?U(U1), ?U(U2) -> ok;
validate({'try',[{y,_},{f,_}]}) -> ok;
validate({try_case,[{y,_}]}) -> ok;
validate({try_case_end,[{x,_}]}) -> ok;
validate({try_end,[{y,_}]}) -> ok;
validate({wait,[{f,_}]}) -> ok;
validate({wait_timeout,[{f,_},S]}) -> src(S);

validate({is_map,[{f,_},S]}) -> src(S);
validate({put_map_assoc,[{f,_},S,D,_Live,{list,_}]}) -> src(S), dst(D);
validate({put_map_exact,[{f,_},S,D,_Live,{list,_}]}) -> src(S), dst(D);
validate({get_map_elements,[{f,_},S,{list,_}]}) -> src(S);
validate({has_map_fields,[{f,_},S,{list,_}]}) -> src(S);

validate({line,_}) -> ok;

validate(X) -> erlang:error(X).

%% a/float/i/literal/nil/x/y
src({a,_}) -> ok;
src({float,_}) -> ok;
src({i,_}) -> ok;
src({literal,_}) -> ok;
src(nil) -> ok;
src({x,_}) -> ok;
src({y,_}) -> ok;
src(X) -> erlang:error(X).

dst({x,_}) -> ok;
dst({y,_}) -> ok;
dst(X) -> erlang:error(X).

%%EOF
