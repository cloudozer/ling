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

-module(ling_code).
-export([beam_to_ling/1]).
-export([specialise_args/1]).
-export([transform/1]).
-export([ling_to_specs/1]).

-include("ling_code.hrl").

-define(MAX_INT_VALUE, ((1 bsl (32 -3 -1)) -1)).
-define(MIN_INT_VALUE, ((-1 bsl (32 -3 -1)))).
-define(IS_SMALL(X), ((X >= ?MIN_INT_VALUE) andalso (X =< ?MAX_INT_VALUE))). 
-define(FLOAT_HEAP_SIZE, 3).

-define(TMP_X, {x,255}).

-define(BSF_ALIGNED, 1).
-define(BSF_LITTLE, 2).
-define(BSF_SIGNED, 4).

-define(SMALL_BITS, (32 -3)).

-define(MAX_HEAP_BIN, 64).
-define(MAX_BYTE_SIZE, ((1 bsl (32 -3)) -1)).
-define(MAX_BIT_SIZE, (?MAX_BYTE_SIZE bsl 3)).

-define(NO_FPE_SIGNALS, false).

beam_to_ling(Bin) when is_binary(Bin) ->
	case ling_disasm:beam(Bin) of
	{ok,Module} ->
		beam_to_ling_1(Module);
	Err ->
		Err
	end;
beam_to_ling(File) ->
	case ling_disasm:beam(File) of
	{ok,Module} ->
		beam_to_ling_1(Module);
	Err ->
		Err
	end.

beam_to_ling_1(#m{code =GenAsm} =Module) ->
	SpecAsm = specialise_args(GenAsm),
	Asm = transform(SpecAsm),
	{ok,Module#m{code=Asm}}.

specialise_args(Code) ->
	lists:map(fun(Op) when is_atom(Op) ->
		Op;
	({Op,Args}) ->
		{Op,[spec_arg(Arg) || Arg <- Args]}
	end, Code).

spec_arg({x,255}) ->
   	erlang:error(x255);
spec_arg({x,1023}) ->
	{x,255};
spec_arg({i,I}) when ?IS_SMALL(I) ->
	{smallint,I};
spec_arg({i,I}) ->
	{bigint,I};
spec_arg({alloc,[{words,W},{floats,F}]}) ->
	W + ?FLOAT_HEAP_SIZE*F;
spec_arg({list,Ls}) ->
	{list,[spec_arg(L) || L <- Ls]};
spec_arg(Arg) ->
	Arg.

transform(Code) ->
	Code1 = transform_1(Code, [], true),
	[expand_list(Op) || Op <- Code1].

transform_1([], Acc, true) ->
	lists:reverse(Acc);
transform_1([], Acc, false) ->
	transform_1(lists:reverse(Acc), [], true);
transform_1([H|T] = Code, Acc, Intact) ->
	case tr(Code) of
	intact ->
		transform_1(T, [H|Acc], Intact);
	Code1 ->
		transform_1(Code1, Acc, false)
	end.

expand_list({Op,Args}) ->
	case lists:keytake(list, 1, Args) of
	{value,{list,Tab},Args1} ->
		{Op,Args1 ++ [length(Tab)],Tab};
	_ ->
		{Op,Args,[]}
	end;
expand_list(Op) when is_atom(Op) ->
	{Op,[],[]}.

tr([{bs_init2,[_,{x,_},_,_,_,_]=As}|Code]) ->
	[{bs_init2_1,As}|Code];
tr([{bs_init2,[_,_N,_,_,_,_]=As}|Code]) ->
	[{bs_init2_2,As}|Code];
tr([{bs_init_bits,[_,{x,_},_,_,_,_]=As}|Code]) ->
	[{bs_init_bits_1,As}|Code];
tr([{bs_init_bits,[_,_N,_,_,_,_]=As}|Code]) ->
	[{bs_init_bits_2,As}|Code];

tr([{fmove,[S,{fr,_}=D]}|Code]) ->
	[{fmove_1,[S,D]}|Code];
tr([{fmove,[{fr,_}=S,D]}|Code]) ->
	[{fmove_2,[S,D]}|Code];

tr([{init,[Y1]},{init,[Y2]},{init,[Y3]}|Code]) -> [{init3,[Y1,Y2,Y3]}|Code];
tr([{init,[Y1]},{init,[Y2]}|Code]) -> [{init2,[Y1,Y2]}|Code];

%%
%% select_vals are preceded by a typecheck
%%
%% +select_val (integers)
%% +select_val (atoms)
%% +select_val (floats)
%%
%% 1. +l_select_val_atoms (atoms) and remove typecheck
%% 2. l_select_val_smallints (smallints) and remove typecheck
%% 3. expand select_val (+bigints/+floats) into a series of comparisons
%%

tr([{is_atom,[F,S]},
		{select_val,[S,F,{list,[{a,_}|_]}]}=Select|Code]) -> [Select|Code];

tr([{is_integer,[F,S]},
	{select_val,[S,F,_]}=Select|Code]) -> [Select|Code];

tr([{is_float,[F,S]}=TypeCheck,{select_val,[S,F,{list,VLs}]}|Code]) ->
	[TypeCheck]
		++
	[{is_ne_exact,[L,S,V]} || {V,L} <- pairup(VLs)]
		++
	[{jump,[F]}|Code];

tr([{select_val,[_S,_F,{list,[{a,_}|_]}]=As}|Code]) ->

	%% NB: the list is not sorted for the binary search;
	%% for preloaded modules the sorting is done statically;
	%% for dynamic modules the values are sorted when the
	%% instruction is first hit

	[{l_select_val_atoms,As}|Code];

%% must be smallints/bigints
tr([{select_val,[S,F,{list,List}]=As}|Code]) ->
	case lists:partition(fun({{Tag,_},_}) ->
				   			Tag =:= smallint
				   		 end, pairup(List))	of
	{_,[]} ->
		[{l_select_val_smallints0,As}|Code];
	{[],L2} ->
		true = lists:all(fun({{Tag,_},_}) -> Tag =:= bigint end, L2),
		[{l_select_val_bigints0,As}|Code];
	{L1,L2} ->
		true = lists:all(fun({{Tag,_},_}) -> Tag =:= bigint end, L2),
		List1 = lists:concat([[V,L] || {V,L} <- L1]),
		List2 = lists:concat([[V,L] || {V,L} <- L2]),
		[{l_select_val_smallints0,[S,{f,0},{list,List1}]},
		 {l_select_val_bigints0,[S,F,{list,List2}]}|Code]
	end;

%% partitioning of integer select_val may result in a single item list

tr([{l_select_val_smallints0,[S,{f,0},{list,[V,L]}]}|Code]) ->
	[{is_ne_exact,[L,S,V]}|Code];
tr([{l_select_val_smallints0,[S,F,{list,[V,L]}]}|Code]) ->
	[{is_ne_exact,[L,S,V]},
	 {jump,[F]}|Code];

%% convert bigint and float select_vals into a series of comparisons

tr([{l_select_val_bigints0,[S,F,{list,[V,L]}]}|Code]) ->
	[{is_ne_exact,[L,S,V]},{jump,[F]}|Code];
tr([{l_select_val_bigints0,[S,F,{list,VLs}]}|Code]) ->
	[{is_bigint,[F,S]}]
		++
	[{is_ne_exact,[L,S,V]} || {V,L} <- pairup(VLs)]
		++
	[{jump,[F]}|Code];

%% special case of two pair select_val (atoms or small ints only), no typecheck

tr([{l_select_val_smallints0,[S,F,{list,[V1,L1,V2,L2]}]}|Code]) ->
	[{l_select_val2,[S,F,V1,L1,V2,L2]}|Code];
tr([{l_select_val_atoms,[S,F,{list,[{a,_}=V1,L1,V2,L2]}]}|Code]) ->
	[{l_select_val2,[S,F,V1,L1,V2,L2]}|Code];

%% special case for a jump table

tr([{l_select_val_smallints0,[S,F,{list,VLs}]}|Code]) ->
	Pairs = pairup(VLs),
	Is = [I || {{smallint,I},_} <- Pairs],
	Min = lists:min(Is),
	Max = lists:max(Is),
	UseJumpTable = Max - Min + 1 == length(Is),
	if UseJumpTable ->
		ILs = [{I,L} || {{smallint,I},L} <- Pairs],
		Ls = [L || {_,L} <- lists:keysort(1, ILs)],
		[{l_jump_on_val,[S,F,comp32(Min),{list,Ls}]}|Code];
	true ->

		%% NB: introducing a mild dependency on the term
		%% layout; the sorting assumes that the tagged
		%% integers follows the order of their unsigned 
		%% complements.

		{_,SortedPairs} = lists:unzip(lists:keysort(1,
				[{comp32(I),P}	|| {{smallint,I},_}=P <- Pairs])),

		Ls = lists:concat([[V,L] || {V,L} <- SortedPairs]),
		[{l_select_val_smallints,[S,F,{list,Ls}]}|Code]
	end;

%% is_function2 with literal arity

tr([{is_function2,[F,S,N]}|Code]) when is_integer(N) ->
	[{l_is_function2,[F,S,N]}|Code];

%% is_tuple before select_tuple_arity is redundant

tr([{is_tuple,[F,S]},{select_tuple_arity,[S,F,List]}|Code]) ->
	[{select_tuple_arity,[S,F,List]}|Code];

%% a special case of a two-pair select_tuple_arity

tr([{select_tuple_arity,[S,F,{list,[A1,L1,A2,L2]}]}|Code]) ->
	[{l_select_tuple_arity2,[S,F,A1,L1,A2,L2]}|Code];

%% sort the list for the rest of select_tuple_arity

tr([{select_tuple_arity,[S,F,{list,VLs}]}|Code]) ->
	Sorted = lists:concat([[V,L] || {V,L} <- lists:keysort(1, pairup(VLs))]),
	[{l_select_tuple_arity,[S,F,{list,Sorted}]}|Code];

%% redundant jumps

tr([{jump,[{f,L}]},{label,L}=Label|Code]) -> [Label|Code];

%% try/catch

tr([{'try',[Y,F]}|Code]) -> [{'catch',[Y,F]}|Code];
tr([{try_case,[Y]}|Code]) -> [{try_end,[Y]}|Code];

tr([{'catch',[Y,{f,L}]}|Code]) -> [{l_catch,[Y,{'catch',L}]}|Code];

tr([{move,[S,{x,0}]},{jump,[L]}|Code]) ->
	[{move_jump,[L,S]}|Code];

tr([{move,[{x,_}=X1,{y,_}=Y1]},{move,[{x,_}=X2,{y,_}=Y2]}|Code]) ->
	[{move2,[X1,Y1,X2,Y2]}|Code];

tr([{move,[{y,_}=Y1,{x,_}=X1]},{move,[{y,_}=Y2,{x,_}=X2]}|Code]) ->
	[{move2,[Y1,X1,Y2,X2]}|Code];

tr([{move,[{x,_}=X1,{x,_}=X2]},{move,[{x,_}=X3,{x,_}=X4]}|Code]) ->
	[{move2,[X1,X2,X3,X4]}|Code];

tr([{move,[nil,{y,_}=Y]}|Code]) -> [{init,[Y]}|Code];

%% extend the timeout value to 32-bits

tr([{wait_timeout,[F,{Tag,T}]}|Code]) when Tag =:= smallint; Tag =:= bigint ->
	[{l_wait_timeout,[F,comp32(T)]}|Code];

%% comparisons with an immediate operand

tr([{is_eq_exact,[F,S,{smallint,_}=C]}|Code]) ->
	[{l_is_eq_exact_immed,[F,S,C]}|Code];
tr([{is_eq_exact,[F,S,{a,_}=C]}|Code]) ->
	[{l_is_eq_exact_immed,[F,S,C]}|Code];
tr([{is_eq_exact,[F,S,nil]}|Code]) ->
	[{l_is_eq_exact_immed,[F,S,nil]}|Code];
tr([{is_eq_exact,[F,S,{literal,_}=Q]}|Code]) ->
	[{l_is_eq_exact_literal,[F,S,Q]}|Code];

tr([{is_ne_exact,[F,S,{smallint,_}=C]}|Code]) ->
	[{l_is_ne_exact_immed,[F,S,C]}|Code];
tr([{is_ne_exact,[F,S,{a,_}=C]}|Code]) ->
	[{l_is_ne_exact_immed,[F,S,C]}|Code];
tr([{is_ne_exact,[F,S,nil]}|Code]) ->
	[{l_is_ne_exact_immed,[F,S,nil]}|Code];
tr([{is_ne_exact,[F,S,{literal,_}=Q]}|Code]) ->
	[{l_is_ne_exact_literal,[F,S,Q]}|Code];

%% other comparisons

tr([{is_eq_exact,[F,S1,S2]}|Code]) ->
	[{l_fetch,[S1,S2]},{l_is_eq_exact,[F]}|Code];
tr([{is_ne_exact,[F,S1,S2]}|Code]) ->
	[{l_fetch,[S1,S2]},{l_is_ne_exact,[F]}|Code];

tr([{is_ge,[F,S1,S2]}|Code]) -> [{l_fetch,[S1,S2]},{l_is_ge,[F]}|Code];
tr([{is_lt,[F,S1,S2]}|Code]) -> [{l_fetch,[S1,S2]},{l_is_lt,[F]}|Code];
tr([{is_eq,[F,S1,S2]}|Code]) -> [{l_fetch,[S1,S2]},{l_is_eq,[F]}|Code];
tr([{is_ne,[F,S1,S2]}|Code]) -> [{l_fetch,[S1,S2]},{l_is_ne,[F]}|Code];

%% putting tuples

tr([{put_tuple,[_Arity,D]}|PutsCode]) ->
	{Puts,Code} = lists:splitwith(fun({put,_}) ->
					true; (_) -> false end,	PutsCode),
	Elems = [X || {put,[X]} <- Puts],
	[{l_put_tuple,[D,{list,Elems}]}|Code];

%% folded instructions

tr([{move,[S,{x,0}]},return|Code]) ->
	[{move_return,[S]}|Code];
tr([{move,[S,{x,0}]},{deallocate,[D]},return|Code]) ->
	[{move_deallocate_return,[S,D]}|Code];
tr([{deallocate,[D]},return|Code]) ->
	[{deallocate_return,[D]}|Code];

tr([{test_heap,[HN,1]},{put_list,[Y,{x,0},{x,0}]}|Code]) ->
	[{test_heap_1_put_list,[HN,Y]}|Code];

%% tuple tests (head)

tr([{is_tuple,[F,S]},{test_arity,[F,S,A]}|Code]) ->
	[{is_tuple_of_arity,[F,S,A]}|Code];

%%
%% converts
%% 	{test_arity,[F,S,A]}
%% 	{get_tuple_element,[S,0,{x,1}]}
%% 	{get_tuple_element,[S,1,{x,2}]}
%% 	{get_tuple_element,[S,2,{x,3}]}
%%
%% to
%%  {test_arity,[F,S,A]}
%%	{extract_next_element3,[{x,1}]}
%%
%% NB: destination is never {x,0} as r0 is not the part of register array
%%

tr([{is_tuple_of_arity,[_F,S,_A]}=Test,
	{get_tuple_element,[S,0,D]}|GetsCode]) when D =/= {x,0} ->
		extract_elements(Test, S, D, GetsCode);
tr([{test_arity,[_F,S,_A]}=Test,
	{get_tuple_element,[S,0,D]}|GetsCode]) when D =/= {x,0} ->
		extract_elements(Test, S, D, GetsCode);

%% other folds

tr([{is_integer,[F,{x,_}=S]},{allocate,[NS,_Live]}|Code]) ->
	[{is_integer_allocate,[F,S,NS]}|Code];

tr([{is_nonempty_list,[F,{x,_}=S]},{allocate,[NS,_Live]}|Code]) ->
	[{is_nonempty_list_allocate,[F,S,NS]}|Code];

tr([{is_nonempty_list,[F,{x,0}]},{test_heap,[HN,Live]}|Code]) ->
	[{is_nonempty_list_test_heap,[F,HN,Live]}|Code];

tr([{allocate,[NS,_Live]},{init,[Y]}|Code]) ->
	[{allocate_init,[NS,Y]}|Code];

%%
%% allocate slots does not need Live
%%

tr([{allocate,[NumSlots,_Live]}|Code]) -> [{l_allocate,[NumSlots]}|Code];
tr([{allocate_zero,[NumSlots,_Live]}|Code]) -> [{l_allocate_zero,[NumSlots]}|Code];

%% functions and BIFs...

tr([{call_ext,[{a,erlang},{a,apply},2]}|Code]) -> [l_apply_fun|Code];
tr([{call_ext_last,[{a,erlang},{a,apply},2,D]}|Code]) -> [{l_apply_fun_last,[D]}|Code];
tr([{call_ext_only,[{a,erlang},{a,apply},2]}|Code]) -> [l_apply_fun_only|Code];

tr([{call_ext,[{a,erlang},{a,apply},3]}|Code]) -> [l_apply|Code];
tr([{call_ext_last,[{a,erlang},{a,apply},3,D]}|Code]) -> [{l_apply_last,[D]}|Code];
tr([{call_ext_only,[{a,erlang},{a,apply},3]}|Code]) -> [l_apply_only|Code];

%% yield/0

tr([{call_ext,[{a,erlang},{a,yield},0]}|Code]) ->
	[l_yield|Code];
tr([{call_ext_last,[{a,erlang},{a,yield},0,D]}|Code]) ->
	[l_yield,{deallocate_return,[D]}|Code];
tr([{call_ext_only,[{a,erlang},{a,yield},0]}|Code]) ->
	[l_yield,return|Code];

%% hibernate/3

tr([{call_ext,[{a,erlang},{a,hibernate},3]}|Code]) ->
	[l_hibernate|Code];
tr([{call_ext_last,[{a,erlang},{a,hibernate},3,_D]}|Code]) ->
	[l_hibernate|Code];
tr([{call_ext_only,[{a,erlang},{a,hibernate},3]}|Code]) ->
	[l_hibernate|Code];

%% erlang:make_fun/3

tr([{move,[{a,F},{x,1}]},
	{move,[{smallint,A},{x,2}]},
	{move,[{a,M},{x,0}]},
	{call_ext,[{a,erlang},{a,make_fun},3]}|Code]) ->
		[{l_make_export,[{e,{M,F,A}}]}|Code];

%% same with intervening line instruction
tr([{move,[{a,F},{x,1}]},
	{move,[{smallint,A},{x,2}]},
	{move,[{a,M},{x,0}]},
	{line,_}=Line,
	{call_ext,[{a,erlang},{a,make_fun},3]}|Code]) ->
		[Line,{l_make_export,[{e,{M,F,A}}]}|Code];

%% the export_t reference is resolved in runtime
tr([{call_ext,[{a,erlang},{a,make_fun},3]}|Code]) ->
		[{l_make_export,[{e,0}]}|Code];

%% more folds; l_call_ext etc are not bif calls

tr([{move,[S,{x,0}]},{l_call_ext,[{e,MFA}]}|Code]) ->
	[{l_move_call_ext,[S,{e,MFA}]}|Code];
tr([{move,[S,{x,0}]},{l_call_ext_last,[{e,MFA},D]}|Code]) ->
	[{l_move_call_ext_last,[{e,MFA},D,S]}|Code];
tr([{move,[S,{x,0}]},{l_call_ext_only,[{e,MFA}]}|Code]) ->
	[{l_move_call_ext_only,[{e,MFA},S]}|Code];

%% same with intervening line instruction

tr([{move,[S,{x,0}]},{line,_}=Line,{l_call_ext,[{e,MFA}]}|Code]) ->
	[Line,{l_move_call_ext,[S,{e,MFA}]}|Code];
tr([{move,[S,{x,0}]},{line,_}=Line,{l_call_ext_last,[{e,MFA},D]}|Code]) ->
	[Line,{l_move_call_ext_last,[{e,MFA},D,S]}|Code];
tr([{move,[S,{x,0}]},{line,_}=Line,{l_call_ext_only,[{e,MFA}]}|Code]) ->
	[Line,{l_move_call_ext_only,[{e,MFA},S]}|Code];

%% BIFs

tr([{bif0,[{a,erlang},{a,self},0,D]}|Code]) -> [{self,[D]}|Code];
tr([{bif0,[{a,erlang},{a,node},0,D]}|Code]) -> [{node,[D]}|Code];

tr([{bif1,[_Fail,{a,erlang},{a,get},1,S,D]}|Code]) ->
	[{l_get,[S,D]}|Code];

tr([{bif2,[{f,0},{a,erlang},{a,element},2,{smallint,I},{XY,_}=T,D]}|Code])
		when I > 0, (XY =:= x orelse XY =:= y) ->
	[{l_fast_element,[T,I,D]}|Code];
tr([{bif2,[{f,0},{a,erlang},{a,element},2,Index,T,D]}|Code]) ->
	[{l_element,[T,Index,D]}|Code];

%% local calls

tr([{move,[S,{x,0}]},{call,[_Arity,Label]}|Code]) ->
	[{l_move_call,[S,Label]}|Code];
tr([{move,[S,{x,0}]},{call_last,[_Arity,Label,D]}|Code]) ->
	[{l_move_call_last,[Label,D,S]}|Code];
tr([{move,[S,{x,0}]},{call_only,[_Arity,Label]}|Code]) ->
	[{l_move_call_only,[Label,S]}|Code];

%% same with intervening line instruction

tr([{move,[S,{x,0}]},{line,_}=Line,{call,[_Arity,Label]}|Code]) ->
	[Line,{l_move_call,[S,Label]}|Code];
tr([{move,[S,{x,0}]},{line,_}=Line,{call_last,[_Arity,Label,D]}|Code]) ->
	[Line,{l_move_call_last,[Label,D,S]}|Code];
tr([{move,[S,{x,0}]},{line,_}=Line,{call_only,[_Arity,Label]}|Code]) ->
	[Line,{l_move_call_only,[Label,S]}|Code];

tr([{call,[_Arity,Label]}|Code]) -> [{l_call,[Label]}|Code];
tr([{call_last,[_Arity,Label,D]}|Code]) -> [{l_call_last,[Label,D]}|Code];
tr([{call_only,[_Arity,Label]}|Code]) -> [{l_call_only,[Label]}|Code];

%% fun calls

tr([{call_fun,[Arity]},{deallocate,[D]},return|Code]) ->
	[{l_call_fun_last,[Arity,D]}|Code];
tr([{call_fun,[Arity]}|Code]) ->
	[{l_call_fun,[Arity]}|Code];

tr([{make_fun2,[OldIndex]}|Code]) ->
	[{l_make_fun,[{fu,OldIndex}]}|Code];

%% binary matching

tr([{bs_start_match2,[F,Bin,X,Y,D]}|Code]) ->
	[{l_bs_start_match2,[Bin,F,X,Y,D]}|Code];

tr([{bs_save2,[R,{a,start}]}|Code]) -> [{l_bs_save2,[R,0]}|Code];
tr([{bs_save2,[R,I]}|Code]) -> [{l_bs_save2,[R,I+1]}|Code];
tr([{bs_restore2,[R,{a,start}]}|Code]) -> [{l_bs_restore2,[R,0]}|Code];
tr([{bs_restore2,[R,I]}|Code]) -> [{l_bs_restore2,[R,I+1]}|Code];

%% NB: changes unsigned argument into a string

tr([{bs_match_string,[Fail,Ms,Bits,Off]}|Code]) ->
	[{l_bs_match_string,[Ms,Fail,Bits,{str,Off}]}|Code];

tr([{bs_put_string,[Bytes,Off]}|Code]) ->
	[{l_bs_put_string,[Bytes,{str,Off}]}|Code];

tr([{bs_get_integer2,[Fail,Ms,_Live,{smallint,I},Unit,Flags,D]}|Code])
	when I*Unit =:= 8, Flags band ?BSF_SIGNED =:= 0 ->
		[{l_bs_get_integer_8,[Ms,Fail,D]}|Code];

tr([{bs_get_integer2,[Fail,Ms,_Live,{smallint,I},Unit,Flags,D]}|Code])
	when I*Unit =:= 16, Flags band ?BSF_SIGNED =:= 0,
   						Flags band ?BSF_LITTLE	=:= 0 ->
		[{l_bs_get_integer_16,[Ms,Fail,D]}|Code];

tr([{bs_get_integer2,[Fail,Ms,Live,{smallint,I},Unit,Flags,D]}|Code])
	when I*Unit =:= 32, Flags band ?BSF_SIGNED =:= 0,
   						Flags band ?BSF_LITTLE	=:= 0 ->
		[{l_bs_get_integer_32,[Ms,Fail,Live,D]}|Code];

tr([{bs_get_integer2,[Fail,Ms,_Live,{smallint,I},Unit,Flags,D]}|Code])
	when I*Unit < ?SMALL_BITS ->
		[{l_bs_get_integer_small_imm,[Ms,I*Unit,Fail,Flags,D]}|Code];
tr([{bs_get_integer2,[Fail,Ms,Live,{smallint,I},Unit,Flags,D]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_bs_get_integer_imm,[Ms,I*Unit,Live,Fail,Flags,D]}|Code];
tr([{bs_get_integer2,[Fail,Ms,Live,{bigint,I},Unit,Flags,D]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_bs_get_integer_imm,[Ms,I*Unit,Live,Fail,Flags,D]}|Code];
tr([{bs_get_integer2,[Fail,_Ms,_Live,{smallint,_},_Unit,_Flags,_D]}|Code]) ->
		[{system_limit,[Fail]}|Code];
tr([{bs_get_integer2,[Fail,_Ms,_Live,{bigint,_},_Unit,_Flags,_D]}|Code]) ->
		[{system_limit,[Fail]}|Code];

tr([{bs_get_integer2,[Fail,Ms,Live,Sz,Unit,Flags,D]}|Code]) ->
	[{l_fetch,[Ms,Sz]},{l_bs_get_integer,[Fail,Live,Unit,Flags,D]}|Code];

tr([{bs_get_binary2,[Fail,Ms,_Live,{a,all},Unit,_Flags,Ms]}|Code]) ->
	[{l_bs_get_binary_all_reuse,[Ms,Fail,Unit]}|Code];
tr([{bs_get_binary2,[Fail,Ms,Live,{a,all},Unit,_Flags,D]}|Code]) ->
	[{l_bs_get_binary_all2,[Fail,Ms,Live,Unit,D]}|Code];

tr([{bs_get_binary2,[Fail,Ms,Live,{smallint,I},Unit,Flags,D]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_bs_get_binary_imm2,[Fail,Ms,Live,I*Unit,Flags,D]}|Code];
tr([{bs_get_binary2,[Fail,Ms,Live,{bigint,I},Unit,Flags,D]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_bs_get_binary_imm2,[Fail,Ms,Live,I*Unit,Flags,D]}|Code];
tr([{bs_get_binary2,[Fail,_Ms,_Live,{smallint,_I},_Unit,_Flags,_D]}|Code]) ->
		[{system_limit,[Fail]}|Code];
tr([{bs_get_binary2,[Fail,_Ms,_Live,{bigint,_I},_Unit,_Flags,_D]}|Code]) ->
		[{system_limit,[Fail]}|Code];

tr([{bs_get_binary2,[Fail,Ms,Live,Sz,Unit,Flags,D]}|Code]) ->
	[{l_bs_get_binary2,[Fail,Ms,Live,Sz,Unit,Flags,D]}|Code];

tr([{bs_get_float2,[Fail,Ms,Live,Sz,Unit,Flags,D]}|Code]) ->
	[{l_bs_get_float2,[Fail,Ms,Live,Sz,Unit,Flags,D]}|Code];

tr([{bs_skip_bits2,[Fail,Ms,{a,all},Unit,_Flags]}|Code]) ->
	[{l_bs_skip_bits_all2,[Fail,Ms,Unit]}|Code];

tr([{bs_skip_bits2,[Fail,Ms,{smallint,I},Unit,_Flags]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_bs_skip_bits_imm2,[Fail,Ms,I*Unit]}|Code];
tr([{bs_skip_bits2,[Fail,Ms,{bigint,I},Unit,_Flags]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_bs_skip_bits_imm2,[Fail,Ms,I*Unit]}|Code];
tr([{bs_skip_bits2,[Fail,_Ms,{smallint,_I},_Unit,_Flags]}|Code]) ->
		[{system_limit,[Fail]}|Code];
tr([{bs_skip_bits2,[Fail,_Ms,{bigint,_I},_Unit,_Flags]}|Code]) ->
		[{system_limit,[Fail]}|Code];

tr([{bs_skip_bits2,[Fail,Ms,Sz,Unit,_Flags]}|Code]) ->
	[{l_bs_skip_bits2,[Fail,Ms,Sz,Unit]}|Code];

tr([{bs_test_tail2,[Fail,Ms,0]}|Code]) ->
	[{l_bs_test_zero_tail2,[Fail,Ms]}|Code];
tr([{bs_test_tail2,[Fail,Ms,N]}|Code]) when is_integer(N) ->
	[{l_bs_test_tail_imm2,[Fail,Ms,N]}|Code];

tr([{bs_test_unit,[Fail,Ms,8]}|Code]) ->
	[{l_bs_test_unit_8,[Fail,Ms]}|Code];
tr([{bs_test_unit,[Fail,Ms,Unit]}|Code]) ->
	[{l_bs_test_unit,[Fail,Ms,Unit]}|Code];

%% utf8/utf16/utf32 support

tr([{bs_get_utf8,[Fail,Ms,_Live,_Flags,D]}|Code]) ->
   	[{l_bs_get_utf8,[Ms,Fail,D]}|Code];
tr([{bs_skip_utf8,[Fail,Ms,_Live,_Flags]}|Code]) ->
   	[{l_bs_get_utf8,[Ms,Fail,?TMP_X]}|Code];

tr([{bs_get_utf16,[Fail,Ms,_,Flags,D]}|Code]) ->
	[{l_bs_get_utf16,[Ms,Fail,Flags,D]}|Code];
tr([{bs_skip_utf16,[Fail,Ms,_Live,Flags]}|Code]) ->
   	[{l_bs_get_utf16,[Ms,Fail,Flags,?TMP_X]}|Code];

tr([{bs_get_utf32,[Fail,Ms,Live,Flags,D]}|Code]) ->
	[{bs_get_integer2,[Fail,Ms,Live,{smallint,32},1,Flags,D]},
	 {l_fetch,[D,Ms]},
	 {l_bs_validate_unicode_retract,[Fail]}|Code];
tr([{bs_skip_utf32,[Fail,Ms,Live,Flags]}|Code]) ->
	[{bs_get_integer2,[Fail,Ms,Live,{smallint,32},1,Flags,?TMP_X]},
	 {l_fetch,[?TMP_X,Ms]},
	 {l_bs_validate_unicode_retract,[Fail]}|Code];

%% binary construction

tr([{bs_init2_2,[Fail,Sz,_Extra,_Live,_Flags,_D]}|Code])
	when is_integer(Sz), Sz > ?MAX_BYTE_SIZE ->
		[{system_limit,[Fail]}|Code];
tr([{bs_init2_2,[_Fail,Sz,Extra,Live,_Flags,D]}|Code])
	when is_integer(Sz), Sz =< ?MAX_HEAP_BIN ->
		[{l_bs_init_heap_bin,[Sz,Extra,Live,D]}|Code];
tr([{bs_init2_2,[_Fail,Sz,Extra,Live,_Flags,D]}|Code])
	when is_integer(Sz) ->
		[{l_bs_init,[Sz,Extra,Live,D]}|Code];
tr([{bs_init2_1,[Fail,Sz,Extra,Live,_Flags,D]}|Code]) ->
	[{l_fetch,[Sz,{x,0}]},{l_bs_init_fail,[Extra,Fail,Live,D]}|Code];

tr([{bs_init_bits_2,[Fail,Sz,_Extra,_Live,_Flags,_D]}|Code])
	when is_integer(Sz), Sz > ?MAX_BIT_SIZE ->
		[{system_limit,[Fail]}|Code];
tr([{bs_init_bits_2,[_Fail,Sz,Extra,Live,_Flags,D]}|Code])
	when is_integer(Sz) ->
		[{l_bs_init_bits,[Sz,Extra,Live,D]}|Code];
tr([{bs_init_bits_1,[Fail,Sz,Extra,Live,_Flags,D]}|Code]) ->
	[{l_fetch,[Sz,{x,0}]},{l_bs_init_bits_fail,[Extra,Fail,Live,D]}|Code];

tr([{bs_add,[_Fail,{smallint,0},S2,1,D]}|Code]) ->
   	[{move,[S2,D]}|Code];
tr([{bs_add,[Fail,S1,S2,Unit,D]}|Code]) ->
	[{l_fetch,[S1,S2]},{l_bs_add,[Fail,Unit,D]}|Code];

tr([{bs_append,[Fail,Size,Extra,Live,Unit,Bin,_Flags,D]}|Code]) ->
	[{l_fetch,[Size,Bin]},{l_bs_append,[Fail,Extra,Live,Unit,D]}|Code];
tr([{bs_private_append,[Fail,Size,Unit,Bin,_Flags,D]}|Code]) ->
	[{l_fetch,[Size,Bin]},{l_bs_private_append,[Fail,Unit,D]}|Code];

%% storing integers in binaries

tr([{bs_put_integer,[Fail,{smallint,I},Unit,Flags,Src]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_new_bs_put_integer_imm,[Fail,I*Unit,Flags,Src]}|Code];
tr([{bs_put_integer,[Fail,{bigint,I},Unit,Flags,Src]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_new_bs_put_integer_imm,[Fail,I*Unit,Flags,Src]}|Code];
tr([{bs_put_integer,[Fail,{smallint,_I},_Unit,_Flags,_Src]}|Code]) ->
		[{system_limit,[Fail]}|Code];
tr([{bs_put_integer,[Fail,{bigint,_I},_Unit,_Flags,_Src]}|Code]) ->
		[{system_limit,[Fail]}|Code];
tr([{bs_put_integer,[Fail,Sz,Unit,Flags,Src]}|Code]) ->
	%% NB: In BEAM the op has 4 arguments, Unit and Flags are combined
	[{l_new_bs_put_integer,[Fail,Sz,Unit,Flags,Src]}|Code];

tr([{bs_utf8_size,[_Fail,S,D]}|Code]) -> [{l_bs_utf8_size,[S,D]}|Code];
tr([{bs_utf16_size,[_Fail,S,D]}|Code]) -> [{l_bs_utf16_size,[S,D]}|Code];

tr([{bs_put_utf8,[Fail,_Flags,S]}|Code]) -> [{l_bs_put_utf8,[Fail,S]}|Code];
tr([{bs_put_utf16,[Fail,Flags,S]}|Code]) -> [{l_bs_put_utf16,[Fail,Flags,S]}|Code];
tr([{bs_put_utf32,[Fail,Flags,S]}|Code]) ->
	[{l_bs_validate_unicode,[Fail,S]},
	 {bs_put_integer,[Fail,{smallint,32},1,Flags,S]}|Code];

%% storing floats in binaries

tr([{bs_put_float,[Fail,{smallint,I},Unit,Flags,Src]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_new_bs_put_float_imm,[Fail,I*Unit,Flags,Src]}|Code];
tr([{bs_put_float,[Fail,{bigint,I},Unit,Flags,Src]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_new_bs_put_float_imm,[Fail,I*Unit,Flags,Src]}|Code];
tr([{bs_put_float,[Fail,{smallint,_I},_Unit,_Flags,_Src]}|Code]) ->
		[{system_limit,[Fail]}|Code];
tr([{bs_put_float,[Fail,{bigint,_I},_Unit,_Flags,_Src]}|Code]) ->
		[{system_limit,[Fail]}|Code];
tr([{bs_put_float,[Fail,Sz,Unit,Flags,Src]}|Code]) ->
	%% NB: In BEAM the op has 4 arguments, Unit and Flags are combined
	[{l_new_bs_put_float,[Fail,Sz,Unit,Flags,Src]}|Code];

%% storing binaries in binaries

tr([{bs_put_binary,[Fail,{a,all},Unit,_Flags,Src]}|Code]) ->
	[{l_new_bs_put_binary_all,[Fail,Src,Unit]}|Code];
tr([{bs_put_binary,[Fail,{smallint,I},Unit,Flags,Src]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_new_bs_put_binary_imm,[Fail,I*Unit,Flags,Src]}|Code];
tr([{bs_put_binary,[Fail,{bigint,I},Unit,Flags,Src]}|Code])
	when I*Unit =< ?MAX_BIT_SIZE ->
		[{l_new_bs_put_binary_imm,[Fail,I*Unit,Flags,Src]}|Code];
tr([{bs_put_binary,[Fail,{smallint,_I},_Unit,_Flags,_Src]}|Code]) ->
		[{system_limit,[Fail]}|Code];
tr([{bs_put_binary,[Fail,{bigint,_I},_Unit,_Flags,_Src]}|Code]) ->
		[{system_limit,[Fail]}|Code];
tr([{bs_put_binary,[Fail,Sz,Unit,Flags,Src]}|Code]) ->
	%% NB: In BEAM the op has 4 arguments, Unit and Flags are combined
	[{l_new_bs_put_binary,[Fail,Sz,Unit,Flags,Src]}|Code];

%% floating-point arithmetics

tr([{fadd,[{f,0},F1,F2,F3]}|Code]) -> [{l_fadd,[F1,F2,F3]}|Code];
tr([{fsub,[{f,0},F1,F2,F3]}|Code]) -> [{l_fsub,[F1,F2,F3]}|Code];
tr([{fmul,[{f,0},F1,F2,F3]}|Code]) -> [{l_fmul,[F1,F2,F3]}|Code];
tr([{fdiv,[{f,0},F1,F2,F3]}|Code]) -> [{l_fdiv,[F1,F2,F3]}|Code];
tr([{fnegate,[{f,0},F1,F2]}|Code]) -> [{l_fnegate,[F1,F2]}|Code];

%%tr([{fcheckerror,[{f,0}]}|Code]) when ?NO_FPE_SIGNALS -> Code;
%%tr([fclearerror|Code]) when ?NO_FPE_SIGNALS -> Code;

tr([{fcheckerror,[{f,0}]}|Code]) -> [l_fcheckerror|Code];

%% arithmetic operations

tr([{gc_bif2,[{f,0},Live,{a,erlang},{a,'+'},2,{smallint,I},{x,_}=X,D]}|Code]) ->
	[{l_increment,[X,comp32(I),Live,D]}|Code];
tr([{gc_bif2,[{f,0},Live,{a,erlang},{a,'+'},2,{smallint,I},{y,_}=Y,D]}|Code]) ->
	[{l_increment,[Y,comp32(I),Live,D]}|Code];
tr([{gc_bif2,[{f,0},Live,{a,erlang},{a,'+'},2,{x,_}=X,{smallint,I},D]}|Code]) ->
	[{l_increment,[X,comp32(I),Live,D]}|Code];
tr([{gc_bif2,[{f,0},Live,{a,erlang},{a,'+'},2,{y,_}=Y,{smallint,I},D]}|Code]) ->
	[{l_increment,[Y,comp32(I),Live,D]}|Code];

tr([{gc_bif2,[{f,0},Live,{a,erlang},{a,'-'},2,{x,_}=X,{smallint,I},D]}|Code]) ->
	[{l_increment,[X,comp32(-I),Live,D]}|Code];
tr([{gc_bif2,[{f,0},Live,{a,erlang},{a,'-'},2,{y,_}=Y,{smallint,I},D]}|Code]) ->
	[{l_increment,[Y,comp32(-I),Live,D]}|Code];

tr([{gc_bif2,[Fail,Live,{a,erlang},{a,'+'},2,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
	 {l_plus,[Fail,Live,D]}|Code];
tr([{gc_bif2,[Fail,Live,{a,erlang},{a,'-'},2,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
	 {l_minus,[Fail,Live,D]}|Code];
tr([{gc_bif2,[Fail,Live,{a,erlang},{a,'*'},2,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
	 {l_times,[Fail,Live,D]}|Code];
tr([{gc_bif2,[Fail,Live,{a,erlang},{a,'/'},2,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
	 {l_m_div,[Fail,Live,D]}|Code];
tr([{gc_bif2,[Fail,Live,{a,erlang},{a,'div'},2,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
	 {l_int_div,[Fail,Live,D]}|Code];
tr([{gc_bif2,[Fail,Live,{a,erlang},{a,'rem'},2,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
	 {l_rem,[Fail,Live,D]}|Code];
tr([{gc_bif2,[Fail,Live,{a,erlang},{a,'bsl'},2,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
	 {l_bsl,[Fail,Live,D]}|Code];
tr([{gc_bif2,[Fail,Live,{a,erlang},{a,'bsr'},2,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
	 {l_bsr,[Fail,Live,D]}|Code];
tr([{gc_bif2,[Fail,Live,{a,erlang},{a,'band'},2,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
	 {l_band,[Fail,Live,D]}|Code];
tr([{gc_bif2,[Fail,Live,{a,erlang},{a,'bor'},2,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
	 {l_bor,[Fail,Live,D]}|Code];
tr([{gc_bif2,[Fail,Live,{a,erlang},{a,'bxor'},2,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
	 {l_bxor,[Fail,Live,D]}|Code];

tr([{gc_bif1,[Fail,Live,{a,erlang},{a,'bnot'},1,S,D]}|Code]) ->
	[{l_int_bnot,[Fail,S,Live,D]}|Code];

tr([{gc_bif1,[Fail,Live,{a,erlang},{a,'-'},1,S,D]}|Code]) ->
	[{l_fetch,[{smallint,0},S]},
	 {l_minus,[Fail,Live,D]}|Code];
tr([{gc_bif1,[Fail,Live,{a,erlang},{a,'+'},1,S,D]}|Code]) ->
	[{l_fetch,[{smallint,0},S]},
	 {l_plus,[Fail,Live,D]}|Code];

%% obscure receive marks

tr([{recv_set,[{f,Mark}]},{label,[Mark]}=Label,{loop_rec,_}=LoopRec|Code]) ->
	[l_recv_set,Label,LoopRec|Code];

%% message passing

tr([{loop_rec,[Fail,{x,X}]}|Code]) -> 0 = X, [{l_loop_rec,[Fail]}|Code];

%% {b,_} and {e,_}

%% tr([{bif0,[{a,M},{a,F},N,D]}|Code]) ->
%%    	[{l_bif0,[{b,{M,F,N}},D]}|Code];
tr([{bif1,[{f,0},{a,M},{a,F},N,S,D]}|Code]) ->
   	[{bif1_body,[{b,{M,F,N}},S,D]}|Code];
tr([{bif1,[Fail,{a,M},{a,F},N,S,D]}|Code]) ->
   	[{l_bif1,[Fail,{b,{M,F,N}},S,D]}|Code];
tr([{bif2,[{f,0},{a,M},{a,F},N,S1,S2,D]}|Code]) ->
   	[{l_fetch,[S1,S2]},{bif2_body,[{b,{M,F,N}},D]}|Code];
tr([{bif2,[Fail,{a,M},{a,F},N,S1,S2,D]}|Code]) ->
   	[{l_fetch,[S1,S2]},{l_bif2,[Fail,{b,{M,F,N}},D]}|Code];

tr([{gc_bif1,[Fail,L,{a,M},{a,F},N,S,D]}|Code]) ->
	[{l_gc_bif1,[Fail,{b,{M,F,N}},S,L,D]}|Code];
tr([{gc_bif2,[Fail,L,{a,M},{a,F},N,S1,S2,D]}|Code]) ->
	[{l_fetch,[S1,S2]},
   	 {l_gc_bif2,[Fail,{b,{M,F,N}},L,D]}|Code];
tr([{gc_bif3,[Fail,Live,{a,M},{a,F},A,S1,S2,S3,D]}|Code]) ->
	Bif = {b,{M,F,A}},
	[{move,[S1,?TMP_X]},
	 {l_fetch,[S2,S3]},
	 {l_gc_bif3,[Fail,Bif,?TMP_X,Live,D]}|Code];

%%
%% TODO: do not add deallocate_return after call_bif's that never return;
%% this may make stack tracing a bit trickier
%%

tr([{call_ext,[{a,M},{a,F},N]}|Code]) ->
	case ling_bifs:is_builtin(M, F, N) of
	true ->
		[{call_bif,[{b,{M,F,N}}]}|Code];
	false ->
		[{l_call_ext,[{e,{M,F,N}}]}|Code]
	end;
tr([{call_ext_last,[{a,M},{a,F},N,D]}|Code]) ->
	case ling_bifs:is_builtin(M, F, N) of
	true ->
		[{call_bif,[{b,{M,F,N}}]},{deallocate_return,[D]}|Code];
	false ->
		[{l_call_ext_last,[{e,{M,F,N}},D]}|Code]
	end;
tr([{call_ext_only,[{a,M},{a,F},N]}|Code]) ->
	case ling_bifs:is_builtin(M, F, N) of
	true ->
		[{allocate,[0,N]},{call_bif,[{b,{M,F,N}}]},{deallocate_return,[0]}|Code];
	false ->
		[{l_call_ext_only,[{e,{M,F,N}}]}|Code]
	end;

tr([{trim,[N,_R]}|Code]) -> [{l_trim,[N]}|Code];	%% 1

%% Maps
tr([{put_map_assoc,[{f,0},{literal,#{}},D,_Live,L]}|Code]) ->
	[{new_map,[D,L]}|Code];
tr([{put_map_assoc,[{f,0},{x,_}=S,D,_Live,L]}|Code]) ->
	[{update_map_assoc,[S,D,L]}|Code];
tr([{put_map_assoc,[{f,0},{y,_}=S,D,_Live,L]}|Code]) ->
	[{update_map_assoc,[S,D,L]}|Code];
tr([{put_map_assoc,[{f,0},S,D,_Live,L]}|Code]) ->
	[{move,[S,?TMP_X]},{update_map_assoc,[?TMP_X,D,L]}|Code];

tr([{put_map_exact,[{f,0},{literal,#{}},D,_Live,L]}|Code]) ->
	[{new_map,[D,L]}|Code];
tr([{put_map_exact,[F,{x,_}=S,D,_Live,L]}|Code]) ->
	[{update_map_exact,[F,S,D,L]}|Code];
tr([{put_map_exact,[F,{y,_}=S,D,_Live,L]}|Code]) ->
	[{update_map_exact,[F,S,D,L]}|Code];
tr([{put_map_exact,[F,S,D,_Live,L]}|Code]) ->
	[{move,[S,?TMP_X]},{update_map_exact,[F,?TMP_X,D,L]}|Code];

tr([{get_map_elements,[F,S,{list,[K,D]}]}|Code]) ->
	tr([{get_map_element,[F,S,K,D]}|Code]);

tr([{has_map_fields,[F,S,{list,[K]}]}|Code]) ->
	tr([{has_map_field,[F,S,K]}|Code]);

tr(_) -> intact.

pairup([]) -> [];
pairup([V,L|VLs]) ->
	[{V,L}|pairup(VLs)].

%%
%%	TODO
%%
%%	This may be enhanced by continuing replacing get_tuple_element
%%	with extract_next_elementN as the pointer in tmp_arg1 is still
%%	valid after extract_next_elementN. The source should be the same.
%%
%%	Example:
%% 		{is_tuple_of_arity,[{f,9},{x,0},3],[]}
%%		{extract_next_element,[{y,0}],[]}
%%		{get_tuple_element,[{x,0},1,{x,2}],[]}	-> convert
%%		{get_tuple_element,[{x,0},2,{x,1}],[]}	-> convert
%%

extract_elements(Test, S, D, GetsCode) ->
	case extract_elements_1(S, D, GetsCode) of
	{1,Code} ->
		[Test,{extract_next_element,[D]}|Code];
	{2,Code} ->
		[Test,{extract_next_element2,[D]}|Code];
	{3,Code} ->
		[Test,{extract_next_element3,[D]}|Code]
	end.

extract_elements_1(S, {XY,D}, GetsCode) ->
	extract_elements_1(S, {XY,D+1}, GetsCode, 1).

extract_elements_1(S, {XY,D}, [{get_tuple_element,[S,P,{XY,D}]}|Code], P)
	when P < 3 ->
		extract_elements_1(S, {XY,D+1}, Code, P+1);
extract_elements_1(_, _, Code, P) ->
	{P,Code}.

comp32(I) when I >= 0, I < (1 bsl 31) ->
	I;
comp32(I) when I < 0, I >= -(1 bsl 31) ->
	(1 bsl 32) + I.

-record(lts, {labels,atoms,literals,catches,imports}).

ling_to_specs(Ling) -> %% {ok,#m{}}
	{Labels,Lines} = label_line_offsets(Ling),

	{NewLineInfo,FileNameAtoms} = case Ling#m.line_info of
	undefined ->
		{undefined,[]};
	{_,_,LineItems,FileNames} ->
		As = [list_to_atom(FN) || FN <- FileNames],

		LIs = lists:map(fun({Off,0}) ->
		{Off,undefined};
			({Off,N}) ->
		{F,L} = lists:nth(N, LineItems),
			{Off,{F,L}}
		end, Lines),

		{{LIs,As},As}
	end,

	SpecsNoInd = lists:map(fun({Op,Args,Trail}) ->
		No = ling_iopvars:fit_args(Op, Args),
		Types = ling_iopvars:var_args(Op, No),

		[{opcode,ling_iopvars:var_index(Op, No)}]
				 ++ spec(Args, Types)
				 ++ spec_trail(Trail)

	end, [Trio || {Op,_,_}=Trio <- Ling#m.code, Op =/= label, Op =/= line]),

	AllArgs = lists:concat(SpecsNoInd),

	%% NB: the standart usort is using == and thus
	%% coalesces floating-point literals with integers
	%%
	EqualsExact = fun(A, B) -> A =:= B end,

	Literals = lists:usort(EqualsExact,
						   [Q || {literal,Q} <- AllArgs]
									++
						   [F || {float,F} <- AllArgs]
									++
						   [I || {bigint,I} <- AllArgs]),
	Catches = lists:usort([L || {'catch',L} <- AllArgs]),
	Imports = lists:usort([MFA || {b,MFA} <- AllArgs])
									++
			  lists:usort([MFA || {e,{_,_,_}=MFA} <- AllArgs]),

	Atoms = lists:usort([A || {a,A} <- AllArgs]
				++ lists:concat([[M,F] || {M,F,_} <- Imports])
				++ FileNameAtoms),

	AtomsDict = index_dict(Atoms),
	LiteralsDict = index_dict(Literals),
	CatchesDict = index_dict(Catches),
	ImportsDict = index_dict(Imports),

	Lts = #lts{labels=Labels,
			   atoms=AtomsDict,
			   literals=LiteralsDict,
			   catches=CatchesDict,
			   imports=ImportsDict},
	
	Specs = [indexify(S, Lts) || S <- SpecsNoInd],

	Funs = [{Name,Arity,dict:fetch(F, Labels),Index,NumFree,Uniq}
				|| {Name,Arity,F,Index,NumFree,Uniq} <- Ling#m.lambdas],
	Exps = [{Function,Arity,dict:fetch(F, Labels)}
				|| {Function,Arity,F} <- Ling#m.exports],
	Cats = [dict:fetch(L, Labels)
				|| L <- Catches],

	{ok,Ling#m{code=Specs,
			   lambdas=Funs,
			   exports=Exps,
			   catches=Cats,
		  	   atoms=Atoms,
		   	   imports=Imports,
		       literals=Literals,
		   	   line_info=NewLineInfo}}.

label_line_offsets(Ling) -> %% {LabDict,LineDict,CodeSize}
	{Labels,Lines,_} = lists:foldl(fun({line,[N],_}, {Labels,Lines,Off}) ->
		{Labels,[{Off,N}|Lines],Off};
	
	({label,[L],_}, {Labels,Lines,Off}) -> 
		{dict:store(L, Off, Labels),Lines,Off};

	({Op,Args,Trail}, {Labels,Lines,Off}) ->
		No = ling_iopvars:fit_args(Op, Args),
		Types = ling_iopvars:var_args(Op, No),
		{Labels,Lines,Off +1 + ling_iops:wsize(Types) + length(Trail)}	%% +1 for opcode
	end, {dict:new(),[],0}, Ling#m.code),

	{Labels,lists:reverse(Lines)}.

index_dict(Ls) ->
	{D,_} = lists:foldl(fun(L, {D,N}) ->
		{dict:store(L, N, D),N+1}
	end, {dict:new(),0}, Ls),
	D.

indexify(As, Lts) ->
	lists:map(fun({f,0}) ->
		{f,none};
	({f,F}) ->
		{f,dict:fetch(F, Lts#lts.labels)};
	({a,A}) ->
		{atom,dict:fetch(A, Lts#lts.atoms)};
	({literal,Q}) ->
		{literal,dict:fetch(Q, Lts#lts.literals)};
	({e,0}) ->
		{export,none};
	({e,MFA}) ->
		{export,dict:fetch(MFA, Lts#lts.imports)};
	({b,MFA}) ->
		{bif,dict:fetch(MFA, Lts#lts.imports)};
	({'catch',L}) ->
		{'catch',dict:fetch(L, Lts#lts.catches)};
	(X) ->
		X
	end, As).

spec(As, Ps) ->
	spec(As, Ps, [], []).

spec([], [], WWs, BWs) ->
	lists:reverse(WWs) ++ [bytes(Bs) || Bs <- lists:reverse(BWs)];
spec([Val|As], [Val|Ps], WWs, BWs) ->
	spec(As, Ps, WWs, BWs);
spec([N|As], [{u,N}|Ps], WWs, BWs) ->
	spec(As, Ps, WWs, BWs);
spec([N|As], [{u8,N}|Ps], WWs, BWs) ->
	spec(As, Ps, WWs, BWs);
spec([{smallint,I}|As], [t|Ps], WWs, BWs) ->
	spec(As, Ps, [{tag_int,I}|WWs], BWs);
spec([{bigint,I}|As], [t|Ps], WWs, BWs) -> %% bigints are now just literals
	spec(As, Ps, [{literal,I}|WWs], BWs);
spec([{float,F}|As], [t|Ps], WWs, BWs) -> %% floats are now just literals
	spec(As, Ps, [{literal,F}|WWs], BWs);
spec([U|As], [u8|Ps], WWs, BWs) when is_integer(U), U >= 0, U =< 255 ->
	spec(As, Ps, WWs, add_byte(BWs, U));
spec([U|As], [u32|Ps], WWs, BWs) when is_integer(U), U >= 0 ->
	spec(As, Ps, [U|WWs], BWs);
spec([{x,X}|As], [x8|Ps], WWs, BWs) ->
	spec(As, Ps, WWs, add_byte(BWs, X));
spec([{x,X}|As], [t|Ps], WWs, BWs) ->
	spec(As, Ps, [{reg_as_term,X}|WWs], BWs);
spec([{fr,R}|As], [fr|Ps], WWs, BWs) ->
	spec(As, Ps, WWs, add_byte(BWs, R));
spec([{y,Y}|As], [y8|Ps], WWs, BWs) ->
	spec(As, Ps, WWs, add_byte(BWs, Y));
spec([{y,Y}|As], [t|Ps], WWs, BWs) ->
	spec(As, Ps, [{slot_as_term,Y}|WWs], BWs);
spec([nil|As], [t|Ps], WWs, BWs) ->
	spec(As, Ps, [nil|WWs], BWs);
spec([{f,none}=F|As], [f|Ps], WWs, BWs) ->
	spec(As, Ps, [F|WWs], BWs);
spec([{f,Off}|As], [f|Ps], WWs, BWs) ->
	spec(As, Ps, [{f,Off}|WWs], BWs);
spec([{e,_}=Exp|As], [e|Ps], WWs, BWs) ->
	spec(As, Ps, [Exp|WWs], BWs);
spec([{b,_}=Bif|As], [b|Ps], WWs, BWs) ->
	spec(As, Ps, [Bif|WWs], BWs);
spec([{fu,_}=Fu|As], [fu|Ps], WWs, BWs) ->
	spec(As, Ps, [Fu|WWs], BWs);
spec([{str,_}=Str|As], [str|Ps], WWs, BWs) ->
	spec(As, Ps, [Str|WWs], BWs);
spec([{'catch',_}=Cc|As], [t|Ps], WWs, BWs) ->
	spec(As, Ps, [Cc|WWs], BWs);
spec([{literal,_}=Lit|As], [t|Ps], WWs, BWs) ->
	spec(As, Ps, [Lit|WWs], BWs);
spec([{a,_}=Atom|As], [t|Ps], WWs, BWs) ->
	spec(As, Ps, [Atom|WWs], BWs).

spec_trail(As) ->
	lists:map(fun({a,_}=A) -> A;
				 ({smallint,I}) -> {tag_int,I};
				 ({bigint,I}) -> {literal,I};
				 ({float,F}) -> {literal,F};
				 ({f,_Off}=F) -> F;
				 ({x,X}) -> {reg_as_term,X};
				 ({y,Y}) -> {slot_as_term,Y};
				 ({literal,_}=Lit) -> Lit;
				 (nil) -> nil;
				 (U) when is_integer(U), U >= 0 -> U
	end, As).

bytes([B]) -> B;
bytes([B1,B2]) -> (B1 bsl 8) bor B2;
bytes([B1,B2,B3]) -> (B1 bsl 16) bor (B2 bsl 8) bor B3;
bytes([B1,B2,B3,B4]) -> (B1 bsl 24) bor (B2 bsl 16) bor (B3 bsl 8) bor B4.

add_byte([], Name) ->
	[[Name]];
add_byte([[_,_,_,_]|_]=Words, Name) ->
	[[Name]|Words];
add_byte([Bs|Words], Name) ->
	[[Name|Bs]|Words].

