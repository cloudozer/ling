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

-module(ling_iops).
-export([packing_options/1,broadest_packing/1]).
-export([packing_tags/1,wsize/1]).

broadest_packing(Types) ->
	[broad(T) || T <- Types].

broad(u) -> u32;
broad(u8) -> u8;
broad(a) -> t;
broad(x) -> t;
broad(y) -> t;
broad(d) -> t;
broad(s) -> t;
broad(f) -> f;
broad(e) -> e;
broad(fu) -> fu;
broad(str) -> str;
broad(ca) -> t;
broad(b) -> b;
broad(fr) -> fr.

packing_options(Args) ->
	generate(fun arg_rep/1, Args).

generate(_, []) ->
	[[]];
generate(F, [A|As]) ->
	[[G|Gs] || G <- F(A), Gs <- generate(F, As)].

%% NB: the broadest case should be in the end
arg_rep({u8,N}) when is_integer(N), N >= 0, N =< 255 -> [u8];
arg_rep(N) when is_integer(N), N >= 0, N =< 255 -> [u8,u32];
arg_rep(N) when is_integer(N), N >= 0 -> [u32];
arg_rep({a,_}) -> [t];
arg_rep({smallint,I}) when I >= -128, I =< 127 -> [i8,t];
arg_rep({smallint,_I}) -> [t];
arg_rep({bigint,_I}) -> [t];
arg_rep(nil) -> [t];
arg_rep({x,X}) when X =< 255 -> [x8,t];
arg_rep({x,_X}) -> [t];
arg_rep({y,Y}) when Y =< 255 -> [y8,t];
arg_rep({y,_Y}) -> [t];
arg_rep({f,_}) -> [f];
arg_rep({float,_}) -> [t];
arg_rep({fr,_}) -> [fr];
arg_rep({literal,_}) -> [t];
arg_rep({e,_}) -> [e];
arg_rep({bif,_}) -> [b].

bs(u8) -> 8;
bs(u32) -> 32;
bs(i8) -> 8;
bs(x8) -> 8;
bs(y8) -> 8;
bs(t) -> 32;
bs(f) -> 32;
bs(e) -> 32;
bs(b) -> 32;
bs(fr) -> 8;
bs(fu) -> 32;
bs(str) -> 32;
bs(nil) -> 0;
bs({_,_}) -> 0.

wsize(RRR) ->
	(lists:sum([bs(A) || A <- RRR]) + 31) div 32.

packing_tags(Ts) ->
	packing_tags(Ts, 0, 0, 0, []).

packing_tags([], NW, _, _, Acc) ->
	lists:map(fun({T,{bits,NB,Off}}) ->
		{T,{bits,NB+NW,Off}};
   	(X) ->
	   	X
	end, lists:reverse(Acc));

packing_tags([{T,V}|Ts], NW, NB, Off, Acc) ->
	packing_tags(Ts, NW, NB, Off, [{T,{value,V}}|Acc]);
packing_tags([nil|Ts], NW, NB, Off, Acc) ->
	packing_tags(Ts, NW, NB, Off, [{t,{value,nil}}|Acc]);
packing_tags([T|Ts], NW, NB, Off, Acc) ->
	case bs(T) of
	8 ->
		pack8(T, Ts, NW, NB, Off, Acc);
	32 ->
		packing_tags(Ts, NW+1, NB, Off, [{T,{word,NW}}|Acc])
	end.

pack8(T, Ts, NW, NB, 0=Off, Acc) ->
	packing_tags(Ts, NW, NB+1, 8, [{T,{bits,NB,Off}}|Acc]);
pack8(T, Ts, NW, NB, 24=Off, Acc) ->
	packing_tags(Ts, NW, NB, 0, [{T,{bits,NB-1,Off}}|Acc]);
pack8(T, Ts, NW, NB, Off, Acc) ->
	packing_tags(Ts, NW, NB, Off+8, [{T,{bits,NB-1,Off}}|Acc]).

%%EOF
