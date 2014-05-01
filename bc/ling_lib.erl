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

-module(ling_lib).
-export([abstract_code/1]).
-export([specs_to_binary/1]).

-include("ling_code.hrl").

abstract_code(Ling) when is_list(Ling) -> %% {ok,Forms} | {error,Error}
	case file:read_file(Ling) of
	{ok,Bin} ->
		abstract_code(Bin);
	Err ->
		Err
	end;
abstract_code(<<"FOR1",_Size:32,"LING",Chunks/binary>>) ->
	scan_chunks(Chunks);
abstract_code(_) ->
	{error,badarg}.

scan_chunks(<<"Abst",Sz:32,Data:(Sz)/binary,_/binary>>) ->
	{ok,binary_to_term(Data)};
scan_chunks(<<_Tag:4/binary,Sz:32,_Data:(Sz)/binary,PadChunks/binary>>) ->
	PadSize = case Sz rem 4 of
	0 -> 0;
	N -> 4 -N
	end,
	<<_Pad:(PadSize)/binary,Chunks/binary>> = PadChunks,
	scan_chunks(Chunks);
scan_chunks(<<>>) ->
	no_abstract_code.
	
%% (unsigned)
%% opcode
%% atom
%% bif
%% catch
%% export
%% reg_as_term
%% slot_as_term
%% tag_int
%% literal
%% label
%% nolabel
%% nil
%% fu
%% str

%% 0xxxxxxx		{opcode,7}
%% 1000xxxx		{reg_as_term,4}
%% 1001xxxx		{slot_as_term,4}
%% 1010xxxx		{unsigned,12} [1]
%% 1011xxxx		{unsigned,20} [2]
%% 1100xxxx		{atom,12} [1]
%% 1101xxxx		{export,12} [1]
%% 111000xx		{literal,10} [1]
%% 111001xx		{bif,10} [1]	
%% 111010xx		{opcode,10} [1]
%% 111011xx		{tag_int,10} [1]
%% 111100xx		{fu,10} [1]
%% 11110100		{catch,8} [1]		
%% 11110101		{str,8} [1]
%% 11110110		(unused)
%% 11110111		(unused)
%% 11111000		{tag_int,32} [4]
%% 11111001		{unsigned,32} [4]
%% 11111010		{reg_as_term,8} [1]
%% 11111011		{slot_as_term,8} [1]
%% 11111100		nil
%% 11111101		{f,none}
%% 11111110		{f,16} [2]
%% 11111111		(extended)

%% extended tag (follows 255):
%%
%% 00000000		{f,32} [4]
%% 00000001		{atom,32} [4]
%% 00000010		{export,32} [4]
%% 00000011		{literal,32} [4]
%% 00000100		{bif,32} [4]
%% 00000101		{fu,32} [4]
%% 00000110		{catch,32} [4]
%% 00000111		{str,32} [4]
%% 00001000		{slot_as_term,32} [4]
%% 00001001		{opcode,32} [4]
%% ...			(unused)
%%

specs_to_binary(Specs) ->	%% {ok,Bin}

	L = [wrap("Atom", atoms_chunk(Specs)),
		 wrap("ExpT", exports_chunk(Specs)),
		 wrap("ImpT", imports_chunk(Specs)),
		 wrap("Code", code_chunk(Specs)),
		 wrap("CatT", catches_chunk(Specs)),
		 wrap("FunT", lambdas_chunk(Specs)),
		 wrap("StrT", strings_chunk(Specs)),
		 wrap("LitT", literals_chunk(Specs)),
		 wrap("Attr", attrs_chunk(Specs)),
		 wrap("CInf", cinfo_chunk(Specs)),
	 	 wrap("Line", line_chunk(Specs))]
	%% 
	%% This increases the size of the image; make an option?
	%%
		++ if Specs#m.abst_code =/= no_abstract_code ->
			[wrap("Abst", term_to_binary(Specs#m.abst_code))];
				true -> [] end,

	Chunks = list_to_binary(L),

	FormSize = byte_size(Chunks) + 4,

	<<"FOR1",FormSize:32,"LING",Chunks/binary>>.

wrap(_, undefined) ->
	<<>>;
wrap([A,B,C,D], Body) ->
	ChunkSize = byte_size(Body),
	AlignedSize = (ChunkSize + 3) band (bnot 3),
	PadSize = AlignedSize - ChunkSize,
	<<A,B,C,D,ChunkSize:32,Body/binary,0:PadSize/unit:8>>.

atoms_chunk(#m{atoms=Atoms}) ->
	%io:format("Atoms=~p~n", [Atoms]),
	L = [<<(length(Atoms)):32>>] ++
		[[length(atom_to_list(A))] ++ atom_to_list(A) || A <- Atoms],
	list_to_binary(L).

exports_chunk(#m{atoms=Atoms,exports=Exports}) ->
	L = [<<(length(Exports)):32>>] ++
		[<<(ai(F, Atoms)):32,N:32,Off:32>> || {F,N,Off} <- Exports],
	list_to_binary(L).

imports_chunk(#m{atoms=Atoms,imports=Imports}) ->
	L = [<<(length(Imports)):32>>] ++
		[<<(ai(M, Atoms)):32,(ai(F, Atoms)):32,N:32>> || {M,F,N} <- Imports],
	list_to_binary(L).

code_chunk(#m{code=Code}) ->

	CodeSize = length(lists:concat(Code)),

	<<CodeSize:32,(encode(Code))/binary>>.

catches_chunk(#m{catches=Catches}) ->
	L = [<<(length(Catches)):32>>] ++
		[<<Off:32>> || Off <- Catches],
	list_to_binary(L).

lambdas_chunk(#m{atoms=Atoms,lambdas=Lambdas}) ->
	L = [<<(length(Lambdas)):32>>] ++
		[<<(ai(F, Atoms)):32,A:32,Off:32,Idx:32,NFree:32,Ou:32>>
					|| {F,A,Off,Idx,NFree,Ou} <- Lambdas],
	list_to_binary(L).

strings_chunk(#m{strings=StrTabBin}) ->
	StrTabBin.

literals_chunk(#m{literals=Literals}) ->
	%io:format("Literals=~p~n", [Literals]),
	EncLits = [term_to_binary(Lit) || Lit <- Literals],
	L = [<<(length(Literals)):32>>] ++
		[<<(byte_size(EncLit)):32,EncLit/binary>> || EncLit <- EncLits],
	list_to_binary(L).

attrs_chunk(#m{attrs=undefined}) ->
	undefined;
attrs_chunk(#m{attrs=Attrs}) ->
	term_to_binary(Attrs).

cinfo_chunk(#m{compile_info=undefined}) ->
	undefined;
cinfo_chunk(#m{compile_info=CInfo}) ->
	term_to_binary(CInfo).

line_chunk(#m{line_info=undefined}) ->
	undefined;
line_chunk(#m{atoms=Atoms,line_info={LineRefs,FileNames}}) ->
	PackedLineRefs = lists:map(fun({Offset,undefined}) ->
		{Offset,0};
	({Offset,{nofile,Line}}) ->
		{Offset,Line};	%% does absense of file ref mean default file?
	({Offset,{File,Line}}) ->
		{Offset,(File bsl 24) bor Line}
	end, LineRefs),

	L = [<<(length(LineRefs)):32,(length(FileNames)):32>>] ++
		[<<Offset:32,Location:32>> || {Offset,Location} <- PackedLineRefs] ++
		[<<(ai(Name, Atoms)):32>> || Name <- FileNames],
	list_to_binary(L).

ai(A, As) ->
	ai(A, As, 0).

ai(A, [A|_], I) -> I;
ai(A, [_|As], I) ->
	ai(A, As, I+1).

encode(Specs) ->
	list_to_binary([enc(S) || S <- lists:concat(Specs)]).

enc({opcode,N}) when N < 128 ->
	<<0:1,N:7>>;
enc({opcode,N}) when N < 1024 ->
	<<58:6,N:10>>;
enc({opcode,N}) ->
	<<255,9,N:32>>;
enc({reg_as_term,X}) when X < 16 ->
	<<8:4,X:4>>;
enc({reg_as_term,X}) when X < 256 ->
	<<250,X>>;
enc({slot_as_term,Y}) when Y < 16 ->
	<<9:4,Y:4>>;
enc({slot_as_term,Y}) when Y < 256 ->
	<<251,Y>>;
enc({slot_as_term,Y}) ->
	<<255,8,Y:32>>;
enc(N) when is_integer(N), N >= 0, N < 4096 ->
	<<10:4,N:12>>;
enc(N) when is_integer(N), N >= 0, N < 1048576 ->
	<<11:4,N:20>>;
enc(N) when is_integer(N), N >= 0 ->
	<<249,N:32>>;
enc({atom,N}) when N < 4096 ->
	<<12:4,N:12>>;
enc({atom,N}) ->
	<<255,1,N:32>>;
enc({export,none}) ->
	<<246>>;	%% very rare - yet a single byte encoding
enc({export,N}) when N < 4096 ->
	<<13:4,N:12>>;
enc({export,N}) ->
	<<255,2,N:32>>;
enc({literal,N}) when N < 1024 ->
	<<56:6,N:10>>;
enc({literal,N}) ->
	<<255,3,N:32>>;
enc({bif,N}) when N < 1024 ->
	<<57:6,N:10>>;
enc({bif,N}) ->
	<<255,4,N:32>>;
enc({tag_int,I}) when I >= -512, I < 512 ->
	<<59:6,I:10/signed>>;
enc({tag_int,I}) ->
	<<248,I:32/signed>>;
enc({fu,N}) when N < 1024 ->
	<<60:6,N:10>>;
enc({fu,N}) ->
	<<255,5,N:32>>;
enc({'catch',N}) when N < 256 ->
	<<244,N>>;
enc({'catch',N}) ->
	<<255,6,N:32>>;
enc({str,N}) when N < 256 ->
	<<245,N>>;
enc({str,N}) ->
	<<255,7,N:32>>;
enc(nil) ->
	<<252>>;
enc({f,none}) ->
	<<253>>;
enc({f,O}) when O < 65536 ->
	<<254,O:16>>;
enc({f,O}) ->
	<<255,0,O:32>>.

%%EOF

