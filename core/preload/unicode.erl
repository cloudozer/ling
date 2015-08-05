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

-module(unicode).

-export([bom_to_encoding/1,encoding_to_bom/1]).
-export([characters_to_list/1]).
-export([characters_to_binary/1,characters_to_binary/2]).

bom_to_encoding(<<239,187,191,_/binary>>) ->
    {utf8,3};
bom_to_encoding(<<0,0,254,255,_/binary>>) ->
    {{utf32,big},4};
bom_to_encoding(<<255,254,0,0,_/binary>>) ->
    {{utf32,little},4};
bom_to_encoding(<<254,255,_/binary>>) ->
    {{utf16,big},2};
bom_to_encoding(<<255,254,_/binary>>) ->
    {{utf16,little},2};
bom_to_encoding(Bin) when is_binary(Bin) ->
    {latin1,0}.

encoding_to_bom(unicode) ->
    <<239,187,191>>;
encoding_to_bom(utf8) ->
    <<239,187,191>>;
encoding_to_bom(utf16) ->
    <<254,255>>;
encoding_to_bom({utf16,big}) ->
    <<254,255>>;
encoding_to_bom({utf16,little}) ->
    <<255,254>>;
encoding_to_bom(utf32) ->
    <<0,0,254,255>>;
encoding_to_bom({utf32,big}) ->
    <<0,0,254,255>>;
encoding_to_bom({utf32,little}) ->
    <<255,254,0,0>>;
encoding_to_bom(latin1) ->
    <<>>.

characters_to_list(Data) ->
	unicode:characters_to_list(Data, unicode).

characters_to_binary(Data) ->
	characters_to_binary(Data, unicode).

characters_to_binary(Data, InEncoding) ->
	unicode:characters_to_binary(Data, InEncoding, unicode).

