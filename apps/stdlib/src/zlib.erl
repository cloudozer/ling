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

%%
%% @doc A do-nothing implementation of zlib
%%
-module(zlib).

-define(ZSTREAM_BUF_SIZE, 4096).

%% Module interface
-export([open/0,close/1]).
-export([deflateInit/1,deflateInit/2,deflateInit/6]).
-export([deflate/2,deflate/3,deflateSetDictionary/2]).
-export([deflateReset/1,deflateParams/3,deflateEnd/1]).
-export([inflateInit/1,inflateInit/2]).
-export([inflate/2,inflateSetDictionary/2]).
-export([inflateReset/1,inflateEnd/1]).
-export([setBufSize/2,getBufSize/1]).

-export([crc32/1,crc32/2,crc32/3,crc32_combine/4]).
-export([adler32/2,adler32/3,adler32_combine/4]).

-export([compress/1,uncompress/1]).
-export([zip/1,unzip/1]).
-export([gzip/1,gunzip/1]).

%%------------------------------------------------------------------------------

open() -> zstream.	%% dummy

close(zstream) -> ok.

deflateInit(Z) ->
	deflateInit(Z, default).

deflateInit(zstream, _Level) -> ok.

deflateInit(zstream, _Level, _Method, _WindowBits, _MemLevel, _Strategy) -> ok.

deflate(Z, Data) ->
	deflate(Z, Data, none).

deflate(zstream, Data, _Flush) -> Data.

deflateSetDictionary(zstream, _Dictionary) -> 0.

deflateReset(zstream) -> ok.

deflateParams(zstream, _Level, _Strategy) -> ok.

deflateEnd(zstream) -> ok.

inflateInit(zstream) -> ok.

inflateInit(zstream, _WindowBits) -> ok.

inflate(zstream, Data) -> Data.

inflateSetDictionary(zstream, _Dictionary) -> 0.

inflateReset(zstream) -> ok.

inflateEnd(zstream) -> ok.

setBufSize(zstream, _Size) -> ok.

getBufSize(zstream) -> ?ZSTREAM_BUF_SIZE.

%%------------------------------------------------------------------------------

%%
%% The following will work in the typical case only
%%
crc32(zstream) -> 0.

crc32(zstream, Data) ->
	erlang:crc32(Data).

crc32(zstream, PrevCRC, Data) ->
	erlang:crc32(PrevCRC, Data).

crc32_combine(zstream, CRC1, CRC2, Size2) ->
	erlang:crc32_combine(CRC1, CRC2, Size2).

adler32(zstream, _Data) ->
	erlang:error(not_implemented).

adler32(zstream, _PrevAdler, _Data) ->
	erlang:error(not_implemented).

adler32_combine(zstream, _Adler1, _Adler2, _Size2) ->
	erlang:error(not_implemented).

%%------------------------------------------------------------------------------

compress(Data) -> Data.

uncompress(Data) -> Data.

zip(Data) -> Data.

unzip(Data) -> Data.

gzip(Data) -> Data.

gunzip(Data) -> Data.

%%EOF
