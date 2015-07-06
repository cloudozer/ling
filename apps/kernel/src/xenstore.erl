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

-module(xenstore).
-export([read/1,write/2,list/1]).

read(Key) ->
	case xenstore:'read$'(Key) of
		Error when is_integer(Error) -> {error,posix(Error)};
		Value -> {ok,Value} end.

write(Key, Value) ->
	case xenstore:'write$'(Key, Value) of
		Error when is_integer(Error) -> {error,posix(Error)};
		ok -> ok end.

list(Key) ->
	case xenstore:'list$'(Key) of
		Error when is_integer(Error) -> {error,posix(Error)};
		List -> {ok,List} end.

posix(22)	-> einval;
posix(13)	-> eaccess;
posix(17)	-> eexist;
posix(21)	-> eisdir;
posix(2)	-> enoent;
posix(12)	-> enomem;
posix(28)	-> enospc;
posix(5)	-> eio;
posix(39)	-> enotempty;
posix(38)	-> enosys;
posix(30)	-> erofs;
posix(16)	-> ebusy;
posix(11)	-> eagain;
posix(106)	-> eisconn;
posix(7)	-> e2big;
posix(Code)	-> Code.

