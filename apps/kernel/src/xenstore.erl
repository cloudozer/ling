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
-export([read/1,write/2,mkdir/1,rm/1,directory/1]).
-export([get_perms/1,set_perms/2,watch/2,unwatch/1]).
-export([transaction/0,commit/1,rollback/1]).

-define(XS_DEBUG, 0).
-define(XS_DIRECTORY, 1).
-define(XS_READ, 2).
-define(XS_GET_PERMS, 3).
-define(XS_WATCH, 4).
-define(XS_UNWATCH, 5).
-define(XS_TRANSACTION_START, 6).
-define(XS_TRANSACTION_END, 7).
-define(XS_INTRODUCE, 8).
-define(XS_RELEASE, 9).
-define(XS_GET_DOMAIN_PATH, 10).
-define(XS_WRITE, 11).
-define(XS_MKDIR, 12).
-define(XS_RM, 13).
-define(XS_SET_PERMS, 14).
-define(XS_WATCH_EVENT, 15).
-define(XS_ERROR, 16).
-define(XS_IS_DOMAIN_INTRODUCED, 17).
-define(XS_RESUME, 18).
-define(XS_SET_TARGET, 19).
-define(XS_RESTRICT, 20).
-define(XS_RESET_WATCHES, 21).

read(Key) ->
	XS = open_port(xenstore, []),
	port_control(XS, ?XS_READ, [0,0,0,0] ++ Key),
	receive R -> R end,
	port_close(XS),
	R.

write(_Key, _Value) -> throw(not_implemented).
mkdir(_Path) 		-> throw(not_implemented).
rm(_Key)			-> throw(not_implemented).
directory(_Path)	-> throw(not_implemented).
get_perms(_Path)	-> throw(not_implemented).
set_perms(_Path, _Perms) -> throw(not_implemented).
watch(_Path, _Token)	 -> throw(not_implemented).
unwatch(_Path)		-> throw(not_implemented).
transaction()		-> throw(not_implemented).
commit(_Tx)			-> throw(not_implemented).
rollback(_Tx)		-> throw(not_implemented).

%%posix(22)	-> einval;
%%posix(13)	-> eaccess;
%%posix(17)	-> eexist;
%%posix(21)	-> eisdir;
%%posix(2)	-> enoent;
%%posix(12)	-> enomem;
%%posix(28)	-> enospc;
%%posix(5)	-> eio;
%%posix(39)	-> enotempty;
%%posix(38)	-> enosys;
%%posix(30)	-> erofs;
%%posix(16)	-> ebusy;
%%posix(11)	-> eagain;
%%posix(106)	-> eisconn;
%%posix(7)	-> e2big;
%%posix(Code)	-> Code.

