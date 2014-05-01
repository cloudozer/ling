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

-module(net_vif).
-export([open/1,open/2]).
-export([close/1]).

%%
%% Raw vif driver
%%

-define(VIF_PREFIX, "eth").

-define(VIF_REQ_OPEN, 100).

-define(VIF_REP_ERROR, 0).
-define(VIF_REP_OK, 1).

open(IfName) ->
	open(IfName, []).

open(IfName, Opts) when is_atom(IfName) ->
	open(atom_to_list(IfName), Opts);

open(?VIF_PREFIX ++ NN, Opts) when is_list(Opts) ->
	IfIndex = list_to_integer(NN),
	Port = erlang:open_port(vif, Opts),
	AsBytes = binary_to_list(<<IfIndex:32>>),
	case erlang:port_control(Port, ?VIF_REQ_OPEN, AsBytes) of
	[?VIF_REP_OK] ->
		{ok,Port};
	[?VIF_REP_ERROR|Err] ->
		{error,list_to_atom(Err)}
	end;

open(_IfName, _Opts) ->
	{error,badarg}.

close(Port) ->
	erlang:port_close(Port).

%%EOF
