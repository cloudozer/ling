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

-module(railing).

-export([main/1]).

main(Args) ->
	case cmd_opts(Args) of
	{[],_} ->
		usage();
	{Cmds,Opts} ->
		actions(Cmds, Opts)
	end.

action(image, Opts) ->
	railing_builder:build(Opts);
action(dconf, Opts) ->
	railing_util:make_domain_config(Opts);
action(help, _Opts) ->
	usage();
action(Cmd, _Opts) ->
	{error,{bad_cmd,Cmd}}.

actions([], _Opts) ->
	ok;
actions([Cmd|Cmds], Opts) ->
	case action(Cmd, Opts) of
	ok ->
		actions(Cmds, Opts);
	{error,Error} ->
		io:format("Error: ~s\n", [format_error(Error)]),
		halt(1)
	end.

cmd_opts(Args) ->
	cmd_opts(Args, [], []).

cmd_opts([], Cmds, Opts) ->
	{lists:reverse(Cmds),
	 lists:reverse(Opts)};
cmd_opts(["-e",ErlRoot|Args], Cmds, Opts) ->
	cmd_opts(Args, Cmds, [{erl_root,ErlRoot}|Opts]);
cmd_opts(["-" ++ _ =Opt|Args], Cmds, Opts) ->
	io:format("~s: option ignored\n", [Opt]),
	cmd_opts(Args, Cmds, Opts);
cmd_opts([Cmd|Args], Cmds, Opts) ->
	cmd_opts(Args, [list_to_atom(Cmd)|Cmds], Opts).

format_error({bad_cmd,Cmd}) ->
	io_lib:format("unknown command: ~s", [Cmd]);
format_error({no_exec,Tag}) ->
	io_lib:format("command not found: ~s", [Tag]);
format_error(no_core_lib) ->
	io_lib:format("vmling.o not found", []);
%format_error({no_config,File}) ->
%	io_lib:format("config not found: ~s", [File]);
format_error({abs_import,Pat}) ->
	io_lib:format("import path must be relative: ~s", [Pat]);
format_error({no_files,Pat}) ->
	io_lib:format("no files found: ~s", [Pat]);
format_error({no_app,App}) ->
	io_lib:format("application not found: ~s", [App]);
format_error({bad_opt,Opt}) ->
	io_lib:format("bad config option: ~p", [Opt]);
format_error(Error) ->
	io_lib:format("~p", [Error]).

usage() ->
	io:format("railing [options] help|image|dconf\n"
		"Commands:\n"
		"    help        show help\n"
		"    image       build LING image using railing.config\n"
		"    dconf       generate domain_config\n"
		"Options:\n"
		"    -e <dir>    set ERL_ROOT for std app import\n", []).

%%EOF
