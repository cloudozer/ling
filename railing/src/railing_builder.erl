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

-module(railing_builder).
-export([build/1]).
-compile({nowarn_unused_function,[debug/2]}).

-include_lib("kernel/include/file.hrl").

-record(rs, {erl_root,
			 ling_root,
			 core_lib,
			 linker_script,
			 cache_dir,
			 proj_name,
			 apps =[kernel,stdlib],
			 files =[],
			 import,
			 embed,
			 cleanup =[]}).

build(Opts) ->
	ErlRoot = case proplists:get_value(erl_root, Opts) of
	undefined ->
		{ok,[[ER]|_]} = init:get_argument(root),
		ER;
	ER ->
		ER
	end,

	%% railing can be installed globally or in one's home
	LingRoot = case init:get_argument(ling_root) of
	error ->
		%% local install
		ScriptDir = filename:dirname(escript:script_name()),
		filename:join(ScriptDir, "..");
	{ok,[[Dir]|_]} ->
		Dir
	end,

	BcDir = filename:join(LingRoot, bc),
	true = code:add_pathz(BcDir),

	CoreLib = filename:join([LingRoot,core,"vmling.o"]),
	LinkerScript = filename:join([LingRoot,core,"ling.lds"]),
	CacheDir = "railing.cache",
	ProjName = case file:get_cwd() of
	{ok,"/"} ->
		"himmel";
	{ok,Cwd} ->
		filename:basename(Cwd)
	end,
	St = #rs{erl_root =ErlRoot,
			 ling_root =LingRoot,
			 core_lib =CoreLib,
			 linker_script =LinkerScript,
			 cache_dir =CacheDir,
			 proj_name =ProjName,
			 import =[{"/boot","boot",[]}]},
	fold_build(build_steps(), St).

build_steps() ->
	[{fun check_exec/2,{objcopy,
						"objcopy -V >/dev/null"}},
	 {fun check_exec/2,{gcc,
						"gcc -v 2>/dev/null"}},
	 {fun check_exists/2,core_lib},
	 fun read_config/1,
	 fun map_std_apps/1,
	 fun map_custom_apps/1,
	 fun map_files/1,
	 fun number_buckets/1,
	 fun replace_beams/1,
	 fun update_boot/1,
	 %{fun debug/2,local_map},
	 fun make_embed/1,
	 %{fun debug/2,embed},
	 fun stale_embed/1,
	 fun refresh_embed/1,
	 fun write_embed_data/1,
	 fun build_image/1,
	 fun cleanup/1].

fold_build([], _St) ->
	ok;
fold_build([{Fun,Arg}|Steps], St) ->
	case Fun(Arg, St) of
	ok ->
		fold_build(Steps, St);
	{ok,St1} ->
		fold_build(Steps, St1);
	{error,_} =Error ->
		Error
	end;
fold_build([Fun|Steps], St) ->
	case Fun(St) of
	ok ->
		fold_build(Steps, St);
	{ok,St1} ->
		fold_build(Steps, St1);
	{error,_} =Error ->
		Error
	end.

check_exec({Tag,Cmd}, _St) ->
	case sh(Cmd, []) of
	ok ->
		ok;
	{error,_} ->
		{error,{no_exec,Tag}}
	end.

check_exists(core_lib, #rs{core_lib =CoreLib}) ->
	case filelib:is_regular(CoreLib) of
	false ->
		{error,no_core_lib};
	true ->
		ok
	end.

read_config(St) ->
	ConfigFile = "railing.config",
	case file:consult(ConfigFile) of
	{ok,Stanza} ->
		read_config_1(Stanza, St);
	_ ->
		io:format("Warning: ~s not found\n", [ConfigFile]),
		%{error,{no_config,ConfigFile}}
		{ok,St}
	end.

read_config_1([], St) ->
	{ok,St};
read_config_1([{import,"/" ++ _} =Opt|_Stanza], _St) ->
	{error,{abs_import,Opt}};
read_config_1([{import,Pat}|Stanza], #rs{files =Files} =St)
		when is_list(Pat) ->
	case filelib:wildcard(Pat) of
	[] ->
		{error,{no_files,Pat}};
	More ->
		read_config_1(Stanza, St#rs{files =Files ++ More})
	end;
read_config_1([{import_lib,App}|Stanza], #rs{apps =Apps} =St)
		when is_atom(App) ->
	read_config_1(Stanza, St#rs{apps =Apps ++ [App]});
read_config_1([{import_lib,As} =Opt|Stanza], #rs{apps =Apps} =St)
		when is_list(As) ->
	case lists:all(fun is_atom/1, As) of
	true ->
		read_config_1(Stanza, St#rs{apps =Apps ++ As});
	false ->
		{error,{bad_opt,Opt}}
	end;
read_config_1([{build_config,fastest}|Stanza], St) ->
	read_config_1(Stanza, St);
read_config_1([Opt|_Stanza], _St) ->
	{error,{bad_opt,Opt}}.

map_std_apps(#rs{apps =Apps,import =Imp} =St) ->
	case map_std_apps_1(Apps) of
	{error,_} =Error ->
		Error;
	Imp1 ->
		{ok,St#rs{import =Imp ++ Imp1}}
	end.

map_std_apps_1(Apps) ->
	map_std_apps_1(Apps, []).

map_std_apps_1([], Acc) ->
	lists:reverse(Acc);
map_std_apps_1([App|Apps], Acc) ->
	case code:lib_dir(App) of
	{error,_} ->
		{error,{no_app,App}};
	Dir ->
		HostDir = filename:join(Dir, ebin),
		VerName = filename:basename(Dir),
		MntDir = filename:join(["/erlang/lib",VerName,ebin]),
		Stem = atom_to_list(App),
		Spec = {MntDir,Stem,filelib:wildcard(filename:join(HostDir, "*"))},
		map_std_apps_1(Apps, [Spec|Acc])
	end.

map_custom_apps(#rs{ling_root =LingRoot,import =Imp} =St) ->
	AppDirs = filelib:wildcard(filename:join([LingRoot,apps,"*"])),
	CustomApps = [{list_to_atom(filename:basename(AD)),AD}
					|| AD <- AppDirs, filelib:is_dir(AD)],
	case map_custom_apps_1(CustomApps) of
	{error,_} =Error ->
		Error;
	Imp1 ->
		{ok,St#rs{import =Imp ++ Imp1}}
	end.

map_custom_apps_1(CAs) ->
	map_custom_apps_1(CAs, []).

map_custom_apps_1([], Acc) ->
	lists:reverse(Acc);
map_custom_apps_1([{App,Dir}|ADs], Acc) ->
	case code:lib_dir(App) of
	{error,_} ->
		{error,{no_app,App}};
	LibDir ->
		HostDir = filename:join(Dir, ebin),
		VerName = filename:basename(LibDir),
		MntDir = filename:join(["/erlang/lib",VerName,ebin]),
		Stem = atom_to_list(App),
		Spec = {MntDir,Stem,filelib:wildcard(filename:join(HostDir, "*"))},
		map_custom_apps_1(ADs, [Spec|Acc])
	end.

map_files(#rs{files =Files,proj_name =ProjName,import =Imp} =St) ->
	DFs = [{filename:dirname(F),F} || F <- lists:usort(Files)],
	Imp1 = [{filename:join(["/",ProjName,Dir]),
			 avoid_ebin_stem(Dir),
			 [File || {Dir1,File} <- DFs, Dir1 =:= Dir]}
						|| Dir <- lists:usort([D || {D,_} <- DFs])],
	{ok,St#rs{import =Imp ++ Imp1}}.

number_buckets(#rs{import =Imp} =St) ->
	{Imp1,_} = lists:mapfoldl(fun({MntDir,Stem,Files}, Prio) ->
		BuckName = lists:flatten(io_lib:format("_~4..0w_~s", [Prio,Stem])),
		{{MntDir,BuckName,Files},Prio +1}
	end, 1, Imp),
	{ok,St#rs{import =Imp1}}.

replace_beams(#rs{import =Imp} =St) ->
	Imp1 = [{M,B,replace_beams_1(Fs)} || {M,B,Fs} <- Imp],
	{ok,St#rs{import =Imp1}}.

replace_beams_1(Fs) ->
	REFs = [{filename:rootname(F),
			 filename:extension(F),
			 F} || F <- Fs],
	replace_beams_1(REFs, []).

replace_beams_1([], Acc) ->
	lists:reverse(Acc);
replace_beams_1([{R,".beam",_},{R,".ling",LF}|REFs], Acc) ->
	replace_beams_1(REFs, [{read,LF}|Acc]);
replace_beams_1([{R,".beam",BF}|REFs], Acc) ->
	Base = filename:basename(R),
	replace_beams_1(REFs, [{transform,Base ++ ".ling",BF}|Acc]);
replace_beams_1([{_,_,F}|REFs], Acc) ->
	replace_beams_1(REFs, [{read,F}|Acc]).

update_boot(#rs{erl_root=ErlRoot,import =Imp} =St) ->
	ScrFile = filename:join([ErlRoot,bin,"start.boot"]),
	true = filelib:is_regular(ScrFile),
	LocalMapData = list_to_binary([io_lib:format("~s /~s\n",
			[MntDir,BuckName])
					|| {MntDir,BuckName,_} <- Imp]),
	{value,{M1,B1,[]},Imp1} = lists:keytake("/boot", 1, Imp),
	Fs1 = [{data,"local.map",LocalMapData},
		   {read,ScrFile}],
	{ok,St#rs{import =[{M1,B1,Fs1}|Imp1]}}.

debug(local_map, #rs{import =Imp}) ->
	io:format("----local.map--------\n", []),
	{_,_,Fs} = lists:keyfind("/boot", 1, Imp),
	{data,_,Data} = lists:keyfind("local.map", 2, Fs),
	io:format("~s", [Data]),
	ok;
debug(import, #rs{import =Imp}) ->
	io:format("----import--------\n", []),
	io:format("~P\n", [Imp,16]),
	ok;
debug(embed, #rs{embed =Embed}) ->
	io:format("----embed--------\n", []),
	io:format("~P\n", [Embed,16]),
	ok.

make_embed(#rs{import =Imp} =St) ->
	Embed = lists:concat([[{BuckName ++ "-" ++ getname(S) ++ ".o",
							BuckName,
							S}
					|| S <- Ss]
			|| {_,BuckName,Ss} <- Imp]),
	{ok,St#rs{embed =Embed}}.

stale_embed(#rs{cache_dir =CacheDir,embed =Embed}) ->
	file:make_dir(CacheDir),	%% ignore eexist
	{ok,Cs} = file:list_dir(CacheDir),
	lists:foreach(fun(C) ->
		case lists:keymember(C, 1, Embed) of
		false ->
			ok = file:delete(filename:join(CacheDir, C));
		true ->
			ok
		end
	end, Cs),
	ok.

refresh_embed(#rs{cache_dir =CacheDir,embed =Embed}) ->
	lists:foreach(fun({F,BuckName,S}) ->
		OutFile = filename:join(CacheDir, F),
		TS = gettime(S),
		case file:read_file_info(OutFile) of
		{error,_} ->
			io:format("embed: ~s\n", [F]),
			refresh_embed_1(S, OutFile, BuckName);
		{ok,#file_info{mtime =MTime}} when MTime =< TS ->
			io:format("refresh: ~s\n", [F]),
			refresh_embed_1(S, OutFile, BuckName);
		_ ->
			ok
		end
	end, Embed),
	ok.

refresh_embed_1({read,InFile}, OutFile, BuckName) ->
	ok = objcopy(InFile, OutFile, BuckName);
refresh_embed_1({transform,Name,BeamFile}, OutFile, BuckName) ->
	{ok,L} = ling_code:beam_to_ling(BeamFile),
	{ok,S} = ling_code:ling_to_specs(L),
	ok = file:write_file(Name, ling_lib:specs_to_binary(S)),
	ok = objcopy(Name, OutFile, BuckName),
	ok = file:delete(Name);
refresh_embed_1({data,Name,Data}, OutFile, BuckName) ->
	ok = file:write_file(Name, Data),
	ok = objcopy(Name, OutFile, BuckName),
	ok = file:delete(Name).

write_embed_data(#rs{import =Imp}) ->
	EmbDataFile = "embed_data.c",
	{ok,Out} = file:open(EmbDataFile, [write]),
	io:format(Out, "// autogenerated by railing\n", []),
	io:format(Out, "#include \"embed.h\"\n\n", []),
	io:format(Out, "embed_buck_t embed_bucks[] = {\n", []),
	NameCount =lists:foldl(fun({_,BuckName,Ss}, Index) ->

		%%
		%% see comment in embed.c
		%%

		NextIndex = Index +length(Ss),
		io:format(Out, "\t{ .bucket_raw = (uint8_t *)\"\\~3.8.0b~s\", .bucket = noval,~n"
				"\t\t.start_index = ~w, .end_index = ~w },~n",
			[length(BuckName),BuckName,Index,NextIndex]),
		NextIndex
	end, 0, Imp),
	io:format(Out, "};\n\n", []),
	io:format(Out, "int nr_embed_bucks = ~w;\n\n", [length(Imp)]),

	BNIs = lists:map(fun({_,BuckName,Ss}) ->
		Is = lists:map(fun(S) ->
			Name = getname(S),
			Name1 = re:replace(Name, "[.-]", "_", [global]),
			Starts = [BuckName,"_binary_",Name1,"_start"],
			Ends = [BuckName,"_binary_",Name1,"_end"],
			io:format(Out, "extern uint8_t ~s[];~n", [Starts]),
			io:format(Out, "extern uint8_t ~s[];~n", [Ends]),
			{Name,Starts,Ends}
		end, Ss),
		{BuckName,Is}
	end, Imp),

	io:format(Out, "\nembed_bin_t embed_bins[] = {\n", []),
	lists:foreach(fun({_,NameInfos}) ->
		lists:foreach(fun({Name,Starts,Ends}) ->
			io:format(Out, "\t{ .name_raw = (uint8_t *)\"\\~3.8.0b~s\", .name = noval,~n"
					"\t\t.starts = ~s, .ends = ~s },~n",
						[length(Name),Name,Starts,Ends])
		end, NameInfos)
	end, BNIs),
	io:format(Out, "};~n~n", []),
	io:format(Out, "int nr_embed_bins = ~w;\n\n", [NameCount]),
	file:close(Out),
	ok.

build_image(#rs{cache_dir =CacheDir,
				ling_root =LingRoot,
				core_lib =CoreLib,
				linker_script =LinkerScript,
				embed =Embed} =St) ->
	EmbDataFile = "embed_data.c",
	EmbDataObj = "embed_data.o",
	Cmd = lists:flatten(io_lib:format("gcc -o vmling "
			"-I ~s/core/include "
			"-T ~s -nostdlib "
			"-Xlinker --build-id=none "
			"-O3 -fno-omit-frame-pointer -fno-stack-protector "
			"~s ~s ~s",
				[LingRoot,LinkerScript,EmbDataFile,CoreLib,
					string:join([filename:join(CacheDir, F)
						|| {F,_,_} <- Embed], " ")])),
	ok = sh(Cmd, []),
	{ok,St#rs{cleanup =[EmbDataFile,EmbDataObj]}}.

cleanup(#rs{cleanup =Cleanup}) ->
	lists:foreach(fun(File) ->
		file:delete(File)
	end, Cleanup),
	ok.

%-------------------------------------------------------------------------------

objcopy(InFile, OutFile, Prefix) ->
	OutAbs = filename:absname(OutFile),
	InDir = filename:dirname(InFile),
	InName = filename:basename(InFile),
	Cmd = lists:flatten(io_lib:format("objcopy -I binary "
			"-O elf64-x86-64 -B i386 "
			"--prefix-symbols=~s "
			"~s ~s", [Prefix,InName,OutAbs])),
	ok = sh(Cmd, [{cd,InDir}]).
	
getname({read,File}) ->
	filename:basename(File);
getname({transform,Name,_}) ->
	Name;
getname({data,Name,_}) ->
	Name.

gettime({read,File}) ->
	{ok,#file_info{mtime =MTime}} = file:read_file_info(File),
	MTime;
gettime({transform,_,BeamFile}) ->
	{ok,#file_info{mtime =MTime}} = file:read_file_info(BeamFile),
	MTime;
gettime({data,_,_}) ->
	calendar:local_time().

avoid_ebin_stem(Dir) ->
	case filename:basename(Dir) of
	"" -> "top";
	"." -> "top";
	"ebin" -> avoid_ebin_stem(filename:dirname(Dir));
	Stem -> Stem
	end.

sh(Command, Opts) ->
    PortOpts = [{line,16384},
				 use_stdio,
				 stderr_to_stdout,
				 exit_status] ++ Opts,
    Port = open_port({spawn, Command}, PortOpts),
	sh_loop(Port).

sh_loop(Port) ->
    receive
        {Port, {data, {eol, Line}}} ->
			io:format("~s~n", [Line]),
			sh_loop(Port);
        {Port, {data, {noeol, Line}}} ->
			io:format("~s", [Line]),
            sh_loop(Port);
        {Port, {exit_status, 0}} ->
			ok;
        {Port,{exit_status,Status}} ->
			{error,Status}
    end.

%%EOF
