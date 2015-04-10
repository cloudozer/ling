-module(railing).
-export([main/1]).

opt_spec() -> [
	{help,    $h, "help",    undefined, "this help"},
	{lib,     $l, "lib",     atom,      "import lib from Erlang OTP"},
	{include, $i, "include", string,    "import directory recursively"},
	{exclude, $x, "exclude", string,    "do not import directories that start with <path>"},
	{name,    $n, "name",    string,    "set image name (default: current dir name)"},
	{domain,  $d, "domain",  string,    "set domain config file name (default: 'domain_config')"},
	{memory,  $m, "memory",  integer,   "set domain memory size (megabytes)"},
	{extra,   $e, "extra",   string,    "append to kernel command line"},
	{version, $v, "version", undefined, "print version info"}
	% debug
	% clean
].

cc() -> cc(?ARCH).
cc(arm) -> ["arm-none-eabi-gcc", "-mfpu=vfp", "-mfloat-abi=hard"];
cc(xen_x86) ->
	case os:type() of
		{unix, darwin} -> ["x86_64-pc-linux-gcc"];
		_ -> ["cc"]
	end;
cc(_) -> ["cc"].

gold() -> gold("ld").
gold(Prog) -> [Prog, "-T", "ling.lds", "-nostdlib"].

ld() -> ld(?ARCH).
ld(arm) -> gold("arm-none-eabi-ld");
ld(xen_x86) ->
	case os:type() of
		{unix, darwin} -> ["x86_64-pc-linux-ld"];
		_ -> gold()
	end;
ld(posix_x86) ->
	case os:type() of
		{unix, darwin} -> [
			"ld",
			"-image_base", "0x8000",
			"-pagezero_size", "0x8000",
			"-arch", "x86_64",
			"-framework", "System"
		];
		_ -> gold()
	end;
ld(_) -> gold().

cache_dir() -> ".railing".

project_name(Config) ->
	DefPrjName =
		case file:get_cwd() of
			{ok,"/"} ->
				"himmel";
			{ok,Cwd} ->
				filename:basename(Cwd)
		end,

	Name =
		lists:foldl(
			fun
				({name, undefined}, Res) ->
					Res;
				({name, N}, _) ->
					N;
				(_, Res) ->
					Res
			end,
			DefPrjName,
			Config
		),
		Name.

image_name(Config) ->
	project_name(Config) ++ ".img".

main(Args) ->
	case erlang:system_info(otp_release) of
		?OTP_VER ->
			ok;
		OtpVer ->
			io:format("Error: incompatible Erlang/OTP version: required ~s, found ~s\n", [?OTP_VER, OtpVer]),
			halt(1)
	end,

	{Opts, Cmds} =
		case getopt:parse(opt_spec(), Args) of
			{error, Error} ->
				io:format("~s\n", [getopt:format_error(opt_spec(), Error)]),
				halt(1);
			{ok, Ok} ->
				Ok
		end,

	case lists:member(version, Opts) of
		true ->
			io:format("LING v~s (railing build for ~p)\n", [?LING_VER, ?ARCH]),
			halt();
		_ ->
			ok
	end,

	io:format("railing build arch: ~p\n", [?ARCH]),

	case lists:member(help, Opts) orelse length(Cmds) == 0 of
		true ->
			getopt:usage(opt_spec(), "railing image", ""),
			halt();
		_ ->
			ok
	end,

	lists:foreach(
		fun
			("image") ->
				ok;
			(UnknownCmd) ->
				io:format("railing: unknown command '~s'\n", [UnknownCmd]),
				halt(1)
		end,
		Cmds
	),

	%% add path to ebins for plugins to work
	ok = code:add_paths([D || D <- filelib:wildcard("**/ebin"), not lists:prefix(cache_dir(),D)]),

	Config =
		lists:foldl(
			fun(Plug, C) ->
				{module, Mod} = code:load_abs(filename:rootname(Plug)),
				case lists:member({railing,0}, Mod:module_info(exports)) of
					true ->
						Mod:railing() ++ C;
					_ ->
						io:format("railing: plugin without entry point '~s'\n", [Plug]),
						halt(1)
				end
			end,
			Opts,
			[P || P <- filelib:wildcard("**/default_railing.beam"), not lists:prefix(cache_dir(),P)]
		),

	Excludes = [cache_dir() | [X || {exclude, X} <- Config]],
	Includes =
		filelib:wildcard(filename:join(["**","ebin","*.{app,beam}"])) ++
		lists:foldl(
			fun(Dir, Files) ->
				Files ++ [F || F <- filelib:wildcard(filename:join(Dir, "**")),	not filelib:is_dir(F)]
			end,
			[],
			[I || {include, I} <- Config]
		),

	Files =
		lists:filter(
			fun(I) ->
				lists:all(
					fun(E) ->
						not lists:prefix(E, I)
					end,
					Excludes
				)
			end,
			Includes
		),

	Apps = [kernel, stdlib] ++ [L || {lib, L} <- Config],

	file:make_dir(cache_dir()),
	{ok, Sections} = escript:extract(escript:script_name(), []),
	Archive = proplists:get_value(archive, Sections),

	%% we can't pass {cwd, cache_dir()} to unzip here
	%% because 'keep_old_files' stops working for unknown reason
	file:set_cwd(".railing"),
	zip:unzip(Archive, [keep_old_files]),
	file:set_cwd(".."),

	PrjName = project_name(Config),
	ImgName = image_name(Config),

	DFs = [{filename:dirname(F),F} || F <- lists:usort(Files)],
	CustomBucks = [
		{
			avoid_ebin_stem(Dir),
			filename:join(["/",PrjName,Dir]),
			[bin(avoid_ebin_stem(Dir),File) || {Dir1,File} <- DFs, Dir1 =:= Dir]
		} || Dir <- lists:usort([D || {D,_} <- DFs])
	],

	StartBoot = filename:join([code:root_dir(),bin,"start.boot"]),
	Bucks =
		[{boot, "/boot", [local_map, StartBoot]}] ++
		[lib(A) || A <- Apps] ++
		CustomBucks,

	io:format("Generate: ~s\n", [ImgName]),

	LocalMap =
		lists:map(
			fun({Buck, Mnt, _}) ->
				io_lib:format("~s /~s\n", [Mnt, Buck])
			end,
			Bucks
		),

    EmbedFsPath = filename:join(cache_dir(),"embed.fs"),
	{ok, EmbedFs} = file:open(EmbedFsPath, [write]),

	BuckCount = erlang:length(Bucks),
	BinCount =
		lists:foldl(
			fun({_Buck, _Mnt, Bins}, Count) ->
				Count + erlang:length(Bins)
			end,
			0,
			Bucks
		),

	file:write(EmbedFs, <<BuckCount:32>>),
	file:write(EmbedFs, <<BinCount:32>>),

	lists:foreach(
		fun({Buck, _Mnt, Bins}) ->
			BuckName = binary:list_to_bin(atom_to_list(Buck)),
			BuckNameSize = erlang:size(BuckName),
			BuckBinCount = erlang:length(Bins),

			file:write(EmbedFs, <<BuckNameSize, BuckName/binary, BuckBinCount:32>>),

			lists:foreach(
				fun
					(local_map) ->
						write_bin(EmbedFs, "local.map", list_to_binary(LocalMap));
					(Bin) ->
						{ok, Data} = file:read_file(Bin),
						write_bin(EmbedFs, filename:basename(Bin), Data)
				end,
				Bins
			)
		end,
		Bucks
	),

	file:close(EmbedFs),

	CC = [{cd, cache_dir()}],

	{ok, EmbedFsObject} = embedfs_object(EmbedFsPath),
	sh(ld() ++ ["vmling.o", EmbedFsObject, "-o", "../" ++ ImgName], CC),

	case ?ARCH of
		posix_x86 -> nevermind;
		_ -> ok = domain_config(CustomBucks, Config)
	end.

embedfs_object(EmbedFsPath) ->
	EmbedCPath = filename:join(filename:absname(cache_dir()), "embedfs.c"),
	OutPath = filename:join(filename:absname(cache_dir()), "embedfs.o"),
	{ok, Embed} = file:read_file(EmbedFsPath),
	ok = bfd_objcopy:blob_to_src(EmbedCPath, "_binary_embed_fs", Embed),
	ok = sh(cc() ++ ["-o", OutPath, "-c", EmbedCPath]),
	{ok, OutPath}.

domain_config(CustomBucks, Config) ->
	Project = project_name(Config),
	Image = image_name(Config),
	DomName =
		lists:foldl(
			fun
				({domain, undefined}, Res) ->
					Res;
				({domain, D}, _) ->
					D;
				(_, Res) ->
					Res
			end,
			"domain_config",
			Config
		),

	io:format("Generate: ~s\n", [DomName]),

	Memory =
		case proplists:get_value(memory, Config) of
			undefined ->
				"";
			M ->
				"memory = " ++ integer_to_list(M) ++ "\n"
		end,

	Vif = "vif = " ++ lists:flatten(io_lib:format("~p", [[Vif || {vif, Vif} <- Config]])),
	Pz = " -pz" ++ lists:flatten([" " ++ Dir || {_, Dir, _} <- CustomBucks]),
	Home = "-home /" ++ Project,
	Extra = [E ++ " " || {extra, E} <- Config] ++ [Home] ++ [Pz],

	%% Xen guest maximum command line length is 1024
	case length(lists:flatten(Extra)) < 1024 of
		true ->
			ok = file:write_file(DomName,
				"name = \"" ++ Project ++ "\"\n" ++
				"kernel = \"" ++ Image ++ "\"\n" ++
				"extra = \"" ++ Extra ++ "\"\n" ++
				Memory ++
				Vif ++ "\n"
			);
		_ ->
			io:format("\tfailed: 'extra' param length exceeded 1024 bytes\n"),
			halt(1)
	end,
	ok.

write_bin(Dev, Bin, Data) ->
	Name = binary:list_to_bin(Bin),
	NameSize = erlang:size(Name),
	DataSize = erlang:size(Data),
	file:write(Dev, <<NameSize, Name/binary, DataSize:32, Data/binary>>).

lib(Lib) ->
	Dir =
		case code:lib_dir(Lib) of
			{error, _} ->
				io:format("can't find lib: ~p\n", [Lib]),
				halt(1);
			Ok ->
				Ok
		end,

	Mnt = filename:join(["/erlang/lib",filename:basename(Dir),ebin]),

	Files = union(
		filelib:wildcard(filename:join([Dir, ebin, "*.{app,beam}"])),
		filelib:wildcard(filename:join([cache_dir(), apps, Lib, ebin, "*.{app,beam}"]))
	),

	NewFiles = [bin(Lib, F) || F <- Files],

	{Lib, Mnt, NewFiles}.


bin(Buck, File) ->
	case filename:extension(File) of
		".beam" ->
			compile(Buck, File);
		_ ->
			File
	end.

compile(Buck, Beam) ->
	Ling = filename:join([
		cache_dir(),
		ling,
		Buck,
		filename:rootname(filename:basename(Beam)) ++ ".ling"
	]),

	NeedUpdate = 
		case filelib:last_modified(Ling) of
			0 ->
				true;
			LingTime ->
				calendar:datetime_to_gregorian_seconds(filelib:last_modified(Beam)) > 
				calendar:datetime_to_gregorian_seconds(LingTime)
		end,

	case NeedUpdate of
		true ->
			io:format("Compile: ~s\n", [Beam]),
			{ok,L} = ling_code:beam_to_ling(Beam),
			{ok,S} = ling_code:ling_to_specs(L),
			ok = filelib:ensure_dir(Ling),
			ok = file:write_file(Ling, ling_lib:specs_to_binary(S));
		_ ->
			ok
	end,

	Ling.

union(A, B) ->
	union(A, B, []).

union([], B, U) ->
	B ++ U;
union([Std|A], B, U) ->
	StdBasename = filename:basename(Std),
	Overwritten = 
		fun(Custom) ->
			filename:basename(Custom) == StdBasename
		end,

	case lists:any(Overwritten, B) of
		true ->
			union(A, B, U);
		false ->
			union(A, B, [Std | U])
	end.

sh(X) -> sh(X, []).

sh([Com|Args], Opts) when is_list(Com) ->
	case os:find_executable(Com) of
		false ->
			io:format("Could not find command ~s~n", [Com]),
			halt(1);
		Executable ->
			io:format("Run: ~s ~s~n", [Executable, string:join(Args, " ")]),
			PortOpts = [{line,16384},
						 use_stdio,
						 stderr_to_stdout,
						 {args, Args},
						 exit_status] ++ Opts,
			Port = open_port({spawn_executable, Executable}, PortOpts),
			sh_loop(Port)
	end;
sh(Command, Opts) ->
	io:format("Run: ~s~n", [Command]),
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

avoid_ebin_stem(Dir) ->
	case filename:basename(Dir) of
		"" -> top;
		"." -> top;
		"ebin" -> avoid_ebin_stem(filename:dirname(Dir));
		Stem -> list_to_atom(Stem)
	end.



%% vim: noet ts=4 sw=4 sts=4
%%EOF
