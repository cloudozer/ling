-module(railing).
-export([main/1]).

opt_spec() -> [
	{help,    $h, "help",    undefined, "This help"},
	{lib,     $l, "lib",     atom,      "Import lib from Erlang OTP"},
	{include, $i, "include", string,    "Import directory recursively"},
	{exclude, $x, "exclude", string,    "Do not import directories that start with path"},
	{name,    $n, "name",    string,    "Set image name (default: current dir name)"},
	{domain,  $d, "domain",  string,    "Set domain file name (default: 'domain_config')"},
	{memory,  $m, "memory",  integer,   "Set image memory (megabytes)"},
	{extra,   $e, "extra",   string,    "Append extra string to the kernel command line"},
	{version, $v, "version", undefined, "Print version information and quit"}
	% debug
	% clean
].

cache_dir() -> ".railing".

main(Args) ->
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
			io:format("LING v~s\n", [?LING_VER]),
			halt();
		_ ->
			ok
	end,

	case lists:member(help, Opts) orelse length(Cmds) == 0 of
		true ->
			getopt:usage(opt_spec(), "railing", "[image]"),
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

	DefPrjName =
		case file:get_cwd() of
			{ok,"/"} ->
				"himmel";
			{ok,Cwd} ->
				filename:basename(Cwd)
		end,

	PrjName =
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

	ImgName = PrjName ++ ".img",

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

	{ok, EmbedFs} = file:open(filename:join(cache_dir(),"embed.fs"), [write]),

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

	ok = sh("ld -r -b binary -o embed.fs.o embed.fs", [{cd, cache_dir()}]),
	ok = sh("ld -T ling.lds -nostdlib vmling.o embed.fs.o -o ../" ++ ImgName, [{cd, cache_dir()}]),

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
	Home = "-home /" ++ PrjName,
	Extra = [" " ++ E || {extra, E} <- Config] ++ [Home] ++ [Pz],

	ok = file:write_file(DomName,
		"name = \"" ++ PrjName ++ "\"\n" ++
		"kernel = \"" ++ ImgName ++ "\"\n" ++
		"extra = \"" ++ Extra ++ "\"\n" ++
		Memory ++
		Vif ++ "\n"
	).

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

avoid_ebin_stem(Dir) ->
	case filename:basename(Dir) of
		"" -> top;
		"." -> top;
		"ebin" -> avoid_ebin_stem(filename:dirname(Dir));
		Stem -> list_to_atom(Stem)
	end.

%%EOF
