-module(railing).
-export([main/1]).

cache_dir() -> ".railing".

main(_) ->
	file:make_dir(cache_dir()),
	{ok, Sections} = escript:extract(escript:script_name(), []),
	Archive = proplists:get_value(archive, Sections),
	zip:extract(Archive, [{cwd, cache_dir()}]),

	{Files, Apps} = read_config(),

	ProjName = 
		case file:get_cwd() of
			{ok,"/"} ->
				"himmel";
			{ok,Cwd} ->
				filename:basename(Cwd)
		end,

	DFs = [{filename:dirname(F),F} || F <- lists:usort(Files)],
	CustomBucks = [
		{
			avoid_ebin_stem(Dir),
			filename:join(["/",ProjName,Dir]),
			[bin(avoid_ebin_stem(Dir),File) || {Dir1,File} <- DFs, Dir1 =:= Dir]
		} || Dir <- lists:usort([D || {D,_} <- DFs])
	],

	StartBoot = filename:join([code:root_dir(),bin,"start.boot"]),
	Bucks =
		[{boot, "/boot", [local_map, StartBoot]}] ++
		[bucket(A) || A <- Apps] ++
		CustomBucks,

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
	ok = sh("ld -o ../vmling -T ling.lds -nostdlib vmling.o embed.fs.o", [{cd, cache_dir()}]).

write_bin(Dev, Bin, Data) ->
	Name = binary:list_to_bin(Bin),
	NameSize = erlang:size(Name),
	DataSize = erlang:size(Data),
	file:write(Dev, <<NameSize, Name/binary, DataSize:32, Data/binary>>).

bucket(App) ->
	Dir = code:lib_dir(App),
	Mnt = filename:join(["/erlang/lib",filename:basename(Dir),ebin]),

	Files = union(
		filelib:wildcard(filename:join([Dir, ebin, "*"])),
		filelib:wildcard(filename:join([cache_dir(), apps, App, ebin, "*"]))
	),

	NewFiles = [bin(App, F) || F <- Files],

	{App, Mnt, NewFiles}.


bin(Buck, File) ->
	case filename:extension(File) of
		".beam" ->
			compile(Buck, File);
		_ ->
			File
	end.

compile(Buck, Beam) ->
	Ling = filename:join([cache_dir(), ling, Buck, filename:rootname(filename:basename(Beam)) ++ ".ling"]),

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

read_config() ->
	ConfigFile = "railing.config",
	DefConf = {[], [kernel, stdlib]},
	case file:consult(ConfigFile) of
		{ok, Stanza} ->
			lists:foldl(
				fun
					({import,"/" ++ _} = Opt, Conf) ->
						io:format("import path must be relative: ~s", [Opt]),
						Conf;
					({import,Pat}, {Files, Apps}) ->
						{Files ++ filelib:wildcard(Pat), Apps};
					({import_lib, App}, {Files, Apps}) when is_atom(App) ->
						{Files, Apps ++ [App]};
					({import_lib, AppList}, {Files, Apps}) when is_list(AppList) ->
						{Files, Apps ++ AppList}
				end,
				DefConf,
				Stanza
			);
		_ ->
			io:format("Warning: ~s not found\n", [ConfigFile]),
			DefConf
	end.

avoid_ebin_stem(Dir) ->
	case filename:basename(Dir) of
		"" -> top;
		"." -> top;
		"ebin" -> avoid_ebin_stem(filename:dirname(Dir));
		Stem -> list_to_atom(Stem)
	end.

%%EOF
