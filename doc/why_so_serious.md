latex input:            mmd-article-header-mk
Title:					Deducing a better language from a driver implementation
Author:					Maxim Kharchenko, Cloudozer LLP
Date:					09/08/2014
latex mode:				memoir
base header level:      2
use xelatex:            true
latex input:            mmd-article-begin-doc

# Overview

```
     1	-module(strawman).
     2	-behaviour(gen_server).
     3	-define(SERVER, ?MODULE).
     4	-export([short_straw/3,short_straw/5]).
     5	
     6	-include("xenstore.hrl").
     7	
     8	-define(NUM_STRAW_REFS, 8).
     9	
    10	%% ------------------------------------------------------------------
    11	%% API Function Exports
    12	%% ------------------------------------------------------------------
    13	
    14	-export([start_link/0]).
    15	-export([open/1]).
    16	-export([split/1]).
    17	
    18	%% ------------------------------------------------------------------
    19	%% gen_server Function Exports
    20	%% ------------------------------------------------------------------
    21	
    22	-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    23	         terminate/2, code_change/3]).
    24	
    25	%% ------------------------------------------------------------------
    26	%% API Function Definitions
    27	%% ------------------------------------------------------------------
    28	
    29	start_link() ->
    30	    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    31	
    32	open(Domid) ->
    33		gen_server:call(?SERVER, {open,Domid}).
    34	
    35	split(Domid) ->
    36		gen_server:call(?SERVER, {split,Domid}).
    37	
    38	%% ------------------------------------------------------------------
    39	%% gen_server Function Definitions
    40	%% ------------------------------------------------------------------
    41	
    42	-record(sm, {top,straws =[]}).
    43	
    44	init(_Args) ->
    45		process_flag(trap_exit, true),
    46		Me = xenstore:domid(),
    47		StrawTop = "data/straw",
    48		ok = xenstore:mkdir(StrawTop),
    49		ok = xenstore:set_perms(StrawTop, [lc("b", Me)]),
    50		WartsTop = "data/warts",
    51		ok = xenstore:mkdir(WartsTop),
    52		ok = xenstore:set_perms(WartsTop, [lc("r", Me)]),
    53		ok = xenstore:watch(StrawTop),
    54		{ok,#sm{top =StrawTop}}.
    55	
    56	handle_call({open,Domid}, _From, St) ->
    57		Me = xenstore:domid(),
    58		WartsDir = lc(["/local/domain/",Me,"/data/warts/",Domid]),
    59		StrawDir = lc(["/local/domain/",Domid,"/data/straw/",Me]),
    60		case xenstore:read(WartsDir) of
    61			{ok,_}	  -> {reply,{error,exists},St};
    62			{error,_} ->
    63				case xenstore:mkdir(StrawDir) of
    64					ok -> do_open(Domid, WartsDir, StrawDir, St);
    65					_  -> {reply,{error,not_found},St} end end;
    66	
    67	handle_call({split,Domid}, _From, #sm{straws =Straws} =St) ->
    68		case lists:keyfind(Domid, 2, Straws) of
    69			{_,_,StrawProc,_,_} -> {reply,{ok,StrawProc},St};
    70			false -> {reply,{error,not_found},St} end.
    71	
    72	handle_cast(_Msg, State) ->
    73	    {noreply, State}.
    74	
    75	handle_info({watch,WatchKey}, #sm{top =StrawTop} =St) ->
    76		case lists:prefix(StrawTop, WatchKey) of
    77			true  -> Suffix = lists:nthtail(length(StrawTop), WatchKey),
    78					 case string:tokens(Suffix, "/") of
    79						[X,"warts"] ->
    80							%% peer wants to communicate
    81							{ok,WartsDir} = xenstore:read(WatchKey),
    82							Domid = list_to_integer(X),
    83							knock_knock(Domid, WartsDir, lc([StrawTop,"/",X]), St);
    84						_ -> {noreply,St} end;
    85			false -> straw_state(WatchKey, St) end;
    86	
    87	handle_info({'EXIT',_,peer_closed}, St) -> {noreply,St};
    88	handle_info(Msg, St) ->
    89		io:format("strawman: info ~p\n", [Msg]),
    90		{noreply,St}.
    91	
    92	terminate(shutdown, #sm{straws =Straws}) ->
    93		ok = close_straws(Straws).
    94	
    95	code_change(_OldVsn, St, _Extra) -> {ok,St}.
    96	
    97	%% ------------------------------------------------------------------
    98	%% Internal Function Definitions
    99	%% ------------------------------------------------------------------
   100	
   101	do_open(Domid, WartsDir, StrawDir, #sm{straws =Straws} =St) ->
   102		%% StrawDir exists, WartsDir does not
   103		{ok,Tid} = xenstore:transaction(),
   104		ok = xenstore:mkdir(WartsDir, Tid),
   105		ok = xenstore:write(lc(WartsDir, "/straw"), StrawDir, Tid),
   106		ok = xenstore:write(lc(WartsDir, "/state"), ?STATE_INIT_WAIT, Tid),
   107		ok = xenstore:write(lc(StrawDir, "/warts"), WartsDir, Tid), %% wakes up peer
   108		ok = xenstore:commit(Tid),
   109		StrawState = lc(StrawDir, "/state"),
   110		ok = xenstore:watch(StrawState),
   111		case xenstore:wait(StrawState, ?STATE_INITIALISED) of
   112			{error,_} =Error -> %% peer gone
   113				ok = xenstore:delete(WartsDir),
   114				ok = xenstore:unwatch(StrawState),
   115				{reply,Error,St};
   116			ok ->
   117				Refs =
   118				lists:map(fun(N) -> {ok,Ref} = xenstore:read_integer(lc([StrawDir,"/ring-ref-",N])),
   119									Ref end, lists:seq(1, ?NUM_STRAW_REFS)),
   120				{ok,Channel} = xenstore:read_integer(lc(StrawDir, "/event-channel")),
   121	      Format = select_format(StrawDir, WartsDir),
   122				StrawProc = spawn_link(?MODULE, short_straw, [self(),Domid,Refs,Channel,Format]),
   123				receive {ready,StrawProc} -> ok end,
   124				ok = xenstore:write(lc(WartsDir, "/state"), ?STATE_CONNECTED),
   125				case xenstore:wait(StrawState, ?STATE_CONNECTED) of
   126					{error,_} =Error ->
   127						ok = xenstore:delete(WartsDir),
   128						ok = xenstore:unwatch(StrawState),
   129						exit(StrawProc, peer_closed),
   130						{reply,Error,St};
   131					ok ->
   132						%% StrawState is being watched
   133						SI = {passive,Domid,StrawProc,StrawState,WartsDir},
   134						{reply,ok,St#sm{straws =[SI|Straws]}} end end.
   135	
   136	knock_knock(Domid, WartsDir, StrawDir, #sm{straws =Straws} =St) ->
   137		StrawState = lc(StrawDir, "/state"),
   138		ok = xenstore:write(StrawState, ?STATE_INITIALISING),
   139		WartsState = lc(WartsDir, "/state"),
   140		ok = xenstore:watch(WartsState),
   141		case xenstore:wait(WartsState, ?STATE_INIT_WAIT) of
   142			{error,_} ->
   143				ok = xenstore:delete(StrawDir),
   144				ok = xenstore:unwatch(WartsState),
   145				{noreply,St};
   146			ok ->
   147	      Format = select_format(StrawDir, WartsDir),
   148				StrawProc = spawn_link(?MODULE, short_straw, [self(),Domid,Format]),
   149				receive {ready,StrawProc,Refs,Channel} -> ok end,
   150				{ok,Tid} = xenstore:transaction(),
   151				lists:foreach(fun({N,Ref}) -> ok = xenstore:write(lc([StrawDir,"/ring-ref-",N]), Ref, Tid) end,
   152								lists:zip(lists:seq(1, ?NUM_STRAW_REFS), Refs)),
   153				ok = xenstore:write(lc(StrawDir, "/event-channel"), Channel, Tid),
   154				ok = xenstore:write(StrawState, ?STATE_INITIALISED, Tid),
   155				ok = xenstore:commit(Tid),
   156				ok = xenstore:wait(WartsState, ?STATE_CONNECTED),
   157				ok = xenstore:write(StrawState, ?STATE_CONNECTED),
   158				SI = {active,Domid,StrawProc,WartsState,StrawDir},
   159				St1 = St#sm{straws = [SI|Straws]},
   160				{noreply,St1} end.
   161	
   162	%%--------------------------------------
   163	%% Active					Passive
   164	%% ======					=======
   165	%% state=CLOSING
   166	%%							unmap refs
   167	%%							state=CLOSED
   168	%%							wait=CLOSED
   169	%% wait=CLOSED
   170	%% end access to refs
   171	%% state=CLOSED
   172	%%--------------------------------------
   173	%% Active					Passive
   174	%% ======					=======
   175	%%							unmap refs
   176	%%							state=CLOSED
   177	%%							wait=CLOSED
   178	%% wait=CLOSED
   179	%%--------------------------------------
   180	
   181	straw_state(WatchKey, #sm{straws =Straws} =St) ->
   182		SI = lists:keyfind(WatchKey, 4, Straws),
   183		straw_state1(SI, St).
   184	
   185	straw_state1(false, St) -> {noreply,St};
   186	straw_state1({_,_,_,StatePath,_} =SI, St) ->
   187		straw_state1(xenstore:read(StatePath), SI, St).
   188	
   189	straw_state1({ok,?STATE_CONNECTED}, _, St) -> {noreply,St};
   190	straw_state1({ok,_}, {active,_,_,_,_}, St) -> {noreply,St};	%% see chart above
   191	straw_state1(_, {_,Domid,StrawProc,StatePath,DataDir}, #sm{straws =Straws} =St) ->
   192		ok = xenstore:unwatch(StatePath),
   193		exit(StrawProc, peer_closed),
   194		ok = xenstore:delete(DataDir),
   195		io:format("strawman: connection to domain ~w lost\n", [Domid]),
   196		Straws1 = lists:keydelete(StrawProc, 3, Straws),
   197		{noreply,St#sm{straws =Straws1}}.
   198	
   199	close_straws([]) -> ok;
   200	close_straws([{Mode,Domid,StrawProc,StatePath,DataDir}|Straws]) ->
   201		if Mode =:= active ->
   202			ok = xenstore:delete(DataDir),
   203			xenstore:watch(StatePath, ?STATE_CLOSED);
   204				true -> ok end,
   205		exit(StrawProc, shutdown),
   206		io:format("strawman: connection to domain ~w closed\n", [Domid]),
   207		close_straws(Straws).
   208	
   209	short_straw(ReplyTo, Domid, Refs, Channel, Format) ->
   210		Pore = pore_straw:open(Domid, Refs, Channel),
   211		ReplyTo ! {ready,self()},
   212		looper(Pore, Format).
   213	
   214	short_straw(ReplyTo, Domid, Format) ->
   215		Pore = pore_straw:open(Domid),
   216		{Refs,Channel} = pore_straw:info(Pore),
   217		ReplyTo ! {ready,self(),Refs,Channel},
   218		looper(Pore, Format).
   219	
   220	looper(Pore, Format) ->
   221		{IA,OA} = pore_straw:avail(Pore),
   222		looper(Pore, IA, OA, undefined, [], 0, [], 0, Format).
   223	
   224	looper(Pore, _IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz, Fmt) when OutSz > 0, OA > 0 ->
   225		{Chip,OutBuf1,OutSz1} = chip(OA, OutBuf, OutSz),
   226		ok = pore_straw:write(Pore, Chip),
   227		true = pore:poke(Pore),
   228		{IA1,OA1} = pore_straw:avail(Pore),
   229		looper(Pore, IA1, OA1, ExpSz, InBuf, InSz, OutBuf1, OutSz1, Fmt);
   230	
   231	looper(Pore, IA, OA, undefined, InBuf, InSz, OutBuf, OutSz, Fmt) when InSz >= 4 ->
   232		{<<ExpSz:32>>,InBuf1,InSz1} = chip(4, InBuf, InSz),
   233		looper(Pore, IA, OA, ExpSz, InBuf1, InSz1, OutBuf, OutSz, Fmt);
   234	
   235	looper(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz, Fmt) when ExpSz =/= undefined, InSz >= ExpSz ->
   236		{Chip,InBuf1,InSz1} = chip(ExpSz, InBuf, InSz),
   237		deliver(Chip, Fmt),
   238		looper(Pore, IA, OA, undefined, InBuf1, InSz1, OutBuf, OutSz, Fmt);
   239	
   240	looper(Pore, IA, _OA, ExpSz, InBuf, InSz, OutBuf, OutSz, Fmt) when IA > 0 ->
   241		Data = pore_straw:read(Pore),
   242		true = pore:poke(Pore),
   243		{IA1,OA1} = pore_straw:avail(Pore),
   244		looper(Pore, IA1, OA1, ExpSz, [InBuf,Data], InSz+iolist_size(Data), OutBuf, OutSz, Fmt);
   245	
   246	looper(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz, Fmt) ->
   247		receive
   248	    {envelope,_,_} =Envelope when Fmt =:= erlang ->
   249	      EnvBin = term_to_binary(Envelope),
   250	      looper_s(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz, Fmt, EnvBin);
   251	
   252	    {envelope,Addressee,Message} when Fmt =:= json, is_atom(Addressee) ->
   253	      try
   254	        Json = [{<<"addr">>,to_bin(Addressee)},
   255	                {<<"msg">>,Message}],
   256	        EnvBin = jsx:encode(Json),
   257	        looper_s(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz, Fmt, EnvBin)
   258	      catch _:_ ->
   259	        io:format("strawman: malformed JSON: ~s\n", [Message]),
   260	        looper(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz, Fmt) end;
   261	      
   262	    {irq,Pore} ->
   263	      {IA1,OA1} = pore_straw:avail(Pore),
   264	      looper(Pore, IA1, OA1, ExpSz, InBuf, InSz, OutBuf, OutSz, Fmt) end.
   265	
   266	looper_s(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf, OutSz, Fmt, EnvBin) ->
   267			Sz = byte_size(EnvBin),
   268			OutBuf1 = [OutBuf,<<Sz:32>>,EnvBin],
   269			OutSz1 = OutSz + 4 + Sz,
   270			looper(Pore, IA, OA, ExpSz, InBuf, InSz, OutBuf1, OutSz1, Fmt).
   271	
   272	select_format(Dir1, Dir2) ->
   273	  select_format1(fmt(Dir1), fmt(Dir2)).
   274	
   275	select_format1(erlang, erlang) -> erlang;
   276	select_format1(_, _) -> json.
   277	
   278	fmt(Dir) ->
   279	  case xenstore:read(lc(Dir, "/format")) of
   280	    {ok,Fmt}  -> list_to_atom(Fmt);
   281	    {error,_} -> erlang end.
   282	
   283	deliver(Bin, erlang) ->
   284	    try
   285	      {envelope,Addressee,Message} = binary_to_term(Bin),
   286	      Addressee ! Message
   287	    catch _:_ ->
   288	      io:format("strawman: bad message: ~p\n", [Bin]) end;
   289	
   290	deliver(Bin, json) ->
   291	    try
   292	        Json = jsx:decode(Bin),
   293	        {_,AddrBin} = lists:keyfind(<<"addr">>, 1, Json),
   294	        {_,Message} = lists:keyfind(<<"msg">>, 1, Json),
   295	        Addressee = list_to_atom(binary_to_list(AddrBin)),
   296	        Addressee ! {json,Message}
   297	    catch _:_ ->
   298	        io:format("strawman: malformed JSON message: ~s\n", [Bin]) end.
   299	
   300	chip(N, Buf, Sz) when Sz =< N -> {iolist_to_binary(Buf),[],0};
   301	chip(N, Buf, Sz) when is_binary(Buf) ->
   302		<<Chip:(N)/binary,Buf1/binary>> =Buf,
   303		{Chip,Buf1,Sz-N};
   304	chip(N, Buf, Sz) -> chip(N, iolist_to_binary(Buf), Sz).
   305	
   306	to_bin(Atom) -> list_to_binary(atom_to_list(Atom)).
   307	
   308	lc(X) -> lists:concat(X).
   309	lc(X, Y) -> lists:concat([X,Y]).
```

```
     1	-module(xenstore).
     2	-define(SERVER, ?MODULE).
     3	
     4	-include("xenstore.hrl").
     5	
     6	%% ------------------------------------------------------------------
     7	%% API Function Exports
     8	%% ------------------------------------------------------------------
     9	
    10	-export([start_link/0]).
    11	-export([read/1,read_integer/1,write/2,write/3,mkdir/1,mkdir/2,delete/1,delete/2,list/1]).
    12	-export([get_perms/1,set_perms/2,set_perms/3,watch/1,unwatch/1]).
    13	-export([transaction/0,commit/1,rollback/1]).
    14	-export([domid/0]).
    15	-export([wait/2]).
    16	
    17	%% ------------------------------------------------------------------
    18	%% API Function Definitions
    19	%% ------------------------------------------------------------------
    20	
    21	start_link() ->
    22		Pid = spawn_link(fun() -> XS = pore_xs:open(),
    23								  looper(XS) end),
    24		
    25		spawn(fun() -> link(Pid),
    26					   process_flag(trap_exit, true),
    27					   receive X -> io:format("MONITOR: ~p\n", [X]) end end),
    28	
    29		register(?SERVER, Pid),
    30		{ok,Pid}.
    31	
    32	read(Key) when is_list(Key) ->
    33		call({self(),?XS_READ,Key,0});
    34	read(_) -> {error,badarg}.
    35	
    36	read_integer(Key) ->
    37		case read(Key) of {ok,S} -> try {ok,list_to_integer(S)}
    38								    catch _:_ -> {error,not_integer} end;
    39						  Error  -> Error end.
    40	
    41	write(Key, Value) -> write(Key, Value, 0).
    42	write(Key, Value, Tid) when is_integer(Value) ->
    43		write(Key, integer_to_list(Value), Tid);
    44	write(Key, Value, Tid) when is_list(Key), is_list(Value), is_integer(Tid) ->
    45		call({self(),?XS_WRITE,[Key,Value],Tid});
    46	write(_, _, _) -> {error,badarg}.
    47	
    48	mkdir(Path) -> mkdir(Path, 0).
    49	mkdir(Path, Tid) when is_list(Path), is_integer(Tid) ->
    50		call({self(),?XS_MKDIR,Path,Tid});
    51	mkdir(_, _) -> {error,badarg}.
    52	
    53	delete(Path) -> delete(Path, 0).
    54	delete(Path, Tid) when is_list(Path), is_integer(Tid) ->
    55		call({self(),?XS_RM,Path,Tid});
    56	delete(_, _) -> {error,badarg}.
    57	
    58	list(Path) when is_list(Path) ->
    59		call({self(),?XS_DIRECTORY,Path,0});
    60	list(_) -> {error,badarg}.
    61	
    62	get_perms(Path) when is_list(Path) ->
    63		call({self(),?XS_GET_PERMS,Path,0});
    64	get_perms(_) -> {error,badarg}.
    65	
    66	set_perms(Path, Perms) -> set_perms(Path, Perms, 0).
    67	set_perms(Path, [X|_] =Perms, Tid) when is_list(Path), is_list(Perms),
    68											is_list(X), is_integer(Tid) ->
    69		call({self(),?XS_SET_PERMS,[Path|Perms],Tid});
    70	set_perms(_, _, _) -> {error,badarg}.
    71	
    72	watch(Path) when is_list(Path) ->
    73		call({self(),?XS_WATCH,[Path,token()],0});
    74	watch(_) -> {error,badarg}.
    75	
    76	unwatch(Path) when is_list(Path) ->
    77		{ok,Tokens} = call({self(),unwatch,Path}),
    78		lists:foreach(fun(Token) -> call({self(),?XS_UNWATCH,[Path,Token],0}) end, Tokens);
    79	unwatch(_) -> {error,badarg}.
    80	
    81	transaction() ->
    82		call({self(),?XS_TRANSACTION_START,[],0}).
    83	
    84	commit(Tid) when is_integer(Tid) ->
    85		call({self(),?XS_TRANSACTION_END,"T",Tid});
    86	commit(_) -> {error,badarg}.
    87	
    88	rollback(Tid) when is_integer(Tid) ->
    89		call({self(),?XS_TRANSACTION_END,"F",Tid});
    90	rollback(_) -> {error,badarg}.
    91	
    92	domid() ->
    93		{ok,Value} = xenstore:read("domid"),
    94		list_to_integer(Value).
    95	
    96	%% The caller must watch() the Path before calling wait().
    97	wait(Path, Target) ->
    98		receive {watch,Path} ->
    99			case xenstore:read(Path) of
   100				{error,enoent} -> wait(Path, Target);		%% ignore
   101				{ok,Target}	   -> ok;
   102				{ok,_}		   -> wait(Path, Target);
   103				{error,_} =Err -> Err end end.
   104	
   105	call(Msg) ->
   106		case whereis(?SERVER) of
   107			undefined -> {error,not_started};
   108			Pid -> Pid ! Msg,
   109				   receive ok =X		-> X;
   110						   {ok,_} =X	-> X;
   111						   {error,_} =X -> X end end.
   112	
   113	%% ------------------------------------------------------------------
   114	
   115	looper(XS) -> looper(XS, ?XS_RING_SIZE, 0, undefined, [], 0, [], 0, undefined, [], {[],1}, []).
   116	
   117	looper(XS, QA, _RA, ExpSz, InBuf, InSz, OutBuf, OutSz, HH, Calls, RR, Ws) when OutSz > 0, QA > 0 ->
   118		{Chip,OutBuf1,OutSz1} = chip(QA, OutBuf, OutSz),
   119		ok = pore_xs:write(XS, Chip),
   120		true = pore:poke(XS),
   121		{QA1,RA1} = pore_xs:avail(XS),
   122		looper(XS, QA1, RA1, ExpSz, InBuf, InSz, OutBuf1, OutSz1, HH, Calls, RR, Ws);
   123	
   124	looper(XS, QA, RA, undefined, InBuf, InSz, OutBuf, OutSz, undefined, Calls, RR, Ws) when InSz >= 16 ->
   125		{<<Op:32/little,
   126		   Rid:32/little,
   127		   Tid:32/little,
   128		   ExpSz:32/little>>,InBuf1,InSz1} = chip(16, InBuf, InSz),
   129		looper(XS, QA, RA, ExpSz, InBuf1, InSz1, OutBuf, OutSz, {Op,Rid,Tid}, Calls, RR, Ws);
   130	
   131	looper(XS, QA, RA, ExpSz, InBuf, InSz, OutBuf, OutSz, {Op,Rid,_} =HH, Calls, RR, Ws)
   132											when ExpSz =/= undefined, InSz >= ExpSz ->
   133		{Chip,InBuf1,InSz1} = chip(ExpSz, InBuf, InSz),
   134		RR1 = rrid(Rid, RR),
   135		%% Chip always ends with zero
   136		Resp = unpack(op_tag(Op), binary_to_list(chomp(Chip))),
   137		looper1(Resp, XS, QA, RA, InBuf1, InSz1, OutBuf, OutSz, HH, Calls, RR1, Ws);
   138	
   139	looper(XS, _QA, RA, ExpSz, InBuf, InSz, OutBuf, OutSz, HH, Calls, RR, Ws) when RA > 0 ->
   140		Data = pore_xs:read(XS),
   141		true = pore:poke(XS),
   142		{QA1,RA1} = pore_xs:avail(XS),
   143		looper(XS, QA1, RA1, ExpSz, [InBuf,Data], InSz + iolist_size(Data), OutBuf, OutSz, HH, Calls, RR, Ws);
   144	
   145	looper(XS, QA, RA, ExpSz, InBuf, InSz, OutBuf, OutSz, HH, Calls, RR, Ws) ->
   146		receive
   147		{From,Op,PL,Tid} ->
   148			{Rid,RR1} = rid(RR),
   149			Trailer = trailer(Op, PL),
   150			TSz = iolist_size(Trailer),
   151			SockMsg = <<Op:32/little,Rid:32/little,Tid:32/little,TSz:32/little>>,
   152			OutBuf1 = [OutBuf,SockMsg,Trailer],
   153			OutSz1 = OutSz + 16 + TSz,
   154			Calls1 = [{From,Rid,PL}|Calls],
   155			looper(XS, QA, RA, ExpSz, InBuf, InSz, OutBuf1, OutSz1, HH, Calls1, RR1, Ws);
   156	
   157		{From,unwatch,Path} ->
   158			{Ws1,Ws2} =
   159			lists:partition(fun({_,Path1,From1}) -> From1 =:= From andalso
   160													Path1 =:= Path end, Ws),
   161			Tokens = [ Token || {Token,_,_} <- Ws1 ],
   162			From ! {ok,Tokens},
   163			looper(XS, QA, RA, ExpSz, InBuf, InSz, OutBuf, OutSz, HH, Calls, RR, Ws2);
   164	
   165		{irq,XS} ->
   166			{QA1,RA1} = pore_xs:avail(XS),
   167			looper(XS, QA1, RA1, ExpSz, InBuf, InSz, OutBuf, OutSz, HH, Calls, RR, Ws) end.
   168	
   169	looper1({ok,[Path,Token]}, XS, QA, RA, InBuf, InSz, OutBuf, OutSz, {?XS_WATCH_EVENT,_,_}, Calls, RR, Ws) ->
   170		case lists:keyfind(Token, 1, Ws) of
   171			{_,_,From} -> From ! {watch,Path}; _ -> ok end,
   172		looper(XS, QA, RA, undefined, InBuf, InSz, OutBuf, OutSz, undefined, Calls, RR, Ws);
   173	
   174	looper1(ok, XS, QA, RA, InBuf, InSz, OutBuf, OutSz, {?XS_WATCH,Rid,_}, Calls, RR, Ws) ->
   175		{value,{From,_,[Path,Token]},Calls1} = lists:keytake(Rid, 2, Calls),
   176		From ! ok,
   177		Ws1 = [{Token,Path,From}|Ws],
   178		looper(XS, QA, RA, undefined, InBuf, InSz, OutBuf, OutSz, undefined, Calls1, RR, Ws1);
   179	
   180	looper1(Resp, XS, QA, RA, InBuf, InSz, OutBuf, OutSz, {_,Rid,_}, Calls, RR, Ws) ->
   181		{value,{From,_,_},Calls1} = lists:keytake(Rid, 2, Calls),
   182		From ! Resp,
   183		looper(XS, QA, RA, undefined, InBuf, InSz, OutBuf, OutSz, undefined, Calls1, RR, Ws).
   184	
   185	chip(N, Buf, Sz) when Sz =< N -> {iolist_to_binary(Buf),[],0};
   186	chip(N, Buf, Sz) when is_binary(Buf) ->
   187		<<Chip:(N)/binary,Buf1/binary>> =Buf,
   188		{Chip,Buf1,Sz-N};
   189	chip(N, Buf, Sz) -> chip(N, iolist_to_binary(Buf), Sz).
   190	
   191	op_tag(?XS_ERROR)	  -> error;
   192	op_tag(?XS_READ)	  -> read;
   193	op_tag(?XS_WRITE)	  -> write;
   194	op_tag(?XS_MKDIR)	  -> mkdir;
   195	op_tag(?XS_RM)		  -> rm;
   196	op_tag(?XS_DIRECTORY) -> directory;
   197	op_tag(?XS_GET_PERMS) -> get_perms;
   198	op_tag(?XS_SET_PERMS) -> set_perms;
   199	op_tag(?XS_WATCH)	  -> watch;
   200	op_tag(?XS_UNWATCH)	  -> unwatch;
   201	op_tag(?XS_WATCH_EVENT) 	  -> watch_event;
   202	op_tag(?XS_TRANSACTION_START) -> transaction_start;
   203	op_tag(?XS_TRANSACTION_END)   -> transaction_end.
   204	
   205	trailer(?XS_WRITE, [P,V]) ->
   206		list_to_binary([P,0,V]);	 %% XS_WRITE is special
   207	trailer(_, [V|_] = PL) when is_list(V) ->
   208		list_to_binary([ [X,0] || X <- PL ]);
   209	trailer(_, V) ->
   210		list_to_binary([V,0]).
   211	
   212	unpack(error, What) ->
   213		{error,list_to_atom(string:to_lower(What))};
   214	unpack(transaction_start, What) ->
   215		{ok,list_to_integer(What)};
   216	unpack(directory, What) ->
   217		{ok,string:tokens(What, [0])};
   218	unpack(_Tag, "OK") -> ok;
   219	unpack(_Tag, What) ->
   220		case lists:member(0, What) of
   221			false -> {ok,What};
   222			true  -> {ok,string:tokens(What, [0])} end.
   223	
   224	rid({[],NR}) -> {NR,{[],NR+1}};
   225	rid({[Rid|Rids],NR}) -> {Rid,{Rids,NR}}.
   226	
   227	rrid(Rid, {Rids,NR}) -> {[Rid|Rids],NR}.
   228	
   229	token() ->
   230		<<A,B,C,D,E,F,G,H>> = crypto:rand_bytes(8),
   231		lists:flatten(io_lib:format("~.16b~.16b~.16b~.16b~.16b~.16b~.16b~.16b", [A,B,C,D,E,F,G,H])).
   232	
   233	chomp(Bin) ->
   234		case binary:last(Bin) of
   235			0 -> binary:part(Bin, 0, byte_size(Bin)-1);
   236			_ -> Bin end.
   237	
```
