-module(ling_selfie).
%%-export([make/0, share/1]).
-compile(export_all).

-define(PAGE_SIZE,	4096).

%% bits in ELF binary types:
-define(Elf_Half,   16).
-define(Elf_Word,   32).
-define(Elf_Sword,  32). % signed
-define(Elf_Xword,  64).
-define(Elf_Sxword, 64). % signed

-define(Elf32_Addr, 32).
-define(Elf32_Off,  32).

-define(Elf64_Addr, 64).
-define(Elf64_Off,  64).

%% ELF header fields constants
-define(ELFCLASS32, 1).
-define(ELFCLASS64, 2).

-define(ELFDATA2LSB, 1).
-define(ELFDATA2MSB, 2).

-define(ELFOSABI_SYSV, 0).
-define(ELFOSABI_STANDALONE, 255).

-define(ET_EXEC, 2).  % executable file

-define(EV_CURRENT, 1).	% ELF version

%% ELF section headers bits
-define(SHT_NULL,     0).
-define(SHT_PROGBITS, 1).
-define(SHT_SYMTAB,   2).
-define(SHT_STRTAB,   3).
-define(SHT_NOTE,     7).
-define(SHT_NOBITS,   8).

-define(STB_LOCAL,  (0 bsl 4)).
-define(STB_GLOBAL, (1 bsl 4)).
-define(STB_WEAK,   (2 bsl 4)).

-define(STT_NOTYPE, 0).
-define(STT_OBJECT, 1).
-define(STT_FUNC,   2).
-define(STT_SECTION, 3).
-define(STT_FILE,   4).

-define(SHN_UNDEF,  0).
-define(SHN_ABS,    16#fff1).
-define(SHN_COMMON, 16#fff2).

-define(SHF_WRITE,   (1 bsl 0)).
-define(SHF_ALLOC,   (1 bsl 1)).
-define(SHF_EXEC,    (1 bsl 2)).
-define(SHF_STRINGS, (1 bsl 5)).

%% ELF program header table constants:
-define(PT_NULL,     0).
-define(PT_LOAD,     1).
-define(PT_GNU_STACK, 16#6474e651).
-define(PF_X,        1).
-define(PF_W,        2).
-define(PF_R,        4).


align(Addr, To) when Addr rem To =:= 0 -> Addr;
align(Addr, To) -> To * (Addr div To + 1).

pad_to(Bin, Size) when is_binary(Bin) ->
	PadSize = Size - size(Bin),
	<<Bin/binary, 0:(PadSize * 8)>>.


-record(elfmeta, {class, data, machine}).

elf_class(#elfmeta{ class = 32 }) -> ?ELFCLASS32;
elf_class(#elfmeta{ class = 64 }) -> ?ELFCLASS64.

elf_hdr_size(?ELFCLASS64) -> 64.
elf_phentsize(?ELFCLASS64) -> 56.
elf_shentsize(?ELFCLASS64) -> 64.

-record(elfsecthdr,	%% section header
	{name, type = ?SHT_NULL, flags = 0, addr, offset, size,
	 link = 0, info = 0, addralign, entsize = 0, contents}).

-record(elfproghdr, %% program header
	{type, flags=?PF_R bor ?PF_W bor ?PF_X, offset=0, vaddr = 0, paddr = 0,
	 filesz = 0, memsz = 0, align = 16#10, sections=[] }).

-record(elfsym, %% ~ symtab entry
	{name, bindntype, sectndx, value, size}).



elf_comment() ->
	Signiture = erlang:system_info(system_version),
	list_to_binary(io_lib:format("Made by ~s, (c) Cloudozer\n", [Signiture])).

elf_xenguest_contents() ->
	<<"GUEST_OS=Ling,XEN_VER=xen-3.0,VIRT_BASE=0x0,ELF_PADDR_OFFSET=0x0,HYPERCALL_PAGE=0x1,LOADER=generic">>.


make_elf_ident(Meta) when is_record(Meta, elfmeta) ->
	EIclass = elf_class(Meta),
	EIdata = case Meta#elfmeta.data of lsb2compl -> ?ELFDATA2LSB; msb2compl -> ?ELFDATA2MSB end,
	pad_to(<<16#7f, $E, $L, $F, EIclass, EIdata, ?EV_CURRENT, ?ELFOSABI_STANDALONE, 0>>, 16).

section_index_by_name(_Name, [], _) ->
	{error, not_found};
section_index_by_name(Name, [#elfsecthdr{name=Name} | _], Idx) ->
	{ok, Idx};
section_index_by_name(Name, [_ | Sections], Idx) ->
	section_index_by_name(Name, Sections, Idx+1).

make_elf_header(Meta, EntryAddr, PHT, Sections, SHTOff) when is_record(Meta, elfmeta) ->
	Eclass = elf_class(Meta),
	lsb2compl = Meta#elfmeta.data,
	64 = Meta#elfmeta.class,
	E_flags = 0,

	E_ident = make_elf_ident(Meta),
	16 = size(E_ident),
	{ok, ShstrtabNdx} = section_index_by_name(<<".shstrtab">>, Sections, 1),

	ElfHdr = << E_ident/binary, ?ET_EXEC:?Elf_Half/little,
		(Meta#elfmeta.machine):?Elf_Half/little,
		?EV_CURRENT:?Elf_Word/little,
		EntryAddr:?Elf64_Addr/little,
		(elf_hdr_size(Eclass)):?Elf64_Off/little,
		SHTOff:?Elf64_Off/little,
		E_flags:?Elf_Word/little,
		(elf_hdr_size(Eclass)):?Elf_Half/little,
		(elf_phentsize(Eclass)):?Elf_Half/little,
		(length(PHT)):?Elf_Half/little,
		(elf_shentsize(Eclass)):?Elf_Half/little,
		(length(Sections) + 1):?Elf_Half/little,
		ShstrtabNdx:?Elf_Half/little
	>>,
	HdrSz = size(ElfHdr),
	HdrSz = elf_hdr_size(elf_class(Meta)),
	ElfHdr.

%% calculate offsets
make_elf_skeleton(Meta, Sections, PHT) when is_record(Meta, elfmeta) ->
	Eclass = elf_class(Meta),
	HdrsLen = elf_hdr_size(Eclass) + elf_phentsize(Eclass) * length(PHT),

	EndOffset =
		fun(#elfsecthdr{offset=Off, contents=undefined}) -> Off;
		   (#elfsecthdr{offset=Off, contents=Bin}) -> Off + size(Bin)
		end,

	SectionsWithOffsReversed = lists:foldl(
		fun(Sect0, [PrevSect | SectAcc]) ->
			Sect = Sect0#elfsecthdr{ offset=EndOffset(PrevSect) },
			[Sect, PrevSect | SectAcc]
		end,
		[#elfsecthdr{offset=align(HdrsLen, ?PAGE_SIZE)}],
		Sections),
	[_Zero | SectionsWithOffs] = lists:reverse(SectionsWithOffsReversed),

	LastSect = lists:last(SectionsWithOffs),
	SHTOff = align(EndOffset(LastSect), 16),
	EOFOff = SHTOff + elf_shentsize(Eclass) * (1 + length(Sections)),

	{SectionsWithOffs, SHTOff, EOFOff}.

make_symbols(Sections) ->
	ZeroSym = #elfsym {name=(<<>>), bindntype=(?STT_NOTYPE bor ?STB_LOCAL),
	                   sectndx=?SHN_UNDEF, value=0, size=0},

	{SectEntries, _} = lists:foldl(
		fun({_SectName, {Start, _End}}, {Syms, Num}) ->
			Sym = #elfsym {name=(<<"">>), bindntype=(?STT_SECTION bor ?STB_LOCAL),
			               sectndx=Num, value=Start, size=0},
			{[Sym | Syms], Num + 1}
		end,
		{[], 1},
		Sections),

	SectSyms = lists:foldl(
		fun({SectName, {Start, End}}, Syms) ->
			SymInf = fun(Sym, Ndx, Off) ->
					#elfsym {name=Sym, bindntype=(?STT_NOTYPE bor ?STB_GLOBAL),
					         sectndx=Ndx, value=Off, size=0}
				end,
			SymsForSect =
				fun(Ndx, Sym1, Sym2) ->
					[SymInf(Sym1, Ndx, Start), SymInf(Sym2, Ndx, End)]
				end,
			case SectName of
			<<".text">>   -> SymsForSect(1, <<"_text">>, <<"_etext">>) ++ Syms;
			<<".rodata">> -> SymsForSect(2, <<"_rodata">>, <<"_erodata">>) ++ Syms;
			<<".data">>   -> SymsForSect(3, <<"_data">>, <<"_edata">>) ++ Syms;
			<<".bss">>    -> SymsForSect(4, <<"_bss">>, <<"_ebss">>) ++ Syms
			end
		end,
		[], Sections),

	[ZeroSym] ++ lists:reverse(SectEntries) ++ lists:reverse(SectSyms).

make_strtab(Symnames) ->
	{StrOffs, _Len2, Strtab} = lists:foldl(
		fun(Name, {Offs, Off, Bin}) ->
			{[{Name, Off} | Offs],
			  Off + size(Name) + 1,
			  <<Bin/binary, Name/binary, 0>>}
		end,
		{[], 1, <<0>>},
		Symnames),
	%% StrOffs is a proplist from binary names to offsets in strtab
	{ok, StrOffs, Strtab}.

make_symtab(Syms, StrOffs) ->
	lists:foldl(
		fun(#elfsym{name=Name, bindntype=STInfo, sectndx=STShNdx, value=STValue, size=STSize}, Bin) ->
			StrOff = case proplists:lookup(Name, StrOffs) of
				{Name, Off} -> Off;
				none -> 0
			end,
			SymBin = <<StrOff:?Elf_Word/little, STInfo, 0, STShNdx:?Elf_Half/little,
			           STValue:?Elf64_Addr/little, STSize:?Elf_Xword/little>>,
			<<Bin/binary, SymBin/binary>>
		end,
		<<>>, Syms).

pht_with_offsets(PHT, Sections) ->
	GetSections =
		fun(PHSects) ->
			Memb = fun(#elfsecthdr{name=SectName}) -> lists:member(SectName, PHSects) end,
			lists:filter(Memb, Sections)
		end,
	lists:map(
		fun(ProgHdr) ->
			SegmSects = GetSections(ProgHdr#elfproghdr.sections),
			case SegmSects of
				[] -> ProgHdr;
				[FstSect | _] ->
					%% the order of sections must be the offset order
					LstSect = lists:last(SegmSects),
					StartOff = FstSect#elfsecthdr.offset,
					EndOff = LstSect#elfsecthdr.offset + LstSect#elfsecthdr.size,

					Addr = lists:min(lists:map(fun(#elfsecthdr{addr=A}) -> A end, SegmSects)),
					EndAddr = lists:max(lists:map(fun(#elfsecthdr{addr=A,size=S}) -> A + S end,
					                              SegmSects)),
					ProgHdr#elfproghdr{
						offset=StartOff, vaddr=Addr, paddr=Addr,
						filesz=(EndOff - StartOff), memsz=(EndAddr - Addr) }
			end
		end,
		PHT).

make_elf_pht(Eclss, PHT) ->
	Iter =
		fun(#elfproghdr{type=PType,flags=PFlags,offset=POff,
		                vaddr=VAddr,paddr=PAddr,filesz=PFileSz,memsz=PMemSz,
		                align=PAlign}, AccBin) ->
			Eclss = ?ELFCLASS64,
			PHBin = <<PType:?Elf_Word/little, PFlags:?Elf_Word/little,
			          POff:?Elf64_Off/little, VAddr:?Elf64_Addr/little, PAddr:?Elf64_Addr/little,
			          PFileSz:?Elf_Xword/little, PMemSz:?Elf_Xword/little, PAlign:?Elf_Xword>>,
			PHSize = size(PHBin), PHSize = elf_phentsize(Eclss),
			<<AccBin/binary, PHBin/binary>>
		end,
	lists:foldl(Iter, <<>>, PHT).

make_elf_segments(ElfSegs) ->
	lists:foldl(
		fun(#elfsecthdr{contents=undefined}, AccBin) ->
				AccBin;
		   (#elfsecthdr{contents=SectBin},   AccBin) ->
				<<AccBin/binary, SectBin/binary>>
		end,
		<<>>,
		ElfSegs).

materialize_sections(Sections) ->
	io:format("materialize_sections(~p)\n", [Sections]),
	{<<".text">>, {TextStart, TextEnd}} = proplists:lookup(<<".text">>, Sections),
	{<<".rodata">>, {RodataStart, RodataEnd}} = proplists:lookup(<<".rodata">>, Sections),
	{<<".data">>, {DataStart, DataEnd}} = proplists:lookup(<<".data">>, Sections),
	{<<".bss">>, {BssStart, BssEnd}} = proplists:lookup(<<".bss">>, Sections),
	io:format("materialize_sections: matched\n"),
	TextRet = ling:memory(TextStart, TextEnd),
	{ok, TextBin} = TextRet,
	{ok, RodataBin} = ling:memory(RodataStart, RodataEnd),
	{ok, DataBin} = ling:memory(DataStart, DataEnd),
	io:format("materialize_sections: binaries loaded\n"),

	Text = #elfsecthdr { name = <<".text">>,
		type = ?SHT_PROGBITS, flags = ?SHF_ALLOC bor ?SHF_EXEC,
		addr = TextStart, size = TextEnd - TextStart, addralign = ?PAGE_SIZE,
		contents = TextBin
	},
	Rodata = #elfsecthdr { name = <<".rodata">>,
		type = ?SHT_PROGBITS, flags = ?SHF_ALLOC,
		addr = RodataStart, size = RodataEnd - RodataStart, addralign = 32,
		contents = RodataBin
	},
	Data = #elfsecthdr { name = <<".data">>,
		type = ?SHT_PROGBITS, flags = ?SHF_ALLOC bor ?SHF_WRITE,
		addr = DataStart, size = DataEnd - DataStart, addralign = 32,
		contents = DataBin
	},
	Bss = #elfsecthdr { name = <<".bss">>,
		type = ?SHT_NOBITS, flags = ?SHF_ALLOC bor ?SHF_WRITE,
		addr = BssStart, size = BssEnd - BssStart, addralign = 32,
		contents = <<>>
	},

	XenGuestBin = elf_xenguest_contents(),
	XenGuest = #elfsecthdr { name = <<"__xen_guest">>,
		type = ?SHT_PROGBITS, flags = 0,
		addr = 0, size = size(XenGuestBin), addralign = 1,
		contents = XenGuestBin
	},

	CommentBin = elf_comment(),
	Comment = #elfsecthdr { name = <<".comment">>,
		type = ?SHT_PROGBITS, flags = ?SHF_STRINGS,
		addr = 0, size = size(CommentBin), addralign = 1,
		contents = CommentBin
	},

	Shstrtab0 = #elfsecthdr { name = <<".shstrtab">>,
		type = ?SHT_STRTAB, flags = 0,
		addr = 0, addralign = 1
	},

	Syms = make_symbols(Sections),
	Symnames = lists:filter(fun(<<>>) -> false; (_) -> true end,
	                        lists:map(fun(#elfsym{name=Name}) -> Name end, Syms)),
	io:format("Symnames = ~p\n", [Symnames]),
	{ok, StrOffs, StrtabBin} = make_strtab(Symnames),
	Strtab = #elfsecthdr { name = <<".strtab">>,
		type = ?SHT_STRTAB, flags = 0,
		addr = 0, size = size(StrtabBin), addralign = 1,
		contents = StrtabBin
	},

	SymtabBin = make_symtab(Syms, StrOffs),
	Symtab0 = #elfsecthdr { name = <<".symtab">>,
		type = ?SHT_SYMTAB, flags = 0,
		addr = 0, size = size(SymtabBin), addralign = 8,
		entsize = size(SymtabBin) div length(Syms), contents = SymtabBin
	},

	AllSections0 = [Text, Rodata, Data, Bss, XenGuest, Comment, Shstrtab0, Symtab0, Strtab],

	SectNames = lists:map(fun(#elfsecthdr{name=Name}) -> Name end, AllSections0),
	{ok, ShstrOffs, ShstrtabBin} = make_strtab(SectNames),
	Shstrtab = Shstrtab0#elfsecthdr{ contents = ShstrtabBin, size = size(ShstrtabBin) },

	Symtab = Symtab0,  % #elfsecthdr { link = StrtabNdx, info = 0},

	AllSections = [Text, Rodata, Data, Bss, XenGuest, Comment, Shstrtab, Symtab, Strtab],
	{ShstrOffs, AllSections}.


make_elf_sht(Eclass, AllSections, ShstrtabOffs) ->
	Zero = #elfsecthdr{name=(<<>>), type=?SHT_NULL, flags=0, addr=0, offset=0, size=0,
	                   link=0, info=0, addralign=0},

	MakeSHTEntry =
		fun(#elfsecthdr{name=SectName, type=Type, flags=Flags, addr=Addr,
		                offset=Off, size=Siz, link=Lnk, info=Inf, addralign=Align,
		                entsize=Entsiz}) ->
			NameOff = case proplists:lookup(SectName, ShstrtabOffs) of
						{SectName, NOff} -> io:format("shstrtab(~p) -> ~p;\n", [SectName, NOff]), Off;
						none -> 0
						end,
			Bin = case Eclass of
				?ELFCLASS64 ->
					<<NameOff:?Elf_Word/little, Type:?Elf_Word/little,
			          Flags:?Elf_Xword/little, Addr:?Elf64_Addr/little, Off:?Elf64_Off/little,
				 	  Siz:?Elf_Xword/little, Lnk:?Elf_Word/little, Inf:?Elf_Word/little,
					  Align:?Elf_Xword/little, Entsiz:?Elf_Xword/little>>
				end,
			BinSiz = size(Bin), BinSiz = elf_shentsize(Eclass),
			Bin
		end,

	lists:foldl(
		fun(Section, AccBin) -> <<AccBin/binary, (MakeSHTEntry(Section))/binary>> end,
		<<>>, [Zero | AllSections]).

initial_pht() -> [
	#elfproghdr{ type=?PT_LOAD, sections=[<<".text">>,<<".rodata">>,<<".data">>,<<".bss">>]},
    #elfproghdr{ type=?PT_GNU_STACK } ].

make_elf(Meta, Sections) ->
	Eclass = elf_class(Meta),
	PHT0 = initial_pht(),

	{ShstrtabOffs, AllSectionsNoOffs} = materialize_sections(Sections),

	{AllSections, SHTOff, _EOFOff} = make_elf_skeleton(Meta, AllSectionsNoOffs, PHT0),

	PHT = pht_with_offsets(PHT0, AllSections),

	{<<".text">>, {EntryAddr, _}} = proplists:lookup(<<".text">>, Sections),
	ElfHdr = make_elf_header(Meta, EntryAddr, PHT, AllSections, SHTOff),

	ElfPHT = make_elf_pht(Eclass, PHT),

	[#elfsecthdr{offset=SectOff} | _] = AllSections,
	SegsPadding = 8 * (SectOff - size(ElfHdr) - size(ElfPHT)),

	ElfHead = <<ElfHdr/binary, ElfPHT/binary, 0:SegsPadding>>,
	SectOff = size(ElfHead),

	ElfBody = pad_to(make_elf_segments(AllSections), SHTOff - SectOff),
	SHTOff = SectOff + size(ElfBody),

	ElfSHT = make_elf_sht(Eclass, AllSections, ShstrtabOffs),

	<<ElfHead/binary, ElfBody/binary, ElfSHT/binary>>.

info() ->
	case ling:exec_info() of
	{elf, LMeta, LSections} ->
		{class, MClass} = proplists:lookup(class, LMeta),
		{data,  MData} = proplists:lookup(data, LMeta),
		{machine, MMach} = proplists:lookup(machine, LMeta),
		Meta = #elfmeta{ class=MClass, data=MData, machine=MMach },
		Sections = lists:map(fun({N, S, E}) -> {erlang:list_to_binary(N), {S, E}} end, LSections),
		{elf, Meta, Sections};
	{Fmt, _,    _} ->
		{error, {unknown_format, Fmt}};
	{error, E} ->
		{error, E};
	_ -> {error, whut}
	end.

make() ->
	io:format("make()\n"),
	case info() of
	{elf, Meta, Sections} ->
		make_elf(Meta, Sections);
	Other -> Other
	end.

share(Image, Addr, Port) ->
	{ok, S} = gen_tcp:connect(Addr, Port, []),
	ChunkSize = 4096,
	Chew = fun
		(_Self, Chunk) when size(Chunk) < ChunkSize ->
			ok = gen_tcp:send(S, Chunk);
		(Self, <<Chunk:ChunkSize/binary, Rest/binary>>) ->
			ok = gen_tcp:send(S, Chunk),
			Self(Self, Rest)
		end,
	Chew(Chew, Image),
	ok = gen_tcp:close(S),
	ok.
