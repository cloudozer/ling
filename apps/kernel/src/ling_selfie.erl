-module(ling_selfie).
-export([make/0, share/1]).

-define(PAGE_SIZE,	4096).

-define(Elf_Half, 2).
-define(Elf_Word, 4).
-define(Elf_Sword, 4). % signed

-define(Elf32_Addr, 4).
-define(Elf32_Off,  4).

-define(Elf64_Xword, 8).
-define(Elf64_Sxword, 8). % signed
-define(Elf64_Addr, 8).
-define(Elf64_Off,  8).

-define(ELFCLASS32, 1).
-define(ELFCLASS64, 2).

-define(ELFDATA2LSB, 1).
-define(ELFDATA2MSB, 2).

-define(ELFOSABI_SYSV, 0).
-define(ELFOSABI_STANDALONE, 255).

-define(ET_EXEC, 2).  % executable file

-define(EV_CURRENT, 1).	% ELF version

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

% for testing
-export([make_elf_ident/1, make_elf_skeleton/3, materialize_sections/1]).
-export([make_symbols/1, make_strtab/1, make_symtab/2]).
-export([pad_to/2, info/0]).

-record(elfmeta, {class, data, machine}).

elf_comment() ->
	Signiture = erlang:system_info(system_version),
	list_to_binary(io_lib:format("Made by ~s, (c) Cloudozer\n", [Signiture])).

elf_xenguest_contents() ->
	<<"GUEST_OS=Ling,XEN_VER=xen-3.0,VIRT_BASE=0x0,ELF_PADDR_OFFSET=0x0,HYPERCALL_PAGE=0x1,LOADER=generic">>.

elf_class(#elfmeta{ class = 32 }) -> ?ELFCLASS32;
elf_class(#elfmeta{ class = 64 }) -> ?ELFCLASS64.

elf_hdr_size(?ELFCLASS64) -> 64.
elf_phentsize(?ELFCLASS64) -> 56.
elf_shentsize(?ELFCLASS64) -> 64.

-record(elfsects, {text, rodata, eh_frame, data, bss,
                   xen_guest, comment, shstrtab, symtab, strtab}).

elf_sect_num() -> record_info(size, elfsects) - 1.

-record(elfsecthdr,	%% section header
	{name, type, flags, addr, offset, size, link = 0, info = 0, addralign, entsize = 0, contents}).

-record(elfsym, %% ~ symtab entry
	{name, bindntype, sectndx, value, size}).

align(Addr, To) when Addr rem To =:= 0 -> Addr;
align(Addr, To) -> To * (Addr div To + 1).

pad_to(Bin, Size) when is_binary(Bin) ->
	PadSize = Size - size(Bin),
	<<Bin/binary, 0:PadSize>>.



make_elf_ident(Meta) when is_record(Meta, elfmeta) ->
	EIclass = elf_class(Meta),
	EIdata = case Meta#elfmeta.data of lsb2compl -> ?ELFDATA2LSB; msb2compl -> ?ELFDATA2MSB end,
	pad_to(<<16#7f, $E, $L, $F, EIclass, EIdata, ?EV_CURRENT, ?ELFOSABI_STANDALONE, 0>>, 16).

make_elf_header(Meta, EntryAddr, PHT, SHTOff) when is_record(Meta, elfmeta) ->
	lsb2compl = Meta#elfmeta.data,
	64 = Meta#elfmeta.class,
	Eclass = elf_class(Meta),
	E_flags = 0,
	E_ident = make_elf_ident(Meta),
	16 = size(E_ident),
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
		(elf_sect_num()):?Elf_Half/little,
		(#elfsects.shstrtab - 2):?Elf_Half/little
	>>,
	HdrSz = size(ElfHdr),
	HdrSz = elf_hdr_size(elf_class(Meta)),
	ElfHdr.

%% calculate offsets
make_elf_skeleton(Meta, Sections, PHT) when is_record(Meta, elfmeta) ->
	Eclass = elf_class(Meta),
	HdrsLen = elf_hdr_size(Eclass) + elf_phentsize(Eclass) * length(PHT),

	SectionEnd = fun(#elfsecthdr{flags=Flags, offset=Off}) when Flags band ?SHF_ALLOC =:= 0 -> Off;
	                (#elfsecthdr{offset=Off, size=Siz}) -> Off + Siz
	             end,

	SectionsWithOffsReversed = lists:foldl(
		fun(Sect0, [PrevSect0 | SectAcc]) ->
			AlignedOffset = align(SectionEnd(PrevSect0), Sect0#elfsecthdr.addralign),
			PrevSize = AlignedOffset - PrevSect0#elfsecthdr.offset,
			PrevSect = PrevSect0#elfsecthdr{ size=PrevSize },
			Sect = Sect0#elfsecthdr{ offset=AlignedOffset },
			[Sect, PrevSect | SectAcc]
		end,
		[#elfsecthdr{offset=0, size=HdrsLen}],
		Sections),
	[_Zero | SectionsWithOffs] = lists:reverse(SectionsWithOffsReversed),

	LastSect = lists:last(SectionsWithOffs),
	SHTOff = align(SectionEnd(LastSect), 16),
	EOFOff = SHTOff + elf_shentsize(Eclass) * elf_sect_num(),

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
			           STValue:?Elf64_Addr/little, STSize:?Elf64_Xword/little>>,
			<<Bin/binary, SymBin/binary>>
		end,
		<<>>, Syms).

make_elf_pht(_PHTNames) ->
	<<"TODO">>.

make_elf_segments(_ElfSegs) ->
	<<"TODO">>.

materialize_sections(Sections) ->
	io:format("materialize_sections(~p)\n", [Sections]),
	{<<".text">>, {TextStart, TextEnd}} = proplists:lookup(<<".text">>, Sections),
	{<<".rodata">>, {RodataStart, RodataEnd}} = proplists:lookup(<<".rodata">>, Sections),
	{<<".data">>, {DataStart, DataEnd}} = proplists:lookup(<<".data">>, Sections),
	{<<".bss">>, {BssStart, BssEnd}} = proplists:lookup(<<".bss">>, Sections),
	io:format("materialize_sections: matched\n"),
	TextRet = ling:memory(TextStart, TextEnd),
	io:format("materialize_sections: TextRet=~p\n", [TextRet]),
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
		addr = RodataStart, size = RodataStart - RodataStart, addralign = 32,
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
	Zero = #elfsecthdr{name=(<<>>), type=?SHT_NULL, flags=0, addr=0, size=0,
	                   link=0, info=0, addralign=0},

	MakeSHTEntry =
		fun(#elfsecthdr{name=SectName, type=Type, flags=Flags, addr=Addr,
		                offset=Off, size=Siz, link=Lnk, info=Inf, addralign=Align,
		                entsize=Entsiz}) ->
			{SectName, NameOff} = proplists:lookup(SectName, ShstrtabOffs),
			Bin = case Eclass of
				?ELFCLASS64 ->
					<<NameOff:?Elf_Word/little, Type:?Elf_Word/little,
			          Flags:?Elf64_Xword/little, Addr:?Elf64_Addr/little, Off:?Elf64_Off/little,
				 	  Siz:?Elf64_Xword/little, Lnk:?Elf_Word/little, Inf:?Elf_Word/little,
					  Align:?Elf64_Xword/little, Entsiz:?Elf64_Xword/little>>
				end,
			BinSiz = size(Bin), BinSiz = elf_shentsize(Eclass),
			Bin
		end,

	lists:foldl(
		fun(Section, AccBin) -> <<AccBin/binary, (MakeSHTEntry(Section))/binary>> end,
		<<>>, [Zero | AllSections]).

make_elf(Meta, Sections) ->
	Eclass = elf_class(Meta),

	PHT = [<<"LOAD">>, <<"GNU_STACK">>],

	{ShstrtabOffs, AllSectionsNoOffs} = materialize_sections(Sections),
	{AllSections, SHTOff, _EOFOff} = make_elf_skeleton(Meta, AllSectionsNoOffs, PHT),

	{<<".text">>, {EntryAddr, _}} = proplists:lookup(<<".text">>, Sections),
	ElfHdr = make_elf_header(Meta, EntryAddr, PHT, SHTOff),

	ElfPHT = make_elf_pht(PHT),

	[FstSect | _] = AllSections,
	SegsPadding = FstSect#elfsecthdr.offset - size(ElfHdr) - size(ElfPHT),

	ElfSegsBin = make_elf_segments(AllSections),
	ElfSHT = make_elf_sht(Eclass, AllSections, ShstrtabOffs),

	<<ElfHdr/binary, ElfPHT/binary, 0:SegsPadding, ElfSegsBin/binary, ElfSHT/binary>>.

info() ->
	io:format("info()\n"),
	case ling:exec_info() of
	{elf, Meta, LSections} ->
		io:format("info(): elf\n"),
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


share(_Addr) -> todo.
