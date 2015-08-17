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

% for testing
-export([make_elf_ident/1]).
-export([make_shstrtab/0]).

-record(elfmeta, {class, data, machine}).

elf_class(#elfmeta{ class = 32 }) -> ?ELFCLASS32;
elf_class(#elfmeta{ class = 64 }) -> ?ELFCLASS64.

elf_hdr_size(?ELFCLASS64) -> 64.
elf_phentsize(?ELFCLASS64) -> 56.
elf_shentsize(?ELFCLASS64) -> 64.

-record(elfsects, {text, rodata, eh_frame, data, bss,
                   xen_guest, comment, shstrtab, symtab, strtab}).

elf_sect_num() -> record_info(size, elfsects) - 1.

-record(elfoffs, {
		hdr,	%% ELF header
		pht, 	%% Program Header Table
		segs,	%% Segments: {SegOff, #elfsects -> Offset}
		sht, 	%% Section Header Table
		eof		%% End Of File
	}).

sect_name(text)      -> <<".text">>;
sect_name(rodata)    -> <<".rodata">>;
sect_name(eh_frame)  -> <<".eh_frame">>;
sect_name(data)      -> <<".data">>;
sect_name(bss)       -> <<".bss">>;
sect_name(xen_guest) -> <<"__xen_guest">>;
sect_name(comment)   -> <<".comment">>;
sect_name(shstrtab)  -> <<".shstrtab">>;
sect_name(symtab)    -> <<".symtab">>;
sect_name(strtab)    -> <<".strtab">>.

sect_align(text)     -> ?PAGE_SIZE;
sect_align(rodata)   -> 32;
sect_align(eh_frame) -> 8;
sect_align(data)     -> 32;
sect_align(bss)      -> 32;
sect_align(xen_guest) -> 1;
sect_align(comment)   -> 1;
sect_align(shstrtab)  -> 16;
sect_align(symtab)	  -> 16;
sect_align(strtab)   -> 16.

align(Addr, To) when Addr rem To =:= 0 -> Addr;
align(Addr, To) -> To * (Addr div To + 1).

align_section(Sect, Addr) when is_atom(Sect) ->
	align(Addr, sect_align(Sect)).

pad_to(Bin, Size) when is_binary(Bin) ->
	<<Bin/binary, 0:(Size - size(Bin))/binary>>.



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
	<<	E_ident/binary, ?ET_EXEC:?Elf_Half/little,
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
	>>.

section_size(Sections, Sect) when is_atom(Sect) ->
	section_size(Sections, sect_name(Sect));
section_size(Sections, Sect) when is_binary(Sect) ->
	case proplists:lookup(Sect, Sections) of
		{ Sect, Start, End } -> End - Start;
		none -> error
	end.

%% calculate offsets
make_elf_skeleton(Meta, Sections, PHT) when is_record(Meta, elfmeta) ->
	Eclass = elf_class(Meta),
	HdrsLen = elf_hdr_size(Eclass) + elf_phentsize(Eclass) * length(PHT),

	%% boring and long list, be cautious changing it
	SegsStart = align_section(text, HdrsLen),
	RodataStart = align_section(rodata, SegsStart + section_size(Sections, text)),
	EhframeStart = align_section(eh_frame, RodataStart + section_size(Sections, rodata)),
	DataStart = align_section(data, EhframeStart + section_size(Sections, eh_frame)),
	BssStart = align_section(bss, DataStart + section_size(Sections, data)),
	XenGuestStart = align_section(xen_guest, BssStart + 0),
	CommentStart = align_section(comment, XenGuestStart + section_size(Sections, xen_guest)),
	ShStrTabStart = align_section(shstrtab, CommentStart + section_size(Sections, comment)),
	SymtabStart = align_section(symtab, ShStrTabStart + section_size(Sections, shstrtab)),
	StrtabStart = align_section(strtab, SymtabStart + section_size(Sections, symtab)),

	SegOffsets = #elfsects {
		text = SegsStart, 
		rodata = RodataStart,
		eh_frame = EhframeStart,
		data = DataStart,
		bss = BssStart,
		xen_guest = XenGuestStart,
		comment = CommentStart,
		shstrtab = ShStrTabStart,
		symtab = SymtabStart,
		strtab = StrtabStart
	},
	SHTOff = align(StrtabStart + section_size(Sections, strtab), 16),
	EOFOff = SHTOff + elf_shentsize(Eclass) *  elf_sect_num(),
		
	#elfoffs{
		hdr = 0,
		pht = elf_hdr_size(Eclass),
		segs = {SegsStart, SegOffsets},
		sht = SHTOff,
		eof = EOFOff
	}.

make_shstrtab() ->
	{ShstrtabOffs, Shstrtab, _Len} = lists:foldl(
		fun(Sect, {ShOffs, ShBin, Off}) -> 
			Name = sect_name(Sect),
			{[{Sect, Off} | ShOffs], <<ShBin/binary, Name/binary>>, Off + size(Name) }
		end,
		{[], <<0>>, 1},
		record_info(fields, elfsects)),
	{ok, ShstrtabOffs, Shstrtab}.

make_elf_pht(_PHTNames) ->
	<<"TODO">>.

make_elf_segments(ElfSegsOffs) when is_record(ElfSegsOffs, elfoffs) ->
	<<"TODO">>.

make_elf_sht(ElfSegsOffs) when is_record(ElfSegsOffs, elfoffs) ->
	<<"TODO">>.

make_elf(Meta, Sections) ->
	PHT = [<<"LOAD">>, <<"GNU_STACK">>],

	ElfOffs = make_elf_skeleton(Meta, Sections, PHT),
	#elfoffs{ segs = {SegsStart, ElfSegsOffs} } = ElfOffs,

	{<<".text">>, EntryAddr, _} = proplists:lookup(<<".text">>, Sections),
	ElfHdr = make_elf_header(Meta, EntryAddr, PHT, ElfOffs#elfoffs.sht),
	HdrSz = size(ElfHdr),
	HdrSz = elf_hdr_size(elf_class(Meta)),

	ElfPHT = pad_to(make_elf_pht(PHT), SegsStart),
	ElfSegs = make_elf_segments(ElfSegsOffs),

	ElfSHT = make_elf_sht(ElfSegsOffs),
	<<ElfHdr/binary, ElfPHT/binary, ElfSegs/binary, ElfSHT/binary>>.

make() ->
	case ling:exec_info() of
	{elf, Meta, Sections} ->
		{ok, make_elf(Meta, Sections)};
	{Fmt, _,    _} ->
		{error, {unknown_format, Fmt}};
	_ ->
		{error, whut}
	end.

share(_Addr) -> todo.
