-module(bfd_objcopy).
-description('Embed binaries into object files (objcopy -I binary -B arch)').
-author('Vladimir Kirillov').
-export([binary_to_c_iolist/2, blob_to_src/3]).

blob_to_src(TargetFile, SymPrefix, Blob) when is_list(Blob) orelse is_binary(Blob) ->
	file:write_file(TargetFile, binary_to_c_iolist(SymPrefix, Blob)).

binary_to_c_iolist(Sym, Blob) ->
	Size = integer_to_list(size(Blob)),

	P1 = ["const char ", Sym, "_start[] = {\n"],
	Chars = [ case is_ascii(C) of
				true -> [$ , $', C, $'];
				false when C < 16 -> [$0, $x, $0, integer_to_list(C, 16)];
				false -> [$0, $x, integer_to_list(C, 16)]
			  end || C <- binary_to_list(Blob) ],
	Intercalated = array_format(Chars),

	P2 = ["const unsigned long ", Sym, "_size = ", Size, ";\n"],
	P3 = ["const char *", Sym, "_end = (char *)&", Sym, "_start + ", Size, ";\n"],
	[ P1, Intercalated, "\n};\n", P2, P3 ].

is_ascii(C) when C >= $0, C =< $9 -> true;
is_ascii(C) when C >= $a, C =< $z -> true;
is_ascii(C) when C >= $A, C =< $Z -> true;
is_ascii(_) -> false.

array_format(Xs) -> ["", intersperse(<<", ">>, <<"\n">>, 10, Xs)].
intersperse(Sep, GroupSep, Count, Xs) -> cute_intersperse(Sep, GroupSep, Count, Xs, 0).

cute_intersperse(_,_, _, [],_)-> [];
cute_intersperse(_,_, _, [X], _)-> [X];
cute_intersperse(Sep, GS, Count, [X|Xs], Count) ->
	[X, [Sep, GS]|cute_intersperse(Sep, GS, Count, Xs, 0)];
cute_intersperse(Sep, GS, Count, [X|Xs], N) ->
	[X, Sep|cute_intersperse(Sep, GS, Count, Xs, N + 1)].

%% vim: noet ts=4 sts=4 sw=4
