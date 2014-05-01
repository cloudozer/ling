%%
%%
%%

-record(m, {
	mod_name,
	code = [],
	atoms = [],
	exports = [],
	imports = [],
	lambdas = [],
	literals = [],
	strings = <<>>,
	attrs,
	compile_info,
	line_info,
	abst_code,
	catches = []
}).

%%EOF
