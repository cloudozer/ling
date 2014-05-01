
.PHONY: default dist

include ../Config.mk

default:
	@echo "{escript_emu_args,\"%%! -ling_root $(LING_ROOT)\\n\"}." > rebar.config
	./rebar compile escriptize

dist:
	@echo "" > rebar.config
	./rebar compile escriptize

