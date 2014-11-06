
.PHONY: default

BEAMS := $(patsubst %.erl,%.beam,$(wildcard *.erl))

default: $(BEAMS)

ling_bifs.erl: scripts/bif.tab
	scripts/bifs2_gen scripts/bif.tab $@

$(BEAMS): %.beam: %.erl
	erlc $<

