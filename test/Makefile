
.PHONY: default

BEAMS := $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))

default: $(BEAMS)
	../railing/railing

$(BEAMS): ebin/%.beam: src/%.erl
	erlc -o ebin $<
