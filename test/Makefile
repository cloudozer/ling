.PHONY: default

BEAMS := $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))

default: $(BEAMS)
	../railing/railing image

$(BEAMS): ebin/%.beam: src/%.erl
	erlc -o ebin $<
