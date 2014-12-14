
.PHONY: default

BEAMS := $(patsubst %.erl,%.beam,$(wildcard *.erl))

default: $(BEAMS)

$(BEAMS): %.beam: %.erl
	erlc $<

scripts/%.beam: scripts/%.erl
	erlc -o scripts $<

gentab/%.beam: gentab/%.erl
	erlc -o gentab $<

ling_bifs.erl: scripts/bif.tab
	scripts/bifs2_gen scripts/bif.tab $@

scripts/iopvars.tab: scripts/beam.src scripts/bif.tab \
					scripts/rare_iops.beam gentab/iops_tab.beam
	scripts/iopvars_gen scripts/beam.src scripts/bif.tab $@

gentab/iops_tab.erl: scripts/iops.tab
	scripts/iops_gen scripts/iops.tab $@

