
.PHONY: default

BEAMS := $(patsubst %.erl,%.beam,$(wildcard *.erl))

default: $(BEAMS)

SAMPLES := $(patsubst %.erl,%.beam,$(wildcard sample/*.erl))

$(BEAMS): %.beam: %.erl
	erlc $<

sample/%.beam: sample/%.erl
	erlc -o sample $<

gentab/%.beam: gentab/%.erl
	erlc -o gentab $<

gentab/iops_tab.erl: scripts/iops.tab
	scripts/iops_gen scripts/iops.tab $@

ling_bifs.erl: scripts/bif.tab
	scripts/bifs2_gen scripts/bif.tab $@

scripts/iopvars.tab: ling_code.beam

scripts/iopvars.tab: scripts/beam.src scripts/bif.tab \
					gentab/iops_tab.beam $(SAMPLES)
	scripts/iopvars_gen scripts/beam.src scripts/bif.tab $@

ling_iopvars.erl: scripts/iopvars.tab scripts/iopvars_erl.et
	scripts/reorder_iopvars scripts/iopvars.tab scripts/hot_cold_iops $@

