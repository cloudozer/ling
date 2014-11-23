
.PHONY: default

STDLIB_BEAMS := $(patsubst stdlib/src/%.erl,stdlib/ebin/%.beam,$(wildcard stdlib/src/*.erl))
KERNEL_BEAMS := $(patsubst kernel/src/%.erl,kernel/ebin/%.beam,$(wildcard kernel/src/*.erl))
CRYPTO_BEAMS := $(patsubst crypto/src/%.erl,crypto/ebin/%.beam,$(wildcard crypto/src/*.erl))
OS_MON_BEAMS := $(patsubst os_mon/src/%.erl,os_mon/ebin/%.beam,$(wildcard os_mon/src/*.erl))

default: $(STDLIB_BEAMS) $(KERNEL_BEAMS) $(CRYPTO_BEAMS) $(OS_MON_BEAMS)

$(STDLIB_BEAMS): stdlib/ebin/%.beam: stdlib/src/%.erl
	erlc -o stdlib/ebin $<

kernel/src/ling_%.erl: ../code/ling_%.erl
	cp $< $@

$(KERNEL_BEAMS): kernel/ebin/%.beam: kernel/src/%.erl
	erlc -o kernel/ebin $<

$(CRYPTO_BEAMS): crypto/ebin/%.beam: crypto/src/%.erl
	erlc -o crypto/ebin $<

$(OS_MON_BEAMS): os_mon/ebin/%.beam: os_mon/src/%.erl
	erlc -o os_mon/ebin $<

