APPS_STDLIB := $(patsubst apps/stdlib/src/%.erl,apps/stdlib/ebin/%.beam,$(wildcard apps/stdlib/src/*.erl))
APPS_KERNEL := \
	$(sort \
		$(patsubst \
			apps/kernel/src/%.erl,\
			apps/kernel/ebin/%.beam,\
			$(wildcard apps/kernel/src/*.erl)\
		) \
		apps/kernel/ebin/ling_bifs.beam \
		apps/kernel/ebin/ling_iopvars.beam \
	)
APPS_CRYPTO := $(patsubst apps/crypto/src/%.erl,apps/crypto/ebin/%.beam,$(wildcard apps/crypto/src/*.erl))
APPS_OS_MON := $(patsubst apps/os_mon/src/%.erl,apps/os_mon/ebin/%.beam,$(wildcard apps/os_mon/src/*.erl))
APPS_ASN1 := $(patsubst apps/asn1/src/%.erl,apps/asn1/ebin/%.beam,$(wildcard apps/asn1/src/*.erl))

APPS_ALL := $(APPS_STDLIB) $(APPS_KERNEL) $(APPS_CRYPTO) $(APPS_OS_MON) $(APPS_ASN1)

ifdef LING_XEN
APPS_EPPFLAGS += -DLING_XEN
else
ifdef LING_POSIX
APPS_EPPFLAGS += -DLING_POSIX
endif
endif

$(APPS_STDLIB): apps/stdlib/ebin/%.beam: apps/stdlib/src/%.erl
	$(ERLC) -o apps/stdlib/ebin $<

apps/kernel/src/ling_%.erl: ../code/ling_%.erl
	cp $< $@

apps/kernel/src/ling_iopvars.erl: bc/ling_iopvars.erl
apps/kernel/src/ling_bifs.erl: bc/ling_bifs.erl

$(APPS_KERNEL): apps/kernel/ebin/%.beam: apps/kernel/src/%.erl
	$(ERLC) $(APPS_EPPFLAGS) -o apps/kernel/ebin $<

$(APPS_CRYPTO): apps/crypto/ebin/%.beam: apps/crypto/src/%.erl
	$(ERLC) -o apps/crypto/ebin $<

$(APPS_OS_MON): apps/os_mon/ebin/%.beam: apps/os_mon/src/%.erl
	$(ERLC) -o apps/os_mon/ebin $<

$(APPS_ASN1): apps/asn1/ebin/%.beam: apps/asn1/src/%.erl
	$(ERLC) -o apps/asn1/ebin $<
