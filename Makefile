
.PHONY: default bc core apps railing test install

LING_VER := 0.3.0

default: bc core apps railing

bc:
	make -C bc

core:
	make -C core LING_VER=$(LING_VER)

apps:
	make -C apps

railing:
	make -C railing LING_VER=$(LING_VER)

test:
	make -C test

install: bc core apps railing
	install railing/railing /usr/bin
