
.PHONY: default bc core apps railing test

default: bc core apps railing

bc:
	make -C bc

core:
	make -C core

apps:
	make -C apps

railing:
	make -C railing

test:
	make -C test
