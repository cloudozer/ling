
.PHONY: default bc core apps railing test install checkotp test.img clean fresh

include Config.mk

default: railing | checkotp

checkotp:
ifneq ($(OTP_VER) , $(shell erl -noshell -eval "io:format(erlang:system_info(otp_release)),erlang:halt(0)."))
	$(error Erlang/OTP $(OTP_VER) not found)
endif

bc: test
	$(MAKE) -C bc

core: bc
	$(MAKE) -C core

apps: bc
	$(MAKE) -C apps

railing: bc core apps
	$(MAKE) -C railing

test:
	$(MAKE) -C test beams

install: bc core apps railing
	install railing/railing /usr/bin

test.img: railing
	$(MAKE) -C test test.img

build-arm:
	$(MAKE) -B ARCH=arm \
		&& cd railing \
		&& ./railing image -n kernel \
		&& mv kernel.img kernel.elf \
		&& arm-none-eabi-objcopy -O binary kernel.elf kernel.img

# use `make fresh` when you don't want to rebuild core/ling_main.c
fresh:
	mv core/ling_main.o . ; \
	rm -r core/*.o core/arch/$(ARCH)/*.o \
	      railing/railing railing/railing.img railing/.railing ; \
	mv ling_main.o core/

clean:
	rm -r core/ling_main.c core/include/atom_defs.h core/*.inc \
	    core/*.o core/arch/$(ARCH)/*.o \
	    core/preload/*.beam railing/railing railing/railing.img railing/.railing \
	    || true
