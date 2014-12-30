
.PHONY: default bc core apps railing test install checkotp test.img

include Config.mk

default: test bc core apps railing | checkotp

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

railing: bc core
	$(MAKE) -C railing

test:
	$(MAKE) -C test beams

install: bc core apps railing
	install railing/railing /usr/bin

test.img: railing
	$(MAKE) -C test test.img
