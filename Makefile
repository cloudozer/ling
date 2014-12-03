
.PHONY: default bc core apps railing test install checkotp

include Config.mk

default: bc core apps railing | checkotp

checkotp:
ifneq ($(OTP_VER) , $(shell erl -noshell -eval "io:format(erlang:system_info(otp_release)),erlang:halt(0)."))
	@echo Erlang/OTP $(OTP_VER) not found
	@exit 1
endif

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

install: bc core apps railing
	install railing/railing /usr/bin
