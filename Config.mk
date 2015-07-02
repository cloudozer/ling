LING_VER := $(shell git describe --abbrev=4 --dirty --always --tags)
OTP_VER := 17
ifeq ($(shell uname),Darwin)
CROSS := 1
else
CROSS := 0
endif
