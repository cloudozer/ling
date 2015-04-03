LING_VER := 0.3.2
OTP_VER := 17
ARCH := posix

ifeq ($(shell uname),Darwin)
CROSS := 0
else
CROSS := 0
endif
