LING_VER := 0.3.1
OTP_VER := 17
ifeq ($(shell uname),Darwin)
CROSS := 1
else
CROSS := 0
endif
