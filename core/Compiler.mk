
ifeq ($(ARCH),arm)
CC := arm-unknown-eabi-gcc
CPPFLAGS += -DLING_ARM

NETTLE_FLAGS := --host=arm-none-eabi
# newlib doesn't support assert very well, so set NDEBUG to elide asserts and prevent undefined ref errors at link time.
NETTLE_CFLAGS := -DNDEBUG --specs=nosys.specs -mfloat-abi=hard -mfpu=vfp
endif

ifeq ($(ARCH),posix)
CC := clang
NETTLE_FLAGS :=
NETTLE_CFLAGS :=
CPPFLAGS += -DLING_POSIX

# assuming Apple LLVM version 6.0 (clang-600.0.57)
CPPFLAGS += -Wno-tautological-compare -Wno-typedef-redefinition -Wno-empty-body
CPPFLAGS += -Wno-unknown-pragmas -Wno-int-conversion -Wno-self-assign
endif

ifeq ($(ARCH),x86)
XEN_INTERFACE_VERSION := 0x00030205

CPPFLAGS += -DLING_XEN
CPPFLAGS += -D__XEN_INTERFACE_VERSION__=$(XEN_INTERFACE_VERSION)

ifeq ($(shell uname),Darwin)
CC := x86_64-pc-linux-gcc
NETTLE_FLAGS := --host=x86_64-pc-linux
NETTLE_CFLAGS :=
else # Darwin
CC := gcc
NETTLE_FLAGS :=
endif # Darwin
endif # x86

