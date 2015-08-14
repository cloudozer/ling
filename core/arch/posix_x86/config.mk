F_NO_REORDER_BLOCKS :=

# posix currently uses system libc
#LIBMISC_ARCH := x86

LING_PLATFORM := unix
LING_POSIX    := 1

UNAME := $(shell uname -s)
ifeq ($(UNAME),Linux)
	CC := gcc
	LDFLAGS += -nostdlib
	LING_OS := linux
else ifeq ($(UNAME),Darwin)
	CC := clang
	LING_OS := darwin
endif

ifeq ($(LING_DEBUG),)
	CFLAGS += -O2
endif

CPPFLAGS      += -DLING_POSIX
STARTUP_OBJ   :=

CPPFLAGS += -Wno-unknown-pragmas -Wno-int-conversion -Wno-empty-body
ifeq ($(CC),clang)
# assuming Apple LLVM version 6.0 (clang-600.0.57)
CPPFLAGS += -Wno-tautological-compare -Wno-typedef-redefinition -Wno-self-assign
endif

LING_WITH_LIBUV  := 1
