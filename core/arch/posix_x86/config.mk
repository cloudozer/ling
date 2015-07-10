F_NO_REORDER_BLOCKS :=

# posix currently uses system libc
#LIBMISC_ARCH := x86

UNAME := $(shell uname -s)
ifeq ($(UNAME),Linux)
	CC := gcc
	LDFLAGS += -nostdlib
else ifeq ($(UNAME),Darwin)
	CC := clang
endif

CPPFLAGS      += -DLING_POSIX
STARTUP_OBJ   := 
# assuming Apple LLVM version 6.0 (clang-600.0.57)
CPPFLAGS += -Wno-tautological-compare -Wno-typedef-redefinition -Wno-empty-body
CPPFLAGS += -Wno-unknown-pragmas -Wno-int-conversion -Wno-self-assign

LING_WITH_LIBUV  := 1
