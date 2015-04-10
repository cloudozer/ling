F_NO_REORDER_BLOCKS :=

# posix currently uses system libc
#LIBMISC_ARCH := x86

CC            := clang
CPPFLAGS      += -DLING_POSIX
STARTUP_OBJ   := 

# assuming Apple LLVM version 6.0 (clang-600.0.57)
CPPFLAGS += -Wno-tautological-compare -Wno-typedef-redefinition -Wno-empty-body
CPPFLAGS += -Wno-unknown-pragmas -Wno-int-conversion -Wno-self-assign
