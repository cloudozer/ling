NO_USE_LIBMISC      := 1
F_NO_REORDER_BLOCKS :=

CC            := clang
NETTLE_FLAGS  += 
NETTLE_CFLAGS += 
CPPFLAGS      += -DLING_POSIX
STARTUP_OBJ   := 

# assuming Apple LLVM version 6.0 (clang-600.0.57)
CPPFLAGS += -Wno-tautological-compare -Wno-typedef-redefinition -Wno-empty-body
CPPFLAGS += -Wno-unknown-pragmas -Wno-int-conversion -Wno-self-assign
