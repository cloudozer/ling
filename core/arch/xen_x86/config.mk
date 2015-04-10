ifeq ($(shell uname),Darwin)
CC           := x86_64-pc-linux-gcc
NETTLE_FLAGS += --host=x86_64-pc-linux
else
CC           := gcc
endif

XEN_INTERFACE_VERSION := 0x00030205

CPPFLAGS += -DLING_XEN -DLING_CONFIG_DISK
CPPFLAGS += -D__XEN_INTERFACE_VERSION__=$(XEN_INTERFACE_VERSION)

CFLAGS   += -std=gnu99
CFLAGS   += -fexcess-precision=standard -frounding-math -mfpmath=sse -msse2
CFLAGS   += -O3
CFLAGS   += -flto
CFLAGS   += -Wno-nonnull -Wno-strict-aliasing

LDFLAGS  += -T arch/$(ARCH)/ling.lds
LDFLAGS  += -static
LDFLAGS  += -Xlinker --build-id=none
LDFLAGS  += -Xlinker --cref -Xlinker -Map=ling.map
LDFLAGS  += -nostdlib
LDFLAGS  += -flto
LDFLAGS_FINAL += -lgcc

STARTUP_OBJ     := arch/$(ARCH)/startup.o
STARTUP_SRC_EXT := S
