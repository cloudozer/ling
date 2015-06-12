#HOST_TOOLCHAIN ?= arm-unknown-eabi
HOST_TOOLCHAIN ?= arm-none-eabi
CC             := $(HOST_TOOLCHAIN)-gcc
CPPFLAGS       += -DLING_ARM
LIBMISC_ARCH   := arm

F_NO_REORDER_BLOCKS := -fno-reorder-blocks

RASPBERRY_PI  := -mfpu=vfp -mfloat-abi=hard -march=armv6zk -mtune=arm1176jzf-s

CFLAGS        += $(RASPBERRY_PI)
LDFLAGS       += $(RASPBERRY_PI)
ASFLAGS       += $(RASPBERRY_PI)

LDFLAGS       += -T arch/$(ARCH)/ling.lds
LDFLAGS       += -static
LDFLAGS       += -Xlinker --build-id=none
LDFLAGS       += -Xlinker --cref -Xlinker -Map=ling.map
LDFLAGS       += -nostdlib
LDFLAGS_FINAL += -lgcc

STARTUP_OBJ     := arch/$(ARCH)/startup.o
STARTUP_SRC_EXT := s

# newlib doesn't support assert very well, so set NDEBUG to elide asserts and prevent undefined ref errors at link time.
NETTLE_CFLAGS  += -DNDEBUG -mfloat-abi=hard -mfpu=vfp
NETTLE_FLAGS   += --host=$(HOST_TOOLCHAIN)
NETTLE_LDFLAGS += -specs=nosys.specs

