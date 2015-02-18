
.PHONY: default lwip pcre
INCLUDES := premod.inc code_base.inc include/mod_info.inc preload/literals.bin catch_tab.inc
default: lwip pcre $(INCLUDES) vmling.o

include ../Config.mk

ARCH := arm

CROSS_PREFIX := arm-unknown-eabi
CC := $(CROSS_PREFIX)-gcc
OBJCOPY := $(CROSS_PREFIX)-objcopy

CPPFLAGS := -D_ISOC99_SOURCE -D_GNU_SOURCE
CPPFLAGS += -isystem lib/nettle
CPPFLAGS += -isystem lib/pcre
CPPFLAGS += -isystem lib
CPPFLAGS += -iquote include -iquote arch/$(ARCH)/include
CPPFLAGS += -iquote bignum
CPPFLAGS += -isystem lib/misc/include
CPPFLAGS += -iquote lib/lwip/src/include
CPPFLAGS += -iquote lib/lwip/src/include/ipv4
CPPFLAGS += -iquote lib/lwip/src/include/ipv6
CPPFLAGS += -iquote lib/lwip/ling

CPPFLAGS += -DLING_VER=$(LING_VER)

CPPFLAGS += -DLING_DEBUG
CPPFLAGS += -DDEBUG_UNUSED_MEM
CPPFLAGS += -DTRACE_HARNESS

COMMON_CFLAGS := -fno-stack-protector -U_FORTIFY_SOURCE -fno-omit-frame-pointer -ffreestanding

#CFLAGS := -Wall -Werror -std=gnu99 -O3 -Wno-nonnull -Wno-strict-aliasing $(COMMON_CFLAGS)
CFLAGS := -Wall -Werror -Wno-nonnull -std=gnu99 -gdwarf-3 $(COMMON_FLAGS)
CFLAGS += -mfpu=vfp -mfloat-abi=hard -march=armv6zk -mtune=arm1176jzf-s

ASFLAGS += -mfpu=vfp -mfloat-abi=hard -march=armv6zk -mtune=arm1176jzf-s

LDFLAGS := -T ling.lds
LDFLAGS += -nostdlib
LDFLAGS += -L lib/nettle
LDFLAGS += -Xlinker --relocatable
LDFLAGS += -Xlinker --build-id=none
LDFLAGS += -Xlinker --cref -Xlinker -Map=ling.map
LDFLAGS += -static
LDFLAGS_FINAL := -lgcc lib/nettle/libnettle.a

STARTUP_OBJ := $(patsubst %.s,%.o,arch/$(ARCH)/startup.s)

ARCH_OBJS := $(patsubst %.c,%.o,$(wildcard arch/$(ARCH)/*.c))
OBJS := $(patsubst %.c,%.o,$(wildcard *.c))
BIGNUM_OBJS := $(patsubst %.c,%.o,$(wildcard bignum/*.c))
MISC_OBJS := $(patsubst %.c,%.o,$(wildcard lib/misc/*.c))
MISC_AS_OBJS := $(patsubst %.s,%.o,$(wildcard lib/misc/arch/$(ARCH)/*.s))
PCRE_OBJS := \
	lib/pcre/pcre_chartables.o \
	lib/pcre/pcre_compile.o \
	lib/pcre/pcre_config.o \
	lib/pcre/pcre_dfa_exec.o \
	lib/pcre/pcre_exec.o \
	lib/pcre/pcre_fullinfo.o \
	lib/pcre/pcre_get.o \
	lib/pcre/pcre_globals.o \
	lib/pcre/pcre_maketables.o \
	lib/pcre/pcre_newline.o \
	lib/pcre/pcre_ord2utf8.o \
	lib/pcre/pcre_study.o \
	lib/pcre/pcre_tables.o \
	lib/pcre/pcre_try_flipped.o \
	lib/pcre/pcre_ucp_searchfuncs.o \
	lib/pcre/pcre_valid_utf8.o \
	lib/pcre/pcre_version.o \
	lib/pcre/pcre_xclass.o

LWIP_SRC_DIRS := \
	lib/lwip/src/api \
	lib/lwip/src/core \
	lib/lwip/src/core/ipv4 \
	lib/lwip/src/core/ipv6 \
	lib/lwip/src/netif

LWIP_SRCS := $(foreach dir,$(LWIP_SRC_DIRS),$(wildcard $(dir)/*.c))
LWIP_OBJS := $(patsubst %.c,%.o,$(LWIP_SRCS))

ifeq ($(CROSS),1)
NETTLE_FLAGS := --host=x86_64-pc-linux
else
NETTLE_FLAGS :=
endif

ALL_OBJS := $(OBJS) $(ARCH_OBJS) $(BIGNUM_OBJS) $(MISC_OBJS) $(PCRE_OBJS) $(LWIP_OBJS) \
	preload/literals.o ling_main.o

$(STARTUP_OBJ): %.o: %.s
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $< -o $@

# Stop iops reordering in ling_main.o
OBJS1 := $(filter-out ling_main.o,$(OBJS))
$(ARCH_OBJS) $(OBJS1) $(BIGNUM_OBJS): %.o: %.c include/atom_defs.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

$(MISC_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

$(MISC_AS_OBJS): %.o: %.s
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $< -o $@

$(PCRE_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -DHAVE_CONFIG_H -o $@ -c $<

$(LWIP_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -Wno-char-subscripts -o $@ -c $<

ling_main.c: scripts/ling_main_c.et scripts/hot_cold_iops
	scripts/main_gen $^

ling_main.o: ling_main.c include/atom_defs.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -fno-reorder-blocks -o $@ -c $<

include/bif.h: ../bc/scripts/bif.tab
	scripts/bifs_gen $< $@

GEN_BEAMS := gentab/atoms.erl gentab/exp_tab.erl
BEAMS := $(patsubst %.erl,%.beam,$(sort $(wildcard gentab/*.erl) $(GEN_BEAMS)))
$(BEAMS): %.beam: %.erl
	erlc -o gentab $<

PREMODS := $(patsubst %.erl,%.beam,$(wildcard preload/*.erl))
$(PREMODS): %.beam: %.erl
	erlc -DLING_VER=\"$(LING_VER)\" -o preload $<

gentab/exp_tab.erl: $(PREMODS) ../bc/scripts/bif.tab
	scripts/exptab_gen preload ../bc/scripts/bif.tab $@

include/atom_defs.h atoms.inc gentab/atoms.erl: scripts/atoms.tab gentab/exp_tab.beam
	scripts/atoms_gen scripts/atoms.tab preload \
		include/atom_defs.h atoms.inc gentab/atoms.erl


premod: $(INCLUDES)
$(INCLUDES): gentab/atoms.beam gentab/exp_tab.beam include/atom_defs.h $(PREMODS)
	scripts/premod_gen preload premod.inc code_base.inc include/mod_info.inc preload/literals.bin catch_tab.inc copy

preload/literals.o: preload/literals.bin
	$(OBJCOPY) -I binary -O elf32-littlearm -B arm \
		--redefine-sym _binary_preload_literals_bin_start=literals_blob_start \
		--redefine-sym _binary_preload_literals_bin_end=literals_blob_end \
		--redefine-sym _binary_preload_literals_bin_size=literals_blob_size \
		$< $@

vmling.o: $(STARTUP_OBJ) $(MISC_AS_OBJS) $(ALL_OBJS)
	$(CC) -o $@ $(STARTUP_OBJ) $(MISC_AS_OBJS) $(ALL_OBJS) $(LDFLAGS) $(LDFLAGS_FINAL) $(CFLAGS)

nettle: lib/nettle/libnettle.a

lib/nettle/libnettle.a: lib/nettle/config.h
	$(MAKE) -C lib/nettle

lib/nettle/config.h: lib/nettle.tar.gz
	mkdir -p lib/nettle && tar vxzf $< -C lib/nettle --strip-components=1 && \
	cd lib/nettle && ./configure --disable-public-key --disable-shared --disable-pic --disable-openssl $(NETTLE_FLAGS) CFLAGS="-fno-stack-protector -U_FORTIFY_SOURCE"

lib/nettle.tar.gz:
	wget -O $@ --no-check-certificate -c https://ftp.gnu.org/gnu/nettle/nettle-2.7.1.tar.gz
