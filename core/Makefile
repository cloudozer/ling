
.PHONY: default
INCLUDES := premod.inc code_base.inc include/mod_info.inc preload/literals.bin catch_tab.inc
default: $(INCLUDES) vmling.o

include ../Config.mk

ARCH := x86
XEN_INTERFACE_VERSION := 0x00030205

ASFLAGS := -D__ASSEMBLY__

CPPFLAGS := -D_ISOC99_SOURCE -D_GNU_SOURCE
CPPFLAGS += -isystem lib/misc/include
CPPFLAGS += -isystem lib/pcre
CPPFLAGS += -iquote include -iquote arch/$(ARCH)/include
CPPFLAGS += -iquote bignum
CPPFLAGS += -iquote lib/lwip/src/include
CPPFLAGS += -iquote lib/lwip/src/include/ipv4
CPPFLAGS += -iquote lib/lwip/src/include/ipv6
CPPFLAGS += -iquote lib/lwip/ling
CPPFLAGS += -D__XEN_INTERFACE_VERSION__=$(XEN_INTERFACE_VERSION)

CPPFLAGS += -DLING_VER=$(LING_VER)
CPPFLAGS += -DLING_HOSTNAME=$(LING_HOSTNAME)

#CPPFLAGS += -DLING_DEBUG
#CPPFLAGS += -DDEBUG_UNUSED_MEM
#CPPFLAGS += -DTRACE_HARNESS

COMMON_CFLAGS := -fno-stack-protector -U_FORTIFY_SOURCE -fno-omit-frame-pointer -flto

CFLAGS := \
	-Wall -Werror -std=gnu99 -O3 \
	-fexcess-precision=standard -frounding-math \
	-Wno-nonnull -Wno-strict-aliasing -Wno-aggressive-loop-optimizations \
	$(COMMON_CFLAGS)

#CFLAGS := -Wall -Werror -std=gnu99 -g 

LDFLAGS := -T ling.lds
LDFLAGS += -nostdlib
LDFLAGS += -L $(NETTLE_DIR)
LDFLAGS += -Xlinker --relocatable
LDFLAGS += -Xlinker --build-id=none
LDFLAGS += -Xlinker -Map=ling.map
LDFLAGS += -static
LDFLAGS_FINAL := -lgcc -lnettle

STARTUP_OBJ := $(patsubst %.S,%.o,arch/$(ARCH)/startup.S)

ARCH_OBJS := $(patsubst %.c,%.o,$(wildcard arch/$(ARCH)/*.c))
OBJS := $(patsubst %.c,%.o,$(wildcard *.c))
BIGNUM_OBJS := $(patsubst %.c,%.o,$(wildcard bignum/*.c))
MISC_OBJS := $(patsubst %.c,%.o,$(wildcard lib/misc/src/*.c))
MISC_AS_OBJS := $(patsubst %.s,%.o,$(wildcard lib/misc/src/x86_64/*.s))
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

ALL_OBJS := $(OBJS) $(ARCH_OBJS) $(BIGNUM_OBJS) $(MISC_OBJS) $(PCRE_OBJS) $(LWIP_OBJS) \
	preload/literals.o

$(STARTUP_OBJ): %.o: %.S
	gcc $(ASFLAGS) $(CPPFLAGS) -c $< -o $@

# Stop iops reordering in ling_main.o
OBJS1 := $(filter-out ling_main.o,$(OBJS))
$(ARCH_OBJS) $(OBJS1) $(BIGNUM_OBJS): %.o: %.c include/atom_defs.h
	gcc $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

$(MISC_OBJS): %.o: %.c
	gcc $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

$(MISC_AS_OBJS): %.o: %.s
	gcc $(ASFLAGS) $(CPPFLAGS) -c $< -o $@

$(PCRE_OBJS): %.o: %.c
	gcc $(CFLAGS) $(CPPFLAGS) -DHAVE_CONFIG_H -o $@ -c $<

$(LWIP_OBJS): %.o: %.c
	gcc $(CFLAGS) $(CPPFLAGS) -Wno-address -Wno-char-subscripts -o $@ -c $<

ling_main.o: ling_main.c include/atom_defs.h
	gcc $(CFLAGS) $(CPPFLAGS) -fno-reorder-blocks -o $@ -c $<

include/bif.h: ../bc/scripts/bif.tab
	scripts/bifs_gen $< $@

GEN_BEAMS := gentab/atoms.erl gentab/exp_tab.erl
BEAMS := $(patsubst %.erl,%.beam,$(sort $(wildcard gentab/*.erl) $(GEN_BEAMS)))
$(BEAMS): %.beam: %.erl
	erlc -o gentab $<

PREMODS := $(patsubst %.erl,%.beam,$(wildcard preload/*.erl))
$(PREMODS): %.beam: %.erl
	erlc -o preload $<

gentab/exp_tab.erl: $(PREMODS) ../bc/scripts/bif.tab
	scripts/exptab_gen preload ../bc/scripts/bif.tab $@

include/atom_defs.h atoms.inc gentab/atoms.erl: scripts/atoms.tab gentab/exp_tab.beam
	scripts/atoms_gen scripts/atoms.tab preload \
		include/atom_defs.h atoms.inc gentab/atoms.erl


premod: $(INCLUDES)
$(INCLUDES): \
				gentab/atoms.beam gentab/exp_tab.beam include/atom_defs.h $(PREMODS)
	scripts/premod_gen preload \
		premod.inc code_base.inc include/mod_info.inc preload/literals.bin catch_tab.inc copy

preload/literals.o: preload/literals.bin
	objcopy -I binary -O elf64-x86-64 -B i386 \
		--redefine-sym _binary_preload_literals_bin_start=literals_blob_start \
		--redefine-sym _binary_preload_literals_bin_end=literals_blob_end \
		--redefine-sym _binary_preload_literals_bin_size=literals_blob_size \
	   	$< $@

vmling.o: $(STARTUP_OBJ) $(MISC_AS_OBJS) $(ALL_OBJS) 
	gcc -o $@ $(STARTUP_OBJ) $(MISC_AS_OBJS) $(ALL_OBJS) $(LDFLAGS) $(LDFLAGS_FINAL) $(CFLAGS)
