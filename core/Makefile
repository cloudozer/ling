
.PHONY: default lwip pcre
INCLUDES := premod.inc code_base.inc include/mod_info.inc preload/literals.bin catch_tab.inc
default: lwip pcre $(INCLUDES) vmling.o

include ../Config.mk

ARCH := arm
TEXT_BASE := 0x200000

CROSS_PREFIX := arm-unknown-eabi
CC := $(CROSS_PREFIX)-gcc
OBJCOPY := $(CROSS_PREFIX)-objcopy

CPPFLAGS := -DTEXT_BASE=$(TEXT_BASE)
CPPFLAGS += -D_ISOC99_SOURCE -D_GNU_SOURCE
CPPFLAGS += -isystem $(NETTLE_DIR)
CPPFLAGS += -isystem lib/pcre
CPPFLAGS += -iquote include -iquote arch/$(ARCH)/include
CPPFLAGS += -iquote bignum
CPPFLAGS += -iquote lib/lwip/src/include
CPPFLAGS += -iquote lib/lwip/src/include/ipv4
CPPFLAGS += -iquote lib/lwip/src/include/ipv6
CPPFLAGS += -iquote lib/lwip/ling

CPPFLAGS += -DLING_VER=$(LING_VER)
CPPFLAGS += -DLING_HOSTNAME=$(LING_HOSTNAME)

#CPPFLAGS += -DLING_DEBUG
#CPPFLAGS += -DDEBUG_UNUSED_MEM
#CPPFLAGS += -DTRACE_HARNESS

COMMON_CFLAGS := -fno-stack-protector -U_FORTIFY_SOURCE -fno-omit-frame-pointer -ffreestanding

#CFLAGS := -Wall -Werror -std=gnu99 -O3 -Wno-nonnull -Wno-strict-aliasing $(COMMON_CFLAGS)
CFLAGS := -Wall -Werror -Wno-nonnull -std=gnu99 -g $(COMMON_FLAGS)

LDFLAGS := -Ttext $(TEXT_BASE)
LDFLAGS += -T ling.lds
LDFLAGS += -nostdlib
LDFLAGS += -L $(NETTLE_DIR)
LDFLAGS += -Xlinker --relocatable
LDFLAGS += -Xlinker --build-id=none
LDFLAGS += -Xlinker --cref -Xlinker -Map=ling.map
LDFLAGS += -static
LDFLAGS += -L lib/pcre/.libs
LDFLAGS += -L lib/lwip
LDFLAGS_FINAL := -lgcc -lc -lm -lnettle
LDFLAGS_FINAL += -lpcre -llwip

STARTUP_OBJ := $(patsubst %.S,%.o,arch/$(ARCH)/startup.S)

ARCH_OBJS := $(patsubst %.c,%.o,$(wildcard arch/$(ARCH)/*.c))
OBJS := $(patsubst %.c,%.o,$(wildcard *.c))
BIGNUM_OBJS := $(patsubst %.c,%.o,$(wildcard bignum/*.c))

ALL_OBJS := $(OBJS) $(ARCH_OBJS) $(BIGNUM_OBJS) \
	preload/literals.o

$(STARTUP_OBJ): %.o: %.S
	$(CC) $(CPPFLAGS) -c $< -o $@

# Stop iops reordering in ling_main.o
OBJS1 := $(filter-out ling_main.o,$(OBJS))
$(ARCH_OBJS) $(OBJS1) $(BIGNUM_OBJS): %.o: %.c include/atom_defs.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

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
	$(OBJCOPY) -I binary -O elf32-littlearm -B arm\
		--redefine-sym _binary_preload_literals_bin_start=literals_blob_start \
		--redefine-sym _binary_preload_literals_bin_end=literals_blob_end \
		--redefine-sym _binary_preload_literals_bin_size=literals_blob_size \
	   	$< $@

lwip:
	make -C lib/lwip

lib/pcre/config.log:
	cd lib/pcre && autoreconf -i && ./configure --disable-shared \
							 --disable-cpp \
							 --disable-stack-for-recursion \
							 --enable-utf8 \
							 --enable-unicode-properties \
							 CFLAGS="-O2 $(COMMON_CFLAGS)" \
							 --no-recursion
	
pcre: lib/pcre/config.log
	make -C lib/pcre

vmling.o: $(STARTUP_OBJ) $(ALL_OBJS)
	$(CC) -o $@ $(STARTUP_OBJ) $(ALL_OBJS) $(LDFLAGS) $(LDFLAGS_FINAL) $(CFLAGS)

