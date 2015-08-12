ifeq (,$(wildcard .config))
$(shell cp doc/default.config .config)
$(error Default .config created)
else
include .config
endif

default: railing/railing

test: default
	cd test && ../railing/railing image -ipriv && ./test.img -home /test -s test play

LING_VER := 0.3.2
OTP_VER := 17
ERLC := $(ERLANG_BIN)erlc
ESCRIPT := $(ERLANG_BIN)escript

ifeq ($(shell uname -s),Darwin)
LING_DARWIN := 1
else
LING_LINUX := 1
endif


## TEST
TEST_ERL := $(wildcard test/src/*.erl)
TEST_BEAM := $(TEST_ERL:test/src/%.erl=test/ebin/%.beam)

test/ebin/%.beam: test/src/%.erl
	$(ERLC) -o test/ebin $<

## BC
BC_ERL := $(filter-out bc/ling_iopvars.erl,$(wildcard bc/*.erl))
BC_BEAM := $(BC_ERL:%.erl=%.beam)
BC_SAMPLE_ERL := $(wildcard bc/sample/*.erl)
BC_SAMPLE_BEAM := $(BC_SAMPLE_ERL:%.erl=%.beam)

bc/%.beam: bc/%.erl
	$(ERLC) -o bc $<

bc/sample/%.beam: bc/sample/%.erl
	$(ERLC) -o bc/sample $<

bc/gentab/iops_tab.erl: bc/scripts/iops.tab bc/scripts/iops_tab_erl.et $(BC_BEAM) 
	bc/scripts/iops_gen bc/scripts/iops.tab bc/scripts/iops_tab_erl.et $@

bc/gentab/%.beam: bc/gentab/%.erl
	$(ERLC) -o bc/gentab $<

bc/scripts/iopvars.tab: bc/scripts/beam.src bc/scripts/bif.tab bc/gentab/iops_tab.beam $(BC_SAMPLE_BEAM) $(TEST_BEAM)
	bc/scripts/iopvars_gen bc/scripts/beam.src bc/scripts/bif.tab $@

bc/ling_iopvars.erl: bc/scripts/iopvars.tab bc/scripts/iopvars_erl.et
	bc/scripts/reorder_iopvars bc/scripts/iopvars.tab bc/scripts/hot_cold_iops bc/scripts/iopvars_erl.et $@

bc/ling_iopvars.beam: bc/ling_iopvars.erl
	$(ERLC) -o bc $<

## CORE
ifdef LING_XEN
LING_PLATFORM := xen
LING_OS := ling
ARCH := xen_x86
LIBMISC_ARCH := x86
ifdef LING_LINUX
CC := gcc
else ifdef LING_DARWIN
CC := x86_64-pc-linux-gcc
endif
endif

ifdef LING_POSIX
LING_PLATFORM := unix
ARCH := posix_x86
ifdef LING_LINUX
CC := gcc
LDFLAGS += -nostdlib
LING_OS := linux
else ifdef LING_DARWIN
CC := clang
LING_OS := darwin
endif
endif

CPPFLAGS += -D_ISOC99_SOURCE -D_GNU_SOURCE
CPPFLAGS += -DLING_VER=$(LING_VER)
CPPFLAGS += -isystem core/lib/nettle
CPPFLAGS += -isystem core/lib/pcre
CPPFLAGS += -isystem core/lib
CPPFLAGS += -iquote core/include
CPPFLAGS += -iquote core/bignum
CPPFLAGS += -iquote core/arch/$(ARCH)/include

CFLAGS   := -Wall
#CFLAGS   += -Werror
CFLAGS   += -Wno-nonnull -std=gnu99
CFLAGS   += -fno-omit-frame-pointer
CFLAGS	 += -fno-stack-protector -U_FORTIFY_SOURCE -ffreestanding

# relocatable (partial linking)
LDFLAGS  += -Xlinker -r

ASFLAGS  := -D__ASSEMBLY__

ifdef LING_XEN
XEN_INTERFACE_VERSION := 0x00030205
CPPFLAGS += -DLING_XEN
#CPPFLAGS += -DLING_CONFIG_DISK
CPPFLAGS += -D__XEN_INTERFACE_VERSION__=$(XEN_INTERFACE_VERSION)
CPPFLAGS += -isystem core/lib/misc/include

CFLAGS   += -std=gnu99
CFLAGS   += -fexcess-precision=standard -frounding-math -mfpmath=sse -msse2
#CFLAGS   += -O3
#CFLAGS   += -flto
CFLAGS   += -Wno-nonnull -Wno-strict-aliasing

LDFLAGS  += -T core/arch/xen_x86/ling.lds
LDFLAGS  += -static
LDFLAGS  += -Xlinker --build-id=none
LDFLAGS  += -Xlinker --cref -Xlinker -Map=core/ling.map
LDFLAGS  += -nostdlib
LDFLAGS_FINAL += -lgcc

STARTUP_OBJ     := core/arch/xen_x86/startup.o
STARTUP_SRC_EXT := S

LING_WITH_LWIP := 1
endif

ifdef LING_POSIX
CPPFLAGS += -DLING_POSIX
CPPFLAGS += -Wno-unknown-pragmas -Wno-int-conversion -Wno-empty-body
STARTUP_OBJ :=
ifdef LING_DARWIN
# assuming Apple LLVM version 6.0 (clang-600.0.57)
CPPFLAGS += -Wno-tautological-compare -Wno-typedef-redefinition -Wno-self-assign
endif
LING_WITH_LIBUV := 1
endif

ifdef LING_DEBUG
CFLAGS += -O0
CPPFLAGS += -DLING_DEBUG=1
CPPFLAGS += -DDEBUG_UNUSED_MEM=1
CPPFLAGS += -DTRACE_HARNESS=1
CPPFLAGS += -gdwarf-3
LDFLAGS  += -g
else
CFLAGS += -O3
ifdef LING_USE_LTO
CFLAGS += -flto
endif
endif

ifdef LING_WITH_LWIP
CPPFLAGS += -DLING_WITH_LWIP=1
CPPFLAGS += -iquote core/lib/lwip/src/include
CPPFLAGS += -iquote core/lib/lwip/src/include/ipv4
CPPFLAGS += -iquote core/lib/lwip/src/include/ipv6
CPPFLAGS += -iquote core/lib/lwip/ling
COMMON_CFLAGS := -fno-stack-protector -U_FORTIFY_SOURCE -fno-omit-frame-pointer

LWIP_DIR := \
	core/lib/lwip/src/api \
	core/lib/lwip/src/core \
	core/lib/lwip/src/core/ipv4 \
	core/lib/lwip/src/core/ipv6 \
	core/lib/lwip/src/netif

LWIP_SRC := $(foreach dir,$(LWIP_DIR),$(wildcard $(dir)/*.c))
LWIP_OBJ := $(patsubst %.c,%.o,$(LWIP_SRC))
LWIP_DEP := $(patsubst %.c,%.d,$(LWIP_SRC))
-include $(LWIP_DEP)

$(LWIP_OBJ): %.o: %.c .config
	$(CC) -MMD -MP $(CFLAGS) $(CPPFLAGS) -Wno-char-subscripts -o $@ -c $<
endif

ifdef LING_WITH_LIBUV
CPPFLAGS += -DLING_WITH_LIBUV=1
endif

ARCH_OBJ := $(patsubst %.c,%.o,$(wildcard core/arch/$(ARCH)/*.c))
CORE_OBJ := $(filter-out core/ling_main.%,$(patsubst %.c,%.o,$(wildcard core/*.c))) core/preload/literals.o
BIGNUM_OBJ := $(patsubst %.c,%.o,$(wildcard core/bignum/*.c))

MISC_SRC :=
MISC_SRC += __cos.c __expo2.c __rem_pio2.c __rem_pio2_large.c __sin.c __tan.c
MISC_SRC += acos.c asin.c atan.c atan2.c atof.c cos.c cosh.c exp.c expm1.c fabs.c
MISC_SRC += floor.c log.c log10.c memcmp.c modf.c pow.c
MISC_SRC += qsort.c scalbln.c scalbn.c sin.c sinh.c stpcpy.c strcat.c strchr.c
MISC_SRC += strchrnul.c strcmp.c strcpy.c strlen.c strncmp.c strtod.c tan.c tanh.c

ifdef LIBMISC_ARCH
ifneq ($(LIBMISC_ARCH),x86)
MISC_SRC += memcpy.c memmove.c memset.c sqrt.c
endif
MISC_OBJ := $(patsubst %.c,%.o,$(addprefix core/lib/misc/,$(MISC_SRC)))
MISC_AS := $(patsubst %.s,%.o,$(wildcard core/lib/misc/arch/$(LIBMISC_ARCH)/*.s))
else
MISC_OBJ :=
MISC_AS :=
endif

PCRE_OBJ := \
	core/lib/pcre/pcre_chartables.o \
	core/lib/pcre/pcre_compile.o \
	core/lib/pcre/pcre_config.o \
	core/lib/pcre/pcre_dfa_exec.o \
	core/lib/pcre/pcre_exec.o \
	core/lib/pcre/pcre_fullinfo.o \
	core/lib/pcre/pcre_get.o \
	core/lib/pcre/pcre_globals.o \
	core/lib/pcre/pcre_maketables.o \
	core/lib/pcre/pcre_newline.o \
	core/lib/pcre/pcre_ord2utf8.o \
	core/lib/pcre/pcre_study.o \
	core/lib/pcre/pcre_tables.o \
	core/lib/pcre/pcre_try_flipped.o \
	core/lib/pcre/pcre_ucp_searchfuncs.o \
	core/lib/pcre/pcre_valid_utf8.o \
	core/lib/pcre/pcre_version.o \
	core/lib/pcre/pcre_xclass.o

NETTLE_SRC := \
	core/lib/nettle/aes-decrypt.c \
	core/lib/nettle/aes-encrypt.c \
	core/lib/nettle/aes-encrypt-table.c \
	core/lib/nettle/aes-set-encrypt-key.c \
	core/lib/nettle/aes-set-decrypt-key.c \
	core/lib/nettle/aes-meta.c \
	core/lib/nettle/arcfour.c \
	core/lib/nettle/arcfour-crypt.c \
	core/lib/nettle/arcfour-meta.c \
	core/lib/nettle/arctwo.c \
	core/lib/nettle/arctwo-meta.c \
	core/lib/nettle/gosthash94-meta.c \
	core/lib/nettle/base16-encode.c \
	core/lib/nettle/base16-decode.c \
	core/lib/nettle/base16-meta.c \
	core/lib/nettle/base64-encode.c \
	core/lib/nettle/base64-decode.c \
	core/lib/nettle/base64-meta.c \
	core/lib/nettle/camellia-crypt.c \
	core/lib/nettle/camellia-set-encrypt-key.c \
	core/lib/nettle/camellia-set-decrypt-key.c \
	core/lib/nettle/camellia-table.c \
	core/lib/nettle/camellia-meta.c \
	core/lib/nettle/cast128.c \
	core/lib/nettle/cast128-meta.c \
	core/lib/nettle/blowfish.c \
	core/lib/nettle/cbc.c \
	core/lib/nettle/ctr.c \
	core/lib/nettle/gcm.c \
	core/lib/nettle/gcm-aes.c \
	core/lib/nettle/des.c \
	core/lib/nettle/des3.c \
	core/lib/nettle/des-compat.c \
	core/lib/nettle/hmac.c \
	core/lib/nettle/hmac-md5.c \
	core/lib/nettle/hmac-ripemd160.c \
	core/lib/nettle/hmac-sha1.c \
	core/lib/nettle/hmac-sha224.c \
	core/lib/nettle/hmac-sha256.c \
	core/lib/nettle/hmac-sha384.c \
	core/lib/nettle/hmac-sha512.c \
	core/lib/nettle/pbkdf2.c \
	core/lib/nettle/pbkdf2-hmac-sha1.c \
	core/lib/nettle/pbkdf2-hmac-sha256.c \
	core/lib/nettle/knuth-lfib.c \
	core/lib/nettle/md2.c \
	core/lib/nettle/md2-meta.c \
	core/lib/nettle/md4.c \
	core/lib/nettle/md4-meta.c \
	core/lib/nettle/md5.c \
	core/lib/nettle/md5-compress.c \
	core/lib/nettle/md5-compat.c \
	core/lib/nettle/md5-meta.c \
	core/lib/nettle/gosthash94.c \
	core/lib/nettle/ripemd160.c \
	core/lib/nettle/ripemd160-compress.c \
	core/lib/nettle/ripemd160-meta.c \
	core/lib/nettle/salsa20r12-crypt.c \
	core/lib/nettle/salsa20-set-key.c \
	core/lib/nettle/sha1.c \
	core/lib/nettle/sha1-meta.c \
	core/lib/nettle/sha256.c \
	core/lib/nettle/sha224-meta.c \
	core/lib/nettle/sha256-meta.c \
	core/lib/nettle/sha512.c \
	core/lib/nettle/sha384-meta.c \
	core/lib/nettle/sha512-meta.c \
	core/lib/nettle/sha3.c \
	core/lib/nettle/sha3-224.c \
	core/lib/nettle/sha3-224-meta.c \
	core/lib/nettle/sha3-256.c \
	core/lib/nettle/sha3-256-meta.c \
	core/lib/nettle/sha3-384.c \
	core/lib/nettle/sha3-384-meta.c \
	core/lib/nettle/sha3-512.c \
	core/lib/nettle/sha3-512-meta.c \
	core/lib/nettle/serpent-set-key.c \
	core/lib/nettle/serpent-meta.c \
	core/lib/nettle/twofish.c \
	core/lib/nettle/twofish-meta.c \
	core/lib/nettle/umac-l2.c \
	core/lib/nettle/umac-l3.c \
	core/lib/nettle/umac-poly64.c \
	core/lib/nettle/umac-poly128.c \
	core/lib/nettle/umac-set-key.c \
	core/lib/nettle/umac32.c \
	core/lib/nettle/umac64.c \
	core/lib/nettle/umac96.c \
	core/lib/nettle/umac128.c \
	core/lib/nettle/yarrow256.c \
	core/lib/nettle/yarrow_key_event.c \
	core/lib/nettle/buffer.c \
	core/lib/nettle/nettle-meta-hashes.c \
	core/lib/nettle/nettle-meta-ciphers.c \
	core/lib/nettle/nettle-meta-armors.c \
	core/lib/nettle/write-be32.c \
	core/lib/nettle/write-le32.c \
	core/lib/nettle/write-le64.c
#core/lib/nettle/realloc.c \
#core/lib/nettle/buffer-init.c \

NETTLE_ASM := \
	core/lib/nettle/aes-decrypt-internal.s \
	core/lib/nettle/aes-encrypt-internal.s \
	core/lib/nettle/camellia-crypt-internal.s \
	core/lib/nettle/salsa20-core-internal.s \
	core/lib/nettle/salsa20-crypt.s \
	core/lib/nettle/sha1-compress.s \
	core/lib/nettle/sha256-compress.s \
	core/lib/nettle/sha512-compress.s \
	core/lib/nettle/sha3-permute.s \
	core/lib/nettle/serpent-encrypt.s \
	core/lib/nettle/serpent-decrypt.s \
	core/lib/nettle/umac-nh.s \
	core/lib/nettle/umac-nh-n.s \
	core/lib/nettle/memxor.s

NETTLE_OBJ := $(patsubst %.c,%.o,$(NETTLE_SRC))
NETTLE_ABJ := $(patsubst %.s,%.o,$(NETTLE_ASM))

ifdef LING_WITH_LIBUV
CPPFLAGS += -isystem core/lib/libuv/include
CPPFLAGS += -isystem core/lib/libuv/src

LIBUV_SRC := \
	core/lib/libuv/src/fs-poll.c \
	core/lib/libuv/src/inet.c \
	core/lib/libuv/src/threadpool.c \
	core/lib/libuv/src/uv-common.c \
	core/lib/libuv/src/version.c \
	core/lib/libuv/src/unix/async.c \
	core/lib/libuv/src/unix/core.c \
	core/lib/libuv/src/unix/dl.c \
	core/lib/libuv/src/unix/fs.c \
	core/lib/libuv/src/unix/getaddrinfo.c \
	core/lib/libuv/src/unix/getnameinfo.c \
	core/lib/libuv/src/unix/loop-watcher.c \
	core/lib/libuv/src/unix/loop.c \
	core/lib/libuv/src/unix/pipe.c \
	core/lib/libuv/src/unix/poll.c \
	core/lib/libuv/src/unix/process.c \
	core/lib/libuv/src/unix/signal.c \
	core/lib/libuv/src/unix/stream.c \
	core/lib/libuv/src/unix/tcp.c \
	core/lib/libuv/src/unix/thread.c \
	core/lib/libuv/src/unix/timer.c \
	core/lib/libuv/src/unix/tty.c \
	core/lib/libuv/src/unix/udp.c \

ifdef LING_LINUX
LIBUV_SRC += \
	core/lib/libuv/src/unix/linux-core.c \
	core/lib/libuv/src/unix/linux-inotify.c \
	core/lib/libuv/src/unix/linux-syscalls.c \
	core/lib/libuv/src/unix/proctitle.c
endif

ifdef LING_DARWIN
LIBUV_SRC += \
	core/lib/libuv/src/unix/darwin.c \
	core/lib/libuv/src/unix/darwin-proctitle.c \
	core/lib/libuv/src/unix/fsevents.c \
	core/lib/libuv/src/unix/kqueue.c \
	core/lib/libuv/src/unix/proctitle.c

CPPFLAGS += -D_DARWIN_USE_64_BIT_INODE=1
CPPFLAGS += -D_DARWIN_UNLIMITED_SELECT=1
endif

LIBUV_OBJ := $(patsubst %.c,%.o,$(LIBUV_SRC))

$(LIBUV_OBJ): %.o: %.c .config
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

endif

$(NETTLE_OBJ): %.o: %.c .config
	$(CC) $(CFLAGS) $(CPPFLAGS) -Wno-uninitialised -Wno-unused-value -Wno-implicit-function-declaration -DHAVE_CONFIG_H -Wno-maybe-uninitialized -Wno-return-type -Wno-int-conversion -o $@ -c $<

$(NETTLE_ABJ): %.o: %.s .config
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $< -o $@

ALL_OBJ := $(CORE_OBJ) $(ARCH_OBJ) $(BIGNUM_OBJ) $(MISC_OBJ) $(PCRE_OBJ) $(LWIP_OBJ) $(NETTLE_OBJ) $(NETTLE_ABJ) $(LIBUV_OBJ)
ALL_OBJ += core/ling_main.o

ifneq ($(STARTUP_SRC_EXT),)
# this is a c file in posix
$(STARTUP_OBJ): %.o: %.$(STARTUP_SRC_EXT) .config
	echo $(ARCH)
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $< -o $@
endif

$(ARCH_OBJ) $(CORE_OBJ) $(BIGNUM_OBJ): %.o: %.c core/include/atom_defs.h core/include/mod_info.inc .config
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

$(MISC_OBJ): %.o: %.c .config
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

$(MISC_AS): %.o: %.s .config
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $< -o $@

$(PCRE_OBJ): %.o: %.c .config
	$(CC) $(CFLAGS) $(CPPFLAGS) -DHAVE_CONFIG_H -o $@ -c $<

CORE_GENTAB_ERL := core/gentab/atoms.erl core/gentab/exp_tab.erl
CORE_GENTAB_BEAM := $(patsubst %.erl,%.beam,$(sort $(wildcard core/gentab/*.erl) $(CORE_GENTAB_ERL)))
$(CORE_GENTAB_BEAM): %.beam: %.erl
	$(ERLC) -o core/gentab $<

CORE_PRELOAD_BEAM := $(patsubst %.erl,%.beam,$(wildcard core/preload/*.erl))
$(CORE_PRELOAD_BEAM): %.beam: %.erl .config
	$(ERLC) -DLING_VER=\"$(LING_VER)\" -DLING_PLATFORM=$(LING_PLATFORM) -DLING_OS=$(LING_OS) -o core/preload $<

CORE_INCLUDES := core/premod.inc core/code_base.inc core/include/mod_info.inc core/preload/literals.c core/catch_tab.inc
CORE_INCLUDES2 = core/premod%inc core/code_base%inc core/include/mod_info%inc core/preload/literals%c core/catch_tab%inc
$(CORE_INCLUDES2): core/gentab/atoms.beam core/gentab/exp_tab.beam core/include/atom_defs.h $(CORE_PRELOAD_BEAM)
	core/scripts/premod_gen core/preload core/premod.inc core/code_base.inc core/include/mod_info.inc core/preload/literals.c core/catch_tab.inc copy

core/gentab/exp_tab.erl: $(CORE_PRELOAD_BEAM) bc/scripts/bif.tab bc/ling_iopvars.beam
	core/scripts/exptab_gen core/preload bc/scripts/bif.tab $@

core/include/atom_defs%h core/atoms%inc core/gentab/atoms%erl: core/scripts/atoms.tab core/gentab/exp_tab.beam
	core/scripts/atoms_gen core/scripts/atoms.tab core/preload core/include/atom_defs.h core/atoms.inc core/gentab/atoms.erl

core/ling_main.c: core/scripts/ling_main_c.et core/scripts/hot_cold_iops $(CORE_GENTAB_BEAM) .config
	core/scripts/main_gen core/scripts/ling_main_c.et core/scripts/hot_cold_iops $@

core/ling_main.o: core/ling_main.c core/include/atom_defs.h $(CORE_INCLUDES)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(F_NO_REORDER_BLOCKS) -o $@ -c $<

core/include/bif.h: bc/scripts/bif.tab
	core/scripts/bifs_gen $< $@

core/vmling.o: $(STARTUP_OBJ) $(MISC_AS) $(ALL_OBJ)
	$(CC) -o $@ $(STARTUP_OBJ) $(MISC_AS) $(ALL_OBJ) $(CFLAGS) $(LDFLAGS) $(LDFLAGS_FINAL)

## APPS
APPS_STDLIB := $(patsubst apps/stdlib/src/%.erl,apps/stdlib/ebin/%.beam,$(wildcard apps/stdlib/src/*.erl))
APPS_KERNEL := $(patsubst apps/kernel/src/%.erl,apps/kernel/ebin/%.beam,$(wildcard apps/kernel/src/*.erl))
APPS_CRYPTO := $(patsubst apps/crypto/src/%.erl,apps/crypto/ebin/%.beam,$(wildcard apps/crypto/src/*.erl))
APPS_OS_MON := $(patsubst apps/os_mon/src/%.erl,apps/os_mon/ebin/%.beam,$(wildcard apps/os_mon/src/*.erl))
APPS_ASN1 := $(patsubst apps/asn1/src/%.erl,apps/asn1/ebin/%.beam,$(wildcard apps/asn1/src/*.erl))

APPS_ALL := $(APPS_STDLIB) $(APPS_KERNEL) $(APPS_CRYPTO) $(APPS_OS_MON) $(APPS_ASN1)

$(APPS_STDLIB): apps/stdlib/ebin/%.beam: apps/stdlib/src/%.erl
	$(ERLC) -o apps/stdlib/ebin $<

apps/kernel/src/ling_%.erl: ../code/ling_%.erl
	cp $< $@

$(APPS_KERNEL): apps/kernel/ebin/%.beam: apps/kernel/src/%.erl
	$(ERLC) -o apps/kernel/ebin $<

$(APPS_CRYPTO): apps/crypto/ebin/%.beam: apps/crypto/src/%.erl
	$(ERLC) -o apps/crypto/ebin $<

$(APPS_OS_MON): apps/os_mon/ebin/%.beam: apps/os_mon/src/%.erl
	$(ERLC) -o apps/os_mon/ebin $<

$(APPS_ASN1): apps/asn1/ebin/%.beam: apps/asn1/src/%.erl
	$(ERLC) -o apps/asn1/ebin $<

## RAILING
railing/railing: $(patsubst %.erl,%.beam,$(wildcard railing/*.erl)) railing/escriptize $(APPS_ALL) core/vmling.o
	./railing/escriptize $(ARCH)

railing/%.beam: railing/%.erl .config
	$(ERLC) -DLING_VER=\"$(LING_VER)\" -DARCH=\'$(ARCH)\' -DOTP_VER=\"$(OTP_VER)\" -o railing $<

.PHONY: default test