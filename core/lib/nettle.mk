NETTLE_DIR := core/lib/nettle

NETTLE_SRC := \
	$(NETTLE_DIR)/aes-decrypt.o \
	$(NETTLE_DIR)/aes-encrypt.o \
	$(NETTLE_DIR)/aes-encrypt-table.o \
	$(NETTLE_DIR)/aes-set-encrypt-key.o \
	$(NETTLE_DIR)/aes-set-decrypt-key.o \
	$(NETTLE_DIR)/aes-meta.o \
	$(NETTLE_DIR)/arcfour.o \
	$(NETTLE_DIR)/arcfour-crypt.o \
	$(NETTLE_DIR)/arcfour-meta.o \
	$(NETTLE_DIR)/arctwo.o \
	$(NETTLE_DIR)/arctwo-meta.o \
	$(NETTLE_DIR)/gosthash94-meta.o \
	$(NETTLE_DIR)/base16-encode.o \
	$(NETTLE_DIR)/base16-decode.o \
	$(NETTLE_DIR)/base16-meta.o \
	$(NETTLE_DIR)/base64-encode.o \
	$(NETTLE_DIR)/base64-decode.o \
	$(NETTLE_DIR)/base64-meta.o \
	$(NETTLE_DIR)/camellia-crypt.o \
	$(NETTLE_DIR)/camellia-set-encrypt-key.o \
	$(NETTLE_DIR)/camellia-set-decrypt-key.o \
	$(NETTLE_DIR)/camellia-table.o \
	$(NETTLE_DIR)/camellia-meta.o \
	$(NETTLE_DIR)/cast128.o \
	$(NETTLE_DIR)/cast128-meta.o \
	$(NETTLE_DIR)/blowfish.o \
	$(NETTLE_DIR)/cbc.o \
	$(NETTLE_DIR)/ctr.o \
	$(NETTLE_DIR)/gcm.o \
	$(NETTLE_DIR)/gcm-aes.o \
	$(NETTLE_DIR)/des.o \
	$(NETTLE_DIR)/des3.o \
	$(NETTLE_DIR)/des-compat.o \
	$(NETTLE_DIR)/hmac.o \
	$(NETTLE_DIR)/hmac-md5.o \
	$(NETTLE_DIR)/hmac-ripemd160.o \
	$(NETTLE_DIR)/hmac-sha1.o \
	$(NETTLE_DIR)/hmac-sha224.o \
	$(NETTLE_DIR)/hmac-sha256.o \
	$(NETTLE_DIR)/hmac-sha384.o \
	$(NETTLE_DIR)/hmac-sha512.o \
	$(NETTLE_DIR)/pbkdf2.o \
	$(NETTLE_DIR)/pbkdf2-hmac-sha1.o \
	$(NETTLE_DIR)/pbkdf2-hmac-sha256.o \
	$(NETTLE_DIR)/knuth-lfib.o \
	$(NETTLE_DIR)/md2.o \
	$(NETTLE_DIR)/md2-meta.o \
	$(NETTLE_DIR)/md4.o \
	$(NETTLE_DIR)/md4-meta.o \
	$(NETTLE_DIR)/md5.o \
	$(NETTLE_DIR)/md5-compress.o \
	$(NETTLE_DIR)/md5-compat.o \
	$(NETTLE_DIR)/md5-meta.o \
	$(NETTLE_DIR)/gosthash94.o \
	$(NETTLE_DIR)/ripemd160.o \
	$(NETTLE_DIR)/ripemd160-compress.o \
	$(NETTLE_DIR)/ripemd160-meta.o \
	$(NETTLE_DIR)/salsa20r12-crypt.o \
	$(NETTLE_DIR)/salsa20-set-key.o \
	$(NETTLE_DIR)/sha1.o \
	$(NETTLE_DIR)/sha1-meta.o \
	$(NETTLE_DIR)/sha256.o \
	$(NETTLE_DIR)/sha224-meta.o \
	$(NETTLE_DIR)/sha256-meta.o \
	$(NETTLE_DIR)/sha512.o \
	$(NETTLE_DIR)/sha384-meta.o \
	$(NETTLE_DIR)/sha512-meta.o \
	$(NETTLE_DIR)/sha3.o \
	$(NETTLE_DIR)/sha3-224.o \
	$(NETTLE_DIR)/sha3-224-meta.o \
	$(NETTLE_DIR)/sha3-256.o \
	$(NETTLE_DIR)/sha3-256-meta.o \
	$(NETTLE_DIR)/sha3-384.o \
	$(NETTLE_DIR)/sha3-384-meta.o \
	$(NETTLE_DIR)/sha3-512.o \
	$(NETTLE_DIR)/sha3-512-meta.o \
	$(NETTLE_DIR)/serpent-set-key.o \
	$(NETTLE_DIR)/serpent-meta.o \
	$(NETTLE_DIR)/twofish.o \
	$(NETTLE_DIR)/twofish-meta.o \
	$(NETTLE_DIR)/umac-l2.o \
	$(NETTLE_DIR)/umac-l3.o \
	$(NETTLE_DIR)/umac-poly64.o \
	$(NETTLE_DIR)/umac-poly128.o \
	$(NETTLE_DIR)/umac-set-key.o \
	$(NETTLE_DIR)/umac32.o \
	$(NETTLE_DIR)/umac64.o \
	$(NETTLE_DIR)/umac96.o \
	$(NETTLE_DIR)/umac128.o \
	$(NETTLE_DIR)/yarrow256.o \
	$(NETTLE_DIR)/yarrow_key_event.o \
	$(NETTLE_DIR)/buffer.o \
	$(NETTLE_DIR)/nettle-meta-hashes.o \
	$(NETTLE_DIR)/nettle-meta-ciphers.o \
	$(NETTLE_DIR)/nettle-meta-armors.o \
	$(NETTLE_DIR)/write-be32.o \
	$(NETTLE_DIR)/write-le32.o \
	$(NETTLE_DIR)/write-le64.o


NETTLE_CFLAGS = $(CFLAGS) $(CPPFLAGS) -Wno-uninitialized -Wno-unused-value -Wno-implicit-function-declaration -DHAVE_CONFIG_H -Wno-return-type -Wno-int-conversion

ifneq ($(CC),clang)
NETTLE_CFLAGS += -Wno-maybe-uninitialized
NETTLE_ASM_DIR := $(NETTLE_DIR)/gcc
else
NETTLE_ASM_DIR := $(NETTLE_DIR)/clang
endif

NETTLE_ASM := \
	$(NETTLE_ASM_DIR)/aes-decrypt-internal.o \
	$(NETTLE_ASM_DIR)/aes-encrypt-internal.o \
	$(NETTLE_ASM_DIR)/camellia-crypt-internal.o \
	$(NETTLE_ASM_DIR)/salsa20-core-internal.o \
	$(NETTLE_ASM_DIR)/salsa20-crypt.o \
	$(NETTLE_ASM_DIR)/sha1-compress.o \
	$(NETTLE_ASM_DIR)/sha256-compress.o \
	$(NETTLE_ASM_DIR)/sha512-compress.o \
	$(NETTLE_ASM_DIR)/sha3-permute.o \
	$(NETTLE_ASM_DIR)/serpent-encrypt.o \
	$(NETTLE_ASM_DIR)/serpent-decrypt.o \
	$(NETTLE_ASM_DIR)/umac-nh.o \
	$(NETTLE_ASM_DIR)/umac-nh-n.o \
	$(NETTLE_ASM_DIR)/memxor.o

NETTLE_DEP := $(patsubst %.o,%.d,$(NETTLE_SRC))
-include $(NETTLE_DEP)

$(NETTLE_SRC): %.o: %.c .config
	$(CC) -MMD -MP $(NETTLE_CFLAGS) -o $@ -c $<

$(NETTLE_ASM): %.o: %.s .config
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $< -o $@

CPPFLAGS += -isystem $(NETTLE_DIR)

ALL_OBJ += $(NETTLE_SRC) $(NETTLE_ASM)
