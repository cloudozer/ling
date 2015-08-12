PCRE_DIR := core/lib/pcre

PCRE_SRC := \
	$(PCRE_DIR)/pcre_chartables.o \
	$(PCRE_DIR)/pcre_compile.o \
	$(PCRE_DIR)/pcre_config.o \
	$(PCRE_DIR)/pcre_dfa_exec.o \
	$(PCRE_DIR)/pcre_exec.o \
	$(PCRE_DIR)/pcre_fullinfo.o \
	$(PCRE_DIR)/pcre_get.o \
	$(PCRE_DIR)/pcre_globals.o \
	$(PCRE_DIR)/pcre_maketables.o \
	$(PCRE_DIR)/pcre_newline.o \
	$(PCRE_DIR)/pcre_ord2utf8.o \
	$(PCRE_DIR)/pcre_study.o \
	$(PCRE_DIR)/pcre_tables.o \
	$(PCRE_DIR)/pcre_try_flipped.o \
	$(PCRE_DIR)/pcre_ucp_searchfuncs.o \
	$(PCRE_DIR)/pcre_valid_utf8.o \
	$(PCRE_DIR)/pcre_version.o \
	$(PCRE_DIR)/pcre_xclass.o

$(PCRE_SRC): %.o: %.c .config
	$(CC) $(CFLAGS) $(CPPFLAGS) -DHAVE_CONFIG_H -o $@ -c $<

CPPFLAGS += -isystem $(PCRE_DIR)

ALL_OBJ += $(PCRE_SRC)
