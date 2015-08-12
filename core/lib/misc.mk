MISC_DIR := core/lib/misc

MISC_SRC :=
MISC_SRC += __cos.c __expo2.c __rem_pio2.c __rem_pio2_large.c __sin.c __tan.c
MISC_SRC += acos.c asin.c atan.c atan2.c atof.c cos.c cosh.c exp.c expm1.c fabs.c
MISC_SRC += floor.c log.c log10.c memcmp.c modf.c pow.c
MISC_SRC += qsort.c scalbln.c scalbn.c sin.c sinh.c stpcpy.c strcat.c strchr.c
MISC_SRC += strchrnul.c strcmp.c strcpy.c strlen.c strncmp.c strtod.c tan.c tanh.c

ifdef LING_XEN
CPPFLAGS += -isystem $(MISC_DIR)/include
endif

ifdef LIBMISC_ARCH
ifneq ($(LIBMISC_ARCH),x86)
MISC_SRC += memcpy.c memmove.c memset.c sqrt.c
endif
MISC_OBJ := $(patsubst %.c,%.o,$(addprefix $(MISC_DIR)/,$(MISC_SRC)))
MISC_AS := $(patsubst %.s,%.o,$(wildcard $(MISC_DIR)/arch/$(LIBMISC_ARCH)/*.s))
else
MISC_OBJ :=
MISC_AS :=
endif

$(MISC_OBJ): %.o: %.c .config
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

$(MISC_AS): %.o: %.s .config
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $< -o $@


ALL_OBJ += $(MISC_OBJ) $(MISC_AS)
