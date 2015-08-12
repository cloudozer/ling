LWIP_DIR := core/lib/lwip

LWIP_SUB := \
	$(LWIP_DIR)/src/api \
	$(LWIP_DIR)/src/core \
	$(LWIP_DIR)/src/core/ipv4 \
	$(LWIP_DIR)/src/core/ipv6 \
	$(LWIP_DIR)/src/netif

LWIP_SRC := $(foreach dir,$(LWIP_SUB),$(wildcard $(dir)/*.c))
LWIP_OBJ := $(patsubst %.c,%.o,$(LWIP_SRC))
LWIP_DEP := $(patsubst %.c,%.d,$(LWIP_SRC))
-include $(LWIP_DEP)

CPPFLAGS += -DLING_WITH_LWIP=1
CPPFLAGS += -iquote $(LWIP_DIR)/src/include
CPPFLAGS += -iquote $(LWIP_DIR)/src/include/ipv4
CPPFLAGS += -iquote $(LWIP_DIR)/src/include/ipv6
CPPFLAGS += -iquote $(LWIP_DIR)/ling
#COMMON_CFLAGS := -fno-stack-protector -U_FORTIFY_SOURCE -fno-omit-frame-pointer

$(LWIP_OBJ): %.o: %.c .config
	$(CC) -MMD -MP $(CFLAGS) $(CPPFLAGS) -Wno-char-subscripts -o $@ -c $<

ALL_OBJ += $(LWIP_OBJ)
