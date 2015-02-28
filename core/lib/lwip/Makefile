
TARGET := liblwip.a

CROSS_COMPILE ?= arm-unknown-eabi-
CC := $(CROSS_COMPILE)gcc

SRC_DIRS := src/api src/core src/core/ipv4 src/core/ipv6
SRC_DIRS += src/netif

SRCS := $(foreach dir,$(SRC_DIRS),$(wildcard $(dir)/*.c))
OBJS := $(patsubst %.c, %.o, $(SRCS))

COMMON_CFLAGS := -fno-stack-protector -fno-omit-frame-pointer -U_FORTIFY_SOURCE

ifeq ($(FULL_SPEED),1)
CFLAGS := -std=c99 -O3 $(COMMON_CFLAGS)
else
CFLAGS := -std=c99 -g $(COMMON_CFLAGS)
endif

INC_DIRS := src/include src/include/ipv4 src/include/ipv6
INC_DIRS += ling

CPPFLAGS := $(foreach dir,$(INC_DIRS),-I$(dir))

.PHONY: default
default: $(TARGET)

$(TARGET): $(OBJS)
	$(AR) rvs $(TARGET) $(OBJS)

$(OBJS): %.o: %.c $(DEPS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

