LIBUV_DIR := core/lib/libuv

LIBUV_SRC := \
	$(LIBUV_DIR)/src/fs-poll.o \
	$(LIBUV_DIR)/src/inet.o \
	$(LIBUV_DIR)/src/threadpool.o \
	$(LIBUV_DIR)/src/uv-common.o \
	$(LIBUV_DIR)/src/version.o \
	$(LIBUV_DIR)/src/unix/async.o \
	$(LIBUV_DIR)/src/unix/core.o \
	$(LIBUV_DIR)/src/unix/dl.o \
	$(LIBUV_DIR)/src/unix/fs.o \
	$(LIBUV_DIR)/src/unix/getaddrinfo.o \
	$(LIBUV_DIR)/src/unix/getnameinfo.o \
	$(LIBUV_DIR)/src/unix/loop-watcher.o \
	$(LIBUV_DIR)/src/unix/loop.o \
	$(LIBUV_DIR)/src/unix/pipe.o \
	$(LIBUV_DIR)/src/unix/poll.o \
	$(LIBUV_DIR)/src/unix/process.o \
	$(LIBUV_DIR)/src/unix/signal.o \
	$(LIBUV_DIR)/src/unix/stream.o \
	$(LIBUV_DIR)/src/unix/tcp.o \
	$(LIBUV_DIR)/src/unix/thread.o \
	$(LIBUV_DIR)/src/unix/timer.o \
	$(LIBUV_DIR)/src/unix/tty.o \
	$(LIBUV_DIR)/src/unix/udp.o

ifdef LING_LINUX
LIBUV_SRC += \
	$(LIBUV_DIR)/src/unix/linux-core.o \
	$(LIBUV_DIR)/src/unix/linux-inotify.o \
	$(LIBUV_DIR)/src/unix/linux-syscalls.o \
	$(LIBUV_DIR)/src/unix/proctitle.o
endif

ifdef LING_DARWIN
LIBUV_SRC += \
	$(LIBUV_DIR)/src/unix/darwin.o \
	$(LIBUV_DIR)/src/unix/darwin-proctitle.o \
	$(LIBUV_DIR)/src/unix/fsevents.o \
	$(LIBUV_DIR)/src/unix/kqueue.o \
	$(LIBUV_DIR)/src/unix/proctitle.o

CPPFLAGS += -D_DARWIN_USE_64_BIT_INODE=1 -D_DARWIN_UNLIMITED_SELECT=1
endif

CPPFLAGS += -DLING_WITH_LIBUV=1
CPPFLAGS += -isystem $(LIBUV_DIR)/include
CPPFLAGS += -isystem $(LIBUV_DIR)/src

LIBUV_CFLAGS := -Wall -O2

LIBUV_DEP := $(patsubst %.o,%.d,$(LIBUV_SRC))
-include $(LIBUV_DEP)

$(LIBUV_SRC): %.o: %.c .config
	$(CC) -MMD -MP $(LIBUV_CFLAGS) $(CPPFLAGS) -o $@ -c $<

ALL_OBJ += $(LIBUV_SRC)
