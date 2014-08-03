#pragma once

#include <stdint.h>

void events_poll(uint64_t ticks);
int events_do_pending(void);

//EOF
