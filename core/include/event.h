#pragma once

#include <stdint.h>
#include "arch_event.h"

void events_poll(uint64_t ticks);
int events_do_pending(void);

//EOF
