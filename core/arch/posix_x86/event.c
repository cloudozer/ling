
#include "ling_common.h"

#include "event.h"

int console_do_pending(void);

void events_poll(uint64_t ticks)
{
	//TODO
}

int events_do_pending(void)
{
	//TODO
	return console_do_pending();
}

