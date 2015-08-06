
#include "ling_common.h"

#include "event.h"
#include "time.h"

#if LING_WITH_LIBUV
# include <uv.h>
#endif

int console_do_pending(void);

void events_poll(uint64_t ticks)
{
#if LING_WITH_LIBUV
	/* HACK: don't make my CPU a frying pan! */
	sleep_us(5000);
	uv_run(uv_default_loop(), UV_RUN_NOWAIT);
#endif
}

int events_do_pending(void)
{
	//TODO
	return console_do_pending();
}

