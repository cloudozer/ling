
#include "ling_common.h"

#include "event.h"

#if LING_WITH_LIBUV
# include <uv.h>
#endif

int console_do_pending(void);

void events_poll(uint64_t ticks)
{
#if LING_WITH_LIBUV
    uv_loop_t *the_loop = uv_default_loop();

    uv_run(the_loop, UV_RUN_NOWAIT);
#endif
}

int events_do_pending(void)
{
	//TODO
	return console_do_pending();
}

