#include "ling_common.h"

#include "console.h"
#include "outlet.h"

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>

#define constwrite1(s) write(1, (s), sizeof(s)-1)

static outlet_t *attached_outlet;
struct termios saved_termios;
bool console_available = false;

void console_restore_termios(void)
{
	tcsetattr(STDIN_FILENO, TCSANOW, &saved_termios);
}

void
console_init(void)
{
	int ret;

	int flag = O_NONBLOCK;
	fcntl(STDIN_FILENO, F_SETFL, flag);

	console_available = true;

	/* switch TTY to raw mode */
	ret = tcgetattr(STDIN_FILENO, &saved_termios);
	if (ret) return;

	struct termios raw_termios = saved_termios;
	cfmakeraw(&raw_termios);
	raw_termios.c_oflag |= OPOST;

	ret = tcsetattr(STDIN_FILENO, TCSANOW, &raw_termios);
	if (ret) return;

	/* don't forget to clean up */
	atexit(console_restore_termios);
}

int
console_is_initialized(void)
{
	return console_available;
}

void
console_attach(outlet_t *ol)
{
	attached_outlet = ol;
}

void
console_detach(outlet_t *ol)
{
	debug("%s()\n", __FUNCTION__);
	attached_outlet = NULL;
}

int
console_write(char *buf, int len)
{
	return write(STDOUT_FILENO, buf, len);
}

int
ser_cons_write(char *buf, int len)
{
	return len;
}

int
console_do_pending(void)
{
	char buf[1];
	int total = 0;

	while (read(STDIN_FILENO, buf, 1) == 1) {
		if (attached_outlet)
			outlet_pass_new_data(attached_outlet, (uint8_t *)buf, 1);
		total++;
	}
	return total;
}

#if LING_DEBUG

#include <stdarg.h>

int debug(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int ret = vfprintf(stderr, fmt, ap);
    va_end(ap);
    return ret;
}
#endif
