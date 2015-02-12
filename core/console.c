
#include "ling_common.h"

#include "console.h"

// this works only for qemu-system-arm -M versatilepb
volatile uint32_t * const UART0 = (uint32_t *)0x101f1000;

int console_is_initialized(void)
{
	return 0;
}

void console_attach(outlet_t *ol)
{
	//TODO
	fatal_error("not implemented");
}

void console_detach(outlet_t *ol)
{
	//TODO
	fatal_error("not implemented");
}

int console_write(char *buf, int len)
{
	//TODO
	fatal_error("not implemented");
}

int ser_cons_present(void)
{
	return 1;
}

int ser_cons_write(char *buf, int len)
{
	while (len > 0)
	{
		*UART0 = (uint32_t)(*buf++);
		len--;
	}
	return 0;
}

