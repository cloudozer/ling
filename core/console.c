
#include "ling_common.h"

#include "console.h"
#include "outlet.h"

#include <string.h>

// this works only for qemu-system-arm -M versatilepb
pl011_t *UART0 = (pl011_t *)0x101f1000;
UNUSED pl011_t *UART1 = (pl011_t *)0x101f2000;
UNUSED pl011_t *UART2 = (pl011_t *)0x101f3000;

#define SEND_BUF_SIZE		1024

static outlet_t *attached_outlet = 0;

static uint8_t send_buffer[SEND_BUF_SIZE];
static int send_buf_off = 0;
static int send_buf_len = 0;
static int buffer_overrun = 0;

void console_attach(outlet_t *ol)
{
	if (attached_outlet != 0)
		printk("%ptWARNING: steals control over console from %pt\n",
				T(ol->oid), T(attached_outlet->oid));
	attached_outlet = ol;
}

void console_detach(outlet_t *ol)
{
	assert(attached_outlet == ol);
	attached_outlet = 0;
}

int console_write(char *buf, int len)
{
	int left = SEND_BUF_SIZE -send_buf_off -send_buf_len;
	if (len > left)
	{
		buffer_overrun += (len -left);
		len = left;
	}
	memcpy(send_buffer +send_buf_off +send_buf_len, buf, len);
	send_buf_len += len;
	return len;
}

int console_do_pending(void)
{
	int nr_fired = 0;
	pl011_t *uart = UART0;
	while (send_buf_len > 0 && ((uart->FR & TXFF) == 0))
	{
		nr_fired++;
		uart->DR = send_buffer[send_buf_off++];
		send_buf_len--;
	}
	if (send_buf_len == 0)
		send_buf_off = 0;
	else if (send_buf_off > SEND_BUF_SIZE /2)
	{
		memmove(send_buffer, send_buffer +send_buf_off, send_buf_len);
		send_buf_off = 0;
	}

	while ((uart->FR & RXFE) == 0)
	{
		nr_fired++;
		uint8_t ch = uart->DR;
		if (attached_outlet != 0)
			outlet_pass_new_data(attached_outlet, &ch, 1);
	}

	return nr_fired;
}

int ser_cons_present(void)
{
	return 1;
}

int ser_cons_write(char *buf, int len)
{
	while (len > 0)
	{
		UART0->DR = (uint32_t)(*buf++);
		len--;
	}
	return 0;
}

