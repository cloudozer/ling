
#include "ling_common.h"

#include "console.h"
#include "outlet.h"

#include <string.h>

static void store(uint32_t addr, uint32_t val)
{
	asm volatile("str %1, [%0]" : : "r" (addr), "r" (val));
}

static uint32_t load(uint32_t addr)
{
	uint32_t val;
	asm volatile("ldr %0, [%0]" : "=r" (val) : "r" (addr));
	return val;
}

void marker(char ch)
{
     while(1)
     {
     	if(load(AUX_MU_LSR_REG) & 0x20)
			break;
     }
     
	store(AUX_MU_IO_REG, ch);
}
 
void console_init(void)
{
    store(AUX_ENABLES,1);
    store(AUX_MU_IER_REG,0);
    store(AUX_MU_CNTL_REG,0);
    store(AUX_MU_LCR_REG,3);
    store(AUX_MU_MCR_REG,0);
    store(AUX_MU_IER_REG,0);
    store(AUX_MU_IIR_REG,0xC6);
    store(AUX_MU_BAUD_REG,270);

	uint32_t ra;
    ra=load(GPFSEL1);
    ra&=~(7<<12); //gpio14
    ra|=2<<12;    //alt5
    ra&=~(7<<15); //gpio15
    ra|=2<<15;    //alt5
    store(GPFSEL1,ra);

    store(GPPUD,0);
    for(ra=0;ra<150;ra++);	//XXX
    store(GPPUDCLK0,(1<<14) | (1 << 15));
    for(ra=0;ra<150;ra++);	//XXX
    store(GPPUDCLK0,0);

    store(AUX_MU_CNTL_REG,3);
}

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
	while (send_buf_len > 0 && (load(AUX_MU_LSR_REG) & 0x20))
	{
		nr_fired++;
		store(AUX_MU_IO_REG, send_buffer[send_buf_off++]);
		send_buf_len--;
	}
	if (send_buf_len == 0)
		send_buf_off = 0;
	else if (send_buf_off > SEND_BUF_SIZE /2)
	{
		memmove(send_buffer, send_buffer +send_buf_off, send_buf_len);
		send_buf_off = 0;
	}

	
	while (load(AUX_MU_LSR_REG) & 1)
	{
		nr_fired++;
		uint8_t ch = load(AUX_MU_IO_REG);
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
//	while (len > 0)
//	{
//		UART0->DR = (uint32_t)(*buf++);
//		len--;
//	}
	return 0;
}

