
#include <stdint.h>

#include "ling_common.h"

// called by startup code
void setup_vectors()
{
	extern uint32_t _vectors_start;
	extern uint32_t _vectors_end;

	uint32_t *dst = (uint32_t *)0;
	uint32_t *src = &_vectors_start;
	while (src < &_vectors_end)
		*dst++ = *src++;
}

void __attribute__((interrupt)) undef_handler(void)
{
	//TODO
	printk("#UNDEF\n");
	while (1);
}

void __attribute__((interrupt)) swi_handler(void)
{
	//TODO
	printk("#SWI\n");
	while (1);
}

void __attribute__((interrupt)) prefetch_abort_handler(void)
{
	//TODO
	printk("#PREFETCH_ABORT\n");
	while (1);
}

void __attribute__((interrupt)) data_abort_handler(void)
{
	//TODO
	printk("#DATA_ABORT\n");
	while (1);
}

void __attribute__((interrupt)) irq_handler(void)
{
	//TODO
	printk("#IRQ_ABORT\n");
	while (1);
}

void __attribute__((interrupt)) fiq_handler(void)
{
	//TODO
	printk("#FIQ_ABORT\n");
	while (1);
}

