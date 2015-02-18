#pragma once

#include <stdint.h>

#define GPFSEL1 	0x20200004
#define GPSET0  	0x2020001C
#define GPCLR0  	0x20200028
#define GPPUD       0x20200094
#define GPPUDCLK0   0x20200098

#define AUX_ENABLES     0x20215004
#define AUX_MU_IO_REG   0x20215040
#define AUX_MU_IER_REG  0x20215044
#define AUX_MU_IIR_REG  0x20215048
#define AUX_MU_LCR_REG  0x2021504C
#define AUX_MU_MCR_REG  0x20215050
#define AUX_MU_LSR_REG  0x20215054
#define AUX_MU_MSR_REG  0x20215058
#define AUX_MU_SCRATCH  0x2021505C
#define AUX_MU_CNTL_REG 0x20215060
#define AUX_MU_STAT_REG 0x20215064
#define AUX_MU_BAUD_REG 0x20215068

typedef struct outlet_t outlet_t;

void console_init(void);

void console_attach(outlet_t *ol);
void console_detach(outlet_t *ol);
int console_write(char *buf, int len);

int console_do_pending(void);

int ser_cons_write(char *bur, int len);

