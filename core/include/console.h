#pragma once

#include <stdint.h>

typedef volatile struct {
	uint32_t DR;
	uint32_t RSR_ECR;
	uint8_t reserved1[0x10];
	const uint32_t FR;
	uint8_t reserved2[0x4];
	uint32_t LPR;
	uint32_t IBRD;
	uint32_t FBRD;
	uint32_t LCR_H;
	uint32_t CR;
	uint32_t IFLS;
	uint32_t IMSC;
	const uint32_t RIS;
	const uint32_t MIS;
	uint32_t ICR;
	uint32_t DMACR;
} pl011_t;

enum {
	RXFE = 0x10,
	TXFF = 0x20,
};
 
typedef struct outlet_t outlet_t;

void console_attach(outlet_t *ol);
void console_detach(outlet_t *ol);
int console_write(char *buf, int len);

int console_do_pending(void);

int ser_cons_present(void);
int ser_cons_write(char *bur, int len);

