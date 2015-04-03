
#include "ling_common.h"

#include "console.h"
#include "outlet.h"

extern int printf(const char * restrict format, ...);

void
console_init(void)
{
	printf("console hello\n");
}

void
console_attach(outlet_t *ol)
{
	printf("console attach\n");
}

void
console_detach(outlet_t *ol)
{
	printf("console detach\n");
}

int
console_write(char *buf, int len)
{
	return printf("%.*s",len,buf);
}

int
ser_cons_write(char *buf, int len)
{
	return printf("%.*s",len,buf);
}

int
console_do_pending(void)
{
	return 0;
}

