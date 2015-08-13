#include "ling_common.h"

#include <unistd.h>

extern void start_ling(int argc, char **argv);

int
main(int argc, char **argv)
{
	/* read domain name */
	gethostname(my_domain_name, DOMAIN_NAME_MAX_SIZE);

	/* ready to rock! */
	start_ling(argc, argv);
	return 0;
}

void
yield(void)
{
}
