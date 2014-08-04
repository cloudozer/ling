#pragma once

#include <stdint.h>

typedef struct disk_info_t disk_info_t;
struct disk_info_t {
	uint32_t info;
	uint64_t sectors;
	uint32_t sector_size;
	int barrier;
	int flush;
	int trim;
};

disk_info_t *disk_get_info(void);

//EOF
