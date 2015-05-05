// Copyright (c) 2013-2014 Cloudozer LLP. All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
// 
// * Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// 
// * Redistributions in any form must be accompanied by information on how to
// obtain complete source code for the LING software and any accompanying
// software that uses the LING software. The source code must either be included
// in the distribution or be available for no more than the cost of distribution
// plus a nominal fee, and must be freely redistributable under reasonable
// conditions.  For an executable file, complete source code means the source
// code for all modules it contains. It does not include source code for modules
// or files that typically accompany the major components of the operating
// system on which the executable file runs.
// 
// THIS SOFTWARE IS PROVIDED BY CLOUDOZER LLP ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE
// DISCLAIMED. IN NO EVENT SHALL CLOUDOZER LLP BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#pragma once

#include <stdint.h>

#define SECTOR_SIZE			512
#define SECTORS_PER_PAGE	(PAGE_SIZE / SECTOR_SIZE)

#define DISK_REQ_READ		0
#define DISK_REQ_WRITE		1
#define	DISK_REQ_BARRIER	2
#define DISK_REQ_FLUSH		3
#define	DISK_REQ_TRIM		4

#define DISK_REP_OK			0
#define DISK_REP_ERROR		1
#define DISK_REP_ENOTSUP	2

typedef struct disk_info_t disk_info_t;
struct disk_info_t {
	uint32_t info;
	uint64_t sectors;
	uint32_t sector_size;
	int barrier;
	int flush;
	int trim;
};

typedef void (*complete_func_t)(int16_t status, void *info, void *arg);

int disk_vbd_is_present(void);
void disk_init(void);

int disk_read(uint64_t sector_start, uint32_t num_sectors,
					complete_func_t complete_cb, void *callback_arg);
int disk_write(uint64_t sector_start, uint8_t *data, int dlen,
					complete_func_t complete_cb, void *callback_arg);
int disk_barrier(complete_func_t complete_cb, void *callback_arg);
int disk_flush(complete_func_t complete_cb, void *callback_arg);

void disk_retrieve_data(void *info, uint8_t *data, int expected_sectors);

disk_info_t *disk_get_info(void);

//EOF
