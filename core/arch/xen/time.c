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

#include "time.h"
#include "arch_time.h"

#include "ling_common.h"
#include "ling_xen.h"

#define SECS_DAY		(24*60*60)
#define EPOCH_YR		1970
#define YEAR0			1900
#define LEAPYEAR(year)  (!((year) % 4) && (((year) % 100) || !((year) % 400)))
#define YEARSIZE(year)  (LEAPYEAR(year) ? 366 : 365)

static uint64_t wall_clock_base;
static vcpu_time_info_t shadow;

// retrieved once by time_init()
uint64_t start_of_day_wall_clock;

static const int _ytab[2][12] = {
	{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
	{ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
};

static unsigned long get_nsec_offset(void);

uint64_t scale_delta(uint64_t delta, uint32_t mul_frac, int shift);
void get_time_values_from_xen(void);

void time_init(void)		// UTC
{
	get_time_values_from_xen();

	uint32_t version;
	uint32_t wc_sec;
	uint32_t wc_nsec;
	do {
		version = shared_info.wc_version;
		rmb();
		wc_sec = shared_info.wc_sec;
		wc_nsec = shared_info.wc_nsec;
		rmb();
	} while ((version & 1) | (version ^ shared_info.wc_version));

	wall_clock_base = (uint64_t)wc_sec * 1000000000ULL +wc_nsec;
	start_of_day_wall_clock = wall_clock();
}

void get_time_values_from_xen(void)
{
	vcpu_time_info_t *vt = &shared_info.vcpu_info[0].time;

	do {
		shadow.version = vt->version;
		rmb();
		shadow.tsc_timestamp = vt->tsc_timestamp;
		shadow.system_time = vt->system_time;
		shadow.tsc_to_system_mul = vt->tsc_to_system_mul;
		shadow.tsc_shift = vt->tsc_shift;
		rmb();
	} while ((shadow.version & 1) | (shadow.version ^ vt->version));
}

static unsigned long get_nsec_offset(void)
{
	uint64_t tsc, delta;
    rdtscll(tsc);
    delta = tsc - shadow.tsc_timestamp;
    return scale_delta(delta, shadow.tsc_to_system_mul, shadow.tsc_shift);
}

uint64_t monotonic_clock(void)
{
	if (shared_info.vcpu_info[0].time.version != shadow.version)
		get_time_values_from_xen();
	return shadow.system_time + get_nsec_offset();
}

//void dump_time_values(void)
//{
//	printk("tsc_timestamp=%016llx\n", shadow.tsc_timestamp);
//	printk("system_time=%llu\n", shadow.system_time);
//	printk("tsc_to_system_mul=%u\n", shadow.tsc_to_system_mul);
//	printk("tsc_shift=%d\n", shadow.tsc_shift);
//	printk("get_nsec_offset()=%lu\n", get_nsec_offset());
//	printk("monotonic_clock()=%lu\n", (long unsigned int)monotonic_clock());
//	uint64_t wc = wall_clock();
//	struct time_exp_t xt;
//	expand_time(&xt, wc);
//	printk("wall_clock()=%lu (%d/%d/%d %d:%d:%d)\n", (long unsigned int) wc,
//		   	xt.tm_mday, xt.tm_mon+1, xt.tm_year+1900,
//			xt.tm_hour, xt.tm_min, xt.tm_sec);
//}

uint64_t wall_clock(void)
{
	return wall_clock_base +monotonic_clock();
}

void expand_time(struct time_exp_t *xt, uint64_t wall_clock)
{
	uint32_t time = NS_TO_SECS(wall_clock);
	unsigned long dayclock, dayno;
	int year = EPOCH_YR;

	dayclock = (unsigned long)time % SECS_DAY;
	dayno = (unsigned long)time / SECS_DAY;
	
	xt->tm_usec = NS_TO_US(wall_clock) % 1000000ULL;

	xt->tm_sec = dayclock % 60;
	xt->tm_min = (dayclock % 3600) / 60;
	xt->tm_hour = dayclock / 3600;
	xt->tm_wday = (dayno + 4) % 7;       /* day 0 was a thursday */
	while (dayno >= YEARSIZE(year)) {
		 dayno -= YEARSIZE(year);
		 year++;
	}
	xt->tm_year = year - YEAR0;
	xt->tm_yday = dayno;
	xt->tm_mon = 0;
	while (dayno >= _ytab[LEAPYEAR(year)][xt->tm_mon]) {
		 dayno -= _ytab[LEAPYEAR(year)][xt->tm_mon];
		 xt->tm_mon++;
	}
	xt->tm_mday = dayno + 1;
	
	xt->tm_isdst = 0;
	xt->tm_gmtoff = 0;
}

/*EOF*/
