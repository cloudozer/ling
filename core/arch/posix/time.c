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

#include "ling_common.h"

#include "time.h"

#include <string.h>
#include <time.h>
#include <sys/time.h>

#ifdef __APPLE__
# include <mach/mach.h>
# include <mach/mach_time.h>
# include <CoreServices/CoreServices.h>
#endif //__APPLE__

/* required for statistics */
uint64_t start_of_day_wall_clock = 0;
static uint64_t monotonic_delta = 0;

void time_init(void)
{
    start_of_day_wall_clock = wall_clock();
    monotonic_delta = start_of_day_wall_clock - monotonic_clock();
    /*printf("start_of_day_wall_clock = %llu\n",
           (unsigned long long)start_of_day_wall_clock); // */
}

uint64_t monotonic_clock(void)
{
    uint64_t nsec = 0;

#ifdef __APPLE__
    mach_timebase_info_data_t timebase_info;
    mach_timebase_info(&timebase_info);
    uint64_t ret = mach_absolute_time();
    nsec = ret * timebase_info.numer / timebase_info.denom;
#else
    struct timespec tp = { .tv_sec = 0, .tv_nsec = 0 };
    clock_gettime(CLOCK_MONOTONIC, &tp);
    nsec = tp.tv_sec * 1000000000ull + (uint64_t)tp.tv_nsec;
#endif
    return nsec + monotonic_delta; // nanoseconds
}

uint64_t wall_clock(void)
{
    struct timeval tv = { .tv_sec = 0, .tv_usec = 0 };
    gettimeofday(&tv, NULL);
    uint64_t t = ((uint64_t)tv.tv_sec * 1000000ull + (uint64_t)tv.tv_usec);
    return 1000ull * t; // nanoseconds
}

void sleep_us(unsigned usec)
{
	struct timespec timeout;
	timeout.tv_sec = usec / 1000000;
	timeout.tv_nsec = 1000 * (usec % 1000000);
	nanosleep(&timeout, NULL);
}

void expand_time(struct time_exp_t *xt, uint64_t wall_clock)
{
    memset(xt, 0, sizeof(*xt));
    time_t wall_sec = wall_clock / 1000000000ull;

    struct tm tm;
    gmtime_r(&wall_sec, &tm);

    xt->tm_usec   = wall_clock % 1000000000ull;
    xt->tm_sec    = tm.tm_sec;
    xt->tm_min    = tm.tm_min;
    xt->tm_hour   = tm.tm_hour;
    xt->tm_mday   = tm.tm_mday;
    xt->tm_mon    = tm.tm_mon;
    xt->tm_year   = tm.tm_year;
    xt->tm_wday   = tm.tm_wday;
    xt->tm_yday   = tm.tm_yday;
    xt->tm_isdst  = tm.tm_isdst;
    xt->tm_gmtoff = tm.tm_gmtoff;
}

