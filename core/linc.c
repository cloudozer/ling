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

//
// Temporary harness for tuning LINC performance
//

#ifdef EXP_LINC_LATENCY

#include "ling_common.h"

#include "time.h"

#include <math.h>

#define LINC_STATE_INIT		0
#define LINC_STATE_RECV1	1
#define LINC_STATE_SENT1	2
#define LINC_STATE_RECV2	3
#define LINC_STATE_SENT2	4

#define LINC_VIF1	1
#define LINC_VIF2	2

static int linc_state = LINC_STATE_INIT;

static int linc_num_errs = 0;
static uint64_t linc_recv1_started;
static uint64_t linc_recv2_started;

static int linc_num_sent = 0;
static double running_average;
static double running_variance;

void update_stats(double delay_us)
{
	if (++linc_num_sent == 1)
	{
		running_average = delay_us;
		running_variance = 0.0;
	}
	else
	{
		double old_average = running_average;
		running_average += (delay_us -old_average) /linc_num_sent;
		running_variance += (delay_us -running_average) *(delay_us -old_average);
	}
}

void linc_incoming(int index)
{
	if (index == LINC_VIF1)
	{
		if (linc_state != LINC_STATE_INIT &&
			linc_state != LINC_STATE_SENT1)
			linc_num_errs++;
		
		linc_recv1_started = monotonic_clock();
		linc_state = LINC_STATE_RECV1;
	}
	else if (index == LINC_VIF2)
	{
		if (linc_state != LINC_STATE_SENT2)
			linc_num_errs++;
		
		linc_recv2_started = monotonic_clock();
		linc_state = LINC_STATE_RECV2;
	}
}

void linc_output(int index)
{
	if (index == LINC_VIF1)
	{
		if (linc_state != LINC_STATE_RECV2)
			linc_num_errs++;

		uint64_t l = monotonic_clock() -linc_recv2_started;
		update_stats(l /1000);

		linc_state = LINC_STATE_SENT1;
	}
	else if (index == LINC_VIF2)
	{
		if (linc_state != LINC_STATE_RECV1)
			linc_num_errs++;

		uint64_t l = monotonic_clock() -linc_recv1_started;
		update_stats(l /1000);

		linc_state = LINC_STATE_SENT2;
	}
}

void linc_display()
{
	if (linc_num_errs > 0)
		printk("Cannot estimate processing delay due to out-of-order packets\n");
	else
	{
		if (linc_num_sent == 0)
			printk("No packets received/processed\n");
		else
		{
			double sdev = sqrt((running_variance) /(linc_num_sent -1));
			double conf = 1.96 *sdev /sqrt(linc_num_sent);

			printk("Processing delay statistics:\n");
			printk("Packets: %d\n", linc_num_sent);
			printk("Delay: %0.3fus +- %0.3f (95%)\n", running_average, conf);
		}
	}

	//reset
	linc_state = LINC_STATE_INIT;
	linc_num_errs = 0;
	linc_num_sent = 0;
}

#endif // EXP_LINC_LATENCY
