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


#include "ling_common.h"

#include "time.h"

#ifdef EXP_LINC_LATENCY

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

#ifdef EXP_LINC_LLSTAT

#define LLSTAT_MAX_INTS		500000

static int llstat_running = 0;
static int llstat_iface;

static uint64_t llstat_started_at;
static uint64_t llstat_stopped_at;

static int64_t llstat_nr_ints;
static int64_t llstat_nr_int_kicks;

static int64_t llstat_rx_cons_min;
static int64_t llstat_rx_cons_sum;
static int64_t llstat_rx_cons_max;

static int64_t llstat_int_tx_freed_min;
static int64_t llstat_int_tx_freed_sum;
static int64_t llstat_int_tx_freed_max;

static int64_t llstat_nr_outs;
static int64_t llstat_nr_out_kicks;
static int64_t llstat_nr_drops;

static int64_t llstat_out_tx_freed_min;
static int64_t llstat_out_tx_freed_sum;
static int64_t llstat_out_tx_freed_max;

static int64_t llstat_out_tx_len;
static int64_t llstat_drop_tx_len;

static int llstat_results_ready = 0;

void llstat_restart(int ifidx)
{
	llstat_started_at = monotonic_clock();

	llstat_nr_ints = 0;
	llstat_nr_int_kicks = 0;

	llstat_rx_cons_min = 0;
	llstat_rx_cons_sum = 0;
	llstat_rx_cons_max = 0;

	llstat_int_tx_freed_min = 0;
	llstat_int_tx_freed_sum = 0;
	llstat_int_tx_freed_max = 0;

	llstat_nr_outs = 0;
	llstat_nr_out_kicks = 0;
	llstat_nr_drops = 0;

	llstat_out_tx_freed_min = 0;
	llstat_out_tx_freed_sum = 0;
	llstat_out_tx_freed_max = 0;

	llstat_out_tx_len = 0;
	llstat_drop_tx_len = 0;

	llstat_iface = ifidx;
	llstat_running = 1;
}

void llstat_stop()
{
	if (!llstat_running)
		return;

	llstat_stopped_at = monotonic_clock();
	llstat_running = 0;
	llstat_results_ready = 1;
}

void linc_int_stat(int ifidx, int rx_consumed, int tx_freed, int kicked)
{
	if (!llstat_running || ifidx != llstat_iface)
		return;

	llstat_nr_ints++;
	if (kicked)
		llstat_nr_int_kicks++;

	if (llstat_rx_cons_min > rx_consumed)
		llstat_rx_cons_min = rx_consumed;
	llstat_rx_cons_sum += rx_consumed;
	if (llstat_rx_cons_max < rx_consumed)
		llstat_rx_cons_max = rx_consumed;

	if (llstat_int_tx_freed_min > tx_freed)
		llstat_int_tx_freed_min = tx_freed;
	llstat_int_tx_freed_sum += tx_freed;
	if (llstat_int_tx_freed_max < tx_freed)
		llstat_int_tx_freed_max = tx_freed;

	if (llstat_nr_ints >= LLSTAT_MAX_INTS)
		llstat_stop();
}

void linc_out_stat(int ifidx, int tx_len, int tx_freed, int kicked)
{
	if (!llstat_running || ifidx != llstat_iface)
		return;

	llstat_nr_outs++;
	if (kicked)
		llstat_nr_out_kicks++;

	if (llstat_out_tx_freed_min > tx_freed)
		llstat_out_tx_freed_min = tx_freed;
	llstat_out_tx_freed_sum += tx_freed;
	if (llstat_out_tx_freed_max < tx_freed)
		llstat_out_tx_freed_max = tx_freed;

	llstat_out_tx_len += tx_len;
}

void linc_out_drop(int ifidx, int tx_len)
{
	if (!llstat_running || ifidx != llstat_iface)
		return;

	llstat_nr_drops++;
	llstat_drop_tx_len += tx_len;
}

void llstat_display()
{
	if (llstat_running)
	{
		printk("*** Statistics are still being collected\n");
		return;
	}

	if (!llstat_results_ready || llstat_nr_ints == 0)
	{
		printk("*** No statistics collected\n");
		return;
	}

	uint64_t elapsed_ns = llstat_stopped_at -llstat_started_at;

	printk("Duration: %.1fms\n", elapsed_ns /1000000.0);
	printk("RX: interrupts: %llu (%llu kicks %.1f%%) (freq %.1f/s period %.1fus)\n",
			llstat_nr_ints,
			llstat_nr_int_kicks,
			llstat_nr_int_kicks *100.0 /llstat_nr_ints,
			llstat_nr_ints *1000000000.0 /elapsed_ns,
			elapsed_ns /1000.0 /llstat_nr_ints);
	printk("RX: reqs per int: %llu/%.1f/%llu\n",
			llstat_rx_cons_min,
			(double)llstat_rx_cons_sum /llstat_nr_ints,
			llstat_rx_cons_max);
	printk("RX: tx buf freed per int: %llu/%.1f/%llu\n",
			llstat_int_tx_freed_min,
			(double)llstat_int_tx_freed_sum /llstat_nr_ints,
			llstat_int_tx_freed_max);

	if (llstat_nr_outs > 0)
	{
		printk("TX: outputs: %llu (%llu kicks %.1f%%) (freq %.1f/s period %.1fus)\n",
				llstat_nr_outs,
				llstat_nr_out_kicks,
				llstat_nr_out_kicks *100.0 /llstat_nr_outs,
				llstat_nr_outs *1000000000.0 /elapsed_ns,
				elapsed_ns /1000.0 /llstat_nr_outs);
		printk("TX: tx buf freed per int: %llu/%.1f/%llu\n",
				llstat_out_tx_freed_min,
				(double)llstat_out_tx_freed_sum /llstat_nr_outs,
				llstat_out_tx_freed_max);
		printk("TX: rates: %.1fkpps %.2fMbps avg pkt size %.1fB\n",
				llstat_nr_outs *1000000.0 /elapsed_ns,
				llstat_out_tx_len *8 *1000.0 /elapsed_ns,
				(double)llstat_out_tx_len /llstat_nr_outs);
	}

	if (llstat_nr_drops > 0)
	{
		printk("TX: drops: %llu (freq %.1f/s period %.1fus)\n",
				llstat_nr_drops,
				llstat_nr_drops *1000000000.0 /elapsed_ns,
				elapsed_ns /1000.0 /llstat_nr_drops);
		printk("TX: drop rates: %.1fkpps %.2fMbps avg pkt size %.1fB\n",
				llstat_nr_drops *1000000.0 /elapsed_ns,
				llstat_drop_tx_len *8 *1000.0 /elapsed_ns,
				(double)llstat_drop_tx_len /llstat_nr_drops);
	}
}

#endif // EXP_LINC_LLSTAT

//EOF
