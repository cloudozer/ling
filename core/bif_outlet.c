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

#include "bif_impl.h"

term_t cbif_port_info2(proc_t *proc, term_t *regs)
{
	term_t Oid = regs[0];
	term_t What = regs[1];

	if (!is_short_oid(Oid))
		badarg(Oid);
	if (!is_atom(What))
		badarg(What);

	outlet_t *ol = outlet_lookup(Oid);
	if (ol == 0)
		return A_UNDEFINED;

	term_t val;
	if (What == A_REGISTERED_NAME)
	{
		val = ol->name;
		if (val == noval)
			return nil;		// be backwards
	}
	else if (What == A_ID)
		val = tag_int(short_oid_id(Oid));
	else if (What == A_CONNECTED)
		val = ol->owner;
	else if (What == A_LINKS)
		val = nil;
	else if (What == A_NAME)
		val = A_UNDEFINED;
	else if (What == A_INPUT)
		val = A_UNDEFINED;
	else if (What == A_OUTPUT)
		val = A_UNDEFINED;
	else
		badarg(What);

	return heap_tuple2(&proc->hp, What, val);
}

term_t cbif_port_open2(proc_t *proc, term_t *regs)
{
	term_t Drv = regs[0];
	term_t BitOpts = regs[1];
	if (!is_atom(Drv))
		badarg(Drv);
	if (!is_int(BitOpts))
		badarg(BitOpts);

	outlet_factory_func_t factory_N = outlet_resolve_driver(Drv);
	if (factory_N == 0)
		badarg(Drv);

	outlet_t *new_ol = factory_N(proc, int_value(BitOpts));
	if (new_ol == 0)
		fail(A_NO_MEMORY);

	int x =	outlet_attach(new_ol);
	if (x < 0)
		fail(err_to_term(x));

	return new_ol->oid;
}

term_t cbif_port_is_busy1(proc_t *proc, term_t *regs)
{
	//TODO
	bif_not_implemented();
}

term_t cbif_port_control3(proc_t *proc, term_t *regs)
{
	term_t Port = regs[0];
	term_t Op = regs[1];
	term_t Data = regs[2];

	//printk("port_control(%pt, %pt, %pt)\n", T(Port), T(Op), T(Data));
	
	if (!is_short_oid(Port) && !is_atom(Port))
		badarg(Port);
	if (!is_int(Op) && !is_boxed_bignum(Op))
		badarg(Op);
	if (!is_list(Data) && !is_boxed_binary(Data))
		badarg(Data);

	outlet_t *ol = (is_short_oid(Port))
		?outlet_lookup(Port)
		:outlet_lookup_by_name(Port);
	if (ol == 0)
		badarg(Port);

	int64_t v = (is_int(Op))
		?int_value(Op)
		:bignum_to_int((bignum_t *)peel_boxed(Op));
	if (v < 0 || v > 0xffffffff)
		badarg(Op);

	int sz = iolist_size(Data);
	if (sz < 0)
		badarg(Data);

	uint8_t buf[sz];
	iolist_flatten(Data, buf);

	term_t reply_to = proc->pid;
	term_t result = outlet_control(ol, (uint32_t)v, buf, sz, reply_to, &proc->hp);
	if (is_atom(result))
		fail(result);

	return result;
}

term_t cbif_port_set_data2(proc_t *proc, term_t *regs)
{
	term_t Oid = regs[0];
	term_t Data = regs[1];
	if (!is_short_oid(Oid))
		badarg(Port);
	if (!is_immed(Data))
		badarg(Data);

	outlet_t *ol = outlet_lookup(Oid);
	if (ol == 0)
		return A_FALSE;
	ol->data = Data;
	return A_TRUE;
}

term_t cbif_port_get_data1(proc_t *proc, term_t *regs)
{
	term_t Oid = regs[0];
	if (!is_short_oid(Oid))
		badarg(Port);

	outlet_t *ol = outlet_lookup(Oid);
	if (ol == 0)
		badarg(Oid);
	return ol->data;
}

term_t cbif_decode_packet4(proc_t *proc, term_t *regs)
{
	term_t Type = regs[0];
	term_t Bin = regs[1];
	term_t PackSize = regs[2];
	term_t LineLen = regs[3];

	if (!is_boxed_binary(Bin))
		badarg(Bin);
	if (!is_int(PackSize) || int_value(PackSize) < 0)
		badarg(PackSize);
	if (!is_int(LineLen) || int_value(LineLen) < 0)
		badarg(LineLen);

	uint32_t pack_size = int_value(PackSize);
	uint32_t line_len = int_value(LineLen);

	int t;
	if (Type == A_RAW || Type == tag_int(0))	t = TCP_PB_RAW;
	else if (Type == tag_int(1))	t = TCP_PB_1;
	else if (Type == tag_int(2))	t = TCP_PB_2;
	else if (Type == tag_int(4))	t = TCP_PB_4;
	else if (Type == A_ASN1)		t = TCP_PB_ASN1;
	else if (Type == A_SUNRM)		t = TCP_PB_RM;
	else if (Type == A_CDR)			t = TCP_PB_CDR;
	else if (Type == A_FCGI)		t = TCP_PB_FCGI;
	else if (Type == A_LINE)		t = TCP_PB_LINE_LF;
	else if (Type == A_TPKT)		t = TCP_PB_TPKT;
	else if (Type == A_HTTP)		t = TCP_PB_HTTP;
	else if (Type == A_HTTPH)		t = TCP_PB_HTTPH;
	else if (Type == A_SSL_TLS)		t = TCP_PB_SSL_TLS;
	else if (Type == A_HTTP_BIN)	t = TCP_PB_HTTP_BIN;
	else if (Type == A_HTTPH_BIN)	t = TCP_PB_HTTPH_BIN;
	else
		badarg(Type);

	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);

	term_t reason;
	uint32_t more_len;
	term_t packet = decode_packet_N(t, &bs, bin_parent(Bin), 1,
				&reason, &more_len, pack_size, line_len, &proc->hp);
	if (packet == noval)
	{
		if (reason == A_NO_MEMORY)
			fail(reason);

		return heap_tuple2(&proc->hp, A_ERROR, reason);
	}
	else if (packet == A_MORE && more_len == 0)
		return heap_tuple2(&proc->hp, A_MORE, A_UNDEFINED);
	else if (packet == A_MORE)
		return heap_tuple2(&proc->hp, A_MORE, int_to_term(more_len, &proc->hp));

	int wsize = WSIZE(t_sub_bin_t);
	uint32_t *p = heap_alloc(&proc->hp, wsize);
	term_t rest = tag_boxed(p);
	box_sub_bin(p, bin_parent(Bin), bs.starts, bs.ends, 0);
	heap_set_top(&proc->hp, p);

	return heap_tuple3(&proc->hp, A_OK, packet, rest);
}

static int bin_to_addr(ip_addr_t *addr, term_t Bin)
{
	assert(is_boxed_binary(Bin));
	bits_t bs;
	bits_get_real(peel_boxed(Bin), &bs);
	if (((bs.ends -bs.starts) & 7) != 0)
		return -BAD_ARG;
	int len = (bs.ends -bs.starts) /8;

	char buf[len +1];

	bits_t dst;
	bits_init_buf((uint8_t *)buf, len, &dst);
	bits_copy(&bs, &dst);
	buf[len] = 0;

	if (!ipaddr_aton(buf, addr))
		return -BAD_ARG;

	return 0;
}

static void status_cb(struct netif *netif)
{
	if (netif_is_up(netif))
	{
		// init ! netif_up
		proc_t *init_proc = scheduler_process_by_name(A_INIT);
		assert(init_proc != 0);
		if (scheduler_new_local_mail_N(init_proc, A_NETIF_UP) < 0)
		{
			printk("status_cb: cannot notify init about status change\n");
			//ignore -- killing init is not an option
		}
	}
}

term_t cbif_setup4(proc_t *proc, term_t *regs)
{
	term_t IpAddr = regs[0];
	term_t NetMask = regs[1];
	term_t Gateway = regs[2];
	term_t Dhcp = regs[3];

	if (IpAddr != A_UNDEFINED && !is_boxed_binary(IpAddr))
		badarg(IpAddr);
	if (NetMask != A_UNDEFINED && !is_boxed_binary(NetMask))
		badarg(NetMask);
	if (Gateway != A_UNDEFINED && !is_boxed_binary(Gateway))
		badarg(Gateway);
	if (!is_bool(Dhcp))
		badarg(Dhcp);

	if (netfe_get_eth_by_index(0) == 0)
	{
		printk("lwip:setup(): no vif detected - networking flags ignored\n");
		return A_FALSE;
	}

	ip_addr_t ip_addr, net_mask, gateway;
	ip_addr_set_zero(&ip_addr);
	ip_addr_set_zero(&net_mask);
	ip_addr_set_zero(&gateway);

	if (IpAddr != A_UNDEFINED)
	{
		if (bin_to_addr(&ip_addr, IpAddr) < 0)
			badarg(IpAddr);
	}

	if (NetMask != A_UNDEFINED)
	{
		if (bin_to_addr(&net_mask, NetMask) < 0)
			badarg(NetMask);
	}

	if (Gateway != A_UNDEFINED)
	{
		if (bin_to_addr(&gateway, Gateway) < 0)
			badarg(Gateway);
	}

	netif_setup(&ip_addr,
				&net_mask,
				&gateway, Dhcp == A_TRUE, status_cb);
	return A_TRUE;
}

//EOF
