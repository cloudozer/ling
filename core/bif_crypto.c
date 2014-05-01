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
//
//

#include "bif_impl.h"

term_t cbif_sha1(proc_t *proc, term_t *regs)
{
	term_t Data = regs[0];

	if (!is_boxed_binary(Data) && !is_list(Data))
		badarg(Data);

	int sz = iolist_size(Data);
	if (sz < 0)
		badarg(Data);
	assert(sz <= 65536);	//TODO: use heap_tmp_buf for larger Data
	uint8_t buf[sz];
	iolist_flatten(Data, buf);

	struct sha1_ctx ctx;
	sha1_init(&ctx);
	sha1_update(&ctx, sz, buf);

	uint8_t *ptr;
	term_t bin = heap_make_bin(&proc->hp, SHA1_DIGEST_SIZE, &ptr);
	sha1_digest(&ctx, SHA1_DIGEST_SIZE, ptr);

	return bin;
}

term_t cbif_sha_init0(proc_t *proc, term_t *regs)
{
	struct sha1_ctx *ctx;
	term_t bin = heap_make_bin(&proc->hp, sizeof(*ctx), (uint8_t **)&ctx);
	sha1_init(ctx);
	return bin;
}

term_t cbif_sha_update2(proc_t *proc, term_t *regs)
{
	term_t Context = regs[0];
	term_t Data = regs[1];

	if (!is_boxed_binary(Context))
		badarg(Context);

	bits_t bs, dst;
	bits_get_real(peel_boxed(Context), &bs);
	if (bs.ends -bs.starts != sizeof(struct sha1_ctx) *8)
		badarg(Context);

	struct sha1_ctx ctx;
	bits_init_buf((uint8_t *)&ctx, sizeof(ctx), &dst);
	bits_copy(&bs, &dst);

	if (!is_boxed_binary(Data) && !is_list(Data))
		badarg(Data);

	int sz = iolist_size(Data);
	if (sz < 0)
		badarg(Data);
	assert(sz <= 65536);	//TODO: use heap_tmp_buf for larger Data
	uint8_t buf[sz];
	iolist_flatten(Data, buf);

	sha1_update(&ctx, sz, buf);

	uint8_t *ptr;
	term_t bin = heap_make_bin(&proc->hp, sizeof(ctx), &ptr);
	memcpy(ptr, &ctx, sizeof(ctx));

	return bin;
}

term_t cbif_sha_final1(proc_t *proc, term_t *regs)
{
	term_t Context = regs[0];

	if (!is_boxed_binary(Context))
		badarg(Context);

	bits_t bs, dst;
	bits_get_real(peel_boxed(Context), &bs);
	if (bs.ends -bs.starts != sizeof(struct sha1_ctx) *8)
		badarg(Context);

	struct sha1_ctx ctx;
	bits_init_buf((uint8_t *)&ctx, sizeof(ctx), &dst);
	bits_copy(&bs, &dst);

	uint8_t *ptr;
	term_t bin = heap_make_bin(&proc->hp, SHA1_DIGEST_SIZE, &ptr);
	sha1_digest(&ctx, SHA1_DIGEST_SIZE, ptr);

	return bin;
}

term_t cbif_md5_mac_n3(proc_t *proc, term_t *regs)
{
	term_t Key = regs[0];
	term_t Data = regs[1];
	term_t Size = regs[2];

	if (!is_list(Key) && !is_boxed_binary(Key))
		badarg(Key);
	if (!is_list(Data) && !is_boxed_binary(Data))
		badarg(Data);
	if (!is_int(Size))
		badarg(Size);

	int trunc_size = int_value(Size);
	if (trunc_size < 1 || trunc_size > MD5_DIGEST_SIZE)
		badarg(Size);

	int key_size = iolist_size(Key);
	if (key_size < 0)
		badarg(Key);
	assert(key_size <= 65536);	// TODO: use heap_tmp_buf for a longer Key
	uint8_t key_buf[key_size];
	iolist_flatten(Key, key_buf);

	int data_size = iolist_size(Data);
	if (data_size < 0)
		badarg(Data);
	assert(data_size <= 65536);	// TODO: use heap_tmp_buf for larger Data
	uint8_t data_buf[data_size];
	iolist_flatten(Data, data_buf);

	struct hmac_md5_ctx ctx;
	hmac_md5_set_key(&ctx, key_size, key_buf);
	hmac_md5_update(&ctx, data_size, data_buf);

	uint8_t *ptr;
	term_t mac = heap_make_bin(&proc->hp, trunc_size, &ptr);
	hmac_md5_digest(&ctx, trunc_size, ptr);

	return mac;
}

term_t cbif_sha_mac_n3(proc_t *proc, term_t *regs)
{
	term_t Key = regs[0];
	term_t Data = regs[1];
	term_t Size = regs[2];

	if (!is_list(Key) && !is_boxed_binary(Key))
		badarg(Key);
	if (!is_list(Data) && !is_boxed_binary(Data))
		badarg(Data);
	if (!is_int(Size))
		badarg(Size);

	int trunc_size = int_value(Size);
	if (trunc_size < 1 || trunc_size > SHA1_DIGEST_SIZE)
		badarg(Size);

	int key_size = iolist_size(Key);
	if (key_size < 0)
		badarg(Key);
	assert(key_size <= 65536);	// TODO: use heap_tmp_buf for a longer Key
	uint8_t key_buf[key_size];
	iolist_flatten(Key, key_buf);

	int data_size = iolist_size(Data);
	if (data_size < 0)
		badarg(Data);
	assert(data_size <= 65536);	// TODO: use heap_tmp_buf for larger Data
	uint8_t data_buf[data_size];
	iolist_flatten(Data, data_buf);

	struct hmac_sha1_ctx ctx;
	hmac_sha1_set_key(&ctx, key_size, key_buf);
	hmac_sha1_update(&ctx, data_size, data_buf);

	uint8_t *ptr;
	term_t mac = heap_make_bin(&proc->hp, trunc_size, &ptr);
	hmac_sha1_digest(&ctx, trunc_size, ptr);

	return mac;
}

term_t cbif_sha224_mac_n3(proc_t *proc, term_t *regs)
{
	//TODO
	printk("sha224_mac_n(%pt, %pt, %pt) called\n", T(regs[0]), T(regs[1]), T(regs[2]));
	bif_not_implemented();
}

term_t cbif_sha256_mac_n3(proc_t *proc, term_t *regs)
{
	//TODO
	printk("sha256_mac_n(%pt, %pt, %pt) called\n", T(regs[0]), T(regs[1]), T(regs[2]));
	bif_not_implemented();
}

term_t cbif_sha384_mac_n3(proc_t *proc, term_t *regs)
{
	//TODO
	printk("sha384_mac_n(%pt, %pt, %pt) called\n", T(regs[0]), T(regs[1]), T(regs[2]));
	bif_not_implemented();
}

term_t cbif_sha512_mac_n3(proc_t *proc, term_t *regs)
{
	//TODO
	printk("sha512_mac_n(%pt, %pt, %pt) called\n", T(regs[0]), T(regs[1]), T(regs[2]));
	bif_not_implemented();
}

term_t cbif_aes_cbc_crypt4(proc_t *proc, term_t *regs)
{
	term_t Key = regs[0];
	term_t IVec = regs[1];
	term_t Data = regs[2];
	term_t Dir = regs[3];

	if (!is_list(Key) && !is_boxed_binary(Key))
		badarg(Key);
	if (!is_boxed_binary(IVec))
		badarg(IVec);
	if (!is_list(Data) && !is_boxed_binary(Data))
		badarg(Data);
	if (!is_bool(Dir))
		badarg(Dir);

	int key_size = iolist_size(Key);
	if (key_size < AES_MIN_KEY_SIZE || key_size > AES_MAX_KEY_SIZE)
		badarg(Key);
	uint8_t key_buf[key_size];
	iolist_flatten(Key, key_buf);

	bits_t src, dst;
	bits_get_real(peel_boxed(IVec), &src);
	if (src.ends -src.starts != AES_BLOCK_SIZE *8)
		badarg(IVec);
	uint8_t ivec_buf[AES_BLOCK_SIZE];
	bits_init_buf(ivec_buf, AES_BLOCK_SIZE, &dst);
	bits_copy(&src, &dst);

	int data_size = iolist_size(Data);
	if (data_size < 0)
		badarg(Data);
	assert(data_size <= 65536);		//TODO: use heap_tmp_buf for larger Data
	uint8_t data_buf[data_size];
	iolist_flatten(Data, data_buf);

	struct CBC_CTX(struct aes_ctx, AES_BLOCK_SIZE) ctx;

	if (Dir == A_TRUE)
		aes_set_encrypt_key((struct aes_ctx *)&ctx, key_size, key_buf);
	else
		aes_set_decrypt_key((struct aes_ctx *)&ctx, key_size, key_buf);

	CBC_SET_IV(&ctx, ivec_buf);

	uint8_t *ptr;
	term_t cipher_text = heap_make_bin(&proc->hp, data_size, &ptr);

	if (Dir == A_TRUE)
		CBC_ENCRYPT(&ctx, aes_encrypt, data_size, ptr, data_buf);
	else
		CBC_DECRYPT(&ctx, aes_decrypt, data_size, ptr, data_buf);

	return cipher_text;
}

term_t cbif_rand_bytes1(proc_t *proc, term_t *regs)
{
	term_t N = regs[0];
	if (!is_int(N) || int_value(N) < 0)
		badarg(N);
	int len = int_value(N);

	uint8_t *ptr;
	term_t bin = heap_make_bin(&proc->hp, len, &ptr);
	uint8_t *p = ptr;
	while (p <= ptr +len -4)
	{
		uint32_t rnd = mt_lrand();
		PUT_UINT_32(p, rnd);
		p += 4;
	}
	uint32_t last = mt_lrand();
	switch(ptr +len -p)
	{
	case 3:
		*p++ = (uint8_t)(last >> 16);
	case 2:
		*p++ = (uint8_t)(last >> 8);
	case 1:
		*p++ = (uint8_t)last;
	case 0:
		break;
	}

	return bin;
}

term_t cbif_exor2(proc_t *proc, term_t *regs)
{
	term_t Bin1 = regs[0];
	term_t Bin2 = regs[1];

	if (!is_list(Bin1) && !is_boxed_binary(Bin1))
		badarg(Bin1);
	if (!is_list(Bin2) && !is_boxed_binary(Bin2))
		badarg(Bin2);

	int sz1 = iolist_size(Bin1);
	if (sz1 < 0)
		badarg(Bin1);
	int sz2 = iolist_size(Bin2);
	if (sz2 != sz1)
		badarg(Bin2);
	assert(sz1 <= 65536);	//TODO: use heap_tmp_buf for larger binaries
	uint8_t data1[sz1];
	iolist_flatten(Bin1, data1);
	uint8_t data2[sz2];
	iolist_flatten(Bin2, data2);

	uint8_t *data3;
	term_t result = heap_make_bin(&proc->hp, sz1, &data3);
	for (int i = 0; i < sz1; i++)
		data3[i] = data1[i] ^ data2[i];

	return result;
}

//EOF
