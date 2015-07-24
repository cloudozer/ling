%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%% Purpose : Main Crypto API module.

-module(crypto).

-export([start/0, stop/0, info_lib/0, supports/0, version/0, bytes_to_integer/1]).
-export([hash/2, hash_init/1, hash_update/2, hash_final/1]).
-export([sign/4, verify/5]).
-export([generate_key/2, generate_key/3, compute_key/4]).
-export([hmac/3, hmac/4, hmac_init/2, hmac_update/2, hmac_final/1, hmac_final_n/2]).
-export([exor/2, strong_rand_bytes/1, mod_pow/3]).
-export([rand_bytes/1, rand_uniform/2]).
-export([block_encrypt/3, block_decrypt/3, block_encrypt/4, block_decrypt/4]).
-export([next_iv/2, next_iv/3]).
-export([stream_init/2, stream_init/3, stream_encrypt/2, stream_decrypt/2]).
-export([public_encrypt/4, private_decrypt/4]).
-export([private_encrypt/4, public_decrypt/4]).
%-export([dh_generate_parameters/2, dh_check/1]). %% Testing see

%% DEPRECATED
%% Replaced by hash_*
%-export([md4/1, md4_init/0, md4_update/2, md4_final/1]).
%-export([md5/1, md5_init/0, md5_update/2, md5_final/1]).
%-export([sha/1, sha_init/0, sha_update/2, sha_final/1]).
%-deprecated({md4, 1, next_major_release}).
%-deprecated({md5, 1, next_major_release}).
%-deprecated({sha, 1, next_major_release}).
%-deprecated({md4_init, 0, next_major_release}).
%-deprecated({md5_init, 0, next_major_release}).
%-deprecated({sha_init, 0, next_major_release}).
%-deprecated({md4_update, 2, next_major_release}).
%-deprecated({md5_update, 2, next_major_release}).
%-deprecated({sha_update, 2, next_major_release}).
%-deprecated({md4_final, 1, next_major_release}).
%-deprecated({md5_final, 1, next_major_release}).
%-deprecated({sha_final, 1, next_major_release}).

%% Replaced by hmac_*
-export([md5_mac/2, md5_mac_96/2, sha_mac/2, sha_mac/3, sha_mac_96/2]).
-deprecated({md5_mac, 2, next_major_release}).
-deprecated({md5_mac_96, 2, next_major_release}).
-deprecated({sha_mac, 2, next_major_release}).
-deprecated({sha_mac, 3, next_major_release}).
-deprecated({sha_mac_96, 2, next_major_release}).

%% Replaced by sign/verify
-export([dss_verify/3, dss_verify/4, rsa_verify/3, rsa_verify/4]).
-export([dss_sign/2, dss_sign/3, rsa_sign/2, rsa_sign/3]).
-deprecated({dss_verify, 3, next_major_release}).
-deprecated({dss_verify, 4, next_major_release}).
-deprecated({rsa_verify, 3, next_major_release}).
-deprecated({rsa_verify, 4, next_major_release}).
-deprecated({dss_sign, 2, next_major_release}).
-deprecated({dss_sign, 3, next_major_release}).
-deprecated({rsa_sign, 2, next_major_release}).
-deprecated({rsa_sign, 3, next_major_release}).

%% Replaced by generate_key
-export([dh_generate_key/1, dh_generate_key/2, dh_compute_key/3]).
-deprecated({dh_generate_key, 1, next_major_release}).
-deprecated({dh_generate_key, 2, next_major_release}).
-deprecated({dh_compute_key, 3, next_major_release}).

%% Replaced by mod_exp_prim and no longer needed
-export([mod_exp/3, mpint/1, erlint/1]).
-deprecated({mod_exp, 3, next_major_release}).
-deprecated({mpint, 1, next_major_release}).
-deprecated({erlint, 1, next_major_release}).

%% Replaced by block_*
-export([des_cbc_encrypt/3, des_cbc_decrypt/3, des_cbc_ivec/1]).
-export([des3_cbc_encrypt/5, des3_cbc_decrypt/5]).
-export([des_ecb_encrypt/2, des_ecb_decrypt/2]).
-export([des_ede3_cbc_encrypt/5, des_ede3_cbc_decrypt/5]).
-export([des_cfb_encrypt/3, des_cfb_decrypt/3, des_cfb_ivec/2]).
-export([des3_cfb_encrypt/5, des3_cfb_decrypt/5]).
-deprecated({des_cbc_encrypt, 3, next_major_release}).
-deprecated({des_cbc_decrypt, 3, next_major_release}).
-deprecated({des_cbc_ivec, 1, next_major_release}).
-deprecated({des3_cbc_encrypt, 5, next_major_release}).
-deprecated({des3_cbc_decrypt, 5, next_major_release}).
-deprecated({des_ecb_encrypt, 2, next_major_release}).
-deprecated({des_ecb_decrypt, 2, next_major_release}).
-deprecated({des_ede3_cbc_encrypt, 5, next_major_release}).
-deprecated({des_ede3_cbc_decrypt, 5, next_major_release}).
-deprecated({des_cfb_encrypt, 3, next_major_release}).
-deprecated({des_cfb_decrypt, 3, next_major_release}).
-deprecated({des_cfb_ivec, 2, next_major_release}).
-deprecated({des3_cfb_encrypt, 5, next_major_release}).
-deprecated({des3_cfb_decrypt, 5, next_major_release}).
-export([blowfish_ecb_encrypt/2, blowfish_ecb_decrypt/2]).
-export([blowfish_cbc_encrypt/3, blowfish_cbc_decrypt/3]).
-export([blowfish_cfb64_encrypt/3, blowfish_cfb64_decrypt/3]).
-export([blowfish_ofb64_encrypt/3]).
-deprecated({blowfish_ecb_encrypt, 2, next_major_release}).
-deprecated({blowfish_ecb_decrypt, 2, next_major_release}).
-deprecated({blowfish_cbc_encrypt, 3, next_major_release}).
-deprecated({blowfish_cbc_decrypt, 3, next_major_release}).
-deprecated({blowfish_cfb64_encrypt, 3, next_major_release}).
-deprecated({blowfish_cfb64_decrypt, 3, next_major_release}).
-deprecated({blowfish_ofb64_encrypt, 3, next_major_release}).
-export([aes_cfb_128_encrypt/3, aes_cfb_128_decrypt/3]).
-export([aes_cbc_128_encrypt/3, aes_cbc_128_decrypt/3]).
-export([aes_cbc_256_encrypt/3, aes_cbc_256_decrypt/3]).
-export([aes_cbc_ivec/1]).
-deprecated({aes_cfb_128_encrypt, 3, next_major_release}).
-deprecated({aes_cfb_128_decrypt, 3, next_major_release}).
-deprecated({aes_cbc_128_encrypt, 3, next_major_release}).
-deprecated({aes_cbc_128_decrypt, 3, next_major_release}).
-deprecated({aes_cbc_256_encrypt, 3, next_major_release}).
-deprecated({aes_cbc_256_decrypt, 3, next_major_release}).
-deprecated({aes_cbc_ivec, 1, next_major_release}).
-export([rc2_cbc_encrypt/3, rc2_cbc_decrypt/3]).
-export([rc2_40_cbc_encrypt/3, rc2_40_cbc_decrypt/3]).
-deprecated({rc2_cbc_encrypt, 3, next_major_release}).
-deprecated({rc2_cbc_decrypt, 3, next_major_release}).
%% allready replaced by above!
-deprecated({rc2_40_cbc_encrypt, 3, next_major_release}).
-deprecated({rc2_40_cbc_decrypt, 3, next_major_release}).

%% Replaced by stream_*
%-export([aes_ctr_stream_init/2, aes_ctr_stream_encrypt/2, aes_ctr_stream_decrypt/2]).
%-export([rc4_set_key/1, rc4_encrypt_with_state/2]).
%-deprecated({aes_ctr_stream_init, 2, next_major_release}).
%-deprecated({aes_ctr_stream_encrypt, 2, next_major_release}).
%-deprecated({aes_ctr_stream_decrypt, 2, next_major_release}).
%-deprecated({rc4_set_key, 1, next_major_release}).
%-deprecated({rc4_encrypt_with_state, 2, next_major_release}).

%% Not needed special case of stream_*
%-export([aes_ctr_encrypt/3, aes_ctr_decrypt/3, rc4_encrypt/2]).
%-deprecated({aes_ctr_encrypt, 3, next_major_release}).
%-deprecated({aes_ctr_decrypt, 3, next_major_release}).
%-deprecated({rc4_encrypt, 2, next_major_release}).

%% Replace by public/private_encrypt/decrypt
-export([rsa_public_encrypt/3, rsa_private_decrypt/3]).
-export([rsa_private_encrypt/3, rsa_public_decrypt/3]).
-deprecated({rsa_public_encrypt, 3, next_major_release}).
-deprecated({rsa_private_decrypt, 3, next_major_release}).
-deprecated({rsa_public_decrypt, 3, next_major_release}).
-deprecated({rsa_private_encrypt, 3, next_major_release}).

%% Replaced by crypto:module_info()
-export([info/0]).
-deprecated({info, 0, next_major_release}).

-include_lib("public_key/include/public_key.hrl").

-type mpint() :: binary().
-type rsa_digest_type() :: 'md5' | 'sha' | 'sha224' | 'sha256' | 'sha384' | 'sha512'.
-type dss_digest_type() :: 'none' | 'sha'.
%%-type ecdsa_digest_type() :: 'md5' | 'sha' | 'sha256' | 'sha384' | 'sha512'.
-type data_or_digest() :: binary() | {digest, binary()}.
-type crypto_integer() :: binary() | integer().
%%-type ec_named_curve() :: atom().
%%-type ec_point() :: crypto_integer().
%%-type ec_basis() :: {tpbasis, K :: non_neg_integer()} | {ppbasis, K1 :: non_neg_integer(), K2 :: non_neg_integer(), K3 :: non_neg_integer()} | onbasis.
%%-type ec_field() :: {prime_field, Prime :: integer()} | {characteristic_two_field, M :: integer(), Basis :: ec_basis()}.
%%-type ec_prime() :: {A :: crypto_integer(), B :: crypto_integer(), Seed :: binary() | none}.
%%-type ec_curve_spec() :: {Field :: ec_field(), Prime :: ec_prime(), Point :: crypto_integer(), Order :: integer(), CoFactor :: none | integer()}.
%%-type ec_curve() :: ec_named_curve() | ec_curve_spec().
%%-type ec_key() :: {Curve :: ec_curve(), PrivKey :: binary() | undefined, PubKey :: ec_point() | undefined}.

-define(CRYPTO_VSN, "3.0").
-define(CRYPTO_INFO_LIB, [<<"nettle">>,26,<<"nettle 2.6">>]).

-define(CRYPTO_MAX_BYTES, 20000).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
%% Crypto app version history:
%% (no version): Driver implementation
%% 2.0         : NIF implementation, requires OTP R14
version() -> ?CRYPTO_VSN.

start() ->
    application:start(crypto).

stop() ->
    application:stop(crypto).

supports()->
    [{hashs, [md4, md5, sha, ripe160, sha224, sha256, sha384, sha512]},
     {ciphers, [des_cbc, des_cfb,  des3_cbc, des3_cbf, des_ede3, blowfish_cbc,
		blowfish_cfb64, blowfish_ofb64, blowfish_ecb, aes_cbc128, aes_cfb128,
		aes_cbc256, rc2_cbc, aes_ctr, rc4
	       ]},
	 {public_keys, [rsa, dss, dh, srp]}
    ].

info_lib() -> ?CRYPTO_INFO_LIB.

-spec hash(_, iodata()) -> binary().

hash(Hash, Data0) ->
    Data = iolist_to_binary(Data0),
    MaxByts = max_bytes(),
    hash(Hash, Data, erlang:byte_size(Data), MaxByts, initial).

-spec hash_init('md5'|'md4'|'ripemd160'|
                'sha'|'sha224'|'sha256'|'sha384'|'sha512') -> any().

hash_init(md5)       -> {md5, crypto:md5_init()};
hash_init(md4)       -> {md4, crypto:md4_init()};
hash_init(sha)       -> {sha, crypto:sha_init()};
%hash_init(ripemd160) -> {ripemd160, ripemd160_init()};
hash_init(sha224)    -> {sha224, crypto:sha224_init()};
hash_init(sha256)    -> {sha256, crypto:sha256_init()};
hash_init(sha384)    -> {sha384, crypto:sha384_init()};
hash_init(sha512)    -> {sha512, crypto:sha512_init()}.

-spec hash_update(_, iodata()) -> any().

hash_update(State, Data0) ->
    Data = iolist_to_binary(Data0),
    MaxBytes = max_bytes(),
    hash_update(State, Data, erlang:byte_size(Data), MaxBytes).

-spec hash_final(_) -> binary().

hash_final({md5,Context})       -> crypto:md5_final(Context);
hash_final({md4,Context})       -> crypto:md4_final(Context);
hash_final({sha,Context})       -> crypto:sha_final(Context);
%hash_final({ripemd160,Context}) -> ripemd160_final(Context);
hash_final({sha224,Context})    -> crypto:sha224_final(Context);
hash_final({sha256,Context})    -> crypto:sha256_final(Context);
hash_final({sha384,Context})    -> crypto:sha384_final(Context);
hash_final({sha512,Context})    -> crypto:sha512_final(Context).


-spec hmac(_, iodata(), iodata()) -> binary().
-spec hmac(_, iodata(), iodata(), integer()) -> binary().
-spec hmac_init(atom(), iodata()) -> binary().
-spec hmac_update(binary(), iodata()) -> binary().
-spec hmac_final(binary()) -> binary().
-spec hmac_final_n(binary(), integer()) -> binary().

hmac(Type, Key, Data0) ->
    Data = iolist_to_binary(Data0),
    hmac(Type, Key, Data, undefined, erlang:byte_size(Data), max_bytes(), initial).
hmac(Type, Key, Data0, MacSize) ->
    Data = iolist_to_binary(Data0),
    hmac(Type, Key, Data, MacSize, erlang:byte_size(Data), max_bytes(), initial).

%% Ecrypt/decrypt %%%

-spec block_encrypt(des_cbc | des_cfb | des3_cbc | des3_cbf | des_ede3 | blowfish_cbc |
		    blowfish_cfb64 | aes_cbc128 | aes_cfb128 | aes_cbc256 | rc2_cbc,
		    Key::iodata(), Ivec::binary(), Data::iodata()) -> binary().

block_encrypt(des_cbc, Key, Ivec, Data) ->
    des_cbc_encrypt(Key, Ivec, Data);
block_encrypt(des_cfb, Key, Ivec, Data) ->
    des_cfb_encrypt(Key, Ivec, Data);
block_encrypt(des3_cbc, [Key1, Key2, Key3], Ivec, Data) ->
    des3_cbc_encrypt(Key1, Key2, Key3, Ivec, Data);
block_encrypt(des3_cbf, [Key1, Key2, Key3], Ivec, Data) ->
    des3_cfb_encrypt(Key1, Key2, Key3, Ivec, Data);
block_encrypt(des_ede3, [Key1, Key2, Key3], Ivec, Data) ->
    des_ede3_cbc_encrypt(Key1, Key2, Key3, Ivec, Data);
block_encrypt(blowfish_cbc, Key, Ivec, Data) ->
    blowfish_cbc_encrypt(Key, Ivec, Data);
block_encrypt(blowfish_cfb64, Key, Ivec, Data) ->
    blowfish_cfb64_encrypt(Key, Ivec, Data);
block_encrypt(blowfish_ofb64, Key, Ivec, Data) ->
    blowfish_ofb64_encrypt(Key, Ivec, Data);
block_encrypt(aes_cbc128, Key, Ivec, Data) ->
    aes_cbc_128_encrypt(Key, Ivec, Data);
block_encrypt(aes_cbc256, Key, Ivec, Data) ->
    aes_cbc_256_encrypt(Key, Ivec, Data);
block_encrypt(aes_cfb128, Key, Ivec, Data) ->
    aes_cfb_128_encrypt(Key, Ivec, Data);
block_encrypt(rc2_cbc, Key, Ivec, Data) ->
    rc2_cbc_encrypt(Key, Ivec, Data).

-spec block_decrypt(des_cbc | des_cfb | des3_cbc | des3_cbf | des_ede3 | blowfish_cbc |
	      blowfish_cfb64 | blowfish_ofb64  | aes_cbc128 | aes_cbc256 | aes_cfb128 | rc2_cbc,
	      Key::iodata(), Ivec::binary(), Data::iodata()) -> binary().

block_decrypt(des_cbc, Key, Ivec, Data) ->
    des_cbc_decrypt(Key, Ivec, Data);
block_decrypt(des_cfb, Key, Ivec, Data) ->
    des_cfb_decrypt(Key, Ivec, Data);
block_decrypt(des3_cbc, [Key1, Key2, Key3], Ivec, Data) ->
    des3_cbc_decrypt(Key1, Key2, Key3, Ivec, Data);
block_decrypt(des3_cbf, [Key1, Key2, Key3], Ivec, Data) ->
    des3_cfb_decrypt(Key1, Key2, Key3, Ivec, Data);
block_decrypt(des_ede3, [Key1, Key2, Key3], Ivec, Data) ->
    des_ede3_cbc_decrypt(Key1, Key2, Key3, Ivec, Data);
block_decrypt(blowfish_cbc, Key, Ivec, Data) ->
    blowfish_cbc_decrypt(Key, Ivec, Data);
block_decrypt(blowfish_cfb64, Key, Ivec, Data) ->
    blowfish_cfb64_decrypt(Key, Ivec, Data);
block_decrypt(blowfish_ofb64, Key, Ivec, Data) ->
    blowfish_ofb64_decrypt(Key, Ivec, Data);
block_decrypt(aes_cbc128, Key, Ivec, Data) ->
    aes_cbc_128_decrypt(Key, Ivec, Data);
block_decrypt(aes_cbc256, Key, Ivec, Data) ->
    aes_cbc_256_decrypt(Key, Ivec, Data);
block_decrypt(aes_cfb128, Key, Ivec, Data) ->
    aes_cfb_128_decrypt(Key, Ivec, Data);
block_decrypt(rc2_cbc, Key, Ivec, Data) ->
    rc2_cbc_decrypt(Key, Ivec, Data).

-spec block_encrypt(des_ecb | blowfish_ecb, Key::iodata(), Data::iodata()) -> binary().

block_encrypt(des_ecb, Key, Data) ->
    des_ecb_encrypt(Key, Data);
block_encrypt(blowfish_ecb, Key, Data) ->
    blowfish_ecb_encrypt(Key, Data).

-spec block_decrypt(des_ecb | blowfish_ecb, Key::iodata(), Data::iodata()) -> binary().

block_decrypt(des_ecb, Key, Data) ->
    des_ecb_decrypt(Key, Data);
block_decrypt(blowfish_ecb, Key, Data) ->
    blowfish_ecb_decrypt(Key, Data).

-spec next_iv(des_cbc | des3_cbc | aes_cbc, Data::iodata()) -> binary().

next_iv(des_cbc, Data) ->
    des_cbc_ivec(Data);
next_iv(des3_cbc, Data) ->
    des_cbc_ivec(Data);
next_iv(aes_cbc, Data) ->
    aes_cbc_ivec(Data).

-spec next_iv(des_cfb, Data::iodata(), Ivec::binary()) -> binary().

next_iv(des_cfb, Data, Ivec) ->
    des_cfb_ivec(Ivec, Data);
next_iv(Type, Data, _Ivec) ->
    next_iv(Type, Data).

stream_init(_Algo, _Key) ->
	erlang:error(not_implemented).

stream_init(aes_ctr, Key, Ivec) ->
	{aes_ctr,aes_ctr_stream_init(Key, Ivec)};
stream_init(_Algo, _Key, _Ivec) ->
	erlang:error(not_implemented).

stream_encrypt(State, Data0) ->
    Data = iolist_to_binary(Data0),
    MaxByts = max_bytes(),
    stream_crypt(fun do_stream_encrypt/2, State, Data, erlang:byte_size(Data), MaxByts, []).

stream_decrypt(State, Data0) ->
    Data = iolist_to_binary(Data0),
    MaxByts = max_bytes(),
    stream_crypt(fun do_stream_decrypt/2, State, Data, erlang:byte_size(Data), MaxByts, []).

%%
%%
%%
-spec rand_bytes(non_neg_integer()) -> binary().
-spec strong_rand_bytes(non_neg_integer()) -> binary().
-spec rand_uniform(crypto_integer(), crypto_integer()) ->
			  crypto_integer().

rand_bytes(N) ->
	crypto:rand_bytes(N).

%%
%% Strong random bytes read from /dev/random
%%
%strong_rand_bytes(N) when is_integer(N), N > 0 ->
%	case try_read_dev_random(N) of
%	{error,enoent} ->
%		'9p_mounter':add_mounts(diod, [{<<"/dev/random">>,<<"/dev/random">>}]),
%		case try_read_dev_random(N) of
%		{error,_} ->
%			erlang:error(low_enthropy);
%		{ok,Bin} ->
%			Bin
%		end;
%	{ok,Bin} ->
%		Bin
%	end;
%strong_rand_bytes(_) ->
%	erlang:error(badarg).
%
%try_read_dev_random(N) ->
%	case file:open("/dev/random", [raw,binary,read]) of
%	{ok,Fd} ->
%		Res = file:read(Fd, N),
%		file:close(Fd),
%		Res;
%	{error,_} =Error ->
%		Error
%	end.

strong_rand_bytes(N) ->
	crypto:rand_bytes(N). %%XXX

rand_uniform(_Lo, _Hi) ->
	erlang:error(not_implemented).

-spec mod_pow(binary()|integer(), binary()|integer(), binary()|integer()) -> binary() | error.
mod_pow(Base, Exponent, Prime) ->
	int_to_bin(mod_exp(Base, Exponent, Prime)).

verify(_Alg, _Type, _Data, _Signature, _Key) ->
	erlang:error(not_implemented).

sign(rsa, Type = sha, Data, [_E,N,D|_]) when is_binary(Data) ->
	%% see rsa_private_encrypt()
	ModLen = 256,	%% other sizes?

	Algo = #'AlgorithmNull'{algorithm = 'OTP-PUB-KEY':'id-sha1'()},
	DigestInfo =
	#'DigestInfoNull'{digestAlgorithm = Algo, digest = hash(Type, Data)},
	{ok,BinMesg} = 'OTP-PUB-KEY':encode('DigestInfoNull', DigestInfo),

	PadLen = ModLen - byte_size(BinMesg) - 3,
	Padding = lists:duplicate(PadLen, 255),

	PaddedMesg = list_to_binary([0,1,Padding,0,BinMesg]),
	MesgInt = bin_to_int(PaddedMesg),

	EncInt = mod_exp(MesgInt, D, N),
	<<EncInt:ModLen/unit:8>>;

sign(_Algo, _Type, _Data, _Key) ->
	erlang:error(not_implemented).

-spec public_encrypt(rsa, binary(), [binary()], rsa_padding()) ->
				binary().
-spec public_decrypt(rsa, binary(), [integer() | binary()], rsa_padding()) ->
				binary().
-spec private_encrypt(rsa, binary(), [integer() | binary()], rsa_padding()) ->
				binary().
-spec private_decrypt(rsa, binary(), [integer() | binary()], rsa_padding()) ->
				binary().

public_encrypt(rsa, BinMesg, Key, Padding) ->
    case rsa_public_encrypt(BinMesg,  map_ensure_int_as_bin(Key), Padding) of
	error ->
	    erlang:error(encrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

%% Binary, Key = [E,N,D]
private_decrypt(rsa, BinMesg, Key, Padding) ->
    case rsa_private_decrypt(BinMesg, map_ensure_int_as_bin(Key), Padding) of
	error ->
	    erlang:error(decrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.


%% Binary, Key = [E,N,D]
private_encrypt(rsa, BinMesg, Key, Padding) ->
    case rsa_private_encrypt(BinMesg, map_ensure_int_as_bin(Key), Padding) of
	error ->
	    erlang:error(encrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

%% Binary, Key = [E,N]
public_decrypt(rsa, BinMesg, Key, Padding) ->
    case rsa_public_decrypt(BinMesg, map_ensure_int_as_bin(Key), Padding) of
	error ->
	    erlang:error(decrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

%%
%% XOR - xor to iolists and return a binary
%% NB doesn't check that they are the same size, just concatenates
%% them and sends them to the driver
%%
-spec exor(iodata(), iodata()) -> binary().

exor(Bin1, Bin2) ->
    Data1 = iolist_to_binary(Bin1),
    Data2 = iolist_to_binary(Bin2),
    MaxBytes = max_bytes(),
    exor(Data1, Data2, erlang:byte_size(Data1), MaxBytes, []).

generate_key(Type, Params) ->
	generate_key(Type, Params, undefined).

generate_key(dh, [P,G], _) ->
	%% P and G are numbers; see dh_generate_key()
	PrivKey = <<PrivInt:128/unit:8>> = crypto:rand_bytes(128),
	SecInt = mod_exp(G, PrivInt, P),
	Secret = <<SecInt:128/unit:8>>,
	{Secret,PrivKey}.

compute_key(_Type, _PubKey, _PrivKey, _Params) -> throw(not_implemented).

%%--------------------------------------------------------------------
%%% Internal functions (some internal API functions are part of the deprecated API)
%%--------------------------------------------------------------------

%% HASH --------------------------------------------------------------------
hash(Hash, Data, Size, Max, initial) when Size =< Max ->
    do_hash(Hash, Data);
hash(State0, Data, Size, Max, continue) when Size =< Max ->
    State = do_hash_update(State0, Data),
    hash_final(State);
hash(Hash, Data, _Size, Max, initial) ->
    <<Increment:Max/binary, Rest/binary>> = Data,
    State0 = hash_init(Hash),
    State = do_hash_update(State0, Increment),
    hash(State, Rest, erlang:byte_size(Rest), max_bytes(), continue);
hash(State0, Data, _Size, MaxByts, continue) ->
    <<Increment:MaxByts/binary, Rest/binary>> = Data,
    State = do_hash_update(State0, Increment),
    hash(State, Rest, erlang:byte_size(Rest), max_bytes(), continue).

do_hash(md5, Data)          -> crypto:md5(Data);
do_hash(md4, Data)          -> crypto:md4(Data);
do_hash(sha, Data)          -> crypto:sha(Data);
%do_hash(ripemd160, Data)    -> ripemd160(Data);
do_hash(sha224, Data)       -> crypto:sha224(Data);
do_hash(sha256, Data)       -> crypto:sha256(Data);
do_hash(sha384, Data)       -> crypto:sha384(Data);
do_hash(sha512, Data)       -> crypto:sha512(Data).

hash_update(State, Data, Size, MaxBytes)  when Size =< MaxBytes ->
    do_hash_update(State, Data);
hash_update(State0, Data, _, MaxBytes) ->
    <<Increment:MaxBytes/binary, Rest/binary>> = Data,
    State = do_hash_update(State0, Increment),
    hash_update(State, Rest, erlang:byte_size(Rest), MaxBytes).

do_hash_update({md5,Context}, Data)       -> {md5, crypto:md5_update(Context,Data)};
do_hash_update({md4,Context}, Data)       -> {md4, crypto:md4_update(Context,Data)};
do_hash_update({sha,Context}, Data)       -> {sha, crypto:sha_update(Context,Data)};
%do_hash_update({ripemd160,Context}, Data) -> {ripemd160, ripemd160_update(Context,Data)};
do_hash_update({sha224,Context}, Data)    -> {sha224, crypto:sha224_update(Context,Data)};
do_hash_update({sha256,Context}, Data)    -> {sha256, crypto:sha256_update(Context,Data)};
do_hash_update({sha384,Context}, Data)    -> {sha384, crypto:sha384_update(Context,Data)};
do_hash_update({sha512,Context}, Data)    -> {sha512, crypto:sha512_update(Context,Data)}.

%% HMAC --------------------------------------

hmac(Type, Key, Data, MacSize, Size, MaxBytes, initial) when Size =< MaxBytes ->
    case MacSize of
	undefined ->
	    do_hmac(Type, Key, Data);
	_ ->
	    do_hmac(Type, Key, Data, MacSize)
    end;
hmac(Type, Key, Data, MacSize, _, MaxBytes, initial) ->
    <<Increment:MaxBytes/binary, Rest/binary>> = Data,
    State0 = hmac_init(Type, Key),
    State = hmac_update(State0, Increment),
    hmac(State, Rest, MacSize, erlang:byte_size(Rest), max_bytes(), continue).
hmac(State0, Data, MacSize, Size, MaxBytes, continue) when Size =< MaxBytes ->
    State = hmac_update(State0, Data),
    case MacSize of
	undefined ->
	    hmac_final(State);
	 _ ->
	    hmac_final_n(State, MacSize)
	end;
hmac(State0, Data, MacSize, _Size, MaxBytes, continue) ->
    <<Increment:MaxBytes/binary, Rest/binary>> = Data,
    State = hmac_update(State0, Increment),
    hmac(State, Rest, MacSize, erlang:byte_size(Rest), max_bytes(), continue).

hmac_init(_Type, _Key) ->
	erlang:error(not_implemented).

hmac_update(State, Data0) ->
    Data = iolist_to_binary(Data0),
    hmac_update(State, Data, erlang:byte_size(Data), max_bytes()).

hmac_update(State, Data, Size, MaxBytes)  when Size =< MaxBytes ->
    do_hmac_update(State, Data);
hmac_update(State0, Data, _, MaxBytes) ->
    <<Increment:MaxBytes/binary, Rest/binary>> = Data,
    State = do_hmac_update(State0, Increment),
    hmac_update(State, Rest, erlang:byte_size(Rest), MaxBytes).

hmac_final(_Context) ->
	erlang:error(not_implemented).
hmac_final_n(_Context, _HashLen) ->
	erlang:error(not_implemented).

do_hmac(md5, Key, Data)    -> md5_mac(Key, Data);
do_hmac(sha, Key, Data)    -> sha_mac(Key, Data);
do_hmac(sha224, Key, Data) -> sha224_mac(Key, Data);
do_hmac(sha256, Key, Data) -> sha256_mac(Key, Data);
do_hmac(sha384, Key, Data) -> sha384_mac(Key, Data);
do_hmac(sha512, Key, Data) -> sha512_mac(Key, Data).

do_hmac(md5, Key, Data, Size)    -> crypto:md5_mac_n(Key, Data, Size);
do_hmac(sha, Key, Data, Size)    -> crypto:sha_mac_n(Key, Data, Size);
do_hmac(sha224, Key, Data, Size) -> sha224_mac(Key, Data, Size);
do_hmac(sha256, Key, Data, Size) -> sha256_mac(Key, Data, Size);
do_hmac(sha384, Key, Data, Size) -> sha384_mac(Key, Data, Size);
do_hmac(sha512, Key, Data, Size) -> sha512_mac(Key, Data, Size).

do_hmac_update(_State, _Data) ->
	erlang:error(not_implemented).

%%
%%  MD5_MAC
%%
-spec md5_mac(iodata(), iodata()) -> binary().
-spec md5_mac_96(iodata(), iodata()) -> binary().

md5_mac(Key, Data) ->
    crypto:md5_mac_n(Key,Data,16).

md5_mac_96(Key, Data) ->
    crypto:md5_mac_n(Key,Data,12).
    
%%
%%  SHA_MAC
%%
-spec sha_mac(iodata(), iodata()) -> binary().
-spec sha_mac_96(iodata(), iodata()) -> binary().

sha_mac(Key, Data) ->
    crypto:sha_mac_n(Key,Data,20).

sha_mac(Key, Data, Size) ->
	crypto:sha_mac_n(Key, Data, Size).

sha_mac_96(Key, Data) ->
    crypto:sha_mac_n(Key,Data,12).

%%
%%  SHA224_MAC
%%
-spec sha224_mac(iodata(), iodata()) -> binary().

sha224_mac(Key, Data) ->
    crypto:sha224_mac_n(Key, Data, 224 div 8).

sha224_mac(Key, Data, Size) ->
	crypto:sha224_mac_n(Key, Data, Size).

%%
%%  SHA256_MAC
%%
-spec sha256_mac(iodata(), iodata()) -> binary().

sha256_mac(Key, Data) ->
    crypto:sha256_mac_n(Key, Data, 256 div 8).

sha256_mac(Key, Data, Size) ->
   crypto:sha256_mac_n(Key, Data, Size).

%%
%%  SHA384_MAC
%%
-spec sha384_mac(iodata(), iodata()) -> binary().

sha384_mac(Key, Data) ->
    crypto:sha384_mac_n(Key, Data, 384 div 8).

sha384_mac(Key, Data, Size) ->
	crypto:sha384_mac_n(Key, Data, Size).

%%
%%  SHA512_MAC
%%
-spec sha512_mac(iodata(), iodata()) -> binary().

sha512_mac(Key, Data) ->
    crypto:sha512_mac_n(Key, Data, 512 div 8).

sha512_mac(Key, Data, MacSz) ->
	crypto:sha512_mac_n(Key, Data, MacSz).

%% CIPHERS --------------------------------------------------------------------

%%
%% DES - in electronic codebook mode (ECB)
%%
-spec des_ecb_encrypt(iodata(), iodata()) -> binary().
-spec des_ecb_decrypt(iodata(), iodata()) -> binary().

des_ecb_encrypt(Key, Data) ->
    crypto:des_ecb_crypt(Key, Data, true).
des_ecb_decrypt(Key, Data) ->
    crypto:des_ecb_crypt(Key, Data, false).

%%
%% DES3 - in cipher block chaining mode (CBC)
%%
-spec des3_cbc_encrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().
-spec des3_cbc_decrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().

des3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    crypto:des_ede3_cbc_crypt(Key1, Key2, Key3, IVec, Data, true).
des_ede3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    crypto:des_ede3_cbc_crypt(Key1, Key2, Key3, IVec, Data, true).

des3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    crypto:des_ede3_cbc_crypt(Key1, Key2, Key3, IVec, Data, false).
des_ede3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    crypto:des_ede3_cbc_crypt(Key1, Key2, Key3, IVec, Data, false).

%%
%% DES3 - in 8-bits cipher feedback mode (CFB)
%%
-spec des3_cfb_encrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().
-spec des3_cfb_decrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().

des3_cfb_encrypt(Key1, Key2, Key3, IVec, Data) ->
    crypto:des_ede3_cfb_crypt(Key1, Key2, Key3, IVec, Data, true).

des3_cfb_decrypt(Key1, Key2, Key3, IVec, Data) ->
    crypto:des_ede3_cfb_crypt(Key1, Key2, Key3, IVec, Data, false).

%%
%% Blowfish
%%
-spec blowfish_ecb_encrypt(iodata(), iodata()) -> binary().
-spec blowfish_ecb_decrypt(iodata(), iodata()) -> binary().
-spec blowfish_cbc_encrypt(iodata(), binary(), iodata()) -> binary().
-spec blowfish_cbc_decrypt(iodata(), binary(), iodata()) -> binary().
-spec blowfish_cfb64_encrypt(iodata(), binary(), iodata()) -> binary().
-spec blowfish_cfb64_decrypt(iodata(), binary(), iodata()) -> binary().
-spec blowfish_ofb64_encrypt(iodata(), binary(), iodata()) -> binary().

blowfish_ecb_encrypt(Key, Data) ->
    crypto:bf_ecb_crypt(Key,Data, true).

blowfish_ecb_decrypt(Key, Data) ->
    crypto:bf_ecb_crypt(Key,Data, false).

blowfish_cbc_encrypt(Key, IVec, Data) ->
    crypto:bf_cbc_crypt(Key,IVec,Data,true).

blowfish_cbc_decrypt(Key, IVec, Data) ->
    crypto:bf_cbc_crypt(Key,IVec,Data,false).

blowfish_cfb64_encrypt(Key, IVec, Data) ->
    crypto:bf_cfb64_crypt(Key, IVec, Data, true).

blowfish_cfb64_decrypt(Key, IVec, Data) ->
    crypto:bf_cfb64_crypt(Key, IVec, Data, false).

blowfish_ofb64_decrypt(_Key, _Ivec, _Data) ->
	erlang:error(not_implemented).

blowfish_ofb64_encrypt(_Key, _IVec, _Data) ->
	erlang:error(not_implemented).

%%
%% AES in cipher feedback mode (CFB)
%%
-spec aes_cfb_128_encrypt(iodata(), binary(), iodata()) -> binary().
-spec aes_cfb_128_decrypt(iodata(), binary(), iodata()) -> binary().

aes_cfb_128_encrypt(Key, IVec, Data) ->
    crypto:aes_cfb_128_crypt(Key, IVec, Data, true).

aes_cfb_128_decrypt(Key, IVec, Data) ->
    crypto:aes_cfb_128_crypt(Key, IVec, Data, false).

%%
%% DES - in cipher block chaining mode (CBC)
%%
-spec des_cbc_encrypt(iodata(), binary(), iodata()) -> binary().
-spec des_cbc_decrypt(iodata(), binary(), iodata()) -> binary().

des_cbc_encrypt(Key, IVec, Data) ->
    crypto:des_cbc_crypt(Key, IVec, Data, true).

des_cbc_decrypt(Key, IVec, Data) ->
    crypto:des_cbc_crypt(Key, IVec, Data, false).

%%
%% dec_cbc_ivec(Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of
%% des_cbc_[encrypt|decrypt].
%%
-spec des_cbc_ivec(iodata()) -> binary().

des_cbc_ivec(Data) when is_binary(Data) ->
    {_, IVec} = split_binary(Data, size(Data) - 8),
    IVec;
des_cbc_ivec(Data) when is_list(Data) ->
    des_cbc_ivec(list_to_binary(Data)).

%%
%% DES - in 8-bits cipher feedback mode (CFB)
%%
-spec des_cfb_encrypt(iodata(), binary(), iodata()) -> binary().
-spec des_cfb_decrypt(iodata(), binary(), iodata()) -> binary().

des_cfb_encrypt(Key, IVec, Data) ->
    crypto:des_cfb_crypt(Key, IVec, Data, true).

des_cfb_decrypt(Key, IVec, Data) ->
    crypto:des_cfb_crypt(Key, IVec, Data, false).

%%
%% dec_cfb_ivec(IVec, Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of
%% des_cfb_[encrypt|decrypt].
%%

-spec des_cfb_ivec(iodata(), iodata()) -> binary().

des_cfb_ivec(IVec, Data) ->
    IVecAndData = list_to_binary([IVec, Data]),
    {_, NewIVec} = split_binary(IVecAndData, byte_size(IVecAndData) - 8),
    NewIVec.

%%
%% AES - with 128 or 256 bit key in cipher block chaining mode (CBC)
%%
-spec aes_cbc_128_encrypt(iodata(), binary(), iodata()) ->
				 binary().
-spec aes_cbc_128_decrypt(iodata(), binary(), iodata()) ->
				 binary().
-spec aes_cbc_256_encrypt(iodata(), binary(), iodata()) ->
				 binary().
-spec aes_cbc_256_decrypt(iodata(), binary(), iodata()) ->
				 binary().

aes_cbc_128_encrypt(Key, IVec, Data) ->
    crypto:aes_cbc_crypt(Key, IVec, Data, true).

aes_cbc_128_decrypt(Key, IVec, Data) ->
    crypto:aes_cbc_crypt(Key, IVec, Data, false).

aes_cbc_256_encrypt(Key, IVec, Data) ->
    crypto:aes_cbc_crypt(Key, IVec, Data, true).

aes_cbc_256_decrypt(Key, IVec, Data) ->
    crypto:aes_cbc_crypt(Key, IVec, Data, false).

%%
%% aes_cbc_ivec(Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of
%% aes_cbc_*_[encrypt|decrypt].
%% IVec size: 16 bytes
%%
aes_cbc_ivec(Data) when is_binary(Data) ->
    {_, IVec} = split_binary(Data, size(Data) - 16),
    IVec;
aes_cbc_ivec(Data) when is_list(Data) ->
    aes_cbc_ivec(list_to_binary(Data)).


%% Stream ciphers --------------------------------------------------------------------

stream_crypt(Fun, State, Data, Size, MaxByts, []) when Size =< MaxByts ->
    Fun(State, Data);
stream_crypt(Fun, State0, Data, Size, MaxByts, Acc) when Size =< MaxByts ->
    {State, Cipher} = Fun(State0, Data),
    {State, list_to_binary(lists:reverse([Cipher | Acc]))};
stream_crypt(Fun, State0, Data, _, MaxByts, Acc) ->
    <<Increment:MaxByts/binary, Rest/binary>> = Data,
    {State, CipherText} = Fun(State0, Increment),
    stream_crypt(Fun, State, Rest, erlang:byte_size(Rest), MaxByts, [CipherText | Acc]).

do_stream_encrypt({aes_ctr, State0}, Data) ->
    {State, Cipher} = aes_ctr_stream_encrypt(State0, Data),
    {{aes_ctr, State}, Cipher};
do_stream_encrypt({rc4, State0}, Data) ->
    {State, Cipher} = rc4_encrypt_with_state(State0, Data),
    {{rc4, State}, Cipher}.

do_stream_decrypt({aes_ctr, State0}, Data) ->
    {State, Text} = aes_ctr_stream_decrypt(State0, Data),
    {{aes_ctr, State}, Text};
do_stream_decrypt({rc4, State0}, Data) ->
    {State, Text} = rc4_encrypt_with_state(State0, Data),
    {{rc4, State}, Text}.

%%
%% AES - in counter mode (CTR) with state maintained for multi-call streaming 
%%
-type ctr_state() :: { iodata(), binary() }.

-spec aes_ctr_stream_init(iodata(), binary()) -> ctr_state().
-spec aes_ctr_stream_encrypt(ctr_state(), binary()) ->
				 { ctr_state(), binary() }.
-spec aes_ctr_stream_decrypt(ctr_state(), binary()) ->
				 { ctr_state(), binary() }.
 
aes_ctr_stream_init(Key, IVec) -> {Key,IVec}.
aes_ctr_stream_encrypt({Key,IVec}, Data) ->
	crypto:aes_ctr_stream_crypt(Key, IVec, Data).
aes_ctr_stream_decrypt({Key,IVec}, Cipher) ->
	crypto:aes_ctr_stream_crypt(Key, IVec, Cipher).	%% same
     
%%
%% RC4 - symmetric stream cipher
%%
%%-spec rc4_encrypt(iodata(), iodata()) -> binary().

%%rc4_encrypt(_Key, _Data) -> 				erlang:error(not_implemented);
%%rc4_set_key(_Key) ->						erlang:error(not_implemented);
rc4_encrypt_with_state(_State, _Data) ->	erlang:error(not_implemented).

%% RC2 block cipher

rc2_cbc_encrypt(Key, IVec, Data) ->
    crypto:rc2_cbc_crypt(Key,IVec,Data,true).

rc2_cbc_decrypt(Key, IVec, Data) ->
    crypto:rc2_cbc_crypt(Key,IVec,Data,false).

%%
%% RC2 - 40 bits block cipher - Backwards compatibility not documented.
%%
rc2_40_cbc_encrypt(Key, IVec, Data) when erlang:byte_size(Key) == 5 ->
    crypto:rc2_cbc_crypt(Key,IVec,Data,true).

rc2_40_cbc_decrypt(Key, IVec, Data)  when erlang:byte_size(Key) == 5 ->
    crypto:rc2_cbc_crypt(Key,IVec,Data,false).

%% Public Keys  --------------------------------------------------------------------
%% DH Diffie-Hellman functions
%% 

%% Generate (and check) Parameters is not documented because they are implemented
%% for testing (and offline parameter generation) only.
%%

%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
%% PrivKey = mpint()
-spec dh_generate_key([binary()]) -> {binary(),binary()}.
-spec dh_generate_key(binary()|undefined, [binary()]) ->
			     {binary(),binary()}.

dh_generate_key([P,G]) ->
	<<Sz:32,_/binary>> = P,
	KeySize = Sz -1,
	PrivKey = <<KeySize:32,(crypto:rand_bytes(KeySize))/binary>>,
	SecInt = mod_exp(erlint(G), erlint(PrivKey), erlint(P)),
	Secret = <<KeySize:32,SecInt:KeySize/unit:8>>,
	{Secret,PrivKey}.

dh_generate_key(PrivKey, [P,G]) ->
	<<KeySize:32,_/binary>> = PrivKey,
	SecInt = mod_exp(erlint(G), erlint(PrivKey), erlint(P)),
	Secret = <<KeySize:32,SecInt:KeySize/unit:8>>,
	{Secret,PrivKey}.

%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
%% MyPrivKey, OthersPublicKey = mpint()
-spec dh_compute_key(binary(), binary(), [binary()]) -> binary().

dh_compute_key(PubKey, PrivKey, [P,_G]) ->
	<<_:32,Secret/binary>> = mod_exp(PubKey, PrivKey, P),
	Secret.

%% MISC --------------------------------------------------------------------

exor(Data1, Data2, Size, MaxByts, [])  when Size =< MaxByts ->
    crypto:exor(Data1, Data2);
exor(Data1, Data2, Size, MaxByts, Acc) when Size =< MaxByts ->
    Result = crypto:exor(Data1, Data2),
    list_to_binary(lists:reverse([Result | Acc]));
exor(Data1, Data2, _Size, MaxByts, Acc) ->
     <<Increment1:MaxByts/binary, Rest1/binary>> = Data1,
     <<Increment2:MaxByts/binary, Rest2/binary>> = Data2,
    Result = crypto:exor(Increment1, Increment2),
    exor(Rest1, Rest2, erlang:byte_size(Rest1), MaxByts, [Result | Acc]).

int_to_bin(X) when X < 0 -> int_to_bin_neg(X, []);
int_to_bin(X) -> int_to_bin_pos(X, []).

int_to_bin_pos(0,Ds=[_|_]) ->
    list_to_binary(Ds);
int_to_bin_pos(X,Ds) ->
    int_to_bin_pos(X bsr 8, [(X band 255)|Ds]).

int_to_bin_neg(-1, Ds=[MSB|_]) when MSB >= 16#80 ->
    list_to_binary(Ds);
int_to_bin_neg(X,Ds) ->
    int_to_bin_neg(X bsr 8, [(X band 255)|Ds]).

bytes_to_integer(Bin) ->
    bin_to_int(Bin).

bin_to_int(Bin) when is_binary(Bin) ->
    Bits = bit_size(Bin),
    <<Integer:Bits/integer>> = Bin,
    Integer;
bin_to_int(undefined) ->
    undefined.

map_ensure_int_as_bin([H|_]=List) when is_integer(H) ->
    lists:map(fun(E) -> int_to_bin(E) end, List);
map_ensure_int_as_bin(List) ->
    List.

ensure_int_as_bin(Int) when is_integer(Int) ->
    int_to_bin(Int);
ensure_int_as_bin(Bin) ->
    Bin.

map_to_norm_bin([H|_]=List) when is_integer(H) ->
    lists:map(fun(E) -> int_to_bin(E) end, List);
map_to_norm_bin(List) ->
    lists:map(fun(E) -> mpint_to_bin(E) end, List).

%%--------------------------------------------------------------------
%%% Deprecated
%%--------------------------------------------------------------------
%%
%%  rsa_public_encrypt
%%  rsa_private_decrypt
-type rsa_padding() :: 'rsa_pkcs1_padding' | 'rsa_pkcs1_oaep_padding' | 'rsa_no_padding'.

-spec rsa_public_encrypt(binary(), [binary()], rsa_padding()) ->
				binary().
-spec rsa_public_decrypt(binary(), [integer() | mpint()], rsa_padding()) ->
				binary().
-spec rsa_private_encrypt(binary(), [integer() | mpint()], rsa_padding()) ->
				binary().
-spec rsa_private_decrypt(binary(), [integer() | mpint()], rsa_padding()) ->
				binary().

%% Binary, Key = [E,N]
rsa_public_encrypt(_BinMesg, _Key, _Padding) ->
	erlang:error(not_implemented).

%% Binary, Key = [E,N,D]
rsa_private_decrypt(_BinMesg, _Key, _Padding) ->
	erlang:error(not_implemented).

%% Binary, Key = [E,N,D]
rsa_private_encrypt(BinMesg, [_PubExp,Modulo,PrivExp|_], rsa_pkcs1_padding) ->
	<<Sz:32,_/binary>> = Modulo,
	ModLen = Sz -1,
	true = byte_size(BinMesg) =< ModLen -11,

	PadLen = ModLen -byte_size(BinMesg) -3,
	Padding = lists:duplicate(PadLen, 255),

	PaddedMesg = list_to_binary([0,1,Padding,0,BinMesg]),
	BitSz = bit_size(PaddedMesg),
	<<MesgInt:BitSz>> = PaddedMesg,

	EncInt = mod_exp(MesgInt, erlint(PrivExp), erlint(Modulo)),
	<<EncInt:ModLen/unit:8>>.

%% Binary, Key = [E,N]
rsa_public_decrypt(_BinMesg, _Key, _Padding) ->
	erlang:error(not_implemented).

map_mpint_to_bin(List) ->
    lists:map(fun(E) -> mpint_to_bin(E) end, List ).

%%
%% DSS, RSA - sign
%%
%% Key = [P,Q,G,X]   P,Q,G=DSSParams  X=PrivateKey
-spec dss_sign(data_or_digest(), [binary()]) -> binary().
-spec dss_sign(dss_digest_type(), data_or_digest(), [binary()]) -> binary().
-spec rsa_sign(data_or_digest(), [binary()]) -> binary().
-spec rsa_sign(rsa_digest_type(), data_or_digest(), [binary()]) -> binary().

dss_sign(DataOrDigest,Key) ->
    crypto:dss_sign(sha,DataOrDigest,Key).
dss_sign(Type, Data, Key) when is_binary(Data), Type=/=none ->
    sign(dss, Type, mpint_to_bin(Data), map_mpint_to_bin(Key));
dss_sign(Type, Digest, Key) ->
    sign(dss, Type, Digest, map_mpint_to_bin(Key)).


%% Key = [E,N,D]  E=PublicExponent N=PublicModulus  D=PrivateExponent
rsa_sign(DataOrDigest,Key) ->
    crypto:rsa_sign(sha, DataOrDigest, Key).

rsa_sign(Type, Data, Key) when is_binary(Data) ->
    sign(rsa, Type, mpint_to_bin(Data), map_mpint_to_bin(Key));
rsa_sign(Type, Digest, Key) ->
    sign(rsa, Type, Digest, map_mpint_to_bin(Key)).

%%
%% DSS, RSA - verify
%%
-spec dss_verify(data_or_digest(), binary(), [binary()]) -> boolean().
-spec dss_verify(dss_digest_type(), data_or_digest(), binary(), [binary()]) -> boolean().
-spec rsa_verify(data_or_digest(), binary(), [binary()]) -> boolean().
-spec rsa_verify(rsa_digest_type(), data_or_digest(), binary(), [binary()]) ->
			boolean().

%% Key = [P,Q,G,Y]   P,Q,G=DSSParams  Y=PublicKey
dss_verify(Data,Signature,Key) ->
    crypto:dss_verify(sha, Data, Signature, Key).

dss_verify(Type,Data,Signature,Key) when is_binary(Data), Type=/=none ->
    verify(dss,Type,mpint_to_bin(Data),mpint_to_bin(Signature),map_mpint_to_bin(Key));
dss_verify(Type,Digest,Signature,Key) ->
    verify(dss,Type,Digest,mpint_to_bin(Signature),map_mpint_to_bin(Key)).

% Key = [E,N]  E=PublicExponent N=PublicModulus
rsa_verify(Data,Signature,Key) ->
    crypto:rsa_verify(sha, Data,Signature,Key).
rsa_verify(Type, Data, Signature, Key) when is_binary(Data) ->
    verify(rsa, Type, mpint_to_bin(Data), mpint_to_bin(Signature), map_mpint_to_bin(Key));
rsa_verify(Type, Digest, Signature, Key) ->
    verify(rsa, Type, Digest, mpint_to_bin(Signature), map_mpint_to_bin(Key)).

%% large integer in a binary with 32bit length
%% MP representaion  (SSH2)
mpint(X) when X < 0 -> mpint_neg(X);
mpint(X) -> mpint_pos(X).

-define(UINT32(X),   X:32/unsigned-big-integer).


mpint_neg(X) ->
    Bin = int_to_bin_neg(X, []),
    Sz = byte_size(Bin),
    <<?UINT32(Sz), Bin/binary>>.
    
mpint_pos(X) ->
    Bin = int_to_bin_pos(X, []),
    <<MSB,_/binary>> = Bin,
    Sz = byte_size(Bin),
    if MSB band 16#80 == 16#80 ->
	    <<?UINT32((Sz+1)), 0, Bin/binary>>;
       true ->
	    <<?UINT32(Sz), Bin/binary>>
    end.

%% int from integer in a binary with 32bit length
erlint(<<MPIntSize:32/integer,MPIntValue/binary>>) ->
    Bits= MPIntSize * 8,
    <<Integer:Bits/integer>> = MPIntValue,
    Integer.

mpint_to_bin(<<Len:32, Bin:Len/binary>>) ->
    Bin.

%%
%% mod_exp - utility for rsa generation
%%
mod_exp(Base, Exponent, Modulo)
		when is_binary(Base), is_binary(Exponent), is_binary(Modulo) ->
	<<Sz:32,_/binary>> = Modulo,
    Integer = mod_exp(erlint(Base), erlint(Exponent), erlint(Modulo)),
    Size = Sz -1,
    <<Size:32,Integer:(Size)/unit:8>>;

mod_exp(Base, 1, Modulo) ->
	Base rem Modulo;
mod_exp(Base, 2, Modulo) ->
	Base*Base rem Modulo;
mod_exp(Base, Exponent, Modulo) ->
	Exp1 = Exponent div 2,
	Exp2 = Exponent -Exp1,
	%% Exp2 = Exp1 or Exp1 +1
	X = mod_exp(Base, Exp1, Modulo),
	case Exp2 of
	Exp1 -> (X*X) rem Modulo;
	_ -> (X*X*Base) rem Modulo
	end.

-define(FUNC_LIST, [hash, hash_init, hash_update, hash_final,
		    hmac, hmac_init, hmac_update, hmac_final, hmac_final_n,
		    %% deprecated
		    md4, md4_init, md4_update, md4_final,
		    md5, md5_init, md5_update, md5_final,
		    sha, sha_init, sha_update, sha_final,
		    md5_mac,  md5_mac_96,
		    sha_mac,  sha_mac_96,
		    %%
		    block_encrypt, block_decrypt,
		    %% deprecated
		    des_cbc_encrypt, des_cbc_decrypt,
		    des_cfb_encrypt, des_cfb_decrypt,
		    des_ecb_encrypt, des_ecb_decrypt,
		    des3_cbc_encrypt, des3_cbc_decrypt,
		    des3_cfb_encrypt, des3_cfb_decrypt,
		    aes_cfb_128_encrypt, aes_cfb_128_decrypt,
		    rc2_cbc_encrypt, rc2_cbc_decrypt,
		    rc2_40_cbc_encrypt, rc2_40_cbc_decrypt,
		    aes_cbc_128_encrypt, aes_cbc_128_decrypt,
		    aes_cbc_256_encrypt, aes_cbc_256_decrypt,
		    blowfish_cbc_encrypt, blowfish_cbc_decrypt,
		    blowfish_cfb64_encrypt, blowfish_cfb64_decrypt,
		    blowfish_ecb_encrypt, blowfish_ecb_decrypt, blowfish_ofb64_encrypt,
		    %%
		    rand_bytes,
		    strong_rand_bytes,
		    rand_uniform,
		    mod_pow,
		    exor,
		    %% deprecated
		    mod_exp,strong_rand_mpint,erlint, mpint,
		    %%
		    sign, verify, generate_key, compute_key,
		    %% deprecated
		    dss_verify,dss_sign,
		    rsa_verify,rsa_sign,
		    rsa_public_encrypt,rsa_private_decrypt,
		    rsa_private_encrypt,rsa_public_decrypt,
		    dh_generate_key, dh_compute_key,
		    %%
		    stream_init, stream_encrypt, stream_decrypt,
		    %% deprecated
		    rc4_encrypt, rc4_set_key, rc4_encrypt_with_state,
		    aes_ctr_encrypt, aes_ctr_decrypt,
                    aes_ctr_stream_init, aes_ctr_stream_encrypt, aes_ctr_stream_decrypt,
		    %%
		    next_iv,
		    %% deprecated
		    aes_cbc_ivec,
		    des_cbc_ivec, des_cfb_ivec,
		    info,
		    %%
		    info_lib, supports]).
info() ->
    ?FUNC_LIST.

max_bytes() ->
	?CRYPTO_MAX_BYTES.

