%% Copyright (c) 2013-2014 Cloudozer LLP. All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%% 
%% * Redistributions of source code must retain the above copyright notice, this
%% list of conditions and the following disclaimer.
%% 
%% * Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
%% 
%% * Redistributions in any form must be accompanied by information on how to
%% obtain complete source code for the LING software and any accompanying
%% software that uses the LING software. The source code must either be included
%% in the distribution or be available for no more than the cost of distribution
%% plus a nominal fee, and must be freely redistributable under reasonable
%% conditions.  For an executable file, complete source code means the source
%% code for all modules it contains. It does not include source code for modules
%% or files that typically accompany the major components of the operating
%% system on which the executable file runs.
%% 
%% THIS SOFTWARE IS PROVIDED BY CLOUDOZER LLP ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE
%% DISCLAIMED. IN NO EVENT SHALL CLOUDOZER LLP BE LIABLE FOR ANY DIRECT,
%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%%
%%% @doc Authentication/authorization for 9p connections. The module implements
%%% two auhtentication schemes: MUNGE and MUMBLE. Both schemes are based on a
%%% pre-shared secrets. The module uses `auth:secret1()' and `auth:secret2()'
%%% calls to retrieve the secrets. The definition of #munge{} and #mumble{}
%%% records is given in 9p_auth.hrl.
%%%
-module('9p_auth').

-export([mumble/1,mumble/2,mumble/3]).
-export([unmumble/1]).

-export([munge/2,munge/3,munge/4]).
-export([unmunge/1]).

-export([generate_session_key/0]).

-include("9p_auth.hrl").

-define(MUMBLE_VER, 16#0100).
-define(MUMBLE_MAC_SIZE, 20).

-define(MUNGE_VER, 3).

-define(MUNGE_DEFAULT_TTL, 300).
-define(MUNGE_MAX_TTL, 3600).

-define(debug(Fmt, Args), io:format(Fmt, Args)).

-spec mumble(SessionKey) -> Mumble when
	SessionKey :: binary(),
	Mumble :: binary().

mumble(SessionKey) ->
	mumble(SessionKey, <<>>, 0).

-spec mumble(SessionKey, Extra) -> Mumble when
	SessionKey :: binary(),
	Extra :: binary(),
	Mumble :: binary().

mumble(SessionKey, Extra) ->
	mumble(SessionKey, Extra, 0).

%% @doc Produce an authenticated MUMBLE message. The `SessionKey' identifies the
%% current session and is needed for session recovery when a transport
%% connection is reestablished. The `SessionKey' must be a binary of length
%% <b>8</b>. `Extra' is arbitrary data embedded into the MUMBLE message. Defaults
%% to `<<>>'. `TTL' is the number of seconds the message is valid after
%% encoding. Defaults to 0 (infinity).
-spec mumble(SessionKey, Extra, TTL) -> Mumble when
	SessionKey :: binary(),
	Extra :: binary(),
	TTL :: integer,
	Mumble :: binary().

mumble(SessionKey, Extra, TTL)
		when byte_size(SessionKey) =:= 8, is_binary(Extra), is_integer(TTL) ->

	NodeB = to_bin(erlang:node()),
	GroupB = to_bin(erlang:node_group()),

	TS = '9p':timestamp(),
	
	ZeroMac = list_to_binary(lists:duplicate(?MUMBLE_MAC_SIZE, 0)),
	ZeroMsg = pack(ZeroMac, SessionKey, TS, TTL, NodeB, GroupB, Extra),

	Mac = crypto:hmac(sha, auth:secret1(), ZeroMsg),
	MumbleBin =pack(Mac, SessionKey, TS, TTL, NodeB, GroupB, Extra),

	<<"MUMBLE:",(base64:encode(MumbleBin))/binary,":">>.

%% @doc Verifies and decodes a MUMBLE message. Returns `not_authenticated' if
%% the computed MAC does not match the one included with the message. A MUMBLE
%% message may contain an expiry time. Accordingly, `expired' may be returned by
%% the call. Uses `auth:secret1()' only. 
-spec unmumble(Message) -> #mumble{} | invalid | not_authenticated | expired when
	Message :: binary().

unmumble(<<"MUMBLE:",MsgCol/binary>>) when size(MsgCol) > 0 ->
	case binary:last(MsgCol) of
	$: ->
		case base64:decode(binary:part(MsgCol, 0, size(MsgCol) -1)) of
		<<?MUMBLE_VER:16/little,
		   Mac:?MUMBLE_MAC_SIZE/binary,
		   SessionKey:8/binary,
		   EncodedTS:32/little,
		   TTL:32/little,
		   NSz:16/little,NodeB:(NSz)/binary,
		   GSz:16/little,GroupB:(GSz)/binary,
		   ESz:32/little,Extra:(ESz)/binary>> ->
	
			TS = '9p':timestamp(),
			if TTL =/= 0, EncodedTS +TTL < TS ->
				expired;
	
			true ->
				ZeroMac = list_to_binary(lists:duplicate(?MUMBLE_MAC_SIZE, 0)),
				ZeroMsg = pack(ZeroMac, SessionKey, EncodedTS, TTL, NodeB, GroupB, Extra),

				case crypto:hmac(sha, auth:secret1(), ZeroMsg) of
				Mac ->

					#mumble{ver =?MUMBLE_VER,
							session_key =SessionKey,
							node =to_atom(NodeB),
							node_group =to_atom(GroupB),
							extra =Extra};

				_ ->
					not_authenticated
				end
			end
		end;
	_ ->
		invalid
	end;

unmumble(_) ->
	invalid.

%%-------- MUNGE scheme -------------------------------------------------------

-spec munge(UnixUid, UnixGid) -> Munge when
	UnixUid :: integer(),
	UnixGid :: integer(),
	Munge :: binary().

munge(UnixUid, UnixGid) ->
	munge(UnixUid, UnixGid, <<>>, 0).
	
-spec munge(UnixUid, UnixGid, Payload) -> Munge when
	UnixUid :: integer(),
	UnixGid :: integer(),
	Payload :: binary(),
	Munge :: binary().

munge(UnixUid, UnixGid, Payload) ->
	munge(UnixUid, UnixGid, Payload, 0).
	
%% @doc Encodes an authenticated MUNGE message. UnixUid and UnixGid are
%% credentials embedded into the message.
-spec munge(UnixUid, UnixGid, Payload, TTL) -> Munge when
	UnixUid :: integer(),
	UnixGid :: integer(),
	Payload :: binary(),
	TTL :: integer(),
	Munge :: binary().

munge(UnixUid, UnixGid, Payload, TTL)
		when is_integer(UnixUid), is_integer(UnixGid),
			 is_binary(Payload), is_integer(TTL) ->

	IVec = crypto:rand_bytes(16),
	Outer = <<?MUNGE_VER,4,3,0,0,IVec/binary>>,
	
	Salt = crypto:rand_bytes(8),
	Origin = get_origin(),
	TS = '9p':timestamp(),
	NormTTL = normalize_ttl(TTL),

	Inner = <<Salt/binary,
			  (size(Origin)),Origin/binary,
			  TS:32,
			  NormTTL:32,
			  UnixUid:32,
			  UnixGid:32,
			  16#ffffffff:32,
			  16#ffffffff:32,
			  (size(Payload)):32,Payload/binary>>,
	
	MKey = auth:secret1(),
	CKey = auth:secret2(),

	Mac = crypto:hmac(sha, MKey, <<Outer/binary,Inner/binary>>),
	
	PadInner = pad(Inner),
	DEK = crypto:hmac(sha, CKey, Mac),
	CryptInner = crypto:block_encrypt(aes_cbc128, DEK, IVec, PadInner),

	Combined = <<Outer/binary,Mac/binary,CryptInner/binary>>,
	<<"MUNGE:",(base64:encode(Combined))/binary,":">>.

%% @doc Verifies and decodes a MUNGE message. Returns `not_authenticated' if the
%% computed Mac does not match. `bad_padding' may be returned if unencrypted
%% message does not follow PKCS#5 padding scheme. The implementation accepts only
%% MUNGE messages that use SHA-1 as a hash function and AES-CBC-128 as a cipher.
%% Uses both `auth:secret1()' and `auth:secret2()'.
-spec unmunge(Message) -> #munge{} |
		invalid | not_authenticated | bad_padding | expired when
	Message :: binary().

unmunge(<<"MUNGE:",MsgCol/binary>>) when size(MsgCol) > 0 ->
	case binary:last(MsgCol) of
	$: ->
		case base64:decode(binary:part(MsgCol, 0, size(MsgCol) -1)) of
		<<?MUNGE_VER,	%% version
		  4,			%% AES-128 CBC
		  3,			%% SHA-1
		  0,			%% no compression
		  RSz,Realm:(RSz)/binary,
		  IVec:16/binary,
		  Mac:20/binary,
		  EncInner/binary>> ->

			Outer = <<?MUNGE_VER,4,3,0,RSz,Realm/binary,IVec/binary>>,

			MKey = auth:secret1(),
			CKey = auth:secret2(),

			DEK = crypto:hmac(sha, CKey, Mac),
			PadInner = crypto:block_decrypt(aes_cbc128, DEK, IVec, EncInner),

			case unpad(PadInner) of
			Inner when is_binary(Inner) ->

				case crypto:hmac(sha, MKey, <<Outer/binary,Inner/binary>>) of
				Mac ->

					case Inner of
					<<_Salt:8/binary,
					  OSz,Origin:(OSz)/binary,
					  EncodedTS:32,
					  TTL:32,
					  Uid:32,
					  Gid:32,
					  DecUid:32,
					  DecGid:32,
					  PSz:32,Payload:(PSz)/binary>> ->

						TS = '9p':timestamp(),
						if TTL =/= 0, EncodedTS +TTL < TS ->
							expired;

						true ->

							#munge{ver =?MUNGE_VER,
								   realm =Realm,
								   origin =Origin,
								   uid =Uid,
								   gid =Gid,
								   dec_uid =DecUid,
								   dec_gid =DecGid,
								   payload =Payload}
						end;
					_ ->
						?debug("unmunge: invalid inner data: ~p\n", [Inner]),
						invalid
					end;
				_WrongMac ->
					?debug("unmunge: MAC mismatch: ~p\n", [_WrongMac]),
					not_authenticated
				end;
			_ ->
				?debug("unmunge: bad padding: ~p\n", [PadInner]),
				bad_padding
			end;
		_Bin ->
			?debug("unmunge: invalid outer data: ~p\n", [_Bin]),
			invalid
		end;
	_ ->
		?debug("unmunge: bad ascii armor\n", []),
		invalid
	end;

unmunge(_) ->
	invalid.

%% @doc Generates a random session key. The session key may be used for
%% recovering a session after a lost transport connection.
-spec generate_session_key() -> binary().

generate_session_key() ->
	crypto:rand_bytes(8).

%%-------- helpers ------------------------------------------------------------

pack(Mac, SessionKey, TS, TTL, NodeB, GroupB, Extra) ->
	<<?MUMBLE_VER:16/little,
	  Mac/binary,
	  SessionKey/binary,
	  TS:32/little,
	  TTL:32/little,
	  (size(NodeB)):16/little,NodeB/binary,
	  (size(GroupB)):16/little,GroupB/binary,
	  (size(Extra)):32/little,Extra/binary>>.
	
to_bin(A) ->
	list_to_binary(atom_to_list(A)).

to_atom(B) ->
	list_to_atom(binary_to_list(B)).

%% add PKCS#5 padding
pad(Unpadded) ->
	N = 16 - size(Unpadded) rem 16,
	Padding = list_to_binary(lists:duplicate(N, N)),
	<<Unpadded/binary,Padding/binary>>.

%% undo PKCS#5 padding
unpad(Padded) when size(Padded) > 0 ->
	case binary:last(Padded) of
	N when N >= 1, N =< 16 ->
		if size(Padded) >= N ->
			{Unpadded,Padding} = erlang:split_binary(Padded, size(Padded) -N),
			case list_to_binary(lists:duplicate(N, N)) of
			Padding ->
				Unpadded;
			_ ->
				invalid
			end;
		true ->
			invalid
		end;
	_ ->
		invalid
	end;
unpad(_) ->
	invalid.

%% get ip address of the current node
get_origin() ->

	%%
	%% TODO: can be done easier with inet:getif/2
	%% when this undocumented function is implemented.
	%%
	%% {ok,[{addr,Addr}]} = inet:ifget("eth0", [addr]),

	{ok,InterInfos} = inet:getifaddrs(),
	{_,Props} = lists:keyfind("eth0", 1, InterInfos),
	{_,{A,B,C,D}} = lists:keyfind(addr, 1, Props),
	<<A,B,C,D>>.

%% follow converntions of standard munge implementation
normalize_ttl(0) -> ?MUNGE_DEFAULT_TTL;
normalize_ttl(-1) -> ?MUNGE_MAX_TTL;
normalize_ttl(TTL) when TTL > ?MUNGE_MAX_TTL -> ?MUNGE_MAX_TTL;
normalize_ttl(TTL) -> TTL.

%%EOF
