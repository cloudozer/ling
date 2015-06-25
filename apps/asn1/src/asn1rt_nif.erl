%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2013. All Rights Reserved.
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
%%
-module(asn1rt_nif).

%% (Faux) nif interface for asn1

-export([encode_per_complete/1,
	 decode_ber_tlv/1,
	 encode_ber_tlv/1]).

decode_ber_tlv(Binary) -> asn1rtt_ber:ber_decode_erlang(Binary).

encode_ber_tlv(_TagValueList)	   -> throw(not_implemented).
encode_per_complete(_TagValueList) -> throw(not_implemented).

