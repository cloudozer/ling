%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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

-module(maps).

-export([
	fold/3,
	map/2,
	size/1,
    without/2,
    with/2,
    get/3
    ]).

-spec get(Key, Map, Default) -> Value | Default when
        Key :: term(),
        Map :: map(),
        Value :: term(),
        Default :: term().

get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.


-spec fold(Fun,Init,Map) -> Acc when
    Fun :: fun((K, V, AccIn) -> AccOut),
    Init :: term(),
    Acc :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Map :: map(),
    K :: term(),
    V :: term().

fold(Fun, Init, Map) when is_function(Fun,3), is_map(Map) ->
    lists:foldl(fun({K,V},A) -> Fun(K,V,A) end,Init,maps:to_list(Map)).

-spec map(Fun,Map1) -> Map2 when
    Fun :: fun((K, V1) -> V2),
    Map1 :: map(),
    Map2 :: map(),
    K :: term(),
    V1 :: term(),
    V2 :: term().

map(Fun, Map) when is_function(Fun, 2), is_map(Map) ->
    maps:from_list(lists:map(fun
		({K,V}) ->
		    {K,Fun(K,V)}
	    end,maps:to_list(Map))).


-spec size(Map) -> non_neg_integer() when
    Map :: map().

size(Map) when is_map(Map) ->
    erlang:map_size(Map).


-spec without(Ks,Map1) -> Map2 when
    Ks :: [K],
    Map1 :: map(),
    Map2 :: map(),
    K :: term().

without(Ks, M) when is_list(Ks), is_map(M) ->
    maps:from_list([{K,V}||{K,V} <- maps:to_list(M), not lists:member(K, Ks)]).


-spec with(Ks, Map1) -> Map2 when
    Ks :: [K],
    Map1 :: map(),
    Map2 :: map(),
    K :: term().

with(Ks, M) when is_list(Ks), is_map(M) ->
    maps:from_list([{K,V}||{K,V} <- maps:to_list(M), lists:member(K, Ks)]).
