%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc CommandMessage Decoding.

-module(amf_CommandMessage).
-author('ruslan@babayev.com').

-export([decode_members/4]).

-define(OPERATION,   1).

-define(IS_SET(Byte, Flag), ((Byte) band Flag) == Flag).
-define(CLEAR(Byte, Flag), ((Byte) band bnot Flag)).

%% @doc Decodes members.
%% @spec decode_members(binary(), Strings, Objects, Traits) ->
%%       {Members, Rest, Strings, Objects, Traits}
%%       Members = amf3:members()
%%       Rest = binary()
%%       Strings = amf3:refs()
%%       Objects = amf3:refs()
%%       Traits = amf3:refs()
decode_members(Data, Strings, Objects, Traits) ->
    {AsyncMessageMembers, Rest, Strings1, Objects1, Traits1} =
	amf_AsyncMessage:decode_members(Data, Strings, Objects, Traits),
    {Bytes, Rest1} = amf_AbstractMessage:decode_flag_bytes(Rest),
    Flags = decode_flags(Bytes),
    {CommandMessageMembers, Rest2, Strings2, Objects2, Traits2} =
	amf_AbstractMessage:decode_members(Flags, Rest1, Strings1,
					   Objects1, Traits1, []),
    Members = AsyncMessageMembers ++ CommandMessageMembers,
    {Members, Rest2, Strings2, Objects2, Traits2}.

%% @doc Decodes flags.
%% @spec decode_flags(Flags::[integer()]) -> [operation | ignored]
decode_flags([B]) ->
    decode_flags1(B, []).

%% @doc Decodes flags.
%% @spec decode_flags1(Byte::integer(), [integer()]) -> [operation | ignored]
decode_flags1(0, Acc) ->
    lists:reverse(Acc);
decode_flags1(B, Acc) when ?IS_SET(B, ?OPERATION) ->
    decode_flags1(?CLEAR(B, ?OPERATION), [operation | Acc]);
decode_flags1(B, Acc) ->
    amf_AbstractMessage:decode_ignored_flags(B bsr 1, Acc).
