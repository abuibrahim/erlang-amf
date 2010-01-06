%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc AcknowledgeMessage Decoding.

-module(amf_AcknowledgeMessage).
-author('ruslan@babayev.com').

-export([decode_members/4]).

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
    {_IgnoredMembers, Rest2, Strings2, Objects2, Traits2} =
	amf_AbstractMessage:decode_members(Flags, Rest1, Strings1,
					   Objects1, Traits1, []),
    {AsyncMessageMembers, Rest2, Strings2, Objects2, Traits2}.

%% @doc Decodes flags.
%% @spec decode_flags(Flags::[integer()]) -> [ignored]
decode_flags([B]) ->
    amf_AbstractMessage:decode_ignored_flags(B, []).
