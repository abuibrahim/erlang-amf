-module('CommandMessage').
-export([decode/4]).

-include("amf.hrl").

-define(OPERATION,   1).

-define(IS_SET(Byte, Flag), ((Byte) band Flag) == Flag).
-define(CLEAR(Byte, Flag), ((Byte) band bnot Flag)).

decode(Data, Strings, Objects, Traits) ->
    {Object1, Rest, Strings1, Objects1, Traits1} =
	'AsyncMessage':decode(Data, Strings, Objects, Traits),
    {Bytes, Rest1} = 'AbstractMessage':decode_flag_bytes(Rest),
    Flags = decode_flags(Bytes),
    {CommandMessageMembers, Rest2, Strings2, Objects2, Traits2} =
	'AbstractMessage':decode_members(Flags, Rest1, Strings1,
					 Objects1, Traits1, []),
    Members = Object1#amf_object.members ++ CommandMessageMembers,
    Object2 = Object1#amf_object{members = Members},
    {Object2, Rest2, Strings2, Objects2, Traits2}.

decode_flags([B]) ->
    decode_flags1(B, []).

decode_flags1(0, Acc) ->
    lists:reverse(Acc);
decode_flags1(B, Acc) when ?IS_SET(B, ?OPERATION) ->
    decode_flags1(?CLEAR(B, ?OPERATION), [operation | Acc]);
decode_flags1(B, Acc) ->
    'AbstractMessage':decode_ignored_flags(B bsr 1, Acc).
