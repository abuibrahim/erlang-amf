-module('AcknowledgeMessage').
-export([decode/4]).

-include("amf.hrl").

-define(IS_SET(Byte, Flag), ((Byte) band Flag) == Flag).
-define(CLEAR(Byte, Flag), ((Byte) band bnot Flag)).

decode(Data, Strings, Objects, Traits) ->
    {Object1, Rest, Strings1, Objects1, Traits1} =
	'AsyncMessage':decode(Data, Strings, Objects, Traits),
    {Bytes, Rest1} = 'AbstractMessage':decode_flag_bytes(Rest),
    Flags = decode_flags(Bytes),
    {_IgnoredMembers, Rest2, Strings2, Objects2, Traits2} =
	'AbstractMessage':decode_members(Flags, Rest1, Strings1,
					 Objects1, Traits1, []),
    {Object1, Rest2, Strings2, Objects2, Traits2}.

decode_flags([B]) ->
    'AbstractMessage':decode_ignored_flags(B, []).
