%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc AMF Encoding and Decoding.

-module(amf).
-author('ruslan@babayev.com').

-export([encode_packet/1, decode_packet/1]).

-include("amf.hrl").

%% @doc Decodes a packet.
%% @spec decode_packet(Bytes::binary()) -> #amf_packet{}
decode_packet(<<Version:16, HeaderCount:16, Data/binary>>) ->
    {Headers, <<MessageCount:16, Rest/binary>>} =
	decode_headers(Data, HeaderCount, []),
    {Messages, _Rest} = decode_messages(Rest, MessageCount, []),
    #amf_packet{version = Version, headers = Headers, messages = Messages}.

%% @doc Decodes headers.
%% @spec decode_headers(binary(), integer(), Acc) -> {Headers, Rest}
decode_headers(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
decode_headers(<<NL:16, Name:NL/binary, M, _DL:32, Data/binary>>, N, Acc) ->
    {Body, Rest} = amf0:decode(Data),
    Header = #amf_header{name = Name, must_understand = (M /= 0), body = Body},
    decode_headers(Rest, N - 1, [Header | Acc]).

%% @doc Decodes messages.
%% @spec decode_messages(binary(), integer(), Acc) -> {Messages, Rest}
decode_messages(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
decode_messages(<<TL:16, Target:TL/binary, RL:16, Response:RL/binary,
		  _DL:32, Data/binary>>, N, Acc) ->
    {Body, Rest} = amf0:decode(Data),
    Message = #amf_message{target = Target, response = Response, body = Body},
    decode_messages(Rest, N - 1, [Message | Acc]).

%% @doc Encodes a packet.
%% @spec encode_packet(Packet::#amf_packet{}) -> binary()
encode_packet(#amf_packet{version = Version, headers = Headers,
			  messages = Messages}) ->
    HeadersBin = encode_headers(Headers, <<>>, Version),
    MessagesBin = encode_messages(Messages, <<>>, Version),
    <<Version:16, (length(Headers)):16, HeadersBin/binary,
      (length(Messages)):16, MessagesBin/binary>>.

%% @doc Encodes headers.
%% @spec encode_headers(Headers, Acc, Version) -> binary()
encode_headers([], Acc, _Version) ->
    Acc;
encode_headers([Header | Rest], Acc, Version) ->
    Name = Header#amf_header.name,
    M = case Header#amf_header.must_understand of
	    true ->  1;
	    false -> 0
	end,
    Body = encode(Header#amf_header.body, Version),
    Bin = <<(size(Name)):16, Name/binary, M, (size(Body)):32, Body/binary>>,
    encode_headers(Rest, <<Acc/binary, Bin/binary>>, Version).

%% @doc Encodes messages.
%% @spec encode_messages(Messages, Acc, Version) -> binary()
encode_messages([], Acc, _Version) ->
    Acc;
encode_messages([Message | Rest], Acc, Version) ->
    #amf_message{target = Target, response = Response, body = Body} = Message,
    Bin0 = encode(Body, Version),
    Bin1 = <<(size(Target)):16, Target/binary,
	     (size(Response)):16, Response/binary,
	     (size(Bin0)):32, Bin0/binary>>,
    encode_messages(Rest, <<Acc/binary, Bin1/binary>>, Version).

%% @doc Encodes body of the message to AMF0 or AMF3.
%% @spec encode(Body, Version::integer()) -> binary()
encode(Body, 0) -> amf0:encode(Body);
encode(Body, 3) -> amf0:encode({avmplus, Body}).
