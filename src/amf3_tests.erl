%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009, Ruslan Babayev.
%% @doc AMF3 unit tests.

-module(amf3_tests).
-author('ruslan@babayev.com').

-include_lib("eunit/include/eunit.hrl").

%% Assert that a value encodes as specified.
-define(_assertEncode(Encoding, Value),
	?_assertMatch(Encoding, amf3:encode(Value))).

%% Assert that a binary decodes as specified.
-define(_assertDecode(Value, Encoded),
	?_assertMatch({Value,<<>>}, amf3:decode(Encoded))).

%% Assert that an encoded value decodes back to itself.
-define(_assertCodec(E),
	?LET(V, E, ?_assertEqual(V, element(1, amf3:decode(amf3:encode(V)))))).

encode_undefined_test() ->
    ?_assertEncode(<<0>>, undefined).

encode_null_test() ->
    ?_assertEncode(<<1>>, null).

encode_false_test() ->
    ?_assertEncode(<<2>>, false).

encode_true_test() ->
    ?_assertEncode(<<3>>, true).

encode_integers_test_() ->
    [?_assertEncode(<<4,192,128,128,0>>, -1 bsl 28),
     ?_assertEncode(<<4,255,255,255,254>>, -2),
     ?_assertEncode(<<4,255,255,255,255>>, -1),
     ?_assertEncode(<<4,0>>, 0),
     ?_assertEncode(<<4,1>>, 1),
     ?_assertEncode(<<4,50>>, 50),
     ?_assertEncode(<<4,127>>, 127),
     ?_assertEncode(<<4,129,0>>, 1 bsl 7),
     ?_assertEncode(<<4,129,1>>, 1 bsl 7 + 1),
     ?_assertEncode(<<4,255,127>>, 1 bsl 14 - 1),
     ?_assertEncode(<<4,129,128,0>>, 1 bsl 14),
     ?_assertEncode(<<4,129,128,1>>, 1 bsl 14 + 1),
     ?_assertEncode(<<4,255,255,127>>, 1 bsl 21 - 1),
     ?_assertEncode(<<4,128,192,128,0>>, 1 bsl 21),
     ?_assertEncode(<<4,128,192,128,1>>, 1 bsl 21 + 1),
     ?_assertEncode(<<4,191,255,255,255>>, 1 bsl 28 - 1)].

encode_large_integers_test_() ->
    [?_assertThrow(badrange, amf3:encode(1 bsl 28)),
     ?_assertThrow(badrange, amf3:encode(-1 bsl 28 - 1))].

encode_doubles_test_() ->
    [?_assertEncode(<<5,-1.0e300:64/big-float>>, -1.0e300),
     ?_assertEncode(<<5,-1.0:64/big-float>>, -1.0),
     ?_assertEncode(<<5,0.0:64/big-float>>, 0.0),
     ?_assertEncode(<<5,1.0:64/big-float>>, 1.0),
     ?_assertEncode(<<5,1234.56:64/big-float>>, 1234.56),
     ?_assertEncode(<<5,1.0e300:64/big-float>>, 1.0e300)].

encode_strings_test_() ->
    [?_assertEncode(<<6,151,57,"bigbigbig",_/binary>>,
		    list_to_binary(string:copies("big", 500))),
     ?_assertEncode(<<6,13,"string">>, <<"string">>)].

encode_dates_test_() ->
    [?_assertEncode(<<8,1,0,0,0,0,0,0,0,0>>, {date,0,0}),
     ?_assertEncode(<<8,1,1252627.0:64/big-float>>, {date,1252627.0,0})].

encode_arrays_test_() ->
    [?_assertEncode(<<9,3,1,4,0>>, [0]),
     ?_assertEncode(<<9,7,1,4,1,4,2,4,3>>, [1,2,3]),
     ?_assertEncode(<<9,7,1,3,3,3>>, [true,true,true])].

encode_objects_test_() ->
    [?_assertEncode(<<10,3,1>>, {object,<<>>,[]}),
     ?_assertEncode(<<10,19,1,11,"prop1",4,127>>, {object,<<>>,[{prop1,127}]}),
     ?_assertEncode(<<10,35,1,11,"prop1",11,"prop2",4,127,3>>,
		    {object,<<>>,[{prop1,127},{prop2,true}]}),
     ?_assertEncode(<<10,19,7,"Foo",3,"a",4,1>>,
		    {object,<<"Foo">>,[{a,1}]})].

encode_xml_test_() ->
    [?_assertEncode(<<11,13,"<xml/>">>, {xml, <<"<xml/>">>}),
     ?_assertEncode(<<11,23,"<xml></xml>">>, {xml, <<"<xml></xml>">>})].

encode_string_refs_test_() ->
    [?_assertEncode(<<6,1>>, <<>>),
     ?_assertEncode(<<9,5,1,6,13,"abcdef",6,0>>, [<<"abcdef">>,<<"abcdef">>]),
     ?_assertEncode(<<9,7,1,6,13,"abcdef",6,7,"foo",6,0>>,
		    [<<"abcdef">>,<<"foo">>,<<"abcdef">>]),
     ?_assertEncode(<<9,9,1,6,13,"abcdef",11,0,11,0,6,0>>,
		    [<<"abcdef">>,{xml,<<"abcdef">>},{xml,<<"abcdef">>},
		     <<"abcdef">>]),
     ?_assertEncode(<<9,13,1,6,13,"abcdef",6,9,"test",6,1,6,0,6,2,6,1>>,
		    [<<"abcdef">>,<<"test">>,<<>>,<<"abcdef">>,
		     <<"test">>,<<>>])].

encode_dates_refs_test_() ->
    ?_assertEncode(<<9,7,1,8,1,0.0:64/big-float,8,1,1.0:64/big-float,8,2>>,
		   [{date,0.0,0},{date,1.0,0},{date,0.0,0}]).
