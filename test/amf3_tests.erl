%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc AMF3 unit tests.
%% @hidden

-module(amf3_tests).
-author('ruslan@babayev.com').

-include_lib("eunit/include/eunit.hrl").

%% Assert that a value encodes as specified.
-define(_assertEncode(Encoding, Value),
	?_assertMatch(Encoding, amf3:encode(Value))).
-define(assertEncode(Encoding, Value),
	?assertMatch(Encoding, amf3:encode(Value))).

%% Assert that a binary decodes as specified.
-define(_assertDecode(Value, Encoded),
	?_assertMatch({Value,<<>>}, amf3:decode(Encoded))).
-define(assertDecode(Value, Encoded),
	?assertMatch({Value,<<>>}, amf3:decode(Encoded))).

%% Assert that an encoded value decodes back to itself.
-define(_assertCodec(E),
	?LET(V, E, ?_assertEqual(V, element(1, amf3:decode(amf3:encode(V)))))).
-define(assertCodec(E),
	?LET(V, E, ?assertEqual(V, element(1, amf3:decode(amf3:encode(V)))))).

encode_undefined_test() ->
    ?assertEncode(<<0>>, undefined).

encode_null_test() ->
    ?assertEncode(<<1>>, null).

encode_false_test() ->
    ?assertEncode(<<2>>, false).

encode_true_test() ->
    ?assertEncode(<<3>>, true).

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
    [?_assertThrow({bad_range, _}, amf3:encode(1 bsl 28)),
     ?_assertThrow({bad_range, _}, amf3:encode(-1 bsl 28 - 1))].

encode_doubles_test_() ->
    [?_assertEncode(<<5,-1.0e300:64/big-float>>, -1.0e300),
     ?_assertEncode(<<5,-1.0:64/big-float>>, -1.0),
     ?_assertEncode(<<5,0.0:64/big-float>>, 0.0),
     ?_assertEncode(<<5,1.0:64/big-float>>, 1.0),
     ?_assertEncode(<<5,1234.56:64/big-float>>, 1234.56),
     ?_assertEncode(<<5,1.0e300:64/big-float>>, 1.0e300),
     ?_assertEncode(<<5,0:1,16#7FF:11,0:52>>, '+infinity'),
     ?_assertEncode(<<5,1:1,16#7FF:11,0:52>>, '-infinity'),
     ?_assertEncode(<<5,0:1,16#7FF:11,1:1,0:51>>, 'qNaN'),
     ?_assertEncode(<<5,0:1,16#7FF:11,0:1,1:51>>, 'sNaN')].

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

encode_dates_refs_test() ->
    ?assertEncode(<<9,7,1,8,1,0.0:64/big-float,8,1,1.0:64/big-float,8,2>>,
		  [{date,0.0,0},{date,1.0,0},{date,0.0,0}]).

encode_object_string_refs_test() ->
    ?assertEncode(<<9,5,1,10,3,15,"MyClass",10,11,0,0,4,0,1>>,
		  [{object,<<"MyClass">>,[]},
		   {object,<<"MyClass">>,[{<<"MyClass">>,0}]}]).

codec_undefined_test() ->
    ?assertCodec(undefined).

codec_null_test() ->
    ?assertCodec(null).

codec_false_test() ->
    ?assertCodec(false).

codec_true_test() ->
    ?assertCodec(true).

codec_integers_test_() ->
    [?_assertCodec(-1 bsl 28),
     ?_assertCodec(-2),
     ?_assertCodec(-1),
     ?_assertCodec(0),
     ?_assertCodec(1),
     ?_assertCodec(2),
     ?_assertCodec(1 bsl 7 - 1),
     ?_assertCodec(1 bsl 7),
     ?_assertCodec(1 bsl 14 - 1),
     ?_assertCodec(1 bsl 14),
     ?_assertCodec(1 bsl 21 - 1),
     ?_assertCodec(1 bsl 21),
     ?_assertCodec(1 bsl 28 - 1)].

codec_doubles_test_() ->
    [?_assertCodec(-1.0e308),
     ?_assertCodec(-1.0),
     ?_assertCodec(0.0),
     ?_assertCodec(1.0),
     ?_assertCodec(1.0e308)].

codec_strings_test_() ->
    [?_assertCodec(<<"small string">>),
     ?_assertCodec(list_to_binary(string:copies("big", 500))),
     ?_assertCodec([<<"string a">>,<<"string b">>,<<"string a">>])].

codec_arrays_test_() ->
    [?_assertCodec([]),
     ?_assertCodec([1]),
     ?_assertCodec([1,2,3]),
     ?_assertCodec([<<"foo">>,<<"foo">>,<<"bar">>,<<"foo">>])].

codec_large_array_test() ->
    ?assertCodec(string:copies([<<"large array">>], 500)).

codec_xml_test() ->
    ?assertCodec({xml,<<"<xml><abc/></xml>">>}).

codec_xml_refs_test() ->
    ?assertCodec([1,{xml,<<"<xml/>">>},2,{xml,<<"<xml/>">>}]).

codec_dates_test_() ->
    [?_assertCodec({date,0.0,0}),
     ?_assertCodec({date,1736428493.9421,0}),
     ?_assertCodec([{date,0.0,0},{date,1.0,0},{date,0.0,0}])].

codec_objects_test_() ->
    [?_assertCodec({object,<<>>,[]}),
     ?_assertCodec({object,<<"MyClass">>,[]}),
     ?_assertCodec({object,<<"MyClass">>,
		    [{static1,1},{static2,2},
		     {<<"dynamic1">>,1},{<<"dynamic2">>,2}]}),
     ?_assertCodec({object,<<"MyClass">>,
		    [{list_to_binary("p" ++ integer_to_list(I)),I} ||
			I <- lists:seq(1,1000)]})].

codec_object_refs_test() ->
    ?assertCodec([{object,<<"MyClass">>,[]},
		  {object,<<"MyClass">>,[]},
		  {object,<<"MyClass">>,
		   [{a,{object,<<"MyClass">>,[]}}]}]).
