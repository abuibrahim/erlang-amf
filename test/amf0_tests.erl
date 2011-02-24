%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc AMF0 unit tests.
%% @hidden

-module(amf0_tests).
-author('ruslan@babayev.com').

-include_lib("eunit/include/eunit.hrl").

%% Assert that a value encodes as specified.
-define(_assertEncode(Encoding, Value),
	?_assertMatch(Encoding, amf0:encode(Value))).
-define(assertEncode(Encoding, Value),
	?assertMatch(Encoding, amf0:encode(Value))).

%% Assert that a binary decodes as specified.
-define(_assertDecode(Value, Encoded),
	?_assertMatch({Value,<<>>}, amf0:decode(Encoded))).
-define(assertDecode(Value, Encoded),
	?assertMatch({Value,<<>>}, amf0:decode(Encoded))).

%% Assert that an encoded value decodes back to itself.
-define(_assertCodec(E),
	?LET(V, E, ?_assertEqual(V, element(1, amf0:decode(amf0:encode(V)))))).
-define(assertCodec(E),
	?LET(V, E, ?assertEqual(V, element(1, amf0:decode(amf0:encode(V)))))).

encode_numbers_test_() ->
    [?_assertEncode(<<0,0.0:64/big-float>>, 0),
     ?_assertEncode(<<0,1.0:64/big-float>>, 1),
     ?_assertEncode(<<0,-1.0:64/big-float>>, -1),
     ?_assertEncode(<<0,0.0:64/big-float>>, 0.0),
     ?_assertEncode(<<0,-7.5:64/big-float>>, -7.5),
     ?_assertEncode(<<0,0:1,16#7FF:11,0:52>>, '+infinity'),
     ?_assertEncode(<<0,1:1,16#7FF:11,0:52>>, '-infinity'),
     ?_assertEncode(<<0,0:1,16#7FF:11,1:1,0:51>>, 'qNaN'),
     ?_assertEncode(<<0,0:1,16#7FF:11,0:1,1:51>>, 'sNaN')].

encode_booleans_test_() ->
    [?_assertEncode(<<1,0>>, false),
     ?_assertEncode(<<1,1>>, true)].

encode_null_test() ->
    ?assertEncode(<<5>>, null).

encode_undefined_test() ->
    ?assertEncode(<<6>>, undefined).

encode_unsupported_test() ->
    ?assertEncode(<<13>>, unsupported).

encode_strings_test_() ->
    [?_assertEncode(<<2,0,6,"abcdef">>, <<"abcdef">>),
     ?_assertEncode(<<2,0,0>>,<<"">>),
     ?_assertEncode(<<2,0,4,"test">>, <<"test">>)].

encode_long_string_test() ->
    ?assertEncode(<<12,0,1,165,230,"abcdef",_:108000/binary>>,
		  list_to_binary(string:copies("abcdef", 18001))).

encode_array_test() ->
    ?assertEncode(<<10,0,0,0,1,0,64,88,64,0,0,0,0,0>>, "a").

encode_object_test() ->
    ?assertEncode(<<3,0,3,"foo",2,0,3,"abc",0,3,"bar",1,1,0,0,9>>,
		  {object,[{foo,<<"abc">>},{bar,true}]}).

encode_arrays_test_() ->
    [?_assertEncode(<<10,0,0,0,0>>, []),
     ?_assertEncode(<<10,0,0,0,1,5>>, [null]),
     ?_assertEncode(<<10,0,0,0,2,5,5>>, [null,null])].

encode_dates_test_() ->
    [?_assertEncode(<<11,1252627967.0:64/big-float,0,0>>,
		    {date,1252627967,0}),
     ?_assertEncode(<<11,1252627967.442:64/big-float,0,0>>,
		    {date,1252627967.442,0})].

encode_xmldoc_test() ->
    ?assertEncode(<<15,0,0,0,6,"<xml/>">>, {xmldoc,<<"<xml/>">>}).

encode_typed_object_test() ->
    ?assertEncode(<<16,0,5,"class",0,1,"a",1,0,0,2,"ab",1,1,0,0,9>>,
		  {object,<<"class">>,[{a, false}, {ab, true}]}).

encode_avmplus_test() ->
    ?assertEncode(<<17,6,(8 bsl 1 bor 1),"testtest">>,
		  {avmplus,<<"testtest">>}).

decode_number_test() ->
    ?assertDecode(0.0, <<0,0.0:64/big-float>>).

decode_string_test() ->
    ?assertDecode(<<"abcdef">>, <<2,0,6,"abcdef">>).

decode_booleans_test_() ->
    [?_assertDecode(false,<<1,0>>),
     ?_assertDecode(true,<<1,1>>)].

decode_avmplus_test() ->
    ?assertDecode({avmplus,1234}, <<17,4,137,82>>).

decode_references_test() ->
    ?assertDecode([{object,[]},{object,[]}], <<10,0,0,0,2,3,0,0,9,7,0,1>>).

codec_numbers_test_() ->
    [?_assertCodec(0.0),
     ?_assertCodec(5.0),
     ?_assertCodec(-5.0),
     ?_assertCodec(1.0e300),
     ?_assertCodec(-1.0e300)].

codec_booleans_test_() ->
    [?_assertCodec(false),
     ?_assertCodec(true)].

codec_null_test() ->
    ?assertCodec(null).

codec_undefined_test() ->
    ?assertCodec(undefined).

codec_unsupported_test() ->
    ?assertCodec(unsupported).

codec_string_test() ->
    ?assertCodec(<<"quickfox">>).

codec_long_string_test() ->
    ?assertCodec(list_to_binary(string:copies("lazydog", 18000))).

codec_object_test() ->
    ?assertCodec({object,[{abc,5.5},{def,false},{xyz,null}]}).

codec_arrays_test_() ->
    [?_assertCodec([]),
     ?_assertCodec([1.0,2.0,3.0]),
     ?_assertCodec([<<"a">>,<<"b">>,[<<"x">>,true,false]])].

codec_dates_test_() ->
    [?_assertCodec({date,0.0,0}),
     ?_assertCodec({date,5000.0,0})].

codec_xmldoc_test() ->
    ?assertCodec({xmldoc,<<"<xml></xml>">>}).

codec_typed_object_test() ->
    ?assertCodec({object,<<"class">>,[{m1,true},{m2,false}]}).

codec_avmplus_test() ->
    ?assertCodec({avmplus,123}).

codec_nested_array_of_dates_test() ->
    D1 = {date,789456.21,0},
    D2 = {date,123456.45,0},
    ?assertCodec([[D2,D2],D1,[D2,D2],[D1,D2],[D2,[D1,D2],D1],D2,D1]).
