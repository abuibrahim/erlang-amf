%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc AMF0 unit tests.

-module(amf0_tests).
-author('ruslan@babayev.com').

-include_lib("eunit/include/eunit.hrl").

%% Assert that a value encodes as specified.
-define(_assertEncode(Encoding, Value),
	?_assertMatch(Encoding, amf0:encode(Value))).

%% Assert that a binary decodes as specified.
-define(_assertDecode(Value, Encoded),
	?_assertMatch({Value,<<>>}, amf0:decode(Encoded))).

%% Assert that an encoded value decodes back to itself.
-define(_assertCodec(E),
	?LET(V, E, ?_assertEqual(V, element(1, amf0:decode(amf0:encode(V)))))).

encode_numbers_test_() ->
    [?_assertEncode(<<0,0.0:64/big-float>>, 0),
     ?_assertEncode(<<0,1.0:64/big-float>>, 1),
     ?_assertEncode(<<0,-1.0:64/big-float>>, -1),
     ?_assertEncode(<<0,0.0:64/big-float>>, 0.0),
     ?_assertEncode(<<0,-7.5:64/big-float>>, -7.5)].

encode_booleans_test_() ->
    [?_assertEncode(<<1,0>>, false),
     ?_assertEncode(<<1,1>>, true)].

encode_null_test() ->
    ?_assertEncode(<<5>>, null).

encode_undefined_test() ->
    ?_assertEncode(<<6>>, undefined).

encode_unsupported_test() ->
    ?_assertEncode(<<13>>, unsupported).

encode_strings_test() ->
    [?_assertEncode(<<2,0,6,"abcdef">>, <<"abcdef">>),
     ?_assertEncode(<<2,0,0>>,<<"">>),
     ?_assertEncode(<<2,0,4,"test">>, <<"test">>)].

encode_long_string_test() ->
    ?_assertEncode(<<12,0,1,165,230,"abcdef",_:108000/binary>>,
		   list_to_binary(string:copies(<<"abcdef">>,18001))).

encode_array_test() ->
    ?_assertEncode(<<10,0,0,0,1,0,64,88,64,0,0,0,0,0>>, "a").

encode_object_test() ->
    ?_assertEncode(<<3,0,3,"foo",2,0,3,"abc",0,3,"bar",1,1,0,0,9>>,
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
    ?_assertEncode(<<15,0,0,0,6,"<xml/>">>, {xmldoc,"<xml/>"}).

encode_typed_object_test() ->
    ?_assertEncode(<<16,0,5,"class",0,1,"a",1,0,0,2,"ab",1,1,0,0,9>>,
		   {object,<<"class">>,[{a, false}, {ab, true}]}).

encode_avmplus_test() ->
    ?_assertEncode(<<17,6,(8 bsl 1 bor 1),"testtest">>,
		   {avmplus, <<"testtest">>}).

decode_number_test() ->
    ?_assertDecode(0.0, <<0,0.0:64/big-float>>).

decode_string_test() ->
    ?_assertDecode(<<"abcdef">>, <<2,0,6,"abcdef">>).

decode_booleans_test_() ->
    [?_assertDecode(false, <<1,0>>),
     ?_assertDecode(true, <<1,1>>)].

decode_avmplus_test() ->
    ?_assertDecode(1234, <<17,4,137,82>>).

decode_references_test() ->
    ?_assertDecode([{object,[]},{object,[]}], <<10,0,0,0,2,3,0,0,9,7,0,1>>).

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
    ?_assertCodec(null).

codec_undefined_test() ->
    ?_assertCodec(undefined).

codec_unsupported_test() ->
    ?_assertCodec(unsupported).

codec_string_test() ->
    ?_assertCodec(<<"quickfox">>).

codec_long_string_test() ->
    ?_assertCodec(list_to_binary(string:copies("lazydog", 18000))).

codec_object_test() ->
    ?_assertCodec({object,[{abc,5.5},{def,false},{xyz,null}]}).

codec_arrays_test_() ->
    [?_assertCodec([]),
     ?_assertCodec([1.0,2.0,3.0]),
     ?_assertCodec([<<"a">>,<<"b">>,[<<"x">>,true,false]])].

codec_dates_test_() ->
    [?_assertCodec({date,0.0,0}),
     ?_assertCodec({date,5000.0,0})].

codec_xmldoc_test() ->
    ?_assertCodec({xmldoc,<<"<xml></xml>">>}).

codec_typed_object_test() ->
    ?_assertCodec({object,<<"class">>,[{m1,true},{m2,false}]}).

codec_nested_array_of_dates_test() ->
    D1 = {date,789456.21,0},
    D2 = {date,123456.45,0},
    ?_assertCodec([[D2,D2],D1,[D2,D2],[D1,D2],[D2,[D1,D2],D1],D2,D1]).
