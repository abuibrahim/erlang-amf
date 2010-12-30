%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc AMF3 Encoding and Decoding.

-module(amf3).
-author('ruslan@babayev.com').

-export([encode/1, decode/1, decode/4]).

-define(UNDEFINED, 16#00).
-define(NULL,      16#01).
-define(FALSE,     16#02).
-define(TRUE,      16#03).
-define(INTEGER,   16#04).
-define(DOUBLE,    16#05).
-define(STRING,    16#06).
-define(XMLDOC,    16#07).
-define(DATE,      16#08).
-define(ARRAY,     16#09).
-define(OBJECT,    16#0A).
-define(XML,       16#0B).
-define(BYTEARRAY, 16#0C).

%% IEEE 754 special values
-define(POS_INFINITY, <<0:1,16#7FF:11,0:52>>).
-define(NEG_INFINITY, <<1:1,16#7FF:11,0:52>>).
-define(QNAN,         <<0:1,16#7FF:11,1:1,0:51>>).
-define(SNAN,         <<0:1,16#7FF:11,0:1,1:51>>).

%% @type members() = [{atom() | binary(), amf3()}].
%% @type object() = {object, Class::binary(), members()}.
%% @type date() = {date, MilliSecs::float(), TimeZone::integer()}.
%% @type xmldoc() = {xmldoc, Contents::binary()}.
%% @type xml() = {xml, Contents::binary()}.
%% @type array() = [{Key::binary(), Value::amf3()} | amf3()].
%% @type bytearray() = {bytearray, Bytes::binary()}.
%% @type double() = float() | '+infinity' | '-infinity' | 'qNaN' | 'sNaN'.
%% @type amf3() = undefined | null | false | true | integer() |
%%                double() | binary() | xmldoc() | date() | array() |
%%                object() | xml() | bytearray().
%% @type refs() = //stdlib/gb_trees:gb_tree()
-record(trait, {class, is_dynamic, is_externalizable, property_names}).

-define(IS_SET(Byte, Flag), ((Byte) band Flag) == Flag).

%% @doc Decodes a value.
%% @spec decode(binary()) -> {Value::amf3(), Rest::binary()}
decode(Data) ->
    Empty = gb_trees:empty(),
    {AMF, Rest, _, _, _} = decode(Data, Empty, Empty, Empty),
    {AMF, Rest}.

%% @doc Decodes a value.
%% @spec decode(Bytes::binary(), Strings, Objects, Traits) ->
%%       {Value::amf3(), Rest::binary(), Strings, Objects, Traits}
%%       Strings = refs()
%%       Objects = refs()
%%       Traits = refs()
%% @throws {unknown_externalized_class, term()}
decode(<<?UNDEFINED, Rest/binary>>, Strings, Objects, Traits) ->
    {undefined, Rest, Strings, Objects, Traits};
decode(<<?NULL, Rest/binary>>, Strings, Objects, Traits) ->
    {null, Rest, Strings, Objects, Traits};
decode(<<?FALSE, Rest/binary>>, Strings, Objects, Traits) ->
    {false, Rest, Strings, Objects, Traits};
decode(<<?TRUE, Rest/binary>>, Strings, Objects, Traits) ->
    {true, Rest, Strings, Objects, Traits};
decode(<<?INTEGER, Data/binary>>, Strings, Objects, Traits) ->
    {UInt29, Rest} = decode_uint29(Data),
    {uint29_to_int29(UInt29), Rest, Strings, Objects, Traits};
decode(<<?DOUBLE, Data:8/binary, Rest/binary>>, Strings, Objects, Traits) ->
    {decode_double(Data), Rest, Strings, Objects, Traits};
decode(<<?STRING, Data/binary>>, Strings, Objects, Traits) ->
    {String, Rest, Strings1} = decode_string(Data, Strings),
    {String, Rest, Strings1, Objects, Traits};
decode(<<?XMLDOC, Data/binary>>, Strings, Objects, Traits) ->
    {String, Rest, Strings1} = decode_string(Data, Strings),
    {{xmldoc, String}, Rest, Strings1, Objects, Traits};
decode(<<?DATE, Data/binary>>, Strings, Objects, Traits) ->
    case decode_by_reference(Data, Objects) of
	{value, Date, Rest} ->
	    {Date, Rest, Strings, Objects, Traits};
	{inline, _, <<TS:64/float, Rest/binary>>} ->
	    Date = {date, TS, 0},
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, Date, Objects),
	    {Date, Rest, Strings, Objects1, Traits}
    end;
decode(<<?ARRAY, Data/binary>>, Strings, Objects, Traits) ->
    case decode_by_reference(Data, Objects) of
	{value, Array, Rest} ->
	    {Array, Rest, Strings, Objects, Traits};
	{inline, Len, Rest} ->
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, [], Objects),
	    {Associative, Rest2, Strings2, Objects2, Traits2} =
		decode_assoc(Rest, Strings, Objects1, Traits, []),
	    {Dense, Rest3, Strings3, Objects3, Traits3} =
		decode_dense(Len, Rest2, Strings2, Objects2, Traits2, []),
	    Array = Associative ++ Dense,
	    Objects4 = gb_trees:update(Key, Array, Objects3),
	    {Array, Rest3, Strings3, Objects4, Traits3}
    end;
decode(<<?OBJECT, Data/binary>>, Strings, Objects, Traits) ->
    case decode_by_reference(Data, Objects) of
	{value, Object, Rest} ->
	    {Object, Rest, Strings, Objects, Traits};
	{inline, Len, Rest} ->
	    {Trait, Rest1, Strings1, Traits1} =
		decode_trait(Len, Rest, Strings, Traits),
	    Object0 = {object, Trait#trait.class, []},
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, Object0, Objects),
	    {Object, Rest2, Strings2, Objects2, Traits2} =
		decode_object(Trait, Rest1, Strings1, Objects1, Traits1),
	    Objects3 = gb_trees:update(Key, Object, Objects2),
	    {Object, Rest2, Strings2, Objects3, Traits2}
    end;
decode(<<?XML, Data/binary>>, Strings, Objects, Traits) ->
    {String, Rest, Strings1} = decode_string(Data, Strings),
    {{xml, String}, Rest, Strings1, Objects, Traits};
decode(<<?BYTEARRAY, Data/binary>>, Strings, Objects, Traits) ->
    {ByteArray, Rest, Objects1} =  decode_bytearray(Data, Objects),
    {ByteArray, Rest, Strings, Objects1, Traits}.

%% @doc Decodes IEEE-754 double precision floating-point number.
%% @spec decode_double(binary()) -> double()
decode_double(?POS_INFINITY)              -> '+infinity';
decode_double(?NEG_INFINITY)              -> '-infinity';
decode_double(<<_:1,16#7FF:11,1:1,_:51>>) -> 'qNaN';
decode_double(<<_:1,16#7FF:11,0:1,_:51>>) -> 'sNaN';
decode_double(<<Num:64/float>>)           -> Num.

%% @doc Decodes a value stored by reference.
%% @spec decode_by_reference(binary(), Tree::refs()) ->
%%       {value, Value::amf3(), binary()} | {inline, Length::integer, binary()}
decode_by_reference(Bin, Tree) ->
    case decode_uint29(Bin) of
	{Int, Rest} when ?IS_SET(Int, 1) ->
	    {inline, Int bsr 1, Rest};
	{Int, Rest} ->
	    {value, gb_trees:get(Int bsr 1, Tree), Rest}
    end.

%% @doc Decodes an unsigned 29-bit integer.
%% @spec decode_uint29(binary()) -> {integer(), Rest::binary()}
decode_uint29(Data) ->
    decode_uint29(Data, 0, 0).

%% @doc Decodes an unsigned 29-bit Integer.
%% @spec decode_uint29(binary(), integer(), integer()) -> {integer(), binary()}
decode_uint29(<<1:1, Num:7, Data/binary>>, Result, N) when N < 3 ->
    decode_uint29(Data, (Result bsl 7) bor Num, N + 1);
decode_uint29(<<0:1, Num:7, Data/binary>>, Result, N) when N < 3 ->
    {(Result bsl 7) bor Num, Data};
decode_uint29(<<Byte, Data/binary>>, Result, _N) ->
    {(Result bsl 8) bor Byte, Data}.

%% @doc Converts an unsigned 29-bit Integer to a signed one.
%% @spec uint29_to_int29(integer()) -> integer() 
uint29_to_int29(UInt29) ->
    case UInt29 >= (1 bsl 28) of
	true ->
	    UInt29 - (1 bsl 29);
	false ->
	    UInt29
    end.

%% @doc Decodes a String.
%% @spec decode_string(binary(), refs()) ->
%%       {String::string(), Rest::binary(), Strings::refs()}
decode_string(Data, Strings) ->
    case decode_by_reference(Data, Strings) of
	{value, String, Rest} ->
	    {String, Rest, Strings};
	{inline, Len, Rest} ->
	    <<String:Len/binary, Rest1/binary>> = Rest,
	    {String, Rest1, insert_string(String, Strings)}
    end.

%% @doc Decodes a Byte Array.
%% @spec decode_bytearray(binary(), refs()) ->
%%       {ByteArray::bytearray(), Rest::binary(), Objects::refs()}
decode_bytearray(Data, Objects) ->
    case decode_by_reference(Data, Objects) of
	{value, ByteArray, Rest} ->
	    {ByteArray, Rest, Objects};
	{inline, Len, Rest} ->
	    <<Bytes:Len/binary, Rest1/binary>> = Rest,
	    Key = gb_trees:size(Objects),
	    ByteArray = {bytearray, Bytes},
	    {ByteArray, Rest1, gb_trees:insert(Key, ByteArray, Objects)}
    end.

%% @doc Decodes the associative portion of Arrays and Objects.
%% @spec decode_assoc(binary(), Strings, Objects, Traits, list()) ->
%%       {Elements, Rest::binary(), Strings, Objects, Traits}
%%       Elements = [{binary(), amf3()}]
%%       Strings = refs()
%%       Objects = refs()
%%       Traits = refs()
decode_assoc(Data, Strings, Objects, Traits, Acc) ->
    case decode_string(Data, Strings) of
	{<<>>, Rest, Strings1} ->
	    {lists:reverse(Acc), Rest, Strings1, Objects, Traits};
	{Key, Rest, Strings1} ->
	    {Value, Rest1, S2, O2, T2} =
		decode(Rest, Strings1, Objects, Traits),
	    decode_assoc(Rest1, S2, O2, T2, [{Key, Value} | Acc])
    end.

%% @doc Decodes the dense portion of Arrays and Objects.
%% @spec decode_dense(integer(), binary(), Strings, Objects, Traits, Acc) ->
%%       {Elements::list(), Rest::binary(), Strings, Objects, Traits}
%%       Strings = refs()
%%       Objects = refs()
%%       Traits = refs()
decode_dense(0, Rest, Strings, Objects, Traits, Acc) ->
    {lists:reverse(Acc), Rest, Strings, Objects, Traits};
decode_dense(N, Data, Strings, Objects, Traits, Acc) ->
    {Element, Rest, S1, O1, T1} = decode(Data, Strings, Objects, Traits),
    decode_dense(N - 1, Rest, S1, O1, T1, [Element | Acc]).

%% @doc Decodes an Object trait.
%% @spec decode_trait(integer(), binary(), Strings, Traits) ->
%%       {#trait{}, Rest::binary(), Strings, Traits}
%%       Strings = refs()
%%       Traits = refs()
decode_trait(Ref, Data, Strings, Traits) ->
    case Ref band 1 of
	1 ->
	    {ClassName, Rest, Strings1} = decode_string(Data, Strings),
	    {PropertyNames, Rest1, Strings2} =
		decode_strings_as_atoms(Ref bsr 3, Rest, Strings1, []),
	    Trait = #trait{class = ClassName,
			   is_externalizable = ?IS_SET(Ref, 2),
			   is_dynamic = ?IS_SET(Ref, 4),
			   property_names = PropertyNames},
	    Key = gb_trees:size(Traits),
	    Traits1 = gb_trees:insert(Key, Trait, Traits),
	    {Trait, Rest1, Strings2, Traits1};
	0 ->
	    {gb_trees:get(Ref bsr 1, Traits), Data, Strings, Traits}
    end.

%% @doc Decodes a sequence of Strings as atoms.
%% @spec decode_strings_as_atoms(integer(), binary(), Strings, Acc) ->
%%       {Atoms::list(), Rest::binary(), Strings}
%%       Strings = refs()
decode_strings_as_atoms(0, Rest, Strings, Acc) ->
    {lists:reverse(Acc), Rest, Strings};
decode_strings_as_atoms(N, Data, Strings, Acc) ->
    {String, Rest, Strings1} = decode_string(Data, Strings),
    Atom = binary_to_atom(String, utf8),
    decode_strings_as_atoms(N - 1, Rest, Strings1, [Atom | Acc]).

%% @doc Decodes an Object.
%% @spec decode_object(#trait{}, binary(), Strings, Objects, Traits) ->
%%       {Object::object(), Rest::binary(), Strings, Objects, Traits}
%%       Strings = refs()
%%       Objects = refs()
%%       Traits = refs()
%% @throws {unknown_externalized_class, term()}
decode_object(Trait, Data, Strings, Objects, Traits)
  when Trait#trait.is_externalizable ->
    case Trait#trait.class of
	<<"flex.messaging.io.ArrayCollection">> ->
	    decode(Data, Strings, Objects, Traits);
	<<"flex.messaging.io.ObjectProxy">> ->
	    decode(Data, Strings, Objects, Traits);
	<<"flex.messaging.io.SerializationProxy">> ->
	    decode(Data, Strings, Objects, Traits);
	Class ->
	    Module = external_module(Class),
	    {Members, Rest, Strings1, Objects1, Traits1} =
		Module:decode_members(Data, Strings, Objects, Traits),
	    {{object, Class, Members}, Rest, Strings1, Objects1, Traits1}
    end;
decode_object(Trait, Data, Strings, Objects, Traits) ->
    Len = length(Trait#trait.property_names),
    {PropertyValues, Rest1, Strings1, Objects1, Traits1} =
	decode_dense(Len, Data, Strings, Objects, Traits, []),
    Sealed = lists:zip(Trait#trait.property_names, PropertyValues),
    {Dynamic, Rest2, Strings2, Objects2, Traits2} =
	case Trait#trait.is_dynamic of
	    true ->
		decode_assoc(Rest1, Strings1, Objects1, Traits1, []);
	    false ->
		{[], Rest1, Strings1, Objects1, Traits1}
	end,
    Object = {object, Trait#trait.class, Sealed ++ Dynamic},
    {Object, Rest2, Strings2, Objects2, Traits2}.

%% @doc Maps the externalized class name to its handler module.
%% @spec external_module(binary()) -> Module::atom()
%% @throws {unknown_externalized_class, term()}
external_module(<<"DSA">>) -> amf_AsyncMessage;
external_module(<<"DSC">>) -> amf_CommandMessage;
external_module(<<"DSK">>) -> amf_AcknowledgeMessage;
external_module(Name) ->
    throw({unknown_externalized_class, Name}).

%% @doc Encodes a value.
%% @spec encode(Value::amf3()) -> binary()
encode(AMF3) ->
    Empty = gb_trees:empty(),
    {Bin, _Strings, _Objects, _Traits} = encode(AMF3, Empty, Empty, Empty),
    Bin.

%% @doc Encodes a value.
%% @spec encode(Value::amf3(), Strings, Objects, Traits) ->
%%       {binary(), Strings, Objects, Traits}
%%       Strings = refs()
%%       Objects = refs()
%%       Traits = refs()
%% @throws {bad_range, term()} | bad_member | {bad_property, term()}
encode(undefined, Strings, Objects, Traits) ->
    {<<?UNDEFINED>>, Strings, Objects, Traits};
encode(null, Strings, Objects, Traits) ->
    {<<?NULL>>, Strings, Objects, Traits};
encode(false, Strings, Objects, Traits) ->
    {<<?FALSE>>, Strings, Objects, Traits};
encode(true, Strings, Objects, Traits) ->
    {<<?TRUE>>, Strings, Objects, Traits};
encode(Integer, Strings, Objects, Traits) when is_integer(Integer) ->
    Bin = encode_int29(Integer),
    {<<?INTEGER, Bin/binary>>, Strings, Objects, Traits};
encode('+infinity', Strings, Objects, Traits) ->
    {<<?DOUBLE, ?POS_INFINITY/binary>>, Strings, Objects, Traits};
encode('-infinity', Strings, Objects, Traits) ->
    {<<?DOUBLE, ?NEG_INFINITY/binary>>, Strings, Objects, Traits};
encode('qNaN', Strings, Objects, Traits) ->
    {<<?DOUBLE, ?QNAN/binary>>, Strings, Objects, Traits};
encode('sNaN', Strings, Objects, Traits) ->
    {<<?DOUBLE, ?SNAN/binary>>, Strings, Objects, Traits};
encode(Double, Strings, Objects, Traits) when is_float(Double) ->
    {<<?DOUBLE, Double/float>>, Strings, Objects, Traits};
encode(String, Strings, Objects, Traits) when is_binary(String) ->
    {Bin, Strings1} = encode_string(String, Strings),
    {<<?STRING, Bin/binary>>, Strings1, Objects, Traits};
encode({xmldoc, String}, Strings, Objects, Traits) ->
    {Bin, Strings1} = encode_string(String, Strings),
    {<<?XMLDOC, Bin/binary>>, Strings1, Objects, Traits};
encode({date, TS, TZ}, Strings, Objects, Traits) ->
    case encode_by_reference({date, TS, TZ}, gb_trees:iterator(Objects)) of
	{ok, Bin} ->
	    {<<?DATE, Bin/binary>>, Strings, Objects, Traits};
	{error, not_found} ->
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, {date, TS, TZ}, Objects),
	    {<<?DATE, 1, TS:64/float>>, Strings, Objects1, Traits}
    end;
encode(Array, Strings, Objects, Traits) when is_list(Array) ->
    case encode_by_reference(Array, gb_trees:iterator(Objects)) of
	{ok, Bin} ->
	    {<<?ARRAY, Bin/binary>>, Strings, Objects, Traits};
	{error, not_found} ->
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, Array, Objects),
	    F = fun({K, _V}) when is_binary(K) -> true;
		   (_V) -> false
		end,
	    {AssocList, DenseList} = lists:partition(F, Array),
	    {AssocBin, Strings2, Objects2, Traits2} =
		encode_assoc(AssocList, <<>>, Strings, Objects1, Traits),
	    DenseLen = encode_uint29(length(DenseList) bsl 1 bor 1),
	    {DenseBin, Strings3, Objects3, Traits3} =
		encode_dense(DenseList, <<>>, Strings2, Objects2, Traits2),
	    Bin = <<?ARRAY, DenseLen/binary, AssocBin/binary,
		    DenseBin/binary>>,
	    {Bin, Strings3, Objects3, Traits3}
    end;
encode({object, Class, Members} = Object, Strings, Objects, Traits) ->
    case encode_by_reference(Object, gb_trees:iterator(Objects)) of
	{ok, Bin} ->
	    {<<?OBJECT, Bin/binary>>, Strings, Objects, Traits};
	{error, not_found} ->
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, Object, Objects),
	    F = fun({K, _}) when is_atom(K)   -> true;
		   ({K, _}) when is_binary(K) -> false
		end,
	    {SealedMembers, DynamicMembers} =
		try lists:partition(F, Members)
		catch
		    error:function_clause ->
			throw(bad_member)
		end,
	    {SealedKeys, SealedVals} = lists:unzip(SealedMembers),
	    Trait = #trait{class = Class,
			   is_dynamic = (length(DynamicMembers) > 0),
			   is_externalizable = false, % TODO: handle ext
			   property_names = SealedKeys
			  },
	    {TraitBin, Strings1, Traits1} =
		encode_trait(Trait, Strings, Traits),
	    {Sealed, Strings2, Objects2, Traits2} =
		encode_dense(SealedVals, <<>>, Strings1, Objects1, Traits1),
	    {Dynamic, Strings3, Objects3, Traits3} =	    
		case Trait#trait.is_dynamic of
		    true ->
			encode_assoc(DynamicMembers, <<>>,
				     Strings2, Objects2, Traits2);
		    false ->
			{<<>>, Strings2, Objects2, Traits2}
		end,
	    Bin = <<?OBJECT, TraitBin/binary, Sealed/binary, Dynamic/binary>>,
	    {Bin, Strings3, Objects3, Traits3}
    end;
encode({xml, String}, Strings, Objects, Traits) ->
    {Bin, Strings1} = encode_string(String, Strings),
    {<<?XML, Bin/binary>>, Strings1, Objects, Traits};
encode({bytearray, _Bytes} = ByteArray, Strings, Objects, Traits) ->
    {Bin, Objects1} = encode_bytearray(ByteArray, Objects),
    {<<?BYTEARRAY, Bin/binary>>, Strings, Objects1, Traits}.

%% @doc Encodes a signed 29-bit Integer.
%% @spec encode_int29(integer()) -> binary()
encode_int29(I) when I >= -16#10000000, I < 0 ->
    encode_uint29(16#20000000 + I);
encode_int29(I) when I =< 16#0FFFFFFF ->
    encode_uint29(I);
encode_int29(I) ->
    throw({bad_range, I}).

%% @doc Encodes an unsigned 29-bit Integer.
%% @spec encode_uint29(integer()) -> binary()
encode_uint29(I) when I >= 16#00000000, I =< 16#0000007F ->
    <<I>>;
encode_uint29(I) when I >= 16#00000080, I =< 16#00003FFF ->
    X1 = 16#80 bor (I bsr 7),
    X2 = I band 16#7F,
    <<X1, X2>>;
encode_uint29(I) when I >= 16#00004000, I =< 16#001FFFFF ->
    X1 = 16#80 bor (I bsr 14),
    X2 = 16#80 bor (I bsr 7),
    X3 = I band 16#7F,
    <<X1, X2, X3>>;
encode_uint29(I) when I >= 16#00200000, I =< 16#1FFFFFFF ->
    X1 = 16#80 bor (I bsr 22),
    X2 = 16#80 bor (I bsr 15),
    X3 = 16#80 bor (I bsr 8),
    X4 = I band 16#FF,
    <<X1, X2, X3, X4>>;
encode_uint29(I) ->
    throw({bad_range, I}).

%% @doc Encodes a String.
%% @spec encode_string(binary(), refs()) -> {binary(), refs()}
encode_string(String, Strings) ->
    case encode_by_reference(String, gb_trees:iterator(Strings)) of
	{ok, Bin} ->
	    {Bin, Strings};
	{error, not_found} ->
	    Strings1 = insert_string(String, Strings),
	    Ref = encode_uint29(size(String) bsl 1 bor 1),
	    {<<Ref/binary, String/binary>>, Strings1}
    end.

%% @doc Encodes a Byte Array.
%% @spec encode_bytearray(bytearray(), refs()) -> {binary(), refs()}
encode_bytearray({bytearray, Bytes} = ByteArray, Objects) ->
    case encode_by_reference(ByteArray, gb_trees:iterator(Objects)) of
	{ok, Bin} ->
	    {Bin, Objects};
	{error, not_found} ->
	    Key = gb_trees:size(Objects),
	    Objects1 = gb_trees:insert(Key, ByteArray, Objects),
	    Ref = encode_uint29(size(Bytes) bsl 1 bor 1),
	    {<<Ref/binary, Bytes/binary>>, Objects1}
    end.

%% @doc Encodes a value by reference.
%% @spec encode_by_reference(amf3(), term()) ->
%%       {ok, binary()} | {error, not_found}
encode_by_reference(Value, Iterator0) ->
    case gb_trees:next(Iterator0) of
	{Key, Value, _} when is_record(Value, trait) ->
	    %% Obj is inline, Trait is a 27bit reference.
	    {ok, encode_uint29(Key bsl 2 bor 1)};
	{Key, Value, _} ->
	    {ok, encode_uint29(Key bsl 1)};
	{_, _, Iterator1} ->
	    encode_by_reference(Value, Iterator1);
	none ->
	    {error, not_found}
    end.

%% @doc Encodes the associative portion of Arrays and Objects.
%% @spec encode_assoc(Elements, binary(), Strings, Objects, Traits) ->
%%       {binary(), Strings, Objects, Traits}
%%       Elements = [{binary(), amf3()}]
%%       Strings = refs()
%%       Objects = refs()
%%       Traits = refs()
encode_assoc([{Key, Value} | Rest], Acc, Strings, Objects, Traits)
  when is_binary(Key) ->
    {KeyBin, Strings1} = encode_string(Key, Strings),
    {ValBin, Strings2, Objects2, Traits2} =
	encode(Value, Strings1, Objects, Traits),
    Acc1 = <<Acc/binary, KeyBin/binary, ValBin/binary>>,
    encode_assoc(Rest, Acc1, Strings2, Objects2, Traits2);
encode_assoc([], Acc, Strings, Objects, Traits) ->
    {EmptyString, _} = encode_string(<<>>, Strings),
    {<<Acc/binary, EmptyString/binary>>, Strings, Objects, Traits};
encode_assoc([Property | _Rest], _Acc, _Strings, _Objects, _Traits) ->
    throw({bad_property, Property}).

%% @doc Encodes the dense portion of Arrays and Objects.
%% @spec encode_dense(Elements, binary(), Strings, Objects, Traits) ->
%%       {binary(), Strings, Objects, Traits}
%%       Elements = [amf3()]
%%       Strings = refs()
%%       Objects = refs()
%%       Traits = refs()
encode_dense([], Acc, Strings, Objects, Traits) ->
    {Acc, Strings, Objects, Traits};
encode_dense([Element | Rest], Acc, Strings, Objects, Traits) ->
    {Bin, Strings1, Objects1, Traits1} =
	encode(Element, Strings, Objects, Traits),
    encode_dense(Rest, <<Acc/binary, Bin/binary>>,
		 Strings1, Objects1, Traits1).

%% @doc Encodes an Object trait.
%% @spec encode_trait(#trait{}, Strings, Traits) -> {binary(), Strings, Traits}
%%       Strings = refs()
%%       Traits = refs()
encode_trait(Trait, Strings, Traits) ->
    case encode_by_reference(Trait, gb_trees:iterator(Traits)) of
	{ok, Bin} ->
	    {Bin, Strings, Traits};
	{error, not_found} ->
	    Key = gb_trees:size(Traits),
	    Traits1 = gb_trees:insert(Key, Trait, Traits),
	    {Class, Strings1} = encode_string(Trait#trait.class, Strings),
	    Ref0 = length(Trait#trait.property_names) bsl 4,
	    Ref1 = Ref0 bor 2#011, % non-ext, trait-inline, obj-inline
	    Ref2 = case Trait#trait.is_dynamic of
		       true ->
			   Ref1 bor 2#1000;
		       false ->
			   Ref1
		   end,
	    RefBin = encode_uint29(Ref2),
	    {PropNames, Strings2} =
		encode_atoms_as_strings(Trait#trait.property_names, Strings1),
	    Bin = <<RefBin/binary, Class/binary, PropNames/binary>>,
	    {Bin, Strings2, Traits1}
    end.

%% @doc Encodes a list of atoms as Strings.
%% @spec encode_atoms_as_strings(Atoms, Strings) -> {binary(), Strings}
%%       Atoms = [atom()]
%%       Strings = refs()
encode_atoms_as_strings(Atoms, Strings) ->
    encode_atoms_as_strings(Atoms, <<>>, Strings).

%% @doc Encodes a list of atoms as Strings.
%% @spec encode_atoms_as_strings(Atoms, binary(), Strings) ->
%%       {binary(), Strings}
%%       Atoms = [atom()]
%%       Strings = refs()
encode_atoms_as_strings([], Acc, Strings) ->
    {Acc, Strings};
encode_atoms_as_strings([Atom | Rest], Acc, Strings) ->
    {Bin, Strings1} = encode_string(atom_to_binary(Atom, utf8), Strings),
    encode_atoms_as_strings(Rest, <<Acc/binary, Bin/binary>>, Strings1).

%% @doc Inserts String into the reference table unless String is empty.
%%      Note, empty Strings are never sent by reference.
%% @spec insert_string(binary(), refs()) -> refs()
insert_string(<<>>, Strings) ->
    Strings;
insert_string(String, Strings) ->
    gb_trees:insert(gb_trees:size(Strings), String, Strings).
