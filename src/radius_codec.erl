-module(radius_codec).

-export([decode_packet/2, attribute_value/2, identify_packet/1]).
-export([encode_response/3, encode_attributes/1]).
-export([encode_request/2]).

-include("radius.hrl").

%% @doc Decode binary RADIUS packet.
-spec decode_packet(Bin :: binary(), Secret :: string()) ->
    {ok, Packet :: #radius_packet{}} | {error, Reason :: term()}.
decode_packet(Bin, Secret) ->
    try
        <<?RADIUS_PACKET>> = Bin,
        case byte_size(Attrs) >= (Length - 20) of
            true ->
                A = decode_attributes(Attrs, []),
                Packet = #radius_packet{
                    code = Code,
                    ident = Ident,
                    auth = Auth,
                    attrs = A
                },
                case attribute_value("Message-Authenticator", A) of
                    undefined ->
                        {ok, Packet};
                    Value ->
                        A1 = lists:keyreplace("Message-Authenticator", 1, A, {"Message-Authenticator", <<0:128>>}),
                        {ok, A2} = encode_attributes(A1),
                        Packet1 = [Code, Ident, <<Length:16>>, Auth, A2],
                        case crypto:hmac(md5, Secret, Packet1) =:= Value of
                            true ->
                                {ok, Packet};
                            false ->
                                {error, invalid_message_authenticator}
                        end
                end;
            false ->
                {error, packet_size_mismatch}
        end
    catch
        _:Reason ->
            {error, Reason}
    end.

%% @doc Returns the value of specified RADIUS attribute
-spec attribute_value(Code :: non_neg_integer() | tuple() | string(), Packet :: #radius_packet{} | [proplists:property()]) ->
    undefined | term().
attribute_value(Code, Packet) when is_record(Packet, radius_packet) ->
    attribute_value(Code, Packet#radius_packet.attrs);
attribute_value(Code, Attrs) when is_list(Attrs) ->
    case radius_dict:lookup_attribute(Code) of
        not_found ->
            undefined;
        #attribute{code = Code1, name = Name} ->
            lookup_value(Code1, Name, Attrs)
    end.

-spec encode_request(#radius_packet{}, string()) -> binary().
encode_request(Request, Secret) ->
    Code = Request#radius_packet.code,
    Ident = Request#radius_packet.ident,
    {ok, Attrs} = encode_attributes(Request#radius_packet.attrs),
    Length = <<(20 + byte_size(Attrs)):16>>,
    Auth = erlang:md5([Code, Ident, Length, <<0:128>>, Attrs, Secret]),
    list_to_binary([Code, Ident, Length, Auth, Attrs]).

%% @doc Returns type of the request.
-spec identify_packet(Type :: non_neg_integer()) -> atom() | integer().
identify_packet(?ACCESS_REQUEST) ->
    'Access-Request';
identify_packet(?ACCOUNTING_REQUEST) ->
    'Accounting-Request';
identify_packet(?ACCESS_CHALLENGE) ->
    'Access-Challenge';
identify_packet(?DISCONNECT_REQUEST) ->
    'Disconnect-Request';
identify_packet(?DISCONNECT_ACK) ->
    'Disconnect-ACK';
identify_packet(?DISCONNECT_NAK) ->
    'Disconnect-NAK';
identify_packet(?COA_REQUEST) ->
    'CoA-Request';
identify_packet(?COA_ACK) ->
    'CoA-ACK';
identify_packet(?COA_NAK) ->
    'CoA-NAK';
identify_packet(Type) ->
    Type.

%% @doc Encode RADIUS packet to binary
-spec encode_response(Request :: #radius_packet{},
                      Response :: #radius_packet{},
                      Secret :: string()) ->
    {ok, binary()} | {error, Reason :: term()}.
encode_response(Request, Response, Secret) ->
    #radius_packet{code = C, attrs = A} = Response,
    Code = <<C:8>>,
    Ident = Request#radius_packet.ident,
    ReqAuth = Request#radius_packet.auth,
    case attribute_value("EAP-Message", A) of
        undefined ->
            case encode_attributes(A) of
                {ok, Attrs} ->
                    Length = <<(20 + byte_size(Attrs)):16>>,
                    Auth = erlang:md5([Code, Ident, Length, ReqAuth, Attrs, Secret]),
                    Data = list_to_binary([Code, Ident, Length, Auth, Attrs]),
                    {ok, Data};
                 {error, Reason} ->
                    {error, Reason}
            end;
        _Value ->
            try
                A1 = A ++ [{"Message-Authenticator", <<0:128>>}],
                {ok, A2} = encode_attributes(A1),

                Length = <<(20 + byte_size(A2)):16>>,
                Packet = list_to_binary([Code, Ident, Length, ReqAuth, A2]),
                MA = crypto:hmac(md5, Secret, Packet),

                A3 = A ++ [{"Message-Authenticator", MA}],
                {ok, A4} = encode_attributes(A3),

                Auth = erlang:md5([Code, Ident, Length, ReqAuth, A4, Secret]),
                Data = list_to_binary([Code, Ident, Length, Auth, A4]),
                {ok, Data}
            catch
                _:Reason ->
                    {error, Reason}
            end
    end.

%% @doc Encode list of RADIUS attributes to binary
-spec encode_attributes(Attrs :: [proplists:property()]) ->
    {ok, binary()} | {error, Reason :: term()}.
encode_attributes(Attrs) ->
    try
        Bin = encode_attributes(Attrs, []),
        {ok, Bin}
    catch
        _:Reason ->
            {error, Reason}
    end.

%%
%% Internal functions
%%
decode_attributes(<<>>, Attrs) ->
    lists:reverse(lists:flatten(Attrs));
decode_attributes(Bin, Attrs) ->
    {Attr, Rest} = decode_attribute(Bin),
    decode_attributes(Rest, [Attr | Attrs]).

decode_attribute(<<Type:8, Length:8, Rest/binary>>) ->
    case Type of
        ?VENDOR_SPECIFIC ->
            L = Length - 2,
            <<Attr:L/binary-unit:8, Rest1/binary>> = Rest,
            {decode_vendor_attributes(Attr), Rest1};
        _ ->
            case radius_dict:lookup_attribute(Type) of
                not_found ->
                    {Value, Rest1} = decode_value(Rest, Length - 2),
                    {{Type, Value}, Rest1};
                A ->
                    {Value, Rest1} = decode_value(Rest, Length - 2, A#attribute.type),
                    {{A#attribute.name, Value}, Rest1}
            end
    end.

decode_vendor_attributes(<<VendorId:4/integer-unit:8, Rest/binary>>) ->
    decode_vendor_attribute(VendorId, Rest, []).

decode_vendor_attribute(_, <<>>, Acc) -> Acc;
decode_vendor_attribute(VendorId, <<Id, Length:8, Value/binary>>, Acc) ->
    case radius_dict:lookup_attribute({VendorId, Id}) of
        not_found ->
            {V, Rest1} = decode_value(Value, Length - 2),
            decode_vendor_attribute(VendorId, Rest1, [{{VendorId, Id}, V} | Acc]);
        A ->
            {V, Rest1} = decode_value(Value, Length - 2, A#attribute.type),
            decode_vendor_attribute(VendorId, Rest1, [{A#attribute.name, V} | Acc])
    end.

%% 0-253 octets
decode_value(Bin, Length, string) ->
    <<Value:Length/binary, Rest/binary>> = Bin,
    {binary_to_list(Value), Rest};
%% 32 bit value in big endian order (high byte first)
decode_value(Bin, Length, integer) ->
    <<Value:Length/integer-unit:8, Rest/binary>> = Bin,
    {Value, Rest};
%% 32 bit value in big endian order - seconds since 00:00:00 GMT, Jan. 1, 1970
decode_value(Bin, Length, date) ->
    decode_value(Bin, Length, integer);
%% 4 octets in network byte order
decode_value(Bin, Length, ipaddr) ->
    <<Value:Length/binary, Rest/binary>> = Bin,
    <<A:8, B:8, C:8, D:8>> = Value,
    {{A, B, C, D}, Rest};
decode_value(Bin, Length, ipv6addr) ->
    <<Value:Length/binary, Rest/binary>> = Bin,
    {list_to_tuple([I || <<I:16>> <= Value]), Rest};
decode_value(Bin, Length, ipv6prefix) ->
    IPLength = Length - 2,
    <<0:8, Prefix:8, IP:IPLength/binary, Rest/binary>> = Bin,
    {{Prefix, list_to_tuple([I || <<I:16>> <= IP])}, Rest};
decode_value(Bin, Length, byte) ->
    <<Value:Length/unsigned-integer-unit:8, Rest/binary>> = Bin,
    {Value, Rest};
decode_value(Bin, Length, _Type) ->
    decode_value(Bin, Length).

decode_value(Bin, Length) ->
    <<Value:Length/binary, Rest/binary>> = Bin,
    {Value, Rest}.

encode_attributes(undefined, []) ->
    <<>>;
encode_attributes([], Bin) ->
    list_to_binary(lists:reverse(Bin));
encode_attributes([A | Attrs], Bin) ->
    encode_attributes(Attrs, [encode_attribute(A) | Bin]).

encode_attribute({Code, Value}) ->
    case radius_dict:lookup_attribute(Code) of
        not_found ->
            throw({error, not_found});
        #attribute{code = Code1, type = Type} ->
            encode_attribute(Code1, Type, Value)
    end.

encode_attribute({Id, Code}, Type, Value) ->
    Bin = encode_value(Value, Type),
    Size = byte_size(Bin),
    VLength = 8 + Size,
    ALength = 2 + Size,
    <<?VENDOR_SPECIFIC:8, VLength:8, Id:32, Code:8, ALength:8, Bin/binary>>;
encode_attribute(Code, Type, Value) ->
    Bin = encode_value(Value, Type),
    Length = 2 + byte_size(Bin),
    <<Code:8, Length:8, Bin/binary>>.

encode_value(Value, _Type) when is_binary(Value) ->
    Value;
encode_value(Value, octets) when is_list(Value) ->
    list_to_binary(Value);
encode_value(Value, string) when is_list(Value) ->
    list_to_binary(Value);
encode_value(Value, integer) when is_list(Value) ->
    try
        IntValue = list_to_integer(Value),
        <<IntValue:32>>
    catch
        _:Reason ->
            throw({error, Reason})
    end;
encode_value(Value, integer) when is_integer(Value) ->
    <<Value:32>>;
encode_value(Value, date) ->
    encode_value(Value, integer);
encode_value(Value, ipaddr) when is_list(Value) ->
    case inet_parse:address(Value) of
        {ok, {A, B, C, D}} ->
            <<A:8, B:8, C:8, D:8>>;
        {error, Reason} ->
            throw({error, Reason})
    end;
encode_value({A, B, C, D}, ipaddr) ->
    <<A:8, B:8, C:8, D:8>>;
encode_value(Value, ipv6addr) when is_list(Value) ->
    case inet_parse:address(Value) of
        {ok, IP} when tuple_size(IP) == 8 ->
            encode_value(IP, ipv6addr);
        {error, Reason} ->
            throw({error, Reason})
    end;
encode_value(Value, ipv6addr) when tuple_size(Value) == 8 ->
    binary:list_to_bin([<<I:16>> || I <- tuple_to_list(Value)]);
encode_value({Prefix, IP}, ipv6prefix) ->
    list_to_binary([<<0:8, Prefix:8>>, encode_value(IP, ipv6addr)]);
encode_value(Value, byte) ->
    <<Value:8/unsigned-integer>>;
encode_value(_Value, _Type) ->
    throw({error, encode_value}).

lookup_value(Code, Name, Attrs) ->
    case lists:keyfind(Code, 1, Attrs) of
        {Code, Value} -> Value;
        false ->
            case lists:keyfind(Name, 1, Attrs) of
                {Name, Value} -> Value;
                false -> undefined
            end
    end.
