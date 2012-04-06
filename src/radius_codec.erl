-module(radius_codec).

-export([decode_packet/2, attribute_value/2, identify_packet/1]).
-export([encode_response/3, encode_attributes/1]).

-include("radius.hrl").

%% @doc Decode binary RADIUS packet.
%% @spec decode_packet(binary(), string()) ->
%%  {ok, radius_packet()} | {error, term()}.
-spec decode_packet(binary(), string()) ->
    {ok, radius_packet()} | {error, term()}.
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
                        case crypto:md5_mac(Secret, Packet1) =:= Value of
                            true ->
                                {ok, Packet};
                            false ->
                                error_logger:error_msg(
                                    "Invalid Message-Authenticator attribute value~n", []),
                                {error, invalid_message_authenticator}
                        end
                end;
            false ->
                error_logger:error_msg(
                    "** Malformed RADIUS packet:~n"
                    "   packet size mismatch: ~p instead of ~p~n",
                    [Length, byte_size(Attrs) + 20]),
                {error, packet_size_mismatch}
        end
    catch
        _:Reason ->
            error_logger:error_msg(
                "** Unable to decode RADIUS packet~n"
                "   for the reason ~p~n", [Reason]),
            {error, Reason}
    end.

attribute_value(Code, Packet) when is_record(Packet, radius_packet) ->
    attribute_value(Code, Packet#radius_packet.attrs);
attribute_value(Code, Attrs) when is_list(Attrs) ->
    case radius_dict:lookup_attribute(Code) of
        not_found ->
            undefined;
        #attribute{code = Code1, name = Name} ->
            lookup_value(Code1, Name, Attrs)
    end.

%% @doc Returns type of the request
%% @spec identify_packet(non_neg_integer()) ->
%%  {ok, atom()} | {unknown, non_neg_integer()}.
-spec identify_packet(non_neg_integer()) ->
    {ok, atom()} | {unknown, non_neg_integer()}.
identify_packet(?ACCESS_REQUEST) ->
    {ok, 'Access-Request'};
identify_packet(?ACCOUNTING_REQUEST) ->
    {ok, 'Accounting-Request'};
identify_packet(?ACCESS_CHALLENGE) ->
    {ok, 'Access-Challenge'};
identify_packet(?DISCONNECT_REQUEST) ->
    {ok, 'Disconnect-Request'};
identify_packet(?DISCONNECT_ACK) ->
    {ok, 'Disconnect-ACK'};
identify_packet(?DISCONNECT_NAK) ->
    {ok, 'Disconnect-NAK'};
identify_packet(?COA_REQUEST) ->
    {ok, 'CoA-Request'};
identify_packet(?COA_ACK) ->
    {ok, 'CoA-ACK'};
identify_packet(?COA_NAK) ->
    {ok, 'CoA-NAK'};
identify_packet(Type) ->
    {unknown, Type}.

%%
%% Internal functions
%%
decode_attributes(<<>>, Attrs) ->
    lists:reverse(Attrs);
decode_attributes(Bin, Attrs) ->
    {ok, Type, Value, Rest} = decode_attribute(Bin),
    decode_attributes(Rest, [{Type, Value} | Attrs]).

decode_attribute(<<?ATTRIBUTE>>) ->
    case Type of
        ?VENDOR_SPECIFIC ->
            decode_vendor_attribute(Rest);
        _ ->
            case radius_dict:lookup_attribute(Type) of
                not_found ->
                    error_logger:warning_msg(
                        "No attribute ~p found in dictionary~n", [Type]),
                    {Value, Rest1} = decode_value(Rest, Length - 2),
                    {ok, Type, Value, Rest1};
                A ->
                    {Value, Rest1} = decode_value(Rest, Length - 2, A#attribute.type),
                    {ok, A#attribute.name, Value, Rest1}
            end
    end.

decode_vendor_attribute(<<?VENDOR_ATTRIBUTE>>) ->
    case radius_dict:lookup_attribute({Id, Type}) of
        not_found ->
            error_logger:warning_msg(
                "No vendor specific attribute ~p found in dictionary~n",
                [{Id, Type}]),
            {Value, Rest1} = decode_value(Rest, Length - 2),
            {ok, {Id, Type}, Value, Rest1};
        A ->
            {Value, Rest1} = decode_value(Rest, Length - 2, A#attribute.type),
            {ok, A#attribute.name, Value, Rest1}
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
decode_value(Bin, _Length, ipv6prefix) ->
    <<0:8, Prefix:8, IP/binary>> = Bin,
    {{Prefix, list_to_tuple([I || <<I:16>> <= IP])}, <<>>};
decode_value(Bin, Length, _Type) ->
    decode_value(Bin, Length).

decode_value(Bin, Length) ->
    <<Value:Length/binary, Rest/binary>> = Bin,
    {Value, Rest}.

lookup_value(Code, Name, Attrs) ->
    case lists:keysearch(Code, 1, Attrs) of
        {value, {_, Value}} ->
            Value;
        false ->
            case lists:keysearch(Name, 1, Attrs) of
                {value, {_, Value}} ->
                    Value;
                false ->
                    undefined
            end
    end.

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
                _ ->
                    {error, invalid}
            end;
        _Value ->
            try
                A1 = A ++ [{"Message-Authenticator", <<0:128>>}],
                {ok, A2} = encode_attributes(A1),

                Length = <<(20 + byte_size(A2)):16>>,
                Packet = list_to_binary([Code, Ident, Length, ReqAuth, A2]),
                MA = crypto:md5_mac(Secret, Packet),

                A3 = A ++ [{"Message-Authenticator", MA}],
                {ok, A4} = encode_attributes(A3),

                Auth = erlang:md5([Code, Ident, Length, ReqAuth, A4, Secret]),
                Data = list_to_binary([Code, Ident, Length, Auth, A4]),
                {ok, Data}
            catch
                _:_ ->
                    {error, invalid}
            end
    end.

encode_attributes(Attrs) ->
    try
        Bin = encode_attributes(Attrs, []),
        {ok, Bin}
    catch
        _:Reason ->
            {error, Reason}
    end.

encode_attributes(undefined, []) ->
    <<>>;
encode_attributes([], Bin) ->
    list_to_binary(lists:reverse(Bin));
encode_attributes([A | Attrs], Bin) ->
    encode_attributes(Attrs, [encode_attribute(A) | Bin]).

encode_attribute({Code, Value}) ->
    case radius_dict:lookup_attribute(Code) of
        not_found ->
            error_logger:warning_msg("Unable to lookup attribute ~p in dictionary~n", [Code]),
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

%% @doc Encodes RADIUS value to Erlang term.
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
        _:_ ->
            error_logger:warning_msg(
                "Unable to encode attribute value ~p as integer~n", [Value]),
            throw({error, encode_value})
    end;
encode_value(Value, integer) when is_integer(Value) ->
    <<Value:32>>;
encode_value(Value, date) ->
    encode_value(Value, integer);
encode_value(Value, ipaddr) when is_list(Value) ->
    case inet_parse:address(Value) of
        {ok, {A, B, C, D}} ->
            <<A:8, B:8, C:8, D:8>>;
        _ ->
            error_logger:warning_msg(
                "Unable to encode attribute value ~p as ipaddr~n", [Value]),
            throw({error, encode_value})
    end;
encode_value({A, B, C, D}, ipaddr) ->
    <<A:8, B:8, C:8, D:8>>;
encode_value(Value, ipv6addr) when is_list(Value) ->
    case inet_parse:address(Value) of
        {ok, IP} when tuple_size(IP) == 8 ->
            encode_value(IP, ipv6addr);
        _ ->
            throw({error, encode_value})
    end;
encode_value(Value, ipv6addr) when tuple_size(Value) == 8 ->
    binary:list_to_bin([<<I:16>> || I <- tuple_to_list(Value)]);
encode_value({Prefix, IP}, ipv6prefix) ->
    list_to_binary([<<0:8, Prefix:8>>, encode_value(IP, ipv6addr)]);
encode_value(Value, Type) ->
    error_logger:warning_msg(
        "Unable to encode attribute value ~p as ~p~n", [Value, Type]),
    throw({error, encode_value}).
