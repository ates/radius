-module(client).

%% API
-export([request/4]).

-include("radius.hrl").

-define(TIMEOUT, 5000).
-define(UDP_OPTS, [{active, false}, inet, {ip, {127, 0, 0, 1}}, binary]).

request(IP, Port, Secret, Packet) ->
    {ok, Socket} = gen_udp:open(0, ?UDP_OPTS),
    Req1 = radius_codec:encode_request(Packet, Secret),
    ok = gen_udp:send(Socket, IP, Port, Req1),
    {ok, {_, _, Res0}} = gen_udp:recv(Socket, 0, ?TIMEOUT),
    {ok, Res1} = radius_codec:decode(Res0),
    ok = gen_udp:close(Socket),
    {ok, Res1}.
