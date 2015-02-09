-module(server).

-behaviour(radius_service).

-export([start/0, stop/0, handle_request/3]).
-export([handle_error/2]).

-include("radius.hrl").

-define(SERVICE_NAME, test_radius_server).

start() ->
    Nas1 = #nas_spec{
        name = nas1,
        ip = {ip, {127, 0, 0, 1}},
        secret = "testing123"
    },
    ServiceOpts = [
        {ip, {0, 0, 0, 0}},
        {port, 1812},
        {callback, ?MODULE}
    ],
    radius:start_service(?SERVICE_NAME, ServiceOpts),
    radius:add_client(?SERVICE_NAME, Nas1).

stop() ->
    radius:stop_service(?SERVICE_NAME).

handle_request('Access-Request', _Request, _Client) ->
    Attrs = [
        {"Framed-IP-Address", {10, 10, 0, 1}}
    ],
    Response = #radius_packet{code = ?ACCESS_ACCEPT, attrs = Attrs},
    {ok, Response}.

handle_error(Reason, Data) ->
    io:format("Reason: ~p, Data: ~p~n", [Reason, Data]).
