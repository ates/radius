RADIUS protocol
===============

Example:
--------

```erlang
-module(server).

-behaviour(radius_service).

-export([start/0, stop/0, handle_request/3]).

-include("radius.hrl").

-define(SERVICE_NAME, test_radius_server).

start() ->
    application:start(radius),
    lists:foreach(fun radius_dict:add/1, radius_dict_file:load("dictionary")),
    Nas1 = #nas_spec{name = nas1, ip = {127,0,0,1}, secret = "testing123"},
    Nas2 = #nas_spec{name = nas2, ip = {10,10,0,1}, secret = "testing123"},
    ServiceOpts = [
        {ip, {0,0,0,0}},
        {port, 1812},
        {callback, ?MODULE}
    ],
    radius:start_service(?SERVICE_NAME, ServiceOpts),
    radius:add_client(?SERVICE_NAME, Nas1),
    radius:add_client(?SERVICE_NAME, Nas2).

stop() ->
    radius:stop_service(?SERVICE_NAME).

handle_request(Type, Request, Client) ->
    io:format("Type: ~p~nRequest: ~p~nClient: ~p~n", [Type, Request, Client]),
    Response = #radius_packet{code = ?ACCESS_ACCEPT, attrs = []},
    {ok, Response}.
```

Supported data types:
---------------------

 * string - 0-253 octets
 * ipaddr - 4 octets in network byte order
 * integer - 32 bit value in big endian order (high byte first)
 * date - 32 bit value in big endian order - seconds since 00:00:00 GMT,  Jan.  1,  1970
 * ipv6addr - 16 octets in network byte order
 * ipv6prefix - 18 octets in network byte order
 * byte - 8 bit unsigned integer
 * octets - raw octets, printed and input as hex strings
