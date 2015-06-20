RADIUS protocol
===============

[![Build Status](https://secure.travis-ci.org/ates/radius.png)](https://travis-ci.org/ates/radius)

REUSE_PORT
----------

To make radius service utilize the REUSE_PORT feature just add the following to the sockets options:

```erlang
{reuseport, {true, N}}
```

Where N is amount of workers which must be listen the port

Server example
--------------

```erlang
-module(server).

-behaviour(radius_service).

-export([start/0, stop/0, handle_request/3]).
-export([handle_error/2]).

-include("radius.hrl").

-define(SERVICE_NAME, test_radius_server).

start() ->
    {ok, _Started} = application:ensure_all_started(radius),
    lists:foreach(fun radius_dict:add/1, radius_dict_file:load("dictionary")),
    Nas = #nas_spec{name = nas1, ip = {ip, {127,0,0,1}}, secret = "testing123"},
    ServiceOpts = [
        {ip, {0,0,0,0}},
        {port, 1812},
        {callback, ?MODULE}
    ],
    radius:start_service(?SERVICE_NAME, ServiceOpts),
    radius:add_client(?SERVICE_NAME, Nas).

stop() ->
    radius:stop_service(?SERVICE_NAME).

handle_request(Type, Request, Client) ->
    io:format("Type: ~p~nRequest: ~p~nClient: ~p~n", [Type, Request, Client]),
    Response = #radius_packet{code = ?ACCESS_ACCEPT, attrs = []},
    {ok, Response}.

handle_error(Reason, Data) ->
    io:format("Reason: ~p, Data: ~p~n", [Reason, Data]).
```

Client example
--------------

```erlang
-module(client).

-export([send/0]).

-include("radius.hrl").

send() ->
    {ok, _Started} = application:ensure_all_started(radius),
    lists:foreach(fun radius_dict:add/1, radius_dict_file:load("dictionary")),

    {ok, Pid} = radius_client:start_link({127, 0, 0, 1}, 1812, "testing123"),

    Attrs = [
        {"User-Name", "john"},
        {"Password", "secret"}
    ],

    Reply = radius_client:send(Pid, ?ACCESS_REQUEST, Attrs),

    io:format("Reply: ~p~n", [Reply]),

    radius_client:stop(Pid).
```

License
-------

All parts of this software are distributed under the Apache License, Version 2.0 terms.
