-module(radius_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

start(_Type, _Args) ->
    radius_sup:start_link().

stop(_State) -> ok.
