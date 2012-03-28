-module(radius_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-type application_start_type() :: normal
    | {takeover, node()} | {failover, node()}.

-spec start(application_start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    radius_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
