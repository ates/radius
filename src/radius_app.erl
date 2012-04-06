-module(radius_app).

-behaviour(application).

%% API
-export([profile_output/0]).

%% application callbacks
-export([start/2, stop/1]).

-type application_start_type() :: normal
    | {takeover, node()} | {failover, node()}.

-spec start(application_start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    consider_profiling(),
    radius_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%% @doc Stop profiling and display results.
%% @spec profile_output() -> ok
-spec profile_output() -> ok.
profile_output() ->
    eprof:stop_profiling(),
    eprof:log("procs.profile"),
    eprof:analyze(procs),
    eprof:log("total.profile"),
    eprof:analyze(total).

%%
%% Internal functions
%%

%% @doc Starts profiling of the application.
-spec consider_profiling() -> profiling | not_profiling.
consider_profiling() ->
    case application:get_env(profile) of
        {ok, true} ->
            eprof:start(),
            eprof:start_profiling([self()]);
        _ ->
            not_profiling
    end.
