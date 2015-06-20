-module(radius).

%% API
-export([start_service/2, stop_service/1, services/0]).
-export([start_service/3]).
-export([add_client/2, del_client/2]).
-export([attribute_value/2]).
-export([stats/1]).

-include("radius.hrl").

%% @doc Start RADIUS service with specified options.
start_service(Name, Opts) ->
    start_service(Name, Opts, []).

start_service(Name, Opts, SocketOpts) ->
    IP = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts),
    Callback = proplists:get_value(callback, Opts),
    radius_sup:start_child([Name, IP, Port, Callback, SocketOpts]).

%% @doc Stop RADIUS service by name.
-spec stop_service(atom()) -> ok | {error, term()}.
stop_service(Name) when is_atom(Name) ->
    F = fun(Id, Pid) ->
        radius_service:stop(Pid),
        supervisor:delete_child(radius_sup, Id)
    end,
    [F(Id, Pid) || {Id, Pid, _, _} <- supervisor:which_children(radius_sup), radius_service:name(Pid) == Name],
    ok.

%% @doc Returns the list of running RADIUS services.
-spec services() -> [term()].
services() ->
    lists:usort([radius_service:name(element(2, S)) || S <- supervisor:which_children(radius_sup)]).

%% @doc Add new NAS to the list of allowed NASes for specific service
-spec add_client(atom(), #nas_spec{}) -> ok.
add_client(Name, NasSpec) when is_atom(Name), is_record(NasSpec, nas_spec) ->
    Specs = case ets:lookup(radius_clients, Name) of
        [] ->
            {Name, [NasSpec]};
        [{Name, Value}] ->
            {Name, [NasSpec | Value]}
    end,
    true = ets:insert(radius_clients, Specs), ok.

%% @doc Delete NAS for specific service
-spec del_client(atom(), any()) -> ok.
del_client(SvcName, NasName) when is_atom(SvcName) ->
    case ets:lookup(radius_clients, SvcName) of
        [] ->
            ok;
        [{SvcName, Value}] ->
            Specs = [Spec || Spec <- Value, Spec#nas_spec.name =/= NasName],
            true = ets:insert(radius_clients, {SvcName, Specs}), ok
    end.

%% @doc Returns value of RADIUS attribute defined in packet
-spec attribute_value(Code :: non_neg_integer() | tuple() | string(), Packet :: #radius_packet{} | [proplists:property()]) ->
    undefined | term().
attribute_value(Code, Packet) ->
    radius_codec:attribute_value(Code, Packet).

stats(Name) ->
    [
        begin
            {ok, Values} = radius_service:stats(Pid),
            {Id, Values}
        end || {Id, Pid, _, _} <- supervisor:which_children(radius_sup), radius_service:name(Pid) == Name
    ].
