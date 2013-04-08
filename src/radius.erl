-module(radius).

%% API
-export([start_service/2, stop_service/1, services/0]).

-export([add_client/2, del_client/2]).

-export([attribute_value/2]).

-include("radius.hrl").

%% @doc Start RADIUS service with specified options.
-spec start_service(atom(), [proplists:property()]) ->
    {ok, pid()} | {error, term()}.
start_service(Name, Opts) when is_atom(Name), is_list(Opts) ->
    IP = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts),
    Callback = proplists:get_value(callback, Opts),
    radius_sup:start_child([Name, IP, Port, Callback]).

%% @doc Stop RADIUS service by name.
-spec stop_service(atom()) -> ok | {error, atom()}.
stop_service(Name) when is_atom(Name) ->
    supervisor:terminate_child(radius_sup, Name),
    supervisor:delete_child(radius_sup, Name).

%% @doc Returns the list of running RADIUS services.
-spec services() -> [] | [atom()].
services() ->
    [S || {S, _, _, _} <- supervisor:which_children(radius_sup)].

%% @doc Add new NAS to the list of allowed NASes for specific service
-spec add_client(atom(), #nas_spec{}) -> ok.
add_client(Name, NasSpec) when is_atom(Name), is_record(NasSpec, nas_spec) ->
    gen_server:call(Name, {add_client, NasSpec}).

%% @doc Delete NAS for specific service
-spec del_client(atom(), any()) -> ok.
del_client(SvcName, NasName) when is_atom(SvcName) ->
    gen_server:call(SvcName, {del_client, NasName}).

%% @doc Returns value of RADIUS attribute defined in packet
-spec attribute_value(non_neg_integer() | tuple() | string(),
    #radius_packet{} | list()) -> undefined | term().
attribute_value(Code, Packet) ->
    radius_codec:attribute_value(Code, Packet).
