-module(radius_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% supervisor callbacks
-export([init/1]).

-define(TABLES, [
    {radius_clients, [{read_concurrency, true}]},
    {radius_dict_attrs, [{keypos, 2}, {read_concurrency, true}]},
    {radius_dict_values, [{read_concurrency, true}]}
]).
-define(SPEC(Id, Name, IP, Port, Callback, SocketOpts),
    {Id, {radius_service, start_link, [Name, IP, Port, Callback, SocketOpts]},
    transient, brutal_kill, worker, [radius_service]}
).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child([Name, IP, Port, Callback, SocketOpts]) ->
    case proplists:get_value(reuseport, SocketOpts) of
        N when is_integer(N) andalso N > 1 ->
            NewSocketOpts = reuseport(SocketOpts),
            F = fun(Idx) ->
                Id = list_to_atom(atom_to_list(Name) ++ integer_to_list(Idx)),
                supervisor:start_child(?MODULE, ?SPEC(Id, Name, IP, Port, Callback, NewSocketOpts))
            end,
            lists:foreach(F, lists:seq(1, N));
        _ ->
            supervisor:start_child(?MODULE, ?SPEC(Name, Name, IP, Port, Callback, SocketOpts))
    end.

init([]) ->
    ok = create_table(?TABLES),
    {ok, {{one_for_one, 5, 10}, []}}.

%% Internal functions
create_table(Tables) when is_list(Tables) ->
    lists:foreach(fun create_table/1, Tables);
create_table({Table, Options}) ->
    ets:new(Table, [named_table, public | Options]).

reuseport(Options) ->
    NewOptions = case os:type() of
        {unix, linux} ->
            Options ++ [{raw, 1, 15, <<1:32/native>>}];
        {unix, OsName} ->
            case lists:member(OsName, [darwin, freebsd, openbsd, netbsd]) of
                true ->
                    Options ++ [{raw, 16#FFFF, 16#0200, <<1:32/native>>}];
                false ->
                    Options
            end;
        _ -> Options
    end,
    lists:keydelete(reuseport, 1, NewOptions).
