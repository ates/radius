-module(radius_service).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-include("radius.hrl").

-callback handle_request(Type :: non_neg_integer(),
                         Request :: #radius_packet{},
                         Client :: #nas_spec{}) ->
    {ok, Response :: #radius_packet{}} | noreply.

-callback handle_error(Reason :: term(), Data :: term()) -> any().

-record(state, {
    socket :: inet:socket(),
    requests :: ets:tid(), %% table used to store requests from clients
    clients :: [#nas_spec{}],
    callback :: module()
}).

start_link(Name, IP, Port, Callback) ->
    gen_server:start_link({local, Name}, ?MODULE, [IP, Port, Callback], []).

init([IP, Port, Callback]) ->
    process_flag(trap_exit, true),
    case gen_udp:open(Port, [binary, {ip, IP}, {reuseaddr, true}]) of
        {ok, Socket} ->
            Requests = ets:new(requests, [public]), %% made it public to allow access from spawned processes(callback)
            Clients = ets:new(clients, [{keypos, 3}]),
            {ok, #state{socket = Socket, requests = Requests, clients = Clients, callback = Callback}};
        {error, Reason} ->
            {error, Reason}
    end.

handle_call({add_client, NasSpec}, _From, State) ->
    ets:insert(State#state.clients, NasSpec),
    {reply, ok, State};

handle_call({del_client, NasName}, _From, State) ->
    Pattern = {nas_spec, NasName, '_', '_'},
    case ets:match_object(State#state.clients, Pattern) of
        [NasSpec] ->
            ets:delete_object(State#state.clients, NasSpec);
        _ -> ok
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({udp, Socket, SrcIP, SrcPort, Bin}, State) ->
    Opts = [SrcIP, SrcPort, Socket, Bin, State],
    proc_lib:spawn_link(fun() -> do_callback(Opts) end),
    {noreply, State};

handle_info({'EXIT', _Pid, normal}, State) -> {noreply, State};
handle_info({'EXIT', Pid, _Reason}, State) ->
    sweep_request(Pid, State#state.requests),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, State) ->
    gen_udp:close(State#state.socket).

%% Internal functions
do_callback([SrcIP, SrcPort, Socket, Bin, #state{requests = Requests, callback = Callback} = State]) ->
    case lookup_client(SrcIP, State#state.clients) of
        {ok, #nas_spec{secret = Secret} = Client} ->
            case radius_codec:decode_packet(Bin, Secret) of
                {ok, #radius_packet{ident = Ident} = Packet} ->
                    case ets:member(Requests, {SrcIP, SrcPort, Ident}) of
                        false ->
                            %% store request in the table to avoid duplicates
                            true = ets:insert(Requests, {{SrcIP, SrcPort, Ident}, self()}),
                            PacketType = radius_codec:identify_packet(Packet#radius_packet.code),
                            case Callback:handle_request(PacketType, Packet, Client) of
                                {ok, Response} ->
                                    {ok, Data} = radius_codec:encode_response(Packet, Response, Secret),
                                    ok = gen_udp:send(Socket, SrcIP, SrcPort, Data);
                                noreply -> ok
                            end;
                        true ->
                            Callback:handle_error(duplicate_request, [Packet, Client])
                    end,
                    sweep_request(SrcIP, SrcPort, Ident, Requests);
                {error, Reason} ->
                    Callback:handle_error(Reason, [Bin, Client])
            end;
        undefined ->
            Callback:handle_error(unknown_client, [SrcIP, SrcPort, Bin])
    end.

lookup_client(IP, Table) ->
    case ets:lookup(Table, IP) of
        [] ->
            undefined;
        [Client] ->
            {ok, Client}
    end.

sweep_request(Pid, Table) ->
    case ets:match_object(Table, {'_', Pid}) of
        [{{IP, Port, Ident}, Pid}] ->
            ets:delete(Table, {IP, Port, Ident});
        [] -> ok
    end.

sweep_request(IP, Port, Ident, Table) ->
    ets:delete(Table, {IP, Port, Ident}).
