-module(radius_service).

-behaviour(gen_server).

%% API
-export([start_link/5]).

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
    name :: atom(),
    socket :: inet:socket(),
    requests :: ets:tid(), %% table used to store requests from clients
    callback :: module()
}).

start_link(Name, IP, Port, Callback, SocketOpts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, IP, Port, Callback, SocketOpts], []).

init([Name, IP, Port, Callback, SocketOpts]) ->
    process_flag(trap_exit, true),
    case gen_udp:open(Port, [binary, {ip, IP}, {reuseaddr, true} | SocketOpts]) of
        {ok, Socket} ->
            Requests = ets:new(requests, [public]), %% made it public to allow access from spawned processes(callback)
            {ok, #state{name = Name, socket = Socket, requests = Requests, callback = Callback}};
        {error, Reason} ->
            {error, Reason}
    end.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({udp, Socket, SrcIP, SrcPort, Bin}, State) ->
    Opts = [SrcIP, SrcPort, Socket, Bin, State],
    proc_lib:spawn_link(fun() -> do_callback(Opts) end),
    {noreply, State};

handle_info({'EXIT', _Pid, normal}, State) -> {noreply, State};
handle_info({'EXIT', Pid, _Reason}, #state{requests = Requests} = State) ->
    case ets:match_object(Requests, {'_', Pid}) of
        [{{IP, Port, Ident}, Pid}] ->
            ets:delete(Requests, {IP, Port, Ident});
        [] -> ok
    end,
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, State) ->
    case Reason of
        normal ->
            true = ets:delete(radius_clients, State#state.name);
        _ -> ok
    end,
    gen_udp:close(State#state.socket).

%% Internal functions
do_callback([SrcIP, SrcPort, Socket, Bin, #state{requests = Requests, callback = Callback} = State]) ->
    case lookup_client(SrcIP, State#state.name) of
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
                    ets:delete(Requests, {SrcIP, SrcPort, Ident});
                {error, Reason} ->
                    Callback:handle_error(Reason, [Bin, Client])
            end;
        undefined ->
            Callback:handle_error(unknown_client, [SrcIP, SrcPort, Bin])
    end.

lookup_client(IP, Name) ->
    case ets:lookup(radius_clients, Name) of
        [] ->
            undefined;
        [{Name, Clients}] ->
            check_client_ip(Clients, IP)
    end.

check_client_ip([], _IP) -> undefined;
check_client_ip([#nas_spec{ip = {ip, IP}} = Client | _Rest], IP) ->
    {ok, Client#nas_spec{ip = IP}};
check_client_ip([#nas_spec{ip = {net, {Network, Mask}}} = Client | Rest], IP) ->
    case in_range(IP, {Network, Mask}) of
        true -> {ok, Client#nas_spec{ip = IP}};
        false ->
            check_client_ip(Rest, IP)
    end;
check_client_ip([_Client| Rest], IP) ->
    check_client_ip(Rest, IP).

-spec aton(inet:ip_address()) -> non_neg_integer().
aton({A, B, C, D}) ->
    (A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D.

-spec in_range(IP :: inet:ip_address(), {Network :: inet:ip_address(), Mask :: 0..32 | inet:ip_address()}) -> boolean().
in_range(IP, {Network, Mask}) ->
    {Network0, Mask0} = parse_address(Network, Mask),
    (aton(IP) band Mask0) == (Network0 band Mask0).

-spec parse_address(IP :: inet:ip_address(), Mask :: 0..32 | inet:ip_address()) -> {0..4294967295, 0..4294967295}.
parse_address(IP, Mask) ->
    NetMask = case Mask of
        N when is_tuple(N) andalso tuple_size(N) == 4 ->
            aton(Mask);
        N when N >= 0 andalso N =< 32 ->
            (16#ffffffff bsr (32 - Mask)) bsl (32 - Mask)
    end,
    {aton(IP), NetMask}.
