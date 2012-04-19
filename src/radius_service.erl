-module(radius_service).

-behaviour(gen_server).

%% API
-export([start_link/4]).
-export([do_callback/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-export([behaviour_info/1]).


-include("radius.hrl").

behaviour_info(callbacks) ->
    [{handle_request, 3}];
behaviour_info(_) ->
    undefined.

-record(state, {socket, clients, callback}).

-spec start_link(atom(), inet:ip_address(), non_neg_integer(), atom()) ->
    {ok, term()} | {error, term()}.
start_link(Name, IP, Port, Callback) ->
    gen_server:start_link({local, Name}, ?MODULE, [IP, Port, Callback], []).

init([IP, Port, Callback] = Options) ->
    process_flag(trap_exit, true),
    case gen_udp:open(Port, [binary, proto(IP), {ip, IP}]) of
        {ok, Socket} ->
            Clients = ets:new(clients, [{keypos, 3}]),
            {ok, #state{socket = Socket, clients = Clients, callback = Callback}};
        {error, Reason} ->
            error_logger:error_msg(
                "** RADIUS service can't start~n"
                "   for the reason ~p: ~s~n"
                "** Options were ~p~n",
                [Reason, inet:format_error(Reason), Options]),
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
    proc_lib:spawn_link(?MODULE, do_callback,[Opts]),
    {noreply, State};

handle_info({'EXIT', _Pid, normal}, State) -> {noreply, State};
handle_info({'EXIT', Pid, _Reason}, State) ->
    sweep_request(Pid),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, State) ->
    gen_udp:close(State#state.socket).

%%
%% Internal functions
%%
-spec proto(inet:ip_address()) -> inet:address_family().
proto(Address) when tuple_size(Address) == 4 -> inet;
proto(Address) when tuple_size(Address) == 8 -> inet6.

do_callback([SrcIP, SrcPort, Socket, Bin, State]) ->
    %error_logger:info_msg("Process do_callback started with Pid: ~p~n", [self()]),
    case lookup_client(SrcIP, State#state.clients) of
        {ok, #nas_spec{secret = Secret} = Client} ->
	    case radius_codec:decode_packet(Bin, Secret) of
                {ok, Packet} ->
		    case request_exists(SrcIP, SrcPort, Packet) of
                        false ->
			    store_request(SrcIP, SrcPort, Packet, self()),
			    case radius_codec:identify_packet(Packet#radius_packet.code) of
				{ok, Type} ->
				    Callback=State#state.callback,
				    case Callback:handle_request(Type, Packet, Client) of
					{ok, Response} ->
					    do_reply(Socket, SrcIP, SrcPort, Response, Packet, Client),
					    sweep_request(SrcIP, SrcPort, Packet);
					noreply ->
					    sweep_request(SrcIP, SrcPort, Packet);
					Unknown ->
					    error_logger:error_msg("Bad return from handler: ~p~n", [Unknown])
				    end;
				{unknown, Unknown} ->
				    error_logger:warning_msg("Unknown request type: ~p~n", [Unknown]),
				    sweep_request(SrcIP, SrcPort, Packet)
			    end;
			true -> ok
                    end;
		_ ->
                    error_logger:error_msg(
		      "Received invalid packet from NAS: ~s~n", [inet_parse:ntoa(SrcIP)])
            end;
	undefined ->
	    error_logger:warning_msg(
	      "Request from unknown client: ~s~n", [inet_parse:ntoa(SrcIP)])
    end.

do_reply(Socket, IP, Port, Response, Request, Client) ->
    Secret = Client#nas_spec.secret,
    case radius_codec:encode_response(Request, Response, Secret) of
        {ok, Data} ->
            gen_udp:send(Socket, IP, Port, Data);
        {error, Reason} ->
            error_logger:error_msg("Unable to respond to client due to ~p~n", [Reason])
    end.

lookup_client(IP, Table) ->
    case ets:lookup(Table, IP) of
        [] ->
            undefined;
        [Client] ->
            {ok, Client}
    end.

request_exists(IP, Port, Packet) ->
    Ident = Packet#radius_packet.ident,
    ets:member(?MODULE, {IP, Port, Ident}).

store_request(IP, Port, Packet, Pid) ->
    Ident = Packet#radius_packet.ident,
    ets:insert(?MODULE, {{IP, Port, Ident}, Pid}).

sweep_request(Pid) ->
    case ets:match_object(?MODULE, {'_', Pid}) of
        [{{IP, Port, Ident}, Pid}] ->
            ets:delete(?MODULE, {IP, Port, Ident});
        [] -> ok
    end.

sweep_request(IP, Port, Packet) ->
    Ident = Packet#radius_packet.ident,
    ets:delete(?MODULE, {IP, Port, Ident}).
