-module(radius_client).

-behaviour(gen_server).

%% API
-export([start_link/3]).
-export([start_link/4]).
-export([send/3]).
-export([stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-include("radius.hrl").

-record(state, {
    socket :: inet:socket(),
    host :: inet:ip_address(),
    port :: inet:port_number(),
    secret :: string(),
    ident = 0 :: 0..255,
    recv_timeout :: pos_integer(),
    wait_for_reply = true :: boolean()
}).

start_link(Host, Port, Secret) ->
    gen_server:start_link(?MODULE, [Host, Port, Secret, []], []).

start_link(Host, Port, Secret, Options) ->
    gen_server:start_link(?MODULE, [Host, Port, Secret, Options], []).

send(Pid, Type, Attrs) ->
    gen_server:call(Pid, {send, Type, Attrs}).

stop(Pid) ->
    gen_server:call(Pid, stop).

init([Host, Port, Secret, Options]) ->
    {ok, Socket} = gen_udp:open(0, [{active, false}, inet, binary]),
    WaitForReply = case lists:member(noreply, Options) of
        true ->
            false;
        false -> true
    end,
    State = #state{
        socket = Socket,
        host = Host,
        port = Port,
        secret = Secret,
        recv_timeout = proplists:get_value(timeout, Options, 5000),
        wait_for_reply = WaitForReply
    },
    {ok, State}.

handle_call({send, Type, Attrs}, _From, #state{host = Host, port = Port, ident = Ident} = State) ->
    Packet = #radius_packet{
        code = Type,
        ident = State#state.ident,
        attrs = Attrs
    },
    Req = radius_codec:encode_request(Packet, State#state.secret),
    ok = gen_udp:send(State#state.socket, Host, Port, Req),
    Reply = case State#state.wait_for_reply of
        true ->
            case gen_udp:recv(State#state.socket, 0, State#state.recv_timeout) of
                {ok, {_Address, _Port, Response}} ->
                    radius_codec:decode_packet(Response, State#state.secret);
                Error -> Error
        end;
        false -> ok
    end,
    NewIdent =
        if Ident == 255 ->
            1;
        true -> Ident + 1
    end,
    {reply, Reply, State#state{ident = NewIdent}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, State) ->
    gen_udp:close(State#state.socket).
