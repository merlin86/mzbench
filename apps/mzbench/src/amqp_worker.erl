-module(amqp_worker).
-export([initial_state/0]).
-export([connect/2, disconnect/1, declare_queue/2, declare_exchange/2, bind/4, publish/4, get/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

initial_state() -> {nil, nil}.

connect(_, Address) ->
    {ok, Target} = amqp_uri:parse(Address),
    {ok, Connection} = amqp_connection:start(Target),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {nil, {Connection, Channel}}.

disconnect({Connection, Channel}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    {nil, initial_state()}.

declare_queue({_, Channel}=State, Q) ->
    #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = Q}),
    {nil, State}.

declare_exchange({_, Channel}=State, X) ->
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = X}),
    {nil, State}.

bind({_, Channel}=State, X, RoutingKey, Q) ->
    amqp_channel:call(Channel, #'queue.bind'{queue = Q, exchange = X, routing_key = RoutingKey}),
    {nil, State}.

publish({_, Channel} = State, X, RoutingKey, Payload) ->
    Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    ok = amqp_channel:call(Channel, Publish, #amqp_msg{payload = Payload}),
    {nil, State}.

get(State, Q) ->
    {_, Channel} = State,

    Get = #'basic.get'{queue = Q, no_ack = true},
    Response = amqp_channel:call(Channel, Get),
    case Response of
        {#'basic.get_ok'{}, Content} ->
            #amqp_msg{payload = _Payload} = Content;
        #'basic.get_empty'{} ->
            nop
    end,
    {nil, State}.
