-module(amqp_worker).
-export([initial_state/0]).
-export([connect/2, disconnect/1, send_message/4, receive_message/2]).
-include("../deps/amqp_client/include/amqp_client.hrl").

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

send_message(State, X, RoutingKey, Payload) ->
    {_, Channel} = State,

    BasicPublish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    ok = amqp_channel:call(Channel, BasicPublish, _MsgPayload = #amqp_msg{payload = Payload}),

    {nil, State}.

receive_message(State, Q) ->
    {_, Channel} = State,

    Get = #'basic.get'{queue = Q, no_ack = true},
    {_, _} = amqp_channel:call(Channel, Get),

    {nil, State}.
