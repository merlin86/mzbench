-module(amqp_worker).
-export([initial_state/0]).
-export([connect/2, disconnect/1, send_message/1, receive_message/1]).
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
    {nil, nil}.

send_message(State) ->
    {_, Channel} = State,

    X = <<"testexchange">>,
    RoutingKey = <<"">>,
    Payload = <<"foobar">>,

    BasicPublish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    ok = amqp_channel:call(Channel, BasicPublish, _MsgPayload = #amqp_msg{payload = Payload}),

    {nil, State}.

receive_message(State) ->
    {_, Channel} = State,

    Q = <<"testqueue">>,

    Get = #'basic.get'{queue = Q, no_ack = true},
    {_, _} = amqp_channel:call(Channel, Get),

    {nil, State}.
