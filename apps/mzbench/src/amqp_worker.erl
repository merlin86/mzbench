-module(amqp_worker).
-export([initial_state/0]).
-export([connect/2, disconnect/1, send_message/4, receive_message/2, autoreceive/2]).
-export([consumer/1]).

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

autoreceive(State, Q) ->
    {_, Channel} = State,
    Consumer = spawn(amqp_worker, consumer, [Channel]),

    Sub = #'basic.consume'{queue = Q},
    #'basic.consume_ok'{consumer_tag = _} = amqp_channel:subscribe(Channel, Sub, Consumer),

    {Consumer, State}.

%% Internal functions
consumer(Channel) ->
    receive
        %% This is the first message received
        #'basic.consume_ok'{} ->
            consumer(Channel);

        %% This is received when the subscription is cancelled
        #'basic.cancel_ok'{} ->
            ok;

        %% A delivery
        {#'basic.deliver'{delivery_tag = Tag}, _} ->
            %% Do something with the message payload
            %% (some work here)

            %% Ack the message
            amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

            %% Loop
            consumer(Channel)
    end.
