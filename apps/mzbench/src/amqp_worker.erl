-module(amqp_worker).
-export([initial_state/0]).
-export([connect/2, disconnect/1, send_message/4, receive_message/2, autoreceive/2,
    declare/4]).
-export([consumer/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(s, {
        connection = undefined,
        channel = undefined,
        consumer_pid = undefined,
        subscription_tag = undefined,
        queue = undefined,
        exchange = undefined}).

initial_state() -> #s{}.

connect(_, Address) ->
    {ok, Target} = amqp_uri:parse(Address),
    {ok, Connection} = amqp_connection:start(Target),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {nil, #s{connection = Connection, channel = Channel}}.

disconnect(#s{connection = Connection, channel = Channel,
        consumer_pid = Consumer, subscription_tag = Tag,
        queue = Queue, exchange = Exchange}) ->
    case Consumer of
        undefined ->
            ok;
        _ ->
            amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = Tag})
    end,
    case Queue of
        undefined ->
            ok;
        _ ->
            DeleteQ = #'queue.delete'{queue = Queue},
            #'queue.delete_ok'{} = amqp_channel:call(Channel, DeleteQ)
    end,
    case Exchange of
        undefined ->
            ok;
        _ ->
            DeleteX = #'exchange.delete'{exchange = Exchange},
            #'exchange.delete_ok'{} = amqp_channel:call(Channel, DeleteX)
    end,
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    {nil, initial_state()}.

declare(State = #s{channel = Channel}, Q, X, RoutingKey) ->
    DeclareX = #'exchange.declare'{exchange = X},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, DeclareX),
    DeclareQ = #'queue.declare'{queue = Q},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, DeclareQ),

    Binding = #'queue.bind'{queue = Q,
        exchange    = X,
        routing_key = RoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

    {nil, State#s{queue = Q, exchange = X}}.

%% TODO investigate failures
send_message(State = #s{channel = Channel}, X, RoutingKey, Payload) ->
    BasicPublish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    ok = amqp_channel:call(Channel, BasicPublish, _MsgPayload = #amqp_msg{payload = Payload}),
    {nil, State}.

receive_message(State = #s{channel = Channel}, Q) ->
    Get = #'basic.get'{queue = Q, no_ack = true},
    {_, _} = amqp_channel:call(Channel, Get),
    {nil, State}.

autoreceive(State = #s{channel = Channel}, Q) ->
    Consumer = spawn_link(amqp_worker, consumer, [Channel]),
    Sub = #'basic.consume'{queue = Q},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, Sub, Consumer),
    {Consumer, State#s{consumer_pid = Consumer, subscription_tag = Tag}}.

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
