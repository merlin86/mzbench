-module(amqp_worker).
-export([initial_state/0]).
-export([connect/2, disconnect/1,
         declare_exchange/2, declare_queue/2, bind/4,
         publish/3, publish/4, get/2, subscribe/2]).
-export([consumer/1, consumer_loop/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(s, {
    connection       = undefined,
    channel          = undefined,
    consumer_pid     = undefined,
    subscription_tag = undefined,
    queue            = undefined
}).

initial_state() -> #s{}.

connect(_, Address) ->
    {ok, Target} = amqp_uri:parse(Address),
    {ok, Connection} = amqp_connection:start(Target),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {nil, #s{connection = Connection, channel = Channel}}.

disconnect(#s{connection = Connection, channel = Channel,
        consumer_pid = Consumer, subscription_tag = Tag}) ->
    case Consumer of
        undefined -> ok;
        _         -> amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = Tag})
    end,
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    {nil, initial_state()}.

declare_queue(State, Q) ->
    Channel = State#s.channel,
    Declare = #'queue.declare'{queue = Q, auto_delete = true},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),
    {nil, State}.

declare_exchange(State, X) ->
    Channel = State#s.channel,
    Declare = #'exchange.declare'{exchange = X, auto_delete = true},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
    {nil, State}.

bind(State, X, RoutingKey, Q) ->
    Channel = State#s.channel,
    amqp_channel:call(Channel, #'queue.bind'{queue = Q, exchange = X, routing_key = RoutingKey}),
    {nil, State}.

publish(State, Q, Payload) ->
    publish(State, <<>>, Q, Payload).

publish(State, X, RoutingKey, Payload) ->
    Channel = State#s.channel,
    Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    ok = amqp_channel:call(Channel, Publish, #amqp_msg{payload = Payload}),
    {nil, State}.

get(State, Q) ->
    Channel = State#s.channel,
    Get = #'basic.get'{queue = Q, no_ack = true},
    Response = amqp_channel:call(Channel, Get),
    case Response of
        {#'basic.get_ok'{}, Content} ->
            #amqp_msg{payload = _Payload} = Content;
        #'basic.get_empty'{} ->
            nop
    end,
    {nil, State}.

subscribe(State, Q) ->
    Channel = State#s.channel,
    Consumer = spawn_link(?MODULE, consumer, [Channel]),
    Sub = #'basic.consume'{queue = Q},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, Sub, Consumer),
    {Consumer, State#s{consumer_pid = Consumer, subscription_tag = Tag}}.

consumer(Channel) ->
    erlang:monitor(process, Channel),
    consumer_loop(Channel).

%% Internal functions
consumer_loop(Channel) ->
    receive
        #'basic.consume_ok'{} ->
            ?MODULE:consumer_loop(Channel);

        #'basic.cancel_ok'{} ->
            ok;

        {#'basic.deliver'{delivery_tag = Tag}, _Content} ->
            amqp_channel:call(Channel, #'basic.ack'{delivery_tag = Tag}),
            ?MODULE:consumer_loop(Channel);
        
        {'DOWN', _, _, _, _} ->
            ok
    end.
