-module(amqp_worker).
-export([initial_state/0]).
-export([connect/3, disconnect/2, prepare/3,
         declare_exchange/3, declare_queue/3, bind/5,
         publish/4, publish/5, get/3, subscribe/3]).
-export([consumer/1, consumer_loop/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(DEFAULT_X, <<"MZBENCH">>).

-record(s, {
    connection       = undefined,
    channel          = undefined,
    consumer_pid     = undefined,
    subscription_tag = undefined,
    queue            = undefined
}).

initial_state() -> #s{}.

connect(_, _Meta, Address) ->
    {ok, Target} = amqp_uri:parse(Address),
    {ok, Connection} = amqp_connection:start(Target),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {nil, #s{connection = Connection, channel = Channel}}.

disconnect(#s{connection = Connection, channel = Channel,
        consumer_pid = Consumer, subscription_tag = Tag}, _Meta) ->
    case Consumer of
        undefined -> ok;
        _         -> amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = Tag})
    end,
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    {nil, initial_state()}.

prepare(State, Meta, InQ) ->
    {_, State1} = declare_exchange(State, Meta, ?DEFAULT_X),
    {_, State2} = declare_queue(State1, Meta, InQ),
    {_, State3} = bind(State2, Meta, ?DEFAULT_X, InQ, InQ),
    {nil, State3}.

declare_queue(State, Meta, InQ) ->
    Q = make_queue_name(Meta, InQ),
    Channel = State#s.channel,
    Declare = #'queue.declare'{queue = Q, auto_delete = true},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),
    {nil, State}.

declare_exchange(State, _Meta, X) ->
    Channel = State#s.channel,
    Declare = #'exchange.declare'{exchange = X, auto_delete = true},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
    {nil, State}.

bind(State, Meta, X, RoutingKey, InQ) ->
    Q = make_queue_name(Meta, InQ),
    Channel = State#s.channel,
    amqp_channel:call(Channel, #'queue.bind'{queue = Q, exchange = X, routing_key = RoutingKey}),
    {nil, State}.

publish(State, Meta, RoutingKey, Payload) ->
    publish(State, Meta, ?DEFAULT_X, RoutingKey, Payload).

publish(State, _Meta, X, RoutingKey, Payload) ->
    Channel = State#s.channel,
    Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    ok = amqp_channel:call(Channel, Publish, #amqp_msg{payload = Payload}),
    {nil, State}.

get(State, Meta, InQ) ->
    Q = make_queue_name(Meta, InQ),
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

subscribe(State, Meta, InQ) ->
    Q = make_queue_name(Meta, InQ),
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

make_queue_name(Meta, Q) ->
    RunId = proplists:get_value(run_id, Meta, <<"default">>),
    <<Q/binary, <<"-">>/binary, RunId/binary>>.
