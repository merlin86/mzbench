-module(amqp_worker).
-export([start/1]).
-export([read_message/0]).
-include("../deps/amqp_client/include/amqp_client.hrl").

start(Address) ->
    {ok, Target} = amqp_uri:parse(Address),
    loop(Target, nil).

loop(Target, Connection) ->
    receive
        connect ->
            io:format("Connecting ~n"),
            {ok, NewConnection} = amqp_connection:start(Target),
            loop(Target, NewConnection);
        disconnect ->
            io:format("Disconnecting ~n"),
            amqp_connection:close(Connection),
            loop(Target, nil);
        publish ->
            io:format("Publishing ~n"),
            send_message(Connection, <<"testexchange">>, <<"">>, <<"foobar">>),
            loop(Target, Connection);
        _ ->
            io:format("Unknown~n"),
            loop(Target, Connection)
    end.

send_message(Connection, X, RoutingKey, Payload) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    io:format("basic.publish setup~n"),
    BasicPublish = #'basic.publish'{exchange = X, routing_key = RoutingKey},

    io:format("amqp_channel:cast~n"),
    ok = amqp_channel:call(Channel, BasicPublish, _MsgPayload = #amqp_msg{payload = Payload}),

    amqp_channel:close(Channel).

read_message() ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Get = #'basic.get'{queue = <<"testqueue">>},
    {#'basic.get_ok'{}, Content} = amqp_channel:call(Channel, Get),
    #'basic.get_empty'{} = amqp_channel:call(Channel, Get),
    io:format("Content ~s~n", Content),
    amqp_channel:call(Channel, #'channel.close'{}).
