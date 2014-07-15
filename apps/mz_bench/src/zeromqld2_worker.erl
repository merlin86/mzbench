-module(zeromqld2_worker).

-export([initial_state/0,
         connect/4,
         detect/4]).

-include("types.hrl").

-record(s, {
    ctx          = undefined :: undefined | erlzmq:erlzmq_context(),
    socket       = undefined :: undefined | erlzmq:erlzmq_socket(),
    services     = [] :: list()
}).

-record('MZIFrame', {
    original_time :: integer() | float(),
    received_latency = 0.0 :: float(),
    message_kind :: undefined | integer(),
    service_version = 1 :: integer(),
    service_name = "" :: string()
}).

-type state() :: #s{}.
-type mziframe() :: #'MZIFrame'{}.
-type ne_binary() :: <<_:8,_:_*8>>.

-define('KIND_SYSTEM_MGMT', 16#4d). % M
-define('KIND_I_AM_SERVICE', 16#54). % S
-define('KIND_SERVICE_QUERY', 16#51). % Q
-define('KIND_SERVICE_ANSWER', 16#41). % A
-define('KIND_PROXY_ERROR', 16#45). % E

-define('LANGUAGES', [<<"cn">>,<<"da">>,<<"de">>,<<"en">>,<<"es">>,<<"fr">>,<<"it">>,
                      <<"ja">>,<<"ko">>,<<"nl">>,<<"no">>,<<"pt">>,<<"sv">>]).

-spec initial_state() -> state().
initial_state() -> #s{}.

%% connect and discover LD service
-spec connect(state(), meta(), string(), string()) -> {nil, state()}.
connect(#s{services = Services} = State, _Meta, Identity, Address) ->
    {ok, C} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(C, [router]),
    erlzmq:setsockopt(Socket, identity, Identity),
    ok = erlzmq:bind(Socket, Address),
    lager:info("Binded to ~p with id=~p~nDiscovering... ", [Address, Identity]),
    {ok, Service} = erlzmq:recv(Socket),
    lager:info("'~p' discovered~n", [Service]),
    {nil, State#s{ctx = C, socket = Socket, services = [Service | Services]}}.

%% send and wait for reply on language detection query
-spec detect(state(), meta(), string(), string()) -> {nil, state()}.
detect(#s{socket = Socket, services = Services} = State, Meta, Text, Lang) ->
    mz_bench_metrics:notify_counter(Meta),
    send_query(Socket, Services, [{"text", Text}, {"keyboard_lang", Lang}]),
    lager:info("Sent '~p' ~p~n", [Text, Lang]),
    Answer = get_answer(Socket),
    lager:info("Received '~p'~n", [Answer]),
    {nil, State}.

-spec receive_whole(erlzmq:erlzmq_socket()) -> [binary()].
receive_whole(Socket) ->
    {ok, R} = erlzmq:recv(Socket),
    {ok, RcvMore} = erlzmq:getsockopt(Socket, rcvmore),
    case RcvMore of
      0 -> [erlang:iolist_to_binary(R)];
      _ -> [erlang:iolist_to_binary(R) | receive_whole(Socket)]
    end.

-spec get_mziframe([ne_binary() | any()]) -> mziframe().
get_mziframe(List) ->
     [_, MZIF | _] = lists:dropwhile(fun(E)-> <<>> =/= E end, List),
     unpack_frame(MZIF).

-spec get_message([binary()]) -> [binary()].
get_message(List) ->
     jsx:decode(lists:last(List)).

-spec get_answer(erlzmq:erlzmq_socket()) -> {mziframe(), [binary()]}.
get_answer(Socket) ->
     Frames = receive_whole(Socket),
     MZIF = get_mziframe(Frames),
     case MZIF#'MZIFrame'.message_kind of
       ?KIND_SERVICE_ANSWER -> {MZIF, get_message(Frames)};
       _ -> lager:info("Got: ~p~n", [Frames]), get_answer(Socket)
     end.

-spec now_seconds() -> integer().
now_seconds() ->
    {MS, S, _} = os:timestamp(),
    MS*1000000 + S.

-spec unpack_frame(ne_binary() | any()) -> mziframe().
unpack_frame(<< OT:64/big-float, RL:32/float, MK:8, SV:8, SNB/binary >>) ->
      #'MZIFrame'{original_time = OT, received_latency = RL,
                  message_kind = MK, service_version = SV, service_name = erlang:binary_to_list(SNB)};
unpack_frame(_) -> #'MZIFrame'{message_kind = undefined}.

-spec pack_frame(mziframe()) -> binary().
pack_frame(#'MZIFrame'{original_time = OT, received_latency = RL,
                       message_kind = MK, service_version = SV, service_name = SN}) ->
    SNB = erlang:list_to_binary(SN),
    << OT:64/big-float, RL:32/float, MK:8, SV:8, SNB/binary >>.

-spec full_json([tuple()]) -> [tuple()].
full_json(Proplist) ->
    BinProplist = lists:map(fun({A, B}) -> {erlang:list_to_binary(A),
                                                          erlang:list_to_binary(B)} end, Proplist),
    [{<<"service">>, <<"language_detect">>},
     {<<"version">>, <<"0.1">>},
     {<<"request">>, BinProplist ++ [{<<"languages">>,?LANGUAGES}]}].

-spec send_query(erlzmq:erlzmq_socket(), list(), list()) -> ok.
send_query(Socket, Services, Proplist) ->
    lists:map(fun(Service) -> ok = erlzmq:send(Socket, Service, [sndmore]) end, Services),
    ok = erlzmq:send(Socket, <<>>, [sndmore]),
    ok = erlzmq:send(Socket,pack_frame(#'MZIFrame'{original_time = now_seconds(),
                                                   message_kind = ?KIND_SERVICE_QUERY}), [sndmore]),
    ok = erlzmq:send(Socket, jsx:encode(full_json(Proplist))).
