-module(zeromqapi_worker).

-export([initial_state/0,
         connect/4,
         detect/4]).

-include("types.hrl").
-type state() :: string().
-type mziframe() :: string().
-type erlzmq_socket() :: tuple().

-record('MZIFrame', {
    original_time,
    received_latency = 0,
    message_kind,
    service_version = 1,
    service_name = ""
}).

-define('KIND_SYSTEM_MGMT', 16#4d). % M
-define('KIND_I_AM_SERVICE', 16#54). % S
-define('KIND_SERVICE_QUERY', 16#51). % Q
-define('KIND_SERVICE_ANSWER', 16#41). % A
-define('KIND_PROXY_ERROR', 16#45). % E

-record(s, {
    ctx          = undefined,
    socket       = undefined,
    services     = []
}).

-spec initial_state() -> state().
initial_state() -> #s{}.

%% connect and discover LD service
-spec connect(state(), meta(), string(), string()) -> {nil, state()}.
connect(_, _Meta, Identity, Address) ->
    {ok, C} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(C, [router]),
    erlzmq:setsockopt(Socket, identity, Identity),
    ok = erlzmq:bind(Socket, Address),
	  lager:info("Binded to ~p with id=~p~nDiscovering... ", [Address, Identity]),
    {ok, Service} = erlzmq:recv(Socket),
	  lager:info("'~p' discovered~n", [Service]),
    {nil, #s{ctx = C, socket = Socket, services = [Service]}}.

%% send and wait for reply on language detection query
-spec detect(state(), meta(), string(), string()) -> {nil, state()}.
detect(#s{socket = Socket, services = Services} = State, Meta, Text, Lang) ->
    mzbench_metrics:notify_counter(Meta),
    send_query(Socket, Services, [{"text", Text}, {"keyboard_lang", Lang}]),
	  lager:info("Sent '~p' ~p~n", [Text, Lang]),
	  lager:info("Received '~p'~n", [get_answer(Socket)]), %% jsx:decode(R)
    {nil, State}.

-spec receive_whole(erlzmq_socket()) -> [binary()].
receive_whole(Socket) ->
    {ok, R} = erlzmq:recv(Socket),
    {ok, RcvMore} = erlzmq:getsockopt(Socket, rcvmore),
    case RcvMore of
      0 -> [R];
      _ -> [R | receive_whole(Socket)]
    end.

-spec get_mziframe([binary()]) -> mziframe().
get_mziframe(List) ->
     [_, MZIF | _] = lists:dropwhile(fun(E)-> <<>> =/= E end, List),
     unpack_frame(MZIF).

-spec get_message([binary()]) -> [binary()].
get_message(List) ->
     jsx:decode(lists:last(List)).

-spec get_answer(erlzmq_socket()) -> {mziframe(), [binary()]}.
get_answer(Socket) ->
     Frames = receive_whole(Socket),
     MZIF = get_mziframe(Frames),
     case MZIF#'MZIFrame'.message_kind of
       ?KIND_SERVICE_ANSWER -> {MZIF, get_message(Frames)};
       _ -> lager:info("Got: ~p~n", [Frames]), get_answer(Socket)
     end.

-spec now_seconds() -> integer().
now_seconds() ->
  {MS, S, _} = erlang:now(),
  MS*1000000 + S.

-spec unpack_frame(binary()) -> mziframe().
unpack_frame(Binary) ->
    case Binary of
    << OT:64/big-float, RL:32/float, MK:8, SV:8, SNB/binary >> ->
      #'MZIFrame'{original_time = OT, received_latency = RL,
                  message_kind = MK, service_version = SV, service_name = erlang:binary_to_list(SNB)};
      _ -> #'MZIFrame'{message_kind = undefined}
    end.

-spec pack_frame(mziframe()) -> binary().
pack_frame(#'MZIFrame'{original_time = OT, received_latency = RL,
                       message_kind = MK, service_version = SV, service_name = SN}) ->
    SNB = erlang:list_to_binary(SN),
    << OT:64/big-float, RL:32/float, MK:8, SV:8, SNB/binary >>.

-spec send_query(erlzmq_socket(), [string()], [tuple()]) -> ok.
send_query(Socket, Services, Proplist) ->
    Languages = lists:map(fun(A)-> erlang:list_to_binary(A) end, ["cn","da","de","en","es","fr","it","ja","ko","nl","no","pt","sv"]),
    lists:map(fun(Service) -> ok = erlzmq:send(Socket, Service, [sndmore]) end, Services),
    ok = erlzmq:send(Socket, <<>>, [sndmore]),
    ok = erlzmq:send(Socket,pack_frame(#'MZIFrame'{original_time = now_seconds(),
                                                   message_kind = ?KIND_SERVICE_QUERY}), [sndmore]),
    BinProplist = lists:map(fun({A, B}) -> {erlang:list_to_binary(A),
                                                          erlang:list_to_binary(B)} end, Proplist),
    FullRequest = [{<<"service">>, <<"language_detect">>},
                   {<<"request">>, BinProplist ++ [{<<"languages">>,Languages}]}],
    ok = erlzmq:send(Socket, jsx:encode(FullRequest)).
