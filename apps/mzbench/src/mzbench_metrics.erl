-module(mzbench_metrics).

-export([start_link/1,
         notify_counter/1,
         notify_roundtrip/2,
         get_metrics_values/1]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(s, {
    prefix      = "undefined" :: string(),
    last_export = undefined   :: erlang:now()
}).

-define(INTERVAL, 10000). % 10 seconds

%%%===================================================================
%%% API
%%%===================================================================

start_link(MetricsPrefix) ->
    gen_server:start_link(?MODULE, [MetricsPrefix], []).

notify_counter(Meta) ->
    folsom_metrics:notify(proplists:get_value(metric_prefix, Meta) ++ ".counter", {inc, 1}, counter).

notify_roundtrip(Meta, Value) ->
    folsom_metrics:notify(proplists:get_value(metric_prefix, Meta) ++ ".roundtrip", Value, histogram).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MetricsPrefix]) ->
    process_flag(trap_exit, true),
    erlang:send_after(?INTERVAL, self(), trigger),
    {ok, #s{prefix = MetricsPrefix}}.

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
    lager:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info(trigger, State) ->
    tick(State),
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, State#s{last_export = erlang:now()}};
handle_info(Info, State) ->
    lager:error("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    tick(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tick(#s{prefix = Prefix} = _State) ->
    Metrics = lists:filter(fun (M) -> lists:prefix(Prefix, M) end, folsom_metrics:get_metrics()),
    lager:info("[ metrics ] Got the following metrics: ~p", [Metrics]),

    Values = lists:foldl(
        fun (N, Acc) ->
            case rpc:call(N, mzbench_metrics, get_metrics_values, [Metrics]) of
                {badrpc, Reason} ->
                    lager:error("Failed to request metrics from node ~p (~p)", [N, Reason]),
                    Acc;
                Res ->
                    lager:info("Received metrics from ~p", [N]),
                    merge_metrics(Res, Acc)
            end
        end, [], [node()|nodes()]),

    send_to_graphite(Values),

    ok.

How to merge it??

merge_metrics([], Metrics) -> Metrics;
merge_metrics([{M, V}|T], Metrics) when is_list(V) ->
    merge_metrics(T, lists:keystore(M, 1, Metrics, {M, V ++ proplists:get_value(M, Metrics, [])}));
merge_metrics([{M, V}|T], Metrics) when is_integer(V) ->
    merge_metrics(T, lists:keystore(M, 1, Metrics, {M, V + proplists:get_value(M, Metrics, 0)})).

send_to_graphite(Values) ->
    case graphite_client_sup:get_client() of
        noclient -> ok;
        {ok, GraphiteClient} ->
            {Mega, Secs, _} = now(),
            Timestamp = Mega * 1000000 + Secs,
            lists:map(fun({MetricName, MetricValue}) ->
                            Msg =lists:flatten(io_lib:format("~s ~p ~p~n",
                                                            [MetricName, MetricValue, Timestamp])),
                            graphite_client:send(GraphiteClient, Msg)
                    end,
                    Values);
        Error -> lager:error("Could not get graphite client: ~p", [Error])
    end.


get_metrics_values(Metrics) ->
    Values = lists:flatmap(
      fun(X) ->
          case lists:suffix("roundtrip", X) of
              false -> [{X, folsom_metrics:get_metric_value(X)}];
              true ->
                  Stats = folsom_metrics:get_histogram_statistics(X),
                  [{X ++ ".mean",
                    proplists:get_value(arithmetic_mean, Stats)},
                   {X ++ ".95percentile",
                    proplists:get_value(95, proplists:get_value(percentile, Stats))}]
          end
      end,
      Metrics),
    lists:foreach(fun folsom_metrics:delete_metric/1, Metrics),
    Values.


