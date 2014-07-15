-module(mz_bench_metrics).

-export([start_link/3,
         notify_counter/1,
         notify_roundtrip/2,
         get_metrics_values/1,
         trigger/1]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(s, {
    prefix      = "undefined" :: string(),
    last_export = undefined   :: undefined | erlang:timestamp(),
    nodes = [],
    supervisor_pid = undefined :: pid()
}).

-define(INTERVAL, 10000). % 10 seconds

%%%===================================================================
%%% API
%%%===================================================================

start_link(MetricsPrefix, Nodes, SuperPid) ->
    gen_server:start_link(?MODULE, [MetricsPrefix, Nodes, SuperPid], []).

notify_counter(Meta) ->
    SampleMetrics = proplists:get_value(sample_metrics, Meta, 1),
    Prefix = extract_metric_prefix(Meta),
    notify(
                Prefix ++ ".counter",
                {inc, 1},
                counter,
                SampleMetrics).

notify_roundtrip(Meta, Value) ->
    SampleMetrics = proplists:get_value(sample_metrics, Meta, 1),
    Prefix = extract_metric_prefix(Meta),
    notify(
      Prefix ++ ".roundtrip",
      Value,
      histogram,
      SampleMetrics).

trigger(Self) ->
    gen_server:call(Self, trigger).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MetricsPrefix, Nodes, SuperPid]) ->
    process_flag(trap_exit, true),
    erlang:send_after(?INTERVAL, self(), trigger),
    {ok, #s{prefix = MetricsPrefix, nodes = Nodes, supervisor_pid = SuperPid}}.

handle_call(trigger, _From, State) ->
    tick(State),
    {reply, ok, State#s{last_export = erlang:now()}};
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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tick(#s{prefix = Prefix, nodes = Nodes, supervisor_pid = SuperPid} = _State) ->

    Values = lists:flatmap(
        fun (N) ->
            case rpc:call(N, mz_bench_metrics, get_metrics_values, [Prefix]) of
                {badrpc, Reason} ->
                    lager:error("Failed to request metrics from node ~p (~p)", [N, Reason]),
                    [];
                Res ->
                    lager:info("Received metrics from ~p", [N]),
                    Res
            end
        end, Nodes),

    send_to_graphite(SuperPid, merge_metrics(Values)),

    ok.

merge_metrics(Values) -> merge_metrics(Values, []).
merge_metrics([], Res) -> Res;
merge_metrics([{M, _}|_] = Metrics, Res) ->
    V = proplists:get_all_values(M, Metrics),
    merge_metrics(proplists:delete(M, Metrics), [{M, merge_values(get_metric_type(M), V)} | Res]).

merge_values("counter", Values) -> lists:sum(Values);
merge_values(_, Values) -> median(Values).

get_metric_type(M) ->
    lists:last(string:tokens(M, ".")).

send_to_graphite(SuperPid, Values) ->
    case mz_bench_director_sup:get_graphite_client(SuperPid) of
        noclient -> ok;
        {ok, GraphiteClient} ->
            {Mega, Secs, _} = now(),
            Timestamp = Mega * 1000000 + Secs,
            lists:map(fun({MetricName, MetricValue}) ->
                        Msg =lists:flatten(io_lib:format("~s ~p ~p~n",
                                [MetricName, MetricValue, Timestamp])),
                        graphite_client:send(GraphiteClient, Msg)
                end,
                Values),
            lager:info("[ metrics ] sent to graphite: ~p", [Values]);
        Error -> lager:error("Could not get graphite client: ~p", [Error])
    end.

median([_|_] = L) -> lists:nth(round(erlang:length(L)/2), lists:usort(L)).

get_metrics_values(Prefix) ->
    Metrics = lists:filter(fun (M) -> lists:prefix(Prefix, M) end, folsom_metrics:get_metrics()),
    lager:info("[ metrics ] Got the following metrics: ~p", [Metrics]),
    Values = lists:flatmap(
      fun(X) ->
          case get_metric_type(X) of
              "counter"   -> [{X, folsom_metrics:get_metric_value(X)}];
              "roundtrip" ->
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

notify(Metric, Value, Type, 1) -> folsom_metrics:safely_notify(Metric, Value, Type);
notify(Metric, Value, Type, SampleMetrics) ->
  case random:uniform() of
    X when X<SampleMetrics -> folsom_metrics:safely_notify(Metric, Value, Type);
    _ -> ok
  end.


extract_metric_prefix(Meta) ->
    PoolName = proplists:get_value(pool_name, Meta, "default_pool"),
    Prefix = proplists:get_value(metric_prefix, Meta),
    case Prefix of
        undefined ->
            lager:error("Missing metric_prefix in meta: ~p", [Meta]),
            "mz_bench.undefined";
        _ -> re:replace(Prefix, "POOL", PoolName, [{return, list}])
    end.
