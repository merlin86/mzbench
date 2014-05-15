-module(metrics).

-export([notify_counter/1, notify_roundtrip/2]).


notify(Metric, Value, Type, 1) -> folsom_metrics:notify(Metric, Value, Type);
notify(Metric, Value, Type, SampleMetrics) ->
  case random:uniform() of
    X when X<SampleMetrics -> folsom_metrics:notify(Metric, Value, Type);
    _ -> ok
  end.

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

extract_metric_prefix(Meta) ->
    Prefix = proplists:get_value(metric_prefix, Meta),
    case Prefix of
        undefined ->
            lager:error("Missing metric_prefix in meta: ~p", Meta),
            "mzbench.undefined";
        _ -> Prefix
    end.
