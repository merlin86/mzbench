-module(metrics).

-export([notify_counter/1, notify_roundtrip/2]).

notify_counter(Meta) ->
    Prefix = extract_metric_prefix(Meta),
    folsom_metrics:notify(
                Prefix ++ ".counter",
                {inc, 1},
                counter).

notify_roundtrip(Meta, Value) ->
    Prefix = extract_metric_prefix(Meta),
    folsom_metrics:notify(
      Prefix ++ ".roundtrip",
      Value,
      histogram).

extract_metric_prefix(Meta) ->
    Prefix = proplists:get_value(metric_prefix, Meta),
    case Prefix of
        undefined ->
            lager:error("Missing metric_prefix in meta: ~p", Meta),
            "mzbench.undefined";
        _ -> Prefix
    end.
