-module(metrics).

-export([notify_counter/1, notify_roundtrip/2]).

notify_counter(Meta) ->
    folsom_metrics:notify(proplists:get_value(metric_prefix, Meta) ++ ".counter", {inc, 1}, counter).

notify_roundtrip(Meta, Value) ->
    folsom_metrics:notify(proplists:get_value(metric_prefix, Meta) ++ ".roundtrip", Value, histogram).
