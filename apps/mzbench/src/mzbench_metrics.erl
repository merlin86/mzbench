-module(mzbench_metrics).

-export([notify/4]).

notify(Metric, Value, Type, 1) -> folsom_metrics:notify(Metric, Value, Type);
notify(Metric, Value, Type, SampleMetrics) ->
  case random:uniform() of
    X when X<SampleMetrics -> folsom_metrics:notify(Metric, Value, Type);
    _ -> ok
  end.

