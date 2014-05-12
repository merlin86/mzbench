-module(dummy_worker).

-export([initial_state/0,
         print/3]).

-include("types.hrl").
-type state() :: string().

-spec initial_state() -> state().
initial_state() -> "".

-spec print(state(), meta(), string()) -> {nil, state()}.
print(State, Meta, Text) ->
    metrics:notify_counter(Meta),

    Line = proplists:get_value(line, Meta, 0),
    SampleMetrics = proplists:get_value(sample_metrics, Meta, 1),

    Metric = string:concat(integer_to_list(Line), "-print"),
    mzbench_metrics:notify(Metric, {inc, 1}, counter, SampleMetrics),

    lager:info("Appending ~p, Meta: ~p~n", [Text, Meta]),
    {nil, State ++ Text}.
