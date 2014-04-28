-module(dummy_worker).

-export([initial_state/0,
         print/3]).

-include("types.hrl").
-type state() :: string().

-spec initial_state() -> state().
initial_state() -> "".

-spec print(state(), string(), meta()) -> {nil, state()}.
print(State, Text, Meta) ->
    Line = proplists:get_value(line, Meta, 0),
    
    Metric = string:concat(integer_to_list(Line), "-print"),
    folsom_metrics:notify(Metric, {inc, 1}, counter),
    
    lager:info("Appending ~p, Meta: ~p~n", [Text, Meta]),
    {nil, State ++ Text}.
