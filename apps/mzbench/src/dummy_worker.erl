-module(dummy_worker).

-export([initial_state/0,
         print/3]).

-spec initial_state() -> string().
initial_state() -> folsom_metrics:new_history(printed),"".

-spec print(string(), string(), list()) -> {nil, string()}.
print(State, Text, Meta) ->
    Line = proplists:get_value(line, Meta),
    folsom_metrics:notify({printed, {Line, Text}}),
    lager:info("Appending ~p, Meta: ~p~n", [Text, Meta]),
    {nil, State ++ Text}.
