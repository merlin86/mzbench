-module(dummy_worker).

-export([initial_state/0,
         print/2]).

-spec initial_state() -> string().
initial_state() -> folsom_metrics:new_history(printed),"".

-spec print(string(), string()) -> {nil, string()}.
print(State, Line) ->
    folsom_metrics:notify({printed, Line}),
    lager:info("Appending ~p", [Line]),
    {nil, State ++ Line}.
