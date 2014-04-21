-module(dummy_worker).

-export([initial_state/0,
         print/2]).

-spec initial_state() -> string().
initial_state() -> "".

-spec print(string(), string()) -> {nil, string()}.
print(State, Line) ->
    lager:info("Appending ~p", [Line]),
    {nil, State ++ Line}.
