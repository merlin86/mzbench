-module(dummy_worker).

-export([initial_state/0,
         print/2]).

initial_state() -> "".

print(State, Line) -> {nil, State ++ Line}.
