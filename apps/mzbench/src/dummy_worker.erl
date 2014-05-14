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

    lager:info("Appending ~p, Meta: ~p~n", [Text, Meta]),
    {nil, State ++ Text}.
