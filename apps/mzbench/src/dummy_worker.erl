-module(dummy_worker).

-export([initial_state/0,
         print/3]).

-spec initial_state() -> string().
initial_state() -> folsom_metrics:new_history(printed),"".

-spec print(string(), string(), list()) -> {nil, string()}.
print(State, Text, Meta) ->
    Line = proplists:get_value(line, Meta, 0),
    notify(string:concat(integer_to_list(Line), "-print"), Text),
    lager:info("Appending ~p, Meta: ~p~n", [Text, Meta]),
    {nil, State ++ Text}.

notify(Metric, Value) ->
  Exists =lists:any(fun(E)-> E == Metric end, folsom_metrics:get_metrics()),
  if
    Exists == false -> folsom_metrics:new_history(Metric);
    true -> ok
  end,
  folsom_metrics:notify({Metric, Value}).
