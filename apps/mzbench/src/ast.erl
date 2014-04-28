-module(ast).

-export([markup/1]).

-include("types.hrl").

-spec markup(abstract_expr()) -> abstract_expr().
markup({tuple, Line, [{atom, L2, Op} | Params]}) ->
  {tuple, Line, [{atom, L2, Op} |
                 markup(Params)] ++
                 [{cons, L2, {tuple, L2, [{atom, L2, line}, {integer, L2, Line}]}, {nil, L2}}]};
markup([]) -> [];
markup([H | T]) -> [markup(H) | markup(T)];
markup(T) when is_tuple(T) ->
  case tuple_to_list(T) of
    [cons, L | S] -> list_to_tuple([cons, L | markup(S)]);
    L -> list_to_tuple(L)
  end;
markup(S) -> S.
