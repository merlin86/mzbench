-module(ast).

-export([transform/1]).

-include("types.hrl").
-include("ast.hrl").

-spec markup(abstract_expr()) -> abstract_expr().
markup({tuple, Line, [{atom, L2, Op} | Params]}) ->
  {tuple, Line, [{atom, L2, Op}, {cons, L2, {tuple, L2, [{atom, L2, line}, {integer, L2, Line}]}, {nil, L2}} |
                 markup(Params)]};
markup([]) -> [];
markup([H | T]) -> [markup(H) | markup(T)];
markup(T) when is_tuple(T) ->
  case tuple_to_list(T) of
    [cons, L | S] -> list_to_tuple([cons, L | markup(S)]);
    _ -> T
  end;
markup(S) -> S.

records([]) -> [];
records([H | T]) -> [records(H) | records(T)];
records(T) when is_tuple(T) ->
  case tuple_to_list(T) of
    [N, Units] when is_number(N) -> #constant{value = N, units = Units};
    [Name, Meta | Params] -> #operation{name = Name, meta = Meta, args = records(Params)};
    _ -> T
  end;
records(S) -> S.

transform(AST) ->
  records(literals:convert(erl_parse:normalise(markup(AST)))).