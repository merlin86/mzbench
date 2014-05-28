-module(ast).

-export([transform/1, add_meta/2, map_meta/2]).

-include("types.hrl").
-include("ast.hrl").

-spec add_meta(abstract_expr(), meta()) -> abstract_expr().
add_meta(Expr, NewMeta) ->
   map_meta(fun (Meta, _) -> Meta ++ NewMeta end, Expr).

-spec map_meta(fun((meta(), atom()) -> meta()), abstract_expr()) -> abstract_expr().
map_meta(Fun, #operation{name = Name, meta = Meta, args = Args} = Op) ->
    Op#operation{meta = Fun(Meta, Name), args = map_meta(Fun, Args)};
map_meta(Fun, [H | T]) ->
    [map_meta(Fun, H) | map_meta(Fun, T)];
map_meta(_, []) -> [];
map_meta(_, C) -> C.

-spec markup(abstract_expr()) -> abstract_expr().
markup({tuple, Line, [{atom, L2, Op} | Params]}) ->
  {tuple, Line, [{atom, L2, Op}, {cons, L2, {tuple, L2, [{atom, L2, line}, {integer, L2, Line}]}, {nil, L2}} |
                 markup(Params)]};
markup({tuple, Line, []}) ->
  {tuple, Line, [{cons, Line, {tuple, Line, [{atom, Line, line}, {integer, Line, Line}]}, {nil, Line}}]};
markup([]) -> [];
markup([H | T]) -> [markup(H) | markup(T)];
markup(T) when is_tuple(T) ->
  case tuple_to_list(T) of
    [cons, L | S] -> list_to_tuple([cons, L | markup(S)]);
    _ -> T
  end;
markup(S) -> S.

records([]) -> [];
records(L) when is_list(L) -> lists:map(fun records/1, L);
records(T) when is_tuple(T) ->
  case tuple_to_list(T) of
    [N, Units] when is_number(N) -> #constant{value = N, units = Units};
    [Name, Meta | Params] -> #operation{name = Name, meta = Meta, args = records(Params)};
    [Meta] -> #operation{name = undefined, meta = Meta, args = []}
  end;
records(S) -> S.

transform(AST) ->
  records(literals:convert(erl_parse:normalise(markup(AST)))).
