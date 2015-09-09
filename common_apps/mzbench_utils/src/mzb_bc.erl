-module(mzb_bc).

-export([
    maps_with/2,
    maps_without/2
]).

maps_with(List, Map) ->
    lists:foldr(fun(K, Acc) -> case maps:find(K, Map) of
        {ok, V} -> maps:put(K, V, Acc);
        _ -> Acc
    end end, #{}, List).

maps_without(List, Map) ->
    maps_with(lists:subtract(maps:keys(Map), List), Map).
