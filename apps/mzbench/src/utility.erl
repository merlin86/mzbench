-module(utility).

-export([choose/1, choose/2, random_binary/1, random_list/1, fold_interval/4]).

taken(L, N) ->
     Len = length(L),
     PropL = lists:zip(lists:seq(1, Len), L),
     taken(PropL, N, Len, []).

taken([], _, _, L) -> L;
taken(_, 0, _, L) -> L;
taken([{_,V}], _, _, L) -> [V | L];
taken(Ps, N, Len, L) ->
  {K1, V1} = lists:nth(crypto:rand_uniform(1, Len), Ps),
  taken(proplists:delete(K1, Ps), N-1, Len-1, [V1 | L]).

choose([]) -> erlang:error(badarg);
choose(List) -> lists:nth(crypto:rand_uniform(1, length(List)), List).

choose(N, List) -> taken(List, N).

random_binary(N) -> crypto:rand_bytes(N).

random_list(N) -> erlang:binary_to_list(crypto:rand_bytes(N)).

fold_interval(_, Acc, Start, End) when Start > End -> Acc;
fold_interval(Fun, Acc, Start, End) ->
    fold_interval(Fun, Fun(Start, Acc), Start + 1, End).


