-module(utility).

-export([choose/1, choose/2, random_bytes/1]).

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

random_bytes(N) -> crypto:rand_bytes(N).
