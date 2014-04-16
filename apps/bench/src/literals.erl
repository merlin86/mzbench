-module(literals).

-export([convert/1]).

convert({Val, kb}) -> {Val*1024, b};
convert({Val, mb}) -> {Val*1024*1024, b};
convert({Val, gb}) -> {Val*1024*1024*1024, b};
convert({Val, tb}) -> {Val*1024*1024*1024*1024, b};
convert({Val, s}) -> {Val*1000, ms};
convert({Val, min}) -> {Val*60*1000, ms};
convert({Val, h}) -> {Val*3600*1000, ms};
convert({Val, rpm}) -> {Val/60, rps};
convert({Val, rph}) -> {Val/3600, rps};
convert([H|T]) -> [convert(H) | convert(T)];
convert(T)  when is_tuple(T) -> erlang:list_to_tuple(convert(erlang:tuple_to_list(T)));
convert(A) -> A.
