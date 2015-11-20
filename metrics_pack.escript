#!/usr/bin/env escript

-module(metrics_pack).


main([File]) ->
    {ok, H} = file:open(File, [read]),
    Res = process(H, #{data => #{}, metrics => []}),
    echo(Res),
    file:close(H).

process(H, S) ->
    case file:read_line(H) of
        {ok, Data} -> process(H, parse_line(string:strip(Data, both, $\n), S));
        eof -> file:close(H), S
    end.

parse_line(Data, S = #{data:= D, metrics:= Metrics}) ->
    [TimestampStr, Metric, Value] = string:tokens(Data, "\t"),
    Timestamp = erlang:list_to_integer(TimestampStr),
    CurTs = maps:get(Timestamp, D, #{}),
    S#{data => D#{Timestamp => CurTs#{Metric => Value}}, metrics => lists:usort([Metric|Metrics])}.

echo(#{data:= Data, metrics:= Metrics}) ->
    Timestamps = lists:usort(maps:keys(Data)),
    lists:foreach(fun (M) -> io:format("~s\t", [M]) end, Metrics),
    lists:foreach(
        fun (T) ->
            io:format("~n~b\t~s", [T, string:join([maps:get(M, maps:get(T, Data), "none") || M <- Metrics], "\t")])
        end, Timestamps),
    io:format("~n").

