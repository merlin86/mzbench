-module(mz_bench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    Args = init:get_plain_arguments(),
    case Args of
        ["console", []] ->
            %% This is what happens when 'mz_bench start' is called.
            mz_bench_sup:start_link();
        ["console", Script] ->
            Sup = mz_bench_sup:start_link(),
            mz_bench_sup:run(Script),
            Sup;
        _ ->
           mz_bench_sup:start_link()
    end.

stop(_State) ->
    ok.
