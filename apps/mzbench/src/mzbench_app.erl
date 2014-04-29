-module(mzbench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    Args = init:get_plain_arguments(),
    case Args of
        ["console"] ->
            mzbench_sup:start_link();
        ["console", Script] ->
            Sup = mzbench_sup:start_link(),
            mzbench_sup:run(Script),
            Sup;
        _ ->
            lager:error("  Usage: make run [<script>]")
    end.

stop(_State) ->
    ok.
